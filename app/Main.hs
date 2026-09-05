-- | minisim: a tiny HDL simulator producing WaveDrom JSON.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Parsec.Error (ParseError, errorPos, setErrorPos)
import Text.Parsec.Pos (newPos, sourceColumn, sourceLine)

import Minisim.Ast (Bit(..), Program)
import Minisim.Diagram (renderDiagram)
import Minisim.Elab (Design(..), elaborate)
import Minisim.Parser (parseProgram)
import Minisim.Sim (SimResult(..), runSim)
import Minisim.WaveDrom (renderWaveDrom)

data Opts = Opts
  { optInputs :: [FilePath]
  , optOutput :: Maybe FilePath
  , optText   :: Bool
  , optDiagram :: Bool
  }

defaultOpts :: Opts
defaultOpts = Opts [] Nothing False False

parseOpts :: [String] -> Either String Opts
parseOpts ("-o" : f : rest) = (\o -> o { optOutput = Just f }) <$> parseOpts rest
parseOpts ("--text" : rest) = do
  o <- parseOpts rest
  if optDiagram o
    then Left "--text and --diagram are mutually exclusive"
    else Right o { optText = True }
parseOpts ("--diagram" : rest) = do
  o <- parseOpts rest
  if optText o
    then Left "--text and --diagram are mutually exclusive"
    else Right o { optDiagram = True }
parseOpts ("-h" : _) = usageErr
parseOpts ("--help" : _) = usageErr
parseOpts (f : rest)
  | take 1 f == "-" = Left ("unknown option " ++ f)
  | otherwise = (\o -> o { optInputs = optInputs o ++ [f] }) <$> parseOpts rest
parseOpts [] = Right defaultOpts

usage :: String
usage = "usage: minisim [--text | --diagram] [-o out.json] input.hdl [more.hdl ...]"

usageErr :: Either String a
usageErr = Left usage

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case parseOpts args of
    Left msg -> hPutStrLn stderr msg >> exitFailure
    Right opts -> case optInputs opts of
      [] -> hPutStrLn stderr usage >> exitFailure
      fs -> do
        inputs <- readInputs fs
        case inputs >>= run opts of
          Left e -> hPutStrLn stderr ("minisim: " ++ e) >> exitFailure
          Right (warnings, out) -> do
            forM_ warnings $ \w -> hPutStrLn stderr ("minisim: warning: " ++ w)
            case optOutput opts of
              Just f -> writeFile f out
              Nothing -> putStr out

-- | Compile and render according to the mode: @--diagram@ renders the
-- circuit structure from the AST (no simulation needed), otherwise the
-- program is simulated for its waveform (or ASCII table).
run :: Opts -> [(FilePath, String)] -> Either String ([String], String)
run opts files
  | optDiagram opts = do
      prog <- parseAll files
      out <- tagged (labelOf (map fst files)) (renderDiagram prog)
      return ([], out)
  | otherwise = do
      (ws, sr) <- compile files
      return (ws, if optText opts then renderText sr else renderWaveDrom sr)
 where
  tagged l = either (Left . ((l ++ ": ") ++)) Right

-- | Read every input file; one that cannot be read fails the whole run with
-- its IO error (which mentions the file name).
readInputs :: [FilePath] -> IO (Either String [(FilePath, String)])
readInputs = go []
 where
  go acc [] = return (Right (reverse acc))
  go acc (f : rest) = do
    r <- try (readFile f) :: IO (Either IOException String)
    case r of
      Left ioe -> return (Left (show ioe))
      Right s -> go ((f, s) : acc) rest

-- | Parse several input files as one program: their texts are
-- concatenated (in command-line order, each terminated by a newline).
parseAll :: [(FilePath, String)] -> Either String Program
parseAll files =
  let srcs = map (ensureNl . snd) files
      starts = init (scanl (+) 1 (map countLines srcs))
      names = map fst files
  in either (Left . remapParseError names starts) Right
       (parseProgram "input" (concat srcs))

-- | Simulate several input files as one program.
compile :: [(FilePath, String)] -> Either String ([String], SimResult)
compile files = do
  prog <- parseAll files
  let label = labelOf (map fst files)
      tagged l = either (Left . ((l ++ ": ") ++)) Right
  design <- tagged label (elaborate prog)
  sr <- tagged label (runSim design)
  return (dWarn design, sr)

labelOf :: [FilePath] -> FilePath
labelOf [f] = f
labelOf fs = intercalate "+" fs

-- A file's text is terminated with a newline so that the last line of one
-- file cannot merge with the first line of the next.
ensureNl :: String -> String
ensureNl s
  | null s || last s == '\n' = s
  | otherwise = s ++ "\n"

countLines :: String -> Int
countLines = length . filter (== '\n')

-- | A parse error in the concatenated source is reported against the file
-- (and the line within that file) the offending line came from.
remapParseError :: [FilePath] -> [Int] -> ParseError -> String
remapParseError names starts pe = case filter ((<= l) . snd) (zip names starts) of
  [] -> show pe
  xs -> let (n, s) = last xs
        in show (setErrorPos (newPos n (l - s + 1) col) pe)
 where
  l = sourceLine (errorPos pe)
  col = sourceColumn (errorPos pe)

--------------------------------------------------------------------------------
-- Human readable dump (one line per signal, one column per timestamp)
--------------------------------------------------------------------------------

renderText :: SimResult -> String
renderText sr = unlines (header : rows)
 where
  t = srT sr
  header = padR 9 "t" ++ unwords [padL 4 (show k) | k <- [1 .. t]]
  clockRow (n, m) = padR 9 n
    ++ unwords [ padL 4 [bitCh (if even ((k - 1) `div` fromInteger m) then B1 else B0)]
               | k <- [1 .. t] ]
  wireRow (n, w) = padR 9 n
    ++ unwords [ padL (max 4 w) (if w == 1 then [bitCh (headOr v)] else map bitCh (reverse v))
               | v <- M.findWithDefault [] n (srHist sr) ]
  headOr (b : _) = b
  headOr [] = BX
  rows = map clockRow (srClocks sr) ++ map wireRow (traced (srWires sr))
  traced ws = [(n, w) | (n, w, True) <- ws]
  bitCh B0 = '0'
  bitCh B1 = '1'
  bitCh BX = 'x'

padL :: Int -> String -> String
padL w s = replicate (max 0 (w - length s)) ' ' ++ s

padR :: Int -> String -> String
padR w s = s ++ replicate (max 0 (w - length s)) ' '
