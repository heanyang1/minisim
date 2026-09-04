-- | minisim: a tiny HDL simulator producing WaveDrom JSON.
module Main (main) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Minisim.Ast (Bit(..))
import Minisim.Elab (Design(..), elaborate)
import Minisim.Parser (parseProgram)
import Minisim.Sim (SimResult(..), runSim)
import Minisim.WaveDrom (renderWaveDrom)

data Opts = Opts
  { optInput  :: Maybe FilePath
  , optOutput :: Maybe FilePath
  , optText   :: Bool
  }

defaultOpts :: Opts
defaultOpts = Opts Nothing Nothing False

parseOpts :: [String] -> Either String Opts
parseOpts ("-o" : f : rest) = (\o -> o { optOutput = Just f }) <$> parseOpts rest
parseOpts ("--text" : rest) = (\o -> o { optText = True }) <$> parseOpts rest
parseOpts ("-h" : _) = usageErr
parseOpts ("--help" : _) = usageErr
parseOpts (f : rest)
  | take 1 f == "-" = Left ("unknown option " ++ f)
  | otherwise = (\o -> o { optInput = Just f }) <$> parseOpts rest
parseOpts [] = Right defaultOpts

usage :: String
usage = "usage: minisim [--text] [-o out.json] input.hdl"

usageErr :: Either String a
usageErr = Left usage

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case parseOpts args of
    Left msg -> hPutStrLn stderr msg >> exitFailure
    Right opts -> case optInput opts of
      Nothing -> hPutStrLn stderr usage >> exitFailure
      Just file -> do
        src <- readFile file
        case compile src of
          Left e -> do
            hPutStrLn stderr ("minisim: " ++ file ++ ": " ++ e)
            exitFailure
          Right (warnings, sr) -> do
            forM_ warnings $ \w -> hPutStrLn stderr ("minisim: warning: " ++ w)
            let out = if optText opts then renderText sr else renderWaveDrom sr
            case optOutput opts of
              Just f -> writeFile f out
              Nothing -> putStr out

compile :: String -> Either String ([String], SimResult)
compile src = do
  prog <- either (Left . show) Right (parseProgram "input" src)
  design <- elaborate prog
  sr <- runSim design
  return (dWarn design, sr)

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
