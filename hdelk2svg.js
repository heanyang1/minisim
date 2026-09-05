#!/usr/bin/env node
/** hdelk2svg.js - render HDElk circuit-diagram JSON (minisim --diagram) to SVG.
 *
 * Usage:
 *     node hdelk2svg.js input.json [-o output.svg]
 *     cat diagram.json | node hdelk2svg.js > diagram.svg
 *
 * Rendering is done by HDElk's own sources -- hdelk.js, elk.bundled.js and
 * svg.min.js from https://github.com/davidthings/hdelk -- loaded into a
 * jsdom window (the same trick HDElk's own smoke test uses).  hdelk.js gets
 * two small in-memory fixes (unitless font sizes and the port-label inset
 * regressed after the browser renders the project ships; see patchHdelk).
 * jsdom has no font engine, so text widths are approximated: the diagram is
 * a faithful structure, but label spacing differs slightly from a browser's.
 *
 * One-time setup (downloads hdelk + npm-installs jsdom, both gitignored):
 *     make hdelk-tools        # or: tools/setup.sh
 *
 * Environment overrides:
 *     HDELK_HOME  hdelk checkout to use        (default: ./tools/hdelk)
 *     HDELK_JSDOM path to a jsdom module       (default: ./tools/node_modules/jsdom)
 */

'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { parseArgs } = require('node:util');

const TOOLS = path.join(__dirname, 'tools');
const DIAGRAM_ID = 'minisim_hdelk';
const LAYOUT_TIMEOUT_MS = 60000;

const usage =
  'usage: node hdelk2svg.js input.json [-o output.svg]\n' +
  '       cat diagram.json | node hdelk2svg.js > diagram.svg';

function die(msg) {
  process.stderr.write('error: ' + msg + '\n');
  process.exit(1);
}

// ---------------------------------------------------------------------------
// locate jsdom and the hdelk sources
// ---------------------------------------------------------------------------

// Returns the jsdom module (with a .JSDOM constructor), or exits with setup
// instructions when none is found.
function resolveJsdom() {
  const candidates = [
    process.env.HDELK_JSDOM,
    path.join(TOOLS, 'node_modules', 'jsdom'),
    'jsdom',                      // plain resolution (e.g. NODE_PATH)
  ].filter(Boolean);
  for (const c of candidates) {
    try {
      const m = require(c);
      const jsdom = m && (m.JSDOM ? m : m.default && m.default.JSDOM ? m.default : null);
      if (jsdom) return jsdom;
    } catch { /* try the next candidate */ }
  }
  die(
    'jsdom not found.  One-time setup:\n' +
    '    make hdelk-tools        # or: tools/setup.sh\n' +
    'or point HDELK_JSDOM at a jsdom module directory.');
}

function resolveHdelkJs() {
  const home = process.env.HDELK_HOME || path.join(TOOLS, 'hdelk');
  const dir = path.join(home, 'js');
  const files = ['elk.bundled.js', 'svg.min.js', 'hdelk.js'].map((f) => path.join(dir, f));
  const missing = files.filter((f) => !fs.existsSync(f));
  if (missing.length) {
    die(
      'hdelk sources not found in ' + dir + '\n' +
      'One-time setup:\n' +
      '    make hdelk-tools        # or: tools/setup.sh\n' +
      'or point HDELK_HOME at an hdelk checkout.');
  }
  return files;
}

// hdelk.js (as of commit 6e6ad7c) regressed from the version that produced
// the browser renders the project itself ships (docs/assets/images/banner.svg):
//   * font sizes are written as unitless CSS ("font-size:12"), which every
//     CSS engine -- browsers included -- silently drops, so all text renders
//     at the viewer's default size;
//   * port labels are inset as if text hung *below* the <text> y, but svg.js
//     drops every line by dy = 1.3*fontsize (read from the font-size
//     *attribute*, absent here), which pushed port labels out of their
//     bars: horizontally ~5px below an 18px bar, and for rotated parameter
//     labels sideways out of the bar ("only half the text is in the box").
// Both are patched in memory: fonts get units plus the attribute (so svg.js
// computes dy from the real size, 15.6px for the 12px port labels), and the
// insets center the 12px label ink in the 18px-tall/wide bars
// (horizontal: -3.5; vertical: +6.5, mirrored by the 90deg rotation).
// The counts warn when upstream changes stop the patches matching.
function patchHdelk(src) {
  let fonts = 0;
  src = src.replace(/\.style\(("font-size:")\s*\+\s*([A-Za-z_$][\w.$]*)\s*\)/g,
    (m, q, v) => {
      fonts++;
      return `.attr("font-size", ${v}).style(${q} + ${v} + "px")`;
    });
  let insetH = 0;
  src = src.replace(/child\.y\+item\.y \+ 2\);/g, () => {
    insetH++;
    return 'child.y+item.y - 3.5);';
  });
  let insetV = 0;
  src = src.replace(/\(item\.width-port_name_font_size\)\/2 \+ 2\)/g, () => {
    insetV++;
    return '(item.width-port_name_font_size)/2 + 6.5)';
  });
  if (fonts === 0 || insetH === 0 || insetV === 0) {
    process.stderr.write(
      'warning: hdelk.js patches matched fonts=' + fonts +
      ' insetH=' + insetH + ' insetV=' + insetV +
      ' (hdelk changed upstream? rendering may differ)\n');
  }
  return src;
}

// ---------------------------------------------------------------------------
// render: HDElk's scripts in a jsdom window
// ---------------------------------------------------------------------------

// jsdom has no text/SVG layout engine; polyfill the handful of APIs svg.js
// and hdelk.js need to measure text (approximation, as in HDElk's own
// headless smoke test).
function polyfillSvgMetrics(window) {
  for (const proto of [
    window.SVGTextElement && window.SVGTextElement.prototype,
    window.SVGTextContentElement && window.SVGTextContentElement.prototype,
    window.SVGElement.prototype,
  ]) {
    if (proto && !proto.getComputedTextLength) {
      proto.getComputedTextLength = function () {
        return ((this.textContent || '').length * 7) | 0;
      };
    }
  }
  for (const proto of [
    window.SVGGraphicsElement && window.SVGGraphicsElement.prototype,
    window.SVGElement.prototype,
  ]) {
    if (proto && !proto.getBBox) {
      proto.getBBox = function () { return { x: 0, y: 0, width: 100, height: 20 }; };
    }
    if (proto && !proto.getTotalLength) {
      proto.getTotalLength = function () { return 100; };
    }
  }
  if (!window.SVGSVGElement.prototype.createSVGRect) {
    window.SVGSVGElement.prototype.createSVGRect =
      () => ({ x: 0, y: 0, width: 0, height: 0 });
  }
  // svg.js's Matrix does its math on the *native* SVGMatrix object; give it
  // a real one (needed for hdelk's rotated parameter-port labels).
  if (!window.SVGSVGElement.prototype.createSVGMatrix) {
    window.SVGSVGElement.prototype.createSVGMatrix = () => svgMatrix(1, 0, 0, 1, 0, 0);
  }
  if (!window.SVGSVGElement.prototype.createSVGPoint) {
    window.SVGSVGElement.prototype.createSVGPoint = () => ({ x: 0, y: 0 });
  }
}

// Minimal SVGMatrix (2D affine): the operations svg.js ever calls.
function svgMatrix(a, b, c, d, e, f) {
  return {
    a, b, c, d, e, f,
    multiply(m) {
      return svgMatrix(
        this.a * m.a + this.c * m.b, this.b * m.a + this.d * m.b,
        this.a * m.c + this.c * m.d, this.b * m.c + this.d * m.d,
        this.a * m.e + this.c * m.f + this.e, this.b * m.e + this.d * m.f + this.f);
    },
    inverse() {
      const det = this.a * this.d - this.b * this.c;
      return svgMatrix(
        this.d / det, -this.b / det, -this.c / det, this.a / det,
        (this.c * this.f - this.d * this.e) / det,
        (this.b * this.e - this.a * this.f) / det);
    },
    translate(x, y) { return this.multiply(svgMatrix(1, 0, 0, 1, x, y)); },
    scale(s) { return this.multiply(svgMatrix(s, 0, 0, s, 0, 0)); },
    scaleNonUniform(x, y) { return this.multiply(svgMatrix(x, 0, 0, y, 0, 0)); },
    rotate(t) {
      const r = (t % 360) * Math.PI / 180;
      return this.multiply(svgMatrix(Math.cos(r), Math.sin(r), -Math.sin(r), Math.cos(r), 0, 0));
    },
    rotateFromVector(x, y) {
      const l = Math.sqrt(x * x + y * y) || 1;
      return this.multiply(svgMatrix(x / l, y / l, -y / l, x / l, 0, 0));
    },
    flipX() { return this.scaleNonUniform(-1, 1); },
    flipY() { return this.scaleNonUniform(1, -1); },
    skewX(t) { return this.multiply(svgMatrix(1, 0, Math.tan(t * Math.PI / 180), 1, 0, 0)); },
    skewY(t) { return this.multiply(svgMatrix(1, Math.tan(t * Math.PI / 180), 0, 1, 0, 0)); },
  };
}

function renderSvg(jsonText, hdelkFiles, jsdom) {
  // Validate the JSON in our own realm first, so bad input is our error.
  let parsed;
  try {
    parsed = JSON.parse(jsonText);
  } catch (e) {
    die('invalid JSON input: ' + e.message);
  }
  if (typeof parsed !== 'object' || parsed === null ||
      !('children' in parsed) && !('edges' in parsed)) {
    die("input is not an HDElk diagram (no 'children'/'edges' keys)");
  }

  const dom = new jsdom.JSDOM(
    '<!DOCTYPE html><html><body>' +
    '<div id="' + DIAGRAM_ID + '"></div>' +            // the diagram lands here
    '<div id="' + DIAGRAM_ID + '_message"></div>' +    // hdelk's error outlet
    '</body></html>',
    { runScripts: 'outside-only', pretendToBeVisual: true });
  const { window } = dom;
  polyfillSvgMetrics(window);
  for (const f of hdelkFiles) {
    const src = fs.readFileSync(f, 'utf8');
    window.eval(/hdelk\.js$/.test(f) ? patchHdelk(src) : src);
  }
  if (typeof window.hdelk !== 'object' || typeof window.hdelk.layout !== 'function') {
    window.close();
    die('hdelk.js did not load (no window.hdelk)');
  }

  // The graph object MUST live in the window realm: handing the GWT-compiled
  // ELK engine a cross-realm object makes every position come out as 0.
  const parseInRealm = window.eval('(function (t) { return JSON.parse(t); })');
  const graph = parseInRealm(jsonText);

  try {
    window.hdelk.layout(graph, DIAGRAM_ID);
  } catch (e) {
    window.close();
    die('hdelk failed: ' + (e && e.message ? e.message : String(e)));
  }

  // hdelk.layout does not return the ELK promise, so poll for the drawing
  // (or for the error it writes into the _message div).  Note: right after
  // the call the div holds hdelk's 0x0 text-measurement dummy svg; only a
  // finished layout replaces it with the real drawing.
  return new Promise((resolve, reject) => {
    const started = Date.now();
    (function poll() {
      const svg = window.document.querySelector('#' + DIAGRAM_ID + ' svg');
      const w = svg ? parseFloat(svg.getAttribute('width') || '0') : 0;
      if (svg && w > 0) {
        const out = svg.outerHTML;
        window.close();
        resolve(out);
        return;
      }
      const msg = window.document.getElementById(DIAGRAM_ID + '_message');
      if (msg && msg.textContent.trim()) {
        const text = msg.textContent.trim();
        window.close();
        reject(new Error('hdelk/ELK error: ' + text.slice(0, 500)));
        return;
      }
      if (Date.now() - started > LAYOUT_TIMEOUT_MS) {
        const why = svg
          ? 'hdelk produced an empty (0x0) diagram'
          : 'timed out after ' + LAYOUT_TIMEOUT_MS + ' ms waiting for ELK';
        window.close();
        reject(new Error(why));
        return;
      }
      setTimeout(poll, 25);
    })();
  });
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

async function main() {
  const { values, positionals } = parseArgs({
    allowPositionals: true,
    options: {
      output: { type: 'string', short: 'o' },
      help: { type: 'boolean', short: 'h' },
    },
  });
  if (values.help) {
    process.stdout.write(usage + '\n');
    return;
  }
  if (positionals.length > 1) die('unexpected argument: ' + positionals[1] + '\n' + usage);

  let jsonText;
  try {
    jsonText = positionals.length === 1
      ? fs.readFileSync(positionals[0], 'utf8')
      : fs.readFileSync(0, 'utf8');             // stdin
  } catch (e) {
    die('cannot read input: ' + e.message);
  }

  const jsdom = resolveJsdom();
  const hdelkFiles = resolveHdelkJs();

  let svg;
  try {
    svg = await renderSvg(jsonText, hdelkFiles, jsdom);
  } catch (e) {
    die(e.message);
  }

  // svg.js emits the xmlns already; add it defensively for standalone files.
  if (!/^<svg[^>]*\sxmlns=/.test(svg)) {
    svg = svg.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
  }

  if (values.output) {
    fs.writeFileSync(values.output, svg.endsWith('\n') ? svg : svg + '\n');
  } else {
    process.stdout.write(svg.endsWith('\n') ? svg : svg + '\n');
  }
}

main().catch((e) => die(e.message));
