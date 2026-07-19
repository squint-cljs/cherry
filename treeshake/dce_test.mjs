#!/usr/bin/env node
// DCE tests for the treeshake transform.
//
// Proves four properties of the transformed artifacts:
//   1. the harness itself measures shaking (negative control on the
//      untransformed core - if this "passes" on baseline, the test is broken)
//   2. dead code is actually eliminated, with hard byte budgets
//   3. the public API surface is unchanged
//   4. runtime behavior is identical: the same compiled cherry programs
//      produce byte-identical stdout on baseline and shaken cores
//
// Usage: node dce_test.mjs   (expects treeshake.mjs output in ./out)

import fs from 'node:fs';
import path from 'node:path';
import zlib from 'node:zlib';
import { execFileSync } from 'node:child_process';
import * as esbuild from 'esbuild';
import * as parser from '@babel/parser';

const here = path.dirname(new URL(import.meta.url).pathname);
const OUT = path.join(here, 'out');
const TMP = path.join(here, '.dce-tmp');
fs.rmSync(TMP, { recursive: true, force: true });
fs.mkdirSync(TMP, { recursive: true });

// baseline lib laid out with real names so relative imports resolve
const BASE = path.join(TMP, 'baseline');
fs.mkdirSync(BASE, { recursive: true });
fs.copyFileSync(path.join(here, 'testdata/cljs.core.orig.js'), path.join(BASE, 'cljs.core.js'));
fs.copyFileSync(path.join(here, 'testdata/clojure.string.orig.js'), path.join(BASE, 'clojure.string.js'));
fs.copyFileSync(path.join(here, 'testdata/clojure.walk.orig.js'), path.join(BASE, 'clojure.walk.js'));

let failures = 0;
function check(label, cond, detail) {
  if (cond) console.log(`PASS: ${label}`);
  else { failures++; console.log(`FAIL: ${label}${detail ? ` - ${detail}` : ''}`); }
}
const kb = (n) => `${(n / 1024).toFixed(1)} KB`;

async function bundle(name, entrySource, coreDir) {
  const entry = path.join(TMP, `${name}.entry.mjs`);
  const outfile = path.join(TMP, `${name}.bundle.mjs`);
  fs.writeFileSync(entry, entrySource);
  await esbuild.build({
    entryPoints: [entry],
    outfile,
    bundle: true,
    minify: true,
    format: 'esm',
    logLevel: 'silent',
    alias: {
      'cherry-cljs/cljs.core.js': path.join(coreDir, 'cljs.core.js'),
      'cherry-cljs/lib/clojure.string.js': path.join(coreDir, 'clojure.string.js'),
      'cherry-cljs/lib/clojure.walk.js': path.join(coreDir, 'clojure.walk.js'),
    },
  });
  const buf = fs.readFileSync(outfile);
  return { outfile, size: buf.length, gzip: zlib.gzipSync(buf).length };
}

function run(outfile) {
  return execFileSync(process.execPath, [outfile], { encoding: 'utf8' });
}

function exportNames(file) {
  const ast = parser.parse(fs.readFileSync(file, 'utf8'), { sourceType: 'module' });
  const names = new Set();
  for (const st of ast.program.body) {
    if (st.type !== 'ExportNamedDeclaration') continue;
    if (st.declaration?.declarations) {
      for (const d of st.declaration.declarations) names.add(d.id.name);
    }
    for (const sp of st.specifiers || []) names.add(sp.exported.name);
  }
  return names;
}

const FLOOR_BUDGET = 6_000;
const GRANULAR_BUDGET = 25_000;
const STR_BUDGET = 180_000;

// ---------------------------------------------------------------- tests ----

// 1. negative control: the untransformed core must NOT shake. If it does,
// the harness is measuring something other than reality.
{
  const b = await bundle('control-floor', `import 'cherry-cljs/cljs.core.js';`, BASE);
  check('negative control: baseline core does not shake',
        b.size > 300_000, `baseline floor ${kb(b.size)} - harness broken?`);
}

// 2. side-effect floor on the shaken facade
{
  const b = await bundle('floor', `import 'cherry-cljs/cljs.core.js';`, OUT);
  check(`floor: bare import <= ${kb(FLOOR_BUDGET)}`,
        b.size <= FLOOR_BUDGET, `${kb(b.size)}`);
  console.log(`  floor: ${b.size} bytes, gzip ${b.gzip}`);
}

// 3. public API surface: facade must re-export exactly the original names
{
  const orig = exportNames(path.join(BASE, 'cljs.core.js'));
  const facade = exportNames(path.join(OUT, 'cljs.core.js'));
  const missing = [...orig].filter((n) => !facade.has(n));
  const extra = [...facade].filter((n) => !orig.has(n));
  check('facade exports = original public exports',
        missing.length === 0 && extra.length === 0,
        `missing: ${missing.slice(0, 5)} extra: ${extra.slice(0, 5)}`);
}

// 4. DCE granularity: a tiny function must not drag the collection machinery
{
  const b = await bundle('granular',
    `import { not } from 'cherry-cljs/cljs.core.js'; console.log(not(false));`, OUT);
  const out = run(b.outfile).trim();
  check('granular: import { not } runs', out === 'true', out);
  check(`granular: import { not } <= ${kb(GRANULAR_BUDGET)}`,
        b.size <= GRANULAR_BUDGET, `${kb(b.size)}`);
  console.log(`  not: ${b.size} bytes, gzip ${b.gzip}`);
}

// 5. a collection-using import stays within its cone budget and works
{
  const b = await bundle('str',
    `import { str } from 'cherry-cljs/cljs.core.js'; console.log(str('a', 1));`, OUT);
  const out = run(b.outfile).trim();
  check('str: runs', out === 'a1', out);
  check(`str: <= ${kb(STR_BUDGET)}`, b.size <= STR_BUDGET, `${kb(b.size)}`);
  console.log(`  str: ${b.size} bytes, gzip ${b.gzip}`);
}

// 6. facade adds no size over importing the internal module directly
{
  const viaFacade = await bundle('eq-facade',
    `import { str } from 'cherry-cljs/cljs.core.js'; console.log(str('a', 1));`, OUT);
  const entry = path.join(TMP, 'eq-internal.entry.mjs');
  fs.writeFileSync(entry,
    `import { str } from '${path.join(OUT, 'internal', 'cljs.core.js')}'; console.log(str('a', 1));`);
  const outfile = path.join(TMP, 'eq-internal.bundle.mjs');
  await esbuild.build({ entryPoints: [entry], outfile, bundle: true, minify: true, format: 'esm', logLevel: 'silent' });
  const internalSize = fs.readFileSync(outfile).length;
  check('facade costs zero bytes', viaFacade.size === internalSize,
        `facade ${viaFacade.size} vs internal ${internalSize}`);
}

// 7. differential runtime: identical stdout on baseline vs shaken, for the
// full-core exercise and the tiny assoc/get program
for (const prog of ['exercise', 'tiny']) {
  const src = fs.readFileSync(path.join(here, `testdata/${prog}.mjs`), 'utf8');
  const base = await bundle(`${prog}-base`, src, BASE);
  const shak = await bundle(`${prog}-shaken`, src, OUT);
  const baseOut = run(base.outfile);
  const shakOut = run(shak.outfile);
  check(`differential: ${prog} stdout identical`, baseOut === shakOut,
        `baseline and shaken outputs differ`);
  check(`differential: ${prog} bundle shrinks`, shak.size < base.size * 0.75,
        `${kb(base.size)} -> ${kb(shak.size)}`);
  console.log(`  ${prog}: ${kb(base.size)} -> ${kb(shak.size)} (gzip ${kb(base.gzip)} -> ${kb(shak.gzip)})`);
}

// 8. rewritten lib module: works against the shaken core, and shakes
{
  const src = `
import { capitalize, blank_QMARK_ } from 'cherry-cljs/lib/clojure.string.js';
console.log(capitalize('cherry'), blank_QMARK_(''), blank_QMARK_('x'));`;
  const base = await bundle('string-base', src, BASE);
  const shak = await bundle('string-shaken', src, OUT);
  const baseOut = run(base.outfile);
  const shakOut = run(shak.outfile);
  check('clojure.string: differential stdout identical', baseOut === shakOut);
  check('clojure.string: bundle shrinks', shak.size < base.size * 0.75,
        `${kb(base.size)} -> ${kb(shak.size)}`);
  console.log(`  clojure.string: ${kb(base.size)} -> ${kb(shak.size)}`);
}

console.log(failures === 0 ? '\nAll DCE tests passed.' : `\n${failures} failure(s).`);
process.exit(failures === 0 ? 0 : 1);
