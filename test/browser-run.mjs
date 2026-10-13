// Run the tests of a :browser-test build in a headless Chrome, print what
// they print, and exit with 0 when they pass and 1 otherwise:
//
//     node test/browser-run.mjs target/browser-test
//
// It needs nothing but Node 22 or later and Chrome. It serves the build on
// 127.0.0.1, which a browser counts as a secure context, opens it in a page
// of its own, and reads the console of the page over the DevTools protocol
// until cljs.test prints its count of failures and errors. CHROME names
// another browser of Chromium.

import { spawn } from 'node:child_process';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';

const dir = path.resolve(process.argv[2] ?? 'target/browser-test');
const minutes = Number(process.env.BROWSER_TEST_MINUTES ?? 10);
const chrome = process.env.CHROME ?? (process.platform === 'darwin'
  ? '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
  : 'google-chrome');
const types = { '.html': 'text/html', '.js': 'text/javascript',
                '.map': 'application/json', '.css': 'text/css' };

const server = http.createServer(async (req, res) => {
  const wanted = decodeURIComponent(new URL(req.url, 'http://x').pathname);
  const file = path.join(dir, wanted.endsWith('/') ? wanted + 'index.html'
                                                  : wanted);
  if (!file.startsWith(dir)) return res.writeHead(403).end();
  try {
    const body = await fs.readFile(file);
    const type = types[path.extname(file)] ?? 'application/octet-stream';
    res.writeHead(200, { 'Content-Type': type }).end(body);
  } catch {
    res.writeHead(404).end();
  }
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const page = `http://127.0.0.1:${server.address().port}/`;

const profile = await fs.mkdtemp(path.join(os.tmpdir(), 'browser-run-'));
const browser = spawn(chrome, [
  '--headless=new', '--remote-debugging-port=0', `--user-data-dir=${profile}`,
  '--no-first-run', '--no-default-browser-check',
  ...(process.env.CI ? ['--no-sandbox'] : []),
  'about:blank'], { stdio: ['ignore', 'ignore', 'pipe'] });

// What went wrong, from the first failure on. On GitHub Actions it's an
// annotation too, which the summary of a run shows without a sign-in.
const report = [];
const complain = text => {
  console.error(text);
  report.push(text);
};
const escaped = text =>
  text.replace(/%/g, '%25').replace(/\r/g, '%0D').replace(/\n/g, '%0A');

let finished = false;
const finish = async code => {
  if (finished) return;
  finished = true;
  if (code && process.env.GITHUB_ACTIONS && report.length) {
    const text = escaped(report.slice(0, 200).join('\n'));
    console.log(`::error title=Browser tests::${text}`);
  }
  browser.kill();
  server.close();
  await fs.rm(profile, { recursive: true, force: true }).catch(() => {});
  process.exit(code);
};
setTimeout(() => {
  complain(`No count of failures within ${minutes} minutes`);
  finish(1);
}, minutes * 60000).unref();

// Chrome names the address of the protocol on its error output
const endpoint = await new Promise((resolve, reject) => {
  let text = '';
  browser.stderr.on('data', data => {
    text += data;
    const found = text.match(/DevTools listening on ws:\/\/([^/\s]+)\//);
    if (found) resolve(found[1]);
  });
  browser.on('exit', code =>
    reject(new Error(`Chrome exited with ${code}: ${text}`)));
});
const target = await (await fetch(`http://${endpoint}/json/new?about:blank`,
                                  { method: 'PUT' })).json();
const socket = new WebSocket(target.webSocketDebuggerUrl);
await new Promise(resolve => socket.addEventListener('open', resolve));

let id = 0;
const send = (method, params = {}) =>
  socket.send(JSON.stringify({ id: ++id, method, params }));

// an error before the first test is one of the build, which no test
// would report
let started = false;
socket.addEventListener('message', ({ data }) => {
  const { method, params } = JSON.parse(data);
  if (method === 'Runtime.consoleAPICalled') {
    const line = params.args.map(a => a.value ?? a.description ?? '').join(' ');
    console.log(line);
    if (report.length || /^(FAIL|ERROR) in /.test(line.trim())) {
      report.push(line);
    }
    started ||= line.startsWith('Testing ');
    const counts = line.match(/(\d+) failures, (\d+) errors\./);
    if (counts) finish(counts[1] === '0' && counts[2] === '0' ? 0 : 1);
  } else if (method === 'Runtime.exceptionThrown') {
    complain(params.exceptionDetails.exception?.description
             ?? params.exceptionDetails.text);
    if (!started) finish(1);
  }
});
send('Runtime.enable');
send('Page.navigate', { url: page });
setTimeout(() => {
  if (!started) {
    complain(`No test started within a minute at ${page}`);
    finish(1);
  }
}, 60000).unref();
