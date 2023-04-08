import ace from 'ace-builds';
import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { openpty } from 'xterm-pty';
import 'xterm/css/xterm.css';

const wacc = import('../target/wacc');

import './styles.css';
import { CoolRuntime } from './cool.js';

const codeEditor = ace.edit('code');
const inputEditor = ace.edit('input');
let slavePty;

function createTerminal(id) {
  const element = document.getElementById(id);
  const terminal = new Terminal({ cols: 2, rows: 1 });
  const termFit = new FitAddon();
  const { master, slave } = openpty();
  terminal.loadAddon(master);
  terminal.loadAddon(termFit);
  terminal.open(element);
  termFit.fit();

  document.getElementsByTagName('body')[0].onresize = () => {
    termFit.fit();
  };

  return slave;
}

function makeReader(data) {
  const lines = data.split('\n');
  let nextIndex = 0;

  return () => lines[nextIndex++];
}

function clearTerminal(pty) {
  const ERASE_DISPLAY = '\x1b[2J';
  const RESET_CURSOR = '\x1b[H';
  const RESET_COLOR = '\x1b[m';

  pty.write(RESET_COLOR + RESET_CURSOR + ERASE_DISPLAY);
}

function makeButtonClickedListener(compiler) {
  const RED_COLOR = '\x1b[31m';

  return async () => {
    // reset the terminal
    clearTerminal(slavePty);

    // grab the code and input data
    const code = codeEditor.getValue();
    const input = inputEditor.getValue();

    const runtime = new CoolRuntime({
      reader: makeReader(input),
      writer: (buf) => {
        slavePty.write(buf);
      },
    });

    try {
      const wasm = compiler.compile_from_string(code);
      const cool = await runtime.instantiate(wasm);

      cool.run();
    } catch (e) {
      slavePty.write('\n\n' + RED_COLOR + e.toString());
    }
  };
};

slavePty = createTerminal('output');
slavePty.write('Loading...');

const runButton = document.getElementById('run');

wacc
  .then(compiler => {
    clearTerminal(slavePty);
    slavePty.write('Press "Run" to execute the code!');
    runButton.onclick = makeButtonClickedListener(compiler);
  })
  .catch(console.error);
