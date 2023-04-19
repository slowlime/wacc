import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { openpty } from 'xterm-pty';
import 'xterm/css/xterm.css';

const wacc = import('../target/wacc');

import './styles.css';

import setUpEditor from './editor.js';
import { CoolRuntime } from './cool.js';

const CODE_EDITOR_STATE_KEY = 'code';
const INPUT_EDITOR_STATE_KEY = 'input';

const codeEditor = setUpEditor(CODE_EDITOR_STATE_KEY, 'code');
const inputEditor = setUpEditor(INPUT_EDITOR_STATE_KEY, 'input');
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

function makeButtonClickedListener(runButton, compiler) {
  const RED_COLOR = '\x1b[31m';
  const GREEN_COLOR = '\x1b[32m';
  const RESET_COLOR = '\x1b[m';

  return async () => {
    runButton.disabled = true;

    try {
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

        slavePty.write(`${GREEN_COLOR}Compilation finished!\n\n${RESET_COLOR}`);
        const cool = await runtime.instantiate(wasm);

        cool.run();
      } catch (e) {
        slavePty.write(`${RESET_COLOR}\n\n${RED_COLOR}${e.toString()}`);
      }
    } finally {
      runButton.disabled = false;
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
    runButton.onclick = makeButtonClickedListener(runButton, compiler);
  })
  .catch(console.error);

document.querySelector('#code textarea').focus();
