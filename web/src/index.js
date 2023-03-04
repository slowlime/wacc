import ace from 'ace-builds';
import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { openpty } from 'xterm-pty';
import 'xterm/css/xterm.css';

const wacc = import('../target/wacc');

import './styles.css';
import { runCoolWasm } from './run.js';

const codeEditor = ace.edit('code');
const inputEditor = ace.edit('input');

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

const slavePty = createTerminal('output');
slavePty.write('Hello world!');

wacc
  .then(m => {
    window.run = async function(code) {
      const wasm = m.compile_from_string(code);
      await runCoolWasm(wasm);
    };
  })
  .catch(console.error);
