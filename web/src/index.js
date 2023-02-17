import ace from 'ace-builds';
import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';

import 'xterm/css/xterm.css';

import './styles.css';

let terminal;
const editor = ace.edit('editor-inner');

function initTerminal() {
  const element = document.getElementById('terminal');
  terminal = new Terminal({ cols: 2, rows: 1 });
  const termFit = new FitAddon();
  terminal.loadAddon(termFit);
  terminal.open(element);
  termFit.fit();

  document.getElementsByTagName('body')[0].onresize = () => {
    termFit.fit();
  };
}

initTerminal();
terminal.write('Hey there.');
