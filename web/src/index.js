import { Terminal } from 'xterm';
import ace from 'ace-builds';

import 'xterm/css/xterm.css';

import './styles.css';

const term = new Terminal();
term.open(document.getElementById('terminal'));
term.write('Hey there.');

const editor = ace.edit('editor-inner');
