// XXX: This script assumes:
// - fd 3 is mapped to a pipe that provides wasm bytes
// - fd 4 is mapped to a pipe that provides UTF-8-encoded input

import * as fs from 'node:fs';

const WASM_FD = 3;
const INPUT_FD = 4;

let inputBuf = Buffer.alloc(0);
let inputEof = false;

function readTestInputLine() {
  if (inputEof) {
    return new Int8Array();
  };

  let bufs = [inputBuf];
  let prevSegmentLen = 0;
  let line;

  while (true) {
    const last = bufs[bufs.length - 1];
    const nlIndex = last.indexOf(0x0a);

    if (nlIndex !== -1) {
      // the `\n` is chopped off
      line = Buffer.concat(bufs, prevSegmentLen + nlIndex);
      inputBuf = last.subarray(nlIndex + 1);

      break;
    }

    const buf = Buffer.alloc(1024);
    const bytesRead = fs.readSync(INPUT_FD, buf);

    if (bytesRead === 0) {
      line = Buffer.concat(bufs);
      inputBuf = Buffer.alloc(0);
      inputEof = true;

      break;
    }

    prevSegmentLen += last.length;
    bufs.push(buf.subarray(0, bytesRead));
  }

  return new Int8Array(line.buffer, line.byteOffset, line.length);
}

function writeToStdout(array) {
  for (let offset = 0; offset < array.length;) {
    const bytesWritten = fs.writeSync(process.stdout.fd, array, offset);

    if (bytesWritten === 0) {
      console.error("writeSync returned 0");

      process.exit(10);
    }

    offset += bytesWritten;
  }
}

// Decodes as UTF-8.
function arrayToString(array) {
  return new TextDecoder().decode(array);
}

function stringToArray(str) {
  return new TextEncoder().encode(str);
}

let exports;

function bytesToArray(bytes) {
  const len = exports.bytes_len(bytes);
  const buf = new Uint8Array(len);

  for (let i = 0; i < len; ++i) {
    buf[i] = exports.bytes_get(bytes, i);
  }

  return buf;
}

function arrayToBytes(buf) {
  const len = buf.length;
  const bytes = exports.bytes_new(len);

  for (let i = 0; i < len; ++i) {
    exports.bytes_set(bytes, i, buf[i]);
  }

  return bytes;
}

const imports = {
  "cool-runtime": {
    abort: () => {
      console.error("the program under test has called `abort`");

      process.exit(2);
    },

    print_bytes: (bytes) => {
      const array = bytesToArray(bytes);

      writeToStdout(array);
    },

    print_int: (i) => {
      const str = String(i);
      const array = stringToArray(str);

      writeToStdout(array);
    },

    read_line: () => {
      const data = readTestInputLine();
      const bytes = arrayToBytes(stringToArray(data));

      return bytes;
    },

    read_int: () => {
      const data = readTestInputLine();
      const value = parseInt(data);

      if (value !== value) {
        return 0;
      }

      return value;
    },
  },
};

function readWasmModule() {
  let bufs = [];
  let size = 0;

  while (true) {
    const buf = Buffer.alloc(8192);
    const bytesRead = fs.readSync(WASM_FD, buf);

    if (bytesRead === 0) {
      break;
    }

    bufs.push(buf.subarray(0, bytesRead));
    size += bytesRead;
  }

  fs.closeSync(WASM_FD);

  return Buffer.concat(bufs, size);
}

const wasm = readWasmModule();

WebAssembly.instantiate(wasm, imports).then((module) => {
  exports = module.instance.exports;

  exports.run();
});
