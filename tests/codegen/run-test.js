let inputLineIdx = 0;

function readTestInputLine() {
  if (inputLineIdx >= inputLines.length) {
    return new Int8Array();
  }

  return inputLines[inputLineIdx++];
}

function arrayToString(array) {
  return decodeUtf8(array);
}

function stringToArray(str) {
  return encodeUtf8(str);
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
      throw "the program under test has called `abort`";
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

WebAssembly.instantiate(wasm, imports).then((module) => {
  exports = module.instance.exports;

  exports.run();
});
