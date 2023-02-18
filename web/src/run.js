let exports;

// Decodes as UTF-8.
function arrayToString(array) {
  return new TextDecoder().decode(array);
}

function stringToArray(str) {
  return new TextEncoder().encode(str);
}

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
      throw "welp.";
    },

    print_bytes: (bytes) => {
      console.log(arrayToString(bytesToArray(bytes)));
    },

    print_int: (i) => {
      console.log(i);
    },

    read_line: () => {
      const data = prompt("The program has requested a line of input") || "";
      const bytes = arrayToBytes(stringToArray(data));

      return bytes;
    },

    read_int: () => {
      const data = prompt("The program has requested an integer") || "";
      const value = +data | 0;

      return bytes;
    },
  },
};

export async function runCoolWasm(wasm) {
  const module = await WebAssembly.instantiate(wasm, imports);
  exports = module.instance.exports;

  exports.run();
}
