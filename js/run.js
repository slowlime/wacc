let exports;

const imports = {
  "cool-runtime": {
    abort: () => {
      throw "welp.";
    },

    print_bytes: (bytes) => {
      console.log(bytes);
    },

    print_int: (i) => {
      console.log(i);
    },

    read_line: () => {
      const empty_bytes = exports.bytes_new(0);

      return empty_bytes;
    },

    read_int: () => {
      return 42;
    },
  },
};

async function run(url) {
  const module = await WebAssembly.instantiateStreaming(fetch(url), imports);
  exports = module.instance.exports;

  exports.run();
}
