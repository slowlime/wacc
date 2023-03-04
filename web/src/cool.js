// Decodes as UTF-8.
function arrayToString(array) {
  return new TextDecoder().decode(array);
}

function stringToArray(str) {
  return new TextEncoder().encode(str);
}

class CoolModule {
  #runtime;
  #moduleExports;

  static async instantiate(runtime, wasm) {
    const cool = new CoolModule(runtime);
    const module = await WebAssembly.instantiate(wasm, cool.#createImportTable());
    cool.#moduleExports = module.instance.exports;

    return cool;
  }

  constructor(runtime) {
    this.#runtime = runtime;
  }

  run() {
    this.#moduleExports.run();
  }

  #createImportTable() {
    return {
      "cool-runtime": {
        abort: this.#coolAbort.bind(this),
        print_bytes: this.#coolPrintBytes.bind(this),
        print_int: this.#coolPrintInt.bind(this),
        read_line: this.#coolReadLine.bind(this),
        read_int: this.#coolReadInt.bind(this),
      },
    };
  }

  #coolAbort() {
    throw "welp.";
  }

  #coolPrintBytes(bytes) {
    this.#runtime.write(this.#bytesToArray(bytes));
  }

  #coolPrintInt(i) {
    this.#runtime.write(String(i));
  }

  #coolReadLine() {
    const data = this.#runtime.readLine() || "";
    const bytes = this.#arrayToBytes(stringToArray(data));

    return bytes;
  }

  #coolReadInt() {
    const data = this.#runtime.readLine();
    const value = parseInt(data);

    if (Number.isNaN(value)) {
      return 0;
    }

    return value;
  }

  #bytesLen(bytes) {
    return this.#moduleExports.bytes_len(bytes);
  }

  #bytesGet(bytes, i) {
    return this.#moduleExports.bytes_get(bytes, i);
  }

  #bytesSet(bytes, i, value) {
    this.#moduleExports.bytes_set(bytes, i, value);
  }

  #bytesNew(len) {
    return this.#moduleExports.bytes_new(len);
  }

  #bytesToArray(bytes) {
    const len = this.#bytesLen(bytes);
    const buf = new Uint8Array(len);

    for (let i = 0; i < len; ++i) {
      buf[i] = this.#bytesGet(bytes, i);
    }

    return buf;
  }

  #arrayToBytes(buf) {
    const len = buf.length;
    const bytes = this.#bytesNew(len);

    for (let i = 0; i < len; ++i) {
      this.#bytesSet(bytes, i, buf[i]);
    }

    return bytes;
  }
}

export class CoolRuntime {
  #reader;
  #writer;

  constructor({reader, writer}) {
    this.#reader = reader;
    this.#writer = writer;
  }

  async instantiate(wasm) {
    return await CoolModule.instantiate(this, wasm);
  }

  write(buf) {
    this.#writer(buf);
  }

  readLine() {
    return this.#reader();
  }
}
