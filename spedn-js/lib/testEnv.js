const NodeEnvironment = require("jest-environment-node");

class Env extends NodeEnvironment {
  async setup() {
    await super.setup();
    this.global.Uint8Array = Uint8Array;
  }

  async teardown() {
    await super.teardown();
  }

  runScript(script) {
    return super.runScript(script);
  }
}

module.exports = Env;
