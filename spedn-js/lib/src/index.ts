import { Worker } from "worker_threads";
import { Bridge } from "./bridge";
import { Disposable, using } from "./disposable";
import * as path from "path";

class SpednOutput {}

export class Spedn implements Disposable {
  private bridge: Bridge = new Bridge(new Worker(__dirname + "/compiler_service.js"));

  async compileCode(code: string): Promise<any> {
    const output: SpednOutput = await this.bridge.request("compileCode", code);
    return output;
  }

  async compileFile(file: string) {
    const output = await this.bridge.request("compileFile", path.resolve(file));
    return output;
  }

  async compileTest(file: string): Promise<any> {
    return await this.bridge.request("compileTest", file);
  }

  dispose(): void {
    this.bridge.dispose();
  }
}
