import * as path from "path";
import { Worker } from "worker_threads";
import { Bridge } from "./bridge";
import { Disposable, using } from "./disposable";
import { Contract, makeContractClass } from "./contracts";

export class Spedn implements Disposable {
  private bridge: Bridge = new Bridge(new Worker(__dirname + "/compiler_service.js"));
  async compileCode(code: string): Promise<Contract> {
    const output = await this.bridge.request("compileCode", code);
    return makeContractClass(output);
  }

  async compileFile(file: string): Promise<Contract> {
    const absolute = path.resolve(file);
    const output = await this.bridge.request("compileFile", absolute);
    if (output) return makeContractClass(output);
    else throw Error(`File not found: ${absolute}`);
  }

  dispose(): void {
    this.bridge.dispose();
  }
}
