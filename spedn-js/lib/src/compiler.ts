import * as path from "path";
import { Worker } from "worker_threads";
import { Bridge } from "./bridge";
import { Contract, makeContractClass, Template } from "./contracts";
import { Disposable } from "./disposable";

interface CompilerOutput {
  Left: any[];
  Right: Template;
}

export class Spedn implements Disposable {
  private bridge = new Bridge(new Worker(__dirname + "/compiler_service.js"));

  async compileCode(code: string): Promise<Contract> {
    const output: CompilerOutput = await this.bridge.request("compileCode", code);
    if (output.Left) throw output.Left;
    return makeContractClass(output.Right);
  }

  async compileFile(file: string): Promise<Contract> {
    const absolute = path.resolve(file);
    const output: CompilerOutput = await this.bridge.request("compileFile", absolute);
    if (!output) throw Error(`File not found: ${absolute}`);
    if (output.Left) throw output.Left;
    return makeContractClass(output.Right);
  }

  dispose() {
    this.bridge.dispose();
  }
}
