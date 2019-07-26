import * as fs from "fs";
import * as path from "path";
import { promisify } from "util";
import { Worker } from "worker_threads";
import { Bridge } from "./Bridge";
import { Contract, makeContractClass, Template } from "./contracts";
import { Disposable } from "./disposable";

interface CompilerOutput {
  Left: any[];
  Right: Template;
}

const fileExists = promisify(fs.exists);

export class Spedn implements Disposable {
  private bridge = new Bridge(new Worker(__dirname + "/compiler_service.js"));

  async compileCode(code: string): Promise<Contract> {
    const output: CompilerOutput = await this.bridge.request("compileCode", code);
    if (output.Left) throw output.Left;
    return makeContractClass(output.Right);
  }

  async compileFile(file: string): Promise<Contract> {
    const absolute = path.resolve(file);
    if (!await fileExists(absolute)) throw Error(`File not found: ${absolute}`);

    const output: CompilerOutput = await this.bridge.request("compileFile", absolute);
    if (output.Left) throw output.Left;
    return makeContractClass(output.Right);
  }

  dispose() {
    this.bridge.dispose();
  }
}
