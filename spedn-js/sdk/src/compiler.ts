import { Disposable, Module, ModuleFactory, PortableModule, Rts } from "@spedn/rts";
import * as fs from "fs";
import * as path from "path";
import { promisify } from "util";
import { Worker } from "worker_threads";
import { Bridge } from "./Bridge";

interface CompilerOutput {
  Left: any[];
  Right: PortableModule;
}

const fileExists = promisify(fs.exists);

type Vm = "bch" | "btc" | "xec" | "xpi";

export class Spedn implements Disposable {
  private bridge = new Bridge(new Worker(__dirname + "/compiler_service.js"));

  async compileCode(target: Vm, code: string): Promise<PortableModule>;
  async compileCode(target: Vm, code: string, rts: Rts): Promise<Module>;
  async compileCode(target: Vm, code: string, rts?: Rts): Promise<Module | PortableModule> {
    const output: CompilerOutput = JSON.parse(await this.bridge.request("compileCode", target, code));
    if (output.Left) throw output.Left;
    return rts ? rts.load(output.Right) : output.Right;
  }

  async compileFile(target: Vm, file: string): Promise<PortableModule>;
  async compileFile(target: Vm, file: string, rts: Rts): Promise<Module>;
  async compileFile(target: Vm, file: string, rts?: Rts): Promise<Module | PortableModule> {
    const absolute = path.resolve(file);
    if (!await fileExists(absolute)) throw Error(`File not found: ${absolute}`);

    const output: CompilerOutput = JSON.parse(await this.bridge.request("compileFile", target, absolute));
    if (output.Left) throw output.Left;
    return rts ? rts.load(output.Right) : output.Right;
  }

  dispose() {
    this.bridge.dispose();
  }
}
