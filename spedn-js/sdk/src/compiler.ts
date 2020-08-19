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

export class Spedn implements Disposable {
  private bridge = new Bridge(new Worker(__dirname + "/compiler_service.js"));

  constructor(private readonly rts: Rts) {}

  async compileCode(code: string): Promise<Module> {
    const output: CompilerOutput = await this.bridge.request("compileCode", code);
    if (output.Left) throw output.Left;
    return new ModuleFactory(this.rts).make(output.Right);
  }

  async compileFile(file: string): Promise<Module> {
    const absolute = path.resolve(file);
    if (!await fileExists(absolute)) throw Error(`File not found: ${absolute}`);

    const output: CompilerOutput = await this.bridge.request("compileFile", absolute);
    if (output.Left) throw output.Left;
    return new ModuleFactory(this.rts).make(output.Right);
  }

  dispose() {
    this.bridge.dispose();
  }
}
