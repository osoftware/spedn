import { BchJsRts } from "@spedn/rts-bchjs";
import { Module, Rts } from "@spedn/rts";
import { Spedn } from "@spedn/sdk";

const rts: Rts = new BchJsRts("bch");

describe("compiler", () => {
  let compiler: Spedn;
  beforeAll(() => (compiler = new Spedn()));
  afterAll(() => compiler.dispose());

  describe("for code file", () => {
    describe("missing", () => {
      it("should return an error", () => {
        return expect(compiler.compileFile("bch", "/x.spedn")).rejects.toHaveProperty("message", "File not found: /x.spedn");
      });
    });

    describe("with valid code", () => {
      let mod: Module;
      beforeAll(async () => (mod = await compiler.compileFile("bch", "../../examples/ExpiringTip.spedn", rts)));
      it("should create a contract", () => expect(mod.ExpiringTip).toBeDefined());
      it("should recognize parameters types", () => {
        expect(mod.ExpiringTip.params).toEqual({
          alice: "Ripemd160",
          bob: "Ripemd160"
        });
      });
    });

    describe("with invalid code", () => {
      it("should return a list of errors", () => {
        return expect(compiler.compileFile("bch", "../../examples/Invalid.spedn")).rejects.toHaveLength(1);
      });
    });
  });
});
