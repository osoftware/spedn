import { Spedn } from "./compiler";
import { Contract } from "./contracts";

describe("compiler", () => {
  let compiler: Spedn;
  beforeAll(() => (compiler = new Spedn()));
  afterAll(() => compiler.dispose());

  describe("for code file", () => {
    describe("missing", () => {
      it("should return an error", () => {
        return expect(compiler.compileFile("/x.spedn")).rejects.toHaveProperty("message", "File not found: /x.spedn");
      });
    });

    describe("with valid code", () => {
      let ExpiringTip: Contract;
      beforeAll(async () => (ExpiringTip = await compiler.compileFile("../../examples/ExpiringTip.spedn")));
      it("should create a contract", () => expect(ExpiringTip).toBeDefined());
      it("should recognize parameters types", () => {
        expect(ExpiringTip.params).toEqual({
          alice: "Ripemd160",
          bob: "Ripemd160"
        });
      });
    });

    describe("with invalid code", () => {
      it("should return a list of errors", () => {
        return expect(compiler.compileFile("../../examples/Invalid.spedn")).rejects.toHaveLength(2);
      });
    });
  });
});
