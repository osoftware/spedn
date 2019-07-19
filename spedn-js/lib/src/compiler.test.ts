import { using } from "./disposable";
import { Spedn } from "./compiler";
import { Contract, Instance } from "./contracts";

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

describe("ExpiringTip contract", () => {
  let ExpiringTip: Contract;
  beforeAll(
    async () =>
      await using(new Spedn(), async compiler => {
        ExpiringTip = await compiler.compileFile("../../examples/ExpiringTip.spedn");
      })
  );

  describe("template", () => {
    it("should have 2 parameters", () => {
      expect(ExpiringTip.params).toEqual({
        alice: "Ripemd160",
        bob: "Ripemd160"
      });
    });
  });

  describe("instance", () => {
    it("should throw on missing parameters", () => {
      expect(() => new ExpiringTip({})).toThrow("Missing parameter: Ripemd160 alice");
    });
    it("should throw on invalid parameters", () => {
      expect(() => new ExpiringTip({ alice: 1, bob: true })).toThrow("Incorrect value for Ripemd160 alice");
    });
    it("should have 2 challenges", () => {
      const instance = new ExpiringTip({
        alice: Buffer.alloc(20),
        bob: Buffer.alloc(20)
      });
      expect(instance.challengeSpecs).toHaveProperty("receive");
      expect(instance.challengeSpecs).toHaveProperty("revoke");
    });
  });
});
