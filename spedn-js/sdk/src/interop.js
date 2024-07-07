const workers = require("worker_threads");

const ref = val => h$c1(h$baseZCGHCziJSziPrimziJSVal_con_e, val);

const ap1 = (f, args) => h$c2(h$ap1_e, f, ...args);
const ap2 = (f, args) => h$c3(h$ap2_e, f, ...args);
const ap3 = (f, args) => h$c4(h$ap3_e, f, ...args);

const ret = f => ap1(h$baseZCGHCziJSziPrimziInternalzisetCurrentThreadResultValue, [f]);

const postResult = (id, t) => {
  setImmediate(() => {
    if (t.status !== 16) postResult(id, t);
    else workers.parentPort.postMessage({ id, result: JSON.parse(t.result) });
  });
};

const func0 = f => (id, args) => postResult(id, h$run(ret(f)));
const func1 = f => (id, args) => postResult(id, h$run(ret(ap1(f, args.map(ref)))));
const func2 = f => (id, args) => postResult(id, h$run(ret(ap2(f, args.map(ref)))));
const func3 = f => (id, args) => postResult(id, h$run(ret(ap3(f, args.map(ref)))));

workers.parentPort.on("message", function ({ id, func, args }) {
  try {
    switch (func) {
      case "dispose":
        workers.parentPort.close();
        h$doneMain();
      default:
        global[func](id, args);
        break;
    }
  } catch (ex) {
    workers.parentPort.postMessage({ id, result: ex });
  }
});

global["compileCode"] = func2(h$mainZCMainzicompileCode);
global["compileFile"] = func2(h$mainZCMainzicompileFile);
