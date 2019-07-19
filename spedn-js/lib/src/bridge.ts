import { Worker } from "worker_threads";
import { Disposable } from "./disposable";

interface Response {
  id: number;
  result: any;
}

export class Bridge implements Disposable {
  private requests: { [id: number]: any } = {};
  private counter = 0;

  constructor(private worker: Worker) {
    this.worker.on("message", this.handleMessage.bind(this));
    this.worker.on("error", e => {
      console.log(e);
    });
  }

  request(func: string, ...args: any[]): Promise<any> {
    return new Promise((resolve, _) => {
      const id = this.counter++;
      this.requests[id] = resolve;
      this.worker.postMessage({ id, func, args });
    });
  }

  dispose() {
    this.worker.postMessage({ id: -1, func: "dispose" });
  }

  private handleMessage({ id, result }: Response) {
    if (this.requests[id]) {
      this.requests[id](result);
      delete this.requests[id];
    }
  }
}
