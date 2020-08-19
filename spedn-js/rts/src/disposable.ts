export interface Disposable {
  dispose(): void;
}

export async function using<T extends Disposable, R>(
  resource: T,
  action: (resource: T) => Promise<R>
) {
  try {
    return await action(resource);
  } finally {
    resource.dispose();
  }
}
