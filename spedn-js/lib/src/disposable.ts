export interface Disposable {
  dispose(): void;
}

export async function using<T extends Disposable>(
  resource: T,
  action: (resource: T) => Promise<void>
) {
  try {
    await action(resource);
  } finally {
    resource.dispose();
  }
}
