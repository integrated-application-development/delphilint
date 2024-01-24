export class Exclusive<T> {
  private readonly _resource: T;
  private readonly _waiting: (() => void)[];
  private _locked: boolean;

  constructor(resource: T) {
    this._resource = resource;
    this._locked = false;
    this._waiting = [];
  }

  async with(func: (resource: T) => Promise<void>) {
    await this.acquire();
    try {
      await func(this._resource);
    } finally {
      this.release();
    }
  }

  private async acquire(): Promise<T> {
    if (this._locked) {
      await new Promise<void>((resolve, _rjct) => this._waiting.push(resolve));
    }
    this._locked = true;
    return this._resource;
  }

  private release() {
    this._locked = false;
    const nextWaiting = this._waiting.shift();
    if (nextWaiting) {
      nextWaiting();
    }
  }

  locked() {
    return this._locked;
  }
}
