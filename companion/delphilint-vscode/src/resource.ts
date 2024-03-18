/*
 * DelphiLint VSCode
 * Copyright (C) 2024 Integrated Application Development
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
