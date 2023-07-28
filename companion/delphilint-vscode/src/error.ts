export class LintError extends Error {
  constructor(message: string) {
    super(message);
  }
}

export class ServerError extends LintError {}
export class InitializeError extends ServerError {}
export class AnalyzeError extends ServerError {}

export class NoAnalyzableFileError extends LintError {}
export class NoDelphiProjectError extends LintError {}
export class NoServerJarError extends LintError {}
