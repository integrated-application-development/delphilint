export class LintError extends Error {}

export class ServerError extends LintError {}
export class InitializeError extends ServerError {}
export class AnalyzeError extends ServerError {}

export class NoAnalyzableFileError extends LintError {}
export class NoServerJarError extends LintError {}
export class NoJavaExeError extends LintError {}
