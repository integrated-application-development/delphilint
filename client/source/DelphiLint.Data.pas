{
DelphiLint Client for RAD Studio
Copyright (C) 2023 Integrated Application Development

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.Data;

interface

uses
    System.JSON
  ;

type

//______________________________________________________________________________________________________________________

  TRange = record
    StartLine: Integer;
    StartLineOffset: Integer;
    EndLine: Integer;
    EndLineOffset: Integer;

    constructor FromJson(Json: TJsonObject);
  end;

//______________________________________________________________________________________________________________________

  TLintIssue = class(TObject)
  private
    FRuleKey: string;
    FMessage: string;
    FFilePath: string;
    FRange: TRange;

  public
    property RuleKey: string read FRuleKey;
    property Message: string read FMessage;
    property FilePath: string read FFilePath;
    property Range: TRange read FRange write FRange;

    constructor FromJson(Json: TJsonObject);
  end;

//______________________________________________________________________________________________________________________

implementation

//______________________________________________________________________________________________________________________

constructor TRange.FromJson(Json: TJsonObject);
begin
  StartLine := Json.GetValue<Integer>('startLine', 0);
  EndLine := Json.GetValue<Integer>('endLine', 0);
  StartLineOffset := Json.GetValue<Integer>('startOffset', 0);
  EndLineOffset := Json.GetValue<Integer>('endOffset', 0);
end;

//______________________________________________________________________________________________________________________

constructor TLintIssue.FromJson(Json: TJsonObject);
var
  RangeJson: TJsonValue;
begin
  FRuleKey := Json.GetValue<string>('ruleKey');
  FMessage := Json.GetValue<string>('message', FRuleKey);
  FFilePath := Json.GetValue<string>('file');

  RangeJson := Json.GetValue<TJsonValue>('range', nil);
  if Assigned(RangeJson) and (RangeJson is TJsonObject) then begin
    FRange := TRange.FromJson(RangeJson as TJsonObject);
  end;
end;

//______________________________________________________________________________________________________________________

end.
