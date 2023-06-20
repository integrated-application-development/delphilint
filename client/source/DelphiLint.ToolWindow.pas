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
unit DelphiLint.ToolWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, DockForm, Vcl.Menus,
  DelphiLint.ToolFrame;

type

  TLintToolWindow = class(TDockableForm)
  private
    FFrame: TLintToolFrame;
  public
    class procedure CreateInstance;
    class procedure RemoveInstance;
    class procedure ShowInstance;
    class function Instance: TLintToolWindow;

    procedure Focus;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    property Frame: TLintToolFrame read FFrame;
  end;

implementation

{$R *.dfm}

uses
    DeskUtil
  , ToolsAPI
  ;

var
  GToolWindow: TLintToolWindow;

//______________________________________________________________________________________________________________________

procedure RegisterDockableForm(var FormInstance: TLintToolWindow);
begin
  if @RegisterFieldAddress <> nil then begin
    RegisterFieldAddress(FormInstance.Name, @FormInstance);
  end;

  RegisterDesktopFormClass(TLintToolWindow, FormInstance.Name, FormInstance.Name);
  (BorlandIDEServices as IOTAIDEThemingServices).RegisterFormClass(TLintToolWindow);
end;

//______________________________________________________________________________________________________________________

procedure UnregisterDockableForm(var FormInstance: TLintToolWindow);
begin
  if (@UnregisterFieldAddress <> nil) and Assigned(FormInstance) then begin
    UnregisterFieldAddress(@FormInstance);
  end;
end;

//______________________________________________________________________________________________________________________

procedure CreateDockableForm(var FormInstance: TLintToolWindow);
begin
  FormInstance := TLintToolWindow.Create(nil);
  RegisterDockableForm(FormInstance);
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(FormInstance);
end;

//______________________________________________________________________________________________________________________

procedure FreeDockableForm(var FormInstance: TLintToolWindow);
begin
  UnregisterDockableForm(FormInstance);
  FreeAndNil(FormInstance);
end;

//______________________________________________________________________________________________________________________

procedure ShowDockableForm(var FormInstance: TLintToolWindow);
begin
  if Assigned(FormInstance) then begin
    if FormInstance.Floating then begin
      FormInstance.Show;
      FormInstance.Focus;
    end
    else begin
      FormInstance.ForceShow;
      FocusWindow(FormInstance);
      FormInstance.Focus;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

class procedure TLintToolWindow.CreateInstance;
begin
  if not Assigned(GToolWindow) then begin
    CreateDockableForm(GToolWindow);
  end;
end;

//______________________________________________________________________________________________________________________

class procedure TLintToolWindow.RemoveInstance;
begin
  if Assigned(GToolWindow) then begin
    FreeDockableForm(GToolWindow);
  end;
end;

//______________________________________________________________________________________________________________________

class procedure TLintToolWindow.ShowInstance;
begin
  CreateInstance;
  ShowDockableForm(GToolWindow);
end;

//______________________________________________________________________________________________________________________

class function TLintToolWindow.Instance: TLintToolWindow;
begin
  Result := GToolWindow;
end;

//______________________________________________________________________________________________________________________

constructor TLintToolWindow.Create(Owner: TComponent);
begin
  inherited;

  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;

  FFrame := TLintToolFrame.Create(Self);
  FFrame.Parent := Self;
  FFrame.Align := alClient;
end;

//______________________________________________________________________________________________________________________

destructor TLintToolWindow.Destroy;
begin
  SaveStateNecessary := True;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintToolWindow.Focus;
begin
  SetFocus;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  if Assigned(GToolWindow) then begin
    GToolWindow.RemoveInstance;
  end;

end.
