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
unit DelphiLint.ToolFormInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, DockForm, Vcl.Menus,
  ToolsAPI, System.IniFiles, Vcl.ActnList, Vcl.ImgList, DesignIntf;

type

  TCustomDockableFormBase = class abstract(TInterfacedObject, INTACustomDockableForm)
  public
    function GetCaption: string; virtual;
    function GetIdentifier: string; virtual; abstract;
    function GetFrameClass: TCustomFrameClass; virtual; abstract;
    procedure FrameCreated(AFrame: TCustomFrame); virtual;
    function GetMenuActionList: TCustomActionList; virtual;
    function GetMenuImageList: TCustomImageList; virtual;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu); virtual;
    function GetToolBarActionList: TCustomActionList; virtual;
    function GetToolBarImageList: TCustomImageList; virtual;
    procedure CustomizeToolBar(ToolBar: TToolBar); virtual;
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean); virtual;
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string); virtual;
    function GetEditState: TEditState; virtual;
    function EditAction(Action: TEditAction): Boolean; virtual;
  end;

  TLintToolFormInfo = class(TCustomDockableFormBase)
  public
    function GetCaption: string; override;
    function GetIdentifier: string; override;
    function GetFrameClass: TCustomFrameClass; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;
  end;

implementation

uses
    DelphiLint.ToolFrame
  ;

//______________________________________________________________________________________________________________________

function TLintToolFormInfo.GetIdentifier: string;
begin
  Result := 'DelphiLintToolForm';
end;

procedure TLintToolFormInfo.FrameCreated(AFrame: TCustomFrame);
begin
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(AFrame);
end;

function TLintToolFormInfo.GetCaption: string;
begin
  Result := 'DelphiLint';
end;

function TLintToolFormInfo.GetFrameClass: TCustomFrameClass;
begin
  Result := TLintToolFrame;
end;

//______________________________________________________________________________________________________________________

procedure TCustomDockableFormBase.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // Empty default implementation
end;

procedure TCustomDockableFormBase.CustomizeToolBar(ToolBar: TToolBar);
begin
  // Empty default implementation
end;

function TCustomDockableFormBase.EditAction(Action: TEditAction): Boolean;
begin
  // Empty default implementation
  Result := True;
end;

procedure TCustomDockableFormBase.FrameCreated(AFrame: TCustomFrame);
begin
  // Empty default implementation
end;

function TCustomDockableFormBase.GetCaption: string;
begin
  // Empty default implementation
  Result := '';
end;

function TCustomDockableFormBase.GetEditState: TEditState;
begin
  // Empty default implementation
  Result := [];
end;

function TCustomDockableFormBase.GetMenuActionList: TCustomActionList;
begin
  // Empty default implementation
  Result := nil;
end;

function TCustomDockableFormBase.GetMenuImageList: TCustomImageList;
begin
  // Empty default implementation
  Result := nil;
end;

function TCustomDockableFormBase.GetToolBarActionList: TCustomActionList;
begin
  // Empty default implementation
  Result := nil;
end;

function TCustomDockableFormBase.GetToolBarImageList: TCustomImageList;
begin
  // Empty default implementation
  Result := nil;
end;

procedure TCustomDockableFormBase.LoadWindowState(Desktop: TCustomIniFile; const Section: string);
begin
  // Empty default implementation
end;

procedure TCustomDockableFormBase.SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
begin
  // Empty default implementation
end;

//______________________________________________________________________________________________________________________

initialization

finalization

end.
