{
DelphiLint Client
Copyright (C) 2024 Integrated Application Development

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.IDEBaseTypes;

interface

uses
    System.IniFiles
  , Vcl.Menus
  , Vcl.Forms
  , Vcl.ActnList
  , Vcl.ImgList
  , Vcl.ComCtrls
{$IFDEF TOOLSAPI}
  , ToolsAPI
  , DockForm
  , DesignIntf
{$ENDIF}
  ;

type
  TCustomDockableFormBase = class abstract(TInterfacedObject{$IFDEF TOOLSAPI}, INTACustomDockableForm{$ENDIF})
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
{$IFDEF TOOLSAPI}
    function GetEditState: TEditState; virtual;
    function EditAction(Action: TEditAction): Boolean; virtual;
{$ENDIF}
  end;

  TAddInOptionsBase = class abstract(TInterfacedObject{$IFDEF TOOLSAPI}, INTAAddInOptions{$ENDIF})
    function GetArea: string; virtual;
    function GetCaption: string; virtual;
    function GetFrameClass: TCustomFrameClass; virtual; abstract;
    procedure FrameCreated(AFrame: TCustomFrame); virtual;
    procedure DialogClosed(Accepted: Boolean); virtual;
    function ValidateContents: Boolean; virtual;
    function GetHelpContext: Integer; virtual;
    function IncludeInIDEInsight: Boolean; virtual;
  end;



implementation

//______________________________________________________________________________________________________________________

procedure TAddInOptionsBase.DialogClosed(Accepted: Boolean);
begin
  // Empty default implementation
end;

procedure TAddInOptionsBase.FrameCreated(AFrame: TCustomFrame);
begin
  // Empty default implementation
end;

function TAddInOptionsBase.GetArea: string;
begin
  // Empty default implementation
  Result := ''; // Third Party
end;

function TAddInOptionsBase.GetCaption: string;
begin
  // Empty default implementation
  Result := '';
end;

function TAddInOptionsBase.GetHelpContext: Integer;
begin
  // Empty default implementation
  Result := 0;
end;

function TAddInOptionsBase.IncludeInIDEInsight: Boolean;
begin
  // Empty default implementation
  Result := True;
end;

function TAddInOptionsBase.ValidateContents: Boolean;
begin
  // Empty default implementation
  Result := True;
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

procedure TCustomDockableFormBase.FrameCreated(AFrame: TCustomFrame);
begin
  // Empty default implementation
end;

function TCustomDockableFormBase.GetCaption: string;
begin
  // Empty default implementation
  Result := '';
end;

{$IFDEF TOOLSAPI}
function TCustomDockableFormBase.GetEditState: TEditState;
begin
  // Empty default implementation
  Result := [];
end;

function TCustomDockableFormBase.EditAction(Action: TEditAction): Boolean;
begin
  // Empty default implementation
  Result := True;
end;
{$ENDIF}

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

end.
