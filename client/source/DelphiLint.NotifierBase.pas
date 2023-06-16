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
unit DelphiLint.NotifierBase;

interface

uses
    ToolsAPI
  , System.Classes
  , DockForm
  , Vcl.Graphics
  , Winapi.Windows
  , System.SysUtils
  , DelphiLint.Events
  ;

type
  TNotifierBase = class abstract(TNotifierObject)
  private
    FOnOwnerFreed: TEventNotifier<TNotifierBase>;
    FOnReleased: TEventNotifier<TNotifierBase>;
  protected
    procedure Destroyed;
  public
    constructor Create;
    procedure Release;

    property OnOwnerFreed: TEventNotifier<TNotifierBase> read FOnOwnerFreed;
    property OnReleased: TEventNotifier<TNotifierBase> read FOnReleased;
  end;

  TEditorNotifierBase = class abstract(TNotifierBase, IOTANotifier, IOTAEditorNotifier, INTAEditServicesNotifier)
  public
    procedure ViewActivated(const View: IOTAEditView); virtual;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation); virtual;
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean); virtual;
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation); virtual;
    procedure WindowActivated(const EditWindow: INTAEditWindow); virtual;
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean); virtual;
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); virtual;
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); virtual;
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
  end;

  TViewNotifierBase = class abstract(TNotifierBase, IOTANotifier, INTAEditViewNotifier)
  public
    procedure EditorIdle(const View: IOTAEditView); virtual;
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean); virtual;
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize); virtual;
    procedure EndPaint(const View: IOTAEditView); virtual;
  end;

  TEditLineNotifierBase = class abstract(TNotifierBase, IOTAEditLineNotifier)
  public
    procedure LineChanged(OldLine: Integer; NewLine: Integer; Data: Integer); virtual;
  end;

  TMessageNotifierBase = class abstract(TNotifierBase, IOTAMessageNotifier)
  public
    procedure MessageGroupAdded(const Group: IOTAMessageGroup); virtual;
    procedure MessageGroupDeleted(const Group: IOTAMessageGroup); virtual;
  end;

  TModuleNotifierBase = class abstract(TNotifierBase, IOTAModuleNotifier90, IOTAModuleNotifier)
  public
    // IOTAModuleNotifier
    function CheckOverwrite: Boolean; virtual;
    procedure ModuleRenamed(const NewName: string); virtual;

    // IOTAModuleNotifier80
    function AllowSave: Boolean; virtual;
    function GetOverwriteFileNameCount: Integer; virtual;
    function GetOverwriteFileName(Index: Integer): string; virtual;
    procedure SetSaveFileName(const FileName: string); virtual;

    property OverwriteFileNameCount: Integer read GetOverwriteFileNameCount;
    property OverwriteFileNames[Index: Integer]: string read GetOverwriteFileName;

    // IOTAModuleNotifier90
    procedure BeforeRename(const OldFileName, NewFileName: string); virtual;
    procedure AfterRename(const OldFileName, NewFileName: string); virtual;
  end;

implementation

//______________________________________________________________________________________________________________________

constructor TNotifierBase.Create;
begin
  FOnOwnerFreed := TEventNotifier<TNotifierBase>.Create;
  FOnReleased := TEventNotifier<TNotifierBase>.Create;
end;

procedure TNotifierBase.Destroyed;
begin
  FOnOwnerFreed.Notify(Self);
end;

procedure TNotifierBase.Release;
begin
  FOnReleased.Notify(Self);
end;

//______________________________________________________________________________________________________________________

procedure TEditorNotifierBase.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.ViewActivated(const View: IOTAEditView);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.WindowActivated(const EditWindow: INTAEditWindow);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin
  // Empty default implementation
end;

procedure TEditorNotifierBase.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin
  // Empty default implementation
end;

//______________________________________________________________________________________________________________________

procedure TViewNotifierBase.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  // Empty default implementation
end;

procedure TViewNotifierBase.EditorIdle(const View: IOTAEditView);
begin
  // Empty default implementation
end;

procedure TViewNotifierBase.EndPaint(const View: IOTAEditView);
begin
  // Empty default implementation
end;

procedure TViewNotifierBase.PaintLine(const View: IOTAEditView; LineNumber: Integer; const LineText: PAnsiChar;
  const TextWidth: Word; const LineAttributes: TOTAAttributeArray; const Canvas: TCanvas; const TextRect,
  LineRect: TRect; const CellSize: TSize);
begin
  // Empty default implementation
end;

//______________________________________________________________________________________________________________________

procedure TEditLineNotifierBase.LineChanged(OldLine, NewLine, Data: Integer);
begin
  // Empty default implementation
end;

//______________________________________________________________________________________________________________________

procedure TMessageNotifierBase.MessageGroupAdded(const Group: IOTAMessageGroup);
begin
  // Empty default implementation
end;

procedure TMessageNotifierBase.MessageGroupDeleted(const Group: IOTAMessageGroup);
begin
  // Empty default implementation
end;

//______________________________________________________________________________________________________________________

procedure TModuleNotifierBase.AfterRename(const OldFileName, NewFileName: string);
begin
  // Empty default implementation
end;

function TModuleNotifierBase.AllowSave: Boolean;
begin
  // Empty default implementation
  Result := True;
end;

procedure TModuleNotifierBase.BeforeRename(const OldFileName, NewFileName: string);
begin
  // Empty default implementation
end;

function TModuleNotifierBase.CheckOverwrite: Boolean;
begin
  // Empty default implementation
  Result := True;
end;

function TModuleNotifierBase.GetOverwriteFileName(Index: Integer): string;
begin
  // Empty default implementation
  Result := '';
end;

function TModuleNotifierBase.GetOverwriteFileNameCount: Integer;
begin
  // Empty default implementation
  Result := 0;
end;

procedure TModuleNotifierBase.ModuleRenamed(const NewName: string);
begin
  // Empty default implementation
end;

procedure TModuleNotifierBase.SetSaveFileName(const FileName: string);
begin
  // Empty default implementation
end;

end.
