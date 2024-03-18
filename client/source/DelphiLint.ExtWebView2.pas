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
unit DelphiLint.ExtWebView2;

interface

uses
    Winapi.ActiveX
  , Winapi.WebView2
  ;

type
  COREWEBVIEW2_NAVIGATION_KIND = TOleEnum;
const
  COREWEBVIEW2_NAVIGATION_KIND_RELOAD = 0;
  COREWEBVIEW2_NAVIGATION_KIND_BACK_OR_FORWARD = 1;
  COREWEBVIEW2_NAVIGATION_KIND_NEW_DOCUMENT = 2;
type
  ICoreWebView2NavigationStartingEventArgs2 = interface(ICoreWebView2NavigationStartingEventArgs)
    function Get_AdditionalAllowedFrameAncestors(out Value: PWideChar): HRESULT;
    function Put_AdditionalAllowedFrameAncestors(Value: PWideChar): HRESULT;
  end;

  ICoreWebView2NavigationStartingEventArgs3 = interface(ICoreWebView2NavigationStartingEventArgs2)
    ['{DDFFE494-4942-4BD2-AB73-35B8FF40E19F}']
    function Get_NavigationKind(out NavigationKind: COREWEBVIEW2_NAVIGATION_KIND): HRESULT; stdcall;
  end;

  COREWEBVIEW2_COLOR = record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  ICoreWebView2Controller2 = interface(ICoreWebView2Controller)
    ['{c979903e-d4ca-4228-92eb-47ee3fa96eab}']
    function Get_DefaultBackgroundColor(out BackgroundColor: COREWEBVIEW2_COLOR): HRESULT; stdcall;
    function Put_DefaultBackgroundColor(BackgroundColor: COREWEBVIEW2_COLOR): HRESULT; stdcall;
  end;

implementation

end.
