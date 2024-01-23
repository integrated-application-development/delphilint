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
