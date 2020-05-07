unit uIPage;

interface

uses
  ComCtrls;

type
  IPage = interface(IInterface)
    ['{933DDA56-093E-4F9B-B789-2F273A3098D9}']
    procedure Clear;
    procedure Populate(Content: string; page: TPageControl);
    function GetContent(aPage: TPageControl): string;
    procedure ReplaceSSN(Primary: Boolean; SSN: string);
    procedure SearchDisplay(SearchText: string);
  end;

implementation

end.
