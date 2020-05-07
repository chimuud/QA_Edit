unit uFrmDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  XMLIntf, XMLDoc, Xml.xmldom, Vcl.Grids;

type
  TFrmDemo = class(TForm)
    PanelDemo: TPanel;
    XML: TXMLDocument;
    GridDemo: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDemoFile: string;
    procedure PopulateGrid;
    procedure ProcessNode(Node: IXMLNode);
  public
    property DemoFile: string read FDemoFile write FDemoFile;
    procedure CreateDemo(aDemoFile: string);
    procedure OpenDemo(aDemoFile: string);
  end;

var
  FrmDemo: TFrmDemo;

implementation

{$R *.dfm}

const
  cLF = #13#10;

{ TFrmDemo }

procedure TFrmDemo.CreateDemo(aDemoFile: string);
begin
  XML.Active := False;
  XML.XML.Text :=
    '<?xml version="1.0" encoding="utf-8" ?>' + cLF +
    '<Demo>' + cLF +
    '</Demo>';
  XML.Active := True;
  XML.SaveToFile(aDemoFile);
end;

procedure TFrmDemo.FormCreate(Sender: TObject);
begin
  XML := TXMLDocument.Create(nil);
end;

procedure TFrmDemo.FormDestroy(Sender: TObject);
begin
  XML.Free;
end;

procedure TFrmDemo.OpenDemo(aDemoFile: string);
begin
  DemoFile := aDemoFile;
  XML.LoadFromFile(FDemoFile);
  PopulateGrid;
end;

procedure TFrmDemo.PopulateGrid;
var
  i: Integer;
  NodeList: IXMLNodeList;
  Node: IXMLNode;
begin
  NodeList := XML.ChildNodes;
  for i := 0 to NodeList.Count - 1 do
  begin
    Node := NodeList.Get(i);
    ProcessNode(Node);
  end;
end;

procedure TFrmDemo.ProcessNode(Node: IXMLNode);
var
  i: Integer;
  AttributeNodes: IXMLNodeList;
begin
  AttributeNodes := Node.AttributeNodes;
  for i := 0 to AttributeNodes.Count - 1 do begin
  end;
end;

end.
