unit UHTMLExportUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStringUtils;

type
  THTMLExport = class
  private
    FOutput: Text;
    FTabs: integer;
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure OpTag(Tag: string; Params: string = '');
    procedure TextOut(AText: string);
    procedure ClTag(Tag: string);
  end;

implementation

constructor THTMLExport.Create(FileName: string);
begin
  Assign(FOutput, FileName);
  Rewrite(FOutput);
  FTabs := 0;
end;

destructor THTMLExport.Destroy;
begin
  Close(FOutput);
  inherited Destroy;
end;

procedure THTMLExport.OpTag(Tag: string; Params: string = '');
begin
  if Params <> '' then
    Tag += ' ';
  Writeln(FOutput, Tabs(FTabs) + '<' + Tag + Params + '>');
  FTabs += 1;
end;

procedure THTMLExport.TextOut(AText: string);
begin
  Writeln(FOutput, Tabs(FTabs) + AText);
end;

procedure THTMLExport.ClTag(Tag: string);
begin
  FTabs -= 1;
  Writeln(FOutput, Tabs(FTabs) + '</' + Tag + '>');
end;

end.

