program lape;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main,

  lpparser, lpcompiler, lptypes, lpeval, lpinterpreter, lpmessages,
  lpvartypes, lpcodeemitter, lptree, lpdisassembler, lpvartypes_array, 
  lpvartypes_ord, lpvartypes_record, lputils, lpffi, ffi;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

