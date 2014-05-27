{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Lape exceptions.
}
unit lpexceptions;

{$I lape.inc}

interface

uses
  SysUtils,
  lptypes;

type
  lpException = class(Exception)
  private
    FOldMsg: string;
    FDocPos: TDocPos;
  public
    constructor Create(Msg: string; OldMsg: string = ''); overload;
    constructor Create(Msg: string; DocPos: TDocPos; OldMsg: string = ''); overload;
    property OldMsg: string read FOldMsg;
    property DocPos: TDocPos read FDocPos;
  end;

resourcestring
  lpeArrayLengthsDontMatch = 'Length of arrays (%s) don''t match';
  lpeAssertionFailure = 'Assertion failure';
  lpeAssertionFailureMsg = 'Assertion failure: %s';
  lpeBlockExpected = 'Block expected';
  lpeCannotAssign = 'Target cannot be assigned to';
  lpeCannotBreak = 'Cannot break out of this statement';
  lpeCannotContinue = 'Cannot use continue in this context';
  lpeCannotEvalConst = 'Cannot be evaluated as constant';
  lpeCannotEvalConstProc = 'Procedures cannot be used for constant evaluation';
  lpeCannotEvalRunTime = 'Cannot be evaluated at runtime';
  lpeCannotInvoke = 'Cannot invoke identifier';
  lpeCannotOverload = 'Cannot overload function';
  lpeClosingParenthesisExpected = 'Closing parenthesis expected';
  lpeConditionalNotClosed = 'Conditional statement not properly closed';
  lpeConstantExpected = 'Constant expression expected';
  //lpeDeclarationOutOfScope = 'Declaration "%s" out of scope';
  lpeDefaultToMoreThanOne = 'Runtime default value can only be assigned to one variable';
  lpeDuplicateDeclaration = 'Duplicate declaration "%s"';
  lpeExceptionAt = '%s at line %d, column %d';
  lpeExceptionIn = '%s in file "%s"';
  lpeExpected = '%s expected';
  lpeExpectedOther = 'Found unexpected token "%s", expected "%s"';
  lpeExpressionExpected = 'Expression expected';
  lpeFileNotFound = 'File "%s" not found';
  lpeImpossible = 'It''s impossible!';  //Should be "Unexpected error at line: ..."?
  lpeIncompatibleAssignment = 'Can''t assign "%s" to "%s"';
  lpeIncompatibleOperator = 'Operator "%s" not compatible with types';
  lpeIncompatibleOperator1 = 'Operator "%s" not compatible with "%s"';
  lpeIncompatibleOperator2 = 'Operator "%s" not compatible with types "%s" and "%s"';
  lpeInvalidAssignment = 'Invalid assignment';
  lpeInvalidCast = 'Invalid cast';
  lpeInvalidCondition = 'Invalid condition';
  lpeInvalidEvaluation = 'Invalid evaluation';
  lpeInvalidForward = 'Forwarded declaration "%s" not resolved';
  lpeInvalidLabel = 'Invalid label';
  lpeInvalidIndex = 'Invalid index "%s"';
  lpeInvalidIterator = 'Variable cannot be used for iteration';
  lpeInvalidJump = 'Invalid jump';
  lpeInvalidRange = 'Expression is not a valid range';
  lpeInvalidValueForType = 'Invalid value for type "%s"';
  lpeInvalidWithReference = 'Invalid with-reference';
  lpeLostClosingParenthesis = 'Found closing parenthesis without matching opening parenthesis';
  lpeLostConditional = 'Found conditional without matching opening statement';
  lpeNoDefaultForParam = 'No default value for parameter %d found';
  lpeNoForwardMatch = 'Forwarded declaration doesn''t match';
  lpeNoOverloadedMethod = 'Don''t know which overloaded method to call with params (%s)';
  lpeOperatorExpected = 'Operator expected';
  lpeOutOfStackRange = 'Out of stack range';
  lpeOutOfTypeRange = 'Out of type range';
  lpeParentOutOfScope = 'Parent declaration is out of scope';
  lpeRuntime = 'Runtime error: "%s"';
  lpeStatementNotAllowed = 'Statement not allowed here';
  lpeTooMuchParameters = 'Too many parameters found';
  lpeTypeExpected = 'Type expected';
  lpeUnexpectedToken = 'Found unexpected token "%s"';
  lpeUnknownDeclaration = 'Unknown declaration "%s"';
  lpeUnknownDirective = 'Unknown compiler directive';
  lpeUnknownOC = 'Unknown opcode';
  lpeUnknownParent = 'Cannot find parent declaration';
  lpeVariableExpected = 'Variable expected';
  lpeVariableOfTypeExpected = 'Expected variable of type "%s", got "%s"';
  lpeWrongNumberParams = 'Wrong number of parameters found, expected %d';
  {$IFDEF Lape_NativeKeyword}lpeNativeFFIMissing = 'The native keyword requires libffi';{$ENDIF}

procedure LapeException(Msg: lpString); overload;
procedure LapeException(Msg: lpString; DocPos: TDocPos); overload;
procedure LapeException(Msg: lpString; DocPos: array of TLapeBaseDeclClass); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: TDocPos); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: array of TLapeBaseDeclClass); overload;

implementation

constructor lpException.Create(Msg: string; OldMsg: string = '');
begin
  inherited Create(Msg);
  with FDocPos do
  begin
    Line := -1;
    Col := 0;
    Filename := '';
  end;

  FOldMsg := OldMsg;
  if (FOldMsg = '') then
    FOldMsg := Msg;
end;

constructor lpException.Create(Msg: string; DocPos: TDocPos; OldMsg: string = '');
begin
  Create(Msg, OldMsg);
  FDocPos := DocPos;
end;

{$IF DEFINED(Delphi) AND (CompilerVersion <= 21.00)}
function ReturnAddress: Pointer;
asm
  MOV  EAX, [EBP+4]
end;
{$IFEND}

procedure _RaiseLapeException(e: lpException); inline;
{$IFDEF FPC}
begin
  raise e at get_caller_addr(get_frame);
end;
{$ELSE}
begin
  raise e at ReturnAddress;
end;
{$ENDIF}

procedure _LapeException(Msg: lpString);
begin
  _RaiseLapeException(lpException.Create(Msg));
end;

procedure _LapeException(e: lpException); overload;
begin
  _RaiseLapeException(e);
end;

function FormatLocation(Msg: lpString; DocPos: TDocPos): lpString; {inline;}
begin
  Result := Msg;
  if (DocPos.Line > 0) and (DocPos.Col > 0) then
    Result := Format(lpeExceptionAt, [Result, DocPos.Line, DocPos.Col]);
  if (DocPos.FileName <> '') then
    Result := Format(lpeExceptionIn, [Result, DocPos.FileName]);
end;

procedure LapeException(Msg: lpString);
begin
  _LapeException(Msg);
end;

procedure LapeException(Msg: lpString; DocPos: TDocPos);
begin
  _LapeException(lpException.Create(FormatLocation(Msg, DocPos), DocPos, Msg));
end;

procedure LapeException(Msg: lpString; DocPos: array of TLapeBaseDeclClass);
var
  i: Integer;
begin
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line)
    then
    begin
      _LapeException(lpException.Create(FormatLocation(Msg, DocPos[i].DocPos), DocPos[i].DocPos, Msg));
      Exit;
    end;
  _LapeException(Msg);
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const);
begin
  _LapeException(Format(Msg, Args));
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: TDocPos);
begin
  _LapeException(lpException.Create(FormatLocation(Format(Msg, Args), DocPos), DocPos, Format(Msg, Args)));
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: array of TLapeBaseDeclClass);
var
  i: Integer;
begin
  Msg := Format(Msg, Args);
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line)
    then
    begin
      _LapeException(lpException.Create(FormatLocation(Msg, DocPos[i].DocPos), DocPos[i].DocPos, Msg));
      Exit;
    end;
  _LapeException(Msg);
end;

end.
