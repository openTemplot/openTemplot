{
   Copyright 2005-2006 Log4Delphi Project

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
}
{*----------------------------------------------------------------------------
   Contains the TDbAppender class.
   @version 0.5
   @author <a href="mailto:Bartosz.Tomasik@gmail.com">Bartosz Tomasik</a>
   @author <a href="mailto:tcmiller@users.sourceforge.net">Trevor Miller</a>
  ----------------------------------------------------------------------------}
unit TDBAppenderUnit;

{$ifdef fpc}
  {$mode objfpc}
  {$h+}
{$endif}

interface

uses
  TDBLogInserterUnit, TAppenderUnit, TLoggingEventUnit, TLogLogUnit;

type
{*----------------------------------------------------------------------------
   DbAppender appends log events to a database.
  ----------------------------------------------------------------------------}
  TDbAppender = class(TAppender)
  protected
    FDBLogInserter: TDBLogInserter;
  public
    constructor Create; overload;
    constructor Create(ADBLogInserter: TDBLogInserter); overload;
    destructor Destroy; override;

    procedure Close();
    procedure Append(AEvent: TLoggingEvent); override;
    procedure SetDbLogInserter(ADBLogInserter: TDBLogInserter);
    function RequiresLayout(): Boolean; override;
  end;

implementation

constructor TDbAppender.Create();
begin
  inherited Create;
  Self.FName := ClassName;
  Self.FDBLogInserter := nil;
  TLogLog.debug('TDBAppender#Create');
end;

constructor TDbAppender.Create(ADBLogInserter: TDBLogInserter);
begin
  inherited Create;
  Self.FName := ClassName;
  Self.FDBLogInserter := ADBLogInserter;
  TLogLog.debug('TDBAppender#Create');
end;

destructor TDbAppender.Destroy;
begin
  if not Self.FClosed then
    Self.Close;
  FDBLogInserter.Free;
  TLogLog.debug('TDBAppender#Destroy');
  inherited Destroy;
end;

procedure TDbAppender.Close();
begin
  if not (Self.FClosed) then
    Self.FClosed := True;
end;

procedure TDbAppender.Append(AEvent: TLoggingEvent);
begin
  if (Self.FClosed) then begin
    if (Self.FErrorHandler <> nil) then
      Self.FErrorHandler.Error(
        'This appender is closed and cannot be written to.');
    Exit;
  end;

  if Assigned(FDBLogInserter) then
    FDBLogInserter.doInsert(AEvent);
end;

procedure TDbAppender.SetDbLogInserter(ADBLogInserter: TDBLogInserter);
begin
  if Assigned(FDBLogInserter) then
    Self.FDBLogInserter.Free;
  Self.FDBLogInserter := ADBLogInserter;
end;

function TDbAppender.RequiresLayout(): Boolean;
begin
  Result := False;
end;

end.
