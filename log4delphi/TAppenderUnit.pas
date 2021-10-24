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
   Contains the TAppender class.
   @version 0.5
   @author <a href="mailto:tcmiller@users.sourceforge.net">Trevor Miller</a>
  ----------------------------------------------------------------------------}
unit TAppenderUnit;

{$mode delphi}
{$h+}

interface

uses
  Classes,
  Generics.Collections,
  TLevelUnit, TLayoutUnit, TLoggingEventUnit, TErrorHandlerUnit;

type
  {$interfaces corba}
  IAppender = interface
    ['IAppender']
    procedure DoAppend(AEvent: TLoggingEvent);

    function GetName(): String;
    function GetLayout(): TLayout;

    procedure SetName(AName: String);
    procedure SetLayout(ALayout: TLayout);

    function RequiresLayout(): Boolean;
  end;

{*----------------------------------------------------------------------------
   Implement this abstract class with specific strategies for outputting
   log statements.
  ----------------------------------------------------------------------------}
  TAppender = class(IAppender)
  private
  protected
    FLayout: TLayout;
    FThreshold: ILevel;
    FErrorHandler: TErrorHandler;
    FName: String;
    FClosed: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(AEvent: TLoggingEvent); virtual; abstract;

    procedure DoAppend(AEvent: TLoggingEvent);
    procedure SetLayout(ALayout: TLayout); virtual;
    procedure SetName(AName: String);
    procedure SetThreshold(AThreshold: ILevel);
    procedure SetErrorHandler(AHandler: TErrorHandler);

    function GetLayout(): TLayout;
    function GetName(): String;
    function GetThreshold(): ILevel;
    function GetErrorHandler(): TErrorHandler;
    function IsAsSevereAsThreshold(ALevel: ILevel): Boolean;
    function RequiresLayout(): Boolean; virtual;
  end;

  TAppendersRegistry = class(TObjectDictionary<String, TAppender>)
  end;

  TAppendersList = class(TList<IAppender>)
  private
    function IndexOf(const AName: String): Integer; overload;
  public
    function Add(constref AAppender: IAppender): SizeInt; override;
    procedure Delete(const AName: String); overload;
    function FindByName(const AName: String): IAppender;
  end;

implementation

uses
  SysUtils,
  TLogLogUnit;

{*----------------------------------------------------------------------------
   Create an instance.
  ----------------------------------------------------------------------------}
constructor TAppender.Create;
begin
  inherited Create;
  Self.FName := Self.ClassName;
  TLogLog.debug('TAppender#Create');
end;

{*----------------------------------------------------------------------------
   Destruct this instance by freeing the layout and error handler.
  ----------------------------------------------------------------------------}
destructor TAppender.Destroy;
begin
  Self.FLayout.Free;
  Self.FErrorHandler.Free;
  TLogLog.debug('TAppender#Destroy: Appender destroyed - name=' + Self.FName);
  inherited Destroy;
end;

{*----------------------------------------------------------------------------
   Log in Appender specific way. When appropriate, Loggers will call the
   append method of appender implementations in order to log.
   @param AEvent The logging event to log
  ----------------------------------------------------------------------------}
procedure TAppender.DoAppend(AEvent: TLoggingEvent);
begin
  if ((not Self.FClosed) and (Self.IsAsSevereAsThreshold(AEvent.GetLevel))) then
    Self.Append(AEvent);
end;

{*----------------------------------------------------------------------------
   Set the Layout for this appender to use.
   @param ALayout The layout this appender uses
  ----------------------------------------------------------------------------}
procedure TAppender.SetLayout(ALayout: TLayout);
begin
  Self.FLayout := ALayout;
  TLogLog.debug('TAppender#SetLayout: ' + ALayout.ClassName);
end;

{*----------------------------------------------------------------------------
   Set the name of this appender. The name is used by other components to
   identify this appender.
   @param AName The name of this appender
  ----------------------------------------------------------------------------}
procedure TAppender.SetName(AName: String);
begin
  Self.FName := AName;
  TLogLog.debug('TAppender#SetName: ' + AName);
end;

{*----------------------------------------------------------------------------
   Set the threshold level for this appender to use.
   @param AThreshold The threshold level this appender uses
  ----------------------------------------------------------------------------}
procedure TAppender.SetThreshold(AThreshold: ILevel);
begin
  Self.FThreshold := AThreshold;
  TLogLog.debug('TAppender#SetThreshold: ' + AThreshold.ToString);
end;

{*----------------------------------------------------------------------------
   Set the ErrorHandler for this appender to use.
   @param AHandler The error handler for this appender
  ----------------------------------------------------------------------------}
procedure TAppender.SetErrorHandler(AHandler: TErrorHandler);
begin
  Self.FErrorHandler := AHandler;
  TLogLog.debug('TAppender#SetErrorHandler: ' + AHandler.ClassName);
end;

{*----------------------------------------------------------------------------
   Returns this appenders layout.
   @return The layout of this appender
  ----------------------------------------------------------------------------}
function TAppender.GetLayout(): TLayout;
begin
  Result := Self.FLayout;
end;

{*----------------------------------------------------------------------------
   Get the name of this appender. The name uniquely identifies the appender.
   @return The name of this appender
  ----------------------------------------------------------------------------}
function TAppender.GetName(): String;
begin
  Result := Self.FName;
end;

{*----------------------------------------------------------------------------
   Returns this appender's threshold level.
   @return The threshold level of this appender
  ----------------------------------------------------------------------------}
function TAppender.getThreshold(): ILevel;
begin
  Result := Self.FThreshold;
end;

{*----------------------------------------------------------------------------
   Return the currently set ErrorHandler for this appender.
   @return The error handler of this appender
  ----------------------------------------------------------------------------}
function TAppender.getErrorHandler(): TErrorHandler;
begin
  Result := Self.FErrorHandler;
end;

{*----------------------------------------------------------------------------
   Check whether the message level is below the appender's threshold. If
   there is no threshold set, then the return value is always true.
   @param ALevel The level to check against
   @return True if this appenders level is greater than or equal to the
      given level, false otherwise
  ----------------------------------------------------------------------------}
function TAppender.IsAsSevereAsThreshold(ALevel: ILevel): Boolean;
begin
  Result := ((Self.FThreshold = nil) or (ALevel.IsGreaterOrEqual(Self.FThreshold)));
end;

{*----------------------------------------------------------------------------
   Determine if the appender requires a layout or not. The default value is
   false, appenders that require a layout will override this method.
   @return True if this appender requires a layout, flase otherwise
  ----------------------------------------------------------------------------}
function TAppender.RequiresLayout(): Boolean;
begin
  Result := False;
end;

{ TAppendersList }

function TAppendersList.Add(constref AAppender: IAppender): SizeInt;
var
  i: Integer;
begin
  i := IndexOf(AAppender);
  if (i = -1) then
    Result := inherited Add(AAppender)
  else
    Result := i;
end;

function TAppendersList.FindByName(const AName: String): IAppender;
var
  index: Integer;
begin
  index := IndexOf(AName);
  if index = -1 then begin
    Result := nil;
    Exit;
  end;

  Result := Items[index];
end;

function TAppendersList.IndexOf(const AName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    if not SameText(Items[i].GetName, AName) then
      continue;

    Result := i;
    Exit;
  end;

  Result := -1;
end;

procedure TAppendersList.Delete(const AName: String);
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i <> -1 then
    Delete(i);
end;

end.
