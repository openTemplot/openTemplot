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
   Contains the TLogger class.
   @version 0.5
   @author <a href="mailto:tcmiller@users.sourceforge.net">Trevor Miller</a>
  ----------------------------------------------------------------------------}
unit TLoggerUnit;

{$mode delphi}
{$h+}

interface

uses
  SysUtils, Classes, Generics.Collections,
  TLogLogUnit, TLevelUnit, TAppenderUnit, TLoggingEventUnit;

type

  {$interfaces corba}
  ILogger = interface
    ['ILogger']
    procedure SetLevel(ALevel: ILevel);
    procedure AddAppender(AAppender: IAppender);
    procedure RemoveAppender(const AName: String);
    procedure RemoveAllAppenders();
    function GetAppender(const AName: String): IAppender;
    function GetAllAppenders(): TAppendersList;
    function GetLevel(): ILevel;
    function GetName(): String;

    procedure Log(AEvent: TLoggingEvent); overload;
    procedure Log(ALevel: ILevel; const AMsg: String); overload;
    procedure Log(ALevel: ILevel; const AMsg: String; AException: Exception); overload;
    procedure Fatal(const AMsg: String);
    procedure Error(const AMsg: String);
    procedure Warn(const AMsg: String);
    procedure Info(const AMsg: String);
    procedure Debug(const AMsg: String);
    procedure Trace(const AMsg: String);
  end;

  Logger = class
  public
    class procedure Initialize();
    class procedure SetDefaultThreshold(ALevel: ILevel);
    class procedure SetDefaultAppender(AAppender: IAppender);

    class procedure FreeInstances();
    class function GetInstance(): ILogger; overload;
    class function GetInstance(const AName: String): ILogger; overload;
  end;



implementation

type
  {*----------------------------------------------------------------------------
   This is the central class in the log4delphi suite. Most logging operations,
   except configuration, are done through this class.
  ----------------------------------------------------------------------------}
  TLogger = class(TObject, ILogger)
  private
    FAppenders: TAppendersList;
    FLevel: ILevel;
    FName: String;
  public
    constructor Create(const AName: String);
    destructor Destroy; override;

    procedure SetLevel(ALevel: ILevel);
    procedure AddAppender(AAppender: IAppender);
    procedure RemoveAppender(const AName: String);
    procedure RemoveAllAppenders();
    function GetAppender(const AName: String): IAppender;
    function GetAllAppenders(): TAppendersList;
    function GetLevel(): ILevel;
    function GetName(): String;

    procedure Log(AEvent: TLoggingEvent); overload;
    procedure Log(ALevel: ILevel; const AMsg: String); overload;
    procedure Log(ALevel: ILevel; const AMsg: String; AException: Exception); overload;
    procedure Fatal(const AMsg: String);
    procedure Error(const AMsg: String);
    procedure Warn(const AMsg: String);
    procedure Info(const AMsg: String);
    procedure Debug(const AMsg: String);
    procedure Trace(const AMsg: String);
  end;


var
  instances: TObjectDictionary<String, TLogger>;
  defaultThreshold: ILevel;
  defaultAppender: IAppender;

{*----------------------------------------------------------------------------
   Initailize the loggers.
  ----------------------------------------------------------------------------}
class procedure Logger.Initialize();
begin
  defaultThreshold := TLevelUnit.INFO;
  if not assigned(instances) then
    instances := TObjectDictionary<String, TLogger>.Create;
  if not instances.ContainsKey('ROOT') then
    instances.Add('ROOT', TLogger.Create('ROOT'));
end;

{*----------------------------------------------------------------------------
   Set the default threshold.
  ----------------------------------------------------------------------------}
class procedure Logger.SetDefaultThreshold(ALevel: ILevel);
begin
  if (ALevel <> nil) then
    defaultThreshold := ALevel;
end;

{*----------------------------------------------------------------------------
   Set the default appender.
  ----------------------------------------------------------------------------}
class procedure Logger.SetDefaultAppender(AAppender: IAppender);
begin
  if (AAppender <> nil) then
    defaultAppender := AAppender;
end;

{*----------------------------------------------------------------------------
   Destroy all instances.
  ----------------------------------------------------------------------------}
class procedure Logger.FreeInstances();
var
  i: Integer;
begin
  instances.Clear;
  FreeAndNil(instances);
  TLogLogUnit.finalize;
end;

{*----------------------------------------------------------------------------
   Return a reference to the ROOT logger.
   @return Root Logger instance
  ----------------------------------------------------------------------------}
class function Logger.GetInstance(): ILogger;
begin
  Result := instances['ROOT'];
end;

{*----------------------------------------------------------------------------
   Return a reference to the named logger.
   @param AName The name of the logger
   @return Named Logger instance
  ----------------------------------------------------------------------------}
class function Logger.GetInstance(const AName: String): ILogger;
var
  index: Integer;
  log: TLogger;
begin
  if not instances.TryGetValue(AName, log) then begin
    log := TLogger.Create(AName);
    if Assigned(defaultAppender) then
       log.AddAppender(defaultAppender);
    instances.Add(AName, log);
  end;

  Result := log;
end;

{*----------------------------------------------------------------------------
   Protected constuctor to prevent instantiation. The singleton instance
   should be accessed using the getInstance method.
  ----------------------------------------------------------------------------}
constructor TLogger.Create(const AName: String);
begin
  inherited Create;
  FAppenders := TAppendersList.Create;
  FLevel := defaultThreshold;
  FName := AName;
  TLogLog.debug('Logger created - name=' + FName + ', level=' + FLevel.toString);
end;

{*----------------------------------------------------------------------------
   Protected destructor to prevent destruction. The singleton instance should
   be destroyed upon application termination using the freeInstance method.
  ----------------------------------------------------------------------------}
destructor TLogger.Destroy;
begin
  FAppenders.Clear;
  FAppenders.Free;
  TLogLog.debug('Logger destroyed - name=' + FName);
  inherited Destroy;
end;

{*----------------------------------------------------------------------------
   Set the level of this Logger.
   @param ALevel The level to set
  ----------------------------------------------------------------------------}
procedure TLogger.SetLevel(ALevel: ILevel);
begin
  Self.FLevel := ALevel;
  TLogLog.Debug('TLogger.SetLevel: ' + ALevel.ToString);
end;

{*----------------------------------------------------------------------------
   Add an appender to the list of appenders of this Logger.
   @param AAppender The appender to add
  ----------------------------------------------------------------------------}
procedure TLogger.AddAppender(AAppender: IAppender);
begin
  FAppenders.Add(AAppender);
  TLogLog.debug('Appender added to ' + FName + ', named ' + AAppender.getName);
end;

{*----------------------------------------------------------------------------
   Remove the appender from the list of appenders.
   @param AName The name of the appender to remove
  ----------------------------------------------------------------------------}
procedure TLogger.RemoveAppender(const AName: String);
begin
  FAppenders.Delete(AName);
  TLogLog.debug('Appender removed from ' + FName + ', named ' + AName);
end;

{*----------------------------------------------------------------------------
   Remove all the appenders from this logger.
  ----------------------------------------------------------------------------}
procedure TLogger.RemoveAllAppenders();
begin
  FAppenders.Clear;
  TLogLog.Debug('TLogger.RemoveAllAppenders');
end;

{*----------------------------------------------------------------------------
   Return the appender with that name if in the list. Return Nil otherwise.
   @param AName The name of the appender
   @return The appender or Nil if not found
  ----------------------------------------------------------------------------}
function TLogger.GetAppender(const AName: String): IAppender;
begin
  Result := FAppenders.FindByName(AName);
end;

{*----------------------------------------------------------------------------
   Get the appenders contained in this Logger as a TStrings instance. The
   caller should not destroy the TStrings instance.
   @return All appenders in a TStrings instance
  ----------------------------------------------------------------------------}
function TLogger.GetAllAppenders(): TAppendersList;
begin
  Result := self.FAppenders;
end;

{*----------------------------------------------------------------------------
   Returns the assigned Level, if any, for this Logger.
   @return The level of this Logger
  ----------------------------------------------------------------------------}
function TLogger.GetLevel(): ILevel;
begin
  Result := Self.FLevel;
end;

{*----------------------------------------------------------------------------
   Return the logger name.
   @return Logger name
  ----------------------------------------------------------------------------}
function TLogger.GetName(): String;
begin
  Result := Self.FName;
end;

{*----------------------------------------------------------------------------
   Send the event to all appender on condition that the event's level is
   greater or equal to this Logger's level.
   @param AEvent The logging event to log
  ----------------------------------------------------------------------------}
procedure TLogger.Log(AEvent: TLoggingEvent);
var
  i: Integer;
begin
  if (AEvent.getLevel.isGreaterOrEqual(Self.FLevel)) then
    for i := 0 to FAppenders.Count - 1 do
      FAppenders[i].doAppend(AEvent);
end;

{*----------------------------------------------------------------------------
   A generic form used to log a message. A new LoggingEvent is created and
   then destroyed upon logging the event.
   @param ALevel The level of the message to log
   @param AMsg The message to log
  ----------------------------------------------------------------------------}
procedure TLogger.Log(ALevel: ILevel; const AMsg: String);
var
  event: TLoggingEvent;
begin
  event := TLoggingEvent.Create(ALevel, AMsg, FName);
  log(event);
  event.Free;
end;

{*----------------------------------------------------------------------------
   A generic form used to log a message. A new LoggingEvent is created and
   then destroyed upon logging the event.
   @param ALevel The level of the message to log
   @param AMsg The message to log
   @param AException The Exception
  ----------------------------------------------------------------------------}
procedure TLogger.Log(ALevel: ILevel; const AMsg: String; AException: Exception);
var
  event: TLoggingEvent;
begin
  event := TLoggingEvent.Create(ALevel, AMsg, FName, AException);
  log(event);
  event.Free;
end;

{*----------------------------------------------------------------------------
   A generic form used to log a fatal message.
  ----------------------------------------------------------------------------}
procedure TLogger.Fatal(const AMsg: String);
begin
  log(TLevelUnit.FATAL, AMsg);
end;

{*----------------------------------------------------------------------------
   A generic form used to log an error message.
  ----------------------------------------------------------------------------}
procedure TLogger.Error(const AMsg: String);
begin
  log(TLevelUnit.ERROR, AMsg);
end;

{*----------------------------------------------------------------------------
   A generic form used to log a warn message.
  ----------------------------------------------------------------------------}
procedure TLogger.Warn(const AMsg: String);
begin
  log(TLevelUnit.WARN, AMsg);
end;

{*----------------------------------------------------------------------------
   A generic form used to log an info message.
  ----------------------------------------------------------------------------}
procedure TLogger.Info(const AMsg: String);
begin
  log(TLevelUnit.INFO, AMsg);
end;

{*----------------------------------------------------------------------------
   A generic form used to log a debug message.
  ----------------------------------------------------------------------------}
procedure TLogger.Debug(const AMsg: String);
begin
  log(TLevelUnit.DEBUG, AMsg);
end;

{*----------------------------------------------------------------------------
   A generic form used to log a trace message.
  ----------------------------------------------------------------------------}
procedure TLogger.Trace(const AMsg: String);
begin
  log(TLevelUnit.TRACE, AMsg);
end;

end.
