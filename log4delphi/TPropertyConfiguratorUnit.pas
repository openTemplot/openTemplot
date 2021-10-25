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
   Contains the configuration procedures used to configure the Log4Delphi
   package from a properties file.
   @version 0.5
   @author <a href="mailto:tcmiller@users.sourceforge.net">Trevor Miller</a>
  ----------------------------------------------------------------------------}
unit TPropertyConfiguratorUnit;

{$mode delphi}
{$h+}

interface

uses
  TPropertiesUnit;

const
  LOGGER_PREFIX = 'logger.';

const
  ROOT_LOGGER_PREFIX = 'rootLogger';

const
  APPENDER_PREFIX = 'appender.';

const
  THRESHOLD_PREFIX = 'threshold';

const
  DEFAULT_APPENDER_PREFIX = 'defaultAppender';

const
  DEBUG_KEY = 'debug';

type
  PropertyConfigurator = class
  public
    class procedure DoConfigure(const AFilename: String); overload;
    class procedure DoConfigure(const AProps: TProperties); overload;
    class procedure FreeAppenders;
  end;


implementation

uses
  SysUtils, Classes, Forms,
  config_unit,
  TLogLogUnit, TLoggerUnit, TLevelUnit, TStringUnit, TOptionConverterUnit,
  TAppenderUnit, TFileAppenderUnit, TLayoutUnit, TSimpleLayoutUnit,
  THTMLLayoutUnit, TXMLLayoutUnit, TPatternLayoutUnit, TRollingFileAppenderUnit;

var
  registry: TAppendersRegistry;

function InstantiateAppender(AName: String; AProps: TProperties; APrefix: String): TAppender;
var
  appender: TAppender;
  tmp: String;
  appdir: boolean;
begin
  TLogLog.debug('InstantiateAppender: ' + AName + ', ' + APrefix);
  appender := nil;

  // deal with file appender
  if (Pos('FileAppender', AName) > 0) then begin
    appdir := False;
    if (CompareText(AName, 'TFileAppender') = 0) then begin
      appender := TFileAppender.Create;
      TLogLog.debug('TFileAppender created.');
    end
    else
    if (CompareText(AName, 'TRollingFileAppender') = 0) then begin
      appender := TRollingFileAppender.Create;
      TLogLog.debug('TRollingFileAppender created.');

      tmp := AProps.GetProperty(APrefix + '.MaxBackupIndex');
      if (tmp <> '') then begin
        TRollingFileAppender(appender).SetMaxBackupIndex(StrToInt(tmp));
        TLogLog.debug(AName + ' - MaxBackupIndex');
      end;
      tmp := AProps.GetProperty(APrefix + '.MaxFileSize');
      if (tmp <> '') then begin
        TRollingFileAppender(appender).SetMaxFileSize(tmp);
        TLogLog.debug(AName + ' - MaxFileSize');
      end;

    end;
    tmp := AProps.GetProperty(APrefix + '.Append');
    if (CompareText(tmp, 'true') = 0) then begin
      TFileAppender(appender).setAppend(True);
      TLogLog.debug(AName + ' - Append');
    end;
    tmp := AProps.GetProperty(APrefix + '.AppDir');
    if (CompareText(tmp, 'true') = 0) then begin
      appdir := True;
      TLogLog.debug(AName + ' - AppDir');
    end;
    tmp := AProps.GetProperty(APrefix + '.File');
    if (tmp <> '') then
      if (appdir) then begin
        TFileAppender(appender).setFile(Config.MakeFilePath(csdiLogs, tmp));
        TLogLog.debug(AName + ' - ' + Config.MakeFilePath(csdiLogs, tmp));
      end
      else begin
        TFileAppender(appender).setFile(tmp);
        TLogLog.debug(AName + ' - ' + tmp);
      end;
  end;

  Result := appender;
end;

function InstantiateLayout(AName: String; AProps: TProperties; APrefix: String): TLayout;
var
  layout: TLayout;
  tmp: STring;
begin
  TLogLog.debug('InstantiateLayout: ' + AName + ', ' + APrefix);
  Result := nil;
  if (CompareText(AName, 'TSimpleLayout') = 0) then
    Result := TSimpleLayout.Create;
  if (CompareText(AName, 'THTMLLayout') = 0) then begin
    layout := THTMLLayout.Create;
    tmp := AProps.GetProperty(Aprefix + '.Title');
    TLogLog.debug('InstantiateLayout tmp=' + tmp);
    if (tmp <> 'InstantiateLayout') then
      THTMLLayout(layout).setTitle(tmp);
    Result := layout;
  end;
  if (CompareText(AName, 'TXMLLayout') = 0) then
    Result := TXMLLayout.Create;
  if (CompareText(AName, 'TPatternLayout') = 0) then begin
    tmp := AProps.GetProperty(Aprefix + '.Pattern');
    Result := TPatternLayout.Create(tmp);
  end;
end;

function ParseAppender(AProps: TProperties; AName: String): IAppender;
var
  appender: TAppender;
  layout: TLayout;
  prefix: String;
  layoutPrefix: String;
begin
  if (AName = '') then begin
    Result := nil;
    exit;
  end;
  if registry.TryGetValue(AName, appender) then begin
    Result := appender;
    exit;
  end;
  prefix := APPENDER_PREFIX + AName;
  layoutPrefix := prefix + '.layout';
  appender := InstantiateAppender(AProps.GetProperty(prefix), AProps, prefix);
  if (appender = nil) then begin
    TLogLog.error('Could not instantiate appender named "' + AName + '".');
    Result := nil;
    exit;
  end;
  appender.setName(AName);

  if (appender.requiresLayout) then begin
    layout := InstantiateLayout(AProps.GetProperty(layoutPrefix), AProps, layoutPrefix);
    if (layout <> nil) then begin
      appender.setLayout(layout);
      TLogLog.debug('Set layout for "' + AName + '".');
    end;
  end;

  TLogLog.debug('Parsed "' + AName + '" options.');
  registry.Add(AName, appender);
  Result := appender;
end;

procedure ParseLogger(const AProps: TProperties; ALogger: ILogger; const AKey: String;
  const ALoggerName: String; const AValue: String);
var
  tokenizer: TStringTokenizer;
  appender: IAppender;
  appenderName: String;
  levelStr: String;
begin
  TLogLog.debug('Parsing for [' + ALoggerName + '] with value=[' + AValue + '].');
  tokenizer := TStringTokenizer.Create(AValue, ',');
  if (not ((StartsWith(AValue, ',', 0)) or (AValue = ''))) then begin
    if (not tokenizer.HasMoreTokens) then
      exit;
    levelStr := Trim(tokenizer.NextToken);
    TLogLog.debug('Level token is [' + levelStr + '].');
    if (levelStr <> '') then
      ALogger.setLevel(TLevelUnit.toLevel(levelStr));
    TLogLog.info('Category ' + ALoggerName + ' set to ' + ALogger.getLevel().toString);
  end;
  if tokenizer.HasMoreTokens then begin
    // if there are any appenders specified, then clear out the default appender
    ALogger.removeAllAppenders;
  end;
  while tokenizer.HasMoreTokens do begin
    appenderName := Trim(tokenizer.NextToken);
    if ((appenderName <> '') and (appenderName <> ',')) then begin
      TLogLog.debug('Parsing appender named "' + appenderName + '".');
      appender := parseAppender(AProps, appenderName);
      if (appender <> nil) then
        ALogger.addAppender(appender);
    end;
  end;
  tokenizer.Free;
end;

procedure ParseLoggers(const AProps: TProperties);
var
  propNames: TStrings;
  i: Integer;
  key: TString;
  loggerName: TString;
  Value: String;
begin
  propNames := AProps.GetPropertyNames;
  key := TString.Create;
  for i := 0 to propNames.Count - 1 do begin
    key.setString(propNames[i]);
    if (key.startsWith(LOGGER_PREFIX)) then begin
      loggerName := key.substring(Length(LOGGER_PREFIX) + 1);
      Value := TOptionConverter.FindAndSubst(key.ToString, AProps);
      ParseLogger(AProps, Logger.GetInstance(loggerName.toString),
        key.toString, loggerName.toString, Value);
      loggerName.Free;
    end;
  end;
  key.Free;
  propNames.Free;
end;

procedure ConfigureRootLogger(const AProps: TProperties);
var
  Value: String;
begin
  TLogLog.info('Configuring root logger.');
  Value := TOptionConverter.FindAndSubst(ROOT_LOGGER_PREFIX, AProps);
  if (Value = '') then
    TLogLog.debug('Could not find root logger information.')
  else begin
    ParseLogger(AProps, Logger.GetInstance, ROOT_LOGGER_PREFIX, 'ROOT', Value);
  end;
end;

class procedure PropertyConfigurator.DoConfigure(const AProps: TProperties);
var
  Value: String;
  appender: IAppender;
begin
  registry := TAppendersRegistry.Create;
  Value := AProps.GetProperty(DEBUG_KEY);
  if (CompareText(Value, 'true') = 0) then
    TlogLogUnit.Initialize(GetCurrentDir + '\log4delphi.log');
  Value := AProps.GetProperty(THRESHOLD_PREFIX);
  Logger.SetDefaultThreshold(TLevelUnit.toLevel(Value));
  Value := AProps.GetProperty(DEFAULT_APPENDER_PREFIX);
  appender := parseAppender(AProps, Value);
  if (appender <> nil) then begin
    Logger.SetDefaultAppender(appender);
  end;

  ConfigureRootLogger(AProps);
  ParseLoggers(AProps);
  TLogLog.debug('Finished configuring.');
end;

class procedure PropertyConfigurator.DoConfigure(const AFilename: String);
var
  props: TProperties;
  fin: TFileStream;
begin
  props := TProperties.Create;
  try
    fin := TFileSTream.Create(AFileName, fmOpenRead);
    props.Load(fin);
    fin.Free;
    DoConfigure(props);
  except
    on E: Exception do begin
      TLogLog.error('Could not read configuration file [' + AFileName + '] ' + e.Message);


      TLogLog.error('Ignoring configuration file [' + AFilename + ']');
    end;
  end;
  props.Free;
end;

class procedure PropertyConfigurator.FreeAppenders;
begin
  registry.Free;
end;

end.
 
