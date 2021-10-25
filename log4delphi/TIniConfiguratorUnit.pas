{
   Copyright 2005-2010 Log4Delphi Project

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
   package from a ini file section.
   @version 0.5
   @author <a href="mailto:vsevolodp@gmail.com">Vsevolod Parfenov</a>
  ----------------------------------------------------------------------------}
unit TIniConfiguratorUnit;

interface

uses
  Classes, SysUtils, IniFiles,
  TPropertiesUnit, TPropertyConfiguratorUnit, TLogLogUnit;

type
  IniConfigurator = class
  public
    class procedure DoConfigure(const AFilename, ASectionName: String);
  end;

implementation

procedure LoadDefaultProperties(var props: TProperties);
begin
  props.SetProperty('debug', 'false');
  props.SetProperty('threshold', 'INFO');
  props.SetProperty('defaultAppender', 'defaultAppender');
  props.SetProperty('rootlogger', 'INFO, defaultAppender');
  props.SetProperty('appender.defaultAppender', 'TRollingFileAppender');
  props.SetProperty('appender.defaultAppender.File', 'openTemplot.log');
  props.SetProperty('appender.defaultAppender.MaxBackupIndex', '2');
  props.SetProperty('appender.defaultAppender.MaxFileSize', '1M');
  props.SetProperty('appender.defaultAppender.Append', 'true');
  props.SetProperty('appender.defaultAppender.Appdir', 'true');
  props.SetProperty('appender.defaultAppender.layout', 'TPatternLayout');
  props.SetProperty('appender.defaultAppender.layout.Pattern', '%d [%5p] <%L>%m');
end;

class procedure IniConfigurator.DoConfigure(const AFilename, ASectionName: String);
var
  props: TProperties;
  fin: TIniFile;
  sl: TStringList;
  i: Integer;
  Value: String;
begin
  props := TProperties.Create;
  fin := TIniFile.Create(AFilename);
  sl := TStringList.Create();
  try
    try
      fin.ReadSection(ASectionName, sl);

      LoadDefaultProperties(props);

      for i := 0 to sl.Count - 1 do begin
        Value := fin.ReadString(ASectionName, sl[i], '');
        props.SetProperty(sl[i], Value);
      end;

      PropertyConfigurator.DoConfigure(props);
    except
      on E: Exception do begin
        TLogLog.error('Could not read configuration file [' + AFileName +
          '] ' + e.Message);
        TLogLog.error('Ignoring configuration file [' + AFilename + ']');
      end;
    end;
  finally
    props.Free;
    fin.Free;
    sl.Free;
  end;

end;

end.
