unit config_unit;

{$mode delphi}

interface

uses
  Classes, IniFiles, LazFileUtils, SysUtils,
  math_unit;

type
  TconfigUserDirId =                    // Identifiers for the user data directories
    (cudiBoxes,         // boxes
    cudiData,           // Data (parent) directory
    cudiDXFs,           // DXF files
    cudiEMFs,           // EMF files
    cudiImages,         // Images
    cudiPDFs,           // PDF files
    cudiPDFPreviews,    // PDF previews
    cudiPrintPreviews,  // Print previews
    cudiShapes,         // Shape files
    cudiSketches,       // Sketches
    cudiSketchImages);  // Sketch images

  TconfigSystemDirId =                    // Identifiers for the system directories
    (csdiBackup,        // backups
    csdiDpi,            // program size
    csdiFileView,       // file viewer
    csdiHelp,           // help
    csdiInternal,       // generic stuff
                        { TODO Get rid of this setting and rehouse its contents }
    csdiMap,            // screenshot maps
    csdiTile,           // tiled maps
    csdiUpdate,         // downloaded updates
    csdiLogs            // log files and log properties
    );


  Config = class

  public
    class procedure Init();

    class function GetDir(cudi: TconfigUserDirId): string; overload;
    class function GetDir(csdi: TconfigSystemDirId): string; overload;

    class function FilePath(cudi: TconfigUserDirId; Fname: string): string; overload;
    class function FilePath(csdi: TconfigSystemDirId; Fname: string): string; overload;

    class function IniFileName: string;

  private
    class var configDirName: string;
    class var configFileName: string;
    class var configFile: TIniFile;

    class var dataDirName: string;

    type
      TdirData = record
        cfgKey: string;
        cfgPath: string;
      end;

    class var
      userDirData: array [TconfigUserDirId] of TdirData;
      systemDirData: array [TconfigSystemDirId] of TdirData;

    class procedure LoadDir(cudi: TconfigUserDirId; cfgKey: string; parts: array of string); overload;
    class procedure LoadDir(csdi: TconfigSystemDirId; cfgKey: string; parts: array of string); overload;

    class function ReadDir(cfgKey: string): string ;             // Write to cfg file
    class procedure WriteDir(cfgKey: string; Value: string);     // Read the cfg file


  end; //Config


implementation

// === First a small utility function ...  ===

function MakePath(parts: array of string): string;
var
  i: integer;
  dir: string = '';
begin
  dir := parts[low(parts)];
  for i := Low(parts) + 1 to High(parts) do begin
    dir := dir + PathDelim + parts[i];
  end;
  Result := TrimFileName(dir);
end;

// === ... then the Config methods ... ===

class procedure Config.LoadDir(cudi: TconfigUserDirId; cfgKey: string; parts: array of string); overload;

// This function reads the entry in the cfg file and writes it to the array
// If no entry is found in the config file, one is created from the provided defaults
var
  path: string;
  dirdata: TdirData;

begin
  path := ReadDir(cfgKey);
  if path = '' then begin
    path := makePath(parts);
    WriteDir(cfgKey, path);
  end;

  // ... then ensure the path exists ...
  ForceDirectories(path);

  // ... then put it in the cache ...
  dirData.cfgKey := cfgKey;
  dirData.cfgPath := path;
  userDirData[cudi] := DirData;
end;

class procedure Config.LoadDir(csdi: TconfigSystemDirId; cfgKey: string; parts: array of string); overload;
var
  path: string;
  dirData: TdirData;
begin
  // Read from the .cfg file ...
  path := ReadDir(cfgKey);
  if path = '' then begin
    path := MakePath(parts);
    WriteDir(cfgKey, path);
  end;

  // ... then ensure the path exists ...
  ForceDirectories(path);

  // ... then put it in the cache ...
  dirData.cfgKey := cfgkey;
  dirData.cfgPath := path;
  systemDirData[csdi] := dirData;
end;

class procedure Config.Init();
begin
  configDirName := GetAppConfigDir(False);
  configFileName := GetAppConfigFile(False);
  configFile := TIniFile.Create(ConfigFileName);

  dataDirName := GetUserDir() + 'openTemplot';

  LoadDir(cudiBoxes, 'boxes', [dataDirName, 'box-files']);
  LoadDir(cudiData, 'data', [dataDirName, '']);
  LoadDir(cudiDXFs, 'dxfs', [dataDirName, 'dxf-files']);
  LoadDir(cudiEMFs, 'emfs', [dataDirName, 'emf-files']);
  LoadDir(cudiImages, 'images', [dataDirName, 'image-files']);
  LoadDir(cudiPDFs, 'pdfs', [dataDirName, 'pdf-files']);
  LoadDir(cudiPDFPreviews, 'pdf-previews', [dataDirName, 'pdf-preview-files']);
  LoadDir(cudiPrintPreviews, 'prt-previews', [dataDirName, 'print-preview-files']);
  LoadDir(cudiShapes, 'shapes', [dataDirName, 'shape_files']);
  LoadDir(cudiSketches, 'sketches', [dataDirName, 'sketchboard-files']);
  LoadDir(cudiSketchImages, 'sketch-images', [dataDirName, 'sketchboard-image-lib']);

  LoadDir(csdiBackup, 'bkp', [ConfigDirName, 'internal', 'bkp']);       // backups
  LoadDir(csdiDpi, 'dpi', [ConfigDirName, 'internal', 'dpi']);       // program size
  LoadDir(csdiFileView, 'fview', [ConfigDirName, 'internal', 'fview']); // file viewer
  LoadDir(csdiHelp, 'help', [ConfigDirName, 'internal', 'help']);    // help
  LoadDir(csdiMap, 'map', [ConfigDirName, 'internal', 'map']);       // screenshot maps
  LoadDir(csdiTile, 'tile', [ConfigDirName, 'internal', 'tile']);    // tiled maps
  LoadDir(csdiUpdate, 'upd', [ConfigDirName, 'internal', 'upd']);       // downloaded updates
  LoadDir(csdiLogs, 'logs', [ConfigDirName, 'internal', 'logs']);
end;

class function Config.GetDir(cudi: TconfigUserDirId): string;
var
  dirData: TdirData;
  idName: string;
begin
  dirData := userDirData[cudi];
  Result := dirData.cfgPath;
  if Result = '' then begin
    WriteStr(idname, cudi);
    show_modal_message('CONFIG ERROR : No directory name for cudi' + idName);
    halt(99);
  end;
end;

class function Config.GetDir(csdi: TconfigSystemDirId): string;
var
  dirData: TdirData;
  idName: string;
begin
  dirData := systemDirData[csdi];
  Result := dirData.cfgPath;
  if Result = '' then begin
    WriteStr(idname, csdi);
    show_modal_message('CONFIG ERROR : No directory name for csdi' + idName);
    halt(99);
  end;
end;

class function Config.FilePath(cudi: TconfigUserDirId; Fname: string): string; overload;
var
  grpdir: string;
begin
  grpdir := GetDir(cudi);
  Result := MakePath([grpdir, Fname]);
end;

class function Config.FilePath(csdi: TconfigSystemDirId; Fname: string): string; overload;
var
  grpdir: string;
begin
  grpdir := GetDir(csdi);
  Result := MakePath([grpdir, Fname]);
end;

class function Config.ReadDir(cfgKey: string): string;
begin
  Result := configFile.ReadString('directories', cfgKey, '');
end;

class procedure Config.WriteDir(cfgKey: string; Value: string);
begin
  configFile.WriteString('directories', cfgKey, Value);
end;

class function Config.IniFileName: string;
begin
  Result := configFileName;
end;

end.
