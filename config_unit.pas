
(*  v1
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  OpenTemplot project contributors

    This program is free software: you may redistribute it and/or modify
    it under the terms of the GNU General Public Licence as published by
    the Free Software Foundation, either version 3 of the Licence, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public Licence for more details.

    You should have received a copy of the GNU General Public Licence
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

====================================================================================
*)

{
  This unit provides cross-platform access to the names of the external files used by OT.

  @bold NOTE:
  Storage of user settings (inward-looking) are in the prefs_unit.
  This unit deals with external files - i.e. outward-looking, to the environment.
}
unit config_unit;

{$mode delphi}

interface

uses
  IniFiles, LazFileUtils, SysUtils, eventlog,
  math_unit;

type
  // User data Directory Identifier
  TconfigUserDirId =
    (cudiBoxes,         //< boxes
    cudiData,           //< Data (parent) directory
    cudiDXFs,           //< DXF files
    cudiEMFs,           //< EMF files
    cudiImages,         //< Images
    cudiPDFs,           //< PDF files
    cudiPDFPreviews,    //< PDF previews
    cudiPDFPagemaps,    //< PDF page maps
    cudiPrintPreviews,  //< Print previews
    cudiShapes,         //< Shape files
    cudiSketches,       //< Sketches
    cudiSketchImages);  //< Sketch images

  // System data Directory Identifier
  TconfigSystemDirId =
    (csdiBackup,        //< backups
    csdiDpi,            //< program size
    csdiFileView,       //< file viewer
    csdiHelp,           //< help
    csdiMap,            //< screenshot maps
    csdiTile,           //< tiled maps
    csdiUpdate,         // downloaded updates
    csdiLogs            // log files and log properties
    );

  TconfigSystemFileID =
    (
    csfiConfig,         //< The 'ini' file.
    csfi_85a_temp,      //<
    csfiAdobePrint,     //<
    csfiB6Startup,      //<
    csfiBackup1,        //<
    csfiBackup2,        //<
    csfiBgndMRU,        //<
    csfiBoxListH,       //<
    csfiBoxMRU,         //<
    csfiCGS,            //<
    csfiCompanion,      //<
    csfiCopy1,          //<
    csfiCopy2,          //<
    csfiE071Box,        //<
    csfiEmptyPic,       //<
    csfiFileViewBkp,    //<
    csfiFullScreen,     //<
    csfiGreenHelmet,    //<
    csfiJotterBkp,      //<
    csfiJotter,         //<
    csfiLinkView,       //<
    csfiMapShot,        //<
    csfiNLScopyright,   //<
    csfiNLStile,        //<
    csfiOSMcopyright,   //<
    csfiOSMtile,        //<
    csfiOTlogo,         //<
    csfiOTssCapture,    //<
    csfiOutputMenu,     //<
    csfiPadColours,     //<
    csfiPrefsPointer,   //<
    csfiRedoChanges,    //<
    csfiRedPointer,     //<
    csfiReminder,       //<
    csfiSavedPrefs,     //<
    csfiSaveForUndo,    //<
    csfiSaveForUndoZ,   //<
    csfiSbSample,       //<
    csfiSBshowItems,    //<
    csfiScaling,        //< scaling details
    csfiScangearDPI,    //<
    csfiScriptError,    //<
    csfiSmile,          //<
    csfiStartBgnd,      //<
    csfiStartBox,       //<
    csfiStoreBgnd,      //<
    csfiTickedBox,      //<
    csfiTreeSymbol,     //<
    csfiUndoChanges,    //<
    csfiUnTickedBox,    //<
    csfiViewerLoading,  //<
    csfiWaitSignal,     //<
    csfiWaitSignalTrans,//<
    csfiZoomTest,       //<
    csfiZoomTestH       //<

    );


  Config = class

  public
    // Checks for existing configuration and creates it if none exists.
    //
    // This function must be called only once per execution.
    class procedure Init();

    // Return the path to a user data directory
    class function GetDir(cudi: TconfigUserDirId): string; overload;

    // Return the path to a system data directory
    class function GetDir(csdi: TconfigSystemDirId): string; overload;

    // Return the path to a specific system file
    class function GetFilePath(csfi: TconfigSystemFileID): String;

    // Return a path created from a user data directory and a file name
    class function MakeFilePath(cudi: TconfigUserDirId; Fname: string): string; overload;

    // Return a path created from a system data directory and a file name
    class function MakeFilePath(csdi: TconfigSystemDirId; Fname: string): string; overload;

    // Write the location of each of the "well known" directories/files to the log
    class procedure WriteToLog;

  private
    class var configLocalDirName: string;
    class var configGlobalDirName: string;
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
    systemFileData: array [TconfigSystemFileId] of String;

    class procedure LoadDir(cudi: TconfigUserDirId; cfgKey: string;
      parts: array of string); overload;
    class procedure LoadDir(csdi: TconfigSystemDirId; cfgKey: string;
      parts: array of string); overload;
    class procedure LoadFile(csfi: TconfigSystemFileId; parts: array of string); overload;

    class function ReadDir(cfgKey: string): string;             // Write to cfg file
    class procedure WriteDir(cfgKey: string; Value: string);     // Read the cfg file


  end; //Config


{ @bold NOTE
  We avoid logging here since the log facility may not yet be initialised }
implementation

uses
  TLoggerUnit;

var
  log: ILogger;
  msg: String;


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

class procedure Config.LoadDir(cudi: TconfigUserDirId; cfgKey: string;
  parts: array of string); overload;

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

class procedure Config.LoadDir(csdi: TconfigSystemDirId; cfgKey: string;
  parts: array of string); overload;
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

class procedure Config.LoadFile(csfi: TconfigSystemFileId; parts: array of string);
var
  path: string;
begin
  // Read from the .cfg file ...
  path := systemFileData[csfi];
  if path = '' then begin
    path := MakePath(parts);
  end
  else begin
    msg := 'CONFIG ERROR : Trying to replace ' + path + ' with ' + MakePath(parts);
    show_modal_message(msg);
    halt(99);
  end;

  // ... then ensure the path exists ...
  ForceDirectories(ExtractFilePath(path));

  // ... then put it in the cache ...
  systemFileData[csfi] := path;
end;

class procedure Config.Init();
var
  csfi: TconfigSystemFileID;
  idName: string;
begin
  // --- get the "well-known" filenames
  configLocalDirName := GetAppConfigDir(False);
  // TODO: The following line should replace the one above once we have
  // an installer which can put the read-only attributes in a system directory.
  //configGlobalDirName := GetAppConfigDir(True);
  configGlobalDirName := configlocalDirName;
  configFileName := GetAppConfigFile(False);

  // --- create the config file
  configFile := TIniFile.Create(ConfigFileName);
  // We poke this value directly into the table since 'LoadFile' would write
  // it into the config file itself.
  systemFileData[csfiConfig] := configFileName;

  dataDirName := GetUserDir() + 'openTemplot';

  // --- Load user directory names into cache
  LoadDir(cudiBoxes, 'boxes', [dataDirName, 'box-files']);
  LoadDir(cudiData, 'data', [dataDirName, '']);
  LoadDir(cudiDXFs, 'dxfs', [dataDirName, 'dxf-files']);
  LoadDir(cudiEMFs, 'emfs', [dataDirName, 'emf-files']);
  LoadDir(cudiImages, 'images', [dataDirName, 'image-files']);
  LoadDir(cudiPDFs, 'pdfs', [dataDirName, 'pdf-files']);
  LoadDir(cudiPDFPagemaps, 'pdf-pagemaps', [dataDirName, 'pdf-pagemaps']);
  LoadDir(cudiPDFPreviews, 'pdf-previews', [dataDirName, 'pdf-previews']);
  LoadDir(cudiPrintPreviews, 'prt-previews', [dataDirName, 'print-previews']);
  LoadDir(cudiShapes, 'shapes', [dataDirName, 'shape_files']);
  LoadDir(cudiSketches, 'sketches', [dataDirName, 'sketchboard-files']);
  LoadDir(cudiSketchImages, 'sketch-images', [dataDirName, 'sketchboard-images']);

  // --- Load system directory names into cache
  LoadDir(csdiBackup, 'bkp', [configLocalDirName, 'bkp']);    // backups
  LoadDir(csdiDpi, 'dpi', [configLocalDirName, 'dpi']);       // program size
  LoadDir(csdiFileView, 'fview', [configLocalDirName, 'fview']); // file viewer
  LoadDir(csdiHelp, 'help', [configLocalDirName, 'help']);    // help
  LoadDir(csdiMap, 'map', [configLocalDirName, 'map']);       // screenshot maps
  LoadDir(csdiTile, 'tile', [configLocalDirName, 'tile']);    // tiled maps
  LoadDir(csdiUpdate, 'upd', [configLocalDirName, 'upd']);    // downloaded updates
  LoadDir(csdiLogs, 'logs', [configLocalDirName, 'logs']);

  // --- Load system (read-only) file names into cache
  LoadFile(csfiAdobePrint, [configGlobalDirName, 'help', 'adobe_print_dialog.png']);
  LoadFile(csfiB6Startup, [configGlobalDirName, 'help', 'b6_startup.gif']);
  LoadFile(csfiCompanion, [configGlobalDirName, 'help', 'companion_taskbar.png']);
  LoadFile(csfiEmptyPic, [configGlobalDirName, 'help', 'empty_picture.bmp']);
  LoadFile(csfiFullScreen, [configGlobalDirName, 'help', 'full_screen.png']);
  LoadFile(csfiGreenHelmet, [configGlobalDirName, 'help', 'green_helmet.gif']);
  LoadFile(csfiLinkView, [configGlobalDirName, 'help', 'link_view.png']);
  LoadFile(csfiNLScopyright, [configGlobalDirName, 'help', 'test_zoom.png']);
  LoadFile(csfiNLStile, [configGlobalDirName, 'graphics', 'nls_tile.jpg']);
  LoadFile(csfiOSMcopyright, [configGlobalDirName, 'graphics', 'osm_copyright.png']);
  LoadFile(csfiOSMtile, [configGlobalDirName, 'graphics', 'osm_tile.png']);
  LoadFile(csfiOTlogo, [configGlobalDirName, 'help', 'ot_logo.bmp']);
  LoadFile(csfiOtssCapture, [configGlobalDirName, 'executable', 'ot_screenshot_capture.exe']);
  LoadFile(csfiOutputMenu, [configGlobalDirName, 'help', 'output_menu.png']);
  LoadFile(csfiPadColours, [configGlobalDirName, 'help', 'pad_colours.gif']);
  LoadFile(csfiRedoChanges, [configGlobalDirName, 'help', 'redo_changes.png']);
  LoadFile(csfiRedPointer, [configGlobalDirName, 'help', 'red_pointer.gif']);
  LoadFile(csfiSavedPrefs, [configGlobalDirName, 'help', 'saved_prefs.png']);
  LoadFile(csfiSBsample, [configGlobalDirName, 'help', 'sketchboard_sample.gif']);
  LoadFile(csfiSBshowItems, [configGlobalDirName, 'help', 'sb_show_items.png']);
  LoadFile(csfiScangearDPI, [configGlobalDirName, 'help', 'scangear_dpi.png']);
  LoadFile(csfiScriptError, [configGlobalDirName, 'help', 'script_error.png']);
  LoadFile(csfiSmile, [configGlobalDirName, 'help', 'smile.gif']);
  LoadFile(csfiStoreBgnd, [configGlobalDirName, 'help', 'store_bgnd_options.png']);
  LoadFile(csfiTickedBox, [configGlobalDirName, 'help', 'tickbox_unticked.png']);
  LoadFile(csfiTreeSymbol, [configGlobalDirName, 'help', 'tree_symbol.gif']);
  LoadFile(csfiUndoChanges, [configGlobalDirName, 'help', 'undo_changes.png']);
  LoadFile(csfiUnTickedBox, [configGlobalDirName, 'help', 'tickbox_ticked.png']);
  LoadFile(csfiViewerLoading, [configGlobalDirName, 'help', 'viewer_loading.png']);
  LoadFile(csfiWaitSignal, [configGlobalDirName, 'help', 'wait_signal.png']);
  LoadFile(csfiWaitSignalTrans, [configGlobalDirName, 'help', 'wait_signal_trans.gif']);
  LoadFile(csfiZoomTest, [configGlobalDirName, 'help', 'zoom_test.bmp']);

  // --- Load system (writeable) file names into cache
  LoadFile(csfi_85a_temp, [configLocalDirName, 'state', '_85a_temp.bmp']);
  LoadFile(csfiBackup1, [configLocalDirName, 'backup', 'ebk1.ebk']);
  LoadFile(csfiBackup2, [configLocalDirName, 'backup', 'ebk2.ebk']);
  LoadFile(csfiBgndMRU, [configLocalDirName, 'state', 'bgndmru.txt']);
  LoadFile(csfiBoxListH, [configLocalDirName, 'html', 'box_list.html']);
  LoadFile(csfiBoxMRU, [configLocalDirName, 'state', 'boxmru.txt']);
  LoadFile(csfiCGS, [configLocalDirName, 'state', 'cgs.cgs']);
  LoadFile(csfiCopy1, [configLocalDirName, 'backup', 'pb.ebk']);
  LoadFile(csfiCopy2, [configLocalDirName, 'backup', 'pbo.ebk']);
  LoadFile(csfiE071Box, [configLocalDirName, 'backup', 'e071.bex']);
  LoadFile(csfiFileViewBkp, [configLocalDirName, 'backup', 'fv.ebk']);
  LoadFile(csfiJotterBkp, [configLocalDirName, 'state', 'jotter_backup.txt']);
  LoadFile(csfiJotter, [configLocalDirName, 'backup', 'jotter.txt']);
  LoadFile(csfiMapShot, [configLocalDirName, 'map', 'mapshot.txt']);
  LoadFile(csfiPrefsPointer, [configLocalDirName, 'state', 'goprefs.txt']);
  LoadFile(csfiReminder, [configLocalDirName, 'state', 'reminder.txt']);
  LoadFile(csfiSaveForUndo, [configLocalDirName, 'backup', 'sfu.ebk']);
  LoadFile(csfiSaveForUndoZ, [configLocalDirName, 'backup', 'sfz.ebk']);
  LoadFile(csfiStartBgnd, [configLocalDirName, 'default', 'start.bgs3']);
  LoadFile(csfiStartBox, [configLocalDirName, 'default', 'start.box3']);
  LoadFile(csfiScaling, [configLocalDirName, 'dpi', 'sz.szx']);       // Scaling data
  LoadFile(csfiZoomTestH, [dataDirName, 'test', 'zoom_test_page.html']);

  // === Validate the table ...
  for  csfi := low(TconfigSystemFileID) to high(TconfigSystemFileID) do begin
    if systemFileData[csfi] = '' then begin
      WriteStr(idname, csfi);
      msg := 'CONFIG ERROR : ' + idname + ' not initialised';
      show_modal_message(msg);
      halt(99);
    end;
  end;
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
    msg := 'CONFIG ERROR : No directory name for cudi ' + idName;
    show_modal_message(msg);
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
    msg := 'CONFIG ERROR : No directory name for csdi ' + idName;
    show_modal_message(msg);
    halt(99);
  end;
end;

class function Config.MakeFilePath(cudi: TconfigUserDirId; Fname: string): string; overload;
var
  grpdir: string;
begin
  grpdir := GetDir(cudi);
  Result := MakePath([grpdir, Fname]);
end;

class function Config.MakeFilePath(csdi: TconfigSystemDirId; Fname: string): string; overload;
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

class function Config.GetFilePath(csfi: TconfigSystemFileID): String;
begin
  Result := systemFileData[csfi];
end;

class procedure Config.WriteToLog();
begin
  log := Logger.GetInstance('Config');
  log.Info('             Config File : ' + configFileName);
  log.Info('  Local config directory : ' + configLocalDirName);
  log.Info(' Global config directory : ' + configGlobalDirName);
end;

end.
