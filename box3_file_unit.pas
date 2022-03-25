unit box3_file_unit;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  template,
  box_file_unit,
  math_unit;

type
  ExSaveBox = class(Exception)
  end;

  TGridInfo = record
    unitsCode: integer;
    spaceX: double;
    spaceY: double;
  end;

procedure SaveBox3(templatesToSave: TTemplateList;
  saveOption: ESaveOption;
  saveDone: boolean;
  const boxFilename: string;
  const projectTitle: string;
  const gridInfo: TGridInfo);

implementation

uses
  control_room,
  shoved_timber;

// new file format including text 17-2-00. (v:0.48 on).
// newer file format including unlimited shoves in StringList 1-5-01 (v:0.71.a on).

// The 071 file format is the same as the 048 format with the addition of "Data Blocks" at the end of the file.

// The Data Blocks section commences with a byte containing an underscore character '_',
// Then 7 more bytes containing '85A_|.x' , where x is the version build letter  (ASCII single-byte characters).
// Then a 16-byte starter record containing the version info and 12 bytes of zeroes (spares):

//        Tblock_start=record
//                       version_number:integer; // the Templot0 version number.
//                       zero1:integer;          // 12 spares (zero)...
//                       zero2:integer;
//                       zero3:integer;
//                     end;

// Each DATA BLOCK comprises:

// 16 byte Tblock_ident comprising...

// 4 bytes = length of data segment x.
// 4 bytes = template index count.
// 4 bytes = code indicating content of block.
// 4 bytes = spare - set to zero.

// then x bytes = data segment.

// DATA BLOCKS are repeated until the END BLOCK, which comprises

// 16 byte Tblock_ident comprising all zeroes (segment length=0).

type
  // start record for trailing data blocks...
  TBlockStart = record
    versionNumber: integer; // the Templot0 version number.
    zero1: integer;          // 12 spares (zero)...
    zero2: integer;
    zero3: integer;
  end;

  TBlockIdent = record
    segmentLength: integer;
    templateIndex: integer;
    blockCode: integer;   // 10 = timber shove data.
    spareZeroes: integer;
  end;

procedure FileWriteError;
begin
  raise ExSaveBox.Create('File write error');
end;

procedure WriteTemplateRecords(
  var boxFile: file;
  templatesToSave: TTemplateList;
  saveOption: ESaveOption;
  saveDone: boolean;
  const projectTitle: string;
  const gridInfo: TGridInfo);
var
  i: integer;
  numberWritten: integer;
  nextTi: Ttemplate_info;
begin
  nextTi.keep_shove_list := Tshoved_timber_list.Create;
  try
    for i := 0 to templatesToSave.Count - 1 do begin     // first write the template data.

      templatesToSave[i].template_info.keep_dims.box_dims1.file_format_code := 1;
      // OT format      // put format in file

      copy_template_info_from_to(False, templatesToSave[i].template_info, nextTi);
      // next template.

      if i = (templatesToSave.Count - 1) then
        nextTi.keep_dims.box_dims1.box_ident := 'NX' + IntToStr(i)
      // last one in file. (string[10])
      else
        nextTi.keep_dims.box_dims1.box_ident := 'N ' + IntToStr(i);

      nextTi.keep_dims.box_dims1.id_byte := 255;
      // identify file as BOX3 rather than BOX      290a


      case saveOption of
        eSO_BackupOnExit: begin    // final backup on exit ..
          nextTi.keep_dims.box_dims1.auto_restore_on_startup := False;
          // these three only read from the first keep in the file,
          nextTi.keep_dims.box_dims1.ask_restore_on_startup := True;
          // but go in every one.
          nextTi.keep_dims.box_dims1.box_save_done := saveDone;
        end;

        eSO_Normal: begin    // normal box save (these are never read) ..
          nextTi.keep_dims.box_dims1.auto_restore_on_startup := False;
          // not used for normal file save/reload
          nextTi.keep_dims.box_dims1.ask_restore_on_startup := False;
          // not used for normal file save/reload
          nextTi.keep_dims.box_dims1.box_save_done := False;
        end;

        eSO_RollingBackup: begin    // rolling backup..
          nextTi.keep_dims.box_dims1.auto_restore_on_startup := True;
          // if both True on loading = abnormal termination.
          nextTi.keep_dims.box_dims1.ask_restore_on_startup := True;
          nextTi.keep_dims.box_dims1.box_save_done := False;
        end;

      end;//case

      //  these go in every template but only the first or last in is read back...

      nextTi.keep_dims.box_dims1.project_for := Copy(projectTitle, 1, 49);
      // goes in every template but only the last in is read back.

      // 0.79.a  20-05-06  save grid info -- to be read from final template...

      //%%%% 0.91.d -- now also in user preferences, these used only if not prefs.

      nextTi.keep_dims.box_dims1.grid_units_code := gridInfo.unitsCode;
      nextTi.keep_dims.box_dims1.x_grid_spacing := gridInfo.spaceX;
      nextTi.keep_dims.box_dims1.y_grid_spacing := gridInfo.spaceY;

      //--------------------

      BlockWrite(boxFile, nextTi.keep_dims, SizeOf(Tkeep_dims), numberWritten);
      // write all the data.

      if numberWritten <> SizeOf(Tkeep_dims) then begin
        FileWriteError;
      end;
    end;//next i
  finally
    nextTi.keep_shove_list.Free;
  end;
end;

procedure WriteStrings(var boxFile: file; templatesToSave: TTemplateList);
var
  i: integer;
  numberWritten: integer;
  len: integer;
  s: string;
begin
  for i := 0 to templatesToSave.Count - 1 do begin        // now add the texts.

    // 0.94.a  fb_kludge templates are created on printing, and destroyed afterwards. Don't save any remaining..

    if templatesToSave[i].template_info.keep_dims.box_dims1.fb_kludge_template_code <> 0 then
      CONTINUE;

    s := remove_esc_str(templatesToSave[i].Name) + Char($1B) + remove_esc_str(
      templatesToSave[i].Memo) + Char($1B) + Char($1B);
    // use ESC chars as terminators, plus one for luck on the end.

    UniqueString(s);  // make sure it's in continuous memory.

    len := Length(s) * SizeOf(Char);

    BlockWrite(boxFile, len, SizeOf(integer), numberWritten);
    // first the length as an integer (4 bytes)
    if numberWritten <> SizeOf(integer) then begin
      FileWriteError;
    end;

    BlockWrite(boxFile, s[1], len, numberWritten);   // then the text.
    if numberWritten <> len then begin
      FileWriteError;
    end;

  end;//next i
end;

procedure WriteDataBlocks(var boxFile: file; templatesToSave: TTemplateList);
var
  s: string;
  i: integer;
  numberWritten: integer;
  nextTi: Ttemplate_info;

  blockStart: TBlockStart;
  blockIdent: TBlockIdent;

  shoveCount: integer;
  st: integer;
  shove_timber_data: Tshove_for_file;

begin
  // now add the DATA BLOCKS section...

  s := '_85A_|    ';  // start marker.

  UniqueString(s);  // make sure it's in continuous memory.

  BlockWrite(boxFile, s[1], 8, numberWritten);
  // 8 bytes of '_85A_|  ' as a DATA BLOCKS start marker.
  if numberWritten <> 8 then begin
    FileWriteError;
  end;

  with blockStart do begin
    versionNumber := file_version;
    zero1 := 0;
    zero2 := 0;
    zero3 := 0;
  end;//with

  BlockWrite(boxFile, blockStart, SizeOf(TBlockStart), numberWritten);
  // 16 bytes = version number + 12 bytes of zero (spares).
  if numberWritten <> SizeOf(TBlockStart) then begin
    FileWriteError;
  end;

  // now the data blocks for each of the loaded templates...
  nextTi.keep_shove_list := Tshoved_timber_list.Create;
  try
    for i := 0 to templatesToSave.Count - 1 do begin

      copy_template_info_from_to(False, templatesToSave[i].template_info, nextTi);
      // next template.

      // first block is the shove timber data...

      // shove data = code 10. 4 bytes containing the count of shoved timbers,
      //                       + a series of Tshove_for_file data records for each one.

      shoveCount := nextTi.keep_shove_list.Count;

      blockIdent.segmentLength := SizeOf(integer) + shoveCount * SizeOf(Tshove_for_file);
      blockIdent.templateIndex := i;
      blockIdent.blockCode := 10;         // = timber shove data.
      blockIdent.spareZeroes := 0;

      BlockWrite(boxFile, blockIdent, SizeOf(TBlockIdent), numberWritten);
      // the data block ident.
      if numberWritten <> SizeOf(TBlockIdent) then begin
        FileWriteError;
      end;

      // now the shove data segment itself..

      BlockWrite(boxFile, shoveCount, SizeOf(integer), numberWritten);
      // first the count of shoved timbers.
      if numberWritten <> SizeOf(integer) then begin
        FileWriteError;
      end;

      if shoveCount > 0 then begin

        for st := 0 to shoveCount - 1 do begin
          shove_timber_data.copy_from(nextTi.keep_shove_list[st]);

          BlockWrite(boxFile, shove_timber_data, SizeOf(Tshove_for_file),
            numberWritten);      // first the count of shoved timbers.
          if numberWritten <> SizeOf(Tshove_for_file) then begin
            FileWriteError;
          end;
        end;//next st
      end;

      // no more DATA BLOCKS yet defined for this template, so on to the next..

    end;//next i
  finally
    nextTi.keep_shove_list.Free;
  end;

  // all templates done, so add the end zeroes ident (zero-length data segment).

  with blockIdent do begin
    segmentLength := 0;
    templateIndex := 0;
    blockCode := 0;
    spareZeroes := 0;
  end;//with

  BlockWrite(boxFile, blockIdent, SizeOf(TBlockIdent), numberWritten);
  // finally the end zeroes.
  if numberWritten <> SizeOf(TBlockIdent) then begin
    FileWriteError;
  end;
end;

procedure SaveBox3(templatesToSave: TTemplateList;
  saveOption: ESaveOption;
  saveDone: boolean;
  const boxFilename: string;
  const projectTitle: string;
  const gridInfo: TGridInfo);
var
  fileSize: integer;
  boxFile: file;               // untyped file.
begin
  if (boxFilename = '') then begin
    raise Exception.Create('No filename provided');
  end;

  fileSize := 0;         // default init..

  try
    try
      AssignFile(boxFile, boxFilename);
      Rewrite(boxFile, 1);               // open for writing, record size = 1 byte.

      WriteTemplateRecords(boxFile, templatesToSave, saveOption, saveDone, projectTitle, gridInfo);
      WriteStrings(boxFile, templatesToSave);
      WriteDataBlocks(boxFile, templatesToSave);

    except
      on EInOutError do begin
        FileWriteError;
      end;
    end;//try-except

    fileSize := System.FileSize(boxFile);      // (file must be open to get the size).

    if (FileExists(boxFilename) = False) or (fileSize = 0)                  // ???
    then begin
      FileWriteError;
    end;

  finally
    try
      CloseFile(boxFile);
    except
      on EInOutError do
    end;  // close file if it's open.
  end;//try
end;

end.
