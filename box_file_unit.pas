unit box_file_unit;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  template;

// 290a - these type declarations moved into the interface for mecbox_unit

type

  // Tkeep_dims has the shove timber data omitted.  v:0.71.a  29-4-01.

  Tkeep_dims1 = record      // first part of Tkeep_dims

    box_dims1: Tbox_dims1;

  end;//record Tkeep_dims1

  Tkeep_dims2 = record

    turnout_info2: Tturnout_info2;

  end;//record Tkeep_dims2

  Told_keep_data = record
    // this matches the old Tkeep_data record pre 071 including the timber shove data.
    // used on loading files.

    old_keep_dims1: Tkeep_dims1;
    old_keep_dims2: Tkeep_dims2;

  end;//record.

  ESaveBox = (
    eSB_SaveOne,           // -1 !
    eSB_SaveAll,           // 0
    eSB_SaveBackground,    // 1
    eSB_SaveUnused,        // 2
    eSB_SaveGroup,         // 3
    eSB_SaveLibrary);      // 4

  ESaveOption = (
    eSO_Normal,            // 0
    eSO_RollingBackup,     // 1
    eSO_BackupOnExit);     // -1


function save_box(this_one: integer; which_ones: ESaveBox; save_option: ESaveOption;
  save_str: string): boolean;
function load_storage_box(normal_load, old_templot_folder: boolean; file_str: string;
  load_backup, make_lib: boolean; var append: boolean;
  var last_bgnd_loaded_index: integer): boolean;


implementation

uses
  Controls,
  Forms,
  alert_unit,
  config_unit,
  control_room,
  help_sheet,
  info_unit,
  keep_select,
  math_unit,
  pad_unit,
  shoved_timber,
  wait_message,
  box3_file_unit;

type
  Tnew_keep_data = record           // this matches Tkeep_dims used in prog - see below.

    new_keep_dims1: Tkeep_dims1;
    new_keep_dims2: Tkeep_dims2;

  end;//record

  // start record for trailing data blocks...

  Tblock_start = record
    version_number: integer; // the Templot0 version number.
    zero1: integer;          // 12 spares (zero)...
    zero2: integer;
    zero3: integer;
  end;

  Tblock_ident = record
    segment_length: integer;
    f_index: integer;
    block_code: integer;   // 10 = timber shove data.
    spare_zeroes: integer;
  end;


// clear a single keep without asking - this is for errors on loading.
procedure clear_keep(n: integer); forward;

//________________________________________________________________________________________

procedure file_error(str: string);     // generic error message.

begin
  alert(5, '     file  error',
    str + '||Sorry, the requested operation on this file has failed. Please check the file and folder names.' + '||If this is a save operation, check the available disk space,' + ' and if saving to a floppy disk, check that it is not write-protected.' + '||If this is a reload operation, check that the named file exists in the named folder.',
    '', '', '', '', '', 'O K', 0);
end;


function save_box(this_one: integer; which_ones: ESaveBox; save_option: ESaveOption;
  save_str: string): boolean;

var
  box_str, backup_del_str: string;

  group_count: integer;
  i, len: integer;
  s: string;
  saved_cursor: TCursor;

  save_bw: boolean; // 0.93.a

  templatesToSave: TTemplateList;
  gridInfo: TGridInfo;

  /////////////////////////////////////////////////////////////

  procedure delete_any_control_templates;  // 0.93.a

  // goes only in file, so delete after saving.

  var
    i, n: integer;
    save_backw: boolean;

  begin

    save_backw := backup_wanted;
    // don't let this action change backup flag (list.OnChange)

    n := 0;
    while n < keeps_list.Count do begin

      if keeps_list[n].template_info.keep_dims.box_dims1.this_was_control_template =
        False  // normal template
      then begin
        Inc(n);
        CONTINUE;
        // leave this one.
      end;

      // delete it...
      keeps_list.Delete(n);
    end;
    //while    // no need to increment n, it is now pointing to the next keep.

    backup_wanted := save_backw;   // restore backup flag
  end;
  /////////////////////////////////////////////////////////////

begin
  Result := False;      // init default.

  delete_any_control_templates;  // 0.93.a we may want to add a new one..

  // 0.93.a automatically add the control template to a file...

  // put it in the box to save file, then delete it after saving...

  if (which_ones = eSB_SaveAll) and (turnoutx > 0) and
    ({check_if_abandoned=-1}abandon_calcs = False)
  // check not zero-length
  then begin
    save_bw := backup_wanted;     // don't let this action change backup flag (list.OnChange)

    store_unused(False, True);   // 0.93.a store the control template unused.

    backup_wanted := save_bw;     // restore backup flag
  end;

  try

    if keeps_list.Count < 1     // no control template added?
    then begin
      backup_wanted := False;
      save_done := True;
      Result := True;
      EXIT;
    end;

    case which_ones of
      eSB_SaveOne:
        if (this_one >= 0) and (this_one < keeps_list.Count)   // index of only one to be saved.
        then
          group_count := 1
        else begin
          group_count := 0;             // keep compiler happy.
          EXIT;
        end;

      eSB_SaveAll:
        group_count := keeps_list.Count;      // save all.

      eSB_SaveBackground: begin
        group_count := any_bgnd;            // save bgnd templates only.
        if group_count = 0 then begin
          alert_no_bgnd;
          EXIT;
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      eSB_SaveUnused: begin
        group_count := any_unused;        // save unused templates only.
        if group_count = 0 then begin
          alert_no_unused;
          EXIT;
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      eSB_SaveGroup: begin
        group_count := any_selected;        // save group members only.
        if group_count = 0 then begin
          if alert_no_group = True    // alert him, and does he want all?
          then
            EXIT
          else
            group_count := any_selected;   // now all of them.
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      eSB_SaveLibrary: begin
        group_count := any_library;        // save library templates only.
        if group_count = 0 then begin
          alert_no_library;
          EXIT;
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      else begin               // ???
        group_count := 0;   // keep compiler happy.
        run_error(39);
        EXIT;
      end;
    end;//case

    if save_option <> eSO_Normal then begin
      // set up to create alternate backup files..

      if FileExists(ebk1_str) = False then begin
        box_str := ebk1_str;         // use first file for backup.
        backup_del_str := ebk2_str;  // and delete the second one afterwards.
      end
      else begin
        box_str := ebk2_str;         // use second file for backup.
        backup_del_str := ebk1_str;  // and delete the first one afterwards.
      end;
    end
    else begin     // normal save...

      if save_str <> '' then
        box_str := save_str
      else begin
        with keep_form.save_dialog do begin         // set up the save dialog.

          if his_save_file_name <> '' then
            InitialDir := ExtractFilePath(his_save_file_name)   // use his previous folder.
          else
            InitialDir := Config.GetDir(cudiBoxes);              // or the default one.

          Filter := ' storage  box  contents  (*.box3)|*.box3';

          case which_ones of
            eSB_SaveOne: begin                             // echo one only
              box_str := Config.GetFilePath(csfiE071Box);
              // echo goes in the folder we started in.
            end;

            // 0.79.a  yy_mm_dd  was yy-mm-dd

            eSB_SaveAll: begin
              Filename :=
                remove_invalid_str(Copy(Trim(box_project_title_str), 1, 20) +
                FormatDateTime(' yyyy_mm_dd_hhmm_ss', Date + Time)) + '.box3';
              // 0.79.a  20 chars was 15
              Title := '    save  all  templates  as ...';
            end;

            eSB_SaveBackground: begin
              Filename :=
                remove_invalid_str('background' +
                FormatDateTime(' yyyy_mm_dd_hhmm_ss', Date + Time)) + '.box3';
              Title := '    save  background  templates  as ...';
            end;

            eSB_SaveUnused: begin
              Filename :=
                remove_invalid_str('unused' + FormatDateTime(' yyyy_mm_dd_hhmm_ss',
                Date + Time)) + '.box3';
              Title := '    save  unused  templates  as ...';
            end;

            eSB_SaveGroup: begin
              Filename :=
                remove_invalid_str('group' + FormatDateTime(' yyyy_mm_dd_hhmm_ss',
                Date + Time)) + '.box3';
              Title := '    save  selected  group  of  templates  as ...';
            end;

            eSB_SaveLibrary: begin
              Filename :=
                remove_invalid_str('library' + FormatDateTime(' yyyy_mm_dd_hhmm_ss',
                Date + Time)) + '.box3';
              Title := '    save  library  templates  as ...';
            end;

            else
              run_error(39);    // ???
          end;//case

          Filename := lower_case_filename(Filename);
          // 0.79.a   to underscores and lower case.

          if which_ones <> eSB_SaveOne then begin
            if Execute = False then
              EXIT;        // get his file name.
            box_str := FileName;

            if invalid_85a_file_name(box_str) = True then
              EXIT;

            box_str := ChangeFileExt(box_str, '.box3');   // force extension

            his_save_file_name := box_str;
            // so can use same folder next time.
          end;

        end;//with
      end;
    end;

    saved_cursor := Screen.Cursor;

    try
      if save_option = eSO_Normal then
        Screen.Cursor := crHourGlass;   // 0.93.a test added     // could take a while if big file.
      if Application.Terminated = False then
        Application.ProcessMessages;
      // so let the form repaint (if not called from quit_alert).

      gridInfo.unitsCode := grid_labels_code_i;
      gridInfo.spaceX := grid_spacex;
      gridInfo.spaceY := grid_spacey;

      // create a non-owning list of templates that we want to save
      templatesToSave := TTemplateList.Create(False);
      try
        for i := 0 to keeps_list.Count - 1 do begin
          // 0.94.a  fb_kludge templates are created on output/printing, and destroyed afterwards. Don't save any remaining..
          if keeps_list[i].template_info.keep_dims.box_dims1.fb_kludge_template_code <> 0 then
            CONTINUE;  // 0.94.a don't save kludge templates, if any found (error in print?)

          case which_ones of
            eSB_SaveOne:
              if i <> this_one then
                CONTINUE;
            eSB_SaveBackground:
              if keeps_list[i].template_info.keep_dims.box_dims1.bgnd_code_077 <> 1 then
                CONTINUE;  // bgnd only, ignore unused and library.
            eSB_SaveUnused:
              if keeps_list[i].template_info.keep_dims.box_dims1.bgnd_code_077 <> 0 then
                CONTINUE;  // unused only, ignore others.
            eSB_SaveGroup:
              if not keeps_list[i].group_selected then
                CONTINUE;  // group only, ignore unselected.
            eSB_SaveLibrary:
              if keeps_list[i].template_info.keep_dims.box_dims1.bgnd_code_077 <> -1 then
                CONTINUE;  // library only, ignore others.
          end;//case

          templatesToSave.Add(keeps_list[i]);
        end;

        try
          SaveBox3(templatesToSave, save_option, save_done, box_str,
            box_project_title_str, gridInfo);
        except
          on ExSaveBox do begin
            if save_option = eSO_Normal then
              file_error(box_str);
            Result := False;
          end;
        end;
      finally
        templatesToSave.Free;
      end;

      if (which_ones = eSB_SaveAll) and (save_option = eSO_Normal) and (save_str = '')
      // normal save of all templates..
      then begin
        keep_form.box_file_label.Caption := ' last saved to :  ' + box_str;
        keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
        // in case too long for caption

        // 0.82.a  control_room_form.statusbar_label.Caption:=' templates'+ExtractFileName(keep_form.file_label.Caption);

        saved_box_str := box_str;  // for print of box contents list.
        reloaded_box_str := '';    // ditto.

        save_done := True;  // this boxful has been saved.
      end;

      if (which_ones <> eSB_SaveOne) and (save_option = eSO_Normal) and (save_str = '')
      // normal save of any templates..
      then
        boxmru_update(box_str);                                 // 0.82.a  update the mru list.

      if save_option <> eSO_Normal then
        DeleteFile(backup_del_str);   // delete the previous backup file.

      Result := True;

    finally
      Screen.Cursor := saved_cursor;
    end;//try

  finally

    // 0.93.a file saved or not, now remove any control templates...

    delete_any_control_templates;

  end;//try
end;
//______________________________________________________________________________________

function load_storage_box(normal_load, old_templot_folder: boolean; file_str: string;
  load_backup, make_lib: boolean; var append: boolean;
  var last_bgnd_loaded_index: integer): boolean;
  // load a file of templates into the keeps box.

  // normal_load True = for use. False = for file viewer.
  // if file_str not empty it is the file name to load.
  // return True any templates loaded/added.
  // also return any change to append.
  // also return last_bgnd_loaded_index, highest bgnd template loaded (for minting).

const
  ask_restore_str: string = '      `0Restore On Startup`9' +
    '||Your work in progress can be restored from your previous working session with Templot0.'
    +
    '||This means restoring your storage box contents `0(if any)`7, background track plan drawing `0(if any)`7, and control template.'
    +
    '||This is done independently of any saving to data files which you may have performed.' +
    '||If you answer "no thanks" the previous data can be restored later by selecting the `0FILES > RESTORE PREVIOUS`1 menu item on the storage box menus.' + '||tree.gif The restore feature works correctly even if your previous session terminated abnormally as a result of a power failure or system malfunction, so there is no need to perform repeated saves as a precaution against these events.' + '||rp.gif The restore feature does not include your Background Shapes or Sketchboard files, which must be saved and reloaded separately as required.' + '||rp.gif If you run two instances of Templot0 concurrently (not recommended for Windows 95/98/ME) from the same `0\TEMPLOT\`2 folder,' + ' the restore data will be held in common between the two. To prevent this happening, create and run the second instance from a different folder (directory).';

var
  test_box_file: file;                 // untyped file for testing format.
  i, n, fsize{,timb_index}: integer;
  loaded_str, box_str, ixt_str, ident: string;
  no_ixt: boolean;
  old_count: integer;                // for append.
  resave_needed: boolean;
  s, info_string, memo_string, _str: string;
  saved_cursor: TCursor;
  restored_save_done: boolean;

  old_next_data: Told_keep_data;
  new_next_data: Tnew_keep_data;

  this_ti: Ttemplate_info;
  _071_format: boolean;
  number_read: integer;

  inbyte: byte;

  saved_control: Ttemplate_info;
  saved_notch: Tnotch;

  saved_control_name_str: string;
  saved_control_memo_str: string;

  ///////////////////////////////////////////////////////////////

  function load_new_format: boolean;

  var
    box_file: file;                // new format untyped file.
    n, i, len: integer;
    n_valid: boolean;
    block_start: Tblock_start;
    block_ident: Tblock_ident;

    /////////////////////////////////

    procedure read_file_error;

    begin
      try
        CloseFile(box_file);
      except
        on EInOutError do
      end;  // close file if it's open.

      if append = False then
        clear_keeps(False, False)     // error reloading, clear all.
      else
        clear_keep(n);               // error adding, we already created the list entry for it.

      if load_backup = False then
        file_error(box_str);
    end;
    /////////////////////////////////

    function load_shove_block(t_index, seg_len: integer): boolean;
      // for a single template.

    var
      shove_timber_data: Tshove_for_file;

      shove_count, st: integer;
      total_read: integer;

    begin
      Result := False;    // default init.
      total_read := 0;    // number of bytes read.

      // first get the count of shoved timbers for this template...

      if EOF(box_file) = True then
        EXIT;
      BlockRead(box_file, shove_count,
        SizeOf(integer), number_read);
      total_read := total_read + number_read;
      if (number_read <> SizeOf(integer)) or (total_read > seg_len) then begin
        try
          CloseFile(box_file);
        except
          on EInOutError do
        end;  // close file if it's open.
        if load_backup = False then
          file_error(box_str);
        EXIT;
      end;

      if seg_len <> (SizeOf(integer) + shove_count * SizeOf(Tshove_for_file)) then
        EXIT;  // the integer is the shove count just read.

      if (t_index < 0) or (t_index > (keeps_list.Count - 1)) then
        EXIT;

      if shove_count > 0
      // now get the data for each shoved timber...
      then begin
        st := 0;    // keep compiler happy.

        with keeps_list[t_index].template_info.keep_shove_list do begin

          if Count <> 0 then
            EXIT;   // !!! shove list should be empty (created in init_ttemplate).

          repeat

            if EOF(box_file) = True then
              EXIT;
            BlockRead(box_file,
              shove_timber_data, SizeOf(Tshove_for_file), number_read);
            total_read := total_read + number_read;
            if (number_read <> SizeOf(Tshove_for_file)) or
              (total_read > seg_len) then begin
              try
                CloseFile(box_file);
              except
                on EInOutError do
              end;  // close file if it's open.
              if load_backup = False then
                file_error(box_str);
              EXIT;
            end;

            try
              st :=
                Add(Tshoved_timber.CreateFrom(shove_timber_data));
            except
              EXIT;       // memory problem?
            end;//try
          until (st = shove_count - 1) or (total_read = seg_len);
        end;//with
      end;
      Result := True;
    end;
    ///////////////////////////////////

  begin
    Result := False;    // init default
    try
      n_valid := False;   // default for error exits.
      n := old_count;     // keep compiler happy.
      try
        AssignFile(box_file, box_str);
        Reset(box_file, 1);              // open for reading, record size = 1 byte.

        repeat
          try
            n_valid := False;   // default for error exits.

            n := keeps_list.Add(TTemplate.Create('no information available'));
          except
            alert(1, '      memory  problem',
              '|||Unable to load templates from the file into your storage box because of memory problems.'
              + '||(Loading terminated after  ' + IntToStr(n - old_count) +
              '  templates.)',
              '', '', '', '', '', 'cancel  reload', 0);
            try
              CloseFile(box_file);
            except
              on EInOutError do
            end;  // close file if it's open.
            if append = False then
              clear_keeps(False, False);
            EXIT;
          end;//try

          init_ttemplate(n);

          n_valid := True;     // got a valid new index to the lists.

          BlockRead(box_file, old_next_data, SizeOf(Told_keep_data), number_read);
          // read bytes.


          s := old_next_data.old_keep_dims1.box_dims1.box_ident;

          if (number_read <> SizeOf(Told_keep_data)) or
            ((s <> ('N ' + IntToStr(n - old_count))) and (s <> ('NX' + IntToStr(n - old_count))))
          // error reading, or this is not a template.
          then begin
            read_file_error;
            EXIT;
          end;

          if version_mismatch(old_next_data) = True then
            resave_needed := True;  // check for version mismatch.

          // !!! version_mismatch must be done first - sets bgnd_code_077...

          if make_lib = True then
            old_next_data.old_keep_dims1.box_dims1.bgnd_code_077 := -1;
          // make it a library template.

          this_ti.keep_shove_list := Tshoved_timber_list.Create;

          this_ti.keep_dims := Tkeep_dims(old_next_data);

          copy_template_info_from_to(True, this_ti, keeps_list[n].template_info);
          // True = free the shove list.

          if (append = True) and (make_lib = False) and
            (keep_form.add_ignore_group_menu_entry.Checked = False) then
            keeps_list[n].group_selected := True;
          // group select added template.

        until Copy(s, 1, 2) = 'NX';      // last template marker.

        Result := True;   // return good result, even if we don't get the texts.

        if EOF(box_file) = True then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          EXIT;
          // there was no text in the file
        end;

      except
        on EInOutError do begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);

          if append = False then
            clear_keeps(False, False)                   // error reloading, clear all.
          else
          if n_valid = True then
            clear_keep(n);  // error adding, we already created the list entry for it.
          EXIT;
        end;
      end;//try-except

      // got the data, now load the text strings (don't clear the data if it fails) ...

      for n := old_count to keeps_list.Count - 1 do begin
        // now get the proper texts.

        BlockRead(box_file, len, SizeOf(integer), number_read);
        // first get the length as an integer (4 bytes)

        if number_read <> SizeOf(integer) then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        // read len bytes into string s...

        s := '';
        while Length(s) < (len div SizeOf(Char)) do
          s := s + '0';

        s := s + '00';        // two more for safety.

        UniqueString(s);  // make sure it's in continuous memory

        if Length(s) > (len div SizeOf(Char)) then
          BlockRead(box_file, s[1], len, number_read);

        if number_read <> len then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        i := Pos(Char($1B), s);          // find info part terminator.

        if i <> 0 then begin
          info_string := Copy(s, 1, i - 1);
          // info string (don't include the ESC).
          Delete(s, 1, i);
          // remove info string and terminator from input.

          i := Pos(Char($1B), s);             // find memo part terminator.

          if i <> 0 then begin
            memo_string := Copy(s, 1, i - 1);
            // memo string (don't incude the ESC).

            // we don't change either unless we've got both..

            keeps_list[n].Name := remove_esc_str(info_string);
            // remove any ESC is belt and braces...
            keeps_list[n].Memo := remove_esc_str(memo_string);
          end;
        end;
      end;//next n
      // now can load any trailing data blocks...

      if (_071_format = True) and (EOF(box_file) = False) then begin

        // The 071 file format is the same as the 048 format with the addition of "Data Blocks" at the end of the file.
        // To create the 048 format the shove timber data is duplicated in the template data record (first 30 shoved timbers only).

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

        // DATA BLOCKS are then repeated until the END BLOCK, which comprises

        // 16 byte Tblock_ident comprising all zeroes (segment length=0).

        // first find the starting underscore character for the trailing data blocks...

        inbyte := 0;
        repeat
          if EOF(box_file) = True then
            EXIT;
          BlockRead(box_file, inbyte, 1, number_read);  // read 1 byte
          if number_read <> 1 then begin
            try
              CloseFile(box_file);
            except
              on EInOutError do
            end;  // close file if it's open.
            if load_backup = False then
              file_error(box_str);
            EXIT;
          end;
        until Chr(inbyte) = '_';

        // then the magic number...

        s := '12345678';    // 8 bytes  (1 extra for safety).
        UniqueString(s);  // make sure it's in continuous memory

        if EOF(box_file) = True then
          EXIT;
        BlockRead(box_file, s[1], 7, number_read);
        if number_read <> 7 then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        if Copy(s, 1, 5) <> '85A_|' then
          EXIT;  // magic number - ignore the final '.x' build letter.

        if EOF(box_file) = True then
          EXIT;
        BlockRead(box_file, block_start, SizeOf(Tblock_start), number_read);
        if number_read <> SizeOf(Tblock_start) then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        if block_start.version_number <> loaded_version then
          EXIT; // double check we are ok to continue.

        // ...ignore any other data in the block start record in this version (071).

        repeat     // get all the data blocks

          // get the ident for the next data block..

          if EOF(box_file) = True then
            EXIT;
          BlockRead(box_file, block_ident, SizeOf(Tblock_ident), number_read);
          if number_read <> SizeOf(Tblock_ident) then begin
            try
              CloseFile(box_file);
            except
              on EInOutError do
            end;  // close file if it's open.
            if load_backup = False then
              file_error(box_str);
            EXIT;
          end;

          if block_ident.segment_length = 0 then
            EXIT;    // end of data blocks.

          n := old_count + block_ident.f_index;   // loaded template list index.

          case block_ident.block_code of

            10:
              if load_shove_block(n, block_ident.segment_length) = False then
                EXIT;  // code 10 = shove data block for this template.

            else begin    // no other codes defined for version 071. 6-5-01.

              for i := 0 to (block_ident.segment_length - 1) do begin
                // so skip this block (later version file?).

                if EOF(box_file) = True then
                  EXIT;
                BlockRead(box_file, inbyte, 1, number_read);
                if number_read <> 1 then begin
                  try
                    CloseFile(box_file);
                  except
                    on EInOutError do
                  end;  // close file if it's open.
                  if load_backup = False then
                    file_error(box_str);
                  EXIT;
                end;
              end;//next i
            end;
          end;//case  // no other codes defined for version 071. 6-5-01.

        until EOF(box_file) = True;
        // shouldn't get here, EXITs on a zero segment length.

      end;//if 071 format

    finally
      try
        CloseFile(box_file);
      except
        on EInOutError do
      end;  // close file if it's open.

      if keep_form.ignore_unused_menu_entry.Checked = True
      // finally remove any unwanted unused templates which got loaded...
      then begin
        n := old_count;
        while n < keeps_list.Count do begin
          if (keeps_list[n].template_info.keep_dims.box_dims1.bgnd_code_077 =
            0) and (keeps_list[n].template_info.keep_dims.box_dims1.this_was_control_template =
            False)  // 0.93.a

          then
            clear_keep(n)
          else
            Inc(n);
        end;//while
      end;
    end;//try
  end;
  /////////////////////////////////////////////////////////////

begin

  Result := False;               // init.
  last_bgnd_loaded_index := -1;  // init.

  if (append = False) and (keeps_list.Count > 0) and (load_backup = False) and
    (file_str = '') // something already there ?
  then begin
    if save_done = False                 // and not saved...
    then begin
      i := alert(7, '      reload  storage  box  -  save  first ?',
        'Your storage box contains one or more templates which have not yet been saved.' +
        ' Reloading your storage box will replace all of the existing contents and background drawing.'
        + '||These templates can be restored by clicking the `0UNDO RELOAD / UNDO CLEAR`1 menu item.'
        + ' But if any of these templates may be needed again, you should save them in a named data file.'
        + '||Do you want to save the existing contents before reloading?' +
        '||Or add the new templates to the existing contents instead?', '',
        '', 'add  new  templates  to  existing    ',
        'replace  existing  contents  without  saving    ', 'cancel  reload    ',
        'save  existing  contents  before  reloading      ', 0);
      case i of
        3:
          append := True;
        5:
          EXIT;
        6:
          if save_box(0, eSB_SaveAll, eSO_Normal, '') = False then
            EXIT;     // go save all the keeps box.
      end;//case
    end
    else begin      //  it has been saved...
      i := alert(7, '      reload  storage  box  -  clear  first ?',
        'Your storage box contains one or more existing templates.' +
        ' Reloading your storage box will replace all of the existing contents and background drawing.'
        + ' These templates can be restored by clicking the `0UNDO RELOAD / UNDO CLEAR`1 menu item.'
        + '||Are you sure you want to replace the existing templates?' +
        '||Or add the new templates to the existing contents instead?', '',
        '', '', 'add  new  templates  to  existing  ', 'cancel  reload    ',
        'reload  and  replace  existing  contents      ', 0);
      case i of
        4:
          append := True;
        5:
          EXIT;
      end;//case
    end;
  end;

  if load_backup = True then begin
    box_str := '';
    if FileExists(ebk1_str) = True then
      box_str := ebk1_str;
    if FileExists(ebk2_str) = True then
      box_str := ebk2_str;

    if box_str = '' then
      EXIT;    // no file to load.
  end
  else begin
    if file_str = '' then begin
      with keep_form.load_dialog do begin
        if append = False then
          Title := '    load  or  reload  storage  box  from  file ..'
        else begin
          if make_lib = True then
            Title := '    add  library  templates  from  file ..'
          else
            Title := '    add  templates  from  file ..';
        end;


        if his_load_file_name <> '' then
          InitialDir := ExtractFilePath(his_load_file_name)
        else
          InitialDir := Config.GetDir(cudiBoxes);

        Filter := ' storage  box  contents  (*.box3)|*.box3';
        Filename := '*.box3';

        if Execute = False then
          EXIT;          // get the file name.

        box_str := FileName;
        his_load_file_name := box_str;
        // so we can use the same folder next time.

      end;//with
    end
    else
      box_str := file_str;                       // file name supplied by caller.

    ixt_str := ChangeFileExt(box_str, '.ixt');

    if FileExists(box_str) = False then begin
      alert(5, '    error  -  file  not  found',
        '||The file :' + '||' + box_str +
        '||is not available. Please check that the file you require exists in the named folder. Then try again.'
        + '||No changes have been made to your storage box.',
        '', '', '', '', 'cancel  reload', '', 0);
      EXIT;
    end;
  end;

  // added 0.78.d 19-02-03...
  saved_control.keep_shove_list := Tshoved_timber_list.Create;
  fill_kd(saved_control);                             // save control template.
  saved_control_name_str := current_name_str;
  saved_control_memo_str := current_memo_str;

  resave_needed := False;                         // init.
  restored_save_done := False;                    // init.
  loaded_version := 50000;                        // init for lowest template version in the file.
  later_file := False;                            // init.

  loading_in_progress := True;
  // 208c lock out any auto backups while loading -- in case any dialogs shown and OnIdle fires

  try // 208c

    if (append = True) and (keep_form.add_new_group_menu_entry.Checked = True) then
      clear_all_selections;   // he wants added templates to form a new group.

    // begin loading...

    saved_cursor := Screen.Cursor;

    try
      Screen.Cursor := crHourGlass;        // could take a while if big file.
      if Application.Terminated = False then
        Application.ProcessMessages;       // so let the form repaint.

      if append = False then
        clear_keeps(False, True);
      // first clear all existing (sets save_done:=True), and save existing for undo.

      old_count := keeps_list.Count;          // for append.

      try
        AssignFile(test_box_file, box_str);      // set the file name (untyped file).
        FileMode := 0;
        // read only (this is a global setting for all subsequent Resets).
        Reset(test_box_file, 1);                 // open for reading, record size = 1 byte.

        //  Tkeep_dims1=record      // first part of a Tkeep_dims record.

        BlockRead(test_box_file, old_next_data.old_keep_dims1, SizeOf(Tkeep_dims1), number_read);
        // read bytes.
        if number_read <> SizeOf(Tkeep_dims1) then begin
          try
            CloseFile(test_box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        CloseFile(test_box_file);    // and close the file. (Re-open later.)
      except
        on EInOutError do begin
          if load_backup = False then
            file_error(box_str);
          if append = False then
            clear_keeps(False, False);
          EXIT;
        end;
      end;//try-except

      with old_next_data.old_keep_dims1.box_dims1 do begin
        s := box_ident;
        if Copy(s, 1, 1) = 'N'              // it's in new file format (v:0.48 on, 17-2-00)
        then begin

          if templot_version > 70
          // mods for v:0.71.a  (shove-timber data blocks appended to file). 2-5-01.
          then
            _071_format := True
          else
            _071_format := False;

          if load_backup = True then begin

            if templot_version > 62 then
              restored_save_done := box_save_done;     // mods 23-6-00 for version 0.63

            if startup_restore_pref = 1 then
              EXIT;   //%%%%   0=ask, 1=don't restore, 2=restore without asking

            if (auto_restore_on_startup = False) or (ask_restore_on_startup = False)
            // if both True??? - must have been abnormal termination, so reload without asking.
            then begin

              if startup_restore_pref = 0
              //%%%%   0=ask, 1=don't restore, 2=restore without asking
              then begin
                if auto_restore_on_startup = False
                // these two only read from the first keep in the file..
                then begin
                  if ask_restore_on_startup = True
                  // he wanted to be asked first.
                  then begin

                    alert_box.
                      preferences_checkbox.Checked := False;       //%%%%
                    if user_prefs_in_use = True then
                      alert_box.preferences_checkbox.Show;

                    repeat
                      i :=
                        alert(4, '    restore  previous  work ?',
                        ' |Do you want to restore your work in progress from your previous Templot0 session?| ',
                        '', '', '', 'more  information', 'no  thanks',
                        'yes  please  -  restore  previous  work', 4);
                      case i of
                        4:
                          alert_help(0, ask_restore_str, '');
                        //%%%%% 5: EXIT;
                      end;//case
                    until i <> 4;

                    //%%%%   0=ask, 1=don't restore, 2=restore without asking

                    if alert_box.preferences_checkbox.Checked   //%%%%
                    then
                      startup_restore_pref := (i - 4)         // 5 or 6 = 1 or 2
                    else
                      startup_restore_pref := 0;

                    alert_box.
                      preferences_checkbox.Hide;

                    if i = 5 then
                      EXIT;  //%%%%

                  end
                  else
                    EXIT;     // restore not wanted.
                end;
                // 0.93.a else keep_form.auto_ebk_load_menu_entry.Checked:=True;      // radio item (maintain this option only for next time).

              end;// ask startup pref
            end;//not abnormal termination
          end;//reload backup

          // load the file...

          wait_form.cancel_button.Hide;
          wait_form.waiting_label.Caption := 'loading  templates ...';

          wait_form.waiting_label.Width :=
            wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // 205b bug fix for Wine

          if normal_load = True then
            wait_form.Show;  // 208d version warnings off for file viewer

          if Application.Terminated = False then
            Application.ProcessMessages;           // let the wait form fully paint.
          if load_new_format = False then
            EXIT;    // go get file in new format.
        end;

      end;//with
      // file loaded...

      if (file_str = '') then
        loaded_str := box_str     // file name from the "open" dialog.
      else begin
        if ExtractFileExt(file_str) = '.box3' then
          loaded_str := file_str
        else
          loaded_str := 'data file';        // don't confuse him with internal file names.
      end;

      if (ExtractFileExt(loaded_str) = '.box3') and (normal_load = True)
      // 208d not for file viewer
      then
        boxmru_update(loaded_str);                              // 0.82.a  update the mru list.

      // file loaded, check it and update the background drawing...

      if append = False then begin
        with old_next_data.old_keep_dims1.box_dims1 do begin

          box_project_title_str := project_for;  // change the title to the one loaded last.

          //     0.79.a 20-05-06  -- saved grid info -- read from last template only...
          //     0.91.d -- read these only if prefs not being used on startup.

          if (grid_units_code <> 0) and (user_prefs_in_use = False)
          // 0.79 file or later --- change grid to as loaded...
          then begin

            grid_labels_code_i := grid_units_code;

            grid_spacex := x_grid_spacing;
            grid_spacey := y_grid_spacing;

            if ruler_units = 0 then
              update_ruler_div;   // 0.93.a ruler as grid option

          end;// if 0.79 or later

        end;//with old_next_data.old_keep_dims1

        save_done := not resave_needed;        // this boxful matches file.
        if load_backup = False then begin
          keep_form.box_file_label.Caption := ' last reloaded from :  ' + loaded_str;
          keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
          // in case too long for caption

          saved_box_str := loaded_str;
          // for print of box contents list.
          reloaded_box_str := '|    ' + loaded_str;
          // ditto.
        end
        else begin
          save_done := restored_save_done;
          // had it been saved?
          keep_form.box_file_label.Caption := ' restored on startup';
          keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
          // in case too long for caption

          saved_box_str := 'startup restore';
          // for print of box contents list.
          reloaded_box_str := '|    startup restore';               // ditto.
        end;
      end
      else begin                 // appending...
        //save_done:=False;
        keep_form.box_file_label.Caption := ' last added from :  ' + loaded_str;
        keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
        // in case too long for caption

        reloaded_box_str := reloaded_box_str + '|    ' + loaded_str;
        // for print of box contents list.
      end;

      if append = False then
        current_state(0)       // update or create listbox entries, need names for refresh...
      else
        current_state(-1);

      // refresh or clear backgrounds for newly loaded keeps...

      if keeps_list.Count <= old_count then
        EXIT;   // cleared on error or nothing loaded.

      with keep_form do begin

        if append = False then
          i := 0
        else
          i := old_count;

        for n := i to (keeps_list.Count - 1) do begin
          if keeps_list[n].template_info.keep_dims.box_dims1.bgnd_code_077 =
            1 then begin
            if update_background_menu_entry.Checked = True then begin
              last_bgnd_loaded_index := n;
              // update index to highest loaded bgnd (for minting).
              list_position := n;
              // put new keep on background.
              copy_keep_to_background(n, False, True);
              // don't update info, reloading=True.
            end
            else begin
              with keeps_list[n].template_info.keep_dims.box_dims1 do begin
                bgnd_code_077 := 0;          // make it unused instead.
                pre077_bgnd_flag := False;
                // in case reloaded in older version than 0.77.a
              end;//with
            end;
          end;
        end;//for

        if update_background_menu_entry.Checked = True then
          pad_form.fit_bgnd_menu_entry.Click;  // show the new background.

      end;//with

      backup_wanted := True;                    // file loaded ok, update the backup.
      Result := True;                           // file loaded.

    finally
      wait_form.Close;
      Screen.Cursor := saved_cursor;
      current_state(-1);                   // tidy up after any error exits.

      copy_keep(saved_control);            // retrieve saved current...
      current_name_str := saved_control_name_str;
      current_memo_str := saved_control_memo_str;
      info_form.ref_name_label.Caption := current_name_str;

      saved_control.keep_shove_list.Free;

    end;//try

    if (later_file = True) and (normal_load = True)   // normal_load 208d (off for file viewer)
    then begin
      alert(1, 'php/980    later  file   -   ( from  version  ' + FormatFloat(
        '0.00', loaded_version / 100) + ' )',
        'The file which you just reloaded contained one or more templates from a later version of Templot0 than this one.'
        +
        ' Some features may not be available or may be drawn differently.' +
        '||The earliest loaded template was from version  ' +
        FormatFloat('0.00', loaded_version / 100) +
        '|This version of Templot0 is  ' + GetVersionString(voShort) +
        '||Please refer to the Templot web site at  templot.com  for information about upgrading to the latest version, or click| <A HREF="online_ref980.85a">more information online</A> .',
        '', '', '', '', '', 'continue', 0);
    end;

    if (loaded_version < 200) and (normal_load = True)   // normal_load 208d (off for file viewer)
    then begin
      i := alert(2, 'php/980    old  file   -   ( from  version  ' +
        FormatFloat('0.00', loaded_version / 100) + ' )',
        'The file which you just reloaded contained one or more templates from an earlier version of Templot0.'
        + '||These have been modified to make them compatible with this version, but some features may now be drawn differently or require adjustment.'
        //+'||To re-create the templates from scratch in line with this version, click the blue bar below or select the PROGRAM > NORMALIZE ALL TEMPLATES menu item on the PROGRAM PANEL window.'
        + '||The earliest loaded template was from version  ' +
        FormatFloat('0.00', loaded_version / 100) + '|This version of Templot0 is  ' +
        GetVersionString(voShort) +
        '||Click for <A HREF="online_ref980.85a">more information online</A> about the differences between these two versions.'
        //+'||Please refer to the Templot web site at  templot.com  for information about the differences between these two versions.'
        + '||green_panel_begin tree.gif The template name labels are now shown in the boxed style by default.'
        + ' To revert to the previous style click the `0trackpad > trackpad background options > background name labels > transparent`1 menu item,|or click below.' + '||To hide the name labels, press the `0END`2 key on the keyboard, or the `0SHIFT+ENTER`2 keys, or click the `0trackpad > hide name labels`1 menu item, or click below.green_panel_end', '', '', 'hide  name  labels', 'change  to  transparent  name  labels', '', 'continue', 0);

      if i = 3 then
        hide_name_labels := True;

      if i = 4 then
        pad_form.transparent_names_menu_entry.Checked := True;    // radio item.
    end;

  finally
    loading_in_progress := False;  // 208c allow backups only after dialogs
  end;//try
end;

//___________________________________________________________________________________________

procedure clear_keep(n: integer);     // clear a single keep without asking.
// this is for errors on loading,
// and ignore unused option.
var
  i: integer;

begin

  if keeps_list.Count < 1 then
    EXIT;

  if (n >= 0) and (n < keeps_list.Count) then begin
    keeps_list.Delete(n);
    save_done := False;
    backup_wanted := True;
  end;
end;


end.

