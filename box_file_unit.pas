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

  ELoadBox = (
    eLB_Normal,
    eLB_FileViewer,
    eLB_Backup,
    eLB_Library);


function save_box(this_one: integer; which_ones: ESaveBox; save_option: ESaveOption;
  save_str: string): boolean;
function load_storage_box(load_options: ELoadBox;
  file_str: string;
  var append: boolean;
  var last_bgnd_loaded_index: integer): boolean;

procedure file_error(str: string);     // generic error message.
// clear a single keep without asking - this is for errors on loading.
procedure clear_keep(n: integer);


implementation

uses
  Controls,
  Forms,
  Dialogs,
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


function load_storage_box(load_options: ELoadBox; file_str: string;
  var append: boolean;
  var last_bgnd_loaded_index: integer): boolean;
  // load a file of templates into the keeps box.

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
  i, n, fsize: integer;
  loaded_str, box_str, ident: string;
  no_ixt: boolean;
  old_count: integer;                // for append.
  resave_needed: boolean;
  s, info_string, memo_string, _str: string;
  saved_cursor: TCursor;
  restored_save_done: boolean;
  project_title: string;
  grid_info: TGridInfo;
  loaded_templates: TTemplateList;

  saved_control: Ttemplate_info;
  saved_notch: Tnotch;

  saved_control_name_str: string;
  saved_control_memo_str: string;

  loadDialog: TOpenDialog;
  backupRestoreOptions: TBackupRestoreOptions;
  t: TTemplate;

  ///////////////////////////////////////////////////////////////

begin

  Result := False;               // init.
  last_bgnd_loaded_index := -1;  // init.

  if load_options = eLB_FileViewer then begin
    // FileViewer will call LoadBox3() directly
          EXIT;
        end;


  if (load_options = eLB_Normal) and not append and (keeps_list.Count > 0) and
    (file_str = '') then begin
    // something already there ?
    if not save_done then begin
      // and not saved...
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
          if not save_box(0, eSB_SaveAll, eSO_Normal, '') then
            EXIT;     // go save all the keeps box.
      end;//case
    end;
  end;

  if load_options = eLB_Backup then begin
    box_str := '';
    if FileExists(ebk1_str) then
      box_str := ebk1_str;
    if FileExists(ebk2_str) then
      box_str := ebk2_str;

    if box_str = '' then
      EXIT;    // no file to load.
  end
  else begin
    if file_str = '' then begin
      loadDialog := TOpenDialog.Create(nil);
      try
        if not append then
          loadDialog.Title := '    load  or  reload  storage  box  from  file ..'
        else begin
          if load_options = eLB_Library then
            loadDialog.Title := '    add  library  templates  from  file ..'
          else
            loadDialog.Title := '    add  templates  from  file ..';
        end;

        if his_load_file_name <> '' then
          loadDialog.InitialDir := ExtractFilePath(his_load_file_name)
        else
          loadDialog.InitialDir := Config.GetDir(cudiBoxes);

        loadDialog.Filter := ' storage  box  contents  (*.box3)|*.box3';
        loadDialog.Filename := '*.box3';

        if not loadDialog.Execute then
          EXIT;          // get the file name.

        box_str := loadDialog.FileName;
        his_load_file_name := box_str;


      finally
        loadDialog.Free;
      end;
    end
    else begin
      // file name supplied by caller
      box_str := file_str;
    end;

    if not FileExists(box_str) then begin
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

    // begin loading...
    saved_cursor := Screen.Cursor;

    try
      Screen.Cursor := crHourGlass;        // could take a while if big file.
      if not Application.Terminated then
        Application.ProcessMessages;       // so let the form repaint.

      if not append then
        clear_keeps(False, True);
      // first clear all existing (sets save_done:=True), and save existing for undo.


      if load_options = eLB_Backup then begin
          try
          backupRestoreOptions := LoadBox3BackupRestoreOptions(box_str);
          except
          on ExLoadBox do begin
            Exit;
        end;
        end;

        restored_save_done := backupRestoreOptions.saveDone;     // mods 23-6-00 for version 0.63

            if startup_restore_pref = 1 then
              EXIT;   //%%%%   0=ask, 1=don't restore, 2=restore without asking

        if (not backupRestoreOptions.autoRestoreOnStartup) or
          (not backupRestoreOptions.askRestoreOnStartup) then begin
            // if both True??? - must have been abnormal termination, so reload without asking.

              if startup_restore_pref = 0
              //%%%%   0=ask, 1=don't restore, 2=restore without asking
              then begin
            if not backupRestoreOptions.autoRestoreOnStartup then begin
                // these two only read from the first keep in the file..
              if backupRestoreOptions.askRestoreOnStartup then begin
                  // he wanted to be asked first.

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

      end;

          wait_form.cancel_button.Hide;
          wait_form.waiting_label.Caption := 'loading  templates ...';

          wait_form.waiting_label.Width :=
            wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // 205b bug fix for Wine

      wait_form.Show;

      if not Application.Terminated then
            Application.ProcessMessages;           // let the wait form fully paint.

      try
        LoadBox3(box_str, project_title, grid_info, loaded_templates);

      except
        on ExLoadBox do begin
          if load_options <> eLB_Backup then
            file_error(box_str);
          Exit;
        end;
        end;

      // file loaded...

      old_count := keeps_list.Count;          // for append.
      if not append then begin
        clear_keeps(False, True);
      end;

      if append and keep_form.add_new_group_menu_entry.Checked then
        clear_all_selections;   // he wants added templates to form a new group.

      while loaded_templates.Count > 0 do begin
        // move ownership of templates to keeps_list
        t := loaded_templates[0];
        loaded_templates.Extract(t);

        if load_options = eLB_Library then begin
          // make it a library template.
          t.template_info.keep_dims.box_dims1.bgnd_code_077 := -1;
        end
        else
        if append and not keep_form.add_ignore_group_menu_entry.Checked then begin
          // group select added template.
          t.group_selected := True;
        end;

        n := keeps_list.Add(t);
      end;


      if (file_str = '') then
        loaded_str := box_str     // file name from the "open" dialog.
      else begin
        if ExtractFileExt(file_str) = '.box3' then
          loaded_str := file_str
        else
          loaded_str := 'data file';        // don't confuse him with internal file names.
      end;

      if (ExtractFileExt(loaded_str) = '.box3') and (load_options <> eLB_FileViewer)
      // 208d not for file viewer
      then
        boxmru_update(loaded_str);                              // 0.82.a  update the mru list.

      // file loaded, check it and update the background drawing...

      if not append then begin
        box_project_title_str := project_title;  // change the title to the one loaded last.

          //     0.79.a 20-05-06  -- saved grid info -- read from last template only...
          //     0.91.d -- read these only if prefs not being used on startup.

        if (grid_info.unitsCode <> 0) and (not user_prefs_in_use)
          // 0.79 file or later --- change grid to as loaded...
          then begin

          grid_labels_code_i := grid_info.unitsCode;

          grid_spacex := grid_info.spaceX;
          grid_spacey := grid_info.spaceY;

            if ruler_units = 0 then
              update_ruler_div;   // 0.93.a ruler as grid option

          end;// if 0.79 or later

        save_done := not resave_needed;        // this boxful matches file.
        if load_options <> eLB_Backup then begin
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

      if not append then
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

    if (later_file) and (load_options <> eLB_FileViewer)
    // normal_load 208d (off for file viewer)
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


