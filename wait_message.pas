
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

unit wait_message;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  {$interfaces COM}
  {
    This is an interface to the basic methods required for working
    with a WaitMessage dialog.

    IAutoWaitMessage is explicit declared as a COM Interface, so that
    the compiler will generate automatic reference counting for any
    instance of this interface.

    See the TWaitForm class functions to ShowWaitMessage...() which
    return instances of this interface.

    Using the interface (with automatic reference counting) means that
    the WaitMessage form will automatically be hidden when the variable
    goes out of scope.

    To hide the WaitMessage earlier, set the interface variable to nil.
  }
  IAutoWaitMessage = interface
    ['{629D827B-A704-4848-9C64-834B8F5E57B4}']
    procedure StepIt;
    procedure StepItWithWraparound;
    procedure UpdateMessage(newMessage: String);
    function IsCancelled: Boolean;
  end;

  TWaitComputeProc = function(Data: Pointer; waitMessage: IAutoWaitMessage): Integer;

  { TWaitForm }

  TWaitForm = class(TForm)
    triggerTimer: TTimer;
    waitingLabel: TLabel;
    signalImage: TImage;
    cancelButton: TButton;
    waitProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure cancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure triggerTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FWaitCancelClicked: boolean;
    FComputeFunction: TWaitComputeProc;
    FComputeData: Pointer;
    FComputeResult: Integer;

    procedure StepIt;
    procedure StepItWithWraparound;

    procedure SetupCaption(Caption: String);
    procedure SetupProgressBar(min, max, position, step: Integer);
    procedure SetupCancel;

    function ShowModalAndCompute(computeFunction: TWaitComputeProc; Data: Pointer): Integer;

  public
    { Public declarations }

    {
      Show the WaitMessage form (non-modal) with the specified caption.

      @returns an IAutoWaitMessage instance to control the display of the wait message form.

      The WaitMessage form will be hidden when the interface variable is set to nil,
      or goes out of scope.
    }
    class function ShowWaitMessage(Caption: String): IAutoWaitMessage;

    {
      Show the WaitMessage form (non-modal) with the specified caption and a progress bar.

      @returns an IAutoWaitMessage instance to control the display of the wait message form.

      The WaitMessage form will be hidden when the interface variable is set to nil,
      or goes out of scope.

      Use the StepIt or StepItWithWrapAround methods to advance the progress bar. Ensure
      Application.ProcessMessages is called regularly to update the WaitMessage form.
    }
    class function ShowWaitMessageWithProgress(Caption: String;
      min, max, position, step: Integer): IAutoWaitMessage;

    {
      Show the WaitMessage form (non-modal) with the specified caption, a progress bar and
      a Cancel button.

      @returns an IAutoWaitMessage instance to control the display of the wait message form.

      The WaitMessage form will be hidden when the interface variable is set to nil,
      or goes out of scope.

      Use the StepIt or StepItWithWrapAround methods to advance the progress bar.

      Check the IsCancelled method regularly and abandon the calculation if it returns True.

      Ensure Application.ProcessMessages is called regularly to update the WaitMessage form.
    }
    class function ShowWaitMessageWithProgressAndCancel(Caption: String;
      min, max, position, step: Integer): IAutoWaitMessage;

    {
      Show the WaitMessage form (modal) with the specified caption, and then calls
      the given computeFunction with the Data as provided.

      @returns -1 if Cancelled, otherwise the return value of computeFunction.

      The computeFunction needs to call Application.ProcessMessages regularly, and
      check the IsCancelled method of the IAutoWaitMessage instance.
    }
    class function ShowWaitMessageAndCompute(Caption: String; computeFunction: TWaitComputeProc;
      Data: Pointer): Integer;

    {
      Show the WaitMessage form (modal) with the specified caption and a Progress Bar,
      and then calls the given computeFunction with the Data as provided.

      @returns -1 if Cancelled, otherwise the return value of computeFunction.

      The computeFunction needs to call Application.ProcessMessages regularly, and
      check the IsCancelled method of the IAutoWaitMessage instance.

      Call the StepIt or StepItWithWraparound methods of the IAutoWaitMessage instance
      to advance the Progress Bar.
    }
    class function ShowWaitMessageWithProgressAndCompute(Caption: String;
      min, max, position, step: Integer; computeFunction: TWaitComputeProc;
      Data: Pointer): Integer;
  end;

var
  WaitForm: TWaitForm;

implementation

{$R *.lfm}

type
  TWaitFormWrapper = class(TInterfacedObject, IAutoWaitMessage)
  public
    constructor Create;
    destructor Destroy; override;

    procedure StepIt;
    procedure StepItWithWraparound;
    procedure UpdateMessage(newMessage: String);
    function IsCancelled: Boolean;
  end;

//__________________________________________________________________________________________

procedure TWaitForm.FormCreate(Sender: TObject);

begin
  if Screen.DesktopWidth < 1000 then begin
    Top := 160;
    Left := 240;
  end;

  // OT-FIRST ClientWidth:=234;
  // OT-FIRST ClientHeight:=64;
  AutoScroll := False;
end;
//______________________________________________________________________________

procedure TWaitForm.cancelButtonClick(Sender: TObject);

begin
  FWaitCancelClicked := True;
end;
//______________________________________________________________________________

procedure TWaitForm.FormShow(Sender: TObject);

var
  newWidth: Integer;
  minWidth: Integer;

  // 0.95.a      // 205b  label_width bug fix in Wine
begin
  waitingLabel.Width := Canvas.TextWidth(waitingLabel.Caption) + 10;

  newWidth := waitingLabel.Left + waitingLabel.Width + 2;  // make sure it is all visible

  minWidth := cancelButton.Left + cancelButton.Width + 12;

  if cancelButton.Visible then begin
    // bug-fix 208d
    if newWidth < minWidth then
      newWidth := minWidth;  // ensure cancel button is visible.
  end
  else begin
    if newWidth < cancelButton.Left then
      newWidth := cancelButton.Left;  // minimum width.
  end;

  ClientWidth := newWidth;
end;

procedure TWaitForm.triggerTimerTimer(Sender: TObject);
var
  waitMessage: IAutoWaitMessage;
begin
  triggerTimer.Enabled := False;

  waitMessage := TWaitFormWrapper.Create;
  FComputeResult := FComputeFunction(FComputeData, waitMessage);

  if FWaitCancelClicked then
    modalResult := mrCancel
  else
    modalResult := mrOk;
end;

//______________________________________________________________________________

procedure TWaitForm.StepIt;
begin
  waitProgressBar.StepIt;
end;

procedure TWaitForm.StepItWithWrapAround;
begin
  waitProgressBar.StepIt;
  if waitProgressBar.Position >= waitProgressBar.Max then
    waitProgressBar.Position := waitProgressBar.Min;
end;

procedure TWaitForm.SetupCaption(Caption: String);
begin
  FWaitCancelClicked := False;
  cancelButton.Visible := False;
  waitProgressBar.Visible := False;
  waitingLabel.Caption := Caption;
  waitingLabel.Width := WaitForm.Canvas.TextWidth(Caption);
end;

procedure TWaitForm.SetupProgressBar(min, max, position, step: Integer);
begin
  waitProgressBar.Min := min;
  waitProgressBar.Max := max;
  waitProgressBar.Position := position;
  waitProgressBar.Step := step;
  waitProgressBar.Visible := True;
end;

procedure TWaitForm.SetupCancel;
begin
  cancelButton.Visible := True;
end;

function TWaitForm.ShowModalAndCompute(computeFunction: TWaitComputeProc; Data: Pointer): Integer;
var
  mr: Integer;
begin
  FComputeFunction := computeFunction;
  FComputeData := Data;
  triggerTimer.Enabled := True;

  mr := ShowModal;

  if mr = mrOk then
    Result := WaitForm.FComputeResult
  else
    Result := -1;

end;

class function TWaitForm.ShowWaitMessage(Caption: String): IAutoWaitMessage;
begin
  Result := TWaitFormWrapper.Create;

  WaitForm.SetupCaption(Caption);
  WaitForm.Show;
end;

class function TWaitForm.ShowWaitMessageWithProgress(Caption: String;
  min, max, position, step: Integer): IAutoWaitMessage;
begin
  Result := TWaitFormWrapper.Create;

  WaitForm.SetupCaption(Caption);
  WaitForm.SetupProgressBar(min, max, position, step);
  WaitForm.Show;
end;

class function TWaitForm.ShowWaitMessageWithProgressAndCancel(Caption: String;
  min, max, position, step: Integer): IAutoWaitMessage;
begin
  Result := TWaitFormWrapper.Create;

  WaitForm.SetupCaption(Caption);
  WaitForm.SetupProgressBar(min, max, position, step);
  WaitForm.SetupCancel;
  WaitForm.Show;
end;

class function TWaitForm.ShowWaitMessageAndCompute(Caption: String;
  computeFunction: TWaitComputeProc; Data: Pointer): Integer;
var
  mr: Integer;
begin
  WaitForm.SetupCaption(Caption);
  WaitForm.SetupCancel;

  Result := WaitForm.ShowModalAndCompute(computeFunction, Data);
end;

class function TWaitForm.ShowWaitMessageWithProgressAndCompute(Caption: String;
  min, max, position, step: Integer; computeFunction: TWaitComputeProc; Data: Pointer): Integer;
var
  mr: Integer;
begin
  WaitForm.SetupCaption(Caption);
  WaitForm.SetupProgressBar(min, max, position, step);
  WaitForm.SetupCancel;

  WaitForm.FComputeFunction := computeFunction;
  WaitForm.FComputeData := Data;
  WaitForm.triggerTimer.Enabled := True;

  Result := WaitForm.ShowModalAndCompute(computeFunction, Data);
end;

constructor TWaitFormWrapper.Create;
begin
  inherited;
end;

destructor TWaitFormWrapper.Destroy;
begin
  WaitForm.Hide;
  inherited;
end;

procedure TWaitFormWrapper.StepIt;
begin
  WaitForm.StepIt;
  Application.ProcessMessages;
end;

procedure TWaitFormWrapper.StepItWithWraparound;
begin
  WaitForm.StepItWithWraparound;
  Application.ProcessMessages;
end;

procedure TWaitFormWrapper.UpdateMessage(newMessage: String);
begin
end;

function TWaitFormWrapper.IsCancelled: Boolean;
begin
  Result := WaitForm.FWaitCancelClicked;
end;

end.
