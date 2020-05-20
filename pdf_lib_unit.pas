//  This unit provides a wrapper round fpPDF to hide the details of that library
// and provide an interface more in keeping with the Templot pdf_unit requirements.

// As an example, functions here:
//    - take x,y coords with origin at top left and convert to a top right origin
//    - take coordinates in dpi and turn them into mm.
//    - take colors in BGR and turn them into RGB
//    - etc

unit pdf_lib_unit;

{$MODE Delphi}

interface

uses
  fpPDF, Classes, Dialogs, FPCanvas, Graphics, SysUtils, Types,
  preview_unit;

type
  Tpdf_TextPosn = (
    tpBottomLeft, tpBottomCentre, tpBottomRight,
    tpMiddleLeft, tpMiddleCentre, tpMiddleRight,
    tpTopLeft, tpTopCentre, tpTopRight);

  Tpdf_textmode = (tmFill, tmStroke, tmFillStroke, tmNone,
    tmFillPath, tmStrokePath, tmFillStrokePath, tmpath);

  Tpdf_page = class(TPDFPage)
  private
    Width: Single;
    Height: Single;
    curr_pen_style: TPDFPenStyle;
    curr_pen_width: Double;
    curr_pen_color: Integer;
    curr_fill_color: Integer;
    curr_font_index: Integer;
    curr_font_size: Integer;
    function dots_to_px(dots: Integer): double;
    function px_to_dots(px: Double): Integer;
    function fontNum(const AFontName: string): Integer;
  public
    constructor Create(AOwner: TPDFDocument);
    procedure draw_line(dots_x1, dots_y1, dots_x2, dots_y2: Integer); overload;
    procedure draw_line(MvTo, LineTo: TPoint); overload;
    procedure draw_line_style(dots_x1, dots_y1, dots_x2, dots_y2: Integer;
      style: Integer); overload;
    procedure draw_line_style(MvTo, LineTo: TPoint; style: Integer); overload;
    procedure draw_open_line(dots_x1, dots_y1, dots_x2, dots_y2: Integer;
      colour: Integer); overload;
    procedure draw_open_line(MvTo, LineTo: TPoint; colour: Integer); overload;
    procedure polygon(dots: array of Tpoint);
    procedure set_fill_color(color: Integer);
    procedure set_font(FontIndex: Integer; FontSize: Integer);
    procedure set_landscape();
    procedure set_textmode(mode: TPDF_textmode);
    procedure set_pen_color(color: Integer);
    procedure set_pen_style(APenStyle: TFPPenStyle = psSolid);
    procedure set_pen_width(Width: LongInt = 1);
    procedure write_comment(comment: String);
    procedure write_text(dots_x, dots_y: Integer; Text: String); overload;
    procedure write_text(dots_x, dots_y: Integer; Text: String; base: Tpdf_TextPosn); overload;
    procedure write_text(dots_x, dots_y: Integer; Text: String; blank_needed: Boolean);
      overload;
    procedure write_text(dots_x, dots_y: Integer; Text: String; base: Tpdf_TextPosn;
      blank_needed: Boolean); overload;
    procedure write_text(dots_x, dots_y: Integer; Text: String; shift: array of Single;
      blank_needed: Boolean); overload;

    // The following are copied from fpPDF
    function GetStdFontCharWidthsArray(const AFontName: string): TPDFFontWidthArray;
    function GetTextWidth(Text: String): single;
    function GetTextHeight(Text: String): single;
  end;

  Tpdf_document = class(TPDFDocument)
  private
    pdf_section: TPDFSection;
  public
    constructor Create(AOwner: TComponent); override;
    function new_page(): TPDF_Page;
  end;


implementation

uses
  strutils;

{$BOOLEVAL ON}

{$I fontmetrics_stdpdf.inc }

//=======================================================================================

function c_to_rgb(color: Integer): Integer;
var
  r, g, b: Integer;
begin
  r := (color) and $ff;
  g := (color shr 8) and $ff;
  b := (color shr 16) and $ff;
  Result := (((r shl 8) or g) shl 8) or b;
end;

function dots_to_mm(dots: Integer): Double;
begin
  Result := dots * 25.4 / 600;
end;


//=======================================================================================


//{$R *.lfm}

//uses

//var

//_______________________________________________________________________________________

constructor Tpdf_document.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartDocument;
  pdf_section := Sections.AddSection;    // Our documents have only one section
end;

//_______________________________________________________________________________________
function Tpdf_document.new_page: TPDF_Page;
var
  pdf_page: TPDF_Page;
begin
  pdf_page := TPDF_page.Create(Self);      // Make a new page ...
  Pages.Add(pdf_page);                     // ... add it to the document ...
  pdf_section.AddPage(pdf_page);           // ... add it to the Section ...
  Result := pdf_page;                      // ... then return it :-)
end;


//=======================================================================================

constructor Tpdf_page.Create(AOwner: TPDFDocument);
begin
  inherited Create(AOwner);
  Height := 297; // Default is portrait
end;

//_______________________________________________________________________________________

function TPDF_page.dots_to_px(dots: LongInt): double;
const
  px_per_dot = 72 / 600;           // px per inch / dots per inch
begin
  Result := dots * px_per_dot;
end;

//_______________________________________________________________________________________

function TPDF_page.px_to_dots(px: Double): Integer;
const
  dots_per_px = 600 / 72;            // dots per inch / px per inch
begin
  Result := round(px * dots_per_px);
end;


//_______________________________________________________________________________________
procedure TPDF_page.set_landscape();
begin
  orientation := ppoLandscape;
  Height := 210;
end;

//_______________________________________________________________________________________
procedure TPDF_page.set_pen_style(APenStyle: TFPPenStyle = psSolid);
begin
  case APenStyle of
    psDot:
      curr_pen_style := ppsDot;
    psDash:
      curr_pen_style := ppsDash;
    else
      curr_pen_style := ppsSolid;
  end;

end;

//_______________________________________________________________________________________
procedure TPDF_page.set_pen_width(Width: LongInt = 1);
begin
  curr_pen_width := dots_to_px(Width);
end;

//_______________________________________________________________________________________

procedure TPDF_page.set_textmode(mode: TPDF_textmode);

begin
  //int_mode = integer(mode);
  AddObject(TPDFFreeFormString.Create(Document, IntToStr(integer(mode)) + ' Tr' + CRLF));
end;

//_______________________________________________________________________________________

procedure TPDF_page.draw_line(dots_x1, dots_y1, dots_x2, dots_y2: Integer);

var
  x1, y1, x2, y2: Double;

const
  dpmm = 600 / 25.4;                 // Dots per mm

begin
  x1 := dots_to_mm(dots_x1);
  y1 := Height - dots_to_mm(dots_y1);
  x2 := dots_to_mm(dots_x2);
  y2 := Height - dots_to_mm(dots_y2);
  DrawLine(x1, y1, x2, y2, curr_pen_width, True);
end;

//_______________________________________________________________________________________

procedure TPDF_page.draw_line(MvTo, LineTo: TPoint);
begin
  draw_line(MvTo.x, MvTo.y, LineTo.x, LineTo.y);
end;

//_______________________________________________________________________________________

procedure TPDF_page.draw_line_style(dots_x1, dots_y1, dots_x2, dots_y2: Integer; style: Integer);

var
  x1, y1, x2, y2: Double;

const
  dpmm = 600 / 25.4;                 // Dots per mm

begin
  x1 := dots_to_mm(dots_x1);
  y1 := Height - dots_to_mm(dots_y1);
  x2 := dots_to_mm(dots_x2);
  y2 := Height - dots_to_mm(dots_y2);
  DrawLineStyle(x1, y1, x2, y2, style);
end;

//_______________________________________________________________________________________

procedure TPDF_page.draw_line_style(MvTo, LineTo: TPoint; style: Integer);
begin
  draw_line_style(MvTo.x, MvTo.y, LineTo.x, LineTo.y, style);
end;

//_______________________________________________________________________________________
//  Line without endpoints

procedure TPDF_page.draw_open_line(dots_x1, dots_y1, dots_x2, dots_y2, colour: Integer);
begin
  draw_line(dots_x1, dots_y1, dots_x2, dots_y2);
end;

//_______________________________________________________________________________________
//  Line without endpoints

procedure TPDF_page.draw_open_line(MvTo, LineTo: TPoint; colour: Integer);
begin
  draw_open_line(MvTo.x, MvTo.y, LineTo.x, LineTo.y, colour);
end;

//_______________________________________________________________________________________

procedure TPDF_Page.write_comment(comment: String);
begin
  AddObject(TPDFFreeFormString.Create(Document, '% ' + comment + CRLF));
end;

//_______________________________________________________________________________________

procedure TPDF_page.write_text(dots_x, dots_y: Integer; Text: String); overload;
begin
  write_text(dots_x, dots_y, Text, tpBottomLeft);
end;

procedure TPDF_page.write_text(dots_x, dots_y: Integer; Text: String;
  base: Tpdf_TextPosn); overload;
begin
  write_text(dots_x, dots_y, Text, base, False);
end;

procedure TPDF_page.write_text(dots_x, dots_y: Integer; Text: String;
  blank_needed: Boolean); overload;
begin
  write_text(dots_x, dots_y, Text, tpBottomLeft, blank_needed);
end;

procedure TPDF_page.write_text(dots_x, dots_y: Integer; Text: String;
  base: Tpdf_TextPosn; blank_needed: Boolean); overload;

const
  shiftnums: array[Tpdf_textposn] of array[0..1] of Single =
    ((0.0, 0.0), (-0.5, 0.0), (-1.0, 0.0),
    (0.0, -0.5), (-0.5, -0.5), (-1.0, -0.5),
    (0.0, -1.0), (-0.5, -1.0), (-1.0, -1.0));
  //       BottomLeft,  BottomCentre, BottomRight,
  //       MiddleLeft,  MiddleCentre, MiddleRight,
  //       TopLeft,     TopCentre,    TopRight
begin
  write_text(dots_x, dots_y, Text, shiftnums[base], blank_needed);
end;


procedure TPDF_page.write_text(dots_x, dots_y: Integer; Text: String;
  shift: array of single; blank_needed: Boolean); overload;
const
  margin = 0.1; // 0.1mm margin on blanking box
var
  x, y: Double; // co-ordinates in mm
  w, h: Double; // width and height of text
  old_pen_color: Integer;
  old_fill_color: Integer;

begin
  // convert the dot co-ordinates to mm ...
  x := dots_to_mm(dots_x);
  y := Height - dots_to_mm(dots_y);

  // Get width & height in mm
  w := GetTextWidth(Text) / 3.7; // 3.7724;
  h := GetTextHeight(Text) / 3.7; // 3.7275;

  // Add in any required shift
  x := x + w * shift[0];
  y := y + h * shift[1];


  if blank_needed then begin
    old_pen_color := current_pen_color;
    old_fill_color := current_fill_color;
    set_pen_color(clWhite);
    set_fill_color(clWhite);
    DrawRect(x - margin, y - margin, w + 2 * margin, h + 2 * margin, 3, True, True);
    set_pen_color(old_pen_color);
    set_fill_color(old_fill_color);
  end;

  WriteText(x, y, Text);
end;

//_______________________________________________________________________________________

procedure TPDF_page.polygon(dots: array of Tpoint);
var
  points: array of TPDFcoord;
  i: integer;
begin
  setlength(points, length(dots));
  for i := 0 to length(dots) - 1 do begin
    points[i].X := dots_to_mm(dots[i].x);
    points[i].Y := Height - dots_to_mm(dots[i].y);
  end;
  inherited DrawPolygon(points, curr_pen_width);
  inherited FillStrokePath();
end;

procedure TPDF_page.set_pen_color(color: Integer);

begin
  curr_pen_color := color;
  inherited SetColor(c_to_rgb(color), True);
end;

procedure TPDF_page.set_fill_color(color: Integer);

begin
  curr_fill_color := color;
  inherited SetColor(c_to_rgb(color), False);
end;

//_______________________________________________________________________________________

function TPDF_page.current_pen_width(): Integer;
begin
  //RESULT := mm_to_dots(curr_pen_width);
  //RESULT := round(curr_pen_width);
  Result := px_to_dots(curr_pen_width);
end;

function TPDF_page.current_pen_style(): TFPPenStyle;
begin
  case curr_pen_style of
    ppsSolid:
      Result := psSolid;
    ppsDot:
      Result := psDot;
    ppsDash:
      Result := psDash;
  end;
end;

function TPDF_page.current_pen_color(): Integer;
begin
  Result := curr_pen_color;
end;

function TPDF_page.current_fill_color(): Integer;
begin
  Result := curr_fill_color;
end;

function TPDF_page.current_font_index(): Integer;
begin
  Result := curr_font_index;
end;

//_______________________________________________________________________________________

procedure TPDF_page.set_font(FontIndex: Integer; FontSize: Integer);
begin
  curr_font_index := FontIndex;
  curr_font_size := FontSize;
  SetFont(FontIndex, FontSize);
end;


//_______________________________________________________________________________________
// These functions copied from TPDFText in unit fpPDF where they are 'private'.
// Why are they not made available there? I am too polite to speculate.
//

// OK, I lied - this one is not copied, but factored out of the following functions ...
function TPDF_page.fontNum(const AFontName: string): Integer;
begin
  Result := AnsiIndexText(AFontName, ['Courier', 'Courier-Bold', 'Courier-Oblique',
    'Courier-BoldOblique', 'Helvetica', 'Helvetica-Bold', 'Helvetica-Oblique',
    'Helvetica-BoldOblique', 'Times-Roman', 'Times-Bold', 'Times-Italic',
    'Times-BoldItalic', 'Symbol', 'ZapfDingbats']);



end;

function TPDF_page.GetStdFontCharWidthsArray(const AFontName: string): TPDFFontWidthArray;
begin
  case fontNum(AFontName) of
    0:
      Result := TPDFFontWidthArray(FONT_COURIER_FULL);
    1:
      Result := TPDFFontWidthArray(FONT_COURIER_FULL);
    2:
      Result := TPDFFontWidthArray(FONT_COURIER_FULL);
    3:
      Result := TPDFFontWidthArray(FONT_COURIER_FULL);
    4:
      Result := TPDFFontWidthArray(FONT_HELVETICA_ARIAL);
    5:
      Result := TPDFFontWidthArray(FONT_HELVETICA_ARIAL_BOLD);
    6:
      Result := TPDFFontWidthArray(FONT_HELVETICA_ARIAL_ITALIC);
    7:
      Result := TPDFFontWidthArray(FONT_HELVETICA_ARIAL_BOLD_ITALIC);
    8:
      Result := TPDFFontWidthArray(FONT_TIMES);
    9:
      Result := TPDFFontWidthArray(FONT_TIMES_BOLD);
    10:
      Result := TPDFFontWidthArray(FONT_TIMES_ITALIC);
    11:
      Result := TPDFFontWidthArray(FONT_TIMES_BOLD_ITALIC);
    12:
      Result := TPDFFontWidthArray(FONT_SYMBOL);
    13:
      Result := TPDFFontWidthArray(FONT_ZAPFDINGBATS);
    //else
    //  raise EPDF.CreateFmt(rsErrUnknownStdFont, [AFontName]);
  end;
end;

function TPDF_page.GetTextWidth(Text: String): single;
var
  i: integer;
  lWidth: double;
  lFontName: string;
begin
  //lFontName := Document.Fonts[Font.FontIndex].Name;
  lFontName := Document.Fonts[current_font_index].Name;
  //if not Document.IsStandardPDFFont(lFontName) then
  //  raise EPDF.CreateFmt(rsErrUnknownStdFont, [lFontName]);
  lWidth := 0;
  for i := 1 to Length(Text) do
    lWidth := lWidth + GetStdFontCharWidthsArray(lFontName)[Ord(Text[i])];
  Result := lWidth * curr_font_size / 1540;
end;

function TPDF_page.GetTextHeight(Text: String): single;
var
  lFontName: string;
begin
  //lFontName := Document.Fonts[Font.FontIndex].Name;
  lFontName := Document.Fonts[current_font_index].Name;
  Result := 0;
  case fontNum(lFontName) of
    0:
      Result := FONT_TIMES_COURIER_CAPHEIGHT;
    1:
      Result := FONT_TIMES_COURIER_CAPHEIGHT;
    2:
      Result := FONT_TIMES_COURIER_CAPHEIGHT;
    3:
      Result := FONT_TIMES_COURIER_CAPHEIGHT;
    4:
      Result := FONT_HELVETICA_ARIAL_CAPHEIGHT;
    5:
      Result := FONT_HELVETICA_ARIAL_BOLD_CAPHEIGHT;
    6:
      Result := FONT_HELVETICA_ARIAL_ITALIC_CAPHEIGHT;
    7:
      Result := FONT_HELVETICA_ARIAL_BOLD_ITALIC_CAPHEIGHT;
    8:
      Result := FONT_TIMES_CAPHEIGHT;
    9:
      Result := FONT_TIMES_BOLD_CAPHEIGHT;
    10:
      Result := FONT_TIMES_ITALIC_CAPHEIGHT;
    11:
      Result := FONT_TIMES_BOLD_ITALIC_CAPHEIGHT;
    12:
      Result := 300;
    13:
      Result := 300;
    //else
    //  raise EPDF.CreateFmt(rsErrUnknownStdFont, [AFontName]);
  end;
  Result := Result * curr_font_size / 1540;
end;

//_______________________________________________________________________________________


end.
