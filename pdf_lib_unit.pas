//  This unit provides a wrapper round fpPDF to hide the details of that library
// and provide an interface more in keeping with the Templot pdf_unit requirements.

// As an example, functions here:
//    - take coordinates in dpi and turn them into mm.
//    - take colors in BRG and turn them into RGB
//    - etc

unit pdf_lib_unit;

{$MODE Delphi}

interface

uses
  fpPDF, Classes, Dialogs, SysUtils, Types,
  preview_unit;

type

  Tpdf_page = class(TPDFPage)
    private
      curr_pen_color: Integer;
      curr_fill_color: Integer;
      function px_to_mm(pixels: Integer): double;
      function dots_to_mm_x(dots_x: Integer): double;
      function dots_to_mm_y(dots_y: Integer): double;
    public
      procedure draw_line(dots_x1, dots_y1, dots_x2, dots_y2 : Integer; thickness:Double = 1.0); overload;
      procedure draw_line(MoveTo, LineTo : TPoint; thickness:Double = 1.0); overload;
      procedure draw_open_line(dots_x1, dots_y1, dots_x2, dots_y2 : Integer; thickness:Double = 1.0); overload;
      procedure draw_open_line(MoveTo, LineTo : TPoint; thickness:Double = 1.0); overload;
      procedure write_text(dots_x, dots_y : Integer; text : String);
      procedure polygon(dots : Array of Tpoint);
      procedure set_pen_color(color: Integer);
      procedure set_fill_color(color: Integer);
      procedure set_font(FontIndex : Integer; FontSize : Integer);
      function current_pen_color(): Integer;
      function current_fill_color(): Integer;
  end;

  Tpdf_document = class(TPDFDocument)
    private
      pdf_section: TPDFSection;
    public
      constructor Create(AOwner : TComponent); override;
      function new_page(): TPDF_Page;
    end;

implementation

{$BOOLEVAL ON}

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
     RESULT := pdf_page;                      // ... then return it :-)
   end;


//=======================================================================================

function TPDF_page.px_to_mm(pixels: Integer): double;
  const
    pxpmm = 72 / 25.4;           // px per inch / mm per inch
  begin
    result := pixels / pxpmm;
end;

//_______________________________________________________________________________________

function TPDF_page.dots_to_mm_x(dots_x: Integer): double;
  const
    xscale = 1.028;
    dpmm = 600 / 25.4 / xscale;           // dots per inch / mm per inch
  begin
    result := dots_x / dpmm + page_margin_left_mm;
end;

//_______________________________________________________________________________________

function TPDF_page.dots_to_mm_y(dots_y: Integer): double;
  const
    yscale = 1.028;
    dpmm = 600 / 25.4 / yscale;           // dots per inch / mm per inch
  begin
    result := 297 - (dots_y / dpmm + page_margin_bottom_mm);
end;

//_______________________________________________________________________________________

  procedure TPDF_page.draw_line(dots_x1, dots_y1, dots_x2, dots_y2 : Integer; thickness:Double = 1.0);

  var
  x1, y1, x2, y2 : Double;

  const
    dpmm = 600 / 25.4;                 // Dots per mm

  begin
  x1 := dots_to_mm_x(dots_x1);
  y1 := dots_to_mm_y(dots_y1);
  x2 := dots_to_mm_x(dots_x2);
  y2 := dots_to_mm_y(dots_y2);
  DrawLine(x1, y1, x2, y2, 1, true);
  end;

//_______________________________________________________________________________________

  procedure TPDF_page.draw_line(MoveTo, LineTo : TPoint; thickness:Double = 1.0);
  begin
    draw_line(MoveTo.x, MoveTo.y, LineTo.x, LineTo.y, thickness);
  end;

//_______________________________________________________________________________________
//  Line without endpoints

  procedure TPDF_page.draw_open_line(dots_x1, dots_y1, dots_x2, dots_y2 : Integer; thickness:Double = 1.0);
    begin
      draw_line(dots_x1, dots_y1, dots_x2, dots_y2, thickness);
    end;

//_______________________________________________________________________________________
//  Line without endpoints

  procedure TPDF_page.draw_open_line(MoveTo, LineTo : TPoint; thickness:Double = 1.0);
  begin
    draw_open_line(MoveTo.x, MoveTo.y, LineTo.x, LineTo.y, thickness);
  end;

  //_______________________________________________________________________________________

procedure TPDF_page.write_text(dots_x, dots_y : Integer; text : String);

var
x, y : Double;

begin
  x := dots_to_mm_x(dots_x);
  y := dots_to_mm_y(dots_y);
  WriteText(x, y, text);
end;

//_______________________________________________________________________________________

procedure TPDF_page.polygon(dots : Array of Tpoint);
var
  points: array of TPDFcoord;
  i: integer;
begin
  setlength(points, length(dots));
  for i := 0 to length(dots)-1 do
    begin
      points[i].X := dots_to_mm_x(dots[i].x);
      points[i].Y := dots_to_mm_y(dots[i].y);
    end;
  inherited DrawPolygon(points, 1.0);
  inherited FillStrokePath();
end;

//_______________________________________________________________________________________

function c_to_rgb(color: Integer): Integer;
  var
   r, g, b: Integer;
  begin
    r := (color       ) and $ff;
    g := (color shr  8) and $ff;
    b := (color shr 16) and $ff;
    RESULT := (((r shl 8) or g) shl 8) or b;
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

function TPDF_page.current_pen_color(): Integer;

begin
  RESULT := curr_pen_color;
end;

function TPDF_page.current_fill_color(): Integer;

begin
  RESULT := curr_fill_color;
end;

//_______________________________________________________________________________________

procedure TPDF_page.set_font(FontIndex : Integer; FontSize : Integer);
  begin
    SetFont(FontIndex, FontSize);
  end;

//_______________________________________________________________________________________


end.

