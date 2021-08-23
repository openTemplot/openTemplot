unit dummy_vehicle_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  dummy_vehicle,
  point_ex,
  path_interface;

type

  Tmock_path = class(IPath)
  public
    function get_curve_length: double; virtual; abstract;
    function get_minimum_radius: double; virtual; abstract;
    function get_xy(dist: double): Tpex; virtual; abstract;
  end;

  Tmock_straight_line = class(Tmock_path)
  private
    m_length: double;

  public
    constructor Create(length: double);
    function get_curve_length: double; override;
    function get_minimum_radius: double; override;
    function get_xy(dist: double): Tpex; override;

  end;

  Tmock_constant_radius = class(Tmock_path)
  private
    m_length: double;
    m_radius: double;
  public
    constructor Create(length: double; radius: double);
    function get_curve_length: double; override;
    function get_minimum_radius: double; override;
    function get_xy(dist: double): Tpex; override;

  end;

  Ttest_dummy_vehicle = class(TTestCase)
  private
    dv: Tdummy_vehicle_info;
    mock_path: Tmock_path;

    function get_straight_line: IPath;
    function get_constant_radius_path: IPath;

  protected
    procedure Setup; override;
    procedure TearDown; override;

  published
    procedure test_default_values;
    procedure test_straight_line;
    procedure test_constant_radius;
  end;

implementation

const
  default_dv_start: double = 111;    // to first axle/bogie-pin from CTRL-0  111" = 9ft-3in
  default_dv_length: double = 780;     // body length 780" = 65ft
  default_dv_width: double = 111;      // body width 111" = 9ft-3in
  default_dv_clearance: double = 6;    // clearance on width 6" per vehicle per side
  default_dv_wheelbase: double = 558; // wheelbase / bogie centres 558" = 46ft-6in

  test_inscale: double = 10 / 12; // 10mm/foot
  test_line_length: double = 1000;
  test_radius: double = 1000;


constructor Tmock_straight_line.Create(length: double);
begin
  m_length := length;
end;

function Tmock_straight_line.get_curve_length: double;
begin
  Result := m_length;
end;

function Tmock_straight_line.get_minimum_radius: double;
begin
  Result := 1.0E8;                      // Max radius
end;

function Tmock_straight_line.get_xy(dist: double): Tpex;
begin
  // for straight line
  Result.x := dist;
  Result.y := 0;
end;

constructor Tmock_constant_radius.Create(length: double; radius: double);
begin
  m_length := length;
  m_radius := radius;
end;

function Tmock_constant_radius.get_curve_length: double;
begin
  Result := m_length;
end;

function Tmock_constant_radius.get_minimum_radius: double;
begin
  Result := m_radius;
end;

function Tmock_constant_radius.get_xy(dist: double): Tpex;
var
  angle: double;
begin
  angle := dist / m_radius;
  Result.x := m_radius * cos(angle);
  Result.y := m_radius * sin(angle);
end;

procedure Ttest_dummy_vehicle.Setup;
begin
  dv := Tdummy_vehicle_info.Create;
end;

procedure Ttest_dummy_vehicle.TearDown;
begin
  mock_path.Free;
  dv.Free;
end;

function Ttest_dummy_vehicle.get_straight_line: IPath;
begin
  mock_path := Tmock_straight_line.Create(test_line_length);

  Result := mock_path;
end;

function Ttest_dummy_vehicle.get_constant_radius_path: IPath;
begin
  mock_path := Tmock_constant_radius.Create(test_line_length, test_radius);
  Result := mock_path;
end;

procedure Ttest_dummy_vehicle.test_default_values;
begin
  //
  // Given a newly constructed dummy_vehicle
  // Then the dummy_vehicle has the expected default values
  //

  AssertEquals('default start', default_dv_start, dv.dv_start);
  AssertEquals('default length', default_dv_length, dv.dv_length);
  AssertEquals('default width', default_dv_width, dv.dv_width);
  AssertEquals('default clearance', default_dv_clearance, dv.dv_clearance);
  AssertEquals('default wheelbase', default_dv_wheelbase, dv.dv_wheelbase);
end;

procedure Ttest_dummy_vehicle.test_straight_line;
var
  straight_line: IPath;
  e: Tdv_envelope;
  expected_outlines: Integer;
  dve: Tdummy_vehicle_envelope;
  previous_dve: Tdummy_vehicle_envelope;
  i: Integer;
begin
  //
  // Given a default dummy_vehicle
  //   And a straight path
  //
  // When an envelope is generated
  //
  // Then the expected envelope is returned
  //

  straight_line := get_straight_line;

  e := dv.generate_envelope(test_inscale, straight_line);

  expected_outlines := Trunc((straight_line.get_curve_length + dv.dv_length * test_inscale) / (3 * test_inscale)) + 1;

  AssertNotNull('generated envelope', e);
  AssertEquals('outlines', expected_outlines, Length(e.dv_outlines));

  previous_dve := e.dv_outlines[0];
  for i := 1 to High(e.dv_outlines) do
  begin
    dve := e.dv_outlines[i];
    AssertEquals(Format('i: b1.x', [i]), previous_dve.b1.x + 3 * test_inscale, dve.b1.x);
    AssertEquals(Format('i: b1.y', [i]), -default_dv_width * test_inscale / 2, dve.b1.y);
    AssertEquals(Format('i: b2.x', [i]), dve.b1.x, dve.b2.x);
    AssertEquals(Format('i: b2.y', [i]), default_dv_width * test_inscale / 2, dve.b2.y);
    AssertEquals(Format('i: b3.x', [i]), dve.b1.x + default_dv_length * test_inscale, dve.b3.x);
    AssertEquals(Format('i: b3.y', [i]), dve.b2.y, dve.b3.y);
    AssertEquals(Format('i: b4.x', [i]), dve.b3.x, dve.b4.x);
    AssertEquals(Format('i: b4.y', [i]), dve.b1.y, dve.b4.y);

    AssertEquals(Format('i: c1.x', [i]), previous_dve.c1.x + 3 * test_inscale, dve.c1.x);
    AssertEquals(Format('i: c1.y', [i]), -(default_dv_width / 2 + default_dv_clearance) * test_inscale, dve.c1.y);
    AssertEquals(Format('i: c2.x', [i]), dve.c1.x, dve.c2.x);
    AssertEquals(Format('i: c2.y', [i]), (default_dv_width / 2 + default_dv_clearance) * test_inscale, dve.c2.y);
    AssertEquals(Format('i: c3.x', [i]), dve.c1.x + default_dv_length * test_inscale, dve.c3.x);
    AssertEquals(Format('i: c3.y', [i]), dve.c2.y, dve.c3.y);
    AssertEquals(Format('i: c4.x', [i]), dve.c3.x, dve.c4.x);
    AssertEquals(Format('i: c4.y', [i]), dve.c1.y, dve.c4.y);

    AssertEquals(Format('i: o1.x', [i]), previous_dve.o1.x + 3 * test_inscale, dve.o1.x);
    AssertEquals(Format('i: o1.y', [i]), -(default_dv_width / 2 + 33) * test_inscale, dve.o1.y);
    AssertEquals(Format('i: o2.x', [i]), dve.o1.x, dve.o2.x);
    AssertEquals(Format('i: o2.y', [i]), (default_dv_width / 2 + 33) * test_inscale, dve.o2.y);
    AssertEquals(Format('i: o3.x', [i]), dve.o1.x + default_dv_length * test_inscale, dve.o3.x);
    AssertEquals(Format('i: o3.y', [i]), dve.o2.y, dve.o3.y);
    AssertEquals(Format('i: o4.x', [i]), dve.o3.x, dve.o4.x);
    AssertEquals(Format('i: o4.y', [i]), dve.o1.y, dve.o4.y);
    previous_dve := dve;
  end;
end;

procedure Ttest_dummy_vehicle.test_constant_radius;
const
  delta: double = 0.005;
var
  path: IPath;
  e: Tdv_envelope;
  expected_outlines: Integer;
  dve: Tdummy_vehicle_envelope;
  i: Integer;
  half_wheelbase_mm: double;
  half_width_mm: double;
  half_length_mm: double;
  wheelbase_midpoint_radius: double;
  inner_mid_radius: double;
  outer_mid_radius: double;
  inner_radius: double;
  outer_radius: double;
begin
  //
  // Given a default dummy_vehicle
  //   And a constant radius path
  //
  // When an envelope is generated
  //
  // Then the expected envelope is returned
  //

  path := get_constant_radius_path;

  e := dv.generate_envelope(test_inscale, path);

  expected_outlines := Trunc((path.get_curve_length + dv.dv_length * test_inscale) / (3 * test_inscale)) + 1;

  AssertNotNull('generated envelope', e);
  AssertEquals('outlines', expected_outlines, Length(e.dv_outlines));

  // work out the actual radius of the inner and outer corners...
  half_wheelbase_mm := default_dv_wheelbase * test_inscale / 2;
  half_width_mm := default_dv_width * test_inscale / 2;
  half_length_mm := default_dv_length * test_inscale / 2;
  wheelbase_midpoint_radius := sqrt(test_radius*test_radius - half_wheelbase_mm * half_wheelbase_mm);

  inner_mid_radius := wheelbase_midpoint_radius - half_width_mm;
  outer_mid_radius := wheelbase_midpoint_radius + half_width_mm;
  inner_radius := sqrt(inner_mid_radius * inner_mid_radius + half_length_mm * half_length_mm);
  outer_radius := sqrt(outer_mid_radius * outer_mid_radius + half_length_mm * half_length_mm);

  for i := 1 to High(e.dv_outlines) do
  begin
    dve := e.dv_outlines[i];
    AssertEquals(Format('i: |b1|', [i]), outer_radius, dve.b1.magnitude, delta);
    AssertEquals(Format('i: |b2|', [i]), inner_radius, dve.b2.magnitude, delta);
    AssertEquals(Format('i: |b3|', [i]), inner_radius, dve.b3.magnitude, delta);
    AssertEquals(Format('i: |b4|', [i]), outer_radius, dve.b4.magnitude, delta);
  end;

end;

initialization

  RegisterTest(Ttest_dummy_vehicle);
end.
