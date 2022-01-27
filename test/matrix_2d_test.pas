unit matrix_2d_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  point_ex,
  matrix_2d;

type
  Ttest_matrix_2d = class(TTestCase)
  private
    m: Tmatrix_2d;

  protected
    procedure TearDown; override;

  published
    procedure test_zero_default;
    procedure test_identity;
    procedure test_set_get;
    procedure test_rotate_by;
    procedure test_rotate_by_vector;
    procedure test_translate_by;
    procedure test_transform_vector;
    procedure test_transform_point;
  end;


implementation

procedure Ttest_matrix_2d.TearDown;
begin
  m.Free;
end;

procedure Ttest_matrix_2d.test_zero_default;
var
  p: Tpex;
begin
  //
  // Given a new, default matrix
  // When we get each column
  // Then the retrieved values are 0
  //

  m := Tmatrix_2d.Create;

  p := m.get_x;
  AssertEquals('X:x', 0, p.x);
  AssertEquals('X:y', 0, p.y);

  p := m.get_y;
  AssertEquals('Y:x', 0, p.x);
  AssertEquals('Y:y', 0, p.y);

  p := m.get_t;
  AssertEquals('T:x', 0, p.x);
  AssertEquals('T:y', 0, p.y);

end;

procedure Ttest_matrix_2d.test_identity;
var
  p: Tpex;
begin
  //
  // Given a new identity matrix
  // When we get each column
  // Then the expected values are 1
  //

  m := Tmatrix_2d.CreateIdentity;

  p := m.get_x;
  AssertEquals('X:x', 1, p.x);
  AssertEquals('X:y', 0, p.y);

  p := m.get_y;
  AssertEquals('Y:x', 0, p.x);
  AssertEquals('Y:y', 1, p.y);

  p := m.get_t;
  AssertEquals('T:x', 0, p.x);
  AssertEquals('T:y', 0, p.y);

end;

procedure Ttest_matrix_2d.test_set_get;
var
  p: Tpex;
  x, y, t: Tpex;
begin
  //
  // Given a matrix
  // When I set values for the 3 columns
  //  And get the values for the 3 columns
  // Then the returned values match the set values
  //
  m := Tmatrix_2d.Create;

  p.set_xy(2, 3);
  m.set_x(p);

  p.set_xy(4, 5);
  m.set_y(p);

  p.set_xy(6, 7);
  m.set_t(p);

  x := m.get_x;
  y := m.get_y;
  t := m.get_t;

  AssertEquals('X.x', 2, x.x);
  AssertEquals('X.y', 3, x.y);

  AssertEquals('Y.x', 4, y.x);
  AssertEquals('Y.y', 5, y.y);

  AssertEquals('T.x', 6, t.x);
  AssertEquals('T.y', 7, t.y);
end;


procedure Ttest_matrix_2d.test_rotate_by;
var
  x, y, t: Tpex;
begin
  //
  // Given an identity matrix
  // When rotate_by(90 degrees)
  // Then expected rotation matrix is formed
  //
  // When rotated by another 90 degrees
  // Then expected rotation matrix is formed
  //
  m := Tmatrix_2d.CreateIdentity;

  m.rotate_by(Pi / 2);

  x := m.get_x;
  y := m.get_y;
  t := m.get_t;

  AssertEquals('X.x', 0, x.x);
  AssertEquals('X.y', 1, x.y);

  AssertEquals('Y.x', -1, y.x);
  AssertEquals('Y.y', 0, y.y);

  AssertEquals('T.x', 0, t.x);
  AssertEquals('T.y', 0, t.y);

  // rotate by another 90 degrees
  m.rotate_by(Pi / 2);

  x := m.get_x;
  y := m.get_y;
  t := m.get_t;

  AssertEquals('X.x', -1, x.x);
  AssertEquals('X.y', 0, x.y);

  AssertEquals('Y.x', 0, y.x);
  AssertEquals('Y.y', -1, y.y);

  AssertEquals('T.x', 0, t.x);
  AssertEquals('T.y', 0, t.y);

end;

procedure Ttest_matrix_2d.test_rotate_by_vector;
var
  x, y, t: Tpex;
  v: Tpex;
begin
  //
  // Given an identity matrix
  // When rotate_by(vector @ 45 degrees)
  // Then expected rotation matrix is formed
  //
  // When rotated by(vector @ -45 degrees)
  // Then identity matrix is formed
  //
  m := Tmatrix_2d.CreateIdentity;

  // rotate by 45 degrees
  v := Tpex.xy(1, 1).normalise;
  m.rotate_by(v);

  x := m.get_x;
  y := m.get_y;
  t := m.get_t;

  AssertEquals('X.x', 0.707, x.x, 1e-3);
  AssertEquals('X.y', 0.707, x.y, 1e-3);

  AssertEquals('Y.x', -0.707, y.x, 1e-3);
  AssertEquals('Y.y', 0.707, y.y, 1e-3);

  AssertEquals('T.x', 0, t.x);
  AssertEquals('T.y', 0, t.y);

  // rotate by -45 degrees
  v := Tpex.xy(1, -1).normalise;
  m.rotate_by(v);

  x := m.get_x;
  y := m.get_y;
  t := m.get_t;

  AssertEquals('X.x', 1, x.x, 1e-6);
  AssertEquals('X.y', 0, x.y, 1e-6);

  AssertEquals('Y.x', 0, y.x, 1e-6);
  AssertEquals('Y.y', 1, y.y, 1e-6);

  AssertEquals('T.x', 0, t.x);
  AssertEquals('T.y', 0, t.y);

end;

procedure Ttest_matrix_2d.test_translate_by;
var
  p: Tpex;
  x, y, t: Tpex;
begin
  //
  // Given an identity matrix
  // When translate_by(some vector)
  // Then expected translation matrix is formed
  //
  // When translated by another vector
  // Then expected translation matrix is formed
  //
  m := Tmatrix_2d.CreateIdentity;

  p.set_xy(2, 3);
  m.translate_by(p);

  x := m.get_x;
  y := m.get_y;
  t := m.get_t;

  AssertEquals('X.x', 1, x.x);
  AssertEquals('X.y', 0, x.y);

  AssertEquals('Y.x', 0, y.x);
  AssertEquals('Y.y', 1, y.y);

  AssertEquals('T.x', 2, t.x);
  AssertEquals('T.y', 3, t.y);

  // translate by another vector
  p.set_xy(-1, 4);
  m.translate_by(p);

  x := m.get_x;
  y := m.get_y;
  t := m.get_t;

  AssertEquals('X.x', 1, x.x);
  AssertEquals('X.y', 0, x.y);

  AssertEquals('Y.x', 0, y.x);
  AssertEquals('Y.y', 1, y.y);

  AssertEquals('T.x', 1, t.x);
  AssertEquals('T.y', 7, t.y);

end;

procedure Ttest_matrix_2d.test_transform_point;
var
  p: Tpex;
begin
  //
  // Given a matrix with a combined rotation and translation
  // When a point is transformed
  // Then the resulting point is rotated and translated
  //
  m := Tmatrix_2d.CreateIdentity;

  m.rotate_by(Pi / 4); // 45 degrees
  m.translate_by(10, 0);

  p.set_xy(1, 0);

  p := m.transform_point(p);

  AssertEquals('p.x', 10 + 1 / sqrt(2), p.x);
  AssertEquals('p.y', 1 / sqrt(2), p.y);
end;

procedure Ttest_matrix_2d.test_transform_vector;
var
  p: Tpex;
begin
  //
  // Given a matrix with a combined rotation and translation
  // When a vector is transformed
  // Then the resulting vector is only rotated
  //
  m := Tmatrix_2d.CreateIdentity;

  m.rotate_by(Pi / 4); // 45 degrees
  m.translate_by(10, 0);

  p.set_xy(1, 0);

  p := m.transform_vector(p);

  AssertEquals('p.x', 1 / sqrt(2), p.x);
  AssertEquals('p.y', 1 / sqrt(2), p.y);
end;

initialization

  RegisterTest(Ttest_matrix_2d);
end.







