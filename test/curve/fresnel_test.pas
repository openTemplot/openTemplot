unit fresnel_test;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  fpcunit,
  testregistry;

type
  TTestFresnel = class(TTestCase)
  published
    procedure test_fresnel;
  end;


implementation

uses
    fresnel_unit;

const eps_d: double   = 2.2204460492503131E-16;   {Hex: 000000000000B03C}


{---------------------------------------------------------------------------}
procedure testrel(nbr, neps: integer; fx, y: double; var cnt, failed: integer);
  {-Check if relative error |(fx-y)/y| is <= neps*eps_d, absolute if y=0}
var
  err: double;
begin
  err := abs(fx-y);
  if (err<>0.0) and (y<>0.0) then err := abs(err/y);
  if err > neps*eps_d then begin
    inc(failed);
    writeln('Test ', nbr:2, ' failed, rel. error = ', err/eps_d:10:3, ' eps > ',neps, ' eps ');
    writeln('      f(x) =',fx:27, ' vs. ',y:27);
  end;
  inc(cnt);
end;


{---------------------------------------------------------------------------}
procedure TTestFresnel.test_fresnel;
var
  x, yc, ys, fs, fc: double;
  cnt, failed: integer;
const
  NE = 4;
begin
  cnt := 0;
  failed := 0;
  writeln('Function: ', 'Fresnel(CS)');

  x := 0;
  fs := 0;
  fc := 0;
  Fresnel(x, ys, yc);
  testrel(1, NE, ys, fs, cnt, failed);
  testrel(2, NE, yc, fc, cnt, failed);

  x := 1e-300;
  fs := 0.5235987755982988731e-900;
  fc := 1e-300;
  ;
  Fresnel(x, ys, yc);
  testrel(3, NE, ys, fs, cnt, failed);
  testrel(4, NE, yc, fc, cnt, failed);

  x := 1e-10;
  fs := 0.5235987755982988731e-30;
  fc := 1e-10;
  Fresnel(x, ys, yc);
  testrel(5, NE, ys, fs, cnt, failed);
  testrel(6, NE, yc, fc, cnt, failed);

  x := 0.00048828125;
  fs := 0.6095491996946437605e-10;
  fc := 0.4882812499999931516e-3;
  Fresnel(x, ys, yc);
  testrel(7, NE, ys, fs, cnt, failed);
  testrel(8, NE, yc, fc, cnt, failed);

  x := -0.125;
  fs := -0.1022609856621743054e-2;
  fc := -0.1249924702994110995;
  Fresnel(x, ys, yc);
  testrel(9, NE, ys, fs, cnt, failed);
  testrel(10, NE, yc, fc, cnt, failed);

  x := 0.5;
  fs := 0.6473243285999927761e-1;
  fc := 0.4923442258714463929;
  Fresnel(x, ys, yc);
  testrel(11, NE, ys, fs, cnt, failed);
  testrel(12, NE, yc, fc, cnt, failed);

  x := 1.0;
  fs := 0.4382591473903547661;
  fc := 0.7798934003768228295;
  Fresnel(x, ys, yc);
  testrel(13, NE, ys, fs, cnt, failed);
  testrel(14, NE, yc, fc, cnt, failed);

  x := -1.5;
  fs := -0.6975049600820930131;
  fc := -0.4452611760398215351;
  Fresnel(x, ys, yc);
  testrel(15, NE, ys, fs, cnt, failed);
  testrel(16, NE, yc, fc, cnt, failed);

  x := 2.0;
  fs := 0.3434156783636982422;
  fc := 0.4882534060753407545;
  Fresnel(x, ys, yc);
  testrel(17, NE, ys, fs, cnt, failed);
  testrel(18, NE, yc, fc, cnt, failed);

  x := 10.0;
  fs := 0.4681699785848822404;
  fc := 0.4998986942055157236;
  Fresnel(x, ys, yc);
  testrel(19, NE, ys, fs, cnt, failed);
  testrel(20, NE, yc, fc, cnt, failed);

  x := -100.0;
  fs := -0.4968169011478375533;
  fc := -0.4999998986788178976;
  Fresnel(x, ys, yc);
  testrel(21, NE, ys, fs, cnt, failed);
  testrel(22, NE, yc, fc, cnt, failed);

  x := 1000.0;
  fs := 0.4996816901138163061;
  fc := 0.4999999998986788164;
  Fresnel(x, ys, yc);
  testrel(23, NE, ys, fs, cnt, failed);
  testrel(24, NE, yc, fc, cnt, failed);

  x := 5000.10009765625;
  fs := 0.4999986583294711585;
  fc := 0.5000636465631322710;
  Fresnel(x, ys, yc);
  testrel(25, NE, ys, fs, cnt, failed);
  testrel(26, NE, yc, fc, cnt, failed);

  x := 6000;
  fs := 0.4999469483523027016;
  fc := 0.4999999999995309204;
  Fresnel(x, ys, yc);
  testrel(27, NE, ys, fs, cnt, failed);
  testrel(28, NE, yc, fc, cnt, failed);

  x := 1e5;
  fs := 0.4999968169011381621;
  fc := 0.4999999999999998987;
  Fresnel(x, ys, yc);
  testrel(29, NE, ys, fs, cnt, failed);
  testrel(20, NE, yc, fc, cnt, failed);

  x := 2e6;
  fs := 0.4999998408450569081;
  fc := 0.5;
  Fresnel(x, ys, yc);
  testrel(31, NE, ys, fs, cnt, failed);
  testrel(32, NE, yc, fc, cnt, failed);

  x := 1e7;
  fs := 0.4999999681690113816;
  fc := 0.5;
  Fresnel(x, ys, yc);
  testrel(33, NE, ys, fs, cnt, failed);
  testrel(34, NE, yc, fc, cnt, failed);

  x := 2e19;
  fs := 0.5;
  fc := 0.5;
  Fresnel(x, ys, yc);
  testrel(35, NE, ys, fs, cnt, failed);
  testrel(36, NE, yc, fc, cnt, failed);


  {one test for separate functions}
  x := 0.75;
  fs := 0.2088771112333835702;
  fc := 0.6935259907871358975;
  ys := FresnelS(x);
  yc := FresnelC(x);
  testrel(37, NE, ys, fs, cnt, failed);
  testrel(48, NE, yc, fc, cnt, failed);

  if failed > 0 then
    writeln('*** failed: ', failed, ' of ', cnt)
  else
    writeln(cnt: 4, ' tests OK');

  CheckEquals(0, failed);
end;

initialization
  RegisterTest(TTestFresnel);

end.
