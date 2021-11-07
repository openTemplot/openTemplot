unit fresnel_unit;

{$mode delphi}{$H+}

(*-------------------------------------------------------------------------
  This code has been extracted from DAMath library by Wolfgang Ehrhardt

  The original code can be found at:
  https://github.com/CMCHTPC/ChromaPrint

---------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------
 (C) Copyright 2013-2015 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

interface

uses
  Classes, SysUtils;

procedure Fresnel(x: double; out s, c: double);
{-Return the Fresnel integrals S(x)=integral(sin(Pi/2*t^2),t=0..x) and C(x)=integral(cos(Pi/2*t^2),t=0..x)}

function FresnelC(x: double): double;
{-Return the Fresnel integral C(x)=integral(cos(Pi/2*t^2),t=0..x)}

function FresnelS(x: double): double;
{-Return the Fresnel integral S(x)=integral(sin(Pi/2*t^2),t=0..x)}


implementation

type
  THexDblW = packed array[0..3] of word;  {Double   as array of word}

  TDblRec  = packed record     {Double as sign, exponent, significand}
               lm: longint;    {low  32 bit of significand}
               hm: longint;    {high bits of significand, biased exponent and sign}
             end;

const
  RTE_NoConvergence: integer = 234;   {RTE for no convergence, set negative}
                                      {to continue (with inaccurate result)}


const
  NaNDHex: THexDblW = ($ffff, $ffff, $ffff, $7fff); {a double NaN as hex}

const
  Sqrt_MinDH: THexDblW = ($0000, $0000, $0000, $2000); {sqrt(MinDouble) as Hex}

const
  Pi_2hex: THexDblW = ($2D18, $5444, $21FB, $3FF9); {Pi/2}


var
  NaN_d: double absolute NaNDHex;     {a double NaN}
  Sqrt_MinDbl: double absolute Sqrt_MinDH;  {= 1.49166814624004E-0154} {=0.5^511}
  Pi_2: double absolute Pi_2hex;     {= 1.5707963267948966192 }

  {---------------------------------------------------------------------------}
  function IsNaND(d: double): boolean;   {$ifdef HAS_INLINE} inline;{$endif}
    {-Return true if d is a NaN}
  begin
    with TDblRec(d) do begin
      IsNaND := (hm and $7FF00000=$7FF00000) and ((hm and $000FFFFF<>0) or (lm<>0));
    end;
  end;

{---------------------------------------------------------------------------}
function floord(x: double): double;
  {-Return the largest integer <= x}
var
  t: double;
begin
  t := int(x);
  if (x >= 0.0) or (x = t) then
    floord := t
  else
    floord := t - 1.0;
end;


{---------------------------------------------------------------------------}
function rem_int2(x: double; out z: double): integer;
  {-Argument reduction of x: z*Pi = x*Pi - n*Pi/2, |z|<=1/4, result = n mod 8.}
  { Used for argument reduction in sin(Pi*x) and cos(Pi*x)}
var
  y: double;
  i: integer;
  w: word;
begin
  w := THexDblW(x)[3] and $7FF0;
  if (w = $7FF0) or (abs(x) <= 0.25) then begin
    {Nan, Inf, or <= 1/4}
    rem_int2 := 0;
    z := x;
    exit;
  end;
  if frac(x) = 0.0 then begin
    {Here x is an integer or abs(x) >= 2^52}
    z := 0.0;
    i := 0;
    {set i=2, if x is a odd}
    if (w < $4340) and (frac(0.5 * x) <> 0.0) then
      i := 2;
  end
  else begin
    {Here x is not an integer. First calculate x mod 2,}
    {this leaves Pi*x = Pi*(x mod 2) mod 2*Pi invariant}
    x := 0.5 * x;
    x := 2.0 * (x - floord(x));
    {then apply the Cody/Waite style range reduction}
    y := floord(4.0 * x);
    i := trunc(y - 16.0 * floord(y / 16.0));
    if odd(i) then begin
      Inc(i);
      y := y + 1.0;
    end;
    i := (i shr 1) and 7;
    z := x - 0.25 * y;
  end;
  rem_int2 := i;
end;


{---------------------------------------------------------------------------}
procedure sincosPi(x: double; var s, c: double);
{-Return s=sin(Pi*x), c=cos(Pi*x); (s,c)=(0,1) for abs(x) >= 2^52}
var
  t, ss, cc: double;
  n: integer;
begin
  if THexDblW(x)[3] and $7FF0 = $7FF0 then begin
    {Inf or NaN}
    s := Nan_D;
    c := Nan_D;
    exit;
  end;
  n := rem_int2(x, t) and 3;
  t := Pi * t;
  ss := system.sin(t);
  cc := system.cos(t);
  case n of
    0: begin
      s := ss;
      c := cc;
    end;
    1: begin
      s := cc;
      c := -ss;
    end;
    2: begin
      s := -ss;
      c := -cc;
    end;
    else begin
      s := -cc;
      c := ss;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure sincosPix2(x: double; out s, c: double);
{-Return s=sin(Pi/2*x^2), c=cos(Pi/2*x^2); (s,c)=(0,1) for abs(x) >= 2^53}
var
  n, f, g: double;
begin
  {Note that this routine is very sensible to argument changes!     }

  {Demo of sincosPix2 sensibility to argument changes               }
  {Computing sin(Pi/2*x^2) with sincosPix2 and MPArith for x=10000.1}
  {mp_float default bit precision = 240,  decimal precision = 72.2  }
  {Machine eps for double = 2.22044604925E-0016                     }
  {d = x(double)      = +10000.100000000000364                      }
  {x - d              = -3.6379788070917129517E-13                  }
  {rel. diff          = -3.6379424276674362773E-17                  }
  {a = sincosPix2     =  1.57073287395724723E-0002                  }
  {b = sin(Pi/2*x^2)  = +1.5707317311820675753E-2                   }
  {c = sin(Pi/2*d^2)  = +1.5707328739572472151E-2                   }
  {(c - b)            = +1.1427751796397653663E-8                   }
  {(c - b)/b          = +7.2754319337507758102E-7                   }
  {(c - a)            = -1.2497147346576779516E-19                  }
  {(c - a)/b          = -7.9562582829926944803E-18                  }

  if THexDblW(x)[3] and $7FF0 >= $4340 then begin
    {abs(x) >= 2^53}
    s := 0.0;
    c := 1.0;
  end
  else begin
    {c, s depend on frac(x) and int(x) mod 4. This code is based on  }
    {W. Van Snyder: Remark on algorithm 723: Fresnel integrals, 1993.}
    {http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.7180}
    {Fortran source from http://netlib.org/toms/723}
    f := abs(x);
    n := int(f);
    f := f - n;
    g := 2.0 * frac(f * int(0.5 * n));
    if frac(0.5 * n) = 0.0 then
      sincosPi(0.5 * f * f + g, s, c)
    else begin
      sincosPi((0.5 * f * f + f) + g, c, s);
      c := -c;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure fresnel_cfrac(x: double; var fs, fc: double);
{-Continued fraction for Fresnel functions, x>=1.5}
const
  maxit2 = 2 * 200;
  eps = 1e-16;
type
  TComplex = record
    r, i: double;
  end;
var
  a, p, q: double;
  b, c, d, h, del: TComplex;
  n: longint;
begin
  {Ref: Numerical Recipes [13], ch. 6.9, p.256, function frenel}

  {Evaluate the continued fraction for the complex erfc function using}
  {the modified Lentz method. Complex arithmetic is done inline.}

  {b = complex(1.0,-pix2)}
  b.r := 1.0;
  b.i := -Pi * x * x;

  {c = complex(1.0/fpmin,0.0)}
  c.r := 1 / Sqrt_MinDbl;
  c.i := 0.0;

  {d = h = cdiv(one,b)}
  p := b.r / b.i;
  q := b.i + p * b.r;
  d.r := p / q;
  d.i := -1.0 / q;
  h.r := d.r;
  h.i := d.i;
  n := -1;

  repeat
    Inc(n, 2);
    a := -n * (n + 1);

    {b = cadd(b,complex(4.0,0.0))}
    b.r := b.r + 4.0;

    {d = cdiv(one,cadd(rcmul(a,d),b))}
    d.r := d.r * a + b.r;
    d.i := d.i * a + b.i;
    if abs(d.r) >= abs(d.i) then begin
      p := d.i / d.r;
      q := d.r + p * d.i;
      d.r := 1.0 / q;
      d.i := -p / q;
    end
    else begin
      p := d.r / d.i;
      q := d.i + p * d.r;
      d.r := p / q;
      d.i := -1.0 / q;
    end;

    {c = cadd(b,cdiv(complex(a,0.0),c))}
    if abs(c.r) >= abs(c.i) then begin
      p := c.i / c.r;
      q := c.r + p * c.i;
      c.r := b.r + a / q;
      c.i := b.i - p * a / q;
    end
    else begin
      p := c.r / c.i;
      q := c.i + p * c.r;
      c.r := b.r + a * p / q;
      c.i := b.i - a / q;
    end;

    {del = cmul(c,d)}
    del.r := c.r * d.r - c.i * d.i;
    del.i := c.i * d.r + c.r * d.i;

    {h = cmul(h,del)}
    q := h.r * del.r - h.i * del.i;
    h.i := h.i * del.r + h.r * del.i;
    h.r := q;
  until (abs(del.r - 1.0) + abs(del.i) < eps) or (n > maxit2);

  {No convergence of CF}
  if (n > maxit2) and (RTE_NoConvergence > 0) then
    RunError(byte(RTE_NoConvergence));

  {d = cmul(complex(ax,-ax),h)}
  d.r := x * (h.r + h.i);
  d.i := x * (h.i - h.r);

  {h = csub(1,cmul(complex(cos(0.5*pix2),sin(0.5*pix2)),d))}
  sincosPix2(x, p, q);
  h.r := 1.0 - (q * d.r - p * d.i);
  h.i := -(p * d.r + q * d.i);

  {(fc,fs) = cmul(complex(0.5,0.5), h)}
  fc := 0.5 * (h.r - h.i);
  fs := 0.5 * (h.r + h.i);
end;


{---------------------------------------------------------------------------}
procedure fresnel_series(x: double; var fs, fc: double);
{-Power series for Fresnel functions, 0<=x<=1.5}
var
  fact, sign, sum, sumc, sums, term, test: double;
  k, n: longint;
const
  maxit = 50;
  eps = 1e-16;
begin
  {Ref: Numerical Recipes [13], ch. 6.9, function frenel}
  {Note: term for x=1.5, k=50 is of order (1.125*Pi)^50/50! ~ 1e-37}
  {and therefore maxit=50 is more than safe. NR parameters maxit=100}
  {together with eps=6e-8 are very very pessimistic :)}
  n := 3;
  sum := 0.0;
  sums := 0.0;
  sumc := x;
  sign := 1.0;
  fact := Pi_2 * x * x;
  term := x;
  for k := 1 to maxit do begin
    term := term * fact / k;
    sum := sum + sign * term / n;
    test := abs(sum) * eps;
    if odd(k) then begin
      sign := -sign;
      sums := sum;
      sum := sumc;
    end
    else begin
      sumc := sum;
      sum := sums;
    end;
    if term < test then begin
      fs := sums;
      fc := sumc;
      exit;
    end;
    Inc(n, 2);
  end;
  if RTE_NoConvergence > 0 then
    RunError(byte(RTE_NoConvergence));
end;


{---------------------------------------------------------------------------}
procedure fresnel_large(x: double; var fs, fc: double);
{-Asymptotic expansion for Fresnel functions, |x| >= 1.562e6}
var
  sx, cx, y: double;
begin
  (* Maple: Asymptotic expansion of S(x) and C(x) for large x:  *)
  (*                                                            *)
  (*                             2                2             *)
  (*                 cos(1/2 Pi x )   sin(1/2 Pi x )       1    *)
  (*   S(x) =  1/2 - -------------- - --------------  + O(----) *)
  (*                      Pi x              2  3            5   *)
  (*                                      Pi  x            x    *)
  (*                                                            *)
  (*                             2                2             *)
  (*                 sin(1/2 Pi x )   cos(1/2 Pi x )       1    *)
  (*   C(x) =  1/2 + -------------- - --------------  + O(----) *)
  (*                      Pi x              2  3            5   *)
  (*                                      Pi  x            x    *)
  (*                                                            *)
  x := abs(x);
  if x > 2.8671e15 then begin
    {All terms other than 1/2 are negligible}
    fs := 0.5;
    fc := 0.5;
  end
  else begin
    sincosPix2(x, sx, cx);
    y := 1.0 / (Pi * x);
    fs := 0.5 - cx * y;
    fc := 0.5 + sx * y;
  end;
end;


{---------------------------------------------------------------------------}
procedure sfd_fresnel(x: double; out s, c: double);
{-Return the Fresnel integrals S(x)=integral(sin(Pi/2*t^2),t=0..x) and C(x)=integral(cos(Pi/2*t^2),t=0..x)}
var
  ax: double;
const
  xsmall = 2.1458772928E-108;  {~ cbrt(6*succ(0)/Pi)}
  xlarge = 1.2221e5;           {~ cbrt(4/Pi^2/eps_d)}
begin
  if IsNanD(x) then begin
    s := NaN_d;
    c := NaN_d;
    exit;
  end;
  ax := abs(x);
  if ax < xsmall then begin
    {C(x) = x + (-1/40*Pi^2)*x^5 + O(x^9) }
    {S(x) = (1/6*Pi)*x^3 + (-1/336*Pi^3)*x^7 + O(x^11) }
    s := 0.0;
    c := x;
  end
  else begin
    if ax < 1.5 then
      fresnel_series(ax, s, c)
    else begin
      if ax >= xlarge then
        fresnel_large(x, s, c)
      else
        fresnel_cfrac(ax, s, c);
    end;
    if x < 0.0 then begin
      s := -s;
      c := -c;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure Fresnel(x: double; out s, c: double);
{-Return the Fresnel integrals S(x)=integral(sin(Pi/2*t^2),t=0..x) and C(x)=integral(cos(Pi/2*t^2),t=0..x)}
begin
  sfd_fresnel(x, s, c);
end;


{---------------------------------------------------------------------------}
function FresnelC(x: double): double;
  {-Return the Fresnel integral C(x)=integral(cos(Pi/2*t^2),t=0..x)}
var
  sx, cx: double;
begin
  sfd_fresnel(x, sx, cx);
  FresnelC := cx;
end;


{---------------------------------------------------------------------------}
function FresnelS(x: double): double;
  {-Return the Fresnel integral S(x)=integral(sin(Pi/2*t^2),t=0..x)}
var
  sx, cx: double;
begin
  sfd_fresnel(x, sx, cx);
  FresnelS := sx;
end;


end.
