
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

unit rail_data_unit;

{$mode delphi}

interface

uses
  Classes, SysUtils, point_ex;

const
  aq_max_c = 48;                     // currently 49 rail-lines 0-48

type
  ERailData = (
    rdStraightStockGaugeFace, // 0
    rdStraightTurnoutWingGaugeFace,             // 1
    rdCurvedTurnoutWingGaugeFace,                  // 2
    rdCurvedStockGaugeFace,                                // 3

    rdVeePointGaugeFace, // 4
    rdVeeSpliceGaugeFace, // 5
    rdMainSideCheckGaugeFace, // 6
    rdTurnoutSideCheckGaugeFace, // 7

    rdStraightStockOuterFace, // 8
    rdStraightTurnoutWingOuterFace, // 9
    rdCurvedTurnoutWingOuterFace, // 10
    rdCurvedStockOuterFace, // 11

    rdVeePointOuterFace, // 12
    rdVeeSpliceOuterFace, // 13
    rdMainSideCheckOuterFace, // 14
    rdTurnoutSideCheckOuterFace, // 15

    rdAdjTrackTurnoutSideNearGaugeFace, // 16
    rdAdjTrackTurnoutSideNearOuterFace, // 17
    rdAdjTrackTurnoutSideFarGaugeFace, // 18
    rdAdjTrackTurnoutSideFarOuterFace, // 19

    rdAdjTrackMainSideNearGaugeFace, // 20
    rdAdjTrackMainSideNearOuterFace, // 21
    rdAdjTrackMainSideFarGaugeFace, // 22
    rdAdjTrackMainSideFarOuterFace, // 23

    rdMainRoadCentreLine, // 24
    rdTurnoutRoadCentreLine, // 25

    rdKCrossingCheckMainSideGaugeFace, // 26
    rdKCrossingCheckMainSideOuterEdge, // 27
    rdKCrossingCheckTurnoutSideGaugeFace, // 28
    rdKCrossingCheckTurnoutSideOuterEdge, // 29

    rdStraightStockFootInnerEdge, // 30
    rdStraightTurnoutWingFootInnerEdge, // 31
    rdCurvedTurnoutWingFootInnerEdge, // 32
    rdCurvedStockFootInnerEdge, // 33

    rdVeePointFootInnerEdge, // 34
    rdVeeSpliceFootInnerEdge, // 35
    rdMainSideCheckFootInnerEdge, // 36
    rdTurnoutSideCheckFootInnerEdge, // 37

    rdStraightStockFootOuterEdge, // 38
    rdStraightTurnoutWindFootOuterEdge, // 39
    rdCurvedTurnoutWindFootOuterEdge, // 40
    rdCurvedStockFootOuterEdge, // 41

    rdVeePointFootOuterEdge, // 42
    rdVeeSpliceFootOuterEdge, // 43
    rdMainSideCheckFootOuterEdge, // 44
    rdTurnoutSideCheckFootOuterEdge, // 45

    rdSwitchFlangewayGuidelineMainSide, // 46
    rdSwitchFlangewayGuidelineTurnoutSide, // 47
    rdUnused // 48
    );

  TPoint_array = array of TPoint;    // array of Windows TPoints (integers)

const
  rdStockRailGaugeFaces = [rdStraightStockGaugeFace, rdCurvedStockGaugeFace];
  rdStockRailOuterFaces = [rdStraightStockOuterFace, rdCurvedStockOuterFace];

  rdStockRails = rdStockRailGaugeFaces + rdStockRailOuterFaces;

  rdAdjacentTracksGaugeFaces = [rdAdjTrackTurnoutSideNearGaugeFace, rdAdjTrackTurnoutSideFarGaugeFace, rdAdjTrackMainSideNearGaugeFace, rdAdjTrackMainSideFarGaugeFace];
  rdAdjacentTracksOuterFaces = [rdAdjTrackTurnoutSideNearOuterFace, rdAdjTrackTurnoutSideFarOuterFace, rdAdjTrackMainSideNearOuterFace, rdAdjTrackMainSideFarOuterFace];
  rdAdjacentTracks = rdAdjacentTracksGaugeFaces + rdAdjacentTracksOuterFaces;


var
  aq_str: array[ERailData] of string;                //  names of rail-edges.


  xy_ends: array[ERailData, 0..1] of Tpex;
  // rail edge end points  (used to mark rail ends and blunt nose). 14-4-99
  // 0=start of edge, 1=end of edge (extended, mm)

  endmarks: array[ERailData, 0..1] of TPoint;
  // rail end mark points. 1/100th mm , curved ready for drawing.
  endmarks_yn: array[ERailData, 0..1] of boolean; // flag end points exist.

  xy_max: array[0..1] of integer;
  // max x and y values in list (could be less than xy_most if drawing all negative).
  xy_min: array[0..1] of integer;
  // min x and y values in list (could be more than xy_least if drawing all positive).

  aqyn: array[ERailData] of boolean;      //  yes/no calc this aq ?

  aq_oppositeFace: array[ERailData] of ERailData;

  // mods 13-6-99. Use home-made dynamic integer arrays.

  xy_p: array[ERailData] of TPoint_array; // arrays containing rail data in 1/100 of a mm.

  nlnow_array: array[ERailData] of integer;
  //  ( aq_i )    current index into each aq array.
  nlmax_array: array[ERailData] of integer;
  //  ( aq_i )    max nlnow so far used for each aq.
  nldim_array: array[ERailData] of integer;
//  ( aq_i )    array length (max index) for each aq.


implementation

initialization

  aq_str[rdStraightStockGaugeFace] := 'straight stock rail, gauge-face'; // 0
  aq_str[rdStraightTurnoutWingGaugeFace] := 'straight turnout rail - wing rail, gauge-face';
  // 1
  aq_str[rdCurvedTurnoutWingGaugeFace] := 'curved turnout rail - wing rail, gauge-face';
  // 2
  aq_str[rdCurvedStockGaugeFace] := 'curved stock rail, gauge-face';
  // 3

  aq_str[rdVeePointGaugeFace] := 'vee point rail, gauge-face'; // 4
  aq_str[rdVeeSpliceGaugeFace] := 'vee splice rail, gauge-face'; // 5
  aq_str[rdMainSideCheckGaugeFace] := 'main-side check rail, gauge-face'; // 6
  aq_str[rdTurnoutSideCheckGaugeFace] := 'turnout-side check rail, gauge-face'; // 7

  aq_str[rdStraightStockOuterFace] := 'straight stock rail, outer-face'; // 8
  aq_str[rdStraightTurnoutWingOuterFace] := 'straight turnout rail - wing rail, outer-face'; // 9
  aq_str[rdCurvedTurnoutWingOuterFace] := 'curved turnout rail - wing rail, outer-face'; // 10
  aq_str[rdCurvedStockOuterFace] := 'curved stock rail, outer-face'; // 11

  aq_str[rdVeePointOuterFace] := 'vee point rail, outer-face'; // 12
  aq_str[rdVeeSpliceOuterFace] := 'vee splice rail, outer-face'; // 13
  aq_str[rdMainSideCheckOuterFace] := 'main-side check rail, outer-face'; // 14
  aq_str[rdTurnoutSideCheckOuterFace] := 'turnout-side check rail, outer-face'; // 15

  aq_str[rdAdjTrackTurnoutSideNearGaugeFace] :=
    'adjacent track (turnout side) near rail, gauge-face'; // 16
  aq_str[rdAdjTrackTurnoutSideNearOuterFace] :=
    'adjacent track (turnout side) near rail, outer-face'; // 17
  aq_str[rdAdjTrackTurnoutSideFarGaugeFace] :=
    'adjacent track (turnout side) far rail, gauge-face'; // 18
  aq_str[rdAdjTrackTurnoutSideFarOuterFace] :=
    'adjacent track (turnout side) far rail, outer-face'; // 19

  aq_str[rdAdjTrackMainSideNearGaugeFace] :=
    'adjacent track (main side) near rail, gauge-face'; // 20
  aq_str[rdAdjTrackMainSideNearOuterFace] :=
    'adjacent track (main side) near rail, outer-face'; // 21
  aq_str[rdAdjTrackMainSideFarGaugeFace] :=
    'adjacent track (main side) far rail, gauge-face'; // 22
  aq_str[rdAdjTrackMainSideFarOuterFace] :=
    'adjacent track (main side) far rail, outer-face'; // 23

  aq_str[rdMainRoadCentreLine] := 'main road centre-line'; // 24
  aq_str[rdTurnoutRoadCentreLine] := 'turnout road centre-line'; // 25

  aq_str[rdKCrossingCheckMainSideGaugeFace] :=
    'K-crossing check rail, main-side, gauge-face'; // 26
  aq_str[rdKCrossingCheckMainSideOuterEdge] :=
    'K-crossing check rail, main-side, outer-edge'; // 27
  aq_str[rdKCrossingCheckTurnoutSideGaugeFace] :=
    'K-crossing check rail, turnout-side, gauge-face'; // 28
  aq_str[rdKCrossingCheckTurnoutSideOuterEdge] :=
    'K-crossing check rail, turnout-side, outer-edge'; // 29

  aq_str[rdStraightStockFootInnerEdge] := 'straight stock rail, foot, inner edge'; // 30
  aq_str[rdStraightTurnoutWingFootInnerEdge] :=
    'straight turnout rail - wing rail, foot, inner edge'; // 31
  aq_str[rdCurvedTurnoutWingFootInnerEdge] :=
    'curved turnout rail - wing rail, foot, inner edge'; // 32
  aq_str[rdCurvedStockFootInnerEdge] := 'curved stock rail, foot, inner edge'; // 33

  aq_str[rdVeePointFootInnerEdge] := 'vee point rail, foot, inner edge'; // 34
  aq_str[rdVeeSpliceFootInnerEdge] := 'vee splice rail, foot, inner edge'; // 35
  aq_str[rdMainSideCheckFootInnerEdge] := 'main-side check rail, foot, inner edge'; // 36
  aq_str[rdTurnoutSideCheckFootInnerEdge] := 'turnout-side check rail, foot, inner edge'; // 37

  aq_str[rdStraightStockFootOuterEdge] := 'straight stock rail, foot, outer edge'; // 38
  aq_str[rdStraightTurnoutWindFootOuterEdge] :=
    'straight turnout rail - wing rail, foot, outer edge'; // 39
  aq_str[rdCurvedTurnoutWindFootOuterEdge] :=
    'curved turnout rail - wing rail, foot, outer edge'; // 40
  aq_str[rdCurvedStockFootOuterEdge] := 'curved stock rail, foot, outer edge'; // 41

  aq_str[rdVeePointFootOuterEdge] := 'vee point rail, foot, outer edge'; // 42
  aq_str[rdVeeSpliceFootOuterEdge] := 'vee splice rail, foot, outer edge'; // 43
  aq_str[rdMainSideCheckFootOuterEdge] := 'main-side check rail, foot, outer edge'; // 44
  aq_str[rdTurnoutSideCheckFootOuterEdge] := 'turnout-side check rail, foot, outer edge'; // 45

  aq_str[rdSwitchFlangewayGuidelineMainSide] := 'switch flangeway guide-line, main-side'; // 46
  aq_str[rdSwitchFlangewayGuidelineTurnoutSide] :=
    'switch flangeway guide-line, turnout-side'; // 47
  aq_str[rdUnused] := 'unused aq'; // 48

  aq_oppositeFace[rdStraightStockGaugeFace] := rdStraightStockOuterFace;
  aq_oppositeFace[rdStraightTurnoutWingGaugeFace] := rdStraightTurnoutWingOuterFace;
  aq_oppositeFace[rdCurvedTurnoutWingGaugeFace] := rdCurvedTurnoutWingOuterFace;
  aq_oppositeFace[rdCurvedStockGaugeFace] := rdCurvedStockOuterFace;
  aq_oppositeFace[rdVeePointGaugeFace] := rdVeePointOuterFace;
  aq_oppositeFace[rdVeeSpliceGaugeFace] := rdVeeSpliceOuterFace;
  aq_oppositeFace[rdMainSideCheckGaugeFace] := rdMainSideCheckOuterFace;
  aq_oppositeFace[rdTurnoutSideCheckGaugeFace] := rdTurnoutSideCheckOuterFace;
  aq_oppositeFace[rdStraightStockOuterFace] := rdUnused;
  aq_oppositeFace[rdStraightTurnoutWingOuterFace] := rdUnused;
  aq_oppositeFace[rdCurvedTurnoutWingOuterFace] := rdUnused;
  aq_oppositeFace[rdCurvedStockOuterFace] := rdUnused;
  aq_oppositeFace[rdVeePointOuterFace] := rdUnused;
  aq_oppositeFace[rdVeeSpliceOuterFace] := rdUnused;
  aq_oppositeFace[rdMainSideCheckOuterFace] := rdUnused;
  aq_oppositeFace[rdTurnoutSideCheckOuterFace] := rdUnused;

  aq_oppositeFace[rdAdjTrackTurnoutSideNearGaugeFace] := rdAdjTrackTurnoutSideNearOuterFace;
  aq_oppositeFace[rdAdjTrackTurnoutSideNearOuterFace] := rdUnused;
  aq_oppositeFace[rdAdjTrackTurnoutSideFarGaugeFace] := rdAdjTrackTurnoutSideFarOuterFace;
  aq_oppositeFace[rdAdjTrackTurnoutSideFarOuterFace] := rdUnused;
  aq_oppositeFace[rdAdjTrackMainSideNearGaugeFace] := rdAdjTrackMainSideNearOuterFace;
  aq_oppositeFace[rdAdjTrackMainSideNearOuterFace] := rdUnused;
  aq_oppositeFace[rdAdjTrackMainSideFarGaugeFace] := rdAdjTrackMainSideFarOuterFace;
  aq_oppositeFace[rdAdjTrackMainSideFarOuterFace] := rdUnused;

  aq_oppositeFace[rdMainRoadCentreLine] := rdUnused;
  aq_oppositeFace[rdTurnoutRoadCentreLine] := rdUnused;

  aq_oppositeFace[rdKCrossingCheckMainSideGaugeFace] := rdKCrossingCheckMainSideOuterEdge;
  aq_oppositeFace[rdKCrossingCheckMainSideOuterEdge] := rdUnused;
  aq_oppositeFace[rdKCrossingCheckTurnoutSideGaugeFace] := rdKCrossingCheckTurnoutSideOuterEdge;
  aq_oppositeFace[rdKCrossingCheckTurnoutSideOuterEdge] := rdUnused;

  aq_oppositeFace[rdStraightStockFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdStraightTurnoutWingFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdCurvedTurnoutWingFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdCurvedStockFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdVeePointFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdVeeSpliceFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdMainSideCheckFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdTurnoutSideCheckFootInnerEdge] := rdUnused;
  aq_oppositeFace[rdStraightStockFootOuterEdge] := rdUnused;
  aq_oppositeFace[rdStraightTurnoutWindFootOuterEdge] := rdUnused;
  aq_oppositeFace[rdCurvedTurnoutWindFootOuterEdge] := rdUnused;
  aq_oppositeFace[rdCurvedStockFootOuterEdge] := rdUnused;

  aq_oppositeFace[rdVeePointFootOuterEdge] := rdUnused;
  aq_oppositeFace[rdVeeSpliceFootOuterEdge] := rdUnused;
  aq_oppositeFace[rdMainSideCheckFootOuterEdge] := rdUnused;
  aq_oppositeFace[rdTurnoutSideCheckFootOuterEdge] := rdUnused;
  aq_oppositeFace[rdSwitchFlangewayGuidelineMainSide] := rdUnused;
  aq_oppositeFace[rdSwitchFlangewayGuidelineTurnoutSide] := rdUnused;

end.
