unit rail_data_unit;

{$mode delphi}

interface

uses
  Classes, SysUtils, point_ex;

const
  aq_max_c = 48;                     // currently 49 rail-lines 0-48

type
  ERailData = (
    eRD_StraightStockGaugeFace, // 0
    eRD_StraightTurnoutWingGaugeFace,             // 1
    eRD_CurvedTurnoutWingGaugeFace,                  // 2
    eRD_CurvedStockGaugeFace,                                // 3

    eRD_VeePointGaugeFace, // 4
    eRD_VeeSpliceGaugeFace, // 5
    eRD_MainSideCheckGaugeFace, // 6
    eRD_TurnoutSideCheckGaugeFace, // 7

    eRD_StraightStockOuterFace, // 8
    eRD_StraightTurnoutWingOuterFace, // 9
    eRD_CurvedTurnoutWingOuterFace, // 10
    eRD_CurvedStockOuterFace, // 11

    eRD_VeePointOuterFace, // 12
    eRD_VeeSpliceOuterFace, // 13
    eRD_MainSideCheckOuterFace, // 14
    eRD_TurnoutSideCheckOuterFace, // 15

    eRD_AdjTrackTurnoutSideNearGaugeFace, // 16
    eRD_AdjTrackTurnoutSideNearOuterFace, // 17
    eRD_AdjTrackTurnoutSideFarGaugeFace, // 18
    eRD_AdjTrackTurnoutSideFarOuterFace, // 19

    eRD_AdjTrackMainSideNearGaugeFace, // 20
    eRD_AdjTrackMainSideNearOuterFace, // 21
    eRD_AdjTrackMainSideFarGaugeFace, // 22
    eRD_AdjTrackMainSideFarOuterFace, // 23

    eRD_MainRoadCentreLine, // 24
    eRD_TurnoutRoadCentreLine, // 25

    eRD_KCrossingCheckMainSideGaugeFace, // 26
    eRD_KCrossingCheckMainSideOuterEdge, // 27
    eRD_KCrossingCheckTurnoutSideGaugeFace, // 28
    eRD_KCrossingCheckTurnoutSideOuterEdge, // 29

    eRD_StraightStockFootInnerEdge, // 30
    eRD_StraightTurnoutWingFootInnerEdge, // 31
    eRD_CurvedTurnoutWingFootInnerEdge, // 32
    eRD_CurvedStockFootInnerEdge, // 33

    eRD_VeePointFootInnerEdge, // 34
    eRD_VeeSpliceFootInnerEdge, // 35
    eRD_MainSideCheckFootInnerEdge, // 36
    eRD_TurnoutSideCheckFootInnerEdge, // 37

    eRD_StraightStockFootOuterEdge, // 38
    eRD_StraightTurnoutWindFootOuterEdge, // 39
    eRD_CurvedTurnoutWindFootOuterEdge, // 40
    eRD_CurvedStockFootOuterEdge, // 41

    eRD_VeePointFootOuterEdge, // 42
    eRD_VeeSpliceFootOuterEdge, // 43
    eRD_MainSideCheckFootOuterEdge, // 44
    eRD_TurnoutSideCheckFootOuterEdge, // 45

    eRD_SwitchFlangewayGuidelineMainSide, // 46
    eRD_SwitchFlangewayGuidelineTurnoutSide, // 47
    eRD_Unused // 48
    );

  TPoint_array = array of TPoint;    // array of Windows TPoints (integers)

const
  eRD_StockRailGaugeFaces = [eRD_StraightStockGaugeFace, eRD_CurvedStockGaugeFace];
  eRD_StockRailOuterFaces = [eRD_StraightStockOuterFace, eRD_CurvedStockOuterFace];

  eRD_StockRails = eRD_StockRailGaugeFaces + eRD_StockRailOuterFaces;

  eRD_AdjacentTracksGaugeFaces = [eRD_AdjTrackTurnoutSideNearGaugeFace, eRD_AdjTrackTurnoutSideFarGaugeFace, eRD_AdjTrackMainSideNearGaugeFace, eRD_AdjTrackMainSideFarGaugeFace];
  eRD_AdjacentTracksOuterFaces = [eRD_AdjTrackTurnoutSideNearOuterFace, eRD_AdjTrackTurnoutSideFarOuterFace, eRD_AdjTrackMainSideNearOuterFace, eRD_AdjTrackMainSideFarOuterFace];
  eRD_AdjacentTracks = eRD_AdjacentTracksGaugeFaces + eRD_AdjacentTracksOuterFaces;


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

  aq_str[eRD_StraightStockGaugeFace] := 'straight stock rail, gauge-face'; // 0
  aq_str[eRD_StraightTurnoutWingGaugeFace] := 'straight turnout rail - wing rail, gauge-face';
  // 1
  aq_str[eRD_CurvedTurnoutWingGaugeFace] := 'curved turnout rail - wing rail, gauge-face';
  // 2
  aq_str[eRD_CurvedStockGaugeFace] := 'curved stock rail, gauge-face';
  // 3

  aq_str[eRD_VeePointGaugeFace] := 'vee point rail, gauge-face'; // 4
  aq_str[eRD_VeeSpliceGaugeFace] := 'vee splice rail, gauge-face'; // 5
  aq_str[eRD_MainSideCheckGaugeFace] := 'main-side check rail, gauge-face'; // 6
  aq_str[eRD_TurnoutSideCheckGaugeFace] := 'turnout-side check rail, gauge-face'; // 7

  aq_str[eRD_StraightStockOuterFace] := 'straight stock rail, outer-face'; // 8
  aq_str[eRD_StraightTurnoutWingOuterFace] := 'straight turnout rail - wing rail, outer-face'; // 9
  aq_str[eRD_CurvedTurnoutWingOuterFace] := 'curved turnout rail - wing rail, outer-face'; // 10
  aq_str[eRD_CurvedStockOuterFace] := 'curved stock rail, outer-face'; // 11

  aq_str[eRD_VeePointOuterFace] := 'vee point rail, outer-face'; // 12
  aq_str[eRD_VeeSpliceOuterFace] := 'vee splice rail, outer-face'; // 13
  aq_str[eRD_MainSideCheckOuterFace] := 'main-side check rail, outer-face'; // 14
  aq_str[eRD_TurnoutSideCheckOuterFace] := 'turnout-side check rail, outer-face'; // 15

  aq_str[eRD_AdjTrackTurnoutSideNearGaugeFace] :=
    'adjacent track (turnout side) near rail, gauge-face'; // 16
  aq_str[eRD_AdjTrackTurnoutSideNearOuterFace] :=
    'adjacent track (turnout side) near rail, outer-face'; // 17
  aq_str[eRD_AdjTrackTurnoutSideFarGaugeFace] :=
    'adjacent track (turnout side) far rail, gauge-face'; // 18
  aq_str[eRD_AdjTrackTurnoutSideFarOuterFace] :=
    'adjacent track (turnout side) far rail, outer-face'; // 19

  aq_str[eRD_AdjTrackMainSideNearGaugeFace] :=
    'adjacent track (main side) near rail, gauge-face'; // 20
  aq_str[eRD_AdjTrackMainSideNearOuterFace] :=
    'adjacent track (main side) near rail, outer-face'; // 21
  aq_str[eRD_AdjTrackMainSideFarGaugeFace] :=
    'adjacent track (main side) far rail, gauge-face'; // 22
  aq_str[eRD_AdjTrackMainSideFarOuterFace] :=
    'adjacent track (main side) far rail, outer-face'; // 23

  aq_str[eRD_MainRoadCentreLine] := 'main road centre-line'; // 24
  aq_str[eRD_TurnoutRoadCentreLine] := 'turnout road centre-line'; // 25

  aq_str[eRD_KCrossingCheckMainSideGaugeFace] :=
    'K-crossing check rail, main-side, gauge-face'; // 26
  aq_str[eRD_KCrossingCheckMainSideOuterEdge] :=
    'K-crossing check rail, main-side, outer-edge'; // 27
  aq_str[eRD_KCrossingCheckTurnoutSideGaugeFace] :=
    'K-crossing check rail, turnout-side, gauge-face'; // 28
  aq_str[eRD_KCrossingCheckTurnoutSideOuterEdge] :=
    'K-crossing check rail, turnout-side, outer-edge'; // 29

  aq_str[eRD_StraightStockFootInnerEdge] := 'straight stock rail, foot, inner edge'; // 30
  aq_str[eRD_StraightTurnoutWingFootInnerEdge] :=
    'straight turnout rail - wing rail, foot, inner edge'; // 31
  aq_str[eRD_CurvedTurnoutWingFootInnerEdge] :=
    'curved turnout rail - wing rail, foot, inner edge'; // 32
  aq_str[eRD_CurvedStockFootInnerEdge] := 'curved stock rail, foot, inner edge'; // 33

  aq_str[eRD_VeePointFootInnerEdge] := 'vee point rail, foot, inner edge'; // 34
  aq_str[eRD_VeeSpliceFootInnerEdge] := 'vee splice rail, foot, inner edge'; // 35
  aq_str[eRD_MainSideCheckFootInnerEdge] := 'main-side check rail, foot, inner edge'; // 36
  aq_str[eRD_TurnoutSideCheckFootInnerEdge] := 'turnout-side check rail, foot, inner edge'; // 37

  aq_str[eRD_StraightStockFootOuterEdge] := 'straight stock rail, foot, outer edge'; // 38
  aq_str[eRD_StraightTurnoutWindFootOuterEdge] :=
    'straight turnout rail - wing rail, foot, outer edge'; // 39
  aq_str[eRD_CurvedTurnoutWindFootOuterEdge] :=
    'curved turnout rail - wing rail, foot, outer edge'; // 40
  aq_str[eRD_CurvedStockFootOuterEdge] := 'curved stock rail, foot, outer edge'; // 41

  aq_str[eRD_VeePointFootOuterEdge] := 'vee point rail, foot, outer edge'; // 42
  aq_str[eRD_VeeSpliceFootOuterEdge] := 'vee splice rail, foot, outer edge'; // 43
  aq_str[eRD_MainSideCheckFootOuterEdge] := 'main-side check rail, foot, outer edge'; // 44
  aq_str[eRD_TurnoutSideCheckFootOuterEdge] := 'turnout-side check rail, foot, outer edge'; // 45

  aq_str[eRD_SwitchFlangewayGuidelineMainSide] := 'switch flangeway guide-line, main-side'; // 46
  aq_str[eRD_SwitchFlangewayGuidelineTurnoutSide] :=
    'switch flangeway guide-line, turnout-side'; // 47
  aq_str[eRD_Unused] := 'unused aq'; // 48

  aq_oppositeFace[eRD_StraightStockGaugeFace] := eRD_StraightStockOuterFace;
  aq_oppositeFace[eRD_StraightTurnoutWingGaugeFace] := eRD_StraightTurnoutWingOuterFace;
  aq_oppositeFace[eRD_CurvedTurnoutWingGaugeFace] := eRD_CurvedTurnoutWingOuterFace;
  aq_oppositeFace[eRD_CurvedStockGaugeFace] := eRD_CurvedStockOuterFace;
  aq_oppositeFace[eRD_VeePointGaugeFace] := eRD_VeePointOuterFace;
  aq_oppositeFace[eRD_VeeSpliceGaugeFace] := eRD_VeeSpliceOuterFace;
  aq_oppositeFace[eRD_MainSideCheckGaugeFace] := eRD_MainSideCheckOuterFace;
  aq_oppositeFace[eRD_TurnoutSideCheckGaugeFace] := eRD_TurnoutSideCheckOuterFace;
  aq_oppositeFace[eRD_StraightStockOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_StraightTurnoutWingOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_CurvedTurnoutWingOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_CurvedStockOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_VeePointOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_VeeSpliceOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_MainSideCheckOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_TurnoutSideCheckOuterFace] := eRD_Unused;

  aq_oppositeFace[eRD_AdjTrackTurnoutSideNearGaugeFace] := eRD_AdjTrackTurnoutSideNearOuterFace;
  aq_oppositeFace[eRD_AdjTrackTurnoutSideNearOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_AdjTrackTurnoutSideFarGaugeFace] := eRD_AdjTrackTurnoutSideFarOuterFace;
  aq_oppositeFace[eRD_AdjTrackTurnoutSideFarOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_AdjTrackMainSideNearGaugeFace] := eRD_AdjTrackMainSideNearOuterFace;
  aq_oppositeFace[eRD_AdjTrackMainSideNearOuterFace] := eRD_Unused;
  aq_oppositeFace[eRD_AdjTrackMainSideFarGaugeFace] := eRD_AdjTrackMainSideFarOuterFace;
  aq_oppositeFace[eRD_AdjTrackMainSideFarOuterFace] := eRD_Unused;

  aq_oppositeFace[eRD_MainRoadCentreLine] := eRD_Unused;
  aq_oppositeFace[eRD_TurnoutRoadCentreLine] := eRD_Unused;

  aq_oppositeFace[eRD_KCrossingCheckMainSideGaugeFace] := eRD_KCrossingCheckMainSideOuterEdge;
  aq_oppositeFace[eRD_KCrossingCheckMainSideOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_KCrossingCheckTurnoutSideGaugeFace] := eRD_KCrossingCheckTurnoutSideOuterEdge;
  aq_oppositeFace[eRD_KCrossingCheckTurnoutSideOuterEdge] := eRD_Unused;

  aq_oppositeFace[eRD_StraightStockFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_StraightTurnoutWingFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_CurvedTurnoutWingFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_CurvedStockFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_VeePointFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_VeeSpliceFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_MainSideCheckFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_TurnoutSideCheckFootInnerEdge] := eRD_Unused;
  aq_oppositeFace[eRD_StraightStockFootOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_StraightTurnoutWindFootOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_CurvedTurnoutWindFootOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_CurvedStockFootOuterEdge] := eRD_Unused;

  aq_oppositeFace[eRD_VeePointFootOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_VeeSpliceFootOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_MainSideCheckFootOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_TurnoutSideCheckFootOuterEdge] := eRD_Unused;
  aq_oppositeFace[eRD_SwitchFlangewayGuidelineMainSide] := eRD_Unused;
  aq_oppositeFace[eRD_SwitchFlangewayGuidelineTurnoutSide] := eRD_Unused;

end.
