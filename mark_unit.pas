unit mark_unit;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  // Enumeration of the possible values for Mark codes

  // NOTE:
  // 1. These names contain the values of the codes. This is a temporary step to help avoid
  //    mistakes as I deploy the enumeration into the code. Later, the names can be refactored
  //    to remove the values from them.
  // 2. Also later, the values themselves can be changed

  // !!! BEWARE !!!
  // The next change here is to allow the values to be derived by the compiler.
  // This work will need to take account of the fact that there is a dependency
  // in many places on the order of the values, namely 'case' and 'if' statements
  // which test for ranges of values.

  EMarkCode = (

    //-493  chair block insert location (DXF)
    eMC__493_DXFblock = -493,         // -493
    eMC__5_Label = -5,                // -5
    eMC__4_TimberSelector,            // -4
    eMC__3_CurvingRadiusCentre_2,     // -3
    eMC__2_CurvingRadiusCentre_1,     // -2
    eMC__1_PegCentre,                 // -1

    eMC_0_Ignore = 0,                 // 0

    eMC_1_GuideMark,                  // 1
    eMC_2_RadialEnd,                  // 2
    eMC_3_TimberOutline,              // 3
    eMC_4_TimberCL,                   // 4
    eMC_5_TimberReducedEnd,           // 5
    eMC_6_RailJoint,                  // 6
    eMC_7_TransitionAndSlewing,       // 7
    eMC_8_PegArm_1,                   // 8
    eMC_9_PegArm_2,                   // 9
    eMC_10_PlainTrackStart,           // 10
    eMC_11_placeholder,               // 11 - placeholder

    // It seems wrong that most mark codes represent what TYPE of thing is being
    // drawn, but the following few also contain aspects of HOW it is drawn.
    // This should be derived at the point of drawing, as for the other codes
    // TODO: Untangle these two ideas. Move 'pen' selection to the point of drawing.
    eMC_14_TimberCLMidline = 14,      // 14 - 'midline' = 'rails as midlines' selected
    eMC_33_SelectedTimberOutline = 33, // 33
    eMC_44_ShovingTimberCL = 44,      // 44 - shov'ing' implies shoved OR selected
    eMC_54_ShovingTimberCLMidline = 54,// 54 - shov'ing' implies shoved OR selected

    eMC_55_ReducedEnd = 55,           // 55

    eMC_93_ShovedTimberOutline = 93,  // 93
    eMC_95_ReducedEndInfill = 95,     // 95
    eMC_98_placeholder = 98,          // 98 - placeholder
    eMC_99_TimberNumber,              // 99

    eMC_100_placeholder,              // 100 - placeholder
    eMC_101_SwitchDrive,              // 101
    eMC_199_placeholder = 199,        // 199 - placeholder

    eMC_200_placeholder,              // 200 - placeholder
    eMC_203_TimberInfill = 203,       // 203
    eMC_233_ShovedTimberInfill = 233, // 233 - Shoved but not selected
    eMC_293_ShovedTimberInfill = 293, // 293 - Shoved but not selected

    eMC_480_ChairStart = 480,         // 480
    eMC_493_Chair = 493,              // 493
    eMC_499_ChairEnd = 499,           // 499

    // code 501..508 = check rail labels
    eMC_501_MSWorkingEnd = 501,       // 501
    eMC_502_MSExtensionEnd,           // 502
    eMC_503_MSWingRail,               // 503
    eMC_504_TSWorkingEnd,             // 504
    eMC_505_TSExtensionEnd,           // 505
    eMC_506_TSWingRail,               // 506
    eMC_507_MSKCheckRail,             // 507
    eMC_508_DSWingRail,               // 508

    eMC_600_LongMark = 600,           // 600
    eMC_601_TipsLabel,                // 601
    eMC_602_SetLabel,                 // 602
    eMC_603_PlaningLabel,             // 603
    eMC_604_StockGaugeLabel,          // 604
    eMC_605_JoggleLabel,              // 605
    eMC_605_SWitchLabelEnd,           // 605
    eMC_607_placeholder = 607,        // 607

    eMC_700_XingLongMark = 700,       // 700
    eMC_701_XingFPLabel,              // 701
    eMC_702_XingBluntNoseLabel,       // 702
    eMC_703_XingTipsLabel,            // 703
    eMC_703_XingLabelEnd              // 703

    );


  Tmark = record                     // mark from p1 to p2.
    p1: TPoint;
    p2: TPoint;
    code: EMarkCode;
  end;

  Tmark_array = array of Tmark;


implementation

end.



