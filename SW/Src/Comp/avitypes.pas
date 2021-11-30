unit AviTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

Const
  CAviZeroResolution    = 1E-12;

Type
  TUavCmd = (ucRuddL, ucRuddR, ucElevU, ucElevD, ucTrotP, ucTrotM, ucRstA);
  TFlyMode = (fmStop, fmManual, fmAutoPilote);

  TRbData = array [0..65535] of byte;

  TRotAxis = (raNone, raX, raY, raZ);
  TAviFloat = Single;

  TVect3s = record
    FX,
    FY,
    FZ          : TAviFloat;
  end;

  TVect3d = record
    FX,
    FY,
    FZ,
    FD          : TAviFloat;
  end;

  TVect2s = record
    FX,
    FY          : TAviFloat;
  end;

  TVect2sList = array of TVect2s;

  TVect2i = record
    FX,
    FY          : Integer;
  end;

  TVect2d = record
    FX,
    FY,
    FD          : TAviFloat;
  end;

  TRay3s = record
    FStart,
    FDir        : TVect3s;
  end;

  TPlane3s = record
    FDir        : TVect3s;
    FD          : TAviFloat;
  end;

  TBasisXZ = record
    FAxisX      : TVect3s;
    FAxisZ      : TVect3s;
  end;

  TBasisXYZ = record
    FAxisX      : TVect3s;
    FAxisY      : TVect3s;
    FAxisZ      : TVect3s;
  end;

  TAviAir = record
    FT,
    FP,
    FRH         : TAviFloat;
  end;

  TAviGeo = record
    FLat,
    FLon,
    FAlt        : TAviFloat;
  end;

  TGeoList = array of TAviGeo;

  TCoursePtr = record
    FGeo        : TAviGeo;
    FFlightAlt  : TAviFloat;
  end;

  TCourseList = array of TCoursePtr;

  TBounds2s = record
    FMinX,
    FMaxX,
    FMinY,
    FMaxY       : TAviFloat;
  end;

  TAviPoint = record
    FAir        : TAviAir;
    FGeo        : TAviGeo;
    FWind       : TVect3s;
  end;

  THdng = record
    FDir        : TAviFloat;  // Angle in degrees [0..360)
    FGrav       : TVect3s; // Acceleration of Plane in aether (normalized)
  end;

  TBaroData = record
    FDataT,
    FDataP,
    FDataH      : TAviFloat;
    FCntT,
    FCntP       : byte;
  end;

  TSensData = record
    FIdx        : word;
    FSensA      : TVect3s;
    FSensG      : TVect3s;
    FSensM      : TVect3s;
    FSensT      : TAviFloat;
    FCntA,
    FCntG,
    FCntM,
    FCntT       : byte;
    FBaroA,
    FBaroB      : TBaroData;
    FGyroGrav   : TVect3s;
    FSpdHEma    : TAviFloat;
    // New variables, add to ASM!!!
    FAltB2G     : TAviFloat;
    FRH         : TAviFloat;
  end;

  TGndData = record
    FIdx        : word;
    FMesId      : byte;
    FNavValid   : word;
    FNavType    : word;
    FUtcY       : word;
    FUtcM       : byte;
    FUtcD       : byte;
    FUtcH       : byte;
    FUtcN       : byte;
    FUtcMS      : word;
    FSatUsed    : Cardinal;
    FLat        : TAviFloat;
    FLon        : TAviFloat;
    FAltEll     : TAviFloat;
    FAltMsl     : TAviFloat;
    FSog        : TAviFloat;
    FCog        : TAviFloat;
    FClimbRate  : TAviFloat;
    FHeadRate   : TAviFloat;
    FEstPosErrH : TAviFloat;
    FEstPosErrV : TAviFloat;
  end;

  TSvData = record
    FId         : byte;
    FRfu        : byte;
    FAzim       : word;
    FElev       : word;
  end;

  TSvList = record
    FIdx        : word;
    FMesId      : byte;
    FList       : array [0..31] of TSvData;
  end;

  TTrackCh = record
    FSvId       : byte;
    FAzim,
    FElev       : byte;
    FState      : word;
    FChan       : array [1..10] of byte;
  end;

  TTrackData = record
    FIdx        : word;
    FMesId      : byte;
    FWeek       : word;
    FTow        : Cardinal;
    FChans      : byte;
    FChList     : array [0..11] of TTrackCh;
  end;

  TCalibSens = array [0..31] of TAviFloat;

  TAviServ = record
    FIdx        : word;
    FFlyMode    : TFlyMode;
    FRudd,
    FElev,
    FAilL,
    FAilR       : TAviFloat;
    FTrot       : TAviFloat;
  end;

  TAviWingA = record
    FArea       : TAviFloat;
    FCLX        : TAviFloat;
    FArmA,                   // Arm around RotAxis (OA)
    FArmB       : TAviFloat; // Arm from Point_A to a beginning of Wing
    FLen        : TAviFloat;
    FCL0,
    FCLM        : TAviFloat;
    FCDMin,
    FCDMax      : TAviFloat;
    FAngleStall : TAviFloat;
  end;

Const
  CCmdServTopi  = $08; // TowerOutPlaneIn
  CCmdServTipo  = $18; // TowerInPlaneOut (In case of Autopilote)
  CCmdCalibSagm = $10;
  CCmdCalibBaro = $11;
  CCmdSensTipo  = $12;
  CCmdGpsTipo   = $13;
  CCmdSensTipoA = $14; // Short Sens, without prediction

  ZServ : TAviServ =
    (
     FIdx:  0;
     FFlyMode: fmStop;
     FRudd: 0.0;
     FElev: 0.0;
     FAilL: 0.0;
     FAilR: 0.0;
     FTrot: 0.0
    );

  CAviHome : TAviPoint =
    (
     FAir:(FT:15.0; FP:926.00; FRH:85.0);
     FGeo:(FLat:46.87; FLon:6.74; FAlt:660.7);
     FWind:(FX:0.0; FY:0.0; FZ:0.0)
    );

  CHdngN : THdng =
    (
     FDir:0.0;
     FGrav:(FX:0.0; FY:0.0; FZ:1.0)
    );

  CHdngE : THdng =
    (
     FDir:90.0;
     FGrav:(FX:0.0; FY:0.0; FZ:1.0)
    );

  ZVect2s : TVect2s = (FX:0; FY:0);
  ZVect3s : TVect3s = (FX:0; FY:0; FZ:0);
  CAxisX : TVect3s = (FX:1; FY:0; FZ:0);
  CAxisY : TVect3s = (FX:0; FY:1; FZ:0);
  CAxisZ : TVect3s = (FX:0; FY:0; FZ:1);
  CBasisE : TBasisXYZ =
   (
    FAxisX: (FX:1; FY:0; FZ:0);
    FAxisY: (FX:0; FY:1; FZ:0);
    FAxisZ: (FX:0; FY:0; FZ:1)
   );


  CSendHdr = #$B2+#$4D;

  CAviStart     = 'AviStart';
  CPlaneHdng    = 'PlaneHdng';

Procedure IncServA ( Var AData : TAviFloat; AStep : TAviFloat );
Procedure DecServA ( Var AData : TAviFloat; AStep : TAviFloat );
Procedure IncServT ( Var AData : TAviFloat; AStep : TAviFloat );
Procedure DecServT ( Var AData : TAviFloat; AStep : TAviFloat );

implementation

Procedure IncServA ( Var AData : TAviFloat; AStep : TAviFloat );
Var
  BData : TAviFloat;
Begin
 BData:=AData+AStep;
 if BData>0.5 then BData:=0.5;
 AData:=BData;
End;

Procedure DecServA ( Var AData : TAviFloat; AStep : TAviFloat );
Var
  BData : TAviFloat;
Begin
 BData:=AData-AStep;
 if BData<-0.5 then BData:=-0.5;
 AData:=BData;
End;

Procedure IncServT ( Var AData : TAviFloat; AStep : TAviFloat );
Var
  BData : TAviFloat;
Begin
 BData:=AData+AStep;
 if BData>1.0 then BData:=1.0;
 AData:=BData;
End;

Procedure DecServT ( Var AData : TAviFloat; AStep : TAviFloat );
Var
  BData : TAviFloat;
Begin
 BData:=AData-AStep;
 if BData<0.0 then BData:=0.0;
 AData:=BData;
End;

end.

