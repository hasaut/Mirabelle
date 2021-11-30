Unit LeadAngle;

Interface

Procedure mdl_ph2C_leadAngleController ( AAngleDiff : Integer );

Implementation

Const
  CAngleVariation       = 10;
  CDegreesToRadians     = 182;
  CAnglePidDivider      = 4096;
  CAnglePidILimit       = 67108864; //16384*CAnglePidDivider;
  CAnglePidDLimit       = 2730;
  CKp_LA                = 80;
  CKi_LA                = 2;
  CKd_LA                = 5;
  CMotorRotSpeedMaxRpm  = 4500;
  CMotorRotSpeedAbsMinRpm = 150;

Var
  FAngleErrorSum        : Integer;
  FLeadAngleSetpoint    : Integer;
  FAngleErrorKeep       : Integer;

  FFailureCode          : Integer;
  FRotSpeed             : Integer;

Procedure mdl_Ph2C_MotorStop ( AFailureCode : Integer );
Begin
 FFailureCode:=AFailureCode;
End;

Procedure mdl_rsp_SetRotationalSpeedActualInRPM ( ARotSpeed : Integer );
Begin
 FRotSpeed:=ARotSpeed;
End;

Procedure mdl_ph2C_leadAngleController ( AAngleDiff : Integer );
Var
  BSpeedUpdate          : Integer;
  BAngleError,
  BAngleErrorSum,
  BAngleErrorDiff       : Integer;
  BControllerY          : Integer;
Begin
 BAngleError:=AAngleDiff-FLeadAngleSetpoint;
 if BAngleError>( CAngleVariation*CDegreesToRadians) then BAngleError:= CAngleVariation*CDegreesToRadians;
 if BAngleError<(-CAngleVariation*CDegreesToRadians) then BAngleError:=-CAngleVariation*CDegreesToRadians;

 BAngleErrorSum:=FAngleErrorSum+BAngleError;
 if BAngleErrorSum>CAnglePidILimit then BAngleErrorSum:=CAnglePidILimit
 else if BAngleErrorSum<0 then BAngleErrorSum:=0;

 BAngleErrorDiff:=BAngleError-FAngleErrorKeep;
 FAngleErrorKeep:=BAngleError;
 if BAngleErrorDiff>CAnglePidDLimit then BAngleErrorDiff:=CAnglePidDLimit
 else if BAngleErrorDiff<-CAnglePidDLimit then BAngleErrorDiff:=-CAnglePidDLimit;

 BControllerY:=BAngleError*CKp_LA+BAngleErrorSum*CKi_LA+BAngleErrorDiff*CKd_LA;
 BSpeedUpdate:=(BControllerY+(CAnglePidDivider div 2)) div CAnglePidDivider;
 if BSpeedUpdate>(CMotorRotSpeedMaxRpm+1000) then
  begin
  BSpeedUpdate:=CMotorRotSpeedMaxRpm+1000;
  mdl_Ph2C_MotorStop(0);
  end
 else if BSpeedUpdate<CMotorRotSpeedAbsMinRpm then BSpeedUpdate:=CMotorRotSpeedAbsMinRpm;

 mdl_rsp_SetRotationalSpeedActualInRPM(BSpeedUpdate);

 FAngleErrorSum:=BAngleErrorSum;
End;

end.

