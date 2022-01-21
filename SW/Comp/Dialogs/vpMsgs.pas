unit vpMsgs;

interface

Uses
  Classes, Controls, MMesOk, MMesYN, MMesYNC;

Procedure VpMesOk ( ASender : TComponent; Const AText : string );
Function VpMesYesNo ( ASender : TComponent; Const AText1, AText2 : string ) : boolean;
Function VpMesYesNo ( ASender : TComponent; Const AHeader, AText1, AText2 : string ) : boolean;
Function MesYNC ( ASender : TComponent; APoint : TPoint; Const AText1 : string ) : byte;

implementation

Procedure VpMesOk ( ASender : TComponent; Const AText : string );
Var
  BMesOk        : TMesOkForm;

Begin
 BMesOk:=TMesOkForm.Create(ASender);
 BMesOk.FormStyle:=fsStayOnTop;
 BMesOk.SetHeader('Error');
 BMesOk.SetText(AText);
 BMesOk.ShowModal;
 BMesOk.Free;
End;

Function VpMesYesNo ( ASender : TComponent; Const AText1, AText2 : string ) : boolean;
Var
  BMesYesNo        : TMesYesNoForm;

Begin
 BMesYesNo:=TMesYesNoForm.Create(ASender);
 BMesYesNo.FormStyle:=fsStayOnTop;
 BMesYesNo.SetHeader('Error');
 BMesYesNo.SetText(AText1,AText2);
 BMesYesNo.ShowModal;
 Result:=BMesYesNo.Result;
 BMesYesNo.Free;
End;

Function VpMesYesNo ( ASender : TComponent; Const AHeader, AText1, AText2 : string ) : boolean;
Var
  BMesYesNo        : TMesYesNoForm;

Begin
 BMesYesNo:=TMesYesNoForm.Create(ASender);
 BMesYesNo.FormStyle:=fsStayOnTop;
 BMesYesNo.SetHeader(AHeader);
 BMesYesNo.SetText(AText1,AText2);
 BMesYesNo.ShowModal;
 Result:=BMesYesNo.Result;
 BMesYesNo.Free;
End;

Function MesYNC ( ASender : TComponent; APoint : TPoint; Const AText1 : string ) : byte;
Var
  BMesYNC        : TMesYNCForm;

Begin
 BMesYNC:=TMesYNCForm.Create(ASender);
 BMesYNC.FormStyle:=fsStayOnTop;
 BMesYNC.SetHeader('Error');
 BMesYNC.SetText(AText1,'');
 BMesYNC.Top:=APoint.y;
 BMesYNC.Left:=APoint.x;
 BMesYNC.ShowModal;
 Result:=BMesYNC.Result;
 BMesYNC.Free;
End;

end.
 
