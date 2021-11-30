unit MirLibBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TIpCmd = record
    FIp         : Cardinal;
    FCmd        : array [0..11] of byte;
    FCmdLen     : Integer;
  end;

implementation

end.

