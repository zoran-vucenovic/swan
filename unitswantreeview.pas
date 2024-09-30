unit UnitSwanTreeView;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, ComCtrls, fpjson;

type

  TComponentArray = array of TComponent;

  TSwanTreeNode = class(TTreeNode)
  end;

  TSwanTreeView = class (TTreeView)
  protected
    procedure DoCreateNodeClass(var NewNodeClass: TTreeNodeClass); override;
  public
    constructor Create(AnOwner: TComponent); override;

    procedure MakeAllNodesVisible;
    function LoadComponentsFromJSonArray(const JArr: TJSONArray; out AArr: TComponentArray): Boolean;
  end;

implementation

{ TSwanTreeView }

procedure TSwanTreeView.DoCreateNodeClass(var NewNodeClass: TTreeNodeClass);
begin
  //inherited DoCreateNodeClass(NewNodeClass);
  NewNodeClass := TSwanTreeNode;
end;

constructor TSwanTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  //
  Options := Options
    + [TTreeViewOption.tvoReadOnly, TTreeViewOption.tvoAutoItemHeight]
    - [TTreeViewOption.tvoAutoExpand, TTreeViewOption.tvoAllowMultiselect];
end;

procedure TSwanTreeView.MakeAllNodesVisible;
var
  Nd: TTreeNode;
begin
  Nd := Items.GetFirstNode;
  while Assigned(Nd) do begin
    Nd.Visible := True;
    Nd := Nd.GetNext;
  end;
end;

function TSwanTreeView.LoadComponentsFromJSonArray(const JArr: TJSONArray; out
  AArr: TComponentArray): Boolean;
var
  JD: TJSONData;
  I: Integer;
  J: Integer;
  S: RawByteString;
  Nd: TTreeNode;
  Cm: TComponent;

begin
  Result := False;
  if Assigned(JArr) then begin
    J := 0;

    SetLength(AArr{%H-}, JArr.Count);
    for I := 0 to JArr.Count - 1 do begin
      S := '';
      JD := JArr.Items[I];
      if JD is TJSONString then begin
        S := JD.AsString;

        Nd := Items.GetFirstNode;
        while Assigned(Nd) do begin
          if TObject(Nd.Data) is TComponent then begin
            Cm := TComponent(Nd.Data);
            if (Cm.Name <> '') and (AnsiCompareText(Cm.Name, S) = 0) then begin
              AArr[J] := Cm;
              Inc(J);

              Break;
            end;
          end;
          Nd := Nd.GetNext;
        end;
      end;
    end;
    SetLength(AArr, J);
    Result := J > 0;
  end;
end;

end.

