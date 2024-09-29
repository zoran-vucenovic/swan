unit UnitSwanTreeView;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, ComCtrls;

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

end.

