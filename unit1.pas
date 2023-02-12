unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, BufDataset, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, laz.VirtualTrees, LazUTF8, db
  ;


const

  {$IFDEF MSWINDOWS}
    DataFile = 'C:\proj\vtv_vertical_grid\base\biolife.dat';
  {$ELSE}
    {$IFDEF UNIX}
      DataFile = '/home/leyba/laz_proj/vtv_vertical_grid/base/biolife.dat';
    {$ELSE}

    {$ENDIF}
  {$ENDIF}



  CantDeleteDataFile = 'Can''t delete old "%s" file. The new file has been saved as "%s"';
  //NoChildRecords = 'Node has no children';

type
  PBioLifeRec = ^TBioLifeRec;
  TBioLifeRec = packed record
    species_no: PtrInt;
    category: String;
    common_name: String;
    species_name: String;
    length_cm: PtrInt;
    length_in: PtrInt;
    notes: String;
    graphic: String;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnLoad: TButton;
    btnSave: TButton;
    BDS: TBufDataset;
    chbExpand: TCheckBox;
    chbComputeHeight: TCheckBox;
    oDlg: TOpenDialog;
    VST: TLazVirtualStringTree;
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chbComputeHeightClick(Sender: TObject);
    procedure chbExpandClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private

  public
    procedure InitBDS(Sender: TBufDataset);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  ColumnParams: array[0..1] of
  record
    Name: String;
    Len: integer;
   end =
  ((Name:'Field Name'     ; Len:150),
   (Name:'Field Value' ; Len:300)
  );
var
  NewColumn: TVirtualTreeColumn;
  i: Integer;
begin
  with VST do
  begin
    ShowHint:= True;
    HintMode:= hmTooltip;
    RootNodeCount:= 0;
    AutoScrollDelay := 100;
    Header.AutoSizeIndex:= 1;
    Header.Options:= Header.Options
                  + [hoVisible]
                  ;
    TreeOptions.AutoOptions:= TreeOptions.AutoOptions
                  + [toAutoScroll]
                  + [toAutoExpand]
                  + [toAutoSpanColumns]
                  ;
    TreeOptions.MiscOptions:= TreeOptions.MiscOptions
                  + [toGridExtensions]
                  //+ [toVariableNodeHeight]
                  ;
    TreeOptions.PaintOptions:= TreeOptions.PaintOptions
                  + [toShowBackground]
                  {$IFDEF LINUX}
                  //+ [toHideTreeLinesIfThemed]
                  //+ [toShowVertGridLines]
                  //+ [toShowHorzGridLines]
                  {$ELSE}
                    {$IF DEFINED(LCLqt5)}
                    //+ [toHideTreeLinesIfThemed]
                    //+ [toShowVertGridLines]
                    //+ [toShowHorzGridLines]
                    {$ELSE}
                    + [toHideTreeLinesIfThemed]
                    + [toShowVertGridLines]
                    + [toShowHorzGridLines]
                    {$ENDIF}
                  {$ENDIF}
                  ;

    TreeOptions.SelectionOptions:= TreeOptions.SelectionOptions
                  //+ [toCenterScrollIntoView]
                  ;
    Header.Columns.Clear;

    for i:= 0 to Pred(length(ColumnParams)) do
    begin
      NewColumn := Header.Columns.Add;
      NewColumn.Text      := ColumnParams[i].Name;
      NewColumn.Width     := ColumnParams[i].Len;
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  btnLoadClick(Sender);
end;

procedure TForm1.VSTAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode
  );
begin
  Caption:= IntToStr(Node^.NodeHeight);
end;

procedure TForm1.VSTExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
var
  i: PtrInt = 0;
begin
  //not allow add child nodes for current child node
  if not (vsHasChildren in Node^.States) then Exit;

  VST.BeginUpdate;
  try
    if chbExpand.Checked then VST.FullCollapse(nil);
    if (Node^.ChildCount > 0) then VST.DeleteChildren(Node);
    for i:= 0 to Pred(Pred(BDS.FieldCount)) do VST.AddChild(Node);
  finally
    VST.EndUpdate;
  end;
end;

procedure TForm1.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PBioLifeRec;
begin
  NodeData:= TBaseVirtualTree(Sender).GetNodeData(Node);

  if Assigned(NodeData) then
  begin
    NodeData^.category:= '';
    NodeData^.species_name:= '';
    NodeData^.common_name:= '';
    NodeData^.length_cm:= 0;
    NodeData^.length_in:= 0;
    NodeData^.species_no:= 0;
    NodeData^.notes:= '';
    NodeData^.graphic:= '';
    Finalize(NodeData^);
  end;
end;

procedure TForm1.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize:= SizeOf(TBioLifeRec);
end;

procedure TForm1.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
const
  FieldNameArr: array[0..6] of String = (
                  'SPECIES_NO',
                  'CATEGORY',
                  //'COMMON_NAME',
                  'SPECIES_NAME',
                  'LENGTH__CM_',
                  'LENGTH_IN',
                  'NOTES',
                  'GRAPHIC'
                );
var
  NodeData: PBioLifeRec;
begin
  if (vsHasChildren in Node^.States) then
    begin
      BDS.RecNo:= Succ(Node^.Index);
      NodeData:= VST.GetNodeData(Node);
      NodeData^.species_no:= BDS.Fields[0].AsInteger;

      case Column of
        //0: CellText:= '';
        //1: CellText:= BDS.Fields[2].AsString;
        0: CellText:= BDS.Fields[2].AsString;
        1: CellText:= '';
      end;
    end
  else
    begin
      NodeData:= VST.GetNodeData(Node^.Parent);

      case Column of
        0: CellText:= FieldNameArr[Node^.Index];
        1:
          case Node^.Index of
            0: CellText:= IntToStr(NodeData^.species_no);
            1: CellText:= NodeData^.category;
            //exclude field 'COMMON_NAME' (MDS.Fields[2])
            2: CellText:= NodeData^.species_name;
            3: CellText:= IntToStr(NodeData^.length_cm);
            4: CellText:= IntToStr(NodeData^.length_in);
            5: CellText:= NodeData^.notes;
            6: CellText:= 'coming soon ...';
          end;

      end;
    end
  ;
end;

procedure TForm1.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  NodeData: PBioLifeRec = nil;
begin
  if not Assigned(ParentNode)
  then
    begin
      InitialStates:= [ivsHasChildren];
      BDS.RecNo:= Succ(Node^.Index);

      NodeData:= VST.GetNodeData(Node);
      NodeData^.species_no:= BDS.Fields[0].AsInteger;
      NodeData^.category:= BDS.Fields[1].AsString;
      NodeData^.common_name:= BDS.Fields[2].AsString;
      NodeData^.species_name:= BDS.Fields[3].AsString;
      NodeData^.length_cm:= BDS.Fields[4].AsInteger;
      NodeData^.length_in:= BDS.Fields[5].AsInteger;
      NodeData^.notes:= BDS.Fields[6].AsString;
      NodeData^.graphic:= BDS.Fields[7].AsString;
    end
  else
    begin
      if (Node^.Index <> 5) //"NOTES" field
      then
        begin
          VST.NodeHeight[Node]:= VST.DefaultNodeHeight;
          Exclude(InitialStates, ivsMultiline);
        end
      else
        begin
          if chbComputeHeight.Checked
          then
            begin

              VST.NodeHeight[Node]:= VST.ComputeNodeHeight(VST.Canvas,Node,1) + 4;
              VST.NodeHeight[Node]:= Max(VST.DefaultNodeHeight,VST.NodeHeight[Node]);
            end
          else
            VST.NodeHeight[Node]:= 50;

          Include(InitialStates, ivsMultiline);
        end;
    end;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
var
  Node: PVirtualNode = nil;
begin
  BDS.Clear;

  if FileExists(DataFile)
  then BDS.LoadFromFile(DataFile)
  else
    begin
      oDlg.Filter:= 'BDS data file (*.dat)|*.dat';
      oDlg.Options:= oDlg.Options + [ofFileMustExist];
      oDlg.InitialDir:= ExtractFilePath(DataFile);
      if oDlg.Execute then BDS.LoadFromFile(oDlg.FileName);
    end;

  Node:= VST.GetFirstSelected;
  if not Assigned(Node) then Node:= VST.GetFirst;

  VST.BeginUpdate;
  try
    VST.Clear;
    VST.RootNodeCount:= BDS.RecordCount;
  finally
    VST.EndUpdate;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  NewDataFile: String = '';
begin
  if BDS.IsEmpty then Exit;

  if FileExists(DataFile) then
    if not DeleteFile(DataFile) then
    begin
      NewDataFile:= DataFile + '_' + FormatDateTime('yyyy_mm_dd__hh_nn_ss_zzz',Now);
      ShowMessage(Format(CantDeleteDataFile,
                        [ExtractFileName(DataFile),
                        ExtractFileName(NewDataFile)]));
      BDS.SaveToFile(NewDataFile);
      Exit;
    end;
  BDS.SaveToFile(DataFile)
end;

procedure TForm1.chbComputeHeightClick(Sender: TObject);
var
  Node: PVirtualNode = nil;
begin
  if VST.IsEmpty then Exit;

  VST.BeginUpdate;
  try
    Node:= VST.GetFirst;
    while Assigned(Node) do
    begin
      if (vsExpanded in Node^.States) then VST.ReinitNode(Node,True);
      Node:= Node^.NextSibling;
    end;
  finally
    VST.EndUpdate;
  end;
end;

procedure TForm1.chbExpandClick(Sender: TObject);
var
  Node: PVirtualNode = nil;
begin
  if VST.IsEmpty then Exit;

  VST.BeginUpdate;
  try
    Node:= VST.GetFirst(False);

    while Assigned(Node) do
    begin
      if (vsExpanded in Node^.States) then
      begin
        VST.FullCollapse(nil);
        VST.Expanded[Node]:= True;
        VST.ScrollIntoView(Node,True);
        Break;
      end;
      Node:= Node^.NextSibling;
    end;
  finally
    VST.EndUpdate;
  end;
end;

procedure TForm1.InitBDS(Sender: TBufDataset);
begin
  with TBufDataset(Sender) do
  begin
    Clear;
    FieldDefs.Add('SPECIES_NO',ftInteger);
    FieldDefs.Add('CATEGORY',ftString,15);
    FieldDefs.Add('COMMON_NAME',ftString,30);
    FieldDefs.Add('SPECIES_NAME',ftString,40);
    FieldDefs.Add('LENGTH__CM_',ftInteger);
    FieldDefs.Add('LENGTH_IN',ftInteger);
    FieldDefs.Add('NOTES',ftMemo);
    FieldDefs.Add('GRAPHIC',ftBlob);
    CreateDataset;
    Active:= True;
  end;

  //CREATE TABLE BIOLIFE (
  //    SPECIES_NO    INTEGER,
  //    CATEGORY      VARCHAR(15),
  //    COMMON_NAME   VARCHAR(30),
  //    SPECIES_NAME  VARCHAR(40),
  //    LENGTH__CM_   INTEGER,
  //    LENGTH_IN     INTEGER,
  //    NOTES         BLOB SUB_TYPE 1 SEGMENT SIZE 80,
  //    GRAPHIC       BLOB SUB_TYPE 0 SEGMENT SIZE 80
  //);
end;

end.

