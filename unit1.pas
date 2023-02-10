unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, memds, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, ExtCtrls, laz.VirtualTrees, LazUTF8, IBDatabase, IBSQL;

const

  {$IFDEF MSWINDOWS}
  ConnStr = '127.0.0.1/31064:C:\proj\vtv_vertical_grid\base\BIOLIFE.FDB';
  LibName = 'd:\Portable_program\Firebird_server\Firebird_3_0_10_x64\fbclient.dll';
  PWDStr = 'password=cooladmin';
  Usr = 'user_name=SYSDBA';
  {$ELSE}
    {$IFDEF DARWIN}
    ConnStr = '';
    LibName = '';
    {$ELSE}
    ConnStr = '';
    LibName = '';
    {$ENDIF}
  {$ENDIF}

  DataFile = 'C:\proj\vtv_vertical_grid\base\biolife.dat';
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
    btnFetch: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    chbExpand: TCheckBox;
    chbComputeHeight: TCheckBox;
    DBase: TIBDatabase;
    MDS: TMemDataset;
    oDlg: TOpenDialog;
    TrRead: TIBTransaction;
    VST: TLazVirtualStringTree;
    procedure btnFetchClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chbComputeHeightClick(Sender: TObject);
    procedure chbExpandClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    procedure InitMDS(Sender: TMemDataset);
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
  with DBase do
  begin
    FirebirdLibraryPathName:= LibName;
    DatabaseName:= ConnStr;
    Params.Add(Usr);
    Params.Add(PWDStr);
    Params.Add('lc_ctype=UTF8');
    LoginPrompt:= False;
  end;

  with TrRead do
  begin
    Params.Add('read');
    Params.Add('read_committed');
    Params.Add('rec_version');
    Params.Add('nowait');
    DefaultDatabase:= DBase;
  end;

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
                  + [toHideTreeLinesIfThemed]
                  + [toShowVertGridLines]
                  + [toShowHorzGridLines]
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
    for i:= 0 to Pred(Pred(MDS.FieldCount)) do VST.AddChild(Node);
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
      MDS.RecNo:= Succ(Node^.Index);
      NodeData:= VST.GetNodeData(Node);
      NodeData^.species_no:= MDS.Fields[0].AsInteger;

      case Column of
        //0: CellText:= '';
        //1: CellText:= MDS.Fields[2].AsString;
        0: CellText:= MDS.Fields[2].AsString;
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
      MDS.RecNo:= Succ(Node^.Index);

      NodeData:= VST.GetNodeData(Node);
      NodeData^.species_no:= MDS.Fields[0].AsInteger;
      NodeData^.category:= MDS.Fields[1].AsString;
      NodeData^.common_name:= MDS.Fields[2].AsString;
      NodeData^.species_name:= MDS.Fields[3].AsString;
      NodeData^.length_cm:= MDS.Fields[4].AsInteger;
      NodeData^.length_in:= MDS.Fields[5].AsInteger;
      NodeData^.notes:= MDS.Fields[6].AsString;
      NodeData^.graphic:= MDS.Fields[7].AsString;
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

procedure TForm1.btnFetchClick(Sender: TObject);
var
  execSQL: TIBSQL = nil;
begin
  if not DBase.Connected then DBase.Connected:= True;
  execSQL:= TIBSQL.Create(Application);
  execSQL.Transaction:= TrRead;
  try
    try
      TrRead.StartTransaction;
      execSQL.SQL.Text:=
        'SELECT ' +
          'SPECIES_NO, ' +
          'CATEGORY, ' +
          'COMMON_NAME, ' +
          'SPECIES_NAME, ' +
          'LENGTH__CM_, ' +
          'LENGTH_IN, ' +
          'NOTES, ' +
          'GRAPHIC ' +
      'FROM BIOLIFE';
      execSQL.ExecQuery;

      InitMDS(MDS);
      VST.BeginUpdate;
      MDS.DisableControls;

      try
        while not execSQL.Eof do
        begin
          MDS.AppendRecord([
                           execSQL.Fields[0].AsInteger,
                           execSQL.Fields[1].AsString,
                           execSQL.Fields[2].AsString,
                           execSQL.Fields[3].AsString,
                           execSQL.Fields[4].AsInteger,
                           execSQL.Fields[5].AsInteger,
                           execSQL.Fields[6].AsVariant,
                           execSQL.Fields[7].AsVariant
                            ]);

          execSQL.Next;
        end;

        VST.Clear;
        VST.RootNodeCount:= MDS.RecordCount;

      finally
        MDS.EnableControls;
        VST.EndUpdate;
      end;

      TrRead.Commit;
    except
      on E:Exception do
      begin
        TrRead.Rollback;
        {$IFDEF MSWINDOWS}
        ShowMessage(WinCPToUTF8(E.Message));
        {$ELSE}
        ShowMessage(E.Message);
        {$ENDIF}
      end;
    end;
  finally
    FreeAndNil(execSQL);
  end;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
var
  Node: PVirtualNode = nil;
begin
  MDS.Clear(True);

  if FileExists(DataFile)
  then MDS.LoadFromFile(DataFile)
  else
    begin
      oDlg.Filter:= 'MDS data file (*.dat)|*.dat';
      oDlg.Options:= oDlg.Options + [ofFileMustExist];
      oDlg.InitialDir:= ExtractFilePath(DataFile);
      if oDlg.Execute then MDS.LoadFromFile(oDlg.FileName);
    end;

  MDS.Active:= True;

  Node:= VST.GetFirstSelected;
  if not Assigned(Node) then Node:= VST.GetFirst;

  VST.BeginUpdate;
  try
    VST.Clear;
    VST.RootNodeCount:= MDS.RecordCount;
  finally
    VST.EndUpdate;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  NewDataFile: String = '';
begin
  if MDS.IsEmpty then Exit;

  if FileExists(DataFile) then
    if not DeleteFile(DataFile) then
    begin
      NewDataFile:= DataFile + '_' + FormatDateTime('yyyy_mm_dd__hh_nn_ss_zzz',Now);
      ShowMessage(Format(CantDeleteDataFile,
                        [ExtractFileName(DataFile),
                        ExtractFileName(NewDataFile)]));
      MDS.SaveToFile(NewDataFile);
      Exit;
    end;
  MDS.SaveToFile(DataFile)
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

procedure TForm1.InitMDS(Sender: TMemDataset);
begin
  with TMemDataset(Sender) do
  begin
    Clear(True);
    FieldDefs.Add('SPECIES_NO',ftInteger);
    FieldDefs.Add('CATEGORY',ftString,15);
    FieldDefs.Add('COMMON_NAME',ftString,30);
    FieldDefs.Add('SPECIES_NAME',ftString,40);
    FieldDefs.Add('LENGTH__CM_',ftInteger);
    FieldDefs.Add('LENGTH_IN',ftInteger);
    FieldDefs.Add('NOTES',ftMemo);
    FieldDefs.Add('GRAPHIC',ftBlob);
    CreateTable;
    Active:= True;
    Filtered:= False;
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

