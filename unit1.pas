unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, ExtCtrls, laz.VirtualTrees, LazUTF8, IBDatabase, IBSQL, IBQuery;

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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    DBase: TIBDatabase;
    MDS: TMemDataset;
    oDlg: TOpenDialog;
    TrRead: TIBTransaction;
    VST: TLazVirtualStringTree;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    RootNodeCount:= 0;
    Header.Options:= Header.Options + [hoVisible];
    TreeOptions.AutoOptions:= TreeOptions.AutoOptions + [toAutoScroll];
    AutoScrollDelay := 100;
    TreeOptions.PaintOptions:= TreeOptions.PaintOptions +[toShowBackground];
    Header.Columns.Clear;

    for i:= 0 to Pred(length(ColumnParams)) do
    begin
      NewColumn := Header.Columns.Add;
      NewColumn.Text      := ColumnParams[i].Name;
      NewColumn.Width     := ColumnParams[i].Len;
    end;
  end;

  Button2.Visible:= False;
  Button3.Visible:= False;
end;

procedure TForm1.VSTExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
var
  i: PtrInt = 0;
  NodeData: PBioLifeRec = nil;
begin
  if (Node^.ChildCount > 0) then VST.DeleteChildren(Node);
  for i:= 0 to Pred(Pred(MDS.FieldCount)) do VST.AddChild(Node);
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
    //Finalize(NodeData);
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
        0: CellText:= 'Common name:';
        1: CellText:= MDS.Fields[2].AsString;
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
            //6: CellText:= NodeData^.graphic;
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
  if not Assigned(ParentNode) then
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
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
begin
  InitMDS(MDS);

  try
    MDS.DisableControls;

    if FileExists(DataFile)
    then MDS.LoadFromFile(DataFile)
    else
      begin
        oDlg.Filter:= 'MDS data file (*.dat)|*.dat';
        oDlg.Options:= oDlg.Options + [ofFileMustExist];
        oDlg.InitialDir:= ExtractFilePath(DataFile);
        //SetCurrentDir(oDlg.InitialDir);
        if oDlg.Execute then MDS.LoadFromFile(oDlg.FileName);
      end;
  finally
    MDS.EnableControls;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
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

