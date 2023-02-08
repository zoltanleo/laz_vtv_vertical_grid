unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, ExtCtrls, laz.VirtualTrees, LazUTF8, IBDatabase, IBSQL;

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

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    DataSource1: TDataSource;
    DBase: TIBDatabase;
    DBGrid1: TDBGrid;
    MDS: TMemDataset;
    oDlg: TOpenDialog;
    Splitter1: TSplitter;
    TrRead: TIBTransaction;
    LazVirtualStringTree1: TLazVirtualStringTree;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
begin
  with DBase do
  begin
    FirebirdLibraryPathName:= LibName;
    DatabaseName:= ConnStr;
    Params.Add(Usr);
    Params.Add(PWDStr);
    Params.Add('lc_ctype=UTF8');
    LoginPrompt:= False;
    //DefaultTransaction:= TrRead;
  end;

  with TrRead do
  begin
    Params.Add('read');
    Params.Add('read_committed');
    Params.Add('rec_version');
    Params.Add('nowait');
    DefaultDatabase:= DBase;
  end;

  Button2.Visible:= False;
  Button3.Visible:= False;
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


      try
        MDS.DisableControls;
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
      finally
        MDS.EnableControls;
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

