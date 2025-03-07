object Form1: TForm1
  Left = 402
  Top = 141
  Width = 809
  Height = 596
  Caption = 'TableGrid Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    793
    558)
  PixelsPerInch = 96
  TextHeight = 13
  object aStringGrid: TStringGrid
    Left = 16
    Top = 16
    Width = 756
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 7
    FixedCols = 0
    TabOrder = 0
    ColWidths = (
      73
      143
      90
      105
      138
      40
      82)
  end
  object PageControl1: TPageControl
    Left = 16
    Top = 296
    Width = 761
    Height = 241
    ActivePage = TabSheet1
    Anchors = [akLeft, akBottom]
    TabIndex = 0
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Basic Operations'
      object Button1: TButton
        Left = 32
        Top = 24
        Width = 116
        Height = 25
        Caption = 'Create TableGrid'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 160
        Top = 24
        Width = 116
        Height = 25
        Caption = 'Destroy TableGrid'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 288
        Top = 24
        Width = 116
        Height = 25
        Caption = 'Define Header'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 544
        Top = 24
        Width = 116
        Height = 25
        Caption = 'Populate'
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 416
        Top = 24
        Width = 116
        Height = 25
        Caption = 'Clear'
        TabOrder = 4
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 160
        Top = 64
        Width = 116
        Height = 25
        Caption = 'Sort by Name'
        TabOrder = 5
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 288
        Top = 64
        Width = 116
        Height = 25
        Caption = 'Sort by Sector'
        TabOrder = 6
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 416
        Top = 64
        Width = 116
        Height = 25
        Caption = 'Sort by Sector, Name'
        TabOrder = 7
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 32
        Top = 104
        Width = 116
        Height = 25
        Caption = 'Begin Update'
        TabOrder = 8
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 288
        Top = 104
        Width = 116
        Height = 25
        Caption = 'Clear Update'
        TabOrder = 9
        OnClick = Button10Click
      end
      object Button11: TButton
        Left = 416
        Top = 104
        Width = 116
        Height = 25
        Caption = 'Commit Update'
        TabOrder = 10
        OnClick = Button11Click
      end
      object Button12: TButton
        Left = 544
        Top = 104
        Width = 116
        Height = 25
        Caption = 'Rollback Update'
        TabOrder = 11
        OnClick = Button12Click
      end
      object edName: TEdit
        Left = 32
        Top = 144
        Width = 233
        Height = 21
        TabOrder = 12
      end
      object Button13: TButton
        Left = 288
        Top = 144
        Width = 116
        Height = 25
        Caption = 'Update Column Name'
        TabOrder = 13
        OnClick = Button13Click
      end
      object Button14: TButton
        Left = 416
        Top = 144
        Width = 116
        Height = 25
        Caption = 'Remove Line'
        TabOrder = 14
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 544
        Top = 64
        Width = 116
        Height = 25
        Caption = 'Fit Column to Grid'
        TabOrder = 15
        OnClick = Button15Click
      end
      object Button16: TButton
        Left = 544
        Top = 144
        Width = 116
        Height = 25
        Caption = 'Swap Columns'
        TabOrder = 16
        OnClick = Button16Click
      end
      object Button17: TButton
        Left = 160
        Top = 104
        Width = 116
        Height = 25
        Caption = 'Mass populate'
        TabOrder = 17
        OnClick = Button17Click
      end
      object chAllowSorting: TCheckBox
        Left = 32
        Top = 185
        Width = 105
        Height = 17
        Caption = 'Allow Sorting'
        Checked = True
        State = cbChecked
        TabOrder = 18
        OnClick = chAllowSortingClick
      end
      object chAllowReordering: TCheckBox
        Left = 144
        Top = 185
        Width = 145
        Height = 17
        Caption = 'Allow reordering columns'
        Checked = True
        State = cbChecked
        TabOrder = 19
        OnClick = chAllowReorderingClick
      end
      object chAllowResizing: TCheckBox
        Left = 288
        Top = 185
        Width = 129
        Height = 17
        Caption = 'Allow resizing columns'
        Checked = True
        State = cbChecked
        TabOrder = 20
        OnClick = chAllowResizingClick
      end
      object Button32: TButton
        Left = 24
        Top = 64
        Width = 125
        Height = 25
        Caption = 'Register Image Column'
        TabOrder = 21
        OnClick = Button32Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Editing'
      ImageIndex = 1
      object Button18: TButton
        Left = 24
        Top = 24
        Width = 139
        Height = 25
        Caption = 'Register Id for Editing'
        TabOrder = 0
        OnClick = Button18Click
      end
      object Button20: TButton
        Left = 176
        Top = 24
        Width = 139
        Height = 25
        Caption = 'Register Name for Editing'
        TabOrder = 1
        OnClick = Button20Click
      end
      object Button22: TButton
        Left = 328
        Top = 24
        Width = 139
        Height = 25
        Caption = 'Register Sector for Editing'
        TabOrder = 2
        OnClick = Button22Click
      end
      object Button24: TButton
        Left = 480
        Top = 24
        Width = 139
        Height = 25
        Caption = 'Register Phone for Editing'
        TabOrder = 3
        OnClick = Button24Click
      end
      object Button28: TButton
        Left = 24
        Top = 64
        Width = 139
        Height = 25
        Caption = 'Register E-Mail for Editing'
        TabOrder = 4
        OnClick = Button28Click
      end
      object Button19: TButton
        Left = 176
        Top = 64
        Width = 139
        Height = 25
        Caption = 'Register C.E. for Editing'
        TabOrder = 5
        OnClick = Button19Click
      end
      object Button21: TButton
        Left = 328
        Top = 64
        Width = 139
        Height = 25
        Caption = 'Begin Edit'
        TabOrder = 6
        OnClick = Button21Click
      end
      object Button23: TButton
        Left = 480
        Top = 64
        Width = 139
        Height = 25
        Caption = 'End Edit'
        TabOrder = 7
        OnClick = Button23Click
      end
      object Button25: TButton
        Left = 24
        Top = 104
        Width = 139
        Height = 25
        Caption = 'Insert Blank Line'
        TabOrder = 8
        OnClick = Button25Click
      end
      object Button26: TButton
        Left = 176
        Top = 104
        Width = 139
        Height = 25
        Caption = 'Check for Errors'
        TabOrder = 9
        OnClick = Button26Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'More'
      ImageIndex = 2
      object Button27: TButton
        Left = 40
        Top = 32
        Width = 89
        Height = 25
        Caption = 'Save To File'
        TabOrder = 0
        OnClick = Button27Click
      end
      object edSearchText: TEdit
        Left = 40
        Top = 80
        Width = 193
        Height = 21
        TabOrder = 1
      end
      object Button29: TButton
        Left = 248
        Top = 80
        Width = 75
        Height = 25
        Caption = 'Find Text'
        TabOrder = 2
        OnClick = Button29Click
      end
      object Button30: TButton
        Left = 152
        Top = 32
        Width = 89
        Height = 25
        Caption = 'Set Title Font'
        TabOrder = 3
        OnClick = Button30Click
      end
      object Button31: TButton
        Left = 264
        Top = 32
        Width = 89
        Height = 25
        Caption = 'Read Dataset'
        TabOrder = 4
        OnClick = Button31Click
      end
      object Button33: TButton
        Left = 368
        Top = 32
        Width = 121
        Height = 25
        Caption = 'Configure Odd Color'
        TabOrder = 5
        OnClick = Button33Click
      end
      object Button34: TButton
        Left = 504
        Top = 32
        Width = 121
        Height = 25
        Caption = 'Configure Even Color'
        TabOrder = 6
        OnClick = Button34Click
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 688
    Top = 488
  end
  object SaveDialog1: TSaveDialog
    Left = 648
    Top = 488
  end
  object SQLConnection1: TSQLConnection
    ConnectionName = 'IBLocal'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    Params.Strings = (
      'BlobSize=-1'
      'CommitRetain=False'
      
        'Database=D:\projeto\delphi\Compartilhado\Classes\Grid Tabela\cli' +
        'entlist.fdb'
      'DriverName=Interbase'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'Password=masterkey'
      'RoleName=RoleName'
      'ServerCharSet='
      'SQLDialect=1'
      'Interbase TransIsolation=ReadCommited'
      'User_Name=sysdba'
      'WaitOnLocks=True')
    VendorLib = 'GDS32.DLL'
    Left = 616
    Top = 488
  end
  object SQLQuery1: TSQLQuery
    SQLConnection = SQLConnection1
    Params = <>
    SQL.Strings = (
      'select * from client order by id')
    Left = 576
    Top = 488
    object SQLQuery1ID: TSmallintField
      FieldName = 'ID'
      Required = True
    end
    object SQLQuery1NAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object SQLQuery1ADDRESS: TStringField
      FieldName = 'ADDRESS'
      Size = 100
    end
    object SQLQuery1PHONE: TStringField
      FieldName = 'PHONE'
    end
    object SQLQuery1EMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 100
    end
    object SQLQuery1STATUS: TSmallintField
      FieldName = 'STATUS'
      OnGetText = SQLQuery1STATUSGetText
    end
    object SQLQuery1RECOMENDED: TStringField
      FieldName = 'RECOMENDED'
      OnGetText = SQLQuery1RECOMENDEDGetText
      FixedChar = True
      Size = 1
    end
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 536
    Top = 488
  end
  object ImStatus: TImageList
    Left = 696
    Top = 408
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007A9DB8001D5887007A9D
      B800F2F5F8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001D58870056A2D600387B
      AC0095B0C7000000000000000000000000000000000000000000000000000000
      0000E3E7F7009AA7E3004C62CC00364FC500344DC300465DC70095A1DE00E1E5
      F600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001D58870056A2D600458C
      BE00608AAB00000000000000000000000000000000000000000000000000BFC7
      EF004C63D1005264D4008490E70095A0EE00959FED00838EE5004C5DCE003D54
      C300B8C1E9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000407299004D97CA0056A2
      D6001D5887000000000000000000000000000000000000000000C1CAF1004760
      D5007584E300A1ACF4007F8BEC005C67E4005B66E3007D87EA009FA8F1006F7C
      DD00324BC200B9C1EA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007EA0BB003C80B10056A2
      D6001D58870000000000000000000000000000000000E7EAFA005970DE007888
      E600A3B0F5005767E7005665E6008992ED008892EC00535FE200525DE1009FA9
      F2006F7DDD004157C600E2E6F600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F955B003CA3450033B2470033B1
      470038A54700558B4C00000000000000000000000000B4C7D7002C6B9C0056A2
      D6001D58870000000000000000000000000000000000A8B4F0006073E000A4B3
      F7005A6EEB00596CEA005869E80000000000000000005562E5005461E300535F
      E2009FA9F2005061D10097A4E100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000829D4B009BDEA500A3E0AA00A3E0
      AA00ABE3B1006ACD7B0035B34C006FAA6800D3E7C100C5CC9800161D3200539E
      D4002B6A9A00C8D6E2000000000000000000000000006A82E9008E9FF0008499
      F4005C73EE005B70EC005A6EEB00909DF100A6AFF3005767E7005665E6005562
      E5007D89EB008591E7004E64CE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B49D5900AAE3B00063CE77005FCD
      740080D68E00A7E3AF00B4E7B900A3E0AB0068C5750078C77100ACB3AC0056A2
      D6003A7EB0008BA9C1000000000000000000000000005D76EA00A0B3F7006580
      F2005F78F0005D76EF005C73EE00D3D9FA0000000000596CEA005869E8005767
      E7005D6CE70099A5F1003C55CC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C4AD8500A0D3A90090DB9B006AD0
      7D006CD17E0086D994009EE0A700C5ECC700B4E6B900B0E6B7008DAB900056A2
      D6004692C70043749B00000000000000000000000000617BEE00A1B6F8006784
      F400607CF3005F7AF1005F78F00000000000000000005B70EC005A6EEB00596C
      EA005F6FE9009BA8F1004159D000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CBC2B100479A5C00A2E1AB0066CF
      7A0068CF7C0084D892009BDFA500B6E7BA00A8E3B00095DEA1009DC9A200428C
      C0007DB7DF001D588700000000000000000000000000768DF30091A6F30088A1
      F8006280F400617EF300607CF30000000000000000005D76EF005C73EE005B70
      EC008293F1008998EC005970D800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0E9E0004B965600A1E0A90064CF
      780064CF780084D8920097DEA300B0E6B700B2E6B70098DFA3008ECF96003579
      AC006EAFDB001D588700000000000000000000000000B2BFFA006C81EC00A9BD
      FB006382F5006281F5006280F40000000000000000005F7AF1005F78F0005D76
      EF00A5B5F8005D70DD00A2AFEB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FAF8F60062925500BDE9C000B8E7
      BC00B7E7BC00C1EBC300C6ECC800A8E3B000BAE8BE0099DFA400A7DC9E00356E
      97008FC1E400215F8F00E8EEF3000000000000000000EBEEFE00758CF7008397
      F000A9BDFB006382F5006382F5000000000000000000617EF300607CF300A6B9
      F9007B8DEA005C74E100E7EAFA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFD009A9A640034A54C0031A6
      4C0031A64D0030A64D0082C29600C3EBC600C9EDCB00B9E9BF00B7E7BC00A0B4
      C00086BCE2002E72A400A8BFD100000000000000000000000000CED7FD006D86
      F8008497F100A9BDFB008AA3F8006B89F6006B89F60089A2F800A8BCFA007F92
      EC005972E500C6CEF60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000035A052005EB77900BFE9C200AEE4B6006DD180004794
      5300EFF7FB003684BB00608AAB0000000000000000000000000000000000CED7
      FD00778EFA006E83EE0092A6F400A0B4F800A0B4F80091A6F300687DE9006981
      ED00C8D1F8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DFE3D9005D835C004B8E56004E994E0062905400698D
      A7001D588700608AAB00BDCFDC00000000000000000000000000000000000000
      0000EBEEFF00B5C3FD007C94FA006C86F7006A84F500778EF500B1BEF800E9EC
      FD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FF87FFFF00000000FF87F00F00000000
      FF87E00700000000FF87C00300000000FF878001000000000387818100000000
      0003800100000000000380810000000000038181000000000003818100000000
      000381810000000000018181000000000001C00300000000FC01E00700000000
      FC01F00F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
