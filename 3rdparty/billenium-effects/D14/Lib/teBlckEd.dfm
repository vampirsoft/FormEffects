inherited BlockTransitionEditor: TBlockTransitionEditor
  Caption = 'BlockTransitionEditor'
  PixelsPerInch = 96
  TextHeight = 14
  inherited PanelOther: TEffectsPanel
    Height = 192
    inherited PanelMsk: TEffectsPanel
      Height = 96
      Visible = True
      object LabelPuzzle: TLabel
        Left = 51
        Top = 8
        Width = 32
        Height = 14
        Alignment = taRightJustify
        Caption = 'Pu&zzle'
        FocusControl = CheckBoxPuzzle
        Transparent = True
      end
      object LabelDual: TLabel
        Left = 161
        Top = 8
        Width = 21
        Height = 14
        Caption = '&Dual'
        FocusControl = CheckBoxDual
        Transparent = True
      end
      object CheckBoxPuzzle: TCheckBox
        Left = 90
        Top = 9
        Width = 13
        Height = 13
        TabOrder = 0
        OnClick = EditMillisecondsChange
      end
      object CheckBoxDual: TCheckBox
        Left = 146
        Top = 9
        Width = 13
        Height = 13
        Caption = 'CheckBoxDual'
        TabOrder = 1
        OnClick = EditMillisecondsChange
      end
      object ComboBoxBlockWidthSel: TComboBox
        Left = 8
        Top = 37
        Width = 131
        Height = 22
        Style = csDropDownList
        DropDownCount = 10
        ItemHeight = 14
        Sorted = True
        TabOrder = 2
        OnChange = ComboBoxBlockWidthSelChange
        Items.Strings = (
          'Block width'
          'Columns')
      end
      object EditBlockWidth: TEdit
        Left = 146
        Top = 37
        Width = 57
        Height = 22
        TabOrder = 3
        Text = '0'
        OnChange = EditMillisecondsChange
      end
      object UpDownBlockWidth: TUpDown
        Left = 203
        Top = 37
        Width = 16
        Height = 22
        Associate = EditBlockWidth
        Min = 0
        Max = 30000
        Position = 0
        TabOrder = 4
        Wrap = False
      end
      object ComboBoxBlockHeightSel: TComboBox
        Left = 8
        Top = 69
        Width = 131
        Height = 22
        Style = csDropDownList
        DropDownCount = 10
        ItemHeight = 14
        Sorted = True
        TabOrder = 5
        OnChange = ComboBoxBlockHeightSelChange
        Items.Strings = (
          'Block height'
          'Rows')
      end
      object EditRows: TEdit
        Left = 146
        Top = 69
        Width = 57
        Height = 22
        TabOrder = 6
        Text = '0'
        Visible = False
        OnChange = EditMillisecondsChange
      end
      object UpDownRows: TUpDown
        Left = 203
        Top = 69
        Width = 16
        Height = 22
        Associate = EditRows
        Min = 0
        Max = 30000
        Position = 0
        TabOrder = 7
        Visible = False
        Wrap = False
      end
      object EditCols: TEdit
        Left = 146
        Top = 37
        Width = 57
        Height = 22
        TabOrder = 8
        Text = '0'
        Visible = False
        OnChange = EditMillisecondsChange
      end
      object UpDownCols: TUpDown
        Left = 203
        Top = 37
        Width = 16
        Height = 22
        Associate = EditCols
        Min = 0
        Max = 30000
        Position = 0
        TabOrder = 9
        Visible = False
        Wrap = False
      end
      object EditBlockHeight: TEdit
        Left = 146
        Top = 69
        Width = 57
        Height = 22
        TabOrder = 10
        Text = '0'
        OnChange = EditMillisecondsChange
      end
      object UpDownBlockHeight: TUpDown
        Left = 203
        Top = 69
        Width = 16
        Height = 22
        Associate = EditBlockHeight
        Min = 0
        Max = 30000
        Position = 0
        TabOrder = 11
        Wrap = False
      end
    end
  end
end
