unit UnitKeyMaps;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, LCLType;

type
  TKeyTexts = array [0..3] of AnsiString;

function DecodePCKey(AKey: Word): AnsiString;
function DecodeSpectrumKey(AKey: Word): AnsiString;
// returns the other text (commands or symbols) on a spectrum key
function DecodeSpectrumKeyTexts(AKey: Word; out KeyTexts: TKeyTexts): AnsiString;

implementation

function DecodePCKey(AKey: Word): AnsiString;
const
  cWinKey =
    {$if defined(unix) and defined(darwin)}
      'Command'
    {$else}
      'Windows'
    {$endif}
     + ' key';
begin
  Result := '';
  if AKey >= $FF then
    Exit;

  case AKey of
    VK_LBUTTON..VK_XBUTTON2:
      begin
        case AKey of
          VK_LBUTTON:
            Result := 'Left';
          VK_RBUTTON:
            Result := 'Rigth';
          VK_MBUTTON:
            Result := 'Middle';
          VK_XBUTTON1:
            Result := 'X1';
          VK_XBUTTON2:
            Result := 'X2';
        otherwise
          Exit;
        end;
        Result := Result + ' mouse button';
      end;
    VK_BACK:
      Result := 'Backspace';
    VK_TAB:
      Result := 'Tab';
    VK_CLEAR:
      Result := 'Clear (numpad 5 without numlock)';
    VK_RETURN:
      Result := 'Enter';
    VK_SHIFT:
      Result := 'Shift';
    VK_CONTROL:
      Result := 'Ctrl';
    VK_MENU:
      Result := 'Alt';
    VK_PAUSE:
      Result := 'Pause/Break';
    VK_CAPITAL:
      Result := 'Caps lock';
    VK_KANA:
      Result := 'IME Kana mode';
    $16:
      Result := 'IME on';
    VK_JUNJA:
      Result := 'IME Junja';
    VK_FINAL:
      Result := 'IME final';
    VK_KANJI:
      Result := 'IME Kanji';
    $1A:
      Result := 'IME off';
    VK_ESCAPE:
      Result := 'Esc';
    VK_CONVERT:
      Result := 'IME convert';
    VK_NONCONVERT:
      Result := 'IME nonconvert';
    VK_ACCEPT:
      Result := 'IME accept';
    VK_MODECHANGE:
      Result := 'IME mode change request';
    VK_SPACE:
      Result := 'Space';
    VK_PRIOR:
      Result := 'Page up';
    VK_NEXT:
      Result := 'Page down';
    VK_END:
      Result := 'End';
    VK_HOME:
      Result := 'Home';
    VK_LEFT:
      Result := 'Left arrow';
    VK_UP:
      Result := 'Up arrow';
    VK_RIGHT:
      Result := 'Right arrow';
    VK_DOWN:
      Result := 'Down arrow';
    VK_SELECT:
      Result := 'Select';
    VK_PRINT:
      Result := 'Print key';
    VK_EXECUTE:
      Result := 'Execute';
    VK_SNAPSHOT:
      Result := 'Print screen';
    VK_INSERT:
      Result := 'Insert';
    VK_DELETE:
      Result := 'Delete';
    VK_HELP:
      Result := 'Help';
    VK_0..VK_9, VK_A..VK_Z:
      Result := Chr(AKey);

    VK_LWIN:
      Result := 'Left ' + cWinKey;
    VK_RWIN:
      Result := 'Right ' + cWinKey;
    VK_APPS:
      Result := 'Pop-up menu key';
    VK_SLEEP:
      Result := 'sleep key';
    VK_NUMPAD0..VK_NUMPAD9:
      Result := 'Numpad ' + Chr(VK_0 + AKey - VK_NUMPAD0);
    VK_MULTIPLY:
      Result := 'Numpad *';
    VK_ADD:
      Result := 'Numpad +';
    VK_SEPARATOR:
      Result := 'Numpad separator';
    VK_SUBTRACT:
      Result := 'Numpad -';
    VK_DECIMAL:
      Result := 'Numpad decimal';
    VK_DIVIDE:
      Result := 'Numpad /';
    VK_F1..VK_F24:
      Result := 'F' + IntToStr(AKey + 1 - VK_F1);
    VK_NUMLOCK:
      Result := 'Num lock';
    VK_SCROLL:
      Result := 'Scroll lock';
    VK_LSHIFT..VK_RMENU:
      begin
        case (AKey div 2) mod 3 of
          0:
            Result := 'Ctrl';
          1:
            Result := 'Alt';
        otherwise
          Result := 'Shift'
        end;

        if Akey mod 2 = 0 then
          Result := 'Left ' + Result
        else
          Result := 'Right ' + Result;
      end;
    VK_BROWSER_BACK..VK_BROWSER_HOME:
      begin
        case AKey of
          VK_BROWSER_BACK:
            Result := 'back';
          VK_BROWSER_FORWARD:
            Result := 'forward';
          VK_BROWSER_REFRESH:
            Result := 'refresh';
          VK_BROWSER_STOP:
            Result := 'stop';
          VK_BROWSER_SEARCH:
            Result := 'search';
          VK_BROWSER_FAVORITES:
            Result := 'favorites';
        otherwise //VK_BROWSER_HOME:
          Result := 'home';
        end;
        Result := 'Browser ' + Result;
      end;
    VK_VOLUME_MUTE:
      Result := 'Volume mute';
    VK_VOLUME_DOWN:
      Result := 'Volume down';
    VK_VOLUME_UP:
      Result := 'Volume up';
    VK_MEDIA_NEXT_TRACK:
      Result := 'Next track';
    VK_MEDIA_PREV_TRACK:
      Result := 'Previous track';
    VK_MEDIA_STOP:
      Result := 'Stop media';
    VK_MEDIA_PLAY_PAUSE:
      Result := 'Play/Pause';
    VK_LAUNCH_MAIL:
      Result := 'Start mail key';
    VK_LAUNCH_MEDIA_SELECT:
      Result := 'Select media key';
    VK_LAUNCH_APP1:
      Result := 'Start application 1 key';
    VK_LAUNCH_APP2:
      Result := 'Start application 2 key';
    VK_OEM_1:
      Result := 'OEM 1';
    VK_OEM_PLUS:
      Result := '"+" key';
    VK_OEM_COMMA:
      Result := '"," key';
    VK_OEM_MINUS:
      Result := '"-" key';
    VK_OEM_PERIOD:
      Result := '"." key';
    VK_OEM_2:
      Result := 'OEM 2';
    VK_OEM_3:
      Result := 'OEM 3';
    VK_OEM_4..VK_OEM_8:
      Result := 'OEM ' + Chr((AKey - VK_OEM_4) + VK_4);
    VK_OEM_102:
      Result := 'OEM 102';
    VK_PROCESSKEY:
      Result := 'IME process key';
    VK_ATTN:
      Result := 'Attn';
    VK_CRSEL:
      Result := 'CrSel';
    VK_EXSEL:
      Result := 'ExSel';
    VK_EREOF:
      Result := 'Erase EOF';
    VK_PLAY:
      Result := 'Play';
    VK_ZOOM:
      Result := 'Zoom';
    VK_PA1:
      Result := 'PA1';
    VK_OEM_CLEAR:
      Result := 'Clear key';
  otherwise
  end;
end;

function DecodeSpectrumKey(AKey: Word): AnsiString;
var
  WR: WordRec absolute AKey;
begin
  Result := '';

  case WR.Hi of
    0:
      case WR.Lo of
        0:
          Result := 'Caps shift';
        1:
          Result := 'Z';
        2:
          Result := 'X';
        3:
          Result := 'C';
        4:
          Result := 'V';
      otherwise
      end;
    1:
      case WR.Lo of
        0:
          Result := 'A';
        1:
          Result := 'S';
        2:
          Result := 'D';
        3:
          Result := 'F';
        4:
          Result := 'G';
      otherwise
      end;
    2:
      case WR.Lo of
        0:
          Result := 'Q';
        1:
          Result := 'W';
        2:
          Result := 'E';
        3:
          Result := 'R';
        4:
          Result := 'T';
      otherwise
      end;
    3:
      case WR.Lo of
        0:
          Result := '1';
        1:
          Result := '2';
        2:
          Result := '3';
        3:
          Result := '4';
        4:
          Result := '5';
      otherwise
      end;

    4:
      case WR.Lo of
        0:
          Result := '0';
        1:
          Result := '9';
        2:
          Result := '8';
        3:
          Result := '7';
        4:
          Result := '6';
      otherwise
      end;
    5:
      case WR.Lo of
        0:
          Result := 'P';
        1:
          Result := 'O';
        2:
          Result := 'I';
        3:
          Result := 'U';
        4:
          Result := 'Y';
      otherwise
      end;
    6:
      case WR.Lo of
        0:
          Result := 'Enter';
        1:
          Result := 'L';
        2:
          Result := 'K';
        3:
          Result := 'J';
        4:
          Result := 'H';
      otherwise
      end;
    7:
      case WR.Lo of
        0:
          Result := 'Break/Space';
        1:
          Result := 'Symbol shift';
        2:
          Result := 'M';
        3:
          Result := 'N';
        4:
          Result := 'B';
      otherwise
      end;
  otherwise
  end;
end;

function DecodeSpectrumKeyTexts(AKey: Word; out KeyTexts: TKeyTexts
  ): AnsiString;

  procedure AssignKeyTexts(const A: Array of String);
  var
    I: Integer;
  begin
    for I := 0 to 3 do
      KeyTexts[I] := A[I];
  end;

var
  WR: WordRec absolute AKey;
begin
  Result := DecodeSpectrumKey(AKey);
  AssignKeyTexts([' ', ' ', ' ', ' ']);

  case WR.Hi of
    0:
      case WR.Lo of
        //0:
        //  AssignKeyTexts(['', '', '', '']); // CS
        1:
          AssignKeyTexts(['COPY', ':', 'LN', 'BEEP']); // Z
        2:
          AssignKeyTexts(['CLEAR', #$c2#$a3 {pound symbol}, 'EXP', 'INK']); // X
        3:
          AssignKeyTexts(['CONT', '?', 'L PRINT', 'PAPER']); // C
        4:
          AssignKeyTexts(['CLS', '/', 'L LIST', 'FLASH']); // V
      otherwise
      end;
    1:
      case WR.Lo of
        0:
          AssignKeyTexts(['NEW', 'STOP', 'READ', '~']); // A
        1:
          AssignKeyTexts(['SAVE', 'NOT', 'RESTORE', '|']); // S
        2:
          AssignKeyTexts(['DIM', 'STEP', 'DATA', '\']); // D
        3:
          AssignKeyTexts(['FOR', 'TO', 'SGN', '{']); // F
        4:
          AssignKeyTexts(['GOTO', 'THEN', 'ABS', '}']); // G
      otherwise
      end;
    2:
      case WR.Lo of
        0:
          AssignKeyTexts(['PLOT', '<=', 'SIN', 'ASN']); // Q
        1:
          AssignKeyTexts(['DRAW', '<>', 'COS', 'ACS']); // W
        2:
          AssignKeyTexts(['REM', '>=', 'TAN', 'ATN']); // E
        3:
          AssignKeyTexts(['RUN', '<', 'INT', 'VERIFY']); // R
        4:
          AssignKeyTexts(['RAND', '>', 'RND', 'MERGE']); // T
      otherwise
      end;
    3:
  //  #$e2#$96#$96 - quadrant lower left
  //  #$e2#$96#$9e - quadrant upper right and lower left
  //  #$e2#$96#$8c - left half block
  //  #$e2#$96#$9b - quadrant upper left and upper right and lower left
  //  #$e2#$96#$84 - lower half block
  //  #$e2#$96#$9f - quadrant upper right and lower left and lower right
  //  #$e2#$96#$99 - quadrant upper left and lower left and lower right
  //  #$e2#$96#$88 - full block
      case WR.Lo of
        0:
          AssignKeyTexts([#$e2#$96#$99, '!', 'EDIT', 'DEF FN']); // 1
        1:
          AssignKeyTexts([#$e2#$96#$9f, '@', 'CAPS LOCK', 'FN']); // 2
        2:
          AssignKeyTexts([#$e2#$96#$84, '#', 'TRUE VIDEO', 'LINE']); // 3
        3:
          AssignKeyTexts([#$e2#$96#$9b, '$', 'INV. VIDEO', 'OPEN #']); // 4
        4:
          AssignKeyTexts([#$e2#$96#$8c, '%', #$e2#$87#$90 {left double arrow}, 'CLOSE #']); // 5
      otherwise
      end;

    4:
      case WR.Lo of
        0:
          //AssignKeyTexts([' ', '_', 'DELETE', 'FORMAT']); // 0
          AssignKeyTexts([' ', '__', 'DELETE', 'FORMAT']); // 0
        1:
          AssignKeyTexts([' ', ')', 'GRAPHICS', 'CAT']); // 9
        2:
          AssignKeyTexts([#$e2#$96#$88, '(', #$e2#$87#$92 {right double arrow}, 'POINT']); // 8
        3:
          AssignKeyTexts([#$e2#$96#$96, '´', #$e2#$87#$91 {up double arrow}, 'ERASE']); // 7
        4:
          AssignKeyTexts([#$e2#$96#$9e, '&', #$e2#$87#$93 {down double arrow}, 'MOVE']); // 6
      otherwise
      end;
    5:
      case WR.Lo of
        0:
          AssignKeyTexts(['PRINT', '"', 'TAB', #$c2#$a9 {copyright symbol}]); // P
        1:
          AssignKeyTexts(['POKE', ';', 'PEEK', 'OUT']); // O
        2:
          AssignKeyTexts(['INPUT', 'AT', 'CODE', 'IN']); // I
        3:
          AssignKeyTexts(['IF', 'OR', 'CHR $', ']']); // U
        4:
          AssignKeyTexts(['RETURN', 'AND', 'STR $', '[']); // Y
      otherwise
      end;
    6:
      case WR.Lo of
        //0:
        //  AssignKeyTexts(['', '', '', '']); // Enter
        1:
          AssignKeyTexts(['LET', '=', 'USR', 'ATTR']); // L
        2:
          AssignKeyTexts(['LIST', '+', 'LEN', 'SCREEN $']); // K
        3:
          AssignKeyTexts(['LOAD', '-', 'VAL', 'VAL $']); // J
        4:
          AssignKeyTexts(['GOSUB', #$e2#$86#$91 {up arrow symbol}, 'SQR', 'CIRCLE']); // H
      otherwise
      end;
    7:
      case WR.Lo of
        //0:
        //  AssignKeyTexts(['', '', '', '']); // Break/Space
        //1:
        //  AssignKeyTexts(['', '', '', '']); // Symbol shift
        2:
          AssignKeyTexts(['PAUSE', '.', 'PI', 'INVERSE']); // M
        3:
          AssignKeyTexts(['NEXT', ',', 'IN KEY $', 'OVER']); // N
        4:
          AssignKeyTexts(['BORDER', '*', 'BIN', 'BRIGHT']); // B
      otherwise
      end;

  otherwise
  end;
end;

end.

