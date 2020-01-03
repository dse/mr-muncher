PROGRAM MrMuncherGraphicsEditor;

(*****************************************************************************)

(*  MGRAPHED.PAS -- by Darren Embry -- Friday, October 25, 1991
    The graphics editor for Mr. Muncher's font characters and animated
    sprites. *)

{$I-}

{ Turbo Pascal 3.0 Only }

{$I TURBO4.P }
{$I GRAPH.P }

{ Turbo Pascal 4.0 Only }

(* Uses CRT,DOS,Graph3; *)

{$I DATATYPE.P }
{$I FILENAME.P }

VAR
  Images : IconsRecord;
  WorkCh : Char;

PROCEDURE GetAKey (VAR Ch : Char);
BEGIN
  Ch := ReadKey;
  IF Ch = #0 THEN BEGIN Ch := ReadKey; Ch := #0; END;
  Ch := UpCase (Ch);
END;

PROCEDURE LoadImages;
VAR F : File;
BEGIN
  Assign (F,IconsScreenFile);
  Reset (F,SizeOf(Images));
  IF (IOresult = 0)
    THEN BlockRead (F,Images,1)
    ELSE FillChar (Images,SizeOf(Images),0);
  Close (F);
END;

PROCEDURE SaveImages;
VAR F : File;
BEGIN
  Assign (F,IconsScreenFile);
  Rewrite (F,SizeOf(Images));
  BlockWrite (F,Images,1);
  Close (F);
END;

PROCEDURE Initialize;
BEGIN
  WorkCh := '';
END;

PROCEDURE FontEditor;
CONST
  FontEditorColumn = 26;
  FontEditorRow = 6;
  ImageX = 296;
  ImageY = 150;
VAR
  CharWidth,Row,Col : Integer;

    function gx (col : integer) : integer;
      begin gx := (fonteditorcolumn + col - 2) * 8; end;
    function gy (col : integer) : integer;
      begin gy := (fonteditorrow + row - 2) * 8; end;

    procedure widthline;
    var w : integer;
    begin
      w := charwidth;
      draw (gx(w+1)+2,gy(1)-8,gx(w+1)+2,gy(maxfontheight)+15,1);
    end;

    procedure erasewidthline;
    var col,row : integer;
    begin
      col := charwidth + 1;
      if col <= maxfontwidth then
        for row := 1 to maxfontheight do
          displaybit (col,row);
    end;

    procedure displaybit (col,row : integer);
    begin
      textcolor (getdotcolor (ImageX+col-1));
      gotoxy (fonteditorcolumn+col-1,fonteditorrow+row-1);
      write ('x');
      if col = (charwidth + 1)
        then widthline;
    end;

    procedure savecharacter;
    begin
      getpic (images._letter [workch],imagex,imagey,
              imagex+maxfontwidth-1,imagey+maxfontheight-1);
      images._letter [workch] [1] := charwidth;
      images._letter [workch] [2] := maxfontheight;
    end;

    procedure loadcharacter;
    var row,col : integer; st : string [255];
    begin
      CharWidth := images._letter [workch] [1];
      PutPic (Images._Letter [WorkCh],ImageX,ImageY);
      for row := 1 to MaxFontHeight DO
        for col := 1 to MaxFontWidth DO
          displaybit (col,row);
      st := '';
      case workch of
        ' ' : st := 'space';
        '!' : st := 'exclamation';
        '"' : st := 'double quote';
        '#' : st := 'number';
        '$' : st := 'dollar';
        '%' : st := 'percent';
        '&' : st := 'ampersand';
        '''' : st := 'apostrophe';
        '(','[','{' : st := 'left';
        ')',']','}' : st := 'right';
        '*' : st := 'asterisk';
        '+' : st := 'plus';
        ',' : st := 'comma';
        '-' : st := 'minus';
        '.' : st := 'period';
        '/' : st := 'slash';
        '0' : st := 'zero';
        '1' : st := 'one';
        '2' : st := 'two';
        '3' : st := 'three';
        '4' : st := 'four';
        '5' : st := 'five';
        '6' : st := 'six';
        '7' : st := 'seven';
        '8' : st := 'eight';
        '9' : st := 'nine';
        '0'..'9' : st := 'digit ' + st;
        ':' : st := 'colon';
        ';' : st := 'semicolon';
        '<' : st := 'less than';
        '=' : st := 'equal';
        '>' : st := 'greater than';
        '?' : st := 'mark';
        '@' : st := 'pound';
        'A'..'Z' : st := 'upper';
        'a'..'z' : st := 'lower';
        'A'..'Z','a'..'z' : st := st + ' case "' + upcase (workch) + '"';
        '\' : st := 'backslash';
        '^' : st := 'caret';
        '_' : st := 'underline';
        '`' : st := 'backwards apostrophe';
        '|' : st := '"or" sign';
        '~' : st := 'tilde';
        '#','$','%','@','+','-','<','=','>' : st := st + ' sign';
        '!','?' : st := st + ' mark';
        '(',')' : st := st + ' parenthesis';
        '[',']' : st := st + ' bracket';
        '{','}' : st := st + ' curly bracket';
      end;
      gotoxy (1,25); repeat write (' '); until wherex = 40;
      st := st + ' ("' + workch + '")';
      gotoxy (40 - legnth (st),25); textcolor (1); write (st);
      gotoxy (1,1);
    end;

    procedure blinkingcursor (col,row : integer);
    var
      whatwasthere : icon8;
      x1,x2,y1,y2,t : integer;
    begin
      x1 := gx (col); x2 := x1 + 7;
      y1 := gy (row); y2 := y1 + 7;
      get (whatwasthere,x1,y1,x2,y2);
      t := 0;
      repeat
        if t = 0 then fillregion (x1,y1,x2,y2,1);
        if t = 10 then put (whatwasthere,x1,y1);
        delay (25);
        t := t + 1;
        if t = 20 then t := 0;
      until keypressed;
      t := 0;
      put (whatwasthere,x1,y1);
    end;

    procedure reversebit (col,row : integer);
    var x,y : integer;
    begin
      x := col - 1 + imagex;
      y := row - 1 + imagey;
      plot (x,y,3-getdotcolor (x,y));
      displaybit (col,row);
    end;

BEGIN
  GraphColorMode;
  TextColor (1);
  Writeln ('Mr. Muncher   FONT EDITOR   (C) 1991 by');
  Writeln ('Version 2.0                Darren Embry');
  Writeln;
  TextColor (3);
  Writeln (#32#24);
  Writeln (#27#32#26' - move cursor');
  Writeln (#32#25);
  Writeln;
  Writeln ('Space - invert dot');
  Writeln;
  Writeln ('PgUp - previous char.');
  Writeln ('PgDn - next character');
  Writeln;
  Writeln ('H - horizontal flip');
  Writeln ('V - vertical flip');
  Writeln ('R - reverse character');
  Writeln ('C - clear character');
  Writeln;
  Writeln ('M - "move character"');
  Writeln ('    mode');
  Writeln ('Ctrl-'#27#44#26' - set width');
  Writeln;
  Writeln ('Esc - return to');
  Writeln ('      main menu');
  Writeln;
  Write ('S - type sample text');
  GotoXY (26,18);
  Write ('Width');
  GotoXY (26,20);
  Write ('Character');
  loadcharacter;
  col := 1; row := 1;
  repeat
    blinkingcursor;
    ch := readkey; ch := upcase (ch);
    case ch of
       #0 : begin
              ch := readkey;
              case ch of
                'K' : if col = 1 then col := maxfontwidth else col := col - 1;
                'M' : if col = maxfontwidth then col := 1 else col := col + 1;
                'H' : if row = 1 then row := maxfontheight else row := row - 1;
                'H' : if row = maxfontheight then row := 1 else row := row + 1;
                'I' : begin
                        savecharacter;
                        if workch = #32
                          then workch := #126
                          else workch := char (ord (workch - 1));
                        loadcharacter;
                      end;
                'Q' : begin
                        savecharacter;
                        if workch = #126
                          then workch := #32
                          else workch := char (ord (workch + 1));
                        loadcharacter;
                      end;
            end;
      #27 : begin
              savecharacter;
              exit;
            end;
      'R' : begin
              colortable (3,2,1,0);
              fillregion (gx(1),gy(1),gx(col)+7,gy(row)+7,-1);
              fillregion (imagex,imagey,imagex+maxfontwidth-1,
                          imagey+maxfontheight-1,-1);
              widthline;
            end;
      'C' : begin
              fillregion (gx(1),gy(1),gx(col)+7,gy(row)+7,0);
              fillregion (imagex,imagey,imagex+maxfontwidth-1,
                          imagey+maxfontheight-1,0);
              widthline;
            end;


END;

PROCEDURE MainMenu (VAR Ch : Char);
BEGIN
  TextMode (C80);
  TextColor (7);
  TextBackground (0);
  Writeln ('Mr. Muncher Version 2.0 - GRAPHICS EDITOR - Copyright (C) 1991 by Darren Embry');
  Writeln;
  Writeln ('1.  Edit FONT.');
  Writeln ('2.  Edit ANIMATED SPRITES.');
  Writeln;
  Write ('Hit appropriate key or press [Esc] to exit to DOS.  ');
  REPEAT
    GetAKey (Ch);
  UNTIL Ch IN ['1','2',#27];
END;

VAR
  Ch : Char;

BEGIN
  LoadImages;
  Initialize;
  REPEAT
    MainMenu (Ch);
    CASE Ch OF
      '1' : FontEditor;
      '2' : SpriteEditor;
    END;
  UNTIL Ch = #27;
  ClrScr;
  Write ('Save changes (Y/N)? ');
  REPEAT
    GetAKey (Ch);
  UNTIL Ch IN ['N','Y'];
  IF Ch = 'Y' THEN
    BEGIN
      SaveChanges;
      SetupScreens;
    END;
END;