{--  MAKER2.PAS -- by Darren Embry -- Tuesday, July 23, 1991
     This program sets up the graphics screens.  --}

{$I-}

{--  These lines are to be added only for version 3.0 of Turbo Pascal.
     If version 4.0 or greater is being used, the symbols (* and *) can be
     inserted in order to skip these lines.  Curly brackets (used for this
     comment) cannot be used, because they cannot be nested.  If you can't
     figure it out, then see the Turbo Pascal Reference Guide!  --}

(*
{$C-,U-}
{$I TURBO4.P }
{$I GRAPH.P }
*)

{--  This line is to be added for version 4.0 or above of Turbo Pascal.
     If version 3.0 is being used, the symbols (* and *) can be inserted
     in order to skip this line.  Curly brackets cannot be used, because
     they cannot be nested.  If you can't figure it out, then see the Turbo
     Pascal Reference Manual.  --}

USES Crt,Dos,Graph3;

{ --------- }

{$I FILENAME.P }
{$I DATATYPE.P }

VAR
  Icons : IconsRecord;
  TopTen : ScoreRecord;
  Wait : Integer;
  WindowX1,WindowY1,WindowX2,WindowY2 : Integer;
  Configuration : ConfigType;

{$I SCROLLS.P }
{$I INCLUDES.P }

TYPE
  BigLine = ARRAY [1..6,1..40] OF Byte;
CONST
  x = 16;
  MrMuncher : BigLine =
   (( x, x, 5,12,13,12, 9, x, x, x, x, x, x, x, x, 5,12,13,12, 9, x, x, x, x, x, x, x, x, x, 1, x, x, x, x, x, x, x, x, x, x),
    ( x, x, 3, x, 3, x, 3, 5,12, 9, x, x, x, x, x, 3, x, 3, x, 3, 1, x, 1, 5,12, 9, 5,12, 9, 7,12, 9, 5,12, 9, 5,12, 9, x, x),
    ( x, x, 3, x, 3, x, 3, 3, x, 2, x, x, x, x, x, 3, x, 3, x, 3, 3, x, 3, 3, x, 3, 3, x, 2, 3, x, 3, 7,12,10, 3, x, 2, x, x),
    ( x, x, 3, x, 2, x, 3, 3, x, x, x, x, x, x, x, 3, x, 2, x, 3, 3, x, 3, 3, x, 3, 3, x, x, 3, x, 3, 3, x, 1, 3, x, x, x, x),
    ( x, x, 2, x, x, x, 2, 2, x, x, 0, x, x, x, x, 2, x, x, x, 2, 6,12,10, 2, x, 2, 6,12, 8, 2, x, 2, 6,12,10, 2, x, x, x, x),
    ( x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x));
  ReaperWare : BigLine =
   (( x, x, x, 5,12,12, 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, x, x, x, 1, x, x, x, x, x, x, x, x, x, x, x, x),
    ( x, x, x, 3, x, x, 3, 5,12, 9, 4,12, 9, 5,12, 9, 5,12, 9, 5,12, 9, x, 3, x, 1, x, 3, 4,12, 9, 5,12, 9, 5,12, 9, x, x, x),
    ( x, x, x, 7,12,13,10, 7,12,10, 5,12,11, 3, x, 3, 7,12,10, 3, x, 2, x, 3, x, 3, x, 3, 5,12,11, 3, x, 2, 7,12,10, x, x, x),
    ( x, x, x, 3, x, 3, x, 3, x, 1, 3, x, 3, 3, x, 3, 3, x, 1, 3, x, x, x, 3, x, 3, x, 3, 3, x, 3, 3, x, x, 3, x, 1, x, x, x),
    ( x, x, x, 2, x, 6, 8, 6,12,10, 6,12,10, 7,12,10, 6,12,10, 2, x, x, x, 6,12,14,12,10, 6,12,10, 2, x, x, 6,12,10, x, x, x),
    ( x, x, x, x, x, x, x, x, x, x, x, x, x, 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x));
  Enterprises : BigLine =
   (( x, x, x, x, 5,12,12, 8, x, x, x, x, 1, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, x, x, x, x, x, x, x, x, x, x),
    ( x, x, x, x, 3, x, x, x, 5,12, 9, 4,15, 8, 5,12, 9, 5,12, 9, 5,12, 9, 5,12, 9, 1, 5,12, 8, 5,12, 9, 5,12, 8, x, x, x, x),
    ( x, x, x, x, 7,12, 8, x, 3, x, 3, x, 3, x, 7,12,10, 3, x, 2, 3, x, 3, 3, x, 2, 3, 6,12, 9, 7,12,10, 6,12, 9, x, x, x, x),
    ( x, x, x, x, 3, x, x, x, 3, x, 3, x, 3, 1, 3, x, 1, 3, x, x, 3, x, 3, 3, x, x, 3, x, x, 3, 3, x, 1, x, x, 3, x, x, x, x),
    ( x, x, x, x, 6,12,12, 8, 2, x, 2, x, 6,10, 6,12,10, 2, x, x, 7,12,10, 2, x, x, 2, 4,12,10, 6,12,10, 4,12,10, x, x, x, x),
    ( x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x));

PROCEDURE DisplayBigText (Row : Integer; VAR Buffer; c0,c2,c3,c1 : Integer);
VAR TextData : BigLine ABSOLUTE Buffer; X,Y : Integer;
    B : ARRAY [1..4] OF Boolean; D : Byte;
BEGIN
  ColorTable (c0,c1,c2,c3);
  FOR X := 1 TO 40 DO FOR Y := Row TO Row + 5 DO
    WITH Icons DO
      BEGIN
        D := TextData [Y-Row+1,X];
        B[1] := Boolean ((D SHR 3) AND 1); B[2] := Boolean ((D SHR 2) AND 1);
        B[3] := Boolean ((D SHR 1) AND 1); B[4] := Boolean ((D SHR 0) AND 1);
        IF D IN [0..15] THEN Put (_Wall [B[1],B[2],B[3],B[4]],X*8-8,Y*8-8);
      END;
  ColorTable (0,1,2,3);
END;

(*

PROCEDURE IntroductionScreen;
BEGIN
  ClearScreen;
  Circle (160,100,40,2); Circle (160,100,45,2); FillShape (160,58,2,2);
  FillRegion (100,97,220,102,2);
  Draw (157,30,163,30,1); Draw (163,30,206,160,1);
  Draw (206,160,200,160,1); Draw (200,160,160,40,1);
  Draw (160,40,120,160,1); Draw (120,160,114,160,1); Draw (114,160,157,30,1);
  FillShape (160,35,3,1); FillShape (160,35,2,1);
  Draw (157,30,163,30,2); Draw (163,30,206,160,2);
  Draw (206,160,200,160,2); Draw (200,160,160,40,2);
  Draw (160,40,120,160,2); Draw (120,160,114,160,2); Draw (114,160,157,30,2);
  DisplayBigText (8,ReaperWare,0,2,3,1);
  DisplayBigText (13,Enterprises,0,2,3,1);
  BoxString (' ...You Will Like My Software!!! ',160,0,2,1,0,Center_,6);
  BoxString (' presents... ',160,176,1,2,0,Center_,6);
  SaveScreen (IntroductionScreenFile);
END;

procedure GameScreen;
begin
  clearscreen;
  box3d (0,0,213,189,1,0,1,0); box3d (1,1,212,188,3,0,1,0);
  box3d (2,2,211,187,3,2,1,0); box3d (3,3,210,186,2,2,1,0);
  box3d (4,4,209,185,2,3,1,0); box3d (5, 5,208,184,0,3,1,0);
  box3d (6,6,207,183,0,1,1,0);
  halftonebox (214,0,319,189,2,0,8);
  displaystring ('PLAYER #1',268,16,3,2,center_);
  halftonebox (228,28,307,43,0,2,3);
  box3d (228,46,307,71,3,0,2,2);
  displaystring ('PLAYER #2',268,120,3,2,center_);
  halftonebox (228,132,307,147,0,2,3);
  box3d (228,150,307,175,3,0,2,2);
  displaystring ('Round',234,50,0,2,left_);
  displaystring ('Men',302,50,0,2,right_);
  displaystring ('Round',234,154,0,2,left_);
  displaystring ('Men',302,154,0,2,right_);
  savescreen(gamescreenfile);
end;

PROCEDURE InstructionScreen;
CONST FromSide = 20;
VAR L : Integer;
BEGIN
  ClearScreen;
  HalfToneBox (0,0,319,15,1,0,3);
  DisplayString ('HOW TO PLAY MR. MUNCHER',160,4,2,1,Center_);
  DisplayString ('The object of this game is similar to that of'   ,160,19,3,0,Center_);
  DisplayString ('Pac-Man.  You eat the food pellets (duh!) to'    ,160,30,3,0,Center_);
  DisplayString ('advance to the next board without getting killed',160,41,3,0,Center_);
  DisplayString ('by the ghosts.  The energy pellets give you time',160,52,3,0,Center_);
  DisplayString ('to eat the ghosts, but watch out for flashing'   ,160,63,3,0,Center_);
  DisplayString ('ghosts that are about to change back to their'   ,160,74,3,0,Center_);
  DisplayString ('dangerous colors.  Here are the keys you need to',160,85,3,0,Center_);
  DisplayString ('use to play the game...'                         ,160,96,3,0,Center_);

  HalfToneBox (FromSide-10,107,329-FromSide,168,2,0,3);

  L := FromSide;
  DisplayString ('Move creature'                       ,L,111,3,2,Left_);
  DisplayString ('Toggle sound on/off'                 ,L,122,3,2,Left_);
  DisplayString ('Pause'                               ,L,133,3,2,Left_);
  DisplayString ('Help screen and options menu goodies',L,144,3,2,Left_);
  DisplayString ('Quit game and return to main menu'   ,L,155,3,2,Left_);
  L := 319 - L;
  DisplayString ('arrow keys',L,111,1,2,Right_);
  DisplayString ('S'         ,L,122,1,2,Right_);
  DisplayString ('P'         ,L,133,1,2,Right_);
  DisplayString ('F1'        ,L,144,1,2,Right_);
  DisplayString ('Esc'       ,L,155,1,2,Right_);

  DisplayString ('Good luck, and HAVE FUN!!!',160,170,1,0,Center_);
  BoxString ('Hit a key already...',160,182,0,2,0,Center_,3);
  SaveScreen (InstructionsScreenFile);
END;

PROCEDURE HighScoreScreen;
CONST FromSide = 24;
VAR P : Integer; St : String [255];
BEGIN
  ClearScreen;
  HalfToneBox (0,0,319,188,2,0,3); HalfToneBox (4,4,315,184,3,2,3);
  HalfToneBox (9,9,309,39,0,3,4);
  DisplayString ('Top Ten Greatest',160,14,1,0,Center_);
  DisplayString ('MR. MUNCHER Players!',160,25,1,0,Center_);
  HalfToneBox (10,43,309,58,2,3,3); HalfToneBox (10,60,309,178,2,3,4);
{
  Field      Starting X     Justification

  Rank       FromSide+15    Right
  Name       FromSide+25    Left
  Round      319-FromSide   Right
  Score      274-FromSide   Right
}
  DisplayString ('Name' ,FromSide+25, 47,0,2,Left_);
  DisplayString ('Round',319-FromSide,47,0,2,Right_);
  DisplayString ('Score',274-FromSide,47,0,2,Right_);
  SaveScreen (HighScoresScreenFile);
END;

procedure boardeditorscreen;
var x,y : integer;
begin
  x := 9; y := 7; clearscreen;
  halftonebox (0,0,105,199,2,0,6);
  halftonebox (106,10,319,199,2,0,6);
  fillregion (113,17,312,192,0);
  box3D (x,y,x+88-1,y+13,0,3,2,1);
  displaystring ('Board Editor',x+4,y+3,1,2,Left_);
  y := y + 16;
  displaystring ('Main Menu',x,y,3,2,left_);
  displaystring ('Esc',x+88-1,y,1,2,right_);
  draw (x,y+9,x+88-1,y+9,0);
  displaystring ('Scan boards',x,y+11,3,2,left_);
  displaystring ('PgUp, PgDn',x+88-1,y+20,1,2,right_);
  draw (x,y+31,x+88-1,y+31,0);
  displaystring ('Save board',x,y+33,3,2,left_);
  displaystring ('S',x+88-1,y+33,1,2,right_);
  draw (x,y+42,x+88-1,y+42,0);
  displaystring ('Load board',x,y+44,3,2,left_);
  displaystring ('L',x+88-1,y+44,1,2,right_);
  draw (x,y+53,x+88-1,y+53,0);
  displaystring ('Clear board',x,y+55,3,2,left_);
  displaystring ('C',x+88-1,y+55,1,2,right_);
  draw (x,y+64,x+88-1,y+64,0);
  displaystring ('Exchange',x,y+66,3,2,left_);
  displaystring ('X',x+88-1,y+66,1,2,right_);
  y := y + 77;
  halftonebox (x,y,x+88-1,y+90,0,2,5);
  displaystring ('Edit Keys:',x+88 div 2,y+7,1,0,center_);
  draw (x,y+18,x+88-1,y+18,2);
  with icons do
    begin
      put (_creature [right,1],x+8,y+20);
      put (_ghost [dangerous],x+8,y+31);
      put (_Dot,x+8,y+53);
      put (_energizer,x+8,y+64);
    end;
  displaystring ('1',x+88-1-8,y+20,1,0,right_);
  displaystring ('2',x+88-1-8,y+31,1,0,right_);
  displaystring ('3',x+88-1-8,y+42,1,0,right_);
  displaystring ('4',x+88-1-8,y+53,1,0,right_);
  displaystring ('5',x+88-1-8,y+64,1,0,right_);
  displaystring ('0',x+88-1-8,y+75,1,0,right_);
  displaystring ('Player',x+(88 div 2),y+20,3,0,center_);
  displaystring ('Enemy' ,x+(88 div 2),y+31,3,0,center_);
  displaystring ('Walls' ,x+(88 div 2),y+42,3,0,center_);
  displaystring ('Food'  ,x+(88 div 2),y+53,3,0,center_);
  displaystring ('Energy',x+(88 div 2),y+64,3,0,center_);
  displaystring ('Eraser',x+(88 div 2),y+75,3,0,center_);
  savescreen (BoardEditorScreenFile);
end;

*)

PROCEDURE MainMenuScreen;
CONST FromSide = 88; { pixels from each side of screen to write text
                       for main menu options }
VAR L : Integer; St : String255;
BEGIN
  ClearScreen;
  HalfToneBox (0,0,319,199,2,0,7);
  DisplayBigText (2,MrMuncher,1,3,2,0);
  BoxString ('Version 0.0',16,50,0,3,2,Left_,4);
  BoxString ('Programmed by Darren Embry',303,50,0,3,2,Right_,4);
  BoxString ('Copy and share this game with your friends!',160,72,3,0,2,Center_,4);
  HalfToneBox (7,102,312,192,0,2,7);
  DisplayString ('MAIN MENU',160,93,1,2,Center_);
  L := FromSide;
  DisplayString ('Exit to DOS'      ,L,113,2,0,Left_);
  DisplayString ('Play a game'      ,L,125,2,0,Left_);
  DisplayString ('See instructions' ,L,137,2,0,Left_);
  DisplayString ('Edit game boards' ,L,149,2,0,Left_);
  DisplayString ('See high scores'  ,L,161,2,0,Left_);
  DisplayString ('Reset high scores',L,173,2,0,Left_);
  L := 319 - L;
  DisplayString ('Esc',L,113,1,0,Right_);
  DisplayString ('P'  ,L,125,1,0,Right_);
  DisplayString ('I'  ,L,137,1,0,Right_);
  DisplayString ('E'  ,L,149,1,0,Right_);
  DisplayString ('H'  ,L,161,1,0,Right_);
  DisplayString ('R'  ,L,173,1,0,Right_);
  HalfToneBox (18,116,FromSide-8,179,2,0,4);
  HalfToneBox (301,116,327-FromSide,179,2,0,4);
  L := (FromSide DIV 2) + 4;
  DisplayString ('Press'  ,l,122,3,2,Center_);
  DisplayString ('arrow'  ,l,133,3,2,Center_);
  DisplayString ('keys to',l,144,3,2,Center_);
  DisplayString ('select' ,l,155,3,2,Center_);
  DisplayString ('item.'  ,l,166,3,2,Center_);
  L := 319 - L;
  DisplayString ('Press'   ,l,122,3,2,Center_);
  DisplayString ('Enter'   ,l,133,3,2,Center_);
  DisplayString ('to use'  ,l,144,3,2,Center_);
  DisplayString ('selected',l,155,3,2,Center_);
  DisplayString ('item.'   ,l,166,3,2,Center_);
  SaveScreen (MainMenuScreenFile);
END;

(*

PROCEDURE SharewareScreen;
BEGIN
  ClearScreen;
  DisplayString ('This is a beta-test version of Mr. Muncher.  You',160,  0,3,0,center_);
  DisplayString ('are allowed (and encouraged) to make unmodified' ,160, 11,3,0,center_);
  DisplayString ('copies of this software and pass them to your'   ,160, 22,3,0,center_);
  DisplayString ('friends, and to upload it to bulletin board'     ,160, 33,3,0,center_);
  DisplayString ('systems.  If you have any comments, suggestions,',160, 44,3,0,center_);
  DisplayString ('complaints, or questions about this program or'  ,160, 55,3,0,center_);
  DisplayString ('you found some bugs in it, then please drop me'  ,160, 66,3,0,center_);
  DisplayString ('a line at:'                                      ,160, 77,3,0,center_);

  DisplayString ('Darren Embry'                                    ,160, 93,1,0,center_);
  DisplayString ('2404 Portland Avenue'                            ,160,104,1,0,center_);
  DisplayString ('Louisville, Ky. 40212-1038'                      ,160,115,1,0,center_);
  DisplayString ('(Enclose a S.A.S.E. for a personal reply.)'      ,160,126,2,0,center_);

  DisplayString ('This version of Mr. Muncher is placed in the'    ,160,142,3,0,center_);
  DisplayString ('public domain.  You do not have to pay any'      ,160,153,3,0,center_);
  DisplayString ('registration fees for this program.'             ,160,164,3,0,center_);

  DisplayString ('Hit any key to exit to DOS...'                   ,160,190,1,0,center_);
  SaveScreen (ExitToDOSscreenFile);
END;

*)

VAR Ch : Char;

BEGIN
  windowx1 := 0; windowy1 := 0; windowx2 := 319; windowy2 := 199;
  GraphColorMode;
  LoadIcons;
(*
  IntroductionScreen;
  HighScoreScreen;
  InstructionScreen;
  BoardEditorScreen;
  GameScreen;
  SharewareScreen;
*)
  MainMenuScreen;
  Readln;
  TextMode (C80);
END.

