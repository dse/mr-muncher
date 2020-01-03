(*****************************************************************************)
(**                                                                         **)
(**  Filename:  MAKER.PAS                                                   **)
(**    Author:  Darren Embry                                                **)
(**      Date:  Sunday, February 24, 1991                                   **)
(**                                                                         **)
(**  This program, when run, makes a data file that contains the images     **)
(**  stored in M-IMAGES.CGA.  See MUNCHER.PAS for more details.             **)
(**                                                                         **)
(**-------------------------------------------------------------------------**)
(**                                                                         **)
(**  NOTE:  I'm not going to explain the procedures used to draw some of    **)
(**         the images.  That is because when I'm about to make an image    **)
(**         editor anyway.                                                  **)
(**                                                                         **)
(*****************************************************************************)

{$I-}

{-----------------------------------------------------------------------------}
{--                                                                         --}
{--  These lines are to be added only for version 3.0 of Turbo Pascal.      --}
{--  If version 4.0 or greater is being used, the symbols (* and *) can be  --}
{--  inserted in order to skip these lines.  Curly brackets (used for this  --}
{--  comment) cannot be used.  If you don't know why, then read the Turbo   --}
{--  Pascal reference manual.                                               --}
{--                                                                         --}
{-----------------------------------------------------------------------------}

(*
{$I TURBO4.P }
{$I GRAPH.P }
*)

{-----------------------------------------------------------------------------}
{--                                                                         --}
{--  This line is to be added for version 4.0 or above of Turbo Pascal.     --}
{--  If version 3.0 is being used, the symbols (* and *) can be inserted    --}
{--  in order to skip this line.  Curly brackets cannot be used, and if     --}
{--  you do not know why, then read the Turbo Pascal Reference Manual.      --}
{--                                                                         --}
{-----------------------------------------------------------------------------}

USES DOS,Crt,Graph3;

{-------------------------------}
{--                           --}
{--  You're on your own now.  --}
{--                           --}
{-------------------------------}

{$I FILENAME.P }
{$I DATATYPE.P }

VAR Icons : IconsRecord;
    wait,windowx1,windowy1,windowx2,windowy2 : integer;
    topten : scorerecord;
    configuration : configtype;

{$I SCROLLS.P }
{$I INCLUDES.P }

PROCEDURE SaveIcons;
VAR F : File;
BEGIN
  Assign (F,IconsScreenFile);
  Rewrite (F,1);
  BlockWrite (F,Icons,SizeOf(Icons));
  Close (F);
END;

CONST
  DigitSize = 58;
  DigitData : ARRAY [1..DigitSize] OF Byte =
   (62,127,65,127,62,0,2,127,127,0,98,115,89,79,70,0,34,99,73,127,54,0,
   24,28,22,127,127,0,39,103,69,125,57,0,62,127,73,123,50,0,1,113,125,15,3,0,
   54,127,73,127,54,0,38,207,73,127,62,0);
  Size = 666;
  Data : ARRAY [1..Size] OF Integer =
    (0,0,0,0,0,14,223,223,14,0,8,6,3,8,6,3,0,80,52,28,86,53,28,22,5,0,
     36,110,74,255,74,122,48,0,70,37,19,200,164,98,0,
     112,250,143,157,247,98,240,152,0,12,6,3,0,60,126,195,129,0,
     129,195,126,60,0,34,20,8,127,8,20,34,0,16,16,16,254,16,16,16,0,
     768,448,192,0,16,16,16,16,16,16,16,0,192,192,0,192,96,48,24,12,6,3,0,
     126,255,129,129,255,126,0,130,130,255,255,128,128,0,
     194,227,177,153,143,134,0,66,195,137,137,255,118,0,48,56,44,38,255,255,0,
     71,199,133,133,253,121,0,126,255,137,137,251,114,0,1,193,241,61,15,3,0,
     118,255,137,137,255,118,0,78,223,145,145,255,126,0,204,204,0,
     768,460,204,0,16,16,40,40,68,68,130,130,0,68,68,68,68,68,68,68,0,
     130,130,68,68,40,40,16,16,0,2,3,209,217,15,6,0,
     126,195,153,165,181,157,163,30,0,248,252,38,35,38,252,248,0,
     255,255,137,137,137,255,118,0,126,255,129,129,129,195,66,0,
     255,255,129,129,129,255,126,0,255,255,137,137,137,129,129,0,
     255,255,9,9,9,1,1,0,126,255,129,129,145,243,114,0,255,255,8,8,8,255,255,0,
     129,255,255,129,0,64,192,128,128,255,127,0,255,255,24,60,102,195,129,0,
     255,255,128,128,128,128,128,0,255,255,14,60,60,14,255,255,0,
     255,255,14,28,56,255,255,0,126,255,129,129,129,255,126,0,
     255,255,17,17,17,31,14,0,126,255,129,385,897,767,638,0,
     255,255,17,17,49,255,206,0,70,207,137,137,137,251,114,0,
     1,1,1,255,255,1,1,1,0,127,255,128,128,128,255,127,0,
     31,63,96,192,96,63,31,0,127,255,128,240,240,128,255,127,0,
     195,231,60,24,60,231,195,0,3,7,12,248,248,12,7,3,0,
     193,225,177,153,141,135,131,0,255,255,129,129,0,3,6,12,24,48,96,192,0,
     129,129,255,255,0,24,12,6,3,6,12,24,0,512,512,512,512,512,512,512,512,0,
     3,6,12,0,96,244,148,148,252,248,0,255,127,196,132,252,120,0,
     120,252,132,132,204,72,0,120,252,132,196,127,255,0,
     120,252,148,148,220,88,0,4,254,255,5,1,0,120,764,644,708,1020,508,0,
     255,255,12,4,252,248,0,4,253,253,0,516,1021,509,0,255,255,16,56,236,196,0,
     1,255,255,0,252,248,4,252,248,4,252,248,0,252,248,12,4,252,248,0,
     120,252,132,132,252,120,0,1020,1016,140,132,252,120,0,
     120,252,132,132,1020,1020,0,252,248,12,4,12,8,0,136,156,148,148,244,96,0,
     4,127,255,132,128,0,124,252,128,192,124,252,0,28,60,96,192,96,60,28,0,
     124,252,128,248,248,128,252,124,0,132,204,120,48,120,204,132,0,
     124,764,640,640,1020,508,0,196,228,180,156,140,132,0,16,254,495,257,0,
     511,0,257,495,254,16,0,6,3,3,6,12,12,6,0);

CONST
  BackgroundColor = 0;
  BoardColor1 = 2; (* outer *)
  BoardColor2 = 3; (* middle *)
  BoardColor3 = 1; (* inner *)
  PlayerColor = 1;
  GhostColor = 3;
  EyeColor = 2; (* definitely should not be same as BackgroundColor ! ! ! ! *)
  EdibleGhostColor = 3;
  EdibleEyeColor = 1; (* definitely should not be BackgroundColor, for
                         EdibleGhostColor only draws image outline *)
  FlashingGhostColor1 = 1;
  FlashingEyeColor1 = 2;
  FlashingGhostColor2 = 2;
  FlashingEyeColor2 = 1;
  DotColor = 2;
  EnergizerColor1 = 1; (* outer *)
  EnergizerColor2 = 3; (* middle *)
  EnergizerColor3 = 2; (* inner *)
  CherryColor = 2;
  StemCOlor = 3;
  StrawberryColor = 2;
  SeedColor = 3;
  BananaColor = 1;
  BonusScoreColor1 = 1; (* 3-D text, main part *)
  BonusScoreColor2 = 0; (* 3-D text, underlay  *)
  BonusScoreColor3 = 2; (* color of inside of box *)
  BonusScoreColor4 = 1; (* outer edge of box *)
  BonusScoreColor5 = 3; (* 2nd closest to outer edge *)
  BonusColor1 = 1; (* extra man, background *)
  BonusColor2 = 2; (* extra man, man itself *)
  LetterColor = 3;

var digit : array ['0'..'9',0..99] of integer;

PROCEDURE BoardBox (X1,Y1,X2,Y2 : Integer);
BEGIN
  Rectangle (x1,y1,x2,y2,boardcolor1);
  Rectangle (x1+1,y1+1,x2-1,y2-1,boardcolor2);
  Rectangle (x1+2,y1+2,x2-2,y2-2,boardcolor3);
end;

PROCEDURE MakeScore (VAR Buffer; St : String255);
VAR P,X1,Y1 : Integer;
BEGIN
  X1 := 3; Y1 := 3;
  FOR P := 1 TO Length (St) DO
    BEGIN
      Put (Digit [St [P]],X1,Y1);
      X1 := X1 + Digit [St [P]] [1];
    END;
  Get (Buffer,3,3,X1-1,10);
  FillRegion (0,0,X1+2,13,BonusScoreColor3);
  Rectangle (0,0,X1+2,13,BonusScoreColor4);
  Rectangle (1,1,X1+1,12,BonusScoreColor5);
  Put (Buffer,3,3);
  Get (Buffer,0,0,X1+2,13);
  GotoXY (1,4);
  FillRegion (0,0,X1+2,13,BackgroundColor);
END;

VAR X,Y,D,C,B,Z : Integer; Ch : Char;

BEGIN
  windowx1 := 0;
  windowy1 := 0;
  windowx2 := 319;
  windowy2 := 199;
  WITH Icons DO
    BEGIN
      GraphColorMode;
      BoardBox (1,1,22,22);
      BoardBox (1,25,22,30);
      BoardBox (25,1,30,22);
      BoardBox (25,25,30,30);
      BoardBox (9,9,6,6);
      BoardBox (9,17,6,14);
      BoardBox (17,17,14,14);
      BoardBox (17,9,14,6);
      C := PlayerColor; Draw (2,32,5,32,C); Draw (1,33,6,33,C);
      Draw (0,34,7,34,C); Draw (0,35,3,35,C); Draw (0,36,3,36,C);
      Draw (0,37,7,37,C); Draw (1,38,6,38,C); Draw (2,39,5,39,C);
      Draw (10,32,13,32,C); Draw (9,33,14,33,C); Draw (8,34,12,34,C);
      Draw (8,35,11,35,C); Draw (8,36,11,36,C); Draw (8,37,12,37,C);
      Draw (9,38,14,38,C); Draw (10,39,13,39,C);
      Draw (10,40,13,40,C); Draw ( 9,41,14,41,C); Draw ( 8,42,15,42,C);
      Draw ( 8,43,15,43,C); Draw ( 8,44,15,44,C); Draw ( 8,45,15,45,C);
      Draw ( 9,46,14,46,C); Draw (10,47,13,47,C);
      FOR X := 0 TO 15 DO FOR Y := 0 TO 7 DO
        BEGIN
          C := GetDotColor (X,32+Y); Plot (23-Y,32+X,C);
          Plot (23-X,55-Y,C); Plot (0+Y,55-X,C);
        END;
      C := PlayerColor;
      Draw (26,32,29,32,C); Draw (25,33,30,33,C); Draw (24,34,31,34,C);
      Draw (24,35,31,35,C); Draw (24,36,27,36,C); Draw (24,37,27,37,C);
      Draw (25,38,28,38,C); Draw (26,39,29,39,C); Draw (30,36,31,36,C);
      Plot (31,37,C);
      FOR X := 0 TO 7 DO FOR Y := 0 TO 7 DO
        BEGIN
          C := GetDotColor (24+X,32+Y); Plot (24+X,47-Y,C);
          Plot (39-X,47-Y,C); Plot (39-X,32+Y,C);
        END;
      C := GhostColor; Draw (32,2,32,7,C); Draw (33,1,33,6,C);
      Draw (34,0,34,7,C); Draw (35,0,35,6,C); Draw (36,0,36,7,C);
      Draw (37,0,37,6,C); Draw (38,1,38,7,C); Draw (39,2,39,6,C);
      C := EyeColor; Draw (34,2,34,3,C); Draw(37,2,37,3,C);
      Plot (33,3,C); Plot (38,3,C);
      FOR X := 0 TO 7 DO FOR Y := 0 TO 7 DO
        IF GetDotColor (32+X,Y) = GhostColor THEN
          BEGIN
            Plot (32+X,Y+8,FlashingGhostColor1);
            Plot (32+X,Y+16,FlashingGhostColor2);
          END
        ELSE IF GetDotColor (32+X,Y) = EyeColor THEN
          BEGIN
            Plot (32+X,Y+8,FlashingEyeColor1);
            Plot (32+X,Y+16,FlashingEyeColor2);
          END;
      C := EdibleGhostColor;
      Draw (32,26,32,31,C); Draw (39,26,39,30,C); Plot (33,25,C);
      Plot (38,25,C); Draw (34,24,37,24,C); Plot (33,30,C); Plot (34,31,C);
      Plot (35,30,C); Plot (36,31,C); Plot (37,30,C); Plot (38,31,C);
      C := EdibleEyeColor; Draw (34,27,34,28,C); Draw (37,27,37,28,C);
      Rectangle (35,51,36,52,DotColor);
      Rectangle (25,49,30,54,EnergizerColor1);
      Rectangle (26,50,29,53,EnergizerColor2);
      Rectangle (27,51,28,52,EnergizerColor3);
      C := CherryColor; Rectangle (0,60,3,61,C); Rectangle (1,59,2,62,C);
      Rectangle (4,61,7,62,C); Rectangle (5,60,6,63,C);
      C := StrawberryColor; Rectangle (8,59,15,61,C); Rectangle (9,58,14,64,C);
      Rectangle (10,58,13,65,C); Rectangle (11,58,12,65,C);
      C := StemColor; Draw (4,59,4,60,C); Draw (5,57,5,58,C);
      Draw (6,56,7,56,C); Draw (12,57,12,58,C); Plot (13,56,C);
      C := SeedColor; Plot (8,60,C); Plot (10,59,C); Plot (15,59,C);
      Plot (13,60,C); Plot (11,61,C); Plot (9,62,C); Plot (14,62,C);
      Plot (12,63,C); Plot (10,64,C); Plot (13,65,C);
      C := BananaColor; Draw (17,64,17,65,C); Draw (18,63,18,65,C);
      Draw (19,62,19,65,C); Draw (20,60,20,64,C); Draw (21,57,21,64,C);
      Draw (22,59,22,62,C);
      C := BonusColor1; Rectangle (24,57,31,62,C);
      Rectangle (25,56,30,63,C); Rectangle (28,59,30,60,C);
      C := BonusColor2; Rectangle (26,57,27,62,C);
      Rectangle (25,58,27,61,C);
      Draw (28,57,29,57,C); Draw (28,58,30,58,C);
      Draw (28,62,29,62,C); Draw (28,61,30,61,C);
      Get (_Wall [False,False,False,False],24,24,31,31);
      Get (_Wall [False,False,False,True],24,0,31,7);
      Get (_Wall [False,False,True,False],24,16,31,23);
      Get (_Wall [False,False,True,True],24,8,31,15);
      Get (_Wall [False,True,False,False],0,24,7,31);
      Get (_Wall [False,True,False,True],0,0,7,7);
      Get (_Wall [False,True,True,False],0,16,7,23);
      Get (_Wall [False,True,True,True],0,8,7,15);
      Get (_Wall [True,False,False,False],16,24,23,31);
      Get (_Wall [True,False,False,True],16,0,23,7);
      Get (_Wall [True,False,True,False],16,16,23,23);
      Get (_Wall [True,False,True,True],16,8,23,15);
      Get (_Wall [True,True,False,False],8,24,15,31);
      Get (_Wall [True,True,False,True],8,0,15,7);
      Get (_Wall [True,True,True,False],8,16,15,23);
      Get (_Wall [True,True,True,True],8,8,15,15);
      Get (_Creature [Left,0],8,40,15,47); Get (_Creature [Left,1],16,48,23,55);
      Get (_Creature [Left,2],8,48,15,55); Get (_Creature [Right,0],8,40,15,47);
      Get (_Creature [Right,1],0,32,7,39); Get (_Creature [Right,2],8,32,15,39);
      Get (_Creature [Up,0],8,40,15,47); Get (_Creature [Up,1],0,48,7,55);
      Get (_Creature [Up,2],0,40,7,47); Get (_Creature [Down,0],8,40,15,47);
      Get (_Creature [Down,1],16,32,23,39); Get (_Creature [Down,2],16,40,23,47);
      Get (_DeadCreature [0],0,48,7,55); Get (_DeadCreature [1],24,40,31,47);
      Get (_DeadCreature [2],0,32,7,39); Get (_DeadCreature [3],24,32,31,39);
      Get (_DeadCreature [4],16,32,23,39); Get (_DeadCreature [5],32,32,39,39);
      Get (_DeadCreature [6],16,48,23,55); Get (_DeadCreature [7],32,40,39,47);
      Get (_Ghost [Dangerous],32,0,39,7); Get (_Ghost [Edible],32,16,39,23);
      Get (_Ghost [Flash1],32,8,39,15); Get (_Ghost [Flash2],32,24,39,31);
      Get (_Dot,32,48,39,55); Get (_Energizer,24,48,31,55);
      Get (_Cherry,0,56,7,63); Get (_Strawberry,8,56,15,65);
      Get (_Banana,16,56,23,65); Get (_ExtraMan,24,56,31,63);
      Readln;
      ClearScreen;
      FillScreen (BackgroundColor);
      Ch := ' '; C := 1; REPEAT C := C + 1 UNTIL Data [C] <> 0;
      C := C-1; X := C-1; Z := 7;
      REPEAT
        Get (_Letter [Ch],0,0,X,Z); FillRegion (0,0,9,9,BackgroundColor);
        Z := 0; C := C + 1; Ch := Succ (Ch); X := 0;
        IF Ch < #127 THEN REPEAT
          Y := Data [C];
          FOR B := 0 TO 9 DO
            BEGIN
              IF (Y AND 1) = 1 THEN
                BEGIN Plot (X,B,LetterColor); IF Z < B THEN Z := B; END;
              Y := Y SHR 1;
            END;
          X := X + 1; C := C + 1;
        UNTIL Data [C] = 0;
        X := X - 1;
      UNTIL Ch = #127;
    END;
  C := 1;
  FOR Ch := '0' TO '9' DO
    BEGIN
      X := 0;
      FillRegion (0,0,19,19,BonusScoreColor3);
      REPEAT
        Y := DigitData [C];
        FOR B := 0 TO 6 DO
          BEGIN
            IF (Y AND 1) = 1 THEN
              BEGIN
                Plot (X,B,BonusScoreColor1);
                Plot (X+1,B+1,BonusScoreColor2);
                IF Z < B THEN Z := B;
              END;
            Y := Y SHR 1;
          END;
        X := X + 1; C := C + 1;
      UNTIL DigitData [C] = 0;
      Get (Digit [Ch],0,0,X,7); FillRegion (0,0,9,9,BackgroundColor);
      C := C + 1;
    END;
  WITH Icons DO
    BEGIN
      MakeScore (_100,'100'); MakeScore (_200,'200');
      MakeScore (_400,'400'); MakeScore (_500,'500');
      MakeScore (_800,'800'); MakeScore (_1000,'1000');
      MakeScore (_1600,'1600');
    END;
  SaveIcons; TextMode (C80);
END.