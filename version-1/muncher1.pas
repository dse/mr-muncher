{$C-,U-}
PROGRAM Mister_Muncher;
TYPE
  ScreenChar = RECORD CASE Byte OF
                 1 : (Character : Char; Attribute : Byte);
                 2 : (CharAttr : Integer);
               END;
  Screen40   = ARRAY [1..25,1..40] OF ScreenChar;
  Registers  = RECORD CASE Byte OF
                 1 : (AX,BX,CX,DX,BP,SI,DI,DS,ES,Flags : Integer);
                 2 : (AL,AH,BL,BH,CL,CH,DL,DH : Byte);
               END;
  Player     = (Computer,Person);
  Char24     = ARRAY [1..24] OF Char;
  ScoreRec   = RECORD
                 TheScore : Real;
                 Name     : Char24;
               END;
  Scores     = ARRAY [1..5,1..20] OF ScoreRec;
VAR
  Screen       : Screen40 ABSOLUTE $B800 : $0000;
  MainMenu     : Screen40 ABSOLUTE $B800 : $0800;
  Instructions : Screen40 ABSOLUTE $B800 : $1000;
  Gameboard    : Screen40 ABSOLUTE $B800 : $1800;
  Backup       : Screen40 ABSOLUTE $B800 : $2000;
  Screen2      : Screen40 ABSOLUTE $B800 : $2800;
  Data      : File;
  Regs      : Registers;
  WantSound : Boolean;
  HighScore : Scores ABSOLUTE $B800 : $3000;

PROCEDURE ClearKeys;
VAR Ch : Char;
BEGIN
  WHILE KeyPressed DO Read (KBD,Ch);
END;

PROCEDURE Tone (A : Integer);
BEGIN
  IF WantSound THEN Sound (A);
END;

PROCEDURE CursorOff;
BEGIN
  WITH Regs DO
    BEGIN
      AX := $0100;
      CX := $2000;
    END;
  Intr ($10,Regs);
END;

PROCEDURE CursorOn;
BEGIN
  WITH Regs DO
    BEGIN
      AX := $0100;
      CX := $0607;
    END;
  Intr ($10,Regs);
END;

PROCEDURE ZeroScores (L : Integer);
VAR A,B,C : Integer;
BEGIN
  IF L = 0 THEN
    FOR A := 1 TO 5 DO
      ZeroScores (A);
  FOR B := 1 TO 20 DO WITH HighScore [L,B] DO
    BEGIN
      TheScore := 0;
      FOR C := 1 TO 24 DO Name [C] := #0;
    END;
END;

PROCEDURE Initialize;
BEGIN
  TextMode (C40);
  ClrScr;
  {$I-}
  Assign (Data,'MUNCHER.DAT');
  Reset (Data,SizeOf(Screen40));
  BlockRead (Data,MainMenu,1);
  BlockRead (Data,Instructions,1);
  BlockRead (Data,GameBoard,1);
  Close (Data);
  {$I+}
  IF NOT (IOresult = 0) THEN
    BEGIN
      TextMode (C80);
      Writeln ('Bad or missing MUNCHER.DAT');
      Halt;
    END;
  {$I-}
  Assign (Data,'MUNCHER.SCR');
  Reset (Data,SizeOf(Scores));
  ZeroScores (0);
  BlockRead (Data,HighScore,1);
  Close (Data);
  {$I+}
  IF NOT (IOresult = 0) THEN
    BEGIN
      {$I-}
      Close (Data);
      Assign (Data,'MUNCHER.SCR');
      Rewrite (Data,SizeOf(Scores));
      BlockWrite (Data,HighScore,1);
      Close (Data);
      {$I+}
      IF NOT (IOresult = 0) THEN
        BEGIN
          TextMode (C80);
          Writeln ('Disk is full.  Cannot create new MUNCHER.SCR scores file');
          Erase (Data);
          Halt;
        END;
    END;
  CursorOff;
  WantSound := True;
END;

PROCEDURE PrintScores (Level : Integer);
VAR A : Integer;
BEGIN
  ClrScr;
  TextColor (White);
  Writeln ('TOP TWENTY HIGH SCORES TO DATE - LEVEL ',Level);
  GotoXY (1,3);
  FOR A := 1 TO 20 DO
    BEGIN
      TextColor (LightMagenta);
      Write (A:3,'.  ');
      TextColor (LightRed);
      Write (HighScore [Level,A].TheScore:7:0);
      TextColor (White);
      Write ('  ');
      TextColor (Yellow);
      Writeln (HighScore [Level,A].Name);
    END;
END;

PROCEDURE DispHighScores (VAR L : Integer);
VAR
  Level,A : Integer;
  Ch : Char;
BEGIN
  TextColor (White);
  GotoXY (10,25);
  Write ('  WHICH LEVEL (1-5)?  ');
  REPEAT
    Read (KBD,Ch);
  UNTIL Ch IN ['1'..'5'];
  Level := Ord (Ch) - 48;
  PrintScores (Level);
  L := Level;
END;

PROCEDURE DisplayScores;
VAR Ch : Char; L : Integer;
BEGIN
  DispHighScores (L);
  GotoXY (3,25);
  TextColor (White);
  Write ('<<< PRESS ANY KEY TO CONTINUE... >>>');
  ClearKeys;
  Read (KBD,Ch);
  ClearKeys;
END;

PROCEDURE ResetScores;
VAR Ch : Char; L : Integer;
BEGIN
  DispHighScores (L);
  GotoXY (11,25);
  TextColor (White);
  Write ('ARE YOU SURE (Y/N)?');
  ClearKeys;
  REPEAT
    Read (KBD,Ch);
    Ch := UpCase (Ch);
  UNTIL Ch IN ['N','Y'];
  IF Ch = 'Y' THEN ZeroScores (L);
  ClearKeys;
END;

PROCEDURE PlayTheGame (ThePlayer : Player);
TYPE
  Direction = (Stall,Left,Right,Up,Down);
CONST
  MaxGhosts = 8;
  GhostChar = #1;
  NormalGhost = Blue;
  EdibleGhost = Green;
  PlayerChar = #2;
  PlayerColor = Yellow;
  Energizer = #254;
  NormalDot = #249;
  Space = #32;
  StartGhostBonus = 100;
  MaxGhostBonus = 800;
  EnergizerBonus = 100;
  MinCol = 15;
  MaxCol = 25;
  StartRow : ARRAY [0..1] OF Byte = (16,11);
  StartDir : ARRAY [0..1] OF Direction = (Stall,Up);
  Reverse  : ARRAY [Stall..Down] OF Direction = (Stall,Right,Left,Down,Up);
  DirX : ARRAY [Stall..Down] OF Integer = (0,-1,1,0,0);
  DirY : ARRAY [Stall..Down] OF Integer = (0,0,0,-1,1);
  Speeds : ARRAY [1..5] OF Integer = (0,10,30,50,70);
VAR
  NotWall : SET OF #0..#255;
  Score : Real;
  GhostRow,GhostColor,GhostCol : ARRAY [0..MaxGhosts] OF Byte;
  GhostDir : ARRAY [0..MaxGhosts] OF Direction;
  Round,MenLeft,Count20000,GhostBonus,TimeToEat,DotsLeft,DotsInRound,
  Energizers,Speed,Level,NumberOfGhosts,A,Z,ZZ : Integer;
  Ch : Char;
  Waiting : Direction;

  PROCEDURE BackupScreen;
  BEGIN
    Move (Screen,BackUp,SizeOf(Screen));
    Port [$3D4] := 12; Port [$3D5] := Hi (Ofs(Backup) DIV 2);
    Port [$3D4] := 13; Port [$3D5] := Lo (Ofs(Backup) DIV 2);
  END;
  PROCEDURE NormalScreen;
  BEGIN
    Port [$3D4] := 12; Port [$3D5] := 0;
    Port [$3D4] := 13; Port [$3D5] := 0;
  END;

  PROCEDURE Initialize (VAR Ch : Char);
  VAR A,R,C : Byte;
  BEGIN
    Ch := ' ';
    TextColor (40);
    ClrScr;
    GotoXY (8,10);
    TextColor (White);
    NumberOfGhosts := 4;
    IF ThePlayer = Person THEN
      BEGIN
        Writeln ('ENTER SPEED LEVEL (1 to 5)');
        Writeln;
        TextColor (LightRed);
        Writeln ('      1 = for professionals');
        Writeln ('      2 = pretty difficult');
        Writeln ('      3 = mediocre');
        Writeln ('      4 = somewhat easy');
        Writeln ('      5 = for beginners');
        REPEAT
          Read (KBD,Ch);
          CASE Ch OF
            #27 : BEGIN
                    IF KeyPressed THEN
                      BEGIN
                        Read (KBD,Ch);
                        Ch := ' ';
                      END
                    ELSE Exit;
                  END;
            '1'..'5' : Level := Ord (Ch) - 48;
          END;
        UNTIL Ch IN [#27,'1'..'5'];
        ClrScr;
      END
      ELSE Level := 3;
    Speed := Speeds [Level];
    NumberOfGhosts := 6 - (Level DIV 2);
    Move (GameBoard,Screen,SizeOf(Screen));
    Move (Screen,Screen2,SizeOf(Screen));
    NotWall := [GhostChar,PlayerChar,Energizer,NormalDot,Space];
    GhostBonus := StartGhostBonus;
    TimeToEat := 0;
    Score := 0.0;
    Round := 1;
    MenLeft := 2;
    Count20000 := 0;
    GhostRow [0] := StartRow [0];
    GhostCol [0] := 20;
    GhostDir [0] := StartDir [0];
    GhostColor [0] := White;
    FOR A := 1 TO NumberOfGhosts DO
      BEGIN
        GhostRow [A] := StartRow [1];
        GhostCol [A] := MinCol + ((A-1) * (MaxCol - MinCol)) DIV
          (NumberOfGhosts - 1);
        GhostDir [A] := StartDir [1];
        GhostColor [A] := NormalGhost;
      END;
    DotsInRound := 0;
    FOR R := 1 TO 25 DO FOR C := 1 TO 40 DO
      CASE Screen [R,C].Character OF
        NormalDot : DotsInRound := DotsInRound + 1;
        Energizer : Energizers := Energizers + 1;
      END;
    DotsLeft := DotsInRound;
    Waiting := Stall;
  END;

  PROCEDURE DisplayMen;
  VAR T : Integer;
  BEGIN
    GotoXY (15,19);
    Write ('           ');
    GotoXY (15,19);
    T := MenLeft;
    IF T > 6 THEN T := 6;
    TextColor (PlayerColor);
    WHILE T > 0 DO
      BEGIN
        Write (PlayerChar,' ');
        T := T - 1;
      END;
  END;

  PROCEDURE DisplayScore;
  BEGIN
    TextColor (15);
    GotoXY (25,24);
    Write (Score:10:0);
    Move (Screen [22,8],Backup [22,8],14);
  END;
  PROCEDURE DisplayRound;
  BEGIN
    TextColor (15);
    GotoXY (12,24);
    Write (Round);
  END;

  PROCEDURE Ready;
  VAR A : Integer;
  BEGIN
    DisplayScore;
    DisplayRound;
    FOR A := 1 TO NumberOfGhosts DO
      BEGIN
        TextColor (GhostColor [A]);
        GotoXY (GhostCol [A],GhostRow [A]);
        Write (GhostChar);
      END;
    TextColor (PlayerColor);
    GotoXY (GhostCol [0],GhostRow [0]);
    Write (PlayerChar);
    TextColor (White);
    GotoXY (25,22);
    Write (HighScore [Level,1].TheScore:10:0);
    GotoXY (12,22);
    Write (Level);
    TextColor (Green);
    GotoXY (15,19);
    IF Round < 100 THEN Write (' ');
    Write (' ROUND ',Round);
    Delay (1000);
    GotoXY (15,19);
    Write ('  READY!!  ');
    Delay (1000);
    DisplayMen;
    ClearKeys;
    BackupScreen;
  END;

  PROCEDURE AddScore (A : Integer);
  BEGIN
    Score := Score + (A * 1.0);
    Count20000 := Count20000 + A;
    IF Count20000 >= 20000 THEN
      BEGIN
        Count20000 := Count20000 - 20000;
        MenLeft := MenLeft + 1;
        IF MenLeft > 6 THEN MenLeft := 6;
        DisplayMen;
      END;
    DisplayScore;
  END;
  PROCEDURE AteTheDot;
  BEGIN
    Tone (440);
    Delay (Speed);
    AddScore (10);
    DotsLeft := DotsLeft - 1;
    NoSound;
  END;
  PROCEDURE AteTheEnergizer;
  VAR C : Integer;
  BEGIN
    Tone (1317); Delay (100); NoSound;
    AddScore (EnergizerBonus);
    TimeToEat := 125;
    GhostBonus := StartGhostBonus;
    FOR C := 19 TO 21 DO
      Screen2 [11,C].Character := Screen [11,C].Character;
    FOR C := 1 TO NumberOfGhosts DO
      GhostColor [C] := EdibleGhost;
  END;
  PROCEDURE AteTheGhost (Z : Integer);
  VAR F : Integer;
  BEGIN
    AddScore (GhostBonus);
    FOR F := 400 TO 800 DO Tone (F);
    NoSound;
    GotoXY (GhostCol [Z],GhostRow [Z]);
    TextColor (Screen2 [GhostRow [Z],GhostCol [Z]].Attribute);
    Write (Screen2 [GhostRow [Z],GhostCol [Z]].Character);
    GhostBonus := GhostBonus * 2;
    IF GhostBonus > MaxGhostBonus THEN GhostBonus := MaxGhostBonus;
    GhostRow [Z] := StartRow [1];
    GhostCol [Z] := MinCol + ((Z-1) * (MaxCol - MinCol)) DIV
      (NumberOfGhosts - 1);
    GhostDir [Z] := StartDir [1];
    GhostColor [Z] := NormalGhost;
  END;

  FUNCTION YouCanGo (Dir : Direction) : Boolean;
  BEGIN
    YouCanGo := (Screen [GhostRow [0] + DirY [Dir],GhostCol [0] +
      DirX [Dir]].Character IN NotWall);
  END;
  FUNCTION NewDirection (Row,Col : Integer; OldDir : Direction) : Direction;
  VAR
    CanGo : ARRAY [Stall..Down] OF Boolean;
    T,Z  : Direction;
    Flags : Byte;
  BEGIN
    FOR Z := Left TO Down DO
      CanGo [Z] := (Screen [Row + DirY [Z],Col + DirX [Z]].Character
        IN NotWall);
    T := Stall;
    IF (Row = 8) AND (Col IN [19..21]) THEN CanGo [Down]  := False;
    IF (Row = 10) AND (Col = 10)       THEN CanGo [Left]  := False;
    IF (Row = 10) AND (Col = 29)       THEN CanGo [Right] := False;
    IF CanGo [OldDir] THEN T := OldDir;
    Flags := Ord (CanGo [Left]) + (2 * Ord (CanGo [Right])) +
      (4 * Ord (CanGo [Up])) + (8 * Ord (CanGo [Down]));
    IF Flags = 1 THEN T := Left;
    IF Flags = 2 THEN T := Right;
    IF Flags = 4 THEN T := Up;
    IF Flags = 8 THEN T := Down;
    IF (Row = 10) AND (Col IN [19,21]) THEN T := Up;
    IF ((Flags = 3) AND (OldDir IN [Left,Right])) OR
      ((Flags = 12) AND (OldDir IN [Up,Down]))
        THEN T := OldDir;
    IF (T = Stall) AND (NOT (Flags = 0)) THEN
      REPEAT
        T := Direction (Random (4) + 1)
      UNTIL (CanGo [T]) AND (NOT (T = Reverse [OldDir]));
    NewDirection := T;
  END;

  PROCEDURE GameOver;
  VAR A,Rank : Integer; PlayerName : String [24];
  BEGIN
    TextColor (White);
    GotoXY (15,18); Write ('┌─────────┐');
    GotoXY (15,19); Write ('│');
    TextColor (LightBlue);
    Write ('GAME OVER');
    TextColor (White);
    Write ('│');
    GotoXY (15,20); Write ('└─────────┘');
    Delay (3000);
    ClrScr;
    IF (Score < HighScore [Level,20].TheScore) OR (ThePlayer = Computer)
      THEN Exit;
    FOR A := 20 DOWNTO 1 DO
      IF (Score >= HighScore [Level,A].TheScore)
        THEN Rank := A;
    FOR A := 19 DOWNTO Rank DO
      BEGIN
        HighScore [Level,A+1].TheScore := HighScore [Level,A].TheScore;
        HighScore [Level,A+1].Name := HighScore [Level,A].Name;
      END;
    WITH HighScore [Level,Rank] DO
      BEGIN
        TheScore := Score;
        FOR A := 1 TO 24 DO Name [A] := #0;
      END;
    PrintScores (Level);
    GotoXY (4,24);
    TextColor (White);
    Write ('ENTER YOUR NAME AND PRESS <ENTER>.');
    GotoXY (15,Rank + 2);
    Write ('<');
    GotoXY (40,Rank + 2);
    Write ('>');
    TextColor (LightRed);
    GotoXY (16,Rank + 2);
    BufLen := 24;
    CursorOn;
    Readln (PlayerName);
    FOR A := 1 TO Length (PlayerName) DO
      HighScore [Level,Rank].Name [A] := PlayerName [A];
    CursorOff;
  END;

BEGIN
  Initialize (Ch);
  IF Ch = #27 THEN Exit;
  DisplayMen;
  DisplayScore;
  DisplayRound;
  Ready;
  REPEAT
    IF KeyPressed
      THEN Read (KBD,Ch)
      ELSE Ch := ' ';
    IF (ThePlayer = Computer) AND (NOT (Ch = #27)) THEN Ch := ' ';
    CASE UpCase (Ch) OF
      #27 : IF KeyPressed
              THEN
                BEGIN
                  Read (KBD,Ch);
                  IF ThePlayer = Computer THEN Ch := ' ';
                  CASE Ch OF
                    'K' : Waiting := Left;
                    'M' : Waiting := Right;
                    'H' : Waiting := Up;
                    'P' : Waiting := Down;
                  END;
                END
              ELSE
                BEGIN
                  NormalScreen;
                  Exit;
                END;
      '4' : Waiting := Left;
      '6' : Waiting := Right;
      '8' : Waiting := Up;
      '2' : Waiting := Down;
      'S' : WantSound := NOT WantSound;
    END;
    FOR A := 0 TO NumberOfGhosts DO
      BEGIN
        GotoXY (GhostCol [A],GhostRow [A]);
        TextColor (Screen2 [GhostRow [A],GhostCol [A]].Attribute);
        Write (Screen2 [GhostRow [A],GhostCol [A]].Character);
      END;
    FOR A := 1 TO NumberOfGhosts DO
      BEGIN
        Z := Integer (NewDirection (GhostRow [A],GhostCol [A],GhostDir [A]));
        IF NOT (((GhostColor [A] AND $0F) = EdibleGhost) AND ((TimeToEat AND 1) = 1)) THEN
          BEGIN
            GhostDir [A] := Direction (Z);
            GhostCol [A] := GhostCol [A] + DirX [GhostDir [A]];
            GhostRow [A] := GhostRow [A] + DirY [GhostDir [A]];
          END;
      END;
    IF ThePlayer = Computer THEN Waiting := Stall;
    IF (YouCanGo (Waiting)) AND (NOT (Waiting = Stall)) THEN
      BEGIN
        GhostDir [0] := Waiting;
        Waiting := Stall;
      END;
    If ThePlayer = Computer THEN GhostDir [0] := NewDirection (GhostRow [0],
      GhostCol [0],GhostDir [0]);
    IF NOT YouCanGo (GhostDir [0]) THEN GhostDir [0] := Stall;
    GhostCol [0] := GhostCol [0] + DirX [GhostDir [0]];
    GhostRow [0] := GhostRow [0] + DirY [GhostDir [0]];
    IF GhostCol [0] = 0  THEN GhostCol [0] := 39;
    IF GhostCol [0] = 40 THEN GhostCol [0] := 1;
    IF TimeToEat = 40 THEN
      FOR A := 1 TO NumberOfGhosts DO
        IF GhostColor [A] = EdibleGhost
          THEN GhostColor [A] := EdibleGhost + 16;
    FOR A := 1 TO NumberOfGhosts DO
      BEGIN
        TextColor (GhostColor [A]);
        GotoXY (GhostCol [A],GhostRow [A]);
        Write (GhostChar);
      END;
    TextColor (PlayerColor);
    GotoXY (GhostCol [0],GhostRow [0]);
    Write (PlayerChar);
    BackupScreen;
    IF Screen2 [GhostRow [0],GhostCol [0]].Character = NormalDot THEN
      BEGIN
        Screen2 [GhostRow [0],GhostCol [0]].Character := Space;
        AteTheDot;
      END
    ELSE
      BEGIN
        Delay (Speed);
        AddScore (0);
      END;
    IF Screen2 [GhostRow [0],GhostCol [0]].Character = Energizer THEN
      BEGIN
        Screen2 [GhostRow [0],GhostCol [0]].Character := Space;
        AteTheEnergizer;
      END;
    FOR A := 1 TO NumberOfGhosts DO
      BEGIN
        IF (Abs (GhostRow [0] - GhostRow [A]) + Abs (GhostCol [0] -
          GhostCol [A]) <= 1)
          THEN
            BEGIN
              NormalScreen;
              IF GhostColor [A] = NormalGhost THEN
                BEGIN
                  FOR ZZ := 20 DOWNTO 15 DO
                    FOR Z := ZZ*25 DOWNTO ZZ*25-300 DO Tone (Z*2);
                  NoSound;
                  Delay (1000);
                  GhostRow [0] := StartRow [0];
                  GhostCol [0] := 20;
                  GhostDir [0] := StartDir [0];
                  FOR Z := 1 TO NumberOfGhosts DO
                    BEGIN
                      GhostRow [Z] := StartRow [1];
                      GhostCol [Z] := MinCol + ((Z-1) * (MaxCol - MinCol))
                        DIV (NumberOfGhosts - 1);
                      GhostDir [Z] := StartDir [1];
                      GhostColor [Z] := NormalGhost;
                    END;
                  MenLeft := MenLeft - 1;
                  IF MenLeft = -1 THEN
                    BEGIN
                      GameOver;
                      Exit;
                    END;
                  Move (Screen2,Screen,SizeOf(Screen));
                  DisplayMen;
                  TimeToEat := 0;
                  DisplayScore;
                  DisplayRound;
                  GhostBonus := StartGhostBonus;
                  TimeToEat := 0;
                  Ready;
                END
                ELSE AteTheGhost (A);
            END;
      END;
    IF TimeToEat > 0
      THEN
        BEGIN
          TimeToEat := TimeToEat - 1;
          IF TimeToEat = 0 THEN
            BEGIN
              FOR A := 19 TO 21 DO
                Screen2 [11,A].Character := Screen [11,A].Character;
              GhostBonus := StartGhostBonus;
              FOR A := 1 TO NumberOfGhosts DO
                GhostColor [A] := NormalGhost;
            END;
        END
      ELSE GhostBonus := StartGhostBonus;
    IF DotsLeft = 0 THEN
      BEGIN
        NormalScreen;
        DotsLeft := DotsInRound;
        Waiting := Stall;
        TimeToEat := 0;
        TextColor (Green);
        GotoXY (15,19);
        IF Round < 100 THEN Write (' ');
        Write (' ROUND ',Round);
        Delay (1000);
        GotoXY (15,19);
        Write ('COMPLETED!!');
        Delay (1000);
        Move (GameBoard,Screen,SizeOf(Screen));
        Move (GameBoard,Screen2,SizeOf(Screen));
        GhostRow [0] := StartRow [0];
        GhostCol [0] := 20;
        GhostDir [0] := StartDir [0];
        Round := Round + 1;
        IF (Round MOD 2) = 1 THEN
          BEGIN
            NumberOfGhosts := NumberOfGhosts + 1;
            IF NumberOfGhosts > MaxGhosts
              THEN NumberOfGhosts := MaxGhosts;
          END;
        FOR A := 1 TO NumberOfGhosts DO
          BEGIN
            GhostRow [A] := StartRow [1];
            GhostCol [A] := MinCol + ((A-1) * (MaxCol - MinCol)) DIV
              (NumberOfGhosts - 1);
            GhostDir [A] := StartDir [1];
            GhostColor [A] := NormalGhost;
          END;
        Ready;
      END;
  UNTIL False;
END;

VAR
  Ch  : Char;
  R   : Byte;
  Z,Y : Integer;
BEGIN
  Initialize;
  Z := 0;
  REPEAT
    Move (MainMenu,Screen,SizeOf(Screen));
    REPEAT
      Y := Screen [3,39].CharAttr;
      Move (Screen [3,2],Screen [3,3],74);
      Screen [3,2].CharAttr := Y;
      IF (Z MOD 2) = 0 THEN For R := 6 TO 8 DO
        BEGIN
          Y := Screen [R,4].CharAttr;
          Move (Screen [R,5],Screen [R,4],66);
          Screen [R,37].CharAttr := Y;
        END;
      Delay (50);
      Ch := ' ';
      IF KeyPressed THEN Read (KBD,Ch);
      IF (Ch = #27) AND KeyPressed THEN
        BEGIN
          Read (KBD,Ch);
          Ch := ' ';
        END;
      Ch := UpCase (Ch);
      Z := Z + 1;
      IF Z = 300 THEN
        BEGIN
          PlayTheGame (Computer);
          Move (MainMenu,Screen,SizeOf(Screen));
          Z := 0;
        END;
    UNTIL Ch IN [#27,'H','I','P','R'];
    CASE Ch OF
      #27 : BEGIN
              Assign (Data,'MUNCHER.SCR');
              Rewrite (Data,SizeOf (Scores));
              BlockWrite (Data,HighScore,1);
              Close (Data);
              TextMode (C80);
              ClrScr;
              CursorOn;
              Halt;
            END;
      'R' : BEGIN
              Z := 0;
              ResetScores;
              Move (MainMenu,Screen,SizeOf(Screen));
            END;
      'H' : BEGIN
              Z := 0;
              DisplayScores;
              Move (MainMenu,Screen,SizeOf(Screen));
            END;
      'I' : BEGIN
              Z := 0;
              Move (Instructions,Screen,SizeOf(Screen));
              Read (KBD,Ch);
              IF (Ch = #27) AND KeyPressed THEN Read (KBD,Ch);
              Ch := ' ';
            END;
      'P' : BEGIN
              Z := 0;
              PlayTheGame (Person);
            END;
    END;
  UNTIL FALSE;
END.
