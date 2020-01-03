(*********************************************************************************************)

(*  MCONFIG.PAS -- by Darren Embry -- Saturday, October 12, 1991
    This is the configuration program for Mr. Muncher.  *)

(*********************************************************************************************)

PROGRAM Configuration (Input,Output);

{ -- The following line should be included for Turbo Pascal version 4.0 and above ONLY.
     For Turbo Pascal 3.0, you must insert the symbols (* and *) (or curly brackets as
     used in these comments) to skip this line. -- }

(* USES CRT,Graph3; *)

{$I DATATYPE.P } { contains data types }
{$I FILENAME.P } { contains file names }

VAR
{ variable that stores configuration settings }
  Configuration : ConfigType;
{ data file used for reading and writing setup file }
  DataFile : File;
{ temporary storage for speed; used for INLINE routine }
  Speed : Integer;

PROCEDURE InvalidSelection;
{ informs user that he requested an invalid number }
BEGIN
  Writeln;
  Writeln;
  Writeln ('You requested an invalid number.');
  Write ('Press <Enter> to continue.');
  Readln;
END;

PROCEDURE Fade (Color : Byte; Seed : Integer);
{ A nifty version of FillScreen.  It kind of "fades."  Seed is important if you can understand
  assembly language, but it can be an odd number between 1 and 16,383. }
VAR Speed : Integer;
BEGIN
  Speed := Configuration.FadeSpeed + 1;
  inline ($8B/$9E/Seed/    (*     mov bx,Seed [bp]       *)
          $80/$CB/$01/     (*     or bl,1                *)
          $8A/$86/Color/   (*     mov al,Color [bp]      *)
          $24/$03/         (*     and al,3               *)
          $88/$C4/         (*     mov ah,al              *)
          $B1/$02/         (*     mov cl,2               *)
          $D2/$E4/         (*     shl ah,cl              *)
          $08/$E0/         (*     or al,ah               *)
          $88/$C4/         (*     mov ah,al              *)
          $B1/$04/         (*     mov cl,4               *)
          $D2/$E4/         (*     shl ah,cl              *)
          $08/$E0/         (*     or al,ah               *)
          $1E/             (*     push ds                *)
          $BA/>$B800/      (*     mov dx,0B800h          *)
          $8E/$DA/         (*     mov ds,dx              *)
          $B9/>16384/      (*     mov cx,16384 (decimal) *)
          $31/$F6/         (*     xor si,si              *)
          $88/$04/         (* l1: mov byte ptr [si],al   *)
          $01/$DE/         (*     add si,bx              *)
          $81/$E6/>$3FFF/  (*     and si,03FFFh          *)
          $51/             (*     push cx                *)
          $8B/$8E/Speed/   (*     mov cx,Speed [BP]      *)
          $E2/$FE/         (* l2: loop l2                *)
          $59/             (*     pop cx                 *)
          $E2/$EE/         (* l3: loop l1                *)
          $1F              (*     pop ds                 *)
          );
END;

PROCEDURE FadeScreen (Color : Byte);
{ This procedure calls procedure Fade using a randomly generated seed }
BEGIN
  Fade (Color, Random (16380) + 2);
END;

PROCEDURE ChangeFadeSettings;
{ menu that allows you to change settings for screen fades }
VAR Choice : Byte; { number of choice selected from menu }
BEGIN
  REPEAT
    GraphColorMode;
    GotoXY (1,25);
    Write ('Fade speed = ',Configuration.FadeSpeed);
    GotoXY (1,1);
    Writeln ('SCREEN FADE CONFIGURATION');
    Writeln;
    Writeln ('This main menu option allows you to');
    Writeln ('set the speed for screen fades.');
    Writeln;
    Writeln ('1.  Change the fading speed.');
    Writeln ('2.  Test out a screen fade at the set');
    Writeln ('    speed.');
    Writeln ('9.  Return to main menu.');
    Writeln;
    Writeln ('Type in the number corresponding to');
    Write ('your selection and press <Enter>:  ');
    Readln (Choice);
    CASE Choice OF
      1 : BEGIN
            GraphColorMode;
            Writeln ('CHANGE SCREEN FADE SPEED');
            Writeln;
            Writeln ('This option allows you to change the');
            Writeln ('speed of screen fading.  Higher numbers');
            Writeln ('result in slower fades.');
            Writeln;
            Writeln ('The fade speed is ',Configuration.FadeSpeed);
            Writeln;
            Write ('Enter new speed, from 0 to 32767:  ');
            Readln (Configuration.FadeSpeed);
          END;
      2 : BEGIN
            FadeScreen (2);
            FadeScreen (0);
          END;
    END;
    IF NOT (Choice IN [1,2,9])
      THEN InvalidSelection;
  UNTIL Choice = 9;
END;

VAR _6845 : Integer ABSOLUTE $0000:$0463;
  { port address of graphics adapter (example: $03D4 for CGA) }

PROCEDURE WaitForRetrace;
{ -- wait until retrace; we can update the video display -- }
BEGIN
  WHILE (Port [_6845+6] AND 8) = 1 DO; WHILE (Port [_6845+6] AND 8) = 0 DO;
END;

PROCEDURE SeeLines (L : Integer);
{ -- a specified number of lines can be displayed on the screen.  Multiply
     L by two to get the number of lines. -- }
VAR P : Integer;
BEGIN
  FOR P := 1 TO Configuration.ScrollSpeed DO WaitForRetrace;
  P := (100 - L) * 40; Port [_6845] := 6; Port [_6845+1] := L;
  PortW [_6845] := $0C OR (P AND $FF00);
  PortW [_6845] := $0D OR ((P SHL 8) AND $FF00);
END;

PROCEDURE ScrollDown; VAR L : Integer;
{ -- smooth scrolling down of the graphics screen currently displayed -- }
BEGIN FOR L := 0 TO 100 DO SeeLines (L); END;

PROCEDURE ScrollUp; VAR L : Integer;
{ -- smoothly scroll the screen up until you see nothingness -- }
BEGIN FOR L := 100 DOWNTO 0 DO SeeLines (L); END;

PROCEDURE ChangeScrollSettings;
{ menu that allows you to change settings for screen scrolling }
VAR Choice : Byte; { number of choice selected from menu }
BEGIN
  REPEAT
    GraphColorMode;
    GotoXY (1,24);
    Writeln ('Scroll speed = ',Configuration.ScrollSpeed);
    Write ('Scroll is ');
    IF Configuration.ScrollWorks
      THEN Write ('ON') ELSE Write ('OFF');
    GotoXY (1,1);
    Writeln ('SCREEN SCROLLING CONFIGURATION');
    Writeln;
    Writeln ('This main menu option allows you to');
    Writeln ('change the settings for screen scrolls.');
    Writeln;
    Writeln ('1.  Change the scrolling speed.');
    Writeln ('2.  Test out a screen scroll at the set');
    Writeln ('    speed.  This also allows you to');
    Writeln ('    tell the program whether to use the');
    Writeln ('    use the scrolling or not.');
    Writeln ('9.  Return to main menu.');
    Writeln;
    Writeln ('Type in the number corresponding to');
    Write ('your selection and press <Enter>:  ');
    Readln (Choice);
    CASE Choice OF
      1 : BEGIN
            GraphColorMode;
            Writeln ('CHANGE SCREEN SCROLL SPEED');
            Writeln;
            Writeln ('This option allows you to change the');
            Writeln ('speed of screen scrolls.  Higher');
            Writeln ('numbers result in slower scrolls.');
            Writeln;
            Writeln ('The scroll speed is ',Configuration.ScrollSpeed);
            Writeln;
            Write ('Enter new speed, from 0 to 32767:  ');
            Readln (Configuration.ScrollSpeed);
          END;
      2 : BEGIN
            ScrollUp;
            FadeScreen (0);
            GotoXY (1,1);
            SeeLines (100);
            Writeln ('1.  The scrolling up alone cleared the');
            Writeln ('    screen.  There was no "fade."');
            Writeln ('2.  The screen scrolled up, but some');
            Writeln ('    of the information was still up');
            Writeln ('    there.  The screen had to be');
            Writeln ('    "faded." in order for it to be');
            Writeln ('    cleared.');
            Writeln;
            Writeln ('Type in the number that goes with your');
            Write ('response and press <Enter>:  ');
            Readln (Choice);
            CASE Choice OF
              1 : Configuration.ScrollWorks := TRUE;
              2 : Configuration.ScrollWorks := FALSE;
            END;
          END;
    END;
    IF NOT (Choice IN [1,2,9])
      THEN InvalidSelection;
  UNTIL Choice = 9;
END;

PROCEDURE SaveConfigurationSettings;
{ Saves the configuration settings to a disk file }
BEGIN
  Assign (DataFile,ConfigurationFile);
  Rewrite (DataFile,SizeOf (Configuration));
  BlockWrite (DataFile,Configuration,1);
  Close (DataFile);
END;

PROCEDURE LoadConfigurationSettings;
{ loads the configuration settings from the disk file }
BEGIN
  WITH Configuration DO
    BEGIN
      FadeSpeed := 0;
      ScrollWorks := TRUE;
      ScrollSpeed := 2;
      AnimationSpeed := 0;
      Smoothness := 2;
      configuration.soundeffects := FALSE;
    END;
  {$I-}
  Assign (DataFile,ConfigurationFile);
  Reset (DataFile,SizeOf (Configuration));
  IF IOresult = 0
    THEN
      BEGIN
        {$I+}
        BlockRead (DataFile,Configuration,1);
        Close (DataFile);
      END
    ELSE
      BEGIN
        {$I+}

        SaveConfigurationSettings;
      END;
END;

PROCEDURE SoundEffectsSwitch;
VAR Choice : Integer;
BEGIN
  TextMode (C80);
  TextColor (LightGray);
  TextBackground (Black);
  ClrScr;
  Writeln ('1.  Turn the sound ON.');
  Writeln ('2.  Turn the sound OFF.');
  Writeln;
  Writeln ('Type in the number corresponding to your selection and press <Enter>:  ');
  Readln (Choice);
  CASE Choice OF
    1 : configuration.soundeffects := TRUE;
    ELSE configuration.soundeffects := FALSE;
  END;
END;

PROCEDURE MainMenu;
{ prints out the main menu selections and gets input from user }
VAR Choice : Byte; { number of choice selected from main menu }
BEGIN
  LoadConfigurationSettings;
  REPEAT
    TextMode (C80);
    TextColor (LightGray);
    TextBackground (Black);
    ClrScr;
    Writeln ('MR. MUNCHER -- Configuration Program -- Version 1.0');
    Writeln ('Copyright (C) 1991 by Darren Embry');
    Writeln;
    Writeln ('This program allows you to change the settings for this ööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööööö