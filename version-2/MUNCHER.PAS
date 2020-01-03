(*********************************************************************************************)

{   MR. MUNCHER -- Version 2.00 -- (C) 1991 by EmbryWare Enterprises.

    MUNCHER.PAS -- by Darren Embry -- Wednesday, February 13, 1991

    This is a video game much like Pac-Man.  This game also contains
    a board editor to customize your own boards.

    This code has been designed to be compatible with both Version 3.0 and
    Version 4.0/later of Borland's Turbo pascal.  To switch between the two,
    minor changes must be made, which are documented below... }

{$I-}

{ These lines are to be added only for version 3.0 of Turbo Pascal.
  If version 4.0 or greater is being used, the symbols (* and *) can be
  inserted in order to skip these lines.  Curly brackets (used for this
  comment) cannot be used.  If you don't know why, then see the Turbo
  Pascal Reference Guide! }

(*
{$I TURBO4.P }    { procedures and functions for compatibility with Turbo 4.0 }
{$I GRAPH.P }     { Turbo 3.0 extended graphics procedures and functions }
*)

{ This line is to be added for version 4.0 or above of Turbo Pascal.
  If version 3.0 is being used, the symbols (* and *) can be inserted
  in order to skip this line.  Curly brackets can be used also. }

USES Crt,Dos,Graph3;

{ ------------- }

{$I FILENAME.P }  { stores names of data files for this program }
{$I DATATYPE.P }  { data types for this program and its accessories }

VAR
  Icons : IconsRecord; Wait : Integer;
  TopTen : ARRAY [1..10] OF ScoreRecord;
  WindowX1,WindowY1,WindowX2,WindowY2 : Integer;
  Configuration : ConfigType;

{$I SCROLLS.P }   { procedures/functions for smooth scrolling }
{$I INCLUDES.P }  { procedures/functions used by this program and accessories }
{$I INCLUDE2.P }  { procedures/functions used exclusively by this program }
{$I INPUT.P }     { string and number input functions used by board editor and game }
{$I EDITOR.P }    { source code for the game board editor }
{$I PAUSE.P }     { pause function for the video game }
{$I GAME.P }      { source code for the game code itself }

VAR
{ code of option selected from main menu }
  Option : Integer;
{ least significant byte of system clock timer count, increments 18.2 times a second }
  Timer : Byte ABSOLUTE $0040:$006C;
  C : Integer; { loop control variable }

PROCEDURE SharewareScreen2;
BEGIN
  TextMode (CO80);
  TextColor (Yellow);
  GotoXY (16,2);
  Writeln ('--- THIS IS A BETA TEST VERSION OF MR. MUNCHER ---');
  Writeln;
  TextColor (LightRed);
  Writeln ('The full version (coming soon) will have 80 fun-packed rounds of action.');
  Writeln ('But I need your help!  I need you guys to create some really hard boards');
  Writeln ('for me.  Send me your ideas (naturally, you have to draw them on graph');
  Writeln ('paper or something) to me at:');
  Writeln;
  TextColor (LIghtBlue);
  Writeln ('        Darren Embry                   don''t forget to send your');
  Writeln ('        2404 Portland Avenue           bugs,suggestions,comments,etc.');
  Writeln ('        Louisville, KY 40212-1038      here, too (I always stress this)');
  Writeln;
  TextColor (LightRed);
  Writeln;
  Writeln ('The best ideas you send me will be used in future editions of this game!');
  Writeln ('Thank you for your cooperation in helping build a great video game!');
  Writeln;
END;

BEGIN { --- MAIN PROGRAM --- }
{ initialize graphics window coordinates (saved in FillRegion, which is used frequently) }
  WindowX1 := 0; WindowY1 := 0; WindowX2 := 319; WindowY2 := 199;
{ initialize random number generator according to Timer count }
  C := Timer; REPEAT Randomize; C := C - 1; UNTIL C = 0;
{ initialize }
  Initialize; IntroductionScreen;
  REPEAT
  { input menu selection from user }
    MainMenu (Option);
    CASE Option OF
      1 : BEGIN
            SharewareScreen;
            SharewareScreen2;
          END;
      2 : PlayTheGame;
      3 : InstructionScreen;
      4 : BoardEditor;
      5 : SeeHighScores;
      6 : ResetHighScores;
    END;
  UNTIL Option = 1; { 1 is the code for "Exit to DOS" }
END.

(* END OF MUNCHER.PAS *)
