// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

(MAINLOOP)
  @KBD
  D=M
  @PAINT
  D;JNE
  @CLEAR
  0;JMP

(PAINT)
  @i
  M=0
  (PAINTFILL)
    @SCREEN
    D=A
    @i
    D=D+M
    A=D
    M=-1
    @i
    M=M+1
    @8192
    D=A
    @i
    D=D-M
    @PAINTFILL
    D;JGT
  @MAINLOOP
  0;JMP

(CLEAR)
  @i
  M=0
  (PAINTCLEAR)
    @SCREEN
    D=A
    @i
    D=D+M
    A=D
    M=0
    @i
    M=M+1
    @8192
    D=A
    @i
    D=D-M
    @PAINTCLEAR
    D;JGT
  @MAINLOOP
  0;JMP
