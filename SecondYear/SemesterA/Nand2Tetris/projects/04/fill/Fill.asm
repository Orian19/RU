// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen
// by writing 'black' in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen by writing
// 'white' in every pixel;
// the screen should remain fully clear as long as no key is pressed.


// keeping number of rows to paint at R0
@8192
D=A
@R0
M=D
(LOOP)
    // initializing i to be counter for the rows
    @i
    M=0
    // getting the input from the keyboard and jumping to a loop to paint the rows.
    @KBD
    D=M
    // if the value is 0 then non of the keys are pressed = paint the screen white
    @WHITE
    D;JEQ
    // if the value is greater than 0 one of the keys is pressed = paint the screen black
    @BLACK
    D;JGT
    (WHITE)
        // going over R0 times by comparing i
        @i
        D=M
        @R0
        D=D-M
        @LOOP
        D;JEQ
        @i
        D=M
        // getting the address of RAM[SCREEN + i]
        @SCREEN
        A=D+A
        // changing the row to 0 will paint that row  - white
        M=0
        @i
        M=M+1        
        @WHITE
        0;JMP
    (BLACK)
        // going over R0 times by comparing i
        @i
        D=M
        @R0
        D=D-M
        @LOOP
        D;JEQ
        @i
        D=M
        @SCREEN
        A=D+A
        // changing the row to -1 will paint that row  - black
        M=-1
        @i
        M=M+1        
        @BLACK
        0;JMP
