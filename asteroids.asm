;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; Asteroids game for  zx81 by Adrian Pilkington
;;; "dodge the flying rocks!!!"
;;;;;;;;;;;;;;;;;;;;;

#include "zx81defs.asm" ;; https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
;EQUs for ROM routines
#include "zx81rom.asm"
;ZX81 char codes/how to survive without ASCII
#include "charcodes.asm"
;system variables
#include "zx81sys.asm"

;the standard REM statement that will contain our 'hex' code
#include "line1.asm"

; these variables need converting to screen addresses for zx81
; problem with zx81 is the screen display D_FILE memory address changes with size of basic program 
; see https://www.sinclairzxworld.com/viewtopic.php?t=3919
; (the asm here is converted to one line of basic)
#define ROWS_IN_SCREEN 24
#define COL_IN_SCREEN 32
#define SPACESHIP_SCREEN_MEM_START_OFFSET 709
#define RANDOM_BYTES_MEM_LOCATION 2000
;((32*23)-1)
#define SCREEN_SCROLL_MEM_OFFSET 693


;D_FILE is location of screen memory (which moves depending on length of basic, but should be fixed after program is loaded
; probably should run some code to detect if this is 1K or 16K as well, or just have 2 verisons 1K and 16K
#define D_FILE 16396
;black block
#define SPACESHIP_CHARACTER_CODE 128  
;blank space
#define NOT_SPACESHIP_CHARACTER_CODE 0
;blank space
#define NOT_ASTEROID_CHARACTER_CODE 0
;black grey block
#define ASTEROID_CHARACTER_CODE 4
#define GREY_SQAURE 8  

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
#define KEYBOARD_READ_PORT_A_TO_G	$FD
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

	jp setHighScoreZero

var_Spaceship_pos 
	DEFB 0,0
var_scroll_screen_from
	DEFB 0,0
var_scroll_screen_to
	DEFB 0,0
to_print_mem
	DEFB 0,0
crash_message_txt
	DEFB	_G,_A,_M,_E,__,_O,_V,_E,_R,$ff	
title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_A,_S,_T,_E,_R,_O,_I,_D,_S,__,__,$ff
keys_screen_txt
	DEFB	_S,__,_T,_O,__,_S,_T,_A,_R,_T,26,__,_Z,__,_L,_E,_F,_T,26,__,_M,__,_R,_I,_G,_H,_T,$ff
last_Score_txt
	DEFB	21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff	
high_Score_txt
	DEFB	21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff		
chequrered_flag		
	DEFB	4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,$ff		
initAsteroidCount		
	DEFB	0,$ff			
score_mem_tens
	DEFB 0
score_mem_hund
	DEFB 0
score_mem_thou
	DEFB 0
high_score_mem_tens
	DEFB 0
high_score_mem_hund
	DEFB 0		
last_score_mem_tens
	DEFB 0
last_score_mem_hund
	DEFB 0			
speedUpLevelCounter	
	DEFB 0,0
initialSpaceshipLeftCountDown
	DEFB 0,0
credits_and_version_1
	DEFB _B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,$ff
credits_and_version_2
	DEFB __,__,__,__,__,__,_2,_0,_2,_2,__,__,__,__,__,$ff
randomBytes	
	DEFB 0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0
indexToRandom
	DEFB 0,0
to_print .equ to_print_mem ;use printByte16

;; note on the zx81 display 
; from previous crashes and experimenting with printing characters to the screen;; 
; and also some forums, it's clear that the zx81 has 32 column* 24 rows of printable/addressable
; screen area, but at the end of each row is is a chr$127, which if overritten
;; can cazuse unpredictable behavoir and system instabiltiy . It also menas calculating 
;; addresses/offsets to print to is not as straightforward as say c64
;; printing to very last column and row, is 32col * 24row + (24"end of lines" - 1)
;; printing to [row][col], use (row * 33) + col, 
;; (row is 0 to 23 for addressing purposes, and column 1 to 32)
;;
;; 1k is different to 16K, on 1K system saves space by putting "end of row markers" chr$127
;; on every line until there is something else on it. 16K preallocates whole display
;; 16K zx81 offsets from D_FILE
;; 1  = top row, first column 
;; 32 = top right, last column
;; 760 = bottom row, first column
;; 791 = bottom row, last column
	
;set b to row, c to col	
printByte 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	PUSH AF ;store the original value of A for later
	
	CALL PRINTAT ;
	POP AF 
	PUSH AF ;store the original value of A for later
	AND $F0 ; isolate the first digit
	RRA
	RRA
	RRA
	RRA
	ADD A,$1C ; add 28 to the character code
	CALL PRINT
	POP AF ; retrieve original value of A
	AND $0F ; isolate the second digit
	ADD A,$1C ; add 28 to the character code
	CALL PRINT
	RET

introWaitLoop
	ld bc,$00ff ;max waiting time
introWaitLoop_1
	dec bc
	ld a,b
	or c
	jr nz, introWaitLoop_1
	jp read_start_key
	
	
setHighScoreZero
	ld a, 0
	ld (high_score_mem_tens), a
	ld (high_score_mem_hund), a
	ld (last_score_mem_tens), a
	ld (last_score_mem_hund), a	
	
	
intro_title
	call CLS	
	ld bc,1
	ld de,chequrered_flag
	call printstring	
	ld bc,34
	ld de,chequrered_flag
	call printstring		
	ld bc,110
	ld de,title_screen_txt
	call printstring
	ld bc,202
	ld de,keys_screen_txt
	call printstring	
	;ld bc,337
	;ld de,high_Score_txt
	;call printstring	
	;ld b, 11			; b is row to print in
	;ld c, 13			; c is column
    ;ld a, (high_score_mem_hund) ; load hundreds
	;call printByte    
	;ld b, 11			; b is row to print in
	;ld c, 15			; c is column
	;ld a, (high_score_mem_tens) ; load tens		
	;call printByte	
	ld bc,436
	ld de,last_Score_txt
	call printstring	
	ld b, 14			; b is row to print in
	ld c, 13			; c is column
    ld a, (last_score_mem_hund) ; load hundreds
	call printByte    
	ld b, 14			; b is row to print in
	ld c, 15			; c is column
	ld a, (last_score_mem_tens) ; load tens		
	call printByte	

	ld bc,537	
	ld de,credits_and_version_1
	call printstring		
	ld bc,569	
	ld de,credits_and_version_2
	call printstring	
	
	ld bc,727
	ld de,chequrered_flag
	call printstring		
	ld bc,760
	ld de,chequrered_flag
	call printstring	

read_start_key
	ld a, KEYBOARD_READ_PORT_A_TO_G	
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop

main
	call CLS
	ld a, 7
	ld (initialSpaceshipLeftCountDown),a
	
	ld a, 0						; initialise score to zero
	ld (score_mem_tens),a
	ld a, 0						; initialise score to zero
	ld (score_mem_hund),a
	ld a, 0						; initialise score to zero
	ld (score_mem_thou),a	

	ld bc, $0aff					; set initial difficulty
	ld (speedUpLevelCounter), bc
	ld bc,0
	
	;; initialise the scroll from and too, 
	;; scroll from is the D_FILE+(cols*(rows-1)-1
	;; scroll to is the D_FILE + (cols*rows)-1     (= scroll from + 32)
	ld hl,(D_FILE) ;initialise screen start memory address
	ld de, SCREEN_SCROLL_MEM_OFFSET
	add hl, de	
	ld (var_scroll_screen_from), hl
	ld de, 33
	add hl, de
	ld (var_scroll_screen_to), hl

	ld b, 30 ; numnber of columns to init
	ld hl,(D_FILE) ;initialise space start memory address (just top left)
	inc hl
	ld (initAsteroidCount), hl
	ld hl, 10
	ld de,randomBytes
	add hl, de
	ld (indexToRandom), hl
	ld de, (indexToRandom)
	
initialiseAsteroidField  
	ld a, (de)
	inc de
    and %00000001
	jp nz, placeAsteroid
	;if not zero place free space
	ld a, NOT_ASTEROID_CHARACTER_CODE
	ld hl,(initAsteroidCount)
	ld (hl),a    			; asteroids starts as random line of character
	jp noRoid
	
placeAsteroid	
	ld hl,(initAsteroidCount)
	ld a, ASTEROID_CHARACTER_CODE	
	ld (hl),a    			; asteroids starts as random line of character
	
noRoid	
	inc hl
	ld (initAsteroidCount), hl
	djnz initialiseAsteroidField	
	
	
	;;;;;;;;;;;;;;;initialise Spaceship	
	ld hl,(D_FILE) 
	ld de, SPACESHIP_SCREEN_MEM_START_OFFSET
	add hl, de	
	ld a,SPACESHIP_CHARACTER_CODE 
	ld (hl),a
	ld (var_Spaceship_pos),hl ;save Spaceship posn
	
principalloop
	ld hl, (var_Spaceship_pos)						; load Spaceship position into hl
	ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			; read keyboard shift to v
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 1, a
	; check bit set for key press left  (Z)
	jp z, Spaceshipleft								; jump to move Spaceship left
	ld a, KEYBOARD_READ_PORT_SPACE_TO_B			; read keyboard space to B
	in a, (KEYBOARD_READ_PORT)					; read from io port		
	bit 2, a									; check bit set for key press right (M)
	jr z, Spaceshipright								; jump to move Spaceship right	
	jp noSpaceshipMove								; dropped through to no move
Spaceshipleft
	dec hl	
	jp noSpaceshipMove	
Spaceshipright
	inc hl
noSpaceshipMove		
	ld (var_Spaceship_pos), hl		
	xor a  ;set Spaceshipry flag to 0
	ld de, 32 
	sbc hl,de
	ld a,(hl)
	or a
	jp nz,gameover
	
	ld a, SPACESHIP_CHARACTER_CODE
	ld (hl),a
	
	
	;scroll screen	
	ld hl,(var_scroll_screen_from)  ; screen left address	
	ld de,(var_scroll_screen_to) ; screen right address		
	ld bc,694 ;736 = 32columns * 23 rows
	; LDDR repeats the instruction LDD (Does a LD (DE),(HL) and decrements 
	; each of DE, HL, and BC) until BC=0. Note that if BC=0 before 
	; the start of the routine, it will try loop around until BC=0 again.	
	lddr

	
	ld b, 30 ; numnber of columns to init
	ld hl,(D_FILE) ;initialise space start memory address (just top left)
	inc hl
	ld (initAsteroidCount), hl	
generateNewAsteroidLine  
	call random
	;jr nz, placeAsteroid2
	;if not zero place free space
	ld de, randomBytes
	ld l, a
	ld h, 0
	add hl, de
	ld a, (hl)
	and 1
	jr nz, placeAsteroid2
	ld a, NOT_ASTEROID_CHARACTER_CODE
	ld hl,(initAsteroidCount)
	ld (hl),a    			; asteroids starts as random line of character
	jp noRoid2
	
placeAsteroid2	
	ld hl,(initAsteroidCount)
	ld a, ASTEROID_CHARACTER_CODE	
	ld (hl),a    			; asteroids starts as random line of character
	
noRoid2	
	inc hl
	ld (initAsteroidCount), hl	
	djnz generateNewAsteroidLine	
	
	
preWaitloop
	ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
	add a,1	
	daa									; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_tens),a	
	cp 153
	jr z, addOneToHund
	jr skipAddHund
addOneToHund
	ld a, 0
	ld (score_mem_tens), a
    ld a, (score_mem_hund)
	add a, 1
	daa
	ld (score_mem_hund), a
skipAddHund	

printScoreInGame
	ld b, 21			; b is row to print in
	ld c, 1			; c is column
    ld a, (score_mem_hund) ; load hundreds
	call printByte    
	ld b, 21			; b is row to print in
	ld c, 3			; c is column
	ld a, (score_mem_tens) ; load tens		
	call printByte

	ld bc, (speedUpLevelCounter)
	ld hl, (speedUpLevelCounter)   ; makes it more difficult as you progress
	ld a, h
	cp 0
	jr z, waitloop
	dec hl 
	ld (speedUpLevelCounter), hl

	ld bc, (speedUpLevelCounter)
	;ld bc, $02ff
waitloop
	dec bc
	ld a,b
	or c
	jr nz, waitloop
	jp principalloop
	
gameover
	ld bc,10
	ld de,crash_message_txt
	call printstring
	; copy the current score to high score, need to check it is higher!!
	
	ld a, (score_mem_tens) ; load tens		
	ld (last_score_mem_tens),a 
	ld a, (score_mem_hund) ; load tens		
	ld (last_score_mem_hund),a	



	ld bc, $ffff   ;; wait max time for 16bits then go back to intro	
waitloop_end_game
	dec bc
	ld a,b
	or c
	jp nz, waitloop_end_game
	jp intro_title
	
	;ret  ; never return to basic
	
; original game written by Jon Kingsman, for zx spectrum, ZX81 port/rework by Adrian Pilkington 


; this prints at top any offset (stored in bc) from the top of the screen D_FILE
printstring
	ld hl,(D_FILE)
	add hl,bc	
printstring_loop
	ld a,(de)
	cp $ff
	jp z,printstring_end
	ld (hl),a
	inc hl
	inc de
	jr printstring_loop
printstring_end	
	ret
	
random 
	ld hl,(FRAMES)
random_seed 
	ld de,0
	add hl,de
	dec hl
	ld a,h
	and $1f
	ld h,a
	ld (random_seed+1),hl
	ld a,(hl)
foundRandom 
	sub b
	jr nc,foundRandom
	adc a,b
	ret 	
	
;include our variables
#include "vars.asm"

; ===========================================================
; code ends
; ===========================================================
;end the REM line and put in the RAND USR line to call our 'hex code'
#include "line2.asm"

;display file defintion
#include "screen.asm"               

;close out the basic program
#include "endbasic.asm"
