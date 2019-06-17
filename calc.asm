;****************************************************************************************************************************************************
;*	CALC.ASM - Gamboy Calculator Source Code
;*
;****************************************************************************************************************************************************

;****************************************************************************************************************************************************
;*	Includes
;****************************************************************************************************************************************************
	; system includes
	INCLUDE	"HARDWARE.INC"

	; project includes

	
;****************************************************************************************************************************************************
;*	user data (constants)
;****************************************************************************************************************************************************

;Defining variables that control the sprite position
g_spriteX	equ _OAMRAM + 1 ; X position in byte 1
g_spriteY	equ _OAMRAM + 0 ; Y position in byte 0

;Defining offsets into RAM that will be global variables for the game
;Use two bytes to store the highlight postion.  Could use a single byte with nibbles, but add a lot of masing instructions
g_xHighlightPosition	equ _RAM + 0 ; 1 byte, store the x value on the grid
g_yHighlightPosition	equ _RAM + 1 ; 1 byte, store the y value on the grid
g_buttonDepressTimer	EQU _RAM + 2 ; 1 byte, stores the number of refreshes since the last update
g_buttonBits		EQU _RAM + 3 ; 1 byte, store the contents of the bit registers for the directional pad and buttons

;At 10 onwards in RAM, keep a stack.  At 9, save off the pointer to the stack location
g_endOfList	equ _RAM + 9 ; variable we increment each time we add to the list
BOTTOM_OF_STACK	equ _RAM + 10 ; constant where the list started for reading it out


;****************************************************************************************************************************************************
;*	equates
;****************************************************************************************************************************************************

REFRESH_TIMEOUT	equ 10 ;10 cycles to let the button settle

CALC_ROW0	equ $28 ;2nd row, so 2 * 32 + 8
CALC_ROW1	equ $48 ;3rd row, so 2 * 32 + 8
CALC_ROW2	equ $68 ;4th row, so 4 * 32 + 8
CALC_ROW3	equ $88 ;5th row, so 6 * 32 + 8
CALC_ROW4	equ $A8 ;6th row, so 8 * 32 + 8

;Define the tiles
BLANK_TILE	equ 0
ONE_TILE	equ 1
TWO_TILE	equ 2
THREE_TILE	equ 3
FOUR_TILE	equ 4
FIVE_TILE	equ 5
SIX_TILE	equ 6
SEVEN_TILE	equ 7
EIGHT_TILE	equ	8
NINE_TILE	equ 9
ZERO_TILE	equ 10

A_TILE		equ 11
B_TILE		equ 12
C_TILE		equ 13
D_TILE		equ 14
E_TILE		equ 15
F_TILE		equ 16
G_TILE		equ 17
H_TILE		equ 18
I_TILE		equ 19
J_TILE		equ 20
K_TILE		equ 21
L_TILE		equ 22
M_TILE		equ 23
N_TILE		equ 24
O_TILE		equ 25
P_TILE		equ 26
Q_TILE		equ 27
R_TILE		equ 28
S_TILE		equ 29
T_TILE		equ 30
U_TILE		equ 31
V_TILE		equ 32
W_TILE		equ 33
X_TILE		equ 34
Y_TILE		equ 35
Z_TILE		equ 36

DIVIDE_TILE	equ 37
PLUS_TILE	equ 38
MINUS_TILE	equ 39
EQUAL_TILE	equ 40

HIGHLIGHT_TILE	equ 41

;****************************************************************************************************************************************************
;*	cartridge header
;****************************************************************************************************************************************************

	SECTION	"Org $00",HOME[$00]
RST_00:	
	jp	$100

	SECTION	"Org $08",HOME[$08]
RST_08:	
	jp	$100

	SECTION	"Org $10",HOME[$10]
RST_10:
	jp	$100

	SECTION	"Org $18",HOME[$18]
RST_18:
	jp	$100

	SECTION	"Org $20",HOME[$20]
RST_20:
	jp	$100

	SECTION	"Org $28",HOME[$28]
RST_28:
	jp	$100

	SECTION	"Org $30",HOME[$30]
RST_30:
	jp	$100

	SECTION	"Org $38",HOME[$38]
RST_38:
	jp	$100

	SECTION	"V-Blank IRQ Vector",HOME[$40]
VBL_VECT:
	call ON_VBLANK
	reti
	
	SECTION	"LCD IRQ Vector",HOME[$48]
LCD_VECT:
	reti

	SECTION	"Timer IRQ Vector",HOME[$50]
TIMER_VECT:
	reti

	SECTION	"Serial IRQ Vector",HOME[$58]
SERIAL_VECT:
	reti

	SECTION	"Joypad IRQ Vector",HOME[$60]
JOYPAD_VECT:
	reti
	
	SECTION	"Start",HOME[$100]
	nop
	jp	Start

	; $0104-$0133 (Nintendo logo - do _not_ modify the logo data here or the GB will not run the program)
	DB	$CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
	DB	$00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
	DB	$BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

	; $0134-$013E (Game title - up to 11 upper case ASCII characters; pad with $00)
	DB	"GB CALC    "
		;0123456789A

	; $013F-$0142 (Product code - 4 ASCII characters, assigned by Nintendo, just leave blank)
	DB	"    "
		;0123

	; $0143 (Color GameBoy compatibility code)
	DB	$00	; $00 - DMG 
			; $80 - DMG/GBC
			; $C0 - GBC Only cartridge

	; $0144 (High-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0145 (Low-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0146 (GameBoy/Super GameBoy indicator)
	DB	$00	; $00 - GameBoy

	; $0147 (Cartridge type - all Color GameBoy cartridges are at least $19)
	DB	$00	; $00 - ROM Only

	; $0148 (ROM size)
	DB	$00	; $00 - 256Kbit = 32Kbyte = 2 banks

	; $0149 (RAM size)
	DB	$00	; $00 - None

	; $014A (Destination code)
	DB	$00	; $01 - All others
			; $00 - Japan

	; $014B (Licensee code - this _must_ be $33)
	DB	$33	; $33 - Check $0144/$0145 for Licensee code.

	; $014C (Mask ROM version - handled by RGBFIX)
	DB	$00

	; $014D (Complement check - handled by RGBFIX)
	DB	$00

	; $014E-$014F (Cartridge checksum - handled by RGBFIX)
	DW	$00


;****************************************************************************************************************************************************
;*	Program Start
;****************************************************************************************************************************************************

	SECTION "Program Start",HOME[$0150]
Start::
	di			;disable interrupts
	ld	sp,$FFFE	;set the stack to $FFFE
	call WAIT_VBLANK	;wait for v-blank

	ld	a,0		;
	ldh	[rLCDC],a	;turn off LCD 

	call ClearMap		;create an empty background
	call LoadTiles		;load the tiles into the tile map memory
	call ClearSprites	;wipe the OAM
	call ResetList		;setup the variables in RAM for storing the list of button inputs
	call LoadMap		;put the background in place
	
	ld a,0 ;initialize the highlight positions
	ld [g_xHighlightPosition],a
	ld [g_yHighlightPosition],a
	ld a,REFRESH_TIMEOUT ;initialize the button timeout
	ld [g_buttonDepressTimer],a
	
	ld hl,_OAMRAM+2 ;load the highlight tile index to the sprite tile selector
	ld [hl],HIGHLIGHT_TILE
	ld hl,_OAMRAM+3 ; load the sprite flags to all 0s
	ld [hl],255
	
	call UpdateCursor

	ld	a,%11100100	;load a normal palette up 11 10 01 00 - dark->light
	ldh	[rBGP],a	;load the palette
	ldh [rOBP0],a
	
	ld	a,%10010011		;  =$93 
	ldh	[rLCDC],a	;turn on the LCD, BG, etc
	
	ld a,IEF_VBLANK
	ldh [rIE],a
	ei ; turn the interupts back on
	
Main_loop:: 
	halt
	nop
	jp Main_loop

;***************************************************************
;* Subroutines
;***************************************************************

	SECTION "Support Routines",HOME

WAIT_VBLANK::
	ldh	a,[rLY]		;get current scanline
	cp	$91			;Are we in v-blank yet?
	jr	nz,WAIT_VBLANK	;if A-91 != 0 then loop
	ret				;done
	
ClearMap::
	ld	hl,_SCRN0		;loads the address of the bg map ($9800) into HL
	ld	bc,32*32		;since we have 32x32 tiles, we'll need a counter so we can clear all of them
ClearMap_loop::
	ld	a,0			;load 0 into A (since our tile 0 is blank)
	ld	[hl+],a		;load A into HL, then increment HL (the HL+)
	dec	bc			;decrement our counter
	ld	a,b			;load B into A
	or	c			;if B or C != 0
	jr	nz,ClearMap_loop	;then loop
	ret				;done
	
ClearSprites::
	ld	hl,_OAMRAM	;load address of sprite ram
	ld	bc,40*4		;set the counter to 40 for 40 loops
ClearSprites_loop::
	ld	a,0		;set tile blank
	ld	[hl],a 	;load A (0) into the HL
	inc l		;increment the address to the next sprite
	dec bc		;count down
	ld a,b		;store off the top of bc
	or c		;see if b or c is 0
	jr nz,ClearSprites_loop
	ret
	
;Procedure that will put the list back to the begining, ready to take more commands
ResetList::
	ld a,0 				;load 0, and write it into the end of list variable
	ld hl,g_endOfList
	ld [hl],a
	ret
	
LoadTiles::
	ld	hl,TileLabel
	ld	de,_VRAM
	ld	bc,(HIGHLIGHT_TILE+1)*16	;we have 42 tiles and each tile takes 16 bytes
LoadTiles_loop::
	ld	a,[hl+]	;get a byte from our tiles, and increment.
	ld	[de],a	;put that byte in VRAM and
	inc	de		;increment.
	dec	bc		;bc=bc-1.
	ld	a,b		;if b or c != 0,
	or	c		;
	jr	nz,LoadTiles_loop	;then loop.
	ret			;done

LoadMap::
	ld	hl,CalcTileMap		;get the first the address of the map zone
	ld	de,_SCRN0+CALC_ROW1	;starting the map in the middle of the screen
	ld	c,4				;load 4 tiles
	call LoadMap_LOOP
	ld	de,_SCRN0+CALC_ROW2	;start the next row
	ld	c,4				;load 4 tiles
	call LoadMap_LOOP
	ld	de,_SCRN0+CALC_ROW3	;start the next row
	ld	c,4				;load 4 tiles
	call LoadMap_LOOP
	ld	de,_SCRN0+CALC_ROW4	;start the next row
	ld	c,4				;load 4 tiles
	call LoadMap_LOOP
	ret ; done, return out
LoadMap_LOOP::
	ld	a,[hl+]	;get a byte of the map and inc hl
	ld	[de],a	;put the byte at de
	inc	de		;duh...
	dec	c		;decrement our counter
	jr	nz,LoadMap_LOOP	;and of the counter != 0 then loop
	ret			;done

;Based on the location specified in the highlight positions, move the highlight sprite
UpdateCursor::
	;Now using the highlight position on the grid, set the sprites correctly
	ld a,[g_yHighlightPosition]
	ld d,4 ;Offest is from the position of 4 (start of top of grid)
	add a,d
	rla ;Shift left by 3 because of the length of a sprite
	rla
	rla
	ld [g_spriteY],a
	
	ld a,[g_xHighlightPosition]
	ld d,9 ;Offest is from the position of 9 (start from the side of the screen)
	add a,d
	rla ;Shift left by 3 because of the length of a sprite
	rla
	rla
	ld [g_spriteX],a

	ret ;done
	
UpdateCalcList::
	ld a,[g_xHighlightPosition] ;figure out which op is highlighted
	ld b,a ; save x position in b
	ld a,[g_yHighlightPosition] ;put y position in a
	rla 
	rla ;;multiply by 4
	add b ;add b in as the lower bits
	ld d,a ; store this tile index in d
	
	ld a,[g_endOfList] ;get the length of the list
	ld b,0
	ld c,a ;Put a into bc to add the offset to the hl
	ld hl,BOTTOM_OF_STACK
	add hl,bc ;increment hl to the end of list
	ld a,d ;write the stored tile index 
	ld [hl],a
	
	;Now draw that tile
	ld hl,CalcTileMap;Look up the operation in the calc tiles map
	ld b,0 ;setup the bc register with value in a
	ld c,a
	add hl,bc ;add the offset to get the tile address in hl
	ld a,[hl]
	ld e,a ;save the tile number off in e
	
	ld hl,_SCRN0+CALC_ROW0
	ld a,[g_endOfList] ;get the length of the list
	ld c,a
	add hl,bc ;add that list offset from the start of the row
	inc a
	ld [g_endOfList],a ;increment the end of the list
	ld a,e ;recall into a the tile number
	ld [hl],a ;write the tile number into the position on the screen
	ret
	
;Trivial multiplier, computing register a * b
Multiply::
	dec b
	jr z,Multiply_end
	add a
	jr Multiply
Multiply_end::
	ret
	
;Euclidian divider, computing a / b (leaving remainder in b)
Divide::
	ld c,0 ;use c to count number of successful divides
Divide_loop::
	sub b
	jr c,Divide_end
	jr Divide_loop
Divide_end::
	ld a,c
	ret
	
;Go through the list, grab every
ComputeCalcList::
	call ClearStack ;wipe the screen
	ret
	
;Wipe all numbers currently written
ClearStack::	
	ld a,[g_endOfList] ;get the length of the list
	ld b,0
	cp b 
	jr z,ClearStack_end ;if list was empty, then end early
	ld b,a ;make b a decrementer for the count here
	ld hl,_SCRN0+CALC_ROW0
	ld a,BLANK_TILE
ClearStack_loop::
	ld [hl+],a ;blank the position 
	dec b
	jr nz,ClearStack_loop
ClearStack_end::
	ld a,0
	ld [g_endOfList],a ;reset the list length
	ret

	
AFTER_BUTTON::
	ld a,10 ;give a brief pause for the button to depress
	ld [g_buttonDepressTimer], a
	call UpdateCursor ;redraw the sprite showing the selection
	ret ;we were jumped to, so we are calling out

SET_DOWN::
	;increment the y position
	ld a,[g_yHighlightPosition]
	inc a ; add 1
	ld b, $3
	and b ;mod by 4
	ld [g_yHighlightPosition],a
	jr AFTER_BUTTON
SET_UP::
	;decrement the y position
	ld a,[g_yHighlightPosition]
	dec a ; substract 1
	ld b, $3
	and b ;mod by 4
	ld [g_yHighlightPosition],a
	jr AFTER_BUTTON

SET_LEFT::
	;decrement the x position
	ld a,[g_xHighlightPosition]
	dec a ; substract 1
	ld b, $3
	and b ;mod by 4
	ld [g_xHighlightPosition],a
	jr AFTER_BUTTON

SET_RIGHT::
	;increment the x position
	ld a,[g_xHighlightPosition]
	inc a ; add 1
	ld b, $3
	and b ;mod by 4
	ld [g_xHighlightPosition],a
	jr AFTER_BUTTON
SET_A::
	call UpdateCalcList ;redraw the background where the work is shown
	jr AFTER_BUTTON
	
SET_B::
	call ComputeCalcList
	jr AFTER_BUTTON

SET_START::
	;ld a,START_TILE
	jr AFTER_BUTTON
	
SET_SELECT::
	;ld a,SELECT_TILE
	jr AFTER_BUTTON

;Label called 
OnVBlank_pauseForRefresh::
	ld [g_buttonDepressTimer],a ;write out the decremented value, stepping towards keeping it at 1
	ret ;let the procedure exit out
	
ON_VBLANK::
	ld a,[g_buttonDepressTimer] ;Check if someone just pressed a button
	dec a ;step towards 0
	jr nz,OnVBlank_pauseForRefresh
	;else keep going, we aren't a pause state
	call READ_INPUTS ;set the button inputs
	ld a,[g_buttonBits] ;get the button bits
	ld b,a ;save them off
	and PADF_DOWN
	jr nz,SET_DOWN
	ld a,b
	and PADF_UP
	jr nz,SET_UP
	ld a,b
	and PADF_LEFT
	jr nz,SET_LEFT
	ld a,b
	and PADF_RIGHT
	jr nz,SET_RIGHT
	ld a,b
	and PADF_A
	jr nz,SET_A
	ld a,b
	and PADF_B
	jr nz,SET_B
	ld a,b
	and PADF_START
	jr nz,SET_START
	ld a,b
	and PADF_SELECT
	jr nz,SET_SELECT
	
	;If we never jumped, just return out and leave things as they are
	ret
	
READ_INPUTS::
	ld a,P1F_5 ;ready the mask for P15
	ld [rP1],a ;set to read alpha pad
	ld a,[rP1] ;wait
	ld a,[rP1] ;wait
	ld a,[rP1] ;wait
	ld a,[rP1] ;wait
	ld a,[rP1] ;look for the input
	cpl ;complent a to get the real bits
	and $0f ;mask off the bottom four bits
	swap a ;put the nibble up to the top
	ld b,a ;save off the value for later
	ld a,P1F_4 ;ready mask for P14 register
	ld [rP1],a ;set to read number pad
	ld a,[rP1] ;wait for debounce
	ld a,[rP1]
	ld a,[rP1]
	ld a,[rP1]
	ld a,[rP1]
	ld a,[rP1] ; read value written into the register
	cpl ;get the real bits
	and $0f ;mask off the bottom four bits
	or b ;combine with the older bits
	ld [g_buttonBits],a ;save the bits off
	or 0
	jr nz,SAVE_BUTTON_BITS
	ret

SAVE_BUTTON_BITS
	ld [$C010],a
	ret
;********************************************************************
; This section was generated by GBTD v2.2

 SECTION "Tiles", HOME

; Start of tile array.
TileLabel::
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$18,$18,$38,$38,$18,$18
DB $18,$18,$18,$18,$3C,$3C,$00,$00
DB $00,$00,$78,$78,$04,$04,$04,$04
DB $38,$38,$60,$60,$40,$40,$7C,$7C
DB $00,$00,$7C,$7C,$04,$04,$04,$04
DB $3C,$3C,$04,$04,$04,$04,$7C,$7C
DB $00,$00,$24,$24,$24,$24,$24,$24
DB $3C,$3C,$04,$04,$04,$04,$04,$04
DB $00,$00,$7C,$7C,$40,$40,$40,$40
DB $78,$78,$04,$04,$04,$04,$78,$78
DB $00,$00,$40,$40,$40,$40,$40,$40
DB $7C,$7C,$44,$44,$44,$44,$7C,$7C
DB $00,$00,$7C,$7C,$44,$44,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04
DB $00,$00,$38,$38,$44,$44,$44,$44
DB $38,$38,$44,$44,$44,$44,$38,$38
DB $00,$00,$38,$38,$44,$44,$44,$44
DB $3C,$3C,$08,$08,$10,$10,$20,$20
DB $00,$00,$38,$38,$44,$44,$44,$44
DB $44,$44,$44,$44,$38,$38,$00,$00
DB $00,$00,$38,$38,$44,$44,$44,$44
DB $7C,$7C,$44,$44,$44,$44,$00,$00
DB $00,$00,$78,$78,$44,$44,$44,$44
DB $78,$78,$44,$44,$44,$44,$78,$78
DB $00,$00,$3C,$3C,$42,$42,$40,$40
DB $40,$40,$40,$40,$42,$42,$3C,$3C
DB $00,$00,$78,$78,$44,$44,$44,$44
DB $44,$44,$44,$44,$44,$44,$78,$78
DB $00,$00,$7C,$7C,$40,$40,$40,$40
DB $78,$78,$40,$40,$40,$40,$7C,$7C
DB $00,$00,$7C,$7C,$40,$40,$40,$40
DB $78,$78,$40,$40,$40,$40,$40,$40
DB $00,$00,$38,$38,$44,$44,$40,$40
DB $40,$40,$5C,$5C,$44,$44,$38,$38
DB $00,$00,$44,$44,$44,$44,$44,$44
DB $7C,$7C,$44,$44,$44,$44,$44,$44
DB $00,$00,$7E,$7E,$18,$18,$18,$18
DB $18,$18,$18,$18,$18,$18,$7E,$7E
DB $00,$00,$7E,$7E,$08,$08,$08,$08
DB $08,$08,$08,$08,$48,$48,$30,$30
DB $00,$00,$48,$48,$50,$50,$60,$60
DB $60,$60,$50,$50,$48,$48,$48,$48
DB $00,$00,$40,$40,$40,$40,$40,$40
DB $40,$40,$40,$40,$40,$40,$7C,$7C
DB $00,$00,$66,$66,$7E,$7E,$5A,$5A
DB $42,$42,$42,$42,$42,$42,$42,$42
DB $00,$00,$62,$62,$62,$62,$52,$52
DB $5A,$5A,$4A,$4A,$46,$46,$46,$46
DB $00,$00,$38,$38,$44,$44,$44,$44
DB $44,$44,$44,$44,$44,$44,$38,$38
DB $00,$00,$78,$78,$44,$44,$44,$44
DB $78,$78,$40,$40,$40,$40,$40,$40
DB $00,$00,$38,$38,$44,$44,$44,$44
DB $54,$54,$4C,$4C,$3C,$3C,$02,$02
DB $00,$00,$78,$78,$44,$44,$44,$44
DB $78,$78,$50,$50,$48,$48,$44,$44
DB $00,$00,$38,$38,$44,$44,$40,$40
DB $38,$38,$04,$04,$44,$44,$38,$38
DB $00,$00,$7C,$7C,$10,$10,$10,$10
DB $10,$10,$10,$10,$10,$10,$10,$10
DB $00,$00,$44,$44,$44,$44,$44,$44
DB $44,$44,$44,$44,$44,$44,$38,$38
DB $00,$00,$00,$00,$44,$44,$44,$44
DB $44,$44,$28,$28,$28,$28,$10,$10
DB $00,$00,$82,$82,$82,$82,$82,$82
DB $54,$54,$54,$54,$54,$54,$28,$28
DB $00,$00,$00,$00,$42,$42,$24,$24
DB $18,$18,$18,$18,$24,$24,$42,$42
DB $00,$00,$44,$44,$44,$44,$28,$28
DB $28,$28,$10,$10,$10,$10,$10,$10
DB $00,$00,$7E,$7E,$06,$06,$08,$08
DB $10,$10,$20,$20,$60,$60,$7E,$7E
DB $00,$00,$30,$30,$30,$30,$00,$00
DB $FC,$FC,$00,$00,$30,$30,$30,$30
DB $00,$00,$00,$00,$10,$10,$10,$10
DB $7C,$7C,$10,$10,$10,$10,$00,$00
DB $00,$00,$00,$00,$00,$00,$3C,$3C
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$3C,$3C,$00,$00
DB $3C,$3C,$00,$00,$00,$00,$00,$00
DB $FF,$FF,$81,$81,$81,$81,$81,$81
DB $81,$81,$81,$81,$81,$81,$FF,$FF


;************************************************************
;* tile map

SECTION "Map",HOME

CalcTileMap::
DB ONE_TILE, TWO_TILE, THREE_TILE, PLUS_TILE
DB FOUR_TILE, FIVE_TILE, SIX_TILE, MINUS_TILE
DB SEVEN_TILE, EIGHT_TILE, NINE_TILE, X_TILE
DB BLANK_TILE, ZERO_TILE, BLANK_TILE, DIVIDE_TILE


;*** End Of File ***