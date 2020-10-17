 processor 6502
	org $400
	; Starting new memory block at $400
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $31,$30,$34,$30
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock5257
	org $410
	; Starting new memory block at $410
tutorial_07_sqrt_and_atan
	jmp block1
x	dc.b	
y	dc.b	
tangent	dc.b	
i	dc.b	
dx	dc.b	
dy	dc.b	
radial	dc.w	
key	dc.b	
numkeys	dc.b	
titlemsg		dc.b	"TRSE EXAMPLE 7  'SQRT AND ATAN'"
	dc.b	0
authormsg		dc.b	"40/80 VERSION 9/2020"
	dc.b	0
promptmsg		dc.b	"USE "
	dc.b	244
	dc.b	"0 OR "
	dc.b	248
	dc.b	"0 COLUMN SCREEN?"
	dc.b	0
exitmsg		dc.b	211
	dc.b	208
	dc.b	193
	dc.b	195
	dc.b	197
	dc.b	" TO QUIT"
	dc.b	0
myscreenwidth	dc.b	$00
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $4c     ;$59 used for hi-byte
initdiv16x8_dividend = $4e	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4e ;save memory by reusing divident to store the result
divide16x8	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
	rol initdiv16x8_dividend+1
	rol initdiv16x8_remainder	;remainder lb & hb * 2 + msb from carry
	rol initdiv16x8_remainder+1
	lda initdiv16x8_remainder
	sec
	sbc initdiv16x8_divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda initdiv16x8_remainder+1
	sbc initdiv16x8_divisor+1
	bcc skip16	;if carry=0 then divisor didn't fit in yet
	sta initdiv16x8_remainder+1	;else save substraction result as new remainder,
	sty initdiv16x8_remainder
	inc initdiv16x8_result	;and INCrement result cause divisor fit in 1 times
skip16	dex
	bne divloop16
	rts
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4c
mul16x8_num1 = $4e
mul16x8_num2 = $50
mul16x8_procedure
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop  ; accumulating multiply entry point (enter with .A=lo, .Y=hi)
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $4c
div8x8_d = $4e
div8x8_e = $50
	; Normal 8x8 bin div
div8x8_procedure
	lda #$00
	ldx #$07
	clc
div8x8_loop1 rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2 dex
	bpl div8x8_loop1
	rol div8x8_d
	lda div8x8_d
div8x8_def_end
	rts
	; ***********  Defining procedure : initatan2
	;    Procedure type : User-defined procedure
;; Calculate the angle, in a 256-degree circle, between two points.
;; The trick is to use logarithmic division to get the y/x ratio and
;; integrate the power function into the atan table. Some branching is
;; avoided by using a table to adjust for the octants.
;; In otherwords nothing new or particularily clever but nevertheless
;; quite useful.
;;
;; by Johan Forslöf (doynax)
octant		= $47
atan2_x1    .byte 0
atan2_x2    .byte 0
atan2_y1    .byte 0
atan2_y2    .byte 0
atan2_call
		lda atan2_x1
		sbc atan2_x2
		bcs *+4
		eor #$ff
		tax
		rol octant
		lda atan2_y1
		sbc atan2_y2
		bcs *+4
		eor #$ff
		tay
		rol octant
                lda log2_table,x
                sbc log2_table,y
		bcc *+4
		eor #$ff
		tax
		lda octant
		rol
		and #%111
		tay
                lda atan_tab,x
		eor octant_adjust,y
		rts
octant_adjust	.byte %00111111		;; x+,y+,|x|>|y|
		.byte %00000000		;; x+,y+,|x|<|y|
		.byte %11000000		;; x+,y-,|x|>|y|
		.byte %11111111		;; x+,y-,|x|<|y|
		.byte %01000000		;; x-,y+,|x|>|y|
		.byte %01111111		;; x-,y+,|x|<|y|
		.byte %10111111		;; x-,y-,|x|>|y|
		.byte %10000000		;; x-,y-,|x|<|y|
		;;;;;;;; atan(2^(x/32))*128/pi ;;;;;;;;
atan_tab	.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $05,$05,$05,$05,$05,$05,$05,$05
		.byte $06,$06,$06,$06,$06,$06,$06,$06
		.byte $07,$07,$07,$07,$07,$07,$08,$08
		.byte $08,$08,$08,$08,$09,$09,$09,$09
		.byte $09,$0a,$0a,$0a,$0a,$0b,$0b,$0b
		.byte $0b,$0c,$0c,$0c,$0c,$0d,$0d,$0d
		.byte $0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f
		.byte $10,$10,$10,$11,$11,$11,$12,$12
		.byte $12,$13,$13,$13,$14,$14,$15,$15
		.byte $15,$16,$16,$17,$17,$17,$18,$18
		.byte $19,$19,$19,$1a,$1a,$1b,$1b,$1c
		.byte $1c,$1c,$1d,$1d,$1e,$1e,$1f,$1f
	rts
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4c
multiplier_a = $4e
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $4e
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop
	bcc mul_skip
mul_mod
	adc multiplier_a
mul_skip
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end
	txa
	rts
initeightbitmul_multiply_eightbit2
	rts
	; ***********  Defining procedure : initlog2
	;    Procedure type : User-defined procedure
log2_table	.byte $00,$00,$20,$32,$40,$4a,$52,$59
		.byte $60,$65,$6a,$6e,$72,$76,$79,$7d
		.byte $80,$82,$85,$87,$8a,$8c,$8e,$90
		.byte $92,$94,$96,$98,$99,$9b,$9d,$9e
		.byte $a0,$a1,$a2,$a4,$a5,$a6,$a7,$a9
		.byte $aa,$ab,$ac,$ad,$ae,$af,$b0,$b1
		.byte $b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9
		.byte $b9,$ba,$bb,$bc,$bd,$bd,$be,$bf
		.byte $c0,$c0,$c1,$c2,$c2,$c3,$c4,$c4
		.byte $c5,$c6,$c6,$c7,$c7,$c8,$c9,$c9
		.byte $ca,$ca,$cb,$cc,$cc,$cd,$cd,$ce
		.byte $ce,$cf,$cf,$d0,$d0,$d1,$d1,$d2
		.byte $d2,$d3,$d3,$d4,$d4,$d5,$d5,$d5
		.byte $d6,$d6,$d7,$d7,$d8,$d8,$d9,$d9
		.byte $d9,$da,$da,$db,$db,$db,$dc,$dc
		.byte $dd,$dd,$dd,$de,$de,$de,$df,$df
		.byte $df,$e0,$e0,$e1,$e1,$e1,$e2,$e2
		.byte $e2,$e3,$e3,$e3,$e4,$e4,$e4,$e5
		.byte $e5,$e5,$e6,$e6,$e6,$e7,$e7,$e7
		.byte $e7,$e8,$e8,$e8,$e9,$e9,$e9,$ea
		.byte $ea,$ea,$ea,$eb,$eb,$eb,$ec,$ec
		.byte $ec,$ec,$ed,$ed,$ed,$ed,$ee,$ee
		.byte $ee,$ee,$ef,$ef,$ef,$ef,$f0,$f0
		.byte $f0,$f1,$f1,$f1,$f1,$f1,$f2,$f2
		.byte $f2,$f2,$f3,$f3,$f3,$f3,$f4,$f4
		.byte $f4,$f4,$f5,$f5,$f5,$f5,$f5,$f6
		.byte $f6,$f6,$f6,$f7,$f7,$f7,$f7,$f7
		.byte $f8,$f8,$f8,$f8,$f9,$f9,$f9,$f9
		.byte $f9,$fa,$fa,$fa,$fa,$fa,$fb,$fb
		.byte $fb,$fb,$fb,$fc,$fc,$fc,$fc,$fc
		.byte $fd,$fd,$fd,$fd,$fd,$fd,$fe,$fe
		.byte $fe,$fe,$fe,$ff,$ff,$ff,$ff,$ff
	rts
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto3
screenmemory =  $fe
screen_x = $4c
screen_y = $4e
SetScreenPosition
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop
	clc
	adc #80
	bcc sskip
	inc screenmemory+1
sskip
	dey
	bne syloop
sydone
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone
	sta screenmemory
	rts
initmoveto_moveto3
	rts
	; ***********  Defining procedure : initprintstring
	;    Procedure type : User-defined procedure
print_text = $4c
print_number_text .dc "    ",0
printstring
	ldy #0
printstringloop
	lda (print_text),y
	cmp #0 ;keep
	beq printstring_done
	cmp #64
	bcc printstring_skip
	sec
	sbc #64
printstring_skip
	sta (screenmemory),y
	iny
	dex
	cpx #0
	beq printstring_done
	jmp printstringloop
printstring_done
	rts
	
; // Text for splash screen  	
; // Holds value from user selection for screen width  
; //	Method to get a char from the keyboard buffer
; //	TRSE procedures return accumulator value
; //
	; ***********  Defining procedure : getKey
	;    Procedure type : User-defined procedure
getKey
	jsr $ffe4
	rts
	
; // getin 
; //	Method which shows title screen and checks screen width
; //
	; ***********  Defining procedure : showTitle
	;    Procedure type : User-defined procedure
showTitle
	
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
showTitle_clearloop6
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne showTitle_clearloop6
	
; // Show the title text
	; MoveTo optimization
	lda #$55
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	clc
	lda #<titlemsg
	adc #$0
	ldy #>titlemsg
	sta print_text+0
	sty print_text+1
	ldx #$1f ; optimized, look out for bugs
	jsr printstring
	; MoveTo optimization
	lda #$aa
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	clc
	lda #<authormsg
	adc #$0
	ldy #>authormsg
	sta print_text+0
	sty print_text+1
	ldx #$14 ; optimized, look out for bugs
	jsr printstring
	
; // Ask user if they have 40 or 80 column screen
	; MoveTo optimization
	lda #$47
	sta screenmemory
	lda #>$8000
	clc
	adc #$01
	sta screenmemory+1
	clc
	lda #<promptmsg
	adc #$0
	ldy #>promptmsg
	sta print_text+0
	sty print_text+1
	ldx #$1b ; optimized, look out for bugs
	jsr printstring
	
; // Show the quit instruction
	; MoveTo optimization
	lda #$7e
	sta screenmemory
	lda #>$8000
	clc
	adc #$03
	sta screenmemory+1
	clc
	lda #<exitmsg
	adc #$0
	ldy #>exitmsg
	sta print_text+0
	sty print_text+1
	ldx #$d ; optimized, look out for bugs
	jsr printstring
showTitle_while15
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne showTitle_elsedoneblock18
showTitle_ConditionalTrueBlock16: ;Main true block ;keep 
	
; // 009E			No. of Chars. in Keyboard Buffer(Queue)
; // 00E3 		Size of Keyboard Buffer
; // 0270-027A  	Keyboard Buffer Queue(FIFO)
; // Is there a value in the character buffer?
	; Assigning single variable : numkeys
	; Peek
	lda $9e + $0
	; Calling storevariable
	sta numkeys
	; Binary clause Simplified: GREATER
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcc showTitle_elsedoneblock54
	beq showTitle_elsedoneblock54
showTitle_ConditionalTrueBlock52: ;Main true block ;keep 
	; Assigning single variable : key
	jsr getKey
	; Calling storevariable
	sta key
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne showTitle_elsedoneblock72
showTitle_ConditionalTrueBlock70: ;Main true block ;keep 
	
; // 52 is '4'
	; Assigning single variable : myscreenwidth
	lda #$28
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock72
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne showTitle_elsedoneblock78
showTitle_ConditionalTrueBlock76: ;Main true block ;keep 
	
; // 56 is '8'
	; Assigning single variable : myscreenwidth
	lda #$50
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock78
showTitle_elsedoneblock54
	jmp showTitle_while15
showTitle_elsedoneblock18
	rts
	; ***********  Defining procedure : RenderScreen
	;    Procedure type : User-defined procedure
RenderScreen
	
; // could equally well have written "screenmemory:=screen_char_loc;"
	; MoveTo optimization
	lda #$00
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	; Assigning single variable : y
	lda #$0
	; Calling storevariable
	sta y
RenderScreen_forloop82
	
; // Fill color data with color value based on tangent
	; Assigning single variable : x
	lda #$0
	; Calling storevariable
	sta x
RenderScreen_forloop84
	
; // Set cp to point to color memory
; // 23 rows
; //40 columns
; // calculate delta x and delta y from the center of the screen(20,13)
	; Assigning single variable : dx
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; abs(x) byte
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc x
	 ; end add / sub var with constant
	cmp #127
	bcc RenderScreen_abslabel86
	eor #$ff
	adc #$00
RenderScreen_abslabel86
	lsr
	; Calling storevariable
	sta dx
	; Assigning single variable : dy
	; abs(x) byte
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #$d
	sec
	sbc y
	 ; end add / sub var with constant
	cmp #127
	bcc RenderScreen_abslabel87
	eor #$ff
	adc #$00
RenderScreen_abslabel87
	; Calling storevariable
	sta dy
	
; // Calculate the "tangential" value of dx,dy. Try to plot this value indepenedntly!
	; Assigning single variable : tangent
	; Call atan2
	lda myscreenwidth
	sta atan2_x1
	lda x
	sta atan2_x2
	lda #$c
	sta atan2_y1
	lda y
	sta atan2_y2
	jsr atan2_call
	; Calling storevariable
	sta tangent
	; Assigning single variable : radial
	ldy #0
	; Generic 16 bit op
	; Mul 16x8 setup
	lda dy
	sta mul16x8_num1
	sty mul16x8_num1Hi
	sta mul16x8_num2
	jsr mul16x8_procedure
RenderScreen_rightvarInteger_var90 = $54
	sta RenderScreen_rightvarInteger_var90
	sty RenderScreen_rightvarInteger_var90+1
	; Mul 16x8 setup
	ldy #0
	lda dx
	sta mul16x8_num1
	sty mul16x8_num1Hi
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc RenderScreen_rightvarInteger_var90
RenderScreen_wordAdd88
	sta RenderScreen_rightvarInteger_var90
	; High-bit binop
	tya
	adc RenderScreen_rightvarInteger_var90+1
	tay
	lda RenderScreen_rightvarInteger_var90
	; Calling storevariable
	sta radial
	sty radial+1
	
; // Calculate the "radial" value of dx,dy. Try to plot this value indepenedntly!
; // Combine the values to create a spiral. Ideally the(tangent
	; Assigning single variable : i
	; Generic 16 bit op
	ldy #0
	lda tangent
RenderScreen_rightvarInteger_var93 = $54
	sta RenderScreen_rightvarInteger_var93
	sty RenderScreen_rightvarInteger_var93+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Integer assignment in nodevar
	lda radial
	ldy radial+1
RenderScreen_int_shift_var94 = $56
	sta RenderScreen_int_shift_var94
	sty RenderScreen_int_shift_var94+1
		lsr RenderScreen_int_shift_var94+1
	ror RenderScreen_int_shift_var94+0

	lda RenderScreen_int_shift_var94
	ldy RenderScreen_int_shift_var94+1
	; Low bit binop:
	clc
	adc RenderScreen_rightvarInteger_var93
RenderScreen_wordAdd91
	sta RenderScreen_rightvarInteger_var93
	; High-bit binop
	tya
	adc RenderScreen_rightvarInteger_var93+1
	tay
	lda RenderScreen_rightvarInteger_var93
	; Calling storevariable
	sta i
	
; // Ideal,(radial,tangent) should be used to lookup a seamless texture
; // Fill screen memory with the character value
	; Assigning single variable : screenmemory
	; Store Variable simplified optimization : right-hand term is pure
	ldy x ; optimized, look out for bugs
	lda i
	sta (screenmemory),y
	inc x
	lda myscreenwidth
	cmp x ;keep
	beq RenderScreen_loopdone105
RenderScreen_loopnotdone106
	jmp RenderScreen_forloop84
RenderScreen_loopdone105
	; Assigning single variable : screenmemory
	lda screenmemory
	clc
	adc myscreenwidth
	sta screenmemory+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc RenderScreen_WordAdd107
	inc screenmemory+1
RenderScreen_WordAdd107
	inc y
	lda #$19
	cmp y ;keep
	beq RenderScreen_loopdone133
RenderScreen_loopnotdone134
	jmp RenderScreen_forloop82
RenderScreen_loopdone133
	rts
	; ***********  Defining procedure : ShiftScreenData
	;    Procedure type : User-defined procedure
ShiftScreenData
	; ****** Inline assembler section
		ldx #0
loop_out:
		inc $8000,x
		inc $8100,x
		inc $8200,x
		inc $8300,x
		inc $8400,x
		inc $8500,x
		inc $8600,x
		inc $8700,x
		inx
		bne loop_out
		
	
	rts
block1
	
; // Main initialization routine
; // Show the title and check number of columns
	jsr showTitle
	jsr RenderScreen
MainProgram_while136
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock139
MainProgram_ConditionalTrueBlock137: ;Main true block ;keep 
	jsr ShiftScreenData
	; Binary clause Simplified: EQUALS
	jsr getKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock153
MainProgram_ConditionalTrueBlock151: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop157
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop157
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock153
	jmp MainProgram_while136
MainProgram_elsedoneblock139
EndSymbol
	; End of program
	; Ending memory block
EndBlock5259
