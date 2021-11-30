 processor 6502
	org $401
StartBlock401:
	; Starting new memory block at $401
	.byte $b ; lo byte of next line
	.byte $4 ; hi byte of next line
	.byte $0a, $00 ; line 10 (lo, hi)
	.byte $9e, $20 ; SYS token and a space
	.byte   $31,$30,$34,$30
	.byte $00, $00, $00 ; end of program
	; Ending memory block at $401
EndBlock401:
	org $410
StartBlock410:
	; Starting new memory block at $410
tutorial_07_sqrt_and_atan
	jmp block1
x1	dc.b	0
y1	dc.b	0
tangent	dc.b	0
i	dc.b	0
dx	dc.b	0
dy	dc.b	0
temp	dc.b	0
radial	dc.w	0
titlemsg		dc.b	"TRSE 'SQRT AND ATAN' DEMO"
	dc.b	0
authormsg		dc.b	"40/80 VERSION FUZZYBAD"
	dc.b	0
exitmsg		dc.b	211
	dc.b	208
	dc.b	193
	dc.b	195
	dc.b	197
	dc.b	" TO QUIT"
	dc.b	0
myscreenwidth	dc.b	$00
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $80     ;$59 used for hi-byte
initdiv16x8_dividend = $82	  ;$fc used for hi-byte
initdiv16x8_remainder = $84	  ;$fe used for hi-byte
initdiv16x8_result = $82 ;save memory by reusing divident to store the result
divide16x8
	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16:	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
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
skip16
	dex
	bne divloop16
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $80
mul16x8_num1 = $82
mul16x8_num2 = $84
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
mul16x8_enterLoop
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $80
div8x8_d = $82
div8x8_e = $84
	; Normal 8x8 bin div
div8x8_procedure
	lda #$00
	ldx #$07
	clc
div8x8_loop1
	rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2
	dex
	bpl div8x8_loop1
	rol div8x8_d
	lda div8x8_d
div8x8_def_end
	rts
	; NodeProcedureDecl -1
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
octant		= $7b
atan2_x1:    .byte 0
atan2_x2:    .byte 0
atan2_y1:    .byte 0
atan2_y2:    .byte 0
atan2_call:
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
octant_adjust:	.byte %00111111		;; x+,y+,|x|>|y|
		.byte %00000000		;; x+,y+,|x|<|y|
		.byte %11000000		;; x+,y-,|x|>|y|
		.byte %11111111		;; x+,y-,|x|<|y|
		.byte %01000000		;; x-,y+,|x|>|y|
		.byte %01111111		;; x-,y+,|x|<|y|
		.byte %10111111		;; x-,y-,|x|>|y|
		.byte %10000000		;; x-,y-,|x|<|y|
		;;;;;;;; atan(2^(x/32))*128/pi ;;;;;;;;
atan_tab:	.byte $00,$00,$00,$00,$00,$00,$00,$00
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $80
multiplier_a = $82
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $82
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initlog2
	;    Procedure type : User-defined procedure
log2_table:	.byte $00,$00,$20,$32,$40,$4a,$52,$59
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto3
screenmemory =  $fe
colormemory =  $fb
screen_x = $80
screen_y = $82
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initprintstring
	;    Procedure type : User-defined procedure
print_text = $80
print_number_text: .dc "    ",0
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : GetKey
	;    Procedure type : User-defined procedure
GetKey
	jsr $ffe4
	rts
	
; // getin
; // Wait for user input
	; NodeProcedureDecl -1
	; ***********  Defining procedure : WaitForKeypress
	;    Procedure type : User-defined procedure
WaitForKeypress
	
; // Pause until key pressed
	lda #$0
	; Calling storevariable on generic assign expression
	sta temp
WaitForKeypress_while6
WaitForKeypress_loopstart10
	; Binary clause Simplified: EQUALS
	lda temp
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne WaitForKeypress_elsedoneblock9
WaitForKeypress_ConditionalTrueBlock7: ;Main true block ;keep 
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta temp
	jmp WaitForKeypress_while6
WaitForKeypress_elsedoneblock9
WaitForKeypress_loopend11
	rts
	
; // Determine screen width to use
	; NodeProcedureDecl -1
	; ***********  Defining procedure : SetScreenWidth
	;    Procedure type : User-defined procedure
SetScreenWidth
	; Binary clause Simplified: EQUALS
	; Peek
	lda $e000 + $1;keep
	; Compare with pure num / var optimization
	cmp #$4b;keep
	bne SetScreenWidth_elseblock17
SetScreenWidth_ConditionalTrueBlock16: ;Main true block ;keep 
	
; // Determine PET model by checking first byte of EDIT ROM at $E000
; //	$A0 [160] = B1
; //	$48 [72]  = B2
; //	$36 [54]  = B4-40
; //	$4B [75]  = B4-80	
	lda #$50
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	jmp SetScreenWidth_elsedoneblock18
SetScreenWidth_elseblock17
	lda #$28
	; Calling storevariable on generic assign expression
	sta myscreenwidth
SetScreenWidth_elsedoneblock18
	
; //centerX := myscreenwidth / 2 - 1;
	; Clear screen with offset
	lda #$20
	ldx #$fa
SetScreenWidth_clearloop23
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne SetScreenWidth_clearloop23
	rts
	
; //	Method which shows title screen and checks screen width
	; NodeProcedureDecl -1
	; ***********  Defining procedure : showTitle
	;    Procedure type : User-defined procedure
showTitle
	
; // Determine screen width and clear it
	jsr SetScreenWidth
	
; // Set uppercase
	; Poke
	; Optimization: shift is zero
	lda #$c
	sta $e84c
	
; // Center the title text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #12
showTitle_rightvarAddSub_var25 = $54
	sta showTitle_rightvarAddSub_var25
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var25
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$1
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<titlemsg
	adc #$0
	ldy #>titlemsg
	sta print_text+0
	sty print_text+1
	ldx #$19 ; optimized, look out for bugs
	jsr printstring
	
; // Center the author text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #11
showTitle_rightvarAddSub_var28 = $54
	sta showTitle_rightvarAddSub_var28
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var28
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$3
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<authormsg
	adc #$0
	ldy #>authormsg
	sta print_text+0
	sty print_text+1
	ldx #$16 ; optimized, look out for bugs
	jsr printstring
	
; // Center exit message
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
showTitle_rightvarAddSub_var31 = $54
	sta showTitle_rightvarAddSub_var31
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var31
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$c
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<exitmsg
	adc #$0
	ldy #>exitmsg
	sta print_text+0
	sty print_text+1
	ldx #$d ; optimized, look out for bugs
	jsr printstring
	
; // Pause until user presses key
	jsr WaitForKeypress
	rts
	; NodeProcedureDecl -1
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
	lda #$0
	; Calling storevariable on generic assign expression
	sta y1
RenderScreen_forloop35
	
; // Fill color data with color value based on tangent
	lda #$0
	; Calling storevariable on generic assign expression
	sta x1
RenderScreen_forloop70
	
; // Set cp to point to color memory
; // 23 rows
; //40 columns
; // calculate delta x and delta y from the center of the screen(20,13)
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; abs(x) byte
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc x1
	 ; end add / sub var with constant
	cmp #127
	bcc RenderScreen_abslabel86
	eor #$ff
	adc #$00
RenderScreen_abslabel86
	lsr
	; Calling storevariable on generic assign expression
	sta dx
	; abs(x) byte
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #$d
	sec
	sbc y1
	 ; end add / sub var with constant
	cmp #127
	bcc RenderScreen_abslabel87
	eor #$ff
	adc #$00
RenderScreen_abslabel87
	; Calling storevariable on generic assign expression
	sta dy
	
; // Calculate the "tangential" value of dx,dy. Try to plot this value indepenedntly!
	; Call atan2
	lda myscreenwidth
	sta atan2_x1
	lda x1
	sta atan2_x2
	lda #$c
	sta atan2_y1
	lda y1
	sta atan2_y2
	jsr atan2_call
	; Calling storevariable on generic assign expression
	sta tangent
	; Generic 16 bit op
	ldy #0
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
	; Calling storevariable on generic assign expression
	sta radial
	sty radial+1
	
; // Calculate the "radial" value of dx,dy. Try to plot this value indepenedntly!
; // Combine the values to create a spiral. Ideally the(tangent
	; Generic 16 bit op
	ldy #0
	lda tangent
RenderScreen_rightvarInteger_var93 = $54
	sta RenderScreen_rightvarInteger_var93
	sty RenderScreen_rightvarInteger_var93+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	ldy radial+1
	lda radial
RenderScreen_int_shift_var94 =  $56
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
	; Calling storevariable on generic assign expression
	sta i
	
; // Ideal,(radial,tangent) should be used to lookup a seamless texture
; // Fill screen memory with the character value
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy x1 ; optimized, look out for bugs
	sta (screenmemory),y
RenderScreen_forloopcounter72
RenderScreen_loopstart73
	; Test Inc dec D
	inc x1
	lda myscreenwidth
	cmp x1 ;keep
	beq RenderScreen_loopdone95
RenderScreen_loopnotdone96
	jmp RenderScreen_forloop70
RenderScreen_loopdone95
RenderScreen_forloopend71
RenderScreen_loopend74
	lda screenmemory
	clc
	adc myscreenwidth
	sta screenmemory+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc RenderScreen_WordAdd97
	inc screenmemory+1
RenderScreen_WordAdd97
RenderScreen_forloopcounter37
RenderScreen_loopstart38
	; Test Inc dec D
	inc y1
	lda #$19
	cmp y1 ;keep
	beq RenderScreen_loopdone98
RenderScreen_loopnotdone99
	jmp RenderScreen_forloop35
RenderScreen_loopdone98
RenderScreen_forloopend36
RenderScreen_loopend39
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : ShiftScreenData
	;    Procedure type : User-defined procedure
ShiftScreenData
	; ****** Inline assembler section
		ldx #0
loop_out:
		inc $8000,x1
		inc $8100,x1
		inc $8200,x1
		inc $8300,x1
		inc $8400,x1
		inc $8500,x1
		inc $8600,x1
		inc $8700,x1
		inx
		bne loop_out
		
	
	rts
block1
	
; // Main initialization routine
; // Show the title and check number of columns
	jsr showTitle
	jsr RenderScreen
MainProgram_while101
MainProgram_loopstart105
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock104
MainProgram_ConditionalTrueBlock102: ;Main true block ;keep 
	jsr ShiftScreenData
	
; // Trigger NMI	    
	; Binary clause Simplified: EQUALS
	jsr GetKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock120
MainProgram_ConditionalTrueBlock118: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop124
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop124
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock120
	jmp MainProgram_while101
MainProgram_elsedoneblock104
MainProgram_loopend106
	; End of program
	; Ending memory block at $410
EndBlock410:
