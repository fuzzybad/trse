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
Tutorial3_plasma
		jsr initsine_calculate
	jmp block1
c	dc.b	0
val	dc.b	0
c2x	dc.b	0
c2y	dc.b	0
ax	dc.b	0
ay	dc.b	0
x	dc.b	0
y	dc.b	0
x1	dc.b	0
temp	dc.b	0
colorP	= $68
titlemsg		dc.b	"TRSE 'PLASMA' DEMO"
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
siny	dc.b	 
	org siny+25
sinx	dc.b	 
	org sinx+80
lookupDiv	dc.b	 
	org lookupDiv+256
vals	dc.b $020, $02e, $0a6, $0a0, $0a0, $0a6, $02e, $020
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
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto2
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
initmoveto_moveto2
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initsinetable
	;    Procedure type : Built-in function
	;    Requires initialization : no
sine .byte 0 
	org sine +#255
value: .word 0
delta: .word 0
initsine_calculate
	ldy #$3f
	ldx #$00
initsin_a
	lda value
	clc
	adc delta
	sta value
	lda value+1
	adc delta+1
	sta value+1
	sta sine+$c0,x
	sta sine+$80,y
	eor #$ff
	sta sine+$40,x
	sta sine+$00,y
	lda delta
	adc #$10   ; this value adds up to the proper amplitude
	sta delta
	bcc initsin_b
	inc delta+1
initsin_b
	inx
	dey
	bpl initsin_a
	rts
	
; // Text for splash screen  	
; // User selection for screen width 
; // charset will be placed at $2000 in bank 1	
; // look in the character set
; // mini sine table
; // Lookup table for division by 16
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
WaitForKeypress_while5
WaitForKeypress_loopstart9
	; Binary clause Simplified: EQUALS
	lda temp
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne WaitForKeypress_elsedoneblock8
WaitForKeypress_ConditionalTrueBlock6: ;Main true block ;keep 
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta temp
	jmp WaitForKeypress_while5
WaitForKeypress_elsedoneblock8
WaitForKeypress_loopend10
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
	bne SetScreenWidth_elseblock16
SetScreenWidth_ConditionalTrueBlock15: ;Main true block ;keep 
	
; // Determine PET model by checking first byte of EDIT ROM at $E000
; //	$A0 [160] = B1
; //	$48 [72]  = B2
; //	$36 [54]  = B4-40
; //	$4B [75]  = B4-80	
	lda #$50
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	jmp SetScreenWidth_elsedoneblock17
SetScreenWidth_elseblock16
	lda #$28
	; Calling storevariable on generic assign expression
	sta myscreenwidth
SetScreenWidth_elsedoneblock17
	
; //centerX := myscreenwidth / 2 - 1;
	; Clear screen with offset
	lda #$20
	ldx #$fa
SetScreenWidth_clearloop22
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne SetScreenWidth_clearloop22
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
	lda #9
showTitle_rightvarAddSub_var24 = $54
	sta showTitle_rightvarAddSub_var24
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var24
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
	ldx #$12 ; optimized, look out for bugs
	jsr printstring
	
; // Center the author text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #11
showTitle_rightvarAddSub_var27 = $54
	sta showTitle_rightvarAddSub_var27
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var27
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
showTitle_rightvarAddSub_var30 = $54
	sta showTitle_rightvarAddSub_var30
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var30
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
	
; // Plasma procedure
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Plasma
	;    Procedure type : User-defined procedure
Plasma
	lda ax
	; Calling storevariable on generic assign expression
	sta c2x
	lda ay
	; Calling storevariable on generic assign expression
	sta c2y
	lda #$0
	; Calling storevariable on generic assign expression
	sta x
Plasma_forloop34
	
; // Set up y-sine table
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Unknown type array, assuming BYTE
	ldx c2x
	lda sine,x
	clc
	; Load Unknown type array, assuming BYTE
	ldx c2y
	adc sine,x
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	ldx x ; optimized, look out for bugs
	sta siny,x
	; Optimizer: a = a +/- b
	lda c2x
	clc
	adc #$4
	sta c2x
	; Optimizer: a = a +/- b
	lda c2y
	clc
	adc #$9
	sta c2y
Plasma_forloopcounter36
Plasma_loopstart37
	; Compare is onpage
	; Test Inc dec D
	inc x
	lda #$19
	cmp x ;keep
	bne Plasma_forloop34
Plasma_loopdone41: ;keep
Plasma_forloopend35
Plasma_loopend38
	; Optimizer: a = a +/- b
	lda ax
	clc
	adc #$3
	sta ax
	; Optimizer: a = a +/- b
	lda ay
	sec
	sbc #$5
	sta ay
	lda #$0
	; Calling storevariable on generic assign expression
	sta x
Plasma_forloop42
	
; // Set up x-sine table
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Unknown type array, assuming BYTE
	ldx c2x
	lda sine,x
	clc
	; Load Unknown type array, assuming BYTE
	ldx c2y
	adc sine,x
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	ldx x ; optimized, look out for bugs
	sta sinx,x
	; Optimizer: a = a +/- b
	lda c2x
	clc
	adc #$3
	sta c2x
	; Optimizer: a = a +/- b
	lda c2y
	clc
	adc #$7
	sta c2y
Plasma_forloopcounter44
Plasma_loopstart45
	; Compare is onpage
	; Test Inc dec D
	inc x
	lda myscreenwidth
	cmp x ;keep
	bne Plasma_forloop42
Plasma_loopdone49: ;keep
Plasma_forloopend43
Plasma_loopend46
	
; // Move cursor to(1,y) on $0400 on bank 1	
; // moveto could also be replaced with : screenmemory:=$0400 + @y_start*40;
	; MoveTo optimization
	lda #$00
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	lda #$0
	; Calling storevariable on generic assign expression
	sta y
Plasma_forloop50
	
; // Full screen, runs slower
; //moveto(0,screen_height/2, hi(screen_char_loc)); 
; // Half screen, runs faster
	; Load Byte array
	ldx y
	lda siny,x
	; Calling storevariable on generic assign expression
	sta val
	lda #$0
	; Calling storevariable on generic assign expression
	sta x
Plasma_forloop67
	
; // here, we take(sin[x]+val) and divide by 16. However, since this is a slow procedure,
; // we have created a lookup table instead!
	; Load Byte array
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Byte array
	ldx x
	lda sinx,x
	clc
	adc val
	 ; end add / sub var with constant
	tax
	lda lookupDiv,x
	; Calling storevariable on generic assign expression
	sta c
	
; // Set the screen memory
	; Load Byte array
	tax ; optimized x, look out for bugs L22 ORG 	ldx c
	lda vals,x
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy x ; optimized, look out for bugs
	sta (screenmemory),y
Plasma_forloopcounter69
Plasma_loopstart70
	; Compare is onpage
	; Test Inc dec D
	inc x
	lda myscreenwidth
	cmp x ;keep
	bne Plasma_forloop67
Plasma_loopdone74: ;keep
Plasma_forloopend68
Plasma_loopend71
	
; // Increase screen memory pointer by MYSCREENWIDTH(next row)
	lda screenmemory
	clc
	adc myscreenwidth
	sta screenmemory+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Plasma_WordAdd75
	inc screenmemory+1
Plasma_WordAdd75
	
; // Increase color pointer by MYSCREENWIDTH(next row)
	lda colorP
	clc
	adc myscreenwidth
	sta colorP+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Plasma_WordAdd76
	inc colorP+1
Plasma_WordAdd76
Plasma_forloopcounter52
Plasma_loopstart53
	; Compare is onpage
	; Test Inc dec D
	inc y
	lda #$19
	cmp y ;keep
	bne Plasma_forloop50
Plasma_loopdone77: ;keep
Plasma_forloopend51
Plasma_loopend54
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitDivision
	;    Procedure type : User-defined procedure
InitDivision
	lda #$0
	; Calling storevariable on generic assign expression
	sta x
InitDivision_forloop79
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda x
	lsr
	lsr
	lsr
	lsr
	lsr
	; Calling storevariable on generic assign expression
	ldx x ; optimized, look out for bugs
	sta lookupDiv,x
InitDivision_forloopcounter81
InitDivision_loopstart82
	; Compare is onpage
	; Test Inc dec D
	inc x
	; Integer constant assigning
	ldy #$01
	lda #$00
	cmp x ;keep
	bne InitDivision_forloop79
InitDivision_loopdone86: ;keep
InitDivision_forloopend80
InitDivision_loopend83
	rts
block1
	
; // Simply store values divided by 16
; // Show the title and check number of columns
	jsr showTitle
	
; // Set charmap location at $2000
	jsr InitDivision
	lda #$1
	; Calling storevariable on generic assign expression
	sta ax
	lda #$5
	; Calling storevariable on generic assign expression
	sta ay
	
; // Clear screen and color memory
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop87
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop87
MainProgram_while88
MainProgram_loopstart92
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock91
MainProgram_ConditionalTrueBlock89: ;Main true block ;keep 
	
; // Main loop
	jsr Plasma
	
; // Trigger NMI	    
	; Binary clause Simplified: EQUALS
	jsr GetKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock107
MainProgram_ConditionalTrueBlock105: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop111
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop111
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock107
	jmp MainProgram_while88
MainProgram_elsedoneblock91
MainProgram_loopend93
	; End of program
	; Ending memory block at $410
EndBlock410:
