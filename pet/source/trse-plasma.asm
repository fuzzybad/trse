 processor 6502
	org $400
	; Starting new memory block at $400
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $31,$30,$34,$30
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock5881
	org $410
	; Starting new memory block at $410
Tutorial3_plasma
		jsr initsine_calculate
	jmp block1
c	dc.b	
val	dc.b	
c2x	dc.b	
c2y	dc.b	
ax	dc.b	
ay	dc.b	
x	dc.b	
y	dc.b	
colorP	= $02
key	dc.b	
numkeys	dc.b	
titlemsg		dc.b	"TRSE EXAMPLE 6  'PLASMA'"
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
siny	dc.b	 
	org siny+25
sinx	dc.b	 
	org sinx+80
lookupDiv	dc.b	 
	org lookupDiv+256
vals	dc.b $020, $02e, $0a6, $0a0, $0a0, $0a6, $02e, $020
	dc.b 
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
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto2
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
initmoveto_moveto2
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
	; ***********  Defining procedure : initsinetable
	;    Procedure type : Built-in function
	;    Requires initialization : no
sine .byte 0 
	org sine +#255
value .word 0
delta .word 0
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
showTitle_clearloop5
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne showTitle_clearloop5
	
; // Show the title text
	; MoveTo optimization
	lda #$58
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
	ldx #$18 ; optimized, look out for bugs
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
showTitle_while14
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne showTitle_elsedoneblock17
showTitle_ConditionalTrueBlock15: ;Main true block ;keep 
	
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
	bcc showTitle_elsedoneblock53
	beq showTitle_elsedoneblock53
showTitle_ConditionalTrueBlock51: ;Main true block ;keep 
	; Assigning single variable : key
	jsr getKey
	; Calling storevariable
	sta key
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne showTitle_elsedoneblock71
showTitle_ConditionalTrueBlock69: ;Main true block ;keep 
	
; // 52 is '4'
	; Assigning single variable : myscreenwidth
	lda #$28
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock71
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne showTitle_elsedoneblock77
showTitle_ConditionalTrueBlock75: ;Main true block ;keep 
	
; // 56 is '8'
	; Assigning single variable : myscreenwidth
	lda #$50
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock77
showTitle_elsedoneblock53
	jmp showTitle_while14
showTitle_elsedoneblock17
	rts
	
; // Plasma procedure
	; ***********  Defining procedure : Plasma
	;    Procedure type : User-defined procedure
Plasma
	; Assigning single variable : c2x
	lda ax
	; Calling storevariable
	sta c2x
	; Assigning single variable : c2y
	lda ay
	; Calling storevariable
	sta c2y
	; Assigning single variable : x
	lda #$0
	; Calling storevariable
	sta x
Plasma_forloop81
	
; // Set up y-sine table
	; Assigning single variable : siny
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Unknown type array
	ldx c2x
	lda sine,x
	clc
	; Load Unknown type array
	ldx c2y
	adc sine,x
	 ; end add / sub var with constant
	; Calling storevariable
	ldx x ; optimized, look out for bugs
	sta siny,x
	; Assigning single variable : c2x
	; Optimizer: a = a +/- b
	lda c2x
	clc
	adc #$4
	sta c2x
	; Assigning single variable : c2y
	; Optimizer: a = a +/- b
	lda c2y
	clc
	adc #$9
	sta c2y
	; IS ONPAGE
	inc x
	lda #$19
	cmp x ;keep
	bne Plasma_forloop81
Plasma_loopdone84: ;keep
	; Assigning single variable : ax
	; Optimizer: a = a +/- b
	lda ax
	clc
	adc #$3
	sta ax
	; Assigning single variable : ay
	; Optimizer: a = a +/- b
	lda ay
	sec
	sbc #$5
	sta ay
	; Assigning single variable : x
	lda #$0
	; Calling storevariable
	sta x
Plasma_forloop85
	
; // Set up x-sine table
	; Assigning single variable : sinx
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Unknown type array
	ldx c2x
	lda sine,x
	clc
	; Load Unknown type array
	ldx c2y
	adc sine,x
	 ; end add / sub var with constant
	; Calling storevariable
	ldx x ; optimized, look out for bugs
	sta sinx,x
	; Assigning single variable : c2x
	; Optimizer: a = a +/- b
	lda c2x
	clc
	adc #$3
	sta c2x
	; Assigning single variable : c2y
	; Optimizer: a = a +/- b
	lda c2y
	clc
	adc #$7
	sta c2y
	; IS ONPAGE
	inc x
	lda myscreenwidth
	cmp x ;keep
	bne Plasma_forloop85
Plasma_loopdone88: ;keep
	
; // Move cursor to(1,y) on $0400 on bank 1	
; // moveto could also be replaced with : screenmemory:=$0400 + @y_start*40;
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
Plasma_forloop89
	
; // Full screen, runs slower
; //moveto(0,screen_height/2, hi(screen_char_loc)); 
; // Half screen, runs faster
	; Assigning single variable : val
	; Load Byte array
	ldx y
	lda siny,x
	; Calling storevariable
	sta val
	; Assigning single variable : x
	lda #$0
	; Calling storevariable
	sta x
Plasma_forloop91
	
; // here, we take(sin[x]+val) and divide by 16. However, since this is a slow procedure,
; // we have created a lookup table instead!
	; Assigning single variable : c
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
	; Calling storevariable
	sta c
	
; // Set the screen memory
	; Assigning single variable : screenmemory
	; Load Byte array
	tax ; optimized x, look out for bugs L22 ORG 	ldx c
	lda vals,x
	; Calling storevariable
	ldy x ; optimized, look out for bugs
	sta (screenmemory),y
	; IS ONPAGE
	inc x
	lda myscreenwidth
	cmp x ;keep
	bne Plasma_forloop91
Plasma_loopdone94: ;keep
	
; // Increase screen memory pointer by MYSCREENWIDTH(next row)
	; Assigning single variable : screenmemory
	lda screenmemory
	clc
	adc myscreenwidth
	sta screenmemory+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Plasma_WordAdd95
	inc screenmemory+1
Plasma_WordAdd95
	
; // Increase color pointer by MYSCREENWIDTH(next row)
	; Assigning single variable : colorP
	lda colorP
	clc
	adc myscreenwidth
	sta colorP+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Plasma_WordAdd96
	inc colorP+1
Plasma_WordAdd96
	; IS ONPAGE
	inc y
	lda #$19
	cmp y ;keep
	bne Plasma_forloop89
Plasma_loopdone104: ;keep
	rts
	; ***********  Defining procedure : InitDivision
	;    Procedure type : User-defined procedure
InitDivision
	; Assigning single variable : x
	lda #$0
	; Calling storevariable
	sta x
InitDivision_forloop106
	; Assigning single variable : lookupDiv
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda x
	lsr
	lsr
	lsr
	lsr
	lsr
	; Calling storevariable
	ldx x ; optimized, look out for bugs
	sta lookupDiv,x
	; IS ONPAGE
	inc x
	; Integer constant assigning
	ldy #$01
	lda #$00
	cmp x ;keep
	bne InitDivision_forloop106
InitDivision_loopdone109: ;keep
	rts
block1
	
; // Simply store values divided by 16
; // Show the title and check number of columns
	jsr showTitle
	
; // Set charmap location at $2000
	jsr InitDivision
	; Assigning single variable : ax
	lda #$1
	; Calling storevariable
	sta ax
	; Assigning single variable : ay
	lda #$5
	; Calling storevariable
	sta ay
	
; // Clear screen and color memory
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop110
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop110
MainProgram_while111
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock114
MainProgram_ConditionalTrueBlock112: ;Main true block ;keep 
	
; // Main loop
	jsr Plasma
	; Binary clause Simplified: EQUALS
	jsr getKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock128
MainProgram_ConditionalTrueBlock126: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop132
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop132
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock128
	jmp MainProgram_while111
MainProgram_elsedoneblock114
EndSymbol
	; End of program
	; Ending memory block
EndBlock5883
