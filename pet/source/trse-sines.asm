 processor 6502
	org $400
	; Starting new memory block at $400
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $31,$30,$34,$30
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock3119
	org $410
	; Starting new memory block at $410
Sines
		jsr initsine_calculate
	jmp block1
i	dc.b	
j	dc.b	
k	dc.b	
zp	= $02
tp	= $04
vals	dc.b $020, $02e, $0a6, $0a0, $0a0, $0a6, $02e, $020
	dc.b 
list	dc.b	 
	org list+256
key	dc.b	
numkeys	dc.b	
titlemsg		dc.b	"TRSE EXAMPLE 8 'SINES'"
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
	lda #$59
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
	ldx #$16 ; optimized, look out for bugs
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
	; ***********  Defining procedure : InitTabs
	;    Procedure type : User-defined procedure
InitTabs
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
InitTabs_forloop82
	
; //for i:=0 to 0 do list[i]:=vals[(sine[i]/13)&7]; 
; // original
; //for i:=0 to 0 do list[i]:=vals[(sine[i]/3)&11]; 
; // clean, cool looking(11/11)
	; Assigning single variable : list
	; Load Byte array
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit div
	; Load Unknown type array
	ldx i
	lda sine,x
	sta div8x8_d
	; Load right hand side
	lda #$d
	sta div8x8_c
	jsr div8x8_procedure
	and #$9
	 ; end add / sub var with constant
	tax
	lda vals,x
	; Calling storevariable
	ldx i ; optimized, look out for bugs
	sta list,x
	; IS ONPAGE
	inc i
	lda #$0
	cmp i ;keep
	bne InitTabs_forloop82
InitTabs_loopdone89: ;keep
	rts
	
; // looks like a wave(13/9, 9/11)
	; ***********  Defining procedure : Render
	;    Procedure type : User-defined procedure
Render
	; Assigning single variable : j
	lda k
	; Calling storevariable
	sta j
	; Assigning single variable : zp
	lda #0
	ldx #128
	sta zp
	stx zp+1
	; Assigning single variable : k
	; Optimizer: a = a +/- b
	lda k
	clc
	adc #$2
	sta k
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
Render_forloop91
	
; // Speed control
	; Assigning single variable : j
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load Unknown type array
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda i
	asl
	clc
	adc k
	 ; end add / sub var with constant
	tax
	lda sine,x
	lsr
	; Calling storevariable
	sta j
	; Assigning single variable : j
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load Unknown type array
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	asl
	clc
	adc k
	 ; end add / sub var with constant
	tax
	lda sine,x
	lsr
	lsr
	clc
	adc j
	 ; end add / sub var with constant
	; Calling storevariable
	sta j
	; Assigning single variable : tp
	; INTEGER optimization: a=b+c 
	lda list
	clc
	adc j
	sta tp+0
	lda list+1
	adc #0
	sta tp+1
	; memcpy
	ldy #0
Render_memcpy94
	lda (tp),y
	sta (zp),y
	iny
	cpy myscreenwidth
	bne Render_memcpy94
	; Assigning single variable : zp
	lda zp
	clc
	adc myscreenwidth
	sta zp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Render_WordAdd95
	inc zp+1
Render_WordAdd95
	inc j
	; IS ONPAGE
	inc i
	lda #$19
	cmp i ;keep
	bne Render_forloop91
Render_loopdone102: ;keep
	rts
block1
	
; // Show the title and check number of columns
	jsr showTitle
	jsr InitTabs
MainProgram_while103
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock106
MainProgram_ConditionalTrueBlock104: ;Main true block ;keep 
	jsr Render
	; Binary clause Simplified: EQUALS
	jsr getKey
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
	jmp MainProgram_while103
MainProgram_elsedoneblock106
EndSymbol
	; End of program
	; Ending memory block
EndBlock3121
