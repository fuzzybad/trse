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
Sines
		jsr initsine_calculate
	jmp block1
i	dc.b	0
j	dc.b	0
k	dc.b	0
temp	dc.b	0
x1	dc.b	0
zp	= $68
tp	=  $6A
vals	dc.b $020, $02e, $0a6, $0a0, $0a0, $0a6, $02e, $020
list	dc.b	 
	org list+256
titlemsg		dc.b	"TRSE 'SINES' DEMO"
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
	
; // Used for keyboard input
; // Text for splash screen  	
; // User selection for screen width 
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
	lda #8
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
	ldx #$11 ; optimized, look out for bugs
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
	; ***********  Defining procedure : InitTabs
	;    Procedure type : User-defined procedure
InitTabs
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
InitTabs_forloop35
	
; //for i:=0 to 255 do list[i]:=vals[(sine[i]/13)&7]; 
; // original
; //for i:=0 to 255 do list[i]:=vals[(sine[i]/3)&11]; 
; // clean, cool looking(11/11)
	; Load Byte array
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit div
	; Load Unknown type array, assuming BYTE
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
	; Calling storevariable on generic assign expression
	ldx i ; optimized, look out for bugs
	sta list,x
InitTabs_forloopcounter37
InitTabs_loopstart38
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$ff
	cmp i ;keep
	bne InitTabs_forloop35
InitTabs_loopdone46: ;keep
InitTabs_forloopend36
InitTabs_loopend39
	rts
	
; // looks like a wave(13/9, 9/11)
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Render
	;    Procedure type : User-defined procedure
Render
	lda k
	; Calling storevariable on generic assign expression
	sta j
	lda #$00
	ldx #$80
	sta zp
	stx zp+1
	; Optimizer: a = a +/- b
	lda k
	clc
	adc #$2
	sta k
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
Render_forloop48
	
; // Speed control
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load Unknown type array, assuming BYTE
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
	; Calling storevariable on generic assign expression
	sta j
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load Unknown type array, assuming BYTE
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
	; Calling storevariable on generic assign expression
	sta j
	
; //tp:=#list;		
	; INTEGER optimization: a=b+c 
	lda #<list
	clc
	adc j
	sta tp+0
	lda #>list
	adc #0
	sta tp+1
	
; //tp:=tp + j;
	; memcpy
	ldy #0
Render_memcpy59
	lda (tp),y
	sta (zp),y
	iny
	cpy myscreenwidth
	bne Render_memcpy59
	lda zp
	clc
	adc myscreenwidth
	sta zp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Render_WordAdd60
	inc zp+1
Render_WordAdd60
	; Test Inc dec D
	inc j
Render_forloopcounter50
Render_loopstart51
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$19
	cmp i ;keep
	bne Render_forloop48
Render_loopdone61: ;keep
Render_forloopend49
Render_loopend52
	rts
block1
	
; // Show the title and check number of columns
	jsr showTitle
	jsr InitTabs
MainProgram_while62
MainProgram_loopstart66
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock65
MainProgram_ConditionalTrueBlock63: ;Main true block ;keep 
	jsr Render
	
; // Trigger NMI	    
	; Binary clause Simplified: EQUALS
	jsr GetKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock81
MainProgram_ConditionalTrueBlock79: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop85
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop85
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock81
	jmp MainProgram_while62
MainProgram_elsedoneblock65
MainProgram_loopend67
	; End of program
	; Ending memory block at $410
EndBlock410:
