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
Kaleido
	jmp block1
centerX	dc.b	$28
centerY	dc.b	$0c
start_pos	dc.b	$01
stop_pos	dc.b	$14
dir	dc.b	$01
char_start	dc.b	0
char_start_st	dc.b	$41
char_offset	dc.b	$3f
myscreenwidth	dc.b	$50
num_chars	dc.b	$06
speed	dc.b	$03
rev_enable	dc.b	$00
pattern_type	dc.b	$01
full_screen	dc.b	$01
rand	dc.b	$00
char_arr	dc.b	 
	org char_arr+12
p	= $68
plotIndex	dc.b	0
plotIndexMax	dc.b	0
mainIndex	dc.b	0
colSelect	dc.b	0
i	dc.b	0
temp	dc.b	0
x1	dc.b	0
y1	dc.b	0
x2	dc.b	0
y2	dc.b	0
key	dc.b	0
titlemsg		dc.b	"KALEIDOSCOPE+"
	dc.b	0
authormsg1		dc.b	"JESSICA PETERSEN <FUZZYBAD>, 2021"
	dc.b	0
authormsg2		dc.b	"BASED ON ROUTINE BY"
	dc.b	0
authormsg3		dc.b	"RUGG AND FELDMAN, 1979"
	dc.b	0
inst1		dc.b	"0-9 - ADJUST SPEED"
	dc.b	0
inst2		dc.b	"Q   - QUIT"
	dc.b	0
promptmsg		dc.b	208
	dc.b	210
	dc.b	197
	dc.b	211
	dc.b	211
	dc.b	32
	dc.b	193
	dc.b	206
	dc.b	217
	dc.b	32
	dc.b	203
	dc.b	197
	dc.b	217
	dc.b	0
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
	; ***********  Defining procedure : initprintdecimal
	;    Procedure type : Built-in function
	;    Requires initialization : no
ipd_div_hi: dc.b 0
ipd_div_lo: dc.b 0
init_printdecimal_div10
	ldx #$11
	lda #$00
	clc
init_printdecimal_loop
	rol
	cmp #$0A
	bcc init_printdecimal_skip
	sbc #$0A
init_printdecimal_skip
	rol ipd_div_lo
	rol ipd_div_hi
	dex
	bne init_printdecimal_loop
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
	; ***********  Defining procedure : initrandom256
	;    Procedure type : User-defined procedure
	; init random256
Random
	lda #$01
	asl
	bcc initrandom256_RandomSkip4
	eor #$4d
initrandom256_RandomSkip4
	sta Random+1
	rts
	
; // Used for keyboard input
; // Text for splash screen  	
; // PRESS ANY KEY
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
WaitForKeypress_while7
WaitForKeypress_loopstart11
	; Binary clause Simplified: EQUALS
	lda temp
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne WaitForKeypress_elsedoneblock10
WaitForKeypress_ConditionalTrueBlock8: ;Main true block ;keep 
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta temp
	jmp WaitForKeypress_while7
WaitForKeypress_elsedoneblock10
WaitForKeypress_loopend12
	rts
	
; // Do timing delay 
; // @TODO: Find better way than delay loop
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DoDelay
	;    Procedure type : User-defined procedure
DoDelay
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda speed
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc DoDelay_elsedoneblock19
DoDelay_ConditionalTrueBlock17: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
DoDelay_forloop30
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
DoDelay_forloopcounter32
DoDelay_loopstart33
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda speed
	cmp i ;keep
	bne DoDelay_forloop30
DoDelay_loopdone37: ;keep
DoDelay_forloopend31
DoDelay_loopend34
DoDelay_elsedoneblock19
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
	bne SetScreenWidth_elseblock41
SetScreenWidth_ConditionalTrueBlock40: ;Main true block ;keep 
	
; // Determine PET model by checking first byte of EDIT ROM at $E000
; //	$A0 [160] = B1
; //	$48 [72]  = B2
; //	$36 [54]  = B4-40
; //	$4B [75]  = B4-80	
	lda #$50
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	jmp SetScreenWidth_elsedoneblock42
SetScreenWidth_elseblock41
	lda #$28
	; Calling storevariable on generic assign expression
	sta myscreenwidth
SetScreenWidth_elsedoneblock42
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta centerX
	; Clear screen with offset
	lda #$20
	ldx #$fa
SetScreenWidth_clearloop47
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne SetScreenWidth_clearloop47
	rts
	
; //	Method which shows title screen and checks screen width
	; NodeProcedureDecl -1
	; ***********  Defining procedure : ShowTitle
	;    Procedure type : User-defined procedure
ShowTitle
	
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
	lda #6
ShowTitle_rightvarAddSub_var49 = $54
	sta ShowTitle_rightvarAddSub_var49
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var49
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
	ldx #$d ; optimized, look out for bugs
	jsr printstring
	
; // Center the author message
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #16
ShowTitle_rightvarAddSub_var52 = $54
	sta ShowTitle_rightvarAddSub_var52
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var52
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$2
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<authormsg1
	adc #$0
	ldy #>authormsg1
	sta print_text+0
	sty print_text+1
	ldx #$21 ; optimized, look out for bugs
	jsr printstring
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #9
ShowTitle_rightvarAddSub_var55 = $54
	sta ShowTitle_rightvarAddSub_var55
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var55
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$4
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<authormsg2
	adc #$0
	ldy #>authormsg2
	sta print_text+0
	sty print_text+1
	ldx #$13 ; optimized, look out for bugs
	jsr printstring
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #11
ShowTitle_rightvarAddSub_var58 = $54
	sta ShowTitle_rightvarAddSub_var58
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var58
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$5
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<authormsg3
	adc #$0
	ldy #>authormsg3
	sta print_text+0
	sty print_text+1
	ldx #$16 ; optimized, look out for bugs
	jsr printstring
	
; // Display Controls
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc #$9
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$7
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<inst1
	adc #$0
	ldy #>inst1
	sta print_text+0
	sty print_text+1
	ldx #$12 ; optimized, look out for bugs
	jsr printstring
	lda x1
	sta screen_x
	lda #$8
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<inst2
	adc #$0
	ldy #>inst2
	sta print_text+0
	sty print_text+1
	ldx #$a ; optimized, look out for bugs
	jsr printstring
	
; // Center prompt to continue
; // TRSE seems bugged at counting length of numeric strings
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
ShowTitle_rightvarAddSub_var65 = $54
	sta ShowTitle_rightvarAddSub_var65
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var65
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$c
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<promptmsg
	adc #$0
	ldy #>promptmsg
	sta print_text+0
	sty print_text+1
	ldx #$d ; optimized, look out for bugs
	jsr printstring
	jsr WaitForKeypress
	rts
	
; // Fill char array
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitData
	;    Procedure type : User-defined procedure
InitData
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
InitData_forloop69
	
; // Set center based on screen width setting
; //centerX := myscreenwidth / 2 - 1;
; // line 150
; // Populate char_arr with 50/50 split between these ranges:
; // 65-128 and 193-256
	lda char_start_st
	; Calling storevariable on generic assign expression
	sta char_start
	
; // line 165
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne InitData_elsedoneblock97
InitData_ConditionalTrueBlock95: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	jsr Random
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc InitData_elsedoneblock109
InitData_ConditionalTrueBlock107: ;Main true block ;keep 
	
; // Option for reverse chars
; // line 165
	; Optimizer: a = a +/- b
	lda char_start
	clc
	adc #$80
	sta char_start
InitData_elsedoneblock109
InitData_elsedoneblock97
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and char_offset
	 ; end add / sub var with constant
	clc
	adc char_start
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	ldx i ; optimized, look out for bugs
	sta char_arr,x
InitData_forloopcounter71
InitData_loopstart72
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda num_chars
	cmp i ;keep
	bne InitData_forloop69
InitData_loopdone112: ;keep
InitData_forloopend70
InitData_loopend73
	rts
	
; // Check for any user inputs
	; NodeProcedureDecl -1
	; ***********  Defining procedure : CheckInputs
	;    Procedure type : User-defined procedure
CheckInputs
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta key
	
; // NMI	    
; //call(^$fd16);	
; // RESET		
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$51;keep
	bne CheckInputs_elsedoneblock117
CheckInputs_ConditionalTrueBlock115: ;Main true block ;keep 
	
; // Q - Quit
	; Clear screen with offset
	lda #$20
	ldx #$fa
CheckInputs_clearloop121
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne CheckInputs_clearloop121
	jsr $fd49
CheckInputs_elsedoneblock117
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$30;keep
	bne CheckInputs_elsedoneblock125
CheckInputs_ConditionalTrueBlock123: ;Main true block ;keep 
	
; // 0-9 Adjust speed 
	lda #$9
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock125
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$31;keep
	bne CheckInputs_elsedoneblock131
CheckInputs_ConditionalTrueBlock129: ;Main true block ;keep 
	
; //(slowest)
	lda #$8
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock131
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$32;keep
	bne CheckInputs_elsedoneblock137
CheckInputs_ConditionalTrueBlock135: ;Main true block ;keep 
	lda #$7
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock137
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$33;keep
	bne CheckInputs_elsedoneblock143
CheckInputs_ConditionalTrueBlock141: ;Main true block ;keep 
	lda #$6
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock143
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne CheckInputs_elsedoneblock149
CheckInputs_ConditionalTrueBlock147: ;Main true block ;keep 
	lda #$5
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock149
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$35;keep
	bne CheckInputs_elsedoneblock155
CheckInputs_ConditionalTrueBlock153: ;Main true block ;keep 
	lda #$4
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock155
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$36;keep
	bne CheckInputs_elsedoneblock161
CheckInputs_ConditionalTrueBlock159: ;Main true block ;keep 
	lda #$3
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock161
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$37;keep
	bne CheckInputs_elsedoneblock167
CheckInputs_ConditionalTrueBlock165: ;Main true block ;keep 
	lda #$2
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock167
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne CheckInputs_elsedoneblock173
CheckInputs_ConditionalTrueBlock171: ;Main true block ;keep 
	lda #$1
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock173
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$39;keep
	bne CheckInputs_elsedoneblock179
CheckInputs_ConditionalTrueBlock177: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock179
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$53;keep
	bne CheckInputs_elsedoneblock185
CheckInputs_ConditionalTrueBlock183: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne CheckInputs_elseblock199
CheckInputs_ConditionalTrueBlock198: ;Main true block ;keep 
	
; //(fastest)
; // S - Toggle 40/80 Col Screen
	lda #$50
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	jmp CheckInputs_elsedoneblock200
CheckInputs_elseblock199
	lda #$28
	; Calling storevariable on generic assign expression
	sta myscreenwidth
CheckInputs_elsedoneblock200
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta centerX
	; Clear screen with offset
	lda #$20
	ldx #$fa
CheckInputs_clearloop205
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne CheckInputs_clearloop205
CheckInputs_elsedoneblock185
	rts
	
; // Poke value into an address
	; NodeProcedureDecl -1
	; ***********  Defining procedure : PokeAddr
	;    Procedure type : User-defined procedure
pokeaddr_x	dc.b	0
pokeaddr_y	dc.b	0
pokeaddr_v	dc.b	0
PokeAddr_block206
PokeAddr
	; Swapped comparison expressions
	; Binary clause Simplified: LESS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Compare with pure num / var optimization
	cmp pokeaddr_x;keep
	bcs PokeAddr_elsedoneblock210
PokeAddr_ConditionalTrueBlock208: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock210
	; Binary clause Simplified: LESS
	lda pokeaddr_x
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock216
PokeAddr_ConditionalTrueBlock214: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock216
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcc PokeAddr_elsedoneblock222
PokeAddr_ConditionalTrueBlock220: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock222
	; Binary clause Simplified: LESS
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock228
PokeAddr_ConditionalTrueBlock226: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock228
	
; // Pointer to screen RAM
	; Generic 16 bit op
	ldy #0
	lda pokeaddr_x
PokeAddr_rightvarInteger_var233 = $54
	sta PokeAddr_rightvarInteger_var233
	sty PokeAddr_rightvarInteger_var233+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$80
	lda #$00
PokeAddr_rightvarInteger_var236 =  $56
	sta PokeAddr_rightvarInteger_var236
	sty PokeAddr_rightvarInteger_var236+1
	; Mul 16x8 setup
	ldy #0
	lda myscreenwidth
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda pokeaddr_y
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var236
PokeAddr_wordAdd234
	sta PokeAddr_rightvarInteger_var236
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var236+1
	tay
	lda PokeAddr_rightvarInteger_var236
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var233
PokeAddr_wordAdd231
	sta PokeAddr_rightvarInteger_var233
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var233+1
	tay
	lda PokeAddr_rightvarInteger_var233
	sta p
	sty p+1
	; Poke
	lda pokeaddr_v
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (p),y
	rts
	
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
; // Plot chars on screen
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Plot
	;    Procedure type : User-defined procedure
Plot
	
; // Debug
; //PrintXYZ(x2, y2, 0);
; //WaitForKeypress();
; // Call timing delay
	jsr DoDelay
	
; // Map chars in the up/down/across indexes
	lda x1
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y1
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	lda char_arr +$0 ; array with const index optimization
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
	; Binary clause Simplified: EQUALS
	lda mainIndex
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne Plot_elsedoneblock241
Plot_ConditionalTrueBlock239: ;Main true block ;keep 
	
; // line 900
	rts
Plot_elsedoneblock241
	
; // line 910
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda mainIndex
	lsr
	clc
	adc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta colSelect
	
; // line 920
; // Modifies character range selection and will take effect 
; // next time InitData() runs
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda mainIndex
	sec
	sbc colSelect
	 ; end add / sub var with constant
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta char_start
	; Binary clause Simplified: EQUALS
	lda pattern_type
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne Plot_elseblock246
Plot_ConditionalTrueBlock245: ;Main true block ;keep 
	
; // line 920
; // Select pattern style
	lda colSelect
	; Calling storevariable on generic assign expression
	sta plotIndexMax
	jmp Plot_elsedoneblock247
Plot_elseblock246
	lda mainIndex
	; Calling storevariable on generic assign expression
	sta plotIndexMax
Plot_elsedoneblock247
	lda #$1
	; Calling storevariable on generic assign expression
	sta plotIndex
Plot_forloop252
	; Binary clause Simplified: EQUALS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bne Plot_localfailed397
	jmp Plot_ConditionalTrueBlock351
Plot_localfailed397
	jmp Plot_elseblock352
Plot_ConditionalTrueBlock351: ;Main true block ;keep 
	
; // Map chars surrounding the up/down/across indexes
; // line 930
; // line 940
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x1
	clc
	adc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x2
	lda y1
	; Calling storevariable on generic assign expression
	sta y2
	
; // Fills right of vertical
	lda x2
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x1
	sec
	sbc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x2
	
; // Fills left of vertical
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
	jmp Plot_elsedoneblock353
Plot_elseblock352
	; Binary clause Simplified: EQUALS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bne Plot_localfailed422
	jmp Plot_ConditionalTrueBlock401
Plot_localfailed422
	jmp Plot_elseblock402
Plot_ConditionalTrueBlock401: ;Main true block ;keep 
	
; // line 950
	lda x1
	; Calling storevariable on generic assign expression
	sta x2
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y1
	clc
	adc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y2
	
; // Fills below horizontal
	lda x2
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y1
	sec
	sbc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y2
	
; // Fills above horizontal
	lda x2
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
	jmp Plot_elsedoneblock403
Plot_elseblock402
	lda y1
	; Calling storevariable on generic assign expression
	sta y2
	; Binary clause Simplified: LESS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bpl Plot_elseblock427
Plot_ConditionalTrueBlock426: ;Main true block ;keep 
	
; // line 970
; // line 970
; // Fills left side diagonal
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x1
	clc
	adc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x2
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
	jmp Plot_elsedoneblock428
Plot_elseblock427
	
; // Fills right side diagonal
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x1
	sec
	sbc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x2
	
; // line 980
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
Plot_elsedoneblock428
	lda x1
	; Calling storevariable on generic assign expression
	sta x2
	; Binary clause Simplified: LESS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bpl Plot_elseblock435
Plot_ConditionalTrueBlock434: ;Main true block ;keep 
	
; // line 990
; // line 990
; // Fills top corners
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y1
	clc
	adc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y2
	lda x2
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
	jmp Plot_elsedoneblock436
Plot_elseblock435
	
; // Fills bottom corners
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y1
	sec
	sbc plotIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y2
	
; // line 1000
	lda x2
	; Calling storevariable on generic assign expression
	sta pokeaddr_x
	lda y2
	; Calling storevariable on generic assign expression
	sta pokeaddr_y
	; Load Byte array
	ldx plotIndex
	lda char_arr,x
	; Calling storevariable on generic assign expression
	sta pokeaddr_v
	jsr PokeAddr
Plot_elsedoneblock436
Plot_elsedoneblock403
Plot_elsedoneblock353
Plot_forloopcounter254
Plot_loopstart255
	; Test Inc dec D
	inc plotIndex
	lda plotIndexMax
	cmp plotIndex ;keep
	beq Plot_loopdone441
Plot_loopnotdone442
	jmp Plot_forloop252
Plot_loopdone441
Plot_forloopend253
Plot_loopend256
	rts
	
; // for
; // Update the display
	; NodeProcedureDecl -1
	; ***********  Defining procedure : MainLoop
	;    Procedure type : User-defined procedure
MainLoop
	
; // line 280
	lda start_pos
	; Calling storevariable on generic assign expression
	sta mainIndex
MainLoop_forloop444
	
; // line 200
; // Draw Right
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerX
	clc
	adc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x1
	
; // line 210
	lda centerY
	; Calling storevariable on generic assign expression
	sta y1
	
; // line 210
	jsr Plot
	
; // line 210
; // Draw Left
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerX
	sec
	sbc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x1
	
; // line 220
	jsr Plot
	
; // line 220
; // Draw Down
	lda centerX
	; Calling storevariable on generic assign expression
	sta x1
	
; // line 230
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerY
	clc
	adc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y1
	
; // line 230
	jsr Plot
	
; // line 230
; // Draw Up
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerY
	sec
	sbc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y1
	
; // line 240
	jsr Plot
	
; // line 240
; // Lower right
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerX
	clc
	adc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x1
	
; // line 250
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerY
	clc
	adc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y1
	
; // line 250
	jsr Plot
	
; // line 250
; // Upper left
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerX
	sec
	sbc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x1
	
; // line 260
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerY
	sec
	sbc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y1
	
; // line 260
	jsr Plot
	
; // line 260
; // Lower left
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerY
	clc
	adc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y1
	
; // line 270
	jsr Plot
	
; // line 270
; // Upper right
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerX
	clc
	adc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x1
	
; // line 280
	; 8 bit binop
	; Add/sub where right value is constant number
	lda centerY
	sec
	sbc mainIndex
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y1
	
; // line 280
	jsr Plot
MainLoop_forloopcounter446
MainLoop_loopstart447
	; 8 bit binop
	; Add/sub where right value is constant number
	lda mainIndex
	clc
	adc dir
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta mainIndex
	lda stop_pos
	cmp mainIndex ;keep
	beq MainLoop_loopdone451
MainLoop_loopnotdone452
	jmp MainLoop_forloop444
MainLoop_loopdone451
MainLoop_forloopend445
MainLoop_loopend448
	
; // Pattern mutations below
; // Reverse direction(1/2 cycle)
	lda dir
	; Unary operator: Negate 8-bit number
	eor #$FF
	clc
	adc #1
	; Calling storevariable on generic assign expression
	sta dir
	lda start_pos
	; Calling storevariable on generic assign expression
	sta temp
	lda stop_pos
	; Calling storevariable on generic assign expression
	sta start_pos
	lda temp
	; Calling storevariable on generic assign expression
	sta stop_pos
	jsr Random
	; Calling storevariable on generic assign expression
	sta rand
	; Binary clause Simplified: EQUALS
	lda dir
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainLoop_localfailed480
	jmp MainLoop_ConditionalTrueBlock454
MainLoop_localfailed480
	jmp MainLoop_elsedoneblock456
MainLoop_ConditionalTrueBlock454: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda pattern_type
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainLoop_elseblock484
MainLoop_ConditionalTrueBlock483: ;Main true block ;keep 
	
; // Mutations every full cycle
; // Toggle pattern type
; // 4-point pattern
	lda #$0
	; Calling storevariable on generic assign expression
	sta pattern_type
	lda #$c
	; Calling storevariable on generic assign expression
	sta num_chars
	jmp MainLoop_elsedoneblock485
MainLoop_elseblock484
	
; // 8-point pattern
	lda #$1
	; Calling storevariable on generic assign expression
	sta pattern_type
	lda #$6
	; Calling storevariable on generic assign expression
	sta num_chars
MainLoop_elsedoneblock485
	; Binary clause Simplified: LESS
	lda rand
	; Compare with pure num / var optimization
	cmp #$20;keep
	bcs MainLoop_elsedoneblock493
MainLoop_ConditionalTrueBlock491: ;Main true block ;keep 
	
; // Randomly change center point
	; Right is PURE NUMERIC : Is word =0
	; 8 bit div
	lda myscreenwidth
	sta div8x8_d
	; Load right hand side
	lda #$5
	sta div8x8_c
	jsr div8x8_procedure
	; Calling storevariable on generic assign expression
	sta temp
	; 8 bit binop
	; Add/sub right value is variable/expression
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc temp
	 ; end add / sub var with constant
	lsr
MainLoop_rightvarAddSub_var502 = $54
	sta MainLoop_rightvarAddSub_var502
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and temp
	 ; end add / sub var with constant
	clc
	adc MainLoop_rightvarAddSub_var502
	; Calling storevariable on generic assign expression
	sta centerX
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and #$5
	 ; end add / sub var with constant
	clc
	adc #$a
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta centerY
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainLoop_clearloop503
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainLoop_clearloop503
MainLoop_elsedoneblock493
MainLoop_elsedoneblock456
	; Binary clause Simplified: LESS
	lda rand
	; Compare with pure num / var optimization
	cmp #$20;keep
	bcs MainLoop_elsedoneblock507
MainLoop_ConditionalTrueBlock505: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda rand
	; Compare with pure num / var optimization
	cmp #$8;keep
	bcs MainLoop_elseblock568
MainLoop_ConditionalTrueBlock567: ;Main true block ;keep 
	
; // Select char range
	lda #$4c
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$5
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
	jmp MainLoop_elsedoneblock569
MainLoop_elseblock568
	; Binary clause Simplified: LESS
	lda rand
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcs MainLoop_elseblock600
MainLoop_ConditionalTrueBlock599: ;Main true block ;keep 
	lda #$4c
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$1e
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
	jmp MainLoop_elsedoneblock601
MainLoop_elseblock600
	; Binary clause Simplified: LESS
	lda rand
	; Compare with pure num / var optimization
	cmp #$18;keep
	bcs MainLoop_elseblock616
MainLoop_ConditionalTrueBlock615: ;Main true block ;keep 
	lda #$74
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$c
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
	jmp MainLoop_elsedoneblock617
MainLoop_elseblock616
	lda #$41
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$3f
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
MainLoop_elsedoneblock617
MainLoop_elsedoneblock601
MainLoop_elsedoneblock569
MainLoop_elsedoneblock507
	; Binary clause Simplified: LESS
	lda rand
	; Compare with pure num / var optimization
	cmp #$40;keep
	bcs MainLoop_elsedoneblock625
MainLoop_ConditionalTrueBlock623: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainLoop_elseblock638
MainLoop_ConditionalTrueBlock637: ;Main true block ;keep 
	
; // Toggle reverse chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta rev_enable
	jmp MainLoop_elsedoneblock639
MainLoop_elseblock638
	lda #$1
	; Calling storevariable on generic assign expression
	sta rev_enable
MainLoop_elsedoneblock639
MainLoop_elsedoneblock625
	; Binary clause Simplified: LESS
	lda rand
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcs MainLoop_elsedoneblock647
MainLoop_ConditionalTrueBlock645: ;Main true block ;keep 
	
; // Toggle pattern size
; // Set pattern moving outward
	lda #$1
	; Calling storevariable on generic assign expression
	sta dir
	; Calling storevariable on generic assign expression
	sta start_pos
	; Binary clause Simplified: EQUALS
	lda full_screen
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainLoop_elseblock661
MainLoop_ConditionalTrueBlock660: ;Main true block ;keep 
	
; // Toggle the flag
	lda #$1
	; Calling storevariable on generic assign expression
	sta full_screen
	lda #$14
	; Calling storevariable on generic assign expression
	sta stop_pos
	jmp MainLoop_elsedoneblock662
MainLoop_elseblock661
	lda #$0
	; Calling storevariable on generic assign expression
	sta full_screen
	lda #$c
	; Calling storevariable on generic assign expression
	sta stop_pos
MainLoop_elsedoneblock662
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainLoop_clearloop667
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainLoop_clearloop667
MainLoop_elsedoneblock647
	rts
block1
	
; // Debug
; //PrintXYZ(temp, 0, 0);
; //WaitForKeypress();
; //call(^$fd49);	
; // Trigger NMI	
; // Show the title and check number of columns
	jsr ShowTitle
	
; // Init char array	
	jsr InitData
	
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop668
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop668
MainProgram_while669
MainProgram_loopstart673
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock672
MainProgram_ConditionalTrueBlock670: ;Main true block ;keep 
	jsr MainLoop
	
; // Check for user input	
	jsr CheckInputs
	
; // Get fresh array of chars
	jsr InitData
	jmp MainProgram_while669
MainProgram_elsedoneblock672
MainProgram_loopend674
	; End of program
	; Ending memory block at $410
EndBlock410:
