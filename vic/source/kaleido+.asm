 processor 6502
	org $1001
StartBlock1001:
	; Starting new memory block at $1001
	.byte $b ; lo byte of next line
	.byte $10 ; hi byte of next line
	.byte $0a, $00 ; line 10 (lo, hi)
	.byte $9e, $20 ; SYS token and a space
	.byte   $34,$31,$31,$32
	.byte $00, $00, $00 ; end of program
	; Ending memory block at $1001
EndBlock1001:
	org $1010
StartBlock1010:
	; Starting new memory block at $1010
Kaleido
	jmp block1
myscreenwidth	dc.b	$16
myscreenheight	dc.b	$17
centerX	dc.b	$0c
centerY	dc.b	$0a
start_pos	dc.b	$01
stop_pos	dc.b	$0c
dir	dc.b	$01
char_start	dc.b	0
char_offset	dc.b	0
char_start_st	dc.b	$40
char_offset_st	dc.b	$3f
num_chars	dc.b	$06
speed	dc.b	$03
rev_enable	dc.b	$00
pattern_type	dc.b	$01
curr_color	dc.b	$01
char_arr	dc.b	 
	org char_arr+20
p	= $68
plotIndex	dc.b	0
plotIndexMax	dc.b	0
mainIndex	dc.b	0
colSelect	dc.b	0
i	dc.b	0
temp	dc.b	0
rando	dc.b	0
x1	dc.b	0
y1	dc.b	0
x2	dc.b	0
y2	dc.b	0
cArr1	dc.b $01, $07, $02, $07, $01
cArr2	dc.b $04, $05, $06, $05, $04
cArr3	dc.b $01, $03, $05, $03, $01
cArr4	dc.b $03, $07, $01, $07, $03
cArr5	dc.b $01, $02, $03, $04, $05, $06, $07, $06
	dc.b $05, $04, $03, $02, $01
colorIdx	dc.b	$00
cArrPtr	= $6A
cArrLen	dc.b	$00
cycleCtr	dc.b	$00
key	dc.b	0
titlemsg		dc.b	"KALEIDOSCOPE+"
	dc.b	0
authormsg1		dc.b	"JESSICA PETERSEN"
	dc.b	0
authormsg2		dc.b	"<FUZZYBAD> 2021"
	dc.b	0
authormsg3		dc.b	"BASED ON ROUTINE BY"
	dc.b	0
authormsg4		dc.b	"RUGG & FELDMAN, 1979"
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
	adc #22
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
ipd_div_hi = $5e
ipd_div_lo = $5f
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
	;    Procedure type : Built-in function
	;    Requires initialization : no
	; init random256
Random
	lda #$01
	asl
	bcc initrandom256_RandomSkip4
	eor #$4d
initrandom256_RandomSkip4
	eor $9124
	sta Random+1
	rts
	
; // Color arrays
; // wht, yel, red 
; // pur, grn, blu 
; // wht, cyn, grn 
; // cyn, yel, wht
; // All
; // VIC Colors
; // 0 = black, 1 = white, 2 = red, 3 = cyan, 4 = purple, 5 = green, 6 = blue, 7 = yellow
; // Used for keyboard input
; // Text for splash screen  	
; // PRESS ANY KEY
; // Include common functions
; //	Get a char from keyboard buffer
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
	
; // Poke value into an address
	; NodeProcedureDecl -1
	; ***********  Defining procedure : PokeAddr
	;    Procedure type : User-defined procedure
pokeaddr_x	dc.b	0
pokeaddr_y	dc.b	0
pokeaddr_v	dc.b	0
PokeAddr_block38
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
	bcs PokeAddr_elsedoneblock42
PokeAddr_ConditionalTrueBlock40: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock42
	; Binary clause Simplified: LESS
	lda pokeaddr_x
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock48
PokeAddr_ConditionalTrueBlock46: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock48
	; Swapped comparison expressions
	; Binary clause Simplified: LESS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenheight
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Compare with pure num / var optimization
	cmp pokeaddr_y;keep
	bcs PokeAddr_elsedoneblock54
PokeAddr_ConditionalTrueBlock52: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock54
	; Binary clause Simplified: LESS
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock60
PokeAddr_ConditionalTrueBlock58: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock60
	
; // Pointer value to screen RAM
	; Generic 16 bit op
	ldy #0
	lda pokeaddr_x
PokeAddr_rightvarInteger_var65 = $88
	sta PokeAddr_rightvarInteger_var65
	sty PokeAddr_rightvarInteger_var65+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$1e
	lda #$00
PokeAddr_rightvarInteger_var68 = $8A
	sta PokeAddr_rightvarInteger_var68
	sty PokeAddr_rightvarInteger_var68+1
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
	adc PokeAddr_rightvarInteger_var68
PokeAddr_wordAdd66
	sta PokeAddr_rightvarInteger_var68
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var68+1
	tay
	lda PokeAddr_rightvarInteger_var68
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var65
PokeAddr_wordAdd63
	sta PokeAddr_rightvarInteger_var65
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var65+1
	tay
	lda PokeAddr_rightvarInteger_var65
	sta p
	sty p+1
	; Poke
	lda pokeaddr_v
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (p),y
	
; // Pointer value to color RAM
	; Generic 16 bit op
	ldy #0
	lda pokeaddr_x
PokeAddr_rightvarInteger_var71 = $88
	sta PokeAddr_rightvarInteger_var71
	sty PokeAddr_rightvarInteger_var71+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$96
	lda #$00
PokeAddr_rightvarInteger_var74 = $8A
	sta PokeAddr_rightvarInteger_var74
	sty PokeAddr_rightvarInteger_var74+1
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
	adc PokeAddr_rightvarInteger_var74
PokeAddr_wordAdd72
	sta PokeAddr_rightvarInteger_var74
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var74+1
	tay
	lda PokeAddr_rightvarInteger_var74
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var71
PokeAddr_wordAdd69
	sta PokeAddr_rightvarInteger_var71
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var71+1
	tay
	lda PokeAddr_rightvarInteger_var71
	sta p
	sty p+1
	; Poke
	lda curr_color
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (p),y
	rts
	
; // Fill char array
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitData
	;    Procedure type : User-defined procedure
InitData
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
InitData_forloop76
	
; // line 150
; // Populate char_arr 
	lda char_start_st
	; Calling storevariable on generic assign expression
	sta char_start
	lda char_offset_st
	; Calling storevariable on generic assign expression
	sta char_offset
	
; // line 165
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne InitData_elsedoneblock104
InitData_ConditionalTrueBlock102: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	jsr Random
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc InitData_elsedoneblock116
InitData_ConditionalTrueBlock114: ;Main true block ;keep 
	
; // Option for reverse chars
; // line 165
	; Optimizer: a = a +/- b
	lda char_start
	clc
	adc #$80
	sta char_start
InitData_elsedoneblock116
InitData_elsedoneblock104
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
InitData_forloopcounter78
InitData_loopstart79
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda num_chars
	cmp i ;keep
	bne InitData_forloop76
InitData_loopdone119: ;keep
InitData_forloopend77
InitData_loopend80
	rts
	
; // Check for any user inputs
	; NodeProcedureDecl -1
	; ***********  Defining procedure : CheckInputs
	;    Procedure type : User-defined procedure
CheckInputs
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta key
	
; // RESET (SYS64802)
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$51;keep
	bne CheckInputs_elsedoneblock124
CheckInputs_ConditionalTrueBlock122: ;Main true block ;keep 
	
; // Q - Quit
	; Clear screen with offset
	lda #$20
	ldx #$fe
CheckInputs_clearloop128
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne CheckInputs_clearloop128
	jsr $fd22
CheckInputs_elsedoneblock124
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$30;keep
	bne CheckInputs_elsedoneblock132
CheckInputs_ConditionalTrueBlock130: ;Main true block ;keep 
	
; // 0-9 Adjust speed 
	lda #$9
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock132
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$31;keep
	bne CheckInputs_elsedoneblock138
CheckInputs_ConditionalTrueBlock136: ;Main true block ;keep 
	
; // (slowest)
	lda #$8
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock138
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$32;keep
	bne CheckInputs_elsedoneblock144
CheckInputs_ConditionalTrueBlock142: ;Main true block ;keep 
	lda #$7
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock144
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$33;keep
	bne CheckInputs_elsedoneblock150
CheckInputs_ConditionalTrueBlock148: ;Main true block ;keep 
	lda #$6
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock150
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne CheckInputs_elsedoneblock156
CheckInputs_ConditionalTrueBlock154: ;Main true block ;keep 
	lda #$5
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock156
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$35;keep
	bne CheckInputs_elsedoneblock162
CheckInputs_ConditionalTrueBlock160: ;Main true block ;keep 
	lda #$4
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock162
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$36;keep
	bne CheckInputs_elsedoneblock168
CheckInputs_ConditionalTrueBlock166: ;Main true block ;keep 
	lda #$3
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock168
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$37;keep
	bne CheckInputs_elsedoneblock174
CheckInputs_ConditionalTrueBlock172: ;Main true block ;keep 
	lda #$2
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock174
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne CheckInputs_elsedoneblock180
CheckInputs_ConditionalTrueBlock178: ;Main true block ;keep 
	lda #$1
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock180
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$39;keep
	bne CheckInputs_elsedoneblock186
CheckInputs_ConditionalTrueBlock184: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock186
	rts
	
; // (fastest)
; //	Method which shows title screen and checks screen width
	; NodeProcedureDecl -1
	; ***********  Defining procedure : ShowTitle
	;    Procedure type : User-defined procedure
ShowTitle
	
; // Set palette
	lda #<cArr1
	ldx #>cArr1
	sta cArrPtr
	stx cArrPtr+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta cArrLen
	
; // Set uppercase
	; Poke
	; Optimization: shift is zero
	lda #$c
	sta $e84c
	
; // 59468 or $E84C
; // Set black background/border
	; Poke
	; Optimization: shift is zero
	lda #$8
	sta $900f
	
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fe
ShowTitle_clearloop190
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne ShowTitle_clearloop190
	
; // Set default colormemory
	; Clear screen with offset
	lda #$3
	ldx #$fe
ShowTitle_clearloop191
	dex
	sta $0000+$9600,x
	sta $00fd+$9600,x
	bne ShowTitle_clearloop191
	
; // Center the title text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
ShowTitle_rightvarAddSub_var192 = $88
	sta ShowTitle_rightvarAddSub_var192
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var192
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$1
	sta screen_y
	lda #>$1e00
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
	lda #8
ShowTitle_rightvarAddSub_var195 = $88
	sta ShowTitle_rightvarAddSub_var195
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var195
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$3
	sta screen_y
	lda #>$1e00
	jsr SetScreenPosition
	clc
	lda #<authormsg1
	adc #$0
	ldy #>authormsg1
	sta print_text+0
	sty print_text+1
	ldx #$10 ; optimized, look out for bugs
	jsr printstring
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #7
ShowTitle_rightvarAddSub_var198 = $88
	sta ShowTitle_rightvarAddSub_var198
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var198
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$4
	sta screen_y
	lda #>$1e00
	jsr SetScreenPosition
	clc
	lda #<authormsg2
	adc #$0
	ldy #>authormsg2
	sta print_text+0
	sty print_text+1
	ldx #$f ; optimized, look out for bugs
	jsr printstring
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #9
ShowTitle_rightvarAddSub_var201 = $88
	sta ShowTitle_rightvarAddSub_var201
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var201
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$7
	sta screen_y
	lda #>$1e00
	jsr SetScreenPosition
	clc
	lda #<authormsg3
	adc #$0
	ldy #>authormsg3
	sta print_text+0
	sty print_text+1
	ldx #$13 ; optimized, look out for bugs
	jsr printstring
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #10
ShowTitle_rightvarAddSub_var204 = $88
	sta ShowTitle_rightvarAddSub_var204
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var204
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$8
	sta screen_y
	lda #>$1e00
	jsr SetScreenPosition
	clc
	lda #<authormsg4
	adc #$0
	ldy #>authormsg4
	sta print_text+0
	sty print_text+1
	ldx #$14 ; optimized, look out for bugs
	jsr printstring
	
; // Display Controls
	lda #$2
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$c
	sta screen_y
	lda #>$1e00
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
	lda #$e
	sta screen_y
	lda #>$1e00
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
ShowTitle_rightvarAddSub_var211 = $88
	sta ShowTitle_rightvarAddSub_var211
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var211
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$14
	sta screen_y
	lda #>$1e00
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
	
; // Setup VIC screen
; // @TODO: Need to check for PAL/NTSC and position screen accordingly
; //	asm("
; //		sei
; //		lda $9002
; //		and #128
; //		ora myscreenwidth
; //		sta $9002	
; // Set # of columns
; //		lda myscreenheight
; //		rol
; //		and #126
; //		sta $9003	
; // Set # of rows
; //		lda #10
; //		sta $9000	
; // Set horizontal screen position
; //		lda #40
; //		sta $9001	
; // Set vertical screen position
; //		cli
; //	");
; //	
; //for i := 0 to myscreenwidth do
; //begin
; //	PokeAddr(i, 0, i);		
; //end;
; //for i := 0 to myscreenheight do
; //begin
; //	PokeAddr(0, i, i);		
; //end;
; //WaitForKeypress();
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
	bne Plot_elsedoneblock218
Plot_ConditionalTrueBlock216: ;Main true block ;keep 
	
; // line 900
	rts
Plot_elsedoneblock218
	
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
	bne Plot_elseblock223
Plot_ConditionalTrueBlock222: ;Main true block ;keep 
	
; // line 920
; // Select pattern style
	lda colSelect
	; Calling storevariable on generic assign expression
	sta plotIndexMax
	jmp Plot_elsedoneblock224
Plot_elseblock223
	lda mainIndex
	; Calling storevariable on generic assign expression
	sta plotIndexMax
Plot_elsedoneblock224
	lda #$1
	; Calling storevariable on generic assign expression
	sta plotIndex
Plot_forloop229
	; Binary clause Simplified: EQUALS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bne Plot_localfailed374
	jmp Plot_ConditionalTrueBlock328
Plot_localfailed374
	jmp Plot_elseblock329
Plot_ConditionalTrueBlock328: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock330
Plot_elseblock329
	; Binary clause Simplified: EQUALS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bne Plot_localfailed399
	jmp Plot_ConditionalTrueBlock378
Plot_localfailed399
	jmp Plot_elseblock379
Plot_ConditionalTrueBlock378: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock380
Plot_elseblock379
	lda y1
	; Calling storevariable on generic assign expression
	sta y2
	; Binary clause Simplified: LESS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bpl Plot_elseblock404
Plot_ConditionalTrueBlock403: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock405
Plot_elseblock404
	
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
Plot_elsedoneblock405
	lda x1
	; Calling storevariable on generic assign expression
	sta x2
	; Binary clause Simplified: LESS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bpl Plot_elseblock412
Plot_ConditionalTrueBlock411: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock413
Plot_elseblock412
	
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
Plot_elsedoneblock413
Plot_elsedoneblock380
Plot_elsedoneblock330
Plot_forloopcounter231
Plot_loopstart232
	; Test Inc dec D
	inc plotIndex
	lda plotIndexMax
	cmp plotIndex ;keep
	beq Plot_loopdone418
Plot_loopnotdone419
	jmp Plot_forloop229
Plot_loopdone418
Plot_forloopend230
Plot_loopend233
	rts
	
; // for
; // Update the display
	; NodeProcedureDecl -1
	; ***********  Defining procedure : MainLoop
	;    Procedure type : User-defined procedure
MainLoop
	lda start_pos
	; Calling storevariable on generic assign expression
	sta mainIndex
MainLoop_forloop421
	
; // line 200
; // Update color
	; Load pointer array
	ldy colorIdx
	lda (cArrPtr),y
	; Calling storevariable on generic assign expression
	sta curr_color
	; Test Inc dec D
	inc colorIdx
	; Binary clause Simplified: GREATEREQUAL
	lda colorIdx
	; Compare with pure num / var optimization
	cmp cArrLen;keep
	bcc MainLoop_elsedoneblock437
MainLoop_ConditionalTrueBlock435: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta colorIdx
MainLoop_elsedoneblock437
	
; // Map the data
; // Right
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
; // Left
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
; // Down
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
; // Up
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
	
; // line 280
; // Check user input	
	jsr CheckInputs
MainLoop_forloopcounter423
MainLoop_loopstart424
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
	beq MainLoop_loopdone440
MainLoop_loopnotdone441
	jmp MainLoop_forloop421
MainLoop_loopdone440
MainLoop_forloopend422
MainLoop_loopend425
	rts
	
; // Various Pattern Mutations
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DoMutations
	;    Procedure type : User-defined procedure
DoMutations
	; Test Inc dec D
	inc cycleCtr
	jsr Random
	; Calling storevariable on generic assign expression
	sta rando
	
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
	; Binary clause Simplified: EQUALS
	lda dir
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne DoMutations_elsedoneblock446
DoMutations_ConditionalTrueBlock444: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda pattern_type
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne DoMutations_elseblock473
DoMutations_ConditionalTrueBlock472: ;Main true block ;keep 
	
; // Mutations every full cycle
; // Toggle pattern type
; // 4-point pattern
	lda #$0
	; Calling storevariable on generic assign expression
	sta pattern_type
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenheight
	lsr
	; Calling storevariable on generic assign expression
	sta num_chars
	jmp DoMutations_elsedoneblock474
DoMutations_elseblock473
	
; // 8-point pattern
	lda #$1
	; Calling storevariable on generic assign expression
	sta pattern_type
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenheight
	lsr
	lsr
	; Calling storevariable on generic assign expression
	sta num_chars
DoMutations_elsedoneblock474
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$40;keep
	bcs DoMutations_elsedoneblock482
DoMutations_ConditionalTrueBlock480: ;Main true block ;keep 
	
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
DoMutations_rightvarAddSub_var491 = $88
	sta DoMutations_rightvarAddSub_var491
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and temp
	 ; end add / sub var with constant
	clc
	adc DoMutations_rightvarAddSub_var491
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
	adc #$6
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta centerY
	; Clear screen with offset
	lda #$20
	ldx #$fe
DoMutations_clearloop492
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne DoMutations_clearloop492
DoMutations_elsedoneblock482
DoMutations_elsedoneblock446
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcs DoMutations_localfailed562
	jmp DoMutations_ConditionalTrueBlock494
DoMutations_localfailed562
	jmp DoMutations_elsedoneblock496
DoMutations_ConditionalTrueBlock494: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$4;keep
	bcs DoMutations_elseblock566
DoMutations_ConditionalTrueBlock565: ;Main true block ;keep 
	
; // One in 16 chance of mutation
; // Select new char range
	lda #$4c
	; Calling storevariable on generic assign expression
	sta char_start_st
	
; // Balls and lines pattern
	lda #$5
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jsr InitData
	jmp DoMutations_elsedoneblock567
DoMutations_elseblock566
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$8;keep
	bcs DoMutations_elseblock598
DoMutations_ConditionalTrueBlock597: ;Main true block ;keep 
	lda #$5f
	; Calling storevariable on generic assign expression
	sta char_start_st
	
; // Triangles
	lda #$b
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jsr InitData
	jmp DoMutations_elsedoneblock599
DoMutations_elseblock598
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$c;keep
	bcs DoMutations_elseblock614
DoMutations_ConditionalTrueBlock613: ;Main true block ;keep 
	lda #$74
	; Calling storevariable on generic assign expression
	sta char_start_st
	
; // Angles
	lda #$b
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jsr InitData
	jmp DoMutations_elsedoneblock615
DoMutations_elseblock614
	lda #$40
	; Calling storevariable on generic assign expression
	sta char_start_st
	
; // Graphics Chars
	lda #$3f
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jsr InitData
DoMutations_elsedoneblock615
DoMutations_elsedoneblock599
DoMutations_elsedoneblock567
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne DoMutations_elseblock622
DoMutations_ConditionalTrueBlock621: ;Main true block ;keep 
	
; // Toggle reverse chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta rev_enable
	jmp DoMutations_elsedoneblock623
DoMutations_elseblock622
	lda #$1
	; Calling storevariable on generic assign expression
	sta rev_enable
DoMutations_elsedoneblock623
	
; // Adjust pattern size		
	lda #$1
	; Calling storevariable on generic assign expression
	sta dir
	
; // Set pattern moving outward
	; Calling storevariable on generic assign expression
	sta start_pos
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and #$6
	 ; end add / sub var with constant
	clc
	adc #$6
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta stop_pos
DoMutations_elsedoneblock496
	
; //clearscreen($20, SCREEN_CHAR_LOC);
	; Binary clause Simplified: GREATEREQUAL
	lda cycleCtr
	; Compare with pure num / var optimization
	cmp #$4;keep
	bcc DoMutations_localfailed753
	jmp DoMutations_ConditionalTrueBlock629
DoMutations_localfailed753
	jmp DoMutations_elsedoneblock631
DoMutations_ConditionalTrueBlock629: ;Main true block ;keep 
	
; // Update color palette	
	jsr Random
	; Calling storevariable on generic assign expression
	sta rando
	; Binary clause Simplified: LESS
	; Compare with pure num / var optimization
	cmp #$33;keep
	bcs DoMutations_elseblock757
DoMutations_ConditionalTrueBlock756: ;Main true block ;keep 
	lda #<cArr1
	ldx #>cArr1
	sta cArrPtr
	stx cArrPtr+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock758
DoMutations_elseblock757
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$66;keep
	bcs DoMutations_elseblock821
DoMutations_ConditionalTrueBlock820: ;Main true block ;keep 
	lda #<cArr2
	ldx #>cArr2
	sta cArrPtr
	stx cArrPtr+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock822
DoMutations_elseblock821
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$99;keep
	bcs DoMutations_elseblock853
DoMutations_ConditionalTrueBlock852: ;Main true block ;keep 
	lda #<cArr3
	ldx #>cArr3
	sta cArrPtr
	stx cArrPtr+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock854
DoMutations_elseblock853
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$cc;keep
	bcs DoMutations_elseblock869
DoMutations_ConditionalTrueBlock868: ;Main true block ;keep 
	lda #<cArr4
	ldx #>cArr4
	sta cArrPtr
	stx cArrPtr+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock870
DoMutations_elseblock869
	lda #<cArr5
	ldx #>cArr5
	sta cArrPtr
	stx cArrPtr+1
	lda #$d
	; Calling storevariable on generic assign expression
	sta cArrLen
DoMutations_elsedoneblock870
DoMutations_elsedoneblock854
DoMutations_elsedoneblock822
DoMutations_elsedoneblock758
	lda #$0
	; Calling storevariable on generic assign expression
	sta cycleCtr
DoMutations_elsedoneblock631
	rts
block1
	
; // End DoMutations
; // Show the title and check number of columns
	jsr ShowTitle
	
; // Init char array	
	jsr InitData
	
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fe
MainProgram_clearloop875
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne MainProgram_clearloop875
MainProgram_while876
MainProgram_loopstart880
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock879
MainProgram_ConditionalTrueBlock877: ;Main true block ;keep 
	jsr MainLoop
	
; // Do Pattern Mutations
	jsr DoMutations
	
; // Get fresh array of chars
	jsr InitData
	jmp MainProgram_while876
MainProgram_elsedoneblock879
MainProgram_loopend881
	; End of program
	; Ending memory block at $1010
EndBlock1010:
