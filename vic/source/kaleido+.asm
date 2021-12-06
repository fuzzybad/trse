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
centerX	dc.b	$0b
centerY	dc.b	$0b
start_pos	dc.b	$01
stop_pos	dc.b	$0c
dir	dc.b	$01
char_start	dc.b	0
char_start_st	dc.b	$41
char_offset	dc.b	$3f
num_chars	dc.b	$06
speed	dc.b	$03
rev_enable	dc.b	$00
pattern_type	dc.b	$01
full_screen	dc.b	$01
curr_color	dc.b	$01
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
	
; // Used for keyboard input
; // Text for splash screen  	
; // PRESS ANY KEY
; // VIC Colors
; // 0 = black
; // 1 = white
; // 2 = red
; // 3 = cyan
; // 4 = purple
; // 5 = green
; // 6 = blue
; // 7 = yellow
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
	
; //	Method which shows title screen and checks screen width
	; NodeProcedureDecl -1
	; ***********  Defining procedure : ShowTitle
	;    Procedure type : User-defined procedure
ShowTitle
	
; // Set uppercase
	; Poke
	; Optimization: shift is zero
	lda #$c
	sta $e84c
	
; // Set black background/border
	; Poke
	; Optimization: shift is zero
	lda #$8
	sta $900f
	
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fe
ShowTitle_clearloop39
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne ShowTitle_clearloop39
	
; // Set default colormemory
	; Clear screen with offset
	lda #$3
	ldx #$fe
ShowTitle_clearloop40
	dex
	sta $0000+$9600,x
	sta $00fd+$9600,x
	bne ShowTitle_clearloop40
	
; // Center the title text
	lda #$5
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
	lda #$3
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
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
	lda #$4
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
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
	lda #$2
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
	lda #$1
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
	lda #$5
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
	
; // Fill char array
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitData
	;    Procedure type : User-defined procedure
InitData
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
InitData_forloop58
	
; // line 150
; // Populate char_arr 
	lda char_start_st
	; Calling storevariable on generic assign expression
	sta char_start
	
; // line 165
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne InitData_elsedoneblock86
InitData_ConditionalTrueBlock84: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	jsr Random
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc InitData_elsedoneblock98
InitData_ConditionalTrueBlock96: ;Main true block ;keep 
	
; // Option for reverse chars
; // line 165
	; Optimizer: a = a +/- b
	lda char_start
	clc
	adc #$80
	sta char_start
InitData_elsedoneblock98
InitData_elsedoneblock86
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
InitData_forloopcounter60
InitData_loopstart61
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda num_chars
	cmp i ;keep
	bne InitData_forloop58
InitData_loopdone101: ;keep
InitData_forloopend59
InitData_loopend62
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
	bne CheckInputs_elsedoneblock106
CheckInputs_ConditionalTrueBlock104: ;Main true block ;keep 
	
; // Q - Quit
	; Clear screen with offset
	lda #$20
	ldx #$fe
CheckInputs_clearloop110
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne CheckInputs_clearloop110
	jsr $fd49
CheckInputs_elsedoneblock106
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$30;keep
	bne CheckInputs_elsedoneblock114
CheckInputs_ConditionalTrueBlock112: ;Main true block ;keep 
	
; // 0-9 Adjust speed 
	lda #$9
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock114
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$31;keep
	bne CheckInputs_elsedoneblock120
CheckInputs_ConditionalTrueBlock118: ;Main true block ;keep 
	
; //(slowest)
	lda #$8
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock120
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$32;keep
	bne CheckInputs_elsedoneblock126
CheckInputs_ConditionalTrueBlock124: ;Main true block ;keep 
	lda #$7
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock126
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$33;keep
	bne CheckInputs_elsedoneblock132
CheckInputs_ConditionalTrueBlock130: ;Main true block ;keep 
	lda #$6
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock132
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne CheckInputs_elsedoneblock138
CheckInputs_ConditionalTrueBlock136: ;Main true block ;keep 
	lda #$5
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock138
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$35;keep
	bne CheckInputs_elsedoneblock144
CheckInputs_ConditionalTrueBlock142: ;Main true block ;keep 
	lda #$4
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock144
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$36;keep
	bne CheckInputs_elsedoneblock150
CheckInputs_ConditionalTrueBlock148: ;Main true block ;keep 
	lda #$3
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock150
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$37;keep
	bne CheckInputs_elsedoneblock156
CheckInputs_ConditionalTrueBlock154: ;Main true block ;keep 
	lda #$2
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock156
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne CheckInputs_elsedoneblock162
CheckInputs_ConditionalTrueBlock160: ;Main true block ;keep 
	lda #$1
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock162
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$39;keep
	bne CheckInputs_elsedoneblock168
CheckInputs_ConditionalTrueBlock166: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock168
	rts
	
; //(fastest)
; // Poke value into an address
	; NodeProcedureDecl -1
	; ***********  Defining procedure : PokeAddr
	;    Procedure type : User-defined procedure
pokeaddr_x	dc.b	0
pokeaddr_y	dc.b	0
pokeaddr_v	dc.b	0
PokeAddr_block171
PokeAddr
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda pokeaddr_x
	; Compare with pure num / var optimization
	cmp #$16;keep
	bcc PokeAddr_elsedoneblock175
PokeAddr_ConditionalTrueBlock173: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock175
	; Binary clause Simplified: LESS
	lda pokeaddr_x
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock181
PokeAddr_ConditionalTrueBlock179: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock181
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp #$17;keep
	bcc PokeAddr_elsedoneblock187
PokeAddr_ConditionalTrueBlock185: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock187
	; Binary clause Simplified: LESS
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock193
PokeAddr_ConditionalTrueBlock191: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock193
	
; // Pointer value to screen RAM
	; Generic 16 bit op
	ldy #0
	lda pokeaddr_x
PokeAddr_rightvarInteger_var198 = $88
	sta PokeAddr_rightvarInteger_var198
	sty PokeAddr_rightvarInteger_var198+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$1e
	lda #$00
PokeAddr_rightvarInteger_var201 = $8A
	sta PokeAddr_rightvarInteger_var201
	sty PokeAddr_rightvarInteger_var201+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0
	lda pokeaddr_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$16
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var201
PokeAddr_wordAdd199
	sta PokeAddr_rightvarInteger_var201
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var201+1
	tay
	lda PokeAddr_rightvarInteger_var201
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var198
PokeAddr_wordAdd196
	sta PokeAddr_rightvarInteger_var198
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var198+1
	tay
	lda PokeAddr_rightvarInteger_var198
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
PokeAddr_rightvarInteger_var204 = $88
	sta PokeAddr_rightvarInteger_var204
	sty PokeAddr_rightvarInteger_var204+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$96
	lda #$00
PokeAddr_rightvarInteger_var207 = $8A
	sta PokeAddr_rightvarInteger_var207
	sty PokeAddr_rightvarInteger_var207+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0
	lda pokeaddr_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$16
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var207
PokeAddr_wordAdd205
	sta PokeAddr_rightvarInteger_var207
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var207+1
	tay
	lda PokeAddr_rightvarInteger_var207
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var204
PokeAddr_wordAdd202
	sta PokeAddr_rightvarInteger_var204
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var204+1
	tay
	lda PokeAddr_rightvarInteger_var204
	sta p
	sty p+1
	; Poke
	lda curr_color
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
	bne Plot_elsedoneblock212
Plot_ConditionalTrueBlock210: ;Main true block ;keep 
	
; // line 900
	rts
Plot_elsedoneblock212
	
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
	bne Plot_elseblock217
Plot_ConditionalTrueBlock216: ;Main true block ;keep 
	
; // line 920
; // Select pattern style
	lda colSelect
	; Calling storevariable on generic assign expression
	sta plotIndexMax
	jmp Plot_elsedoneblock218
Plot_elseblock217
	lda mainIndex
	; Calling storevariable on generic assign expression
	sta plotIndexMax
Plot_elsedoneblock218
	lda #$1
	; Calling storevariable on generic assign expression
	sta plotIndex
Plot_forloop223
	; Binary clause Simplified: EQUALS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bne Plot_localfailed368
	jmp Plot_ConditionalTrueBlock322
Plot_localfailed368
	jmp Plot_elseblock323
Plot_ConditionalTrueBlock322: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock324
Plot_elseblock323
	; Binary clause Simplified: EQUALS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bne Plot_localfailed393
	jmp Plot_ConditionalTrueBlock372
Plot_localfailed393
	jmp Plot_elseblock373
Plot_ConditionalTrueBlock372: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock374
Plot_elseblock373
	lda y1
	; Calling storevariable on generic assign expression
	sta y2
	; Binary clause Simplified: LESS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bpl Plot_elseblock398
Plot_ConditionalTrueBlock397: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock399
Plot_elseblock398
	
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
Plot_elsedoneblock399
	lda x1
	; Calling storevariable on generic assign expression
	sta x2
	; Binary clause Simplified: LESS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bpl Plot_elseblock406
Plot_ConditionalTrueBlock405: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock407
Plot_elseblock406
	
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
Plot_elsedoneblock407
Plot_elsedoneblock374
Plot_elsedoneblock324
Plot_forloopcounter225
Plot_loopstart226
	; Test Inc dec D
	inc plotIndex
	lda plotIndexMax
	cmp plotIndex ;keep
	beq Plot_loopdone412
Plot_loopnotdone413
	jmp Plot_forloop223
Plot_loopdone412
Plot_forloopend224
Plot_loopend227
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
MainLoop_forloop415
	
; // line 200
; // Update color
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and #$7
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta curr_color
MainLoop_while430
MainLoop_loopstart434
	; Binary clause Simplified: EQUALS
	lda curr_color
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainLoop_elsedoneblock433
MainLoop_ConditionalTrueBlock431: ;Main true block ;keep 
	
; // If black, pick another color..
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and #$7
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta curr_color
	jmp MainLoop_while430
MainLoop_elsedoneblock433
MainLoop_loopend435
	
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
MainLoop_forloopcounter417
MainLoop_loopstart418
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
	beq MainLoop_loopdone438
MainLoop_loopnotdone439
	jmp MainLoop_forloop415
MainLoop_loopdone438
MainLoop_forloopend416
MainLoop_loopend419
	
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
	sta temp
	; Binary clause Simplified: EQUALS
	lda dir
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainLoop_elsedoneblock443
MainLoop_ConditionalTrueBlock441: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda pattern_type
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainLoop_elseblock466
MainLoop_ConditionalTrueBlock465: ;Main true block ;keep 
	
; // Mutations every full cycle
; // Toggle pattern type
; // 4-point pattern
	lda #$0
	; Calling storevariable on generic assign expression
	sta pattern_type
	lda #$b
	; Calling storevariable on generic assign expression
	sta num_chars
	jmp MainLoop_elsedoneblock467
MainLoop_elseblock466
	
; // 8-point pattern
	lda #$1
	; Calling storevariable on generic assign expression
	sta pattern_type
	lda #$5
	; Calling storevariable on generic assign expression
	sta num_chars
MainLoop_elsedoneblock467
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$20;keep
	bcs MainLoop_elsedoneblock475
MainLoop_ConditionalTrueBlock473: ;Main true block ;keep 
	
; // Randomly change center point
	lda #$4
	; Calling storevariable on generic assign expression
	sta temp
	; 8 bit binop
	; Add/sub right value is variable/expression
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #$16
	sec
	sbc temp
	 ; end add / sub var with constant
	lsr
MainLoop_rightvarAddSub_var480 = $88
	sta MainLoop_rightvarAddSub_var480
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and temp
	 ; end add / sub var with constant
	clc
	adc MainLoop_rightvarAddSub_var480
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
	ldx #$fe
MainLoop_clearloop481
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne MainLoop_clearloop481
MainLoop_elsedoneblock475
MainLoop_elsedoneblock443
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$20;keep
	bcs MainLoop_elsedoneblock485
MainLoop_ConditionalTrueBlock483: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$8;keep
	bcs MainLoop_elseblock546
MainLoop_ConditionalTrueBlock545: ;Main true block ;keep 
	
; // Select char range
	lda #$4c
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$5
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
	jmp MainLoop_elsedoneblock547
MainLoop_elseblock546
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcs MainLoop_elseblock578
MainLoop_ConditionalTrueBlock577: ;Main true block ;keep 
	lda #$4c
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$1e
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
	jmp MainLoop_elsedoneblock579
MainLoop_elseblock578
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$18;keep
	bcs MainLoop_elseblock594
MainLoop_ConditionalTrueBlock593: ;Main true block ;keep 
	lda #$74
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$c
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
	jmp MainLoop_elsedoneblock595
MainLoop_elseblock594
	lda #$41
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$3f
	; Calling storevariable on generic assign expression
	sta char_offset
	jsr InitData
MainLoop_elsedoneblock595
MainLoop_elsedoneblock579
MainLoop_elsedoneblock547
MainLoop_elsedoneblock485
	; Binary clause Simplified: LESS
	jsr Random
	; Compare with pure num / var optimization
	cmp #$40;keep
	bcs MainLoop_elsedoneblock603
MainLoop_ConditionalTrueBlock601: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainLoop_elseblock616
MainLoop_ConditionalTrueBlock615: ;Main true block ;keep 
	
; // Toggle reverse chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta rev_enable
	jmp MainLoop_elsedoneblock617
MainLoop_elseblock616
	lda #$1
	; Calling storevariable on generic assign expression
	sta rev_enable
MainLoop_elsedoneblock617
MainLoop_elsedoneblock603
	; Binary clause Simplified: LESS
	jsr Random
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcs MainLoop_elsedoneblock625
MainLoop_ConditionalTrueBlock623: ;Main true block ;keep 
	
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
	bne MainLoop_elseblock639
MainLoop_ConditionalTrueBlock638: ;Main true block ;keep 
	
; // Toggle the flag
	lda #$1
	; Calling storevariable on generic assign expression
	sta full_screen
	lda #$b
	; Calling storevariable on generic assign expression
	sta stop_pos
	jmp MainLoop_elsedoneblock640
MainLoop_elseblock639
	lda #$0
	; Calling storevariable on generic assign expression
	sta full_screen
	lda #$f
	; Calling storevariable on generic assign expression
	sta stop_pos
MainLoop_elsedoneblock640
	; Clear screen with offset
	lda #$20
	ldx #$fe
MainLoop_clearloop645
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne MainLoop_clearloop645
MainLoop_elsedoneblock625
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
	ldx #$fe
MainProgram_clearloop646
	dex
	sta $0000+$1e00,x
	sta $00fd+$1e00,x
	bne MainProgram_clearloop646
MainProgram_while647
MainProgram_loopstart651
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock650
MainProgram_ConditionalTrueBlock648: ;Main true block ;keep 
	jsr MainLoop
	
; // Check for user input	
	jsr CheckInputs
	
; // Get fresh array of chars
	jsr InitData
	jmp MainProgram_while647
MainProgram_elsedoneblock650
MainProgram_loopend652
	; End of program
	; Ending memory block at $1010
EndBlock1010: