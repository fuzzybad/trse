 processor 6502
	org $1c01
StartBlock1c01:
	; Starting new memory block at $1c01
	.byte $b ; lo byte of next line
	.byte $1c ; hi byte of next line
	.byte $0a, $00 ; line 10 (lo, hi)
	.byte $9e, $20 ; SYS token and a space
	.byte   $38,$32,$32,$34
	.byte $00, $00, $00 ; end of program
	; Ending memory block at $1c01
EndBlock1c01:
	org $2020
StartBlock2020:
	; Starting new memory block at $2020
Kaleido
	jmp block1
myscreenheight	dc.b	$18
myscreenwidth	dc.b	$50
centerX	dc.b	$0c
centerY	dc.b	$14
start_pos	dc.b	$01
stop_pos	dc.b	$14
dir	dc.b	$01
char_start	dc.b	0
char_offset	dc.b	0
char_start_st	dc.b	$40
char_offset_st	dc.b	$3f
num_chars	dc.b	$06
speed	dc.b	$00
rev_enable	dc.b	$00
pattern_type	dc.b	$01
curr_color	dc.b	$01
char_arr	dc.b	 
	org char_arr+20
p	= $02
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
cArr1	dc.b $01, $0f, $0c, $0b, $0b, $0c, $0f, $01
cArr2	dc.b $05, $0d, $01, $0d, $05
cArr3	dc.b $01, $03, $0e, $06, $0e, $03, $01
cArr4	dc.b $07, $08, $02, $0a, $02, $08, $07
cArr5	dc.b $02, $08, $07, $05, $06, $04, $06, $05
	dc.b $07, $08, $02
colorIdx	dc.b	$00
cArrPtr	= $04
cArrLen	dc.b	$00
cycleCtr	dc.b	$00
key	dc.b	0
titlemsg	
	dc.b	$0b, $01, $0c, $05, $09, $04, $0f, $13, $03
	dc.b	$0f, $10, $05, $2b, 0
authormsg1	
	dc.b	$0a, $05, $13, $13, $09, $03, $01, $20, $10
	dc.b	$05, $14, $05, $12, $13, $05, $0e, $20, $3c
	dc.b	$06, $15, $1a, $1a, $19, $02, $01, $04, $3e
	dc.b	$2c, $20, $32, $30, $32, $31, 0
authormsg2	
	dc.b	$02, $01, $13, $05, $04, $20, $0f, $0e, $20
	dc.b	$12, $0f, $15, $14, $09, $0e, $05, $20, $02
	dc.b	$19, 0
authormsg3	
	dc.b	$12, $15, $07, $07, $20, $01, $0e, $04, $20
	dc.b	$06, $05, $0c, $04, $0d, $01, $0e, $2c, $20
	dc.b	$31, $39, $37, $39, 0
inst1	
	dc.b	$30, $2d, $39, $20, $2d, $20, $01, $04, $0a
	dc.b	$15, $13, $14, $20, $13, $10, $05, $05, $04
	dc.b	0 
inst2	
	dc.b	$11, $20, $20, $20, $2d, $20, $11, $15, $09
	dc.b	$14, 0
inst3	
	dc.b	$13, $15, $10, $10, $0f, $12, $14, $13, $20
	dc.b	$34, $30, $20, $01, $0e, $04, $20, $38, $30
	dc.b	$20, $03, $0f, $0c, 0
promptmsg	
	dc.b	$10, $12, $05, $13, $13, $20, $01, $0e, $19
	dc.b	$20, $0b, $05, $19, 0
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $4c     ;$59 used for hi-byte
initdiv16x8_dividend = $4e	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4e ;save memory by reusing divident to store the result
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
mul16x8_enterLoop
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
	; NodeProcedureDecl -1
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initgetkey
	;    Procedure type : Built-in function
	;    Requires initialization : no
;jmp c64_getKey
key_columntab
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $60,
 dc.b $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60,
 dc.b $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $50,
 dc.b $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $40,
 dc.b $40, $40, $40, $40, $40, $40, $40, $30, $30, $30, $30, $20, $20, $10, $00, $00,
key_chartab	dc.b $00, $051, $00, $020, $032, $00, $02a, $031
        dc.b $06f, $051, $07d, $00, $072, $03b, $02a, $01c
        dc.b $06c, $040, $07a, $06e, $02d, $04c, $050, $02b
        dc.b $04e, $04f, $04b, $04d, $030, $04a, $049, $039
        dc.b $056, $055, $048, $042, $038, $047, $059, $037
        dc.b $058, $054, $046, $043, $036, $044, $052, $035
        dc.b $00, $045, $053, $05a, $034, $041, $057, $033
        dc.b $00, $051, $00, $00, $00, $00, $08e, $0f7
        dc.b
key_row dc.b 0
key_col dc.b 0
c64_getKey:
    lda #$0
    sta $dc03	; port b ddr (input)
    lda #$ff
    sta $dc02	; port a ddr (output)
    lda #$00
    sta $dc00	; port a
    lda $dc01       ; port b
    cmp #$ff
    beq key_nokey
; got column
    tay
    lda #$7f
    sta key_nokey2+1
    ldx #8
key_nokey2:
    lda #0
    sta $dc00	; port a
    sec
    ror key_nokey2+1
    dex
    bmi key_nokey
    lda $dc01       ; port b
    cmp #$ff
    beq key_nokey2
    ; got row in X
    txa
    asl
    asl
    asl
    tax
    stx key_row
    txa
    lda key_columntab,y
    ror
    ror
    ror
    ror
    sta key_col
    ora key_row
    sta key_row
    lda #64
    sbc key_row
    ;	sec
    tax
    lda key_chartab,x
    jmp key_cont
key_nokey:
    lda #$FF
key_cont:
    rts
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto3
screenmemory =  $fe
colormemory =  $fb
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
	adc #40
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
	; ***********  Defining procedure : initmoveto80
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto80_moveto804
screenmemory80 =  $fe
screen_x_80 .byte 0 
screen_y_80 .byte 0 
SetScreenPosition80
	sta screenmemory80+1
	lda #0
	sta screenmemory80
	ldy screen_y_80
	cpy #0
	beq sydone80
syloop80
	clc
	adc #80
	bcc sskip80
	inc screenmemory80+1
sskip80
	dey
	cpy #$00
	bne syloop80
sydone80
	ldx screen_x_80
	cpx #0
	beq sxdone80
	clc
	adc screen_x_80
	bcc sxdone80
	inc screenmemory80+1
sxdone80
	sta screenmemory80
initmoveto80_moveto804
	lda #18
	sta $D600
initmoveto80_a_moveto805
	bit $D600
	bpl initmoveto80_a_moveto805
	lda screenmemory80+1
	sta $D601
	lda #19
	sta $D600
initmoveto80_b_moveto806
	bit $D600
	bpl initmoveto80_b_moveto806
	lda screenmemory80
	sta $D601
	rts
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
	;    Procedure type : Built-in function
	;    Requires initialization : no
print_text = $4c
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
	bcc initrandom256_RandomSkip7
	eor #$4d
initrandom256_RandomSkip7
	eor $dc04
	sta Random+1
	rts
	
; // Color arrays
; // Grays
; // Greens
; // Cool colors
; // Warm colors
; // Rainbow
; //cArr5			: array[] of byte =(14, 10, 1, 10, 14); 
; // Trans rights	
; //	0	$00	black
; //	1	$01	white
; //	2	$02	red
; //	3	$03	cyan
; //	4	$04	purple
; //	5	$05	green
; //	6	$06	blue
; //	7	$07	yellow
; //	8	$08	orange
; //	9	$09	brown
; //	10	$0A	pink
; //	11	$0B	dark grey
; //	12	$0C	grey
; //	13	$0D	light green
; //	14	$0E	light blue
; //	15	$0F	light grey
; //
; // Used for keyboard input
; // Text for splash screen  	
; // PRESS ANY KEY
; //promptmsg: cstring =(208, 210, 197, 211, 211, 32, 193, 206, 217, 32, 203, 197, 217);
; // NOTES:
; //	
; // $D600 is VDC Address Register
; // $D601 is VDC Data Register
; // VDC Character Display area(screen) $0000-$07FF
; // VDC Character Attributes(color) $0800-$0FFF
; // VDC Character Definitions $2000-$3FFF
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
WaitForKeypress_while10
WaitForKeypress_loopstart14
	; Binary clause Simplified: EQUALS
	lda temp
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne WaitForKeypress_elsedoneblock13
WaitForKeypress_ConditionalTrueBlock11: ;Main true block ;keep 
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta temp
	jmp WaitForKeypress_while10
WaitForKeypress_elsedoneblock13
WaitForKeypress_loopend15
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
	bcc DoDelay_elsedoneblock22
DoDelay_ConditionalTrueBlock20: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
DoDelay_forloop33
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
DoDelay_forloopcounter35
DoDelay_loopstart36
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda speed
	cmp i ;keep
	bne DoDelay_forloop33
DoDelay_loopdone40: ;keep
DoDelay_forloopend34
DoDelay_loopend37
DoDelay_elsedoneblock22
	rts
	
; // Clear 80-col screen
; //	Clear80(32, $00); 
; // fill screen with spaces
; //	Clear80(8, $08); 
; // fill screen color
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Clear80
	;    Procedure type : User-defined procedure
myChar	dc.b	0
myLoc	dc.w	0
Clear80_block41
Clear80
	
; // space char
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
Clear80_forloop42
	lda #$0
	sta screen_x_80
	lda i
	sta screen_y_80
	; integer assignment NodeVar
	ldy myLoc+1 ; keep
	lda myLoc
	jsr SetScreenPosition80
	; Fill80
	ldy myChar ; optimized, look out for bugs
	ldx #$50 ; optimized, look out for bugs
	lda #31
	sta $D600
Clear80_fill80_text51
	bit $D600
	bpl Clear80_fill80_text51
	sty $D601
	dex
Clear80_fill80_loop52
	sty $D601
	dex
	bne Clear80_fill80_loop52
Clear80_forloopcounter44
Clear80_loopstart45
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$19
	cmp i ;keep
	bne Clear80_forloop42
Clear80_loopdone53: ;keep
Clear80_forloopend43
Clear80_loopend46
	rts
	
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
	lda #$8
	; Calling storevariable on generic assign expression
	sta cArrLen
	
; // Set uppercase
	; Poke
	; Optimization: shift is zero
	lda #$c
	sta $e84c
	
; // Set 80-col screen
	; Go 80 columns
	lda $d7
	jsr $FF5F
	lda #$20
	; Calling storevariable on generic assign expression
	sta myChar
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta myLoc
	sty myLoc+1
	jsr Clear80
	
; // Init VDC screen mem
	lda #$3
	; Calling storevariable on generic assign expression
	sta myChar
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$8
	; Calling storevariable on generic assign expression
	sta myLoc
	sty myLoc+1
	jsr Clear80
	
; // Init VDC color mem 
; // Set background/border colors
	; Poke
	; Optimization: shift is zero
	lda #$0
	sta $d020
	; Poke
	; Optimization: shift is zero
	sta $d021
	
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
ShowTitle_clearloop55
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne ShowTitle_clearloop55
	
; // Set cyan color
	; Clear screen with offset
	lda #$3
	ldx #$fa
ShowTitle_clearloop56
	dex
	sta $0000+$d800,x
	sta $00fa+$d800,x
	sta $01f4+$d800,x
	sta $02ee+$d800,x
	bne ShowTitle_clearloop56
	lda #$28
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	
; // Center the title text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
ShowTitle_rightvarAddSub_var57 = $54
	sta ShowTitle_rightvarAddSub_var57
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var57
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$1
	sta screen_y
	lda #>$400
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
ShowTitle_rightvarAddSub_var60 = $54
	sta ShowTitle_rightvarAddSub_var60
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var60
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$3
	sta screen_y
	lda #>$400
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
ShowTitle_rightvarAddSub_var63 = $54
	sta ShowTitle_rightvarAddSub_var63
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var63
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$6
	sta screen_y
	lda #>$400
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
ShowTitle_rightvarAddSub_var66 = $54
	sta ShowTitle_rightvarAddSub_var66
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var66
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$8
	sta screen_y
	lda #>$400
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
	lda #$b
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$c
	sta screen_y
	lda #>$400
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
	lda #>$400
	jsr SetScreenPosition
	clc
	lda #<inst2
	adc #$0
	ldy #>inst2
	sta print_text+0
	sty print_text+1
	ldx #$a ; optimized, look out for bugs
	jsr printstring
	
; // Show 40/80 message 
	lda #$9
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$12
	sta screen_y
	lda #>$400
	jsr SetScreenPosition
	clc
	lda #<inst3
	adc #$0
	ldy #>inst3
	sta print_text+0
	sty print_text+1
	ldx #$16 ; optimized, look out for bugs
	jsr printstring
	
; // Center prompt to continue
; // TRSE seems bugged at counting length of numeric strings
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
ShowTitle_rightvarAddSub_var75 = $54
	sta ShowTitle_rightvarAddSub_var75
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var75
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$14
	sta screen_y
	lda #>$400
	jsr SetScreenPosition
	clc
	lda #<promptmsg
	adc #$0
	ldy #>promptmsg
	sta print_text+0
	sty print_text+1
	ldx #$d ; optimized, look out for bugs
	jsr printstring
	
; // VDC text matrix is at $0000
; // VDC color matrix is at $0800
	lda #$50
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	
; // Center the title text on VDC screen 
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
ShowTitle_rightvarAddSub_var78 = $54
	sta ShowTitle_rightvarAddSub_var78
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var78
	; Calling storevariable on generic assign expression
	sta x1
	
; // Display text	
	sta screen_x_80
	lda #$1
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text79
	bit $D600
	bpl ShowTitle_print80_text79
	lda titlemsg,x
	sta $D601
	inx
ShowTitle_print80_loop80
	lda titlemsg,x
	sta $D601
	inx
	cpx #$d
	bne ShowTitle_print80_loop80
	
; // Fill color for string
	lda x1
	sta screen_x_80
	lda #$1
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy #$d ; optimized, look out for bugs
	ldx #$d ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text81
	bit $D600
	bpl ShowTitle_fill80_text81
	sty $D601
	dex
ShowTitle_fill80_loop82
	sty $D601
	dex
	bne ShowTitle_fill80_loop82
	
; // yellow
; // Center the author message
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #16
ShowTitle_rightvarAddSub_var83 = $54
	sta ShowTitle_rightvarAddSub_var83
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var83
	; Calling storevariable on generic assign expression
	sta x1
	
; // Display text	
	sta screen_x_80
	lda #$3
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text84
	bit $D600
	bpl ShowTitle_print80_text84
	lda authormsg1,x
	sta $D601
	inx
ShowTitle_print80_loop85
	lda authormsg1,x
	sta $D601
	inx
	cpx #$21
	bne ShowTitle_print80_loop85
	
; // Fill color for string
	lda x1
	sta screen_x_80
	lda #$3
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy #$5 ; optimized, look out for bugs
	ldx #$21 ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text86
	bit $D600
	bpl ShowTitle_fill80_text86
	sty $D601
	dex
ShowTitle_fill80_loop87
	sty $D601
	dex
	bne ShowTitle_fill80_loop87
	
; // green
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #9
ShowTitle_rightvarAddSub_var88 = $54
	sta ShowTitle_rightvarAddSub_var88
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var88
	; Calling storevariable on generic assign expression
	sta x1
	
; // Display text	
	sta screen_x_80
	lda #$6
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text89
	bit $D600
	bpl ShowTitle_print80_text89
	lda authormsg2,x
	sta $D601
	inx
ShowTitle_print80_loop90
	lda authormsg2,x
	sta $D601
	inx
	cpx #$13
	bne ShowTitle_print80_loop90
	
; // Fill color for string
	lda x1
	sta screen_x_80
	lda #$6
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy #$7 ; optimized, look out for bugs
	ldx #$13 ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text91
	bit $D600
	bpl ShowTitle_fill80_text91
	sty $D601
	dex
ShowTitle_fill80_loop92
	sty $D601
	dex
	bne ShowTitle_fill80_loop92
	
; // cyan
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #11
ShowTitle_rightvarAddSub_var93 = $54
	sta ShowTitle_rightvarAddSub_var93
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var93
	; Calling storevariable on generic assign expression
	sta x1
	
; // Display text	
	sta screen_x_80
	lda #$8
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text94
	bit $D600
	bpl ShowTitle_print80_text94
	lda authormsg3,x
	sta $D601
	inx
ShowTitle_print80_loop95
	lda authormsg3,x
	sta $D601
	inx
	cpx #$16
	bne ShowTitle_print80_loop95
	
; // Fill color for string
	lda x1
	sta screen_x_80
	lda #$8
	sta screen_y_80
	jsr SetScreenPosition80
	; Fill80
	ldy #$7 ; optimized, look out for bugs
	ldx #$16 ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text96
	bit $D600
	bpl ShowTitle_fill80_text96
	sty $D601
	dex
ShowTitle_fill80_loop97
	sty $D601
	dex
	bne ShowTitle_fill80_loop97
	
; // cyan
; // Display Controls
	lda #$1f
	; Calling storevariable on generic assign expression
	sta x1
	
; // Display text	
	sta screen_x_80
	lda #$c
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text98
	bit $D600
	bpl ShowTitle_print80_text98
	lda inst1,x
	sta $D601
	inx
ShowTitle_print80_loop99
	lda inst1,x
	sta $D601
	inx
	cpx #$12
	bne ShowTitle_print80_loop99
	
; // Fill color for string
	lda x1
	sta screen_x_80
	lda #$c
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy #$f ; optimized, look out for bugs
	ldx #$12 ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text100
	bit $D600
	bpl ShowTitle_fill80_text100
	sty $D601
	dex
ShowTitle_fill80_loop101
	sty $D601
	dex
	bne ShowTitle_fill80_loop101
	
; // white
; // Display text	
	lda x1
	sta screen_x_80
	lda #$e
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text102
	bit $D600
	bpl ShowTitle_print80_text102
	lda inst2,x
	sta $D601
	inx
ShowTitle_print80_loop103
	lda inst2,x
	sta $D601
	inx
	cpx #$a
	bne ShowTitle_print80_loop103
	
; // Fill color for string
	lda x1
	sta screen_x_80
	lda #$e
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy #$f ; optimized, look out for bugs
	ldx #$a ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text104
	bit $D600
	bpl ShowTitle_fill80_text104
	sty $D601
	dex
ShowTitle_fill80_loop105
	sty $D601
	dex
	bne ShowTitle_fill80_loop105
	
; // white
; // Center 40/80 message 
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #11
ShowTitle_rightvarAddSub_var106 = $54
	sta ShowTitle_rightvarAddSub_var106
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var106
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x_80
	lda #$12
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text107
	bit $D600
	bpl ShowTitle_print80_text107
	lda inst3,x
	sta $D601
	inx
ShowTitle_print80_loop108
	lda inst3,x
	sta $D601
	inx
	cpx #$16
	bne ShowTitle_print80_loop108
	
; // Fill color for string
	lda x1
	sta screen_x_80
	lda #$e
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy #$f ; optimized, look out for bugs
	ldx #$a ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text109
	bit $D600
	bpl ShowTitle_fill80_text109
	sty $D601
	dex
ShowTitle_fill80_loop110
	sty $D601
	dex
	bne ShowTitle_fill80_loop110
	
; // white
; // Center prompt to continue
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
ShowTitle_rightvarAddSub_var111 = $54
	sta ShowTitle_rightvarAddSub_var111
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var111
	; Calling storevariable on generic assign expression
	sta x1
	
; // Display text
	sta screen_x_80
	lda #$18
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text112
	bit $D600
	bpl ShowTitle_print80_text112
	lda promptmsg,x
	sta $D601
	inx
ShowTitle_print80_loop113
	lda promptmsg,x
	sta $D601
	inx
	cpx #$d
	bne ShowTitle_print80_loop113
	
; //	Fill80(23, length(promptmsg)); 
; // cyan blinking
; //	Fill80(71, length(promptmsg)); 
; // cyan reverse
	lda x1
	sta screen_x_80
	lda #$18
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy #$57 ; optimized, look out for bugs
	ldx #$d ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text114
	bit $D600
	bpl ShowTitle_fill80_text114
	sty $D601
	dex
ShowTitle_fill80_loop115
	sty $D601
	dex
	bne ShowTitle_fill80_loop115
	
; // cyan blinking reverse
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
InitData_forloop117
	
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
	bne InitData_elsedoneblock145
InitData_ConditionalTrueBlock143: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	jsr Random
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc InitData_elsedoneblock157
InitData_ConditionalTrueBlock155: ;Main true block ;keep 
	
; // Option for reverse chars
; // line 165
	; Optimizer: a = a +/- b
	lda char_start
	clc
	adc #$80
	sta char_start
InitData_elsedoneblock157
InitData_elsedoneblock145
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
InitData_forloopcounter119
InitData_loopstart120
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda num_chars
	cmp i ;keep
	bne InitData_forloop117
InitData_loopdone160: ;keep
InitData_forloopend118
InitData_loopend121
	rts
	
; // Check for any user inputs
	; NodeProcedureDecl -1
	; ***********  Defining procedure : CheckInputs
	;    Procedure type : User-defined procedure
CheckInputs
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta key
	
; // RESET
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$51;keep
	bne CheckInputs_elsedoneblock165
CheckInputs_ConditionalTrueBlock163: ;Main true block ;keep 
	
; // Q - Quit
	; Clear screen with offset
	lda #$20
	ldx #$fa
CheckInputs_clearloop169
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne CheckInputs_clearloop169
	jsr $ff3d
CheckInputs_elsedoneblock165
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$30;keep
	bne CheckInputs_elsedoneblock173
CheckInputs_ConditionalTrueBlock171: ;Main true block ;keep 
	
; // 0-9 Adjust speed 
	lda #$9
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock173
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$31;keep
	bne CheckInputs_elsedoneblock179
CheckInputs_ConditionalTrueBlock177: ;Main true block ;keep 
	
; //(slowest)
	lda #$8
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock179
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$32;keep
	bne CheckInputs_elsedoneblock185
CheckInputs_ConditionalTrueBlock183: ;Main true block ;keep 
	lda #$7
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock185
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$33;keep
	bne CheckInputs_elsedoneblock191
CheckInputs_ConditionalTrueBlock189: ;Main true block ;keep 
	lda #$6
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock191
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne CheckInputs_elsedoneblock197
CheckInputs_ConditionalTrueBlock195: ;Main true block ;keep 
	lda #$5
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock197
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$35;keep
	bne CheckInputs_elsedoneblock203
CheckInputs_ConditionalTrueBlock201: ;Main true block ;keep 
	lda #$4
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock203
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$36;keep
	bne CheckInputs_elsedoneblock209
CheckInputs_ConditionalTrueBlock207: ;Main true block ;keep 
	lda #$3
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock209
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$37;keep
	bne CheckInputs_elsedoneblock215
CheckInputs_ConditionalTrueBlock213: ;Main true block ;keep 
	lda #$2
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock215
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne CheckInputs_elsedoneblock221
CheckInputs_ConditionalTrueBlock219: ;Main true block ;keep 
	lda #$1
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock221
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$39;keep
	bne CheckInputs_elsedoneblock227
CheckInputs_ConditionalTrueBlock225: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock227
	rts
	
; // Poke value into an address
	; NodeProcedureDecl -1
	; ***********  Defining procedure : PokeAddr
	;    Procedure type : User-defined procedure
pokeaddr_x	dc.b	0
pokeaddr_y	dc.b	0
pokeaddr_v	dc.b	0
PokeAddr_block230
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
	bcs PokeAddr_elsedoneblock234
PokeAddr_ConditionalTrueBlock232: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock234
	; Binary clause Simplified: LESS
	lda pokeaddr_x
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock240
PokeAddr_ConditionalTrueBlock238: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock240
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
	bcs PokeAddr_elsedoneblock246
PokeAddr_ConditionalTrueBlock244: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock246
	; Binary clause Simplified: LESS
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock252
PokeAddr_ConditionalTrueBlock250: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock252
	
; // VIC screen plot
; // Pointer value to screen RAM
	; Generic 16 bit op
	ldy #0
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	lda pokeaddr_x
PokeAddr_int_shift_var257 = $54
	sta PokeAddr_int_shift_var257
	sty PokeAddr_int_shift_var257+1
		lsr PokeAddr_int_shift_var257+1
	ror PokeAddr_int_shift_var257+0

	lda PokeAddr_int_shift_var257
	ldy PokeAddr_int_shift_var257+1
PokeAddr_rightvarInteger_var258 = $54
	sta PokeAddr_rightvarInteger_var258
	sty PokeAddr_rightvarInteger_var258+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$04
	lda #$00
PokeAddr_rightvarInteger_var261 = $56
	sta PokeAddr_rightvarInteger_var261
	sty PokeAddr_rightvarInteger_var261+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0
	lda pokeaddr_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var261
PokeAddr_wordAdd259
	sta PokeAddr_rightvarInteger_var261
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var261+1
	tay
	lda PokeAddr_rightvarInteger_var261
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var258
PokeAddr_wordAdd255
	sta PokeAddr_rightvarInteger_var258
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var258+1
	tay
	lda PokeAddr_rightvarInteger_var258
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
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	lda pokeaddr_x
PokeAddr_int_shift_var264 = $54
	sta PokeAddr_int_shift_var264
	sty PokeAddr_int_shift_var264+1
		lsr PokeAddr_int_shift_var264+1
	ror PokeAddr_int_shift_var264+0

	lda PokeAddr_int_shift_var264
	ldy PokeAddr_int_shift_var264+1
PokeAddr_rightvarInteger_var265 = $54
	sta PokeAddr_rightvarInteger_var265
	sty PokeAddr_rightvarInteger_var265+1
	; Generic 16 bit op
	; Integer constant assigning
	ldy #$d8
	lda #$00
PokeAddr_rightvarInteger_var268 = $56
	sta PokeAddr_rightvarInteger_var268
	sty PokeAddr_rightvarInteger_var268+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0
	lda pokeaddr_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var268
PokeAddr_wordAdd266
	sta PokeAddr_rightvarInteger_var268
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var268+1
	tay
	lda PokeAddr_rightvarInteger_var268
	; Low bit binop:
	clc
	adc PokeAddr_rightvarInteger_var265
PokeAddr_wordAdd262
	sta PokeAddr_rightvarInteger_var265
	; High-bit binop
	tya
	adc PokeAddr_rightvarInteger_var265+1
	tay
	lda PokeAddr_rightvarInteger_var265
	sta p
	sty p+1
	; Poke
	lda curr_color
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (p),y
	
; // VDC screen plot
; // Fill color for string
	lda pokeaddr_x
	sta screen_x_80
	lda pokeaddr_y
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	; Fill80
	ldy curr_color ; optimized, look out for bugs
	ldx #$2 ; optimized, look out for bugs
	lda #31
	sta $D600
PokeAddr_fill80_text269
	bit $D600
	bpl PokeAddr_fill80_text269
	sty $D601
	dex
PokeAddr_fill80_loop270
	sty $D601
	dex
	bne PokeAddr_fill80_loop270
	
; // Display text
	lda pokeaddr_x
	sta screen_x_80
	lda pokeaddr_y
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Fill80
	ldy pokeaddr_v ; optimized, look out for bugs
	ldx #$2 ; optimized, look out for bugs
	lda #31
	sta $D600
PokeAddr_fill80_text271
	bit $D600
	bpl PokeAddr_fill80_text271
	sty $D601
	dex
PokeAddr_fill80_loop272
	sty $D601
	dex
	bne PokeAddr_fill80_loop272
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
	bne Plot_elsedoneblock277
Plot_ConditionalTrueBlock275: ;Main true block ;keep 
	
; // line 900
	rts
Plot_elsedoneblock277
	
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
	bne Plot_elseblock282
Plot_ConditionalTrueBlock281: ;Main true block ;keep 
	
; // line 920
; // Select pattern style
	lda colSelect
	; Calling storevariable on generic assign expression
	sta plotIndexMax
	jmp Plot_elsedoneblock283
Plot_elseblock282
	lda mainIndex
	; Calling storevariable on generic assign expression
	sta plotIndexMax
Plot_elsedoneblock283
	lda #$1
	; Calling storevariable on generic assign expression
	sta plotIndex
Plot_forloop288
	; Binary clause Simplified: EQUALS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bne Plot_localfailed433
	jmp Plot_ConditionalTrueBlock387
Plot_localfailed433
	jmp Plot_elseblock388
Plot_ConditionalTrueBlock387: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock389
Plot_elseblock388
	; Binary clause Simplified: EQUALS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bne Plot_localfailed458
	jmp Plot_ConditionalTrueBlock437
Plot_localfailed458
	jmp Plot_elseblock438
Plot_ConditionalTrueBlock437: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock439
Plot_elseblock438
	lda y1
	; Calling storevariable on generic assign expression
	sta y2
	; Binary clause Simplified: LESS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bpl Plot_elseblock463
Plot_ConditionalTrueBlock462: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock464
Plot_elseblock463
	
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
Plot_elsedoneblock464
	lda x1
	; Calling storevariable on generic assign expression
	sta x2
	; Binary clause Simplified: LESS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bpl Plot_elseblock471
Plot_ConditionalTrueBlock470: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock472
Plot_elseblock471
	
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
Plot_elsedoneblock472
Plot_elsedoneblock439
Plot_elsedoneblock389
Plot_forloopcounter290
Plot_loopstart291
	; Test Inc dec D
	inc plotIndex
	lda plotIndexMax
	cmp plotIndex ;keep
	beq Plot_loopdone477
Plot_loopnotdone478
	jmp Plot_forloop288
Plot_loopdone477
Plot_forloopend289
Plot_loopend292
	rts
	
; // for
; // End Plot
; // Update the display
	; NodeProcedureDecl -1
	; ***********  Defining procedure : MainLoop
	;    Procedure type : User-defined procedure
MainLoop
	lda start_pos
	; Calling storevariable on generic assign expression
	sta mainIndex
MainLoop_forloop480
	
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
	bcc MainLoop_elsedoneblock496
MainLoop_ConditionalTrueBlock494: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta colorIdx
MainLoop_elsedoneblock496
	
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
MainLoop_forloopcounter482
MainLoop_loopstart483
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
	beq MainLoop_loopdone499
MainLoop_loopnotdone500
	jmp MainLoop_forloop480
MainLoop_loopdone499
MainLoop_forloopend481
MainLoop_loopend484
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
	bne DoMutations_localfailed529
	jmp DoMutations_ConditionalTrueBlock503
DoMutations_localfailed529
	jmp DoMutations_elsedoneblock505
DoMutations_ConditionalTrueBlock503: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda pattern_type
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne DoMutations_elseblock533
DoMutations_ConditionalTrueBlock532: ;Main true block ;keep 
	
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
	jmp DoMutations_elsedoneblock534
DoMutations_elseblock533
	
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
DoMutations_elsedoneblock534
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$40;keep
	bcs DoMutations_elsedoneblock542
DoMutations_ConditionalTrueBlock540: ;Main true block ;keep 
	
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
DoMutations_rightvarAddSub_var551 = $54
	sta DoMutations_rightvarAddSub_var551
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and temp
	 ; end add / sub var with constant
	clc
	adc DoMutations_rightvarAddSub_var551
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
DoMutations_clearloop552
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne DoMutations_clearloop552
	lda #$20
	; Calling storevariable on generic assign expression
	sta myChar
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta myLoc
	sty myLoc+1
	jsr Clear80
DoMutations_elsedoneblock542
DoMutations_elsedoneblock505
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcs DoMutations_localfailed622
	jmp DoMutations_ConditionalTrueBlock554
DoMutations_localfailed622
	jmp DoMutations_elsedoneblock556
DoMutations_ConditionalTrueBlock554: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$4;keep
	bcs DoMutations_elseblock626
DoMutations_ConditionalTrueBlock625: ;Main true block ;keep 
	
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
	jmp DoMutations_elsedoneblock627
DoMutations_elseblock626
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$8;keep
	bcs DoMutations_elseblock658
DoMutations_ConditionalTrueBlock657: ;Main true block ;keep 
	lda #$5f
	; Calling storevariable on generic assign expression
	sta char_start_st
	
; // Triangles
	lda #$b
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jsr InitData
	jmp DoMutations_elsedoneblock659
DoMutations_elseblock658
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$c;keep
	bcs DoMutations_elseblock674
DoMutations_ConditionalTrueBlock673: ;Main true block ;keep 
	lda #$74
	; Calling storevariable on generic assign expression
	sta char_start_st
	
; // Angles
	lda #$b
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jsr InitData
	jmp DoMutations_elsedoneblock675
DoMutations_elseblock674
	lda #$40
	; Calling storevariable on generic assign expression
	sta char_start_st
	
; // Graphics Chars
	lda #$3f
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jsr InitData
DoMutations_elsedoneblock675
DoMutations_elsedoneblock659
DoMutations_elsedoneblock627
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne DoMutations_elseblock682
DoMutations_ConditionalTrueBlock681: ;Main true block ;keep 
	
; // Toggle reverse chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta rev_enable
	jmp DoMutations_elsedoneblock683
DoMutations_elseblock682
	lda #$1
	; Calling storevariable on generic assign expression
	sta rev_enable
DoMutations_elsedoneblock683
	
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
	and #$8
	 ; end add / sub var with constant
	clc
	adc #$c
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta stop_pos
DoMutations_elsedoneblock556
	
; //clearscreen($20, SCREEN_CHAR_LOC);
	; Binary clause Simplified: GREATEREQUAL
	lda cycleCtr
	; Compare with pure num / var optimization
	cmp #$4;keep
	bcc DoMutations_localfailed813
	jmp DoMutations_ConditionalTrueBlock689
DoMutations_localfailed813
	jmp DoMutations_elsedoneblock691
DoMutations_ConditionalTrueBlock689: ;Main true block ;keep 
	
; // Update color palette	
	jsr Random
	; Calling storevariable on generic assign expression
	sta rando
	; Binary clause Simplified: LESS
	; Compare with pure num / var optimization
	cmp #$33;keep
	bcs DoMutations_elseblock817
DoMutations_ConditionalTrueBlock816: ;Main true block ;keep 
	lda #<cArr1
	ldx #>cArr1
	sta cArrPtr
	stx cArrPtr+1
	lda #$8
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock818
DoMutations_elseblock817
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$66;keep
	bcs DoMutations_elseblock881
DoMutations_ConditionalTrueBlock880: ;Main true block ;keep 
	lda #<cArr2
	ldx #>cArr2
	sta cArrPtr
	stx cArrPtr+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock882
DoMutations_elseblock881
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$99;keep
	bcs DoMutations_elseblock913
DoMutations_ConditionalTrueBlock912: ;Main true block ;keep 
	lda #<cArr3
	ldx #>cArr3
	sta cArrPtr
	stx cArrPtr+1
	lda #$7
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock914
DoMutations_elseblock913
	; Binary clause Simplified: LESS
	lda rando
	; Compare with pure num / var optimization
	cmp #$cc;keep
	bcs DoMutations_elseblock929
DoMutations_ConditionalTrueBlock928: ;Main true block ;keep 
	lda #<cArr4
	ldx #>cArr4
	sta cArrPtr
	stx cArrPtr+1
	lda #$7
	; Calling storevariable on generic assign expression
	sta cArrLen
	jmp DoMutations_elsedoneblock930
DoMutations_elseblock929
	lda #<cArr5
	ldx #>cArr5
	sta cArrPtr
	stx cArrPtr+1
	lda #$b
	; Calling storevariable on generic assign expression
	sta cArrLen
DoMutations_elsedoneblock930
DoMutations_elsedoneblock914
DoMutations_elsedoneblock882
DoMutations_elsedoneblock818
	lda #$0
	; Calling storevariable on generic assign expression
	sta cycleCtr
DoMutations_elsedoneblock691
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
	ldx #$fa
MainProgram_clearloop935
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne MainProgram_clearloop935
	lda #$20
	; Calling storevariable on generic assign expression
	sta myChar
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta myLoc
	sty myLoc+1
	jsr Clear80
	
; // Init VDC screen mem
	lda #$3
	; Calling storevariable on generic assign expression
	sta myChar
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$8
	; Calling storevariable on generic assign expression
	sta myLoc
	sty myLoc+1
	jsr Clear80
MainProgram_while936
MainProgram_loopstart940
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock939
MainProgram_ConditionalTrueBlock937: ;Main true block ;keep 
	
; // Init VDC color mem 
	jsr MainLoop
	
; // Do Pattern Mutations
	jsr DoMutations
	
; // Get fresh array of chars
	jsr InitData
	jmp MainProgram_while936
MainProgram_elsedoneblock939
MainProgram_loopend941
	; End of program
	; Ending memory block at $2020
EndBlock2020:
