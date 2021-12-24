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
centerX	dc.b	$28
centerY	dc.b	$0c
start_pos	dc.b	$01
stop_pos	dc.b	$0c
dir	dc.b	$01
char_start	dc.b	0
char_offset	dc.b	0
char_start_st	dc.b	$41
char_offset_st	dc.b	$3e
num_chars	dc.b	$06
speed	dc.b	$03
rev_enable	dc.b	$00
pattern_type	dc.b	$01
full_screen	dc.b	$01
curr_color	dc.b	$01
char_arr	dc.b	 
	org char_arr+12
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
	dc.b	$10, $0c, $05, $01, $13, $05, $20, $13, $17
	dc.b	$09, $14, $03, $08, $20, $14, $0f, $20, $38
	dc.b	$30, $20, $03, $0f, $0c, $15, $0d, $0e, $20
	dc.b	$13, $03, $12, $05, $05, $0e, 0
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
	jmp initmoveto_moveto2
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
initmoveto_moveto2
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto80
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto80_moveto803
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
initmoveto80_moveto803
	lda #18
	sta $D600
initmoveto80_a_moveto804
	bit $D600
	bpl initmoveto80_a_moveto804
	lda screenmemory80+1
	sta $D601
	lda #19
	sta $D600
initmoveto80_b_moveto805
	bit $D600
	bpl initmoveto80_b_moveto805
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
	bcc initrandom256_RandomSkip6
	eor #$4d
initrandom256_RandomSkip6
	eor $dc04
	sta Random+1
	rts
	
; // Used for keyboard input
; // Text for splash screen  	
; // PRESS ANY KEY
; //promptmsg: cstring =(208, 210, 197, 211, 211, 32, 193, 206, 217, 32, 203, 197, 217);
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
WaitForKeypress_while9
WaitForKeypress_loopstart13
	; Binary clause Simplified: EQUALS
	lda temp
	; Compare with pure num / var optimization
	cmp #$0;keep
	; Signed compare
	bne WaitForKeypress_elsedoneblock12
WaitForKeypress_ConditionalTrueBlock10: ;Main true block ;keep 
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta temp
	jmp WaitForKeypress_while9
WaitForKeypress_elsedoneblock12
WaitForKeypress_loopend14
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
	bcc DoDelay_elsedoneblock21
DoDelay_ConditionalTrueBlock19: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
DoDelay_forloop32
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
DoDelay_forloopcounter34
DoDelay_loopstart35
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda speed
	cmp i ;keep
	bne DoDelay_forloop32
DoDelay_loopdone39: ;keep
DoDelay_forloopend33
DoDelay_loopend36
DoDelay_elsedoneblock21
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
Clear80_block40
Clear80
	
; // space char
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
Clear80_forloop41
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
Clear80_fill80_text50
	bit $D600
	bpl Clear80_fill80_text50
	sty $D601
	dex
Clear80_fill80_loop51
	sty $D601
	dex
	bne Clear80_fill80_loop51
Clear80_forloopcounter43
Clear80_loopstart44
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$19
	cmp i ;keep
	bne Clear80_forloop41
Clear80_loopdone52: ;keep
Clear80_forloopend42
Clear80_loopend45
	rts
	
; //	Method which shows title screen and checks screen width
	; NodeProcedureDecl -1
	; ***********  Defining procedure : ShowTitle
	;    Procedure type : User-defined procedure
ShowTitle
	
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
; // Set uppercase
; //poke(^59468, 0, 12);
; // Set background/border colors(40-col)
	; Poke
	; Optimization: shift is zero
	lda #$0
	sta $d020
	; Poke
	; Optimization: shift is zero
	sta $d021
	
; // Clear screen(40-col)
	; Clear screen with offset
	lda #$20
	ldx #$fa
ShowTitle_clearloop54
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne ShowTitle_clearloop54
	
; // Set cyan color(40-col)
	; Clear screen with offset
	lda #$3
	ldx #$fa
ShowTitle_clearloop55
	dex
	sta $0000+$d800,x
	sta $00fa+$d800,x
	sta $01f4+$d800,x
	sta $02ee+$d800,x
	bne ShowTitle_clearloop55
	
; // Show message to switch to 80 columns on VIC screen
	lda #$4
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$8
	sta screen_y
	lda #>$400
	jsr SetScreenPosition
	clc
	lda #<inst3
	adc #$0
	ldy #>inst3
	sta print_text+0
	sty print_text+1
	ldx #$21 ; optimized, look out for bugs
	jsr printstring
	
; // VDC text matrix is at $0000
; // VDC color matrix is at $0800
; // Center the title text on VDC screen 
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
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
	
; // Fill color for string
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
ShowTitle_fill80_text59
	bit $D600
	bpl ShowTitle_fill80_text59
	sty $D601
	dex
ShowTitle_fill80_loop60
	sty $D601
	dex
	bne ShowTitle_fill80_loop60
	
; // yellow
; // Display text	
	lda x1
	sta screen_x_80
	lda #$1
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text61
	bit $D600
	bpl ShowTitle_print80_text61
	lda titlemsg,x
	sta $D601
	inx
ShowTitle_print80_loop62
	lda titlemsg,x
	sta $D601
	inx
	cpx #$d
	bne ShowTitle_print80_loop62
	
; // Center the author message
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #16
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
	
; // Fill color for string
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
ShowTitle_fill80_text64
	bit $D600
	bpl ShowTitle_fill80_text64
	sty $D601
	dex
ShowTitle_fill80_loop65
	sty $D601
	dex
	bne ShowTitle_fill80_loop65
	
; // green
; // Display text	
	lda x1
	sta screen_x_80
	lda #$3
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text66
	bit $D600
	bpl ShowTitle_print80_text66
	lda authormsg1,x
	sta $D601
	inx
ShowTitle_print80_loop67
	lda authormsg1,x
	sta $D601
	inx
	cpx #$21
	bne ShowTitle_print80_loop67
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #9
ShowTitle_rightvarAddSub_var68 = $54
	sta ShowTitle_rightvarAddSub_var68
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var68
	; Calling storevariable on generic assign expression
	sta x1
	
; // Fill color for string
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
ShowTitle_fill80_text69
	bit $D600
	bpl ShowTitle_fill80_text69
	sty $D601
	dex
ShowTitle_fill80_loop70
	sty $D601
	dex
	bne ShowTitle_fill80_loop70
	
; // cyan
; // Display text	
	lda x1
	sta screen_x_80
	lda #$6
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text71
	bit $D600
	bpl ShowTitle_print80_text71
	lda authormsg2,x
	sta $D601
	inx
ShowTitle_print80_loop72
	lda authormsg2,x
	sta $D601
	inx
	cpx #$13
	bne ShowTitle_print80_loop72
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #11
ShowTitle_rightvarAddSub_var73 = $54
	sta ShowTitle_rightvarAddSub_var73
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var73
	; Calling storevariable on generic assign expression
	sta x1
	
; // Fill color for string
	sta screen_x_80
	lda #$8
	sta screen_y_80
	jsr SetScreenPosition80
	; Fill80
	ldy #$7 ; optimized, look out for bugs
	ldx #$16 ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text74
	bit $D600
	bpl ShowTitle_fill80_text74
	sty $D601
	dex
ShowTitle_fill80_loop75
	sty $D601
	dex
	bne ShowTitle_fill80_loop75
	
; // cyan
; // Display text	
	lda x1
	sta screen_x_80
	lda #$8
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text76
	bit $D600
	bpl ShowTitle_print80_text76
	lda authormsg3,x
	sta $D601
	inx
ShowTitle_print80_loop77
	lda authormsg3,x
	sta $D601
	inx
	cpx #$16
	bne ShowTitle_print80_loop77
	
; // Display Controls
	lda #$1f
	; Calling storevariable on generic assign expression
	sta x1
	
; // Fill color for string
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
ShowTitle_fill80_text78
	bit $D600
	bpl ShowTitle_fill80_text78
	sty $D601
	dex
ShowTitle_fill80_loop79
	sty $D601
	dex
	bne ShowTitle_fill80_loop79
	
; // white
; // Display text	
	lda x1
	sta screen_x_80
	lda #$c
	sta screen_y_80
	lda #$0
	jsr SetScreenPosition80
	; Print80
	ldx #0
	ldy #31
	sty $D600
ShowTitle_print80_text80
	bit $D600
	bpl ShowTitle_print80_text80
	lda inst1,x
	sta $D601
	inx
ShowTitle_print80_loop81
	lda inst1,x
	sta $D601
	inx
	cpx #$12
	bne ShowTitle_print80_loop81
	
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
ShowTitle_fill80_text82
	bit $D600
	bpl ShowTitle_fill80_text82
	sty $D601
	dex
ShowTitle_fill80_loop83
	sty $D601
	dex
	bne ShowTitle_fill80_loop83
	
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
ShowTitle_print80_text84
	bit $D600
	bpl ShowTitle_print80_text84
	lda inst2,x
	sta $D601
	inx
ShowTitle_print80_loop85
	lda inst2,x
	sta $D601
	inx
	cpx #$a
	bne ShowTitle_print80_loop85
	
; // Center prompt to continue
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
ShowTitle_rightvarAddSub_var86 = $54
	sta ShowTitle_rightvarAddSub_var86
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc ShowTitle_rightvarAddSub_var86
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x_80
	lda #$18
	sta screen_y_80
	lda #$8
	jsr SetScreenPosition80
	
; //	Fill80(23, length(promptmsg)); 
; // cyan blinking
; //	Fill80(71, length(promptmsg)); 
; // cyan reverse
	; Fill80
	ldy #$57 ; optimized, look out for bugs
	ldx #$d ; optimized, look out for bugs
	lda #31
	sta $D600
ShowTitle_fill80_text87
	bit $D600
	bpl ShowTitle_fill80_text87
	sty $D601
	dex
ShowTitle_fill80_loop88
	sty $D601
	dex
	bne ShowTitle_fill80_loop88
	
; // cyan blinking reverse
; // Display text
	lda x1
	sta screen_x_80
	lda #$18
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
	lda promptmsg,x
	sta $D601
	inx
ShowTitle_print80_loop90
	lda promptmsg,x
	sta $D601
	inx
	cpx #$d
	bne ShowTitle_print80_loop90
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
InitData_forloop92
	
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
	bne InitData_elsedoneblock120
InitData_ConditionalTrueBlock118: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	jsr Random
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc InitData_elsedoneblock132
InitData_ConditionalTrueBlock130: ;Main true block ;keep 
	
; // Option for reverse chars
; // line 165
	; Optimizer: a = a +/- b
	lda char_start
	clc
	adc #$80
	sta char_start
InitData_elsedoneblock132
InitData_elsedoneblock120
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
InitData_forloopcounter94
InitData_loopstart95
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda num_chars
	cmp i ;keep
	bne InitData_forloop92
InitData_loopdone135: ;keep
InitData_forloopend93
InitData_loopend96
	rts
	
; // Check for any user inputs
	; NodeProcedureDecl -1
	; ***********  Defining procedure : CheckInputs
	;    Procedure type : User-defined procedure
CheckInputs
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta key
	
; // Reboot
; //call(^$fffc); 
; // RESET
; //call(^$fffe); 
; // NMI
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$51;keep
	bne CheckInputs_elsedoneblock140
CheckInputs_ConditionalTrueBlock138: ;Main true block ;keep 
	
; // Q - Quit
	jsr $ff3d
CheckInputs_elsedoneblock140
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$30;keep
	bne CheckInputs_elsedoneblock146
CheckInputs_ConditionalTrueBlock144: ;Main true block ;keep 
	
; // 0-9 Adjust speed 
	lda #$9
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock146
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$31;keep
	bne CheckInputs_elsedoneblock152
CheckInputs_ConditionalTrueBlock150: ;Main true block ;keep 
	
; //(slowest)
	lda #$8
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock152
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$32;keep
	bne CheckInputs_elsedoneblock158
CheckInputs_ConditionalTrueBlock156: ;Main true block ;keep 
	lda #$7
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock158
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$33;keep
	bne CheckInputs_elsedoneblock164
CheckInputs_ConditionalTrueBlock162: ;Main true block ;keep 
	lda #$6
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock164
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne CheckInputs_elsedoneblock170
CheckInputs_ConditionalTrueBlock168: ;Main true block ;keep 
	lda #$5
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock170
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$35;keep
	bne CheckInputs_elsedoneblock176
CheckInputs_ConditionalTrueBlock174: ;Main true block ;keep 
	lda #$4
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock176
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$36;keep
	bne CheckInputs_elsedoneblock182
CheckInputs_ConditionalTrueBlock180: ;Main true block ;keep 
	lda #$3
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock182
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$37;keep
	bne CheckInputs_elsedoneblock188
CheckInputs_ConditionalTrueBlock186: ;Main true block ;keep 
	lda #$2
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock188
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne CheckInputs_elsedoneblock194
CheckInputs_ConditionalTrueBlock192: ;Main true block ;keep 
	lda #$1
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock194
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$39;keep
	bne CheckInputs_elsedoneblock200
CheckInputs_ConditionalTrueBlock198: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta speed
CheckInputs_elsedoneblock200
	rts
	
; //(fastest)
; // Poke value into an address
	; NodeProcedureDecl -1
	; ***********  Defining procedure : PokeAddr
	;    Procedure type : User-defined procedure
pokeaddr_x	dc.b	0
pokeaddr_y	dc.b	0
pokeaddr_v	dc.b	0
PokeAddr_block203
PokeAddr
	; Swapped comparison expressions
	; Binary clause Simplified: LESS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc #$2
	 ; end add / sub var with constant
	; Compare with pure num / var optimization
	cmp pokeaddr_x;keep
	bcs PokeAddr_elsedoneblock207
PokeAddr_ConditionalTrueBlock205: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock207
	; Binary clause Simplified: LESS
	lda pokeaddr_x
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock213
PokeAddr_ConditionalTrueBlock211: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock213
	; Binary clause Simplified: GREATER
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp myscreenheight;keep
	bcc PokeAddr_elsedoneblock219
	beq PokeAddr_elsedoneblock219
PokeAddr_ConditionalTrueBlock217: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock219
	; Binary clause Simplified: LESS
	lda pokeaddr_y
	; Compare with pure num / var optimization
	cmp #$0;keep
	bcs PokeAddr_elsedoneblock225
PokeAddr_ConditionalTrueBlock223: ;Main true block ;keep 
	rts
PokeAddr_elsedoneblock225
	
; // Alternately, select char from string by index 
; //temp := outstr[0];
; //Fill80(temp, 2);
; // Debug
; //PrintXYZ(pokeaddr_x, pokeaddr_y, pokeaddr_v);
; //WaitForKeypress();
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne PokeAddr_elseblock230
PokeAddr_ConditionalTrueBlock229: ;Main true block ;keep 
	jmp PokeAddr_elsedoneblock231
PokeAddr_elseblock230
	
; // VIC screen plot
; // Pointer value to screen RAM
; //p := SCREEN_CHAR_LOC + SCREEN_WIDTH * pokeaddr_y + pokeaddr_x;
; //poke(p, 0, pokeaddr_v);
; // Pointer value to color RAM
; //p := SCREEN_COL_LOC + SCREEN_WIDTH * pokeaddr_y + pokeaddr_x;
; //poke(p, 0, curr_color);
; // VDC screen plot
; // $D600 is VDC Address Register
; // $D601 is VDC Data Register
; // VDC Character Display area(screen) $0000-$07FF
; // VDC Character Attributes(color) $0800-$0FFF
; // VDC Character Definitions $2000-$3FFF
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
PokeAddr_fill80_text240
	bit $D600
	bpl PokeAddr_fill80_text240
	sty $D601
	dex
PokeAddr_fill80_loop241
	sty $D601
	dex
	bne PokeAddr_fill80_loop241
	
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
PokeAddr_fill80_text242
	bit $D600
	bpl PokeAddr_fill80_text242
	sty $D601
	dex
PokeAddr_fill80_loop243
	sty $D601
	dex
	bne PokeAddr_fill80_loop243
PokeAddr_elsedoneblock231
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
	
; // Map points in the four indexes
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
	; Signed compare
	bne Plot_elsedoneblock248
Plot_ConditionalTrueBlock246: ;Main true block ;keep 
	
; // line 900
; //PokeAddr(x1, y1, outstr[0]);					
; // line 900
	rts
Plot_elsedoneblock248
	
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
	bne Plot_elseblock253
Plot_ConditionalTrueBlock252: ;Main true block ;keep 
	
; // line 920
; // Select pattern style
	lda colSelect
	; Calling storevariable on generic assign expression
	sta plotIndexMax
	jmp Plot_elsedoneblock254
Plot_elseblock253
	lda mainIndex
	; Calling storevariable on generic assign expression
	sta plotIndexMax
Plot_elsedoneblock254
	lda #$1
	; Calling storevariable on generic assign expression
	sta plotIndex
Plot_forloop259
	; Binary clause Simplified: EQUALS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bne Plot_localfailed404
	jmp Plot_ConditionalTrueBlock358
Plot_localfailed404
	jmp Plot_elseblock359
Plot_ConditionalTrueBlock358: ;Main true block ;keep 
	
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
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
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
	jmp Plot_elsedoneblock360
Plot_elseblock359
	; Binary clause Simplified: EQUALS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bne Plot_localfailed429
	jmp Plot_ConditionalTrueBlock408
Plot_localfailed429
	jmp Plot_elseblock409
Plot_ConditionalTrueBlock408: ;Main true block ;keep 
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
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
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
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
	jmp Plot_elsedoneblock410
Plot_elseblock409
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
	lda y1
	; Calling storevariable on generic assign expression
	sta y2
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
	; Binary clause Simplified: LESS
	lda x1
	; Compare with pure num / var optimization
	cmp centerX;keep
	; Signed compare
	bpl Plot_elseblock434
Plot_ConditionalTrueBlock433: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock435
Plot_elseblock434
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
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
Plot_elsedoneblock435
	lda x1
	; Calling storevariable on generic assign expression
	sta x2
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
	; Binary clause Simplified: LESS
	lda y1
	; Compare with pure num / var optimization
	cmp centerY;keep
	; Signed compare
	bpl Plot_elseblock442
Plot_ConditionalTrueBlock441: ;Main true block ;keep 
	
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
	jmp Plot_elsedoneblock443
Plot_elseblock442
	
; //PokeAddr(x2, y2, outstr[plotIndex]);
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
Plot_elsedoneblock443
Plot_elsedoneblock410
Plot_elsedoneblock360
Plot_forloopcounter261
Plot_loopstart262
	; Test Inc dec D
	inc plotIndex
	lda plotIndexMax
	cmp plotIndex ;keep
	beq Plot_loopdone448
Plot_loopnotdone449
	jmp Plot_forloop259
Plot_loopdone448
Plot_forloopend260
Plot_loopend263
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
MainLoop_forloop451
	
; // line 200
; // Update color
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and #$f
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta curr_color
MainLoop_while466
MainLoop_loopstart470
	; Binary clause Simplified: EQUALS
	lda curr_color
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne MainLoop_elsedoneblock469
MainLoop_ConditionalTrueBlock467: ;Main true block ;keep 
	
; // If black, pick another color..
	; 8 bit binop
	; Add/sub where right value is constant number
	jsr Random
	and #$f
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta curr_color
	jmp MainLoop_while466
MainLoop_elsedoneblock469
MainLoop_loopend471
	
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
; // Check for user input	
	jsr CheckInputs
MainLoop_forloopcounter453
MainLoop_loopstart454
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
	beq MainLoop_loopdone474
MainLoop_loopnotdone475
	jmp MainLoop_forloop451
MainLoop_loopdone474
MainLoop_forloopend452
MainLoop_loopend455
	rts
	
; // Various Pattern Mutations
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DoMutations
	;    Procedure type : User-defined procedure
DoMutations
	
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
	bne DoMutations_localfailed538
	jmp DoMutations_ConditionalTrueBlock478
DoMutations_localfailed538
	jmp DoMutations_elsedoneblock480
DoMutations_ConditionalTrueBlock478: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda pattern_type
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne DoMutations_elseblock542
DoMutations_ConditionalTrueBlock541: ;Main true block ;keep 
	
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
	jmp DoMutations_elsedoneblock543
DoMutations_elseblock542
	
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
DoMutations_elsedoneblock543
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$80;keep
	; Signed compare
	bpl DoMutations_elsedoneblock551
DoMutations_ConditionalTrueBlock549: ;Main true block ;keep 
	
; // Randomly change center point
; // Determines width & height of area where pattern originates
	jsr Random
	; Calling storevariable on generic assign expression
	sta centerX
DoMutations_while572
DoMutations_loopstart576
	; Binary clause Simplified: LESS
	lda centerX
	; Compare with pure num / var optimization
	cmp #$f;keep
	bcs DoMutations_localfailed579
	jmp DoMutations_ConditionalTrueBlock573
DoMutations_localfailed579: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: GREATEREQUAL
	lda centerX
	; Compare with pure num / var optimization
	cmp #$42;keep
	bcc DoMutations_elsedoneblock575
DoMutations_ConditionalTrueBlock573: ;Main true block ;keep 
	jsr Random
	; Calling storevariable on generic assign expression
	sta centerX
	jmp DoMutations_while572
DoMutations_elsedoneblock575
DoMutations_loopend577
	jsr Random
	; Calling storevariable on generic assign expression
	sta centerY
DoMutations_while581
DoMutations_loopstart585
	; Binary clause Simplified: LESS
	lda centerY
	; Compare with pure num / var optimization
	cmp #$6;keep
	bcs DoMutations_localfailed588
	jmp DoMutations_ConditionalTrueBlock582
DoMutations_localfailed588: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: GREATEREQUAL
	lda centerY
	; Compare with pure num / var optimization
	cmp #$13;keep
	bcc DoMutations_elsedoneblock584
DoMutations_ConditionalTrueBlock582: ;Main true block ;keep 
	jsr Random
	; Calling storevariable on generic assign expression
	sta centerY
	jmp DoMutations_while581
DoMutations_elsedoneblock584
DoMutations_loopend586
DoMutations_elsedoneblock551
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$4;keep
	; Signed compare
	bpl DoMutations_elsedoneblock593
DoMutations_ConditionalTrueBlock591: ;Main true block ;keep 
	
; // Clear screen once in a while
	lda #$20
	; Calling storevariable on generic assign expression
	sta myChar
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta myLoc
	sty myLoc+1
	jsr Clear80
DoMutations_elsedoneblock593
DoMutations_elsedoneblock480
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$20;keep
	; Signed compare
	bpl DoMutations_elsedoneblock599
DoMutations_ConditionalTrueBlock597: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda temp
	; Compare with pure num / var optimization
	cmp #$10;keep
	; Signed compare
	bpl DoMutations_elseblock612
DoMutations_ConditionalTrueBlock611: ;Main true block ;keep 
	
; // Select char range
; //if( temp < 11 ) then
; //begin
; //	char_start_st	:= 234;
; //	char_offset_st	:= 22;
; //end
; //else if( temp < 22 ) then
	lda #$60
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$1f
	; Calling storevariable on generic assign expression
	sta char_offset_st
	jmp DoMutations_elsedoneblock613
DoMutations_elseblock612
	lda #$41
	; Calling storevariable on generic assign expression
	sta char_start_st
	lda #$3e
	; Calling storevariable on generic assign expression
	sta char_offset_st
DoMutations_elsedoneblock613
DoMutations_elsedoneblock599
	; Binary clause Simplified: LESS
	jsr Random
	; Compare with pure num / var optimization
	cmp #$40;keep
	bcs DoMutations_elsedoneblock621
DoMutations_ConditionalTrueBlock619: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda rev_enable
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne DoMutations_elseblock634
DoMutations_ConditionalTrueBlock633: ;Main true block ;keep 
	
; // Toggle reverse chars
	lda #$0
	; Calling storevariable on generic assign expression
	sta rev_enable
	jmp DoMutations_elsedoneblock635
DoMutations_elseblock634
	lda #$1
	; Calling storevariable on generic assign expression
	sta rev_enable
DoMutations_elsedoneblock635
DoMutations_elsedoneblock621
	
; //clearscreen($20, SCREEN_CHAR_LOC);
; //		Clear80(32, 0); 
	; Binary clause Simplified: LESS
	jsr Random
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcs DoMutations_elsedoneblock643
DoMutations_ConditionalTrueBlock641: ;Main true block ;keep 
	
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
	bne DoMutations_elseblock656
DoMutations_ConditionalTrueBlock655: ;Main true block ;keep 
	
; // Toggle the flag
	lda #$1
	; Calling storevariable on generic assign expression
	sta full_screen
	lda #$18
	; Calling storevariable on generic assign expression
	sta stop_pos
	jmp DoMutations_elsedoneblock657
DoMutations_elseblock656
	lda #$0
	; Calling storevariable on generic assign expression
	sta full_screen
	lda #$c
	; Calling storevariable on generic assign expression
	sta stop_pos
DoMutations_elsedoneblock657
DoMutations_elsedoneblock643
	rts
block1
	
; // Debug
; //PrintXYZ(temp, 0, 0);
; //WaitForKeypress();
; //call(^$fd49);	
; // Trigger NMI	
; // Show the title and check number of columns
	jsr ShowTitle
	
; // Set 2 mhz mode
	lda $D030
	ora #1
	sta $D030
	
; //poke(^53296, 0, 1);  	
; //	2Mhz(works in both 64 and 128 mode) POKE 53296,1
; //poke(^53296, 0, 0);	
; //	1Mhz(works in both 64 and 128 mode) POKE 53296,0
; // Init char array	
	jsr InitData
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
MainProgram_while662
MainProgram_loopstart666
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock665
MainProgram_ConditionalTrueBlock663: ;Main true block ;keep 
	
; // Init VDC color mem 
	jsr MainLoop
	
; // Pattern modifications
	jsr DoMutations
	
; // Get fresh array of chars
	jsr InitData
	jmp MainProgram_while662
MainProgram_elsedoneblock665
MainProgram_loopend667
	; End of program
	; Ending memory block at $2020
EndBlock2020:
