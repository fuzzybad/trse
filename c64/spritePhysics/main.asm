
 processor 6502
	org $801
StartBlock801:
	; Starting new memory block at $801
	.byte $b ; lo byte of next line
	.byte $8 ; hi byte of next line
	.byte $0a, $00 ; line 10 (lo, hi)
	.byte $9e, $20 ; SYS token and a space
	.byte   $32,$30,$36,$34
	.byte $00, $00, $00 ; end of program
	; Ending memory block at $801
EndBlock801:
	org $810
StartBlock810:
	; Starting new memory block at $810
SpritePhysics
	jmp block1
Key_keys	dc.b	 
	org Key_keys+8
Key_keysLast	dc.b	 
	org Key_keysLast+8
Key_keysPressed	dc.b	 
	org Key_keysPressed+8
Key_keyState	dc.b	0
Key_temp	dc.b	0
Key_KeyRow = $dc00
Key_KeyRead = $dc01
Key_DataDirA = $dc02
Key_DataDirB = $dc03
Key_keyRowValues	dc.b $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f
loc_work	= $02
i = $3fc
sprite_x	dc.w	$62c
sprite_y	dc.w	$280
dir_x	dc.w	$00
dir_y	dc.w	$00
max_x	dc.b	$64
max_y	dc.b	$50
inc_amt	dc.b	$0a
sprite_x_hi	dc.b	$00
phyx_enable	dc.b	$01
border_rt	dc.w	$1f8
border_lt	dc.w	$d8
border_tp	dc.w	$00
border_bt	dc.w	$820
sprite_data_arr	dc.b $0, $7f, $0, $1, $ff, $c0, $3, $ff
	dc.b $e0, $3, $e7, $e0, $7, $d9, $f0, $7
	dc.b $df, $f0, $7, $d9, $f0, $3, $e7, $e0
	dc.b $3, $ff, $e0, $3, $ff, $e0, $2, $ff
	dc.b $a0, $1, $7f, $40, $1, $3e, $40, $0
	dc.b $9c, $80, $0, $9c, $80, $0, $49, $0
	dc.b $0, $49, $0, $0, $3e, $0, $0, $3e
	dc.b $0, $0, $3e, $0, $0, $1c, $0
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $4C     ;$59 used for hi-byte
initdiv16x8_dividend = $4E	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4E ;save memory by reusing divident to store the result
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
end_procedure_init16x8div
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4C
mul16x8_num1 = $4E
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
end_procedure_init16x8mul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $4C
div8x8_d = $4E
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
end_procedure_init8x8div
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4C
multiplier_a = $4E
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $4E
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
end_procedure_initeightbitmul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key_Read
	;    Procedure type : User-defined procedure
Key_Read
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta Key_keyState
	
; // set data port direction
	; Forcetype: NADA
	lda #$ff
	; Calling storevariable on generic assign expression
	sta Key_DataDirA
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta Key_DataDirB
	
; // row to test
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta Key_temp
Key_Read_forloop4
	; Load Byte array
	; CAST type NADA
	ldx Key_temp
	lda Key_keyRowValues,x 
	; Calling storevariable on generic assign expression
	sta Key_KeyRow
	; ****** Inline assembler section
		lda Key_KeyRead ; get bits
		eor #$ff ; invert so the below works
		sta Key_keys,x
		eor Key_keysLast,x
		and Key_keys,x
		sta Key_keysPressed,x
		lda Key_keys,x
		sta Key_keysLast,x
		
		; record if no key pressed
		clc
		adc Key_keyState
		sta Key_keyState
		
		
Key_Read_loopstart5
	; Compare is onpage
	; Test Inc dec D
	inc Key_temp
	; Forcetype: NADA
	lda #$8
	cmp Key_temp ;keep
	bne Key_Read_forloop4
Key_Read_loopdone9: ;keep
Key_Read_loopend6
	rts
end_procedure_Key_Read
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key_Held
	;    Procedure type : User-defined procedure
Key_Held_block10
Key_Held
	; ****** Inline assembler section
	
	sty Key_temp
	lda Key_keys,x
	and Key_temp 	; contains column 
	
	
	rts
end_procedure_Key_Held
	;*
; // *	C64 Sprite Physics demo
; // *	JLP 2023(fuzzybad@gmail.com)
; // 
; // Border color 
; // Background color 0
; // location of sprite data
; // Default X pos
; // Default Y pos
; // $3FC-$3FF are unused
; // NOTE: Position & inc vars are x10, in order to calculate fracional values
; //
; //   20 x=165: y=64: dx=0: dy=0: r=0
; // inner edge of right border
; // inner edge of left border
; // 500;
; //   40 v=53248
; // not needed
; //  300 data 0,127,0,1,255,192,3,255,224,3,231,224
; //  310 data 7,217,240,7,223,240,7,217,240,3,231,224
; //  320 data 3,255,224,3,255,224,2,255,160,1,127,64
; //  330 data 1,62,64,0,156,128,0,156,128,0,73,0,0,73,0
; //  340 data 0,62,0,0,62,0,0,62,0,0,28,0
; // at 832;
; // Delay function
	; NodeProcedureDecl -1
	; ***********  Defining procedure : do_delay
	;    Procedure type : User-defined procedure
dd_i	dc.b	0
delay_val	dc.b	0
do_delay_block11
do_delay
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda delay_val
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc do_delay_elsedoneblock15
do_delay_ConditionalTrueBlock13: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dd_i
do_delay_forloop24
	; Wait
	; Forcetype: NADA
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
do_delay_loopstart25
	; Compare is onpage
	; Test Inc dec D
	inc dd_i
	lda delay_val
	cmp dd_i ;keep
	bne do_delay_forloop24
do_delay_loopdone29: ;keep
do_delay_loopend26
do_delay_elsedoneblock15
	rts
end_procedure_do_delay
	; NodeProcedureDecl -1
	; ***********  Defining procedure : checkInput
	;    Procedure type : User-defined procedure
checkInput
	
; // Read Keyboard
; //  100 get a$
	jsr Key_Read
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$01
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock34
checkInput_ConditionalTrueBlock32: ;Main true block ;keep 
	
; // Check cursor up/down
; //  110 if a$="{up}" then dy=dy-1
; //  120 if a$="{down}" then dy=dy+1
	lda dir_y
	sec
	sbc inc_amt
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs checkInput_WordAdd38
	dec dir_y+1
checkInput_WordAdd38
checkInput_elsedoneblock34
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$01
	ldy #$20
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock42
checkInput_ConditionalTrueBlock40: ;Main true block ;keep 
	
; // up
	lda dir_y
	clc
	adc inc_amt
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc checkInput_WordAdd46
	inc dir_y+1
checkInput_WordAdd46
checkInput_elsedoneblock42
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$01
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock50
checkInput_ConditionalTrueBlock48: ;Main true block ;keep 
	
; // down	
; // Check cursor left/right
; //  130 if a$="{left}" then dx=dx-1
; //  140 if a$="{rght}" then dx=dx+1
	lda dir_x
	sec
	sbc inc_amt
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs checkInput_WordAdd54
	dec dir_x+1
checkInput_WordAdd54
checkInput_elsedoneblock50
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$02
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock58
checkInput_ConditionalTrueBlock56: ;Main true block ;keep 
	
; // left
	lda dir_x
	clc
	adc inc_amt
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc checkInput_WordAdd62
	inc dir_x+1
checkInput_WordAdd62
checkInput_elsedoneblock58
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$07
	ldy #$10
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock66
checkInput_ConditionalTrueBlock64: ;Main true block ;keep 
	
; // right
; //  150 if a$=" " then dx=0: dy=0: x=165: y=64
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta dir_x
	sty dir_x+1
	; Forcetype: INTEGER
	; Calling storevariable on generic assign expression
	sta dir_y
	sty dir_y+1
	; Forcetype: INTEGER
	; Integer constant assigning
	; Forcetype:  INTEGER
	ldy #$06
	lda #$2c
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; Forcetype: INTEGER
	; Integer constant assigning
	; Forcetype:  INTEGER
	ldy #$02
	lda #$80
	; Calling storevariable on generic assign expression
	sta sprite_y
	sty sprite_y+1
checkInput_elsedoneblock66
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$03
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock72
checkInput_ConditionalTrueBlock70: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda phyx_enable
	; cmp #$00 ignored
	beq checkInput_elseblock85
checkInput_ConditionalTrueBlock84: ;Main true block ;keep 
	
; // Toggle physics
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta phyx_enable
	; Poke
	; Optimization: shift is zero
	; Forcetype: NADA
	lda #$4
	sta $d021
	jmp checkInput_elsedoneblock86
checkInput_elseblock85
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta phyx_enable
	; Poke
	; Optimization: shift is zero
	; Forcetype: NADA
	lda #$0
	sta $d021
checkInput_elsedoneblock86
	
; // cheap debounce
	; Forcetype: NADA
	lda #$32
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
checkInput_elsedoneblock72
	rts
end_procedure_checkInput
	; NodeProcedureDecl -1
	; ***********  Defining procedure : updateState
	;    Procedure type : User-defined procedure
dir_x_tmp	dc.w	0
dir_y_tmp	dc.w	0
updateState_block91
updateState
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; Peek
	lda $d01f + $0;keep
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq updateState_localfailed126
	jmp updateState_ConditionalTrueBlock93
updateState_localfailed126
	jmp updateState_elsedoneblock95
updateState_ConditionalTrueBlock93: ;Main true block ;keep 
	
; // Debug
; //moveto(5,5,hi(screen_char_loc));
; //printdecimal(dir_x,4);
; //moveto(5,6,hi(screen_char_loc));
; //printdecimal(dir_y,4);
; // Background collision reverses direction 
; //  245 if(peek(v+31)and1) then poke 53280,(peek(53280)+1) and 15: y=y-dy*2: dy=-dy*.6
; //inc(^$D020); 
; // flash border
; // Reverse direction
	; HandleVarBinopB16bit
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy dir_y+1
	lda dir_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
updateState_rightvarInteger_var129 = $54
	sta updateState_rightvarInteger_var129
	sty updateState_rightvarInteger_var129+1
	lda dir_y+1
	sec
	sbc updateState_rightvarInteger_var129+1
	tay
	lda dir_y
	sec
	sbc updateState_rightvarInteger_var129
	bcs updateState_wordAdd128
	dey
updateState_wordAdd128
	; Calling storevariable on generic assign expression
	sta dir_y
	sty dir_y+1
	
; // Workaround to set negative value	
	lda sprite_y
	clc
	adc dir_y
	sta sprite_y+0
	lda sprite_y+1
	adc dir_y+1
	sta sprite_y+1
	; Binary clause Simplified: NOTEQUALS
	clc
	lda phyx_enable
	; cmp #$00 ignored
	beq updateState_elsedoneblock134
updateState_ConditionalTrueBlock132: ;Main true block ;keep 
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda dir_y+1   ; compare high bytes
	sbc #$00 
	bvc updateState_label1153
	eor #$80
updateState_label1153
	bmi updateState_ConditionalTrueBlock148
	bvc updateState_label2154
	eor #$80
updateState_label2154
	bne updateState_elsedoneblock150
	lda dir_y   ; compare high bytes
	sbc #$00 
	bcs updateState_elsedoneblock150
updateState_ConditionalTrueBlock148: ;Main true block ;keep 
	
; // Reduce velocity on impact	
	lda dir_y
	clc
	adc #$09
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd156
	inc dir_y+1
updateState_WordAdd156
updateState_elsedoneblock150
updateState_elsedoneblock134
updateState_elsedoneblock95
	; Binary clause Simplified: NOTEQUALS
	clc
	lda phyx_enable
	; cmp #$00 ignored
	beq updateState_elsedoneblock160
updateState_ConditionalTrueBlock158: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	; abs(x) integer
	ldy dir_x+1 ;keep
	lda dir_x
	cpy #127
	bcc updateState_abslabel218
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel218
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc updateState_elsedoneblock202
updateState_ConditionalTrueBlock200: ;Main true block ;keep 
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda #$00   ; compare high bytes
	sbc dir_x+1 
	bvc updateState_label1228
	eor #$80
updateState_label1228
	bmi updateState_ConditionalTrueBlock221
	bvc updateState_label2229
	eor #$80
updateState_label2229
	bne updateState_elseblock222
	lda #$00   ; compare high bytes
	sbc dir_x 
	bcs updateState_elseblock222
updateState_ConditionalTrueBlock221: ;Main true block ;keep 
	
; // Update rate of change to simulate physics
; //  160 dx=dx*.95
; //  170 dy=dy+.3
; // Decay X-dir
	lda dir_x
	sec
	sbc #$01
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs updateState_WordAdd231
	dec dir_x+1
updateState_WordAdd231
	jmp updateState_elsedoneblock223
updateState_elseblock222
	lda dir_x
	clc
	adc #$01
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd233
	inc dir_x+1
updateState_WordAdd233
updateState_elsedoneblock223
updateState_elsedoneblock202
	
; // Decay Y-dir(always moves down)
	lda dir_y
	clc
	adc #$01
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd234
	inc dir_y+1
updateState_WordAdd234
updateState_elsedoneblock160
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda dir_x+1   ; compare high bytes
	sbc #$00 
	bvc updateState_label1259
	eor #$80
updateState_label1259
	bmi updateState_ConditionalTrueBlock236
	bvc updateState_label2260
	eor #$80
updateState_label2260
	bne updateState_elseblock237
	lda dir_x   ; compare high bytes
	sbc #$00 
	bcs updateState_elseblock237
updateState_ConditionalTrueBlock236: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_x+1 ;keep
	lda dir_x
	cpy #127
	bcc updateState_abslabel268
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel268
	; Compare with pure num / var optimization
	cmp max_x;keep
	bcc updateState_elsedoneblock265
	beq updateState_elsedoneblock265
updateState_ConditionalTrueBlock263: ;Main true block ;keep 
	
; // Check max rates
	lda dir_x
	clc
	adc inc_amt
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd270
	inc dir_x+1
updateState_WordAdd270
updateState_elsedoneblock265
	jmp updateState_elsedoneblock238
updateState_elseblock237
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_x+1 ;keep
	lda dir_x
	cpy #127
	bcc updateState_abslabel278
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel278
	; Compare with pure num / var optimization
	cmp max_x;keep
	bcc updateState_elsedoneblock275
	beq updateState_elsedoneblock275
updateState_ConditionalTrueBlock273: ;Main true block ;keep 
	lda dir_x
	sec
	sbc inc_amt
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs updateState_WordAdd280
	dec dir_x+1
updateState_WordAdd280
updateState_elsedoneblock275
updateState_elsedoneblock238
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda dir_y+1   ; compare high bytes
	sbc #$00 
	bvc updateState_label1305
	eor #$80
updateState_label1305
	bmi updateState_ConditionalTrueBlock282
	bvc updateState_label2306
	eor #$80
updateState_label2306
	bne updateState_elseblock283
	lda dir_y   ; compare high bytes
	sbc #$00 
	bcs updateState_elseblock283
updateState_ConditionalTrueBlock282: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_y+1 ;keep
	lda dir_y
	cpy #127
	bcc updateState_abslabel314
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel314
	; Compare with pure num / var optimization
	cmp max_y;keep
	bcc updateState_elsedoneblock311
	beq updateState_elsedoneblock311
updateState_ConditionalTrueBlock309: ;Main true block ;keep 
	lda dir_y
	clc
	adc inc_amt
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd316
	inc dir_y+1
updateState_WordAdd316
updateState_elsedoneblock311
	jmp updateState_elsedoneblock284
updateState_elseblock283
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_y+1 ;keep
	lda dir_y
	cpy #127
	bcc updateState_abslabel324
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel324
	; Compare with pure num / var optimization
	cmp max_y;keep
	bcc updateState_elsedoneblock321
	beq updateState_elsedoneblock321
updateState_ConditionalTrueBlock319: ;Main true block ;keep 
	lda dir_y
	sec
	sbc inc_amt
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs updateState_WordAdd326
	dec dir_y+1
updateState_WordAdd326
updateState_elsedoneblock321
updateState_elsedoneblock284
	
; // Update position
; //  180 x=x+dx: y=y+dy
	lda sprite_x
	clc
	adc dir_x
	sta sprite_x+0
	lda sprite_x+1
	adc dir_x+1
	sta sprite_x+1
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda border_lt+1   ; compare high bytes
	sbc sprite_x+1 
	bvc updateState_label1336
	eor #$80
updateState_label1336
	bmi updateState_localsuccess335
	bvc updateState_label2337
	eor #$80
updateState_label2337
	bne updateState_localfailed334
	lda border_lt   ; compare high bytes
	sbc sprite_x 
	bcs updateState_localfailed334
updateState_localsuccess335: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	bne updateState_localfailed334
	jmp updateState_ConditionalTrueBlock329
updateState_localfailed334: ;keep
	; ; logical OR, second chance
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_x+1   ; compare high bytes
	sbc border_rt+1 
	bvc updateState_label1339
	eor #$80
updateState_label1339
	bmi updateState_localsuccess338
	bvc updateState_label2340
	eor #$80
updateState_label2340
	bne updateState_elsedoneblock331
	lda sprite_x   ; compare high bytes
	sbc border_rt 
	bcs updateState_elsedoneblock331
updateState_localsuccess338: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda sprite_x_hi
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne updateState_elsedoneblock331
updateState_ConditionalTrueBlock329: ;Main true block ;keep 
	
; // Only move up/down while sprite is onscreen
	lda sprite_y
	clc
	adc dir_y
	sta sprite_y+0
	lda sprite_y+1
	adc dir_y+1
	sta sprite_y+1
updateState_elsedoneblock331
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_x+1   ; compare high bytes
	sbc border_lt+1 
	bvc updateState_label1349
	eor #$80
updateState_label1349
	bmi updateState_localsuccess348
	bvc updateState_label2350
	eor #$80
updateState_label2350
	bne updateState_elsedoneblock346
	lda sprite_x   ; compare high bytes
	sbc border_lt 
	bcs updateState_elsedoneblock346
updateState_localsuccess348: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	bne updateState_elsedoneblock346
updateState_ConditionalTrueBlock344: ;Main true block ;keep 
	
; // Check X-boundaries.  
; // Going offscreen left-right wraps around
; //
; //  190 if x<0 and r=0 then x=89: r=1
	ldy border_rt+1 ;keep
	lda border_rt
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta sprite_x_hi
updateState_elsedoneblock346
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda #$09   ; compare high bytes
	sbc sprite_x+1 
	bvc updateState_label1358
	eor #$80
updateState_label1358
	bmi updateState_localsuccess357
	bvc updateState_label2359
	eor #$80
updateState_label2359
	bne updateState_elsedoneblock355
	lda #$f6   ; compare high bytes
	sbc sprite_x 
	bcs updateState_elsedoneblock355
updateState_localsuccess357: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	bne updateState_elsedoneblock355
updateState_ConditionalTrueBlock353: ;Main true block ;keep 
	
; //  200 if x>255 and r=0 then x=0: r=1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta sprite_x_hi
updateState_elsedoneblock355
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_x+1   ; compare high bytes
	sbc #$00 
	bvc updateState_label1367
	eor #$80
updateState_label1367
	bmi updateState_localsuccess366
	bvc updateState_label2368
	eor #$80
updateState_label2368
	bne updateState_elsedoneblock364
	lda sprite_x   ; compare high bytes
	sbc #$00 
	bcs updateState_elsedoneblock364
updateState_localsuccess366: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	beq updateState_elsedoneblock364
updateState_ConditionalTrueBlock362: ;Main true block ;keep 
	
; //  210 if x<0 and r=1 then x=255: r=0
	; Forcetype: INTEGER
	; Integer constant assigning
	; Forcetype:  INTEGER
	ldy #$09
	lda #$f6
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta sprite_x_hi
updateState_elsedoneblock364
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda border_rt+1   ; compare high bytes
	sbc sprite_x+1 
	bvc updateState_label1376
	eor #$80
updateState_label1376
	bmi updateState_localsuccess375
	bvc updateState_label2377
	eor #$80
updateState_label2377
	bne updateState_elsedoneblock373
	lda border_rt   ; compare high bytes
	sbc sprite_x 
	bcs updateState_elsedoneblock373
updateState_localsuccess375: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	beq updateState_elsedoneblock373
updateState_ConditionalTrueBlock371: ;Main true block ;keep 
	
; //  220 if x>89 and r=1 then x=0: r=0
	ldy border_lt+1 ;keep
	lda border_lt
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta sprite_x_hi
updateState_elsedoneblock373
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_y+1   ; compare high bytes
	sbc border_tp+1 
	bvc updateState_label1385
	eor #$80
updateState_label1385
	bmi updateState_ConditionalTrueBlock380
	bvc updateState_label2386
	eor #$80
updateState_label2386
	bne updateState_elsedoneblock382
	lda sprite_y   ; compare high bytes
	sbc border_tp 
	bcs updateState_elsedoneblock382
updateState_ConditionalTrueBlock380: ;Main true block ;keep 
	
; // Check Y-boundaries.  
; // Hitting top/botton reverses direction
; //
; //  230 if y<1 then y=1: dy=0
	ldy border_tp+1 ;keep
	lda border_tp
	; Calling storevariable on generic assign expression
	sta sprite_y
	sty sprite_y+1
	; abs(x) integer
	ldy dir_y+1 ;keep
	lda dir_y
	cpy #127
	bcc updateState_abslabel388
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel388
	; Calling storevariable on generic assign expression
	sta dir_y
	sty dir_y+1
updateState_elsedoneblock382
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda border_bt+1   ; compare high bytes
	sbc sprite_y+1 
	bvc updateState_label1404
	eor #$80
updateState_label1404
	bmi updateState_ConditionalTrueBlock390
	bvc updateState_label2405
	eor #$80
updateState_label2405
	bne updateState_elsedoneblock392
	lda border_bt   ; compare high bytes
	sbc sprite_y 
	bcs updateState_elsedoneblock392
updateState_ConditionalTrueBlock390: ;Main true block ;keep 
	
; //  240 if y>208 then y=208: dy=-dy*.6
	ldy border_bt+1 ;keep
	lda border_bt
	; Calling storevariable on generic assign expression
	sta sprite_y
	sty sprite_y+1
	; HandleVarBinopB16bit
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy dir_y+1
	lda dir_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
updateState_rightvarInteger_var408 = $54
	sta updateState_rightvarInteger_var408
	sty updateState_rightvarInteger_var408+1
	lda dir_y+1
	sec
	sbc updateState_rightvarInteger_var408+1
	tay
	lda dir_y
	sec
	sbc updateState_rightvarInteger_var408
	bcs updateState_wordAdd407
	dey
updateState_wordAdd407
	; Calling storevariable on generic assign expression
	sta dir_y
	sty dir_y+1
	; Binary clause Simplified: NOTEQUALS
	clc
	lda phyx_enable
	; cmp #$00 ignored
	beq updateState_elsedoneblock412
updateState_ConditionalTrueBlock410: ;Main true block ;keep 
	
; // Workaround to set negative value	
; // Reduce velocity on impact
	lda dir_y
	clc
	adc #$06
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd416
	inc dir_y+1
updateState_WordAdd416
updateState_elsedoneblock412
updateState_elsedoneblock392
	rts
end_procedure_updateState
	; NodeProcedureDecl -1
	; ***********  Defining procedure : updateScreen
	;    Procedure type : User-defined procedure
updateScreen
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	beq updateScreen_elseblock420
updateScreen_ConditionalTrueBlock419: ;Main true block ;keep 
	
; // Sprite 0 hi-X bit
; //  250 if r=0 then poke v+16,0: goto 270
; //  260 poke v+16,1	
	; Toggle bit with constant
	; Forcetype: NADA
	lda $d010
	ora #%1
	sta $d010
	; Forcetype: NADA
	ldx #$0 ; optimized, look out for bugs
	lda #1
updateScreen_shiftbit431
	cpx #0
	beq updateScreen_shiftbitdone432
	asl
	dex
	jmp updateScreen_shiftbit431
updateScreen_shiftbitdone432
updateScreen_bitmask_var433 = $54
	sta updateScreen_bitmask_var433
	; Forcetype: NADA
	lda $d010
	ora updateScreen_bitmask_var433
	sta $d010
	jmp updateScreen_elsedoneblock421
updateScreen_elseblock420
	; Toggle bit with constant
	; Forcetype: NADA
	lda $d010
	and #%11111110
	sta $d010
	; Forcetype: NADA
	ldx #$0 ; optimized, look out for bugs
	lda #1
updateScreen_shiftbit435
	cpx #0
	beq updateScreen_shiftbitdone436
	asl
	dex
	jmp updateScreen_shiftbit435
updateScreen_shiftbitdone436
updateScreen_bitmask_var437 = $54
	sta updateScreen_bitmask_var437
	lda #$FF
	eor updateScreen_bitmask_var437
	sta updateScreen_bitmask_var437
	; Forcetype: NADA
	lda $d010
	and updateScreen_bitmask_var437
	sta $d010
updateScreen_elsedoneblock421
	
; // Sprite 0 position
; //  270 poke v,x
; //  280 poke v+1,y
; //SpritePos(sprite_x/10, sprite_y/10, 0);
	; Poke
	; Optimization: shift is zero
	; Right is PURE NUMERIC : Is word =1
	; 16x8 div
	ldy sprite_x+1 ;keep
	lda sprite_x
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	; Forcetype: NADA
	lda #$a
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	sta $d000
	; Poke
	; Optimization: shift is zero
	; Right is PURE NUMERIC : Is word =1
	; 16x8 div
	ldy sprite_y+1 ;keep
	lda sprite_y
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	; Forcetype: NADA
	lda #$a
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	ldy initdiv16x8_dividend+1
	sta $d001
	rts
end_procedure_updateScreen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : setup
	;    Procedure type : User-defined procedure
setup
	
; // Clear screen, set black background & border
; //   10 print "{clr}" poke 53281,0: poke 53280,0
	; Clear screen with offset
	; Forcetype: NADA
	lda #$20
	ldx #$fa
setup_clearloop439
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne setup_clearloop439
	
; // init screen ram
	; Clear screen with offset
	; Forcetype: NADA
	lda #$6
	ldx #$fa
setup_clearloop440
	dex
	sta $0000+$d800,x
	sta $00fa+$d800,x
	sta $01f4+$d800,x
	sta $02ee+$d800,x
	bne setup_clearloop440
	
; // init color ram
	; Assigning memory location
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d021
	; Assigning memory location
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta $d020
	
; // Draw lines on screen
; //   15 for a=0 to 15: poke 1024+a+40*15,160: next
; //   16 for a=25 to 39: poke 1024+a+40*15,160: next
	lda #88
	ldy #6
	sta loc_work
	sty loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
setup_forloop441
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (loc_work),y
setup_loopstart442
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$f
	cmp i ;keep
	bne setup_forloop441
setup_loopdone446: ;keep
setup_loopend443
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta i
setup_forloop447
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (loc_work),y
setup_loopstart448
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne setup_forloop447
setup_loopdone452: ;keep
setup_loopend449
	lda #192
	ldy #7
	sta loc_work
	sty loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
setup_forloop453
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (loc_work),y
setup_loopstart454
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne setup_forloop453
setup_loopdone458: ;keep
setup_loopend455
	
; // sprite data 
; //   30 poke 2040,13: for s=0 to 62: read sd: poke 832+s,sd: next
	; Poke
	; Optimization: shift is zero
	; Forcetype: NADA
	lda #$d
	sta $7f8
	lda #$40
	ldx #$03
	sta loc_work
	stx loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
setup_forloop459
	; Load Byte array
	; CAST type NADA
	ldx i
	lda sprite_data_arr,x 
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (loc_work),y
setup_loopstart460
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$3f
	cmp i ;keep
	bne setup_forloop459
setup_loopdone464: ;keep
setup_loopend461
	
; // enable sprite 0
; //   50 poke v+21,1
	; Assigning memory location
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta $d015
	
; // enable sprite 0
; // set sprite 0 color
; //   60 poke v+39,3
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta $D027+$0
	; Assigning memory location
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d01c
	
; // disable multicolor 
; // set sprite 0 X/Y expansion
; //   70 poke v+23,1: poke v+29,1
	; Assigning memory location
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta $d01d
	
; // expand sprites
	; Assigning memory location
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta $d017
	rts
end_procedure_setup
block1
main_block_begin_
	
; // Execution starts here
screenmemory =  $fe
colormemory =  $fb
	jsr setup
	
; //do_delay(10);
; //  290 goto 100			
MainProgram_while465
MainProgram_loopstart469
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock468
MainProgram_ConditionalTrueBlock466: ;Main true block ;keep 
MainProgram_verticalblank1475
	bit $D011
	bpl MainProgram_verticalblank1475
MainProgram_verticalblank2476
	bit $D011
	bmi MainProgram_verticalblank2476
	jsr checkInput
	jsr updateState
	jsr updateScreen
	jmp MainProgram_while465
MainProgram_elsedoneblock468
MainProgram_loopend470
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

