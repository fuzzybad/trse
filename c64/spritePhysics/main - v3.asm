
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
sprite_x	dc.w	$9e
sprite_y	dc.w	$40
dir_x	dc.w	$00
dir_y	dc.w	$00
max_x	dc.b	$08
max_y	dc.b	$06
sprite_x_hi	dc.b	$00
border_rt	dc.b	$48
border_lt	dc.b	$00
border_tp	dc.b	$32
border_bt	dc.b	$d0
sprite_data_arr	dc.b $0, $7f, $0, $1, $ff, $c0, $3, $ff
	dc.b $e0, $3, $e7, $e0, $7, $d9, $f0, $7
	dc.b $df, $f0, $7, $d9, $f0, $3, $e7, $e0
	dc.b $3, $ff, $e0, $3, $ff, $e0, $2, $ff
	dc.b $a0, $1, $7f, $40, $1, $3e, $40, $0
	dc.b $9c, $80, $0, $9c, $80, $0, $49, $0
	dc.b $0, $49, $0, $0, $3e, $0, $0, $3e
	dc.b $0, $0, $3e, $0, $0, $1c, $0
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
	ldx #$00
	ldy #$80
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock15
checkInput_ConditionalTrueBlock13: ;Main true block ;keep 
	
; // down	
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$01
	ldy #$80
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_localfailed39
	jmp checkInput_ConditionalTrueBlock32
checkInput_localfailed39: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$06
	ldy #$10
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elseblock33
checkInput_ConditionalTrueBlock32: ;Main true block ;keep 
	
; // Check cursor up/down
; //  110 if a$="{up}" then dy=dy-1
; //  120 if a$="{down}" then dy=dy+1
; // Check shift 
	lda dir_y
	sec
	sbc #$01
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs checkInput_WordAdd41
	dec dir_y+1
checkInput_WordAdd41
	jmp checkInput_elsedoneblock34
checkInput_elseblock33
	
; // up
	lda dir_y
	clc
	adc #$01
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc checkInput_WordAdd43
	inc dir_y+1
checkInput_WordAdd43
checkInput_elsedoneblock34
checkInput_elsedoneblock15
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$00
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock47
checkInput_ConditionalTrueBlock45: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$01
	ldy #$80
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_localfailed71
	jmp checkInput_ConditionalTrueBlock64
checkInput_localfailed71: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$06
	ldy #$10
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elseblock65
checkInput_ConditionalTrueBlock64: ;Main true block ;keep 
	
; // Check cursor left/right
; //  130 if a$="{left}" then dx=dx-1
; //  140 if a$="{rght}" then dx=dx+1
; // Check shift 
	lda dir_x
	sec
	sbc #$01
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs checkInput_WordAdd73
	dec dir_x+1
checkInput_WordAdd73
	jmp checkInput_elsedoneblock66
checkInput_elseblock65
	lda dir_x
	clc
	adc #$01
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc checkInput_WordAdd75
	inc dir_x+1
checkInput_WordAdd75
checkInput_elsedoneblock66
checkInput_elsedoneblock47
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$07
	ldy #$10
	jsr Key_Held
	; cmp #$00 ignored
	beq checkInput_elsedoneblock79
checkInput_ConditionalTrueBlock77: ;Main true block ;keep 
	
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
	lda #$a5
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; Forcetype: INTEGER
	lda #$40
	; Calling storevariable on generic assign expression
	sta sprite_y
	sty sprite_y+1
checkInput_elsedoneblock79
	rts
end_procedure_checkInput
	; NodeProcedureDecl -1
	; ***********  Defining procedure : updateState
	;    Procedure type : User-defined procedure
dir_x_tmp	dc.w	0
dir_y_tmp	dc.w	0
updateState_block82
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
	beq updateState_elsedoneblock86
updateState_ConditionalTrueBlock84: ;Main true block ;keep 
	
; // Debug
; //moveto(5,5,hi(screen_char_loc));
; //printdecimal(dir_x,4);
; //moveto(5,6,hi(screen_char_loc));
; //printdecimal(dir_y,4);
; // Background collision reverses direction 
; //  245 if(peek(v+31)and1) then poke 53280,(peek(53280)+1) and 15: y=y-dy*2: dy=-dy*.6
	; Assigning memory location
	; Test Inc dec D
	inc $d020
	
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
updateState_rightvarInteger_var93 = $54
	sta updateState_rightvarInteger_var93
	sty updateState_rightvarInteger_var93+1
	lda dir_y+1
	sec
	sbc updateState_rightvarInteger_var93+1
	tay
	lda dir_y
	sec
	sbc updateState_rightvarInteger_var93
	bcs updateState_wordAdd92
	dey
updateState_wordAdd92
	; Calling storevariable on generic assign expression
	sta dir_y
	sty dir_y+1
	
; // Workaround method to set dir_y to negative value	
	lda sprite_y
	clc
	adc dir_y
	sta sprite_y+0
	lda sprite_y+1
	adc dir_y+1
	sta sprite_y+1
updateState_elsedoneblock86
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda dir_x+1   ; compare high bytes
	sbc #$00 
	bvc updateState_label1119
	eor #$80
updateState_label1119
	bmi updateState_ConditionalTrueBlock96
	bvc updateState_label2120
	eor #$80
updateState_label2120
	bne updateState_elseblock97
	lda dir_x   ; compare high bytes
	sbc #$00 
	bcs updateState_elseblock97
updateState_ConditionalTrueBlock96: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_x+1 ;keep
	lda dir_x
	cpy #127
	bcc updateState_abslabel128
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel128
	; Compare with pure num / var optimization
	cmp max_x;keep
	bcc updateState_elsedoneblock125
	beq updateState_elsedoneblock125
updateState_ConditionalTrueBlock123: ;Main true block ;keep 
	
; // Update rate of change
; //  160 dx=dx*.95
; //  170 dy=dy+.3
; // Check max rates
	lda dir_x
	clc
	adc #$01
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd130
	inc dir_x+1
updateState_WordAdd130
updateState_elsedoneblock125
	jmp updateState_elsedoneblock98
updateState_elseblock97
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_x+1 ;keep
	lda dir_x
	cpy #127
	bcc updateState_abslabel138
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel138
	; Compare with pure num / var optimization
	cmp max_x;keep
	bcc updateState_elsedoneblock135
	beq updateState_elsedoneblock135
updateState_ConditionalTrueBlock133: ;Main true block ;keep 
	lda dir_x
	sec
	sbc #$01
	sta dir_x+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs updateState_WordAdd140
	dec dir_x+1
updateState_WordAdd140
updateState_elsedoneblock135
updateState_elsedoneblock98
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda dir_y+1   ; compare high bytes
	sbc #$00 
	bvc updateState_label1165
	eor #$80
updateState_label1165
	bmi updateState_ConditionalTrueBlock142
	bvc updateState_label2166
	eor #$80
updateState_label2166
	bne updateState_elseblock143
	lda dir_y   ; compare high bytes
	sbc #$00 
	bcs updateState_elseblock143
updateState_ConditionalTrueBlock142: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_y+1 ;keep
	lda dir_y
	cpy #127
	bcc updateState_abslabel174
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel174
	; Compare with pure num / var optimization
	cmp max_y;keep
	bcc updateState_elsedoneblock171
	beq updateState_elsedoneblock171
updateState_ConditionalTrueBlock169: ;Main true block ;keep 
	lda dir_y
	clc
	adc #$01
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc updateState_WordAdd176
	inc dir_y+1
updateState_WordAdd176
updateState_elsedoneblock171
	jmp updateState_elsedoneblock144
updateState_elseblock143
	; Binary clause Simplified: GREATER
	; abs(x) integer
	ldy dir_y+1 ;keep
	lda dir_y
	cpy #127
	bcc updateState_abslabel184
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel184
	; Compare with pure num / var optimization
	cmp max_y;keep
	bcc updateState_elsedoneblock181
	beq updateState_elsedoneblock181
updateState_ConditionalTrueBlock179: ;Main true block ;keep 
	lda dir_y
	sec
	sbc #$01
	sta dir_y+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs updateState_WordAdd186
	dec dir_y+1
updateState_WordAdd186
updateState_elsedoneblock181
updateState_elsedoneblock144
	
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
	lda #0   ; compare high bytes
	sbc sprite_x+1 
	bvc updateState_label1196
	eor #$80
updateState_label1196
	bmi updateState_localsuccess195
	bvc updateState_label2197
	eor #$80
updateState_label2197
	bne updateState_localfailed194
	lda border_lt   ; compare high bytes
	sbc sprite_x 
	bcs updateState_localfailed194
updateState_localsuccess195: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	bne updateState_localfailed194
	jmp updateState_ConditionalTrueBlock189
updateState_localfailed194: ;keep
	; ; logical OR, second chance
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_x+1   ; compare high bytes
	sbc #0 
	bvc updateState_label1199
	eor #$80
updateState_label1199
	bmi updateState_localsuccess198
	bvc updateState_label2200
	eor #$80
updateState_label2200
	bne updateState_elsedoneblock191
	lda sprite_x   ; compare high bytes
	sbc border_rt 
	bcs updateState_elsedoneblock191
updateState_localsuccess198: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda sprite_x_hi
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne updateState_elsedoneblock191
updateState_ConditionalTrueBlock189: ;Main true block ;keep 
	
; // Only move up/down while sprite is onscreen
	lda sprite_y
	clc
	adc dir_y
	sta sprite_y+0
	lda sprite_y+1
	adc dir_y+1
	sta sprite_y+1
updateState_elsedoneblock191
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_x+1   ; compare high bytes
	sbc #0 
	bvc updateState_label1209
	eor #$80
updateState_label1209
	bmi updateState_localsuccess208
	bvc updateState_label2210
	eor #$80
updateState_label2210
	bne updateState_elsedoneblock206
	lda sprite_x   ; compare high bytes
	sbc border_lt 
	bcs updateState_elsedoneblock206
updateState_localsuccess208: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	bne updateState_elsedoneblock206
updateState_ConditionalTrueBlock204: ;Main true block ;keep 
	
; // Check X-boundaries.  
; // Going offscreen left-right wraps around
; //
; //  190 if x<0 and r=0 then x=89: r=1
	ldy #0 ; Fake 16 bit
	lda border_rt
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta sprite_x
	sty sprite_x+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta sprite_x_hi
updateState_elsedoneblock206
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda #$00   ; compare high bytes
	sbc sprite_x+1 
	bvc updateState_label1218
	eor #$80
updateState_label1218
	bmi updateState_localsuccess217
	bvc updateState_label2219
	eor #$80
updateState_label2219
	bne updateState_elsedoneblock215
	lda #$ff   ; compare high bytes
	sbc sprite_x 
	bcs updateState_elsedoneblock215
updateState_localsuccess217: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	bne updateState_elsedoneblock215
updateState_ConditionalTrueBlock213: ;Main true block ;keep 
	
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
updateState_elsedoneblock215
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_x+1   ; compare high bytes
	sbc #$00 
	bvc updateState_label1227
	eor #$80
updateState_label1227
	bmi updateState_localsuccess226
	bvc updateState_label2228
	eor #$80
updateState_label2228
	bne updateState_elsedoneblock224
	lda sprite_x   ; compare high bytes
	sbc #$00 
	bcs updateState_elsedoneblock224
updateState_localsuccess226: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	beq updateState_elsedoneblock224
updateState_ConditionalTrueBlock222: ;Main true block ;keep 
	
; //  210 if x<0 and r=1 then x=255: r=0
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$ff
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta sprite_x_hi
updateState_elsedoneblock224
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda #0   ; compare high bytes
	sbc sprite_x+1 
	bvc updateState_label1236
	eor #$80
updateState_label1236
	bmi updateState_localsuccess235
	bvc updateState_label2237
	eor #$80
updateState_label2237
	bne updateState_elsedoneblock233
	lda border_rt   ; compare high bytes
	sbc sprite_x 
	bcs updateState_elsedoneblock233
updateState_localsuccess235: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sprite_x_hi
	; cmp #$00 ignored
	beq updateState_elsedoneblock233
updateState_ConditionalTrueBlock231: ;Main true block ;keep 
	
; //  220 if x>89 and r=1 then x=0: r=0
	ldy #0 ; Fake 16 bit
	lda border_lt
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta sprite_x
	sty sprite_x+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta sprite_x_hi
updateState_elsedoneblock233
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda sprite_y+1   ; compare high bytes
	sbc #0 
	bvc updateState_label1245
	eor #$80
updateState_label1245
	bmi updateState_ConditionalTrueBlock240
	bvc updateState_label2246
	eor #$80
updateState_label2246
	bne updateState_elsedoneblock242
	lda sprite_y   ; compare high bytes
	sbc border_tp 
	bcs updateState_elsedoneblock242
updateState_ConditionalTrueBlock240: ;Main true block ;keep 
	
; // Check Y-boundaries.  
; // Hitting top/botton reverses direction
; //
; //  230 if y<1 then y=1: dy=0
	ldy #0 ; Fake 16 bit
	lda border_tp
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta sprite_y
	sty sprite_y+1
	; abs(x) integer
	ldy dir_y+1 ;keep
	lda dir_y
	cpy #127
	bcc updateState_abslabel248
	pha
	tya
	eor #$ff
	tay
	pla
	eor #$ff
	clc
	adc #$01
updateState_abslabel248
	; Calling storevariable on generic assign expression
	sta dir_y
	sty dir_y+1
updateState_elsedoneblock242
	
; // Workaround method to set dir_y to negative value	
	; Binary clause INTEGER: LESS
	; Signed compare
	sec
	lda #0   ; compare high bytes
	sbc sprite_y+1 
	bvc updateState_label1256
	eor #$80
updateState_label1256
	bmi updateState_ConditionalTrueBlock250
	bvc updateState_label2257
	eor #$80
updateState_label2257
	bne updateState_elsedoneblock252
	lda border_bt   ; compare high bytes
	sbc sprite_y 
	bcs updateState_elsedoneblock252
updateState_ConditionalTrueBlock250: ;Main true block ;keep 
	
; //  240 if y>208 then y=208: dy=-dy*.6
	ldy #0 ; Fake 16 bit
	lda border_bt
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
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
updateState_rightvarInteger_var260 = $54
	sta updateState_rightvarInteger_var260
	sty updateState_rightvarInteger_var260+1
	lda dir_y+1
	sec
	sbc updateState_rightvarInteger_var260+1
	tay
	lda dir_y
	sec
	sbc updateState_rightvarInteger_var260
	bcs updateState_wordAdd259
	dey
updateState_wordAdd259
	; Calling storevariable on generic assign expression
	sta dir_y
	sty dir_y+1
updateState_elsedoneblock252
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
	beq updateScreen_elseblock264
updateScreen_ConditionalTrueBlock263: ;Main true block ;keep 
	
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
updateScreen_shiftbit275
	cpx #0
	beq updateScreen_shiftbitdone276
	asl
	dex
	jmp updateScreen_shiftbit275
updateScreen_shiftbitdone276
updateScreen_bitmask_var277 = $54
	sta updateScreen_bitmask_var277
	; Forcetype: NADA
	lda $d010
	ora updateScreen_bitmask_var277
	sta $d010
	jmp updateScreen_elsedoneblock265
updateScreen_elseblock264
	; Toggle bit with constant
	; Forcetype: NADA
	lda $d010
	and #%11111110
	sta $d010
	; Forcetype: NADA
	ldx #$0 ; optimized, look out for bugs
	lda #1
updateScreen_shiftbit279
	cpx #0
	beq updateScreen_shiftbitdone280
	asl
	dex
	jmp updateScreen_shiftbit279
updateScreen_shiftbitdone280
updateScreen_bitmask_var281 = $54
	sta updateScreen_bitmask_var281
	lda #$FF
	eor updateScreen_bitmask_var281
	sta updateScreen_bitmask_var281
	; Forcetype: NADA
	lda $d010
	and updateScreen_bitmask_var281
	sta $d010
updateScreen_elsedoneblock265
	
; // Sprite 0 position
; //  270 poke v,x
; //  280 poke v+1,y
	; Poke
	; Optimization: shift is zero
	ldy sprite_x+1 ;keep
	lda sprite_x
	sta $d000
	; Poke
	; Optimization: shift is zero
	ldy sprite_y+1 ;keep
	lda sprite_y
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
setup_clearloop283
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne setup_clearloop283
	
; // init screen ram
	; Clear screen with offset
	; Forcetype: NADA
	lda #$6
	ldx #$fa
setup_clearloop284
	dex
	sta $0000+$d800,x
	sta $00fa+$d800,x
	sta $01f4+$d800,x
	sta $02ee+$d800,x
	bne setup_clearloop284
	
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
	
; //   15 for a=0 to 15: poke 1024+a+40*15,160: next
	lda #88
	ldy #6
	sta loc_work
	sty loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
setup_forloop285
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (loc_work),y
setup_loopstart286
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$f
	cmp i ;keep
	bne setup_forloop285
setup_loopdone290: ;keep
setup_loopend287
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta i
setup_forloop291
	
; //   16 for a=25 to 39: poke 1024+a+40*15,160: next
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (loc_work),y
setup_loopstart292
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne setup_forloop291
setup_loopdone296: ;keep
setup_loopend293
	
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
setup_forloop297
	; Load Byte array
	; CAST type NADA
	ldx i
	lda sprite_data_arr,x 
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (loc_work),y
setup_loopstart298
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$3f
	cmp i ;keep
	bne setup_forloop297
setup_loopdone302: ;keep
setup_loopend299
	
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
MainProgram_while303
MainProgram_loopstart307
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock306
MainProgram_ConditionalTrueBlock304: ;Main true block ;keep 
MainProgram_verticalblank1313
	bit $D011
	bpl MainProgram_verticalblank1313
MainProgram_verticalblank2314
	bit $D011
	bmi MainProgram_verticalblank2314
	jsr checkInput
	jsr updateState
	jsr updateScreen
	jmp MainProgram_while303
MainProgram_elsedoneblock306
MainProgram_loopend308
main_block_end_
	; End of program
	; Ending memory block at $810
EndBlock810:

