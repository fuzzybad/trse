
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
LightCycles
	jmp block1
Interrupts_interruptFlag = $e813
Interrupts_org_irq	= $68
Interrupts_isInitialised	dc.b	$00
Interrupts_irq_address = $90
Model_Flags	dc.b	0
Model_IRQFlag = $91
Model_CheckAvailMem = $c353
Key_keys	dc.b	 
	org Key_keys+10
Key_keysLast	dc.b	 
	org Key_keysLast+10
Key_keysPressed	dc.b	 
	org Key_keysPressed+10
Key_keyState	dc.b	0
Key_temp	dc.b	0
Key_KeyRow = $e810
Key_KeyRead = $e812
MYSCREEN_WIDTH	dc.b	$28
DEF_GAME_SPEED	dc.b	$32
DEF_ANIM_SPEED	dc.b	$06
DEF_SCROLL_SPEED	dc.b	$10
screen_loc	= $6A
screen_loc_work	= $6C
message_ptr	= $6E
song_ptr	= $70
i	dc.b	$00
j	dc.b	$00
k	dc.b	$00
tmp	dc.w	$00
tmp_irq	dc.b	$00
message_len	dc.b	$00
temp_byte	dc.b	$00
game_mode	dc.b	$00
game_difficulty	dc.b	$02
game_mode_changed	dc.b	$01
game_over_flag	dc.b	$00
game_run_flag	dc.b	$00
keybd_type_flag	dc.b	$00
score_p1	dc.b	$00
score_p2	dc.b	$00
scroll_speed	dc.b	$10
game_speed	dc.b	$32
anim_speed	dc.b	$06
music_idx	dc.b	$00
music_sust_idx	dc.b	$00
sound_pitch	dc.b	$ff
sound_oct_idx	dc.b	$00
sound_oct_arr	dc.b $f, $33, $55
crash_anim_arr	dc.b $2a, $57, $51
dir_map_arr	dc.w $0, $ffff, $1, $ffd8, $28
dir_map_arr80	dc.w $0, $ffff, $1, $ffb0, $50
dir_opp_arr	dc.b $0, $2, $1, $4, $3
turn_counter	dc.b	$00
player_1_input	dc.b	$00
player_2_input	dc.b	$00
keybd_input	dc.b	$00
player_1_head	dc.b	$00
player_2_head	dc.b	$00
player_1_fire	dc.b	$00
player_2_fire	dc.b	$00
player_1_crash	dc.b	$00
player_2_crash	dc.b	$00
player_1_xy	dc.w	$00
player_2_xy	dc.w	$00
player_1_trail	dc.w	 
	org player_1_trail+4
player_2_trail	dc.w	 
	org player_2_trail+4
scroll_num_char	dc.b	$00
scroll_x	dc.b	$00
scroll_y	dc.b	$00
scroll_num_trail	dc.b	$00
scroll_inp_brk	dc.b	$00
scroll_step	dc.b	$00
scroll_input	dc.b	$00
sark_near_player_x_dist	dc.b	$00
sark_near_player_x_neg	dc.b	$00
sark_near_player_y_dist	dc.b	$00
sark_near_player_y_neg	dc.b	$00
title_msg_1		dc.b	17
	dc.b	17
	dc.b	213
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	195
	dc.b	201
	dc.b	0
title_msg_2		dc.b	194
	dc.b	" LIGHT CYCLE DUEL "
	dc.b	221
	dc.b	0
title_msg_3		dc.b	202
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	198
	dc.b	203
	dc.b	0
title_msg_4		dc.b	17
	dc.b	"2023 FUZZYBAD"
	dc.b	0
title_msg_5		dc.b	17
	dc.b	"FIRST PLAYER TO SCORE FIVE WINS"
	dc.b	0
title_msg_6		dc.b	"TURNS DISABLED DURING BOOST"
	dc.b	0
title_msg_7		dc.b	17
	dc.b	"PLAYER 1       PLAYER 2"
	dc.b	0
title_msg_8		dc.b	218
	dc.b	218
	dc.b	"             "
	dc.b	209
	dc.b	209
	dc.b	0
title_msg_9		dc.b	17
	dc.b	"USE DUAL JOYSTICK ADAPTER"
	dc.b	0
title_msg_10		dc.b	"OR             OR"
	dc.b	0
title_msg_11		dc.b	"WASD+SPC       8456+RSH"
	dc.b	0
title_msg_12		dc.b	17
	dc.b	17
	dc.b	17
	dc.b	17
	dc.b	17
	dc.b	18
	dc.b	"PRESS FIRE TO BEGIN"
	dc.b	0
score_msg_0		dc.b	"PLAYER 1:"
	dc.b	0
score_msg_1		dc.b	":PLAYER 2"
	dc.b	0
score_msg_2		dc.b	"  PLAYER:"
	dc.b	0
score_msg_3		dc.b	":SARK"
	dc.b	0
msg_both_crash		dc.b	"BOTH CRASHED, REDO"
	dc.b	0
msg_p1_crash		dc.b	"PLAYER 1 CRASHED"
	dc.b	0
msg_p2_crash		dc.b	"PLAYER 2 CRASHED"
	dc.b	0
msg_plr_crash		dc.b	"PLAYER CRASHED"
	dc.b	0
msg_sark_crash		dc.b	"SARK CRASHED"
	dc.b	0
msg_p1_wins		dc.b	"PLAYER 1 WINS"
	dc.b	0
msg_p2_wins		dc.b	"PLAYER 2 WINS"
	dc.b	0
msg_plr_wins		dc.b	"PLAYER WINS"
	dc.b	0
msg_sark_wins		dc.b	"SARK WINS"
	dc.b	0
msg_get_ready		dc.b	"PRESS FIRE WHEN READY"
	dc.b	0
msg_one_player		dc.b	">>> "
	dc.b	18
	dc.b	"ONE PLAYER"
	dc.b	146
	dc.b	" >>>"
	dc.b	0
msg_two_player		dc.b	"<<< "
	dc.b	18
	dc.b	"TWO PLAYER"
	dc.b	146
	dc.b	" <<<"
	dc.b	0
msg_diff_select		dc.b	"DIFFICULTY:"
	dc.b	0
msg_diff_one		dc.b	18
	dc.b	"1"
	dc.b	146
	dc.b	" 2 3: EASY  "
	dc.b	0
msg_diff_two		dc.b	"1 "
	dc.b	18
	dc.b	"2"
	dc.b	146
	dc.b	" 3: MEDIUM"
	dc.b	0
msg_diff_three		dc.b	"1 2 "
	dc.b	18
	dc.b	"3"
	dc.b	146
	dc.b	": HARD  "
	dc.b	0
theme_music_arr	dc.b $0, $0, $7, $0, $0, $2, $ee, $1
	dc.b $1, $d2, $1, $1, $bc, $1, $3, $0
	dc.b $0, $1, $ee, $1, $1, $d2, $1, $1
	dc.b $bc, $1, $2, $ee, $1, $2, $9e, $1
	dc.b $4, $d2, $1, $4, $fb, $1, $4, $9e
	dc.b $0, $3, $0, $0, $2, $8c, $0, $1
	dc.b $fb, $1, $1, $ee, $1, $3, $0, $0
	dc.b $1, $8c, $0, $1, $fb, $1, $1, $ee
	dc.b $1, $2, $8c, $0, $2, $bc, $1, $4
	dc.b $fb, $1, $4, $95, $0, $4, $bc, $0
	dc.b $3, $0, $0, $2, $8c, $0, $1, $9e
	dc.b $0, $1, $b1, $0, $2, $8c, $0, $2
	dc.b $fb, $1, $6, $0, $0, $2, $ee, $1
	dc.b $1, $fb, $1, $1, $8c, $0, $2, $ee
	dc.b $1, $2, $d2, $1, $6, $0, $0, $2
	dc.b $c7, $1, $1, $e0, $1, $1, $fb, $1
	dc.b $2, $c7, $1, $2, $a8, $1, $4, $e0
	dc.b $1, $3, $0, $0, $2, $a8, $1, $1
	dc.b $bc, $1, $1, $d2, $1, $2, $a8, $1
	dc.b $2, $8c, $1, $6, $0, $0, $7, $0
	dc.b $0, $0
game_end_music_arr	dc.b $0, $0, $7, $d2, $0, $2, $9e, $0
	dc.b $2, $ee, $0, $1, $e0, $0, $2, $95
	dc.b $0, $2, $0, $0, $1, $fb, $1, $1
	dc.b $8c, $0, $1, $fb, $1, $1, $bc, $1
	dc.b $4, $0, $0, $e, $0, $0, $0
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
end_procedure_init16x8div
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
end_procedure_init16x8mul
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
end_procedure_init8x8div
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
end_procedure_initeightbitmul
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
end_procedure_initmoveto
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
end_procedure_initprintdecimal
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
end_procedure_initprintstring
	;*
; //<p>Disables interrupts. This prevents a number of things from running 
; //including the kernal's keyboard scan and the jiffy clock TI$. 
; //As these things are not running every frame you will see some speed
; //benefits in your main code.
; //<p>The Key unit is not affected as it polls the keyboard directly. 
; 

	;*
; //<p>Enables interrupts.
; 

	;*
; //<p>Starts an IRQ	
; 

	;*
; //<p>Ends an IRQ
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : Interrupts_RasterIRQ
	;    Procedure type : User-defined procedure
Interrupts_addr	dc.w	0
Interrupts_RasterIRQ_block4
Interrupts_RasterIRQ
	; Binary clause Simplified: EQUALS
	clc
	lda Interrupts_isInitialised
	; cmp #$00 ignored
	bne Interrupts_RasterIRQ_elsedoneblock8
Interrupts_RasterIRQ_ConditionalTrueBlock6: ;Main true block ;keep 
	; ****** Inline assembler section
		lda $90
		sta Interrupts_org_irq
		lda $91
		sta Interrupts_org_irq+1
	 	
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta Interrupts_isInitialised
Interrupts_RasterIRQ_elsedoneblock8
	; ****** Inline assembler section
	sei	
	ldy Interrupts_addr+1 ;keep
	lda Interrupts_addr
	; Calling storevariable on generic assign expression
	sta Interrupts_irq_address
	sty Interrupts_irq_address+1
	; ****** Inline assembler section
	cli	
	rts
end_procedure_Interrupts_RasterIRQ
	;*
; //<p>Run tests to detect what model of Pet the program is running on.</p>
; //<p>Model::Flags will be set and can be tested with Model::ISROM4 and Model::IS40COL.</p>
; //<p>Alternatively, Is_ROM4(); and Is_40Col(); procedures will return TRUE or FALSE.</p>
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : Model_Detect
	;    Procedure type : User-defined procedure
Model_char	dc.b	0
Model_test_string		dc.b	147
	dc.b	17
	dc.b	43
	dc.b	0
Model_Detect_block11
Model_Detect
	
; // Tests by Chris Garrett
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta Model_Flags
	; Binary clause Simplified: EQUALS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda Model_IRQFlag
	; Forcetype: NADA
	and #$f0
	 ; end add / sub var with constant
	; Compare with pure num / var optimization
	cmp #$e0;keep
	bne Model_Detect_localfailed17
	jmp Model_Detect_ConditionalTrueBlock13
Model_Detect_localfailed17: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda Model_CheckAvailMem
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne Model_Detect_elsedoneblock15
Model_Detect_ConditionalTrueBlock13: ;Main true block ;keep 
	; 8 bit binop
	; Add/sub where right value is constant number
	lda Model_Flags
	; Forcetype: NADA
	ora #$80
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta Model_Flags
Model_Detect_elsedoneblock15
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta Model_char
Model_Detect_forloop19
	; Assigning to register
	; Assigning register : _a
	; Load Unknown type array, assuming BYTE
	; CAST type NADA
	ldx Model_char
	lda Model_test_string,x 
	; Forcetype: NADA
	jsr $ffd2
Model_Detect_loopstart20
	; Compare is onpage
	; Test Inc dec D
	inc Model_char
	; Forcetype: NADA
	lda #$3
	cmp Model_char ;keep
	bne Model_Detect_forloop19
Model_Detect_loopdone24: ;keep
Model_Detect_loopend21
	; Binary clause Simplified: EQUALS
	; Forcetype: NADA
	lda $8050
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne Model_Detect_elsedoneblock28
Model_Detect_ConditionalTrueBlock26: ;Main true block ;keep 
	; 8 bit binop
	; Add/sub where right value is constant number
	lda Model_Flags
	; Forcetype: NADA
	ora #$40
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta Model_Flags
Model_Detect_elsedoneblock28
	lda Model_Flags
	rts
	rts
end_procedure_Model_Detect
	;*
; //<p>Returns TRUE if 40 column display mode, otherwise 
; //false if 80 column display mode.</p>
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : Model_Is_40Col
	;    Procedure type : User-defined procedure
Model_Is_40Col
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda Model_Flags
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq Model_Is_40Col_elsedoneblock35
Model_Is_40Col_ConditionalTrueBlock33: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	rts
Model_Is_40Col_elsedoneblock35
	; Forcetype: NADA
	lda #$0
	rts
	rts
end_procedure_Model_Is_40Col
	;*
; //<p>Returns TRUE if BASIC ROM version 4 is present, otherwise 
; //false unknown.</p>
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : Model_Is_ROM4
	;    Procedure type : User-defined procedure
Model_Is_ROM4
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda Model_Flags
	; Forcetype: NADA
	and #$80
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq Model_Is_ROM4_elsedoneblock42
Model_Is_ROM4_ConditionalTrueBlock40: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	rts
Model_Is_ROM4_elsedoneblock42
	; Forcetype: NADA
	lda #$0
	rts
	rts
end_procedure_Model_Is_ROM4
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key__GetKeyOffset
	;    Procedure type : User-defined procedure
Key__GetKeyOffset_block45
Key__GetKeyOffset
	; ****** Inline assembler section
	
	; pass with _a containing key to check 1, 2, 4, 8, 16, 32, 64, 128
	; and y the row offset 0 - 9
	tax ; temp store key
	
	tya ; multiply row offset by 8 to find starting pos in array
	asl
	asl
	asl ; *8
	sta TEMP_VAR1
	
	; use temporary variable as a pointer
	lda #>Key_screencodes_graphic
	sta TEMP_VAR2+1 ; high byte
	lda #<Key_screencodes_graphic
	sta TEMP_VAR2 ; low byte
	
	clc
	adc TEMP_VAR1
	sta TEMP_VAR2
	bcc getKey_NoOverflow
	inc TEMP_VAR2+1 ; byte overflow, update high byte
	
getKey_NoOverflow
	; then work out the offest 0 - 7
	txa ; the key
	
	ldy #0  ; count the number of shifts
	ldx #8	; test all 8 bits
getKey_testLoop
	lsr ; bit put in carry
	bcs getKey_foundBit
	iny	; counter for next bit to test
	dex	; 
	bne getKey_testLoop
	
getKey_foundBit
	; x has the bit 0-7 or 8 if not found
	cpy #8
	beq getKey_NotFound
	
	lda (TEMP_VAR2),y  ; load screencode + bit offset
	rts 
	
getKey_NotFound
	lda #KEY_NOKEY ; not found
	
	rts
end_procedure_Key__GetKeyOffset
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key_Read
	;    Procedure type : User-defined procedure
Key_Read
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta Key_keyState
	
; // row to test
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta Key_temp
Key_Read_forloop47
	lda Key_temp
	; Calling storevariable on generic assign expression
	sta Key_KeyRow
	; ****** Inline assembler section
		lda Key_KeyRead ; get bits
		eor #$ff ; invert so the below works
		ldx Key_temp
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
		
		
Key_Read_loopstart48
	; Compare is onpage
	; Test Inc dec D
	inc Key_temp
	; Forcetype: NADA
	lda #$a
	cmp Key_temp ;keep
	bne Key_Read_forloop47
Key_Read_loopdone52: ;keep
Key_Read_loopend49
	rts
end_procedure_Key_Read
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key_Held
	;    Procedure type : User-defined procedure
Key_Held_block53
Key_Held
	; ****** Inline assembler section
	
	sty Key_temp
	lda Key_keys,x
	and Key_temp 	; contains column 
	
	
	rts
end_procedure_Key_Held
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key_Pressed
	;    Procedure type : User-defined procedure
Key_Pressed_block54
Key_Pressed
	; ****** Inline assembler section
	
	sty Key_temp
	lda Key_keysPressed,x
	and Key_temp 	; contains column 
	
	rts
end_procedure_Key_Pressed
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key_GetHeld
	;    Procedure type : User-defined procedure
Key_screencodes_graphic	
	dc.b	$21, $23, $25, $26, 40, 255, 255, 255, 34
	dc.b	$24, $27, $1c, 41, 255, 255, 254, $11, $05
	dc.b	$14, $15, $0f, 70, $37, $39, $17, $12, $19
	dc.b	$09, $10, 255, $38, $2f, $01, $04, $07, $0a
	dc.b	$0c, 255, $34, $36, $13, $06, $08, $0b, $3a
	dc.b	255, $35, $2a, $1a, $03, $02, $0d, $3b, 255
	dc.b	$31, $33, $18, $16, $0e, 44, $3f, 255, $32
	dc.b	$2b, 255, $00, $1d, 255, $3e, 255, $30, $2d
	dc.b	255, $1b, $20, $3c, 255, 255, $2e, $3d 
Key_GetHeld_block55
Key_GetHeld
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta Key_temp
Key_GetHeld_forloop56
	
; // Look up tables to convert key row / columns to screencodes
; // note: only text, numbers, punctuation and ins/del key are listed
; //       everything else is nokey so not detected in this table
	; Assigning to register
	; Assigning register : _a
	; Load Byte array
	; CAST type NADA
	ldx Key_temp
	lda Key_keysPressed,x 
	; Assigning to register
	; Assigning register : _y
	ldy Key_temp
	jsr Key__GetKeyOffset
	; Calling storevariable on generic assign expression
	sta $5e
	; Binary clause Simplified: NOTEQUALS
	; Forcetype: NADA
	; Compare with pure num / var optimization
	cmp #$ff;keep
	beq Key_GetHeld_elsedoneblock70
Key_GetHeld_ConditionalTrueBlock68: ;Main true block ;keep 
	; Forcetype: NADA
	lda $5e
	rts
Key_GetHeld_elsedoneblock70
Key_GetHeld_loopstart57
	; Compare is onpage
	; Test Inc dec D
	inc Key_temp
	; Forcetype: NADA
	lda #$a
	cmp Key_temp ;keep
	bne Key_GetHeld_forloop56
Key_GetHeld_loopdone73: ;keep
Key_GetHeld_loopend58
	
; // no key held!
	; Forcetype: NADA
	lda #$ff
	rts
	rts
end_procedure_Key_GetHeld
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_model
	;    Procedure type : User-defined procedure
check_model
	jsr Model_Detect
	
; // Business keyboard
	; Binary clause Simplified: EQUALS
	clc
	jsr Model_Is_40Col
	; cmp #$00 ignored
	bne check_model_elsedoneblock78
check_model_ConditionalTrueBlock76: ;Main true block ;keep 
	
; // Model::Is_ROM4() values:
; // 8096, 8032, 4032, 3032: 1
; // 2001: 0
; // Reset constants for 80-col PET
; // 
; // 	Model::Is_40Col() values:
; // 		8096, 8032: 0
; // 		4032, 3032, 2001: 1
	; Forcetype: NADA
	lda #$50
	; Calling storevariable on generic assign expression
	sta MYSCREEN_WIDTH
	
; //		DEF_GAME_SPEED := 25;
; //		DEF_ANIM_SPEED := 6;
; //		DEF_SCROLL_SPEED := 8;
	lda DEF_GAME_SPEED
	; Calling storevariable on generic assign expression
	sta game_speed
	lda DEF_ANIM_SPEED
	; Calling storevariable on generic assign expression
	sta anim_speed
	lda DEF_SCROLL_SPEED
	; Calling storevariable on generic assign expression
	sta scroll_speed
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta keybd_type_flag
check_model_elsedoneblock78
	rts
end_procedure_check_model
	
; // Clear Screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cls
	;    Procedure type : User-defined procedure
cls
	; Clear screen with offset
	; Forcetype: NADA
	lda #$20
	ldx #$fa
cls_clearloop82
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne cls_clearloop82
	; MoveTo optimization
	lda #$00
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	rts
end_procedure_cls
	
; // Set lower/uppercase mode
	; NodeProcedureDecl -1
	; ***********  Defining procedure : set_lowercase
	;    Procedure type : User-defined procedure
set_lowercase
	; Poke
	; Optimization: shift is zero
	; Forcetype: NADA
	lda #$e
	sta $E84C
	rts
end_procedure_set_lowercase
	
; // Set uppercase/graphics mode
	; NodeProcedureDecl -1
	; ***********  Defining procedure : set_uppercase
	;    Procedure type : User-defined procedure
set_uppercase
	; Poke
	; Optimization: shift is zero
	; Forcetype: NADA
	lda #$c
	sta $E84C
	rts
end_procedure_set_uppercase
	
; // Set graphic mode
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cursor_graphic
	;    Procedure type : User-defined procedure
cursor_graphic
	; Assigning to register
	; Assigning register : _a
	; Forcetype: NADA
	lda #$8e
	jsr $FFD2
	rts
end_procedure_cursor_graphic
	
; // Set text mode
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cursor_text
	;    Procedure type : User-defined procedure
cursor_text
	; Assigning to register
	; Assigning register : _a
	; Forcetype: NADA
	lda #$e
	jsr $FFD2
	rts
end_procedure_cursor_text
	
; // Home cursor
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cursor_home
	;    Procedure type : User-defined procedure
cursor_home
	; Assigning to register
	; Assigning register : _a
	; Forcetype: NADA
	lda #$13
	jsr $FFD2
	rts
end_procedure_cursor_home
	
; // Clear screen & home cursor
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cursor_clear
	;    Procedure type : User-defined procedure
cursor_clear
	; Assigning to register
	; Assigning register : _a
	; Forcetype: NADA
	lda #$93
	jsr $FFD2
	rts
end_procedure_cursor_clear
	
; // Return/Line Feed
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cursor_return
	;    Procedure type : User-defined procedure
cursor_return
	; Assigning to register
	; Assigning register : _a
	; Forcetype: NADA
	lda #$d
	jsr $FFD2
	rts
end_procedure_cursor_return
	
; // Move cursor down
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cursor_down
	;    Procedure type : User-defined procedure
cursor_down
	; Assigning to register
	; Assigning register : _a
	; Forcetype: NADA
	lda #$11
	jsr $FFD2
	rts
end_procedure_cursor_down
	
; // Move cursor right
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cursor_right
	;    Procedure type : User-defined procedure
cursor_right
	; Assigning to register
	; Assigning register : _a
	; Forcetype: NADA
	lda #$1d
	jsr $FFD2
	rts
end_procedure_cursor_right
	
; // Print using KERNAL routine
	; NodeProcedureDecl -1
	; ***********  Defining procedure : basic_print
	;    Procedure type : User-defined procedure
next_ch	dc.b	0
bp_i	dc.b	0
_ptr	= $72
_center_txt	dc.b	0
_mylen	dc.b	0
basic_print_block92
basic_print
	; Binary clause Simplified: NOTEQUALS
	clc
	lda _center_txt
	; cmp #$00 ignored
	beq basic_print_elsedoneblock96
basic_print_ConditionalTrueBlock94: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda _mylen
	; Compare with pure num / var optimization
	cmp MYSCREEN_WIDTH;keep
	bcs basic_print_elsedoneblock120
basic_print_ConditionalTrueBlock118: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta bp_i
basic_print_forloop129
	
; // Center text
	jsr cursor_right
basic_print_loopstart130
	; Compare is onpage
	; Test Inc dec D
	inc bp_i
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; 8 bit binop
	; Add/sub where right value is constant number
	lda MYSCREEN_WIDTH
	sec
	sbc _mylen
	 ; end add / sub var with constant
	lsr
	cmp bp_i ;keep
	bne basic_print_forloop129
basic_print_loopdone134: ;keep
basic_print_loopend131
basic_print_elsedoneblock120
basic_print_elsedoneblock96
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta next_ch
basic_print_while135
basic_print_loopstart139
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	; cmp #$00 ignored
	beq basic_print_elsedoneblock138
basic_print_ConditionalTrueBlock136: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc next_ch
	jmp basic_print_while135
basic_print_elsedoneblock138
basic_print_loopend140
	jsr cursor_return
	rts
end_procedure_basic_print
	
; // Print using KERNAL routine at X,Y location
	; NodeProcedureDecl -1
	; ***********  Defining procedure : basic_printat
	;    Procedure type : User-defined procedure
pa_next_ch	dc.b	0
pa_i	dc.b	0
_pa_ptr	= $72
_pa_myx	dc.b	0
_pa_myy	dc.b	0
basic_printat_block143
basic_printat
	
; // Home cursor
	jsr cursor_home
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myx
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock147
basic_printat_ConditionalTrueBlock145: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop156
	
; // Cursor right
	jsr cursor_right
basic_printat_loopstart157
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myx
	cmp pa_i ;keep
	bne basic_printat_forloop156
basic_printat_loopdone161: ;keep
basic_printat_loopend158
basic_printat_elsedoneblock147
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myy
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock165
basic_printat_ConditionalTrueBlock163: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop174
	
; // Cursor down
	jsr cursor_down
basic_printat_loopstart175
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myy
	cmp pa_i ;keep
	bne basic_printat_forloop174
basic_printat_loopdone179: ;keep
basic_printat_loopend176
basic_printat_elsedoneblock165
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_next_ch
basic_printat_while180
basic_printat_loopstart184
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	; cmp #$00 ignored
	beq basic_printat_elsedoneblock183
basic_printat_ConditionalTrueBlock181: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc pa_next_ch
	jmp basic_printat_while180
basic_printat_elsedoneblock183
basic_printat_loopend185
	rts
end_procedure_basic_printat
	
; // Delay function
	; NodeProcedureDecl -1
	; ***********  Defining procedure : do_delay
	;    Procedure type : User-defined procedure
dd_i	dc.b	0
delay_val	dc.b	0
do_delay_block188
do_delay
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda delay_val
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc do_delay_elsedoneblock192
do_delay_ConditionalTrueBlock190: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dd_i
do_delay_forloop201
	; Wait
	; Forcetype: NADA
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
do_delay_loopstart202
	; Compare is onpage
	; Test Inc dec D
	inc dd_i
	lda delay_val
	cmp dd_i ;keep
	bne do_delay_forloop201
do_delay_loopdone206: ;keep
do_delay_loopend203
do_delay_elsedoneblock192
	rts
end_procedure_do_delay
	
; // Play music - called from external loop
	; NodeProcedureDecl -1
	; ***********  Defining procedure : play_music
	;    Procedure type : User-defined procedure
play_music
	; Binary clause Simplified: EQUALS
	clc
	lda music_sust_idx
	; cmp #$00 ignored
	bne play_music_elsedoneblock211
play_music_ConditionalTrueBlock209: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	; Load pointer array
	ldy music_idx
	lda (song_ptr),y
	; cmp #$00 ignored
	bne play_music_elsedoneblock224
play_music_localsuccess226: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	; Load pointer array
	; 8 bit binop
	; Add/sub where right value is constant number
	lda music_idx
	clc
	; Forcetype: NADA
	adc #$2
	 ; end add / sub var with constant
	tay
	lda (song_ptr),y
	; cmp #$00 ignored
	bne play_music_elsedoneblock224
play_music_ConditionalTrueBlock222: ;Main true block ;keep 
	
; // Loop over note array
; // 	Structure - note value, octave, sustain value 
; // Change note when index is zero
; // End tune when pitch & sustain both = 0
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta music_idx
play_music_elsedoneblock224
	
; // Set octave
	; Load Byte array
	; CAST type NADA
	; Load pointer array
	; 8 bit binop
	; Add/sub where right value is constant number
	lda music_idx
	clc
	; Forcetype: NADA
	adc #$1
	 ; end add / sub var with constant
	tay
	lda (song_ptr),y
	tax
	lda sound_oct_arr,x 
	; Calling storevariable on generic assign expression
	sta $e84a
	
; // Set the pitch
	; Load pointer array
	ldy music_idx
	lda (song_ptr),y
	; Calling storevariable on generic assign expression
	sta $e848
	
; // Set sustain - this value is tuned based on routine calling this function
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load pointer array
	; 8 bit binop
	; Add/sub where right value is constant number
	lda music_idx
	clc
	; Forcetype: NADA
	adc #$2
	 ; end add / sub var with constant
	tay
	lda (song_ptr),y
	asl
	asl
	asl
	; Calling storevariable on generic assign expression
	sta music_sust_idx
	
; // Next note
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda music_idx
	clc
	; Forcetype: NADA
	adc #$3
	sta music_idx
play_music_elsedoneblock211
	; Test Inc dec D
	dec music_sust_idx
	rts
end_procedure_play_music
	
; // Control function for text cycling
; // 	xpos
; //	ypos
; //	num_char
; //	num_cycles - set to zero for infinite cycles
; //	scroll direction - 0:left, 1:right
; //	break on input
; //	play music
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cycle_text
	;    Procedure type : User-defined procedure
cts_xpos	dc.b	0
cts_ypos	dc.b	0
cts_num_char	dc.b	0
cts_num_cycles	dc.b	0
cts_dir	dc.b	0
cts_input_brk	dc.b	0
cts_play_music	dc.b	0
cycle_text_block228
cycle_text
	
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(cts_input_brk,2);
; // Controls when trailing chars at end are drawn
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta temp_byte
	
; // Number of trailing chars
	lda cts_num_char
	; Calling storevariable on generic assign expression
	sta scroll_num_char
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Forcetype:  BYTE
	lsr
	; Calling storevariable on generic assign expression
	sta scroll_num_trail
	
; // Screen position to start effect
	lda cts_xpos
	; Calling storevariable on generic assign expression
	sta scroll_x
	lda cts_ypos
	; Calling storevariable on generic assign expression
	sta scroll_y
	
; // Break scroll on input
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	; Binary clause Simplified: EQUALS
	clc
	lda cts_num_cycles
	; cmp #$00 ignored
	bne cycle_text_elseblock231
cycle_text_ConditionalTrueBlock230: ;Main true block ;keep 
	
; // Value of zero means we cycle forever
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scroll_step
	jmp cycle_text_elsedoneblock232
cycle_text_elseblock231
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_step
cycle_text_elsedoneblock232
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock240
cycle_text_ConditionalTrueBlock238: ;Main true block ;keep 
	
; // Initialize music	
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta music_idx
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta music_sust_idx
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta $e84b
cycle_text_elsedoneblock240
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta k
cycle_text_forloop243
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_dir
	; cmp #$00 ignored
	beq cycle_text_elseblock264
cycle_text_ConditionalTrueBlock263: ;Main true block ;keep 
	
; // Scroll for the given # cycles
	jsr cycle_text_left
	jmp cycle_text_elsedoneblock265
cycle_text_elseblock264
	jsr cycle_text_right
cycle_text_elsedoneblock265
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_inp_brk
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock273
cycle_text_ConditionalTrueBlock271: ;Main true block ;keep 
	
; // Break on input
	rts
cycle_text_elsedoneblock273
cycle_text_loopstart244
	; Compare is onpage
	; 8 bit binop
	; Add/sub where right value is constant number
	lda k
	clc
	adc scroll_step
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta k
	lda cts_num_cycles
	cmp k ;keep
	bne cycle_text_forloop243
cycle_text_loopdone276: ;keep
cycle_text_loopend245
	rts
end_procedure_cycle_text
	
; // Cycle text right 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cycle_text_right
	;    Procedure type : User-defined procedure
cycle_text_right
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
cycle_text_right_forloop278
	; Generic 16 bit op
	ldy #0
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda scroll_y
	sta mul16x8_num2
	jsr mul16x8_procedure
cycle_text_right_rightvarInteger_var340 = $54
	sta cycle_text_right_rightvarInteger_var340
	sty cycle_text_right_rightvarInteger_var340+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_right_skip342
	iny
cycle_text_right_skip342
	; Low bit binop:
	clc
	adc cycle_text_right_rightvarInteger_var340
cycle_text_right_wordAdd338
	sta cycle_text_right_rightvarInteger_var340
	; High-bit binop
	tya
	adc cycle_text_right_rightvarInteger_var340+1
	tay
	lda cycle_text_right_rightvarInteger_var340
	sta screen_loc_work
	sty screen_loc_work+1
	
; // Leading char
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy i
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp scroll_num_trail;keep
	bcs cycle_text_right_elseblock345
cycle_text_right_ConditionalTrueBlock344: ;Main true block ;keep 
	
; // Trailing char
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scroll_num_char
	sec
	sbc scroll_num_trail
	 ; end add / sub var with constant
	clc
	adc i
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta j
	jmp cycle_text_right_elsedoneblock346
cycle_text_right_elseblock345
	; 8 bit binop
	; Add/sub where right value is constant number
	lda i
	sec
	sbc scroll_num_trail
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta j
	
; // Activate trailing char after index reaches a given point
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta temp_byte
cycle_text_right_elsedoneblock346
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock354
cycle_text_right_ConditionalTrueBlock352: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_elsedoneblock354
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock360
cycle_text_right_ConditionalTrueBlock358: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock378
cycle_text_right_ConditionalTrueBlock376: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_right_elsedoneblock378
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock384
cycle_text_right_ConditionalTrueBlock382: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_right_elsedoneblock384
cycle_text_right_elsedoneblock360
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock390
cycle_text_right_ConditionalTrueBlock388: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_right_elsedoneblock390
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_right_loopstart279
	; Test Inc dec D
	inc i
	lda scroll_num_char
	cmp i ;keep
	beq cycle_text_right_loopdone393
cycle_text_right_loopnotdone394
	jmp cycle_text_right_forloop278
cycle_text_right_loopdone393
cycle_text_right_loopend280
	rts
end_procedure_cycle_text_right
	
; // Cycle text left
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cycle_text_left
	;    Procedure type : User-defined procedure
cycle_text_left
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scroll_num_char
	sec
	; Forcetype: NADA
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta i
cycle_text_left_forloop396
	
; // Cycle characters within string
	; Generic 16 bit op
	ldy #0
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda scroll_y
	sta mul16x8_num2
	jsr mul16x8_procedure
cycle_text_left_rightvarInteger_var458 = $54
	sta cycle_text_left_rightvarInteger_var458
	sty cycle_text_left_rightvarInteger_var458+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_left_skip460
	iny
cycle_text_left_skip460
	; Low bit binop:
	clc
	adc cycle_text_left_rightvarInteger_var458
cycle_text_left_wordAdd456
	sta cycle_text_left_rightvarInteger_var458
	; High-bit binop
	tya
	adc cycle_text_left_rightvarInteger_var458+1
	tay
	lda cycle_text_left_rightvarInteger_var458
	sta screen_loc_work
	sty screen_loc_work+1
	
; // Leading char
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy i
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
	; Binary clause Simplified: LESS
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scroll_num_char
	sec
	sbc scroll_num_trail
	 ; end add / sub var with constant
	; Compare with pure num / var optimization
	cmp i;keep
	bcs cycle_text_left_elseblock463
cycle_text_left_ConditionalTrueBlock462: ;Main true block ;keep 
	
; // Trailing char
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda i
	sec
	sbc scroll_num_trail
	 ; end add / sub var with constant
	sec
	; Forcetype: NADA
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta j
	jmp cycle_text_left_elsedoneblock464
cycle_text_left_elseblock463
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scroll_num_char
	sec
	sbc scroll_num_trail
	 ; end add / sub var with constant
	clc
	adc i
	 ; end add / sub var with constant
	sec
	; Forcetype: NADA
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta j
	
; // Activate trailing char after index reaches a given point
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta temp_byte
cycle_text_left_elsedoneblock464
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock472
cycle_text_left_ConditionalTrueBlock470: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_left_elsedoneblock472
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock478
cycle_text_left_ConditionalTrueBlock476: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_left_elsedoneblock478
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock484
cycle_text_left_ConditionalTrueBlock482: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock502
cycle_text_left_ConditionalTrueBlock500: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_left_elsedoneblock502
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock508
cycle_text_left_ConditionalTrueBlock506: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_left_elsedoneblock508
cycle_text_left_elsedoneblock484
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_left_loopstart397
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda i
	clc
	; Forcetype: NADA
	adc #$ff
	sta i
	; Forcetype: NADA
	lda #$ff
	cmp i ;keep
	beq cycle_text_left_loopdone511
cycle_text_left_loopnotdone512
	jmp cycle_text_left_forloop396
cycle_text_left_loopdone511
cycle_text_left_loopend398
	rts
end_procedure_cycle_text_left
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_input
	;    Procedure type : User-defined procedure
check_input_val	dc.b	0
check_input_block513
check_input
	
; // Initialize	
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_1_input
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_input
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_1_fire
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_fire
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta keybd_input
	
; // Read controller values from Port B
	; Forcetype: NADA
	lda $e84f
	; Calling storevariable on generic assign expression
	sta check_input_val
	; Binary clause Simplified: EQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; Forcetype: NADA
	and #$3
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bne check_input_elsedoneblock517
check_input_ConditionalTrueBlock515: ;Main true block ;keep 
	
; // Check SPT Single Joystick
; //if (not check_input_val & 1) then player_1_input := 1;	
; // Left
; //if (not check_input_val & 2) then player_1_input := 2;	
; // Right
; //if (not check_input_val & 4) then player_1_input := 3;	
; // Up
; //if (not check_input_val & 8) then player_1_input := 4;	
; // Down
; //if (not check_input_val & 32) then player_1_fire := 1;	
; // Fire
; // Check SPT Double Joysticks
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock517
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$4
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock523
check_input_ConditionalTrueBlock521: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock523
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$8
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock529
check_input_ConditionalTrueBlock527: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock529
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock535
check_input_ConditionalTrueBlock533: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock535
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$2
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock541
check_input_ConditionalTrueBlock539: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock541
	; Binary clause Simplified: EQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$30
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bne check_input_elsedoneblock547
check_input_ConditionalTrueBlock545: ;Main true block ;keep 
	
; // Down
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock547
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock553
check_input_ConditionalTrueBlock551: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock553
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$80
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock559
check_input_ConditionalTrueBlock557: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock559
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$10
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock565
check_input_ConditionalTrueBlock563: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock565
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	; Forcetype: NADA
	and #$20
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock571
check_input_ConditionalTrueBlock569: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock571
	
; // Down
; // uncomment function in support_functions to debug keyboard codes
; //check_keyboard_codes();
; // Read PET Keyboard
	jsr Key_Read
	
; // K Key (graphics)
; //if Key::Held($0401) then keybd_input := 5;	
; // Left Arrow Key - Abort game
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda keybd_type_flag
	; Forcetype: NADA
	eor #$ff
	 ; end add / sub var with constant
	and #1
	; cmp #$00 ignored
	beq check_input_localfailed760
	jmp check_input_ConditionalTrueBlock575
check_input_localfailed760
	jmp check_input_elseblock576
check_input_ConditionalTrueBlock575: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$04
	ldy #$01
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock765
check_input_ConditionalTrueBlock763: ;Main true block ;keep 
	
; // Values for "graphics" keyboard
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock765
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$04
	ldy #$40
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock771
check_input_ConditionalTrueBlock769: ;Main true block ;keep 
	
; // A - P1 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock771
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$04
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock777
check_input_ConditionalTrueBlock775: ;Main true block ;keep 
	
; // 4 - P2 Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock777
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$04
	ldy #$80
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock783
check_input_ConditionalTrueBlock781: ;Main true block ;keep 
	
; // D - P1 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock783
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$03
	ldy #$01
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock789
check_input_ConditionalTrueBlock787: ;Main true block ;keep 
	
; // 6 - P2 Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock789
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$03
	ldy #$40
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock795
check_input_ConditionalTrueBlock793: ;Main true block ;keep 
	
; // W - P1 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock795
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$05
	ldy #$01
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock801
check_input_ConditionalTrueBlock799: ;Main true block ;keep 
	
; // 8 - P2 Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock801
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$05
	ldy #$40
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock807
check_input_ConditionalTrueBlock805: ;Main true block ;keep 
	
; // S - P1 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock807
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$09
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock813
check_input_ConditionalTrueBlock811: ;Main true block ;keep 
	
; // 5 - P2 Down
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock813
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$08
	ldy #$20
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock819
check_input_ConditionalTrueBlock817: ;Main true block ;keep 
	
; // Space - P1 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock819
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$06
	ldy #$40
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock825
check_input_ConditionalTrueBlock823: ;Main true block ;keep 
	
; // RShift - P2 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock825
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$07
	ldy #$40
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock831
check_input_ConditionalTrueBlock829: ;Main true block ;keep 
	
; // 1 Key
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock831
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$06
	ldy #$80
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock837
check_input_ConditionalTrueBlock835: ;Main true block ;keep 
	
; // 2 Key
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock837
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$02
	ldy #$20
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock843
check_input_ConditionalTrueBlock841: ;Main true block ;keep 
	
; // 3 Key
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock843
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$05
	ldy #$08
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock849
check_input_ConditionalTrueBlock847: ;Main true block ;keep 
	
; // K Key (business)
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock849
	jmp check_input_elsedoneblock577
check_input_elseblock576
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$03
	ldy #$01
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock856
check_input_ConditionalTrueBlock854: ;Main true block ;keep 
	
; // K Key (graphics)
; //if Key::Held($2000) then keybd_input := 5;	
; // Left Arrow Key - Abort game
; // Values for "business" keyboard
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock856
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$01
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock862
check_input_ConditionalTrueBlock860: ;Main true block ;keep 
	
; // A - P1 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock862
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$03
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock868
check_input_ConditionalTrueBlock866: ;Main true block ;keep 
	
; // 4 - P2 Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock868
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$09
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock874
check_input_ConditionalTrueBlock872: ;Main true block ;keep 
	
; // D - P1 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock874
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$04
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock880
check_input_ConditionalTrueBlock878: ;Main true block ;keep 
	
; // 6 - P2 Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock880
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$00
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock886
check_input_ConditionalTrueBlock884: ;Main true block ;keep 
	
; // W - P1 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock886
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$02
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock892
check_input_ConditionalTrueBlock890: ;Main true block ;keep 
	
; // 8 - P2 Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock892
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$00
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock898
check_input_ConditionalTrueBlock896: ;Main true block ;keep 
	
; // S - P1 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock898
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$08
	ldy #$04
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock904
check_input_ConditionalTrueBlock902: ;Main true block ;keep 
	
; // 5 - P2 Down
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock904
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$06
	ldy #$40
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock910
check_input_ConditionalTrueBlock908: ;Main true block ;keep 
	
; // Space - P1 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock910
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$01
	ldy #$01
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock916
check_input_ConditionalTrueBlock914: ;Main true block ;keep 
	
; // RShift - P2 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock916
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$00
	ldy #$01
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock922
check_input_ConditionalTrueBlock920: ;Main true block ;keep 
	
; // 1 Key -
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock922
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$09
	ldy #$02
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock928
check_input_ConditionalTrueBlock926: ;Main true block ;keep 
	
; // 2 Key
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock928
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$02
	ldy #$20
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock934
check_input_ConditionalTrueBlock932: ;Main true block ;keep 
	
; // 3 Key
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock934
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$05
	ldy #$08
	jsr Key_Held
	; cmp #$00 ignored
	beq check_input_elsedoneblock940
check_input_ConditionalTrueBlock938: ;Main true block ;keep 
	
; // K Key (business)
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta keybd_input
check_input_elsedoneblock940
check_input_elsedoneblock577
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_input_elsedoneblock946
check_input_localsuccess948: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	clc
	lda game_run_flag
	; cmp #$00 ignored
	beq check_input_elsedoneblock946
check_input_ConditionalTrueBlock944: ;Main true block ;keep 
	
; // Get Sark's move when in single-player mode
	jsr sark_move
check_input_elsedoneblock946
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_localfailed956
	jmp check_input_ConditionalTrueBlock951
check_input_localfailed956: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elseblock952
check_input_ConditionalTrueBlock951: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	rts
	jmp check_input_elsedoneblock953
check_input_elseblock952
	; Forcetype: NADA
	lda #$0
	rts
check_input_elsedoneblock953
	rts
end_procedure_check_input
	
; // Displays game mode
	; NodeProcedureDecl -1
	; ***********  Defining procedure : display_game_mode
	;    Procedure type : User-defined procedure
display_game_mode
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc #$12
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	sbc #$00
	tay 
	pla 
display_game_mode_int_shift_var961 = $54
	sta display_game_mode_int_shift_var961
	sty display_game_mode_int_shift_var961+1
		lsr display_game_mode_int_shift_var961+1
	ror display_game_mode_int_shift_var961+0

	lda display_game_mode_int_shift_var961
	ldy display_game_mode_int_shift_var961+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne display_game_mode_elsedoneblock965
display_game_mode_ConditionalTrueBlock963: ;Main true block ;keep 
	lda #<msg_one_player
	ldx #>msg_one_player
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
display_game_mode_elsedoneblock965
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne display_game_mode_elsedoneblock971
display_game_mode_ConditionalTrueBlock969: ;Main true block ;keep 
	lda #<msg_two_player
	ldx #>msg_two_player
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
display_game_mode_elsedoneblock971
	rts
end_procedure_display_game_mode
	
; //if game_mode = 2 then basic_printat(#msg_tvs_player,tmp,20);
; // Displays game mode
	; NodeProcedureDecl -1
	; ***********  Defining procedure : display_game_difficulty
	;    Procedure type : User-defined procedure
display_game_difficulty
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc #$17
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	sbc #$00
	tay 
	pla 
display_game_difficulty_int_shift_var976 = $54
	sta display_game_difficulty_int_shift_var976
	sty display_game_difficulty_int_shift_var976+1
		lsr display_game_difficulty_int_shift_var976+1
	ror display_game_difficulty_int_shift_var976+0

	lda display_game_difficulty_int_shift_var976
	ldy display_game_difficulty_int_shift_var976+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	lda #<msg_diff_select
	ldx #>msg_diff_select
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
	; Binary clause Simplified: EQUALS
	lda game_difficulty
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne display_game_difficulty_elsedoneblock980
display_game_difficulty_ConditionalTrueBlock978: ;Main true block ;keep 
	lda #<msg_diff_one
	ldx #>msg_diff_one
	sta _pa_ptr
	stx _pa_ptr+1
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	ldy tmp+1
	lda tmp
	clc
	; Forcetype: NADA
	adc #$c
	sta _pa_myx
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
display_game_difficulty_elsedoneblock980
	; Binary clause Simplified: EQUALS
	lda game_difficulty
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne display_game_difficulty_elsedoneblock986
display_game_difficulty_ConditionalTrueBlock984: ;Main true block ;keep 
	lda #<msg_diff_two
	ldx #>msg_diff_two
	sta _pa_ptr
	stx _pa_ptr+1
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	ldy tmp+1
	lda tmp
	clc
	; Forcetype: NADA
	adc #$c
	sta _pa_myx
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
display_game_difficulty_elsedoneblock986
	; Binary clause Simplified: EQUALS
	lda game_difficulty
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne display_game_difficulty_elsedoneblock992
display_game_difficulty_ConditionalTrueBlock990: ;Main true block ;keep 
	lda #<msg_diff_three
	ldx #>msg_diff_three
	sta _pa_ptr
	stx _pa_ptr+1
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	ldy tmp+1
	lda tmp
	clc
	; Forcetype: NADA
	adc #$c
	sta _pa_myx
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
display_game_difficulty_elsedoneblock992
	rts
end_procedure_display_game_difficulty
	
; // Handle additional input during cycle text routine
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cycle_text_input_handler
	;    Procedure type : User-defined procedure
cycle_text_input_handler
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_localfailed1002
	jmp cycle_text_input_handler_localsuccess1001
cycle_text_input_handler_localfailed1002: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_elsedoneblock999
cycle_text_input_handler_localsuccess1001: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_elsedoneblock999
cycle_text_input_handler_ConditionalTrueBlock997: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // one player
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_mode_changed
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock999
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_localfailed1010
	jmp cycle_text_input_handler_localsuccess1009
cycle_text_input_handler_localfailed1010: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_elsedoneblock1007
cycle_text_input_handler_localsuccess1009: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne cycle_text_input_handler_elsedoneblock1007
cycle_text_input_handler_ConditionalTrueBlock1005: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // two player
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta game_mode_changed
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock1007
	; Binary clause Simplified: NOTEQUALS
	clc
	lda keybd_input
	; cmp #$00 ignored
	beq cycle_text_input_handler_elsedoneblock1015
cycle_text_input_handler_localsuccess1025: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	lda keybd_input
	; Compare with pure num / var optimization
	cmp game_difficulty;keep
	beq cycle_text_input_handler_elsedoneblock1015
cycle_text_input_handler_ConditionalTrueBlock1013: ;Main true block ;keep 
	
; // one player mode difficulty selection
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_mode_changed
	; Binary clause Simplified: EQUALS
	lda keybd_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_localfailed1033
	jmp cycle_text_input_handler_ConditionalTrueBlock1028
cycle_text_input_handler_localfailed1033: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda keybd_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_localfailed1032
	jmp cycle_text_input_handler_ConditionalTrueBlock1028
cycle_text_input_handler_localfailed1032: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda keybd_input
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne cycle_text_input_handler_elsedoneblock1030
cycle_text_input_handler_ConditionalTrueBlock1028: ;Main true block ;keep 
	lda keybd_input
	; Calling storevariable on generic assign expression
	sta game_difficulty
cycle_text_input_handler_elsedoneblock1030
cycle_text_input_handler_elsedoneblock1015
	; Binary clause Simplified: NOTEQUALS
	clc
	lda game_mode_changed
	; cmp #$00 ignored
	beq cycle_text_input_handler_elsedoneblock1038
cycle_text_input_handler_ConditionalTrueBlock1036: ;Main true block ;keep 
	
; // update display for difficulty selection
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_mode_changed
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne cycle_text_input_handler_elseblock1059
cycle_text_input_handler_ConditionalTrueBlock1058: ;Main true block ;keep 
	jsr display_game_difficulty
	jmp cycle_text_input_handler_elsedoneblock1060
cycle_text_input_handler_elseblock1059
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc #$1e
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	sbc #$00
	tay 
	pla 
cycle_text_input_handler_int_shift_var1070 = $54
	sta cycle_text_input_handler_int_shift_var1070
	sty cycle_text_input_handler_int_shift_var1070+1
		lsr cycle_text_input_handler_int_shift_var1070+1
	ror cycle_text_input_handler_int_shift_var1070+0

	lda cycle_text_input_handler_int_shift_var1070
	ldy cycle_text_input_handler_int_shift_var1070+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	; Assigning a string : _pa_ptr
	;has array index
	lda #<cycle_text_input_handler_stringassignstr1072
	ldy #>cycle_text_input_handler_stringassignstr1072
	sta _pa_ptr
	sty _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
cycle_text_input_handler_elsedoneblock1060
cycle_text_input_handler_elsedoneblock1038
	; Binary clause Simplified: EQUALS
	lda keybd_input
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne cycle_text_input_handler_elsedoneblock1076
cycle_text_input_handler_ConditionalTrueBlock1074: ;Main true block ;keep 
	
; // Check keyboard toggle
	; MoveTo optimization
	lda #$51
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	; Binary clause Simplified: EQUALS
	clc
	lda keybd_type_flag
	; cmp #$00 ignored
	bne cycle_text_input_handler_elseblock1099
cycle_text_input_handler_ConditionalTrueBlock1098: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta keybd_type_flag
	
; // show message
cycle_text_input_handler_printstring_call1108
	clc
	lda #<cycle_text_input_handler_printstring_text1109
	; Forcetype: NADA
	adc #$0
	ldy #>cycle_text_input_handler_printstring_text1109
	sta print_text+0
	sty print_text+1
	; Forcetype: NADA
	ldx #$8 ; optimized, look out for bugs
	jsr printstring
	jmp cycle_text_input_handler_elsedoneblock1100
cycle_text_input_handler_elseblock1099
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta keybd_type_flag
	
; // show message
cycle_text_input_handler_printstring_call1111
	clc
	lda #<cycle_text_input_handler_printstring_text1112
	; Forcetype: NADA
	adc #$0
	ldy #>cycle_text_input_handler_printstring_text1112
	sta print_text+0
	sty print_text+1
	; Forcetype: NADA
	ldx #$8 ; optimized, look out for bugs
	jsr printstring
cycle_text_input_handler_elsedoneblock1100
	; Forcetype: NADA
	lda #$ff
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	; Forcetype: NADA
	lda #$ff
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	
; // erase message
	; MoveTo optimization
	lda #$51
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
cycle_text_input_handler_printstring_call1113
	clc
	lda #<cycle_text_input_handler_printstring_text1114
	; Forcetype: NADA
	adc #$0
	ldy #>cycle_text_input_handler_printstring_text1114
	sta print_text+0
	sty print_text+1
	; Forcetype: NADA
	ldx #$8 ; optimized, look out for bugs
	jsr printstring
cycle_text_input_handler_elsedoneblock1076
	
; // Animate the scroll around title screen
; // Ideally this would run by IRQ, but not able to solve crash..
	jsr play_title_animation
	rts
end_procedure_cycle_text_input_handler
	
; // Update score disp
	; NodeProcedureDecl -1
	; ***********  Defining procedure : update_score
	;    Procedure type : User-defined procedure
us_do_beep	dc.b	0
update_score_block1115
update_score
	
; // position of rightmost score
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda MYSCREEN_WIDTH
	sec
	; Forcetype: NADA
	sbc #$c
	sta temp_byte
	
; // Convert value from decimal number to screen code
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda score_p1
	clc
	; Forcetype: NADA
	adc #$30
	 ; end add / sub var with constant
	clc
	; Forcetype: NADA
	adc #$80
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$b
	sta (screen_loc),y
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda score_p2
	clc
	; Forcetype: NADA
	adc #$30
	 ; end add / sub var with constant
	clc
	; Forcetype: NADA
	adc #$80
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy temp_byte ; optimized, look out for bugs
	sta (screen_loc),y
	; Binary clause Simplified: NOTEQUALS
	clc
	lda us_do_beep
	; cmp #$00 ignored
	beq update_score_elsedoneblock1119
update_score_ConditionalTrueBlock1117: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
update_score_forloop1152
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock1172
update_score_ConditionalTrueBlock1170: ;Main true block ;keep 
	
; // Beep and flash score change
; // Flash changed score
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy temp_byte
	lda (screen_loc),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc),y
update_score_elsedoneblock1172
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock1178
update_score_ConditionalTrueBlock1176: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy #$b
	lda (screen_loc),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	; Forcetype: NADA
	sta (screen_loc),y
update_score_elsedoneblock1178
	
; // Enable sound
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta $e84b
	
; // Set octave
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	
; // Set the pitch
	; Forcetype: NADA
	lda #$bc
	; Calling storevariable on generic assign expression
	sta $e848
	
; // Sustain note
	; Forcetype: NADA
	lda #$80
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	
; // Turn off sound
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta $e84b
	
; // Sustain pause		
	; Forcetype: NADA
	lda #$80
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
update_score_loopstart1153
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$6
	cmp i ;keep
	bne update_score_forloop1152
update_score_loopdone1181: ;keep
update_score_loopend1154
update_score_elsedoneblock1119
	rts
end_procedure_update_score
	
; // Draw box around title screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : draw_title_screen_box
	;    Procedure type : User-defined procedure
dtsb_tmp	dc.b	0
dtsb_i	dc.b	0
draw_title_screen_box_block1182
draw_title_screen_box
	
; // Draw top and bottom	
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var1185 = $54
	sta draw_title_screen_box_rightvarInteger_var1185
	sty draw_title_screen_box_rightvarInteger_var1185+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$18
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_title_screen_box_rightvarInteger_var1185
draw_title_screen_box_wordAdd1183
	sta draw_title_screen_box_rightvarInteger_var1185
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var1185+1
	tay
	lda draw_title_screen_box_rightvarInteger_var1185
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dtsb_i
draw_title_screen_box_forloop1186
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy dtsb_i ; optimized, look out for bugs
	sta (screen_loc),y
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	sta (screen_loc_work),y
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	lda dtsb_i
	clc
	; Forcetype: NADA
	adc #$1
	 ; end add / sub var with constant
	tay
	pla
	sta (screen_loc),y
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	lda dtsb_i
	clc
	; Forcetype: NADA
	adc #$1
	 ; end add / sub var with constant
	tay
	pla
	sta (screen_loc_work),y
draw_title_screen_box_loopstart1187
	; Compare is onpage
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda dtsb_i
	clc
	; Forcetype: NADA
	adc #$2
	sta dtsb_i
	lda MYSCREEN_WIDTH
	cmp dtsb_i ;keep
	bne draw_title_screen_box_forloop1186
draw_title_screen_box_loopdone1191: ;keep
draw_title_screen_box_loopend1188
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dtsb_i
draw_title_screen_box_forloop1192
	
; // Draw sides
	; 8 bit binop
	; Add/sub where right value is constant number
	lda MYSCREEN_WIDTH
	sec
	; Forcetype: NADA
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta dtsb_tmp
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var1202 = $54
	sta draw_title_screen_box_rightvarInteger_var1202
	sty draw_title_screen_box_rightvarInteger_var1202+1
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy #0
	lda dtsb_tmp
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda dtsb_i
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_title_screen_box_rightvarInteger_var1202
draw_title_screen_box_wordAdd1200
	sta draw_title_screen_box_rightvarInteger_var1202
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var1202+1
	tay
	lda draw_title_screen_box_rightvarInteger_var1202
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy dtsb_i ; optimized, look out for bugs
	sta (screen_loc_work),y
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	lda dtsb_i
	clc
	adc dtsb_tmp
	 ; end add / sub var with constant
	tay
	pla
	sta (screen_loc_work),y
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	lda dtsb_i
	clc
	adc MYSCREEN_WIDTH
	 ; end add / sub var with constant
	tay
	pla
	sta (screen_loc_work),y
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda dtsb_i
	clc
	adc MYSCREEN_WIDTH
	 ; end add / sub var with constant
	clc
	adc dtsb_tmp
	 ; end add / sub var with constant
	tay
	pla
	sta (screen_loc_work),y
draw_title_screen_box_loopstart1193
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda dtsb_i
	clc
	; Forcetype: NADA
	adc #$2
	sta dtsb_i
	; Forcetype: NADA
	lda #$18
	cmp dtsb_i ;keep
	beq draw_title_screen_box_loopdone1203
draw_title_screen_box_loopnotdone1204
	jmp draw_title_screen_box_forloop1192
draw_title_screen_box_loopdone1203
draw_title_screen_box_loopend1194
	rts
end_procedure_draw_title_screen_box
	
; // Show title
	; NodeProcedureDecl -1
	; ***********  Defining procedure : title_screen
	;    Procedure type : User-defined procedure
title_screen
	
; // Call CLR/HOME
	jsr cursor_clear
	
; // Print title strings
; //basic_print(#title_msg_0, 1, 19);
	lda #<title_msg_1
	ldx #>title_msg_1
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_2
	ldx #>title_msg_2
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_3
	ldx #>title_msg_3
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_4
	ldx #>title_msg_4
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_5
	ldx #>title_msg_5
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$1f
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_6
	ldx #>title_msg_6
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$1b
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_7
	ldx #>title_msg_7
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$17
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_8
	ldx #>title_msg_8
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$11
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_9
	ldx #>title_msg_9
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_10
	ldx #>title_msg_10
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$11
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_11
	ldx #>title_msg_11
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$17
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	lda #<title_msg_12
	ldx #>title_msg_12
	sta _ptr
	stx _ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _center_txt
	; Forcetype: NADA
	lda #$13
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	
; // Draw box around screen
	jsr draw_title_screen_box
	
; // Start animation interrupt
; //init_irq_animation();
; // Show game mode
	jsr display_game_mode
	
; // Run input function within cycle routine
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_input
	
; // Song to play	
	lda #<theme_music_arr
	ldx #>theme_music_arr
	sta song_ptr
	stx song_ptr+1
	
; // Adjust scroll speed to compensate additional
; // CPU load for music & animation
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda scroll_speed
	sec
	; Forcetype: NADA
	sbc #$8
	sta scroll_speed
	
; // Center scroll message
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc #$13
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	sbc #$00
	tay 
	pla 
title_screen_int_shift_var1207 = $54
	sta title_screen_int_shift_var1207
	sty title_screen_int_shift_var1207+1
		lsr title_screen_int_shift_var1207+1
	ror title_screen_int_shift_var1207+0

	lda title_screen_int_shift_var1207
	ldy title_screen_int_shift_var1207+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	
; // Cycle text, wait for input, play music
; // xpos, ypos, # chars, # cycles, direction, input break, play music
; //song_ptr := #game_end_music_arr;
	ldy tmp+1 ;keep
	; Calling storevariable on generic assign expression
	sta cts_xpos
	; Forcetype: NADA
	lda #$16
	; Calling storevariable on generic assign expression
	sta cts_ypos
	; Forcetype: NADA
	lda #$13
	; Calling storevariable on generic assign expression
	sta cts_num_char
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta cts_dir
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_input_brk
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta cts_play_music
	jsr cycle_text
	
; // Reset input during text scroll 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scroll_input
	
; // Reset scroll speed	
	lda DEF_SCROLL_SPEED
	; Calling storevariable on generic assign expression
	sta scroll_speed
	rts
end_procedure_title_screen
	
; // Stop animation interrupt
; //Interrupts::Disable();
; //@@TODO: Animate pattern or cycles going around perimiter of title screen
; //		  at a fixed distance from each other. 
; // Draw box around game screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : draw_game_screen_box
	;    Procedure type : User-defined procedure
draw_game_screen_box
	
; // Draw top line
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc MYSCREEN_WIDTH
	sta screen_loc_work+0
	lda screen_loc+1
	adc #0
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop1210
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart1211
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda MYSCREEN_WIDTH
	cmp i ;keep
	bne draw_game_screen_box_forloop1210
draw_game_screen_box_loopdone1215: ;keep
draw_game_screen_box_loopend1212
	
; // Draw bottom line
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1218 = $54
	sta draw_game_screen_box_rightvarInteger_var1218
	sty draw_game_screen_box_rightvarInteger_var1218+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$18
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1218
draw_game_screen_box_wordAdd1216
	sta draw_game_screen_box_rightvarInteger_var1218
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1218+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1218
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop1219
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart1220
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda MYSCREEN_WIDTH
	cmp i ;keep
	bne draw_game_screen_box_forloop1219
draw_game_screen_box_loopdone1224: ;keep
draw_game_screen_box_loopend1221
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop1225
	
; // Draw sides
	; INTEGER optimization: a=b+c 
	lda MYSCREEN_WIDTH
	sec
	sbc #$01
	sta tmp+0
	lda #0
	sbc #$00
	sta tmp+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1238 = $54
	sta draw_game_screen_box_rightvarInteger_var1238
	sty draw_game_screen_box_rightvarInteger_var1238+1
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy tmp+1
	lda tmp
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	ldy #0
	lda i
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1238
draw_game_screen_box_wordAdd1236
	sta draw_game_screen_box_rightvarInteger_var1238
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1238+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1238
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$5d
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	pha
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda i
	clc
	adc tmp
	; Testing for byte:  tmp+1
	; RHS is word, no optimization
	pha 
	tya 
	adc tmp+1
	tay 
	pla 
	tay
	pla
	sta (screen_loc_work),y
draw_game_screen_box_loopstart1226
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_game_screen_box_forloop1225
draw_game_screen_box_loopdone1240: ;keep
draw_game_screen_box_loopend1227
	
; // top left mid box corner
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc MYSCREEN_WIDTH
	sta screen_loc_work+0
	lda screen_loc+1
	adc #0
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$70
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	
; // top right mid box corner
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
draw_game_screen_box_rightvarInteger_var1244 = $54
	sta draw_game_screen_box_rightvarInteger_var1244
	sty draw_game_screen_box_rightvarInteger_var1244+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1247 = $56
	sta draw_game_screen_box_rightvarInteger_var1247
	sty draw_game_screen_box_rightvarInteger_var1247+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1247
draw_game_screen_box_wordAdd1245
	sta draw_game_screen_box_rightvarInteger_var1247
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1247+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1247
	; Low bit binop:
	sec
	sbc draw_game_screen_box_rightvarInteger_var1244
draw_game_screen_box_wordAdd1242
	sta draw_game_screen_box_rightvarInteger_var1244
	; High-bit binop
	tya
	sbc draw_game_screen_box_rightvarInteger_var1244+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1244
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$6e
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	
; // bot left mid box corner
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1250 = $54
	sta draw_game_screen_box_rightvarInteger_var1250
	sty draw_game_screen_box_rightvarInteger_var1250+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$18
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1250
draw_game_screen_box_wordAdd1248
	sta draw_game_screen_box_rightvarInteger_var1250
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1250+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1250
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$6d
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	
; // bot right mid box corner
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
draw_game_screen_box_rightvarInteger_var1253 = $54
	sta draw_game_screen_box_rightvarInteger_var1253
	sty draw_game_screen_box_rightvarInteger_var1253+1
	; Generic 16 bit op
	ldy #0
	ldx #0 ; Fake 24 bit
	lda MYSCREEN_WIDTH
draw_game_screen_box_rightvarInteger_var1256 = $56
	sta draw_game_screen_box_rightvarInteger_var1256
	sty draw_game_screen_box_rightvarInteger_var1256+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1259 = $58
	sta draw_game_screen_box_rightvarInteger_var1259
	sty draw_game_screen_box_rightvarInteger_var1259+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$18
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1259
draw_game_screen_box_wordAdd1257
	sta draw_game_screen_box_rightvarInteger_var1259
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1259+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1259
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1256
draw_game_screen_box_wordAdd1254
	sta draw_game_screen_box_rightvarInteger_var1256
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1256+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1256
	; Low bit binop:
	sec
	sbc draw_game_screen_box_rightvarInteger_var1253
draw_game_screen_box_wordAdd1251
	sta draw_game_screen_box_rightvarInteger_var1253
	; High-bit binop
	tya
	sbc draw_game_screen_box_rightvarInteger_var1253+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1253
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$7d
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	rts
end_procedure_draw_game_screen_box
	
; // Setup game screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : game_screen
	;    Procedure type : User-defined procedure
game_screen
	
; // Call CLR/HOME
	jsr cursor_clear
	
; // Draw box around screen
	jsr draw_game_screen_box
	
; // string, x, y
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne game_screen_elsedoneblock1264
game_screen_ConditionalTrueBlock1262: ;Main true block ;keep 
	
; // Draw score display (text, not score values)
	; INTEGER optimization: a=b+c 
	lda MYSCREEN_WIDTH
	sec
	sbc #$0a
	sta tmp+0
	lda #0
	sbc #$00
	sta tmp+1
	lda #<score_msg_2
	ldx #>score_msg_2
	sta _pa_ptr
	stx _pa_ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
	
; // string, x, y
	lda #<score_msg_3
	ldx #>score_msg_3
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
game_screen_elsedoneblock1264
	
; // string, x, y
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne game_screen_elsedoneblock1272
game_screen_ConditionalTrueBlock1270: ;Main true block ;keep 
	; INTEGER optimization: a=b+c 
	lda MYSCREEN_WIDTH
	sec
	sbc #$0a
	sta tmp+0
	lda #0
	sbc #$00
	sta tmp+1
	lda #<score_msg_0
	ldx #>score_msg_0
	sta _pa_ptr
	stx _pa_ptr+1
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
	
; // string, x, y
	lda #<score_msg_1
	ldx #>score_msg_1
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
game_screen_elsedoneblock1272
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta us_do_beep
	jsr update_score
	jsr init_engine_sound
	jsr init_gamestate
	rts
end_procedure_game_screen
	
; // Start new round
	; NodeProcedureDecl -1
	; ***********  Defining procedure : new_round
	;    Procedure type : User-defined procedure
new_round
	
; // Print crash message
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc message_len
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcs new_round_skip1279
	dey
new_round_skip1279
new_round_int_shift_var1280 = $54
	sta new_round_int_shift_var1280
	sty new_round_int_shift_var1280+1
		lsr new_round_int_shift_var1280+1
	ror new_round_int_shift_var1280+0

	lda new_round_int_shift_var1280
	ldy new_round_int_shift_var1280+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	lda message_ptr
	ldx message_ptr+1
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
	
; // string, x, y
; // xpos, ypos, # chars, # cycles, direction, input break, play music
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta cts_xpos
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta cts_ypos
	lda message_len
	; Calling storevariable on generic assign expression
	sta cts_num_char
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_dir
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta cts_input_brk
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta cts_play_music
	jsr cycle_text
	
; // Beep and flash score change
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta us_do_beep
	jsr update_score
	
; // Print new round message & wait for input
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	; Forcetype: NADA
	lda #$15
	; Calling storevariable on generic assign expression
	sta message_len
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc message_len
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcs new_round_skip1282
	dey
new_round_skip1282
new_round_int_shift_var1283 = $54
	sta new_round_int_shift_var1283
	sty new_round_int_shift_var1283+1
		lsr new_round_int_shift_var1283+1
	ror new_round_int_shift_var1283+0

	lda new_round_int_shift_var1283
	ldy new_round_int_shift_var1283+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	lda #<msg_get_ready
	ldx #>msg_get_ready
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
	
; // xpos, ypos, # chars, # cycles, direction, input break, play music
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta cts_xpos
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta cts_ypos
	lda message_len
	; Calling storevariable on generic assign expression
	sta cts_num_char
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta cts_dir
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_input_brk
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta cts_play_music
	jsr cycle_text
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_localfailed1289
	jmp new_round_ConditionalTrueBlock1285
new_round_localfailed1289: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_elsedoneblock1287
new_round_ConditionalTrueBlock1285: ;Main true block ;keep 
	
; // Adjust game speed
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$a
	sta game_speed
new_round_elsedoneblock1287
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_localfailed1296
	jmp new_round_ConditionalTrueBlock1292
new_round_localfailed1296: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_elsedoneblock1294
new_round_ConditionalTrueBlock1292: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$f
	sta game_speed
new_round_elsedoneblock1294
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_localfailed1303
	jmp new_round_ConditionalTrueBlock1299
new_round_localfailed1303: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_elsedoneblock1301
new_round_ConditionalTrueBlock1299: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$14
	sta game_speed
new_round_elsedoneblock1301
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_localfailed1310
	jmp new_round_ConditionalTrueBlock1306
new_round_localfailed1310: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_elsedoneblock1308
new_round_ConditionalTrueBlock1306: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$19
	sta game_speed
new_round_elsedoneblock1308
	
; // Redraw game screen
	jsr game_screen
	rts
end_procedure_new_round
	
; // Start new game
	; NodeProcedureDecl -1
	; ***********  Defining procedure : end_game
	;    Procedure type : User-defined procedure
end_game
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta us_do_beep
	jsr update_score
	
; // Print Game Over message
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc message_len
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcs end_game_skip1314
	dey
end_game_skip1314
end_game_int_shift_var1315 = $54
	sta end_game_int_shift_var1315
	sty end_game_int_shift_var1315+1
		lsr end_game_int_shift_var1315+1
	ror end_game_int_shift_var1315+0

	lda end_game_int_shift_var1315
	ldy end_game_int_shift_var1315+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	lda message_ptr
	ldx message_ptr+1
	sta _pa_ptr
	stx _pa_ptr+1
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
	
; // string, x, y
; // Cycle text, wait for input, & play music
	lda #<game_end_music_arr
	ldx #>game_end_music_arr
	sta song_ptr
	stx song_ptr+1
	
; // xpos, ypos, # chars, # cycles, direction, input break, play music
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta cts_xpos
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta cts_ypos
	lda message_len
	; Calling storevariable on generic assign expression
	sta cts_num_char
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta cts_dir
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta cts_input_brk
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_play_music
	jsr cycle_text
	
; // Reset scores, game speed & return to main loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta score_p1
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta score_p2
	lda DEF_GAME_SPEED
	; Calling storevariable on generic assign expression
	sta game_speed
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_over_flag
	rts
end_procedure_end_game
	
; // Game state
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_game_state
	;    Procedure type : User-defined procedure
check_game_state
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1322
	jmp check_game_state_ConditionalTrueBlock1318
check_game_state_localfailed1322: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1320
check_game_state_ConditionalTrueBlock1318: ;Main true block ;keep 
	
; // Check quit game hotkey 
; //if (keybd_input = 5) then 
; //begin
; //	game_over_flag := 1;
; //	stop_sound();
; //	do_delay(255);
; //	do_delay(255);
; //	return;
; //end;	
; // Check for player crashed
	jsr player_crash
	jsr stop_sound
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_run_flag
check_game_state_elsedoneblock1320
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1482
check_game_state_localsuccess1483: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1482
	jmp check_game_state_ConditionalTrueBlock1325
check_game_state_localfailed1482
	jmp check_game_state_elseblock1326
check_game_state_ConditionalTrueBlock1325: ;Main true block ;keep 
	lda #<msg_both_crash
	ldx #>msg_both_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
	jmp check_game_state_elsedoneblock1327
check_game_state_elseblock1326
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock1488
check_game_state_ConditionalTrueBlock1487: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p2
	; Binary clause Simplified: LESS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1568
check_game_state_ConditionalTrueBlock1566: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1581
check_game_state_ConditionalTrueBlock1580: ;Main true block ;keep 
	lda #<msg_plr_crash
	ldx #>msg_plr_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$e
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1582
check_game_state_elseblock1581
	lda #<msg_p1_crash
	ldx #>msg_p1_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1582
	jsr new_round
check_game_state_elsedoneblock1568
	jmp check_game_state_elsedoneblock1489
check_game_state_elseblock1488
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1591
check_game_state_ConditionalTrueBlock1589: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p1
	; Binary clause Simplified: LESS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1619
check_game_state_ConditionalTrueBlock1617: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1632
check_game_state_ConditionalTrueBlock1631: ;Main true block ;keep 
	lda #<msg_sark_crash
	ldx #>msg_sark_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1633
check_game_state_elseblock1632
	lda #<msg_p2_crash
	ldx #>msg_p2_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1633
	jsr new_round
check_game_state_elsedoneblock1619
check_game_state_elsedoneblock1591
check_game_state_elsedoneblock1489
check_game_state_elsedoneblock1327
	; Binary clause Simplified: GREATEREQUAL
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elseblock1640
check_game_state_ConditionalTrueBlock1639: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1677
check_game_state_ConditionalTrueBlock1676: ;Main true block ;keep 
	
; // Check for end of game
	lda #<msg_plr_wins
	ldx #>msg_plr_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$b
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1678
check_game_state_elseblock1677
	lda #<msg_p1_wins
	ldx #>msg_p1_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1678
	jsr end_game
	jmp check_game_state_elsedoneblock1641
check_game_state_elseblock1640
	; Binary clause Simplified: GREATEREQUAL
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elsedoneblock1687
check_game_state_ConditionalTrueBlock1685: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1700
check_game_state_ConditionalTrueBlock1699: ;Main true block ;keep 
	lda #<msg_sark_wins
	ldx #>msg_sark_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$9
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1701
check_game_state_elseblock1700
	lda #<msg_p2_wins
	ldx #>msg_p2_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1701
	jsr end_game
check_game_state_elsedoneblock1687
check_game_state_elsedoneblock1641
	rts
end_procedure_check_game_state
	
; // Check Collisions
; //	player_1_xy, player_2_xy 		- Player Coordinates
; //	player_1_crash, player_2_crash	- Crash Flags
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_collisions
	;    Procedure type : User-defined procedure
check_collisions
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_1_crash
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_crash
	; Binary clause INTEGER: EQUALS
	lda player_1_xy+1   ; compare high bytes
	cmp player_2_xy+1 ;keep
	bne check_collisions_elsedoneblock1710
	lda player_1_xy
	cmp player_2_xy ;keep
	bne check_collisions_elsedoneblock1710
	jmp check_collisions_ConditionalTrueBlock1708
check_collisions_ConditionalTrueBlock1708: ;Main true block ;keep 
	
; // Both crashed before trail is drawn. This solves edge case 
; // when both players try to occupy the same location.
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1710
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1725
	jmp check_collisions_ConditionalTrueBlock1714
check_collisions_localfailed1725: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1716
check_collisions_ConditionalTrueBlock1714: ;Main true block ;keep 
	
; // P1 crashed
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc player_1_xy
	sta screen_loc_work+0
	lda screen_loc+1
	adc player_1_xy+1
	sta screen_loc_work+1
	; Binary clause Simplified: NOTEQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (screen_loc_work),y
	; Compare with pure num / var optimization
	cmp #$20;keep
	beq check_collisions_elsedoneblock1731
check_collisions_ConditionalTrueBlock1729: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
check_collisions_elsedoneblock1731
check_collisions_elsedoneblock1716
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1746
	jmp check_collisions_ConditionalTrueBlock1735
check_collisions_localfailed1746: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1737
check_collisions_ConditionalTrueBlock1735: ;Main true block ;keep 
	
; // P2 crashed
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc player_2_xy
	sta screen_loc_work+0
	lda screen_loc+1
	adc player_2_xy+1
	sta screen_loc_work+1
	; Binary clause Simplified: NOTEQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (screen_loc_work),y
	; Compare with pure num / var optimization
	cmp #$20;keep
	beq check_collisions_elsedoneblock1752
check_collisions_ConditionalTrueBlock1750: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1752
check_collisions_elsedoneblock1737
	rts
end_procedure_check_collisions
	
; // Check & return distance to crash_anim_arr
	; NodeProcedureDecl -1
	; ***********  Defining procedure : sark_crash_distance
	;    Procedure type : User-defined procedure
scd_i	dc.b	0
scd_crash	dc.b	0
scd_tmp_int	dc.w	0
scd_head	dc.b	0
sark_crash_distance_block1755
sark_crash_distance
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scd_i
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta scd_crash
	ldy player_2_xy+1 ;keep
	lda player_2_xy
	; Calling storevariable on generic assign expression
	sta scd_tmp_int
	sty scd_tmp_int+1
sark_crash_distance_while1756
sark_crash_distance_loopstart1760
	; Binary clause Simplified: EQUALS
	clc
	lda scd_crash
	; cmp #$00 ignored
	bne sark_crash_distance_elsedoneblock1759
sark_crash_distance_ConditionalTrueBlock1757: ;Main true block ;keep 
	; Test Inc dec D
	inc scd_i
	
; // increment pos			
	; Binary clause Simplified: NOTEQUALS
	clc
	jsr Model_Is_40Col
	; cmp #$00 ignored
	beq sark_crash_distance_elseblock1785
sark_crash_distance_ConditionalTrueBlock1784: ;Main true block ;keep 
	
; // number of moves until crash
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	; Load Integer array
	; CAST type INTEGER
	lda scd_head
	asl
	tax
	lda dir_map_arr,x 
	ldy dir_map_arr+1,x 
	clc
	adc scd_tmp_int
	; Testing for byte:  scd_tmp_int+1
	; RHS is word, no optimization
	pha 
	tya 
	adc scd_tmp_int+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta scd_tmp_int
	sty scd_tmp_int+1
	jmp sark_crash_distance_elsedoneblock1786
sark_crash_distance_elseblock1785
	
; // increment pos
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	; Load Integer array
	; CAST type INTEGER
	lda scd_head
	asl
	tax
	lda dir_map_arr80,x 
	ldy dir_map_arr80+1,x 
	clc
	adc scd_tmp_int
	; Testing for byte:  scd_tmp_int+1
	; RHS is word, no optimization
	pha 
	tya 
	adc scd_tmp_int+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta scd_tmp_int
	sty scd_tmp_int+1
sark_crash_distance_elsedoneblock1786
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc scd_tmp_int
	sta screen_loc_work+0
	lda screen_loc+1
	adc scd_tmp_int+1
	sta screen_loc_work+1
	; Binary clause Simplified: NOTEQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (screen_loc_work),y
	; Compare with pure num / var optimization
	cmp #$20;keep
	beq sark_crash_distance_elsedoneblock1799
sark_crash_distance_ConditionalTrueBlock1797: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scd_crash
sark_crash_distance_elsedoneblock1799
	jmp sark_crash_distance_while1756
sark_crash_distance_elsedoneblock1759
sark_crash_distance_loopend1761
	lda scd_i
	rts
end_procedure_sark_crash_distance
	
; // Check Sark's distance to player in x and y dirs
	; NodeProcedureDecl -1
	; ***********  Defining procedure : sark_check_player_dist
	;    Procedure type : User-defined procedure
scpd_sark_x	dc.b	0
scpd_sark_y	dc.b	0
scpd_player_x	dc.b	0
scpd_player_y	dc.b	0
sark_check_player_dist_block1802
sark_check_player_dist
	
; //	sark_near_player_x_dist: byte;
; //	sark_near_player_x_neg:	boolean;	
; // left is neg
; //	sark_near_player_y_dist:	byte;
; //	sark_near_player_y_neg:	boolean;	
; // up is neg
; // Determine Sark's XY position
	; 16x8 div
	ldy player_2_xy+1 ;keep
	lda player_2_xy
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	lda MYSCREEN_WIDTH
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	; Calling storevariable on generic assign expression
	sta scpd_sark_y
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; 8 bit mul
	; Load right hand side
	tax
	lda MYSCREEN_WIDTH
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
sark_check_player_dist_rightvarInteger_var1806 = $54
	sta sark_check_player_dist_rightvarInteger_var1806
	sty sark_check_player_dist_rightvarInteger_var1806+1
	lda player_2_xy+1
	sec
	sbc sark_check_player_dist_rightvarInteger_var1806+1
	tay
	lda player_2_xy
	sec
	sbc sark_check_player_dist_rightvarInteger_var1806
	bcs sark_check_player_dist_wordAdd1803
	dey
sark_check_player_dist_wordAdd1803
	; Calling storevariable on generic assign expression
	sta scpd_sark_x
	
; // Derermine player's XY position
	; 16x8 div
	ldy player_1_xy+1 ;keep
	lda player_1_xy
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	lda MYSCREEN_WIDTH
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	; Calling storevariable on generic assign expression
	sta scpd_player_y
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; 8 bit mul
	; Load right hand side
	tax
	lda MYSCREEN_WIDTH
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
sark_check_player_dist_rightvarInteger_var1810 = $54
	sta sark_check_player_dist_rightvarInteger_var1810
	sty sark_check_player_dist_rightvarInteger_var1810+1
	lda player_1_xy+1
	sec
	sbc sark_check_player_dist_rightvarInteger_var1810+1
	tay
	lda player_1_xy
	sec
	sbc sark_check_player_dist_rightvarInteger_var1810
	bcs sark_check_player_dist_wordAdd1807
	dey
sark_check_player_dist_wordAdd1807
	; Calling storevariable on generic assign expression
	sta scpd_player_x
	; Binary clause Simplified: GREATER
	lda scpd_sark_y
	; Compare with pure num / var optimization
	cmp scpd_player_y;keep
	bcc sark_check_player_dist_elseblock1813
	beq sark_check_player_dist_elseblock1813
sark_check_player_dist_ConditionalTrueBlock1812: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta sark_near_player_y_neg
	
; // player is above Sark
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scpd_sark_y
	sec
	sbc scpd_player_y
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta sark_near_player_y_dist
	jmp sark_check_player_dist_elsedoneblock1814
sark_check_player_dist_elseblock1813
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta sark_near_player_y_neg
	
; // player is below Sark
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scpd_player_y
	sec
	sbc scpd_sark_y
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta sark_near_player_y_dist
sark_check_player_dist_elsedoneblock1814
	; Binary clause Simplified: GREATER
	lda scpd_sark_x
	; Compare with pure num / var optimization
	cmp scpd_player_x;keep
	bcc sark_check_player_dist_elseblock1821
	beq sark_check_player_dist_elseblock1821
sark_check_player_dist_ConditionalTrueBlock1820: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta sark_near_player_x_neg
	
; // player is left of Sark
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scpd_sark_x
	sec
	sbc scpd_player_x
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta sark_near_player_x_dist
	jmp sark_check_player_dist_elsedoneblock1822
sark_check_player_dist_elseblock1821
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta sark_near_player_x_neg
	
; // player is right of Sark
	; 8 bit binop
	; Add/sub where right value is constant number
	lda scpd_player_x
	sec
	sbc scpd_sark_x
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta sark_near_player_x_dist
sark_check_player_dist_elsedoneblock1822
	rts
end_procedure_sark_check_player_dist
	
; //	Sark's logic routine to box in opponent & avoid crashing.
; //  Sets: player_2_input, player_2_fire
	; NodeProcedureDecl -1
	; ***********  Defining procedure : sark_move
	;    Procedure type : User-defined procedure
sm_tmp_int	dc.w	0
sm_cr_dist	dc.b	0
sm_cr_dist_l	dc.b	0
sm_cr_dist_r	dc.b	0
sm_cr_dist_u	dc.b	0
sm_cr_dist_d	dc.b	0
sm_cr_th	dc.b	0
sm_boost_th	dc.b	0
sm_prox_th	dc.b	0
sm_head	dc.b	0
sm_i	dc.b	0
sark_move_block1827
sark_move
	
; // crash threshold
; // boost threshold
; // proximity threshold	
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_2_input
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_fire
	; Forcetype: NADA
	lda #$8
	; Calling storevariable on generic assign expression
	sta sm_boost_th
	
; // proximity to boost
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta sm_prox_th
	
; // proximity to cut off
	; Forcetype: NADA
	lda #$5
	; Calling storevariable on generic assign expression
	sta sm_cr_th
	
; // min val 2
	lda player_2_head
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_cr_dist
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_cr_dist_l
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_cr_dist_r
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_cr_dist_u
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_cr_dist_d
	
; // Check relative position of player
	jsr sark_check_player_dist
	; Binary clause Simplified: LESS
	lda sm_cr_dist
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcs sark_move_localfailed2259
	jmp sark_move_ConditionalTrueBlock1829
sark_move_localfailed2259
	jmp sark_move_elsedoneblock1831
sark_move_ConditionalTrueBlock1829: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed2476
	jmp sark_move_ConditionalTrueBlock2262
sark_move_localfailed2476: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_localfailed2475
	jmp sark_move_ConditionalTrueBlock2262
sark_move_localfailed2475
	jmp sark_move_elseblock2263
sark_move_ConditionalTrueBlock2262: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2480
	beq sark_move_elseblock2480
sark_move_localsuccess2513: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2480
	beq sark_move_elseblock2480
sark_move_ConditionalTrueBlock2479: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2517
sark_move_ConditionalTrueBlock2516: ;Main true block ;keep 
	
; //	Check distance on current heading to obstacle.  
; //	If both turn options are elegible, turn towards player.
; //	Otherwise, turn in direction with greater distance.
; // crash imminent, check other directions
; // Sark is moving L or R. Check U/D
; // If both alternatives have 'good' crash distance, 
; // move in direction closer to player 
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2518
sark_move_elseblock2517
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2518
	jmp sark_move_elsedoneblock2481
sark_move_elseblock2480
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_dist_d;keep
	bcc sark_move_elseblock2526
	beq sark_move_elseblock2526
sark_move_localsuccess2536: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elseblock2526
	beq sark_move_elseblock2526
sark_move_ConditionalTrueBlock2525: ;Main true block ;keep 
	
; // Take direction with best crash distance
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2527
sark_move_elseblock2526
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elsedoneblock2542
	beq sark_move_elsedoneblock2542
sark_move_ConditionalTrueBlock2540: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2542
sark_move_elsedoneblock2527
sark_move_elsedoneblock2481
	jmp sark_move_elsedoneblock2264
sark_move_elseblock2263
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_localfailed2618
	jmp sark_move_ConditionalTrueBlock2547
sark_move_localfailed2618: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne sark_move_elsedoneblock2549
sark_move_ConditionalTrueBlock2547: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2622
	beq sark_move_elseblock2622
sark_move_localsuccess2655: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2622
	beq sark_move_elseblock2622
sark_move_ConditionalTrueBlock2621: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2659
sark_move_ConditionalTrueBlock2658: ;Main true block ;keep 
	
; // Sark is moving U or D. Check L/R
; // If both alternatives have 'good' crash distance,
; // move in direction closer to player
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2660
sark_move_elseblock2659
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2660
	jmp sark_move_elsedoneblock2623
sark_move_elseblock2622
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_dist_r;keep
	bcc sark_move_elseblock2668
	beq sark_move_elseblock2668
sark_move_localsuccess2678: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elseblock2668
	beq sark_move_elseblock2668
sark_move_ConditionalTrueBlock2667: ;Main true block ;keep 
	
; // Take direction with best crash distance
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2669
sark_move_elseblock2668
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elsedoneblock2684
	beq sark_move_elsedoneblock2684
sark_move_ConditionalTrueBlock2682: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2684
sark_move_elsedoneblock2669
sark_move_elsedoneblock2623
sark_move_elsedoneblock2549
sark_move_elsedoneblock2264
sark_move_elsedoneblock1831
	; Binary clause Simplified: EQUALS
	lda game_difficulty
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_localfailed2712
	jmp sark_move_ConditionalTrueBlock2688
sark_move_localfailed2712: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda game_difficulty
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_elsedoneblock2690
sark_move_ConditionalTrueBlock2688: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock2717
sark_move_ConditionalTrueBlock2715: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda sark_near_player_x_dist
	; Compare with pure num / var optimization
	cmp sm_boost_th;keep
	bcs sark_move_elsedoneblock2730
sark_move_localsuccess2732: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	lda sark_near_player_y_dist
	; Compare with pure num / var optimization
	cmp sm_boost_th;keep
	bcs sark_move_elsedoneblock2730
sark_move_ConditionalTrueBlock2728: ;Main true block ;keep 
	
; //	If opponent less than 'n' moves away, engage turbo boost 
; //	unless it would cause a crash.
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
sark_move_elsedoneblock2730
sark_move_elsedoneblock2717
sark_move_elsedoneblock2690
	; Binary clause Simplified: EQUALS
	lda game_difficulty
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_localfailed2959
	jmp sark_move_ConditionalTrueBlock2735
sark_move_localfailed2959
	jmp sark_move_elsedoneblock2737
sark_move_ConditionalTrueBlock2735: ;Main true block ;keep 
	; Binary clause Simplified: LESSEQUAL
	lda sark_near_player_x_dist
	; Compare with pure num / var optimization
	cmp sm_prox_th;keep
	beq sark_move_localsuccess3073
	bcs sark_move_localfailed3072
sark_move_localsuccess3073: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESSEQUAL
	lda sark_near_player_y_dist
	; Compare with pure num / var optimization
	cmp sm_prox_th;keep
	beq sark_move_ConditionalTrueBlock2962
	bcs sark_move_localfailed3072
	jmp sark_move_ConditionalTrueBlock2962
sark_move_localfailed3072
	jmp sark_move_elsedoneblock2964
sark_move_ConditionalTrueBlock2962: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed3103
	jmp sark_move_ConditionalTrueBlock3076
sark_move_localfailed3103: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_elsedoneblock3078
sark_move_ConditionalTrueBlock3076: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	beq sark_move_elseblock3107
sark_move_localsuccess3118: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elseblock3107
sark_move_ConditionalTrueBlock3106: ;Main true block ;keep 
	
; //	If within player proximity, both have same heading, AND
; //	player's row/col is open at Sark's row/col position,
; //	then cut off the player 
; // Sark is moving left/right
; // Player is above Sark, and move available
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
	
; // move up
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_2_fire
	jmp sark_move_elsedoneblock3108
sark_move_elseblock3107
	
; // can't boost & turn
	; Binary clause Simplified: EQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	bne sark_move_elsedoneblock3124
sark_move_localsuccess3126: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock3124
sark_move_ConditionalTrueBlock3122: ;Main true block ;keep 
	
; // can't boost & turn
; // Player is below Sark, and move available
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
	
; // move down
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_2_fire
sark_move_elsedoneblock3124
sark_move_elsedoneblock3108
sark_move_elsedoneblock3078
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_localfailed3156
	jmp sark_move_ConditionalTrueBlock3129
sark_move_localfailed3156: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne sark_move_elsedoneblock3131
sark_move_ConditionalTrueBlock3129: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	beq sark_move_elseblock3160
sark_move_localsuccess3171: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elseblock3160
sark_move_ConditionalTrueBlock3159: ;Main true block ;keep 
	
; // Sark is moving up/down
; // Player is left of Sark, and move available
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
	
; // move left
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_2_fire
	jmp sark_move_elsedoneblock3161
sark_move_elseblock3160
	
; // can't boost & turn
	; Binary clause Simplified: EQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	bne sark_move_elsedoneblock3177
sark_move_localsuccess3179: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock3177
sark_move_ConditionalTrueBlock3175: ;Main true block ;keep 
	
; // can't boost & turn
; // Player is below Sark, and move available
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
	
; // move right
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_2_fire
sark_move_elsedoneblock3177
sark_move_elsedoneblock3161
sark_move_elsedoneblock3131
sark_move_elsedoneblock2964
sark_move_elsedoneblock2737
	rts
end_procedure_sark_move
	
; // DEBUG
; //	moveto(1,1,hi(screen_char_loc));
; //	printstring("MOVING DN",0,9);
; // Update Positions
; // 	player_1_xy, player_2_xy 		- Player Coordinates
; // 	player_1_head, player_2_head 		- Player Headings 
; // 	player_1_fire, player_2_fire 		- Turbo Engaged
; // 	player_1_trail, player_2_trail 	- Trail Positions 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : update_positions
	;    Procedure type : User-defined procedure
update_positions
	
; // Check if move avail
	; Test Inc dec D
	inc turn_counter
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc update_positions_elsedoneblock3185
update_positions_ConditionalTrueBlock3183: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta turn_counter
update_positions_elsedoneblock3185
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed3255
	jmp update_positions_ConditionalTrueBlock3189
update_positions_localfailed3255: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed3254
	jmp update_positions_ConditionalTrueBlock3189
update_positions_localfailed3254
	jmp update_positions_elsedoneblock3191
update_positions_ConditionalTrueBlock3189: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock3260
update_positions_ConditionalTrueBlock3258: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock3273
update_positions_localsuccess3275: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_1_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_1_input;keep
	beq update_positions_elsedoneblock3273
update_positions_ConditionalTrueBlock3271: ;Main true block ;keep 
	
; // May need to tweak this value
; // Move P1 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_1_input
	; Calling storevariable on generic assign expression
	sta player_1_head
update_positions_elsedoneblock3273
update_positions_elsedoneblock3260
	
; // Draw Trail
	; Load Integer array
	; CAST type INTEGER
	ldx #0 ; watch for bug, Integer array has max index of 128
	lda player_1_trail,x 
	ldy player_1_trail+1,x 
	; Calling storevariable on generic assign expression
	sta player_1_trail+2
	sty player_1_trail+3
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc player_1_xy
	; Testing for byte:  player_1_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	adc player_1_xy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta player_1_trail+0
	sty player_1_trail+1
	; Binary clause Simplified: NOTEQUALS
	clc
	jsr Model_Is_40Col
	; cmp #$00 ignored
	beq update_positions_elseblock3280
update_positions_ConditionalTrueBlock3279: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock3304
update_positions_ConditionalTrueBlock3302: ;Main true block ;keep 
	
; // Update Player 1 Position
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	; Load Integer array
	; CAST type INTEGER
	lda player_1_head
	asl
	tax
	lda dir_map_arr,x 
	ldy dir_map_arr+1,x 
	clc
	adc player_1_xy
	; Testing for byte:  player_1_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	adc player_1_xy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta player_1_xy
	sty player_1_xy+1
update_positions_elsedoneblock3304
	jmp update_positions_elsedoneblock3281
update_positions_elseblock3280
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock3313
update_positions_ConditionalTrueBlock3311: ;Main true block ;keep 
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	; Load Integer array
	; CAST type INTEGER
	lda player_1_head
	asl
	tax
	lda dir_map_arr80,x 
	ldy dir_map_arr80+1,x 
	clc
	adc player_1_xy
	; Testing for byte:  player_1_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	adc player_1_xy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta player_1_xy
	sty player_1_xy+1
update_positions_elsedoneblock3313
update_positions_elsedoneblock3281
update_positions_elsedoneblock3191
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed3385
	jmp update_positions_ConditionalTrueBlock3319
update_positions_localfailed3385: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed3384
	jmp update_positions_ConditionalTrueBlock3319
update_positions_localfailed3384
	jmp update_positions_elsedoneblock3321
update_positions_ConditionalTrueBlock3319: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock3390
update_positions_ConditionalTrueBlock3388: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock3403
update_positions_localsuccess3405: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_2_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_2_input;keep
	beq update_positions_elsedoneblock3403
update_positions_ConditionalTrueBlock3401: ;Main true block ;keep 
	
; // Move P2 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_2_input
	; Calling storevariable on generic assign expression
	sta player_2_head
update_positions_elsedoneblock3403
update_positions_elsedoneblock3390
	
; // Draw Trail
	; Load Integer array
	; CAST type INTEGER
	ldx #0 ; watch for bug, Integer array has max index of 128
	lda player_2_trail,x 
	ldy player_2_trail+1,x 
	; Calling storevariable on generic assign expression
	sta player_2_trail+2
	sty player_2_trail+3
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc player_2_xy
	; Testing for byte:  player_2_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	adc player_2_xy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta player_2_trail+0
	sty player_2_trail+1
	; Binary clause Simplified: NOTEQUALS
	clc
	jsr Model_Is_40Col
	; cmp #$00 ignored
	beq update_positions_elseblock3410
update_positions_ConditionalTrueBlock3409: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock3434
update_positions_ConditionalTrueBlock3432: ;Main true block ;keep 
	
; // Update Player 2 Position
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	; Load Integer array
	; CAST type INTEGER
	lda player_2_head
	asl
	tax
	lda dir_map_arr,x 
	ldy dir_map_arr+1,x 
	clc
	adc player_2_xy
	; Testing for byte:  player_2_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	adc player_2_xy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta player_2_xy
	sty player_2_xy+1
update_positions_elsedoneblock3434
	jmp update_positions_elsedoneblock3411
update_positions_elseblock3410
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock3443
update_positions_ConditionalTrueBlock3441: ;Main true block ;keep 
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	; Load Integer array
	; CAST type INTEGER
	lda player_2_head
	asl
	tax
	lda dir_map_arr80,x 
	ldy dir_map_arr80+1,x 
	clc
	adc player_2_xy
	; Testing for byte:  player_2_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	adc player_2_xy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta player_2_xy
	sty player_2_xy+1
update_positions_elsedoneblock3443
update_positions_elsedoneblock3411
update_positions_elsedoneblock3321
	rts
end_procedure_update_positions
	
; // Update Screen
; //	player_1_xy, player_2_xy - Player Coordinates 	
; //	player_1_trail, player_2_trail - Trail Coordinates
	; NodeProcedureDecl -1
	; ***********  Defining procedure : update_screen
	;    Procedure type : User-defined procedure
update_screen
	
; // P1 bike & trail
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc player_1_xy
	sta screen_loc_work+0
	lda screen_loc+1
	adc player_1_xy+1
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$5a
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	; Load Integer array
	; CAST type INTEGER
	ldx #2 ; watch for bug, Integer array has max index of 128
	lda player_1_trail,x 
	ldy player_1_trail+1,x 
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	
; // P2 bike & trail
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc player_2_xy
	sta screen_loc_work+0
	lda screen_loc+1
	adc player_2_xy+1
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$51
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	sta (screen_loc_work),y
	; Load Integer array
	; CAST type INTEGER
	lda player_2_trail,x 
	ldy player_2_trail+1,x 
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	rts
end_procedure_update_screen
	
; // Init Variables
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init_gamestate
	;    Procedure type : User-defined procedure
init_gamestate
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$5
init_gamestate_rightvarInteger_var3454 = $54
	sta init_gamestate_rightvarInteger_var3454
	sty init_gamestate_rightvarInteger_var3454+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$c
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc init_gamestate_rightvarInteger_var3454
init_gamestate_wordAdd3452
	sta init_gamestate_rightvarInteger_var3454
	; High-bit binop
	tya
	adc init_gamestate_rightvarInteger_var3454+1
	tay
	lda init_gamestate_rightvarInteger_var3454
	; Calling storevariable on generic assign expression
	sta player_1_xy
	sty player_1_xy+1
	; Generic 16 bit op
	ldy #0
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype: INTEGER
	lda #$c
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
init_gamestate_rightvarInteger_var3457 = $54
	sta init_gamestate_rightvarInteger_var3457
	sty init_gamestate_rightvarInteger_var3457+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda MYSCREEN_WIDTH
	sec
	sbc #$06
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	sbc #$00
	tay 
	pla 
	; Low bit binop:
	clc
	adc init_gamestate_rightvarInteger_var3457
init_gamestate_wordAdd3455
	sta init_gamestate_rightvarInteger_var3457
	; High-bit binop
	tya
	adc init_gamestate_rightvarInteger_var3457+1
	tay
	lda init_gamestate_rightvarInteger_var3457
	; Calling storevariable on generic assign expression
	sta player_2_xy
	sty player_2_xy+1
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_head
	
; // heading right
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_head
	
; // heading left
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta player_1_trail+0
	sty player_1_trail+1
	; Forcetype: INTEGER
	; Calling storevariable on generic assign expression
	sta player_1_trail+2
	sty player_1_trail+3
	; Forcetype: INTEGER
	; Calling storevariable on generic assign expression
	sta player_2_trail+0
	sty player_2_trail+1
	; Forcetype: INTEGER
	; Calling storevariable on generic assign expression
	sta player_2_trail+2
	sty player_2_trail+3
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_1_crash
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_crash
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_1_input
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_input
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_1_fire
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_fire
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta turn_counter
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_run_flag
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta game_mode_changed
	rts
end_procedure_init_gamestate
	
; // Play title screen animation 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : play_title_animation
	;    Procedure type : User-defined procedure
pta_tmp	dc.b	0
pta_i	dc.b	0
pta_screen_loc	= $72
play_title_animation_block3459
play_title_animation
	; Binary clause Simplified: EQUALS
	clc
	lda anim_speed
	; cmp #$00 ignored
	bne play_title_animation_localfailed3579
	jmp play_title_animation_ConditionalTrueBlock3461
play_title_animation_localfailed3579
	jmp play_title_animation_elsedoneblock3463
play_title_animation_ConditionalTrueBlock3461: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop3581
	
; // Top/bottom sides
; // Top
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc pta_i
	sta pta_screen_loc+0
	lda screen_loc+1
	adc #0
	sta pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock3611
play_title_animation_ConditionalTrueBlock3610: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3612
play_title_animation_elseblock3611
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3612
	
; // Bottom
	; Generic 16 bit op
	ldy #0
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	lda #$18
	sta mul16x8_num2
	jsr mul16x8_procedure
play_title_animation_rightvarInteger_var3619 = $54
	sta play_title_animation_rightvarInteger_var3619
	sty play_title_animation_rightvarInteger_var3619+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc pta_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc play_title_animation_skip3621
	iny
play_title_animation_skip3621
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var3619
play_title_animation_wordAdd3617
	sta play_title_animation_rightvarInteger_var3619
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3619+1
	tay
	lda play_title_animation_rightvarInteger_var3619
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock3624
play_title_animation_ConditionalTrueBlock3623: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3625
play_title_animation_elseblock3624
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3625
play_title_animation_loopstart3582
	; Test Inc dec D
	inc pta_i
	lda MYSCREEN_WIDTH
	cmp pta_i ;keep
	beq play_title_animation_loopdone3630
play_title_animation_loopnotdone3631
	jmp play_title_animation_forloop3581
play_title_animation_loopdone3630
play_title_animation_loopend3583
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop3632
	
; // Left/Right sides
; // Left
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var3667 = $54
	sta play_title_animation_rightvarInteger_var3667
	sty play_title_animation_rightvarInteger_var3667+1
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy #0
	lda pta_i
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var3667
play_title_animation_wordAdd3665
	sta play_title_animation_rightvarInteger_var3667
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3667+1
	tay
	lda play_title_animation_rightvarInteger_var3667
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock3670
play_title_animation_ConditionalTrueBlock3669: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3671
play_title_animation_elseblock3670
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3671
	
; // Right
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
play_title_animation_rightvarInteger_var3678 = $54
	sta play_title_animation_rightvarInteger_var3678
	sty play_title_animation_rightvarInteger_var3678+1
	; Generic 16 bit op
	ldy #0
	ldx #0 ; Fake 24 bit
	lda MYSCREEN_WIDTH
play_title_animation_rightvarInteger_var3681 = $56
	sta play_title_animation_rightvarInteger_var3681
	sty play_title_animation_rightvarInteger_var3681+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var3684 = $58
	sta play_title_animation_rightvarInteger_var3684
	sty play_title_animation_rightvarInteger_var3684+1
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy #0
	lda pta_i
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype:  INTEGER
	lda MYSCREEN_WIDTH
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var3684
play_title_animation_wordAdd3682
	sta play_title_animation_rightvarInteger_var3684
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3684+1
	tay
	lda play_title_animation_rightvarInteger_var3684
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var3681
play_title_animation_wordAdd3679
	sta play_title_animation_rightvarInteger_var3681
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3681+1
	tay
	lda play_title_animation_rightvarInteger_var3681
	; Low bit binop:
	sec
	sbc play_title_animation_rightvarInteger_var3678
play_title_animation_wordAdd3676
	sta play_title_animation_rightvarInteger_var3678
	; High-bit binop
	tya
	sbc play_title_animation_rightvarInteger_var3678+1
	tay
	lda play_title_animation_rightvarInteger_var3678
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock3687
play_title_animation_ConditionalTrueBlock3686: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3688
play_title_animation_elseblock3687
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3688
play_title_animation_loopstart3633
	; Test Inc dec D
	inc pta_i
	; Forcetype: NADA
	lda #$18
	cmp pta_i ;keep
	beq play_title_animation_loopdone3693
play_title_animation_loopnotdone3694
	jmp play_title_animation_forloop3632
play_title_animation_loopdone3693
play_title_animation_loopend3634
	
; // Reset animation counter 
	lda DEF_ANIM_SPEED
	; Calling storevariable on generic assign expression
	sta anim_speed
play_title_animation_elsedoneblock3463
	; Test Inc dec D
	dec anim_speed
	rts
end_procedure_play_title_animation
	
; // Runs on IRQ
; // I must be doing something wrong here, stack seems to get corrupted
	; NodeProcedureDecl -1
	; ***********  Defining procedure : do_irq_animation
	;    Procedure type : User-defined procedure
do_irq_animation
	; ****** Inline assembler section
	pha
	txa
	pha
	tya
	pha
	
	
; // Play animation
	jsr play_title_animation
	
; // Play music inside the cycle_text() function instead for 2001 compatibility
; //play_music();
	; Test Inc dec D
	inc tmp_irq
	
; // DEBUG counter
	; MoveTo optimization
	lda #$29
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	ldy #0
	lda tmp_irq
	sta ipd_div_lo
	sty ipd_div_hi
	; Forcetype: NADA
	ldy #$2 ; optimized, look out for bugs
do_irq_animation_printdecimal3697
	jsr init_printdecimal_div10 
	ora #$30
	sta (screenmemory),y
	dey
	bpl do_irq_animation_printdecimal3697
	; ****** Inline assembler section
	pla 
	tay
	pla 
	tax
	pla
	jmp(Interrupts_org_irq)
	
	rts
end_procedure_do_irq_animation
	
; // Routine to animate & play music via raster interrupt
; // NOTE: It seems only one interrupt can be running at a given time
; // NOTE: Raster not available on PET 2001 - use only for non-critical functions.
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init_irq_animation
	;    Procedure type : User-defined procedure
init_irq_animation
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp_irq
	
; // Set interrupt function & enable
	; Forcetype: NADA
	lda #$3c
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	lda #<do_irq_animation
	ldy #>do_irq_animation
	; Calling storevariable on generic assign expression
	sta Interrupts_addr
	sty Interrupts_addr+1
	jsr Interrupts_RasterIRQ
	; Forcetype: NADA
	lda #$3d
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	rts
end_procedure_init_irq_animation
	
; // crash visual & sound effect
	; NodeProcedureDecl -1
	; ***********  Defining procedure : player_crash
	;    Procedure type : User-defined procedure
player_crash
	
; // IDEA: Explode in all directions:
; //
; //  *  *  *
; //   * * *
; //    ***
; //  *******
; //    *** 
; //   * * *
; //  *  *  *
; // Animate crash & make sound
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	sta i
player_crash_forloop3703
	
; // Increment animation		
	lda tmp
	clc
	adc #$01
	sta tmp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc player_crash_WordAdd3731
	inc tmp+1
player_crash_WordAdd3731
	; Binary clause INTEGER: GREATER
	lda tmp+1   ; compare high bytes
	cmp #$00 ;keep
	bcc player_crash_elsedoneblock3735
	bne player_crash_ConditionalTrueBlock3733
	lda tmp
	cmp #$02 ;keep
	bcc player_crash_elsedoneblock3735
	beq player_crash_elsedoneblock3735
player_crash_ConditionalTrueBlock3733: ;Main true block ;keep 
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
player_crash_elsedoneblock3735
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock3741
player_crash_ConditionalTrueBlock3739: ;Main true block ;keep 
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc player_1_xy
	sta screen_loc_work+0
	lda screen_loc+1
	adc player_1_xy+1
	sta screen_loc_work+1
	; Load Byte array
	; CAST type NADA
	ldx tmp
	lda crash_anim_arr,x 
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
player_crash_elsedoneblock3741
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock3749
player_crash_ConditionalTrueBlock3747: ;Main true block ;keep 
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc player_2_xy
	sta screen_loc_work+0
	lda screen_loc+1
	adc player_2_xy+1
	sta screen_loc_work+1
	; Load Byte array
	; CAST type NADA
	ldx tmp
	lda crash_anim_arr,x 
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
player_crash_elsedoneblock3749
	
; // Make sound
	lda i
	; Calling storevariable on generic assign expression
	sta $e848
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
player_crash_loopstart3704
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda i
	clc
	; Forcetype: NADA
	adc #$ff
	sta i
	; Forcetype: NADA
	lda #$7f
	cmp i ;keep
	beq player_crash_loopdone3754
player_crash_loopnotdone3755
	jmp player_crash_forloop3703
player_crash_loopdone3754
player_crash_loopend3705
	rts
end_procedure_player_crash
	
; // init engine sound
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init_engine_sound
	;    Procedure type : User-defined procedure
init_engine_sound
	
; // enable sound
	; Assigning memory location
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta $e84b
	
; // set octave
	; Assigning memory location
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	
; // set pitch 
	; Forcetype: NADA
	lda #$c8
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jsr alternate_engine_sound
	rts
end_procedure_init_engine_sound
	
; // alt engine sound
	; NodeProcedureDecl -1
	; ***********  Defining procedure : alternate_engine_sound
	;    Procedure type : User-defined procedure
alternate_engine_sound
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq alternate_engine_sound_localfailed3764
	jmp alternate_engine_sound_ConditionalTrueBlock3759
alternate_engine_sound_localfailed3764: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq alternate_engine_sound_elseblock3760
alternate_engine_sound_ConditionalTrueBlock3759: ;Main true block ;keep 
	
; // Higher octave when turbo is engaged
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	jmp alternate_engine_sound_elsedoneblock3761
alternate_engine_sound_elseblock3760
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
alternate_engine_sound_elsedoneblock3761
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c8;keep
	bne alternate_engine_sound_elseblock3769
alternate_engine_sound_ConditionalTrueBlock3768: ;Main true block ;keep 
	
; // Iterate through several pitch values
	; Forcetype: NADA
	lda #$cd
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock3770
alternate_engine_sound_elseblock3769
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$cd;keep
	bne alternate_engine_sound_elseblock3797
alternate_engine_sound_ConditionalTrueBlock3796: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c3
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock3798
alternate_engine_sound_elseblock3797
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c3;keep
	bne alternate_engine_sound_elsedoneblock3812
alternate_engine_sound_ConditionalTrueBlock3810: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c8
	; Calling storevariable on generic assign expression
	sta sound_pitch
alternate_engine_sound_elsedoneblock3812
alternate_engine_sound_elsedoneblock3798
alternate_engine_sound_elsedoneblock3770
	
; // Set the pitch
	; Assigning memory location
	lda sound_pitch
	; Calling storevariable on generic assign expression
	sta $e848
	rts
end_procedure_alternate_engine_sound
	
; // sound stop
	; NodeProcedureDecl -1
	; ***********  Defining procedure : stop_sound
	;    Procedure type : User-defined procedure
stop_sound
	; Assigning memory location
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta $e84b
	rts
end_procedure_stop_sound
	
; // eof
; // === main logic ===================================
	; NodeProcedureDecl -1
	; ***********  Defining procedure : game_loop
	;    Procedure type : User-defined procedure
game_loop
game_loop_while3817
game_loop_loopstart3821
	; Binary clause Simplified: EQUALS
	clc
	lda game_over_flag
	; cmp #$00 ignored
	bne game_loop_elsedoneblock3820
game_loop_ConditionalTrueBlock3818: ;Main true block ;keep 
	
; // check input devices
	jsr check_input
	
; // update positions
	jsr update_positions
	
; // go vroom
	jsr alternate_engine_sound
	
; // check collisions
	jsr check_collisions
	
; // game state
	jsr check_game_state
	
; // update screen
	jsr update_screen
	; Binary clause Simplified: EQUALS
	lda game_over_flag
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne game_loop_elsedoneblock3834
game_loop_ConditionalTrueBlock3832: ;Main true block ;keep 
	rts
game_loop_elsedoneblock3834
	
; // Slow it down
	lda game_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	jmp game_loop_while3817
game_loop_elsedoneblock3820
game_loop_loopend3822
	rts
end_procedure_game_loop
block1
main_block_begin_
	
; // Init
; // Check PET model being used
	jsr check_model
	
; // Check if 40 or 80 column screen
	jsr cursor_graphic
	
; // Eliminates space between rows
	; Forcetype: NADA
	lda #$3c
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	lda #$00
	ldx #$80
	sta screen_loc
	stx screen_loc+1
	jsr set_uppercase
MainProgram_while3838
MainProgram_loopstart3842
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock3841
MainProgram_ConditionalTrueBlock3839: ;Main true block ;keep 
	
; // Primary loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_over_flag
	jsr title_screen
	jsr game_screen
	jsr game_loop
	jmp MainProgram_while3838
MainProgram_elsedoneblock3841
MainProgram_loopend3843
main_block_end_
	; End of program
	; Ending memory block at $410
cycle_text_input_handler_printstring_text1109	dc.b	"BUSINESS"
	dc.b	0
cycle_text_input_handler_printstring_text1112	dc.b	"GRAPHICS"
	dc.b	0
cycle_text_input_handler_printstring_text1114	dc.b	"         "
	dc.b	0
cycle_text_input_handler_stringassignstr1072		dc.b	"                              "
	dc.b	0
EndBlock410:

