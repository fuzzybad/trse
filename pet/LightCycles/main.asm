
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
MYSCREEN_WIDTH	dc.b	$28
DEF_GAME_SPEED	dc.b	$32
DEF_ANIM_SPEED	dc.b	$06
DEF_SCROLL_SPEED	dc.b	$10
screen_loc	=            $6A
screen_loc_work	=            $6C
message_ptr	=            $6E
song_ptr	=            $70
i	dc.b	$00
j	dc.b	$00
k	dc.b	$00
tmp	dc.w	$00
tmp_irq	dc.b	$00
message_len	dc.b	$00
temp_byte	dc.b	$00
game_mode	dc.b	$00
game_over_flag	dc.b	$00
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
title_msg_0		dc.b	17
	dc.b	17
	dc.b	18
	dc.b	"GREETINGS"
	dc.b	146
	dc.b	" "
	dc.b	18
	dc.b	"PROGRAMS!!"
	dc.b	0
title_msg_1		dc.b	17
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
msg_tvs_player		dc.b	"<<< "
	dc.b	18
	dc.b	"TWO V.SARK"
	dc.b	146
	dc.b	" <<<"
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
	; ***********  Defining procedure : check_model
	;    Procedure type : User-defined procedure
check_model
	jsr Model_Detect
	; Binary clause Simplified: EQUALS
	clc
	jsr Model_Is_40Col
	; cmp #$00 ignored
	bne check_model_elsedoneblock49
check_model_ConditionalTrueBlock47: ;Main true block ;keep 
	
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
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta DEF_GAME_SPEED
	; Forcetype: NADA
	lda #$6
	; Calling storevariable on generic assign expression
	sta DEF_ANIM_SPEED
	; Forcetype: NADA
	lda #$8
	; Calling storevariable on generic assign expression
	sta DEF_SCROLL_SPEED
	lda DEF_GAME_SPEED
	; Calling storevariable on generic assign expression
	sta game_speed
	lda DEF_ANIM_SPEED
	; Calling storevariable on generic assign expression
	sta anim_speed
	lda DEF_SCROLL_SPEED
	; Calling storevariable on generic assign expression
	sta scroll_speed
check_model_elsedoneblock49
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
cls_clearloop53
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne cls_clearloop53
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
_ptr	=            $72
_center_txt	dc.b	0
_mylen	dc.b	0
basic_print_block63
basic_print
	; Binary clause Simplified: NOTEQUALS
	clc
	lda _center_txt
	; cmp #$00 ignored
	beq basic_print_elsedoneblock67
basic_print_ConditionalTrueBlock65: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda _mylen
	; Compare with pure num / var optimization
	cmp MYSCREEN_WIDTH;keep
	bcs basic_print_elsedoneblock91
basic_print_ConditionalTrueBlock89: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta bp_i
basic_print_forloop100
	
; // Center text
	jsr cursor_right
basic_print_loopstart101
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
	bne basic_print_forloop100
basic_print_loopdone105: ;keep
basic_print_loopend102
basic_print_elsedoneblock91
basic_print_elsedoneblock67
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta next_ch
basic_print_while106
basic_print_loopstart110
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	; cmp #$00 ignored
	beq basic_print_elsedoneblock109
basic_print_ConditionalTrueBlock107: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc next_ch
	jmp basic_print_while106
basic_print_elsedoneblock109
basic_print_loopend111
	jsr cursor_return
	rts
end_procedure_basic_print
	
; // Print using KERNAL routine at X,Y location
	; NodeProcedureDecl -1
	; ***********  Defining procedure : basic_printat
	;    Procedure type : User-defined procedure
pa_next_ch	dc.b	0
pa_i	dc.b	0
_pa_ptr	=            $72
_pa_myx	dc.b	0
_pa_myy	dc.b	0
basic_printat_block114
basic_printat
	
; // Home cursor
	jsr cursor_home
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myx
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock118
basic_printat_ConditionalTrueBlock116: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop127
	
; // Cursor right
	jsr cursor_right
basic_printat_loopstart128
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myx
	cmp pa_i ;keep
	bne basic_printat_forloop127
basic_printat_loopdone132: ;keep
basic_printat_loopend129
basic_printat_elsedoneblock118
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myy
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock136
basic_printat_ConditionalTrueBlock134: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop145
	
; // Cursor down
	jsr cursor_down
basic_printat_loopstart146
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myy
	cmp pa_i ;keep
	bne basic_printat_forloop145
basic_printat_loopdone150: ;keep
basic_printat_loopend147
basic_printat_elsedoneblock136
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_next_ch
basic_printat_while151
basic_printat_loopstart155
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	; cmp #$00 ignored
	beq basic_printat_elsedoneblock154
basic_printat_ConditionalTrueBlock152: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc pa_next_ch
	jmp basic_printat_while151
basic_printat_elsedoneblock154
basic_printat_loopend156
	rts
end_procedure_basic_printat
	
; // Delay function
	; NodeProcedureDecl -1
	; ***********  Defining procedure : do_delay
	;    Procedure type : User-defined procedure
dd_i	dc.b	0
delay_val	dc.b	0
do_delay_block159
do_delay
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda delay_val
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc do_delay_elsedoneblock163
do_delay_ConditionalTrueBlock161: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dd_i
do_delay_forloop172
	; Wait
	; Forcetype: NADA
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
do_delay_loopstart173
	; Compare is onpage
	; Test Inc dec D
	inc dd_i
	lda delay_val
	cmp dd_i ;keep
	bne do_delay_forloop172
do_delay_loopdone177: ;keep
do_delay_loopend174
do_delay_elsedoneblock163
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
	bne play_music_elsedoneblock182
play_music_ConditionalTrueBlock180: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	; Load pointer array
	ldy music_idx
	lda (song_ptr),y
	; cmp #$00 ignored
	bne play_music_elsedoneblock195
play_music_localsuccess197: ;keep
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
	bne play_music_elsedoneblock195
play_music_ConditionalTrueBlock193: ;Main true block ;keep 
	
; // Loop over note array
; // 	Structure - note value, octave, sustain value 
; // Change note when index is zero
; // End tune when pitch & sustain both = 0
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta music_idx
play_music_elsedoneblock195
	
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
play_music_elsedoneblock182
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
cycle_text_block199
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
	bne cycle_text_elseblock202
cycle_text_ConditionalTrueBlock201: ;Main true block ;keep 
	
; // Value of zero means we cycle forever
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scroll_step
	jmp cycle_text_elsedoneblock203
cycle_text_elseblock202
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_step
cycle_text_elsedoneblock203
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock211
cycle_text_ConditionalTrueBlock209: ;Main true block ;keep 
	
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
cycle_text_elsedoneblock211
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta k
cycle_text_forloop214
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_dir
	; cmp #$00 ignored
	beq cycle_text_elseblock235
cycle_text_ConditionalTrueBlock234: ;Main true block ;keep 
	
; // Scroll for the given # cycles
	jsr cycle_text_left
	jmp cycle_text_elsedoneblock236
cycle_text_elseblock235
	jsr cycle_text_right
cycle_text_elsedoneblock236
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_inp_brk
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock244
cycle_text_ConditionalTrueBlock242: ;Main true block ;keep 
	
; // Break on input
	rts
cycle_text_elsedoneblock244
cycle_text_loopstart215
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
	bne cycle_text_forloop214
cycle_text_loopdone247: ;keep
cycle_text_loopend216
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
cycle_text_right_forloop249
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
cycle_text_right_rightvarInteger_var311 = $54
	sta cycle_text_right_rightvarInteger_var311
	sty cycle_text_right_rightvarInteger_var311+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_right_skip313
	iny
cycle_text_right_skip313
	; Low bit binop:
	clc
	adc cycle_text_right_rightvarInteger_var311
cycle_text_right_wordAdd309
	sta cycle_text_right_rightvarInteger_var311
	; High-bit binop
	tya
	adc cycle_text_right_rightvarInteger_var311+1
	tay
	lda cycle_text_right_rightvarInteger_var311
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
	bcs cycle_text_right_elseblock316
cycle_text_right_ConditionalTrueBlock315: ;Main true block ;keep 
	
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
	jmp cycle_text_right_elsedoneblock317
cycle_text_right_elseblock316
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
cycle_text_right_elsedoneblock317
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock325
cycle_text_right_ConditionalTrueBlock323: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_elsedoneblock325
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock331
cycle_text_right_ConditionalTrueBlock329: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock349
cycle_text_right_ConditionalTrueBlock347: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_right_elsedoneblock349
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock355
cycle_text_right_ConditionalTrueBlock353: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_right_elsedoneblock355
cycle_text_right_elsedoneblock331
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock361
cycle_text_right_ConditionalTrueBlock359: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_right_elsedoneblock361
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_right_loopstart250
	; Test Inc dec D
	inc i
	lda scroll_num_char
	cmp i ;keep
	beq cycle_text_right_loopdone364
cycle_text_right_loopnotdone365
	jmp cycle_text_right_forloop249
cycle_text_right_loopdone364
cycle_text_right_loopend251
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
cycle_text_left_forloop367
	
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
cycle_text_left_rightvarInteger_var429 = $54
	sta cycle_text_left_rightvarInteger_var429
	sty cycle_text_left_rightvarInteger_var429+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_left_skip431
	iny
cycle_text_left_skip431
	; Low bit binop:
	clc
	adc cycle_text_left_rightvarInteger_var429
cycle_text_left_wordAdd427
	sta cycle_text_left_rightvarInteger_var429
	; High-bit binop
	tya
	adc cycle_text_left_rightvarInteger_var429+1
	tay
	lda cycle_text_left_rightvarInteger_var429
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
	bcs cycle_text_left_elseblock434
cycle_text_left_ConditionalTrueBlock433: ;Main true block ;keep 
	
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
	jmp cycle_text_left_elsedoneblock435
cycle_text_left_elseblock434
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
cycle_text_left_elsedoneblock435
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock443
cycle_text_left_ConditionalTrueBlock441: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_left_elsedoneblock443
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock449
cycle_text_left_ConditionalTrueBlock447: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_left_elsedoneblock449
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock455
cycle_text_left_ConditionalTrueBlock453: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock473
cycle_text_left_ConditionalTrueBlock471: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_left_elsedoneblock473
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock479
cycle_text_left_ConditionalTrueBlock477: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_left_elsedoneblock479
cycle_text_left_elsedoneblock455
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_left_loopstart368
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
	beq cycle_text_left_loopdone482
cycle_text_left_loopnotdone483
	jmp cycle_text_left_forloop367
cycle_text_left_loopdone482
cycle_text_left_loopend369
	rts
end_procedure_cycle_text_left
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_input
	;    Procedure type : User-defined procedure
check_input_val	dc.b	0
check_input_block484
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
	bne check_input_elsedoneblock488
check_input_ConditionalTrueBlock486: ;Main true block ;keep 
	
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
check_input_elsedoneblock488
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
	beq check_input_elsedoneblock494
check_input_ConditionalTrueBlock492: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock494
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
	beq check_input_elsedoneblock500
check_input_ConditionalTrueBlock498: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock500
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
	beq check_input_elsedoneblock506
check_input_ConditionalTrueBlock504: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock506
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
	beq check_input_elsedoneblock512
check_input_ConditionalTrueBlock510: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock512
	; Binary clause Simplified: EQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$30
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bne check_input_elsedoneblock518
check_input_ConditionalTrueBlock516: ;Main true block ;keep 
	
; // Down
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock518
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
	beq check_input_elsedoneblock524
check_input_ConditionalTrueBlock522: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock524
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
	beq check_input_elsedoneblock530
check_input_ConditionalTrueBlock528: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock530
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
	beq check_input_elsedoneblock536
check_input_ConditionalTrueBlock534: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock536
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
	beq check_input_elsedoneblock542
check_input_ConditionalTrueBlock540: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock542
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta j
check_input_forloop545
	
; // Down
; // Scan keyboard rows and process input
; // 	- Apparently TRSE loops don't run the last item?!
	lda j
	; Calling storevariable on generic assign expression
	sta $e810
	; 8 bit binop
	; Add/sub where right value is constant number
	; Forcetype: NADA
	lda #$ff
	sec
	; Forcetype: NADA
	sbc $e812
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta check_input_val
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne check_input_elsedoneblock703
check_input_ConditionalTrueBlock701: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock721
check_input_ConditionalTrueBlock719: ;Main true block ;keep 
	
; //		if j = 0 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; //		if j = 1 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; //		if j = 2 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; // W - P1 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock721
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock727
check_input_ConditionalTrueBlock725: ;Main true block ;keep 
	
; // 8 - P2 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock727
check_input_elsedoneblock703
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne check_input_elsedoneblock733
check_input_ConditionalTrueBlock731: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock763
check_input_ConditionalTrueBlock761: ;Main true block ;keep 
	
; // A - P1 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock763
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$2
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock769
check_input_ConditionalTrueBlock767: ;Main true block ;keep 
	
; // D - P1 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock769
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock775
check_input_ConditionalTrueBlock773: ;Main true block ;keep 
	
; // 4 - P2 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock775
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$80
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock781
check_input_ConditionalTrueBlock779: ;Main true block ;keep 
	
; // 6 - P2 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock781
check_input_elsedoneblock733
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$5;keep
	bne check_input_elsedoneblock787
check_input_ConditionalTrueBlock785: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock805
check_input_ConditionalTrueBlock803: ;Main true block ;keep 
	
; // S - P1 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock805
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock811
check_input_ConditionalTrueBlock809: ;Main true block ;keep 
	
; // 5 - P2 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock811
check_input_elsedoneblock787
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$8;keep
	bne check_input_elsedoneblock817
check_input_ConditionalTrueBlock815: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$20
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock829
check_input_ConditionalTrueBlock827: ;Main true block ;keep 
	
; //		if j = 6 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; //		if j = 7 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; // RIGHT SHIFT - P1 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock829
check_input_elsedoneblock817
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$9;keep
	bne check_input_elsedoneblock835
check_input_ConditionalTrueBlock833: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$4
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock847
check_input_ConditionalTrueBlock845: ;Main true block ;keep 
	
; // SPACE - P2 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock847
check_input_elsedoneblock835
check_input_loopstart546
	; Test Inc dec D
	inc j
	; Forcetype: NADA
	lda #$a
	cmp j ;keep
	beq check_input_loopdone850
check_input_loopnotdone851
	jmp check_input_forloop545
check_input_loopdone850
check_input_loopend547
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_input_elsedoneblock855
check_input_ConditionalTrueBlock853: ;Main true block ;keep 
	
; // Get Sark's move when in single-player mode
	jsr sark_move
check_input_elsedoneblock855
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_localfailed864
	jmp check_input_ConditionalTrueBlock859
check_input_localfailed864: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elseblock860
check_input_ConditionalTrueBlock859: ;Main true block ;keep 
	
; // getin - read kernal keyboard input
; //call(#$FFE4); 
; //check_input_val := _A;
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(check_input_val,2);
	; Forcetype: NADA
	lda #$1
	rts
	jmp check_input_elsedoneblock861
check_input_elseblock860
	; Forcetype: NADA
	lda #$0
	rts
check_input_elsedoneblock861
	rts
end_procedure_check_input
	
; // Displays game mode
	; NodeProcedureDecl -1
	; ***********  Defining procedure : display_game_mode
	;    Procedure type : User-defined procedure
display_game_mode
	; MoveTo optimization
	lda #$2b
	sta screenmemory
	lda #>$8000
	clc
	adc #$03
	sta screenmemory+1
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
display_game_mode_int_shift_var869 = $54
	sta display_game_mode_int_shift_var869
	sty display_game_mode_int_shift_var869+1
		lsr display_game_mode_int_shift_var869+1
	ror display_game_mode_int_shift_var869+0

	lda display_game_mode_int_shift_var869
	ldy display_game_mode_int_shift_var869+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne display_game_mode_elsedoneblock873
display_game_mode_ConditionalTrueBlock871: ;Main true block ;keep 
	lda #<msg_one_player
	ldx #>msg_one_player
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
display_game_mode_elsedoneblock873
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne display_game_mode_elsedoneblock879
display_game_mode_ConditionalTrueBlock877: ;Main true block ;keep 
	lda #<msg_two_player
	ldx #>msg_two_player
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
display_game_mode_elsedoneblock879
	rts
end_procedure_display_game_mode
	
; //if game_mode = 2 then basic_printat(#msg_tvs_player,tmp,20);
; // Handle additional input during cycle text routine
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cycle_text_input_handler
	;    Procedure type : User-defined procedure
cycle_text_input_handler
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_localfailed888
	jmp cycle_text_input_handler_ConditionalTrueBlock884
cycle_text_input_handler_localfailed888: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_elsedoneblock886
cycle_text_input_handler_ConditionalTrueBlock884: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // one player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock886
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_localfailed895
	jmp cycle_text_input_handler_ConditionalTrueBlock891
cycle_text_input_handler_localfailed895: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_elsedoneblock893
cycle_text_input_handler_ConditionalTrueBlock891: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // two player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock893
	
; //	if (player_1_input = 1 or player_2_input = 1) then game_mode := game_mode -1;
; //	if (player_1_input = 2 or player_2_input = 2) then game_mode := game_mode +1;
; //	if (game_mode = 255) then game_mode := 0;
; //	if (game_mode > 2) then game_mode := 2;
; //	display_game_mode();
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
update_score_block897
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
	beq update_score_elsedoneblock901
update_score_ConditionalTrueBlock899: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
update_score_forloop934
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock954
update_score_ConditionalTrueBlock952: ;Main true block ;keep 
	
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
update_score_elsedoneblock954
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock960
update_score_ConditionalTrueBlock958: ;Main true block ;keep 
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
update_score_elsedoneblock960
	
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
update_score_loopstart935
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$6
	cmp i ;keep
	bne update_score_forloop934
update_score_loopdone963: ;keep
update_score_loopend936
update_score_elsedoneblock901
	rts
end_procedure_update_score
	
; // Draw box around title screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : draw_title_screen_box
	;    Procedure type : User-defined procedure
dtsb_tmp	dc.b	0
dtsb_i	dc.b	0
draw_title_screen_box_block964
draw_title_screen_box
	
; // Draw top and bottom	
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var967 = $54
	sta draw_title_screen_box_rightvarInteger_var967
	sty draw_title_screen_box_rightvarInteger_var967+1
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
	adc draw_title_screen_box_rightvarInteger_var967
draw_title_screen_box_wordAdd965
	sta draw_title_screen_box_rightvarInteger_var967
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var967+1
	tay
	lda draw_title_screen_box_rightvarInteger_var967
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dtsb_i
draw_title_screen_box_forloop968
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
draw_title_screen_box_loopstart969
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
	bne draw_title_screen_box_forloop968
draw_title_screen_box_loopdone973: ;keep
draw_title_screen_box_loopend970
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dtsb_i
draw_title_screen_box_forloop974
	
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
draw_title_screen_box_rightvarInteger_var984 = $54
	sta draw_title_screen_box_rightvarInteger_var984
	sty draw_title_screen_box_rightvarInteger_var984+1
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
	adc draw_title_screen_box_rightvarInteger_var984
draw_title_screen_box_wordAdd982
	sta draw_title_screen_box_rightvarInteger_var984
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var984+1
	tay
	lda draw_title_screen_box_rightvarInteger_var984
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
draw_title_screen_box_loopstart975
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
	beq draw_title_screen_box_loopdone985
draw_title_screen_box_loopnotdone986
	jmp draw_title_screen_box_forloop974
draw_title_screen_box_loopdone985
draw_title_screen_box_loopend976
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
	lda #<title_msg_0
	ldx #>title_msg_0
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
title_screen_int_shift_var989 = $54
	sta title_screen_int_shift_var989
	sty title_screen_int_shift_var989+1
		lsr title_screen_int_shift_var989+1
	ror title_screen_int_shift_var989+0

	lda title_screen_int_shift_var989
	ldy title_screen_int_shift_var989+1
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
draw_game_screen_box_forloop992
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart993
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda MYSCREEN_WIDTH
	cmp i ;keep
	bne draw_game_screen_box_forloop992
draw_game_screen_box_loopdone997: ;keep
draw_game_screen_box_loopend994
	
; // Draw bottom line
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1000 = $54
	sta draw_game_screen_box_rightvarInteger_var1000
	sty draw_game_screen_box_rightvarInteger_var1000+1
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
	adc draw_game_screen_box_rightvarInteger_var1000
draw_game_screen_box_wordAdd998
	sta draw_game_screen_box_rightvarInteger_var1000
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1000+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1000
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop1001
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart1002
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda MYSCREEN_WIDTH
	cmp i ;keep
	bne draw_game_screen_box_forloop1001
draw_game_screen_box_loopdone1006: ;keep
draw_game_screen_box_loopend1003
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop1007
	
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
draw_game_screen_box_rightvarInteger_var1020 = $54
	sta draw_game_screen_box_rightvarInteger_var1020
	sty draw_game_screen_box_rightvarInteger_var1020+1
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
	adc draw_game_screen_box_rightvarInteger_var1020
draw_game_screen_box_wordAdd1018
	sta draw_game_screen_box_rightvarInteger_var1020
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1020+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1020
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
draw_game_screen_box_loopstart1008
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_game_screen_box_forloop1007
draw_game_screen_box_loopdone1022: ;keep
draw_game_screen_box_loopend1009
	
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
draw_game_screen_box_rightvarInteger_var1026 = $54
	sta draw_game_screen_box_rightvarInteger_var1026
	sty draw_game_screen_box_rightvarInteger_var1026+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1029 =            $56
	sta draw_game_screen_box_rightvarInteger_var1029
	sty draw_game_screen_box_rightvarInteger_var1029+1
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
	adc draw_game_screen_box_rightvarInteger_var1029
draw_game_screen_box_wordAdd1027
	sta draw_game_screen_box_rightvarInteger_var1029
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1029+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1029
	; Low bit binop:
	sec
	sbc draw_game_screen_box_rightvarInteger_var1026
draw_game_screen_box_wordAdd1024
	sta draw_game_screen_box_rightvarInteger_var1026
	; High-bit binop
	tya
	sbc draw_game_screen_box_rightvarInteger_var1026+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1026
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
draw_game_screen_box_rightvarInteger_var1032 = $54
	sta draw_game_screen_box_rightvarInteger_var1032
	sty draw_game_screen_box_rightvarInteger_var1032+1
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
	adc draw_game_screen_box_rightvarInteger_var1032
draw_game_screen_box_wordAdd1030
	sta draw_game_screen_box_rightvarInteger_var1032
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1032+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1032
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
draw_game_screen_box_rightvarInteger_var1035 = $54
	sta draw_game_screen_box_rightvarInteger_var1035
	sty draw_game_screen_box_rightvarInteger_var1035+1
	; Generic 16 bit op
	ldy #0
	ldx #0 ; Fake 24 bit
	lda MYSCREEN_WIDTH
draw_game_screen_box_rightvarInteger_var1038 =            $56
	sta draw_game_screen_box_rightvarInteger_var1038
	sty draw_game_screen_box_rightvarInteger_var1038+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1041 =            $58
	sta draw_game_screen_box_rightvarInteger_var1041
	sty draw_game_screen_box_rightvarInteger_var1041+1
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
	adc draw_game_screen_box_rightvarInteger_var1041
draw_game_screen_box_wordAdd1039
	sta draw_game_screen_box_rightvarInteger_var1041
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1041+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1041
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1038
draw_game_screen_box_wordAdd1036
	sta draw_game_screen_box_rightvarInteger_var1038
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1038+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1038
	; Low bit binop:
	sec
	sbc draw_game_screen_box_rightvarInteger_var1035
draw_game_screen_box_wordAdd1033
	sta draw_game_screen_box_rightvarInteger_var1035
	; High-bit binop
	tya
	sbc draw_game_screen_box_rightvarInteger_var1035+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1035
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
	bne game_screen_elsedoneblock1046
game_screen_ConditionalTrueBlock1044: ;Main true block ;keep 
	
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
game_screen_elsedoneblock1046
	
; // string, x, y
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne game_screen_elsedoneblock1054
game_screen_ConditionalTrueBlock1052: ;Main true block ;keep 
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
game_screen_elsedoneblock1054
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
	bcs new_round_skip1061
	dey
new_round_skip1061
new_round_int_shift_var1062 = $54
	sta new_round_int_shift_var1062
	sty new_round_int_shift_var1062+1
		lsr new_round_int_shift_var1062+1
	ror new_round_int_shift_var1062+0

	lda new_round_int_shift_var1062
	ldy new_round_int_shift_var1062+1
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
	bcs new_round_skip1064
	dey
new_round_skip1064
new_round_int_shift_var1065 = $54
	sta new_round_int_shift_var1065
	sty new_round_int_shift_var1065+1
		lsr new_round_int_shift_var1065+1
	ror new_round_int_shift_var1065+0

	lda new_round_int_shift_var1065
	ldy new_round_int_shift_var1065+1
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
	bne new_round_localfailed1071
	jmp new_round_ConditionalTrueBlock1067
new_round_localfailed1071: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_elsedoneblock1069
new_round_ConditionalTrueBlock1067: ;Main true block ;keep 
	
; // Adjust game speed
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$a
	sta game_speed
new_round_elsedoneblock1069
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_localfailed1078
	jmp new_round_ConditionalTrueBlock1074
new_round_localfailed1078: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_elsedoneblock1076
new_round_ConditionalTrueBlock1074: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$f
	sta game_speed
new_round_elsedoneblock1076
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_localfailed1085
	jmp new_round_ConditionalTrueBlock1081
new_round_localfailed1085: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_elsedoneblock1083
new_round_ConditionalTrueBlock1081: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$14
	sta game_speed
new_round_elsedoneblock1083
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_localfailed1092
	jmp new_round_ConditionalTrueBlock1088
new_round_localfailed1092: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_elsedoneblock1090
new_round_ConditionalTrueBlock1088: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda DEF_GAME_SPEED
	sec
	; Forcetype: NADA
	sbc #$19
	sta game_speed
new_round_elsedoneblock1090
	
; // Redraw game screen
	jsr game_screen
	rts
end_procedure_new_round
	
; // Start new game
	; NodeProcedureDecl -1
	; ***********  Defining procedure : new_game
	;    Procedure type : User-defined procedure
new_game
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
	bcs new_game_skip1096
	dey
new_game_skip1096
new_game_int_shift_var1097 = $54
	sta new_game_int_shift_var1097
	sty new_game_int_shift_var1097+1
		lsr new_game_int_shift_var1097+1
	ror new_game_int_shift_var1097+0

	lda new_game_int_shift_var1097
	ldy new_game_int_shift_var1097+1
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
end_procedure_new_game
	
; // Game state
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_game_state
	;    Procedure type : User-defined procedure
check_game_state
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1104
	jmp check_game_state_ConditionalTrueBlock1100
check_game_state_localfailed1104: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1102
check_game_state_ConditionalTrueBlock1100: ;Main true block ;keep 
	
; // Check for player crashed
	jsr player_crash
	jsr stop_sound
check_game_state_elsedoneblock1102
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1264
check_game_state_localsuccess1265: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1264
	jmp check_game_state_ConditionalTrueBlock1107
check_game_state_localfailed1264
	jmp check_game_state_elseblock1108
check_game_state_ConditionalTrueBlock1107: ;Main true block ;keep 
	lda #<msg_both_crash
	ldx #>msg_both_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
	jmp check_game_state_elsedoneblock1109
check_game_state_elseblock1108
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock1270
check_game_state_ConditionalTrueBlock1269: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p2
	; Binary clause Simplified: LESS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1350
check_game_state_ConditionalTrueBlock1348: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1363
check_game_state_ConditionalTrueBlock1362: ;Main true block ;keep 
	lda #<msg_plr_crash
	ldx #>msg_plr_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$e
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1364
check_game_state_elseblock1363
	lda #<msg_p1_crash
	ldx #>msg_p1_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1364
	jsr new_round
check_game_state_elsedoneblock1350
	jmp check_game_state_elsedoneblock1271
check_game_state_elseblock1270
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1373
check_game_state_ConditionalTrueBlock1371: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p1
	; Binary clause Simplified: LESS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1401
check_game_state_ConditionalTrueBlock1399: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1414
check_game_state_ConditionalTrueBlock1413: ;Main true block ;keep 
	lda #<msg_sark_crash
	ldx #>msg_sark_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1415
check_game_state_elseblock1414
	lda #<msg_p2_crash
	ldx #>msg_p2_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1415
	jsr new_round
check_game_state_elsedoneblock1401
check_game_state_elsedoneblock1373
check_game_state_elsedoneblock1271
check_game_state_elsedoneblock1109
	; Binary clause Simplified: GREATEREQUAL
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elseblock1422
check_game_state_ConditionalTrueBlock1421: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1459
check_game_state_ConditionalTrueBlock1458: ;Main true block ;keep 
	
; // Check for end of game
	lda #<msg_plr_wins
	ldx #>msg_plr_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$b
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1460
check_game_state_elseblock1459
	lda #<msg_p1_wins
	ldx #>msg_p1_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1460
	jsr new_game
	jmp check_game_state_elsedoneblock1423
check_game_state_elseblock1422
	; Binary clause Simplified: GREATEREQUAL
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elsedoneblock1469
check_game_state_ConditionalTrueBlock1467: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1482
check_game_state_ConditionalTrueBlock1481: ;Main true block ;keep 
	lda #<msg_sark_wins
	ldx #>msg_sark_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$9
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1483
check_game_state_elseblock1482
	lda #<msg_p2_wins
	ldx #>msg_p2_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1483
	jsr new_game
check_game_state_elsedoneblock1469
check_game_state_elsedoneblock1423
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
	bne check_collisions_elsedoneblock1492
	lda player_1_xy
	cmp player_2_xy ;keep
	bne check_collisions_elsedoneblock1492
	jmp check_collisions_ConditionalTrueBlock1490
check_collisions_ConditionalTrueBlock1490: ;Main true block ;keep 
	
; // Both crashed before trail is drawn. This solves edge case 
; // when both players try to occupy the same location.
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1492
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1507
	jmp check_collisions_ConditionalTrueBlock1496
check_collisions_localfailed1507: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1498
check_collisions_ConditionalTrueBlock1496: ;Main true block ;keep 
	
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
	beq check_collisions_elsedoneblock1513
check_collisions_ConditionalTrueBlock1511: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
check_collisions_elsedoneblock1513
check_collisions_elsedoneblock1498
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1528
	jmp check_collisions_ConditionalTrueBlock1517
check_collisions_localfailed1528: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1519
check_collisions_ConditionalTrueBlock1517: ;Main true block ;keep 
	
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
	beq check_collisions_elsedoneblock1534
check_collisions_ConditionalTrueBlock1532: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1534
check_collisions_elsedoneblock1519
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
sark_crash_distance_block1537
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
sark_crash_distance_while1538
sark_crash_distance_loopstart1542
	; Binary clause Simplified: EQUALS
	clc
	lda scd_crash
	; cmp #$00 ignored
	bne sark_crash_distance_elsedoneblock1541
sark_crash_distance_ConditionalTrueBlock1539: ;Main true block ;keep 
	; Test Inc dec D
	inc scd_i
	
; // increment pos			
	; Binary clause Simplified: NOTEQUALS
	clc
	jsr Model_Is_40Col
	; cmp #$00 ignored
	beq sark_crash_distance_elseblock1567
sark_crash_distance_ConditionalTrueBlock1566: ;Main true block ;keep 
	
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
	jmp sark_crash_distance_elsedoneblock1568
sark_crash_distance_elseblock1567
	
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
sark_crash_distance_elsedoneblock1568
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
	beq sark_crash_distance_elsedoneblock1581
sark_crash_distance_ConditionalTrueBlock1579: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scd_crash
sark_crash_distance_elsedoneblock1581
	jmp sark_crash_distance_while1538
sark_crash_distance_elsedoneblock1541
sark_crash_distance_loopend1543
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
sark_check_player_dist_block1584
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
sark_check_player_dist_rightvarInteger_var1588 = $54
	sta sark_check_player_dist_rightvarInteger_var1588
	sty sark_check_player_dist_rightvarInteger_var1588+1
	lda player_2_xy+1
	sec
	sbc sark_check_player_dist_rightvarInteger_var1588+1
	tay
	lda player_2_xy
	sec
	sbc sark_check_player_dist_rightvarInteger_var1588
	bcs sark_check_player_dist_wordAdd1585
	dey
sark_check_player_dist_wordAdd1585
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
sark_check_player_dist_rightvarInteger_var1592 = $54
	sta sark_check_player_dist_rightvarInteger_var1592
	sty sark_check_player_dist_rightvarInteger_var1592+1
	lda player_1_xy+1
	sec
	sbc sark_check_player_dist_rightvarInteger_var1592+1
	tay
	lda player_1_xy
	sec
	sbc sark_check_player_dist_rightvarInteger_var1592
	bcs sark_check_player_dist_wordAdd1589
	dey
sark_check_player_dist_wordAdd1589
	; Calling storevariable on generic assign expression
	sta scpd_player_x
	; Binary clause Simplified: GREATER
	lda scpd_sark_y
	; Compare with pure num / var optimization
	cmp scpd_player_y;keep
	bcc sark_check_player_dist_elseblock1595
	beq sark_check_player_dist_elseblock1595
sark_check_player_dist_ConditionalTrueBlock1594: ;Main true block ;keep 
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
	jmp sark_check_player_dist_elsedoneblock1596
sark_check_player_dist_elseblock1595
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
sark_check_player_dist_elsedoneblock1596
	; Binary clause Simplified: GREATER
	lda scpd_sark_x
	; Compare with pure num / var optimization
	cmp scpd_player_x;keep
	bcc sark_check_player_dist_elseblock1603
	beq sark_check_player_dist_elseblock1603
sark_check_player_dist_ConditionalTrueBlock1602: ;Main true block ;keep 
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
	jmp sark_check_player_dist_elsedoneblock1604
sark_check_player_dist_elseblock1603
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
sark_check_player_dist_elsedoneblock1604
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
sark_move_block1609
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
	bcs sark_move_localfailed2041
	jmp sark_move_ConditionalTrueBlock1611
sark_move_localfailed2041
	jmp sark_move_elsedoneblock1613
sark_move_ConditionalTrueBlock1611: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed2258
	jmp sark_move_ConditionalTrueBlock2044
sark_move_localfailed2258: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_localfailed2257
	jmp sark_move_ConditionalTrueBlock2044
sark_move_localfailed2257
	jmp sark_move_elseblock2045
sark_move_ConditionalTrueBlock2044: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2262
	beq sark_move_elseblock2262
sark_move_localsuccess2295: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2262
	beq sark_move_elseblock2262
sark_move_ConditionalTrueBlock2261: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2299
sark_move_ConditionalTrueBlock2298: ;Main true block ;keep 
	
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
	jmp sark_move_elsedoneblock2300
sark_move_elseblock2299
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2300
	jmp sark_move_elsedoneblock2263
sark_move_elseblock2262
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_dist_d;keep
	bcc sark_move_elseblock2308
	beq sark_move_elseblock2308
sark_move_localsuccess2318: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elseblock2308
	beq sark_move_elseblock2308
sark_move_ConditionalTrueBlock2307: ;Main true block ;keep 
	
; // Take direction with best crash distance
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2309
sark_move_elseblock2308
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elsedoneblock2324
	beq sark_move_elsedoneblock2324
sark_move_ConditionalTrueBlock2322: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2324
sark_move_elsedoneblock2309
sark_move_elsedoneblock2263
	jmp sark_move_elsedoneblock2046
sark_move_elseblock2045
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_localfailed2400
	jmp sark_move_ConditionalTrueBlock2329
sark_move_localfailed2400: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne sark_move_elsedoneblock2331
sark_move_ConditionalTrueBlock2329: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2404
	beq sark_move_elseblock2404
sark_move_localsuccess2437: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2404
	beq sark_move_elseblock2404
sark_move_ConditionalTrueBlock2403: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2441
sark_move_ConditionalTrueBlock2440: ;Main true block ;keep 
	
; // Sark is moving U or D. Check L/R
; // If both alternatives have 'good' crash distance,
; // move in direction closer to player
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2442
sark_move_elseblock2441
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2442
	jmp sark_move_elsedoneblock2405
sark_move_elseblock2404
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_dist_r;keep
	bcc sark_move_elseblock2450
	beq sark_move_elseblock2450
sark_move_localsuccess2460: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elseblock2450
	beq sark_move_elseblock2450
sark_move_ConditionalTrueBlock2449: ;Main true block ;keep 
	
; // Take direction with best crash distance
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2451
sark_move_elseblock2450
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elsedoneblock2466
	beq sark_move_elsedoneblock2466
sark_move_ConditionalTrueBlock2464: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2466
sark_move_elsedoneblock2451
sark_move_elsedoneblock2405
sark_move_elsedoneblock2331
sark_move_elsedoneblock2046
sark_move_elsedoneblock1613
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock2472
sark_move_ConditionalTrueBlock2470: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda sark_near_player_x_dist
	; Compare with pure num / var optimization
	cmp sm_boost_th;keep
	bcs sark_move_elsedoneblock2485
sark_move_localsuccess2487: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	lda sark_near_player_y_dist
	; Compare with pure num / var optimization
	cmp sm_boost_th;keep
	bcs sark_move_elsedoneblock2485
sark_move_ConditionalTrueBlock2483: ;Main true block ;keep 
	
; //	If opponent less than 'n' moves away, engage turbo boost 
; //	unless it would cause a crash.
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
sark_move_elsedoneblock2485
sark_move_elsedoneblock2472
	; Binary clause Simplified: LESSEQUAL
	lda sark_near_player_x_dist
	; Compare with pure num / var optimization
	cmp sm_prox_th;keep
	beq sark_move_localsuccess2601
	bcs sark_move_localfailed2600
sark_move_localsuccess2601: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESSEQUAL
	lda sark_near_player_y_dist
	; Compare with pure num / var optimization
	cmp sm_prox_th;keep
	beq sark_move_ConditionalTrueBlock2490
	bcs sark_move_localfailed2600
	jmp sark_move_ConditionalTrueBlock2490
sark_move_localfailed2600
	jmp sark_move_elsedoneblock2492
sark_move_ConditionalTrueBlock2490: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed2631
	jmp sark_move_ConditionalTrueBlock2604
sark_move_localfailed2631: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_elsedoneblock2606
sark_move_ConditionalTrueBlock2604: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2635
sark_move_localsuccess2646: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elseblock2635
sark_move_ConditionalTrueBlock2634: ;Main true block ;keep 
	
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
	jmp sark_move_elsedoneblock2636
sark_move_elseblock2635
	
; // can't boost & turn
	; Binary clause Simplified: EQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	bne sark_move_elsedoneblock2652
sark_move_localsuccess2654: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock2652
sark_move_ConditionalTrueBlock2650: ;Main true block ;keep 
	
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
sark_move_elsedoneblock2652
sark_move_elsedoneblock2636
sark_move_elsedoneblock2606
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_localfailed2684
	jmp sark_move_ConditionalTrueBlock2657
sark_move_localfailed2684: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne sark_move_elsedoneblock2659
sark_move_ConditionalTrueBlock2657: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2688
sark_move_localsuccess2699: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elseblock2688
sark_move_ConditionalTrueBlock2687: ;Main true block ;keep 
	
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
	jmp sark_move_elsedoneblock2689
sark_move_elseblock2688
	
; // can't boost & turn
	; Binary clause Simplified: EQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	bne sark_move_elsedoneblock2705
sark_move_localsuccess2707: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock2705
sark_move_ConditionalTrueBlock2703: ;Main true block ;keep 
	
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
sark_move_elsedoneblock2705
sark_move_elsedoneblock2689
sark_move_elsedoneblock2659
sark_move_elsedoneblock2492
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
	bcc update_positions_elsedoneblock2713
update_positions_ConditionalTrueBlock2711: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta turn_counter
update_positions_elsedoneblock2713
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed2783
	jmp update_positions_ConditionalTrueBlock2717
update_positions_localfailed2783: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed2782
	jmp update_positions_ConditionalTrueBlock2717
update_positions_localfailed2782
	jmp update_positions_elsedoneblock2719
update_positions_ConditionalTrueBlock2717: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock2788
update_positions_ConditionalTrueBlock2786: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2801
update_positions_localsuccess2803: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_1_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_1_input;keep
	beq update_positions_elsedoneblock2801
update_positions_ConditionalTrueBlock2799: ;Main true block ;keep 
	
; // May need to tweak this value
; // Move P1 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_1_input
	; Calling storevariable on generic assign expression
	sta player_1_head
update_positions_elsedoneblock2801
update_positions_elsedoneblock2788
	
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
	beq update_positions_elseblock2808
update_positions_ConditionalTrueBlock2807: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2832
update_positions_ConditionalTrueBlock2830: ;Main true block ;keep 
	
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
update_positions_elsedoneblock2832
	jmp update_positions_elsedoneblock2809
update_positions_elseblock2808
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2841
update_positions_ConditionalTrueBlock2839: ;Main true block ;keep 
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
update_positions_elsedoneblock2841
update_positions_elsedoneblock2809
update_positions_elsedoneblock2719
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed2913
	jmp update_positions_ConditionalTrueBlock2847
update_positions_localfailed2913: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed2912
	jmp update_positions_ConditionalTrueBlock2847
update_positions_localfailed2912
	jmp update_positions_elsedoneblock2849
update_positions_ConditionalTrueBlock2847: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock2918
update_positions_ConditionalTrueBlock2916: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2931
update_positions_localsuccess2933: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_2_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_2_input;keep
	beq update_positions_elsedoneblock2931
update_positions_ConditionalTrueBlock2929: ;Main true block ;keep 
	
; // Move P2 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_2_input
	; Calling storevariable on generic assign expression
	sta player_2_head
update_positions_elsedoneblock2931
update_positions_elsedoneblock2918
	
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
	beq update_positions_elseblock2938
update_positions_ConditionalTrueBlock2937: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2962
update_positions_ConditionalTrueBlock2960: ;Main true block ;keep 
	
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
update_positions_elsedoneblock2962
	jmp update_positions_elsedoneblock2939
update_positions_elseblock2938
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2971
update_positions_ConditionalTrueBlock2969: ;Main true block ;keep 
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
update_positions_elsedoneblock2971
update_positions_elsedoneblock2939
update_positions_elsedoneblock2849
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
init_gamestate_rightvarInteger_var2982 = $54
	sta init_gamestate_rightvarInteger_var2982
	sty init_gamestate_rightvarInteger_var2982+1
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
	adc init_gamestate_rightvarInteger_var2982
init_gamestate_wordAdd2980
	sta init_gamestate_rightvarInteger_var2982
	; High-bit binop
	tya
	adc init_gamestate_rightvarInteger_var2982+1
	tay
	lda init_gamestate_rightvarInteger_var2982
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
init_gamestate_rightvarInteger_var2985 = $54
	sta init_gamestate_rightvarInteger_var2985
	sty init_gamestate_rightvarInteger_var2985+1
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
	adc init_gamestate_rightvarInteger_var2985
init_gamestate_wordAdd2983
	sta init_gamestate_rightvarInteger_var2985
	; High-bit binop
	tya
	adc init_gamestate_rightvarInteger_var2985+1
	tay
	lda init_gamestate_rightvarInteger_var2985
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
	rts
end_procedure_init_gamestate
	
; // Play title screen animation 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : play_title_animation
	;    Procedure type : User-defined procedure
pta_tmp	dc.b	0
pta_i	dc.b	0
pta_screen_loc	=            $72
play_title_animation_block2987
play_title_animation
	; Binary clause Simplified: EQUALS
	clc
	lda anim_speed
	; cmp #$00 ignored
	bne play_title_animation_localfailed3107
	jmp play_title_animation_ConditionalTrueBlock2989
play_title_animation_localfailed3107
	jmp play_title_animation_elsedoneblock2991
play_title_animation_ConditionalTrueBlock2989: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop3109
	
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
	bne play_title_animation_elseblock3139
play_title_animation_ConditionalTrueBlock3138: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3140
play_title_animation_elseblock3139
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3140
	
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
play_title_animation_rightvarInteger_var3147 = $54
	sta play_title_animation_rightvarInteger_var3147
	sty play_title_animation_rightvarInteger_var3147+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc pta_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc play_title_animation_skip3149
	iny
play_title_animation_skip3149
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var3147
play_title_animation_wordAdd3145
	sta play_title_animation_rightvarInteger_var3147
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3147+1
	tay
	lda play_title_animation_rightvarInteger_var3147
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock3152
play_title_animation_ConditionalTrueBlock3151: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3153
play_title_animation_elseblock3152
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3153
play_title_animation_loopstart3110
	; Test Inc dec D
	inc pta_i
	lda MYSCREEN_WIDTH
	cmp pta_i ;keep
	beq play_title_animation_loopdone3158
play_title_animation_loopnotdone3159
	jmp play_title_animation_forloop3109
play_title_animation_loopdone3158
play_title_animation_loopend3111
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop3160
	
; // Left/Right sides
; // Left
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var3195 = $54
	sta play_title_animation_rightvarInteger_var3195
	sty play_title_animation_rightvarInteger_var3195+1
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
	adc play_title_animation_rightvarInteger_var3195
play_title_animation_wordAdd3193
	sta play_title_animation_rightvarInteger_var3195
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3195+1
	tay
	lda play_title_animation_rightvarInteger_var3195
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock3198
play_title_animation_ConditionalTrueBlock3197: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3199
play_title_animation_elseblock3198
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3199
	
; // Right
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
play_title_animation_rightvarInteger_var3206 = $54
	sta play_title_animation_rightvarInteger_var3206
	sty play_title_animation_rightvarInteger_var3206+1
	; Generic 16 bit op
	ldy #0
	ldx #0 ; Fake 24 bit
	lda MYSCREEN_WIDTH
play_title_animation_rightvarInteger_var3209 =            $56
	sta play_title_animation_rightvarInteger_var3209
	sty play_title_animation_rightvarInteger_var3209+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var3212 =            $58
	sta play_title_animation_rightvarInteger_var3212
	sty play_title_animation_rightvarInteger_var3212+1
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
	adc play_title_animation_rightvarInteger_var3212
play_title_animation_wordAdd3210
	sta play_title_animation_rightvarInteger_var3212
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3212+1
	tay
	lda play_title_animation_rightvarInteger_var3212
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var3209
play_title_animation_wordAdd3207
	sta play_title_animation_rightvarInteger_var3209
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var3209+1
	tay
	lda play_title_animation_rightvarInteger_var3209
	; Low bit binop:
	sec
	sbc play_title_animation_rightvarInteger_var3206
play_title_animation_wordAdd3204
	sta play_title_animation_rightvarInteger_var3206
	; High-bit binop
	tya
	sbc play_title_animation_rightvarInteger_var3206+1
	tay
	lda play_title_animation_rightvarInteger_var3206
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock3215
play_title_animation_ConditionalTrueBlock3214: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock3216
play_title_animation_elseblock3215
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock3216
play_title_animation_loopstart3161
	; Test Inc dec D
	inc pta_i
	; Forcetype: NADA
	lda #$18
	cmp pta_i ;keep
	beq play_title_animation_loopdone3221
play_title_animation_loopnotdone3222
	jmp play_title_animation_forloop3160
play_title_animation_loopdone3221
play_title_animation_loopend3162
	
; // Reset animation counter 
	lda DEF_ANIM_SPEED
	; Calling storevariable on generic assign expression
	sta anim_speed
play_title_animation_elsedoneblock2991
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
do_irq_animation_printdecimal3225
	jsr init_printdecimal_div10 
	ora #$30
	sta (screenmemory),y
	dey
	bpl do_irq_animation_printdecimal3225
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
player_crash_forloop3231
	
; // Increment animation		
	lda tmp
	clc
	adc #$01
	sta tmp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc player_crash_WordAdd3259
	inc tmp+1
player_crash_WordAdd3259
	; Binary clause INTEGER: GREATER
	lda tmp+1   ; compare high bytes
	cmp #$00 ;keep
	bcc player_crash_elsedoneblock3263
	bne player_crash_ConditionalTrueBlock3261
	lda tmp
	cmp #$02 ;keep
	bcc player_crash_elsedoneblock3263
	beq player_crash_elsedoneblock3263
player_crash_ConditionalTrueBlock3261: ;Main true block ;keep 
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
player_crash_elsedoneblock3263
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock3269
player_crash_ConditionalTrueBlock3267: ;Main true block ;keep 
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
player_crash_elsedoneblock3269
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock3277
player_crash_ConditionalTrueBlock3275: ;Main true block ;keep 
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
player_crash_elsedoneblock3277
	
; // Make sound
	lda i
	; Calling storevariable on generic assign expression
	sta $e848
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
player_crash_loopstart3232
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
	beq player_crash_loopdone3282
player_crash_loopnotdone3283
	jmp player_crash_forloop3231
player_crash_loopdone3282
player_crash_loopend3233
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
	beq alternate_engine_sound_localfailed3292
	jmp alternate_engine_sound_ConditionalTrueBlock3287
alternate_engine_sound_localfailed3292: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq alternate_engine_sound_elseblock3288
alternate_engine_sound_ConditionalTrueBlock3287: ;Main true block ;keep 
	
; // Higher octave when turbo is engaged
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	jmp alternate_engine_sound_elsedoneblock3289
alternate_engine_sound_elseblock3288
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
alternate_engine_sound_elsedoneblock3289
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c8;keep
	bne alternate_engine_sound_elseblock3297
alternate_engine_sound_ConditionalTrueBlock3296: ;Main true block ;keep 
	
; // Iterate through several pitch values
	; Forcetype: NADA
	lda #$cd
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock3298
alternate_engine_sound_elseblock3297
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$cd;keep
	bne alternate_engine_sound_elseblock3325
alternate_engine_sound_ConditionalTrueBlock3324: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c3
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock3326
alternate_engine_sound_elseblock3325
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c3;keep
	bne alternate_engine_sound_elsedoneblock3340
alternate_engine_sound_ConditionalTrueBlock3338: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c8
	; Calling storevariable on generic assign expression
	sta sound_pitch
alternate_engine_sound_elsedoneblock3340
alternate_engine_sound_elsedoneblock3326
alternate_engine_sound_elsedoneblock3298
	
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
game_loop_while3345
game_loop_loopstart3349
	; Binary clause Simplified: EQUALS
	clc
	lda game_over_flag
	; cmp #$00 ignored
	bne game_loop_elsedoneblock3348
game_loop_ConditionalTrueBlock3346: ;Main true block ;keep 
	
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
	bne game_loop_elsedoneblock3362
game_loop_ConditionalTrueBlock3360: ;Main true block ;keep 
	rts
game_loop_elsedoneblock3362
	
; // Slow it down
	lda game_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	jmp game_loop_while3345
game_loop_elsedoneblock3348
game_loop_loopend3350
	rts
end_procedure_game_loop
block1
main_block_begin_
	
; // Init
; // Check PET model being used
	jsr check_model
	; Forcetype: NADA
	lda #$3c
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	lda #$00
	ldx #$80
	sta screen_loc
	stx screen_loc+1
	jsr set_uppercase
MainProgram_while3366
MainProgram_loopstart3370
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock3369
MainProgram_ConditionalTrueBlock3367: ;Main true block ;keep 
	
; // Primary loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_over_flag
	jsr title_screen
	jsr game_screen
	jsr game_loop
	jmp MainProgram_while3366
MainProgram_elsedoneblock3369
MainProgram_loopend3371
main_block_end_
	; End of program
	; Ending memory block at $410
EndBlock410:

