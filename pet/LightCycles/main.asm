
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
Interrupts_irq_address = $90
Model_IRQFlag = $91
Model_CheckAvailMem = $c353
screen_loc	= $68
screen_loc_work	= $6A
message_ptr	= $6C
song_ptr	= $6E
i	dc.b	$00
j	dc.b	$00
k	dc.b	$00
tmp	dc.w	$00
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
sound_oct_arr	dc.b $f, $33, $55
crash_anim_arr	dc.b $2a, $57, $51
dir_map_arr	dc.w $0, $ffff, $1, $ffd8, $28
dir_opp_arr	dc.b $0, $2, $1, $4, $3
enter_pressed	dc.b	$00
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
title_msg_0		dc.b	17
	dc.b	17
	dc.b	18
	dc.b	"GREETINGS"
	dc.b	146
	dc.b	" "
	dc.b	18
	dc.b	"PROGRAMS!"
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
	dc.b	"FIRST PLAYER TO SCORE FIVE WINS"
	dc.b	0
title_msg_5		dc.b	"TURNS DISABLED DURING BOOST"
	dc.b	0
title_msg_6		dc.b	17
	dc.b	"2023 FUZZYBAD"
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
score_msg_0		dc.b	"PLAYER 1:                    :PLAYER 2"
	dc.b	0
score_msg_1		dc.b	"  PLAYER:                    :SARK"
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
_ptr	= $70
_center_txt	dc.b	0
_mylen	dc.b	0
basic_print_block10
basic_print
	; Binary clause Simplified: NOTEQUALS
	clc
	lda _center_txt
	; cmp #$00 ignored
	beq basic_print_elsedoneblock14
basic_print_ConditionalTrueBlock12: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda _mylen
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs basic_print_elsedoneblock38
basic_print_ConditionalTrueBlock36: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta bp_i
basic_print_forloop47
	
; // Center text
	jsr cursor_right
basic_print_loopstart48
	; Compare is onpage
	; Test Inc dec D
	inc bp_i
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; 8 bit binop
	; Add/sub where right value is constant number
	; Forcetype: NADA
	lda #$28
	sec
	sbc _mylen
	 ; end add / sub var with constant
	lsr
	cmp bp_i ;keep
	bne basic_print_forloop47
basic_print_loopdone52: ;keep
basic_print_loopend49
basic_print_elsedoneblock38
basic_print_elsedoneblock14
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta next_ch
basic_print_while53
basic_print_loopstart57
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	; cmp #$00 ignored
	beq basic_print_elsedoneblock56
basic_print_ConditionalTrueBlock54: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc next_ch
	jmp basic_print_while53
basic_print_elsedoneblock56
basic_print_loopend58
	jsr cursor_return
	rts
end_procedure_basic_print
	
; // Print using KERNAL routine at X,Y location
	; NodeProcedureDecl -1
	; ***********  Defining procedure : basic_printat
	;    Procedure type : User-defined procedure
pa_next_ch	dc.b	0
pa_i	dc.b	0
_pa_ptr	= $70
_pa_myx	dc.b	0
_pa_myy	dc.b	0
basic_printat_block61
basic_printat
	
; // Home cursor
	jsr cursor_home
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myx
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock65
basic_printat_ConditionalTrueBlock63: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop74
	
; // Cursor right
	jsr cursor_right
basic_printat_loopstart75
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myx
	cmp pa_i ;keep
	bne basic_printat_forloop74
basic_printat_loopdone79: ;keep
basic_printat_loopend76
basic_printat_elsedoneblock65
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myy
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock83
basic_printat_ConditionalTrueBlock81: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop92
	
; // Cursor down
	jsr cursor_down
basic_printat_loopstart93
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myy
	cmp pa_i ;keep
	bne basic_printat_forloop92
basic_printat_loopdone97: ;keep
basic_printat_loopend94
basic_printat_elsedoneblock83
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_next_ch
basic_printat_while98
basic_printat_loopstart102
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	; cmp #$00 ignored
	beq basic_printat_elsedoneblock101
basic_printat_ConditionalTrueBlock99: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc pa_next_ch
	jmp basic_printat_while98
basic_printat_elsedoneblock101
basic_printat_loopend103
	rts
end_procedure_basic_printat
	
; // Delay function
	; NodeProcedureDecl -1
	; ***********  Defining procedure : do_delay
	;    Procedure type : User-defined procedure
dd_i	dc.b	0
delay_val	dc.b	0
do_delay_block106
do_delay
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda delay_val
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc do_delay_elsedoneblock110
do_delay_ConditionalTrueBlock108: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dd_i
do_delay_forloop119
	; Wait
	; Forcetype: NADA
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
do_delay_loopstart120
	; Compare is onpage
	; Test Inc dec D
	inc dd_i
	lda delay_val
	cmp dd_i ;keep
	bne do_delay_forloop119
do_delay_loopdone124: ;keep
do_delay_loopend121
do_delay_elsedoneblock110
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
	bne play_music_elsedoneblock129
play_music_ConditionalTrueBlock127: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	; Load pointer array
	ldy music_idx
	lda (song_ptr),y
	; cmp #$00 ignored
	bne play_music_elsedoneblock142
play_music_localsuccess144: ;keep
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
	bne play_music_elsedoneblock142
play_music_ConditionalTrueBlock140: ;Main true block ;keep 
	
; // Loop over note array
; // 	Structure - note value, octave, sustain value 
; // Change note when index is zero
; // End tune when pitch & sustain both = 0
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta music_idx
play_music_elsedoneblock142
	
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
play_music_elsedoneblock129
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
cycle_text_block146
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
	bne cycle_text_elseblock149
cycle_text_ConditionalTrueBlock148: ;Main true block ;keep 
	
; // Value of zero means we cycle forever
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scroll_step
	jmp cycle_text_elsedoneblock150
cycle_text_elseblock149
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_step
cycle_text_elsedoneblock150
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock158
cycle_text_ConditionalTrueBlock156: ;Main true block ;keep 
	
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
cycle_text_elsedoneblock158
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta k
cycle_text_forloop161
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_dir
	; cmp #$00 ignored
	beq cycle_text_elseblock182
cycle_text_ConditionalTrueBlock181: ;Main true block ;keep 
	
; // Scroll for the given # cycles
	jsr cycle_text_left
	jmp cycle_text_elsedoneblock183
cycle_text_elseblock182
	jsr cycle_text_right
cycle_text_elsedoneblock183
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_inp_brk
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock191
cycle_text_ConditionalTrueBlock189: ;Main true block ;keep 
	
; // Break on input
	rts
cycle_text_elsedoneblock191
cycle_text_loopstart162
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
	bne cycle_text_forloop161
cycle_text_loopdone194: ;keep
cycle_text_loopend163
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
cycle_text_right_forloop196
	; Generic 16 bit op
	ldy #0
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	lda scroll_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
cycle_text_right_rightvarInteger_var258 = $54
	sta cycle_text_right_rightvarInteger_var258
	sty cycle_text_right_rightvarInteger_var258+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_right_skip260
	iny
cycle_text_right_skip260
	; Low bit binop:
	clc
	adc cycle_text_right_rightvarInteger_var258
cycle_text_right_wordAdd256
	sta cycle_text_right_rightvarInteger_var258
	; High-bit binop
	tya
	adc cycle_text_right_rightvarInteger_var258+1
	tay
	lda cycle_text_right_rightvarInteger_var258
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
	bcs cycle_text_right_elseblock263
cycle_text_right_ConditionalTrueBlock262: ;Main true block ;keep 
	
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
	jmp cycle_text_right_elsedoneblock264
cycle_text_right_elseblock263
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
cycle_text_right_elsedoneblock264
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock272
cycle_text_right_ConditionalTrueBlock270: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_elsedoneblock272
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock278
cycle_text_right_ConditionalTrueBlock276: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock296
cycle_text_right_ConditionalTrueBlock294: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_right_elsedoneblock296
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock302
cycle_text_right_ConditionalTrueBlock300: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_right_elsedoneblock302
cycle_text_right_elsedoneblock278
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock308
cycle_text_right_ConditionalTrueBlock306: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_right_elsedoneblock308
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_right_loopstart197
	; Test Inc dec D
	inc i
	lda scroll_num_char
	cmp i ;keep
	beq cycle_text_right_loopdone311
cycle_text_right_loopnotdone312
	jmp cycle_text_right_forloop196
cycle_text_right_loopdone311
cycle_text_right_loopend198
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
cycle_text_left_forloop314
	
; // Cycle characters within string
	; Generic 16 bit op
	ldy #0
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	lda scroll_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
cycle_text_left_rightvarInteger_var376 = $54
	sta cycle_text_left_rightvarInteger_var376
	sty cycle_text_left_rightvarInteger_var376+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_left_skip378
	iny
cycle_text_left_skip378
	; Low bit binop:
	clc
	adc cycle_text_left_rightvarInteger_var376
cycle_text_left_wordAdd374
	sta cycle_text_left_rightvarInteger_var376
	; High-bit binop
	tya
	adc cycle_text_left_rightvarInteger_var376+1
	tay
	lda cycle_text_left_rightvarInteger_var376
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
	bcs cycle_text_left_elseblock381
cycle_text_left_ConditionalTrueBlock380: ;Main true block ;keep 
	
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
	jmp cycle_text_left_elsedoneblock382
cycle_text_left_elseblock381
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
cycle_text_left_elsedoneblock382
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock390
cycle_text_left_ConditionalTrueBlock388: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_left_elsedoneblock390
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock396
cycle_text_left_ConditionalTrueBlock394: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_left_elsedoneblock396
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock402
cycle_text_left_ConditionalTrueBlock400: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock420
cycle_text_left_ConditionalTrueBlock418: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_left_elsedoneblock420
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock426
cycle_text_left_ConditionalTrueBlock424: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_left_elsedoneblock426
cycle_text_left_elsedoneblock402
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_left_loopstart315
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
	beq cycle_text_left_loopdone429
cycle_text_left_loopnotdone430
	jmp cycle_text_left_forloop314
cycle_text_left_loopdone429
cycle_text_left_loopend316
	rts
end_procedure_cycle_text_left
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_input
	;    Procedure type : User-defined procedure
check_input_val	dc.b	0
check_input_block431
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
	sta enter_pressed
	
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
	bne check_input_elsedoneblock435
check_input_ConditionalTrueBlock433: ;Main true block ;keep 
	
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
check_input_elsedoneblock435
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
	beq check_input_elsedoneblock441
check_input_ConditionalTrueBlock439: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock441
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
	beq check_input_elsedoneblock447
check_input_ConditionalTrueBlock445: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock447
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
	beq check_input_elsedoneblock453
check_input_ConditionalTrueBlock451: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock453
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
	beq check_input_elsedoneblock459
check_input_ConditionalTrueBlock457: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock459
	; Binary clause Simplified: EQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$30
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bne check_input_elsedoneblock465
check_input_ConditionalTrueBlock463: ;Main true block ;keep 
	
; // Down
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock465
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
	beq check_input_elsedoneblock471
check_input_ConditionalTrueBlock469: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock471
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
	beq check_input_elsedoneblock477
check_input_ConditionalTrueBlock475: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock477
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
	beq check_input_elsedoneblock483
check_input_ConditionalTrueBlock481: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock483
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
	beq check_input_elsedoneblock489
check_input_ConditionalTrueBlock487: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock489
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta j
check_input_forloop492
	
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
	
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
	; Binary clause Simplified: EQUALS
	clc
	lda j
	; cmp #$00 ignored
	bne check_input_elsedoneblock680
check_input_ConditionalTrueBlock678: ;Main true block ;keep 
check_input_elsedoneblock680
	
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elsedoneblock686
check_input_ConditionalTrueBlock684: ;Main true block ;keep 
check_input_elsedoneblock686
	
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne check_input_elsedoneblock692
check_input_ConditionalTrueBlock690: ;Main true block ;keep 
check_input_elsedoneblock692
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne check_input_elsedoneblock698
check_input_ConditionalTrueBlock696: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock716
check_input_ConditionalTrueBlock714: ;Main true block ;keep 
	
; // W - P1 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock716
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock722
check_input_ConditionalTrueBlock720: ;Main true block ;keep 
	
; // 8 - P2 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock722
check_input_elsedoneblock698
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne check_input_elsedoneblock728
check_input_ConditionalTrueBlock726: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock758
check_input_ConditionalTrueBlock756: ;Main true block ;keep 
	
; // A - P1 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock758
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$2
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock764
check_input_ConditionalTrueBlock762: ;Main true block ;keep 
	
; // D - P1 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock764
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock770
check_input_ConditionalTrueBlock768: ;Main true block ;keep 
	
; // 4 - P2 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock770
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$80
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock776
check_input_ConditionalTrueBlock774: ;Main true block ;keep 
	
; // 6 - P2 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock776
check_input_elsedoneblock728
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$5;keep
	bne check_input_elsedoneblock782
check_input_ConditionalTrueBlock780: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock800
check_input_ConditionalTrueBlock798: ;Main true block ;keep 
	
; // S - P1 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock800
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock806
check_input_ConditionalTrueBlock804: ;Main true block ;keep 
	
; // 5 - P2 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock806
check_input_elsedoneblock782
	
; // RETURN - Toggle Options
; //if (check_input_val & 32) then begin
; //	enter_pressed := 1;
; //	moveto(41,0,hi(screen_char_loc));
; //	printstring("RETURN",0,6);
; //end;
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$6;keep
	bne check_input_elsedoneblock812
check_input_ConditionalTrueBlock810: ;Main true block ;keep 
check_input_elsedoneblock812
	
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$7;keep
	bne check_input_elsedoneblock818
check_input_ConditionalTrueBlock816: ;Main true block ;keep 
check_input_elsedoneblock818
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$8;keep
	bne check_input_elsedoneblock824
check_input_ConditionalTrueBlock822: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$20
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock836
check_input_ConditionalTrueBlock834: ;Main true block ;keep 
	
; // RIGHT SHIFT - P1 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock836
check_input_elsedoneblock824
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$9;keep
	bne check_input_elsedoneblock842
check_input_ConditionalTrueBlock840: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$4
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock854
check_input_ConditionalTrueBlock852: ;Main true block ;keep 
	
; // SPACE - P2 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock854
check_input_elsedoneblock842
check_input_loopstart493
	; Test Inc dec D
	inc j
	; Forcetype: NADA
	lda #$a
	cmp j ;keep
	beq check_input_loopdone857
check_input_loopnotdone858
	jmp check_input_forloop492
check_input_loopdone857
check_input_loopend494
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_localfailed865
	jmp check_input_ConditionalTrueBlock860
check_input_localfailed865: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elseblock861
check_input_ConditionalTrueBlock860: ;Main true block ;keep 
	
; // getin - read kernal keyboard input
; //call(#$FFE4); 
; //check_input_val := _A;
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(check_input_val,2);
	; Forcetype: NADA
	lda #$1
	rts
	jmp check_input_elsedoneblock862
check_input_elseblock861
	; Forcetype: NADA
	lda #$0
	rts
check_input_elsedoneblock862
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
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne display_game_mode_elsedoneblock872
display_game_mode_ConditionalTrueBlock870: ;Main true block ;keep 
	lda #<msg_one_player
	ldx #>msg_one_player
	sta _pa_ptr
	stx _pa_ptr+1
	; Forcetype: NADA
	lda #$b
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
display_game_mode_elsedoneblock872
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne display_game_mode_elsedoneblock878
display_game_mode_ConditionalTrueBlock876: ;Main true block ;keep 
	lda #<msg_two_player
	ldx #>msg_two_player
	sta _pa_ptr
	stx _pa_ptr+1
	; Forcetype: NADA
	lda #$b
	; Calling storevariable on generic assign expression
	sta _pa_myx
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta _pa_myy
	jsr basic_printat
display_game_mode_elsedoneblock878
	rts
end_procedure_display_game_mode
	
; // Handle additional input during cycle text routine
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cycle_text_input_handler
	;    Procedure type : User-defined procedure
cycle_text_input_handler
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_localfailed887
	jmp cycle_text_input_handler_ConditionalTrueBlock883
cycle_text_input_handler_localfailed887: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_elsedoneblock885
cycle_text_input_handler_ConditionalTrueBlock883: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // two player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock885
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_localfailed894
	jmp cycle_text_input_handler_ConditionalTrueBlock890
cycle_text_input_handler_localfailed894: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_elsedoneblock892
cycle_text_input_handler_ConditionalTrueBlock890: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // one player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock892
	
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
update_score_block896
update_score
	
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
	; Forcetype: NADA
	ldy #$1c
	sta (screen_loc),y
	; Binary clause Simplified: NOTEQUALS
	clc
	lda us_do_beep
	; cmp #$00 ignored
	beq update_score_elsedoneblock900
update_score_ConditionalTrueBlock898: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
update_score_forloop933
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock953
update_score_ConditionalTrueBlock951: ;Main true block ;keep 
	
; // Beep and flash score change
; // Flash changed score_msg_0
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy #$1c
	lda (screen_loc),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	; Forcetype: NADA
	sta (screen_loc),y
update_score_elsedoneblock953
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock959
update_score_ConditionalTrueBlock957: ;Main true block ;keep 
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
update_score_elsedoneblock959
	
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
update_score_loopstart934
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$6
	cmp i ;keep
	bne update_score_forloop933
update_score_loopdone962: ;keep
update_score_loopend935
update_score_elsedoneblock900
	rts
end_procedure_update_score
	
; // Draw box around title screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : draw_title_screen_box
	;    Procedure type : User-defined procedure
dtsb_tmp	dc.b	0
dtsb_i	dc.b	0
draw_title_screen_box_block963
draw_title_screen_box
	
; // Draw top and bottom	
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc #$c0
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$03
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dtsb_i
draw_title_screen_box_forloop965
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
draw_title_screen_box_loopstart966
	; Compare is onpage
	; Optimizer: a = a +/- b
	; Forcetype:  BYTE
	lda dtsb_i
	clc
	; Forcetype: NADA
	adc #$2
	sta dtsb_i
	; Forcetype: NADA
	lda #$28
	cmp dtsb_i ;keep
	bne draw_title_screen_box_forloop965
draw_title_screen_box_loopdone970: ;keep
draw_title_screen_box_loopend967
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dtsb_i
draw_title_screen_box_forloop971
	
; // Draw sides
	; Forcetype: NADA
	lda #$27
	; Calling storevariable on generic assign expression
	sta dtsb_tmp
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var981 = $54
	sta draw_title_screen_box_rightvarInteger_var981
	sty draw_title_screen_box_rightvarInteger_var981+1
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
	adc draw_title_screen_box_rightvarInteger_var981
draw_title_screen_box_wordAdd979
	sta draw_title_screen_box_rightvarInteger_var981
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var981+1
	tay
	lda draw_title_screen_box_rightvarInteger_var981
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
	; Forcetype: NADA
	adc #$28
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
	; Forcetype: NADA
	adc #$28
	 ; end add / sub var with constant
	clc
	adc dtsb_tmp
	 ; end add / sub var with constant
	tay
	pla
	sta (screen_loc_work),y
draw_title_screen_box_loopstart972
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
	beq draw_title_screen_box_loopdone982
draw_title_screen_box_loopnotdone983
	jmp draw_title_screen_box_forloop971
draw_title_screen_box_loopdone982
draw_title_screen_box_loopend973
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
	lda #$1f
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
	lda #$1b
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
	lda #$d
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
	
; // Cycle text, wait for input, play music
; // xpos, ypos, # chars, # cycles, direction, input break, play music
; //song_ptr := #game_end_music_arr;
	; Forcetype: NADA
	lda #$a
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
	
; // Disable input during text scroll 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scroll_input
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
	adc #$28
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$00
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop987
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart988
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop987
draw_game_screen_box_loopdone992: ;keep
draw_game_screen_box_loopend989
	
; // Draw bottom line
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc #$c0
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$03
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop994
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart995
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop994
draw_game_screen_box_loopdone999: ;keep
draw_game_screen_box_loopend996
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop1000
	
; // Draw sides
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1010 = $54
	sta draw_game_screen_box_rightvarInteger_var1010
	sty draw_game_screen_box_rightvarInteger_var1010+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy #0
	lda i
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	lda #$27
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var1010
draw_game_screen_box_wordAdd1008
	sta draw_game_screen_box_rightvarInteger_var1010
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1010+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1010
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
	; 8 bit binop
	; Add/sub where right value is constant number
	lda i
	clc
	; Forcetype: NADA
	adc #$27
	 ; end add / sub var with constant
	tay
	pla
	sta (screen_loc_work),y
draw_game_screen_box_loopstart1001
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_game_screen_box_forloop1000
draw_game_screen_box_loopdone1011: ;keep
draw_game_screen_box_loopend1002
	
; // top left mid box corner
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc #$28
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$00
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$70
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (screen_loc_work),y
	
; // top right mid box corner
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc #$4f
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$00
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$6e
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	sta (screen_loc_work),y
	
; // bot left mid box corner
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc #$c0
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$03
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$6d
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	sta (screen_loc_work),y
	
; // bot right mid box corner
	; INTEGER optimization: a=b+c 
	lda screen_loc
	clc
	adc #$e7
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$03
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$7d
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
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
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne game_screen_elsedoneblock1020
game_screen_ConditionalTrueBlock1018: ;Main true block ;keep 
	
; // Draw score display (text, not score values)
	lda #<score_msg_1
	ldx #>score_msg_1
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
game_screen_elsedoneblock1020
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne game_screen_elsedoneblock1026
game_screen_ConditionalTrueBlock1024: ;Main true block ;keep 
	
; // string, x, y
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
game_screen_elsedoneblock1026
	
; // string, x, y
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
	; Generic 16 bit op
	ldy #0
	lda message_len
new_round_rightvarInteger_var1032 = $54
	sta new_round_rightvarInteger_var1032
	sty new_round_rightvarInteger_var1032+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var1032
new_round_wordAdd1030
	sta new_round_rightvarInteger_var1032
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var1032+1
	tay
	lda new_round_rightvarInteger_var1032
new_round_int_shift_var1033 = $54
	sta new_round_int_shift_var1033
	sty new_round_int_shift_var1033+1
		lsr new_round_int_shift_var1033+1
	ror new_round_int_shift_var1033+0

	lda new_round_int_shift_var1033
	ldy new_round_int_shift_var1033+1
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
	; Generic 16 bit op
	ldy #0
new_round_rightvarInteger_var1036 = $54
	sta new_round_rightvarInteger_var1036
	sty new_round_rightvarInteger_var1036+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var1036
new_round_wordAdd1034
	sta new_round_rightvarInteger_var1036
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var1036+1
	tay
	lda new_round_rightvarInteger_var1036
new_round_int_shift_var1037 = $54
	sta new_round_int_shift_var1037
	sty new_round_int_shift_var1037+1
		lsr new_round_int_shift_var1037+1
	ror new_round_int_shift_var1037+0

	lda new_round_int_shift_var1037
	ldy new_round_int_shift_var1037+1
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
	bne new_round_localfailed1043
	jmp new_round_ConditionalTrueBlock1039
new_round_localfailed1043: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_elsedoneblock1041
new_round_ConditionalTrueBlock1039: ;Main true block ;keep 
	
; // Adjust game speed
	; Forcetype: NADA
	lda #$28
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1041
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_localfailed1050
	jmp new_round_ConditionalTrueBlock1046
new_round_localfailed1050: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_elsedoneblock1048
new_round_ConditionalTrueBlock1046: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1e
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1048
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_localfailed1057
	jmp new_round_ConditionalTrueBlock1053
new_round_localfailed1057: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_elsedoneblock1055
new_round_ConditionalTrueBlock1053: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1055
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_localfailed1064
	jmp new_round_ConditionalTrueBlock1060
new_round_localfailed1064: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_elsedoneblock1062
new_round_ConditionalTrueBlock1060: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1062
	
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
	; Generic 16 bit op
	ldy #0
	lda message_len
new_game_rightvarInteger_var1069 = $54
	sta new_game_rightvarInteger_var1069
	sty new_game_rightvarInteger_var1069+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_game_rightvarInteger_var1069
new_game_wordAdd1067
	sta new_game_rightvarInteger_var1069
	; High-bit binop
	tya
	sbc new_game_rightvarInteger_var1069+1
	tay
	lda new_game_rightvarInteger_var1069
new_game_int_shift_var1070 = $54
	sta new_game_int_shift_var1070
	sty new_game_int_shift_var1070+1
		lsr new_game_int_shift_var1070+1
	ror new_game_int_shift_var1070+0

	lda new_game_int_shift_var1070
	ldy new_game_int_shift_var1070+1
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
	; Forcetype: NADA
	lda #$32
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
	bne check_game_state_localfailed1077
	jmp check_game_state_ConditionalTrueBlock1073
check_game_state_localfailed1077: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1075
check_game_state_ConditionalTrueBlock1073: ;Main true block ;keep 
	
; // Check for player crashed
	jsr player_crash
	jsr stop_sound
check_game_state_elsedoneblock1075
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1237
check_game_state_localsuccess1238: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1237
	jmp check_game_state_ConditionalTrueBlock1080
check_game_state_localfailed1237
	jmp check_game_state_elseblock1081
check_game_state_ConditionalTrueBlock1080: ;Main true block ;keep 
	lda #<msg_both_crash
	ldx #>msg_both_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
	jmp check_game_state_elsedoneblock1082
check_game_state_elseblock1081
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock1243
check_game_state_ConditionalTrueBlock1242: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p2
	; Binary clause Simplified: LESS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1323
check_game_state_ConditionalTrueBlock1321: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1336
check_game_state_ConditionalTrueBlock1335: ;Main true block ;keep 
	lda #<msg_plr_crash
	ldx #>msg_plr_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$e
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1337
check_game_state_elseblock1336
	lda #<msg_p1_crash
	ldx #>msg_p1_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1337
	jsr new_round
check_game_state_elsedoneblock1323
	jmp check_game_state_elsedoneblock1244
check_game_state_elseblock1243
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1346
check_game_state_ConditionalTrueBlock1344: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p1
	; Binary clause Simplified: LESS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1374
check_game_state_ConditionalTrueBlock1372: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1387
check_game_state_ConditionalTrueBlock1386: ;Main true block ;keep 
	lda #<msg_sark_crash
	ldx #>msg_sark_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1388
check_game_state_elseblock1387
	lda #<msg_p2_crash
	ldx #>msg_p2_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1388
	jsr new_round
check_game_state_elsedoneblock1374
check_game_state_elsedoneblock1346
check_game_state_elsedoneblock1244
check_game_state_elsedoneblock1082
	; Binary clause Simplified: GREATEREQUAL
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elseblock1395
check_game_state_ConditionalTrueBlock1394: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1432
check_game_state_ConditionalTrueBlock1431: ;Main true block ;keep 
	
; // Check for end of game
	lda #<msg_plr_wins
	ldx #>msg_plr_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$b
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1433
check_game_state_elseblock1432
	lda #<msg_p1_wins
	ldx #>msg_p1_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1433
	jsr new_game
	jmp check_game_state_elsedoneblock1396
check_game_state_elseblock1395
	; Binary clause Simplified: GREATEREQUAL
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elsedoneblock1442
check_game_state_ConditionalTrueBlock1440: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1455
check_game_state_ConditionalTrueBlock1454: ;Main true block ;keep 
	lda #<msg_sark_wins
	ldx #>msg_sark_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$9
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1456
check_game_state_elseblock1455
	lda #<msg_p2_wins
	ldx #>msg_p2_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1456
	jsr new_game
check_game_state_elsedoneblock1442
check_game_state_elsedoneblock1396
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
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1474
	jmp check_collisions_ConditionalTrueBlock1463
check_collisions_localfailed1474: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1465
check_collisions_ConditionalTrueBlock1463: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1480
check_collisions_ConditionalTrueBlock1478: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
check_collisions_elsedoneblock1480
check_collisions_elsedoneblock1465
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1495
	jmp check_collisions_ConditionalTrueBlock1484
check_collisions_localfailed1495: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1486
check_collisions_ConditionalTrueBlock1484: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1501
check_collisions_ConditionalTrueBlock1499: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1501
check_collisions_elsedoneblock1486
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
sark_crash_distance_block1504
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
sark_crash_distance_while1505
sark_crash_distance_loopstart1509
	; Binary clause Simplified: EQUALS
	clc
	lda scd_crash
	; cmp #$00 ignored
	bne sark_crash_distance_elsedoneblock1508
sark_crash_distance_ConditionalTrueBlock1506: ;Main true block ;keep 
	; Test Inc dec D
	inc scd_i
	
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
	
; // increment pos
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
	beq sark_crash_distance_elsedoneblock1526
sark_crash_distance_ConditionalTrueBlock1524: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scd_crash
sark_crash_distance_elsedoneblock1526
	jmp sark_crash_distance_while1505
sark_crash_distance_elsedoneblock1508
sark_crash_distance_loopend1510
	lda scd_i
	rts
end_procedure_sark_crash_distance
	
; // P2 routine for single player game
; //  Set player_2_input, player_2_fire
	; NodeProcedureDecl -1
	; ***********  Defining procedure : sark_move
	;    Procedure type : User-defined procedure
sm_tmp_int	dc.w	0
sm_tmp_byt1	dc.b	0
sm_tmp_byt2	dc.b	0
sm_i	dc.b	0
sm_head	dc.b	0
sm_cr_th	dc.b	0
sm_trbo_th	dc.b	0
sark_move_block1529
sark_move
	
; // crash threshold
; // turbo threshold
; // Logic routine to box in opponent & avoid crashing..
; //
; //	1.	Check distance on current heading to obstacle.  
; //		Avoid crashing.
; //		Turn to direction with longest distance.
; //
	; Forcetype: NADA
	lda #$5
	; Calling storevariable on generic assign expression
	sta sm_cr_th
	
; // min val 2
	; Forcetype: NADA
	lda #$8
	; Calling storevariable on generic assign expression
	sta sm_trbo_th
	lda player_2_head
	; Calling storevariable on generic assign expression
	sta sm_head
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt1
	; Binary clause Simplified: LESS
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcs sark_move_localfailed1576
	jmp sark_move_ConditionalTrueBlock1531
sark_move_localfailed1576
	jmp sark_move_elsedoneblock1533
sark_move_ConditionalTrueBlock1531: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda sm_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed1600
	jmp sark_move_ConditionalTrueBlock1579
sark_move_localfailed1600: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda sm_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_elseblock1580
sark_move_ConditionalTrueBlock1579: ;Main true block ;keep 
	
; // crash imminent, check other directions
; // Sark moving L or R. Check U/D			
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta sm_head
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt1
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta sm_head
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt2
	; Binary clause Simplified: GREATEREQUAL
	lda sm_tmp_byt1
	; Compare with pure num / var optimization
	cmp sm_tmp_byt2;keep
	bcc sark_move_elseblock1604
sark_move_ConditionalTrueBlock1603: ;Main true block ;keep 
	
; // Switch to direction with longest distance
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_head
	jmp sark_move_elsedoneblock1605
sark_move_elseblock1604
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_head
sark_move_elsedoneblock1605
	jmp sark_move_elsedoneblock1581
sark_move_elseblock1580
	
; // Sark moving U or D. Check L/R
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta sm_head
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt1
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta sm_head
	; Calling storevariable on generic assign expression
	sta scd_head
	jsr sark_crash_distance
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt2
	; Binary clause Simplified: GREATEREQUAL
	lda sm_tmp_byt1
	; Compare with pure num / var optimization
	cmp sm_tmp_byt2;keep
	bcc sark_move_elseblock1613
sark_move_ConditionalTrueBlock1612: ;Main true block ;keep 
	
; // Switch to direction with longest distance
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_head
	jmp sark_move_elsedoneblock1614
sark_move_elseblock1613
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_head
sark_move_elsedoneblock1614
sark_move_elsedoneblock1581
sark_move_elsedoneblock1533
	; Binary clause INTEGER: GREATER
	lda player_1_xy+1   ; compare high bytes
	cmp player_2_xy+1 ;keep
	bcc sark_move_localfailed1639
	bne sark_move_ConditionalTrueBlock1620
	lda player_1_xy
	cmp player_2_xy ;keep
	bcc sark_move_localfailed1639
	beq sark_move_localfailed1639
	jmp sark_move_ConditionalTrueBlock1620
sark_move_localfailed1639
	jmp sark_move_elseblock1621
sark_move_ConditionalTrueBlock1620: ;Main true block ;keep 
	
; // DEBUG distance to obstacle
; //moveto(1,1,hi(screen_char_loc));
; //printdecimal(sm_tmp_byt1,2);
; // 	2. If opponent less than 'n' moves away, engage turbo boost.
; // row value - remainder dropped
	; Right is PURE NUMERIC : Is word =1
	; 16x8 div
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy player_1_xy+1 ;keep
	lda player_1_xy
	sec
	sbc player_2_xy
	; Testing for byte:  player_2_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	sbc player_2_xy+1
	tay 
	pla 
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	; Forcetype: NADA
	lda #$28
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt1
	
; // col value in remainder
	; Generic 16 bit op
	ldy #0
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	; Load right hand side
	; Forcetype: NADA
	tax
	lda #$28
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
sark_move_rightvarInteger_var1646 = $54
	sta sark_move_rightvarInteger_var1646
	sty sark_move_rightvarInteger_var1646+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy player_1_xy+1 ;keep
	lda player_1_xy
	sec
	sbc player_2_xy
	; Testing for byte:  player_2_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	sbc player_2_xy+1
	tay 
	pla 
	; Low bit binop:
	sec
	sbc sark_move_rightvarInteger_var1646
sark_move_wordAdd1642
	sta sark_move_rightvarInteger_var1646
	; High-bit binop
	tya
	sbc sark_move_rightvarInteger_var1646+1
	tay
	lda sark_move_rightvarInteger_var1646
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt2
	jmp sark_move_elsedoneblock1622
sark_move_elseblock1621
	
; // row value - remainder dropped
	; Right is PURE NUMERIC : Is word =1
	; 16x8 div
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy player_2_xy+1 ;keep
	lda player_2_xy
	sec
	sbc player_1_xy
	; Testing for byte:  player_1_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	sbc player_1_xy+1
	tay 
	pla 
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	; Forcetype: NADA
	lda #$28
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt1
	
; // col value in remainder
	; Generic 16 bit op
	ldy #0
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	; Load right hand side
	; Forcetype: NADA
	tax
	lda #$28
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
sark_move_rightvarInteger_var1654 = $54
	sta sark_move_rightvarInteger_var1654
	sty sark_move_rightvarInteger_var1654+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy player_2_xy+1 ;keep
	lda player_2_xy
	sec
	sbc player_1_xy
	; Testing for byte:  player_1_xy+1
	; RHS is word, no optimization
	pha 
	tya 
	sbc player_1_xy+1
	tay 
	pla 
	; Low bit binop:
	sec
	sbc sark_move_rightvarInteger_var1654
sark_move_wordAdd1650
	sta sark_move_rightvarInteger_var1654
	; High-bit binop
	tya
	sbc sark_move_rightvarInteger_var1654+1
	tay
	lda sark_move_rightvarInteger_var1654
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt2
sark_move_elsedoneblock1622
	; Binary clause Simplified: LESS
	lda sm_tmp_byt1
	; Compare with pure num / var optimization
	cmp sm_trbo_th;keep
	bcs sark_move_elsedoneblock1659
sark_move_localsuccess1661: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	lda sm_tmp_byt2
	; Compare with pure num / var optimization
	cmp sm_trbo_th;keep
	bcs sark_move_elsedoneblock1659
sark_move_ConditionalTrueBlock1657: ;Main true block ;keep 
	
; // DEBUG rows from player
; //moveto(41,1,hi(screen_char_loc));
; //printdecimal(sm_tmp_byt1,2);
; // DEBUG cols from player
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(sm_tmp_byt2,2);
; // Engage turbo if threshold reached
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
sark_move_elsedoneblock1659
	rts
end_procedure_sark_move
	
; // If within player proximity, both have same heading, AND
; //  player's row/col is open at Sark's row/col position,
; //	then cut off the player 
; // Update Positions
; // 	player_1_xy, player_2_xy 		- Player Coordinates
; // 	player_1_head, player_2_head 		- Player Headings 
; // 	player_1_fire, player_2_fire 		- Turbo Engaged
; // 	player_1_trail, player_2_trail 	- Trail Positions 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : update_positions
	;    Procedure type : User-defined procedure
update_positions
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1667
update_positions_ConditionalTrueBlock1665: ;Main true block ;keep 
	
; // P2 routine for single player game
	jsr sark_move
update_positions_elsedoneblock1667
	
; // Check if move avail
	; Test Inc dec D
	inc turn_counter
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc update_positions_elsedoneblock1673
update_positions_ConditionalTrueBlock1671: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta turn_counter
update_positions_elsedoneblock1673
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1710
	jmp update_positions_ConditionalTrueBlock1677
update_positions_localfailed1710: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1679
update_positions_ConditionalTrueBlock1677: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1715
update_positions_ConditionalTrueBlock1713: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1728
update_positions_localsuccess1730: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_1_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_1_input;keep
	beq update_positions_elsedoneblock1728
update_positions_ConditionalTrueBlock1726: ;Main true block ;keep 
	
; // May need to tweak this value
; // Move P1 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_1_input
	; Calling storevariable on generic assign expression
	sta player_1_head
update_positions_elsedoneblock1728
update_positions_elsedoneblock1715
	
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
	lda player_1_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1736
update_positions_ConditionalTrueBlock1734: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1736
update_positions_elsedoneblock1679
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1775
	jmp update_positions_ConditionalTrueBlock1742
update_positions_localfailed1775: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1744
update_positions_ConditionalTrueBlock1742: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1780
update_positions_ConditionalTrueBlock1778: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1793
update_positions_localsuccess1795: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_2_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_2_input;keep
	beq update_positions_elsedoneblock1793
update_positions_ConditionalTrueBlock1791: ;Main true block ;keep 
	
; // Move P2 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_2_input
	; Calling storevariable on generic assign expression
	sta player_2_head
update_positions_elsedoneblock1793
update_positions_elsedoneblock1780
	
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
	lda player_2_head
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1801
update_positions_ConditionalTrueBlock1799: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1801
update_positions_elsedoneblock1744
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
	; Forcetype: INTEGER
	; Integer constant assigning
	; Forcetype:  INTEGER
	ldy #$01
	lda #$e5
	; Calling storevariable on generic assign expression
	sta player_1_xy
	sty player_1_xy+1
	; Forcetype: INTEGER
	; Integer constant assigning
	; Forcetype:  INTEGER
	ldy #$02
	lda #$02
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
pta_screen_loc	= $70
play_title_animation_block1810
play_title_animation
	; Binary clause Simplified: EQUALS
	clc
	lda anim_speed
	; cmp #$00 ignored
	bne play_title_animation_localfailed1930
	jmp play_title_animation_ConditionalTrueBlock1812
play_title_animation_localfailed1930
	jmp play_title_animation_elsedoneblock1814
play_title_animation_ConditionalTrueBlock1812: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop1932
	
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
	bne play_title_animation_elseblock1962
play_title_animation_ConditionalTrueBlock1961: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock1963
play_title_animation_elseblock1962
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock1963
	
; // Bottom
	; Generic 16 bit op
	; Forcetype: INTEGER
	; Integer constant assigning
	; Forcetype:  INTEGER
	ldy #$03
	lda #$c0
play_title_animation_rightvarInteger_var1970 = $54
	sta play_title_animation_rightvarInteger_var1970
	sty play_title_animation_rightvarInteger_var1970+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc pta_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc play_title_animation_skip1972
	iny
play_title_animation_skip1972
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var1970
play_title_animation_wordAdd1968
	sta play_title_animation_rightvarInteger_var1970
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var1970+1
	tay
	lda play_title_animation_rightvarInteger_var1970
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock1975
play_title_animation_ConditionalTrueBlock1974: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock1976
play_title_animation_elseblock1975
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock1976
play_title_animation_loopstart1933
	; Test Inc dec D
	inc pta_i
	; Forcetype: NADA
	lda #$28
	cmp pta_i ;keep
	beq play_title_animation_loopdone1981
play_title_animation_loopnotdone1982
	jmp play_title_animation_forloop1932
play_title_animation_loopdone1981
play_title_animation_loopend1934
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop1983
	
; // Left/Right sides
; // Left
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var2018 = $54
	sta play_title_animation_rightvarInteger_var2018
	sty play_title_animation_rightvarInteger_var2018+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy #0
	lda pta_i
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var2018
play_title_animation_wordAdd2016
	sta play_title_animation_rightvarInteger_var2018
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var2018+1
	tay
	lda play_title_animation_rightvarInteger_var2018
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock2021
play_title_animation_ConditionalTrueBlock2020: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock2022
play_title_animation_elseblock2021
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock2022
	
; // Right
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
play_title_animation_rightvarInteger_var2029 = $54
	sta play_title_animation_rightvarInteger_var2029
	sty play_title_animation_rightvarInteger_var2029+1
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$28
play_title_animation_rightvarInteger_var2032 = $56
	sta play_title_animation_rightvarInteger_var2032
	sty play_title_animation_rightvarInteger_var2032+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var2035 = $58
	sta play_title_animation_rightvarInteger_var2035
	sty play_title_animation_rightvarInteger_var2035+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	ldy #0
	lda pta_i
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var2035
play_title_animation_wordAdd2033
	sta play_title_animation_rightvarInteger_var2035
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var2035+1
	tay
	lda play_title_animation_rightvarInteger_var2035
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var2032
play_title_animation_wordAdd2030
	sta play_title_animation_rightvarInteger_var2032
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var2032+1
	tay
	lda play_title_animation_rightvarInteger_var2032
	; Low bit binop:
	sec
	sbc play_title_animation_rightvarInteger_var2029
play_title_animation_wordAdd2027
	sta play_title_animation_rightvarInteger_var2029
	; High-bit binop
	tya
	sbc play_title_animation_rightvarInteger_var2029+1
	tay
	lda play_title_animation_rightvarInteger_var2029
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock2038
play_title_animation_ConditionalTrueBlock2037: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock2039
play_title_animation_elseblock2038
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock2039
play_title_animation_loopstart1984
	; Test Inc dec D
	inc pta_i
	; Forcetype: NADA
	lda #$18
	cmp pta_i ;keep
	beq play_title_animation_loopdone2044
play_title_animation_loopnotdone2045
	jmp play_title_animation_forloop1983
play_title_animation_loopdone2044
play_title_animation_loopend1985
	
; // Reset animation counter 
	; Forcetype: NADA
	lda #$6
	; Calling storevariable on generic assign expression
	sta anim_speed
play_title_animation_elsedoneblock1814
	; Test Inc dec D
	dec anim_speed
	rts
end_procedure_play_title_animation
	
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
player_crash_forloop2047
	
; // Increment animation		
	lda tmp
	clc
	adc #$01
	sta tmp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc player_crash_WordAdd2075
	inc tmp+1
player_crash_WordAdd2075
	; Binary clause INTEGER: GREATER
	lda tmp+1   ; compare high bytes
	cmp #$00 ;keep
	bcc player_crash_elsedoneblock2079
	bne player_crash_ConditionalTrueBlock2077
	lda tmp
	cmp #$02 ;keep
	bcc player_crash_elsedoneblock2079
	beq player_crash_elsedoneblock2079
player_crash_ConditionalTrueBlock2077: ;Main true block ;keep 
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
player_crash_elsedoneblock2079
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock2085
player_crash_ConditionalTrueBlock2083: ;Main true block ;keep 
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
player_crash_elsedoneblock2085
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock2093
player_crash_ConditionalTrueBlock2091: ;Main true block ;keep 
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
player_crash_elsedoneblock2093
	
; // Make sound
	lda i
	; Calling storevariable on generic assign expression
	sta $e848
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
player_crash_loopstart2048
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
	beq player_crash_loopdone2098
player_crash_loopnotdone2099
	jmp player_crash_forloop2047
player_crash_loopdone2098
player_crash_loopend2049
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
	beq alternate_engine_sound_localfailed2108
	jmp alternate_engine_sound_ConditionalTrueBlock2103
alternate_engine_sound_localfailed2108: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq alternate_engine_sound_elseblock2104
alternate_engine_sound_ConditionalTrueBlock2103: ;Main true block ;keep 
	
; // Higher octave when turbo is engaged
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	jmp alternate_engine_sound_elsedoneblock2105
alternate_engine_sound_elseblock2104
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
alternate_engine_sound_elsedoneblock2105
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c8;keep
	bne alternate_engine_sound_elseblock2113
alternate_engine_sound_ConditionalTrueBlock2112: ;Main true block ;keep 
	
; // Iterate through several pitch values
	; Forcetype: NADA
	lda #$cd
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock2114
alternate_engine_sound_elseblock2113
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$cd;keep
	bne alternate_engine_sound_elseblock2141
alternate_engine_sound_ConditionalTrueBlock2140: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c3
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock2142
alternate_engine_sound_elseblock2141
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c3;keep
	bne alternate_engine_sound_elsedoneblock2156
alternate_engine_sound_ConditionalTrueBlock2154: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c8
	; Calling storevariable on generic assign expression
	sta sound_pitch
alternate_engine_sound_elsedoneblock2156
alternate_engine_sound_elsedoneblock2142
alternate_engine_sound_elsedoneblock2114
	
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
game_loop_while2161
game_loop_loopstart2165
	; Binary clause Simplified: EQUALS
	clc
	lda game_over_flag
	; cmp #$00 ignored
	bne game_loop_elsedoneblock2164
game_loop_ConditionalTrueBlock2162: ;Main true block ;keep 
	
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
	bne game_loop_elsedoneblock2178
game_loop_ConditionalTrueBlock2176: ;Main true block ;keep 
	rts
game_loop_elsedoneblock2178
	
; // Slow it down
; // @@TODO: Replace with interrupt routine
	lda game_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	jmp game_loop_while2161
game_loop_elsedoneblock2164
game_loop_loopend2166
	rts
end_procedure_game_loop
block1
main_block_begin_
	
; // Init
	; Forcetype: NADA
	lda #$3c
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	lda #$00
	ldx #$80
	sta screen_loc
	stx screen_loc+1
	jsr set_uppercase
MainProgram_while2182
MainProgram_loopstart2186
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock2185
MainProgram_ConditionalTrueBlock2183: ;Main true block ;keep 
	
; // Primary loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_over_flag
	jsr title_screen
	jsr game_screen
	jsr game_loop
	jmp MainProgram_while2182
MainProgram_elsedoneblock2185
MainProgram_loopend2187
main_block_end_
	; End of program
	; Ending memory block at $410
EndBlock410:

