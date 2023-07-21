
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
screen_loc_work	=     $6A
message_ptr	=     $6C
song_ptr	=     $6E
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
_ptr	=     $70
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
_pa_ptr	=     $70
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
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne check_input_elsedoneblock650
check_input_ConditionalTrueBlock648: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock668
check_input_ConditionalTrueBlock666: ;Main true block ;keep 
	
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
check_input_elsedoneblock668
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock674
check_input_ConditionalTrueBlock672: ;Main true block ;keep 
	
; // 8 - P2 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock674
check_input_elsedoneblock650
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne check_input_elsedoneblock680
check_input_ConditionalTrueBlock678: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock710
check_input_ConditionalTrueBlock708: ;Main true block ;keep 
	
; // A - P1 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock710
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$2
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock716
check_input_ConditionalTrueBlock714: ;Main true block ;keep 
	
; // D - P1 Right
	; Forcetype: NADA
	lda #$2
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
	
; // 4 - P2 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock722
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$80
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock728
check_input_ConditionalTrueBlock726: ;Main true block ;keep 
	
; // 6 - P2 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock728
check_input_elsedoneblock680
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$5;keep
	bne check_input_elsedoneblock734
check_input_ConditionalTrueBlock732: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock752
check_input_ConditionalTrueBlock750: ;Main true block ;keep 
	
; // S - P1 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock752
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock758
check_input_ConditionalTrueBlock756: ;Main true block ;keep 
	
; // 5 - P2 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock758
check_input_elsedoneblock734
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$8;keep
	bne check_input_elsedoneblock764
check_input_ConditionalTrueBlock762: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$20
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock776
check_input_ConditionalTrueBlock774: ;Main true block ;keep 
	
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
check_input_elsedoneblock776
check_input_elsedoneblock764
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$9;keep
	bne check_input_elsedoneblock782
check_input_ConditionalTrueBlock780: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$4
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock794
check_input_ConditionalTrueBlock792: ;Main true block ;keep 
	
; // SPACE - P2 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock794
check_input_elsedoneblock782
check_input_loopstart493
	; Test Inc dec D
	inc j
	; Forcetype: NADA
	lda #$a
	cmp j ;keep
	beq check_input_loopdone797
check_input_loopnotdone798
	jmp check_input_forloop492
check_input_loopdone797
check_input_loopend494
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_input_elsedoneblock802
check_input_ConditionalTrueBlock800: ;Main true block ;keep 
	
; // Get Sark's move when in single-player mode
	jsr sark_move
check_input_elsedoneblock802
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_localfailed811
	jmp check_input_ConditionalTrueBlock806
check_input_localfailed811: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elseblock807
check_input_ConditionalTrueBlock806: ;Main true block ;keep 
	
; // getin - read kernal keyboard input
; //call(#$FFE4); 
; //check_input_val := _A;
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(check_input_val,2);
	; Forcetype: NADA
	lda #$1
	rts
	jmp check_input_elsedoneblock808
check_input_elseblock807
	; Forcetype: NADA
	lda #$0
	rts
check_input_elsedoneblock808
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
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$b
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne display_game_mode_elsedoneblock818
display_game_mode_ConditionalTrueBlock816: ;Main true block ;keep 
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
display_game_mode_elsedoneblock818
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne display_game_mode_elsedoneblock824
display_game_mode_ConditionalTrueBlock822: ;Main true block ;keep 
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
display_game_mode_elsedoneblock824
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne display_game_mode_elsedoneblock830
display_game_mode_ConditionalTrueBlock828: ;Main true block ;keep 
	lda #<msg_tvs_player
	ldx #>msg_tvs_player
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
display_game_mode_elsedoneblock830
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
	bne cycle_text_input_handler_localfailed839
	jmp cycle_text_input_handler_ConditionalTrueBlock835
cycle_text_input_handler_localfailed839: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_elsedoneblock837
cycle_text_input_handler_ConditionalTrueBlock835: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // two player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock837
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_localfailed846
	jmp cycle_text_input_handler_ConditionalTrueBlock842
cycle_text_input_handler_localfailed846: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_elsedoneblock844
cycle_text_input_handler_ConditionalTrueBlock842: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // one player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock844
	
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
update_score_block848
update_score
	
; // position of rightmost score
	; Forcetype: NADA
	lda #$1c
	; Calling storevariable on generic assign expression
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
	beq update_score_elsedoneblock852
update_score_ConditionalTrueBlock850: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
update_score_forloop885
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock905
update_score_ConditionalTrueBlock903: ;Main true block ;keep 
	
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
update_score_elsedoneblock905
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock911
update_score_ConditionalTrueBlock909: ;Main true block ;keep 
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
update_score_elsedoneblock911
	
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
update_score_loopstart886
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$6
	cmp i ;keep
	bne update_score_forloop885
update_score_loopdone914: ;keep
update_score_loopend887
update_score_elsedoneblock852
	rts
end_procedure_update_score
	
; // Draw box around title screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : draw_title_screen_box
	;    Procedure type : User-defined procedure
dtsb_tmp	dc.b	0
dtsb_i	dc.b	0
draw_title_screen_box_block915
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
draw_title_screen_box_forloop917
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
draw_title_screen_box_loopstart918
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
	bne draw_title_screen_box_forloop917
draw_title_screen_box_loopdone922: ;keep
draw_title_screen_box_loopend919
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta dtsb_i
draw_title_screen_box_forloop923
	
; // Draw sides
	; Forcetype: NADA
	lda #$27
	; Calling storevariable on generic assign expression
	sta dtsb_tmp
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var933 = $54
	sta draw_title_screen_box_rightvarInteger_var933
	sty draw_title_screen_box_rightvarInteger_var933+1
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
	adc draw_title_screen_box_rightvarInteger_var933
draw_title_screen_box_wordAdd931
	sta draw_title_screen_box_rightvarInteger_var933
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var933+1
	tay
	lda draw_title_screen_box_rightvarInteger_var933
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
draw_title_screen_box_loopstart924
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
	beq draw_title_screen_box_loopdone934
draw_title_screen_box_loopnotdone935
	jmp draw_title_screen_box_forloop923
draw_title_screen_box_loopdone934
draw_title_screen_box_loopend925
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
	sbc #$4
	sta scroll_speed
	
; // Center scroll message
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$a
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
	; Forcetype: NADA
	lda #$10
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
	adc #$28
	sta screen_loc_work+0
	lda screen_loc+1
	adc #$00
	sta screen_loc_work+1
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop939
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart940
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop939
draw_game_screen_box_loopdone944: ;keep
draw_game_screen_box_loopend941
	
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
draw_game_screen_box_forloop946
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart947
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop946
draw_game_screen_box_loopdone951: ;keep
draw_game_screen_box_loopend948
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop952
	
; // Draw sides
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$27
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var963 = $54
	sta draw_game_screen_box_rightvarInteger_var963
	sty draw_game_screen_box_rightvarInteger_var963+1
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
	adc draw_game_screen_box_rightvarInteger_var963
draw_game_screen_box_wordAdd961
	sta draw_game_screen_box_rightvarInteger_var963
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var963+1
	tay
	lda draw_game_screen_box_rightvarInteger_var963
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
draw_game_screen_box_loopstart953
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_game_screen_box_forloop952
draw_game_screen_box_loopdone965: ;keep
draw_game_screen_box_loopend954
	
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
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
draw_game_screen_box_rightvarInteger_var969 = $54
	sta draw_game_screen_box_rightvarInteger_var969
	sty draw_game_screen_box_rightvarInteger_var969+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc #$50
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	adc #$00
	tay 
	pla 
	; Low bit binop:
	sec
	sbc draw_game_screen_box_rightvarInteger_var969
draw_game_screen_box_wordAdd967
	sta draw_game_screen_box_rightvarInteger_var969
	; High-bit binop
	tya
	sbc draw_game_screen_box_rightvarInteger_var969+1
	tay
	lda draw_game_screen_box_rightvarInteger_var969
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
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
draw_game_screen_box_rightvarInteger_var974 = $54
	sta draw_game_screen_box_rightvarInteger_var974
	sty draw_game_screen_box_rightvarInteger_var974+1
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$28
draw_game_screen_box_rightvarInteger_var977 =     $56
	sta draw_game_screen_box_rightvarInteger_var977
	sty draw_game_screen_box_rightvarInteger_var977+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc #$c0
	; Testing for byte:  #$03
	; RHS is word, no optimization
	pha 
	tya 
	adc #$03
	tay 
	pla 
	; Low bit binop:
	clc
	adc draw_game_screen_box_rightvarInteger_var977
draw_game_screen_box_wordAdd975
	sta draw_game_screen_box_rightvarInteger_var977
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var977+1
	tay
	lda draw_game_screen_box_rightvarInteger_var977
	; Low bit binop:
	sec
	sbc draw_game_screen_box_rightvarInteger_var974
draw_game_screen_box_wordAdd972
	sta draw_game_screen_box_rightvarInteger_var974
	; High-bit binop
	tya
	sbc draw_game_screen_box_rightvarInteger_var974+1
	tay
	lda draw_game_screen_box_rightvarInteger_var974
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
	bne game_screen_elsedoneblock983
game_screen_ConditionalTrueBlock981: ;Main true block ;keep 
	
; // Draw score display (text, not score values)
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$1e
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
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
game_screen_elsedoneblock983
	
; // string, x, y
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne game_screen_elsedoneblock989
game_screen_ConditionalTrueBlock987: ;Main true block ;keep 
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$1e
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
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
game_screen_elsedoneblock989
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
new_round_rightvarInteger_var995 = $54
	sta new_round_rightvarInteger_var995
	sty new_round_rightvarInteger_var995+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var995
new_round_wordAdd993
	sta new_round_rightvarInteger_var995
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var995+1
	tay
	lda new_round_rightvarInteger_var995
new_round_int_shift_var996 = $54
	sta new_round_int_shift_var996
	sty new_round_int_shift_var996+1
		lsr new_round_int_shift_var996+1
	ror new_round_int_shift_var996+0

	lda new_round_int_shift_var996
	ldy new_round_int_shift_var996+1
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
new_round_rightvarInteger_var999 = $54
	sta new_round_rightvarInteger_var999
	sty new_round_rightvarInteger_var999+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var999
new_round_wordAdd997
	sta new_round_rightvarInteger_var999
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var999+1
	tay
	lda new_round_rightvarInteger_var999
new_round_int_shift_var1000 = $54
	sta new_round_int_shift_var1000
	sty new_round_int_shift_var1000+1
		lsr new_round_int_shift_var1000+1
	ror new_round_int_shift_var1000+0

	lda new_round_int_shift_var1000
	ldy new_round_int_shift_var1000+1
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
	bne new_round_localfailed1006
	jmp new_round_ConditionalTrueBlock1002
new_round_localfailed1006: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_elsedoneblock1004
new_round_ConditionalTrueBlock1002: ;Main true block ;keep 
	
; // Adjust game speed
	; Forcetype: NADA
	lda #$28
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1004
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_localfailed1013
	jmp new_round_ConditionalTrueBlock1009
new_round_localfailed1013: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_elsedoneblock1011
new_round_ConditionalTrueBlock1009: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1e
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1011
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_localfailed1020
	jmp new_round_ConditionalTrueBlock1016
new_round_localfailed1020: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_elsedoneblock1018
new_round_ConditionalTrueBlock1016: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1018
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_localfailed1027
	jmp new_round_ConditionalTrueBlock1023
new_round_localfailed1027: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_elsedoneblock1025
new_round_ConditionalTrueBlock1023: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1025
	
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
new_game_rightvarInteger_var1032 = $54
	sta new_game_rightvarInteger_var1032
	sty new_game_rightvarInteger_var1032+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_game_rightvarInteger_var1032
new_game_wordAdd1030
	sta new_game_rightvarInteger_var1032
	; High-bit binop
	tya
	sbc new_game_rightvarInteger_var1032+1
	tay
	lda new_game_rightvarInteger_var1032
new_game_int_shift_var1033 = $54
	sta new_game_int_shift_var1033
	sty new_game_int_shift_var1033+1
		lsr new_game_int_shift_var1033+1
	ror new_game_int_shift_var1033+0

	lda new_game_int_shift_var1033
	ldy new_game_int_shift_var1033+1
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
	bne check_game_state_localfailed1040
	jmp check_game_state_ConditionalTrueBlock1036
check_game_state_localfailed1040: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1038
check_game_state_ConditionalTrueBlock1036: ;Main true block ;keep 
	
; // Check for player crashed
	jsr player_crash
	jsr stop_sound
check_game_state_elsedoneblock1038
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1200
check_game_state_localsuccess1201: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1200
	jmp check_game_state_ConditionalTrueBlock1043
check_game_state_localfailed1200
	jmp check_game_state_elseblock1044
check_game_state_ConditionalTrueBlock1043: ;Main true block ;keep 
	lda #<msg_both_crash
	ldx #>msg_both_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
	jmp check_game_state_elsedoneblock1045
check_game_state_elseblock1044
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock1206
check_game_state_ConditionalTrueBlock1205: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p2
	; Binary clause Simplified: LESS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1286
check_game_state_ConditionalTrueBlock1284: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1299
check_game_state_ConditionalTrueBlock1298: ;Main true block ;keep 
	lda #<msg_plr_crash
	ldx #>msg_plr_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$e
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1300
check_game_state_elseblock1299
	lda #<msg_p1_crash
	ldx #>msg_p1_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1300
	jsr new_round
check_game_state_elsedoneblock1286
	jmp check_game_state_elsedoneblock1207
check_game_state_elseblock1206
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1309
check_game_state_ConditionalTrueBlock1307: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p1
	; Binary clause Simplified: LESS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1337
check_game_state_ConditionalTrueBlock1335: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1350
check_game_state_ConditionalTrueBlock1349: ;Main true block ;keep 
	lda #<msg_sark_crash
	ldx #>msg_sark_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1351
check_game_state_elseblock1350
	lda #<msg_p2_crash
	ldx #>msg_p2_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1351
	jsr new_round
check_game_state_elsedoneblock1337
check_game_state_elsedoneblock1309
check_game_state_elsedoneblock1207
check_game_state_elsedoneblock1045
	; Binary clause Simplified: GREATEREQUAL
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elseblock1358
check_game_state_ConditionalTrueBlock1357: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1395
check_game_state_ConditionalTrueBlock1394: ;Main true block ;keep 
	
; // Check for end of game
	lda #<msg_plr_wins
	ldx #>msg_plr_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$b
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1396
check_game_state_elseblock1395
	lda #<msg_p1_wins
	ldx #>msg_p1_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1396
	jsr new_game
	jmp check_game_state_elsedoneblock1359
check_game_state_elseblock1358
	; Binary clause Simplified: GREATEREQUAL
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elsedoneblock1405
check_game_state_ConditionalTrueBlock1403: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1418
check_game_state_ConditionalTrueBlock1417: ;Main true block ;keep 
	lda #<msg_sark_wins
	ldx #>msg_sark_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$9
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1419
check_game_state_elseblock1418
	lda #<msg_p2_wins
	ldx #>msg_p2_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1419
	jsr new_game
check_game_state_elsedoneblock1405
check_game_state_elsedoneblock1359
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
	beq check_collisions_localfailed1437
	jmp check_collisions_ConditionalTrueBlock1426
check_collisions_localfailed1437: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1428
check_collisions_ConditionalTrueBlock1426: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1443
check_collisions_ConditionalTrueBlock1441: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
check_collisions_elsedoneblock1443
check_collisions_elsedoneblock1428
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1458
	jmp check_collisions_ConditionalTrueBlock1447
check_collisions_localfailed1458: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1449
check_collisions_ConditionalTrueBlock1447: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1464
check_collisions_ConditionalTrueBlock1462: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1464
check_collisions_elsedoneblock1449
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
sark_crash_distance_block1467
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
sark_crash_distance_while1468
sark_crash_distance_loopstart1472
	; Binary clause Simplified: EQUALS
	clc
	lda scd_crash
	; cmp #$00 ignored
	bne sark_crash_distance_elsedoneblock1471
sark_crash_distance_ConditionalTrueBlock1469: ;Main true block ;keep 
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
	beq sark_crash_distance_elsedoneblock1489
sark_crash_distance_ConditionalTrueBlock1487: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scd_crash
sark_crash_distance_elsedoneblock1489
	jmp sark_crash_distance_while1468
sark_crash_distance_elsedoneblock1471
sark_crash_distance_loopend1473
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
sark_check_player_dist_block1492
sark_check_player_dist
	
; //	sark_near_player_x_dist: byte;
; //	sark_near_player_x_neg:	boolean;	
; // left is neg
; //	sark_near_player_y_dist:	byte;
; //	sark_near_player_y_neg:	boolean;	
; // up is neg
; // Determine Sark's XY position
	; Right is PURE NUMERIC : Is word =1
	; 16x8 div
	ldy player_2_xy+1 ;keep
	lda player_2_xy
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
	sta scpd_sark_y
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	; Load right hand side
	; Forcetype: NADA
	tax
	lda #$28
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
sark_check_player_dist_rightvarInteger_var1496 = $54
	sta sark_check_player_dist_rightvarInteger_var1496
	sty sark_check_player_dist_rightvarInteger_var1496+1
	lda player_2_xy+1
	sec
	sbc sark_check_player_dist_rightvarInteger_var1496+1
	tay
	lda player_2_xy
	sec
	sbc sark_check_player_dist_rightvarInteger_var1496
	bcs sark_check_player_dist_wordAdd1493
	dey
sark_check_player_dist_wordAdd1493
	; Calling storevariable on generic assign expression
	sta scpd_sark_x
	
; // Derermine player's XY position
	; Right is PURE NUMERIC : Is word =1
	; 16x8 div
	ldy player_1_xy+1 ;keep
	lda player_1_xy
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
	sta scpd_player_y
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	; Load right hand side
	; Forcetype: NADA
	tax
	lda #$28
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
sark_check_player_dist_rightvarInteger_var1500 = $54
	sta sark_check_player_dist_rightvarInteger_var1500
	sty sark_check_player_dist_rightvarInteger_var1500+1
	lda player_1_xy+1
	sec
	sbc sark_check_player_dist_rightvarInteger_var1500+1
	tay
	lda player_1_xy
	sec
	sbc sark_check_player_dist_rightvarInteger_var1500
	bcs sark_check_player_dist_wordAdd1497
	dey
sark_check_player_dist_wordAdd1497
	; Calling storevariable on generic assign expression
	sta scpd_player_x
	; Binary clause Simplified: GREATER
	lda scpd_sark_y
	; Compare with pure num / var optimization
	cmp scpd_player_y;keep
	bcc sark_check_player_dist_elseblock1503
	beq sark_check_player_dist_elseblock1503
sark_check_player_dist_ConditionalTrueBlock1502: ;Main true block ;keep 
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
	jmp sark_check_player_dist_elsedoneblock1504
sark_check_player_dist_elseblock1503
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
sark_check_player_dist_elsedoneblock1504
	; Binary clause Simplified: GREATER
	lda scpd_sark_x
	; Compare with pure num / var optimization
	cmp scpd_player_x;keep
	bcc sark_check_player_dist_elseblock1511
	beq sark_check_player_dist_elseblock1511
sark_check_player_dist_ConditionalTrueBlock1510: ;Main true block ;keep 
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
	jmp sark_check_player_dist_elsedoneblock1512
sark_check_player_dist_elseblock1511
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
sark_check_player_dist_elsedoneblock1512
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
sark_move_block1517
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
	bcs sark_move_localfailed1949
	jmp sark_move_ConditionalTrueBlock1519
sark_move_localfailed1949
	jmp sark_move_elsedoneblock1521
sark_move_ConditionalTrueBlock1519: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed2166
	jmp sark_move_ConditionalTrueBlock1952
sark_move_localfailed2166: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_localfailed2165
	jmp sark_move_ConditionalTrueBlock1952
sark_move_localfailed2165
	jmp sark_move_elseblock1953
sark_move_ConditionalTrueBlock1952: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2170
	beq sark_move_elseblock2170
sark_move_localsuccess2203: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2170
	beq sark_move_elseblock2170
sark_move_ConditionalTrueBlock2169: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2207
sark_move_ConditionalTrueBlock2206: ;Main true block ;keep 
	
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
	jmp sark_move_elsedoneblock2208
sark_move_elseblock2207
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2208
	jmp sark_move_elsedoneblock2171
sark_move_elseblock2170
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_dist_d;keep
	bcc sark_move_elseblock2216
	beq sark_move_elseblock2216
sark_move_localsuccess2226: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elseblock2216
	beq sark_move_elseblock2216
sark_move_ConditionalTrueBlock2215: ;Main true block ;keep 
	
; // Take direction with best crash distance
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2217
sark_move_elseblock2216
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elsedoneblock2232
	beq sark_move_elsedoneblock2232
sark_move_ConditionalTrueBlock2230: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2232
sark_move_elsedoneblock2217
sark_move_elsedoneblock2171
	jmp sark_move_elsedoneblock1954
sark_move_elseblock1953
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_localfailed2308
	jmp sark_move_ConditionalTrueBlock2237
sark_move_localfailed2308: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne sark_move_elsedoneblock2239
sark_move_ConditionalTrueBlock2237: ;Main true block ;keep 
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2312
	beq sark_move_elseblock2312
sark_move_localsuccess2345: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp sm_cr_th;keep
	bcc sark_move_elseblock2312
	beq sark_move_elseblock2312
sark_move_ConditionalTrueBlock2311: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2349
sark_move_ConditionalTrueBlock2348: ;Main true block ;keep 
	
; // Sark is moving U or D. Check L/R
; // If both alternatives have 'good' crash distance,
; // move in direction closer to player
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2350
sark_move_elseblock2349
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2350
	jmp sark_move_elsedoneblock2313
sark_move_elseblock2312
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_dist_r;keep
	bcc sark_move_elseblock2358
	beq sark_move_elseblock2358
sark_move_localsuccess2368: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elseblock2358
	beq sark_move_elseblock2358
sark_move_ConditionalTrueBlock2357: ;Main true block ;keep 
	
; // Take direction with best crash distance
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
	jmp sark_move_elsedoneblock2359
sark_move_elseblock2358
	; Binary clause Simplified: GREATER
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp sm_cr_dist;keep
	bcc sark_move_elsedoneblock2374
	beq sark_move_elsedoneblock2374
sark_move_ConditionalTrueBlock2372: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
sark_move_elsedoneblock2374
sark_move_elsedoneblock2359
sark_move_elsedoneblock2313
sark_move_elsedoneblock2239
sark_move_elsedoneblock1954
sark_move_elsedoneblock1521
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock2380
sark_move_ConditionalTrueBlock2378: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda sark_near_player_x_dist
	; Compare with pure num / var optimization
	cmp sm_boost_th;keep
	bcs sark_move_elsedoneblock2393
sark_move_localsuccess2395: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	lda sark_near_player_y_dist
	; Compare with pure num / var optimization
	cmp sm_boost_th;keep
	bcs sark_move_elsedoneblock2393
sark_move_ConditionalTrueBlock2391: ;Main true block ;keep 
	
; //	If opponent less than 'n' moves away, engage turbo boost 
; //	unless it would cause a crash.
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
sark_move_elsedoneblock2393
sark_move_elsedoneblock2380
	; Binary clause Simplified: LESSEQUAL
	lda sark_near_player_x_dist
	; Compare with pure num / var optimization
	cmp sm_prox_th;keep
	beq sark_move_localsuccess2509
	bcs sark_move_localfailed2508
sark_move_localsuccess2509: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESSEQUAL
	lda sark_near_player_y_dist
	; Compare with pure num / var optimization
	cmp sm_prox_th;keep
	beq sark_move_ConditionalTrueBlock2398
	bcs sark_move_localfailed2508
	jmp sark_move_ConditionalTrueBlock2398
sark_move_localfailed2508
	jmp sark_move_elsedoneblock2400
sark_move_ConditionalTrueBlock2398: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed2539
	jmp sark_move_ConditionalTrueBlock2512
sark_move_localfailed2539: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_elsedoneblock2514
sark_move_ConditionalTrueBlock2512: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2543
sark_move_localsuccess2554: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_u
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elseblock2543
sark_move_ConditionalTrueBlock2542: ;Main true block ;keep 
	
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
	jmp sark_move_elsedoneblock2544
sark_move_elseblock2543
	
; // can't boost & turn
	; Binary clause Simplified: EQUALS
	clc
	lda sark_near_player_y_neg
	; cmp #$00 ignored
	bne sark_move_elsedoneblock2560
sark_move_localsuccess2562: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_d
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock2560
sark_move_ConditionalTrueBlock2558: ;Main true block ;keep 
	
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
sark_move_elsedoneblock2560
sark_move_elsedoneblock2544
sark_move_elsedoneblock2514
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne sark_move_localfailed2592
	jmp sark_move_ConditionalTrueBlock2565
sark_move_localfailed2592: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_head
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne sark_move_elsedoneblock2567
sark_move_ConditionalTrueBlock2565: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	beq sark_move_elseblock2596
sark_move_localsuccess2607: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_l
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elseblock2596
sark_move_ConditionalTrueBlock2595: ;Main true block ;keep 
	
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
	jmp sark_move_elsedoneblock2597
sark_move_elseblock2596
	
; // can't boost & turn
	; Binary clause Simplified: EQUALS
	clc
	lda sark_near_player_x_neg
	; cmp #$00 ignored
	bne sark_move_elsedoneblock2613
sark_move_localsuccess2615: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: GREATEREQUAL
	lda sm_cr_dist_r
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc sark_move_elsedoneblock2613
sark_move_ConditionalTrueBlock2611: ;Main true block ;keep 
	
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
sark_move_elsedoneblock2613
sark_move_elsedoneblock2597
sark_move_elsedoneblock2567
sark_move_elsedoneblock2400
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
	bcc update_positions_elsedoneblock2621
update_positions_ConditionalTrueBlock2619: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta turn_counter
update_positions_elsedoneblock2621
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed2658
	jmp update_positions_ConditionalTrueBlock2625
update_positions_localfailed2658: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock2627
update_positions_ConditionalTrueBlock2625: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock2663
update_positions_ConditionalTrueBlock2661: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2676
update_positions_localsuccess2678: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_1_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_1_input;keep
	beq update_positions_elsedoneblock2676
update_positions_ConditionalTrueBlock2674: ;Main true block ;keep 
	
; // May need to tweak this value
; // Move P1 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_1_input
	; Calling storevariable on generic assign expression
	sta player_1_head
update_positions_elsedoneblock2676
update_positions_elsedoneblock2663
	
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
	beq update_positions_elsedoneblock2684
update_positions_ConditionalTrueBlock2682: ;Main true block ;keep 
	
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
update_positions_elsedoneblock2684
update_positions_elsedoneblock2627
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed2723
	jmp update_positions_ConditionalTrueBlock2690
update_positions_localfailed2723: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock2692
update_positions_ConditionalTrueBlock2690: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock2728
update_positions_ConditionalTrueBlock2726: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock2741
update_positions_localsuccess2743: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_2_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_2_input;keep
	beq update_positions_elsedoneblock2741
update_positions_ConditionalTrueBlock2739: ;Main true block ;keep 
	
; // Move P2 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_2_input
	; Calling storevariable on generic assign expression
	sta player_2_head
update_positions_elsedoneblock2741
update_positions_elsedoneblock2728
	
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
	beq update_positions_elsedoneblock2749
update_positions_ConditionalTrueBlock2747: ;Main true block ;keep 
	
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
update_positions_elsedoneblock2749
update_positions_elsedoneblock2692
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
pta_screen_loc	=     $70
play_title_animation_block2758
play_title_animation
	; Binary clause Simplified: EQUALS
	clc
	lda anim_speed
	; cmp #$00 ignored
	bne play_title_animation_localfailed2878
	jmp play_title_animation_ConditionalTrueBlock2760
play_title_animation_localfailed2878
	jmp play_title_animation_elsedoneblock2762
play_title_animation_ConditionalTrueBlock2760: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop2880
	
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
	bne play_title_animation_elseblock2910
play_title_animation_ConditionalTrueBlock2909: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock2911
play_title_animation_elseblock2910
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock2911
	
; // Bottom
	; Generic 16 bit op
	; Forcetype: INTEGER
	; Integer constant assigning
	; Forcetype:  INTEGER
	ldy #$03
	lda #$c0
play_title_animation_rightvarInteger_var2918 = $54
	sta play_title_animation_rightvarInteger_var2918
	sty play_title_animation_rightvarInteger_var2918+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc pta_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc play_title_animation_skip2920
	iny
play_title_animation_skip2920
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var2918
play_title_animation_wordAdd2916
	sta play_title_animation_rightvarInteger_var2918
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var2918+1
	tay
	lda play_title_animation_rightvarInteger_var2918
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock2923
play_title_animation_ConditionalTrueBlock2922: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock2924
play_title_animation_elseblock2923
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock2924
play_title_animation_loopstart2881
	; Test Inc dec D
	inc pta_i
	; Forcetype: NADA
	lda #$28
	cmp pta_i ;keep
	beq play_title_animation_loopdone2929
play_title_animation_loopnotdone2930
	jmp play_title_animation_forloop2880
play_title_animation_loopdone2929
play_title_animation_loopend2882
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta pta_i
play_title_animation_forloop2931
	
; // Left/Right sides
; // Left
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var2966 = $54
	sta play_title_animation_rightvarInteger_var2966
	sty play_title_animation_rightvarInteger_var2966+1
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
	adc play_title_animation_rightvarInteger_var2966
play_title_animation_wordAdd2964
	sta play_title_animation_rightvarInteger_var2966
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var2966+1
	tay
	lda play_title_animation_rightvarInteger_var2966
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock2969
play_title_animation_ConditionalTrueBlock2968: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock2970
play_title_animation_elseblock2969
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock2970
	
; // Right
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$1
play_title_animation_rightvarInteger_var2977 = $54
	sta play_title_animation_rightvarInteger_var2977
	sty play_title_animation_rightvarInteger_var2977+1
	; Generic 16 bit op
	ldy #0
	; Forcetype: INTEGER
	lda #$28
play_title_animation_rightvarInteger_var2980 =     $56
	sta play_title_animation_rightvarInteger_var2980
	sty play_title_animation_rightvarInteger_var2980+1
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
play_title_animation_rightvarInteger_var2983 =     $58
	sta play_title_animation_rightvarInteger_var2983
	sty play_title_animation_rightvarInteger_var2983+1
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
	adc play_title_animation_rightvarInteger_var2983
play_title_animation_wordAdd2981
	sta play_title_animation_rightvarInteger_var2983
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var2983+1
	tay
	lda play_title_animation_rightvarInteger_var2983
	; Low bit binop:
	clc
	adc play_title_animation_rightvarInteger_var2980
play_title_animation_wordAdd2978
	sta play_title_animation_rightvarInteger_var2980
	; High-bit binop
	tya
	adc play_title_animation_rightvarInteger_var2980+1
	tay
	lda play_title_animation_rightvarInteger_var2980
	; Low bit binop:
	sec
	sbc play_title_animation_rightvarInteger_var2977
play_title_animation_wordAdd2975
	sta play_title_animation_rightvarInteger_var2977
	; High-bit binop
	tya
	sbc play_title_animation_rightvarInteger_var2977+1
	tay
	lda play_title_animation_rightvarInteger_var2977
	sta pta_screen_loc
	sty pta_screen_loc+1
	; Binary clause Simplified: EQUALS
	; Peek
	; Forcetype: NADA
	ldy #$0
	lda (pta_screen_loc),y
	; Compare with pure num / var optimization
	cmp #$a0;keep
	bne play_title_animation_elseblock2986
play_title_animation_ConditionalTrueBlock2985: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$66
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
	jmp play_title_animation_elsedoneblock2987
play_title_animation_elseblock2986
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	; Forcetype: NADA
	ldy #$0
	sta (pta_screen_loc),y
play_title_animation_elsedoneblock2987
play_title_animation_loopstart2932
	; Test Inc dec D
	inc pta_i
	; Forcetype: NADA
	lda #$18
	cmp pta_i ;keep
	beq play_title_animation_loopdone2992
play_title_animation_loopnotdone2993
	jmp play_title_animation_forloop2931
play_title_animation_loopdone2992
play_title_animation_loopend2933
	
; // Reset animation counter 
	; Forcetype: NADA
	lda #$6
	; Calling storevariable on generic assign expression
	sta anim_speed
play_title_animation_elsedoneblock2762
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
player_crash_forloop2995
	
; // Increment animation		
	lda tmp
	clc
	adc #$01
	sta tmp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc player_crash_WordAdd3023
	inc tmp+1
player_crash_WordAdd3023
	; Binary clause INTEGER: GREATER
	lda tmp+1   ; compare high bytes
	cmp #$00 ;keep
	bcc player_crash_elsedoneblock3027
	bne player_crash_ConditionalTrueBlock3025
	lda tmp
	cmp #$02 ;keep
	bcc player_crash_elsedoneblock3027
	beq player_crash_elsedoneblock3027
player_crash_ConditionalTrueBlock3025: ;Main true block ;keep 
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
player_crash_elsedoneblock3027
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock3033
player_crash_ConditionalTrueBlock3031: ;Main true block ;keep 
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
player_crash_elsedoneblock3033
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock3041
player_crash_ConditionalTrueBlock3039: ;Main true block ;keep 
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
player_crash_elsedoneblock3041
	
; // Make sound
	lda i
	; Calling storevariable on generic assign expression
	sta $e848
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
player_crash_loopstart2996
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
	beq player_crash_loopdone3046
player_crash_loopnotdone3047
	jmp player_crash_forloop2995
player_crash_loopdone3046
player_crash_loopend2997
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
	beq alternate_engine_sound_localfailed3056
	jmp alternate_engine_sound_ConditionalTrueBlock3051
alternate_engine_sound_localfailed3056: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq alternate_engine_sound_elseblock3052
alternate_engine_sound_ConditionalTrueBlock3051: ;Main true block ;keep 
	
; // Higher octave when turbo is engaged
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	jmp alternate_engine_sound_elsedoneblock3053
alternate_engine_sound_elseblock3052
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
alternate_engine_sound_elsedoneblock3053
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c8;keep
	bne alternate_engine_sound_elseblock3061
alternate_engine_sound_ConditionalTrueBlock3060: ;Main true block ;keep 
	
; // Iterate through several pitch values
	; Forcetype: NADA
	lda #$cd
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock3062
alternate_engine_sound_elseblock3061
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$cd;keep
	bne alternate_engine_sound_elseblock3089
alternate_engine_sound_ConditionalTrueBlock3088: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c3
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock3090
alternate_engine_sound_elseblock3089
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c3;keep
	bne alternate_engine_sound_elsedoneblock3104
alternate_engine_sound_ConditionalTrueBlock3102: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c8
	; Calling storevariable on generic assign expression
	sta sound_pitch
alternate_engine_sound_elsedoneblock3104
alternate_engine_sound_elsedoneblock3090
alternate_engine_sound_elsedoneblock3062
	
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
game_loop_while3109
game_loop_loopstart3113
	; Binary clause Simplified: EQUALS
	clc
	lda game_over_flag
	; cmp #$00 ignored
	bne game_loop_elsedoneblock3112
game_loop_ConditionalTrueBlock3110: ;Main true block ;keep 
	
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
	bne game_loop_elsedoneblock3126
game_loop_ConditionalTrueBlock3124: ;Main true block ;keep 
	rts
game_loop_elsedoneblock3126
	
; // Slow it down
	lda game_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	jmp game_loop_while3109
game_loop_elsedoneblock3112
game_loop_loopend3114
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
MainProgram_while3130
MainProgram_loopstart3134
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock3133
MainProgram_ConditionalTrueBlock3131: ;Main true block ;keep 
	
; // Primary loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_over_flag
	jsr title_screen
	jsr game_screen
	jsr game_loop
	jmp MainProgram_while3130
MainProgram_elsedoneblock3133
MainProgram_loopend3135
main_block_end_
	; End of program
	; Ending memory block at $410
EndBlock410:

