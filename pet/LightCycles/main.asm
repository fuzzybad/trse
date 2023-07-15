
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
Key_KeyRow = $e810
Key_KeyRead = $e812
screen_loc	= $68
screen_loc_work	= $6A
message_ptr	= $6C
i	dc.b	$00
j	dc.b	$00
tmp	dc.w	$00
message_len	dc.b	$00
game_over_flag	dc.b	$00
score_p1	dc.b	$00
score_p2	dc.b	$00
temp_byte	dc.b	$00
num_trail	dc.b	$00
scroll_speed	dc.b	$10
game_speed	dc.b	$20
do_music	dc.b	$00
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
title_msg_0		dc.b	17
	dc.b	17
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
	dc.b	18
	dc.b	"FIRST"
	dc.b	146
	dc.b	" "
	dc.b	18
	dc.b	"PLAYER"
	dc.b	146
	dc.b	" "
	dc.b	18
	dc.b	"TO"
	dc.b	146
	dc.b	" "
	dc.b	18
	dc.b	"SCORE"
	dc.b	146
	dc.b	" "
	dc.b	18
	dc.b	"FIVE"
	dc.b	146
	dc.b	" "
	dc.b	18
	dc.b	"WINS"
	dc.b	0
title_msg_5		dc.b	17
	dc.b	"PLAYER 1       PLAYER 2"
	dc.b	0
title_msg_6		dc.b	17
	dc.b	218
	dc.b	218
	dc.b	"             "
	dc.b	209
	dc.b	209
	dc.b	0
title_msg_7		dc.b	17
	dc.b	"USE SPT DUAL JOYSTICKS"
	dc.b	0
title_msg_8		dc.b	17
	dc.b	"OR             OR"
	dc.b	0
title_msg_9		dc.b	"WASD+SPC       8456+RSH"
	dc.b	0
title_msg_10		dc.b	17
	dc.b	17
	dc.b	18
	dc.b	"PRESS FIRE TO BEGIN"
	dc.b	0
score_msg_0		dc.b	"PLAYER 1:                    :PLAYER 2"
	dc.b	0
msg_both_crash		dc.b	"BOTH CRASHED, REDO"
	dc.b	0
msg_p1_crash		dc.b	"PLAYER 1 CRASHED"
	dc.b	0
msg_p2_crash		dc.b	"PLAYER 2 CRASHED"
	dc.b	0
msg_p1_wins		dc.b	"PLAYER 1 WINS"
	dc.b	0
msg_p2_wins		dc.b	"PLAYER 2 WINS"
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
_ptr	= $6E
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
	sta i
basic_print_forloop47
	
; // Center text
	jsr cursor_right
basic_print_loopstart48
	; Compare is onpage
	; Test Inc dec D
	inc i
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
	cmp i ;keep
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
_pa_ptr	= $6E
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
	sta i
basic_printat_forloop74
	
; // Cursor right
	jsr cursor_right
basic_printat_loopstart75
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda _pa_myx
	cmp i ;keep
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
	sta i
basic_printat_forloop92
	
; // Cursor down
	jsr cursor_down
basic_printat_loopstart93
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda _pa_myy
	cmp i ;keep
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
	
; // Might not be needed
; // Read input devices - keyboard & SPT dual joys
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_input
	;    Procedure type : User-defined procedure
check_input_val	dc.b	0
check_input_block106
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
	bne check_input_elsedoneblock110
check_input_ConditionalTrueBlock108: ;Main true block ;keep 
	
; // Checking SPT Single Joystick
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
; // Checking SPT Double Joysticks
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock110
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
	beq check_input_elsedoneblock116
check_input_ConditionalTrueBlock114: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock116
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
	beq check_input_elsedoneblock122
check_input_ConditionalTrueBlock120: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock122
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
	beq check_input_elsedoneblock128
check_input_ConditionalTrueBlock126: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock128
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
	beq check_input_elsedoneblock134
check_input_ConditionalTrueBlock132: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock134
	; Binary clause Simplified: EQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$30
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bne check_input_elsedoneblock140
check_input_ConditionalTrueBlock138: ;Main true block ;keep 
	
; // Down
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock140
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
	beq check_input_elsedoneblock146
check_input_ConditionalTrueBlock144: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock146
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
	beq check_input_elsedoneblock152
check_input_ConditionalTrueBlock150: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock152
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
	beq check_input_elsedoneblock158
check_input_ConditionalTrueBlock156: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock158
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
	beq check_input_elsedoneblock164
check_input_ConditionalTrueBlock162: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock164
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta j
check_input_forloop167
	
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
	bne check_input_elsedoneblock351
check_input_ConditionalTrueBlock349: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock369
check_input_ConditionalTrueBlock367: ;Main true block ;keep 
	
; //		 * These rows not used here
; //		 *
; //		if j = 0 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; //	
; //		if j = 1 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; //	
; //		if j = 2 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; //		
; // W - P1 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock369
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock375
check_input_ConditionalTrueBlock373: ;Main true block ;keep 
	
; // 8 - P2 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock375
check_input_elsedoneblock351
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne check_input_elsedoneblock381
check_input_ConditionalTrueBlock379: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock411
check_input_ConditionalTrueBlock409: ;Main true block ;keep 
	
; // A - P1 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock411
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$2
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock417
check_input_ConditionalTrueBlock415: ;Main true block ;keep 
	
; // D - P1 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock417
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock423
check_input_ConditionalTrueBlock421: ;Main true block ;keep 
	
; // 4 - P2 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock423
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$80
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock429
check_input_ConditionalTrueBlock427: ;Main true block ;keep 
	
; // 6 - P2 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock429
check_input_elsedoneblock381
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$5;keep
	bne check_input_elsedoneblock435
check_input_ConditionalTrueBlock433: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock453
check_input_ConditionalTrueBlock451: ;Main true block ;keep 
	
; // S - P1 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock453
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock459
check_input_ConditionalTrueBlock457: ;Main true block ;keep 
	
; // 5 - P2 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock459
check_input_elsedoneblock435
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$6;keep
	bne check_input_elsedoneblock465
check_input_ConditionalTrueBlock463: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$20
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock481
check_input_ConditionalTrueBlock479: ;Main true block ;keep 
	
; // RETURN - Toggle Options
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta enter_pressed
	; MoveTo optimization
	lda #$29
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
check_input_printstring_call486
	clc
	lda #<check_input_printstring_text487
	; Forcetype: NADA
	adc #$0
	ldy #>check_input_printstring_text487
	sta print_text+0
	sty print_text+1
	; Forcetype: NADA
	ldx #$6 ; optimized, look out for bugs
	jsr printstring
check_input_elsedoneblock481
check_input_elsedoneblock465
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$8;keep
	bne check_input_elsedoneblock491
check_input_ConditionalTrueBlock489: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$20
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock503
check_input_ConditionalTrueBlock501: ;Main true block ;keep 
	
; //		 * This row not used here
; //		 *
; //		if j = 7 then begin
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
; //		end;
; //		
; // RIGHT SHIFT - P1 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock503
check_input_elsedoneblock491
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$9;keep
	bne check_input_elsedoneblock509
check_input_ConditionalTrueBlock507: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$4
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock521
check_input_ConditionalTrueBlock519: ;Main true block ;keep 
	
; // SPACE - P2 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock521
check_input_elsedoneblock509
check_input_loopstart168
	; Test Inc dec D
	inc j
	; Forcetype: NADA
	lda #$a
	cmp j ;keep
	beq check_input_loopdone524
check_input_loopnotdone525
	jmp check_input_forloop167
check_input_loopdone524
check_input_loopend169
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_localfailed532
	jmp check_input_ConditionalTrueBlock527
check_input_localfailed532: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elseblock528
check_input_ConditionalTrueBlock527: ;Main true block ;keep 
	
; // getin - read kernal keyboard input
; //call(#$FFE4); 
; //check_input_val := _A;
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(check_input_val,2);
	; Forcetype: NADA
	lda #$1
	rts
	jmp check_input_elsedoneblock529
check_input_elseblock528
	; Forcetype: NADA
	lda #$0
	rts
check_input_elsedoneblock529
	rts
end_procedure_check_input
	
; // Update score disp
	; NodeProcedureDecl -1
	; ***********  Defining procedure : update_score
	;    Procedure type : User-defined procedure
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
	rts
end_procedure_update_score
	
; // Delay function
	; NodeProcedureDecl -1
	; ***********  Defining procedure : do_delay
	;    Procedure type : User-defined procedure
delay_val	dc.b	0
do_delay_block536
do_delay
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda delay_val
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc do_delay_elsedoneblock540
do_delay_ConditionalTrueBlock538: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta j
do_delay_forloop549
	; Wait
	; Forcetype: NADA
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
do_delay_loopstart550
	; Compare is onpage
	; Test Inc dec D
	inc j
	lda delay_val
	cmp j ;keep
	bne do_delay_forloop549
do_delay_loopdone554: ;keep
do_delay_loopend551
do_delay_elsedoneblock540
	rts
end_procedure_do_delay
	
; // Cycle text right & await input
	; NodeProcedureDecl -1
	; ***********  Defining procedure : cycle_text_right_wait
	;    Procedure type : User-defined procedure
ctrw_x	dc.b	0
ctrw_y	dc.b	0
ctrw_num_chars	dc.b	0
cycle_text_right_wait_block555
cycle_text_right_wait
	
; // Controls when trailing chars at end are drawn
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta temp_byte
	
; // Number of trailing chars
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Forcetype:  BYTE
	lda ctrw_num_chars
	lsr
	; Calling storevariable on generic assign expression
	sta num_trail
	; Binary clause Simplified: NOTEQUALS
	clc
	lda do_music
	; cmp #$00 ignored
	beq cycle_text_right_wait_elsedoneblock559
cycle_text_right_wait_ConditionalTrueBlock557: ;Main true block ;keep 
	
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
cycle_text_right_wait_elsedoneblock559
cycle_text_right_wait_while562
cycle_text_right_wait_loopstart566
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq cycle_text_right_wait_localfailed686
	jmp cycle_text_right_wait_ConditionalTrueBlock563
cycle_text_right_wait_localfailed686
	jmp cycle_text_right_wait_elsedoneblock565
cycle_text_right_wait_ConditionalTrueBlock563: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
cycle_text_right_wait_forloop688
	
; // Cycle characters within string
	; Generic 16 bit op
	ldy #0
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Forcetype:  INTEGER
	lda ctrw_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Forcetype: INTEGER
	lda #$28
	sta mul16x8_num2
	jsr mul16x8_procedure
cycle_text_right_wait_rightvarInteger_var750 = $54
	sta cycle_text_right_wait_rightvarInteger_var750
	sty cycle_text_right_wait_rightvarInteger_var750+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc ctrw_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_right_wait_skip752
	iny
cycle_text_right_wait_skip752
	; Low bit binop:
	clc
	adc cycle_text_right_wait_rightvarInteger_var750
cycle_text_right_wait_wordAdd748
	sta cycle_text_right_wait_rightvarInteger_var750
	; High-bit binop
	tya
	adc cycle_text_right_wait_rightvarInteger_var750+1
	tay
	lda cycle_text_right_wait_rightvarInteger_var750
	sta screen_loc_work
	sty screen_loc_work+1
	; Binary clause Simplified: GREATEREQUAL
	; Load pointer array
	ldy i
	lda (screen_loc_work),y
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc cycle_text_right_wait_elseblock755
cycle_text_right_wait_ConditionalTrueBlock754: ;Main true block ;keep 
	
; // Leading char
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy i
	lda (screen_loc_work),y
	sec
	; Forcetype: NADA
	sbc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
	jmp cycle_text_right_wait_elsedoneblock756
cycle_text_right_wait_elseblock755
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy i
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_wait_elsedoneblock756
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp num_trail;keep
	bcs cycle_text_right_wait_elseblock763
cycle_text_right_wait_ConditionalTrueBlock762: ;Main true block ;keep 
	
; // Trailing char
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda ctrw_num_chars
	sec
	sbc num_trail
	 ; end add / sub var with constant
	clc
	adc i
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta j
	jmp cycle_text_right_wait_elsedoneblock764
cycle_text_right_wait_elseblock763
	; 8 bit binop
	; Add/sub where right value is constant number
	lda i
	sec
	sbc num_trail
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta j
	
; // Activate trailing char after index reaches a given point
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta temp_byte
cycle_text_right_wait_elsedoneblock764
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_wait_elsedoneblock772
cycle_text_right_wait_ConditionalTrueBlock770: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc cycle_text_right_wait_elseblock785
cycle_text_right_wait_ConditionalTrueBlock784: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	sec
	; Forcetype: NADA
	sbc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
	jmp cycle_text_right_wait_elsedoneblock786
cycle_text_right_wait_elseblock785
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_wait_elsedoneblock786
cycle_text_right_wait_elsedoneblock772
	; Binary clause Simplified: NOTEQUALS
	clc
	lda do_music
	; cmp #$00 ignored
	beq cycle_text_right_wait_elsedoneblock794
cycle_text_right_wait_ConditionalTrueBlock792: ;Main true block ;keep 
	
; // Play theme song
	jsr play_title_music
cycle_text_right_wait_elsedoneblock794
	; Binary clause Simplified: NOTEQUALS
	clc
	jsr check_input
	; cmp #$00 ignored
	beq cycle_text_right_wait_elsedoneblock800
cycle_text_right_wait_ConditionalTrueBlock798: ;Main true block ;keep 
	rts
cycle_text_right_wait_elsedoneblock800
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_right_wait_loopstart689
	; Test Inc dec D
	inc i
	lda ctrw_num_chars
	cmp i ;keep
	beq cycle_text_right_wait_loopdone803
cycle_text_right_wait_loopnotdone804
	jmp cycle_text_right_wait_forloop688
cycle_text_right_wait_loopdone803
cycle_text_right_wait_loopend690
	jmp cycle_text_right_wait_while562
cycle_text_right_wait_elsedoneblock565
cycle_text_right_wait_loopend567
	rts
end_procedure_cycle_text_right_wait
	
; // Draw box around title screen
	; NodeProcedureDecl -1
	; ***********  Defining procedure : draw_title_screen_box
	;    Procedure type : User-defined procedure
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
	sta i
draw_title_screen_box_forloop807
	; Forcetype: NADA
	lda #$a0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc),y
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	sta (screen_loc_work),y
draw_title_screen_box_loopstart808
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_title_screen_box_forloop807
draw_title_screen_box_loopdone812: ;keep
draw_title_screen_box_loopend809
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_title_screen_box_forloop813
	
; // Draw sides
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var823 = $54
	sta draw_title_screen_box_rightvarInteger_var823
	sty draw_title_screen_box_rightvarInteger_var823+1
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
	adc draw_title_screen_box_rightvarInteger_var823
draw_title_screen_box_wordAdd821
	sta draw_title_screen_box_rightvarInteger_var823
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var823+1
	tay
	lda draw_title_screen_box_rightvarInteger_var823
	sta screen_loc_work
	sty screen_loc_work+1
	; Forcetype: NADA
	lda #$a0
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
draw_title_screen_box_loopstart814
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_title_screen_box_forloop813
draw_title_screen_box_loopdone824: ;keep
draw_title_screen_box_loopend815
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
	lda #$17
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
	lda #$11
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
	lda #$16
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
	lda #$17
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
	lda #$13
	; Calling storevariable on generic assign expression
	sta _mylen
	jsr basic_print
	
; // Draw box around screen
	jsr draw_title_screen_box
	
; // Play theme song
; //play_title_music();
; // cycle text & wait (X, Y, # Chars)
	; Forcetype: NADA
	lda #$a
	; Calling storevariable on generic assign expression
	sta ctrw_x
	; Forcetype: NADA
	lda #$15
	; Calling storevariable on generic assign expression
	sta ctrw_y
	; Forcetype: NADA
	lda #$13
	; Calling storevariable on generic assign expression
	sta ctrw_num_chars
	jsr cycle_text_right_wait
	rts
end_procedure_title_screen
	
; //basic_printat(#title_msg_10, 10, 19);
; //cycle_text_left_wait(10, 21, 19);
; //@@TODO: Animate pattern or cycles going around perimiter of title screen
; //		  at a fixed distance from each other. 
; //
; //		  STRETCH_GOAL:
; //		  Trail erases itself after some amount of chars (32?).
; //		  Allow player to control either or both cycles at title screen;
; //		  If either cycle crashes, take user to special screen with credits
; //		  and access to alternate trail mode, which is erased after some amount 
; // 		  of chars (255?)
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
draw_game_screen_box_forloop828
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart829
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop828
draw_game_screen_box_loopdone833: ;keep
draw_game_screen_box_loopend830
	
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
draw_game_screen_box_forloop835
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart836
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop835
draw_game_screen_box_loopdone840: ;keep
draw_game_screen_box_loopend837
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop841
	
; // Draw sides
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var851 = $54
	sta draw_game_screen_box_rightvarInteger_var851
	sty draw_game_screen_box_rightvarInteger_var851+1
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
	adc draw_game_screen_box_rightvarInteger_var851
draw_game_screen_box_wordAdd849
	sta draw_game_screen_box_rightvarInteger_var851
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var851+1
	tay
	lda draw_game_screen_box_rightvarInteger_var851
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
draw_game_screen_box_loopstart842
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_game_screen_box_forloop841
draw_game_screen_box_loopdone852: ;keep
draw_game_screen_box_loopend843
	
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
	
; // Draw score display (text, not score values)
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
	jsr update_score
	jsr init_engine_sound
	jsr init_gamestate
	rts
end_procedure_game_screen
	
; // debug
; //basic_printat("GAME START", 15, 10);	
; // string, x, y
; // Start new round
	; NodeProcedureDecl -1
	; ***********  Defining procedure : new_round
	;    Procedure type : User-defined procedure
new_round
	jsr update_score
	
; // Calculation for centered text
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Generic 16 bit op
	ldy #0
	lda message_len
new_round_rightvarInteger_var861 = $54
	sta new_round_rightvarInteger_var861
	sty new_round_rightvarInteger_var861+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var861
new_round_wordAdd859
	sta new_round_rightvarInteger_var861
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var861+1
	tay
	lda new_round_rightvarInteger_var861
new_round_int_shift_var862 = $54
	sta new_round_int_shift_var862
	sty new_round_int_shift_var862+1
		lsr new_round_int_shift_var862+1
	ror new_round_int_shift_var862+0

	lda new_round_int_shift_var862
	ldy new_round_int_shift_var862+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	
; // Print new round message & wait for input
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
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta ctrw_x
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta ctrw_y
	lda message_len
	; Calling storevariable on generic assign expression
	sta ctrw_num_chars
	jsr cycle_text_right_wait
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_localfailed868
	jmp new_round_ConditionalTrueBlock864
new_round_localfailed868: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_elsedoneblock866
new_round_ConditionalTrueBlock864: ;Main true block ;keep 
	
; // x, y, # chars
; // Adjust game speed based on round 
	; Forcetype: NADA
	lda #$1c
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock866
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_localfailed875
	jmp new_round_ConditionalTrueBlock871
new_round_localfailed875: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_elsedoneblock873
new_round_ConditionalTrueBlock871: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$18
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock873
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_localfailed882
	jmp new_round_ConditionalTrueBlock878
new_round_localfailed882: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_elsedoneblock880
new_round_ConditionalTrueBlock878: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock880
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_localfailed889
	jmp new_round_ConditionalTrueBlock885
new_round_localfailed889: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_elsedoneblock887
new_round_ConditionalTrueBlock885: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock887
	jsr game_screen
	rts
end_procedure_new_round
	
; // Start new game
	; NodeProcedureDecl -1
	; ***********  Defining procedure : new_game
	;    Procedure type : User-defined procedure
new_game
	jsr update_score
	
; // Calculation for centered text
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Generic 16 bit op
	ldy #0
	lda message_len
new_game_rightvarInteger_var894 = $54
	sta new_game_rightvarInteger_var894
	sty new_game_rightvarInteger_var894+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_game_rightvarInteger_var894
new_game_wordAdd892
	sta new_game_rightvarInteger_var894
	; High-bit binop
	tya
	sbc new_game_rightvarInteger_var894+1
	tay
	lda new_game_rightvarInteger_var894
new_game_int_shift_var895 = $54
	sta new_game_int_shift_var895
	sty new_game_int_shift_var895+1
		lsr new_game_int_shift_var895+1
	ror new_game_int_shift_var895+0

	lda new_game_int_shift_var895
	ldy new_game_int_shift_var895+1
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
	
; // Print Game Over message
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
; // await input
	ldy tmp+1 ;keep
	lda tmp
	; Calling storevariable on generic assign expression
	sta ctrw_x
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta ctrw_y
	lda message_len
	; Calling storevariable on generic assign expression
	sta ctrw_num_chars
	jsr cycle_text_right_wait
	
; // x, y, # chars
; // Reset scores
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta score_p1
	; Forcetype: NADA
	; Calling storevariable on generic assign expression
	sta score_p2
	
; // Reset game speed
	; Forcetype: NADA
	lda #$20
	; Calling storevariable on generic assign expression
	sta game_speed
	
; // Return to main loop
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
	bne check_game_state_localfailed902
	jmp check_game_state_ConditionalTrueBlock898
check_game_state_localfailed902: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock900
check_game_state_ConditionalTrueBlock898: ;Main true block ;keep 
	
; // Check for player crashed
	jsr player_crash
	jsr stop_sound
check_game_state_elsedoneblock900
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock906
check_game_state_localsuccess966: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock906
check_game_state_ConditionalTrueBlock905: ;Main true block ;keep 
	lda #<msg_both_crash
	ldx #>msg_both_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
	jmp check_game_state_elsedoneblock907
check_game_state_elseblock906
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock971
check_game_state_ConditionalTrueBlock970: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p2
	; Binary clause Simplified: LESS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1003
check_game_state_ConditionalTrueBlock1001: ;Main true block ;keep 
	lda #<msg_p1_crash
	ldx #>msg_p1_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
check_game_state_elsedoneblock1003
	jmp check_game_state_elsedoneblock972
check_game_state_elseblock971
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1010
check_game_state_ConditionalTrueBlock1008: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p1
	; Binary clause Simplified: LESS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1022
check_game_state_ConditionalTrueBlock1020: ;Main true block ;keep 
	lda #<msg_p2_crash
	ldx #>msg_p2_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
check_game_state_elsedoneblock1022
check_game_state_elsedoneblock1010
check_game_state_elsedoneblock972
check_game_state_elsedoneblock907
	; Binary clause Simplified: GREATEREQUAL
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elseblock1027
check_game_state_ConditionalTrueBlock1026: ;Main true block ;keep 
	
; // Check for end of game
	lda #<msg_p1_wins
	ldx #>msg_p1_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_game
	jmp check_game_state_elsedoneblock1028
check_game_state_elseblock1027
	; Binary clause Simplified: GREATEREQUAL
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elsedoneblock1042
check_game_state_ConditionalTrueBlock1040: ;Main true block ;keep 
	lda #<msg_p2_wins
	ldx #>msg_p2_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_game
check_game_state_elsedoneblock1042
check_game_state_elsedoneblock1028
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
	beq check_collisions_localfailed1058
	jmp check_collisions_ConditionalTrueBlock1047
check_collisions_localfailed1058: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1049
check_collisions_ConditionalTrueBlock1047: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1064
check_collisions_ConditionalTrueBlock1062: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
check_collisions_elsedoneblock1064
check_collisions_elsedoneblock1049
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1079
	jmp check_collisions_ConditionalTrueBlock1068
check_collisions_localfailed1079: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1070
check_collisions_ConditionalTrueBlock1068: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1085
check_collisions_ConditionalTrueBlock1083: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1085
check_collisions_elsedoneblock1070
	rts
end_procedure_check_collisions
	
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
	bcc update_positions_elsedoneblock1092
update_positions_ConditionalTrueBlock1090: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta turn_counter
update_positions_elsedoneblock1092
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1129
	jmp update_positions_ConditionalTrueBlock1096
update_positions_localfailed1129: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1098
update_positions_ConditionalTrueBlock1096: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1134
update_positions_ConditionalTrueBlock1132: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1147
update_positions_localsuccess1149: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_1_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_1_input;keep
	beq update_positions_elsedoneblock1147
update_positions_ConditionalTrueBlock1145: ;Main true block ;keep 
	
; // May need to tweak this value
; // Move P1 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_1_input
	; Calling storevariable on generic assign expression
	sta player_1_head
update_positions_elsedoneblock1147
update_positions_elsedoneblock1134
	
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
	beq update_positions_elsedoneblock1155
update_positions_ConditionalTrueBlock1153: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1155
update_positions_elsedoneblock1098
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1194
	jmp update_positions_ConditionalTrueBlock1161
update_positions_localfailed1194: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1163
update_positions_ConditionalTrueBlock1161: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1199
update_positions_ConditionalTrueBlock1197: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1212
update_positions_localsuccess1214: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_2_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_2_input;keep
	beq update_positions_elsedoneblock1212
update_positions_ConditionalTrueBlock1210: ;Main true block ;keep 
	
; // Move P2 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_2_input
	; Calling storevariable on generic assign expression
	sta player_2_head
update_positions_elsedoneblock1212
update_positions_elsedoneblock1199
	
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
	beq update_positions_elsedoneblock1220
update_positions_ConditionalTrueBlock1218: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1220
update_positions_elsedoneblock1163
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
	
; // crash visual & sound effect
	; NodeProcedureDecl -1
	; ***********  Defining procedure : player_crash
	;    Procedure type : User-defined procedure
player_crash
	
; // Explode in all directions:
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
player_crash_forloop1230
	
; // Increment animation		
	lda tmp
	clc
	adc #$01
	sta tmp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc player_crash_WordAdd1258
	inc tmp+1
player_crash_WordAdd1258
	; Binary clause INTEGER: GREATER
	lda tmp+1   ; compare high bytes
	cmp #$00 ;keep
	bcc player_crash_elsedoneblock1262
	bne player_crash_ConditionalTrueBlock1260
	lda tmp
	cmp #$02 ;keep
	bcc player_crash_elsedoneblock1262
	beq player_crash_elsedoneblock1262
player_crash_ConditionalTrueBlock1260: ;Main true block ;keep 
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
player_crash_elsedoneblock1262
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock1268
player_crash_ConditionalTrueBlock1266: ;Main true block ;keep 
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
player_crash_elsedoneblock1268
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock1276
player_crash_ConditionalTrueBlock1274: ;Main true block ;keep 
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
player_crash_elsedoneblock1276
	
; // Make sound
	ldy #0 ; Fake 16 bit
	lda i
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta $e848
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
player_crash_loopstart1231
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
	beq player_crash_loopdone1281
player_crash_loopnotdone1282
	jmp player_crash_forloop1230
player_crash_loopdone1281
player_crash_loopend1232
	rts
end_procedure_player_crash
	
; // Play title music - called from external loop
	; NodeProcedureDecl -1
	; ***********  Defining procedure : play_title_music
	;    Procedure type : User-defined procedure
play_title_music
	; Binary clause Simplified: EQUALS
	clc
	lda music_sust_idx
	; cmp #$00 ignored
	bne play_title_music_elsedoneblock1287
play_title_music_ConditionalTrueBlock1285: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	; Load Byte array
	; CAST type NADA
	ldx music_idx
	lda theme_music_arr,x 
	; cmp #$00 ignored
	bne play_title_music_elsedoneblock1308
play_title_music_localsuccess1310: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	clc
	; Load Byte array
	; CAST type NADA
	; 8 bit binop
	; Add/sub where right value is constant number
	lda music_idx
	clc
	; Forcetype: NADA
	adc #$2
	 ; end add / sub var with constant
	tax
	lda theme_music_arr,x 
	; cmp #$00 ignored
	bne play_title_music_elsedoneblock1308
play_title_music_ConditionalTrueBlock1306: ;Main true block ;keep 
	
; // Loop over note array
; // 	Structure - note value, octave, sustain value 
; // Change note when index is zero
; // End tune when pitch & sustain both = 0
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta music_idx
play_title_music_elsedoneblock1308
	
; // Set octave
	; Load Byte array
	; CAST type NADA
	; Load Byte array
	; CAST type NADA
	; 8 bit binop
	; Add/sub where right value is constant number
	lda music_idx
	clc
	; Forcetype: NADA
	adc #$1
	 ; end add / sub var with constant
	tax
	lda theme_music_arr,x 
	tax
	lda sound_oct_arr,x 
	; Calling storevariable on generic assign expression
	sta $e84a
	
; // Set the pitch
	; Load Byte array
	ldy #0 ; lhs is byte, but integer required
	; CAST type INTEGER
	ldx music_idx
	lda theme_music_arr,x 
	; Calling storevariable on generic assign expression
	sta $e848
	
; // Set sustain - this value is tuned based on routine calling this function
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul
	; Load Byte array
	; CAST type NADA
	; 8 bit binop
	; Add/sub where right value is constant number
	lda music_idx
	clc
	; Forcetype: NADA
	adc #$2
	 ; end add / sub var with constant
	tax
	lda theme_music_arr,x 
	; Load right hand side
	; Forcetype: NADA
	tax
	lda #$9
	jsr multiply_eightbit
	txa
	ldy #0 ; ::EightbitMul
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
	; Binary clause Simplified: GREATEREQUAL
	; Compare with pure num / var optimization
	cmp #$a2;keep
	bcc play_title_music_elsedoneblock1317
play_title_music_ConditionalTrueBlock1315: ;Main true block ;keep 
	
; // Just in case we hit end of array before end of music marker
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta music_idx
play_title_music_elsedoneblock1317
play_title_music_elsedoneblock1287
	; Test Inc dec D
	dec music_sust_idx
	rts
end_procedure_play_title_music
	; - Singleton version
; //
; // Play title music
; //procedure play_title_music();
; //begin
; //	
; // enable sound
; //	LOC_SOUND_REG	:= 16;
; //
; //	
; // Loop over note array
; //	
; // 	Structure - note value, octave, sustain value 
; //	for i:=0 to length(theme_music_arr) step 3 do
; //	begin
; //		
; // Exit when pitch & sustain both = 0
; //		if (theme_music_arr[i] = 0 and theme_music_arr[i+2] = 0) then break;
; //
; //		
; // set octave
; //		LOC_SOUND_OCT	:= sound_oct_arr[theme_music_arr[i+1]];
; //		
; // Set the pitch
; //		LOC_SOUND_FREQ	:= theme_music_arr[i];
; //		
; //		
; // Sustain
; //		for tmp:=0 to theme_music_arr[i+2] do
; //		begin
; //			do_delay(150);
; //		end;
; //	end;
; //	
; //	stop_sound();
; //end;
; //
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
	beq alternate_engine_sound_localfailed1328
	jmp alternate_engine_sound_ConditionalTrueBlock1323
alternate_engine_sound_localfailed1328: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq alternate_engine_sound_elseblock1324
alternate_engine_sound_ConditionalTrueBlock1323: ;Main true block ;keep 
	
; // Higher octave when turbo is engaged
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	jmp alternate_engine_sound_elsedoneblock1325
alternate_engine_sound_elseblock1324
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
alternate_engine_sound_elsedoneblock1325
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c8;keep
	bne alternate_engine_sound_elseblock1333
alternate_engine_sound_ConditionalTrueBlock1332: ;Main true block ;keep 
	
; // Iterate through several pitch values
	; Forcetype: NADA
	lda #$cd
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock1334
alternate_engine_sound_elseblock1333
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$cd;keep
	bne alternate_engine_sound_elseblock1361
alternate_engine_sound_ConditionalTrueBlock1360: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c3
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock1362
alternate_engine_sound_elseblock1361
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c3;keep
	bne alternate_engine_sound_elsedoneblock1376
alternate_engine_sound_ConditionalTrueBlock1374: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c8
	; Calling storevariable on generic assign expression
	sta sound_pitch
alternate_engine_sound_elsedoneblock1376
alternate_engine_sound_elsedoneblock1362
alternate_engine_sound_elsedoneblock1334
	
; // Set the pitch
	; Assigning memory location
	ldy #0 ; Fake 16 bit
	lda sound_pitch
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
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
	
; // eof === main logic ============================================================================================ 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : game_loop
	;    Procedure type : User-defined procedure
game_loop
game_loop_while1381
game_loop_loopstart1385
	; Binary clause Simplified: EQUALS
	clc
	lda game_over_flag
	; cmp #$00 ignored
	bne game_loop_elsedoneblock1384
game_loop_ConditionalTrueBlock1382: ;Main true block ;keep 
	
; // check input devices
	jsr check_input
	
; // 115 gosub2220:rem alt engine sound
; //alternate_engine_sound();
; // update positions
	jsr update_positions
	
; // 135 rem gosub2220:rem alt engine sound
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
	bne game_loop_elsedoneblock1398
game_loop_ConditionalTrueBlock1396: ;Main true block ;keep 
	
; // 170 rem gosub2220:rem alt engine sound
; //alternate_engine_sound();
; // 180 if ge then ge=0:goto40:rem new game
	rts
game_loop_elsedoneblock1398
	
; // Slow it down
; // @@TODO: Replace with interrupt routine
	lda game_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	jmp game_loop_while1381
game_loop_elsedoneblock1384
game_loop_loopend1386
	rts
end_procedure_game_loop
block1
main_block_begin_
	
; // Init
	lda #$00
	ldx #$80
	sta screen_loc
	stx screen_loc+1
	jsr set_uppercase
MainProgram_while1401
MainProgram_loopstart1405
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock1404
MainProgram_ConditionalTrueBlock1402: ;Main true block ;keep 
	
; // Primary loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_over_flag
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta do_music
	jsr title_screen
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta do_music
	jsr game_screen
	jsr game_loop
	jmp MainProgram_while1401
MainProgram_elsedoneblock1404
MainProgram_loopend1406
main_block_end_
	; End of program
	; Ending memory block at $410
check_input_printstring_text487	dc.b	"RETURN"
	dc.b	0
EndBlock410:

