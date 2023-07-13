
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
; // Scan keyboard directly
; // Bits 0-3 = row select
; //	----+------------------------
; //	row |  7  6  5  4  3  2  1  0
; //	    | 128 64 32 16 8  4  2  1
; //	----+------------------------
; //	 9  |  =  .  -- ^C  <  sp [ ^R   ^C = STOP, ^R = Reverse on
; //	 8  |  -  0  rs  >  -- ]  @  ls  rs = right shift, ls = left shift
; //	 7  |  +  2  --  ?  ,  n  v  x
; //	 6  |  3  1  ^M  ;  m  b  c  z   ^M = return
; //	 5  |  *  5  --  :  k  h  f  s
; //	 4  |  6  4  --  l  j  g  d  a
; //	 3  |  /  8  --  p  i  y  r  w
; //	 2  |  9  7  ^   o  u  t  e  q
; //	 1  | ^T  ^Q --  )  \  '  $  "   ^T = DEL, ^Q = cursor down
; //	 0  | ^]  ^S <-  (  &  %  #  !   ^] = cursor right, ^S = home
; //
; //	 W  = row 3, val 1	- P1 Up
; //	 8  = row 3, val 64	- P2 Down
; //	 A  = row 4, val 1	- P1 Left
; //	 D  = row 4, val 2	- P1 Right
; //	 6  = row 4, val 128	- P2 Right
; //	 4  = row 4, val 64	- P2 Left
; //	 S  = row 5, val 1	- P1 Down
; //	 5  = row 5, val 64 	- P2 Down
; //	 EN = row 6, val 32 	- Return	
; //	 RS = row 8, val 32 	- P2 Fire
; //	 SP = row 9, val 4	- P1 Fire
; // Scan the keyboard rows and process input
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
	
; // getin - read keyboard input
; //call(#$FFE4); 
; //check_input_val := _A;
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(check_input_val,2);
; //if (check_input_val <> 0 or (player_1_fire = 1 or player_2_fire = 1)) then begin 
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
cycle_text_right_wait_while556
cycle_text_right_wait_loopstart560
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq cycle_text_right_wait_localfailed668
	jmp cycle_text_right_wait_ConditionalTrueBlock557
cycle_text_right_wait_localfailed668
	jmp cycle_text_right_wait_elsedoneblock559
cycle_text_right_wait_ConditionalTrueBlock557: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
cycle_text_right_wait_forloop670
	
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
cycle_text_right_wait_rightvarInteger_var726 = $54
	sta cycle_text_right_wait_rightvarInteger_var726
	sty cycle_text_right_wait_rightvarInteger_var726+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc ctrw_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_right_wait_skip728
	iny
cycle_text_right_wait_skip728
	; Low bit binop:
	clc
	adc cycle_text_right_wait_rightvarInteger_var726
cycle_text_right_wait_wordAdd724
	sta cycle_text_right_wait_rightvarInteger_var726
	; High-bit binop
	tya
	adc cycle_text_right_wait_rightvarInteger_var726+1
	tay
	lda cycle_text_right_wait_rightvarInteger_var726
	sta screen_loc_work
	sty screen_loc_work+1
	; Binary clause Simplified: GREATEREQUAL
	; Load pointer array
	ldy i
	lda (screen_loc_work),y
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc cycle_text_right_wait_elseblock731
cycle_text_right_wait_ConditionalTrueBlock730: ;Main true block ;keep 
	
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
	jmp cycle_text_right_wait_elsedoneblock732
cycle_text_right_wait_elseblock731
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy i
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_wait_elsedoneblock732
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp num_trail;keep
	bcs cycle_text_right_wait_elseblock739
cycle_text_right_wait_ConditionalTrueBlock738: ;Main true block ;keep 
	
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
	jmp cycle_text_right_wait_elsedoneblock740
cycle_text_right_wait_elseblock739
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
cycle_text_right_wait_elsedoneblock740
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_wait_elsedoneblock748
cycle_text_right_wait_ConditionalTrueBlock746: ;Main true block ;keep 
	; Binary clause Simplified: GREATEREQUAL
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	; Compare with pure num / var optimization
	cmp #$80;keep
	bcc cycle_text_right_wait_elseblock761
cycle_text_right_wait_ConditionalTrueBlock760: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	sec
	; Forcetype: NADA
	sbc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
	jmp cycle_text_right_wait_elsedoneblock762
cycle_text_right_wait_elseblock761
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_wait_elsedoneblock762
cycle_text_right_wait_elsedoneblock748
	; Binary clause Simplified: NOTEQUALS
	clc
	jsr check_input
	; cmp #$00 ignored
	beq cycle_text_right_wait_elsedoneblock770
cycle_text_right_wait_ConditionalTrueBlock768: ;Main true block ;keep 
	rts
cycle_text_right_wait_elsedoneblock770
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_right_wait_loopstart671
	; Test Inc dec D
	inc i
	lda ctrw_num_chars
	cmp i ;keep
	beq cycle_text_right_wait_loopdone773
cycle_text_right_wait_loopnotdone774
	jmp cycle_text_right_wait_forloop670
cycle_text_right_wait_loopdone773
cycle_text_right_wait_loopend672
	jmp cycle_text_right_wait_while556
cycle_text_right_wait_elsedoneblock559
cycle_text_right_wait_loopend561
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
draw_title_screen_box_forloop777
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
draw_title_screen_box_loopstart778
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_title_screen_box_forloop777
draw_title_screen_box_loopdone782: ;keep
draw_title_screen_box_loopend779
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_title_screen_box_forloop783
	
; // Draw sides
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var793 = $54
	sta draw_title_screen_box_rightvarInteger_var793
	sty draw_title_screen_box_rightvarInteger_var793+1
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
	adc draw_title_screen_box_rightvarInteger_var793
draw_title_screen_box_wordAdd791
	sta draw_title_screen_box_rightvarInteger_var793
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var793+1
	tay
	lda draw_title_screen_box_rightvarInteger_var793
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
draw_title_screen_box_loopstart784
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_title_screen_box_forloop783
draw_title_screen_box_loopdone794: ;keep
draw_title_screen_box_loopend785
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
draw_game_screen_box_forloop798
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart799
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop798
draw_game_screen_box_loopdone803: ;keep
draw_game_screen_box_loopend800
	
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
draw_game_screen_box_forloop805
	; Forcetype: NADA
	lda #$40
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy i ; optimized, look out for bugs
	sta (screen_loc_work),y
draw_game_screen_box_loopstart806
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_game_screen_box_forloop805
draw_game_screen_box_loopdone810: ;keep
draw_game_screen_box_loopend807
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_game_screen_box_forloop811
	
; // Draw sides
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var821 = $54
	sta draw_game_screen_box_rightvarInteger_var821
	sty draw_game_screen_box_rightvarInteger_var821+1
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
	adc draw_game_screen_box_rightvarInteger_var821
draw_game_screen_box_wordAdd819
	sta draw_game_screen_box_rightvarInteger_var821
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var821+1
	tay
	lda draw_game_screen_box_rightvarInteger_var821
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
draw_game_screen_box_loopstart812
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_game_screen_box_forloop811
draw_game_screen_box_loopdone822: ;keep
draw_game_screen_box_loopend813
	
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
	
; // debug line
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
new_round_rightvarInteger_var831 = $54
	sta new_round_rightvarInteger_var831
	sty new_round_rightvarInteger_var831+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var831
new_round_wordAdd829
	sta new_round_rightvarInteger_var831
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var831+1
	tay
	lda new_round_rightvarInteger_var831
new_round_int_shift_var832 = $54
	sta new_round_int_shift_var832
	sty new_round_int_shift_var832+1
		lsr new_round_int_shift_var832+1
	ror new_round_int_shift_var832+0

	lda new_round_int_shift_var832
	ldy new_round_int_shift_var832+1
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
	bne new_round_localfailed838
	jmp new_round_ConditionalTrueBlock834
new_round_localfailed838: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_elsedoneblock836
new_round_ConditionalTrueBlock834: ;Main true block ;keep 
	
; // x, y, # chars
; // Adjust game speed based on round 
	; Forcetype: NADA
	lda #$1c
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock836
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_localfailed845
	jmp new_round_ConditionalTrueBlock841
new_round_localfailed845: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_elsedoneblock843
new_round_ConditionalTrueBlock841: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$18
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock843
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_localfailed852
	jmp new_round_ConditionalTrueBlock848
new_round_localfailed852: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_elsedoneblock850
new_round_ConditionalTrueBlock848: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock850
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_localfailed859
	jmp new_round_ConditionalTrueBlock855
new_round_localfailed859: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_elsedoneblock857
new_round_ConditionalTrueBlock855: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock857
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
new_game_rightvarInteger_var864 = $54
	sta new_game_rightvarInteger_var864
	sty new_game_rightvarInteger_var864+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_game_rightvarInteger_var864
new_game_wordAdd862
	sta new_game_rightvarInteger_var864
	; High-bit binop
	tya
	sbc new_game_rightvarInteger_var864+1
	tay
	lda new_game_rightvarInteger_var864
new_game_int_shift_var865 = $54
	sta new_game_int_shift_var865
	sty new_game_int_shift_var865+1
		lsr new_game_int_shift_var865+1
	ror new_game_int_shift_var865+0

	lda new_game_int_shift_var865
	ldy new_game_int_shift_var865+1
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
	bne check_game_state_localfailed872
	jmp check_game_state_ConditionalTrueBlock868
check_game_state_localfailed872: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock870
check_game_state_ConditionalTrueBlock868: ;Main true block ;keep 
	
; // Check for player crashed
	jsr player_crash
	jsr sound_stop
check_game_state_elsedoneblock870
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock876
check_game_state_localsuccess936: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock876
check_game_state_ConditionalTrueBlock875: ;Main true block ;keep 
	lda #<msg_both_crash
	ldx #>msg_both_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
	jmp check_game_state_elsedoneblock877
check_game_state_elseblock876
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock941
check_game_state_ConditionalTrueBlock940: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p2
	; Binary clause Simplified: LESS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock973
check_game_state_ConditionalTrueBlock971: ;Main true block ;keep 
	lda #<msg_p1_crash
	ldx #>msg_p1_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
check_game_state_elsedoneblock973
	jmp check_game_state_elsedoneblock942
check_game_state_elseblock941
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock980
check_game_state_ConditionalTrueBlock978: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p1
	; Binary clause Simplified: LESS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock992
check_game_state_ConditionalTrueBlock990: ;Main true block ;keep 
	lda #<msg_p2_crash
	ldx #>msg_p2_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
check_game_state_elsedoneblock992
check_game_state_elsedoneblock980
check_game_state_elsedoneblock942
check_game_state_elsedoneblock877
	; Binary clause Simplified: GREATEREQUAL
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elseblock997
check_game_state_ConditionalTrueBlock996: ;Main true block ;keep 
	
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
	jmp check_game_state_elsedoneblock998
check_game_state_elseblock997
	; Binary clause Simplified: GREATEREQUAL
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elsedoneblock1012
check_game_state_ConditionalTrueBlock1010: ;Main true block ;keep 
	lda #<msg_p2_wins
	ldx #>msg_p2_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_game
check_game_state_elsedoneblock1012
check_game_state_elsedoneblock998
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
	beq check_collisions_localfailed1028
	jmp check_collisions_ConditionalTrueBlock1017
check_collisions_localfailed1028: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1019
check_collisions_ConditionalTrueBlock1017: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1034
check_collisions_ConditionalTrueBlock1032: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
check_collisions_elsedoneblock1034
check_collisions_elsedoneblock1019
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1049
	jmp check_collisions_ConditionalTrueBlock1038
check_collisions_localfailed1049: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1040
check_collisions_ConditionalTrueBlock1038: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1055
check_collisions_ConditionalTrueBlock1053: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1055
check_collisions_elsedoneblock1040
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
	bcc update_positions_elsedoneblock1062
update_positions_ConditionalTrueBlock1060: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta turn_counter
update_positions_elsedoneblock1062
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1099
	jmp update_positions_ConditionalTrueBlock1066
update_positions_localfailed1099: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1068
update_positions_ConditionalTrueBlock1066: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1104
update_positions_ConditionalTrueBlock1102: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1117
update_positions_localsuccess1119: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_1_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_1_input;keep
	beq update_positions_elsedoneblock1117
update_positions_ConditionalTrueBlock1115: ;Main true block ;keep 
	
; // May need to tweak this value
; // Move P1 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_1_input
	; Calling storevariable on generic assign expression
	sta player_1_head
update_positions_elsedoneblock1117
update_positions_elsedoneblock1104
	
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
	beq update_positions_elsedoneblock1125
update_positions_ConditionalTrueBlock1123: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1125
update_positions_elsedoneblock1068
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1164
	jmp update_positions_ConditionalTrueBlock1131
update_positions_localfailed1164: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1133
update_positions_ConditionalTrueBlock1131: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1169
update_positions_ConditionalTrueBlock1167: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1182
update_positions_localsuccess1184: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_2_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_2_input;keep
	beq update_positions_elsedoneblock1182
update_positions_ConditionalTrueBlock1180: ;Main true block ;keep 
	
; // Move P2 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_2_input
	; Calling storevariable on generic assign expression
	sta player_2_head
update_positions_elsedoneblock1182
update_positions_elsedoneblock1169
	
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
	beq update_positions_elsedoneblock1190
update_positions_ConditionalTrueBlock1188: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1190
update_positions_elsedoneblock1133
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
	rts
end_procedure_player_crash
	
; // Explode in all directions:
; //
; //  *  *  *
; //   * * *
; //    ***
; //  *******
; //    *** 
; //   * * *
; //  *  *  *
; // 2265 j=.:for i=160 to 127 step-1
; // 2270 if c1=1 then pokesc+x1+y1,cd(j)
; // 2275 if c2=1 then pokesc+x2+y2,cd(j)
; // 2280 j=j+1:if j>2 then j=0
; // 2285 pokesf,i:next
; // sound stop
	; NodeProcedureDecl -1
	; ***********  Defining procedure : sound_stop
	;    Procedure type : User-defined procedure
sound_stop
	rts
end_procedure_sound_stop
	
; // 2255 pokesr,0:return
; // init engine sound
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init_engine_sound
	;    Procedure type : User-defined procedure
init_engine_sound
	
; // 2205 pokesr,16:rem enable sound
; // 2210 pokesv,se(0):rem octave
; // 2215 sp=200:rem pitch
	jsr alternate_engine_sound
	rts
end_procedure_init_engine_sound
	
; // alt engine sound
	; NodeProcedureDecl -1
	; ***********  Defining procedure : alternate_engine_sound
	;    Procedure type : User-defined procedure
alternate_engine_sound
	rts
end_procedure_alternate_engine_sound
	
; // 2225 if sp=200 then sp=205:goto2240
; // 2230 if sp=205 then sp=200
; // 2240 pokesf,sp:rem pitch === main logic ============================================================================================ 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : game_loop
	;    Procedure type : User-defined procedure
game_loop
game_loop_while1204
game_loop_loopstart1208
	; Binary clause Simplified: EQUALS
	clc
	lda game_over_flag
	; cmp #$00 ignored
	bne game_loop_elsedoneblock1207
game_loop_ConditionalTrueBlock1205: ;Main true block ;keep 
	
; // check input devices
	jsr check_input
	
; // 115 gosub2220:rem alt engine sound
; // update positions
	jsr update_positions
	
; // 135 rem gosub2220:rem alt engine sound
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
	bne game_loop_elsedoneblock1221
game_loop_ConditionalTrueBlock1219: ;Main true block ;keep 
	
; // 170 rem gosub2220:rem alt engine sound
; // 180 if ge then ge=0:goto40:rem new game
	rts
game_loop_elsedoneblock1221
	
; // Slow it down
; // @@TODO: Replace with interrupt routine
	lda game_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	jmp game_loop_while1204
game_loop_elsedoneblock1207
game_loop_loopend1209
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
MainProgram_while1224
MainProgram_loopstart1228
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock1227
MainProgram_ConditionalTrueBlock1225: ;Main true block ;keep 
	
; // Primary loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_over_flag
	jsr title_screen
	jsr game_screen
	jsr game_loop
	jmp MainProgram_while1224
MainProgram_elsedoneblock1227
MainProgram_loopend1229
main_block_end_
	; End of program
	; Ending memory block at $410
check_input_printstring_text487	dc.b	"RETURN"
	dc.b	0
EndBlock410:

