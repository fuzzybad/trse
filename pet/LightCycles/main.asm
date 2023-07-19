
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
Model_IRQFlag = $91
Model_CheckAvailMem = $c353
screen_loc	= $6A
screen_loc_work	= $6C
message_ptr	= $6E
song_ptr	= $70
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
game_end_music_arr	dc.b $0, $0, $7, $d2, $1, $2, $9e, $0
	dc.b $2, $ee, $1, $1, $e0, $1, $2, $95
	dc.b $1, $2, $0, $0, $1, $fb, $1, $1
	dc.b $8c, $1, $1, $fb, $1, $1, $bc, $1
	dc.b $4, $0, $0, $e, $0, $0, $0, 0
	dc.b 0, 0, 0, 0, 0, 0, 0, 0
	dc.b 0, 0
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
_ptr	= $72
_center_txt	dc.b	0
_mylen	dc.b	0
basic_print_block17
basic_print
	; Binary clause Simplified: NOTEQUALS
	clc
	lda _center_txt
	; cmp #$00 ignored
	beq basic_print_elsedoneblock21
basic_print_ConditionalTrueBlock19: ;Main true block ;keep 
	; Binary clause Simplified: LESS
	lda _mylen
	; Compare with pure num / var optimization
	cmp #$28;keep
	bcs basic_print_elsedoneblock45
basic_print_ConditionalTrueBlock43: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta bp_i
basic_print_forloop54
	
; // Center text
	jsr cursor_right
basic_print_loopstart55
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
	bne basic_print_forloop54
basic_print_loopdone59: ;keep
basic_print_loopend56
basic_print_elsedoneblock45
basic_print_elsedoneblock21
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta next_ch
basic_print_while60
basic_print_loopstart64
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	; cmp #$00 ignored
	beq basic_print_elsedoneblock63
basic_print_ConditionalTrueBlock61: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy next_ch
	lda (_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc next_ch
	jmp basic_print_while60
basic_print_elsedoneblock63
basic_print_loopend65
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
basic_printat_block68
basic_printat
	
; // Home cursor
	jsr cursor_home
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myx
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock72
basic_printat_ConditionalTrueBlock70: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop81
	
; // Cursor right
	jsr cursor_right
basic_printat_loopstart82
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myx
	cmp pa_i ;keep
	bne basic_printat_forloop81
basic_printat_loopdone86: ;keep
basic_printat_loopend83
basic_printat_elsedoneblock72
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda _pa_myy
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc basic_printat_elsedoneblock90
basic_printat_ConditionalTrueBlock88: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_i
basic_printat_forloop99
	
; // Cursor down
	jsr cursor_down
basic_printat_loopstart100
	; Compare is onpage
	; Test Inc dec D
	inc pa_i
	lda _pa_myy
	cmp pa_i ;keep
	bne basic_printat_forloop99
basic_printat_loopdone104: ;keep
basic_printat_loopend101
basic_printat_elsedoneblock90
	
; // Print text
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta pa_next_ch
basic_printat_while105
basic_printat_loopstart109
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	; cmp #$00 ignored
	beq basic_printat_elsedoneblock108
basic_printat_ConditionalTrueBlock106: ;Main true block ;keep 
	; Assigning to register
	; Assigning register : _a
	; Load pointer array
	ldy pa_next_ch
	lda (_pa_ptr),y
	jsr $FFD2
	; Test Inc dec D
	inc pa_next_ch
	jmp basic_printat_while105
basic_printat_elsedoneblock108
basic_printat_loopend110
	rts
end_procedure_basic_printat
	
; // Delay function
	; NodeProcedureDecl -1
	; ***********  Defining procedure : do_delay
	;    Procedure type : User-defined procedure
delay_val	dc.b	0
do_delay_block113
do_delay
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda delay_val
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc do_delay_elsedoneblock117
do_delay_ConditionalTrueBlock115: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta j
do_delay_forloop126
	; Wait
	; Forcetype: NADA
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
do_delay_loopstart127
	; Compare is onpage
	; Test Inc dec D
	inc j
	lda delay_val
	cmp j ;keep
	bne do_delay_forloop126
do_delay_loopdone131: ;keep
do_delay_loopend128
do_delay_elsedoneblock117
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
	bne play_music_elsedoneblock136
play_music_ConditionalTrueBlock134: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	; Load pointer array
	ldy music_idx
	lda (song_ptr),y
	; cmp #$00 ignored
	bne play_music_elsedoneblock149
play_music_localsuccess151: ;keep
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
	bne play_music_elsedoneblock149
play_music_ConditionalTrueBlock147: ;Main true block ;keep 
	
; // Loop over note array
; // 	Structure - note value, octave, sustain value 
; // Change note when index is zero
; // End tune when pitch & sustain both = 0
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta music_idx
play_music_elsedoneblock149
	
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
play_music_elsedoneblock136
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
cycle_text_block153
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
	bne cycle_text_elseblock156
cycle_text_ConditionalTrueBlock155: ;Main true block ;keep 
	
; // Value of zero means we cycle forever
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta cts_num_cycles
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta scroll_step
	jmp cycle_text_elsedoneblock157
cycle_text_elseblock156
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_step
cycle_text_elsedoneblock157
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock165
cycle_text_ConditionalTrueBlock163: ;Main true block ;keep 
	
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
cycle_text_elsedoneblock165
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta k
cycle_text_forloop168
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_dir
	; cmp #$00 ignored
	beq cycle_text_elseblock189
cycle_text_ConditionalTrueBlock188: ;Main true block ;keep 
	
; // Scroll for the given # cycles
	jsr cycle_text_left
	jmp cycle_text_elsedoneblock190
cycle_text_elseblock189
	jsr cycle_text_right
cycle_text_elsedoneblock190
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_inp_brk
	; cmp #$00 ignored
	beq cycle_text_elsedoneblock198
cycle_text_ConditionalTrueBlock196: ;Main true block ;keep 
	
; // Break on input
	rts
cycle_text_elsedoneblock198
cycle_text_loopstart169
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
	bne cycle_text_forloop168
cycle_text_loopdone201: ;keep
cycle_text_loopend170
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
cycle_text_right_forloop203
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
cycle_text_right_rightvarInteger_var265 = $54
	sta cycle_text_right_rightvarInteger_var265
	sty cycle_text_right_rightvarInteger_var265+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_right_skip267
	iny
cycle_text_right_skip267
	; Low bit binop:
	clc
	adc cycle_text_right_rightvarInteger_var265
cycle_text_right_wordAdd263
	sta cycle_text_right_rightvarInteger_var265
	; High-bit binop
	tya
	adc cycle_text_right_rightvarInteger_var265+1
	tay
	lda cycle_text_right_rightvarInteger_var265
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
	bcs cycle_text_right_elseblock270
cycle_text_right_ConditionalTrueBlock269: ;Main true block ;keep 
	
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
	jmp cycle_text_right_elsedoneblock271
cycle_text_right_elseblock270
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
cycle_text_right_elsedoneblock271
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock279
cycle_text_right_ConditionalTrueBlock277: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_right_elsedoneblock279
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock285
cycle_text_right_ConditionalTrueBlock283: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_right_elsedoneblock303
cycle_text_right_ConditionalTrueBlock301: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_right_elsedoneblock303
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock309
cycle_text_right_ConditionalTrueBlock307: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_right_elsedoneblock309
cycle_text_right_elsedoneblock285
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_right_elsedoneblock315
cycle_text_right_ConditionalTrueBlock313: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_right_elsedoneblock315
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_right_loopstart204
	; Test Inc dec D
	inc i
	lda scroll_num_char
	cmp i ;keep
	beq cycle_text_right_loopdone318
cycle_text_right_loopnotdone319
	jmp cycle_text_right_forloop203
cycle_text_right_loopdone318
cycle_text_right_loopend205
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
cycle_text_left_forloop321
	
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
cycle_text_left_rightvarInteger_var383 = $54
	sta cycle_text_left_rightvarInteger_var383
	sty cycle_text_left_rightvarInteger_var383+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy screen_loc+1 ;keep
	lda screen_loc
	clc
	adc scroll_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc cycle_text_left_skip385
	iny
cycle_text_left_skip385
	; Low bit binop:
	clc
	adc cycle_text_left_rightvarInteger_var383
cycle_text_left_wordAdd381
	sta cycle_text_left_rightvarInteger_var383
	; High-bit binop
	tya
	adc cycle_text_left_rightvarInteger_var383+1
	tay
	lda cycle_text_left_rightvarInteger_var383
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
	bcs cycle_text_left_elseblock388
cycle_text_left_ConditionalTrueBlock387: ;Main true block ;keep 
	
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
	jmp cycle_text_left_elsedoneblock389
cycle_text_left_elseblock388
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
cycle_text_left_elsedoneblock389
	; Binary clause Simplified: EQUALS
	lda temp_byte
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock397
cycle_text_left_ConditionalTrueBlock395: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load pointer array
	ldy j
	lda (screen_loc_work),y
	clc
	; Forcetype: NADA
	adc #$80
	; Storing to a pointer
	sta (screen_loc_work),y
cycle_text_left_elsedoneblock397
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_play_music
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock403
cycle_text_left_ConditionalTrueBlock401: ;Main true block ;keep 
	
; // Play theme song
	jsr play_music
cycle_text_left_elsedoneblock403
	; Binary clause Simplified: NOTEQUALS
	clc
	lda cts_input_brk
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock409
cycle_text_left_ConditionalTrueBlock407: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	jsr check_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_left_elsedoneblock427
cycle_text_left_ConditionalTrueBlock425: ;Main true block ;keep 
	
; // If scrolling can be interrupted
; // Break on 'fire'
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scroll_inp_brk
	rts
cycle_text_left_elsedoneblock427
	; Binary clause Simplified: NOTEQUALS
	clc
	lda scroll_input
	; cmp #$00 ignored
	beq cycle_text_left_elsedoneblock433
cycle_text_left_ConditionalTrueBlock431: ;Main true block ;keep 
	
; // If additional input handling required
	jsr cycle_text_input_handler
cycle_text_left_elsedoneblock433
cycle_text_left_elsedoneblock409
	lda scroll_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
cycle_text_left_loopstart322
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
	beq cycle_text_left_loopdone436
cycle_text_left_loopnotdone437
	jmp cycle_text_left_forloop321
cycle_text_left_loopdone436
cycle_text_left_loopend323
	rts
end_procedure_cycle_text_left
	; NodeProcedureDecl -1
	; ***********  Defining procedure : check_input
	;    Procedure type : User-defined procedure
check_input_val	dc.b	0
check_input_block438
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
	bne check_input_elsedoneblock442
check_input_ConditionalTrueBlock440: ;Main true block ;keep 
	
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
check_input_elsedoneblock442
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
	beq check_input_elsedoneblock448
check_input_ConditionalTrueBlock446: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock448
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
	beq check_input_elsedoneblock454
check_input_ConditionalTrueBlock452: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock454
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
	beq check_input_elsedoneblock460
check_input_ConditionalTrueBlock458: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock460
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
	beq check_input_elsedoneblock466
check_input_ConditionalTrueBlock464: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock466
	; Binary clause Simplified: EQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$30
	 ; end add / sub var with constant
	; cmp #$00 ignored
	bne check_input_elsedoneblock472
check_input_ConditionalTrueBlock470: ;Main true block ;keep 
	
; // Down
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock472
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
	beq check_input_elsedoneblock478
check_input_ConditionalTrueBlock476: ;Main true block ;keep 
	
; // Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock478
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
	beq check_input_elsedoneblock484
check_input_ConditionalTrueBlock482: ;Main true block ;keep 
	
; // Left
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock484
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
	beq check_input_elsedoneblock490
check_input_ConditionalTrueBlock488: ;Main true block ;keep 
	
; // Right
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock490
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
	beq check_input_elsedoneblock496
check_input_ConditionalTrueBlock494: ;Main true block ;keep 
	
; // Up
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock496
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta j
check_input_forloop499
	
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
	bne check_input_elsedoneblock687
check_input_ConditionalTrueBlock685: ;Main true block ;keep 
check_input_elsedoneblock687
	
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elsedoneblock693
check_input_ConditionalTrueBlock691: ;Main true block ;keep 
check_input_elsedoneblock693
	
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne check_input_elsedoneblock699
check_input_ConditionalTrueBlock697: ;Main true block ;keep 
check_input_elsedoneblock699
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne check_input_elsedoneblock705
check_input_ConditionalTrueBlock703: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock723
check_input_ConditionalTrueBlock721: ;Main true block ;keep 
	
; // W - P1 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock723
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock729
check_input_ConditionalTrueBlock727: ;Main true block ;keep 
	
; // 8 - P2 Up
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock729
check_input_elsedoneblock705
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne check_input_elsedoneblock735
check_input_ConditionalTrueBlock733: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock765
check_input_ConditionalTrueBlock763: ;Main true block ;keep 
	
; // A - P1 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock765
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$2
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock771
check_input_ConditionalTrueBlock769: ;Main true block ;keep 
	
; // D - P1 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock771
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock777
check_input_ConditionalTrueBlock775: ;Main true block ;keep 
	
; // 4 - P2 Left
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock777
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$80
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock783
check_input_ConditionalTrueBlock781: ;Main true block ;keep 
	
; // 6 - P2 Right
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock783
check_input_elsedoneblock735
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$5;keep
	bne check_input_elsedoneblock789
check_input_ConditionalTrueBlock787: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$1
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock807
check_input_ConditionalTrueBlock805: ;Main true block ;keep 
	
; // S - P1 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_1_input
check_input_elsedoneblock807
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$40
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock813
check_input_ConditionalTrueBlock811: ;Main true block ;keep 
	
; // 5 - P2 Down
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_input
check_input_elsedoneblock813
check_input_elsedoneblock789
	
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
	bne check_input_elsedoneblock819
check_input_ConditionalTrueBlock817: ;Main true block ;keep 
check_input_elsedoneblock819
	
; //			moveto(1,1,hi(screen_char_loc));
; //			printdecimal(check_input_val,2);
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$7;keep
	bne check_input_elsedoneblock825
check_input_ConditionalTrueBlock823: ;Main true block ;keep 
check_input_elsedoneblock825
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$8;keep
	bne check_input_elsedoneblock831
check_input_ConditionalTrueBlock829: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$20
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock843
check_input_ConditionalTrueBlock841: ;Main true block ;keep 
	
; // RIGHT SHIFT - P1 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_fire
check_input_elsedoneblock843
check_input_elsedoneblock831
	; Binary clause Simplified: EQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$9;keep
	bne check_input_elsedoneblock849
check_input_ConditionalTrueBlock847: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	; 8 bit binop
	; Add/sub where right value is constant number
	lda check_input_val
	; Forcetype: NADA
	and #$4
	 ; end add / sub var with constant
	; cmp #$00 ignored
	beq check_input_elsedoneblock861
check_input_ConditionalTrueBlock859: ;Main true block ;keep 
	
; // SPACE - P2 Fire
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_fire
check_input_elsedoneblock861
check_input_elsedoneblock849
check_input_loopstart500
	; Test Inc dec D
	inc j
	; Forcetype: NADA
	lda #$a
	cmp j ;keep
	beq check_input_loopdone864
check_input_loopnotdone865
	jmp check_input_forloop499
check_input_loopdone864
check_input_loopend501
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_localfailed872
	jmp check_input_ConditionalTrueBlock867
check_input_localfailed872: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_input_elseblock868
check_input_ConditionalTrueBlock867: ;Main true block ;keep 
	
; // getin - read kernal keyboard input
; //call(#$FFE4); 
; //check_input_val := _A;
; //moveto(1,2,hi(screen_char_loc));
; //printdecimal(check_input_val,2);
	; Forcetype: NADA
	lda #$1
	rts
	jmp check_input_elsedoneblock869
check_input_elseblock868
	; Forcetype: NADA
	lda #$0
	rts
check_input_elsedoneblock869
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
	bne display_game_mode_elsedoneblock879
display_game_mode_ConditionalTrueBlock877: ;Main true block ;keep 
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
display_game_mode_elsedoneblock879
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne display_game_mode_elsedoneblock885
display_game_mode_ConditionalTrueBlock883: ;Main true block ;keep 
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
display_game_mode_elsedoneblock885
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
	bne cycle_text_input_handler_localfailed894
	jmp cycle_text_input_handler_ConditionalTrueBlock890
cycle_text_input_handler_localfailed894: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne cycle_text_input_handler_elsedoneblock892
cycle_text_input_handler_ConditionalTrueBlock890: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // two player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock892
	; Binary clause Simplified: EQUALS
	lda player_1_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_localfailed901
	jmp cycle_text_input_handler_ConditionalTrueBlock897
cycle_text_input_handler_localfailed901: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_input
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne cycle_text_input_handler_elsedoneblock899
cycle_text_input_handler_ConditionalTrueBlock897: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta game_mode
	
; // one player
	jsr display_game_mode
cycle_text_input_handler_elsedoneblock899
	rts
end_procedure_cycle_text_input_handler
	
; // Update score disp
	; NodeProcedureDecl -1
	; ***********  Defining procedure : update_score
	;    Procedure type : User-defined procedure
us_do_beep	dc.b	0
update_score_block903
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
	beq update_score_elsedoneblock907
update_score_ConditionalTrueBlock905: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
update_score_forloop940
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock960
update_score_ConditionalTrueBlock958: ;Main true block ;keep 
	
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
update_score_elsedoneblock960
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq update_score_elsedoneblock966
update_score_ConditionalTrueBlock964: ;Main true block ;keep 
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
update_score_elsedoneblock966
	
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
update_score_loopstart941
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$6
	cmp i ;keep
	bne update_score_forloop940
update_score_loopdone969: ;keep
update_score_loopend942
update_score_elsedoneblock907
	rts
end_procedure_update_score
	
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
draw_title_screen_box_forloop972
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
draw_title_screen_box_loopstart973
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$28
	cmp i ;keep
	bne draw_title_screen_box_forloop972
draw_title_screen_box_loopdone977: ;keep
draw_title_screen_box_loopend974
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta i
draw_title_screen_box_forloop978
	
; // Draw sides
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_title_screen_box_rightvarInteger_var988 = $54
	sta draw_title_screen_box_rightvarInteger_var988
	sty draw_title_screen_box_rightvarInteger_var988+1
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
	adc draw_title_screen_box_rightvarInteger_var988
draw_title_screen_box_wordAdd986
	sta draw_title_screen_box_rightvarInteger_var988
	; High-bit binop
	tya
	adc draw_title_screen_box_rightvarInteger_var988+1
	tay
	lda draw_title_screen_box_rightvarInteger_var988
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
draw_title_screen_box_loopstart979
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_title_screen_box_forloop978
draw_title_screen_box_loopdone989: ;keep
draw_title_screen_box_loopend980
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
	
; // Start animation interrupt
	jsr play_animate_irq
	
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
	
; // Stop animation interrupt
	; Forcetype: NADA
	lda #$3c
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	rts
end_procedure_title_screen
	
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
	; Forcetype: NADA
	lda #$28
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
	; Generic 16 bit op
	ldy screen_loc+1 ;keep
	lda screen_loc
draw_game_screen_box_rightvarInteger_var1017 = $54
	sta draw_game_screen_box_rightvarInteger_var1017
	sty draw_game_screen_box_rightvarInteger_var1017+1
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
	adc draw_game_screen_box_rightvarInteger_var1017
draw_game_screen_box_wordAdd1015
	sta draw_game_screen_box_rightvarInteger_var1017
	; High-bit binop
	tya
	adc draw_game_screen_box_rightvarInteger_var1017+1
	tay
	lda draw_game_screen_box_rightvarInteger_var1017
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
draw_game_screen_box_loopstart1008
	; Compare is onpage
	; Test Inc dec D
	inc i
	; Forcetype: NADA
	lda #$18
	cmp i ;keep
	bne draw_game_screen_box_forloop1007
draw_game_screen_box_loopdone1018: ;keep
draw_game_screen_box_loopend1009
	
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
	bne game_screen_elsedoneblock1027
game_screen_ConditionalTrueBlock1025: ;Main true block ;keep 
	
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
game_screen_elsedoneblock1027
	; Binary clause Simplified: EQUALS
	lda game_mode
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne game_screen_elsedoneblock1033
game_screen_ConditionalTrueBlock1031: ;Main true block ;keep 
	
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
game_screen_elsedoneblock1033
	
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
new_round_rightvarInteger_var1039 = $54
	sta new_round_rightvarInteger_var1039
	sty new_round_rightvarInteger_var1039+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var1039
new_round_wordAdd1037
	sta new_round_rightvarInteger_var1039
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var1039+1
	tay
	lda new_round_rightvarInteger_var1039
new_round_int_shift_var1040 = $54
	sta new_round_int_shift_var1040
	sty new_round_int_shift_var1040+1
		lsr new_round_int_shift_var1040+1
	ror new_round_int_shift_var1040+0

	lda new_round_int_shift_var1040
	ldy new_round_int_shift_var1040+1
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
new_round_rightvarInteger_var1043 = $54
	sta new_round_rightvarInteger_var1043
	sty new_round_rightvarInteger_var1043+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_round_rightvarInteger_var1043
new_round_wordAdd1041
	sta new_round_rightvarInteger_var1043
	; High-bit binop
	tya
	sbc new_round_rightvarInteger_var1043+1
	tay
	lda new_round_rightvarInteger_var1043
new_round_int_shift_var1044 = $54
	sta new_round_int_shift_var1044
	sty new_round_int_shift_var1044+1
		lsr new_round_int_shift_var1044+1
	ror new_round_int_shift_var1044+0

	lda new_round_int_shift_var1044
	ldy new_round_int_shift_var1044+1
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
	bne new_round_localfailed1050
	jmp new_round_ConditionalTrueBlock1046
new_round_localfailed1050: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne new_round_elsedoneblock1048
new_round_ConditionalTrueBlock1046: ;Main true block ;keep 
	
; // Adjust game speed
	; Forcetype: NADA
	lda #$28
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1048
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_localfailed1057
	jmp new_round_ConditionalTrueBlock1053
new_round_localfailed1057: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne new_round_elsedoneblock1055
new_round_ConditionalTrueBlock1053: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1e
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1055
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_localfailed1064
	jmp new_round_ConditionalTrueBlock1060
new_round_localfailed1064: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne new_round_elsedoneblock1062
new_round_ConditionalTrueBlock1060: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$19
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1062
	; Binary clause Simplified: EQUALS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_localfailed1071
	jmp new_round_ConditionalTrueBlock1067
new_round_localfailed1071: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne new_round_elsedoneblock1069
new_round_ConditionalTrueBlock1067: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$14
	; Calling storevariable on generic assign expression
	sta game_speed
new_round_elsedoneblock1069
	
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
new_game_rightvarInteger_var1076 = $54
	sta new_game_rightvarInteger_var1076
	sty new_game_rightvarInteger_var1076+1
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$28
	; Low bit binop:
	sec
	sbc new_game_rightvarInteger_var1076
new_game_wordAdd1074
	sta new_game_rightvarInteger_var1076
	; High-bit binop
	tya
	sbc new_game_rightvarInteger_var1076+1
	tay
	lda new_game_rightvarInteger_var1076
new_game_int_shift_var1077 = $54
	sta new_game_int_shift_var1077
	sty new_game_int_shift_var1077+1
		lsr new_game_int_shift_var1077+1
	ror new_game_int_shift_var1077+0

	lda new_game_int_shift_var1077
	ldy new_game_int_shift_var1077+1
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
	lda #$14
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
	bne check_game_state_localfailed1084
	jmp check_game_state_ConditionalTrueBlock1080
check_game_state_localfailed1084: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1082
check_game_state_ConditionalTrueBlock1080: ;Main true block ;keep 
	
; // Check for player crashed
	jsr player_crash
	jsr stop_sound
check_game_state_elsedoneblock1082
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1244
check_game_state_localsuccess1245: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_localfailed1244
	jmp check_game_state_ConditionalTrueBlock1087
check_game_state_localfailed1244
	jmp check_game_state_elseblock1088
check_game_state_ConditionalTrueBlock1087: ;Main true block ;keep 
	lda #<msg_both_crash
	ldx #>msg_both_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$12
	; Calling storevariable on generic assign expression
	sta message_len
	jsr new_round
	jmp check_game_state_elsedoneblock1089
check_game_state_elseblock1088
	; Binary clause Simplified: EQUALS
	lda player_1_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elseblock1250
check_game_state_ConditionalTrueBlock1249: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p2
	; Binary clause Simplified: LESS
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1330
check_game_state_ConditionalTrueBlock1328: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1343
check_game_state_ConditionalTrueBlock1342: ;Main true block ;keep 
	lda #<msg_plr_crash
	ldx #>msg_plr_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$e
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1344
check_game_state_elseblock1343
	lda #<msg_p1_crash
	ldx #>msg_p1_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1344
	jsr new_round
check_game_state_elsedoneblock1330
	jmp check_game_state_elsedoneblock1251
check_game_state_elseblock1250
	; Binary clause Simplified: EQUALS
	lda player_2_crash
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne check_game_state_elsedoneblock1353
check_game_state_ConditionalTrueBlock1351: ;Main true block ;keep 
	
; // Increment score 
	; Test Inc dec D
	inc score_p1
	; Binary clause Simplified: LESS
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcs check_game_state_elsedoneblock1381
check_game_state_ConditionalTrueBlock1379: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1394
check_game_state_ConditionalTrueBlock1393: ;Main true block ;keep 
	lda #<msg_sark_crash
	ldx #>msg_sark_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$c
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1395
check_game_state_elseblock1394
	lda #<msg_p2_crash
	ldx #>msg_p2_crash
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1395
	jsr new_round
check_game_state_elsedoneblock1381
check_game_state_elsedoneblock1353
check_game_state_elsedoneblock1251
check_game_state_elsedoneblock1089
	; Binary clause Simplified: GREATEREQUAL
	lda score_p1
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elseblock1402
check_game_state_ConditionalTrueBlock1401: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1439
check_game_state_ConditionalTrueBlock1438: ;Main true block ;keep 
	
; // Check for end of game
	lda #<msg_plr_wins
	ldx #>msg_plr_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$b
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1440
check_game_state_elseblock1439
	lda #<msg_p1_wins
	ldx #>msg_p1_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1440
	jsr new_game
	jmp check_game_state_elsedoneblock1403
check_game_state_elseblock1402
	; Binary clause Simplified: GREATEREQUAL
	lda score_p2
	; Compare with pure num / var optimization
	cmp #$5;keep
	bcc check_game_state_elsedoneblock1449
check_game_state_ConditionalTrueBlock1447: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda game_mode
	; cmp #$00 ignored
	bne check_game_state_elseblock1462
check_game_state_ConditionalTrueBlock1461: ;Main true block ;keep 
	lda #<msg_sark_wins
	ldx #>msg_sark_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$9
	; Calling storevariable on generic assign expression
	sta message_len
	jmp check_game_state_elsedoneblock1463
check_game_state_elseblock1462
	lda #<msg_p2_wins
	ldx #>msg_p2_wins
	sta message_ptr
	stx message_ptr+1
	; Forcetype: NADA
	lda #$d
	; Calling storevariable on generic assign expression
	sta message_len
check_game_state_elsedoneblock1463
	jsr new_game
check_game_state_elsedoneblock1449
check_game_state_elsedoneblock1403
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
	beq check_collisions_localfailed1481
	jmp check_collisions_ConditionalTrueBlock1470
check_collisions_localfailed1481: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1472
check_collisions_ConditionalTrueBlock1470: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1487
check_collisions_ConditionalTrueBlock1485: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_1_crash
check_collisions_elsedoneblock1487
check_collisions_elsedoneblock1472
	; Binary clause Simplified: NOTEQUALS
	clc
	lda turn_counter
	; cmp #$00 ignored
	beq check_collisions_localfailed1502
	jmp check_collisions_ConditionalTrueBlock1491
check_collisions_localfailed1502: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq check_collisions_elsedoneblock1493
check_collisions_ConditionalTrueBlock1491: ;Main true block ;keep 
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
	beq check_collisions_elsedoneblock1508
check_collisions_ConditionalTrueBlock1506: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_crash
check_collisions_elsedoneblock1508
check_collisions_elsedoneblock1493
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
sark_crash_distance_block1511
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
sark_crash_distance_while1512
sark_crash_distance_loopstart1516
	; Binary clause Simplified: EQUALS
	clc
	lda scd_crash
	; cmp #$00 ignored
	bne sark_crash_distance_elsedoneblock1515
sark_crash_distance_ConditionalTrueBlock1513: ;Main true block ;keep 
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
	beq sark_crash_distance_elsedoneblock1533
sark_crash_distance_ConditionalTrueBlock1531: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta scd_crash
sark_crash_distance_elsedoneblock1533
	jmp sark_crash_distance_while1512
sark_crash_distance_elsedoneblock1515
sark_crash_distance_loopend1517
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
sark_move_block1536
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
	lda #$4
	; Calling storevariable on generic assign expression
	sta sm_cr_th
	; Forcetype: NADA
	lda #$5
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
	bcs sark_move_localfailed1583
	jmp sark_move_ConditionalTrueBlock1538
sark_move_localfailed1583
	jmp sark_move_elsedoneblock1540
sark_move_ConditionalTrueBlock1538: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	lda sm_head
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne sark_move_localfailed1607
	jmp sark_move_ConditionalTrueBlock1586
sark_move_localfailed1607: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda sm_head
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne sark_move_elseblock1587
sark_move_ConditionalTrueBlock1586: ;Main true block ;keep 
	
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
	; Binary clause Simplified: GREATER
	lda sm_tmp_byt1
	; Compare with pure num / var optimization
	cmp sm_tmp_byt2;keep
	bcc sark_move_elseblock1611
	beq sark_move_elseblock1611
sark_move_ConditionalTrueBlock1610: ;Main true block ;keep 
	
; // Switch to direction with longest distance
	; Forcetype: NADA
	lda #$3
	; Calling storevariable on generic assign expression
	sta player_2_head
	jmp sark_move_elsedoneblock1612
sark_move_elseblock1611
	; Forcetype: NADA
	lda #$4
	; Calling storevariable on generic assign expression
	sta player_2_head
sark_move_elsedoneblock1612
	jmp sark_move_elsedoneblock1588
sark_move_elseblock1587
	
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
	; Binary clause Simplified: GREATER
	lda sm_tmp_byt1
	; Compare with pure num / var optimization
	cmp sm_tmp_byt2;keep
	bcc sark_move_elseblock1620
	beq sark_move_elseblock1620
sark_move_ConditionalTrueBlock1619: ;Main true block ;keep 
	
; // Switch to direction with longest distance
	; Forcetype: NADA
	lda #$1
	; Calling storevariable on generic assign expression
	sta player_2_head
	jmp sark_move_elsedoneblock1621
sark_move_elseblock1620
	; Forcetype: NADA
	lda #$2
	; Calling storevariable on generic assign expression
	sta player_2_head
sark_move_elsedoneblock1621
sark_move_elsedoneblock1588
sark_move_elsedoneblock1540
	; Binary clause INTEGER: GREATER
	lda player_1_xy+1   ; compare high bytes
	cmp player_2_xy+1 ;keep
	bcc sark_move_localfailed1646
	bne sark_move_ConditionalTrueBlock1627
	lda player_1_xy
	cmp player_2_xy ;keep
	bcc sark_move_localfailed1646
	beq sark_move_localfailed1646
	jmp sark_move_ConditionalTrueBlock1627
sark_move_localfailed1646
	jmp sark_move_elseblock1628
sark_move_ConditionalTrueBlock1627: ;Main true block ;keep 
	
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
sark_move_rightvarInteger_var1653 = $54
	sta sark_move_rightvarInteger_var1653
	sty sark_move_rightvarInteger_var1653+1
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
	sbc sark_move_rightvarInteger_var1653
sark_move_wordAdd1649
	sta sark_move_rightvarInteger_var1653
	; High-bit binop
	tya
	sbc sark_move_rightvarInteger_var1653+1
	tay
	lda sark_move_rightvarInteger_var1653
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt2
	jmp sark_move_elsedoneblock1629
sark_move_elseblock1628
	
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
sark_move_rightvarInteger_var1661 = $54
	sta sark_move_rightvarInteger_var1661
	sty sark_move_rightvarInteger_var1661+1
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
	sbc sark_move_rightvarInteger_var1661
sark_move_wordAdd1657
	sta sark_move_rightvarInteger_var1661
	; High-bit binop
	tya
	sbc sark_move_rightvarInteger_var1661+1
	tay
	lda sark_move_rightvarInteger_var1661
	; Calling storevariable on generic assign expression
	sta sm_tmp_byt2
sark_move_elsedoneblock1629
	; Binary clause Simplified: LESS
	lda sm_tmp_byt1
	; Compare with pure num / var optimization
	cmp sm_trbo_th;keep
	bcs sark_move_elsedoneblock1666
sark_move_localsuccess1668: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	lda sm_tmp_byt2
	; Compare with pure num / var optimization
	cmp sm_trbo_th;keep
	bcs sark_move_elsedoneblock1666
sark_move_ConditionalTrueBlock1664: ;Main true block ;keep 
	
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
sark_move_elsedoneblock1666
	rts
end_procedure_sark_move
	
; // TODO: Every 'n' cycles:
; //	If opponent more than 'n' moves away, turn towards their predicted position
; //	(based on current heading) if safe.  Prioritize X over Y (Y has fewer possible moves)
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
	bne update_positions_elsedoneblock1674
update_positions_ConditionalTrueBlock1672: ;Main true block ;keep 
	
; // P2 routine for single player game
	jsr sark_move
update_positions_elsedoneblock1674
	
; // Check if move avail
	; Test Inc dec D
	inc turn_counter
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$2;keep
	bcc update_positions_elsedoneblock1680
update_positions_ConditionalTrueBlock1678: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta turn_counter
update_positions_elsedoneblock1680
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1717
	jmp update_positions_ConditionalTrueBlock1684
update_positions_localfailed1717: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_1_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1686
update_positions_ConditionalTrueBlock1684: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_1_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1722
update_positions_ConditionalTrueBlock1720: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1735
update_positions_localsuccess1737: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_1_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_1_input;keep
	beq update_positions_elsedoneblock1735
update_positions_ConditionalTrueBlock1733: ;Main true block ;keep 
	
; // May need to tweak this value
; // Move P1 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_1_input
	; Calling storevariable on generic assign expression
	sta player_1_head
update_positions_elsedoneblock1735
update_positions_elsedoneblock1722
	
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
	beq update_positions_elsedoneblock1743
update_positions_ConditionalTrueBlock1741: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1743
update_positions_elsedoneblock1686
	; Binary clause Simplified: EQUALS
	lda turn_counter
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_localfailed1782
	jmp update_positions_ConditionalTrueBlock1749
update_positions_localfailed1782: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: EQUALS
	lda player_2_fire
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne update_positions_elsedoneblock1751
update_positions_ConditionalTrueBlock1749: ;Main true block ;keep 
	; Binary clause Simplified: EQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	bne update_positions_elsedoneblock1787
update_positions_ConditionalTrueBlock1785: ;Main true block ;keep 
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_input
	; cmp #$00 ignored
	beq update_positions_elsedoneblock1800
update_positions_localsuccess1802: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: NOTEQUALS
	; Load Byte array
	; CAST type NADA
	ldx player_2_head
	lda dir_opp_arr,x 
	; Compare with pure num / var optimization
	cmp player_2_input;keep
	beq update_positions_elsedoneblock1800
update_positions_ConditionalTrueBlock1798: ;Main true block ;keep 
	
; // Move P2 if normal move is available or turbo engaged
; // Direction change only allowed when turbo not engaged
; // Verify input was received and not opposite of current dir
; // Set new heading
	lda player_2_input
	; Calling storevariable on generic assign expression
	sta player_2_head
update_positions_elsedoneblock1800
update_positions_elsedoneblock1787
	
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
	beq update_positions_elsedoneblock1808
update_positions_ConditionalTrueBlock1806: ;Main true block ;keep 
	
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
update_positions_elsedoneblock1808
update_positions_elsedoneblock1751
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
	
; // Play music & animate via raster interrupt
	; NodeProcedureDecl -1
	; ***********  Defining procedure : play_music_animate
	;    Procedure type : User-defined procedure
play_music_animate
	; ****** Inline assembler section
	pha
	txa
	pha
	tya
	
	
; // Play music inside the cycle_text() function instead for 2001 compatibility
; //play_music();
; // For debugging	
; //tmp_irq := tmp_irq + 1;
; //moveto(41,0,hi(screen_char_loc));
; //printdecimal(tmp_irq,2);
; // calling do_delay() causes crash
; //wait(255);
; //wait(255);
; //wait(255);
	; ****** Inline assembler section
	tay
	pla 
	tax
	pla
	jmp(Interrupts_org_irq)
	
	rts
end_procedure_play_music_animate
	
; // Routine to animate & play music via raster interrupt
; // NOTE: It seems only one interrupt can be running at a given time
; // NOTE: Raster not available on PET 2001 - use only for non-critical functions.
; // Init 'song_ptr' before calling:
; // 	song_ptr := #theme_music_arr;
	; NodeProcedureDecl -1
	; ***********  Defining procedure : play_animate_irq
	;    Procedure type : User-defined procedure
play_animate_irq
	
; //music_idx		:= 0;
; //music_sust_idx	:= 0;
; //LOC_SOUND_REG	:= 16;
; // Set interrupt function & enable
	lda #<play_music_animate
	ldy #>play_music_animate
	; Calling storevariable on generic assign expression
	sta Interrupts_addr
	sty Interrupts_addr+1
	jsr Interrupts_RasterIRQ
	; Forcetype: NADA
	lda #$3d
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	rts
end_procedure_play_animate_irq
	
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
player_crash_forloop1823
	
; // Increment animation		
	lda tmp
	clc
	adc #$01
	sta tmp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc player_crash_WordAdd1851
	inc tmp+1
player_crash_WordAdd1851
	; Binary clause INTEGER: GREATER
	lda tmp+1   ; compare high bytes
	cmp #$00 ;keep
	bcc player_crash_elsedoneblock1855
	bne player_crash_ConditionalTrueBlock1853
	lda tmp
	cmp #$02 ;keep
	bcc player_crash_elsedoneblock1855
	beq player_crash_elsedoneblock1855
player_crash_ConditionalTrueBlock1853: ;Main true block ;keep 
	; Forcetype: INTEGER
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta tmp
	sty tmp+1
player_crash_elsedoneblock1855
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_1_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock1861
player_crash_ConditionalTrueBlock1859: ;Main true block ;keep 
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
player_crash_elsedoneblock1861
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_crash
	; cmp #$00 ignored
	beq player_crash_elsedoneblock1869
player_crash_ConditionalTrueBlock1867: ;Main true block ;keep 
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
player_crash_elsedoneblock1869
	
; // Make sound
	lda i
	; Calling storevariable on generic assign expression
	sta $e848
	; Forcetype: NADA
	lda #$10
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
player_crash_loopstart1824
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
	beq player_crash_loopdone1874
player_crash_loopnotdone1875
	jmp player_crash_forloop1823
player_crash_loopdone1874
player_crash_loopend1825
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
	beq alternate_engine_sound_localfailed1884
	jmp alternate_engine_sound_ConditionalTrueBlock1879
alternate_engine_sound_localfailed1884: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: NOTEQUALS
	clc
	lda player_2_fire
	; cmp #$00 ignored
	beq alternate_engine_sound_elseblock1880
alternate_engine_sound_ConditionalTrueBlock1879: ;Main true block ;keep 
	
; // Higher octave when turbo is engaged
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
	jmp alternate_engine_sound_elsedoneblock1881
alternate_engine_sound_elseblock1880
	; Load Byte array
	; CAST type NADA
	lda sound_oct_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $e84a
alternate_engine_sound_elsedoneblock1881
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c8;keep
	bne alternate_engine_sound_elseblock1889
alternate_engine_sound_ConditionalTrueBlock1888: ;Main true block ;keep 
	
; // Iterate through several pitch values
	; Forcetype: NADA
	lda #$cd
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock1890
alternate_engine_sound_elseblock1889
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$cd;keep
	bne alternate_engine_sound_elseblock1917
alternate_engine_sound_ConditionalTrueBlock1916: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c3
	; Calling storevariable on generic assign expression
	sta sound_pitch
	jmp alternate_engine_sound_elsedoneblock1918
alternate_engine_sound_elseblock1917
	; Binary clause Simplified: EQUALS
	lda sound_pitch
	; Compare with pure num / var optimization
	cmp #$c3;keep
	bne alternate_engine_sound_elsedoneblock1932
alternate_engine_sound_ConditionalTrueBlock1930: ;Main true block ;keep 
	; Forcetype: NADA
	lda #$c8
	; Calling storevariable on generic assign expression
	sta sound_pitch
alternate_engine_sound_elsedoneblock1932
alternate_engine_sound_elsedoneblock1918
alternate_engine_sound_elsedoneblock1890
	
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
game_loop_while1937
game_loop_loopstart1941
	; Binary clause Simplified: EQUALS
	clc
	lda game_over_flag
	; cmp #$00 ignored
	bne game_loop_elsedoneblock1940
game_loop_ConditionalTrueBlock1938: ;Main true block ;keep 
	
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
	bne game_loop_elsedoneblock1954
game_loop_ConditionalTrueBlock1952: ;Main true block ;keep 
	rts
game_loop_elsedoneblock1954
	
; // Slow it down
; // @@TODO: Replace with interrupt routine
	lda game_speed
	; Calling storevariable on generic assign expression
	sta delay_val
	jsr do_delay
	jmp game_loop_while1937
game_loop_elsedoneblock1940
game_loop_loopend1942
	rts
end_procedure_game_loop
block1
main_block_begin_
	
; // Init
; // Disable interrupts
	; Forcetype: NADA
	lda #$3c
	; Calling storevariable on generic assign expression
	sta Interrupts_interruptFlag
	lda #$00
	ldx #$80
	sta screen_loc
	stx screen_loc+1
	jsr set_uppercase
MainProgram_while1958
MainProgram_loopstart1962
	; Binary clause Simplified: NOTEQUALS
	clc
	; Forcetype: NADA
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_elsedoneblock1961
MainProgram_ConditionalTrueBlock1959: ;Main true block ;keep 
	
; // Primary loop
	; Forcetype: NADA
	lda #$0
	; Calling storevariable on generic assign expression
	sta game_over_flag
	jsr title_screen
	jsr game_screen
	jsr game_loop
	jmp MainProgram_while1958
MainProgram_elsedoneblock1961
MainProgram_loopend1963
main_block_end_
	; End of program
	; Ending memory block at $410
EndBlock410:

