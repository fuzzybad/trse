
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
C64Project
	; LineNumber: 699
	jmp block1
	; LineNumber: 9
Key_keys	dc.b	 
	org Key_keys+8
	; LineNumber: 10
Key_keysLast	dc.b	 
	org Key_keysLast+8
	; LineNumber: 11
Key_keysPressed	dc.b	 
	org Key_keysPressed+8
	; LineNumber: 14
Key_keyState	dc.b	0
	; LineNumber: 18
Key_temp	dc.b	0
	; LineNumber: 20
Key_KeyRow = $dc00
	; LineNumber: 21
Key_KeyRead = $dc01
	; LineNumber: 23
Key_DataDirA = $dc02
	; LineNumber: 24
Key_DataDirB = $dc03
	; LineNumber: 26
Key_keyRowValues	dc.b $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f
	; LineNumber: 44
	; Ending memory block at $810
	; LineNumber: 45
	; Ending memory block at $810
	; LineNumber: 46
	; Ending memory block at $810
	; LineNumber: 54
	; Ending memory block at $810
	; LineNumber: 55
	; Ending memory block at $810
	; LineNumber: 56
	; Ending memory block at $810
	; LineNumber: 57
	; Ending memory block at $810
	; LineNumber: 58
	; Ending memory block at $810
	; LineNumber: 59
	; Ending memory block at $810
	; LineNumber: 60
	; Ending memory block at $810
	; LineNumber: 62
	; Ending memory block at $810
	; LineNumber: 64
	; Ending memory block at $810
	; LineNumber: 65
	; Ending memory block at $810
	; LineNumber: 68
	; Ending memory block at $810
	; LineNumber: 72
	; Ending memory block at $810
	; LineNumber: 76
	; Ending memory block at $810
	; LineNumber: 89
	; Ending memory block at $810
	; LineNumber: 90
	; Ending memory block at $810
	; LineNumber: 91
	; Ending memory block at $810
	; LineNumber: 96
	; Ending memory block at $810
	; LineNumber: 98
	; Ending memory block at $810
	; LineNumber: 99
	; Ending memory block at $810
	; LineNumber: 101
	; Ending memory block at $810
	; LineNumber: 102
	; Ending memory block at $810
	; LineNumber: 104
	; Ending memory block at $810
	; LineNumber: 107
	; Ending memory block at $810
	; LineNumber: 110
	; Ending memory block at $810
	; LineNumber: 113
	; Ending memory block at $810
	; LineNumber: 116
	; Ending memory block at $810
	; LineNumber: 119
	; Ending memory block at $810
	; LineNumber: 124
	; Ending memory block at $810
	; LineNumber: 127
	; Ending memory block at $810
	; ***********  Defining procedure : Key_Read
	;    Procedure type : User-defined procedure
	; LineNumber: 243
Key_Read
	; LineNumber: 245
	lda #$0
	; Calling storevariable on generic assign expression
	sta Key_keyState
	; LineNumber: 248
	
; // set data port direction
	lda #$ff
	; Calling storevariable on generic assign expression
	sta Key_DataDirA
	; LineNumber: 249
	lda #$0
	; Calling storevariable on generic assign expression
	sta Key_DataDirB
	; LineNumber: 278
	
; // row to test
	; Calling storevariable on generic assign expression
	sta Key_temp
Key_Read_forloop3
	; LineNumber: 252
	; LineNumber: 254
	; Load Byte array
	; CAST type NADA
	ldx Key_temp
	lda Key_keyRowValues,x 
	; Calling storevariable on generic assign expression
	sta Key_KeyRow
	; LineNumber: 256
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
		
		
	; LineNumber: 277
Key_Read_loopstart4
	; Compare is onpage
	; Test Inc dec D
	inc Key_temp
	lda #$8
	cmp Key_temp ;keep
	bne Key_Read_forloop3
Key_Read_loopdone8: ;keep
Key_Read_loopend5
	; LineNumber: 279
	rts
end_procedure_Key_Read
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Key_Held
	;    Procedure type : User-defined procedure
	; LineNumber: 284
	; LineNumber: 283
Key_Held_block9
Key_Held
	; LineNumber: 286
	
	sty Key_temp
	lda Key_keys,x
	and Key_temp 	; contains column 
	
	
	; LineNumber: 294
	rts
end_procedure_Key_Held
	; NodeProcedureDecl 0
	; Ending memory block at $810
EndBlock810:
	org $1000
StartBlock1000:
	org $1000
	incbin	"C:/Users/fuzzy/Dropbox/Development/trse/c64/vcf-mw-2026//assets/_Monkey_Island_Theme_Hokutu_Force.dat"
EndBlock1000:
	org $2200
StartBlock2200:
	; Starting new memory block at $2200
raster_sid_brk_pos	dc.b	$28
	; Resuming memory block at $2200
title_text_color	dc.b	$0a
	; Resuming memory block at $2200
subtext_color	dc.b	$04
	; Resuming memory block at $2200
raster_title_brk_pos	dc.b	$7b
	; Resuming memory block at $2200
raster_title_brk2_pos	dc.b	$82
	; Resuming memory block at $2200
raster_vcf_title_brk_pos	dc.b	$7b
	; Resuming memory block at $2200
raster_vcf_title_brk2_pos	dc.b	$82
	; Resuming memory block at $2200
raster_troll_brk_pos	dc.b	$6d
	; Resuming memory block at $2200
title_display_idx	dc.b	$00
	; Resuming memory block at $2200
title_display_max	dc.b	$06
	; Resuming memory block at $2200
title_display_arr	dc.b $0, $1, $0, $2, $0, $3, $0
	; Resuming memory block at $2200
title_delay_arr	dc.w $190, $190, $1e, $96, $28, $5dc, $30c
	; Resuming memory block at $2200
title_anim_ctr	dc.w	$190
	; Resuming memory block at $2200
title_spr_arr	dc.b $0, $1, $2, $3, $4, $5, $6, $7
	dc.b $8, $9, $b, $c, $d, $e, $f, $10
	dc.b $11, $12, $13, $14, $15, $16, $17, $18
	dc.b $b
	; Resuming memory block at $2200
title_spr_xpos_arr	dc.w $40, $70, $a0, $d0, $100, $40, $70, $a0
	dc.w $d0, $100, $70, $7a, $aa, $da, $40, $70
	dc.w $a0, $d0, $100, $40, $70, $a0, $d0, $100
	dc.w $b0
	; Resuming memory block at $2200
title_spr_ypos_arr	dc.b $58, $58, $58, $58, $58, $82, $82, $82
	dc.b $82, $82, $ac, $74, $74, $74, $58, $58
	dc.b $58, $58, $58, $82, $82, $82, $82, $82
	dc.b $aa
	; Resuming memory block at $2200
raster_subtxt_brk_pos	dc.b	$c5
	; Resuming memory block at $2200
subtxt_display_idx	dc.b	$00
	; Resuming memory block at $2200
subtxt_display_max	dc.b	$0a
	; Resuming memory block at $2200
subtxt_display_arr	dc.b $0, $1, $0, $2, $0, $3, $0, $4
	dc.b $0, $5, $0
	; Resuming memory block at $2200
subtxt_delay_arr	dc.w $c8, $c8, $2ee, $10e, $5f, $10e, $5f, $10e
	dc.w $5f, $109, $316
	; Resuming memory block at $2200
subtxt_anim_ctr	dc.w	$c8
	; Resuming memory block at $2200
subtxt_xpos_arr	dc.b $5e, $8e, $be, $ee
	; Resuming memory block at $2200
subtxt_ypos_arr	dc.b $cc, $cc, $cc, $cc
	; Resuming memory block at $2200
subtxt_1_spr_arr	dc.b $19, $1a, $1b, $1c
	; Resuming memory block at $2200
subtxt_2_spr_arr	dc.b $1d, $1e, $1f, $20
	; Resuming memory block at $2200
subtxt_3_spr_arr	dc.b $21, $22, $23, $24
	; Resuming memory block at $2200
subtxt_4_spr_arr	dc.b $25, $26, $27, $28
	; Resuming memory block at $2200
subtxt_5_spr_arr	dc.b $29, $2a, $2b, $2c
	; Resuming memory block at $2200
subtxt_6_spr_arr	dc.b $2d, $2e, $2f, $30
	; Resuming memory block at $2200
	; Resuming memory block at $2200
	; LineNumber: 131
	
; //	Sid music is played by calling the sidfile_1_play address for each raster interupt trigger
	; NodeProcedureDecl 0
	; ***********  Defining procedure : exitDemo
	;    Procedure type : User-defined procedure
	rts
end_procedure_exitDemo
	; NodeProcedureDecl 0
	; ***********  Defining procedure : irSpriteReset
	;    Procedure type : User-defined procedure
	rti
end_procedure_irSpriteReset
	; NodeProcedureDecl -1
	; Ending memory block at $2200
	; Resuming memory block at $2200
	; ***********  Defining procedure : irRasterMainLoop
	;    Procedure type : User-defined procedure
	; LineNumber: 152
irRasterMainLoop
	; LineNumber: 153
	; StartIRQ
	asl $d019
	; LineNumber: 156
	
; //screen_bg_col := green;
	jsr $1003
	; LineNumber: 159
	; Binary clause INTEGER: EQUALS
	lda title_anim_ctr+1   ; compare high bytes
	cmp #$00 ;keep
	bne irRasterMainLoop_localfailed128
	lda title_anim_ctr
	cmp #$00 ;keep
	bne irRasterMainLoop_localfailed128
	jmp irRasterMainLoop_ctb12
irRasterMainLoop_localfailed128
	jmp irRasterMainLoop_eblock13
irRasterMainLoop_ctb12: ;Main true block ;keep 
	; LineNumber: 160
	; LineNumber: 162
	
; // Check if title sprite mode should change
; // Increment to next mode
	; Test Inc dec D
	inc title_display_idx
	; LineNumber: 163
	; Binary clause Simplified: GREATER
	lda title_display_idx
	; Compare with pure num / var optimization
	cmp title_display_max;keep
	bcc irRasterMainLoop_edblock133
	beq irRasterMainLoop_edblock133
irRasterMainLoop_ctb131: ;Main true block ;keep 
	; LineNumber: 162
	lda #$0
	; Calling storevariable on generic assign expression
	sta title_display_idx
irRasterMainLoop_edblock133
	; LineNumber: 166
	
; // Set new delay value
	; Load Integer array
	; CAST type INTEGER
	lda title_display_idx
	asl
	tax
	lda title_delay_arr,x 
	ldy title_delay_arr+1,x 
	; Calling storevariable on generic assign expression
	sta title_anim_ctr
	sty title_anim_ctr+1
	; LineNumber: 169
	; Binary clause Simplified: EQUALS
	clc
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; cmp #$00 ignored
	bne irRasterMainLoop_eblock138
irRasterMainLoop_ctb137: ;Main true block ;keep 
	; LineNumber: 169
	; LineNumber: 172
	
; // Configure sprites for each mode
; //  0 - no sprites displayed
; //screen_bg_col := black;
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d015
	; LineNumber: 173
	jmp irRasterMainLoop_edblock139
irRasterMainLoop_eblock138
	; LineNumber: 172
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne irRasterMainLoop_eblock194
irRasterMainLoop_ctb193: ;Main true block ;keep 
	; LineNumber: 173
	; LineNumber: 176
	
; //  1 - display monkey island title
; //screen_bg_col := white;
	lda #$a
	; Calling storevariable on generic assign expression
	sta title_text_color
	; LineNumber: 177
	jsr initSprites
	; LineNumber: 178
	jsr initSprites_mode1
	; LineNumber: 179
	jmp irRasterMainLoop_edblock195
irRasterMainLoop_eblock194
	; LineNumber: 178
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne irRasterMainLoop_eblock222
irRasterMainLoop_ctb221: ;Main true block ;keep 
	; LineNumber: 179
	; LineNumber: 183
	jmp irRasterMainLoop_edblock223
irRasterMainLoop_eblock222
	; LineNumber: 182
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne irRasterMainLoop_edblock237
irRasterMainLoop_ctb235: ;Main true block ;keep 
	; LineNumber: 183
	; LineNumber: 186
	
; //  2 - display 'Psych!'
; //screen_bg_col := yellow;
; // These sprites are now multiplexed and handled in interrupt
; //  3 - display VCF-MW title		
; //screen_bg_col := orange;
	lda #$4
	; Calling storevariable on generic assign expression
	sta title_text_color
	; LineNumber: 187
	jsr initSprites
	; LineNumber: 188
	jsr initSprites_mode3
	; LineNumber: 189
irRasterMainLoop_edblock237
irRasterMainLoop_edblock223
irRasterMainLoop_edblock195
irRasterMainLoop_edblock139
	; LineNumber: 190
	jmp irRasterMainLoop_edblock14
irRasterMainLoop_eblock13
	; LineNumber: 190
	; LineNumber: 191
	lda title_anim_ctr
	sec
	sbc #$01
	sta title_anim_ctr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs irRasterMainLoop_WordAdd241
	dec title_anim_ctr+1
irRasterMainLoop_WordAdd241
	; LineNumber: 192
irRasterMainLoop_edblock14
	; LineNumber: 195
	; Binary clause INTEGER: EQUALS
	lda subtxt_anim_ctr+1   ; compare high bytes
	cmp #$00 ;keep
	bne irRasterMainLoop_eblock244
	lda subtxt_anim_ctr
	cmp #$00 ;keep
	bne irRasterMainLoop_eblock244
	jmp irRasterMainLoop_ctb243
irRasterMainLoop_ctb243: ;Main true block ;keep 
	; LineNumber: 196
	; LineNumber: 198
	
; // Check if bottom text sprite sprites should change
; // Increment to next mode
	; Test Inc dec D
	inc subtxt_display_idx
	; LineNumber: 199
	; Binary clause Simplified: GREATER
	lda subtxt_display_idx
	; Compare with pure num / var optimization
	cmp subtxt_display_max;keep
	bcc irRasterMainLoop_edblock259
	beq irRasterMainLoop_edblock259
irRasterMainLoop_ctb257: ;Main true block ;keep 
	; LineNumber: 198
	lda #$0
	; Calling storevariable on generic assign expression
	sta subtxt_display_idx
irRasterMainLoop_edblock259
	; LineNumber: 202
	
; // Set new delay value
	; Load Integer array
	; CAST type INTEGER
	lda subtxt_display_idx
	asl
	tax
	lda subtxt_delay_arr,x 
	ldy subtxt_delay_arr+1,x 
	; Calling storevariable on generic assign expression
	sta subtxt_anim_ctr
	sty subtxt_anim_ctr+1
	; LineNumber: 203
	jmp irRasterMainLoop_edblock245
irRasterMainLoop_eblock244
	; LineNumber: 203
	; LineNumber: 204
	lda subtxt_anim_ctr
	sec
	sbc #$01
	sta subtxt_anim_ctr+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs irRasterMainLoop_WordAdd263
	dec subtxt_anim_ctr+1
irRasterMainLoop_WordAdd263
	; LineNumber: 205
irRasterMainLoop_edblock245
	; LineNumber: 208
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne irRasterMainLoop_localfailed318
	jmp irRasterMainLoop_ctb265
irRasterMainLoop_localfailed318
	jmp irRasterMainLoop_eblock266
irRasterMainLoop_ctb265: ;Main true block ;keep 
	; LineNumber: 209
	; LineNumber: 213
	
; // Take action for current display mode
; //  1 - display monkey island title
; // Display "Band A" sprites 0, 1, 2, 3 which are shared
	jsr setTitleRow1
	; LineNumber: 222
	
; // Setup IRQ to display "Band B" sprites 0, 1, 2 which are shared
; // This is done several scanlines before raster reaches position 126, so 
; // there's enough time to process in advance. irSetTitleRow2() only sets 
; // new position of sprites 0, 1, 2, as this can be done in advance.
; //
; // Changing new sprite pointers for sprites 0, 1, 2 is actually done on 
; // scanline 126 within the main loop.
	; RasterIRQ : Hook a procedure
	lda raster_title_brk_pos
	sta $d012
	lda #<irSetTitleRow2
	sta $0314
	lda #>irSetTitleRow2
	sta $0315
	; LineNumber: 223
	jmp irRasterMainLoop_edblock267
irRasterMainLoop_eblock266
	; LineNumber: 222
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne irRasterMainLoop_eblock323
irRasterMainLoop_ctb322: ;Main true block ;keep 
	; LineNumber: 224
	; LineNumber: 226
	
; //	2 - display 'Psych!'
	lda #$5
	; Calling storevariable on generic assign expression
	sta title_text_color
	; LineNumber: 227
	jsr initSprites
	; LineNumber: 228
	lda #$7
	; Calling storevariable on generic assign expression
	sta $d015
	; LineNumber: 230
	; RasterIRQ : Hook a procedure
	lda raster_troll_brk_pos
	sta $d012
	lda #<irSetTrollRow
	sta $0314
	lda #>irSetTrollRow
	sta $0315
	; LineNumber: 231
	jmp irRasterMainLoop_edblock324
irRasterMainLoop_eblock323
	; LineNumber: 230
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne irRasterMainLoop_eblock351
irRasterMainLoop_ctb350: ;Main true block ;keep 
	; LineNumber: 232
	; LineNumber: 236
	
; //  3 - display VCF title
; // Display "Band A" sprites 0, 1, 2 which are shared
	jsr setVcfTitleRow1
	; LineNumber: 239
	
; // Setup IRQ to display "Band B" sprites 0, 1, 2 in new position
	; RasterIRQ : Hook a procedure
	lda raster_vcf_title_brk_pos
	sta $d012
	lda #<irSetVcfTitleRow2
	sta $0314
	lda #>irSetVcfTitleRow2
	sta $0315
	; LineNumber: 240
	jmp irRasterMainLoop_edblock352
irRasterMainLoop_eblock351
	; LineNumber: 239
	; Binary clause Simplified: GREATEREQUAL
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc irRasterMainLoop_edblock366
irRasterMainLoop_ctb364: ;Main true block ;keep 
	; LineNumber: 240
	; LineNumber: 241
	
; // Display text at bottom
	; RasterIRQ : Hook a procedure
	lda raster_subtxt_brk_pos
	sta $d012
	lda #<irSetSubtxt
	sta $0314
	lda #>irSetSubtxt
	sta $0315
	; LineNumber: 243
irRasterMainLoop_edblock366
irRasterMainLoop_edblock352
irRasterMainLoop_edblock324
irRasterMainLoop_edblock267
	; LineNumber: 245
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 246
	rti
end_procedure_irRasterMainLoop
	
; // Set title sprites - Rows 2&3 - Y pos=126
	; NodeProcedureDecl 0
	; ***********  Defining procedure : irSetTitleRow2
	;    Procedure type : User-defined procedure
	; LineNumber: 250
irSetTitleRow2
	; LineNumber: 251
	; StartIRQ
	asl $d019
	; LineNumber: 253
	
; //inc(screen_bg_col);
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #10 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #0
	sta $D000,x
	cpy #0
	beq irSetTitleRow2_spritepos370
	lda $D010
	ora #%1
	sta $D010
	jmp irSetTitleRow2_spriteposcontinue371
irSetTitleRow2_spritepos370
	lda $D010
	and #%11111110
	sta $D010
irSetTitleRow2_spriteposcontinue371
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$5 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 255
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #12 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #2
	sta $D000,x
	cpy #0
	beq irSetTitleRow2_spritepos372
	lda $D010
	ora #%10
	sta $D010
	jmp irSetTitleRow2_spriteposcontinue373
irSetTitleRow2_spritepos372
	lda $D010
	and #%11111101
	sta $D010
irSetTitleRow2_spriteposcontinue373
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$6 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 256
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #14 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #4
	sta $D000,x
	cpy #0
	beq irSetTitleRow2_spritepos374
	lda $D010
	ora #%100
	sta $D010
	jmp irSetTitleRow2_spriteposcontinue375
irSetTitleRow2_spritepos374
	lda $D010
	and #%11111011
	sta $D010
irSetTitleRow2_spriteposcontinue375
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$7 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 257
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #6 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	sta $D000,x
	cpy #0
	beq irSetTitleRow2_spritepos376
	lda $D010
	ora #%1000
	sta $D010
	jmp irSetTitleRow2_spriteposcontinue377
irSetTitleRow2_spritepos376
	lda $D010
	and #%11110111
	sta $D010
irSetTitleRow2_spriteposcontinue377
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$3 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 261
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc irSetTitleRow2_eblock380
irSetTitleRow2_ctb379: ;Main true block ;keep 
	; LineNumber: 261
	; LineNumber: 261
	
; //screen_bg_col := green; 
; // for testing
; // Check if bottom text should be displayed, or return IRQ to SID player
	; RasterIRQ : Hook a procedure
	lda raster_subtxt_brk_pos
	sta $d012
	lda #<irSetSubtxt
	sta $0314
	lda #>irSetSubtxt
	sta $0315
	; LineNumber: 263
	jmp irSetTitleRow2_edblock381
irSetTitleRow2_eblock380
	; LineNumber: 263
	; LineNumber: 264
	; RasterIRQ : Hook a procedure
	lda raster_sid_brk_pos
	sta $d012
	lda #<irRasterMainLoop
	sta $0314
	lda #>irRasterMainLoop
	sta $0315
	; LineNumber: 265
irSetTitleRow2_edblock381
	; LineNumber: 267
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 268
	rti
end_procedure_irSetTitleRow2
	
; // Set title sprites - Rows 2&3 - Y pos=126
	; NodeProcedureDecl 0
	; ***********  Defining procedure : irSetVcfTitleRow2
	;    Procedure type : User-defined procedure
	; LineNumber: 272
irSetVcfTitleRow2
	; LineNumber: 273
	; StartIRQ
	asl $d019
	; LineNumber: 275
	
; //inc(screen_bg_col);
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #38 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #0
	sta $D000,x
	cpy #0
	beq irSetVcfTitleRow2_spritepos387
	lda $D010
	ora #%1
	sta $D010
	jmp irSetVcfTitleRow2_spriteposcontinue388
irSetVcfTitleRow2_spritepos387
	lda $D010
	and #%11111110
	sta $D010
irSetVcfTitleRow2_spriteposcontinue388
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$13 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 277
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #40 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #2
	sta $D000,x
	cpy #0
	beq irSetVcfTitleRow2_spritepos389
	lda $D010
	ora #%10
	sta $D010
	jmp irSetVcfTitleRow2_spriteposcontinue390
irSetVcfTitleRow2_spritepos389
	lda $D010
	and #%11111101
	sta $D010
irSetVcfTitleRow2_spriteposcontinue390
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$14 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 278
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #42 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #4
	sta $D000,x
	cpy #0
	beq irSetVcfTitleRow2_spritepos391
	lda $D010
	ora #%100
	sta $D010
	jmp irSetVcfTitleRow2_spriteposcontinue392
irSetVcfTitleRow2_spritepos391
	lda $D010
	and #%11111011
	sta $D010
irSetVcfTitleRow2_spriteposcontinue392
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$15 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 279
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #34 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #6
	sta $D000,x
	cpy #0
	beq irSetVcfTitleRow2_spritepos393
	lda $D010
	ora #%1000
	sta $D010
	jmp irSetVcfTitleRow2_spriteposcontinue394
irSetVcfTitleRow2_spritepos393
	lda $D010
	and #%11110111
	sta $D010
irSetVcfTitleRow2_spriteposcontinue394
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$11 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 283
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc irSetVcfTitleRow2_eblock397
irSetVcfTitleRow2_ctb396: ;Main true block ;keep 
	; LineNumber: 283
	; LineNumber: 283
	
; //screen_bg_col := green; 
; // for testing
; // Check if bottom text should be displayed, or return IRQ to SID player
	; RasterIRQ : Hook a procedure
	lda raster_subtxt_brk_pos
	sta $d012
	lda #<irSetSubtxt
	sta $0314
	lda #>irSetSubtxt
	sta $0315
	; LineNumber: 285
	jmp irSetVcfTitleRow2_edblock398
irSetVcfTitleRow2_eblock397
	; LineNumber: 285
	; LineNumber: 286
	; RasterIRQ : Hook a procedure
	lda raster_sid_brk_pos
	sta $d012
	lda #<irRasterMainLoop
	sta $0314
	lda #>irRasterMainLoop
	sta $0315
	; LineNumber: 287
irSetVcfTitleRow2_edblock398
	; LineNumber: 289
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 290
	rti
end_procedure_irSetVcfTitleRow2
	
; // Set 'Troll' title row
	; NodeProcedureDecl 0
	; ***********  Defining procedure : irSetTrollRow
	;    Procedure type : User-defined procedure
	; LineNumber: 294
irSetTrollRow
	; LineNumber: 295
	; StartIRQ
	asl $d019
	; LineNumber: 298
	
; //inc(screen_bg_col);
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$b ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 299
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$c ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 300
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$d ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 301
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #22 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #0
	sta $D000,x
	cpy #0
	beq irSetTrollRow_spritepos404
	lda $D010
	ora #%1
	sta $D010
	jmp irSetTrollRow_spriteposcontinue405
irSetTrollRow_spritepos404
	lda $D010
	and #%11111110
	sta $D010
irSetTrollRow_spriteposcontinue405
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$b ; array with const index optimization 
	sta $D000,y
	; LineNumber: 302
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #24 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #2
	sta $D000,x
	cpy #0
	beq irSetTrollRow_spritepos406
	lda $D010
	ora #%10
	sta $D010
	jmp irSetTrollRow_spriteposcontinue407
irSetTrollRow_spritepos406
	lda $D010
	and #%11111101
	sta $D010
irSetTrollRow_spriteposcontinue407
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$c ; array with const index optimization 
	sta $D000,y
	; LineNumber: 303
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #26 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #4
	sta $D000,x
	cpy #0
	beq irSetTrollRow_spritepos408
	lda $D010
	ora #%100
	sta $D010
	jmp irSetTrollRow_spriteposcontinue409
irSetTrollRow_spritepos408
	lda $D010
	and #%11111011
	sta $D010
irSetTrollRow_spriteposcontinue409
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$d ; array with const index optimization 
	sta $D000,y
	; LineNumber: 306
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc irSetTrollRow_eblock412
irSetTrollRow_ctb411: ;Main true block ;keep 
	; LineNumber: 306
	; LineNumber: 306
	
; // Check if bottom text should be displayed, or return IRQ to SID player
	; RasterIRQ : Hook a procedure
	lda raster_subtxt_brk_pos
	sta $d012
	lda #<irSetSubtxt
	sta $0314
	lda #>irSetSubtxt
	sta $0315
	; LineNumber: 308
	jmp irSetTrollRow_edblock413
irSetTrollRow_eblock412
	; LineNumber: 308
	; LineNumber: 309
	; RasterIRQ : Hook a procedure
	lda raster_sid_brk_pos
	sta $d012
	lda #<irRasterMainLoop
	sta $0314
	lda #>irRasterMainLoop
	sta $0315
	; LineNumber: 310
irSetTrollRow_edblock413
	; LineNumber: 311
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 313
	rti
end_procedure_irSetTrollRow
	
; // Set bottom text: The Collective Presents
	; NodeProcedureDecl 0
	; ***********  Defining procedure : irSetSubtxt
	;    Procedure type : User-defined procedure
	; LineNumber: 317
irSetSubtxt
	; LineNumber: 318
	; StartIRQ
	asl $d019
	; LineNumber: 322
	
; //inc(screen_bg_col);
; // Ensure sprites 0-3 are enabled
	; Assigning memory location
	; 8 bit binop
	; Add/sub where right value is constant number
	lda $d015
	ora #$f
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta $d015
	; LineNumber: 326
	
; //SPRITE_STRETCH_X			:= SPRITE_STRETCH_X | %11111111;
; //SPRITE_STRETCH_Y			:= SPRITE_STRETCH_Y | %11111111;
	; Assigning memory location
	; 8 bit binop
	; Add/sub where right value is constant number
	lda $d01c
	ora #$f
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta $d01c
	; LineNumber: 327
	; Assigning memory location
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d025
	; LineNumber: 330
	
; //SPRITE_MULTICOLOR_REG2	:= green;
	lda subtext_color
	; Calling storevariable on generic assign expression
	sta $D027+$0
	; LineNumber: 331
	; Calling storevariable on generic assign expression
	sta $D027+$1
	; LineNumber: 332
	; Calling storevariable on generic assign expression
	sta $D027+$2
	; LineNumber: 333
	; Calling storevariable on generic assign expression
	sta $D027+$3
	; LineNumber: 335
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne irSetSubtxt_localfailed644
	jmp irSetSubtxt_ctb420
irSetSubtxt_localfailed644
	jmp irSetSubtxt_eblock421
irSetSubtxt_ctb420: ;Main true block ;keep 
	; LineNumber: 335
	; LineNumber: 336
	; Load Byte array
	; CAST type NADA
	lda subtxt_1_spr_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 337
	; Load Byte array
	; CAST type NADA
	lda subtxt_1_spr_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 338
	; Load Byte array
	; CAST type NADA
	lda subtxt_1_spr_arr +$2 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 339
	; Load Byte array
	; CAST type NADA
	lda subtxt_1_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 344
	jmp irSetSubtxt_edblock422
irSetSubtxt_eblock421
	; LineNumber: 343
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne irSetSubtxt_localfailed758
	jmp irSetSubtxt_ctb648
irSetSubtxt_localfailed758
	jmp irSetSubtxt_eblock649
irSetSubtxt_ctb648: ;Main true block ;keep 
	; LineNumber: 344
	; LineNumber: 345
	
; //SpritePos(subtxt_1_xpos_arr[0], subtxt_ypos_1_arr[0], 0);
; //SpritePos(subtxt_1_xpos_arr[1], subtxt_ypos_1_arr[1], 1);
; //SpritePos(subtxt_1_xpos_arr[2], subtxt_ypos_1_arr[2], 2);
; //SpritePos(subtxt_1_xpos_arr[3], subtxt_ypos_1_arr[3], 3);
	; Load Byte array
	; CAST type NADA
	lda subtxt_2_spr_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 346
	; Load Byte array
	; CAST type NADA
	lda subtxt_2_spr_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 347
	; Load Byte array
	; CAST type NADA
	lda subtxt_2_spr_arr +$2 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 348
	; Load Byte array
	; CAST type NADA
	lda subtxt_2_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 353
	jmp irSetSubtxt_edblock650
irSetSubtxt_eblock649
	; LineNumber: 352
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne irSetSubtxt_localfailed815
	jmp irSetSubtxt_ctb762
irSetSubtxt_localfailed815
	jmp irSetSubtxt_eblock763
irSetSubtxt_ctb762: ;Main true block ;keep 
	; LineNumber: 353
	; LineNumber: 354
	
; //SpritePos(subtxt_2_xpos_arr[0], subtxt_2_ypos_arr[0], 0);
; //SpritePos(subtxt_2_xpos_arr[1], subtxt_2_ypos_arr[1], 1);
; //SpritePos(subtxt_2_xpos_arr[2], subtxt_2_ypos_arr[2], 2);
; //SpritePos(subtxt_2_xpos_arr[3], subtxt_2_ypos_arr[3], 3);
	; Load Byte array
	; CAST type NADA
	lda subtxt_3_spr_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 355
	; Load Byte array
	; CAST type NADA
	lda subtxt_3_spr_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 356
	; Load Byte array
	; CAST type NADA
	lda subtxt_3_spr_arr +$2 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 357
	; Load Byte array
	; CAST type NADA
	lda subtxt_3_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 362
	jmp irSetSubtxt_edblock764
irSetSubtxt_eblock763
	; LineNumber: 361
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne irSetSubtxt_eblock820
irSetSubtxt_ctb819: ;Main true block ;keep 
	; LineNumber: 362
	; LineNumber: 363
	
; //SpritePos(subtxt_3_xpos_arr[0], subtxt_3_ypos_arr[0], 0);
; //SpritePos(subtxt_3_xpos_arr[1], subtxt_3_ypos_arr[1], 1);
; //SpritePos(subtxt_3_xpos_arr[2], subtxt_3_ypos_arr[2], 2);
; //SpritePos(subtxt_3_xpos_arr[3], subtxt_3_ypos_arr[3], 3);
	; Load Byte array
	; CAST type NADA
	lda subtxt_4_spr_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 364
	; Load Byte array
	; CAST type NADA
	lda subtxt_4_spr_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 365
	; Load Byte array
	; CAST type NADA
	lda subtxt_4_spr_arr +$2 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 366
	; Load Byte array
	; CAST type NADA
	lda subtxt_4_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 371
	jmp irSetSubtxt_edblock821
irSetSubtxt_eblock820
	; LineNumber: 370
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$5;keep
	bne irSetSubtxt_eblock848
irSetSubtxt_ctb847: ;Main true block ;keep 
	; LineNumber: 371
	; LineNumber: 372
	
; //SpritePos(subtxt_4_xpos_arr[0], subtxt_4_ypos_arr[0], 0);
; //SpritePos(subtxt_4_xpos_arr[1], subtxt_4_ypos_arr[1], 1);
; //SpritePos(subtxt_4_xpos_arr[2], subtxt_4_ypos_arr[2], 2);
; //SpritePos(subtxt_4_xpos_arr[3], subtxt_4_ypos_arr[3], 3);
	; Load Byte array
	; CAST type NADA
	lda subtxt_5_spr_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 373
	; Load Byte array
	; CAST type NADA
	lda subtxt_5_spr_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 374
	; Load Byte array
	; CAST type NADA
	lda subtxt_5_spr_arr +$2 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 375
	; Load Byte array
	; CAST type NADA
	lda subtxt_5_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 380
	jmp irSetSubtxt_edblock849
irSetSubtxt_eblock848
	; LineNumber: 379
	
; //SpritePos(subtxt_6_xpos_arr[0], subtxt_6_ypos_arr[0], 0);
; //SpritePos(subtxt_6_xpos_arr[1], subtxt_6_ypos_arr[1], 1);
; //SpritePos(subtxt_6_xpos_arr[2], subtxt_6_ypos_arr[2], 2);
; //SpritePos(subtxt_6_xpos_arr[3], subtxt_6_ypos_arr[3], 3);
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx subtxt_display_idx
	lda subtxt_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$6;keep
	bne irSetSubtxt_edblock863
irSetSubtxt_ctb861: ;Main true block ;keep 
	; LineNumber: 380
	; LineNumber: 381
	
; //SpritePos(subtxt_5_xpos_arr[0], subtxt_5_ypos_arr[0], 0);
; //SpritePos(subtxt_5_xpos_arr[1], subtxt_5_ypos_arr[1], 1);
; //SpritePos(subtxt_5_xpos_arr[2], subtxt_5_ypos_arr[2], 2);
; //SpritePos(subtxt_5_xpos_arr[3], subtxt_5_ypos_arr[3], 3);
	; Load Byte array
	; CAST type NADA
	lda subtxt_6_spr_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 382
	; Load Byte array
	; CAST type NADA
	lda subtxt_6_spr_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 383
	; Load Byte array
	; CAST type NADA
	lda subtxt_6_spr_arr +$2 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 384
	; Load Byte array
	; CAST type NADA
	lda subtxt_6_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 389
irSetSubtxt_edblock863
irSetSubtxt_edblock849
irSetSubtxt_edblock821
irSetSubtxt_edblock764
irSetSubtxt_edblock650
irSetSubtxt_edblock422
	; LineNumber: 391
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Byte array
	; CAST type NADA
	lda  subtxt_xpos_arr +$0 ; array with const index optimization 
	ldx #0
	sta $D000,x
irSetSubtxt_spritepos866
	lda $D010
	and #%11111110
	sta $D010
irSetSubtxt_spriteposcontinue867
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  subtxt_ypos_arr +$0 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 392
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Byte array
	; CAST type NADA
	lda  subtxt_xpos_arr +$1 ; array with const index optimization 
	ldx #2
	sta $D000,x
irSetSubtxt_spritepos868
	lda $D010
	and #%11111101
	sta $D010
irSetSubtxt_spriteposcontinue869
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  subtxt_ypos_arr +$1 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 393
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Byte array
	; CAST type NADA
	lda  subtxt_xpos_arr +$2 ; array with const index optimization 
	ldx #4
	sta $D000,x
irSetSubtxt_spritepos870
	lda $D010
	and #%11111011
	sta $D010
irSetSubtxt_spriteposcontinue871
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  subtxt_ypos_arr +$2 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 394
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Byte array
	; CAST type NADA
	lda  subtxt_xpos_arr +$3 ; array with const index optimization 
	ldx #6
	sta $D000,x
irSetSubtxt_spritepos872
	lda $D010
	and #%11110111
	sta $D010
irSetSubtxt_spriteposcontinue873
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  subtxt_ypos_arr +$3 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 396
	; RasterIRQ : Hook a procedure
	lda raster_sid_brk_pos
	sta $d012
	lda #<irRasterMainLoop
	sta $0314
	lda #>irRasterMainLoop
	sta $0315
	; LineNumber: 398
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	; LineNumber: 399
	rti
end_procedure_irSetSubtxt
	
; // Set title sprites - Rows 1 - Y pos=119
; // This function should be called on every frame when title is active
	; NodeProcedureDecl 0
	; ***********  Defining procedure : setTitleRow1
	;    Procedure type : User-defined procedure
	; LineNumber: 404
setTitleRow1
	; LineNumber: 406
	
; // Set title row 1 sprites that are shared 
	lda #$a
	; Calling storevariable on generic assign expression
	sta title_text_color
	; LineNumber: 407
	jsr initSprites
	; LineNumber: 409
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$0 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 410
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$1 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 411
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$2 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 412
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 412
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #0 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	sta $D000,x
	cpy #0
	beq setTitleRow1_spritepos875
	lda $D010
	ora #%1
	sta $D010
	jmp setTitleRow1_spriteposcontinue876
setTitleRow1_spritepos875
	lda $D010
	and #%11111110
	sta $D010
setTitleRow1_spriteposcontinue876
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$0 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 414
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #2 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	sta $D000,x
	cpy #0
	beq setTitleRow1_spritepos877
	lda $D010
	ora #%10
	sta $D010
	jmp setTitleRow1_spriteposcontinue878
setTitleRow1_spritepos877
	lda $D010
	and #%11111101
	sta $D010
setTitleRow1_spriteposcontinue878
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$1 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 415
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #4 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	sta $D000,x
	cpy #0
	beq setTitleRow1_spritepos879
	lda $D010
	ora #%100
	sta $D010
	jmp setTitleRow1_spriteposcontinue880
setTitleRow1_spritepos879
	lda $D010
	and #%11111011
	sta $D010
setTitleRow1_spriteposcontinue880
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$2 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 416
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #6 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	sta $D000,x
	cpy #0
	beq setTitleRow1_spritepos881
	lda $D010
	ora #%1000
	sta $D010
	jmp setTitleRow1_spriteposcontinue882
setTitleRow1_spritepos881
	lda $D010
	and #%11110111
	sta $D010
setTitleRow1_spriteposcontinue882
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$3 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 418
	rts
end_procedure_setTitleRow1
	
; //screen_bg_col := yellow; 
; // for testing
; // Set VCF title sprites - Rows 1 - Y pos=119
; // This function should be called on every frame when title is active
	; NodeProcedureDecl 0
	; ***********  Defining procedure : setVcfTitleRow1
	;    Procedure type : User-defined procedure
	; LineNumber: 423
setVcfTitleRow1
	; LineNumber: 425
	
; // Set VCF title row 1 sprites that are shared 
	lda #$4
	; Calling storevariable on generic assign expression
	sta title_text_color
	; LineNumber: 426
	jsr initSprites
	; LineNumber: 428
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$e ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 429
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$f ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 430
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$10 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 431
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$11 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 431
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #28 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #0
	sta $D000,x
	cpy #0
	beq setVcfTitleRow1_spritepos884
	lda $D010
	ora #%1
	sta $D010
	jmp setVcfTitleRow1_spriteposcontinue885
setVcfTitleRow1_spritepos884
	lda $D010
	and #%11111110
	sta $D010
setVcfTitleRow1_spriteposcontinue885
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$e ; array with const index optimization 
	sta $D000,y
	; LineNumber: 433
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #30 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #2
	sta $D000,x
	cpy #0
	beq setVcfTitleRow1_spritepos886
	lda $D010
	ora #%10
	sta $D010
	jmp setVcfTitleRow1_spriteposcontinue887
setVcfTitleRow1_spritepos886
	lda $D010
	and #%11111101
	sta $D010
setVcfTitleRow1_spriteposcontinue887
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$f ; array with const index optimization 
	sta $D000,y
	; LineNumber: 434
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #32 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #4
	sta $D000,x
	cpy #0
	beq setVcfTitleRow1_spritepos888
	lda $D010
	ora #%100
	sta $D010
	jmp setVcfTitleRow1_spriteposcontinue889
setVcfTitleRow1_spritepos888
	lda $D010
	and #%11111011
	sta $D010
setVcfTitleRow1_spriteposcontinue889
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$10 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 435
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #34 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #6
	sta $D000,x
	cpy #0
	beq setVcfTitleRow1_spritepos890
	lda $D010
	ora #%1000
	sta $D010
	jmp setVcfTitleRow1_spriteposcontinue891
setVcfTitleRow1_spritepos890
	lda $D010
	and #%11110111
	sta $D010
setVcfTitleRow1_spriteposcontinue891
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$11 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 437
	rts
end_procedure_setVcfTitleRow1
	
; //screen_bg_col := yellow; 
; // for testing
; // Set title sprites - Rows 2&3 - Y pos=126
	; NodeProcedureDecl 0
	; ***********  Defining procedure : setTitleRow2
	;    Procedure type : User-defined procedure
	; LineNumber: 441
setTitleRow2
	; LineNumber: 442
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$5 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 443
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$6 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 444
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$7 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 445
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$3 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 446
	rts
end_procedure_setTitleRow2
	
; // Set title sprites - Rows 2&3 - Y pos=126
	; NodeProcedureDecl 0
	; ***********  Defining procedure : setVcfTitleRow2
	;    Procedure type : User-defined procedure
	; LineNumber: 450
setVcfTitleRow2
	; LineNumber: 451
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$13 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$0
	; LineNumber: 452
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$14 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$1
	; LineNumber: 453
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$15 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$2
	; LineNumber: 454
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$11 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$3
	; LineNumber: 455
	rts
end_procedure_setVcfTitleRow2
	
; // Set sprite colors
	; NodeProcedureDecl 0
	; ***********  Defining procedure : set_sprite_colors
	;    Procedure type : User-defined procedure
	; LineNumber: 459
set_sprite_colors
	; LineNumber: 460
	lda title_text_color
	; Calling storevariable on generic assign expression
	sta $D027+$0
	; LineNumber: 461
	; Calling storevariable on generic assign expression
	sta $D027+$1
	; LineNumber: 462
	; Calling storevariable on generic assign expression
	sta $D027+$2
	; LineNumber: 463
	; Calling storevariable on generic assign expression
	sta $D027+$3
	; LineNumber: 464
	; Calling storevariable on generic assign expression
	sta $D027+$4
	; LineNumber: 465
	; Calling storevariable on generic assign expression
	sta $D027+$5
	; LineNumber: 466
	; Calling storevariable on generic assign expression
	sta $D027+$6
	; LineNumber: 467
	; Calling storevariable on generic assign expression
	sta $D027+$7
	; LineNumber: 468
	rts
end_procedure_set_sprite_colors
	
; // Show Koala-format image
	; NodeProcedureDecl 0
	; ***********  Defining procedure : displayKoalaScreen
	;    Procedure type : User-defined procedure
	; LineNumber: 472
displayKoalaScreen
	; LineNumber: 472
	; Set bank
	lda #$2
	sta $dd00
	; LineNumber: 473
	
; // Bank #1, $4000-$7FFF
	; Bitmap mode 
	lda #$3b
	sta $d011
	; LineNumber: 474
	
; // Enable bitmap
	; Multicolor mode
	lda #16
	ora $d016
	sta $d016
	; LineNumber: 487
	
; // Enable MC
; // $D018 VIC-II Chip Memory Control Register
; //
; // Bit 0: Unused
; // Bits 1-3: Text character dot-data base address within VIC-II address space
; // Bits 4-7: Video matrix base address within VIC-II address space
; //
; // Set screen address to $5C00 CPU ($1C00 VIC)
; // Set bitmap loc to $6000 CPU ($2000 VIC) 
; // Sprite data loads at $4000 CPU ($0000 VIC) - Allowing room for up to 106 sprite definitions
; // Theoretically, there should be 384 bytes free at $1A80-$1BFF but TRSE throws an error.
	; Poke
	; Optimization: shift is zero
	lda #$7c
	sta $d018
	; LineNumber: 489
	
; // Copy bitmap data
	; Block memcpy
	ldy #0
displayKoalaScreen_Blockmemcpy_lbl1896
	lda $8002,y
	sta $6000,y
	lda $8102,y
	sta $6100,y
	lda $8202,y
	sta $6200,y
	lda $8302,y
	sta $6300,y
	lda $8402,y
	sta $6400,y
	lda $8502,y
	sta $6500,y
	lda $8602,y
	sta $6600,y
	lda $8702,y
	sta $6700,y
	lda $8802,y
	sta $6800,y
	lda $8902,y
	sta $6900,y
	lda $8a02,y
	sta $6a00,y
	lda $8b02,y
	sta $6b00,y
	lda $8c02,y
	sta $6c00,y
	lda $8d02,y
	sta $6d00,y
	lda $8e02,y
	sta $6e00,y
	lda $8f02,y
	sta $6f00,y
	dey
	bne displayKoalaScreen_Blockmemcpy_lbl1896
	; LineNumber: 490
	
; // Copy 4,096 bytes
	; Block memcpy
	ldy #0
displayKoalaScreen_Blockmemcpy_lbl1898
	lda $9002,y
	sta $7000,y
	lda $9102,y
	sta $7100,y
	lda $9202,y
	sta $7200,y
	lda $9302,y
	sta $7300,y
	lda $9402,y
	sta $7400,y
	lda $9502,y
	sta $7500,y
	lda $9602,y
	sta $7600,y
	lda $9702,y
	sta $7700,y
	lda $9802,y
	sta $7800,y
	lda $9902,y
	sta $7900,y
	lda $9a02,y
	sta $7a00,y
	lda $9b02,y
	sta $7b00,y
	lda $9c02,y
	sta $7c00,y
	lda $9d02,y
	sta $7d00,y
	lda $9e02,y
	sta $7e00,y
	lda $9f02,y
	sta $7f00,y
	dey
	bne displayKoalaScreen_Blockmemcpy_lbl1898
	; LineNumber: 494
	
; // Copy 4,096 bytes
; // Copy screen memory
	; Block memcpy
	ldy #0
displayKoalaScreen_Blockmemcpy_lbl1900
	lda $9f42,y
	sta $5c00,y
	lda $a042,y
	sta $5d00,y
	lda $a142,y
	sta $5e00,y
	lda $a242,y
	sta $5f00,y
	dey
	bne displayKoalaScreen_Blockmemcpy_lbl1900
	; LineNumber: 497
	
; // Copy color memory
	; Block memcpy
	ldy #0
displayKoalaScreen_Blockmemcpy_lbl1902
	lda $a32a,y
	sta $d800,y
	lda $a42a,y
	sta $d900,y
	lda $a52a,y
	sta $da00,y
	lda $a62a,y
	sta $db00,y
	dey
	bne displayKoalaScreen_Blockmemcpy_lbl1902
	; LineNumber: 500
	
; // Set BG/Border colors
	lda #$6
	; Calling storevariable on generic assign expression
	sta $D020+$0
	; LineNumber: 501
	; Peek
	lda $a712 + $0;keep
	; Calling storevariable on generic assign expression
	sta $D021+$0
	; LineNumber: 502
	rts
end_procedure_displayKoalaScreen
	
; // Sprites 0-2 are multiplexed and defined in separate functions
; // Sprites 3-7 are static
	; NodeProcedureDecl 0
	; ***********  Defining procedure : initSprites_mode1
	;    Procedure type : User-defined procedure
	; LineNumber: 507
initSprites_mode1
	; LineNumber: 515
	
; // Hardware sprites that are multiplexed: 0, 1, 2
; // No need to define these here, because they get initialized on every frame.
; // Hardware sprites that appear continually:
; // "Band A" HW sprite 4
; // "Band B" HW sprites 5 & 6
; // "Band C" HW sprite 7
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$4 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$4
	; LineNumber: 516
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$8 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$5
	; LineNumber: 517
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$9 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$6
	; LineNumber: 518
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$a ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$7
	; LineNumber: 519
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #8 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	sta $D000,x
	cpy #0
	beq initSprites_mode1_spritepos905
	lda $D010
	ora #%10000
	sta $D010
	jmp initSprites_mode1_spriteposcontinue906
initSprites_mode1_spritepos905
	lda $D010
	and #%11101111
	sta $D010
initSprites_mode1_spriteposcontinue906
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$4 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 520
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #16 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #10
	sta $D000,x
	cpy #0
	beq initSprites_mode1_spritepos907
	lda $D010
	ora #%100000
	sta $D010
	jmp initSprites_mode1_spriteposcontinue908
initSprites_mode1_spritepos907
	lda $D010
	and #%11011111
	sta $D010
initSprites_mode1_spriteposcontinue908
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$8 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 521
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #18 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #12
	sta $D000,x
	cpy #0
	beq initSprites_mode1_spritepos909
	lda $D010
	ora #%1000000
	sta $D010
	jmp initSprites_mode1_spriteposcontinue910
initSprites_mode1_spritepos909
	lda $D010
	and #%10111111
	sta $D010
initSprites_mode1_spriteposcontinue910
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$9 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 521
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #20 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #14
	sta $D000,x
	cpy #0
	beq initSprites_mode1_spritepos911
	lda $D010
	ora #%10000000
	sta $D010
	jmp initSprites_mode1_spriteposcontinue912
initSprites_mode1_spritepos911
	lda $D010
	and #%1111111
	sta $D010
initSprites_mode1_spriteposcontinue912
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$a ; array with const index optimization 
	sta $D000,y
	; LineNumber: 523
	rts
end_procedure_initSprites_mode1
	
; // Enable sprites 0-3
; //SPRITE_BITMASK			:= %00000111;
; //SPRITE_STRETCH_X			:= SPRITE_STRETCH_X | %11111111;
; //SPRITE_STRETCH_Y			:= SPRITE_STRETCH_Y | %11111111;
; //SPRITE_MULTICOLOR		:= %00000111;
; //LOC_SPRITE_PTRS[0]	:= title_spr_arr[11];
; //LOC_SPRITE_PTRS[1]	:= title_spr_arr[12];
; //LOC_SPRITE_PTRS[2]	:= title_spr_arr[13];
; //SpritePos(title_spr_xpos_arr[11], title_spr_ypos_arr[11], 0);
; //SpritePos(title_spr_xpos_arr[12], title_spr_ypos_arr[12], 1);
; //SpritePos(title_spr_xpos_arr[13], title_spr_ypos_arr[13], 2);
; // Shows VCF Title screen
	; NodeProcedureDecl 0
	; ***********  Defining procedure : initSprites_mode3
	;    Procedure type : User-defined procedure
	; LineNumber: 544
initSprites_mode3
	; LineNumber: 552
	
; // Hardware sprites that are multiplexed: 0, 1, 2, 3
; // No need to define these here, because they get initialized on every frame.
; // Hardware sprites that appear continually:
; // "Band A" HW sprite 4
; // "Band B" HW sprites 5 & 6
; // "Band C" HW sprite 7
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$12 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$4
	; LineNumber: 553
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$16 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$5
	; LineNumber: 554
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$17 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$6
	; LineNumber: 555
	; Load Byte array
	; CAST type NADA
	lda title_spr_arr +$18 ; array with const index optimization 
	; Calling storevariable on generic assign expression
	sta $5FF8+$7
	; LineNumber: 556
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #36 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #8
	sta $D000,x
	cpy #0
	beq initSprites_mode3_spritepos914
	lda $D010
	ora #%10000
	sta $D010
	jmp initSprites_mode3_spriteposcontinue915
initSprites_mode3_spritepos914
	lda $D010
	and #%11101111
	sta $D010
initSprites_mode3_spriteposcontinue915
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$12 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 557
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #44 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #10
	sta $D000,x
	cpy #0
	beq initSprites_mode3_spritepos916
	lda $D010
	ora #%100000
	sta $D010
	jmp initSprites_mode3_spriteposcontinue917
initSprites_mode3_spritepos916
	lda $D010
	and #%11011111
	sta $D010
initSprites_mode3_spriteposcontinue917
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$16 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 558
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #46 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #12
	sta $D000,x
	cpy #0
	beq initSprites_mode3_spritepos918
	lda $D010
	ora #%1000000
	sta $D010
	jmp initSprites_mode3_spriteposcontinue919
initSprites_mode3_spritepos918
	lda $D010
	and #%10111111
	sta $D010
initSprites_mode3_spriteposcontinue919
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$17 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 558
	; Setting sprite position
	; isi-pisi: value is constant
	; Load Integer array
	; CAST type NADA
	ldx #48 ; watch for bug, Integer array has max index of 128
	lda title_spr_xpos_arr,x 
	ldy title_spr_xpos_arr+1,x 
	ldx #14
	sta $D000,x
	cpy #0
	beq initSprites_mode3_spritepos920
	lda $D010
	ora #%10000000
	sta $D010
	jmp initSprites_mode3_spriteposcontinue921
initSprites_mode3_spritepos920
	lda $D010
	and #%1111111
	sta $D010
initSprites_mode3_spriteposcontinue921
	inx
	txa
	tay
	; Load Byte array
	; CAST type NADA
	lda  title_spr_ypos_arr +$18 ; array with const index optimization 
	sta $D000,y
	; LineNumber: 560
	rts
end_procedure_initSprites_mode3
	
; // Initialize sprites
	; NodeProcedureDecl 0
	; ***********  Defining procedure : initSprites
	;    Procedure type : User-defined procedure
	; LineNumber: 564
initSprites
	; LineNumber: 565
	; Assigning memory location
	lda #$ff
	; Calling storevariable on generic assign expression
	sta $d015
	; LineNumber: 566
	; Assigning memory location
	; Calling storevariable on generic assign expression
	sta $d01d
	; LineNumber: 567
	; Assigning memory location
	lda #$7f
	; Calling storevariable on generic assign expression
	sta $d017
	; LineNumber: 568
	
; // HW sprite 7 is the drop-arrow on 3rd row; this is regular-height
	; Assigning memory location
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d01c
	; LineNumber: 573
	
; // to save vertical space.
; //SPRITE_MULTICOLOR_REG1	:= light_red;
; //SPRITE_MULTICOLOR_REG2	:= white;	
; // Set initial sprite colors
	jsr set_sprite_colors
	; LineNumber: 574
	rts
end_procedure_initSprites
block1
main_block_begin_
	; LineNumber: 715
	
; //
; // Load MENU,8 (57 blocks/14,478 bytes/14.2 KB) Loads at $0801. Runs with SYS 2061 / call(^2061);
; //
; //
; //
; // Store this procedure in high memory ($C000-$CFFF) so it's not overwritten by loaded program.
; //@startblock $C000 "LOADMENU"
; //procedure loadMenu();
; //begin
; //    asm("
; //        sei
; //
; //		;jsr $FF81	; SCINIT Initialize screen editor - Unnecessary and wipes out loading message
; //		jsr $FF84	; IOINIT Initialize I/O devices
; //		jsr $FF87	; RAMTAS Memory initialization
; //		jsr $FF8A	; RESTOR Restore default system and interrupt vectors
; //
; //        ; Restore normal C64 memory configuration
; //        lda #$37
; //        sta $01
; //
; //        ; Disable VIC raster IRQs
; //        lda #$00
; //        sta $d01a
; //        
; //        ; Restore standard KERNAL IRQ vector
; //        lda #$31
; //        sta $0314
; //        lda #$ea
; //        sta $0315
; //
; //        cli
; //
; //        ; SETNAM: filename = MENU
; //        lda #4
; //        ldx #<menu_filename
; //        ldy #>menu_filename
; //        jsr $ffbd
; //
; //        ; SETLFS: logical file 1, device 8, secondary address 0
; //        lda #1
; //        ldx #8
; //        ldy #0
; //        jsr $ffba
; //
; //        ; LOAD MENU explicitly at $0801
; //        lda #0
; //        ldx #$01
; //        ldy #$08
; //        jsr $ffd5
; //
; //        bcs menu_load_error
; //
; //        ; X/Y contain address immediately following loaded program.
; //        ; Set BASIC variable start.
; //        stx $2d	; #45 (val after loading MENU: 215)
; //        sty $2e	; #46 (val after loading MENU: 63) ($3FD7)
; //
; //		; CLOSE - closes logical file
; //		;lda #1
; //   		;jsr $FFC3	
; //
; //		; CLALL - closes all open files & resets I/O channels
; //		jsr $FFE7
; //		
; //        ; BASIC program starts at $0801
; //        lda #$01
; //        sta $2b
; //        lda #$08
; //        sta $2c
; //
; //        ; SYS 2061 - Calling SYS address runs the program
; //        jmp $080D
; //
; //menu_load_error:
; //        rts
; //
; //menu_filename:
; //        .byte $4d,$45,$4e,$55			; MENU
; //    ");
; //end;
; //@endblock
; //
; //
; // Exit demo and load MENU.PRG
; //procedure exitDemo();
; //begin
; //	
; // Disable interrupt
; //	preventirq();
; //	
; //	disableciainterrupts();
; //	
; //	
; // Disable all sprites
; //	SPRITE_BITMASK := %00000000;
; //	
; //	
; // Stop all sound
; //	LOC_SIGVOL[0] := 0;
; //
; //	
; // Restore defaults
; //	SCREEN_BG_COL[0] := light_blue;
; //	SCREEN_FG_COL[0] := blue;
; //	
; //	SetMemoryConfig(1,1,1); 
; // Enable I/O, Enable KERNAL, Enable BASIC
; //	SetBank(VIC_BANK0);	
; // Bank #0, $0000-$3FFF
; //	SetTextMode(); 
; // Set Text mode
; //	poke(^$D016, 0, $c8); 
; // Set default text mode
; //	SetCharsetAndScreenLocation($D000, $0400);
; //	
; //poke(^$D018, 0, %00010101);	
; // 21 - Set screen address to $0400, char data to default
; //	ClearScreen($20, $0400); 
; // Clear text screen
; //	ClearScreen(light_blue, LOC_CHAR_COLOR); 
; // Fill color ram
; //	moveTo(13, 12, hi(^$0400));
; //	printstring("LOADING MENU..",0,16);
; //
; //
; //	
; // Load menu program
; //	loadMenu();
; //end;
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
; //
; //
; //
; /// End functions
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
; //
; //
; /// Main routine
; // A Koala Painter image typically contains:
; //
; // Section	       	Address (offset)	Size
; // Header byte	   	$0000	2 bytes 		($0	  	- $2)
; // Bitmap data	   	$0002	8000 bytes 	($2	  	- $1F41)
; // Screen RAM	   	$1F42	1000 bytes 	($1F42	- $2329)
; // Color RAM	       	$232A	1000 bytes 	($232A	- $2711)
; // Background color	$2712	1 byte		($2712)
; // Use memory locations:
; // Bitmap data: $2000 VIC ($6000 CPU)
; // Screen RAM: $0400 VIC ($4400 CPU)
; // Color RAM: $D800
; // VIC-II screen control: $D011, $D016, etc
; // This example displays a Koala image assuming it's loaded at $6000.
screenmemory =  $fe
colormemory =  $fb
	; LineNumber: 717
	sei
	; LineNumber: 718
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	; LineNumber: 719
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%110
	sta $01
	; LineNumber: 729
	
; // Enable I/O, Enable KERNAL, Disable BASIC
; // $D01B Sprite to Foreground Display Priority Register
; // value of 0 = sprite appears above foreground
; // value of 1 = sprite appears behind foreground 
; // Bit 0: priority of sprite 0 
; // Bit 1: priority of sprite 1
; // Etc..
; // Sprites with lower numbers appear above higher numbers (sprite 0 above all, etc)
	; Poke
	; Optimization: shift is zero
	lda #$0
	sta $d01b
	; LineNumber: 732
	
; // Play sid file
	; initsid
	lda #0
	tax
	tay
	jsr $1000
	; LineNumber: 735
	
; //StartRasterChain(irRasterMainLoop(),$50,USE_KERNAL);
; // Configure raster IRQ
	; RasterIRQ : Hook a procedure
	lda raster_sid_brk_pos
	sta $d012
	lda #<irRasterMainLoop
	sta $0314
	lda #>irRasterMainLoop
	sta $0315
	; LineNumber: 738
	; Enable raster IRQ
	lda $d01a
	ora #$01
	sta $d01a
	lda #$1B
	sta $d011
	; LineNumber: 739
	asl $d019
	cli
	; LineNumber: 742
	
; // Show Koala image	
	jsr displayKoalaScreen
	; LineNumber: 745
	
; // Init sprites
	jsr initSprites
	; LineNumber: 747
MainProgram_while923
MainProgram_loopstart927
	; Binary clause Simplified: NOTEQUALS
	clc
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_localfailed968
	jmp MainProgram_ctb924
MainProgram_localfailed968
	jmp MainProgram_edblock926
MainProgram_ctb924: ;Main true block ;keep 
	; LineNumber: 748
	; LineNumber: 750
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne MainProgram_eblock972
MainProgram_ctb971: ;Main true block ;keep 
	; LineNumber: 749
	; LineNumber: 752
	
; // Display title sprites only when flag is active
; // Change row 2 sprite pointers
	; wait for raster
	ldx raster_title_brk2_pos ; optimized, look out for bugs
	cpx $d012
	bne *-3
	; LineNumber: 753
	jsr setTitleRow2
	; LineNumber: 754
	jmp MainProgram_edblock973
MainProgram_eblock972
	; LineNumber: 753
	; Binary clause Simplified: EQUALS
	; Load Byte array
	; CAST type NADA
	ldx title_display_idx
	lda title_display_arr,x 
	; Compare with pure num / var optimization
	cmp #$3;keep
	bne MainProgram_edblock987
MainProgram_ctb985: ;Main true block ;keep 
	; LineNumber: 753
	; LineNumber: 756
	
; // Change row 2 sprite pointers
	; wait for raster
	ldx raster_vcf_title_brk2_pos ; optimized, look out for bugs
	cpx $d012
	bne *-3
	; LineNumber: 757
	jsr setVcfTitleRow2
	; LineNumber: 758
MainProgram_edblock987
MainProgram_edblock973
	; LineNumber: 761
	
; // Read Keyboard 
	jsr Key_Read
	; LineNumber: 762
	
; // Reboots C64
; //exitDemo();		
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$07
	ldy #$10
	jsr Key_Held
	; cmp #$00 ignored
	beq MainProgram_edblock993
MainProgram_ctb991: ;Main true block ;keep 
	; LineNumber: 762
	; LineNumber: 762
	jsr $fce2
	; LineNumber: 765
MainProgram_edblock993
	; LineNumber: 767
	
; //screen_bg_col := cyan;
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$05
	ldy #$01
	jsr Key_Held
	; cmp #$00 ignored
	beq MainProgram_edblock999
MainProgram_ctb997: ;Main true block ;keep 
	; LineNumber: 767
	; LineNumber: 768
	; Test Inc dec D
	inc title_text_color
	; LineNumber: 769
	jsr set_sprite_colors
	; LineNumber: 769
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
	; LineNumber: 769
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
	; LineNumber: 770
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
	; LineNumber: 772
MainProgram_edblock999
	; LineNumber: 774
	
; //screen_bg_col  := green;
	; Binary clause Simplified: NOTEQUALS
	clc
	; Assigning to register
	; Assigning register : _xy
	ldx #$05
	ldy #$08
	jsr Key_Held
	; cmp #$00 ignored
	beq MainProgram_edblock1005
MainProgram_ctb1003: ;Main true block ;keep 
	; LineNumber: 774
	; LineNumber: 774
	; Test Inc dec D
	dec title_text_color
	; LineNumber: 776
	jsr set_sprite_colors
	; LineNumber: 776
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
	; LineNumber: 776
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
	; LineNumber: 777
	; Wait
	ldx #$ff ; optimized, look out for bugs
	dex
	bne *-1
	; LineNumber: 779
MainProgram_edblock1005
	; LineNumber: 781
	jmp MainProgram_while923
MainProgram_edblock926
MainProgram_loopend928
	; LineNumber: 783
	jmp * ; loop like (�/%
	; LineNumber: 784
main_block_end_
	; End of program
	; Ending memory block at $2200
EndBlock2200:
	org $4000
StartBlock4000:
	org $4000
sprites:
	incbin	 "C:/Users/fuzzy/Dropbox/Development/trse/c64/vcf-mw-2026///assets/mi-title-sprites-v2.bin"
end_incbin_sprites:
EndBlock4000:
	org $8000
StartBlock8000:
	org $8000
imagedata:
	incbin	 "C:/Users/fuzzy/Dropbox/Development/trse/c64/vcf-mw-2026///assets/melee_island_v11.koa"
end_incbin_imagedata:
EndBlock8000:

