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
Boxes
	jmp block1
saddr	dc.w	 
	org saddr+50
box	dc.b $055, $043, $049, $05d, $04b, $043, $04a, $05d
i	dc.b	$00
x1	dc.b	$00
y1	dc.b	$00
dx	dc.b	$00
dy	dc.b	$00
temp	dc.b	$00
titlemsg		dc.b	"TRSE 'BOXES' DEMO"
	dc.b	0
authormsg		dc.b	"40/80 VERSION FUZZYBAD"
	dc.b	0
exitmsg		dc.b	211
	dc.b	208
	dc.b	193
	dc.b	195
	dc.b	197
	dc.b	" TO QUIT"
	dc.b	0
myscreenwidth	dc.b	$00
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initdrawtextbox
	;    Procedure type : User-defined procedure
	; ----------
	; InitDrawTextBox
	; addr vars: addrtableaddr,petsciitable
idtb_at_lo: dc.b 0
idtb_at_hi: dc.b 0
idtb_petscii_tl: dc.b 0
idtb_petscii_t: dc.b 0
idtb_petscii_tr: dc.b 0
idtb_petscii_r: dc.b 0
idtb_petscii_br: dc.b 0
idtb_petscii_b: dc.b 0
idtb_petscii_bl: dc.b 0
idtb_petscii_l: dc.b 0
	; temp vars: col,row,width,height
idtb_t_col: dc.b 0
idtb_t_row: dc.b 0
idtb_t_wid: dc.b 0
idtb_t_hei: dc.b 0
idtb_tmp: dc.b 0
AddrCalcRowTextBoxDraw
	; Calculate screen offset for row number in A
	ldy idtb_at_hi
	asl
	clc
	adc idtb_at_lo
	sta $80
	bcc NooverflowTextBox
	iny
NooverflowTextBox
	sty $80+1
	ldy #0
	lda ($80),y
	tax
	iny
	lda ($80),y
	sta $80+1
	stx $80
	rts
PerformTextBoxDraw
	; Draw box top
	lda idtb_t_row
	jsr AddrCalcRowTextBoxDraw
	lda idtb_petscii_tl
	ldy idtb_t_col
	sta ($80),y
	lda idtb_petscii_t
TopLoopTextBox
	iny
	sta ($80),y
	cpy idtb_t_wid
	bne TopLoopTextBox
	iny
	lda idtb_petscii_tr
	sta ($80),y
	; Draw box center
	ldx idtb_t_row
CenterLoopTextBox
	inx
	stx idtb_tmp
	txa
	jsr AddrCalcRowTextBoxDraw
	lda idtb_petscii_l
	ldy idtb_t_col
	sta ($80),y
	ldy idtb_t_wid
	iny
	lda idtb_petscii_r
	sta ($80),y
	ldx idtb_tmp
	cpx idtb_t_hei
	bne CenterLoopTextBox
	; Draw box bottom
	inc idtb_t_hei
	lda idtb_t_hei
	jsr AddrCalcRowTextBoxDraw
	lda idtb_petscii_bl
	ldy idtb_t_col
	sta ($80),y
	lda idtb_petscii_b
BotLoopTextBox
	iny
	sta ($80),y
	cpy idtb_t_wid
	bne BotLoopTextBox
	iny
	lda idtb_petscii_br
	sta ($80),y
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto2
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
initmoveto_moveto2
	rts
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
	
; // Text for splash screen
; // User selection for screen width 
; //	Method to get a char from the keyboard buffer
; //	TRSE procedures return accumulator value
	; NodeProcedureDecl -1
	; ***********  Defining procedure : GetKey
	;    Procedure type : User-defined procedure
GetKey
	jsr $ffe4
	rts
	
; // getin
; // Wait for user input
	; NodeProcedureDecl -1
	; ***********  Defining procedure : WaitForKeypress
	;    Procedure type : User-defined procedure
WaitForKeypress
	
; // Pause until key pressed
	lda #$0
	; Calling storevariable on generic assign expression
	sta temp
WaitForKeypress_while5
WaitForKeypress_loopstart9
	; Binary clause Simplified: EQUALS
	lda temp
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne WaitForKeypress_elsedoneblock8
WaitForKeypress_ConditionalTrueBlock6: ;Main true block ;keep 
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta temp
	jmp WaitForKeypress_while5
WaitForKeypress_elsedoneblock8
WaitForKeypress_loopend10
	rts
	
; // Determine screen width to use
	; NodeProcedureDecl -1
	; ***********  Defining procedure : SetScreenWidth
	;    Procedure type : User-defined procedure
SetScreenWidth
	; Binary clause Simplified: EQUALS
	; Peek
	lda $e000 + $1;keep
	; Compare with pure num / var optimization
	cmp #$4b;keep
	bne SetScreenWidth_elseblock16
SetScreenWidth_ConditionalTrueBlock15: ;Main true block ;keep 
	
; // Determine PET model by checking first byte of EDIT ROM at $E000
; //	$A0 [160] = B1
; //	$48 [72]  = B2
; //	$36 [54]  = B4-40
; //	$4B [75]  = B4-80	
	lda #$50
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	jmp SetScreenWidth_elsedoneblock17
SetScreenWidth_elseblock16
	lda #$28
	; Calling storevariable on generic assign expression
	sta myscreenwidth
SetScreenWidth_elsedoneblock17
	
; //centerX := myscreenwidth / 2 - 1;
	; Clear screen with offset
	lda #$20
	ldx #$fa
SetScreenWidth_clearloop22
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne SetScreenWidth_clearloop22
	rts
	
; //	Method which shows title screen and checks screen width
	; NodeProcedureDecl -1
	; ***********  Defining procedure : showTitle
	;    Procedure type : User-defined procedure
showTitle
	
; // Determine screen width and clear it
	jsr SetScreenWidth
	
; // Set uppercase
	; Poke
	; Optimization: shift is zero
	lda #$c
	sta $e84c
	
; // Center the title text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #8
showTitle_rightvarAddSub_var24 = $54
	sta showTitle_rightvarAddSub_var24
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var24
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$1
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<titlemsg
	adc #$0
	ldy #>titlemsg
	sta print_text+0
	sty print_text+1
	ldx #$11 ; optimized, look out for bugs
	jsr printstring
	
; // Center the author text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #11
showTitle_rightvarAddSub_var27 = $54
	sta showTitle_rightvarAddSub_var27
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var27
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$3
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<authormsg
	adc #$0
	ldy #>authormsg
	sta print_text+0
	sty print_text+1
	ldx #$16 ; optimized, look out for bugs
	jsr printstring
	
; // Center exit message
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
showTitle_rightvarAddSub_var30 = $54
	sta showTitle_rightvarAddSub_var30
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var30
	; Calling storevariable on generic assign expression
	sta x1
	sta screen_x
	lda #$c
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<exitmsg
	adc #$0
	ldy #>exitmsg
	sta print_text+0
	sty print_text+1
	ldx #$d ; optimized, look out for bugs
	jsr printstring
	
; // Pause until user presses key
	jsr WaitForKeypress
	rts
block1
	
; // Show the title and check number of columns
	jsr showTitle
	
; // Set screen background/border color	
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop33
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop33
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne MainProgram_elsedoneblock37
MainProgram_ConditionalTrueBlock35: ;Main true block ;keep 
	
; // Sets up the address tables for the screen & color memory    
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
MainProgram_dtloop42
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow43
	iny
MainProgram_dtnooverflow43
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc MainProgram_dtloop42
MainProgram_elsedoneblock37
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$50;keep
	bne MainProgram_elsedoneblock47
MainProgram_ConditionalTrueBlock45: ;Main true block ;keep 
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
MainProgram_dtloop52
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$50
	bcc MainProgram_dtnooverflow53
	iny
MainProgram_dtnooverflow53
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc MainProgram_dtloop52
MainProgram_elsedoneblock47
	
; // dx and dy are initialized to 1
	lda #$1
	; Calling storevariable on generic assign expression
	sta dx
	; Calling storevariable on generic assign expression
	sta dy
MainProgram_while54
MainProgram_loopstart58
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_localfailed89
	jmp MainProgram_ConditionalTrueBlock55
MainProgram_localfailed89
	jmp MainProgram_elsedoneblock57
MainProgram_ConditionalTrueBlock55: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
MainProgram_forloop91
	
; // Make sure we only draw 1 box per frame
; // Add the delta dx and dy to x and y
	; Wait
	ldx #$2 ; optimized, look out for bugs
	dex
	bne *-1
MainProgram_forloopcounter93
MainProgram_loopstart94
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$ff
	cmp i ;keep
	bne MainProgram_forloop91
MainProgram_loopdone98: ;keep
MainProgram_forloopend92
MainProgram_loopend95
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x1
	clc
	adc dx
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x1
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y1
	clc
	adc dy
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta y1
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc #$9
	 ; end add / sub var with constant
	cmp x1 ;keep
	bne MainProgram_casenext100
	
; // Flip dx and dy when borders are reached
; //71: dx := -1;
	lda #$ff
	; Calling storevariable on generic assign expression
	sta dx
	jmp MainProgram_caseend99
MainProgram_casenext100
	lda #$0
	cmp x1 ;keep
	bne MainProgram_casenext102
	lda #$1
	; Calling storevariable on generic assign expression
	sta dx
MainProgram_casenext102
MainProgram_caseend99
	lda #$14
	cmp y1 ;keep
	bne MainProgram_casenext105
	lda #$ff
	; Calling storevariable on generic assign expression
	sta dy
	jmp MainProgram_caseend104
MainProgram_casenext105
	lda #$0
	cmp y1 ;keep
	bne MainProgram_casenext107
	lda #$1
	; Calling storevariable on generic assign expression
	sta dy
MainProgram_casenext107
MainProgram_caseend104
	
; // Draw two boxes in opposing corners
	; ----------
	; DrawTextBox addrtable, petsciiarray, column, row, width, height
	lda #<saddr ; address table lo
	ldx #>saddr ; address table hi
	sta idtb_at_lo
	stx idtb_at_hi
	ldx #8
MainProgram_PetsciiCopy109
	dex
	lda box,x
	sta idtb_petscii_tl,x
	cpx #0
	bne MainProgram_PetsciiCopy109
	lda x1
	sta idtb_t_col
	lda y1
	sta idtb_t_row
	lda #$9
	clc
	adc idtb_t_col
	sbc #1
	sta idtb_t_wid
	lda #$5
	clc
	adc idtb_t_row
	sbc #1
	sta idtb_t_hei
	jsr PerformTextBoxDraw
	; ----------
	; DrawTextBox addrtable, petsciiarray, column, row, width, height
	lda #<saddr ; address table lo
	ldx #>saddr ; address table hi
	sta idtb_at_lo
	stx idtb_at_hi
	ldx #8
MainProgram_PetsciiCopy110
	dex
	lda box,x
	sta idtb_petscii_tl,x
	cpx #0
	bne MainProgram_PetsciiCopy110
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc #$9
	 ; end add / sub var with constant
	sec
	sbc x1
	 ; end add / sub var with constant
	sta idtb_t_col
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #$14
	sec
	sbc y1
	 ; end add / sub var with constant
	sta idtb_t_row
	lda #$9
	clc
	adc idtb_t_col
	sbc #1
	sta idtb_t_wid
	lda #$5
	clc
	adc idtb_t_row
	sbc #1
	sta idtb_t_hei
	jsr PerformTextBoxDraw
	
; // Trigger NMI	    
	; Binary clause Simplified: EQUALS
	jsr GetKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock114
MainProgram_ConditionalTrueBlock112: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop118
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop118
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock114
	jmp MainProgram_while54
MainProgram_elsedoneblock57
MainProgram_loopend59
	; End of program
	; Ending memory block at $410
EndBlock410:
