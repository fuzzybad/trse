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
MatrixEffect
	jmp block1
i	dc.b	0
j	dc.b	0
temp	dc.b	0
x1	dc.b	0
x2	dc.b	0
y1	dc.b	0
y2	dc.b	0
ypos	dc.b	 
	org ypos+80
yadd	dc.b	 
	org yadd+80
saddr	dc.w	 
	org saddr+160
sp	= $68
titlemsg		dc.b	"TRSE 'MATRIX' DEMO"
	dc.b	0
authormsg		dc.b	"40/80 MESSAGE VERSION FUZZYBAD"
	dc.b	0
exitmsg		dc.b	211
	dc.b	208
	dc.b	193
	dc.b	195
	dc.b	197
	dc.b	" TO QUIT"
	dc.b	0
msg_0		dc.b	"WORLD OF COMMODORE 2021"
	dc.b	0
msg_1		dc.b	"                       "
	dc.b	0
msg_2		dc.b	"   DECEMBER 4, 2021    "
	dc.b	0
msg_3		dc.b	"                       "
	dc.b	0
msg_4		dc.b	"TORONTO PET USERS GROUP"
	dc.b	0
msg_len	dc.b	$17
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initrandom256
	;    Procedure type : User-defined procedure
	; init random256
Random
	lda #$01
	asl
	bcc initrandom256_RandomSkip3
	eor #$4d
initrandom256_RandomSkip3
	sta Random+1
	rts
	
; // Text for splash screen  	
; // Text for message area
; // # of chars in the width
; // @TODO: There must be some way to calculate string length..
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
WaitForKeypress_while6
WaitForKeypress_loopstart10
	; Binary clause Simplified: EQUALS
	lda temp
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne WaitForKeypress_elsedoneblock9
WaitForKeypress_ConditionalTrueBlock7: ;Main true block ;keep 
	jsr GetKey
	; Calling storevariable on generic assign expression
	sta temp
	jmp WaitForKeypress_while6
WaitForKeypress_elsedoneblock9
WaitForKeypress_loopend11
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
	bne SetScreenWidth_elseblock17
SetScreenWidth_ConditionalTrueBlock16: ;Main true block ;keep 
	
; // Determine PET model by checking first byte of EDIT ROM at $E000
; //	$A0 [160] = B1
; //	$48 [72]  = B2
; //	$36 [54]  = B4-40
; //	$4B [75]  = B4-80	
	lda #$50
	; Calling storevariable on generic assign expression
	sta myscreenwidth
	jmp SetScreenWidth_elsedoneblock18
SetScreenWidth_elseblock17
	lda #$28
	; Calling storevariable on generic assign expression
	sta myscreenwidth
SetScreenWidth_elsedoneblock18
	
; //centerX := myscreenwidth / 2 - 1;
	; Clear screen with offset
	lda #$20
	ldx #$fa
SetScreenWidth_clearloop23
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne SetScreenWidth_clearloop23
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
	lda #9
showTitle_rightvarAddSub_var25 = $54
	sta showTitle_rightvarAddSub_var25
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var25
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
	ldx #$12 ; optimized, look out for bugs
	jsr printstring
	
; // Center the author text
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #15
showTitle_rightvarAddSub_var28 = $54
	sta showTitle_rightvarAddSub_var28
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var28
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
	ldx #$1e ; optimized, look out for bugs
	jsr printstring
	
; // Center exit message
	; 8 bit binop
	; Add/sub right value is variable/expression
	lda #6
showTitle_rightvarAddSub_var31 = $54
	sta showTitle_rightvarAddSub_var31
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda myscreenwidth
	lsr
	sec
	sbc showTitle_rightvarAddSub_var31
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
	
; //	Method that initializes the lookup tables used in this program
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitTables
	;    Procedure type : User-defined procedure
InitTables
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
InitTables_forloop35
	
; // Yadd has random values(0-255)/80 +1
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit div
	jsr Random
	sta div8x8_d
	; Load right hand side
	lda myscreenwidth
	sta div8x8_c
	jsr div8x8_procedure
	clc
	adc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	ldx i ; optimized, look out for bugs
	sta yadd,x
InitTables_forloopcounter37
InitTables_loopstart38
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda myscreenwidth
	cmp i ;keep
	bne InitTables_forloop35
InitTables_loopdone46: ;keep
InitTables_forloopend36
InitTables_loopend39
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
InitTables_forloop47
	
; // ypos has values 0-128
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	jsr Random
	lsr
	; Calling storevariable on generic assign expression
	ldx i ; optimized, look out for bugs
	sta ypos,x
InitTables_forloopcounter49
InitTables_loopstart50
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda myscreenwidth
	cmp i ;keep
	bne InitTables_forloop47
InitTables_loopdone54: ;keep
InitTables_forloopend48
InitTables_loopend51
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne InitTables_elsedoneblock58
InitTables_ConditionalTrueBlock56: ;Main true block ;keep 
	
; // Sets up the address tables for the screen & color memory    
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
InitTables_dtloop63
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$28
	bcc InitTables_dtnooverflow64
	iny
InitTables_dtnooverflow64
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc InitTables_dtloop63
InitTables_elsedoneblock58
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$50;keep
	bne InitTables_elsedoneblock68
InitTables_ConditionalTrueBlock66: ;Main true block ;keep 
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
InitTables_dtloop73
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$50
	bcc InitTables_dtnooverflow74
	iny
InitTables_dtnooverflow74
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc InitTables_dtloop73
InitTables_elsedoneblock68
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : FillColor
	;    Procedure type : User-defined procedure
xx	dc.b	0
yy	dc.b	0
cc	dc.b	0
FillColor_block75
FillColor
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda yy
	; Compare with pure num / var optimization
	cmp #$f1;keep
	bcc FillColor_elsedoneblock79
FillColor_ConditionalTrueBlock77: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	lda yy
	clc
	adc #$19
	sta yy
FillColor_elsedoneblock79
	; Binary clause Simplified: LESS
	lda yy
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs FillColor_elsedoneblock85
FillColor_ConditionalTrueBlock83: ;Main true block ;keep 
	; ----------
	; AddressTable address, xoffset, yoffset
	; yoffset is complex
	lda yy
	asl ; *2
	tax
	lda saddr,x   ; Address of table lo
	ldy saddr+1,x   ; Address of table hi
	clc
	adc xx
	bcc FillColor_dtnooverflow89
	iny  ; overflow into high byte
FillColor_dtnooverflow89
	sta sp
	sty sp+1
	lda cc
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (sp),y
FillColor_elsedoneblock85
	rts
	
; //	Show the message
	; NodeProcedureDecl -1
	; ***********  Defining procedure : displayMessage
	;    Procedure type : User-defined procedure
displayMessage
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc msg_len
	 ; end add / sub var with constant
	lsr
	; Calling storevariable on generic assign expression
	sta x1
	; 8 bit binop
	; Add/sub where right value is constant number
	clc
	adc msg_len
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta x2
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne displayMessage_elsedoneblock94
displayMessage_ConditionalTrueBlock92: ;Main true block ;keep 
	lda #$6
	; Calling storevariable on generic assign expression
	sta y1
	lda #$e
	; Calling storevariable on generic assign expression
	sta y2
displayMessage_elsedoneblock94
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$50;keep
	bne displayMessage_elsedoneblock100
displayMessage_ConditionalTrueBlock98: ;Main true block ;keep 
	lda #$3
	; Calling storevariable on generic assign expression
	sta y1
	lda #$8
	; Calling storevariable on generic assign expression
	sta y2
displayMessage_elsedoneblock100
	
; // @TODO Put strings into array and print using loop
	lda x1
	sta screen_x
	lda #$3
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<msg_0
	adc #$0
	ldy #>msg_0
	sta print_text+0
	sty print_text+1
	ldx msg_len ; optimized, look out for bugs
	jsr printstring
	lda x1
	sta screen_x
	lda #$4
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<msg_1
	adc #$0
	ldy #>msg_1
	sta print_text+0
	sty print_text+1
	ldx msg_len ; optimized, look out for bugs
	jsr printstring
	lda x1
	sta screen_x
	lda #$5
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<msg_2
	adc #$0
	ldy #>msg_2
	sta print_text+0
	sty print_text+1
	ldx msg_len ; optimized, look out for bugs
	jsr printstring
	lda x1
	sta screen_x
	lda #$6
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<msg_3
	adc #$0
	ldy #>msg_3
	sta print_text+0
	sty print_text+1
	ldx msg_len ; optimized, look out for bugs
	jsr printstring
	lda x1
	sta screen_x
	lda #$7
	sta screen_y
	lda #>$8000
	jsr SetScreenPosition
	clc
	lda #<msg_4
	adc #$0
	ldy #>msg_4
	sta print_text+0
	sty print_text+1
	ldx msg_len ; optimized, look out for bugs
	jsr printstring
	rts
	
; //	Render the matrix effect
	; NodeProcedureDecl -1
	; ***********  Defining procedure : RenderMatrix
	;    Procedure type : User-defined procedure
RenderMatrix
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
RenderMatrix_forloop114
	
; // Loop through the 40/80 columns
; // Calculate actualy y screen position
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load Byte array
	ldx i
	lda ypos,x
	lsr
	lsr
	lsr
	; Calling storevariable on generic assign expression
	sta j
	; Binary clause Simplified: LESS
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs RenderMatrix_elsedoneblock162
RenderMatrix_ConditionalTrueBlock160: ;Main true block ;keep 
	
; // random character is located between 0 and 64
	; Binary clause Simplified: LESS
	lda i
	; Compare with pure num / var optimization
	cmp x1;keep
	bcs RenderMatrix_localfailed183
	jmp RenderMatrix_ConditionalTrueBlock177
RenderMatrix_localfailed183: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: GREATEREQUAL
	lda i
	; Compare with pure num / var optimization
	cmp x2;keep
	bcc RenderMatrix_localfailed182
	jmp RenderMatrix_ConditionalTrueBlock177
RenderMatrix_localfailed182: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: LESS
	lda j
	; Compare with pure num / var optimization
	cmp y1;keep
	bcs RenderMatrix_localfailed184
	jmp RenderMatrix_ConditionalTrueBlock177
RenderMatrix_localfailed184: ;keep
	; ; logical OR, second chance
	; Binary clause Simplified: GREATEREQUAL
	lda j
	; Compare with pure num / var optimization
	cmp y2;keep
	bcc RenderMatrix_elsedoneblock179
RenderMatrix_ConditionalTrueBlock177: ;Main true block ;keep 
	
; // Print random character
; // This creates space for the message
	; ----------
	; AddressTable address, xoffset, yoffset
	; yoffset is complex
	lda j
	asl ; *2
	tax
	lda saddr,x   ; Address of table lo
	ldy saddr+1,x   ; Address of table hi
	clc
	adc i
	bcc RenderMatrix_dtnooverflow186
	iny  ; overflow into high byte
RenderMatrix_dtnooverflow186
	sta sp
	sty sp+1
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	jsr Random
	lsr
	lsr
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (sp),y
RenderMatrix_elsedoneblock179
RenderMatrix_elsedoneblock162
	
; // Fill the "tail" colors
	lda i
	; Calling storevariable on generic assign expression
	sta xx
	; Optimizer: a = a +/- b
	lda j
	sec
	sbc #$8
	sta yy
	lda #$20
	; Calling storevariable on generic assign expression
	sta cc
	jsr FillColor
	
; // increase ypos[i] with yadd[i]		
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Byte array
	ldx i
	lda ypos,x
	clc
	; Load Byte array
	adc yadd,x
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta ypos,x
	; Binary clause Simplified: GREATEREQUAL
	; Load Byte array
	; Compare with pure num / var optimization
	cmp #$f0;keep
	bcc RenderMatrix_elsedoneblock190
RenderMatrix_ConditionalTrueBlock188: ;Main true block ;keep 
	
; // Reset values when ypos is outside of the screen
	lda #$0
	; Calling storevariable on generic assign expression
	ldx i ; optimized, look out for bugs
	sta ypos,x
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit div
	jsr Random
	sta div8x8_d
	; Load right hand side
	lda myscreenwidth
	sta div8x8_c
	jsr div8x8_procedure
	clc
	adc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	ldx i ; optimized, look out for bugs
	sta yadd,x
RenderMatrix_elsedoneblock190
RenderMatrix_forloopcounter116
RenderMatrix_loopstart117
	; Test Inc dec D
	inc i
	lda myscreenwidth
	cmp i ;keep
	beq RenderMatrix_loopdone197
RenderMatrix_loopnotdone198
	jmp RenderMatrix_forloop114
RenderMatrix_loopdone197
RenderMatrix_forloopend115
RenderMatrix_loopend118
	rts
block1
	
; //	Main block
; // Show the title and check number of columns
	jsr showTitle
	
; // Initialize tables	
	jsr InitTables
MainProgram_while199
MainProgram_loopstart203
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock202
MainProgram_ConditionalTrueBlock200: ;Main true block ;keep 
	
; // Display the message(not working from here for some reason)
; //displayMessage();
; // Loop effect
	jsr displayMessage
	jsr RenderMatrix
	
; // Trigger NMI	    
	; Binary clause Simplified: EQUALS
	jsr GetKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock218
MainProgram_ConditionalTrueBlock216: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop222
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop222
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock218
	jmp MainProgram_while199
MainProgram_elsedoneblock202
MainProgram_loopend204
	; End of program
	; Ending memory block at $410
EndBlock410:
