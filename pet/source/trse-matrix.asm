 processor 6502
	org $400
	; Starting new memory block at $400
StartBlock400
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $31,$30,$34,$30
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock400
	org $410
	; Starting new memory block at $410
StartBlock410
MatrixEffect
	jmp block1
i	dc.b	0
j	dc.b	0
ypos	dc.b	 
	org ypos+80
yadd	dc.b	 
	org yadd+80
saddr	dc.w	 
	org saddr+160
sp	= $02
key	dc.b	0
titlemsg		dc.b	"TRSE EXAMPLE 5  'MATRIX'"
	dc.b	0
authormsg		dc.b	"40/80 VERSION 9/2020"
	dc.b	0
promptmsg		dc.b	"USE "
	dc.b	244
	dc.b	"0 OR "
	dc.b	248
	dc.b	"0 COLUMN SCREEN?"
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
initdiv16x8_divisor = $4c     ;$59 used for hi-byte
initdiv16x8_dividend = $4e	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4e ;save memory by reusing divident to store the result
divide16x8	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
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
skip16	dex
	bne divloop16
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $4c
div8x8_d = $4e
div8x8_e = $50
	; Normal 8x8 bin div
div8x8_procedure
	lda #$00
	ldx #$07
	clc
div8x8_loop1 rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2 dex
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
colormemory =  $fc
screen_x = $4c
screen_y = $4e
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
print_text = $4c
print_number_text .dc "    ",0
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
; // User selection for screen width  
; //	Method to get a char from the keyboard buffer
; //	TRSE procedures return accumulator value
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : getKey
	;    Procedure type : User-defined procedure
getKey
	jsr $ffe4
	rts
	
; // getin 
; //	Method which shows title screen and checks screen width
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : showTitle
	;    Procedure type : User-defined procedure
showTitle
	
; // Set uppercase
	; Poke
	; Optimization: shift is zero
	lda #$c
	sta $e84c
	
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
showTitle_clearloop6
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne showTitle_clearloop6
	
; // Show the title text
	; MoveTo optimization
	lda #$58
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	clc
	lda #<titlemsg
	adc #$0
	ldy #>titlemsg
	sta print_text+0
	sty print_text+1
	ldx #$18 ; optimized, look out for bugs
	jsr printstring
	; MoveTo optimization
	lda #$aa
	sta screenmemory
	lda #>$8000
	clc
	adc #$00
	sta screenmemory+1
	clc
	lda #<authormsg
	adc #$0
	ldy #>authormsg
	sta print_text+0
	sty print_text+1
	ldx #$14 ; optimized, look out for bugs
	jsr printstring
	
; // Ask user if they have 40 or 80 column screen
	; MoveTo optimization
	lda #$47
	sta screenmemory
	lda #>$8000
	clc
	adc #$01
	sta screenmemory+1
	clc
	lda #<promptmsg
	adc #$0
	ldy #>promptmsg
	sta print_text+0
	sty print_text+1
	ldx #$1b ; optimized, look out for bugs
	jsr printstring
	
; // Show the quit instruction
	; MoveTo optimization
	lda #$7e
	sta screenmemory
	lda #>$8000
	clc
	adc #$03
	sta screenmemory+1
	clc
	lda #<exitmsg
	adc #$0
	ldy #>exitmsg
	sta print_text+0
	sty print_text+1
	ldx #$d ; optimized, look out for bugs
	jsr printstring
showTitle_while15
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne showTitle_elsedoneblock18
showTitle_ConditionalTrueBlock16: ;Main true block ;keep 
	
; // 009E			No. of Chars. in Keyboard Buffer(Queue)
; // 00E3 		Size of Keyboard Buffer
; // 0270-027A  	Keyboard Buffer Queue(FIFO)
; // Is there a value in the character buffer?
; // NOTE: Doesn't work on early ROMS
; //numkeys := peek(^$009E, 0);
; //if(numkeys > 0) then begin
	; Assigning single variable : key
	jsr getKey
	; Calling storevariable
	sta key
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne showTitle_elsedoneblock36
showTitle_ConditionalTrueBlock34: ;Main true block ;keep 
	
; // 52 is '4'
	; Assigning single variable : myscreenwidth
	lda #$28
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock36
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne showTitle_elsedoneblock42
showTitle_ConditionalTrueBlock40: ;Main true block ;keep 
	
; // 56 is '8'
	; Assigning single variable : myscreenwidth
	lda #$50
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock42
	jmp showTitle_while15
showTitle_elsedoneblock18
	rts
	
; //	Method that initializes the lookup tables used in this program
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitTables
	;    Procedure type : User-defined procedure
InitTables
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
InitTables_forloop46
	
; // Yadd has random values(0-255)/80 +1
	; Assigning single variable : yadd
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
	; Calling storevariable
	ldx i ; optimized, look out for bugs
	sta yadd,x
InitTables_forloopcounter48
	; Compare is onpage
	inc i
	lda myscreenwidth
	cmp i ;keep
	bne InitTables_forloop46
InitTables_loopdone55: ;keep
InitTables_forloopend47
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
InitTables_forloop56
	
; // ypos has values 0-128
	; Assigning single variable : ypos
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	jsr Random
	lsr
	; Calling storevariable
	ldx i ; optimized, look out for bugs
	sta ypos,x
InitTables_forloopcounter58
	; Compare is onpage
	inc i
	lda myscreenwidth
	cmp i ;keep
	bne InitTables_forloop56
InitTables_loopdone61: ;keep
InitTables_forloopend57
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne InitTables_elsedoneblock65
InitTables_ConditionalTrueBlock63: ;Main true block ;keep 
	
; // Sets up the address tables for the screen & color memory    
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
InitTables_dtloop70
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$28
	bcc InitTables_dtnooverflow71
	iny
InitTables_dtnooverflow71
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc InitTables_dtloop70
InitTables_elsedoneblock65
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$50;keep
	bne InitTables_elsedoneblock75
InitTables_ConditionalTrueBlock73: ;Main true block ;keep 
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
InitTables_dtloop80
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$50
	bcc InitTables_dtnooverflow81
	iny
InitTables_dtnooverflow81
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc InitTables_dtloop80
InitTables_elsedoneblock75
	rts
	; NodeProcedureDecl -1
	; ***********  Defining procedure : FillColor
	;    Procedure type : User-defined procedure
xx	dc.b	0
yy	dc.b	0
cc	dc.b	0
FillColor_block82
FillColor
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda yy
	; Compare with pure num / var optimization
	cmp #$f1;keep
	bcc FillColor_elsedoneblock86
FillColor_ConditionalTrueBlock84: ;Main true block ;keep 
	; Assigning single variable : yy
	; Optimizer: a = a +/- b
	lda yy
	clc
	adc #$19
	sta yy
FillColor_elsedoneblock86
	; Binary clause Simplified: LESS
	lda yy
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs FillColor_elsedoneblock92
FillColor_ConditionalTrueBlock90: ;Main true block ;keep 
	; Assigning single variable : sp
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
	bcc FillColor_dtnooverflow96
	iny  ; overflow into high byte
FillColor_dtnooverflow96
	sta sp
	sty sp+1
	; Assigning single variable : sp
	; Store Variable simplified optimization : right-hand term is pure
	ldy #$0 ; optimized, look out for bugs
	lda cc
	sta (sp),y
FillColor_elsedoneblock92
	rts
	
; //	Renders the actual matrix effect
; //
	; NodeProcedureDecl -1
	; ***********  Defining procedure : RenderMatrix
	;    Procedure type : User-defined procedure
RenderMatrix
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
RenderMatrix_forloop98
	
; // Look through 40/80 columns
; // Calculate actualy y screen position
	; Assigning single variable : j
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load Byte array
	ldx i
	lda ypos,x
	lsr
	lsr
	lsr
	; Calling storevariable
	sta j
	; Binary clause Simplified: LESS
	; Compare with pure num / var optimization
	cmp #$19;keep
	bcs RenderMatrix_elsedoneblock124
RenderMatrix_ConditionalTrueBlock122: ;Main true block ;keep 
	
; // Print random character
	; Assigning single variable : sp
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
	bcc RenderMatrix_dtnooverflow128
	iny  ; overflow into high byte
RenderMatrix_dtnooverflow128
	sta sp
	sty sp+1
	; Assigning single variable : sp
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	jsr Random
	lsr
	lsr
	; Calling storevariable
	; Storing to a pointer
	ldy #$0
	sta (sp),y
RenderMatrix_elsedoneblock124
	
; // Fill the "tail" colors
	; Assigning single variable : xx
	lda i
	; Calling storevariable
	sta xx
	; Assigning single variable : yy
	; Optimizer: a = a +/- b
	lda j
	sec
	sbc #$8
	sta yy
	; Assigning single variable : cc
	lda #$20
	; Calling storevariable
	sta cc
	jsr FillColor
	
; // increase ypos[i] with yadd[i]		
	; Assigning single variable : ypos
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Byte array
	ldx i
	lda ypos,x
	clc
	; Load Byte array
	adc yadd,x
	 ; end add / sub var with constant
	; Calling storevariable
	sta ypos,x
	; Binary clause Simplified: GREATEREQUAL
	; Load Byte array
	; Compare with pure num / var optimization
	cmp #$f0;keep
	bcc RenderMatrix_elsedoneblock132
RenderMatrix_ConditionalTrueBlock130: ;Main true block ;keep 
	
; // Reset values when ypos is outside of the screen
	; Assigning single variable : ypos
	; Store Variable simplified optimization : right-hand term is pure
	ldx i ; optimized, look out for bugs
	lda #$0
	sta ypos,x
	; Assigning single variable : yadd
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
	; Calling storevariable
	ldx i ; optimized, look out for bugs
	sta yadd,x
RenderMatrix_elsedoneblock132
RenderMatrix_forloopcounter100
	inc i
	lda myscreenwidth
	cmp i ;keep
	beq RenderMatrix_loopdone139
RenderMatrix_loopnotdone140
	jmp RenderMatrix_forloop98
RenderMatrix_loopdone139
RenderMatrix_forloopend99
	rts
block1
	
; //	Main block
; //
; // Show the title and check number of columns
	jsr showTitle
	
; // Initialize tables	
	jsr InitTables
MainProgram_while141
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_elsedoneblock144
MainProgram_ConditionalTrueBlock142: ;Main true block ;keep 
	
; // Loop effect
	jsr RenderMatrix
	; Binary clause Simplified: EQUALS
	jsr getKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock158
MainProgram_ConditionalTrueBlock156: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop162
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop162
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock158
	jmp MainProgram_while141
MainProgram_elsedoneblock144
	; End of program
	; Ending memory block
EndBlock410
