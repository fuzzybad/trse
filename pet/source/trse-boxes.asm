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
Boxes
	jmp block1
saddr	dc.w	 
	org saddr+50
box	dc.b $055, $043, $049, $05d, $04b, $043, $04a, $05d
i	dc.b	$00
x	dc.b	$00
y	dc.b	$00
dx	dc.b	$00
dy	dc.b	$00
key	dc.b	0
titlemsg		dc.b	"TRSE EXAMPLE 3 'BOXES'"
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
	; ***********  Defining procedure : initdrawtextbox
	;    Procedure type : User-defined procedure
	; ----------
	; InitDrawTextBox
	; addr vars: addrtableaddr,petsciitable
idtb_at_lo dc.b 0
idtb_at_hi dc.b 0
idtb_petscii_tl dc.b 0
idtb_petscii_t dc.b 0
idtb_petscii_tr dc.b 0
idtb_petscii_r dc.b 0
idtb_petscii_br dc.b 0
idtb_petscii_b dc.b 0
idtb_petscii_bl dc.b 0
idtb_petscii_l dc.b 0
	; temp vars: col,row,width,height
idtb_t_col dc.b 0
idtb_t_row dc.b 0
idtb_t_wid dc.b 0
idtb_t_hei dc.b 0
idtb_tmp dc.b 0
AddrCalcRowTextBoxDraw
	; Calculate screen offset for row number in A
	ldy idtb_at_hi
	asl
	clc
	adc idtb_at_lo
	sta $4c
	bcc NooverflowTextBox
	iny
NooverflowTextBox
	sty $4c+1
	ldy #0
	lda ($4c),y
	tax
	iny
	lda ($4c),y
	sta $4c+1
	stx $4c
	rts
PerformTextBoxDraw
	; Draw box top
	lda idtb_t_row
	jsr AddrCalcRowTextBoxDraw
	lda idtb_petscii_tl
	ldy idtb_t_col
	sta ($4c),y
	lda idtb_petscii_t
TopLoopTextBox
	iny
	sta ($4c),y
	cpy idtb_t_wid
	bne TopLoopTextBox
	iny
	lda idtb_petscii_tr
	sta ($4c),y
	; Draw box center
	ldx idtb_t_row
CenterLoopTextBox
	inx
	stx idtb_tmp
	txa
	jsr AddrCalcRowTextBoxDraw
	lda idtb_petscii_l
	ldy idtb_t_col
	sta ($4c),y
	ldy idtb_t_wid
	iny
	lda idtb_petscii_r
	sta ($4c),y
	ldx idtb_tmp
	cpx idtb_t_hei
	bne CenterLoopTextBox
	; Draw box bottom
	inc idtb_t_hei
	lda idtb_t_hei
	jsr AddrCalcRowTextBoxDraw
	lda idtb_petscii_bl
	ldy idtb_t_col
	sta ($4c),y
	lda idtb_petscii_b
BotLoopTextBox
	iny
	sta ($4c),y
	cpy idtb_t_wid
	bne BotLoopTextBox
	iny
	lda idtb_petscii_br
	sta ($4c),y
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
	
; // Text for splash screen  	
; // User selection for screen width 
; //
; //	This tutorial draws boxes on the screen using the "drawcolortextbox" method. This method
; //	requires pointers to screen address tables, which are set up using the createaddresstable methods.
; //
; //	createaddresstable lets the user create a lookup table instead of using multiplications. example:
; //	to find the adress on the screen, a "address:=position + y*40;" multiplication is required. An adress
; //	table eliminates the need of having this multiplication, and relies solely on a lookup.  	
; //
; // 
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
showTitle_clearloop5
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne showTitle_clearloop5
	
; // Show the title text
	; MoveTo optimization
	lda #$59
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
	ldx #$16 ; optimized, look out for bugs
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
showTitle_while14
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne showTitle_elsedoneblock17
showTitle_ConditionalTrueBlock15: ;Main true block ;keep 
	
; // 009E			No. of Chars. in Keyboard Buffer(Queue)
; // 00E3 		Size of Keyboard Buffer
; // 0270-027A  	Keyboard Buffer Queue(FIFO)
; // Is there a value in the character buffer?
; // NOTE: Doesn't work on early ROMS
; //numkeys := peek(^$009E, 0);
	; Assigning single variable : key
	jsr getKey
	; Calling storevariable
	sta key
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$34;keep
	bne showTitle_elsedoneblock35
showTitle_ConditionalTrueBlock33: ;Main true block ;keep 
	
; // 52 is '4'
	; Assigning single variable : myscreenwidth
	lda #$28
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock35
	; Binary clause Simplified: EQUALS
	lda key
	; Compare with pure num / var optimization
	cmp #$38;keep
	bne showTitle_elsedoneblock41
showTitle_ConditionalTrueBlock39: ;Main true block ;keep 
	
; // 56 is '8'
	; Assigning single variable : myscreenwidth
	lda #$50
	; Calling storevariable
	sta myscreenwidth
showTitle_elsedoneblock41
	jmp showTitle_while14
showTitle_elsedoneblock17
	rts
block1
	
; // Show the title and check number of columns
	jsr showTitle
	
; // Set screen background/border color	
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop44
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop44
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne MainProgram_elsedoneblock48
MainProgram_ConditionalTrueBlock46: ;Main true block ;keep 
	
; // Sets up the address tables for the screen & color memory    
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
MainProgram_dtloop53
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow54
	iny
MainProgram_dtnooverflow54
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc MainProgram_dtloop53
MainProgram_elsedoneblock48
	; Binary clause Simplified: EQUALS
	lda myscreenwidth
	; Compare with pure num / var optimization
	cmp #$50;keep
	bne MainProgram_elsedoneblock58
MainProgram_ConditionalTrueBlock56: ;Main true block ;keep 
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$8000
	lda #<$8000
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
MainProgram_dtloop63
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$50
	bcc MainProgram_dtnooverflow64
	iny
MainProgram_dtnooverflow64
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc MainProgram_dtloop63
MainProgram_elsedoneblock58
	
; // dx and dy are initialized to 1
	; Assigning single variable : dx
	lda #$1
	; Calling storevariable
	sta dx
	; Assigning single variable : dy
	; Calling storevariable
	sta dy
MainProgram_while65
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_localfailed97
	jmp MainProgram_ConditionalTrueBlock66
MainProgram_localfailed97
	jmp MainProgram_elsedoneblock68
MainProgram_ConditionalTrueBlock66: ;Main true block ;keep 
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
MainProgram_forloop99
	
; // Make sure we only draw 1 box per frame
; // Add the delta dx and dy to x and y
	; Wait
	ldx #$2 ; optimized, look out for bugs
	dex
	bne *-1
MainProgram_forloopcounter101
	; Compare is onpage
	inc i
	lda #$ff
	cmp i ;keep
	bne MainProgram_forloop99
MainProgram_loopdone104: ;keep
MainProgram_forloopend100
	; Assigning single variable : x
	; 8 bit binop
	; Add/sub where right value is constant number
	lda x
	clc
	adc dx
	 ; end add / sub var with constant
	; Calling storevariable
	sta x
	; Assigning single variable : y
	; 8 bit binop
	; Add/sub where right value is constant number
	lda y
	clc
	adc dy
	 ; end add / sub var with constant
	; Calling storevariable
	sta y
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc #$9
	 ; end add / sub var with constant
MainProgram_temp1_var107 = $56
	sta MainProgram_temp1_var107
	lda x
	cmp MainProgram_temp1_var107 ;keep
	bne MainProgram_casenext106
	
; // Flip dx and dy when borders are reached
; //71: dx := -1;
	; Assigning single variable : dx
	lda #$ff
	; Calling storevariable
	sta dx
	jmp MainProgram_caseend105
MainProgram_casenext106
	lda x
	cmp #$0 ;keep
	bne MainProgram_casenext109
	; Assigning single variable : dx
	lda #$1
	; Calling storevariable
	sta dx
	jmp MainProgram_caseend105
MainProgram_casenext109
MainProgram_caseend105
	lda y
	cmp #$14 ;keep
	bne MainProgram_casenext112
	; Assigning single variable : dy
	lda #$ff
	; Calling storevariable
	sta dy
	jmp MainProgram_caseend111
MainProgram_casenext112
	lda y
	cmp #$0 ;keep
	bne MainProgram_casenext114
	; Assigning single variable : dy
	lda #$1
	; Calling storevariable
	sta dy
	jmp MainProgram_caseend111
MainProgram_casenext114
MainProgram_caseend111
	
; // Draw two boxes in opposing corners
	; ----------
	; DrawTextBox addrtable, petsciiarray, column, row, width, height
	lda #<saddr ; address table lo
	ldx #>saddr ; address table hi
	sta idtb_at_lo
	stx idtb_at_hi
	ldx #8
MainProgram_PetsciiCopy116
	dex
	lda box,x
	sta idtb_petscii_tl,x
	cpx #0
	bne MainProgram_PetsciiCopy116
	lda x
	sta idtb_t_col
	lda y
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
MainProgram_PetsciiCopy117
	dex
	lda box,x
	sta idtb_petscii_tl,x
	cpx #0
	bne MainProgram_PetsciiCopy117
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda myscreenwidth
	sec
	sbc #$9
	 ; end add / sub var with constant
	sec
	sbc x
	 ; end add / sub var with constant
	sta idtb_t_col
	; 8 bit binop
	; Add/sub where right value is constant number
	lda #$14
	sec
	sbc y
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
	; Binary clause Simplified: EQUALS
	jsr getKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock121
MainProgram_ConditionalTrueBlock119: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop125
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop125
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock121
	jmp MainProgram_while65
MainProgram_elsedoneblock68
	; End of program
	; Ending memory block
EndBlock410
