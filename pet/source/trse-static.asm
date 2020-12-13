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
Randomness
	jmp block1
x	dc.b	0
y	dc.b	0
index	dc.b	0
random_values	dc.b	 
	org random_values+256
screenP	= $02
zp	= $04
key	dc.b	0
titlemsg		dc.b	"TRSE EXAMPLE 4  'RANDOMNESS'"
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
	
; // Used for keyboard input
; // Text for splash screen  	
; // Holds value from user selection for screen width  
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
	lda #$56
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
	ldx #$1c ; optimized, look out for bugs
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
	
; // Initialize a random table of 256 bytes
; // generator
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitializeRandom
	;    Procedure type : User-defined procedure
InitializeRandom
	; Assigning single variable : x
	lda #$0
	; Calling storevariable
	sta x
InitializeRandom_forloop46
	
; // same as : for x:=0 to 0 do begin..
	; Assigning single variable : random_values
	jsr Random
	; Calling storevariable
	ldx x ; optimized, look out for bugs
	sta random_values,x
InitializeRandom_forloopcounter48
	; Compare is onpage
	inc x
	; Integer constant assigning
	ldy #$01
	lda #$00
	cmp x ;keep
	bne InitializeRandom_forloop46
InitializeRandom_loopdone51: ;keep
InitializeRandom_forloopend47
	rts
block1
	
; // Show the title and check number of columns
	jsr showTitle
	jsr InitializeRandom
	
; // point to start of random table
	; Assigning single variable : index
	lda #$0
	; Calling storevariable
	sta index
MainProgram_while52
	; Binary clause Simplified: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	beq MainProgram_localfailed77
	jmp MainProgram_ConditionalTrueBlock53
MainProgram_localfailed77
	jmp MainProgram_elsedoneblock55
MainProgram_ConditionalTrueBlock53: ;Main true block ;keep 
	
; // infinite loop
; // Set pointer to point to beginning of screen/color ram($0400 and $D800)
	; Assigning single variable : screenP
	lda #$00
	ldx #$80
	sta screenP
	stx screenP+1
	; Assigning single variable : y
	lda #$0
	; Calling storevariable
	sta y
MainProgram_forloop79
	
; // loop y		
; // moves current screen position
; // Select some random color
	; Assigning single variable : zp
	; INTEGER optimization: a=b+c 
	lda #<random_values
	clc
	adc index
	sta zp+0
	lda #>random_values
	adc #0
	sta zp+1
	; memcpy
	ldy #0
MainProgram_memcpy88
	lda (zp),y
	sta (screenP),y
	iny
	cpy #$50
	bne MainProgram_memcpy88
	; Assigning single variable : index
	; Optimizer: a = a +/- b
	lda index
	clc
	adc #$b
	sta index
	; Assigning single variable : screenP
	lda screenP
	clc
	adc #$50
	sta screenP+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc MainProgram_WordAdd89
	inc screenP+1
MainProgram_WordAdd89
MainProgram_forloopcounter81
	; Compare is onpage
	inc y
	lda #$19
	cmp y ;keep
	bne MainProgram_forloop79
MainProgram_loopdone90: ;keep
MainProgram_forloopend80
	; Binary clause Simplified: EQUALS
	jsr getKey
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne MainProgram_elsedoneblock94
MainProgram_ConditionalTrueBlock92: ;Main true block ;keep 
	
; // Exit if user pressed space
; // Clear screen
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop98
	dex
	sta $0000+$8000,x
	sta $00fa+$8000,x
	sta $01f4+$8000,x
	sta $02ee+$8000,x
	sta $03e8+$8000,x
	sta $04e2+$8000,x
	sta $05dc+$8000,x
	sta $06d6+$8000,x
	bne MainProgram_clearloop98
	
; //call(^$fd16);	
; // RESET
	jsr $fd49
MainProgram_elsedoneblock94
	jmp MainProgram_while52
MainProgram_elsedoneblock55
	; End of program
	; Ending memory block
EndBlock410
