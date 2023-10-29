.p816
.i16
.a8

.segment "HEADER"
	.byte "65C816 TEST          "
.segment "ROMINFO" ; $FFD5
	.byte $30  ; LoROM
	.byte 0    ; no battery/chips
    .byte $08  ; 256K
	.byte 0, 0, 0, 0
	.word $0000, $FFFF ;checksum+complement

native_brk_handler = $1000
native_cop_handler = $1004
emulation_brk_handler = $1008
emulation_cop_handler = $100C
vblank_every_n_tests = 10

.segment "VECTORS"
	.word 0, 0, native_cop_handler, native_brk_handler, 0, 0, 0, 0
	.word 0, 0, emulation_cop_handler, 0, 0, 0, main, emulation_brk_handler

.segment "ZEROPAGE"
.res $10
test_num: .word 0
result_a: .word 0
result_x: .word 0
result_y: .word 0
result_p: .word 0
result_s: .word 0
result_d: .word 0
result_dbr: .byte 0
retaddr: .word 0  ; return address from bankN_save_results
vblank_counter: .byte 0  ; wait for vblank when it reaches 0

.segment "CODE"


main:
	clc
	xce
	sei
	rep #$18  ; 16 bit X/Y
	sep #$20  ; 8 bit A
	ldx #$01EF
	txs

	jsr init

	ldx #$ffff
	stx test_num
	jmp start_tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init:
	jsr init_regs
	jsr init_video_mem
	jsr init_mem

	ldx #txt_running
	ldy #$21
	jsr write_text

	ldx #txt_testnum
	ldy #$61
	jsr write_text

	lda #$0F ; screen on
	sta $2100

	jsr wait_for_vblank

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_regs:
	lda #$8F ; screen off
	sta $2100

	stz $2105 ; BG mode 0, 8x8 tiles
	stz $2106 ; No mosaic
	stz $2107 ; BG1: 32x32 tilemap at address 0
	lda #$04
	sta $210B ; BG1 tiles at byte $8000 / word $4000
	stz $210D ; BG1HOFS = 0
	stz $210D
	lda #$FF
	sta $210E ; BG1VOFS = -1
	sta $210E
	lda #$80
	sta $2115 ; VMAIN: inc address by 1 after high byte
	stz $2121 ; Palette addr = 0
	stz $2122
	stz $2122 ; palette 0 = black
	lda #$FF
	sta $2122
	sta $2122 ; palette 1 = white
	lda #$01
	sta $212C ; enable BG1
	stz $212D ; disable subscreen
	stz $212E ; no window masking
	stz $2130 ; no window force black
	lda #$30
	sta $2131 ; no color math
	stz $2133 ; no hires/interlace/overscan
	lda #$01
	sta $4200 ; no NMI/IRQ, joypad autoread enabled
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_video_mem:
	; clear vmem
	stz $2116
	stz $2117 ; VADDR = 0

	lda #$09
	sta $4300 ; DMA0: to ppu, write to 2 registers, no increment
	lda #$18
	sta $4301 ; DMA0: Write to PPU 2118/2119 (VMDATA)
	lda #<zero
	sta $4302
	lda #>zero
	sta $4303
	lda #^zero
	sta $4304
	stz $4305
	stz $4306 ; 65536 bytes
	lda #$01
	sta $420B ; run DMA0

	; copy font to vmem
	stz $2116
	lda #$40
	sta $2117 ; VADDR = $4000 (word address)
	lda #$01
	sta $4300 ; DMA0: to ppu, 2 bytes->2 registers, inc by 1
	lda #<font
	sta $4302
	lda #>font
	sta $4303
	lda #^font
	sta $4304
	stz $4305
	lda #$08
	sta $4306 ; 2048 bytes
	lda #$01
	sta $420B ; run DMA0
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_mem:
	; Put STP opcodes in some places in case of errant jumps or software interrupts
	lda #$DB
	; BRK/COP handlers
	sta $1000
	sta $1004
	sta $1008
	sta $100C

	lda #$01
	sta vblank_counter
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 0:x = text (null-terminated).  y = vmem word address
write_text:
	sty $2116
@loop:
	lda $00,x
	beq @end
	sta $2118
	stz $2119
	inx
	bra @loop
@end:
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a = val.  y = vmem word address
write_hex8:
	sty $2116
	pha
	lsr a
	lsr a
	lsr a
	lsr a
	clc
	jsr @write_digit
	pla
	and #$0F
@write_digit:  ; write hex digit in A
	cmp #$0A
	bcc @num
	clc
	adc #'A'-$0A-'0'
@num:
	clc
	adc #'0'
	sta $2118
	stz $2119
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 0:x = address. y = vmem word address
write_hex16:
	lda $01,x
	jsr write_hex8
	lda $00,x
	iny
	iny
	jsr write_hex8
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; x = new test num
init_test:
	; Check that we haven't skipped a test
	dex
	cpx test_num
	beq @ok

	; ** Invalid test order - possibly an errant jump **
	clc
	xce
	sei
	rep #$18  ; 16 bit X/Y
	sep #$20  ; 8 bit A
	ldx #$01EF
	txs

	jsr wait_for_vblank
	jsr update_test_num
	ldx #txt_fail
	ldy #$32
	jsr write_text
	ldx #txt_skipped
	ldy #$A1
	jsr write_text
@end:
	jmp @end
	
@ok:
	inx
	stx test_num

	; Every N tests, wait for vblank. This is done to ensure that we're only updating the 
	; screen during vblank so that the update takes effect.
	dec vblank_counter
	bne @after_vblank
	jsr wait_for_vblank
	lda #vblank_every_n_tests
	sta vblank_counter
@after_vblank:

	jsr update_test_num
	rtl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Save the register values, and reset state (D, DBR, etc.)
save_results:
	; p register was already saved, and emulation mode was cleared.
	sei
	rep #$38
	.a16
	.i16
	phd
	pha
	lda #$0000
	tcd
	pla
	sta result_a
	stx result_x
	sty result_y
	plx  ; d register
	stx result_d
	tsc  ; original S value minus 3 (due to jsl).
	inc a
	inc a
	inc a
	sta result_s

	sep #$20
	.a8
	phb
	pla
	sta result_dbr
	lda #$00
	pha
	plb
	rtl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

success:
	jsr wait_for_vblank
	jsr update_test_num
	
	ldx #txt_success
	ldy #$32
	jsr write_text

@end:	jmp @end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

update_test_num:
	ldx #test_num
	ldy #$6E
	jmp write_hex16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

fail:
	ldx #$1ef
	txs  ; in case s is invalid
	jsr wait_for_vblank
	jsr update_test_num

	ldx #txt_fail
	ldy #$32
	jsr write_text
	ldx #txt_a
	ldy #$A1
	jsr write_text
	ldx #txt_x
	ldy #$C1
	jsr write_text
	ldx #txt_y
	ldy #$E1
	jsr write_text
	ldx #txt_p
	ldy #$101
	jsr write_text
	ldx #txt_s
	ldy #$121
	jsr write_text

	lda result_p
	ldy #$105
	jsr write_hex8

	ldx #result_a
	ldy #$A5
	jsr write_hex16

	ldx #result_x
	ldy #$C5
	jsr write_hex16

	ldx #result_y
	ldy #$E5
	jsr write_hex16

	ldx #result_s
	ldy #$125
	jsr write_hex16


	jsr wait_for_key
	jsr wait_for_vblank
	jsr init

	; jump to next test

	rep #$20
	.a16
	lda test_num
	inc a
	asl a  ; A = (test_num+1) * 2
	sec
	adc test_num  ; A = (test_num+1) * 3
	tax
	sep #$20
	.a8
	ldy tests_table,x   ; y = test offset
	lda tests_table+2,x ; a = test bank
	pha
	dey  ; the return address should be 1 less than the target
	phy
	rtl  ; actually a jump to the next test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


wait_for_key:
	ldx #txt_press
	ldy #$341
	jsr write_text
@wait_press:
	lda $4212
	bit #$01
	bne @wait_press  ; wait for joypad auto-read to finish
	lda $4218  ; joypad 1 low byte
	bpl @wait_press

@wait_release:
	lda $4212
	bit #$01
	bne @wait_release  ; wait for joypad auto-read to finish
	lda $4218  ; joypad 1 low byte
	bmi @wait_release

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


wait_for_vblank:
@wait1:
	bit $4210
	bmi @wait1
@wait2:
	bit $4210
	bpl @wait2
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


.include "tests.inc"


.segment "RODATA"
txt_running: .byte "Running tests...", 0
txt_success: .byte "Success", 0
txt_fail: .byte "Failed", 0
txt_skipped: .byte "Invalid test order", 0
txt_testnum: .byte "Test number:", 0
txt_press: .byte "Press A for next tests...", 0
txt_a: .byte "A = ", 0
txt_x: .byte "X = ", 0
txt_y: .byte "Y = ", 0
txt_p: .byte "P = ", 0
txt_s: .byte "S = ", 0
zero:
	.byte 0, 0
font:
	.incbin "font.bin"

tests_table:
	.include "tests_table.inc"
	.faraddr success

.segment "TEST_DATA"  ; At address FFA0. Used by some tests
test_addr:    ; $FFA0
	.word $1212
test_target:  ; $FFA2
	.word $8000
test_target24:  ; $FFA4
	.word $8000
	.byte $7E
