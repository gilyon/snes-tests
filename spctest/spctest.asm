.p816
.i16
.a8

; CPU <--> SPC communication:
; - Normal IPL ROM load+execute program
; - SPC sets port0 = 0, CPU waits for it. 
; - CPU sets port2/3 = last test num (or -1 at start), port1 = 1, SPC waits for it
; - SPC continuously writes 16-bit test numbers to port2/port3, without waiting for response
; - When all tests done successfully:
;   - SPC sets port2/3 = last test num, port0 = 1
;   - CPU sets port1 = 0, SPC waits for it.
;   - SPC jumps to IPL ROM. CPU repeats from start if needed, otherwise stops.
; - When a test fails:
;   - SPC sets port2/3 = test num, port1 = PSW, port0 = 2
;   - CPU sets port1 = 2, SPC waits for it.
;   - SPC sets port1 = A, port2 = X, port3 = Y, port0 = 3
;   - SPC and CPU stop


.segment "HEADER"
	.byte "SPC-700 TEST         "
.segment "ROMINFO" ; $FFD5
	.byte $30  ; LoROM
	.byte 0    ; no battery/chips
        .byte $07  ; 128K
	.byte 0, 0, 0, 0
	.word $0000, $FFFF ;checksum+complement

.segment "VECTORS"
	.word 0, 0, 0, 0, 0, 0, 0, 0
	.word 0, 0, 0, 0, 0, 0, main, 0

.segment "ZEROPAGE"
spc_addr: .word 0
spc_size: .word 0
last_test_num: .word 0

.segment "CODE"

.macro load_and_run_spc addr, end_addr
	lda #^addr
	ldx #.loword(addr)
	ldy #end_addr-addr
	jsr load_spc
	jsr wait_result
.endmacro


main:
	clc
	xce
	sei
	rep #$18  ; 16 bit X/Y
	sep #$20  ; 8 bit A
	ldx #$1FFF
	txs
	jsr init_regs
	jsr init_video_mem

	ldx #txt_running
	ldy #$21
	jsr write_text

	ldx #txt_testnum
	ldy #$61
	jsr write_text

	lda #$0F ; screen on
	sta $2100

	ldx #$ffff
	stx last_test_num
	load_and_run_spc spc0, spc0end
	load_and_run_spc spc1, spc1end
	load_and_run_spc spc2, spc2end

	ldx #txt_success
	ldy #$32
	jsr write_text
@end:	jmp @end

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
	stz $4200 ; no NMI, IRQ, joypad autoread
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


; Input: a = bank, x = addr, y = size
load_spc:
	stx spc_addr
	sty spc_size
	pha
	plb  ; DBR = bank

	ldx #$BBAA
@wait1:
	cpx $2140
	bne @wait1

	ldx #$300  ; dest address
	stx $2142
	lda #$CC
	sta $2141  ; any nonzero value
	sta $2140
@wait2:
	cmp $2140
	bne @wait2

	ldy #$0000
load_spc_loop:
	lda (spc_addr),y
	sta $2141
	tya
	sta $2140

@wait3:
	cmp $2140
	bne @wait3

	iny
	cpy spc_size
	bne load_spc_loop

	ldx #$300  ; execution address
	stx $2142
	stz $2141
	ina
	ina
	sta $2140

@wait4: ; wait for acknowledgement
	cmp $2140
	bne @wait4

	; IPL protocol ends here

	; wait until port0 == 0
@wait5:
	lda $2140
	bne @wait5

	ldx last_test_num
	stx $2142  ; port 2/3 = 1 less than next test num (will be checked by SPC)

	lda #$01
	sta $2141  ; set port1 = 1

	lda #$00  ; reset DBR to 0
	pha
	plb

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

update_test_num:
	ldx $2142
	stx last_test_num
	ldx #$2142
	ldy #$6E
	jmp write_hex16
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

wait_result:
@wait1:
	jsr update_test_num   ; This may not have effect while rendering, but we repeat it later anyway
	lda $2140
	beq @wait1

	jsr wait_for_vblank
	jsr update_test_num
	lda $2140
	cmp #$01
	bne @failed

	;  Tests succeeded
	stz $2141  ; port1 = 0  (tell SPC to continue into IPL ROM)
	rts

@failed:
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

	lda $2141  ; PSW
	ldy #$105
	jsr write_hex8

	lda #$02
	sta $2141

	lda #$03
@wait3:
	cmp $2140
	bne @wait3
	
	lda $2141 ; a
	ldy #$A5
	jsr write_hex8

	lda $2142 ; x
	ldy #$C5
	jsr write_hex8

	lda $2143 ; y
	ldy #$E5
	jsr write_hex8

@end: bra @end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


wait_for_vblank:
@wait1:
	bit $4210
	bmi @wait1
@wait2:
	bit $4210
	bpl @wait2
	rts


.segment "RODATA"
txt_running: .byte "Running tests...", 0
txt_success: .byte "Success", 0
txt_fail: .byte "Failed", 0
txt_testnum: .byte "Test number:", 0
txt_a: .byte "A = ", 0
txt_x: .byte "X = ", 0
txt_y: .byte "Y = ", 0
txt_p: .byte "P = ", 0
zero:
	.byte 0, 0
font:
	.incbin "font.bin"

spc0:
	.incbin "spc_tests0.spc", $300
spc0end:

.segment "BANK1"
spc1:
	.incbin "spc_tests1.spc", $300
spc1end:

.segment "BANK2"
spc2:
	.incbin "spc_tests2.spc", $300
spc2end:
