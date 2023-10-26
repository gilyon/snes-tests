; All memory up to $300 is uninitialized
org $0
    ; First 16 bytes are reserved for testing instructions.
org $10
test_num:
    dw 0
result_a: db 0
result_x: db 0
result_y: db 0
result_p: db 0

; Load RAM from here
org $300
main:
    mov $f4, #$00
    mov $f1, #$00  ; disable IPL ROM
.wait1:  ; wait for CPU to set port1 = 1
    cmp $f5, #$01
    bne .wait1

    ; CPU put last test num in ports 2/3
    mov a, $F6
    mov test_num, a
    mov a, $F7
    mov test_num+1, a

    jmp start_tests


; input: YA = test number
init_test:
    incw test_num
    cmpw ya, test_num
    beq .ok
    jmp fail  ; We are at the wrong test - possibly due to an errant jump
.ok:
    movw $f6, ya   ; let main CPU know what test is running
    ret


; saves registers to memory and returns psw in a. Also clears P flag
save_results:
    push psw
    clrp
    mov result_a, a
    mov result_x, x
    mov result_y, y
    pop a
    mov result_p, a
    ret


success:
    mov $f4, #$01
.wait1:  ; Wait for CPU set set port1 = 0
    cmp $f5, #$00
    bne .wait1
    
    mov $f1, #$80  ; enable IPL ROM
    jmp $ffc0  ; jump to IPL ROM


fail:
    ; $f6,$f7 contain last test number
    mov a, result_p
    mov $f5, a
    mov $f4, #$02
.wait1:     ; wait for CPU to set port1 = 2
    cmp $f5, #$02
    bne .wait1

    mov a, result_a
    mov $f5, a
    mov a, result_x
    mov $f6, a
    mov a, result_y
    mov $f7, a
    mov $f4, #$03
.end: bra .end
