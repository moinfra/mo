.text
.arch rv64gc
.file "generated.s"

.globl add
.globl main

add:
entry:
    addi sp, sp, -24
    sw ra, 20(sp)
    addi vreg1000, sp, -4
    sw vreg1001, 0(vreg1000)
    addi vreg1002, sp, -8
    sw vreg1003, 0(vreg1002)
    lw vreg1004, 0(vreg1000)
    lw vreg1005, 0(vreg1002)
    add vreg1006, vreg1004, vreg1005
    add a0, vreg1006, zero
    lw ra, 20(sp)
    addi sp, sp, 24
    ret

main:
entry:
    addi sp, sp, -16
    sw ra, 12(sp)
    add a0, vreg1000, zero
    add a1, vreg1001, zero
    jal ra, add
    add vreg1002, a0, zero
    add a0, vreg1002, zero
    lw ra, 12(sp)
    addi sp, sp, 16
    ret

