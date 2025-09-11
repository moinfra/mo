.text
.arch rv64gc
.file "generated.s"

.globl add
.globl main

add:
entry:
    addi sp, sp, -24
    sw ra, 20(sp)
    addi t2, sp, -4
    sw t0, 0(vreg1000)
    addi t3, sp, -8
    sw t1, 0(vreg1002)
    lw t0, 0(vreg1000)
    lw t1, 0(vreg1002)
    add t2, t0, t1
    add a0, t2, zero
    lw ra, 20(sp)
    addi sp, sp, 24
    ret

main:
entry:
    addi sp, sp, -16
    sw ra, 12(sp)
    add a0, t0, zero
    add a1, t1, zero
    jal ra, add
    add t0, a0, zero
    add a0, t0, zero
    lw ra, 12(sp)
    addi sp, sp, 16
    ret

