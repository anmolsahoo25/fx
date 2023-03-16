    .section    __TEXT,__text,regular,pure_instructions
    .globl  _fiber1
    .p2align    2
_fiber1:
    .cfi_startproc

    ; load parent sp in w8
    add x8, sp, #0

    ; update new sp
    add sp, x1, 0

    ; create stack space to store x29 , x30, old sp, extra
    sub sp, sp, #32

    ; store fp and link register
    stp x29, x30, [sp, #16]

    ; store parent sp in first slot
    str x8, [sp,#8]

    ; call function
    blr x2

    ; reload old stack pointer into x8
    ldr x8, [sp, #8]
    add sp, x8, 0

    ; restore fp and lnk register
    ldp x29, x30, [sp, #16]

    ; clear stack
    add sp, sp, #32

    ; return
    ret
    .cfi_endproc
