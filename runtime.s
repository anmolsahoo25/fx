    .section    __TEXT,__text,regular,pure_instructions
    .globl  _dummy_personality
    .p2align    2
_dummy_personality:
    ret

    .section    __TEXT,__text,regular,pure_instructions
    .globl  _exn
    .p2align    2
_exn:
    ret

    .section    __TEXT,__text,regular,pure_instructions
    .globl  _fiber1
    .p2align    2
_fiber1:
    .cfi_startproc

    ; load parent sp in w8
    add x8, sp, #0

    ; update new sp
    add sp, x1, 0

    ; create stack space to store x29 , x30, old sp, landing pad
    sub sp, sp, #32

    ; store fp and link register
    stp x29, x30, [sp, #16]

    ; store parent sp in first slot
    str x8, [sp,#8]

    ; store handler address
    str x3, [sp]

    ; calculate fp
    add x8, sp, #32

    ; store fp in x29
    mov x29, x8

    ; call function
    blr x2

    ; reload old stack pointer into x8
    ldr x8, [sp, #8]

    ; restore fp and lnk register
    ldp x29, x30, [sp, #16]

    ; clear stack
    add sp, sp, #32

    ; move old stack into sp
    add sp, x8, 0

    ; return
    ret
    .cfi_endproc

    .section    __TEXT,__text,regular,pure_instructions
    .globl  _perform
    .p2align    2
_perform:
    .cfi_startproc

    ; create 2 slots for storing x29, x30
    sub sp, sp, #16

    ; store x29 and x30
    stp x29, x30, [sp]

    ; move sp into x8
    add x8, sp, 0

    ; store sp in continuation object
    str x8, [x1,#8]

    ; load parent sp into x8
    ldr x8, [x29, #-24]

    ; load parent sp into sp
    add sp, x8, 0

    ; load exn handler address in x8
    ldr x8, [x29, #-32]

    ; jump to handler
    br x8

    .cfi_endproc

    .section    __TEXT,__text,regular,pure_instructions
    .globl  _continue
    .p2align    2
_continue:

    ; load continuation sp
    ldr x8, [x1,#8]

    ; load current sp
    mov sp, x8

    ; load link register and fp
    ldp x29,x30,[sp]

    ; erase two slack slots
    add sp, sp, #16

    ; return to caller
    ret
