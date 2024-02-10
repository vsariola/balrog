XRES equ 1024
YRES equ 768
STEPS equ 20
ITERS equ 23
XCOORD equ 0xFFE8
YCOORD equ 0xFFEC

org 100h

; The video mode setup and bankswitching loop are based on code by TomCat, see
; https://www.pouet.net/prod.php?which=93549
; The shadertoy prototype is here: https://www.shadertoy.com/view/4XsGRr

main:
    push    0xa000
    pop     es
    scasw
    cwd
    mov     bx, 0x118
.nextbank:
    add     ax, 0x4F02
    int     10h
    sub     bx, bx
.nextpixel:
    mov     bp, BASE ; y at [bx], x at [bx-4]
    mov     si, XRES*4
    pusha
    xchg    ax, di
    div     si
    sub     ax, YRES/2
    sub     dh, XRES/2*4/256
    pusha
    xor     dx, dx
    fldz
    fst     dword [bx] ; save glow
    fld     dword [c_camy+bp-BASE]
    fld     dword [c_camz+bp-BASE]
.marchloop:
    fld     st2
    fdiv    dword [c_xmult+bp-BASE]
    fcos
    fdiv    dword [c_xdiv+bp-BASE]
    fstp    dword [si]
    fldz
    fld     st1
    fld     st3
    fld     st5         ; stack: tx ty tz r pz py px
    call    bp
    jc      .out
    inc     dx
    cmp     dx, STEPS
    jl      .marchloop
.out:
    mov     [si], dx
    fild    word [si]
    fmul    dword [c_colorscale+bp-BASE]
    fmul    st0
    popa
    popa
.colorloop:
    fld     st1
    fldpi
    fmulp   st1, st0
    fcos
    fmul    dword [bx]
    fadd    st1
    fsin
    fabs
    fimul   word [c_255+bp-BASE]
    fistp   word [si]
    lodsb
    stosb
    fstp    st1
    inc     si
    jpo     .colorloop
    stosb
    fstp    st0
    test    di, di
    jnz     .nextpixel
    inc     dx
    mov     al, 3
    cmp     dl, XRES*YRES/16384
    jne     .nextbank
.forever:
    jmp     .forever ; No extra cool points for clean exits :(
c_255 equ $-1
    db 0x00

BASE EQU $

inner:
    mov     cl, ITERS
.maploop:
    fld     st0          ; tx tx
    frndint
    fsubp   st1, st0     ; tx-round(tx)
    fabs                 ; tx = abs(tx-round(tx))
    fadd    st0
    fld     dword [c_rscale+bp-BASE]
    fmulp   st4, st0
    fld     st0
    fmul    st0
    faddp   st4, st0     ; r += tx*tx
    fxch    st2, st0
    fxch    st1, st0
    fld     st2
    fmul    dword [si]
    faddp   st1, st0
    fld     st0
    fmul    dword [si]
    c_stepsizediv_x equ $-2
    fsubp   st3, st0
    loop    .maploop
.clearloop:
    fstp    st0
    inc     cx
    jpo     .clearloop
    fdiv    dword [c_rdiv+bp-BASE]
    fsqrt
    fld1
    fsub    st1, st0    ; 1 dist=sqrt(r/rdiv)-1 px py pz
    fld     st1
    fmul    st0
    fmul    dword [c_glowdecay+bp-BASE]
    faddp   st1         ; 1+dist*dist*glowdecay
    fdivr   dword [c_glowamount+bp-BASE]
    fadd    dword [bx] ; glow += glowamount/(1+dist*dist*glowdecay)
    fstp    dword [bx]
    ; X:
    fild    word [bx+XCOORD]
    fmul    st0, st1
    fidiv   word [c_stepsizediv_x+bp-BASE]
    faddp   st4
    ; Y:
    fild    word [bx+YCOORD]
    fmul    st0, st1
    fidiv   word [c_stepsizediv+bp-BASE]
    faddp   st3
    ; is dist < MINDIST?
    fcom    dword [c_mindist+bp-BASE]
    fnstsw  ax
    sahf
    ; Z:
    fdiv    dword [c_stepsizediv_z+bp-BASE]
    faddp   st1
    ret

; floating constants truncated using https://www.h-schmidt.net/FloatConverter/IEEE754.html
c_mindist equ $-3
    db      0x38  ; 0.0001

c_glowamount equ $-2
c_colorscale equ $-2
    dw      0x3d61  ; 0.055

c_stepsizediv equ $-1
    db      0x03 ; 807

c_stepsizediv_z equ $-3
    db      0x40 ; 2.1006666666666662

c_glowdecay equ $-2
    dw      0x461c ; 1e4

c_rscale equ $-2
    db      0xa1, 0x3f  ; 1.2599210498948732

c_rdiv equ $-2
    dw      0x434b ; 203.18733465192963

c_camz equ $-1
    db      0xcc, 0x12, 0x42 ; 36.7

c_xdiv equ $-1
    db      0x09, 0x00, 0x40 ; 2.0006

c_fpustatus equ $
c_xmult equ $-2
    dw      0x3f2a

c_camy equ $-2
    dw      0x3f1c ; 0.61
