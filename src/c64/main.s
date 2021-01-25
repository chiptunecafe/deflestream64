section "Basic Autostart"
.org $0801
    .word next, 10
    .byte $9e
    .text " 2064"
    .byte 0
next:
    .word 0

section "Main"
.org $810
    sei                             ; disable interrupts
    
    jmp install_loader
error:
    lda #$02
    sta $d021
    jmp error

install_loader:
    jsr $1000
    bcs error

    lda #$05                        ; bank out everything
    sta $01

    lda #$7f                        ; disable cia interrupts
    sta $dc0d
    sta $dd0d

    lda $dc0d                       ; ack pending interrupts
    lda $dd0d
    asl $d019

    ldx #$7e                        ; shorten stack size a bit
    txs
    
    ldx #$00                        ; copy in resident loader code
copy_resident:
    lda resident,x
    sta $0180,x
    lda resident+$100,x
    sta $0280,x
    lda resident+$200,x
    sta $0380,x
    inx
    bne copy_resident

    lda #$20                        ; clear screen
clear_loop:
    sta $0400,x
    sta $0500,x
    sta $0600,x
    sta $0700,x
    inx
    bne clear_loop

    ldx #$00                        ; write text to screen
copy_line0:
    lda line0,x
    beq copy_line0_done
    sta $0400+40,x
    inx
    jmp copy_line0
copy_line0_done:
    ldx #$00
copy_line1:
    lda line1,x
    beq copy_line1_done
    sta $0400+80,x
    inx
    jmp copy_line1
copy_line1_done:
    ldx #$00
copy_line2:
    lda line2,x
    beq copy_line2_done
    sta $0400+120,x
    inx
    jmp copy_line2

copy_line2_done:
    inc $d020                       ; load first two banks
    ldx #<bank0
    ldy #>bank0
    jsr $180
    bcc lb0_ok
    jmp error

lb0_ok:
    ldx #<nextfile
    ldy #>nextfile
    jsr $180
    bcc lb1_ok
    jmp error

lb1_ok:
    dec $d020

    lda #$01                        ; store greatest loaded bank
    sta $22

    lda #$01                        ; enable vic raster irq
    sta $d01a

    lda #$e0                        ; set raster irq line
    sta $d012
    lda #$1b
    sta $d011

    lda #<irq                       ; set irq vector
    sta $fffe
    lda #>irq
    sta $ffff

    lda #$00                        ; store pointer to vgm data
    sta $20
    lda #$10
    sta $21

    asl $d019                       ; ack pending vic interrupts

    cli                             ; reenable interrupts

wait_bank_2:
    lda $21
    cmp #$70
    bcc wait_bank_2

    inc $d020
    ldx #<nextfile
    ldy #>nextfile
    jsr $180
    bcc wb2_ok
    jmp error
wb2_ok:
    dec $d020
    inc $22
    lda $22
    cmp #N_BANKS
    beq done

wait_bank_1:
    lda $21
    cmp #$70
    bcs wait_bank_1

    inc $d020
    ldx #<nextfile
    ldy #>nextfile
    jsr $180
    bcc wb1_ok
    jmp error
wb1_ok:
    dec $d020
    inc $22
    lda $22
    cmp #N_BANKS
    beq done

    jmp wait_bank_2

done:
    jmp done

irq:
    pha                             ; push cpu state
    txa
    pha
    tya
    pha

    dec $d021

    lda $21                         ; loop to previous bank if needed
    cmp #$90
    bcc no_loop
    lda #$00
    sta $20
    lda #$10
    sta $21
no_loop:
    ldy #$00                        ; vgm playback routine
register_loop:
    lda ($20),y
    cmp #$ff
    beq not_data
    tax
    iny
    lda ($20),y
    sta $d400,x
    iny
    jmp register_loop
    
not_data:
    iny
    lda ($20),y
    cmp #$ff
    bne song_not_ended
    dey
    jmp song_ended
song_not_ended:
    iny

song_ended:
    tya                             ; save new playback ptr
    clc
    adc $20
    sta $20
    bcc no_ptr_carry
    inc $21
no_ptr_carry:
    inc $d021

    asl $d019                       ; ack interrupt, restore state, and return
    pla
    tay
    pla
    tax
    pla
    rti

bank0:
    .text petscii "bank0"
    .byte 0
nextfile:
    .byte 0

line0: 
    .text c64screen " deflestream64 0.1.0"
    .byte 0
line1: 
    .text c64screen " code by rytone"
    .byte 0
line2: 
    .text c64screen " loader by krill"
    .byte 0

segment "Loader"
.org $1000
install:
    .incbin "./loader-install.bin"

resident:
    .incbin "./loader-resident.bin"

.export "Main"
