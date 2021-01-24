
.include "cpu.inc"
.include "cia.inc"
.include "via.inc"

.include "drives/drivecode-common.inc"

ZP_FIRST              = $00

BUFFER                = $00
SYS_SP                = $01
JOBCODESTABLE         = $02; fixed in ROM
JOBTRKSCTTABLE        = $0b; fixed in ROM - $0b..$1c
FILETRACK             = $0b
FILESECTOR            = $0c
FILENAMEHASHLO        = $0d
FILENAMEHASHHI        = $0e
NUMFILES              = $0f
CURRDIRBLOCKTRACK     = $10
CYCLESTARTENDTRACK    = $11
CYCLESTARTENDSECTOR   = $12
DIRCYCLEFLAG          = $13
NEXTDIRBLOCKTRACK     = $14
NEXTDIRBLOCKSECTOR    = $15
FIRSTDIRSECTOR        = $16
NEXTCONTIGUOUSBLOCK   = $17
TEMP                  = $18

ZP_LAST               = $18

;BLOCKBUFFERJOBTRACK  = $1b; fixed in ROM - track for job at buffer 8 ($0b00)
;BLOCKBUFFERJOBSECTOR = $1c; fixed in ROM - sector for job at buffer 8 ($0b00)

DISKCHANGED           = $25; fixed in ROM
DRIVESTATE            = $26; fixed in ROM
DRIVEOFF              = $00; literal
OPEN_FILE_TRACK       = $4c; fixed in ROM

LEDSTATE              = $79; fixed in ROM
IRQVECTOR_LO          = $0192
IRQVECTOR_HI          = $0193
HDRS2                 = $01bc
ROMOS_DIRTRACK81      = $022b
OPEN_FILE_SECTOR      = $028b

STROBE_CONTROLLER     = $ff54

READ_DV               = $80
MOTOFFI_DV            = $8a
SEEK_DV               = $8c

OK_DV                 = $00

BUFFER0               = $0300
BUFFERSIZE            = $0100

SENDTABLELO           = $0900
SENDTABLEHI           = $0a00

BLOCKBUFFER81         = $0b00
TRACKOFFSET           = $00
SECTOROFFSET          = $01

REQUESTEDTRACK        = JOBTRKSCTTABLE + (2 * BUFFERINDEX) + TRACKOFFSET
REQUESTEDSECTOR       = JOBTRKSCTTABLE + (2 * BUFFERINDEX) + SECTOROFFSET
LOADEDSECTOR          = JOBTRKSCTTABLE + (2 * BUFFERINDEX) + SECTOROFFSET

LINKTRACK             = BLOCKBUFFER81 + TRACKOFFSET
LINKSECTOR            = BLOCKBUFFER81 + SECTOROFFSET

BINARY_NIBBLE_MASK    = %00001111

ROMOS_MAXTRACK        = $8f; MAXTRACK81 - 1
ROMOS_MAXSECTOR       = $75; MAXSECTOR81 + 1
MAXTRACK81            = 80; literal
MAXSECTOR81           = 39; literal

.if !DISABLE_WATCHDOG
RESET_TIMERB          = $cb9f
CONTROLLERIRQPERIODFD = $4e20
    .macro INIT_CONTROLLER
            jsr initcontrl
    .endmacro
.else
    .macro INIT_CONTROLLER
    .endmacro
.endif

BUFFERINDEX           = (BLOCKBUFFER81 - BUFFER0) / BUFFERSIZE

            .org $0300

.export cmdfdfix0 : absolute
.export cmdfdfix1 : absolute
.export cmdfdfix2 : absolute
.if (::PLATFORM <> diskio::platform::COMMODORE_128) & (!DISABLE_WATCHDOG)
.export cmdfdfix3 : absolute
.export cmdfdfix4 : absolute
.endif

drvcodebeg81: .byte .hibyte(drvcodeend81 - * + $0100 - $01); init transfer count hi-byte

SENDNIBBLETAB:
            BIT0DEST = 3
            BIT1DEST = 1
            BIT2DEST = 2
            BIT3DEST = 0

            .repeat $10, I
                .byte (((~I >> 0) & 1) << BIT0DEST) | (((~I >> 1) & 1) << BIT1DEST) | (((~I >> 2) & 1) << BIT2DEST) | (((~I >> 3) & 1) << BIT3DEST)
            .endrep

syszpbuf:   .res ZP_LAST - ZP_FIRST + 1

.if !DISABLE_WATCHDOG
sysirqvbuf: .byte 0, 0
.endif

filename:   .res 16; note: this is not in the zeropage

dcodinit81: lda #CLK_OUT
            sta CIA_PRB; signal idle to the host, note that ATNA response with ATN_IN low is not possible on 1581,
                       ; so a KERNAL LISTEN command cannot be detected for automatic uninstallation

            ldx #ZP_LAST - ZP_FIRST
:           lda ZP_FIRST,x
            sta syszpbuf,x
            dex
            bpl :-
            tsx
            stx SYS_SP

.if !DISABLE_WATCHDOG
            lda IRQVECTOR_LO
            sta sysirqvbuf + 0
            lda IRQVECTOR_HI
            sta sysirqvbuf + 1
.endif
            jsr motrledoff

            ldx #$00
:           txa
            and #BINARY_NIBBLE_MASK
            tay
            lda SENDNIBBLETAB,y
            sta SENDTABLELO,x
            txa
            lsr
            lsr
            lsr
            lsr
            tay
            lda SENDNIBBLETAB,y
            sta SENDTABLEHI,x
            inx
            bne :-

            lda cmdfdfix2; 0 for FD
            beq :+
.if ::PLATFORM = diskio::platform::COMMODORE_128
            lda #$06; set burst timer, anything below $06 yields transfer errors
            sta CIA_TA_LO
            lda #$00
            sta CIA_TA_HI
            lda #IOMODE_OUTPUT | COUNT_PHI2 | FORCE_LOAD | CONTINUOUS | TIMER_START; enable burst mode
            sta CIA_CRA
.endif; ::PLATFORM = diskio::platform::COMMODORE_128
.if !DISABLE_WATCHDOG
            ; watchdog initialisation
            jsr initwatchd
            lda #CIA_CLR_INTF | EVERY_IRQ
            sta CIA_ICR
            lda #CIA_SET_INTF | TIMERB_IRQ
            sta CIA_ICR
            bne :++
:           jsr initwatchd
.endif
:           jsr getdirtrk
            ldy #0
            jsr trackseek

            lda #$ff
            sta NUMFILES

idleloop:   ldx #$00; turn motor and busy led off
            lda #DRIVE_LED; check if busy led is lit
            and CIA_PRA
            beq driveidle
            dex; fade off the busy led, then turn motor off
driveidle:  jsr fadeled; fade off the busy led
            lda CIA_PRB
            and #ATN_IN | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            eor #ATN_IN | CLK_OUT | CLK_IN            | DATA_IN
            beq driveidle; wait until there is something to do
            lda CIA_PRB; to be safe, read a second time
            and #ATN_IN | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            eor #ATN_IN | CLK_OUT | CLK_IN            | DATA_IN
            lsr; check for reset, uninstallation or custom drive code upload
            beq loadfile
            jmp uninstall; check for reset or uninstallation

loadfile:
    .if !DISABLE_WATCHDOG
            jsr enablewdog; enable watchdog, the computer might be reset while sending over a
                          ; byte, leaving the drive waiting for handshake pulses
    .endif; !DISABLE_WATCHDOG

            GET_FILENAME 1581
            ; matches against hash of filename in FILENAMEHASHLO/HI
            BLOCKBUFFER = BLOCKBUFFER81
            FIND_FILE 1581
            bcs filenfound

            lda FILENAMEHASHVALLO + 1,x ; and PREPARE_NEXT_FILE
            sta FILENAMEHASHLO          ; functionality
            lda FILENAMEHASHVALHI + 1,x ; store hash of next file's
            sta FILENAMEHASHHI          ; name for loadnext
            lda DIRTRACKS,x
            tax

            ; check for illegal track or sector
            beq toillegal
            cpx ROMOS_MAXTRACK; #MAXTRACK81 - 1
            beq :+
            bcs toillegal + $01
:           cpy ROMOS_MAXSECTOR; #MAXSECTOR81 + 1
            bcc :+
toillegal:  sec
cmdfdfix0:  jmp illegalts; is changed to bit illegalts on FD2000/4000 to disable illegal track or sector error,
                         ; ROM variables for logical track/sector boundaries aren't known (probably around MAXTRACKFD = $54)

:           tya
            pha
            jsr busyledon
            pla
            tay; FILESECTOR
            txa; FILETRACK

            ldx #$02
            stx NEXTCONTIGUOUSBLOCK

loadblock:  sta REQUESTEDTRACK
            sty REQUESTEDSECTOR
:           jsr getblockag
            bcs :-

           ;ldy LINKSECTOR
           ;lda LINKTRACK
            pha
            beq :+
            ldy #$ff
:           lda LINKSECTOR; the file's last block's length (last byte index)
            pha
            sty blocksize + $01
            dey
            tya
            eor #$ff
            ldx LINKTRACK
            beq :+
            lda NEXTCONTIGUOUSBLOCK
:           sta BLOCKBUFFER81 + $01; block length
            lda #1
            ldx #0
            cpx LINKTRACK; last track: bit 0 set
            rol
            jsr sendblock; send the block over
            inc NEXTCONTIGUOUSBLOCK
            pla; LINKSECTOR
            tay
            pla; LINKTRACK
            bne loadblock

            ; loading is finished

            clc; all ok after loading

filenfound: ; branches here with carry set
illegalts:  ; or illegal t or s

            jsr sendstatus

:           lda CIA_PRB
            and #ATN_IN | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            cmp #ATN_IN | CLK_OUT | CLK_IN            | DATA_IN
            bne :-; wait until host is in idle mode

            lda BLOCKBUFFER81 + $00; offset 0: block index or status byte
            bmi :+; only after successful load
            PREPARE_NEXT_FILE 1581
:           jmp idleloop

.if !DISABLE_WATCHDOG
initcontrl: lda sysirqvbuf + 0
            sta IRQVECTOR_LO
            lda sysirqvbuf + 1
            sta IRQVECTOR_HI
            lda cmdfdfix2; 0 for FD
            beq :+
            jmp RESET_TIMERB
:           lda #.lobyte(CONTROLLERIRQPERIODFD)
            sta VIA_T1C_L
            lda #.hibyte(CONTROLLERIRQPERIODFD)
            sta VIA_T1C_H
            rts
.endif

getdirtrk:
cmdfdfix1 = * + 1
cmdfdfix2 = * + 2
            lda ROMOS_DIRTRACK81
.if DIRTRACK81 <> 40
            cmp #40
            bne :+
            lda #DIRTRACK81
:
.endif
            rts

trackseek:  sta REQUESTEDTRACK
            sty REQUESTEDSECTOR
            tax
            dex
            stx HDRS2 + (2 * BUFFERINDEX)
            INIT_CONTROLLER
            lda #SEEK_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER

            lda DISKCHANGED
            ora diskchangd + 1
            sta diskchangd + 1

.if DISABLE_WATCHDOG
            rts
.else
            ; fall through

initwatchd: ; the i-flag is set here
            lda #.lobyte(uninstall)
            sta IRQVECTOR_LO
            lda #.hibyte(uninstall)
            sta IRQVECTOR_HI
            lda cmdfdfix2; 0 for FD
            beq :+
            lda #$ff
            sta CIA_TB_LO
            sta CIA_TB_HI
:           rts

enablewdog: lda cmdfdfix2; 0 for FD
            beq :+
            ldy #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sty CIA_CRB
            bit CIA_ICR
            ENABLE_WATCHDOG
            rts
:           lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS
            sta VIA_IER; no IRQs from VIA
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA_IER; timer 1 IRQs from VIA
            ldy #$ff
            sty VIA_T1C_H
            ENABLE_WATCHDOG
            rts
.endif; !DISABLE_WATCHDOG

fadeled:    txa
            tay
            beq motrledoff
:           nop
            bit OPC_BIT_ZP
            dey
            bne :-
            pha
            jsr busyledoff
            pla
            tay
:           nop
            bit OPC_BIT_ZP
            iny
            bne :-
            dex
busyledon:  lda #DRIVE_LED
            ora CIA_PRA
            ldy #$ff
            bne store_cia; jmp

motrledoff: lda DRIVESTATE
            ora LEDSTATE
            beq :+

            txa
            pha

            ; fill track cache
            lda REQUESTEDTRACK
            ldy REQUESTEDSECTOR
            jsr getblock

            ; turn off motor
            INIT_CONTROLLER
            lda #MOTOFFI_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER
            lda #DRIVEOFF; $00
            sta DRIVESTATE

            pla
            tax

busyledoff: lda CIA_PRA
            and #.lobyte(~DRIVE_LED); turn off drive led
            ldy #$00
store_cia:  sta CIA_PRA
            sty LEDSTATE
:           rts

getblock:   sta REQUESTEDTRACK
            sty REQUESTEDSECTOR
getblockag: INIT_CONTROLLER
            lda #READ_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER

            lda DISKCHANGED
            ora diskchangd + 1
            sta diskchangd + 1

.if !DISABLE_WATCHDOG
            jsr initwatchd
.endif
            lda JOBCODESTABLE + BUFFERINDEX; FD does not return the error status in the accu
            cmp #OK_DV + 1

            ; the link track is returned last so that the z-flag
            ; is set if this block is the file's last one (see FIND_FILE)
            ldy LINKSECTOR
            ldx LOADEDSECTOR
            lda LINKTRACK
            rts

            ; get custom drive code
getcustom:  ldx SYS_SP
            txs
            ldx #ZP_LAST - ZP_FIRST
:           lda syszpbuf,x
            sta ZP_FIRST,x
            dex
            bpl :-
            INIT_CONTROLLER
            lda #CIA_SET_INTF | FLAG1_IRQ | SERIAL_IRQ | TIMERB_IRQ
            sta CIA_ICR
            bit CIA_ICR

customparm = JOBTRKSCTTABLE
uploadrout = $2000 - customend + dgetbyte; as high up in RAM as possible

            ldx #customend - dgetbyte
:           lda dgetbyte - 1,x
            sta uploadrout - 1,x
            dex
            bne :-
            lda #OPC_STA_ZPXI
            sta uploadrout + getbyterts - dgetbyte

            lda #DATA_OUT
            sta CIA_PRB; set DATA_OUT to signal ready for code upload
            lda #CLK_IN
:           and CIA_PRB; wait for CLK_IN clear
            bne :-
            stx CIA_PRB; clear DATA_OUT

            ldx #5
:           jsr dgetbyte
            sta customparm,x
            dex
            bpl :-
            jmp uploadrout

            ; must not clobber x
dgetbyte:   lda #%10000000
:           pha
:           lda CIA_PRB
            bpl :-
:           cmp CIA_PRB
            beq :-
            lda CIA_PRB
            and #CLK_IN
            cmp #CLK_IN
            pla
            ror
            bcc :---
getbyterts: rts; is changed to sta (zp,x) for custom drive code upload
            .byte customparm + $04 + 1
            inc customparm + $04
            bne :+
            inc customparm + $05
:           dec customparm + $02
            bne dgetbyte
            dec customparm + $03
            bpl dgetbyte
            jmp (customparm); execute custom drive code
customend:

uninstall:  jsr getdirtrk
            jsr trackseek

            ldx LEDSTATE
:           jsr fadeled
            lda LEDSTATE
            bne :-

            ldx SYS_SP
            txs
            ldx #ZP_LAST - ZP_FIRST
:           lda syszpbuf,x
            sta ZP_FIRST,x
            dex
            bpl :-
            INIT_CONTROLLER
            lda #CIA_SET_INTF | FLAG1_IRQ | SERIAL_IRQ | TIMERB_IRQ
            sta CIA_ICR
            bit CIA_ICR

            cli
            rts

            FNAMEHASH 1581

            ; carry: clear = ok, set = load error
sendstatus: lda #$00
            sta blocksize + $01

            lda #diskio::status::FILE_NOT_FOUND
            bcs sendblock
            lda #diskio::status::OK
sendblock:  sta BLOCKBUFFER81 + $00; block index
.if !DISABLE_WATCHDOG
            jsr enablewdog
.endif
            lda #DATA_OUT; clear CLK_OUT, set DATA_OUT as signal of presence
            sta CIA_PRB; block ready signal

            ldx #$20; here, the watchdog timer is polled manually because
                    ; an extra-long time-out period is needed since the computer may
                    ; still be busy decompressing a large chunk of data,
                    ; this is the round counter: $20 * ($ff00 - $0100) = 2,080,768 cycles at 2 MHz is roughly 1 second
            lda cmdfdfix2; 0 for FD
            bne waitready

waitrdyfd:  lda VIA_T1C_H; see if the watchdog barked
            bne :+
            dex          ; if yes, decrease the round counter
.if DISABLE_WATCHDOG
            beq :+
.else
            beq timeout; and trigger watchdog on time-out
           ;ldy #$ff
            sty VIA_T1C_H; reset watchdog time-out
.endif
:           bit CIA_PRB
            bmi waitrdyfd; wait for ATN_IN clear
.if !DISABLE_WATCHDOG
            sty VIA_T1C_H; reset watchdog time-out
.endif
            jmp :++

waitready:  lda CIA_TB_HI; see if the watchdog barked
            bne :+
            dex          ; if yes, decrease the round counter
.if DISABLE_WATCHDOG
            beq :+
.else
            beq timeout; and trigger watchdog on time-out
           ;ldy #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sty CIA_CRB; reset watchdog time-out
.endif
:           bit CIA_PRB
            bmi waitready; wait for ATN_IN clear
.if !DISABLE_WATCHDOG
            sty CIA_CRB; reset watchdog time-out
.endif
timeout:
:           ENABLE_WATCHDOG

            ldy #$00

.if ::PLATFORM = diskio::platform::COMMODORE_128

            bit CIA_ICR
            lda cmdfdfix2; 0 for FD
            beq fdsendblk
            ldx #FSM_BUS_DRIVER_OUTPUT | CLK_OUT
            stx CIA_PRB

    .if USE_ASYNCHRONOUS_BURST_HANDSHAKE

            lda BLOCKBUFFER81,y
sendloop:   sta CIA_SDR; clock out data byte
blocksize:  cpy #$00
            iny
            lda #SERIAL_IRQ
            ldx #FSM_BUS_DRIVER_OUTPUT; clear CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB; reset watchdog time-out
        .else
            nop; need some slack, as CIA sets the flag a little too early
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            bcs bcssendone

            sta CIA_SDR; clock out data byte
            cpy blocksize + 1
            iny
            lda #SERIAL_IRQ
            ldx #FSM_BUS_DRIVER_OUTPUT | CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB; reset watchdog time-out
        .else
            nop; need some slack, as CIA sets the flag a little too early
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            bcc sendloop
bcssendone: bcs senddone; jmp

fdsendblk:  lda #$38
            sta VIA_PRA; set to output
            lda VIA_ACR
            and #.lobyte(~SHIFT_REG_CONTROL)
            ora #SHIFT_OUT_T2
            sta VIA_ACR

            lda BLOCKBUFFER81,y
fdsendloop: sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
            cpy blocksize + 1
            iny
            lda #IRQ_SHIFT_REG
            ldx #FSM_BUS_DRIVER_OUTPUT; clear CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .else
            nop; need some slack
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            bcs senddone
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
            cpy blocksize + 1
            iny
            lda #IRQ_SHIFT_REG
            ldx #FSM_BUS_DRIVER_OUTPUT | CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
        .if !DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .else
            nop; need some slack, as CIA sets the flag a little too early
            nop
        .endif
            stx CIA_PRB; toggle CLK_OUT: signal data sent
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            bcc fdsendloop

    .else ; !USE_ASYNCHRONOUS_BURST_HANDSHAKE

            lda BLOCKBUFFER81,y
            sta CIA_SDR; clock out data byte
sendloop:
blocksize:  cpy #$00
            bcs bcswaittkn
            iny
        .if !DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START 
            sta CIA_CRB; reset watchdog time-out
        .endif
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            sta CIA_SDR; clock out data byte

            cpy blocksize + 1
            bcs waitdtaken
            iny
        .if !DISABLE_WATCHDOG
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB; reset watchdog time-out
        .endif
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            sta CIA_SDR; clock out data byte
            bcc sendloop; jmp

fdsendblk:  lda #$38
            sta VIA_PRA; set to output
            lda VIA_ACR
            and #.lobyte(~SHIFT_REG_CONTROL)
            ora #SHIFT_OUT_T2
            sta VIA_ACR

            lda BLOCKBUFFER81,y
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
fdsendloop: cpy blocksize + 1
bcswaittkn: bcs waittaken
            iny
        .if !DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .endif
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN set = data taken
            bpl :-
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte

            cpy blocksize + 1
            bcs waitdtaken
            iny
        .if !DISABLE_WATCHDOG
            lda #$ff
            sta VIA_T1C_H; reset watchdog time-out
        .endif
            lda BLOCKBUFFER81,y
:           bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi :-
            sta VIA_T2C_H
            eor #$ff
            sta VIA_SR; clock out data byte
            bcc fdsendloop; jmp

waitdtaken: bit CIA_PRB; wait for ATN_IN clear = data taken
            bmi waitdtaken
            bpl senddone; jmp
waittaken:  bit CIA_PRB; wait for ATN_IN set = data taken
            bpl waittaken

    .endif; !USE_ASYNCHRONOUS_BURST_HANDSHAKE

senddone:   lda cmdfdfix2; 0 for FD
            bne :+
            lda #$18
            sta VIA_PRA
:
.else; ::PLATFORM <> diskio::platform::COMMODORE_128

sendloop:
        .if !DISABLE_WATCHDOG
cmdfdfix3 = * + 1
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START ; is changed to VIA access for FD
cmdfdfix4 = * + 1
            sta CIA_CRB                                           ; 2 + 4 - reset watchdog time-out
        .endif
            ldx BLOCKBUFFER81,y                                   ; 4
            lda SENDTABLELO,x                                     ; 4
                                                                  ; = 22 (+6 with watchdog)

:           bit CIA_PRB                                           ; 4
            bmi :-                                                ; 3
            sta CIA_PRB                                           ; 4
            asl                                                   ; 2
            and #.lobyte(~ATNA_ENABLE_OUT)                        ; 2
                                                                  ; = 15

:           bit CIA_PRB                                           ; 4
            bpl :-                                                ; 3
            sta CIA_PRB                                           ; 4
            ldx BLOCKBUFFER81,y                                   ; 4
            lda SENDTABLEHI,x                                     ; 4
                                                                  ; = 19

:           bit CIA_PRB                                           ; 4
            bmi :-                                                ; 3
            sta CIA_PRB                                           ; 4
            asl                                                   ; 2
            and #.lobyte(~ATNA_ENABLE_OUT)                        ; 2
blocksize:  cpy #$00                                              ; 2
            iny                                                   ; 2
                                                                  ; = 19

:           bit CIA_PRB                                           ; 4
            bpl :-                                                ; 3
            sta CIA_PRB                                           ; 4
            bcc sendloop                                          ; 3
                                                                  ; = 75
:           bit CIA_PRB; wait for acknowledgement
            bmi :-     ; of the last data byte

.endif; ::PLATFORM <> diskio::platform::COMMODORE_128

            lda #CLK_OUT; drive busy
            sta CIA_PRB

:           bit CIA_PRB; wait for ATN_IN set,
            bpl :-     ; acknowledgement of the block transfer

            sei; disable watchdog
            rts

drvcodeend81:

DIRBUFFSIZE       = (SENDTABLELO - *) / 4
DIRTRACKS         = *
DIRSECTORS        = DIRTRACKS + DIRBUFFSIZE
FILENAMEHASHVALLO = DIRSECTORS + DIRBUFFSIZE
FILENAMEHASHVALHI = FILENAMEHASHVALLO + DIRBUFFSIZE

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

DIRBUFFSIZE81 = DIRBUFFSIZE
.export DIRBUFFSIZE81

            .assert * <= BLOCKBUFFER81, error, "***** 1581 drive code too large. *****"

dinstall:   sei
            lda #CIA_ATN_IN_INPUT | WRITE_PROTECT_OUTPUT | FSM_BUS_DRIVER_DIRECTION_OUTPUT | ATNA_ENABLE_OUT_OUTPUT | CLK_OUT_OUTPUT | CLK_IN_INPUT | DATA_OUT_OUTPUT | DATA_IN_INPUT
            sta CIA_DDRB
            lda #CLK_OUT
            sta CIA_PRB

            ldx #.lobyte(drvcodebeg81 - $01)

:           lda CIA_PRB; wait for ATN_IN set and DATA_IN clear
            and #ATN_IN | DATA_IN
            cmp #ATN_IN
            bne :-

dgetrout:   inx
            bne :+
            inc dgetputhi
:
            ; must not clobber x
            lda #%10000000
            sta BUFFER
            sta CIA_PRB
:           lda CIA_PRB
:           cmp CIA_PRB
            beq :-
            lda CIA_PRB
            and #CLK_IN
            cmp #CLK_IN
            ror BUFFER
            bcc :--
            lda BUFFER
dgetputhi = * + $02
            sta a:.hibyte(drvcodebeg81 - $01) << 8,x
            cpx #.lobyte(drvcodeend81 - $01)
            bne dgetrout
            dec drvcodebeg81
            bne dgetrout

            jmp dcodinit81

drvprgend81:
            .reloc
