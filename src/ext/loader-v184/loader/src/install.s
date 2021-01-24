
.fileopt comment, "Loader install code portion"
.fileopt compiler, "CA65"
.fileopt author, "Gunnar Ruthenberg"

__NO_LOADER_SYMBOLS_IMPORT = 1
.include "loader.inc"
.include "../version.inc"

.include "cpu.inc"
.include "cia.inc"
.include "vdc.inc"

.include "basic.inc"; for PETSCII_RETURN
.include "kernal.inc"

CBM1581_8  = $a6e9; the '8' in the 1581's ID string
FD2K4K_F   = $fea4; the 'f' in the FD 2000/4000's ID string


.include "hal/hal.inc"

.include "drives/drivecode-common.inc"

.importzp BLOCKDESTLO


.if ONLY_1541_AND_COMPATIBLE = 0
.import c1570fix0
.import c1570fix1

.import cmdfdfix0
.import cmdfdfix1
.import cmdfdfix2
    .if !DISABLE_WATCHDOG
.import cmdfdfix3
.import cmdfdfix4
        .if ::PLATFORM = diskio::platform::COMMODORE_128
.import cmdfdfix5
.import cmdfdfix6
        .endif ; ::PLATFORM = diskio::platform::COMMODORE_128
    .endif; !DISABLE_WATCHDOG
.endif; ONLY_1541_AND_COMPATIBLE = 0


USE_GENERIC_DRIVE = 0


.macro itoa4 value
            .if (value & $0f > 9)
                .byte (value & $0f) + 'a' - 10
            .else
                .byte (value & $0f) + '0'
            .endif
.endmacro

.macro itoa1 value
            itoa4 value <> 0
.endmacro

.macro itoa8 value
            itoa4 value >> 4
            itoa4 value & $0f
.endmacro

.macro itoa16 value
            itoa8 value >> 8
            itoa8 value & $ff
.endmacro


.segment "HEADER"
.segment "EXTZP"; not used otherwise, the EXTZP segment is not
                ; optional in the o65 built-in ld65 config

.segment "DISKIO_INSTALL"

.ifdef INSTADDR
            .org INSTADDR - 2
            .word * + 2; load address
.endif

.export install

            ; Install the loader

            ; in:  nothing
            ; out: c - set on error
            ;      a - status
            ;      x - drive type (one of diskio::drivetype)
            ;      y - if status is diskio::status::OK, zp address of version string address
install:    jmp doinstall

            ; unfortunately, scopes must be defined before using them,
            ; this is why the actual install code is moved to after the drive code
.scope cbm1541
drivecode41:
            .include "drives/drivecode1541.s"

            .export CHECKLOADREQUEST41 : zeropage
            .export NUMFILES41         : zeropage
            .export REQUESTEDSECTOR41  : zeropage
            .export TRACKLINKTABLE41   : zeropage

            .export topofstack41       : absolute
            .export idleloop41         : absolute
            .export BLOCKBUFFER41      : absolute
.endscope

    .if ONLY_1541_AND_COMPATIBLE = 0
.scope cbm1571
drivecode71:
            .include "drives/drivecode1571.s"

            .export NUMFILES71         : zeropage
            .export REQUESTEDSECTOR71  : zeropage
            .export TRACKLINKTABLE71   : zeropage

            .export topofstack71       : absolute
            .export idleloop71         : absolute
            .export collswit71         : absolute
            .export BLOCKBUFFER71      : absolute
.endscope

.scope cbm1581
drivecode81:
            .include "drives/drivecode1581.s"

            .export dcodinit81         : absolute
            .export BLOCKBUFFER81      : absolute
.endscope
    .endif; ONLY_1541_AND_COMPATIBLE = 0

doinstall:  lda #.lobyte(version)
            sta BLOCKDESTLO + 0
            lda #.hibyte(version)
            sta BLOCKDESTLO + 1

            BRANCH_IF_NOT_INSTALLED :+
            jmp isinstalld

:           php; I flag buffer

            jsr CLALL

.if PLATFORM = diskio::platform::COMMODORE_128
            ; set data and filename banks to current program bank
            lda MMU_CR
            asl
            rol
            rol
            and #$03; BA
            tax     ; FNBANK
            jsr SETBANK
.endif; PLATFORM = diskio::platform::COMMODORE_128

            ; try the drive as denoted by FA (current drive) first
            lda FA
            cmp #MIN_DEVICE_NO
            bcc :+
            cmp #MAX_DEVICE_NO + 1
            bcc :++
:           lda #MIN_DEVICE_NO; FA does not contain a drive address (MIN_DEVICE_NO..MAX_DEVICE_NO), try MIN_DEVICE_NO first
:
            ; find first available drive,
            ; this is done via the high-level open/read/close routines,
            ; so non-serial bus devices will also respond
            sta FA

find1stdrv: pha; buffer active drive device number
            lda #$00
            sta STATUS
            lda #drvchkone - drvchkon
            ldx #.lobyte(drvchkon)
            ldy #.hibyte(drvchkon)
            jsr fileopen
            bcc drivefound; drive present

            cmp #OPEN_DEVICENOTPRESENT
            beq :+
            tax
            pla; buffered active drive device number
            lda #diskio::status::GENERIC_KERNAL_ERROR
            bne installerr; jmp

:           ; drive not present, try next address
            lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCH
            ldx FA
            inx
            cpx #MAX_DEVICE_NO + 1
            bne :+
            ldx #MIN_DEVICE_NO
:           stx FA
            pla; buffered active drive device number
            cmp FA
            bne find1stdrv

devnotpres: lda #diskio::status::DEVICE_NOT_PRESENT
            ldx #diskio::drivetype::DEVICE_NONE
installerr: ldy #BLOCKDESTLO
            plp; I flag restore
            sec
            rts

drivefound: ldx #COMMAND_ERROR_CHANNEL
            jsr CHKIN
            jsr BASIN
            lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCH

            pla; buffered active drive device number
            sta FA

            ; read error channel on active drive to stop potentially blinking error LED
            lda #0
            tax
            tay
            jsr fileopen
            bcs devnotpres
            ldx #COMMAND_ERROR_CHANNEL
            jsr CHKIN
:           jsr READSS
            bne :+
            jsr BASIN
            jmp :-
:           lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCH

.if ONLY_1541_AND_COMPATIBLE = 0
    .if USE_GENERIC_DRIVE
            jmp usegeneric
    .endif

            jsr chkdrvcode
            bne usegeneric

            ; drive allows code upload and execution,
            ; check which model the drive is and upload corresponding drive code

            ; check if running on a 1541/70/71 compatible drive
            lda #.lobyte($e5c6)
            ldx #.hibyte($e5c6)
            jsr memreadbyt

            cmp #'4'
            beq is1541
            cmp #'7'
            bne :+
            jmp is157x
:
            ; neither 1541 nor 157x

            ; try FD2000/FD4000
            lda #.lobyte(FD2K4K_F)
            ldx #.hibyte(FD2K4K_F)
            jsr memreadbyt
            cmp #'f'
            bne check1581
            lda #OPC_BIT_ABS
            sta cmdfdfix0 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.lobyte($54); DIRTRACKFD
            sta cmdfdfix1 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.hibyte($54); DIRTRACKFD
            sta cmdfdfix2 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.lobyte($fef0)
            ldx #.hibyte($fef0)
            jsr memreadbyt
            ldy #diskio::drivetype::DRIVE_CMD_FD_2000
            cmp #'4'
            bne isfd2000
            iny; diskio::drivetype::DRIVE_CMD_FD_4000
isfd2000:
    .if !DISABLE_WATCHDOG
            lda #$ff
            ldx #.lobyte($1c05)
    .endif
            jmp iscmdfd

            ; check if 1581
check1581:  lda #.lobyte(CBM1581_8)
            ldx #.hibyte(CBM1581_8)
            jsr memreadbyt
            ldy #diskio::drivetype::DRIVE_1581
            cmp #'8'
            bne usegeneric
            jmp is1581

usegeneric: ; no compatible drive found
            lda #diskio::status::DEVICE_INCOMPATIBLE
            jmp nodrvcode

            ; select appropriate drive code

is1541:     ; find out if 1541, 1541-C or 1541-II
            lda #.lobyte($c002)
            ldx #.hibyte($c002)
            jsr memreadbyt
            cmp #'c'
            beq chk1541ii; branch if 'c' at $c002 (from 'COPYRIGHT' etc.)
            ; find out if 1541 or 1541-C
            lda #.lobyte($eaa3)
            ldx #.hibyte($eaa3)
            jsr memreadbyt
            ldy #diskio::drivetype::DRIVE_1541
            cmp #$ff
            beq check1541u
            ; 1541-C: no $ff at $eaa3 (but likely $fe, data direction for track 0 sensor bit)
            ldy #diskio::drivetype::DRIVE_1541_C
            beq check1541u
chk1541ii:  lda #.lobyte($e5b7)
            ldx #.hibyte($e5b7)
            jsr memreadbyt
            tax
            lda #$ff
            cpx #'c' | $80; 'CBM DOS' etc.
            bne :+; treat as 1541-II if no match, so as not to detect JiffyDOS, SpeedDOS etc. as 1541-C but 1541-II instead
            ; find out if 1541-C or 1541-II
            lda #.lobyte($eaa3)
            ldx #.hibyte($eaa3)
            jsr memreadbyt
:           ldy #diskio::drivetype::DRIVE_1541_C
            cmp #$ff
            bne check1541u; 1541-C: no $ff at $eaa3 (but likely $fe, data direction for track 0 sensor bit)
            iny; diskio::drivetype::DRIVE_1541_II: $ff at $eaa3
            ; check for discrete drive logics vs gate array
            sty drivetype
            jsr drvlistn
            ldx #$00
:           lda dchk1541ii,x
            jsr CIOUT
            inx
            cpx #dch1541iie - dchk1541ii
            bne :-
            jsr UNLSN
            lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
            beq :+; branch if 1541-II
            ldy #diskio::drivetype::DRIVE_1541
check1541u: sty drivetype
:           jsr drvlistn
            ldx #$00
:           lda drvch1541u,x
            jsr CIOUT
            inx
            cpx #drvchkued - drvch1541u
            bne :-
            jsr UNLSN
            lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
            bmi :+; branch if 1541U
            ldy drivetype
            SKIPWORD
:           ldy #diskio::drivetype::DRIVE_1541U
            jmp selectdcod

            ; find out if 1570 or 1571
is157x:     cpx #'1' | $80; 71
            lda #OPC_BIT_ZP; one-sided seek boundaries
            ldx #OPC_BIT_ABS; no VIA2_PRA writes to switch sides
            ldy #diskio::drivetype::DRIVE_1570
            bcc is1570
            ; 1571 or 1571CR

            lda #.lobyte($e5c2)
            ldx #.hibyte($e5c2)
            jsr memreadbyt
            cmp #'1'; 3.1
            lda #OPC_BCS
            ldx #OPC_STA_ABS
            ldy #diskio::drivetype::DRIVE_1571
            bcc :+
            iny; diskio::drivetype::DRIVE_1571CR
:
is1570:     sta c1570fix0 - cbm1571::drvcodebeg71 + cbm1571::drivecode71
            stx c1570fix1 - cbm1571::drvcodebeg71 + cbm1571::drivecode71

            ; fall through

is1581:     lda #OPC_JMP_ABS
            sta cmdfdfix0 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.lobyte($022b); DIRTRACK81
            sta cmdfdfix1 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.hibyte($022b); DIRTRACK81
            sta cmdfdfix2 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
    .if (::PLATFORM <> diskio::platform::COMMODORE_128) & (!DISABLE_WATCHDOG)
            lda #COUNT_TA_UNDF | FORCE_LOAD | ONE_SHOT | TIMER_START
            ldx #.lobyte(CIA_CRB)
iscmdfd:    sta cmdfdfix3 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            stx cmdfdfix4 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
    .else
iscmdfd:
    .endif

selectdcod: sty drivetype
            tya
            lsr
            lsr
            lsr
            lsr
            tax
            lda dcodeselt0,x
            sta dcodesel0
            lda dcodeselt1,x
            sta dcodesel1
            lda dcodeselt2,x
            sta dcodesel2
            lda dcodeselt3,x
            sta dcodesel3
            lda dcodeselt4,x
            sta dcodesel4
            lda dcodeselt5,x
            sta dcodesel5
            lda dcodeselt6,x
            sta dcodesel6
            lda dcodeselt7,x
            sta dcodesel7
            lda dcodeselt8,x
            sta dcodesel8

.else; ONLY_1541_AND_COMPATIBLE

            ; check if 1541U
            jsr drvlistn
            ldx #$00
:           lda drvch1541u,x
            jsr CIOUT
            inx
            cpx #drvchkued - drvch1541u
            bne :-
            jsr UNLSN
            lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
            bmi :+; branch if 1541U
            lda #diskio::drivetype::DRIVE_1541
            SKIPWORD
:           lda #diskio::drivetype::DRIVE_1541U
            sta drivetype

            lda #.lobyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            sta dcodesel0
            lda #.hibyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            sta dcodesel1
            lda #.lobyte(cbm1541::drvprgend41 - cbm1541::drvcodeend41 + cbm1541::TRAMPOLINEOFFSET)
            sta dcodesel2
            lda #.lobyte(cbm1541::drivecode41)
            sta dcodesel3
            lda #.hibyte(cbm1541::drivecode41)
            sta dcodesel4
            lda #.hibyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            sta dcodesel5
            lda #.lobyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            sta dcodesel6
            lda #.hibyte(cbm1541::dinstall)
            sta dcodesel7
            lda #.lobyte(cbm1541::dinstall)
            sta dcodesel8

.endif; ONLY_1541_AND_COMPATIBLE

            ; check if there is more than 1 drive on the serial bus,
            ; upload silencing routines to the passive drives in order
            ; to make sure the 2bit+ATN protocol can work alright,
            ; detection is done via the low-level serial bus routines,
            ; so non-serial bus devices won't respond
            ; (1551 on Plus/4 does respond, though, so a little extra
            ; treatment is done through the drive disturbance HAL macros)

            lda FA; active drive
            pha
            ldx #MIN_DEVICE_NO
checkbus:   stx FA
            pla
            pha
            cmp FA
            beq jmpnodrive

            lda #$00
            sta STATUS
            PREPARE_DRIVE_DISTURBANCE_VALIDATION
            jsr drvlistn
            BRANCH_IF_DRIVE_DOES_NOT_DISTURB_SERIAL_BUS jmpnodrive
            jsr READSS
            bpl :+
jmpnodrive: jmp nodrive

            ; more than 1 drive on the bus or generic serial devices present
:           jsr UNLSN

            ; upload and execute silencing routine
.if ONLY_1541_AND_COMPATIBLE = 0
            jsr chkdrvcode
            beq :+
            pla
            sta FA
            lda #diskio::status::TOO_MANY_DEVICES
            jmp nodrvcode

:           cpx #'8'
            beq drvsilnc81
            cpy #'f'
            bne :+
            jmp drvsilncfd
:
.endif; ONLY_1541_AND_COMPATIBLE = 0

            jsr drvlistn
            ldx #$00
:           lda drvsilencc,x
            jsr CIOUT
            inx
            cpx #atnfallbck - drvsilencc
            bne :-
            jsr drvrelistn
            ldx #$00
:           lda atnfallbck,x
            jsr CIOUT
            inx
            cpx #atnlo - atnfallbck
            bne :-
            jsr drvrelistn
            ldx #$00
:           lda atnlo,x
            jsr CIOUT
            inx
            cpx #atnhi - atnlo
            bne :-
            jsr drvrelistn
            ldx #$00
:           lda atnhi,x
            jsr CIOUT
            inx
            cpx #atnhiend - atnhi
            bne :-
.if ONLY_1541_AND_COMPATIBLE = 0
            jsr UNLSN
            lda #.lobyte($e5c6)
            ldx #.hibyte($e5c6)
            jsr memreadbyt
            cmp #'7'
            bne :++
            jsr drvlistn
            ldx #$00
:           lda drvslnc71,x
            jsr CIOUT
            inx
            cpx #drvslnc71e - drvslnc71
            bne :-
            beq slncunlstn; jmp
:           jsr drvlistn
.else; ONLY_1541_AND_COMPATIBLE = 0
            jsr drvrelistn
.endif; ONLY_1541_AND_COMPATIBLE = 0
            ldx #$00
:           lda drvsilence,x
            jsr CIOUT
            inx
            cpx #drvsilnced - drvsilence
            bne :-

.if ONLY_1541_AND_COMPATIBLE = 0
            beq slncunlstn; jmp

drvsilnc81: jsr drvlistn
            ldx #$00
:           lda drvslnc81,x
            jsr CIOUT
            inx
            cpx #drvslnc81e - drvslnc81
            bne :-
            beq slncunlstn

drvsilncfd: jsr drvlistn
            ldx #$00
:           lda drvslncfd,x
            jsr CIOUT
            inx
            cpx #drvslncfde - drvslncfd
            bne :-
.endif; ONLY_1541_AND_COMPATIBLE = 0

slncunlstn: jsr UNLSN
            jmp nodrive

nodrvcode:  pha; error code

            CHECK_AND_BRANCH_IF_DRIVE_PARALLEL drivepar

.if LOAD_VIA_KERNAL_FALLBACK
            ; quicker head stepping
            jsr drvlistn
            ldx #6
:           lda drvfaststp,x
            jsr CIOUT
            dex
            bpl :-
            jsr UNLSN

            lda #.lobyte($e5c6)
            ldx #.hibyte($e5c6)
            jsr memreadbyt
            cpx #'1' | $80; 71
            bne :++
            jsr drvlistn
            ldx #4
:           lda twosided,x
            jsr CIOUT
            dex
            bpl :-
            jsr drvrelistn; make sure the drive is done
            jsr UNLSN     ; when leaving the init routine
:
.endif; LOAD_VIA_KERNAL_FALLBACK

.if PLATFORM = diskio::platform::COMMODORE_128
            lda burstflag
            beq :+
            ldx #diskio::drivetype::DRIVE_GENERIC_BURST
            SKIPWORD
:
.endif
            ldx #diskio::drivetype::DRIVE_GENERIC_SERIAL
            SKIPWORD
drivepar:   ldx #diskio::drivetype::DRIVE_GENERIC_PARALLEL
            pla; error code
            plp; I flag restore
            ldy #BLOCKDESTLO
.if LOAD_VIA_KERNAL_FALLBACK
            clc; this is not to be regarded as an error
.else
            sec
.endif
            rts

nodrive:    jsr UNLSN
            ldx FA
            inx
            cpx #MAX_DEVICE_NO + 1
            beq :+
            jmp checkbus
:           pla
            sta FA; active drive

            ; install drive-side loader code
.if PLATFORM = diskio::platform::COMMODORE_128
            lda #$00
            sta SERIAL
.endif
            jsr drvlistn
            ldx #$00
install1:   sec
            stx :+ + $01
dcodesel2 = * + $01
            lda #$00
:           sbc #$00
            cmp #$23
            bcc :+
            lda #$23
:           sta drvrutmw

            ldy #$06
:           lda drvrutmw - 1,y
            jsr CIOUT
            dey
            bne :-

dcodesel0 = * + $01
dcodesel1 = * + $02
:           lda a:$00,x
            jsr CIOUT
            inx
            cpx dcodesel2
            beq :+
            iny
            cpy #$23
            bne :-
            jsr drvrelistn
            clc
            lda #$23
            adc drvrutmw + $02
            sta drvrutmw + $02
            bcc install1
            inc drvrutmw + $01
            bne install1

:           jsr drvrelistn
            ldx #4
:           lda droutrun,x
            jsr CIOUT
            dex
            bpl :-
            jsr UNLSN

            INIT_CLEAR_ATN_OUT_CLEAR_CLK_OUT_CLEAR_DATA_OUT

:           SET_FLAGS_N_DATA_V_CLK
            bvs :-

            CLEAR

:           SET_FLAGS_N_DATA_V_CLK
            bvc :-

dcodesel3 = * + 1
            ldy #$00
dcodesel4 = * + 2
fastinst:   lda a:$00,y
            SENDBYTE
            iny
            bne :+
            inc fastinst + 2
:           cpy dcodesel0
            bne fastinst
            lda fastinst + 2
            cmp dcodesel1
            bne fastinst

            INSTALL_IDLE

.if PLATFORM = diskio::platform::COMMODORE_128
burstflag = * + 1
            lda #$00
    .if ONLY_1541_AND_COMPATIBLE = 0
            bne burst
    .endif
            DISABLE_BURST_MODE
burst:
.endif; PLATFORM = diskio::platform::COMMODORE_128

            ; if 1541U is detected, it runs on buggy firmware
            lda drivetype
            cmp #diskio::drivetype::DRIVE_1541U
            beq emubugwarn

noemubugwr: plp; I flag restore

isinstalld: lda #diskio::status::OK
drivetype = * + $01
            ldx #$00
            ldy #BLOCKDESTLO
            clc
            rts

emubugwarn: lda BORDERCOLOUR
            pha
            lda BGCOLOUR
            pha
            lda #COLOUR_RED
            sta BORDERCOLOUR
            sta BGCOLOUR
.if PLATFORM = diskio::platform::COMMODORE_16
            lda TED_CTRL1
            pha
            lda #TEXT_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3; $1b
            sta TED_CTRL1
            lda TED_CTRL2
            pha
            and #.lobyte(~MULTICOLOUR_MODE)
            ora #COLUMNS_40
            sta TED_CTRL2
            lda TED_BITMAP_ADDR
            pha
            ora #CHARSET_BITMAP_IN_ROM
            sta TED_BITMAP_ADDR
            lda TED_CHARGEN_ADDR
            pha
            and #.lobyte(~CHARGEN_ADDR_MASK)
            ora #MAKE_CHARGEN_ADDR(CHARSET_ADDR_UPPERLOWER)
            sta TED_CHARGEN_ADDR
            lda TED_SCREEN_ADDR
            pha
            and #.lobyte(~SCREEN_ADDR_MASK)
            ora #MAKE_SCREEN_ADDR($0c00)
            sta TED_SCREEN_ADDR
            lda PALETTE
            pha
            lda #PALETTE_DEFAULT
            sta PALETTE
.else; PLATFORM <> diskio::platform::COMMODORE_16
    .if PLATFORM = diskio::platform::COMMODORE_128
            lda #FG_BG
            sta VDC_CR
:           bit VDC_SR; wait until update ready
            bpl :-
            lda VDC_DR
            pha
            lda #RED
            sta VDC_DR
    .endif ; PLATFORM = diskio::platform::COMMODORE_128
            lda VIC2_CTRL1
            and #.lobyte(~RASTERLINE_BIT8)
            pha
            lda VIC2_CTRL2
            pha
            lda VIC2_ADDR
            pha
            lda CIA2_PRA
            and #VIC2_BANK
            pha
            lda #TEXT_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3; $1b
            sta VIC2_CTRL1
            lda #SINGLECOLOUR_MODE | COLUMNS_40 | SCROLLX_0; $08
            sta VIC2_CTRL2
            lda #VIC2_MAKE_ADDR($0400, CHARSET_ADDR_UPPERLOWER)
            sta VIC2_ADDR
            lda #(VIC2_MAKE_BANK $0400) & VIC2_BANK
            sta CIA2_PRA
.endif; PLATFORM <> diskio::platform::COMMODORE_16
            lda DFLTO
            pha
            lda COLOR
            pha
            lda #DEVICE_SCREEN
            sta DFLTO
            ldx #0
:           lda warnemubug,x
            beq :+
            jsr BSOUT
    .if PLATFORM = diskio::platform::COMMODORE_16
            lda #BLINK
            ora COLOR
            sta COLOR
    .endif; PLATFORM = diskio::platform::COMMODORE_16
            inx
            bne :-
:           pla
            sta COLOR
            pla
            sta DFLTO
            lda #$08
:           dex
            bne :-
            dey
            bne :-
            sbc #1
            bne :-
.if PLATFORM = diskio::platform::COMMODORE_16
            pla
            sta PALETTE
            pla
            sta TED_SCREEN_ADDR
            pla
            sta TED_CHARGEN_ADDR
            pla
            sta TED_BITMAP_ADDR
            pla
            sta TED_CTRL2
            pla
            sta TED_CTRL1
.else; PLATFORM <> diskio::platform::COMMODORE_16
            pla
            sta CIA2_PRA
            pla
    .if PLATFORM = diskio::platform::COMMODORE_128
            sta VM1
    .endif ; PLATFORM = diskio::platform::COMMODORE_128
            sta VIC2_ADDR
            pla
            sta VIC2_CTRL2
            pla
            sta VIC2_CTRL1
    .if PLATFORM = diskio::platform::COMMODORE_128
            lda #FG_BG
            sta VDC_CR
:           bit VDC_SR; wait until update ready
            bpl :-
            pla
            sta VDC_DR
    .endif ; PLATFORM = diskio::platform::COMMODORE_128
.endif; PLATFORM <> diskio::platform::COMMODORE_16
            pla
            sta BGCOLOUR
            pla
            sta BORDERCOLOUR
            jmp noemubugwr

fileopen:   jsr SETNAM
            lda #COMMAND_ERROR_CHANNEL
            ldx FA
            tay
            jsr SETLFS
            jmp OPEN

drvrelistn: jsr UNLSN
drvlistn:   lda FA
            jsr LISTN
.if PLATFORM = diskio::platform::COMMODORE_128
            lda SERIAL
            sta burstflag
.endif
            lda #SA_OPENCHANNEL | COMMAND_ERROR_CHANNEL
            jmp SECND

memreadbyt: sta drvchkmr + $03
            stx drvchkmr + $04
            lda #drvchkmred - drvchkmr
            ldx #.lobyte(drvchkmr)
            ldy #.hibyte(drvchkmr)
            jsr SETNAM
            lda #COMMAND_ERROR_CHANNEL
            ldx FA
            tay
            jsr SETLFS
            jsr OPEN
            bcc :+
kernalerr:  pla
            pla
            plp; I flag restore
            lda #diskio::status::GENERIC_KERNAL_ERROR
            ldx #diskio::drivetype::DEVICE_UNKNOWN
            ldy #BLOCKDESTLO
            sec
            rts
:           ldx #COMMAND_ERROR_CHANNEL
            jsr CHKIN
            jsr BASIN
            pha
            jsr BASIN
            pha
            jsr BASIN
            pha
            lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCH
            pla
            tay
            pla
            tax
            pla
            clc
            rts

drvchkmr:   .byte "m-r", $00, $00, $03; read forward
drvchkmred:

.if ONLY_1541_AND_COMPATIBLE = 0

dchk1541ii: .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
            lda $1c0c
            ldx #$ec; disable byte sync: clear SOE
            stx $1c0c
            ldy #$01
            bit $1c01
            ldx $1c01
:           cpx $1c01; with disabled byte sync, $1c01 does not change on a 1541-II
            bne :+
            iny
            bne :-
:           sty $0300; if not 0, not a 1541-II (discrete drive logics rather than gate array)
            sta $1c0c
            cli
            rts
dch1541iie:
            .assert (* - dchk1541ii) <= 41, error, "dchk1541ii too big"

.endif ; ONLY_1541_AND_COMPATIBLE = 0

            ; may be executed on 1570/71 with ONLY_1541_AND_COMPATIBLE
drvch1541u: .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
           ;ldy #$00
            dey
            sty $0300
            sty $1803; set all port pins as outputs
            lda #$a4; bit 0 may be forced to GND (1541-II) or connected to track 0 sensor (1541-C, normally 0 = not on track 0)
            sta $1801
            cmp $1801
            bne is1541u
            anc #$8a; and #imm, but no asl/rol, bit 7 of result goes to carry
            beq is1541u
            bcc is1541u
            tya
            arr #$7f; bit 6 of result goes to carry
            ror $0300
is1541u:    lda #$66; 1570/71 data directions
            sta $1803
            ; no cli
            rts
drvchkued:
            .assert (* - drvch1541u) <= 41, error, "drvch1541u too big"

drvsilence: .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
            lda #$ff
            sta $1803; set all $1801 port pins as outputs
            ldx #.hibyte($0400); CLK_IN, ATNA_OUT cleared, CLK_OUT low, DATA_OUT low
            stx $1801
            ldy #$7f
            sty $1802; set only ATN_IN as input
            sty $180e; no IRQs from VIA1
            sty $1c0e; no IRQs from VIA2
            ldy #$c0 ; timer 1 IRQs from VIA2
            sty $1c0e
            ldy #$d0 ; JOBCODE_EXECUTE
            jmp $0440; waitactive
drvsilnced:
            .assert (* - drvsilence) <= 41, error, "drvsilence too big"

.if ONLY_1541_AND_COMPATIBLE = 0
drvslnc71:  .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
            lda #$ff
            sta $1803; set all $1801 port pins as outputs
            ldx #.hibyte($0400); CLK_IN, ATNA_OUT cleared, CLK_OUT low, DATA_OUT low
            stx $1801
            ldy #$7f
            sty $1802; set only ATN_IN as input
            sty $180e; no IRQs from VIA1
            sty $1c0e; no IRQs from VIA2
            sty $400d; no IRQs from CIA/MOS5710
            ldy #$c0 ; timer 1 IRQs from VIA2
            sty $1c0e
            ldy #$d0 ; JOBCODE_EXECUTE
            jmp $0440; waitactive
drvslnc71e:
            .assert (* - drvslnc71) <= 41, error, "drvslnc71 too big"
.endif ; ONLY_1541_AND_COMPATIBLE = 0

drvsilencc: .byte "m-w", .lobyte($0440), .hibyte($0440), atnfallbck - (* + 1); read forward
waitactive: sty $01 ; JOBCODE0400
            sta $1c06; VIA2 timer 1 latch lo
            ldy #$90 ; ATNA_OUT set, CLK_OUT low, DATA_OUT low
:          ;ldx #.hibyte($0400); CLK_IN, ATNA_OUT cleared, CLK_OUT low, DATA_OUT low
            stx $1800; clear ATNA_OUT, set CLK_IN
:           bit $1800
            bpl :-
           ;ldy #$90 ; ATNA_OUT set, CLK_OUT low, DATA_OUT low
            sty $1800; set ATNA_OUT, clear CLK_IN
           ;lda #$ff
            sta $1c05; timer 1 hi
:           bit $1800
            bpl :---
            bit $1c05; timer 1 hi
            bne :-
            cpx $1801; check if fast silencing using jmp ($1800) is possible
            .assert (* - drvsilencc) <= 41, error, "waitactive too big"
atnfallbck: .byte "m-w", .lobyte($0440 + atnfallbck - waitactive), .hibyte($0440 + atnfallbck - waitactive), atnlo - (* + 1); read forward
            bne * + 7
            cpy $1800
            beq * + 32; silncentry: jmp ($1800)
            ; slower fallback silence routine, this is required on at least 1541u, which does not implement VIA1 port A correctly,
            ; it may also be required for drives modded with a parallel connection
            inc $1803; set all $1801 port pins to input
:           bit $1800
            bmi :-
            stx $1800; clear ATNA_OUT
           ;lda #$ff
            sta $1c05; timer 1 hi
            cli
:           bit $1800
            bpl :-
            sty $1800; set ATNA_OUT
            sei
            bmi :--
            .assert (* - atnfallbck) <= 41, error, "atnfallbck too big"
atnlo:      .byte "m-w", .lobyte($0400), .hibyte($0400), atnhi - (* + 1); read forward
            jmp (RESET_VECTOR)
            .byte 0
            cli; $0404, $04 = CLK_IN
            jmp ($1800); jump to $0404 or $0484
            .byte 0, 0, 0, 0, 0, 0, 0, 0
            ; $0410, $10 = ATNA_OUT
            ; ATN_IN is clear, but ATNA_OUT is set: clear ATNA_OUT and reset timer
            stx $1800; x = $04 = CLK_IN
            sta $1c05; timer 1 hi
            jmp ($1800); jump to $0404 or $0484
            .assert (* - atnlo) <= 41, error, "atnlo too big"
atnhi:      .byte "m-w", .lobyte($0484), .hibyte($0484), atnhiend - (* + 1); read forward
            ; $0484, $84 = ATN_IN | CLK_IN
            ; ATN_IN is set, but ATNA_OUT is cleared: set ATNA_OUT
            sty $1800; y = $90 = ATN_IN | ATNA_OUT
silncentry: jmp ($1800); jump to $0410 or $0490
            .byte 0, 0, 0, 0, 0, 0
            sei; $0490, $90 = ATN_IN | ATNA_OUT
            jmp ($1800); jump to $0410 or $0490
atnhiend:
            .assert (* - atnhi) <= 41, error, "atnhi too big"

.if ONLY_1541_AND_COMPATIBLE = 0
drvslnc81:  .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
           ;ldy #$00
            sty CIA_PRB
            dey
            sty CIA_TB_HI
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
slnc81loop: sta CIA_CRB
:           bit CIA_PRB
slnc81sens: bpl slnc81loop; first, wait for a long period of ATN_IN set
            ldx CIA_TB_HI
            bne :-
            ldx #OPC_BMI
            cpx slnc81sens - drvslnc81 + $0200
            stx slnc81sens - drvslnc81 + $0200
            bne slnc81loop
            jmp (RESET_VECTOR)
drvslnc81e:
            .assert (* - drvslnc81) <= 41, error, "drvslnc81 too big"

drvslncfd:  .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
            ldx #$00
            stx $4001
            dex
slncfdloop: stx $4005
:           bit $4001
slncfdsens: bpl slncfdloop
            lda $4005
            bne :-
            lda #OPC_BMI
            cmp slncfdsens - drvslncfd + $0200
            sta slncfdsens - drvslncfd + $0200
            bne slncfdloop
            jmp (RESET_VECTOR)
drvslncfde:
            .assert (* - drvslncfd) <= 41, error, "drvslncfd too big"

            ; check if drive allows code upload and execution
chkdrvcode: lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
            eor #$ff
            sta drvchkval + 1
            jsr drvlistn
            ldx #$00
:           lda drvchkme,x
            jsr CIOUT
            inx
            cpx #drvchkmed - drvchkme
            bne :-
            jsr UNLSN
            lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
drvchkval:  cmp #$00
            rts

drvchkme:   .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            lda #$ff
            eor $0300
            sta $0300
            lda CBM1581_8
            sta $0301
            lda FD2K4K_F
            sta $0302
            rts
drvchkmed:
            .assert (* - drvchkme) <= 41, error, "drvchkme too big"

    .if LOAD_VIA_KERNAL_FALLBACK
drvfaststp: .byte MINSTEPSPEED, $01, .hibyte($1c07), .lobyte($1c07), "w-m"; read backward
twosided:   .byte "1m>0u"; read backward, enable 1571 mode to read two-sided disks
    .endif; LOAD_VIA_KERNAL_FALLBACK

dcodeselt0: .byte .lobyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            .byte .lobyte(cbm1571::drvcodeend71 - cbm1571::drvcodebeg71 + cbm1571::drivecode71)
            .byte .lobyte(cbm1581::drvcodeend81 - cbm1581::drvcodebeg81 + cbm1581::drivecode81)
dcodeselt1: .byte .hibyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            .byte .hibyte(cbm1571::drvcodeend71 - cbm1571::drvcodebeg71 + cbm1571::drivecode71)
            .byte .hibyte(cbm1581::drvcodeend81 - cbm1581::drvcodebeg81 + cbm1581::drivecode81)
dcodeselt2: .byte .lobyte(cbm1541::drvprgend41 - cbm1541::drvcodeend41 + cbm1541::TRAMPOLINEOFFSET)
            .byte .lobyte(cbm1571::drvprgend71 - cbm1571::drvcodeend71 + cbm1571::TRAMPOLINEOFFSET)
            .byte .lobyte(cbm1581::drvprgend81 - cbm1581::drvcodeend81)
dcodeselt3: .byte .lobyte(cbm1541::drivecode41)
            .byte .lobyte(cbm1571::drivecode71)
            .byte .lobyte(cbm1581::drivecode81)
dcodeselt4: .byte .hibyte(cbm1541::drivecode41)
            .byte .hibyte(cbm1571::drivecode71)
            .byte .hibyte(cbm1581::drivecode81)
dcodeselt5: .byte .hibyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            .byte .hibyte(cbm1571::drvcodeend71 - cbm1571::TRAMPOLINEOFFSET)
            .byte .hibyte(cbm1581::drvcodeend81)
dcodeselt6: .byte .lobyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            .byte .lobyte(cbm1571::drvcodeend71 - cbm1571::TRAMPOLINEOFFSET)
            .byte .lobyte(cbm1581::drvcodeend81)
dcodeselt7: .byte .hibyte(cbm1541::dinstall)
            .byte .hibyte(cbm1571::dinstall)
            .byte .hibyte(cbm1581::dinstall)
dcodeselt8: .byte .lobyte(cbm1541::dinstall)
            .byte .lobyte(cbm1571::dinstall)
            .byte .lobyte(cbm1581::dinstall)

.endif; ONLY_1541_AND_COMPATIBLE = 0

drvchkon:   .byte "m-r", .lobyte($0300), .hibyte($0300)
drvchkone:

dcodesel5 = * + $01
dcodesel6 = * + $02
drvrutmw:   .byte $23, $00, $00, "w-m"; read backward
dcodesel7 = * + $00
dcodesel8 = * + $01
droutrun:   .byte $00, $00, "e-m"; read backward

warnemubug: .byte PETSCII_LO_UP_CASE
.if PLATFORM = diskio::platform::COMMODORE_128
            .byte PETSCII_BLINK
.endif
.if PLATFORM = diskio::platform::COMMODORE_16
            .byte PETSCII_ORANGE
.else
            .byte PETSCII_WHITE
.endif
            .byte "WARNING: Buggy "
.if PLATFORM = diskio::platform::COMMODORE_64
            .byte "1541U firmware"
.else
            .byte "emulator"
.endif
            .byte " detected. Please update.", 0

version:    .byte "Krill's Loader, revision ", REPOSITORY_VERSION, PETSCII_RETURN, "config "
            itoa4 MIN_DEVICE_NO
            itoa8 MAX_DEVICE_NO
            itoa1 ONLY_1541_AND_COMPATIBLE
            .byte '.'
            itoa8 DIRTRACK
            itoa8 DIRTRACK81
            itoa8 FILENAME_MAXLENGTH
            .byte '.'
            itoa8 MINSTEPSPEED
            itoa8 MAXSTEPSPEED
            itoa4 STEPPERACC
            .byte '.'
            itoa1 LOAD_COMPD_API
            itoa1 LOAD_RAW_API
            itoa1 NTSC_COMPATIBILITY
            itoa1 ALLOW_2_MHZ_ON_C128
            itoa1 LOAD_UNDER_D000_DFFF
            itoa1 MEM_DECOMP_API
            itoa1 MEM_DECOMP_TO_API
            itoa1 LOAD_TO_API
            itoa1 END_ADDRESS_API
            itoa1 LOAD_VIA_KERNAL_FALLBACK
            itoa1 IDLE_BUS_LOCK
            itoa1 DISABLE_WATCHDOG
            .byte '.'
            itoa4 DECOMPRESSOR
            itoa4 LC_SPEED
            .byte 0

            CHECK_INSTALL_END_ADDRESS

.exportzp status_OK                       = diskio::status::OK
.exportzp status_DEVICE_INCOMPATIBLE      = diskio::status::DEVICE_INCOMPATIBLE
.exportzp status_TOO_MANY_DEVICES         = diskio::status::TOO_MANY_DEVICES
.exportzp status_GENERIC_KERNAL_ERROR     = diskio::status::GENERIC_KERNAL_ERROR
.exportzp status_DEVICE_NOT_PRESENT       = diskio::status::DEVICE_NOT_PRESENT
.exportzp status_FILE_NOT_FOUND           = diskio::status::FILE_NOT_FOUND

.exportzp config_DECOMPRESSOR             = DECOMPRESSOR
.exportzp config_LC_SPEED                 = LC_SPEED
.exportzp config_LOAD_COMPD_API           = LOAD_COMPD_API
.exportzp config_LOAD_RAW_API             = LOAD_RAW_API
.exportzp config_NTSC_COMPATIBILITY       = NTSC_COMPATIBILITY
.exportzp config_ALLOW_2_MHZ_ON_C128      = ALLOW_2_MHZ_ON_C128
.exportzp config_UNINSTALL_API            = UNINSTALL_API
.exportzp config_LOAD_UNDER_D000_DFFF     = LOAD_UNDER_D000_DFFF
.exportzp config_MEM_DECOMP_API           = MEM_DECOMP_API
.exportzp config_MEM_DECOMP_TO_API        = MEM_DECOMP_TO_API
.exportzp config_LOAD_TO_API              = LOAD_TO_API
.exportzp config_END_ADDRESS_API          = END_ADDRESS_API
.exportzp config_LOAD_VIA_KERNAL_FALLBACK = LOAD_VIA_KERNAL_FALLBACK
.exportzp config_IDLE_BUS_LOCK            = IDLE_BUS_LOCK
.exportzp config_DIRTRACK                 = DIRTRACK
.exportzp config_DIRTRACK81               = DIRTRACK81
.exportzp config_FILENAME_MAXLENGTH       = FILENAME_MAXLENGTH
.exportzp config_DISABLE_WATCHDOG         = DISABLE_WATCHDOG
.exportzp config_ONLY_1541_AND_COMPATIBLE = ONLY_1541_AND_COMPATIBLE
