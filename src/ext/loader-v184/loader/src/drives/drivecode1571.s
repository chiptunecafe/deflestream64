
.include "cpu.inc"
.include "via.inc"
.include "cia.inc"

.include "drives/drivecode-common.inc"

CURRTRACK              = $00
MAXTRACK_A             = $01
LOADEDSECTOR           = $02
NUMSECTORS             = $03
MAXTRACK               = $04
LEDSTATE               = $05
DISKCHANGEBUFFER       = $06
ID0                    = $07
ID1                    = $08
CHECKSUM               = $09
ERRORCOUNT             = $0a
CLEARTRACKLINKTABLE    = $0b
HEADERTRACK            = $0c
TEMP                   = $0d
CURRSTEPSPEEDLOW       = TEMP
GCRBUFFER              = TEMP
TRACKINC               = $0e
LINKTRACK              = $0f
LINKSECTOR             = CHECKSUM
FILETRACK              = $10
FILESECTOR             = $11
FILENAMEHASHLO         = $12
FILENAMEHASHHI         = $13
NUMFILEBLOCKS          = $14
BLOCKINDEXBASE         = $15
CURRBLOCKINDEX         = $16
PREVBLOCKINDEXPLUS1    = $17
NUMFILES71             = $18; fixed: OPC_CLC
MAXCONFIRMEDBLOCKINDEX = $19
MAXCONFIRMEDBLKIDXPOS  = $1a
NUMCONTIGUOUSBLOCKS    = $1b
NEXTCONTIGUOUSBLOCK    = $1c
NEXTDIRBLOCKSECTOR     = $1d
DIRBLOCKPOS            = $1e
SPECULATIVEINTERLEAVE  = $1f

CUSTOMCODEUPLOAD       = INDEXTABLE

NUMFILEBLOCKSONTRACK   = $20; must be INDEXTABLE - 2: see trackinit
REQUESTEDSECTOR71      = $21; must be INDEXTABLE - 1: see trackinit
INDEXTABLE             = $22; $15 = MAXNUMSECTORS bytes
TRACKLINKTABLE71       = INDEXTABLE + MAXNUMSECTORS; $15 = MAXNUMSECTORS bytes

DIRBUFFER              = TRACKLINKTABLE71 + MAXNUMSECTORS

CYCLESTARTENDSECTOR    = NEXTCONTIGUOUSBLOCK
FILEINDEX              = CYCLESTARTENDSECTOR

FILENAME               = INDEXTABLE; max. $10 bytes

DIRBUFFSIZE            = (enddirbuffer - DIRBUFFER) / 5
DIRTRACKS              = DIRBUFFER
DIRSECTORS             = DIRTRACKS + DIRBUFFSIZE
NUMBLOCKS              = DIRSECTORS + DIRBUFFSIZE
FILENAMEHASHVALLO      = NUMBLOCKS + DIRBUFFSIZE
FILENAMEHASHVALHI      = FILENAMEHASHVALLO + DIRBUFFSIZE

DIRBUFFSIZE71          = DIRBUFFSIZE
.export DIRBUFFSIZE71

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

BLOCKBUFFER71          = $0700
TRACKOFFSET            = $00
SECTOROFFSET           = $01
BLOCKSOFFSET           = $1e

ROMOS_TRACK_DIFF       = $42

DECGCRTAB10ZZZ432LO    = $9f0d
DECGCRTAB3210ZZZ4LO    = $9f0f
DECGCRTAB0ZZZ4321HI    = $9f1d
DECGCRTAB210ZZZ43HI    = $9f2a
DECGCRTAB43210ZZZHI    = $a00d
DECGCRTABZZ43210ZHI    = $a10d
DECGCRTABZ43210ZZLO    = $a20d
DECGCRTABZZZ43210LO    = $a30d


BINARY_NIBBLE_MASK     = %00001111

MAXINTERLEAVE          = 16
MAXNUMSECTORS          = 21
NUMTRACKS_SINGLESIDED  = 41
NUMTRACKS_A            = 35
NUMTRACKS_B            = NUMTRACKS_SINGLESIDED
MAXTRACK71             = NUMTRACKS_A + NUMTRACKS_B

INDEXSPECULATIVE       = %01000000
BLOCKPENDING           = %00100000

TIMER                  = VIA2_T1C_H

ERR                    = $80; ERRORCOUNT reset value, $40 is too little: would cause stepping when spinning up
ERT                    = $20; ERRORCOUNT retry value

BITRATECOUNT           = 4; number of read attempts before bit-rate cycling on initial current track retrieval
SUCCESSCOUNT           = 32; number of consecutive successful per-bit-rate attempts on initial current track retrieval

.if (::PLATFORM <> diskio::platform::COMMODORE_128) || USE_ASYNCHRONOUS_BURST_HANDSHAKE
            .org $87
.else
            .org $7e
.endif

.export c1570fix0 : absolute
.export c1570fix1 : absolute

drvcodebeg71:
            .byte .hibyte(dinstall - * + $0100 - $01); init transfer count hi-byte

FINDTRACKN = INDEXTABLE

bitratecnt = PREVBLOCKINDEXPLUS1
successcnt = MAXCONFIRMEDBLKIDXPOS

enddirbuffer:

V2A:        .word VIA2_PRA

V1B = dgetbyte + 3
dgetbyte:   DRIVEGETBYTE getbyte
getbyterts: rts; is changed to sta (zp,x) for custom drive code upload
            .byte .lobyte(CUSTOMPARAM + $04 - $ff); x = $ff
            inc .lobyte(CUSTOMPARAM + $04)
            bne :+
            inc .lobyte(CUSTOMPARAM + $05)
:           dec .lobyte(CUSTOMPARAM + $02)
            bne dgetbyte
            dec .lobyte(CUSTOMPARAM + $03)
            bpl dgetbyte

            .byte OPC_JMP_ABS; execute custom drive code
customparm: .byte 0

fadeled:    jsr one_mhz; 1 MHz so the LED fades at the same speed as on 1541

MOTOR_LED_MASK = ~(BUSY_LED | MOTOR)
            ldx #.lobyte(MOTOR_LED_MASK)
            lda (.lobyte(V2B - MOTOR_LED_MASK),x); VIA2_PRB
            ldy LEDSTATE
            bne :+
            sax (.lobyte(V2B - MOTOR_LED_MASK),x); VIA2_PRB
            rts

:           dey
            bne :-
            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in LED fade loop. *****"

BUSY_LED_MASK = ~BUSY_LED
            ldx #.lobyte(BUSY_LED_MASK)
            sax (.lobyte(V2B - BUSY_LED_MASK),x); VIA2_PRB
            ldy LEDSTATE
:           iny
            bne :-
            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in LED fade loop. *****"

            dec LEDSTATE
V2B = * + 1
bitsetv2b:  ora VIA2_PRB
store_via2: sta VIA2_PRB
notrkstep:  rts

trackseek:  lda #MOTOR; turn on the motor
trackseekx: jsr bitsetv2b
            ldx #$80 | (MINSTEPSPEED + 1)
trackstep:  tya; destination track
            beq notrkstep; don't do anything if invalid track
            cmp MAXTRACK
            bcs notrkstep; don't do anything if invalid track
            sec
            lda CURRTRACK
            sbc MAXTRACK_A
            beq :+
            bcc :+
            sta CURRTRACK; the current track is on the 2nd side,
                         ; temporarily store the 2nd side physical track number
:           sec
            tya; destination track
            sbc MAXTRACK_A
            beq :+
            bcs :++; branch if the destination track is on the 2nd side
:           clc
            tya; the destination track is on the 1st side
:           pha
V1A = * + 1
            lda VIA1_PRA
            .assert * <= $0100, error, "***** 1571 V1A too high in memory. *****"

            and #.lobyte(~SIDE_SELECT)
            bcc :+
            ora #SIDE_B
:
c1570fix1:  sta VIA1_PRA
            pla
            sec
            sbc CURRTRACK
            sty CURRTRACK
            beq initlinktb; branch if destination track reached

            ; do the track jump
            ldy #$01; move up (inwards)
            sty CURRSTEPSPEEDLOW
            bcs :+
            eor #.lobyte(~$00); invert track difference
            adc #$01
halftrkdwn: ldy #$03; move down (outwards)
:           sty TRACKINC
            asl; half-tracks
            tay

            jsr one_mhz

halftrack:  stx TIMER; reset track-step timer
            lda VIA2_PRB
            anc #.lobyte(~(SYNC_MARK | MOTOR)); clc
            adc TRACKINC
            ora #MOTOR
            sta VIA2_PRB
            cpx #($80 | SINGLESTEPSPEED) - 1
            beq initlinktb; stepping to adjacent track: branch if second half-track step has been issued
            txa
headaccl:   cmp #$80 | MAXSTEPSPEED
            beq noheadacc
            pha
           ;sec
            lda CURRSTEPSPEEDLOW
            sbc #STEPPERACC
            sta CURRSTEPSPEEDLOW
            pla
            sbc #$00
noheadacc:  cpx TIMER
            beq noheadacc; wait until the counter hi-byte has decreased by 1
            dex
            bmi headaccl
seekswitch: tax
            dey
            bne halftrack

initlinktb: lda CLEARTRACKLINKTABLE
            bpl :++
            ldx #MAXNUMSECTORS - 1
:           sta TRACKLINKTABLE71,x; sector links are unknown
            dex
            bpl :-
:           lda #ERR
            sta ERRORCOUNT

            ; bit-rates:
            ; tracks 31-35/66+   (17 blocks): %00 - sector interleave 3 (lowest density, slowest clock, innermost tracks)
            ; tracks 25-30/60-65 (18 blocks): %01 - sector interleave 3
            ; tracks 18-24/53-59 (19 blocks): %10 - sector interleave 3
            ; tracks  1-17/36-52 (21 blocks): %11 - sector interleave 4 (highest density, fastest clock, outermost tracks)
setbitrate: lda CURRTRACK
            jsr getnumscts
            stx NUMSECTORS
            ldy #$00
            sta (V2B),y; VIA2_PRB
            ; fall through

two_mhz:    lda #BYTE_READY | TWO_MHZ; the accu must contain a negative number upon return
            ora VIA1_PRA
            bne :+; jmp

            ; for normal busy LED fading speed and correct head stepping speed
one_mhz:    lda #.lobyte(~TWO_MHZ)
            and VIA1_PRA
:           sta VIA1_PRA
readerrts:  rts

getnumscts: tay
            sec
            sbc MAXTRACK_A
            beq :+
            bcs getnumscts
:           ldx #21; number of blocks
            lda #SYNC_MARK | BITRATE_MASK; $e0
            ora ((V2B - 21),x); VIA2_PRB
            cpy #18
            bcc :++; bit-rate $60
            dex
            dex; 19
            cpy #25
            bcc :+ ; bit-rate $40
            dex; 18
            and #.lobyte(~(%10 << BITRATE_SHIFT)); -$40
            cpy #31
            bcc :++; bit-rate $20
            dex; 17
:           and #.lobyte(~(%01 << BITRATE_SHIFT)); -$20
:           rts

            BLOCKBUFFER = BLOCKBUFFER71
            FNAMEHASH 1571; must not be overwritten by watchdog IRQ/reset/custom code upload

            ; * >= $0100
stack:
            .assert stack >= $0100, error, "***** 1571 stack too low in memory. *****"

            .word 0, 0, 0, 0
stackend:
topofstack71 = stackend - 1

            .assert stackend < $0200, error, "***** 1571 stack too high in memory. *****"

readblock:  lda #%11000000
           ;ldy #$00
readheader: bit VIA1_PRA
            bmi readheader
            eor VIA2_PRA ; 11222223
            sta GCRBUFFER
            and #%11000000
            bne readerrts; z = 0
            sta CHECKSUM

loaddata:   bit VIA1_PRA              ;  4
            bmi loaddata              ;  6
            lda GCRBUFFER             ;  9 ; 11222223
            lsr                       ; 11 ; .1122222:3
            lda VIA2_PRA              ; 15 ; 33334444
            pha                       ; 18
            ror                       ; 20 ; 33333444
            lsr                       ; 22 ; .3333344
            tax                       ; 24
            lda DECGCRTABZ43210ZZLO,x ; 28 ; x = [($00..$ff) & $f1], 22223333
            ldx GCRBUFFER             ; 31 ; 11222223
            ora DECGCRTABZZ43210ZHI,x ; 35 ; x = [$00..$ff], %2222....
            sta BLOCKBUFFER71 + $00,y ; 40
            eor CHECKSUM              ; 43

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            sta CHECKSUM              ;  9
            lda VIA2_PRA              ; 13 ; 45555566
            sta GCRBUFFER             ; 16
            asl                       ; 18 ; 5555566.
            pla                       ; 22 ; 33334444
            rol                       ; 24 ; 33344444
            asl                       ; 26 ; 3344444.
            tax                       ; 28
            lda DECGCRTABZZ43210ZHI,x ; 32 ; x = [($00..$ff) & $8f], 4444....
            ldx GCRBUFFER             ; 35 ; 45555566
            ora DECGCRTABZ43210ZZLO,x ; 39 ; x = [$00..$ff], 44445555

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            sta BLOCKBUFFER71 + $01,y ; 11
            eor CHECKSUM              ; 14
            sta CHECKSUM              ; 17
            lda #%00000011            ; 19
            axs #%00100000            ; 21 ; HHHLLL66
            lda VIA2_PRA              ; 25 ; 66677777
            sta GCRBUFFER             ; 28
            ora #%00011111            ; 30 ; 666HHHHH
            axs #$00                  ; 32 ; 666LLL66
            lda DECGCRTAB210ZZZ43HI,x ; 36 ; x = [($00..$ff) & $e3], 6666....
            ldx GCRBUFFER             ; 39 ; 66677777
            ora DECGCRTABZZZ43210LO,x ; 43 ; x = [$00..$ff], 66667777

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            ldx VIA2_PRA              ; 10 ; 00000111
            sta BLOCKBUFFER71 + $02,y ; 15
            eor CHECKSUM              ; 18
            sta CHECKSUM              ; 21
            lda DECGCRTAB43210ZZZHI,x ; 25 ; x = [$00..$ff], 0000....
            pha                       ; 28
            lda #%00000111            ; 30
            axs #%01000000            ; 32 ; HHLLLL111
            iny                       ; 34
            iny                       ; 36
            iny                       ; 38

:           bit VIA1_PRA              ;  4
            bmi :-                    ;  6
            lda VIA2_PRA              ; 10 ; 11222223
            sta GCRBUFFER             ; 13
            ora #%00111111            ; 15 ; 11HHHHHH
            axs #$00                  ; 17 ; 11LLL111
            pla                       ; 21 ; 0000....
            ora DECGCRTAB10ZZZ432LO,x ; 25 ; x = [($00..$ff) & $87]; ....1111
            sta BLOCKBUFFER71 + $00,y ; 30
            eor CHECKSUM              ; 33
            sta CHECKSUM              ; 36
            iny                       ; 38
            beq :+                    ; 40
            jmp loaddata              ; 43

:           lda (V1A),y; VIA1_PRA
            bmi :-
            lax GCRBUFFER         ; 11222223
            lsr                   ; .1122222:3
            lda (V2A),y; VIA2_PRA ; 33334444
            ror                   ; 33333444
            lsr                   ; .3333344
            tay
            lda DECGCRTABZZ43210ZHI,x; x = [$00..$ff], 2222....
            ora DECGCRTABZ43210ZZLO,y; y = [($00..$ff) & $f1], 22223333
            tay; ID0
            eor CHECKSUM
           ;clc
            rts; z = 1

chksumerr:  ; with repeated checksum errors, head might have landed between tracks or on the wrong track
            lda ERRORCOUNT; is changed to dec ERRORCOUNT after init
            bne :+

            ldx #ERT
            stx ERRORCOUNT
            lda (.lobyte(V2B - ERT),x); VIA2_PRB
            anc #BITRATE | BUSY_LED | TRACK_STEP
            sbc #(1 << BITRATE_SHIFT) - 1; cycle to the next bit-rate (denser until wrap)
            adc #$03; and step down half a track on bit-rate wrap
            ora #MOTOR
            sta (.lobyte(V2B - ERT),x); VIA2_PRB
:           sec; operation not successful
            rts; checksum mismatch: z = 0, c = 1

            ; getblock calls
            ; in: y: track
            ;     a: sector or negative value for any sector
getblkchid: ldx #.lobyte(compareid - (idswitch + 2)); check against stored ID
getblkstid: ; store read ID, ldx #.lobyte(storeid - (idswitch + 2)) executed by caller
            ldy #DIRTRACK
            sta REQUESTEDSECTOR71
            stx idswitch + 1
            jsr trackseek; stores the number of blocks on the current track in NUMSECTORS
getblkscan: ; the disk spins at approximately 300 rpm, so a revolution takes about 2,000,000 * 60 / 300 = 400,000 cycles at 2 MHz,
            ; so the timeout counter cannot be set to one revolution: it is reset upon waiting for every new sync,
            ; thus a timeout only indicates a sync-less track range (about 65536 / 400,000 * 19 = 3.11 sectors), but missing
            ; sectors or similar will leave the loader spinning forever
findblkhdr: jsr waitsync
            beq bcsreaderr; returns with carry set on time-out
            cmp #%01010010; $52, check if the sync is followed by a block header
            beq :+
            jsr waitsync; not a block header, retry
            beq bcsreaderr; returns with carry set on time-out
            cmp #%01010010; $52, check if the sync is followed by a block header

.if (::PLATFORM <> diskio::platform::COMMODORE_128) || USE_ASYNCHRONOUS_BURST_HANDSHAKE
            SKIPWORD_NOP

            .assert * >= $02a9, error, "***** 1571 watchdog IRQ vector located below $02a9. *****"
            .assert * <= $02a9, error, "***** 1571 watchdog IRQ vector located above $02a9. *****"

            .word uninstall
.endif ; (::PLATFORM <> diskio::platform::COMMODORE_128) || USE_ASYNCHRONOUS_BURST_HANDSHAKE

            bne chksumerr

:           lda #%01000000
            ldy #$fc
            jsr readheader

.if (::PLATFORM = diskio::platform::COMMODORE_128) && (USE_ASYNCHRONOUS_BURST_HANDSHAKE = 0)
            SKIPWORD_NOP

            .assert * >= $02a9, error, "***** 1571 watchdog IRQ vector located below $02a9. *****"
            .assert * <= $02a9, error, "***** 1571 watchdog IRQ vector located above $02a9. *****"

            .word uninstall
.endif ; (::PLATFORM = diskio::platform::COMMODORE_128) && (USE_ASYNCHRONOUS_BURST_HANDSHAKE = 0)

bnecsumerr: bne chksumerr

            lda BLOCKBUFFER71 + $fd; sector
            cmp NUMSECTORS
            bcs chksumerr
            sta LOADEDSECTOR
            lda BLOCKBUFFER71 + $fe; track
            beq chksumerr
            cmp MAXTRACK
            bcs chksumerr

            sta HEADERTRACK
            ldx BLOCKBUFFER71 + $ff; ID1

            ; y = ID0
idswitch:   .byte OPC_RTS, 0; is changed to bcc storeid/compareid after init

storeid:    sty ID0
            stx ID1

compareid:  cpy ID0
            bne readerror
            cpx ID1
            bne readerror

            lax LOADEDSECTOR
            eor REQUESTEDSECTOR71
            beq loadblock; branch if requested sector

            bmi :+; branch if ANYSECTOR
            ; specific sector requested but not reached yet
            lda REQUESTEDSECTOR71
            cmp NUMSECTORS
            bcc findblkhdr
bcsreaderr: bcs readerror; jmp

:           ; negative value: no specific sector requested, out-of-order sector fetch
           ;ldx LOADEDSECTOR
            lda INDEXTABLE,x
            ldy TRACKLINKTABLE71,x
            ldx #.lobyte(~(BLOCKPENDING | INDEXSPECULATIVE))
            sax CURRBLOCKINDEX
            asl
            iny; with negative INDEXTABLE value, branch to findblkhdr if block index is unknown but linked block is known (the block has been loaded before already)
            bcs :+; branch if block index not known
            asl; n = BLOCKPENDING, block index is known, or speculated to belong to the file
:           bpl findblkhdr; with positive INDEXTABLE value, branch if the block has already been loaded into the computer's memory (BLOCKPENDING not set)

loadblock:  ; wait for data block sync
            jsr waitsync; reset the time-out timer here
            cmp #%01010101; $55, check if the sync is followed by a data block
            bne bnecsumerr

            jsr readblock
            bne bnecsumerr
           ;clc

            lax HEADERTRACK
            eor CURRTRACK
            beq sanitychsw

            ; track error
            ldy CURRTRACK
            stx CURRTRACK
            lsr CLEARTRACKLINKTABLE
            jsr trackseek
            asl CLEARTRACKLINKTABLE
            bcc readerror; jmp

            ; block link sanity check
sanitychsw: .byte OPC_RTS, LOADEDSECTOR; is changed to ldx LOADEDSECTOR after init
            lda #$ff
            sta TRACKLINKTABLE71,x

            lda BLOCKBUFFER71 + SECTOROFFSET
            sta LINKSECTOR
TWO = * + 1
            cmp #$02
            lda BLOCKBUFFER71 + TRACKOFFSET
            sta LINKTRACK
            beq chklastbsz
            cmp MAXTRACK; check whether track link is within the valid range
            bcs linkerror; if not, return error
            jsr getnumscts; get number of sectors on linked track
            cpx LINKSECTOR; check whether sector link is within the valid range
chklastbsz: ; branch if invalid block size (0..1 = 1..2 bytes)
            bcc linkerror; branch if sector number too large

            lda LINKSECTOR   ; return the loaded block's sector link sector number
            ldx LOADEDSECTOR ; return the loaded block's sector number
            ldy LINKTRACK    ; return the loader block's sector link track number
            cpy CURRTRACK
            bne :+
            cmp LOADEDSECTOR       ; block must not link to itself
            beq linkerror          ; (but larger cycles are not detected)
            cpy CURRTRACK          ; z flag must be set
            sta TRACKLINKTABLE71,x ; set block link
:           clc                    ; operation successful
            rts

linkerror:  asl INDEXTABLE,x
            bmi readerror; branch if INDEXSPECULATIVE is set: clear index, set MSB
doinvalidx: ror INDEXTABLE,x; restore non-speculative index
            SKIPWORD; skip to NUMFILES71 = clc

:           sec            ; refill directory buffer when disk has been changed
            ror NUMFILES71 ; NUMFILES71 = $18 = OPC_CLC
readerror:  ; ID mismatch or illegal track or sector (invalid track/sector link)
checkchg:   ; must not change y
            lax VIA2_PRB; check light sensor for disk removal
            eor DISKCHANGEBUFFER
            stx DISKCHANGEBUFFER
            and #WRITE_PROTECT
            bne :-
            ; read error: z = 1
            sec; operation not successful
            rts

fadeledidl: jsr fadeled  ; will switch off motor
            bne fadeledidl
            beq havelinks; jmp

waitsync:   ldy #$ff
            sty TIMER
            iny
:           lda TIMER
            beq readerror; will return $00 in the accu
            lda (V1B),y; VIA1_PRB
chkbusswit: .byte OPC_BIT_ZP, bplchckbus - (* + 1); is changed to bpl bplchckbus after init: no watchdog, go to reset routine on ATN_IN clear
            lsr                                          ; check for DATA_IN clear when collecting links,
collswit71: .byte OPC_BIT_ZP, .lobyte(checkchg - (* + 1)); is changed to bcc checkchg to branch on load request
            lda (V2B),y; VIA2_PRB, check SYNC
            bmi :-; branch if no sync
            lda (V2A),y; VIA2_PRA; clear latch
:           lda (V1A),y; VIA1_PRA, check BYTEREADY
            bmi :-; branch if byte not ready
            lda (V2A),y; VIA2_PRA; is never $00 but usually $52 (header) or $55 (data)
            rts

statussent: lda BLOCKBUFFER71 + $00; offset 0: block index or status byte
            bmi fadeledidl; only after successful load
            PREPARE_NEXT_FILE 1571

idleloop71: jsr checkchg

            lda REQUESTEDSECTOR71
            bpl havelinks

            ; collect links
            ldx NUMSECTORS
           ;sec
checklinks: sta INDEXTABLE - 1,x
            ldy TRACKLINKTABLE71 - 1,x
            iny
            bpl :+
            clc; link not known
:           dex
            bne checklinks
            bcc collectlnk; branch if any link not known

havelinks:  jsr fadeled
            lsr REQUESTEDSECTOR71
bplchckbus: bpl checkbus; jmp

            ; get custom drive code
getcustom: ;ldx #$00
:           inx
            ldy dgetbyte - 1,x
            sty CUSTOMCODEUPLOAD - 1,x
            bne :-

            ldx #OPC_STA_ZPXI; $81 = ATN_IN | DATA_IN
            stx .lobyte(getbyterts - dgetbyte + CUSTOMCODEUPLOAD)

            ldx #5

            ; no watchdog, ATN_IN is clear
           ;lda #CLK_IN
            sta (V1B),y; VIA1_PRB, clear ATNA_OUT: sets DATA_OUT to signal ready for code upload
:           and (V1B),y; VIA1_PRB, wait for CLK_IN clear
            bne :-
            lda #ATNA_OUT
            sta (V1B),y; VIA1_PRB

CUSTOMPARAM = customparm - dgetbyte + CUSTOMCODEUPLOAD
:           jsr dgetbyte
            sta .lobyte(CUSTOMPARAM),x
            dex
            bpl :-

            jsr two_mhz

           ;ldx #$ff
            txs
            jmp CUSTOMCODEUPLOAD

:           inc CURRTRACK
uninstall:  ldy #18; ROM dir track
            jsr trackseek
           ;ldy #$00
            lda (V2B),y; VIA2_PRB
            bit TWO; the stepper bits are set to %00 after reset: ensure full-track
            bne :- ; stepper alignment (may still be off by a half-track)
            and #BUSY_LED
            beq :++
            dey
            sty LEDSTATE
:           jsr fadeled
            bne :-
:           jmp (RESET_VECTOR)

collectlnk: jsr two_mhz
            lda #OPC_BCC
            sta collswit71
            jsr getblkscan
            lda #OPC_BIT_ZP
            sta collswit71

checkbus:   ldx #0
            lda (V1B,x); VIA1_PRB
            ora (V1B,x); VIA1_PRB, to be safe, read a second time
            bpl uninstall
            lsr
            bcs idleloop71; wait until there is something to do

            GET_FILENAME 1571; transfer in slow mode in order to be less susceptible
            jsr two_mhz      ; to interference by any passive drives running at 1 MHz

            ; matches against hash of filename in FILENAMEHASHLO/HI
            NUMFILES = NUMFILES71
            FIND_FILE 1571

            lda #diskio::status::FILE_NOT_FOUND; $ff
            bcs filenfound
            sta LEDSTATE
            lda DIRSECTORS,x
            sta MAXCONFIRMEDBLKIDXPOS
            sta FILESECTOR
            lda FILENAMEHASHVALLO + 1,x ; PREPARE_NEXT_FILE
            sta FILENAMEHASHLO          ; functionality
            lda FILENAMEHASHVALHI + 1,x ; store hash of next file's
            sta FILENAMEHASHHI          ; name for loadnext

            lda #$00
            sta PREVBLOCKINDEXPLUS1
            sta MAXCONFIRMEDBLOCKINDEX
trackloop:  sta BLOCKINDEXBASE
            lda #BUSY_LED | MOTOR
            jsr trackseekx
            ; accu contains a negative number
            ; x contains the number of sectors on this track
            inx
trackinit:  sta INDEXTABLE - 2,x; sector indices are unknown
            dex
            bpl trackinit; sets REQUESTEDSECTOR71 = INDEXTABLE - 1 with x = 1 and NUMFILEBLOCKSONTRACK = INDEXTABLE - 2 with x = 0

            .assert .hibyte(*) = .hibyte(trackinit), error, "***** Page boundary crossing in trackinit loop. *****"

            ldx FILESECTOR
            stx NEXTCONTIGUOUSBLOCK
           ;ldy #$00; initial block index
            sty NUMCONTIGUOUSBLOCKS
            jsr speculinit; set non-speculative block indices according to known links, build initial speculative block index table for this track

blockloop:  jsr getblkscan
            bcs blockloop; branch if block fetch not successful

           ;ldx LOADEDSECTOR
           ;ldy LINKTRACK
           ;cpy CURRTRACK
            bne nosetspcil; branch if linked block not on current track
            sec
           ;lda LINKSECTOR
            sbc LOADEDSECTOR; determine likely interleave
            tay
            bcs setspecilv
nosetspcil: ldy SPECULATIVEINTERLEAVE
setspecilv: jsr loadspec
            bcs blockloop

            bne blockloop

           ;lda NUMFILEBLOCKSONTRACK
            adc BLOCKINDEXBASE
            ldy FILETRACK
            bne trackloop

            ; loading is finished

            tya; $00 = diskio::status::OK
filenfound: ldy #$00; send over one byte
            jsr sendblock
            lda #ATN_IN | ATNA_OUT | CLK_OUT | CLK_IN
:           cmp VIA1_PRB; wait until host
            beq :-      ; is in idle mode
            jmp statussent

loaderror:  sec; operation not successful
            rts

loadspec:  ;ldx LOADEDSECTOR
            lda INDEXTABLE,x
            and #.lobyte(~(BLOCKPENDING | INDEXSPECULATIVE)); clc, clear mis-speculation detected flag
            bmi loaderror; branch if block conceivably not belonging to the file
            sta CURRBLOCKINDEX
            beq refuspecil; branch if first file block on track: cannot determine likely interleave

            lda PREVBLOCKINDEXPLUS1; first file block must be loaded first in order to know the load address to be able to place successive
            beq idxinvalid         ; blocks in host memory, invalidate index to avoid skipping over every other block in a livelock

            tya
            beq refuspecil
            eor #$ff
            sec
            adc LOADEDSECTOR
            bcs :+
            adc NUMSECTORS
:           tax
            lda TRACKLINKTABLE71,x
            cmp LOADEDSECTOR
            bne refuspecil; only accept interleave if distances to this block and the next match
            cpy #MAXINTERLEAVE + 1; validate
            bcs refuspecil        ; speculative interleave
            sty SPECULATIVEINTERLEAVE

refuspecil: ldx MAXCONFIRMEDBLKIDXPOS
            ldy MAXCONFIRMEDBLOCKINDEX
            clc; clear mis-speculation detected flag
            jsr specmanage

            ldx LOADEDSECTOR
            lda INDEXTABLE,x
            anc #.lobyte(~BLOCKPENDING); clc
            sta INDEXTABLE,x
           ;clc
            sbc #INDEXSPECULATIVE - 1
            ; c = 1 if index speculative, 0 otherwise
            ldy TRACKLINKTABLE71,x
            bpl notrklink

            ; block links to another track or is the file's last one
            ldy CURRBLOCKINDEX
            iny
            bcc settrklink; branch if block was not loaded speculatively
            bit NUMFILEBLOCKSONTRACK
            bpl idxinvalid; branch if track link has been set already
            cpy NUMSECTORS
            bcc idxinvalid; branch if possibly not file's last block on track

settrklink: sty NUMFILEBLOCKSONTRACK
            ldy LINKTRACK
            sty FILETRACK
            ldy LINKSECTOR
            sty FILESECTOR; first sector on the next track

notrklink:  bcc :+; branch if block index is not speculative
            cmp NUMFILEBLOCKSONTRACK
            bcc :+; do not transfer block if speculative index is out of range
idxinvalid: sec
            jmp doinvalidx
:
            ldx NEXTCONTIGUOUSBLOCK
:           lda INDEXTABLE,x
            cmp #$1f
            bcs :+; branch if block has not been confirmed and transferred already
            inc NUMCONTIGUOUSBLOCKS
            lda TRACKLINKTABLE71,x
            tax
            bpl :-; branch if there is a linked block on the same track
:           stx NEXTCONTIGUOUSBLOCK

            lda NUMCONTIGUOUSBLOCKS
            cmp NUMFILEBLOCKSONTRACK
            bne nostep
            lda FILETRACK
            beq nostep

            ; perform Shrydar Stepping (R)(TM) to minimise single-track stepping overhead:
            ; nudge the R/W head in the right direction, then wait a few bycles while it gains momentum,
            ; then enable the destination track's stepper magnet long before the head has reached the intermediate half-tracks magnet,
            ; relying on the head's inertia, then send over the block while the head keeps moving beyond the intermediate half-tracks stepper magnet
            ; and ultimately settles on the destination track.
            ; sending the block over takes at least 72 bycles
            ldy CURRTRACK
            cpy FILETRACK
            bcs :+
            iny
            SKIPBYTE
:           dey
            ldx #$80 | SINGLESTEPSPEED
            jsr trackstep

nostep:     sec
            lax BLOCKINDEXBASE
            adc NUMCONTIGUOUSBLOCKS
            ldy #$ff
            dec LINKTRACK
            bpl :+
            ; handle file's last block
            ldy LINKSECTOR; the file's last block's length (last byte index)
           ;clc
            lda #$01
            sbc LINKSECTOR
:           sta BLOCKBUFFER71 + $01; block size

            ; calculate block index in host memory
            sec
            txa; BLOCKINDEXBASE
            adc CURRBLOCKINDEX
            tax
            sec
            sbc PREVBLOCKINDEXPLUS1
            stx PREVBLOCKINDEXPLUS1

            asl LINKTRACK; last block: set lsb, clear lsb otherwise
            rol

            ; accu: block index or status byte
sendblock:  sta BLOCKBUFFER71 + $00; block index or status byte
            sty blocksize + $01
            ldx #$20; here, the watchdog timer is polled manually because
                    ; an extra-long time-out period is needed since the computer may
                    ; still be busy decompressing a large chunk of data,
                    ; this is the round counter: $20 * ($ff00 - $0100) = 2,080,768 cycles at 2 MHz is roughly 1 second

            ldy #$ff
            sty TIMER; reset watchdog time-out

            lda #ATNA_OUT | DATA_OUT; clear CLK_OUT, set DATA_OUT as signal of presence
            sta (V1B - $20,x); VIA1_PRB; block ready signal
waitready:  lda TIMER; see if the watchdog barked
            bne :+
            dex      ; if yes, decrease the round counter
.if DISABLE_WATCHDOG
            beq :+
.else
            beq timeout; and trigger watchdog on time-out
.endif
            sty TIMER; reset watchdog time-out
:           bit VIA1_PRB
            bmi waitready; wait for ATN_IN clear
            sty TIMER; reset watchdog time-out

timeout:    ENABLE_WATCHDOG
            iny; ldy #$00

.if ::PLATFORM = diskio::platform::COMMODORE_128

            lda #TWO_MHZ | FAST_SERIAL_OUTPUT
            ora (V1A),y; VIA1_PRA
            sta (V1A),y; VIA1_PRA
            lda #IOMODE_OUTPUT | COUNT_PHI2 | CONTINUOUS | TIMER_START
            sta CIA_CRA
            lda #CLK_OUT; clear ATNA_OUT, $08 = SERIAL_IRQ
            sta (V1B),y; VIA1_PRB

            bit CIA_ICR
            ldx BLOCKBUFFER71,y

    .if USE_ASYNCHRONOUS_BURST_HANDSHAKE

sendloop:   stx CIA_SDR; clock out data byte
blocksize:  cpy #$00
            iny
            ldx #ATN_IN | DEVICE_NUMBER | ATNA_OUT; set ATNA_OUT, clear CLK_OUT
:           bit CIA_ICR; wait until data sent
            beq :-
            stx TIMER; need some slack, as CIA sets the flag a little too early (MOS5710 doesn't)
            stx VIA1_PRB; toggle CLK_OUT: signal data sent, toggle ATNA_OUT
            ldx BLOCKBUFFER71,y
:           bit VIA1_PRB; wait for ATN_IN set = data taken
            bpl :-
            bcs senddone

            stx CIA_SDR; clock out data byte
            cpy blocksize + 1
            iny
            ldx #$ff
:           bit CIA_ICR; wait until data sent
            beq :-
            stx TIMER; need some slack, as CIA sets the flag a little too early (MOS5710 doesn't)
            sta VIA1_PRB; toggle CLK_OUT: signal data sent, toggle ATNA_OUT
            ldx BLOCKBUFFER71,y
:           bit VIA1_PRB; wait for ATN_IN clear = data taken
            bmi :-
            bcc sendloop
senddone:

    .else; !USE_ASYNCHRONOUS_BURST_HANDSHAKE

            stx CIA_SDR; clock out data byte
sendloop:
blocksize:  cpy #$00
            bcs waittaken
            iny
            ldx BLOCKBUFFER71,y
            lda #ATN_IN | DEVICE_NUMBER | ATNA_OUT; set ATNA_OUT
            sta TIMER
:           bit VIA1_PRB; wait for ATN_IN set = data taken
            bpl :-
            sta VIA1_PRB; toggle ATNA_OUT
            stx CIA_SDR; clock out data byte

            cpy blocksize + 1
            bcs waitdtaken
            iny
            ldx BLOCKBUFFER71,y
            lda #ATN_IN | DEVICE_NUMBER; clear ATNA_OUT
            sta TIMER
:           bit VIA1_PRB; wait for ATN_IN clear = data taken
            bmi :-
            sta VIA1_PRB; toggle ATNA_OUT
            stx CIA_SDR; clock out data byte
            bcc sendloop; jmp

waitdtaken: bit VIA1_PRB; wait for ATN_IN clear = data taken
            bmi waitdtaken
            bpl senddone
waittaken:  bit VIA1_PRB; wait for ATN_IN set = data taken
            bpl waittaken
senddone:
    .endif; !USE_ASYNCHRONOUS_BURST_HANDSHAKE

BURST_OFF = IOMODE_INPUT | COUNT_PHI2 | CONTINUOUS | TIMER_START
            ldx #BURST_OFF
            stx CIA_CRA
            lda #.lobyte(~FAST_SERIAL_OUTPUT)
            and (V1A - BURST_OFF,x); VIA1_PRA
            sta (V1A - BURST_OFF,x); VIA1_PRA

            lda #ATNA_OUT | CLK_OUT; drive busy
            sta (V1B - BURST_OFF,x); VIA1_PRB

.else; ::PLATFORM <> diskio::platform::COMMODORE_128

sendloop:   lda #$ff                ; 2
            sta TIMER               ; 4 - reset watchdog time-out
            lda BLOCKBUFFER71,y     ; 4
            and #BINARY_NIBBLE_MASK ; 2
            tax                     ; 2
            lda sendnibbletab,x     ; 4
                                    ; = 32

:           bit VIA1_PRB            ; 4
            bmi :-                  ; 3
            sta VIA1_PRB            ; 4
            asl                     ; 2
            ora #ATNA_OUT           ; 2
                                    ; = 15

:           bit VIA1_PRB            ; 4
            bpl :-                  ; 3
            sta VIA1_PRB            ; 4
            lda BLOCKBUFFER71,y     ; 4
            lsr                     ; 2
            lsr                     ; 2
            lsr                     ; 2
            lsr                     ; 2
            tax                     ; 2
            lda sendnibbletab,x     ; 4
                                    ; = 29

:           bit VIA1_PRB            ; 4
            bmi :-                  ; 3
            sta VIA1_PRB            ; 4
            asl                     ; 2
            ora #ATNA_OUT           ; 2
blocksize:  cpy #$00                ; 2
            iny                     ; 2
                                    ; = 19

:           bit VIA1_PRB            ; 4
            bpl :-                  ; 3
            sta VIA1_PRB            ; 4
            bcc sendloop            ; 3
                                    ; = 95

            lda #ATNA_OUT | CLK_OUT; drive busy
:           bit VIA1_PRB; wait for ATN low,
            bmi :-      ; acknowledgement of the last data byte

            sta VIA1_PRB

.endif; ::PLATFORM <> diskio::platform::COMMODORE_128

:           bit VIA1_PRB; wait for ATN_IN set,
            bpl :-      ; acknowledgement of the block transfer

            sei; disable watchdog
            lda NUMCONTIGUOUSBLOCKS
            cmp NUMFILEBLOCKSONTRACK
            clc
genspcdone: rts

            ; validate block indices according to currently-known links
indexloop:  tax; link sector
            iny; block index
            tya
            eor INDEXTABLE,x
            eor #INDEXSPECULATIVE
            beq confirmspc; branch if already-loaded speculative index matches
            eor #BLOCKPENDING
            beq confirmidx; branch if not-yet-loaded speculative index matches
speculinit: sec ; mis-speculated block index detected
confirmidx: tya
            ora #BLOCKPENDING
confirmspc: sta INDEXTABLE,x
specmanage: lda TRACKLINKTABLE71,x; linked sector
            bpl indexloop; branch if there is a linked block on the same track

            .assert .hibyte(*) = .hibyte(indexloop), error, "***** Page boundary crossing in indexloop. *****"

            lda PREVBLOCKINDEXPLUS1
            beq genspcdone

            stx MAXCONFIRMEDBLKIDXPOS
            sty MAXCONFIRMEDBLOCKINDEX
            bcc genspcdone

            ; mis-speculated block indices detected: rebuild speculative block index table
            lda BLOCKINDEXBASE
            beq genspcdone; do not speculate for the first file track, as this likely leads to much mis-speculation and ultimately lower speed

            clc; minus one block: do not transfer alleged final block, as it may be mis-speculated and load beyond the actual file end address
            lda NUMFILEBLOCKS
            sbc BLOCKINDEXBASE
            bcc genspcdone; limit to alleged number of file blocks
            cmp NUMSECTORS
            ldx NUMSECTORS
            bcc :+
           ;ldx NUMSECTORS
            txa
:           sta TEMP

clearidxtb: asl INDEXTABLE - 1,x
            bmi :+; branch if INDEXSPECULATIVE is set: clear index, set MSB
            ror INDEXTABLE - 1,x; restore non-speculative index
:           dex
            bne clearidxtb

            ldx MAXCONFIRMEDBLKIDXPOS
           ;ldy MAXCONFIRMEDBLOCKINDEX
genspcloop: iny
            cpy TEMP
            bcs genspcdone
            lda TRACKLINKTABLE71,x
            bpl havenewidx; branch if there is a linked block on the same track
            txa
           ;clc
            adc SPECULATIVEINTERLEAVE
            SKIPWORD
specidxtkn: inx
            txa
            cmp NUMSECTORS
            bcc havenewidx
           ;sec
            beq :+
            clc; subtract one after wrap (supposedly due to large tail gap)
:           sbc NUMSECTORS
havenewidx: tax
            lda INDEXTABLE,x
            bpl specidxtkn
            tya
            ora #BLOCKPENDING | INDEXSPECULATIVE
            sta INDEXTABLE,x
            bne genspcloop; jmp

.if ::PLATFORM <> diskio::platform::COMMODORE_128
sendnibbletab:
            BIT0DEST = 3
            BIT1DEST = 1
            BIT2DEST = 2
            BIT3DEST = 0

            .repeat $10, I
                .byte (((~I >> 0) & 1) << BIT0DEST) | (((~I >> 1) & 1) << BIT1DEST) | (((~I >> 2) & 1) << BIT2DEST) | (((~I >> 3) & 1) << BIT3DEST)
            .endrep
.endif; ::PLATFORM <> diskio::platform::COMMODORE_128

            .assert * <= BLOCKBUFFER71, error, "***** 1571 drive code too large. *****"

dcodinit71: lda #ATNA_OUT | CLK_OUT; signal idle to the host with ATN_IN clear
            sta VIA1_PRB

           ;ldx ROMOS_TRACK_DIFF
            txa
            ldx #.lobyte(topofstack71)
            txs
            pha

            lda #T1_FREE_RUNNING | PA_LATCHING_ENABLE; watchdog IRQ: count phi2 pulses, 16-bit free-running,
            sta VIA2_ACR                             ; port a latching should not be needed here
                                                     ; (IC rather than discrete logic),
                                                     ; but it is enabled just to be sure
            lda #READ_MODE | BYTE_SYNC_ENABLE
            sta VIA2_PCR

            ; set seek boundaries according to number of disk sides
            lda #MAXTRACK71 + 1
            ldx #NUMTRACKS_A
c1570fix0:  bne :+
            lda #NUMTRACKS_SINGLESIDED + 1
            ldx #NUMTRACKS_SINGLESIDED
:           sta MAXTRACK
            stx MAXTRACK_A

.if ::PLATFORM = diskio::platform::COMMODORE_128
            lda #$05     ; default burst timing is $06, anything below $05 yields transfer
            sta CIA_TA_LO; errors, hard-wired setting for 1571CR's MOS5710 is also $05
           ;lda #$00
           ;sta CIA_TA_HI
.endif; ::PLATFORM = diskio::platform::COMMODORE_128

            ; watchdog initialisation
            lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS
            sta VIA1_IER; no IRQs from VIA 1
            sta VIA2_IER; no IRQs from VIA 2
            sta CIA_ICR; no IRQs from CIA
            bit CIA_ICR

TIMER_VAL = IRQ_SET_FLAGS | IRQ_TIMER_1; $c0
            ldx #TIMER_VAL
.if !DISABLE_WATCHDOG
            stx VIA2_IER; timer 1 IRQs from VIA 2
.else
            bit VIA2_IER; timer 1 IRQs from VIA 2
.endif
           ;ldx #$c0
            stx CLEARTRACKLINKTABLE
            stx NUMFILES71

            ; fade off the busy LED if lit
            lda #.lobyte(~MOTOR)
            and (.lobyte(VIA2_PRB - TIMER_VAL),x); VIA2_PRB
            sta (.lobyte(VIA2_PRB - TIMER_VAL),x); VIA2_PRB
            sta DISKCHANGEBUFFER; store light sensor state for disk removal detection
            and #BUSY_LED
            beq :+
            lda #$ff
:           sta LEDSTATE
:           jsr fadeled
            bne :-

            jsr two_mhz

            lda #MAXNUMSECTORS
            sta NUMSECTORS

            ldy #$ff; invalid track number -> no step but turn on motor
            pla; ROMOS_TRACK_DIFF
            bne stepperok; branch if the drive had already seeked before the loader has been started
            ; the drive was reset immediately before running the loader -
            ; step down a track: this works normally if the stepping bits are congruent with the stepper motor;
            ; however, it may happen that the bits are misaligned (opposite to the actual stepper position, bit 1
            ; reversed), this alone does not move the head but stepping makes it go into the direction opposite to
            ; the one desired when moving; the stepping down two halftracks will actually step up and step down one
            ; halftrack each and thus will end up on the same track as before, but align the stepper bits to the motor.
            ldy #$02
            sty CURRTRACK
            dey
stepperok:  jsr trackseek

            ldx #findtrkned - findtrkerr
:           lda findtrkerr - 1,x
            sta FINDTRACKN - 1,x
            dex
            bne :-

            .assert (FINDTRACKN + findtrkned - findtrkerr) <= enddirbuffer, error, "***** 1571 findtrackno too large. *****"

            stx REQUESTEDSECTOR71
            jmp findtrckno - findtrkerr + FINDTRACKN

findtrkerr: dec bitratecnt
            bne findtrknum
            lax VIA2_PRB
            axs #$0100 - (1 << BITRATE_SHIFT)
            stx VIA2_PRB; cycle through the 4 bit-rates

            dec ERRORCOUNT
            bne :+
            lda #OPC_RTS
            sta seekswitch
            jsr halftrkdwn
            jsr two_mhz
            lda #OPC_TAX
            sta seekswitch
findtrckno: lda #ERR
            sta ERRORCOUNT

:           lda #BITRATECOUNT
            sta bitratecnt

            ; find current track number
findtrknum: lda #SUCCESSCOUNT
            sta successcnt

:           jsr getblkscan; any sector, no block link sanity check, store ID
            bcs findtrkerr
            cmp CURRTRACK
            sta CURRTRACK
            bne findtrknum; on different track number, retry with same bit-rate
            jsr loadblock
            bcs findtrkerr
            dec successcnt; accept current track number only after some consecutive successful attempts
            bne :-

            lda #OPC_DEC_ZP
            sta chksumerr
            lda #OPC_BCC
            sta idswitch
            lda #OPC_LDX_ZP
            sta sanitychsw
            lda #OPC_BPL
            sta chkbusswit
            jsr initlinktb

            ldy #DIRTRACK
            jmp trseekidle
findtrkned:

drvcodeend71:

TRAMPOLINEOFFSET = $24; dgetrout - dinstall + 1

            .org * - TRAMPOLINEOFFSET

dinstall:   sei
            ldx ROMOS_TRACK_DIFF
            txs

            lda #.lobyte(~TWO_MHZ); transfer in slow mode in order to be
            and VIA1_PRA          ; less susceptible to interference by
            sta VIA1_PRA          ; any passive drives running at 1 MHz

            lda #ATNA_OUT | CLK_OUT
            sta VIA1_PRB
            lda #VIA_ATN_IN_INPUT | VIA_DEVICE_NUMBER_OUTPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT
            sta VIA1_DDRB
:           lda VIA1_PRB; wait for ATN_IN set and DATA_IN clear
            lsr
            bcs :-
            lda #ATNA_OUT
            sta VIA1_PRB

            ldx #.lobyte(drvcodebeg71 - 1)
dgetrout:   inx
            .assert * >= drvcodeend71, error, "***** 1571 trampoline too low in memory. *****"

            bne :+
            inc dgetputhi
:           DRIVEGETBYTE getbyteinstall; there is no watchdog while installing
dgetputhi = * + 2
            sta a:.hibyte(drvcodebeg71 - 1) << 8,x
            cpx #.lobyte(drvcodeend71 - 1)
            bne dgetrout
            dec drvcodebeg71
            bne dgetrout

            tsx; ROMOS_TRACK_DIFF
            jmp dcodinit71

drvprgend71:
            .assert * <= $0800, error, "***** 1571 drive code too large. *****"

            .reloc
