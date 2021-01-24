
.include "loader.inc"

.include "cpu.inc"
.include "via.inc"

.include "drives/drivecode-common.inc"

CURRTRACK              = $13
HEADERTRACK            = $17; HEADERTRACK = LINKSECTOR = DECODETEMP = BLOCKINDEX_STATUS
ID0                    = $18
ID1                    = $19
LOADEDSECTOR           = $1b; fixed
ERRORCOUNT             = $20; fixed
DISKCHANGEBUFFER       = $23
NUMSECTORSONTRACK      = $29; fixed
LEDSTATE               = $2b
CURRBLOCKINDEX         = $33
MAXCONFIRMEDBLKIDXPOS  = $37; fixed
NUMFILES41             = $38; fixed: OPC_SEC
SPECULATIVEINTERLEAVE  = $3b; fixed
FILETRACK              = $3e
FILESECTOR             = $3f
MAXCONFIRMEDBLOCKINDEX = $40
NUMFILEBLOCKS          = $42
NUMFILEBLOCKSONTRACK   = $43
NUMCONTIGUOUSBLOCKS    = $4e
NEXTCONTIGUOUSBLOCK    = $4f
BLOCKINDEXBASE         = $50
FILENAMEHASHLO         = $52
FILENAMEHASHHI         = $53
PREVBLOCKINDEXPLUS1    = $54; fixed

STACKBUFFER            = $56; 4 bytes

DIRBLOCKPOS            = readloop + 1
NEXTDIRBLOCKSECTOR     = nxtdirblks

CYCLESTARTENDSECTOR    = NEXTCONTIGUOUSBLOCK

CUSTOMCODEUPLOAD       = INDEXTABLE

DIRBUFFSIZE            = 9
FILENAMEHASHVALLO      = $44; length is DIRBUFFSIZE

REQUESTEDSECTOR41      = $66; fixed, must be INDEXTABLE - 1: see trackinit
INDEXTABLE             = $67; length is MAXNUMSECTORS = $15 bytes

FILENAME               = INDEXTABLE; max. 16 bytes

TRACKLINKTABLE41       = INDEXTABLE + MAXNUMSECTORS; length is MAXNUMSECTORS = $15 bytes
FILENAMEHASHVALHI      = TRACKLINKTABLE41 + MAXNUMSECTORS; length is DIRBUFFSIZE

LINKSECTOR             = HEADERTRACK
DECODETEMP             = HEADERTRACK
BLOCKINDEX_STATUS      = HEADERTRACK

LINKTRACK              = .lobyte(decode4 + 1)

TRACKINC               = .lobyte(decode6 + 1)
CURRSTEPSPEEDLOW       = .lobyte(decode1 + 1)

BLOCKBUFFER41          = $0100

DECODETABLE            = $0700; effectively starts at $0722

DIRTRACKS              = $0710
DIRSECTORS             = DIRTRACKS + DIRBUFFSIZE
NUMBLOCKS              = $0796

ROMOS_TRACK_DIFF       = $42

MAXINTERLEAVE          = 16
MAXNUMSECTORS          = 21
MAXTRACK41             = 41

TIMER                  = VIA2_T1C_H
TIMERLO                = VIA2_T1C_L
TIMER2                 = VIA1_T1C_H
TIMER2LO               = VIA1_T1C_L

INDEXSPECULATIVE       = %01000000
BLOCKPENDING           = %00100000

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

NMS = MAXNUMSECTORS; NUMSECTORS initial value
NFL = $ff          ; NUMFILES41 initial value
SIL = $08          ; SPECULATIVEINTERLEAVE initial value
RQS = $00          ; REQUESTEDSECTOR41 initial value

_00 = $0b; fixed
FE  = $22
EF  = $24; fixed, ~ATNA_OUT
_1F = $3a; fixed

T1  = TIMER
TI1 = $39
T2  = TIMER2 + $0100 + $90; $1995 rather than $1805: low-byte is NEGMAXNUMSECTORS, high-byte is CHECKLOADREQUEST41
TI2 = $2f
NEGMAXNUMSECTORS   = TI2 + 0
CHECKLOADREQUEST41 = TI2 + 1

V1  = VIA1_PRB
V1B = $07
V2  = VIA2_PRB
V2B = $0f

BR  = DECODETAB8
BRS = $27; fixed
D2  = DECODETAB2
DT2 = $01; fixed, JOBCODE0400/JOBCODE0500: positive, not interpreted as job code
CLEARTRACKLINKTABLE = DT2

ST  = STACK
STK = DT2 + 1; fixed, JOBCODE0500/JOBCODE0600: positive, not interpreted as job code
BL  = STACK - $1a; offset for number of file blocks in directory sector
BLP = DT2 - 1; fixed, JOBCODE0300: negative, but jobcodes are checked back to front, JOBCODE0400: positive, not interpreted as job code

ERR = $80; ERRORCOUNT reset value, $40 is too little (and only bits 7 and 6 may be non-0): would cause stepping when spinning up
ERT = $20; ERRORCOUNT retry value

BTR = 4; number of read attempts before bit-rate cycling on initial current track retrieval
SCN = 32; number of consecutive successful per-bit-rate attempts on initial current track retrieval, will be $ff after init as MAXCONFIRMEDBLKIDXPOS initial value
BITRATECOUNT = PREVBLOCKINDEXPLUS1
SUCCESSCOUNT = MAXCONFIRMEDBLKIDXPOS

___ = 184; will be initialised dynamically

            .org 0

drvcodebeg41:
            .byte .hibyte(drvcodeend41 - * + $0100 - $01); init transfer count hi-byte

            ;                         $d0 doubles as JOBCODE_JUMP to $0700 to handle the watchdog IRQ
            .byte      >BL, >D2, >ST, $d0, $60, $90, $20, >V1, $40, $f0, $00, $50, $30, $10, $f0; $00 is <BL after transfer
            .byte >V2, $8a, $df, ___, $cf, $2a, $4f, ___, ___, ___, $9f, ___, $8f, $4a, $0f, ___
            .byte ERR, $ca, $fe, ___, $ef, $6a, $6f, <BR, >BR, NMS, $3f, ___, $af, $1a, $2f, <T2
            .byte >T2, $9a, $5f, ___, $ff, $0a, $7f, SCN, NFL, <T1, $1f, SIL, $bf, $5a

findtrkerr: lax VIA2_PRB; retry counter for next bit-rate

            .byte      $da; nop

            dec BITRATECOUNT
            bne findtrknum
            axs #$0100 - (1 << BITRATE_SHIFT)
            stx VIA2_PRB; cycle through the 4 bit-rates
            lda #BTR

            .byte                                                                  $3a; nop

            sta BITRATECOUNT
            clc

            .byte      $ba; tsx

            bcc :+; jmp

            .byte                     BTR, $aa, ___, ___, ___, ___

todataread: ldx #.lobyte(stackend - 3)   ;  79             ; $04
            txs                          ;  81
            .byte                                                                  $7a; nop
            lda (V2A - $04,x) ; VIA2_PRA ;  89  94  98 101 ; 3:33334444 ; 2 - %11: [78..103], %10: [84..111], %01: [90..119], %00: [96..127]
            .byte OPC_ARR_IMM            ;                 ;   33333---
            .byte      $fa
            jmp dataread

            .byte                          $ea, RQS

:           dec ERRORCOUNT
            bne findtrknum

            asl seekswitch; OPC_BMI -> OPC_RTS
           ;clc
            ldx #$80 | (MINSTEPSPEED + 1)
            jsr halftrkdwn; resets ERRORCOUNT
            lsr seekswitch; OPC_RTS -> OPC_BMI

findtrknum: lda #SCN; number of attempts on initial current track retrieval
            sta SUCCESSCOUNT; reset attempt counter
:           jsr getblkscan; any sector, no block link sanity check, store ID
            bcc findtrkerr
            cmp CURRTRACK
            sta CURRTRACK
            bne findtrknum; on different track number, retry with same bit-rate
            jsr loadblock
            bcc findtrkerr
            dec SUCCESSCOUNT; accept current track number only after some consecutive successful attempts
            bne :-

            ldy CURRTRACK
            jsr initlinktb
            lda #OPC_LAX_ZP; $a7
            ldx #LOADEDSECTOR
            jmp toidleloop

            ;                       bit-rates %11 %10 %01 %00
readloop:   lda $00                        ;               8
            .byte OPC_LDA_ZPIY             ;           9  12 ; lda ($a8),y, ($a8) = $cbe0 (ROM), y >= $48
            tay                            ;       7  11  14 ; $a8
readloop11: lda DECODETAB5,x               ;   7  11  15  18 ;   455555-- -> $xy, partial
decode4:    adc $00                        ;  10             ;   ----4444 -> $x0
            pha                            ;  13
V2A = * + 1
            lda VIA2_PRA                   ;  17  21  25  28 ;   66677777   ; 4 - %11: [0..25], %10: [0..27], %01: [0..29], %00: [0..31]
_E0 = * + 1
            ldx #%11100000                 ;  19
            axs #$0100 - decoffs6          ;  21             ;   666-----
            and _1F                        ;  24  28  32  35 ;   ---77777
            tay                            ;  26
decode6:    lda DECODETAB6,x               ;  30             ;   666-5566 -> $x0 ; x = 666-----, lsb = ----5566
            eor DECODETAB7,y               ;  34             ;   ---77777 -> $0x
            pha                            ;  37
            lax VIA2_PRA                   ;  41  45  49  52 ;   00000111   ; 0 - %11: [26..51], %10: [28..55], %01: [30..59], %00: [32..63]
            eor #%11111000                 ;  43
            sax .lobyte(decode1 + 1)       ;  46             ;   -----111
            alr #%11111000                 ;  48             ;   -00000--
            tay                            ;  50
            ldx #%00111110                 ;  52
            lda VIA2_PRA                   ;  56  60  64  67 ;   11222223   ; 1 - %11: [52..77], %10: [56..83], %01: [60..89], %00: [64..95]
            sax .lobyte(decode2 + 1)       ;  59             ;   --22222-
            alr #%11000001                 ;  61             ;   -11-----:3
            tax                            ;  63
            lda DECODETAB0,y               ;  67             ;   -00000-- -> $x0, zeropage access
decode1:    eor DECODETAB1,x               ;  71             ;   -11--111 -> $0x, x = -11-----, lsb = -----111
            tsx                            ;  73
            pha                            ;  76
            beq todataread                 ;  78
ST1 = * + 1; $0101
            eor STACK + 1,x                ;  82
ST2 = * + 1; $0102
            eor STACK + 2,x                ;  86
ST3 = * + 1; $0103
            eor STACK + 3,x                ;  90
checksum:   eor #$00                       ;  92
readdata:   sta checksum + 1               ;  95
            lax VIA2_PRA                   ;  99 103 107 110 ; 3:33334444   ; 2 - %11: [78..103], %10: [84..111], %01: [90..119], %00: [96..127]
            arr #%11110000                 ; 101             ;   33333---
            tay                            ; 103
DT3 = * + 1
            adc DECODETAB3,y               ; 107             ;   33333--- -> $0x, sec
decode2:    eor $00                        ; 110             ;   --22222- -> $x0
            pha                            ; 113
            lda #%00001111                 ; 115
            sax decode4 + 1                ; 118             ;   ----4444
            ldx VIA2_PRA                   ; 122 126 130 133 ;   45555566   ; 3 - %11: [104..129], %10: [112..139], %01: [120..149], %00: [128..159]
            sax decode6 + 1                ; 125             ;   ----5566
            adc #%11111100 - %00001111 - 1 ; 127 131 135 138 ;              ; clv - %11: [104..129], %10: [112..139], %01: [120..149], %00: [128..159]
            axs #$00                       ; 129             ;   455555--   ; sec
                                           ;[130 140 150 160]
densityswt: bvs readloop11                 ;   3   2         ; is changed to bvc * for bit-rates %10, %01, %00
            bvs readloop11                 ;   3   5         ; branch to readloop + [0, 2, 3, 4]
bitrateswt   = * - 1
READLOOPBASE = .lobyte(readloop - *)
            bcs readloop11                 ;   3             ; jmp

nxtdirblks: .byte 0

            ; * = $0100

            .byte 0
            .byte 0; number of blocks (low-byte) of file #8 in loaded dir block
            .byte 0
stack:
            .assert stack >= (STACK + 3), error, "***** 1541 stack below $0103. *****"
            .assert stack <= (STACK + 3), error, "***** 1541 stack above $0103. *****"

            .word 0, 0
stackend:
topofstack41 = stackend - 1

            .assert stackend >= (STACK + 7), error, "***** 1541 top of stack below $0107. *****"
            .assert stackend <= (STACK + 7), error, "***** 1541 top of stack above $0107. *****"

stepperfix: ; here, the drive was apparently reset immediately before running the loader -
            ; step down a track: this works normally if the stepping bits are congruent with the stepper motor.
            ; however, it may happen that the bits are misaligned (opposite to the actual stepper position, bit 1
            ; reversed), this alone does not move the head but stepping makes it go into the direction opposite to
            ; the one desired when moving. the stepping down two halftracks will actually step up and step down one
            ; halftrack each and thus will end up on the same track as before, but align the stepper bits to the motor.
            ldy #$02
            sty CURRTRACK
            dey
            jsr trackseek

stepperok:  lda #OPC_STA_ZPX
            sta initlinklp
            ldy #DECTABOFFS
:           lda decodetab,y
            sta DECODETABLE,y
            iny
            bne :-
            jmp findtrknum

DECTABOFFS = $22
decodetab  = * & $ff00

decoffs0   = $0d
decoffs1   = $00
decoffs2   = $00
decoffs3   = $0f
decoffs5   = $04
decoffs6   = $04
decoffs7   = $77
decoffs8   = $a6

DECODETAB0 = decoffs0
DECODETAB1 = DECODETABLE + decoffs1
DECODETAB2 = decoffs2
DECODETAB3 = DECODETABLE + decoffs3
DECODETAB5 = DECODETABLE + decoffs5
DECODETAB6 = DECODETABLE + decoffs6
DECODETAB7 = DECODETABLE + decoffs7
DECODETAB8 = DECODETABLE + decoffs8

            .assert .lobyte(*) >= DECTABOFFS, error, "***** 'decodetab' data is below in-page offset $22. *****"
            .assert .lobyte(*) <= DECTABOFFS, error, "***** 'decodetab' data is above in-page offset $22. *****"

BR0 = READLOOPBASE + 0
BR1 = READLOOPBASE + 2
BR2 = READLOOPBASE + 3
BR3 = READLOOPBASE + 4

            .byte           $04, $00, $7f, $0a, $0c, $08, $9d, $8c, $72, $0c, $9e, $8c, $c6, $0c
            .byte $96, $8c, $e3, $0c
waitdone:   sei; disable watchdog
            lax NUMCONTIGUOUSBLOCKS
            SKIPBYTE_NOP
            .byte                                         $99
            bpl tonotstats
            SKIPBYTE
            .byte                                                             $9a
            inx
            bcc tostatusnt; jmp
            .byte $92, $74, $05, $01, $07, $03, $06, $02, $d8, $9c, $bc, $ac, $9c, $9c, $bc, $ac
            .byte $94, $9c, $bc, $ac
filenfsent: jmp fadeledidl; force fading off the busy LED on file not found
            .byte                                    $e8, $8f
senddone:   bcc toprepwait; jmp, entry
tosendstat: clc; entry
            .byte                                                             $98; tya
            bcc tosendsts2; jmp
            .byte                                                                            $df
            .byte $90, $c7, $0d, $09, $0f, $0b, $0e, $df, $95, $1c, $3c, $2c, $9b, $1c, $3c, $2c
            .byte $93, $1c, $3c, $2c
tostatusnt: bne filenfsent; ok: x = $00, file not found: x = $ff
            SKIPWORD
            .byte                                    $cc, $91
            jmp findfile; prepare next file, only after successful load
            .byte                                                             $97
tonotstats: bpl notstatus; jmp
            .byte                                                                            $c3
            .byte $62, $63, $6b, $0d, $66, $67, $6f, $c3, $a8, $61, $69, $a2, $6c, $65, $6d, $10
            .byte $6a, $60, $68, $9c, $6e, $64
totrskidle:;lda #OPC_LAX_ZP; $a7
           ;ldx #OPC_BPL   ; $10
            sta sanitychsw ; $0796..$079e:
            stx chkbusswit ; 9 bytes used
            jmp trseekidle ; for NUMBLOCKS
            .byte                                                                            $a2
waitsent:   lda (.lobyte(V1B - $ef),x); VIA1_PRB, wait for ATN_IN set,
            bpl waitsent              ; acknowledgement of the block transfer
            bmi waitdone; jmp
            .byte                               BR0, $a2, $1d, $cc, $6c, $4c, $1e, $cc, $6c, $4c
            .byte $16, $cc, $6c
tosendsts4: .byte                $4c, <sendstatus, >sendstatus; jmp sendstatus
            .byte                               BR1, $97, $19
tosendsts3: sty blocksize; y < blocksize: send over one byte
            .byte                                                             $1a; nop
            iny
            SKIPWORD
            .byte                                                                            $86
            .byte $12
            dec prpnxtfjmp; OPC_EOR_ABS -> OPC_JMP_ABS
            bcc tosendsts4; jmp
            .byte                               BR2, $86, $27, $dc, $fc, $ec, $1c, $dc, $fc, $ec
            .byte $14, $dc, $fc, $ec
toprepwait: bcc prepwait; jmp
            .byte                               BR3, $70, $0f
tosendsts2: dey; y must be negative
            sty NUMCONTIGUOUSBLOCKS; y negative: status flag
            .byte                                                             $18; clc
            bcc tosendsts3; jmp
            .byte                                                                            $62
            .byte $10
prepwait:   lda #ATNA_OUT | CLK_OUT; drive busy
            sta (.lobyte(V1B - $ef),x); VIA1_PRB
            bne waitsent; jmp
            .byte                                    $62, $15, $5c, $7c, $a8, $1b, $5c, $7c, $e3
            .byte $13, $5c, $7c
.if WORK_AROUND_1541U_BUGS
notstatus:  cmp NUMFILEBLOCKSONTRACK
            clc
            SKIPWORD_NOP
            .byte                                    $54, $11; is changed to $55 (eor zp,x) with 1541U arr #imm bug
.else; !WORK_AROUND_1541U_BUGS
            nop; spare
notstatus:  cmp NUMFILEBLOCKSONTRACK
            clc
            .byte                                    $54, $11; nop zp,x
.endif; !WORK_AROUND_1541U_BUGS
            jmp blocksent
            .byte                                                             $17, $2f, $9e, $46

            FNAMEHASH 1541

            ; y = destination track must be a valid track number
trackseek:  lda #MOTOR; turn on the motor
trackseekx: ldx #$80 | (MINSTEPSPEED + 1)
trackstep:  sax CURRSTEPSPEEDLOW; stores $0x
            jsr bitsetv2b
            cpy CURRTRACK
            beq setbitrate; branch if on same track
nexttrack:  bcc :+
            inc CURRTRACK; move up (inwards)
            SKIPWORD
:           dec CURRTRACK; move down (outwards)
halftrkdwn: lda #ERR | (MOTOR >> 2); = $81, $40 is too little (and only bits 7, 6 and 1 may be non-0): would cause stepping when spinning up
            sta ERRORCOUNT
            rol
            asl
halftrack:  sta TRACKINC; $04: move down (outwards), $06: move up (inwards)
            eor VIA2_PRB
           ;clc
            jsr exectrseek
            txa
            cmp #($80 | SINGLESTEPSPEED) - 1
            beq initlinktb; stepping to adjacent track: branch if second half-track step has been issued
            stx TIMER; reset track-step timer
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
            tax
           ;sec
            lda #$7f
            adc TRACKINC
seekswitch: bmi halftrack
            cpy CURRTRACK
            bne nexttrack; branch if not on destination track

initlinktb: lsr CLEARTRACKLINKTABLE
            bcs setbitrate
            lax NEGMAXNUMSECTORS; MAXNUMSECTORS + $80
            dex
initlinklp: lda .lobyte(TRACKLINKTABLE41 - $80),x; is changed to sta .lobyte(TRACKLINKTABLE41 - $80),x after init, sector links are unknown
            dex
            bmi initlinklp

            ; bit-rates:
            ; tracks 31+   (17 blocks): %00 - sector interleave 3 (lowest density, slowest clock, innermost tracks)
            ; tracks 25-30 (18 blocks): %01 - sector interleave 3
            ; tracks 18-24 (19 blocks): %10 - sector interleave 3
            ; tracks  1-17 (21 blocks): %11 - sector interleave 4 (highest density, fastest clock, outermost tracks)
setbitrate: lax _E0; $e0 = SYNC_MARK | BITRATE_MASK
            ora (.lobyte(V2B - $e0),x); VIA2_PRB
           ;ldy CURRTRACK
            jsr getnumscts
            inx
            stx NUMSECTORSONTRACK
            SKIPWORD
motrledoff: and #.lobyte(~(SYNC_MARK | BUSY_LED | MOTOR))
            ldy #$00
store_via2: sta VIA2_PRB; store_via2 must not clobber y, see trackseek/exectrseek
            rts

            ; the raw <-> serial mapping swaps bits 0 and 3 of both nibbles and
            ; inverts the result, so it is same for both encoding and decoding
sertorawd:  dey
sertorawr:  lda (STK),y; STACK
sertoraw:
rawtoser:   sta DECODETEMP
            lsr
            lsr
            alr #%00100010
            eor DECODETEMP; bit 3 ^ bit 0
            and #%00010001; if result is 0: both are equal (no swap required)
           ;clc
            adc #%01110111; 0 -> 7, 1 -> 8
            ora #%01100110; 0 -> 7, 1 -> e
            eor #%10001000; 0 -> f, 1 -> 6
            eor DECODETEMP
            sta HEADERTRACK; = LINKSECTOR = DECODETEMP = BLOCKINDEX_STATUS
           ;clc
            rts

            .assert * >= $02a9, error, "***** 1571 watchdog IRQ vector in 1541 drivecode located below $02a9. *****"
            .assert * <= $02a9, error, "***** 1571 watchdog IRQ vector in 1541 drivecode located above $02a9. *****"

            .word uninstall; relevant for option ONLY_1541_AND_COMPATIBLE

getnumscts: ldx #21 - 1; number of blocks minus 1
            cpy #18
            bcc :++; bit-rate $60
            dex; 19 - 1
            dex
            cpy #25
            bcc :+ ; bit-rate $40
            dex; 18 - 1
            and #.lobyte(~(%10 << BITRATE_SHIFT)); -$40
            cpy #31
            bcc :++; bit-rate $20
            dex; 17 - 1
:           and #.lobyte(~(%01 << BITRATE_SHIFT)); -$20
:           rts

fadeled:
BUSY_LED_MASK = ~BUSY_LED
            ldx #.lobyte(BUSY_LED_MASK)
            lda (.lobyte(V2B - BUSY_LED_MASK),x); VIA2_PRB
            ldy LEDSTATE
            beq motrledoff

:           dey
            bne :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in LED fade loop. *****"

            sax (.lobyte(V2B - BUSY_LED_MASK),x); VIA2_PRB
            ldy LEDSTATE
:           iny
            bne :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in LED fade loop. *****"

            dec LEDSTATE

bitsetv2b:  ora VIA2_PRB; bitsetv2b must not clobber y, see trackseek
            bne store_via2; jmp

dataread:   ; decode checksum
            tay
            adc (DT3),y                ; DECODETAB3      ;   33333--- -> $0x, sec
            ldy decode2 + 1            ;                 ;   --22222-
            eor (DT2),y                ; DECODETAB2      ;   --22222- -> $x0
            tay                        ; ID0
            eor (.lobyte(STK - $04),x) ; STACK + 0, ID1
            eor (ST1 - $04,x)          ; STACK + 1, track
            eor (ST2 - $04,x)          ; STACK + 2, sector
            eor (ST3 - $04,x)          ; STACK + 3, checksum
            eor checksum + 1           ; is 0 with header
            cmp #$ff; checksum

            bit LOADEDSECTOR
            bpl blockread

            ; process block header
            bcc bccchksume

            ; header: checksum sector track ID1 ID0
            lda (.lobyte(STK - $04),x) ; STACK + 0, ID1
            cpy ID0
            bne :+
            cmp ID1
            beq :++
:           ldx REQUESTEDSECTOR41
            bne readerror; set ID only when reading the BAM sector
            sty ID0
            sta ID1

:           ldy #$02
            jsr sertorawr; STACK + $02, sector
            cmp NUMSECTORSONTRACK; check if sector number is within range of
            bcs bcschksume       ; the allowed sector numbers for the current track
            sta LOADEDSECTOR

            jsr sertorawd; STACK + $01, track
            beq chksumerr
            cmp #MAXTRACK41 + 1
bcschksume: bcs chksumerr

trkchkswtc: sec; is changed to lax LOADEDSECTOR after init
            rts
            eor REQUESTEDSECTOR41
            beq loadblock; branch if requested sector
            bpl findblkhdr; specific sector requested but not reached yet

            ; negative value: no specific sector requested, out-of-order sector fetch
           ;ldx LOADEDSECTOR
            lda INDEXTABLE,x
            ldy TRACKLINKTABLE41,x
            ldx #.lobyte(~(BLOCKPENDING | INDEXSPECULATIVE))
            sax CURRBLOCKINDEX
            asl
            iny; with negative INDEXTABLE value, branch to findblkhdr if block index is unknown but linked block is known (the block has been loaded before already)
            bcs :+; branch if block index not known
            asl; n flag = BLOCKPENDING, block index is known, or speculated to belong to the file
:           bpl findblkhdr; with positive INDEXTABLE value, branch if the block has already been loaded into the computer's memory (BLOCKPENDING not set)

loadblock:  tsx; $04
            pla; return address lo
            sta STACKBUFFER + 2
            pla; return address hi
            sta STACKBUFFER + 3
            txs
            jsr waitsync    ; 16 ; returns with x = 0 for txs : pha
            bpl chksumerr   ; 18 ; branch if not block data
            txs             ; 20
toreaddata: ldx #%00111110  ; 22/24/21
            bvc *           ;  2

            .assert .hibyte(*) = .hibyte(* - 2), error, "***** Page boundary crossing in bytesync loop. *****"

            pha             ;  5 ;            ; set stackpointer for readdata
            sax decode2 + 1 ;  8 ;   --22222-
            alr #%00000001  ; 10 ;   --------:3
            jmp readdata    ; 13 ; will jump to dataread
           ;lda VIA2_PRA    ; 20 ; 3:33334444 ; 2 - cycle 20 is -5 in  [0..25]
           ;lda VIA2_PRA    ; 43 ;   45555566 ; 3 - cycle 43 is -8 in [32..51]
           ;adc #$ec        ; 48 ;   45555566 ;     cycle 48 is -3 in [32..51]
           ;axs #$00        ; 50 ;   455555-- ;     cycle 50 is -1 in [32..51]

blockread:  ; LINKTRACK must not be 0 or negative, see nostep
           ;stx LINKTRACK; = decode4 + 1, which is non-0 and positive at this point

            ; swap stack with loaded data
:           ldy stack - 1,x
            lda STACKBUFFER - 1,x
            sta stack - 1,x
            sty STACKBUFFER - 1,x
            dex
            bne :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in swap stack <-> data loop. *****"

bccchksume: bcc chksumerr; branch on checksum mismatch

            lax HEADERTRACK
            eor CURRTRACK
            beq sanitychck

            ; track error
            ldy CURRTRACK
            stx CURRTRACK
            inc CLEARTRACKLINKTABLE; is reset in trackseek
            bne getblkseek; jmp

linkerror:  asl INDEXTABLE,x
            bmi readerror; branch if INDEXSPECULATIVE is set: clear index, set MSB
            ror INDEXTABLE,x; restore non-speculative index
            SKIPWORD; skip to NUMFILES41 = sec
:           sec            ; refill directory buffer when disk has been changed
            ror NUMFILES41 ; NUMFILES41 = $38 = OPC_SEC
readerror:  ; sync timeout, ID mismatch or illegal track or sector (invalid track/sector link)
checkchg:   ; must not change y
            ldx #.lobyte(stackend - 3); $04
            txs
            lax (V2B - $04,x); VIA2_PRB, check light sensor for disk removal
            eor DISKCHANGEBUFFER
            stx DISKCHANGEBUFFER
            alr #WRITE_PROTECT; clc
            bne :-
            ; read error: z = 1, c = 0
            rts

getblkdir:  sta REQUESTEDSECTOR41
            ldy #DIRTRACK
getblkseek: jsr trackseek; stores the number of blocks on the current track in NUMSECTORSONTRACK
getblkscan: ; the disk spins at approximately 300 rpm, so a revolution takes about 1,000,000 * 60 / 300 = 200,000 cycles,
            ; so the timeout counter cannot be set to one revolution: it is reset upon waiting for every new sync,
            ; thus a timeout only indicates a sync-less track range (about 65536 / 200,000 * 19 = 6.23 sectors), but missing
            ; sectors or similar will leave the loader spinning forever
findblkhdr: jsr waitsync
            sty LOADEDSECTOR ; 19 ; y is negative: header/data flag
            bpl toreaddata   ; 22 ; branch if block header
            jsr waitsync     ; 16
            bpl toreaddata   ; 19 ; branch if block header
            ; fall through if not a block header
chksumerr:  ; with repeated checksum errors, the r/w head might have landed between tracks or on the wrong track
            ldx #ERT
            lda (.lobyte(V2B - ERT),x); VIA2_PRB
            anc #BITRATE | BUSY_LED | TRACK_STEP; anc is okay, as this code is not reached with 1541U
chksumswtc: lda ERRORCOUNT; is changed to dec ERRORCOUNT after init
            bne chksumerrt
            stx ERRORCOUNT
           ;clc
            sbc #(1 << BITRATE_SHIFT) - 1; cycle to the next bit-rate (denser until wrap)
exectrseek: adc #$03; and step down half a track on bit-rate wrap
            ora #MOTOR
            jmp store_via2

sanitychck: tay
           ;sec
sanitychsw: .byte OPC_RTS, LOADEDSECTOR; is changed to lax LOADEDSECTOR after init
            lda TRACKLINKTABLE41,x
            bpl returnokz; branch if valid block link known already, no sanity check required, save some
                         ; cycles to catch next block in time when not transferring the currently loaded block
            jsr sertorawd
           ;ldy #$ff
            sty TRACKLINKTABLE41,x; mark linked block as known
            sta LINKTRACK
            jsr sertorawd; sector link or block size
           ;sta LINKSECTOR

            ; block link sanity check
            cmp #$02
            ldy LINKTRACK
            beq chklastbsz; branch if last block
            cpy #MAXTRACK41 + 1; check whether track link is within the valid range
            bcs linkerror

linktrkok:  jsr getnumscts; get number of sectors on linked track
            cpx LINKSECTOR; check whether sector link is within the valid range
chklastbsz: ; branch if invalid block size (0..1 = 1..2 bytes)
            bcc linkerror; branch if sector number too large

           ;lda LINKSECTOR   ; return the loaded block's sector link sector number or block size
            ldx LOADEDSECTOR ; return the loaded block's sector number
           ;ldy LINKTRACK    ; return the loader block's sector link track number
            cpy CURRTRACK
            bne returnok
            cmp LOADEDSECTOR       ; block must not link to itself
            beq linkerror          ; (but larger cycles are not detected)
            sta TRACKLINKTABLE41,x ; set block link
returnokz:  bit _00; $00: z flag must be set (linked block is on same track)
returnok:   sec; operation successful
chksumerrt: rts

waitsynclp: lda (.lobyte(TI1 - $fe),x); TIMER
            beq readerror
            lda (.lobyte(V1B - $fe),x); VIA1_PRB
chkbusswit: .byte OPC_BIT_ZP, bplchckbus - (* + 1); is changed to bpl bplchckbus after init: no watchdog, go to reset routine on ATN_IN clear
            ora CHECKLOADREQUEST41 ; check for DATA_IN clear
            lsr                    ; when collecting links,
            bcs :+                 ; branch on no load request
            bcc checkchg; jmp

waitsync:   ldx #$fe
            stx TIMER; reset sync time-out
:           lda VIA2_PRB; wait for SYNC, could be lda (.lobyte(V2B - $fe),x), but better keep this loop tight for faster response
            bmi waitsynclp ; -XX43210

            alr #%01100000 ; --XX----, clc
            tay
            lda (BRS),y
            sta bitrateswt
            sbc #BR3 - 1   ; clv
            ldy #OPC_BVS
            lda #BR3 + 2
            bcs :+
            ldy #OPC_BVC; bit-rates %10, %01, %00
            txa; $fe: bvc *
:           cmp VIA2_PRA   ; clear latch, reads $ff and then $52 (header) or $55 (data)
            sty .lobyte(densityswt - $fe),x
            tay
            inx
            bvc *          ;  2

            .assert .hibyte(*) = .hibyte(* - 2), error, "***** Page boundary crossing in bytesync loop. *****"

            clv            ;  4
            bne :-         ;  6
            lda VIA2_PRA   ; 10 ; 11222223, reads %-H222223 (header) or %HH222223 (data)
                           ;    ; could be lda (V2A,x), but needs to be tight for low cycle count until next bytesync
            rts            ; 16

            ; validate block indices according to currently-known links
indexloop:  tax; link sector
            iny; block index
            tya
            eor INDEXTABLE,x
            eor #INDEXSPECULATIVE
            beq confirmspc; branch if already-loaded speculative index matches
            eor #BLOCKPENDING
            beq confirmidx; branch if not-yet-loaded speculative index matches
speculinit: clc ; mis-speculated block index detected
confirmidx: tya
            ora #BLOCKPENDING
confirmspc: sta INDEXTABLE,x
specmanage: lda TRACKLINKTABLE41,x; linked sector
            bpl indexloop; branch if there is a linked block on the same track

            .assert .hibyte(*) = .hibyte(indexloop), error, "***** Page boundary crossing in indexloop. *****"

            lda PREVBLOCKINDEXPLUS1
            beq returnok

            stx MAXCONFIRMEDBLKIDXPOS
            sty MAXCONFIRMEDBLOCKINDEX
bcsretrnok: bcs returnok

            ; mis-speculated block indices detected: rebuild speculative block index table
            lda BLOCKINDEXBASE
            beq returnok; do not speculate for the first file track, as this likely leads to much mis-speculation and ultimately lower speed

           ;clc; minus one block: do not transfer alleged final block, as it may be mis-speculated and load beyond the actual file end address
            lda NUMFILEBLOCKS
            sbc BLOCKINDEXBASE
            bcc returnok; limit to alleged number of file blocks
            cmp NUMSECTORSONTRACK
            ldx NUMSECTORSONTRACK
            bcc :+
            txa
:           sta genspeclim

clearidxtb: lda INDEXTABLE - 1,x
            bmi :+; branch if index clear already
            asl
            bpl :+; branch if INDEXSPECULATIVE not set
            sta INDEXTABLE - 1,x; set MSB: clear index
:           dex
            bne clearidxtb

            .assert .hibyte(*) = .hibyte(clearidxtb), error, "***** Page boundary crossing in clear index table loop. *****"

            ldx MAXCONFIRMEDBLKIDXPOS
           ;ldy MAXCONFIRMEDBLOCKINDEX
genspcloop: iny
genspeclim = * + 1
            cpy #0
            bcs bcsretrnok
            lda TRACKLINKTABLE41,x
            bpl havenewidx; branch if there is a linked block on the same track
            txa
           ;clc
            adc SPECULATIVEINTERLEAVE
            SKIPWORD
specidxtkn: inx
            txa
            cmp NUMSECTORSONTRACK
            bcc havenewidx
           ;sec
            beq :+
            clc; subtract one after wrap (supposedly due to large tail gap)
:           sbc NUMSECTORSONTRACK
havenewidx: tax
            lda INDEXTABLE,x
            bpl specidxtkn
            tya
            ora #BLOCKPENDING | INDEXSPECULATIVE
            sta INDEXTABLE,x
            bne genspcloop; jmp

            .assert .hibyte(*) = .hibyte(genspcloop), error, "***** Page boundary crossing in genspcloop. *****"

            PREPARE_NEXT_FILE 1541 ; jsr trackseek
idleloop41: jsr checkchg; check light sensor for disk removal

            lda REQUESTEDSECTOR41
            bpl havelinks

            ; collect links
            ldx NUMSECTORSONTRACK
           ;clc
checklinks: sta INDEXTABLE - 1,x
            ldy TRACKLINKTABLE41 - 1,x
            iny
            bpl :+
            sec; link not known
:           dex
            bne checklinks
            bcc havelinks

            dec CHECKLOADREQUEST41; $18: check DATA_IN clear for early exit upon load request
            jsr getblkscan
            inc CHECKLOADREQUEST41; $19
bplchckbus: bpl checkbus; jmp

            ; get custom drive code
getcustom: ;ldx #$00
:           dex
            sty .lobyte(readloop - $0100 - 3),x
            ldy customend - $0100,x
            bne :-
CUSTOM_BASE = CUSTOMCODEUPLOAD - (CUSTOM_LOW + 2)
            dec .lobyte(CUSTOM_BASE + customloop); OPC_ORA_ZPIY -> OPC_BPL
           ;lda #CLK_IN
            sta (V1B),y; VIA1_PRB, clear ATNA_OUT: sets DATA_OUT to signal ready for code upload
:           and (V1B),y; VIA1_PRB, wait for CLK_IN clear
            bne :-
            lda #OPC_STA_ZPXI | ATNA_OUT; $91
            sta (V1B),y; VIA1_PRB, set ATNA_OUT
            .assert .lobyte(CUSTOM_LOW - customend) <= $cf, error, "***** custom code upload too large. *****"
            .assert .lobyte(CUSTOM_LOW - customend) >= $cf, error, "***** custom code upload too small. *****"
           ;ldx #$cf
            sax .lobyte(CUSTOM_BASE + getbyterts); ($91 & $cf) = $81 = OPC_STA_ZPXI
            ldx #5
CUSTOMPARAM = CUSTOM_BASE + customparm
:           jsr dgetbyte; no watchdog
            sta .lobyte(CUSTOMPARAM),x
            dex
            bpl :-
            jmp .lobyte(CUSTOM_BASE + dgetbyte)

:           inc CURRTRACK
uninstall:  ldy #18; ROM dir track
            jsr trackseek
           ;lda VIA2_PRB
            bit ST2; the stepper bits are set to %00 after reset: ensure full-track
            bne :- ; stepper alignment (may still be off by a half-track)
           ;ldy #$00
            dey
            sty LEDSTATE
            dec reset; OPC_ADC_ABS -> OPC_JMP_ABSI

fadeledidl: jsr fadeled   ; if the LED is off, it will remain off and not be faded,
            bne fadeledidl; but the loop is executed regardless and the motor switched off eventually
reset:      adc RESET_VECTOR

havelinks:  lsr REQUESTEDSECTOR41
            jsr fadeled

checkbus:   ldx #0
            lda (V1B,x); VIA1_PRB
            ora (V1B,x); VIA1_PRB, to be safe, read a second time
            bpl uninstall
            lsr
            bcs idleloop41; wait until there is something to do

            GET_FILENAME 1541

            ; matches against hash of filename in FILENAMEHASHLO/HI
            NUMFILES = NUMFILES41
            FIND_FILE 1541; sets y to file track

            lda NUMBLOCKS,x
            sta NUMFILEBLOCKS
            lda FILENAMEHASHVALLO + 1,x ; PREPARE_NEXT_FILE
            sta FILENAMEHASHLO          ; functionality
            lda FILENAMEHASHVALHI + 1,x ; store hash of next file's
            sta FILENAMEHASHHI          ; name for loadnext
            lda DIRSECTORS,x
            sta MAXCONFIRMEDBLKIDXPOS
            sta FILESECTOR

            lax _00; $00
            dex
            stx LEDSTATE
            sta PREVBLOCKINDEXPLUS1
            sta MAXCONFIRMEDBLOCKINDEX
trackloop:  sta BLOCKINDEXBASE
            lda #BUSY_LED | MOTOR
            jsr trackseekx
            ; accu contains a negative number
            ; x contains the number of sectors on this track
trackinit:  sta INDEXTABLE - 1,x; sector indices are unknown
            dex
            bpl trackinit; sets REQUESTEDSECTOR41 = INDEXTABLE - 1 with x = 0

            .assert .hibyte(*) = .hibyte(trackinit), error, "***** Page boundary crossing in trackinit loop. *****"

            stx NUMFILEBLOCKSONTRACK
            ldx FILESECTOR
            cpx NUMSECTORSONTRACK; check whether sector link is within the valid range, skip files with invalid start sector
            bcs findloop         ; this is only done here because of loop files with negative start sectors denoting offset to referenced dir entry
            stx NEXTCONTIGUOUSBLOCK
           ;ldy #$00; initial block index
            sty NUMCONTIGUOUSBLOCKS
            jsr speculinit; set non-speculative block indices according to known links, build initial speculative block index table for this track

blockloop:  jsr getblkscan
            bcc blockloop; branch if block fetch not successful

            ldy CURRBLOCKINDEX
            bmi blockloop; branch if block conceivably not belonging to the file
            beq refuspecil; if first file block on track: skip PREVBLOCKINDEXPLUS1 check, cannot determine likely interleave

           ;ldx LOADEDSECTOR
            ldy PREVBLOCKINDEXPLUS1; first file block must be loaded first in order to know the load address to be able to place successive
            beq idxinvalid         ; blocks in host memory, invalidate index to avoid skipping over every other block in a livelock

           ;sec
           ;lda LINKSECTOR
            sbc LOADEDSECTOR; determine likely interleave
            tay
            beq refuspecil
            eor #$ff
           ;sec
            adc LOADEDSECTOR
            bcs :+
            adc NUMSECTORSONTRACK
:           tax
            lda TRACKLINKTABLE41,x
            cmp LOADEDSECTOR
            bne refuspecil; only accept interleave if distances to this block and the next match
            cpy #MAXINTERLEAVE + 1; validate
            bcs refuspecil        ; speculative interleave
            sty SPECULATIVEINTERLEAVE

refuspecil: ldx MAXCONFIRMEDBLKIDXPOS
            ldy MAXCONFIRMEDBLOCKINDEX
            sec; set no mis-speculation detected flag
            jsr specmanage

            ldx LOADEDSECTOR
            lda INDEXTABLE,x
            and #.lobyte(~BLOCKPENDING)
            sta INDEXTABLE,x
           ;sec
            sbc #INDEXSPECULATIVE

            ; c = 1 if index speculative, 0 otherwise
            ldy TRACKLINKTABLE41,x
            bpl notrklink

            ; block links to another track or is the file's last one
            ldy CURRBLOCKINDEX
            iny
            bcc settrklink; branch if block was not loaded speculatively
            bit NUMFILEBLOCKSONTRACK
            bpl idxinvalid; branch if track link has been set already
            cpy NUMSECTORSONTRACK
            bcs settrklink; do not branch if possibly not file's last block on track

idxinvalid: sec
            ror INDEXTABLE,x; clear index, set MSB

blocksent:  bne blockloop

           ;clc
           ;lda NUMFILEBLOCKSONTRACK
            adc BLOCKINDEXBASE

            ldy FILETRACK
            bne trackloop

            ; loading is finished
            ; $00 = diskio::status::OK
            jmp DECODETABLE + .lobyte(tosendstat)

settrklink: sty NUMFILEBLOCKSONTRACK
            ldy LINKTRACK
            sty FILETRACK
            ldy LINKSECTOR
            sty FILESECTOR; first sector on the next track

notrklink:  bcc sendblock; branch if block index is not speculative
            cmp NUMFILEBLOCKSONTRACK
            bcs idxinvalid; do not transfer block if speculative index is out of range

sendblock:  ldx NEXTCONTIGUOUSBLOCK
:           lda INDEXTABLE,x
            cmp #$1f
            bcs :+; branch if block has not been confirmed and transferred already
            inc NUMCONTIGUOUSBLOCKS
            lda TRACKLINKTABLE41,x
            tax
            bpl :-; branch if there is a linked block on the same track

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in contiguous blocks update loop. *****"

:           stx NEXTCONTIGUOUSBLOCK

            lda NUMCONTIGUOUSBLOCKS
            eor NUMFILEBLOCKSONTRACK
            bne nostep
            ldy FILETRACK
            beq nostep

            ; perform Shrydar Stepping (R)(TM) to minimise single-track stepping overhead:
            ; nudge the R/W head in the right direction, then wait a few bycles while it gains momentum,
            ; then enable the destination track's stepper magnet long before the head has reached the intermediate half-tracks magnet,
            ; relying on the head's inertia, then send over the block while the head keeps moving beyond the intermediate half-tracks stepper magnet
            ; and ultimately settles on the destination track.
            ; sending the block over takes at least 72 bycles
            ldx #$80 | SINGLESTEPSPEED
            jsr trackstep

nostep:     sec
            lax BLOCKINDEXBASE
            adc NUMCONTIGUOUSBLOCKS
            ldy #$01
            dec LINKTRACK
            bpl :+
            ; handle file's last block
            tya
           ;clc
            sbc LINKSECTOR; the file's last block's length (last byte offset)
            tay
:           sty blocksize
            jsr rawtoser
            ldy #$fe
            sta (STK),y; STACK + $fe, next contiguous block index/block size

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
sendstatus: iny; block: $fe -> $ff, status ok: $00 -> $01, file not found: $ff -> $00
            jsr rawtoser
           ;sta BLOCKINDEX_STATUS

            ; restore block buffer on stack
            ldx #4
:           lda STACKBUFFER - 1,x
            sta stack - 1,x
            dex
            bne :-

            ; send block ready signal and wait for the signal to begin transferring the block
            lax EF; .lobyte(~ATNA_OUT) = $ef
            sta (.lobyte(TI2 - $ef),x); TIMER2, poll two cascaded timers: an extra-long time-out period is needed since
            sta (.lobyte(TI1 - $ef),x); TIMER,  the host computer may still be busy decompressing a large chunk of data
            lsr; clear CLK_OUT, set DATA_OUT via ATNA_OUT clear as signal of presence
            sta (.lobyte(V1B - $ef),x); VIA1_PRB, block ready signal
waitready:  lda TIMER2LO; see if the watchdog barked
            cmp TIMERLO
.if DISABLE_WATCHDOG
            bmi :+
:
.else
            bmi timeout; and trigger watchdog on time-out
.endif
            bit VIA1_PRB; could be lda (.lobyte(V1B - $ef),x), but better keep this loop tight for faster response
            bmi waitready; wait for ATN_IN clear

            .assert .hibyte(*) = .hibyte(waitready), error, "***** Page boundary crossing in block send wait loop. *****"

            stx TIMER; reset watchdog time-out and clear possibly set IRQ flag
timeout:    ENABLE_WATCHDOG

            lda BLOCKINDEX_STATUS
            sax (.lobyte(V1B - $ef),x); VIA1_PRB
                               ;           v v
                               ; 4 -   465-0213
sendloop:   asl                ; 2 - 4:6570213-
            ora #ATNA_OUT      ; 2 - 4:657H213-, next bit pair will be transferred with ATN_IN set
                               ; = 18 (17 + 1 spare cycle)

:           bit VIA1_PRB       ; 4 - sync 1: wait for ATN_IN set
            bpl :-             ; 2 -       v v
            sta VIA1_PRB       ; 4 - 4:657H213-
            rol                ; 2 - 6:57H213-4
            rol                ; 2 - 5:7H213-46
            rol                ; 2 - 7:H213-465
            rol                ; 2 - H:213-4657
                               ; = 18

:           bit VIA1_PRB       ; 4 - sync 2: wait for ATN_IN clear
            bmi :-             ; 2 -       v v
            sta VIA1_PRB       ; 4 -   213-4657
            asl                ; 2 -   13-4657-
            ora #ATNA_OUT      ; 2 -   13-H657-, next bit pair will be transferred with ATN_IN set
            stx TIMER          ; 4 - reset watchdog time-out
                               ; = 18

:           bit VIA1_PRB       ; 4 - sync 3: wait for ATN_IN set
            bpl :-             ; 2 -       v v
            sta VIA1_PRB       ; 4 -   13-H657-
blocksize = * + 1
            cpy #$00           ; 2
            dey                ; 2
            lda STACK,y        ; 4 -   46570213
                               ; = 18
CUSTOM_LOW = * + 1
:           bit VIA1_PRB       ; 4 - sync 4: wait for ATN_IN clear
            bmi :-             ; 2 -       v v
            sax VIA1_PRB + $10 ; 4 -   465-0213, low-byte must not be 0 due to custom code upload
            bcs sendloop       ; 3
                               ; = 72

            .assert .hibyte(*) = .hibyte(sendloop), error, "***** Page boundary crossing in sendloop. *****"

            bcc DECODETABLE + .lobyte(senddone); jmp

dgetbyte:   DRIVEGETBYTE getbyte
getbyterts: rts; is changed to sta (zp,x) for custom drive code upload
            .byte .lobyte(CUSTOMPARAM + $04 - $ff); x = $ff
            inc .lobyte(CUSTOMPARAM + $04)

            .assert * >= $0700, error, "***** 1541 watchdog handler below $0700. *****"
            .assert * <= $0700, error, "***** 1541 watchdog handler above $0700. *****"

            bne :+
            inc .lobyte(CUSTOMPARAM + $05)
:           dec .lobyte(CUSTOMPARAM + $02)
            bne customloop
            dec .lobyte(CUSTOMPARAM + $03)
customloop: .byte OPC_ORA_ZPIY, .lobyte(dgetbyte - (* + 1)); is changed to bpl dgetbyte for custom drive code upload
           ;ldx #$ff
            txs
customparm = * + 1
            jmp uninstall
customend:
            .assert * <= DIRTRACKS, error, "***** custom upload code too high in memory. *****"

            ; will be overwritten with DIRTRACKS ($0710..$0718) and DIRSECTORS ($0719..$0721)
toidleloop:;lda #OPC_LAX_ZP; $a7
           ;ldx #LOADEDSECTOR
            sta trkchkswtc + 0
            stx trkchkswtc + 1
            ldx #OPC_DEC_ZP; $c6
            stx chksumswtc
            ldx #OPC_BPL
            ldy #DIRTRACK
            bne DECODETABLE + .lobyte(totrskidle); jmp

            .assert * <= (DECODETABLE + DECTABOFFS), error, "***** 'toidleloop' code too large. *****"

dcodinit41: lda #ATNA_OUT | CLK_OUT; drive idle
            sta VIA1_PRB

            lda #.lobyte(BL)
            sta BLP

           ;ldx ROMOS_TRACK_DIFF
            txa
            ldx #.lobyte(topofstack41); $06
            txs
            pha

.if WORK_AROUND_1541U_BUGS
            lda #$ff; bit 6 of result goes to carry, but 1541U does not implement
            arr #$7f; arr #imm correctly, and the result lsb dropping out goes to
            bcc :++ ; carry instead, so adjust the decoding table for this case
            ldy #arroffsetse - arroffsets - 1
:           ldx arroffsets,y
            inc decodetab,x
            dey
            bpl :-
:
.endif; WORK_AROUND_1541U_BUGS

            lda #T1_FREE_RUNNING | PA_LATCHING_ENABLE; watchdog IRQ: count phi2 pulses, 16-bit free-running,
            sta VIA2_ACR                             ; enable port A latching to grab one GCR byte at a time
                                                     ; rather than letting the GCR bitstream scroll through
                                                     ; port A (applies to 1541 and Oceanic OC-118, but not
                                                     ; 1541-II)
            lda #READ_MODE | BYTE_SYNC_ENABLE
            sta VIA2_PCR

            ; timers/watchdog initialisation
            ; for the long timeout between block-ready and block-send, use two arithmetically cascaded timers:
            ; their periods differ, so their counters drift further apart every time either timer resets.
            ; the effective timeout is reached as soon as the difference between the counters is >= 128, which for
            ; the used periods' difference of 7 cycles with the counter periods of $ef00 and $ef07 is at least
            ; floor(128 / 7) * $ef07 = 18 * $ef07 = 1,101,438 cycles at 1 MHz, i.e., roughly 1 second.
            ; a few cycles are added to or subtracted from the effective timeout: added because as a counter reset
            ; apparently takes 2 cycles, so the effective periods are $ef02 and $ef09, subtracted because the counters'
            ; difference does not increase by 7 on counter $ef07 reset, but increases by 7 and then decreases by 2.
            lda #$07
            sta VIA1_T1L_L; VIA1 timer 1 (TIMER2) low-order latch
            ldx #$00
            stx VIA2_T1L_L; VIA2 timer 1 (TIMER)  low-order latch
            stx VIA1_PRA; one MHz if running on 1570/71 with option ONLY_1541_AND_COMPATIBLE

            lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS; $7f
            sta VIA1_IER; no IRQs from VIA 1
            sta VIA2_IER; no IRQs from VIA 2
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA2_IER; timer 1 IRQs from VIA 2

           ;ldx #$00
            lda (V1B,x); VIA1_PRB
            and #DEVICE_NUMBER
            ora getbyte + 1
            sta getbyte + 1

            ; fade off the busy LED if lit
           ;ldx #$00
            lda (V2B,x); VIA2_PRB
            sta DISKCHANGEBUFFER; store light sensor state for disk removal detection
            and #BUSY_LED
            beq :+
            dex
:           stx LEDSTATE
:           jsr fadeled
            bne :-

            pla
            bne :+
            jmp stepperfix; motor on via trackstep
:           lda #MOTOR
            jsr bitsetv2b
            jmp stepperok

.if WORK_AROUND_1541U_BUGS
arroffsets: .byte $57, $5f, $67, $77, $7f, $87
            .byte $d7, $df, $e7, $f7, $ff
arroffsetse:
.endif; WORK_AROUND_1541U_BUGS

TRAMPOLINEOFFSET = $27; dgetrout - dinstall + 1

drvcodeend41:
            .org * - TRAMPOLINEOFFSET

dinstall:   sei
            ldx ROMOS_TRACK_DIFF; $42
            txs
            lda #VIA_ATN_IN_INPUT | VIA_DEVICE_NUMBER_INPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT
            sta VIA1_DDRB
            lda VIA1_PRB
            and #DEVICE_NUMBER
            ora getbyteinstall + 1
            sta getbyteinstall + 1

            lda #ATNA_OUT | CLK_OUT
            sta VIA1_PRB
:           lda VIA1_PRB; wait for ATN_IN set and DATA_IN clear
            lsr
            bcs :-
            lda #ATNA_OUT
            sta VIA1_PRB

            ldx #.lobyte(drvcodebeg41 - 1)
dgetrout:   inx

            .assert * >= drvcodeend41, error, "***** 1541 trampoline too low in memory. *****"

            bne :+
            inc dgetputhi
:           DRIVEGETBYTE getbyteinstall; there is no watchdog while installing
dgetputhi = * + 2
            sta a:.hibyte(drvcodebeg41 - 1) << 8,x
            cpx #.lobyte(drvcodeend41 - 1)
            bne dgetrout
            dec drvcodebeg41
            bne dgetrout

            tsx; ROMOS_TRACK_DIFF
            jmp dcodinit41
drvprgend41:
            .assert * <= $0800, error, "***** 1541 drive code too large. *****"

            .reloc
