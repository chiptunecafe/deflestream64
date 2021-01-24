
.fileopt comment, "Loader resident code portion"
.fileopt compiler, "CA65"
.fileopt author, "Gunnar Ruthenberg"

__NO_LOADER_SYMBOLS_IMPORT = 1
__NOIMPORTVARS = 1

.include "loader.inc"
.include "kernal.inc"

.include "cpu.inc"

.include "hal/hal.inc"


.segment "DISKIO_ZP" : zeropage

; all zeropage variables can be
; overwritten when the loader is idle

.macro alloc_zpvar symbol
symbol:       .res 1
    .exportzp symbol
.endmacro

.macro alloc_zpvar_2 symbol
symbol:       .res 2
    .exportzp symbol
.endmacro

loader_zp_first = *
.export loader_zp_first

alloc_zpvar loadaddrlo
alloc_zpvar loadaddrhi

.if LOADCOMPD_TO
    alloc_zpvar loadaddroffslo
    alloc_zpvar loadaddroffshi
.endif

.if HAVE_DECOMPRESSOR
    alloc_zpvar decdestlo
    alloc_zpvar decdesthi
.endif

.if END_ADDRESS_API
    alloc_zpvar endaddrlo
    alloc_zpvar endaddrhi
.endif

.if BYTESTREAM
    alloc_zpvar LOADXBUF
    alloc_zpvar LOADYBUF
    alloc_zpvar YPNTRBUF
    alloc_zpvar STREAMBLKIDX
    alloc_zpvar NEXTSTREAMBLKIDX
    .if GETCZPPOINTERS
    alloc_zpvar_2 GETCLOADADDRESS
    alloc_zpvar_2 GETCMEMADDRESS
    .endif
.endif

.if PLATFORM <> diskio::platform::COMMODORE_16
    alloc_zpvar GETBYTE_CLOCK_ATN_HI
.endif

alloc_zpvar BLOCKDESTLO
alloc_zpvar BLOCKINDEX; this one must be there after BLOCKDESTLO (used as pointer hibyte)
.assert BLOCKINDEX = BLOCKDESTLO + 1, error, "BLOCKINDEX != BLOCKDESTLO + 1"

.if (!HAVE_DECOMPRESSOR) & LOAD_VIA_KERNAL_FALLBACK
    alloc_zpvar_2 LOADDESTPTR
.endif

; decompressor
DECOMPVARS:
.exportzp DECOMPVARS

.if DECOMPRESSOR = DECOMPRESSORS::BITNAX
              .res 7
.elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER2
              .res 1
.elseif DECOMPRESSOR = DECOMPRESSORS::DOYNAX_LZ
              .res 3
.elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
              .res 8
.elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
              .res 2
.elseif DECOMPRESSOR = DECOMPRESSORS::NUCRUNCH
              .res 5
.elseif DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
              .res 3
.elseif DECOMPRESSOR = DECOMPRESSORS::SUBSIZER
              .res 8
.elseif DECOMPRESSOR = DECOMPRESSORS::TINYCRUNCH
              .res 6
.endif

.if HAVE_DECOMPRESSOR & LOAD_VIA_KERNAL_FALLBACK
    alloc_zpvar_2 LOADDESTPTR
    .exportzp LOADDESTPTR
.endif

loader_zp_last = * - 1
.export loader_zp_last

            CHECK_LOADER_ZP_ADDRESSES

.segment "DISKIO"


.if PLATFORM = diskio::platform::COMMODORE_16
KERNAL_FALLBACK_WAIT_FOR_BLOCK_READY_DELAY = $0c
.else
KERNAL_FALLBACK_WAIT_FOR_BLOCK_READY_DELAY = $07
.endif

.if GETCZPPOINTERS
getclodadr = GETCLOADADDRESS
getcmemadr = GETCMEMADDRESS
.endif

.ifdef RESIADDR
            .org RESIADDR - 2
            .word * + 2; load address
.endif

            CHECK_RESIDENT_START_ADDRESS

.if LOAD_RAW_API

.export loadraw

            ; --- load file without decompression ---
            ; in:  x/y - x: lo, y: hi to 0-terminated filename string,
            ;            zero-length file name will load next file
            ;      c - if LOAD_TO_API != 0, c = 0: load to address as stored in the file
            ;                               c = 1: load to caller-specified address (loadaddrlo/hi)
            ; out: c - set on error
            ;      a - status

            ; C-64/128: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the IO space at $d000 is enabled
loadraw:    jsr openfile
    .if LOAD_VIA_KERNAL_FALLBACK
            bcs openerror; only with kernal fallback because only then the call might fail
    .endif
:           jsr pollblock
            bcc :-
            cmp #diskio::status::OK + 1
    .if LOAD_VIA_KERNAL_FALLBACK
openerror:
    .endif
            rts

.endif; LOAD_RAW_API

.if LOAD_COMPD_API

.export loadcompd
    .if DISABLE_WATCHDOG
.export loadcompdopen
.export loadcompdexecute
    .endif; DISABLE_WATCHDOG

            ; --- load a compressed file ---
            ; in:  x/y - x: lo, y: hi to 0-terminated filename string,
            ;            zero-length file name will load next file
            ;      c - if LOAD_TO_API != 0, c = 0: load to address as stored in the file
            ;                               c = 1: load with caller-specified address offset (loadaddroffslo/hi)
            ; out: c - set on error
            ;      a - status

            ; C-64/128: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the IO space at $d000 is enabled
loadcompd:
    .if LOAD_TO_API
            bcs :+
            clc
            jsr openfile
            jmp compdfileopen
:
    .endif
            jsr loadcompdopen
compdfileopen:
    .if LOAD_VIA_KERNAL_FALLBACK
            ; only with kernal fallback because only then the call might fail
            bcc :+
            rts
:
    .endif
loadcompdexecute:
            ; throw exception on stream error
            tsx
            stx stackpntr + $01

    .if EXCEPTIONS & LOAD_RAW_API
            inc throwswtch + $01; throw exception on stream error
    .endif

    .if LOAD_VIA_KERNAL_FALLBACK
            BRANCH_IF_INSTALLED nodeploadf

            jsr decompress; calls getckernal, which sets memory configuration

            ; close the file
kernalwind: jsr getckernal
            bcc kernalwind
            bcs compdeof; jmp
nodeploadf:
    .endif; LOAD_VIA_KERNAL_FALLBACK

    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_ALL_RAM
    .endif

            jsr decompress

            ; decompression is finished

            ; handle special case that decompressing is as quick as loading,
            ; this call will fetch the loading finished status and ack loading,
            ; this also loads any remaining uncompressed blob when using Bitnax
:           lda getcmemadr + 1
            bne compdeof
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
        .endif
            jsr getnewblkx
            bcc :-

            ; loading and decompression is done
compdeof:   OK_CLC; a = #diskio::status::OK; $00, clc = all ok

            ; fall through
maybethrow:
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ldy memconfig + $01
            SET_MEMCONFIG_Y
    .endif

    .if LOAD_RAW_API
throwswtch: ldy #$00
            beq dontthrow
    .endif
    .if LOAD_VIA_KERNAL_FALLBACK
            stx :+ + 1; x holds KERNAL error code
    .endif
            ; throw exception
stackpntr:  ldx #$00
            txs
    .if LOAD_VIA_KERNAL_FALLBACK
:           ldx #0; x holds KERNAL error code
    .endif
dontthrow:
    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            bcs :+; skip on error
            ; return execution address in x/y
            ldx lo + $01
            ldy hi + $01
:
    .endif
            rts
.endif; LOAD_COMPD_API

.if DISABLE_WATCHDOG | .defined(OPENFILE_POLLBLOCK_API)
.export openfile
.endif

openfile:
.if LOADCOMPD_TO
            lda #$00
            sta loadaddroffslo
            sta loadaddroffshi
.endif
.if LOAD_TO_API
            lda #OPC_LDA_ZP
            bcs :+
loadcompdopen:
            lda #OPC_STA_ZP
:           sta storeladrl
            sta storeladrh
.else
loadcompdopen:
.endif
            sty BLOCKINDEX; file ID parameter buffer

.if END_ADDRESS_API | BYTESTREAM
    .if HAVE_GETC
            lda #.lobyte(getcload)
            ldy #.hibyte(getcload)
            jsr puttoloadb
    .endif; HAVE_GETC

    .if HAVE_GETC & BYTESTREAM
            ldy #$ff
            sty YPNTRBUF
            iny
    .else
            ldy #$00
    .endif
    .if END_ADDRESS_API
            sty endaddrlo
            sty endaddrhi
    .endif
    .if BYTESTREAM
        .if EXCEPTIONS & LOAD_COMPD_API & LOAD_RAW_API
            sty throwswtch + $01; return errors to the caller
        .endif
            sty STREAMBLKIDX
            sty getcmemadr + 1
    .endif
.endif; END_ADDRESS_API | BYTESTREAM

.if MEM_DECOMP_TO_API
    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            lda #OPC_BIT_ZP
    .else
            lda #OPC_STA_ZP
    .endif
            sta storedadrl
            sta storedadrh
.endif

.if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            GET_MEMCONFIG
            sta memconfig + $01
            GOT_MEMCONFIG = 1
.else
            GOT_MEMCONFIG = 0
.endif

.if LOAD_VIA_KERNAL_FALLBACK

    .if !GOT_MEMCONFIG
            GET_MEMCONFIG
    .endif; !GOT_MEMCONFIG
            sta kernaloff + 1

            BRANCH_IF_NOT_INSTALLED ldrnotinst
            jmp nofallback

            ; loader is not installed,
            ; load via KERNAL calls

ldrnotinst: ENABLE_KERNAL_SERIAL_ROUTINES

    .if BYTESTREAM
            lda #.lobyte(getckernal)
            ldy #.hibyte(getckernal)
            jsr puttoloadb
    .endif; BYTESTREAM
            stx namestrpos + 0

    .if KERNAL_FALLBACK_SEI_WORKAROUNDS
            ENABLE_WAITBUSY_KERNAL
    .endif; KERNAL_FALLBACK_SEI_WORKAROUNDS

    .if BYTESTREAM
            ldy BLOCKINDEX; get buffered parameter
    .endif
            sty namestrpos + 1
            ldx #$ff
:           inx
namestrpos = * + $01
            lda a:$00,x
            bne :-
            txa
            pha; name length

    .if PLATFORM = diskio::platform::COMMODORE_128
            lda #0
            sta FNLEN
            lda #COMMAND_ERROR_CHANNEL
            ldx FA
            tay
            jsr SETLFS
            jsr OPEN
            bcs burstopner
        .if KERNAL_FALLBACK_SEI_WORKAROUNDS
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            CLEAR_DATA_OUT_CLEAR_CLK_OUT_ASSERT_ATN
:           SET_FLAGS_N_DATA_V_CLK
            bpl :-
            ldx #$b8; delay
:           dex     ; of
            bne :-  ; ~1 ms
            lda FA
            sta DFLTO
            ora #SA_LISTEN
            sta BSOUR
            sei
            jsr NOEOI; send byte
            lda #SA_OPENCHANNEL | COMMAND_ERROR_CHANNEL
            jsr SECND; send listen secondary address
            POP_CLOCKCONFIG
        .else; !KERNAL_FALLBACK_SEI_WORKAROUNDS
            ldx LA
            jsr CKOUT
        .endif; !KERNAL_FALLBACK_SEI_WORKAROUNDS
            bcc :+
burstopner: tax
            pla; name length
            txa
            jmp kernlopner
:           ldy #burstldcme - burstldcmd
:           lda burstldcmd - 1,y
            jsr BSOUT
            dey
            bne :-
            pla; name length
            sta FNLEN
            lda namestrpos + 0
            sta FNADR + 0
            lda namestrpos + 1
            sta FNADR + 1
:           lda (FNADR),y
            jsr BSOUT
            iny
            cpy FNLEN
            bne :-
            jsr CLRCH
            bit SERIAL
            bvs burstload

            lda FNLEN
            pha
            lda LA
            jsr CLOSE
            jmp noburstlod

burstload:  START_BURST_LOAD

            ldx #$fe
            jsr getburstby; status
            cmp #KERNAL_STATUS_ERROR_BURST
            bcc burstopnok
            bne :+
            lda #diskio::status::FILE_NOT_FOUND
:           cmp #KERNAL_STATUS_EOF_BURST
            beq :+
            ; this is a job error code
            jmp kernalerr
:           jsr getburstby
            tax
burstopnok: stx COUNT
            jmp kernalfinp

getburstby: GET_BURST_BYTE
            rts

burstldcmd: .byte $1f, "0u"; read backwards
burstldcme:

noburstlod:
    .endif ; PLATFORM = diskio::platform::COMMODORE_128

            PREPARE_PARALLEL_CHECK

            lda #KERNALFILENO
            ldx FA
            ldy #$00
            jsr SETLFS
            pla; name length
            ldx namestrpos + 0
            ldy namestrpos + 1
            jsr SETNAM
            jsr OPEN
            bcc kernalfopn

kernlopner: tax
            lda #diskio::status::DEVICE_NOT_PRESENT
            cpx #OPEN_DEVICENOTPRESENT
            beq :+
            lda #diskio::status::GENERIC_KERNAL_ERROR
:           ldy kernaloff + 1
            SET_MEMCONFIG_Y
            sec; error
            rts

kernalfopn:
    .if KERNAL_FALLBACK_SEI_WORKAROUNDS
            BRANCH_IF_DRIVE_PARALLEL noseiwrkar

        .if USE_2_MHZ
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
        .endif
            CLEAR_DATA_OUT_CLEAR_CLK_OUT_ASSERT_ATN
:           SET_FLAGS_N_DATA_V_CLK
            bpl :-
            ldx #$b8; delay
:           dex     ; of
            bne :-  ; ~1 ms
            lda FA
            sta DFLTN
            ora #SA_TALK
            sta BSOUR
            sei
            jsr NOEOI; send byte
            lda #SA_OPENCHANNEL; secondary address
            jsr TKSA; send talk secondary address
        .if USE_2_MHZ
            POP_CLOCKCONFIG
        .endif
            jmp kernalfinp
noseiwrkar:
    .endif; !KERNAL_FALLBACK_SEI_WORKAROUNDS
            ldx #KERNALFILENO
            jsr CHKIN
kernalfinp:
            ; file not found is not detected at this point
            ; but after "getting" the load address,
            ; the busy led will keep flashing
    .if LOAD_TO_API
            lda #OPC_STA_ZP
            cmp storeladrl
            beq :+
            lda loadaddrlo
            sta LOADDESTPTR + 0
            lda loadaddrhi
            sta LOADDESTPTR + 1
        .if PLATFORM = diskio::platform::COMMODORE_128
            bit SERIAL
            bvc :+
            jsr getburstby; skip load
            jsr getburstby; address
            jmp kernopenok
:
        .endif ; PLATFORM = diskio::platform::COMMODORE_128
            jsr BASIN; skip load
            jsr BASIN; address
            jmp kernopenok
:
    .endif

    .if PLATFORM = diskio::platform::COMMODORE_128
            bit SERIAL
            bvc :+
            jsr getburstby
            jmp kernalstrl
:
    .endif ; PLATFORM = diskio::platform::COMMODORE_128
            jsr BASIN
kernalstrl: sta LOADDESTPTR + 0
            sta loadaddrlo
    .if PLATFORM = diskio::platform::COMMODORE_128
            bit SERIAL
            bvc :+
            jsr getburstby
            jmp kernalstrh
:
    .endif ; PLATFORM = diskio::platform::COMMODORE_128
            jsr BASIN
kernalstrh: sta LOADDESTPTR + 1
            sta loadaddrhi

kernopenok: lda STATUS
            beq :+
            lda #diskio::status::FILE_NOT_FOUND; cannot determine type of error
            jmp kernalerr

:
    .if HAVE_KERNAL_POLLBLOCK
            lda #$fc; $0100 bytes minus 2 bytes track/sector link and 2 bytes load address
            sta kernalblockcnt
    .endif
            jmp retrnokclc

nofallback:

.endif; LOAD_VIA_KERNAL_FALLBACK

            WAKEUP

            WAIT_FOR_BLOCK_READY

            ; x contains the filename pointer lobyte parameter
            stx BLOCKDESTLO; pointer hibyte is already stored at BLOCKINDEX = BLOCKDESTLO + 1

.if !(END_ADDRESS_API | BYTESTREAM)
            ldy #$00
.endif
sendname:   lda (BLOCKDESTLO),y
            pha
            SENDBYTE
            GETBYTE_SETUP
            iny
            pla
            beq :+
            cpy #FILENAME_MAXLENGTH
            bne sendname
:
            ; no asynchronicity:
            ; the drive must be as quick or quicker than the host here,
            ; it must get the last data bit in time

            ; clear DATA OUT and CLK OUT so they can be polled later
            ; (when DATA IN = 0: drive busy,
            ;  when DATA IN = 1, CLK IN = 1: device not present)
            CLEAR

           ;ldx #$ff
            stx BLOCKINDEX

.if LOAD_VIA_KERNAL_FALLBACK
            ; check whether the loader is still installed
            ldx #KERNAL_FALLBACK_WAIT_FOR_BLOCK_READY_DELAY; some delay until the drive side is ready
:           dex
            bne :-
            SET_FLAGS_N_DATA_V_CLK
            bvc retrnokclc

            ; if not, try again with kernal routines
            SET_IO_KERNAL
            ldx BLOCKDESTLO + 0
            ldy BLOCKDESTLO + 1
            jmp ldrnotinst
.endif

.if HAVE_KERNAL_POLLBLOCK

    .if HAVE_POLLBLOCK = 0
kernalblockcnt:
            .byte 0
    .endif

    .if (PLATFORM = diskio::platform::COMMODORE_128)
gburstatus: lda STATUS
            cmp #KERNAL_STATUS_ERROR_BURST
            bcc :++
            cmp #KERNAL_STATUS_EOF_BURST
            bne :+
            lda #KERNAL_STATUS_EOF; $40
            sta STATUS
:           jsr kernalerr
            bcs :++

:           jsr getburstby
            cmp #KERNAL_STATUS_ERROR_BURST
            bcc :+
            sta STATUS
            cmp #KERNAL_STATUS_EOF_BURST
            bne :--; branch if not EOF, this is a job error code
            jsr getburstby
            tax
            clc
:           rts
    .endif
.endif

.if HAVE_POLLBLOCK | LOAD_VIA_KERNAL_FALLBACK
retrnokclc: OK_CLC; a = #diskio::status::OK; $00, clc = all ok
.else
retrnokclc: clc; actually only need the 2 cycles for SEND_BLOCK_SIGNAL on C-64/128
.endif

            ; no asynchronicity:
            ; the drive must be as quick or quicker than the computer here,
            ; it sets the busy flag which the computer polls after returning
.if !(USE_2_MHZ | (PLATFORM = diskio::platform::COMMODORE_16))
polldone:
.endif
.if HAVE_POLLBLOCK
            rts

    .if LOAD_VIA_KERNAL_FALLBACK
kernlgberr: cmp #KERNAL_STATUS_EOF; $40
            beq kernalbeof; carry is set on branch
            sec
            rts
    .endif

    .if DISABLE_WATCHDOG | .defined(OPENFILE_POLLBLOCK_API)
.export pollblock
    .endif

pollblock:
    .if LOAD_VIA_KERNAL_FALLBACK

        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE
        .endif

            BRANCH_IF_INSTALLED getblnofbk

kernalblockcnt = * + 1
            ldx #0
            bne kernalgblk

            ldx #$fe; $0100 bytes minus 2 bytes for track/sector link
        .if PLATFORM = diskio::platform::COMMODORE_128
            bit SERIAL
            bvc :+
            ENABLE_KERNAL_SERIAL_ROUTINES
            jsr gburstatus
            bcc kernalgblc
            bcs kernlgberr; jmp
:
        .endif ; PLATFORM = diskio::platform::COMMODORE_128
            lda STATUS
            bne kernlgberr

kernalgblc: stx kernalblockcnt

kernalgblk: ENABLE_KERNAL_SERIAL_ROUTINES

kernalgblp:
        .if PLATFORM = diskio::platform::COMMODORE_128
            bit SERIAL
            bvc :+
            jsr getburstby
            jmp kernbytldd
:
        .endif ; PLATFORM = diskio::platform::COMMODORE_128

            jsr kernalgbyt
            bcc kernbytldd

            cmp #KERNAL_STATUS_EOF; $40
            bne kernlgberr
           ;sec
            lda kernalblockcnt
            stx COUNT
            sbc COUNT
            tay
            ldx #0
            stx kernalblockcnt
            beq krnlastblk; jmp

kernbytldd:
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_ALL_RAM_Y
        .endif

            ldy #$00
            sta (LOADDESTPTR),y

        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_KERNAL_SERIAL_ROUTINES
        .endif

            inc LOADDESTPTR + 0
            bne :+
            inc LOADDESTPTR + 1
:           dex
            bne kernalgblp
            ldy kernalblockcnt
            stx kernalblockcnt

krnlastblk:
        .if (!LOAD_UNDER_D000_DFFF) & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE
        .else
            ENABLE_ALL_RAM
        .endif

            clc
kernalbeof: lda #diskio::status::OK
            rts

getblnofbk: jsr getblock
            bcs enteridle
            jmp retrnokclc
    .else; !LOAD_VIA_KERNAL_FALLBACK
            jsr getblock
            bcc retrnokclc
    .endif; !LOAD_VIA_KERNAL_FALLBACK

enteridle:  IDLE

.endif; HAVE_POLLBLOCK

pollfail:   rts

getblock:   POLL_BLOCK pollfail

            SEND_BLOCK_SIGNAL

            lda #OPC_RTS; disable getblock store and loop
            jsr get1byte; get block index or error/eof code

.if END_ADDRESS_API | LOAD_UNDER_D000_DFFF | USE_2_MHZ | (PLATFORM = diskio::platform::COMMODORE_16)
           ;sec
            beq :+
            cmp #diskio::status::FILE_NOT_FOUND
            bcc :++
:           jmp polldone
:
.else
           ;sec
            beq :+
            cmp #diskio::status::FILE_NOT_FOUND
:           bcs polldone
.endif
            pha
            jsr getbyte; get block size
            tay
            pla
            cmp #$80; sign extension
            ror
            bcs :+; branch if file's last block
.if BYTESTREAM
            sty NEXTSTREAMBLKIDX
.endif
            ldy #$01; block size
:           clc
            adc BLOCKINDEX
            sta BLOCKINDEX
            clc
            bne calcaddr

            ; first block: get load address
            jsr getbyte; load address lo
.if LOADCOMPD_TO
            clc
            adc loadaddroffslo
            php
.endif; LOADCOMPD_TO
storeladrl: sta loadaddrlo; is changed to lda on load_to
            jsr getbyte; load address hi
.if LOADCOMPD_TO
            plp
            adc loadaddroffshi
            sec
.endif; LOADCOMPD_TO
storeladrh: sta loadaddrhi; is changed to lda on load_to
           ;sec

calcaddr:   ; calculate the position in memory according to the block number,
            ; this is performing: pos = loadaddr + BLOCKINDEX * 254 - 2
            sty BLOCKDESTLO
            php
            lda loadaddrlo
            sbc BLOCKINDEX
            pha
            lda loadaddrhi
            adc BLOCKINDEX
            tax
            pla
            plp
.if BYTESTREAM
            php
.endif; BYTESTREAM
            sbc BLOCKINDEX
            bcs :+
            dex
:           dex

            clc
            sbc BLOCKDESTLO
            sta storebyte + $01
            bcs :+
            dex
:           stx storebyte + $02
.if BYTESTREAM
            plp
            bcc :+
            ; first block
            sta getclodadr + 0
            stx getclodadr + 1
            iny
            sty YPNTRBUF
            dey
:
.endif; BYTESTREAM

            ; getblock loop/get1byte subroutine
.if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ; trading off speed for size: choose either of two getblock loops
            ; depending on the destination of the data to be downloaded
            lda storebyte + $02
            cmp #.hibyte($cf00)
            bcc :+
            cmp #.hibyte($e000)
            bcc getblockio
:
.endif; LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)

            lda #OPC_STA_ABSY; enable getblock store and loop
get1byte:   sta storebyte

.macro STORE
storebyte:  sta $0000,y
.endmacro; STORE
            GETBYTE getbyte, STORE

.if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
    .if PLATFORM = diskio::platform::COMMODORE_128
            jmp gotblock
    .else; PLATFORM <> diskio::platform::COMMODORE_128
            beq gotblock; jmp
    .endif; PLATFORM <> diskio::platform::COMMODORE_128

getblockio: lda storebyte + 1
            sta storebytio + 1
            lda storebyte + 2
            sta storebytio + 2

            GETBYTE getbyteio, STOREBYTE_ALLRAM
gotblock:
.endif; LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)

.if END_ADDRESS_API
            ; update end address
            ldx storebyte + $01
            cpx endaddrlo
            ldy storebyte + $02
            iny
            tya
            sbc endaddrhi
            bcc :+
            stx endaddrlo
            sty endaddrhi
:
.endif; END_ADDRESS_API

.if BYTESTREAM | END_ADDRESS_API | (PLATFORM <> diskio::platform::COMMODORE_64)
            clc; ok
.endif; BYTESTREAM | END_ADDRESS_API | (PLATFORM <> diskio::platform::COMMODORE_64)

.if USE_2_MHZ | (PLATFORM = diskio::platform::COMMODORE_16)
polldone:
.endif; USE_2_MHZ | (PLATFORM = diskio::platform::COMMODORE_16)
            ENDGETBLOCK
            rts

.if LOAD_VIA_KERNAL_FALLBACK
            ; get a byte from the file's byte-stream using the KERNAL API,
            ; sets memory configuration and buffers the y register
    .if BYTESTREAM
getckernal: sty LOADYBUF

            ENABLE_KERNAL_SERIAL_ROUTINES

            jsr kernalgbyt

        .if (!LOAD_UNDER_D000_DFFF) & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
        .else
            ENABLE_ALL_RAM_Y
        .endif

        .if END_ADDRESS_API
            bcs :+
            inc LOADDESTPTR + 0
            bne :+
            inc LOADDESTPTR + 1
:
        .endif; END_ADDRESS_API

            bcc :+
            ldy kernaloff + 1; only on errors on subsequent getc calls, restore the previous
            SET_MEMCONFIG_Y  ; memory configuration which had been active before calling openfile
:
            ldy LOADYBUF
            rts
    .endif; BYTESTREAM

            ; get a byte from the file using the KERNAL API,
            ; the KERNAL ROM must be enabled
            ; in  : nothing
            ; out : a - status on error
            ;     : c - set on error
kernalgbyt:
    .if PLATFORM = diskio::platform::COMMODORE_128
            bit SERIAL
            bvc :+++
            lda kernalblockcnt
            bne :+
            stx burstxbuf + 1
            ldx #$fe; $0100 bytes minus 2 bytes for track/sector link
            jsr gburstatus
            stx kernalblockcnt
burstxbuf:  ldx #0
            bcs :++
:           jsr getburstby
            dec kernalblockcnt
            clc
:           rts
:
    .endif
            lda STATUS
            bne kernalerr

    .if KERNAL_FALLBACK_SEI_WORKAROUNDS
            BRANCH_IF_DRIVE_PARALLEL :+
            WAIT_FOR_BLOCK_READY
:
    .endif; KERNAL_FALLBACK_SEI_WORKAROUNDS
            jsr BASIN

            ldy STATUS
            beq :+
            cpy #KERNAL_STATUS_EOF; $40
            bne kernalerry

:           clc
            rts

            ; EOF or error, close file
kernalerry: tya
kernalerr:  pha; KERNAL status byte
            txa; block counter
            pha
            lda #KERNALFILENO
            jsr CLOSE
            jsr CLALL
            jsr CLRCH
            pla
            tax

    .if END_ADDRESS_API
            lda LOADDESTPTR + 0
            sta endaddrlo
            lda LOADDESTPTR + 1
            sta endaddrhi
    .endif; !END_ADDRESS_API

kernaloff:  lda #$00
            SET_MEMCONFIG
            pla; KERNAL status byte
    .if PLATFORM = diskio::platform::COMMODORE_16
            sta STATUS; the CLRCH call above sets STATUS ($90)
    .endif
            cmp #KERNAL_STATUS_EOF; $40
            bne kernaloerr
           ;sec
            rts; EOF
kernaloerr: sec
            tax
            cmp #diskio::status::DEVICE_NOT_PRESENT
            bcs :+
            lda #diskio::status::GENERIC_KERNAL_ERROR
            sec
:
    .if EXCEPTIONS
            jmp maybethrow
    .else; !EXCEPTIONS
            rts
    .endif; !EXCEPTIONS

.endif; LOAD_VIA_KERNAL_FALLBACK

.if (BYTESTREAM | HAVE_DECOMPRESSOR) & HAVE_GETC
            ; get a byte from the file's byte-stream, read from memory

            ; C-64/128: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the
            ; IO space at $d000 is disabled if data is accessed at $d000..$dfff
getcmem:
getcmemadr = * + 1
            lda a:$00
            inc getcmemadr + 0
            beq :+
            rts; one extra byte for one cycle less
:           inc getcmemadr + 1
            rts
.endif; (BYTESTREAM | HAVE_DECOMPRESSOR) & HAVE_GETC

.if BYTESTREAM
            ; getcload: get a byte from the file's byte-stream, download a file block before if possible

            ; C-64/128: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the
            ; IO space at $d000 is disabled if data is accessed at $d000..$dfff

waitforblk:
    .if HAVE_GETC
            pla; current stream byte
    .endif
load1stblk: jsr getnewblk; LOADXBUF is set already
            bcs xbuffer; branch on error
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
    .endif
            ldy STREAMBLKIDX
            bne chkloaded
            ; first block loaded, return to caller
            inc STREAMBLKIDX

    .if HAVE_GETC
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_ALL_RAM
        .endif
            SKIPWORD; return first file byte and maybe download another block before that
getcload:
            sty LOADYBUF

dogetcload: ldy YPNTRBUF

getclodadr = * + 1
            lda a:$00,y
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
        .endif
            inc YPNTRBUF
            beq nxtstrmblk; branch to process next stream block
maybegtblk: BRANCH_IF_BLOCK_READY getnewblkx; download block as soon as possible
    .endif; HAVE_GETC

loadbytret:
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_ALL_RAM_Y
    .endif
    .if HAVE_GETC
            ldy LOADYBUF
    .endif
            rts

nxtstrmblk: ldy STREAMBLKIDX; block index and flag to skip waiting for the next block to download,
                            ; the value is increased for every loaded block and set to $ff after loading is finished
    .if HAVE_GETC
            stx LOADXBUF
    .endif
            beq load1stblk

chkloaded:
    .if HAVE_GETC
            pha; current stream byte
    .endif
            iny
            beq xbufclc; branch if last file block had been loaded already, clear carry: ok
            cpy NEXTSTREAMBLKIDX
            bcs waitforblk; branch if the next block in the stream is not yet loaded

            ; advance stream pointer
            ; this is not done after the first file block had been downloaded
           ;clc
            lda #$fe
            adc getclodadr + 0
            sta getclodadr + 0
            bcc :+
            inc getclodadr + 1
:           lda #$02
            sta YPNTRBUF
            inc STREAMBLKIDX
            bne xbufclc; jmp, clear carry: ok

.if BYTESTREAM & (DECOMPRESSOR = DECOMPRESSORS::BITNAX)
getnewblkz: lda #$00
            sta YPNTRBUF
.endif
getnewblkx: stx LOADXBUF
getnewblk:
    .if HAVE_GETC
            pha; current stream byte
    .endif
            jsr getblock
            bcs gotstatus; branch if error or loading finished

xbufclc:    clc; ok, this is branched to
xbuf:
    .if HAVE_GETC
            pla; current stream byte
    .endif
xbuffer:    ldx LOADXBUF
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            jmp loadbytret; restore memory configuration
    .else
        .if HAVE_GETC
            ldy LOADYBUF
        .endif
            rts
    .endif

            ; the status byte has been received, end loading
gotstatus:  ; switch to memory-read getc routine
           ;sec
    .if HAVE_GETC
            pha; status
            ldx YPNTRBUF; YPNTRBUF = $00 -> add 256
            dex
            txa
           ;sec
            adc getclodadr + 0
            sta getcmemadr + 0; current stream buffer position lo
            lda #$00
            adc getclodadr + 1
            jsr setgetcmem
            pla; status
            sec
    .else; !HAVE_GETC
            ldx getclodadr + 0
            stx getcmemadr + 0; current stream buffer position lo
            ldx getclodadr + 1
            inx
            stx getcmemadr + 1; current stream buffer position hi
    .endif; !HAVE_GETC

            ldx #$ff
            stx STREAMBLKIDX; mark load finished

    .if HAVE_POLLBLOCK
            jsr enteridle
    .else; !HAVE_POLLBLOCK
            IDLE
    .endif; !HAVE_POLLBLOCK

            tax; status
            ; carry is cleared upon return to signal ok
            beq xbufclc; clear carry: ok

            ; an error occured, stop loading and/or decompressing, return error to the caller,
            ; a = status
           ;sec
    .if EXCEPTIONS
            jmp maybethrow
    .else
            rts
    .endif
.endif; BYTESTREAM

.if MEM_DECOMP_API

.export memdecomp

            ; --- decompress a compressed file from memory ---
            ; in:  x/y - lo/hi of compressed file in memory
            ;      c   - if MEMDECOMP_TO_API != 0, c = 0: decompress to address as stored in the file
            ;                                      c = 1: decompress to caller-specified address (loadaddrlo/hi)

            ; out: undefined
memdecomp:  stx getcmemadr + 0
    .if BYTESTREAM
            tya
            jsr setgetcmem
    .else
            sty getcmemadr + 1
    .endif

    .if MEM_DECOMP_TO_API
        .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            lda #OPC_BIT_ZP
        .else
            lda #OPC_STA_ZP
        .endif
            bcc :+
            lda #OPC_LDA_ZP
:           sta storedadrl
            sta storedadrh
    .endif

    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            jsr decompress
            ; return the execution address in x/y
            ldx lo + $01
            ldy hi + $01
            rts
    .else
            jmp decompress
    .endif

.endif; MEM_DECOMP_API

.if UNINSTALL_API

.export uninstall

            ; --- uninstall the loader ---
            ; in:  nothing
            ; out: undefined
uninstall:  DO_UNINSTALL
            rts

.endif; UNINSTALL_API

.if HAVE_DECOMPRESSOR
    .if DECOMPRESSOR = DECOMPRESSORS::BITNAX
        .include "decompress/bitnaxdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER2
        .include "decompress/b2decomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::DOYNAX_LZ
        .include "decompress/doynaxdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
        .include "decompress/exodecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
        .include "decompress/lcdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::NUCRUNCH
        .include "decompress/ncdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
        .include "decompress/pudecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::SUBSIZER
        .include "decompress/subsizerdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::TINYCRUNCH
        .include "decompress/tcdecomp.s"

    .else
        .error "***** Error: The selected decompressor option is not implemented. *****"
    .endif
.endif; HAVE_DECOMPRESSOR

.if HAVE_GETC
setgetcmem: sta getcmemadr + 1
            lda #.lobyte(getcmem)
            ldy #.hibyte(getcmem)

            ; patch the various calls to the getchar routines,
            ; one out of three functions is used:
            ; getcmem    - get a char from memory after the whole file is loaded
            ; getcload   - get a char and before that, download a file block if possible/necessary
            ; getckernal - get a char when using the KERNAL API as fallback
puttoloadb:
    .if HAVE_DECOMPRESSOR
            SETDECOMPGETBYTE
    .endif
            rts
.endif; HAVE_GETC

            CHECK_RESIDENT_END_ADDRESS
