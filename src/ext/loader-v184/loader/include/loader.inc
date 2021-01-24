
.ifndef _LOADER_INC_
_LOADER_INC_ = 1

; C-128 only: for burst transfers, use the CLK line for the incoming data sent signal rather than the
;             SERIAL_IRQ flag in CIA1_ICR ($dc0d) in order to avoid interference by CIA1 interrupt handlers,
;             note that this slows down transfer speed
.define ASYNCHRONOUS_BURST_HANDSHAKE 0

KERNAL_FALLBACK_SEI_WORKAROUNDS = 1; set to 0 if loading via KERNAL fallback is problematic with non-standard KERNALs, IEEE-488 interfaces or devices like IDE64

.define WORK_AROUND_1541U_BUGS 1; disabling this will make the loader not work with earlier 1541U firmwares

.pushseg

.segment "CODE"; import symbols as absolute by default

.scope diskio

.scope platform
    COMMODORE_64            =   64
    COMMODORE_128           =  128
    COMMODORE_16            =   16
.endscope; platform

.scope drivetype
    DRIVES_1541             =  $00
    DRIVES_157X             =  $10
    DRIVES_1581_CMD         =  $20

    DRIVE_1541              =  $00
    DRIVE_1541_C            =  $01
    DRIVE_1541_II           =  $02
    DRIVE_1541U             =  $03
    DRIVE_1570              =  $14
    DRIVE_1571              =  $15
    DRIVE_1571CR            =  $16
    DRIVE_1581              =  $27
    DRIVE_CMD_FD_2000       =  $28
    DRIVE_CMD_FD_4000       =  $29

    DRIVE_GENERIC_SERIAL    = .lobyte(-5) ; $fb
    DRIVE_GENERIC_BURST     = .lobyte(-4) ; $fc
    DRIVE_GENERIC_PARALLEL  = .lobyte(-3) ; $fd
    DEVICE_UNKNOWN          = .lobyte(-2) ; $fe
    DEVICE_NONE             = .lobyte(-1) ; $ff
.endscope; drivetype

.scope status
    OK                      =  $00

    DEVICE_INCOMPATIBLE     = .lobyte(-5) ; $fb, cannot upload loader drive code to active device, if LOAD_VIA_KERNAL_FALLBACK != 0, do not regard this as an error
    TOO_MANY_DEVICES        = .lobyte(-4) ; $fc, another device on the bus other than the active device is not compatible, if LOAD_VIA_KERNAL_FALLBACK != 0, do not regard this as an error
    GENERIC_KERNAL_ERROR    = .lobyte(-3) ; $fd, an error occured during installation or while loading without installed drive code via KERNAL fallback, check the x register for further information
    DEVICE_NOT_PRESENT      = .lobyte(-2) ; $fe
    FILE_NOT_FOUND          = .lobyte(-1) ; $ff
.endscope; status

.endscope; diskio

.enum DECOMPRESSORS

    BITNAX                  =  $00 ; Doynax and Bitbreaker's Bitnax
    BYTEBOOZER2             =  $01 ; HCL/Booze Design's ByteBoozer2
    DOYNAX_LZ               =  $02 ; Doynax LZ, aka Doynamite
    EXOMIZER                =  $03 ; Magnus Lind's Exomizer
    LEVELCRUSH              =  $04 ; Taboo Levelcrush
    NUCRUNCH                =  $05 ; ChristopherJam aka Shrydar's NuCrunch
    PUCRUNCH                =  $06 ; Pasi Ojala's Pucrunch
    SUBSIZER                =  $07 ; tlr's Subsizer
    TINYCRUNCH              =  $08 ; ChristopherJam aka Shrydar's tinycrunch

    NONE                    = .lobyte(-1) ; $ff

.endenum


.ifdef EXTCONFIGPATH
    .include "loaderconfig.inc"
.else
    .include "config.inc"
.endif; !EXTCONFIGPATH

.if .defined(FORCE_ASYNCHRONOUS_BURST_HANDSHAKE)
.define USE_ASYNCHRONOUS_BURST_HANDSHAKE 1
.else
.define USE_ASYNCHRONOUS_BURST_HANDSHAKE ASYNCHRONOUS_BURST_HANDSHAKE
.endif


.ifndef __NO_LOADER_SYMBOLS_IMPORT

.if PLATFORM <> diskio::platform::COMMODORE_16
    ; Set the VIC bank
    ; in:  a - VIC bank (0..3)
    ; out: undefined
    .macro SET_VIC_BANK bank
            lda #bank & 3
            sta CIA2_PRA
    .endmacro

    .if ALLOW_2_MHZ_ON_C128 | (PLATFORM = diskio::platform::COMMODORE_128)
    ; For 2 MHz in interrupt handlers during loading,
    ; to be run at the start of every IRQ handler
    .macro LOADER_IRQ_HANDLER_PROLOGUE
            ; buffer clock setting and continue at 2 MHz
            lda VIC2_C128_CLOCK
            pha
            lda #C128_TWO_MHZ
            sta VIC2_C128_CLOCK
    .endmacro

    ; For 2 MHz in interrupt handlers during loading,
    ; to be run at the end of every IRQ handler
    .macro LOADER_IRQ_HANDLER_EPILOGUE
            pla
            lda VIC2_C128_CLOCK
    .endmacro
    .endif
.else
    ; To be run at the start of every IRQ handler
    .macro LOADER_IRQ_HANDLER_PROLOGUE
            ; buffer force single clock flag
            ; and allow double clock
            lda TED_CHARGEN_ADDR
            pha
            and #.lobyte(~FORCE_SINGLE_CLOCK)
            sta TED_CHARGEN_ADDR
    .endmacro

    ; To be run at the end of every IRQ handler
    .macro LOADER_IRQ_HANDLER_EPILOGUE
            ; restore force single clock flag
            .local fast

            pla
            lsr
            lsr
            lda TED_CHARGEN_ADDR
            and #.lobyte(~FORCE_SINGLE_CLOCK)
            bcc fast
            ora #FORCE_SINGLE_CLOCK
fast:       sta TED_CHARGEN_ADDR
    .endmacro
.endif


; Install the loader
; note: KERNAL ROM must be enabled, and the I flag will be cleared (CLI)
; in:  nothing
; out: c   - set on error
;      a   - status
;      x   - drive type (one of diskio::drivetypes)
;      y   - if status is diskio::status::OK, zeropage address of version string address
;      i   - 0
.import install
.macro LOADER_INSTALL
            jsr install
.endmacro

.if LOAD_RAW_API
; Load a file without decompression
; in:  x/y - x: lo, y: hi to 0-terminated filename string,
;            zero-length file name will load next file
;      c   - if LOAD_TO_API != 0, c = 0: load to address as stored in the file
;                                 c = 1: load to caller-specified address (loadaddrlo/hi)
; out: c   - set on error
;      a   - status
.import loadraw
.macro LOADRAW namelo_or_track, namehi_or_sector
    .if LOAD_TO_API
            clc
    .endif
            ldx namelo_or_track
            ldy namehi_or_sector
            jsr loadraw
.endmacro

.macro LOADNEXTRAW
            lda #$00
            LOADRAW #.lobyte(* - 1), #.hibyte(* - 1)
.endmacro

.if LOAD_TO_API
    .macro LOADRAW_LOADTO namelo_or_track, namehi_or_sector, dest_lo, dest_hi
            sec
            lda dest_lo
            sta loadaddrlo
            lda dest_hi
            sta loadaddrhi
            ldx namelo_or_track
            ldy namehi_or_sector
            jsr loadraw
    .endmacro

    .macro LOADNEXTRAW_LOADTO dest_lo, dest_hi
            lda #$00
            LOADRAW_LOADTO #.lobyte(* - 1), #.hibyte(* - 1), dest_lo, dest_hi
    .endmacro
.endif; LOAD_TO_API
.endif; LOAD_RAW_API

.if LOAD_COMPD_API
; Load a compressed file
; in:  x/y - x: lo, y: hi to 0-terminated filename string,
;            zero-length file name will load next file
;      c   - if DECOMPLOAD_TO_API != 0, c = 0: load to address as stored in the file
;                                       c = 1: load with caller-specified address offset (loadaddroffslo/hi)
; out: c   - set on error
;      a   - status
;      x/y - if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH, x: lo, y: hi of the loaded file's execution address
.import loadcompd
.import loadcompdopen
.import loadcompdexecute
.macro LOADCOMPD namelo_or_track, namehi_or_sector
            ldx namelo_or_track
            ldy namehi_or_sector
            jsr loadcompd
.endmacro

.macro LOADNEXTCOMPD
            lda #$00
            LOADCOMPD #.lobyte(* - 1), #.hibyte(* - 1)
.endmacro

.if LOAD_TO_API
    .macro LOADCOMPD_RELTO namelo_or_track, namehi_or_sector, load_address_offset_lo, load_address_offset_hi
            sec
            lda load_address_offset_lo
            sta loadaddroffslo
            lda load_address_offset_hi
            sta loadaddroffshi
            ldx namelo_or_track
            ldy namehi_or_sector
            jsr loadcompd
    .endmacro

    .macro LOADNEXTCOMPD_RELTO dest_lo, dest_hi
            lda #$00
            LOADCOMPD_RELTO #.lobyte(* - 1), #.hibyte(* - 1), dest_lo, dest_hi
    .endmacro
.endif; LOAD_TO_API
.endif; LOAD_COMPD_API

.if MEM_DECOMP_API
; Decompress a compressed file from memory
; in:  x/y - x: lo, y: hi of compressed file in memory
;      c   - if MEMDECOMP_TO_API != 0, c = 0: decompress to address as stored in the file
;                                      c = 1: decompress to caller-specified address (loadaddrlo/hi)
; out: x/y - if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH, x: lo, y: hi of the file's execution address
.import memdecomp
.macro MEMDECOMP source_lo, source_hi
    .if MEM_DECOMP_TO_API
            clc
    .endif
            ldx source_lo
            ldy source_hi
            jsr memdecomp
.endmacro

.if MEM_DECOMP_TO_API
    .macro MEMDECOMP_TO source_lo, source_hi, dest_lo, dest_hi
            sec
            lda dest_lo
            sta decdestlo
            lda dest_hi
            sta decdesthi
            ldx source_lo
            ldy source_hi
            jsr memdecomp
    .endmacro
.endif; MEM_DECOMP_TO_API
.endif; MEM_DECOMP_API

.if UNINSTALL_API
; Uninstall the loader
; in:  nothing
; out: undefined
.import uninstall
.macro LOADER_UNINSTALL
            jsr uninstall
.endmacro
.endif; UNINSTALL_API


; linker-generated loader segments symbols

.import __DISKIO_ZP_START__
.import __DISKIO_ZP_END__
.import __DISKIO_ZP_LOAD__
.import __DISKIO_ZP_RUN__
.import __DISKIO_ZP_SIZE__

.import __DISKIO_INSTALL_START__
.import __DISKIO_INSTALL_END__
.import __DISKIO_INSTALL_LOAD__
.import __DISKIO_INSTALL_RUN__
.import __DISKIO_INSTALL_SIZE__

.import __DISKIO_START__
.import __DISKIO_END__
.import __DISKIO_LOAD__
.import __DISKIO_RUN__
.import __DISKIO_SIZE__

.endif; !__NO_LOADER_SYMBOLS_IMPORT


.ifndef __NOIMPORTVARS
    .importzp loadstatus

    .importzp loadaddrlo
    .importzp loadaddrhi

    .importzp loadaddroffslo
    .importzp loadaddroffshi

    .importzp decdestlo
    .importzp decdesthi

    .importzp endaddrlo
    .importzp endaddrhi

    .importzp bytesloadedlo
    .importzp bytesloadedhi
.endif  ; !__NOIMPORTVARS

.if PLATFORM = diskio::platform::COMMODORE_16

    .include "cpu.inc"
    .include "ted.inc"

    IO_PORT_DIR_COMMON  = IO_PORT_SERIAL_DATA_IN_INPUT | IO_PORT_SERIAL_CLK_IN_INPUT | IO_PORT_CST_MTR_OUTPUT | IO_PORT_SERIAL_ATN_OUT_OUTPUT | IO_PORT_SERIAL_CLK_OUT_OUTPUT | IO_PORT_SERIAL_DATA_OUT_OUTPUT; $0f

                                             ; effectively, this is the KERNAL flag:
                                             ; 0 = input  = KERNAL,
                                             ; 1 = output = loader
    IO_PORT_DIR_KERNAL  = IO_PORT_DIR_COMMON | IO_PORT_CST_RD_INPUT ; $0f

    IO_PORT_DIR_OPERATE = IO_PORT_DIR_COMMON | IO_PORT_CST_RD_OUTPUT; $1f

    .macro INSTALL_IDLE
            lda #IO_PORT_CST_MTR | IO_PORT_SERIAL_ATN_OUT | IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_DATA_OUT; $0f
            sta IO_PORT
            lda #IO_PORT_DIR_OPERATE
            sta IO_PORT_DIRECTION
    .endmacro

    .macro CLEAR
            lda #IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_DATA_OUT) | (0 & IO_PORT_SERIAL_CLK_OUT) | IO_PORT_SERIAL_ATN_OUT; $0c
            sta IO_PORT
    .endmacro

    .macro UPLOAD_SEND
            lda #IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_DATA_OUT) | IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_ATN_OUT; $0e
            sta IO_PORT
    .endmacro

    .macro PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            .local singleclk

            lda TED_CHARGEN_ADDR
            and #FORCE_SINGLE_CLOCK
            pha
            bne singleclk
            lda #FORCE_SINGLE_CLOCK
            php
            sei                  ; 2
            ora TED_CHARGEN_ADDR ; 4
            sta TED_CHARGEN_ADDR ; 4
            plp                  ; 4
singleclk:                       ; = 14
    .endmacro

    .macro POP_CLOCKCONFIG
            .local singleclk

            pla
            bne singleclk
            lda #.lobyte(~FORCE_SINGLE_CLOCK)
            php
            sei                  ; 2
            and TED_CHARGEN_ADDR ; 4
            sta TED_CHARGEN_ADDR ; 4
            plp                  ; 4
singleclk:                       ; = 14
    .endmacro

    .macro SENDBYTE
            ; does not clobber y

            .local sendbyte
            .local bitset

            tax
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            txa
            ldx #$07
sendbyte:   lsr
            pha
            lda IO_PORT
            ora #IO_PORT_SERIAL_ATN_OUT | IO_PORT_SERIAL_CLK_OUT
            bcs bitset
            and #.lobyte(~IO_PORT_SERIAL_CLK_OUT)
bitset:     eor #IO_PORT_SERIAL_DATA_OUT
            sta IO_PORT
            pla
            dex
            bpl sendbyte
            POP_CLOCKCONFIG
    .endmacro

    .macro SET_FLAGS_N_DATA_V_CLK
            bit IO_PORT
    .endmacro

.else ; PLATFORM <> diskio::platform::COMMODORE_16

    .include "cia.inc"
    .include "vic.inc"

    ; CIA2 DDRA ($DD02) definitions
    CIA2_DDRA_COMMON  = CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_VIC2_BANK_OUTPUT; $03
                                                                                                                              ; effectively, this is the KERNAL flag:
                                                                                                                              ; 0 = input  = loader,
    ; DATA OUT, CLK OUT, ATN OUT are clear, RS232_TXD is clear                                                                ; 1 = output = KERNAL
    CIA2_DDRA_KERNAL  = CIA2_DDRA_COMMON | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_RS232_TXD_OUTPUT; $3f

    ; DATA OUT, CLK OUT, ATN OUT are set, RS232_TXD input, all bits except VIC bank bits are inputs so that $DD00 writes do not change the bus state
    CIA2_DDRA_IDLE    = CIA2_DDRA_COMMON | CIA_SERIAL_ATN_OUT_INPUT  | CIA_SERIAL_CLK_OUT_INPUT  | CIA_SERIAL_DATA_OUT_INPUT  | CIA_RS232_TXD_INPUT ; $03

    ; DATA OUT and CLK OUT are clear, ATN OUT is set, RS232_TXD is input
    CIA2_DDRA_CLEAR   = CIA2_DDRA_COMMON | CIA_SERIAL_ATN_OUT_INPUT  | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_RS232_TXD_INPUT ; $33

    ; DATA OUT, CLK OUT and ATN OUT are clear, RS232_TXD is input
    CIA2_DDRA_RECEIVE = CIA2_DDRA_COMMON | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_RS232_TXD_INPUT ; $3b

    ; DATA OUT is clear, CLK OUT and ATN OUT are set, RS232_TXD is input
    CIA2_DDRA_SEND    = CIA2_DDRA_COMMON | CIA_SERIAL_ATN_OUT_INPUT  | CIA_SERIAL_CLK_OUT_INPUT  | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_RS232_TXD_INPUT ; $23

    .macro INSTALL_IDLE
            lda #CIA2_DDRA_IDLE
            sta CIA2_DDRA
    .endmacro

    .macro CLEAR
            lda #CIA2_DDRA_CLEAR
            sta CIA2_DDRA
    .endmacro

    .macro UPLOAD_SEND
            lda #CIA2_DDRA_SEND
            sta CIA2_DDRA
    .endmacro

    .macro SET_FLAGS_N_DATA_V_CLK
            bit CIA2_PRA
    .endmacro

    .macro PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            lda VIC2_C128_CLOCK
            pha
            lda #0
            sta VIC2_C128_CLOCK
    .endmacro

    .macro POP_CLOCKCONFIG
            pla
            sta VIC2_C128_CLOCK
    .endmacro

    .macro SENDBYTE
            ; does not clobber y

            .local sendbyte
            .local bitset

        .if USE_2_MHZ
            tax
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
            txa
        .endif
            ldx #$07
sendbyte:   lsr
            pha
            pha; delay
        .if NTSC_COMPATIBILITY
            nop
        .endif
            lda CIA2_DDRA
            and #.lobyte(~(SERIAL_ATN_OUT | SERIAL_CLK_OUT))
            bcs bitset
            ora #SERIAL_CLK_OUT
bitset:     eor #SERIAL_DATA_OUT
            sta CIA2_DDRA
            pla; delay
            pla
            dex
            bpl sendbyte
        .if USE_2_MHZ
            POP_CLOCKCONFIG
        .endif
    .endmacro

.endif ; PLATFORM <> diskio::platform::COMMODORE_16

.macro UPLOAD_CUSTOM_DRIVE_CODE from, to, length, entry

            .local waitupload
            .local uploadparm
            .local uploadcode
            .local noinchibyt
            .local sendbyte
            .local uplodprms
            .local uplodcntl
            .local uplodcnth
            .local waitdone
            .local uploaddone

            lda #.lobyte(from)
            sta uploadcode + 1
            lda #.hibyte(from)
            sta uploadcode + 2
            lda #.lobyte(length)
            sta uplodcntl
            lda #.hibyte(length)
            sta uplodcnth

            UPLOAD_SEND

waitupload: SET_FLAGS_N_DATA_V_CLK
            bmi waitupload

            CLEAR

            ldy #6
uploadparm: lda uplodprms - 1,y
            jsr sendbyte
            dey
            bne uploadparm

uploadcode: lda $0000,y
            jsr sendbyte
            iny
            bne noinchibyt
            inc uploadcode + 2
noinchibyt: dec uplodcntl
            bne uploadcode
            dec uplodcnth
            bpl uploadcode

waitdone:   SET_FLAGS_N_DATA_V_CLK
            bvs waitdone

            INSTALL_IDLE

            jmp uploaddone

sendbyte:   pha
            nop; delay
            pla
            SENDBYTE
            rts

uplodprms:  .byte .lobyte(entry)
            .byte .hibyte(entry)
uplodcntl:  .byte 0
uplodcnth:  .byte 0
            .byte .lobyte(to)
            .byte .hibyte(to)

uploaddone:
.endmacro

.macro RETURNTOLOADER41
            .importzp CHECKLOADREQUEST41
            .importzp NUMFILES41
            .importzp REQUESTEDSECTOR41
            .importzp TRACKLINKTABLE41

            .import topofstack41
            .import idleloop41
            .import BLOCKBUFFER41

            .local clearlinks

            .assert * >= BLOCKBUFFER41, error, "Return to 1541 loader code too low in memory"

            ldx #$18; ATNA_OUT | CLK_OUT
            stx $1800
            inx     ; $19 = >(TIMER2 + $0100)
            stx CHECKLOADREQUEST41
            lda #$fe; must be negative but not $ff
            sta NUMFILES41; reload directory
            sta REQUESTEDSECTOR41; collect links
            ldx #21 - 1; MAXNUMSECTORS
clearlinks: sta TRACKLINKTABLE41,x; clear links
            dex
            bpl clearlinks
            ldx #.lobyte(topofstack41)
            txs
            jmp idleloop41

            .assert * < (BLOCKBUFFER41 + $0100), error, "Return to 1541 loader code too high in memory"
.endmacro

.macro RETURNTOLOADER71
            .importzp NUMFILES71
            .importzp REQUESTEDSECTOR71
            .importzp TRACKLINKTABLE71

            .import topofstack71
            .import idleloop71
            .import collswit71
            .import BLOCKBUFFER71

            .local clearlinks

            .assert * >= BLOCKBUFFER71, error, "Return to 1571 loader code too low in memory"

            lda #$18 ; ATNA_OUT | CLK_OUT
            sta $1800; VIA1_PRB
            lda #$fe; must be negative but not $ff
            sta NUMFILES71; reload directory
            sta REQUESTEDSECTOR71; collect links
            ldx #21 - 1; MAXNUMSECTORS
clearlinks: sta TRACKLINKTABLE71,x
            dex
            bpl clearlinks
            lda #OPC_BIT_ZP
            sta collswit71
            ldx #.lobyte(topofstack71)
            txs
            jmp idleloop71

            .assert * < (BLOCKBUFFER71 + $0100), error, "Return to 1571 loader code too high in memory"
.endmacro

.macro RETURNTOLOADER81
            .import dcodinit81
            .import BLOCKBUFFER81

            .assert * >= BLOCKBUFFER81, error, "Return to 1581 loader code too low in memory"

            jmp dcodinit81

            .assert * < (BLOCKBUFFER81 + $0100), error, "Return to 1581 loader code too high in memory"
.endmacro

.popseg

.if (ALLOW_2_MHZ_ON_C128 & (PLATFORM = diskio::platform::COMMODORE_64)) | (PLATFORM = diskio::platform::COMMODORE_128)
    USE_2_MHZ = 1
.else
    USE_2_MHZ = 0
.endif

.if (DECOMPRESSOR <> DECOMPRESSORS::NONE)
    HAVE_DECOMPRESSOR = 1
.else
    HAVE_DECOMPRESSOR = 0
.endif

.if LOAD_COMPD_API
    BYTESTREAM = 1
    EXCEPTIONS = 1
.else
    BYTESTREAM = 0
    EXCEPTIONS = 0
.endif

.if LOAD_COMPD_API & LOAD_TO_API
    LOADCOMPD_TO = 1
.else
    LOADCOMPD_TO = 0
.endif

.if (DECOMPRESSOR = DECOMPRESSORS::BITNAX) | (DECOMPRESSOR = DECOMPRESSORS::DOYNAX_LZ) | (DECOMPRESSOR = DECOMPRESSORS::TINYCRUNCH)
    HAVE_BLOCK_INTERFACE = 1
.else
    HAVE_BLOCK_INTERFACE = 0
.endif

.if LOAD_RAW_API | (LOAD_VIA_KERNAL_FALLBACK & HAVE_BLOCK_INTERFACE)
    HAVE_POLLBLOCK = 1
.else
    HAVE_POLLBLOCK = 0
.endif

.if LOAD_VIA_KERNAL_FALLBACK & (HAVE_POLLBLOCK | (PLATFORM = diskio::platform::COMMODORE_128))
    HAVE_KERNAL_POLLBLOCK = 1
.else
    HAVE_KERNAL_POLLBLOCK = 0
.endif

.if (BYTESTREAM | HAVE_DECOMPRESSOR) & ((HAVE_BLOCK_INTERFACE = 0) | MEM_DECOMP_API | LOAD_VIA_KERNAL_FALLBACK)
    HAVE_GETC = 1
.else
    HAVE_GETC = 0
.endif

.if (BYTESTREAM | HAVE_DECOMPRESSOR) & (HAVE_GETC = 0)
    GETCZPPOINTERS = 1
.else
    GETCZPPOINTERS = 0
.endif


.if LOAD_COMPD_API & (!HAVE_DECOMPRESSOR)
    .error "***** Option LOAD_COMPD_API requires DECOMPRESSOR != DECOMPRESSORS::NONE *****"
.endif

.if MEM_DECOMP_API & (!HAVE_DECOMPRESSOR)
    .error "***** Option MEM_DECOMP_API requires DECOMPRESSOR != DECOMPRESSORS::NONE *****"
.endif

.if MEM_DECOMP_TO_API & (!MEM_DECOMP_API)
    .error "***** Option MEM_DECOMP_TO_API requires MEM_DECOMP_API *****"
.endif

.if HAVE_DECOMPRESSOR & (!LOAD_COMPD_API) & (!MEM_DECOMP_API)
    .error "***** Decompressor included but not used, as neither LOAD_COMPD_API nor MEM_DECOMP_API are enabled *****"
.endif


.if (!LOAD_RAW_API) & (!LOAD_COMPD_API)
    .error "***** No actual loading calls enabled, please select LOAD_RAW_API and/or LOAD_COMPD_API *****"
.endif


.if (FILENAME_MAXLENGTH < 1) | (FILENAME_MAXLENGTH > 16)
    .error "***** Invalid FILENAME_MAXLENGTH setting, please set it to 1..16 *****"
.endif


.if LOAD_VIA_KERNAL_FALLBACK
    .if DIRTRACK <> 18
        .error "***** Option LOAD_VIA_KERNAL_FALLBACK requires DIRTRACK to be 18 *****"
    .endif
    .if DIRTRACK81 <> 40
        .error "***** Option LOAD_VIA_KERNAL_FALLBACK requires DIRTRACK81 to be 40 *****"
    .endif

    .if ONLY_1541_AND_COMPATIBLE
        .error "***** Option LOAD_VIA_KERNAL_FALLBACK requires ONLY_1541_AND_COMPATIBLE = 0 *****"
    .endif

    .if ASYNCHRONOUS_BURST_HANDSHAKE
        .error "***** Option LOAD_VIA_KERNAL_FALLBACK requires ASYNCHRONOUS_BURST_HANDSHAKE = 0 *****"
    .endif

    .macro CHECK_LOADER_ZP_ADDRESSES
        .assert (loader_zp_first > STATUS) || (loader_zp_last < STATUS), error, "Loader zeropage variables cross KERNAL STATUS register at $90"
        .assert (loader_zp_first > C3PO) || (loader_zp_last < C3PO), error, "Loader zeropage variables cross KERNAL serial buffer output char buffered flag C3PO at $94"
        .assert (loader_zp_first > BSOUR) || (loader_zp_last < BSOUR), error, "Loader zeropage variables cross KERNAL serial buffer byte BSOUR at $95"

        .if PLATFORM = diskio::platform::COMMODORE_16
        .assert (loader_zp_first > LDTND) || (loader_zp_last < LDTND), error, "Loader zeropage variables cross KERNAL logical file index/open files count at $97"
        .assert (loader_zp_first > DFLTN) || (loader_zp_last < DFLTN), error, "Loader zeropage variables cross KERNAL input device variable DFLTN at $98"
        .assert (loader_zp_first > DFLTO) || (loader_zp_last < DFLTO), error, "Loader zeropage variables cross KERNAL output device variable DFLTO at $99"
        .assert (loader_zp_first > R2D2) || (loader_zp_last < R2D2), error, "Loader zeropage variables cross KERNAL serial bus EOI flag R2D2 at $a6"
        .assert (loader_zp_first > BSOUR1) || (loader_zp_last < BSOUR1), error, "Loader zeropage variables cross KERNAL serial bus shift counter BSOUR1 at $a8"
        .assert (loader_zp_first > COUNT) || (loader_zp_last < COUNT), error, "Loader zeropage variables cross KERNAL serial bus counter COUNT at $aa"
        .assert (loader_zp_first > USE4DY) || (loader_zp_last < USE4DY), error, "Loader zeropage variables cross KERNAL parallel drive state register USE4DY at $f9"
        .else; PLATFORM <> diskio::platform::COMMODORE_16
        .assert (loader_zp_first > LDTND) || (loader_zp_last < LDTND), error, "Loader zeropage variables cross KERNAL logical file index/open files count at $98"
        .assert (loader_zp_first > DFLTN) || (loader_zp_last < DFLTN), error, "Loader zeropage variables cross KERNAL input device variable DFLTN at $99"
        .assert (loader_zp_first > DFLTO) || (loader_zp_last < DFLTO), error, "Loader zeropage variables cross KERNAL output device variable DFLTO at $9a"
        .assert (loader_zp_first > R2D2) || (loader_zp_last < R2D2), error, "Loader zeropage variables cross KERNAL serial bus EOI flag R2D2 at $a3"
        .assert (loader_zp_first > BSOUR1) || (loader_zp_last < BSOUR1), error, "Loader zeropage variables cross KERNAL serial bus shift counter BSOUR1 at $a4"
        .assert (loader_zp_first > COUNT) || (loader_zp_last < COUNT), error, "Loader zeropage variables cross KERNAL serial bus counter COUNT at $a5"
        .endif; PLATFORM <> diskio::platform::COMMODORE_16
    .endmacro
.else
    .macro CHECK_LOADER_ZP_ADDRESSES
    .endmacro
.endif

.endif; _LOADER_INC_
