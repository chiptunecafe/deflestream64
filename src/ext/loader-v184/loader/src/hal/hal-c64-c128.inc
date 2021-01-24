
.ifndef _HAL_C64_INC_
_HAL_C64_INC_ = 1

.include "cia.inc"


.macro CHECK_INSTALL_END_ADDRESS
    .assert * <= $d000, error, "Install code exceeds $d000, please make sure the DISKIO_INSTALL segment ends below $d000"
.endmacro

.if LOAD_VIA_KERNAL_FALLBACK
    .macro CHECK_RESIDENT_START_ADDRESS
         RESIDENT_START_ADDRESS = *
    .endmacro

    .macro CHECK_RESIDENT_END_ADDRESS
        .assert (RESIDENT_START_ADDRESS > ENABL) || (* < ENABL), error, "Resident code crosses KERNAL RS232 state variable ENABL"

        .assert * <= $d000, error, "Resident code exceeds $d000, please make sure the DISKIO segment ends below $d000"
    .endmacro
.else
    .macro CHECK_RESIDENT_START_ADDRESS
        .assert (* <= $d000) || (* >= $e000), error, "Resident code resides at $d000..$dfff, please make sure the DISKIO segment does not overlap with that memory range"
    .endmacro

    .macro CHECK_RESIDENT_END_ADDRESS
        .assert (* <= $d000) || (* >= $e000), error, "Resident code resides at $d000..$dfff, please make sure the DISKIO segment does not overlap with that memory range"
    .endmacro
.endif


.macro OK_CLC
            ; use illegals to save size
            alr #diskio::status::OK; $00, clc = all ok
.endmacro


.macro PREPARE_DRIVE_DISTURBANCE_VALIDATION
            ; nothing to do
.endmacro

.macro BRANCH_IF_DRIVE_DOES_NOT_DISTURB_SERIAL_BUS to
            ; nothing to do
.endmacro


.if LOAD_VIA_KERNAL_FALLBACK
    .macro PREPARE_PARALLEL_CHECK
            ; nothing to do
    .endmacro
.endif

.if PLATFORM <> diskio::platform::COMMODORE_128

    .if LOAD_VIA_KERNAL_FALLBACK
        .macro ENABLE_KERNAL_SERIAL_ROUTINES
            lda #MEMCONFIG_IO_KERNAL_BASIC
            sta IO_PORT
        .endmacro

        .macro ENABLE_KERNAL_SERIAL_ROUTINES_Y
            ldy #MEMCONFIG_IO_KERNAL_BASIC
            sty IO_PORT
        .endmacro
    .endif; LOAD_VIA_KERNAL_FALLBACK

    .if LOAD_UNDER_D000_DFFF | LOAD_VIA_KERNAL_FALLBACK
        .macro ENABLE_IO_SPACE
            lda #MEMCONFIG_IO
            sta IO_PORT
        .endmacro

        .macro ENABLE_IO_SPACE_Y
            ldy #MEMCONFIG_IO
            sty IO_PORT
        .endmacro

        .macro ENABLE_ALL_RAM
            lda #MEMCONFIG_ALL_RAM
            sta IO_PORT
        .endmacro

        .macro ENABLE_ALL_RAM_X
            ldx #MEMCONFIG_ALL_RAM
            stx IO_PORT
        .endmacro

        .macro ENABLE_ALL_RAM_Y
            ldy #MEMCONFIG_ALL_RAM
            sty IO_PORT
        .endmacro

        .macro GET_MEMCONFIG
            lda IO_PORT
        .endmacro

        .macro SET_MEMCONFIG
            sta IO_PORT
        .endmacro

        .macro SET_MEMCONFIG_X
            stx IO_PORT
        .endmacro

        .macro SET_MEMCONFIG_Y
            sty IO_PORT
        .endmacro
    .endif; !(LOAD_UNDER_D000_DFFF | LOAD_VIA_KERNAL_FALLBACK)

.else; PLATFORM = diskio::platform::COMMODORE_128

    .include "mmu.inc"

    .macro DISABLE_BURST_MODE
            lda #IOMODE_OUTPUT
            ora CIA1_CRA
            sta CIA1_CRA
    .endmacro

    .if LOAD_VIA_KERNAL_FALLBACK
        .macro ENABLE_KERNAL_SERIAL_ROUTINES
            lda #BANK_0 | SYSTEM_ROM | MID_RAM | LOW_RAM | IO_SPACE
            sta MMU_CR
        .endmacro

        .macro ENABLE_KERNAL_SERIAL_ROUTINES_Y
            ldy #BANK_0 | SYSTEM_ROM | MID_RAM | LOW_RAM | IO_SPACE
            sty MMU_CR
        .endmacro

        .macro START_BURST_LOAD
            bit CIA1_ICR
            lda #SERIAL_CLK_OUT
            eor CIA2_PRA
            sta CIA2_PRA
        .endmacro

        .macro GET_BURST_BYTE
            .local waitburst

            lda #SERIAL_IRQ
waitburst:  bit CIA1_ICR
            beq waitburst
            ldy CIA1_SDR
            lda #SERIAL_CLK_OUT
            php
            sei
            eor CIA2_PRA
            sta CIA2_PRA
            plp
            tya
        .endmacro
    .endif; LOAD_VIA_KERNAL_FALLBACK

    .if LOAD_UNDER_D000_DFFF | LOAD_VIA_KERNAL_FALLBACK
        .macro ENABLE_IO_SPACE
            lda #BANK_0 | HIGH_RAM | MID_RAM | LOW_RAM | IO_SPACE
            sta MMU_CR
        .endmacro

        .macro ENABLE_IO_SPACE_Y
            ldy #BANK_0 | HIGH_RAM | MID_RAM | LOW_RAM | IO_SPACE
            sty MMU_CR
        .endmacro

        .macro ENABLE_ALL_RAM
            lda #BANK_0 | HIGH_RAM | MID_RAM | LOW_RAM | RAM_ROM
            sta MMU_CR
        .endmacro

        .macro ENABLE_ALL_RAM_X
            ldx #BANK_0 | HIGH_RAM | MID_RAM | LOW_RAM | RAM_ROM
            stx MMU_CR
        .endmacro

        .macro ENABLE_ALL_RAM_Y
            ldy #BANK_0 | HIGH_RAM | MID_RAM | LOW_RAM | RAM_ROM
            sty MMU_CR
        .endmacro

        .macro GET_MEMCONFIG
            lda MMU_CR
        .endmacro

        .macro SET_MEMCONFIG
            sta MMU_CR
        .endmacro

        .macro SET_MEMCONFIG_X
            stx MMU_CR
        .endmacro

        .macro SET_MEMCONFIG_Y
            sty MMU_CR
        .endmacro
    .endif; !(LOAD_UNDER_D000_DFFF | LOAD_VIA_KERNAL_FALLBACK)

.endif; PLATFORM = diskio::platform::COMMODORE_128

; CIA2_DDRA_*, PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK, POP_CLOCKCONFIG, SET_FLAGS_N_DATA_V_CLK, CLEAR, SENDBYTE and INSTALL_IDLE
; are defined in loader/include/loader.inc,
; as they are required for custom drive code upload

; RS232 TXD set to output so this bit won't interfere with byte fetch from serial bus
CLOCK        = CIA2_DDRA_COMMON | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_RS232_TXD_OUTPUT; $37
CLOCK_ATN_HI = CLOCK | CIA_SERIAL_ATN_OUT_INPUT ; 1st and 3rd bit pairs, $37
CLOCK_ATN_LO = CLOCK | CIA_SERIAL_ATN_OUT_OUTPUT; 2nd and 4th bit pairs, $3f

.macro ENABLE_WAITBUSY_KERNAL
            lda #.lobyte(~(SERIAL_DATA_OUT | SERIAL_CLK_OUT))
            and CIA2_PRA
            sta CIA2_PRA
.endmacro

.macro INIT_CLEAR_ATN_OUT_CLEAR_CLK_OUT_CLEAR_DATA_OUT
            ; this is executed during install and openfile if IDLE_BUS_LOCK != 0
            ; hence it accesses the port register rather than the data direction register
            lda #.lobyte(~(SERIAL_ATN_OUT | SERIAL_CLK_OUT | SERIAL_DATA_OUT | RS232_TXD))
            and CIA2_PRA
            sta CIA2_PRA
.endmacro

.if LOAD_VIA_KERNAL_FALLBACK
            KERNALFILENO = $02

    .macro CLEAR_DATA_OUT_CLEAR_CLK_OUT_ASSERT_ATN
            lda CIA2_PRA
            and #.lobyte(~(SERIAL_DATA_OUT | SERIAL_CLK_OUT))
            ora #SERIAL_ATN_OUT
            sta CIA2_PRA
    .endmacro
.endif

.macro BRANCH_IF_INSTALLED to
            lda #CIA2_DDRA_KERNAL
            cmp CIA2_DDRA
            bne to
.endmacro

.macro BRANCH_IF_NOT_INSTALLED to
            lda #CIA2_DDRA_KERNAL
            cmp CIA2_DDRA
            beq to
.endmacro

.macro CHECK_AND_BRANCH_IF_DRIVE_PARALLEL to
            lda IOPEN + 1
            cmp #.hibyte($df00); serial: AR fastload vectors point to $DFxx, parallel: IDE64 vectors point to to $DExx, IEEE-488 interfaces to $Cxxx
            bcc to
.endmacro

.macro BRANCH_IF_DRIVE_PARALLEL to
            CHECK_AND_BRANCH_IF_DRIVE_PARALLEL to
.endmacro

.macro IDLE
            ldx #CIA2_DDRA_IDLE
            stx CIA2_DDRA
.endmacro

.macro WAKEUP
        .if IDLE_BUS_LOCK
            ; when the loader is idle, the user is
            ; allowed to write anything to CIA2_DDRA ($DD00) -
            ; set it to a known and valid state here.
            ;php; without these, there's a race condition and thus a small
            ;sei; chance for video glitches, but with these, there's a
            INIT_CLEAR_DATA_OUT_CLEAR_CLK_OUT_CLEAR_ATN_OUT
            ;plp; chance for actual crashes with stable raster routines
        .endif
            CLEAR
.endmacro

.macro POLL_BLOCK device_not_present
            lda #diskio::status::DEVICE_NOT_PRESENT; $fe
            WAIT_FOR_BLOCK_READY
            cmp CIA2_PRA; to be safe, read a second time
            bpl device_not_present; branches with carry set if device not present
.endmacro

.macro SEND_BLOCK_SIGNAL
            ldy #CIA2_DDRA_RECEIVE; use y to ensure that y < $fe when
            sty CIA2_DDRA         ; calling getbyte for the 2 control bytes
    .if USE_2_MHZ
            PUSH_CLOCKCONFIG_AND_FORCE_SLOW_CLOCK
    .endif
            jsr retrnokclc; delay
    .if (PLATFORM = diskio::platform::COMMODORE_128) & USE_ASYNCHRONOUS_BURST_HANDSHAKE
            lda #OPC_BVS
            sta burstwait
          .if LOAD_UNDER_D000_DFFF
            sta burstwaitio
          .endif
    .endif; !((PLATFORM = diskio::platform::COMMODORE_128) & USE_ASYNCHRONOUS_BURST_HANDSHAKE)
.endmacro

.macro GETBYTE_SETUP
            lda #CLOCK_ATN_HI
            sta GETBYTE_CLOCK_ATN_HI
.endmacro

.macro GETBYTE label, store
            .local entry
            .local getloop
            .local merge
            .local out

label:      sec; carry needs to be set upon return

    .if PLATFORM = diskio::platform::COMMODORE_128
            .local burstloop
            .local storeburst
            .local waitburst
            .local noburst

        .if .not .xmatch (store, STOREBYTE_ALLRAM)
            .define burstswit burstwait
        .else
            .define burstswit burstwaitio
        .endif

            bit CIA1_CRA
            bvs noburst; branch if IO mode is set to output

        .if .not .xmatch (store, STOREBYTE_ALLRAM)
            lda storebyte + 0
            sta storeburst + 0
            lda storebyte + 1
            sta storeburst + 1
            lda storebyte + 2
            sta storeburst + 2
        .else
            lda storebytio + 1
            sta storbytiob + 1
            lda storebytio + 2
            sta storbytiob + 2
        .endif
            iny

        .if USE_ASYNCHRONOUS_BURST_HANDSHAKE

burstloop:  lda #OPC_BVC ^ OPC_BVS; use the CLK line for the incoming data sent signal rather
            eor burstswit         ; than the SERIAL_IRQ flag in CIA1_ICR ($dc0d) in order to
            sta burstswit         ; avoid interference by CIA1 interrupt handlers
            lda CIA2_DDRA         ; and use the ATN line for the data taken signal, as DATA is
            eor #SERIAL_ATN_OUT   ; reserved for incoming bits (also to keep passive drives from
            tax                   ; leaving the silencing loop after too long a period of ATN clear)
waitburst:  bit CIA2_PRA
burstswit:  bvs waitburst         ; wait for incoming data sent signal
            lda CIA1_SDR
            stx CIA2_DDRA         ; signal data taken

        .else; !USE_ASYNCHRONOUS_BURST_HANDSHAKE

burstloop:  lda CIA2_DDRA         ; use the ATN line for the data taken signal in
            eor #SERIAL_ATN_OUT   ; order to keep passive drives from leaving the
            tax                   ; silencing loop after too long a period of ATN clear
            lda #SERIAL_IRQ
waitburst:  bit CIA1_ICR
            beq waitburst
            lda CIA1_SDR
            stx CIA2_DDRA         ; signal data taken

        .endif; !USE_ASYNCHRONOUS_BURST_HANDSHAKE

        .if .not .xmatch (store, STOREBYTE_ALLRAM)
storeburst: sta $0000,y
        .else
            ENABLE_ALL_RAM_X
storbytiob: sta $0000,y
            ldx memconfig + 1
            SET_MEMCONFIG_X
        .endif
            iny
            bne burstloop

            lda #CLOCK_ATN_HI
            sta CIA2_DDRA
            bne out; jmp

noburst:
    .endif; PLATFORM = diskio::platform::COMMODORE_128

            bcs entry; jmp

            ; note: between each store to CIA2_DDRA to signal ready for the next bit pair,
            ;       10 cycles must pass before read access of CIA2_PRA:
            ;       the drive needs max. 14 cycles to respond with a loop in the form of 'bit VIA1_PRB : bpl/bmi * - 3 : sta VIA1_PRB' -
            ;       this means that 18 cycles per bit pair are the minimum

getloop:    lsr                                 ; 2 - -3210HHH
            lsr                                 ; 2 - --3210HH:H
    .if NTSC_COMPATIBILITY
            ldx GETBYTE_CLOCK_ATN_HI ; $37      ; 3
    .else
            ldx #CLOCK_ATN_HI ; $37             ; 2
    .endif
            ora CIA2_PRA - CLOCK_ATN_HI,x       ; 5 - 543210HH - delay
            stx CIA2_DDRA; sync 3: set ATN high ; 4
                                                ; = 18

            ror                                 ; 2 - H543210H:H
            ror                                 ; 2 - HH543210:H
            ldx #CLOCK_ATN_LO ; $3f             ; 2
            sax merge + $01                     ; 4 - --543210
    .if NTSC_COMPATIBILITY
            and CIA2_PRA - CLOCK_ATN_LO,x       ; 4 - 7654.2..
    .else
            and CIA2_PRA                        ; 4 - 7654.2..
    .endif
            stx CIA2_DDRA; sync 4: set ATN low  ; 4
                                                ; = 18

merge:      ora #$00                            ; 2 - 76543210
            store                               ; 5 - sta mem16,y
entry:      lax GETBYTE_CLOCK_ATN_HI ; $37      ; 3
    .if NTSC_COMPATIBILITY
            adc CIA2_PRA - CLOCK_ATN_HI,x       ; 5 - 10HHH...
    .else
            adc CIA2_PRA                        ; 4 - 10HHH...
    .endif
            stx CIA2_DDRA; sync 1: set ATN high ; 4
                                                ; = 18

            iny                                 ; 2
            beq out                             ; 2
            lsr                                 ; 2 - -10HHH..
            lsr                                 ; 2 - --10HHH.
            ldx #CLOCK_ATN_LO ; $3f             ; 2
    .if NTSC_COMPATIBILITY
            ora CIA2_PRA - CLOCK_ATN_LO,x       ; 5 - 3210HHH.
    .else
            ora CIA2_PRA                        ; 4 - 3210HHH.
    .endif
            stx CIA2_DDRA; sync 2: set ATN low  ; 4
                                                ; = 18

            jmp getloop                         ; 3
out:       ;clc
.endmacro

.macro STOREBYTE_ALLRAM
            ENABLE_ALL_RAM_X
storebytio: sta $0000,y
memconfig:  ldx #$00
            SET_MEMCONFIG_X
.endmacro

.macro ENDGETBLOCK
    .if USE_2_MHZ
            tax; special block number (irrelevant for regular blocks)
            POP_CLOCKCONFIG
            txa; special block number (irrelevant for regular blocks)
    .endif
.endmacro

.macro SET_IO_KERNAL
            lda #CIA2_DDRA_KERNAL
            sta CIA2_DDRA
.endmacro

.endif; !_HAL_C64_INC_
