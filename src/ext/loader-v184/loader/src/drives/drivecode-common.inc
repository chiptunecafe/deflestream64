
.ifndef _DRIVECODE_COMMON_INC_
_DRIVECODE_COMMON_INC_ = 1

MIN_DEVICE_NO   =  8 ; these two settings define the device number range when scanning for devices in install.s,
MAX_DEVICE_NO   = 30 ; devices beyond this range will not be recognized or useable

; 1541/41-C/41-II/41U/70/71/71CR
MINSTEPSPEED    = $18 ; min. R/W head stepping speed
MAXSTEPSPEED    = $10 ; max. R/W head stepping speed
STEPPERACC      = $0c ; R/W head stepping acceleration, smaller is slower acceleration
SINGLESTEPSPEED = $0c ; time between two consecutive half-track steps for a single-track step

; 1541/41-C/41-II/41U/70/71/71CR, single disk side
.define SECTORS_ON_TRACK(T) (21 - ((.min(T, 18) / 18) * 2) - (T / 25) - (T / 31))

.macro ENABLE_WATCHDOG
    .if DISABLE_WATCHDOG
            sei
    .else
            cli
    .endif
.endmacro

.macro DRIVEGETBYTE comparelbl
            ; must not clobber x
            lda #%10000000
            ldy VIA1_PRB + $10; low-byte must not be 0 for custom drive code upload
:           cpy VIA1_PRB + $10
            beq :-
            ldy VIA1_PRB + $10
comparelbl: cpy #ATN_IN | ATNA_OUT | CLK_IN
            ror
            bcc :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in getbyte loop, fatal cycle loss. *****"
.endmacro

.macro GET_FILENAME drivetype
   .if .not .xmatch (drivetype, 1581)
           ;ldx #0
            lda #ATNA_OUT; ATNA_OUT set, CLK_OUT and DATA_OUT clear: drive is ready
            sta (V1B,x); VIA1_PRB
            lda #CLK_IN
            and (V1B,x); VIA1_PRB
            bne getcustom
getfilenam: lda #$ff
            sta TIMER; reset watchdog time-out
            ENABLE_WATCHDOG
    .else; 1581
            ldx #0
            stx CIA_PRB; CLK_OUT and DATA_OUT clear: drive is ready
            lda #CLK_IN
            and CIA_PRB
            beq getfilenam
            jmp getcustom
getfilenam:
    .endif
            jsr dgetbyte; get filename
            beq :+
    .if .not .xmatch (drivetype, 1581)
            sta .lobyte(FILENAME),x
    .else; 1581
            sta filename,x
    .endif; 1581
            inx
            cpx #FILENAME_MAXLENGTH
            bcc getfilenam
:
    .if .not .xmatch (drivetype, 1581)
            lda #ERR | ATNA_OUT | CLK_OUT; drive busy
            sta VIA1_PRB
            sta ERRORCOUNT; reset, as spinning up the motor would yield errors
    .else
            lda #CLK_OUT; drive busy
            sta CIA_PRB
    .endif
            sei; disable watchdog
            txa              ; if zero-length filename,
            beq loadnextfile ; use hash values of next file
            jsr gethashval
            sta FILENAMEHASHLO
    .if .xmatch (drivetype, 1541)
            sty FILENAMEHASHHI
    .else
            lda TEMP
            sta FILENAMEHASHHI
    .endif
loadnextfile:
.endmacro; GET_FILENAME drivetype

; matches against hash of filename in FILENAMEHASHLO/HI,
; sets a to file track and y to file sector

.macro FIND_FILE drivetype
            inc prpnxtfjmp; OPC_JMP_ABS -> OPC_EOR_ABS, do not jump to after prepare-next-file routine
findfile:
    .if .not .xmatch (drivetype, 1581)
chknewdisk: lda #$00; BAM block number
            sta CYCLESTARTENDSECTOR
            ldx NUMFILES
            bpl findloop

            ; a new disk has been inserted
           ;lda #$00
        .if .xmatch (drivetype, 1541)
            ldx #$fc
            stx DIRBLOCKPOS
            jsr getblkdir; store ID, sector link sanity check
            bcc chknewdisk
        .else
            sta DIRBLOCKPOS
            ldx #.lobyte(storeid - (idswitch + 2))
            jsr getblkstid; store ID, sector link sanity check
            bcs chknewdisk
        .endif
           ;lda LINKSECTOR
           ;ldx LOADEDSECTOR; $00
           ;ldy LINKTRACK

    .else; 1581

            lda NUMFILES
            bmi newdisk
diskchangd: lda #$00
            bne newdisk
            jmp samedisk

newdisk:    ; a new disk has been inserted
            jsr getdirtrk
            ldy #$00
            sty diskchangd + 1
            sty CYCLESTARTENDTRACK
            sty CYCLESTARTENDSECTOR
            jsr getblock
            bcs newdisk

           ;lda LINKTRACK
           ;ldx LOADEDSECTOR; $00
           ;ldy LINKSECTOR
            sty FIRSTDIRSECTOR

    .endif; 1581

            ; directory cycling: fill the directory buffer with the next file entries,
            ; this is also executed upon file not found in the currently buffered directory segment
            ; (with accu containing NEXTDIRBLOCKSECTOR's value)
nextdirseg:
    .if .not .xmatch (drivetype, 1581)
            ldy #$ff
    .else
            ldx #$ff
    .endif
            bit CYCLESTARTENDSECTOR
            bpl :+ ; branch if cycle not complete

            ; file not found
    .if .xmatch (drivetype, 1541)
           ;ldy #diskio::status::FILE_NOT_FOUND; $ff
            jmp DECODETABLE + .lobyte(tosendstat)
    .else; !1541
            sec
            jmp filenotfnd
    .endif; !1541
:
    .if .not .xmatch (drivetype, 1581)
            sty NUMFILES

        .if .xmatch (drivetype, 1541)
nextdirsct: ; a = sector
            cmp #SECTORS_ON_TRACK(DIRTRACK)
            bcs chknewdisk
            jsr getblkdir; compare ID, sector link sanity check
            bcc chknewdisk
        .else
            SKIPWORD; do not set CYCLESTARTENDSECTOR
dircycle:   sta CYCLESTARTENDSECTOR
nextdirsct: ; a = sector
            jsr getblkchid; compare ID, sector link sanity check
            bcc :+
            beq chknewdisk; read error or ID mismatch
            lda REQUESTEDSECTOR71; checksum error, retry
            bpl nextdirsct; jmp
:
        .endif
           ;lda LINKSECTOR
           ;ldx LOADEDSECTOR
           ;ldy LINKTRACK
    .else; 1581
            stx NUMFILES

nextdirsct: ; a = track
            ; y = sector
            sta CURRDIRBLOCKTRACK
            jsr getblock
            bcs newdisk; start over on error
           ;ldy LINKSECTOR
           ;lda LINKTRACK
    .endif; 1581

    .if .not .xmatch (drivetype, 1581)
           ;ldy LINKTRACK
           ;cpy CURTRACK
            beq :+; branch if not wrapping to first dir block
            lda #1; wrap around to first dir sector
:           sta NEXTDIRBLOCKSECTOR
    .else; 1581
            bne :+; branch if not wrapping to first dir block
            jsr getdirtrk
            ldy FIRSTDIRSECTOR
:           sta NEXTDIRBLOCKTRACK
            sty NEXTDIRBLOCKSECTOR
    .endif; 1581

    .if .xmatch (drivetype, 1541)
getdirloop: ldy DIRBLOCKPOS
            jsr sertorawr; get file's start track
            beq notafile; skip non-files denoted by track 0
            cmp #MAXTRACK41 + 1
            bcs notafile; or files with invalid start tracks
            inc NUMFILES
            ldx NUMFILES
            sta DIRTRACKS,x
            jsr sertorawd; get file's start sector
            sta DIRSECTORS,x
    .elseif (.xmatch (drivetype, 1571))
getdirloop: ldy DIRBLOCKPOS
            lda BLOCKBUFFER + 3 + TRACKOFFSET,y; get file's start track
            beq notafile; skip non-files denoted by track 0
            cmp #MAXTRACK71 + 1
            bcs notafile; or files with invalid start tracks
            inc NUMFILES
            ldx NUMFILES
            sta .lobyte(DIRTRACKS),x
            lda BLOCKBUFFER + 3 + SECTOROFFSET,y; get file's start sector
            sta .lobyte(DIRSECTORS),x
            lda BLOCKBUFFER + BLOCKSOFFSET,y
            sta .lobyte(NUMBLOCKS),x
    .else; 1581
            ldy #$00
getdirloop: lda BLOCKBUFFER + 3 + TRACKOFFSET,y; get file's start track
            beq notafile; skip non-files denoted by track 0
            inc NUMFILES
            ldx NUMFILES
            sta DIRTRACKS,x
            lda BLOCKBUFFER + 3 + SECTOROFFSET,y; get file's start sector
            sta DIRSECTORS,x
    .endif; 1581

            jsr fnamehash
            ldx NUMFILES; x is clobbered in fnamehash
            sta FILENAMEHASHVALLO,x
    .if .xmatch (drivetype, 1541)
            sty FILENAMEHASHVALHI,x
    .else
            lda TEMP
            sta FILENAMEHASHVALHI,x
    .endif

            ; advance to next file or quit loop
            cpx #DIRBUFFSIZE - 1
            bcs dirbuffull

notafile:
    .if .xmatch (drivetype, 1541)
            lax DIRBLOCKPOS
            axs #$20; 8 entries per block, $20 bytes per entry
            stx DIRBLOCKPOS
            bcs getdirloop; process all entries in a dir block
    .elseif .xmatch (drivetype, 1571)
            tya
            and #%11100000; 8 entries per block, $20 bytes per entry
           ;clc
            adc #$20
            sta DIRBLOCKPOS
            bcc getdirloop; process all entries in a dir block
    .else
            tya
            and #%11100000; 8 entries per block, $20 bytes per entry
           ;clc
            adc #$20
            tay
            bcc getdirloop; process all entries in a dir block
    .endif
            ; process next dir block
    .if .xmatch (drivetype, 1541)
            lda NEXTDIRBLOCKSECTOR
            ldy CYCLESTARTENDSECTOR
            beq :+; set CYCLESTARTENDSECTOR
            cmp CYCLESTARTENDSECTOR
            bne nextdirsct
            ; cycle complete
            .byte OPC_LDY_IMM; ldy #$aa, skip tax and clear z
           ;ldx #$fc
:           tax; clears z
            stx CYCLESTARTENDSECTOR
    .elseif .xmatch (drivetype, 1571)
            lda NEXTDIRBLOCKSECTOR
            ldy CYCLESTARTENDSECTOR
            beq dircycle; set CYCLESTARTENDSECTOR
            cmp CYCLESTARTENDSECTOR
            bne nextdirsct

            ; cycle complete
           ;sec
            ror CYCLESTARTENDSECTOR
    .else; 1581
            jsr getdirtrk
            ldy NEXTDIRBLOCKSECTOR
            ldx CYCLESTARTENDTRACK
            bne :+
            sta CYCLESTARTENDTRACK
            sty CYCLESTARTENDSECTOR
            beq nextdirsct; jmp
:           cmp CYCLESTARTENDTRACK
            bne nextdirsct
            cpy CYCLESTARTENDSECTOR
            bne nextdirsct

            ; cycle complete
           ;sec
            ror CYCLESTARTENDSECTOR
    .endif; 1581
    .if (.not .xmatch (drivetype, 1581)) && (DIRBUFFSIZE < 16)
            ; always fill up a dir buffer that holds fewer than 2 dir blocks, because if the dir is 2 blocks
            ; big, the dir buffer may otherwise only hold parts of 2 blocks despite having more space
            bne nextdirsct; jmp
    .endif
dirbuffull: lda LOADEDSECTOR
            sta NEXTDIRBLOCKSECTOR
    .if .not .xmatch (drivetype, 1581)
        .if DIRBUFFSIZE < 16
           ;ldx NUMFILES; actually number of files - 1
        .else
            ldx NUMFILES; actually number of files - 1
        .endif
    .else
            lda CURRDIRBLOCKTRACK
            sta NEXTDIRBLOCKTRACK

            ; the disk was not changed, or the dir has just been read
samedisk:   ldx NUMFILES; actually number of files - 1
    .endif; 1581

findloop:
    .if .not .xmatch (drivetype, 1581)
            lda NEXTDIRBLOCKSECTOR
    .else; !1581
            lda NEXTDIRBLOCKTRACK
            ldy NEXTDIRBLOCKSECTOR
    .endif
            dex ; skip the last file entry to keep it as an overflow entry for load-next and PREPARE_NEXT_FILE
            bmi nextdirseg; if the dir buffer does not contain the file, cycle through the directory to find it

nextfile:   lda FILENAMEHASHLO
            cmp FILENAMEHASHVALLO,x
            bne findloop
            lda FILENAMEHASHHI
            cmp FILENAMEHASHVALHI,x
            bne findloop

            ; file found

    .if .xmatch (drivetype, 1541)
            ldy DIRTRACKS,x
           ;sec
    .elseif (.xmatch (drivetype, 1571))
            ; check for illegal track or sector
           ;sec
            stx FILEINDEX
            lda .lobyte(DIRTRACKS),x
            beq filenotfnd
            cmp MAXTRACK
            bcs filenotfnd
            jsr getnumscts
            stx TEMP
            ldx FILEINDEX
            lda .lobyte(NUMBLOCKS),x
            sta NUMFILEBLOCKS
            ldy .lobyte(DIRTRACKS),x
            lda .lobyte(DIRSECTORS),x
            bmi :+; branch if loop file
            cmp TEMP
            bcs filenotfnd
:           clc
filenotfnd:
    .else; 1581
           ;lda #$00
            sta CYCLESTARTENDTRACK
            sta CYCLESTARTENDSECTOR

            lda DIRTRACKS,x
            ldy DIRSECTORS,x
            clc
filenotfnd:
    .endif; 1581
prpnxtfjmp: jmp prpdnxtfil; is changed to eor prpdnxtfil
    .if .not .xmatch (drivetype, 1541)
            dec prpnxtfjmp; OPC_EOR_ABS -> OPC_JMP_ABS
    .endif
.endmacro; FIND_FILE drivetype

.macro PREPARE_NEXT_FILE drivetype
    .if .not .xmatch (drivetype, 1541)
prepnxtfil: jmp findfile; jumps to prpdnxtfil
    .endif; !1541
prpdnxtfil: ; always succeeds, trackseek to track in y is valid
    .if .xmatch (drivetype, 1541)
trseekidle: jsr trackseek
    .elseif .xmatch (drivetype, 1571) ; not enough space for loop file handling in 1541 RAM
            lda DIRSECTORS - 1,x
            bpl trseekidle
            dex; file index in [1..DIRBUFFSIZE] -> [0..DIRBUFFSIZE - 1]
            txa
            clc
            adc DIRSECTORS,x; negative offset to referenced file entry
            tax
            bpl :+; branch if not wrapping in dir buffer
            axs #.lobyte(-DIRBUFFSIZE)
:           tya; file track
            cmp DIRTRACKS,x
            bne :+; do not seek to referenced file, as it is not in the dir buffer
trseekidle: jsr trackseek
:
    .else
trseekidle: jsr trackseek
    .endif
.endmacro; PREPARE_NEXT_FILE drivetype

.macro FNAMEHASH drivetype

fnamehash:
    .if .xmatch (drivetype, 1541)
            lda (BLP),y; number of file blocks (low-byte)
            jsr sertoraw
            sta NUMBLOCKS,x
            ldx #0
:           jsr sertorawd
    .else; 1571 | 1581
            ; must not clobber y
            ldx #0
:           lda BLOCKBUFFER + 5,y
            iny
    .endif; 1571 | 1581
            cmp #' ' | $80; $a0 = end of filename
            beq gethashval
    .if .not .xmatch (drivetype, 1581)
            sta .lobyte(FILENAME),x
    .else; 1581
            sta filename,x
    .endif; 1581
            inx
            cpx #FILENAME_MAXLENGTH
            bcc :-

            ; fall through

gethashval: txa
    .if .xmatch (drivetype, 1541)
            tay
    .else
            sta TEMP
    .endif
           ;sec
hashloop:
    .if .not .xmatch (drivetype, 1581)
            adc .lobyte(FILENAME - 1),x
    .else; 1581
            adc filename - 1,x
    .endif; 1581
    .if .xmatch (drivetype, 1541)
            pha
            tya
            adc stack + 1; = the value just pushed
            tay
            pla
    .else
            pha
            adc TEMP
            sta TEMP
            pla
    .endif
            dex
            bne hashloop
            rts

.endmacro; FNAMEHASH drivetype

.endif; !_DRIVECODE_COMMON_INC_
