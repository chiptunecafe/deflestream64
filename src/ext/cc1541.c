/*******************************************************************************
* Copyright (c) 2008-2019 JackAsser, Krill, Claus, Björn Esser
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*******************************************************************************/

#define VERSION "3.1"

#define _CRT_SECURE_NO_WARNINGS /* avoid security warnings for MSVC */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#define min(a, b) (((a) < (b)) ? (a) : (b))

#ifdef _WIN32
#include <windows.h>
#define FILESEPARATOR '\\'
#else
#define FILESEPARATOR '/'
#endif

#define DIRENTRIESPERBLOCK     8
#define DIRTRACK_D41_D71       18
#define DIRTRACK_D81           40
#define SECTORSPERTRACK_D81    40
#define MAXNUMFILES_D81        ((SECTORSPERTRACK_D81 - 3) * DIRENTRIESPERBLOCK)
#define DIRENTRYSIZE           32
#define BLOCKSIZE              256
#define BLOCKOVERHEAD          2
#define TRACKLINKOFFSET        0
#define SECTORLINKOFFSET       1
#define FILETYPEOFFSET         2
#define FILETYPEDEL            0
#define FILETYPESEQ            1
#define FILETYPEPRG            2
#define FILETYPEUSR            3
#define FILETYPEREL            4
#define FILETRACKOFFSET        3
#define FILESECTOROFFSET       4
#define FILENAMEOFFSET         5
#define FILENAMEMAXSIZE        16
#define FILENAMEEMPTYCHAR      (' ' | 0x80)
#define FILEBLOCKSLOOFFSET     30
#define FILEBLOCKSHIOFFSET     31
#define D64NUMBLOCKS           (664 + 19)
#define D64SIZE                (D64NUMBLOCKS * BLOCKSIZE)
#define D64SIZE_EXTENDED       (D64SIZE + 5 * 17 * BLOCKSIZE)
#define D71SIZE                (D64SIZE * 2)
#define D81SIZE                (D81NUMTRACKS * SECTORSPERTRACK_D81 * BLOCKSIZE)
#define D64NUMTRACKS           35
#define D64NUMTRACKS_EXTENDED  (D64NUMTRACKS + 5)
#define D71NUMTRACKS           (D64NUMTRACKS * 2)
#define D81NUMTRACKS           80
#define BAM_OFFSET_SPEED_DOS   0xac
#define BAM_OFFSET_DOLPHIN_DOS 0xc0
#define UNALLOCATED            0
#define ALLOCATED              1
#define FILESTART              2
#define DIRSLOTEXISTS          0
#define DIRSLOTFOUND           1
#define DIRSLOTNOTFOUND        2

typedef struct {
    const unsigned char* alocalname;                  /* local file name or name of loop file in ASCII */
    unsigned char        plocalname[FILENAMEMAXSIZE]; /* loop file in PETSCII */
    const unsigned char* afilename;                   /* disk file name in ASCII */
    unsigned char        pfilename[FILENAMEMAXSIZE];  /* disk file name in PETSCII */
    int                  direntryindex;
    int                  direntrysector;
    int                  direntryoffset;
    int                  sectorInterleave;
    int                  first_sector_new_track;
    int                  track;
    int                  sector;
    int                  nrSectors;
    int                  nrSectorsShown;
    int                  filetype;
    int                  mode;
} imagefile;

enum mode {
    MODE_BEGINNING_SECTOR_MASK   = 0x003f, /* 6 bits */
    MODE_MIN_TRACK_MASK          = 0x0fc0, /* 6 bits */
    MODE_MIN_TRACK_SHIFT         = 6,
    MODE_SAVETOEMPTYTRACKS       = 0x1000,
    MODE_FITONSINGLETRACK        = 0x2000,
    MODE_SAVECLUSTEROPTIMIZED    = 0x4000,
    MODE_LOOPFILE                = 0x8000
};

typedef enum {
    IMAGE_D64,
    IMAGE_D64_EXTENDED_SPEED_DOS,
    IMAGE_D64_EXTENDED_DOLPHIN_DOS,
    IMAGE_D71,
    IMAGE_D81
} image_type;

static const char *filetypename[] = {
    "del", "seq", "prg", "usr", "rel", "cbm", "???", "???",
    "???", "???", "???", "???", "???", "???", "???", "???"
};

static const int
sectors_per_track[] = {
    /*  1-17 */ 21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
    /* 18-24 */ 19,19,19,19,19,19,19,
    /* 25-30 */ 18,18,18,18,18,18,
    /* 31-35 */ 17,17,17,17,17,
    /* 36-52 */ 21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
    /* 53-59 */ 19,19,19,19,19,19,19,
    /* 60-65 */ 18,18,18,18,18,18,
    /* 66-70 */ 17,17,17,17,17
};

static const int
sectors_per_track_extended[] = {
    /*  1-17 */ 21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
    /* 18-24 */ 19,19,19,19,19,19,19,
    /* 25-30 */ 18,18,18,18,18,18,
    /* 31-35 */ 17,17,17,17,17,
    /* 36-40 */ 17,17,17,17,17,
    /* 41-57 */ 21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
    /* 58-64 */ 19,19,19,19,19,19,19,
    /* 65-70 */ 18,18,18,18,18,18,
    /* 71-75 */ 17,17,17,17,17,
    /* 76-80 */ 17,17,17,17,17
};

static int quiet           = 0;  /* global quiet flag */
static int verbose         = 0;  /* global verbose flag */
static int num_files       = 0;  /* number of files to be written provided by the user */
static int max_hash_length = 16; /* number of bytes of the filenames to calculate the hash over */

/* Prints the commandline help */
static void
usage()
{
    printf("\n*** This is cc1541 version " VERSION " built on " __DATE__ " ***\n\n");
    printf("Usage: cc1541 -niwfoVTPOlBMmdtuxFsSeErbc45gqvh image.[d64|d71|d81]\n\n");
    printf("-n diskname   Disk name, default='cc1541'.\n");
    printf("-i id         Disk ID, default='00 2a'.\n");
    printf("-w localname  Write local file to disk, if filename is not set then the\n");
    printf("              local name is used. After file written, the filename is unset.\n");
    printf("-f filename   Use filename as name when writing next file, use prefix # to\n");
    printf("              include arbitrary PETSCII characters (e.g. -f \"START#a0,8,1\").\n");
    printf("-o            Do not overwrite if file with same name exists already.\n");
    printf("-V            Do not modify image unless it is in valid CBM DOS format.\n");
    printf("-T filetype   Filetype for next file, allowed parameters are PRG, SEQ, USR, REL\n");
    printf("              and DEL. For DEL, the input file is ignored. Default is PRG.\n");
    printf("-P            Set write protect flag for next file.\n");
    printf("-O            Set open flag for next file.\n");
    printf("-l filename   Write loop file (an additional dir entry) to existing file to\n");
    printf("              disk, set filename with -f.\n");
    printf("-B numblocks  Write the given value as file size in blocks to the directory for\n");
    printf("              the next file.\n");
    printf("-M numchars   Hash computation maximum filename length, this must\n");
    printf("              match loader option FILENAME_MAXLENGTH in Krill's Loader.\n");
    printf("              Default is 16.\n");
    printf("-m            Ignore filename hash collisions, without this switch a collision\n");
    printf("              results in an error.\n");
    printf("-d track      Maintain a shadow directory (copy of the actual directory without\n");
    printf("              a valid BAM).\n");
    printf("-t            Use dirtrack to also store files (makes -x useless) (default no).\n");
    printf("-u numblocks  When using -t, amount of dir blocks to leave free (default=2).\n");
    printf("-x            Don't split files over dirtrack hole (default split files).\n");
    printf("-F            Next file first sector on a new track (default=0).\n");
    printf("              Any negative value assumes aligned tracks and uses current\n");
    printf("              sector + interleave. After each file, the value falls back to the\n");
    printf("              default. Not applicable for D81.\n");
    printf("-S value      Default sector interleave, default=10.\n");
    printf("              At track end, reduces this by 1 to accomodate large tail gap.\n");
    printf("              If negative, no special treatment of tail gap.  Not applicable for\n");
    printf("              D81.\n");
    printf("-s value      Next file sector interleave, valid after each file.\n");
    printf("              At track end, reduces this by 1 to accomodate large tail gap.\n");
    printf("              If negative, no special treatment of tail gap.\n");
    printf("              The interleave value falls back to the default value set by -S\n");
    printf("              after the first sector of the next file. Not applicable for D81.\n");
    printf("-e            Start next file on an empty track (default start sector is\n");
    printf("              current sector plus interleave).\n");
    printf("-E            Try to fit file on a single track.\n");
    printf("-r track      Restrict next file blocks to the specified track or higher.\n");
    printf("-b sector     Set next file beginning sector to the specified value.\n");
    printf("              Not applicable for D81.\n");
    printf("-c            Save next file cluster-optimized (d71 only).\n");
    printf("-4            Use tracks 35-40 with SPEED DOS BAM formatting.\n");
    printf("-5            Use tracks 35-40 with DOLPHIN DOS BAM formatting.\n");
    printf("-g filename   Write additional g64 output file with given name.\n");
    printf("-q            Be quiet.\n");
    printf("-v            Be verbose.\n");
    printf("-h            Print this commandline help.\n");
    printf("\n");

    exit(-1);
}

/* Returns a pointer to the filename in a path */
static const unsigned char*
basename(const unsigned char* path)
{
    const unsigned char* name;
    (name = (unsigned char*)strrchr((char *)path, FILESEPARATOR)) ? ++name : (name = path);
    return name;
}

/* Calculates a hash from a filename to be used by Krill's Loader */
static unsigned int
filenamehash(const unsigned char *filename)
{
    int pos = min(max_hash_length, (int)strlen((char *)filename));
    while ((pos > 0) && (((unsigned char) filename[pos - 1]) == FILENAMEEMPTYCHAR)) {
        --pos;
    }

    unsigned char hashlo = pos;
    unsigned char hashhi = pos;
    int carry = 1;

    for (int i = pos - 1; i >= 0; --i) {
        unsigned int sum = hashlo + filename[i] + carry;
        carry = (sum >= 256) ? 1 : 0;
        sum &= 0xff;
        hashlo = sum;
        sum += hashhi + carry;
        carry = (sum >= 256) ? 1 : 0;
        hashhi = sum;
    }

    return (hashhi << 8) | hashlo;
}

/* Returns the size of an image in bytes */
static unsigned int
image_size(image_type type)
{
    switch (type) {
    case IMAGE_D64:
        return D64SIZE;

    case IMAGE_D64_EXTENDED_SPEED_DOS:
    /* fall through */
    case IMAGE_D64_EXTENDED_DOLPHIN_DOS:
        return D64SIZE_EXTENDED;

    case IMAGE_D71:
        return D71SIZE;

    case IMAGE_D81:
        return D81SIZE;

    default:
        return 0;
    }
}

/* Returns the number of tracks in an image */
static unsigned int
image_num_tracks(image_type type)
{
    switch (type) {
    case IMAGE_D64:
        return D64NUMTRACKS;

    case IMAGE_D64_EXTENDED_SPEED_DOS:
    /* fall through */
    case IMAGE_D64_EXTENDED_DOLPHIN_DOS:
        return D64NUMTRACKS_EXTENDED;

    case IMAGE_D71:
        return D71NUMTRACKS;

    case IMAGE_D81:
        return D81NUMTRACKS;

    default:
        return 0;
    }
}

/* Returns the directory track of an image */
static int
dirtrack(image_type type)
{
    return (type == IMAGE_D81) ? DIRTRACK_D81 : DIRTRACK_D41_D71;
}

/* Returns the number of sectors for a given track */
static int
num_sectors(image_type type, int track)
{
    return (type == IMAGE_D81) ? SECTORSPERTRACK_D81
           : (((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) ? sectors_per_track_extended[track - 1]
              : sectors_per_track[track - 1]);
}

/* Converts an ASCII character to PETSCII */
static unsigned char
a2p(unsigned char a)
{
    switch (a) {
    case '\n':
        return 0x0d;
    case 0x7e:
        return 0xff;
    default:
        if ((a >= 0x5b) && (a <= 0x5f)) {
            return a;
        }
        if ((a >= 0x60) && (a <= 0x7e)) {
            return a ^ 0x20;
        }
        if ((a >= 'A') && (a <= 'Z')) {
            return a | 0x80;
        }
        return a;
    }
}

/* Converts a PETSCII character to ASCII */
static unsigned char
p2a(unsigned char p)
{
    switch (p) {
    case 0x0a:
    case 0x0d:
        return '\n';
    case 0x40:
    case 0x60:
        return p;
    case 0xa0:
    case 0xe0:
        return ' ';
    default:
        switch (p & 0xe0) {
        case 0x40:
        case 0x60:
            return (p ^ 0x20);
        case 0xc0:
            return (p ^ 0x80);
        }
    }
    return ((isprint(p) ? p : '.'));
}

/* Converts an ASCII string to PETSCII filled up with Shift-Space to length */
static void
ascii2petscii(const unsigned char* ascii, unsigned char* petscii, int len)
{
    int pos = 0;
    while (ascii[pos] != '\0' && pos < len) {
        petscii[pos] = a2p(ascii[pos]);
        ++pos;
    }
    while (pos < len) {
        petscii[pos] = FILENAMEEMPTYCHAR;
        ++pos;
    }
}

/* Converts an PETSCII string to ASCII with length restriction */
static int
petscii2ascii(const unsigned char* petscii, unsigned char* ascii, int len)
{
    int pos = 0;
    int last = pos;
    while (pos < len) {
        ascii[pos] = p2a(petscii[pos]);
        if (petscii[pos] != FILENAMEEMPTYCHAR) {
            last = pos;
        }
        ++pos;
    }
    ascii[last+1] = '\0';
    return pos;
}

/* Converts a two digit hex string to an int */
static unsigned int
hex2int(char hex)
{
    if ((hex < '0' || hex > '9') && (hex < 'a' || hex > 'f')) {
        fprintf(stderr, "ERROR: Invalid hex string in filename\n");

        exit(-1);
    }
    if (hex <= '9') {
        hex -= '0';
    } else {
        hex -= 'a' - 10;
    }
    return (unsigned int)hex;
}

/* Converts an ASCII string to PETSCII with escape evaluation filled up with Shift-Space to length */
static void
evalhexescape(const unsigned char* ascii, unsigned char* petscii, int len)
{
    int read = 0, write = 0;

    while (ascii[read] != '\0' && write < len) {
        if (ascii[read] == '#') {
            unsigned int hi = hex2int(ascii[++read]);
            unsigned int lo = hex2int(ascii[++read]);
            petscii[write] = (unsigned char)(16 * hi + lo);
        } else {
            petscii[write] = a2p(ascii[read]);
        }
        read++;
        write++;
    }
    while (write < len) {
        petscii[write] = FILENAMEEMPTYCHAR;
        ++write;
    }
}

/* Calculates the overall sector index from a given track and sector */
static int
linear_sector(image_type type, int track, int sector)
{
    if ((track < 1) || (track > ((type == IMAGE_D81) ? D81NUMTRACKS : ((type == IMAGE_D64) ? D64NUMTRACKS : (type == IMAGE_D71 ? D71NUMTRACKS : D64NUMTRACKS_EXTENDED))))) {
        fprintf(stderr, "ERROR: Illegal track %d\n", track);

        exit(-1);
    }

    int numsectors = num_sectors(type, track);
    if ((sector < 0) || (sector >= numsectors)) {
        fprintf(stderr, "ERROR: Illegal sector %d for track %d (max. is %d)\n", sector, track, numsectors - 1);

        exit(-1);
    }

    int linear_sector = 0;
    for (int i = 0; i < track - 1; i++) {
        linear_sector += num_sectors(type, i + 1);
    }
    linear_sector += sector;

    return linear_sector;
}

/* Finds all filenames with the given hash */
static int
count_hashes(image_type type, unsigned char* image, unsigned int hash, bool print)
{
    int num = 0;

    int dirsector = (type == IMAGE_D81) ? 3 : 1;
    do {
        int dirblock = linear_sector(type, dirtrack(type), dirsector) * BLOCKSIZE;

        for (int i = 0; i < DIRENTRIESPERBLOCK; ++i) {
            int entryOffset = i * DIRENTRYSIZE;
            int filetype = image[dirblock + entryOffset + FILETYPEOFFSET] & 0xf;
            if (filetype != FILETYPEDEL) {
                unsigned char *filename = (unsigned char *) image + dirblock + entryOffset + FILENAMEOFFSET;
                if (hash == filenamehash(filename)) {
                    ++num;

                    if (print) {
                        unsigned char afilename[FILENAMEMAXSIZE + 1];
                        petscii2ascii(filename, afilename, FILENAMEMAXSIZE);
                        printf(" [$%04x] \"%s\"\n", filenamehash(filename), afilename);
                    }
                }
            }
        }

        if (image[dirblock + TRACKLINKOFFSET] == dirtrack(type)) {
            dirsector = image[dirblock + SECTORLINKOFFSET];
        } else {
            dirsector = 0;
        }
    } while (dirsector > 0);

    return num;
}

/* Checks if multiple filenames have the same hash */
static bool
check_hashes(image_type type, unsigned char* image)
{
    bool collision = false;

    printf("\n");
    int dirsector = 1;
    do {
        int dirblock = linear_sector(type, dirtrack(type), dirsector) * BLOCKSIZE;

        for (int i = 0; i < DIRENTRIESPERBLOCK; ++i) {
            int entryOffset = i * DIRENTRYSIZE;
            int filetype = image[dirblock + entryOffset + FILETYPEOFFSET];
            if (filetype != FILETYPEDEL) {
                unsigned char *filename = (unsigned char *) image + dirblock + entryOffset + FILENAMEOFFSET;
                if (count_hashes(type, image, filenamehash(filename), false /* print */) > 1) {
                    collision = 1;
                    unsigned char afilename[FILENAMEMAXSIZE + 1];
                    petscii2ascii(filename, afilename, FILENAMEMAXSIZE);
                    fprintf(stderr, "Hash of filename \"%s\" [$%04x] is not unique\n", afilename, filenamehash(filename));
                    count_hashes(type, image, filenamehash(filename), true /* print */);
                }
            }
        }

        if (image[dirblock + TRACKLINKOFFSET] == dirtrack(type)) {
            dirsector = image[dirblock + SECTORLINKOFFSET];
        } else {
            dirsector = 0;
        }
    } while (dirsector > 0);

    return collision;
}

/* Returns the image offset of the bam entry for a given track */
static int
get_bam_offset(image_type type, unsigned int track)
{
    int bam;
    unsigned int offset;

    if (type == IMAGE_D81) {
        if (track <= 40) {
            bam = linear_sector(type, dirtrack(type), 1 /* sector */) * BLOCKSIZE;
            offset = bam + (track * 6) + 11;
        } else {
            bam = linear_sector(type, dirtrack(type), 2 /* sector */) * BLOCKSIZE;
            offset = bam + ((track - 40) * 6) + 11;
        }
    } else if ((type == IMAGE_D71) && (track > D64NUMTRACKS)) {
        /* access second side bam */
        bam = linear_sector(type, dirtrack(type) + D64NUMTRACKS, 0) * BLOCKSIZE;
        offset = bam + (track - D64NUMTRACKS - 1) * 3;
    } else {
        if (((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) && (track > D64NUMTRACKS)) {
            track -= D64NUMTRACKS;
            bam = linear_sector(type, dirtrack(type), 0) * BLOCKSIZE + ((type == IMAGE_D64_EXTENDED_SPEED_DOS) ? BAM_OFFSET_SPEED_DOS : BAM_OFFSET_DOLPHIN_DOS);
        } else {
            bam = linear_sector(type, dirtrack(type), 0) * BLOCKSIZE;
        }
        offset = bam + track * 4 + 1;
    }
    return offset;
}

/* Checks if a given sector is marked as free in the BAM and also not used by directory */
static int
is_sector_free(image_type type, unsigned char* image, int track, int sector, int numdirblocks, int dir_sector_interleave)
{
    int bam;
    unsigned char* bitmap;

    if (sector < 0) {
        fprintf(stderr, "ERROR: Illegal sector %d for track %d\n", sector, track);

        exit(-1);
    }

    if (type == IMAGE_D81) {
        if (track <= 40) {
            bam = linear_sector(type, dirtrack(type), 1 /* sector */) * BLOCKSIZE;
            bitmap = image + bam + (track * 6) + 11;
        } else {
            bam = linear_sector(type, dirtrack(type), 2 /* sector */) * BLOCKSIZE;
            bitmap = image + bam + ((track - 40) * 6) + 11;
        }
    } else if ((type == IMAGE_D71) && (track > D64NUMTRACKS)) {
        /* access second side bam */
        bam = linear_sector(type, dirtrack(type) + D64NUMTRACKS, 0) * BLOCKSIZE;
        bitmap = image + bam + (track - D64NUMTRACKS - 1) * 3;
    } else {
        if (((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) && (track > D64NUMTRACKS)) {
            track -= D64NUMTRACKS;
            bam = linear_sector(type, dirtrack(type), 0) * BLOCKSIZE + ((type == IMAGE_D64_EXTENDED_SPEED_DOS) ? BAM_OFFSET_SPEED_DOS : BAM_OFFSET_DOLPHIN_DOS);
        } else {
            bam = linear_sector(type, dirtrack(type), 0) * BLOCKSIZE;
        }
        bitmap = image + bam + (track * 4) + 1;
    }

    int byte = sector >> 3;
    int bit = sector & 7;

    int is_not_dir_block = 1;
    if ((track == dirtrack(type)) && (numdirblocks > 0)) {
        int dirsector = 0;
        int s = 2;
        for (int i = 0; is_not_dir_block && (i < numdirblocks); i++) {
            switch (i) {
            case 0:
                dirsector = 0;
                break;

            case 1:
                dirsector = 1;
                break;

            default:
                dirsector += dir_sector_interleave;
                if (dirsector >= num_sectors(type, track)) {
                    dirsector = s;
                    s++;
                }
                break;
            }
            is_not_dir_block = (sector != dirsector);
        }
    }

    return is_not_dir_block && ((bitmap[byte] & (1 << bit)) != 0);
}

/* Marks given sector as allocated in BAM */
static void
mark_sector(image_type type, unsigned char* image, int track, int sector, int free)
{
    if (free != is_sector_free(type, image, track, sector, 0, 0)) {
        int bam;
        unsigned char* bitmap;
        if (type == IMAGE_D81) {
            if (track <= 40) {
                bam = linear_sector(type, dirtrack(type), 1 /* sector */) * BLOCKSIZE;
                bitmap = image + bam + (track * 6) + 11;
            } else {
                bam = linear_sector(type, dirtrack(type), 2 /* sector */) * BLOCKSIZE;
                bitmap = image + bam + ((track - 40) * 6) + 11;
            }

            /* update number of free sectors on track */
            if (free) {
                ++bitmap[-1];
            } else {
                --bitmap[-1];
            }
        } else if ((type == IMAGE_D71) && (track > D64NUMTRACKS)) {
            /* access second side bam */
            bam = linear_sector(type, dirtrack(type) + D64NUMTRACKS, 0) * BLOCKSIZE;
            bitmap = image + bam + (track - D64NUMTRACKS - 1) * 3;

            /* update number of free sectors on track */
            if (free) {
                image[bam + 0xdd + track - D64NUMTRACKS - 1]++;
            } else {
                image[bam + 0xdd + track - D64NUMTRACKS - 1]--;
            }
        } else {
            if (((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) && (track > D64NUMTRACKS)) {
                track -= D64NUMTRACKS;
                bam = linear_sector(type, dirtrack(type), 0) * BLOCKSIZE + ((type == IMAGE_D64_EXTENDED_SPEED_DOS) ? BAM_OFFSET_SPEED_DOS : BAM_OFFSET_DOLPHIN_DOS);
            } else {
                bam = linear_sector(type, dirtrack(type), 0) * BLOCKSIZE;
            }
            bitmap = image + bam + (track * 4) + 1;

            /* update number of free sectors on track */
            if (free) {
                ++image[bam + (track * 4)];
            } else {
                --image[bam + (track * 4)];
            }
        }

        /* update bitmap */
        int byte = sector >> 3;
        int bit = sector & 7;

        if (free) {
            bitmap[byte] |= 1 << bit;
        } else {
            bitmap[byte] &= ~(1 << bit);
        }
    }
}

/* Returns offset for header on directory track */
static int
get_header_offset(image_type type)
{
    int offset;

    if (type == IMAGE_D81) {
        offset = 4;
    } else {
        offset = 0x90;
    }
    return offset;
}

/* Returns offset for id on directory track */
static int
get_id_offset(image_type type)
{
    int offset;

    if (type == IMAGE_D81) {
        offset = 0x16;
    } else {
        offset = 0xa2;
    }
    return offset;
}

/* Updates the directory with the given header and id */
static void
update_directory(image_type type, unsigned char* image, unsigned char* header, unsigned char* id, int shadowdirtrack)
{
    unsigned int bam = linear_sector(type, dirtrack(type), 0) * BLOCKSIZE;

    if (type != IMAGE_D81) {
        image[bam + 0x03] = (type == IMAGE_D71) ? 0x80 : 0x00;
    }

    /* Set header and ID */
    unsigned char pheader[FILENAMEMAXSIZE];
    unsigned char pid[5];
    evalhexescape(header, pheader, FILENAMEMAXSIZE);
    evalhexescape(id, pid, 5);
    memcpy(image + bam + get_header_offset(type), pheader, FILENAMEMAXSIZE);
    memcpy(image + bam + get_id_offset(type), pid, 5);

    if (type == IMAGE_D81) {
        unsigned int bam = linear_sector(type, dirtrack(type), 1 /* sector */) * BLOCKSIZE;
        image[bam + 0x04] = id[0];
        image[bam + 0x05] = id[1];

        bam = linear_sector(type, dirtrack(type), 2 /* sector */) * BLOCKSIZE;
        image[bam + 0x04] = id[0];
        image[bam + 0x05] = id[1];
    }

    if (shadowdirtrack > 0) {
        unsigned int shadowbam = linear_sector(type, shadowdirtrack, 0 /* sector */) * BLOCKSIZE;
        memcpy(image + shadowbam, image + bam, BLOCKSIZE);

        image[shadowbam + 0x00] = shadowdirtrack;
    }
}

/* Writes an empty directory and BAM */
static void
initialize_directory(image_type type, unsigned char* image, unsigned char* header, unsigned char* id, int shadowdirtrack)
{
    unsigned int dir = linear_sector(type, dirtrack(type), 0 /* sector */) * BLOCKSIZE;

    /* Clear image */
    memset(image, 0, image_size(type));

    /* Write initial BAM */
    if (type == IMAGE_D81) {
        image[dir + 0x00] = dirtrack(type);
        image[dir + 0x01] = 3;
        image[dir + 0x02] = 0x44;

        image[dir + 0x14] = FILENAMEEMPTYCHAR;
        image[dir + 0x15] = FILENAMEEMPTYCHAR;

        image[dir + 0x1b] = FILENAMEEMPTYCHAR;
        image[dir + 0x1c] = FILENAMEEMPTYCHAR;

        unsigned int bam = linear_sector(type, dirtrack(type), 1 /* sector */) * BLOCKSIZE;
        image[bam + 0x00] = dirtrack(type);
        image[bam + 0x01] = 2;
        image[bam + 0x02] = 0x44;
        image[bam + 0x03] = 0xbb;
        image[bam + 0x06] = 0xc0;

        bam = linear_sector(type, dirtrack(type), 2 /* sector */) * BLOCKSIZE;
        image[bam + 0x00] = 0;
        image[bam + 0x01] = 255;
        image[bam + 0x02] = 0x44;
        image[bam + 0x03] = 0xbb;
        image[bam + 0x06] = 0xc0;
    } else {
        image[dir + 0x00] = dirtrack(type);
        image[dir + 0x01] = 1;
        image[dir + 0x02] = 0x41;
        image[dir + 0x03] = (type == IMAGE_D71) ? 0x80 : 0x00;

        image[dir + 0xa0] = FILENAMEEMPTYCHAR;
        image[dir + 0xa1] = FILENAMEEMPTYCHAR;

        image[dir + 0xa7] = FILENAMEEMPTYCHAR;
        image[dir + 0xa8] = FILENAMEEMPTYCHAR;
        image[dir + 0xa9] = FILENAMEEMPTYCHAR;
        image[dir + 0xaa] = FILENAMEEMPTYCHAR;
    }

    /* Mark all sectors unused */
    for (unsigned int t = 1; t <= image_num_tracks(type); t++) {
        for (int s = 0; s < num_sectors(type, t); s++) {
            mark_sector(type, image, t, s, 1 /* free */);
        }
    }

    /* Reserve space for BAM */
    mark_sector(type, image, dirtrack(type), 0 /* sector */, 0 /* not free */);
    if (type == IMAGE_D71) {
        mark_sector(type, image, dirtrack(type) + D64NUMTRACKS, 0 /* sector */, 0 /* not free */);
    } else if (type == IMAGE_D81) {
        mark_sector(type, image, dirtrack(type), 1 /* sector */, 0 /* not free */);
        mark_sector(type, image, dirtrack(type), 2 /* sector */, 0 /* not free */);
    }

    /* first dir block */
    unsigned int dirblock = linear_sector(type, dirtrack(type), (type == IMAGE_D81) ? 3 : 1) * BLOCKSIZE;
    image[dirblock + SECTORLINKOFFSET] = 255;
    mark_sector(type, image, dirtrack(type), (type == IMAGE_D81) ? 3 : 1 /* sector */, 0 /* not free */);

    if (shadowdirtrack > 0) {
        dirblock = linear_sector(type, shadowdirtrack, (type == IMAGE_D81) ? 3 : 1 /* sector */) * BLOCKSIZE;
        image[dirblock + SECTORLINKOFFSET] = 255;

        mark_sector(type, image, shadowdirtrack, 0 /* sector */, 0 /* not free */);
        mark_sector(type, image, shadowdirtrack, (type == IMAGE_D81) ? 3 : 1 /* sector */, 0 /* not free */);
    }

    update_directory(type, image, header, id, shadowdirtrack);
}

/* Deletes a file from directory and BAM */
static void
wipe_file(image_type type, unsigned char* image, unsigned int track, unsigned int sector)
{
    if (sector >= 0x80) {
        return; /* loop file */
    }

    while (track != 0) {
        int block_offset = linear_sector(type, track, sector) * BLOCKSIZE;
        int next_track = image[block_offset + TRACKLINKOFFSET];
        int next_sector = image[block_offset + SECTORLINKOFFSET];
        memset(image + block_offset, 0, BLOCKSIZE);
        mark_sector(type, image, track, sector, 1 /* free */);
        track = next_track;
        sector = next_sector;
    }
}

/* Sets image offset to the next DIR entry, returns false when the DIR end was reached */
static bool
next_dir_entry(image_type type, unsigned char* image, int *track, int *sector, int *offset)
{
    if (*offset % BLOCKSIZE == 7 * DIRENTRYSIZE) {
        /* last entry in sector */

        *track = image[linear_sector(type, *track, *sector) * BLOCKSIZE + TRACKLINKOFFSET];
        if (*track == 0) {
            /* this was the last DIR sector */
            return false;
        } else {
            /* follow the t/s link */
            *sector = image[linear_sector(type, *track, *sector) * BLOCKSIZE + SECTORLINKOFFSET];
            *offset = 0;
        }
    } else {
        *offset += DIRENTRYSIZE;
    }
    return 1;
}

/* Returns DIRSLOTEXISTS, directory index and offset if file with given filename exists,
   DIRSLOTFOUND, directory index and offset if an empty slot was found
   or DIRSLOTNOTFOUND, index and offset of last entry otherwise */
static int
find_file(image_type type, unsigned char* image, unsigned char* filename, int *index, int *track, int *sector, int *offset)
{
    bool found = false;
    int t = dirtrack(type);
    int s = (type == IMAGE_D81) ? 3 : 1;
    int o = 0;
    *index = 0;

    do {
        int b = linear_sector(type, t, s) * BLOCKSIZE + o;
        int filetype = image[b + FILETYPEOFFSET] & 0xf;
        switch (filetype) {
        case FILETYPESEQ:
        case FILETYPEPRG:
        case FILETYPEUSR:
        case FILETYPEREL:
            if (memcmp(image + b + FILENAMEOFFSET, filename, FILENAMEMAXSIZE) == 0) {
                *track = t;
                *sector = s;
                *offset = o;
                return DIRSLOTEXISTS;
            }
            break;
        case FILETYPEDEL:
            if (image[b + FILETYPEOFFSET] == 0 && !found) {
                found = true;
                *track = t;
                *sector = s;
                *offset = o;
            } else if (memcmp(image + b + FILENAMEOFFSET, filename, FILENAMEMAXSIZE) == 0) {
                *track = t;
                *sector = s;
                *offset = o;
                return DIRSLOTEXISTS;
            }
            break;
        default:
            break;
        }
        ++(*index);
    } while (next_dir_entry(type, image, &t, &s, &o));
    if (!found) {
        /* no free slot? then return last one */
        *track = t;
        *sector = s;
        *offset = o;
        return DIRSLOTNOTFOUND;
    }
    return DIRSLOTFOUND;
}

/* Returns suitable index and offset for given filename (either existing slot when overwriting, first free slot or slot in newly allocated segment) */
static bool
find_dir_slot(image_type type, unsigned char* image, unsigned char* filename, int dir_sector_interleave, int shadowdirtrack, int *index, int *dirsector,  int *entry_offset)
{
    int track;
    int ret = find_file(type, image, filename, index, &track, dirsector, entry_offset);

    if (ret == DIRSLOTEXISTS) {
        return true;
    }
    if (ret == DIRSLOTFOUND) {
        return false;
    }
    /* allocate new dir block */
    int last_sector = *dirsector;
    int next_sector = -1;
    for (int s = 1; s < num_sectors(type, dirtrack(type)); s++) {
        int sector = (last_sector + s * dir_sector_interleave) % num_sectors(type, dirtrack(type));
        if (is_sector_free(type, image, dirtrack(type), sector, 0, 0)) {
            next_sector = sector;
            break;
        }
    }
    if (next_sector == -1) {
        fprintf(stderr, "ERROR: Dir track full\n");
        exit(-1);
    }
    int b = linear_sector(type, dirtrack(type), last_sector) * BLOCKSIZE;
    image[b + TRACKLINKOFFSET] = dirtrack(type);
    image[b + SECTORLINKOFFSET] = next_sector;
    mark_sector(type, image, dirtrack(type), next_sector, 0 /* not free */);

    b = linear_sector(type, dirtrack(type), next_sector) * BLOCKSIZE;
    memset(image + b, 0, BLOCKSIZE);
    image[b + SECTORLINKOFFSET] = 255;
    *dirsector = next_sector;
    *entry_offset = 0;

    if (shadowdirtrack > 0) {
        b = linear_sector(type, shadowdirtrack, last_sector) * BLOCKSIZE;
        image[b + TRACKLINKOFFSET] = shadowdirtrack;
        image[b + SECTORLINKOFFSET] = next_sector;
        mark_sector(type, image, shadowdirtrack, next_sector, 0 /* not free */);

        b = linear_sector(type, shadowdirtrack, next_sector) * BLOCKSIZE;
        memset(image + b, 0, BLOCKSIZE);
        image[b + SECTORLINKOFFSET] = 255;
    }
    return false;
}

/* Adds the specified new entries to the directory */
static void
create_dir_entries(image_type type, unsigned char* image, imagefile* files, int num_files, int dir_sector_interleave, unsigned int shadowdirtrack, int nooverwrite)
{
    /* this does not check for uniqueness of filenames */

    int num_overwritten_files = 0;

    if (verbose && num_files > 0) {
        printf("\nCreating dir entries:\n");
    }

    for (int i = 0; i < num_files; i++) {
        /* find or create slot */
        imagefile *file = files + i;

        if (verbose) {
            printf("  \"%s\"\n", file->afilename);
        }

        if (find_dir_slot(type, image, file->pfilename, dir_sector_interleave, shadowdirtrack, &file->direntryindex, &file->direntrysector, &file->direntryoffset)) {
            if (nooverwrite) {
                fprintf(stderr, "ERROR: Filename exists on disk image already and -o was set\n");
                exit(-1);
            }
            int b = linear_sector(type, dirtrack(type), file->direntrysector) * BLOCKSIZE + file->direntryoffset;
            wipe_file(type, image, image[b + FILETRACKOFFSET], image[b + FILESECTOROFFSET]);
            num_overwritten_files++;
        }

        int b = linear_sector(type, dirtrack(type), file->direntrysector) * BLOCKSIZE + file->direntryoffset;
        image[b + FILETYPEOFFSET] = file->filetype;
        memcpy(image + b + FILENAMEOFFSET, file->pfilename, FILENAMEMAXSIZE);

        if (shadowdirtrack > 0) {
            b = linear_sector(type, shadowdirtrack, file->direntrysector) * BLOCKSIZE + file->direntryoffset;
            image[b + FILETYPEOFFSET] = file->filetype;
            memcpy(image + b + FILENAMEOFFSET, file->pfilename, FILENAMEMAXSIZE);
        }
    }

    if (!quiet && (num_overwritten_files > 0)) {
        printf("%d out of %d files exist and will be overwritten\n", num_overwritten_files, num_files);
    }
}

/* Prints the filetype like the C64 when listing the directory */
static void
print_filetype(int filetype)
{
    if ((filetype & 0x80) == 0) {
        printf("*");
    } else {
        printf(" ");
    }
    printf("%s", filetypename[filetype & 0xf]);
    if ((filetype & 0x40) != 0) {
        printf("<");
    } else {
        printf(" ");
    }
}

/* Prints the allocated tracks and sectors for every new file */
static void
print_file_allocation(image_type type, unsigned char* image, imagefile* files, int num_files)
{
    if(num_files > 0) {
        printf("\nFile allocation:\n");
    }

    for (int i = 0; i < num_files; i++) {
        printf("%3d (0x%02x 0x%02x:0x%02x) \"%s\" => \"%s\" (SL: %d)", files[i].nrSectors, files[i].direntryindex, files[i].direntrysector, files[i].direntryoffset,
               files[i].alocalname, files[i].afilename, files[i].sectorInterleave);

        if ((files[i].mode & MODE_LOOPFILE) && (files[i].sectorInterleave != 0)) {
            printf("\n");

            continue;
        }

        int track = files[i].track;
        int sector = files[i].sector;

        bool firsttrack = true;
        int firstsector = sector;
        bool fileblocks[SECTORSPERTRACK_D81];
        memset(fileblocks, 0, sizeof fileblocks);
        fileblocks[sector] = true;

        int j = 0;
        while (track != 0) {
            if (j == 0) {
                printf("\n    ");
            }
            printf("%02d/%02d", track, sector);
            int offset = linear_sector(type, track, sector) * BLOCKSIZE;
            int next_track = image[offset + 0];
            int next_sector = image[offset + 1];
            if ((track != next_track) && (next_track != 0)) {
                /* track change */
                if (next_sector != 0) {
                    /* interleave violation */
                    printf("!-");
                } else {
                    printf(" -");
                }
            } else if ((next_sector < sector) && (next_track != 0)) {
                /* sector wrap */
                int expected_next_sector = ((sector + abs(files[i].sectorInterleave)) % num_sectors(type, track));
                if (expected_next_sector > 0) {
                  --expected_next_sector;
                }
                bool on_nonempty_firsttrack = (expected_next_sector < next_sector) && firsttrack && (firstsector != 0);
                if ((expected_next_sector != next_sector) && (!on_nonempty_firsttrack)) {
                    while ((expected_next_sector < next_sector) && fileblocks[expected_next_sector]) {
                        ++expected_next_sector;
                    }
                    if (expected_next_sector != next_sector) {
                        /* interleave violation */
                        printf("!.");
                    } else {
                        printf(" .");
                    }
                } else {
                    printf(" .");
                }
            } else if (((next_sector - sector) != abs(files[i].sectorInterleave)) && (next_track != 0)) {
                /* interleave violation */
                printf(" !");
            } else {
                printf("  ");
            }

            if (track != next_track) {
                memset(fileblocks, 0, sizeof fileblocks);
                firsttrack = false;
            }
            track = next_track;
            sector = next_sector;
            if (next_track != 0) {
                fileblocks[sector] = true;
            }

            j++;
            if (j == 10) {
                j = 0;
            }
        }
        printf("\n");
    }
    printf("\n");
}

/* Returns if the file starting on the given filetrack and filesector uses the given track */
static bool
fileontrack(image_type type, unsigned char *image, int track, int filetrack, int filesector)
{
    while (filetrack != 0) {
        if (filetrack == track) {
            return true;
        }

        int block_offset = linear_sector(type, filetrack, filesector) * BLOCKSIZE;
        int next_track = image[block_offset + TRACKLINKOFFSET];
        int next_sector = image[block_offset + SECTORLINKOFFSET];
        filetrack = next_track;
        filesector = next_sector;
    }

    return false;
}

/* Prints all filenames of files that use the given track */
static void
print_track_usage(image_type type, unsigned char *image, int track)
{
    int dirsector = (type == IMAGE_D81) ? 3 : 1;
    do {
        int dirblock = linear_sector(type, dirtrack(type), dirsector) * BLOCKSIZE;

        for (int i = 0; i < DIRENTRIESPERBLOCK; ++i) {
            int entryOffset = i * DIRENTRYSIZE;
            int filetype = image[dirblock + entryOffset + FILETYPEOFFSET] % 0xf;
            if (filetype != 0) {
                int filetrack = image[dirblock + entryOffset + FILETRACKOFFSET];
                int filesector = image[dirblock + entryOffset + FILESECTOROFFSET];
                bool ontrack = (type == IMAGE_D71) ? fileontrack(type, image, track, (filetrack > D64NUMTRACKS) ? filetrack - D64NUMTRACKS : filetrack + D64NUMTRACKS, filesector) : false;
                if (ontrack || fileontrack(type, image, track, filetrack, filesector)) {
                    unsigned char *filename = (unsigned char *) image + dirblock + entryOffset + FILENAMEOFFSET;
                    unsigned char afilename[FILENAMEMAXSIZE + 1];
                    petscii2ascii(filename, afilename, FILENAMEMAXSIZE);
                    printf("\"%s\" ", afilename);
                }
            }
        }

        if (image[dirblock + TRACKLINKOFFSET] == dirtrack(type)) {
            dirsector = image[dirblock + SECTORLINKOFFSET];
        } else {
            dirsector = 0;
        }
    } while (dirsector > 0);
}

/* Prints the BAM allocation map and returns the number of free blocks */
static int
check_bam(image_type type, unsigned char* image)
{
    int sectorsFree = 0;
    int sectorsFreeOnDirTrack = 0;
    int sectorsOccupied = 0;
    int sectorsOccupiedOnDirTrack = 0;

    if (verbose) {
        printf("Block allocation:\n");
    }

    int max_track = (type == IMAGE_D81) ? D81NUMTRACKS
                    : (((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) ? D64NUMTRACKS_EXTENDED
                       : D64NUMTRACKS);
    for (int t = 1; t <= max_track; t++) {

        if (verbose) {
            printf("  %2d: ", t);
        }
        for (int s = 0; s < num_sectors(type, t); s++) {
            if (is_sector_free(type, image, t, s, 0, 0)) {
                if (verbose) {
                    printf("0");
                }
                if (t != dirtrack(type)) {
                    sectorsFree++;
                } else {
                    sectorsFreeOnDirTrack++;
                }
            } else {
                if (verbose) {
                    printf("1");
                }
                if (t != dirtrack(type)) {
                    sectorsOccupied++;
                } else {
                    sectorsOccupiedOnDirTrack++;
                }
            }
        }

        if (type == IMAGE_D71) {
            for (int i = num_sectors(type, t); i < 23; i++) {
                if (verbose) {
                    printf(" ");
                }
            }
            int t2 = t + D64NUMTRACKS;

            if (verbose) {
                printf("%2d: ", t2);
            }
            for (int s = 0; s < num_sectors(type, t2); s++) {
                if (is_sector_free(type, image, t2, s, 0, 0)) {
                    if (verbose) {
                        printf("0");
                    }
                    if (t2 != dirtrack(type)) {
                        sectorsFree++;
                    } else {
                        /* track 53 is usually empty except the extra BAM block */
                        sectorsFreeOnDirTrack++;
                    }
                } else {
                    if (verbose) {
                        printf("1");
                    }
                    sectorsOccupied++;
                }
            }
        }

        for (int i = ((type == IMAGE_D81) ? 42 : 23) - num_sectors(type, t); i > 0; --i) {
            if (verbose) {
                printf(" ");
            }
        }
        if (verbose) {
            print_track_usage(type, image, t);
            printf("\n");
        }
    }
    if (verbose) {
        printf("%3d/%3d blocks free (%d/%d including dir track)\n", sectorsFree, sectorsFree + sectorsOccupied,
               sectorsFree + sectorsFreeOnDirTrack, sectorsFree + sectorsFreeOnDirTrack + sectorsOccupied + sectorsOccupiedOnDirTrack);
    }

    return sectorsFree;
}

/* Prints the given PETSCII filename like the C64 when listing the directory */
static void
print_filename(unsigned char* pfilename)
{
    int ended = 0;
    putc('\"', stdout);
    for (int pos = 0; pos < FILENAMEMAXSIZE; pos++) {
        if (pfilename[pos] == FILENAMEEMPTYCHAR) {
            if (!ended) {
                putc('\"', stdout);
                ended = 1;
            } else {
                putc(' ', stdout);
            }
        } else {
            putc(p2a(pfilename[pos]), stdout);
        }
    }
    if (!ended) {
        putc('\"', stdout);
    } else {
        putc(' ', stdout);
    }
}

#ifdef _WIN32
/* Enables console formatting under Windows if possible */
static bool
EnableVTMode()
{
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut == INVALID_HANDLE_VALUE) {
        return false;
    }
    DWORD dwMode = 0;
    if (!GetConsoleMode(hOut, &dwMode)) {
        return false;
    }
    dwMode |= 0x0004; /* ENABLE_VIRTUAL_TERMINAL_PROCESSING not defined for older SDKs */
    if (!SetConsoleMode(hOut, dwMode)) {
        return false;
    }
    return true;
}
#endif

/* Prints the directory like the C64 when listing the directory */
static void
print_directory(image_type type, unsigned char* image, int blocks_free)
{
    unsigned char aheader[FILENAMEMAXSIZE + 1];
    unsigned char aid[6];
    unsigned char* bam = image + linear_sector(type, dirtrack(type), 0) * BLOCKSIZE;
    petscii2ascii(bam + get_header_offset(type), aheader, FILENAMEMAXSIZE);
    petscii2ascii(bam + get_id_offset(type), aid, 5);

#ifdef _WIN32
    /* Avoid escape values for inverse printing under Windows if they are not supported by the console */
    if (EnableVTMode()) {
        printf("\n0 \033[7m\"%-16s\" %-5s\033[m", aheader, aid);
    } else {
        printf("\n0 \"%-16s\" %-5s", aheader, aid);
    }
#else
    printf("\n0 \033[7m\"%-16s\" %-5s\033[m", aheader, aid);
#endif
    if (verbose) {
        printf("    hash");
    }
    printf("\n");

    int dirsector = (type == IMAGE_D81) ? 3 : 1;
    do {
        int dirblock = linear_sector(type, dirtrack(type), dirsector) * BLOCKSIZE;

        for (int i = 0; i < DIRENTRIESPERBLOCK; ++i) {
            int entryOffset = i * DIRENTRYSIZE;
            int filetype = image[dirblock + entryOffset + FILETYPEOFFSET];
            int blocks = image[dirblock + entryOffset + FILEBLOCKSLOOFFSET] + 256 * image[dirblock + entryOffset + FILEBLOCKSHIOFFSET];

            if (filetype != FILETYPEDEL) {
                unsigned char* filename = (unsigned char*)image + dirblock + entryOffset + FILENAMEOFFSET;
                printf("%-5d", blocks);
                print_filename(filename);
                print_filetype(filetype);
                if (verbose) {
                    printf(" [$%04x]", filenamehash(filename));
                }
                printf("\n");
            }
        }

        if (image[dirblock + TRACKLINKOFFSET] == dirtrack(type)) {
            dirsector = image[dirblock + SECTORLINKOFFSET];
        } else {
            dirsector = 0;
        }
    } while (dirsector > 0);
    printf("%d blocks free.\n", blocks_free);
}

/* Write files to disk */
static void
write_files(image_type type, unsigned char *image, imagefile *files, int num_files, int usedirtrack, int dirtracksplit, int shadowdirtrack, int numdirblocks, int dir_sector_interleave)
{
    unsigned char track = 1;
    unsigned char sector = 0;
    int bytes_to_write = 0;
    int lastTrack = track;
    int lastSector = sector;
    int lastOffset = linear_sector(type, lastTrack, lastSector) * BLOCKSIZE;

    /* make sure the first file already takes first sector per track into account */
    if (num_files > 0) {
        sector = (type == IMAGE_D81) ? 0 : files[0].first_sector_new_track;
    }

    for (int i = 0; i < num_files; i++) {
        imagefile *file = files + i;
        if (type == IMAGE_D81) {
            file->sectorInterleave = 1; /* TODO: disallow setting interleave in case of D81 */
        }

        if ((file->filetype & 0xf) == FILETYPEDEL) {
            if (file->nrSectorsShown == -1) {
                file->nrSectorsShown = file->nrSectors;
            }
            file->track = 0;
            file->sector = 0;
            int entryOffset = linear_sector(type, dirtrack(type), file->direntrysector) * BLOCKSIZE + file->direntryoffset;
            image[entryOffset + FILETRACKOFFSET] = file->track;
            image[entryOffset + FILESECTOROFFSET] = file->sector;
            image[entryOffset + FILEBLOCKSLOOFFSET] = file->nrSectorsShown & 255;
            image[entryOffset + FILEBLOCKSHIOFFSET] = file->nrSectorsShown >> 8;
            if (shadowdirtrack > 0) {
                entryOffset = linear_sector(type, shadowdirtrack, file->direntrysector) * BLOCKSIZE + file->direntryoffset;
                image[entryOffset + FILETRACKOFFSET] = file->track;
                image[entryOffset + FILESECTOROFFSET] = file->sector;
                image[entryOffset + FILEBLOCKSLOOFFSET] = file->nrSectors & 255;
                image[entryOffset + FILEBLOCKSHIOFFSET] = file->nrSectors >> 8;
            }
        } else if (!(file->mode & MODE_LOOPFILE)) { /* loop files are handled later */

            struct stat st;
            stat((char*)files[i].alocalname, &st);

            int fileSize = st.st_size;

            unsigned char* filedata = (unsigned char*)calloc(fileSize, sizeof(unsigned char));
            if (filedata == NULL) {
                fprintf(stderr, "ERROR: Memory allocation error\n");

                exit(-1);
            }
            FILE* f = fopen((char*)file->alocalname, "rb");
            if (f == NULL) {
                fprintf(stderr, "ERROR: Could not open file \"%s\" for reading\n", file->alocalname);

                exit(-1);
            }
            if (fread(filedata, fileSize, 1, f) != 1) {
                fprintf(stderr, "ERROR: Unexpected filesize when reading %s\n", file->alocalname);
                exit(-1);
            }
            fclose(f);

            if ((file->mode & MODE_MIN_TRACK_MASK) > 0) {
                track = (file->mode & MODE_MIN_TRACK_MASK) >> MODE_MIN_TRACK_SHIFT;
                /* note that track may be smaller than lastTrack now */
                if (track > image_num_tracks(type)) {
                    fprintf(stderr, "ERROR: Invalid minimum track %d for file %s (%s) specified\n", track, file->alocalname, file->afilename);

                    exit(-1);
                }
                while ((!usedirtrack)
                        && ((track == dirtrack(type)) || (track == shadowdirtrack)
                            || ((type == IMAGE_D71) && (track == (D64NUMTRACKS + dirtrack(type)))))) { /* .d71 track 53 is usually empty except the extra BAM block */
                    ++track; /* skip dir track */
                }
                if (abs(((int) track) - lastTrack) > 1) {
                    /* previous file's last track and this file's beginning track have tracks in between */
                    sector = (type == IMAGE_D81) ? 0 : file->first_sector_new_track;
                }
            }

            if ((file->mode & MODE_BEGINNING_SECTOR_MASK) > 0) {
                sector = (file->mode & MODE_BEGINNING_SECTOR_MASK) - 1;
            }

            if (((file->mode & MODE_SAVETOEMPTYTRACKS) != 0)
                    || ((file->mode & MODE_FITONSINGLETRACK) != 0)) {

                /* find first empty track */
                int found = 0;
                while (!found) {
                    for (int s = 0; s < num_sectors(type, track); s++) {
                        if (is_sector_free(type, image, track, s, usedirtrack ? numdirblocks : 0, dir_sector_interleave)) {
                            if (s == (num_sectors(type, track) - 1)) {
                                found = 1;
                                /* In first pass, use sector as left by previous file (or as set by -b) to reach first file block quickly. */
                                /* Claus: according to Krill, on real HW tracks are not aligned anyway, so it does not make a difference. */
                                /* Emulators tend to reset the disk angle on track changes, so this should rather be 3. */
                                if (sector >= num_sectors(type, track)) {
                                    if ((file->mode & MODE_BEGINNING_SECTOR_MASK) > 0) {
                                        fprintf(stderr, "ERROR: Invalid beginning sector %d on track %d for file %s (%s) specified\n", sector, track, file->alocalname, file->afilename);

                                        exit(-1);
                                    }

                                    sector %= num_sectors(type, track);
                                }
                            }
                        } else {
                            int prev_track = track;
                            if (file->mode & MODE_SAVECLUSTEROPTIMIZED) {
                                if (track > D64NUMTRACKS) {
                                    int next_track = track - D64NUMTRACKS + 1; /* to next track on first side */
                                    if (next_track < D64NUMTRACKS) {
                                        track = next_track;
                                    } else {
                                        ++track; /* disk full */
                                    }
                                } else {
                                    track += D64NUMTRACKS; /* to same track on second side */
                                }
                            } else {
                                ++track;
                            }
                            while ((!usedirtrack)
                                    && ((track == dirtrack(type)) || (track == shadowdirtrack)
                                        || ((type == IMAGE_D71) && (track == D64NUMTRACKS + dirtrack(type))))) { /* .d71 track 53 is usually empty except the extra BAM block */
                                ++track; /* skip dir track */
                            }
                            if (file->mode & MODE_FITONSINGLETRACK) {
                                int file_size = fileSize;
                                int first_sector = -1;
                                for (int s = 0; s < num_sectors(type, prev_track); s++) {
                                    if (is_sector_free(type, image, prev_track, s, usedirtrack ? numdirblocks : 0, dir_sector_interleave)) {
                                        if (first_sector < 0) {
                                            first_sector = s;
                                        }
                                        file_size -= BLOCKSIZE + BLOCKOVERHEAD;
                                        if (file_size <= 0) {
                                            found = 1;
                                            track = prev_track;
                                            sector = first_sector;
                                            break;
                                        }
                                    }
                                }
                            }

                            if (track > image_num_tracks(type)) {
                                fprintf(stderr, "ERROR: Disk full, file %s (%s)\n", file->alocalname, file->afilename);

                                exit(-1);
                            }
                            break;
                        }
                    } /* for each sector on track */

                    if ((track == (lastTrack + 2))
                            && (file->mode & MODE_BEGINNING_SECTOR_MASK) == 0) {
                        /* previous file's last track and this file's beginning track have tracks in between now */
                        sector = 0;
                    }
                } /* while not found */
            }

            if ((file->mode & MODE_BEGINNING_SECTOR_MASK) > 0) {
                if (sector != ((file->mode & MODE_BEGINNING_SECTOR_MASK) - 1)) {
                    fprintf(stderr, "ERROR: Specified beginning sector of file %s (%s) not free on track %d\n", file->alocalname, file->afilename, track);

                    exit(-1);
                }
            }

            /* found start track, now save file */
            if (type == IMAGE_D81) {
                sector = 0;
            }

            int byteOffset = 0;
            int bytesLeft = fileSize;
            while (bytesLeft > 0) {
                /* Find free track & sector, starting from current T/S forward one revolution, then the next track etc... skip dirtrack (unless -t is active) */
                /* If the file didn't fit before dirtrack then restart on dirtrack + 1 and try again (unless -t is active). */
                /* If the file didn't fit before track 36/41/71 then the disk is full. */

                int blockfound = 0;
                int findSector = 0;

                while (!blockfound) {
                    /* find spare block on the current track */
                    for (int s = sector; s < sector + num_sectors(type, track); s++) {
                        findSector = s % num_sectors(type, track);

                        if (is_sector_free(type, image, track, findSector, usedirtrack ? numdirblocks : 0, dir_sector_interleave)) {
                            blockfound = 1;
                            break;
                        }
                    }

                    if (!blockfound) {
                        /* find next track, use some magic to make up for track seek delay */
                        int prev_track = track;
                        int seek_delay = 1;
                        if (file->mode & MODE_SAVECLUSTEROPTIMIZED) {
                            if (track > D64NUMTRACKS) {
                                track = track - D64NUMTRACKS + 1;
                            } else {
                                track += D64NUMTRACKS;
                                seek_delay = 0; /* switching to the other side, no head movement */
                            }
                        } else {
                            ++track;
                        }
                        if (type == IMAGE_D81) {
                            sector = 0;
                        } else if (file->first_sector_new_track < 0) {
                            sector += seek_delay - 1;
                        } else {
                            sector = file->first_sector_new_track;
                        }
                        sector += num_sectors(type, prev_track);
                        sector %= num_sectors(type, prev_track);

                        while ((!usedirtrack)
                                && ((track == dirtrack(type)) || (track == shadowdirtrack)
                                    || ((type == IMAGE_D71) && (track == D64NUMTRACKS + dirtrack(type))))) { /* .d71 track 53 is usually empty except the extra BAM block */
                            /* Delete old fragments and restart file */
                            if (!dirtracksplit) {
                                if (file->nrSectors > 0) {
                                    int deltrack = file->track;
                                    int delsector = file->sector;
                                    while (deltrack != 0) {
                                        mark_sector(type, image, deltrack, delsector, 1 /* free */);
                                        int offset = linear_sector(type, deltrack, delsector) * BLOCKSIZE;
                                        deltrack = image[offset + 0];
                                        delsector = image[offset + 1];
                                        memset(image + offset, 0, BLOCKSIZE);
                                    }
                                }

                                bytesLeft = fileSize;
                                byteOffset = 0;
                                file->nrSectors = 0;
                            }
                            ++track;
                        }

                        if (track > image_num_tracks(type)) {
                            if (verbose) {
                                print_file_allocation(type, image, files, num_files);
                                check_bam(type, image);
                            }

                            fprintf(stderr, "ERROR: Disk full, file %s (%s)\n", file->alocalname, file->afilename);
                            free(filedata);

                            exit(-1);
                        }
                    }
                } /* while not block found */

                sector = findSector;
                int offset = linear_sector(type, track, sector) * BLOCKSIZE;

                if (bytesLeft == fileSize) {
                    file->track = track;
                    file->sector = sector;
                    lastTrack = track;
                    lastSector = sector;
                    lastOffset = offset;
                } else {
                    image[lastOffset + 0] = track;
                    image[lastOffset + 1] = sector;
                }

                /* write sector */
                bytes_to_write = min(BLOCKSIZE - BLOCKOVERHEAD, bytesLeft);
                memcpy(image + offset + 2, filedata + byteOffset, bytes_to_write);

                bytesLeft -= bytes_to_write;
                byteOffset += bytes_to_write;

                lastTrack = track;
                lastSector = sector;
                lastOffset = offset;

                mark_sector(type, image, track, sector, 0 /* not free */);

                if (num_sectors(type, track) <= abs(file->sectorInterleave)) {
                    fprintf(stderr, "ERROR: Invalid interleave %d on track %d (%d sectors), file %s (%s)\n", file->sectorInterleave, track, num_sectors(type, track), file->alocalname, file->afilename);

                    exit(-1);
                }

                sector += abs(file->sectorInterleave);
                if (sector >= num_sectors(type, track)) {
                    sector -= num_sectors(type, track);
                    if ((file->sectorInterleave >= 0) && (sector > 0)) {
                        --sector; /* subtract one after wrap (supposedly due to large tail gap) */
                    }
                }

                file->nrSectors++;
            } /* while bytes left */

            free(filedata);

            image[lastOffset + 0] = 0x00;
            image[lastOffset + 1] = bytes_to_write + 1;

            /* update directory entry */
            int entryOffset = linear_sector(type, dirtrack(type), file->direntrysector) * BLOCKSIZE + file->direntryoffset;
            image[entryOffset + FILETRACKOFFSET] = file->track;
            image[entryOffset + FILESECTOROFFSET] = file->sector;

            if (file->nrSectorsShown < 0) {
                file->nrSectorsShown = file->nrSectors;
            }
            image[entryOffset + FILEBLOCKSLOOFFSET] = file->nrSectorsShown & 255;
            image[entryOffset + FILEBLOCKSHIOFFSET] = file->nrSectorsShown >> 8;

            if (shadowdirtrack > 0) {
                entryOffset = linear_sector(type, shadowdirtrack, file->direntrysector) * BLOCKSIZE + file->direntryoffset;
                image[entryOffset + FILETRACKOFFSET] = file->track;
                image[entryOffset + FILESECTOROFFSET] = file->sector;

                image[entryOffset + FILEBLOCKSLOOFFSET] = file->nrSectors & 255;
                image[entryOffset + FILEBLOCKSHIOFFSET] = file->nrSectors >> 8;
            }
        }
    } /* for each file */

    /* update loop files */
    for (int i = 0; i < num_files; i++) {
        imagefile *file = files + i;
        if (((file->filetype & 0xf) != FILETYPEDEL) && (file->mode & MODE_LOOPFILE)) {
            int track, sector, offset;
            int index;
            if (find_file(type, image, file->plocalname, &index, &track, &sector, &offset) == DIRSLOTEXISTS) {
                /* read track/sector and nrSectors from disk image */
                int b = linear_sector(type, track, sector) * BLOCKSIZE + offset;
                file->track = image[b + FILETRACKOFFSET];
                file->sector = image[b + FILESECTOROFFSET];
                file->nrSectors = image[b + FILEBLOCKSLOOFFSET] + (image[b + FILEBLOCKSHIOFFSET] << 8);

                /* update directory entry */
                b = linear_sector(type, dirtrack(type), file->direntrysector) * BLOCKSIZE + file->direntryoffset;
                image[b + FILETRACKOFFSET] = file->track;
                image[b + FILESECTOROFFSET] = file->sector;

                if (file->nrSectorsShown == -1) {
                    file->nrSectorsShown = file->nrSectors;
                }
                image[b + FILEBLOCKSLOOFFSET] = file->nrSectorsShown & 255;
                image[b + FILEBLOCKSHIOFFSET] = file->nrSectorsShown >> 8;

                if (shadowdirtrack > 0) {
                    b = linear_sector(type, shadowdirtrack, file->direntrysector) * BLOCKSIZE + file->direntryoffset;
                    image[b + FILETRACKOFFSET] = file->track;
                    image[b + FILESECTOROFFSET] = file->sector;

                    image[b + FILEBLOCKSLOOFFSET] = file->nrSectors & 255;
                    image[b + FILEBLOCKSHIOFFSET] = file->nrSectors >> 8;
                }

                for (int j = 0; j < num_files; j++) {
                    imagefile *other_file = files + j;
                    if ((i != j)
                     && (file->track == other_file->track)
                     && (file->sector == other_file->sector)) {
                        file->sectorInterleave = other_file->sectorInterleave;

                        break;
                    }
                }

                continue;
            } else {
                fprintf(stderr, "ERROR: Loop source file '%s' (%d) not found\n", file->alocalname, i + 1);
                exit(-1);
            }
        }
    }
}

/* Writes 16 bit value to file */
static size_t
write16(unsigned int value, FILE* f)
{
    char byte = value & 0xff;
    size_t bytes_written = fwrite(&byte, 1, 1, f);

    byte = (value >> 8) & 0xff;
    bytes_written += fwrite(&byte, 1, 1, f);

    return bytes_written;
}

/* Writes 32 bit value to file */
static size_t
write32(unsigned int value, FILE* f)
{
    size_t bytes_written = write16(value, f);
    bytes_written += write16(value >> 16, f);

    return bytes_written;
}

/* Performs GCR encoding on 32 bit value */
static void
encode_4_bytes_gcr(char* in, char* out)
{
    static const uint8_t nibble_to_gcr[] = {
        0x0a, 0x0b, 0x12, 0x13,
        0x0e, 0x0f, 0x16, 0x17,
        0x09, 0x19, 0x1a, 0x1b,
        0x0d, 0x1d, 0x1e, 0x15
    };

    out[0] = (nibble_to_gcr[(in[0] >> 4) & 0xf] << 3) | (nibble_to_gcr[ in[0]       & 0xf] >> 2); /* 11111222 */
    out[1] = (nibble_to_gcr[ in[0]       & 0xf] << 6) | (nibble_to_gcr[(in[1] >> 4) & 0xf] << 1) | (nibble_to_gcr[ in[1]       & 0xf] >> 4); /* 22333334 */
    out[2] = (nibble_to_gcr[ in[1]       & 0xf] << 4) | (nibble_to_gcr[(in[2] >> 4) & 0xf] >> 1); /* 44445555 */
    out[3] = (nibble_to_gcr[(in[2] >> 4) & 0xf] << 7) | (nibble_to_gcr[ in[2]       & 0xf] << 2) | (nibble_to_gcr[(in[3] >> 4) & 0xf] >> 3); /* 56666677 */
    out[4] = (nibble_to_gcr[(in[3] >> 4) & 0xf] << 5) |  nibble_to_gcr[ in[3]       & 0xf]; /* 77788888 */
}

/* Writes image as G64 file */
static int
generate_uniformat_g64(unsigned char* image, const char *imagepath)
{
    FILE* f = fopen(imagepath, "wb");

    size_t filepos = 0;

    static const char signature[] = "GCR-1541";
    filepos += fwrite(signature, 1, sizeof signature - 1, f);

    const char version = 0;
    filepos += fwrite(&version, 1, 1, f);

    const char num_tracks = 35 * 2;
    filepos += fwrite(&num_tracks, 1, 1, f);

    const unsigned int track_size = 7692; /* = track_bytes on tracks 1..17 */
    filepos += write16(track_size, f);

    const unsigned int table_size = num_tracks * 4;
    const unsigned int tracks_offset = (int)filepos + (table_size * 2);

    for (int track = 0; track < num_tracks; ++track) {
        unsigned int track_offset = 0;

        if ((track & 1) == 0) {
            track_offset = tracks_offset + ((track >> 1) * (2 + track_size));
        }

        filepos += write32(track_offset, f);
    }

    for (int track = 0; track < num_tracks; ++track) {
        unsigned int bit_rate = 0;

        if ((track & 1) == 0) {
            switch (sectors_per_track[track >> 1]) {
            case 21:
                bit_rate = 3;
                break;
            case 19:
                bit_rate = 2;
                break;
            case 18:
                bit_rate = 1;
                break;
            case 17:
                bit_rate = 0;
                break;
            }
        }

        filepos += write32(bit_rate, f);
    }

    const unsigned char sync[5] = { 0xff, 0xff, 0xff, 0xff, 0xff };
    const char gap[9] = { 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55 };
    char header_gcr[10];

    const unsigned int block_size =
        (sizeof sync)
        + (sizeof header_gcr)
        + (sizeof gap)
        + (sizeof sync)
        + 325; /* data */

    const char id[3] = { '2', 'A', '\0' };

    bool is_uniform = true;

    for (int track = 0; track < (num_tracks >> 1); ++track) {
        int track_bytes = 0;
        int num_sectors = sectors_per_track[track];
        switch (num_sectors) {
        case 21:
            track_bytes = 7692;
            break; /* = track_size */
        case 19:
            track_bytes = 7142;
            break;
        case 18:
            track_bytes = 6666;
            break;
        case 17:
            track_bytes = 6250;
            break;
        }

        filepos += write16(track_bytes, f);
        size_t track_begin = filepos;

        int data_bytes = num_sectors * block_size;
        int gap_size = (track_bytes - data_bytes) / num_sectors;
        if (gap_size < 0) {
            printf("\nERROR: Track too small for G64 output\n");
            fclose(f);

            return -1;
        }

        float average_gap_remainder = (((float) (track_bytes - data_bytes)) / num_sectors) - gap_size;
        if (average_gap_remainder >= 1.0f) {
            average_gap_remainder = 0.0f; /* 0..1 */
        }

        float remainder = 0.0f;
        for (int sector = 0; sector < num_sectors; ++sector) {
            unsigned int gap_bytes = gap_size;
            remainder += average_gap_remainder;
            if (remainder >= 0.5f) {
                remainder -= 1.0f;
                ++gap_bytes;
            }

            filepos += fwrite(sync, 1, sizeof sync, f);
            char header[8] = {
                0x08, /* header ID */
                (char) (sector ^ (track + 1) ^ id[1] ^ id[0]), /* checksum */
                (char) sector,
                (char) (track + 1),
                id[1],
                id[0],
                0x0f, 0x0f
            };

            encode_4_bytes_gcr(header, header_gcr);
            encode_4_bytes_gcr(header + 4, header_gcr + 5);

            filepos += fwrite(header_gcr, 1, sizeof header_gcr, f);
            filepos += fwrite(gap, 1, sizeof gap, f);

            filepos += fwrite(sync, 1, sizeof sync, f);

            char group[5];

            char checksum = image[0] ^ image[1] ^ image[2];
            char data[4] = { 0x07, (char) image[0], (char) image[1], (char) image[2] };
            encode_4_bytes_gcr(data, group);
            filepos += fwrite(group, 1, sizeof group, f);
            for (int i = 0; i < 0x3f; ++i) {
                data[0] = image[(i * 4) + 3];
                data[1] = image[(i * 4) + 4];
                data[2] = image[(i * 4) + 5];
                data[3] = image[(i * 4) + 6];
                encode_4_bytes_gcr(data, group);
                filepos += fwrite(group, 1, sizeof group, f);
                checksum ^= (data[0] ^ data[1] ^ data[2] ^ data[3]);
            }
            data[0] = image[0xff];
            data[1] = data[0] ^ checksum;
            data[2] = 0;
            data[3] = 0;
            encode_4_bytes_gcr(data, group);
            filepos += fwrite(group, 1, sizeof group, f);

            for (int i = gap_bytes; i > 0; --i) {
                filepos += fwrite(&gap, 1, 1, f);
            }

            image += 0x0100;
        } /* for each sector */

        size_t tail_gap = track_bytes - filepos + track_begin;
        if (tail_gap > 0) {
            for (size_t i = tail_gap; i > 0; --i) {
                filepos += fwrite(&gap, 1, 1, f);
            }

            is_uniform = false;
        }

        for (int i = (track_size - track_bytes); i > 0; --i) {
            filepos += fwrite(sync, 1, 1, f);
        }
    } /* for each track */

    fclose(f);

    if (!is_uniform) {
        printf("\nWARNING: \"%s\" is not UniFormAt'ed\n", imagepath);
    }

    return 0;
}

/* Performs strict CBM DOS validation on the image */
static void
validate(image_type type, unsigned char* image)
{
    /* determine number of blocks */
    int num_blocks = 0;
    for (unsigned int t = 1; t <= image_num_tracks(type); t++) {
        num_blocks += num_sectors(type, t);
    }
    /* create block allocation table */
    int *atab = (int *)calloc(num_blocks, sizeof(int));
    if (atab == NULL) {
        fprintf(stderr, "ERROR: error allocating memory");
        exit(-1);
    }
    /* check format specifier */
    int format = image[linear_sector(type, dirtrack(type), 0) * BLOCKSIZE + 2];
    if (format != 0x41) {
        fprintf(stderr, "ERROR: validation failed, format specifier in directory (0x%02x) does not specify 1541 (0x41)\n", format);
        exit(-1);
    }
    /* check each directory entry and set block allocation table */
    atab[linear_sector(type, dirtrack(type), 0)] = ALLOCATED;
    unsigned int dt = dirtrack(type);
    int dirsector = 1;
    unsigned int start_track = 1;
    while (start_track != 0) {
        atab[linear_sector(type, dt, dirsector)] = ALLOCATED;
        int dirblock = linear_sector(type, dt, dirsector) * BLOCKSIZE;
        for (int direntry = 0; direntry < DIRENTRIESPERBLOCK; direntry++) {
            int entryOffset = direntry * DIRENTRYSIZE;
            int filetype = image[dirblock + entryOffset + FILETYPEOFFSET] & 0xf;
            if (filetype > 4) {
                fprintf(stderr, "ERROR: validation failed, illegal file type (0x%02x) in directory\n", filetype);
                exit(-1);
            }
            if (filetype != 0) { /* skip deleted entries */
                start_track = image[dirblock + entryOffset + FILETRACKOFFSET];
                int start_sector = image[dirblock + entryOffset + FILESECTOROFFSET];
                if (start_track == 0 || start_track > image_num_tracks(type)) {
                    fprintf(stderr, "ERROR: validation failed, illegal track reference (%d) in directory\n", start_track);
                    exit(-1);
                }
                if (start_sector >= num_sectors(type, start_track)) {
                    fprintf(stderr, "ERROR: validation failed, illegal sector reference (track %d, sector %d) in directory\n", start_track, start_sector);
                    exit(-1);
                }
                if (atab[linear_sector(type, start_track, start_sector)] == ALLOCATED) {
                    fprintf(stderr, "ERROR: validation failed, file starts in the middle of another file (track %d, sector %d)\n", start_track, start_sector);
                    exit(-1);
                }
                if (atab[linear_sector(type, start_track, start_sector)] != FILESTART) { /* loop files are allowed */
                    atab[linear_sector(type, start_track, start_sector)] = FILESTART;
                    /* follow sector chain */
                    unsigned int track = start_track;
                    int sector = start_sector;
                    while (1) {
                        int block_offset = linear_sector(type, track, sector) * BLOCKSIZE;
                        track = image[block_offset + TRACKLINKOFFSET];
                        sector = image[block_offset + SECTORLINKOFFSET];
                        if (track == 0) {
                            break;
                        }
                        if (track > image_num_tracks(type)) {
                            fprintf(stderr, "ERROR: validation failed, illegal track reference (%d) in file sector chain\n", track);
                            exit(-1);
                        }
                        if (sector >= num_sectors(type, track)) {
                            fprintf(stderr, "ERROR: validation failed, illegal sector reference in file sector chain (track %d, sector %d)\n", track, sector);
                            exit(-1);
                        }
                        if (atab[linear_sector(type, track, sector)] != UNALLOCATED) {
                            fprintf(stderr, "ERROR: validation failed, sector (track %d, sector %d) is referenced more than once\n", track, sector);
                            exit(-1);
                        }
                        atab[linear_sector(type, track, sector)] = ALLOCATED;
                    }
                }
            }
        }
        dt = image[dirblock + TRACKLINKOFFSET];
        dirsector = image[dirblock + SECTORLINKOFFSET];
        if (dt == 0) {
            break;
        }
    }
    /* check BAM for consistency with block allocation table */
    for (unsigned int t = 1; t <= image_num_tracks(type); t++) {
        unsigned char* bitmap = image + get_bam_offset(type, t);
        int num_free = 0;
        for (int s = 0; s < num_sectors(type, t); s++) {
            int atab_used = (atab[linear_sector(type, t, s)] != UNALLOCATED);
            int bam_used = ((bitmap[s >> 3] & (1 << (s & 7))) == 0);
            num_free += (1 - bam_used);
            if (bam_used != atab_used) {
                fprintf(stderr, "ERROR: validation failed, BAM (%s) is not consistent with files (%s) for track %d sector %d\n", bam_used ? "used" : "free", atab_used ? "used" : "free", t, s);
                exit(-1);
            }
        }
        if (*(bitmap - 1) != num_free) {
            fprintf(stderr, "ERROR: validation failed, BAM number of free blocks (%d) is not consistent with bitmap (%#02x%#02x%#02x) for track %d\n", *(bitmap - 1), *bitmap, *(bitmap + 1), *(bitmap + 2), t);
            exit(-1);
        }
    }
    free(atab);
}

int
cc1541_main(int argc, char* argv[])
{
    imagefile files[MAXNUMFILES_D81];
    memset(files, 0, sizeof files);

    image_type type = IMAGE_D64;
    char* imagepath = NULL;
    char* filename_g64 = NULL;
    unsigned char* header  = (unsigned char*)"cc1541";
    unsigned char* id      = (unsigned char*)"00 2a";
    int dirtracksplit = 1;
    int usedirtrack = 0;
    unsigned int shadowdirtrack = 0;

    int default_first_sector_new_track = 0;
    int first_sector_new_track = 0;
    int defaultSectorInterleave = 10;
    int sectorInterleave = 0;
    int dir_sector_interleave = 3;
    int numdirblocks = 2;
    int nrSectorsShown = -1;
    unsigned char* filename = NULL;
    int set_header = 0;
    int nooverwrite = 0;
    int dovalidate = 0;
    int ignore_collision = 0;
    int filetype = 0x82; /* default is closed PRG */

    /* flags to detect illegal settings for D81 */
    int sector_interleave_set = 0;
    int default_sector_interleave_set = 0;
    int file_start_sector_set = 0;
    int new_track_start_sector_set = 0;

    int retval = 0;

    int i, j;

    if (argc == 1 || strcmp(argv[argc-1], "-h") == 0) {
        usage();
    }
    for (j = 1; j < argc - 1; j++) {
        if (strcmp(argv[j], "-n") == 0) {
            if (argc < j + 2) {
                fprintf(stderr, "ERROR: Error parsing argument for -n\n");
                return -1;
            }
            header = (unsigned char*)argv[++j];
            set_header = 1;
        } else if (strcmp(argv[j], "-i") == 0) {
            if (argc < j + 2) {
                fprintf(stderr, "ERROR: Error parsing argument for -i\n");
                return -1;
            }
            id = (unsigned char*)argv[++j];
            set_header = 1;
        } else if (strcmp(argv[j], "-M") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &max_hash_length)) {
                fprintf(stderr, "ERROR: Error parsing argument for -M\n");
                return -1;
            }
            if ((max_hash_length < 1) || (max_hash_length > FILENAMEMAXSIZE)) {
                fprintf(stderr, "ERROR: Hash computation maximum filename length %d specified\n", max_hash_length);
                return -1;
            }
        } else if (strcmp(argv[j], "-m") == 0) {
            ignore_collision = 1;
        } else if (strcmp(argv[j], "-F") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &first_sector_new_track)) {
                fprintf(stderr, "ERROR: Error parsing argument for -F\n");
                return -1;
            }
            new_track_start_sector_set = 1;
        } else if (strcmp(argv[j], "-S") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &defaultSectorInterleave)) {
                fprintf(stderr, "ERROR: Error parsing argument for -S\n");
                return -1;
            }
            default_sector_interleave_set = 1;
        } else if (strcmp(argv[j], "-s") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &sectorInterleave)) {
                fprintf(stderr, "ERROR: Error parsing argument for -s\n");
                return -1;
            }
            sector_interleave_set = 1;
        } else if (strcmp(argv[j], "-f") == 0) {
            if (argc < j + 2) {
                fprintf(stderr, "ERROR: Error parsing argument for -f\n");
                return -1;
            }
            filename = (unsigned char*)argv[++j];
        } else if (strcmp(argv[j], "-e") == 0) {
            files[num_files].mode |= MODE_SAVETOEMPTYTRACKS;
        } else if (strcmp(argv[j], "-E") == 0) {
            files[num_files].mode |= MODE_FITONSINGLETRACK;
        } else if (strcmp(argv[j], "-r") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &i)) {
                fprintf(stderr, "ERROR: Error parsing argument for -r\n");
                return -1;
            }
            if ((i < 1) || (((i << MODE_MIN_TRACK_SHIFT) & MODE_MIN_TRACK_MASK) != (i << MODE_MIN_TRACK_SHIFT))) {
                fprintf(stderr, "ERROR: Invalid minimum track %d specified\n",  i);
                return -1;
            }
            files[num_files].mode = (files[num_files].mode & ~MODE_MIN_TRACK_MASK) | (i << MODE_MIN_TRACK_SHIFT);
        } else if (strcmp(argv[j], "-b") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &i)) {
                fprintf(stderr, "ERROR: Error parsing argument for -b\n");
                return -1;
            }
            if ((i < 0) || (i >= num_sectors(type, 1))) {
                fprintf(stderr, "ERROR: Invalid beginning sector %d specified\n", i);
                return -1;
            }
            files[num_files].mode = (files[num_files].mode & ~MODE_BEGINNING_SECTOR_MASK) | (i + 1);
            file_start_sector_set = 1;
        } else if (strcmp(argv[j], "-c") == 0) {
            files[num_files].mode |= MODE_SAVECLUSTEROPTIMIZED;
        } else if (strcmp(argv[j], "-o") == 0) {
            nooverwrite = 1;
        } else if (strcmp(argv[j], "-V") == 0) {
            dovalidate = 1;
        } else if (strcmp(argv[j], "-T") == 0) {
            if (argc < j + 2) {
                fprintf(stderr, "ERROR: Error parsing argument for -T\n");
                return -1;
            }
            if (strcmp(argv[j + 1], "DEL") == 0) {
                filetype = (filetype & 0xf0) | FILETYPEDEL;
            } else if (strcmp(argv[j + 1], "SEQ") == 0) {
                filetype = (filetype & 0xf0) | FILETYPESEQ;
            } else if (strcmp(argv[j + 1], "PRG") == 0) {
                filetype = (filetype & 0xf0) | FILETYPEPRG;
            } else if (strcmp(argv[j + 1], "USR") == 0) {
                filetype = (filetype & 0xf0) | FILETYPEUSR;
            } else if (strcmp(argv[j + 1], "REL") == 0) {
                filetype = (filetype & 0xf0) | FILETYPEREL;
            } else {
                fprintf(stderr, "ERROR: Error parsing argument for -T\n");
                return -1;
            }
            j++;
        } else if (strcmp(argv[j], "-O") == 0) {
            filetype &= 0x7f;
        } else if (strcmp(argv[j], "-P") == 0) {
            filetype |= 0x40;
        } else if (strcmp(argv[j], "-w") == 0) {
            if (argc < j + 2) {
                fprintf(stderr, "ERROR: Error parsing argument for -w\n");
                return -1;
            }
            struct stat st;
            if (stat(argv[j + 1], &st) != 0) {
                fprintf(stderr, "ERROR: File '%s' (%d) not found\n", argv[j + 1], num_files + 1);
                return -1;
            }
            files[num_files].alocalname = (unsigned char*)argv[j + 1];
            if (filename == NULL) {
                files[num_files].afilename = basename(files[num_files].alocalname);
                ascii2petscii(files[num_files].afilename, files[num_files].pfilename, FILENAMEMAXSIZE); /* do not eval escapes when converting the filename, as the local filename could contain the escape char */
            } else {
                files[num_files].afilename = filename;
                evalhexescape(files[num_files].afilename, files[num_files].pfilename, FILENAMEMAXSIZE);
            }
            files[num_files].sectorInterleave = sectorInterleave ? sectorInterleave : defaultSectorInterleave;
            files[num_files].first_sector_new_track = first_sector_new_track;
            files[num_files].nrSectorsShown = nrSectorsShown;
            files[num_files].filetype = filetype;
            first_sector_new_track = default_first_sector_new_track;
            filename = NULL;
            sectorInterleave = 0;
            nrSectorsShown = -1;
            filetype = 0x82;
            num_files++;
            j++;
        } else if (strcmp(argv[j], "-l") == 0) {
            if (argc < j + 2) {
                fprintf(stderr, "ERROR: Error parsing argument for -l\n");
                return -1;
            }
            files[num_files].alocalname = (unsigned char*)argv[j + 1];
            evalhexescape(files[num_files].alocalname, files[num_files].plocalname, FILENAMEMAXSIZE);
            if (filename == NULL) {
                fprintf(stderr, "ERROR: Loop files require a filename set with -f\n");
                return -1;
            }
            files[num_files].afilename = filename;
            evalhexescape(files[num_files].afilename, files[num_files].pfilename, FILENAMEMAXSIZE);
            files[num_files].mode |= MODE_LOOPFILE;
            files[num_files].sectorInterleave = 0;
            files[num_files].first_sector_new_track = first_sector_new_track;
            first_sector_new_track = default_first_sector_new_track;
            files[num_files].nrSectorsShown = nrSectorsShown;
            files[num_files].filetype = filetype;
            filename = NULL;
            sectorInterleave = 0;
            nrSectorsShown = -1;
            filetype = 0x82;
            num_files++;
            j++;
        } else if (strcmp(argv[j], "-x") == 0) {
            dirtracksplit = 0;
        } else if (strcmp(argv[j], "-t") == 0) {
            usedirtrack = 1;
        } else if (strcmp(argv[j], "-d") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%u", &shadowdirtrack)) {
                fprintf(stderr, "ERROR: Error parsing argument for -d\n");
                return -1;
            }
        } else if (strcmp(argv[j], "-u") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &numdirblocks)) {
                fprintf(stderr, "ERROR: Error parsing argument for -u\n");
                return -1;
            }
        } else if (strcmp(argv[j], "-B") == 0) {
            if ((argc < j + 2) || !sscanf(argv[++j], "%d", &nrSectorsShown)) {
                fprintf(stderr, "ERROR: Error parsing argument for -B\n");
                return -1;
            }
            if (nrSectorsShown < 0 || nrSectorsShown > 65535) {
                fprintf(stderr, "ERROR: Argument must be between 0 and 65535 for -B\n");
                return -1;
            }
        } else if (strcmp(argv[j], "-4") == 0) {
            type = IMAGE_D64_EXTENDED_SPEED_DOS;
        } else if (strcmp(argv[j], "-5") == 0) {
            type = IMAGE_D64_EXTENDED_DOLPHIN_DOS;
        } else if(strcmp(argv[j], "-g") == 0) {
            if (argc < j + 2) {
                fprintf(stderr, "ERROR: Error parsing argument for -g\n");
                return -1;
            }
            filename_g64 = argv[++j];
        } else if (strcmp(argv[j], "-q") == 0) {
            quiet = 1;
        } else if (strcmp(argv[j], "-v") == 0) {
            verbose = 1;
        } else if (strcmp(argv[j], "-h") == 0) {
            usage();
        } else {
            fprintf(stderr, "ERROR: Error parsing commandline at \"%s\"\n", argv[j]);
            printf("Use -h for help.\n");
            return -1;
        }
    }
    if (j >= argc) {
        fprintf(stderr, "ERROR: No image file provided, or misparsed last option\n");
        return -1;
    }
    imagepath = argv[argc-1];

    if (strlen(imagepath) >= 4) {
        if (strcmp(imagepath + strlen(imagepath) - 4, ".d71") == 0) {
            if ((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) {
                fprintf(stderr, "ERROR: Extended .d71 images are not supported\n");
                return -1;
            }
            type = IMAGE_D71;
        } else if (strcmp(imagepath + strlen(imagepath) - 4, ".d81") == 0) {
            if ((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) {
                fprintf(stderr, "ERROR: Extended .d81 images are not supported\n");
                return -1;
            }
            type = IMAGE_D81;
            dir_sector_interleave = 1;
        }
    }

    if (filename_g64 != NULL && type != IMAGE_D64) {
        fprintf(stderr, "ERROR: G64 output is only supported for non-extended D64 images\n");
        return -1;
    }

    /* Check for unsupported settings for D81 */
    if (type == IMAGE_D81) {
        if (default_sector_interleave_set) {
            fprintf(stderr, "ERROR: -S is not supported for D81 images\n");
            return -1;
        }
        if (sector_interleave_set) {
            fprintf(stderr, "ERROR: -s is not supported for D81 images\n");
            return -1;
        }
        if (new_track_start_sector_set) {
            fprintf(stderr, "ERROR: -F is not supported for D81 images\n");
            return -1;
        }
        if (file_start_sector_set) {
            fprintf(stderr, "ERROR: -b is not supported for D81 images\n");
            return -1;
        }
    }

    /* quiet has precedence over verbose */
    if(quiet) {
        verbose = 0;
    }

    /* open image */
    unsigned int imagesize = image_size(type);
    unsigned char* image = (unsigned char*)calloc(imagesize, sizeof(unsigned char));
    if (image == NULL) {
        fprintf(stderr, "ERROR: Memory allocation error\n");
        return -1;
    }
    FILE* f = fopen(imagepath, "rb");
    if (f == NULL) {
        if (!quiet) {
            printf("Adding %d files to new image %s\n", num_files, basename((unsigned char*)imagepath));
        }
        initialize_directory(type, image, header, id, shadowdirtrack);
    } else {
        if (!quiet) {
            printf("Adding %d files to existing image %s\n", num_files, basename((unsigned char*)imagepath));
        }
        size_t read_size = fread(image, 1, imagesize, f);
        fclose(f);
        if (read_size != imagesize) {
            if (((type == IMAGE_D64_EXTENDED_SPEED_DOS) || (type == IMAGE_D64_EXTENDED_DOLPHIN_DOS)) && (read_size == D64SIZE)) {
                /* Clear extra tracks */
                memset(image + image_size(IMAGE_D64), 0, image_size(type) - image_size(IMAGE_D64));

                /* Mark all extra sectors unused */
                for (unsigned int t = D64NUMTRACKS + 1; t <= image_num_tracks(type); t++) {
                    for (int s = 0; s < num_sectors(type, t); s++) {
                        mark_sector(type, image, t, s, 1 /* free */);
                    }
                }
            } else {
                fprintf(stderr, "ERROR: Wrong filesize: expected to read %d bytes, but read %d bytes\n", imagesize, (int) read_size);
                return -1;
            }
        }
        if (dovalidate) {
            validate(type, image);
        }
        if (set_header) {
            update_directory(type, image, header, id, shadowdirtrack);
        }
    }

    /* Create directory entries */
    create_dir_entries(type, image, files, num_files, dir_sector_interleave, shadowdirtrack, nooverwrite);

    /* Write files and mark sectors in BAM */
    write_files(type, image, files, num_files, usedirtrack, dirtracksplit, shadowdirtrack, numdirblocks, dir_sector_interleave);

    /* Print allocation info */
    if (verbose) {
        print_file_allocation(type, image, files, num_files);
    }
    int blocks_free = check_bam(type, image);

    /* Print directory */
    if (!quiet) {
        print_directory(type, image, blocks_free);
    }

    /* Save image */
    f = fopen(imagepath, "wb");
    if (fwrite(image, imagesize, 1, f) != 1) {
        fprintf(stderr, "ERROR: Failed to write %s\n", image);
        retval = -1;
    }
    fclose(f);

    /* Save optional g64 image */
    if (filename_g64 != NULL) {
        /* retval might be set to -1 already.  Thus we need to take its
           previous state and OR it with the following return value. */
        retval |= generate_uniformat_g64(image, filename_g64);
    }

    if (!ignore_collision && check_hashes(type, image)) {
        fprintf(stderr, "\nERROR: Filename hash collision detected, image is not compatible with Krill's Loader. Use -m to ignore this error.\n");
        retval = -1;
    }

    free(image);

    return retval;
}
