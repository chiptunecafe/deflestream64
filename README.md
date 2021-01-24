# deflestream64

A new player for Deflemask SID tunes based on Krill's Loader to stream VGM data off disk.
Data is compressed using the Bitnax LZ compressor from Bitfire and included in Krill's Loader.

## Usage

Windows: Drag and drop a SID VGM file exported by DefleMask onto the executable

Command line: `deflestream64 <vgm file>`

## Compiling

`cargo build --release`

Requires the Rust toolchain and a C/C++ compiler

## Roadmap

0.1.0: More efficient memory layout, official binaries

0.2.0: Customizable bank sizes and base address, read GD3 metadata

0.3.0: Prettier player, playerless export

## Credits

* [Krill's Loader](https://csdb.dk/release/?id=189130) by Krill/Plush
* [x65](https://github.com/Sakrac/x65) by Carl-Henrik Skårstedt
* [cc1541](https://bitbucket.org/PTV_Claus/cc1541/src/master/) by JackAsser et al.
* [Bitfire/Bitnax](https://github.com/bboxy/bitfire) by Bitbreaker/Oxyron
