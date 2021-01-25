use std::{
    ffi::CString,
    fs::File,
    io::{BufWriter, Read, Write},
    path::PathBuf,
};

use anyhow::Result;
use argh::FromArgs;
use byteorder::{ByteOrder, LittleEndian};

extern "C" {
    fn lz_main(argc: i32, argv: *const *const i8) -> i32;
    fn cc1541_main(argc: i32, argv: *const *const i8) -> i32;
    fn x65_main(argc: i32, argv: *const *const i8) -> i32;
}

#[derive(FromArgs)]
/// Streaming player for DefleMask SID tunes
struct Args {
    #[argh(positional)]
    /// vgm file path
    file: PathBuf,
}

#[derive(Debug)]
enum VGMCommand {
    Write { to: u8, v: u8 },
    WaitFrame,
    End,
}

impl VGMCommand {
    fn serialize(&self) -> [u8; 2] {
        match self {
            VGMCommand::Write { to, v } => [*to, *v],
            VGMCommand::WaitFrame => [0xff, 0xfe],
            VGMCommand::End => [0xff, 0xff],
        }
    }
}

// This function SHOULD NOT panic as to properly clean up the temp directory
fn run(args: Args, tempdir: PathBuf) -> Result<()> {
    // Read file
    let mut file = File::open(&args.file)?;
    let mut vgm_buf = Vec::new();
    file.read_to_end(&mut vgm_buf)?;
    drop(file);

    // Parse VGM data into simplified commands
    println!(">>> Parsing VGM data");
    let mut data_idx = (LittleEndian::read_u32(&vgm_buf[0x34..0x38]) + 0x34) as usize;
    let mut vgm_done = false;
    let mut commands = Vec::new();
    while !vgm_done {
        match vgm_buf[data_idx] {
            // Any wait
            0x61 => {
                let wait_len = LittleEndian::read_u16(&vgm_buf[data_idx + 1..data_idx + 3]);
                for _ in 0..wait_len / 882 {
                    commands.push(VGMCommand::WaitFrame);
                }
                data_idx += 3;
            }
            // 50hz wait
            0x63 => {
                commands.push(VGMCommand::WaitFrame);
                data_idx += 1;
            }
            // End
            0x66 => {
                commands.push(VGMCommand::End);
                vgm_done = true;
            }
            // Write
            0xb6 => {
                commands.push(VGMCommand::Write {
                    to: vgm_buf[data_idx + 1],
                    v: vgm_buf[data_idx + 2],
                });
                data_idx += 3;
            }
            v => anyhow::bail!("Unknown VGM command {:#x}", v),
        };
    }

    println!(">>> Writing player source");
    let mut player_src_path = tempdir.clone();
    player_src_path.push(format!("main.s"));
    {
        let data = include_str!("./c64/main.s");
        let mut file = File::create(&player_src_path)?;

        // 128 commands (256 bytes) per page, 64 pages per bank
        let n_banks = commands.len() / (128 * 64);
        file.write_all(data.replace("N_BANKS", &format!("{}", n_banks)).as_bytes())?;
    }

    println!(">>> Writing loader files");
    let mut loader_install_path = tempdir.clone();
    loader_install_path.push(format!("loader-install.bin"));
    {
        let data = include_bytes!(concat!(env!("OUT_DIR"), "/loader-install.bin"));
        let mut file = File::create(&loader_install_path)?;
        file.write_all(data)?;
    }
    let mut loader_resident_path = tempdir.clone();
    loader_resident_path.push(format!("loader-resident.bin"));
    {
        let data = include_bytes!(concat!(env!("OUT_DIR"), "/loader-resident.bin"));
        let mut file = File::create(&loader_resident_path)?;
        file.write_all(data)?;
    }

    println!(">>> Assembling and packing player");
    let mut player_out = tempdir.clone();
    player_out.push("player.prg");

    unsafe {
        let args = vec![
            CString::new("x65")?,
            CString::new("-c64")?,
            CString::new("-sect")?,
            CString::new(player_src_path.to_string_lossy().as_ref())?,
            CString::new(player_out.to_string_lossy().as_ref())?,
        ];

        let x65_args: Vec<_> = args.iter().map(|a| a.as_ptr()).collect();
        x65_main(x65_args.len() as i32, x65_args.as_ptr());
    }

    let mut player_packed_out = tempdir.clone();
    player_packed_out.push("player.prg");

    unsafe {
        let args = vec![
            CString::new("lz")?,
            CString::new("--sfx")?,
            CString::new("0x0810")?,
            CString::new("-o")?,
            CString::new(player_packed_out.to_string_lossy().as_ref())?,
            CString::new(player_out.to_string_lossy().as_ref())?,
        ];

        let lz_args: Vec<_> = args.iter().map(|a| a.as_ptr()).collect();
        lz_main(lz_args.len() as i32, lz_args.as_ptr());
    }

    // Set up now so we can push in arguments as we build banks
    let mut cc1541_args = vec![
        CString::new("cc1541")?,
        CString::new("-n")?,
        CString::new("deflestream")?,
        CString::new("-f")?,
        CString::new("player.prg")?,
        CString::new("-w")?,
        CString::new(player_packed_out.to_string_lossy().as_ref())?,
    ];

    // 128 commands (256 bytes) per page, 64 pages per bank
    println!(">>> Writing and crunching banks...");
    for (i, chunk) in commands.chunks(128 * 64).enumerate() {
        let mut path = tempdir.clone();
        path.push(format!("bank{}.prg", i));

        {
            let mut file = BufWriter::new(File::create(&path)?);

            // Write PRG load address (hard coded)
            if i % 2 == 0 {
                file.write_all(&[0x00, 0x10])?;
            } else {
                file.write_all(&[0x00, 0x50])?;
            }

            for command in chunk {
                file.write_all(&command.serialize())?;
            }
        }

        let mut outpath = tempdir.clone();
        outpath.push(format!("bank{}.lz", i));

        unsafe {
            let args = vec![
                CString::new("lz")?,
                CString::new("--bitfire")?,
                CString::new("-o")?,
                CString::new(outpath.to_string_lossy().as_ref())?,
                CString::new(path.to_string_lossy().as_ref())?,
            ];

            let lz_args: Vec<_> = args.iter().map(|a| a.as_ptr()).collect();
            lz_main(lz_args.len() as i32, lz_args.as_ptr());
        }

        cc1541_args.push(CString::new("-f")?);
        cc1541_args.push(CString::new(format!("bank{}", i))?);
        cc1541_args.push(CString::new("-w")?);
        cc1541_args.push(CString::new(outpath.to_string_lossy().as_ref())?);
    }

    println!(">>> Building disk image...");
    let d64_path = args.file.with_extension("d64");
    cc1541_args.push(CString::new(d64_path.to_string_lossy().as_ref())?);
    unsafe {
        let cc1541_ptrs: Vec<_> = cc1541_args.iter().map(|a| a.as_ptr()).collect();
        cc1541_main(cc1541_ptrs.len() as i32, cc1541_ptrs.as_ptr());
    }

    println!("---");
    println!("Done! Exported to `{}`", d64_path.display());

    Ok(())
}

// TODOS:
// * Configurable bank size
// * Better VGM parsing
// * NTSC support
fn main() {
    // Parse command line arguments
    let args: Args = argh::from_env();

    let tempdir = mktemp::Temp::new_dir().expect("Failed to create temp directory");
    if let Err(e) = run(args, tempdir.to_path_buf()) {
        eprintln!("{}", e);
    }
}
