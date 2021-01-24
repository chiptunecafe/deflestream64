use std::process::Command;

fn main() {
    // Build Bitnax
    cc::Build::new()
        .file("src/ext/lz.c")
        .warnings(false)
        .compile("bitnax");

    // Build cc1541
    cc::Build::new()
        .file("src/ext/cc1541.c")
        .warnings(false)
        .compile("cc1541");

    // Build x65
    cc::Build::new()
        .cpp(true)
        .file("src/ext/x65.cpp")
        .warnings(false)
        .compile("x65");

    // Build Krill's Loader resident object
    Command::new("ca65")
        .arg("-tc64")
        .arg("--cpu")
        .arg("6502X")
        .arg("-D")
        .arg("EXTCONFIGPATH")
        .arg("-I")
        .arg("./src/c64")
        .arg("-I")
        .arg("./src/ext/loader-v184/shared")
        .arg("-I")
        .arg("./src/ext/loader-v184/loader/include")
        .arg("-o")
        .arg(std::env::var("OUT_DIR").unwrap() + "/resident.o")
        .arg("./src/ext/loader-v184/loader/src/resident.s")
        .output()
        .expect("Failed to build loader resident code");

    // Build Krill's Loader install object
    Command::new("ca65")
        .arg("-tc64")
        .arg("--cpu")
        .arg("6502X")
        .arg("-D")
        .arg("EXTCONFIGPATH")
        .arg("-I")
        .arg("./src/c64")
        .arg("-I")
        .arg("./src/ext/loader-v184/shared")
        .arg("-I")
        .arg("./src/ext/loader-v184/loader/include")
        .arg("-o")
        .arg(std::env::var("OUT_DIR").unwrap() + "/install.o")
        .arg("./src/ext/loader-v184/loader/src/install.s")
        .output()
        .expect("Failed to build loader install code");

    // Link Krill's Loader
    Command::new("cl65")
        .arg("-C")
        .arg("./src/c64/linker.cfg")
        .arg("-o")
        .arg(std::env::var("OUT_DIR").unwrap() + "/loader")
        .arg(std::env::var("OUT_DIR").unwrap() + "/resident.o")
        .arg(std::env::var("OUT_DIR").unwrap() + "/install.o")
        .arg("c64.lib")
        .output()
        .expect("Failed to link loader");
}
