{ pkgs ? import <nixpkgs> {}, unstable ? import <nixos-unstable> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.vice unstable.cc65
      (pkgs.latest.rustChannels.stable.rust.override { extensions = [ "rust-src" ]; })
    ];
    RUST_SRC_PATH = "${pkgs.latest.rustChannels.stable.rust-src}/lib/rustlib/src/rust/src";
  }

