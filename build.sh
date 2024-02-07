#!/usr/bin/env bash

mkdir bin -p

cargo build --target x86_64-pc-windows-gnu --release > /dev/null 2>&1
cp target/x86_64-pc-windows-gnu/release/gerac.exe bin/gerac-windows.exe

cargo build --target x86_64-unknown-linux-musl --release > /dev/null 2>&1
cp target/x86_64-unknown-linux-musl/release/gerac bin/gerac-linux

export PATH="/home/devtaube/osxcross/target/bin:$PATH"
export CC=x86_64-apple-darwin14-clang
export CXX=x86_64-apple-darwin14-clang++
export AR=x86_64-apple-darwin14-ar
export CARGO_TARGET_X86_64_APPLE_DARWIN_LINKER=x86_64-apple-darwin14-clang
cargo build --target x86_64-apple-darwin --release > /dev/null 2>&1
cp target/x86_64-apple-darwin/release/gerac bin/gerac-macos