curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

rustup toolchain add nightly
rustup component add rust-src
rustup component add clippy

cargo +nightly install racer
cargo install ripgrep

# Rust analyzer installation (because VS Code wants us to suffer)
# Requires:
# * Cargo
# * NodeJS
# * NPM
# * rust-src
git clone https://github.com/rust-analyzer/rust-analyzer.git /tmp/rust-analyzer
cd /tmp/rust-analyzer
cargo xtask install
