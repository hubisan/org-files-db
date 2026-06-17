.PHONY: help fmt clippy test build release check ci install install-check update clean

help:
	@echo "Available targets:"
	@echo "  make help          Show this help"
	@echo "  make ci            Run CI checks: fmt, clippy, test, build"
	@echo "  make check         Alias for ci"
	@echo "  make fmt           Check formatting"
	@echo "  make clippy        Run clippy with warnings as errors"
	@echo "  make test          Run all tests"
	@echo "  make build         Build debug binary"
	@echo "  make release       Build release binary"
	@echo "  make install       Install local binary with Cargo.lock, overwrite existing"
	@echo "  make install-check Test install with Cargo.lock, no overwrite"
	@echo "  make update        Update Cargo.lock and run checks"
	@echo "  make clean         Remove build artifacts"

fmt:
	cargo fmt --all --check

clippy:
	cargo clippy --all-targets --all-features -- -D warnings

test:
	cargo test --all-targets --all-features

build:
	cargo build --all-targets --all-features

release:
	cargo build --release

ci: fmt clippy test build

check: ci

install-check:
	cargo install --path . --locked

install:
	cargo install --path . --locked --force

update:
	cargo update
	$(MAKE) ci
	$(MAKE) install-check

clean:
	cargo clean
