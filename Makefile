# Makefile

.PHONY: help all prepare test lint compile clean clean-elc

help:
	$(info )
	$(info - make            # Show this help)
	$(info - make help       # Show this help)
	$(info - make all        # Run tests, lint and compile)
	$(info - make prepare    # Install dependencies)
	$(info - make test       # Run tests and installs dependencies unless already installed)
	$(info - make lint       # Lint the package)
	$(info - make compile    # Compiles the files to check for errors/warnings)
	$(info - make clean      # Clean everything)
	$(info - make clean-elc  # Clean .elc, .info and info-dir)
	$(info )

all: test lint compile

prepare:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> PREPARE'
	@eldev --color --packaged --unstable --debug --trace --time prepare

test:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> TEST'
	@eldev --color --packaged --unstable --debug --time test

lint:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT'
	@eldev --color --debug --time lint

compile:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> COMPILE'
	@eldev --color --packaged --unstable --debug --time compile --warnings-as-errors
	@eldev clean .elc > /dev/null

clean:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN ALL'
	@eldev clean all

clean-elc:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN'
	@eldev clean

