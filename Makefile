# Makefile

.PHONY: help prepare test lint compile clean clean-elc

help:
	$(info )
	$(info - make            # Show this help)
	$(info - make help       # Show this help)
	$(info - make prepare    # Install dependencies)
	$(info - make test       # Install dependencies unless already installed and run tests)
	$(info - make lint       # Lint the package)
	$(info - make clean-elc  # Clean .elc, .info and .)
	$(info - make clean      # Clean .elc, .info and .)
	$(info )

prepare:
	@echo 'PREPARE'
	@eldev --color --packaged --unstable --debug --time prepare

test:
	@echo
	@echo 'RUNNING TESTS'
	@echo
	@eldev --color --packaged --unstable --debug --time test

lint:
	@echo
	@echo 'LINTING'
	@eldev --color --debug --time lint

compile:
	@echo
	@echo 'COMPILING'
	@eldev --color --packaged --unstable --debug --time compile --warnings-as-errors
	@eldev clean .elc > /dev/null

clean:
	@eldev clean all

clean-elc:
	@eldev clean

