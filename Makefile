
EMACS ?= emacs
EMACS_BATCH := $(EMACS) -Q --batch --no-site-file --load
TEST_ENV_DIR := .emacs.test-env

.PHONY: test clean

test:
	${EMACS_BATCH} test/run-tests.el

clean:
	rm -rf $(TEST_ENV_DIR)

