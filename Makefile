
EMACS ?= emacs
EMACS_BATCH := $(EMACS) -Q --batch --no-site-file --load
TEST_ENV_DIR := .emacs.test-env

.PHONY: test-all test clean

test:
	${EMACS_BATCH} test/run-tests.el

test-all: test
	@for dir in packages/*; do \
	  if [ -f $$dir/Makefile ]; then \
	    echo "Running tests in $$dir..."; \
	    $(MAKE) -C $$dir test || exit 1; \
	  fi; \
	done

clean:
	rm -rf $(TEST_ENV_DIR)

