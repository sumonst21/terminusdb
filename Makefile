VERSION=4.2.3
LICENSE=Apache-2.0
MAINTAINER="TerminusDB Team <team@terminusdb.com>"
TARGET=terminusdb
SWIPL=LANG=C.UTF-8 $(SWIPL_DIR)swipl
RONN_FILE=docs/terminusdb.1.ronn
ROFF_FILE=docs/terminusdb.1

################################################################################

# Build the binary (default).
.PHONY: bin
bin: $(TARGET)

# Build the binary and the documentation.
.PHONY: all
all: bin docs

# Build a debug version of the binary.
.PHONY: debug
debug:
	echo "main, halt." | $(SWIPL) -f src/bootstrap.pl

# Quick command for interactive
.PHONY: i
i:
	$(SWIPL) -f src/interactive.pl

# Remove the binary.
.PHONY: clean
clean:
	rm -f $(TARGET)

# Build the documentation.
.PHONY: docs
docs: $(ROFF_FILE)

# Remove the documentation.
.PHONY: docs-clean
docs-clean:
	rm -f $(RONN_FILE) $(ROFF_FILE)

################################################################################

$(TARGET):
	# Build the target and fail for errors and warnings. Ignore warnings
	# having "qsave(strip_failed(..." that occur on macOS.
	$(SWIPL) -t 'main,halt.' -O -q -f src/bootstrap.pl 2>&1 | \
	  grep -v 'qsave(strip_failed' | \
	  (! grep -e ERROR -e Warning)

# Create input for `ronn` from a template and the `terminusdb` help text.
$(RONN_FILE): docs/terminusdb.1.ronn.template $(TARGET)
	HELP="$$(./$(TARGET) help -m)" envsubst < $< > $@

# Create a man page from using `ronn`.
$(ROFF_FILE): $(RONN_FILE)
	ronn --roff $<
