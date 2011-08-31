.PHONY: all world test src samples reinstall clean

# Default target: build src directory
all: src

# Convenience target to build library, install it, run tests, and build samples
world: src install test samples

# Build/run tests
test:
	make -C $@

# Build the library
src:
	make -C $@

# Build the samples
samples:
	make -C $@

# Convenience target to reinstall library and rebuild tests and samples
reinstall: clean uninstall world

# Clean everything
clean:
	make -C test clean
	make -C src clean
	make -C samples clean

%:
	cd src && make $@
