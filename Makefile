# This variable contains the root dir in which the project is installed
# You can safely change it when calling make install
PREFIX=install

# Check if we are on unix or windows system
EXEEXT:=$(strip $(shell if test "$(OS)" = "Windows_NT"; then echo ".exe"; fi))

# Variables that control compilation flags, debuggging mode, ...
BUILD=prod
COVERAGE=false

# Main build target
all:
	@echo "building gsh for $(SYS)"
	gprbuild -p -P posix_shell -XBUILD=$(BUILD) -XCOVERAGE=$(COVERAGE)

# Launch the testsuite
check:
	@(PATH=`cd $(PREFIX); pwd`/bin:$${PATH} && export PATH && cd testsuite && python ./testsuite.py -t tmp -j0 $(TEST))

.PHONY: install
install:
	mkdir -p $(PREFIX)
	mkdir -p $(PREFIX)/etc
	mkdir -p $(PREFIX)/bin
	if [ "$(SYS)" = "windows" ]; then cp -p -r gnutools/* $(PREFIX)/; fi
	cp -r etc/* $(PREFIX)/etc
	cp -p obj/$(BUILD)/no-cov/no-gmem/gsh$(EXEEXT) $(PREFIX)/bin/gsh$(EXEEXT)
	if [ "$(OS)" = "Windows_NT" ]; then \
	  cp -p obj/$(BUILD)/no-cov/no-gmem/gsh$(EXEEXT) $(PREFIX)/bin/sh$(EXEEXT) && \
	  cp -p obj/$(BUILD)/no-cov/no-gmem/gsh$(EXEEXT) $(PREFIX)/bin/bash$(EXEEXT) && \
	  for u in "[" cat command cp echo expr false printf pwd recho test true which wc basename dirname head mkdir rm tail type uname; do \
	     cp -p obj/$(BUILD)/no-cov/no-gmem/builtin$(EXEEXT) $(PREFIX)/bin/$$u$(EXEEXT); \
	  done; \
	fi

clean:
	-rm -rf obj/*
