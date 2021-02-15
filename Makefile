# This variable contains the root dir in which the project is installed
# You can safely change it when calling make install
PREFIX=install

# Check if we are on unix or windows system
EXEEXT:=$(strip $(shell if test "$(OS)" = "Windows_NT"; then echo ".exe"; fi))
PATHSEP:=$(shell if test "$(OS)" = "Windows_NT"; then echo ";"; else echo ":"; fi)

# Main build target
all:
	GPR_PROJECT_PATH="`pwd`/os$(PATHSEP)`pwd`/c$(PATHSEP)`pwd`/gsh$(PATHSEP)$(GPR_PROJECT_PATH)" gprbuild -j0 -p -P posix_shell -XBUILD=prod
	GPR_PROJECT_PATH="`pwd`/os$(PATHSEP)`pwd`/c$(PATHSEP)`pwd`/gsh$(PATHSEP)$(GPR_PROJECT_PATH)" gprbuild -j0 -p -P posix_shell -XBUILD=dev
# Launch the testsuite
check:
	# ON Windows we need a full installation to run correctly the testsuite
	if [ "$(OS)" = "Windows_NT" ]; then \
	   make all PREFIX=testsuite/prod-base; \
	   make install PREFIX=testsuite/prod-base; \
	fi
	@(cd testsuite && python ./run-tests -t tmp -j1 $(TEST))

.PHONY: install
install:
	mkdir -p $(PREFIX)
	mkdir -p $(PREFIX)/etc
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/bin_dev
	if [ "$(OS)" = "Windows_NT" ]; then cp -p -r gnutools/* $(PREFIX)/; fi
	cp -r etc/* $(PREFIX)/etc
	cp -p obj/prod/gsh$(EXEEXT) $(PREFIX)/bin/gsh$(EXEEXT)
	cp -p obj/dev/gsh$(EXEEXT) $(PREFIX)/bin_dev/gsh$(EXEEXT)
	if [ "$(OS)" = "Windows_NT" ]; then \
	  cp -p obj/prod/gsh$(EXEEXT) $(PREFIX)/bin/sh$(EXEEXT) && \
	  cp -p obj/dev/gsh$(EXEEXT) $(PREFIX)/bin_dev/sh$(EXEEXT) && \
	  cp -p obj/prod/gsh$(EXEEXT) $(PREFIX)/bin/bash$(EXEEXT) && \
	  cp -p obj/dev/gsh$(EXEEXT) $(PREFIX)/bin_dev/bash$(EXEEXT) && \
	  for u in "[" cat command cp echo expr false printf pwd recho test true which wc basename dirname head mkdir rm tail type uname; do \
	     cp -p obj/prod/builtin$(EXEEXT) $(PREFIX)/bin/$$u$(EXEEXT); \
	     cp -p obj/dev/builtin$(EXEEXT) $(PREFIX)/bin_dev/$$u$(EXEEXT); \
	  done; \
	fi

clean:
	GPR_PROJECT_PATH="`pwd`/os$(PATHSEP)`pwd`/c$(PATHSEP)`pwd`/gsh$(PATHSEP)$(GPR_PROJECT_PATH)" gprclean -r -P posix_shell -XBUILD=prod
	GPR_PROJECT_PATH="`pwd`/os$(PATHSEP)`pwd`/c$(PATHSEP)`pwd`/gsh$(PATHSEP)$(GPR_PROJECT_PATH)" gprclean -r -P posix_shell -XBUILD=dev        
