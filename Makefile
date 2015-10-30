# This variable contains the root dir in which the project is installed
# You can safely change it when calling make install
PREFIX=install

# Check if we are on unix or windows system
EXEEXT:=$(strip $(shell if test "$(OS)" = "Windows_NT"; then echo ".exe"; fi))
LUA_PLAT:=$(strip $(shell if test "$(OS)" = "Windows_NT"; then echo "mingw"; else if test `uname` = 'Darwin'; then echo "macosx"; else echo "linux"; fi; fi))

# Main build target
all: src/posix_shell-lua_bindings.adb
	@echo "building gsh"
	gprbuild -p -P posix_shell -XBUILD=prod
	gprbuild -p -P posix_shell -XBUILD=dev 

src/posix_shell-lua_bindings.adb: unit_test_gen
	@echo "generated unit testing bindings"
	python ./unit_test_gen > $@

# Launch the testsuite
check:
	@(cd testsuite && python ./testsuite -t tmp -j0 $(TEST))

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
	cp -p obj/prod/gsh_unit$(EXEEXT) $(PREFIX)/bin/gsh_unit$(EXEEXT)
	cp -p obj/dev/gsh_unit$(EXEEXT) $(PREFIX)/bin_dev/gsh_unit$(EXEEXT)
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
	gprclean -r -P posix_shell -XBUILD=prod
	gprclean -r -P posix_shell -XBUILD=dev        
