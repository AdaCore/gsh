# This variable contains the root dir in which the project is installed
# You can safely change it when calling make install
PREFIX=install

# Find the DDK directory
GCC_BIN:=$(shell dirname $$(which gcc))
DDK_DIR:=$(shell echo `dirname $(GCC_BIN)`/i686-pc-mingw32/include/ddk)

# Check if we are on unix or windows system
SYS:=$(strip $(shell if test -d $(DDK_DIR); then echo 'windows'; else echo 'unix'; fi))
EXEEXT:=$(strip $(shell if test "$(SYS)" = "windows"; then echo ".exe"; fi))

UNAME:=$(strip $(shell uname))
ifeq ($(UNAME),Darwin)
   LUA_TARGET:=macosx
else
   ifeq ($(UNAME),Linux)
      LUA_TARGET:=linux
   else
      # Default on windows
      LUA_TARGET:=mingw
   endif
endif

# Variables that control compilation flags, debuggging mode, ...
BUILD=prod
COVERAGE=false

# Main build target
all:
	@echo "building gsh for $(SYS)"
	gprbuild -p -P posix_shell -XBUILD=$(BUILD) -XCOVERAGE=$(COVERAGE) \
		 -XDDK_DIR=$(DDK_DIR) -XSYS=$(SYS)

lua/install/liblua.a:
	@echo "building lua for $(LUA_TARGET)"
	(cd lua && make $(LUA_TARGET))
	(cd lua && make install INSTALL_TOP=`pwd`/install)

# Use this if you need to recompile libreadline (Windows only)
readline/libreadline.a: readline/Makefile
	cd readline && make

readline/Makefile:
	cd readline && ./configure --disable-shared --build=i686-pc-mingw32 --prefix=`pwd`/readline-install

# Launch the testsuite
check:
	@(PATH=`cd $(PREFIX); pwd`/bin:$${PATH} && export PATH && cd testsuite && python ./testsuite.py -t tmp)

.PHONY: install
install:
	mkdir -p $(PREFIX)
	mkdir -p $(PREFIX)/etc
	if [ "$(SYS)" = "windows" ]; then cp -p -r gnutools/* $(PREFIX)/; fi
	cp -r etc/* $(PREFIX)/etc
	cp -p obj/prod/no-cov/no-gmem/gsh$(EXEEXT) $(PREFIX)/bin/gsh$(EXEEXT)
	if [ "$(SYS)" = "windows" ]; then \
	  cp -p obj/prod/no-cov/no-gmem/gsh$(EXEEXT) $(PREFIX)/bin/sh$(EXEEXT) && \
	  cp -p obj/prod/no-cov/no-gmem/gsh$(EXEEXT) $(PREFIX)/bin/bash$(EXEEXT); \
	fi

clean:
	-rm -rf obj/*
	-rm -rf lua/install
	-(cd lua && make clean)
