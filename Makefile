PREFIX=install

all:
	gprbuild -p -P posix_shell -XBUILD=prod

check:
	(export PATH=`cd $(PREFIX); pwd`/bin:$${PATH} && cd testsuite && python ./testsuite.py)

.PHONY: install
install:
	mkdir -p $(PREFIX)
	cp -p -r gnutools/* $(PREFIX)/
	cp -p obj/prod/no-cov/no-gmem/gsh.exe $(PREFIX)/bin/sh.exe
	cp -p obj/prod/no-cov/no-gmem/gsh.exe $(PREFIX)/bin/gsh.exe

clean:
	-rm -rf obj/*
