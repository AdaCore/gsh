all:
	gprmake -P posix_shell

check:
	cd testsuite && ./run.sh

clean:
	-rm -rf obj/*
