echo 'echo "`basename $0 .exe` => $@"' | gsh -s "a b c" "d"
echo 'echo wrong' | gsh -c -s "a" "b" 2>/dev/null
echo $?


