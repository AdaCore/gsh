gsh -c 'echo "$0 => $@"' a b c "d e"
gsh -c 2>/dev/null
echo $?

