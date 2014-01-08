rm -r foobartoto 2>&1 | grep foobartoto | wc -l
rm -r foobartoto 2>&1 | grep -v foobartoto | wc -l
