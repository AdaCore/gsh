echo "PASSED" > redir.out
ls "foo-bar" 2> redir.err
echo "Content"
cat redir.out
