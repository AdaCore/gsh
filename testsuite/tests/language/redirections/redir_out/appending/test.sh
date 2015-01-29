echo "PASSED" > redir.out
ls "foo-bar" 2> redir.err
echo "PASSED" >> redir.out
echo "Content"
cat redir.out
