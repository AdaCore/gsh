echo "* type coin"
type coin
echo "* command -v coin (should not produce output)"
command -v coin
echo "* command -V coin"
command -V coin
echo "* which coin"
which coin

echo "* option -q for the 3 tested builtins"
type -q
command -q
which -q

echo "* command with several but no -v"
command cp ls echo printf
true
