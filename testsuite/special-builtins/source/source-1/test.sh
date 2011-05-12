echo "++ ensure that a variable set in a sourced script is visible"
. ./test.sub
echo "return status: $?"
echo "\$toto='$toto'"

echo "++ use of source builtin without any argument"
. 2>/dev/null
echo "return status: $?"

echo "++ sourcing inexistant script"
. ./foo.sub 2>/dev/null
echo "return status: $?"

echo "++ sourcing a script + positional arg"
. ./test.sub a b c
echo "return status: $?"

