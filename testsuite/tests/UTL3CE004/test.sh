echo "when 'a' does not exist but 'a.exe' does : cp a copy_a"
cp a copy_a
echo "* ls copy_a.exe"
ls copy_a.exe
echo "* cat copy_a.exe"
cat copy_a.exe

rm -r copy_a.exe

echo ""
echo "copy on side of an existing file : cp a b"
cp a b
echo "* ls b*"
ls b*
echo "* cat b*"
cat b*

rm -r b.exe

echo ""
echo "copy c is prioritary on copy of c.exe : cp c copy_c"
cp c copy_c
echo "* cat copy_c"
cat copy_c

rm -r copy_c
