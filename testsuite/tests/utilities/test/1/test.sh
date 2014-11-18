echo "built-in 'test': -nt and -ot operators"
echo ""
touch a.txt
sleep 2
touch b.txt
touch sametimestamp.txt sametimestamp2.txt
mkdir dir_a
sleep 2
mkdir dir_b
mkdir sametimestamp_dir sametimestamp_dir2
echo "Files"
echo "====="
echo ""
test a.txt -ot b.txt ; echo "a.txt is older than b.txt status (true): $?"
test b.txt -nt a.txt ; echo "b.txt is newer than a.txt status (true): $?"
echo ""
test a.txt -nt b.txt ; echo "a.txt is newer than b.txt status (false): $?"
test b.txt -ot a.txt ; echo "b.txt is older than a.txt status (false): $?"

echo ""
test b.txt -ot notexisting ; echo "b.txt is older than notexisting status (false): $?"
test b.txt -nt notexisting ; echo "b.txt is newer than notexisting status (false): $?"
echo ""
test notexisting -ot b.txt ; echo "notexisting is older than b.txt status (false): $?"
test notexisting -nt b.txt ; echo "notexisting is newer than b.txt status (false): $?"

echo ""
test sametimestamp.txt -ot sametimestamp2.txt ; echo "same time_stamp with older status (false): $?"
test sametimestamp.txt -nt sametimestamp2.txt ; echo "same time_stamp with newer status (false): $?"

echo ""
echo "Directories"
echo "==========="
echo ""
test dir_a -ot dir_b; echo "dir_a is older than dir_b status (true): $?"
test dir_b -nt dir_a ; echo "dir_b is newer than dir_a status (true): $?"
echo ""
test dir_a -nt dir_b ; echo "dir_a is newer than dir_b status (false): $?"
test dir_b -ot dir_a ; echo "dir_b is older than dir_a status (false): $?"

echo ""
test dir_b -ot notexisting ; echo "dir_b is older than notexisting status (false): $?"
test dir_b -nt notexisting ; echo "dir_b is newer than notexisting status (false): $?"

echo ""
test notexisting -ot dir_b ; echo "notexisting is older than dir_b status (false): $?"
test notexisting -nt dir_b ; echo "notexisting is newer than dir_b status (false): $?"

echo ""
test sametimestamp_dir -ot sametimestamp_dir2 ; echo "same time_stamp with older status (false): $?"
test sametimestamp_dir -nt sametimestamp_dir2 ; echo "same time_stamp with newer status (false): $?"


echo ""
echo "Mixing Files and Directories"
echo "============================"
echo ""
test a.txt -ot dir_a; echo "a.txt is older than dir_a status (true): $?"
test dir_a -nt a.txt; echo "dir_a is newer than a.txt status (true): $?"
echo ""
test a.txt -nt dir_a; echo "a.txt is newer than dir_a status (false): $?"
test dir_a -ot a.txt; echo "dir_a is older than a.txt status (false): $?"

rm a.txt b.txt sametimestamp.txt sametimestamp2.txt
rmdir dir_a dir_b sametimestamp_dir sametimestamp_dir2
