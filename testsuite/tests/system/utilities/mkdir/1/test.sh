echo "create simple directories"
mkdir a b
echo $?

echo "create recursively a directory"
mkdir -p a/b/c a 
echo $?

echo "check if error is raised"
mkdir a b
echo $?

echo "check that no error is raised with -p"
mkdir -p a b
echo $?

echo "check creation of directories with spaces"
mkdir -p " b a"/b
echo $?
cd " b a"/b
touch toto.txt
cd ../..
ls */*/toto.txt

true
