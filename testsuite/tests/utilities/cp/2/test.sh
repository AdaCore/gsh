echo "duplicate and rename a file: cp a copy_a"
cp a copy_a
echo "* ls copy_a"
ls copy_a
echo "* cat copy_a"
cat copy_a

rm -r copy_a

echo ""
echo "duplicate and rename a file - use '-f': cp -f a copy_a"
cp -f a copy_a
echo "* ls copy_a"
ls copy_a
echo "* cat copy_a"
cat copy_a

rm -r copy_a

echo ""
echo "duplicate and rename a file - use '-p': cp -p a copy_a"
cp -p a copy_a
echo "* ls copy_a"
ls copy_a
echo "* cat copy_a"
cat copy_a

rm -r copy_a

echo ""
echo "copy a file into an existing directory: cp a dir_a"
cp a dir_a
echo "* ls dir_a/"
ls dir_a/
echo "* dir dir_a/"
cat dir_a/a

rm -r dir_a/a

echo ""
echo "copy and rename a file into an existing directory: cp a dir_a/copy_a"
cp a dir_a/copy_a
echo "* ls dir_a/"
ls dir_a/
echo "* dir dir_a/copy_a"
cat dir_a/copy_a

rm -r dir_a/copy_a

echo ""
echo "copy and rename a file from a dir: cp dir_a/initial copy_a"
cp dir_a/initial copy_a
echo "* ls copy_a"
ls copy_a
echo "* cat copy_a"
cat copy_a

rm -r copy_a

echo ""
echo "duplicate and rename a dir with -r: cp -r dir_a copy_dir_a"
cp -r dir_a copy_dir_a
echo "* ls copy_dir_a/*"
ls copy_dir_a/*
echo "* cat copy_dir_a/*"
cat copy_dir_a/*

rm -r copy_dir_a

echo ""
echo "duplicate and rename a dir with -R: cp -R dir_b copy_dir_b"
cp -R dir_b copy_dir_b
echo "* ls copy_dir_b"
ls copy_dir_b
echo "* cat copy_dir_b/*"
cat copy_dir_b/*

rm -r copy_dir_b

echo ""
echo "copy several files into an existing dir: cp a b.rst dir_a"
cp a b.rst dir_a
echo "* ls dir_a"
ls dir_a
echo "* cat dir_a/*"
cat dir_a/*

rm dir_a/a dir_a/b.rst

echo ""
echo "copy a file and a dir into an existing dir: cp -r a dir_b dir_a"
cp -r a dir_b dir_a
echo "* ls dir_a"
ls dir_a
echo "* cat dir_a/*"
cat dir_a/*

rm -r dir_a/a dir_a/dir_b

echo ""
echo "last copy prevail: cp -r dir_a/* dir_b/* dir_c"
cp  -r dir_a/* dir_b/* dir_c
echo "* ls dir_c"
ls dir_c
echo "* cat dir_c"
cat dir_c/*

rm -r dir_c/initial* dir_c/nested_dir

