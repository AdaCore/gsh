echo "no args: cp"
cp

echo ""
echo "unknow option: cp -q a copy_a"
cp -q a copy_a

echo ""
echo "missing target: cp -f -p a"
cp -f -p a

echo ""
echo "unknown source file: cp unknown new_file"
cp unknown new_file

echo ""
echo "unknown source file in position 2: cp a unknown dir_a"
cp a unknown dir_a

echo ""
echo "unknow target dir: cp a b unknown_dir"
cp a b unknown_dir

echo ""
echo "copy dir with no Recursive option: cp dir_a new_dir_a"
cp dir_a new_dir_a

echo ""
echo "duplicate dir in non-existing dir : cp -r dir_a new_dir/nested_new_dir"
cp -r dir_a new_dir/nested_new_dir


echo ""
echo "copy and rename a dir with -r into a not yet existing dir: cp -r dir_a new_dir/copy_dir_a"
cp -r dir_a new_dir/copy_dir_a
