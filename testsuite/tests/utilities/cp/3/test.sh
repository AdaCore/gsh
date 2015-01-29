echo "overwrite into .dotted_folder"
echo "* cat .dotted_folder/a"
cat .dotted_folder/a
cp a .dotted_folder
echo "* cat .dotted_folder/a"
cat .dotted_folder/a

echo ""
echo "copy in relative directory"
echo "* cd relative_dir and ls"
cd  relative_dir
ls .
echo "* duplicate relative_dir/relative_a and ls"
cp relative_a relative_a_copy
ls .
echo "* copy a from previous folder"
cp ../a copy_a
ls .

rm -r copy_a relative_a_copy
cd ..

echo ""
echo "overwrite file with extension"
cat .dotted_folder/to_be_overwritten.txt
cp to_be_overwritten.txt .dotted_folder/
cat .dotted_folder/to_be_overwritten.txt
