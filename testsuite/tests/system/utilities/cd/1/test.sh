(
echo "unknow option: cd -q dir_a"
cd -q dir_a
)

(
echo ""
echo "unknown dir: cd unknown_dir"
cd unknown_dir
)

(
echo ""
echo "cd on file: cd a.txt"
cd a.txt
)

(
echo ""
echo "'-' when no oldpwd: cd -"
cd -
)
