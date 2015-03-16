(
echo "simple change dir: cd dir_a"
oldpwd=`pwd`
cd dir_a
newpwd=`pwd`
test $oldpwd = $newpwd; echo "currently in $newpwd - path changed (true):" $?;
)

(
echo ""
echo "cd ."
oldpwd=`pwd`
cd .
newpwd=`pwd`
test $oldpwd = $newpwd; echo "currently in $newpwd - path changed (false):" $?;
)

(
echo ""
echo "cd ./dir_a"
oldpwd=`pwd`
cd ./dir_a
newpwd=`pwd`
test $oldpwd = $newpwd; echo "currently in $newpwd - path changed (true):" $?;
)


(
echo ""
echo "cd -"
cd dir_a
cd ../dir_b
oldpwd=`pwd`
cd -
newpwd=`pwd`
test $oldpwd = $newpwd; echo "currently in $newpwd - path changed (true):" $?;
)

(
echo ""
echo "cd with no args"
oldpwd=`pwd`
cd
newpwd=`pwd`
test $oldpwd = $newpwd; echo "currently in $newpwd - path changed (true):" $?;
)

(
echo ""
echo "cd with arg '..'"
cd dir_b/nested_dir
oldpwd=`pwd`
cd ..
newpwd=`pwd`
test $oldpwd = $newpwd; echo "currently in $newpwd - path changed (true):" $?;
)

