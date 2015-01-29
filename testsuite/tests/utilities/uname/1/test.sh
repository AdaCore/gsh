echo "--help"
uname --help

echo ""
echo "no option" # as if -s was specified
unamenoopt=`uname`
echo $unamenoopt

echo ""
echo "option -a"
unamea=`uname -a`
echo $unamea

echo ""
echo "option -m"
uname -m

echo ""
echo "option -n"
uname -n

echo ""
echo "option -r"
uname -r

echo ""
echo "option -s"
unames=`uname -s`
test "$unamenoopt" = "$unames"; echo "'$unames' is equiv to 'uname': '$unamenoopt' (true): " $?

echo ""
echo "option -v"
uname -v

echo ""
echo "options -mnrsvo" # must be equiv to option -a (Cygwin behaviour)
unamemnrsvo=`uname -mnrsvo`
test "$unamea" = "$unamemnrsvo"; echo "'$unamemnrsvo' is equiv to 'uname -a': '$unamea' (true): " $?

echo ""
echo "option -p"
uname -p

echo ""
echo "option -i"
uname -i

echo ""
echo "option -o"
uname -o

echo ""
echo "option -moi -p"
uname -moi -p
