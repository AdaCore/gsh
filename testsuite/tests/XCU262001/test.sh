param="abcd"
empty_str=""

echo ""
echo "expansion with -"
echo '${param:-myword}': ${param:-myword}
echo '${empty_str:-myword}': ${empty_str:-myword}

echo ""
echo "expansion with :"
echo '${param:=myword}': ${param:=myword}
echo '${empty_str:=myword}': ${empty_str:=myword}

echo ""
echo "expansion with +"
echo '${param:+myword}': ${param:+myword}
echo '${empty_str:+myword}': ${empty_str:+myword}


echo ""
echo expansion starting with '#'
echo '${#param}': ${#param}

echo ""
echo "expansion with ?"
echo '${param:?myword}': ${param:?myword}
echo '${empty_str:?myword}': ${empty_str:?myword}

echo ""
echo "expansion with %"
x=file.c
echo '${x%.c}.o': ${x%.c}.o

echo ""
echo "expansion with %%"
x=posix/src/std
echo '${x%%/*}': ${x%%/*}
echo '${x%%*/}': ${x%%/*}

echo ""
echo "expansion with #"
x=$HOME/src/cmd
echo '${x#$HOME}': ${x#$HOME}
echo '${x#*/}': ${x#*/}

echo ""
echo "expansion with ##"
x=/one/two/three
echo '${x##*/}': ${x##*/}
