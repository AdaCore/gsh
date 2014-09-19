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
