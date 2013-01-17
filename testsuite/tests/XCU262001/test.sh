toto="abcd"
tutu=""

echo "expansion with -"
tata=${toto:-myword}
echo "$tata"
tata=${tutu:-myword}
echo "$tata"

echo "expansion with :"
tata=${toto:=myword}
echo "$tata"
tata=${tutu:=myword}
echo "$tata"

echo "expansion with +"
tata=${toto:+myword}
echo "$tata"
tata=${tutu:+myword}
echo "$tata"
