test_expr () {
   expected="$2"
   expected_status="$3"
   
   result=`eval "$1" 2>/dev/null`
   result_status=$?

   printf "test: %-24s, output: %-8s, status: %2s\n" "$1" "'$result'" "$result_status"
}

echo "++++ Test string argument ++++"
test_expr "expr toto" "toto" 0
test_expr "expr ''" "" 1
test_expr "expr 0ab"

echo
echo "++++ Test integer argument ++++"
test_expr "expr 4" 4 0
test_expr "expr 0" 0 1
echo

for op in '=' '>=' '>' '<' '<=' '!=' '+' '-' '*' '/' '%' '&' '|'; do
   echo "++++ Test $op operator ++++"
   for left in 0 "''" 'abcd' 'bcde' 42 -42 5; do
       for right in  0 "''" 'abcd' 'bcde' 42 -42 5; do
           test_expr "expr $left '$op' $right" 
       done
   done
   echo
done

expr '(' 5 + 3 ')' \* 4
expr 4 = 4
expr 4 = 5
expr 3 '>' 4
expr 4 '>' 3
expr 3 '<' 4
expr 4 '<' 3

expr abcde : a
expr abcde : ..c.

expr a != 0
expr 0 != a
expr ab != ab
expr 3 != 4
expr 03 != 3

true
