echo "erroneous writing - to many op"
(
echo $(( 3 + - + - 4))
)
(
echo $(( 3 * *  4))
)

echo ""
echo "when a variable substitution leads to a non-valid arith expr"
(
str='(3+4'
x=$(($str - 1))
echo $x
)
(
str='(3+4'
x=$((str - 1))
echo $x
)
(
str='this is a string'
x=$(($str - 1))
echo $x
)
(
str='this is a string'
x=$((str - 1))
echo $x
)

echo ""
echo "unknow character"
(
y=$(( 1 ~ 21))
)

echo ""
echo "no variable in affectation"
(
y=$(( 3 = 4 ))
)

echo ""
echo "unhandled operators"
(
x=$((3 ++ 4))
)
(
y=10
echo $((y++)) $y
)
(
y=10
echo $((y--)) $y
)
(
y=10
echo $((++y)) $y
)
(
y=10
echo $((--y)) $y
)
