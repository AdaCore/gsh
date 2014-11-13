echo "* repeat a command 5 times"
x=5
while [ $x -gt 0 ]
do
    echo "repeat $(($x-1)) times more"
    x=$(($x-1))
done


echo ""
echo "* all the following should print 42"
echo ""
x_is_21=21
simple=$((x_is_21 + 21))
dollared=$(($x_is_21 + 21))
braced=$((${x_is_21} + 21))

echo 'simple=$((x_is_21 + 21))' is $simple
echo 'dollared=$(($x_is_21 + 21))' is $dollared
echo 'braced=$((${x_is_21} + 21))' is $braced


echo ""
echo "variable as second argument"
x_is_21=21
simple=$((21 + x_is_21))
dollared=$((21 + $x_is_21))
braced=$((21 +      ${x_is_21}  ))

echo 'simple=$((21 + x_is_21))' is $simple
echo 'dollared=$((21 + $x_is_21))' is $dollared
echo 'braced=$((21 +      ${x_is_21} ))' is $braced

echo ""
echo "* no spaces"
echo '$((10*4+2))' is $((10*4+2))
echo '$((x_is_21+x_is_21))' is $((x_is_21+x_is_21))

echo ""
echo "* multiple spaces and parantheses"
echo '$(( 10 *    (4+2 ) - ((40 / 2)   + 2 -2 )  + (2)  ))' is $(( 10 *    (4+2 ) - ((40 / 2)   + 2 -2 ) + (2)  ))

echo ""
echo "* recursive"
echo '$(($((10*4+2))))' is $(($((10*4+2))))
echo '$(( $((10*4)) +2))' is $(( $((10*4)) +2))

echo ""
echo "* unary + / -"
echo '$(( +4 - 8 ))' is $(( +4 - 8 ))
echo '$(( -4 + 8 ))' is $(( -4 + 8 ))
echo '$((3 + + 4))' is $((3 + + 4))
echo '$((3 - - 4))' is $((3 - - 4))

echo ""
echo "* test operators == and !="
echo '$(( 10==10 ))' is $(( 10==10 ))
echo '$(( 10 != 10 ))' is $(( 10 != 10 ))
x=10
echo '$(( x==10 ))' is $(( x==10 ))
echo '$(( x!=10 ))' is $(( x!=10 ))

echo ""
echo "* different treatment of '$x' and x"
x='3 + 4 * 2'
echo x is '3 + 4 * 2'
echo '$(( 2 * $x))' is $(( 2 * $x)) and must be 14
echo '$(( 2 * x))' is $(( 2 * x)) and must be 22

echo ""
echo "affectation"
y=0
z=20
echo '$(( y = z ))' is $(( y = z ))
echo '$(( y = y + z +2 ))' with init y=$y and z=$z : $(( y = y + z+2 )) '(new y='$y')'

echo ""
echo "affectation using undefined variable"
echo '$(( undef = z ))' is $(( undef = z ))
echo '$(( y = undef_2 ))' is $(( y = undef_2 ))


