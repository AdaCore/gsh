echo "check value of $? after assignment"
a=notset
b=notset
c=notset
false
a=`echo $?`$? && b=$? && c=$?
echo "a: $a; b: $b; c: $c"

a=notset
b=notset
c=notset
false
a=$?$? && b=$? && c=$?
echo "a: $a; b: $b; c: $c"

a=notset
b=notset
c=notset
false
a=`echo false; false` true  && b=$? && c=$?
echo "a: $a; b: $b; c: $c"

a=notset
b=notset
c=notset
false
a=`d=; echo $?`$? && b=$? && c=$?
echo "a: $a; b: $b; c: $c"

a=notset
b=notset
c=notset
false
a=$? b=$?`echo $?` c=$?; d=$?
echo "a: $a; b: $b; c: $c; d: $d"

a=notset
b=notset
c=notset
false
a=$? b=`false` c=$?; d=$?
echo "a: $a; b: $b; c: $c; d: $d"
