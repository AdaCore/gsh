echo "==== break without arguments should resume in innermost loop ===="
for j in a b; do
    echo "entering outtermost loop (written twice)"
    for k in d e; do
        echo "status should be 0 (written twice)"
        false
        break
        echo "this message should not be printed"
    done
    echo "leaving outtermost loop (written twice)"
done
echo "status: $?"
echo

echo "==== break without arguments should resume in innermost loop (while) ===="
k=2
for j in a b; do
    echo "entering outtermost loop (written twice)"
    while [ $k -gt 0 ]; do
        echo "status should be 0 (written twice)"
        k=`expr $k - 1`
        false
        break
        echo "this message should not be printed"
    done
    echo "leaving outtermost loop (written twice)"
done
echo "status: $?"
echo



echo "==== break with n too high ===="
for k in a b; do
   echo "should be written twice (outter)"
   for j in d e ; do
      echo "should be written twice (inner)"
      break 10
      echo "should not be written"
   done
   echo "should not be written"
done
echo "status: $?"
echo

echo "==== break with n too high (while) ===="
for k in a b; do
   echo "should be written twice (outter)"
   j=2
   while [ $j -gt 2 ]; do
      j=`expr $j - 1`
      echo "should be written twice (inner)"
      break 10
      echo "should not be written"
   done
   echo "should not be written"
done
echo "status: $?"
echo

echo "==== break with n negative ===="
for j in `seq 1 10`; do
     break -1 2>/dev/null
done
echo "status: $?"

echo "==== break with n negative (while) ===="
j=2
while [ $j -gt 2 ]; do
     j=`expr $j - 1`
     break -1 2>/dev/null
done
echo "status: $?"

echo
echo "==== break the middle loop ===="
rm -f inner_loop.txt
echo "+ stdout"
for i in a b; do
   for j in `seq 1 2`; do
      echo "This message should be written 4 times on stdout"
      for k in `seq 1 2`; do
         echo "This message should written 4 times in inner_loop.txt"
         break 2
         echo "FAILED - break ignored ? (still in inner loop)"
      done >> inner_loop.txt
      echo "FAILED - break ignored ? (after inner loo)"
   done
done
echo "status should be 0. Get $?"
echo "+ inner_loop.txt"
cat inner_loop.txt
echo

echo
echo "==== break the middle loop (while)===="
rm -f inner_loop.txt
echo "+ stdout"
for i in a b; do
   for j in `seq 1 2`; do
      echo "This message should be written 4 times on stdout"
      k=2
      while [ $k -gt 0 ]; do
         k=`expr $k - 1`
         echo "This message should written 4 times in inner_loop.txt"
         break 2
         echo "FAILED - break ignored ? (still in inner loop)"
      done >> inner_loop.txt
      echo "FAILED - break ignored ? (after inner loo)"
   done
done
echo "status should be 0. Get $?"
echo "+ inner_loop.txt"
cat inner_loop.txt
echo

# Call of break in a brace compound command
echo "==== break in an inner brace construct ===="
rm -f brace.txt
echo "== stdout =="
for j in `seq 1 2`; do
    echo "this message should written twice on stdout"
    {
       echo "PASSED"
       break
       echo "FAILED - break ignored ?"
    } >> brace.txt
    echo "FAILED - break ignored ?"
done
echo "status should be 0: get $?"
echo "== brace.txt =="
cat brace.txt

# This test should be last as it will cause the shell exit
(echo "+ Last TEST"
 for j in `seq 1 10`; do
    break a 2>/dev/null
 done
 echo "FAILED"
)
echo "status: $?"

