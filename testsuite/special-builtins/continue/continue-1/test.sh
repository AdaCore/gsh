echo "==== continue without arguments should resume in innermost loop ===="
for j in a b c; do
    echo "entering outtermost loop (written twice)"
    for k in d e f; do
        echo "status should be 0 (written twice)"
        false
        continue
        echo "FAILED"
    done
    echo "leaving outtermost loop (written twice)"
done

echo "PASSED"

for j in `seq 1 10`; do
    continue 10
done
echo $?

for j in `seq 1 10`; do
     continue -1 2>/dev/null
done
echo $?

for j in `seq 1 2`; do
    echo "Entering inner loop"
    for k in `seq 1 10`; do
         continue 
         echo "FAILED"
    done
    echo "Exiting inner loop"
done
echo $?

echo "+ continue the outter loop"
rm -f inner_loop.txt
echo "== stdout =="
for j in `seq 1 2`; do
    echo "This message should be written twice on stdout"
    for k in `seq 1 2`; do
         echo "This message should written twice in inner_loop.txt"
         continue 2
         echo "FAILED - continue ignored ? (still in inner loop)"
    done >> inner_loop.txt
    echo "FAILED - continue ignored ? (after inner loo)"
done
echo "status should be 0. Get $?"
echo "== inner_loop.txt =="
cat inner_loop.txt


# Call of continue in a brace compound command
echo "+ continue in an inner brace construct"
rm -f brace.txt
echo "== stdout =="
for j in `seq 1 2`; do
    echo "this message should written twice on stdout"
    {
       echo "PASSED"
       continue
       echo "FAILED - continue ignored ?"
    } >> brace.txt
    echo "FAILED - continue ignored ?"
done
echo "status should be 0: get $?"
echo "== brace.txt =="
cat brace.txt

# This test should be last as it will cause the shell exit
echo "+ Last TEST"
for j in `seq 1 10`; do
    continue a 2>/dev/null
done
echo "FAILED"

