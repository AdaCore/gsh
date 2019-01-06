for j in `seq 1 10`; do
    break
    echo "FAILED"
done

echo "PASSED"
for j in `seq 1 10`; do
    echo "Entering inner loop"
    for k in `seq 1 10`; do
         break 2
         echo "FAILED"
    done
    echo "FAILED"
done

for j in `seq 1 2`; do
    echo "Entering inner loop"
    for k in `seq 1 10`; do
         break 1
         echo "FAILED"
    done
    echo "Exiting inner loop"
done
echo $?

for j in `seq 1 10`; do
    break 10
done
echo $?

for j in `seq 1 10`; do
     break -1 2>/dev/null
done
echo $?

(for j in `seq 1 10`; do
    break a 2>/dev/null
done
echo "FAILED" 
echo $?
)
true
