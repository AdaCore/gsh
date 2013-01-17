count=1
while test $count -lt 1000; do
   echo "count = $count"
   count="${count}0"
done
echo $?

false
while false; do
   true
done
echo $?

