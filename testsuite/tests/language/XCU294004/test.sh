counter=0
until test $counter -eq 2; do
  echo "counter: $counter"
  counter=`expr $counter + 1`
done

