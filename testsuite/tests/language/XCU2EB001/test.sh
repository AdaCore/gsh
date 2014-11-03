echo "a" > tata.txt
echo "b" > tata-2.txt

echo tata*.txt
set -f
echo tata*.txt
set +f 
echo tata*.txt

