counter=0
. sourced_script.sh
echo "counter: $counter"
mkdir subdir
cd subdir

. ../sourced_script.sh
echo "counter: $counter"
