list="$@"
echo "$list"
echo "$@"
   echo "Quoted @"
   for p in "$@"; do
      echo "param: '$p'"
   done
   echo "NonQuoted @"
   for p in $@; do
      echo "param: '$p'"
   done

