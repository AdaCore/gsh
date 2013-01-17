print_params () {
   echo "Quoted *"
   for p in "$*"; do
      echo "param: '$p'"
   done
   echo "NonQuoted *"
   for p in $*; do
      echo "param: '$p'"
   done
}

echo "== no args =="
print_params 
echo "== empty args =="
print_params ''
echo "== three args =="
print_params a b c
echo "== args with space =="
print_params a "b c" d

