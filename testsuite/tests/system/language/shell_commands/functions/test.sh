fun_a () { echo "call fun_a"; echo "$1"; echo "$2"; return 0; }

. external_functions.sh

eval 'fun_c ()
{
   echo "call fun_c"
   echo "$1"
   echo "$2"
   false
}'

fun_d () { echo "call fun_d"; exit 0; }

fun_a a_param1 a_param2
fun_b b_param
fun_c c_param
fun_d
echo "should not go here"
