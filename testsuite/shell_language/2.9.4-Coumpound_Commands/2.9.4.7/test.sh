dummy="hello"

case $dummy in
  hello) echo "PASSED";;
      *) echo "FAILED";;
esac

case $dummy in
  hella) echo "FAILED";;
  hello | tutu ) echo "PASSED";;
esac
