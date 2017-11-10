
(while read a b c; do
   echo "a:'$a'" "b:'$b'" "c:'$c'"
done) << EOF
1 2 3
aa bb cc
hello this is nico
EOF
