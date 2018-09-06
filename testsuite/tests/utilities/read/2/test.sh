target="x86_64-pc-mingw32"
field1="a"
IFS="-" read -r field1 field2 field3 field4 <<EOF
$target
EOF
echo IFS=$IFS            # test unset variable before the read
echo field1 = ${field1}  # pre-existing variable but must have change
echo field2 = $field2    # variable created during the read
echo field3 = $field3    # ditto
echo field4 = $field4    # ditto
