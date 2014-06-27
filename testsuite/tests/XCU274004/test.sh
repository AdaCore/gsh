toto="hello"
cat << EOF
My value is $toto or "$toto" and we have a file `echo *.res`

'tata'

Hehe

This is \$ \` \\ and \a \" "\""
EOF

cat << \EOF
My value is $toto or "$toto" and we have a file `echo *.res`

Hehe

This is \$ \` \\ and \a
EOF

cat << "EOF"
My value is $toto or "$toto" and we have a file `echo *.res`

Hehe

EOF

cat << 'EOF'
My value is $toto or "$toto" and we have a file `echo *.res`

Hehe

EOF


