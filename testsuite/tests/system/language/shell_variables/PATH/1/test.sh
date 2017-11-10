(
 PATH=.:$PATH
 export PATH
 cd sub
 sub.sh
)

(
 PATH=$PATH:sub
 export PATH
 sub.sh
)


