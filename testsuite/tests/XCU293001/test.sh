#!/bin/sh

f () {
  echo PASSED
  false
}

no_f () {
  echo FAILED
  false
}

t () {
  echo PASSED
  true
}

no_t () {
  echo FAILED
  true
}


f && no_t && no_t
echo "status: $?"

t && f && no_t
echo "status: $?"

t && t && t
echo "status: $?"

f || t
echo "status: $?"

t || no_f
echo "status: $?"

f || f || t && f && no_t && no_t && no_t || t
echo "status: $?"
