#! /bin/sh

# something like this...
for i in mod*.hs; do 
  if ! test -f ${i%.hs}.output; then 
     echo "test('${i%.hs}', normal, compile, [''])"
  else if grep error ${i%.hs}.output >/dev/null; then
     echo "test('${i%.hs}', normal, compile_fail, [''])"
  else 
     echo "test('${i%.hs}', normal, compile, [''])"
  fi fi
done
