for i in `seq 1 9`; do
  echo "(" > tmp.chi
  cat ../self-interpreter.chi >> tmp.chi
  echo ")" >> tmp.chi
  cat case-$i.chi >> tmp.chi
  chi tmp.chi | diff expected-$i.chi -
  if [ $? -ne 0 ]; then
    echo "$i: failed."
  else
    echo "$i: OK."
  fi
done
rm -f tmp.chi
