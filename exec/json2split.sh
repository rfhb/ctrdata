#!/bin/bash

## ralf.herold@gmx.net - 2017-01-15
## part of https://github.com/rfhb/ctrdata
# last edited: 2017-01-15
#
# note line endings are to be kep by using in
# .gitattributes for compatibility with cygwin:
# *.sh  text eol=lf
# *.php text eol=lf
#
# time json2split.sh:
# 2017-01-12: real  for  documents ~  ms per trial (MacBookPro2015)

FILE=allfiles.json
COUNT=1


cat "$1/$FILE" | (
  I=0;
  while read line; do
    if [[ $line == *x1* ]];
      then I=$[I+1];
    fi;
    echo $line >> "$1/allfiles-$I.json";
  done;
)

exit

cat "$1/$FILE" | (
  I=0;
  echo -n "" > file0;
  while read line; do
    echo $line >> "$1/file$I";
    if [[ $line == *x1* ]];
      then I=$[I+1];
      echo -n "" > file$I;
    fi;
done;
)


sed 's/x1_eudract_number/\n/g' "$1/$FILE" | while read LINE ; do
  if [ "$LINE" ] ; then
    echo "$LINE" >"$1/${FILE%.*}-${COUNT}.${FILE##*.}"
    COUNT=$((COUNT+1))
  fi
  echo "$COUNT"
done
