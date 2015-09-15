#!/bin/sh

## ralf.herold@gmx.net - 2015-08-15 last edited: 2015-08-21
# for 49 documents: ~ 11 ms per trial (time euctr2json.sh)

# TODO: adapt for MS Windows
# copy /b *.txt combined.txt

#DEBUG:
rm "$1/allfiles.json"

ME="$PWD/$0"

cat "$1/euctr-trials-page_"* > "$1/allfiles.txt"

# notes to myself: sed cannot use + or other
# alternatively install gnu-sed: > brew install gnu-sed
# perl: The -p argument makes sure the code gets executed on
# every line, and that the line gets printed out after that.

# transform to json for import in mongodb
# http://docs.mongodb.org/manual/reference/bios-example-collection/

< "$1/allfiles.txt" perl -pe '
  # this section is faster with perl compared to sed

  # delete non-informative lines
  next if /^$/;
  next if /^Summary$/;
  next if /^This file contains .*$/;

  # remove explanatory information from key F.3.3.1
  next if /^\(For clinical trials recorded/;
  next if /^did not include the words /;
  next if /^or not they would be using contraception/;
  next if /^database on [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/;
  next if /Information not present in EudraCT/;

  # add identifiers for special cases
  #s/^(EudraCT Number .*)$/X.1 $1/g;
  #,Sponsor,National Competent,Clinical Trial Type,Trial Status,Date on,Link

  # sanitise file
  s/\t/ /g;
  s/\r/\n/g;
  s/\n+/\n/g;
  s/\"|\{|\}//g;

  # create id per record from eudract number + country 2 character id
  s/^Link.*search\/trial\/([0-9-]*)\/([A-Z][A-Z])\/$/"_id": "$1-$2"/g;

  # handle single case where colon is in key
  s/^(.+)Opinion: Reason(.+)$/\1Opinion Reason\2/g;
  ' | \
perl -pe '
  # special handling by concatenating multi-line values in D.3.7.
  BEGIN{undef $/;}
  s/(D\.3\.7.+?)\nD\./ ( my $tmp = $1 ) =~ s! \n ! !xg; $tmp."\nD" /exgs;
  #
  #s/\n+([^ABCDEFGNPX][^.][^I 1-9])/$1 /gs;
  #
  s/\n([^ABCDEF])/$1 /gs;
  #
  #s/\n+([^A,B,C,D,E,F,G,N,P,EudraCT Number,Sponsor,National Competent,Clinical Trial Type,Trial Status,Date on,Link]\. )/$1 /gs;
  #s/\n+([^ABCDEFGNP]\. )/$1 /gs;
  # EudraCT Number,Sponsor,National Competent,Clinical Trial Type,Trial Status,Date on,Link]\. )/$1 /gs;
  #
  #s/\n/\r/gs;
  #s/\r+(A|B|C|D|E|F|G|N|P|EudraCT Number|Sponsor|National Competent|Clinical Trial Type|Trial Status|Date on|Link)\. /\n$1. /gs;
  #s/\r/ /gs;
  #
  #s/\n+(?<!=A|B|C|D|E|F|G|N|P|EudraCT Number|Sponsor|National Competent|Clinical Trial Type|Trial Status|Date on|Link)\. /\n$1. /xgs;
  #
  ' | \
perl -pe '
   # split key and value, delete special characters, transform, trim, construct quuoted key value pair
   s/^(.+?): (.*)$/ my ($tmp1, $tmp2) = ( lc ($1), $2);
      $tmp1 =~ s! !_!g;
      $tmp1 =~ s![^a-z0-9_]!!g ;
      $tmp2 =~ s![^a-zA-Z0-9- ]*!!g ;
      $tmp2 =~ s!^\s+|\s+$!!g ;
   "\"".$tmp1."\": \"".$tmp2."\",\n" /exgs;
   ' | \
sed \
  -e 's/\("eudract_number.*$\)/}{\1/g' \
  -e 's/^"dimp": "1",$/"dimp": [ { "_dimp": "1",/g' \
  -e '/^["{}]/!d' \
  -e '/""/d' \
  | \
sed \
  -e '1 s/}{/{/' \
  -e '$ s/\(.*\),/\1}/' \
  | \
perl -pe 'BEGIN{undef $/;}
  # delete comma from last line in record
  s/,\n}{/}\n\n{/g ;

  # now create array with imp(s)
  s/("d[0-9]+_.*"),\n"dimp": "([2-9])",/$1}, \n{ "_dimp": "$2",/g ;
  s/("d[0-9]+_.*"),\n"e1(.*)/$1}\n],\n"e1$2/g ;

  ' \
> "$1/allfiles.json"

# to change:
# lines start with [A,B,C,D,E,F,GN,P,][.] plus
# [Summary,EudraCT Number,Sponsor's Protocol Code Number,National Competent Authority,Clinical Trial Type,Trial Status,Date on which,Link]
# within these blocks remove all punctuation


# to the above the following could be added:
#      $tmp1 =~ s!([a-z0-9]+?_){3}.*!$1!g ;
#   $F[0] =  substr $F[0], 0, 25;
# in order to limit the field width

# adapt delimiters for last entry in document, see
# http://stackoverflow.com/questions/1030787/multiline-search-replace-with-perl

# finally import
#mongo users --eval "db.dropDatabase()"
#importdate=`date +%Y%m%d%H%M%S`
# mongoimport does not return upon exit any useful value, hence redirect stderr to stdout
#mongoimport --db="$2" --collection="$3" --upsert --type=json --file "$1/allfiles.json" 2>&1
