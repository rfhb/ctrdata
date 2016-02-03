#!/bin/sh

## ralf.herold@gmx.net - 2015-08-15 last edited: 2015-09-15
# 2.4 s for 221 documents: ~ 11 ms per trial (time euctr2json.sh)

cat "$1/euctr-trials-page_"* > "$1/allfiles.txt"

# notes to myself: sed cannot use + or other
# alternatively install gnu-sed: > brew install gnu-sed
# perl: The -p argument makes sure the code gets executed on
# every line, and that the line gets printed out after that.

# transform to json for import in mongodb, reference:
# http://docs.mongodb.org/manual/reference/bios-example-collection/

LC_CTYPE=C && LANG=C && < "$1/allfiles.txt" perl -ne '
  # this section is faster with perl compared to sed

  # delete non-informative lines
  next if /^$/;
  next if /^Summary$/;
  next if /^This file contains .*$/;
  next if /Sponsor Information/;

  # remove explanatory information from key F.3.3.1
  next if /^\(For clinical trials recorded/;
  next if /^did not include the words /;
  next if /^or not they would be using contraception/;
  next if /^database on [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/;
  next if /Information not present in EudraCT/;

  # add identifiers for special cases
  s/^(EudraCT Number.*)$/X.1 $1/g;
  s/^.*(Protocol Code Number.*)$/X.2 Sponsor $1/g;
  s/^(Sponsor) (.*)$/B.1 $1: $2/g;
  s/^(National Competent.*)$/X.3 $1/g;
  s/^(Clinical Trial.*)$/X.4 $1/g;
  s/^(Trial Status.*)$/X.5 $1/g;
  s/^(Date on.*)$/X.6 $1/g;
  s/^(Link.*)$/X.7 $1/g;

  # sanitise file
  s/\t/ /g;
  s/\r/\n/g;
  s/\n+/\n/g;
  s/\"|\{|\}//g;

  # handle single case where colon is in key
  s/^(.+)Opinion: Reason(.+)$/$1Opinion Reason$2/g;

  # create id per record from eudract number followed by
  # country 2 character id or by "3rd" for non-EU countries
  s/^X.7 Link.*search\/trial\/([0-9-]*)\/([A-Z][A-Z]|3rd)\/$/xxxxxxxxxx"_id": "$1-\U$2\E"/g;

  # crude attack on newlines within variable fields
  s/^([ABDEFGNPX][.][1-9 I].+)$/\nxxxxxxxxxx$1/g;
  s/\n/ /g;
  s/xxxxxxxxxx/\n/g;

  print $_;

  ' | \
perl -pe '
   # split key and value, delete special characters, transform, trim, construct quoted key value pair
   s/^(.+?): (.*)$/ my ($tmp1, $tmp2) = ( lc ($1), $2);
      $tmp1 =~ s! !_!g;
      $tmp1 =~ s![^a-z0-9_]!!g ;
      $tmp2 =~ s![^a-zA-Z0-9- ]*!!g ;
      $tmp2 =~ s!^\s+|\s+$!!g ;
   "\"".$tmp1."\": \"".$tmp2."\",\n" /exgs;
   ' | \
sed \
  -e 's/\("x1_eudract_number.*$\)/}{\1/g' \
  -e 's/^"dimp": "1",$/"dimp": [ { "_dimp": "1",/g' \
  -e 's/^"b1_sponsor": "1",$/"b1_sponsor": [ { "_b1_sponsor": "1",/g' \
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

  # create array with imp(s)
  s/("d[0-9]+_.*"),\n"dimp": "([2-9])",/$1}, \n{ "_dimp": "$2",/g ;
  s/("d[0-9]+_.*"),\n"e1(.*)/$1}\n],\n"e1$2/g ;

  # create array with sponsor(s), closed before dimp or e11_ elements
  s/("b[0-9]+_.*"),\n"b1_sponsor": "([2-9])",/$1}, \n{ "_b1_sponsor": "$2",/g ;
  s/("b[0-9]+_.*"),\n"dimp(.*)/$1}\n],\n"dimp$2/g ;
  s/("b[0-9]+_.*"),\n"e11_medical(.*)/$1}\n],\n"e11_medical$2/g ;

  ' \
> "$1/allfiles.json"

# the following could be added to limit field width:
#   $tmp1 =~ s!([a-z0-9]+?_){3}.*!$1!g ;
#   $F[0] =  substr $F[0], 0, 25;

# no more needed - could be adapted to create an array:
# perl -pe '
#   # special handling by concatenating multi-line values in D.3.7.
#   #BEGIN{undef $/;}
#   #s/(D\.3\.7.+?)\nD\./ ( my $tmp = $1 ) =~ s! \n ! !xg; $tmp."\nD" /exgs;

# adapt delimiters for last entry in document, see
# http://stackoverflow.com/questions/1030787/multiline-search-replace-with-perl

