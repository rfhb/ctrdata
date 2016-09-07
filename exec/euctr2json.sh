#!/bin/sh

## ralf.herold@gmx.net - 2015-08-15
## part of https://github.com/rfhb/ctrdata
# last edited: 2016-04-20
# time euctr2json.sh:
# 2015-08-15: 2.4 s for 221 documents: ~ 11 ms per trial
# 2016-04-20: 1.2 s for 151 documents: ~  8 ms per trial

cat "$1/euctr-trials-page_"* > "$1/allfiles.txt"

# notes to myself: sed cannot use + or other
# alternatively install gnu-sed: > brew install gnu-sed
# perl: The -p argument makes sure the code gets executed on
# every line, and that the line gets printed out after that.

# transform to json for import in mongodb, reference:
# http://docs.mongodb.org/manual/reference/bios-example-collection/

LC_CTYPE=C && LANG=C && < "$1/allfiles.txt" perl -ne '
  # this section is faster with perl compared to sed

  # get UTC date, time in format correspondsing to the
  # R default for format methods: "%Y-%m-%d %H:%M:%S"
  use POSIX;
  my $dt = POSIX::strftime("%Y-%m-%d %H:%M:%S", gmtime());

  while (<>) {

  # elimination of non-printing characters such as control M
  tr/\015//d;

  # delete non-informative lines
  next if /^$/;
  next if /^Summary/;
  next if /^This file contains/;
  next if /A\. Protocol Information/;
  next if /B\. Sponsor Information/;
  next if /D\. IMP Identification/;
  next if /E\. General Information on the Trial/;
  next if /F\. Population of Trial Subjects/;
  next if /N\. Review by the/;
  next if /P\. End of Trial.$/;
  next if /G. Investigator Networks/;
  next if /H.4 Third Country/;

  # remove explanatory information from key F.3.3.1
  next if /^\(For clinical trials recorded/;
  next if /^did not include the words /;
  next if /^or not they would be using contraception/;
  next if /^database on [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/;
  next if /Information not present in EudraCT/;

  # workarounds
  # - sponsor records were added but left empty -> create placeholder
  s/^(B\.1\.1 Name of Sponsor:)\s+$/$1 empty/g;

  # add identifiers for special cases
  s/^(EudraCT Number.*)$/X.1 $1/g;
  s/^.*(Protocol Code Number.*)$/X.2 Sponsor $1/g;
  s/^(Sponsor) (.*)$/B.1 $1: $2/g;
  s/^(National Competent.*)$/X.3 $1/g;
  s/^(Clinical Trial.*)$/X.4 $1/g;
  s/^(Trial Status.*)$/X.5 $1/g;
  s/^(Date on.*)$/X.6 $1/g;
  s/^(Link.*)$/X.7 $1/g;
  # add identifiers for summary documents
  s/^Sponsor Protocol Number: (.*)$/A.4.1 Sponsors protocol code number: $1/;
  s/^Sponsor Name: (.*)$/B.1 Sponsor: 1xxxxxxxxxxB.1.1 Name of sponsor: $1/;
  s/^Full Title: (.*)$/A.3 Full Title of the Trial: $1/;
  s/^(Start Date:.*)$/X.7 $1/;
  s/^Medical condition: (.*)$/E.1.1 Medical conditions being investigated: $1/;
  s/^Disease: (.*)$/E.1.1.2 Therapeutic area: $1/;
  s/^(Population Age.*)$/X.8 $1/;
  s/^(Gender.*)$/X.9 $1/;
  s/^Trial protocol: (.*)$/X.5 Trial status: $1/;

  # sanitise file
  s/\t/ /g;
  s/\r/\n/g;
  s/\n+/\n/g;
  s/\"|\{|\}//g;

  # todo
  # - eliminate these extra lines that duplicate other fields
  #   "x1_eudract_number"
  #   "x2_sponsor_protocol_code_number"
  #   "x3_national_competent_authority"
  #   "x5_trial_status"

  # handle single case where colon is in key
  s/^(.+)Opinion: Reason(.+)$/$1Opinion Reason$2/g;

  # create id per record from eudract number followed by
  # country 2 character id or by "3rd" for non-EU countries
  # - for details document
  s/^X.7 Link.*search\/trial\/([0-9-]*)\/([A-Z][A-Z]|3rd)\/$/xxxxxxxxxx"_id": "$1-\U$2\E"/;
  # - for summary document
  s/^X.7 Link.*search\/.*:([0-9-]{14})$/xxxxxxxxxx"_id": "$1"/;

  # crude attack on newlines within variable fields
  s/^([ABDEFGHNPX][.][1-9 I].+)$/\nxxxxxxxxxx$1/g;
  s/\n/ /g;
  s/xxxxxxxxxx/\n/g;

  print $_;

  print "\nrecord last import: " . $dt if /X.1/;
  print "\nctrname: EUCTR" if /X.1/;

  }
  ' | \
perl -pe '
   # split key and value, delete special characters,
   # transform, trim, construct quoted key value pair
   s/^(.+?): (.*)$/ my ($tmp1, $tmp2) = (lc ($1), $2);
      $tmp1 =~ s! !_!g;
      $tmp1 =~ s![^a-z0-9_]!!g ;
      $tmp2 =~ s![^a-zA-Z0-9-: ]*!!g ;
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
  s/,\n\}\{/\}\n\n\{/g ;

  # create array with imp(s)
  s/("d[0-9]+_.*"),\n"dimp": "([2-9])",/$1\}, \n\{ "_dimp": "$2",/g ;
  s/("d[0-9]+_.*"),\n"e1(.*)/$1\}\n],\n"e1$2/g ;

  # create array with sponsor(s), closed before dimp or e11_ or a3_full_ elements
  s/("b[0-9]+_.*"),\n"b1_sponsor": "([2-9])",/$1\}, \n\{ "_b1_sponsor": "$2",/g ;
  s/("b[0-9]+_.*"),\n"dimp(.*)/$1\}\n],\n"dimp$2/g ;
  s/("b[0-9]+_.*"),\n"e11_medical(.*)/$1\}\n],\n"e11_medical$2/g ;
  s/("b[0-9]+_.*"),\n"a3_full(.*)/$1\}\n],\n"a3_full$2/g ;

  ' \
> "$1/allfiles.json"

