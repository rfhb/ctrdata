#!/bin/sh

## ralf.herold@gmx.net - 2015-08-15
## part of https://github.com/rfhb/ctrdata
# last edited: 2017-01-12
#
# note line endings are to be kep by using in
# .gitattributes for compatibility with cygwin:
# *.sh  text eol=lf
# *.php text eol=lf
#
# time euctr2json.sh:
# 2015-08-15: 2.4 s for 221 documents: ~ 11 ms per trial (MacBookPro2011)
# 2016-04-20: 1.2 s for 151 documents: ~  8 ms per trial (MacBookPro2011)
# 2016-09-11: 1.2 s for 151 documents: ~  8 ms per trial (MacBookPro2015)
# 2017-01-12: real 1m23.021s for 10978 documents ~ 8 ms per trial (MacBookPro2015)

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
  next if /E\. General Information on the Trial/;
  next if /F\. Population of Trial Subjects/;
  next if /N\. Review by the/;
  next if /P\. End of Trial.$/;
  next if /G. Investigator Networks/;
  next if /H.4 Third Country/;
  next if /MedDRA Classification/;

  # remove explanatory information from key F.3.3.1
  next if /^\(For clinical trials recorded/;
  next if /^did not include the words /;
  next if /^or not they would be using contraception/;
  next if /^database on [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/;

  # workarounds
  # - sponsor records were added but left empty -> create placeholder
  s/^(B\.1\.1 Name of Sponsor:)\s+$/$1 empty/g;

  # add identifiers for special cases
  s/^(EudraCT Number.*)$/X.1 $1/;
  s/^(Sponsor) ([0-9]+)$/B.1 $1: $2/;
  s/^(Clinical Trial.*)$/X.4 $1/;
  s/^(Date on.*)$/X.6 $1/;
  s/^(Link.*)$/X.7 $1/;

  # add identifier for end of arrays
  s/^D\.8 Information on Placebo$/X.9 ENDDMP: TRUE/;
  s/^D\. IMP Identification$/X.9 ENDSPONSOR: TRUE/;
  # for details = FALSE it is:
  s/^Full Title:/\nX.9 ENDSPONSOR: TRUE\nxxxxxxxxxxA.3 Full Title of the Trial:/;

  # add identifiers for summary documents (terminating - needed for later step)
  s/^Sponsor Protocol Number: (.*)$/A.4.1 Sponsors protocol code number: $1/;
  s/^Sponsor Name: (.*)$/B.1 Sponsor: 1xxxxxxxxxxB.1.1 Name of sponsor: $1/;
  s/^Full Title: (.*)$/A.3 Full Title of the Trial: $1-/;
  s/^(Start Date:.*)$/X.7 $1/;
  s/^Medical condition: (.*)$/E.1.1 Medical conditions being investigated: $1/;
  s/^Disease: (.*)$/E.1.1.2 Therapeutic area: $1/;
  s/^(Population Age.*)$/X.8 $1/;
  s/^(Gender.*)$/X.9 $1/;

  # sanitise file
  s/\t/ /g;
  s/\r/\n/g;
  s/\n+/\n/g;
  s/\"|\{|\}//g;

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

  # bring back newlines around identifier for end of arrays
  s/X.9 ENDSPONSOR: TRUE/\nX.9 ENDSPONSOR: TRUE\n/;
  s/\n+/\n/g;

  print $_;

  print "\nrecord last import: " . $dt if /"_id"/;
  print "\nctrname: EUCTR" if /"_id"/;

  }
  ' | \
perl -pe '
   # split key and value, delete special characters,
   # transform, trim, construct quoted key value pair
   s/^(.+?): (.*)$/ my ($tmp1, $tmp2) = (lc ($1), $2);
      $tmp1 =~ s! !_!g;
      $tmp1 =~ s![^a-z0-9_]!!g ;
      $tmp2 =~ s![^a-zA-Z0-9+-:\/ ]*!!g ;
      $tmp2 =~ s!^\s+|\s+$!!g ;
   "\"".$tmp1."\": \"".$tmp2."\",\n" /exgs;
   ' | \
sed \
  -e 's/\("x1_eudract_number.*$\)/}{/g' \
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
  s/("d[0-9]+_.*"),\n"x9_enddmp.*/$1\}\n],/g ;

  # create array with sponsor(s), closed before:
  # dimp or e11_ or a3_full_ elements
  s/("b[0-9]+_.*"),\n"b1_sponsor": "([2-9])",/$1\}, \n\{ "_b1_sponsor": "$2",/g ;
  s/("b[0-9]+_.*"),\n"x9_endsponsor.*/$1\}\n],/g ;

  ' \
> "$1/allfiles.json"



# file to hold the eudract numbers
LC_CTYPE=C && LANG=C && < "$1/allfiles.json" perl -ne '

  while (<>) {

    # {\n"a2_eudract_number": "2006-001238-41",
    # print $_ if /"a2_eudract_number"/;

    #if (/"a2_eudract_number"/)
    if (/^"_id"/)
    {
    s/^.*([0-9]{4}-[0-9]{6}-[0-9]{2}-?[A-Z3]{0,3}).*$/$1/g;
    print $_;

    }

  } ' \
> "$1/alleudract.txt"
