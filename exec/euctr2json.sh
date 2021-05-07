#!/bin/sh

## ralf.herold@gmx.net - 2015-08-15
## part of https://github.com/rfhb/ctrdata
#
# note line endings are to be kept by using in
# .gitattributes for compatibility with cygwin:
# *.sh  text eol=lf
# *.php text eol=lf
#
# time euctr2json.sh:
# 2015-08-15: real 2.4 s for 221 documents: ~ 11 ms per trial (MacBookPro2011)
# 2016-04-20: real 1.2 s for 151 documents: ~  8 ms per trial (MacBookPro2011)
# 2016-09-11: real 1.2 s for 151 documents: ~  8 ms per trial (MacBookPro2015)
# 2017-01-12: real 1m23.021s for 10978 doc: ~  8 ms per trial (MacBookPro2015)
# 2019-08-10: real 3s    for 446 documents: ~  7 ms per trial (MacBookPro2015)
# 2021-04-18: 5.7 s for 503 records: ~ 11 ms per trial (MacBookPro2015)
# 2021-05-07: total 2.5 s for 366 records: ~ 7 ms per trial (MacBookPro2015)

# notes to myself: sed cannot use + or other
# alternatively install gnu-sed: > brew install gnu-sed
# perl: The -p argument makes sure the code gets executed on
# every line, and that the line gets printed out after that.

# transform to json for import in mongodb, reference:
# http://docs.mongodb.org/manual/reference/bios-example-collection/

for inFileName in "$1"/euctr_trials_*.txt; do
    [ -e "$inFileName" ] || continue

    outFileName=( `echo $inFileName | sed 's/txt$/json/'` )
    # echo $outFileName

LC_CTYPE=C && LANG=C && < "$inFileName" perl -ne '
  # this section is faster with perl compared to sed

  # get UTC date, time in format correspondsing to the
  # R default for format methods: "%Y-%m-%d %H:%M:%S"
  use POSIX;
  my $dt = POSIX::strftime("%Y-%m-%d %H:%M:%S", gmtime());

  while (<>) {

  # elimination of non-printing characters such as control M
  # \000-\011\013\014\016-\037
  # NUL  TAB  VT FF  SO   US
  tr/\015//d;

  # delete non-informative lines
  next if /^$/;
  next if /^Summary/;
  next if /^This file contains/;
  next if /A\. Protocol Information/;
  #next if /B\. Sponsor Information/;
  next if /F\. Population of Trial Subjects/;
  next if /P\. End of Trial$/;

  # remove explanatory information from key F.3.3.1
  next if /^\(For clinical trials recorded/;
  next if /^did not include the words /;
  next if /^or not they would be using contraception/;
  next if /^database on [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/;

  # workarounds
  # - sponsor records were added but left empty -> create placeholder
  s/^(B\.1\.1 Name of Sponsor:)\s+$/$1 empty/g;
  #  # - some third country records do not have a sponsor -> placeholder
  #  s/^(B\. Sponsor Information)[\n ]+(D.\ IMP)/B.1.1 Name of Sponsor: empty\n\n$2/g;

  # - prepare array for meddra
  s/MedDRA Classification/E.1.2 MedDRA Classification: Yes/g;

  # - prepare array for sponsor supports
  s/^B\.4 Source\(s\) of Monetary or Material Support.*$/B.4 Sources of Monetary or Material Support: Yes/g;

  # - prepare array for networks
  s/^G\. Investigator Networks.*$/G.4 Investigator Networks: Yes/g;

  # - prepare array for inn proposed names
  s/^D\.3\.8 to D\.3\.10 IMP Identification Details.*$/D.3.8 IMP Identification details: Yes/g;

  # - prepare array for placebos
  print "\nD.8 Information on Placebo: Yes" if/^D.8 Placebo: 1$/;

  # add identifiers for special cases
  s/^(EudraCT Number.*)$/X.1 $1/;
  s/^(Sponsor) ([0-9]+)$/B.1 $1: $2/;
  s/^(Clinical Trial.*)$/X.4 $1/;
  s/^(Trial Status.*)$/X.5 $1/;
  s/^(Date on.*)$/X.6 $1/;
  s/^(Link.*)$/X.7 $1/;

  # add identifier for end of arrays
  s/^D\. IMP Identification$/X.9 ENDSPONSOR: TRUE/;

  print "\nX.9 ENDDMP: TRUE"      if /^D\.8 Information on Placebo$/;
  print "\nX.9 ENDMEDDRA: TRUE"   if /^E\.1\.3 Condition/;
  print "\nX.9 ENDSUPPORT: TRUE"  if /^B\.5 Contact/;
  print "\nX.9 ENDNETWORK: TRUE"  if /^N\. Review|^H\.4 Third Country/;
  # print "\nX.9 ENDIMPIDENT: TRUE" if /^D\.3\.11 The IMP contains an/;

  # sanitise file
  s/\t/ /g;
  s/\r/\n/g;
  s/\n+/\n/g;
  s/\"|\{|\}//g;

  # handle single case where colon is in key
  s/^(.+)Opinion: Reason(.+)$/$1Opinion Reason$2/g;

  # create id per record from eudract number followed by
  # country 2 character id or by "3rd" for non-EU countries
  s/^X.7 Link.*search\/trial\/([0-9-]*)\/([A-Z][A-Z]|3rd)\/$/xxxxxxxxxx"_id": "$1-\U$2\E"/;

  # crude attack on newlines within variable fields
  s/^([ABDEFGHNPX][.][1-9 I].+)$/\nxxxxxxxxxx$1/g;
  s/\n/ /g;
  s/xxxxxxxxxx/\n/g;

  # bring back newlines around identifier for end of arrays
  s/X.9 ENDSPONSOR: TRUE/\nX.9 ENDSPONSOR: TRUE/;
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
      $tmp2 =~ s![^a-zA-Z0-9+-:\/_@ ]*!!g ;
      $tmp2 =~ s!^\s+|\s+$!!g ;
   "\"".$tmp1."\": \"".$tmp2."\",\n" /exgs;
   ' | \
sed \
  -e 's/\("x1_eudract_number.*$\)/}{/g' \
  -e 's/^"dimp": "1",$/"dimp": [ { "_dimp": "1",/g' \
  -e 's/^"b1_sponsor": "1",$/"b1_sponsor": [ { "_b1_sponsor": "1",/g' \
  -e 's/^"e12_meddra_classification": "Yes",$/"e12_meddra_classification": [ {/g' \
  -e 's/^"b4_sources_of_monetary_or_material_support": "Yes",$/"b4_sources_of_monetary_or_material_support": [ {/g' \
  -e 's/^"g4_investigator_networks": "Yes",$/"g4_investigator_networks": [/g' \
  -e 's/^"d38_imp_identification_details": "Yes",$/"d38_imp_identification_details": [ {/g' \
  -e 's/^"d8_information_on_placebo": "Yes",$/"d8_information_on_placebo": [/g' \
  -e '/^["{}]/!d' \
  -e '/""/d' \
  | \
sed \
  -e '1 s/}{/{/' \
  -e '$ s/\(.*\),/\1}/' \
  | \
perl -pe 'BEGIN{undef $/;}

  # here we can do multi-line edits

  # delete comma from last line in record
  s/,\n\}\{/\}\nNEWRECORDIDENTIFIER\n\{/g ;

  # create array with imp(s)
  s/("d[0-9]+_.*"),\n"dimp": "([2-9]|[1-9][0-9])",/$1\}, \n\{ "_dimp": "$2",/g ;
  s/("d[0-9]+_.*"),\n"x9_enddmp.*/$1\}\n],/g ;

  # create array with sponsor(s)
  s/,\n"b1_sponsor": "([2-9])",/}, \n\{ "_b1_sponsor": "$1",/g ;
  s/("b[0-9]+_.*"),\n"x9_endsponsor.*/$1\}\n],/g ;

  # create array of investigator network from first element
  s/("g4_investigator_network_to_be_involved_in_the_trial": ".*?"),/},{$1,/g ;
  s/"x9_endnetwork": "TRUE"/} ]/g ;

  # create array of meddra terms
  s/("e12_version": ".*?"),/},{$1,/g ;
  s/"x9_endmeddra": "TRUE"/} ]/g ;

  # create array of b4_source_of_monetary_or_material_support terms
  s/("b41_name_of_organisation_providing_support": ".*?"),/},{$1,/g ;
  s/\n"x9_endsupport": "TRUE"/} ]/g ;
  # if any sponsor element
  s/("b[0-9]+_.*?),\n"x9_endsponsor": "TRUE"/$1 } ]/g ;
  # otherwise remove
  s/"x9_endsponsor": "TRUE",\n//g ;

  # create array of imp identification details
  s/("d38_inn__proposed_inn": ".*?"|"d393_other_descriptive_name": ".*?"),/},{$1,/g ;

  # close array with identification details
  s/("d38|"d39|"d310)(.*?),\n("d311)/$1$2} ],\n$3/g ;

  # details may be missing in some records,
  s/"d3[189][^,]+": "[^\"]+",\n"x9_endimpident": "TRUE"/} ] /g ;
  # thus delete end identifier if there was no array
  s/"x9_endimpident": "TRUE",//g ;

  # create array of placebos
  s/("d8_placebo": ".*?"),/},{$1,/g ;
  s/(d8.*?\n)("e11|"e12)/$1} ],\n$2/g ;

  # correct formatting artefacts
  s/{\n?},//g ;
  s/\[\n?},/[/g ;
  s/,\n?}/}/g ;
  # empty array
  s/\[\n?} ?\]/[]/g ;

  ' | \
perl -pe '

  # write NDJSON
  s/\n//g ;
  s/NEWRECORDIDENTIFIER/\n/g ;

  # add a final EOL with sed
  ' | \
sed \
  -e '$a\' \
> "$outFileName"

done

## print total number of ndjson lines
sed -n '$=' "$1"/euctr_trials_*.json
