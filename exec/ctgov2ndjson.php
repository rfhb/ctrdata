<?php

// file: ctgov2json.php
// ralf.herold@gmx.net
// part of https://github.com/rfhb/ctrdata
// last edited: 2021-03-20
// used for: ctgov
// time ctgov2json.php:
// 2016-04-20: 0.05s for 2 trials ~ 25 ms per trial
// 2021-04-18: 1.5s for 200 trials ~ 7.5 ms per trial
// 2021-05-07: total 0.7s for 246 trials ~ 3 ms per trial
// 2021-12-11: total 7.1s for 7125 trials ~ 1 ms per trial

// note line endings are to be kept by using in
// .gitattributes for compatibility with cygwin:
// *.sh  text eol=lf
// *.php text eol=lf

if ($argc <= 1) {
  die("Usage: php -n -f ctgov2json.php <directory_path_with_xml_files>\n");
} else {
  $testXmlFile = $argv[1];
}

file_exists($testXmlFile) or die('Directory does not exist: ' . $testXmlFile);

// get UTC date, time in format corresponding to the
// R default for format methods: "%Y-%m-%d %H:%M:%S"
$dt = gmdate("Y-m-d H:i:s");

// chunk and trial numbers
$cn = 0;
$tn = 0;

// chunk size set to 25 to avoid stack overrun issues and to mirror euctr
foreach (array_chunk(glob("$testXmlFile/NCT*.xml"), 25) as $chunkFileNames) {

  $cn = $cn + 1;

  foreach ($chunkFileNames as $inFileName) {

    // get contents
    $fileContents = file_get_contents($inFileName);

    // normalise contents
    $fileContents = str_replace(array("\n", "\r", "\t"), '', $fileContents);

    // https://stackoverflow.com/questions/44765194/how-to-parse-invalid-bad-not-well-formed-xml
    $fileContents = preg_replace('/[^\x{0009}\x{000a}\x{000d}\x{0020}-\x{D7FF}\x{E000}-\x{FFFD}]+/u', ' ', $fileContents);

    // escapes
    $fileContents = trim(str_replace("@", "", $fileContents));
    $fileContents = trim(str_replace("'", "&apos;", $fileContents));
    $fileContents = trim(str_replace("&", "&amp;", $fileContents));

    // remove white space
    $fileContents = preg_replace('/  +/', ' ', $fileContents);

    // use single quotes for xml
    $fileContents = trim(str_replace('"', "'", $fileContents));

    // write NCT number into _id and date of last import into additional fields
    $fileContents = str_replace(
      '<required_header>',
      '<_id>' . basename($inFileName, '.xml') . '</_id>' .
        '<record_last_import>' . $dt . '</record_last_import>' .
        '<ctrname>CTGOV</ctrname>' .
        '<required_header>',
      $fileContents
    );

    // turn repeat elements
    $simpleXml = simplexml_load_string($fileContents, 'SimpleXMLElement', LIBXML_COMPACT | LIBXML_NOBLANKS | LIBXML_NOENT);

    // save all in concatenated file per chunk
    file_put_contents("$testXmlFile/NCT_" . $cn . ".ndjson", json_encode($simpleXml) . "\n", FILE_APPEND | LOCK_EX);

    // increment trial counter
    $tn = $tn + 1;
  }
}

print $tn;
