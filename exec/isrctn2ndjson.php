<?php

// file: isrctn2json.php
// ralf.herold@gmx.net
// part of https://github.com/rfhb/ctrdata
// last edited: 2021-04-24
// used for: isrctn
// 2021-12-11: total 0.7 s for 540 trials ~ 1.3 ms per trial

// note line endings are to be kept by using in
// .gitattributes for compatibility with cygwin:
// *.sh  text eol=lf
// *.php text eol=lf

if ($argc <= 1) {
	die("Usage: php -n -f isrctn2json.php <directory_path_with_xml_files>\n");
} else {
	$testXmlFile = $argv[1];
}

// check infile
$inFileName = "$testXmlFile/isrctn.xml";
file_exists($inFileName) or die('Directory does not exist: ' . $inFileName);
$outFileName = "$testXmlFile/isrctn_out.xml";

// get UTC date, time in format correspondsing to the
// R default for format methods: "%Y-%m-%d %H:%M:%S"
$dt = gmdate("Y-m-d H:i:s");

// get contents
$fileContents = file_get_contents($inFileName);

// normalise contents
$fileContents = str_replace(array("\n", "\r", "\t"), ' ', $fileContents);
$fileContents = trim(str_replace("'", "&apos;", $fileContents));
$fileContents = trim(str_replace("&", "&amp;", $fileContents));

// remove white space
$fileContents = preg_replace('/  +/', ' ', $fileContents);

// use single quotes for xml
$fileContents = trim(str_replace('"', "'", $fileContents));

// write date of last import into additional field
$fileContents = str_replace('<trialDescription>',
                            '<record_last_import>' . $dt . '</record_last_import>' .
                            '<ctrname>ISRCTN</ctrname>' .
                            '<trialDescription>', $fileContents);

// copy ISRCTN number into _id for respective study
$fileContents = preg_replace('/>([0-9]{8}?)<\/isrctn>/',
                             '>$1</isrctn><_id>$1</_id>',
                             $fileContents, -1, $counter);

// write out
file_put_contents($outFileName, $fileContents);

// load
$simpleXml = simplexml_load_file($outFileName, 'SimpleXMLElement', LIBXML_COMPACT | LIBXML_NOBLANKS | LIBXML_NOENT);

// split xml, convert to json, save
$i = 0;
foreach ($simpleXml->children('http://www.67bricks.com/isrctn')->fullTrial as $trial) {
  $i = $i + 1;
  file_put_contents($testXmlFile . "/isrctn_trial.ndjson", json_encode($trial->trial) . "\n", FILE_APPEND | LOCK_EX);
}

// return value for import
echo $i;

?>
