<?php

// file: xml2json.php
// ralf.herold@gmx.net
// part of https://github.com/rfhb/ctrdata
// last edited: 2016-04-20
// time xml2json.php:
// 2016-04-20: 0.05 s for 2 trials ~ 25 ms per trial

if ($argc <= 1) {
	die("Usage: php -n -f xml2json.php <directory_path_with_xml_files>\n");
} else {
	$testXmlFile = $argv[1];
}

file_exists($testXmlFile) or die('Directory or file does not exist: ' . $testXmlFile);

// determine outfile and make sure it does not exist
$outFileName = $testXmlFile . '/allfiles.json';
file_exists($outFileName) and unlink($outFileName);

// get UTC date, time in format correspondsing to the
// R default for format methods: "%Y-%m-%d %H:%M:%S"
$dt = gmdate("Y-m-d H:i:s");

foreach (glob("$testXmlFile/NCT*.xml") as $inFileName) {

  $fileContents = file_get_contents($inFileName);

  // normalise contents and remove whitespace
  $fileContents = str_replace(array("\n", "\r", "\t"), '', $fileContents);
  $fileContents = preg_replace('/ +/', ' ', $fileContents);
  $fileContents = trim(str_replace('"', "'", $fileContents));

  // First write NCT number into _id for respective study:
  //    <id_info><nct_id>NCT00097292</nct_id></id_info>
  // Second write date of last import into additional field:
  //    <record_last_import>2016-06-22 08:35:32</record_last_import>
  // Third write clinical trial register name into additional field:
  //    <ctrname>CTGOV</ctrname>
  $fileContents = str_replace('<required_header>',
                              '<_id>' . basename($inFileName, '.xml') . '</_id>' .
                              '<record_last_import>' . $dt . '</record_last_import>' .
                              '<ctrname>CTGOV</ctrname>' .
                              '<required_header>', $fileContents);

  // turn repeat elements
  $simpleXml = simplexml_load_string($fileContents, 'SimpleXMLElement', LIBXML_COMPACT | LIBXML_NOBLANKS | LIBXML_NOENT);

  // to also flatten cdata elements:
  //$simpleXml = simplexml_load_string($fileContents,'SimpleXMLElement',LIBXML_NOCDATA)

  // Json & Array from XML:
  //$json = json_encode($simpleXml);
  //$simpleXml = json_decode($json, TRUE);

  // save all in one concatenated file
  file_put_contents($outFileName, json_encode($simpleXml), FILE_APPEND | LOCK_EX);

}

?>
