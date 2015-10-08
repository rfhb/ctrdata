<?php

// file: xml2json.php
// ralf.herold@gmx.net
// last edited: 2015-10-02

if ($argc <= 1) {
	die("Usage: php -n -f xml2json.php <directory_path_with_xml_files>\n");
} else {
	$testXmlFile = $argv[1];
}

file_exists($testXmlFile) or die('Directory or file does not exist: ' . $testXmlFile);

// determine outfile and make sure it does not exist
$outFileName = $testXmlFile . '/allfiles.json';
file_exists($outFileName) and unlink($outFileName);

foreach (glob("$testXmlFile/NCT*.xml") as $inFileName) {

  $fileContents = file_get_contents($inFileName);

  // normalise contents and remove whitespace
  $fileContents = str_replace(array("\n", "\r", "\t"), '', $fileContents);
  $fileContents = preg_replace('/ +/', ' ', $fileContents);
  $fileContents = trim(str_replace('"', "'", $fileContents));

  // write NCT number into _id for respective study
  // may break and approach can be improved
  //<id_info><nct_id>NCT00097292</nct_id></id_info>
  $fileContents = str_replace('<required_header>', '<_id>' . basename($inFileName, '.xml') . '</_id><required_header>', $fileContents);

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
