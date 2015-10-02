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
unlink($outFileName);

foreach (glob("$testXmlFile/NCT*.xml") as $inFileName) {

  $fileContents = file_get_contents($inFileName);

  // write NCT number into _id for respective study
  // may break and approach can be improved
  //<id_info><nct_id>NCT00097292</nct_id></id_info>
  $fileContents = str_replace('<required_header>', '<_id>' . basename($inFileName, '.xml') . '</_id><required_header>', $fileContents);

  $fileContents = str_replace(array("\n", "\r", "\t"), '', $fileContents);
  $fileContents = trim(str_replace('"', "'", $fileContents));

  $simpleXml = simplexml_load_string($fileContents);
  // to also flatten cdata elements:
  //$simpleXml = simplexml_load_string($fileContents,'SimpleXMLElement',LIBXML_NOCDATA)

  // Json & Array from XML in 3 lines:
  //$xml = simplexml_load_string($xml_string);
  //$json = json_encode($xml);
  //$array = json_decode($json,TRUE);

  // save all in one concatenated file
  file_put_contents($outFileName, json_encode($simpleXml), FILE_APPEND | LOCK_EX);

}

?>
