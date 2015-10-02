<?php

// file: xml2json.php
// ralf.herold@gmx.net
// last edited: 2015-10-01

if ($argc <= 1) {
	print("Usage: php -n -f xml2json.php test1.xml\n");
	return;
} else {
	$testXmlFile = $argv[1];
}

file_exists($testXmlFile) or die('File does not exist: ' . $testXmlFile);

//$fileContents = simplexml_load_file($testXmlFile);
$fileContents = file_get_contents($testXmlFile);


//  <id_info><nct_id>NCT00097292</nct_id></id_info>

$testIdFile = preg_grep ('/.*(NCT.*).xml/', '$1', $testXmlFile);
$fileContents = str_replace('<required_header>', '<_id>' . $testIdFile . '</_id><required_header>', $fileContents);


$fileContents = str_replace(array("\n", "\r", "\t"), '', $fileContents);
$fileContents = trim(str_replace('"', "'", $fileContents));

$simpleXml = simplexml_load_string($fileContents);
// to flatten cdata elements:
//$simpleXml = simplexml_load_string($fileContents,'SimpleXMLElement',LIBXML_NOCDATA)

// Json & Array from XML in 3 lines:
//$xml = simplexml_load_string($xml_string);
//$json = json_encode($xml);
//$array = json_decode($json,TRUE);


echo json_encode($simpleXml);

?>
