function euctr2ndjson(txt, dt) {

    // delete non-informative lines e.g. explanatory information from key F.3.3.1
    txt = txt.replaceAll(/^Summary$|^This file contains.*$|^A. Protocol Info.*$|^F. Population of Trial Sub.*$|^P. End of Trial$|^[(]For clinical trials.*$|^did not include the words.*$|^or not they would be using contraception.*$|^database on [0-9][0-9][0-9][0-9].*$/gm, "");

    // sponsor records were added but left empty -> create placeholder
    txt = txt.replaceAll(/^(B.1.1 Name of Sponsor:)\\s+$/gm, "$1 empty");

    // prepare arrays
    txt = txt.replaceAll(/^MedDRA Classification$/gm, "E.1.2 MedDRA Classification: Yes");
    txt = txt.replaceAll(/^B.4 Source[(]s[)] of Monetary or Material Support.*$/gm, "B.4 Sources of Monetary or Material Support: Yes");
    txt = txt.replaceAll(/^G. Investigator Networks.*$/gm, "G.4 Investigator Networks: Yes");
    txt = txt.replaceAll(/^D.3.8 to D.3.10 IMP Identification Details.*$/gm, "D.3.8 IMP Identification details: Yes");
    txt = txt.replaceAll(/^(D.8 Placebo: 1)$/gm, "D.8 Information on Placebo: Yes\n$1"); // modified

    // add identifiers for special cases
    txt = txt.replaceAll(/^(EudraCT Number.*)$/gm, "X.1 $1");
    txt = txt.replaceAll(/^(Sponsor) ([0-9]+)$/gm, "B.1 $1: $2");
    txt = txt.replaceAll(/^(Clinical Trial.*)$/gm, "X.4 $1");
    txt = txt.replaceAll(/^(Trial Status.*)$/gm, "X.5 $1");
    txt = txt.replaceAll(/^(Date on.*)$/gm, "X.6 $1");
    txt = txt.replaceAll(/^(Link.*)$/gm, "X.7 $1");

    // add identifier for end of arrays
    txt = txt.replaceAll(/^D. IMP Identification$/gm, "X.9 ENDSPONSOR: TRUE");

    // 3rd country trials vs other trials
    txt = txt.replaceAll(/^E.8.3.+single site.+:/gm, "E.8.3.Single site trial:");
    txt = txt.replaceAll(/^E.8.4 Will this trial be conducted at multiple sites globally.+:/gm, "E.8.4.0 Multiple sites globally:");
    txt = txt.replaceAll(/^E.8.4 The trial involves multiple sites in the Member State.+:/gm, "E.8.4 Multiple sites in Member State:");
    txt = txt.replaceAll(/^E.8.6.3.+trial sites are planned:/gm, "E.8.6.3 Trial sites planned in:");

    // add array end indicators
    txt = txt.replaceAll(/^(D.8 Information on Placebo.*)$/gm, "$1\nX.9 ENDDMP: TRUE");
    txt = txt.replaceAll(/^(E.1.3 Condition.*)/gm, "$1\nX.9 ENDMEDDRA: TRUE");
    txt = txt.replaceAll(/^(B.5 Contact.*)/gm, "$1\nX.9 ENDSUPPORT: TRUE");
    txt = txt.replaceAll(/^(N. Review.*|^H.4 Third Country.*)/gm, "$1\nX.9 ENDNETWORK: TRUE");

    // so far 4 ms

    // sanitise
    txt = txt.replaceAll(/\t/gm, " ");
    txt = txt.replaceAll(/\r/gm, "\n");
    txt = txt.replaceAll(/\n\n+/gm, "\n");
    txt = txt.replaceAll(/\"|\{|\}/gm, "");

    // handle single case where colon is in key
    txt = txt.replaceAll(/^(.+)Opinion: Reason(.+)$/gm, "$1Opinion Reason$2");

    // create id per record from eudract number followed by country 2 character id or by "3rd" for non - EU countries
    txt = txt.replaceAll(/^X.7 Link.*trial\/([0-9-]*)\/([A-Z][A-Z]|3rd)\/$/gm,
        function(match, p1, p2, offset, string, groups) {
          return 'xxxxxxxxxx"_id": "' + p1 + '-' + p2.toUpperCase() + '"'});

    // remove newlines within variable fields
    txt = txt.replaceAll(/^([ABDEFGHNPX][.][1-9 I].+)$/gm, "\nxxxxxxxxxx$1");
    txt = txt.replaceAll(/\n/gm, " ");
    txt = txt.replaceAll(/xxxxxxxxxx/gm, "\n");

    // bring back newlines around identifier for end of arrays
    // txt = txt.replaceAll(/X.9 ENDSPONSOR: TRUE/gm, "\nX.9 ENDSPONSOR: TRUE"); // not needed?!

    // clean up
    txt = txt.replaceAll(/\n\n+/gm, "\n/");

    // add information
    txt = txt.replaceAll(/^("_id":.+")/gm, "$1\nrecord last import: " + dt);
    txt = txt.replaceAll(/^("_id":.+")/gm, "$1\nctrname: EUCTR");

    // so far 6 ms

    // key value formatting
    function keyValueFormatter(match, p1, p2, offset, string, groups) {
        p1 = p1.toLowerCase().replaceAll(/ /g, "_")
        p1 = p1.replaceAll(/[^a-z0-9_]/g, "")
        p2 = p2.replaceAll(/[^a-zA-Z0-9+-:\/_@ ]*/g, "")
        p2 = p2.replaceAll(/^\s+|\s+$/g, "")
        return '"' + p1 + '": "' + p2 + '",';
    }
    txt = txt.replaceAll(/^(.+?): (.*)$/gm, keyValueFormatter);

    // so far 6 ms

    // sed commands
    txt = txt.replaceAll(/^"x1_eudract_number.*$/gm, "}{"); // modified
    txt = txt.replaceAll(/^"dimp": "1",$/gm, '"dimp": [ { "_dimp": "1",');
    txt = txt.replaceAll(/^"b1_sponsor": "1",$/gm, '"b1_sponsor": [ { "_b1_sponsor": "1",');
    txt = txt.replaceAll(/^"e12_meddra_classification": "Yes",$/gm, '"e12_meddra_classification": [ {');
    txt = txt.replaceAll(/^"b4_sources_of_monetary_or_material_support": "Yes",$/gm, '"b4_sources_of_monetary_or_material_support": [ {');
    txt = txt.replaceAll(/^"g4_investigator_networks": "Yes",$/gm, '"g4_investigator_networks": [');
    txt = txt.replaceAll(/^"d38_imp_identification_details": "Yes",$/gm, '"d38_imp_identification_details": [ {');
    txt = txt.replaceAll(/^"d8_information_on_placebo": "Yes",$/gm, '"d8_information_on_placebo": [');

    // so far 6 ms

    // -e '/^["{}]/!d'
    txt = txt.replaceAll(/^[^"{}].*$/gm, "");

    // -e '/""/d'
    // txt = txt.replaceAll(/""/gm, ""); // unclear why to remove empty values

    // -e '1 s/}{/{/' \
    txt = txt.replace(/^\s*}{/, "{"); // modified, first line

    // -e '$ s/\(.*\),/\1}/' \
    txt = txt.replace(/,\s*$/, "}"); // modified

    // delete comma from last line in record
    txt = txt.replaceAll(/,\n}{/gm, "} \nNEWRECORDIDENTIFIER\n{");

    // so far 7 ms

    // create array with imp(s)
    txt = txt.replaceAll(/("d[0-9]+_.*"),\n+"dimp": "([2-9]|[1-9][0-9])",/gm, '$1}, \n{ "_dimp": "$2",'); // modified
    txt = txt.replaceAll(/("d[0-9]+_.*"),\n+"x9_enddmp.*/gm, '$1}\n],'); // modified
    // if no array, remove remnant
    txt = txt.replaceAll(/"x9_enddmp": "TRUE",/gm, "");

    // create array with sponsor(s)
    txt = txt.replaceAll(/,\n"b1_sponsor": "([2-9])",/gm, '}, \n{ "_b1_sponsor": "$1",');
    txt = txt.replaceAll(/("b[0-9]+_.*"),\n+"x9_endsponsor.*/gm, '$1}\n],'); // modified

    // create array of investigator network from first element
    txt = txt.replaceAll(/("g4_investigator_network_to_be_involved_in_the_trial": ".*?"),/gm, '},{$1,');
    txt = txt.replaceAll(/"x9_endnetwork": "TRUE"/gm, '} ]');

    // create array of meddra terms
    txt = txt.replaceAll(/("e12_version": ".*?"),/gm, '},{$1,');
    txt = txt.replaceAll(/"x9_endmeddra": "TRUE"/gm, '} ]');

    // create array of b4_source_of_monetary_or_material_support terms
    txt = txt.replaceAll(/("b41_name_of_organisation_providing_support": ".*?"),/gm, '},{$1,');
    txt = txt.replaceAll(/\n+"x9_endsupport": "TRUE"/gm, '} ]'); // modified
    // if any sponsor element
    txt = txt.replaceAll(/("b[0-9]+_.*?),\n+"x9_endsponsor": "TRUE"/gm, '$1 } ]'); // modified
    // otherwise remove
    txt = txt.replaceAll(/"x9_endsponsor": "TRUE",\n/gm, '');

    // create array of imp identification details
    txt = txt.replaceAll(/("d38_inn__proposed_inn": ".*?"|"d393_other_descriptive_name": ".*?"),/gm, '},{$1,');

    // close array with identification details
    txt = txt.replaceAll(/("d38|"d39|"d310)(.*?),\n+("d311)/gm, '$1$2} ],\n$3'); // modified

    // details may be missing in some records,
    txt = txt.replaceAll(/"d3[189][^,]+": "[^"]+",\n+"x9_endimpident": "TRUE"/gm, '} ]'); // modified
    // thus delete end identifier if there was no array
    txt = txt.replaceAll(/"x9_endimpident": "TRUE",/gm, '');

    // create array of placebos
    txt = txt.replaceAll(/("d8_placebo": ".*?"),/gm, '},{$1,');
    txt = txt.replaceAll(/(d8.*?\n+)("e11|"e12)/gm, '$1} ],\n$2'); // modified

    // correct formatting artefacts
    txt = txt.replaceAll(/{\n*},/gm, ''); // modified
    txt = txt.replaceAll(/\[\n*},/gm, '['); // modified
    txt = txt.replaceAll(/,\n*}/gm, '}'); // modified
    // empty array
    txt = txt.replaceAll(/\[\n*} ?\]/gm, '[]'); // modified

    // create ndjson
    txt = txt.replaceAll(/\n/gm, '');
    txt = txt.replaceAll(/NEWRECORDIDENTIFIER/gm, '\n');

    // so far 5 to 7 ms

    return txt + "\n";

}
