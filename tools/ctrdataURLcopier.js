// ==UserScript==
//
// @name         ctrdataURLcopier
// @version      0.7
// @description  Copies to the clipboard the link to a user's search in trial registers (CTIS, EUCTR, CTGOV, ISRCTN) for use with R package ctrdata
// @author       ralf.herold@mailbox.org
//
// @namespace    https://cran.r-project.org/package=ctrdata
// @updateURL    https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js
// @downloadURL  https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js
// @supportURL   https://github.com/rfhb/ctrdata/issues
//
// @match        https://www.clinicaltrials.gov/*
// @match        https://clinicaltrials.gov/*
// @match        https://www.isrctn.com/search?*
// @match        https://www.isrctn.com/ISRCTN*
// @match        https://euclinicaltrials.eu/ctis-public/*
// @match        https://euclinicaltrials.eu/search-for-clinical-trials/*
// @match        https://www.clinicaltrialsregister.eu/ctr-search/search?query=*
// @match        https://www.clinicaltrialsregister.eu/ctr-search/trial/*
//
// @grant        unsafeWindow
// @grant        GM_setClipboard
// @grant        window.onurlchange
//
// ==/UserScript==

let dbg = 0;

if (dbg) console.log('[ctrdataURLcopier] checkpoint A');

function formatUrl(x) {

    x = x.replace(/#.*/, '');
    x = x.replace(/[&?]page.*?=[-,0-9]+/g, '');
    x = x.replace(/[&?]Search=Search/, '');
    x = x.replace(/[&?]draw=[0-9]+/, '');
    x = x.replace(/[&?]rank=[0-9]+/, '');
    x = x.replace(/[&?]tab=[a-z]+/, '');
    x = x.replace(/[&?]lang=[a-z]+/, '');
    x = x.replace(/[&?]totalResults=[0-9]+/g, '');
    x = x.replace(/[a-z][a-z_]+=&|[a-z_]+=$/ig, ''); // keep &q=&

    return (x);
}

const removeEmpty = obj => {

    Object.entries(obj).forEach(
        ([key, val]) => (val === null || val === '') && delete obj[key]
    );

    return JSON.stringify(obj);
};


// Register CTGOV
if (window.onurlchange === null) {

    if (dbg) console.log('[ctrdataURLcopier] checkpoint B1');

    window.addEventListener('urlchange', function () {

        var queryUrl = window.location.href;

        queryUrl = formatUrl(queryUrl);

        if (queryUrl.search(/search.+|study|ISRCTN|view/) > -1) {

            if (dbg) console.log('[ctrdataURLcopier] checkpoint C2');

            console.log('[ctrdataURLcopier] Copied to clipboard [1]:', queryUrl);
            GM_setClipboard(queryUrl);
        }
    });

};

// Registers except CTIS and CTGOV
if (window.location.href.search(/euclinicaltrials/) == -1) {

    if (dbg) console.log('[ctrdataURLcopier] checkpoint C1');

    var queryUrl = window.location.href;

    if (queryUrl.search(/search|study|ISRCTN/) > -1) {

        if (dbg) console.log('[ctrdataURLcopier] checkpoint C2');

        queryUrl = formatUrl(queryUrl);

        console.log('[ctrdataURLcopier] Copied to clipboard [2]:', queryUrl);
        GM_setClipboard(queryUrl);

    };

} else {

    if (dbg) console.log('[ctrdataURLcopier] checkpoint D1');

    // Register CTIS - example URLs created by this script:
    // https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"","containAny":"lymphoma","containNot":""}

    let queryQuery = window.location.href;
    if (dbg) console.log('[ctrdataURLcopier] window.location.href, queryQuery:', queryQuery);

    let queryUrl = window.location.protocol + '//' + window.location.hostname + window.location.pathname;
    if (dbg) console.log('[ctrdataURLcopier] query without parameters, queryUrl:', queryUrl);

    let searchFromUrl = queryQuery.replace(/.+\/search#?(.*)$/, '$1')
    searchFromUrl = decodeURI(searchFromUrl);
    searchFromUrl = searchFromUrl.replace(/searchCriteria=/g, '');
    if (dbg) console.log('[ctrdataURLcopier] content of searchCriteria to go into POST body, searchFromUrl:', searchFromUrl);

    let searchFromUrlJSON = searchFromUrl ? JSON.parse(searchFromUrl) : {};
    if (dbg) console.log('[ctrdataURLcopier] JSON of searchCriteria to go into POST body, searchFromUrlJSON:', searchFromUrlJSON);

    let origSend = unsafeWindow.XMLHttpRequest.prototype.send;
    unsafeWindow.XMLHttpRequest.prototype.send = function (body) {

        if (dbg) console.log('[ctrdataURLcopier] checkpoint D2');

        let postBodysearchCriteria = body;
        postBodysearchCriteria = body ? JSON.parse(body) : {};
        postBodysearchCriteria = postBodysearchCriteria.searchCriteria || {};

        let filteredSearchCriteria = removeEmpty(postBodysearchCriteria);
        if (dbg) console.log('[ctrdataURLcopier] POST body search criteria, filteredSearchCriteria:', filteredSearchCriteria);


        // handling a POST request coming from search within CTIS
        if (filteredSearchCriteria != '{}' & (filteredSearchCriteria != searchFromUrlJSON)) {

            if (dbg) console.log('[ctrdataURLcopier] checkpoint D3 - forward POST from within CTIS');

            queryUrl = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria=' + filteredSearchCriteria;

            window.history.pushState({}, "", decodeURI(queryUrl));
            // ^^^ does not work from iframe in https://euclinicaltrials.eu/search-for-clinical-trials/
            GM_setClipboard(queryUrl);
            console.log("[ctrdataURLcopier] Copied to clipboard [3]:", queryUrl);

            return origSend.apply(this, arguments);

        } else {

            // handling when CTIS is called from an URL
            if (searchFromUrl != '' & filteredSearchCriteria == '{}' ) {

                if (dbg) console.log('[ctrdataURLcopier] checkpoint D4 - dispatch POST from URL');

                alert("Click on 'Search results' to see\nthe studies found with the query");

                arguments[0] = "{\"pagination\":{\"page\":1,\"size\":20},\"sort\":{\"property\":\"decisionDate\",\"direction\":\"DESC\"}," +
                    "\"searchCriteria\":" + JSON.stringify(searchFromUrlJSON) + "}";

                if (dbg) console.log('[ctrdataURLcopier] arguments for POST call:', arguments);

                return origSend.apply(this, arguments);

            } else {

                if (dbg) console.log('[ctrdataURLcopier] checkpoint D5 - dispatch without processing');

                if (filteredSearchCriteria == '{}') {

                    queryUrl = 'https://euclinicaltrials.eu/ctis-public/search#searchCriteria=' + filteredSearchCriteria;

                    console.log("[ctrdataURLcopier] Copied to clipboard [4]:", queryUrl);

                }

                // dispatch
                return origSend.apply(this, arguments);

            }
        }

    };

}
