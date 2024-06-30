// ==UserScript==
//
// @name         ctrdataURLcopier
// @version      0.6
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

    return (x)
}

// Register CTGOV
if (window.onurlchange === null) {

    window.addEventListener('urlchange', function () {

        var queryUrl = window.location.href;

        queryUrl = formatUrl(queryUrl);

        if (queryUrl.search(/search.+|study|ISRCTN/) > -1) {

            console.log('[ctrdataURLcopier] Copied to clipboard [1]:', queryUrl);
            GM_setClipboard(queryUrl);
        }
    });

};

// Registers except CTIS and CTGOV
if (window.location.href.indexOf("euclinicaltrials") == -1) {

    var queryUrl = window.location.href;

    if (queryUrl.search(/search|study|ISRCTN/) > -1) {

        queryUrl = formatUrl(queryUrl);

        console.log('[ctrdataURLcopier] Copied to clipboard [2]:', queryUrl);
        GM_setClipboard(queryUrl);

    };

} else {

    // Register CTIS - example URLs created by this script:
    // https://euclinicaltrials.eu/ctis-public/search#searchCriteria={"containAll":"","containAny":"lymphoma","containNot":""}

    let origSend = unsafeWindow.XMLHttpRequest.prototype.send;
    unsafeWindow.XMLHttpRequest.prototype.send = function (body) {

        let queryQuery = window.location.href;
        let queryUrl = window.location.protocol + '//' + window.location.hostname + window.location.pathname;

        let postBody = body ? JSON.parse(body) : {};
        let searchCriteria = postBody.searchCriteria || {};
        let postBodyString = JSON.stringify(searchCriteria, null, 0);
        let filteredSearchCriteria = Object.entries(searchCriteria)
            .filter(([key, value]) => value !== '')
            .reduce((obj, [key, value]) => ({ ...obj, [key]: value }), {});

        // handling a POST request coming from search within CTIS
        if (JSON.stringify(filteredSearchCriteria, null, 0) != '{}') {

            postBodyString = 'searchCriteria=' + postBodyString;
            queryUrl = queryUrl + '#' + postBodyString;

            console.log('[ctrdataURLcopier] searchCriteria from POST body:', postBodyString);
            window.history.pushState({}, "", queryUrl);
            // ^^^ does not work from iframe in https://euclinicaltrials.eu/search-for-clinical-trials/
            GM_setClipboard(queryUrl);
            console.log('[ctrdataURLcopier] Copied to clipboard [3]:', queryUrl);

            return origSend.apply(this, arguments);
        }

        // handling when CTIS is newly opened with anchor
        if (queryQuery.indexOf("#") != -1) {

            postBodyString = queryQuery.replace(/^.+#(.+)$/, '$1')
            postBodyString = postBodyString.replace(/=/g, '":')
            postBodyString = decodeURI(postBodyString);

            console.log('[ctrdataURLcopier] string after # for POST body:', postBodyString);
            arguments[0] = "{\"pagination\":{\"page\":1,\"size\":20},\"sort\":{\"property\":\"decisionDate\",\"direction\":\"DESC\"}," +
                "\"" + postBodyString + "}";

            alert("Click on 'Search results' to see\nthe studies found with the query");
            window.history.pushState({}, "", decodeURI(queryQuery));
            return origSend.apply(this, arguments);
        }

        // handling when CTIS is newly opened without an anchor
        return origSend.apply(this, arguments);
    };

}
