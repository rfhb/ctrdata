// ==UserScript==
//
// @name         ctrdataURLcopier
// @version      0.4
// @description  Copies to the clipboard the link to a user's search in trial registers (CTIS, EUCTR, CTGOV, ISRCTN) for use with R package ctrdata
// @author       ralf.herold@mailbox.org
//
// @namespace    https://cran.r-project.org/package=ctrdata
// @updateURL    https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js
// @downloadURL  https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js
// @supportURL   https://github.com/rfhb/ctrdata/issues
//
// @match        https://classic.clinicaltrials.gov/ct2/results?*
// @match        https://classic.clinicaltrials.gov/ct2/show/*
// @match        https://www.clinicaltrials.gov/search?*
// @match        https://www.clinicaltrials.gov/study/*
// @match        https://clinicaltrials.gov/search?*
// @match        https://clinicaltrials.gov/study/*
// @match        https://www.isrctn.com/search?*
// @match        https://www.isrctn.com/ISRCTN*?q=*
// @match        https://euclinicaltrials.eu/app/*
//
// @grant        unsafeWindow
// @grant        GM_setClipboard
//
// ==/UserScript==
if (window.location.href.indexOf("euclinicaltrials") == -1) {

    var queryUrl = window.location.href;
    queryUrl = queryUrl.replace(/&page.*?=[-,0-9]+/g, '');
    queryUrl = queryUrl.replace(/&draw=[0-9]+/, '');
    queryUrl = queryUrl.replace(/&rank=[0-9]+/, '');
    queryUrl = queryUrl.replace(/&totalResults=[0-9]+/g, '');
    queryUrl = queryUrl.replace(/[a-z][a-z_]+=&|[a-z_]+=$/ig, ''); // keep &q=&

    console.log(queryUrl);
    GM_setClipboard(queryUrl);

} else {

    let origOpen = unsafeWindow.XMLHttpRequest.prototype.open;

    unsafeWindow.XMLHttpRequest.prototype.open = function (method, url) {

        var queryUrl = url;
        console.log(queryUrl);

        queryUrl = queryUrl.replace(/&?paging=[-,0-9]+/, '');
        queryUrl = queryUrl.replace(/&?sorting=[-a-zA-Z]+/, '');
        queryUrl = queryUrl.replace(/&?isEeaOnly=false/, '');
        queryUrl = queryUrl.replace(/&?isNonEeaOnly=false/, '');
        queryUrl = queryUrl.replace(/&?isBothEeaNonEea=false/, '');
        queryUrl = queryUrl.replace(/[?]$/, '');
        queryUrl = queryUrl.replace(
            'https://euclinicaltrials.eu/ct-public-api-services/services/ct/publiclookup',
            'https://euclinicaltrials.eu/app/#/search');
        queryUrl = queryUrl.replace(
            /https:\/\/euclinicaltrials.eu\/ct-public-api-services\/services\/ct\/([-0-9]+)\/publicview/,
            'https://euclinicaltrials.eu/app/#/view/$1');

        console.log('[ctrdataURLcopier] Mangled query: ', queryUrl);

        if (queryUrl.match('app/#/(search|view)')) {

            GM_setClipboard(queryUrl);
            console.log('[ctrdataURLcopier] Copied to clipboard: ', queryUrl);

            queryUrl = queryUrl.replace('https://euclinicaltrials.eu', '');
            console.log('[ctrdataURLcopier] Window history updated with: ', queryUrl);
            window.history.pushState({}, "", queryUrl);

        };

        return origOpen.apply(this, arguments);
    };

}

