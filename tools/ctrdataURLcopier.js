// ==UserScript==
//
// @name         ctrdataURLcopier
// @version      0.2
// @description  Copies to the clipboard the link to a user's search in trial registers (CTIS, EUCTR, CTGOV, ISRCTN) for use with R package ctrdata
// @author       ralf.herold@mailbox.org
//
// @namespace    https://cran.r-project.org/package=ctrdata
// @updateURL    https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js
// @downloadURL  https://raw.githubusercontent.com/rfhb/ctrdata/master/tools/ctrdataURLcopier.js
// @supportURL   https://github.com/rfhb/ctrdata/issues
//
// @match        https://euclinicaltrials.eu/search*
// @match        https://euclinicaltrials.eu/app*
// @match        https://clinicaltrials.gov/ct2/results?*
// @match        https://clinicaltrials.gov/ct2/show/*
// @match        https://beta.clinicaltrials.gov/search?*
// @match        https://www.clinicaltrialsregister.eu/ctr-search/search?*
// @match        https://www.clinicaltrialsregister.eu/ctr-search/trial/*
// @match        https://www.isrctn.com/search?*
// @match        https://www.isrctn.com/ISRCTN*?q=*
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

    unsafeWindow.XMLHttpRequest.prototype.open = function(method, url) {

        var queryUrl = url;
        queryUrl = queryUrl.replace(/&?paging=[-,0-9]+/, '');
        queryUrl = queryUrl.replace(/&?sorting=[-a-zA-Z]+/, '');
        queryUrl = queryUrl.replace(/&?isEeaOnly=false/, '');
        queryUrl = queryUrl.replace(/&?isNonEeaOnly=false/, '');
        queryUrl = queryUrl.replace(/&?isBothEeaNonEea=false/, '');

        console.log(queryUrl);
        GM_setClipboard(queryUrl);

        return origOpen.apply(this, arguments);
    };
}
