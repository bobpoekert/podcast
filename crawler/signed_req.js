function _setSignedRequestQueryParams(e){
    var t;
    var timestamp = Math.round((new Date).getTime()/1e3);
    var whitelist =its.serverData.properties["SF6.StorePlatform.whitelistParams"]; //["caller","dsid","id","p"]
    var headers = {}; // not used???
    for(var s=0; s<whitelist.length; s++) {
        // populate missing keys with null values
        headers[whitelist[s]]=!0;
    }
    var querystring = "";
    if(/[?]/.test(e)) {
        var kv_pairs=e.split("?")[1].split("&").sort();
        for(var s=0; s<kv_pairs.length; s++) {
            var kv_pair = kv_pairs[s].split("=");
            if (kv_pair.length==2 && whitelist[kv_pair[0]]) {
                querystring += kv_pair[1];
            }
        }
    }
    var f=[timestamp,iTunes.storefront,decodeURIComponent(querystring)].join("");
    return t=encodeURIComponent(iTunes.signStorePlatformRequestData(f)),e+(e.indexOf("?")>=0?"&":"?")+"X-JS-SP-TOKEN="+t+"&X-JS-TIMESTAMP="+n
}

// id, p, caller
