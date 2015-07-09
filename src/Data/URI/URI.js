/* global exports */
'use strict';

// module Data.URI.Common

exports._match1From =   function(just, nothing, rx, i, str) {  
  var rxStr = rx.toString(),
      flagIndex = rxStr.lastIndexOf("/"),
      pattern = rxStr.substring(1, flagIndex),
      flags = rxStr.substring(flagIndex + 1);

  rx = new RegExp(pattern, flags.indexOf("g") === -1 ? flags + "g" : flags);
  rx.lastIndex = i;

  var result = rx.exec(str);
  return result && result.index === i ? just(result[0]) : nothing;
};
