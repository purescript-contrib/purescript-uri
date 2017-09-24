/* global exports */
"use strict";

// module Data.URI.Common

exports.match1FromImpl = function (just) {
  return function (nothing) {
    return function (pattern) {
      return function (flags) {
        return function (i) {
          var rx = new RegExp(pattern, flags.indexOf("g") === -1 ? flags + "g" : flags);
          return function (str) {
            rx.lastIndex = i;
            var result = rx.exec(str);
            return result && result.index === i ? just(result[0]) : nothing;
          };
        };
      };
    };
  };
};
