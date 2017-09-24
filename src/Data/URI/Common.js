/* global exports */
"use strict";

// module Data.URI.Common

exports.match1FromImpl = function (just) {
  return function (nothing) {
    return function (rx) {
      return function (i) {
        return function (str) {
          rx.lastIndex = i;
          var result = rx.exec(str);
          return result && result.index === i ? just(result[0]) : nothing;
        };
      };
    };
  };
};
