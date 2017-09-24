/* global exports */
"use strict";

// module Data.URI.Common

exports.match1FromImpl = function (just) {
  return function (nothing) {
    return function (rx) {
      return function (str) {
        var result = rx.exec(str);
        return result && result.index === 0 ? just(result[0]) : nothing;
      };
    };
  };
};
