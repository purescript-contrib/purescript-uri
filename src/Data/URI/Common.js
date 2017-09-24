/* global exports */
"use strict";

// module Data.URI.Common

exports.match1FromImpl = function (just) {
  return function (nothing) {
    return function (rx) {
      return function (i) {
        return function (str) {
          var result = rx.exec(str.slice(i));
          return result && result.index === 0 ? just(result[0]) : nothing;
        };
      };
    };
  };
};
