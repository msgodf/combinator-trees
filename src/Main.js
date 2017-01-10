/* global exports */
"use strict";

var selection = require("d3-selection");

// module Main

var guid = function() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
    s4() + '-' + s4() + s4() + s4();
}

exports.logAnythingImpl = function(x) {
    return function() {
	console.log(x);
    }
}
