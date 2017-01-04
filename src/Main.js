/* global exports */
"use strict";

var selection = require("d3-selection");

// module Main

var test = "hello";

var guid = function() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
    s4() + '-' + s4() + s4() + s4();
}
exports.rootSelect = function(selector) {
    return function() {
	console.log("Selecting the root");
	var s = selection.select(selector);
	s.guid = guid();
		console.log(s);
	return s;
    }
}


exports.rootSelectAll = function(selector) {

    return function() {
	    console.log("Selecting the root all");
	var s = selection.selectAll(selector);

	s.guid = guid();
		console.log(s);
	return s;
    }
}


exports.selectImpl = function(x,selector) {
    return function() {
	console.log("Selecting a child");
	var s = x.select(selector);

	s.guid = guid();
		console.log(s);
	return s;
    }
}

exports.selectAllImpl = function(x, selector) {
    return function() {
	console.log("Selecting all a child");
	var s = x.selectAll(selector);
	s.guid=guid();
	console.log(s);
	return s;
    }
}

exports.test = test;

exports.testFnImpl = function(a,b) {
    return a + b + "Hello";
}

exports.logAnythingImpl = function(x) {
    return function() {
	console.log(x);
    }
}

exports.attrImpl = function(x,name,value) {
    return function() {
	return x.attr(name,value);
    }
}

exports.appendImpl = function(x,type) {
    return function() {
	console.log("Appending to ");
	console.log(x);
	var s = x.append(type);
	s.guid = guid();
		console.log(s);
	return s;
    }
}

exports.dataImpl = function(x,data) {
    return function() {
	console.log("Binding data");
	console.log(data);
	console.log("to");
	console.log(x);
	var s = x.data(data);
	s.guid = guid();
		console.log(s);
	return s
    }
}

exports.enterImpl = function(x) {
    return function() {
	console.log("Entering the selection");
	console.log(x);
	var s = x.enter();
	s.guid = guid();
			console.log(s);
	return s;
    }
}

exports.textImpl = function(x,y) {
    return function() {
	var s = x.text(y.value0);
	console.log("Setting text to " + y + " for ");
	console.log(x);
	s.guid = guid();		console.log(s);
	return s;
    }
}
