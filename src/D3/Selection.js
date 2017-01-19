/* global exports */

"use strict";

// Module D3.Selection

var selection = require("d3-selection");

exports.rootSelectImpl = function(selector) {
    return function() {
	return selection.select(selector);
    }
}

exports.rootSelectAll = function(selector) {
    return function() {
	return selection.selectAll(selector);
    }
}


exports.selectImpl = function(x,selector) {
    return function() {
	return x.select(selector);
    }
}

exports.selectAllImpl = function(x, selector) {
    return function() {
	return x.selectAll(selector);
    }
}

exports.insertImpl = function(x,type,before) {
    return function() {
	return x.insert(type.value0, before);
    }
}

exports.attrImpl = function(x,name,value) {
    return function() {
	return x.attr(name,value.value0);
    }
}

exports.styleImpl = function(x,name,value) {
    return function() {
	return x.style(name,value.value0);
    }
}

exports.appendImpl = function(x,type) {
    return function() {
	return x.append(type);
    }
}

exports.dataImpl = function(x,data) {
    return function() {
	return x.data(data);
    }
}

exports.enterImpl = function(x) {
    return function() {
	return x.enter();
    }
}

exports.textImpl = function(x,y) {
    return function() {
	// Extract value from Value wrapper
	return x.text(y.value0);
    }
}

exports.mergeImpl = function(x,y) {
    return function() {
	x.merge(y);
    }
}
