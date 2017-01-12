/* global exports */

"use strict";

var d3hierarchy = require("d3-hierarchy");

// module Tree

exports.treeImpl = function() {
    return function() {
	return d3hierarchy.tree();
    }
}

exports.runTreeImpl = function(tree,hierarchy) {
    return function() {
	return tree(hierarchy);
    }
}

exports.nodesImpl = function(tree) {
    return function() {
	return tree.nodes();
    }
}

exports.sizeImpl = function(tree) {
    return function() {
	return tree.size();
    }
}

exports.hierarchyImpl = function(data) {
    return function() {
	return d3hierarchy.hierarchy(data);
    }
}

exports.hierarchyChildrenImpl = function(data,childrenFn) {
    return function() {
	return d3hierarchy.hierarchy(data, childrenFn);
    }
}

exports.descendantsImpl = function(rootHierarchy) {
    return function() {
	return rootHierarchy.descendants();
    }
}
