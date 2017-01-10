/* global exports */

"use strict";

var d3hierarchy = require("d3-hierarchy");

// module Tree

exports.treeImpl = function(root) {
    return function() {
	return d3hierarchy.tree(root);
    }
}

exports.nodesImpl = function(tree) {
    return function() {
	return tree.nodes();
    }
}
