exports._Checkbox = require("@material-ui/core/Checkbox").default;
exports._eqColorProp = function(left){ return function(right){ return left === right }};
exports._ordColorProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};