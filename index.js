"use strict";

var lambda = require('./output/Main')

exports.handler = function(data, context) {
  lambda.handler(context)(data)();
};
