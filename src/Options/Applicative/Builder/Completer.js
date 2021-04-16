"use strict";
var execSync = require('child_process').execSync;

exports.execSyncCommand = function(command) {
  return function() {
    return execSync(command);
  };
};
