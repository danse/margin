#!/usr/bin/env node
'use strict';

var fs = require('fs'),
    pd = require('pretty-data').pd,
    fileName = 'margin-data.json';

function write(json) {
    var s = pd.json(json);
    var b = fs.openSync(fileName, 'w+');
    fs.writeSync(b, s);
}

function read() {
    var s = fs.readFileSync(fileName)
    return JSON.parse(s.toString())
}

function convertSingle(datum) {
  datum.t = new Date(datum.t).toJSON();
  return datum;
}

function convert(data) {
  return data.map(convertSingle);
}

write(convert(read(fileName)));