// scrape.js

// A generic webpage sraping script
// powered by PhantomJS

var system = require('system');
var args = system.args;
if (args.length < 3) {
  console.log("Usage: ./phantomjs scrape.js URL DEST_FILE");
  phantom.exit(1);
}

var url = args[1];
var path = args[2];

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');

try {
  page.open(url, function (status) {
    var content = page.content;
    fs.write(path,content,'w');
    phantom.exit();
  });
} catch(e) {
  console.log(e);
  phantom.exit(1);
}
