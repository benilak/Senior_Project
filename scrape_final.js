var url ='https://eresearch.fidelity.com/eresearch/markets_sectors/sectors/industries.jhtml?tab=investments&industry=255040&topic=etf';
var page = new WebPage()
var fs = require('fs');


page.open(url, function (status) {
  just_wait();
});

function just_wait() {
  setTimeout(function() {
    fs.write('1.html', page.content, 'w');
    phantom.exit();
  }, 2500);
}
