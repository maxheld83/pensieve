// use strict mode to avoid problems, should apply only to this script
"use strict";

// run this on all window size change events
$(window).bind("load resize orientationchange", function() {
  resizer($(".ps-grid"));
});

function resizer(el) {
  // this approach is inspired by https://www.html5rocks.com/en/tutorials/casestudies/gopherwoord-studios-resizing-html5-games/#disqus_thread
  // to be safe, this SHOULD be recalculated every time, b/c el MIGHT have stuff which does not resize dynamically, therefore altering aspect ratio
  var reqAspRatio = findAspectRatio(el);
  // this should respect parent width
  var availWidth = el.parent().innerWidth();
  // but for height, we only care about viewport limitations; users will have to scroll accordingly
  var availHeight = $(window).innerHeight();
  var availAspRat = availWidth / availHeight;

  // remember that we ONLY write out width, rest is dealt with via css padding hack
  if (availAspRat > reqAspRatio) {
    // this is e.g. 4/3 in 16/9, needs boxes on the side, aka width based on height
    el.css("width", Math.floor(availHeight * reqAspRatio));
  } else if (availAspRat < reqAspRatio) {
    // this is e.g. 16/9 in 4/3, needs boxes on the top, aka, width can be max
    el.css("width", Math.floor(availWidth));
  }
}

function findAspectRatio(el) {
  // we're letting the browser figure out the correct aspect ratio of the *overall* grid, based on the padding-hack in the css
  var aspRatio = el.outerWidth() / el.outerHeight();
  return aspRatio;
}
