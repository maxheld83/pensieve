// use strict mode to avoid problems, should apply only to this script
"use strict";

function resizePsGrid(inan) {
  // define required vars
  var aspectRatioCards = inan.data("aspect_ratio_cards");
  var nRows = inan.data("n_rows");
  var nCols = inan.data("n_cols");
  
  // find required aspect ratio
  var tableAspRatio = findTableAspRatio(nCols, nRows, aspectRatioCards);
  // TODO add header and footer here to tableAspRatio
  // this should be done by recalculating the *required aspect ratio* after rendering; we then know how high the actual table with header and footer is, then run resize again
  
  // find new dims
  var newDims = resize2AspRatio(inan.innerWidth(), inan.innerHeight(), tableAspRatio);
  // height is required as card height, not overall height
  var newCardHeight = Math.floor(newDims.newHeight/nRows) + "px";
  
  // write out cardheight and tablewidth
  // to avoid repeating cardheight for every cell, we write it to the css in head, not every cell
  document.querySelector('head').innerHTML += "<style> #" + inan.attr('id') + " .ps-grid .cell {height:" + newCardHeight + "}</style>";
  // there is only one such object, so we can write directly css inline
  inan.find(".ps-grid").css("width", Math.floor(newDims.newWidth) + "px");
};

function resize2AspRatio(availWidth, availHeight, reqAspRatio) {
  var availAspRat = availWidth / availHeight;
  
  // default value is whatever is given
  var newWidth = availWidth;
  var newHeight = availHeight;
  
  // this approach is inspired by https://www.html5rocks.com/en/tutorials/casestudies/gopherwoord-studios-resizing-html5-games/#disqus_thread
  if (availAspRat > reqAspRatio) {
    // this is e.g. 4/3 in 16/9, needs boxes on the side, aka width based on height
    newWidth = availHeight * reqAspRatio;
  } else if (availAspRat < reqAspRatio) {
    // this is e.g. 16/9 in 4/3, needs boxes on the top, aka, height based on width
    newHeight = availWidth / reqAspRatio;
  }
  
  var newDims = {
    newWidth: newWidth,
    newHeight: newHeight
  };
  
  return newDims;
}

function findTableAspRatio(nCols, nRows, aspectRatioCards) {
  var tableAspRatio = nCols/nRows * aspectRatioCards;
  return tableAspRatio;
}

// create table from json
function createTable(tableData, colNames, rowNames, header, footer, aspectRatioCards) {
  var table = document.createElement('table');
  table.className = "ps-grid";
  var tableBody = document.createElement('tbody');

  tableData.forEach(function(rowData) {
    var row = createTableRow(rowData, false, false);

    tableBody.appendChild(row);
  });
  
  // make header and footer
  if (header) {
    var tableHeader = document.createElement('thead');
    tableHeader.appendChild(createTableRow(colNames, true, false))
    table.appendChild(tableHeader);
  }
  if (footer) {
    var tableFooter = document.createElement('tfoot');
    tableFooter.appendChild(createTableRow(colNames, false, true))
    table.appendChild(tableFooter);
  }
  
  table.appendChild(tableBody);
  return table;
}

function createTableRow(rowData, header, footer) {
  var row = document.createElement('tr');
  
  rowData.forEach(function(cellData) {
    if (header) {
      var cell = document.createElement('th');
    } else if (footer) {
      var cell = document.createElement('th');
    } else {
      var cell = document.createElement('td');
      cell.className = "cell";
      if (cellData) {
        cell.className += " allowed";
      }
    }
    cell.appendChild(document.createTextNode(cellData));
    row.appendChild(cell);
  });
  
  return row;
}
