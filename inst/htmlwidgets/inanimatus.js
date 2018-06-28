HTMLWidgets.widget({

  name: 'inanimatus',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        var inan = $(el);

        // prepend html for table
        inan.prepend(createTable(x.grid, x.colnames, x.rownames, x.header, x.footer, x.aspect_ratio_cards));
        
        // define required vars
        var nCols = Object.keys(x.rownames).length;
        var nRows = Object.keys(x.rownames).length;
        var aspectRatioCards = x.aspect_ratio_cards;
        
        // initial resizing
        var tableAspRatio = findTableAspRatio(nCols, nRows, aspectRatioCards);
        var newDims = resize2AspRatio(inan.innerWidth(), inan.innerHeight(), tableAspRatio);
        
        // this is a side effect, but makes for cleaner DOM
        // to avoid repeating this for every cell, we write it to the css in head, not every cell
        var newCardHeight = Math.floor(newDims.newHeight/nRows) + "px";
        document.querySelector('head').innerHTML += "<style>.ps-grid .cell {height:" + newCardHeight + "}</style>";
        // there is only one such object, so we can write directly css inline
        console.log(inan);
        inan.find(".ps-grid").css("width", Math.floor(newDims.newWidth) + "px");

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
