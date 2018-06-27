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
        
        // initial resizing
        var availWidth = inan.innerWidth();
        var nCols = Object.keys(x.colnames).length;
        var nRows = Object.keys(x.rownames).length;
        var cardWidth = availWidth/nCols;
        var cardHeight = cardWidth/x.aspect_ratio_cards;
        var tableHeight = cardHeight * nRows;
                
        // this is a side effect, but makes for cleaner DOM
        document.querySelector('head').innerHTML += "<style>.ps-grid .cell {height:" + cardHeight + "px" + "}</style>";

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
