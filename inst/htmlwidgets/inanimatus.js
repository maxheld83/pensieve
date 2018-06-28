HTMLWidgets.widget({

  name: 'inanimatus',

  type: 'output',

  factory: function(el, width, height) {

    var inan = $(el);

    return {

      renderValue: function(x) {
        
        // prepend html for table
        inan.prepend(createTable(x.grid, x.colnames, x.rownames, x.header, x.footer, x.aspect_ratio_cards));
        
        // write info necessary for resizing into data attribute
        inan.attr("data-aspect_ratio_cards", x.aspect_ratio_cards);
        inan.attr("data-n_rows", Object.keys(x.rownames).length);
        inan.attr("data-n_cols", Object.keys(x.colnames).length);
        
        // initial resizing
        resizePsGrid(inan);
      },

      resize: function(width, height) {
        resizePsGrid(inan);
      }

    };
  }
});
