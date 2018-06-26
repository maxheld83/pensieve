HTMLWidgets.widget({

  name: 'inanimatus',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        var grid = x.grid;

        // TODO: code to render the widget, e.g.
        el.innerText = grid;

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
