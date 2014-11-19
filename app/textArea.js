// http://stackoverflow.com/a/14452990/2954547

$(document).on("click", "textarea.inputTextArea", function(evt) {
  
  // evt.target is the button that was clicked
  var el = $(evt.target);
  
  // Raise an event to signal that the value changed
  el.trigger("change");
});

var inputTextAreaBinding = new Shiny.InputBinding();
$.extend(inputTextAreaBinding, {
  find: function(scope) {
    return $(scope).find(".inputTextArea");
  },
  getValue: function(el) {
    return $(el).text();
  },
  setValue: function(el, value) {
    $(el).text(value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputTextAreaBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".inputTextAreaBinding");
  }
});

Shiny.inputBindings.register(inputTextAreaBinding);

// http://stackoverflow.com/a/14452990/2954547
