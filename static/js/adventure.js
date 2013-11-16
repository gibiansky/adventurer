// Generated by CoffeeScript 1.6.1

/* Models
*/


(function() {
  var AdventureView, Command, CommandHistory, CommandView, Sidebar, TextField,
    __hasProp = {}.hasOwnProperty,
    __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    _this = this;

  Command = (function(_super) {

    __extends(Command, _super);

    function Command() {
      return Command.__super__.constructor.apply(this, arguments);
    }

    Command.prototype.url = "/run";

    return Command;

  })(Backbone.Model);

  /* Collections
  */


  CommandHistory = (function(_super) {

    __extends(CommandHistory, _super);

    function CommandHistory() {
      return CommandHistory.__super__.constructor.apply(this, arguments);
    }

    CommandHistory.prototype.model = Command;

    CommandHistory.prototype.url = "/history";

    return CommandHistory;

  })(Backbone.Collection);

  /* Views
  */


  Sidebar = (function(_super) {

    __extends(Sidebar, _super);

    function Sidebar() {
      var _this = this;
      this.addHintText = function() {
        return Sidebar.prototype.addHintText.apply(_this, arguments);
      };
      this.render = function() {
        return Sidebar.prototype.render.apply(_this, arguments);
      };
      this.update = function() {
        return Sidebar.prototype.update.apply(_this, arguments);
      };
      this.initialize = function() {
        return Sidebar.prototype.initialize.apply(_this, arguments);
      };
      return Sidebar.__super__.constructor.apply(this, arguments);
    }

    Sidebar.prototype.initialize = function() {
      return this.update();
    };

    Sidebar.prototype.update = function() {
      var _this = this;
      return $.get("/items", function(data) {
        _this.items = $.parseJSON(data);
        return _this.render();
      });
    };

    Sidebar.prototype.render = function() {
      var _this = this;
      this.$el.html('');
      _.each(this.items, function(item) {
        var itemDiv;
        itemDiv = $(document.createElement('div'));
        itemDiv.attr("class", "item");
        itemDiv.html(item);
        return _this.$el.append(itemDiv);
      });
      return this.addHintText();
    };

    Sidebar.prototype.addHintText = function() {
      var div, hintText;
      hintText = _.template($('#hint-text-template').html(), {});
      div = $(document.createElement('div'));
      div.html(hintText);
      return this.$el.append(div);
    };

    return Sidebar;

  })(Backbone.View);

  AdventureView = (function(_super) {

    __extends(AdventureView, _super);

    function AdventureView() {
      var _this = this;
      this.render = function() {
        return AdventureView.prototype.render.apply(_this, arguments);
      };
      this.createAndRender = function(collection) {
        return AdventureView.prototype.createAndRender.apply(_this, arguments);
      };
      this.addCommand = function(cmd) {
        return AdventureView.prototype.addCommand.apply(_this, arguments);
      };
      this.initialize = function(options) {
        return AdventureView.prototype.initialize.apply(_this, arguments);
      };
      return AdventureView.__super__.constructor.apply(this, arguments);
    }

    AdventureView.prototype.views = [];

    AdventureView.prototype.initialize = function(options) {
      this.historyView = $(document.createElement('div'));
      this.historyView.attr("id", 'history-view');
      this.$el.append(this.historyView);
      this.textEntry = $(document.createElement('div'));
      this.textEntry.attr("id", 'text-entry');
      this.$el.append(this.textEntry);
      this.history = options.history;
      this.history.bind('add', this.addCommand);
      this.history.bind('change', this.render);
      this.history.bind('reset', this.createAndRender);
      return this.render();
    };

    AdventureView.prototype.addCommand = function(cmd) {
      this.views.push(new CommandView({
        model: cmd
      }));
      return this.render();
    };

    AdventureView.prototype.createAndRender = function(collection) {
      var _this = this;
      _.each(collection.models, function(cmd) {
        return _this.addCommand(cmd);
      });
      return this.render;
    };

    AdventureView.prototype.render = function() {
      var _this = this;
      this.historyView.innerHtml = '';
      _.each(this.views, function(view) {
        return _this.historyView.append(view.render().el);
      });
      this.historyView.scrollTop(this.historyView.prop('scrollHeight'));
      this.historyView.find("img").load(function() {
        return _this.historyView.scrollTop(_this.historyView.prop('scrollHeight'));
      });
      return this;
    };

    return AdventureView;

  })(Backbone.View);

  TextField = (function(_super) {

    __extends(TextField, _super);

    function TextField() {
      var _this = this;
      this.render = function() {
        return TextField.prototype.render.apply(_this, arguments);
      };
      this.cursorSplitText = function() {
        return TextField.prototype.cursorSplitText.apply(_this, arguments);
      };
      this.keyUp = function(event) {
        return TextField.prototype.keyUp.apply(_this, arguments);
      };
      this.keyDown = function(event) {
        return TextField.prototype.keyDown.apply(_this, arguments);
      };
      this.keyPress = function(event) {
        return TextField.prototype.keyPress.apply(_this, arguments);
      };
      this.getHistoryCommands = function() {
        return TextField.prototype.getHistoryCommands.apply(_this, arguments);
      };
      this.submit = function(text) {
        return TextField.prototype.submit.apply(_this, arguments);
      };
      this.initialize = function(options) {
        return TextField.prototype.initialize.apply(_this, arguments);
      };
      return TextField.__super__.constructor.apply(this, arguments);
    }

    TextField.cursorImgSrc = "/static/imgs/cursor.gif";

    TextField.prototype.cursorHtml = "<img src='" + TextField.cursorImgSrc + "' id='cursor' />";

    TextField.prototype.enteredText = "";

    TextField.prototype.cursorPosition = 0;

    TextField.prototype.initialize = function(options) {
      _.bindAll(this);
      $(document).bind('keypress', this.keyPress);
      $(document).bind('keydown', this.keyDown);
      $(document).bind('keyup', this.keyUp);
      this.history = options.history;
      this.items = options.items;
      this.sidebar = options.sidebar;
      this.historyPosition = this.history.length - 1;
      return this.render();
    };

    TextField.prototype.submit = function(text) {
      var command;
      command = new Command({
        command: text,
        response: ""
      });
      this.history.add(command);
      this.historyPosition = this.history.length;
      command.save();
      return this.sidebar.update();
    };

    TextField.prototype.getHistoryCommands = function() {
      return this.history.collect((function(model) {
        return model.get("command");
      }));
    };

    TextField.prototype.keyPress = function(event) {
      var char, postCursor, preCursor, printable, _ref;
      char = String.fromCharCode(event.which);
      printable = /^[-a-zA-Z0-9 ]$/.test(char);
      if (!printable) {
        return;
      }
      _ref = this.cursorSplitText(), preCursor = _ref[0], postCursor = _ref[1];
      this.enteredText = preCursor + char + postCursor;
      this.cursorPosition += 1;
      return this.render();
    };

    TextField.prototype.keyDown = function(event) {
      var backspaceKeyCode, deleteKeyCode, downKeyCode, leftKeyCode, postCursor, preCursor, rightKeyCode, textLen, upKeyCode, _ref;
      deleteKeyCode = 46;
      backspaceKeyCode = 8;
      leftKeyCode = 37;
      upKeyCode = 38;
      rightKeyCode = 39;
      downKeyCode = 40;
      textLen = this.enteredText.length;
      _ref = this.cursorSplitText(), preCursor = _ref[0], postCursor = _ref[1];
      if (this.historyPosition < 0 || this.historyPosition > this.history.length) {
        this.historyPosition = this.history.length;
      }
      switch (event.keyCode) {
        case leftKeyCode:
          if (this.cursorPosition !== 0) {
            this.cursorPosition--;
          }
          break;
        case rightKeyCode:
          if (this.cursorPosition !== textLen) {
            this.cursorPosition++;
          }
          break;
        case deleteKeyCode:
          if (this.cursorPosition !== textLen) {
            this.enteredText = preCursor + postCursor.substring(1);
          }
          break;
        case backspaceKeyCode:
          event.preventDefault();
          if (this.cursorPosition !== 0) {
            this.enteredText = preCursor.substring(0, preCursor.length - 1) + postCursor;
            this.cursorPosition--;
          }
          break;
        case upKeyCode:
          if (this.historyPosition > 0) {
            this.historyPosition--;
            this.enteredText = this.getHistoryCommands()[this.historyPosition];
            this.cursorPosition = this.enteredText.length;
          }
          break;
        case downKeyCode:
          if (this.historyPosition < this.getHistoryCommands().length - 1) {
            this.historyPosition++;
            this.enteredText = this.getHistoryCommands()[this.historyPosition];
            this.cursorPosition = this.enteredText.length;
          } else {
            this.historyPosition = this.history.length;
            this.enteredText = "";
            this.cursorPosition = 0;
          }
      }
      return this.render();
    };

    TextField.prototype.keyUp = function(event) {
      var enterKeyCode, submitting;
      enterKeyCode = 13;
      if (event.which === enterKeyCode) {
        submitting = this.enteredText;
        if (submitting.length > 0) {
          this.enteredText = "";
          this.cursorPosition = 0;
          this.submit(submitting);
          return this.render();
        }
      }
    };

    TextField.prototype.cursorSplitText = function() {
      var postCursor, preCursor;
      preCursor = this.enteredText.substring(0, this.cursorPosition);
      postCursor = this.enteredText.substring(this.cursorPosition);
      return [preCursor, postCursor];
    };

    TextField.prototype.render = function() {
      var postCursor, preCursor, template, text, _ref;
      _ref = this.cursorSplitText(), preCursor = _ref[0], postCursor = _ref[1];
      text = preCursor + this.cursorHtml + postCursor;
      template = _.template($('#text-entry-template').html(), {
        text: text
      });
      this.$el.html(template);
      return this;
    };

    return TextField;

  })(Backbone.View);

  CommandView = (function(_super) {

    __extends(CommandView, _super);

    function CommandView() {
      var _this = this;
      this.render = function() {
        return CommandView.prototype.render.apply(_this, arguments);
      };
      this.initialize = function() {
        return CommandView.prototype.initialize.apply(_this, arguments);
      };
      return CommandView.__super__.constructor.apply(this, arguments);
    }

    CommandView.prototype.tagName = 'div';

    CommandView.prototype.initialize = function() {
      return this.model.bind('change', this.render);
    };

    CommandView.prototype.render = function() {
      var response, template;
      response = this.model.attributes.response;
      if (response.match(/^error:/)) {
        response = "<span class='error'>" + response + "</span>";
      }
      template = _.template($('#command-template').html(), {
        command: this.model.attributes.command,
        response: response.replace(/\n/g, "<br/>")
      });
      this.$el.html(template);
      return this;
    };

    return CommandView;

  })(Backbone.View);

  this.loadGame = function() {
    var adventureView, history, sidebarView, textField;
    history = new CommandHistory([]);
    sidebarView = new Sidebar({
      el: $('#sidebar')
    });
    adventureView = new AdventureView({
      el: $('#main'),
      history: history
    });
    textField = new TextField({
      el: adventureView.textEntry,
      history: history,
      sidebar: sidebarView
    });
    return history.fetch({
      reset: true
    });
  };

}).call(this);
