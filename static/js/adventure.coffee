### Models ###
class Command extends Backbone.Model
    url: "{name}/run"

### Collections ###
class CommandHistory extends Backbone.Collection
    model: Command
    url: "{name}/history"

### Views ###
class AdventureView extends Backbone.View
    views: []

    initialize: (options) =>
        # Create a place to put history.
        @historyView = $(document.createElement 'div')
        @historyView.attr("id", 'history-view')
        @$el.append(@historyView)

        # Create text entry field. This is the backing element for the TextField.
        @textEntry = $(document.createElement 'div')
        @textEntry.attr("id", 'text-entry')
        @$el.append(@textEntry)

        # Store the command history model.
        # Update and re-render when the collection changes.
        @history = options.history
        @history.bind('add', @addCommand)
        @history.bind('change', @render)
        @history.bind('reset', @createAndRender)

        # Do initial rendering.
        @render()

    addCommand: (cmd) =>
        @views.push (new CommandView model: cmd)
        @render()

    createAndRender: (collection) =>
        _.each collection.models, (cmd) =>
            @addCommand cmd
        @render

    render: =>
        # Clear old history.
        @historyView.innerHtml = ''

        # Render each command in the history.
        _.each @views, (view) =>
            @historyView.append(view.render().el)

        # Make sure the bottom of the history is always visible.
        @historyView.scrollTop(@historyView.prop('scrollHeight'))

        # Prevent images from screwing this up.
        @historyView.find("img").load =>
          @historyView.scrollTop(@historyView.prop('scrollHeight'))

        this

class TextField extends Backbone.View
    # The HTML code used to represent the cursor.
    @cursorImgSrc: "/static/imgs/cursor.gif"
    cursorHtml: "<img src='#{@cursorImgSrc}' id='cursor' />"

    # The text currently entered in the text field.
    enteredText: ""

    # The position of the cursor in the text field.
    cursorPosition: 0

    initialize: (options) =>
        # Bind keypress and keydown events.
        _.bindAll(this)
        $(document).bind 'keypress', @keyPress
        $(document).bind 'keydown', @keyDown
        $(document).bind 'keyup', @keyUp

        # Store history and items so we can add to it.
        @history = options.history
        @items = options.items
        @historyPosition = @history.length - 1

        # Do initial rendering.
        @render()

    submit: (text) =>
        command = new Command
            command: text
            response: ""
        @history.add command
        @historyPosition = @history.length
        command.save()

    getHistoryCommands: () =>
        return @history.collect ((model) -> model.get("command"))

    keyPress: (event) =>
        # Get the character to insert.
        char = String.fromCharCode(event.which)

        # Verify that this character is allowed.
        printable = /^[-a-zA-Z0-9 ]$/.test(char)
        if not printable then return
        
        # Split around the cursor, so we can insert the character.
        [preCursor, postCursor] = @cursorSplitText()

        # Add the character and increment cursor position to account for it.
        @enteredText = preCursor + char + postCursor
        @cursorPosition += 1

        # Update rendered text.
        @render()

    keyDown: (event) =>
        # Keycodes we care about.
        deleteKeyCode    = 46
        backspaceKeyCode = 8
        leftKeyCode      = 37
        upKeyCode        = 38
        rightKeyCode     = 39
        downKeyCode      = 40

        textLen = @enteredText.length
        [preCursor, postCursor] = @cursorSplitText()

        # Fix history position.
        if @historyPosition < 0 or @historyPosition > @history.length
            @historyPosition = @history.length

        switch event.keyCode
            # Move the cursor left and right with arrow keys.
            when leftKeyCode
                if @cursorPosition != 0
                    @cursorPosition--
            when rightKeyCode
                if @cursorPosition != textLen
                    @cursorPosition++

            # Delete after and before the cursor appropriately.
            when deleteKeyCode
                if @cursorPosition != textLen
                    @enteredText = preCursor + postCursor.substring(1)
            when backspaceKeyCode
                # Do not go back.
                event.preventDefault()

                # Delete a character.
                if @cursorPosition != 0
                    @enteredText = preCursor.substring(0, preCursor.length - 1) + postCursor
                    @cursorPosition--

            # Navigate up in history
            when upKeyCode
                if @historyPosition > 0
                    @historyPosition--
                    @enteredText = @getHistoryCommands()[@historyPosition]
                    @cursorPosition = @enteredText.length

            when downKeyCode
                if @historyPosition < @getHistoryCommands().length - 1
                    @historyPosition++
                    @enteredText = @getHistoryCommands()[@historyPosition]
                    @cursorPosition = @enteredText.length
                else
                    # If we're at the end of history, set a blank line for a new command.
                    @historyPosition = @history.length
                    @enteredText = ""
                    @cursorPosition = 0

        # Update rendered text.
        @render()

    keyUp: (event) =>
        # Submit the command on ENTER.
        enterKeyCode = 13
        if event.which == enterKeyCode
            submitting = @enteredText

            if submitting.length > 0
                # Clear text field.
                @enteredText = ""
                @cursorPosition = 0

                # Submit command and re-render everything!
                @submit(submitting)
                @render()


    cursorSplitText: =>
        # Split the entered text around the cursor, and return the two chunks.
        preCursor = @enteredText.substring(0, @cursorPosition)
        postCursor = @enteredText.substring(@cursorPosition)

        [preCursor, postCursor]

    render: =>
        # Update the text entered in the text field.
        [preCursor, postCursor] = @cursorSplitText()
        text = preCursor + @cursorHtml + postCursor

        template = _.template $('#text-entry-template').html(), text: text
        @$el.html template

        this

class CommandView extends Backbone.View
    tagName: 'div'

    initialize: =>
        @model.bind('change', @render)

    render: =>
        response = @model.attributes.response
        if response.match /^error:/
            response = "<span class='error'>#{response}</span>"

        template = _.template $('#command-template').html(),
            command: @model.attributes.command
            response: response.replace(/\n/g, "<br/>")

        @$el.html template

        this

@loadGame = ->
    history = new CommandHistory []

    # Load all views: main view, and text entry view.
    adventureView = new AdventureView
        el: $('#main')
        history: history
    textField = new TextField
        el: adventureView.textEntry
        history: history

    # Load command history.
    history.fetch
      reset: true
