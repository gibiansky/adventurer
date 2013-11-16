define(['require', 'editorLanguage'], function (require) {
       // make and editor, set some stuff
       var editor = ace.edit("editor");
       editor.setTheme("ace/theme/monokai");

var str = "# Initialize variable\
state { \
 variable integer 0;\
 variable string 1;\
} \
 \
# Define synonyms for common actions\
synonym \"look\" \"examine\" \"look at\"; \
 \
# required starting area. Command start is automatically run when game begins. \
location init { \
    command start { \
        respond { \
            You begin writing your text adventure game! \
        } \
        \
        # Move yourself to starting area\
        move-to start; \
    } \
} \
\
location start {\
    command increment {\
        respond {\
            You incremented an integer!\
        }\
        set integer `add integer 1`;\
    }\
    \
    command see {\
        respond {\
            The current integer is `integer`.\
        }\
    }\
}";

       editor.setValue(str);

       var myMode = require("editorLanguage").Mode;
       editor.getSession().setMode(new myMode());

       });
