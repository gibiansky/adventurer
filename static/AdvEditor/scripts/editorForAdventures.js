define(['require', 'editorLanguage'], function (require) {
       // make and editor, set some stuff
       var editor = ace.edit("editor");
       editor.setTheme("ace/theme/monokai");

var str = "# Initialize variable\
\nstate { \
\n variable integer 0;\
\n variable string 1;\
\n} \
\n \
\n# Define synonyms for common actions\
\nsynonym \"look\" \"examine\" \"look at\"; \
\n \
\n# required starting area. Command start is automatically run when game begins. \
\nlocation init { \
\n    command start { \
\n        respond { \
\n            You begin writing your text adventure game! \
\n        } \
\n        \
\n        # Move yourself to starting area\
\n        move-to start; \
\n    } \
\n} \
\n\
\nlocation start {\
\n    command increment {\
\n        respond {\
\n            You incremented an integer!\
\n        }\
\n        set integer `add integer 1`;\
\n    }\
\n    \
\n    command see {\
\n        respond {\
\n            The current integer is `integer`.\
\n        }\
\n    }\
\n}";

       editor.setValue(str);

       var myMode = require("editorLanguage").Mode;
       editor.getSession().setMode(new myMode());

       });
