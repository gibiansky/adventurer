define(['require', 'editorLanguage'], function (require) {
       // make and editor, set some stuff
       var editor = ace.edit("editor");
       editor.setTheme("ace/theme/monokai");
       editor.setValue("'This is a string' and state this is a `backtick YAY!`. We also have special words location, environment, add(1,1), or(thing thing) also synonyms respond i dunno things yes? sub(8, x) and(things) object if else variable");

       var myMode = require("editorLanguage").Mode;
       editor.getSession().setMode(new myMode());

       });
