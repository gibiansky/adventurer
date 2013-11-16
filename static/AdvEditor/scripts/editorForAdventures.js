define(['require', 'editorLanguage'], function (require) {
       // make and editor, set some stuff
       var editor = ace.edit("editor");
       editor.setTheme("ace/theme/monokai");

var str = " # Starter code for your text adventure! \
\n \
\n# initialize all variables first, in your state \
\nstate { \
\n    variable hasinitializestate 1; \
\n    variable descriptionofgame \"empty\"; \
\n    variable haswon 0; \
\n    variable failtowin 0; \
\n    variable shrew 0; \
\n    variable hasshrew 0; \
\n    variable neversleep 0; \
\n} \
\n \
\n# add synonyms next so users have multiple options of what words they can type \
\nsynonym \"look\" \"examine\" \"look at\"; \
\nsynonym \"go\" \"move to\" \"got to\"; \
\nsynonym \"stuff\" \"things\"; \
\n \
\n# required starting area. Command start is automatically run when game begins. \
\nlocation init { \
\n    command start { \
\n        respond { \
\n            You begin writing your text adventure game! \
\n        } \
\n        #move yourself to starting area \
\n        move-to start; \
\n    } \
\n} \
\n \
\nlocation start { \
\n	command win { \
\n        # inside commands you can use conditionals \
\n        if `haswon` { \
\n            # using `s, you can  \
\n            respond { \
\n                You win this `descriptionofgame` game! \
\n            } \
\n            move-to end; \
\n        } \
\n		else { \
\n			set failtowin (add failtowin 1); \
\n			if `eq failtowin 1` { \
\n				respond { \
\n					You have failed to win `failtowin`  time \
\n				} \
\n			} \
\n		else { \
\n			respond { \
\n				You have failed to win `failtowin`  times \
\n			} \
\n		} \
\n    } \
\n     \
\n    command look shrew { \
\n    	if `or hasshrew shrew` { \
\n    		respond { \
\n    			It's adorable! \
\n    		} \
\n    	} \
\n    	else { \
\n    		respond { \
\n    			What shrew? \
\n    		} \
\n    	} \
\n    } \
\n    command never sleep { \
\n    	if `neversleep` { \
\n    		respond { \
\n    			With the powers of neversleep, you stay up all night to hackathon! \
\n    		} \
\n    	} \
\n    	else { \
\n    		respond { \
\n    			You try to neversleep, but zzzzzzzzzzzzz. \
\n    		} \
\n    	} \
\n    } \
\n	command look { \
\n		respond { \
\n			You are at the start of your game! \
\n			 \
\n			Your game is currently `decriptionofgame`. \
\n		} \
\n		if `eq decriptionofgame \"empty\"` \
\n		{ \
\n			respond { \
\n				You should try to add stuff. \
\n			} \
\n		} \
\n		else { \
\n			if `eq decriptionofgame \"a good start\"` { \
\n				Almost there! Add commands! \
\n			} \
\n			else { \
\n				respond { \
\n					Now you can win! Try it! \
\n				} \
\n				set haswon 1; \
\n			} \
\n		} \
\n	} \
\n	command add stuff { \
\n		respond { \
\n			You add some locations with objects you can interact with. \
\n			 \
\n			An elephant shrew appears. \
\n		} \
\n		set descriptionofgame \"a good start\"; \
\n		set shrew 1; \
\n	} \
\n	command take shrew { \
\n		if `shrew` { \
\n			respond { \
\n				A shrew! \
\n				 \
\n				<img src=\"http://a-z-animals.com/animals/elephant-shrew/\"> \
\n				 \
\n			} \
\n			set shrew 0; \
\n			set hasshrew 1; \
\n		} \
\n		else { \
\n			What shrew? \
\n		} \
\n	} \
\n	command add commands { \
\n		respond { \
\n			You add the ability to pull all-nighters! Try the command never sleep. \
\n		} \
\n		set neversleep 1; \
\n		set descriptionofgame \"awesome\"; \
\n	} \
\n} \
\n \
\n# End state! You won! \
\nlocation end { \
\n	command look {  \
\n		respond { \
\n		You won!  \
\n		} \
\n	} \
\n}";

       editor.setValue(str);

       var myMode = require("editorLanguage").Mode;
       editor.getSession().setMode(new myMode());

       });
