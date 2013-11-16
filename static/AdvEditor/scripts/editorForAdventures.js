define(['require', 'editorLanguage'], function (require) {
       // make and editor, set some stuff
       var editor = ace.edit("editor");
       editor.setTheme("ace/theme/monokai");

var str = " # Starter code for your text adventure! \
 \
# initialize all variables first, in your state \
state { \
    variable hasinitializestate 1; \
    variable descriptionofgame \"empty\"; \
    variable haswon 0; \
    variable failtowin 0; \
    variable shrew 0; \
    variable hasshrew 0; \
    variable neversleep 0; \
} \
 \
# add synonyms next so users have multiple options of what words they can type \
synonym \"look\" \"examine\" \"look at\" \
synonym \"go\" \"move to\" \"got to\" \
synonym \"stuff\" \"things\" \
 \
# required starting area. Command start is automatically run when game begins. \
location init { \
    command start { \
        respond { \
            You begin writing your text adventure game! \
        } \
        #move yourself to starting area \
        move-to start \
    } \
} \
 \
location start { \
	command win { \
        # inside commands you can use conditionals \
        if `haswon` { \
            # using `s, you can  \
            respond { \
                You win this `descriptionofgame` game! \
            } \
            move-to end \
        } \
		else { \
			set failtowin (add failtowin 1); \
			if `eq failtowin 1` { \
				respond { \
					You have failed to win `failtowin`  time \
				} \
			} \
		else { \
			respond { \
				You have failed to win `failtowin`  times \
			} \
		} \
    } \
     \
    command look shrew { \
    	if `or hasshrew shrew` { \
    		respond { \
    			It's adorable! \
    		} \
    	} \
    	else { \
    		respond { \
    			What shrew? \
    		} \
    	} \
    } \
    command never sleep { \
    	if `neversleep` { \
    		respond { \
    			With the powers of neversleep, you stay up all night to hackathon! \
    		} \
    	} \
    	else { \
    		respond { \
    			You try to neversleep, but zzzzzzzzzzzzz. \
    		} \
    	} \
    } \
	command look { \
		respond { \
			You are at the start of your game! \
			 \
			Your game is currently `decriptionofgame`. \
		} \
		if `eq decriptionofgame \"empty\"` \
		{ \
			respond { \
				You should try to add stuff. \
			} \
		} \
		else { \
			if `eq decriptionofgame \"a good start\"` { \
				Almost there! Add commands! \
			} \
			else { \
				respond { \
					Now you can win! Try it! \
				} \
				set haswon 1; \
			} \
		} \
	} \
	command add stuff { \
		respond { \
			You add some locations with objects you can interact with. \
			 \
			An elephant shrew appears. \
		} \
		set descriptionofgame \"a good start\"; \
		set shrew 1; \
	} \
	command take shrew { \
		if `shrew` { \
			respond { \
				A shrew! \
				 \
				<img src=\"http://a-z-animals.com/animals/elephant-shrew/\"> \
				 \
			} \
			set shrew 0; \
			set hasshrew 1; \
		} \
		else { \
			What shrew? \
		} \
	} \
	command add commands { \
		respond { \
			You add the ability to pull all-nighters! Try the command never sleep. \
		} \
		set neversleep 1; \
		set descriptionofgame \"awesome\"; \
	} \
} \
 \
# End state! You won! \
location end in global { \
	command look {  \
		respond { \
		You won!  \
		} \
	} \
}";

       editor.setValue(str);

       var myMode = require("editorLanguage").Mode;
       editor.getSession().setMode(new myMode());

       });
