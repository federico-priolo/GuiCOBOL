       IDENTIFICATION DIVISION.
       PROGRAM-ID.    demo.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 alabel               usage pointer.

  
       PROCEDURE DIVISION.

           invoke SUPER "addform" returning SELF.

            move  "GuiCOBOL TEST with a label blue with yellow "  to "caption" of self
                            
            invoke self "addlabel" using " blue label with yellow back" returning alabel.
                 
            move "blue" to "textcolor" of alabel
 
            move "yellow" to "backcolor" of alabel.
            
            move "arial" to "font" of alabel.
            
            move "300%" to "fontsize" of alabel.

            invoke  self "show"

           invoke SELF "run"

           stop run.

       end program demo.

