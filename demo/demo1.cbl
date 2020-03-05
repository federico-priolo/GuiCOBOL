000010  IDENTIFICATION DIVISION.
000020  PROGRAM-ID.    demo1.
000030  ENVIRONMENT DIVISION.
000040  DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 stringa       pic is  x(100).
        01 ageneric        usage pointer external.
        01 attivo        usage pointer external.
        01 abox         usage pointer external.
        01 abox1         usage pointer external.
        01 atext        usage pointer external.
        01 abutton      usage pointer external.
        01 abutton1     usage pointer external.
        01 abutton2     usage pointer external.
        01 alabel       usage pointer external.
        01 aradio       usage pointer external.
        01 acheck       usage pointer external.
        
        
000240  PROCEDURE DIVISION.
000260
000270           invoke SUPER "addform" returning SELF.

                 invoke SELF "size" using 900  500
                 
                 invoke self "addspaceshoriz"
                 
                 move "MIDDLERIGHT" TO "position" of self

                 move  "GuiCOBOL TEST FOR GNUCOBOL "  to "caption" of self

                 move "orangered" to "backcolor" of self.
                 move "darkgreen" to "textcolor" of self.
                 move "120%" to "fontsize" of self.

                 move spaces to stringa.
                 
                 move "caption" of  self  to stringa.
                 
                invoke self "addpane" using "v" returning abox abox1.
                 
                move 1 to "homogenous" of abox.

                invoke abox "addtext" using "First name " returning atext
                
                move "90%" to "fontsize" of atext.

                move "blue" to "backcolor" of atext.
                move "yellow" to "textcolor" of atext.

                move "x(30)" to "render" of atext
                
                move "write your first name here"      to "text" OF atext.
                

                 invoke atext "refresh"
                
                 invoke abox "addtext" using "Second name " returning atext
                 
                 move "90%" to "fontsize" of atext.

                move "blue" to "backcolor" of atext.
                move "yellow" to "textcolor" of atext.

                invoke self "addspaceshoriz"

                 invoke abox "addcombo" using "Gender"   returning abutton
                 
                  move "x(10)" to "render" of abutton.
                 
                  move 2 to "maxitem" of abutton.
                 
                  invoke abutton "additem"  using "male"
                  invoke abutton "additem"  using "female"
                

                invoke self "addbox" returning abox.
                
                invoke self "addspaceshoriz"
                 invoke self "addspaceshoriz"
                 invoke self "addspaceshoriz"


                 invoke abox  "addradioto" using "pressedkey"  returning aradio

                 move "h" to "disposition" of aradio
                 invoke aradio "additem"  using "choice one"
                 invoke aradio  "additem"  using "choice two"
                 invoke aradio  "additem"  using "choice three"
                 invoke aradio  "additem"  using "choice four"
                 invoke aradio  "additem"  using "choice five"
                

           
                 invoke self "show"
               
                 invoke SELF  "Closed" using "exitForm"


                 invoke SELF "run"
000280
001080           stop run.
001090
001100  end program demo1.


001120  identification division.
001130  program-id. exitForm.
001140  data division.
001150  working-storage section.
001160
001260  procedure division.
  
      *               invoke self "close".

      *               invoke self  "stoprun".

                   display "you are exitform".


001350  exit program.
001360  end program exitForm.
001110

       identification division.
       program-id. DefaultOpened.
       data division.
       working-storage section.

       linkage section.

       procedure division .

               display "sono in Defaultopen".
 
       exit program.
       end program DefaultOpened.
