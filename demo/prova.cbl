000010  identification division.
000020  program-id. sample.
000030
000040  environment division.
000050  configuration section.
000060
000070  data division.
000080  working-storage section.
000090  01 rc usage binary-long.
000100  01 win usage pointer.
000110  01 extraneous-pointer usage pointer.
000110  01 label1             usage pointer.
000110  01 agar-widget        usage pointer.
000110  01 agar-struct        usage pointer.
000110  01 buffer usage pointer.
000110  01 agar-string  pic x(100) based.
000120  01 agar-text pic x(40) Value "questa e' la stringa".
000130  procedure division.
000140  sample-main.
000150            call "AG_InitCore" using null by value 0 returning rc
000160             on exception display "error: no libagar" upon syserr
000170            end-call
000180           if rc = -1 then
000190           display "error: AG_InitCore failure" upon syserr
000200           goback
000210           end-if 
000220           call static "AG_InitGraphics" using by value 0
000230              returning rc
000240           if rc = -1 then
000250           display "error: AG_InitGraphics failure" upon syserr
000260           goback
000270          end-if
000310

000340         call "AG_WindowNew" using by value 0 returning win

70		       call "AG_LabelNew" using
000380		           by value win 0
000390		           by reference "ciao Federico"
000400		       returning label1

                
               call static "AG_ObjectGetName" 
                using by value label1
                  returning agar-struct.
                

                set address of agar-string  TO agar-struct.

                move agar-string      to agar-text.
                
                display "cerco label0"
                
                call  "ag_object_find_child" 
                    using by value win
                     by content  "label0"
                      returning agar-widget.
                

                display "finchild riporta" 
                 agar-widget.    


                call static "AG_LabelSizeHint"
                   using by value label1
                    by reference agar-text.
       
                 call  "AG_SetStyle" using
                   by value label1
                  by reference     "background-color"
000057            by reference  "blue".


                
                call "AG_SetStyle" using
                   by value label1
                  by reference  "text-color"
000057            by reference  "red".


                    
      * call "AG_ObjectFind" 
      *  using by value win
      *               by reference  agar-text
      *                returning agar-widget.
       
      *          display function trim(agar-text) 
      *            "find riporta" agar-widget.    
                 
                         

                 call "AG_ObjectGetClassName" 
                   using by value win
017040                 by value 0  returning buffer
017060                
017070            set address of agar-string  TO buffer.



000410        call "AG_WindowShow" using by value win returning omitted
000420		        call "AG_EventLoop" returning rc
000430		                call static "AG_LabelNew" using
000440	                by value win 0
000450           by reference "ciao "
000460            returning extraneous-pointer
000470          call static "AG_WindowShow" using by value win 
000480           returning omitted
000490          call  static "AG_EventLoop" returning rc
000500          display  "Agar rc = " rc
000510          goback.
000520 end program sample.
