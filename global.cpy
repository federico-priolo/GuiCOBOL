      *
      ** GuiCobol private objects
      *

        01 agar-root          usage pointer       external.
        01 agar-main          usage pointer       external.
        01 agar-fixed         usage pointer       external.
        01 agar-form          usage pointer       external.
        01 agar-widget        usage pointer       external.
        01 agar-dummy         usage pointer       external.
        01 agar-panel         usage pointer       external.
        01 agar-parent        usage pointer       external.
        01 agar-frame         usage pointer       external.
        01 agar-object        usage pointer       external.
        01 agar-eventmanager  usage binary-long   external.
        01 agar-menu          usage pointer       external.
        01 agar-struct        usage pointer       external.
        01 agar-pane-one      usage pointer       external.
        01 agar-pane-two      usage pointer       external.
        77 agar-color         pic x(7)            external.
        
        
        01 agar-null-pointer  usage pointer value null.
        01 agar-function      pic x(100)          external.
        01 agar-text          pic x(512)          external.
        01 agar-value         pic x(512)          external.
        01 agar-string        pic x(512)          based.
        01 agar-event         pic x(100)          external.
        01 agar-number        pic s9(12)v9(6)     external.
        01 agar-key-code      usage binary-long   external.
        01 agar-key-char      pic x               external.
        01 agar-button-code   pic s9(5) comp-5    external.
        01 agar-type          usage binary-long.
        01 agar-flags         usage binary-long unsigned.

        01 agar-style         usage binary-long    external.
        
        01 agar-ind           usage signed-short   external.
        01 agar-int           usage unsigned-short external.
        01 agar-items         usage signed-short   external.
        01 agar-size          usage unsigned-short external.
        01 agar-boolean       usage signed-short external.
        01 agar-size-t        usage binary-c-long.  
        01 agar-class         pic x(32) external.
        01 agar-binary        pic 9(5).  
        01 agar-x             usage binary-long    external.
        01 agar-y             usage binary-long    external.
        01 agar-width         usage binary-long    external.
        01 agar-height        usage binary-long    external.
        01 agar-use-width     usage binary-long    external.
        01 agar-use-height    usage binary-long    external.
        01 agar-red           usage unsigned-short external.
        01 agar-green         usage unsigned-short external.
        01 agar-blue          usage unsigned-short external.
        01 agar-callback  usage procedure-pointer external.
        01 agar-procedure     pic x(32)            external.
		
		
        01 rc usage binary-long external.

        01 params            usage pointer.
        01 result            usage binary-long   external.
        01 agar-started       pic 9               external.
        01 agar-cobol         pic x(7)            value "agarcob".
        01 agar-null         usage unsigned-short value null.
      

        01 agar-true          pic 9 value 1.
        01 agar-false         pic 9 value zeros.
        01 agar-on            pic 9 value 1.
        01 agar-off           pic 9 value zeros.
      

 
 
