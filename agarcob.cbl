       identification division.
       program-id. agarcob is recursive.
       
      *
      * agar connector for GNUCOBOL
      *
      * FIRST 1th AUGUST   0.1.0  LAST 0.1.26  3th march  2020
      *
      * Copyright (C) 2012-2020 Federico Priolo TP ONE SRL
      *
      * This program is free software; you can redistribute it and/or modify
      * it under the terms of the GNU General Public License as published by
      * the Free Software Foundation; either version 2, or (at your option)
      * any later version.
      *
      * This program is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.
      *
      * You should have received a copy of the GNU General Public License
      * along with this software; see the file COPYING.  If not, write to
      * the Free Software Foundation, 51 Franklin Street, Fifth Floor
      * Boston, MA 02110-1301 USA
      *
      *
      *---------------------------------------------------------------------
       data division.

       working-storage section.
       01 ind                      usage binary-long unsigned.
       01 ind2                     usage binary-long unsigned.
       
       01 pane-instance based.  
          05 filler                    pic x(712).
          
          05 pane-type                 usage binary-long sync.
          05 pane-flags                usage binary-long unsigned.
 
          05 pane-divs             occurs 2 times usage pointer sync.
       
       
       01 combo-instance based.
         04 filler                    pic x(712).
         
         04 resto-combo.
          05 combo-flags               usage binary-long sync.
          05 combo-tbox                usage pointer     sync.
          05 combo-button              usage pointer.
          05 combo-tlist               usage pointer.
          05 combo-panel               usage pointer.
          05 combo-wsaved              usage binary-long sync.
          05 combo-hsaved              usage binary-long.
          05 combo-wprelist            usage binary-long.
          05 combo-hprelist            usage binary-long.



        01 window-instance based.
         04 filler                     pic x(640).
         04 resto-win.
          05 win-flags                 usage binary-long sync.
          

       01 object-flags.
          05 AG-OBJECT-FLOATING-VARS   usage binary-long value h'00001'.
          05 AG-OBJECT-NON-PERSISTENT  usage binary-long value h'00002'.
          05 AG-OBJECT-INDESTRUCTIBLE  usage binary-long value h'00004'.
          05 AG-OBJECT-RESIDENT        usage binary-long value h'00008'.
          05 AG-OBJECT-PRESERVE-DEPS   usage binary-long value h'00010'.
          05 AG-OBJECT-STATIC          usage binary-long value h'00020'.
          05 AG-OBJECT-READONLY        usage binary-long value h'00040'.
          05 AG-OBJECT-WAS-RESIDENT    usage binary-long value h'00080'.
          05 AG-OBJECT-REOPEN-ONLOAD   usage binary-long value h'00200'.
          05 AG-OBJECT-REMAIN-DATA     usage binary-long value h'00400'.
          05 AG-OBJECT-DEBUG           usage binary-long value h'00800'.
          05 AG-OBJECT-NAME-ONATTACH   usage binary-long value h'01000'.
          05 AG-OBJECT-CHLD-AUTOSAVE   usage binary-long value h'02000'.
          05 AG-OBJECT-DEBUG-DATA      usage binary-long value h'04000'.
          05 AG-OBJECT-INATTACH        usage binary-long value h'08000'.
          05 AG-OBJECT-INDETACH        usage binary-long value h'10000'.
      *>  05 AG-OBJECT-SAVED-FLAGS     usage binary-long value h'G_OBJ'.
      *>  05 AG-OBJECT-DUPED-FLAGS     usage binary-long value h'G_OBJ'.

       01 window-positions.
          05 AG-WINDOW-NONE            usage binary-long value 0.
          05 AG-WINDOW-UPPER-LEFT      usage binary-long value 1.
          05 AG-WINDOW-UPPER-CENTER    usage binary-long value 2.
          05 AG-WINDOW-UPPER-RIGHT     usage binary-long value 3.
          05 AG-WINDOW-MIDDLE-LEFT     usage binary-long value 4.
          05 AG-WINDOW-CENTER          usage binary-long value 5.
          05 AG-WINDOW-MIDDLE-RIGHT    usage binary-long value 6.
          05 AG-WINDOW-LOWER-LEFT      usage binary-long value 7.
          05 AG-WINDOW-LOWER-CENTER    usage binary-long value 8.
          05 AG-WINDOW-LOWER-RIGHT     usage binary-long value 9.
          05 AG-WINDOW-LAST            usage binary-long value 10.

       01 progress-flags.
         05 AG-PROGRESS-BAR-HFILL	   USAGE binary-long value "01".
         05 AG-PROGRESS-BAR-VFILL	   USAGE binary-long value "02".
         05 AG-PROGRESS-BAR-SHOW-PCT   USAGE binary-long value "04".
         05 AG-PROGRESS-BAR-EXCL       USAGE binary-long value "08".
         05 AG-PROGRESS-EXPAND         USAGE binary-long value "03".
         
       01 box-types.
          05 AG-BOX-HORIZ              usage binary-long value 0.
          05 AG-BOX-VERT               usage binary-long value 1.

       01 box-flags.
          05 AG-BOX-HOMOGENOUS         usage binary-long value h'01'.
          05 AG-BOX-HFILL              usage binary-long value h'02'.
          05 AG-BOX-VFILL              usage binary-long value h'04'.
          05 AG-BOX-FRAME              usage binary-long value h'08'.
          05 AG-BOX-EXPAND             usage binary-long value h'06'.
          05 AG-BOX-DEFAULT            usage binary-long value h'06'.

       01 label-flags.
          05 AG-LABEL-HFILL            usage binary-long value h'01'.
          05 AG-LABEL-VFILL            usage binary-long value h'02'.
          05 AG-LABEL-NOMINSIZE        usage binary-long value h'04'.
          05 AG-LABEL-PARTIAL          usage binary-long value h'10'.
          05 AG-LABEL-REGEN            usage binary-long value h'20'.
          05 AG-LABEL-FRAME            usage binary-long value h'80'.
          05 AG-LABEL-EXPAND           usage binary-long value h'03'.
       01 button-flags.
          05 AG-BUTTON-STICKY          usage binary-long value h'0002'.
          05 AG-BUTTON-REPEAT          usage binary-long value h'0008'.
          05 AG-BUTTON-HFILL           usage binary-long value h'0010'.
          05 AG-BUTTON-VFILL           usage binary-long value h'0020'.
          05 AG-BUTTON-INVSTATE        usage binary-long value h'0400'.
          05 AG-BUTTON-KEYDOWN         usage binary-long value h'0800'.
          05 AG-BUTTON-EXCL            usage binary-long value h'1000'.
          05 AG-BUTTON-NOEXCL          usage binary-long value h'2000'.
          05 AG-BUTTON-EXPAND          usage binary-long value h'0030'.
          05 AG-BUTTON-DEFAULT         usage binary-long value h'2000'.
       01 wcombo-flags.
          05 AG-COMBO-POLL             usage binary-long value h'01'.
          05 AG-COMBO-TREE             usage binary-long value h'02'.
          05 AG-COMBO-ANY-TEXT         usage binary-long value h'04'.
          05 AG-COMBO-HFILL            usage binary-long value h'08'.
          05 AG-COMBO-VFILL            usage binary-long value h'10'.
          05 AG-COMBO-SCROLLTOSEL      usage binary-long value h'40'.
          05 AG-COMBO-EXPAND           usage binary-long value h'18'.
      
      
    
          
       01 console-flags.
          05 AG-CONSOLE-HFILL          usage binary-long value h'01'.
          05 AG-CONSOLE-VFILL          usage binary-long value h'02'.
          05 AG-CONSOLE-NOAUTOSCROLL   usage binary-long value h'04'.
          05 AG-CONSOLE-NOPOPUP        usage binary-long value h'08'.
          05 AG-CONSOLE-EXPAND         usage binary-long value h'03'.
          05 AG-CONSOLE-SELECTING      usage binary-long value h'10'.

       01 cpu-ext-bits.
             05 AG-EXT-CPUID usage binary-long value 1.
             05 AG-EXT-MMX usage binary-long value 2.
             05 AG-EXT-MMX-EXT usage binary-long value 4.
             05 AG-EXT-3DNOW usage binary-long value 8.
             05 AG-EXT-3DNOW-EXT usage binary-long value 16.
             05 AG-EXT-ALTIVEC usage binary-long value 32.
             05 AG-EXT-SSE usage binary-long value 64.
             05 AG-EXT-SSE2 usage binary-long value 128.
             05 AG-EXT-SSE3 usage binary-long value 256.
             05 AG-EXT-LONG-MODE usage binary-long value 512.
             05 AG-EXT-RDTSCP usage binary-long value 1024.
             05 AG-EXT-FXSR usage binary-long value 2048.
             05 AG-EXT-PAGE-NX usage binary-long value 4096.
             05 AG-EXT-SSE5A usage binary-long value 8192.
             05 AG-EXT-3DNOW-PREFETCH usage binary-long value 16384.
             05 AG-EXT-SSE-MISALIGNED usage binary-long value 32768.
             05 AG-EXT-SSE4A usage binary-long value 65536.
             05 AG-EXT-ONCHIP-FPU usage binary-long value 131072.
             05 AG-EXT-TSC usage binary-long value 262144.
             05 AG-EXT-CMOV usage binary-long value 524288.
             05 AG-EXT-CLFLUSH usage binary-long value 1048576.
             05 AG-EXT-HTT usage binary-long value 2097152.
             05 AG-EXT-MON usage binary-long value 4194304.
             05 AG-EXT-VMX usage binary-long value 8388608.
             05 AG-EXT-SSSE3 usage binary-long value 16777216.
             05 AG-EXT-SSE41 usage binary-long value 33554432.
             05 AG-EXT-SSE42 usage binary-long value 67108864.

       01 dirdlg-flags.
          05 AG-DIRDLG-MULTI           usage binary-long value h'001'.
          05 AG-DIRDLG-CLOSEWIN        usage binary-long value h'002'.
          05 AG-DIRDLG-LOAD            usage binary-long value h'004'.
          05 AG-DIRDLG-SAVE            usage binary-long value h'008'.
          05 AG-DIRDLG-ASYNC           usage binary-long value h'010'.
          05 AG-DIRDLG-RESET-ONSHOW    usage binary-long value h'020'.
          05 AG-DIRDLG-HFILL           usage binary-long value h'100'.
          05 AG-DIRDLG-VFILL           usage binary-long value h'200'.
          05 AG-DIRDLG-EXPAND          usage binary-long value h'300'.
          05 AG-DIRDLG-NOBUTTONS       usage binary-long value h'400'.

       01 editable-flags.
          05 AG-EDITABLE-HFILL         usage binary-long value h'00001'.
          05 AG-EDITABLE-VFILL         usage binary-long value h'00002'.
          05 AG-EDITABLE-EXPAND        usage binary-long value h'00003'.
          05 AG-EDITABLE-MULTILINE     usage binary-long value h'00004'.
          05 AG-EDITABLE-BLINK-ON      usage binary-long value h'00008'.
          05 AG-EDITABLE-PASSWORD      usage binary-long value h'00010'.
          05 AG-EDITABLE-ABANDON-FOCUS usage binary-long value h'00020'.
          05 AG-EDITABLE-INT-ONLY      usage binary-long value h'00040'.
          05 AG-EDITABLE-FLT-ONLY      usage binary-long value h'00080'.
          05 AG-EDITABLE-CATCH-TAB     usage binary-long value h'00100'.
          05 AG-EDITABLE-CURSOR-MOVING usage binary-long value h'00200'.
          05 AG-EDITABLE-KEEPVISCURSOR usage binary-long value h'00800'.
          05 AG-EDITABLE-MARKPREF      usage binary-long value h'02000'.
          05 AG-EDITABLE-EXCL          usage binary-long value h'04000'.
          05 AG-EDITABLE-NOEMACS       usage binary-long value h'08000'.
          05 AG-EDITABLE-NOLATIN1      usage binary-long value h'20000'.
          05 AG-EDITABLE-WORDWRAP      usage binary-long value h'40000'.
          05 AG-EDITABLE-NOPOPUP       usage binary-long value h'80000'.
          05 AG-EDITABLE-WORDSELECT   usage binary-long value h'100000'.
          05 AG-EDITABLE-READONLY     usage binary-long value h'200000'.
          05 AG-EDITABLE-MULTILINGUAL usage binary-long value h'400000'.

       01 exec-wait-types.
          05 AG-EXEC-WAIT-IMMEDIATE    usage binary-long value 0.
          05 AG-EXEC-WAIT-INFINITE     usage binary-long value 1.

       01 filedlg-flags.
          05 AG-FILEDLG-MULTI          usage binary-long value h'0001'.
          05 AG-FILEDLG-CLOSEWIN       usage binary-long value h'0002'.
          05 AG-FILEDLG-LOAD           usage binary-long value h'0004'.
          05 AG-FILEDLG-SAVE           usage binary-long value h'0008'.
          05 AG-FILEDLG-ASYNC          usage binary-long value h'0010'.
          05 AG-FILEDLG-RESET-ONSHOW   usage binary-long value h'0020'.
          05 AG-FILEDLG-HFILL          usage binary-long value h'0100'.
          05 AG-FILEDLG-VFILL          usage binary-long value h'0200'.
          05 AG-FILEDLG-EXPAND         usage binary-long value h'0300'.
          05 AG-FILEDLG-NOBUTTONS      usage binary-long value h'0400'.
          05 AG-FILEDLG-MASK-EXT       usage binary-long value h'0800'.
          05 AG-FILEDLG-MASK-HIDDEN    usage binary-long value h'1000'.
          05 AG-FILEDLG-NOMASKOPTS     usage binary-long value h'2000'.
          05 AG-FILEDLG-NOTYPESELECT   usage binary-long value h'4000'.

       01 fixed-flags.
          05 AG-FIXED-HFILL            usage binary-long value h'01'.
          05 AG-FIXED-VFILL            usage binary-long value h'02'.
          05 AG-FIXED-NO-UPDATE        usage binary-long value h'04'.
          05 AG-FIXED-FILLBG           usage binary-long value h'08'.
          05 AG-FIXED-BOX              usage binary-long value h'10'.
          05 AG-FIXED-INVBOX           usage binary-long value h'20'.
          05 AG-FIXED-FRAME            usage binary-long value h'40'.
          05 AG-FIXED-EXPAND           usage binary-long value h'03'.
          05 AG-FIXED-DEFAULT          usage binary-long value h'43'.

       01 graph-flags.
          05 AG-GRAPH-HFILL            usage binary-long value h'01'.
          05 AG-GRAPH-VFILL            usage binary-long value h'02'.
          05 AG-GRAPH-EXPAND           usage binary-long value h'03'.
          05 AG-GRAPH-SCROLL           usage binary-long value h'04'.
          05 AG-GRAPH-DRAGGING         usage binary-long value h'08'.
          05 AG-GRAPH-PANNING          usage binary-long value h'10'.
          05 AG-GRAPH-NO-MOVE          usage binary-long value h'20'.
          05 AG-GRAPH-NO-SELECT        usage binary-long value h'40'.
          05 AG-GRAPH-NO-MENUS         usage binary-long value h'80'.
          05 AG-GRAPH-READONLY         usage binary-long value h'E0'.

       01 graph-vertex-flags.
          05 AG-GRAPH-VERTEX-RECTANGLE usage binary-long value 0.
          05 AG-GRAPH-VERTEX-CIRCLE    usage binary-long value 1.


       01 plotter-types.
          05 AG-PLOTTER-POINTS         usage binary-long value 0.
          05 AG-PLOTTER-LINES          usage binary-long value 1.

       01 plotter-flags.
          05 AG-FIXED-PLOTTER-SCROLL   usage binary-long value 1.
          05 AG-FIXED-PLOTTER-XAXIS    usage binary-long value 2.
          05 AG-FIXED-PLOTTER-HFILL    usage binary-long value 4.
          05 AG-FIXED-PLOTTER-VFILL    usage binary-long value 8.
          05 AG-FIXED-PLOTTER-EXPAND   usage binary-long value 12.

       01 fontselector-flags.
          05 AG-FONTSELECTOR-UPDATE    usage binary-long value h'001'.
          05 AG-FONTSELECTOR-HFILL     usage binary-long value h'100'.
          05 AG-FONTSELECTOR-VFILL     usage binary-long value h'200'.
          05 AG-FONTSELECTOR-EXPAND    usage binary-long value h'300'.

       01 hsvpal-flags.
          05 AG-HSVPAL-PIXEL           usage binary-long value h'01'.
          05 AG-HSVPAL-DIRTY           usage binary-long value h'02'.
          05 AG-HSVPAL-HFILL           usage binary-long value h'04'.
          05 AG-HSVPAL-VFILL           usage binary-long value h'08'.
          05 AG-HSVPAL-NOALPHA         usage binary-long value h'10'.
          05 AG-HSVPAL-FORCE-NOALPHA   usage binary-long value h'10'.
          05 AG-HSVPAL-NOPREVIEW       usage binary-long value h'20'.
          05 AG-HSVPAL-SHOW-RGB        usage binary-long value h'40'.
          05 AG-HSVPAL-SHOW-HSV        usage binary-long value h'80'.
          05 AG-HSVPAL-EXPAND          usage binary-long value h'0C'.

    
       01 mpane-flags.
          05 AG-MPANE-HFILL            usage binary-long value h'01'.
          05 AG-MPANE-VFILL            usage binary-long value h'02'.
          05 AG-MPANE-FRAMES           usage binary-long value h'04'.
          05 AG-MPANE-FORCE-DIV        usage binary-long value h'08'.
          05 AG-MPANE-EXPAND           usage binary-long value h'03'.
          05 AG-MPANE-DEFAULT          usage binary-long value h'09'.


       01 mpane-layouts.
          05 AG-MPANE1                 usage binary-long value 00.
          05 AG-MPANE2V                usage binary-long value 01.
          05 AG-MPANE2H                usage binary-long value 02.
          05 AG-MPANE2L1R              usage binary-long value 03.
          05 AG-MPANE1L2R              usage binary-long value 04.
          05 AG-MPANE2T1B              usage binary-long value 05.
          05 AG-MPANE1T2B              usage binary-long value 06.
          05 AG-MPANE3L1R              usage binary-long value 07.
          05 AG-MPANE1L3R              usage binary-long value 08.
          05 AG-MPANE3T1B              usage binary-long value 09.
          05 AG-MPANE1T3B              usage binary-long value 10.
          05 AG-MPANE4                 usage binary-long value 11.

       01 notebook-flags.
          05 AG-NOTEBOOK-HFILL         usage binary-long value h'01'.
          05 AG-NOTEBOOK-VFILL         usage binary-long value h'02'.
          05 AG-NOTEBOOK-HIDE-TABS     usage binary-long value h'04'.
          05 AG-NOTEBOOK-EXPAND        usage binary-long value h'03'.

       01 numeri1cal-flags.
          05 AG-NUMERICAL-HFILL        usage binary-long value h'01'.
          05 AG-NUMERICAL-VFILL        usage binary-long value h'02'.
          05 AG-NUMERICAL-INT          usage binary-long value h'04'.
          05 AG-NUMERICAL-EXCL         usage binary-long value h'08'.
          05 AG-NUMERICAL-DEFAULT      usage binary-long value h'09'.

       01 socket-flags.
          05 AG-SOCKET-HFILL           usage binary-long value h'01'.
          05 AG-SOCKET-VFILL           usage binary-long value h'02'.
          05 AG-SOCKET-EXPAND          usage binary-long value h'03'.
          05 AG-SOCKET-MOUSEOVER       usage binary-long value h'04'.
          05 AG-SOCKET-STICKY_STATE    usage binary-long value h'08'.

       01 table-flags.
          05 AG-TABLE-MULTI            usage binary-long value h'001'.
          05 AG-TABLE-MULTITOGGLE      usage binary-long value h'002'.
          05 AG-TABLE-REDRAW-CELLS     usage binary-long value h'004'.
          05 AG-TABLE-POLL             usage binary-long value h'008'.
          05 AG-TABLE-HIGHLIGHT-COLS   usage binary-long value h'040'.
          05 AG-TABLE-WIDGETS          usage binary-long value h'080'.
          05 AG-TABLE-MULTIMODE        usage binary-long value h'003'.
          05 AG-TABLE-NOAUTOSORT       usage binary-long value h'100'.
          05 AG-TABLE-NEEDSORT         usage binary-long value h'200'.

       01 timer-flags.
          05 AG-TIMER-SURVIVE-DETACH   usage binary-long value h'01'.
          05 AG-TIMER-AUTO-FREE        usage binary-long value h'02'.
          05 AG-TIMER-EXECD            usage binary-long value h'04'.
          05 AG-TIMER-RESTART          usage binary-long value h'08'.

       01 textbox-flags.
          05 AG-TEXTBOX-MULTILINE     usage binary-long value h'000001'.
          05 AG-TEXTBOX-PASSWORD      usage binary-long value h'000004'.
          05 AG-TEXTBOX-ABANDON-FOCUS usage binary-long value h'000008'.
          05 AG-TEXTBOX-COMBO         usage binary-long value h'000010'.
          05 AG-TEXTBOX-HFILL         usage binary-long value h'000020'.
          05 AG-TEXTBOX-VFILL         usage binary-long value h'000040'.
          05 AG-TEXTBOX-EXPAND        usage binary-long value h'000030'.
          05 AG-TEXTBOX-READONLY      usage binary-long value h'000100'.
          05 AG-TEXTBOX-INT-ONLY      usage binary-long value h'000200'.
          05 AG-TEXTBOX-FLT-ONLY      usage binary-long value h'000400'.
          05 AG-TEXTBOX-CATCH-TAB     usage binary-long value h'000800'.
          05 AG-TEXTBOX-CURSOR-MOVING usage binary-long value h'001000'.
          05 AG-TEXTBOX-EXCL          usage binary-long value h'004000'.
          05 AG-TEXTBOX-NOEMACS       usage binary-long value h'008000'.
          05 AG-TEXTBOX-NOLATIN1      usage binary-long value h'020000'.
          05 AG-TEXTBOX-WORDWRAP      usage binary-long value h'040000'.
          05 AG-TEXTBOX-NOPOPUP       usage binary-long value h'080000'.
          05 AG-TEXTBOX-MULTILINGUAL  usage binary-long value h'100000'.
          05 AG-TEXTBOX-DEFAULT       usage binary-long value h'004000'.
          
       01 treetbl-flags.
          05 AG-TREETBL-MULTI         usage binary-long value h'001'.
          05 AG-TREETBL-MULTITOGGLE   usage binary-long value h'002'.
          05 AG-TREETBL-REORDERCOLS   usage binary-long value h'004'.
          05 AG-TREETBL-NODUPCHECKS   usage binary-long value h'008'.
          05 AG-TREETBL-SORT          usage binary-long value h'010'.
          05 AG-TREETBL-POLLED        usage binary-long value h'020'.
          05 AG-TREETBL-HFILL         usage binary-long value h'040'.
          05 AG-TREETBL-VFILL         usage binary-long value h'080'.
          05 AG-TREETBL-EXPAND        usage binary-long value h'0C0'.


      *> ***************************************************************
      *> Networking
       01 addr-families.
          05 AG-NET-AF-NONE            usage binary-long value 0.
          05 AG-NET-LOCAL              usage binary-long value 1.
          05 AG-NET-INET4              usage binary-long value 2.
          05 AG-NET-INET6              usage binary-long value 3.

       01 socket-types.
          05 AG-NET-SOCKET-NONE        usage binary-long value 0.
          05 AG-NET-STREAM             usage binary-long value 1.
          05 AG-NET-DGRAM              usage binary-long value 2.
          05 AG-NET-RAW                usage binary-long value 3.
          05 AG-NET-RDM                usage binary-long value 4.
          05 AG-NET-SEQPACKET          usage binary-long value 5.

       01 socket-poll-flags.
          05 AG-NET-POLL-READ          usage binary-long value 1.


       

       77 agar-debug                    pic x(10).

       01 tab-numbers.
        07 tab-alfa                    pic x(10).
        07 tab-num                     pic 9(10).
       
 
       77 local-buffer                 pic x(10) value space. 
       77 local-string                 pic x(100) based. 
       77 local                        usage pointer.
            copy "global".
       procedure division.

            ACCEPT agar-debug from environment "GuiCOBOLdebug".

            if agar-started not = agar-true
             and agar-function not = "initialize"

              perform inizializza      thru ex-inizializza.

            if function lower-case(agar-local-debug) = "y"
              move "enable"              to agar-debug.
            if function lower-case(agar-local-debug) = "n"
              move spaces              to agar-debug.


            if function lower-case(agar-debug) = "enable" 
               perform agar-do-debug      thru ex-agar-do-debug.

            
            evaluate function lower-case(agar-function)

            when "addform"      perform addform     thru ex-addform
            when "show"         perform set-visible thru ex-set-visible

            when "set-error"    perform set-error   thru ex-set-error
            when "set-warning"  perform set-warning thru ex-set-warning
            when "set-info"     perform set-info    thru ex-set-info
            when "set-caption"  perform set-caption thru ex-set-caption
            when "get-caption"  perform get-caption thru ex-get-caption
            when "set-width"    perform set-width   thru ex-set-width
            when "set-height"   perform set-height  thru ex-set-height

            when "set-value"    perform set-value   thru ex-set-value
            
            when "set-position" perform set-position 
                                                    thru ex-set-position
            when "set-disposition" perform set-disposition
                                                thru ex-set-disposition
            
            when "set-visible"  perform set-visible thru ex-set-visible
            when "set-invisible"  
                                perform set-invisible  
                                   thru ex-set-invisible
            when "hide"         perform set-invisible 
                                      thru ex-set-invisible
   
            when "addlabel"     perform addlabel    thru ex-addlabel
            when "addpane"      perform addpane     thru ex-addpane
            when "addprogress"  perform addprogress thru ex-addprogress
            when "addslider"    perform addslider   thru ex-addslider
            when "addsliderv"   perform addsliderv  thru ex-addsliderv
            when "addscroll"    perform addscroll   thru ex-addscroll
            when "addscrollv"   perform addscrollv  thru ex-addscrollv
            when "addscrollview"  
                                perform addscrollview   
                                                 thru ex-addscrollview

            when "addbutton"    perform addbutton   thru ex-addbutton
            when "addbuttonto"  perform addbuttonto thru ex-addbuttonto

            when "addcheck"     perform addcheck    thru ex-addcheck
            when "addcheckto"   perform addcheckto  thru ex-addcheckto

            when "additem"      perform additem     thru ex-additem

              
            when "addtext"      perform addtext     thru ex-addtext
            when "addnumeric"   perform addnumeric  thru ex-addnumeric
            when "addfixed"     perform addfixed    thru ex-addfixed
            when "addcombo"     perform addcombo    thru ex-addcombo
            when "addradio"     perform addradio    thru ex-addradio

            when "move"         perform moveto      thru ex-moveto
            when "size"         perform sizeto      thru ex-sizeto
            when "set-size"     perform sizeto      thru ex-sizeto
             
            when "setevent"     perform setevent    thru ex-setevent
            when "addevent"     perform addevent    thru ex-addevent

            when "addbox"       perform addbox      thru ex-addbox
            when "addframe"     perform addframe    thru ex-addframe
            
            when "get-class"    perform get-class   thru ex-get-class
            when "set-text"     perform set-text    thru ex-set-text
            when "get-text"     perform get-text    thru ex-get-text

            when "set-render"   perform set-render  thru ex-set-render
        
               
            when "set-name"     perform set-name    thru ex-set-name
            when "get-name"     perform get-name    thru ex-get-name
            when "get-focused"  perform get-focused thru ex-get-focused


            when "addproperty"  perform addproperty 
                                   thru ex-addproperty
            when "get-property" perform get-property 
                                   thru ex-get-property

            when "bindto"       perform bindto      thru ex-bindto

            when "refresh"      perform refresh     thru ex-refresh

            when other
             display "agarcob - command: '" 
              function trim(agar-FUNCTION)
                "'  not yet implemented in agarcob"
                        upon syserr
            end-evaluate.

            if function lower-case(agar-debug) = "enable"
             perform agar-after-debug    thru ex-agar-after-debug.

            

            exit program.

       inizializza.

            if agar-started not = agar-true

            call static "AG_InitCore" using null by value 0 returning rc
          *>    on exception display "error: no libagar " upon syserr
            end-call

            if rc = -1 then
             display "error: AG_InitCore failure" upon syserr
            goback
            end-if

            call "AG_InitGraphics" using by value 0 returning rc
            if rc = -1 then
             display "error: AG_InitGraphics failure" upon syserr
            goback
           end-if

            move agar-true  to agar-started.

            set agar-main   to null.


       ex-inizializza.
            exit.

       addform.

            call "AG_WindowNew" using by value 0 returning agar-object.

            if agar-main = null
              move agar-object to agar-main.

            move agar-object               to agar-form.
            move "DefaultDestroy"          to agar-procedure.
            set agar-callback              to entry "DefaultDestroy".
            move "window-detached" & X"00" to agar-event.
            move "setevent"                to agar-function.
            perform setevent               thru ex-setevent.

       ex-addform.
            exit.

       get-focused.
           
                call "AG_WidgetFindFocused"
                  using by value agar-object  returning agar-widget.
           
       ex-get-focused.
            exit.
            
       set-visible.

            if agar-widget = agar-form
                call "AG_WindowShow"
                using by value agar-widget returning omitted
                 go to ex-set-visible.

            if agar-debug = "enable"
             move "internal get-class"   to agar-function
              perform agar-do-debug      thru ex-agar-do-debug.

             perform get-class thru ex-get-class.

            if agar-debug = "enable"
             perform agar-after-debug    thru ex-agar-after-debug.


            evaluate agar-class
               when "form"

              call "AG_WindowShow"
                using by value agar-widget returning omitted

               when other
               
             call "AG_WidgetShow"
                using by value agar-widget returning omitted.
   
       ex-set-visible.
            exit.

       set-invisible.
    
             perform get-class thru ex-get-class.

            evaluate agar-class
               when "form"
                call "AG_WindowHide"
                using by value agar-widget returning omitted
                
               when other 
             call "AG_WidgetHide"
                using by value agar-widget returning omitted.
  
       ex-set-invisible.
            exit.

       setevent.
            
             move agar-event        to agar-text.
             perform asciiz         thru ex-asciiZ
             move agar-text         to agar-event.
            
            if agar-callback equal null then
              display "windown entry not found:"
              function trim(agar-procedure)
              else
                 call "AG_SetEvent" using
                 by value agar-object
                 by reference agar-event
                 by value agar-callback
                 by reference null
                  end-if.

             set agar-callback to null.
             move spaces       to agar-procedure.
             move spaces       to agar-event.
             
       ex-setevent.
            exit.

       addevent.
            
             move agar-event        to agar-text.
             perform asciiz         thru ex-asciiZ
             move agar-text         to agar-event.
            
            if agar-callback equal null then
              display "windown entry not found:"
              function trim(agar-procedure)
              else
                 call "AG_AddEvent" using
                 by value agar-object
                 by reference agar-event
                 by value agar-callback
                 by reference null
                  end-if.

             set agar-callback to null.
             move spaces       to agar-procedure.
             move spaces       to agar-event.
             
       ex-addevent.
            exit.


       refresh.
           
            if agar-debug = "enable"
             move "internal get-class"   to agar-function
              perform agar-do-debug      thru ex-agar-do-debug.

           
            perform get-class thru ex-get-class.

            evaluate agar-class
               when "form"
               CALL static  "AG_WindowUpdate" 
                using by value agar-object
                 returning omitted
               
               when other 

               CALL  STATIC  "ag_widget_update" 
                using by value agar-object
                  returning omitted
            end-evaluate.             
            

       ex-refresh.
            exit.
            
       set-value.

            if agar-debug = "enable"
             move "internal get-class"   to agar-function
              perform agar-do-debug      thru ex-agar-do-debug.

            perform get-class thru ex-get-class.

            if agar-debug = "enable"
             perform agar-after-debug    thru ex-agar-after-debug.
           
              move "set-value"           to agar-function
         
            evaluate agar-class
               when "progress"  
           
             
            call static "AG_BindInt" using
               by value agar-object
                  by reference z"value"
                   by reference agar-int
            end-call     

               when "check"
                       
               call static "AG_CheckboxSetState" using
               by value  
                 agar-object                    
                  by value
                     agar-int
                   end-call
                  
               when other display agar-class
            end-evaluate.
                  
                  
       ex-set-value.
            exit.

       get-value.
           
            if agar-debug = "enable"
             move "internal get-class"   to agar-function
              perform agar-do-debug      thru ex-agar-do-debug.

            perform get-class           thru ex-get-class.

            if agar-debug = "enable"
             perform agar-after-debug    thru ex-agar-after-debug.
           
              move "get-value"           to agar-function
  
            evaluate agar-class
            
               when "check"

            call static "AG_CheckboxGetState" using
               by value  
                 agar-object                  
                   returning 
                     agar-int                   
                end-call


                  
               when other display agar-class
            end-evaluate.
                 
       ex-get-value.
            exit.          
            
       set-caption.

            perform asciiZ  thru ex-asciiZ.

            if agar-debug = "enable"
             move "internal get-class"   to agar-function
              perform agar-do-debug      thru ex-agar-do-debug.
           


            perform get-class thru ex-get-class.

            if agar-debug = "enable"
             perform agar-after-debug    thru ex-agar-after-debug.

            move "set-caption"           to agar-function
       
            evaluate agar-class
               when "form"

               move agar-form           to agar-object

               CALL  "AG_WindowSetCaptionS"
                using by value agar-object
                by reference agar-text
               
               when "box"

               call static "AG_BoxSetLabelS" using by value 
                agar-object 
                  by reference agar-text

               when "label"

               call static "AG_LabelText" using by value 
                agar-object 
                  by reference agar-text

               when "text"

               call static "AG_TextboxSetLabelS" using by value 
                agar-object 
                  by reference agar-text

               when "button"
                  call static "AG_ButtonText" using by value
                   agar-object 
                  by reference agar-text

               WHEN other
                
                DISPLAY "caption is not defined for"
                      function trim(agar-class) upon syserr

            end-evaluate.

            
            move agar-text         to agar-value
        
            call "AG_SetString" using
               by value agar-object
                  by reference z"caption"
                   by reference agar-value
                     returning  omitted.
            

       ex-set-caption.
            exit.

       
       addbox.
 
           call static "AG_BoxNewHoriz" using
               by value agar-object
                 BY VALUE 10
      *              by value AG-BOX-HFILL
      *              by value AG-BOX-HOMOGENOUS
      *            by value AG-BOX-HOMOGENOUS
      *             by value AG-BOX-HFILL
      *            by value AG-BOX-VFILL
      *           by value AG-BOX-FRAME
      
               returning agar-widget.
               
       ex-addbox.
            exit.

       addframe.
 
           call static "AG_BoxNewVert" using
               by value agar-object
                 BY VALUE 12
      *              by value AG-BOX-HOMOGENOUS
      *            by value AG-BOX-HOMOGENOUS
      *             by value AG-BOX-HFILL
      *            by value AG-BOX-VFILL
      *           by value AG-BOX-FRAME
      
               returning agar-widget.
               
       ex-addframe.
            exit.



       addlabel.

            PERFORM asciiZ thru ex-asciiz.
                 
      *      call static "AG_LabelNew" using
             call  "AG_LabelNew" using
               by value agar-object 0
                    by reference agar-text 
                     returning agar-widget.
               
                   call static "AG_LabelSizeHint"
                   using by value agar-widget 
                    by reference agar-text.
               
       ex-addlabel.
            exit.

       addprogress.

            call static "AG_ProgressBarNewHoriz" using
               by value agar-object
                by value 13
                     returning agar-widget.
               
       ex-addprogress.
            exit.

       addslider.

             call static "AG_SliderNew" using
               by value agar-object
                by value 0
                by value 9
                     returning agar-widget.

                     
               
       ex-addslider.
            exit.

       addsliderv.

            call static "AG_SliderNew" using
               by value agar-object
                by value 1
                by value 9
                     returning agar-widget.
               
       ex-addsliderv.
            exit.


       addscroll.

             call static "AG_ScrollbarNewHoriz" using
               by value agar-object
                   by value 5
                     returning agar-widget.

       ex-addscroll.
            exit.

       addscrollview.

             call static "AG_ScrollviewNew" using
               by value agar-object
                   by value  3
                     returning agar-widget.

       ex-addscrollview.
            exit.


       addscrollv.

             call static "AG_ScrollbarNewVert" using
               by value agar-object
                   by value 5
                     returning agar-widget.

       ex-addscrollv.
            exit.




       addpane.

            PERFORM asciiZ thru ex-asciiz.
                 
            if function upper-case(agar-text(1:1)) = "H"
            call static "AG_PaneNewHoriz" using
               by value agar-object
                   by value AG-MPANE-EXPAND
                     returning agar-widget
                 else               

             call static "AG_PaneNewVert" using
               by value agar-object
                   by value AG-MPANE-EXPAND
                     returning agar-widget.
               

               set address of pane-instance  to agar-widget.
               
               display function length(pane-instance)
               
                display function length(pane-type)
                display function length(pane-flags)
               
            
               move pane-divs(1)               to agar-pane-one
               move pane-divs(2)               to agar-pane-two.
                
            
       ex-addpane.
            exit.


      *
      * usage: move  "format string" to "render" of object
      *
       set-render.

            PERFORM asciiZ thru ex-asciiZ.     

            move agar-widget       to agar-object

            perform get-class      thru ex-get-class.
            
            move zero to ind
            
            inspect agar-text tallying ind for all "("
            inspect agar-text tallying ind for all ")"
            if ind = 2
             perform varying ind from function length(agar-text) 
              by -1 until ind = zero
             or agar-text(ind:1) = ")"
             continue
             end-perform
             subtract 1 from ind
             move zeros to tab-num
             move length of tab-num to ind2
             perform varying ind from ind by -1 until ind = zeros
             or agar-text(ind:1) = "("
             move agar-text(ind:1) to tab-num(ind2:1)
             subtract 1 from ind2
             end-perform
             
             if tab-num > 100 move 100 to tab-num.
             perform varying ind from 1 by 1 until ind > tab-num
             move "x"       to agar-text(ind:1)
             end-perform.
             


            evaluate agar-class
               
               when "text"

               call static "AG_TextboxSizeHint"
                using by value agar-widget 
                 by content agar-text 
             
               when "label"
                  
                  call static "AG_LabelSizeHint"
                   using by value agar-widget 
                    by content agar-text
                  end-call
               
               when "combo"
                  
                  call static "AG_ComboSizeHint"
                   using by value agar-widget 
                    by content agar-text
                    by value 10
                  end-call


               when other  
                  display "unrecognized render call " 
                   function trim (agar-text) upon syserr
                  continue

            end-evaluate.             
            
       ex-set-render.
            exit.    

     

       addfixed.

                 
            call static "AG_FixedNew" using
               by value agar-object
                by value AG-FIXED-DEFAULT
                   returning agar-widget.
             
            move agar-widget   to agar-fixed.
            
          *>   call static "AG_FixedSizeHint" using
          *>      by value agar-widget
          *>         by value agar-x
          *>          by value agar-y 
          *>              returning omitted.
               
       ex-addfixed.
            exit.

       addradio.

            call static "AG_RadioNew" using
                by value agar-object
                by value agar-null-pointer
                      returning agar-widget.
                           
       ex-addradio.
            exit.


       addbutton.

            PERFORM asciiZ thru ex-asciiZ.     
            
            call static "AG_ButtonNewS" using
               by value agar-object
      *                by value AG-BUTTON-HFILL
      *            by value AG-BUTTON-NOEXCL 
                    by value AG-BUTTON-HFILL
              *>   by value AG-BUTTON-DEFAULT
                  by content  agar-text 
                     returning agar-widget.
               
       ex-addbutton.
            exit.
       
       
       addcheck.

            PERFORM asciiZ thru ex-asciiZ.     
            
            call static "AG_CheckboxNewS" using
               by value agar-object
                 by value AG-BOX-HFILL
                  by content  agar-text 
                     returning agar-widget.
               
       ex-addcheck.
            exit.
       
       
       additem.

             move agar-widget       to agar-object
            
             perform get-class thru ex-get-class.

             PERFORM asciiZ thru ex-asciiZ.    
            
             evaluate agar-class

                when "radio" 
            
               call "AG_RadioAddItemS" using
                by value agar-widget
                  by content  agar-text  
                     returning omitted
                end-call
                
                when "combo"

                    
              
               set address of combo-instance  to agar-widget 

            *> display "combo" function length(combo-instance)
            *>    display "combo-flags" function length(combo-flags)
            *>    display "combo-tbox" function length(combo-tbox)
            *>    display "combo-tlist" function length(combo-tlist)
            *>    display "combo-wsaved" function length(combo-wsaved)
            *>    display "combo-hsaved" function length(combo-hsaved)
            *>    display "combo-wprelist" function length(combo-wprelist)
            *>    display "combo-hprelist" function length(combo-hprelist)

               call static "AG_TlistAddS" using
                by value combo-tlist
                 by value agar-null-pointer
                  by content  agar-text  
                     returning omitted
                end-call
           
                  when other display 
                   " additem not yet implemented for "
                        function trim(agar-class)
                   
              end-evaluate.
               
       ex-additem.
            exit.
       
        
       addtext.

            PERFORM asciiZ thru ex-asciiZ.     
            
            call static "AG_TextboxNewS" using
               by value agar-object
                  by value AG-TEXTBOX-DEFAULT 
                  by content  agar-text 
                     returning agar-widget.
 
           call static "AG_TextboxSizeHint"
            using by value agar-widget 
             by reference agar-text.
             
                       
       ex-addtext.
            exit.
       
        
       addnumeric.

            PERFORM asciiZ thru ex-asciiZ.     
            
            call static "AG_NumericalNewS" using
               by value agar-object
                  by value AG-NUMERICAL-DEFAULT 
                   by value 0
                    by content  agar-text 
                     returning agar-widget.
                 
       ex-addnumeric.
            exit.
       
       
       
       addcombo.

            PERFORM asciiZ thru ex-asciiZ.     
            
            call static "AG_ComboNewS" using
               by value agar-object
                  by value AG-COMBO-HFILL
                   by content  agar-text 
                     returning agar-widget.
 
           call static "AG_ComboSizeHint"
            using by value agar-widget 
             by content agar-text
             by value 3
               returning omitted.

           
           
       ex-addcombo.
            exit.
       
       addcheckto.

             PERFORM addcheck              thru ex-addcheck.
             
             move agar-widget              to agar-object
             set agar-callback             to entry agar-procedure.
                                              
             move "checkbox-changed" & X"00"  to agar-event.
             move "setevent"               to agar-function.
             perform setevent              thru ex-setevent.
                   
       ex-addcheckto.
            exit.
     
       addbuttonto.

             PERFORM addbutton             thru ex-addbutton.
             
             move agar-widget              to agar-object
             set agar-callback             to entry agar-procedure.
                                              
             move "button-pushed" & X"00"  to agar-event.
             move "setevent"               to agar-function.
             perform setevent              thru ex-setevent.
                   
       ex-addbuttonto.
            exit.


       set-name.

            PERFORM asciiZ thru ex-asciiZ.
                 
            call static "AG_ObjectSetName" using
               by value agar-object
                  by reference   agar-text 
                     returning  omitted.
       
       ex-set-name.
            exit. 


       set-position.

           evaluate function upper-case(agar-text)
              when  "NONE"            MOVE 0 TO agar-int
              when  "TOPLEFT"         MOVE 1 TO agar-int
              when  "TOPCENTER"       MOVE 2 TO agar-int
              when  "MIDDLERIGHT"     MOVE 3 TO agar-int
              when  "MIDDLELEFT"      MOVE 4 TO agar-int
              when  "MIDDLECENTER"    MOVE 5 TO agar-int
              when  "MIDDLERIGHT"     MOVE 6 TO agar-int
              when  "BOTTONLEFT"      MOVE 7 TO agar-int
              when  "BOTTONCENTER"    MOVE 8 TO agar-int
              when  "BOTTONPRIGHT"    MOVE 9 TO agar-int
              when  "LAST"            MOVE 10 TO agar-int
                 
           end-evaluate.

             if agar-text(1:2) numeric
                move zeros             to agar-binary
                move agar-text(1:2)    to agar-binary(4:2)
                   move agar-binary    to agar-int                               
             else
             if agar-text(1:1) numeric
                move zeros             to agar-binary
                move agar-text(1:1)    to agar-binary(5:1)
                 move agar-binary    to agar-int.           
                
            call static "AG_WindowSetPosition" using
               by value agar-object
                  by value agar-int 
                   by value 1
                     returning  omitted.
       
       ex-set-position.
            exit. 



       set-disposition.

           evaluate function upper-case(agar-text) (1:1)
              when  "V"            MOVE 0 TO agar-int
              when  OTHER          MOVE 1 TO agar-int
                 
           end-evaluate.

           call static "AG_RadioSetDisposition" using
               by value agar-object
                  by value agar-int 
                    returning  omitted.
       
       ex-set-disposition.
            exit. 


       get-name.

               call static "AG_ObjectGetName" 
                using by value agar-object
                  returning agar-struct.
                
                set address of agar-string  to null.

                set address of agar-string  TO agar-struct.

                move agar-string      to agar-text.
                     
       ex-get-name.
            exit. 


       addproperty.

            move spaces        to agar-value.

            PERFORM asciiZ thru ex-asciiZ.
                
            call static "AG_SetString" using
               by value agar-widget
                  by reference agar-text
                   by reference agar-value
                     returning  agar-dummy.
                                        
       ex-addproperty.
            exit. 


       get-property.
           
           PERFORM asciiZ thru ex-asciiZ.

           move spaces                to agar-value
           move  LENGTH OF agar-value to agar-int

           call static "ag_defined" using
              by value agar-widget
                 by reference agar-text
                  returning agar-boolean.

            
           if agar-boolean = 1
            call static "AG_GetString" using
               by value agar-widget
                 by reference agar-text
                  by reference agar-value
                     by reference agar-int
                      returning agar-size
                       else
                       display "get string non allowed for "
                           agar-text " with object:"
                               agar-widget.
                     
       ex-get-property.
            exit. 

       get-caption.

               move "caption"     to agar-text.

               perform get-property thru ex-get-property.

       ex-get-caption.
            exit.
       


       get-text.

          *>     call static "AG_TextboxDupString" 
          *>       using by value agar-widget
          *>          returning agar-struct.
                  
          *>         move spaces to agar-text.
                              
          *>         set address of agar-string  TO agar-struct.
          *>         move agar-string            to agar-text.
                 
          *>         PERFORM asciiZ thru ex-asciiZ.   
          *>         display "1 nuova=" agar-text(1:40).
          
                move 50 to agar-size
                move spaces        to agar-text
                call static "AG_TextboxCopyString"
                 using by value agar-widget
                  by reference agar-text
                    by value agar-size
                    returning agar-int.
   
                    
          *>    move length of agar-text   to agar-size
          *>    move "federico"                 to agar-text
             
          *>    call static "AG_TextboxBindASCII" 
          *>     using by value agar-widget
          *>           by reference agar-text
          *>           by reference agar-size.
              
          *>   display "valore=" agar-text(1:20).
            
       ex-get-text.
            exit. 
      
      
      
       bindto.
          
             move length of agar-text   to agar-size
             
             call static "AG_TextboxBindASCII" 
              using by value agar-widget
                    by reference agar-text
                    by reference agar-size.
                         
       ex-bindto.
            exit.
            
       set-text.

            PERFORM asciiZ thru ex-asciiZ.   
                
            call static "AG_TextboxSetString" 
            using   by value agar-widget
                    by content agar-text.

       ex-set-text.
            exit. 

       
     

       moveto.
           
            call static "AG_FixedMove" 
              using by value agar-fixed
                by value agar-widget
                 by value agar-x
                  by value agar-y
                    returning omitted.
           
          *>   call static "AG_WidgetSetPosition" using
          *>      by value agar-widget
          *>         by value agar-x
          *>          by value agar-y 
          *>              returning omitted.
                   
         
       ex-moveto.
            exit.

       sizeto.
           
            move agar-widget       to agar-object

            perform get-class      thru ex-get-class.

            evaluate agar-class

               when "form"
           
		     call "AG_WindowSetGeometryAligned" 
		      using by value agar-form 0 agar-width agar-height 
               returning omitted
               end-call
               
               when "text"

               call static "AG_TextboxSizeHintPixels"
                using by value agar-widget 
                  agar-width agar-height
               end-call
               
               when "label"

                compute agar-size = agar-width / 10
                 on size error move 100 to agar-size
                end-compute
                
                move spaces to agar-text
                
                if agar-size < function length (agar-text)
                move X"00" to agar-text(agar-size:)
                move X"00" to agar-text(100:)
                end-if
                  
                call static "AG_LabelSizeHint" using
                   by value agar-widget
                      by reference agar-text
                           returning omitted
                end-call
           
           
               when "slider"

               call static "AG_SliderSetControlSize"
                using by value agar-widget 
                  agar-width 
               end-call
                            
             
               when other  
               
               call static "AG_WidgetSetSize" using
                   by value agar-widget
                      by value agar-width
                       by value  agar-height
                           
                end-call
                
            end-evaluate.             
                   
       ex-sizeto.
            exit.

       set-width.
           
           call static "AG_WidgetSetSize" using
               by value agar-widget
                  by reference agar-width
                   by reference agar-use-height.
                   
       ex-set-width.
            exit.


       set-height.
           
           call static "AG_WidgetSetSize" using
               by value agar-widget
                  by reference agar-use-width
                   by reference agar-height.
                   
       ex-set-height.
            exit.

       set-font.

           if agar-text = spaces move "Courier" to agar-text.
          
           call static "AG_SetStyle" using
               by value agar-widget
                  by reference "font-family"
                   by reference agar-text.
       
            
       ex-set-font.
            exit.
       

       set-fontstyle.

            call static "AG_SetStyle" using
               by value agar-widget
                  by reference  "font-style"
                   by reference  agar-text.
                    
       ex-set-fontstyle.
            exit.
            
       set-fontweight.
           
            evaluate function upper-case(agar-text) 
               when "BOLD"     move "bold"    to local-buffer
               when "NORMAL"   move "normal"  to local-buffer
               when "!PARENT"  move "!parent" to local-buffer
               when OTHER      move "normal"  to local-buffer
            end-evaluate
       
            call static "AG_SetStyle" using
               by value agar-widget
                  by reference  "font-weight"
                   by reference  "bold".
                    
       ex-set-fontweight.
            exit.


       set-bordercolor.

           move agar-text to agar-color

            call static "AG_SetStyle" using
               by value agar-widget
                  by reference  "border-color"
                   by reference  agar-color.
                    
                    
       ex-set-bordercolor.
            exit.

            
       set-fontsize.

           if agar-text = spaces move "10pts" to agar-text.
          
           call static "AG_SetStyle" using
               by value agar-widget
                  by content "font-size"
                  by content  agar-text 
                   returning omitted.
           
       ex-set-fontsize.
            exit.

       set-error.

       
           if agar-text = spaces
              move z"Messaggio di errore" to agar-text.

           PERFORM asciiZ thru ex-asciiZ.                 
          
           call static "AG_TextError" using
              by content  agar-text.

       ex-set-error.
            exit.

       set-warning.


           PERFORM asciiZ thru ex-asciiZ.                 
          
           if agar-text = spaces
              move z"Avviso generico per l'operatore" to agar-text.
          
           call static "AG_TextWarning" using
              by content  Z"Segnalazione per l'operatore"
              by reference  agar-text.

       ex-set-warning.
            exit.

       set-info.

           PERFORM asciiZ thru ex-asciiZ.                 

           if agar-text = spaces
              move z"informazioni per l'operatore" to agar-text.
          
           call static "AG_TextMsg" using
              by value 2
              by reference  agar-text.
                          
       ex-set-info.
            exit.



       
       size-agar-text.

            perform varying agar-int from length of agar-text by -1
             until agar-int = zeros
             or ( agar-text(agar-int:1) > spaces
             and agar-text(agar-int:1) not = x"00" )
             continue
            end-perform.

       ex-size-agar-text.
            exit.

       asciiZ.

            perform size-agar-text      thru ex-size-agar-text

            add 1 to agar-int.
            move X"00" to agar-text(agar-int:1).

       ex-asciiZ.
            exit.

       compute-size.

            perform size-agar-text      thru ex-size-agar-text.

            if agar-int = zeros move 1 to agar-int.

            if agar-int < 5
            compute agar-width = agar-int * 10
            else
            if agar-int > 10
            compute agar-width = agar-int * 6
            else
            compute agar-width = agar-int * 12.

      *      perform is-check-button thru ex-is-check-button.

            if agar-boolean = agar-true
              add 15         to agar-width.

            perform setsize  thru ex-setsize.

       ex-compute-size.
            exit.

       setsize.

            move agar-width   to agar-use-width.
            move agar-height  to agar-use-height.

            if agar-use-width  = zeros go to ex-setsize.
            if agar-use-height = zeros go to ex-setsize.


            move zeros       to agar-use-width.
            move zeros       to agar-use-height.

       ex-setsize.
            exit.
    
 
       get-class.
               
                call "AG_ObjectGetClassName" 
                     using by value agar-object 
                       by value 0
                       returning local
                       
                  set address of local-string  TO local.
            
                  Evaluate  function upper-case( local-string(4:3))
                  
                   when "WIN"      MOVE "form"         to agar-class
                   when "BOX"      MOVE "box"          to agar-class
                   when "LAB"      MOVE "label"        to agar-class
                   when "TEX"      MOVE "text"         to agar-class
                   when "RAD"      MOVE "radio"        to agar-class
                   when "CHE"      MOVE "check"        to agar-class
                   when "COM"      MOVE "combo"        to agar-class
                   when "BUT"      MOVE "button"       to agar-class
                   when "PAN"      MOVE "pane"         to agar-class
                   when "PRO"      MOVE "progress"     to agar-class
                   when "SLI"      MOVE "slider"       to agar-class
                   when "SCR"      MOVE "scroll"       to agar-class
                   when other 
                     move 
                      agar-string(4:3)         to agar-class
                        display "new class -->" agar-class
                                              
                  end-evaluate.                   
                
       ex-get-class.
               exit.


       agar-do-debug.

            Display "---------------------------------------------".

            if agar-started not = agar-true
             and agar-function not = "initialize"
             Display "agarcob starts:does automatic initialize"
             else
             Display "agarcob executes  :" function  trim(agar-function).

             perform trace-debug thru ex-trace-debug.

       ex-agar-do-debug.
             exit.

       trace-debug.

             Display "agar-form:             " agar-form.
             Display "agar-main:             " agar-main.
             Display "agar-panel:            " agar-panel.
             Display "agar-object:           " agar-object.
             Display "agar-parent:           " agar-parent.
             Display "agar-event:            " agar-event.
             Display "agar-procedure:        " agar-procedure.
             Display "agar-widget:           " agar-widget.
             Display "agar-text:             "
               function trim(agar-text).
             Display "agar-x:                "  agar-x.
             Display "agar-y:                "  agar-y.
             Display "agar-width:            "  agar-width.
             Display "agar-heigth:           "  agar-height.
             Display "agar-boolean:          "  agar-boolean.
             Display "agar-int:              "  agar-int.
             Display "agar-ind:              "  agar-ind.
             Display "agar-class:             "
               function trim(agar-class).
             
       ex-trace-debug.
            exit.

       agar-after-debug.

            if agar-started not = agar-true
             and agar-function not = "initialize"
             Display "agarcob after:automatic initialize"
             else
             Display "agarcob after:       "
             function  trim(agar-function).

             perform trace-debug thru ex-trace-debug.

             Display "agarcob has processed:        "
             function  trim(agar-function).

             Display " ".
             Display " ".
             Display " ".
             Display "             *** end:"
             function  trim(agar-function) " "
              function trim("***").

       ex-agar-after-debug.
            exit.

       end program agarcob.

       identification division.
       program-id. DefaultDestroy.
       data division.
       working-storage section.

            copy "global".

       linkage section.

       procedure division .


               call "AG_Terminate" using by value 0.

       exit program.
       end program DefaultDestroy.
