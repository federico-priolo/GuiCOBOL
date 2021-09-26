000010 identification division.
000020 program-id. agarcob is recursive.
000030 
000040*
000050* agar connector for GNUCOBOL
000060*
000070* FIRST 1th AUGUST   0.1.0  LAST 26.09.2021
000080*
000090* Copyright (C) 2012-2021 Federico Priolo TP ONE SRL
000100*
000110* This program is free software; you can redistribute it and/or modify
000120* it under the terms of the GNU General Public License as published by
000130* the Free Software Foundation; either version 2, or (at your option)
000140* any later version.
000150*
000160* This program is distributed in the hope that it will be useful,
000170* but WITHOUT ANY WARRANTY; without even the implied warranty of
000180* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
000190* GNU General Public License for more details.
000200*
000210* You should have received a copy of the GNU General Public License
000220* along with this software; see the file COPYING.  If not, write to
000230* the Free Software Foundation, 51 Franklin Street, Fifth Floor
000240* Boston, MA 02110-1301 USA
000250*
000260*
000270*---------------------------------------------------------------------
000280 data division.
000290
000300 working-storage section.
000310 01 ind                      usage binary-long unsigned.
000320 01 ind2                     usage binary-long unsigned.
000330 
000340 01 pane-instance based.  
000350     05 filler                    pic x(712). 
000350*    05 filler                    pic x(724).  linux
000350*     05 filler                    pic x(720).    debug windows
000360    
000370    05 pane-type                 usage binary-long sync.
000380    05 pane-flags                usage binary-long unsigned.
000390
000400    05 pane-divs             occurs 2 times usage pointer sync.



        01 menu-instance based.
          05 filler                    pic x(712).
          05 menu-flags                usage binary-long sync.
          05 menu-style                usage binary-long sync.
          05 menu-root                 usage pointer sync.
          

000410 
000420 
000430 01 combo-instance based.
000440   04 filler                    pic x(712).
000450   
000460   04 resto-combo.
000470    05 combo-flags               usage binary-long sync.
000480    05 combo-tbox                usage pointer     sync.
000490    05 combo-button              usage pointer.
000500    05 combo-tlist               usage pointer.
000510    05 combo-panel               usage pointer.
000520    05 combo-wsaved              usage binary-long sync.
000530    05 combo-hsaved              usage binary-long.
000540    05 combo-wprelist            usage binary-long.
000550    05 combo-hprelist            usage binary-long.
000560
000570
000580
000590  01 window-instance based.
000600   04 filler                     pic x(640).
000610   04 resto-win.
000620    05 win-flags                 usage binary-long sync.
000630    
000640
000650 01 object-flags.
000660    05 AG-OBJECT-FLOATING-VARS   usage binary-long value h'00001'.
000670    05 AG-OBJECT-NON-PERSISTENT  usage binary-long value h'00002'.
000680    05 AG-OBJECT-INDESTRUCTIBLE  usage binary-long value h'00004'.
000690    05 AG-OBJECT-RESIDENT        usage binary-long value h'00008'.
000700    05 AG-OBJECT-PRESERVE-DEPS   usage binary-long value h'00010'.
000710    05 AG-OBJECT-STATIC          usage binary-long value h'00020'.
000720    05 AG-OBJECT-READONLY        usage binary-long value h'00040'.
000730    05 AG-OBJECT-WAS-RESIDENT    usage binary-long value h'00080'.
000740    05 AG-OBJECT-REOPEN-ONLOAD   usage binary-long value h'00200'.
000750    05 AG-OBJECT-REMAIN-DATA     usage binary-long value h'00400'.
000760    05 AG-OBJECT-DEBUG           usage binary-long value h'00800'.
000770    05 AG-OBJECT-NAME-ONATTACH   usage binary-long value h'01000'.
000780    05 AG-OBJECT-CHLD-AUTOSAVE   usage binary-long value h'02000'.
000790    05 AG-OBJECT-DEBUG-DATA      usage binary-long value h'04000'.
000800    05 AG-OBJECT-INATTACH        usage binary-long value h'08000'.
000810    05 AG-OBJECT-INDETACH        usage binary-long value h'10000'.
000820*>  05 AG-OBJECT-SAVED-FLAGS     usage binary-long value h'G_OBJ'.
000830*>  05 AG-OBJECT-DUPED-FLAGS     usage binary-long value h'G_OBJ'.
000840
000850 01 window-positions.
000860    05 AG-WINDOW-NONE            usage binary-long value 0.
000870    05 AG-WINDOW-UPPER-LEFT      usage binary-long value 1.
000880    05 AG-WINDOW-UPPER-CENTER    usage binary-long value 2.
000890    05 AG-WINDOW-UPPER-RIGHT     usage binary-long value 3.
000900    05 AG-WINDOW-MIDDLE-LEFT     usage binary-long value 4.
000910    05 AG-WINDOW-CENTER          usage binary-long value 5.
000920    05 AG-WINDOW-MIDDLE-RIGHT    usage binary-long value 6.
000930    05 AG-WINDOW-LOWER-LEFT      usage binary-long value 7.
000940    05 AG-WINDOW-LOWER-CENTER    usage binary-long value 8.
000950    05 AG-WINDOW-LOWER-RIGHT     usage binary-long value 9.
000960    05 AG-WINDOW-LAST            usage binary-long value 10.
000970
000980 01 progress-flags.
000990   05 AG-PROGRESS-BAR-HFILL	   USAGE binary-long value "01".
001000   05 AG-PROGRESS-BAR-VFILL	   USAGE binary-long value "02".
001010   05 AG-PROGRESS-BAR-SHOW-PCT   USAGE binary-long value "04".
001020   05 AG-PROGRESS-BAR-EXCL       USAGE binary-long value "08".
001030   05 AG-PROGRESS-EXPAND         USAGE binary-long value "03".
001040   
001050 01 box-types.
001060    05 AG-BOX-HORIZ              usage binary-long value 0.
001070    05 AG-BOX-VERT               usage binary-long value 1.
001080
001090 01 box-flags.
001100    05 AG-BOX-HOMOGENOUS         usage binary-long value h'01'.
001110    05 AG-BOX-HFILL              usage binary-long value h'02'.
001120    05 AG-BOX-VFILL              usage binary-long value h'04'.
001130    05 AG-BOX-FRAME              usage binary-long value h'08'.
001140    05 AG-BOX-EXPAND             usage binary-long value h'06'.
001150    05 AG-BOX-DEFAULT            usage binary-long value h'06'.
001160
001170 01 label-flags.
001180    05 AG-LABEL-HFILL            usage binary-long value h'01'.
001190    05 AG-LABEL-VFILL            usage binary-long value h'02'.
001200    05 AG-LABEL-NOMINSIZE        usage binary-long value h'04'.
001210    05 AG-LABEL-PARTIAL          usage binary-long value h'10'.
001220    05 AG-LABEL-REGEN            usage binary-long value h'20'.
001230    05 AG-LABEL-FRAME            usage binary-long value h'80'.
001240    05 AG-LABEL-EXPAND           usage binary-long value h'03'.
001250 01 button-flags.
001260    05 AG-BUTTON-STICKY          usage binary-long value h'0002'.
001270    05 AG-BUTTON-REPEAT          usage binary-long value h'0008'.
001280    05 AG-BUTTON-HFILL           usage binary-long value h'0010'.
001290    05 AG-BUTTON-VFILL           usage binary-long value h'0020'.
001300    05 AG-BUTTON-INVSTATE        usage binary-long value h'0400'.
001310    05 AG-BUTTON-KEYDOWN         usage binary-long value h'0800'.
001320    05 AG-BUTTON-EXCL            usage binary-long value h'1000'.
001330    05 AG-BUTTON-NOEXCL          usage binary-long value h'2000'.
001340    05 AG-BUTTON-EXPAND          usage binary-long value h'0030'.
001350    05 AG-BUTTON-DEFAULT         usage binary-long value h'2000'.
001360 01 wcombo-flags.
001370    05 AG-COMBO-POLL             usage binary-long value h'01'.
001380    05 AG-COMBO-TREE             usage binary-long value h'02'.
001390    05 AG-COMBO-ANY-TEXT         usage binary-long value h'04'.
001400    05 AG-COMBO-HFILL            usage binary-long value h'08'.
001410    05 AG-COMBO-VFILL            usage binary-long value h'10'.
001420    05 AG-COMBO-SCROLLTOSEL      usage binary-long value h'40'.
001430    05 AG-COMBO-EXPAND           usage binary-long value h'18'.
001440
001450
001460
001470    
001480 01 console-flags.
001490    05 AG-CONSOLE-HFILL          usage binary-long value h'01'.
001500    05 AG-CONSOLE-VFILL          usage binary-long value h'02'.
001510    05 AG-CONSOLE-NOAUTOSCROLL   usage binary-long value h'04'.
001520    05 AG-CONSOLE-NOPOPUP        usage binary-long value h'08'.
001530    05 AG-CONSOLE-EXPAND         usage binary-long value h'03'.
001540    05 AG-CONSOLE-SELECTING      usage binary-long value h'10'.
001550
001560 01 cpu-ext-bits.
001570       05 AG-EXT-CPUID usage binary-long value 1.
001580       05 AG-EXT-MMX usage binary-long value 2.
001590       05 AG-EXT-MMX-EXT usage binary-long value 4.
001600       05 AG-EXT-3DNOW usage binary-long value 8.
001610       05 AG-EXT-3DNOW-EXT usage binary-long value 16.
001620       05 AG-EXT-ALTIVEC usage binary-long value 32.
001630       05 AG-EXT-SSE usage binary-long value 64.
001640       05 AG-EXT-SSE2 usage binary-long value 128.
001650       05 AG-EXT-SSE3 usage binary-long value 256.
001660       05 AG-EXT-LONG-MODE usage binary-long value 512.
001670       05 AG-EXT-RDTSCP usage binary-long value 1024.
001680       05 AG-EXT-FXSR usage binary-long value 2048.
001690       05 AG-EXT-PAGE-NX usage binary-long value 4096.
001700       05 AG-EXT-SSE5A usage binary-long value 8192.
001710       05 AG-EXT-3DNOW-PREFETCH usage binary-long value 16384.
001720       05 AG-EXT-SSE-MISALIGNED usage binary-long value 32768.
001730       05 AG-EXT-SSE4A usage binary-long value 65536.
001740       05 AG-EXT-ONCHIP-FPU usage binary-long value 131072.
001750       05 AG-EXT-TSC usage binary-long value 262144.
001760       05 AG-EXT-CMOV usage binary-long value 524288.
001770       05 AG-EXT-CLFLUSH usage binary-long value 1048576.
001780       05 AG-EXT-HTT usage binary-long value 2097152.
001790       05 AG-EXT-MON usage binary-long value 4194304.
001800       05 AG-EXT-VMX usage binary-long value 8388608.
001810       05 AG-EXT-SSSE3 usage binary-long value 16777216.
001820       05 AG-EXT-SSE41 usage binary-long value 33554432.
001830       05 AG-EXT-SSE42 usage binary-long value 67108864.
001840
001850 01 dirdlg-flags.
001860    05 AG-DIRDLG-MULTI           usage binary-long value h'001'.
001870    05 AG-DIRDLG-CLOSEWIN        usage binary-long value h'002'.
001880    05 AG-DIRDLG-LOAD            usage binary-long value h'004'.
001890    05 AG-DIRDLG-SAVE            usage binary-long value h'008'.
001900    05 AG-DIRDLG-ASYNC           usage binary-long value h'010'.
001910    05 AG-DIRDLG-RESET-ONSHOW    usage binary-long value h'020'.
001920    05 AG-DIRDLG-HFILL           usage binary-long value h'100'.
001930    05 AG-DIRDLG-VFILL           usage binary-long value h'200'.
001940    05 AG-DIRDLG-EXPAND          usage binary-long value h'300'.
001950    05 AG-DIRDLG-NOBUTTONS       usage binary-long value h'400'.
001960
001970 01 editable-flags.
001980    05 AG-EDITABLE-HFILL         usage binary-long value h'00001'.
001990    05 AG-EDITABLE-VFILL         usage binary-long value h'00002'.
002000    05 AG-EDITABLE-EXPAND        usage binary-long value h'00003'.
002010    05 AG-EDITABLE-MULTILINE     usage binary-long value h'00004'.
002020    05 AG-EDITABLE-BLINK-ON      usage binary-long value h'00008'.
002030    05 AG-EDITABLE-PASSWORD      usage binary-long value h'00010'.
002040    05 AG-EDITABLE-ABANDON-FOCUS usage binary-long value h'00020'.
002050    05 AG-EDITABLE-INT-ONLY      usage binary-long value h'00040'.
002060    05 AG-EDITABLE-FLT-ONLY      usage binary-long value h'00080'.
002070    05 AG-EDITABLE-CATCH-TAB     usage binary-long value h'00100'.
002080    05 AG-EDITABLE-CURSOR-MOVING usage binary-long value h'00200'.
002090    05 AG-EDITABLE-KEEPVISCURSOR usage binary-long value h'00800'.
002100    05 AG-EDITABLE-MARKPREF      usage binary-long value h'02000'.
002110    05 AG-EDITABLE-EXCL          usage binary-long value h'04000'.
002120    05 AG-EDITABLE-NOEMACS       usage binary-long value h'08000'.
002130    05 AG-EDITABLE-NOLATIN1      usage binary-long value h'20000'.
002140    05 AG-EDITABLE-WORDWRAP      usage binary-long value h'40000'.
002150    05 AG-EDITABLE-NOPOPUP       usage binary-long value h'80000'.
002160    05 AG-EDITABLE-WORDSELECT   usage binary-long value h'100000'.
002170    05 AG-EDITABLE-READONLY     usage binary-long value h'200000'.
002180    05 AG-EDITABLE-MULTILINGUAL usage binary-long value h'400000'.
002190
002200 01 exec-wait-types.
002210    05 AG-EXEC-WAIT-IMMEDIATE    usage binary-long value 0.
002220    05 AG-EXEC-WAIT-INFINITE     usage binary-long value 1.
002230
002240 01 filedlg-flags.
002250    05 AG-FILEDLG-MULTI          usage binary-long value h'0001'.
002260    05 AG-FILEDLG-CLOSEWIN       usage binary-long value h'0002'.
002270    05 AG-FILEDLG-LOAD           usage binary-long value h'0004'.
002280    05 AG-FILEDLG-SAVE           usage binary-long value h'0008'.
002290    05 AG-FILEDLG-ASYNC          usage binary-long value h'0010'.
002300    05 AG-FILEDLG-RESET-ONSHOW   usage binary-long value h'0020'.
002310    05 AG-FILEDLG-HFILL          usage binary-long value h'0100'.
002320    05 AG-FILEDLG-VFILL          usage binary-long value h'0200'.
002330    05 AG-FILEDLG-EXPAND         usage binary-long value h'0300'.
002340    05 AG-FILEDLG-NOBUTTONS      usage binary-long value h'0400'.
002350    05 AG-FILEDLG-MASK-EXT       usage binary-long value h'0800'.
002360    05 AG-FILEDLG-MASK-HIDDEN    usage binary-long value h'1000'.
002370    05 AG-FILEDLG-NOMASKOPTS     usage binary-long value h'2000'.
002380    05 AG-FILEDLG-NOTYPESELECT   usage binary-long value h'4000'.
002390
002400 01 fixed-flags.
002410    05 AG-FIXED-HFILL            usage binary-long value h'01'.
002420    05 AG-FIXED-VFILL            usage binary-long value h'02'.
002430    05 AG-FIXED-NO-UPDATE        usage binary-long value h'04'.
002440    05 AG-FIXED-FILLBG           usage binary-long value h'08'.
002450    05 AG-FIXED-BOX              usage binary-long value h'10'.
002460    05 AG-FIXED-INVBOX           usage binary-long value h'20'.
002470    05 AG-FIXED-FRAME            usage binary-long value h'40'.
002480    05 AG-FIXED-EXPAND           usage binary-long value h'03'.
002490    05 AG-FIXED-DEFAULT          usage binary-long value h'43'.
002500
002510 01 graph-flags.
002520    05 AG-GRAPH-HFILL            usage binary-long value h'01'.
002530    05 AG-GRAPH-VFILL            usage binary-long value h'02'.
002540    05 AG-GRAPH-EXPAND           usage binary-long value h'03'.
002550    05 AG-GRAPH-SCROLL           usage binary-long value h'04'.
002560    05 AG-GRAPH-DRAGGING         usage binary-long value h'08'.
002570    05 AG-GRAPH-PANNING          usage binary-long value h'10'.
002580    05 AG-GRAPH-NO-MOVE          usage binary-long value h'20'.
002590    05 AG-GRAPH-NO-SELECT        usage binary-long value h'40'.
002600    05 AG-GRAPH-NO-MENUS         usage binary-long value h'80'.
002610    05 AG-GRAPH-READONLY         usage binary-long value h'E0'.
002620
002630 01 graph-vertex-flags.
002640    05 AG-GRAPH-VERTEX-RECTANGLE usage binary-long value 0.
002650    05 AG-GRAPH-VERTEX-CIRCLE    usage binary-long value 1.
002660
002670
002680 01 plotter-types.
002690    05 AG-PLOTTER-POINTS         usage binary-long value 0.
002700    05 AG-PLOTTER-LINES          usage binary-long value 1.
002710
002720 01 plotter-flags.
002730    05 AG-FIXED-PLOTTER-SCROLL   usage binary-long value 1.
002740    05 AG-FIXED-PLOTTER-XAXIS    usage binary-long value 2.
002750    05 AG-FIXED-PLOTTER-HFILL    usage binary-long value 4.
002760    05 AG-FIXED-PLOTTER-VFILL    usage binary-long value 8.
002770    05 AG-FIXED-PLOTTER-EXPAND   usage binary-long value 12.
002780
002790 01 fontselector-flags.
002800    05 AG-FONTSELECTOR-UPDATE    usage binary-long value h'001'.
002810    05 AG-FONTSELECTOR-HFILL     usage binary-long value h'100'.
002820    05 AG-FONTSELECTOR-VFILL     usage binary-long value h'200'.
002830    05 AG-FONTSELECTOR-EXPAND    usage binary-long value h'300'.
002840
002850 01 hsvpal-flags.
002860    05 AG-HSVPAL-PIXEL           usage binary-long value h'01'.
002870    05 AG-HSVPAL-DIRTY           usage binary-long value h'02'.
002880    05 AG-HSVPAL-HFILL           usage binary-long value h'04'.
002890    05 AG-HSVPAL-VFILL           usage binary-long value h'08'.
002900    05 AG-HSVPAL-NOALPHA         usage binary-long value h'10'.
002910    05 AG-HSVPAL-FORCE-NOALPHA   usage binary-long value h'10'.
002920    05 AG-HSVPAL-NOPREVIEW       usage binary-long value h'20'.
002930    05 AG-HSVPAL-SHOW-RGB        usage binary-long value h'40'.
002940    05 AG-HSVPAL-SHOW-HSV        usage binary-long value h'80'.
002950    05 AG-HSVPAL-EXPAND          usage binary-long value h'0C'.
002960
002970
002980 01 mpane-flags.
002990    05 AG-MPANE-HFILL            usage binary-long value h'01'.
003000    05 AG-MPANE-VFILL            usage binary-long value h'02'.
003010    05 AG-MPANE-FRAMES           usage binary-long value h'04'.
003020    05 AG-MPANE-FORCE-DIV        usage binary-long value h'08'.
003030    05 AG-MPANE-EXPAND           usage binary-long value h'03'.
003040    05 AG-MPANE-DEFAULT          usage binary-long value h'09'.
003050
003060
003070 01 mpane-layouts.
003080    05 AG-MPANE1                 usage binary-long value 00.
003090    05 AG-MPANE2V                usage binary-long value 01.
003100    05 AG-MPANE2H                usage binary-long value 02.
003110    05 AG-MPANE2L1R              usage binary-long value 03.
003120    05 AG-MPANE1L2R              usage binary-long value 04.
003130    05 AG-MPANE2T1B              usage binary-long value 05.
003140    05 AG-MPANE1T2B              usage binary-long value 06.
003150    05 AG-MPANE3L1R              usage binary-long value 07.
003160    05 AG-MPANE1L3R              usage binary-long value 08.
003170    05 AG-MPANE3T1B              usage binary-long value 09.
003180    05 AG-MPANE1T3B              usage binary-long value 10.
003190    05 AG-MPANE4                 usage binary-long value 11.
003200
003210 01 notebook-flags.
003220    05 AG-NOTEBOOK-HFILL         usage binary-long value h'01'.
003230    05 AG-NOTEBOOK-VFILL         usage binary-long value h'02'.
003240    05 AG-NOTEBOOK-HIDE-TABS     usage binary-long value h'04'.
003250    05 AG-NOTEBOOK-EXPAND        usage binary-long value h'03'.
003260
003270 01 numeri1cal-flags.
003280    05 AG-NUMERICAL-HFILL        usage binary-long value h'01'.
003290    05 AG-NUMERICAL-VFILL        usage binary-long value h'02'.
003300    05 AG-NUMERICAL-INT          usage binary-long value h'04'.
003310    05 AG-NUMERICAL-EXCL         usage binary-long value h'08'.
003320    05 AG-NUMERICAL-DEFAULT      usage binary-long value h'09'.
003330
003340 01 socket-flags.
003350    05 AG-SOCKET-HFILL           usage binary-long value h'01'.
003360    05 AG-SOCKET-VFILL           usage binary-long value h'02'.
003370    05 AG-SOCKET-EXPAND          usage binary-long value h'03'.
003380    05 AG-SOCKET-MOUSEOVER       usage binary-long value h'04'.
003390    05 AG-SOCKET-STICKY_STATE    usage binary-long value h'08'.
003400
003410 01 table-flags.
003420    05 AG-TABLE-MULTI            usage binary-long value h'001'.
003430    05 AG-TABLE-MULTITOGGLE      usage binary-long value h'002'.
003440    05 AG-TABLE-REDRAW-CELLS     usage binary-long value h'004'.
003450    05 AG-TABLE-POLL             usage binary-long value h'008'.
003460    05 AG-TABLE-HIGHLIGHT-COLS   usage binary-long value h'040'.
003470    05 AG-TABLE-WIDGETS          usage binary-long value h'080'.
003480    05 AG-TABLE-MULTIMODE        usage binary-long value h'003'.
003490    05 AG-TABLE-NOAUTOSORT       usage binary-long value h'100'.
003500    05 AG-TABLE-NEEDSORT         usage binary-long value h'200'.
003510
003520 01 timer-flags.
003530    05 AG-TIMER-SURVIVE-DETACH   usage binary-long value h'01'.
003540    05 AG-TIMER-AUTO-FREE        usage binary-long value h'02'.
003550    05 AG-TIMER-EXECD            usage binary-long value h'04'.
003560    05 AG-TIMER-RESTART          usage binary-long value h'08'.
003570
003580 01 textbox-flags.
003590    05 AG-TEXTBOX-MULTILINE     usage binary-long value h'000001'.
003600    05 AG-TEXTBOX-PASSWORD      usage binary-long value h'000004'.
003610    05 AG-TEXTBOX-ABANDON-FOCUS usage binary-long value h'000008'.
003620    05 AG-TEXTBOX-COMBO         usage binary-long value h'000010'.
003630    05 AG-TEXTBOX-HFILL         usage binary-long value h'000020'.
003640    05 AG-TEXTBOX-VFILL         usage binary-long value h'000040'.
003650    05 AG-TEXTBOX-EXPAND        usage binary-long value h'000030'.
003660    05 AG-TEXTBOX-READONLY      usage binary-long value h'000100'.
003670    05 AG-TEXTBOX-INT-ONLY      usage binary-long value h'000200'.
003680    05 AG-TEXTBOX-FLT-ONLY      usage binary-long value h'000400'.
003690    05 AG-TEXTBOX-CATCH-TAB     usage binary-long value h'000800'.
003700    05 AG-TEXTBOX-CURSOR-MOVING usage binary-long value h'001000'.
003710    05 AG-TEXTBOX-EXCL          usage binary-long value h'004000'.
003720    05 AG-TEXTBOX-NOEMACS       usage binary-long value h'008000'.
003730    05 AG-TEXTBOX-NOLATIN1      usage binary-long value h'020000'.
003740    05 AG-TEXTBOX-WORDWRAP      usage binary-long value h'040000'.
003750    05 AG-TEXTBOX-NOPOPUP       usage binary-long value h'080000'.
003760    05 AG-TEXTBOX-MULTILINGUAL  usage binary-long value h'100000'.
003770    05 AG-TEXTBOX-DEFAULT       usage binary-long value h'000000'.
003780    
003790 01 treetbl-flags.
003800    05 AG-TREETBL-MULTI         usage binary-long value h'001'.
003810    05 AG-TREETBL-MULTITOGGLE   usage binary-long value h'002'.
003820    05 AG-TREETBL-REORDERCOLS   usage binary-long value h'004'.
003830    05 AG-TREETBL-NODUPCHECKS   usage binary-long value h'008'.
003840    05 AG-TREETBL-SORT          usage binary-long value h'010'.
003850    05 AG-TREETBL-POLLED        usage binary-long value h'020'.
003860    05 AG-TREETBL-HFILL         usage binary-long value h'040'.
003870    05 AG-TREETBL-VFILL         usage binary-long value h'080'.
003880    05 AG-TREETBL-EXPAND        usage binary-long value h'0C0'.
003890
003900
003910*> ***************************************************************
003920*> Networking
003930 01 addr-families.
003940    05 AG-NET-AF-NONE            usage binary-long value 0.
003950    05 AG-NET-LOCAL              usage binary-long value 1.
003960    05 AG-NET-INET4              usage binary-long value 2.
003970    05 AG-NET-INET6              usage binary-long value 3.
003980
003990 01 socket-types.
004000    05 AG-NET-SOCKET-NONE        usage binary-long value 0.
004010    05 AG-NET-STREAM             usage binary-long value 1.
004020    05 AG-NET-DGRAM              usage binary-long value 2.
004030    05 AG-NET-RAW                usage binary-long value 3.
004040    05 AG-NET-RDM                usage binary-long value 4.
004050    05 AG-NET-SEQPACKET          usage binary-long value 5.
004060
004070 01 socket-poll-flags.
004080    05 AG-NET-POLL-READ          usage binary-long value 1.
004090
004100
004110 
004120
004130 77 agar-debug                   pic x(10).
004140 01 stringa                      pic x(100) value space. 
004150 01 tab-numbers.
004160  07 tab-alfa                    pic x(10).
004170  07 tab-num                     pic 9(10).
004180 
004190
004200 77 local-buffer                 pic x(10) value space. 
004210 77 local-string                 pic x(100) based. 
004220 77 local                        usage pointer.
004230      copy "global".
004240 procedure division.
004250*set-debug-off
004260      ACCEPT agar-debug from environment "GuiCOBOLdebug".
004270
004280      if agar-started not = agar-true
004290       and agar-function not = "initialize"
004300
004310        perform inizializza      thru ex-inizializza.
004320
004330      if function lower-case(agar-local-debug) = "y"
004340        move "enable"              to agar-debug.
004350      if function lower-case(agar-local-debug) = "n"
004360        move spaces              to agar-debug.
004370
004380
004390      if function lower-case(agar-debug) = "enable" 
004400         perform agar-do-debug      thru ex-agar-do-debug.
004410
004420      
004430      
004440      evaluate function lower-case(agar-function)
004450
004460      when "addform"      perform addform     thru ex-addform
004470      when "show"         perform set-visible thru ex-set-visible
004480
004490      when "set-error"    perform set-error   thru ex-set-error
004500      when "set-warning"  perform set-warning thru ex-set-warning
004510      when "set-info"     perform set-info    thru ex-set-info
004520      when "set-caption"  perform set-caption thru ex-set-caption
004530      when "get-caption"  perform get-caption thru ex-get-caption
004530      when "get-node"     perform get-node    thru ex-get-node
004540      when "set-width"    perform set-width   thru ex-set-width
004550      when "set-height"   perform set-height  thru ex-set-height
004560      when "get-width"    perform get-width   thru ex-get-width
004570      when "get-height"   perform get-height  thru ex-get-height
004580
004590
004600      when "set-top"      perform set-top     thru ex-set-top 
004610      when "set-left"     perform set-left    thru ex-set-left
004620      when "get-top"      perform get-top     thru ex-get-top
004630      when "get-left"     perform get-left    thru ex-get-left
004630      when "get-value"    perform get-value   thru ex-get-value
004640
004650
004660      when "set-value"    perform set-value   thru ex-set-value
004670      
004680      when "set-position" perform set-position 
004690                                              thru ex-set-position
004700      when "set-disposition" perform set-disposition
004710                                          thru ex-set-disposition
004720      
004730      when "set-visible"  perform set-visible thru ex-set-visible
004740      when "set-invisible"  
004750                          perform set-invisible  
004760                             thru ex-set-invisible
004770      when "hide"         perform set-invisible 
004780                                thru ex-set-invisible
004790
004800      when "addlabel"     perform addlabel    thru ex-addlabel
004810      when "addpane"      perform addpane     thru ex-addpane
004820      when "addprogress"  perform addprogress thru ex-addprogress
004830      when "addslider"    perform addslider   thru ex-addslider
004840      when "addsliderv"   perform addsliderv  thru ex-addsliderv
004850      when "addscroll"    perform addscroll   thru ex-addscroll
004860      when "addscrollv"   perform addscrollv  thru ex-addscrollv
004870      when "addscrollview"  
004880                          perform addscrollview   
004890                                           thru ex-addscrollview
004900
004910      when "addbutton"    perform addbutton   thru ex-addbutton
004920      when "addbuttonto"  perform addbuttonto thru ex-addbuttonto
004930
004940      when "addcheck"     perform addcheck    thru ex-addcheck
004950      when "addcheckto"   perform addcheckto  thru ex-addcheckto
004960
004970      when "additem"      perform additem     thru ex-additem
004980
004990        
005000      when "addtext"      perform addtext     thru ex-addtext
005010      when "addnumeric"   perform addnumeric  thru ex-addnumeric
005020      when "addfixed"     perform addfixed    thru ex-addfixed
005030      when "addcombo"     perform addcombo    thru ex-addcombo
005040      when "addradio"     perform addradio    thru ex-addradio
004810      when "addmenu"      perform addmenu     thru ex-addmenu
004810      when "addnode"      perform addnode     thru ex-addnode
004810      when "addmenuitem"  perform addmenuitem thru ex-addmenuitem
004810      when "addlink"      perform addlink     thru ex-addlink
005060      when "remove"       perform remove      thru ex-remove
005060      when "move"         perform moveto      thru ex-moveto
005070      when "size"         perform sizeto      thru ex-sizeto
005080      when "set-size"     perform sizeto      thru ex-sizeto
005090       
005100      when "setevent"     perform setevent    thru ex-setevent
005110      when "addevent"     perform addevent    thru ex-addevent
005120
005130      when "addbox"       perform addbox      thru ex-addbox
005140      when "addframe"     perform addframe    thru ex-addframe
005150      
005160      when "get-class"    perform get-class   thru ex-get-class
005170      when "set-text"     perform set-text    thru ex-set-text
005180      when "get-text"     perform get-text    thru ex-get-text
005190
005200      when "set-render"   perform set-render  thru ex-set-render
005200      when "set-prompt"   perform set-prompt  thru ex-set-prompt
005210  
005220         
005230      when "set-name"     perform set-name    thru ex-set-name
005240      when "get-name"     perform get-name    thru ex-get-name
005250      when "get-focused"  perform get-focused thru ex-get-focused

005240      when "get-cursor"   perform get-cursor  thru ex-get-cursor
005240      when "set-cursor"   perform set-cursor  thru ex-set-cursor
005270
005280      when "addproperty"  perform addproperty 
005290                             thru ex-addproperty
005300      when "get-property" perform get-property 
005310                             thru ex-get-property
005320

005340      when "find"         perform find        thru ex-find
005350
005360      when "refresh"      perform refresh     thru ex-refresh
005360      when "disable"      perform disableit   thru ex-disableit
005360      when "enable"       perform enableit    thru ex-enableit
005370
005380      when other
005390       display "agarcob - command: '" 
005400        function trim(agar-FUNCTION)
005410          "'  not yet implemented in agarcob"
005420                  upon syserr
005430      end-evaluate.
005440
005450      if function lower-case(agar-debug) = "enable"
005460       perform agar-after-debug    thru ex-agar-after-debug.
005470
005480      
005490
005500      exit program.
005510
005520 inizializza.
005530
005540      if agar-started not = agar-true
005550
005560      call static "AG_InitCore" using null by value 0 returning rc
00557 *    on exception display "error: no libagar " upon syserr
005580      end-call
005590
005600      if rc = -1 then
005610       display "error: AG_InitCore failure" upon syserr
005620      goback
005630      end-if
005640
005650      call "AG_InitGraphics" using by value 0 returning rc
005660      if rc = -1 then
005670       display "error: AG_InitGraphics failure" upon syserr
005680      goback
005690     end-if
005700
005710      move agar-true  to agar-started.
005720
005730      set agar-main   to null.
005740
005750
005760 ex-inizializza.
005770      exit.
005780
005790 addform.
005800
005810      call "AG_WindowNew" using by value 0 returning agar-object.
005820
005830      if agar-main = null
005840        move agar-object to agar-main.
005850
005860      move agar-object               to agar-form.
005870      move "DefaultDestroy"          to agar-procedure.
005880      set agar-callback              to entry "DefaultDestroy".
005890      move "window-detached" & X"00" to agar-event.
005900      move "setevent"                to agar-function.
005910      perform setevent               thru ex-setevent.
005920
005930 ex-addform.
005940      exit.
005950
005960 get-focused.
005970     
005980          call "AG_WidgetFindFocused"
005990            using by value agar-object  returning agar-widget.
006000     
006010 ex-get-focused.
006020      exit.
006030      
006040 set-visible.
006050
006060      if agar-widget = agar-form
006070          call "AG_WindowShow"
006080          using by value agar-widget returning omitted
006090           go to ex-set-visible.
006100
006110      if agar-debug = "enable"
006120       move "internal get-class"   to agar-function
006130        perform agar-do-debug      thru ex-agar-do-debug.
006140
006150       perform get-class thru ex-get-class.
006160
006170      if agar-debug = "enable"
006180       perform agar-after-debug    thru ex-agar-after-debug.
006190
006200
006210      evaluate agar-class
006220         when "form"
006230
006240        call "AG_WindowShow"
006250          using by value agar-widget returning omitted
006260
006270         when other
006280         
006290       call "AG_WidgetShow"
006300          using by value agar-widget returning omitted.
006310
006320 ex-set-visible.
006330      exit.
006340
006350 set-invisible.
006360
006370       perform get-class thru ex-get-class.
006380
006390      evaluate agar-class
006400         when "form"
006410          call "AG_WindowHide"
006420          using by value agar-widget returning omitted
006430          
006440         when other 
006450       call "AG_WidgetHide"
006460          using by value agar-widget returning omitted.
006470
006480 ex-set-invisible.
006490      exit.
006500
006510 setevent.
006520      
006530       move agar-event        to agar-text.
006540       perform asciiz         thru ex-asciiZ
006550       move agar-text         to agar-event.
006560      
006570      if agar-callback equal null then
006580        display "windown entry not found:"
006590        function trim(agar-procedure)
006600        else
006610           call "AG_SetEvent" using
006620           by value agar-object
006630           by reference agar-event
006640           by value agar-callback
006650           by reference null
006660            end-if.
006670
006680       set agar-callback to null.
006690       move spaces       to agar-procedure.
006700       move spaces       to agar-event.
006710       
006720 ex-setevent.
006730      exit.
006740
006750 addevent.
006760      
006770       move agar-event        to agar-text.
006780       perform asciiz         thru ex-asciiZ
006790       move agar-text         to agar-event.
006800      
006810      if agar-callback equal null then
006820        display "windown entry not found:"
006830        function trim(agar-procedure)
006840        else
006850           call "AG_AddEvent" using
006860           by value agar-object
006870           by reference agar-event
006880           by value agar-callback
006890           by reference null
006900            end-if.
006910
006920       set agar-callback to null.
006930       move spaces       to agar-procedure.
006940       move spaces       to agar-event.
006950       
006960 ex-addevent.
006970      exit.
006980
006990
007000 refresh.
007010     
007020      if agar-debug = "enable"
007030       move "internal get-class"   to agar-function
007040        perform agar-do-debug      thru ex-agar-do-debug.
007050
007060     
007070      perform get-class thru ex-get-class.
007080
007090      evaluate agar-class
007100         when "form"
007110         CALL static  "AG_WindowUpdate" 
007120          using by value agar-object
007130           returning omitted
007140         
007150         when other 
007160
007170         CALL  STATIC  "ag_widget_update" 
007180          using by value agar-object
007190            returning omitted
007200      end-evaluate.             
007210      
007220
007230 ex-refresh.
007240      exit.

007000 enableit.
007010     
007020      if agar-debug = "enable"
007030       move "internal get-class"   to agar-function
007040        perform agar-do-debug      thru ex-agar-do-debug.
007050
007060     
007070      perform get-class thru ex-get-class.
007080
007090      evaluate agar-class
007100         when "menu"
007110         CALL static  "AG_MenuState" 
007120          using by value agar-object
                 by value 1
007140         
007150         when other 
007160
            call static "AG_WidgetEnable"
007180          using by value agar-object

007200      end-evaluate.             
007210      
007220
007230 ex-enableit.
007240      exit.

007000 disableit.
007010     
007020      if agar-debug = "enable"
007030       move "internal get-class"   to agar-function
007040        perform agar-do-debug      thru ex-agar-do-debug.
007050
007060     
007070      perform get-class thru ex-get-class.
007080
007090      evaluate agar-class
007100         when "menu"
007110         CALL static  "AG_MenuState" 
007120          using by value agar-object
                  by value 0
007140         
007150         when other 
007160
            call static "AG_WidgetDisable"
007180          using by value agar-object

007200      end-evaluate.             
007210      
007220
007230 ex-disableit.
007240      exit.

007250      
007260 set-value.
007270
007280      if agar-debug = "enable"
007290       move "internal get-class"   to agar-function
007300        perform agar-do-debug      thru ex-agar-do-debug.
007310
007320      perform get-class thru ex-get-class.
007330
007340      if agar-debug = "enable"
007350       perform agar-after-debug    thru ex-agar-after-debug.
007360     
007370        move "set-value"           to agar-function
007380   
007390      evaluate agar-class
007400         when "progress"  
007410     
007420       
007430      call  "AG_BindInt" using
007440         by value agar-object
007450            by reference z"value"
007460             by reference agar-int
007470      end-call     
007480
007490         when "check"
007500                 
007510         call  "AG_CheckboxSetState" using
007520         by value  
007530           agar-object                    
007540            by value
007550               agar-int
007560             end-call
007570            
007580         when other display " agar-class"  agar-class
007590      end-evaluate.
007600            
007610            
007620 ex-set-value.
007630      exit.
007640
007650 get-value.
007660     
007670      if agar-debug = "enable"
007680       move "internal get-class"   to agar-function
007690        perform agar-do-debug      thru ex-agar-do-debug.
007700
007710      perform get-class           thru ex-get-class.
007720
007730      if agar-debug = "enable"
007740       perform agar-after-debug    thru ex-agar-after-debug.
007750     
007760        move "get-value"           to agar-function
007770
007780      evaluate agar-class
007790      
007800         when "check"
007810
007820      call static "AG_CheckboxGetState" using
007830         by value  
007840           agar-object                  
007850             returning 
007860               agar-int                   
007870          end-call
007880
007890
007900            
007910         when other display "class in get value:" agar-class
007920      end-evaluate.
007930           
007940 ex-get-value.
007950      exit.          
007960      
007970 set-caption.
007980
007990      perform asciiZ  thru ex-asciiZ.
008000
008010      if agar-debug = "enable"
008020       move "internal get-class"   to agar-function
008030        perform agar-do-debug      thru ex-agar-do-debug.
008040     
008050
008060
008070      perform get-class thru ex-get-class.
008080
008090      if agar-debug = "enable"
008100       perform agar-after-debug    thru ex-agar-after-debug.
008110
008120      move "set-caption"           to agar-function
008130 
008140      evaluate agar-class
008150         when "form"
008160
008170         move agar-form           to agar-object
008180
008190         CALL  "AG_WindowSetCaptionS"
008200          using by value agar-object
008210          by reference agar-text
008220         
008230         when "box"
008240
008250         call static "AG_BoxSetLabelS" using by value 
008260          agar-object 
008270            by reference agar-text
008280
008290         when "label"
008300
008310         call static "AG_LabelText" using by value 
008320          agar-object 
008330            by reference agar-text
008340
008350         when "text"
008360
008370         call static "AG_TextboxSetLabelS" using by value 
008380          agar-object 
008390            by reference agar-text
008400
008410         when "button"

008420            call static "AG_ButtonText" using by value
008430             agar-object 
008440            by reference agar-text

008410         when "menu"
                 

                call  "AG_MenuSetLabel" 
                   using by value  agar-object 
008440                by reference agar-text

008450
008460         WHEN other
008470          
008480          DISPLAY "caption is not defined for"
008490                function trim(agar-class) upon syserr
008500
008510      end-evaluate.
008520
008530      
008540      move agar-text         to agar-value
008550  
008560      call "AG_SetString" using
008570         by value agar-object
008580            by reference z"caption"
008590             by reference agar-value
008600               returning  omitted.
008610      
008620
008630 ex-set-caption.
008640      exit.
008650
008660 
008670 
008680 addbox.
008690
008700     call static "AG_BoxNewHoriz" using
008710         by value agar-object
008720           BY VALUE 10
008730*              by value AG-BOX-HFILL
008740*              by value AG-BOX-HOMOGENOUS
008750*            by value AG-BOX-HOMOGENOUS
008760*             by value AG-BOX-HFILL
008770*            by value AG-BOX-VFILL
008780*           by value AG-BOX-FRAME
008790
008800         returning agar-widget.
008810         
008820 ex-addbox.
008830      exit.
008840
008850 addframe.
008860
008870     call static "AG_BoxNewVert" using
008880         by value agar-object
008890           BY VALUE 12
008900*              by value AG-BOX-HOMOGENOUS
008910*            by value AG-BOX-HOMOGENOUS
008920*             by value AG-BOX-HFILL
008930*            by value AG-BOX-VFILL
008940*           by value AG-BOX-FRAME
008950
008960         returning agar-widget.
008970         
008980 ex-addframe.
008990      exit.
009000
009010
009020
009030 addlabel.
009040
009050      PERFORM asciiZ thru ex-asciiz.
009060           
009070*      call static "AG_LabelNew" using
009080       call  "AG_LabelNew" using
009090         by value agar-object 0
009100              by reference agar-text 
009110               returning agar-widget.
009120         
009130             call static "AG_LabelSizeHint"
009140             using by value agar-widget 
009150              by reference agar-text.
009160         
009170 ex-addlabel.
009180      exit.
009190
009200 addprogress.
009210
009220      call static "AG_ProgressBarNewHoriz" using
009230         by value agar-object
009240          by value 13
009250               returning agar-widget.
009260         
009270 ex-addprogress.
009280      exit.
009290
009300 addslider.
009310
009320       call static "AG_SliderNew" using
009330         by value agar-object
009340          by value 0
009350          by value 9
009360               returning agar-widget.
009370
009380               
009390         
009400 ex-addslider.
009410      exit.
009420
009430 addsliderv.
009440
009450      call static "AG_SliderNew" using
009460         by value agar-object
009470          by value 1
009480          by value 9
009490               returning agar-widget.
009500         
009510 ex-addsliderv.
009520      exit.
009530
009540
009550 addscroll.
009560
009570       call static "AG_ScrollbarNewHoriz" using
009580         by value agar-object
009590             by value 5
009600               returning agar-widget.
009610
009620 ex-addscroll.
009630      exit.
009640
009650 addscrollview.
009660
009670       call static "AG_ScrollviewNew" using
009680         by value agar-object
009690             by value  3
009700               returning agar-widget.
009710
009720 ex-addscrollview.
009730      exit.
009740
009750
009760 addscrollv.
009770
009780       call static "AG_ScrollbarNewVert" using
009790         by value agar-object
009800             by value 5
009810               returning agar-widget.
009820
009830 ex-addscrollv.
009840      exit.
009850
009860
009870
009880
009890 addpane.
009900
009910      PERFORM asciiZ thru ex-asciiz.
009920           
009930      if function upper-case(agar-text(1:1)) = "H"
009940      call static "AG_PaneNewHoriz" using
009950         by value agar-object
009960             by value AG-MPANE-EXPAND
009970               returning agar-widget
009980           else               
009990
010000       call static "AG_PaneNewVert" using
010010         by value agar-object
010020             by value AG-MPANE-EXPAND
010030               returning agar-widget.
010040         
010050
010060         set address of pane-instance  to agar-widget.
010070         
010080*          display function length(pane-instance)
010090*          display function length(pane-type)
010100*          display function length(pane-flags)
010110      
010120         move pane-divs(1)               to agar-pane-one
010130         move pane-divs(2)               to agar-pane-two.
010140          
010150      
010160 ex-addpane.
010170      exit.


009890 addmenu.
009900
009910      PERFORM asciiZ thru ex-asciiz.
009920           
             call static "AG_MenuNew" using by value agar-object
                 by value 1
008960          returning agar-widget.
010050
010060         set address of menu-instance    to agar-widget.
010070         
010120         move menu-root                  to agar-root.
010150      
010160 ex-addmenu.
010170      exit.



       get-node.
       
               set address of menu-instance    to agar-object.
010070         
010120         move menu-root                  to agar-root.
010150 
       
       ex-get-node.
            exit.
            
009890 addnode.
009900
009910      PERFORM asciiZ thru ex-asciiz.

009920           
             call static "AG_MenuNode" using by value agar-object
                 by reference agar-text 
                 by value agar-null-pointer
008960          returning agar-widget.

010150      
010160 ex-addnode.
010170      exit.

            
009890 addmenuitem.
009900
009910      PERFORM asciiZ thru ex-asciiz.

             call static "AG_MenuDynamicItem" 
                using by value agar-object
                 by reference agar-text 
                 by value agar-null-pointer
                 by value agar-null-pointer
                 by value agar-null-pointer
008960           returning agar-widget.

010160 ex-addmenuitem.
010170      exit.

009890 addlink.
009900
009910       PERFORM asciiZ                 thru ex-asciiz.
             
             display "agar:" function trim(agar-procedure)
                             " " agar-callback
009920           
006570       if agar-callback equal null then
006580        display "windown entry not found:"
006590        function trim(agar-procedure)
006600        else
              call static "AG_MenuAction" using
               by value agar-object
               by reference function concatenate(agar-text low-value)
               by value agar-null-pointer
               by value agar-callback
                by reference "%p(menu)"
                by value agar-null-pointer

               returning agar-widget.                         
               
               
               

010160 ex-addlink.
010170      exit.

007970 remove.
007980
008000
008010      if agar-debug = "enable"
008020       move "internal get-class"   to agar-function
008030        perform agar-do-debug      thru ex-agar-do-debug.
008040     
008060
008070      perform get-class thru ex-get-class.
008080
008090      if agar-debug = "enable"
008100       perform agar-after-debug    thru ex-agar-after-debug.
008110
008130 
008140      evaluate agar-class
008150         when "form"
008410         when "menu"

                call  "AG_MenuDeL" 
                   using by value  agar-object 
                   
008450
008460         WHEN other
008470          
008480          DISPLAY "remove is not defined for"
008490                function trim(agar-class) upon syserr
008500
008510      end-evaluate.
008520
008620
008630 ex-remove.
008640      exit.


010190
010180
010190
010200*
010210* usage: move  "format string" to "render" of object
010220*
010230 set-render.
010240
010250      PERFORM asciiZ thru ex-asciiZ.     
010260
010270      move agar-widget       to agar-object
010280
010290      perform get-class      thru ex-get-class.
010300      
010310      move zero to ind
010320      
010330      inspect agar-text tallying ind for all "("
010340      inspect agar-text tallying ind for all ")"
010350      if ind = 2
010360       perform varying ind from function length(agar-text) 
010370        by -1 until ind = zero
010380       or agar-text(ind:1) = ")"
010390       continue
010400       end-perform
010410       subtract 1 from ind
010420       move zeros to tab-num
010430       move length of tab-num to ind2
010440       perform varying ind from ind by -1 until ind = zeros
010450       or agar-text(ind:1) = "("
010460       move agar-text(ind:1) to tab-num(ind2:1)
010470       subtract 1 from ind2
010480       end-perform
010490       
010500       if tab-num > 100 
                move 100 to tab-num 
             end-if
             move spaces to agar-text
            
010510       perform varying ind from 1 by 1 until ind > tab-num
010520       move "x"       to agar-text(ind:1)
           
010530       end-perform

            PERFORM asciiZ thru ex-asciiZ.   
010540       
010550
010560
010570      evaluate agar-class
010580         
010590         when "text"

               
010610         call static "AG_TextboxSizeHint"
010620          using by value agar-widget 
010630           by reference agar-text 
010640       
010650         when "label"
010660            
010670            call static "AG_LabelSizeHint"
010680             using by value agar-widget 
010630               by reference agar-text 
010700            end-call
010710         
010720         when "combo"
010730            
010740            call static "AG_ComboSizeHint"
010750             using by value agar-widget 
10630               by reference agar-text 
010770              by value 10
010780            end-call
010790
010800
010810         when other  
010820            display "unrecognized render call " 
010830             function trim (agar-text) upon syserr
010840            continue
010850
010860      end-evaluate.             
010870      
010880 ex-set-render.
010890      exit.    
010900
010910
010920
010930 addfixed.
010940
010950           
010960      call static "AG_FixedNew" using
010970         by value agar-object
010980          by value AG-FIXED-DEFAULT
010990             returning agar-widget.
011000       
011010      move agar-widget   to agar-fixed.
011020      
011030    *>   call static "AG_FixedSizeHint" using
011040    *>      by value agar-widget
011050    *>         by value agar-x
011060    *>          by value agar-y 
011070    *>              returning omitted.
011080         
011090 ex-addfixed.
011100      exit.
011110
011120 addradio.
011130
011140      call static "AG_RadioNew" using
011150          by value agar-object
011160          by value agar-null-pointer
011170                returning agar-widget.
011180                     
011190 ex-addradio.
011200      exit.
011210
011220
011230 addbutton.
011240
011250      PERFORM asciiZ thru ex-asciiZ.     
011260      
011270      call static "AG_ButtonNewS" using
011280         by value agar-object
011290*                by value AG-BUTTON-HFILL
011300*            by value AG-BUTTON-NOEXCL 
011310              by value AG-BUTTON-HFILL
011320        *>   by value AG-BUTTON-DEFAULT
011330            by content  agar-text 
011340               returning agar-widget.
011350         
011360 ex-addbutton.
011370      exit.
011380 
011390 
011400 addcheck.
011410
011420      PERFORM asciiZ thru ex-asciiZ.     
011430      
011440      call static "AG_CheckboxNewS" using
011450         by value agar-object
011460           by value AG-BOX-HFILL
011470            by content  agar-text 
011480               returning agar-widget.
011490         
011500 ex-addcheck.
011510      exit.
011520 
011530 
011540 additem.
011550
011560       move agar-widget       to agar-object
011570      
011580       perform get-class thru ex-get-class.
011590
011600       PERFORM asciiZ thru ex-asciiZ.    
011610      
011620       evaluate agar-class
011630
011640          when "radio" 
011650      
011660         call "AG_RadioAddItemS" using
011670          by value agar-widget
011680            by content  agar-text  

011700          end-call
011710          
011720          when "combo"
011730
011740              
011750        
011760         set address of combo-instance  to agar-widget 
011770
011780      *> display "combo" function length(combo-instance)
011790      *>    display "combo-flags" function length(combo-flags)
011800      *>    display "combo-tbox" function length(combo-tbox)
011810      *>    display "combo-tlist" function length(combo-tlist)
011820      *>    display "combo-wsaved" function length(combo-wsaved)
011830      *>    display "combo-hsaved" function length(combo-hsaved)
011840      *>    display "combo-wprelist" function length(combo-wprelist)
011850      *>    display "combo-hprelist" function length(combo-hprelist)
011860
011870         call static "AG_TlistAddS" using
011880          by value combo-tlist
011890           by value agar-null-pointer
011900            by content  agar-text  

011920          end-call


                 when "menu"
                 
                 display "todo menu item"
011930     
011940            when other display 
011950             " additem not yet implemented for "
011960                  function trim(agar-class)
011970             
011980        end-evaluate.
011990         
012000 ex-additem.
012010      exit.
012020 
012030  
012040 addtext.
012050
012060      PERFORM asciiZ thru ex-asciiZ.     
012070      
012080      call static "AG_TextboxNewS" using
012090         by value agar-object
012100            by value AG-TEXTBOX-DEFAULT 
012110            by content  agar-text 
012120               returning agar-widget.
012130
012140     call static "AG_TextboxSizeHint"
012150      using by value agar-widget 
012160       by reference agar-text.
012170       
012180                 
012190 ex-addtext.
012200      exit.
012210 
012220  
012230 addnumeric.
012240
012250      PERFORM asciiZ thru ex-asciiZ.     
012260      
012270      call static "AG_NumericalNewS" using
012280         by value agar-object
012290            by value AG-NUMERICAL-DEFAULT 
012300             by value 0
012310              by content  agar-text 
012320               returning agar-widget.
012330           
012340 ex-addnumeric.
012350      exit.
012360 
012370 
012380 
012390 addcombo.
012400
012410      PERFORM asciiZ thru ex-asciiZ.     
012420      
012430      call static "AG_ComboNewS" using
012440         by value agar-object
012450            by value AG-COMBO-HFILL
012460             by content  agar-text 
012470               returning agar-widget.
012480
012490     call static "AG_ComboSizeHint"
012500      using by value agar-widget 
012510       by content agar-text
012520       by value 3
012530         returning omitted.
012540
012550     
012560     
012570 ex-addcombo.
012580      exit.
012590 
012600 addcheckto.
012610
012620       PERFORM addcheck              thru ex-addcheck.
012630       
012640       move agar-widget              to agar-object
012650       set agar-callback             to entry agar-procedure.
012660                                        
012670       move "checkbox-changed" & X"00"  to agar-event.
012680       move "setevent"               to agar-function.
012690       perform setevent              thru ex-setevent.
012700             
012710 ex-addcheckto.
012720      exit.
012730
012740 addbuttonto.
012750
012760       PERFORM addbutton             thru ex-addbutton.
012770       
012780       move agar-widget              to agar-object
012790       set agar-callback             to entry agar-procedure.
012800                                        
012810       move "button-pushed" & X"00"  to agar-event.
012820       move "setevent"               to agar-function.
012830       perform setevent              thru ex-setevent.
012840             
012850 ex-addbuttonto.
012860      exit.
012870
012880
012890 set-name.
012900
012910      PERFORM asciiZ thru ex-asciiZ.
012920           
012930      call static "AG_ObjectSetName" using
012940         by value agar-object
012950            by reference   agar-text 
012960               returning  omitted.
012970 
012980 ex-set-name.
012990      exit. 

012890 set-prompt.
012900
012910      PERFORM asciiZ thru ex-asciiZ.
012920           
012930      call static "AG_TextboxSetPlaceholder" using
012940         by value agar-object
012950            by reference   agar-text.

012980 ex-set-prompt.
012990      exit. 
013000
013000
013010
013020 set-position.
013030
013040     evaluate function upper-case(agar-text)
013050        when  "NONE"            MOVE 0 TO agar-int
013060        when  "TOPLEFT"         MOVE 1 TO agar-int
013070        when  "TOPCENTER"       MOVE 2 TO agar-int
013080        when  "MIDDLERIGHT"     MOVE 3 TO agar-int
013090        when  "MIDDLELEFT"      MOVE 4 TO agar-int
013100        when  "MIDDLECENTER"    MOVE 5 TO agar-int
013110        when  "MIDDLERIGHT"     MOVE 6 TO agar-int
013120        when  "BOTTONLEFT"      MOVE 7 TO agar-int
013130        when  "BOTTONCENTER"    MOVE 8 TO agar-int
013140        when  "BOTTONPRIGHT"    MOVE 9 TO agar-int
013150        when  "LAST"            MOVE 10 TO agar-int
013160           
013170     end-evaluate.
013180
013190       if agar-text(1:2) numeric
013200          move zeros             to agar-binary
013210          move agar-text(1:2)    to agar-binary(4:2)
013220             move agar-binary    to agar-int                               
013230       else
013240       if agar-text(1:1) numeric
013250          move zeros             to agar-binary
013260          move agar-text(1:1)    to agar-binary(5:1)
013270           move agar-binary    to agar-int.           
013280          
013290      call static "AG_WindowSetPosition" using
013300         by value agar-object
013310            by value agar-int 
013320             by value 1
013330               returning  omitted.
013340 
013350 ex-set-position.
013360      exit. 
013370
013380
013390
013400 set-disposition.
013410
013420     evaluate function upper-case(agar-text) (1:1)
013430        when  "V"            MOVE 0 TO agar-int
013440        when  OTHER          MOVE 1 TO agar-int
013450           
013460     end-evaluate.
013470
013480     call static "AG_RadioSetDisposition" using
013490         by value agar-object
013500            by value agar-int 
013510              returning  omitted.
013520 
013530 ex-set-disposition.
013540      exit. 
013550
013560
013570 get-name.
013580
013590         call static "AG_ObjectGetName" 
013600          using by value agar-object
013610            returning agar-struct.


013630          set address of agar-string  TO agar-struct.
                
013650          move agar-string      to agar-text.
                
                perform asciiZ thru ex-asciiZ.
                      
013680 ex-get-name.
013690      exit. 
013700

013570 get-cursor.
013580
013590         call "AG_TextboxGetCursorPos" 
013600          using by value agar-object
013610            returning agar-int.
                      
013680 ex-get-cursor.
013690      exit. 
013700
       set-cursor.
013580
013590         call "AG_TextboxSetCursorPos" 
013600          using by value agar-object
013610            by value agar-int.
                      
013680 ex-set-cursor.
013690      exit. 
013700
013710
013720 addproperty.
013730
013740      move spaces        to agar-value.
013750
013760      PERFORM asciiZ thru ex-asciiZ.
013770          
013780      call static "AG_SetString" using
013790         by value agar-widget
013800            by reference agar-text
013810             by reference agar-value
013820               returning  agar-dummy.
013830                                  
013840 ex-addproperty.
013850      exit. 
013860*set-debug-on                       
013870
013880 get-property.
013890     
013900     PERFORM asciiZ thru ex-asciiZ.
013910
013920     move spaces                to agar-value
013930     move  LENGTH OF agar-value to agar-int
013940
013950     call static "ag_defined" using
013960        by value agar-widget
013970           by reference agar-text
013980            returning agar-boolean.
013990
014000      
014010     if agar-boolean = 1
014020      call static "AG_GetString" using
014030         by value agar-widget
014040           by reference agar-text
014050            by reference agar-value
014060               by reference agar-int
014070                returning agar-size
014080                 else
014090                 display "get string non allowed for "
014100                     agar-text " with object:"
014110                         agar-widget.
014120               
014130 ex-get-property.
014140      exit. 
014150
014160 get-caption.
014170
014180         move "caption"     to agar-text.
014190
014200         perform get-property thru ex-get-property.
014210
014220 ex-get-caption.
014230      exit.
014240 
014250
014260
014270 get-text.
014280
014290    *>     call static "AG_TextboxDupString" 
014300    *>       using by value agar-widget
014310    *>          returning agar-struct.
014320            
014330    *>         move spaces to agar-text.
014340                        
014350    *>         set address of agar-string  TO agar-struct.
014360    *>         move agar-string            to agar-text.
014370           
014380    *>         PERFORM asciiZ thru ex-asciiZ.   
014390    *>         display "1 nuova=" agar-text(1:40).
014400    
014410          move 50 to agar-size
014420          move spaces        to agar-text
014430          call static "AG_TextboxCopyString"
014440           using by value agar-widget
014450            by reference agar-text
014460              by value agar-size
014470              returning agar-int.
014480
014490              
014500    *>    move length of agar-text   to agar-size
014510    *>    move "federico"                 to agar-text
014520       
014530    *>    call static "AG_TextboxBindASCII" 
014540    *>     using by value agar-widget
014550    *>           by reference agar-text
014560    *>           by reference agar-size.
014570        
014580    *>   display "valore=" agar-text(1:20).
014590      
014600 ex-get-text.
014610      exit. 
014620
014630
014640
014770
014780 find.
014790            
015040      PERFORM asciiZ thru ex-asciiZ.   
014800                                 
014810       call  "ag_object_find_child" 
014820          using by value agar-object
014830              by reference agar-text
014840                returning agar-widget.
014850
014860 
014870                
014880*     move agar-text to stringa
014890*       
014900*     call  "AG_ObjectFind" 
014910*      using by value agar-object
014920*        by reference stringa
014930*          returning agar-widget.      
014940*                     
014950*                     * display "finisco con " agar-widget.
014960                                          
014970 ex-find.
014980      exit.
014990
015000*set-debug-off
015010
015020 set-text.
015030
015040      PERFORM asciiZ thru ex-asciiZ.   
015050          
015060      call static "AG_TextboxSetString" 
015070      using   by value agar-widget
015080              by content agar-text.
015090
015100 ex-set-text.
015110      exit. 
015120
015130
015140
015150 moveto.
015160
015170 
015180      move agar-widget       to agar-object
015190
015200      perform get-class      thru ex-get-class.
015210 
015220      evaluate agar-class
015230
015240      when "form" continue
015250          
015260      when "fixed"
015270
015280      call  "AG_FixedMove" 
015290        using by value agar-fixed
015300          by value agar-widget
015310           by value agar-x
015320            by value agar-y
015330              returning omitted
015340
015350      when other 
015360     
015370      call  "AG_WidgetSetPosition" using
015380         by value agar-widget
015390            by value agar-x
015400             by value agar-y 
015410               returning omitted
015420               
015430       end-evaluate.
015440             
015450   
015460 ex-moveto.
015470      exit.
015480      
015490
015500
015510 sizeto.
015520     
015530      move agar-widget       to agar-object
015540
015550      perform get-class      thru ex-get-class.
015560
015570      evaluate agar-class
015580
015590         when "form"
015600     
015610          call "AG_WindowSetGeometryAligned" 
015620          using by value agar-form 0 agar-width agar-height 
015630         returning omitted
015640         end-call
015650         
015660         when "text"
015670
015680         call static "AG_TextboxSizeHintPixels"
015690          using by value agar-widget 
015700            agar-width agar-height
015710         end-call
015720         
015730         when "label"
015740
015750          compute agar-size = agar-width / 10
015760           on size error move 100 to agar-size
015770          end-compute
015780          
015790          move spaces to agar-text
015800          
015810          if agar-size < function length (agar-text)
015820          move X"00" to agar-text(agar-size:)
015830          move X"00" to agar-text(100:)
015840          end-if
015850            
015860          call static "AG_LabelSizeHint" using
015870             by value agar-widget
015880                by reference agar-text
015890                     returning omitted
015900          end-call
015910     
015920     
015930         when "slider"
015940
015950         call static "AG_SliderSetControlSize"
015960          using by value agar-widget 
015970            agar-width 
015980         end-call
015990                      
016000       
016010         when other  
016020         
016030         call static "AG_WidgetSetSize" using
016040             by value agar-widget
016050                by value agar-width
016060                 by value  agar-height
016070                     
016080          end-call
016090          
016100      end-evaluate.             
016110             
016120 ex-sizeto.
016130      exit.
016140
016150
016160
016170 set-width.
016180 
016190         call "AG_SetInt" using
016200         by value agar-widget
016210            by reference z"width"
016220             by value agar-width
016230               returning  omitted.
016240  
016250         perform get-width thru ex-get-width.
016260         
016270         move agar-int      to agar-width
016280         
016290         perform get-height thru ex-get-height.
016300         
016310         move agar-int      to agar-height.
016320         
016330         perform sizeto    thru ex-sizeto.
016340
016350
016360 ex-set-width.
016370      exit.
016380
016390 set-height.
016400     
016410         call  "AG_SetInt" using
016420         by value agar-widget
016430            by reference z"height"
016440             by value agar-height
016450               returning  omitted.
016460         
016470         perform get-height thru ex-get-height.
016480         
016490         move agar-int      to agar-height.
016500         
016510         perform get-width thru ex-get-width.
016520
016530         move agar-int       to agar-width
016540         
016550         perform sizeto    thru ex-sizeto.
016560               
016570             
016580 ex-set-height.
016590      exit.
016600
016610 get-width.
016620  
016630      call static "ag_defined" using
016640         by value agar-widget
016650            by reference z"width"
016660             returning agar-boolean.
016670             
016680      if agar-boolean = 1
016690      call "AG_GetInt" using
016700         by value agar-widget
016710            by reference z"width"
016720             returning agar-int
016730       else
016740      move zeros to agar-int.
016750       
016760 ex-get-width.
016770      exit.
016780
016790 get-height.
016800 
016810      call static "ag_defined" using
016820         by value agar-widget
016830            by reference z"height"
016840             returning agar-boolean.
016850             
016860      if agar-boolean = 1
016870      
016880      call  "AG_GetInt" using
016890         by value agar-widget
016900            by reference z"height"
016910             returning agar-int
016920       else   
016930        
016940      move zeros to agar-int.
016950      
016960 ex-get-height.
016970      exit.
016980
016990
017000
017010
017020
017030 set-top.
017040 
017050         call "AG_SetInt" using
017060         by value agar-widget
017070            by reference z"top"
017080             by value agar-y
017090               returning  omitted.
017100  
017110         perform get-top   thru ex-get-top.
017120         
017130         move agar-int      to agar-y
017140         
017150         perform get-left   thru ex-get-left.
017160         
017170         move agar-int      to agar-x.
017180         
017190         perform moveto    thru ex-moveto.
017200
017210 ex-set-top.
017220 
017230      exit.
017240
017250 set-left.
017260     
017270         call  "AG_SetInt" using
017280         by value agar-widget
017290            by reference z"left"
017300             by value agar-x
017310               returning  omitted.
017320         
017330         perform get-left thru ex-get-left
017340         
017350         move agar-int     to agar-x
017360         
017370         perform get-top   thru ex-get-top
017380
017390         move agar-int     to agar-y
017400         
017410         perform moveto    thru ex-moveto.
017420 
017430             
017440 ex-set-left.
017450      exit.
017460
017470 get-top.
017480  
017490      call static "ag_defined" using
017500         by value agar-widget
017510            by reference z"top"
017520             returning agar-boolean.
017530             
017540      if agar-boolean = 1
017550      call "AG_GetInt" using
017560         by value agar-widget
017570            by reference z"top"
017580             returning agar-int
017590       else
017600      move zeros to agar-int.
017610       
017620 ex-get-top.
017630      exit.
017640
017650 get-left.
017660 
017670      call static "ag_defined" using
017680         by value agar-widget
017690            by reference z"left"
017700             returning agar-boolean.
017710             
017720      if agar-boolean = 1
017730      
017740      call  "AG_GetInt" using
017750         by value agar-widget
017760            by reference z"left"
017770             returning agar-int
017780       else   
017790        
017800      move zeros to agar-int.
017810      
017820 ex-get-left.
017830      exit.
017840
017850
017860
017870
017880 set-font.
017890
017900     if agar-text = spaces move "Courier" to agar-text.
017910    
017920     call static "AG_SetStyle" using
017930         by value agar-widget
017940            by reference "font-family"
017950             by reference agar-text.
017960 
017970      
017980 ex-set-font.
017990      exit.
018000 
018010
018020 set-fontstyle.
018030
018040      call static "AG_SetStyle" using
018050         by value agar-widget
018060            by reference  "font-style"
018070             by reference  agar-text.
018080              
018090 ex-set-fontstyle.
018100      exit.
018110      
018120 set-fontweight.
018130     
018140      evaluate function upper-case(agar-text) 
018150         when "BOLD"     move "bold"    to local-buffer
018160         when "NORMAL"   move "normal"  to local-buffer
018170         when "!PARENT"  move "!parent" to local-buffer
018180         when OTHER      move "normal"  to local-buffer
018190      end-evaluate
018200 
018210      call static "AG_SetStyle" using
018220         by value agar-widget
018230            by reference  "font-weight"
018240             by reference  "bold".
018250              
018260 ex-set-fontweight.
018270      exit.
018280
018290
018300 set-bordercolor.
018310
018320     move agar-text to agar-color
018330
018340      call static "AG_SetStyle" using
018350         by value agar-widget
018360            by reference  "border-color"
018370             by reference  agar-color.
018380              
018390              
018400 ex-set-bordercolor.
018410      exit.
018420
018430      
018440 set-fontsize.
018450
018460     if agar-text = spaces move "10pts" to agar-text.
018470    
018480     call static "AG_SetStyle" using
018490         by value agar-widget
018500            by content "font-size"
018510            by content  agar-text 
018520             returning omitted.
018530     
018540 ex-set-fontsize.
018550      exit.
018560
018570 set-error.
018580
018590 
018600     if agar-text = spaces
018610        move z"Messaggio di errore" to agar-text.
018620
018630     PERFORM asciiZ thru ex-asciiZ.                 
018640    
018650     call static "AG_TextError" using
018660        by content  agar-text.
018670
018680 ex-set-error.
018690      exit.
018700
018710 set-warning.
018720
018730
018740     PERFORM asciiZ thru ex-asciiZ.                 
018750    
018760     if agar-text = spaces
018770        move z"Avviso generico per l'operatore" to agar-text.
018780    
018790     call static "AG_TextWarning" using
018800        by content  Z"Segnalazione per l'operatore"
018810        by reference  agar-text.
018820
018830 ex-set-warning.
018840      exit.
018850
018860 set-info.
018870
018880     PERFORM asciiZ thru ex-asciiZ.                 
018890
018900     if agar-text = spaces
018910        move z"informazioni per l'operatore" to agar-text.
018920    
018930     call  "AG_TextMsg" using
018940        by value 2
018950        by reference  agar-text.
018960                    
018970 ex-set-info.
018980      exit.
018990
019000
019010
019020 
019030 size-agar-text.
019040
019050      perform varying agar-int from length of agar-text by -1
019060       until agar-int = zeros
019070       or ( agar-text(agar-int:1) > spaces
019080       and agar-text(agar-int:1) not = x"00" )
019090       continue
019100      end-perform.
019110
019120 ex-size-agar-text.
019130      exit.
019140
019150 asciiZ.
019160
019170      perform size-agar-text      thru ex-size-agar-text
019180
019190      add 1 to agar-int.
019200      move X"00" to agar-text(agar-int:1).
019210
019220 ex-asciiZ.
019230      exit.
019240
019250 compute-size.
019260
019270      perform size-agar-text      thru ex-size-agar-text.
019280
019290      if agar-int = zeros move 1 to agar-int.
019300
019310      if agar-int < 5
019320      compute agar-width = agar-int * 10
019330      else
019340      if agar-int > 10
019350      compute agar-width = agar-int * 6
019360      else
019370      compute agar-width = agar-int * 12.
019380
019390*      perform is-check-button thru ex-is-check-button.
019400
019410      if agar-boolean = agar-true
019420        add 15         to agar-width.
019430
019440      perform setsize  thru ex-setsize.
019450
019460 ex-compute-size.
019470      exit.
019480
019490 setsize.
019500
019510      move agar-width   to agar-use-width.
019520      move agar-height  to agar-use-height.
019530
019540      if agar-use-width  = zeros go to ex-setsize.
019550      if agar-use-height = zeros go to ex-setsize.
019560
019570
019580      move zeros       to agar-use-width.
019590      move zeros       to agar-use-height.
019600
019610 ex-setsize.
019620      exit.
019630
019640
019650
019660 get-class.
019670         
019680          call "AG_ObjectGetClassName" 
019690               using by value agar-object 
019700                 by value 0
019710                 returning local
019720                 
019730            set address of local-string  TO local.
019740      
019750            Evaluate  function upper-case( local-string(4:3))
019760            
019770             when "WIN"      MOVE "form"         to agar-class
019780             when "BOX"      MOVE "box"          to agar-class
019790             when "LAB"      MOVE "label"        to agar-class
019800             when "TEX"      MOVE "text"         to agar-class
019810             when "RAD"      MOVE "radio"        to agar-class
019820             when "CHE"      MOVE "check"        to agar-class
019830             when "COM"      MOVE "combo"        to agar-class
019840             when "BUT"      MOVE "button"       to agar-class
019850             when "PAN"      MOVE "pane"         to agar-class
019860             when "PRO"      MOVE "progress"     to agar-class
019870             when "SLI"      MOVE "slider"       to agar-class
019880             when "SCR"      MOVE "scroll"       to agar-class
019890             when "FIX"      MOVE "fixed"        to agar-class
019890             when "MEN"      MOVE "menu"         to agar-class
019900             when other 
019910               move 
019920                agar-string(4:3)         to agar-class
019930                  display "new class -->" agar-class
019940                                        
019950            end-evaluate.                   
019960          
019970 ex-get-class.
019980         exit.
019990
020000         
020010
020020
020030 agar-do-debug.
020040
020050      Display "---------------------------------------------".
020060
020070      if agar-started not = agar-true
020080       and agar-function not = "initialize"
020090       Display "agarcob starts:does automatic initialize"
020100       else
020110       Display "agarcob executes  :" function  trim(agar-function).
020120
020130       perform trace-debug thru ex-trace-debug.
020140
020150 ex-agar-do-debug.
020160       exit.
020170
020180 trace-debug.
020190
020200       Display "agar-form:             " agar-form.
020210       Display "agar-main:             " agar-main.
020220       Display "agar-panel:            " agar-panel.
020230       Display "agar-object:           " agar-object.
020240       Display "agar-parent:           " agar-parent.
020250       Display "agar-event:            " agar-event.
020260       Display "agar-procedure:        " agar-procedure.
020270       Display "agar-widget:           " agar-widget.
020280       Display "agar-text:             "
020290         function trim(agar-text).
020300       Display "agar-x:                "  agar-x.
020310       Display "agar-y:                "  agar-y.
020320       Display "agar-width:            "  agar-width.
020330       Display "agar-heigth:           "  agar-height.
020340       Display "agar-boolean:          "  agar-boolean.
020350       Display "agar-int:              "  agar-int.
020360       Display "agar-ind:              "  agar-ind.
020370       Display "agar-class:             "
020380         function trim(agar-class).
020390       
020400 ex-trace-debug.
020410      exit.
020420
020430 agar-after-debug.
020440
020450      if agar-started not = agar-true
020460       and agar-function not = "initialize"
020470       Display "agarcob after:automatic initialize"
020480       else
020490       Display "agarcob after:       "
020500       function  trim(agar-function).
020510
020520       perform trace-debug thru ex-trace-debug.
020530
020540       Display "agarcob has processed:        "
020550       function  trim(agar-function).
020560
020570       Display " ".
020580       Display " ".
020590       Display " ".
020600       Display "             *** end:"
020610       function  trim(agar-function) " "
020620        function trim("***").
020630
020640 ex-agar-after-debug.
020650      exit.
020660
020670 end program agarcob.
020680
020690 identification division.
020700 program-id. DefaultDestroy.
020710 data division.
020720 working-storage section.
020730
020740      copy "global".
020750
020760 linkage section.
020770
020780 procedure division .
020790
020800
020810         call "AG_Terminate" using by value 0.
020820
020830 exit program.
020840 end program DefaultDestroy.
