
*!*	Define pad UtilPad of _mSysMenu prompt "Util" key Alt+U
*!*	ON PAD UtilPad OF _MSYSMENU ACTIVATE POPUP Utils
*!*	DEFINE POPUP Utils MARGIN RELATIVE

*!*	DEFINE BAR CNTBAR('Utils') + 1 OF Utils PROMPT "Clip Error"
*!*	on selection bar CNTBAR('Utils') of Utils  ;
*!*	  _cliptext = ExecScript( [local lae(1)]+Chr(13)+;
*!*	      'return Iif( aerror(lae)=0, [], [Error# ]+Transform(lae(1))+[, "]+lae(2)+["  Code: ]+Message(1))')

*!*	DEFINE BAR CNTBAR('Utils') + 1 OF Utils PROMPT "\-"

*!*	DEFINE BAR CNTBAR('Utils') + 1 OF Utils PROMPT 'Reset IDE'
*!*	on selection bar CNTBAR('Utils') of Utils do wait "?????????"

*SET STEP ON 
_screen.Caption="PtgSupport:"
_screen.Icon ="D:\VFP\PtgSupport\PtgSupport.ico"
*_screen.Icon ='D:\VFP\ACC\GRAPHICS\TOOLS.ICO'

DEFINE BAR 20 OF _msystem KEY Alt+F1 PROMPT "Hacker's Guide"
ON SELECTION BAR 20 OF _msystem RUN /n hh.exe D:\Documentacii\hackfox7.chm

_path1="d:\vfp\PtgSupport\"
SET DATE DMY 
SET CENTURY on
*set procedure to d:\vfp\acc\progs\lib1
*set classlib to  d:\vfp\acc\libs\acc

INKEY(0.2)

IF NOT WEXIST("standard")
   KEYBOARD "{ALT+V}{ENTER}{PGDN}{UPARROW}{SPACEBAR}{CTRL+W}"
ENDIF 

IF WEXIST("standard")
   SHOW WINDOW standard 
   ACTIVATE WINDOW standard
ENDIF    