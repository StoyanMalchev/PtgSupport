PARAMETERS cPath1
IF PCOUNT() = 1 
   retu   
ENDIF    

PUBLIC lcnz
lcnz=CREATEOBJECT("lcnz_")

DECLARE INTEGER SetForegroundWindow IN USER32 INTEGER
DECLARE INTEGER ShowWindow IN USER32 INTEGER, INTEGER
*DECLARE INTEGER FindWindow IN USER32 STRING @ , STRING @

*lnHWND=FindWindow(0,application.Caption)
lnHWND=_screen.hwnd

DECLARE INTEGER InternetCheckConnection IN wininet  ;
  STRING lpszUrlSTRING, INTEGER dwFlags, INTEGER dwReserved

DECLARE INTEGER ShellExecute IN SHELL32.dll ;
    INTEGER nWinHandle,;
    STRING cOperation,;
    STRING cFileName,;
    STRING cParameters,;
    STRING cDirectory,;
    INTEGER nShowWindow
    
DECLARE INTEGER GetActiveWindow IN user32     

PUBLIC gcDBID, gcDBID_

PUBLIC _path1
_Path1=ADDBS(SYS(5)+SYS(2003))
_screen.AddProperty("Path1_", _Path1)

IF "program files" $ LOWER(_Path1) OR "syswow64" $ LOWER(_Path1)
	= ADIR(la, '.', 'D')	
	_Path1 = ADDBS("c:\ptg\"+la[1,1])
	IF NOT DIRECTORY(_Path1)
	   MD (_Path1)
	ENDIF 		
ENDIF 

lcLogFile="c:\ptg\support1.log"

IF FILE(lcLogFile)
   =ADIR(la, lcLogFile)
   IF la[1,2]> 20000
      COPY FILE c:\ptg\support1.log to c:\ptg\support1_.log      
      STRTOFILE("PtgSupport:", lcLogFile)
   ENDIF 
	lnRows = ALINES(la, FILETOSTR(lcLogFile) )
	IF lnRows > 200
		lnLogFile = FCREATE( lcLogFile)
		lnJ=lnRows - 100
		FOR lnJ = lnRows - 100 TO lnRows
	      =FPUTS(lnLogFile, la[lnJ])       	  	   
		ENDFOR 
		=FCLOSE(lnLogFile)
	ENDIF 
ENDIF 

SET CLASSLIB TO tools
SET CLASSLIB TO PTGSupp ADDITIVE 
PUBLIC goWS,ptgDotNet
=CREATEOBJECT("ws")

IF TYPE("goWS")#"O"
   IF regAsm("")
	  CREATEOBJECT("ws")
	  IF TYPE("goWS")#"O"
         IF regAsm("64")
            =CREATEOBJECT("ws")
         ENDIF 
      ENDIF 	  
   ENDIF 
   
   IF TYPE("goWS")#"O"
      MESSAGEBOX("RegAsm PtgDotNet.dll ",32,"Проблем:", 3000)
   ENDIF 
   
ENDIF 

lcPCN = "0"
*!*	if InternetCheckConnection("http://www.pitagor.com",0x01,0)=1
*!*	   lcPCID = IIF(FILE("c:\ptg\id.ptg"), MLINE(FILETOSTR("c:\ptg\id.ptg"), 1), "???")
*!*	   IF FILE("ptg-xp.lcnz")
*!*	      lcPCN = LTRIM(MLINE(FILETOSTR("ptg-xp.lcnz"), 3))
*!*	   ENDIF 
*!*	   
*!*	   lcURL = "http://www.pitagor.com/log.php?"+;
*!*	        "PC="+lcPCID+"&"+"PCN="+lcPCN+"&"+"User="+gcLcnz                 

*!*	ENDIF 

IF NOT Instance1("Ptg-Support")
   CLEAR EVENTS    
   RETURN 
ENDIF

IF PCOUNT() = 1 
   CD (_Path1)
ENDIF

CD (_Path1)

SET RESOURCE OFF
ON ERROR DO ErrLog WITH ERROR(),PROG(),LINE(), MESSAGE(),MESSAGE(1), SYS(2018)
*ON SHUTDOWN CLEAR EVENTS
ON SHUTDOWN do ShutDown
ON KEY LABEL f12 do sspnd
ON KEY LABEL F9  do stts

_screen.TitleBar = 1
_screen.Caption = "ПП ПИТАГОР: Допълнение за поддържане и контрол на данните"

*=AGetFileVersion(la, Sys(16))	&& la[3] : Description

_screen.AddProperty("dtc", "???")
_screen.AddProperty("cRls", "???")
_screen.AddProperty("nErrFTP", 0)
=AGetFileVersion(la, Sys(16))	&& la[3] : Description
_screen.dtc=la[3]
_screen.cRls=la[4]

PUBLIC objFTP
*SET PROCEDURE TO ftp.prg ADDITIVE
#INCLUDE "ftp.h"
SET PROCEDURE TO ftp.prg ADDITIVE
IF TYPE("objFTP") # "O"
   objFTP = CREATEOBJECT('ftp_service')
ENDIF 
*loFTP.nFlags=BITXOR(loFTP.nFlags, INTERNET_FLAG_PASSIVE) && Reset passive mode

PUBLIC lFTPStop, tmStartFTP
lFTPStop=.f.
tmStartFTP = DATETIME()

IF PCOUNT() = 1 
	DO FORM support WITH .t.	&& СТАРТ СЪС ЗАРЕЖДАНЕТО НА WINDOWS
ELSE
	DO FORM support WITH .f.
ENDIF

READ EVENTS

**
FUNCTION regAsm (c64)
lcDotNet4 = GetEnv("systemroot")+"\Microsoft.NET\Framework&c64\v4.0.30319"
IF NOT DIRECTORY(lcDotnet4)
   RETURN .f.
ENDIF    

lcRegAsm = [ExpandEnvironmentStrings("%SystemRoot%")&"\Microsoft.NET]
lcFW = "\Framework"+c64
lcRegAsm = lcRegAsm + lcFW
lcRegAsm = lcRegAsm + [\v4.0.30319\RegAsm.exe ]

lcErr = ON("error")
lErr=.f.
ON ERROR lErr=.t.

lcVBS= "c:\ptg\RegAsm_.vbs"
*!*		IF os64bits()
*!*	       lcFW = "\Framework64"
*!*		ENDIF 
	
SET TEXTMERGE TO (lcVBS)
SET TEXTMERGE ON NOSHOW	
\\Set wshShell = WScript.CreateObject("WScript.Shell")	
\strCmd = wshShell.<<lcRegAsm>> PtgDotNet.dll -u"
\Set objExec=wshShell.Exec(strCmd)

SET TEXTMERGE OFF
SET TEXTMERGE TO
ShellExecute(GetActiveWindow(), "open", lcVBS, "", SYS(2023), 1)

SET TEXTMERGE TO (lcVBS)
SET TEXTMERGE ON NOSHOW	
\\Set wshShell = WScript.CreateObject("WScript.Shell")	
\strCmd = wshShell.<<lcRegAsm>> c:\ptg\PtgDotNet.dll -codebase"
\Set objExec=wshShell.Exec(strCmd)
SET TEXTMERGE OFF
SET TEXTMERGE TO

ShellExecute(GetActiveWindow(), "open", lcVBS, "", SYS(2023), 1)

ON ERROR &lcErr

RETURN (NOT lErr)


***************
FUNCTION ErrLog
PARAMETER M.ERRNUM, M.PROGRAM, M.LINE, M.MESS, M.MESS1, M.PARAM

_SCREEN.VISIBLE = .T.
SET ESCAPE OFF
DO CASE
CASE  Wexist("Trace")   
   SUSPEND

CASE M.ERRNUM = 1707	&& * нет индексного файла
  RETRY	&& * тогда мы попробуем еще раз
CASE M.ERRNUM=109	&& * запись заблокирована другим пользователем
  IF MESSAGEBOX("Блокиран запис от друг потребител", 48 + 1, WTITLE("")) = 1
     RETRY
  ELSE
     RETURN
  ENDIF
ENDCASE

PRIVATE K, I
SET safety OFF 
SET TEXTMERGE TO Err.log
SET TEXTMERGE ON NOSHOW		

\<<Date()>> <<Time()>>
IF TYPE("db_.path")="C"
   \\  Папка: <<db_.path>>
ENDIF 
\Проблем:   Метод:<<m.program>> Ред:<<m.line>>
\Message:<<m.mess>>
DO CASE
CASE "error reading file" $ LOWER(m.mess) OR ;
     "file access" $ LOWER(m.mess)
	SET TEXTMERGE OFF	
	SET TEXTMERGE TO
	logEvent( FILETOSTR("Err.Log"), "ErrSupport_.log")
	retu

CASE m.ErrNum=39
  \ Alias: << ALIAS() >>
  \\ << IIF( EMPTY(ALIAS()),"", "Record: " + TRANSFORM( RECNO()) ) >>
CASE m.ErrNum=1884
   Local luDoubleValue
   FOR j=1 TO TAGCOUNT()   
     If PRIMARY(j) 
        luDoublevalue = Evaluate(KEY(j))     
        \Дублиране на първичен ключ :<<KEY(j)>>  За таблица:<<Alias()>>
        \\  Стойност:<<Transform(luDoublevalue)>>
     ENDIF 
     NEXT 
ENDCASE 

\stack

I = 1
* начинаем использовать функцию SYS(16) для определения вызвавшей программы
DO WHILE NOT EMPTY(SYS(16, I))
	* соотвественно все уровни мы записываем в текстовый файл
	\<<i>> <<sys(16,i)>>
	I = I+1
ENDDO

\PtgSupport Стартиран от <<_screen.path1_>>  Текуща папка: <<curdir()>>

SET TEXTMERGE OFF	&& * сообщения могут опять появляться
SET TEXTMERGE TO

logEvent(FILETOSTR("Err.Log"), _Path1+"ErrSupport.log")
logEvent( FILETOSTR("Err.Log"), "ErrSupport_.log")

*!*	SET TEXTMERGE TO (_Path1+"ErrSupport.log") ADDITIVE
*!*	SET TEXTMERGE ON NOSHOW		&& * выключаем вывод на экран 
*!*	\-----------------------------
*!*	\PtgSupport Стартиране от <<_screen.path1_>>  Текуща папка: <<curdir()>>
*!*	\ <<FILETOSTR("Err.Log") >>
*!*	SET TEXTMERGE OFF	
*!*	SET TEXTMERGE TO

*!*	_CLIPTEXT = FILETOSTR("Err.Log")
*!*	= MESSAGEBOX("Грешка: Метод:"+m.program+" Ред:"+TRANSFORM(m.line), 48, "Грешка")

QUIT
CLEAR EVENTS 

********************
PROCEDURE Instance1
PARAMETERS myApp
LOCAL hsem, lpszSemName
#DEFINE ERROR_ALREADY_EXISTS 183
Declare Integer GetLastError In win32API
Declare Integer CreateSemaphore In WIN32API ;
        string @ lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, string @ lpName

hsem = CreateSemaphore(0, 0, 1, myApp)
IF (hsem # 0 And GetLastError() == ERROR_ALREADY_EXISTS)
   _SCREEN.VISIBLE = .F.   
   MESSAGEBOX("Приложението вече е стартирано", 48, "PtgSupport")

   CLEAR EVENTS 
   QUIT 
   RETURN .F.
ENDIF
RETURN .T.


***************
PROCEDURE sspnd
IF TYPE("_vfp.ActiveForm.Activecontrol")="O"
*   MESSAGEBOX("Обект:"+_vfp.ActiveForm.Activecontrol.name)
   lo_ = _vfp.ActiveForm.Activecontrol
ENDIF 

Keyboard '{CTRL+F2}'

IF WEXIST("Standard")
  ACTIVATE WINDOW Standard
ELSE
  KEYBOARD "{ALT+V}{ENTER}{PGDN}{UPARROW}{SPACEBAR}{CTRL+W}"
ENDIF  

IF Wexist("Trace")   
   SUSPEND
ENDIF 

*************
FUNC ShutDown

SET TEXTMERGE OFF	
SET TEXTMERGE TO
*_Path1=ADDBS(SYS(5) + SYS(2003))
SET TEXTMERGE TO (ADDBS(_Path1)+"xLogs.txt") ADDITIVE
SET TEXTMERGE ON NOSHOW		&& * выключаем вывод на экран 
\ShutDown: <<Date()>> <<Time()>>
SET TEXTMERGE OFF	
SET TEXTMERGE TO

ON ESCAPE
SET ESCAPE ON
ON KEY LABEL f11
ON KEY LABEL f12

ON ERROR
ON SHUTDOWN
DO WHILE TXNLEVEL() > 0
   ROLLBACK   
ENDDO 

SET PATH TO
SET SYSMENU TO DEFAULT
SET SYSMENU ON

_vfp.StatusBar=""
_screen.Closable=.t.
CLEAR PROGRAM
CLEAR MEMORY
CLEAR
RELEASE ALL EXTENDED

MODIFY WINDOW SCREEN
SET TALK ON
SET STATUS BAR ON
*SET RESOURCE ON
SET DEVELOPMENT ON

CLEAR events
DISPLAY MEMORY TO FILE mem NOCONSOLE 

ENDFUNC

******************************
DEFINE CLASS QUnload AS Custom
     oScreen = _Screen
     PROCEDURE oScreen.QueryUnload 
     ENDPROC
     PROCEDURE oScreen.Unload 
     ENDPROC
ENDDEFINE

*************
FUNC LogTasks( cMsg)
PRIVATE lnLogFile, lcLogFile
lcLogFile = "c:\ptg\Support1.log"
IF FILE( lcLogFile)  
	lnLogFile = FOPEN( lcLogFile,12)  && If so, open read-write
ELSE
	lnLogFile = FCREATE( lcLogFile)  && If not, create it
ENDIF
IF lnLogFile < 0  && Check for error opening file
	MESSAGEBOX("Грешка: отваряне на "+lcLogFile,48)
	RETURN -1
ELSE  && If no error, write to file
	=FSEEK(lnLogFile,0,2)	
	=FWRITE(lnLogFile, CHR(13)+CHR(10)+;
	  TTOC(DATETIME())+" PC"+lcnz.pc+" "+cMSG)
ENDIF

=FCLOSE(lnLogFile)  && Close file
ENDFUNC

*************
FUNC LogEvent( cMsg, cLogFile)
PRIVATE lnLogFile

IF FILE(cLogFile)  
	lnLogFile = FOPEN(cLogFile,12)  && If so, open read-write
ELSE
	lnLogFile = FCREATE(cLogFile)  && If not, create it
ENDIF
IF lnLogFile < 0  && Check for error opening file
	MESSAGEBOX("Грешка: отваряне на "+cLogFile,48)
	RETURN -1
ELSE  && If no error, write to file
	=FSEEK(lnLogFile,0,2)	
	=FWRITE(lnLogFile, CHR(13)+CHR(10)+TTOC(DATETIME())+" "+cMSG)
ENDIF

=FCLOSE(lnLogFile)  && Close file
ENDFUNC

********************
Define Class Lcnz_ as Custom 
opt    = ""
LcnzID = "????????"
LcnzName= ""
ok=.f.
idPC=SPACE(40)
PC="???"

**************
Procedure init
Private lcLcnz, lcDirID, lcFN, la, lcID2

IF NOT DIRECTORY("c:\ptg")
   MD c:\ptg
ENDIF

Private lcLcnz, lcDirID, lcFN, la

Declare SHORT GetVolumeInformation In Win32API;
  STRING @lpRootPathName, String @lpVolumeNameBuffer,;
  INTEGER nVolumeNameSize, Integer @lpVolumeSerialNumber,;
  INTEGER @lpMaximumComponentLength, ;
  Integer @lpFileSystemFlags, ;
  STRING @lpFileSystemNameBuffer, ;
  Integer nFileSystemNameSize
  
Local lnComplen, lnSysFlags, lnSerN, lcVolName, lcSysName,lnVolSize, lnNameSize

Store 0 To lncomplen, lnsysflags, lnSerN
Store Space(260) To lcVolName, lcSysName
Store Len(m.lcVolName) To m.lnvolsize, m.lnnamesize
lcRoot = 'c:\'
GetVolumeInformation(m.lcRoot, @lcVolname,;
  m.lnvolsize, @lnSerN, @lncomplen, @lnsysflags,;
  @lcsysname, lnnamesize)
  
PUBLIC gcPC, gcLcnz, gcOPT
_screen.AddProperty("LcnzOK", .f.)
_screen.AddProperty("LcnzPC", 0)

gcPC = Right(Trans(m.lnSerN,'@0'),8)
gcPC = SUBSTR(gcPC,5,4)+"-"+SUBSTR(gcPC,1,4)
gcLcnz="_"
gcOPT=SPACE(10)

If file("c:\ptg\Ptg-XP.lcnz")
   lcLcnz= FileToStr("c:\ptg\Ptg-XP.lcnz")
   gcLCNZ = Mline(lcLcnz,1)
   gcOPT = Mline(lcLcnz,4)
   this.opt = m.gcOPT
   this.PC =  ALLTRIM( Mline(lcLcnz,3) )
   _screen.LcnzPC = VAL(this.PC)
ENDIF 
IF FILE("c:\ptg\id.ptg")
   this.idPC = MLINE(FILETOSTR("c:\ptg\id.ptg"), 1)      
ENDIF 

_screen.LcnzOK = ( SUBSTR(gcOPT,8,1)="+" )

ENDDEFINE 

*****************
FUNCTION os64bits
DECLARE Long GetModuleHandle IN WIN32API STRING lpModuleName
DECLARE Long GetProcAddress IN WIN32API Long hModule, String lpProcName

** WOW64 е нещо за изпълняване на 32 битови приложения в 64 битова ОС
** Проверява има ли функция IsWow64Process в kernel32.dll:
llIsWow64ProcessExists = (GetProcAddress(GetModuleHandle("kernel32"),"IsWow64Process") <> 0)
 
llIs64BitOS = .F.
IF llIsWow64ProcessExists 

	DECLARE Long GetCurrentProcess IN WIN32API 
	DECLARE Long IsWow64Process IN WIN32API Long hProcess, Long @ Wow64Process
	lnIsWow64Process = 0
	* IsWow64Process function return value is nonzero if it succeeds 
	* The second output parameter value will be nonzero if VFP application is running under 64-bit OS 
	IF IsWow64Process( GetCurrentProcess(), @lnIsWow64Process) <> 0
		llIs64BitOS = (lnIsWow64Process <> 0)
	ENDIF	
ENDIF	
retu llIs64BitOS 

********
FUNCTION CleaFolder(cFolder)
PRIVATE lnFiles, lnJ, laFiles, lcFolder1
*lcFolder1 = CURDIR()
lcFolder1 = SET('DEFAULT') + SYS(2003)

lnFiles = ADIR(laFiles, ADDBS(cFolder)+"*.*")
SET DEFAULT TO ( cFolder )
FOR lnJ = 1 TO lnFiles
  ERASE ( laFiles[lnJ,1] )
ENDFOR  

CD ( lcFolder1 )
RETURN .t.

**************
Procedure nCSum_
Parameters cStr
priv n,j
n=0
For j=1 to Len(cStr)
  n = n + j*Asc(Substr(cStr,j,1))
  Next  
Return n

**
PROCEDURE Stts
PUSH KEY clea
_screen.Visible = .t.

DISPLAY STATUS TO (_Path1 + "stts.log") NOCONSOLE 
DISPLAY MEMORY TO (_Path1 + "stts.log") ADDITIVE  NOCONSOLE 

IF TYPE("_screen.forms[1].dataLst")="O"
   wait _screen.forms[1].dataLst.controlsource WINDOW AT 0,0 NOWAIT 
endi

MODIFY COMMAND (_Path1 + "stts.log") noedit

POP KEY all


**
*!*	FUNCTION dbID_
*!*	lcID = CHRTRAN(CHRTRAN(lcPath, "\", "_"), ":", "_")
*!*	IF NOT (FILE(lcPath+"dbID.dat")and FILE(lcPath+"prmw.dbf"))
*!*	   RETURN lcID
*!*	ENDIF    

*!*	IF ALINES(la, FILETOSTR(lcPath+"dbID.dat"))=6
*!*		lcSfx = ""
*!*		lnP1=RAT("\", lcPath, 1)
*!*		lnP2=RAT("\", lcPath, 2)
*!*		lcFldr = SUBSTR(lcPath, lnP2+1, lnP1-lnP2-1)

*!*		USE (lcPath+"prmw") IN 0
*!*		DO case
*!*		CASE lcFldr=TRAN(YEAR(prmw.d1))
*!*		   lcSfx = "_"+lcFldr
*!*		CASE LOWER(lcFldr)="z"+TRAN(YEAR(prmw.D1Zpl))
*!*		   lcSfx = "_"+lcFldr
*!*		ENDCASE    
*!*		USE IN prmw

*!*	   IF la[1] # gcLcnz
*!*	      RETURN ""
*!*	   ENDIF 
*!*	   IF VAL(la[6]) == nCSum_(la[1]+la[2]+la[3]+la[4]+la[5]) AND VAL(la[4]) > 0
*!*	      RETURN alltrim(la[4])+lcSfx + "_"+la[5]
*!*	   ELSE
*!*	      RETURN lcID
*!*	   ENDIF    
*!*	ENDIF 

**************
PROCEDURE  dbID_ ( cPath )

SET EXCLUSIVE OFF
gcDBID =""
gcDBID_=""
lcID = CHRTRAN(CHRTRAN(m.cPath, "\", "_"), ":", "_")
IF NOT (FILE(m.cPath+"dbID.ptg")and FILE(m.cPath+"prmw.dbf"))
   gcDBID = lcID
   RETURN .f.
ENDIF    

_screen.forms[1].timerShutDown.enabled=.f.
priv lnSess1, loSess2
lnSess1 = SET("DataSession") 
loSess2 = CreateObject("Session") 
Set DataSession To loSess2.DataSessionId  

lcErr = ON("error")
lErr = .F.
ON ERROR lErr = .T.

IF ALINES(la, FILETOSTR(m.cPath+"dbID.ptg"))=6
	lcSfx = ""
	lnP1=RAT("\", m.cPath, 1)
	lnP2=RAT("\", m.cPath, 2)
	lcFldr = SUBSTR(m.cPath, lnP2+1, lnP1-lnP2-1)

	USE (m.cPath+"prmw") IN 0
	DO case
	CASE lcFldr=TRAN(YEAR(prmw.d1))
	   lcSfx = "_"+lcFldr
	CASE LOWER(lcFldr)="z"+TRAN(YEAR(prmw.D1Zpl))
	   lcSfx = "_"+lcFldr
	ENDCASE    
	USE IN prmw

   IF VAL(la[6]) == nCSum_(la[1]+la[2]+la[3]+la[4]+la[5]) AND VAL(la[4]) > 0   
      gcDBID = alltrim(la[4]) + lcSfx + "_"+IIF(":" $ la[5], "", la[5])
      gcDBID_= alltrim(la[4]) + lcSfx
   ELSE
      gcDBID = lcID
   ENDIF    

   IF la[1] # gcLcnz
      gcDBID=""
   ENDIF 
ENDIF 

CLOSE DATABASES all

ON ERROR &lcErr
SET DATASESSION TO m.lnSess1

_screen.forms[1].timerShutDown.enabled=.f.

RETURN NOT EMPTY(gcDBID)
