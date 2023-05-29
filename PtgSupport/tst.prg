DECLARE INTEGER InternetCheckConnection IN wininet  ;
  STRING lpszUrlSTRING, INTEGER dwFlags, INTEGER dwReserved

lcnz=CREATEOBJECT("lcnz_")
PUBLIC goWS
SET CLASSLIB TO d:\vfp\acc\libs\tools
CREATEOBJECT("ws")
lo=goWS.GetLatestVersions()
lcTxt = ttoc(lo.GetVersionInfoByProduct("PTG-XP").BuildDT)

??

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
