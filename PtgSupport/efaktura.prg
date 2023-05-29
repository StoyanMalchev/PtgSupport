PARAMETERS cEFN, cPath

lcOnError = ON("ERROR")
lErr=.f.
ON ERROR lErr=.t.
LOCAL  loXml
loXml = CreateObject("Chilkat.Xml")

IF TYPE("loXml")#"O" OR lErr
   ON ERROR &lcOnError
   RETURN
ENDIF 

SELECT 0
*!*	CREATE CURSOR ef_ (fan c(10), dt d, ;
*!*	  efn1 c(13), idVAT1 c(15), nm1 c(50), adr1t c(30), adr1s c(40),;
*!*	  efn2 c(13), idVAT2 c(15), nm2 c(50), adr2t c(30), adr2s c(40),;
*!*	    iban c(22), bic c(8), Banka c(40),;
*!*	  nTtl n(12,2), nVat n(12,2), nTxd n(12,2))
  
CREATE CURSOR ef_ (FAN C(10), DT D, ;
  EFN1 C(13), IDVAT1 C(15),NM1 C(50), ADR1T C(30),ADR1S C(40), ;
  EFN2 C(13), IDVAT2 C(15),NM2 C(50), ADR2T C(30),ADR2S C(40), ;
  IBAN C(22), BIC C(8), BANKA C(40), ;
  NTTL N(12,2), NVAT N(12,2), NTXD N(12,2),;
  TD C(10), NDOK N(10,0) )

*lcEFPath = "d:\temp\ef\"
lcTmpDir = ADDBS(SYS(2023)) + "_EF\"
oZip = CreateObject("XStandard.Zip")
lnZips = ADir(la, ADDBS(support.ef)+"*.zip")

For j=1 to lnZips
  lcFN = ADDBS(support.ef) + la(j,1)
  IF NOT DIRECTORY(lcTmpDir)
     MD (lcTmpDir )
  ENDIF 
  ERASE (lcTmpDir+"Invoices\*.*")
  oZip.UnPack(lcFN, lcTmpDir, "*.xml")
  lnXml = ADir(la2, lcTmpDir+"Invoices\*.xml")
  FOR jj=1 TO lnXml
    loXML.LoadXmlFile(lcTmpDir+"Invoices\"+la2[jj,1])
	lo__ = loXML.FindChild("InvoiceHeader")    
    lcDt = lo__.GetChildContent("InvoiceDate")
    lcDt = SUBSTR(lcDt,9,2)+"-"+SUBSTR(lcDt,6,2)+"-"+SUBSTR(lcDt,1,4)
*    IF EMPTY(CTOD(lcDt))
	IF CTOD(lcDt) < DATE()-100
       LOOP
    ENDIF     
    INSERT INTO ef_ ( dt ) VALUES ( CTOD(lcDt) )
    REPLACE Fan WITH lo__.GetChildContent("InvoiceNumber")
    
	lo__ = loXML.FindChild("Recipient")    
    REPLACE efn1 with lo__.GetChildContent("IdentificationNumber")
    REPLACE idVAT1 with lo__.GetChildContent("VATIdentificationNumber")
	lo__ = lo__.FindChild("Address")    
    REPLACE nm1 with lo__.GetChildContent("Name")
    REPLACE adr1t with lo__.GetChildContent("Town")
    REPLACE adr1s with lo__.GetChildContent("Street")
    
	lo__ = loXML.FindChild("Biller")    
    REPLACE efn2 with lo__.GetChildContent("IdentificationNumber")
    REPLACE idVAT2 with lo__.GetChildContent("VATIdentificationNumber")
	lo__ = lo__.FindChild("Address")    
    REPLACE nm2 with lo__.GetChildContent("Name")
    REPLACE adr2t with lo__.GetChildContent("Town")
    REPLACE adr2s with lo__.GetChildContent("Street")

	lo__ = loXML.FindChild("PaymentMethod")    
	lo__ = lo__.FindChild("BeneficiaryAccount")    
    REPLACE IBAN with STRTRAN(lo__.GetChildContent("IBAN")," ","")
    REPLACE BIC with lo__.GetChildContent("BIC")
    REPLACE Banka with lo__.GetChildContent("BankName")
    
    REPLACE nTtl with VAL(loXML.FindChild("TotalGrossAmount").content)

	lo__ = loXML.FindChild("Tax")    
	lo__ = lo__.FindChild("VAT")    
	lo__ = lo__.FindChild("Item")    
    REPLACE nVAT with VAL(lo__.GetChildContent("Amount"))
   
  ENDFOR 
EndFor   

SELECT dist * from ef_ ;
  where efn1==cEFN ;
  ORDER BY dt ;
  INTO CURSOR ef_

BROWSE TITLE lcFN

IF NOT FILE(cPath+"ef.dat")
   SELECT *, SPACE(10)as td, 0000000000 as NDOK from ef_ INTO CURSOR ef_
   COPY TO (cPath+"ef.dat")
ELSE
   USE (cPath+"ef.dat") IN 0
   SELECT ef_
   SCAN
     SCATTER TO loEF
     SELECT ef
     LOCATE for efn2=ef_.efn2 AND fan=ef_.fan
     IF EOF()
        APPEND BLANK 
     ELSE
        IF NOT EMPTY(ef.td)
           LOOP
        ENDIF 
     ENDIF 
     GATHER FROM loEF
   ENDSCAN 

ENDIF 
	
Release oZip
ON ERROR &lcOnError

******************
PROCEDURE shrSales

IF NOT (FILE("prmw.dbf") AND FILE("fa.dbf") AND FILE("ptgacc.dbc"))
   RETURN
ENDIF

lnPeriod = 50
priv lnSess1, loSess2
lnSess1 = SET("DataSession") 
loSess2 = CreateObject("Session") 
Set DataSession To loSess2.DataSessionId  

lcOnErr = ON("error")
lErr = .F.
ON ERROR lErr = .T.

oZip = CreateObject("XStandard.Zip")	   	  
lcTmpDir = ADDBS(SYS(2023)) + "_Ptg\"
IF NOT DIRECTORY(lcTmpDir)
	MD (lcTmpDir)
ENDIF 
  
ERASE (lcTmpDir + "*.*")

USE prmw IN 0
SELECT fa.fan,fa.dt, ;
        ktrg.efn as efn1, ktrg.id_dds as idVAT1, ktrg.name as nm1, ;
		prmw.efn as efn2, prmw.id_dds as idVAT2, prmw.name as nm2, ;        
        sum(fa2.St2)as nTTL, sum(fa2.st2 - fa2.StFar)as nVAT ;
  FROM fa ;
    inner join fa2 ;
      on STR(fa2.gg,4)+Fa2.fas+STR(Fa2.fan,10)=;
         STR(YEAR(fa.dt),4)+fa.fas+str(fa.fan,10);
    inner join ktrg on ktrg.ktrg=fa.ktrg ;
  WHERE fa.dt > DATE()-lnPeriod and EMPTY(fa.fas) ;
  group by fa2.fan ;
  INTO cursor fa_

IF NOT lErr
   COPY TO (lcTmpDir+"ptgINV.dat")
ENDIF

lcArcName = "_invSH"+tran(INT(100000*RAND()))+".zip"	      	
ERASE (lcArcName)
ozip.Pack(lcTmpDir + "ptgINV.dat", lcTmpDir+lcArcName, .F., "", 9)          

   lcHost="ftp.pitagor.com"   
   lcUSR="nl1-wss2\PtgSupport"
   lcPW="DanniSupport1674"


ok = objFtp.OpenInternet(lcUSR, lcPW, lcHost, "21" )
lcFldr= "/ptgTSKS/"
ok_= objFtp.CreateFtpDirectory(lcFldr)
ok_= objFTP.ChangeFTPDirectory(lcFldr)
= objFtp.DeleteFtpFile(JUSTFNAME(lcArcName))
ok = objFtp.PutFtpFile(JUSTFNAME(lcArcName), lcTmpDir+lcArcName, 2)

IF NOT ok THEN
   lnErr = objFtp.GetErrorCode(.F.)
   lcMsg = lcArcName + "  FTP Проблем.:"+objFtp.GetErrorText(lnErr)+ ;
		       	      " грешка "+TRANSFORM(lnErr)+CHR(13)+;
		       	      "Host:"+lcHost+" User:"+lcUSR 

   REPLACE msg WITH TRIM(msg)+CHR(13)+lcMsg IN logs_local					  
   LogTasks(lcMsg)
   =objFTP.CloseInternet() 
   loop
ELSE		      				  
   DIMENSION la[1,1]
   ok=objFTP.GetFTPDirectoryArray(@la, JUSTFNAME(lcArcName))
   IF TYPE("la[1,1]")="C"					   
      ok = (JUSTFNAME(lcArcName)==la[1,1])
	  IF ok
	     lcMsg = TRAN(RECC("fa_"))+" записа"
	  ELSE 
		 lcMsg = "??? неуспешно записване"
	  ENDIF 
   ENDIF 
ENDIF 
IF ok
   lcMsg = lcArcName
ENDIF 

=objFTP.CloseInternet() 

CLOSE DATABASES all
ON ERROR &lcOnErr
SET DATASESSION TO m.lnSess1

INSERT INTO logs_local(dt, tm, task, msg, problems) ;
   VALUES (DATE(), TTOC(DATETIME()), "shINV", lcMsg, IIF(ok,"","!"))
 		    	          	
INSERT INTO logs_global (tm, path, task, msg);
       	VALUES (TIME(),  db_.path, logs_local.task, logs_local.msg)

REPLACE nDuration WITH SECONDS()-lnStart IN logs_Global
_screen.forms[1].lstStatus.Requery() 

****************
PROCEDURE reqINV
** Заявка за споделени еФактури за получателя от папката
IF NOT (FILE("prmw.dbf") AND FILE("fa.dbf") AND FILE("ptgacc.dbc"))
   RETURN
ENDIF

priv lnSess1, loSess2
lnSess1 = SET("DataSession") 
loSess2 = CreateObject("Session") 
Set DataSession To loSess2.DataSessionId  

USE prmw IN 0

lcOnErr = ON("error")
lErr = .F.
ON ERROR lErr = .T.

SELECT 0
IF NOT FILE("reqPTG.dat")
   CREATE TABLE reqPTG.dat FREE ;
      (id n(8), efn c(15), cTM c(20), cTM2 c(20))
   APPEND BLANK   
ENDIF 

m.lnID = RAND()* 100000000
UPDATE reqPTG.dat set id=m.lnID, efn=prmw.efn, ;
   cTM=TTOC(DATETIME(),1), cTm2="" 

lcFN = "getINV_"+TRANSFORM(INT(RAND()*100000))+".rq"
SET TEXTMERGE TO ( lcFN )
SET TEXTMERGE ON NOSHOW 
\\<<reqPTG.id >>
\<<reqPTG.efn>>
\<<reqPTG.cTM>>
SET TEXTMERGE OFF	
SET TEXTMERGE TO

USE IN reqPTG.dat 

   lcHost="ftp.pitagor.com"   
   lcUSR="nl1-wss2\PtgSupport"
   lcPW="DanniSupport1674"

ok = objFtp.OpenInternet(lcUSR, lcPW, lcHost, "21" )
lcFldr= "/ptgTSKS/"
ok_= objFtp.CreateFtpDirectory(lcFldr)
ok_= objFTP.ChangeFTPDirectory(lcFldr)
= objFtp.DeleteFtpFile(JUSTFNAME(lcFN))
ok = objFtp.PutFtpFile(JUSTFNAME(lcFN), lcFN, 2)

IF NOT ok THEN
   lnErr = objFtp.GetErrorCode(.F.)
   lcMsg = lcArcName + "  FTP Проблем.:"+objFtp.GetErrorText(lnErr)+ ;
		       	      " грешка "+TRANSFORM(lnErr)+CHR(13)+;
		       	      "Host:"+lcHost+" User:"+lcUSR 
   LogTasks(lcMsg)
ELSE		      				  
   DIMENSION la[1,1]
   ok=objFTP.GetFTPDirectoryArray(@la, JUSTFNAME(lcFN))
   IF TYPE("la[1,1]")="C"					   
      ok = (JUSTFNAME(lcFN)==la[1,1])
   ENDIF 
ENDIF 
IF ok
   lcMsg = lcFN
ENDIF 
   
=objFTP.CloseInternet() 

ERASE (lcFN )

CLOSE DATABASES all
ON ERROR &lcOnErr
SET DATASESSION TO m.lnSess1

INSERT INTO logs_local(dt, tm, task, msg, problems) ;
   VALUES (DATE(), TTOC(DATETIME()), tasks_local.task, lcMsg, IIF(ok,"","!"))
     		    	          	
INSERT INTO logs_global (tm, path, task, msg);
       	VALUES (TIME(),  db_.path, logs_local.task, logs_local.msg)

REPLACE nDuration WITH SECONDS()-lnStart IN logs_Global
_screen.forms[1].lstStatus.Requery() 
 
*****************
PROCEDURE rreqINV 
** Отговор на заявка за фактури:
CD(ADDBS(db_.Path))
IF NOT (FILE("prmw.dbf") AND FILE("fa.dbf") AND FILE("ptgacc.dbc"))
   RETURN
ENDIF

lnStart=SECONDS()
lcFN = "ptgINV_"+tran(r_.id)+".rrq"

lcOnError = ON("ERROR")
lErr=.f.
ON ERROR lErr=.t.

priv lnSess1, loSess2, lnCount
lnCount=0
lnSess1 = SET("DataSession") 
loSess2 = CreateObject("Session") 

Set DataSession To loSess2.DataSessionId  

lcHost="ftp.pitagor.com"   
lcUSR="nl1-wss2\PtgSupport"
lcPW="DanniSupport1674"

ok = objFtp.OpenInternet(lcUSR, lcPW, lcHost, "21" )

lcTmpDir = ADDBS(SYS(2023)) + "_EF\"
ok_= objFTP.ChangeFTPDirectory("/ptgTSKS")
*ok=objFTP.GetFTPDirectoryArray(@la, "*.rrq")
ok = objFtp.GetFtpFile(lcFN, (lcTmpDir+lcFN) )
IF ok THEN
   SELECT * from (lcTmpDir+lcFN) INTO CURSOR inv_ 
   lcMsg = lcPath+" Получени "+TRANSFORM(RECCOUNT("inv_"))+" фактури:"   
   IF NOT FILE("ef.dat")
** EFN1 : получател   
		CREATE table ef.dat free (FAN C(10), DT D, ;
		  EFN1 C(13), IDVAT1 C(15),NM1 C(50), ADR1T C(30),ADR1S C(40), ;
		  EFN2 C(13), IDVAT2 C(15),NM2 C(50), ADR2T C(30),ADR2S C(40), ;
		  IBAN C(22), BIC C(8), BANKA C(40), ;
		  NTTL N(12,2), NVAT N(12,2), NTXD N(12,2),;
		  TD C(10), NDOK N(10,0) )
	  APPEND FROM DBF("inv_")
      lnCount=RECCOUNT("inv_")
   ELSE
      USE ef.dat IN 0
      SELECT inv_
      SCAN
        SCATTER NAME loINV1 FIELDS EXCEPT nRls
        SELECT ef
        LOCATE FOR fan=STR(inv_.fan,10)
        IF NOT EOF("ef")
           SCATTER NAME loINV0 FIELDS EXCEPT nRls
           SELECT ef
           GATHER NAME loINV1 
        ELSE
           SELECT ef
           APPEND BLANK IN ef
           GATHER name loINV1
           lnCount = lnCount + 1
        ENDIF                 
      ENDSCAN     
      USE IN ef
      USE IN inv_
   ENDIF 
      
   LogTasks(lcMsg)
   ok_ = objFtp.DeleteFtpFile(JUSTFNAME(lcFN))        
   IF NOT ok_
      lnErr = objFtp.GetErrorCode(.F.)
 	  lcMsg = lcFN_ + "  Проблем DeleteFTPFile: "+objFtp.GetErrorText(lnErr)+ ;
   			       " ( "+TRAN(lnErr)+" )"
   ENDIF 
   
ENDIF 

=objFTP.CloseInternet() 

ERASE (lcFN )

CLOSE DATABASES all
ON ERROR &lcOnErr
SET DATASESSION TO m.lnSess1

IF TYPE("r_.cTm2")="C" AND ok
   REPLACE cTm2 WITH TTOC(DATETIME()) IN r_
ENDIF 

IF lnCount > 0
   INSERT INTO logs_global (tm, path, task, msg);
   		       	VALUES (TIME(),  db_.path, "gtINV", "Получени "+TRANSFORM(lnCount)+" фактури")

   REPLACE nDuration WITH SECONDS()-lnStart IN logs_Global
   _screen.forms[1].lstStatus.Requery() 
ENDIF 

