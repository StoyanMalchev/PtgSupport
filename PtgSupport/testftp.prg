PARAMETERS oCnt

IF TYPE("objFTP") # "O"
   PUBLIC objFTP
#INCLUDE "ftp.h"
   SET PROCEDURE TO ftp.prg ADDITIVE
   objFTP = CREATEOBJECT('ftp_service')
ENDIF 

IF NOT USED("support")
   USE support.dbf IN 0
ENDIF    

lcUser= ALLTRIM(oCnt.usernameTxt.Value)
lcPW  = ALLTRIM(oCnt.passwordTxt.Value)
lcHost= ALLTRIM(oCnt.hostTxt.Value)
lcPort= ALLTRIM(oCnt.portTxt.Value)

ok = .F.
*!*	IF empty(support.ftpHost) THEN
*!*	   ok = objFtp.OpenInternet("danni@pitagorpitagor.com", "Danni", "felix.tophost.bg", "21")
*!*	ELSE
   ok = objFtp.OpenInternet(lcUser, lcPW, lcHost, lcPort )
*!*	ENDIF
IF NOT ok THEN
   lnErr = objFtp.GetErrorCode(.f.)
   MESSAGEBOX(objFtp.GetErrorText(lnErr)+" грешка "+TRANSFORM(lnErr),48,  "FTP Проблем:")
ELSE
*!*		IF EMPTY(support.ftpFolder)
	   lcFldr= "PtgSupport"
*!*		ELSE
*!*		   lcFldr= trim(support.ftpFolder)
*!*		endi
	= objFtp.CreateFtpDirectory(lcFldr)
	STRTOFILE("TestFTP", "tstFTP.txt")
	ok = objFtp.PutFtpFile(lcFldr+"/tstFTP.txt", "tstFTP.txt")
	IF NOT ok THEN
	   lnErr = objFtp.GetErrorCode(.F.)
 	   MESSAGEBOX(objFtp.GetErrorText(lnErr)+" грешка "+TRANSFORM(lnErr),48,"FTP Проблем:")
	ELSE
 	   MESSAGEBOX("Успешн тест за достъп до FTP сървъра",48,"FTP настройки:")
   	ENDIF
   	=objFTP.CloseInternet() 
ENDIF	  
  	