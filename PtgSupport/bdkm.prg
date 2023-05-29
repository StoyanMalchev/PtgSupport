PROCEDURE loadBDKM
IF EMPTY(support.bnkFolder) ;
     OR NOT (DIRECTORY(support.bnkFolder)AND TYPE("_vfp.forms[1].path")="C")
   RETURN
ENDIF

lcOnErr = ON("error")
ok=.t.
ON ERROR ok=.f.

IF TYPE("m.loIBAN")#"O"
   PUBLIC loIBAN
   loIBAN=CREATEOBJECT("IBAN")
ENDIF 

loXML = CreateObject('Chilkat.Xml')

SELECT 0
lcFN = ADDBS(_vfp.forms[1].path)+"_bdkm.dat"
IF NOT FILE(lcFN)
   CREATE table (lcFN) FREE ;
     (DT DATE, IBAN1 C(22), IBAN2 C(22), KName c(30),;
      dbt N(14,2), kdt N(14,2),rem1 c(120),rem2 c(120))
ENDIF 
USE (lcFN)
IF LEN(rem1) < 110
   USE
   ERASE (lcFN )
   RETURN
ENDIF    

lcPath=ADDBS(support.bnkFolder)
IF NOT DIRECTORY(lcPath+"bak_")
   MD (lcPath+"bak_")     
ENDIF 

lnFiles = ADIR(la, lcPath+"*.iban") 
FOR lnJ=1 TO lnFiles
  lcIBAN_ = FILETOSTR(lcPath+la[lnJ,1])
*  lcIBAN_ = SUBSTR(lcIBAN_,1,22)  
  lcIBAN_ = right(lcIBAN_,22)  
  IF LEN(lcIBAN_) # 22
     LOOP
  ENDIF 
  
  lnF = ADIR(la_, lcPath+JUSTSTEM(la[lnJ,1])+"*.xml")
  FOR lnJ_ = 1 TO lnF
     lcFN_ = lcPath+la_[lnJ_,1]
*!*	     IF UPPER(JUSTEXT(lcFN_))= "IBAN"
*!*	        LOOP
*!*	     ENDIF 
     lcFN  = lcPath+lcIBAN_+".xml"
     COPY FILE (lcFN_) TO (lcFN)     
     COPY FILE (lcFN_) TO (lcPath+"bak_")
     ERASE  (lcFN_)  
  ENDFOR 
ENDFOR 

*!*	lnFiles = ADIR(la, lcPath+"BG*.xml") && Специални имена за ДСК, Алианц, Райфайзен:
*!*	FOR lnJ=1 TO lnFiles
*!*	  lcIBAN_= SUBSTR(la[lnJ,1],1,22)
*!*	  IF LEN(lcIBAN_)=22
*!*	     lcFN=SUBSTR(lcIBAN_,5,4)+lcIBAN_+".xml"
*!*	     COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+lcFN)
*!*	     COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+strtran(la[lnJ,1],".", "._"))
*!*	     ERASE  (lcPath+la[lnJ,1])  
*!*	  ENDIF 
*!*	ENDFOR 

*lnFiles = ADIR(la, lcPath+"ubbs*.xml")		&& ОББ:
=Alines(laBNK_, "UBBS;BUIN;DEMI;BUIB;UNCR;BPBI;FINV;STSA;RZBB;IABG",";")
**          ОББ,Алианц,ДБанк;СИБанк;UNCR;Пощенска;ПИБ;ДСК;Райфайзен;ИАБанк
lnFiles = ADIR(la, lcPath+"*.xml")		
FOR lnJ=1 TO lnFiles
  lcFN = la[lnJ,1]
  lcBIC = SUBSTR(lcFN,1,4)
  lcIBAN_ = RIGHT(JUSTSTEM(lcFN), 22)
  IF LEN(lcIBAN_)=22 AND loIBAN.valid(lcIBAN_)
     lcBIC = SUBSTR(lcIBAN_,5,4)
  ELSE
	  lcIBAN_ = left(JUSTSTEM(lcFN), 22)
	  IF LEN(lcIBAN_)=22 AND loIBAN.valid(lcIBAN_)
	     lcBIC = SUBSTR(lcIBAN_,5,4)
      ELSE 		
         lcIBAN_=""
      ENDIF 
  ENDIF 

*!*	  DO case
*!*	  CASE ASCAN(laBNK_, lcBIC ) > 0  
*!*	     lcID=SUBSTR(lcFN,1,4)
*!*	  CASE loIBAN.valid(lcFN_)
*!*	     lcID=SUBSTR(lcFN_,5,4)
*!*	  OTHERWISE 
*!*	     LOOP
*!*	  ENDCASE 
  
  IF ASCAN(laBNK_, lcBIC ) > 0  
       IF EMPTY(lcIBAN_)
          DO bnk&lcBIC WITH  lcPath + lcFN
       ELSE
          DO bnk&lcBIC WITH  lcPath + lcFN, lcIBAN_       
       ENDIF 
  ENDIF 
  
  COPY FILE (lcPath+lcFN) TO (lcPath+"bak_")
*  ERASE  (lcPath+la[lnJ,1])  
  ERASE (lcPath+lcFN)
ENDFOR 

* lcPath=ADDBS(support.bnkFolder)
** Alianz:
*lnFiles = ADIR(la, lcPath+"BUIN*.xml")		
*!*	lnFiles = ADIR(la, lcPath+"*.xml")		
*!*	FOR lnJ=1 TO lnFiles
*!*	  IF SUBSTR(la[lnJ,1],5,4) # "BUIN"
*!*	     LOOP
*!*	  ENDIF 
*!*	  DO bnkBUIN WITH  lcPath+la[lnJ,1]
*!*	  COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+strtran(la[lnJ,1],".", "._"))
*!*	  ERASE  (lcPath+la[lnJ,1])  
*!*	ENDFOR 

*!*	lnFiles = ADIR(la, lcPath+"BUIB*.xml")		&& СИ БАНК:
*!*	FOR lnJ=1 TO lnFiles
*!*	  DO bnkSIBNK WITH  lcPath+la[lnJ,1]
*!*	  COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+strtran(la[lnJ,1],".", "._"))
*!*	  ERASE  (lcPath+la[lnJ,1])  
*!*	ENDFOR 

*!*	lnFiles = ADIR(la, lcPath+"uncr*.xml")		&&  Уникредит
*!*	FOR lnJ=1 TO lnFiles
*!*	  DO bnkUNCR WITH  lcPath+la[lnJ,1]
*!*	  COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+strtran(la[lnJ,1],".", "._"))
*!*	  ERASE  (lcPath+la[lnJ,1])  
*!*	ENDFOR 

*!*	lnFiles = ADIR(la, lcPath+"BPBI*.xml")		&& Пощенска:
*!*	FOR lnJ=1 TO lnFiles
*!*	  DO bnkBPBI WITH  lcPath+la[lnJ,1]
*!*	  COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+strtran(la[lnJ,1],".", "._"))
*!*	  ERASE  (lcPath+la[lnJ,1])  
*!*	ENDFOR 

*!*	*lnFiles = ADIR(la, lcPath+"FINV*.xml")		&& ПИБ:
*!*	lnFiles = ADIR(la, lcPath+"*.xml")		
*!*	FOR lnJ=1 TO lnFiles
*!*	  IF SUBSTR(la[lnJ,1],5,4) # "FINV"	&& ПИБ:
*!*	     LOOP
*!*	  ENDIF 

*!*	  DO bnkFINV WITH  lcPath+la[lnJ,1]
*!*	  COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+strtran(la[lnJ,1],".", "._"))
*!*	  ERASE  (lcPath+la[lnJ,1])  
*!*	ENDFOR 

*!*	lnFiles = ADIR(la, lcPath+"*.xml")		&& ДСК
*!*	FOR lnJ=1 TO lnFiles
*!*	  IF SUBSTR(la[lnJ,1],5,4) # "STSA"
*!*	     LOOP
*!*	  ENDIF 

*!*	  DO bnkSTSA WITH  lcPath+la[lnJ,1], SUBSTR(la[lnJ,1],1,22)
*!*	  COPY FILE (lcPath+la[lnJ,1]) TO (lcPath+JUSTSTEM(la[lnJ,1])+"._xml")
*!*	  ERASE  (lcPath+la[lnJ,1])  
*!*	ENDFOR   

*lnFiles = ADIR(la, lcPath+"BUIN*.xml")		&& Alianc
*!*	lnFiles = ADIR(la, lcPath+"*.xml")		&& Alianc
*!*	FOR lnJ=1 TO lnFiles
*!*	  IF SUBSTR(la[lnJ,1],5,4) # "BUIN"
*!*	     LOOP
*!*	  ENDIF 
*!*	  DO bnkBUIN WITH  lcPath+la[1,1], SUBSTR(la[1,1],1,22)
*!*	  COPY FILE (lcPath+la[1,1]) TO (lcPath+SUBSTR(la[1,1],1,26)+"._xml")
*!*	  ERASE  (lcPath+la[1,1])  
*!*	ENDFOR   

*lnFiles = ADIR(la, lcPath+"RZBB*.xml")		&& RAIFAIZEN
*!*	lnFiles = ADIR(la, lcPath+"*.xml")		
*!*	FOR lnJ=1 TO lnFiles
*!*	  IF SUBSTR(la[lnJ,1],5,4) # "RZBB"
*!*	     LOOP
*!*	  ENDIF 
*!*	  DO bnkRZBB WITH  lcPath+la[1,1], SUBSTR(la[1,1],1,22)
*!*	  lcFN_ = lcPath+STRTRAN(lower(la[1,1]),".xml", "._xml")
*!*	  COPY FILE (lcPath+la[1,1]) TO (lcFN_)
*!*	  ERASE  (lcPath+la[1,1])  
*!*	ENDFOR   

*!*	lnFiles = ADIR(la, lcPath+"IABG*.xml")	&& ИНТЕРНЕШЕНЪЛ АСЕТ БАНК
*!*	FOR lnJ=1 TO lnFiles
*!*	  DO bnkIABG WITH  lcPath+la[1,1]
*!*	  lcFN_ = lcPath+STRTRAN(lower(la[1,1]),".xml", "._xml")
*!*	  COPY FILE (lcPath+la[1,1]) TO (lcFN_)
*!*	  ERASE  (lcPath+la[1,1])  
*!*	ENDFOR   

USE IN _bdkm
RELEASE loXML

ON ERROR &lcOnErr

IF TYPE("_vfp.ActiveForm.tmr")="O"
   _VFP.ActiveForm.tmr.ENABLED=.t.
ENDIF    

****** край на loadBDKM

*******************
PROCEDURE bdkmImport( cFN, cID)
IF NOT (DIRECTORY(support.bnkFolder) AND TYPE("_vfp.forms[1].path")="C")
   _screen.Visible = .t.
   MESSAGEBOX(support.bnkFolder, 32, "Липсва папка:")
   _screen.Visible = .f.
   RETURN
ENDIF

lcOnErr = ON("error")
ok=.t.
ON ERROR ok=.f.

SELECT iban from bski_ ;
  WHERE UPPER(SUBSTR(iban,5,4))=cID AND ADDBS(path)==ADDBS(db__.path) ;
  INTO CURSOR bski__
IF RECCOUNT("bski__") > 1
   LOCATE FOR iban = ALLTRIM(JUSTSTEM(cFN))
   IF EOF()
      RETURN 
   ENDIF 
*!*	   _screen.Visible=.t.
*!*	   BROWSE TITLE "Изберете сметка:"
*!*	   _screen.Visible=.f.
ENDIF 

loXML = CreateObject('Chilkat.Xml')

DO case
CASE cID== "UBBS"		&& ОББ
   DO bnkUBBS WITH cFN
CASE cID== "BUIN"		&& Alianz:
  DO bnkBUIN WITH  cFN, bski__.iban
CASE cID== "STSA"		&& ДСК:
  DO bnkSTSA WITH  cFN, bski__.iban    
CASE cID== "BUIB"		&& СИ БАНК:
  DO bnkBUIB WITH  cFN
CASE cID== "IABG"		&& ИНТЕРНЕШЕНЪЛ АСЕТ БАНК
  DO bnkIABG WITH  cFN
CASE cID== "RZBB"		&& RAIFAIZEN
  DO bnkRZBB WITH  cFN, bski__.iban
CASE cID== "UNCR"		&&  Уникредит
  DO bnkUNCR WITH  cFN
CASE cID== "BPBI"		&& Пощенска:
  DO bnkBPBI WITH  cFN
CASE cID== "FINV"		&& ПИБ:
  DO bnkFINV WITH  cFN, bski__.iban
CASE cID== "DEMI"		&& ПИБ:
  DO bnkDEMI WITH  cFN, bski__.iban
CASE cID== "PRCB"		&& ПРОКРЕДИТ
   DO bnkPRCB WITH cFN, bski__.iban
ENDCASE 

RELEASE loXML
ON ERROR &lcOnErr

****************
PROCEDURE bnkUBBS (cFN)
SELECT 0
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), ;
   dbt N(14,2), kdt N(14,2), DT DATE, ;
   rem1 c(120), rem2 c(120))
lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    MESSAGEBOX(loXML.LastErrorText,64,"Проблем:")
    retu
ENDIF

ok=.t.
lcIBAN1 = ALLTRIM(loXML.GetChildContent('IBAN_S'))
lnCountTransactions = loXML.NumChildrenHavingTag("TRANSACTION")
FOR i = 0 TO lnCountTransactions - 1
    loTrnsct = loXML.GetNthChildWithTag("TRANSACTION", i)
	
	lcIBAN2 = ALLTRIM(loTrnsct.GetChildContent("IBAN_R"))
	lcTr_name = ALLTRIM(loTrnsct.GetChildContent("TR_NAME"))		

	INSERT INTO _bnk(IBAN1, IBAN2)	VALUES (lcIBAN1, lcIBAN2)
	lcDbt = STRTRAN( loTrnsct.GetChildContent("AMOUNT_D"), ",", "")
	lcKdt = STRTRAN( loTrnsct.GetChildContent("AMOUNT_C"), ",", "")
	
	REPLACE dt with CTOD(loTrnsct.GetChildContent("POST_DATE")),;
	      KName WITH loTrnsct.GetChildContent("NAME_R"),;
	      dbt  WITH VAL( lcDbt), kdt  WITH VAL(lcKdt),;
	      rem1 WITH loTrnsct.GetChildContent("REM_I"),;
	      rem2 WITH loTrnsct.GetChildContent("REM_II")
NEXT

*!*	SELECT * from _bnk WHERE NOT (EMPTY(iban1)or EMPTY(dt)or dbt+kdt=0.00) INTO CURSOR _bnk
*!*	CALCULATE MIN(dt), MAX(dt) to gDt1, gDt2 

IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF    

**********
PROCEDURE bnkBUIN (cFN, cIBAN1)
SELECT 0
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), DT DATE, rem1 c(120), rem2 c(120))
lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    MESSAGEBOX(loXML.LastErrorText,64,"Проблем:")
    retu
ENDIF

IF PCOUNT()=2
   lcIBAN1 = cIBAN1
ELSE 
   lcIBAN1 = UPPER(SUBSTR(la[1,1],5,22))
ENDIF    

*MESSAGEBOX(TRANSFORM(PCOUNT())+"  "+lcIBAN1, 64,"IBAN:")

lnCountTransactions = loXML.NumChildrenHavingTag("TRANSACTION")

loTransactions = loXML.GetChildWithTag('STATEMENTS')
lnCountTransactions = loXML.NumChildrenHavingTag('STATEMENT')
FOR j = 0 TO lnCountTransactions - 1
	loTrnsct = loXML.GetNthChildWithTag('STATEMENT', j)
	
	*!* ТОВА МОЖЕ ДА ГО НЯМА ИЛИ ДА Е ТЕКСТ
	lcIBAN2 = loTrnsct.GetAttrValue('rem_iii')
	
	*!* 24/06/2009
	ldDT = CTOT(loTrnsct.GetAttrValue('valuedate'))
		
	lnAmount = VAL(STRTRAN(loTrnsct.GetAttrValue('amount'), ',', ''))
	
	*!*		lnAmount = VAL(loDoc.GetChildContent('Amount'))

	lcKName = ALLTRIM(loTrnsct.GetAttrValue('contragent'))
	lcREM1 = ALLTRIM(loTrnsct.GetAttrValue('rem_i'))
	lcREM2 = ALLTRIM(loTrnsct.GetAttrValue('rem_ii'))
	
	*!*		D - дебит ; K - кредит
	lcType = loTrnsct.GetAttrValue('dtkt')
	
	INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
	VALUES (lcIBAN1, lcIBAN2, lcKName, ;
	  IIF(lcType = 'D', lnAmount, 0), ;
	  IIF(lcType = 'K', lnAmount, 0), ldDT, lcREM1, lcREM2)
NEXT

DO bnkImport

**
PROCEDURE bnkUNCR(cFN)
PRIVATE lnJ
SELECT 0
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
  DT DATE, rem1 c(120), rem2 c(120))

loXML = CREATEOBJECT( "Microsoft.XMLDOM" ) 
loXML.ASYNC = .f.
loXML.LOAD(cFN)
loXML.setProperty("SelectionLanguage", "XPath")

*lcNode = [//Items/AccountMovement/Account]
lo = loXML.selectNodes("//Items/AccountMovement")
FOR lnJ=0 TO lo.length - 1
  lo_ = lo.item[lnJ]
  ldDt   = TTOD(CTOT(lo_.getElementsByTagName("PaymentDateTime").item[0].nodeTypedValue))
  lcIBAN1= lo_.getElementsByTagName("IBAN").item[0].nodeTypedValue
  lcIBAN2= lo_.getElementsByTagName("OppositeSideAccount").item[0].nodeTypedValue
*!*	  lcNM   = STRCONV(lo_.getElementsByTagName("OppositeSideName").item[0].nodeTypedValue,11)
*!*	  lcTyp  = STRCONV(lo_.getElementsByTagName("Tooltip").item[0].nodeTypedValue, 11)
*!*	  lcRem1 = STRCONV(lo_.getElementsByTagName("NarrativeI02").item[0].nodeTypedValue, 11)
*!*	  lcRem2 = STRCONV(lo_.getElementsByTagName("ReasonI02").item[0].nodeTypedValue, 11)
  lcNM   = lo_.getElementsByTagName("OppositeSideName").item[0].nodeTypedValue
  lcTyp  = lo_.getElementsByTagName("Tooltip").item[0].nodeTypedValue
  lcRem1 = lo_.getElementsByTagName("NarrativeI02").item[0].nodeTypedValue
  lcRem2 = lo_.getElementsByTagName("ReasonI02").item[0].nodeTypedValue
  lnValue= VAL(lo_.getElementsByTagName("Amount").item[0].nodeTypedValue)

  IF EMPTY(lcIBAN1)or EMPTY(lcIBAN2)or EMPTY(ldDt)
     LOOP
  ENDIF 

  INSERT INTO _bnk (IBAN1, IBAN2, KName, DT, rem1, rem2)	;
 	  VALUES (lcIBAN1, lcIBAN2, lcNM,  ldDT, lcREM1, lcREM2) 		
  
  STORE 0 TO lnDbt, lnKdt  
  IF LOWER(lcTyp)="входяща"
     REPLACE  kdt WITH lnValue IN _bnk
  ELSE
     REPLACE  dbt WITH lnValue IN _bnk
  ENDIF   
NEXT 


*!*	lnSuccess = loXML.LoadXmlFile(cFN)
*!*	IF (lnSuccess <> 1) THEN
*!*	    wait loXML.LastErrorText WINDOW AT 0,0
*!*	    retu
*!*	ENDIF

*!*	lnSuccess = loXML.LoadXmlFile(cFN)
*!*	IF (lnSuccess <> 1) THEN
*!*	    wait loXML.LastErrorText WINDOW AT 0,0
*!*	    return
*!*	ENDIF

*!*	SET DATE ymd
*!*	loAccounts = loXML.GetChildWithTag('ArrayOfAPAccounts')
*!*	lnCountAccounts = loAccounts.NumChildrenHavingTag('APAccount')
*!*	IF lnCountAccounts < 1
*!*	   
*!*	   DO bnkError WITH cFN, "Некоректен формат !" 
*!*	   retu
*!*	ENDIF 

*!*	FOR i = 0 TO lnCountAccounts - 1
*!*		loAcc = loAccounts.GetNthChildWithTag("APAccount", i).GetChildWithTag('BankAccount')
*!*		lcIBAN1 = loAcc.GetChildContent('IBAN')
*!*		loTransactions = loAcc.GetChildWithTag('BankAccountMovements').GetChildWithTag('ArrayOfBankAccountMovements')
*!*		lnCountTransactions = loTransactions.NumChildrenHavingTag('BankAccountMovement')
*!*		FOR j = 0 TO lnCountTransactions - 1
*!*			loTrnsct = loTransactions.GetNthChildWithTag('BankAccountMovement', j)
*!*			
*!*	*		ldDT = TTOD(CTOT(loTrnsct.GetChildContent('ValDate')))
*!*			ldDT = TTOD(CTOT(loTrnsct.GetChildContent('PaymentDate')))

*!*			loDoc = loTrnsct.GetChildWithTag('MovementDocument')
*!*			IF NOT ISNULL(loDoc)		
*!*				lnAmount = VAL(loDoc.GetChildContent('Amount'))
*!*				lcIBAN2 = ALLTRIM(loDoc.GetChildContent('PayeeIBAN'))
*!*				lcKName = ALLTRIM(loDoc.GetChildContent('PayeeName'))			
*!*				IF lcIBAN2=lcIBAN1
*!*					lcIBAN2 = ALLTRIM(loDoc.GetChildContent('PayerIBAN'))
*!*					lcKName = ALLTRIM(loDoc.GetChildContent('PayerName'))
*!*				ENDIF 			
*!*							
*!*				lcREM1 = ALLTRIM(loDoc.GetChildContent('Description1'))
*!*				lcREM2 = ALLTRIM(loDoc.GetChildContent('Description2'))
*!*				
*!*				*!*		1 - дебит ; 2 - кредит
*!*				lnType = VAL(loTrnsct.GetChildContent('MovementType'))
*!*				
*!*				INSERT INTO _bnk (IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
*!*		 		  VALUES (lcIBAN1, lcIBAN2, lcKName, IIF(lnType=1, lnAmount, 0), IIF(lnType=2, lnAmount, 0), ldDT, lcREM1, lcREM2) 		
*!*			ENDIF 
*!*		NEXT
*!*	NEXT		

*!*	SET DATE dmy
		
IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF    
		
**
PROCEDURE bnkBPBI (cFN)		&&       Б П Б
SELECT 0
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
   DT DATE, rem1 c(120), rem2 c(120))
lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    wait loXML.LastErrorText WINDOW AT 0,0
    retu
ENDIF

ok = .t.
loAccounts = loXML.GetChildWithTag('ArrayOfAPAccounts')
lnCountAccounts = loAccounts.NumChildrenHavingTag('APAccount')
FOR i = 0 TO lnCountAccounts - 1
	loAcc = loAccounts.GetNthChildWithTag("APAccount", i).GetChildWithTag('BankAccount')
	lcIBAN1 = loAcc.GetChildContent('IBAN')
	loTransactions = loAcc.GetChildWithTag('Movements').GetChildWithTag('ArrayOfMovements')
	lnCountTransactions = loTransactions.NumChildrenHavingTag('BankAccountMovement')
	FOR j = 0 TO lnCountTransactions - 1
		loTrnsct = loTransactions.GetNthChildWithTag('BankAccountMovement', j)
		
		ldDT = TTOD(CTOT(loTrnsct.GetChildContent('ValDate')))
		
		lnAmount = VAL(loTrnsct.GetChildContent('MovementAmount'))		
		loDoc = loTrnsct.GetChildWithTag('MovementDocument')
		lcIBAN2 = ALLTRIM(loDoc.GetChildContent('PayeeAccountNumber'))
		lcKName = ALLTRIM(loDoc.GetChildContent('PayeeName'))
		lcREM1 = ALLTRIM(loDoc.GetChildContent('Description'))

		*!*		1 - дебит ; 2 - кредит
		lnType = VAL(loTrnsct.GetChildContent('MovementType'))
		
		INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
		VALUES (lcIBAN1, lcIBAN2, lcKName, IIF(lnType = 1, lnAmount, 0), IIF(lnType = 2, lnAmount, 0), ldDT, lcREM1, "")
	NEXT
NEXT		

* _screen.visible=.t.
* SELECT * from _bnk	
* _screen.visible=.f.	
IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF    

**
PROCEDURE bnkFINV(cFN, cIBAN)
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
   DT DATE, rem1 c(120), rem2 c(120))
lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    wait loXML.LastErrorText WINDOW AT 0,0
    return
ENDIF

IF PCOUNT()=2
	lcIBAN1 = m.cIBAN
ELSE 
	RETURN 
*	lcIBAN1 = FORCEEXT(JUSTFNAME(cFN),"")
ENDIF 
lo = loXML.GetChildWithTag('movement')
lnCount = lo.NumChildrenHavingTag('transaction')
FOR j = 0 TO lnCount - 1
  lo_ = lo.GetNthChildWithTag('transaction', j)
   ldDT = CTOD(lo_.FindChild('valuedate').content)
   lnDBT= VAL(lo_.FindChild('debit_tov').content)
   lnKDT= VAL(lo_.FindChild('credit_tov').content)
  
  lo__ = lo_.FindChild('operreason')
  lcIBAN1_ = lo__.FindChild('arranger_account').content
  lcIBAN_ = lo__.FindChild('recipient_account').Content
  
  IF lcIBAN1_==lcIBAN1 
     lcIBAN2=lcIBAN_
  ELSE
	  IF lcIBAN_ == lcIBAN1 
	     lcIBAN2 = lcIBAN_
	  ELSE
	     lcIBAN2 =""	  
	  ENDIF   
  ENDIF 
  
  lcKNAME = lo__.FindChild('recipient_name').Content
  lcREM1 = lo__.FindChild('description').Content
  lcREM2 = lo__.FindChild('description').Content  
  IF NOT EMPTY(lcIBAN2)
 	 INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
		VALUES (lcIBAN1, lcIBAN2, lcKName, lnDBT, lnKDT, ldDT, lcREM1, lcREM2)
  ENDIF 		
NEXT				
		
IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF    

*****************
PROCEDURE bnkSTSA(cFN, cIBAN1)	&& ДСК:
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
   DT DATE, rem1 c(120), rem2 c(120))
lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    wait loXML.LastErrorText WINDOW AT 0,0
    return
ENDIF

*lcIBAN1 = UPPER(SUBSTR(la[1,1],5,22))
lcIBAN1 = cIBAN1

loAccMvmnts = loXML.GetChildWithTag('AccountMovements')

lnCountTransactions = loAccMvmnts.NumChildrenHavingTag('AccountMovement')
FOR j = 0 TO lnCountTransactions - 1
	loTrnsct = loAccMvmnts.GetNthChildWithTag('AccountMovement', j)
		
	ldDT = TTOD(CTOT(loTrnsct.GetChildContent('ValueDate')))
	lcReason = ALLTRIM(loTrnsct.GetChildContent('Reason'))
	lcKName = ALLTRIM(loTrnsct.GetChildContent('OppositeSideName'))
		
	lcIBAN2 = ALLTRIM(loTrnsct.GetChildContent('OppositeSideAccount'))
		
	*!*		Debit - дебит ; Debit - кредит
	lcType = ALLTRIM(loTrnsct.GetChildContent('MovementType'))
				
    lcAmount = STRTRAN(loTrnsct.GetChildContent('Amount'), ",", ".")
	lnAmount = VAL(lcAmount)
		
	INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
	VALUES (lcIBAN1, lcIBAN2, lcKName, IIF(lcType = 'Debit', lnAmount, 0), IIF(lcType = 'Credit', lnAmount, 0), ldDT, lcReason, '')
NEXT

DO bnkImport

**
PROCEDURE bnkBUIB(cFN)	&& СИ БАНК
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
   DT DATE, rem1 c(120), rem2 c(120))
lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    wait loXML.LastErrorText WINDOW AT 0,0
    return
ENDIF

lcIBAN1 = loXML.GetAttrValue('iban')
lnCountTransactions = loXML.NumChildrenHavingTag('STATEMENT')
FOR j = 0 TO lnCountTransactions - 1
	loTrnsct = loXML.GetNthChildWithTag('STATEMENT', j)	
	lcIBAN2 = loTrnsct.GetAttrValue('rem_iii')
	
	ldDT = CTOT(loTrnsct.GetAttrValue('valuedate'))	
	lnAmount = VAL(loTrnsct.GetAttrValue('amount'))	
	lcKName = ALLTRIM(loTrnsct.GetAttrValue('contragent'))
	lcREM1 = ALLTRIM(loTrnsct.GetAttrValue('rem_i'))
	lcREM2 = ALLTRIM(loTrnsct.GetAttrValue('rem_ii'))
	
	*!*		D - дебит ; C - кредит
	lcType = loTrnsct.GetAttrValue('dtkt')
	
	INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
	VALUES (lcIBAN1, lcIBAN2, lcKName, IIF(lcType = 'D', lnAmount, 0), IIF(lcType = 'C', lnAmount, 0), ldDT, lcREM1, lcREM2)
NEXT			

RELEASE loTrnsct	

IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF    

******************
PROCEDURE bnkRZBB(cFN, cIBAN1)	&& РАЙФАЙЗЕН БАНК
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
     DT DATE, rem1 c(120), rem2 c(120))
*!*	lnSuccess = loXML.LoadXmlFile(cFN)
*!*	IF (lnSuccess <> 1) THEN
*!*	    wait loXML.LastErrorText WINDOW AT 0,0
*!*	    return
*!*	ENDIF

PRIVATE loXML
clea
loXML = CREATEOBJECT('MSXML2.DomDocument')  
loXML.ASYNC = .f.
loXML.LOAD(cFN) 

loNodes = loXml.selectnodes("//d3p1:AccountMovement")
lnCount = loNodes.length
FOR i = 0 TO lnCount - 1
	loItem = loNodes.item[i]
    lo = loItem.getElementsByTagName("d3p1:OppositeSideAccount").item[0]
	lcIBAN_ = PADR(lo.nodeTypedValue, 40, " ")

    lo = loItem.selectSingleNode("d3p1:Account")
    lo_ = lo.selectSingleNode("d3p1:IBAN")
	lcIBAN1_ = alltrim(lo_.nodeTypedValue)
	IF PCOUNT()=2
	   IF NOT cIBAN1 == lcIBAN1
	      LOOP
	   ENDIF 
	ENDIF 

	lo=loItem.getElementsByTagName("d3p1:OppositeSideName")
	lcKName = PADR(lo.item[0].nodeTypedValue, 40, " ")

   lcREM1 = loNodes.item[i].getElementsByTagName("d3p1:Narrative").item[0].nodeTypedValue
   lcREM2 = loNodes.item[i].getElementsByTagName("d3p1:Reason").item[0].nodeTypedValue

*  lnValue = VAL(loXml.selectnodes("//d3p1:Amount").item[i].text  )
  lnValue = VAL(loNodes.item[i].getElementsByTagName("d3p1:Amount").item[0].nodeTypedValue)

  lcDt = loNodes.item[i].getElementsByTagName("d3p1:PaymentDate").item[0].nodeTypedValue 
  _dt = TTOD(CTOT(lcDt))
*  INSERT INTO _bnk(IBAN1, IBAN2, KName)	VALUES (cIBAN1, lcIBAN_, lcKName)
  INSERT INTO _bnk(IBAN1, IBAN2, KName)	VALUES (lcIBAN1_, lcIBAN_, lcKName)
  
  lcType = loXml.selectnodes("//d3p1:MovementType").item[i].text  
  IF lcType="Debit"
     REPLACE dbt WITH lnValue
*!*	*     lcKName = PADR(loXml.selectnodes("//d3p1:Account/d3p1:BankClientName").item[i].text, 40, " ")
*!*	     lcNd_ = "//d3p1:MovementDocument/d3p1:PayeeName"
*!*	     lcKName = PADR(loXml.selectnodes(lcND_).item[i].text, 40, " ")
  ELSE
     REPLACE kdt WITH lnValue
*!*	     lcKName = PADR(loXml.selectnodes("//d3p1:Account/d3p1:BankClientName").item[i].text, 40, " ")
*!*	     lcIBAN_ = PADR(loXml.selectnodes("//d3p1:MovementDocument/d3p1:PayeeIBAN").item[i].text, 40, " ")
*!*	  
  ENDIF 
  
  REPLACE rem1 WITH lcRem1, rem2 WITH lcRem2, dt WITH _dt
*? lcType, lcDt, lcIBAN_, lcKName, ROUND(lnValue,2), lcRem1
ENDFOR 

*!*	_screen.Visible=.t.
*!*	brow
*!*	COPY TO d:\temp\bnk_

*!*	lo=loXML.selectNodes("//BkToCstmrStmt/Stmt/Ntry")
*!*	FOR lnJ=0 TO lo.length - 1
*!*	  lo_=lo.item[lnJ]
*!*	  lcValue = lo_.getElementsByTagName("AmtDtls/InstdAmt/Amt").item[0].text
*!*	  
*!*	  lcIBAN1 = lo_.getElementsByTagName("RltdPties/DbtrAcct/Id/IBAN").item[0].text
*!*	  lcIBAN2 = lo_.getElementsByTagName("RltdPties/CdtrAcct/Id/IBAN").item[0].text
*!*	  
*!*	  lcRem1 =  lo_.getElementsByTagName("NtryDtls/TxDtls/RmtInf/Ustrd").item[0].text
*!*	  lcRem2 =  lo_.getElementsByTagName("NtryDtls/TxDtls/RtrInf/Rsn/Cd").item[0].text
*!*	   
*!*	  lcKName = ""
*!*	  STORE 0 TO lnDbt, lnKdt
*!*	  if lcIBAN2==cIBAN1
*!*	     IF TYPE('lo_.getElementsByTagName("RltdPties/Dbtr/Nm").item[0].text')="C"
*!*	        lcKName = lo_.getElementsByTagName("RltdPties/Dbtr/Nm").item[0].text
*!*	     ENDIF 
*!*	     lnKdt=VAL(lcValue)
*!*	  ELSE
*!*	     IF TYPE('lo_.getElementsByTagName("RltdPties/Cdtr/Nm").item[0].text')="C"
*!*	        lcKName = lo_.getElementsByTagName("RltdPties/Cdtr/Nm").item[0].text
*!*	     ENDIF 
*!*	     lnDbt=VAL(lcValue)
*!*	  ENDIF 

*!*	*  lcNm1 = lo_.getElementsByTagName("RltdPties/Dbtr/Nm").item[0].text
*!*	*  lcNm2 = lo_.getElementsByTagName("RltdPties/Cdtr/Nm").item[0].text

*!*	  lcDT =  lo_.getElementsByTagName("ValDt/Dt").item[0].text
*!*	  ldDT = date(val(substr(lcDT,1,4)), val(substr(lcDT,6,2)), val(substr(lcDT,9,2)))			

*!*	  if lcIBAN2==cIBAN1
*!*	     lcIBAN2=lcIBAN1
*!*	     lcIBAN1=cIBAN1
*!*	  ENDIF 

*!*	  INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2) ;
*!*		VALUES (lcIBAN1, lcIBAN2, lcKName, lnDbt, lnKdt, ldDT, lcRem1, lcRem2 )
*!*	ENDFOR 

IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF    

**
PROCEDURE bnkIABG(cFN) && ИНТЕРНЕШЕНЪЛ АСЕТ БАНК
SELECT 0
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
   DT DATE, rem1 c(120), rem2 c(120))

lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    WAIT loXML.LastErrorText WINDOW AT 0,0
	RETURN
ENDIF

loResultArray = loXML.GetChildWithTag('ResultArray')
lnCountMoevements = loResultArray.NumChildrenHavingTag('MovementsOnAllAccountsData')
FOR i = 0 TO lnCountMoevements - 1
	mvmnt = loResultArray.GetNthChildWithTag("MovementsOnAllAccountsData", i)
			
	*!*		Дебит ; Кредит 
	lcType = UPPER(mvmnt.GetChildContent('TypeOper'))			

	*!* 	DateAccounted: yyyyMMdd пр. 20160323
	lcDT = mvmnt.GetChildContent('DateAccounted')
	ldDT = date(val(substr(lcDT,1,4)), val(substr(lcDT,5,2)), val(substr(lcDT,7,2)))			
			
	lnAmount = VAL(mvmnt.GetChildContent('Sum'))
	
	IF lcType = 'ДЕБИТ' THEN
		lcIBAN1 = ALLTRIM(mvmnt.GetChildContent('PayerBankAccount'))
		lcIBAN2 = ALLTRIM(mvmnt.GetChildContent('RecieverBankAccount'))
		lcKName = ALLTRIM(mvmnt.GetChildContent('RecieverName'))
	ELSE
		lcIBAN1 = ALLTRIM(mvmnt.GetChildContent('RecieverBankAccount'))
		lcIBAN2 = ALLTRIM(mvmnt.GetChildContent('PayerBankAccount'))
		lcKName = ALLTRIM(mvmnt.GetChildContent('PayerName'))
	ENDIF 
	
	IF EMPTY(lcIBAN1) THEN
		lcIBAN1 = ALLTRIM(mvmnt.GetChildContent('Id'))
	ENDIF
	
	lcREM1 = ALLTRIM(mvmnt.GetChildContent('Details'))
	
	INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
	VALUES (lcIBAN1, lcIBAN2, lcKName, IIF(lcType = 'ДЕБИТ', lnAmount, 0), IIF(lcType = 'КРЕДИТ', lnAmount, 0), ldDT, lcREM1, "")
NEXT

IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF    

****************
PROCEDURE bnkDEMI (cFN)
RETURN 

SELECT 0
CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), ;
   DT DATE, rem1 c(120), rem2 c(120))
lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    MESSAGEBOX(loXML.LastErrorText,64,"Проблем:")
    retu
ENDIF

ok=.t.
lcIBAN1 = ALLTRIM(loXML.GetChildContent('IBAN_S'))
lnCountTransactions = loXML.NumChildrenHavingTag("TRANSACTION")
FOR i = 0 TO lnCountTransactions - 1
    loTrnsct = loXML.GetNthChildWithTag("TRANSACTION", i)
	
    lcTXT = loTrnsct.GetChildContent("REM_I")
    lcTXT = lcTXT + loTrnsct.GetChildContent("REM_II")

	INSERT INTO _bnk(IBAN1, IBAN2)	VALUES (lcIBAN1, lcIBAN2)
	lcDbt = STRTRAN( loTrnsct.GetChildContent("AMOUNT_D"), ",", "")
	lcKdt = STRTRAN( loTrnsct.GetChildContent("AMOUNT_C"), ",", "")
	
	REPLACE dt with CTOD(loTrnsct.GetChildContent("POST_DATE")),;
	      KName WITH loTrnsct.GetChildContent("NAME_R"),;
	      dbt  WITH VAL( lcDbt), kdt  WITH VAL(lcKdt),;
	      rem1 WITH loTrnsct.GetChildContent("REM_I"),;
	      rem2 WITH loTrnsct.GetChildContent("REM_II")
NEXT

*!*	SELECT * from _bnk WHERE NOT (EMPTY(iban1)or EMPTY(dt)or dbt+kdt=0.00) INTO CURSOR _bnk
*!*	CALCULATE MIN(dt), MAX(dt) to gDt1, gDt2 

IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF   


**
PROCEDURE bnkPRCB (cFN, cIBAN1)
SELECT 0
* CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), ;
*  dbt N(14,2), kdt N(14,2), DT DATE, rem1 C(30), rem2 C(30))

ok=.t.
SELECT 0
*lcFN = ADDBS(support.bnkFolder)+cFN
IMPORT FROM (cFN ) xl5
IF NOT ok
   MESSAGEBOX("Некоректен формат на "+cFN,48, "Проблем:")
   retu
ENDIF 

lcAlias=ALIAS()

SELECT CTOD(b)as dt, ROUND(VAL(e),2)as stst, f as d_k, ;
           i as KName, j as IBAN2, k as rem1 ;
  FROM &lcAlias  INTO CURSOR _bnk

USE IN &lcAlias 
 
sele dt, cIBAN1 as IBAN1, IBAN2, KName, ;
     IIF(d_k="Дт", stst, 0000000.00)as dbt,;
     IIF(d_k="Кт", stst, 0000000.00)as kdt,  rem1, "" as rem2 ;
  from _bnk into curs _bnk
  
IF ok
   DO bnkImport
ELSE
   DO bnkError WITH cFN
ENDIF 

*!*	lnSuccess = loXML.LoadXmlFile(cFN)
*!*	IF (lnSuccess <> 1) THEN
*!*	    WAIT loXML.LastErrorText WINDOW AT 0,0
*!*		RETURN
*!*	ENDIF

*!*	loResultArray = loXML.GetChildWithTag('ResultArray')
*!*	lnCountMoevements = loResultArray.NumChildrenHavingTag('MovementsOnAllAccountsData')
*!*	FOR i = 0 TO lnCountMoevements - 1
*!*		mvmnt = loResultArray.GetNthChildWithTag("MovementsOnAllAccountsData", i)
*!*				
*!*		*!*		Дебит ; Кредит 
*!*		lcType = UPPER(mvmnt.GetChildContent('TypeOper'))			

*!*		*!* 	DateAccounted: yyyyMMdd пр. 20160323
*!*		lcDT = mvmnt.GetChildContent('DateAccounted')
*!*		ldDT = date(val(substr(lcDT,1,4)), val(substr(lcDT,5,2)), val(substr(lcDT,7,2)))			
*!*				
*!*		lnAmount = VAL(mvmnt.GetChildContent('Sum'))
*!*		
*!*		IF lcType = 'ДЕБИТ' THEN
*!*			lcIBAN1 = ALLTRIM(mvmnt.GetChildContent('PayerBankAccount'))
*!*			lcIBAN2 = ALLTRIM(mvmnt.GetChildContent('RecieverBankAccount'))
*!*			lcKName = ALLTRIM(mvmnt.GetChildContent('RecieverName'))
*!*		ELSE
*!*			lcIBAN1 = ALLTRIM(mvmnt.GetChildContent('RecieverBankAccount'))
*!*			lcIBAN2 = ALLTRIM(mvmnt.GetChildContent('PayerBankAccount'))
*!*			lcKName = ALLTRIM(mvmnt.GetChildContent('PayerName'))
*!*		ENDIF 
*!*		
*!*		IF EMPTY(lcIBAN1) THEN
*!*			lcIBAN1 = ALLTRIM(mvmnt.GetChildContent('Id'))
*!*		ENDIF
*!*		
*!*		lcREM1 = ALLTRIM(mvmnt.GetChildContent('Details'))
*!*		
*!*		INSERT INTO _bnk(IBAN1, IBAN2, KName, dbt, kdt, DT, rem1, rem2)	;
*!*		VALUES (lcIBAN1, lcIBAN2, lcKName, IIF(lcType = 'ДЕБИТ', lnAmount, 0), IIF(lcType = 'КРЕДИТ', lnAmount, 0), ldDT, lcREM1, "")
*!*	NEXT

*!*	IF ok
*!*	   DO bnkImport
*!*	ELSE
*!*	   DO bnkError WITH cFN
*!*	ENDIF    

*******************
PROCEDURE bnkImport
PRIVATE lcPath
*!*	IF NOT ised("dbBSKI")
*!*	   DO loadBSKI
*!*	ENDDO 

SELECT * from _bnk WHERE NOT (EMPTY(iban1)or EMPTY(dt)or dbt+kdt=0.00) INTO CURSOR _bnk
CALCULATE MIN(dt), MAX(dt) to gDt1, gDt2 
GO top
lcIBAN1=IBAN1

ok=.f.
SELECT BSKI_
GO top
SCAN
*!*	   IF EMPTY(db__.df)
*!*	      LOOP
*!*	   ENDIF 
*!*		lcPath = ADDBS(db__.path)

*!*		IF NOT DIRECTORY(lcPath)
*!*		 	LOOP
*!*		ENDIF 
*!*		
*!*		SELECT 0
*!*	    IF NOT (FILE(lcPath+"_tasks.dat")and FILE(lcPath+"bdkm.dbf"))
*!*		   LOOP
*!*		ENDIF
*!*		

*!*		GO TOP IN _bnk
*!*		SELECT 0
*!*		use (lcPath+"bski.dbf") 
*!*		
*!*		LOCATE FOR  EMPTY(ktrg) AND iban=_bnk.iban1

    IF BSKI_.IBAN # lcIBAN1
       LOOP
    ENDIF 
    lcPath = ADDBS(BSKI_.path)
        
*	IF NOT EOF("bski")
	   ok=.t.
	   SELECT 0
	   USE (lcPath+"bdkm.dbf")
	   DELETE FOR iban1=lcIBAN1 AND BETWEEN(dt, gDt1, gDt2)
	   APPEND FROM DBF("_bnk")
	   use	   
       INSERT INTO logs_global (tm, path, task, msg);
	     VALUES (TIME(),  lcPath, "Банкиране", "IBAN:&lcIBAN1 " +;
	        TRAN(RECC("_bnk"))+" записа от:"+TRAN(gDt1)+" до "+TRAN(gDt2) )	
*	ENDIF 
*	USE IN bski
	
*	IF TYPE("lcPath")="C" AND FILE(lcPath+"PtgAcc.dbc")
	   OPEN DATABASE (lcPath+"PtgAcc.dbc")
	   CLOSE DATABASES
*	ENDIF 
ENDSCAN 
IF NOT ok
   INSERT INTO logs_global (tm, task, path, msg);
     VALUES (TIME(),  "Банкиране", "проблем!", "Няма папка за "+_bnk.iban1)
ENDIF 

IF TYPE("_vfp.ActiveForm.lstStatus")="O"
   _vfp.ActiveForm.lstStatus.Requery() 
ENDIF    

*!*	IF NOT USED("_bdkm.dat")
*!*	   USE ( ADDBS(_vfp.forms[1].path)+"_bdkm.dat" ) again IN 0
*!*	ENDIF    

*!*	SELECT _bdkm
*!*	DELETE FOR iban1=lcIBAN1 AND BETWEEN(dt,gDt1,gDt2)
*!*	APPEND FROM DBF("_bnk")

lcMSG = "Добавени "+TRAN(RECCOUNT("_bnk"))+" записа за &lcIBAN1 " 

USE IN _bnk

logTasks(lcMsg )   
WAIT lcMSG WINDOW AT 0,0 TIMEOUT 3 

RELEASE loTrnsct

******************
PROCEDURE bnkError(cFN, cMsg)
PRIVATE lcMsg
IF PCOUNT() < 2
   lcMsg = "Неуспешен импорт на б. извлечения:"
ELSE
   lcMsg = cMsg
ENDIF          
lcMsg = lcMsg + JUSTSTEM(cFN)

_screen.Visible = .t.
WAIT lcFN WINDOW AT 0,0 TIMEOUT 10
_screen.Visible = .f.

logTasks(lcMsg )   


***************************
DEFINE CLASS IBAN as Custom
   value = SPACE(22)
   Key_ID = SPACE(8) 
      
*  BIC = SPACE(8) 		&& Определя банката, но не и клона
*  Banka = SPACE(30)
*  BKLON = SPACE(20)

**
FUNCTION Banka
PARAMETERS cIBAN
IF TYPE("cIBAN")="C"
   this.value = cIBAN
ENDIF 
IF NOT USED("BANKI")
   USE banki ORDER klon IN 0
ENDIF 
PRIVATE lcKlon
lcKlon = SUBSTR(this.value, 5, 8)
IF banki.Klon_ID # lcKlon
   SEEK lcKlon IN banki
   IF EOF("banki") AND FILE(_Path1+"banki.dat")
      SELECT TOP 1 * from (_Path1+"banki.dat") ;
        WHERE klon_id=lcKlon AND NOT EMPTY(bic) ;
        INTO CURSOR banka_
      SELECT banki
      APPEND FROM DBF("banka_")
      USE IN banka_
	  SEEK lcKlon IN banki      
   ENDIF    
ENDIF 
RETURN banki.Banka

**
FUNCTION IBAN
PARAMETERS cIBAN
IF TYPE("cIBAN")="C"
   this.value = cIBAN
ENDIF 
IF NOT USED("BANKI")
   USE banki ORDER klon IN 0
ENDIF 
PRIVATE lcKlon
lcKlon = SUBSTR(this.value,4,8)
IF banki.Klon_ID # lcKlon
   SEEK lcKlon IN banki
ENDIF 
RETURN banki.IBAN

**
FUNCTION Klon
PARAMETERS cIBAN
IF TYPE("cIBAN")="C"
   this.value = cIBAN
ENDIF 
IF NOT USED("BANKI")
   USE banki ORDER klon IN 0
ENDIF 
PRIVATE lcKlon
lcKlon = SUBSTR(this.value,4,8)
IF banki.Klon_ID # lcKlon
   SEEK lcKlon IN banki
ENDIF 
RETURN banki.Klon

**
Function valid
PARAMETERS cIBAN
PRIVATE tempint, tempstr, divstr, tempstr, i, ch, lcIBAN
IF TYPE("cIBAN")="C"
   this.value = cIBAN
ENDIF 
lcIBAN = this.value
tempstr = Right(lcIBAN, Len(lcIBAN) - 4) + Left(lcIBAN, 4)

lcStr=""
for i=1 to Len(tempstr)
   ch = substr(tempstr, i, 1)
   do case
   CASE BETWEEN(ch,"A","Z")
      lcStr = lcStr + STR(ASC(ch)-65+10,2)
   OTHERWISE 
      lcStr = lcStr + ch   
   EndCase
   next

Do while Len(lcStr) >= 5
   int1 = MOD(val(Left(lcStr,5)),97)
   lcStr = LTRIM(STR(int1,2)) + Right(lcStr, Len(lcStr) - 5) 
enddo
RETURN ( MOD(val(lcStr),97) = 1 )

ENDDEFINE		&& CLASS IBAN

