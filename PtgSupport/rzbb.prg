clea
loXML = CreateObject('Chilkat.Xml')
lnSuccess = loXML.LoadXmlFile("d:\temp\rzbb.xml")

CREATE curs _bnk (IBAN1 C(22), IBAN2 C(22), KName c(30), dbt N(14,2), kdt N(14,2), DT DATE, rem1 C(30), rem2 C(30))

cFN= "d:\temp\_bnk\RZBB-UNISOFT.XML"

lnSuccess = loXML.LoadXmlFile(cFN)
IF (lnSuccess <> 1) THEN
    wait loXML.LastErrorText WINDOW AT 0,0
    return
ENDIF
clea
loXML = CREATEOBJECT('MSXML2.DomDocument')  
loXML.ASYNC = .f.
loXML.LOAD(cFN)
*loRoot = loXML.documentElement
*!*	*lo=loXML.selectNodes("//BkToCstmrStmt/")
lo=loXML.selectNodes("//BkToCstmrStmt/Stmt/Ntry")
*IF TYPE("lo.length")#"N"
FOR lnJ=0 TO lo.length - 1
  lo_=lo.item[lnJ]
  lcValue = lo_.getElementsByTagName("AmtDtls/InstdAmt/Amt").item[0].text
  lcIBAN1 = lo_.getElementsByTagName("RltdPties/DbtrAcct/Id/IBAN").item[0].text
  lcIBAN2 = lo_.getElementsByTagName("RltdPties/CdtrAcct/Id/IBAN").item[0].text
  lcNm1 = lo_.getElementsByTagName("RltdPties/Dbtr/Nm").item[0].text
  lcNm2 = lo_.getElementsByTagName("RltdPties/Cdtr/Nm").item[0].text
ENDFOR 
