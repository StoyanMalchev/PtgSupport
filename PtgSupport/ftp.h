* !!! Don't remove next two rows !!!
*!*	<pdm_sc_yes/>
*!*	<pdm_dd_yes/>

#DEFINE __FTP_INCLUDE

#IFNDEF __CORE_INCLUDE
   #INCLUDE "core.h"
#ENDIF


* Internet API Returns
*
#define ERROR_SUCCESS							  0
#define ERROR_FILE_NOT_FOUND             		  2
#define ERROR_PATH_NOT_FOUND             		  3
#define ERROR_ACCESS_DENIED              		  5
#define ERROR_INVALID_HANDLE             		  6
#define ERROR_NO_MORE_FILES						  18
#define ERROR_FILE_EXISTS                		  80
#define ERROR_INVALID_PARAMETER          		  87

*** begin of olrrai fix
* Timeout options
#define INTERNET_OPTION_CALLBACK                1
#define INTERNET_OPTION_CONNECT_TIMEOUT         2
#define INTERNET_OPTION_CONNECT_RETRIES         3
#define INTERNET_OPTION_CONNECT_BACKOFF         4
#define INTERNET_OPTION_SEND_TIMEOUT            5
#define INTERNET_OPTION_CONTROL_SEND_TIMEOUT    INTERNET_OPTION_SEND_TIMEOUT
#define INTERNET_OPTION_RECEIVE_TIMEOUT         6
#define INTERNET_OPTION_CONTROL_RECEIVE_TIMEOUT INTERNET_OPTION_RECEIVE_TIMEOUT
#define INTERNET_OPTION_DATA_SEND_TIMEOUT       7
#define INTERNET_OPTION_DATA_RECEIVE_TIMEOUT    8
#define INTERNET_OPTION_HANDLE_TYPE             9
#define INTERNET_OPTION_CONTEXT_VALUE           10
#define INTERNET_OPTION_LISTEN_TIMEOUT          11
#define INTERNET_OPTION_READ_BUFFER_SIZE        12
#define INTERNET_OPTION_WRITE_BUFFER_SIZE       13
#define INTERNET_OPTION_ASYNC_ID                15
#define INTERNET_OPTION_ASYNC_PRIORITY          16
#define INTERNET_OPTION_PARENT_HANDLE           21
#define INTERNET_OPTION_KEEP_CONNECTION         22
#define INTERNET_OPTION_REQUEST_FLAGS           23
#define INTERNET_OPTION_EXTENDED_ERROR          24 && LONG
#define INTERNET_OPTION_OFFLINE_MODE            26
#define INTERNET_OPTION_CACHE_STREAM_HANDLE     27
#define INTERNET_OPTION_USERNAME                28 && STRING
#define INTERNET_OPTION_PASSWORD                29 && STRING
#define INTERNET_OPTION_ASYNC                   30
#define INTERNET_OPTION_SECURITY_FLAGS          31
#define INTERNET_OPTION_SECURITY_CERTIFICATE_STRUCT    32 && STRING/STRUC
#define INTERNET_OPTION_DATAFILE_NAME           33 && String
#define INTERNET_OPTION_URL                     34 && STRING
#define INTERNET_OPTION_SECURITY_CERTIFICATE    35
#define INTERNET_OPTION_SECURITY_KEY_BITNESS    36
#define INTERNET_OPTION_REFRESH                 37
#define INTERNET_OPTION_PROXY                   38 && STRING/STRUC
#define INTERNET_OPTION_SETTINGS_CHANGED        39
#define INTERNET_OPTION_VERSION                 40 && STRING
#define INTERNET_OPTION_USER_AGENT              41 && STRING/STRUC
************** end of olrrai fix
#define INTERNET_OPTION_CONNECTED_STATE            50

#define INTERNET_OPTION_MAXFLAG            50

#define ERROR_INTERNET_BASE 					  12000

#define ERROR_INTERNET_OUT_OF_HANDLES             (ERROR_INTERNET_BASE + 1)
#define ERROR_INTERNET_TIMEOUT                    (ERROR_INTERNET_BASE + 2)
#define ERROR_INTERNET_EXTENDED_ERROR             (ERROR_INTERNET_BASE + 3)
#define ERROR_INTERNET_INTERNAL_ERROR             (ERROR_INTERNET_BASE + 4)
#define ERROR_INTERNET_INVALID_URL                (ERROR_INTERNET_BASE + 5)
#define ERROR_INTERNET_UNRECOGNIZED_SCHEME        (ERROR_INTERNET_BASE + 6)
#define ERROR_INTERNET_NAME_NOT_RESOLVED          (ERROR_INTERNET_BASE + 7)
#define ERROR_INTERNET_PROTOCOL_NOT_FOUND         (ERROR_INTERNET_BASE + 8)
#define ERROR_INTERNET_INVALID_OPTION             (ERROR_INTERNET_BASE + 9)
#define ERROR_INTERNET_BAD_OPTION_LENGTH          (ERROR_INTERNET_BASE + 10)
#define ERROR_INTERNET_OPTION_NOT_SETTABLE        (ERROR_INTERNET_BASE + 11)
#define ERROR_INTERNET_SHUTDOWN                   (ERROR_INTERNET_BASE + 12)
#define ERROR_INTERNET_INCORRECT_USER_NAME        (ERROR_INTERNET_BASE + 13)
#define ERROR_INTERNET_INCORRECT_PASSWORD         (ERROR_INTERNET_BASE + 14)
#define ERROR_INTERNET_LOGIN_FAILURE              (ERROR_INTERNET_BASE + 15)
#define ERROR_INTERNET_INVALID_OPERATION          (ERROR_INTERNET_BASE + 16)
#define ERROR_INTERNET_OPERATION_CANCELLED        (ERROR_INTERNET_BASE + 17)
#define ERROR_INTERNET_INCORRECT_HANDLE_TYPE      (ERROR_INTERNET_BASE + 18)
#define ERROR_INTERNET_INCORRECT_HANDLE_STATE     (ERROR_INTERNET_BASE + 19)
#define ERROR_INTERNET_NOT_PROXY_REQUEST          (ERROR_INTERNET_BASE + 20)
#define ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND   (ERROR_INTERNET_BASE + 21)
#define ERROR_INTERNET_BAD_REGISTRY_PARAMETER     (ERROR_INTERNET_BASE + 22)
#define ERROR_INTERNET_NO_DIRECT_ACCESS           (ERROR_INTERNET_BASE + 23)
#define ERROR_INTERNET_NO_CONTEXT                 (ERROR_INTERNET_BASE + 24)
#define ERROR_INTERNET_NO_CALLBACK                (ERROR_INTERNET_BASE + 25)
#define ERROR_INTERNET_REQUEST_PENDING            (ERROR_INTERNET_BASE + 26)
#define ERROR_INTERNET_INCORRECT_FORMAT           (ERROR_INTERNET_BASE + 27)
#define ERROR_INTERNET_ITEM_NOT_FOUND             (ERROR_INTERNET_BASE + 28)
#define ERROR_INTERNET_CANNOT_CONNECT             (ERROR_INTERNET_BASE + 29)
#define ERROR_INTERNET_CONNECTION_ABORTED         (ERROR_INTERNET_BASE + 30)
#define ERROR_INTERNET_CONNECTION_RESET           (ERROR_INTERNET_BASE + 31)
#define ERROR_INTERNET_FORCE_RETRY                (ERROR_INTERNET_BASE + 32)
#define ERROR_INTERNET_INVALID_PROXY_REQUEST      (ERROR_INTERNET_BASE + 33)
#define ERROR_INTERNET_NEED_UI                    (ERROR_INTERNET_BASE + 34)

#define ERROR_INTERNET_HANDLE_EXISTS              (ERROR_INTERNET_BASE + 36)
#define ERROR_INTERNET_SEC_CERT_DATE_INVALID      (ERROR_INTERNET_BASE + 37)
#define ERROR_INTERNET_SEC_CERT_CN_INVALID        (ERROR_INTERNET_BASE + 38)
#define ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR     (ERROR_INTERNET_BASE + 39)
#define ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR     (ERROR_INTERNET_BASE + 40)
#define ERROR_INTERNET_MIXED_SECURITY             (ERROR_INTERNET_BASE + 41)
#define ERROR_INTERNET_CHG_POST_IS_NON_SECURE     (ERROR_INTERNET_BASE + 42)
#define ERROR_INTERNET_POST_IS_NON_SECURE         (ERROR_INTERNET_BASE + 43)
#define ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED    (ERROR_INTERNET_BASE + 44)
#define ERROR_INTERNET_INVALID_CA                 (ERROR_INTERNET_BASE + 45)
#define ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP      (ERROR_INTERNET_BASE + 46)
#define ERROR_INTERNET_ASYNC_THREAD_FAILED        (ERROR_INTERNET_BASE + 47)
#define ERROR_INTERNET_REDIRECT_SCHEME_CHANGE     (ERROR_INTERNET_BASE + 48)
#define ERROR_INTERNET_DIALOG_PENDING             (ERROR_INTERNET_BASE + 49)
#define ERROR_INTERNET_RETRY_DIALOG               (ERROR_INTERNET_BASE + 50)
#define ERROR_INTERNET_HTTPS_HTTP_SUBMIT_REDIR    (ERROR_INTERNET_BASE + 52)
#define ERROR_INTERNET_INSERT_CDROM               (ERROR_INTERNET_BASE + 53)



#define INTERNET_STATE_CONNECTED     1
#define INTERNET_STATE_DISCONNECTED  2 
#define INTERNET_STATE_DISCONNECTED_BY_USER 0x10 && no network requests being made (by Wininet)
#define INTERNET_STATE_IDLE        0x100          && network requests being made (by  Wininet)
#define INTERNET_STATE_BUSY        0x200


*
* FTP API errors
*

#define FTP_TRANSFER_IN_PROGRESS            	  (ERROR_INTERNET_BASE + 110)
#define FTP_DROPPED                         	  (ERROR_INTERNET_BASE + 111)
#define FTP_NO_PASSIVE_MODE                       (ERROR_INTERNET_BASE + 112)


*
* additional Internet API error codes
*

#define ERROR_INTERNET_SECURITY_CHANNEL_ERROR     (ERROR_INTERNET_BASE + 157)
#define ERROR_INTERNET_UNABLE_TO_CACHE_FILE       (ERROR_INTERNET_BASE + 158)
#define ERROR_INTERNET_TCPIP_NOT_INSTALLED        (ERROR_INTERNET_BASE + 159)
#define ERROR_INTERNET_DISCONNECTED               (ERROR_INTERNET_BASE + 163)
#define ERROR_INTERNET_SERVER_UNREACHABLE         (ERROR_INTERNET_BASE + 164)
#define ERROR_INTERNET_PROXY_SERVER_UNREACHABLE   (ERROR_INTERNET_BASE + 165)

#define ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT      (ERROR_INTERNET_BASE + 166)
#define ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT  (ERROR_INTERNET_BASE + 167)
#define ERROR_INTERNET_SEC_INVALID_CERT    		  (ERROR_INTERNET_BASE + 169)
#define ERROR_INTERNET_SEC_CERT_REVOKED    		  (ERROR_INTERNET_BASE + 170)

*
* handle types
*

#define ERROR_INTERNET_HANDLE_TYPE_INTERNET              1
#define ERROR_INTERNET_HANDLE_TYPE_CONNECT_FTP           2
#define ERROR_INTERNET_HANDLE_TYPE_CONNECT_GOPHER        3
#define ERROR_INTERNET_HANDLE_TYPE_CONNECT_HTTP          4
#define ERROR_INTERNET_HANDLE_TYPE_FTP_FIND              5
#define ERROR_INTERNET_HANDLE_TYPE_FTP_FIND_HTML         6
#define ERROR_INTERNET_HANDLE_TYPE_FTP_FILE              7
#define ERROR_INTERNET_HANDLE_TYPE_FTP_FILE_HTML         8
#define ERROR_INTERNET_HANDLE_TYPE_GOPHER_FIND           9
#define ERROR_INTERNET_HANDLE_TYPE_GOPHER_FIND_HTML      10
#define ERROR_INTERNET_HANDLE_TYPE_GOPHER_FILE           11
#define ERROR_INTERNET_HANDLE_TYPE_GOPHER_FILE_HTML      12
#define ERROR_INTERNET_HANDLE_TYPE_HTTP_REQUEST          13



#define BYTE_1											 1
#define BYTE_2                                           256
#define BYTE_3                                           65536
#define BYTE_4                                           16777216
#define MAXDWORD										 4294967295

*
* File Caching Flags
*

#define INTERNET_FLAG_DONT_CACHE						 67108864
#define INTERNET_FLAG_HYPERLINK							 1024
#define INTERNET_FLAG_MAKE_PERSISTENT					 33554432
#define INTERNET_FLAG_MUST_CACHE_REQUEST			     16
#define INTERNET_FLAG_RELOAD							 2147483648
#define INTERNET_FLAG_NO_CACHE_WRITE				     67108864
#define INTERNET_FLAG_RESYNCHRONIZE						 2048
#define INTERNET_FLAG_NEED_FILE                          0x00000010
#define INTERNET_FLAG_OFFLINE                            0x00000010
#define INTERNET_FLAG_DONT_CARE				             INTERNET_FLAG_NO_CACHE_WRITE


*
* File Attributes
*

#define FILE_ATTRIBUTE_READONLY         				 1
#define FILE_ATTRIBUTE_HIDDEN           				 2
#define FILE_ATTRIBUTE_SYSTEM              				 4
#define FILE_ATTRIBUTE_DIRECTORY        				 16  
#define FILE_ATTRIBUTE_ARCHIVE          	   			 32  
#define FILE_ATTRIBUTE_ENCRYPTED        				 64 
#define FILE_ATTRIBUTE_NORMAL           				 128  
#define FILE_ATTRIBUTE_TEMPORARY        				 256  
#define FILE_ATTRIBUTE_SPARSE_FILE      				 512
#define FILE_ATTRIBUTE_REPARSE_POINT    				 1024	
#define FILE_ATTRIBUTE_COMPRESSED       				 2048  
#define FILE_ATTRIBUTE_OFFLINE          				 4096


#define BIT_ATTRIBUTE_READONLY         				 	 0
#define BIT_ATTRIBUTE_HIDDEN           				 	 1
#define BIT_ATTRIBUTE_SYSTEM              				 2
#define BIT_ATTRIBUTE_DIRECTORY        				 	 4  
#define BIT_ATTRIBUTE_ARCHIVE          	   			 	 5
#define BIT_ATTRIBUTE_ENCRYPTED							 6  
#define BIT_ATTRIBUTE_NORMAL           				 	 7  
#define BIT_ATTRIBUTE_TEMPORARY        				 	 8
#define BIT_ATTRIBUTE_SPARSE_FILE						 9
#define BIT_ATTRIBUTE_REPARSE_POINT                      10  
#define BIT_ATTRIBUTE_COMPRESSED       				 	 11  
#define BIT_ATTRIBUTE_OFFLINE          				 	 12  


* Flags for InternetAutodial
#define INTERNET_AUTODIAL_FORCE_ONLINE          	     1
#define INTERNET_AUTODIAL_FORCE_UNATTENDED      	     2
#define INTERNET_AUTODIAL_FAILIFSECURITYCHECK   	     4

*
* FTP
*TYPE

#define FTP_TRANSFER_TYPE_UNKNOWN   			         0
#define FTP_TRANSFER_TYPE_ASCII     			         1
#define FTP_TRANSFER_TYPE_BINARY    			         2

#define INTERNET_FLAG_TRANSFER_ASCII    FTP_TRANSFER_TYPE_ASCII
#define INTERNET_FLAG_TRANSFER_BINARY   FTP_TRANSFER_TYPE_BINARY


* Flags for InternetGetConnectedState
#define INTERNET_CONNECTION_MODEM                       1
#define INTERNET_CONNECTION_LAN                         2
#define INTERNET_CONNECTION_PROXY                       4
#define INTERNET_CONNECTION_MODEM_BUSY                  8
#define INTERNET_RAS_INSTALLED                       0x10
#define INTERNET_CONNECTION_OFFLINE                  0x20
#define INTERNET_CONNECTION_CONFIGURED               0x40


#define FTP_TRANSFER_TYPE_MASK      			         3  && (FTP_TRANSFER_TYPE_ASCII | FTP_TRANSFER_TYPE_BINARY)

*
* Internet Variables

#define INTERNET_INVALID_PORT_NUMBER    0           && use the protocol-specific default

#define INTERNET_DEFAULT_FTP_PORT       21          && default for FTP servers
#define INTERNET_DEFAULT_GOPHER_PORT    70          &&    "     "  gopher "
#define INTERNET_DEFAULT_HTTP_PORT      80          &&    "     "  HTTP   "
#define INTERNET_DEFAULT_HTTPS_PORT     443         &&    "     "  HTTPS  "
#define INTERNET_DEFAULT_SOCKS_PORT     1080        && default for SOCKS firewall servers.

#define MAX_CACHE_ENTRY_INFO_SIZE       4096

#define INTERNET_SERVICE_FTP    1
#define INTERNET_SERVICE_GOPHER 2
#define INTERNET_SERVICE_HTTP   3

#define INTERNET_FLAG_PASSIVE           0x08000000  && used for FTP connections
#define INTERNET_FLAG_ASYNC             0x10000000  && this request is asynchronous (where supported)
#define INTERNET_FLAG_FROM_CACHE        0x01000000  &&


#define INTERNET_OPEN_TYPE_PRECONFIG    0   && use registry configuration
#define INTERNET_OPEN_TYPE_DIRECT       1   && direct to net
#define INTERNET_OPEN_TYPE_PROXY        3   && via named proxy
#define INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY 4 



#define PRE_CONFIG_INTERNET_ACCESS  INTERNET_OPEN_TYPE_PRECONFIG
#define LOCAL_INTERNET_ACCESS       INTERNET_OPEN_TYPE_DIRECT
#define GATEWAY_INTERNET_ACCESS     2   && Internet via gateway
#define CERN_PROXY_INTERNET_ACCESS  INTERNET_OPEN_TYPE_PROXY


#define cNULL									         CHR(0)
#define MAX_PATH 								         260


#define GENERIC_READ                     0x80000000
#define GENERIC_WRITE                    0x40000000


#DEFINE _FTPS_RWF_Resume  0
#DEFINE _FTPS_RWF_File    0
#DEFINE _FTPS_RWF_String  1
#DEFINE _FTPS_RWF_Rewrite 2
#DEFINE _FTPS_RWF_Array   4


#DEFINE _FTPS_MaxFileSize 15000000


#DEFINE _FTPS_Option_CurrentDir           1
#DEFINE _FTPS_Option_DLL_Loaded           2
#DEFINE _FTPS_Option_Inet_Handle          3
#DEFINE _FTPS_Option_Connect_Handle       4
#DEFINE _FTPS_Option_IPAddress            5
#DEFINE _FTPS_Option_UserName             6
#DEFINE _FTPS_Option_Password             7
#DEFINE _FTPS_Option_Port                 8
#DEFINE _FTPS_Option_UseProxy             9

#DEFINE _FTPS_FA_Maxsize        20
#DEFINE _FTPS_FA_Default        1
#DEFINE _FTPS_FA_AccessType     2
#DEFINE _FTPS_FA_ModeFile       3

#DEFINE _FTPS_FA_MF_Error       2
#DEFINE _FTPS_FA_MF_Append      1
#DEFINE _FTPS_FA_MF_New         0


#DEFINE _FTPS_UE_FNDEF     -50001 && file not define
#DEFINE _FTPS_UE_FNEXISTS  -50002 && file not exists
#DEFINE _FTPS_UE_CNF       -50003 && Cannot read file
#DEFINE _FTPS_UE_CCF       -50004 && Cannot create file
#DEFINE _FTPS_UE_COF       -50005 && Cannot open file
#DEFINE _FTPS_UE_CWF       -50006 && Cannot write data to file
#DEFINE _FTPS_UE_PLF       -50007 && Parse list failed

#IFNDEF CRLF
  #DEFINE CRLF CHR(13)+CHR(10)
#ENDIF
  
#DEFINE _FTPC_PL_EPLF 0 && EPLF
#DEFINE _FTPC_PL_UNIX 1 && UNIX 
#DEFINE _FTPC_PL_MSWIN 2 && MSWIN
#DEFINE _FTPC_PL_DOS 3 && DOS
#DEFINE _FTPC_PL_NWL 4 && Novell
#DEFINE _FTPC_PL_MAC 5 && Mac