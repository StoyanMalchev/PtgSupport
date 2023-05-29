TEXT TO lcJetsons
{
  fullname: 'George Jetson',
  employer: 'Spacely Sprockets',
  spouse: { fullname: 'Jane', age: 'unspecified', shopping: true },
  children: [ 'Elroy', 'Judy' ],
  isCool: true
}
ENDTEXT
loOBJ=NEWOBJECT()
someObj = Jsonparse( lcJetsons )
??


PROCEDURE Jsonparse
PARAMETERS pcJSON, poTarget
LOCAL lcScript
LOCAL sc AS scriptcontrol
IF PCOUNT() = 1
	poTarget = NEWOBJECT("Empty")
ENDIF

sc = CREATEOBJECT("scriptcontrol")
sc.Language="JScript"
TEXT TO lcScript TEXTMERGE NOSHOW
	function parseJSON() {
	  var json = << pcJSON >>;
	  var str = "";
	  for (var name in json) {
	    var realType = RealTypeOf(json[name]);
	    if ( realType == "array" ) {
	      str += "ADDPROPERTY(poTarget,'"+name+"(1)',.f.)\r\n";
	      for (var i = 0;i < json[name].length;i++) {
	        var arrayElement = "poTarget."+name+"["+(i+1)+"]"
	        str += "DIMENSION "+arrayElement+"\r\n";
	        var value = json[name][i];
		    var realType = RealTypeOf(value);
	        if ( realType == "object") {
	          str += arrayElement + " =  jsonParse(\""+toJson(value)+"\")\r\n";
	        } else if ( realType == "string" ) {
	          str += arrayElement + ' = "'+value+'"'+"\r\n";
	        } else if ( realType == "number" ) {
	          str += arrayElement + " = "+value+"\r\n";
	        } else if ( realType == "date" ) {
	          str += arrayElement + " = {"+value+"}\r\n";
	        } else if ( realType == "boolean" ) {
		      str += arrayElement + " = "+((value)?".t.":".f.")+"\r\n";
            }
          }
        } else if ( realType == "string" ) {
	      str += "ADDPROPERTY(poTarget,'"+name+"',["+json[name]+"])\r\n";
	    } else if ( realType == "number" ) {
	      str += "ADDPROPERTY(poTarget,'"+name+"',"+json[name]+")\r\n";
	    } else if ( realType == "date" ) {
	      str += "ADDPROPERTY(poTarget,'"+name+"',{"+json[name]+"})\r\n";
	    } else if (realType == "boolean" ) {
	      str += "ADDPROPERTY(poTarget,'"+name+"',"+((json[name])?".t.":".f.")+")\r\n";
        } else if ( realType == "object") {
	      str += "ADDPROPERTY(poTarget,'"+name+"',jsonParse(\""+toJson(json[name])+"\"))\r\n";
        }
      }
	  return str;
    }

	function RealTypeOf(v) {
          if (typeof(v) == "object") {
            if (v == null) return "null";
            if (v.constructor == (new Array).constructor) return "array";
            if (v.constructor == (new Date).constructor) return "date";
            if (v.constructor == (new RegExp).constructor) return "regex";
            return "object";
          }
          return typeof(v);
        }

        function toJson(obj) {
          switch (typeof obj) {
            case 'object':
              if (obj) {
                  var list = [];
                  if (obj instanceof Array) {
                       for (var i=0;i < obj.length;i++) {
                             list.push(toJson(obj[i]));
                       }
                       return '[' + list.join(',') + ']';
                  } else {
                       for (var prop in obj) {
                             list.push(prop + ':' + toJson(obj[prop]));
                       }
                       return '{' + list.join(',') + '}';
                  }
              } else {
                  return 'null';
              }
            case 'string':
              return "'" + obj.replace(/(['])/g, '\\$1') + "'";
            case 'number':
            case 'boolean':
              return new String(obj);
          }
        }
ENDTEXT
sc.AddCode(lcScript)
lcScript = sc.Eval("parseJSON();")
EXECSCRIPT(lcScript)
sc = null
RELEASE sc

RETURN poTarget
