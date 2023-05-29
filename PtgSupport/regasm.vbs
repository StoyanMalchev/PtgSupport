Set WshShell = WScript.CreateObject("WScript.Shell")
strCmd = wshShell.ExpandEnvironmentStrings("%SystemRoot%") & "\Microsoft.NET\Framework\v4.0.30319\RegAsm.exe PtgDotNet.dll \codebase"
'WScript.echo(strCmd)
Set objExec=WshShell.Exec(strCmd)
