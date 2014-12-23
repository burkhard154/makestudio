REM Die Umgebungsvariable VS71COMNTOOLS ist in im Kontextmenü des Arbeitsplatzes unter:
REM 	Eigenschaften\Erwitert\Button Umgebungsvariablen\Systemvariablen
REM zu finden!
REM Bei einer Standardinstallation von MS Visual Studio steht sie auf
REM 	c:\Programme\Microsoft Visual Studio .NET 2003\Common7\Tools
REM Ggfs. ist sie so anzupassen, dass sie auf die tools zeigt!!

rem call "%VS71COMNTOOLS%"\vsvars32.bat

REM generate jvcsmak.dll
"C:\Program Files\Microsoft.NET\SDK\v2.0\Bin\tlbimp" .\jvcsmak.tlb /transform:dispret /verbose /namespace:JediMake
copy jvcsmak.dll .\bin

REM -- PAUSE


