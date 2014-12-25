REM generate jvcsmak.dll
"C:\Program Files\Microsoft.NET\SDK\v4.0\Bin\tlbimp" 
rem ..\source\framework\makestudio.tlb /transform:dispret /verbose /namespace:MakeStudio
copy makestudio.dll .\bin

REM -- 
PAUSE


