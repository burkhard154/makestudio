@ECHO ****************************************
@ECHO Building wizard resources

del /F plugintemplate.res
del /F plugintemplate_csharp.res
del /F plugintemplate_delphi.res

brcc32 plugintemplate.rc
brcc32 plugintemplate_csharp.rc
brcc32 plugintemplate_delphi.rc

copy /Y plugintemplate.res ..\
copy /Y plugintemplate_csharp.res ..\
copy /Y plugintemplate_delphi.res ..\

PAUSE