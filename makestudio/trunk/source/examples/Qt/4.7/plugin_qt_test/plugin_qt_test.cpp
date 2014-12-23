#include "plugin_qt_test.h"
#include <QtGui/QMessageBox>

DLL_C_EXPORT void DllTestProc(){
    QMessageBox::information( NULL, "Aus der DLL", "Erfolgreicher Aufruf der DLL");
}

DLL_C_EXPORT BOOL WINAPI DllMain(
    HINSTANCE hinstDLL,  // handle to DLL module
    DWORD dwReason,     // reason for calling function
    LPVOID /*lpReserved*/ )  // reserved
{
    return TRUE;
}
