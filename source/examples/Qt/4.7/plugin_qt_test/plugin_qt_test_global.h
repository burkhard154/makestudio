#ifndef PLUGIN_QT_TEST_GLOBAL_H
#define PLUGIN_QT_TEST_GLOBAL_H

#include <QtCore/qglobal.h>

#ifdef __WIN32__
#ifdef PLUGIN_QT_TEST_LIBRARY
#define DLL_C_EXPORT extern "C" __declspec( dllexport )
#else
#define DLL_C_EXPORT extern "C" __declspec( dllimport )
#endif
#include <windows.h>
#else
#define DLL_C_EXPORT extern "C"
#endif


#endif // PLUGIN_QT_TEST_GLOBAL_H
