#region "Copyright"
//
// Copyright (C) 2003 Steve Kenzell
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// As a special exception, the copyright holders of this library give you
// permission to link this library with independent modules to produce an
// executable, regardless of the license terms of these independent
// modules, and to copy and distribute the resulting executable under
// terms of your choice, provided that you also meet, for each linked
// independent module, the terms and conditions of the license of that
// module.  An independent module is a module which is not derived from
// or based on this library.  If you modify this library, you may extend
// this exception to your version of the library, but you are not
// obligated to do so.  If you do not wish to do so, delete this
// exception statement from your version.
//
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;
using System.IO;
using System.Reflection;
using System.Diagnostics;

namespace ICSharpCode.SharpCvsLib {
	/// <summary>
	/// The assembly helper class is used to load assemblies dynamically if needed.
	/// </summary>
	public class AssemblyHelper {
        /// <summary>
        /// TODO: DOCUMENT ME!
        /// </summary>
        public const string SHARPCVSLIB = "ICSharpCode.SharpCvsLib";
        /// <summary>
        /// TODO: DOCUMENT ME!
        /// </summary>
        public const string SHARPCVSLIB_CONSOLE = "ICSharpCode.SharpCvsLib.Console";
        /// <summary>
        /// TODO: DOCUMENT ME!
        /// </summary>
        public const string SHARPZIPLIB = "ICSharpCode.SharpZipLib";
        /// <summary>
        /// TODO: DOCUMENT ME!
        /// </summary>
        public const string LOG4NET = "log4net";

        /// <summary>
        /// Create a new instance of the assembly helper.  
        /// </summary>
		private AssemblyHelper() {
		}

        /// <summary>
        /// Load the given assembly.
        /// </summary>
        /// <param name="dllName"></param>
        /// <returns></returns>
        public static Assembly LoadAssembly (string dllName) {
            Assembly sharpcvslib = null;
            try {
                // initial search location.
                DirectoryInfo baseDir = 
                    new DirectoryInfo(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));
                ArrayList possibleDlls = new ArrayList();
                possibleDlls.Add(
                    new FileInfo(Path.Combine(baseDir.FullName, dllName + ".dll")));

                // add some other cantidates
                DirectorySearch(possibleDlls, baseDir, dllName);

                foreach (FileInfo file in possibleDlls) {
                    try {
                        sharpcvslib = Assembly.LoadFrom(file.FullName);
                        AppDomain.CurrentDomain.AppendPrivatePath(Path.GetDirectoryName(sharpcvslib.Location));
                        break;
                    } catch (Exception) {
                        // keep on going
                    }
                }
            } catch (Exception) {
                // unable to load all assemblies, exit.
                System.Environment.Exit(-1);
            }
            return sharpcvslib;
        }

        private static void DirectorySearch(ArrayList matches, DirectoryInfo dirInfo, string assemblyName) {
            try {
                foreach (DirectoryInfo directory in dirInfo.GetDirectories()) {
                    foreach (FileInfo file in directory.GetFiles(assemblyName + "*")) {
                        matches.Add(file);
                    }
                    DirectorySearch(matches, directory, assemblyName);
                }
            }
            finally {
                // do nothing
            }
        }

        /// <summary>
        /// Load the version of the assembly referenced by this assembly.
        /// </summary>
        /// <param name="assembly"></param>
        public static Assembly LoadLog4Net (Assembly assembly) {
            Assembly assemblyLoaded = null;
            BuildPrivateBinPath(assembly, LOG4NET);
            assemblyLoaded = Assembly.Load(LOG4NET);
            return assemblyLoaded;
        }

        private static void BuildPrivateBinPath (Assembly assembly, string referencedAssembly) {
            DirectoryInfo baseDir = 
                new DirectoryInfo(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));
            ArrayList possibleDlls = new ArrayList();
            FileInfo rootMatch = new FileInfo(Path.Combine(baseDir.FullName, referencedAssembly + ".dll"));
            if (rootMatch.Exists) {
                possibleDlls.Add(
                    rootMatch);
            }

            // add some other cantidates
            DirectorySearch(possibleDlls, baseDir, referencedAssembly);
            FileInfo match = null;

            foreach (FileInfo possibleDll in possibleDlls) {
                if (IsMatch(Path.GetDirectoryName(assembly.Location), 
                    Path.GetDirectoryName(possibleDll.FullName))) {
                    match = possibleDll;
                    break;
                }
            }
            if (match == null) {
                throw new Exception("Unable to find correct version of referenced assembly.");
            }
            AppDomain.CurrentDomain.AppendPrivatePath(Path.GetDirectoryName(match.FullName));
        }

        private static bool IsMatch (string consoleDir, string log4netDir) {
            AppDomainSetup setup = new AppDomainSetup();
            setup.ApplicationBase = consoleDir;
            setup.PrivateBinPath = log4netDir;
            setup.ApplicationName = "ICSharpCode.SharpCvslib.Console";
            AppDomain appDomain = AppDomain.CreateDomain("Cvs", null, setup);

            bool isMatch;
            try {
                object console = appDomain.CreateInstanceAndUnwrap(SHARPCVSLIB_CONSOLE, 
                    "ICSharpCode.SharpCvsLib.Console.ConsoleMain");
                isMatch = true;
            } catch (System.Reflection.TargetInvocationException) {
                isMatch = false;
            } catch (Exception) {
                isMatch = true;
            }
            return isMatch;
        }
	}
}
