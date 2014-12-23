// project created on 18/07/2003 at 6:10 PM
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
//    <author>Steve Kenzell</author>
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;
using System.IO;
using System.Reflection;

namespace ICSharpCode.SharpCvsLib {

    /// <summary>
    /// Provides the main entry point into the console application.  Calls the
    ///     ConsoleMain delegate in order to limit/ break the connection to the
    ///     command line as much as possible.
    /// </summary>
    public class MainClass {
        /// <summary>
        ///  Static Main method.
        /// </summary>
        public static void Main(String[] args) {
            Assembly sharpziplib = AssemblyHelper.LoadAssembly(AssemblyHelper.SHARPZIPLIB);
            Assembly sharpcvslib = AssemblyHelper.LoadAssembly(AssemblyHelper.SHARPCVSLIB);
            Assembly sharpcvslibConsole = AssemblyHelper.LoadAssembly(AssemblyHelper.SHARPCVSLIB_CONSOLE);
            
            try {
                Assembly log4net = AssemblyHelper.LoadLog4Net(sharpcvslibConsole);
            } catch (Exception) {
                // unable to load log4net, get on with it.
                //System.Console.WriteLine(e.Message);
            }
            //Assembly log4net = AssemblyHelper.LoadAssembly(AssemblyHelper.LOG4NET);

            Type type = sharpcvslibConsole.GetType("ICSharpCode.SharpCvsLib.Console.ConsoleMain");
            object console = Activator.CreateInstance(type);

            if (null == args) {
                args = new string[1];
            }
            MethodInfo initLogger = console.GetType().GetMethod("InitLog4net", new Type[0]);
            initLogger.Invoke(console, null);
            console.GetType().GetProperty("Args").SetValue(console, args, null);
            MethodInfo execute = console.GetType().GetMethod("Execute", new Type[0]);
            execute.Invoke(console, null);
        }
    }
}
