#region "Copyright"
// Copyright (C) 2003 Clayton Harbour
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
//
#endregion

using System;
using System.Configuration;
using System.Xml;
using System.Xml.Serialization;
using System.Xml.XPath;

using log4net;

namespace ICSharpCode.SharpCvsLib.Tests.Config {

    /// <summary>
    ///     Handles loading of the sharpcvslib configuration file.
    /// </summary>
    public class SharpCvsLibTestsConfigHandler : IConfigurationSectionHandler {

        private readonly ILog LOGGER = LogManager.GetLogger (typeof (SharpCvsLibTestsConfigHandler));
        /// <summary>
        /// Application configuration node name.
        /// </summary>
        public const String APP_CONFIG_SECTION = "sharpcvslib-tests";

        /// <summary>
        /// Create the configuration section.
        /// </summary>
        /// <param name="parent"></param>
        /// <param name="configContext"></param>
        /// <param name="section"></param>
        /// <returns></returns>
        public object Create(object parent,
                            object configContext,
                            XmlNode section) {
            LOGGER.Debug ("Attempting to load configuration handler for tests.");
            XPathNavigator nav = section.CreateNavigator();
            String typename = (String) nav.Evaluate("string(@type)");
            Type type = Type.GetType(typename);
            object theObject = this.GetConfigObject (type,
                            section.SelectSingleNode ("//" + SharpCvsLibTestsConfig.SUB_SECTION));

            LOGGER.Debug("Configuration Loaded");
            LOGGER.Debug("Config=[" + theObject + "]");
            return theObject;
        }

        /// <summary>
        /// Get the object using the xml serializer.
        /// </summary>
        /// <param name="type">The type of object we are expecting.</param>
        /// <param name="node">Xml node to look for in the config file.</param>
        /// <returns>The inflated xml object.</returns>
        private object GetConfigObject (Type type, XmlNode node) {
            object theObject;
            XmlSerializer ser = new XmlSerializer(type);
            theObject = ser.Deserialize(new XmlNodeReader(node));

            return theObject;

        }

    }
}
