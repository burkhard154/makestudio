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
//    Author        Clayton Harbour
//
#endregion

using System;
using System.Configuration;
using System.IO;
using System.Xml;
using System.Xml.Serialization;
using System.Xml.XPath;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Config {

/// <summary>
///     Tests that ensure the configuration handler is loading the
///         application configuration settings correctly.
/// </summary>
[TestFixture]
public class SharpCvsLibConfigHandlerTest {
    private const String CONFIG_FILE = "ICSharpCode.SharpCvsLib.dll.config";

    private const int TIMEOUT = 1000;
    private const int AUTH_SLEEP = 1000;

    private readonly ILog LOGGER =
        LogManager.GetLogger (typeof (SharpCvsLibConfigHandlerTest));

    /// <summary>
    /// Test that all configuration settings are loaded correctly.
    /// </summary>
    [Test]
    public void LoadAppConfigTest () {
        SharpCvsLibConfigHandler configHandler =
            new SharpCvsLibConfigHandler ();
        ConfigXmlDocument xmlDoc =
            new ConfigXmlDocument ();

        xmlDoc.Load (CONFIG_FILE);

        SharpCvsLibConfig config = null;
        config =
            (SharpCvsLibConfig)configHandler.Create
            (xmlDoc.SelectSingleNode ("configuration"),
             null,
             xmlDoc.SelectSingleNode ("//" +
                                      SharpCvsLibConfigHandler.APP_CONFIG_SECTION));
        this.CheckValues (config);
    }

    private void CheckValues (SharpCvsLibConfig config) {
        Assert.AreEqual (TIMEOUT,
                                1000, "timeout value");
        Assert.AreEqual (AUTH_SLEEP,
                                1000,
            "authorization sleep value");
    }

    /// <summary>
    /// Test that we can get the config file from the
    ///     configuration settings.  This config file is the config
    ///     file that is loaded with the test assembly and should
    ///     mirror the gallery config file.
    /// </summary>
    [Test]
    public void GetConfigTestFile () {
        SharpCvsLibConfig config =
            (SharpCvsLibConfig)ConfigurationSettings.GetConfig
            (SharpCvsLibConfigHandler.APP_CONFIG_SECTION);

        this.CheckValues (config);
    }

}
}
