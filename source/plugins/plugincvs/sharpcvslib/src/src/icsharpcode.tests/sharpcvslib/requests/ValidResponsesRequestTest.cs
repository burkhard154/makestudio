#region "Copyright"
// Copyright (C) 2003 Gerald Evans
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
//    <author>Gerald Evans</author>
#endregion

using System;
using System.Collections;
using System.IO;
using System.Reflection;
using System.Threading;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Responses;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    ///     Test ValidResponsesRequest
    ///
    ///     cvsclient.info from the 1.11.6 release of cvs (http://ccvs.cvshome.org)
    ///     was used as the basis for these tests.
    /// </summary>
    [TestFixture]
    public class ValidResponsesRequestTest {
        private static readonly ILog LOGGER =
            LogManager.GetLogger (typeof (ValidResponsesRequestTest));

        /// <summary>
        ///     Just tests the request name and virtual boolean values.
        /// </summary>
        [Test]
        public void BasicTest()
        {
            IRequest request = new ValidResponsesRequest();
            string requestString = request.RequestString;
            string[] responses;

            // split the responses in the request into seperate strings
            responses = requestString.Split(null);

            Assert.IsTrue(responses.Length > 1);
            Assert.AreEqual(responses[0], "Valid-responses");

            Assert.IsTrue(!request.IsResponseExpected);
            Assert.IsTrue(!request.DoesModifyConnection);
        }

        /// <summary>
        ///     Tests that the responses that we say we support
        ///     in this message really are supported.
        ///
        ///     We determine if a message is supported or not
        ///     by whether the ResponseFactory can create the response class or not.
        ///
        ///     TODO: Check if there are any responses that we want to tell the server
        ///     that we support, but can safely ignore if the server actually
        ///     sends them to us.
        /// </summary>
        [Test]
        public void AllResponsesSupportedTest()
        {
            IRequest request = new ValidResponsesRequest();
            string requestString = request.RequestString;
            string[] responses;

            // Need to remove terminating '\n' to simplify Split()
            if (requestString.Length > 0 && requestString[requestString.Length - 1] == '\n') {
                requestString = requestString.Substring(0, requestString.Length - 1);
            }
            // split the responses in the request into seperate strings
            responses = requestString.Split(null);

            // Check that we really do support each of these responses
            for (int responseNum = 1; responseNum < responses.Length; responseNum++) {
                IResponse response = ResponseFactory.CreateResponse(responses[responseNum]);
                Assert.IsNotNull(response, responses[responseNum] + " not really supported");
            }
        }

        /// <summary>
        ///     The idea here is to check that when a new response class is written
        ///     it is also added into the Valid-responses request.
        ///
        ///     We can easily look in the assembly to find the response classes
        ///     but what we can't do is from this class fins its corresponding
        ///     request string as this is hard coded in the factory.
        ///
        ///     So instead we just check that the counts match.
        /// </summary>
        [Test]
        public void NoMissingResponsesTest()
        {
            IRequest request = new ValidResponsesRequest();
            string requestString = request.RequestString;
            string[] responses;
            int responseClasses = 0;

            // Need to remove terminating '\n' to simplify Split()
            if (requestString.Length > 0 && requestString[requestString.Length - 1] == '\n') {
                requestString = requestString.Substring(0, requestString.Length - 1);
            }
            // split the responses in the request into seperate strings
            // remember first entry will be the response name
            responses = requestString.Split(null);

            Assembly cvsLibAssembly = Assembly.GetAssembly(request.GetType());
            Assert.IsNotNull(cvsLibAssembly);

            Type[] types = cvsLibAssembly.GetTypes();
            foreach(Type t in types) {
                // There are several ways of looking for the response classes.
                // The method chosen here is to look for all classes that
                // implement IResponse
                if (t.IsClass && t.GetInterface("IResponse") != null) {
                    System.Console.WriteLine(t.Name);
                    responseClasses++;
                }
            }

            Assert.AreEqual(responseClasses, responses.Length - 1, "Mismatch betwen response classes and the Valid-responses request");
        }
    }
}
