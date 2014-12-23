#region "Copyright"
// MessageTaggedResponse.cs
// Copyright (C) 2001 Mike Krueger
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
#endregion

using System;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;

namespace ICSharpCode.SharpCvsLib.Responses {

    /// <summary>
    /// Message tagged response.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class MessageTaggedResponse : AbstractResponse {
        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (MessageTaggedResponse));

        /// <summary>
        /// MT tagname data \n
        ///     This response provides for tagged text. It is similar to SGML/HTML/XML 
        ///     in that the data is structured and a naive application can also make 
        ///     some sense of it without understanding the structure. The syntax is 
        ///     not SGML-like, however, in order to fit into the CVS protocol better 
        ///     and (more importantly) to make it easier to parse, especially in a 
        ///     language like perl or awk. The tagname can have several forms. If it 
        ///     starts with `a' to `z' or `A' to `Z', then it represents tagged text. 
        ///     If the implementation recognizes tagname, then it may interpret data 
        ///     in some particular fashion. If the implementation does not recognize 
        ///     tagname, then it should simply treat data as text to be sent to the 
        ///     user (similar to an `M' response). There are two tags which are 
        ///     general purpose. The `text' tag is similar to an unrecognized tag in 
        ///     that it provides text which will ordinarily be sent to the user. The 
        ///     `newline' tag is used without data and indicates that a newline will 
        ///     ordinarily be sent to the user (there is no provision for embedding 
        ///     newlines in the data of other tagged text responses). If tagname 
        ///     starts with `+' it indicates a start tag and if it starts with `-' it 
        ///     indicates an end tag. The remainder of tagname should be the same for 
        ///     matching start and end tags, and tags should be nested (for example 
        ///     one could have tags in the following order +bold +italic text -italic 
        ///     -bold but not +bold +italic text -bold -italic). A particular start 
        ///     and end tag may be documented to constrain the tagged text responses 
        ///     which are valid between them. Note that if data is present there will 
        ///     always be exactly one space between tagname and data; if there is more 
        ///     than one space, then the spaces beyond the first are part of data. 
        /// </summary>
        /// <example>
        /// Here is an example of some tagged text responses. Note that there is a 
        /// trailing space after `Checking in' and `initial revision:' and there are 
        /// two trailing spaces after `&gt;--'. Such trailing spaces are, of course, part 
        /// of data.
        /// 
        ///     MT +checking-in
        ///     MT text Checking in 
        ///     MT fname gz.tst
        ///     MT text ;
        ///     MT newline
        ///     MT rcsfile /home/kingdon/zwork/cvsroot/foo/gz.tst,v
        ///     MT text   &gt;--  MT fname gz.tst
        ///     MT newline
        ///     MT text initial revision: 
        ///     MT init-rev 1.1
        ///     MT newline
        ///     MT text done
        ///     MT newline
        ///     MT -checking-in
        ///
        /// If the client does not support the `MT' response, the same responses might 
        ///     be sent as:
        ///
        ///     M Checking in gz.tst;
        ///     M /home/kingdon/zwork/cvsroot/foo/gz.tst,v  &gt;--  gz.tst
        ///     M initial revision: 1.1
        ///     M done
        ///     
        /// </example>
        public override void Process() {
            string message = this.ReadLine();
            // Fire message event to the client app
            Services.SendMessage("MT " + message);
            String msg = "cvs server: MT " + message;
            LOGGER.Debug (msg);
        }

        /// <summary>
        /// Indicator stating whether the response is terminating or not.
        /// </summary>
        public override bool IsTerminating {
            get {return false;}
        }
    }
}
