using System;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Misc {
	/// <summary>
	/// The unsupported protocol exception is thrown when a client attempts to use
	///     a protocol that is not understood or not supported currently by the
	///     library.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
	[Obsolete("Use ICSharpCode.SharpCvsLib.Exceptions.UnsupportedProtocolException.")]
	public class UnsupportedProtocolException : NotImplementedException{
        /// <summary>
        /// Indicate that an unknown protocol has been used.
        /// </summary>
        /// <param name="msg">A useful message that will help a developer debug
        ///     the problem that has occurred.</param>
		public UnsupportedProtocolException(String msg) : base (msg) {
		}

        /// <summary>
        /// Indicate that an unknown protocol has been used.
        /// </summary>
        /// <param name="msg">A useful message that will help a developer debug
        ///     the problem that has occurred.</param>
        /// <param name="e"></param>
        public UnsupportedProtocolException (String msg, Exception e) : base (msg, e) {

        }
	}
}
