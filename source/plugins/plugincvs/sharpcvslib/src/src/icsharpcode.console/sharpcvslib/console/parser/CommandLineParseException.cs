using System;

namespace ICSharpCode.SharpCvsLib.Console.Parser
{
	/// <summary>
	/// The command line parse exception is thrown when invalid parameters are 
	///     passed into the parsing routine.  The exception can then be caught
	///     in the main execution stream and allow the program to exit
	///     gracefully.
	/// </summary>
	public class CommandLineParseException : Exception{
        /// <summary>
        /// Create a new exception passing a message.  The message should contain
        ///     information that will make it easier to debug the problem that
        ///     has occurred.
        /// </summary>
        /// <param name="msg">A useful message that will help a developer debug
        ///     the problem that has occurred.</param>
		public CommandLineParseException(String msg) : base (msg) {
		}

        /// <summary>
        /// Create a new exception passing a message.  The message should contain
        ///     information that will make it easier to debug the problem that
        ///     has occurred.  In addition to the message the inner exception is
        ///     passed in to preserve the stack trace.
        /// </summary>
        /// <param name="msg"></param>
        /// <param name="e"></param>
        public CommandLineParseException (String msg, Exception e) : base (msg, e) {

        }
	}
}
