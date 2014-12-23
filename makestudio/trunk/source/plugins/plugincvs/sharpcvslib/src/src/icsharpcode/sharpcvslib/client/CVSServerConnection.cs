#region "Copyright"
// CVSServerConnection.cs
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
using System.Configuration;
using System.IO;
using System.Collections;
using System.Threading;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Diagnostics;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Config;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Responses;
using ICSharpCode.SharpCvsLib.FileHandler;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Streams;
using ICSharpCode.SharpCvsLib.Logs;
using ICSharpCode.SharpCvsLib.Protocols;

using log4net;

namespace ICSharpCode.SharpCvsLib.Client {

    /// <summary>
    /// Cvs server connection, handles connections to the cvs server.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class CVSServerConnection : 
        IConnection, IResponseServices, ICommandConnection {

        private static readonly ILog LOGGER =
            LogManager.GetLogger (typeof (CVSServerConnection));

        private string nextFileDate;

        private CvsStream inputStream;
        private CvsStream outputStream;

        private WorkingDirectory repository;

        private IFileHandler uncompressedFileHandler =
            new UncompressedFileHandler();
        private IFileHandler compressedFileHandler =
            new CompressedFileHandler();

        private const String PSERVER_AUTH_SUCCESS = "I LOVE YOU";
        private const String PSERVER_AUTH_FAIL = "I HATE YOU";

        private RequestLog requestLog;
        private ResponseLog responseLog;

        private SharpCvsLibConfig config;

        private IProtocol protocol;

        /// <summary>
        /// Fired when an attempt is made to connect to the repository.
        /// </summary>
        public event ProcessEventHandler StartProcessEvent;
        /// <summary>
        /// Fired when all processing is done and the connection is closed.
        /// </summary>
        public event ProcessEventHandler StopProcessEvent;

        /// <summary>
        ///     Initialize the cvs server connection.
        /// </summary>
        public CVSServerConnection () : this(DeriveWorkingDirectory()) {

        }

        /// <summary>
        /// Create a new connection and initialize with the working directory
        /// object.
        /// </summary>
        /// <param name="workingDirectory"></param>
        public CVSServerConnection (WorkingDirectory workingDirectory)  {
            this.repository = workingDirectory;
            this.Init();
        }

        private void Init () {
            inputStream  = new CvsStream (new MemoryStream());
            outputStream = new CvsStream (new MemoryStream());

            this.config = SharpCvsLibConfig.GetInstance();
            try {
                if (config.Log.DebugLog.Enabled) {
                    requestLog = new RequestLog ();
                    responseLog = new ResponseLog ();

                    this.InputStream.RequestMessage.MessageEvent +=
                        new EncodedMessage.MessageHandler (requestLog.Log);
                    this.OutputStream.ResponseMessage.MessageEvent +=
                        new EncodedMessage.MessageHandler (responseLog.Log);
                }
            } catch (Exception e) {
                LOGGER.Error (e);
            }

            if (null == config) {
                config = new SharpCvsLibConfig ();
            }
            LOGGER.Debug("Config=["  + config.ToString() + "]");

            if (this.repository == null) {
                this.repository = DeriveWorkingDirectory();
            }
        }

        private static WorkingDirectory DeriveWorkingDirectory () {
            DirectoryInfo currDir = new DirectoryInfo(Environment.CurrentDirectory);
            LOGGER.Info(string.Format("Repository is null, " +
                "attempting to derive from current directory: {0}.", currDir.FullName));
                    
                Manager manager = new Manager(currDir);
            Repository repository = 
                manager.FetchRepository(currDir.FullName);
            Root root = 
                manager.FetchRoot(currDir.FullName);
            CvsRoot cvsRoot = new CvsRoot(root.FileContents);
            return
                new WorkingDirectory(cvsRoot, Environment.CurrentDirectory, repository.ModuleName);
        }

        /// <summary>
        /// Gets a file handler for files that are not zipped.
        /// </summary>
        public IFileHandler UncompressedFileHandler {
            get {return uncompressedFileHandler;}
        }

        /// <summary>
        ///     Set the time to sleep between sending the authentication request
        ///         and receiving the authentication response.  Accounts for
        ///         slow responses on some servers.
        /// </summary>
        public int AuthSleep {
            get {return this.config.AuthSleep;}
        }

        /// <summary>
        /// The port that should be used for cvs connections.
        /// </summary>
        public int Port {
            get {
                return this.repository.CvsRoot.Port;
            }
        }

        /// <summary>
        /// Cvs input stream writer
        /// </summary>
        public CvsStream InputStream {
            get {return inputStream;}
            set {inputStream = value;}
        }

        /// <summary>
        /// Wrapper for the request message delegate on the input stream.
        /// </summary>
        public EncodedMessage RequestMessage {
            get {return InputStream.RequestMessage;}
        }

        /// <summary>
        /// Wrapper for the response message delegate on the output
        ///     stream.
        /// </summary>
        public EncodedMessage ResponseMessage {
            get {return OutputStream.ResponseMessage;}
        }

        /// <summary>
        /// Cvs output stream reader
        /// </summary>
        public CvsStream OutputStream {
            get {return outputStream;}
            set {outputStream = value;}
        }

        /// <summary>
        /// Message event.
        /// </summary>
        public EncodedMessage MessageEvent = new EncodedMessage ();

        /// <summary>
        /// Occurs when a message is sent to the cvs server.
        /// </summary>
        public event MessageEventHandler RequestMessageEvent;
        /// <summary>
        /// Occurs when a message is received from the cvs server.
        /// </summary>
        public event MessageEventHandler ResponseMessageEvent;

        /// <summary>
        /// This message event is fired when there is an error message returned
        ///     from the server.
        /// </summary>
        public EncodedMessage ErrorMessageEvent = new EncodedMessage();

        /// <summary>
        /// Send the message to the message event handler.
        /// </summary>
        /// <param name="message"></param>
        public void SendMessage(string message) {
            MessageEvent.SendMessage(message);
        }

        private ResponseMessageEvents responseMessageEvents;
        /// <summary>
        /// Property to encapsulate all response message events.
        /// </summary>
        public ResponseMessageEvents ResponseMessageEvents {
            get {
                if (null == this.responseMessageEvents) {
                    this.responseMessageEvents = new ResponseMessageEvents();
                }
                return this.responseMessageEvents;}
        }

        /// <summary>
        /// Use this event to send error messages to a client.
        /// </summary>
        /// <param name="errorMessage">A message that will notify the client 
        ///     as to the nature of the error message.</param>
        public void SendErrorMessage(string errorMessage) {
            ErrorMessageEvent.SendMessage("Error: " + errorMessage);
        }

        /// <summary>
        /// Send the message to the message event handler.
        /// </summary>
        /// <param name="module">The cvs module.</param>
        /// <param name="repositoryPath">The path to the cvs repository.</param>
        /// <param name="filename">The name of the file being manipulated.</param>
        public void SendMessage (String module,
            String repositoryPath,
            String filename) {
            StringBuilder sb = new StringBuilder ();
            sb.Append (module);
            sb.Append (repositoryPath);
            sb.Append (filename);

            this.SendMessage (sb.ToString ());
        }

        /// <summary>
        /// Module to execute cvs commands on.
        /// </summary>
        private class Module {
            public string localdir = null;
            public ArrayList entries = new ArrayList();
        }

        private void HandleResponses() {
            SortedList modules = new SortedList();

            while (true) {
                string responseStr = inputStream.ReadToFirstWS();
                if (LOGGER.IsDebugEnabled) {
                    LOGGER.Debug ("Response : " + responseStr);
                }

                if (responseStr.Length == 0) {
                    SendMessage("server timed out");
                    break;
                }

                IResponse response = 
                    ResponseFactory.CreateResponse(responseStr.Substring(0, responseStr.Length - 1));
                if (LOGGER.IsDebugEnabled) {
                    LOGGER.Debug("cvs server: " + response);
                }

                if (response == null) {
                    if (responseStr.EndsWith(" ")) {
                        inputStream.ReadLine();
                    }
                    break;
                }
                response.Process(inputStream, this);
                if (response.IsTerminating) {
                    break;
                }
                if (null != response && null != response.ResponseString) {
                    try {
                        this.ResponseMessageEvent(this, new MessageEventArgs(response,
                            response.GetType().Name));
                    } catch (NullReferenceException) {
                        LOGGER.Debug("No one is listening to the response message event.");
                    }
                }
            }
        }

        /// <summary>
        /// Submit a request to the cvs repository.
        /// </summary>
        /// <param name="request"></param>
        public void SubmitRequest(IRequest request) {
            if (null != request && null != request.RequestString) {
                if (null != this.RequestMessageEvent) {
                    this.RequestMessageEvent(this, new MessageEventArgs(request,
                        request.GetType().Name));
                }
            }

            outputStream.SendString(request.RequestString);

            if (request.DoesModifyConnection) {
                request.ModifyConnection(this);
            }

            if (request.IsResponseExpected) {
                HandleResponses();
            }
        }

        /// <summary>
        /// Send a file to the cvs repository.
        /// </summary>
        /// <param name="fullPath">The full path to the file being sent to the server.</param>
        /// <param name="isBinary"><code>true</code> if the file is binary; 
        ///     otherwise <code>false</code>.</param>
        public void SendFile(string fullPath, bool isBinary) {
            try {
                if (isBinary) {
                    UncompressedFileHandler.SendBinaryFile(OutputStream, fullPath);
                } else {
                    UncompressedFileHandler.SendTextFile(OutputStream, fullPath);
                }
            } catch (IOException) {
                // try to connect again and retry the send
                this.Connect(this.repository, this.protocol.Password);
                this.SendFile(fullPath, isBinary);
            }
        }

        /// <summary>
        /// Connect to the repository.
        /// </summary>
        /// <param name="repository"></param>
        /// <param name="password"></param>
        public void Connect(WorkingDirectory repository, string password) {
            this.repository = repository;
            if (StartProcessEvent != null) {
                this.StartProcessEvent(this, new ProcessEventArgs());
            }
            Authentication(password);
        }

        /// <summary>
        /// Authentication for the repository.
        /// </summary>
        /// <param name="password"></param>
        public void Authentication(string password) {
            this.protocol = 
                ProtocolFactory.Instance.GetProtocol(repository.CvsRoot.Protocol);

            this.protocol.Repository = this.Repository;
            this.protocol.Password = password;

            protocol.Connect();
            this.inputStream = protocol.InputStream;
            this.outputStream = protocol.OutputStream;


            // TODO: Move these into an abstract request class
            SubmitRequest(new ValidResponsesRequest());
            SubmitRequest(new ValidRequestsRequest());

            SubmitRequest(new UseUnchangedRequest());
            SubmitRequest(new RootRequest(repository.CvsRoot.CvsRepository));

            SubmitRequest(new GlobalOptionRequest(GlobalOptionRequest.Options.QUIET));
        }
    
        /// <summary>
        /// The repository information.
        /// </summary>
        public WorkingDirectory Repository {
            get {return repository;}
        }

        /// <summary>
        /// Next file date.
        /// </summary>
        public string NextFileDate {
            get {return nextFileDate;}
            set {nextFileDate = value;}
        }
        private string nextFile = null;
        /// <summary>
        /// The next file.
        /// </summary>
        public string NextFile {
            get {return nextFile;}
            set {nextFile = value;}
        }

        /// <summary>
        /// Close the cvs server connection.
        /// </summary>
        public void Close() {
            this.protocol.Disconnect();
            if (StopProcessEvent != null) {
                this.StopProcessEvent(this, new ProcessEventArgs());
            }
        }
    }
}
