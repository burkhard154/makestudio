using System;
using System.Runtime.InteropServices; // COM attributes

namespace csclient
{
	/// <summary>
	/// Zusammenfassung f�r Class1.
	/// </summary>
	[ComVisible( true)]
	public class Class1
	{
		public Class1()
		{
			//
			// TODO: F�gen Sie hier die Konstruktorlogik hinzu
			//
		}

		public string TestCall( string S)
		{
			return S + " Test";
		}
	}

}
