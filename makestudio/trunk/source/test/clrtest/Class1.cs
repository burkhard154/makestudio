using System;
using System.Runtime.InteropServices;

namespace dotnetclient.cs
{
	/// <summary>
	/// Zusammenfassende Beschreibung f�r Class1.
	/// </summary>
	[ComVisible( true)]
	public class Class1
	{
		public string TestCall( string S)
		{
		  return S + " - .NET object answers!";
		}

	}
}
