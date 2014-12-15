// Copyright (c) 2014, Fanael Linithien
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//   * Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//   * Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

internal interface ICommand
{
    void Run(string[] args);
}

internal class Program
{
    private static int Main(string[] args)
    {
        try
        {
            Console.OutputEncoding = Encoding.UTF8;
            ICommand command = (ICommand)Activator.CreateInstance(null, args[0]).Unwrap();
            command.Run(args);
            return 0;
        }
        catch (Exception e)
        {
            Console.Error.WriteLine(e);
            return 255;
        }
    }
}

internal class ItemLister : ICommand
{
    public void Run(string[] args)
    {
        List<string> shortcuts = new List<string>(100);
        foreach (string path in GetStartMenuPaths())
        {
            shortcuts.AddRange(Directory.GetFiles(path, "*.lnk", SearchOption.AllDirectories));
        }

        Console.Write(LispPrinter.PrintStartMenuEntries(shortcuts));
    }

    private static string[] GetStartMenuPaths()
    {
        return new string[]
        {
            Environment.GetFolderPath(Environment.SpecialFolder.StartMenu),
            GetCommonStartMenu()
        };
    }

    private static string GetCommonStartMenu()
    {
        const int CSIDL_COMMON_STARTMENU = 0x16;
        const int MAX_PATH = 260;
        StringBuilder result = new StringBuilder(MAX_PATH);
        int ok = SHGetFolderPath(IntPtr.Zero, CSIDL_COMMON_STARTMENU, IntPtr.Zero, 0, result);
        if (ok != 0)
        {
            throw new ExternalException("Failed to get common start menu path", ok);
        }

        return result.ToString();
    }

    [DllImport("shell32.dll")]
    private static extern int SHGetFolderPath(
        IntPtr owner,
        int folder,
        IntPtr token,
        uint flags,
        StringBuilder path);
}

internal abstract class LispPrinter
{
    private LispPrinter()
    {
    }

    public static string PrintStartMenuEntries(IEnumerable<string> entryPaths)
    {
        Impl impl = new Impl();
        impl.PrintStartMenuEntries(entryPaths);
        return impl.Result;
    }

    private class Impl
    {
        private StringBuilder result = new StringBuilder(65536);

        public Impl()
        {
        }

        public string Result
        {
            get { return this.result.ToString(); }
        }

        public void PrintStartMenuEntries(IEnumerable<string> entryPaths)
        {
            this.result.Append('(');
            foreach (string entryPath in entryPaths)
            {
                this.result.Append('(');
                this.PrintString(Path.GetFileNameWithoutExtension(entryPath));
                this.result.Append(" . ");
                this.PrintString(entryPath);
                this.result.Append(')');
            }

            this.result.Append(')');
        }

        private void PrintString(string str)
        {
            this.result.Append('"');
            this.result.Append(str.Replace(@"\", @"\\").Replace("\"", "\\\""));
            this.result.Append('"');
        }
    }
}