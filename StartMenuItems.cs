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
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using System.Collections.Generic;

class StartMenuItems
{
    static void Main()
    {
        string[] paths = new string[]{Environment.GetFolderPath(Environment.SpecialFolder.StartMenu),
                                      GetCommonStartMenu()};
        List<string> shortcuts = new List<string>(100);
        foreach (string path in paths)
            shortcuts.AddRange(Directory.GetFiles(path, "*.lnk", SearchOption.AllDirectories));
        StringBuilder result = new StringBuilder(65536);
        PrintReadably(result, shortcuts);
        Console.Write(result);
    }

    private static void PrintReadably(StringBuilder result, IEnumerable<String> shortcutPaths)
    {
        result.Append('(');
        foreach (string shortcutPath in shortcutPaths)
        {
            result.Append('(');
            PrintReadably(result, Path.GetFileNameWithoutExtension(shortcutPath));
            result.Append(" . ");
            PrintReadably(result, shortcutPath);
            result.Append(')');
        }
        result.Append(')');
    }

    private static void PrintReadably(StringBuilder result, string str)
    {
        result.Append('"');
        result.Append(str.Replace(@"\", @"\\").Replace("\"", "\\\""));
        result.Append('"');
    }

    private static string GetCommonStartMenu()
    {
        StringBuilder sb = new StringBuilder(260);
        const int CSIDL_COMMON_STARTMENU = 0x16;
        SHGetFolderPath(IntPtr.Zero, CSIDL_COMMON_STARTMENU, IntPtr.Zero, 0, sb);
        return sb.ToString();
    }

    [DllImport("shell32.dll")]
    private static extern int SHGetFolderPath(IntPtr hwndOwner, int nFolder, IntPtr hToken,
                                              uint dwFlags, StringBuilder pszPath);
}