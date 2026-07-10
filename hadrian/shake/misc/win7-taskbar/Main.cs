using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[assembly: AssemblyTitle("TaskbarProgress")]
[assembly: AssemblyDescription("Show progress values on the Win7 taskbar")]
[assembly: AssemblyCompany("Neil Mitchell")]
[assembly: AssemblyProduct("TaskbarProgress")]
[assembly: AssemblyCopyright("Copyright © Neil Mitchell 2013")]

// Setting ComVisible to false makes the types in this assembly not visible
// to COM components.  If you need to access a type in this assembly from
// COM, set the ComVisible attribute to true on that type.
[assembly: ComVisible(false)]


namespace Taskbar
{
    class Args
    {
        public readonly string Title = "";
        public readonly TaskbarProgressBarStatus? State;
        public readonly int? Value;

        public Args(string[] xs)
        {
            var bad = new List<string>();
            var help = xs.Length == 0;
            foreach (var x in xs)
            {
                var pos = x.IndexOf('=');
                var flag = pos == -1 ? x : x.Substring(0, pos);
                var arg = pos == -1 ? "" : x.Substring(pos + 1);
                switch (flag)
                {
                    case "--title":
                        Title = arg;
                        break;
                    case "--state":
                        State = ToState(arg);
                        if (!State.HasValue) bad.Add(x);
                        break;
                    case "--value":
                        Value = ToValue(arg);
                        if (!Value.HasValue) bad.Add(x);
                        break;
                    case "--help":
                    case "-h":
                    case "/?":
                        help = true;
                        break;
                    default:
                        bad.Add(x);
                        break;
                }
            }
            foreach (var s in bad)
                Console.WriteLine("Bad argument: " + s);
            if (bad.Count > 0 || help)
            {
                Console.WriteLine("Windows 7 Taskbar progress changer (part of the Shake build system)");
                Console.WriteLine("");
                Console.WriteLine("  --title=STR    Name of the titlebar to modify, defaults to current console");
                Console.WriteLine("  --state=STATE  One of NoProgress, Normal, Error, Indeterminate or Paused");
                Console.WriteLine("  --value=INT    Percentage complete, between 0 and 100");
                Environment.Exit(bad.Count > 0 ? 1 : 0);
            }
        }

        private static TaskbarProgressBarStatus? ToState(string s)
        {
            try
            {
                return (TaskbarProgressBarStatus)Enum.Parse(typeof(TaskbarProgressBarStatus), s);
            }
            catch (Exception)
            {
                return null;
            }
        }

        private static int? ToValue(string s)
        {
            try
            {
                var res = Convert.ToInt32(s);
                if (res < 0 || res > 100)
                    return null;
                else
                    return res;
            }
            catch (Exception)
            {
                return null;
            }
        }
    }

    class Program
    {
        [DllImport("kernel32.dll")]
        private static extern IntPtr GetConsoleWindow();

        [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        private static extern int GetWindowText(IntPtr hWnd, StringBuilder lpString, int nMaxCount);

        // Return the list of windows I should manipulate
        // If I have no title to match on, use my console
        // If I have a console with the right title, use that
        // Otherwise, use all windows with the right title
        private static IntPtr[] FindWindow(string Title)
        {
            IntPtr hConsole = GetConsoleWindow();
            if (Title == "") return hConsole == IntPtr.Zero ? new IntPtr[]{} : new IntPtr[]{hConsole};

            var sb = new StringBuilder(Title.Length + 1);
            var len = hConsole == IntPtr.Zero ? 0 : GetWindowText(hConsole, sb, sb.Capacity);
            if (len == Title.Length && sb.ToString() == Title) return new IntPtr[]{hConsole};

            // Our console is not the right window, go hunting for a suitable window
            var res = new List<IntPtr>();
            foreach (var p in Process.GetProcesses())
            {
                if (p.MainWindowTitle == Title)
                    res.Add(p.MainWindowHandle);
            }
            return res.ToArray();
        }

        static void Main(string[] args)
        {
            var opts = new Args(args);
            var wnds = FindWindow(opts.Title);
            if (wnds.Length == 0) Environment.Exit(2);

            ITaskbarList4 inst = null;
            try
            {
                inst = (ITaskbarList4)new CTaskbarList();
                inst.HrInit();
            }
            catch (Exception e)
            {
                Console.WriteLine("Failed to create ITaskbarList4 interface, usually because you are not on Windows 7.");
                Console.WriteLine("");
                Console.WriteLine(e.Message);
                Environment.Exit(3);
            }

            foreach (var wnd in wnds)
            {
                if (opts.Value.HasValue)
                    inst.SetProgressValue(wnd, (ulong) opts.Value.Value, 100);
                if (opts.State.HasValue)
                    inst.SetProgressState(wnd, opts.State.Value);
            }
        }
    }
}
