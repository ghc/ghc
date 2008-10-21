
[Setup]
AppName=GHC
AppVerName=GHC @VERSION@
DefaultDirName={sd}\ghc\ghc-@VERSION@
UsePreviousAppDir=no
DefaultGroupName=GHC
UninstallDisplayIcon={app}\bin\ghci.exe
Compression=lzma
SolidCompression=yes
PrivilegesRequired=none
ChangesAssociations=yes
ChangesEnvironment=yes


[Files]
Source: "ghc-@VERSION@\*"; DestDir: "{app}"; Flags: recursesubdirs

[Icons]
Name: "{group}\@VERSION@\GHCi"; Filename: "{app}\bin\ghci.exe"
Name: "{group}\@VERSION@\GHC Documentation"; Filename: "{app}\doc\index.html"
Name: "{group}\@VERSION@\GHC Library Documentation"; Filename: "{app}\doc\libraries\index.html"
Name: "{group}\@VERSION@\GHC Flag Reference"; Filename: "{app}\doc\users_guide\flag-reference.html"

[Registry]
; set up icon associations
; this does _not_ follow the "play nice" proposal
; future version should
Root: HKCR; Subkey: ".hs"; ValueType: string; ValueName: ""; ValueData: "ghc_haskell"; Flags: uninsdeletevalue
Root: HKCR; Subkey: ".lhs"; ValueType: string; ValueName: ""; ValueData: "ghc_haskell"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "ghc_haskell"; ValueType: string; ValueName: ""; ValueData: "Haskell Source File"; Flags: uninsdeletekey
Root: HKCR; Subkey: "ghc_haskell\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\icons\hsicon.ico"
Root: HKCR; Subkey: "ghc_haskell\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\ghci.exe"" ""%1"""

; these flags were always set in the past, by the installer
; some programs may rely on them to find GHC
Root: HKCU; Subkey: "Software\Haskell\GHC\ghc-@VERSION@"; ValueType: string; ValueName: "InstallDir"; ValueData: "{app}"; Flags: uninsdeletekey
Root: HKCU; Subkey: "Software\Haskell\GHC"; ValueType: string; ValueName: "InstallDir"; ValueData: "{app}"; Flags: uninsdeletevalue

; set the PATH variable, for both GHC and Cabal
Root: HKCU; Subkey: "Environment"; ValueName: "Path"; ValueType: "string"; ValueData: "{app}\bin;{olddata}";  Check: NotOnPathAlready('{app}\bin'); Flags: preservestringtype
Root: HKCU; Subkey: "Environment"; ValueName: "Path"; ValueType: "string"; ValueData: "{pf}\Haskell\bin;{olddata}";  Check: NotOnPathAlready('{pf}\Haskell\bin'); Flags: preservestringtype


; stolen from Gtk2Hs, I'm sure they like us :-)
; @dcoutts++
[Code]

function NotOnPathAlready(NewValue : String): Boolean;
var
  Path: String;
begin
  // Log('Checking if Gtk2Hs\bin dir is already on the %PATH%');
  if RegQueryStringValue(HKEY_CURRENT_USER, 'Environment', 'Path', Path) then
  begin // Successfully read the value
    // Log('HKCU\Environment\PATH = ' + Path);
    NewValue := ExpandConstant(NewValue);
    // Log('Looking for Gtk2Hs\bin dir in %PATH%: ' + BinDir + ' in ' + Path);
    if Pos(LowerCase(NewValue), Lowercase(Path)) = 0 then
    begin
      // Log('Did not find Gtk2Hs\bin dir in %PATH% so will add it');
      Result := True;
    end
    else
    begin
      // Log('Found Gtk2Hs bin dir in %PATH% so will not add it again');
      Result := False;
    end
  end
  else // The key probably doesn't exist
  begin
    // Log('Could not access HKCU\Environment\PATH so assume it is ok to add it');
    Result := True;
  end;
end;

