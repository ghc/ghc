module GHC.Linker.Windows
   ( ManifestOpts (..)
   , initManifestOpts
   , maybeCreateManifest
   )
where

import GHC.Prelude
import GHC.SysTools
import GHC.Driver.Session
import GHC.Utils.TmpFs
import GHC.Utils.Logger

import System.FilePath
import System.Directory

data ManifestOpts = ManifestOpts
  { manifestEmbed :: !Bool    -- ^ Should the manifest be embedded in the binary with Windres
  , manifestTempdir :: TempDir
  , manifestWindresConfig :: WindresConfig
  , manifestObjectSuf :: String
  }

initManifestOpts :: DynFlags -> ManifestOpts
initManifestOpts dflags = ManifestOpts
  { manifestEmbed = gopt Opt_EmbedManifest dflags
  , manifestTempdir = tmpDir dflags
  , manifestWindresConfig = configureWindres dflags
  , manifestObjectSuf = objectSuf dflags
  }

maybeCreateManifest
   :: Logger
   -> TmpFs
   -> ManifestOpts
   -> FilePath      -- ^ filename of executable
   -> IO [FilePath] -- ^ extra objects to embed, maybe
maybeCreateManifest logger tmpfs opts exe_filename = do
   let manifest_filename = exe_filename <.> "manifest"
       manifest =
         "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n\
         \  <assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">\n\
         \  <assemblyIdentity version=\"1.0.0.0\"\n\
         \     processorArchitecture=\"X86\"\n\
         \     name=\"" ++ dropExtension exe_filename ++ "\"\n\
         \     type=\"win32\"/>\n\n\
         \  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">\n\
         \    <security>\n\
         \      <requestedPrivileges>\n\
         \        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>\n\
         \        </requestedPrivileges>\n\
         \       </security>\n\
         \  </trustInfo>\n\
         \</assembly>\n"

   writeFile manifest_filename manifest

   -- Windows will find the manifest file if it is named
   -- foo.exe.manifest. However, for extra robustness, and so that
   -- we can move the binary around, we can embed the manifest in
   -- the binary itself using windres:
   if not (manifestEmbed opts)
      then return []
      else do
         rc_filename <- newTempName logger tmpfs (manifestTempdir opts) TFL_CurrentModule "rc"
         rc_obj_filename <-
           newTempName logger tmpfs (manifestTempdir opts) TFL_GhcSession (manifestObjectSuf opts)

         writeFile rc_filename $
             "1 24 MOVEABLE PURE \"" ++ manifest_filename ++ "\"\n"
               -- magic numbers :-)

         runWindres logger (manifestWindresConfig opts) $ map GHC.SysTools.Option $
               ["--input="++rc_filename,
                "--output="++rc_obj_filename,
                "--output-format=coff"]
               -- no FileOptions here: windres doesn't like seeing
               -- backslashes, apparently

         removeFile manifest_filename

         return [rc_obj_filename]
