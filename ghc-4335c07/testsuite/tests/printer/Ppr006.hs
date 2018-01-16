{-# LANGUAGE QuasiQuotes #-}
module Ppr006 where

commands :: [Command]
commands = [
    command "help" "display a list of all commands, and their current keybindings" $ do
      macroGuesses <- Macro.guessCommands commandNames <$> getMacros
      addTab (Other "Help") (makeHelpWidget commands macroGuesses) AutoClose

  , command "log" "show the error log" $ do
      messages <- gets logMessages
      let widget = ListWidget.moveLast (ListWidget.new $ reverse messages)
      addTab (Other "Log") (AnyWidget . LogWidget $ widget) AutoClose

  , command "map" "display a list of all commands that are currently bound to keys" $ do
      showMappings

  , command "map" "display the command that is currently bound to the key {name}" $ do
      showMapping

  , command "map" [help|
        Bind the command {expansion} to the key {name}.  The same command may
        be bound to different keys.
        |] $ do
      addMapping

  , command "unmap" "remove the binding currently bound to the key {name}" $ do
      \(MacroName m) -> removeMacro m

  , command "mapclear" "" $ do
      clearMacros

  , command "exit" "exit vimus" $ do
      eval "quit"

  , command "quit" "exit vimus" $ do
      liftIO exitSuccess :: Vimus ()

  , command "close" "close the current window (not all windows can be closed)" $ do
      void closeTab

  , command "source" "read the file {path} and interprets all lines found there as if they were entered as commands." $ do
      \(Path p) -> liftIO (expandHome p) >>= either printError source_

  , command "runtime" "" $
      \(Path p) -> liftIO (getDataFileName p) >>= source_

  , command "color" "define the fore- and background color for a thing on the screen." $ do
      \color fg bg -> liftIO (defineColor color fg bg) :: Vimus ()

  , command "repeat" "set the playlist option *repeat*. When *repeat* is set, the playlist will start over when the last song has finished playing." $ do
      MPD.repeat  True :: Vimus ()

  , command "norepeat" "Unset the playlist option *repeat*." $ do
      MPD.repeat  False :: Vimus ()

  , command "consume" "set the playlist option *consume*. When *consume* is set, songs that have finished playing are automatically removed from the playlist." $ do
      MPD.consume True :: Vimus ()

  , command "noconsume" "Unset the playlist option *consume*." $ do
      MPD.consume False :: Vimus ()

  , command "random" "set the playlist option *random*. When *random* is set, songs in the playlist are played in random order." $ do
      MPD.random  True :: Vimus ()

  , command "norandom" "Unset the playlist option *random*." $ do
      MPD.random  False :: Vimus ()

  , command "single" "Set the playlist option *single*. When *single* is set, playback does not advance automatically to the next item in the playlist. Combine with *repeat* to repeatedly play the same song." $ do
      MPD.single  True :: Vimus ()

  , command "nosingle" "Unset the playlist option *single*." $ do
      MPD.single  False :: Vimus ()

  , command "autotitle" "Set the *autotitle* option.  When *autotitle* is set, the console window title is automatically set to the currently playing song." $ do
      setAutoTitle True

  , command "noautotitle" "Unset the *autotitle* option." $ do
      setAutoTitle False

  , command "volume" "[+-] set volume to  or adjust by [+-] num" $ do
      volume :: Volume -> Vimus ()

 , command "toggle-repeat" "Toggle the *repeat* option." $ do
      MPD.status >>= MPD.repeat  . not . MPD.stRepeat :: Vimus ()

  , command "toggle-consume" "Toggle the *consume* option." $ do
      MPD.status >>= MPD.consume . not . MPD.stConsume :: Vimus ()

  , command "toggle-random" "Toggle the *random* option." $ do
      MPD.status >>= MPD.random  . not . MPD.stRandom :: Vimus ()

  , command "toggle-single" "Toggle the *single* option." $ do
      MPD.status >>= MPD.single  . not . MPD.stSingle :: Vimus ()

  , command "set-library-path" "While MPD knows where your songs are stored, vimus doesn't. If you want to use the *%* feature of the command :! you need to tell vimus where your songs are stored." $ do
      \(Path p) -> setLibraryPath p

  , command "next" "stop playing the current song, and starts the next one" $ do
      MPD.next :: Vimus ()

  , command "previous" "stop playing the current song, and starts the previous one" $ do
      MPD.previous :: Vimus ()

  , command "toggle" "toggle between play and pause" $ do
      MPDE.toggle :: Vimus ()

  , command "stop" "stop playback" $ do
      MPD.stop :: Vimus ()

  , command "update" "tell MPD to update the music database. You must update your database when you add or delete files in your music directory, or when you edit the metadata of a song.  MPD will only rescan a file already in the database if its modification time has changed." $ do
      void (MPD.update Nothing) :: Vimus ()

  , command "rescan" "" $ do
      void (MPD.rescan Nothing) :: Vimus ()

  , command "clear" "delete all songs from the playlist" $ do
      MPD.clear :: Vimus ()

  , command "search-next" "jump to the next occurrence of the search string in the current window"
      searchNext

  , command "search-prev" "jump to the previous occurrence of the search string in the current window"
      searchPrev


  , command "window-library" "open the *Library* window" $
      selectTab Library

  , command "window-playlist" "open the *Playlist* window" $
      selectTab Playlist

  , command "window-search" "open the *SearchResult* window" $
      selectTab SearchResult

  , command "window-browser" "open the *Browser* window" $
      selectTab Browser

  , command "window-next" "open the window to the right of the current one"
      nextTab

  , command "window-prev" "open the window to the left of the current one"
      previousTab

  , command "!" "execute {cmd} on the system shell. See chapter \"Using an external tag editor\" for an example."
      runShellCommand

  , command "seek" "jump to the given position in the current song"
      seek

  , command "visual" "start visual selection" $
      sendEventCurrent EvVisual

  , command "novisual" "cancel visual selection" $
      sendEventCurrent EvNoVisual

  -- Remove current song from playlist
  , command "remove" "remove the song under the cursor from the playlist" $
      sendEventCurrent EvRemove

  , command "paste" "add the last deleted song after the selected song in the playlist" $
      sendEventCurrent EvPaste

  , command "paste-prev" "" $
      sendEventCurrent EvPastePrevious

  , command "copy" "" $
      sendEventCurrent EvCopy

  , command "shuffle" "shuffle the current playlist" $ do
      MPD.shuffle Nothing :: Vimus ()

  , command "add" "append selected songs to the end of the playlist" $ do
      sendEventCurrent EvAdd

  -- insert a song right after the current song
  , command "insert" [help|
      inserts a song to the playlist. The song is inserted after the currently
      playing song.
      |] $ do
      st <- MPD.status
      case MPD.stSongPos st of
        Just n -> do
          -- there is a current song, insert after
          sendEventCurrent (EvInsert (n + 1))
        _ -> do
          -- there is no current song, just add
          sendEventCurrent EvAdd

  -- Playlist: play selected song
  -- Library:  add song to playlist and play it
  -- Browse:   either add song to playlist and play it, or :move-in
  , command "default-action" [help|
      depending on the item under the cursor, something different happens:

      - *Playlist* start playing the song under the cursor

      - *Library* append the song under the cursor to the playlist and start playing it

      - *Browser* on a song: append the song to the playlist and play it. On a directory: go down to that directory.
      |] $ do
      sendEventCurrent EvDefaultAction

  , command "add-album" "add all songs of the album of the selected song to the playlist" $ do
      songs <- fromCurrent MPD.Album [MPD.Disc, MPD.Track]
      maybe (printError "Song has no album metadata!") (MPDE.addMany "" . map MPD.sgFilePath) songs

  , command "add-artist" "add all songs of the artist of the selected song to the playlist" $ do
      songs <- fromCurrent MPD.Artist [MPD.Date, MPD.Album, MPD.Disc, MPD.Track]
      maybe (printError "Song has no artist metadata!") (MPDE.addMany "" . map MPD.sgFilePath) songs

  -- movement
  , command "move-up" "move the cursor one line up" $
      sendEventCurrent EvMoveUp

  , command "move-down" "move the cursor one line down" $
      sendEventCurrent EvMoveDown

  , command "move-album-prev" "move the cursor up to the first song of an album" $
      sendEventCurrent EvMoveAlbumPrev

  , command "move-album-next" "move the cursor down to the first song of an album" $
      sendEventCurrent EvMoveAlbumNext

  , command "move-in" "go down one level the directory hierarchy in the *Browser* window" $
      sendEventCurrent EvMoveIn

  , command "move-out" "go up one level in the directory hierarchy in the *Browser* window" $
      sendEventCurrent EvMoveOut

  , command "move-first" "go to the first line in the current window" $
      sendEventCurrent EvMoveFirst

  , command "move-last" "go to the last line in the current window" $
      sendEventCurrent EvMoveLast

  , command "scroll-up" "scroll the contents of the current window up one line" $
      sendEventCurrent (EvScroll (-1))

  , command "scroll-down" "scroll the contents of the current window down one line" $
      sendEventCurrent (EvScroll 1)

  , command "scroll-page-up" "scroll the contents of the current window up one page" $
      pageScroll >>= sendEventCurrent . EvScroll . negate

  , command "scroll-half-page-up" "scroll the contents of the current window up one half page" $
      pageScroll >>= sendEventCurrent . EvScroll . negate . (`div` 2)

  , command "scroll-page-down" "scroll the contents of the current window down one page" $
      pageScroll >>= sendEventCurrent . EvScroll

  , command "scroll-half-page-down" "scroll the contents of the current window down one half page" $
      pageScroll >>= sendEventCurrent . EvScroll . (`div` 2)

  , command "song-format" "set song rendering format" $
      sendEvent . EvChangeSongFormat
  ]
