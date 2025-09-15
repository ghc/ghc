{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wno-tabs #-}

import Text.Printf (printf)

{-
Test the MultilineStrings proposal
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst
-}

main :: IO ()
main = do
  putStrLn "-- 1"
  prints example_1
  putStrLn "\n-- 2"
  prints example_2a
  prints example_2b
  prints example_2c
  prints example_2d
  putStrLn "\n-- 3"
  prints example_3
  putStrLn "\n-- 4"
  prints example_4
  putStrLn "\n-- 5"
  prints example_5
  putStrLn "\n-- 6"
  prints example_6a
  prints example_6b
  putStrLn "\n-- 7"
  prints example_7a
  prints example_7b_1
  prints example_7b_2
  putStrLn "\n-- 8"
  prints example_8
  putStrLn "\n-- 9"
  prints example_9
  putStrLn "\n-- 10"
  prints example_10a
  prints example_10b
  putStrLn "\n-- 11"
  prints example_11

  putStrLn "\n-- extra"
  prints """"""
  prints
    """
    """
  prints
    """
    a"""
  prints
    """a
    """
  prints
    """
    \n
    """
  prints
    """
    \\n
    """
  prints
    """
    a
        
      b
        """
  where
    prints :: String -> IO ()
    prints = print

example_1 =
      """
      abc

      def
  
    ghi
        \njkl
   """

example_2a =
  """Line 1
     Line 2
  Line 3
  """

example_2b =
  """\
 \Line 1
     Line 2
  Line 3
  """

example_2c = """hello world"""

example_2d =
  """    hello
  world
  """

example_3 =
    """
      a b\
  \ c d e
      f g
    """

example_4 =
	"""
	        a
	 	b
	    	c
	"""

example_5 =
  """

  a
  b
  c
  """

example_6a =
  """
  a
  b
  c"""

example_6b =
  """
  a
  b
  c\
  \"""

example_7a =
  """
    a
    b
    c
  """

example_7b_1 =
  """
  \&  a
    b
    c
  """

example_7b_2 =
  """
  \&  a
  \&  b
  \&  c
  """

example_8 =
  """
  This is a literal multiline string:
  \"\"\"
  Hello
    world!
  \"""
  """

example_9 =
  """
   name\tage
   Alice\t20
   Bob\t30
  \t40
  """

example_10a =
  """
  \\v -> case v of
    Aeson.Null -> pure PrintStyleInherit
    Aeson.String "" -> pure PrintStyleInherit
    _ -> PrintStyleOverride <$> Aeson.parseJSON v
  """

example_10b =
  """
  \\s -> case s of
    "" -> pure PrintStyleInherit
    _ -> PrintStyleOverride <$> parsePrinterOptType s
  """

example_11 =
  printf
    """
    instance Aeson.FromJSON %s where
      parseJSON =
        Aeson.withText "%s" $ \\s ->
          either Aeson.parseFail pure $
            parsePrinterOptType (Text.unpack s)

    instance PrinterOptsFieldType %s where
      parsePrinterOptType s =
        case s of
    %s
          _ ->
            Left . unlines $
              [ "unknown value: " <> show s
              , "Valid values are: %s"
              ]
    """
    fieldTypeName
    fieldTypeName
    fieldTypeName
    ( unlines
        [ printf "      \"%s\" -> Right %s" val con
        | (con, val) <- enumOptions
        ]
    )
    (unwords $ map snd enumOptions)
  where
    fieldTypeName = "MyEnum"
    enumOptions = [("Foo", "foo"), ("BarBaz", "bar-baz")]
