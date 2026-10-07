-- | Tests for Doxyfile generation ('generateConfig').
module Test.Doxygen.Parser.Config (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

import Doxygen.Parser.Internal (Config (..), defaultConfig, generateConfig)

tests :: [TestTree]
tests =
  [ testCase "no aliases emits no ALIASES line" $
      aliasLines defaultConfig @?= []
  , testCase "each alias appends one quoted key=value pair" $ do
      let config =
            defaultConfig
              { aliases =
                  [ ("threadsafety", "\\par Thread safety:^^")
                  , ("sdlversion", "3.2.0")
                  , ("quotation", "This is an inner \"quotation\"!")
                  , ("newline", "This is a\nmultiline entry.")
                  , ("empty", "")
                  , ("crlf", "CR\rLF\nCRLF\r\nEND")
                  , ("math", "2+2-1=3")
                  , ("carets", "look up ^^")
                  , ("numeric_alias_123", "val")
                  ]
              }
      aliasLines config
        @?= [ "ALIASES += threadsafety=\"\\par Thread safety:^^\""
            , "ALIASES += sdlversion=\"3.2.0\""
            , "ALIASES += quotation=\"This is an inner \\\"quotation\\\"!\""
            , "ALIASES += newline=\"This is a^^multiline entry.\""
            , "ALIASES += empty=\"\""
            , "ALIASES += crlf=\"CR^^LF^^CRLF^^END\""
            , "ALIASES += math=\"2+2-1=3\""
            , "ALIASES += carets=\"look up ^^\""
            , "ALIASES += numeric_alias_123=\"val\""
            ]
  ]

-- | The @ALIASES@ lines of the generated Doxyfile.
aliasLines :: Config -> [Text]
aliasLines config =
  filter ("ALIASES" `Text.isPrefixOf`) $
    Text.lines (generateConfig config ("sdl.h" :| []) "/out")
