{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Messages.Internal (messages) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Lib (trim)

parse :: String -> [(String, String)]
parse = map toT . filter (((/=) "" . trim)) . lines
  where
    toT :: String -> (String, String)
    toT l = case words l of
        k : v : _ -> (trim k, trim v)
        [k] -> (trim k, "malformed format")
        [] -> ("malformedFormat", "malformed format")

messages :: QuasiQuoter
messages =
    QuasiQuoter
        { quoteExp = undefined
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = messageFunctionDecs
        }

messageFunctionDecs :: String -> Q [Dec]
messageFunctionDecs =
    return . concatMap tToDec . parse
  where
    tToDec :: (String, String) -> [Dec]
    tToDec (k, v) =
        [ SigD
            (mkName k)
            (ConT ''String)
        , FunD
            (mkName k)
            [ Clause
                []
                (NormalB $ LitE $ StringL v)
                []
            ]
        ]
