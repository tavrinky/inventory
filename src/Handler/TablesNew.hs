
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.TablesNew where

import Import
import Utils

import Yesod.Core.Handler 
import Data.ByteString.Char8 as B (pack,unpack)


getTablesNewR :: Handler Html
getTablesNewR = do 
    defaultLayout $ $(widgetFile "tablesnew")

postTablesNewR :: Handler Html 
postTablesNewR = do 
    text <- getPostParams
    maybeCurrentUserId <- maybeAuthId
    let ptext = process text 
    
    case maybeCurrentUserId of 
        (Just uid) -> 
            appendTables (stringify ptext uid) >> defaultLayout [whamlet| foo |]

  where 
   
    stringify a b = (<>) <$> getTables <*> pure (show (a,b))

     process text = Import.unpack . concat $ map (munge . snd) text 