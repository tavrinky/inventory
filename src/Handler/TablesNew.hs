
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
        (Just uid) -> do 
            tableId <- runDB . insert $ Table ptext (Just uid) 
            redirect $ TablesEditR tableId  


  where 
   
    stringify a b = (<>) <$> getTables <*> pure (show (a,b))

    process text = intercalate delimiter $ map snd text 


