
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.TablesNew where

import Import

import Yesod.Form.Bootstrap3
import Yesod.Core.Handler 
import Data.ByteString.Char8 as B (pack,unpack)

itemNumberForm =  areq intField (bfs ("Number of items" :: Text)) Nothing


getTablesNewR :: Handler Html
getTablesNewR = do 
    (widget, enctype) <- generateFormPost . renderBootstrap3 BootstrapBasicForm $ itemNumberForm
    defaultLayout $ $(widgetFile "tablesnew")

postTablesNewR :: Handler Html 
postTablesNewR = do 
    text <- getPostParams
    maybeCurrentUserId <- maybeAuthId
    let ptext = process text 
    
    str <- getTables
    length str `seq` return ()
    case maybeCurrentUserId of 
        (Just uid) -> 
            appendTables ptext uid >> defaultLayout [whamlet| #{ptext} |]

  where 
    process text = Import.unpack . concat . intersperse delimiter $ map snd text 
    delimiter = "\n"
    logfile = "logfile.txt" 

    getTables :: (MonadIO f) => f [Char]
    getTables =  do 
        B.unpack <$> readFile logfile
    
    appendTables :: (MonadIO m, Show a, Show b) => a -> b -> m ()
    appendTables ptext uid = do 
        str <- getTables 
        writeFile logfile $ B.pack $ str <> (show $ (ptext, uid))

