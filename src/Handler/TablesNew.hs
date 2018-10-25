
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.TablesNew where

import Import

import Yesod.Form.Bootstrap3

itemNumberForm =  areq intField (bfs ("Number of items" :: Text)) Nothing


getTablesNewR :: Handler Html
getTablesNewR = do 
    (widget, enctype) <- generateFormPost . renderBootstrap3 BootstrapBasicForm $ itemNumberForm
    defaultLayout $ $(widgetFile "tablesnew")

postTablesNewR :: Handler Html 
postTablesNewR = do 
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm itemNumberForm
    maybeCurrentUserId <- maybeAuthId
    case res of 
        FormSuccess itemnum -> do 
            redirect $ TablesEditR itemnum
        _ -> defaultLayout $(widgetFile "tablesnew")
