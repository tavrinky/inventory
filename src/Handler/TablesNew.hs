
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

itemNumberForm =  areq intField (bfs ("Number of items" :: Text)) Nothing


getTablesNewR :: Handler Html
getTablesNewR = do 
    (widget, enctype) <- generateFormPost . renderBootstrap3 BootstrapBasicForm $ itemNumberForm
    defaultLayout $ $(widgetFile "tablesnew")

postTablesNewR :: Handler Html 
postTablesNewR = do 
    text <- getPostParams
    maybeCurrentUserId <- maybeAuthId
    let res = concatMap (uncurry (<>)) text  
    defaultLayout [whamlet| #{res} |]
