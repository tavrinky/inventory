{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.TablesList where

import Import


getTablesListR :: Handler Html
getTablesListR =  do 
    allDecks <- runDB $ selectList [] [Desc TableId]
    
    defaultLayout $ do
        $(widgetFile "tables")
