{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.TablesList where

import Import


getTablesListR :: Handler Html
getTablesListR = do
    defaultLayout $ do
        setTitle . toHtml $ ("tables" :: Text)
        $(widgetFile "tables")
