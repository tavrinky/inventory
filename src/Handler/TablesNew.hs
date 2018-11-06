
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

import Data.Maybe



getTablesNewR :: Handler Html
getTablesNewR = do 
    defaultLayout $ $(widgetFile "tablesnew")

postTablesNewR :: Handler Html 
postTablesNewR = do 
    text <- getPostParams
    maybeCurrentUserId <- maybeAuthId
    let (header, ptext) = process text 
    
    case maybeCurrentUserId of 
        (Just uid) -> do 
            tableId <- runDB . insert $ Table header ptext uid 
            redirect $ TablesEditR tableId  


  where 
   
    stringify a b = (<>) <$> getTables <*> pure (show (a,b))


    process text = (title, (intercalate delimiter . concat . map (uncurry (:) . concatColumns) . toColumns$ rest)) 
      where 
        title = fromJust . lookup "title" $ text 
        rest =  filter ((/= "title") . fst) text 

    toColumns :: [(Text,Text)] -> [[(Text, Text)]]
    toColumns = groupBy ((==) `on` fst) 

    concatColumns :: [(Text, Text)] -> (Text, [Text])
    concatColumns xs = (fst . Import.unsafeHead $ xs, map snd xs)




