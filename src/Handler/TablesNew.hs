
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
import Data.Serialize

getTablesNewR :: Handler Html
getTablesNewR = do 
    defaultLayout $ $(widgetFile "tablesnew")

postTablesNewR :: Handler Html 
postTablesNewR = do 
    text <- getPostParams
    maybeCurrentUserId <- maybeAuthId
    let (title, (terms:defs:_)) = process text  
    let bytestrings = map (encodeUtf8 *** encodeUtf8) $ zip terms defs  
    case maybeCurrentUserId of 
        (Just uid) -> do 
            let cards = map (\(term,def) -> Card term def 0 0) bytestrings 
            let deck = Deck title (runPut $ put cards) uid  
            deckId <- runDB $ insert deck 
            --redirect $ TablesEditR tableId  
            redirect HomeR

  where 
   
    stringify a b = (<>) <$> getTables <*> pure (show (a,b))

    


    

    process text =  (title, concatColumns <$> (toColumns rest))  
      where 
        title = fromJust . lookup "title" $ text 
        rest =  filter ((/= "title") . fst) text 

    toColumns :: [(Text,Text)] -> [[(Text, Text)]]
    toColumns = groupBy ((==) `on` fst) 

    concatColumns :: [(Text, Text)] -> [Text]
    concatColumns xs = map snd xs




