{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.DeckStudy where

import Import
import Utils 
import Data.Maybe

getDeckStudyR :: DeckId -> Handler Html
getDeckStudyR deckId = do 
    
  (Deck title contents _) <- runDB $ get404 deckId 
  let cards = getCards contents 
  let cols = map (\(Card term def _ _) -> (decodeUtf8 term, decodeUtf8 def)) cards
  --_ <- runDB $ deleteWhere [TableTablecontents !=. ""]
  defaultLayout $ do
      $(widgetFile "deckstudy")



postDeckStudyR :: DeckId -> Handler Html
postDeckStudyR deckId = do 
  preCards <- getPostParams >>= (return . fromJust . lookup "hidden")
  let readCards :: [[Text]]
      readCards = fromJust $ readMay preCards 
      cards = map mkCard readCards 
  
  defaultLayout $ toWidget [hamlet| #{show cards} |]

  where 
    mkCard (term:def:correct:incorrect:[]) = Card (encodeUtf8 term) (encodeUtf8 def) (fromJust $ readMay correct) (fromJust $ readMay incorrect) 

