{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.DeckStudy where

import Import
import Utils 
import Data.Maybe
import Data.Serialize

getDeckStudyR :: DeckId -> Handler Html
getDeckStudyR deckId = do 
    
  (Deck title contents _) <- runDB $ get404 deckId 
  let cards = getCards contents 
  --_ <- runDB $ deleteWhere [DeckTitle !=. ""]
  defaultLayout $ do
      $(widgetFile "deckstudy")



postDeckStudyR :: DeckId -> Handler Html
postDeckStudyR deckId = do 
  preCards <- getPostParams >>= (return . fromJust . lookup "hidden")
  let readCards :: [[Text]]
      myRead :: Text -> Maybe [[[Text]]]
      myRead = readMay 
      readCards = (concat <$>) <$> fromJust $ myRead preCards 
      cards = map mkCard readCards 
  
  runDB $ update deckId [DeckCards =. (runPut . put $ cards)]


  
  defaultLayout $ toWidget [hamlet| #{show cards} |] 



  where 
    mkCard (term:def:correct:incorrect:[]) = Card (encodeUtf8 term) (encodeUtf8 def) (fromJust $ readMay correct) (fromJust $ readMay incorrect) 

