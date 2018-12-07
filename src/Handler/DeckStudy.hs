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
  newPreCards <- getPostParams >>= (return . fromJust . lookup "new")
  oldPreCards <- getPostParams >>= (return . fromJust . lookup "old") 
  let 
      myRead :: Text -> Maybe [[[Text]]]
      myRead = readMay 
      readNewCards = (concat <$>) <$> fromJust $ myRead newPreCards 

      newCards = map mkCard readNewCards
      
      readOldCards = (concat <$>) <$> fromJust $ myRead oldPreCards 

      oldCards = map mkCard readOldCards 

      scores = zipMap pieceDiff (collect newCards) (collect oldCards) 

      

  runDB $ update deckId [DeckCards =. (runPut . put $ newCards)]  
  defaultLayout $ do $(widgetFile "deckafterstudy")



  where 
    mkCard (term:def:correct:incorrect:[]) = Card (encodeUtf8 term) (encodeUtf8 def) (fromJust $ readMay correct) (fromJust $ readMay incorrect) 

    zipMap _ [] _ = [] 
    zipMap f ((key, val1):xs) ys = val1 `f` val2 : zipMap f xs ys 
      where 
        val2 = fromJust (lookup key ys)

    collect cards = map (\(Card term def correct incorrect) -> ((term, def),(correct, incorrect))) cards
    pieceDiff (x1, y1) (x2, y2) = (x1-x2, y1-y2) 


        

