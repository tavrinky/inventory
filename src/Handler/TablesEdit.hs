{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}



module Handler.TablesEdit where




import Import
import Utils

import qualified Data.Text (splitOn)
import Database.Persist.Sql
import Text.Blaze


getTablesEditR :: DeckId -> Handler Html
getTablesEditR deckId = do 
  (Deck title contents _) <- runDB $ get404 deckId 
  let cards = getCards contents 
  let cols = map (\(Card term def _ _) -> (decodeUtf8 term, decodeUtf8 def)) cards
  --_ <- runDB $ deleteWhere [TableTablecontents !=. ""]
  defaultLayout $ do
      setTitle $ toMarkup title
      $(widgetFile "tablesedit")



    

    
