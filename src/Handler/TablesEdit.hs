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


getTablesEditR :: TableId -> Handler Html
getTablesEditR tableId = do 
  (Table title contents _) <- runDB $ get404 tableId 
  let elems = Data.Text.splitOn delimiter contents 
  let (col1, col2) = (unsafeTail $ takeWhile (/= "definition") elems, unsafeTail $ dropWhile (/= "definition") elems) 
  let cols = zip  col1 col2 
  
  defaultLayout $ do
      setTitle $ toMarkup title
      $(widgetFile "tablesedit")



    

    
