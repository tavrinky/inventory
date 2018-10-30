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

getTablesEditR :: TableId -> Handler Html
getTablesEditR tableId = do 
  (Table contents _) <- runDB $ get404 tableId
  defaultLayout $ do
      $(widgetFile "tablesedit")

    

    
