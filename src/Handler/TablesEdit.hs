{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}



module Handler.TablesEdit where




import Import
import Yesod.Form.Bootstrap3

import Prelude (map, (++))

itemsForm :: Int -> [AForm Handler Text]
itemsForm x =  [tForm n | n <- [1..x]]
  where 
    tForm :: Int -> AForm Handler Text
    tForm n = areq textField (displayString n) Nothing 
    displayString n = bfs (pack ("Item number " Prelude.++ show n Prelude.++ ":") :: Text)

getTablesEditR :: Int -> Handler Html
getTablesEditR int = do 
    let mkPost form = generateFormPost $ renderBootstrap3 BootstrapBasicForm form 
    let forms = Prelude.map mkPost (itemsForm int) 
    results <- sequence forms 
    defaultLayout $(widgetFile "tablesedit") 

    

    
