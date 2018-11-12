{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE DeriveGeneric #-}


module Utils where 

import Import 

import Data.ByteString.Char8 as B (pack, unpack)
import Data.Serialize 
import Data.Either 
import Text.Blaze



data Card = Card {term :: ByteString, definition :: ByteString, correct :: Int, incorrect :: Int} deriving (Eq, Show, Read, Generic)
instance Serialize Card


logfile :: FilePath
logfile = "logfile.txt" 


getTables ::  HandlerFor site [Char]
getTables =  do 
    B.unpack <$> readFile logfile

appendTables :: HandlerFor App String -> HandlerFor App () 
appendTables str = str >>= writeOp

    
writeOp :: String -> HandlerFor App () 
writeOp = writeFile logfile . B.pack    


delimiter :: Text
delimiter = "\n"

innerdelimiter :: Text 
innerdelimiter = "\t"

getCards bcards = fromRight [] $ runGet Data.Serialize.get bcards  

instance ToMarkup ByteString where 
    toMarkup = toMarkup . decodeUtf8
    

