{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Utils where 

import Import 

import Data.ByteString.Char8 as B (pack, unpack)



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


