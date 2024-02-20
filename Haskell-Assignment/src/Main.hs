module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  _ <- DB.save DB.empty
  return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  let entryIdToGet = getOptId getOpts
  dbResult <- DB.load
  case dbResult of
    Success snippetDB ->
      let 
        foundEntry = DB.findFirst (\entry -> entryId entry == entryIdToGet) snippetDB
      in 
        case foundEntry of
          Nothing -> putStrLn "Entry not found"
          Just entry -> putStrLn (entrySnippet entry)
    _ -> putStrLn "Failed to load DB"
        
-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  dbResult <- DB.load
  case dbResult of
    Success snippetDB ->
      let 
        matchingEntries = DB.findAll (matchedByAllQueries queries) snippetDB where queries = searchOptTerms searchOpts
      in 
        if matchingEntries == [] 
          then putStrLn "No entries found"
          else mapM_ (putStrLn . show . FmtEntry) matchingEntries
    _ -> putStrLn "Failed to load DB"

makeEntry :: Int -> String -> AddOptions -> Entry
makeEntry id snippet addOpts =
  Entry
    { entryId = id,
      entrySnippet = snippet,
      entryFilename = addOptFilename addOpts,
      entryLanguage = addOptLanguage addOpts,
      entryDescription = addOptDescription addOpts,
      entryTags = addOptTags addOpts
    }

-- -- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  dbResult <- DB.load
  source <- readFile (addOptFilename addOpts)
  case dbResult of
    Success snippetDB -> 
      let 
        insertEntryAndSave = do
          let dbUpdated = DB.insertWith (\id -> makeEntry id source addOpts) snippetDB
          DB.save dbUpdated
          return ()
        entryExists = DB.findFirst(\entry -> entrySnippet entry == source) snippetDB
      in
        case entryExists of
          Nothing -> insertEntryAndSave
          Just entry -> Prelude.mapM_ putStrLn (["Entry with this content already exists: ", (show (FmtEntry entry))])
    _ -> putStrLn "Failed to load DB"

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
