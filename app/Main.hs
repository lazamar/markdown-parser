module Main where

import           Data.List              (intercalate)
import qualified Data.Text              as T
import           Lib
import           Lucid
import           System.IO              (hGetContents, stdin)
import           Text.Megaparsec
import           Text.Megaparsec.String


type MarkdownDocument =
   [Markdown]

data Markdown
  = Paragraph [Inline]
  | Header Int [Inline]
  deriving (Show)

data Inline
  = InlineCode String
  | InlineText String
  | Link String String
  deriving (Show)

header1 :: Parser Markdown
header1 =
   Header 1 <$> (string "# " >> someTillEol inline)

someTillEol :: Parser a -> Parser [a]
someTillEol p = someTill p ((eol >> return ()) <|> eof)

paragraph :: Parser Markdown
paragraph =
  Paragraph <$>
    (
      someTillEol inline

    )


inlineCode :: Parser Inline
inlineCode =
  fmap InlineCode
  (between (char '`') (char '`') (many (noneOf "`")))


inlineText :: Parser Inline
inlineText =
  InlineText <$> some (noneOf "\n`")


link :: Parser Inline
link =
  Link <$>
  between (char '[') (char ']') (many (noneOf "]"))
  <*> between (char '(') (char ')') (many (noneOf ")"))


inline :: Parser Inline
inline =
  inlineCode <|> link <|> inlineText


-------------------------------------------



markdownDocument :: Parser MarkdownDocument
markdownDocument =
  many (header1 <|> paragraph)

printer :: MarkdownDocument -> Html ()
printer document =
  html_ $ do
    head_

           (link_ [rel_ (T.pack "stylesheet"), href_ (T.pack "https://rawgit.com/sinnerschrader/markdown-css/master/swiss.css")])
    body_ (mconcat (map printMarkdown document))


printMarkdown :: Markdown -> Html ()
printMarkdown (Paragraph content) =
  p_ (mconcat (printInline <$> content))
printMarkdown (Header level content) =
    h1_ (mconcat (printInline <$> content))

printInline :: Inline -> Html ()
printInline (InlineCode content) = code_ (toHtml content)
printInline (InlineText content) = toHtml content
printInline (Link content link)  = a_ [href_ (T.pack link)] (toHtml content)

main :: IO ()
main = do
  input <- hGetContents stdin
  let parsed = parse markdownDocument "" input
  case parsed of
    Left error     -> print error
    Right markdown -> do
      putStrLn "Parsed:\n"
      print markdown
      putStrLn ""
      putStrLn "Here is your HTML:"
      print (printer markdown)
