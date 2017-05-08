module Main where

import           Data.List              (intercalate)
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

printer :: MarkdownDocument -> String
printer document =
  intercalate "\n" (map printMarkdown document)


printMarkdown :: Markdown -> String
printMarkdown (Paragraph content) =
  "<p>" ++ concat (printInline <$> content) ++ "</p>"
printMarkdown (Header level content) =
    "<h" ++ show level ++ ">" ++ concat (printInline <$> content) ++"</h" ++ show level ++ ">"

printInline :: Inline -> String
printInline (InlineCode content) = "<pre style=\"color: red\">" ++ content ++ "</pre>"
printInline (InlineText content) = content
printInline (Link content link) = "<a href=\"" ++ link ++ "\">" ++ content ++ "</a>"

main :: IO ()
main = do
  input <- hGetContents stdin
  let parsed = parse markdownDocument "" input
  case parsed of
    Left error     -> print error
    Right markdown -> do
      print markdown
      putStrLn (printer markdown)
