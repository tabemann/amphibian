module Network.IRC.Client.Amphibian.StyledText

       (StyledText(..),
        StyledTextElement(..),
        TextStyle(..),
        TextColor,
        empty,
        addStyle,
        removeStyle,
        setStyle,
        mergeStyle,
        append,
        concat,
        intercalate,
        isColor,
        decode,
        encode)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.Text as T
import Text.Read (readMaybe)
import Text.Printf
import Data.List (find,
                  intercalate)

-- | Empty styled text.
empty :: StyledText
empty = StyledText []

-- | Attach a style to text.
addStyle :: [TextStyle] -> T.Text -> StyledText
addStyle style text = StyledText [StyledTextElement style text]

-- | Remove style from text.
removeStyle :: StyledText -> T.Text
removeStyle (StyledText xs) = T.intercalate T.empty $ map (\(StyledTextElement _ text) -> text) xs

-- | Set style for text.
setStyle :: [TextStyle] -> StyledText -> StyledText
setStyle style styledText = addStyle style . removeStyle

-- | Merge style into existing style for text.
mergeStyle :: [TextStyle] -> StyledText -> StyledText
mergeStyle mergedStyle (StyledText elements) = StyledText $ mergeStyle' elements
  where mergeStyle' (StyledTextElement style text : rest) =
          let style' = if any isColor mergedStyle then filter (not . isColor) style else style in
          let style = if TxstBold `elem` mergedStyle then filter (/= TxstBold) style' else style' in
          let style' = if TxstUnderline `elem` mergedStyle then filter (/= TxstUnderline) style else style in
          let style = mergedStyle ++ style' in
          StyledTextElement style text : mergeStyle' rest
        mergeStyle' [] = []  

-- | Set base color for style.
setBaseColor :: TextColor -> StyledText -> StyledText
setBaseColor color (StyledText elements) = StyledText $ serBaseColor' elements
  where setBaseColor' (StyledTextElement style text : rest)
          | any isColor style = StyledTextElement style text : setBaseColor' rest
          | otherwise = StyledTextElement (TxstColor color : style) text : setBaseColor' rest
        setBaseColor' [] = []

-- | Append two sections of styled text.
append :: StyledText -> StyledText -> StyledText
append (StyledText xs) (StyledText ys) = StyledText $ xs ++ ys

-- Concatenate a list of styled text.
concat :: [StyledText] -> StyledText
concat xs = StyledText . concat $ map (\(StyledText ys) -> ys) xs

-- | Intercalate a list of styled text.
intercalate : StyledText -> [StyledText] -> StyledText
intercalate (StyledText x) xs = StyledText . intercalate x $ map (\(StyledText ys) -> ys) xs

-- | Get whether a style element is a color.
isColor :: TextStyle -> Bool
isColor (TxstColor _) = True
isColor _ = False

-- | Special encoded characters.
specialChars :: [Char]
specialChars = ['\x2', '\x1F', '\xF', '\x3']

-- | Decode styled text.
decode :: T.Text -> StyledText
decode text = decode' text [] []
  where decode' text style parts =
          case T.uncons text of
            Just (char, rest)
              | char == '\x2' ->
                if TxstBold `notElem` style
                then decode' rest (TxstBold : style) parts
                else case parts of
                  (StyledTextElement _ lastPart) : otherParts ->
                    let (part, rest') = T.break (\char -> char `elem` specialChars) rest in
                    decode' rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                  _ -> decode' rest style parts
              | char == '\x1F' ->
                if TxstUnderline `notElem` style
                then decode' rest (TxstUnderline : style) parts
                else case parts of
                  (StyledTextElement _ lastPart) : otherParts ->
                    let (part, rest') = T.break (\char -> char `elem` specialChars) rest in
                    decode' rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                  _ -> decode' rest style parts
              | char == '\xF' ->
                case parts of
                  (StyledTextElement [] lastPart) : otherParts ->
                      let (part, rest') = T.break (\char -> char `elem` specialChars) rest in
                      decode' rest' [] (StyledTextElement [] (T.append lastPart part) : otherParts)
                  _ -> decode' rest [] parts
              | char == '\x3' ->
                let (color, rest) = T.splitAt 2 rest in
                case readMaybe color of
                  Just color ->
                    if color `elem` [0 .. 15]
                    then
                      if TxstColor color `notElem` style
                      then 
                        let styleFilter (TxstColor _) -> False
                            styleFilter _ -> True in
                        decode' rest (TxstColor color : filter styleFilter style) parts
                      else case parts of
                        (StyledTextElement [] lastPart) : otherParts ->
                          let (part, rest') = T.break (\char -> char `elem` specialChars) rest in
                          decode' rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                        _ -> decode rest style parts
                    else decode' rest style parts
                  Nothing -> decode' rest style parts
              | otherwise ->
                let (part, rest) = T.break (\char -> char `elem` specialChars) text in
                decode' rest style (StyledTextElement style part : parts)
            Nothing -> StyledText $ reverse parts

-- | Encode styled text.
encode :: StyledText -> T.Text
encode (StyledText parts) = encode' parts [] []
  where encode' ((StyledTextElement partStyle part) : rest) style encoded =
          if part /= T.empty
          then
            let (style', encoded') =
              if ((TxstBold `elem` style) && (TxstBold `notElem` partStyle)) ||
                 ((TxstUnderline `elem` style) && (TxstUnderline `notElem` partStyle)) ||
                 (any isColor style && not (any isColor partStyle))
              then ([], T.singleton '\xF' : encoded)
              else (style, encoded) in
            let (style, encoded) =
              if (TxstBold `elem` partStyle) && (TxstBold `notElem` style')
              then (TxstBold : style', T.singleton '\x2' : encoded')
              else (style', encoded') in
            let (style', encoded') =
              if (TxstUnderline `elem` partStyle) && (TxstUnderline `notElem` style)
              then (TxstUnderline : style, T.singleton '\x1F' : encoded)
              else (style, encoded) in
            let (style, encoded) =
              case (find isColor partStyle) of
                Just (TxstColor partStyleColor) ->
                  if partStyleColor `elem` [0 .. 15]
                  then
                    case (find isColor style) of
                      Just (TxstColor styleColor) ->
                        if partStyleColor /= styleColor
                        then (TxstColor partStyleColor : filter (not . isColor) style',
                              T.pack (printf "\x3%02d" partStyleColor) : encoded')
                        else (style', encoded')
                      Nothing -> (TxstColor partStyleColor : filter (not . isColor) style',
                                  T.pack (printf "\x3%02d" partStyleColor) : encoded')
                  else (style', encoded')
                Nothing -> (style', encoded') in
            encode' rest style (part : encoded)
          else encode' rest style encoded
        encode' [] _ encoded = T.intercalate T.empty $ reverse encoded
