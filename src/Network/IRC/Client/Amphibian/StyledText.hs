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
        setBaseForeColor,
        setBaseBackColor,
        length,
        append,
        appendUnstyled,
        insertUnstyled,
        splitAt,
        concat,
        intercalate,
        isForeColor,
        isBackColor,
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
          let style' = if any isForeColor mergedStyle then filter (not . isForeColor) style else style in
          let style = if any isBackColor mergedStyle then filter (not . isBackColor) style' else style' in
          let style' = if TxstBold `elem` mergedStyle then filter (/= TxstBold) style else style in
          let style = if TxstUnderline `elem` mergedStyle then filter (/= TxstUnderline) style' else style' in
          StyledTextElement (mergedStyle ++ style) text : mergeStyle' rest
        mergeStyle' [] = []  

-- | Set base foreground color for style.
setBaseForeColor :: TextColor -> StyledText -> StyledText
setBaseForeColor color (StyledText elements) = StyledText $ setBaseForeColor' color elements
  where setBaseForeColor' color (StyledTextElement style text : rest)
          | any isForeColor style = StyledTextElement style text : setBaseForeColor' color rest
          | otherwise = StyledTextElement (TxstForeColor color : style) text : setBaseForeColor' color rest
        setBaseForeColor' _ [] = []

-- | Set base background color for style.
setBaseBackColor :: TextColor -> StyledText -> StyledText
setBaseBackColor color (StyledText elements) = StyledText $ setBaseBackColor' color elements
  where setBaseBackColor' color (StyledTextElement style text : rest)
          | any isBackColor style = StyledTextElement style text : setBaseBackColor' color rest
          | otherwise = StyledTextElement (TxstBackColor color : style) text : setBaseColor' color rest
        setBaseColor' _  [] = []

-- | Get the length of styled text.
length :: StyledText -> Int
length (StyledText xs) = foldl' (\prev (StyledTextElement _ text) -> prev + T.length text) 0 xs

-- | Append two sections of styled text.
append :: StyledText -> StyledText -> StyledText
append (StyledText xs) (StyledText ys) = StyledText $ xs ++ ys

-- | Append unstyled text after styled text.
appendUnstyled :: StyledText -> T.Text -> StyledText
appendUnstyled (StyledText xs) text = StyledText $ appendUnstyled' xs text
  where appendUnstyled' (x : xs@(_ : _)) text = x : appendUnstyled' xs text
        appendUnstyled' (StyledTextElement style text : []) appendText =
          [StyledTextElement style $ T.append text appendedText]
        appendUnstyled' [] text = [StyledTextElement [] text]

-- | Insert unstyled text into styled text.
insertUnstyled :: Int -> T.Text -> StyledText -> StyledText
insertUnstyled insertIndex insertedText styledText =
  let (beforeStyledText, afterStyledText) = splitAt insertIndex styledText in
  append (appendUnstyled beforeStyledText insertedText) afterStyledText

-- | Split unstyled text at a index.
splitAt :: Int -> StyledText -> (StyledText, StyledText)
splitAt (StyledText xs) index = splitAt' xs index []
  where splitAt' (x@(StyledTextElement style text) : rest) index prev =
          if index < T.length text
          then let (before, after) = T.splitAt index text in
                (StyledText . reverse $ StyledTextElement style before : prev,
                 StyledText $ StyledTextElement style after : rest)
          else splitAt' rest (index - T.length text) (x : prev)
        splitAt' [] _ prev = (StyledText $ reverse prev, StyledText [])

-- | Concatenate a list of styled text.
concat :: [StyledText] -> StyledText
concat xs = StyledText . concat $ map (\(StyledText ys) -> ys) xs

-- | Intercalate a list of styled text.
intercalate :: StyledText -> [StyledText] -> StyledText
intercalate (StyledText x) xs = StyledText . intercalate x $ map (\(StyledText ys) -> ys) xs

-- | Get whether a style element is a foreground color.
isForeColor :: TextStyle -> Bool
isForeColor (TxstForeColor _) = True
isForeColor _ = False

-- | Get whether a style element is a background color.
isBackColor :: TextStyle -> Bool
isBackColor (TxstBackColor _) = True
isBackColor _ = False

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
                  case T.uncons rest of
                   Just (char, rest')
                     | char == ',' -> decodeBackColor' rest style parts
                   _ -> decodeForeColor rest style parts
              | otherwise ->
                  let (part, rest) = T.break (`elem` specialChars) text in
                  decode' rest style (StyledTextElement style part : parts)
            Nothing -> StyledText $ reverse parts
        decodeForeColor text style parts =
          let (color, rest) = T.splitAt 2 text in
          case readMaybe color of
           Just color ->
             if color `elem` [0 .. 15]
             then
               if TxstForeColor color `notElem` style
               then decode' rest (TxstForeColor color : filter (not . isForeColor) style) parts
               else case parts of
                     StyledTextElement [] lastPart : otherParts ->
                       let (part, rest') = T.break (`elem` specialChars) rest in
                       decodeBackColor rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                     _ -> decodeBackColor rest style parts
             else decode' text style parts
           Nothing -> decode' rest style parts
        decodeBackColor text style parts =
          case T.cons text of
           Just (char, rest)
             | char ==  ',' -> decodeBackColor' rest style parts
           _ -> decode' text style parts
        decodeBackColor' text style parts =
          let (color, rest) = T.splitAt 2 text in
          case readMaybe color of
           Just color ->
             if color `elem` [0 .. 15]
             then
               if TxstBackColor color `notElem` style
               then decode' rest (TxstBackColor color : filter (not . isBackColor) style) parts
               else case parts of
                     StyledTextElement [] lastPart : otherParts ->
                       let (part, rest') = T.break (`elem` specialChars) rest in
                       decode' rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                     _ -> decode' rest style parts
             else decode' (T.append (T.singleton ',') text) style parts
           Nothing -> decode' (T.append (T.singleton ',') text) style parts

-- | Encode styled text.
encode :: StyledText -> T.Text
encode (StyledText parts) = encode' parts [] []
  where encode' ((StyledTextElement partStyle part) : rest) style encoded =
          if part /= T.empty
          then
            let (style', encoded') =
                  if ((TxstBold `elem` style) && (TxstBold `notElem` partStyle)) ||
                     ((TxstUnderline `elem` style) && (TxstUnderline `notElem` partStyle)) ||
                     (any isForeColor style && not (any isForeColor partStyle)) ||
                     (any isBackColor style && not (any isBackColor partStyle))
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
            let (style, encoded, hasForeColor) =
                  case find isForeColor partStyle of
                   Just (TxstForeColor partStyleColor) ->
                     if partStyleColor `elem` [0 .. 15]
                     then
                       case find isForeColor style' of
                        Just (TxstForeColor styleColor) ->
                          if partStyleColor /= styleColor
                          then (TxstForeColor partStyleColor : filter (not . isForeColor) style',
                                T.pack (printf "\x3%02d" partStyleColor) : encoded', True)
                          else (style', encoded', False)
                        Nothing -> (TxstForeColor partStyleColor : filter (not . isForeColor) style',
                                    T.pack (printf "\x3%02d" partStyleColor) : encoded', True)
                     else (style', encoded', False)
                   Nothing -> (style', encoded', False) in
            let (style', encoded') =
                  case find isBackColor partStyle of
                   Just (TxstBackColor partStyleColor) ->
                     if partStyleColor `elem` [0 .. 15]
                     then
                       case find isBackColor style of
                        Just (TxstBackColor styleColor) ->
                          if partStyleColor /= styleColor
                          then
                            if hasForeColor
                            then (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                  T.pack (printf ",%02d" partStyleColor) : encoded)
                            else (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                  T.pack (printf "\x3,%02d" partStyleColor) : encoded)
                          else (style, encoded)
                        Nothing ->
                          if hasForeColor
                          then (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                T.pack (printf ",%02d" partStyleColor) : encoded)
                          else (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                T.pack (printf "\x3,%02d" partStyleColor) : encoded)
                     else (style, encoded)
                   Nothing -> (style, encoded) in
            encode' rest style' (part : encoded')
          else encode' rest style encoded
        encode' [] _ encoded = T.intercalate T.empty $ reverse encoded
