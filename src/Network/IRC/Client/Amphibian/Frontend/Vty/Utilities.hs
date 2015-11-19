module Network.IRC.Client.Amphibian.Frontend.Vty.Utilities

       (breakLines,
        fitWidth)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import qualified Data.Text as T
import qualified Graphics.Vty.Image as VIm

-- | Break text into lines.
breakLines :: Int -> StyledText -> [StyledText]
breakLines width (StyledText styledText) = breakLines' width styledText 0 [] []
  where breakLines' width (StyledTextElement style part : rest) widthCount
          lineParts@(StyledTextElement lastStyle lastPart : lineRest) otherParts =
            case T.uncons part of
             Just (char, partRest) ->
               let charWidth = VIm.safeWcWidth char in
               if widthCount + charWidth <= width
               then if style == lastStyle
                    then breakLines' width (StyledTextElement style partRest : rest) (widthCount + charWidth)
                         (StyledTextElement lastStyle (T.snoc lastPart char) : lineRest) otherParts
                    else breakLines' width (StyledTextElement style partRest : rest) (widthCount + charWidth)
                         (StyledTextElement style (T.singleton char) : lineParts) otherParts
               else breakLines' width (StyledTextElement style partRest : rest) charWidth
                    [StyledTextElement style (T.singleton char)] (StyledText (reverse lineParts) : otherParts)
             Nothing -> breakLines' width rest widthCount lineParts otherParts
        breakLines' _ [] _ lineParts@(_ : _) otherParts = reverse $ Styledtext (reverse lineParts) : otherParts
        breakLines' _ [] _ [] otherParts = reverse otherParts

-- | Get width and length in characters of text that fit within a given width, or Nothing if characters do not
-- reach total width.
fitWidth :: Int -> T.Text -> Maybe (Int, Int)
fitWidth totalWidth text = fitWidth' totalWidth text 0 0
  where fitWidth' totalWidth text prevWidth prevLength =
          case T.uncons text of
           Just (char, rest) ->
             let charWidth = VIm.safeWcWidth char in
             if prevWidth + charWidth <= totalWidth
             then fitWidth' totalWidth rest (prevWidth + charWidth) (prevLength + 1)
             else Just (prevWidth, prevLength)
           Nothing
             | prevWidth == totalWidth -> Just (prevWidth, prevLength)
             | otherwise -> Nothing
