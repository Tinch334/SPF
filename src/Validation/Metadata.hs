module Validation.Metadata (validateMeta) where

import Datatypes.ParseTokens
import Datatypes.ValidatedTokens
import Datatypes.Located
import Validation.GenericValidations

import Data.Validation

locateMeta p v = case v of
  Failure errs -> Failure $ (map (at p) errs)
  Success s -> Success s


-- Validates the documents metadata.
--validateMeta :: DocumentMetadata -> Validation [LocatedError] ValidatedMetadata
validateMeta (DocumentMetadata t a d) = ValidatedMetadata
  <$> maybeValidate t <*> maybeValidate a <*> maybeValidate d where
    maybeValidate (Just (Located pos (PMetaOpt (PTitle t) op))) = locateMeta pos $ Just <$> namedFontWithSize VTitle t op
    maybeValidate (Just (Located pos (PMetaOpt (PAuthor t) op))) = locateMeta pos $ Just <$> namedFontWithSize VAuthor t op
    maybeValidate (Just (Located pos (PMetaOpt (PDate t) op))) = locateMeta pos $ Just <$> namedFontWithSize VDate t op
    maybeValidate Nothing = Success Nothing