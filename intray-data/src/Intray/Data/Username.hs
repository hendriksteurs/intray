{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Data.Username
  ( Username (),
    parseUsername,
    parseUsernameWithError,
    usernameText,
    validUsernameChar,
  )
where

import Data.Aeson as JSON
import Data.Aeson.Types as JSON (toJSONKeyText)
import qualified Data.Char as Char
import Data.Hashable
import qualified Data.Text as T
import Database.Persist.Sql
import Intray.Data.Import

newtype Username
  = Username
      { usernameText :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long.",
        mconcat
          $ flip map (zip [1 ..] $ map UsernameChar $ T.unpack t)
          $ \(ix, uc@(UsernameChar c)) ->
            annotate uc $ unwords ["character number", show (ix :: Int), "of the username:", show c]
      ]

instance Hashable Username

instance PersistField Username where
  toPersistValue (Username t) = PersistText t
  fromPersistValue (PersistText t) =
    case parseUsername t of
      Nothing -> Left "Text isn't a valid username"
      Just un -> Right un
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance FromJSONKey Username where
  fromJSONKey = FromJSONKeyTextParser parseUsername

instance FromJSON Username where
  parseJSON = withText "Username" parseUsername

parseUsername :: MonadFail m => Text -> m Username
parseUsername t =
  case parseUsernameWithError t of
    Left err -> fail err
    Right un -> pure un

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError t =
  case checkValidity $ Username t of
    Right un -> Right un
    Left chains -> Left $ unlines $ map go chains
  where
    go :: ValidationChain -> String
    go (Violated invariant) = "Violated: " ++ show invariant
    go (Location loc rest) = go rest ++ " in " ++ loc

instance ToJSON Username where
  toJSON = toJSON . usernameText

instance ToJSONKey Username where
  toJSONKey = toJSONKeyText usernameText

newtype UsernameChar
  = UsernameChar Char

instance Validity UsernameChar where
  validate (UsernameChar '-') = valid
  validate (UsernameChar '_') = valid
  validate (UsernameChar c) =
    mconcat
      [ check (not (Char.isControl c)) "The character is not a control character.",
        check (Char.isAlphaNum c) "The character is alphanumeric.",
        check (Char.isLatin1 c) "The character is part of Latin1."
      ]

validUsernameChar :: Char -> Bool
validUsernameChar = isValid . UsernameChar
