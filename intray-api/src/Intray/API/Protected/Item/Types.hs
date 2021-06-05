{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Item.Types
  ( ItemType (..),
    TypedItem (..),
    textTypedItem,
    TypedItemCase (..),
    typedItemCase,
    AddedItem (..),
    ItemInfo (..),
    ItemUUID,
    module Data.UUID.Typed,
  )
where

import Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API.Types ()
import Intray.Data

data TypedItem = TypedItem
  { itemType :: ItemType,
    itemData :: ByteString
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity TypedItem

instance FromJSON TypedItem where
  parseJSON =
    withObject "TypedItem" $ \o ->
      TypedItem <$> o .: "type"
        <*> ( do
                t <- o .: "data"
                case Base64.decode $ SB8.pack t of
                  Left err -> fail $ unwords ["Failed to decode base64-encoded typed item data:", err]
                  Right r -> pure r
            )

instance ToJSON TypedItem where
  toJSON TypedItem {..} = object ["type" .= itemType, "data" .= SB8.unpack (Base64.encode itemData)]

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}

typedItemCase :: TypedItem -> Either String TypedItemCase
typedItemCase TypedItem {..} =
  case itemType of
    TextItem -> left show $ CaseTextItem <$> TE.decodeUtf8' itemData
    ImageItem it -> pure $ CaseImageItem it itemData

data TypedItemCase
  = CaseTextItem Text
  | CaseImageItem ImageType ByteString
  deriving (Show, Read, Eq, Ord, Generic)

data AddedItem a = AddedItem
  { addedItemContents :: a,
    addedItemCreated :: UTCTime
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (AddedItem a)

instance ToJSON a => ToJSON (AddedItem a) where
  toJSON AddedItem {..} = object ["contents" .= addedItemContents, "created" .= addedItemCreated]

instance FromJSON a => FromJSON (AddedItem a) where
  parseJSON = withObject "AddedItem" $ \o -> AddedItem <$> o .: "contents" <*> o .: "created"

data ItemInfo a = ItemInfo
  { itemInfoIdentifier :: ItemUUID,
    itemInfoContents :: a,
    itemInfoCreated :: UTCTime
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (ItemInfo a)

instance ToJSON a => ToJSON (ItemInfo a) where
  toJSON ItemInfo {..} =
    object
      ["id" .= itemInfoIdentifier, "contents" .= itemInfoContents, "created" .= itemInfoCreated]

instance FromJSON a => FromJSON (ItemInfo a) where
  parseJSON =
    withObject "ItemInfo TypedItem" $ \o ->
      ItemInfo <$> o .: "id" <*> o .: "contents" <*> o .: "created"
