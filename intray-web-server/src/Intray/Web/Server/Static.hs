{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Intray.Web.Server.Static where

import Import

import Yesod.EmbeddedStatic

mkEmbeddedStatic
    False
    "myStatic"
    [ embedFile "static/logo.svg"
    , embedFile "static/gtd_flowchart.jpg"
    , embedFile "static/semantic/dist/semantic.min.css"
    , embedFile "static/semantic/dist/semantic.min.js"
    , embedDirAt
          "static/semantic/dist/themes/default/assets/fonts"
          "static/semantic/dist/themes/default/assets/fonts"
    ]
