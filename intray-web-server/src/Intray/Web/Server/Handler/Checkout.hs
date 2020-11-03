module Intray.Web.Server.Handler.Checkout
  ( getCheckoutSuccessR,
    getCheckoutCanceledR,
  )
where

import Intray.Web.Server.Foundation
import Yesod

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
