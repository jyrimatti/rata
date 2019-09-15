{-# LANGUAGE TypeApplications #-}
module Dispatcher where

import Prelude                        ( (.) )
import React.Flux                     ( action
                                                , SomeStoreAction
                                                , EventModification
                                                , simpleHandler
                                                )
import Store

dispatch :: AppAction -> [SomeStoreAction]
dispatch a = [action @AppState a]

handle :: AppAction -> ([SomeStoreAction], [EventModification])
handle = simpleHandler . dispatch
