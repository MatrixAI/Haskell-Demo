-- Demo showing how to bake runtime dependency (e.g. shellout cli programs) 
-- into the binary so that it becomes part of the closure of the build output
module Deps where

import           System.Process.Typed           ( runProcess_
                                                , proc
                                                )
import           Deps.Paths                     ( hello )

sayHello :: IO ()
sayHello = runProcess_ $ proc hello []
