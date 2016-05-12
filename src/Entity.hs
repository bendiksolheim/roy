module Entity where

import Surface
import Material

data Entity = Entity
              { entityObject :: AnyObject
              , entityMaterial :: Material
              }
