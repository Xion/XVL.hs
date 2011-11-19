{- Contains the types describing the structure of XVL document. -}
module Text.XVL.Structure (
        XVLDocument,
        XVLItem(..),
        XVLValue(..)
        ) where


type XVLDocument = [XVLItem]

data XVLItem = XVLSection {
                   xvlSectionName :: String,
                   xvlSectionItems :: [XVLItem]
               }
             | XVLKeyValue {
                   xvlKey :: String,
                   xvlValue :: Maybe XVLValue
               }
               
data XVLValue = XVLText String
              | XVLArray [XVLValue]