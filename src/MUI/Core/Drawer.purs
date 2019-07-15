module MUI.Core.Drawer where

import MUI.Core.Modal (ModalProps)
import MUI.Core.Paper (PaperProps)
import MUI.Core.Slide (SlideProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type DrawerProps =
  ( anchor :: String
  , children :: Array JSX
  , classes :: DrawerClassKey
  , elevation :: Number
  , "ModalProps" :: { | ModalProps } 
  , onClose :: EventHandler
  , open :: Boolean
  , "PaperProps" :: { | (PaperProps Props_div) }
  , "SlideProps" :: { | SlideProps }
  , transitionDuration :: { enter :: Number, exit :: Number }
  , variant :: String
  )

foreign import data DrawerClassKey :: Type
foreign import data DrawerPropsPartial :: Type

type DrawerClassKeyOptions =
  ( root :: String
  , docked :: String
  , paper :: String
  , paperAnchorLeft :: String
  , paperAnchorRight :: String
  , paperAnchorTop :: String
  , paperAnchorBottom :: String
  , paperAnchorDockedLeft :: String
  , paperAnchorDockedTop :: String
  , paperAnchorDockedRight :: String
  , paperAnchorDockedBottom :: String
  , modal :: String
  )

drawerClassKey 
  :: ∀ options options_
  . Union options options_ DrawerClassKeyOptions
  => Record options
  -> DrawerClassKey
drawerClassKey = unsafeCoerce

drawer
  :: ∀ props props_
  . Union props props_ DrawerProps
  => Record props 
  -> JSX
drawer = element _Drawer

drawerPropsPartial
  :: ∀ props props_
  . Union props props_ DrawerProps
  => Record props 
  -> DrawerPropsPartial 
drawerPropsPartial = unsafeCoerce

foreign import _Drawer :: ∀ a. ReactComponent a