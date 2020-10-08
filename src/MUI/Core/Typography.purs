{- This module was autogenerated. Please don't edit. -}
module MUI.Core.Typography where

import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_p) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Align :: Type

align ::
  { center :: Align
  , inherit :: Align
  , justify :: Align
  , left :: Align
  , right :: Align
  }
align = { center: unsafeCoerce "center", inherit: unsafeCoerce "inherit", justify: unsafeCoerce "justify", left: unsafeCoerce "left", right: unsafeCoerce "right" }

foreign import data Color :: Type

color ::
  { error :: Color
  , inherit :: Color
  , initial :: Color
  , primary :: Color
  , secondary :: Color
  , textPrimary :: Color
  , textSecondary :: Color
  }
color = { error: unsafeCoerce "error", inherit: unsafeCoerce "inherit", initial: unsafeCoerce "initial", primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary", textPrimary: unsafeCoerce "textPrimary", textSecondary: unsafeCoerce "textSecondary" }

foreign import data Display :: Type

display ::
  { block :: Display
  , initial :: Display
  , inline :: Display
  }
display = { block: unsafeCoerce "block", initial: unsafeCoerce "initial", inline: unsafeCoerce "inline" }

foreign import data Variant :: Type

variant ::
  { body1 :: Variant
  , body2 :: Variant
  , button :: Variant
  , caption :: Variant
  , h1 :: Variant
  , h2 :: Variant
  , h3 :: Variant
  , h4 :: Variant
  , h5 :: Variant
  , h6 :: Variant
  , inherit :: Variant
  , overline :: Variant
  , srOnly :: Variant
  , subtitle1 :: Variant
  , subtitle2 :: Variant
  }
variant = { body1: unsafeCoerce "body1", body2: unsafeCoerce "body2", button: unsafeCoerce "button", caption: unsafeCoerce "caption", h1: unsafeCoerce "h1", h2: unsafeCoerce "h2", h3: unsafeCoerce "h3", h4: unsafeCoerce "h4", h5: unsafeCoerce "h5", h6: unsafeCoerce "h6", inherit: unsafeCoerce "inherit", overline: unsafeCoerce "overline", srOnly: unsafeCoerce "srOnly", subtitle1: unsafeCoerce "subtitle1", subtitle2: unsafeCoerce "subtitle2" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqDisplay :: Eq Display where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

instance eqAlign :: Eq Align where
  eq = unsafeRefEq

type TypographyClassesGenericRow a
  = ( alignCenter :: a
    , alignJustify :: a
    , alignLeft :: a
    , alignRight :: a
    , body1 :: a
    , body2 :: a
    , button :: a
    , caption :: a
    , colorError :: a
    , colorInherit :: a
    , colorPrimary :: a
    , colorSecondary :: a
    , colorTextPrimary :: a
    , colorTextSecondary :: a
    , displayBlock :: a
    , displayInline :: a
    , gutterBottom :: a
    , h1 :: a
    , h2 :: a
    , h3 :: a
    , h4 :: a
    , h5 :: a
    , h6 :: a
    , noWrap :: a
    , overline :: a
    , paragraph :: a
    , root :: a
    , srOnly :: a
    , subtitle1 :: a
    , subtitle2 :: a
    )

type TypographyClassesKey
  = TypographyClassesGenericRow String

type TypographyClassesJSS
  = TypographyClassesGenericRow JSS

type TypographyOptPropsRow (r :: # Type)
  = ( align :: Align
    , children :: Array JSX
    , classes :: { | TypographyClassesKey }
    , color :: Color
    , component :: String
    , display :: Display
    , gutterBottom :: Boolean
    , noWrap :: Boolean
    , paragraph :: Boolean
    , variant :: Variant
    , variantMapping :: { body1 :: String, body2 :: String, button :: String, caption :: String, h1 :: String, h2 :: String, h3 :: String, h4 :: String, h5 :: String, h6 :: String, overline :: String, srOnly :: String, subtitle1 :: String, subtitle2 :: String }
    | r
    )

type TypographyReqPropsRow (r :: # Type)
  = r

type TypographyPropsRow (r :: # Type)
  = TypographyOptPropsRow (TypographyReqPropsRow r)

foreign import _UnsafeTypography :: forall componentProps. ReactComponent { | TypographyPropsRow componentProps }

_Typography ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TypographyReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TypographyPropsRow React.Basic.DOM.Props_p) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Typography = unsafeCoerce _UnsafeTypography

typography ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TypographyReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TypographyPropsRow React.Basic.DOM.Props_p) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
typography props = element _Typography props

typographyWithStyles ::
  forall jss_ jss given optionalGiven optionalMissing props required.
  Nub' (TypographyReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TypographyPropsRow React.Basic.DOM.Props_p) props =>
  Prim.Row.Union given optionalMissing props =>
  Prim.Row.Union jss jss_ TypographyClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> { | given } -> JSX
typographyWithStyles style props = element (withStyles' style _Typography) props
  where
  withStyles' :: (MUI.Core.Styles.Theme -> { | jss }) -> ReactComponent { | given } -> ReactComponent { | given }
  withStyles' = unsafeCoerce MUI.Core.Styles.withStyles

foreign import data TypographyProps :: Type

typographyProps ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TypographyReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TypographyPropsRow React.Basic.DOM.Props_p) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> TypographyProps
typographyProps = unsafeCoerce
