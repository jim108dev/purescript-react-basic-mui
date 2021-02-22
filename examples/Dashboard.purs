module Examples.Dashboard where

import Prelude
import Data.Array (elem, singleton) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import MUI.Core (jss, mediaQuery)
import MUI.Core.AppBar as AppBar
import MUI.Core.Backdrop (_UnsafeBackdrop)
import MUI.Core.Badge (badge, badgeWithStyles)
import MUI.Core.Badge (color) as Badge
import MUI.Core.Box (box)
import MUI.Core.Button (button, buttonWithStyles)
import MUI.Core.Button (button, color, props) as Button
import MUI.Core.ButtonGroup (buttonGroup)
import MUI.Core.ButtonGroup (buttonGroupWithStyles, color, variant) as ButtonGroup
import MUI.Core.Container (container)
import MUI.Core.CssBaseline (cssBaseline)
import MUI.Core.Divider (divider, dividerWithStyles)
import MUI.Core.Divider (variant) as Dividier
import MUI.Core.Drawer (anchor) as Drawer
import MUI.Core.Drawer (drawer)
import MUI.Core.Fade (fade)
import MUI.Core.FormControl (formControlWithStyles)
import MUI.Core.FormControl (props) as FormControl
import MUI.Core.FormHelperText (formHelperText)
import MUI.Core.Grid (grid)
import MUI.Core.Grid (gridSize) as Grid
import MUI.Core.Hidden (hidden)
import MUI.Core.Hidden (implementation, only) as Hidden
import MUI.Core.IconButton as IconButton
import MUI.Core.Input (input)
import MUI.Core.InputLabel (inputLabel)
import MUI.Core.Link (color) as Link
import MUI.Core.Link (link)
import MUI.Core.List (list)
import MUI.Core.ListItem (listItem)
import MUI.Core.ListItemIcon (listItemIcon)
import MUI.Core.ListItemText (listItemText)
import MUI.Core.Modal (modal)
import MUI.Core.Styles (Theme)
import MUI.Core.Styles (md) as Styles
import MUI.Core.Styles.CreateMuiTheme (createMuiTheme)
import MUI.Core.Styles.CreatePalette (paletteOptions)
import MUI.Core.Styles.MakeStyles (makeStyles)
import MUI.Core.Styles.MuiThemeProvider (muiThemeProvider)
import MUI.Core.Styles.Types (multiplier, spacing)
import MUI.Core.TextField (filledWithStyles, outlinedWithStyles, standardWithStyles)
import MUI.Core.TextField (filledWithStyles, outlinedWithStyles, standardWithStyles) as TextField
import MUI.Core.TextField.FilledTextField (props) as FilledTextField
import MUI.Core.TextField.OutlinedTextField (props) as OutlinedWithStyles
import MUI.Core.TextField.StandardTextField (props) as StandardTextField
import MUI.Core.Toolbar (toolbar)
import MUI.Core.Typography (typography)
import MUI.Core.Typography (variant) as Typography
import MUI.Icons (icon, iconWithStyles)
import MUI.Icons (props) as Icons
import MUI.Icons.Inbox (inbox)
import MUI.Icons.Mail (mail)
import MUI.Icons.Menu (menu)
import MUI.React.TransitionGroup (single) as TransitionGroup
import MUI.System.Display (flex, none) as Only
import MUI.System.Display (hiding)
import MUI.System.Flexbox.JustifyContent (flexEnd) as JustifyContent
import React.Basic (Component, JSX, ReactComponent, createComponent, element, fragment, make)
import React.Basic.DOM (a, button, div, div_, form, h2, p, span, text) as DOM
import React.Basic.DOM (render)
import React.Basic.Events (handler_)
import React.Basic.Hooks (useState)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Props
  = {}

theme :: Theme
theme =
  createMuiTheme
    { palette: paletteOptions { type: "light" } }

component :: Component Props
component = createComponent "Counter"

gridItem :: JSX -> JSX
gridItem child = grid { item: true, children: [ child ], xs: Grid.gridSize.six }

arr :: forall a. a -> Array a
arr = Array.singleton

-- | Based on: https://github.com/mui-org/material-ui/blob/master/docs/src/pages/components/drawers/TemporaryDrawer.js
drawerList :: Effect (ReactComponent {})
drawerList =
  let
    useStyles =
      makeStyles \t ->
        { paper:
            { backgroundColor: theme.palette.background.paper
            , padding:
                spacing
                  (multiplier 2.0)
                  (multiplier 4.0)
                  (multiplier 3.0)
                  (multiplier 4.0)
                  t
            }
        , list: { width: 250 }
        , fullList: { width: "auto" }
        }

    menuList classes anchor =
      DOM.div
        $ { className:
              if anchor `Array.elem` [ Drawer.anchor.bottom, Drawer.anchor.top ] then
                classes.fullList
              else
                classes.list
          , children: _
          , role: "presentation"
          }
        $ arr
        <<< list
        $ { children: _ }
        $ ( [ "Inbox", "Starred", "Send email", "Drafts" ]
              <#> \text ->
                  listItem
                    $ { button: true, children: _ }
                        [ listItemIcon { children: [ icon (if text == "Inbox" then inbox else mail) {} ] }
                        , listItemText { primary: DOM.text text }
                        ]
          )
        <> [ divider {} ]
        <> ( [ "All mail", "Trash", "Spam" ]
              <#> \text ->
                  listItem
                    $ { button: true, children: _ }
                        [ listItemIcon { children: [ icon (if text == "Inbox" then inbox else mail) {} ] }
                        , listItemText { primary: DOM.text text }
                        ]
          )
  in
    React.component "TemporaryDrawer" \_ -> React.do
      state /\ setState <- useState Nothing
      classes <- useStyles
      let
        -- In the original example we have here also this check
        -- if (event.type === 'keydown' && (event.key === 'Tab' || event.key === 'Shift')) {
        --    return;
        --  }
        openDrawer a = setState (const $ Just a)

        closeDrawer = setState (const Nothing)
      pure $ DOM.div_ $ [ Drawer.anchor.left, Drawer.anchor.right, Drawer.anchor.top, Drawer.anchor.bottom ]
        <#> \anchor ->
            DOM.div
              $ { key: unsafeCoerce anchor, children: _ }
                  [ button
                      { onClick: handler_ (openDrawer anchor)
                      , children: [ DOM.text (unsafeCoerce anchor) ]
                      }
                  , drawer
                      $ { anchor
                        , open: Just anchor == state
                        , onClose: handler_ closeDrawer
                        , children: _
                        }
                      $ [ menuList classes anchor ]
                  ]

type Components
  = { drawerList :: ReactComponent {}
    , appBar :: ReactComponent {}
    }

menuIcon = unsafePerformEffect $ iconWithStyles (\t -> { root: jss { marginRight: t.spacing 2.0 } }) menu

appBar :: Effect (ReactComponent {})
appBar =
  let
    useStyles =
      makeStyles \t ->
        { paper:
            { backgroundColor: theme.palette.background.paper
            , padding:
                spacing
                  (multiplier 2.0)
                  (multiplier 4.0)
                  (multiplier 3.0)
                  (multiplier 4.0)
                  t
            }
        , list: { width: 250 }
        , fullList: { width: "auto" }
        }

    menuList classes anchor =
      DOM.div
        $ { className:
              if anchor `Array.elem` [ Drawer.anchor.bottom, Drawer.anchor.top ] then
                classes.fullList
              else
                classes.list
          , children: _
          , role: "presentation"
          }
        $ arr
        <<< list
        $ { children: _ }
        $ ( [ "Inbox", "Starred", "Send email", "Drafts" ]
              <#> \text ->
                  listItem
                    $ { button: true, children: _ }
                        [ listItemIcon { children: [ icon (if text == "Inbox" then inbox else mail) {} ] }
                        , listItemText { primary: DOM.text text }
                        ]
          )
        <> [ divider {} ]
        <> ( [ "All mail", "Trash", "Spam" ]
              <#> \text ->
                  listItem
                    $ { button: true, children: _ }
                        [ listItemIcon { children: [ icon (if text == "Inbox" then inbox else mail) {} ] }
                        , listItemText { primary: DOM.text text }
                        ]
          )

    anchor = Drawer.anchor.left
  in
    React.component "TopDrawer" \_ -> React.do
      state /\ setState <- useState Nothing
      classes <- useStyles
      let
        openDrawer a = setState (const $ Just a)

        closeDrawer = setState (const Nothing)
      pure $ DOM.div
        $ { children: _ }
            [ AppBar.appBar' $ AppBar.props
                $ { position: AppBar.position.absolute
                  , children: _
                  }
                    [ toolbar
                        $ { children: _ }
                            [ IconButton.iconButton
                                { onClick: handler_ (openDrawer anchor)
                                , children: [ menuIcon $ Icons.props {} ]
                                }
                            , typography $ { children: _, variant: Typography.variant.h6 } <<< Array.singleton
                                $ link { children: [ DOM.text "Dashboard" ], href: "#TEST", color: Link.color.inherit }
                            ]
                    ]
            , drawer
                $ { anchor
                  , open: Just anchor == state
                  , onClose: handler_ closeDrawer
                  , children: _
                  }
                $ [ menuList classes anchor ]
            ]

{-
           [ AppBar.appBar' $ AppBar.props
                $ { position: AppBar.position.absolute
                  , children: _
                  }
                    [ toolbar
                        $ { children: _ }
                            [ menuIcon
                                $ Icons.props { onClick: handler_ (openDrawer Drawer.anchor.left) }
                            , typography $ { children: _, variant: Typography.variant.h6 } <<< Array.singleton
                                $ link { children: [ DOM.text "Dashboard" ], href: "#TEST", color: Link.color.inherit }
                            ]
                    ]
            , drawer
                $ { anchor
                  , open: Just anchor == state
                  , onClose: handler_ closeDrawer
                  , children: _
                  }
                $ [ menuList classes anchor ]
            ]
-}
-- | XXX:
-- | This kind of unsafe execution should be
-- | only on the top level of the module.
-- | You SHOULD NOT add any constraint to the
-- | type here - like taking the props record
-- | and transforming it to the final opaque value.
{-
appBar' = appBarWithStyles (\t → { root: style t })
  where
  style t =
    mediaQuery (t.breakpoints.down Styles.md)
      (jss { backgroundColor: t.palette.primary.dark })
      <> mediaQuery (t.breakpoints.up Styles.md)
          (jss { backgroundColor: t.palette.secondary.dark })
-}
app :: Components -> JSX
app components = make component { initialState: {}, render } {}
  where
  render self =
    muiThemeProvider $ { theme, children: _ }
      $ element components.appBar {}

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  components <- { drawerList: _, appBar: _ } <$> drawerList <*> appBar
  case container of
    Nothing -> throw "Container element not found."
    Just c -> render (app components) c
