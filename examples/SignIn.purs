module Examples.SignIn where

import Prelude
import Data.Array (elem, singleton) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import MUI.Core (jss, mediaQuery)
import MUI.Core.AppBar (appBarWithStyles)
import MUI.Core.AppBar (position, props) as AppBar
import MUI.Core.Avatar (avatar)
import MUI.Core.Backdrop (_UnsafeBackdrop)
import MUI.Core.Badge (badge, badgeWithStyles)
import MUI.Core.Badge (color) as Badge
import MUI.Core.Box (box)
import MUI.Core.Button (button, buttonWithStyles)
import MUI.Core.Button (button, color, props, variant) as Button
import MUI.Core.ButtonBase as ButtonBase
import MUI.Core.ButtonGroup (buttonGroup)
import MUI.Core.ButtonGroup (buttonGroupWithStyles, color, variant) as ButtonGroup
import MUI.Core.Checkbox as Checkbox
import MUI.Core.Container (container)
import MUI.Core.Container as Container
import MUI.Core.CssBaseline (cssBaseline)
import MUI.Core.Divider (divider, dividerWithStyles)
import MUI.Core.Divider (variant) as Dividier
import MUI.Core.Drawer (anchor) as Drawer
import MUI.Core.Drawer (drawer)
import MUI.Core.Fade (fade)
import MUI.Core.FormControl (formControlWithStyles)
import MUI.Core.FormControl (props) as FormControl
import MUI.Core.FormControlLabel (formControlLabel)
import MUI.Core.FormHelperText (formHelperText)
import MUI.Core.Grid (alignItems, grid)
import MUI.Core.Grid (alignItems, gridSize) as Grid
import MUI.Core.Hidden (hidden)
import MUI.Core.Hidden (implementation, only) as Hidden
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
import MUI.Core.Styles.MakeStyles (UseStyles, makeStyles)
import MUI.Core.Styles.MuiThemeProvider (muiThemeProvider)
import MUI.Core.Styles.Types (multiplier, spacing)
import MUI.Core.TextField (filledWithStyles, outlinedWithStyles, standardWithStyles)
import MUI.Core.TextField (filledWithStyles, outlinedWithStyles, standardWithStyles) as TextField
import MUI.Core.TextField.FilledTextField (props) as FilledTextField
import MUI.Core.TextField.OutlinedTextField as OutlinedTextField
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
import MUI.System.Display (flex, none) as Display
import MUI.System.Display (hiding)
import MUI.System.Flexbox.FlexDirection as FlexDirection
import MUI.System.Flexbox.JustifyContent (flexEnd) as JustifyContent
import React.Basic (Component, JSX, ReactComponent, createComponent, element, fragment, make)
import React.Basic.DOM (a, button, div, div_, form, h2, p, span, text) as DOM
import React.Basic.DOM (render)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Render, useState)
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
component = createComponent "main"

-- | Based on: https://github.com/mui-org/material-ui/blob/master/docs/src/pages/components/drawers/TemporaryDrawer.js
mainComponent :: Effect (ReactComponent {})
mainComponent =
  let
    useStyles =
      makeStyles \t ->
        { paper:
            { marginTop: t.spacing 8.0
            , display: Display.flex
            , flexDirection: FlexDirection.column
            , alignItems: Grid.alignItems.center
            }
        , avatar:
            { margin: t.spacing 1.0
            , backgroundColor: t.palette.secondary.main
            }
        , form:
            { width: "100%"
            , marginTop: t.spacing 1.0
            }
        , submit:
            { marginTop: t.spacing 3.0
            , marginRight: t.spacing 0.0
            , marginLeft: t.spacing 0.0
            , marginBottom: t.spacing 2.0
            }
        }

    outlinedTextField = unsafePerformEffect $ outlinedWithStyles \_ -> {}
  in
    React.component "main" \_ -> React.do
      state /\ setState <- useState Nothing
      classes <- useStyles
      pure $ container
        $ { maxWidth: Container.maxWidth.xs, children: _ }
            [ cssBaseline
            , DOM.div
                $ { className: classes.paper, children: _ }
                    [ avatar
                        $ { className: classes.avatar }
                    , typography
                        $ { component: "h1", variant: Typography.variant.h5, children: _ }
                            [ DOM.text "Sign in" ]
                    , DOM.form
                        $ { className: classes.form, children: _ }
                            [ outlinedTextField
                                $ OutlinedTextField.props
                                    { variant: OutlinedTextField.variant.outlined
                                    , required: true
                                    , id: "email"
                                    , label: DOM.text "Email Address"
                                    , name: "email"
                                    , autoComplete: "email"
                                    , autoFocus: true
                                    , margin: OutlinedTextField.margin.normal
                                    , fullWidth: true
                                    }
                            , outlinedTextField
                                $ OutlinedTextField.props
                                    { variant: OutlinedTextField.variant.outlined
                                    , required: true
                                    , id: "password"
                                    , label: DOM.text "Password"
                                    , type: "password"
                                    , name: "password"
                                    , autoComplete: "current-password"
                                    , margin: OutlinedTextField.margin.normal
                                    , fullWidth: true
                                    }
                            , formControlLabel
                                { control:
                                    Checkbox.checkbox
                                      $ { value: "remember"
                                        , color: Checkbox.color.primary
                                        }
                                , label: DOM.text "Remember me"
                                }
                            , Button.button
                                { variant: Button.variant.contained
                                , className: classes.submit
                                , type: ButtonBase.type_.submit
                                , fullWidth: true
                                , children: [ DOM.text "Sign In" ]
                                }
                            ]
                    ]
            ]

type Components
  = { mainComponent :: ReactComponent {}
    }

app :: Components -> JSX
app components = make component { initialState: {}, render } {}
  where
  render self =
    muiThemeProvider $ { theme, children: _ }
      $ element components.mainComponent {}

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  components <- { mainComponent: _ } <$> mainComponent
  case container of
    Nothing -> throw "Container element not found."
    Just c -> render (app components) c
