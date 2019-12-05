{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Icons.IconButton (
    module Icons.IconButton,
    module React.Flux.Rn.Components.Text,
    module React.Flux.Rn.Components.TouchableHighlight,
    module React.Flux.Rn.Components.TouchableWithoutFeedback,
    module TextStyleProps,
    module ViewStyleProps,
    module LayoutStyleProps,
    module ShadowStyleProps,
    module TransformsStyleProps,
    Color(..), KeyboardType(..),
    OnContentSizeChange(OnContentSizeChange), DataDetectorTypes(..), KeyboardAppearance(..), OnKeyPress(OnKeyPress), DocumentSelectionState,
    ReturnKeyType(..), Selection(Selection), TextBreakStrategy(..), ClearButtonMode(..), OnScroll(OnScroll), OnSelectionChange(OnSelectionChange), AutoCapitalize(..),
    ViewProps.AccessibilityComponentTypes(..),
    ViewProps.AccessibilityLiveRegion(..),
    ViewProps.AccessibilityTraits(..),
    ViewProps.ImportantForAccessibility(..),
    ViewProps.Inset(Inset),
    ViewProps.OnLayout(OnLayout),
    ViewProps.PointerEvents(..),
    ViewProps.SyntheticTouchEvent(SyntheticTouchEvent),
    CommonProps.selectionColor,
    CommonProps.style
) where

import           Numeric.Natural
import           Prelude                        ( fmap
                                                , (.)
                                                , mempty
                                                , ($)
                                                , String
                                                )
import           React.Flux                     ( foreign_
                                                , ReactElementM
                                                )
import           React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                , props
                                                , Styles
                                                , nestedProp
                                                )
import qualified React.Flux.Rn.Props.CommonProps as CommonProps
import qualified React.Flux.Rn.Props.ViewProps   as ViewProps
import React.Flux.Rn.Components.Text
import React.Flux.Rn.Components.TouchableWithoutFeedback
import React.Flux.Rn.Components.TouchableHighlight
import           React.Flux.Rn.StyleProps.LayoutStyleProps as LayoutStyleProps
import           React.Flux.Rn.StyleProps.ShadowStyleProps as ShadowStyleProps
import           React.Flux.Rn.StyleProps.TextStyleProps as TextStyleProps
import           React.Flux.Rn.StyleProps.TransformsStyleProps as TransformsStyleProps
import           React.Flux.Rn.StyleProps.ViewStyleProps as ViewStyleProps hiding (borderBottomWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderWidth)
import           React.Flux.Rn.Types             (AutoCapitalize (..),
                                                  ClearButtonMode (..),
                                                  Color (..),
                                                  DataDetectorTypes (..),
                                                  DocumentSelectionState,
                                                  ImageSource (..),
                                                  KeyboardAppearance (..),
                                                  KeyboardType (..),
                                                  OnContentSizeChange (OnContentSizeChange),
                                                  OnKeyPress (OnKeyPress),
                                                  OnScroll (OnScroll),
                                                  OnSelectionChange (OnSelectionChange),
                                                  ReturnKeyType (..),
                                                  Selection (Selection),
                                                  TextBreakStrategy (..))

data IconButton

antDesign :: [Props IconButton handler] -> ReactElementM handler ()
antDesign = ($ mempty) . foreign_ "Icon.AntDesign" . fmap props

entypo :: [Props IconButton handler] -> ReactElementM handler ()
entypo = ($ mempty) . foreign_ "Icon.Entypo" . fmap props

evilIcons :: [Props IconButton handler] -> ReactElementM handler ()
evilIcons = ($ mempty) . foreign_ "Icon.EvilIcons" . fmap props

feather :: [Props IconButton handler] -> ReactElementM handler ()
feather = ($ mempty) . foreign_ "Icon.Feather" . fmap props

fontAwesome :: [Props IconButton handler] -> ReactElementM handler ()
fontAwesome = ($ mempty) . foreign_ "Icon.FontAwesome" . fmap props

fontAwesome5 :: [Props IconButton handler] -> ReactElementM handler ()
fontAwesome5 = ($ mempty) . foreign_ "Icon.FontAwesome5" . fmap props

fontisto :: [Props IconButton handler] -> ReactElementM handler ()
fontisto = ($ mempty) . foreign_ "Icon.Fontisto" . fmap props

foundation :: [Props IconButton handler] -> ReactElementM handler ()
foundation = ($ mempty) . foreign_ "Icon.Foundation" . fmap props

ionicons :: [Props IconButton handler] -> ReactElementM handler ()
ionicons = ($ mempty) . foreign_ "Icon.Ionicons" . fmap props

materialIcons :: [Props IconButton handler] -> ReactElementM handler ()
materialIcons = ($ mempty) . foreign_ "Icon.MaterialIcons" . fmap props

materialCommunityIcons :: [Props IconButton handler] -> ReactElementM handler ()
materialCommunityIcons = ($ mempty) . foreign_ "Icon.MaterialCommunityIcons" . fmap props

octicons :: [Props IconButton handler] -> ReactElementM handler ()
octicons = ($ mempty) . foreign_ "Icon.Octicons" . fmap props

zocial :: [Props IconButton handler] -> ReactElementM handler ()
zocial = ($ mempty) . foreign_ "Icon.Zocial" . fmap props

simpleLineIcons :: [Props IconButton handler] -> ReactElementM handler ()
simpleLineIcons = ($ mempty) . foreign_ "Icon.SimpleLineIcons" . fmap props



size :: Has c "size" => Natural -> Props c handler
size = prop "size"

iconStyle :: Has c "iconStyle" => [Styles c handler] -> Props c handler
iconStyle = nestedProp "iconStyle"

instance Has IconButton "size"
instance Has IconButton "iconStyle"


-- Functions:
-- TODO:


-- TouchableHighlightProps:

instance Has IconButton "activeOpacity"
instance Has IconButton "onHideUnderlay"
instance Has IconButton "onShowUnderlay"
--instance Has IconButton "style"
instance Has IconButton "underlayColor"
instance Has IconButton "hasTVPreferredFocus"
instance Has IconButton "tvParallaxProperties"


-- TouchableWithoutFeedbackProps:

instance Has IconButton "hitSlop"
instance Has IconButton "accessibilityComponentType"
--instance Has IconButton "accessible"
instance Has IconButton "delayLongPress"
instance Has IconButton "delayPressIn"
instance Has IconButton "delayPressOut"
--instance Has IconButton "disabled"
instance Has IconButton "accessibilityTraits"
--instance Has IconButton "onLayout"
--instance Has IconButton "onLongPress"
--instance Has IconButton "onPress"
instance Has IconButton "onPressIn"
instance Has IconButton "onPressOut"
--instance Has IconButton "pressRetentionOffset"


-- TextProps:

instance Has IconButton "selectable"
instance Has IconButton "accessible"
instance Has IconButton "ellipsizeMode"
instance Has IconButton "nativeID"
instance Has IconButton "numberOfLines"
instance Has IconButton "onLayout"
instance Has IconButton "onLongPress"
instance Has IconButton "onPress"
instance Has IconButton "pressRetentionOffset"
instance Has IconButton "allowFontScaling"
instance Has IconButton "style"
instance Has IconButton "testID"
instance Has IconButton "disabled"
instance Has IconButton "selectionColor"
instance Has IconButton "IconBreakStrategy"
instance Has IconButton "adjustsFontSizeToFit"
instance Has IconButton "minimumFontScale"
instance Has IconButton "suppressHighlighting"


-- IconStyleProps:

instance Has IconButton "IconShadowOffset"
instance Has IconButton "color"
instance Has IconButton "fontSize"
instance Has IconButton "fontStyle"
instance Has IconButton "fontWeight"
instance Has IconButton "lineHeight"
instance Has IconButton "IconAlign"
instance Has IconButton "IconDecorationLine"
instance Has IconButton "IconShadowColor"
instance Has IconButton "fontFamily"
instance Has IconButton "IconShadowRadius"
instance Has IconButton "includeFontPadding"
instance Has IconButton "IconAlignVertical"
instance Has IconButton "fontVariant"
instance Has IconButton "letterSpacing"
instance Has IconButton "IconDecorationColor"
instance Has IconButton "IconDecorationStyle"
instance Has IconButton "writingDirection"

-- ViewStyleProps:

instance Has IconButton "borderRightColor"
instance Has IconButton "backfaceVisibility"
instance Has IconButton "borderBottomColor"
instance Has IconButton "borderBottomEndRadius"
instance Has IconButton "borderBottomLeftRadius"
instance Has IconButton "borderBottomRightRadius"
instance Has IconButton "borderBottomStartRadius"
--instance Has IconButton "borderBottomWidth"
instance Has IconButton "borderColor"
instance Has IconButton "borderEndColor"
--instance Has IconButton "borderLeftWidth"
instance Has IconButton "borderRadius"
instance Has IconButton "backgroundColor"
--instance Has IconButton "borderRightWidth"
instance Has IconButton "borderStartColor"
instance Has IconButton "borderStyle"
instance Has IconButton "borderTopColor"
instance Has IconButton "borderTopEndRadius"
instance Has IconButton "borderTopLeftRadius"
instance Has IconButton "borderTopRightRadius"
instance Has IconButton "borderTopStartRadius"
--instance Has IconButton "borderTopWidth"
--instance Has IconButton "borderWidth"
instance Has IconButton "opacity"
instance Has IconButton "elevation"

-- LayoutStyleProps:

instance Has IconButton "marginHorizontal"
instance Has IconButton "alignContent"
instance Has IconButton "alignSelf"
instance Has IconButton "aspectRatio"
instance Has IconButton "borderBottomWidth"
instance Has IconButton "borderEndWidth"
instance Has IconButton "borderLeftWidth"
instance Has IconButton "borderRightWidth"
instance Has IconButton "borderStartWidth"
instance Has IconButton "borderTopWidth"
instance Has IconButton "borderWidth"
instance Has IconButton "bottom"
instance Has IconButton "display"
instance Has IconButton "end"
instance Has IconButton "flex"
instance Has IconButton "flexBasis"
instance Has IconButton "flexDirection"
instance Has IconButton "flexGrow"
instance Has IconButton "flexShrink"
instance Has IconButton "flexWrap"
instance Has IconButton "height"
instance Has IconButton "justifyContent"
instance Has IconButton "left"
instance Has IconButton "margin"
instance Has IconButton "marginBottom"
instance Has IconButton "marginEnd"
instance Has IconButton "alignItems"
instance Has IconButton "marginLeft"
instance Has IconButton "marginRight"
instance Has IconButton "marginStart"
instance Has IconButton "marginTop"
instance Has IconButton "marginVertical"
instance Has IconButton "maxHeight"
instance Has IconButton "maxWidth"
instance Has IconButton "minHeight"
instance Has IconButton "minWidth"
instance Has IconButton "overflow"
instance Has IconButton "padding"
instance Has IconButton "paddingBottom"
instance Has IconButton "paddingEnd"
instance Has IconButton "paddingHorizontal"
instance Has IconButton "paddingLeft"
instance Has IconButton "paddingRight"
instance Has IconButton "paddingStart"
instance Has IconButton "paddingTop"
instance Has IconButton "paddingVertical"
instance Has IconButton "position"
instance Has IconButton "right"
instance Has IconButton "start"
instance Has IconButton "top"
instance Has IconButton "width"
instance Has IconButton "zIndex"
instance Has IconButton "direction"

-- ShadowStyleProps:

instance Has IconButton "shadowColor"
instance Has IconButton "shadowOffset"
instance Has IconButton "shadowOpacity"
instance Has IconButton "shadowRadius"

-- TransformsStyleProps:

instance Has IconButton "transform"