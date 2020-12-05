{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Icons.Icon (
    module Icons.Icon,
    module React.Flux.Rn.Props.CommonProps,
    module React.Flux.Rn.StyleProps.TextStyleProps,
    module React.Flux.Rn.StyleProps.ViewStyleProps,
    module React.Flux.Rn.StyleProps.LayoutStyleProps,
    module React.Flux.Rn.StyleProps.ShadowStyleProps,
    module React.Flux.Rn.StyleProps.TransformsStyleProps
) where

import Numeric.Natural
import Prelude                        ( fmap
                                                , (.)
                                                , mempty
                                                , ($)
                                                , String
                                                )
import React.Flux                     ( foreign_
                                                , ReactElementM
                                                )
import React.Flux.Rn.Components.Text
import React.Flux.Rn.Properties       ( Has
                                                , Props
                                                , prop
                                                , props
                                                )
import React.Flux.Rn.Props.CommonProps (style, selectionColor, Color(..))
import React.Flux.Rn.StyleProps.LayoutStyleProps hiding (Visible, Hidden, width, height)
import React.Flux.Rn.StyleProps.ShadowStyleProps
import React.Flux.Rn.StyleProps.TextStyleProps hiding (Dashed, Dotted, Solid, None, Center, width, height)
import React.Flux.Rn.StyleProps.TransformsStyleProps
import React.Flux.Rn.StyleProps.ViewStyleProps hiding (borderBottomWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderWidth)

data Icon

antDesign :: [Props Icon handler] -> ReactElementM handler ()
antDesign = ($ mempty) . foreign_ "Icon.AntDesign" . fmap props

entypo :: [Props Icon handler] -> ReactElementM handler ()
entypo = ($ mempty) . foreign_ "Icon.Entypo" . fmap props

evilIcons :: [Props Icon handler] -> ReactElementM handler ()
evilIcons = ($ mempty) . foreign_ "Icon.EvilIcons" . fmap props

feather :: [Props Icon handler] -> ReactElementM handler ()
feather = ($ mempty) . foreign_ "Icon.Feather" . fmap props

fontAwesome :: [Props Icon handler] -> ReactElementM handler ()
fontAwesome = ($ mempty) . foreign_ "Icon.FontAwesome" . fmap props

fontAwesome5 :: [Props Icon handler] -> ReactElementM handler ()
fontAwesome5 = ($ mempty) . foreign_ "Icon.FontAwesome5" . fmap props

fontisto :: [Props Icon handler] -> ReactElementM handler ()
fontisto = ($ mempty) . foreign_ "Icon.Fontisto" . fmap props

foundation :: [Props Icon handler] -> ReactElementM handler ()
foundation = ($ mempty) . foreign_ "Icon.Foundation" . fmap props

ionicons :: [Props Icon handler] -> ReactElementM handler ()
ionicons = ($ mempty) . foreign_ "Icon.Ionicons" . fmap props

materialIcons :: [Props Icon handler] -> ReactElementM handler ()
materialIcons = ($ mempty) . foreign_ "Icon.MaterialIcons" . fmap props

materialCommunityIcons :: [Props Icon handler] -> ReactElementM handler ()
materialCommunityIcons = ($ mempty) . foreign_ "Icon.MaterialCommunityIcons" . fmap props

octicons :: [Props Icon handler] -> ReactElementM handler ()
octicons = ($ mempty) . foreign_ "Icon.Octicons" . fmap props

zocial :: [Props Icon handler] -> ReactElementM handler ()
zocial = ($ mempty) . foreign_ "Icon.Zocial" . fmap props

simpleLineIcons :: [Props Icon handler] -> ReactElementM handler ()
simpleLineIcons = ($ mempty) . foreign_ "Icon.SimpleLineIcons" . fmap props



size :: Has c "size" => Natural -> Props c handler
size = prop "size"

name :: Has c "name" => String -> Props c handler
name = prop "name"

instance Has Icon "size"
instance Has Icon "name"


-- Functions:
-- TODO:


-- TextProps:

instance Has Icon "selectable"
instance Has Icon "accessible"
instance Has Icon "ellipsizeMode"
instance Has Icon "nativeID"
instance Has Icon "numberOfLines"
instance Has Icon "onLayout"
instance Has Icon "onLongPress"
instance Has Icon "onPress"
instance Has Icon "pressRetentionOffset"
instance Has Icon "allowFontScaling"
instance Has Icon "style"
instance Has Icon "testID"
instance Has Icon "disabled"
instance Has Icon "selectionColor"
instance Has Icon "IconBreakStrategy"
instance Has Icon "adjustsFontSizeToFit"
instance Has Icon "minimumFontScale"
instance Has Icon "suppressHighlighting"


-- IconStyleProps:

instance Has Icon "IconShadowOffset"
instance Has Icon "color"
instance Has Icon "fontSize"
instance Has Icon "fontStyle"
instance Has Icon "fontWeight"
instance Has Icon "lineHeight"
instance Has Icon "IconAlign"
instance Has Icon "IconDecorationLine"
instance Has Icon "IconShadowColor"
instance Has Icon "fontFamily"
instance Has Icon "IconShadowRadius"
instance Has Icon "includeFontPadding"
instance Has Icon "IconAlignVertical"
instance Has Icon "fontVariant"
instance Has Icon "letterSpacing"
instance Has Icon "IconDecorationColor"
instance Has Icon "IconDecorationStyle"
instance Has Icon "writingDirection"

-- ViewStyleProps:

instance Has Icon "borderRightColor"
instance Has Icon "backfaceVisibility"
instance Has Icon "borderBottomColor"
instance Has Icon "borderBottomEndRadius"
instance Has Icon "borderBottomLeftRadius"
instance Has Icon "borderBottomRightRadius"
instance Has Icon "borderBottomStartRadius"
--instance Has Icon "borderBottomWidth"
instance Has Icon "borderColor"
instance Has Icon "borderEndColor"
--instance Has Icon "borderLeftWidth"
instance Has Icon "borderRadius"
instance Has Icon "backgroundColor"
--instance Has Icon "borderRightWidth"
instance Has Icon "borderStartColor"
instance Has Icon "borderStyle"
instance Has Icon "borderTopColor"
instance Has Icon "borderTopEndRadius"
instance Has Icon "borderTopLeftRadius"
instance Has Icon "borderTopRightRadius"
instance Has Icon "borderTopStartRadius"
--instance Has Icon "borderTopWidth"
--instance Has Icon "borderWidth"
instance Has Icon "opacity"
instance Has Icon "elevation"

-- LayoutStyleProps:

instance Has Icon "marginHorizontal"
instance Has Icon "alignContent"
instance Has Icon "alignSelf"
instance Has Icon "aspectRatio"
instance Has Icon "borderBottomWidth"
instance Has Icon "borderEndWidth"
instance Has Icon "borderLeftWidth"
instance Has Icon "borderRightWidth"
instance Has Icon "borderStartWidth"
instance Has Icon "borderTopWidth"
instance Has Icon "borderWidth"
instance Has Icon "bottom"
instance Has Icon "display"
instance Has Icon "end"
instance Has Icon "flex"
instance Has Icon "flexBasis"
instance Has Icon "flexDirection"
instance Has Icon "flexGrow"
instance Has Icon "flexShrink"
instance Has Icon "flexWrap"
instance Has Icon "height"
instance Has Icon "justifyContent"
instance Has Icon "left"
instance Has Icon "margin"
instance Has Icon "marginBottom"
instance Has Icon "marginEnd"
instance Has Icon "alignItems"
instance Has Icon "marginLeft"
instance Has Icon "marginRight"
instance Has Icon "marginStart"
instance Has Icon "marginTop"
instance Has Icon "marginVertical"
instance Has Icon "maxHeight"
instance Has Icon "maxWidth"
instance Has Icon "minHeight"
instance Has Icon "minWidth"
instance Has Icon "overflow"
instance Has Icon "padding"
instance Has Icon "paddingBottom"
instance Has Icon "paddingEnd"
instance Has Icon "paddingHorizontal"
instance Has Icon "paddingLeft"
instance Has Icon "paddingRight"
instance Has Icon "paddingStart"
instance Has Icon "paddingTop"
instance Has Icon "paddingVertical"
instance Has Icon "position"
instance Has Icon "right"
instance Has Icon "start"
instance Has Icon "top"
instance Has Icon "width"
instance Has Icon "zIndex"
instance Has Icon "direction"

-- ShadowStyleProps:

instance Has Icon "shadowColor"
instance Has Icon "shadowOffset"
instance Has Icon "shadowOpacity"
instance Has Icon "shadowRadius"

-- TransformsStyleProps:

instance Has Icon "transform"