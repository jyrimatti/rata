import MapView from 'react-native-maps';

import { createAppContainer, withNavigation, NavigationActions, NavigationContext } from 'react-navigation';
import { createDrawerNavigator, DrawerActions } from 'react-navigation-drawer';
import { createBottomTabNavigator, TabActions } from 'react-navigation-tabs';
import Screens from 'react-native-screens';
import { SvgUri, SvgXml } from 'react-native-svg';

import AntDesign from 'react-native-vector-icons/AntDesign';
import Entypo from 'react-native-vector-icons/Entypo';
import EvilIcons from 'react-native-vector-icons/EvilIcons';
import Feather from 'react-native-vector-icons/Feather';
import FontAwesome from 'react-native-vector-icons/FontAwesome';
import FontAwesome5 from 'react-native-vector-icons/FontAwesome5';
//import Fontisto from 'react-native-vector-icons/Fontisto';
import Foundation from 'react-native-vector-icons/Foundation';
import Ionicons from 'react-native-vector-icons/Ionicons';
import MaterialIcons from 'react-native-vector-icons/MaterialIcons';
import MaterialCommunityIcons from 'react-native-vector-icons/MaterialCommunityIcons';
import Octicons from 'react-native-vector-icons/Octicons';
import Zocial from 'react-native-vector-icons/Zocial';
import SimpleLineIcons from 'react-native-vector-icons/SimpleLineIcons';

module.exports = (function() {
  window['MapView'] = MapView;
  window['MapView.Marker'] = MapView.Marker;
  window['MapView.Callout'] = MapView.Callout;
  window['MapView.Polygon'] = MapView.Polygon;
  window['MapView.Polyline'] = MapView.Polyline; 
  window['MapView.Circle'] = MapView.Circle;
  window['MapView.Overlay'] = MapView.Overlay;
  window['MapView.Heatmap'] = MapView.Heatmap;
  window['MapView.Geojson'] = MapView.Geojson;
  window['MapView.UrlTile'] = MapView.UrlTile;
  window['MapView.LocalTile'] = MapView.LocalTile;

  window['navigation_createAppContainer'] = createAppContainer;
  window['navigation_createDrawerNavigator'] = createDrawerNavigator;
  window['navigation_createBottomTabNavigator'] = createBottomTabNavigator;
  window['navigation_NavigationActions'] = NavigationActions;
  window['navigation_NavigationContext'] = NavigationContext;
  window['navigation_DrawerActions'] = DrawerActions;
  window['navigation_TabActions'] = TabActions;
  window['navigation_withNavigation'] = withNavigation;
  
  window['Screens'] = Screens;

  window['Svg.SvgUri'] = SvgUri;
  window['Svg.SvgXml'] = SvgXml;

  window['Icon.AntDesign'] = AntDesign;
  window['Icon.Entypo'] = Entypo;
  window['Icon.EvilIcons'] = EvilIcons;
  window['Icon.Feather'] = Feather;
  window['Icon.FontAwesome'] = FontAwesome;
  window['Icon.FontAwesome5'] = FontAwesome5;
//  window['Icon.Fontisto'] = Fontisto;
  window['Icon.Foundation'] = Foundation;
  window['Icon.Ionicons'] = Ionicons;
  window['Icon.MaterialIcons'] = MaterialIcons;
  window['Icon.MaterialCommunityIcons'] = MaterialCommunityIcons;
  window['Icon.Octicons'] = Octicons;
  window['Icon.Zocial'] = Zocial;
  window['Icon.SimpleLineIcons'] = SimpleLineIcons;
})();
