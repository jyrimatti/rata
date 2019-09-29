import MapView from 'react-native-maps';

import { createAppContainer, withNavigation, NavigationActions, NavigationContext } from 'react-navigation';
import { createDrawerNavigator, DrawerActions } from 'react-navigation-drawer';
import Screens from 'react-native-screens';
import { SvgUri } from 'react-native-svg';

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

  window['navigation_createAppContainer'] = createAppContainer;
  window['navigation_createDrawerNavigator'] = createDrawerNavigator;
  window['navigation_NavigationActions'] = NavigationActions;
  window['navigation_NavigationContext'] = NavigationContext;
  window['navigation_DrawerActions'] = DrawerActions;
  window['navigation_withNavigation'] = withNavigation;
  
  window['Screens'] = Screens;

  window['SvgUri'] = SvgUri;
})();
