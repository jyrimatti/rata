import MapView from 'react-native-maps';

import { createAppContainer, NavigationActions } from 'react-navigation';
import { createDrawerNavigator } from 'react-navigation-drawer';
import Screens from 'react-native-screens';
import { SvgUri } from 'react-native-svg';

module.exports = (function() {
  window['MapView'] = MapView;
  window['MapView.Marker'] = MapView.Marker;
  window['MapView.Circle'] = MapView.Circle;
  window['MapView.Polyline'] = MapView.Polyline;
  window['MapView.Polygon'] = MapView.Polygon;
  window['MapView.UrlTile'] = MapView.UrlTile;

  window['navigation_createAppContainer'] = createAppContainer;
  window['navigation_createDrawerNavigator'] = createDrawerNavigator;
  window['navigation_NavigationActions'] = NavigationActions;
  window['Screens'] = Screens;

  window['SvgUri'] = SvgUri;
})();
