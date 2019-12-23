import '../platformAssets/runtime/fontManager';

require('./register_rn');
require('./register_components');
require('./register_addons');
require('./register_icons');

window.getCircularReplacer = () => {
    seen = new WeakSet();
    return (key, value) => {
        if (typeof value === "object" && value !== null) {
            if (seen.has(value)) {
                return;
            }
            seen.add(value);
        }
        return value;
    };
};

// remove this to get rid of event debug messages
window['renativehs_debug'] = function(x) { console.log(JSON.stringify(x, window.getCircularReplacer())); return x; };

let rootView;
window.__registerComponent = function(name,c) {
    console.log('Registering app ' + name);
    rootView = c;
};
console.log('Requiring app code');
require('./all');

console.log(window);

let App = function() { return React.createElement(rootView); }

export default App;
