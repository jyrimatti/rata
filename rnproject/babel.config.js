// const cleanAliases = require('./platforms/common/pathAliases');

module.exports = {
    retainLines: true,
    presets: ['module:metro-react-native-babel-preset'],
    plugins: [
        [
            require.resolve('babel-plugin-module-resolver'),
            {
                root: ['.'],
            },
        ],
        [
            require.resolve("babel-plugin-inline-import"),
            {
                "extensions": [".svg"]
            }
        ]
    ],
    ignore: ["**/all.js", "./src/all.js", "all.js"],
    exclude: ["**/all.js", "./src/all.js", "all.js"]
};
