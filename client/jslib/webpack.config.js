const path = require("path");

module.exports = {
  entry: "./index.js",
  mode: "none",
  cache: {
    type: "filesystem"
  },
  output: {
    filename: "compiled.js",
    path: path.resolve(__dirname, "dist")
  },
  target: "browserslist:ie 11",
  module: {
    rules: [
      {
        test: /\.js/i,
        exclude: /node_modules[\\/]core-js/,
        use: [
          {
            loader: "babel-loader",
            options: {
              presets: [
                ["@babel/preset-env", { targets: "ie 11", useBuiltIns: "entry", corejs: "3.34.0" }]
              ]
            }
          }
        ]
      }
    ]
  }
};