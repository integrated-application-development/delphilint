const path = require("path");

module.exports = {
  entry: "./index.js",
  mode: "production",
  cache: {
    type: "filesystem"
  },
  output: {
    filename: "compiled.js",
    path: path.resolve(__dirname, "dist"),
    environment: {
      arrowFunction: false,
      const: false,
      destructuring: false,
      forOf: false,
      globalThis: false,
      module: false,
      optionalChaining: false,
      templateLiteral: false
    }
  },
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
                ["@babel/preset-env", { targets: "ie 11" }]
              ]
            }
          }
        ]
      }
    ]
  }
};