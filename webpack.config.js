const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: {
    app: [
      './src/index.js',
    ],
  },
  output: {
    filename: '[name]-dist.js',
    path: path.resolve(__dirname, 'dist'),
  },
  module: {
    rules: [
      {
        test: /\.(css|scss)$/,
        use: [
          'style-loader',
          'css-loader',
        ],
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader?verbose=true&warn=true',
      },
    ],
    noParse: /\.elm$/,
  },
  plugins: [
    new HtmlWebpackPlugin({
        title: 'Space Junker',
        template: 'src/index.template.ejs',
        inject: 'body',
    }),
  ],
  devServer: {
    inline: true,
    stats: { colors: true },
  },
};
