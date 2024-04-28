const path = require('path');

module.exports = {
  entry: './src/index.js', // 你的主入口文件
  output: {
    path: path.resolve(__dirname, 'static'), // 输出文件夹
    filename: 'bundle.js' // 打包后的文件名
  },
  mode: 'development',
  module: {
    rules: [
      {
        test: /\.js$/, // 用正则匹配所有.js文件
        exclude: /node_modules/, // 排除 node_modules 目录
        use: {
          loader: 'babel-loader', // 使用 babel-loader
          options: {
            presets: ['@babel/preset-env']
          }
        }
      }
      // 你可以在这里添加更多的规则
    ]
  }
};
