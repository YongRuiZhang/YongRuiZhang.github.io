// const { createProxyMiddleware } = require('http-proxy-middleware');

// const apiProxy = createProxyMiddleware('/api', {
//   target: 'http://yongruizhang.github.io',
//   changeOrigin: true,
//   pathRewrite: {
//     '^/api/': '/', // rewrite path 将链接中的 /api/ 替换为 '/'
//   },
// });

// hexo.extend.filter.register('server_middleware', function(app){
//   // 表示以 api 开头的请求将被转发
//   app.use('/api', apiProxy);
// });
