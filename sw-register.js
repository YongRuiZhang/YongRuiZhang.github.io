navigator.serviceWorker&&navigator.serviceWorker.register('/sw.js?v=20240226220417').then(function(){navigator.serviceWorker.addEventListener('message',function(a){if('sw.update'===a.data){let a=document.querySelector('meta[name=theme-color]'),b=document.createElement('div');a&&(a.content='#000'),b.innerHTML='<div><link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/Colsrch/CDN/hexo-offline-popup.css"><div class="c-message animated animated-lento slideInRight"><i class=" c-message--icon c-message--success"></i><div class="el-notification__group"><h2 class="c-message__title">\u64cd\u4f5c\u901a\u77e5</h2><div class="el-notification__content">\u5df2\u66f4\u65b0\u6700\u65b0\u7248\u672c\uff08\u5237\u65b0\u751f\u6548\uff09</div><div class="c-message--close" onclick="location.reload()">×</div></div></div></div>',document.body.appendChild(b),setTimeout(function(){document.getElementById('app-refresh').className+=' app-refresh-show'},16)}})});