/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","df989e96a7549e8a951e3b57b5fc6429"],["/about/index.html","192d146e82b66c5149f7158bb9a82bbc"],["/archives/2023/01/index.html","642bf3641838d015c587c7048e224e2d"],["/archives/2023/02/index.html","b545044f92f43e537901f32ff15cf9ac"],["/archives/2023/02/page/2/index.html","7270253a467b01f7c121924eb2acca94"],["/archives/2023/index.html","d24b88edff5005a9a59daf676d05a8ea"],["/archives/2023/page/2/index.html","8553c08cece23b79bd8e9af4717deaba"],["/archives/2023/page/3/index.html","964232659f95f31822a7614c9c8980ea"],["/archives/index.html","8d8b47df21e195476e472d2f05937776"],["/archives/page/2/index.html","d397395217a2f76c1492802c8204ae3d"],["/archives/page/3/index.html","fc94ef69c891e3b4cc5c484ac2196e47"],["/categories/Java/index.html","c6dcf0af9cbce6b31845b673e498a9a9"],["/categories/Java/后端/index.html","fad32f7cfefd9d2d8b21bc78e9dce40c"],["/categories/Java/基础/index.html","52d96dd2fd789f9782819370c469a495"],["/categories/Java/基础/集合/index.html","074bb1661aea9e214ffaa787d110f7b8"],["/categories/Python/index.html","ff89fd8afb3b7cd084f2f1037b7278d0"],["/categories/Python/编程环境/index.html","4f23436bc460a641037c878c536eda46"],["/categories/index.html","b0dda7db03f34f2d9bf314d986313e93"],["/categories/大数据开发/Hadoop/index.html","189f64995d837d6d4de9b192c63687f0"],["/categories/大数据开发/Hadoop/环境搭建/index.html","c3fbcd5f06e1ba513b68fb1c60d35ff8"],["/categories/大数据开发/Zookeeper/index.html","b4f8fab6f8ad0b90a1acb4fb2d52caad"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f91fb4c292f9afc6d56d7fffb11f55d0"],["/categories/大数据开发/index.html","d62e9b7d258c0026bab384029a805999"],["/categories/操作系统/Linux/index.html","6b5a592e76a93e9d657a05520e3db763"],["/categories/操作系统/Mac/index.html","b087aeb18158e22d2c94f65631a3061b"],["/categories/操作系统/index.html","0390b6b8b3f832763e0209d9bc74b2bb"],["/categories/数学建模/index.html","584d98539cce57825f7d3982fce45850"],["/categories/数学建模/latex/index.html","89e2e83f174493423d5b5fb26ef1c030"],["/categories/数学建模/优化类/index.html","ab20eb32b2557bb35dc5bf7cd1224250"],["/categories/数学建模/优化类/现代优化算法/index.html","03e7eddaeabd029e1c7564fd7a05955e"],["/categories/数学建模/优化类/规划类/index.html","a56f4a3437f45ae31c487c19efe936a8"],["/categories/数学建模/绘图/index.html","88cae56615e248f5acc3b5218f673d59"],["/categories/数据库/MySQL/index.html","3dfc195c02be40b9068454b985b07a23"],["/categories/数据库/index.html","f0d2e943d708f7c08505ddd1204ad39f"],["/categories/数据结构和算法/bfs/index.html","f559b97630e06acfbe91c2c3914dc4a1"],["/categories/数据结构和算法/dfs/index.html","603c7f9b8e8160567f2b056ac7020d65"],["/categories/数据结构和算法/index.html","8ebebb3f3d1403fefa8d88d0e7cbf82a"],["/categories/数据结构和算法/前缀和/index.html","87f73e89766e581875066ce78960bfde"],["/categories/数据结构和算法/图论/index.html","da1e4d863d15696bba001a9295a57358"],["/categories/数据结构和算法/数组和字符串/index.html","7ddc673483ac0f86ce330b07da294faf"],["/categories/数据结构和算法/数论/index.html","b4b125381830490e2d623720c67e15fe"],["/categories/数据结构和算法/链表/index.html","c8cea453d0709e7c356f5587a2460cb8"],["/categories/杂七杂八/index.html","2cbb74066ffac4320117f7653a2a4624"],["/categories/杂七杂八/博客搭建/index.html","55b6ac517967d59f418baf9abed6bd41"],["/categories/编程环境/index.html","2bb04f2d43235d4d9539a6155573629f"],["/categories/英语学习/index.html","4fbdc25ec20da5ae6f602e172eacf553"],["/categories/英语学习/英语语法/index.html","5434e3c2d31c0841dfc6d4542b18d732"],["/comments/index.html","823f53a6aa287d8debcd95247f78ba68"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e1586f01b04ffe87dc8ddaf22a992411"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","66bb469308d22cdad57a6407fb56ee28"],["/movies/index.html","35ec8f09a8d00e39b4e04a5c97be1589"],["/music/index.html","37af31663d4cdda9332278bb31909db9"],["/page/2/index.html","a025aeb28a077493a8f5fe324b4068f4"],["/page/3/index.html","c1b68433aa382fe43ec3904dd90c34eb"],["/page/4/index.html","69547767a8dbde3c91f362271acbc052"],["/posts/1021360842.html","65c0efc1cb4ccbabce88fc52784218a9"],["/posts/1120620192.html","b338c625e9201e496140d9fd8a300855"],["/posts/1141628095.html","ee625746c220735f7a89ed97c26162d3"],["/posts/1168613674.html","e5e6f87808b7de3485ac6be271c8e6d2"],["/posts/1219920510.html","417c019812519aaa8a4c929a3fd41a52"],["/posts/1222166338.html","81aa9fb8ad34d911f90adc3b3a44a2b2"],["/posts/1259097482.html","94def2a096835302c2f69b6c023acfc2"],["/posts/1271036369.html","d8832fb33735c3d7177344dad68e15f1"],["/posts/135355774.html","60dc266380704c221988f5da40c52bd3"],["/posts/1375344716.html","c10e1c520d4ba7a0b28e21c642d6601e"],["/posts/1388991698.html","b2cbb75af69582904a1dad2d8e3e9aa4"],["/posts/1410315814.html","02515e9dc0d089afb1a75e31caa0f4af"],["/posts/1470079884.html","edbc1a663b2656781cd34fbea8d78f3e"],["/posts/1470079885.html","680c4395ee1cd6a58427b4405a2f78eb"],["/posts/1470079886.html","fd40a480d54b7a5d518a1a440c41ba50"],["/posts/1470079887.html","71de27117141f6194ccc96f8ab794e79"],["/posts/1498536549.html","65293b9dacd7f25110740da386c385c8"],["/posts/1571776361.html","69005bc4e11f3d97ab3a7489032439ff"],["/posts/1605124548.html","6eb6a677dbbfbd3ed63ecd31b24f1fd7"],["/posts/1925125395.html","421cb88ef7075a00238e0f0c58424be5"],["/posts/1966191251.html","f965af6c04fcf2b010ca6bcd20f423b8"],["/posts/1987617322.html","fd2341c7b43ba699c65255e90d559123"],["/posts/2075104059.html","2a793e2c1e044337c2b34a7232436fa1"],["/posts/2087796737.html","83fef7383596ac07ce9031fec1eeec66"],["/posts/2207806286.html","cc9733682ac592fbb101226f50508c1e"],["/posts/2364755265.html","6bfff26a52f3ca80e1c0c413cea8125e"],["/posts/2414116852.html","2ac18463125c88cec9486db1a2aebfba"],["/posts/2495386210.html","04b962a060c04bb2d01fd1b2804f2f05"],["/posts/2516528882.html","3f3a4e8a1950ef4d64dc87a10ebacc06"],["/posts/2529807823.html","d8019386c83e5515c57ac1f0a9835fd8"],["/posts/2891591958.html","2e70737a1aa2e6c043a2ed9b86da2df8"],["/posts/2909934084.html","36e38403089487489220446779c68356"],["/posts/3169224211.html","7b289959b2ba93cd0d73d1c90a305335"],["/posts/3259212833.html","5ae182c2c3a0fc56a7f7e2afcc024cd7"],["/posts/3266130344.html","364984bea924630d797b9078c7154ae5"],["/posts/3306641566.html","489e947a9cb2fc5c1cdb2ba5f79c6fda"],["/posts/3312011324.html","01eaa5c479338c418d89c53db57724cf"],["/posts/336911618.html","9767cef54fd79c26e0f52e64ce9aec56"],["/posts/3402121571.html","7309df86f0c1386878c9b7d65da708b6"],["/posts/3513711414.html","9233e93cfd4274711c23203ba1feb3d1"],["/posts/3546711884.html","45172f7e3e7de211017c7bade22f8516"],["/posts/3731385230.html","0d22d50283424090d75c9eebbd80ce69"],["/posts/3772089482.html","6a7dc5d2ce436605aa41d586d5af86a0"],["/posts/4130790367.html","e5aea0a6f8866dfae4802018df07ace9"],["/posts/4131986683.html","f4471f226a25ce79817b646e77517d5a"],["/posts/4177218757.html","5be0b5455b92bfc225511a4dd386e372"],["/posts/4192183953.html","ef8f7d00d3604cc26c569494262689b5"],["/posts/4261103898.html","a0e4825f45f960949cf54ef9dbcf501c"],["/posts/482495853.html","ea2033ce3311cc46f3c238f0b5080147"],["/posts/488247922.html","e0e5e89c30ca05deb231c5b40cb039a7"],["/posts/570165348.html","91623531b87688d769ac5db341e0bb80"],["/posts/595890772.html","63f86cd678684e62413061e8fda4cdc7"],["/posts/694347442.html","fe0210d32034e0efe8d373072d3fe734"],["/posts/707384687.html","715b963a85d0c76807c106a9df3241f3"],["/posts/795397410.html","b22099e62d47bdc7c10a650ea89ab36d"],["/posts/820223701.html","f51e38af5b96e8092fa2400a3a7bf960"],["/posts/88294277.html","a58a00326e3c27715d95dd0d75ae4878"],["/posts/939963535.html","401df900cd5de58ea90047d3f9ad9840"],["/posts/983786067.html","1c68be7bd8a0584fda46e2d2ed152277"],["/sw-register.js","5c6a8bdde65937c9afe26e588da991ba"],["/tags/C/index.html","330cc8622f60f43e1f6958312fcddec5"],["/tags/GUI/index.html","11b1da5a9ccd245320c201be19506f14"],["/tags/Hadoop/index.html","10a6ea26357672b9dcf8c4c21a8e06e0"],["/tags/Java后端/index.html","e0c0e42e66e4a1606e08a52509dfa7ec"],["/tags/Java基础/index.html","be25a5279e00ced073171534ffd87f19"],["/tags/Java基础/page/2/index.html","1a89718f6bc37a8aaee4c80f433de4a1"],["/tags/Linux/index.html","e91a2caae97acc24fb02ddf4d845a903"],["/tags/Linux/page/2/index.html","c147613f32d6b03c698f30025fd9396c"],["/tags/Mac/index.html","9e5411dcc861ea05deae77a90b58ae5c"],["/tags/Maven/index.html","d5c34610f0a7aa50b97917164995e34b"],["/tags/MySQL/index.html","d610d0befc767d8a377c032edc36c135"],["/tags/Ubuntu/index.html","36bd78b9deb95380c24ad5ae653a88ad"],["/tags/Zookeeper/index.html","23aa1a74b6a41af83ed8721309036e40"],["/tags/bfs/index.html","dfc79196acbc4c1d7cfad2e8884cc737"],["/tags/dfs/index.html","18c316bdee15736f5b41fe3d6d445828"],["/tags/folium/index.html","7526316ac608f78abb22655472fd136b"],["/tags/git/index.html","41038e079d1847a5d8a4425bd450ef2c"],["/tags/index.html","614d4c203df976c1e5622d8fb76aabd2"],["/tags/latex/index.html","d7d5ebf5eacc4b3a2abf1401e501fd97"],["/tags/python/index.html","4e94fb640624145ef531472b68884a00"],["/tags/优化类/index.html","4c2c491203afb22f9f6cdb09bdea2c42"],["/tags/前缀和/index.html","6044f98ee904cd35baae98ae801ca503"],["/tags/博客搭建/index.html","5c2ae90437707c8bb653cc5d5329e51b"],["/tags/图论/index.html","951d0ab72bc6c668fd29ac98d6d0f2a7"],["/tags/大数据/index.html","f9ab00bcf849330c4bbbbf33875983c4"],["/tags/操作系统/index.html","da8ab5a4724e5d29cfcf38b478d3aa1e"],["/tags/数学建模/index.html","7fed4f445d110af4cb2e5adc580f119a"],["/tags/数据库/index.html","dc8add15847dc7f2d83c33ed3c357cf0"],["/tags/数据结构和算法/index.html","d1ae0b3fd578fb4348c06dc69eb27f31"],["/tags/数组和字符串/index.html","c910deee16b02333b3d24e01e8a3c10a"],["/tags/枚举类/index.html","a69400a2e6099f6c7ed465ed9a9c5a70"],["/tags/树论/index.html","90e41d73f94a24f8d4bdfe16968945c1"],["/tags/测试/index.html","c63b4a123248cb5f27be88fd400c0ee3"],["/tags/环境/index.html","73b4ab1c87406a3c2263f194f058be92"],["/tags/环境变量/index.html","040c4a2cbabd0c7d6de9b104d6314339"],["/tags/绘图/index.html","88732f22466c8e1aca5030c3f88975c7"],["/tags/编程环境/index.html","70c058a55a7a5b781d2620de537feeab"],["/tags/网络编程/index.html","f7bdd213154d65d837d66af5860a0162"],["/tags/英语语法/index.html","367c0f6f11324a6b18e637d90836b904"],["/tags/论文/index.html","c39878cb5aa8183f63f3d5b88bf16ef3"],["/tags/资源下载/index.html","97f971d9b01839dd8f6db63661f78a78"],["/tags/链表/index.html","ec6e5f3fe6c0633af26a0f345634673b"],["/tags/集合/index.html","f861069703d31befc2855820a3bc2c57"],["/tags/集群/index.html","06007747baac649c006ea857813ef5ee"]];
var cacheName = 'sw-precache-v3--' + (self.registration ? self.registration.scope : '');
var firstRegister = 1; // 默认1是首次安装SW， 0是SW更新


var ignoreUrlParametersMatching = [/^utm_/];


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var cleanResponse = function (originalResponse) {
    // 如果没有重定向响应，不需干啥
    if (!originalResponse.redirected) {
        return Promise.resolve(originalResponse);
    }

    // Firefox 50 及以下不知处 Response.body 流, 所以我们需要读取整个body以blob形式返回。
    var bodyPromise = 'body' in originalResponse ?
        Promise.resolve(originalResponse.body) :
        originalResponse.blob();

    return bodyPromise.then(function (body) {
        // new Response() 可同时支持 stream or Blob.
        return new Response(body, {
            headers: originalResponse.headers,
            status: originalResponse.status,
            statusText: originalResponse.statusText
        });
    });
};

var createCacheKey = function (originalUrl, paramName, paramValue,
    dontCacheBustUrlsMatching) {

    // 创建一个新的URL对象，避免影响原始URL
    var url = new URL(originalUrl);

    // 如果 dontCacheBustUrlsMatching 值没有设置，或是没有匹配到，将值拼接到url.serach后
    if (!dontCacheBustUrlsMatching ||
        !(url.pathname.match(dontCacheBustUrlsMatching))) {
        url.search += (url.search ? '&' : '') +
            encodeURIComponent(paramName) + '=' + encodeURIComponent(paramValue);
    }

    return url.toString();
};

var isPathWhitelisted = function (whitelist, absoluteUrlString) {
    // 如果 whitelist 是空数组，则认为全部都在白名单内
    if (whitelist.length === 0) {
        return true;
    }

    // 否则逐个匹配正则匹配并返回
    var path = (new URL(absoluteUrlString)).pathname;
    return whitelist.some(function (whitelistedPathRegex) {
        return path.match(whitelistedPathRegex);
    });
};

var stripIgnoredUrlParameters = function (originalUrl,
    ignoreUrlParametersMatching) {
    var url = new URL(originalUrl);
    // 移除 hash; 查看 https://github.com/GoogleChrome/sw-precache/issues/290
    url.hash = '';

    url.search = url.search.slice(1) // 是否包含 '?'
        .split('&') // 分割成数组 'key=value' 的形式
        .map(function (kv) {
            return kv.split('='); // 分割每个 'key=value' 字符串成 [key, value] 形式
        })
        .filter(function (kv) {
            return ignoreUrlParametersMatching.every(function (ignoredRegex) {
                return !ignoredRegex.test(kv[0]); // 如果 key 没有匹配到任何忽略参数正则，就 Return true
            });
        })
        .map(function (kv) {
            return kv.join('='); // 重新把 [key, value] 格式转换为 'key=value' 字符串
        })
        .join('&'); // 将所有参数 'key=value' 以 '&' 拼接

    return url.toString();
};


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var hashParamName = '_sw-precache';
var urlsToCacheKeys = new Map(
    precacheConfig.map(function (item) {
        var relativeUrl = item[0];
        var hash = item[1];
        var absoluteUrl = new URL(relativeUrl, self.location);
        var cacheKey = createCacheKey(absoluteUrl, hashParamName, hash, false);
        return [absoluteUrl.toString(), cacheKey];
    })
);

function setOfCachedUrls(cache) {
    return cache.keys().then(function (requests) {
        // 如果原cacheName中没有缓存任何收，就默认是首次安装，否则认为是SW更新
        if (requests && requests.length > 0) {
            firstRegister = 0; // SW更新
        }
        return requests.map(function (request) {
            return request.url;
        });
    }).then(function (urls) {
        return new Set(urls);
    });
}

self.addEventListener('install', function (event) {
    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return setOfCachedUrls(cache).then(function (cachedUrls) {
                return Promise.all(
                    Array.from(urlsToCacheKeys.values()).map(function (cacheKey) {
                        // 如果缓存中没有匹配到cacheKey，添加进去
                        if (!cachedUrls.has(cacheKey)) {
                            var request = new Request(cacheKey, { credentials: 'same-origin' });
                            return fetch(request).then(function (response) {
                                // 只要返回200才能继续，否则直接抛错
                                if (!response.ok) {
                                    throw new Error('Request for ' + cacheKey + ' returned a ' +
                                        'response with status ' + response.status);
                                }

                                return cleanResponse(response).then(function (responseToCache) {
                                    return cache.put(cacheKey, responseToCache);
                                });
                            });
                        }
                    })
                );
            });
        })
            .then(function () {
            
            // 强制 SW 状态 installing -> activate
            return self.skipWaiting();
            
        })
    );
});

self.addEventListener('activate', function (event) {
    var setOfExpectedUrls = new Set(urlsToCacheKeys.values());

    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return cache.keys().then(function (existingRequests) {
                return Promise.all(
                    existingRequests.map(function (existingRequest) {
                        // 删除原缓存中相同键值内容
                        if (!setOfExpectedUrls.has(existingRequest.url)) {
                            return cache.delete(existingRequest);
                        }
                    })
                );
            });
        }).then(function () {
            
            return self.clients.claim();
            
        }).then(function () {
                // 如果是首次安装 SW 时, 不发送更新消息（是否是首次安装，通过指定cacheName 中是否有缓存信息判断）
                // 如果不是首次安装，则是内容有更新，需要通知页面重载更新
                if (!firstRegister) {
                    return self.clients.matchAll()
                        .then(function (clients) {
                            if (clients && clients.length) {
                                clients.forEach(function (client) {
                                    client.postMessage('sw.update');
                                })
                            }
                        })
                }
            })
    );
});



    self.addEventListener('fetch', function (event) {
        if (event.request.method === 'GET') {

            // 是否应该 event.respondWith()，需要我们逐步的判断
            // 而且也方便了后期做特殊的特殊
            var shouldRespond;


            // 首先去除已配置的忽略参数及hash
            // 查看缓存简直中是否包含该请求，包含就将shouldRespond 设为true
            var url = stripIgnoredUrlParameters(event.request.url, ignoreUrlParametersMatching);
            shouldRespond = urlsToCacheKeys.has(url);

            // 如果 shouldRespond 是 false, 我们在url后默认增加 'index.html'
            // (或者是你在配置文件中自行配置的 directoryIndex 参数值)，继续查找缓存列表
            var directoryIndex = 'index.html';
            if (!shouldRespond && directoryIndex) {
                url = addDirectoryIndex(url, directoryIndex);
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 仍是 false，检查是否是navigation
            // request， 如果是的话，判断是否能与 navigateFallbackWhitelist 正则列表匹配
            var navigateFallback = '';
            if (!shouldRespond &&
                navigateFallback &&
                (event.request.mode === 'navigate') &&
                isPathWhitelisted([], event.request.url)
            ) {
                url = new URL(navigateFallback, self.location).toString();
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 被置为 true
            // 则 event.respondWith()匹配缓存返回结果，匹配不成就直接请求.
            if (shouldRespond) {
                event.respondWith(
                    caches.open(cacheName).then(function (cache) {
                        return cache.match(urlsToCacheKeys.get(url)).then(function (response) {
                            if (response) {
                                return response;
                            }
                            throw Error('The cached response that was expected is missing.');
                        });
                    }).catch(function (e) {
                        // 如果捕获到异常错误，直接返回 fetch() 请求资源
                        console.warn('Couldn\'t serve response for "%s" from cache: %O', event.request.url, e);
                        return fetch(event.request);
                    })
                );
            }
        }
    });



// *** Start of auto-included sw-toolbox code. ***
/* 
 Copyright 2016 Google Inc. All Rights Reserved.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var t;t="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,t.toolbox=e()}}(function(){return function e(t,n,r){function o(c,s){if(!n[c]){if(!t[c]){var a="function"==typeof require&&require;if(!s&&a)return a(c,!0);if(i)return i(c,!0);var u=new Error("Cannot find module '"+c+"'");throw u.code="MODULE_NOT_FOUND",u}var f=n[c]={exports:{}};t[c][0].call(f.exports,function(e){var n=t[c][1][e];return o(n?n:e)},f,f.exports,e,t,n,r)}return n[c].exports}for(var i="function"==typeof require&&require,c=0;c<r.length;c++)o(r[c]);return o}({1:[function(e,t,n){"use strict";function r(e,t){t=t||{};var n=t.debug||m.debug;n&&console.log("[sw-toolbox] "+e)}function o(e){var t;return e&&e.cache&&(t=e.cache.name),t=t||m.cache.name,caches.open(t)}function i(e,t){t=t||{};var n=t.successResponses||m.successResponses;return fetch(e.clone()).then(function(r){return"GET"===e.method&&n.test(r.status)&&o(t).then(function(n){n.put(e,r).then(function(){var r=t.cache||m.cache;(r.maxEntries||r.maxAgeSeconds)&&r.name&&c(e,n,r)})}),r.clone()})}function c(e,t,n){var r=s.bind(null,e,t,n);d=d?d.then(r):r()}function s(e,t,n){var o=e.url,i=n.maxAgeSeconds,c=n.maxEntries,s=n.name,a=Date.now();return r("Updating LRU order for "+o+". Max entries is "+c+", max age is "+i),g.getDb(s).then(function(e){return g.setTimestampForUrl(e,o,a)}).then(function(e){return g.expireEntries(e,c,i,a)}).then(function(e){r("Successfully updated IDB.");var n=e.map(function(e){return t.delete(e)});return Promise.all(n).then(function(){r("Done with cache cleanup.")})}).catch(function(e){r(e)})}function a(e,t,n){return r("Renaming cache: ["+e+"] to ["+t+"]",n),caches.delete(t).then(function(){return Promise.all([caches.open(e),caches.open(t)]).then(function(t){var n=t[0],r=t[1];return n.keys().then(function(e){return Promise.all(e.map(function(e){return n.match(e).then(function(t){return r.put(e,t)})}))}).then(function(){return caches.delete(e)})})})}function u(e,t){return o(t).then(function(t){return t.add(e)})}function f(e,t){return o(t).then(function(t){return t.delete(e)})}function h(e){e instanceof Promise||p(e),m.preCacheItems=m.preCacheItems.concat(e)}function p(e){var t=Array.isArray(e);if(t&&e.forEach(function(e){"string"==typeof e||e instanceof Request||(t=!1)}),!t)throw new TypeError("The precache method expects either an array of strings and/or Requests or a Promise that resolves to an array of strings and/or Requests.");return e}function l(e,t,n){if(!e)return!1;if(t){var r=e.headers.get("date");if(r){var o=new Date(r);if(o.getTime()+1e3*t<n)return!1}}return!0}var d,m=e("./options"),g=e("./idb-cache-expiration");t.exports={debug:r,fetchAndCache:i,openCache:o,renameCache:a,cache:u,uncache:f,precache:h,validatePrecacheInput:p,isResponseFresh:l}},{"./idb-cache-expiration":2,"./options":4}],2:[function(e,t,n){"use strict";function r(e){return new Promise(function(t,n){var r=indexedDB.open(u+e,f);r.onupgradeneeded=function(){var e=r.result.createObjectStore(h,{keyPath:p});e.createIndex(l,l,{unique:!1})},r.onsuccess=function(){t(r.result)},r.onerror=function(){n(r.error)}})}function o(e){return e in d||(d[e]=r(e)),d[e]}function i(e,t,n){return new Promise(function(r,o){var i=e.transaction(h,"readwrite"),c=i.objectStore(h);c.put({url:t,timestamp:n}),i.oncomplete=function(){r(e)},i.onabort=function(){o(i.error)}})}function c(e,t,n){return t?new Promise(function(r,o){var i=1e3*t,c=[],s=e.transaction(h,"readwrite"),a=s.objectStore(h),u=a.index(l);u.openCursor().onsuccess=function(e){var t=e.target.result;if(t&&n-i>t.value[l]){var r=t.value[p];c.push(r),a.delete(r),t.continue()}},s.oncomplete=function(){r(c)},s.onabort=o}):Promise.resolve([])}function s(e,t){return t?new Promise(function(n,r){var o=[],i=e.transaction(h,"readwrite"),c=i.objectStore(h),s=c.index(l),a=s.count();s.count().onsuccess=function(){var e=a.result;e>t&&(s.openCursor().onsuccess=function(n){var r=n.target.result;if(r){var i=r.value[p];o.push(i),c.delete(i),e-o.length>t&&r.continue()}})},i.oncomplete=function(){n(o)},i.onabort=r}):Promise.resolve([])}function a(e,t,n,r){return c(e,n,r).then(function(n){return s(e,t).then(function(e){return n.concat(e)})})}var u="sw-toolbox-",f=1,h="store",p="url",l="timestamp",d={};t.exports={getDb:o,setTimestampForUrl:i,expireEntries:a}},{}],3:[function(e,t,n){"use strict";function r(e){var t=a.match(e.request);t?e.respondWith(t(e.request)):a.default&&"GET"===e.request.method&&0===e.request.url.indexOf("http")&&e.respondWith(a.default(e.request))}function o(e){s.debug("activate event fired");var t=u.cache.name+"$$$inactive$$$";e.waitUntil(s.renameCache(t,u.cache.name))}function i(e){return e.reduce(function(e,t){return e.concat(t)},[])}function c(e){var t=u.cache.name+"$$$inactive$$$";s.debug("install event fired"),s.debug("creating cache ["+t+"]"),e.waitUntil(s.openCache({cache:{name:t}}).then(function(e){return Promise.all(u.preCacheItems).then(i).then(s.validatePrecacheInput).then(function(t){return s.debug("preCache list: "+(t.join(", ")||"(none)")),e.addAll(t)})}))}e("serviceworker-cache-polyfill");var s=e("./helpers"),a=e("./router"),u=e("./options");t.exports={fetchListener:r,activateListener:o,installListener:c}},{"./helpers":1,"./options":4,"./router":6,"serviceworker-cache-polyfill":16}],4:[function(e,t,n){"use strict";var r;r=self.registration?self.registration.scope:self.scope||new URL("./",self.location).href,t.exports={cache:{name:"$$$toolbox-cache$$$"+r+"$$$",maxAgeSeconds:null,maxEntries:null},debug:!1,networkTimeoutSeconds:null,preCacheItems:[],successResponses:/^0|([123]\d\d)|(40[14567])|410$/}},{}],5:[function(e,t,n){"use strict";var r=new URL("./",self.location),o=r.pathname,i=e("path-to-regexp"),c=function(e,t,n,r){t instanceof RegExp?this.fullUrlRegExp=t:(0!==t.indexOf("/")&&(t=o+t),this.keys=[],this.regexp=i(t,this.keys)),this.method=e,this.options=r,this.handler=n};c.prototype.makeHandler=function(e){var t;if(this.regexp){var n=this.regexp.exec(e);t={},this.keys.forEach(function(e,r){t[e.name]=n[r+1]})}return function(e){return this.handler(e,t,this.options)}.bind(this)},t.exports=c},{"path-to-regexp":15}],6:[function(e,t,n){"use strict";function r(e){return e.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&")}var o=e("./route"),i=e("./helpers"),c=function(e,t){for(var n=e.entries(),r=n.next(),o=[];!r.done;){var i=new RegExp(r.value[0]);i.test(t)&&o.push(r.value[1]),r=n.next()}return o},s=function(){this.routes=new Map,this.routes.set(RegExp,new Map),this.default=null};["get","post","put","delete","head","any"].forEach(function(e){s.prototype[e]=function(t,n,r){return this.add(e,t,n,r)}}),s.prototype.add=function(e,t,n,c){c=c||{};var s;t instanceof RegExp?s=RegExp:(s=c.origin||self.location.origin,s=s instanceof RegExp?s.source:r(s)),e=e.toLowerCase();var a=new o(e,t,n,c);this.routes.has(s)||this.routes.set(s,new Map);var u=this.routes.get(s);u.has(e)||u.set(e,new Map);var f=u.get(e),h=a.regexp||a.fullUrlRegExp;f.has(h.source)&&i.debug('"'+t+'" resolves to same regex as existing route.'),f.set(h.source,a)},s.prototype.matchMethod=function(e,t){var n=new URL(t),r=n.origin,o=n.pathname;return this._match(e,c(this.routes,r),o)||this._match(e,[this.routes.get(RegExp)],t)},s.prototype._match=function(e,t,n){if(0===t.length)return null;for(var r=0;r<t.length;r++){var o=t[r],i=o&&o.get(e.toLowerCase());if(i){var s=c(i,n);if(s.length>0)return s[0].makeHandler(n)}}return null},s.prototype.match=function(e){return this.matchMethod(e.method,e.url)||this.matchMethod("any",e.url)},t.exports=new s},{"./helpers":1,"./route":5}],7:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache first ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(t){var r=n.cache||o.cache,c=Date.now();return i.isResponseFresh(t,r.maxAgeSeconds,c)?t:i.fetchAndCache(e,n)})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],8:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache only ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(e){var t=n.cache||o.cache,r=Date.now();if(i.isResponseFresh(e,t.maxAgeSeconds,r))return e})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],9:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: fastest ["+e.url+"]",n),new Promise(function(r,c){var s=!1,a=[],u=function(e){a.push(e.toString()),s?c(new Error('Both cache and network failed: "'+a.join('", "')+'"')):s=!0},f=function(e){e instanceof Response?r(e):u("No result returned")};o.fetchAndCache(e.clone(),n).then(f,u),i(e,t,n).then(f,u)})}var o=e("../helpers"),i=e("./cacheOnly");t.exports=r},{"../helpers":1,"./cacheOnly":8}],10:[function(e,t,n){t.exports={networkOnly:e("./networkOnly"),networkFirst:e("./networkFirst"),cacheOnly:e("./cacheOnly"),cacheFirst:e("./cacheFirst"),fastest:e("./fastest")}},{"./cacheFirst":7,"./cacheOnly":8,"./fastest":9,"./networkFirst":11,"./networkOnly":12}],11:[function(e,t,n){"use strict";function r(e,t,n){n=n||{};var r=n.successResponses||o.successResponses,c=n.networkTimeoutSeconds||o.networkTimeoutSeconds;return i.debug("Strategy: network first ["+e.url+"]",n),i.openCache(n).then(function(t){var s,a,u=[];if(c){var f=new Promise(function(r){s=setTimeout(function(){t.match(e).then(function(e){var t=n.cache||o.cache,c=Date.now(),s=t.maxAgeSeconds;i.isResponseFresh(e,s,c)&&r(e)})},1e3*c)});u.push(f)}var h=i.fetchAndCache(e,n).then(function(e){if(s&&clearTimeout(s),r.test(e.status))return e;throw i.debug("Response was an HTTP error: "+e.statusText,n),a=e,new Error("Bad response")}).catch(function(r){return i.debug("Network or response error, fallback to cache ["+e.url+"]",n),t.match(e).then(function(e){if(e)return e;if(a)return a;throw r})});return u.push(h),Promise.race(u)})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],12:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: network only ["+e.url+"]",n),fetch(e)}var o=e("../helpers");t.exports=r},{"../helpers":1}],13:[function(e,t,n){"use strict";var r=e("./options"),o=e("./router"),i=e("./helpers"),c=e("./strategies"),s=e("./listeners");i.debug("Service Worker Toolbox is loading"),self.addEventListener("install",s.installListener),self.addEventListener("activate",s.activateListener),self.addEventListener("fetch",s.fetchListener),t.exports={networkOnly:c.networkOnly,networkFirst:c.networkFirst,cacheOnly:c.cacheOnly,cacheFirst:c.cacheFirst,fastest:c.fastest,router:o,options:r,cache:i.cache,uncache:i.uncache,precache:i.precache}},{"./helpers":1,"./listeners":3,"./options":4,"./router":6,"./strategies":10}],14:[function(e,t,n){t.exports=Array.isArray||function(e){return"[object Array]"==Object.prototype.toString.call(e)}},{}],15:[function(e,t,n){function r(e,t){for(var n,r=[],o=0,i=0,c="",s=t&&t.delimiter||"/";null!=(n=x.exec(e));){var f=n[0],h=n[1],p=n.index;if(c+=e.slice(i,p),i=p+f.length,h)c+=h[1];else{var l=e[i],d=n[2],m=n[3],g=n[4],v=n[5],w=n[6],y=n[7];c&&(r.push(c),c="");var b=null!=d&&null!=l&&l!==d,E="+"===w||"*"===w,R="?"===w||"*"===w,k=n[2]||s,$=g||v;r.push({name:m||o++,prefix:d||"",delimiter:k,optional:R,repeat:E,partial:b,asterisk:!!y,pattern:$?u($):y?".*":"[^"+a(k)+"]+?"})}}return i<e.length&&(c+=e.substr(i)),c&&r.push(c),r}function o(e,t){return s(r(e,t))}function i(e){return encodeURI(e).replace(/[\/?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function c(e){return encodeURI(e).replace(/[?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function s(e){for(var t=new Array(e.length),n=0;n<e.length;n++)"object"==typeof e[n]&&(t[n]=new RegExp("^(?:"+e[n].pattern+")$"));return function(n,r){for(var o="",s=n||{},a=r||{},u=a.pretty?i:encodeURIComponent,f=0;f<e.length;f++){var h=e[f];if("string"!=typeof h){var p,l=s[h.name];if(null==l){if(h.optional){h.partial&&(o+=h.prefix);continue}throw new TypeError('Expected "'+h.name+'" to be defined')}if(v(l)){if(!h.repeat)throw new TypeError('Expected "'+h.name+'" to not repeat, but received `'+JSON.stringify(l)+"`");if(0===l.length){if(h.optional)continue;throw new TypeError('Expected "'+h.name+'" to not be empty')}for(var d=0;d<l.length;d++){if(p=u(l[d]),!t[f].test(p))throw new TypeError('Expected all "'+h.name+'" to match "'+h.pattern+'", but received `'+JSON.stringify(p)+"`");o+=(0===d?h.prefix:h.delimiter)+p}}else{if(p=h.asterisk?c(l):u(l),!t[f].test(p))throw new TypeError('Expected "'+h.name+'" to match "'+h.pattern+'", but received "'+p+'"');o+=h.prefix+p}}else o+=h}return o}}function a(e){return e.replace(/([.+*?=^!:${}()[\]|\/\\])/g,"\\$1")}function u(e){return e.replace(/([=!:$\/()])/g,"\\$1")}function f(e,t){return e.keys=t,e}function h(e){return e.sensitive?"":"i"}function p(e,t){var n=e.source.match(/\((?!\?)/g);if(n)for(var r=0;r<n.length;r++)t.push({name:r,prefix:null,delimiter:null,optional:!1,repeat:!1,partial:!1,asterisk:!1,pattern:null});return f(e,t)}function l(e,t,n){for(var r=[],o=0;o<e.length;o++)r.push(g(e[o],t,n).source);var i=new RegExp("(?:"+r.join("|")+")",h(n));return f(i,t)}function d(e,t,n){return m(r(e,n),t,n)}function m(e,t,n){v(t)||(n=t||n,t=[]),n=n||{};for(var r=n.strict,o=n.end!==!1,i="",c=0;c<e.length;c++){var s=e[c];if("string"==typeof s)i+=a(s);else{var u=a(s.prefix),p="(?:"+s.pattern+")";t.push(s),s.repeat&&(p+="(?:"+u+p+")*"),p=s.optional?s.partial?u+"("+p+")?":"(?:"+u+"("+p+"))?":u+"("+p+")",i+=p}}var l=a(n.delimiter||"/"),d=i.slice(-l.length)===l;return r||(i=(d?i.slice(0,-l.length):i)+"(?:"+l+"(?=$))?"),i+=o?"$":r&&d?"":"(?="+l+"|$)",f(new RegExp("^"+i,h(n)),t)}function g(e,t,n){return v(t)||(n=t||n,t=[]),n=n||{},e instanceof RegExp?p(e,t):v(e)?l(e,t,n):d(e,t,n)}var v=e("isarray");t.exports=g,t.exports.parse=r,t.exports.compile=o,t.exports.tokensToFunction=s,t.exports.tokensToRegExp=m;var x=new RegExp(["(\\\\.)","([\\/.])?(?:(?:\\:(\\w+)(?:\\(((?:\\\\.|[^\\\\()])+)\\))?|\\(((?:\\\\.|[^\\\\()])+)\\))([+*?])?|(\\*))"].join("|"),"g")},{isarray:14}],16:[function(e,t,n){!function(){var e=Cache.prototype.addAll,t=navigator.userAgent.match(/(Firefox|Chrome)\/(\d+\.)/);if(t)var n=t[1],r=parseInt(t[2]);e&&(!t||"Firefox"===n&&r>=46||"Chrome"===n&&r>=50)||(Cache.prototype.addAll=function(e){function t(e){this.name="NetworkError",this.code=19,this.message=e}var n=this;return t.prototype=Object.create(Error.prototype),Promise.resolve().then(function(){if(arguments.length<1)throw new TypeError;return e=e.map(function(e){return e instanceof Request?e:String(e)}),Promise.all(e.map(function(e){"string"==typeof e&&(e=new Request(e));var n=new URL(e.url).protocol;if("http:"!==n&&"https:"!==n)throw new t("Invalid scheme");return fetch(e.clone())}))}).then(function(r){if(r.some(function(e){return!e.ok}))throw new t("Incorrect response status");return Promise.all(r.map(function(t,r){return n.put(e[r],t)}))}).then(function(){})},Cache.prototype.add=function(e){return this.addAll([e])})}()},{}]},{},[13])(13)});


// *** End of auto-included sw-toolbox code. ***



// Runtime cache 配置转换后的 toolbox 代码.

toolbox.router.get("/*", toolbox.cacheFirst, {"origin":"unpkg.com"});
toolbox.router.get("/npm/*", toolbox.cacheFirst, {"origin":"cdn.jsdelivr.net"});





/* eslint-enable */
