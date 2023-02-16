/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","6ed02e96a4f1448e41200e3dfd64550e"],["/about/index.html","02c3ceb9fa7a64582882058d44885ffd"],["/archives/2023/01/index.html","d8e271bd9d2d447fa7959f50d415cb30"],["/archives/2023/02/index.html","77895d143d23df32154670656b77c0da"],["/archives/2023/02/page/2/index.html","64ca0815503d850ff2669876ba3f46b7"],["/archives/2023/index.html","d0cbf59cc720da2d83b8b754fead7da6"],["/archives/2023/page/2/index.html","1788451e39002b7f9fcc41f4ed1279c7"],["/archives/2023/page/3/index.html","69d8be146d4704b352f5b14ea6dd9221"],["/archives/index.html","e51eb5f9bcb7c72b489cebdc715f4d8a"],["/archives/page/2/index.html","6c68c90607fb24bec032cabf18e903aa"],["/archives/page/3/index.html","3a36302187fc00746a53b29193a68f81"],["/categories/Java/index.html","51ffca119180650fe004ec779ae213d4"],["/categories/Java/后端/index.html","ccd79413513f8a332343989d70ae6104"],["/categories/Java/基础/index.html","7c1afd21a0f4a0dbb002ea0cd3652434"],["/categories/Java/基础/集合/index.html","91202b69ad59ef46b119e2d654ee8ed8"],["/categories/Python/index.html","1f56261df6b77bb69c8fac942c0617a2"],["/categories/Python/编程环境/index.html","3910f54e908491e2eeca2d0b3f08653a"],["/categories/index.html","a3a01335645cb741601e4049fa7a0656"],["/categories/大数据开发/Hadoop/index.html","7ab5ab2a03edd68f5ac46e807cec7751"],["/categories/大数据开发/Hadoop/环境搭建/index.html","14baa379a733f86f26640342284c9452"],["/categories/大数据开发/Zookeeper/index.html","16270c486980594470b6dba2a18f4bd2"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","58391effc56ba7c73f90ee8a4decfc1a"],["/categories/大数据开发/index.html","20c95d13ba3f2c306473b9771bbebacc"],["/categories/操作系统/Linux/index.html","4b8fc18dc04c0f30ac75bdfed6afd6c7"],["/categories/操作系统/Mac/index.html","bf8f06c6e5b7bf5a0559e76f7bafb1dd"],["/categories/操作系统/index.html","f05f8e6592adfaf4e437337726c0b328"],["/categories/数学建模/index.html","de900bb07ac7533f0fff7bb40e75be50"],["/categories/数学建模/latex/index.html","6181ab9a5a20c7029f2233565b24577f"],["/categories/数学建模/优化类/index.html","644fd66ad229faa87f1833cabccd3416"],["/categories/数学建模/优化类/现代优化算法/index.html","c3a0c8496b6f940b46937ecb4ceaa33b"],["/categories/数学建模/优化类/规划类/index.html","71be6c5ef83193c0abaa4eb9f6f61de2"],["/categories/数学建模/绘图/index.html","da913ac8302931032da20a9f01e66e07"],["/categories/数据库/MySQL/index.html","ba1a235f1178123486f10af361c5806f"],["/categories/数据库/index.html","3b7dd71b787efc36fea1c8d3b162351b"],["/categories/数据结构和算法/index.html","71faf98861a2a1a7f04c5bd85b76ff5d"],["/categories/数据结构和算法/基本原理/bfs/index.html","2016b5fa176f8e7b170c1ff68a15fc94"],["/categories/数据结构和算法/基本原理/dfs/index.html","d8b1823b0e0997ddcf714168fb860505"],["/categories/数据结构和算法/基本原理/index.html","471fea26a8f22f0aeff6db52fb77a667"],["/categories/数据结构和算法/基本原理/图论/index.html","c58069367fa55ba7d94b3163460e4991"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","c09ddc1905f628e7ff59a47ed400d4c3"],["/categories/数据结构和算法/基本原理/数论/index.html","19d8c760fd8fcd1b9e3e2fbed1745009"],["/categories/数据结构和算法/基本原理/链表/index.html","644a787a92e0c1d304805d84d7e4b67c"],["/categories/数据结构和算法/算法题/index.html","6f6d8461f952bac50a638320f62ddcd5"],["/categories/数据结构和算法/算法题/二分查找/index.html","34ecbc565c380b39c72db83997da92a0"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4ab77dc3b5386e7a47efee956f4237db"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c42ed5652da7b5d2fbc37c062ebe7ab5"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8e1615fb84b027f8af90067b7953f489"],["/categories/杂七杂八/index.html","57bce125d639491e27e83ee3d749796f"],["/categories/杂七杂八/博客搭建/index.html","c7ac18aeaf495aafaedbf521423cea15"],["/categories/编程环境/index.html","e40865f5dee96c8997d576ac433fbb30"],["/categories/英语学习/index.html","79e0b036c750f9c82a6bdf96ad4c7263"],["/categories/英语学习/英语语法/index.html","678bf8431bfc16dbc90f25818a675d70"],["/comments/index.html","54c508556430d945ca7d9f8a580d30cf"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c069ce4ecb5b2a1727c27a7bec2d877d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","7bd5aeb26253dbec69fc92dce188da37"],["/movies/index.html","b2c25d40534989c5be2a34a8648fbbe8"],["/music/index.html","e57b5517df658e85a7afbdc22ae5c460"],["/page/2/index.html","147af1eb994fd22ff165e1393e63a8a1"],["/page/3/index.html","199658c53d8ab71cd82f2f725ce51730"],["/page/4/index.html","5f91300c406f200faf69be2a2737bfc3"],["/posts/1021360842.html","16930ce9266a76cda46abfc635df5cf5"],["/posts/1120620192.html","b044174275857dd84c6bd8831fa8cf29"],["/posts/1141628095.html","5d1944d450db00fe7ce60cdc7550880d"],["/posts/1168613674.html","d49ba653ab3beb21f853ceecc534129e"],["/posts/1219920510.html","e02a47c286a7e85d8186f19ff05f636c"],["/posts/1222166338.html","e71d05a444bcd78c7b8b804025abdc7c"],["/posts/1259097482.html","47cf1db55aaef66e26066fea83245821"],["/posts/1271036369.html","a33be15c8003459d9dd3e4903a612424"],["/posts/135355774.html","377750197aaa1f4f74ac904f0bd2bccb"],["/posts/1375344716.html","16369d91e9163fb3f2166f210c0acc6c"],["/posts/1388991698.html","7b41c02817fd77648b1168de9a4212da"],["/posts/1410315814.html","8f7b3d263cd9c952225dff1be266b9df"],["/posts/1470079884.html","a2b7f36e0452abf7b6e49371b86f0a3b"],["/posts/1470079885.html","d9f6856138e9e7b90ae98d013bcc5ef4"],["/posts/1470079886.html","b0fa0cab2f477eeb1a9a90563871f1a0"],["/posts/1470079887.html","ce3d7ac649181b2e6d3fb8b4c42f4f30"],["/posts/1498536549.html","b2a47d838eb240dee2679ffe7a4b9dda"],["/posts/1571776361.html","ce5ce77dfee462e3c2dc9121cd29230b"],["/posts/1605124548.html","1d2d00c44baa2861a175662c605acd41"],["/posts/1925125395.html","b27d073e88f5fe1a70abadfd202bb110"],["/posts/1966191251.html","e35fa6a0e254d4d414e4bed713410210"],["/posts/1987617322.html","d598265b7624629846703e321eb7e8fe"],["/posts/2075104059.html","b773132ae1d6e3bce76df8ae0b5412f4"],["/posts/2087796737.html","96870411dd76ec1c7b598430c1a3f5b9"],["/posts/2207806286.html","c9132ac0488d279f56901069b07ed2a1"],["/posts/2281352001.html","2840099da84ddb04a6b9c86f52888c76"],["/posts/2364755265.html","a9039e39f5198a1dd215582aea92d0c0"],["/posts/2414116852.html","6c6a2ff16d1a2233613cc76071b0af41"],["/posts/2495386210.html","1e3e8b25a32f5438ee0d1138cd229fcc"],["/posts/2516528882.html","705c597f141fab2991a5c7f81ee767a2"],["/posts/2529807823.html","90899a73334a3b532dfcfe58a31e262e"],["/posts/2891591958.html","99570a011e1e90003325d6b0b5faee21"],["/posts/2909934084.html","a794adeed16f9ca58f7933ef234ef260"],["/posts/3005926051.html","b40bae78af4b69f7df2bd5390a9e7d43"],["/posts/3169224211.html","5430ee4c24309a1feab87a34f09d2815"],["/posts/3259212833.html","696337492da4cac031bf80707d19e380"],["/posts/3266130344.html","fb73fd0d56d909bee6226ef2d636d87d"],["/posts/3306641566.html","38922ef12c8254780fd47e5881a94116"],["/posts/3312011324.html","950b1ea1028b5994c470eb75e617b29a"],["/posts/336911618.html","c97d097078790b7402a4d01c91c731cb"],["/posts/3402121571.html","1a9dad84ec19acf6e79c98535bdecf83"],["/posts/3513711414.html","0a70ad80dc178c3e89462abab1950925"],["/posts/3546711884.html","59c9ee8d81f7236aa0ac7fea83c3dcbe"],["/posts/3731385230.html","ea931b5a40e00aaae8d16774d95de1bf"],["/posts/3772089482.html","482fcc1f006a705ae75c0a7f293f08ae"],["/posts/4130790367.html","5f6a73fc7d462a676ead069ef4fba786"],["/posts/4131986683.html","38d020a4ab18973558e978ff7038eb1d"],["/posts/4177218757.html","0d38165249141ca305c639fab699431c"],["/posts/4192183953.html","d7e85e27ba721d91991eb18d6ab3f87c"],["/posts/4261103898.html","90b54c5c0e13f68c94c7a6e1cfc71efe"],["/posts/482495853.html","f295ae2a5acf14ae52e9680378ad1416"],["/posts/488247922.html","1fd37908798cd911642815013db6d392"],["/posts/570165348.html","89d14e340aaec47b1ad25abbcd497793"],["/posts/595890772.html","21d71c665657b708d6892aee814d4780"],["/posts/694347442.html","e89bed04c8b3e8ae5c73e6d3bb24e809"],["/posts/707384687.html","bb03dbee27318ebafe297cfe88ca6317"],["/posts/71180092.html","42292980d15b22130d095652299788c5"],["/posts/795397410.html","b3e6785830a3ad06b60b3ce893ced7ea"],["/posts/820223701.html","feb28811cddf5b8cf8f802b3360a3fbd"],["/posts/88294277.html","ee661ffe394d8a57a74c663da1920223"],["/posts/939963535.html","364fab7be2b9512a4056400cbed24949"],["/posts/983786067.html","fe3baff77374143c2fd40d0b984c5d52"],["/sw-register.js","b2005c240223ac88992768a96e8b26b5"],["/tags/C/index.html","1456dd92c46be42545dd2551460713d1"],["/tags/C/page/2/index.html","04f2af79b4b2f34c59d5525fd768d31b"],["/tags/GUI/index.html","8d8271a0ca463880e25a5eaf5132b360"],["/tags/Hadoop/index.html","da7954739a808a4d2cbb4ca338869352"],["/tags/Java/index.html","6637870cb16ebbe6007ef1fa8712226b"],["/tags/Java后端/index.html","163ccac9fc542a2e1e46c2cd59308031"],["/tags/Java基础/index.html","6361439a7cf5e127b5c0245b59fc051e"],["/tags/Java基础/page/2/index.html","155e8ae4d62a096591748d20dec0ff49"],["/tags/Linux/index.html","b487e1bce13c6c91f3f7e0fed1dd1c80"],["/tags/Linux/page/2/index.html","0ad30a2f8e61d534044fc660b1eac616"],["/tags/Mac/index.html","ce7615d9d086803191f91034049ea56b"],["/tags/Maven/index.html","190677cde9815e5d49e0237f47195b70"],["/tags/MySQL/index.html","50cf5c219dd9054b0c4ee078d8cb7be5"],["/tags/Python/index.html","247c33ca54e2b4d9a21588124611a9d6"],["/tags/Ubuntu/index.html","11fdf09eabaa179376c4779969d3a591"],["/tags/Zookeeper/index.html","e53bb98681f004333cdec67efdf2f9ae"],["/tags/bfs/index.html","c2aa0f4615305458b7c6857a755afe8f"],["/tags/dfs/index.html","c63a8be8c817b2ae411e7523fdf331d0"],["/tags/folium/index.html","9c37ee44d00a686458239a581af6e5d0"],["/tags/git/index.html","5754dfcacd715fbf80cb951d76067e3a"],["/tags/index.html","3a912818184e0baf72b50d7c65116f40"],["/tags/latex/index.html","6ac039bf7a842a738246ffa415f12cd3"],["/tags/二分查找/index.html","aa06437eeb1282a426a39aa336f329d2"],["/tags/优化类/index.html","493a65086a80edce615913775468a9b5"],["/tags/前缀和与差分/index.html","a758eff603e98a3e5057bf0f851788c8"],["/tags/博客搭建/index.html","854dfa65770de6611f956461b3eb0786"],["/tags/图论/index.html","7539ba28da3735516f769a6e682e0363"],["/tags/大数据/index.html","76469274e6f8430b0dae158c10ebd1a5"],["/tags/操作系统/index.html","cf97e073ee45be39e3b3d4a727bc6ced"],["/tags/数学建模/index.html","d3497d7fb4b7725a641efe6f5eb9bf6f"],["/tags/数据库/index.html","656a48fd993387a9874ff1f989964528"],["/tags/数据结构和算法/index.html","1a8b37d4e726b6faffdf250896c3efab"],["/tags/数据结构和算法/page/2/index.html","302f65deafededb025a8014559e5c1bc"],["/tags/数组和字符串/index.html","9233176cec6073795527561f815dfdce"],["/tags/枚举类/index.html","8650fa43e23c30d0f5c5ce7f4b464a2f"],["/tags/栈和队列/index.html","77242bfc5ccf5231e57591bc49feefe1"],["/tags/树论/index.html","1f316a94fa9a3a16871169b3f66eda5f"],["/tags/测试/index.html","b695e342c82d5aa5e6875a955655bac3"],["/tags/环境/index.html","8f29132253df03711b3ea8ec36d1103d"],["/tags/环境变量/index.html","4c4720e2c4dce3dd45995e72aadc959d"],["/tags/绘图/index.html","e4c1ff18fc96413f541995aca57db893"],["/tags/编程环境/index.html","70d86aaf5250c9f7d47dc5799ecdab1f"],["/tags/网络编程/index.html","5b016838944b9fe7e681a8086489df16"],["/tags/英语语法/index.html","d5c0447d98e8777cea9f02b5b0f841ec"],["/tags/论文/index.html","d59cc870c308982ab7d2217e9035cb91"],["/tags/资源下载/index.html","d25ff45411c9d5a7716ba4dcc1906200"],["/tags/链表/index.html","b37eb9b1a972c99180a92338d5f60eb1"],["/tags/集合/index.html","d31a1b7edc235bab16b8fe91cfc4187c"],["/tags/集群/index.html","8d46da67b6e3a00aa215e0001c1b22a5"]];
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
