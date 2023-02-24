/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","20a4403f49aacb34a4008aeac51c0a16"],["/about/index.html","b909e4b6c77c7910a4c787485c696890"],["/archives/2023/01/index.html","04117c9096b8690724c067336987e4fc"],["/archives/2023/02/index.html","be34e1595038fab8b1d572cc90d9e845"],["/archives/2023/02/page/2/index.html","a53fe05c2e96c431ea93ff466ccd981c"],["/archives/2023/index.html","2fd94f3f3f2f5663a9c8f3fab0966b4a"],["/archives/2023/page/2/index.html","6d4e4f52c03fbc46788d7ed70f1215b6"],["/archives/2023/page/3/index.html","ad63b4380482c400987e3160ad020b74"],["/archives/index.html","6fa762b9e421652d9bba9d2858fad685"],["/archives/page/2/index.html","44598eb31b3f9311fe60dee5906d027c"],["/archives/page/3/index.html","09352b13a5619194a12dd60482d08564"],["/categories/Java/index.html","21e17485efb22449b92929eeb68698e3"],["/categories/Java/后端/index.html","469a7ea4652d6b6c41650391e0eecd1e"],["/categories/Java/基础/index.html","fb295f06490765ed3aa44c4489e8b24f"],["/categories/Java/基础/集合/index.html","a8fb8832ebe1600af95d903f4f70247f"],["/categories/Python/index.html","5cd71da3cd7749ac231881c2fb7a7d4b"],["/categories/Python/编程环境/index.html","9784ed278a34cae1e6e8ae57355e76c6"],["/categories/index.html","9a6e35e14eccfc61ef7c0f2a2cef53a2"],["/categories/大数据开发/Hadoop/index.html","26e6e821e87357c6dd94e05bccec3407"],["/categories/大数据开发/Hadoop/环境搭建/index.html","86e0e4e1ad06e6bba61410b13ec301c4"],["/categories/大数据开发/Zookeeper/index.html","154d360ac9c685e7e436c87ef71eed78"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","319998ff6075c1cef4a1f30a006ff72f"],["/categories/大数据开发/index.html","4263ac8ca179e237fefef7c69c874dee"],["/categories/操作系统/Linux/index.html","1eb321a236a02e3747a9e5c6eaf2ac90"],["/categories/操作系统/Mac/index.html","22575e9e74a95d236de6990f27592f26"],["/categories/操作系统/index.html","7e8edbcd5253e0cb0a01d62232bb7ce1"],["/categories/数学建模/index.html","31ac87518fc84f6e87bfab76eb00498c"],["/categories/数学建模/latex/index.html","d0e018d7ba8eb32298b8fcf14a0f16b5"],["/categories/数学建模/优化类/index.html","b3fe334593a11a5246e01eb2415e6081"],["/categories/数学建模/优化类/现代优化算法/index.html","9e56279e3c4ac6f316a33e9101addbd7"],["/categories/数学建模/优化类/规划类/index.html","ed6682867a2f23d31af77a738cd52a43"],["/categories/数学建模/绘图/index.html","29bd0fd540fbbed2a98da798ad877f1c"],["/categories/数据库/MySQL/index.html","7bd7eabfe09362e2f3216a66754f9eb9"],["/categories/数据库/index.html","c128928e7d3fa45034f5cb9bdbc7005e"],["/categories/数据结构和算法/index.html","19c99215e9656171aeefe76f4c8f5dc1"],["/categories/数据结构和算法/基本原理/bfs/index.html","a8ad553ccf0ba5e7a560a77ebb1c0c12"],["/categories/数据结构和算法/基本原理/dfs/index.html","a2b754146a041315f1d4f18f2d9042ba"],["/categories/数据结构和算法/基本原理/index.html","767e1c0f08c1b3f57aab7f700eb4d655"],["/categories/数据结构和算法/基本原理/图论/index.html","fd5bbd8c93e2cc80bf71d38e9b0964d6"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d917d565f875253200045282c18fe4de"],["/categories/数据结构和算法/基本原理/数论/index.html","d966654a275cb101ba6547f9a438ece5"],["/categories/数据结构和算法/基本原理/树论/index.html","cfbc3ebce96f606a54932056e29c1a59"],["/categories/数据结构和算法/基本原理/链表/index.html","04c3375f314162f1db28d0da31ff71d3"],["/categories/数据结构和算法/算法题/index.html","06e43b74909aeddfb9d7dcf68596fd68"],["/categories/数据结构和算法/算法题/二分查找/index.html","7e4cfd68c5c1f73f7faa163ea2cef86a"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","a25ff36b7e8fde2cfc4145c242d8144b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","3142053e2c944a8be972e327e94e3b8b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","91dd00e33266b447adaeb1165d511067"],["/categories/数据结构和算法/算法题/树论/index.html","0db1ac33ddaed1a7025fdf9c47b82c96"],["/categories/杂七杂八/index.html","3baaa5418c34db8d57f73532d56c8c96"],["/categories/杂七杂八/博客搭建/index.html","471ad80ec5e8675bb2aeef8e71d2d740"],["/categories/编程环境/index.html","f01c0db2c9b9467f94b5af4551409234"],["/categories/英语学习/index.html","7d02cdbacd3a348ee2c3927cf4e7e540"],["/categories/英语学习/英语语法/index.html","573dccbe0d6a7db753886026a5aed710"],["/comments/index.html","b432bf9e7566632df677ed8e0866cb4e"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","bf47ae94cf6578b0511bdc0366198060"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","383c6918f89f43daf552054810b9d175"],["/movies/index.html","9f412d70c62cc9b407b0cca3790f3511"],["/music/index.html","e237ca332ae46a5556c8978172c236a6"],["/page/2/index.html","19aee30458ac8fa5e86a7783e96c9ac7"],["/page/3/index.html","98333b6115784e61038516085d4c267a"],["/page/4/index.html","d4e2a39a056bf52b26013e833929ca65"],["/posts/1021360842.html","647b6ba36c928698df03c0b80f1ccbd0"],["/posts/1120620192.html","11c8c67a393d01d99124c805064d17e2"],["/posts/1141628095.html","e84b0a2408120071a3632620d40cf7df"],["/posts/1168613674.html","c42a63b23e67ffd9b12eeee26ffc1d3a"],["/posts/1219920510.html","930213b771eb0ea7cfeb36e11e38f766"],["/posts/1222166338.html","424b0ae66277bff3f2c8f88e847c17ec"],["/posts/1259097482.html","76cf67d1c5ee75e2f99c38aa2aa3c26f"],["/posts/1271036369.html","7763954a9a8ed0e765916cfe48fa6539"],["/posts/135355774.html","4b33db19084e2aaec119ad0dbbbc9a7e"],["/posts/1375344716.html","029f13d3651a73bd056497c6b985246e"],["/posts/1388991698.html","81eee4a949c5692c2df07c19778bd837"],["/posts/1410315814.html","1a32c95f5da68e9b21a9e51637964906"],["/posts/1470079884.html","dac7e792fefd46331d10715c5f392a0d"],["/posts/1470079885.html","78dd748fefa966085cf12ecf09a10ebe"],["/posts/1470079886.html","119213b91e88cff9c049d87032f36f15"],["/posts/1470079887.html","4cc8e1e1cb7992b66af4a8e334b71f6e"],["/posts/1498536549.html","e5e47f78c68c11a0f6cb6ea0fbb23088"],["/posts/1571776361.html","be94bc91f39bbd8f9b4a1622cfc16fbf"],["/posts/1605124548.html","a72e9614c917e2e9475e494e19334109"],["/posts/1925125395.html","ffdf2639f4240fdddc6d98aee8c5f358"],["/posts/1966191251.html","91e22a9be6cd2c1dfba7c593926d93d4"],["/posts/1987617322.html","c173a276b25006158268e2d7f2d8f27b"],["/posts/2075104059.html","278c1eb5dc4b189a5eefc9a1a466077a"],["/posts/2087796737.html","33e3276fcf377384f3c7c025b7e58744"],["/posts/2207806286.html","89f081e1001923974e7d11f80436ef54"],["/posts/2225903441.html","b2f93d174114a936a9d89035def20bb3"],["/posts/2281352001.html","a4d6b239744a18b69c16fc4221ed8e39"],["/posts/2364755265.html","5afdb00d8cca15477704891c307e5f7f"],["/posts/2414116852.html","1294fd7b7fa2736aa7a374057b48cac2"],["/posts/2495386210.html","31852e463e87f7fec81bf65937dd92bf"],["/posts/2516528882.html","7e689db0b47cecba56015a48a766b3bc"],["/posts/2529807823.html","7962643083c081b9002869fe870fd1fc"],["/posts/2891591958.html","ed670baf7671eb63488c2a2eb65c8ba4"],["/posts/2909934084.html","80130775667ce38241aa1f14f4a355d5"],["/posts/3005926051.html","be0360c0ee9aa88b82bf8d7aa9f92697"],["/posts/3169224211.html","a24eba279709b6da76ef3c86b14f9677"],["/posts/3259212833.html","5b1e42ed827a531e9bfceb75941af59c"],["/posts/3266130344.html","2ec5d14fe84f5d321669207c90760eef"],["/posts/3306641566.html","69ccca9c63e079008a8431c64a084bc4"],["/posts/3312011324.html","b00e9b9b0aad898ad028f5f0bd2d569b"],["/posts/336911618.html","54fbdccd077932a63377b033e854123e"],["/posts/3402121571.html","e774a8d40168114b9fd4941d0ef5b2eb"],["/posts/3513711414.html","304da61a27056b91f57734394ea89675"],["/posts/3546711884.html","5e2589a03c6c333fe9b0e4c2e1ce34fa"],["/posts/3731385230.html","c6f1ce9e73b4373ae69af5ca60e20ae2"],["/posts/3772089482.html","03ee63cc4bc74cd6332fa07c0d8b0dc9"],["/posts/4130790367.html","b217450424785260b9c8e2fe93a12d29"],["/posts/4131986683.html","18de983d174dc3af78972ae88e89e10e"],["/posts/4177218757.html","314ad39b93af4376d445d1fbc87ca60e"],["/posts/4192183953.html","d145550a9bf2c9de7c8ca60b8efa515a"],["/posts/4261103898.html","1d996fa9b87faa886937d87cea7db21b"],["/posts/482495853.html","51afd711569d501f820ed999be8c7df4"],["/posts/488247922.html","4b8a57b6ec22442a37e1e1c65a0ef7cd"],["/posts/570165348.html","0de1c48b705ebf4e1efd565ac70e8fc0"],["/posts/595890772.html","2724d749f62f71caf2e88c3086eeaa86"],["/posts/694347442.html","374890c84e8db02f81e0f1006bec85ac"],["/posts/707384687.html","3bbf038ab07ce61bb188c1db1b653a92"],["/posts/71180092.html","e25a882f990482c0c94dc44a8a7be99a"],["/posts/716459272.html","b5e5c14f5a99e65fb32ff3952b5b5b34"],["/posts/795397410.html","5660f5daee2d888163fde49ba5937051"],["/posts/820223701.html","8936d93aa4e18fdff4663d0e4fbcab2d"],["/posts/88294277.html","a17b6c4f9df132791eae5e768dba531e"],["/posts/939963535.html","9a9399eb6308c5d248e4044ee2ee0268"],["/posts/983786067.html","3db11d91147053a8f461f54ecc0e59f0"],["/sw-register.js","57220630496f71d0dc7a5a43f9761c5a"],["/tags/C/index.html","4e2146fb41c644eef1fa037062236240"],["/tags/C/page/2/index.html","93c1c7eba0b023272bd275ce1f90e1e3"],["/tags/GUI/index.html","b6a3fe0eccf4be551d062db3f1ba1711"],["/tags/Hadoop/index.html","180737e363cd87c30385669d3bf053b3"],["/tags/Java/index.html","677081d819850af5ffe72d394939f5e2"],["/tags/Java后端/index.html","4637f491469336c911a2501fe198acb7"],["/tags/Java基础/index.html","4273f8e2c76e713399b6996500f1c99f"],["/tags/Java基础/page/2/index.html","9cd230e1d7c13cbde7aa3e91c18b1dd6"],["/tags/Linux/index.html","2815737f64ae5478e4068e41680fb0a9"],["/tags/Linux/page/2/index.html","35ade12e19e5730bf4069d5b4a205763"],["/tags/Mac/index.html","9141e1bf1fdf14e002e6bd4d99c668a0"],["/tags/Maven/index.html","a28d493bb9034565ae8d844c96668478"],["/tags/MySQL/index.html","ef682871e740011ff6f864eedc2f9649"],["/tags/Python/index.html","d4509b46d3641cdd00f06097b0524d22"],["/tags/Ubuntu/index.html","7d7da11ca875bc5e74fb0aecee678c12"],["/tags/Zookeeper/index.html","34fbac7da17cc49a50fa617f905181fc"],["/tags/bfs/index.html","f98df841a72622c9cdc30754115714e1"],["/tags/dfs/index.html","599eff5df71f64f684e17d830e3657d5"],["/tags/folium/index.html","4c0c9c933c2db70434e1f3d11bcbb231"],["/tags/git/index.html","dcd5fbf1642dc37bfd19d42d132e6325"],["/tags/index.html","9ad943779dc8531ad93d84386fbacd6c"],["/tags/latex/index.html","2d950fced6fa640a62ec06611494d863"],["/tags/二分查找/index.html","96989c74822a5c4d93f640866b9bdb34"],["/tags/优化类/index.html","d97538ab39a135e797370b08a9de8604"],["/tags/前缀和与差分/index.html","b5fecb7f4683dad123a345ffa7de7155"],["/tags/博客搭建/index.html","c49cf2f4c9ca13dcf14d278e0754dcff"],["/tags/图论/index.html","409087fc2731654266e3143a0fa74f38"],["/tags/大数据/index.html","b1102914b9253a1b339d31a9e470f596"],["/tags/操作系统/index.html","49712eb2a34f8193ea381ce12996f8e4"],["/tags/数学建模/index.html","d755b85a0b14cbc9d46f6fe1ae808a4f"],["/tags/数据库/index.html","80ac93dd5ef181d0e43d6970b4adfa8a"],["/tags/数据结构和算法/index.html","9a79cb882afffe1b46533ebebca90902"],["/tags/数据结构和算法/page/2/index.html","1c0cef9e8f9617167802e1aa09b6620c"],["/tags/数组和字符串/index.html","e72c4b604764bbfdf17ff5ca3b9a6a1e"],["/tags/枚举类/index.html","65fe142cf55463e16c82e4eef3aead88"],["/tags/栈和队列/index.html","8c48690588a72f012387182a34f7a1cc"],["/tags/树论/index.html","cfabc34f79821d0d822ea0e41797175e"],["/tags/测试/index.html","fb2e115db53ec1a83229f320c368b108"],["/tags/环境/index.html","27964336f866e2b7f2a3f405d8921dee"],["/tags/环境变量/index.html","11e9eb34eabc61fcbd806341ad0b2629"],["/tags/绘图/index.html","5ccbf84eea44cbae4ef00ce7472c7ba2"],["/tags/编程环境/index.html","af39bf7e625cb3a325f24901f58ca5f4"],["/tags/网络编程/index.html","12d3268d1ac2a97670458f0884036aad"],["/tags/英语语法/index.html","ec5f5c9990b9011c2e06c246c18d8920"],["/tags/论文/index.html","26b3e410afde6724f253121868b8c74f"],["/tags/资源下载/index.html","4dc94e30034208a520eec57c5a76e233"],["/tags/链表/index.html","97a9e8e920959bcc34f15c8063f43890"],["/tags/集合/index.html","22ed296fcf9cea0949449fe7fd9d6e1b"],["/tags/集群/index.html","e63f4bade8c629988708ab96893ac831"]];
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
