/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4dde5db08450b05b5b706f1bf1d904a7"],["/about/index.html","2dd1def30731a4f24a192416d23c0224"],["/archives/2023/01/index.html","e8fecb52afd95adf21d260c830bcde86"],["/archives/2023/02/index.html","95032f22596d91376dd25d58a3f7f5c0"],["/archives/2023/02/page/2/index.html","c01d9f72f96aedcb50a87a43cd7a3504"],["/archives/2023/index.html","78dd46f640108d0c72c5218c9f43b324"],["/archives/2023/page/2/index.html","d48e9977f818bb06779ec73a4561a828"],["/archives/2023/page/3/index.html","b3fbb2cd1af039a57b19068057f63ee4"],["/archives/index.html","141b478930920e9aea97709d83ba38e1"],["/archives/page/2/index.html","6a982da0e33196085c65a148cfb028fe"],["/archives/page/3/index.html","e1542f67135bd978819a5d73c65f04de"],["/categories/Java/index.html","360e756db9dff1cec83c11c0ba074173"],["/categories/Java/后端/index.html","e8c08cf4241ef7bf999c1c9a5987d596"],["/categories/Java/基础/index.html","4889ebb886e936fc8e4b2501cb718938"],["/categories/Java/基础/集合/index.html","90e79b020906ec6d71cf14765a547578"],["/categories/Python/index.html","8311a508101990fe06dac315adb9651a"],["/categories/Python/编程环境/index.html","92670ae9b8fa1499d83d0c484f61fa53"],["/categories/index.html","a6487a431705a3e724bfafd9e2a20f5f"],["/categories/大数据开发/Hadoop/index.html","2e6645f9b5c84271eb341ef5b69f355a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","076f1be94aca17bf3f052bde3ea8b753"],["/categories/大数据开发/Zookeeper/index.html","9f3669f22cef718fa3378b16ce1d7c8a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","dc3a75683383f21b768ba85e761fdb40"],["/categories/大数据开发/index.html","a28f540a7099a2b81d0bad80a59a12ea"],["/categories/操作系统/Linux/index.html","7b38bd70cbceb54a65ab1af451f00d4d"],["/categories/操作系统/Mac/index.html","abd93e26740fb31b98864224f48b697e"],["/categories/操作系统/index.html","21c2accca58dba89201e477405c281a9"],["/categories/数学建模/index.html","be0e37a100ba8c1cf9dc4eec39e2dece"],["/categories/数学建模/latex/index.html","fa461fee9b2d6990f3521d1137bdb39c"],["/categories/数学建模/优化类/index.html","2b43d5f304cd7478a7c9f744fc3f4f5b"],["/categories/数学建模/优化类/现代优化算法/index.html","02765ae2c4ef4e5a1e1863bcda9b8606"],["/categories/数学建模/优化类/规划类/index.html","ec7e76e7cfd0a966d3294356bad9fecd"],["/categories/数学建模/绘图/index.html","0c7caf8fc098536e67cff8d7c94faccd"],["/categories/数据库/MySQL/index.html","3e9f809c4e5f3fb96b3dac42b45af4d8"],["/categories/数据库/index.html","07b33e72db988f7c5bc02b15c0a6f5b5"],["/categories/数据结构和算法/index.html","28bf3153a0c4a6f44f2b493b072339fe"],["/categories/数据结构和算法/基本原理/bfs/index.html","25b32568ff66757c73235d72b5e5db4b"],["/categories/数据结构和算法/基本原理/dfs/index.html","887033f14e5bba88466b3efb56a2fac4"],["/categories/数据结构和算法/基本原理/index.html","bfcce011211be4710ed17843d281ca1c"],["/categories/数据结构和算法/基本原理/图论/index.html","73676184a6fa9f8128bb921df7577867"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b9fa3b04e4a23eaa25e22797630988cf"],["/categories/数据结构和算法/基本原理/数论/index.html","886905e53ccb6701a72a0fe6783be5c3"],["/categories/数据结构和算法/基本原理/链表/index.html","2a6b4be6d0074d3160304425a14898d1"],["/categories/数据结构和算法/算法题/index.html","428c6928d37ba2b6b4917a9c9d5d4aef"],["/categories/数据结构和算法/算法题/二分查找/index.html","c6e6b2230a727368830d8f678200c507"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ec50cb430002c99a4d1df2ca8952726c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","92dc38cd07a1eb693feae40f20cf1230"],["/categories/数据结构和算法/算法题/栈和队列/index.html","99ea095b001ff582034744d5280d3338"],["/categories/杂七杂八/index.html","41ae9d60b8fa927a85d87c180f85ae75"],["/categories/杂七杂八/博客搭建/index.html","079039141ba17f0bff42c62d9fa21288"],["/categories/编程环境/index.html","6fa9a96aefa8522b558f8c8f474ad061"],["/categories/英语学习/index.html","1cb7bb4540b962ea210c22e505d3c8d6"],["/categories/英语学习/英语语法/index.html","a269d3d6f449d6196e8945200ba51b3e"],["/comments/index.html","fc1f8b49ab164ceb05d7bb13f1edba4d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","977b9e4ff69df9fc8413ab57ac76eed0"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","b565bb4fdd6732d4fc7595cf7a56cbc3"],["/movies/index.html","6c4d6c682abea3075fa99545bc703cab"],["/music/index.html","fafcfd38fe29e800deb151436da97232"],["/page/2/index.html","af565e2403e4a2ccb55096274a1f7d9d"],["/page/3/index.html","df61229168e19fa75a288545e3c3fb09"],["/page/4/index.html","a12cef850b7e6d622a0b153b10c02c9f"],["/posts/1021360842.html","1aeacceb3273c5671e5b560c9bac5e39"],["/posts/1120620192.html","05b21ce886eb27798dd6546cd8d54d97"],["/posts/1141628095.html","250e7042fe8aebf21e51878ad5cdc4c1"],["/posts/1168613674.html","67315d547734fbe0661292efae31882a"],["/posts/1219920510.html","5b2d302c8df8b89c6155a75bb9a4d61e"],["/posts/1222166338.html","bb64101dd2c18bc8b202e45450f54933"],["/posts/1259097482.html","b13d8f68c2a2b32a65a52e2e966f9d02"],["/posts/1271036369.html","836d40f5f31ed29a29093edfa34253ef"],["/posts/135355774.html","0edc75b8dc78856afdda4f4ee97f703c"],["/posts/1375344716.html","6462b94c752c297fb5881a6228cdb384"],["/posts/1388991698.html","ef679b1d00b4a85bca4c1ff6c02211a9"],["/posts/1410315814.html","b0d677797784809b2f7d33c29b52125e"],["/posts/1470079884.html","3bfc4277c09b919bd221f7a676cd4bac"],["/posts/1470079885.html","0a26562910f2d743d8774d274e8e88c2"],["/posts/1470079886.html","686e893ebda0ff6cbcc8b9e160d4d2e5"],["/posts/1470079887.html","1c06e4a916eda094dd58e51e27f2faeb"],["/posts/1498536549.html","c77da5ec1d8335a5dda6551da1036e56"],["/posts/1571776361.html","029e65de566c83883c14d15b40df7ab9"],["/posts/1605124548.html","821adb93153efdf53ce14cd4f97dc05d"],["/posts/1925125395.html","46918912b901d79b079cf3d789795035"],["/posts/1966191251.html","556d67ca6055cb461ed3fabb3d3a00b5"],["/posts/1987617322.html","e0bedf6a2b80f13edcd426128eef6f6c"],["/posts/2075104059.html","36203507be9ee56d641c33c697726092"],["/posts/2087796737.html","904d6dc8fd8a792c4d91e068db0b0191"],["/posts/2207806286.html","166e81cd21339003299faaa28e9a5d7b"],["/posts/2281352001.html","3a0c63cc82bced1538924948ce37cac7"],["/posts/2364755265.html","56ccc443db46de9c4bf4d428b1cfae8f"],["/posts/2414116852.html","20a15009663ac972d5bed8bb6546c018"],["/posts/2495386210.html","98285e5a313f85e11e64de367254bc89"],["/posts/2516528882.html","fd2b6a3b318522d30aa6f4e1044ea01c"],["/posts/2529807823.html","7ad459bfa894258c00b3fda68f5540b8"],["/posts/2891591958.html","994cc8a636e3298b8f430b77f12bdb6c"],["/posts/2909934084.html","89da8c24b7edb810cafa95a389ff3ec4"],["/posts/3005926051.html","857687a5262c2237ab582603251253a6"],["/posts/3169224211.html","2b8fc6df8d4e38a4920e0a0404c3ab6c"],["/posts/3259212833.html","31471f582fb3a46310ccf92644d2d4c1"],["/posts/3266130344.html","561d4b73a22668f33b21e8bd9474dd19"],["/posts/3306641566.html","1c07cf51d178c3c54a2f23650565cb2a"],["/posts/3312011324.html","bf522f8a3104326431270b5d8c2f1105"],["/posts/336911618.html","43c284e724a3c98723bf9b04b938af10"],["/posts/3402121571.html","11b4f3d33f78e6532fba1d3077f79e13"],["/posts/3513711414.html","2ae9e525ffc3f37a0388d8f3bef3bea0"],["/posts/3546711884.html","1eee2659e6c6ee923ef5810b4b22c4ce"],["/posts/3731385230.html","5fc12b36c373753c38fdbc1d9c40e2e3"],["/posts/3772089482.html","3de7773eb3dcc2081ebe3e8cc22b795d"],["/posts/4130790367.html","bb2eed3b44bc56b6262119e587d2a958"],["/posts/4131986683.html","1267f5987ae46fb25874155d1e00cce5"],["/posts/4177218757.html","f1d9f010bbb8e9b933c9165b2b5fc01f"],["/posts/4192183953.html","534ed0d5439ceeb8b34b1e845dbd6a98"],["/posts/4261103898.html","b92fb68502ac8bacb92817d6dc1ec65e"],["/posts/482495853.html","b0246407d4a3340d321a970396d1ab9d"],["/posts/488247922.html","6b4051d667ed4a2cfaba0cfe40684c23"],["/posts/570165348.html","227b4f813df895d3864ae684c9609107"],["/posts/595890772.html","7106e3905937ef25f7840308b68c524d"],["/posts/694347442.html","9ab5533dabef9968523696cc136f8d9f"],["/posts/707384687.html","19381b228790176d9fa63dc886ffa54d"],["/posts/71180092.html","6dc82afba12def528a8a4be2372e5da0"],["/posts/795397410.html","bba5079cdb88bef3828d9b7187d2b9fc"],["/posts/820223701.html","817953970d77308638ada49dd8de438b"],["/posts/88294277.html","adfe0e71d432a0ae7d922bb3ab968772"],["/posts/939963535.html","2d8a0fd2345e5598a49be83227680fa4"],["/posts/983786067.html","6e05430888a282290c7263b893f489ff"],["/sw-register.js","3dab698002e4f559b919dc6ede375ef5"],["/tags/C/index.html","0669f2262e14f6cee1042935a1a50f9e"],["/tags/C/page/2/index.html","b9b118eb62347d22194740109e9b12c9"],["/tags/GUI/index.html","4ffd7913ab68ceab887faa22a3aa8541"],["/tags/Hadoop/index.html","519c051ad6e23cfcbf2869603b34a08a"],["/tags/Java/index.html","3566223e92df6580a69e6ddefd19bba2"],["/tags/Java后端/index.html","d979c58a9b4883335f51a41f606d7c59"],["/tags/Java基础/index.html","1e21882c7fd5e7deb84fa86bc01e3cf7"],["/tags/Java基础/page/2/index.html","c8602744a991d9394b1e16e7de86eaeb"],["/tags/Linux/index.html","9d242ed3858ad2a5141aaf7da1e30d72"],["/tags/Linux/page/2/index.html","2a0578cf5441c4c59d14290c8aec5e57"],["/tags/Mac/index.html","c4352d2d2691433c5193a41286ed496d"],["/tags/Maven/index.html","82bb2224e9b0529fda783f21daa34def"],["/tags/MySQL/index.html","646a8371cad5950486da75f4f9fa09d8"],["/tags/Python/index.html","3be86ce85715e81df4b8f867324ac616"],["/tags/Ubuntu/index.html","c94fa9b29d8c3417839a67f851bc8acb"],["/tags/Zookeeper/index.html","7ed81b48de004e5037a0d91242868d35"],["/tags/bfs/index.html","7af5e0fb043d7a281377c4e1c1a87058"],["/tags/dfs/index.html","75d437b00f27b270bd29a8073f17c3f1"],["/tags/folium/index.html","d344048a267a5019fce8dab6f3d24088"],["/tags/git/index.html","4aa8757d04f7f2b89ea98889d5209543"],["/tags/index.html","540c333158595ce73240efc6f55a8719"],["/tags/latex/index.html","1ff597a3eec3fb65977cc7de09c9eb08"],["/tags/二分查找/index.html","f138b68db563e70b7c4ecb2af9ad67ab"],["/tags/优化类/index.html","7a95d5cfcd15185ca57c9ee5d23adb9b"],["/tags/前缀和与差分/index.html","d395c500c31908e1e9956fc186e51c09"],["/tags/博客搭建/index.html","08699cf5b4b8b9663e89b3fee29d6ddb"],["/tags/图论/index.html","0fbe403876390dcf73e01d346d9d3f9f"],["/tags/大数据/index.html","80a6e2b157eb60303e78392c4be1b150"],["/tags/操作系统/index.html","4fdceffd22710fc72ab4bc29ce186f1a"],["/tags/数学建模/index.html","e9e8c4c8247f6a1a4ded816802ce9498"],["/tags/数据库/index.html","7fbfd362d18dd5b457b53e0ad8c6f5fe"],["/tags/数据结构和算法/index.html","188aa75cafb7e03720e166ec8eb7cf81"],["/tags/数据结构和算法/page/2/index.html","24e787fa046a47ba13fa0ca3d49ffd6d"],["/tags/数组和字符串/index.html","2dbecc311a6e28982d98abd181bb8c9e"],["/tags/枚举类/index.html","ff7b5017dc3cd0ee43ced53a94d0e3be"],["/tags/栈和队列/index.html","f34076c041955f0674986a647515812e"],["/tags/树论/index.html","1c430c1e2bdb8019c180e497c5e3854c"],["/tags/测试/index.html","254b1817a57478e03559e6415558360c"],["/tags/环境/index.html","c9ce2b00d90a39b0bab5bd183fe93356"],["/tags/环境变量/index.html","312b2f647cfd2f01707ee4d5f0bdbc39"],["/tags/绘图/index.html","6f6e4e76896b20d642c3bc84c29454d6"],["/tags/编程环境/index.html","2a6296d416ba906f4febbe8456dd534d"],["/tags/网络编程/index.html","e59be1408c9f64b4077a99271619da04"],["/tags/英语语法/index.html","c08d148b6839f305d15e1142e3be5f77"],["/tags/论文/index.html","6bd2fce6945a5cb5a44825af23ac66b8"],["/tags/资源下载/index.html","d2df1dd9388eac660b29e0d2502e6146"],["/tags/链表/index.html","c37cf28b7b5f6d9c8bf4d0f9ea2f9552"],["/tags/集合/index.html","b7c7eba5e29bc29fdae2507e3d5ed6a2"],["/tags/集群/index.html","0e57e1c7865b1c0eff90ae98285eabea"]];
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
