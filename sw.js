/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4d5fdf43255871d7fcd0cbcbcfc4ac26"],["/about/index.html","288bc6d716d4fa3110484b57f80d5c66"],["/archives/2023/01/index.html","2ca433710cf7c72adc901db591800841"],["/archives/2023/02/index.html","fe6fe29ad64f70e183b485e9eab84b53"],["/archives/2023/02/page/2/index.html","7a68a7733de2f3f1e3063b902f79b0bd"],["/archives/2023/index.html","d30b0a481f031d479f4e80e09b407e93"],["/archives/2023/page/2/index.html","5da7ddb3b07980f869634b8ce890d0ff"],["/archives/2023/page/3/index.html","64b353b5e1f2f7990c773d6374472c33"],["/archives/index.html","07bed912e3211b6ce5f7dc75eee78d1f"],["/archives/page/2/index.html","4a88d619d90ef74604424bb699886676"],["/archives/page/3/index.html","5bcafe1f1c6a9205505b8690ddcde92f"],["/categories/Java/index.html","4e26418d847765e714d72aa67398937f"],["/categories/Java/后端/index.html","81ddf2c956ce7d768d3b180011a93e7a"],["/categories/Java/基础/index.html","2a7bbdfab629a438afd18246c5b3fe06"],["/categories/Java/基础/集合/index.html","c62b788c2109d70b94b02f180b06dedf"],["/categories/Python/index.html","7c3c717c9c7602c48e9b3d35a58196bf"],["/categories/Python/编程环境/index.html","9397e94f0b9dca32a9f0b2f488104201"],["/categories/index.html","079dfa05dd7000274125ab6c53856556"],["/categories/大数据开发/Hadoop/index.html","d9e6c23d19f90a03d3cc60c726597d23"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a39ba3598d078124c70028bae069c598"],["/categories/大数据开发/Zookeeper/index.html","9a5a28d61a4b01e8af9b71f5047c9d54"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f29b40b6b56997eeb650c73af93f4000"],["/categories/大数据开发/index.html","265ea696ba947540d95c486791f756a9"],["/categories/操作系统/Linux/index.html","607f2aca741b6226bc64b77ec8056e52"],["/categories/操作系统/Mac/index.html","bc7a2c539702d46f7a251c8f7ef2b206"],["/categories/操作系统/index.html","3bbb66dc441a085354933e111f21062d"],["/categories/数学建模/index.html","4cbf0dc92fb6ee9589b1e44b5c0367b2"],["/categories/数学建模/latex/index.html","b3863a2dc36e623a803227033cf5c3b9"],["/categories/数学建模/优化类/index.html","f4620d96df8a8e91533f0b01f50ef563"],["/categories/数学建模/优化类/现代优化算法/index.html","7764900fd2de88f71e71dd93663d1ea5"],["/categories/数学建模/优化类/规划类/index.html","45e2795ae9af0276c456db871c4b56b6"],["/categories/数据库/MySQL/index.html","84bf930f0de55211a7d21efc54ec3c9c"],["/categories/数据库/index.html","8ba95756219b320c2a91b59afab74273"],["/categories/数据结构和算法/bfs/index.html","6c33f6e73805f095786557272b75c77b"],["/categories/数据结构和算法/dfs/index.html","d00950388ebc7cea42e7e06aa30f258a"],["/categories/数据结构和算法/index.html","a883e9feb2d9be9e9c49d7453667bd03"],["/categories/数据结构和算法/前缀和/index.html","635b8ce9be00f85817266ea398bbb2f5"],["/categories/数据结构和算法/图论/index.html","3c7805902e08ea9a90091efb2e86757d"],["/categories/数据结构和算法/数论/index.html","e9380f102b8390c2b8001bb1034eafee"],["/categories/数据结构和算法/链表/index.html","bf1b432422d43dfd8c349a54dcf99f5a"],["/categories/杂七杂八/index.html","e206ebd7c2fd783ef2b946455d8d9431"],["/categories/杂七杂八/博客搭建/index.html","537af53d8f3fc67cfdab232eacf31857"],["/categories/编程环境/index.html","5e02f83e6c224724a841adb147c9aa79"],["/categories/英语学习/index.html","875b5d0788f63ecc1405bb40f01b4212"],["/categories/英语学习/英语语法/index.html","444ccf8a4860bd66365caf5a2b29c7cb"],["/comments/index.html","c6e3f25defea75e4ee380979cb9139ee"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0079da11d83f724203f6ea2a20404a14"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","18a92c95569897e198427957af80b15e"],["/movies/index.html","dec15e520ff3259fa3b3749f25b1cfa8"],["/music/index.html","d803143c5bd684db1dbca44d05f974a7"],["/page/2/index.html","af5d30d192d5be452845b2247f7c8e29"],["/page/3/index.html","6d28776252d8e871dc83320d1decb1a6"],["/page/4/index.html","c0ef7b639058b1da5bdc9f787daebca9"],["/posts/1021360842.html","cf3f0eed79dbe5179e949fda6bbeeff6"],["/posts/1120620192.html","c46e51c1b1514d6c5806f100612e0e86"],["/posts/1141628095.html","ce258e3790f028e91ec86be237dcdada"],["/posts/1168613674.html","624e9e02eeab7f583f2d4e5c949ecf42"],["/posts/1219920510.html","244f66ec79fdfcdadd195a2071961b85"],["/posts/1222166338.html","88cfbd01abfcc7a5242f0a4445c9a891"],["/posts/1259097482.html","5b18963988e642d47739e967f849a426"],["/posts/1271036369.html","6a0e2e6420bbba9f4daa8bb43bb6fdae"],["/posts/135355774.html","760b2d06748ba9fcabd5b89228b35caa"],["/posts/1375344716.html","bc067336bb916a4d47273ec3033cabd3"],["/posts/1388991698.html","adcf5113066569200e34da6e22c5ab83"],["/posts/1410315814.html","0cdfbc62a291d0cbba7dde225cea7cf0"],["/posts/1470079884.html","1183fea730966ae351abe881e6084cc1"],["/posts/1470079885.html","b30c933bb95369cb43cf87d0eef39886"],["/posts/1470079886.html","9afa2f8fdfcbd583c228ac82d654d868"],["/posts/1470079887.html","69b84499fc8063cbad5e0ffaf0799264"],["/posts/1498536549.html","5b0233f4a295b69aff7185e00c358730"],["/posts/1571776361.html","a2849b1c6d3d959bc6c1c6d354b598ac"],["/posts/1605124548.html","5392ef93fb6e3df19ffd3efdc5708d34"],["/posts/1925125395.html","8cf0239552796643260bfb52284181a0"],["/posts/1966191251.html","c006a7d14b786dc096e8a51e46a244e1"],["/posts/1987617322.html","c8e5a632ef6c4c814c12ce5a45aac3c7"],["/posts/2075104059.html","387c940bab72ac598c80239386fbc0ca"],["/posts/2087796737.html","0b1a06350633f8b7748ccb5c04bd0c87"],["/posts/2207806286.html","76e4d059e4b5b171cc1fa36f7a3bfda3"],["/posts/2364755265.html","704f340f83795f740b8e789f8d1a44b3"],["/posts/2414116852.html","087520e6d35970a8ff9b2448177b1f82"],["/posts/2495386210.html","d89bf46ca0cc92a2b74c68a230c7c4bf"],["/posts/2516528882.html","34619636eb20025f884592e05fb52e4f"],["/posts/2529807823.html","223616099ff175f2c2baaeed19db975a"],["/posts/2891591958.html","0527e80f0626a83b3039ee1c08134845"],["/posts/2909934084.html","92f8fd5e46cd419596a9e6050d24ef8b"],["/posts/3169224211.html","41dcffb4bc0e0c22122d7d4ca8c4dbcb"],["/posts/3259212833.html","727d965d7efa1cc54c7743729860d583"],["/posts/3266130344.html","c87e1f04210138aca663d98747ca15b6"],["/posts/3306641566.html","7b449ea7e9dff0a2487696e56edb9c8e"],["/posts/3312011324.html","f710307a1cae1706a612204745b5ae59"],["/posts/3402121571.html","7e4423fd1cf02f615844e7414c1600ad"],["/posts/3513711414.html","980b183f05ee1272d3d354a228536a19"],["/posts/3546711884.html","0455e6acf837a926baff1abbce98dc8c"],["/posts/3731385230.html","6ae680328d734f50c827d1762ecaea7e"],["/posts/3772089482.html","24e7aaca80dc2036bdbdb8f31c2386e3"],["/posts/4130790367.html","a0201fc1eb621c46a98362e4ccfc3b35"],["/posts/4131986683.html","59350d1ea0decf335aba5255968845ed"],["/posts/4177218757.html","6a246c2b69680ca3f630ac57525114a3"],["/posts/4192183953.html","6cf2c65ea0c2f784053b62d47277e5ad"],["/posts/4261103898.html","7fdbefd7f51f31779f79bc78cc9fb81b"],["/posts/482495853.html","bbbc41bd5b4ed70da7540cc027d0a622"],["/posts/488247922.html","20ca4626ce214f761176cdb8051c1014"],["/posts/570165348.html","6f4bc798f4714b3bed115ab857295b30"],["/posts/595890772.html","f7e0777f1c1017f7600cc778d2c9a6a8"],["/posts/694347442.html","f946dfc73589c699185f5e3666fa5740"],["/posts/707384687.html","586c7c59ad2ea7672862cb36d500ad40"],["/posts/820223701.html","fdf832dd0e17f58c380f540eabb86e94"],["/posts/88294277.html","4641d3f534f7ed8fdf3683dd1662f084"],["/posts/983786067.html","1510b41d66a70270080324a4946852c5"],["/sw-register.js","0fd1f5123b865561f7fd19273455d230"],["/tags/C/index.html","5c6fe7cb09c37fd447d7d3761b656930"],["/tags/GUI/index.html","f70baa982a4445a199767d6b6a4298ba"],["/tags/Hadoop/index.html","5fa906f0bb0482986e49c768c2ff5836"],["/tags/Java后端/index.html","30945be3e6b8bd958a18581b1db1ac86"],["/tags/Java基础/index.html","92c8494f97b6c51719320b5e05388c0c"],["/tags/Java基础/page/2/index.html","969eb6168969d0fbb3c2c7667e12fe4e"],["/tags/Linux/index.html","34ad4d7d23009dc7ccbfedae16b83223"],["/tags/Linux/page/2/index.html","926603a7715805de5ef998c843a4c140"],["/tags/Mac/index.html","2c3c8603a796a1b256dff27d4c1e4692"],["/tags/Maven/index.html","02970ed3199ef1e6804309b3e8fb5763"],["/tags/MySQL/index.html","d3201367a51d3e98dae5474548d0d0fb"],["/tags/Ubuntu/index.html","2dbea376e4c1df2ab0e00306a3ac311c"],["/tags/Zookeeper/index.html","c1761311ba1a92cf98cad8f720aeed50"],["/tags/bfs/index.html","c1c7eab7693826a73a80a5b6e3d50c62"],["/tags/dfs/index.html","8b67073b48954e61c5c353fedf145c28"],["/tags/git/index.html","c555ba435c086eb3d2e178356b85eb10"],["/tags/index.html","f1939d3324630057ea206ce2a20e5b0b"],["/tags/latex/index.html","4291338e4ff66b6f34cb33210ab891ea"],["/tags/python/index.html","4584de3fe445d75aaf73a9ec73a96773"],["/tags/优化类/index.html","1ad74a0576b83880c2c214bbae5692cc"],["/tags/前缀和/index.html","d3daec1c778b536f159acc111288b5fe"],["/tags/博客搭建/index.html","a136a336db8de39538cb4c7837afb2eb"],["/tags/图论/index.html","6da0b4c0eb0ed4e7b98b31b18b39b9c6"],["/tags/大数据/index.html","c5cf66f38342a84045bc851db76880e3"],["/tags/操作系统/index.html","266ffa2a0fc835b50080f18e34893f18"],["/tags/数学建模/index.html","5d72cf29a43e5902086bfcb07b8bc1de"],["/tags/数据库/index.html","0dd7248fbf6f49b57d854a6d6b8ffe82"],["/tags/数据结构和算法/index.html","8c74cf0f881dd739f79ecf7c13cab6e9"],["/tags/枚举类/index.html","ff53d495d7839a027afca580d705a892"],["/tags/树论/index.html","50adf420a99820b9a697826b63df5af9"],["/tags/测试/index.html","47db77aa8638057feb54a36adc0eb0c4"],["/tags/环境/index.html","1f8ae8cfdfa2853da8cb60c7699ed16a"],["/tags/环境变量/index.html","e0f51959196f84c27d49db1ff052b344"],["/tags/编程环境/index.html","7e5010c70f07f6c5f67d10785b14a2d9"],["/tags/网络编程/index.html","9b726171a6220999819adcbd969af6a7"],["/tags/英语语法/index.html","e71db7b43f84621f745d4dbd9df7e51c"],["/tags/论文/index.html","c4463196fb04beb474b009ff58d3ddcc"],["/tags/资源下载/index.html","77d72758bf396223c12eb07f51a4833c"],["/tags/链表/index.html","267bfb561dfddefdc03c8055c16988a0"],["/tags/集合/index.html","21fe5b5016ef07c79efdce4a5d6f3905"],["/tags/集群/index.html","eaa54583c34414e0b61c620ab635d401"]];
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
