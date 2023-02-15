/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2b70ee3f3b711470de774286df68e644"],["/about/index.html","059e84f5392f6e23c77437abd3912f0c"],["/archives/2023/01/index.html","042fbf2b9c850fb793219b7f8897b672"],["/archives/2023/02/index.html","3820c3bb39b5b1e2e530db46979facef"],["/archives/2023/02/page/2/index.html","0f39a9cf9b18a312328e714ab37a4468"],["/archives/2023/index.html","9d57ba70b921c68853ab8053469fc2fc"],["/archives/2023/page/2/index.html","dc5f602c7f4423cbb60ee9c35ca88951"],["/archives/2023/page/3/index.html","3643633e33be38cab22769eb181b7c68"],["/archives/index.html","41654dbc2040d2f41c712b84bb7d1e2e"],["/archives/page/2/index.html","96672917475e770df193bc32cb1a8ec4"],["/archives/page/3/index.html","a2b1c69edc5186f12dcb251b85175e2a"],["/categories/Java/index.html","9b59e2cb55fb03ade8afa42de877fe42"],["/categories/Java/后端/index.html","9944fc1f7f6278014fff5940ea30f8f5"],["/categories/Java/基础/index.html","ead6cf279ec493011974d1ffd8abf5ab"],["/categories/Java/基础/集合/index.html","92a880aeb1c7242d2f572b3022ed7e48"],["/categories/Python/index.html","f8eab7d70bf94a1d2f8e75e542df4c89"],["/categories/Python/编程环境/index.html","1dcfb7dafa8ebfd1e4911ea6bdd8e7f1"],["/categories/index.html","77252bf2fb5c76b398f5fc37d26b8428"],["/categories/大数据开发/Hadoop/index.html","e5d7feb19574e561092ce1af0a39fddd"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e50df11a00d8754143a92cf9fdffa10a"],["/categories/大数据开发/Zookeeper/index.html","d92a2d7c9f663b746e8e4c265402eb83"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","7aa05c6d0adfab5a06953daa0dfd843a"],["/categories/大数据开发/index.html","80373ef65c802c6d769d573d135316b8"],["/categories/操作系统/Linux/index.html","55d3e912599467d71eb07f25a6061cb2"],["/categories/操作系统/Mac/index.html","b6ce83c3a27d39ab3b1a17abcb9e56b7"],["/categories/操作系统/index.html","17a341c0e3c44ef505fc1df0aa8eca3c"],["/categories/数学建模/index.html","b40ee6af4e268d37e7b7c7a67d13eaf9"],["/categories/数学建模/latex/index.html","c46d177ea10ead002821b80333e7a7f7"],["/categories/数学建模/优化类/index.html","89287555e1a8af6d6a6eb57eaec1fe0f"],["/categories/数学建模/优化类/现代优化算法/index.html","a4bfee43fd31cbf61873cac17f23a5ab"],["/categories/数学建模/优化类/规划类/index.html","28a6a8418c67a664f7409e90dd83e8f3"],["/categories/数学建模/绘图/index.html","c97285f44e349228b1535ac8314a5ee7"],["/categories/数据库/MySQL/index.html","79effc5e5ece19650e0aa2d1680a6a9f"],["/categories/数据库/index.html","3a7c9a838b6b2390e8dd61c8a970172a"],["/categories/数据结构和算法/bfs/index.html","3be36495c07453bccc6d96b6a407c40f"],["/categories/数据结构和算法/dfs/index.html","eec52a3207f6136db2c1e1fd0c04efca"],["/categories/数据结构和算法/index.html","2d2fd67e1ff6137dc6166c2f5e5bd0b7"],["/categories/数据结构和算法/前缀和/index.html","53b87e67d05f4239ea5c446d20cecd42"],["/categories/数据结构和算法/图论/index.html","59e8ba53fb5992ae2fc0fa21c8e6e15b"],["/categories/数据结构和算法/数论/index.html","c94dbcc384abaca323cbfa6c43be04e8"],["/categories/数据结构和算法/链表/index.html","0c62b44f91ea4d5d80bd599ce950b73d"],["/categories/杂七杂八/index.html","6868cd5cdebbaf9869ba1ed07e8767e4"],["/categories/杂七杂八/博客搭建/index.html","b9ecb5f70521fd7df66b1a7c66452c1f"],["/categories/编程环境/index.html","9d34fa4aaff147cbcb4f0aa59579044b"],["/categories/英语学习/index.html","4cd8cc3518d266808dfec6ddaa8a097d"],["/categories/英语学习/英语语法/index.html","b4c836a10ca03b624e7ee73d037ff218"],["/comments/index.html","c47182ca07d4dbc92e62fb985adeb674"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","fb2cd5d838aead703844e762a86c35a3"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","fe25b0c029a05483d9e278af3a0297cd"],["/movies/index.html","36fc2575328181cabe923785ef0ee709"],["/music/index.html","7400fc207a928e3bb8759d7ca60f5bb6"],["/page/2/index.html","c138b86b17858736ffde517632d7a965"],["/page/3/index.html","178e79fdfb90ab34a62300b3ce74838f"],["/page/4/index.html","a5415670a2de84a2782f7f71099c39e4"],["/posts/1021360842.html","fabfb26d6548fd150a3c3d8e7d6a9df9"],["/posts/1120620192.html","519b1cf510c7d995f79c636ec5f7f519"],["/posts/1141628095.html","58063264fe95d67950b4ec70f0f3818b"],["/posts/1168613674.html","a4dc0e9a0b7041cc3a021471f4729e42"],["/posts/1219920510.html","ff053e41e6fd29109890b167247a3e7d"],["/posts/1222166338.html","06ab6debcff00af638805f897d628a90"],["/posts/1259097482.html","934046908e7d91c8355b9c1740fe69e4"],["/posts/1271036369.html","febb4c1168dbc8a8ec6ca4a0dbe1cf88"],["/posts/135355774.html","52e1d6efc65a465aa2858348a38d2b97"],["/posts/1375344716.html","d3a9d1a51085c01040b51ff4c6b548f9"],["/posts/1388991698.html","776cefeff6704110f7c4773e3243328b"],["/posts/1410315814.html","97a21cd5249306b2ffd378d9e334d879"],["/posts/1470079884.html","265c944c96985e869e9410e71bbe0625"],["/posts/1470079885.html","0ada209ad14b0357d0fe2c30547d30fb"],["/posts/1470079886.html","c10aac3157906fea47ac70ff1f5ef990"],["/posts/1470079887.html","14e33c389768004bb5923273f613699e"],["/posts/1498536549.html","5d558f46910051620909a5698b6c2fc8"],["/posts/1571776361.html","15426f8e4625b71a2161fbcc8f215c73"],["/posts/1605124548.html","341ded47614e3ad709756c4d1190893f"],["/posts/1925125395.html","4e3d31c3629b3c122bb1020170b875c5"],["/posts/1966191251.html","bc3f356162101d9685b5f124ee5b2b11"],["/posts/1987617322.html","4a2d80f09f32ec3b640cd281c40234e0"],["/posts/2075104059.html","f7499bad3a15a519b5a355bc9c47f6fa"],["/posts/2087796737.html","21886c9faa486c6f8ba373ec5e48749f"],["/posts/2207806286.html","9f9afa0fb1b2f114f0b40d325865fead"],["/posts/2364755265.html","2447fa7e3fb8eed7fc0b68265956f8b7"],["/posts/2414116852.html","97ff3e8ac9d894bfe04accd3a29b5868"],["/posts/2495386210.html","f73b0611704dd09b648338f438e25a1a"],["/posts/2516528882.html","7d2bcd4467284f242acb7b8c6906545f"],["/posts/2529807823.html","565e264873cebd40a62287db5a3ebc41"],["/posts/2891591958.html","3418e4af8000c2e0b2ea9aba60ba8b83"],["/posts/2909934084.html","5fa5e4b8f7e26ce48b035f01b8e6b14c"],["/posts/3169224211.html","efabba601a9a52063424ea323775a171"],["/posts/3259212833.html","0d676ddc547f28afeea34e65ec38aa2f"],["/posts/3266130344.html","b2b00d279f15e421b9f9aaa48495d75a"],["/posts/3306641566.html","10dc8e9a259e983b018aaad209cef24f"],["/posts/3312011324.html","56de70bbcef880e68d7f15ee3f3e289b"],["/posts/336911618.html","d3e03e881a95773b4ac8a60961cbd38f"],["/posts/3402121571.html","a59f2cc67651316845456755de6a7449"],["/posts/3513711414.html","ea76cd56e326abee671dc3bf3599e466"],["/posts/3546711884.html","ea348ae94f6aa00da5a0875a95d8b1b0"],["/posts/3731385230.html","c040adc379ef8a248f617e6bec6c10f7"],["/posts/3772089482.html","abb04d18ab23661b3f18702c1996b754"],["/posts/4130790367.html","71d3a2d3eba9a21b14cacff8e83404f7"],["/posts/4131986683.html","200b869c638eb50a7addf0f25cc078a9"],["/posts/4177218757.html","d0729610344065c81bc572815f59a0cd"],["/posts/4192183953.html","23c9f56f5018d1a2f0e870765cf9b022"],["/posts/4261103898.html","e9b066311b49bfb1f825bf2cbf76b15a"],["/posts/482495853.html","38edb602a79d17025f8ec167e9a6b08b"],["/posts/488247922.html","a0bea84ad5206099d8fdb6508f5f41ec"],["/posts/570165348.html","0f133f32cec156bde2e5065e2893b6e6"],["/posts/595890772.html","b8a8bc0df18715531538eed6b2e3366d"],["/posts/694347442.html","49633e9a40ac741ea01b4150c0cb3f9d"],["/posts/707384687.html","b2a770aef25cf7ebd1ad7a8b7ceff294"],["/posts/820223701.html","aa70f64774a69b2e7410706ed827bbb0"],["/posts/88294277.html","ade1963d545fbf0680b9bc66981fe7ce"],["/posts/983786067.html","f5c1440bfbac183eb01b521b4c98edba"],["/sw-register.js","9ef0948475df88be34b670311a5dd9f8"],["/tags/C/index.html","af3c68fbcdd747caefde59f7deeb4247"],["/tags/GUI/index.html","6297b266ae192815dd165c4562b6274f"],["/tags/Hadoop/index.html","19798a753e2d4bff71196e1bc222f9cb"],["/tags/Java后端/index.html","444b28a04d4aafc94ef25c24fd42a422"],["/tags/Java基础/index.html","a434a0a32b668af1b7b5103f06d69fc8"],["/tags/Java基础/page/2/index.html","3ef85e15f89a2f436e0c0abe3f6cc487"],["/tags/Linux/index.html","3a6f5ef9f6145f33f6ad872090af25fe"],["/tags/Linux/page/2/index.html","52216031372ba8e572183f6cc048c4c5"],["/tags/Mac/index.html","10f894549b6695ded64937175b998aa4"],["/tags/Maven/index.html","19b1a73ef541fc9ca9d1f6d405966e14"],["/tags/MySQL/index.html","a65260499e623e38c1de79f6add31129"],["/tags/Ubuntu/index.html","a75e7dd514992ab1f96509898337e63b"],["/tags/Zookeeper/index.html","e1dd238168e52ee4117c5c378c7a64f3"],["/tags/bfs/index.html","4628fd51a9ca352ed4eff2d239ff0b1b"],["/tags/dfs/index.html","bccd19cff0b7aa55fa95dd2fc288dcad"],["/tags/folium/index.html","54dc2f400cf13ed9b32a3ebd0fa0df56"],["/tags/git/index.html","d5799f312041170200937583e70733e0"],["/tags/index.html","e7bd8cf38eb48fc1b247909796ae3761"],["/tags/latex/index.html","9689bb9e319df821fc4362ffcf7b5c42"],["/tags/python/index.html","0f3ea2bfe1862a452eb228ce1001a306"],["/tags/优化类/index.html","2ecebad0c20e9165dc32f0898dfeca2d"],["/tags/前缀和/index.html","69a29bb812f90967125aa45b92f56b87"],["/tags/博客搭建/index.html","6f96e7134a9e6ce50274601022a344f6"],["/tags/图论/index.html","7422f17ed631c2f568a77ecf8dec894f"],["/tags/大数据/index.html","2d450194817da5d5449e826e49e8c7dd"],["/tags/操作系统/index.html","7ceead3bc62ac9cdb39371dee64cd577"],["/tags/数学建模/index.html","a63abcd6b222271f94cd8da6ae026586"],["/tags/数据库/index.html","0f505b15205e944d10c893f0804e8d26"],["/tags/数据结构和算法/index.html","0fbc6fe1a2ead0d74232bcf51e31c5bc"],["/tags/枚举类/index.html","b2ff6ec99d484a2f07680bc32a63edec"],["/tags/树论/index.html","5ca9e84061252036137b7d3397d459aa"],["/tags/测试/index.html","84ea227ccaf44fa7a259983a72a08a56"],["/tags/环境/index.html","e848daa328618b2198efb2e0a2c7d2b5"],["/tags/环境变量/index.html","f4b2f329b2479956d494b277fa8da6b3"],["/tags/绘图/index.html","4f1fb94538eddc57a3f5b320ba27a433"],["/tags/编程环境/index.html","0d3267a6171e14181a7779004a379ea3"],["/tags/网络编程/index.html","40cc70a551aef82e26c1e9b87aa024c1"],["/tags/英语语法/index.html","1a1cbe65b20163e3eda87467ee2b702e"],["/tags/论文/index.html","56df1ec4933c51915a018dc15e889989"],["/tags/资源下载/index.html","4264a290ff94f2d3917bd1e3db4e6243"],["/tags/链表/index.html","38b9aa1ed59572ea55ce816b30652e5a"],["/tags/集合/index.html","4ec997ebcb93929a638caab4286fb035"],["/tags/集群/index.html","76820c7279a0fdd3cdd1a50d008db198"]];
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
