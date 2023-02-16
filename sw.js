/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","6f8244f1ad0b68c42dbe9f35c1253d2a"],["/about/index.html","6eae6f3430d07e953bc1606e66d17c82"],["/archives/2023/01/index.html","af896c47e6f9ef77f2adf1210011be1d"],["/archives/2023/02/index.html","1249d2b62860bec6133ced181e1b407d"],["/archives/2023/02/page/2/index.html","720a4bb80ceacc6d21d737b5924a5bc8"],["/archives/2023/index.html","a20977e69e608a6ecedd04a1af833f08"],["/archives/2023/page/2/index.html","5c7db789702d57a75052bb5af1e33218"],["/archives/2023/page/3/index.html","20f37b5c79ed46b0b4c1afa1f0ed8e47"],["/archives/index.html","a4b95f4273df6c3013402c857728166c"],["/archives/page/2/index.html","6c53adda901ec088d36f3bf255ced838"],["/archives/page/3/index.html","3a34497cb30dd3619846153bda0db782"],["/categories/Java/index.html","7f02a07cdb7a929006f9908743ce8ff1"],["/categories/Java/后端/index.html","2429852a0b2a4fb596e80beb8ceada2b"],["/categories/Java/基础/index.html","3701805b5637c52ab35ec27255a4670f"],["/categories/Java/基础/集合/index.html","65068521a55ee338848244fe680a4018"],["/categories/Python/index.html","3b964ecf140a771d75cbb35edc1f9df8"],["/categories/Python/编程环境/index.html","6a7249fc2de1204a41989c0ed85ade23"],["/categories/index.html","dfb289c435fa38d0aa0d1dea7a419d18"],["/categories/大数据开发/Hadoop/index.html","e10c61156dabe76e9b4d08c58f63a5b7"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a0e520fcf664ac4062a59fc19fb72144"],["/categories/大数据开发/Zookeeper/index.html","144657efc729b4d093bae0144d6f59c5"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","35e025981ba6912aa1dd18e58010d801"],["/categories/大数据开发/index.html","3959db3fc13ac9025cfcd28d20f40cc5"],["/categories/操作系统/Linux/index.html","ced26a84a4698959f19de8f4d60045a4"],["/categories/操作系统/Mac/index.html","09600e87a552882a8ef5d8893fda7ae1"],["/categories/操作系统/index.html","4d11dce002d57304dfd566f82f7a425a"],["/categories/数学建模/index.html","7f47689f0ba94ca81490bb29b56bb862"],["/categories/数学建模/latex/index.html","2d7b89e2947dd330af6e7786f376a966"],["/categories/数学建模/优化类/index.html","23c64e555a5a7e83c157f3bfbecb14e5"],["/categories/数学建模/优化类/现代优化算法/index.html","b391e345865000cd45558bcf393ac750"],["/categories/数学建模/优化类/规划类/index.html","e27cd6e0e1434853f39099db873c271f"],["/categories/数学建模/绘图/index.html","02105fb768445b9fdadc595d11adc302"],["/categories/数据库/MySQL/index.html","3e6008a2e498e6bd12cb987de727827d"],["/categories/数据库/index.html","7f6bd5dae5006fde44f53c5eede57952"],["/categories/数据结构和算法/bfs/index.html","559502bbab376b90eece021c36091129"],["/categories/数据结构和算法/dfs/index.html","6c7843d95903c8b1d46062b6e80035a4"],["/categories/数据结构和算法/index.html","1377a42434048b2a8862c4fbcd0f09f5"],["/categories/数据结构和算法/二分查找/index.html","060e97f3cf760f6f9364067e09bec28c"],["/categories/数据结构和算法/前缀和/index.html","daf6ecf744db27c4ca76b9d70bad72c5"],["/categories/数据结构和算法/图论/index.html","dbcffe7e97bcd1c5f579686970811241"],["/categories/数据结构和算法/数组和字符串/index.html","ada34b38644b9c392d9274669348b4e1"],["/categories/数据结构和算法/数论/index.html","df307f7a50dc220f16180ff4ea250799"],["/categories/数据结构和算法/链表/index.html","bd989064a9179e8cb768b7311d22534e"],["/categories/杂七杂八/index.html","19ff6b91b8a61cd14519a19970942948"],["/categories/杂七杂八/博客搭建/index.html","616ccf5b2d5fc231a4365036984db895"],["/categories/编程环境/index.html","80d288f45dec0f079de43d10d34e3463"],["/categories/英语学习/index.html","3eacb1677eb2e6d6053cb3a7b524b557"],["/categories/英语学习/英语语法/index.html","7fba6ef275ffe97222ec2ed4d3b309b8"],["/comments/index.html","85ff9e56beccb0bc245f2bfc4bd7aa05"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","6ba71cb7a5778834a46944df6c784297"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","826fc40a06d6ff0e4ba6c6890ac87da9"],["/movies/index.html","3fa0cf54ed0a9874ab0df5aabcc29a8c"],["/music/index.html","360b5cecda0c6daab81f63d8e86ba11f"],["/page/2/index.html","7769594d0f96a8db63b902217264eef3"],["/page/3/index.html","0a2cbd3ac68e0f0a8c2e6107bb281a88"],["/page/4/index.html","be1a66871a3f11e8c9e4090e7abf804b"],["/posts/1021360842.html","77ddfa7db07937691a9ebacf4f160ad2"],["/posts/1120620192.html","fb80d6da975888b406e57b821919907d"],["/posts/1141628095.html","f58490aeea567a17502a3d67e9ac320d"],["/posts/1168613674.html","5eaf98d9e5a5b757c8f2712690b4d319"],["/posts/1219920510.html","1352426162bb8d65efcd9fa3879250df"],["/posts/1222166338.html","6aeab67cfda29259035a82b597a75ee6"],["/posts/1259097482.html","92519d99065ad0cef48f6eca9c09e280"],["/posts/1271036369.html","3a76e5a3dabfcfe7c62f0d7409d2ba99"],["/posts/135355774.html","7f559142edf4a12ae14373971195a2c2"],["/posts/1375344716.html","0893a565f4def3b00b2c81a924f69b42"],["/posts/1388991698.html","3f987490d41bac2b173249ba539a232e"],["/posts/1410315814.html","438c470454004c23d2ce7b9fc9e7f0ba"],["/posts/1470079884.html","c57e6716ca1cc690dc187f0c9d9d56f9"],["/posts/1470079885.html","e115c9c1c40bcd4d99f899991732ddc4"],["/posts/1470079886.html","87c9a9b66e8a360b7d6897f81529bed9"],["/posts/1470079887.html","9ced153829ce1bf2b190baa464cea1f7"],["/posts/1498536549.html","8a98c3a09bfadfdb18718030d7c9593b"],["/posts/1571776361.html","b1b83822cf5d99aad280372849af11b6"],["/posts/1605124548.html","d23c88f0d572426de6153f920c80182e"],["/posts/1925125395.html","07fe411ef4c90b888351b418a9cea3b6"],["/posts/1966191251.html","2524021a1da5ebfd798313550c158636"],["/posts/1987617322.html","ce307be8ab87fe99be4cf807d49296bf"],["/posts/2075104059.html","921d9c0cf5f050b873bb8d069c387812"],["/posts/2087796737.html","ab3a01ab499e79ca358c565f9df1fb06"],["/posts/2207806286.html","83fcc0e8fb600e0ec2e3dd409e9c6209"],["/posts/2281352001.html","028c1c7b5c0947c18bfae76d742400c2"],["/posts/2364755265.html","2b0b5b562ac747b75b7f1f0c06081894"],["/posts/2414116852.html","c2cfc384208b58b2213cc5f78e54d17d"],["/posts/2495386210.html","22135fcf113765b1f0a32f72242ffd70"],["/posts/2516528882.html","7e2bf8cecc50c43322d591bc8e9945ce"],["/posts/2529807823.html","32fb53e3bd66a36f55bc757a0ea6475a"],["/posts/2891591958.html","34e9879e5fbbc5dacb306f96aa546e11"],["/posts/2909934084.html","eb787cb6ec1abb822e243eb662b0b730"],["/posts/3005926051.html","fb7c5555c34d52cd2222a2572a575254"],["/posts/3169224211.html","7b42f9a606473b6502107d856dc445a1"],["/posts/3259212833.html","f8ec6319cf33ffca9594334d7e03ea2d"],["/posts/3266130344.html","0a52512f3bce603b6c7afcf2c49f974d"],["/posts/3306641566.html","4d7dbdbebcfca40d79bc2e8e58b56f30"],["/posts/3312011324.html","8c86d92622700371e5326ac31117d457"],["/posts/336911618.html","8f7b14d07fed156eb1ea2ad9ead32d1a"],["/posts/3402121571.html","05dd446b85250cdbee87b166c7f671b5"],["/posts/3513711414.html","c93e9056664e9ef6683b7e24357b405e"],["/posts/3546711884.html","944f425558e495d6d2aae86f11d56fc4"],["/posts/3731385230.html","4282bc7f74454f59fb28e9173173383f"],["/posts/3772089482.html","2822486bfdb453b253ce94b16420d9f0"],["/posts/4130790367.html","a669c7793e77979709b1068222fb7cbf"],["/posts/4131986683.html","518b1708a73a829446c748ead727e5fd"],["/posts/4177218757.html","a5eabf9a345582f6a513d66e227c7015"],["/posts/4192183953.html","ba21e7db1de33e2e50efb9635ed608bb"],["/posts/4261103898.html","85e1941811a4a448db84876a95a8d8fb"],["/posts/482495853.html","5442886a0a02fd74601c2d626c159f39"],["/posts/488247922.html","7b6a19cfe9bb6290bce2f34dae8e38ed"],["/posts/570165348.html","2bcc2d82007a8cc17f3ec26d43eac9dd"],["/posts/595890772.html","a91d132d27c266edd4d8ac223da63ac0"],["/posts/694347442.html","fbef914bfafb03de89c0871b7c329aec"],["/posts/707384687.html","7401edc0f5b1996a749c400a4c8d2ba8"],["/posts/795397410.html","c6ea46ac757355937b518af0dcb95ade"],["/posts/820223701.html","7f421717c0ecd05c40603b4c4133e898"],["/posts/88294277.html","b1538f9f08cedc9b29d4888e551106fc"],["/posts/939963535.html","cf1021bd581da4aac1f6fd8c86c65327"],["/posts/983786067.html","54697b86fd767096d0fb4d38b3d52a79"],["/sw-register.js","c9e91c6327f42f6086a4d42de3eaab9e"],["/tags/C/index.html","1b05d5f4b1eb1eddfef3c4535adb031c"],["/tags/C/page/2/index.html","d5ee780190cae1f8a1e40cfbf9eec2c7"],["/tags/GUI/index.html","92145a4999b4ccf983c7b1232100a027"],["/tags/Hadoop/index.html","f011b7697b6531173a0176d8a06efca0"],["/tags/Java后端/index.html","6d865b33a1e57be3e3736e7d0777f2e3"],["/tags/Java基础/index.html","b2b6dc11bf54bd403a8bec08ce6ec19e"],["/tags/Java基础/page/2/index.html","a262c0876e171b4d26dc38b544fbfe30"],["/tags/Linux/index.html","67b74d694d2f2fc8a5b95a8e8f0f007e"],["/tags/Linux/page/2/index.html","652d790100c222caed64cb2745fa8a0f"],["/tags/Mac/index.html","6669415cffb6aeceb9b515017a223726"],["/tags/Maven/index.html","649ec6342bf22365c64af3358888a1c5"],["/tags/MySQL/index.html","4ff0f36538d29425c7813244c6e00ab6"],["/tags/Ubuntu/index.html","058d03fa403b9f65c243570f7f49a6a9"],["/tags/Zookeeper/index.html","0bb4709b0c3663310d71065162b684fc"],["/tags/bfs/index.html","0ff70f126991606dea053cc276bc0b34"],["/tags/dfs/index.html","ee951ae20a6d2a8476cd07f99c7f4dd9"],["/tags/folium/index.html","0ce5bd01b00716cce8464958f3dd7808"],["/tags/git/index.html","f20c3bd1594fb52be3e44092523d3fd9"],["/tags/index.html","d9f9dca32d7dd88dc9fdf20bb4812000"],["/tags/latex/index.html","7b336d856c5ad9e9b1591afb9570f040"],["/tags/python/index.html","8fc14c47506be23bc546ab2f3d42e269"],["/tags/二分查找/index.html","967af952c98359f20377f347e24bebe1"],["/tags/优化类/index.html","d603afcb1fcc28bc62c3d6f46c35e2e3"],["/tags/前缀和/index.html","1182c84cb23757875a4577c33b8222df"],["/tags/博客搭建/index.html","9b398cf3728628fd39e05a87072859ea"],["/tags/图论/index.html","e44814b9d349535f142c3a917f1186e0"],["/tags/大数据/index.html","f2131b142f47870b1ab1fcff80a7b028"],["/tags/操作系统/index.html","f1dac36ce74cb9d40c813c320393fed6"],["/tags/数学建模/index.html","b89103689911355c81eda52c5b04ba4b"],["/tags/数据库/index.html","3661c2fe30325d19eabc62069e54fb34"],["/tags/数据结构和算法/index.html","cbf692280285a244597c75ede897f886"],["/tags/数据结构和算法/page/2/index.html","7675b8db39a6b48f0cefacd65b022ab8"],["/tags/数组和字符串/index.html","d185d606c63db3753371d8024bd22936"],["/tags/枚举类/index.html","a30b6b584d8d831217fde8bfe80c10d2"],["/tags/树论/index.html","394498aeda49c1c220c95b98640abc6a"],["/tags/测试/index.html","7c2c4be9df89d5d02c8d18169cf20a6e"],["/tags/环境/index.html","db8651b12f8666e061680207a19418b2"],["/tags/环境变量/index.html","fd0daa513131c29c868b7c376f94ad87"],["/tags/绘图/index.html","28d98e53c4f5440b0ea11c0ef63cef50"],["/tags/编程环境/index.html","3ea5f1790bea09f590c1760c03243e63"],["/tags/网络编程/index.html","576b4e24f2a01f611954315694e2cb15"],["/tags/英语语法/index.html","890c7b5931694c4456fec1d3fb231abf"],["/tags/论文/index.html","297f59587d6ca3ba6314ae23da65fa5d"],["/tags/资源下载/index.html","c7d3bd4fb87003fb15974522aecefbc5"],["/tags/链表/index.html","77ce5deeb905d280ec14daa73d511d63"],["/tags/集合/index.html","12ec006d22687671ab26ad1f92244fb1"],["/tags/集群/index.html","c8c454f4e62bb56c3a1cd9c9002fe7ca"]];
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
