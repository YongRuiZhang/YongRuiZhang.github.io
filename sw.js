/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e7b98c85c4b254acfcd76e324919bd41"],["/about/index.html","9cd015d656dc69704ef103c894f14254"],["/archives/2023/01/index.html","4594c764406b998d30dccad639df2e07"],["/archives/2023/02/index.html","95df573ca49804dc71685afd98714b51"],["/archives/2023/02/page/2/index.html","f63248a561d56d78ef0e679c53d99d10"],["/archives/2023/index.html","667b397e03774b630aec3139a93e5ba8"],["/archives/2023/page/2/index.html","cac5b292a9a63bba08d117058d394b46"],["/archives/2023/page/3/index.html","35e5a4af2735e41606008cf421d4b52d"],["/archives/index.html","7b0715532b1d3c699d7b569867364154"],["/archives/page/2/index.html","1e27d66d2bbc8ebfa62e7110d942887b"],["/archives/page/3/index.html","3e535262fd0ece54e1376d3d42b0028b"],["/categories/Java/index.html","c4d124996a4116d73e55013f4a580b90"],["/categories/Java/后端/index.html","37ee4ec1f50aa1c2c47be73418bc6454"],["/categories/Java/基础/index.html","1c7585d1371684fdcd4960412fb6510b"],["/categories/Java/基础/集合/index.html","87edbc6473849bca64bcf3bec9c1603d"],["/categories/Python/index.html","fe68161809eabd61a3306a045de307ba"],["/categories/Python/编程环境/index.html","e5becc5abd375dff489ae50b53cf3667"],["/categories/index.html","1e1d578dfdeb541de7a8b35319892877"],["/categories/大数据开发/Hadoop/index.html","78207b4647da26be10279ceca892d5fb"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f99b659c3695e867e9fba046104fc924"],["/categories/大数据开发/Zookeeper/index.html","f1436851445f12a2055c312c96b45a73"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","10fa1daba34d51dccd287bad361a7b4c"],["/categories/大数据开发/index.html","a4d6ee3e14b2cb0278d40d5d52a70ba3"],["/categories/操作系统/Linux/index.html","77b2d78f1decbddbd4c8f49584651a25"],["/categories/操作系统/Mac/index.html","c5aa9a6efe9989c5b202f4d0d94f4a38"],["/categories/操作系统/index.html","6abfd04a61f0ebf4e4212cc826ea42cb"],["/categories/数学建模/index.html","a8f76d70fb37c114c8aabf2d2414d58a"],["/categories/数学建模/latex/index.html","cdba78148c848f8c4d29e93f2705fe67"],["/categories/数学建模/优化类/index.html","76ae3aa757ef98563ebd058ad20a9943"],["/categories/数学建模/优化类/现代优化算法/index.html","8276e126bcc242f3e2f085135588a475"],["/categories/数学建模/优化类/规划类/index.html","2c9004bdd7db357b1aa04c2630870679"],["/categories/数据库/MySQL/index.html","cf75d32ba57edd0d49b3155b3710ebac"],["/categories/数据库/index.html","9cf5564e8a71d647dc518a4da8507da3"],["/categories/数据结构和算法/bfs/index.html","724202c2b9aad685782e49762d2068a5"],["/categories/数据结构和算法/dfs/index.html","7eabd24b3de9bb6efa92276e7da1a9c2"],["/categories/数据结构和算法/index.html","d669842febff76712842c0154ff4eee8"],["/categories/数据结构和算法/前缀和/index.html","d3f8e9eb552fdcf56c73f0677578b711"],["/categories/数据结构和算法/图论/index.html","73dbf9b8bca6aecab0495d1bdad6583d"],["/categories/数据结构和算法/数论/index.html","4a8104a3d6ab60556d2c8ff66bab3a7f"],["/categories/数据结构和算法/链表/index.html","3c9734431ba1c9baa64c9f32621cd3d2"],["/categories/杂七杂八/index.html","2997bbb74656af1c99f2eb4f5a774570"],["/categories/杂七杂八/博客搭建/index.html","6dc8ac6bf21f4ed0f74fcd44e5e32ca1"],["/categories/编程环境/index.html","03ffe75cd686319aca5d1689502d8fd9"],["/categories/英语学习/index.html","6101bb9a04710c7d9ebb356a1e82c7be"],["/categories/英语学习/英语语法/index.html","6dc4c1afdfd4316a1ef6c747f5ae5e6e"],["/comments/index.html","53d3475796ac3ab53bf81df15921dba2"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","22f8a7eeb8ffec049412c38e3a5259aa"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","6c40fb953eed149bf48998887aca4956"],["/movies/index.html","2da95aa0f1b27892e07feacaedbf5737"],["/music/index.html","fc79f125a121b59b18a8a5c5f83d731e"],["/page/2/index.html","caf346dfa0cbbc2bf4774bbfe25e73d2"],["/page/3/index.html","dc2279cf759710394a0330a0c34d9d16"],["/page/4/index.html","0d31b0cb9b198981e8b551eabfd9512e"],["/posts/1021360842.html","2ec237532178fa39cb3802ba57e17dff"],["/posts/1120620192.html","f4463812a0d2f74b011b70b44882edc5"],["/posts/1141628095.html","ba1c55cd17e79ff35bc57df3e1767b89"],["/posts/1168613674.html","7516a1648146d2dd86528ae470f0092b"],["/posts/1219920510.html","c5b123aee754c5aaafe4d09056ad1f2a"],["/posts/1222166338.html","b336bc2effce8ad39f4f908424eaa8eb"],["/posts/1259097482.html","24cc0b345a59fd69a63597ec45178980"],["/posts/1271036369.html","f0f9aa9130fe3f489cb2c9a0d6c4e87d"],["/posts/135355774.html","b81cd8d6f7ae1dd4e4fca8e4f0c864ae"],["/posts/1375344716.html","336def47001771313b1e506cc0cf777f"],["/posts/1388991698.html","6853b8fc4acd32f37ae4339909a7ef0c"],["/posts/1410315814.html","d463f60d72372aeaf07768be2b8c5242"],["/posts/1470079884.html","604819f461e72971b16c74e9c380ae4a"],["/posts/1470079885.html","8cee260500eddfb037e753f1acc09cf0"],["/posts/1470079886.html","aa190f3824f5c3d376a49cf41bfd2ad7"],["/posts/1470079887.html","9ee41fa9668a848efe63ca40dabd2fa7"],["/posts/1498536549.html","ba42d7959021f9c82fcd1f42a7720b45"],["/posts/1571776361.html","292b73bf5cc43c13dbc89af9c2389b82"],["/posts/1605124548.html","25e9db1d4c5adc9b319738c38fc82177"],["/posts/1925125395.html","9d9db877523ee99e063515ce8888f751"],["/posts/1966191251.html","de219a8682c1ca8c5c2e5bf0f6adbf15"],["/posts/1987617322.html","8fc6288dcec387b007bca084527a6340"],["/posts/2075104059.html","3a7f3acbc52cc64ba6bf00e10114c09b"],["/posts/2087796737.html","48a7a367b40ef2ee6f6e132f212e910f"],["/posts/2207806286.html","c30b44368cacce4b82969da38481220a"],["/posts/2364755265.html","86c42902c3ff1e0fe612f432b1d76562"],["/posts/2414116852.html","8b0b05cca4691dad001aec22354794b2"],["/posts/2495386210.html","a7efb4179336ef9d63860331e085a406"],["/posts/2516528882.html","d613810ab9ecfce42303f8c50a6138ee"],["/posts/2529807823.html","beae139a02d2df41f3a8cfffa68a7a4b"],["/posts/2891591958.html","c220da5f1caaf5a817fc6101f7216ee6"],["/posts/2909934084.html","4af190c6e062cb01bf44eb4f038976a4"],["/posts/3169224211.html","ae1a0922eaed0543cb7133cf85683036"],["/posts/3259212833.html","dff28021b5ccfa28c4ac74ee9d57dc10"],["/posts/3266130344.html","e6d54f6165e98f499c5cea05682b147f"],["/posts/3306641566.html","2606c91b5b4953b7f86fe6ca28b5439f"],["/posts/3312011324.html","e6cad4fd3b75c868d5f52ddaff577cde"],["/posts/3402121571.html","7ae32e97339d529f44623c16435b5339"],["/posts/3513711414.html","333a188a632fdff8cd82853eadbc809e"],["/posts/3546711884.html","2f1840aba167782f2ccde090a6dfb5e8"],["/posts/3731385230.html","1709e85239900735f6014040fe70bb54"],["/posts/3772089482.html","d8abd7a8b6e2ed1cfae0b2f51f158c38"],["/posts/4130790367.html","227cbf0016cb580a0e80497df69af851"],["/posts/4131986683.html","e1bd5f2438032fb336cec94875c527af"],["/posts/4177218757.html","fc3c67e1d2fbe0afef7ceb32cb399ddd"],["/posts/4192183953.html","d7cd675e64a65526b2e2fe611b594f4f"],["/posts/4261103898.html","263c1a5ca0f8a0ae7431ffdc07858c13"],["/posts/482495853.html","b999da88ee355d2c8fbc9106fcd4ea25"],["/posts/488247922.html","f3ebb3641ab38813b9e3c53fd678f02a"],["/posts/570165348.html","a0bf8fbea19c48af05ddb3eba50a2807"],["/posts/595890772.html","4b6b41dc414c8dc2d24134cc4fa39d96"],["/posts/694347442.html","f99a872dbf948fbe45775d88615c4156"],["/posts/707384687.html","820c25079e1767e86ae24636636db4fd"],["/posts/820223701.html","93f92c0a48bf2435c104b56541f81b17"],["/posts/88294277.html","c81fc185747fcb38a424a5717459280a"],["/posts/983786067.html","8c50b3d0347eea1987c0f46ad83d1ddc"],["/sw-register.js","65e7ecb0b7da7dab46d5bc11087baaec"],["/tags/C/index.html","180c046206210bbd096583f1c30a257f"],["/tags/GUI/index.html","3223cdb4dfea29402b5efa1507f1c096"],["/tags/Hadoop/index.html","46eb9e41ff8f3b5e573fa039a145ef9b"],["/tags/Java后端/index.html","a84baa3e34eb95c6515c41432b54f62f"],["/tags/Java基础/index.html","b48f5fc8e3a45b4bd5c23710d7f93743"],["/tags/Java基础/page/2/index.html","aa53da8c31a80eb75d0c7fcf9d5a3904"],["/tags/Linux/index.html","a61de43414c6ef979882013cf4beeab1"],["/tags/Linux/page/2/index.html","294ce1c405b8bd75fa573245f419b100"],["/tags/Mac/index.html","95b5c153ae126a241f743578e339c711"],["/tags/Maven/index.html","f1d6ef97685d74a335b44e36db3533b1"],["/tags/MySQL/index.html","d9022a170b65b8a5bda1bd83f2299331"],["/tags/Ubuntu/index.html","5d9a570bcba84ae5c1b922e75dbd64fe"],["/tags/Zookeeper/index.html","f019d62385a3e46ca75ffa20135846a3"],["/tags/bfs/index.html","5c6ecbb15e0501951ce55568ece8338f"],["/tags/dfs/index.html","e7a7e1974e4cd348b618cb441e2b2801"],["/tags/git/index.html","4536318b9878a45049f22955f2c15ce2"],["/tags/index.html","f2aef67f1c6aeeae99f155bb60777c0d"],["/tags/latex/index.html","21d3bae97660f6e94364d5f50a3b5d5a"],["/tags/python/index.html","4fdebbc39bc275d7e793bd9f1b716252"],["/tags/优化类/index.html","827583684f91f2562ee4554ae78507ea"],["/tags/前缀和/index.html","ca1674ad5ab4fa7adc7821e83ceca8b4"],["/tags/博客搭建/index.html","97e27956cb5a3713012467ca58a7628c"],["/tags/图论/index.html","a10571a7817401df49f3eb3e0b727605"],["/tags/大数据/index.html","b897985a6f5217e8ea4db23d476e3b23"],["/tags/操作系统/index.html","78dd28805d0defaaf889954b2753040b"],["/tags/数学建模/index.html","9b9be568e423fb42a7f8a50702c24db0"],["/tags/数据库/index.html","1eb63eef4e5995d697c562918ea64a46"],["/tags/数据结构和算法/index.html","8feca79fbef0906f98a7fbf2d5175c51"],["/tags/枚举类/index.html","0ea954d86f835ca3bf23fbd76868fdda"],["/tags/树论/index.html","a74090d286ea156a906a9c0d5b8ea269"],["/tags/测试/index.html","ce21d754a0fb421d497e7024cdd50875"],["/tags/环境/index.html","c5028188376375942e6923d57666d399"],["/tags/环境变量/index.html","21d608112604b4e7f2a83f1fd5b6b6c6"],["/tags/编程环境/index.html","29c3a89e1cad0029de005b7de7b26cc1"],["/tags/网络编程/index.html","9b29408d604163a0df8b33d9854dcf72"],["/tags/英语语法/index.html","4db79fb49eb17eaaeaebef4ee5ec3dca"],["/tags/论文/index.html","87000fdc568be406846c4f5376a6ba5d"],["/tags/资源下载/index.html","b786f753ca86960883a874202bc9ea50"],["/tags/链表/index.html","9a71baf9c7bf4e753f8bfaf911910285"],["/tags/集合/index.html","3d362bbcd2f0d2bbd14a29c2cd0eb776"],["/tags/集群/index.html","b7e22bca43ef37a33c2af0d005ff6e2d"]];
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
