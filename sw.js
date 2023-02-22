/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ff6e8f49e857f61c20fd1367bae26874"],["/about/index.html","0929b28657be9434f003defc2cb3f0c1"],["/archives/2023/01/index.html","d8d30d77df636d2c7b402f6310f623a8"],["/archives/2023/02/index.html","1733b9a45ebec5de8928ad344b6b3c04"],["/archives/2023/02/page/2/index.html","e40d80b308c29a221f5c942e281055b7"],["/archives/2023/index.html","26a49c3fc344d1fd7ab3d9c33511b639"],["/archives/2023/page/2/index.html","c42c3716494a664b1c79d010cd934f1d"],["/archives/2023/page/3/index.html","0d86c5ac1439bfb2ab1f0a2e7c70158a"],["/archives/index.html","39ecae4edf518d8a6e45dca051d5b777"],["/archives/page/2/index.html","6f71d1a6b5e93fe3220c130de7d5a808"],["/archives/page/3/index.html","9fb998fef88c85305ab7add534ba862e"],["/categories/Java/index.html","c9fa149a71a55dbc5c6dd0bb2a2c2bd5"],["/categories/Java/后端/index.html","b9b7a4f81ae3834898e9272eddd2d81b"],["/categories/Java/基础/index.html","bf811d90e486dfc4cf111f820b039d72"],["/categories/Java/基础/集合/index.html","78a4ce680cb2a1f13d0b6a1d1f04c805"],["/categories/Python/index.html","75de02de5a2fbc618f7ac464ee292ca8"],["/categories/Python/编程环境/index.html","91f546263eec67216a4588eb32c9db63"],["/categories/index.html","f80108b6d76f70e03a0adf8ecda83bfa"],["/categories/大数据开发/Hadoop/index.html","7ea8c633f74bddb0541e3c4324cc4b05"],["/categories/大数据开发/Hadoop/环境搭建/index.html","7d0f30cff00def073953dc826a8de070"],["/categories/大数据开发/Zookeeper/index.html","fd32ff9b0f7672b0b28a50c78e7a33ed"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","30b52df62906ad0c0fb904120b2eafee"],["/categories/大数据开发/index.html","7c363f8ebc0501eb1daa3c6e95a77b0c"],["/categories/操作系统/Linux/index.html","2f64caa7da8a040c4f469d97b9a14031"],["/categories/操作系统/Mac/index.html","66469046495f488b78c86d7da9438021"],["/categories/操作系统/index.html","4a554e1c5a1a67df216c454bec5394f9"],["/categories/数学建模/index.html","474bed83db2fe022c96993f1eec542e8"],["/categories/数学建模/latex/index.html","ebeee852b139464ed3b13e7a474c1fef"],["/categories/数学建模/优化类/index.html","44940b8e76083a693bfa56f9737e9e6d"],["/categories/数学建模/优化类/现代优化算法/index.html","ecb6f8e9ff3c37d1bb032cb20b836a3a"],["/categories/数学建模/优化类/规划类/index.html","6c1cf034bf47c2db83caacc4d8e22dbc"],["/categories/数学建模/绘图/index.html","f206b839ee2e25516e31ce43b65e3481"],["/categories/数据库/MySQL/index.html","43ce4e7037cac319593b5f3cc8e0dc61"],["/categories/数据库/index.html","7da811b40ff1977cbd5cf895a2eb1ec6"],["/categories/数据结构和算法/index.html","eafe652550cfc7aebd0bde9d7cd6ddb8"],["/categories/数据结构和算法/基本原理/bfs/index.html","211f90a103e0faf12c8c935b31e48517"],["/categories/数据结构和算法/基本原理/dfs/index.html","8b495cc9dca969ff91944e19c5130195"],["/categories/数据结构和算法/基本原理/index.html","38b1048075fcdd6cca87b805d89b11b7"],["/categories/数据结构和算法/基本原理/图论/index.html","7c852e9d7721f0da9499c83821fdc943"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","4861a1df21c5c455e101d04e2d5339d0"],["/categories/数据结构和算法/基本原理/数论/index.html","9691e1714b256612d3352b18883b6a89"],["/categories/数据结构和算法/基本原理/树论/index.html","d94343e4396f923d3929bfc1b273efe3"],["/categories/数据结构和算法/基本原理/链表/index.html","05b211a4b35dce4da209f049a871ed76"],["/categories/数据结构和算法/算法题/index.html","5206d31de3b793011f417bc6cabf9ded"],["/categories/数据结构和算法/算法题/二分查找/index.html","a4f49466168a9c938e95b0643c99a0b9"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","cd042460264120ac08a540adc3518f5e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d1b8269ca6c7c16639deff1345f1d11d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d0891dd19dccb0df6edf4fa17856643c"],["/categories/数据结构和算法/算法题/树论/index.html","c6577a38227fe332a3aa82cb663b7c11"],["/categories/杂七杂八/index.html","4b00aab438e2709aec7bb7e9fc348fe6"],["/categories/杂七杂八/博客搭建/index.html","ab4eba89eaeec9876f591b2eb31973d6"],["/categories/编程环境/index.html","5f16c70c6fb986296bc557d73f98e2cd"],["/categories/英语学习/index.html","da7e70d7839f29b9913eb50d9cad5b25"],["/categories/英语学习/英语语法/index.html","887d3c2d39f6a9a8005b404562b87847"],["/comments/index.html","9097f35b692d1aa243be4d921ca4d0ad"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","d4979cb479b07ed330738fc5d9376395"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","daa59c1dc4b82dede3e550355d3cc84b"],["/movies/index.html","06ff514fa8fd6ade7c36c5b41fd0a47c"],["/music/index.html","71550197243db6ad0b8a4e389adfcc75"],["/page/2/index.html","381f559124a49b4d2f2f512a9155654f"],["/page/3/index.html","4ba18c3be1b2119e12e6e7231f7819f0"],["/page/4/index.html","1f633cf6b24964202483f0e7b517a431"],["/posts/1021360842.html","391639218bbb7e749e1e81975271dfaf"],["/posts/1120620192.html","aa688c4f3d19addac8076e387851bb81"],["/posts/1141628095.html","a9285eb5d4819c09747a5afc831242fd"],["/posts/1168613674.html","8aa67a0c7b35dccda76c8271fce1699d"],["/posts/1219920510.html","930213b771eb0ea7cfeb36e11e38f766"],["/posts/1222166338.html","424b0ae66277bff3f2c8f88e847c17ec"],["/posts/1259097482.html","9e8729b8f28eeff54f61f0ef8d43e756"],["/posts/1271036369.html","8346870ad0c9435a172fbeea823d85fc"],["/posts/135355774.html","4fd25613519484108ddf661f370f3e5b"],["/posts/1375344716.html","d4f6367ed6ca020876f83a5fa4befaa3"],["/posts/1388991698.html","6b5cf388e07f88ec83e29b0ea828d74f"],["/posts/1410315814.html","c0e9909cc93a8789c37b1c699531e2d5"],["/posts/1470079884.html","5a3cfa8d062f63577a1e7caf46ad14b6"],["/posts/1470079885.html","ba5118ceb71ec6baf55f586e633c4e50"],["/posts/1470079886.html","15e3245ef325256a68b73852212360e9"],["/posts/1470079887.html","19ce731594c310060f350461b3fc775c"],["/posts/1498536549.html","ae1e09fe4202cfb30b388bb2e7c33933"],["/posts/1571776361.html","ea88df60f53a5e60bc9a735673d30b38"],["/posts/1605124548.html","6a144ab7491151d6d36c3d1d5730f91e"],["/posts/1925125395.html","f556fbf908584588c2b2af6c2eb6838d"],["/posts/1966191251.html","b48f3513cbcbdbcf0cba4359e59bc0b1"],["/posts/1987617322.html","548e8ec25eb049de2ab4127a7db18188"],["/posts/2075104059.html","95b66be02fd013f774b55002875b998d"],["/posts/2087796737.html","33e3276fcf377384f3c7c025b7e58744"],["/posts/2207806286.html","dd8b28cf75e14992e8ba7d0395849d58"],["/posts/2225903441.html","b2f93d174114a936a9d89035def20bb3"],["/posts/2281352001.html","a4d6b239744a18b69c16fc4221ed8e39"],["/posts/2364755265.html","762f350dde6b5b9b646b20a78ebc72f6"],["/posts/2414116852.html","1294fd7b7fa2736aa7a374057b48cac2"],["/posts/2495386210.html","1ec4b458efb761216b6954057c2ab9e3"],["/posts/2516528882.html","582d07ce8eed7e24294bc28f6f26c86f"],["/posts/2529807823.html","cf4dded272c91988aa44536b751d11c2"],["/posts/2891591958.html","ed670baf7671eb63488c2a2eb65c8ba4"],["/posts/2909934084.html","80130775667ce38241aa1f14f4a355d5"],["/posts/3005926051.html","be0360c0ee9aa88b82bf8d7aa9f92697"],["/posts/3169224211.html","a24eba279709b6da76ef3c86b14f9677"],["/posts/3259212833.html","eb71f7ad0a0d5e0979d9f12d9bc37b01"],["/posts/3266130344.html","853be9a031422a8dd31e3e5a063a01d3"],["/posts/3306641566.html","7742d44318c0a78a040f37ece904c499"],["/posts/3312011324.html","889a43c42ac8c3f2089a0764b90bef9b"],["/posts/336911618.html","d23c058aa42e44efa171d1f31f3a196b"],["/posts/3402121571.html","c6c8f41e9bcee33c7bad37867658ca9d"],["/posts/3513711414.html","9cafdff72c8d35543eb49fd18706665f"],["/posts/3546711884.html","776eecf4e0781f1f9e2563aeedf42d83"],["/posts/3731385230.html","4553681b5162ae48e2f41d12bd5415c4"],["/posts/3772089482.html","82abd9d2bd72ba850f8d9013912808bd"],["/posts/4130790367.html","4f85d2839f88698615874bbd5403071a"],["/posts/4131986683.html","7b3fe3e82f7a2b594fbc705e1e31c1c4"],["/posts/4177218757.html","e410daa3c4cdfc060f1870c290976a8e"],["/posts/4192183953.html","a396e7e3a90b5f34c3df22b7f635942d"],["/posts/4261103898.html","f1de28732ad7bab18f9e899cbdf2f180"],["/posts/482495853.html","3e64fc1b364744eaaaeb022701f5706b"],["/posts/488247922.html","6f8268185ed23833474d4d5f02afd8cd"],["/posts/570165348.html","b8a1bed0aa4ef61537e63fb3f721e25d"],["/posts/595890772.html","a69ab0185ef5cdcde452035a5f4dc166"],["/posts/694347442.html","07553d0ad6a0e729b36f03909db4ca3b"],["/posts/707384687.html","541fe2dc0c9c725b15a2a9ddd14bdec7"],["/posts/71180092.html","e25a882f990482c0c94dc44a8a7be99a"],["/posts/716459272.html","d3ee6a55a56abe3c5caf846b706365d5"],["/posts/795397410.html","5660f5daee2d888163fde49ba5937051"],["/posts/820223701.html","de4dade4a453fa1091ca73646788f20d"],["/posts/88294277.html","c55689dd75e629fa73a2b7017b019a59"],["/posts/939963535.html","9a9399eb6308c5d248e4044ee2ee0268"],["/posts/983786067.html","6e7a0d6ae27a2e90349f9c5cb2448ceb"],["/sw-register.js","147d9eac0a95c11eeb1e545676113cc9"],["/tags/C/index.html","624145db257cacc1b38e29673ae858ba"],["/tags/C/page/2/index.html","add47bae2599eedf76ad4955d3a5344a"],["/tags/GUI/index.html","6d37cb79bc4865f27f5546a8c0794ce2"],["/tags/Hadoop/index.html","2b0552780647533f34fa8d994a0387ad"],["/tags/Java/index.html","247a13e0705c435d9c399344b8a6946b"],["/tags/Java后端/index.html","6eda0043b6e0c54e577a9d83fc051fd8"],["/tags/Java基础/index.html","3880e7343c27876bf31b2060e5bfaf62"],["/tags/Java基础/page/2/index.html","b08e0042e5a87d3f83ad8df8b4049345"],["/tags/Linux/index.html","efe18d2c09e2f8207689c67e384376b4"],["/tags/Linux/page/2/index.html","e2e296d0dd7f0f86157e2bb3d3b20ddb"],["/tags/Mac/index.html","a50774faf72d75e5b4bd76151b12eaa9"],["/tags/Maven/index.html","80ae500ee5a53644372d610680e72bde"],["/tags/MySQL/index.html","7444a6b16b491eabec48e85df02da44e"],["/tags/Python/index.html","9dc6342e6dddb83a1183e8b5069ff373"],["/tags/Ubuntu/index.html","5f027713321fca4aa8287c64d552a0aa"],["/tags/Zookeeper/index.html","98d88e4e2ca1e2ed9a01d1c5d5f3247d"],["/tags/bfs/index.html","3296e87117b0da36784bdacf0c919ce2"],["/tags/dfs/index.html","35eb6720d238fa0fe83150c158205f27"],["/tags/folium/index.html","4e6d9e72e573c6d7f639be9fc11ba3ee"],["/tags/git/index.html","ac3843f8695ae9e81a720ee214950a5c"],["/tags/index.html","ea52a4a1ace865104ef0bd0e9dadd054"],["/tags/latex/index.html","18118b1a8f1bc1c40e61e0a49a2ebb2c"],["/tags/二分查找/index.html","34e626eff56b7089091c4cf0b3932f67"],["/tags/优化类/index.html","24c39617ec7babe37c15da5e33019047"],["/tags/前缀和与差分/index.html","038c020d825acd3dc5f9830042bf6181"],["/tags/博客搭建/index.html","3be51dec6c7e82f4a958492c321aa42b"],["/tags/图论/index.html","e8c0ca46ffaee320dbad24b0872a4661"],["/tags/大数据/index.html","7f20fe98e930273e7266ae3fbf6d9540"],["/tags/操作系统/index.html","be06dc2591c4e49d450cc58b3749b64e"],["/tags/数学建模/index.html","25945da65a9eeb5b6ad663cfd668ffeb"],["/tags/数据库/index.html","0768f6c05df091241399bd45e4e0c6a2"],["/tags/数据结构和算法/index.html","5f01a719c75b392b7cec3f2ae56b08e4"],["/tags/数据结构和算法/page/2/index.html","742cb5757a6c3e6873f0a54fcde459f3"],["/tags/数组和字符串/index.html","aa6c1a21759dcc3af1d6a0b8cc0cf8b8"],["/tags/枚举类/index.html","976e6be5f3632576c9f95b48a881579f"],["/tags/栈和队列/index.html","e6b677eadb98637afb6721d8572cac01"],["/tags/树论/index.html","595536d52a7456e3fbb3b84570904a93"],["/tags/测试/index.html","138b2d1fe4a6511a2631a5edf27b738c"],["/tags/环境/index.html","db59909aa50e346949ffdf75caad6ade"],["/tags/环境变量/index.html","d50c686e2b54ff7e04929155ff92a84e"],["/tags/绘图/index.html","ba684991ce2eb224a77e0b2619f8eb08"],["/tags/编程环境/index.html","d6383a0d4891a17fae2401e2f45aa000"],["/tags/网络编程/index.html","c4eced87a72cc64f633c5dbbf755dcd0"],["/tags/英语语法/index.html","db8a63674a9da4e22161e6a95b110561"],["/tags/论文/index.html","58552507bfccd03a8d835eb7bc502c97"],["/tags/资源下载/index.html","fae319a7ec3d8a9c3754124883965e3f"],["/tags/链表/index.html","5467c8d5aac7e9a6d09f710e11a58c2b"],["/tags/集合/index.html","10e90b982f2bfe208757fb9676ab1147"],["/tags/集群/index.html","1d2b85300aac5a4908a0368a93431cd0"]];
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
