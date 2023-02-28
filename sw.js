/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","fff4e6453614ea7804859e99e266b51e"],["/about/index.html","0cec414e1d76a9b2090f03df71f04b87"],["/archives/2023/01/index.html","f447101bdecf07519df6c1b41d661c6b"],["/archives/2023/02/index.html","745fa8cf5d241c303da62be8eb76efe5"],["/archives/2023/02/page/2/index.html","356632c49a6cc63804d5111c6558b636"],["/archives/2023/index.html","f1f3b941831b2da64459a4dbc78d03f3"],["/archives/2023/page/2/index.html","dc450ebbecb3d4a74cd7db38a0389db3"],["/archives/2023/page/3/index.html","b95cc908b75a3a4211234a5527078d9c"],["/archives/index.html","92773a3f2dec3cc20311319f3c33e5ab"],["/archives/page/2/index.html","fcfdcfc3fba94aef8984b2768bb4b1c1"],["/archives/page/3/index.html","56e82d028cf358e803c35cba9af4b6a5"],["/categories/Java/index.html","aeec83c125eeec2aad49db2f74c1fbb9"],["/categories/Java/后端/index.html","5f25fff19ff40bf1345e9de04b13f214"],["/categories/Java/基础/index.html","5251229d6a9053834b570979bddcea5f"],["/categories/Java/基础/集合/index.html","b04271bffa854471f0445406deac8aec"],["/categories/Python/index.html","94a75e912e44bb834ab07f48cb6e5e4e"],["/categories/Python/编程环境/index.html","8fc2737e175adc5b38313231c91e57c7"],["/categories/R语言/index.html","6f4f4bf5f011d85f2b47a8a1a79cbe1f"],["/categories/R语言/编程环境/index.html","df6d54ffa01bae2a36f53f784c28830e"],["/categories/index.html","e03536d55332012da7d61efdd65ab7ea"],["/categories/大数据开发/HBase/index.html","6b8b01ad6b5194582d7170f99d87d8fd"],["/categories/大数据开发/HBase/环境搭建/index.html","04532fb4bf74ecc0113c0649bc34ec87"],["/categories/大数据开发/Hadoop/index.html","b13ef39fca7a336d48bf5b1001f14151"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a454366d5e957aac98bced2e2228f651"],["/categories/大数据开发/Zookeeper/index.html","3108c824e4c86ef24b97c7a07303405f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","abce325d1b48956d5d4d41bd22868687"],["/categories/大数据开发/index.html","0ba7270a036231d8503ae74e3aa8286c"],["/categories/操作系统/Linux/index.html","2c2675cac3fe1545fca36a409a7a32c7"],["/categories/操作系统/Mac/index.html","e990f92341c21434e41541d3636effd3"],["/categories/操作系统/index.html","cec3c753f42a1ec15a27582e7f5d5ca2"],["/categories/数学建模/index.html","c492e266357cda9ddf154f820cdc34e8"],["/categories/数学建模/latex/index.html","24d922525a6924f29610f31bcf8b598f"],["/categories/数学建模/优化类/index.html","1035adff636fff63bca67d8ca82a2bc9"],["/categories/数学建模/优化类/现代优化算法/index.html","3f3475b64655f07a019989f00f8cf12f"],["/categories/数学建模/优化类/规划类/index.html","ae80fdfcca535e7ecbd085c4d03e81e1"],["/categories/数学建模/绘图/index.html","83d61c084c70e2b4f3d12fae99715ac2"],["/categories/数据库/MySQL/index.html","c0048ca3bad1940dc46c0efb10f734ae"],["/categories/数据库/index.html","121f07ba0cf95afb8ba72fc3afd42aab"],["/categories/数据结构和算法/index.html","a7080c187ceec8b4ce17f062300fa835"],["/categories/数据结构和算法/基本原理/bfs/index.html","e0044659d0ab332324c8858aa9d776d7"],["/categories/数据结构和算法/基本原理/dfs/index.html","6dbc982b9f75b6a2708697f3658fd220"],["/categories/数据结构和算法/基本原理/index.html","95abd08dda266ddfd3e77080e5901956"],["/categories/数据结构和算法/基本原理/图论/index.html","f8fed817302ea634eb375ea2a707b0aa"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","282803080bec7f7eb7eae72cd9bcb7a6"],["/categories/数据结构和算法/基本原理/数论/index.html","c0f0df2379d8d41075b5ecf712ad61e9"],["/categories/数据结构和算法/基本原理/树论/index.html","74833752e686911ebe71f7c2674eee48"],["/categories/数据结构和算法/基本原理/链表/index.html","4dfd7e59b656bf66fa3ac0df51f46e46"],["/categories/数据结构和算法/算法题/index.html","64f9c53331d4f1c66906f7a5fa023e85"],["/categories/数据结构和算法/算法题/二分查找/index.html","738f71864f648fe1a2a48d5a7d279828"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1fa2c3115ca39a0b852489ae3712e551"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","79e8d20a793e8f95195586b21a0876cc"],["/categories/数据结构和算法/算法题/栈和队列/index.html","4bc9369aef851c889ec3a17934b1a457"],["/categories/数据结构和算法/算法题/树论/index.html","19e69fee96d8d0f24beccb42928d11b6"],["/categories/杂七杂八/index.html","099b17408fc7b777891e43c2ec0d63bb"],["/categories/杂七杂八/博客搭建/index.html","3d3c9064ff3c101a94723b509fab6fa2"],["/categories/编程环境/index.html","371d5200043b77e53d03b87137b8bbeb"],["/categories/英语学习/index.html","268ba1c0462508e407782ab92379af84"],["/categories/英语学习/英语语法/index.html","db63af899c5f7dc9fccc56198d502418"],["/comments/index.html","250021da90fc021d9f2c921123379770"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ff38029d0149d4599414827352b3cad0"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","fdc9d1f311957a5a69aa8713d21a5c0d"],["/movies/index.html","90d944595cf8200ee5607f53de893aa0"],["/music/index.html","197983281b8edec6ba5e91dfc530a90a"],["/page/2/index.html","19de6161fbba263cb0c2dd78208d838e"],["/page/3/index.html","0fa7e87b7e1f5bdc8cc344e8f379644e"],["/page/4/index.html","55a4d53b6a806cff9267d4e0a4eaaa17"],["/posts/1021360842.html","37453e146ab975b82bf774be576bed1c"],["/posts/1120620192.html","6387e5e25c08b3ae2bf9179986f5c135"],["/posts/1141628095.html","98d36bf8df29ec42cd339ddda7b0f47e"],["/posts/1168613674.html","16d9cd32941b20e65654b8c79ad6842f"],["/posts/1219920510.html","e89f34ca84ae9ded7095a3ccf1210be4"],["/posts/1222166338.html","5d5752a103a78fbf2a60594a435d2874"],["/posts/1259097482.html","b19b485dbc85ddd4b32a04380b101c8d"],["/posts/1271036369.html","ff981979c634fd56248d1f2d03cc7de9"],["/posts/135355774.html","1a97c873c4979c9f081ebe0a05796363"],["/posts/1375344716.html","065d6351f6a43c269069e2e0940d6ab8"],["/posts/1388991698.html","33de5855396ced04025ac03eca12adc5"],["/posts/1410315814.html","f3870b53de86036cb06bc34ec5e270af"],["/posts/1452790229.html","66edc8743cb8ec2ed7e9be9edb14722a"],["/posts/1470079884.html","e6503b009ac91eb391defbf4847d0172"],["/posts/1470079885.html","08c133d1fe14559d4c08ea14a0b6a770"],["/posts/1470079886.html","51781d349611a260f055bf85a9200077"],["/posts/1470079887.html","d9fa968aeeb5480c1f1b9c56e0dd5c55"],["/posts/1498536549.html","c847a6aec3a62fd49cb3bcb7c2451f72"],["/posts/1571776361.html","6e5ec4c32c6690d5d027f7a1ad5fbfc5"],["/posts/1605124548.html","6bd4d9fc3c4ce9644a97357f2012bc23"],["/posts/1817748743.html","7f4bb0ab3be94628558b53002a0b3d0f"],["/posts/1925125395.html","292f52874a570d519505b6ff66f4c8b6"],["/posts/1966191251.html","67ab128c1305387a7d4e48fe1acaf0d7"],["/posts/1987617322.html","239e5e50702881414db8555220f9805c"],["/posts/2075104059.html","f3c0aeb5f9efa12450fafc56dba88ebe"],["/posts/2087796737.html","5d422acc715ac6ca6bf16265be0360bd"],["/posts/2207806286.html","bdafb4b9887d2085f00347b82d0daefc"],["/posts/2225903441.html","5b87cd937a4f55fb97bbec87faf0182c"],["/posts/2281352001.html","1d8e16a36f41b313242d6897234dc795"],["/posts/2364755265.html","23b92d60490dae17d0489975563a63d9"],["/posts/2414116852.html","e1318cee177dbdb79779bd1464c83a35"],["/posts/2495386210.html","660b68ced6ba6315681e30b963b1464d"],["/posts/2516528882.html","18da43c56b593ac814ef984f33addb19"],["/posts/2529807823.html","2a071aba300b407741c48b50834ec733"],["/posts/2891591958.html","0f07e0dada398ad06b9d4344510e7cfa"],["/posts/2909934084.html","8b7cbea3691c2cd1825dbf6a02c87c01"],["/posts/3005926051.html","ea09d3ed64a5781448c88981a6cd93d9"],["/posts/3169224211.html","64bae02431e149a6c645487982312f44"],["/posts/3259212833.html","d3fb93385159e553d0a6dba56180082c"],["/posts/3266130344.html","eaf85b137d9ec6aca17521d39fa35302"],["/posts/3306641566.html","4d9a4ae31484ee0728cf533e52839f75"],["/posts/3312011324.html","e5619783094cf718f0081cc4175f8d1f"],["/posts/336911618.html","a3bcf178d5726090deb8c18e9188dff3"],["/posts/3402121571.html","c92f2f1d4fc66f7d22d644e2e3890901"],["/posts/3513711414.html","6b23bfca63b02e0a0f18fdc5d67f6a9a"],["/posts/3546711884.html","a0299ef70125dbfd2c9276b5da10d86e"],["/posts/3731385230.html","8dd43896a7234ccb0befa460f4299b72"],["/posts/3772089482.html","0cfa91ee268a924ae95751e204715d4d"],["/posts/4115971639.html","498da0782757651f69c210c1a60fa511"],["/posts/4130790367.html","b51240b72ed85b4f2475cc59f491b31c"],["/posts/4131986683.html","e676129408f0270e5b9547d7138a23d4"],["/posts/4177218757.html","ab15eb29249eace2dee7ba714d70f81b"],["/posts/4192183953.html","0acb77e9767509b23b61ed53ba6c9b03"],["/posts/4261103898.html","c4fe158e33cd43870791dd18adc79b6e"],["/posts/482495853.html","cc1ae0e190a6ac82bd4f970f5ea5ffa7"],["/posts/488247922.html","8bbe933c5909d21ae8e2431c3fd18e5d"],["/posts/570165348.html","1591625db22628d2350c83999469e6c8"],["/posts/595890772.html","71b98a87db13758890a5b160799716a2"],["/posts/694347442.html","3b962d2d7e3ffbbfc8678421c4fa43ec"],["/posts/707384687.html","258926bb30ae3f2914f4236aef4b1dea"],["/posts/71180092.html","210c96d5ecc5f77ca4c88bb8cdc5ab26"],["/posts/716459272.html","8044cbad507de08c8c0b9851c4d52e43"],["/posts/795397410.html","32e1ed8f4c77897671c106b63b61a285"],["/posts/820223701.html","6d69b587d0ab45b9d009bc5b825c34bd"],["/posts/830372185.html","0801d264978734466dfa9604b3e52cc5"],["/posts/88294277.html","289cc4633b9834160d6b0622319db0b5"],["/posts/939963535.html","14d0fe7ea372f8ff3b1402a1b481761e"],["/posts/983786067.html","dd16a33861d90ff413ccc5d5f5c9833b"],["/sw-register.js","2fc65bfabd1f0b9b12fd9dd9954815e4"],["/tags/C/index.html","d7f39935888cd8ae9ab7f5e362d91597"],["/tags/C/page/2/index.html","57dfef7d2d83f13f56b499e71e78fc78"],["/tags/GUI/index.html","2923ddd0f3af4831ce97b6a5c7acb782"],["/tags/HBase/index.html","d26651aea924e694350c974670784a1e"],["/tags/Hadoop/index.html","1f6ea58461d68c43c029014ccdcc17a7"],["/tags/Java/index.html","232a531f4542e9e3205d30646f3f0762"],["/tags/Java后端/index.html","9624a302ed799adca577d8e4627b04a4"],["/tags/Java基础/index.html","505fd7beb7231cd1858b2c678d7d5c7c"],["/tags/Java基础/page/2/index.html","a45f1e19ceb91af33840ad49abd999a5"],["/tags/Linux/index.html","136fe97cc40225c1b82ed8161e5d4c84"],["/tags/Linux/page/2/index.html","d56fb485856606064d1dca51ea5ad22b"],["/tags/Mac/index.html","b8046ec7141ae01ba2a0c84e37eac952"],["/tags/Maven/index.html","fcb9f831908bb7454c9116ba00dc552c"],["/tags/MySQL/index.html","eca78ec831c03b38dc13afc2dd956719"],["/tags/Python/index.html","35c50b14f435e37656782303a1df1157"],["/tags/R语言/index.html","bee89d0cf16639fb6d4acc824ea5e63c"],["/tags/Ubuntu/index.html","2c6393bc7cddf07ad924ecb15e2a726a"],["/tags/ZooKeeper/index.html","fbc20895215f222081ed6b75aab2d8e9"],["/tags/bfs/index.html","80596f61bba70a42d07cb471c5355797"],["/tags/dfs/index.html","e56c7148db1082078c43d5a79cba0a76"],["/tags/folium/index.html","df1f1f4edfe0bc99ca56b4ec8a9b1bbb"],["/tags/git/index.html","bc4152814d129bb0ec4285c4a49dc6ed"],["/tags/index.html","b5200ccf41e796904ce3dc02cb6d994e"],["/tags/latex/index.html","040ca54a4f8c88cb6efeb6b042d6f64f"],["/tags/二分查找/index.html","dcfc85ca413f013ead910d90611d2cad"],["/tags/优化类/index.html","2bbae66abad0e43844483df1fef419c5"],["/tags/前缀和与差分/index.html","c4997986bcb5fb7f3bc625ae3def8bd2"],["/tags/博客搭建/index.html","0a98c290bf3f53601e0278c06f2b5565"],["/tags/图论/index.html","81ac1dd5d1939be8a93f50ec60b75d75"],["/tags/大数据/index.html","1dd926c955c154c992c3c2ade2903422"],["/tags/操作系统/index.html","0b26696f830f1b482a6b4cfaffdc17bd"],["/tags/数学建模/index.html","5c17d93cc941074138c9064af8ca5295"],["/tags/数据库/index.html","ad878e417eb11e38e857d8f1be2c22b6"],["/tags/数据结构和算法/index.html","1045749aedb3e09f6d12c94613650763"],["/tags/数据结构和算法/page/2/index.html","28a8ed34fcdd4d167a463218920e6852"],["/tags/数组和字符串/index.html","69c042e6af3665ec2997e8192274a55a"],["/tags/枚举类/index.html","4953ba3a77191cfe4bd95b607b700911"],["/tags/栈和队列/index.html","36fedae3ca82b0bf7b8b51b5d3a5bee0"],["/tags/树论/index.html","671f68f901c2abe23647a8c9c03359ec"],["/tags/测试/index.html","4641b4a6b8025bffdd0bdbab0a64cbee"],["/tags/环境/index.html","d15869ab25d1ea91acd1b5555e0d6fd8"],["/tags/环境变量/index.html","1f65dab7244a9e9c8d1ec6d82ebed1fd"],["/tags/绘图/index.html","4e6a4b367f69db7d442b2e60f71bad23"],["/tags/编程环境/index.html","f4e5e3d9ad3fd82231ef0cd2ae036214"],["/tags/网络编程/index.html","8d831481c962802680bb04ef3bdc4df9"],["/tags/英语语法/index.html","3fdceddc0cec83da773f73035505f0b9"],["/tags/论文/index.html","451a721ab0832a41b795aaedcd3dd8c2"],["/tags/资源下载/index.html","ecfb105464859dff241296e44a0b04c9"],["/tags/链表/index.html","5262ec2f3724e4dd9c191d9b790a9f28"],["/tags/集合/index.html","233669d42ca6d8f8f05ff2c994dafecb"],["/tags/集群/index.html","80ee208c82f1aa96abdff5a8a4105768"]];
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
