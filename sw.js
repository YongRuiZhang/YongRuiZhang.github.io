/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","cb15ec1eb80eed697b1eb483eedd203e"],["/about/index.html","ba44d8ff87bc8f941c316dd9a108a81d"],["/archives/2023/01/index.html","21406a956d5884471edd982365b2f18d"],["/archives/2023/02/index.html","929e838a2422abefe5521de7452af672"],["/archives/2023/02/page/2/index.html","aebddba531f02dfaa529bd4af67decc5"],["/archives/2023/index.html","c275f85812142e0b3a82241bc124e871"],["/archives/2023/page/2/index.html","41a1829d561bdce064c1c66521ce9307"],["/archives/2023/page/3/index.html","d8624365a9952ce9fba7fbe29665f051"],["/archives/index.html","441b46afc8dd032bc2f1591f7892795a"],["/archives/page/2/index.html","814643bceb8c14169bd5bf192535b572"],["/archives/page/3/index.html","64b1d4138736fa250653673a1bdb62e6"],["/categories/Java/index.html","d63d2fe29c31a3f3e933c09ecd2fd39a"],["/categories/Java/后端/index.html","a389b18db27b4b4f4a0d18c61d2b55c5"],["/categories/Java/基础/index.html","2583c7ec226fdb123f319db1364e12f2"],["/categories/Java/基础/集合/index.html","540649eb229f1ee3e105cec4d5efa043"],["/categories/Python/index.html","eb7875c7a3244ad42c0d42bfd84c7826"],["/categories/Python/编程环境/index.html","682170c1bff6dc360edc9d0078687855"],["/categories/index.html","6f1f004fce35ddcd89eedb9cf919147c"],["/categories/大数据开发/HBase/index.html","36c8c21416a2c6ba13d667aa0d0c06c6"],["/categories/大数据开发/HBase/环境搭建/index.html","e0612aaf7884112739a0573e036fe832"],["/categories/大数据开发/Hadoop/index.html","a26e312dd2094b997f0381e171a56aba"],["/categories/大数据开发/Hadoop/环境搭建/index.html","7f6edd38a4ca08df0f2ed145468ea0fa"],["/categories/大数据开发/Zookeeper/index.html","c85b7f9527def16e77e019bd6677a2cd"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","d454466c004f0682058dd5ed3476095e"],["/categories/大数据开发/index.html","eba1fce4504453d682caaa6f6cee5b5b"],["/categories/操作系统/Linux/index.html","a5602cd11757480de6390f2ae57a7010"],["/categories/操作系统/Mac/index.html","02fc1a1c9fca10374e336bcc3ab42d6b"],["/categories/操作系统/index.html","11bd22a42b42bbc043fc0da4e2074a62"],["/categories/数学建模/index.html","efc6e3a9ac9061adb0c8262c90f43cb0"],["/categories/数学建模/latex/index.html","ece7632102926204806ccc55cb51fef8"],["/categories/数学建模/优化类/index.html","f323e60e6e4123c4296ea6222b4a8b42"],["/categories/数学建模/优化类/现代优化算法/index.html","82c66c6dc080a72804549ab1947cb75d"],["/categories/数学建模/优化类/规划类/index.html","d6a0a0f9836087214329b3d4e8afff8a"],["/categories/数学建模/绘图/index.html","3867d4fb21b47170ba22d71a3a43c283"],["/categories/数据库/MySQL/index.html","d8495a0fc3bede41823e04b76a9658a7"],["/categories/数据库/index.html","912a5dd47451967809f2888c18b5148a"],["/categories/数据结构和算法/index.html","cf7180b6d740c41bbee33e378405ed45"],["/categories/数据结构和算法/基本原理/bfs/index.html","0566262105591297b77593071ca9963a"],["/categories/数据结构和算法/基本原理/dfs/index.html","3ecde3dff0f4412158c11a196daf2328"],["/categories/数据结构和算法/基本原理/index.html","ed64e50d5b0ae2a79e14130cd718f9e8"],["/categories/数据结构和算法/基本原理/图论/index.html","52769cd7dc7bf1a73e22b037f84a1685"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","04d61173fcdbe2fa6608957d42bb9bd1"],["/categories/数据结构和算法/基本原理/数论/index.html","d6a9a54b4be5da88fcd1aaa1025c3fe7"],["/categories/数据结构和算法/基本原理/树论/index.html","3ebc6e2a23423d592c32d3dfe68ad17b"],["/categories/数据结构和算法/基本原理/链表/index.html","f5597c4442c96ba1c17c8ce918f13b9e"],["/categories/数据结构和算法/算法题/index.html","684c903a5eba1b3897663f46b71d5a2c"],["/categories/数据结构和算法/算法题/二分查找/index.html","d96a7b67b8519b67be560f3340beeceb"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","984fbae7e16e503f9c41be09d072d564"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","7432e7cefc2cae4a9ee10fbabf8fab6d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","73cbe8cf7507864b84eccf2964e0895c"],["/categories/数据结构和算法/算法题/树论/index.html","bc83f6757848e1dd189351264e016c8a"],["/categories/杂七杂八/index.html","616e97128ca0a2ce182e34f615912655"],["/categories/杂七杂八/博客搭建/index.html","1b8b6695fc033c3bbe0a9fa0f14ebf89"],["/categories/编程环境/index.html","be759e7069abfed7a291ef9c77d0fb9d"],["/categories/英语学习/index.html","ed98046099e6e9a708bc10614cc81cfa"],["/categories/英语学习/英语语法/index.html","805afd45e54d60293e712a1649410dd3"],["/comments/index.html","420b63c4b59d187b651a4db18b60384f"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9edb097e969b42e46979bafbf6001f97"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","7ebbc926b3b436471e5b3c4380dfcf8d"],["/movies/index.html","d859f256726013b41a60e30b6decb4b1"],["/music/index.html","8f1e4acce4b5e6198d7c8ae26797eed9"],["/page/2/index.html","5a4fcaa46c25566a6410169ea6ff73c7"],["/page/3/index.html","2eae6b0c3ac45a69d45bdc08067d56c8"],["/page/4/index.html","47d2742d3036e0de2da6c962dc4cfb0a"],["/posts/1021360842.html","2751f9cd89213262f51f8561e1f0add1"],["/posts/1120620192.html","e917f552df5548676f67eb6f2649501c"],["/posts/1141628095.html","b0cf71fa414c47a604b7b7c728050779"],["/posts/1168613674.html","211ce4c840aed3c44265c0018ddc6908"],["/posts/1219920510.html","96fdb733c25da2a81f9c95906c24dd4d"],["/posts/1222166338.html","bb9ef489efb3434f26924aea7c860d16"],["/posts/1259097482.html","f5286505a50a767b8d878b92e4184824"],["/posts/1271036369.html","31cdf29d648953f34e9b5727db3b16eb"],["/posts/135355774.html","93d4401aa30c945754ad33c7fb3b98c9"],["/posts/1375344716.html","c4b8554f4db218a5a18125fe0a88148e"],["/posts/1388991698.html","cf08ac79c4850f43b7134b2ad1d96291"],["/posts/1410315814.html","29e9abb23f9bf14306b504e2e5c65864"],["/posts/1470079884.html","c2be6edec8d2a9e7d49c4a8785fc61ec"],["/posts/1470079885.html","264bf1fbb1af6adc42f7e17c5fc97267"],["/posts/1470079886.html","ccdda0222e976f8b63e0429c857d7446"],["/posts/1470079887.html","9bb9dbe388aeaacab8b443c2d4ed26b2"],["/posts/1498536549.html","d48ba17a5ce00821939f6dc8b1d1b904"],["/posts/1571776361.html","7bd9d81d45c12f2cb3767b8c7119bf53"],["/posts/1605124548.html","097a2ad5b7aa3e32bbf0bec88ade188f"],["/posts/1817748743.html","7d5d754a4dc3e18311437513dedd702a"],["/posts/1925125395.html","d9c93065b5ff1b5c8bc664a96a551cbf"],["/posts/1966191251.html","39d76bec3e92550163b3d2cf1b822bb1"],["/posts/1987617322.html","4258c8d6243011080466539da948739f"],["/posts/2075104059.html","069cc67b519dd70fc6f62a19b4d242dc"],["/posts/2087796737.html","5718e740797a57bdae671ecbe8f83f06"],["/posts/2207806286.html","e02a0b518ca9956723378eee5badf0de"],["/posts/2225903441.html","f11d900d53211313a815412c037fea0f"],["/posts/2281352001.html","dcc791b8b0c33d0182fa180e1c07f323"],["/posts/2364755265.html","7c1b6d0c334d9cab42393eb1c9ef2595"],["/posts/2414116852.html","f48d67ca4d4720420966e8a91881293e"],["/posts/2495386210.html","bcb986f7c6e3807acc0edfffd52c9275"],["/posts/2516528882.html","ce2433837e54806365348ab953e3ec2a"],["/posts/2529807823.html","866d3dea0db72bebf2541859d38df5b9"],["/posts/2891591958.html","109bf04081ba81a4270e99142249b73f"],["/posts/2909934084.html","c8b4d59daddd3421cd081f0618b7856c"],["/posts/3005926051.html","6a6fc3eb52a0ae994e2a88dbe8a13143"],["/posts/3169224211.html","7a4b82773ba722b2949d8e9a44f48b65"],["/posts/3259212833.html","c775bdd003277d4301140b8723011e68"],["/posts/3266130344.html","e9b4c5093a7929677a3c1e7f9e727af6"],["/posts/3306641566.html","9d10967b2b8c600aa7aa48d4d9264722"],["/posts/3312011324.html","cc6103f0459f8d70cafdba7a54710500"],["/posts/336911618.html","b8f922a4c88a3d5a7894495cf99a12be"],["/posts/3402121571.html","76d76c3e8f0b924351b2e3f55308cc6f"],["/posts/3513711414.html","bc1534a92bd62be3d4b60908b73eeee9"],["/posts/3546711884.html","fadc6dd9555a512768c5ebaf7cb72fb9"],["/posts/3731385230.html","cc0711a210446d9a8fd9bcb2928b208f"],["/posts/3772089482.html","45cd6e9204067210d419ea03841ca224"],["/posts/4115971639.html","1bc465f23c79c42ec0dfc292996bce80"],["/posts/4130790367.html","d2506d9941effbaa27b09477057e2a75"],["/posts/4131986683.html","e32e341d15e7eeab705c3e3793bb28bb"],["/posts/4177218757.html","832cbcaa9d497840ed133be75ed8d20b"],["/posts/4192183953.html","aa10b9a218af83de9f67045612aa6dad"],["/posts/4261103898.html","5eb79c6bb83c7c583b2a7ae4339f72b4"],["/posts/482495853.html","40b1c3a695d47c138986b36868070404"],["/posts/488247922.html","0edd765954aaad869ef7871d21a099da"],["/posts/570165348.html","e4813d5d509ab00531bbcef6c3e0551d"],["/posts/595890772.html","5dbac63af2d5731bc5c68f66ad5d6a87"],["/posts/694347442.html","f8eb2f7f139b024842746a23170badbc"],["/posts/707384687.html","010ad2e017420f35cc767264f910c109"],["/posts/71180092.html","2f0769fbf969078ddd91ce8b3e34ffb5"],["/posts/716459272.html","281d3108930937440d94b1e7f295ace6"],["/posts/795397410.html","3d1f08a2dc2eef1c75b200ad97b7794a"],["/posts/820223701.html","dd20e791b3a5637c1e7d853578b10f14"],["/posts/88294277.html","ff91b4b558848b824554ba2eede8785e"],["/posts/939963535.html","12b56f982e9d589c8200e1fa34a6b6a1"],["/posts/983786067.html","4b5f38522bf60364472f424ac41949da"],["/sw-register.js","2029b57db03dfdeb2c9f20e84c11121c"],["/tags/C/index.html","82d639939c448ca72f8aa8fa9deeaab5"],["/tags/C/page/2/index.html","c287e20c802171ab0ffe4332738f6025"],["/tags/GUI/index.html","f48ff5e27c559934c917d73d074e3859"],["/tags/HBase/index.html","31513874dce19d8394adf27727b4c935"],["/tags/Hadoop/index.html","e18bb2ea22ece5eb83b7a0a186d03b97"],["/tags/Java/index.html","1ab0a0954972825b42ba5c40711a4988"],["/tags/Java后端/index.html","ffe05415501d626e10d117618f25c48d"],["/tags/Java基础/index.html","d8e43c659ccc397d6473982c0fbb9b02"],["/tags/Java基础/page/2/index.html","18466de7d744bbcb610d3cc77f92648b"],["/tags/Linux/index.html","326444161728930b291c71c30495ca49"],["/tags/Linux/page/2/index.html","5e5a2cfb5a2031001303f7b4fd2dd916"],["/tags/Mac/index.html","a0ea40ef0d842b7920569125331cd368"],["/tags/Maven/index.html","8b962ce49b92fec981727ba3bce99c27"],["/tags/MySQL/index.html","720518e6f8df1238e8641bee7c7e79d8"],["/tags/Python/index.html","409a53333145df3045a138bc84369729"],["/tags/Ubuntu/index.html","8fa830a2422f2c9475d2f8efec288215"],["/tags/ZooKeeper/index.html","48b532118eab9f1f5efc39650c51319e"],["/tags/bfs/index.html","25d37520a4655d4500f1ff0ff795a84e"],["/tags/dfs/index.html","2c9ce463b982776e00c7cc4c2e3ddea9"],["/tags/folium/index.html","51c434e8d2f74ed49eb40c3c1b016937"],["/tags/git/index.html","cd42312347bfd873dde6ebf309de00ec"],["/tags/index.html","d5896b4f83b5abf12ce8603f4d66b60b"],["/tags/latex/index.html","5e33e11864e839a8f0a6065986daf5c1"],["/tags/二分查找/index.html","cb1f2c836acd63f9740c59440b813fdb"],["/tags/优化类/index.html","b78b90fa26f9399f8c52f1b285b29862"],["/tags/前缀和与差分/index.html","68be6c46aece16a0ea668ed0903fa0b8"],["/tags/博客搭建/index.html","31975943713d5d6a568d57a941d09bb8"],["/tags/图论/index.html","1814c6be5ca1d4449d3158bc5195a3c0"],["/tags/大数据/index.html","f41a6cb4ba22610e201cfe3e5e2dfcfa"],["/tags/操作系统/index.html","622ce8e3f535e9be06393dd396919aea"],["/tags/数学建模/index.html","14c488497bec29100a7fe7fb2538fb1a"],["/tags/数据库/index.html","d82404126289b832ce6799387aa16580"],["/tags/数据结构和算法/index.html","1c6afb3e0b611864a54d0b49ccedd0be"],["/tags/数据结构和算法/page/2/index.html","98becbb1c98ef40d7f78e6c7422cecf9"],["/tags/数组和字符串/index.html","580ecca1dbd4860cd08360e2fd6402d6"],["/tags/枚举类/index.html","864e68de812b87922db68c6dc283764b"],["/tags/栈和队列/index.html","f4321bd21ff7ddba5ed386cee94c04f0"],["/tags/树论/index.html","a67811ea581d93990d21481bb984ddd7"],["/tags/测试/index.html","7b5b2ba67e2a1ca0bc22d875c1089999"],["/tags/环境/index.html","ca751a480d8310d4405f1124d91552ef"],["/tags/环境变量/index.html","a6842f039ae9ed6e39ba6b7fc4753110"],["/tags/绘图/index.html","90818dec7f28a242f82a03359f8922b4"],["/tags/编程环境/index.html","bbe632196aa01bc8dd77b325ae35b33f"],["/tags/网络编程/index.html","67a0a3682bacda5167d80a3091c37549"],["/tags/英语语法/index.html","1466d3c9abdf88518f051d2ac7fcb2f1"],["/tags/论文/index.html","9b892691d059acb343f17697f62ed023"],["/tags/资源下载/index.html","755ab2d7c684ac535e193ea12e4c02c0"],["/tags/链表/index.html","7faa6db17228c96a9e12362956ef39e2"],["/tags/集合/index.html","cb752a3da2d4e6745a64eb9bd7270338"],["/tags/集群/index.html","b8d34de57ea9153554c62944605b30cd"]];
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
