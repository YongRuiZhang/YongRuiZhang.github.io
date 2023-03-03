/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","13803f8c0e9eb4f1b3ac6706eceb1ba1"],["/about/index.html","39ce936751af47684011d0f6bfada057"],["/archives/2023/01/index.html","21cabf0dfa733abcb6adddd5a2341299"],["/archives/2023/02/index.html","01e0075d563154daa4d0874345a9b673"],["/archives/2023/02/page/2/index.html","4d0fdd4f64fada6fac7dc22d46d8ec9c"],["/archives/2023/03/index.html","602c5b13b9bee08f88159c40799af7ea"],["/archives/2023/index.html","a2c1b9eb8d401b388b1e0b2eb269add6"],["/archives/2023/page/2/index.html","4e79f4028448944243931bf076c102ae"],["/archives/2023/page/3/index.html","2ca3ef7ee70911e2e3ce349faa0a99d3"],["/archives/index.html","b7392c856f10ea37ddaa70c943208492"],["/archives/page/2/index.html","7fdead9c667da7593c0e16f6ab763e6a"],["/archives/page/3/index.html","1d660efb8982287ce6a86e27be62f8d8"],["/categories/Java/index.html","e981659b910085d1b218dbc190bcac74"],["/categories/Java/后端/index.html","10d502c0df39dce5401ec456f3071f34"],["/categories/Java/基础/index.html","f12f1298cb1a517fb433a97144fd1ac2"],["/categories/Java/基础/集合/index.html","737cccb5ae721accad5857d9062f029f"],["/categories/Python/index.html","ed6bd63838074906f9cf4e4c08c1a0b0"],["/categories/Python/编程环境/index.html","6b8754a490e79221101058999541dd25"],["/categories/R语言/index.html","a41159a35536fb7e68bd78dc2fca2a08"],["/categories/R语言/编程环境/index.html","c526080e343c082034633c31ccfe0733"],["/categories/index.html","b84c1b2e26716b1fd131bc7fef77f91a"],["/categories/大数据开发/HBase/index.html","d0f2654140f7cc63dc1fe1ab162e156c"],["/categories/大数据开发/HBase/环境搭建/index.html","d335543d387d272b6cca6cdea221fac1"],["/categories/大数据开发/Hadoop/index.html","0ea346e7ac6b0dbcf9484930b16dd325"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b3136c0e58c083766e4e059128ede13a"],["/categories/大数据开发/Zookeeper/index.html","acef274fa617376cc61dfbd9226b1cc8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ce8e0fb26b00e22afca3e08b82ee1a7b"],["/categories/大数据开发/index.html","55b5b7a0e0571ca9538d1ec75fbe4a47"],["/categories/操作系统/Linux/index.html","d28eac62f003267d1f2ffc64cabb9525"],["/categories/操作系统/Mac/index.html","ed11ecf2c7340bb8ef826455f6feaf12"],["/categories/操作系统/Windows/index.html","e7c01376304f61dba3fa47136fbb0bc9"],["/categories/操作系统/index.html","94b09a2366e41ae4c0564f24a30f33e7"],["/categories/数学建模/index.html","6ae0f5a5a9e664812ba4088f90226b4c"],["/categories/数学建模/latex/index.html","c720ed9b9778c9c3b658da4d2976d1e1"],["/categories/数学建模/优化类/index.html","367c46efdaf9efdf6047d1f1178399bf"],["/categories/数学建模/优化类/现代优化算法/index.html","465d6b796780d0e27b55d68bb11114f4"],["/categories/数学建模/优化类/规划类/index.html","36f19981eb758f77af3fbe9fe5457c0a"],["/categories/数学建模/绘图/index.html","4db44ee45137284e5453e4f46067f51a"],["/categories/数据库/MySQL/index.html","71a95c8b201622d106aea5e4f3b86204"],["/categories/数据库/index.html","71fcc50cdb48012cdf035d2ad10f26ee"],["/categories/数据结构和算法/index.html","a35a55da453d795148756d820f83f065"],["/categories/数据结构和算法/基本原理/bfs/index.html","a25648f28e3cb17bdac1a15ff2c79225"],["/categories/数据结构和算法/基本原理/dfs/index.html","74cc602d6f443e7a77aa7ab88a30b786"],["/categories/数据结构和算法/基本原理/index.html","8184d8a576de7a291be46f85e61105d6"],["/categories/数据结构和算法/基本原理/图论/index.html","ee333aa3b5d3c18fecfea4cb3b7283da"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b2c3184b51eb274991c6e9b90919cac1"],["/categories/数据结构和算法/基本原理/数论/index.html","2fc9d9ae19eb1a9456008ca0d61741de"],["/categories/数据结构和算法/基本原理/树论/index.html","27520a0189b8a34554987aa01b338a7b"],["/categories/数据结构和算法/基本原理/链表/index.html","6c9588a99fb05831eb4a6317803c0288"],["/categories/数据结构和算法/算法题/index.html","6b253524ee0867dcd4741af0e8f42f2c"],["/categories/数据结构和算法/算法题/二分查找/index.html","0f60bb8259b0e1b536e41c66e20d0c1e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ba35eac662a528fa023ea214ffee8dee"],["/categories/数据结构和算法/算法题/动态规划/index.html","279a16dfd6f8021d26d6d50a79191608"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","f1a781b3ac9d04f47fed1f4e18e24972"],["/categories/数据结构和算法/算法题/栈和队列/index.html","ec010e962c035b2aaf5ba2797332f8db"],["/categories/数据结构和算法/算法题/树论/index.html","4a6a83b536a01ce6a419524c94c1bcad"],["/categories/杂七杂八/index.html","a6492b1a8bfb42714016389e3294b25f"],["/categories/杂七杂八/博客搭建/index.html","6ef1789c9924303f4426239eb0f0736a"],["/categories/编程环境/index.html","9d834cfba2f0fad26c8cbc1270d1e3a1"],["/categories/英语学习/index.html","bb63ed6564dad3a05fa336a1b73b1b2d"],["/categories/英语学习/英语语法/index.html","32fdf4edf9e699f1b049fa669a158c72"],["/comments/index.html","02589088ee5061c02bce1e1476bf5696"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","dad57b06bc59744e6321fd044ac2f663"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","3bc935fa9495493389f652b4e8e38d93"],["/movies/index.html","dd4b21d6cf9c2ceb0b5e503f0dffa0f9"],["/music/index.html","00312ec5e82eef97d3df8ecb35198236"],["/page/2/index.html","defe79bb9117fe03222bbf58f3b3ece3"],["/page/3/index.html","71ead8715e098ec6c3a5f3e654de6a33"],["/page/4/index.html","64c0f07b4af1be889481deb204056aee"],["/posts/1021360842.html","1e297d88734cd3dfc0e821a9c185eac4"],["/posts/1120620192.html","6d152f3dbb7603564a1bcb09e15b3017"],["/posts/1141628095.html","73334c64e11de5e2cde7416c54a3f45d"],["/posts/1168613674.html","d3aa66a869961020432ffa251451e6d4"],["/posts/1219920510.html","554ec3ec8c1d185677d8897bdcfe6293"],["/posts/1222166338.html","51e85580ed833e387ac2741144a526f0"],["/posts/1259097482.html","8c3176cbcb0b7f36f8a1492f6682ca95"],["/posts/1271036369.html","8da2b48de96056638f00b56323787276"],["/posts/135355774.html","1799faee615389214bc6af41e384e07b"],["/posts/1375344716.html","3bf050a64881afaa74a9c2483080335e"],["/posts/1388991698.html","f4f11c19ade2fe41c1269e5523bea748"],["/posts/1410315814.html","d6e0c6d89a2b74e149319247839e5074"],["/posts/1452790229.html","bc0ab13e7fafb9e48c6202849841265c"],["/posts/1470079884.html","09e21d551a42a1550766c2d8c08fc67e"],["/posts/1470079885.html","be1e66ab849a6359564f3de1f8c098a9"],["/posts/1470079886.html","400b5324ba7f22f5a9bb5ae817e0dc81"],["/posts/1470079887.html","c752b65fd4ce0565179a466ae5e5f52d"],["/posts/1498536549.html","b0bb4e5b9866ad7b9aa72c345756d509"],["/posts/1571776361.html","2d61b5d13a5e1c9590dfb1d7d42efff4"],["/posts/1605124548.html","2772158637a7887d68d5537c437f14cf"],["/posts/1765123828.html","900a88cf443e311efaf777e4c8ba1f58"],["/posts/1817748743.html","da6b4b1e22a116baafc40eec8b198679"],["/posts/1925125395.html","956ec41134cfde3da6acdf50e7ad4f1c"],["/posts/1966191251.html","584c432ce3f6a8a697302ba2de0c993f"],["/posts/1987617322.html","a1b795356e36e94c40efc15c86ba00bc"],["/posts/2075104059.html","108b8ca833ab963d364d318b9af72877"],["/posts/2087796737.html","820ab775e6735623fe15c90405b85a45"],["/posts/2207806286.html","646f0290dc08c1b98266ed86b24d2dc8"],["/posts/2225903441.html","9944460d7bf8a03f87fa2a31bdf0965e"],["/posts/2281352001.html","8a6a8dd12ea97ba73ccd3dd7d5efa4dc"],["/posts/2364755265.html","09204e5102ac74412d59007751832fcc"],["/posts/2414116852.html","e03b21868965117af8988890047e4750"],["/posts/2495386210.html","f0021a4a0952cbd388a929de44213d2a"],["/posts/2516528882.html","c93cf0641f24df4764782c5c0d4d26c7"],["/posts/2529807823.html","7f8700bfede53717c45c736d2e79ba23"],["/posts/2888309600.html","303cc0513bf10af7e14fe2f361c81c7e"],["/posts/2891591958.html","da713d0e08efb973774a9f5f53c97a6f"],["/posts/2909934084.html","63806e18cfe946008d7b57fdbb87a6d8"],["/posts/3005926051.html","96295ea457c99da94812c5c645188f71"],["/posts/3169224211.html","c9d6465734cc8b4fb4c5049eed20f70e"],["/posts/3259212833.html","19957b83f73d4b9fbaa844a1c51fb053"],["/posts/3266130344.html","29dc5790da3dda87628d98dbec2b3a1b"],["/posts/3306641566.html","3efab8e82d68ac1801bd64dcd6851125"],["/posts/3312011324.html","fce14d3a058c00907c450435c1a35cdb"],["/posts/336911618.html","321dcb9bf4c1fd78f3956e6399c70bfd"],["/posts/3402121571.html","ba3ce4e85c220f75c4f7c9739e2a6f74"],["/posts/3513711414.html","df995cbeb235239757c652752d08e57b"],["/posts/3546711884.html","a694364718117e37e14079f6c6f491cc"],["/posts/3731385230.html","bbccc8d2fdb80b2ac41965d0aa44ec29"],["/posts/3772089482.html","c0fd7059c6ad8ec1c41d6f01257363dd"],["/posts/4115971639.html","a60006db44b8d0147bd2a77db5ebac47"],["/posts/4130790367.html","a6b3cb4f9f636169b7b962e5c4b4883d"],["/posts/4131986683.html","9cf48682593087235f8c52e9cf1e56bf"],["/posts/4177218757.html","a6a938b08b2b23fc38b9669c53996cef"],["/posts/4192183953.html","88ab7fc2d753a8b1b1b6f10f0fbb3119"],["/posts/4261103898.html","104a5f91ce13fd611aa347bd0210320b"],["/posts/482495853.html","17652115c72ca853fae51138fc102a25"],["/posts/488247922.html","8862da13c52f266b58394c100d9ad390"],["/posts/570165348.html","9fff9f4117b5d7b0d1b9273e4b79009f"],["/posts/595890772.html","4c77f245629c0c15d627f4e3967b88f0"],["/posts/694347442.html","dd3611732eb652afeb8f04b8294b29d9"],["/posts/707384687.html","9bbb0f1016780280cd5df09b4a3f56a5"],["/posts/71180092.html","1f2b6801d5fcf7738d9ddfea49740952"],["/posts/716459272.html","651cba7ac543e246ceb96b87b0f73ed0"],["/posts/795397410.html","5bf55273300652b1ff7dfb0d55cc13ef"],["/posts/820223701.html","7d40e0477363a9713001017dd0482bc8"],["/posts/830372185.html","28cedb2ced5730c0bf22811cb0793a83"],["/posts/88294277.html","150c104b1b9e96af8355ecb95b2f3469"],["/posts/939963535.html","87eb11ef4aa75fba60eb8c10bb654cc7"],["/posts/983786067.html","19b69e02e8d45d42e0f15b89cece3044"],["/sw-register.js","d879439d684961a482782d633972be35"],["/tags/C/index.html","b8f8035222c7a7113e41f3e660716850"],["/tags/C/page/2/index.html","78ebc44319b1fb3f571ac40f44457597"],["/tags/GUI/index.html","a64481eaf826808154f655d391f391cd"],["/tags/HBase/index.html","9903e20da63b288642b1dcbf10e8b0a7"],["/tags/Hadoop/index.html","c37d2369ded97156fb7e7ec36ed14a78"],["/tags/Java/index.html","5a4e75cda2df76cdf1cf31217e123c60"],["/tags/Java后端/index.html","a4ed6e3bbbb3e4cfcda6ffa08dcdd265"],["/tags/Java基础/index.html","f04baa29476eb6e5ee10e3ff2da4f2bd"],["/tags/Java基础/page/2/index.html","605a5001604730c0ec3d0cb5e90e6a34"],["/tags/Linux/index.html","1d7904722cec59bdaa23fc6ae62fbc29"],["/tags/Linux/page/2/index.html","d029c158b0181d84dbeaf923e2fcdca3"],["/tags/Mac/index.html","982c6535932f64b7131b51ad191057fa"],["/tags/Mac/page/2/index.html","57d7d0bd7bd4f588774e8ce7680b4fc6"],["/tags/Maven/index.html","c514fdeb2f91db419b1e7b19a31b33cb"],["/tags/MySQL/index.html","f93a4ea036c0ac563cf37f901214390f"],["/tags/Python/index.html","204fd5b17b590086fb6810c9750ac2c3"],["/tags/R语言/index.html","f9fa91416cf88cf7ce9599f32947b171"],["/tags/Ubuntu/index.html","0e202698797415450e17e60089e01b4c"],["/tags/Windows/index.html","40b5f53d40d048d06f221530e333f90c"],["/tags/ZooKeeper/index.html","4c452cb5afdbba1bdf366dbbb9c8ff6e"],["/tags/bfs/index.html","e53a39fe6b7f759f96866ad343356a09"],["/tags/dfs/index.html","7975e2c2bacafa7adf30f1ebf1d154ac"],["/tags/folium/index.html","629507b4539e5511259c38c04e5ce326"],["/tags/git/index.html","a613f500db3666db3cc548667bbbf437"],["/tags/index.html","1e4b5d8067803914691307ac23ff8576"],["/tags/latex/index.html","cb908a1ebd3e67b5acdeb95b549da365"],["/tags/二分查找/index.html","724a8c5a833772c70d8755082d176eea"],["/tags/优化类/index.html","502610aaef328ac94faaca37492e2ed5"],["/tags/前缀和与差分/index.html","f090a8d4c69c262e104380751cf3f398"],["/tags/动态规划/index.html","2b9e6b3ee55705d53c1057186a74b40c"],["/tags/博客搭建/index.html","62f276ac97f05143185e5394d75c718d"],["/tags/图论/index.html","731ebaa0dee052eed42344fff71c2f8c"],["/tags/大数据/index.html","a555486e4d3825c5c1b10b53e25ba3f8"],["/tags/操作系统/index.html","a08cec1ed080d7d32fe2a286f9010758"],["/tags/数学建模/index.html","c42d5c7dca94f347539c3469fd6aeb75"],["/tags/数据库/index.html","4f007d94af17d7ef3ffcf45d87f15604"],["/tags/数据结构和算法/index.html","81ae471654cda8660fdcf77cd2587415"],["/tags/数据结构和算法/page/2/index.html","cf4978922019fef1d09c3cab05ced3a7"],["/tags/数组和字符串/index.html","e2442554eec718d3e33d5aa65e05235f"],["/tags/枚举类/index.html","dba7820a92a5b4e7b04720dadf388142"],["/tags/栈和队列/index.html","ccd09f9689279905e947916201b3139f"],["/tags/树论/index.html","ef41ea430b6c31dc2c2baf23920eef55"],["/tags/测试/index.html","8440b4f510323cc25148da6dc27e1d21"],["/tags/环境/index.html","cfd7ade19a87b94d5203a71b07e9cc20"],["/tags/环境变量/index.html","aec26d618ca8ef3e135b826f8b525b14"],["/tags/绘图/index.html","564e4f9a71fa1d22d4ad6b786a62a6e0"],["/tags/编程环境/index.html","1820c64a2cd7c2b5ab0b8f5ba0e2b0c7"],["/tags/网络编程/index.html","0190e8c641668371fb92039c1bfee029"],["/tags/英语语法/index.html","c38a0c62dd46e75da1dd7657b2253f7a"],["/tags/论文/index.html","3d40c4e0cbc853fdc40144bc8afd635d"],["/tags/资源下载/index.html","e18243ac48856c981e9dff4226672b8d"],["/tags/链表/index.html","633c0bbdf79d163ec68ba0d935c2cf12"],["/tags/集合/index.html","281ef7b70ef7714f2338f3f26730597f"],["/tags/集群/index.html","1c317eb14cc692b6c22f4f92dacca949"]];
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
