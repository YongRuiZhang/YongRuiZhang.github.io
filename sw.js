/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","437cdbb0310b6c4c78a5d7b9b90cdedb"],["/about/index.html","83d7446a5653771d01cfc5933b126986"],["/archives/2023/01/index.html","8c47937384d66ff354fa46e1268acfd1"],["/archives/2023/02/index.html","12f67b697b0e803aedfdd779e44d2847"],["/archives/2023/02/page/2/index.html","e34813546456d8b17a37712dc6bc0588"],["/archives/2023/03/index.html","9b0c8b2db3675ea852669ac6f1f0bf0f"],["/archives/2023/index.html","6949dbd99c72052c6e43747bbef0b0eb"],["/archives/2023/page/2/index.html","bde9de075d6446ce5b2374be4c67d309"],["/archives/2023/page/3/index.html","8ac85ad62f7a84b19716a08ba5ce1fba"],["/archives/index.html","6bb043b389b91bc70318e20880565fc1"],["/archives/page/2/index.html","479e6213cbc43d953510ef635c59f6b5"],["/archives/page/3/index.html","78584be801c74ee86bf96d6a6d68dcc6"],["/categories/Java/index.html","ec9903f44747063697fcbb27f755ce5e"],["/categories/Java/后端/index.html","072395a622f9bf49a7b5e27674ba1b1f"],["/categories/Java/基础/index.html","446788df421652f6f30d2e48bb9e635b"],["/categories/Java/基础/集合/index.html","786f1846ba9f443663f6d0b7046f01a0"],["/categories/Python/index.html","00e3c273efb7acffded43ff9a6f246ae"],["/categories/Python/编程环境/index.html","bb99ea3bb8585cc2fb8a30da18ce0d10"],["/categories/R语言/index.html","62ef85ce1e8e3cb98b25fa910fd243d3"],["/categories/R语言/编程环境/index.html","8515a291bd3cea89390ee31c01c0fba7"],["/categories/index.html","9dcb4c9cbc74014a8fdf24852e9fa11d"],["/categories/大数据开发/HBase/index.html","507ac1c9c722f5883109f8a6598e4d69"],["/categories/大数据开发/HBase/环境搭建/index.html","ba8fe810e736bf852774c44871f58a10"],["/categories/大数据开发/Hadoop/index.html","f9e88c76e8980cf92af14c70050bf09a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","614c22c815584fd9105780175edc0ae9"],["/categories/大数据开发/Zookeeper/index.html","f4a6fc57428bcefe34c8e7b265961444"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","98bf4b5eb6a36fcdb74bf75f8bae45f9"],["/categories/大数据开发/index.html","6cb836fadc3d78389ae984ca53f669a1"],["/categories/操作系统/Linux/index.html","cd2a96d27036ea5d7261b170def2a3fc"],["/categories/操作系统/Mac/index.html","c0ca6ce585542a47384cd508e37830e7"],["/categories/操作系统/Windows/index.html","f0e3489159bcd5e82a0ab85bf5c43157"],["/categories/操作系统/index.html","279e5e732097705cbdc13446adb6ec33"],["/categories/数学建模/index.html","32b9986198ce9819c875563ca1bac1f4"],["/categories/数学建模/latex/index.html","e429636b0416286dc9e20239623dae8f"],["/categories/数学建模/优化类/index.html","c64e6676648383c12cd31c90bba9129d"],["/categories/数学建模/优化类/现代优化算法/index.html","e84d11020b9b25d737afeceb895d5515"],["/categories/数学建模/优化类/规划类/index.html","164968b345116c2c575a3d3ace42ca51"],["/categories/数学建模/绘图/index.html","ee67af7f3a6c447ac22d7e5e665083de"],["/categories/数据库/MySQL/index.html","b431dc428e2d6526fe48792f1b5c0e4b"],["/categories/数据库/index.html","d2d58fea40a6352d406a99861c00c40b"],["/categories/数据结构和算法/index.html","a1b6fd00162b7366c4f468d9589e546a"],["/categories/数据结构和算法/基本原理/bfs/index.html","6e6ebe97253e93dc7910bef17b69b5e8"],["/categories/数据结构和算法/基本原理/dfs/index.html","9d84e83b085ac8dba828f5fa0f312b5e"],["/categories/数据结构和算法/基本原理/index.html","f8770902b105ee413b662c4f4054949c"],["/categories/数据结构和算法/基本原理/图论/index.html","92e6f54c4ff7476717acab123ab516ca"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","427ba2d6c7b8d071703aa229a3d6959b"],["/categories/数据结构和算法/基本原理/数论/index.html","cfce179b34a8242d39ea64cdb94e5bb6"],["/categories/数据结构和算法/基本原理/树论/index.html","4db342b08907ceb3a026f353f3829c15"],["/categories/数据结构和算法/基本原理/链表/index.html","e1e99b0f297da0a2bb4ccfc5cd8a4e7a"],["/categories/数据结构和算法/算法题/index.html","a9a43b6edd832e2faf5e854f56496642"],["/categories/数据结构和算法/算法题/二分查找/index.html","26ab86ea88294592b38003c132fdef80"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2dfe2e72c7e8ef866910997b24bcc84b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2c0d049f75c80c266203c5284c6b1b0e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b7b4e30575b729e9356661a55839cdd6"],["/categories/数据结构和算法/算法题/树论/index.html","7f868e071f8adf9fa243635447a2a445"],["/categories/杂七杂八/index.html","c0c67803b8d6a095e74178f05cff8615"],["/categories/杂七杂八/博客搭建/index.html","e5c2e35f822ec838dc1fdf28c28cb46d"],["/categories/编程环境/index.html","05061cd93d473537612467c01e3880af"],["/categories/英语学习/index.html","e302442749978c04b1cbd254895cd9c1"],["/categories/英语学习/英语语法/index.html","b5e0b9f131004dcd377a1d8694369ef2"],["/comments/index.html","54a68b81be58536455dc3272b6e32bba"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","94ffaa6e98f9e173596c229639cb941d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cae4114a295672b3dd6b92a023f7831f"],["/movies/index.html","5589ec8eac9de5a20c2d6fbfa7dd997b"],["/music/index.html","8530b1ea83cb02bbd00cc0872f98cc60"],["/page/2/index.html","4ddaefadd6f12823b776028457f8d114"],["/page/3/index.html","6260fd957aea4fccf6ae2f8ce4c9bb05"],["/page/4/index.html","c02626205b45380411ffdf0c189e3028"],["/posts/1021360842.html","6d74ac72fed1e53696f11240494e9084"],["/posts/1120620192.html","27c0aad9aed118577adb2573f5e31e6c"],["/posts/1141628095.html","0678d025c7a8f7792d7ce87fd869ea96"],["/posts/1168613674.html","dd71b3058838ff3791746c68cce4e323"],["/posts/1219920510.html","3813dd5e4c84933433035f6c73fbd3e3"],["/posts/1222166338.html","fac8fe5fd3e0f7835ce74f21b58da15e"],["/posts/1259097482.html","c94d4e86589b0ce748a409b8a4bd0519"],["/posts/1271036369.html","4ab416a22703f3c0baf7ffee34a472b3"],["/posts/135355774.html","b6310a197099e93b0b4df226ffd5b0be"],["/posts/1375344716.html","447070255bec17d87ff5bcd70874c984"],["/posts/1388991698.html","f8251219b48364d5317d4fe048033bbf"],["/posts/1410315814.html","b951af12caa038c888be4b701a381ea8"],["/posts/1452790229.html","c1376cc9a4c31f5a17ea6e2516178d27"],["/posts/1470079884.html","25b09ce0d01ec2699936b593781e58dd"],["/posts/1470079885.html","af5d70a00468881e9efb98dcf417a261"],["/posts/1470079886.html","1748c3407b88666bdf0eba18e9c0fddb"],["/posts/1470079887.html","df0be6df0e5a7420f6b39e8485061731"],["/posts/1498536549.html","07404a10ee60b46160c71babfe20cceb"],["/posts/1571776361.html","7fc1b902c5cf06b4175877608497fd8a"],["/posts/1605124548.html","729e6ab456a943b81d8256d585afb826"],["/posts/1817748743.html","9d88a8033b79c064e374cd98248697c1"],["/posts/1925125395.html","3fc2dbc599091b19257f5248046accb1"],["/posts/1966191251.html","da75a75ecafe0495cd6c6942a26c7a4d"],["/posts/1987617322.html","c93dd5ca1dd30e99cd8a393cdca25582"],["/posts/2075104059.html","74a2d1366080dd1a3ed9edfb1e87be82"],["/posts/2087796737.html","624c8a78fd7cb31e667000ec38a7a19e"],["/posts/2207806286.html","e3cdfb96977b40f43e5712ae5d7d2f3b"],["/posts/2225903441.html","240dfd5973055fa2b60f3d5b3b05a3ee"],["/posts/2281352001.html","c7da12a50ef06cfefa4ad9fc84aaa9fc"],["/posts/2364755265.html","0fb0bf1c6f16a30d7da36f490a51919e"],["/posts/2414116852.html","1bc2b22d1ca1b5c7ce79a4ca4a80e854"],["/posts/2495386210.html","fa18b8eb8a39d2a48527685fc03ec269"],["/posts/2516528882.html","de13fd08d03a51ae58debb7c90f0e10b"],["/posts/2529807823.html","12dd5e890faa8f745105d3c647f99786"],["/posts/2888309600.html","82f2bc8bc45786d00422f4d3a68f120e"],["/posts/2891591958.html","3a2fbbf067c2453185b5ed2191706f75"],["/posts/2909934084.html","d9adad142e664966a738a18cc95eb3db"],["/posts/3005926051.html","c8e04961c83277cbbfe4cd5647fd19c6"],["/posts/3169224211.html","1377b3e8ab247e28e075505238bda7d2"],["/posts/3259212833.html","99cb5e58a58ff49b4bdc2d9677278dc3"],["/posts/3266130344.html","6a6afff407673c6d90921eebe7497500"],["/posts/3306641566.html","b937695b971f39c703d6e270268753fb"],["/posts/3312011324.html","45c0b8c7f9c4d874cf656b88cd7f783c"],["/posts/336911618.html","09c1d49523318c416772cb6a28b94333"],["/posts/3402121571.html","ad3824031c56549b8ada0109fa5011f6"],["/posts/3513711414.html","9cc441908ae0f2292013f2f79c93b322"],["/posts/3546711884.html","9cae9243ab73f753c5501b65c07cf583"],["/posts/3731385230.html","ceabf8c84915d1baf4b60122ec6b404b"],["/posts/3772089482.html","5bf76ef71a97fbb5bc1a47c3673537b6"],["/posts/4115971639.html","282dbb5c7cb292c943230fb64af3b2b1"],["/posts/4130790367.html","51f98ce665b28c6f3554c826ca72ae93"],["/posts/4131986683.html","e50f72f68a4993f3e8dd1c65763543aa"],["/posts/4177218757.html","8dfa7283eb654f876092294d46d6fdcd"],["/posts/4192183953.html","f1d75177bf9b09acadf7241ca72e9528"],["/posts/4261103898.html","20741abd5901923e89f24c301bd7362a"],["/posts/482495853.html","09b165547482ee00a7d228110e62fd4e"],["/posts/488247922.html","378380fce5e312cab592a29ff5b07f8f"],["/posts/570165348.html","56bbd9c94fc61beac35c11119dd6a187"],["/posts/595890772.html","5216a4c75bb059d5e12ff9d1c230e0b6"],["/posts/694347442.html","7fb5d793e29b54cb6a05b6677f7d298e"],["/posts/707384687.html","c8ce4b4c1d663559f5f264b5b136b918"],["/posts/71180092.html","4599833134d20eb01773c122a37558d1"],["/posts/716459272.html","5bac7d55cfc4882816197a938402920c"],["/posts/795397410.html","044f215c9e3d006402b9e758b924b4d4"],["/posts/820223701.html","fc64d24952bef5ccb72b034d8105ab79"],["/posts/830372185.html","ed703e50827a1eb777a785fd301d98aa"],["/posts/88294277.html","3021bc940e2a480133c9320cf2bd4b90"],["/posts/939963535.html","5d0b9a5f70fcb8a0a24921bde4a8fee4"],["/posts/983786067.html","d11b909d447c88b938852f4ad4438815"],["/sw-register.js","b999949d7aed7b6657f94c794d32cc6e"],["/tags/C/index.html","6e6b615b1a8b283f93b48bfe45bfb3e6"],["/tags/C/page/2/index.html","ea6c7dff1a6855ce525b497b6ae8c706"],["/tags/GUI/index.html","e8087a21e0a1d81656c8f821ee8ec0d8"],["/tags/HBase/index.html","a67b116190d404dee4ea228d6935b3dd"],["/tags/Hadoop/index.html","20eb01ad734d9fcd7bb82aad6c235c91"],["/tags/Java/index.html","196866ca6bccc95e3a1c39261bc32331"],["/tags/Java后端/index.html","ffdfdf40952865c92254711c008b10bc"],["/tags/Java基础/index.html","43abe530806e6abc2f497e02c8bd2a51"],["/tags/Java基础/page/2/index.html","8b211ec36681eed9aefe1a632657b3b0"],["/tags/Linux/index.html","ffeb99f55a229e803f491668c6baec60"],["/tags/Linux/page/2/index.html","48aa7834e5e144a510df8d0467b0c08d"],["/tags/Mac/index.html","59c6effa7f9a9bb1ac179460e185396c"],["/tags/Mac/page/2/index.html","1d9698e55e4de9b912c6271a46147ca8"],["/tags/Maven/index.html","aea794ec8df732a9f44809b8b1a64ddb"],["/tags/MySQL/index.html","7d3a7d05bb4953d658f089a181ef4039"],["/tags/Python/index.html","d215e7a96a64c08187740e1ee0777e23"],["/tags/R语言/index.html","655db25a280df6ea945987c59654cb5b"],["/tags/Ubuntu/index.html","92a58b85bea324fff0c5247fb3b99f92"],["/tags/Windows/index.html","d7b2e8b07da60b471fc69668c93aec4f"],["/tags/ZooKeeper/index.html","ffd37baab68a8a24e76ff411128b00a8"],["/tags/bfs/index.html","76c777c53faf502cd2d6b6d17d835119"],["/tags/dfs/index.html","4b2c3d45ba539dbc47229c3df67b6a8e"],["/tags/folium/index.html","336753d3285d35cf6741bca89b3cfb1d"],["/tags/git/index.html","5f51cff1021c31115d57e4e5eb80276d"],["/tags/index.html","057f560f46c1d7f041198b0bb6818b3e"],["/tags/latex/index.html","06b6f2eb65d81592e494bd1611ee1d87"],["/tags/二分查找/index.html","74e2eca1e5c1a079d93865e9d5c935b2"],["/tags/优化类/index.html","54ed0ab07c713d71846c7877dbf5baaa"],["/tags/前缀和与差分/index.html","886d59d6716e9db7a75570d32aadafcf"],["/tags/博客搭建/index.html","d972a0fc7b9c281ea5004222a32a3d10"],["/tags/图论/index.html","f9b2cdb1ddfca7b3b5b0f466ebe64f85"],["/tags/大数据/index.html","94a37d4577e16fa1b87ec676b17bfd71"],["/tags/操作系统/index.html","6a907467c9ffb9bd9078c3fc142f6f86"],["/tags/数学建模/index.html","f08b63fa1275dd714cdd27277917ff69"],["/tags/数据库/index.html","97d030dab1ff5c3cdb073c24254237dd"],["/tags/数据结构和算法/index.html","5fed3ec9c8f7e0b0c7921c4d28a04953"],["/tags/数据结构和算法/page/2/index.html","afdc74e4db6bf65b8481c7b68e5b5a77"],["/tags/数组和字符串/index.html","106e6b8e9a304aeea39108be796ff792"],["/tags/枚举类/index.html","f66eca9267181d716bc0e890bffdd9c4"],["/tags/栈和队列/index.html","4e7b707ff82e5ae38694049298e8d8b3"],["/tags/树论/index.html","bb4b0e3f0a6258c5ed5fec8eafd08b99"],["/tags/测试/index.html","a8f6ff845e075242e7c936adf237b25b"],["/tags/环境/index.html","0d388b5ab8374aa7c86cc6480324f2a7"],["/tags/环境变量/index.html","2ca25caa303da6722b1754fd17b0c523"],["/tags/绘图/index.html","f0a1bd393b65024df0d8f8155e9bf6aa"],["/tags/编程环境/index.html","37fa7194c5c35e13cbae5d7a4cb0ccc5"],["/tags/网络编程/index.html","2d95f485a4aecec582a9304121bf5cf1"],["/tags/英语语法/index.html","2c4d93dd01657824928d08b294c2e77d"],["/tags/论文/index.html","2f7bfed8b952fc1eabca85feb178a04e"],["/tags/资源下载/index.html","efe6cd4a0da117c103c27b72c655c851"],["/tags/链表/index.html","1524099e781f8b251fe21fc63566f078"],["/tags/集合/index.html","5c3be229e5069b884dee1750994c4f6f"],["/tags/集群/index.html","9439c63708f890590e7e2a7fdfee332f"]];
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
