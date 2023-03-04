/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","924b599b22206a1ec4e6770ab79880d7"],["/about/index.html","2f987e6fa5fb29bb4839a7f0c55d5038"],["/archives/2023/01/index.html","aa813863228689c754af445650f457c3"],["/archives/2023/02/index.html","f97be7871d85017a81e8b7caf7775222"],["/archives/2023/02/page/2/index.html","0ebcd05989199a0f580cd2401b5b1c2a"],["/archives/2023/03/index.html","bf318248c55ec3b59fee7c5c61e979d8"],["/archives/2023/index.html","3d153836e8966d3b5b4636aea2fcd824"],["/archives/2023/page/2/index.html","7e4cda4f472dfbd3914c08f57a62a785"],["/archives/2023/page/3/index.html","afd284433c6c3593148604cb52413b62"],["/archives/index.html","f844629e4a4f216b58f6bbfb5dd28af2"],["/archives/page/2/index.html","581ab82ecbc0725a23d7df3a01fc2203"],["/archives/page/3/index.html","3b8ce6e6c77f7783817e027f8dfd4f31"],["/categories/Java/index.html","c7550de4c761eba7b9630a23811298a6"],["/categories/Java/后端/index.html","463f944b83f133f67e31e215a65c215d"],["/categories/Java/基础/index.html","b8d3c4fd79f1445a3bc334aac2a8fdfc"],["/categories/Java/基础/集合/index.html","21592d7a97349a2d7f75e0624c9a3560"],["/categories/Python/index.html","4a56bffb2827751fae888d783d941554"],["/categories/Python/编程环境/index.html","a7246af94eb6ed7d6262aabbceb44cf5"],["/categories/R语言/index.html","a90ff97903dc682896523dbe17c911af"],["/categories/R语言/编程环境/index.html","0472e82758b3015cf0279d6976878689"],["/categories/index.html","5f4f8bf236ff57543065901564743ae5"],["/categories/大数据开发/HBase/index.html","7762efbde7b6817cc6be26befa1a1c03"],["/categories/大数据开发/HBase/环境搭建/index.html","cf3e0df352d661fa5f2610df2cf14586"],["/categories/大数据开发/Hadoop/index.html","60059c61006642319a21fabbafd204aa"],["/categories/大数据开发/Hadoop/环境搭建/index.html","6dc238cf83a51c1d771b29c5f5985423"],["/categories/大数据开发/Zookeeper/index.html","b10f1fca40d7f390c8a1816c46217eae"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3507e4d4dbdf642856bf798f52c15e94"],["/categories/大数据开发/index.html","0b87653028f898c05a2262fd39242a84"],["/categories/操作系统/Linux/index.html","d1514d1c9724592e1ab08f43c5966089"],["/categories/操作系统/Mac/index.html","83cf9cd73ff858ddad5a9e1ffac4ca62"],["/categories/操作系统/Windows/index.html","9bcca43ba2f9354cddaf8a25e150f631"],["/categories/操作系统/index.html","fe46d45b41ee1e28867519da9d41158e"],["/categories/数学建模/index.html","b0d5fc2b6978fa0bcf0fd12352d25935"],["/categories/数学建模/latex/index.html","5f3448085638e2f95eb734bce798d7f7"],["/categories/数学建模/优化类/index.html","9a52bb31e5e5cf1ccbb4d83cd49347f9"],["/categories/数学建模/优化类/现代优化算法/index.html","e43b1ce904877f20b9e09a1e018d2bee"],["/categories/数学建模/优化类/规划类/index.html","61cf106822d40b656aabab2c62c85f45"],["/categories/数学建模/绘图/index.html","a689e055e3e142853d3a8cc3f0387fe7"],["/categories/数据库/MySQL/index.html","7d7eb0f1a43e6cf45483c942d271f7de"],["/categories/数据库/index.html","a8e2e1043d929284524320f3d95291a3"],["/categories/数据结构和算法/index.html","179b14dc4439d4768e8a3d2bb8a2b507"],["/categories/数据结构和算法/基本原理/bfs/index.html","522ad1771e238a970786f11db064261b"],["/categories/数据结构和算法/基本原理/dfs/index.html","b63d7ad512aaabbd5bfb1ce6ac18dc7b"],["/categories/数据结构和算法/基本原理/index.html","5ee6b83c6bf718e715394053067f979e"],["/categories/数据结构和算法/基本原理/动态规划/index.html","07f17d3849beac6b519ebd2acabd8a79"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","2ab21393a366657ad8b8edd3c3ff2cda"],["/categories/数据结构和算法/基本原理/图论/index.html","9c84b3e2ed1e76eccbfd1054c3e4eccf"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fa196fdc23dd9696ac72c257c948230b"],["/categories/数据结构和算法/基本原理/数论/index.html","bb39b72663e3a1ac779e0210e2808584"],["/categories/数据结构和算法/基本原理/树论/index.html","44cfe1f67cd536abf790e5a1ed92fdd8"],["/categories/数据结构和算法/基本原理/链表/index.html","b0fc358312f6b139d25fac974a51773c"],["/categories/数据结构和算法/算法题/index.html","6b2f409b9533b2e747726b31c6b33c85"],["/categories/数据结构和算法/算法题/二分查找/index.html","0affaedc211d954377db54d1e5991689"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c4f484cf613c0a15d0bf78c970c03e69"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","df88a426ef5399fe79167ad018d0b49a"],["/categories/数据结构和算法/算法题/栈和队列/index.html","bb77d28958250e027d21ccb49b6a5e5e"],["/categories/数据结构和算法/算法题/树论/index.html","32747fe57a0ab1c0b0481b95d927e48f"],["/categories/杂七杂八/index.html","38363e8402f3f0f3de6c746da7fa113b"],["/categories/杂七杂八/博客搭建/index.html","58b5c1ae2ef5d74d260f1be2c126e4c1"],["/categories/编程环境/index.html","be40e701e794c9c6acc3d6f6c948d986"],["/categories/英语学习/index.html","ab391ecdc895a3e3d3f4ad29ad523e3e"],["/categories/英语学习/英语语法/index.html","fc7124e482af0705d45a06be7f0434ca"],["/comments/index.html","eeeb621c378422047144a74d6ae98664"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ea495e452177662e81876966580aa5ab"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","39023aeb1d369241eb1e1bee1347adbd"],["/movies/index.html","d38f4877207dafd6a9d20ea1e2cf973e"],["/music/index.html","11207e36730139b2371cf66b432ddb89"],["/page/2/index.html","99424b501fba3dc819e24da6e6343d53"],["/page/3/index.html","28f79129f66bcd4d4c8ace177c1416ad"],["/page/4/index.html","ce2565f305ccb0144035915363c7a4fa"],["/posts/1021360842.html","c157c8cd87023fb374d1247c535757c8"],["/posts/1120620192.html","d7d08fd47e2e4a3274f3d723591c1d5b"],["/posts/1141628095.html","73f5ff35cace6d3415d2b6b3dbf7e058"],["/posts/1168613674.html","d7fdb38d8a9549ba3c83cd290d7133cf"],["/posts/1219920510.html","884305ac1fbae4ec1a4e529cd1dc8678"],["/posts/1222166338.html","0e901b82fa97baab6d453ee146a0b039"],["/posts/1259097482.html","775a88d021c7f9aa19b916ddfe252526"],["/posts/1271036369.html","63c9396608244b4d0fa23df4dba237d9"],["/posts/135355774.html","47e3844a7dbe3e778f9dfb22d8d82190"],["/posts/1375344716.html","1eeb680aa23fc5781a919b88ffe95168"],["/posts/1388991698.html","216b3d519b9190c5ac00a1e8cd8fbde0"],["/posts/1410315814.html","3729c3db61bf21f7a4d7132035832ade"],["/posts/1452790229.html","0776607ecff987185dac84159187e155"],["/posts/1470079884.html","b00a671a052fe73c89fa91ea25253e2f"],["/posts/1470079885.html","b70f1f90b53c7d85457c5f824d1ce9db"],["/posts/1470079886.html","d52d27b7779a4c84db55f1b96cff5cde"],["/posts/1470079887.html","dde0ed6d58c6ee89fce063638bed5ad2"],["/posts/1498536549.html","56a1c938cc8f986708a8fd19634d342d"],["/posts/1557866301.html","bfbc805f8c8bb6f3532e1131e0eac90b"],["/posts/1571776361.html","994f75a9ccf4cef2e11f2b68653afe86"],["/posts/1605124548.html","e1ef15730cf3bfbcceddb37bff6e07c5"],["/posts/1765123828.html","d1d0ff344d9e2053a0abb44647c0a32b"],["/posts/1817748743.html","2e4fb4cd74032250743199d964e0ce4c"],["/posts/1925125395.html","aa713039aa621170ef875b1112fc1f2e"],["/posts/1966191251.html","1740bc8d2aee4496b375a575d19d3393"],["/posts/1987617322.html","9c020a97020f0376e9274b85cc9273a5"],["/posts/2075104059.html","458fe3869d955bfec4b61f9f927b4430"],["/posts/2087796737.html","ab96e6dd11875751ea93f9b48449fd58"],["/posts/2207806286.html","b7027b6bed58828ab85a6afb512cc244"],["/posts/2225903441.html","f58508e9538e4f4415862fd01137f106"],["/posts/2281352001.html","3546b95bef57da9c01382d4ae75e6014"],["/posts/2364755265.html","e88a926101dc9c329502d99f5c661010"],["/posts/2414116852.html","0b0f21565effee88af26ff179d20abe2"],["/posts/2495386210.html","1c0f569a3c9c35f2efe16d8cc30ccda6"],["/posts/2516528882.html","66b753418e7750d1e983f629bc432598"],["/posts/2529807823.html","bc7de477895e1b0a7eb8d5105b6b0fe0"],["/posts/2888309600.html","3e92f0c46770670a46c6d60f52707c00"],["/posts/2891591958.html","477bd62e315ee5c4cc74d70ef58deee0"],["/posts/2909934084.html","092dd9192cab31fcd8d750cefd998872"],["/posts/3005926051.html","ba26cae846afcd98a4bf877c43d7a53e"],["/posts/3169224211.html","18b7e3b7762d157116a7df61882df92e"],["/posts/3259212833.html","480e75d17f5b7b6966797ad0a39822bf"],["/posts/3266130344.html","94074f88c47fb7bbbbc28acafc5e8aa6"],["/posts/3306641566.html","92bd0fde94635394082753d837ef19c2"],["/posts/3312011324.html","41390f075ed45135eb1e67cbec8cec99"],["/posts/336911618.html","246c5a03608d964f810d2e964c69c493"],["/posts/3402121571.html","3630789cd2c5492ecc6901df0d25eb58"],["/posts/3405577485.html","a91a4cde90c03d5ebb1e7192dfbdbf12"],["/posts/3513711414.html","00ff34ff07fe10308eeb99a407b37814"],["/posts/3546711884.html","185ed9e8b83d78bb165c29b3af01f23a"],["/posts/3731385230.html","a609c57a1cc92a1e683eefab3216e8ab"],["/posts/3772089482.html","91788d11212139fedbd381267845d5b7"],["/posts/4115971639.html","49cf3fc85088778ed87aa9119cf0ddd4"],["/posts/4130790367.html","39004bb48da424a6ebefbf3898d7dffa"],["/posts/4131986683.html","38e169b2f3ce899aa7fd0dcd75420e26"],["/posts/4177218757.html","be5b5a5ec3fef4e6d823990c365aabdf"],["/posts/4192183953.html","d20ef1a28d21c69d79f3a1c576a8563d"],["/posts/4261103898.html","4c83614e5da72c00a2a472070c7d9adc"],["/posts/482495853.html","329dcc8ad39b70c875a1e4ea4e91a5e4"],["/posts/488247922.html","da1987e169b222d97666af8ca62ea7d6"],["/posts/570165348.html","48d87588f72e13f12db67c03f6a643d6"],["/posts/595890772.html","b5e8ef5e9ba8e7b747143d732e0c95fb"],["/posts/694347442.html","4b96d1b3701255bc993e78c1f07fe64d"],["/posts/707384687.html","3c4af36c85b1aa02ed4810deb6bb957f"],["/posts/71180092.html","99c5082d9763a59baed4a00d855b1489"],["/posts/716459272.html","e71abcca5c237fc0cc4c0088796596b0"],["/posts/795397410.html","c245b2cfdfabf0055748ce4e414322ae"],["/posts/820223701.html","768b48dfa7a5821c76140fa96ff8b8c3"],["/posts/830372185.html","56fef8cd806fc5f66590c222d815c40d"],["/posts/88294277.html","3b5a35ec57a7915d1731892c69b32857"],["/posts/939963535.html","8b0fe59063053a12415be9956d538a34"],["/posts/983786067.html","86b389ce1153ea6600b14ecb0d43f0f5"],["/sw-register.js","b8dcc684cbee6749f8d73ca5e5256a28"],["/tags/C/index.html","c9dacdf0690e94bc0f84c1a06527eecd"],["/tags/C/page/2/index.html","aa35eb37084b0b3ffa3eaf7832fc9b07"],["/tags/GUI/index.html","f0305c8cee27db627b4793870ed4efe7"],["/tags/HBase/index.html","b1d9fd5dd260d7457b2f1334285b4ac5"],["/tags/Hadoop/index.html","26281ac4a858eee121c306a5ee60cd3e"],["/tags/Java/index.html","681034a70fbcb8f9a654a9167f8bbbee"],["/tags/Java后端/index.html","cdba57795cb3e84b6c9a1eb908a77b70"],["/tags/Java基础/index.html","29758137fa66cc3de10496e0df29ff84"],["/tags/Java基础/page/2/index.html","73525573c61ea8f9121d85022f008de2"],["/tags/Linux/index.html","413a7da44e9934b8ad4c6babb9acd221"],["/tags/Linux/page/2/index.html","037822105df803cdb63efed2651d7480"],["/tags/Mac/index.html","5544c75b4cfe9f7281fbeabf8b345b81"],["/tags/Mac/page/2/index.html","8505198857fc22f526961c340c7e2084"],["/tags/Maven/index.html","5ea950a85982253cdcb4f8957281e536"],["/tags/MySQL/index.html","88ad3551f33ba5fdc95dc327d6afd4e6"],["/tags/Python/index.html","5b6d5c7dee6bcaa89a45b2d3de4764a3"],["/tags/R语言/index.html","08036d8aaf7535b8c83745105c5f80f4"],["/tags/Ubuntu/index.html","8df427fe336d05b7100a42b813601843"],["/tags/Windows/index.html","f836050484226f553714e075ed60e627"],["/tags/Zookeeper/index.html","9d247e50edf88197f32a15832ee8e340"],["/tags/bfs/index.html","653a7fc926b7504ca2a617c7a2a865e8"],["/tags/dfs/index.html","831327e354273a875c69c373e5609049"],["/tags/folium/index.html","dbd4c19a962a8018248175a2f81cbe5b"],["/tags/git/index.html","4a33d877de5901e0306c8c6791cf3038"],["/tags/index.html","421b8d6fd932758eccaeacf28f7d4802"],["/tags/latex/index.html","85517fc2229d53d43546cb4b33a3d779"],["/tags/二分查找/index.html","b01ca2dcc5f6d57cebaf9752c1455e59"],["/tags/优化类/index.html","2163a339dae5363651f51d558b2e0f2e"],["/tags/前缀和与差分/index.html","d442be5b6f276bf9cb0ddce78c555e44"],["/tags/动态规划/index.html","bcde6721e1c7fad01a90ef43242b33d2"],["/tags/博客搭建/index.html","6d39fb8ee342e75b98adc3a8a2fbea16"],["/tags/图论/index.html","bd68656d1b55ed283fa5dfc713059e24"],["/tags/大数据/index.html","b75418d5f2981c2ffbdc182459f4130b"],["/tags/操作系统/index.html","a67ec6aeced8ad368471f60861e0ad97"],["/tags/数学建模/index.html","e6916efff7f52794a4b66dd80585096f"],["/tags/数据库/index.html","5600092c2e235edb56b87e99dc18d5ed"],["/tags/数据结构和算法/index.html","edea2b7d25447370c0c5a1a23413f769"],["/tags/数据结构和算法/page/2/index.html","7d8feec9c3be5a49a166c2d3e47382f1"],["/tags/数组和字符串/index.html","c75f1defe72b943dfa41357011213725"],["/tags/枚举类/index.html","9742dd91066a8b9492e83b73e18ffe21"],["/tags/栈和队列/index.html","dfdf76503c4e40a41cb6d55a465f89b3"],["/tags/树论/index.html","c533db746742bc15e76a8378a9ee2b4d"],["/tags/测试/index.html","3bfbe18d8f582223e20324ed1a1827e9"],["/tags/环境/index.html","a2034423ee69ddde8a91bd7da5c08112"],["/tags/环境变量/index.html","f70a666ac5117bfd7e683f114bc32dd2"],["/tags/绘图/index.html","974e338d2baabef3c75e7900a32d8b94"],["/tags/编程环境/index.html","de9b5a80dbe2258d99d94cadc36c2b7a"],["/tags/网络编程/index.html","99f6f0f08568c942a8bfa929d1e091d8"],["/tags/英语语法/index.html","9aa0f17dc809c0ed281d4115c6ee8ee3"],["/tags/论文/index.html","20c2b2d8ad5a0d693b0982a9f001a5e9"],["/tags/资源下载/index.html","5b440d2290574f50d0ba01ef904a18d8"],["/tags/链表/index.html","c255f8d5fc7129ecc20b628b3324b2dd"],["/tags/集合/index.html","14eb3680942cea8e1c2d9f9703ad0fc9"],["/tags/集群/index.html","c3423e0e79e990f4ca15e4dea394c3f3"]];
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
