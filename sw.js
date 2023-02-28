/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e5763f202af930d323f8505bbd8cb201"],["/about/index.html","d6abe274703d7e49f96e68ebc51e2c49"],["/archives/2023/01/index.html","20c15d0c61cef6d45a3d5a8cd332cfd3"],["/archives/2023/02/index.html","f9a03a2cf896ecebe15c3281b999f156"],["/archives/2023/02/page/2/index.html","9e11abfcf708ec27e19f991673a09931"],["/archives/2023/index.html","7236b4ed1dbe2dc5bd13810a54f6bd7a"],["/archives/2023/page/2/index.html","534678ebb88e7feceaabf57b0c8055af"],["/archives/2023/page/3/index.html","b787959f917f273ba480e822783015f0"],["/archives/index.html","37d735b5393fb4142d0a3112f9909cb2"],["/archives/page/2/index.html","5abd99a17f50eb39f00e73b30e1bcd0f"],["/archives/page/3/index.html","152e38068c96ef2201ceeea149f7e65d"],["/categories/Java/index.html","ddd9f4d38eef4ed5d4cac8149422e60a"],["/categories/Java/后端/index.html","a09319ef2702f79a86176e3c0ab38e55"],["/categories/Java/基础/index.html","858bc310d7551f4511a84678d68e6d07"],["/categories/Java/基础/集合/index.html","4dc41a1ad52446331e1728ad918db598"],["/categories/Python/index.html","121da8747b725bff051c581f90edf0dc"],["/categories/Python/编程环境/index.html","e877fbebdccecde6accb4995468bc23c"],["/categories/R语言/index.html","81db3256229472ebe30fdbb3b79d42dc"],["/categories/R语言/编程环境/index.html","695b22bcf04710914a5985d188880b91"],["/categories/index.html","a80d90db41bf811b38818c17636415f8"],["/categories/大数据开发/HBase/index.html","bee9f111f5891a503273714e620fbcf3"],["/categories/大数据开发/HBase/环境搭建/index.html","a52c18e2a3dfb52c8e32a9cbf020df03"],["/categories/大数据开发/Hadoop/index.html","ca999b435d00317729b149f5fcbc8795"],["/categories/大数据开发/Hadoop/环境搭建/index.html","46bffc5ff473da070e462b7a2b298f99"],["/categories/大数据开发/Zookeeper/index.html","89bc9a6d1397670d662693ac065f356d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","c1e06e99e2c20ee694dc1eadd7558582"],["/categories/大数据开发/index.html","4229717494ac0f520c14a13c8bddef42"],["/categories/操作系统/Linux/index.html","648a281d8a6e5309d9d7fba81fbbc2e4"],["/categories/操作系统/Mac/index.html","a97ad20047fa96053cf53bb1767ed7ba"],["/categories/操作系统/index.html","cbe13fa06aae1c9895ad6908c46d4bcd"],["/categories/数学建模/index.html","35948bc492e7bdf63427a9cb4683c6eb"],["/categories/数学建模/latex/index.html","b7495c423c33255ec57b94406fb8e5c2"],["/categories/数学建模/优化类/index.html","5302e0a27d79968fc338ee793044879c"],["/categories/数学建模/优化类/现代优化算法/index.html","0c718da28ff07ebd4fdec53aa240a365"],["/categories/数学建模/优化类/规划类/index.html","721af96437b531d2b197c1bc4b04bf35"],["/categories/数学建模/绘图/index.html","c663a0bd9b166cd0cb7a9c7fdfeb2dc3"],["/categories/数据库/MySQL/index.html","2d075424239e5d0555053d1cc460948e"],["/categories/数据库/index.html","6c37766e1456acf929fa6df84acb0668"],["/categories/数据结构和算法/index.html","60531718bc96797361f322742350e95b"],["/categories/数据结构和算法/基本原理/bfs/index.html","9cba40e95249332971ecce7dbc0b144d"],["/categories/数据结构和算法/基本原理/dfs/index.html","bc4b765602d12ce06a990ef95dc9e478"],["/categories/数据结构和算法/基本原理/index.html","c56de173d2a6823f570532a4463662a8"],["/categories/数据结构和算法/基本原理/图论/index.html","95c52b71720965d4a1fcb1906b0eea39"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6e04a95c432e7f7230cb2bb639eeaeac"],["/categories/数据结构和算法/基本原理/数论/index.html","94fe5fcfa77f78ec2124cb4d527b8d93"],["/categories/数据结构和算法/基本原理/树论/index.html","eced59c566bc507e4826938baf6e6080"],["/categories/数据结构和算法/基本原理/链表/index.html","7d33a469a6081bfe25ec73a108f1cb4c"],["/categories/数据结构和算法/算法题/index.html","cc01da82650fba956b981b3ec99d164e"],["/categories/数据结构和算法/算法题/二分查找/index.html","41777a1419d77a58f788e13d92510109"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6f9bdf327599b685c827e1a281ae399a"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2611e1bfe893865978075c39a677d649"],["/categories/数据结构和算法/算法题/栈和队列/index.html","beed1d78acfb528b85899ee4985b2444"],["/categories/数据结构和算法/算法题/树论/index.html","37a4764454be60116e9bd98b209898b7"],["/categories/杂七杂八/index.html","53ebf3465195b49bd6515a52add11c35"],["/categories/杂七杂八/博客搭建/index.html","4136936a80c902d95dc5cb4eb30fb3cb"],["/categories/编程环境/index.html","e98c824ca8fb70017f289430dbf7d349"],["/categories/英语学习/index.html","b886a675d19262e258f2c58ab06c07fc"],["/categories/英语学习/英语语法/index.html","ef07990331c81b1cac8b7526d3a76a69"],["/comments/index.html","74cd8601e54968a59a8a6bdd1ef9b19c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","abe8800264b8d03bb0caf8f527b7b599"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","610a72bffd4c1c57d136189e45c94680"],["/movies/index.html","09c046b12a067ee3222fb4fc65dedcc8"],["/music/index.html","3c60f8be6ff97b140e2584a45e1d19de"],["/page/2/index.html","e8c02ed1c4ff6c93b7816c7c4f13c1e4"],["/page/3/index.html","9c326e73ffbeec0e6ea92afce593d75c"],["/page/4/index.html","8d8831ee16d3970c33c9c11cd69693aa"],["/posts/1021360842.html","ce53c95d79f2e78c95db7d9558c5dc48"],["/posts/1120620192.html","9f583df4f7a529a5faec3550aff6ffaa"],["/posts/1141628095.html","483fa458851999b38da42450d193f1b2"],["/posts/1168613674.html","476fe1f5c34e9a5e81e9a8397d0a3986"],["/posts/1219920510.html","30ba8e457463a3907fc975dc41b886ce"],["/posts/1222166338.html","6da290c7c7d6a01e3984d0403d172527"],["/posts/1259097482.html","a77e06c71052fd350905d98c284122a4"],["/posts/1271036369.html","a097035810d5164a6e2de8a2c94cc7da"],["/posts/135355774.html","8abb908df2098cfb08953aaaff3c1f67"],["/posts/1375344716.html","534c56a7843fae50fdbfe051410b96f7"],["/posts/1388991698.html","286e2430d71174dcf1421fbf97ba37d3"],["/posts/1410315814.html","56a950405ba55802234a808f30ef620b"],["/posts/1470079884.html","ba73376958b05e835f08a48432a0c952"],["/posts/1470079885.html","4cf7ae78f88de97945e6cff55bfb81e2"],["/posts/1470079886.html","f6ad13310653fb39736de1d4771a99aa"],["/posts/1470079887.html","88a2700edd23b2590881adfdc30c3733"],["/posts/1498536549.html","c1b8b8a4a14d485bd66c380eb8ed76bc"],["/posts/1571776361.html","894740f12d0e929313ec6114d2fcf617"],["/posts/1605124548.html","8a7c71d2e53c69d751b2e228873ad3a9"],["/posts/1817748743.html","341498d4d920d9c6b2840fdc8aeaf73e"],["/posts/1925125395.html","ff08548726b72f383196851678bb969a"],["/posts/1966191251.html","136c4701dd70064443f33f49f3729592"],["/posts/1987617322.html","cf7f6b60c9a84816346015dbc3547d8e"],["/posts/2075104059.html","4adbbe6ab61706a808a14562c3e49de4"],["/posts/2087796737.html","d61f9c3287da8bab68be957c6049dee8"],["/posts/2207806286.html","12611bd24a896be49723ce8dd934dacd"],["/posts/2225903441.html","ce2effc53375c56be284030c2a39ab23"],["/posts/2281352001.html","53675dff2acdd9e76fcfb75211ffeaae"],["/posts/2364755265.html","5483e5d41d6ba149fdec144c878c5a92"],["/posts/2414116852.html","62dc31ea41a9b7a11bd5ba97529dfdc3"],["/posts/2495386210.html","94f325ff438693af24fbf3303009a8a5"],["/posts/2516528882.html","1d08654984409ad7bbc53c5654fb86ad"],["/posts/2529807823.html","57c24ebae17b0c790a4a74f94960b9e1"],["/posts/2891591958.html","f7e343ec6aea27dae0bc5299973a0e74"],["/posts/2909934084.html","d974a506edabb447931dafb0442abe00"],["/posts/3005926051.html","d514bc82ade764341ef1c6d12995c7ee"],["/posts/3169224211.html","e73f1da03e0dc8a9b89efaae303970c4"],["/posts/3259212833.html","8c696218cad97ea96824a3f7b637e205"],["/posts/3266130344.html","b491fd561f3ed501f45d74abebb6520c"],["/posts/3306641566.html","86f8d6cb99b1b33b3b59c237d85902d7"],["/posts/3312011324.html","f569169543f0e3a8b958911aed67f1f6"],["/posts/336911618.html","5664a33eecf1634157700376155d34eb"],["/posts/3402121571.html","33c6b4bcf8369b01d094fe46c2d09109"],["/posts/3513711414.html","78518d26bea8af41e6069e919dc9c163"],["/posts/3546711884.html","186b3386ad5f31c6ddb8109a4bde2b8f"],["/posts/3731385230.html","f8f8cfc981e4c4dee4090e908beb7d3c"],["/posts/3772089482.html","e65a16be9641001c5bca8a84439456e4"],["/posts/4115971639.html","7aef49cbf4a47bc298152eccda959431"],["/posts/4130790367.html","09d46b7b6ae7ee180ba53899c580b03a"],["/posts/4131986683.html","a8c31ba7fcef02f0b027111c2fda83cb"],["/posts/4177218757.html","d887f088e7d08163d249d65b7a0670ae"],["/posts/4192183953.html","6408c0870cfea2f919d15e94026f2de0"],["/posts/4261103898.html","0fc188adfd12ac8e82c94da124eaf93a"],["/posts/482495853.html","2c7ee67ade56deb7b0d3fcf1080d0683"],["/posts/488247922.html","bd57223a7745302b9ba2cc6a083cc98d"],["/posts/570165348.html","664fc80f2798a5f0fe7107f03e4519b4"],["/posts/595890772.html","fcef914205958f5b157d885c35b5c27f"],["/posts/694347442.html","80273eaf01d8379dd8ae4b6ba3b00602"],["/posts/707384687.html","8310957aad0f6fd96c7c526a0126f6f2"],["/posts/71180092.html","b3b5208af62eddd847e5b1bb36f5b653"],["/posts/716459272.html","2124cf0495459d328cd1f165663e4a5a"],["/posts/795397410.html","fd30285b26ead07df0a22f4293e28e12"],["/posts/820223701.html","ecf757ebc596d0c4076efc223e123f94"],["/posts/830372185.html","edf234dd65e12f85875167e2d601868a"],["/posts/88294277.html","9ae93a48bc00b825ef13ec721d3a0051"],["/posts/939963535.html","b7f2e9a0ef018f92eefa75dd95784da4"],["/posts/983786067.html","d9c7e271e51854cdd4c776a40cbc6c9f"],["/sw-register.js","9b4951fa994e1cc2377670dadb9823d4"],["/tags/C/index.html","3fcc9da7303de540ba60e278dcefa0f7"],["/tags/C/page/2/index.html","0b1db60c61804de54bf98fe0572920ce"],["/tags/GUI/index.html","6bed931a1dfc78b44d223864a2d22206"],["/tags/HBase/index.html","6a5d691f8853a29088f0c1035845760b"],["/tags/Hadoop/index.html","dead60b63cd318523533a9a1697536fe"],["/tags/Java/index.html","e656d89aadd7973f49f215bf974756e7"],["/tags/Java后端/index.html","72da50829c7780a11ef92b12bd3b1e1d"],["/tags/Java基础/index.html","b532de08d53dda7166f97ab18a630205"],["/tags/Java基础/page/2/index.html","d0d2df9b76fb83a7a2b1b47e895e3fdd"],["/tags/Linux/index.html","40dc90ff6207551742d35d34e0d56a62"],["/tags/Linux/page/2/index.html","cba1fe4c2080f9966c8d190bd2e3c96b"],["/tags/Mac/index.html","0a18259967d723efe7d97ecb1f6372d9"],["/tags/Maven/index.html","4a3c5b39618db0ff662d51435f42d148"],["/tags/MySQL/index.html","6f6032da3b364ae5bd700f6de25edc8b"],["/tags/Python/index.html","4475b91a72767048d376f8f79fd48490"],["/tags/R语言/index.html","bd3cd8ed9a61ae6c18b600da52e1bd11"],["/tags/Ubuntu/index.html","0d3fe03a4da7ce00138368ef7cf5362b"],["/tags/ZooKeeper/index.html","d5221f919cf5e0b2821d75b28e0df122"],["/tags/bfs/index.html","9d1dfe7c80678684e2d5be115adc0510"],["/tags/dfs/index.html","170f886ad1f49333130922a44b686924"],["/tags/folium/index.html","244ff512c2e58b0571a21c4f0d7c9832"],["/tags/git/index.html","57e8a07981bea421b077c5cbec7414d4"],["/tags/index.html","caa449273b3df8c839b3ff07451001be"],["/tags/latex/index.html","5f9440bc623b1b6761f255c519c2ca19"],["/tags/二分查找/index.html","956ab3accca47290d0bac116d26073f5"],["/tags/优化类/index.html","771a9fa932598a47340ed182418d5fde"],["/tags/前缀和与差分/index.html","fa05f76bdef94e0dcc40423ac453a9ec"],["/tags/博客搭建/index.html","f02b733bc0322c73375074cbb53d485c"],["/tags/图论/index.html","3c8855be0f3de44600360c1f7ccdd33f"],["/tags/大数据/index.html","ab1a56435eede5d77374eeef6bf1d2c7"],["/tags/操作系统/index.html","abb11883e9afc12a813671fda640a8c8"],["/tags/数学建模/index.html","960abe3f4131f80d45855509624afa96"],["/tags/数据库/index.html","26db8230aa46d9a606391da5c81791e8"],["/tags/数据结构和算法/index.html","2f245fafc027af73a71131e64241f9ef"],["/tags/数据结构和算法/page/2/index.html","5859afc85ce9c6b6df8f3b17fedd0ff2"],["/tags/数组和字符串/index.html","8e61d5d428de091cb589da0ba656d093"],["/tags/枚举类/index.html","1ad07350bd012d12e19e5f38689ec381"],["/tags/栈和队列/index.html","5174965120cb2cf69d5aeb73bb804d03"],["/tags/树论/index.html","2d0464e5c7000a72ac061427bc7f0fb3"],["/tags/测试/index.html","013010574e1d0216f967d7c83413ba95"],["/tags/环境/index.html","063744b6d712668778d6a3c577fadd4a"],["/tags/环境变量/index.html","7a5fc9151a1610e7bb9179505d9fad1f"],["/tags/绘图/index.html","83b6a834976410976d1a912e6d37006e"],["/tags/编程环境/index.html","139ca6a2464fa431e9f6510726fdd3f8"],["/tags/网络编程/index.html","52565f36acb19b901f53995f883421fe"],["/tags/英语语法/index.html","0268ba610306b342943646297beb440b"],["/tags/论文/index.html","eb506fd5b8dd0b00fecfde7d62dd0b9d"],["/tags/资源下载/index.html","5e31a702003ef0e32de4edb8f8a3ad4a"],["/tags/链表/index.html","b1b2852600c08c61de4d44fdd08e8200"],["/tags/集合/index.html","3a76726b9de45c635f32c215a7456d10"],["/tags/集群/index.html","9e0e608f6a49937cb1fb773c1065bf0a"]];
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
