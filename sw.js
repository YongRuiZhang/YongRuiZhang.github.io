/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","fa839ee169b75817cde2c10a355f6d51"],["/about/index.html","13419ea36561a4cbe67d296d07ff6ac8"],["/archives/2023/01/index.html","b30967feca5bed1f4a4f6c3c536369fe"],["/archives/2023/02/index.html","516bf3794151cbd6cb4c532ff2384480"],["/archives/2023/02/page/2/index.html","9f458d34aefe8df364e6dc43bb7f14db"],["/archives/2023/03/index.html","bf8b8ade41c9bd7600a2592433a4a0c6"],["/archives/2023/index.html","43fb61459cf2bc739c6c890cff9e7c46"],["/archives/2023/page/2/index.html","12a55a1a4013e177533333ea48282016"],["/archives/2023/page/3/index.html","74034f682e63f75729b14f6bf744eceb"],["/archives/index.html","216189fee44420079338f0f0f784e5d9"],["/archives/page/2/index.html","a9d751ecc628ae9822a7d628ecd72860"],["/archives/page/3/index.html","07abfd587510a5c9f78c00769c57dfa7"],["/categories/Java/index.html","262c2b8ed9bfca14af373c9397b9dfb0"],["/categories/Java/后端/index.html","ffed8599dc5b1dbc3ef764a4ccbd9cfb"],["/categories/Java/基础/index.html","6cad83e84bfe8f30638d71df51440cbc"],["/categories/Java/基础/集合/index.html","d2bdc3f650ea4526c3a1e4f237a6b17a"],["/categories/Python/index.html","2585bc51d5a493a127a1df3feff67eec"],["/categories/Python/编程环境/index.html","eb7ae8c04d2226d40b1974adf402d54c"],["/categories/R语言/index.html","56a0bc1979f12ffab19dfc3c26201272"],["/categories/R语言/编程环境/index.html","b6399acb22b2c1f557ac95a30f68dd49"],["/categories/index.html","ed8426d54f9cd6bcf81f2e513f970c32"],["/categories/大数据开发/HBase/index.html","d15e4929f707a6375f462f361a5f8840"],["/categories/大数据开发/HBase/环境搭建/index.html","910fa297c17ea6f54e2c37890a2ed597"],["/categories/大数据开发/Hadoop/index.html","63c79c210ece253cb688287cdb67b9c1"],["/categories/大数据开发/Hadoop/环境搭建/index.html","714363afb7b3427bc87b709cb6455037"],["/categories/大数据开发/Zookeeper/index.html","e1e458f4f3eda8442a69634caa3aeceb"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","17261db3663f1df21f17629553fbeaf0"],["/categories/大数据开发/index.html","cd1f70928bfde990ad9cb701f353f44e"],["/categories/操作系统/Linux/index.html","07ef88a1f74ab82c1774c79634e51833"],["/categories/操作系统/Mac/index.html","b36e2d4bc80a703025de9711d987a508"],["/categories/操作系统/Windows/index.html","33a2dc8bc9e737a5215fee0ad290fb0b"],["/categories/操作系统/index.html","63cc7b43c0ddfd86229a7350fe66ae65"],["/categories/数学建模/index.html","e371e65b60a223268aa84d6a3d49eb4c"],["/categories/数学建模/latex/index.html","c81431871b992c59a76b9b11d259bfaa"],["/categories/数学建模/优化类/index.html","73eb6bcc17a09fca198582e5513cca7b"],["/categories/数学建模/优化类/现代优化算法/index.html","2ab510e18a7101821d63b27c78596dee"],["/categories/数学建模/优化类/规划类/index.html","a612f8b130286430cfa7a7d661f0e501"],["/categories/数学建模/绘图/index.html","1cf30903f6bb1e3443b93bfc4721bc0d"],["/categories/数据库/MySQL/index.html","1ec8a4358ccccd23aeb35225e940b01e"],["/categories/数据库/index.html","b04b5ce77650b1c8501acc487cf1212e"],["/categories/数据结构和算法/index.html","2022658501c495ce41e590e4c78ddca6"],["/categories/数据结构和算法/基本原理/bfs/index.html","ae26477f58eca89beb25fab43f3711a6"],["/categories/数据结构和算法/基本原理/dfs/index.html","5b6ba4e21af125d3ad83f93e72e17d00"],["/categories/数据结构和算法/基本原理/index.html","9090ceb671f3f71079bcd2cda47f3cd6"],["/categories/数据结构和算法/基本原理/图论/index.html","e2366f9cafdc6e6576d159cdec7b5443"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","990209ca52f5d46c43a2f9ad2549ed2e"],["/categories/数据结构和算法/基本原理/数论/index.html","3d1a2a18ca6768f2f3c6917f815e06e2"],["/categories/数据结构和算法/基本原理/树论/index.html","7455da8f41482fcec1802fde3095bbe5"],["/categories/数据结构和算法/基本原理/链表/index.html","b57bcfd30b973c0426c2279bcda5ed80"],["/categories/数据结构和算法/算法题/index.html","3fa099fa922e5371d9d535ebae5093fb"],["/categories/数据结构和算法/算法题/二分查找/index.html","aed3117def8fb9f79bdfdc8992bd84a3"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","d2733b1d31850a0faac98a24ec23fe0e"],["/categories/数据结构和算法/算法题/动态规划/index.html","414e9fa15d900aec22c0db8014a8137b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","6c81839e2cb40d26925675372585eb16"],["/categories/数据结构和算法/算法题/栈和队列/index.html","64cd7e83fc5a78ae9a5009dca2b403cf"],["/categories/数据结构和算法/算法题/树论/index.html","bc0b6547abe73c7afe3bf775e0bbfbf7"],["/categories/杂七杂八/index.html","d494de7197d39e73f1512ad9ab0b120e"],["/categories/杂七杂八/博客搭建/index.html","095e0ec8cd4a486d1839b724c8d63200"],["/categories/编程环境/index.html","a79249e3b9c1166d47962b6bf0631f82"],["/categories/英语学习/index.html","d6fbb979cd94e01502066282ed7f445e"],["/categories/英语学习/英语语法/index.html","e417c53ac0829d619bb562b4117fd4f6"],["/comments/index.html","5ede3802e0d7e4c54639ed8c142fe41a"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","4e7830eb52eb2d6fac7194696cb9a1db"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","149a7a68eccac3a6311c43cec915fddb"],["/movies/index.html","1517204bcd5361249ebeb90b44ef510d"],["/music/index.html","1b54bff029acbccca1f9a44683ffeeb5"],["/page/2/index.html","47eb981798eb4caf968e4d10c8f6083f"],["/page/3/index.html","c4bf2eb915ba73b483992fa332432bbd"],["/page/4/index.html","19738f6981f6644e9b01bdc61525e466"],["/posts/1021360842.html","73862e91040ef65d5451a3f38f8fd348"],["/posts/1120620192.html","1d4305887d4096489851705b9b3442b0"],["/posts/1141628095.html","76bdc03aa26121c52127191d35a733c0"],["/posts/1168613674.html","94b9df39baad8f0b0e4796fbcf3d6fcd"],["/posts/1219920510.html","07087bbe18731880f3089831dd88de6b"],["/posts/1222166338.html","decac47d6772cf6dc821df5591d2dfc7"],["/posts/1259097482.html","53177e11ad8cd11d66f1840f23d01083"],["/posts/1271036369.html","0917d17c0a1a9463866bdc5e1384c104"],["/posts/135355774.html","f7a5c733b6e1b1810ab5217ff66d614c"],["/posts/1375344716.html","39b193ff2ce8167f53969bde920392ed"],["/posts/1388991698.html","ffb2e11f8801e6ba91a791232b344256"],["/posts/1410315814.html","d1eb5d1ab975ad0e3604f6e1791fd4a0"],["/posts/1452790229.html","47da4805d55822b38aad83e207353879"],["/posts/1470079884.html","f1f9e6ec873e7f0ae5db5b74f60eed2b"],["/posts/1470079885.html","f75e6f5ead520a43b6a325855abc217c"],["/posts/1470079886.html","136598a9a138912a89ede8c35778977a"],["/posts/1470079887.html","3777462fb1f5d038824ffa50808dd68e"],["/posts/1498536549.html","0ef765c037551af1e8beba8fac199a90"],["/posts/1557866301.html","7ce2f328d79a55bce778a0606dcee4eb"],["/posts/1571776361.html","00bb82637292920b18590e963561e5b5"],["/posts/1605124548.html","0e1e3c546ef80ff704c3e028529aa958"],["/posts/1765123828.html","cd72fd0ccff4bfd6630e43d8aaac2292"],["/posts/1817748743.html","957eab5da3d9bacbbfa0d19536655261"],["/posts/1925125395.html","48281470777f398dc76c6e0afdc86b14"],["/posts/1966191251.html","9de6d87518bd4c3532f01704a87d8a52"],["/posts/1987617322.html","6c3b0264776216c9b3e2f041397df000"],["/posts/2075104059.html","cdea0a96bc3d547d72274a9c4da75876"],["/posts/2087796737.html","fae4af767fc948ebd642ae31e362fbef"],["/posts/2207806286.html","3b30ab84515e920d3857da6eba638e14"],["/posts/2225903441.html","4eae019913c2025ee2c95f6b7e3f0988"],["/posts/2281352001.html","b1c71b2288ba4af6571a0e4965bb67ff"],["/posts/2364755265.html","5ed0fda247178b7da360baa4a3f1c4fd"],["/posts/2414116852.html","3b0428da79203386f973d6c3cc924b4c"],["/posts/2495386210.html","c92e216107fc87795f337c6ed5286195"],["/posts/2516528882.html","a2ac3e04211da365ba1c12763392a028"],["/posts/2529807823.html","b72e9a817c5820a2d99de6bd2529b185"],["/posts/2888309600.html","c8c4e47b21aa2613228faa7b36b74508"],["/posts/2891591958.html","fba1a6ca9d476cceee4c50167501a921"],["/posts/2909934084.html","8d3f27c833246d7478e5adaef68b75e5"],["/posts/3005926051.html","d5ea92a18e11f02cc0bf86f8753a2b91"],["/posts/3169224211.html","b777d9afe087c68267d56255f7b0b81b"],["/posts/3259212833.html","65b6d53b026b12baad332c8907da91f3"],["/posts/3266130344.html","1030a813b5270594efa1ddfed24408bf"],["/posts/3306641566.html","275ea2c95faa94c82bd8bcd23de7f6d3"],["/posts/3312011324.html","57502f902d1d43b79bba45d760589151"],["/posts/336911618.html","ef208abe7b8cba19e8bd3e0b7309bf03"],["/posts/3402121571.html","59f1cee5f06b9bc395955caf602e383e"],["/posts/3513711414.html","5767aed676f998bfd24da5f8249e8cfe"],["/posts/3546711884.html","950069233e90a22e83bae9b44ed063e0"],["/posts/3731385230.html","c29531d3eea8080fb6dbc012e87d0be4"],["/posts/3772089482.html","4ff16b8d325fd8f996fd6685a54aa35b"],["/posts/4115971639.html","557c395cb607f50608b62173deb37bfb"],["/posts/4130790367.html","a0342b9f227942d696c781229fe5b3fa"],["/posts/4131986683.html","4ddd6878129758952010b37a96b3bdf2"],["/posts/4177218757.html","4cf555c92ba166acc892d23d75326561"],["/posts/4192183953.html","7c7e4dddf74ebe7af253398a04432b37"],["/posts/4261103898.html","a0051030655bf73b5e0f0e0e44915d18"],["/posts/482495853.html","c9679a90dcc27a77d8533a384a074245"],["/posts/488247922.html","5354544f236492e9d66bf57ad93ea5da"],["/posts/570165348.html","1cbc881cc37450ff46f6c82ad11f9260"],["/posts/595890772.html","ea976a1b1370963204ca0d08c323cca2"],["/posts/694347442.html","4a26657e6ec43c365375394c873ac002"],["/posts/707384687.html","3836dc174f0e6d8bf57c88618ad782e7"],["/posts/71180092.html","4304a15f83a8f8eed5e157ff3211f0aa"],["/posts/716459272.html","594652b4fef3b2bb950a0af04accb0e2"],["/posts/795397410.html","deb012ba40781417126610c6c44fea07"],["/posts/820223701.html","b2451ab735d5731db0eee6f6b1337170"],["/posts/830372185.html","dcb23279b470f2eb806eecd7e88c1098"],["/posts/88294277.html","09b92eff614d208bcd01ade54f0b7dda"],["/posts/939963535.html","240ec12c716a9d0df93f29ecb4f9ffb4"],["/posts/983786067.html","15bc966696c803e2bb825e46920a4c05"],["/sw-register.js","fc813107e060f09c3f0531c0ac35ab82"],["/tags/C/index.html","56a4cca91b7a60e34714648496567d11"],["/tags/C/page/2/index.html","3a8642a9f81e83fa5513505bf26eff67"],["/tags/GUI/index.html","3569cdf963152ac6d35f94543ed8663b"],["/tags/HBase/index.html","3e5cb893b118ff28acc6336483ebdb54"],["/tags/Hadoop/index.html","48d2741012c7c416552146de97e32ce0"],["/tags/Java/index.html","0b43b41cc4d342e82bcd8a73b6798733"],["/tags/Java后端/index.html","041dda3991a045fe9ffacfc669181541"],["/tags/Java基础/index.html","905e8f1042bfbe68d4ed4c72d16e776f"],["/tags/Java基础/page/2/index.html","712f8ccfe492c57f55f38fa552edeeb4"],["/tags/Linux/index.html","2fbc59124631393392f15dc43016036b"],["/tags/Linux/page/2/index.html","d33ca20adc795bcd0a52f8535aad22ae"],["/tags/Mac/index.html","bd58d4f4c8fec2a002ae73c084453e42"],["/tags/Mac/page/2/index.html","65498b6ad0f691696d78edd6144cf4b8"],["/tags/Maven/index.html","46d6ead93767d11a8b0ae40cfaba859f"],["/tags/MySQL/index.html","3504cabff5df9cab849baf050b2f47c0"],["/tags/Python/index.html","f6b213dc0c90adef37e851c78e76910a"],["/tags/R语言/index.html","3c4c3143488c864abc301f46fda5576e"],["/tags/Ubuntu/index.html","daed65a58a250802d27f8424b4c361ef"],["/tags/Windows/index.html","cc772abad205805538d7996110aa0fdf"],["/tags/ZooKeeper/index.html","f6a075e4e32b16803a24c82d71c12317"],["/tags/bfs/index.html","097a21986248de95990a3dcdca333462"],["/tags/dfs/index.html","5115151e040b16e48c13b07a1701a07f"],["/tags/folium/index.html","f46ec12f07b65277b0791eea1861b2e1"],["/tags/git/index.html","968b6470570a3d89980fd93d3b80e67d"],["/tags/index.html","1d71d79c597cb715944708f307003e1c"],["/tags/latex/index.html","c11969c4d65063141af3a1ad5c4103b0"],["/tags/二分查找/index.html","c901e377ada386688b34e82d2b560d71"],["/tags/优化类/index.html","3051ba6a647d7e141fa8a71ef9fdb1e8"],["/tags/前缀和与差分/index.html","bee527f171c1f5bc5541a3c6e0b1e8f5"],["/tags/动态规划/index.html","86e0988f40e801218cd032f22a6179eb"],["/tags/博客搭建/index.html","36a8077312592343787f44cacefda4cd"],["/tags/图论/index.html","9e3dedddb03fa80ff8c18c629694c39f"],["/tags/大数据/index.html","92acb198492f24b6edfc1e97b6470714"],["/tags/操作系统/index.html","1e3ab38585aa87bd06d2a23b7eac9924"],["/tags/数学建模/index.html","877d19066c1151ccfc9114b198bfe30c"],["/tags/数据库/index.html","ddcb8553ef5d730c80c74be09caf36aa"],["/tags/数据结构和算法/index.html","0513a437933d02cc25dee71bd211133a"],["/tags/数据结构和算法/page/2/index.html","a8d96fa6cde034536ccee4ca0d6f3d7d"],["/tags/数组和字符串/index.html","8587dbdebe92c1bffddb93edc9e4be6d"],["/tags/枚举类/index.html","0f10cee5c309b82d5568172608658760"],["/tags/栈和队列/index.html","ea70722e578cbef2bf4cb49dc7c19b5a"],["/tags/树论/index.html","4e76052dbfaea175786bc94e0dd89526"],["/tags/测试/index.html","15d3e91c1552ba5a8fa34dae78bd8e52"],["/tags/环境/index.html","1a08eac46e17ee46bda128fda67aab67"],["/tags/环境变量/index.html","7af471bc7363c2ccae2709e2594c51d0"],["/tags/绘图/index.html","466db4c7c02891256ae9b88fe8d82022"],["/tags/编程环境/index.html","fb1d5c71a9b4c866ef90734e427ea8ce"],["/tags/网络编程/index.html","0fd01c8a8e4a94a113c628eef568fe71"],["/tags/英语语法/index.html","8f5af3e5b90402a2004f7af8b6706746"],["/tags/论文/index.html","68cb80bfd394148da33c472896d1a9be"],["/tags/资源下载/index.html","cb08cb36fc9d4fa5c2c146b797cb4739"],["/tags/链表/index.html","9abdff8bfd50580c92c88c5383396dda"],["/tags/集合/index.html","d327f3df3738a9ce00f31d65edd4f740"],["/tags/集群/index.html","ba2d6c33783f12f64dc55bee617d3ee2"]];
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
