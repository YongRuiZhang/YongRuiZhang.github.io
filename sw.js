/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","6732358fb34a88e658c6ccbfeae68112"],["/about/index.html","83d7446a5653771d01cfc5933b126986"],["/archives/2023/01/index.html","82fc68cd6f2ea677d18795654918c0b1"],["/archives/2023/02/index.html","950dce565ba8c56c9fe6d0c0a490421f"],["/archives/2023/02/page/2/index.html","97220a3c2e828bd1c0525cf32f34af9d"],["/archives/2023/03/index.html","65965e0690b6af53fbc486eaed4b9ff4"],["/archives/2023/index.html","dd2717ac695c550059afbf71e3934519"],["/archives/2023/page/2/index.html","f2d1f1ad3ae5720415e2db7b894fe540"],["/archives/2023/page/3/index.html","7ad8caaad49a77c0ff771f3cbea34407"],["/archives/index.html","23b6e5211c024e51ac7e7bc04c92a927"],["/archives/page/2/index.html","8a7716a5f81baf1019b5f7b2f4311040"],["/archives/page/3/index.html","51420e1d503d541977135fce0fd41b2d"],["/categories/Java/index.html","f6abbc6656f34713ae9faa97a7146f87"],["/categories/Java/后端/index.html","a1fe435e7b2ee61a4a65493c5b25dd6e"],["/categories/Java/基础/index.html","7f1fbf8d54b6049e7127c8db33547861"],["/categories/Java/基础/集合/index.html","e6f5bdf3693f1bd396b7604ade1af072"],["/categories/Python/index.html","4a94b8a81201b37b3b96f799f417b04f"],["/categories/Python/编程环境/index.html","8f247f7513ed71dd24db17de86cb4dff"],["/categories/R语言/index.html","4672afcc2cb311fc202324b7bf2107ad"],["/categories/R语言/编程环境/index.html","dd5f7734f920bff26493da8237864e83"],["/categories/index.html","ee748028ca902adc57ce699740e05657"],["/categories/大数据开发/HBase/index.html","7e3ac9880d4e38b34c28779ef0271d72"],["/categories/大数据开发/HBase/环境搭建/index.html","25952f4943879042d610f77257aded54"],["/categories/大数据开发/Hadoop/index.html","26e786c0f647f397f31e35aaef41b651"],["/categories/大数据开发/Hadoop/环境搭建/index.html","db3bdf72ef4ba343dfc058f38e53eb8a"],["/categories/大数据开发/Zookeeper/index.html","e0db77706bed1718ec5536bb52e44da4"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","c9f3c946088e8c148642b85c3e49494f"],["/categories/大数据开发/index.html","70436e258134f01750c1f1f3e1bb74f5"],["/categories/操作系统/Linux/index.html","79fb3c081e014a4649decf7dd5de6549"],["/categories/操作系统/Mac/index.html","708b9e72d5f2157f4443ce7bb0071547"],["/categories/操作系统/Windows/index.html","63ead39244d8fbbae2fe0e6e49b01e6a"],["/categories/操作系统/index.html","bb2338b9ccd11e8d33c9bc2c88fb5248"],["/categories/数学建模/index.html","a5f59befde39c515ac1bce450fe4aa24"],["/categories/数学建模/latex/index.html","82af180b70d810d0fe30312ed495511a"],["/categories/数学建模/优化类/index.html","9187dc88626e46d410779441bf1ecda3"],["/categories/数学建模/优化类/现代优化算法/index.html","b6fe410cd85cde8945406829ddeda21a"],["/categories/数学建模/优化类/规划类/index.html","fbd692cac0107b3a305d769b33304c5a"],["/categories/数学建模/绘图/index.html","10015216d8770b900099ec9904d9ce5b"],["/categories/数据库/MySQL/index.html","bd92ee9469f23e3fc54bc22d7e94fa4f"],["/categories/数据库/index.html","fe5cd576c6f1d53ae09415d345c01193"],["/categories/数据结构和算法/index.html","d995738f4d5f5b8386cc9936fa6de4e2"],["/categories/数据结构和算法/基本原理/bfs/index.html","5eac5fa18a51454aac12fe27dfc74969"],["/categories/数据结构和算法/基本原理/dfs/index.html","68a97ab65e5e9e49dc9b239bf838d911"],["/categories/数据结构和算法/基本原理/index.html","1c99047e48f4aee1246e8b5e06ce283d"],["/categories/数据结构和算法/基本原理/图论/index.html","5aebb3de1cca6bf6dfdf74a796319b86"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","89f65b0d12870debaeafced694954f17"],["/categories/数据结构和算法/基本原理/数论/index.html","5cce755360b726fe23eae98d89a07c3f"],["/categories/数据结构和算法/基本原理/树论/index.html","0c6567dc46ff83faf36d5487bb40decc"],["/categories/数据结构和算法/基本原理/链表/index.html","1a51077211776ed385ba2aa243005244"],["/categories/数据结构和算法/算法题/index.html","a9067a273084ccace8e004d380460372"],["/categories/数据结构和算法/算法题/二分查找/index.html","a82b8d236efd474265f09d68e9312f18"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","dd0d5a21e19984704558baa5e9b99702"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b4728e347758a633955310882f1879e1"],["/categories/数据结构和算法/算法题/栈和队列/index.html","dee910209cb6860d351b12330978f8f4"],["/categories/数据结构和算法/算法题/树论/index.html","1d1a32e2e09654c7fe49bf68e7f78228"],["/categories/杂七杂八/index.html","0bc2630ebae22a4426c85dd86156c912"],["/categories/杂七杂八/博客搭建/index.html","26dc8caeace58c2d127f962bdd7b2929"],["/categories/编程环境/index.html","5780f09e259326cd4e63a5d227b18f8b"],["/categories/英语学习/index.html","ff74a50b7915192bd9f1f9d0bc3430b4"],["/categories/英语学习/英语语法/index.html","924a7d20e32ebd0c4f0f2f63c32b9913"],["/comments/index.html","4f522aafe570d72dc502f1cb19036e21"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","d582ecca7d0eae99ff1a7e7d1d0074ff"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","23dba54febddc422a92deaff9dc65251"],["/movies/index.html","4ffdc06278726d3d84f5e7c318119c47"],["/music/index.html","f6db0768e60d4a2cb06ad669bedee8e4"],["/page/2/index.html","c2ca1c41ec1c0749fe75c8571d8886a8"],["/page/3/index.html","7d57d53ec3c64f166075fb863499fd50"],["/page/4/index.html","aa12304db2c544a769a427c9b0f2e816"],["/posts/1021360842.html","8167d04892aa5eaa234dff262c82c830"],["/posts/1120620192.html","fdfdaf252421a1f23a7808e36a2efc5c"],["/posts/1141628095.html","2bee3f19c742561d6c947674f43639ee"],["/posts/1168613674.html","ba7286da006a199b3d274ccd52882f0d"],["/posts/1219920510.html","a4ddbfc01884c8a67d1e052ecc70d502"],["/posts/1222166338.html","81ffe6cea7a8595f0ffbfdb220d44494"],["/posts/1259097482.html","c94d4e86589b0ce748a409b8a4bd0519"],["/posts/1271036369.html","107b5d1465f325e714664658eb532e09"],["/posts/135355774.html","561b365829f09ba7d112d007eb1a0c1b"],["/posts/1375344716.html","bcc5aec24da10ac3e2eb03c9c866c2be"],["/posts/1388991698.html","2ec12abddf899e0b8bbf4f41ce4e70f2"],["/posts/1410315814.html","345f9a4a861f5e067798023d4aaf0e92"],["/posts/1452790229.html","c1376cc9a4c31f5a17ea6e2516178d27"],["/posts/1470079884.html","84046e7dcad1debaed499c9a5e6fc6b7"],["/posts/1470079885.html","1bf64d300234b0c808719ea2e269a1a3"],["/posts/1470079886.html","71fdf26778e7d1893cc10754912d6916"],["/posts/1470079887.html","47d08eba81f1b77638c0f90de0e8f82f"],["/posts/1498536549.html","d93d048470ed4b5a8a47f7402732806a"],["/posts/1571776361.html","f0f07790faca3e7b831e28ebcf4ebf3f"],["/posts/1605124548.html","b8fe664ad253ec87eaccc00dc46b98ab"],["/posts/1817748743.html","e4a605f0f92d60c0f17ae81f6ae857a3"],["/posts/1925125395.html","8d4d41ff54670719a7a8c3b434c544e8"],["/posts/1966191251.html","560cf6ded311fc0013fde0d9ebd9a30e"],["/posts/1987617322.html","a024fe82adf9ff1111ac5bb28f033ef9"],["/posts/2075104059.html","5991b55d5a6a81820df1f35edf53751b"],["/posts/2087796737.html","624c8a78fd7cb31e667000ec38a7a19e"],["/posts/2207806286.html","450c61894d660c5ea94b03c6eeaa2c9c"],["/posts/2225903441.html","240dfd5973055fa2b60f3d5b3b05a3ee"],["/posts/2281352001.html","c7da12a50ef06cfefa4ad9fc84aaa9fc"],["/posts/2364755265.html","f062e5a8b43a6d56082fce6cea55e847"],["/posts/2414116852.html","1bc2b22d1ca1b5c7ce79a4ca4a80e854"],["/posts/2495386210.html","52cdf5f5a9fe92f45c1f877e0b820a6a"],["/posts/2516528882.html","de13fd08d03a51ae58debb7c90f0e10b"],["/posts/2529807823.html","adf425d2c6f150542a6cb5447d088f95"],["/posts/2888309600.html","02c615586e6944f055fe4a8c6ab0c2a5"],["/posts/2891591958.html","3a2fbbf067c2453185b5ed2191706f75"],["/posts/2909934084.html","d9adad142e664966a738a18cc95eb3db"],["/posts/3005926051.html","c8e04961c83277cbbfe4cd5647fd19c6"],["/posts/3169224211.html","1377b3e8ab247e28e075505238bda7d2"],["/posts/3259212833.html","7df6f34f2e1532564359fe3a436515b4"],["/posts/3266130344.html","65479edde18b39f758777fb261aa3083"],["/posts/3306641566.html","b937695b971f39c703d6e270268753fb"],["/posts/3312011324.html","89fd38c3001ee627f88761f9efbf9612"],["/posts/336911618.html","0219025a5f5da01d4ae789d507a524a2"],["/posts/3402121571.html","db271547135fbb49a55fbd8516e9743e"],["/posts/3513711414.html","71a32ae0b3f52e94210774037829ec57"],["/posts/3546711884.html","bb3dbbad0d6cc7557e283bb72d97627f"],["/posts/3731385230.html","ceabf8c84915d1baf4b60122ec6b404b"],["/posts/3772089482.html","a860866aa1546058431b7c434d38b32e"],["/posts/4115971639.html","8de0e5b70a1e708a096d9fa988d82443"],["/posts/4130790367.html","69a8b995d088df4c8c715ad67194b555"],["/posts/4131986683.html","9e9af3c81cef494dbe1ba83bf8fe9e2c"],["/posts/4177218757.html","ff1f705b47bb835fd0ce8f2afbcf84ac"],["/posts/4192183953.html","52f1b1e7168cc1b95c0ec789a78d0001"],["/posts/4261103898.html","9a6567274c72dc32bf4923dc35ae218a"],["/posts/482495853.html","445e25a173a99ea63ac334a35afecc5b"],["/posts/488247922.html","04794023415e6e38786146b22429a5df"],["/posts/570165348.html","cf77573ae0206fd263e5beb1f23c9263"],["/posts/595890772.html","5216a4c75bb059d5e12ff9d1c230e0b6"],["/posts/694347442.html","a830f12a1c0f9523fee7b48723c80c38"],["/posts/707384687.html","c8ce4b4c1d663559f5f264b5b136b918"],["/posts/71180092.html","4599833134d20eb01773c122a37558d1"],["/posts/716459272.html","5bac7d55cfc4882816197a938402920c"],["/posts/795397410.html","044f215c9e3d006402b9e758b924b4d4"],["/posts/820223701.html","00f78c059c8e338cbebe8629ee9e48ba"],["/posts/830372185.html","fb64a3056575170e480945f5e750af98"],["/posts/88294277.html","c53567a3742978b35e92a0e9fc7df15b"],["/posts/939963535.html","5d0b9a5f70fcb8a0a24921bde4a8fee4"],["/posts/983786067.html","33cecf14e3165ac6dec27ec1fa6bc0b7"],["/sw-register.js","33f646f6e8a44c70acd515654139c168"],["/tags/C/index.html","125db267d9116d7e7e2569c56804a901"],["/tags/C/page/2/index.html","0b180454071363c52640ce36923245f9"],["/tags/GUI/index.html","5ac031548b2d30baa9d8b1307a46318f"],["/tags/HBase/index.html","fdebc2c605593d724579f46395987769"],["/tags/Hadoop/index.html","a95376df043362f49849ebeb4771cc83"],["/tags/Java/index.html","461c204e288571ba60f1955ff949d2b4"],["/tags/Java后端/index.html","61867d2e14b96937b2658e6911b7c054"],["/tags/Java基础/index.html","e34c748e1d6224c0495f52c226e4de6a"],["/tags/Java基础/page/2/index.html","c29cecfd3ea172a6d360b4c54d8576d7"],["/tags/Linux/index.html","7542a1fae3005cf9da31e557722bd2a3"],["/tags/Linux/page/2/index.html","432cbf2f2cece89bce340d43cdfeda44"],["/tags/Mac/index.html","8437c5546e0c49fe37f73bc3ecc53871"],["/tags/Mac/page/2/index.html","01ae45683c27e8298315c89f1aa1095a"],["/tags/Maven/index.html","5a69716406008481974837de5a79eed3"],["/tags/MySQL/index.html","20304de5b10e1e0961edca7130004e0e"],["/tags/Python/index.html","c4f701b68c584731df088530c8c6e6dc"],["/tags/R语言/index.html","7323f39f22a13cf6fe28972596af82cf"],["/tags/Ubuntu/index.html","6c997988b0358d34f57f1f283a577b24"],["/tags/Windows/index.html","b9a894fb80353e5c6971342b6e9c85f6"],["/tags/ZooKeeper/index.html","d1bfb2159a0ec2c3180a047238901eea"],["/tags/bfs/index.html","a01b9b629bba01d2ed286edcb1531339"],["/tags/dfs/index.html","7a7f9d38ba1a9e3d46a961118dd3d9a9"],["/tags/folium/index.html","4aab55235574427a01a05d5e1c42a501"],["/tags/git/index.html","fa9705684f40dbaeae1b5424d57507c6"],["/tags/index.html","e0f67d2d0c098125c8759c4385dc24d4"],["/tags/latex/index.html","f608678462af31848f19004843a500cb"],["/tags/二分查找/index.html","ff7ec034d02f16c6a1bcccf50170815c"],["/tags/优化类/index.html","f7f87af63ad3df78d52d3485f19efd40"],["/tags/前缀和与差分/index.html","ca5e357819afc2b12df14742ed67613e"],["/tags/博客搭建/index.html","56bebbfc9c3fe8899e20f6d5cc95b8dd"],["/tags/图论/index.html","af1f74e539f8473fda9188ec9e98a9ec"],["/tags/大数据/index.html","3779ef74b616bb43c4fb998d21025058"],["/tags/操作系统/index.html","07085adecb2faf03c0e81a9829e0c396"],["/tags/数学建模/index.html","63fd4c41a0a18ce42b6245f84ac40448"],["/tags/数据库/index.html","08ce82333ede70a1e4a8157f9b1c953b"],["/tags/数据结构和算法/index.html","8de844d93706b087c6bafdc56f88ceec"],["/tags/数据结构和算法/page/2/index.html","08eb447ac6a16910ec0fb01a00ee6616"],["/tags/数组和字符串/index.html","99f1b585a107d62795421d4362ae16d5"],["/tags/枚举类/index.html","8007bc83214aea6ec3df91e26c0b3a2c"],["/tags/栈和队列/index.html","371df96b255cffd346502dd9d8a8eaa5"],["/tags/树论/index.html","ef5d03f1b44844e94dfe8c768b5ae43d"],["/tags/测试/index.html","79a67289b78c82385b301990ea227d48"],["/tags/环境/index.html","1d2e33d85fe72c154825e665358b4683"],["/tags/环境变量/index.html","3ba5fd1b1b1dce36493801479eb69dc9"],["/tags/绘图/index.html","ff90beec7cd75829fde722d4120731d0"],["/tags/编程环境/index.html","c2b6a0f4fe3a519d1982c78b6cf67909"],["/tags/网络编程/index.html","cf677138cb2697f927a42bbaec4713c0"],["/tags/英语语法/index.html","f491ad49f08c3e1d666344a06180f8a0"],["/tags/论文/index.html","a6299bde7435571a9dec2ac925302d4a"],["/tags/资源下载/index.html","80475619e02adc3c7da82612db4e846a"],["/tags/链表/index.html","310a4036838cca9e2fbc3ec72fa9530b"],["/tags/集合/index.html","18737806237650c1d2d1790b0a2f6946"],["/tags/集群/index.html","b92d5133dcb38e89d8090c907aa38c7d"]];
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
