/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","225ba8367c78aa909c452ff1446e2949"],["/about/index.html","36add90a1dffec8b202b8f049142cfbf"],["/archives/2023/01/index.html","30c2ad958f94947bd74864f29d3e8631"],["/archives/2023/02/index.html","622f6e53af99d87a1ae118f21f221a19"],["/archives/2023/02/page/2/index.html","45f787815562f6350e508cf370aa21b3"],["/archives/2023/03/index.html","3ea35f90271868859fa6287ebc281088"],["/archives/2023/index.html","94b753f618de6239bdf5cd635c275dae"],["/archives/2023/page/2/index.html","57716d3abd9af7ae721a3261e5c7a45b"],["/archives/2023/page/3/index.html","6fb1266635fd24d6d7284c96aeb90690"],["/archives/index.html","5f2473ec3f663f951b26f284c64da0da"],["/archives/page/2/index.html","215e47cc451701b657a56a74b75100a1"],["/archives/page/3/index.html","5fc27581f55406ff1abd2ad08e402234"],["/categories/Java/index.html","97826775624c6fa24daeb3503b61af36"],["/categories/Java/后端/index.html","7cc4d0640355bca0bbb61f5dcef6fdeb"],["/categories/Java/基础/index.html","944d15970ac22b645a88dab4638e0639"],["/categories/Java/基础/集合/index.html","a15c902c845dca76c6c8257a2e1543fd"],["/categories/Python/index.html","ca02983a1091da2eef5883bd05d4c9e0"],["/categories/Python/编程环境/index.html","a37074ff7beffd426171e6c55e5cb05f"],["/categories/R语言/index.html","9acd9f1cafaab10a624a96c529d48258"],["/categories/R语言/编程环境/index.html","1c0c8884a171c8a8b1c4c14c97d0836d"],["/categories/index.html","1e4d5b7554a4a166dd13d5a926318d60"],["/categories/大数据开发/HBase/index.html","d73860af577a1cc209e05975f26bf053"],["/categories/大数据开发/HBase/环境搭建/index.html","56a35d13666a1f4c1afea4b2487a16a0"],["/categories/大数据开发/Hadoop/index.html","f301b8a27ed18ace411f8a0fb0c3ca35"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f89d7d46a7ec1c07d24de231889b35b1"],["/categories/大数据开发/Zookeeper/index.html","59ce72e6b5fd21067abfee6d1f901a55"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","d5f36069a9b22c4ad91d662dd595561c"],["/categories/大数据开发/index.html","4e5228ddecea4fd3491c988f3178384a"],["/categories/操作系统/Linux/index.html","22203d1a6753d21b4a91df90b8cf743a"],["/categories/操作系统/Mac/index.html","376ab23b981f7ae7de29df101ebe9b6e"],["/categories/操作系统/Windows/index.html","e8893649fdeb8c01517b06de14be9935"],["/categories/操作系统/index.html","e02ca668f21d635a9e61ca37f1e38db7"],["/categories/数学建模/index.html","7853de2f861a2e778020690f6f9f103d"],["/categories/数学建模/latex/index.html","93f1eadd127127bc135497a838ab6810"],["/categories/数学建模/优化类/index.html","595fc5ac283ac524e23d1899b1071f91"],["/categories/数学建模/优化类/现代优化算法/index.html","021520f1d001b60f63c36a60e72154a0"],["/categories/数学建模/优化类/规划类/index.html","43e18aa6dedc0e97bcf89b58d1ad08d3"],["/categories/数学建模/绘图/index.html","bb83d57697a79454e1d50394b985b383"],["/categories/数据库/MySQL/index.html","197cf8801036472b427db30a08d72da1"],["/categories/数据库/index.html","124570cd0d00857ce688036ed2639f90"],["/categories/数据结构和算法/index.html","5151a06c9dcac3925ac13becd3c13534"],["/categories/数据结构和算法/基本原理/bfs/index.html","34297885de5672d3c36687e00fe9072b"],["/categories/数据结构和算法/基本原理/dfs/index.html","494f483be181514a23955d767274d304"],["/categories/数据结构和算法/基本原理/index.html","9b139ba5d517ff78015dc0973071efb9"],["/categories/数据结构和算法/基本原理/动态规划/index.html","1d60f30ab2e8d9f212ab1e481c41c43c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","23ead8bd7ee27383531af07d412f7211"],["/categories/数据结构和算法/基本原理/图论/index.html","27d3a6f6479706c18487e9cd91c7f685"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","000898cf0ee552a426b232c75035d970"],["/categories/数据结构和算法/基本原理/数论/index.html","78fc604dc5323fb84a619cd545a0907a"],["/categories/数据结构和算法/基本原理/树论/index.html","c3341e0c9cefae888e6ce019e94cbb11"],["/categories/数据结构和算法/基本原理/链表/index.html","5b37fc0ec1c6ee3d305514752416ce99"],["/categories/数据结构和算法/算法题/index.html","af54f995b0709ccd2748f5267245dfd0"],["/categories/数据结构和算法/算法题/二分查找/index.html","be9d39f145d0b343183c4c0042c025c3"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","7f9ecda1890178da8f7bc0a19133847c"],["/categories/数据结构和算法/算法题/动态规划/index.html","c2562b08bd1a8920ca6aa4f41da3a591"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","eeab098ec94177345219f2f67c90c861"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","3cbdaf0ba2d9a935a04a98710dbfc757"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2762da06c77c2a694593b54f58872ab2"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b1913ff364ae2982084a7022e4c5c2d0"],["/categories/数据结构和算法/算法题/树论/index.html","ae5acf7ef8d2f20b37fd72e7c81114e5"],["/categories/杂七杂八/index.html","bfe832bcc6883c42014ea8a802116dd3"],["/categories/杂七杂八/博客搭建/index.html","1083cbf3b868a791a9cd7ce79d2bf64f"],["/categories/编程环境/index.html","097d5f6ba2f7f6d47f635b59c4b38991"],["/categories/英语学习/index.html","820f1d232ec06de8bd1a155601509a11"],["/categories/英语学习/英语语法/index.html","67121a9762a15bacf65f631fe49a8163"],["/comments/index.html","5feda6b3f17b4e3e53ca58609ca659c8"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9a063d2819cf69fa737f48605fd8cbdf"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","31321d2cc7db6ba054579eacae16e717"],["/movies/index.html","a39bd5fbb9178ec3be947fb8a540a8c9"],["/music/index.html","d000503fc514bb71ba62c890ba96994b"],["/page/2/index.html","1f505197c3d0e2eb09a5fabb4933e4af"],["/page/3/index.html","347c6db4361651de279448ecf2ea5bac"],["/page/4/index.html","e30bdc008fa1907cdba9a40a585b2a44"],["/page/5/index.html","e35def2fdb31b36753f539740c219726"],["/posts/1021360842.html","e86f4ce0228571610c30c362515b8760"],["/posts/1120620192.html","5374f757c8be3214922d3450416292f1"],["/posts/1141628095.html","e938ceff01dfa0d499f44270bd9f1409"],["/posts/1168613674.html","4c0dd55401f3a0dba5e356f8d0c3a87a"],["/posts/1219920510.html","3749bc627fc3a9c2c4441ef6d05148c7"],["/posts/1222166338.html","5e87e761203295e24b4ba55480dc0525"],["/posts/1259097482.html","658e7e91b0b89b2456e0f82b682a4a39"],["/posts/1271036369.html","454bc417d933ad4f2c3d06c1aed4744e"],["/posts/135355774.html","c58dd18e19d251f647efd06cc7d3ce06"],["/posts/1375344716.html","0d51bb5cb21f31d664d4c9d2a7735dc7"],["/posts/1388991698.html","1876bf590b8157f8db3fd1900004a921"],["/posts/1410315814.html","509533f5f4e6a5ee93b884bbb4185dfc"],["/posts/1452790229.html","40b18c07413d7d959d7d4b5fc54e6019"],["/posts/1470079884.html","b6403bcf46c265ffd2b0a190f95aa5ae"],["/posts/1470079885.html","35270c022de9a6de87668fa2ab0ba0c6"],["/posts/1470079886.html","883609e93953b8de730ee73fbdc520a2"],["/posts/1470079887.html","61e2b48809eddbf69ac1dc5e373888f2"],["/posts/1498536549.html","66fbdcbe0e5d2a376a8f9cb93a148e6c"],["/posts/1557866301.html","c6ce22ab567db8c33254deff0bacdcfe"],["/posts/1571776361.html","4917131c6d7ab3ccabebc2cc88a0a120"],["/posts/1605124548.html","03758a67201978d629e27e19427add8d"],["/posts/1633036852.html","d2892e563365bcb586c5286d9928c716"],["/posts/1765123828.html","c3f44d2a842e3f60506f8f7dd83ab590"],["/posts/1817748743.html","2ae16fe3701750650493aed7b032da01"],["/posts/1925125395.html","fc8bc242aa26902f478e0b26dd287a67"],["/posts/1966191251.html","5a88c70863b709b42a2f034a27d4101c"],["/posts/1987617322.html","3051011ac028f6a39580043beeb97eff"],["/posts/1999788039.html","53970dddaee0e4afb756fdc230adf2a9"],["/posts/2075104059.html","082e07fb13d7d58e24b3b916dc707354"],["/posts/2087796737.html","3535e5378408630240562db6aa155f5c"],["/posts/2207806286.html","8fd179ccc67b930a47e95b9d008b499a"],["/posts/2225903441.html","287f4d81267bd7daa5e85020f5ba58e6"],["/posts/2265610284.html","6a4d431299c4705ec754af621d4c134c"],["/posts/2281352001.html","44cabb2b3503c911f7a3bd726725ca3e"],["/posts/2364755265.html","2a2b94d6b579fb9a865142b4952877ef"],["/posts/2414116852.html","942e3f718ae50bcbd9ba195d3416aeef"],["/posts/2482902029.html","a1953e57d32849b247ce3168a59f7b3e"],["/posts/2495386210.html","ee76caf791de153f10380cf8e9970a03"],["/posts/2516528882.html","f7c5643c202070b9d0b541914ce58465"],["/posts/2529807823.html","a71d00fb71941e9349cc8be8e4e3eef5"],["/posts/2888309600.html","f3b5d7dfe13f0d96bcd2d9bfed81c1ae"],["/posts/2891591958.html","1cb44347594872e29933c57b9ab9729c"],["/posts/2909934084.html","f523621afef791047a9753078ffe6ca1"],["/posts/3005926051.html","75b2911fd5732dd018c3e8ba2582135a"],["/posts/3169224211.html","0bed5335c404697ddce059148da75a14"],["/posts/3259212833.html","c2177a06a1da454578d15ab2a68a4018"],["/posts/3266130344.html","f1a0442b78fe7112121538338cc35899"],["/posts/3306641566.html","66bf050c251902d20a9835c6b469b426"],["/posts/3312011324.html","0744dfab6a6310f20420802f4eda118b"],["/posts/336911618.html","001f52c78ed9b0cbf05a1dbf2c8871be"],["/posts/3402121571.html","f3e28cb261180e9b3e8656a5612516f8"],["/posts/3405577485.html","ab00549021de8a6f02a27fe1198c52f4"],["/posts/3513711414.html","79a3e1eda41b5f49d55481687b2b0b15"],["/posts/3546711884.html","40a3befb39dad97a22eba123e7f2f7cf"],["/posts/3731385230.html","92f31603944bdbf97fccab3d129883e0"],["/posts/3772089482.html","46615402a28f1d3084df27dcc9f7fcdc"],["/posts/4044235327.html","53f13875d4052267fdc9b6bb20773505"],["/posts/4115971639.html","085b235ad1eda76bc34d23d08e91e6bb"],["/posts/4130790367.html","49b215b919d747c162163472e109b8e7"],["/posts/4131986683.html","7bca706aa8bbc554a4189afaa2c4ff45"],["/posts/4177218757.html","6194d58ae1c877eee686e6c1fb8e8cb8"],["/posts/4192183953.html","61221c3b49410c1197ad74a6deeb3299"],["/posts/4261103898.html","6b1b593f5bdf21f2723fc5a61ca15d80"],["/posts/482495853.html","58da562198c40f23131978617743b6f1"],["/posts/488247922.html","d0523d1188e285b556fdcca3550ec4a2"],["/posts/570165348.html","cd66f9853e9563bf4d4726df5d0cfcfd"],["/posts/595890772.html","9effe91deba416391b6e2fbc774769ab"],["/posts/694347442.html","01cf1da7125e06f64e5a3dae120c5210"],["/posts/707384687.html","1c1955472d54b5fc1f1ce27b88915380"],["/posts/71180092.html","33bfacf5db07c55c64476a7a44180c29"],["/posts/716459272.html","5da3a5ae4611281bac33562a38869ffb"],["/posts/795397410.html","3da2ad98c06fed9d789ea3a4c06438f4"],["/posts/820223701.html","7a84709325d7756e3e3127465730a115"],["/posts/830372185.html","06d446fd77f5ff3ecf6e29431bdbae0b"],["/posts/88294277.html","40c5fbc6e926d739a411a4123b803910"],["/posts/939963535.html","fbd988ddb363b520289d3e2282925311"],["/posts/983786067.html","ea2371ccbd4edb62c557bcc7c708f438"],["/sw-register.js","f00997e68684e27748aa2f1cd094cfc7"],["/tags/C/index.html","90c3670f0a0e951462b540b95e5c4c27"],["/tags/C/page/2/index.html","0e450e06f180a6b264a88d2dee9c1f66"],["/tags/C/page/3/index.html","bd4c152583da805b910dbf3e7e90c042"],["/tags/GUI/index.html","e0bf773711b6f54858c1712f034804fc"],["/tags/HBase/index.html","503f910bf68cd59bc6be2311246f99a3"],["/tags/Hadoop/index.html","5c444a8b9246b403e9d3f9bcb43db75a"],["/tags/Java/index.html","823e0e861f50cf56188960983c7ad247"],["/tags/Java后端/index.html","99572fc2de1acf0b8290839f94062c2c"],["/tags/Java基础/index.html","bbc63ac181e5c08aa3b2f247b645414f"],["/tags/Java基础/page/2/index.html","1d1686afc47cc3508017748785ae0dd4"],["/tags/Linux/index.html","928d7f020c0e93f1a15190272d962585"],["/tags/Linux/page/2/index.html","1a557dfa773f7119ec4ca7d67ae464ce"],["/tags/Mac/index.html","96e83b6e2a03f87180a8f9037f95be02"],["/tags/Mac/page/2/index.html","a0dc961e62c2c7203b2c458cb48cbf23"],["/tags/Maven/index.html","442aa182431033ce62fdc162e0433985"],["/tags/MySQL/index.html","488b7300aee30eb17aaabde60d077648"],["/tags/Python/index.html","bf212c0610ac86aa3554ddaa5e614f94"],["/tags/R语言/index.html","272f581413bb72f9a6fd6935511a0f53"],["/tags/Ubuntu/index.html","47aae99d5a953584cf1743a3fe70855c"],["/tags/Windows/index.html","931733b20f2899d3d121cc86966768c4"],["/tags/ZooKeeper/index.html","4e3ec9500e6b2672270e6e469b64faa6"],["/tags/bfs/index.html","bf31db0a57a52ff79b5459fabc862fd3"],["/tags/dfs/index.html","1888f8d7fa2f4ccb44ef25f5c549361d"],["/tags/folium/index.html","9c8739671bc7df92ee9c4d1fd6968b7f"],["/tags/git/index.html","cd0eab2bc36ee48ebe6e498865917e90"],["/tags/index.html","3987e11bfc38965e0350e2e3eb3a045f"],["/tags/latex/index.html","67d41c95bfbdb09a3de85cf6771c9d83"],["/tags/二分查找/index.html","adfaeff6a6119b0389a34ff91b723a83"],["/tags/优化类/index.html","091c987326a659f5cd414ad6e79eefa2"],["/tags/前缀和与差分/index.html","3bf16e422eaa51e226b26dad57621f8c"],["/tags/动态规划/index.html","2a16aec73868dbb25a9937171faa9b97"],["/tags/博客搭建/index.html","57b14e9a44752182e49f99076f0cc895"],["/tags/图论/index.html","b9318b09efa1fc2d4338ed995723ddf3"],["/tags/大数据/index.html","4da770c96ae06c293fb65c4b7a94e841"],["/tags/操作系统/index.html","7c54577076cb405eae98d579472e1799"],["/tags/数学建模/index.html","6d5619c4df11e33aefff97e4cf483f59"],["/tags/数据库/index.html","fbda865bf52a60a42b1b35b3e2182047"],["/tags/数据结构和算法/index.html","4448a6893d5f965ccabb009d72ce2c38"],["/tags/数据结构和算法/page/2/index.html","773d38e537a1d375e7e9b8d60271a7b4"],["/tags/数据结构和算法/page/3/index.html","2723c14361bf4f465246ef21458bec23"],["/tags/数组和字符串/index.html","56768e4c77720d26f29f8a2eee091c7b"],["/tags/枚举类/index.html","3ca0ec05eaff358efcb92d096a6945bb"],["/tags/栈和队列/index.html","f9ce4c54f9c9b4efb9bf52b799481484"],["/tags/树论/index.html","f408b7c53744b2222f8460090dea9dd0"],["/tags/测试/index.html","710ade7f182f72bc89aea17b133643ad"],["/tags/环境/index.html","4815e746d80d626324ffd1c5af79054c"],["/tags/环境变量/index.html","721b4b9e3f329ba0e4ad2e043bdf8cd1"],["/tags/绘图/index.html","f2f029887c5b83a8ea94be7a16e6a581"],["/tags/编程环境/index.html","3c41a1268c8824fbbcb4aff3e56c13c2"],["/tags/网络编程/index.html","ec8ee32f2c8b5670899404b973bf63f2"],["/tags/英语语法/index.html","dfc579bd852309726b15f4dff656b6f9"],["/tags/论文/index.html","6fe201eb5257e1bd902736ca1399e473"],["/tags/资源下载/index.html","01106c3c5bf75058d32901cfc73486e4"],["/tags/链表/index.html","6a8f6908656ccb9d2d026f87cdc1e78b"],["/tags/集合/index.html","6f2bf0b6f6d31097d99c127e6609038c"],["/tags/集群/index.html","0a4b5b100b719f58ca742a9bbdd70a88"]];
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
