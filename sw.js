/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","5ebc3ac90b0fe2a38b7f1075c544a846"],["/about/index.html","8195b9799a505be699fa65b7a59baf54"],["/archives/2023/01/index.html","8723a1f6ab0d18a8c6b06b5173b568a4"],["/archives/2023/02/index.html","123c297793505059f8304dd0c9aeaaeb"],["/archives/2023/02/page/2/index.html","906e26ae8af0327b6f883a89c4abf0ee"],["/archives/2023/03/index.html","2072cd3cc58a42ea94036b78ec814a70"],["/archives/2023/index.html","8c91c94afdd55d2451f323f3244c568a"],["/archives/2023/page/2/index.html","084f9a2a22f5eac2d0714678b348509e"],["/archives/2023/page/3/index.html","cbf1c1e0c083e8bb6b60e29931f2861f"],["/archives/index.html","df50d04df39e14e1a4c6f4f26f797ad4"],["/archives/page/2/index.html","2d9aab8af2005c38a8f265afec3e2cbc"],["/archives/page/3/index.html","8f1c8b15d1bf002a6be2b68189497a3f"],["/categories/Java/index.html","2496e3f8835395adfc66935de37b62df"],["/categories/Java/后端/index.html","21d840eb68ce595bb9816238669db220"],["/categories/Java/基础/index.html","34592c55f88b4241f737d98d4881b7e6"],["/categories/Java/基础/集合/index.html","88439c4da7ce5ec775be5c61e76c82d3"],["/categories/Python/index.html","2c8857b0ba6d4d59222d68398b30ef91"],["/categories/Python/编程环境/index.html","aa4d7e58ab5d17a8ef9c3275a857206c"],["/categories/R语言/index.html","bb5eee2a0831bf220bacc9ba21a1dee4"],["/categories/R语言/编程环境/index.html","4ef6272427545215ebcf2dde0a84606a"],["/categories/index.html","cb22138a29b60eb4deeeee84e954b652"],["/categories/大数据开发/HBase/index.html","9a7143690a2291d633dc1c309abff448"],["/categories/大数据开发/HBase/环境搭建/index.html","3e06c684e5739132e4f2993a4a1af899"],["/categories/大数据开发/Hadoop/index.html","b1be265f089537bfbe4f43acdfd67b3a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e937ff7492d7abdbfc2207d785f46c5a"],["/categories/大数据开发/Zookeeper/index.html","f6e87c7087c3ffe994a410d749d89559"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3f960b38f31c2b242346acde7cd4b35f"],["/categories/大数据开发/index.html","a655033caaaa2174724c3a0f9a9592bf"],["/categories/操作系统/Linux/index.html","d8752f38ad82f9bd0d4727fce727233d"],["/categories/操作系统/Mac/index.html","1a44c2d383792f3d69dae22b788d34c2"],["/categories/操作系统/Windows/index.html","0f7afc5b5da83820d8d0c76094a3512e"],["/categories/操作系统/index.html","182b6c5cd7145b085801579724198a55"],["/categories/数学建模/index.html","6d110f111c5c194d4b2b761ded93a1d4"],["/categories/数学建模/latex/index.html","c36bd4f9453457987aef658f2f325cbe"],["/categories/数学建模/优化类/index.html","0b10a0cdd2fc093ad78923823b4b2552"],["/categories/数学建模/优化类/现代优化算法/index.html","fda78d67aaaf379a951a43d5ebd0cf21"],["/categories/数学建模/优化类/规划类/index.html","24ee2da43277556c9f812186986fcfb7"],["/categories/数学建模/绘图/index.html","af766074a8c065beebfeda31b846b032"],["/categories/数据库/MySQL/index.html","6af17029f32d1235704dc285b1bf9dfc"],["/categories/数据库/index.html","0f6464dca07927300c2ce7373ecdc388"],["/categories/数据结构和算法/index.html","e91ce2b72d53691941ace84a6b1f5020"],["/categories/数据结构和算法/page/2/index.html","ed5d7e5bc38ae5127fbe618f9c45aad4"],["/categories/数据结构和算法/基本原理/bfs/index.html","d6a6499ac787e7bd99ce0e7429069fd3"],["/categories/数据结构和算法/基本原理/dfs/index.html","2722c5e5badb6c43332dd71f3d0ee99e"],["/categories/数据结构和算法/基本原理/index.html","9d0c00ada01400e1f3685e63f6e078a0"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c62680fe2585e49057105615d9ccbac6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","609ec8d1705329337c7f713301a48b0c"],["/categories/数据结构和算法/基本原理/图论/index.html","a3e3de4789ae8c58138a8335b35aa7fe"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","cf82c9b35e3fd5a5943e51b59e5e8ec3"],["/categories/数据结构和算法/基本原理/数论/index.html","26b5a2bc3aefd03f3206fa72abf4ecd1"],["/categories/数据结构和算法/基本原理/树论/index.html","0238282e2eb2ba2d18defd3e6797a19e"],["/categories/数据结构和算法/基本原理/链表/index.html","958dce62bd5288532c544afea5861508"],["/categories/数据结构和算法/算法题/index.html","8875016c00de9b9183944ecbf01b2d8f"],["/categories/数据结构和算法/算法题/二分查找/index.html","ae053b1c49db4b5e1194b5b5cf3e8632"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","9f9aa544b0fbcc57c176f9b007fcfee0"],["/categories/数据结构和算法/算法题/动态规划/index.html","fb7969a36aec00aae7c550f665aefd5e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","ac817a372f922bc21e7981e6765431aa"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","49727fea1ef382799169bec88eb7a183"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","01afcc999890f995e9e3cf081cf56899"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","092ab8f3a9046566d6ba335c64eaf439"],["/categories/数据结构和算法/算法题/栈和队列/index.html","ffa96315dc2b350efc69ec083c2d5e49"],["/categories/数据结构和算法/算法题/树论/index.html","4f8e698062b829b16f424161ea654c5d"],["/categories/杂七杂八/index.html","1ef5c9782248f2128d9493de2066b6d1"],["/categories/杂七杂八/博客搭建/index.html","2af7178de7ca86a048c07a9931e53bb4"],["/categories/编程环境/index.html","3ebad84a65db54d9627d1f3a9cd51e3c"],["/categories/英语学习/index.html","de1c936c22a6ec67a693d88c8406991f"],["/categories/英语学习/英语语法/index.html","6ce5173c71ba56b4c367b83f59320f63"],["/comments/index.html","e866fd8c9c75c19611051b94f7c00187"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","77cf91cb2e5d5c496daa4f60ebcd3556"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1b0f52833a0eb9a23e70dd5bd31f7dc4"],["/movies/index.html","d4cef123978d3d8c4ff27b1d51ad43d4"],["/music/index.html","bc300f7e0a89f1d408eaf125e620f42a"],["/page/2/index.html","42c7a7707e22c333572488c5ff1e9174"],["/page/3/index.html","028a51d670c2ea99add299ff5ce4dafb"],["/page/4/index.html","7f9b75f5a625e014cedce55e9db3f771"],["/page/5/index.html","3d105c9ec329bcd704b92942ce13183c"],["/posts/1021360842.html","c4b8ababe906eb40fa351cddbc8333ef"],["/posts/1120620192.html","7081fd28cf3ce9ab73ed451db50c906b"],["/posts/1141628095.html","aafdf7819e168b539b2e09ae4c90f222"],["/posts/1168613674.html","7c23711c8abee0a293f32568290a5775"],["/posts/1219920510.html","25fbf333b621c0911d7d33d0bc7a7492"],["/posts/1222166338.html","847962590115c193cf8efb3df52ad794"],["/posts/1259097482.html","790c6c14af41d8318e1f11deab17a4c8"],["/posts/1271036369.html","e5c6cff29236244470e9886548fc8953"],["/posts/135355774.html","3ddd6b8fcb2120425934ebbae3ff6f69"],["/posts/1375344716.html","dda1aa0ad9eee8b29f5fc1a14e2f54a7"],["/posts/1388991698.html","a689115cf5ae2b8f646eb89560debda6"],["/posts/1410315814.html","27c143d7772f66a77560ff76e5e67f9e"],["/posts/1452790229.html","a7665461ce3f05dcdc9fca99f6e45c67"],["/posts/1470079884.html","655363a25d3187226d88ffb4b0dd567d"],["/posts/1470079885.html","fb007c1f0dbaa2e20e25478272657438"],["/posts/1470079886.html","8e2d9961223c3446a432d239743577f4"],["/posts/1470079887.html","53e8d5eb963aa493b6cee8c9bb8ea5d1"],["/posts/1498536549.html","0ddae50ee999173fea24418cf9e053ba"],["/posts/1557866301.html","c3d7b136b7ac93cb893ecf12fa5e73b2"],["/posts/1571776361.html","4565006c0d4ef3ee64a4c98abe4b9468"],["/posts/1605124548.html","d4026fcaa230adefbc0c12b3677d5cbc"],["/posts/1633036852.html","c48cd478ad6471fc3a9b92f33242e6f5"],["/posts/1765123828.html","6c401008acabfdc647d176dd83df4a6a"],["/posts/1776114197.html","8287c0720d2180dce989ceaa3d80901c"],["/posts/1817748743.html","b5b3315dbc8b8a5f2db39e886a3e426d"],["/posts/1925125395.html","c1015571cebb791b8dd007cbd9869d00"],["/posts/1966191251.html","af56cb856d5d5e41b935b83b81ceaae1"],["/posts/1987617322.html","962ca224fe848a8fc10b2fb2a5b38adc"],["/posts/1999788039.html","25062fb67762431ff91293d75757558f"],["/posts/2075104059.html","97744000549e4dafe538b04d74b094aa"],["/posts/2087796737.html","23f8c9b36f4a94511e00293a1073be8b"],["/posts/2207806286.html","bebefa3becfd1f1ede25ab05b8f390ed"],["/posts/2225903441.html","2a056cb6846258bb43b5f3f067a75c12"],["/posts/2265610284.html","57a5ac63bbcbd68b1d4b7cfd5e350051"],["/posts/2281352001.html","a47dbcd76bd877610c82ea50091d3678"],["/posts/2364755265.html","6ddadbac9f59c153f83f856389ac6f1f"],["/posts/2414116852.html","c81a987dcc8216f9e769edff2869b5d0"],["/posts/2482902029.html","59a1900153cce742f85121f1a55f87ac"],["/posts/2495386210.html","0ce5d51ecf09635595ed5590729e6b83"],["/posts/2516528882.html","91f8152a17f93e762088a3a2122ca570"],["/posts/2526659543.html","b335d992f11470699c2f840337a8720a"],["/posts/2529807823.html","d5e85f8e8cfc7da34864e4a0337d5df2"],["/posts/2742438348.html","6ef1a4645b6bb41124e35040117f7185"],["/posts/2888309600.html","bb89555c08aa4b3f82fd92ddf9795d35"],["/posts/2891591958.html","0b166e9af1e7d7e767ccef97fd730e78"],["/posts/2909934084.html","6b86428385942edeae751e06387ab9ee"],["/posts/3005926051.html","33651d0e9f46e9454e55fff3a0613d8e"],["/posts/3169224211.html","5415f6dc1f19d6bfc36f2bf7fcc7945e"],["/posts/3259212833.html","01a29ecb02c90538f1b336959f8942c9"],["/posts/3266130344.html","654eeef2c4340387a7851d4239c86741"],["/posts/3306641566.html","f9762876d0518e1882146f314fb3af1a"],["/posts/3312011324.html","49f8d6aa72a6f5ceda2286cab6f42d36"],["/posts/336911618.html","e6ffa46b628e5bbd571f23897848d234"],["/posts/3402121571.html","33ecd894b531827100a336f3e3367d46"],["/posts/3405577485.html","99ad9c39eb91503feafddaf7374e9d44"],["/posts/3498516849.html","29b98c22f08a1dee5fc6e64408e77f55"],["/posts/3513711414.html","e04b3a9615e01836a2c4b927aaeccdbe"],["/posts/3546711884.html","e0d774f3410b0c344793c81ea70fcf72"],["/posts/3731385230.html","28b2a2b9ae9a3fdfc8a864196b166ec9"],["/posts/3772089482.html","85253ef9eb0c0c30d6dbe8b5fbaee53c"],["/posts/4044235327.html","1cf57d7e860f0976d4e0da2f3334e022"],["/posts/4115971639.html","5fe01554129c9ab5a3d62e571be7a695"],["/posts/4130790367.html","2c0eaa97073fe1787729b81330b3d582"],["/posts/4131986683.html","e185408247c5ebd7e4bea9c49cae0fde"],["/posts/4177218757.html","8c34ed83d604c14d3595342542f72127"],["/posts/4192183953.html","8a10d4af0b1ddb361077a63d9bf002f4"],["/posts/4261103898.html","77607e5a0006a764e88b1c049ac59447"],["/posts/482495853.html","340d1e88dcc951d61dd37d944ebedbe0"],["/posts/488247922.html","0d63d4f216be53dc5acc5ba364d1a895"],["/posts/570165348.html","7a0e096b9497c9dd44900e7bd7986b26"],["/posts/595890772.html","b60cd6aa2bc457f1173bafca27ed1810"],["/posts/694347442.html","18086d06c7789fb5a8fb8a3f578ee957"],["/posts/707384687.html","a3b0f0f5bb4dcee0ed649b69466acbdb"],["/posts/71180092.html","84207ee0000619092d430c0084ffb9ef"],["/posts/716459272.html","418b087bab0438d2070abe2e0bdb8405"],["/posts/795397410.html","8004c3fc5d49978113fcb7019acc0fa8"],["/posts/820223701.html","36ee0384692514d18a0ab2c807da2489"],["/posts/830372185.html","fd4b419374fcacda6de49f13289447e1"],["/posts/88294277.html","396b5c504dbda011e67423244c38f821"],["/posts/939963535.html","3ca06dcaca8adcee3f362358c4225350"],["/posts/983786067.html","0f15dbd75c6e54699eca2befa01e0523"],["/sw-register.js","63f860464c4d28a984ec26fee77af11d"],["/tags/C/index.html","f388a6e5dc9ccba1986bb66a93cd75e2"],["/tags/C/page/2/index.html","4ccbe138ab024468dc668f57453ef65a"],["/tags/C/page/3/index.html","32d5016b086c952339d56c824ff49c1c"],["/tags/GUI/index.html","35e4658804c8ad53b1b0f30c51775165"],["/tags/HBase/index.html","4dbed18d6c25cee4ec001b1d67759ab2"],["/tags/Hadoop/index.html","73eb330bfdce2819ae28217c5117669d"],["/tags/Java/index.html","f09cf469ab1db0f7aae2b019d7c4b6a3"],["/tags/Java后端/index.html","b0bf4d86a9ba28a3b08322a7c54f504a"],["/tags/Java基础/index.html","5d5846391cf35a7ac875d8d742621fe1"],["/tags/Java基础/page/2/index.html","5d7c0a1574bede90ecf36bdfee8b953d"],["/tags/Linux/index.html","2ea47bc2b924aff6c8de98861f520ceb"],["/tags/Linux/page/2/index.html","e2db068d4f2953749d96f849401be7bf"],["/tags/Mac/index.html","13d103c90590ad0db41bc22e2b9e2b87"],["/tags/Mac/page/2/index.html","7c5b85a200ebc710a75c4fdb739fff7b"],["/tags/Maven/index.html","851adfe4b0b9cb2e9b7f766b20476288"],["/tags/MySQL/index.html","95297bacb23ee9fefe354cd5e15138b5"],["/tags/Python/index.html","bb48c21a74404f90870291e35df68c20"],["/tags/R语言/index.html","5444233a4d0ab27dc6ed5a58e050262e"],["/tags/Ubuntu/index.html","6bb6c1564da1af9d62ae6f7a12695dc1"],["/tags/Windows/index.html","2d4c21a4c9128d80fb0eb17bf5f311f4"],["/tags/ZooKeeper/index.html","9813619baac49b2b03980d2dfd718c2d"],["/tags/bfs/index.html","dc704813995da39ee113a54c17c9dbf3"],["/tags/dfs/index.html","0f83bfd80e5c23a8747e3c27084c74a6"],["/tags/folium/index.html","c048961ff2464bf26c65ad389ef0cabf"],["/tags/git/index.html","07710ec37c7736ceedf956d7dca97d9b"],["/tags/index.html","1e024086b6a2d0e082a73bf6e8e8b025"],["/tags/latex/index.html","ea0a415a0c55548ffa1368cf0f4a5ec0"],["/tags/二分查找/index.html","c2a64fc3da0d4cc76e3c8b7ed68f4c34"],["/tags/优化类/index.html","73551ada99b37ad6cf12653c88542c25"],["/tags/前缀和与差分/index.html","96880ece51c36a90cccd4fea59894361"],["/tags/动态规划/index.html","16c980cef402eda2c69e244af875db05"],["/tags/动态规划/page/2/index.html","ecea7aed8fa02b86e4005330b908f652"],["/tags/博客搭建/index.html","7c35bdee130cd15ad6f226a2b37293cc"],["/tags/图论/index.html","5e99d76e5ed97e4bfd6d1673042b6901"],["/tags/大数据/index.html","b924f67834100a221771f0163980b7a8"],["/tags/操作系统/index.html","0685e34b9af4ce05f3e7baab5a68c130"],["/tags/数学建模/index.html","14622a5a1ebca65589c4174d28079400"],["/tags/数据库/index.html","76523aa74fb32f409224c4836d6778ab"],["/tags/数据结构和算法/index.html","a32ececb5a5748cfe6f228a90b2b25b1"],["/tags/数据结构和算法/page/2/index.html","95605751a4a330e0e1ae4a1596dbcd02"],["/tags/数据结构和算法/page/3/index.html","5094d062839436fc8519373b50116004"],["/tags/数组和字符串/index.html","198c9f1908804f378241f7d6f62d490e"],["/tags/枚举类/index.html","4e48195dc86f4759a5a5ec4d1d23f2a6"],["/tags/栈和队列/index.html","413a0e932bff0ab9449dc37d60a90310"],["/tags/树论/index.html","13ac77320c80efbbaac3aeaa4a1cd175"],["/tags/测试/index.html","d9b4b7c7757570fa4d5438bd1091ccd1"],["/tags/环境/index.html","a8d8ecabe6542473a940b07576956e68"],["/tags/环境变量/index.html","f056f21496e9a41c12ad48784e064852"],["/tags/绘图/index.html","1dbe4f1bdf28bbf589f77339e42081ed"],["/tags/编程环境/index.html","074ea0ee05833397d3566972bebc9af1"],["/tags/网络编程/index.html","e24be6cff00ac7e86b2e654a55cc212d"],["/tags/英语语法/index.html","ddac155e860fd861ff15d99b98340f5a"],["/tags/论文/index.html","6e0e01e045eb2a6066410a8e3c671baf"],["/tags/资源下载/index.html","d74903cdc9d40b2112b61d07124d0c2e"],["/tags/链表/index.html","1980438a0b508aea8753269e912c7f8f"],["/tags/集合/index.html","6e51d6e42a062c77d863236a171501dd"],["/tags/集群/index.html","8a4a3f0327dbb81c7fbaca01736ce60c"]];
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
