/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","0e8a8d7cf1a1593a79d55dab5771ccad"],["/about/index.html","3a69551a6894287b06d046e90a6bc2c5"],["/archives/2023/01/index.html","70a060bd8c10106cfe4dddd5bf97d2ee"],["/archives/2023/02/index.html","003b98f121a28408be809fa543cf5bc8"],["/archives/2023/02/page/2/index.html","a451128a2478a870f0d001389f0bd595"],["/archives/2023/03/index.html","7b1a8e53f96346482fa5cf7358117a71"],["/archives/2023/index.html","95ddf9575c33cadae3a4964f5344ffd8"],["/archives/2023/page/2/index.html","dd786af550dc726d37ed852611be5930"],["/archives/2023/page/3/index.html","ba899d0e4f69caa16a88e98dd0ce92bf"],["/archives/index.html","0654607a603ca9e3b874330fe36874de"],["/archives/page/2/index.html","357c342a192eec203c0254cbaf76afd8"],["/archives/page/3/index.html","54710780aa3382a2b0d0715d951a09ad"],["/categories/Java/index.html","0269bb14e1980c4ec71ef8134666b723"],["/categories/Java/后端/index.html","1a4eaa182a846d92fb93ca565353b671"],["/categories/Java/基础/index.html","88780dcf7a739a3be634e31ce55fea59"],["/categories/Java/基础/集合/index.html","5e2905363f34767c0f53c3eb2f60aef3"],["/categories/Python/index.html","4a8a7a246ba736d232ea6131801a8aca"],["/categories/Python/编程环境/index.html","4a322ca4c8299b70fc7e5143460a1d42"],["/categories/R语言/index.html","65e5a502621f2c8916cdecaab5d54585"],["/categories/R语言/编程环境/index.html","93eff7a4808880bb8b0b95dd538f39e6"],["/categories/index.html","ba24e4193562241725e8d16bc3ec4f26"],["/categories/大数据开发/HBase/index.html","4d6bb9665b5a6f352e1e414bca91d527"],["/categories/大数据开发/HBase/环境搭建/index.html","0daf07be4f1e42ddac4f1be38f22f20f"],["/categories/大数据开发/Hadoop/index.html","3e37307b5070b71e3b7333fa1401735c"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f3c2940cd0e52468f085c7d8bffd2963"],["/categories/大数据开发/Zookeeper/index.html","83d1de40552a20af6a2491534debddac"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","46d9515fb5574eb2e4ec9346e9a1a0a4"],["/categories/大数据开发/index.html","0b203b36afd1ae874bcd30daf69784d2"],["/categories/操作系统/Linux/index.html","22f757ffaed19f409c8a11b1030bece5"],["/categories/操作系统/Mac/index.html","6f0b14af0b10683ca8db82ebcc2f8ffe"],["/categories/操作系统/Windows/index.html","2df99501b1746e764d38d5bb1fd6196c"],["/categories/操作系统/index.html","badcab62bd63f50e0d7a7f564cb1a40c"],["/categories/数学建模/index.html","c92785e6b86ad4f44b000af335af3fef"],["/categories/数学建模/latex/index.html","e2fc8cca98df491b41d3360ba05a2140"],["/categories/数学建模/优化类/index.html","ef04e6a0880047874761046687c1e11a"],["/categories/数学建模/优化类/现代优化算法/index.html","72031b6a973b06ae14a3e2815db879a0"],["/categories/数学建模/优化类/规划类/index.html","95c9132834b73dc01ddb2b526322bd5d"],["/categories/数学建模/绘图/index.html","93067051a3a2165ed73c397995009193"],["/categories/数据库/MySQL/index.html","5eb4ca7fcb5b6951f6ec7856e5a14c67"],["/categories/数据库/index.html","a13ffd2b15d2e946e72034ba3607893c"],["/categories/数据结构和算法/index.html","a075b6a3abd9608414bb3dd67e7f7d9b"],["/categories/数据结构和算法/基本原理/bfs/index.html","b41756aa6a7fe62637c6ae1e9aa00c78"],["/categories/数据结构和算法/基本原理/dfs/index.html","492c8da9358aa64fd25298d933d9c0dd"],["/categories/数据结构和算法/基本原理/index.html","5d73dbb6106f794ad8514acc5e1d7466"],["/categories/数据结构和算法/基本原理/动态规划/index.html","a219c7fb134494f5b3a60e2903f6a010"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","01a8e3d5568207e73960b61fb10866b0"],["/categories/数据结构和算法/基本原理/图论/index.html","036861db1fffa1cb6f34930b7ea26702"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","07e7c07b113dd0f3dc459e55220eed4d"],["/categories/数据结构和算法/基本原理/数论/index.html","d646b482c939bfc256708c38a7f57d9b"],["/categories/数据结构和算法/基本原理/树论/index.html","5d506bd054defed83433b06c155a0701"],["/categories/数据结构和算法/基本原理/链表/index.html","50e2e72208cc7284d9ea7dbc42782551"],["/categories/数据结构和算法/算法题/index.html","10a6414f74c9cab0861649fe236eec9d"],["/categories/数据结构和算法/算法题/二分查找/index.html","2c254a8deaf065e7ab20889d4d716125"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","48d86e4a104cd5e1f8c03fe1f4ff8085"],["/categories/数据结构和算法/算法题/动态规划/index.html","5f1a09d904c2ad280370383fc5ef0502"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3c7d95070f50ff2d40d3ab4ff3e86132"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b3c62ea6ff74deec5958c80ebceecfcf"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5d72af6d6d9eec72d17a996ca445c1ae"],["/categories/数据结构和算法/算法题/树论/index.html","6ab6d2dca029db9f33730d35e59af08d"],["/categories/杂七杂八/index.html","02b2b452f73a41a04f726c33104593af"],["/categories/杂七杂八/博客搭建/index.html","6ea9a413c798270b82e7a2bcfd817f11"],["/categories/编程环境/index.html","456194d64e8f38003a10486fb4c435a7"],["/categories/英语学习/index.html","f95b6f8c911b67302fc75c02137857b0"],["/categories/英语学习/英语语法/index.html","807b04535369e226cfb73694210f2465"],["/comments/index.html","8edd1b3fce94f8f2bebef4557d76f47b"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b1f56ed52cd72f10569dd9acc6cefe86"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","d1cc5a3b2ad1cac610cd6918bc61dfac"],["/movies/index.html","9ab090338243fa7947c17e53e38d1421"],["/music/index.html","efb060106ce83d1131cd4b02800bdf59"],["/page/2/index.html","19abf394f6a94c75efb88564040f2d9e"],["/page/3/index.html","63085348e831a359bca29642b1c2b99a"],["/page/4/index.html","ba887800d264b87e8fc4773e3529baae"],["/page/5/index.html","66c6f63171d016247da97e5011bb772b"],["/posts/1021360842.html","b2076b638294e84e4f2cacfd71105b00"],["/posts/1120620192.html","773145058f04b86a1be5a1175ff3e61c"],["/posts/1141628095.html","776594cf3523d9f83471ab3a5b35aa90"],["/posts/1168613674.html","1e5723055420e9f7ae602621beff5775"],["/posts/1219920510.html","d96d67f9006870dab57983971e485fa1"],["/posts/1222166338.html","128a74a5b550df4f1f114eb6171f82d9"],["/posts/1259097482.html","f3359ca0b92d20727b57d96df80f058c"],["/posts/1271036369.html","8bcd981b816816f9dbb19d77b6fba2e5"],["/posts/135355774.html","685fa10ac2326d82453e31476a562d32"],["/posts/1375344716.html","9389d0ca06a5fc7950b9d1e67e17624e"],["/posts/1388991698.html","e42175c1c416f28ba757aeccfddc6918"],["/posts/1410315814.html","a3404228c88a003b165b74a1fdb72dad"],["/posts/1452790229.html","1677a632accf3ac909806d1c6a1baa8d"],["/posts/1470079884.html","ad7b83bf645099a2c32eed8b08ba0a79"],["/posts/1470079885.html","343c16c9177f35de88563ef50afc59ca"],["/posts/1470079886.html","fb698d698b352d48899bf25036159c2d"],["/posts/1470079887.html","d1b24d23a226dc03d2ce04dbed843d14"],["/posts/1498536549.html","47930e01e7ffa09a1c378e319355a801"],["/posts/1557866301.html","51ec4d47e9a52f7c1d6b691ccb7e1c21"],["/posts/1571776361.html","f2083796e3cd309d19b899f97b6d9e91"],["/posts/1605124548.html","1deea9f369386c75cb24c9a672fdf807"],["/posts/1633036852.html","d4dbbc3451755eaf528197bbab43d037"],["/posts/1765123828.html","26fc343b1ffb3d24ab411c4696fa385d"],["/posts/1817748743.html","d98e86a6a3a1fd493bf854ac1ea20047"],["/posts/1925125395.html","0cf0d68580966a8a22e4fa3fdd5839f2"],["/posts/1966191251.html","83180275940eeb40b7a02dd96bf146ad"],["/posts/1987617322.html","2a36f7e5515fcc4e9ef0bbc2fa99adee"],["/posts/2075104059.html","733a0fb7d20b1e197b1e608ea9d93b1a"],["/posts/2087796737.html","31712789075d29988912f077eb4942aa"],["/posts/2207806286.html","2b9e1547d14bb8d85ab8c6fc550ab73d"],["/posts/2225903441.html","4ed353c7f0f3ffc3ef758a318cc39b63"],["/posts/2281352001.html","7d753d7b228227c3a75a8808a6292560"],["/posts/2364755265.html","ad2880f052f0e0633d62c350f5bc3a6c"],["/posts/2414116852.html","e5aa6d89452de6deda289667807c03e2"],["/posts/2495386210.html","e2cb6a8abb509fa8b8c336190c9181c2"],["/posts/2516528882.html","b0169561e69079df463b76de891ee3ee"],["/posts/2529807823.html","5150156886eecdfe0ba2ba440f4df934"],["/posts/2888309600.html","d6be6fe51bb2f007f49b4c6fda2d58c4"],["/posts/2891591958.html","448e5ce3a54a9a21e0e51ff598c61b49"],["/posts/2909934084.html","4791d350390432941065f7f04f7055e6"],["/posts/3005926051.html","4153d70b738b412ad0641919583e6d2d"],["/posts/3169224211.html","4630bb91ca350a5bf46cc48f8fbc3597"],["/posts/3259212833.html","f55bd1d6f61c015bb098586cff8c13ac"],["/posts/3266130344.html","3029b00d17def00bd0ec77ad097700e4"],["/posts/3306641566.html","5b4e38561c26118a603169d34301c287"],["/posts/3312011324.html","831468bfd388a38ae8f32b9838643c69"],["/posts/336911618.html","68262b9ea13b92df2e01e0f5b406e9c5"],["/posts/3402121571.html","42d2757c8a59d54e5daaedf332cc9825"],["/posts/3405577485.html","2c583c77aaf2ea1f1ce523479108d76c"],["/posts/3513711414.html","a0e798d11074247d331ebd3ff61adc24"],["/posts/3546711884.html","20c2b6ee27a300f2da161aa917e2149c"],["/posts/3731385230.html","0067f91843cb8f35ea9a8840f38a6320"],["/posts/3772089482.html","dd82974c96121e2bd71fa3ed7d13f948"],["/posts/4115971639.html","1ce9f263156de97e94b5cd50388c3c3a"],["/posts/4130790367.html","bc9fe5b1d969bd04f960d9e84618ef71"],["/posts/4131986683.html","b05881fc9845bee4118f951d8b96e27c"],["/posts/4177218757.html","0375851fa52a2e344451b4e38021fe39"],["/posts/4192183953.html","ec7377b24c01e8a7c3c76c0845df2e0b"],["/posts/4261103898.html","fde7f6175963166660f690862d0608a6"],["/posts/482495853.html","89c874d4d816a1a0abb0933cce525caf"],["/posts/488247922.html","8fa3512fe1c90893fff27ed4bd237037"],["/posts/570165348.html","ba501e91d8bbe631d309cd5110a8d80b"],["/posts/595890772.html","26129421f41c94096ade7f26e975ad84"],["/posts/694347442.html","54b1437b3cc7f3ddf143098d9256d9d0"],["/posts/707384687.html","607224c413ffb4971ecadc405ffa3028"],["/posts/71180092.html","8ac3700d869b5b5aa308fdcedf5d67a1"],["/posts/716459272.html","508b915afeb1dd1a696efffd10668c26"],["/posts/795397410.html","7bf3964296f4131a49e7ac925be0a9e9"],["/posts/820223701.html","2da7ecf7242a57ebdd4242e590169188"],["/posts/830372185.html","3e568403f6981d55c4959d768c0bc0ae"],["/posts/88294277.html","c6576f873ae1e6da87ed34a828df7431"],["/posts/939963535.html","c3eb6bb87a26292a47bf72e6a906478a"],["/posts/983786067.html","dd6fdc8582500a4ec4b1d3f1c74722fc"],["/sw-register.js","74a64e1fe850b36a85ebf465fcc576f1"],["/tags/C/index.html","029cea7617800b260082207244c4c241"],["/tags/C/page/2/index.html","67dee669ebc306c2c3f24e7132b8c68f"],["/tags/GUI/index.html","1b7731ebdba498f0c61fa4435ea861f8"],["/tags/HBase/index.html","3d708490a61d8e46e06d8e13720c3ba7"],["/tags/Hadoop/index.html","255a5e626d5f4f8d33d5541847220d07"],["/tags/Java/index.html","fa40ebeefe50d640ade1c3649d5c187b"],["/tags/Java后端/index.html","0c585e95b32bd45dfac92ab96636b4c0"],["/tags/Java基础/index.html","22ccc59d7048cfaf2cba9f840071d07b"],["/tags/Java基础/page/2/index.html","1d626707ae8b7e0cb215d052b6f0711d"],["/tags/Linux/index.html","bd46c0bdf8f81f64490438e9eb373471"],["/tags/Linux/page/2/index.html","5d2d26b4f205683c2b6c96bb513bf208"],["/tags/Mac/index.html","34b1612214f7b059db6a32b03624205b"],["/tags/Mac/page/2/index.html","183a880f52fff7cb38373f4c26cf97e2"],["/tags/Maven/index.html","cb6efc5adc2f3146c8221b0a88ced87f"],["/tags/MySQL/index.html","717d025b131b6dcfb8686cc75692289a"],["/tags/Python/index.html","600000b6c19bbb5f88bc0e8895d1bb76"],["/tags/R语言/index.html","5534d8b1052112cfbb5e7dbbfd680151"],["/tags/Ubuntu/index.html","bc45cd21aef10f8335511dbb6f140ab6"],["/tags/Windows/index.html","c980982e40e7f595097f53c9ee5ba692"],["/tags/ZooKeeper/index.html","184c4133d147fd41e138634c0bfaf898"],["/tags/bfs/index.html","57d8cdf4dc8a30d7518e362bd308d3d2"],["/tags/dfs/index.html","6d822c1b91c7da3729519c2335a66d04"],["/tags/folium/index.html","3773ae3a0342f3c6453f0f8e04c63b06"],["/tags/git/index.html","41d01c257627697edd333c0d989b028e"],["/tags/index.html","0817c239197ee8a4b19d91209ed40ce0"],["/tags/latex/index.html","6e568b9c38bffe07e76c28c8a06f142b"],["/tags/二分查找/index.html","4d1d335ba2b1f528775049f66b0e17fa"],["/tags/优化类/index.html","d2a92d52df09c97ccca87458ff8edd53"],["/tags/前缀和与差分/index.html","be7bdb1a50308761f7fb5ceb1e3b4770"],["/tags/动态规划/index.html","6d106fedb7caeefc37347668ae4db834"],["/tags/博客搭建/index.html","84670221e063210ff0bd74cef9000380"],["/tags/图论/index.html","0332c552b8bb6aed44527867affdc782"],["/tags/大数据/index.html","43160fd5d0dc24ddf9fa5bf55096da30"],["/tags/操作系统/index.html","2c98518da2afcf2f62b33315f4f547f3"],["/tags/数学建模/index.html","1fbdc736ead786aa0642cb6fa41a57c0"],["/tags/数据库/index.html","b708946a4d5835588cedc5afdd5109fc"],["/tags/数据结构和算法/index.html","3de51e57cf9c1af6bf76978d8e86375a"],["/tags/数据结构和算法/page/2/index.html","f086cbbe47844036ec102f1f5169ae76"],["/tags/数组和字符串/index.html","1f8e06b247e7981b2077c0526e8e0aa0"],["/tags/枚举类/index.html","75bc7eea5b0f8fc7c15dad3530c2ecab"],["/tags/栈和队列/index.html","3defdd2ca97e2c8f53ad15b2dcb7298c"],["/tags/树论/index.html","a18e2933af9e934cbc21a6b8ca36a3a7"],["/tags/测试/index.html","66b445e0f364cd43564186dc355f48fd"],["/tags/环境/index.html","3d3b0af5e496a84898dd223e9cc95b75"],["/tags/环境变量/index.html","829f047d679514211f211ee39cf497a5"],["/tags/绘图/index.html","55a657a50b4b2cd8b0ea20aa566a347b"],["/tags/编程环境/index.html","98ae10f040189e7f8d1ff23c25eea878"],["/tags/网络编程/index.html","694a296d8b3645e3896fe3aea822fba8"],["/tags/英语语法/index.html","a243e5bfe471b02e5c327f8afd20de9e"],["/tags/论文/index.html","c1e2de8be540a7544c41ca095a4e0e4c"],["/tags/资源下载/index.html","24886b5382329f524d636336509ef178"],["/tags/链表/index.html","ffe18e6820ba59e8115066890e3beb9b"],["/tags/集合/index.html","e9eaff1721121de7c81bf083c6110352"],["/tags/集群/index.html","eb7b4263d48a72a2e2bfe97a88d01ba3"]];
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
