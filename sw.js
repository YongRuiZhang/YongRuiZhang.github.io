/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","fa1e2b00c1eebb2f8cd1fd53921d0db4"],["/about/index.html","b8704d72178839e3a1a90aa742ab2926"],["/archives/2023/01/index.html","2ccf0b252b46e268ef8d3ccee8d5d7bf"],["/archives/2023/02/index.html","6132fca8157ea7eaf18b5f279e9eb6c4"],["/archives/2023/02/page/2/index.html","814536909433dae59e218301125c6596"],["/archives/2023/03/index.html","75125f8c147f2c7b414c4dbca533731b"],["/archives/2023/index.html","3d132b3ff8e8386fa44b6b947e978762"],["/archives/2023/page/2/index.html","3137042ce814600acbced32f874d8ed6"],["/archives/2023/page/3/index.html","e542135489c42244d4e60eb58631abd4"],["/archives/index.html","a4d4f70d395aceebc21d738c5239e772"],["/archives/page/2/index.html","b122547d3ae00270002c131847f86ca7"],["/archives/page/3/index.html","a2f976875a46ce5c050611a566dc7222"],["/categories/Java/index.html","eb17077ba0f03ef71fe5b0005b5a81b5"],["/categories/Java/后端/index.html","c4da8eedcc8bca29b658e76f9be93821"],["/categories/Java/基础/index.html","8238a1e4ce59c9632166988c9abb8538"],["/categories/Java/基础/集合/index.html","91539d92d67a71555ddbb840bc8e8c86"],["/categories/Python/index.html","63e5a349271e8232260be4815f9ab0d7"],["/categories/Python/编程环境/index.html","f43c073f400a46f7908289bb4394cd68"],["/categories/R语言/index.html","20fe767b660960259b6628464eecdae0"],["/categories/R语言/编程环境/index.html","21a8061c6cb31550f57e00dfa002ea60"],["/categories/index.html","12c9efd42dcb0d2947f7ef82cb083112"],["/categories/大数据开发/HBase/index.html","1c88fe1b6380ee5363c87f6ef164ff11"],["/categories/大数据开发/HBase/环境搭建/index.html","8c5ee2d30d5bfecfedf1e4296e1fc16f"],["/categories/大数据开发/Hadoop/index.html","aa6a044b3e1fe836b17ef9084fbd3d0b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5c4e3ef7609e8df281a57a529b4f0f68"],["/categories/大数据开发/Zookeeper/index.html","37a97109fde922d69925916a5e8a28e8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","878e5e6b0dfc9893597d78ad381adb39"],["/categories/大数据开发/index.html","4ab84fde99fb406e6d958e9532cfa48c"],["/categories/操作系统/Linux/index.html","a048d35999e801bc623b2ac3da0fc67f"],["/categories/操作系统/Mac/index.html","77445efc12281e3b628d289ab9e66f46"],["/categories/操作系统/Windows/index.html","5cd14cf8504851b91ebb7920c1e7d896"],["/categories/操作系统/index.html","86fc32714f1fc0855fce7c1c351b5435"],["/categories/数学建模/index.html","8aec2223475489a01aee913fea64f70b"],["/categories/数学建模/latex/index.html","21d93701df95900ada719e875caddae2"],["/categories/数学建模/优化类/index.html","894f15325d4d47ecd8c298ca9e178604"],["/categories/数学建模/优化类/现代优化算法/index.html","0af83d59b521ea04c13c1032956e39fa"],["/categories/数学建模/优化类/规划类/index.html","1a24cdd36876b60de53370fc2f23028c"],["/categories/数学建模/绘图/index.html","10b5162e785d2dc6e41e3e85494ea857"],["/categories/数据库/MySQL/index.html","fbace46576b790f33d7da8f0f442e55c"],["/categories/数据库/index.html","9d0671a4acb2bf8f109ec0595a498ad1"],["/categories/数据结构和算法/index.html","35d3b26263f86b08eb4302520d8c1e51"],["/categories/数据结构和算法/page/2/index.html","ce8bf5658f4dc8e4a6438bc03c6e3d0e"],["/categories/数据结构和算法/基本原理/bfs/index.html","9974db4e1d64cdb48ae7503e5c84aad7"],["/categories/数据结构和算法/基本原理/dfs/index.html","9e9408454bc1da8a387b172aef4535aa"],["/categories/数据结构和算法/基本原理/index.html","9fe73025ae45d3295bda989094d6b673"],["/categories/数据结构和算法/基本原理/动态规划/index.html","86b8f2307b28a2ab666d092e6d9dd783"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","29f960bf038cae19442e49ef332196e9"],["/categories/数据结构和算法/基本原理/图论/index.html","e58d198af40aad5e59228d5e590f3071"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","da9568f39f2814128f97393029396093"],["/categories/数据结构和算法/基本原理/数论/index.html","e56de34be816defee9f0e0c831ef2003"],["/categories/数据结构和算法/基本原理/树论/index.html","5efd37929af4d7dbc16a8296e854d213"],["/categories/数据结构和算法/基本原理/链表/index.html","448db1f304d189455c3121ec59551e8a"],["/categories/数据结构和算法/算法题/index.html","8e20d4638c0efece8315b9f92f39d004"],["/categories/数据结构和算法/算法题/二分查找/index.html","596a75ae859c07b53f8421ed8eca8f8f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f2a268480051837a7afd8e84f6ceaae5"],["/categories/数据结构和算法/算法题/动态规划/index.html","0fdb462baf9db53205d50b95de3b29f2"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a3b3c01da7397711b59635be864cda8f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","715f5c74c4e6e2501afd897eb635c707"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","350ef7af526c8e514d87ca29f140c1cb"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","5c9728bea310b40475122b8085e942ae"],["/categories/数据结构和算法/算法题/栈和队列/index.html","6ea0598347ec32e1e21831d2546554d0"],["/categories/数据结构和算法/算法题/树论/index.html","0ead1ab878c1faba084ccae39d468119"],["/categories/杂七杂八/index.html","99ac0d5f7f02ce2fc7b282efb244fd7c"],["/categories/杂七杂八/博客搭建/index.html","e29bb96e6a1e9de98dec7cad447a6ece"],["/categories/编程环境/index.html","caa1809b49f023cfb18035de837de89b"],["/categories/英语学习/index.html","988307801826ddb14e54034b17e4b743"],["/categories/英语学习/英语语法/index.html","b20a7633eb0d67c752c7304c7ce32068"],["/comments/index.html","7b37739ae16afcd0b31e5dbeacf45b9c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","1d38cc21b11005ea15136ab34f654847"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","61fe588d9292c3c675a07ba67fa48d32"],["/movies/index.html","2a5800d47835c49ee033be1bfeb1462e"],["/music/index.html","ce8de6202853aada079cd55f9116030f"],["/page/2/index.html","aac00ef45eb27f8db8050750296c8bab"],["/page/3/index.html","cf0df0c0eb227d775f24efc456c02288"],["/page/4/index.html","5002b9ee98bdc20387871b4f0fbfacdc"],["/page/5/index.html","f30e451cc25324241e15a35c9bf95385"],["/posts/1021360842.html","02730d5e4d14e8635065accfeca11543"],["/posts/1120620192.html","d91c5c0d5df47c32a815e252033828f6"],["/posts/1141628095.html","a9839e4492c183cee00fbd5352748f47"],["/posts/1168613674.html","0f801f10e8ea28a50bc96c3948be171e"],["/posts/1219920510.html","27636c31d353353d9037b4a840b92b3f"],["/posts/1222166338.html","5325d8e540d26770aff2ca9ac6c12040"],["/posts/1259097482.html","d6ea9208bddfeaae7d346d371ea7b28c"],["/posts/1271036369.html","c581ea6c470628c6dfdf98f44fde715b"],["/posts/135355774.html","3b7250cd0c38ed5295189284e9eea5fc"],["/posts/1375344716.html","2d87cdbdc0f7bb758f7ce40b18d8a8f5"],["/posts/1388991698.html","f25ea3a3b170b50203bc773c78710fae"],["/posts/1410315814.html","de611e2359ca6a1a3c58febd6f3b58a9"],["/posts/1452790229.html","3eadff50abb759491b4ee7bd8ce04202"],["/posts/1470079884.html","477c4f3838dd81dba0f1584065692d6d"],["/posts/1470079885.html","11b48976955d22f5df327de04344785c"],["/posts/1470079886.html","7d852fa1cffdd3d3a9b2028acaced4aa"],["/posts/1470079887.html","cbb39107225f16ad081c24dd3b117ec7"],["/posts/1498536549.html","18a5822e5f2c5b82798d9c2dc8734665"],["/posts/1557866301.html","73fc20fe1692b107412d9a3e0f9a23a5"],["/posts/1571776361.html","9a5fe413b97ce2c3f5ea92d9f10a021c"],["/posts/1605124548.html","eab687965ed0f6f73a953a57f3dbbd3d"],["/posts/1633036852.html","b32bafe2a1ac54057b436901ee05b1c2"],["/posts/1765123828.html","11ca8686c828c221eaf04299e423ee55"],["/posts/1776114197.html","fc140ec34f1c9eceff624cbf4d30b2b4"],["/posts/1817748743.html","c81ef4241e5c2c8d9f6dd8018b2ca742"],["/posts/1925125395.html","b053f499361581c55e33300765fbb8c3"],["/posts/1966191251.html","829b742b45dbfe15512de0967a33bebe"],["/posts/1987617322.html","596068061703e226c2f109c6d2783b39"],["/posts/1999788039.html","13eabe9db56f8ceeaeb15c2f8aaa0cc7"],["/posts/2075104059.html","506db839099b3eeb99dd0846d4bc5203"],["/posts/2087796737.html","d636ee962ef2b9ae637afc5a43813251"],["/posts/2207806286.html","021645fe644963e9321178db6acf5bf4"],["/posts/2225903441.html","3a79ed82c383db26ac88757f5b6fa05e"],["/posts/2265610284.html","6538d6bde56e561105b1e6512ffbc264"],["/posts/2281352001.html","11843766206499e90a9cf00ac54c0199"],["/posts/2364755265.html","c6b0dfbd6aeb71e639f2dcfbffdc0dc8"],["/posts/2414116852.html","33c9b703890d241cce1a2b69482ef671"],["/posts/2482902029.html","83788c761700fcc3491db67b62735fa7"],["/posts/2495386210.html","fd5ef986aede7058573f1a50890a073e"],["/posts/2516528882.html","c07ce8f35a8ed9bad26a3307a80909ab"],["/posts/2526659543.html","ae57b18c4d3303d427d82d10f350a947"],["/posts/2529807823.html","033dad837972d82cc456f6ada00772bd"],["/posts/2742438348.html","6ee436dda752fff270887ebcd8fd30af"],["/posts/2888309600.html","c7fd07b45d317519c3fdaf57a92c188b"],["/posts/2891591958.html","3332de4bed30a05d174a627c6bd66673"],["/posts/2909934084.html","bcd05960e6184ef7c029cfbd86cdad56"],["/posts/3005926051.html","dd5a4b5069330e28b8431a72d38782c3"],["/posts/3169224211.html","e30bad9e27ef9c0541c1ae20f2fe3a97"],["/posts/3259212833.html","bd4403f560797d4c1ae120fdd23b42a1"],["/posts/3266130344.html","2c22b38ba295f4be4d0f6e751adedd1e"],["/posts/3306641566.html","3d8091a6cbb92bff355466c1a459cdbe"],["/posts/3312011324.html","77385d73da8a82ca62c839af37def5ea"],["/posts/336911618.html","544187de36ef75198939faf76dc57c95"],["/posts/3402121571.html","807dcfd7d3d8186a13046ce0fbb2d774"],["/posts/3405577485.html","4448f8da94f7504557b2709e10bcbbd0"],["/posts/3498516849.html","204d955e66718a3d6a4d02560535edce"],["/posts/3513711414.html","3fd957c8a5296627426ec2067bd5a1eb"],["/posts/3546711884.html","518c98f8c162baf0ca9b8b7b61c81dc5"],["/posts/3731385230.html","90ce8cda4a00681b5dce9efc976027dc"],["/posts/3772089482.html","7087b7d45aca9319d564073560471023"],["/posts/4044235327.html","298212fa96d5c3c4b00b8ce0f1998a2a"],["/posts/4115971639.html","db9de65c74e11c6c9e5a6dafdf1aa902"],["/posts/4130790367.html","c8cf2495b6c4e426624e4dec510ad23d"],["/posts/4131986683.html","e7d7e04a94f39d3d6f1aa5a7242e7e04"],["/posts/4177218757.html","9af805214cf9d9f0916a60bcf76e08b2"],["/posts/4192183953.html","73f3d1ef2c5394f603ed9730957630e9"],["/posts/4261103898.html","ce237e0492dc238ccf31a44ffbda6f0b"],["/posts/482495853.html","25526de42de983184c8c23f5760ab496"],["/posts/488247922.html","bab7817f9662d41c3d5f1ef3445d1cfd"],["/posts/570165348.html","5d5f685d4946390f14abe7c4cf470b53"],["/posts/595890772.html","7ee931778f21c5464513cd05bea8831a"],["/posts/694347442.html","cd45aed6bedd83145142e612e4484f10"],["/posts/707384687.html","6f8a1ca0b24bd41f1e9f7ef096ae8d0a"],["/posts/71180092.html","330f9c2bfe0107477544427974ecab1f"],["/posts/716459272.html","ee26637e06d3003fdf4761d0668792d9"],["/posts/795397410.html","b35d8b267cfde0d66cb5c0f3e624e438"],["/posts/820223701.html","7ef6f8f4a2da0096012e7e9eee848af0"],["/posts/830372185.html","8a6e49379992a870ecf49aa8d95fbadc"],["/posts/88294277.html","8196f66c78722d7f3c920dba666dc087"],["/posts/939963535.html","db0d21a0f7147ed9d31c20f245e62ecc"],["/posts/983786067.html","ea6ac9fc602c393ad47d6bd7557ebe3e"],["/sw-register.js","a25acf42e12f4dc2ae551879ce36cc27"],["/tags/C/index.html","38b8fe268cc49e7161038aac9624d145"],["/tags/C/page/2/index.html","b1c2a596182bd6aef7a63fa739b11edb"],["/tags/C/page/3/index.html","d99fa1066a98413fd9113cedad21eabc"],["/tags/GUI/index.html","bd32a6144f1db88b70e9fc0030c1673b"],["/tags/HBase/index.html","0d0f3c205e279f36dfb1f1e798c8bbbd"],["/tags/Hadoop/index.html","3becd2f656b6be89fef0e1c0072c8c6a"],["/tags/Java/index.html","1cb47301aa56e65d263760c1092b74d4"],["/tags/Java后端/index.html","c8bcb330cac5bfe798584a622b52d800"],["/tags/Java基础/index.html","27c796894640488f4e08effa69169a97"],["/tags/Java基础/page/2/index.html","ec307969f937c466bf4201518bc4b473"],["/tags/Linux/index.html","b982ba5bfc501278c01422dd1bc044d8"],["/tags/Linux/page/2/index.html","606b8d165dfb1f3a477e257e4547097c"],["/tags/Mac/index.html","52d33c090707c594e94410c813588065"],["/tags/Mac/page/2/index.html","6403c6c4279ecfbbd8c16b838a38c3b5"],["/tags/Maven/index.html","c4d254543b0fbc6cad6c3bc93a6d1462"],["/tags/MySQL/index.html","087d7ad667152390235b3e75fa94b821"],["/tags/Python/index.html","76875774cdc4b70b717b3f18041ac61b"],["/tags/R语言/index.html","74308bfc14961451228873f55d592cad"],["/tags/Ubuntu/index.html","c0bf59a1606df4a24534882bd5a3298c"],["/tags/Windows/index.html","eadbbc099b52c18e642ad72dbc05f736"],["/tags/ZooKeeper/index.html","19d17580957decad0d2e7bdcea8b34c8"],["/tags/bfs/index.html","6f83cfad197ad17d06f8c03aa22d2d9b"],["/tags/dfs/index.html","3bd33595152a4a1a8d7b853e1df19ae1"],["/tags/folium/index.html","60ad39c4278298a84b3fb87f2b8d04ab"],["/tags/git/index.html","44466947e9b48c06baf1636388d60a35"],["/tags/index.html","202ba682cd55c9c3a0b28cd01b78a2b8"],["/tags/latex/index.html","b5697445d407e03d1d55b625243a485a"],["/tags/二分查找/index.html","b36d9535f23d36811e1c880875e4cc11"],["/tags/优化类/index.html","f02f4b10d41a68121a58d3850b33f259"],["/tags/前缀和与差分/index.html","7a94e6d58e0235f558ed023a63eb5948"],["/tags/动态规划/index.html","d18fef7690110d0369e9a012b5eef386"],["/tags/动态规划/page/2/index.html","ec93d7a9daea23fa95a8e0f233a4d15d"],["/tags/博客搭建/index.html","27176fdd74077a8467529c1e3ff5f73a"],["/tags/图论/index.html","aab32389c4e6bde85b78ea4c064ce014"],["/tags/大数据/index.html","24ba0a376a3a992621bacb4effaf54e7"],["/tags/操作系统/index.html","9a256a4f4565e1b1bcdc0f9acb143697"],["/tags/数学建模/index.html","e8e29353cb54b2122a9f15370fed7181"],["/tags/数据库/index.html","00eb53328803de631db9260e37cecc90"],["/tags/数据结构和算法/index.html","85274ef7f5ae0b3b0c1dea5a143031ea"],["/tags/数据结构和算法/page/2/index.html","afb6b2c6309b00e28b8d57436aeec426"],["/tags/数据结构和算法/page/3/index.html","f4229fc109ba77edc72b3a76412babcb"],["/tags/数组和字符串/index.html","033281d74309bb1821bc88c55a232fee"],["/tags/枚举类/index.html","a1c8f4a79b490fbce5431c9f7a2e8f24"],["/tags/栈和队列/index.html","9245b351b77c69ac0a6e707ecc8be2bb"],["/tags/树论/index.html","d6189c6136994ef7a9a8c3ced0f01241"],["/tags/测试/index.html","3153fb3b4fd7937dca68093741ed6c48"],["/tags/环境/index.html","9569fe86338c00ecc582e19ef4b88f35"],["/tags/环境变量/index.html","64ad28109868251bad70dde783e3947c"],["/tags/绘图/index.html","7430932cb2c21702c01cf2a95eb691f6"],["/tags/编程环境/index.html","efdc0057b3e1a5d837d095db262d9fd9"],["/tags/网络编程/index.html","9f81ad79f9171f9dac02d4b2e9de2b0a"],["/tags/英语语法/index.html","6c52517791a85dfb46e08e99a1772cb9"],["/tags/论文/index.html","c8a655fddf752c0534cfdd8fe78d0f0e"],["/tags/资源下载/index.html","1c7e18bdad9b59cea50519de08378686"],["/tags/链表/index.html","9557eaca6147012809e8f54d740aff89"],["/tags/集合/index.html","df38a27a745a284ac735d1d4114eb6ec"],["/tags/集群/index.html","abbbec8ef995a8e39eef5007efb167db"]];
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
