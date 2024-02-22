/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","533687731de47021aaac30f5b462176d"],["/about/index.html","f352247ce6f53b664ff03dad5f4f3117"],["/archives/2023/01/index.html","0c8f5bf22c7e5737aaf5f7788c4b7423"],["/archives/2023/02/index.html","8d60c8dc97139b20f9b02343758d5f00"],["/archives/2023/02/page/2/index.html","be64ce5fea2471329415b46e79fc10ba"],["/archives/2023/03/index.html","7eb35cac677a75c89e192a4a9afc9fc1"],["/archives/2023/05/index.html","0ddcb96d64b2a66bd65b255215d6269d"],["/archives/2023/06/index.html","7a81348abbe348f7a05e58550132c5a0"],["/archives/2023/09/index.html","3fbde21e30b06dcecd5f21e56bb4df83"],["/archives/2023/11/index.html","5ea8f42060c98fae8f8c88c0ef38a113"],["/archives/2023/12/index.html","741a90dc9ab7a47908fc871e5c665214"],["/archives/2023/index.html","6b761d69c8c789450da77299a3542394"],["/archives/2023/page/2/index.html","f3b06c1e81f4162e7a7c7e05fafc252e"],["/archives/2023/page/3/index.html","1f7c8e80af7166f6a80ada1136a93d4d"],["/archives/2023/page/4/index.html","6d784d72a2001e69cb953e80d049fa39"],["/archives/2024/02/index.html","22db52ee547602f388c240328cc16db3"],["/archives/2024/index.html","b96f10d4e84d91fcd0b996af2425ac31"],["/archives/index.html","9f44d7522d10d96022e23ce9b03a4a85"],["/archives/page/2/index.html","6774ad8c8b80247d616cdf1af76e7cb8"],["/archives/page/3/index.html","8e21960deaacd3c6f94e973a15fd27c6"],["/archives/page/4/index.html","ec89aaf1403a219dc5ded3ecee7883be"],["/baidu_verify_codeva-qQP2iZOMLX.html","fe41149cfd3b241c9bfb5a91659afc5f"],["/categories/Java/index.html","b59b7aeea75b309ea6aa2295b4a09746"],["/categories/Java/后端/index.html","2875c696c0daf21740e69bf7e0e8c45e"],["/categories/Java/基础/index.html","fe49ac44a72497e198e5b0dc3e3a86e8"],["/categories/Java/基础/集合/index.html","02cca38b3eb7ed292762404b5929b0b3"],["/categories/Python/index.html","d1a2aff275deb440dbad9963f788865c"],["/categories/Python/编程环境/index.html","f8ec4ee0005c6665aa2187aea438dbef"],["/categories/R语言/index.html","292efb635ec6d9ce626413e3bd87d706"],["/categories/R语言/编程环境/index.html","ddba77b34d89cc8ca6a83cc29d496a8d"],["/categories/index.html","9ae045b99aa872c79ddcdbf94abb9567"],["/categories/中间件/index.html","0fe34102809d560f51a84ec28e961f57"],["/categories/前端/Vue/index.html","876352c291eb6834d3a8c6429f4b8daa"],["/categories/前端/index.html","1dcd2732a127dd1bca28c081d80befdb"],["/categories/大数据开发/ElasticSearch/index.html","6449a87e7e97e48f07f9155a0e449953"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c210b3f1196d189df915bcdd4e3fa710"],["/categories/大数据开发/HBase/index.html","38c9c9fc49dd879113a57d742a098f68"],["/categories/大数据开发/HBase/学习笔记/index.html","4a4fcba49e947a1ed7fe5806d036a3a3"],["/categories/大数据开发/HBase/环境搭建/index.html","1b48c325c98569b3743f7a7a7dd6d2db"],["/categories/大数据开发/Hadoop/index.html","009d4fb45e147e19b1c01bb1053d61c3"],["/categories/大数据开发/Hadoop/技术/index.html","befbf415aef126b110db8c4d34304ebc"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e4d33c1e16ee8187f94fd32030599a62"],["/categories/大数据开发/Redis/index.html","6a74c0d1fff7347f8ea430fc5f7e901d"],["/categories/大数据开发/Redis/技术/index.html","fcb688c5d116d1a874b0ffdd0f70871e"],["/categories/大数据开发/Redis/环境搭建/index.html","b64e71516e898848f03500693f26a855"],["/categories/大数据开发/Spark/index.html","717aaf83e5105c3aa0fade6a972ad323"],["/categories/大数据开发/Spark/环境搭建/index.html","bff4cc8f7c14cf46e6355d59d63021b6"],["/categories/大数据开发/Zookeeper/index.html","0d429e1703284cdec43988619288a479"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","fa8ae489fd6f73435920708d2f65a8e2"],["/categories/大数据开发/index.html","4d67966ccbe55f710467a818507d1262"],["/categories/学校课程/index.html","5763b7f47c37141ee99bcf01a5ab82fa"],["/categories/学校课程/计算机操作系统/index.html","7f89ef9990be82c072f0fa37129186b8"],["/categories/操作系统/Linux/index.html","45a4bc1b0dfb0b593d41a6a6a7f987f3"],["/categories/操作系统/Mac/index.html","4b9554a285a5e56d6ab18a3635890dc3"],["/categories/操作系统/Windows/index.html","fd29924076438fe3521b211d6ceaa04f"],["/categories/操作系统/index.html","d10abff5286189f742f544b3462633fe"],["/categories/数学建模/index.html","94f2d4fd98749390841bc7cc3d943086"],["/categories/数学建模/latex/index.html","f6f69c1ba693a9a669e5956edbef0ef1"],["/categories/数学建模/优化类/index.html","a350a02fba159e3d79f9fd96d249bf8e"],["/categories/数学建模/优化类/现代优化算法/index.html","5611ea3b23ee1bb36ab3e5746c8f1bc8"],["/categories/数学建模/优化类/规划类/index.html","e9a94e49c01f6026e2f7d1b9ee365a4f"],["/categories/数学建模/绘图/index.html","794da6ea9cff594236cb9ae8f5115a80"],["/categories/数据库/MySQL/index.html","c8dd138e39e7089228ad7883094d3578"],["/categories/数据库/index.html","45e2680a5d1cb3ec1826ac239d1f5456"],["/categories/数据结构和算法/index.html","4e26631ee5a17aaefa2ac772fac792fd"],["/categories/数据结构和算法/page/2/index.html","730cb320a6fee6142d23259d4b9fd702"],["/categories/数据结构和算法/基本原理/bfs/index.html","f2033ea32e9dd68024c455c26523e86f"],["/categories/数据结构和算法/基本原理/dfs/index.html","aec7ca730ef0554d59fd1877091b2470"],["/categories/数据结构和算法/基本原理/index.html","d759f8998e7305b59ae11df09b26bbc8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f3f57cb352a2a619fb17548e16945263"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a0951b3b6ef8aab9cd4d00287396e5cb"],["/categories/数据结构和算法/基本原理/图论/index.html","52137fd57456a6f40b3f08544dc8efaf"],["/categories/数据结构和算法/基本原理/字符串/index.html","e6394b05885952088ea50e04b611adba"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","eb3baf90e884949381b70db9077fcfa7"],["/categories/数据结构和算法/基本原理/数论/index.html","4310869e2a31de20552ccb14eefb6cf6"],["/categories/数据结构和算法/基本原理/树论/index.html","6c95ff025ca22df494899560c902dd0a"],["/categories/数据结构和算法/基本原理/链表/index.html","90b79bf2f070ed0be5cc49ced584c48c"],["/categories/数据结构和算法/算法题/index.html","192ecf74833a59a0e7ada29b8894721b"],["/categories/数据结构和算法/算法题/二分查找/index.html","8f92af86dfaeb8595ea2bb95bd9870fa"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f3769e2a842d598f96ffec7b042b2fdd"],["/categories/数据结构和算法/算法题/动态规划/index.html","e7edb4a7540937cd972ca8f8ceca0ee0"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","523dfc10f8749f544e4c7877e9790d94"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","dea5949d1592ee745ca8530ec690ee46"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","34dcc45fd1516e2636065bc8879158bd"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","1baba9e4d3965a35bc6821e8bc379614"],["/categories/数据结构和算法/算法题/数论/index.html","f6c9668a22e00373b0d44736bc9228c9"],["/categories/数据结构和算法/算法题/栈和队列/index.html","46ddf793d6e8b0a3c5fb229b01039989"],["/categories/数据结构和算法/算法题/树论/index.html","234f8e8ba91a751242474d83177fc8e4"],["/categories/杂七杂八/index.html","7f64736434c2edf70d30475c889534a5"],["/categories/杂七杂八/博客搭建/index.html","19737676829a791bca688ba2e24e62ed"],["/categories/编程工具下载/index.html","93beff9188bc8fc38ec883b4596c879d"],["/categories/编程环境/index.html","25981c23349f9992a22db323bd74809c"],["/categories/编程环境/大数据/index.html","dafd315067dad608851b94d0c9381037"],["/categories/英语学习/index.html","76295a5297b43303da1389b89b416e9d"],["/categories/英语学习/英语语法/index.html","fc3faecc160c393c535abea57814e374"],["/comments/index.html","30e7100dca6992f6e57f0807ff96dd93"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","f5c384cbe13edba697f3f8f41448df84"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cd15e37c5d738a508915769ed7ffd1c1"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","87a6f06e3f62d8748bbbf14acbaa8b2c"],["/movies/index.html","0efc07689ac2fa77601711bdac97a5a3"],["/music/index.html","a898d95fc86cfbb81af82a6b5d587ec4"],["/page/2/index.html","da1c899d7f45d94e1d049d4c8d6c71a4"],["/page/3/index.html","20029b0ebaf76442b9e79d2dd82f9b32"],["/page/4/index.html","b07746af35481c2443eda1b9f001a79a"],["/page/5/index.html","1b7273c1ee18aae8e293c98f8387b528"],["/page/6/index.html","6a22e5cec5d65b44bb65a50e3236f759"],["/posts/1021360842.html","13ec367a827108ec3864e101f9c7fc7e"],["/posts/1120620192.html","69e6e73f0bf672f031ee2014ffa2e325"],["/posts/1141628095.html","0c757df2fd475b8fa3b77c0b0575d226"],["/posts/1168613674.html","1982575eb5bdc28a7f37428c93d6b3d0"],["/posts/1219920510.html","24608d6c607a01439451ccdc0cb1aa09"],["/posts/1222166338.html","062b3b55999fec2a706da2fa0d9a471b"],["/posts/1259097482.html","d930c59559739e21a281a3e3e6decb70"],["/posts/1271036369.html","81938a5e1cedb6ac0f93b30db2f11077"],["/posts/1312847445.html","6ea496e5cd55168bf8f874ed38b09b4d"],["/posts/135355774.html","71fdaf45bf6c904e0ec9b04bf451389a"],["/posts/1375344716.html","aa22c8608c678cb14b87f80e2599ea92"],["/posts/1388991698.html","dca9df05a08b8d421deb8a01cdc923be"],["/posts/1410315814.html","056f6e67ff79787224422468aefe4b84"],["/posts/1452790229.html","4b0c95f95882aa4c63e1aaa50ace753c"],["/posts/1470079884.html","3152512f13bd7d8f174ec6bca4b9854c"],["/posts/1470079885.html","8731d5797aec8cbff0bed3182cfa4ff0"],["/posts/1470079886.html","d81c3f9af18cbbc6101e68fcd522591d"],["/posts/1470079887.html","bbe286ebeb97c72c8a9986c1cd4cf299"],["/posts/1498536549.html","1216f44519c8029152040e92dc328403"],["/posts/1539568593.html","f92d6ced21ed225945af7379c64ef5ce"],["/posts/1547067935.html","a533ef61442d9f4f1129fdca0bca4730"],["/posts/1557866301.html","db282fdcd6b042c9f2b09617bbaa1618"],["/posts/1571776361.html","c4ea8882a382921974a2b9fd0b6402e8"],["/posts/1605124548.html","ac07100e97d18b4d2c08cf22ce4464dc"],["/posts/1633036852.html","44025a64946127d7cc6125405c913c74"],["/posts/1674202625.html","bd63c3b4566d3127f64b162929e38ff4"],["/posts/1765123828.html","31a6d85c2e9852e9ef4ba0f988360540"],["/posts/1767336200.html","974407730f029fb3c4091a170245ba6c"],["/posts/1776114197.html","4d6ad064df0c8caefef1d373765091db"],["/posts/1817748743.html","4e197cc2ccadc1f29f87bbd8a34c8d90"],["/posts/1925125395.html","15a4486c37af21ec23ae76940672076b"],["/posts/1966191251.html","b34c3043226de1c2db6850159c875855"],["/posts/1987617322.html","0110cd9a8bb869610883029883ddd2ef"],["/posts/1999788039.html","41b2164d2bbbd1982760840dd6067c54"],["/posts/2075104059.html","09284fe78bbc20e07a8ff4e252151122"],["/posts/2087796737.html","8cb9a58f029da23d40cc5c3da6036c59"],["/posts/2106547339.html","ffa7a338e8bfcfc7f42d76b7f18465a6"],["/posts/2207806286.html","7f168064a289aabeb398c573783747ff"],["/posts/2225903441.html","d454ebdb7245d681580ca01da51fe41d"],["/posts/2265610284.html","90b06e4d47d949641f734a4c58aa353c"],["/posts/2281352001.html","e8631ab05c3e10f93aba845b7bf3325a"],["/posts/2364755265.html","e62c13f9ed4710697fafe4f8baf53185"],["/posts/2414116852.html","97359b59341269d48792d279f2cd8fc0"],["/posts/2421785022.html","e6cfa3aa6cb074249f09d42f945e09e1"],["/posts/2482902029.html","5946354808e5912cf75ac2f913e9e0a8"],["/posts/2495386210.html","615854d36a2e9ebbfe53a0376a3cf611"],["/posts/2516528882.html","05bcbc92d1e93a2cb29ffb11e59af5fc"],["/posts/2526659543.html","d617d075cadae7eac0d661df04f2b0cd"],["/posts/2529807823.html","d5d8b8ae3953cb8b034b7545f9376972"],["/posts/2596601004.html","062756af641f5486ee8e1adfce813f65"],["/posts/2697614349.html","0234cb4456acbb5964498eeeed41d414"],["/posts/2742438348.html","e36f66415cf671be587ebe08fa32bb04"],["/posts/2768249503.html","a1b0fe1dec24b5cbe2d4d13c57e1336d"],["/posts/2864584994.html","31b191f674e750d1a93525d41a0f5907"],["/posts/2888309600.html","9bc5112137f9a67ddb7c9b7e2576eeb3"],["/posts/2891591958.html","d2298ac355a199f51612d69d903b1114"],["/posts/2909934084.html","157415e61bfa2f54a4ee3668d8a37428"],["/posts/2920256992.html","4b323ddb52fed2cb2021fc3f0df5b690"],["/posts/2959474469.html","f01f0381b4298d5a9632f02626a2f409"],["/posts/3005926051.html","4fa4d436692c3c57ddece2c492c2cefc"],["/posts/309775400.html","d7266d1d72b139c2fff149b0bdea96b6"],["/posts/3156194925.html","008e2de8ed2e1bd9f9e5ad8cc309c023"],["/posts/3169224211.html","dd0825e3ceea9afdd74b95bf7280fe5a"],["/posts/3213899550.html","028016d4577040eebe20e11b598e7350"],["/posts/3259212833.html","7e5e3bf2c4b9195cbb0c97d5df6a871b"],["/posts/3266130344.html","75548d36ec92c5661756fdee9716b549"],["/posts/3292663995.html","268b9baedc89b2a7b626882d39a3a1f7"],["/posts/3297135020.html","ef896fd5fbf7ff188e074e65c9ef63dc"],["/posts/3306641566.html","d8e761928f2aad28e7f5c5c0e507d786"],["/posts/3312011324.html","a2642143bb77205ce4a7e6d2693d90de"],["/posts/336911618.html","5fafca8bf911770a2341000efc72b8e4"],["/posts/3402121571.html","fecd04b70ad05d61829ce285ec70810f"],["/posts/3405577485.html","d1ee9f333253fe59f31e5644380c4a84"],["/posts/3498516849.html","3890a20630f8499eb1c55b090f57874f"],["/posts/3513711414.html","109a5044c6f44e23845b94f062bc79b4"],["/posts/3523095624.html","98ca74878b800166710f87a705c7465f"],["/posts/3546711884.html","08defef761eb2abd978a8111c499b942"],["/posts/3731385230.html","7a989e366e4cff4d9bd37d6ebf1beab7"],["/posts/3772089482.html","1540fc6ba9c67390d6f6f834aa3fc211"],["/posts/386609427.html","ef30210b5d1a22c683ba77d859b2b027"],["/posts/4044235327.html","d36ebe23f10911d2e958f64b49f2873d"],["/posts/4115971639.html","fd673d7a0841f44dbd8692cc1d103ddb"],["/posts/4130790367.html","66390f95023e4ae5f7f2583034808fbc"],["/posts/4131986683.html","a4c40dfa27f295c8cd8a945f62aa189f"],["/posts/4177218757.html","17a9800f960677278cb0249aa5b9cd4c"],["/posts/4192183953.html","ad81a1a6a9d5d411e03befde822f7c5b"],["/posts/4261103898.html","364f9082275b14758513813d5347ee5c"],["/posts/469711973.html","1e48ed994c10fff051474f9543af5b56"],["/posts/482495853.html","043923391d40418dee34e117a8ac701a"],["/posts/488247922.html","acf03b082a97aa0ed1b1f47f9a8d7eb3"],["/posts/517302816.html","6bee3377562c792986ea4082c3f1c813"],["/posts/570165348.html","ee7a64d64dd5ae5d8ef2ee67d8b5c5fe"],["/posts/595890772.html","c5cf44faf9a33e22b8eb9a69836ebfb9"],["/posts/67485572.html","9e8196501fee7722ba4118abb3cd5044"],["/posts/694347442.html","2410ba71e211c3a78ca60febda7a952d"],["/posts/707384687.html","641c2050032d2953d012926157963939"],["/posts/71180092.html","49f698bcc824775dea3b696b63f0dbca"],["/posts/716459272.html","21e97e2ada6291c5ebd00bf09937d146"],["/posts/765481613.html","79e777fc12260e0c3a2148a4d8468174"],["/posts/778231993.html","6c3bf1eedfe871f7f01d23a2dac3121c"],["/posts/795397410.html","cd10ee2ac85f1da93f6073f62d5a9025"],["/posts/820223701.html","4628319862dbaef446f268be5f33d9ca"],["/posts/830372185.html","d34fcc9eaf6d34f1be90c20b5301fc01"],["/posts/88294277.html","ebac4fbe089ba1cafeedc75d9adff0ae"],["/posts/939963535.html","93259b67d469c4ed59d9ca49266e3382"],["/posts/983786067.html","2b4d10396d4f240e0bae7e21629abbeb"],["/sw-register.js","b3e85e8f0aae691a53a33a7d0a52349c"],["/tags/C/index.html","2e5178e73327b80f93728ea07bac3f09"],["/tags/C/page/2/index.html","d467c9e7e7407b0170856bb72b3d938e"],["/tags/C/page/3/index.html","12467e068d4ba81e6bb5eb0de84c2e10"],["/tags/C/page/4/index.html","59f4e86cc27c675b720523e8b256a682"],["/tags/ETL/index.html","f9536e5b8766565dde2c0000791e1b78"],["/tags/ElasticSearch/index.html","67b07a182e58fb0242c7c690e8b0e499"],["/tags/GUI/index.html","135e2c3cb3612dc65e311825c5c99d1e"],["/tags/HBase/index.html","ca48a0b228c768215ece053a205d8336"],["/tags/Hadoop/index.html","9a8f11808dcc385d81bb7a18ba0d52e7"],["/tags/Hadoop/page/2/index.html","a388ed965411dc46f79116d135f5d978"],["/tags/Java/index.html","3cbbd4d41ac3e11d70da0f371566d19f"],["/tags/Java后端/index.html","c1e06faa4d16c5691d21a8e23527f563"],["/tags/Java后端/page/2/index.html","a1b92444d2fcd936f2288106b2d4b7b2"],["/tags/Java基础/index.html","6a5a311b92aaf4b3456f6c3c88769c95"],["/tags/Java基础/page/2/index.html","9f45428538735a3b7ffba51ff85eac98"],["/tags/Kettle/index.html","7d372b3511dd3eab8be1ea9d9608b34a"],["/tags/Kibana/index.html","b9dc6152a9e72eb2aca83bdd3dd707b6"],["/tags/Linux/index.html","9122e84e831ef120aad56f5277f4666d"],["/tags/Linux/page/2/index.html","60715fc838f5b96b05169cbf1d116d42"],["/tags/Linux/page/3/index.html","3b2841fa4f3d7af134b5bdc4c1e215bb"],["/tags/Mac/index.html","69bb5bebcfd4a8a693dcb5d69f44b313"],["/tags/Mac/page/2/index.html","2b447e17d9bdf639ea513cfbfc3267d2"],["/tags/Maven/index.html","981b45f2613daefa8f7a886bebb4fa2e"],["/tags/MySQL/index.html","55dc91b24102ad943cbb3b2506bcbfbb"],["/tags/Python/index.html","e8756b455dc714f674b807ee9653f678"],["/tags/Redis/index.html","33ab94266146b2e8b42dcdd57bee3fba"],["/tags/R语言/index.html","5ac8c56315b7187807333537a7b78d80"],["/tags/Spark/index.html","71eda7bff94eafca985b640ec25e4f7d"],["/tags/Ubuntu/index.html","a753e4bcd1d61b8857e3c1b6fa40dbd3"],["/tags/Vue/index.html","2617e627cb4bf224aaaa460f511c9a81"],["/tags/Windows/index.html","17a6461aeeadf48eda47012303bda699"],["/tags/ZooKeeper/index.html","02f88c4074cab1c8793a33fb8390ed20"],["/tags/bfs/index.html","a4daaa23b1a284f8b87da482ec8ee94c"],["/tags/dfs/index.html","73cb5a15ef6f49532f04915d6350dad1"],["/tags/folium/index.html","6a463f90b6a4c96033789eb55dd94155"],["/tags/git/index.html","c62529b9404ad1de91152e5645353f82"],["/tags/index.html","ace6c707db33cd1faf5f7cc37e1c8f8b"],["/tags/latex/index.html","088dbb5f4650b3a91e43b7f37353313b"],["/tags/中间件/index.html","29c021ac3da0039de95c87e4159b572f"],["/tags/二分查找/index.html","a5ea87e3e39fc6baa6581079f5d3492f"],["/tags/优化类/index.html","42cc0fa4ebcf56709cf3d313924fbc74"],["/tags/前端/index.html","5f511f0a219326c9684452a6ceadff41"],["/tags/前缀和与差分/index.html","5f07dbc36e73f4ee38b64d7713123f1e"],["/tags/动态规划/index.html","69ccf1f5b82294d9488b3fd94f052709"],["/tags/动态规划/page/2/index.html","1a8a14637c4e2cc601f066b0197b8054"],["/tags/博客搭建/index.html","322e3c738b793130db5615bbc4529ad6"],["/tags/图论/index.html","23e087658592de36234e18c9179e3eb0"],["/tags/大数据/index.html","30f2a7d028cae756b6acba70121f6a11"],["/tags/大数据/page/2/index.html","1f72e58817df1552d5af2c63e807b0df"],["/tags/操作系统/index.html","3e813343330c0fef1817c3a1fc61497e"],["/tags/数学建模/index.html","a2887de2d30877123b7387fec21271eb"],["/tags/数据库/index.html","e3b565ed933defa18a4852144025985f"],["/tags/数据结构和算法/index.html","249c6858eb6225388b1dd5b06aedaca7"],["/tags/数据结构和算法/page/2/index.html","1ab578d519a25cc690693b5da3e5c544"],["/tags/数据结构和算法/page/3/index.html","1215015b7fe2854cfdb86f44d085a5e5"],["/tags/数据结构和算法/page/4/index.html","582f29d8f27c9b08eca8e637d74b5167"],["/tags/数组和字符串/index.html","2fbed7b5183ce51bbb998aec4148631d"],["/tags/数论/index.html","cc44a796bfb7cae0a92f95645dee6336"],["/tags/枚举类/index.html","35afd5e84a05365f889cf8548b1bc05d"],["/tags/栈和队列/index.html","c410ad72ef86d02d6f83f87652dacc21"],["/tags/树论/index.html","f9ad22d1cfe8eda4a49c35f821a018ec"],["/tags/测试/index.html","5e4bd1c46187e6da097aabb6b11ad51f"],["/tags/环境/index.html","9c90c9fdb2e20f4040634d58a58b7a94"],["/tags/环境变量/index.html","544f64bd907ff797a22c29cc41ad4ac6"],["/tags/绘图/index.html","beb7fe7b6517824aa00086721b957907"],["/tags/编程工具/index.html","53757c2f50272bb03daa0284fc149990"],["/tags/编程环境/index.html","3737ccf79c4090d9ea0ff0bdc180d8a0"],["/tags/网络编程/index.html","17bfc26db6613187e6800f47f5158792"],["/tags/英语语法/index.html","5d7a8592fb71bcd6d42078d80450cc74"],["/tags/计算机操作系统/index.html","f7b4045698e03db6ba9d1ec8dc9e038f"],["/tags/论文/index.html","c6c204daea8dec226cb60520e5b0972d"],["/tags/资源下载/index.html","7fff7c7df179bbb6a90d3c0b422aeb57"],["/tags/链表/index.html","8a6edeadc81df51b872a24f346ef73c6"],["/tags/集合/index.html","0696c71662e4af842633c667c360bc0a"],["/tags/集群/index.html","bc953e2e9be253720c1acfdfda5b8263"]];
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
