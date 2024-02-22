/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4427c22a503ed1585d7fcdeeb6429dda"],["/about/index.html","cd024c4bee5bd91d11e39abe0cfca87b"],["/archives/2023/01/index.html","23c851406e74ec601a21a26cae3eeb94"],["/archives/2023/02/index.html","00f851d3cb12fd2c2b8f336d8fa792b9"],["/archives/2023/02/page/2/index.html","e4762a86bd1d0a4360f0df363696a9e4"],["/archives/2023/03/index.html","52b2f52da623ec3b8ec9b2c1562774dc"],["/archives/2023/05/index.html","953df8e875356f729dd0309ef0bb65c8"],["/archives/2023/06/index.html","f36ce095b8049bf5e46b937a9e72837c"],["/archives/2023/09/index.html","4a9b5e8cc731545f31d0c59fc853ee5c"],["/archives/2023/11/index.html","03ec443947f716d9b173d1a7d3a62399"],["/archives/2023/12/index.html","ac9a5973966f6054cea7d470eb64db95"],["/archives/2023/index.html","8cd8cf8d9eb51f0f139161d92b8939b7"],["/archives/2023/page/2/index.html","a9f4b21409ad2c33b586bc80b0f0bd84"],["/archives/2023/page/3/index.html","77f64cc35aa79ea61227dc5356553216"],["/archives/2023/page/4/index.html","43a58eacac9a18268b9638c86706585b"],["/archives/2024/02/index.html","11e2be223242c0ab4e60e138552257cc"],["/archives/2024/index.html","18951706228ef1338be2a8be04d02d58"],["/archives/index.html","ff6ac0d23846025911fd228159a5417c"],["/archives/page/2/index.html","fa1136e1b8542dd7a95b220519931e24"],["/archives/page/3/index.html","098f4703c7ad1f3ab5bbfade6443dd24"],["/archives/page/4/index.html","47cda4289e525d873b0681c732d22c5b"],["/baidu_verify_codeva-qQP2iZOMLX.html","43909e4037969a09040a14a1c48289ab"],["/categories/Java/index.html","59a61a5088c598db1fbd0bd9f1bb0640"],["/categories/Java/后端/index.html","2d1474231c911bc7cd92281e1ba58271"],["/categories/Java/基础/index.html","137e12d54bb296f553a95fb34225d12f"],["/categories/Java/基础/集合/index.html","54230f3359bf8d6d8341a6fdc8c0c4dc"],["/categories/Python/index.html","a822173243b3b7cd634535b9e0d822a3"],["/categories/Python/编程环境/index.html","c18a17fee857d46ef5990b22c0f888da"],["/categories/R语言/index.html","f73b4a347c2c1ed3011972493f2cfea4"],["/categories/R语言/编程环境/index.html","8012fee6ec6e6d3d40e442465f61b3ab"],["/categories/index.html","662924d35b01c2c9d808ad4b4f3ed82f"],["/categories/中间件/index.html","ca692ebbd5d7d2900c797b400182548c"],["/categories/前端/Vue/index.html","bfc1c37433ae4fb254362e860e6f6bc5"],["/categories/前端/index.html","50be8c6f9d7f474cd5336a2423e779c5"],["/categories/大数据开发/ElasticSearch/index.html","ced472de007d1502a910bea351d6a12e"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","644d94cd867077d7ebb03648062582d5"],["/categories/大数据开发/HBase/index.html","2de7b544eb1829a599ff879a82e29ae8"],["/categories/大数据开发/HBase/学习笔记/index.html","e8bba27e19215b2c113773985b052d8c"],["/categories/大数据开发/HBase/环境搭建/index.html","635b9ba6db647fc8f244e42244e30880"],["/categories/大数据开发/Hadoop/index.html","360a9dc67ca69aac3703050c0e320242"],["/categories/大数据开发/Hadoop/技术/index.html","3d872f471d68b959dfbb4d878811f35d"],["/categories/大数据开发/Hadoop/环境搭建/index.html","aec65a9d0ead81113baa9a485ee34bbd"],["/categories/大数据开发/Redis/index.html","ab44318db750d0a3225a7ba5334cc5ef"],["/categories/大数据开发/Redis/技术/index.html","c2e8b440ab5543b1e72c840f65fe2366"],["/categories/大数据开发/Redis/环境搭建/index.html","c0a6f3902d2bdd25f7159712e165b6f3"],["/categories/大数据开发/Spark/index.html","be1ccfbed909b043be2a96025f420f07"],["/categories/大数据开发/Spark/环境搭建/index.html","e714ccbdcb5e8c33a09955a082776e49"],["/categories/大数据开发/Zookeeper/index.html","36f444feda7617df64b397aa9646cc99"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","04cdb8627c86b1ff85171efb0d92c4df"],["/categories/大数据开发/index.html","63b75639780fdc02ab735c452bc5311d"],["/categories/学校课程/index.html","60a880f8bce458ce1853d230beda0d22"],["/categories/学校课程/计算机操作系统/index.html","698e8ec4a228628e49606725cccff0d4"],["/categories/操作系统/Linux/index.html","7dda17b48ebe14cac0a394e7d1266786"],["/categories/操作系统/Mac/index.html","c50283e3136ec5e76e75014dddc065a9"],["/categories/操作系统/Windows/index.html","f8d52409876d136947b542b4d74c0a28"],["/categories/操作系统/index.html","ed99f089f0114487f7557e4a05949911"],["/categories/数学建模/index.html","e9ae528b56f051a0c87ad995b4c3c0ef"],["/categories/数学建模/latex/index.html","311515c0a4da4e331293d35a7a880c85"],["/categories/数学建模/优化类/index.html","d3d1f34193631c2ce0f0679e870eb871"],["/categories/数学建模/优化类/现代优化算法/index.html","ea6a4ab242de954a0914ba2f157ccbf4"],["/categories/数学建模/优化类/规划类/index.html","f8441dfc5071122ee445de4dfba2ba4a"],["/categories/数学建模/绘图/index.html","1e8926f378337b19972b1b61fd918451"],["/categories/数据库/MySQL/index.html","f1ad4b80707c8b576527b080e86c1724"],["/categories/数据库/index.html","f09b452a0a6f966afb878e89eac09d0f"],["/categories/数据结构和算法/index.html","997b09b6cbfc793bc1715a3ffeb53b9f"],["/categories/数据结构和算法/page/2/index.html","cf05cac66386b870fa187133a355c3cc"],["/categories/数据结构和算法/基本原理/bfs/index.html","85caedbee769acb24a9368acc39e27f4"],["/categories/数据结构和算法/基本原理/dfs/index.html","0b52e8bf2282875351c88d32195d26ba"],["/categories/数据结构和算法/基本原理/index.html","4c40b0b574d7f661854a9ddb5386df15"],["/categories/数据结构和算法/基本原理/动态规划/index.html","8dc8fd019c824e309b0612da602fdf40"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","bcc2076159466781043b172b32e7bcc4"],["/categories/数据结构和算法/基本原理/图论/index.html","0e810f1dc37158b4625a31af229e8e8e"],["/categories/数据结构和算法/基本原理/字符串/index.html","97242b202babb959b85e5c5238cd96b1"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","090375665c546040e00049cfce9c207a"],["/categories/数据结构和算法/基本原理/数论/index.html","34b665d08e50161d3251d9358c9764d1"],["/categories/数据结构和算法/基本原理/树论/index.html","f53ae81d70344a381b65bf8bc929ab10"],["/categories/数据结构和算法/基本原理/链表/index.html","3e5cfceb609223e0c8d463b35905bec1"],["/categories/数据结构和算法/算法题/index.html","94bd5f9a2b28ed35f8b7d4b237c8915f"],["/categories/数据结构和算法/算法题/二分查找/index.html","5681cea61d41df7c3b281d317eea16e6"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","40f0177af7a364262673a91fc2b5e72b"],["/categories/数据结构和算法/算法题/动态规划/index.html","55af584d76b1480c3b54d0fc91841720"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","23cff396503400e0b0eb3e75e72484fc"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","35aed613f4e436a179bff9eeecd10cbd"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","8aaf81e35a93736e76409b750f587c64"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d8532c0bd67ad0e2e5fedf7087328e35"],["/categories/数据结构和算法/算法题/数论/index.html","8cb3dfd02a48f1132564a2b14727d888"],["/categories/数据结构和算法/算法题/栈和队列/index.html","cd96f167d50175d5c28f07fe97cce046"],["/categories/数据结构和算法/算法题/树论/index.html","081556688ed637f7cf1728a3fa193f3d"],["/categories/杂七杂八/index.html","1f1dbaa520e35d48a15910c94519e424"],["/categories/杂七杂八/博客搭建/index.html","68ca2fc1694ea1035c6a630c59883d2e"],["/categories/编程工具下载/index.html","ef6a68d9841c7e1afc57a9a2aaedb5b2"],["/categories/编程环境/index.html","2971b0a031b3aab35a274804063ec644"],["/categories/编程环境/大数据/index.html","4901f0152e827aaeb6fddfdcc79ba95d"],["/categories/英语学习/index.html","5063d647c8f991a3b5212bc472f65eb3"],["/categories/英语学习/英语语法/index.html","94361a526a4d3d748c4a382f7cd157b9"],["/comments/index.html","bc6ff0ba44b93f1e55b005c6539ce806"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","264b7d7489bfa13a8c9aa1ede959960d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","5de61d8dc92c1c8087230c5cd307848f"],["/movies/index.html","0e7ff72beba10a7dc78ab018422b9de7"],["/music/index.html","7d768be28d6c28333c17e29fccf0174e"],["/page/2/index.html","615c8f6c47ee36399623f6c8026c4c76"],["/page/3/index.html","8436dd7517bc93f901fc8a4e7be38692"],["/page/4/index.html","54bd8ba42519b4c7a7f705ab99d0e7fe"],["/page/5/index.html","ebce8d8132f6771a61fd5f4c08f91d86"],["/page/6/index.html","10deb658b79018fca6803b424e1c138f"],["/posts/1021360842.html","ab4c749ae47f032b31cb4f4be82a9f5c"],["/posts/1120620192.html","862592dc275d5b42702ddbf2872c5970"],["/posts/1141628095.html","84c2b45f363f60b92cafffff48abf6d0"],["/posts/1168613674.html","5039a72beb1b4313bcbd0d85bea32237"],["/posts/1219920510.html","6ca4ffbe6d7f383c0af867680db7eb39"],["/posts/1222166338.html","803450e277e26ab420d50a5485c2fec5"],["/posts/1259097482.html","db564101e77f614371560d3b712e46fa"],["/posts/1271036369.html","eb9db1cd9f78f8861b62af49fd27be62"],["/posts/1312847445.html","41e9234fb8545d32eb6875b23cc00361"],["/posts/135355774.html","2392b8cb4aecd2757a84b147dcc65251"],["/posts/1375344716.html","1d04b14a082a4d1557d7c15b690763fc"],["/posts/1388991698.html","250835864ff76ba12b4b0b69097f7fac"],["/posts/1410315814.html","a67b99a7cca8260388fa51944ca9f9bb"],["/posts/1452790229.html","0d100175a68d5048dcb4177469bf8b09"],["/posts/1470079884.html","e005339f62a792eb80a8c007f6a572cf"],["/posts/1470079885.html","dab46c19f92418aa0bd0d2a561076c18"],["/posts/1470079886.html","263b4dc791109d3ecdc6de00e7c51893"],["/posts/1470079887.html","acf0554a22651dc158bb8b3a042dbb64"],["/posts/1498536549.html","80f973d5c6448cab2d0f797605b3c927"],["/posts/1539568593.html","8e434bc87d8f6669dc8c2e4f9ceea2de"],["/posts/1547067935.html","7d5263ef1a589df3f2eb28b0ae1a66f5"],["/posts/1557866301.html","0bd562d2d206000e5517d9f027da1b47"],["/posts/1571776361.html","4ca80970344d2a8aeec20cfa5ae8b651"],["/posts/1605124548.html","032f87d941491b7396b7dca4af8a1707"],["/posts/1633036852.html","234f1a677b6aa649b87ed5c4a90afd80"],["/posts/1674202625.html","d1234c40a0547dbeb36d1cbe4049daf2"],["/posts/1765123828.html","6c7c9222b80c694a7539e3e1b6558c03"],["/posts/1767336200.html","e4aa14e612b3f79504ef7c69d823b1b1"],["/posts/1776114197.html","3b01211dbc66cc0e37fa56ac5d5e77ef"],["/posts/1817748743.html","077ddfe6fcd39ffc6f08d817ded3c76c"],["/posts/1925125395.html","bc80ea960ac0ea51d89c0d000b4df911"],["/posts/1966191251.html","eef53db438616e6a4c99ddf649cf2848"],["/posts/1987617322.html","fb81fa55f9599962c965a3d8befdd728"],["/posts/1999788039.html","bbe342db5623e79a64fa885c78c5f04c"],["/posts/2075104059.html","f3f904496974334c294a10e6c54fd1e4"],["/posts/2087796737.html","5b21387f88c8767765ca03a667d4993b"],["/posts/2106547339.html","3bf3f2b577744cede1c1099cb2304902"],["/posts/2207806286.html","35dccb6d743389e06ce225b3a05fef18"],["/posts/2225903441.html","2a95805ca11f6631d81e2fb929342974"],["/posts/2265610284.html","18f673a85547a5b42de36e0f6a4be64f"],["/posts/2281352001.html","e1e332e187198984fffa33f5dae209c3"],["/posts/2364755265.html","7237e44eca8418fa400975b832368bba"],["/posts/2414116852.html","26d624de13e120fe7f761824d9e3af39"],["/posts/2421785022.html","ac6ac9d791fc965f8d963a29183dad93"],["/posts/2482902029.html","ea83598bec97245932e732577b8e1708"],["/posts/2495386210.html","0f3eaf405866a71d447a296768a743d2"],["/posts/2516528882.html","7655239c26aa8fa04a9fe74965246bd3"],["/posts/2526659543.html","34c0444dcd8635ed007c28c389bf2d67"],["/posts/2529807823.html","90f0b0b467ee76fe2ab3824a2c5ee43d"],["/posts/2596601004.html","194025f4b2416ef72bb472392a84f834"],["/posts/2697614349.html","8825fc9d315989dc6c3ab8c2c2b00b4a"],["/posts/2742438348.html","ccaabc73cb3021af5dae2facf02b0664"],["/posts/2768249503.html","c5a9dd23d5bf915939c1e5742b5a9814"],["/posts/2864584994.html","f3d5fdb4f12260638dd3c5efaf2bd3a1"],["/posts/2888309600.html","ff2a239258c7056680eec034d57145d3"],["/posts/2891591958.html","79bcdbab36d5ae1c569f821d407ca172"],["/posts/2909934084.html","c3f09fbe132381a3568d3d0d5d764b9e"],["/posts/2920256992.html","99cb8bdf5737acc081677fe6e7f90494"],["/posts/2959474469.html","6bef0bac9ce4d8c538dd7f3cac489769"],["/posts/3005926051.html","bcb9944f252613bb679cb9fe1b315933"],["/posts/309775400.html","d33d950757275e46bd8ea5aad65c0feb"],["/posts/3156194925.html","5dc527d86801f657d3a5fae2f4840dd5"],["/posts/3169224211.html","ef86c80ec82d9681259e9451cbf25a72"],["/posts/3213899550.html","4e5cf0a0936deaa76866b0eff1de5c2f"],["/posts/3259212833.html","3a065e7e0477e538b4cdda7532733aa0"],["/posts/3266130344.html","603e2fa0b939ad6d172cfd0da47a7b48"],["/posts/3292663995.html","6269c9edf389e3f3f2a21ecc56ba6c2e"],["/posts/3297135020.html","4e00e43d175e7bf1084588daebfe3a92"],["/posts/3306641566.html","789e423f32f85cf88480f9d773970c06"],["/posts/3312011324.html","1ea07f8439d2bd93ca5552291faea05f"],["/posts/336911618.html","74c310eec2d479deb86ab576741aba18"],["/posts/3402121571.html","b2ff966665b48045a6e6a27ac2610a1d"],["/posts/3405577485.html","0de661053593436ab3e1024172be596a"],["/posts/3498516849.html","94f1a363ab30fe5d5aaa3ac6db28255e"],["/posts/3513711414.html","36fbaf79e323aa72a53a816d53fcf4d1"],["/posts/3523095624.html","d11f05281b640642cb571721455a0f33"],["/posts/3546711884.html","5f31516b1232d2d895a5386b61d1392f"],["/posts/3731385230.html","bc192635265e3f22aacd0955b0cab05d"],["/posts/3772089482.html","439fbb16d55c2a54709d50e449014259"],["/posts/386609427.html","58fa3abc760301bd1d85280cde431e42"],["/posts/4044235327.html","d2bba159ed99cbbc62ad6886cd00e8cd"],["/posts/4115971639.html","e9162d389790240900df253fae26a87f"],["/posts/4130790367.html","33e3715855be60e6644efa997d9e59ef"],["/posts/4131986683.html","03c91aac3e7742a17f6c43bc372fe6ed"],["/posts/4177218757.html","1d48e5219d0e9da271145eb549afc057"],["/posts/4192183953.html","199ef8ad41b002a36d59215ee6ff3f5d"],["/posts/4261103898.html","f801e0da96cb4da9df5f57d0f5be6917"],["/posts/469711973.html","4cd45604f614108c397d6d7dc465371d"],["/posts/482495853.html","bbed090367a9766f03d5ca33f90d01e1"],["/posts/488247922.html","a531449fa7a9ebab4356f1e43159d740"],["/posts/517302816.html","4e45d2486f7f8295e6c55ec8b92aca8b"],["/posts/570165348.html","9a8fdba0c312cdb9d2a53998cdf65a6d"],["/posts/595890772.html","4fd91567a7be1185abc78f30774dde24"],["/posts/67485572.html","1f5ff9fc91a9eee19df80743f4de1db2"],["/posts/694347442.html","562b1eadc68e2175c82192449ac9e741"],["/posts/707384687.html","ebffae9daa983efac78612ed7fce7487"],["/posts/71180092.html","aaba098448a86f8c15f42c5567ea4e42"],["/posts/716459272.html","d0f828d88f4fa66d5c73f47c8064f537"],["/posts/765481613.html","31a0930c3f77bdfea306a2c8ce0b26a5"],["/posts/778231993.html","12f876f4691fa99684594a449ca0c2f8"],["/posts/795397410.html","3ba360ad22c9acbf21341dd51e36f506"],["/posts/820223701.html","ab1382dc12a286069c2274a185c04488"],["/posts/830372185.html","15bf2f3c7ecc93abeaa1dd6974e70cdd"],["/posts/88294277.html","81f326b67d815dff4d76203326b5dd41"],["/posts/939963535.html","c671ff0c0ca70bb71b9a32ba05293e26"],["/posts/983786067.html","fd84e949c506f4570c130ff24cf4b5da"],["/sw-register.js","2ecf95a2b81045646fe54636b509f1f6"],["/tags/C/index.html","1771967d9654e113f592a35977ebaefa"],["/tags/C/page/2/index.html","f802fe03e89f6f69a15f37ac776cbab9"],["/tags/C/page/3/index.html","195285162825e30e44ee6b0cf9f1e344"],["/tags/C/page/4/index.html","8e55e86adb8aa893cc349e2eb61d2250"],["/tags/ETL/index.html","c9a7d7c4519b4455355c877258e92f53"],["/tags/ElasticSearch/index.html","e5ca89e8482c5e4e202464f3bc17b6fa"],["/tags/GUI/index.html","3e1fdf1122fa46d023c9625d32b915c3"],["/tags/HBase/index.html","c2aa26f7586a1f43d54e562351103d13"],["/tags/Hadoop/index.html","ad038208bc3eac1afe5a00e792b29ad4"],["/tags/Hadoop/page/2/index.html","222bef671cdaa2aff337645309b0b504"],["/tags/Java/index.html","2cb370a37ab2c5ba4d85432a68b2b52a"],["/tags/Java后端/index.html","c990f3bfad15569c163c2270d7437b0c"],["/tags/Java后端/page/2/index.html","9173a314634e493fb1c11da36f9a06d8"],["/tags/Java基础/index.html","52f8705998d2901b7c95d81be6e644a4"],["/tags/Java基础/page/2/index.html","fd279c2f061249f5a651dd35770e7f3a"],["/tags/Kettle/index.html","7fda75a48241505de059a5769bbf714d"],["/tags/Kibana/index.html","10de38572b8f34dd99af83964cd8ac66"],["/tags/Linux/index.html","a74df70e27497cf15b14ef7509eaa928"],["/tags/Linux/page/2/index.html","010708e91a883545b78fbc22f782812b"],["/tags/Linux/page/3/index.html","aaace19f66f5fce83cdb743a3f4662fd"],["/tags/Mac/index.html","f493072e565b986431f1526ac42fbbb2"],["/tags/Mac/page/2/index.html","6cdc2440c03a50828bf9d96d1df964c2"],["/tags/Maven/index.html","4930071798d374c832b3ca11f8b6a170"],["/tags/MySQL/index.html","d248c3ea0843bc511327aa2b41026351"],["/tags/Python/index.html","cad6b29883424e7fd2919ecdf761d71b"],["/tags/Redis/index.html","205a2ab61ad84a5628348aabbd886b22"],["/tags/R语言/index.html","b04fad2cbbdf6157628fe3b9da58c4dc"],["/tags/Spark/index.html","6fb636e4672d745e92411a2fcb81f2ff"],["/tags/Ubuntu/index.html","51c1d2ec06bf0ebb695c3eba22041770"],["/tags/Vue/index.html","aad025f93005943a82b6a1d7462d06bd"],["/tags/Windows/index.html","f52910c2b18d00f61ffe776141f415a2"],["/tags/ZooKeeper/index.html","c4297dbd01fad4a93fea00933ef177fd"],["/tags/bfs/index.html","34d5b44202c57f351d9ab93b42c0e462"],["/tags/dfs/index.html","c51562d4fb63b86f1011c56c55f8cc95"],["/tags/folium/index.html","94532e89b41e7ebc6f04db4acee721b9"],["/tags/git/index.html","80580c52f1331833ad73ab77bbbf0d5f"],["/tags/index.html","89ed553fc902e64d0bbf58d70cbbfe3d"],["/tags/latex/index.html","b1aa8ad3fea65adb62d21ccd64f53d3b"],["/tags/中间件/index.html","bc57a45adf3e5f508c1836a69d771e3c"],["/tags/二分查找/index.html","25d70080cf4cb51a186859dfdf57936a"],["/tags/优化类/index.html","2dec02906cfd32a01011fedf808d144a"],["/tags/前端/index.html","765e26fbd34cacd2056259215528359e"],["/tags/前缀和与差分/index.html","46b65c5643f98670f33e25a59b463959"],["/tags/动态规划/index.html","b7003d9cf83f519a9a09a31070a36015"],["/tags/动态规划/page/2/index.html","21ca55794582bab106926820fd69c249"],["/tags/博客搭建/index.html","3a837fada6a2dfe925fbdfed7021ff4f"],["/tags/图论/index.html","a5cc29ecda8c1d2379de28b9bd0423bc"],["/tags/大数据/index.html","a558441159b15fae4743a8772af57326"],["/tags/大数据/page/2/index.html","e092dd8579d3322990b17041776a47f9"],["/tags/操作系统/index.html","495384a4182a5843591a45ea2d938f2d"],["/tags/数学建模/index.html","da01de2885a7c862d34e2a397a9182de"],["/tags/数据库/index.html","d33722fc7d970bfc3c11f159004d33c0"],["/tags/数据结构和算法/index.html","7182937c42f4a6f731c72cc7edf38929"],["/tags/数据结构和算法/page/2/index.html","1eacfc1b3feb930daf7e3ccca8108270"],["/tags/数据结构和算法/page/3/index.html","9df71beeb2e28bfd63831acbab30aba1"],["/tags/数据结构和算法/page/4/index.html","96812bc307ee0b3bdafa66177ba46415"],["/tags/数组和字符串/index.html","fd70d006d19d354e2d8622ed9afb5fec"],["/tags/数论/index.html","45cc563cf529b2116c2edea051d3c5da"],["/tags/枚举类/index.html","8e0ffdbd3c38db38829f3d0d87882bd0"],["/tags/栈和队列/index.html","b292cd0875f2360685ad7ef8d44ecfa9"],["/tags/树论/index.html","0a18072c26b3100a554d813b5ac752e6"],["/tags/测试/index.html","1c596d85de64a7095a74cad780eba899"],["/tags/环境/index.html","fbe35d160b95610e11170c622b0a3634"],["/tags/环境变量/index.html","9fde4223a0eb46289dfa41b9c3b54fe5"],["/tags/绘图/index.html","7c5e69302cc247060dd1f9f135ecbf33"],["/tags/编程工具/index.html","dbb08c5a9a068f952fcfe318e90208cf"],["/tags/编程环境/index.html","2147e42b0a667ae818bd32835e3e8176"],["/tags/网络编程/index.html","bb05182b6bad373c177a803cc614468f"],["/tags/英语语法/index.html","9369b8c038967ee1c5efceb377404cdb"],["/tags/计算机操作系统/index.html","1330274224888001c8b24566a0018988"],["/tags/论文/index.html","0c8a89e1e5a30d065eac5f6f315f87e9"],["/tags/资源下载/index.html","7ec25db8ecf9bcd215699b8dddbcee47"],["/tags/链表/index.html","5462039e4f453ef90e10957555a1b6ce"],["/tags/集合/index.html","eff5e0a143c45d00f12d13992e4cf186"],["/tags/集群/index.html","216eb6b0d42f9fd98cc2aa21c8975789"]];
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
