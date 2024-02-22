/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","7c26be18f7a3b0d909902848c6468a35"],["/about/index.html","cd024c4bee5bd91d11e39abe0cfca87b"],["/archives/2023/01/index.html","1350f38bd83d23f4bd12a20eda18dffe"],["/archives/2023/02/index.html","ec8f0ab31f64ae5393c8b3ef3963122d"],["/archives/2023/02/page/2/index.html","03599820156b3be17f7277df81855f47"],["/archives/2023/03/index.html","f8d476c31652671c3f183c4e4afd93af"],["/archives/2023/05/index.html","68f786ec68e099793b5b913b2be61c4a"],["/archives/2023/06/index.html","a4c3c25688d406d9053afd1bd239bde3"],["/archives/2023/09/index.html","398479135fe3604b52ea10915e3b9c7f"],["/archives/2023/11/index.html","90601db51f51bdf5d8c7b2fd748866aa"],["/archives/2023/12/index.html","a18434e9fac161d2182119ecefed73d0"],["/archives/2023/index.html","964789d3d8e0f7d3e2f30f9e04540b12"],["/archives/2023/page/2/index.html","b819291625ec80cf68dcc98e15b28000"],["/archives/2023/page/3/index.html","750014d772d6fa20c6c5b5e13221f33d"],["/archives/2023/page/4/index.html","7863e1d5dc00a96c7dc36b5c413f4ccb"],["/archives/2024/02/index.html","0683887f328e6ff18010690c5b9772ac"],["/archives/2024/index.html","78b777d1331a6736457f058019f99bc9"],["/archives/index.html","a78e06bf2ef792dbac4d13d643e5b86a"],["/archives/page/2/index.html","2de0f94eb3b9ba2a6bfb26751780f104"],["/archives/page/3/index.html","5d6186be8d031a9e370a8fef5e61a0e6"],["/archives/page/4/index.html","30a11d753f1c94c014870e5501350af2"],["/baidu_verify_codeva-qQP2iZOMLX.html","0fe0456ef9ebf9221db243a9e8f77d6a"],["/categories/Java/index.html","7941b7ea88f702ea07962ff421ec50e7"],["/categories/Java/后端/index.html","bc0c475994106ea637fbed5afee766ce"],["/categories/Java/基础/index.html","16cd05c240cc2976030aa4838ac55370"],["/categories/Java/基础/集合/index.html","19c72126cb7c000a2912b2e446a3ab58"],["/categories/Python/index.html","8fef4827c89c214ddaa310da3f2faaa0"],["/categories/Python/编程环境/index.html","c68dcc571c9751df6af4165386b024d5"],["/categories/R语言/index.html","de6f6bd96c33234b4055700cdc087683"],["/categories/R语言/编程环境/index.html","91251aead39489154edee5509260b4c9"],["/categories/index.html","662924d35b01c2c9d808ad4b4f3ed82f"],["/categories/中间件/index.html","7983ebbb8e99c06129c5f27d5566470e"],["/categories/前端/Vue/index.html","dd7a60e40ed4c6ebabf9ad3d76cf4e12"],["/categories/前端/index.html","686111b8680373e6de5264f9568c375b"],["/categories/大数据开发/ElasticSearch/index.html","b4ae850810c6a1ef37f15b64dcb45488"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","b90378755f5a6f8f0646056b18b5d1b3"],["/categories/大数据开发/HBase/index.html","c1444d6bf9ada032c6af57ffae8e63cf"],["/categories/大数据开发/HBase/学习笔记/index.html","e644f817507303f717177becbb135c87"],["/categories/大数据开发/HBase/环境搭建/index.html","8ec45efdc8d3d125eb97c4c2613f5404"],["/categories/大数据开发/Hadoop/index.html","d3311838a0ca44ef1a46040f70a41e3a"],["/categories/大数据开发/Hadoop/技术/index.html","412f55a647fafbbc8caa65bee78e3d2c"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a5f758dce327d045bb2ab9bd5afc6fe8"],["/categories/大数据开发/Redis/index.html","d6b0cac5563c34997aafae9fa48088ac"],["/categories/大数据开发/Redis/技术/index.html","1fd9d74e3572c6861075b53ae5c97302"],["/categories/大数据开发/Redis/环境搭建/index.html","e9f8f793c25e962c375340627af0b092"],["/categories/大数据开发/Spark/index.html","bfda9f942cd25a13b209a5d449355f03"],["/categories/大数据开发/Spark/环境搭建/index.html","8b6f89e5fdef02504e997cdcc316040a"],["/categories/大数据开发/Zookeeper/index.html","d2431989810b67e9a868ad47f407b671"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","fff76e9ee595dc083c46bae5ff599e64"],["/categories/大数据开发/index.html","0d9bc8d934fa44f441662d5d889de105"],["/categories/学校课程/index.html","41b837a84cf2f22f4d9137e34f1fee87"],["/categories/学校课程/计算机操作系统/index.html","b0ab5e7b6a4993ffee96051277ac61f7"],["/categories/操作系统/Linux/index.html","5b74bf2570abb6a62dfb06028530163c"],["/categories/操作系统/Mac/index.html","e2c2c196d875f2028c707a9d802773ef"],["/categories/操作系统/Windows/index.html","4e9805e256cedad7bbbdf3892a3c950d"],["/categories/操作系统/index.html","363b49a16c273a6289277176b3153083"],["/categories/数学建模/index.html","13528f14cf9a544cad7aec5ad39bea8f"],["/categories/数学建模/latex/index.html","47047441280790807a2b846675b9ceb9"],["/categories/数学建模/优化类/index.html","5771bbd08e48f203a2685e8c407bd36b"],["/categories/数学建模/优化类/现代优化算法/index.html","2751b2e03dd51bfabcfcaa69b7c2bc6e"],["/categories/数学建模/优化类/规划类/index.html","0401ca817d4105f13a0f4a64534d72d1"],["/categories/数学建模/绘图/index.html","2c813b143e19afc0a199cd502ee45c66"],["/categories/数据库/MySQL/index.html","62d122bb5d16feb5125f30bc20f301dc"],["/categories/数据库/index.html","e6eaccb286b3c0a12d52faf093c3e2ac"],["/categories/数据结构和算法/index.html","75068093fb360ba8b5336cef3725203e"],["/categories/数据结构和算法/page/2/index.html","126d81db15aaea937de38bbc9fe7ed0e"],["/categories/数据结构和算法/基本原理/bfs/index.html","7f4db9c331e17e446d771aed9a229a9c"],["/categories/数据结构和算法/基本原理/dfs/index.html","e5d69049d2793de9c801c1322dc469cb"],["/categories/数据结构和算法/基本原理/index.html","823ceff4d9c64e708a013fa6f9df2cab"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f4fad1d89fa5d60c90fdf07dd3edd87e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0255228a464cf8fd4cd7ffde0fdf5e77"],["/categories/数据结构和算法/基本原理/图论/index.html","a666f161c8d1b159e4dfe61b50fdfa90"],["/categories/数据结构和算法/基本原理/字符串/index.html","a5c3aa57d521ce04d92d5b1c22fc5b18"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","effb56628979084ec4e565d10f398ac5"],["/categories/数据结构和算法/基本原理/数论/index.html","9c6d06e491b576ffeb1dee31b95ffe11"],["/categories/数据结构和算法/基本原理/树论/index.html","56c95aea058485f269bef5f65fb8ef50"],["/categories/数据结构和算法/基本原理/链表/index.html","c069694da18caf5b82ff3ca3ab186d60"],["/categories/数据结构和算法/算法题/index.html","60922ec4409c97f38a7536f53cd4ccd4"],["/categories/数据结构和算法/算法题/二分查找/index.html","1cb1295c1d0d8a358d60f7701e99b32c"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","74e0ea8ae02e69baa20fd748b55f2d04"],["/categories/数据结构和算法/算法题/动态规划/index.html","49517c71a91b2817a194d618a0211ad2"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","98325c884ca928437928407de16e0382"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","c3a634d30ec98dabd5f95eb02f84a175"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d8c5c161e2fffeb6f754b6130d9141d9"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","5101295462d18e7d6baee5549bf7bc9e"],["/categories/数据结构和算法/算法题/数论/index.html","d05ef12c3fdb8a000db7160af1c07a2d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","27fecb867d4acd50a44480af60a35c4e"],["/categories/数据结构和算法/算法题/树论/index.html","0c5cb560179947cb5682274362a67d02"],["/categories/杂七杂八/index.html","0dce233b09e741f58d09c92a8e59ca94"],["/categories/杂七杂八/博客搭建/index.html","0258e1755b34729c6635b186f9f01f4e"],["/categories/编程工具下载/index.html","a5402eff3b919d378c216322993eb5df"],["/categories/编程环境/index.html","190349a6c52faccf897d0e4c547cb2c9"],["/categories/编程环境/大数据/index.html","9b3910b0b177a4f8f8cd3a2def43aed6"],["/categories/英语学习/index.html","d659d79b38be9ce14560423e17de6f34"],["/categories/英语学习/英语语法/index.html","61f28ccd8a73ce0c2bde5b9838d81dc2"],["/comments/index.html","d4c6a17f658c594c26cc6d0694a28dbf"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","7231062bef9d413c8c9f365397af0efd"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1b067fe966896d2fc1ad08fd161be3f4"],["/movies/index.html","7defda07e2c18d8b6c7e340619b26953"],["/music/index.html","ba38ce320ee2ae1b1473b07aa3bd1fee"],["/page/2/index.html","0d57af3c4e421e491dba0561022d8b63"],["/page/3/index.html","24f787c4dda9a8da1801b4dee7533418"],["/page/4/index.html","a0dc5dd3404f61009d56fe3b3c645ca4"],["/page/5/index.html","6e1ed02d09503b6457aea9e5f5eec8f5"],["/page/6/index.html","dad1f0040341781adacec4f3101fb106"],["/posts/1021360842.html","ab4c749ae47f032b31cb4f4be82a9f5c"],["/posts/1120620192.html","862592dc275d5b42702ddbf2872c5970"],["/posts/1141628095.html","84c2b45f363f60b92cafffff48abf6d0"],["/posts/1168613674.html","5039a72beb1b4313bcbd0d85bea32237"],["/posts/1219920510.html","6ca4ffbe6d7f383c0af867680db7eb39"],["/posts/1222166338.html","803450e277e26ab420d50a5485c2fec5"],["/posts/1259097482.html","db564101e77f614371560d3b712e46fa"],["/posts/1271036369.html","eb9db1cd9f78f8861b62af49fd27be62"],["/posts/1312847445.html","41e9234fb8545d32eb6875b23cc00361"],["/posts/135355774.html","2392b8cb4aecd2757a84b147dcc65251"],["/posts/1375344716.html","1d04b14a082a4d1557d7c15b690763fc"],["/posts/1388991698.html","250835864ff76ba12b4b0b69097f7fac"],["/posts/1410315814.html","a67b99a7cca8260388fa51944ca9f9bb"],["/posts/1452790229.html","0d100175a68d5048dcb4177469bf8b09"],["/posts/1470079884.html","e005339f62a792eb80a8c007f6a572cf"],["/posts/1470079885.html","dab46c19f92418aa0bd0d2a561076c18"],["/posts/1470079886.html","263b4dc791109d3ecdc6de00e7c51893"],["/posts/1470079887.html","acf0554a22651dc158bb8b3a042dbb64"],["/posts/1498536549.html","80f973d5c6448cab2d0f797605b3c927"],["/posts/1539568593.html","8e434bc87d8f6669dc8c2e4f9ceea2de"],["/posts/1547067935.html","7d5263ef1a589df3f2eb28b0ae1a66f5"],["/posts/1557866301.html","0bd562d2d206000e5517d9f027da1b47"],["/posts/1571776361.html","4ca80970344d2a8aeec20cfa5ae8b651"],["/posts/1605124548.html","032f87d941491b7396b7dca4af8a1707"],["/posts/1633036852.html","234f1a677b6aa649b87ed5c4a90afd80"],["/posts/1674202625.html","d1234c40a0547dbeb36d1cbe4049daf2"],["/posts/1765123828.html","6c7c9222b80c694a7539e3e1b6558c03"],["/posts/1767336200.html","e4aa14e612b3f79504ef7c69d823b1b1"],["/posts/1776114197.html","3b01211dbc66cc0e37fa56ac5d5e77ef"],["/posts/1817748743.html","077ddfe6fcd39ffc6f08d817ded3c76c"],["/posts/1925125395.html","bc80ea960ac0ea51d89c0d000b4df911"],["/posts/1966191251.html","eef53db438616e6a4c99ddf649cf2848"],["/posts/1987617322.html","fb81fa55f9599962c965a3d8befdd728"],["/posts/1999788039.html","bbe342db5623e79a64fa885c78c5f04c"],["/posts/2075104059.html","f3f904496974334c294a10e6c54fd1e4"],["/posts/2087796737.html","5b21387f88c8767765ca03a667d4993b"],["/posts/2106547339.html","3bf3f2b577744cede1c1099cb2304902"],["/posts/2207806286.html","35dccb6d743389e06ce225b3a05fef18"],["/posts/2225903441.html","2a95805ca11f6631d81e2fb929342974"],["/posts/2265610284.html","18f673a85547a5b42de36e0f6a4be64f"],["/posts/2281352001.html","e1e332e187198984fffa33f5dae209c3"],["/posts/2364755265.html","7237e44eca8418fa400975b832368bba"],["/posts/2414116852.html","26d624de13e120fe7f761824d9e3af39"],["/posts/2421785022.html","ac6ac9d791fc965f8d963a29183dad93"],["/posts/2482902029.html","ea83598bec97245932e732577b8e1708"],["/posts/2495386210.html","0f3eaf405866a71d447a296768a743d2"],["/posts/2516528882.html","7655239c26aa8fa04a9fe74965246bd3"],["/posts/2526659543.html","34c0444dcd8635ed007c28c389bf2d67"],["/posts/2529807823.html","90f0b0b467ee76fe2ab3824a2c5ee43d"],["/posts/2596601004.html","194025f4b2416ef72bb472392a84f834"],["/posts/2697614349.html","8825fc9d315989dc6c3ab8c2c2b00b4a"],["/posts/2742438348.html","ccaabc73cb3021af5dae2facf02b0664"],["/posts/2768249503.html","c5a9dd23d5bf915939c1e5742b5a9814"],["/posts/2864584994.html","f3d5fdb4f12260638dd3c5efaf2bd3a1"],["/posts/2888309600.html","ff2a239258c7056680eec034d57145d3"],["/posts/2891591958.html","79bcdbab36d5ae1c569f821d407ca172"],["/posts/2909934084.html","c3f09fbe132381a3568d3d0d5d764b9e"],["/posts/2920256992.html","99cb8bdf5737acc081677fe6e7f90494"],["/posts/2959474469.html","6bef0bac9ce4d8c538dd7f3cac489769"],["/posts/3005926051.html","bcb9944f252613bb679cb9fe1b315933"],["/posts/309775400.html","d33d950757275e46bd8ea5aad65c0feb"],["/posts/3156194925.html","5dc527d86801f657d3a5fae2f4840dd5"],["/posts/3169224211.html","ef86c80ec82d9681259e9451cbf25a72"],["/posts/3213899550.html","4e5cf0a0936deaa76866b0eff1de5c2f"],["/posts/3259212833.html","3a065e7e0477e538b4cdda7532733aa0"],["/posts/3266130344.html","603e2fa0b939ad6d172cfd0da47a7b48"],["/posts/3292663995.html","6269c9edf389e3f3f2a21ecc56ba6c2e"],["/posts/3297135020.html","4e00e43d175e7bf1084588daebfe3a92"],["/posts/3306641566.html","789e423f32f85cf88480f9d773970c06"],["/posts/3312011324.html","1ea07f8439d2bd93ca5552291faea05f"],["/posts/336911618.html","74c310eec2d479deb86ab576741aba18"],["/posts/3402121571.html","b2ff966665b48045a6e6a27ac2610a1d"],["/posts/3405577485.html","0de661053593436ab3e1024172be596a"],["/posts/3498516849.html","94f1a363ab30fe5d5aaa3ac6db28255e"],["/posts/3513711414.html","36fbaf79e323aa72a53a816d53fcf4d1"],["/posts/3523095624.html","d11f05281b640642cb571721455a0f33"],["/posts/3546711884.html","5f31516b1232d2d895a5386b61d1392f"],["/posts/3731385230.html","bc192635265e3f22aacd0955b0cab05d"],["/posts/3772089482.html","439fbb16d55c2a54709d50e449014259"],["/posts/386609427.html","58fa3abc760301bd1d85280cde431e42"],["/posts/4044235327.html","d2bba159ed99cbbc62ad6886cd00e8cd"],["/posts/4115971639.html","e9162d389790240900df253fae26a87f"],["/posts/4130790367.html","33e3715855be60e6644efa997d9e59ef"],["/posts/4131986683.html","03c91aac3e7742a17f6c43bc372fe6ed"],["/posts/4177218757.html","1d48e5219d0e9da271145eb549afc057"],["/posts/4192183953.html","199ef8ad41b002a36d59215ee6ff3f5d"],["/posts/4261103898.html","f801e0da96cb4da9df5f57d0f5be6917"],["/posts/469711973.html","4cd45604f614108c397d6d7dc465371d"],["/posts/482495853.html","bbed090367a9766f03d5ca33f90d01e1"],["/posts/488247922.html","a531449fa7a9ebab4356f1e43159d740"],["/posts/517302816.html","4e45d2486f7f8295e6c55ec8b92aca8b"],["/posts/570165348.html","9a8fdba0c312cdb9d2a53998cdf65a6d"],["/posts/595890772.html","4fd91567a7be1185abc78f30774dde24"],["/posts/67485572.html","1f5ff9fc91a9eee19df80743f4de1db2"],["/posts/694347442.html","562b1eadc68e2175c82192449ac9e741"],["/posts/707384687.html","ebffae9daa983efac78612ed7fce7487"],["/posts/71180092.html","aaba098448a86f8c15f42c5567ea4e42"],["/posts/716459272.html","d0f828d88f4fa66d5c73f47c8064f537"],["/posts/765481613.html","31a0930c3f77bdfea306a2c8ce0b26a5"],["/posts/778231993.html","12f876f4691fa99684594a449ca0c2f8"],["/posts/795397410.html","3ba360ad22c9acbf21341dd51e36f506"],["/posts/820223701.html","ab1382dc12a286069c2274a185c04488"],["/posts/830372185.html","15bf2f3c7ecc93abeaa1dd6974e70cdd"],["/posts/88294277.html","81f326b67d815dff4d76203326b5dd41"],["/posts/939963535.html","c671ff0c0ca70bb71b9a32ba05293e26"],["/posts/983786067.html","fd84e949c506f4570c130ff24cf4b5da"],["/sw-register.js","6648f60140522330b4aac45211c051a1"],["/tags/C/index.html","98eb4d10ad2be63bdd236c281306abf6"],["/tags/C/page/2/index.html","7e34406bba496c235a9bc533a66bcff7"],["/tags/C/page/3/index.html","884edd8e40268a240238521ba008e361"],["/tags/C/page/4/index.html","3075de0f1aff34e7974204474a8f4d91"],["/tags/ETL/index.html","24b7178764f08ba9b12c339a1cae5401"],["/tags/ElasticSearch/index.html","8aec940bf2caf1fb8a41fc6fdd7368b9"],["/tags/GUI/index.html","591d59c8052723f1757f5232d3e0a675"],["/tags/HBase/index.html","80b2edb624379f19f9108cf750ac0f99"],["/tags/Hadoop/index.html","8aa21625aa597000ccea84d0b9d28f25"],["/tags/Hadoop/page/2/index.html","0dd9d53f1f2dbf8b0e68c626d286bac4"],["/tags/Java/index.html","fa94030508fd0bcaac6f0c9af6e2e0c5"],["/tags/Java后端/index.html","9e3e275d29852a08ecd244400d1b7932"],["/tags/Java后端/page/2/index.html","9fcfcb213ad394948e58d1d820a921cb"],["/tags/Java基础/index.html","5c22cd487410ed76579bac95e011a255"],["/tags/Java基础/page/2/index.html","a72ca7af0ef3eaee410cb07ae591c653"],["/tags/Kettle/index.html","666d5eeccd666d4ac8cbbbe0cb924874"],["/tags/Kibana/index.html","a15c081e0c3d8f8b981f1c4d5798cec5"],["/tags/Linux/index.html","391cb1889b7330d2e285149d0ab23354"],["/tags/Linux/page/2/index.html","d890ef49248f91c7deb1c9c396446b16"],["/tags/Linux/page/3/index.html","59b128305907f66a452d81ff4b12f671"],["/tags/Mac/index.html","3f443cd2c81eaf84d403fadc7f9a2340"],["/tags/Mac/page/2/index.html","83c589c4e3022a6df89756633c7164e3"],["/tags/Maven/index.html","22ccf780261f0a4942ef8494718e2666"],["/tags/MySQL/index.html","2a035b17c71cff668b07c12bc7f62405"],["/tags/Python/index.html","374456e67349b83f0790f8925350195e"],["/tags/Redis/index.html","58952e7777ab463a66d43561cc663e40"],["/tags/R语言/index.html","face28ba09ab56913599d8da16bc5583"],["/tags/Spark/index.html","aa820ccaf6c62400a53995f42932bf20"],["/tags/Ubuntu/index.html","5128c5071a222b4cafef4c82d31118a8"],["/tags/Vue/index.html","695388662824c421c7750cae14a13a9d"],["/tags/Windows/index.html","360d587bea5303f1a428d70b1d50eb3c"],["/tags/ZooKeeper/index.html","c42648fe68867e8246389eeb3f24b6ab"],["/tags/bfs/index.html","551cdeac811093ef5b326d9af20ac041"],["/tags/dfs/index.html","c37cd14bd1ebf9f9efaae6f9aed43198"],["/tags/folium/index.html","02bc94ac38c48afe8de041ff76cc60e6"],["/tags/git/index.html","0d7aebd06c57d87bfa0462321c75bb25"],["/tags/index.html","77bbba577410b4121d0ae33759d49350"],["/tags/latex/index.html","69708af66394007ce9517b7f52b87d50"],["/tags/中间件/index.html","ca363b8fce4892b7df53e19aeb636e92"],["/tags/二分查找/index.html","4aaae8f903078ba887ea576d784e09f6"],["/tags/优化类/index.html","9e791edeb175fb16b69910e1170cf4f5"],["/tags/前端/index.html","798622482bd6a28fa3439b9dc61bbe01"],["/tags/前缀和与差分/index.html","378b1c9e02837d3a9b24c7403e6ab02d"],["/tags/动态规划/index.html","a3d9407e0f4e582f0c1dc7cd5f2a3578"],["/tags/动态规划/page/2/index.html","c5572db6358f1474e4541ac2b12e9b33"],["/tags/博客搭建/index.html","8634fb09f63f62652d5567d5f4827cd8"],["/tags/图论/index.html","ea3342f9d69b9a75267f6ac06a81025b"],["/tags/大数据/index.html","a8ceedad109e4af1f7525b876fd0d7ba"],["/tags/大数据/page/2/index.html","80c4f9e076ca566f2cfa43fbfae6c6f5"],["/tags/操作系统/index.html","e7872cf97e2dd6042d8a3b0c4dd06e7c"],["/tags/数学建模/index.html","8f80e059c7ce3248584eee1260d1844c"],["/tags/数据库/index.html","48de24cf1ffbbe751757fe748274d259"],["/tags/数据结构和算法/index.html","3575c5ec7c3c0e27672eda4d9f86c009"],["/tags/数据结构和算法/page/2/index.html","3a1cbdf772b4a9739ed7c1011e89ff91"],["/tags/数据结构和算法/page/3/index.html","0b251e91e947a4bb14163c5bad124136"],["/tags/数据结构和算法/page/4/index.html","1e22f096d326bfb2eb438baa926704fe"],["/tags/数组和字符串/index.html","959d230d526a332b8709ca52cd0b37fe"],["/tags/数论/index.html","9e183a6fb00ca1789a8d3e9b2a268d0c"],["/tags/枚举类/index.html","0f159c6f077ac5bbf8574c0cbe73122d"],["/tags/栈和队列/index.html","146b7312be077140ead9545bbcd1f54c"],["/tags/树论/index.html","88b6fac6da785392340fb0ae2939c9ab"],["/tags/测试/index.html","b79b290a157cfe9b4bc3f1addc4018ac"],["/tags/环境/index.html","d07b61a8f0f05fd225add7a5ecffc841"],["/tags/环境变量/index.html","5e5846c8a7cb3b3e319b291e0bf85008"],["/tags/绘图/index.html","671cae9f635affc5b32470a5e201b38a"],["/tags/编程工具/index.html","02a285965f0d343a7f4a8022292af8f4"],["/tags/编程环境/index.html","30f3a897ffe2568bd41033e3d4d77f9d"],["/tags/网络编程/index.html","30b0c1ed4bc2293aeab60d34c4d38b48"],["/tags/英语语法/index.html","23cef54a40cb6e4cc92ef88effe2837a"],["/tags/计算机操作系统/index.html","761b04f9ef74740483af838ccac0846a"],["/tags/论文/index.html","e4f832343ac9ded39950a9a8cdec91e2"],["/tags/资源下载/index.html","d0f3cdfff64a2f740f809c8e383ff2bb"],["/tags/链表/index.html","cf91c403136ef007280d3cb795e5d15b"],["/tags/集合/index.html","a092396c1e11881b80cea4224bb9e09e"],["/tags/集群/index.html","b4d7bd06d808e199edcd0acfce8f7a24"]];
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
