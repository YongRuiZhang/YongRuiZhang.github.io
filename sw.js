/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","27685c04384599d9f48cf9c393c20a6b"],["/about/index.html","61830b3352a2bd58c727a699ac6543c5"],["/archives/2023/01/index.html","5daeae396f48ec96f82c717c97bd6f3e"],["/archives/2023/02/index.html","bbe2a91f124701a372350b4a2019493c"],["/archives/2023/02/page/2/index.html","32d1a1ebbc7238b5a469e185260b3945"],["/archives/2023/03/index.html","1fb8a58704ad74033d4ec2a81b5969b3"],["/archives/2023/05/index.html","61c02fc7ac7b01c48d1669545a22e296"],["/archives/2023/06/index.html","976f94ed1a5ac9d0b581a24cd34b429d"],["/archives/2023/09/index.html","5caa49da051eec8c287c3fa9ed30b63b"],["/archives/2023/11/index.html","56aaff3b0c9e563b49e6c1aaa3bfcddd"],["/archives/2023/12/index.html","974fcdbc7e3d39e8b9f5428be18ace0d"],["/archives/2023/index.html","5d389e93a613c9a07e973c60bf45e99e"],["/archives/2023/page/2/index.html","9bb077ec243641a57d506e046fe6a033"],["/archives/2023/page/3/index.html","344f708f3d0d7c83ff8034164aa16933"],["/archives/2023/page/4/index.html","65a20c27db88a1db3c1b79d5cedf83d5"],["/archives/2024/02/index.html","5199ae73ef1e7161adb57e79be81a778"],["/archives/2024/index.html","b36ade5e15bc2c5c4a041a5158e9e7c5"],["/archives/index.html","0dec70a1e854b0363fd32331bf5ef8ed"],["/archives/page/2/index.html","73694c2b52b566eade8da2a5ad6777a1"],["/archives/page/3/index.html","64f086cdbd1e30db7dc77250119fb18c"],["/archives/page/4/index.html","05239f707880bd0c51cb2b689acd9a30"],["/baidu_verify_codeva-qQP2iZOMLX.html","a8eeeb20a4ee17a034fce0edf82ea5c4"],["/categories/Java/index.html","b967b297f6ced8303aa50e662bc5fcb2"],["/categories/Java/后端/index.html","0f8d84be085523eb19e6be7a4138c882"],["/categories/Java/基础/index.html","b1e63b386c76493ba3581159813e048b"],["/categories/Java/基础/集合/index.html","961702c9d6e50361a7002e9b65044183"],["/categories/Python/index.html","41232bdfc6e2c4e4781aa239b8874ff8"],["/categories/Python/编程环境/index.html","265f51efd2d9265c74d9d633bd29e1e9"],["/categories/R语言/index.html","9d53a71141e0eec4d4da50d9ba40a248"],["/categories/R语言/编程环境/index.html","ff8c4e29b698dcfa1e1e4c28f92786b7"],["/categories/index.html","d81a06ed60e391dafdb20b113f38c7f7"],["/categories/中间件/index.html","f2acc854f8b6e080811aae1f9655292b"],["/categories/前端/Vue/index.html","dc258642936d9ef690bcf3fe49b05de0"],["/categories/前端/index.html","596a9ed07e7a31350cb8e3c064596634"],["/categories/大数据开发/ElasticSearch/index.html","3ca458a217dc7bec2d1227182bcce427"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","66652aa50f31214244c4e4fd0d8b0676"],["/categories/大数据开发/HBase/index.html","0fab2364f42f65874b8a05cc516f3118"],["/categories/大数据开发/HBase/学习笔记/index.html","3e21528cf02b614fcaf7f19aa260da5f"],["/categories/大数据开发/HBase/环境搭建/index.html","3f4f844093acfb2593ed6706fc3c4e8e"],["/categories/大数据开发/Hadoop/index.html","73706ddc932218dc2499fffea25816ca"],["/categories/大数据开发/Hadoop/技术/index.html","67564b0da3f4826370c16748c5554162"],["/categories/大数据开发/Hadoop/环境搭建/index.html","065f997bdc42733477db308b5cf53291"],["/categories/大数据开发/Redis/index.html","14889e64cc6f38bb0ad0b43f51de5f74"],["/categories/大数据开发/Redis/技术/index.html","094bbebcb08b8a6fab8347c004b5de67"],["/categories/大数据开发/Redis/环境搭建/index.html","3aa69b6d26e7ac812ca1dce76b626d39"],["/categories/大数据开发/Spark/index.html","57bad1c2114942a149ded446b7e10631"],["/categories/大数据开发/Spark/环境搭建/index.html","94919ae45fd62e20e0e84536923ab791"],["/categories/大数据开发/Zookeeper/index.html","aeafb6d9475b2f9b1bd4a397f7cc4695"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9a0c2a7dcbb40d48b833c3e44384f416"],["/categories/大数据开发/index.html","a78de56762c8a9c13b5f43a89578df5a"],["/categories/学校课程/index.html","2d660148ce5de0a7f722b2a6bc367717"],["/categories/学校课程/计算机操作系统/index.html","dab15c31be8b342d689ee668346b22fc"],["/categories/操作系统/Linux/index.html","9864d1c393886467730c7d91a1e3133d"],["/categories/操作系统/Mac/index.html","87188e1c7e30090b7413cc70a021afee"],["/categories/操作系统/Windows/index.html","1c18108faad0b2a07b6e562e9b3f09b2"],["/categories/操作系统/index.html","a1dfde9dd08155f9f42277c4a78ee4cc"],["/categories/数学建模/index.html","2ad62419f0db7741834787a7523a7017"],["/categories/数学建模/latex/index.html","fe67ed25b0209ea977741af403a6b2eb"],["/categories/数学建模/优化类/index.html","275d0c7f53e8ef6acfbd0837a51e17ed"],["/categories/数学建模/优化类/现代优化算法/index.html","c407fd6fda00d1a0fd36f95f4a58fb5f"],["/categories/数学建模/优化类/规划类/index.html","8b6587ff1adcddb54bafcfe0c937afef"],["/categories/数学建模/绘图/index.html","7475539a499d36c157cc31a4dc81261f"],["/categories/数据库/MySQL/index.html","29ac4ead85b2e5223826f095c17ccb64"],["/categories/数据库/index.html","e79767a67fff925b06a1b5a101d15428"],["/categories/数据结构和算法/index.html","b7f6f022a160250d552ec0677bbc66ff"],["/categories/数据结构和算法/page/2/index.html","047e0a3a259335b122a87a9b078f6a99"],["/categories/数据结构和算法/基本原理/bfs/index.html","ce7a58e5741e7069ae23f7c920d55e3a"],["/categories/数据结构和算法/基本原理/dfs/index.html","c3541ce4066281af8f3917e54f8cc6e0"],["/categories/数据结构和算法/基本原理/index.html","c23d77499159b9c70e2ec0af817bbd80"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4d1ba9ee10839dc628f9e72d0b1b9266"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","33e189bc9a008b8b0e5a774549eeff2c"],["/categories/数据结构和算法/基本原理/图论/index.html","96454b104cfd96e64e35934192be9042"],["/categories/数据结构和算法/基本原理/字符串/index.html","07e052d14c2b6d672fd9fe83f0bd41eb"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","74a17ffe870ed4ac813e3cf7e1a59153"],["/categories/数据结构和算法/基本原理/数论/index.html","f6e8ea569b67a6a7a7f5d98d86a501b3"],["/categories/数据结构和算法/基本原理/树论/index.html","4bb5ad64fc66b7c59ffafd9ee058d436"],["/categories/数据结构和算法/基本原理/链表/index.html","d833fa7c5793a7a139797c39e183e646"],["/categories/数据结构和算法/算法题/index.html","2356fb543b82b25ea5bbe803427a7441"],["/categories/数据结构和算法/算法题/二分查找/index.html","a57e53b263042c107f96c9157f006658"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c4b15a7c7a92fb42d30c04f3888c5676"],["/categories/数据结构和算法/算法题/动态规划/index.html","042114e7b49a974abdfb7abfccc1bf54"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","048bf3d617137f65fccdf18968dc63a0"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","c6e79e5f70ee8afcf3d280df76177d3a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","323d1638dc3d17674cf7d17b4ab92ca3"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","f86bb77be0ff3e525b44b6ae7384ef0b"],["/categories/数据结构和算法/算法题/数论/index.html","f4623b1c0a6080b2182cd0818abe133e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c2f919469807e2911047da037d22752f"],["/categories/数据结构和算法/算法题/树论/index.html","5891c9b70becc8435a60d43f8053de4d"],["/categories/杂七杂八/index.html","25f971c03ee781f06d3bf266d3b087a9"],["/categories/杂七杂八/博客搭建/index.html","9424174d580eaeadae7a124933bd830e"],["/categories/编程工具下载/index.html","f9baad0e6c786f02984fe81bd2bf0956"],["/categories/编程环境/index.html","84be3fec7aa098f667b1237a40a3961f"],["/categories/编程环境/大数据/index.html","20539eebcd888f955b91eae38cb12b8b"],["/categories/英语学习/index.html","c40aa479f26327b69ab85713f3a0b3c9"],["/categories/英语学习/英语语法/index.html","5b166181789e74b4be5c4a8e0c1e93bc"],["/comments/index.html","3401d83a689bd120e4d327015b955072"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","423a37214797c475f34d7d3a95b1c107"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","b2a235a6b4945848398674abb28052b9"],["/movies/index.html","3b028f3322483313fbe2f80f72ff13c9"],["/music/index.html","fed8c5b8e643a30a1b34ffe18fe6e293"],["/page/2/index.html","1195d1b02874ccdd02d246737cd2808a"],["/page/3/index.html","697c92aa89c5060768e3dd94a5ed96c2"],["/page/4/index.html","5e39218e41934eae594ad04d580d4e94"],["/page/5/index.html","a6967c5cb40c08df5dccc907fa88313d"],["/page/6/index.html","edad617024487b095da462b451b479a3"],["/posts/1021360842.html","a606f43abdc6c684cfb078222f597afa"],["/posts/1120620192.html","6e3201ec4ad4ca70161f0c8e35cc6799"],["/posts/1141628095.html","44b0cd7c5b5ab362446ba12e023a0fb0"],["/posts/1168613674.html","b34dacd36930f53219717c5e50b9f063"],["/posts/1219920510.html","55414a20d13110fe0c26ea7e4b87d110"],["/posts/1222166338.html","b0ed66c3b98bf8e23e18ba83b86954eb"],["/posts/1259097482.html","fe23917b061ac266c0e7a668176541c7"],["/posts/1271036369.html","c3522be2e6730096846f48e0db009879"],["/posts/1312847445.html","1aa03722c991d3490d1515db6b90789f"],["/posts/135355774.html","e493e9a1bcb7afe58065666638c7c505"],["/posts/1375344716.html","f26b226b892b70f7ef1a7d8e3258a665"],["/posts/1388991698.html","821f5a98f667d76622dfc988e8d6eab6"],["/posts/1410315814.html","80952b95886b42ce5d58d7b616df5466"],["/posts/1452790229.html","5242e0bc93df49dd50192e369986f3d3"],["/posts/1470079884.html","0e0e7608e28658c4422c6a8b34c6005c"],["/posts/1470079885.html","5f16c7453628a8b05ab219fee48d1e64"],["/posts/1470079886.html","9a4d905b0fb67b80b627cab21ebebc5d"],["/posts/1470079887.html","88bacea2cf67a25a4689fc42b83b5c7d"],["/posts/1498536549.html","927acd9b8c47d88d7922aeaa21341434"],["/posts/1539568593.html","fd5cfd37801d04f8feb91f9d789706f1"],["/posts/1547067935.html","ffa68a881bb23877dc728fb4531f8a12"],["/posts/1557866301.html","27c8f80a188d3cc51c04d2ed9dfa9b2f"],["/posts/1571776361.html","c657f6e234af29de817f04c5112bd20d"],["/posts/1605124548.html","051e6b095df4ca17579b044c7d0c59a6"],["/posts/1633036852.html","50960cee0a1062cafcbc070390c98b9d"],["/posts/1674202625.html","476c367a78eeac6791159b17fb58b9d9"],["/posts/1765123828.html","46cf4a14e19c2be995630877e47e4ad1"],["/posts/1767336200.html","c7a4795dee62c79077901d0fd965b4e3"],["/posts/1776114197.html","f412c6eb8e14a7f0f7a9cda30505b6d3"],["/posts/1817748743.html","2bfacc467a87ba29646d8befabf10590"],["/posts/1925125395.html","1ad2fb28eaaf8d2cd492420912e9a119"],["/posts/1966191251.html","3ec0539792ae30d590f3b6c8c7ee0eb7"],["/posts/1987617322.html","be92587dafe3917af8314780fa2b93a9"],["/posts/1999788039.html","08035ae83c0b060d436494f58b41d393"],["/posts/2075104059.html","7afbf4a46c1a16e3386655c25b62c549"],["/posts/2087796737.html","54d124bbfd7d9c5e1d5e9b5e39b53fcb"],["/posts/2106547339.html","f90b99c71c846ffcbab94268956ed082"],["/posts/2207806286.html","723d7212a47e749391306cf8ec5f4318"],["/posts/2225903441.html","6837b3b66bc2edd9122eb35cbe6ec216"],["/posts/2265610284.html","3b24e9fcf4c8fbb2424693e3ca93ee5f"],["/posts/2281352001.html","f7cc39cf417177e9d47e0ff56ad81154"],["/posts/2364755265.html","c4319caa7c74f0dec5f31ca6b4e48101"],["/posts/2414116852.html","ffbc4720efe111222db846086543b61d"],["/posts/2421785022.html","6bcc72329ec559cbff1db75cb54e1d7d"],["/posts/2482902029.html","6390ab770c9733beeb5f1fe5e7d9a4c7"],["/posts/2495386210.html","2d962c908d8059b3362dbabefdcc62f9"],["/posts/2516528882.html","b90fcbb519ea283c3cf20d1ab22d6965"],["/posts/2526659543.html","c4e95086e941d03cccb3987f37530036"],["/posts/2529807823.html","0f65e4b6bae90fae7ddcbe8215a3d4b7"],["/posts/2596601004.html","c3dddb5b1e6ffe25dcf05e387af10ab8"],["/posts/2697614349.html","2b61c47e5abfa09a1178f3470f089ff9"],["/posts/2742438348.html","c82d513b0bb22be756db69433e574b0c"],["/posts/2768249503.html","9494dc127a5c7fbe1c89a7426c142854"],["/posts/2864584994.html","169076e4d6d69591e7b61714060685af"],["/posts/2888309600.html","404af96036aedcb8b81917c1c356e9ba"],["/posts/2891591958.html","ce185588f20fc9ee1ae282919e1c991f"],["/posts/2909934084.html","60484a80f7093a401711928defe39543"],["/posts/2920256992.html","2478415b05d9d4c9956aefca99b98b51"],["/posts/2959474469.html","fd71d3cfda04069472b3d09a97005694"],["/posts/3005926051.html","5c56ecf125aecc70137566ce2cf97d45"],["/posts/309775400.html","fb9b1a7b067f597be93892f7b6c24aa5"],["/posts/3156194925.html","e97dd17bb5a3076e7c635d23dd38f93e"],["/posts/3169224211.html","9a02f3777a4063d513f53379a2825d28"],["/posts/3213899550.html","a05b095e1e5d25a6ecd210591d393116"],["/posts/3259212833.html","a365da2b6b66b4ab701afde480ed61e1"],["/posts/3266130344.html","73c848775458764fe873f9643b98cb63"],["/posts/3292663995.html","e709fbf28cc9afb079671b917831a907"],["/posts/3297135020.html","dc40257ae1d40a7f7c3ea60837bcb0b1"],["/posts/3306641566.html","71b5b206bdd3da8fb996ae82e6c24ee6"],["/posts/3312011324.html","9caeabc0865dd946f2702305bf7a63bd"],["/posts/336911618.html","139c96eb0db397399fd15318b510dbd7"],["/posts/3402121571.html","dc4ef22a5b4bc860cffafdeb18f422a4"],["/posts/3405577485.html","ae22894e488db59035b2741802ae71f4"],["/posts/3498516849.html","de02c2359d47fefa895847bea95700fe"],["/posts/3513711414.html","f17f09bdf7c7394167dda477bb2e48f7"],["/posts/3523095624.html","fffa4e88c2c527c826ce8bbdae0594aa"],["/posts/3546711884.html","476d7d4b5937f640af9a2402d69260fd"],["/posts/3731385230.html","28f1ca7818e53742db6dd033e5b9d36f"],["/posts/3772089482.html","dd17edfd3b328cf2012eefd82c5c6bb9"],["/posts/386609427.html","14ec3c488821990063532c80c325c448"],["/posts/4044235327.html","55923df237859de60acd46aaa5c13765"],["/posts/4115971639.html","4052d508d97df184d018970abd742426"],["/posts/4130790367.html","d63569f55ef40acb7aaba494b0c7628f"],["/posts/4131986683.html","a823c7d9057fbcdf948ad4f986917078"],["/posts/4177218757.html","74c00c6947ebc0668caa664c82025cfa"],["/posts/4192183953.html","4a83ee86088ef5e1bbbd43c4b1b843ae"],["/posts/4261103898.html","fbb5d9e3acc647e5e129c232fff5805a"],["/posts/469711973.html","a6b56b87f828ced66c668322372c0901"],["/posts/482495853.html","b17c76cbd70cda5a9e9db0d1180b1b18"],["/posts/488247922.html","d08678c1710e653d6150051bfcb8566d"],["/posts/517302816.html","2c3b1512b779ec18232d4a2c0092fae2"],["/posts/570165348.html","9d7b4f551563244cfafdb130c8957d4a"],["/posts/595890772.html","55839dd68ad3b87edf24163c32588071"],["/posts/67485572.html","ca00bd2c4cbedf2112c6ac9a8b18b2d8"],["/posts/694347442.html","da59da4fb28db54ba21fe863f2fe2d9a"],["/posts/707384687.html","4e94c94118a5b0e610452b0b36d28e97"],["/posts/71180092.html","2bd8905f18bc349c874fde0169f5e9df"],["/posts/716459272.html","3c490fe9ebca5e729aade0061a986d9a"],["/posts/765481613.html","47c7d78cd9be1150c0a6233bd203246b"],["/posts/778231993.html","11647f6140b1a142a203f476b16970a0"],["/posts/795397410.html","fcb1ec3aab7bd9b0ccafba03e8bebb1e"],["/posts/820223701.html","9bf7aa8f4aab6d537144d2dc243ea3a9"],["/posts/830372185.html","053d0d95b9558d20b664d69560418a0c"],["/posts/88294277.html","5caa896a09e17f9f06c16ad81387714b"],["/posts/939963535.html","9eb5f6a5b796c9ea984de73f7f692af3"],["/posts/983786067.html","c1d519c7cacb08f806ef8ed82561cf14"],["/sw-register.js","a4f58ffc898c5cc09eb2e5dcf3a1af4a"],["/tags/C/index.html","e06d8e75f4aa6ea3c72b6b46340a87c7"],["/tags/C/page/2/index.html","0fd28e7f82a2d5df74f8ff668d01800a"],["/tags/C/page/3/index.html","59e1e730777dc51cfbc71167579b003e"],["/tags/C/page/4/index.html","540466035e442eaf5065cb4e6d929bdb"],["/tags/ETL/index.html","a27df559b187c0a0a421193f982cbb24"],["/tags/ElasticSearch/index.html","86227709a1b2620f4e30af67093611de"],["/tags/GUI/index.html","489ebc4505289cc4c343310b23b24bd8"],["/tags/HBase/index.html","be442d8454758485088648c8f46a4627"],["/tags/Hadoop/index.html","fc79fb99380c90ca43623964a6bbe791"],["/tags/Hadoop/page/2/index.html","7922875313cb4e9cc4ea2c27b4493d81"],["/tags/Java/index.html","c35e28bf238457f7eae921933c2c4f38"],["/tags/Java后端/index.html","5dd2f15b232540a38579aae05f29f465"],["/tags/Java后端/page/2/index.html","4713427d62a48a35de516dd9b3da1f2a"],["/tags/Java基础/index.html","d6250b0bfa2a440ed49cfc644e95e76e"],["/tags/Java基础/page/2/index.html","24ebfb64a3dc934531b57d01281ce4aa"],["/tags/Kettle/index.html","2d621e258b29d6cffe2857698412c1b6"],["/tags/Kibana/index.html","3679e1b8c0c6577c2a2109e524db0c22"],["/tags/Linux/index.html","18eda438d62fcf90866f80ee8db54776"],["/tags/Linux/page/2/index.html","22a85d3e18f9a259578c9393eccd4e0e"],["/tags/Linux/page/3/index.html","4264cea78a5d275ec1ec392726878b03"],["/tags/Mac/index.html","bb5cc1a0ed976bce39c22ab900879dac"],["/tags/Mac/page/2/index.html","be91881f6cb32cbddf44c542674e4de2"],["/tags/Maven/index.html","858912e7c400e8e28ed8593aefcd8db6"],["/tags/MySQL/index.html","a2220583892446ef19f77f866675f720"],["/tags/Python/index.html","58d023409923cecd449bdf58a2aa8260"],["/tags/Redis/index.html","b3f5b1a27ffd1c122b8b2f8327f460f4"],["/tags/R语言/index.html","ba3c1ee8217cd9d6e589a09bcc4caa3e"],["/tags/Spark/index.html","9392f904bc79d2bace2058f74339aac1"],["/tags/Ubuntu/index.html","a1c20466b28d8c5784f1e8140cf684bb"],["/tags/Vue/index.html","e9e7a92d3c97d0642158854df71f0cac"],["/tags/Windows/index.html","6f0527174df86c7d5d4639a95654ca3a"],["/tags/ZooKeeper/index.html","f8a444d51c5a67db30e34759b478b218"],["/tags/bfs/index.html","bd490774f894a16800df8b96f4565907"],["/tags/dfs/index.html","deeecabfaec84c0f7e9bc12a054e9b07"],["/tags/folium/index.html","c1c17a76ffa3d56e524d6859bd27a324"],["/tags/git/index.html","1813f1ed0213da7f41d317dae40dae65"],["/tags/index.html","06fc3826888276660771430da0b6135e"],["/tags/latex/index.html","0291a2a41779c607c41785630ede6df4"],["/tags/中间件/index.html","b1c9a2defb6343c17d4b573ebea0ca4a"],["/tags/二分查找/index.html","4849b34414be721ee00e868e827da55c"],["/tags/优化类/index.html","13d0b97d9bb803a1d01dc40e7008e46b"],["/tags/前端/index.html","7dabe821c4f201fd386d4a3851707112"],["/tags/前缀和与差分/index.html","3c1475e6eeb3425b0c097ffc862a0845"],["/tags/动态规划/index.html","3ebc17ec2bfa9821b637277826375fa7"],["/tags/动态规划/page/2/index.html","aaa1b17901b4ea03870360c9e04fa802"],["/tags/博客搭建/index.html","63c268482f1daf6c423a65354b175fa3"],["/tags/图论/index.html","061b02a73797fbbbe9ff70a376ad7aa6"],["/tags/大数据/index.html","6df6560fc7460cf278f9f9601282d435"],["/tags/大数据/page/2/index.html","0cfc2d44054d0cdf87a5c4994c8365e3"],["/tags/操作系统/index.html","70a43b3ad99d8ecccd482cc18fdaa573"],["/tags/数学建模/index.html","704a8c141938a8930d646333d87e2994"],["/tags/数据库/index.html","0b0ab0565bbec8724f203dac4fba1c4c"],["/tags/数据结构和算法/index.html","6a719748046b5170724c1087e6bc535c"],["/tags/数据结构和算法/page/2/index.html","43a35e9a737136b7b641e0613689b984"],["/tags/数据结构和算法/page/3/index.html","ae25d86e9a466c94671cf89debacd763"],["/tags/数据结构和算法/page/4/index.html","5e32460d181d4617a809475c5adbf5ce"],["/tags/数组和字符串/index.html","6337f527b2ec563159fdfbb51e57a244"],["/tags/数论/index.html","2b9ede541642f4f567846a436f48e869"],["/tags/枚举类/index.html","aab8453873b592d34a9ad06cfe562634"],["/tags/栈和队列/index.html","cecca90bdbc7d344b5c8fe1d2aed4fb5"],["/tags/树论/index.html","5a4c028c90dd1097aec9d9b1075d616f"],["/tags/测试/index.html","25012afb3dfcb447820a7ff7144a422e"],["/tags/环境/index.html","8cd2c7019600dde5c6b27c35b7408f5f"],["/tags/环境变量/index.html","bfa752f4552ce8c1c27fcc9e8d79b9ad"],["/tags/绘图/index.html","44ecaa78c772c9e049436f55be224951"],["/tags/编程工具/index.html","7467f0970860edad0218459aad971fdb"],["/tags/编程环境/index.html","c7226272b24a3657f92dd6d2f3e85d06"],["/tags/网络编程/index.html","2d3872b4a104730d4dcbdbfc8ea92869"],["/tags/英语语法/index.html","96bfbbe070a0c29ae11cf50c80b4aac6"],["/tags/计算机操作系统/index.html","bc1061de834d4c58d6d0d27ff0ca2592"],["/tags/论文/index.html","e4d35e8a204448b956d25ec85428ef49"],["/tags/资源下载/index.html","c67ebfcd7a9cb9a4b6eba737c40190cc"],["/tags/链表/index.html","fe9ef03693e77214cc7d4cabd4353be9"],["/tags/集合/index.html","651edbf1f45d39a03a092fef9383fcfe"],["/tags/集群/index.html","c759632a970ee41e05899b70792273f7"]];
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
