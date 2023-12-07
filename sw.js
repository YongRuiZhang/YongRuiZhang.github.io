/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","407bbeef9fb40a75992f568a63f58459"],["/about/index.html","5cc07c1790c21e8aaa4aacb14ec0482e"],["/archives/2023/01/index.html","b55fd6b0e2d2127476da388306d63b57"],["/archives/2023/02/index.html","5ed6e46573dba400324aa8fcf0d05702"],["/archives/2023/02/page/2/index.html","3c356b09dfe091a043c5e134fed9c384"],["/archives/2023/03/index.html","abf15747d5f065ac3b29ccb8cb87e557"],["/archives/2023/05/index.html","4550b5e1b11ba84561dd17f176781e50"],["/archives/2023/06/index.html","25d26826ba13114099023646cb3b9a54"],["/archives/2023/09/index.html","a350f673d618d2c4932e0b4cf076283d"],["/archives/2023/11/index.html","38fe04d449f706d27ec3aff733f61125"],["/archives/2023/12/index.html","979712be3ffa64780696aff5c9a3ea59"],["/archives/2023/index.html","caddee32b520f5ba591e4f15c95c8482"],["/archives/2023/page/2/index.html","c89c17f5d60c10be036d380702671c4d"],["/archives/2023/page/3/index.html","d634e7cc39e4a96e431b552ab9ac2b65"],["/archives/2023/page/4/index.html","85b8bb17df68e8de47f01eb445c2a095"],["/archives/index.html","0ffc9d0003ae742538b140f7b6a9d7bd"],["/archives/page/2/index.html","c3d057bb14e033621afd2bef9172ba8c"],["/archives/page/3/index.html","f8f37a93c79dfd98e2ec8123081b6ca0"],["/archives/page/4/index.html","e4596d019592c2abf7ab5aa0c4ec56c4"],["/baidu_verify_codeva-qQP2iZOMLX.html","2d0d32dde3edd0fd1409977e516c800c"],["/categories/Java/index.html","71d49b016ff63f7f3b53a1b34fe1766d"],["/categories/Java/后端/index.html","7430199c606aeeb186c03103ee33db92"],["/categories/Java/基础/index.html","9be9a3d1c82ec31bf2278c0b06e22fbf"],["/categories/Java/基础/集合/index.html","9c7a6d5f39a12debde63790f9b9072f8"],["/categories/Python/index.html","d6f42f66da823a1368b95e874c005608"],["/categories/Python/编程环境/index.html","601ff46cae65240c72196f58584032ed"],["/categories/R语言/index.html","555c7195626b257fb8b2600001b03729"],["/categories/R语言/编程环境/index.html","357d66907a977b4e05d1f00652b9fd00"],["/categories/index.html","ca9a1dd26543e37bcf76efaf87cefa8d"],["/categories/中间件/index.html","29b1e4f5e098758784947ad2608a502a"],["/categories/前端/Vue/index.html","b1a0211f0bf4bd1232adda9f9b84304f"],["/categories/前端/index.html","13a3d734b70da1488050939b63e36cd6"],["/categories/大数据开发/ElasticSearch/index.html","12df779634ec861b525aa4760f8d24b2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","255d4f660d4a29f23ef49d87de66bfdc"],["/categories/大数据开发/HBase/index.html","34dddb3a8e26cf589b9fd25771cd29a9"],["/categories/大数据开发/HBase/学习笔记/index.html","d9c61687bd6fc8c2a8034a4988787b25"],["/categories/大数据开发/HBase/环境搭建/index.html","d0b6e8495e486c1f502854b638512845"],["/categories/大数据开发/Hadoop/index.html","9ed16efbe9db5ea18bdfd07356ce1a84"],["/categories/大数据开发/Hadoop/技术/index.html","886e67e64dc72d98e32097bbf61e20f4"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d76df7a425c8e2d8b5088a79b25875fe"],["/categories/大数据开发/Redis/index.html","49cf21e7eb60a7ab920c9c042dfbdb9e"],["/categories/大数据开发/Redis/技术/index.html","2eee6f9d54c5e6c624642760067909e2"],["/categories/大数据开发/Redis/环境搭建/index.html","a69ea77fc397c39c50223805c591aabd"],["/categories/大数据开发/Spark/index.html","15d7ab79af69ee465dcb31e6953f5d84"],["/categories/大数据开发/Spark/环境搭建/index.html","7932d99ec63cb9281ff270d2963d4694"],["/categories/大数据开发/Zookeeper/index.html","fb5c9446cf1993d76de53cbd171ff63a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","58d6683e87acfe9ff995d5a86cc8f2f1"],["/categories/大数据开发/index.html","3adeb18eb2f2926c614bc65070446788"],["/categories/学校课程/index.html","be4013ae6d4ff49131bec3cd23a54ad5"],["/categories/学校课程/计算机操作系统/index.html","9270e7b080ead7c61886d8211fe267ca"],["/categories/操作系统/Linux/index.html","1bfa9ab9ba2ab18baa4f5c808644b08f"],["/categories/操作系统/Mac/index.html","ddb5c62764198da1c6e47f4aa4e6596b"],["/categories/操作系统/Windows/index.html","61227924aa6e0c2f812a56d7c808b26c"],["/categories/操作系统/index.html","7652e4308daf72006e273af064f1c191"],["/categories/数学建模/index.html","a50dd4f19b40db4617897ad4039eed27"],["/categories/数学建模/latex/index.html","6547088e9edf2afdb394213170ea864f"],["/categories/数学建模/优化类/index.html","14f73fe24f0f468a9109ce3c6253413c"],["/categories/数学建模/优化类/现代优化算法/index.html","954b9a5729c0d4fd33309a9f3159eac5"],["/categories/数学建模/优化类/规划类/index.html","194a25fa5bee9b555645cf9a4202f243"],["/categories/数学建模/绘图/index.html","ded2cf49b1a7693eb3411f87d03c8dd8"],["/categories/数据库/MySQL/index.html","bfcbca4c31602afa0b9fdffe2b4203d6"],["/categories/数据库/index.html","ff584e0eebfd3ab36f3b29dd25ee02fb"],["/categories/数据结构和算法/index.html","051a130ce5b55ef7085112f73cd990d3"],["/categories/数据结构和算法/page/2/index.html","5d19786a3c8eba103676ef3ab2ed14da"],["/categories/数据结构和算法/基本原理/bfs/index.html","0a40f4611836328e40b74b84953d76d7"],["/categories/数据结构和算法/基本原理/dfs/index.html","048d4ad25664e4c35a0c36e39d9b0f23"],["/categories/数据结构和算法/基本原理/index.html","a7d481968fc035fa21c5e13887ba6a0b"],["/categories/数据结构和算法/基本原理/动态规划/index.html","5210057d8f05b121e27c40a992f890e0"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0db524a0c65a20ee77d34a9169c8656c"],["/categories/数据结构和算法/基本原理/图论/index.html","d2e34b3102744d0f91d3ba92459531ee"],["/categories/数据结构和算法/基本原理/字符串/index.html","ab70315078dc251ce98aaa239b79b176"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1717fa20269a6bc87bb944f15a8a51cd"],["/categories/数据结构和算法/基本原理/数论/index.html","665c9b4ca40bfee5779b203738c01487"],["/categories/数据结构和算法/基本原理/树论/index.html","2f73318d7decf9e451f595b6600124fa"],["/categories/数据结构和算法/基本原理/链表/index.html","212149ee3229b0b29c724c415fc1afa9"],["/categories/数据结构和算法/算法题/index.html","73fcf6ef31e654f77595b6cbd1176d79"],["/categories/数据结构和算法/算法题/二分查找/index.html","d72ae754db9bc738dbbdb7c63c386b74"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2b67659558cdac1bb5e9293d180237cc"],["/categories/数据结构和算法/算法题/动态规划/index.html","4cca1d2ad06b50b6b115e50c39575ef5"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","fe1ecf842225ff00d9baf4fe4ccf6b30"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","cf2d8c8a3bae1f3c9309288a64c908f6"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","029b1f08bf0b98e17aadf47bab3a2efb"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e4b142a9fb6d6438f4a9cee1f8f4ae5d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","2fdc4a4f9365fa660f7fba26944191d9"],["/categories/数据结构和算法/算法题/树论/index.html","097f631dace9809375bd4362c235a586"],["/categories/杂七杂八/index.html","f2f4bfa6c6a4214919cac2c8eb1f69bb"],["/categories/杂七杂八/博客搭建/index.html","d4e677f7ef71d2d641001c308ec53959"],["/categories/编程工具下载/index.html","7b0cbbefc67c2eab2d81a7e483a4d4f6"],["/categories/编程环境/index.html","50cc5a4690be3400d2fe9c1edf9a514e"],["/categories/编程环境/大数据/index.html","9c512c33e57c807dafa656c1171c331c"],["/categories/英语学习/index.html","1080ece7e06bd1b2a03e5e0f0c89322f"],["/categories/英语学习/英语语法/index.html","797a87193602d53889e379a49cd8d1bd"],["/comments/index.html","fd10083a2bfc363b6bdd8545d31d66c7"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","db968735abc02a58cb7df6415cb4be33"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","17e41c394ec22b55b52ab153279fa153"],["/movies/index.html","fa6a2e7e6cbdbe8dac6b67c8746014f7"],["/music/index.html","57823d0727f7aafb0a333c1a71cb9942"],["/page/2/index.html","fb5d0e9d589f14cbfb7e748d26ff2c22"],["/page/3/index.html","b9a6b694e471f5fbc5116f803a5a1efe"],["/page/4/index.html","a564b05ef5a7615363a7d9f402380d71"],["/page/5/index.html","bcdf0aabb342d38314c744b5f08e0f5b"],["/page/6/index.html","5186e02e2c4b81def404031eb7070144"],["/posts/1021360842.html","0520d54ead312f899b5b40db16473d81"],["/posts/1120620192.html","66355592141a9e40dd414f50810900d7"],["/posts/1141628095.html","135b704247143f0ceaa10e54962f422e"],["/posts/1168613674.html","34b2458eda72d4a7667b025b33077977"],["/posts/1219920510.html","260f307b33211b4acc0d8b394e983bd2"],["/posts/1222166338.html","8313fca9cdd1dbdb9245105c02d2bb75"],["/posts/1259097482.html","cbb68620ec8a78dad43ee8a42e9fd2dc"],["/posts/1271036369.html","2871c33ec1244ac15f13ca13ec530287"],["/posts/1312847445.html","2660ce4597a7f08380c86126bfd01b41"],["/posts/135355774.html","3da63224346f04328a4935537e6c7594"],["/posts/1375344716.html","f59810c8568f13957f81228dfca58721"],["/posts/1388991698.html","00cfb5da82fb564efaf075c9ff338bfd"],["/posts/1410315814.html","c23d922ad299569486b3b1a9415aab46"],["/posts/1452790229.html","4b4657af31562b9507694aeaa7afe3be"],["/posts/1470079884.html","2c2b60c1691219edb64007d7dc201558"],["/posts/1470079885.html","f796f95cd8b38040f4f06114666bfab5"],["/posts/1470079886.html","df12afa55b2a01f077cc3212c43d37e3"],["/posts/1470079887.html","3cea6bd8a8dec42e7971ffa81ba821ec"],["/posts/1498536549.html","852e96d93cb3145f4ec5e5a86888dee5"],["/posts/1547067935.html","ecc3d1e1add50901b8e18b8bddd15cca"],["/posts/1557866301.html","b66b3e7a57abe210cb3dd5853e29b6ae"],["/posts/1571776361.html","6981c612f9633bfefa3c5e98db785456"],["/posts/1605124548.html","40e4f1b5d8f521a346b3a0e6c7fa074d"],["/posts/1633036852.html","f19eed30870a83d9767e3c38f2d7e9c0"],["/posts/1674202625.html","cf6fd3ada5dee1084428cbcc280b7c0c"],["/posts/1765123828.html","d0eca332cb56b7e989089d12502c2f52"],["/posts/1767336200.html","bd4480a3b4541a72db22d2e69b0df304"],["/posts/1776114197.html","dc7f12ef1d4a22623388ffc7fe4ffa1e"],["/posts/1817748743.html","1e6a56526e9aa5c55798aefffa5063dc"],["/posts/1925125395.html","5cc7599511bd1c71ecb2647f94c8b639"],["/posts/1966191251.html","e61138b4523fb8bb49b2c1ba7201536e"],["/posts/1987617322.html","b41ad35b0c60618564a59e0d625e0381"],["/posts/1999788039.html","f7eea95906dcc5ce3f13c050726b8dd3"],["/posts/2075104059.html","0a4f8339b0b51f9c8a46255da3c56f2b"],["/posts/2087796737.html","1cae67c0574de2f57359e6001e476345"],["/posts/2106547339.html","9e79d0fbc9ee2c3ab4436c572eaa338d"],["/posts/2207806286.html","f66d3270ab164195329612bf688ad907"],["/posts/2225903441.html","f5dbbea082c613d446725a6d419c7cf3"],["/posts/2265610284.html","264ac5525f10125f2f83fad31186d3ad"],["/posts/2281352001.html","40107d278b16ebd0daf782be8edbda60"],["/posts/2364755265.html","bf93dac89205057bb733a31092d50707"],["/posts/2414116852.html","34cdbd68ead60b8aa45e42c8c19bf3ac"],["/posts/2421785022.html","8923e161fe16455a5d44fec98ea6c9af"],["/posts/2482902029.html","a4f03673ab85fa96e66744da431614fc"],["/posts/2495386210.html","fd33a740536ffe6acdba52e042b4d662"],["/posts/2516528882.html","24d1aeafd940d5aff2bd16a8d963c66d"],["/posts/2526659543.html","a132b3fabe557312c416cddca2b37593"],["/posts/2529807823.html","c996dab8f340c244e07e1afd6376dca8"],["/posts/2596601004.html","001b2e6be292c0429456d5dcdd417324"],["/posts/2742438348.html","865720109797c08daffab26f1cf3af7f"],["/posts/2864584994.html","3efed5730c34c8897f255b4ebbf18220"],["/posts/2888309600.html","362d0c79abb667f2940926f0a38a623f"],["/posts/2891591958.html","4d41c4f3677caa110f76c2533bf8d7d5"],["/posts/2909934084.html","9861346e7f5f7841c3a3f4829125bbf6"],["/posts/2920256992.html","a751d1ba6f88b02a6e1a9e2eac867044"],["/posts/2959474469.html","6943ebb7fe95fd6d9ec6395e18b3e6e5"],["/posts/3005926051.html","6f4ea950dea1742b2bc24e27300cc545"],["/posts/309775400.html","c7794930f16f3f16518a66eabf697773"],["/posts/3156194925.html","9d43844a2dcca25f9b0cc9be64f87046"],["/posts/3169224211.html","7b8e7d90bbbb35181e89ad418846c67f"],["/posts/3213899550.html","9c9a51d5a84353c04ecd70e40338869d"],["/posts/3259212833.html","42eded3cd0452f63ed1f866dd24f568c"],["/posts/3266130344.html","1b5e72c1007f75a7a4e1a39a1651e194"],["/posts/3292663995.html","9880a8fde3e2df5a5fb534902acf357d"],["/posts/3297135020.html","070b6fbe24e9e3f56c4ceb810c257d44"],["/posts/3306641566.html","73589f16d82aaacceaba6e5493adbc2d"],["/posts/3312011324.html","ca6c3a71077fe4772e448e4376b6456a"],["/posts/336911618.html","b9c97283df3ec9515cacc00d2f1bf335"],["/posts/3402121571.html","891e7bb1fe04c8e2bec5a3227ca2b427"],["/posts/3405577485.html","27026c529f6f40983c49d1053bf6ceae"],["/posts/3498516849.html","417968974e21a342a407ccb7e5bfd36d"],["/posts/3513711414.html","9be4769769203afcc26e6d64e49267df"],["/posts/3546711884.html","6f44e60ee89e8b1f86d16032441725b1"],["/posts/3731385230.html","8f72ee0a014cb0cd03da689fd5b0ee4d"],["/posts/3772089482.html","3a8a917a114858e21368fa9c41a32b58"],["/posts/386609427.html","8703a9b22c5450a3bbdc398085511dc7"],["/posts/4044235327.html","24b2fd7c9d4f997385aed0b1600c2d88"],["/posts/4115971639.html","25f42862badbb661d52a70ba1118a656"],["/posts/4130790367.html","830ba4233c4c2e873abd15e8a6b78e8c"],["/posts/4131986683.html","41b79a569a01b60c4c4bde7bfcb7476a"],["/posts/4177218757.html","11d70a0f11604d611970962799f53bca"],["/posts/4192183953.html","6f1541b87851529e0917b8657ff7df62"],["/posts/4261103898.html","98336c2b40a3df076fd479468ccd2cc2"],["/posts/469711973.html","575644c67f090b81e4e0a7ee30f5ecd2"],["/posts/482495853.html","ab80bb6d1fb178809b5e52bd86d09084"],["/posts/488247922.html","41f848543be22e9036855047673659e9"],["/posts/517302816.html","63e85b6d4666b0d43a3c931088451127"],["/posts/570165348.html","9f8789ff222d1fcfe0a7a67fc532c54f"],["/posts/595890772.html","3b6d3753b8330fade282ac394edfbb61"],["/posts/67485572.html","18a018278adaee868e06d5fdd1868bfc"],["/posts/694347442.html","454220b7e050202e811191ee92d024d2"],["/posts/707384687.html","983c02f30859f0f4e843ad59a8afb7f2"],["/posts/71180092.html","209882fe03ab63603e8a1b1f9e21886b"],["/posts/716459272.html","a214590a30ab4ccda1d376384ccdeb2d"],["/posts/765481613.html","825d611cd4f167a0bb67e653c288b16a"],["/posts/778231993.html","96ced55e10952f6c77c3f4d13b2c522d"],["/posts/795397410.html","18bab38ff418fdd87790922738cdd3f7"],["/posts/820223701.html","6eaee77b8d87e143067b809955ea07eb"],["/posts/830372185.html","a7a12e21c51b9843b80eb431ad575ae2"],["/posts/88294277.html","d292fbe67c59ae020cb3231f61f80819"],["/posts/939963535.html","ee82e8633158d548d3b89474ce63dcdc"],["/posts/983786067.html","e8a2cff0d3949d9c6bec5927fce69186"],["/sw-register.js","29c3279fe9c1b863b2487c211d9a644d"],["/tags/C/index.html","2f99ddd535edab8f6e8ddf28470d738a"],["/tags/C/page/2/index.html","3b7e2df945a78b3f1773a9b7b76574a4"],["/tags/C/page/3/index.html","94c92694a43b1bbed3b28b4ada0cfffb"],["/tags/ETL/index.html","62489014ff1f32f2152d396fa499aaa8"],["/tags/ElasticSearch/index.html","ba604baccef356ac6fb0d4c6db7ec80b"],["/tags/GUI/index.html","e56a45b490a498b13e3acdbb47ee294e"],["/tags/HBase/index.html","9ea74561d692e84188d4ebe2122309aa"],["/tags/Hadoop/index.html","5587f520ccc9073891994f0f458263e0"],["/tags/Hadoop/page/2/index.html","b7ab09d359de2d8b392b62cc7dc172b5"],["/tags/Java/index.html","ab715d04eabd080842deb9f1c7014875"],["/tags/Java后端/index.html","20fe267cb604851aa69532c57d8f3f88"],["/tags/Java后端/page/2/index.html","529d0039f55702d88d7e5393ca643ff3"],["/tags/Java基础/index.html","13cbc11d2358e07990835a50702dcb1f"],["/tags/Java基础/page/2/index.html","4b593f0abc01e84afc41431a250bf7f3"],["/tags/Kettle/index.html","d06f9c6267f4b49d54edc4effc96820c"],["/tags/Kibana/index.html","56073c6a9f381e1e4ba50dfa33757d92"],["/tags/Linux/index.html","d67c77f0bec084b1532f6343c843fdec"],["/tags/Linux/page/2/index.html","6f4f041fc2c78692cc1a7be5d12c2696"],["/tags/Linux/page/3/index.html","eaf47cee6995d37698bc0786cc1ba4a5"],["/tags/Mac/index.html","06691342509db0ba38c48e7c7e51b6d5"],["/tags/Mac/page/2/index.html","6b10b6399141ac58e7d117bad844e7c7"],["/tags/Maven/index.html","eec2a3b45403a7f0da8d5af25983c98d"],["/tags/MySQL/index.html","f012cfc0661b46ddcd80fe19485d77c8"],["/tags/Python/index.html","b99df84d6187e1abecba2c3affe2ae7c"],["/tags/Redis/index.html","275ed9ae05deae57371f3689ef9a1395"],["/tags/R语言/index.html","c366387c7814dc75bc0065b2f9f2686b"],["/tags/Spark/index.html","083e274645530e35db2a8dd3af074b08"],["/tags/Ubuntu/index.html","5cbfc5b5d1a7023212c671ff41958331"],["/tags/Vue/index.html","389fd96b4f5f4feb61b822e0d661d2a5"],["/tags/Windows/index.html","e6151410e44bb7f38ad8379f6fa8fec0"],["/tags/ZooKeeper/index.html","151cfc4f949edc7e604ae359a273ce4f"],["/tags/bfs/index.html","b62eb95b2b93192218a9b4ffc9752ca0"],["/tags/dfs/index.html","90f5496328c225f9b0a3249c89e11b6d"],["/tags/folium/index.html","ad17561e1fedae8d45b4758d1096e593"],["/tags/git/index.html","69c016bed347991cc4ef83456ab64cdc"],["/tags/index.html","37f61df076d498064de1468e0b6e70bc"],["/tags/latex/index.html","1d6617d0973ba06aaf31c52c13452a62"],["/tags/中间件/index.html","37815eac06e4014e69fc6294ee630ca2"],["/tags/二分查找/index.html","6082c4049a57389e7a66380a958886ee"],["/tags/优化类/index.html","0bb549fd87e4d9274dad149a4aef23e5"],["/tags/前端/index.html","be45c804b42535910da10ffd82bb4abf"],["/tags/前缀和与差分/index.html","6897b47c9cb088cf69c4d16aa4a07bf4"],["/tags/动态规划/index.html","46add278230b9b5f60edf2c18ef1329e"],["/tags/动态规划/page/2/index.html","c9fb4d563ef05211d3ea07d32aba80b6"],["/tags/博客搭建/index.html","76d6d52a96bbdd8cd9ca46ee0ddd0f62"],["/tags/图论/index.html","26a9ac3fff34429b481f24faac2d31eb"],["/tags/大数据/index.html","2dafa4ac00dc03f6d206df5b7fe4b431"],["/tags/大数据/page/2/index.html","a4ccc4e3d5a3091a4c9e59aa172c7fb1"],["/tags/操作系统/index.html","6760141061bbccc28126bac934b994bf"],["/tags/数学建模/index.html","8489e1f3e20c7756f15e4f3c05bbfeec"],["/tags/数据库/index.html","223174a297e7a1bf225b3174c2965822"],["/tags/数据结构和算法/index.html","19958dff8d254be99b26efa5c13e201e"],["/tags/数据结构和算法/page/2/index.html","4616bedb9853cf76b71af3060dc7703c"],["/tags/数据结构和算法/page/3/index.html","77ef4bbda18254c879f2cd9b05af0dde"],["/tags/数组和字符串/index.html","ad0e095cd5d7487c2dbe83087bceaf00"],["/tags/枚举类/index.html","8da51ba88f258f347b22a1acadd42ec4"],["/tags/栈和队列/index.html","a58edbf39e3374a089a9406177ce0098"],["/tags/树论/index.html","9ece0771e8196be92e634c9abc57d17c"],["/tags/测试/index.html","f5bbc4273d871bf5f9202af995864b66"],["/tags/环境/index.html","69447d397a06ed2c166c56021ce3e4a4"],["/tags/环境变量/index.html","2aa61275088fad798441bda5e1978f9d"],["/tags/绘图/index.html","15ea6e13d8043b361b865cfa61d7184c"],["/tags/编程工具/index.html","693af7cb699bdba3f4b3705f4b589080"],["/tags/编程环境/index.html","a103612b6e7a97342505ccf410bc68cd"],["/tags/网络编程/index.html","e2f01a10ab7d905fd20b6342a4f5c118"],["/tags/英语语法/index.html","17b5164af3ec0af65852425242b8ada7"],["/tags/计算机操作系统/index.html","ea5958dd91cc00fd37ac4e04f8b7bd5e"],["/tags/论文/index.html","9f3c30ae203497516443aaa3e56564ee"],["/tags/资源下载/index.html","9b8c93dfe16006f57e5fa37978b88d69"],["/tags/链表/index.html","568c450721c2a13957273dc848ef9783"],["/tags/集合/index.html","4ccb1ae84a1951dc255c6b5c4c115f06"],["/tags/集群/index.html","06e14bf0ea7fa89527826e75199f0a85"]];
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
