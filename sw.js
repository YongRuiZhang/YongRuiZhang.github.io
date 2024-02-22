/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e45097ce75bc866cd253af5d91a8c9f3"],["/about/index.html","638ec9bdf44a97e00314d5da410cb077"],["/archives/2023/01/index.html","30897bb617430424f367a6f4eda03d39"],["/archives/2023/02/index.html","994996ee3a9aaa5575eedb2079a5c053"],["/archives/2023/02/page/2/index.html","d202ed30118cde8d99568d65a7ea535a"],["/archives/2023/03/index.html","cfeef446c7324296342d144c432ed7ba"],["/archives/2023/05/index.html","60086772372bcc22aaade6856200c734"],["/archives/2023/06/index.html","20be882aa90a3d1744fca526fb688f17"],["/archives/2023/09/index.html","865ef1a08f48bddf1d916a085f52ed02"],["/archives/2023/11/index.html","f8a7756c1cc9af2c953778b62c576fec"],["/archives/2023/12/index.html","24d7b9ca205409ef2fca81d0d42b972a"],["/archives/2023/index.html","4d86a57c05a6113bf2a6eb8952c91c8d"],["/archives/2023/page/2/index.html","99bb6240d839fba941725b9f2b8e021e"],["/archives/2023/page/3/index.html","af09f9da2b9b7c3ee931cd8374f7a537"],["/archives/2023/page/4/index.html","b0723c6f23b85c786aea97332f2605f2"],["/archives/2024/02/index.html","99eeb71ada48235a48240d9806e80182"],["/archives/2024/index.html","cec4f446bf09422e8dcfa0337d24f2c5"],["/archives/index.html","ce09e9d037d5ef0ef85e14949dedcd71"],["/archives/page/2/index.html","dc0e5de73cd04b1769771c408aaa23a7"],["/archives/page/3/index.html","00b07df8703f302f96b93df40fcbf3ff"],["/archives/page/4/index.html","5e41b5b86ee63802ebf4b272cdd71b0b"],["/baidu_verify_codeva-qQP2iZOMLX.html","4cf4417944b6c8534127ec75064a7630"],["/categories/Java/index.html","d908459995c5ea42b52ce13011fa3bb1"],["/categories/Java/后端/index.html","92b3a06aaf9a820000614353fdb10a38"],["/categories/Java/基础/index.html","aceffa6632a333231c9ab2efad975987"],["/categories/Java/基础/集合/index.html","47e78dbbb4db32b465d7809f0e4372fe"],["/categories/Python/index.html","c80f434da23d5b1e286567ee8cda7446"],["/categories/Python/编程环境/index.html","7121fe2624add0e88d6dda4bb096d1a7"],["/categories/R语言/index.html","b8136a0cadb527f66dd6b8e9dcbcb705"],["/categories/R语言/编程环境/index.html","516ac72bf6caf75eabaefb4372382f63"],["/categories/index.html","43548fbc2c9cbc5b7eaf19e66fc4d40f"],["/categories/中间件/index.html","cac9393e37af0110946b3afa8087c551"],["/categories/前端/Vue/index.html","4bf79749ca6765f14f4e9ac5c8a9e74c"],["/categories/前端/index.html","c2af36b0913d518e2d9e231e537d0a4e"],["/categories/大数据开发/ElasticSearch/index.html","551973bfd9e10cb2f56678bc310c0d05"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","b9eeac9c92fb0a9be2187b42ea6747a6"],["/categories/大数据开发/HBase/index.html","75784c6738865d63cf8ab1a5fe939f32"],["/categories/大数据开发/HBase/学习笔记/index.html","542546e0488ec4f6b6e48110290fcfa8"],["/categories/大数据开发/HBase/环境搭建/index.html","30982eed08f534fd6e34827260469edf"],["/categories/大数据开发/Hadoop/index.html","68013a64c0b363475ca7ca2437783d59"],["/categories/大数据开发/Hadoop/技术/index.html","52ef4414b1a7f25fe3cf1c207d5a912d"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3244e273186f43369c0dfab30a79b01f"],["/categories/大数据开发/Redis/index.html","32c75122b9fc609a3e1c65bdaca1af0a"],["/categories/大数据开发/Redis/技术/index.html","cec07334b26d2a4f17e48bb8a72ed2c3"],["/categories/大数据开发/Redis/环境搭建/index.html","dcb67903e353e8eb43d9638a252c95f3"],["/categories/大数据开发/Spark/index.html","9ba988a26bc10844a134261d2e69e0f4"],["/categories/大数据开发/Spark/环境搭建/index.html","39e1d3cd862b699186513dbbe8a605c6"],["/categories/大数据开发/Zookeeper/index.html","94b79fdb796146f97263c05bd21a8b37"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f75ee3c353eb5485ea14996a28875c50"],["/categories/大数据开发/index.html","082586827fc9f17798596b56a48db609"],["/categories/学校课程/index.html","9ae5a119c57b770f00b3637ad917b4f4"],["/categories/学校课程/计算机操作系统/index.html","ccea786b1475870bf6ccb68bdf9da9c7"],["/categories/操作系统/Linux/index.html","816e98fbf3b68808d9ea87c8f424ae55"],["/categories/操作系统/Mac/index.html","df9bd496f3b35ed3848a8ca3c87bbd9c"],["/categories/操作系统/Windows/index.html","e2230800ad6684b55456c11c9331702f"],["/categories/操作系统/index.html","c610ba90d3a10ca3911ef486d25ee592"],["/categories/数学建模/index.html","2ab104bad2608e8d47746a87633209b5"],["/categories/数学建模/latex/index.html","8e6aa5e43643049c903d8b64c93e819a"],["/categories/数学建模/优化类/index.html","e6d58f3b7a5d99a84e8d0f6a805ddd1e"],["/categories/数学建模/优化类/现代优化算法/index.html","c0c03f0eba3237a76d3afbd9d012e590"],["/categories/数学建模/优化类/规划类/index.html","77940ddabb8d8184c6876a6cecc7c3d2"],["/categories/数学建模/绘图/index.html","b4807756b7900f598c51aa83394d4556"],["/categories/数据库/MySQL/index.html","d184042676dd4df8b7f94f3020a3efd2"],["/categories/数据库/index.html","77838913197ce1384266a01655afde8e"],["/categories/数据结构和算法/index.html","0aa4e39c3aa4831c07b75d13d07fef25"],["/categories/数据结构和算法/page/2/index.html","89a842724fed1cb85189fb78d18c893c"],["/categories/数据结构和算法/基本原理/bfs/index.html","46b3e8ef3edae23872dde4b26ba5701d"],["/categories/数据结构和算法/基本原理/dfs/index.html","6b1dd41b53c27b58056ffe27bd49fa97"],["/categories/数据结构和算法/基本原理/index.html","29ea8705dcc9ad9131b285fa7a269ba3"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e0018c5e6cfbc250c508835aeb4b452d"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","c14e17a6ae7075dbc775358edc84d294"],["/categories/数据结构和算法/基本原理/图论/index.html","de15d0c48a42d971a9fefb212f968ef1"],["/categories/数据结构和算法/基本原理/字符串/index.html","8d236fc208c1148d224fbfd45dc3b563"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2d1113f56ca3249ebea572ef9ddb5af0"],["/categories/数据结构和算法/基本原理/数论/index.html","eddbb63ce2be8acfcd1ba81e258578ad"],["/categories/数据结构和算法/基本原理/树论/index.html","d1c8b366f58819da958975c4143023d9"],["/categories/数据结构和算法/基本原理/链表/index.html","a24a3f87d308cbcf4d7e46fc528b2cc7"],["/categories/数据结构和算法/算法题/index.html","575e4e89b1ae9fd46d7424010bcaaf6d"],["/categories/数据结构和算法/算法题/二分查找/index.html","8b23d234d1c86de11bd7f6408eed3777"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1b80d341d1266eba503b1da785581da8"],["/categories/数据结构和算法/算法题/动态规划/index.html","52657bee803978bd801b5e9e0b253879"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","90b1451eafee0295742a4f0a032787ab"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","ec1a9c355b5dfe98ca71ca44a1ec002d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","2f39a1a44835005f7a1606927c8469c7"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","9a45f8003fbc676d945759120430934f"],["/categories/数据结构和算法/算法题/数论/index.html","6d3691d60782b8ed09acd94d557550b3"],["/categories/数据结构和算法/算法题/栈和队列/index.html","a74b3a809202ed589db0809bb37a97e2"],["/categories/数据结构和算法/算法题/树论/index.html","c14dc7a43dac33b5b257e318749d5a63"],["/categories/杂七杂八/index.html","3a093d83e53c3e6db632783adbb1c15a"],["/categories/杂七杂八/博客搭建/index.html","b1d6418798bab5ddb7b14dd8b8763a67"],["/categories/编程工具下载/index.html","3851782e3b1bbf38ac0284c7e920068d"],["/categories/编程环境/index.html","25ccb6d5ddabd8a58294f17a100d88d9"],["/categories/编程环境/大数据/index.html","e13be6f2d6c54d0327e1ac5e9947d031"],["/categories/英语学习/index.html","b17047bc162438b25741ba02751ac1b9"],["/categories/英语学习/英语语法/index.html","9be511258aa5a2566fbc499655cae8a0"],["/comments/index.html","81f4436360973f646f571d58de981ae6"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","c96337d1b6ead12cd775d65d1feddeaa"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0f09793614f1c812e6cad264f820b2b4"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","8fd4a77863aee028c61bd5624c557ab1"],["/movies/index.html","5ec564ae19f18cd0cb022b7a9594f903"],["/music/index.html","4911330083b02cd798f2b508ed4ee78a"],["/page/2/index.html","06a40748d52023878445ebaf4b0e255f"],["/page/3/index.html","a33cd53d4c14e9443e86ebc9f4f84374"],["/page/4/index.html","005540d3387feb1ff4e8cb73d3fdac92"],["/page/5/index.html","db28dbb81cc01edaee2a6fe3672c623e"],["/page/6/index.html","a04878bdf18e2b0816f435c897e8e94c"],["/posts/1021360842.html","234eb0efbdf03e43a4e5b17d8fdc57ec"],["/posts/1120620192.html","ff12b4372418331a38a35841a677119a"],["/posts/1141628095.html","805e2407bc1af6804464cad568b5e486"],["/posts/1168613674.html","67a742e207a48af0177c9a559f584531"],["/posts/1219920510.html","be475662569ea1d9031fa732b32fac3d"],["/posts/1222166338.html","c7aee23fc349a4ad6ae8d6822993c3e1"],["/posts/1259097482.html","8043f749e2823ed04521e490acfedc7c"],["/posts/1271036369.html","a86034711b89640ccdd1c959515d7685"],["/posts/1312847445.html","116d20ba254f0fadbd085d5064fbd41e"],["/posts/135355774.html","e02a055a62f39bf48d47477662a2021e"],["/posts/1375344716.html","a852e5e4c6e0b45e4157c2810cc0ee8c"],["/posts/1388991698.html","efe623848f6abf9b662a05daf5f4b246"],["/posts/1410315814.html","4fd68bde754d9a672038d266ee5fed44"],["/posts/1452790229.html","1b24f008b88d096013d28cbba9221504"],["/posts/1470079884.html","a1a67f06e383bd8e3e6e46f25c2de4a5"],["/posts/1470079885.html","1cf51bd9402d51c3efbe4656b58acf80"],["/posts/1470079886.html","47bde3a9f15fc20186ad85cefcb04d19"],["/posts/1470079887.html","958fc31f5cac6546878e07e696ebfda2"],["/posts/1498536549.html","e5c8fbf600e4a5e504d36f340a2bd582"],["/posts/1539568593.html","dcef27590e8287db3b3a81f6339a2ef7"],["/posts/1547067935.html","4646a46a3bb113831ba7201bfc734c1b"],["/posts/1557866301.html","de5b351ddcfcb7a0e848a921f6db7f8a"],["/posts/1571776361.html","f9c7b31df07aa53ed0baa27b408ab98b"],["/posts/1605124548.html","f89e678cf04107eb6f33940e2d5f003f"],["/posts/1633036852.html","47bcd2f8191584b75cde0552ffa1044f"],["/posts/1674202625.html","02fa36b810cb4bd5e3d897d2c1a9af54"],["/posts/1765123828.html","0e79362a09e6e208ceaf83416628230b"],["/posts/1767336200.html","ff23cbc8edb9c9ac381cf7c85f70b0d6"],["/posts/1776114197.html","a8596a48c20c601fc25958f94420b985"],["/posts/1817748743.html","2063e1bfed76ef399d0dd858e9b544d4"],["/posts/1925125395.html","b3bde16a3d0fbb4397103a768fb15d8b"],["/posts/1966191251.html","99450c75ab0c7605364847d7dc835d40"],["/posts/1987617322.html","fbb45d7ef02b5db020e0fad331fadd8e"],["/posts/1999788039.html","b6466345d50edbd2bae61890594110e6"],["/posts/2075104059.html","288eee1468e05e50fff4ae152c0e6073"],["/posts/2087796737.html","369a423a5eb45bca41bfda621aedfd56"],["/posts/2106547339.html","7eeaa5552f2309cabb04de2d158db4ef"],["/posts/2207806286.html","d1939272c1f1178b7ab58738e361c947"],["/posts/2225903441.html","21b0f2f74bbd61461574692cfa445b03"],["/posts/2265610284.html","6a3d93fbc7cc2cb20b5072cf8f9212a8"],["/posts/2281352001.html","596c2789a1891bb32293856db89b7cca"],["/posts/2364755265.html","75f6103860734fec6318e77caa9d55f7"],["/posts/2414116852.html","eb3f78e241c84e4b14f6d552ca8bf218"],["/posts/2421785022.html","3360c417638feff8c6e28fb943545e24"],["/posts/2482902029.html","35b677c7d44616d19befd3bb1d7c869d"],["/posts/2495386210.html","0abbd39727a09a749131f331f5b05ea1"],["/posts/2516528882.html","68c0bbdb0ad0eccab0fa7990acb30dae"],["/posts/2526659543.html","3bcc84182a516f8ab9a033edfdd4969e"],["/posts/2529807823.html","df1f419fdc8768e617082a2408172a7d"],["/posts/2596601004.html","76f48c5f0cba1a75a9a51f67366fd96f"],["/posts/2697614349.html","d4d22fb3a74216001a56ee8fe3037068"],["/posts/2742438348.html","dc8e2444415a3f7cac74d4ef5f47ea83"],["/posts/2768249503.html","34168f4dcb9fbb20f14038699a37c4a9"],["/posts/2864584994.html","d8789a2a34f69462ac56d2723937dfa8"],["/posts/2888309600.html","a6cca8c8479f91184edfee4c63d4c8c2"],["/posts/2891591958.html","e6d4562c4458134490762ef020f63ec4"],["/posts/2909934084.html","145f6916e77e98ec1e5ad265b21cdc66"],["/posts/2920256992.html","ef1149de83f16918bf36b8a068130c44"],["/posts/2959474469.html","303af190546af3861031e921e6686030"],["/posts/3005926051.html","cb1cdbe9553c3f220e3300121445089a"],["/posts/309775400.html","18ce3ab7b2e516f29388848227b9089c"],["/posts/3156194925.html","a48057c83a00c95e1abd8005e7ccc72c"],["/posts/3169224211.html","b909c1aad3705656057600e56dc4f483"],["/posts/3213899550.html","561b4169e0789b4067e57447304084e0"],["/posts/3259212833.html","d50859075f59da756d8dd7d2e7a3136a"],["/posts/3266130344.html","5b5133911970ef152989b839da05bf58"],["/posts/3292663995.html","ce3583953dca7d8cb7a82c76a98a8bd1"],["/posts/3297135020.html","1f391242b16d4c0bb43e3ccf026a6d52"],["/posts/3306641566.html","8c301911d7557933567d6e8b7b16d0da"],["/posts/3312011324.html","ca3124c0a3a006c5b71a21389e8b6031"],["/posts/336911618.html","eb2c71b139b7d2ef223efe11e1451a71"],["/posts/3402121571.html","628a503346b74764a61ecd549c3581e3"],["/posts/3405577485.html","8e2f75fd3e2562e6e12ac7953d98d400"],["/posts/3498516849.html","c6c04a1ab9955a8f64b54902402ac2a9"],["/posts/3513711414.html","366f635c90de72bf226dea7a58d213b8"],["/posts/3523095624.html","34b8536dfc45e94839ee8b51b21662ee"],["/posts/3546711884.html","515f40fc74a9e2d93a5376b54a52afd6"],["/posts/3731385230.html","38c4adb1be2fcd41ffb458e8ebbd8614"],["/posts/3772089482.html","ac70311737d97c7f8dcfd51fb1a76d15"],["/posts/386609427.html","ebf65ccaa7c6749faa24befb4bb6c403"],["/posts/4044235327.html","b8773c0057f87c12af649e4bae897375"],["/posts/4115971639.html","00e228e4b56c1bb8db8f19067f3ff427"],["/posts/4130790367.html","3ebbc37496d7afcfce8d423a13e098dd"],["/posts/4131986683.html","02e65a8e9523405765c41f24d8b445a0"],["/posts/4177218757.html","3bac78069dab12f23269b4caace35cd0"],["/posts/4192183953.html","b9203eea142d5d90128b67b6b7446c4c"],["/posts/4261103898.html","cfeb90a2ba4f9081f3ea5b9a96686570"],["/posts/469711973.html","afe9a7db8d6306bbdf464e96eb5a628e"],["/posts/482495853.html","3bd6ea55facaaac41cb707ec183e88ad"],["/posts/488247922.html","176ddb4bc260df028e44157df09fb76f"],["/posts/517302816.html","917991d16762b2a2350bd2a150fe39f8"],["/posts/570165348.html","e56e59a1f7ae21606c3a87e2026b1d1b"],["/posts/595890772.html","63c1c6e3626538a4aaef86280c23f77d"],["/posts/67485572.html","3ffc427f93ddfa54957b0ad715b7e646"],["/posts/694347442.html","f2df0d5d75d1cd09a5777b686ae958ae"],["/posts/707384687.html","056c7293cd8c6cc2f9efcc4730aa81c2"],["/posts/71180092.html","45e886828c6115c36cb2efab5bc03458"],["/posts/716459272.html","f776bd74bf72121b8ba2dbac77ed602b"],["/posts/765481613.html","f5f889b4dbe29498da23aed94fd31645"],["/posts/778231993.html","b0afae4fea70ca5f8092277840f6552f"],["/posts/795397410.html","c2e9335a3ae75d297e639cc6fe14ebb2"],["/posts/820223701.html","2de1b868189d766b39b2e1f6e8edcb37"],["/posts/830372185.html","6365b2e629f60b8eb352186347c0d9e3"],["/posts/88294277.html","d52f7f9b8931d6f5e7019eac0d3a6374"],["/posts/939963535.html","ae2394172e94480ce912c1bfba7c3441"],["/posts/983786067.html","3b8ec6ad73c200ac28114c7e8337eb62"],["/sw-register.js","45be55a9acf2f23284a5f8a22cef0957"],["/tags/C/index.html","43f748b601ed47687cad5563aa48b5a7"],["/tags/C/page/2/index.html","9ee01c34864a985cc8e946b86ab76b27"],["/tags/C/page/3/index.html","b93157da3de03036e81430629f1f9fb4"],["/tags/C/page/4/index.html","8dedec9bef61edbf902b591b0e14a7b9"],["/tags/ETL/index.html","b30bb6b8092aeecfea302ca156fabd4c"],["/tags/ElasticSearch/index.html","df4aad232d0e832bd48f2d97549df7ea"],["/tags/GUI/index.html","a78f18ef7da98aeaaae3918f5f62952c"],["/tags/HBase/index.html","caf08d7ac42d189a0748d2cec7b59a9c"],["/tags/Hadoop/index.html","24f30f9ea0fb99825bfb0079a4558d89"],["/tags/Hadoop/page/2/index.html","67bf50f4fbe4450c596146ec4ac5cf9f"],["/tags/Java/index.html","c0bd71b3b8a6b328734d7e82e058381b"],["/tags/Java后端/index.html","2be3fee26f1a385cfcaca12e363c4fa9"],["/tags/Java后端/page/2/index.html","ece0b685e5005cd4b1ca6e44d274f6ef"],["/tags/Java基础/index.html","80c1fe90f57c73cdd7983fd5ca27ab66"],["/tags/Java基础/page/2/index.html","2a07145715b3ff21446a72d864b5d5c5"],["/tags/Kettle/index.html","bc1c3775cb07c61ff3ce2b66526a06bd"],["/tags/Kibana/index.html","6396117022db7ac6b840a8ed8e53ec51"],["/tags/Linux/index.html","1e8164c03103cb4ce1c187620b53868a"],["/tags/Linux/page/2/index.html","ab09d36048eab48228d2029acc0a8fe9"],["/tags/Linux/page/3/index.html","42a897e215890212b8aaaebec789a4b7"],["/tags/Mac/index.html","f891327aadc904bbe9e8184bcc54a673"],["/tags/Mac/page/2/index.html","e43ec70af3918d2f5a0f5dc13a224194"],["/tags/Maven/index.html","8558fd8907609475e918fbba7e099395"],["/tags/MySQL/index.html","5fb0e95bc7e987223a7c69ab67b63427"],["/tags/Python/index.html","cec8e2ab9a6140f7aab1e33f4bf7b616"],["/tags/Redis/index.html","15a0a6c919d86aaea1afe27069b7328e"],["/tags/R语言/index.html","163d52070b14c1a6a66a13ee79e9a510"],["/tags/Spark/index.html","cf92af1670b9ccbd27d957105e9e0776"],["/tags/Ubuntu/index.html","3869bc16db0dc2ac501d71583a443f9b"],["/tags/Vue/index.html","0eb68a811f7285db87ef435a2cd0920f"],["/tags/Windows/index.html","f93664a00dedc86f432f120c1eb0dd7c"],["/tags/ZooKeeper/index.html","3447862934a9f9a53a08afa38769c6c6"],["/tags/bfs/index.html","6f4c9053600039dd9015685458cbfbbb"],["/tags/dfs/index.html","f03500b4f0c85db8b8b209a0e4c29368"],["/tags/folium/index.html","f9216d118b03f36ec3b0fc23085edd6c"],["/tags/git/index.html","973a71f1b2ab86bfa8a1c76f22c0dc86"],["/tags/index.html","bad48154a4c7ff5949339e725ddddcbd"],["/tags/latex/index.html","708057084b5715eda5c9f496148dc87a"],["/tags/中间件/index.html","23895016c493debe03c36f5d9505c642"],["/tags/二分查找/index.html","debcd2a955f4162562f141b4a6edf6fa"],["/tags/优化类/index.html","a322bb3dbbc8880384c67d5963b475da"],["/tags/前端/index.html","e4271f996681d37e6c95272e91130342"],["/tags/前缀和与差分/index.html","db740123b01f51c3afc08b1eb68eb85e"],["/tags/动态规划/index.html","47aa3c2a173ba6587d8ce81ab9ab6abb"],["/tags/动态规划/page/2/index.html","ade91d1775f49aee18b6040fcc77aab6"],["/tags/博客搭建/index.html","ee4e6499099dae28d971567e077b458b"],["/tags/图论/index.html","29a8036054d1fefd75bf2635bd7c6b69"],["/tags/大数据/index.html","25880a4fee7b06a07a934e7e7ed2883d"],["/tags/大数据/page/2/index.html","d36d42830a846e92c211a93773c83186"],["/tags/操作系统/index.html","49ee4b9dd1f6c8710f9fa637fd7f6cb9"],["/tags/数学建模/index.html","f1c3c087869d698a34f8f11fc6ca9049"],["/tags/数据库/index.html","d0939aca2015697a830968158bbd8434"],["/tags/数据结构和算法/index.html","8d169d920c0e756a17c1a60e41012b00"],["/tags/数据结构和算法/page/2/index.html","13bed364346846e1d678e577cf4e134d"],["/tags/数据结构和算法/page/3/index.html","e34e74f9e549995271433be0f5101caa"],["/tags/数据结构和算法/page/4/index.html","ac8ddca46a2ba1b472e444ff22ddda73"],["/tags/数组和字符串/index.html","e3a8386809469a1610179a3edea71332"],["/tags/数论/index.html","d35540f69392ea359b39667db969b175"],["/tags/枚举类/index.html","18d2150bf049e278d6c832ce405c7042"],["/tags/栈和队列/index.html","b3a88b9651921e75bfdc1f37b6f1423e"],["/tags/树论/index.html","cc2141195a4a552b7ba3bd2979243f78"],["/tags/测试/index.html","f18fc22154591988a05620bc53223045"],["/tags/环境/index.html","620a0869d735c220b88a4d51b8edabd6"],["/tags/环境变量/index.html","b5af15fd4a5e5aa222087ce346ecda32"],["/tags/绘图/index.html","7841c13ef0e9925b838abd457107a869"],["/tags/编程工具/index.html","88f9ea88c3cfc2a64bc0ed133ebc632e"],["/tags/编程环境/index.html","08e86efe1403c6a84d3bacabb28cdef4"],["/tags/网络编程/index.html","960e6cd9ee2cc1d8d9a8d0154c695758"],["/tags/英语语法/index.html","20c9ff0e13d17269313f1e3f8c7d2300"],["/tags/计算机操作系统/index.html","5ca157327377c678bc3ae48002df9eeb"],["/tags/论文/index.html","55c5016a05fb26d6d40b2e21582fe1c9"],["/tags/资源下载/index.html","36fbce182eebf2bdaec5bc1c6bcb2572"],["/tags/链表/index.html","79f1915d2ad1fd01441b116da25e257f"],["/tags/集合/index.html","476ff005de32bb58223f613828f214fb"],["/tags/集群/index.html","5a004f2917321a7945850f9aeef5e5ff"]];
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
