/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","9cff05386d7270233840b68048f3616f"],["/about/index.html","78a8cdf55d4eaf5e0079aba5b115d03e"],["/archives/2023/01/index.html","7750d6858272f6ee4efd23648008ff46"],["/archives/2023/02/index.html","739ccdeb6e7a94dd9a0ea7c3f217c7fc"],["/archives/2023/02/page/2/index.html","271fab1db28d412111779b5f93d17d99"],["/archives/2023/03/index.html","a7b29a4ebe1bd007cdf9fa1f3eb45540"],["/archives/2023/05/index.html","2e4d245c2384688903a6706a127b0edf"],["/archives/2023/06/index.html","6d46b4e2f1724ad7ef5fa952aae8ea55"],["/archives/2023/09/index.html","9b30761ca8b80a831925a8f2efc53483"],["/archives/2023/11/index.html","e4288f8dbe92cc009a50a421251d3089"],["/archives/2023/12/index.html","4667956f8e35a98c65fca99a983c3e9f"],["/archives/2023/index.html","06f86694d1aecf006d9e0148156b1235"],["/archives/2023/page/2/index.html","3dcca659473fb1da52ce02b90e347858"],["/archives/2023/page/3/index.html","ccc9c73fe82d399f52ccd44ef860a286"],["/archives/2023/page/4/index.html","98d2749a6222df253895d6c9c8a15aad"],["/archives/2024/02/index.html","893ead562fb6fd1e2cd03b857492b98d"],["/archives/2024/index.html","ca35fd11ba2219c02a52d51db884a698"],["/archives/index.html","428abf567fb365f3cd5654e903309e43"],["/archives/page/2/index.html","d24f758de898f9cb293a04642537b942"],["/archives/page/3/index.html","053cf00a82e7d5cb56d349302178ccdc"],["/archives/page/4/index.html","90de18cc7debc561f61c2bd61e26c6fa"],["/baidu_verify_codeva-qQP2iZOMLX.html","12e3825959c1cb9bd185f0c8e0b9dbfe"],["/categories/Java/index.html","4eda495605342fa1332791ed8c6d570b"],["/categories/Java/后端/index.html","09fae6d02b4f2a2c01238d23aa7dbf7f"],["/categories/Java/基础/index.html","e84937d51e474fc7d2d8dfc1fc605037"],["/categories/Java/基础/集合/index.html","c87987ebc8c5dab67c423745124664d8"],["/categories/Python/index.html","660897985a1f62c570b4db0c49bdcf36"],["/categories/Python/编程环境/index.html","5175616824f2a9cdff735872d9440c8a"],["/categories/R语言/index.html","13aaff6ef93a3bd7e9453f8a51c59b80"],["/categories/R语言/编程环境/index.html","96d45aefea01c556d7ce95ea89a9df20"],["/categories/index.html","43548fbc2c9cbc5b7eaf19e66fc4d40f"],["/categories/中间件/index.html","5f53606e2ea30286f70c0395a6fe495e"],["/categories/前端/Vue/index.html","066750c15f0e009abf7e12e9b49c65d4"],["/categories/前端/index.html","edb21a849a6972a435da9555c58a1ccb"],["/categories/大数据开发/ElasticSearch/index.html","ac2d778c316462c2ac851c3000717f54"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","bce290271122e6aa6a46474e47da2db9"],["/categories/大数据开发/HBase/index.html","d5c039b01c2e28859a0a0b71e3581988"],["/categories/大数据开发/HBase/学习笔记/index.html","36a997c4004dcce945907dcd4f374e43"],["/categories/大数据开发/HBase/环境搭建/index.html","d3945489c03c347d4cd0b5d3d44b2152"],["/categories/大数据开发/Hadoop/index.html","3004ed3d7a49b2de089a0265201e0d27"],["/categories/大数据开发/Hadoop/技术/index.html","b93b4ec089c57b5189b5c05a31a4420a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","974a172ba35c80ac36d0bbe6772d1c05"],["/categories/大数据开发/Redis/index.html","94fc87ddfba77c920c44c5d7e2eafe4a"],["/categories/大数据开发/Redis/技术/index.html","d7dc2c472ff4abde7d4bc9cfa30e54c2"],["/categories/大数据开发/Redis/环境搭建/index.html","68702f28c1cb5c2a458d50401943442a"],["/categories/大数据开发/Spark/index.html","2cdfecce79731deb665b90756be2031e"],["/categories/大数据开发/Spark/环境搭建/index.html","ee827d5cdae0e0c7ac747a9e1a98034d"],["/categories/大数据开发/Zookeeper/index.html","39e3a3ba00f3f76b33f1b7fdfb5f8091"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b08fdc8faf53c241732853e84f138373"],["/categories/大数据开发/index.html","a7e06fc10355e2e8a1075bf2da94d4d9"],["/categories/学校课程/index.html","9545226232ede5b63b6d5febf5ec995b"],["/categories/学校课程/计算机操作系统/index.html","979d1a90a2f2886d4ae961d6eb8e6791"],["/categories/操作系统/Linux/index.html","005749d74bd9eb375ddd408987b6b5ae"],["/categories/操作系统/Mac/index.html","1ab4620a1b6a0575598e558fc667cb15"],["/categories/操作系统/Windows/index.html","f6602838951b0fc2ab15dcdac46af2da"],["/categories/操作系统/index.html","a24b07cf78ace82ff3e6601955bf279b"],["/categories/数学建模/index.html","02d2532190b6129dd0d4e5b14a089d39"],["/categories/数学建模/latex/index.html","43a55aa406f5ef0b2716aef6b3ce92a4"],["/categories/数学建模/优化类/index.html","01d1b03a5eeb278c59e35e113c4e6497"],["/categories/数学建模/优化类/现代优化算法/index.html","8fe51de74407798adb09d2beefb6924f"],["/categories/数学建模/优化类/规划类/index.html","b67fb2eab456ee6cebf04f16970003c3"],["/categories/数学建模/绘图/index.html","395cd360d12adfdcac34f5ead4f7b2d5"],["/categories/数据库/MySQL/index.html","c04d25b57b77c75f51b8f35f5350eea7"],["/categories/数据库/index.html","17ba1a454a65c77b3b591ab3bb59cde2"],["/categories/数据结构和算法/index.html","588bb3359f078049c87011ffe2f63ca0"],["/categories/数据结构和算法/page/2/index.html","27e848505a28aea953d24646c7f3a6e5"],["/categories/数据结构和算法/基本原理/bfs/index.html","3ac5b03c547b1d48efb5b63b50ad3b6e"],["/categories/数据结构和算法/基本原理/dfs/index.html","8f25891e086129f17afb5f7415a61e3d"],["/categories/数据结构和算法/基本原理/index.html","12e91d6b7a1ff59b8bda954b168aedd1"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b5e1791054f34d9e231ec9d1a9ba482c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3017f8f3a2de94c5db22f8c0becc23da"],["/categories/数据结构和算法/基本原理/图论/index.html","660c6f228761e321544f163a9deb6ea7"],["/categories/数据结构和算法/基本原理/字符串/index.html","b718f40f729325db1d2a9f214aed4beb"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","c71ed970e959a639de81fedb37077524"],["/categories/数据结构和算法/基本原理/数论/index.html","44ccbd52443353bc3bfaefadabb54ec6"],["/categories/数据结构和算法/基本原理/树论/index.html","4516f2f82d2e01fe248e27af56e8b8e1"],["/categories/数据结构和算法/基本原理/链表/index.html","a4b90d85acfe9ea280cd6e856821ca9e"],["/categories/数据结构和算法/算法题/index.html","3bb570f50f38966e897d3cde9de697bc"],["/categories/数据结构和算法/算法题/二分查找/index.html","e46eadbba2e5df2d4049284db65b06e3"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5bd66878b78e8c7435c653ac1f133e63"],["/categories/数据结构和算法/算法题/动态规划/index.html","b9a35fb785bd47f2ecea37b350ed519b"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a374dac95cc40398dbdab2a7094c3401"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","9fe88d1e36f17c90e815754831853c0b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b33af52b5502d6606f7375574cc5f580"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c3ce83d7e1cc010a9404b525435d6384"],["/categories/数据结构和算法/算法题/数论/index.html","aca4481daa002e2b50ccf90115254a1e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","3a5e48a3a61672335b493716bd6e8b4a"],["/categories/数据结构和算法/算法题/树论/index.html","3416b0627f05adb63c7763b821382e89"],["/categories/杂七杂八/index.html","0242fde33382ed43cbf33afd10f75d36"],["/categories/杂七杂八/博客搭建/index.html","fcef74ebea7392e4a38caa9161bf1ca9"],["/categories/编程工具下载/index.html","e25a37a0f2ca0c4b8a36f42b861e9462"],["/categories/编程环境/index.html","882367d15fbfb1dc18bdd539be7c43c5"],["/categories/编程环境/大数据/index.html","f6b2b1627ea55acb65655367c502d3e2"],["/categories/英语学习/index.html","ed3f21e24ee9784c4c2b321064e8823e"],["/categories/英语学习/英语语法/index.html","d1f27174b8a7e9f327a6fa55684075d2"],["/comments/index.html","d54633695017df7a29cbc5af1e46fbd9"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","96c22a81b5e82b984725b87192ad0aa2"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","49735093e77ae175c54318616e2b2087"],["/movies/index.html","d3bea95abbb91dd202ca9d5e5a3bc0ae"],["/music/index.html","98b0a1bcfd419f691048434b202c6166"],["/page/2/index.html","f15ecde8f2b54d8a7700403c96ed50ca"],["/page/3/index.html","edf366362f750e29cd11354ddecc0721"],["/page/4/index.html","c7223502f9e836558f98c3acda1fc8af"],["/page/5/index.html","c19802ad563aa08234f7b8a13b6fdea3"],["/page/6/index.html","5db2a5cd971f18e66bba131e440b14fa"],["/posts/1021360842.html","234eb0efbdf03e43a4e5b17d8fdc57ec"],["/posts/1120620192.html","ff12b4372418331a38a35841a677119a"],["/posts/1141628095.html","805e2407bc1af6804464cad568b5e486"],["/posts/1168613674.html","67a742e207a48af0177c9a559f584531"],["/posts/1219920510.html","be475662569ea1d9031fa732b32fac3d"],["/posts/1222166338.html","c7aee23fc349a4ad6ae8d6822993c3e1"],["/posts/1259097482.html","8043f749e2823ed04521e490acfedc7c"],["/posts/1271036369.html","a86034711b89640ccdd1c959515d7685"],["/posts/1312847445.html","116d20ba254f0fadbd085d5064fbd41e"],["/posts/135355774.html","e02a055a62f39bf48d47477662a2021e"],["/posts/1375344716.html","a852e5e4c6e0b45e4157c2810cc0ee8c"],["/posts/1388991698.html","efe623848f6abf9b662a05daf5f4b246"],["/posts/1410315814.html","4fd68bde754d9a672038d266ee5fed44"],["/posts/1452790229.html","1b24f008b88d096013d28cbba9221504"],["/posts/1470079884.html","a1a67f06e383bd8e3e6e46f25c2de4a5"],["/posts/1470079885.html","1cf51bd9402d51c3efbe4656b58acf80"],["/posts/1470079886.html","47bde3a9f15fc20186ad85cefcb04d19"],["/posts/1470079887.html","958fc31f5cac6546878e07e696ebfda2"],["/posts/1498536549.html","e5c8fbf600e4a5e504d36f340a2bd582"],["/posts/1539568593.html","dcef27590e8287db3b3a81f6339a2ef7"],["/posts/1547067935.html","4646a46a3bb113831ba7201bfc734c1b"],["/posts/1557866301.html","de5b351ddcfcb7a0e848a921f6db7f8a"],["/posts/1571776361.html","f9c7b31df07aa53ed0baa27b408ab98b"],["/posts/1605124548.html","f89e678cf04107eb6f33940e2d5f003f"],["/posts/1633036852.html","47bcd2f8191584b75cde0552ffa1044f"],["/posts/1674202625.html","02fa36b810cb4bd5e3d897d2c1a9af54"],["/posts/1765123828.html","0e79362a09e6e208ceaf83416628230b"],["/posts/1767336200.html","ff23cbc8edb9c9ac381cf7c85f70b0d6"],["/posts/1776114197.html","a8596a48c20c601fc25958f94420b985"],["/posts/1817748743.html","2063e1bfed76ef399d0dd858e9b544d4"],["/posts/1925125395.html","b3bde16a3d0fbb4397103a768fb15d8b"],["/posts/1966191251.html","99450c75ab0c7605364847d7dc835d40"],["/posts/1987617322.html","fbb45d7ef02b5db020e0fad331fadd8e"],["/posts/1999788039.html","b6466345d50edbd2bae61890594110e6"],["/posts/2075104059.html","288eee1468e05e50fff4ae152c0e6073"],["/posts/2087796737.html","369a423a5eb45bca41bfda621aedfd56"],["/posts/2106547339.html","7eeaa5552f2309cabb04de2d158db4ef"],["/posts/2207806286.html","d1939272c1f1178b7ab58738e361c947"],["/posts/2225903441.html","21b0f2f74bbd61461574692cfa445b03"],["/posts/2265610284.html","6a3d93fbc7cc2cb20b5072cf8f9212a8"],["/posts/2281352001.html","596c2789a1891bb32293856db89b7cca"],["/posts/2364755265.html","75f6103860734fec6318e77caa9d55f7"],["/posts/2414116852.html","eb3f78e241c84e4b14f6d552ca8bf218"],["/posts/2421785022.html","3360c417638feff8c6e28fb943545e24"],["/posts/2482902029.html","35b677c7d44616d19befd3bb1d7c869d"],["/posts/2495386210.html","0abbd39727a09a749131f331f5b05ea1"],["/posts/2516528882.html","68c0bbdb0ad0eccab0fa7990acb30dae"],["/posts/2526659543.html","3bcc84182a516f8ab9a033edfdd4969e"],["/posts/2529807823.html","df1f419fdc8768e617082a2408172a7d"],["/posts/2596601004.html","76f48c5f0cba1a75a9a51f67366fd96f"],["/posts/2697614349.html","d4d22fb3a74216001a56ee8fe3037068"],["/posts/2742438348.html","dc8e2444415a3f7cac74d4ef5f47ea83"],["/posts/2768249503.html","34168f4dcb9fbb20f14038699a37c4a9"],["/posts/2864584994.html","d8789a2a34f69462ac56d2723937dfa8"],["/posts/2888309600.html","a6cca8c8479f91184edfee4c63d4c8c2"],["/posts/2891591958.html","e6d4562c4458134490762ef020f63ec4"],["/posts/2909934084.html","145f6916e77e98ec1e5ad265b21cdc66"],["/posts/2920256992.html","ef1149de83f16918bf36b8a068130c44"],["/posts/2959474469.html","303af190546af3861031e921e6686030"],["/posts/3005926051.html","cb1cdbe9553c3f220e3300121445089a"],["/posts/309775400.html","18ce3ab7b2e516f29388848227b9089c"],["/posts/3156194925.html","a48057c83a00c95e1abd8005e7ccc72c"],["/posts/3169224211.html","b909c1aad3705656057600e56dc4f483"],["/posts/3213899550.html","561b4169e0789b4067e57447304084e0"],["/posts/3259212833.html","d50859075f59da756d8dd7d2e7a3136a"],["/posts/3266130344.html","5b5133911970ef152989b839da05bf58"],["/posts/3292663995.html","ce3583953dca7d8cb7a82c76a98a8bd1"],["/posts/3297135020.html","1f391242b16d4c0bb43e3ccf026a6d52"],["/posts/3306641566.html","8c301911d7557933567d6e8b7b16d0da"],["/posts/3312011324.html","ca3124c0a3a006c5b71a21389e8b6031"],["/posts/336911618.html","eb2c71b139b7d2ef223efe11e1451a71"],["/posts/3402121571.html","628a503346b74764a61ecd549c3581e3"],["/posts/3405577485.html","8e2f75fd3e2562e6e12ac7953d98d400"],["/posts/3498516849.html","c6c04a1ab9955a8f64b54902402ac2a9"],["/posts/3513711414.html","366f635c90de72bf226dea7a58d213b8"],["/posts/3523095624.html","34b8536dfc45e94839ee8b51b21662ee"],["/posts/3546711884.html","515f40fc74a9e2d93a5376b54a52afd6"],["/posts/3731385230.html","38c4adb1be2fcd41ffb458e8ebbd8614"],["/posts/3772089482.html","ac70311737d97c7f8dcfd51fb1a76d15"],["/posts/386609427.html","ebf65ccaa7c6749faa24befb4bb6c403"],["/posts/4044235327.html","b8773c0057f87c12af649e4bae897375"],["/posts/4115971639.html","00e228e4b56c1bb8db8f19067f3ff427"],["/posts/4130790367.html","3ebbc37496d7afcfce8d423a13e098dd"],["/posts/4131986683.html","02e65a8e9523405765c41f24d8b445a0"],["/posts/4177218757.html","3bac78069dab12f23269b4caace35cd0"],["/posts/4192183953.html","b9203eea142d5d90128b67b6b7446c4c"],["/posts/4261103898.html","cfeb90a2ba4f9081f3ea5b9a96686570"],["/posts/469711973.html","afe9a7db8d6306bbdf464e96eb5a628e"],["/posts/482495853.html","3bd6ea55facaaac41cb707ec183e88ad"],["/posts/488247922.html","176ddb4bc260df028e44157df09fb76f"],["/posts/517302816.html","917991d16762b2a2350bd2a150fe39f8"],["/posts/570165348.html","e56e59a1f7ae21606c3a87e2026b1d1b"],["/posts/595890772.html","63c1c6e3626538a4aaef86280c23f77d"],["/posts/67485572.html","3ffc427f93ddfa54957b0ad715b7e646"],["/posts/694347442.html","f2df0d5d75d1cd09a5777b686ae958ae"],["/posts/707384687.html","056c7293cd8c6cc2f9efcc4730aa81c2"],["/posts/71180092.html","45e886828c6115c36cb2efab5bc03458"],["/posts/716459272.html","f776bd74bf72121b8ba2dbac77ed602b"],["/posts/765481613.html","f5f889b4dbe29498da23aed94fd31645"],["/posts/778231993.html","b0afae4fea70ca5f8092277840f6552f"],["/posts/795397410.html","c2e9335a3ae75d297e639cc6fe14ebb2"],["/posts/820223701.html","2de1b868189d766b39b2e1f6e8edcb37"],["/posts/830372185.html","6365b2e629f60b8eb352186347c0d9e3"],["/posts/88294277.html","d52f7f9b8931d6f5e7019eac0d3a6374"],["/posts/939963535.html","ae2394172e94480ce912c1bfba7c3441"],["/posts/983786067.html","3b8ec6ad73c200ac28114c7e8337eb62"],["/sw-register.js","7401b94dc276e67cac8d6bd8b364ca01"],["/tags/C/index.html","6d7689fe2bdf8154c9aae7eaaac9eb20"],["/tags/C/page/2/index.html","cc45b7a47ebf1d43e864e48891e8d5f8"],["/tags/C/page/3/index.html","fda19f67f8fe87b28fe3e81f46420668"],["/tags/C/page/4/index.html","61fa0661a12e3e999e92f15f31b8f472"],["/tags/ETL/index.html","136de3a28265f25bc338318b6cb26a6d"],["/tags/ElasticSearch/index.html","bc29433a54f87c91af70b89080ad78ab"],["/tags/GUI/index.html","ff0549ffc75fcaf8b8f10e2cb3b1df7a"],["/tags/HBase/index.html","2f0c98b58ac90e07eaeff99fb3fb4f34"],["/tags/Hadoop/index.html","568e5f0844f9ed0042e4614a9a44134a"],["/tags/Hadoop/page/2/index.html","f3f41df7bba2d0f6c9ff2d6cb9dbcd67"],["/tags/Java/index.html","2e750ed40392f6e995b9fa7533997776"],["/tags/Java后端/index.html","3e184e3f6262a5b49bd966d9b0efc476"],["/tags/Java后端/page/2/index.html","87662c3ec609c8b0b217a7fbac652093"],["/tags/Java基础/index.html","f98fa317c22b19a40937724baac85ff9"],["/tags/Java基础/page/2/index.html","04cbfd73ad00589d43eb064345c9a71d"],["/tags/Kettle/index.html","211005a531284b806f2e6f4c64108450"],["/tags/Kibana/index.html","8b7925a8b0acc09122c676603600866d"],["/tags/Linux/index.html","d775012d1fd72bdcf82554d6992d88eb"],["/tags/Linux/page/2/index.html","c67e89fdd6359a65b92b966bf68569e2"],["/tags/Linux/page/3/index.html","056e8b1268980bdc0a29174bd41469f2"],["/tags/Mac/index.html","3a0c008d9bf0eb5fcbca125fb674d4a0"],["/tags/Mac/page/2/index.html","067af08868f04d6eccf46dd0a343c3c0"],["/tags/Maven/index.html","a1eed05e3af66e5d2c21f1369d1698b5"],["/tags/MySQL/index.html","1398950f304b2ecd899caf7a5fc59b0c"],["/tags/Python/index.html","7824b0dbdaba6088f6b3c0842a28f183"],["/tags/Redis/index.html","1ef2d5a3d7631fcb96e801bb6b6ea92b"],["/tags/R语言/index.html","20ae655afad557d0752f884aff1b6402"],["/tags/Spark/index.html","05a2f80774de0520ca05c3b30f9e4fcc"],["/tags/Ubuntu/index.html","4d3f84f53c3f1b9ba704a3db0c558cfe"],["/tags/Vue/index.html","c04192624451c00de84fea548fa981a2"],["/tags/Windows/index.html","73aa6e3cb28fa6b01aa8148edd08fd8b"],["/tags/ZooKeeper/index.html","c2e58feffd7891b983aa88070a598097"],["/tags/bfs/index.html","880481b5d78fe446ce1dd4c34be9d280"],["/tags/dfs/index.html","b4cb249f920a80d2ef7ba74f51e8f661"],["/tags/folium/index.html","3aefdfb70b1a20b65e0bfc20482795ed"],["/tags/git/index.html","478907ae4102bd5f08389bdc1b8e3851"],["/tags/index.html","629855ee5a6f264bb39f4103ec0a3d1a"],["/tags/latex/index.html","a46c9308ae4615d8d385c5049722387f"],["/tags/中间件/index.html","053e41e8520f22a499cc14bfa9f94824"],["/tags/二分查找/index.html","b123328071a1a8b24e81fcfdbff6cfe8"],["/tags/优化类/index.html","05e35246bdb71f1263ec1657e88d075e"],["/tags/前端/index.html","75333d4732898d3d0dee7c9cbf3e86db"],["/tags/前缀和与差分/index.html","9cde063ff514b293a3cbe659510512bf"],["/tags/动态规划/index.html","6a0725ede55dc365017befffc028f9b9"],["/tags/动态规划/page/2/index.html","e6fa800e4b793179a94f1d44c3eec5db"],["/tags/博客搭建/index.html","0de90f2415d8970b479b5b69584279b9"],["/tags/图论/index.html","1ad62d99524c65ecefd4dd76c9b60701"],["/tags/大数据/index.html","d9aa69c346d8862554991c96bcdd2b4a"],["/tags/大数据/page/2/index.html","60587a0beea86129d530240c72e7a675"],["/tags/操作系统/index.html","fba489f3a116bbde5330648afa3c61b8"],["/tags/数学建模/index.html","71f80f3c7062d0eebc5d26c7c61ab31b"],["/tags/数据库/index.html","ed59458b95fe9d891f9f1193e7b737a3"],["/tags/数据结构和算法/index.html","f274ad7b82e81e5e33951fe1b946f555"],["/tags/数据结构和算法/page/2/index.html","45ba807278272e5413ebb5f489ca6090"],["/tags/数据结构和算法/page/3/index.html","01c03faaa186bf85be083a65efe94608"],["/tags/数据结构和算法/page/4/index.html","a07f458742b5d3a2e9948e1128966b59"],["/tags/数组和字符串/index.html","17453b64ff0e5e99e16263be0bff0f52"],["/tags/数论/index.html","07c003139edd84fbf5c9bccc1acdff8b"],["/tags/枚举类/index.html","5df4a4dcb3c380dfeb3910102461f331"],["/tags/栈和队列/index.html","30e7a3285605e20c8fa1488820a5ed16"],["/tags/树论/index.html","c44fcb89b56809d7366801cd06702628"],["/tags/测试/index.html","da46044e1d8a46cf61bb2ae21d2db06a"],["/tags/环境/index.html","1c79cdd272cf944ba141b10299e90fa2"],["/tags/环境变量/index.html","4df18830e2b9752b764e5a4d1f8909f2"],["/tags/绘图/index.html","475fa6d2193a2bb66b82c2595f341824"],["/tags/编程工具/index.html","0feb229ae7d0afd13f098daaa701d481"],["/tags/编程环境/index.html","616486749efc956318180aeacd29daa8"],["/tags/网络编程/index.html","e631c381128f3b23042ddabd283c5d97"],["/tags/英语语法/index.html","fbf1e62eea16f23a790eb9f6d04e775c"],["/tags/计算机操作系统/index.html","1159af3b0685eba092ac52b84d32a82f"],["/tags/论文/index.html","e833feaddd1e255625c99c67eadc9f13"],["/tags/资源下载/index.html","b9302dfd5502a7371256e83775a1dbe1"],["/tags/链表/index.html","40a91eec42afd7925482a34f11c727a3"],["/tags/集合/index.html","17c499d2a45efe9ea76b950a0d13d37e"],["/tags/集群/index.html","806fcfde243c6b4c6b3cf847461a0849"]];
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
