/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2d75b507870239b32a5387adeb9cf77f"],["/about/index.html","fda808bfe759bd2a140247572f50248d"],["/archives/2023/01/index.html","4ccaea2fee868a1a0b86278272ed2d63"],["/archives/2023/02/index.html","715ac8d3767b337e248f70524632a112"],["/archives/2023/02/page/2/index.html","a6b3c6acd3bec9b97ce42acdcb8bf9ce"],["/archives/2023/03/index.html","06a35fc90233de5c3ac9b04fb453d87a"],["/archives/2023/05/index.html","6bf44b35031217a94317b47e641960b7"],["/archives/2023/06/index.html","4ee07ae0e4b29fce224004558669a8c7"],["/archives/2023/09/index.html","5bb5f234071ecd2a24a0740574e1ac3d"],["/archives/2023/11/index.html","c7752076fb90a6824c1b2ba7b8c1665a"],["/archives/2023/12/index.html","c3053f39925561fb2df9db5c370ec1b1"],["/archives/2023/index.html","4d28ff30d99bf70ccd48f5e2b2c693f7"],["/archives/2023/page/2/index.html","512e95bb191c33a48d73ee7175092bc8"],["/archives/2023/page/3/index.html","b628a3fab0be048bef00880188fd82e8"],["/archives/2023/page/4/index.html","a7e9b0f6e9d4e1f06404ddbdd8bc00a9"],["/archives/2024/02/index.html","3beadbdcd4acb4d87b714724bf4de39c"],["/archives/2024/index.html","68201f6230e351b239b779e322021cdb"],["/archives/index.html","b1d38e8b0762b96a43835536e5562ce2"],["/archives/page/2/index.html","76348664ab9d4ecf22c17d80bfbfed2c"],["/archives/page/3/index.html","e000d0b033a3913b12e5dd4602fe0f8d"],["/archives/page/4/index.html","9e4b49ec243ea68702c161d682423782"],["/baidu_verify_codeva-qQP2iZOMLX.html","345912ecf9ed395e226f639342ff0aea"],["/categories/Java/index.html","2d1222b65a3a6b82aa5d1ac5fee857ce"],["/categories/Java/后端/index.html","81a7935d4ac459f701122b5141d601df"],["/categories/Java/基础/index.html","75dc1025bfc13db6a516f3f7d91cc5a2"],["/categories/Java/基础/集合/index.html","21157e58ff38fed78b5359a643c260ad"],["/categories/Python/index.html","08e9cc0e608386c6e9423c94ee8eb759"],["/categories/Python/编程环境/index.html","ee7e22cd5366dc7121a7d1e1ca510b51"],["/categories/R语言/index.html","354c6ccc99bb8d58d53aa3a68ad77f41"],["/categories/R语言/编程环境/index.html","ebbacc95f0a813752d6803c27e9b66fe"],["/categories/index.html","45ff695dbd552eace9dc4671a07df9c7"],["/categories/中间件/index.html","f8a5225be4b5294cdfa7d995267856e9"],["/categories/前端/Vue/index.html","8cd506a59b41af7cfe8a5f979584d402"],["/categories/前端/index.html","369faa23ad7e3d38bddc50e349a8c70f"],["/categories/大数据开发/ElasticSearch/index.html","2047773721080ae5e33853696de8c9a7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","e3796795e3766cf1e67f19bbd7d5b336"],["/categories/大数据开发/HBase/index.html","72961802a8d8a926151003ffd3dfc558"],["/categories/大数据开发/HBase/学习笔记/index.html","f4d81ae521466e0d26e550e1725433b4"],["/categories/大数据开发/HBase/环境搭建/index.html","5279c9d081074ada1784192fddc85b1b"],["/categories/大数据开发/Hadoop/index.html","cd61c3e2718bc69ca3dd1582887c279c"],["/categories/大数据开发/Hadoop/技术/index.html","e08337250fe3411c74a41a6b8c44cb91"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3963a399879d0ae24a72f8bb1ec3c7cd"],["/categories/大数据开发/Redis/index.html","8cbb7e8d6fa4e24ef2f2f1fc7ccc53f8"],["/categories/大数据开发/Redis/技术/index.html","965601d71befd3b1138dc14df8e39446"],["/categories/大数据开发/Redis/环境搭建/index.html","e11b087fe5198a94843a2100fbbc45e6"],["/categories/大数据开发/Spark/index.html","1a3a544b74c6dd3111df57106d305bf5"],["/categories/大数据开发/Spark/环境搭建/index.html","749374c6e4f2332f05520dfd96d07a99"],["/categories/大数据开发/Zookeeper/index.html","b568ca397d938955cac562414e57f95d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","eeaeb3b64428f0036004011354a916a0"],["/categories/大数据开发/index.html","5984be3347079c4b3aea9037eac67ee3"],["/categories/学校课程/index.html","643151e6713d8872f2af9d8269e58852"],["/categories/学校课程/计算机操作系统/index.html","2081b7f36dc724dda8e7a782f4c346aa"],["/categories/操作系统/Linux/index.html","8b80bd0df878138ad291dd5b34309676"],["/categories/操作系统/Mac/index.html","d0b91ec6787df89f3485393acbcb6493"],["/categories/操作系统/Windows/index.html","ab42b7bd934efd0014f451a1518951fa"],["/categories/操作系统/index.html","72ba84ae8e4e9f90030c59bada3c1b9f"],["/categories/数学建模/index.html","652ef7320802d5eec74fb224b71e699f"],["/categories/数学建模/latex/index.html","87ff7461ef6ca24f822b44e51f708c8d"],["/categories/数学建模/优化类/index.html","2d5bca04ce21aa821f92833eb33c93bf"],["/categories/数学建模/优化类/现代优化算法/index.html","fe66f32534174cb0e66e5e8fd663354e"],["/categories/数学建模/优化类/规划类/index.html","9101278cab65b804715ad9f8c0a903f2"],["/categories/数学建模/绘图/index.html","654bafd6f06ac954b50d2ec65a91be54"],["/categories/数据库/MySQL/index.html","6b003b6161045b8068abff860fb1d692"],["/categories/数据库/index.html","0fd9fc1e2098af316d1fa791a727fd3c"],["/categories/数据结构和算法/index.html","3930c6d598bf68d940886cbe883c01df"],["/categories/数据结构和算法/page/2/index.html","badacf82c7999eda62f8ce3e2606e6c9"],["/categories/数据结构和算法/基本原理/bfs/index.html","2b4cb6dd727f894e478f5173d7b7ec9c"],["/categories/数据结构和算法/基本原理/dfs/index.html","aa3ad0a8e1a818276ea79df5f480e56f"],["/categories/数据结构和算法/基本原理/index.html","d3d1d1d94d107c7b793f6008ea2415e7"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9ba8f08920bb8c7c351292f05dcdcbd7"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5e82e343dd9c2d5e2fb55596f4ae379c"],["/categories/数据结构和算法/基本原理/图论/index.html","e6552745bca13ab9fe702db35f139e8d"],["/categories/数据结构和算法/基本原理/字符串/index.html","6f75b8164857ff1ab3f021aad6a0501b"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1265a30d8d91f674cb662d5fb467da2f"],["/categories/数据结构和算法/基本原理/数论/index.html","f2ed0a40fe638a7394d11399e03c7bc6"],["/categories/数据结构和算法/基本原理/树论/index.html","f876d42bde5cff65a8a66d515cc3ad4b"],["/categories/数据结构和算法/基本原理/链表/index.html","4f9b25104c5860b9f7e3b0e22760c8de"],["/categories/数据结构和算法/算法题/index.html","2d487b9b2a6c47fa39d8ea3c1e320a56"],["/categories/数据结构和算法/算法题/二分查找/index.html","bc4a285a2fa6a6de5215577371e97a31"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","cb62c895a7e745a9effdf0e088e6618b"],["/categories/数据结构和算法/算法题/动态规划/index.html","1137c24a8df8f2672802d8b2a94e482a"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2b51d77873717f98a6303714c715ef94"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","be4a402cac8eb959393f73373374fb52"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","89e0b27851fca3be1bd3e88111c301d1"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","70760fca5b84cbc87c56de92d5a754e5"],["/categories/数据结构和算法/算法题/数论/index.html","5f4f6f6b23ebda7266dd4dd8b6ecbc13"],["/categories/数据结构和算法/算法题/栈和队列/index.html","18afb9385bbc54417d8c298920029f06"],["/categories/数据结构和算法/算法题/树论/index.html","293a1dce34fbd12dde64d547215125ee"],["/categories/杂七杂八/index.html","63a8c5f929d50962144946826ea57a2c"],["/categories/杂七杂八/博客搭建/index.html","39d633b6b8a1ac3428d31328f5ee4495"],["/categories/编程工具下载/index.html","920ab14e0acd195bb3f4cd7322df9345"],["/categories/编程环境/index.html","111e3af5f99a32b782fbf0c8c7d41e29"],["/categories/编程环境/大数据/index.html","d5c09a3112ab320b612684e8cb820b2c"],["/categories/英语学习/index.html","7e187fa410a3c8e8651f53d5711170e7"],["/categories/英语学习/英语语法/index.html","9d01cd6d16d8bb97208096f4d188b447"],["/comments/index.html","942bdad266742f87a0d79eb4fd7f4d70"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","5cb18752799844acffc00859656cdb49"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","289920a73e070925640b23c71857e29b"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","59196d6bd4d90bbc3b397ba062ff699f"],["/movies/index.html","7ad2f6e28e110f03db3930a1757a57b9"],["/music/index.html","b530cc91716192f2d4586d1fa80d3d43"],["/page/2/index.html","68a59848a68496aa01e799f5cbeb94cf"],["/page/3/index.html","9a1677baf2ec172044a294f6f4079139"],["/page/4/index.html","5ee81b33672e6d8016a22788f3badcfa"],["/page/5/index.html","87a9c940e48548ef85abc45d8a1cc7fa"],["/page/6/index.html","857165da948e6754152e5dff15693408"],["/posts/1021360842.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1120620192.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1141628095.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1168613674.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1219920510.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1222166338.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1259097482.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1271036369.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1312847445.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/135355774.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1375344716.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1388991698.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1410315814.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1452790229.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079884.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079885.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079886.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079887.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1498536549.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1539568593.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1547067935.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1557866301.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1571776361.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1605124548.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1633036852.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1674202625.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1765123828.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1767336200.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1776114197.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1817748743.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1925125395.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1966191251.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1987617322.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1999788039.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2075104059.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2087796737.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2106547339.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2207806286.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2225903441.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2265610284.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2281352001.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2364755265.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2414116852.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2421785022.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2482902029.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2495386210.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2516528882.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2526659543.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2529807823.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2596601004.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2697614349.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2742438348.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2768249503.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2864584994.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2888309600.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2891591958.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2909934084.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2920256992.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2959474469.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3005926051.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/309775400.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3156194925.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3169224211.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3213899550.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3259212833.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3266130344.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3292663995.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3297135020.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3306641566.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3312011324.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/336911618.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3402121571.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3405577485.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3498516849.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3513711414.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3523095624.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3546711884.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3731385230.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3772089482.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/386609427.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4044235327.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4115971639.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4130790367.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4131986683.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4177218757.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4192183953.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4261103898.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/469711973.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/482495853.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/488247922.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/517302816.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/570165348.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/595890772.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/67485572.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/694347442.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/707384687.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/71180092.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/716459272.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/765481613.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/778231993.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/795397410.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/820223701.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/830372185.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/88294277.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/939963535.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/983786067.html","d41d8cd98f00b204e9800998ecf8427e"],["/sw-register.js","a9eee9b403cf8434f96649d2d803b068"],["/tags/C/index.html","90f765c5476c34d295183bb9740e0330"],["/tags/C/page/2/index.html","6210da0c327f4441a88591624b9648da"],["/tags/C/page/3/index.html","5c9397ad85b2fad424d404fb2f354414"],["/tags/C/page/4/index.html","6a7e93c16940b9cff23627691f96d4a6"],["/tags/ETL/index.html","3bee4f2c7dbe83395d31f2c18538a964"],["/tags/ElasticSearch/index.html","741e9b3d8a95bfffd0b79d41a055e4d0"],["/tags/GUI/index.html","0749381a293736ae659fe0932b741249"],["/tags/HBase/index.html","d76147a363dae9fcd34f7a08974a9e11"],["/tags/Hadoop/index.html","0841c60fd7e87220be3323a6f30179b7"],["/tags/Hadoop/page/2/index.html","bd3c02e8b72c831dc8d129c62877089c"],["/tags/Java/index.html","b6ebb49b7da103b427c47094fa162c9a"],["/tags/Java后端/index.html","477fafd4baa3db5c23ff57b402d8c5d1"],["/tags/Java后端/page/2/index.html","1af9596048cd218caa46b9115b048916"],["/tags/Java基础/index.html","c899505010cd4d4cfd45ce0d7854df5d"],["/tags/Java基础/page/2/index.html","5847fd6ac3eb4a94f128ee7a17742bef"],["/tags/Kettle/index.html","76ad89a416caf36590a3279cec40f62b"],["/tags/Kibana/index.html","a20e3cf7bc1ce2046a16f08aa3038283"],["/tags/Linux/index.html","7e7e75c78a1a911bdb198cb91875b298"],["/tags/Linux/page/2/index.html","55dad72e9a50831bd072521168e9a1c1"],["/tags/Linux/page/3/index.html","039fc1ec098895a533d7087a43240ac4"],["/tags/Mac/index.html","751556b22fd90007fd5ffd53a73b4059"],["/tags/Mac/page/2/index.html","d9605d9000bd9d32fc1fa3b6436e5a8f"],["/tags/Maven/index.html","58761a6d302d4a05d283bd705e0efa78"],["/tags/MySQL/index.html","3c8a244bec2e1b658c935eee3d688f8a"],["/tags/Python/index.html","bb7ad2c12f5eb73c85b94b5b528fce6f"],["/tags/Redis/index.html","f006570ad70610871643365d75bde6e0"],["/tags/R语言/index.html","cd649ae0fe520113ce901eb3f88a35cd"],["/tags/Spark/index.html","27a2a12bb213cd9f4383d006639a7fce"],["/tags/Ubuntu/index.html","7c556ad57fc62d5ee562a2382f7f40e1"],["/tags/Vue/index.html","16f9d2dc9ec6cd34173c90e08f771a34"],["/tags/Windows/index.html","7f5c5274e7837f708f3b174feaeee52a"],["/tags/ZooKeeper/index.html","0a2a5fa33a3fa2bd0ef561bc3bf6835e"],["/tags/bfs/index.html","bc7679a777f52cf123187b5d05fcc0da"],["/tags/dfs/index.html","710932c81f3067f454f08ab1332c5545"],["/tags/folium/index.html","5c7b9642a82655f33ce86a2c11d84a78"],["/tags/git/index.html","4b5187a192baf9d8c73318c991c57cf6"],["/tags/index.html","b7b0312c257030c9851a85ecb0800ecd"],["/tags/latex/index.html","739f9c3f10e0b924871ebc9fb3b766ce"],["/tags/中间件/index.html","520e3416cdd5a5d18d6d74941a57a91c"],["/tags/二分查找/index.html","a191055c49e76c13e06f1b7826424c6d"],["/tags/优化类/index.html","a0976423e122c51509a3c6a49a5cf407"],["/tags/前端/index.html","23ff73aa9ff71725f888ed84cf449b30"],["/tags/前缀和与差分/index.html","34a08b2eeb4f77ba01dc267c563efd90"],["/tags/动态规划/index.html","a3b44b41b913e096d26a5b03e454ff31"],["/tags/动态规划/page/2/index.html","b23d31128264ef99d0f871092cc13b6b"],["/tags/博客搭建/index.html","46a2418424870fa7574f8472627a3861"],["/tags/图论/index.html","da96f4e113f3d65e75fcf546d0cccb0c"],["/tags/大数据/index.html","caf248d03a1cd07ac5a363a008944750"],["/tags/大数据/page/2/index.html","702583ef7929982c4490749787e799f7"],["/tags/操作系统/index.html","3f6830841ad3070846d1963155404212"],["/tags/数学建模/index.html","c76d1e5edec61d99fa36137d59dcaf2d"],["/tags/数据库/index.html","bda121c9382cce0378ef02bd1af12ad1"],["/tags/数据结构和算法/index.html","d63f8123c3bb1de982b5d72b961da1af"],["/tags/数据结构和算法/page/2/index.html","acfd14bfe49d42e439289c2b76f68187"],["/tags/数据结构和算法/page/3/index.html","b4c1e916c66190a7d51bb20dc22416fb"],["/tags/数据结构和算法/page/4/index.html","558b05510d0c47e82723079eb636dfc6"],["/tags/数组和字符串/index.html","6b5d5d746ecc8bc25a131ab3e0c6509c"],["/tags/数论/index.html","4d15efa3df2b4dd8e6264b0474719c3f"],["/tags/枚举类/index.html","27ff85e71c7823248fa1ad528b05f277"],["/tags/栈和队列/index.html","41ff67743c778ae9d0a5d91290f673e0"],["/tags/树论/index.html","c8cc778feb93b7f5b8fd839b61c4b6ac"],["/tags/测试/index.html","ab92216ed996ca5576c67df92e6e15ac"],["/tags/环境/index.html","bf15dda714536e056d92c64595dded03"],["/tags/环境变量/index.html","e07c0e79676043fdaad3d8e2e289d224"],["/tags/绘图/index.html","a435b36ff7334de894644bcc210e04a8"],["/tags/编程工具/index.html","c5b175d905d862f1a5a5bda9c238dab8"],["/tags/编程环境/index.html","1d41bbc35bb73c234426ae98b6c2d3fe"],["/tags/网络编程/index.html","11605b1b7aed0a63fc7b4d2d8a4ba275"],["/tags/英语语法/index.html","298a68e4745a532a18c145aac8520962"],["/tags/计算机操作系统/index.html","8962c3534c2be8652d14b6d73e86b63f"],["/tags/论文/index.html","6be4762d667bddae1645f4d95632424d"],["/tags/资源下载/index.html","41180711e7b928eaeb38c78a665f51b6"],["/tags/链表/index.html","ea90555992dc997364f3cbbcbda132d4"],["/tags/集合/index.html","2de949498f37f8b7053abde02ebdd5d8"],["/tags/集群/index.html","26333f13253492e45742685f21bdeb95"]];
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
