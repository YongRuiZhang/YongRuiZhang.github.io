/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","bcb5cf44a032140f4e4f17b27704b1e8"],["/about/index.html","09acac93a9a99538b9b8e533f4126556"],["/archives/2023/01/index.html","da10c39f14474976623c5e5939a85bfa"],["/archives/2023/02/index.html","5efb2b1fc8fb94b91e26dee4658ab742"],["/archives/2023/02/page/2/index.html","5944ccc9cdbd0c97e2d371e4fb3c7542"],["/archives/2023/03/index.html","d15cead60669864b944d7a9ed749b96a"],["/archives/2023/05/index.html","cbcb9d66361b835e52b3777993cd1df0"],["/archives/2023/06/index.html","e9ea1ddfe0c08cae08084e7e1478ae7a"],["/archives/2023/09/index.html","4e879b47b9d1ce5ae88e39fc43f9ebde"],["/archives/2023/11/index.html","cca542cd2e906588f2859eeafbfd4344"],["/archives/2023/index.html","289e1ccfa007f3b686b2f59d93da0e5c"],["/archives/2023/page/2/index.html","a3e467e95c048ab61d8926526d808c72"],["/archives/2023/page/3/index.html","46f8baed1b3876a0bba2e8f8d4526f79"],["/archives/2023/page/4/index.html","1be1b14236ae658a34b4eb6d1e8c069d"],["/archives/index.html","3057f52401733034a4ddfbd2c3a7945f"],["/archives/page/2/index.html","6e301a20fbd5fb942e903a571a10c73f"],["/archives/page/3/index.html","1a60057fe8518f5fba1d18134fd6c2b8"],["/archives/page/4/index.html","430f2b9d5424dd3ffdd3f2c1979683fb"],["/baidu_verify_codeva-qQP2iZOMLX.html","558ef050d22a6d046d2965da87d0c391"],["/categories/Java/index.html","d0811334ec9e8709d44cbe2c47dbecc7"],["/categories/Java/后端/index.html","d90adbf66aa9f509038cb918a2831058"],["/categories/Java/基础/index.html","93b07a80f72c0bd621337490cee4c100"],["/categories/Java/基础/集合/index.html","f8c0d07f244d6f39e2d6121cf846ee0d"],["/categories/Python/index.html","68955a6f5cf7404c0c2d62c1d28afa40"],["/categories/Python/编程环境/index.html","435ef8743343cae15c985c7d78b50b0c"],["/categories/R语言/index.html","aa9e9349e77ebbca044f76c3ddec884a"],["/categories/R语言/编程环境/index.html","929b739c921855051bcdefa240adf8e2"],["/categories/index.html","1e738e69899e7891d5a374810dad2b9c"],["/categories/中间件/index.html","cd692a709a0bbb1f39d2af8b599de846"],["/categories/前端/Vue/index.html","d2dfa39369b2bddcceb8c36a3a685168"],["/categories/前端/index.html","bb9171de1a0ed407bcd478febb860e1e"],["/categories/大数据开发/ElasticSearch/index.html","5dc7c9c73b1f34a6b675fabcd52f1808"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c389eefc69d2d01e68d8f8815aafaf69"],["/categories/大数据开发/HBase/index.html","c1892eb1c9a1d07bb023a110d10f1cce"],["/categories/大数据开发/HBase/学习笔记/index.html","1d844cb9f188587f2f7edad9c5ae3fff"],["/categories/大数据开发/HBase/环境搭建/index.html","1a723969d4ac295c9a124becfa15d301"],["/categories/大数据开发/Hadoop/index.html","4ebeee4afabcd4657d6e7c9e2771b5de"],["/categories/大数据开发/Hadoop/技术/index.html","f4c86703f663609acabba7ef41c743ac"],["/categories/大数据开发/Hadoop/环境搭建/index.html","604610a49201362c22f45dc574bd6d6d"],["/categories/大数据开发/Redis/index.html","777a5eb84284bf0ad39b58ab8969ff0f"],["/categories/大数据开发/Redis/技术/index.html","d21f36d6d03a407faeaba985b2120ff8"],["/categories/大数据开发/Redis/环境搭建/index.html","f77aae641e542e68620ced69cb2b0c5f"],["/categories/大数据开发/Spark/index.html","b740e8c12e9cda3d220f2f9323b3419a"],["/categories/大数据开发/Spark/环境搭建/index.html","abdf4ce80ad80d99627c97df81f4589b"],["/categories/大数据开发/Zookeeper/index.html","3bfeb61df4d6f1625d9329e739fc9074"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","bb8fb12f2d36ca1bbd86bc9f94404820"],["/categories/大数据开发/index.html","07b7e85a184da7abad0f4196ebc797fb"],["/categories/操作系统/Linux/index.html","9b86eab8732f561f019922e92673b736"],["/categories/操作系统/Mac/index.html","713858a0d8db9a53aca98cf781cba339"],["/categories/操作系统/Windows/index.html","0c8da88bcc77d8a9b14f52446d525802"],["/categories/操作系统/index.html","c0da91830f32b251162adbc09b3809c3"],["/categories/数学建模/index.html","ee960028603237554f402a6209fcec41"],["/categories/数学建模/latex/index.html","725595342912f8e4d0b61df57a84ff2c"],["/categories/数学建模/优化类/index.html","5cb153037f2c1b83973146b78fb5f627"],["/categories/数学建模/优化类/现代优化算法/index.html","daa38d58972f993b70747a8610fad5fe"],["/categories/数学建模/优化类/规划类/index.html","b70d282724b6bab574f6e8240a929850"],["/categories/数学建模/绘图/index.html","edeb9293200150a02a28dee278983343"],["/categories/数据库/MySQL/index.html","e35227ac866e4e0b4967851449029b2a"],["/categories/数据库/index.html","3a54d7fee3a54d6523646849dbe83792"],["/categories/数据结构和算法/index.html","df8d16b46db9e85f52afbff245ba42a6"],["/categories/数据结构和算法/page/2/index.html","744888e2f007ad171b82fac4dfe95330"],["/categories/数据结构和算法/基本原理/bfs/index.html","6db766b7c20ffcec56e7e56ef405d1eb"],["/categories/数据结构和算法/基本原理/dfs/index.html","f540a9364a4687d9ce7e7b64303920db"],["/categories/数据结构和算法/基本原理/index.html","3a9fec0653ab389a0d3def09cd028fb0"],["/categories/数据结构和算法/基本原理/动态规划/index.html","36658084abc79897b3d9a67e97b50774"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5ee2c9889e48899980e351e417472c97"],["/categories/数据结构和算法/基本原理/图论/index.html","211e7c64aa6aa220360966da0d1ea6d3"],["/categories/数据结构和算法/基本原理/字符串/index.html","e50fcfc08eb6459b6e542e1d95983e90"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ea4c12b9b459ef340b242c5ce34e62c0"],["/categories/数据结构和算法/基本原理/数论/index.html","dbb9aa7e6ac5a13ba46ced049f8b4568"],["/categories/数据结构和算法/基本原理/树论/index.html","3faf3cc71691f4a966e05b6f4229f177"],["/categories/数据结构和算法/基本原理/链表/index.html","09bcfe341ee54030a4746d2b991c4f85"],["/categories/数据结构和算法/算法题/index.html","ebea52424e833ebef48d6f27d452acf4"],["/categories/数据结构和算法/算法题/二分查找/index.html","f0e08c451a5da2c2c175050dc11f84f1"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1d8303371d1d0196a6ebe2f6c5c32ba1"],["/categories/数据结构和算法/算法题/动态规划/index.html","232ee00c0219fd39f4eef7c764e95060"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","4a96dc41b3c20463dfd05de896677dd3"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","365d65959ba5f67b1c0292034f3a9d49"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","eda621229941d80f1d19b0631fd770b2"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","a081d43c72a535377353957fa969d159"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5af5eb5e879d6fa6f1d35a9284f834ac"],["/categories/数据结构和算法/算法题/树论/index.html","739aba7941e721d4d7f75708175b7cdb"],["/categories/杂七杂八/index.html","14889873fd13967665ee7405890ba35c"],["/categories/杂七杂八/博客搭建/index.html","3c96354bd44ba1c234c0679df052bf6b"],["/categories/编程工具下载/index.html","93657d8ac0e9a8e2ea917d0b7f5a81fc"],["/categories/编程环境/index.html","fe0566a29b70fe7e55216d0aa364162e"],["/categories/编程环境/大数据/index.html","eeb4bcca09a9adf60d6b5b2868cb69eb"],["/categories/英语学习/index.html","8c2cf7c476236661227690af632a0324"],["/categories/英语学习/英语语法/index.html","9a8ceb98e3eb4fbee558eced8d7e5cbb"],["/comments/index.html","ab7ae90759bc82dba618d82b3a732521"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","058acaeecb4f9a5dde27cef1338996cf"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","17fae9c32eef85882e2be7ba2cba5c55"],["/movies/index.html","32436241efa23446a6d120b6cfbb5733"],["/music/index.html","fe2df805b603651161deede48e4c7068"],["/page/2/index.html","4ab7046b7a2124d30dafc042adbda58c"],["/page/3/index.html","53dccfcb3b14b8f58aa2bece5df694f0"],["/page/4/index.html","1530b116a7cc302e7ae81d2b26094de4"],["/page/5/index.html","24741df7611d9739805295978882e76d"],["/page/6/index.html","232e1515248fbfccde223da4ae21e873"],["/posts/1021360842.html","13ceec47aff98845229e476d4b37245d"],["/posts/1120620192.html","4d5dd85877a135820dd03814df5d4582"],["/posts/1141628095.html","2e3e1d6d9ac35195fc05067081fc688f"],["/posts/1168613674.html","4d562e1dc2ab01ebe8ffd2b74f721e53"],["/posts/1219920510.html","81a8e1500aa1864d04fb2f6b5c344dbb"],["/posts/1222166338.html","9850a1f6d9124352998fb81977aa8f52"],["/posts/1259097482.html","329de17159837237b8619eeadf1ea2ed"],["/posts/1271036369.html","cade3c3ae32fb5cebcb14a627c2db7a4"],["/posts/1312847445.html","29236eb9d4b78e266f90e049a8142708"],["/posts/135355774.html","e61ca53c43f953cb8859897f8753fa69"],["/posts/1375344716.html","94077f58b1cc09b0f3673394fcf1c182"],["/posts/1388991698.html","7eb08b77e3ac11270ef43bf74b8264eb"],["/posts/1410315814.html","ce99901928543133b48aacfbd0413b54"],["/posts/1452790229.html","08d966b4f69d0d29e7c4596ade48646e"],["/posts/1470079884.html","54be8bb2c2ad15ce13e6a3b0f1ae96cd"],["/posts/1470079885.html","6e5e29e648b1c1136cd49fd08c6b2597"],["/posts/1470079886.html","f112a0042f49dfa9c7f1b9b8c132adac"],["/posts/1470079887.html","c8af91ad4614f2f3206ddd9755a744b5"],["/posts/1498536549.html","15e61c4fc44026a3ef4a37e03c4f67dc"],["/posts/1547067935.html","35800d4e78ea3e5d60d0e532b2ccebff"],["/posts/1557866301.html","c5f9f6a411dc0a122a84a18dac5d0f61"],["/posts/1571776361.html","772f4f008f80df7531326e294b0c3cba"],["/posts/1605124548.html","1e4173031c6cc228083ac1aea14730d5"],["/posts/1633036852.html","8330a977abeb2db958b0048de6402740"],["/posts/1674202625.html","06da8c244bff6ea9002bd057642a3138"],["/posts/1765123828.html","5ec747c3da824dc9454f51be417bb7e3"],["/posts/1767336200.html","ab638c723154248c58cdeb2fc5867dd2"],["/posts/1776114197.html","1e5559f9b58c643c4051dc59a8d690ba"],["/posts/1817748743.html","4ba6582a0500d5c5abb838672413f69a"],["/posts/1925125395.html","8eb9e99415097f203e320f987dc85d11"],["/posts/1966191251.html","349e3b26d88771536f750343d3a9ec48"],["/posts/1987617322.html","3f392c638e30b6903ce601d69ae20059"],["/posts/1999788039.html","da03cdca8764a26df4adf7d205777f0f"],["/posts/2075104059.html","0d83a6b2c4a88978bf9d6a274925a098"],["/posts/2087796737.html","996851a4dd6e9271c502c94877d129b5"],["/posts/2106547339.html","929b09e0242d1ff0388e084cbf88edef"],["/posts/2207806286.html","0b218e8a55afd7b119ae4b1533351e28"],["/posts/2225903441.html","73766b5438e4b541801fa78332f3f180"],["/posts/2265610284.html","be5b22cdcf8b624bdb6065b1eb4652d2"],["/posts/2281352001.html","55d34d4f2929b94906bb581140444c20"],["/posts/2364755265.html","0d0fced3088255ada0a6100009b2de98"],["/posts/2414116852.html","cedfcae91f7377aef69bc310f99715b1"],["/posts/2421785022.html","a5b63cf8d16f7e092f3da94a8c891b05"],["/posts/2482902029.html","aae09ba3317b2cd2206fe81f573a3159"],["/posts/2495386210.html","ba072f5af5f31d388934de4fb466a650"],["/posts/2516528882.html","4bcc930e3d210e2539634ad49a4cba79"],["/posts/2526659543.html","6cc04ad46f8334ae6d2d3e4c94c76551"],["/posts/2529807823.html","039055cdb4221cf2ce5649b50e757d64"],["/posts/2596601004.html","2fa439ebd2a9de9a5df1f40de8bba1f8"],["/posts/2742438348.html","a27787a6daf0d40b90a5a542a9ebd37e"],["/posts/2888309600.html","81b83cc58232847cdd784e8c43593dd6"],["/posts/2891591958.html","992f17d6ea4de23cb278230d3d01a932"],["/posts/2909934084.html","5c705b438aacb140fd2bd52d5b27da8e"],["/posts/2920256992.html","a7ea06b3f5ece28d3870302d064533e1"],["/posts/3005926051.html","c20308e4f1a190f3f1419fe04b5e44cf"],["/posts/309775400.html","fd9f22f929d6e6c3453a1b0a2a51e180"],["/posts/3156194925.html","6bd30e82a66e7e02b83310b3675e95e5"],["/posts/3169224211.html","8857d1f24948d327f3b6ed384ea974df"],["/posts/3213899550.html","bf1e9ea5f2973ef3e60430f20f02b206"],["/posts/3259212833.html","a5e6a277a361bc255ebb35fc4562b295"],["/posts/3266130344.html","ae4204726addaccc15f3d2f3f57dbb02"],["/posts/3292663995.html","c15fedc085d88ac566c279d25511cf85"],["/posts/3297135020.html","2c098bd85b39132b4a0c805cf6c43274"],["/posts/3306641566.html","e4cbf1cda36070ace298d6ce5827f216"],["/posts/3312011324.html","964d3917a9be6dfbb3dae8468691893e"],["/posts/336911618.html","70aa85715d5f9eb8127353d35edd5a31"],["/posts/3402121571.html","9b5b07a524806a471258617fc8fe6494"],["/posts/3405577485.html","aae19f3f981c26ebc06f50315dd2cb25"],["/posts/3498516849.html","29901cec7eb89b5ef13477b204fad3fa"],["/posts/3513711414.html","09aea5d02510ece00e00a0924726be36"],["/posts/3546711884.html","76b399f7d5b52dc0dee37005144e4639"],["/posts/3731385230.html","fb088c80e14c59685c22072f24b066ab"],["/posts/3772089482.html","c18bb124c51ad222b59a9f82f9ddd483"],["/posts/386609427.html","878618b480065a5f3f7582c00ddc0013"],["/posts/4044235327.html","e09b57e713542e500e2e7d38696543c9"],["/posts/4115971639.html","5717a2e200b00bd105d050c572e78ee5"],["/posts/4130790367.html","832db88e7378e86bde47aad628fb549c"],["/posts/4131986683.html","be40135986841267661aedf18786e5fd"],["/posts/4177218757.html","556eb80a5398b61296cdaa6e175b669c"],["/posts/4192183953.html","802afe548d35a21b72d53a0e5be4ff03"],["/posts/4261103898.html","2c234bc013d18454f3d08ce75101a6cb"],["/posts/469711973.html","6ee8524177bb503322136f0a0c6a15a3"],["/posts/482495853.html","d459cfcd6e44dbba3789d88ee287156d"],["/posts/488247922.html","df146cd8bd66d0654a56bd88b85e53be"],["/posts/517302816.html","50c0f04b740a214a296956b0c654d311"],["/posts/570165348.html","34897076d06a050106773fbf3f7ac4b0"],["/posts/595890772.html","7703b32c3286e3e4cabc6b28d12e1e10"],["/posts/67485572.html","e6600a464912c504fba09a0569a762b2"],["/posts/694347442.html","36724fa8fb217cfb01d44316ac681ae1"],["/posts/707384687.html","3f1ca4ebc954df143c8da068ad7d0528"],["/posts/71180092.html","0e8fd7f3fcbc4983b65b55545a55a529"],["/posts/716459272.html","16aa3b47a504d8ba1482922d8ef20a71"],["/posts/765481613.html","044dcffa45c10895a100098a4aa03e33"],["/posts/778231993.html","0bda2fe45be9cd34165c81e2d27057fa"],["/posts/795397410.html","ab8d44103e513650feb2a84a44ab56cc"],["/posts/820223701.html","359fa65e08984c5b556ae3e0fa5be009"],["/posts/830372185.html","721e0a5db5a2aeed0ea53ee0d78143e9"],["/posts/88294277.html","d1dc1bfe31dcf675b90d69df36bb967d"],["/posts/939963535.html","c44420157497230d42db541d932c7cb8"],["/posts/983786067.html","be5929800361c483a48162d5eb8f7f36"],["/sw-register.js","fe2564b13465c9a139174435c670d41b"],["/tags/C/index.html","5da01eb4dac666565db75d0aa9d989c0"],["/tags/C/page/2/index.html","b16f6f898f263c7a68738893f3f3d2ef"],["/tags/C/page/3/index.html","b35d78cf68068861ece748933d4faeb5"],["/tags/ETL/index.html","818d820bffe7895c81d5470beb2d3195"],["/tags/ElasticSearch/index.html","ef0ba0e0a8e99641a7d0bf914cd4da3f"],["/tags/GUI/index.html","4f7bc32219d60fa3ccaa367bdb454c81"],["/tags/HBase/index.html","2b1cb7bf13a1c6c4f9e6644725acf701"],["/tags/Hadoop/index.html","efa276d94a1fbd27d3f31a367f30d3a1"],["/tags/Hadoop/page/2/index.html","c7eb56a4903b60bdb9b4c448dc143e94"],["/tags/Java/index.html","b8e777ad36d6c9a1b555a6def59e6fe8"],["/tags/Java后端/index.html","9dff5a608d479667bdcf97e72d4a2fb1"],["/tags/Java后端/page/2/index.html","759cb7cea288673a7eeacea00846db21"],["/tags/Java基础/index.html","0029a8c85a7453963c7972bf281b4959"],["/tags/Java基础/page/2/index.html","90f7f6d22859c7ec0205330869a299cb"],["/tags/Kettle/index.html","d119603119dcda4d85fe49ac79ceb3f7"],["/tags/Kibana/index.html","9c6241f4d240121bbe2ad6c4717ca1f8"],["/tags/Linux/index.html","a8086b315aea551987925247aaf55fc2"],["/tags/Linux/page/2/index.html","b9897fa06077999583267ea6a523c9ac"],["/tags/Linux/page/3/index.html","fc3570a5acde31a71993fd50420c0f59"],["/tags/Mac/index.html","57cfcf1761f5185adcc11abef21f6fa4"],["/tags/Mac/page/2/index.html","d42b1d9ae7f2045d4ffa867d9c0285a9"],["/tags/Maven/index.html","b493c27278f859c4880e81073fd5a964"],["/tags/MySQL/index.html","d3be4907d2c7303eac50047823a610f0"],["/tags/Python/index.html","e91fbb55c2144f253486fd113053a90b"],["/tags/Redis/index.html","dfabb57faf7f9081101fcfa3376940f7"],["/tags/R语言/index.html","36c21b89216875ab7279be35f858f9be"],["/tags/Spark/index.html","19ec185ec38c918a8059473e1548275b"],["/tags/Ubuntu/index.html","5fc44a9b101c400adcf24d3b5e1a5377"],["/tags/Vue/index.html","1aaf969d42ef1b762cf097eae0c5b3b0"],["/tags/Windows/index.html","da0a9b1351c7d03365669ad72dd13ae2"],["/tags/ZooKeeper/index.html","2e68cd14d58304519b1d941f0b4e6123"],["/tags/bfs/index.html","c03581873378a1f5838f8405d8516e22"],["/tags/dfs/index.html","2326a13d32da6285242bb85cea15f7ad"],["/tags/folium/index.html","e35138cf463041f97bbe517a90fb2d1c"],["/tags/git/index.html","bef067be482410866d4a82e628f462d9"],["/tags/index.html","8fe575f4b94d23e5d5c0ed42529faa52"],["/tags/latex/index.html","27b7380b886e9fe215397d9cb8fe298f"],["/tags/中间件/index.html","7ac644c34736371e1d0a9db92e5afb7f"],["/tags/二分查找/index.html","319f84c725d8bf9c74c2ae5a95abd4b2"],["/tags/优化类/index.html","94c84dafce6ca6157736dac7d48b2311"],["/tags/前端/index.html","a5de42a4a8d47b14c4dbbf1fc250fb85"],["/tags/前缀和与差分/index.html","d07c679c633f8a07fdc9b32f2f961eff"],["/tags/动态规划/index.html","e48193bccfe45f0ac70dce5cce00faa7"],["/tags/动态规划/page/2/index.html","d55b0ce70eb4baf94de31ad5826f6f55"],["/tags/博客搭建/index.html","6f52612bc5457acab8f784e89dbe86cb"],["/tags/图论/index.html","325d56624210c53f683bd2bf402b6fd0"],["/tags/大数据/index.html","2dbc809c7191153d076151f18dac1b81"],["/tags/大数据/page/2/index.html","d4ae0bbd18f2dd1d36006fb17afe2323"],["/tags/操作系统/index.html","1b2dca3fb74dddfe407907139cf06651"],["/tags/数学建模/index.html","d8d5526ff5e392425a86feb5aa1ab091"],["/tags/数据库/index.html","519ab14c9c1c290fbc8e66f4b63eefa4"],["/tags/数据结构和算法/index.html","ccdb8bfdc4f26877997de6a4951a7b7d"],["/tags/数据结构和算法/page/2/index.html","2d4e3fb96913e33a949ff196b8546f0f"],["/tags/数据结构和算法/page/3/index.html","e77bbc19aa9273996db00f7020de4ace"],["/tags/数组和字符串/index.html","c522118b1679f44b26722a47e2231c01"],["/tags/枚举类/index.html","6a2653292d8438a01f3be41f40932f04"],["/tags/栈和队列/index.html","bb6ab4990df2f6bfc35d2e215f22e27b"],["/tags/树论/index.html","a583e6160e17be2aba49c6bf0d7f5d5e"],["/tags/测试/index.html","7c47b257d2e62fc6a7f130ba61ee1335"],["/tags/环境/index.html","d19bb89b878225d91309524c78f3278b"],["/tags/环境变量/index.html","8a219ae83d12daf6f8dcab986ae513e7"],["/tags/绘图/index.html","15f0b7416c408996c3066d430c2023ec"],["/tags/编程工具/index.html","537ab113572327771b6e59b558fdf3a8"],["/tags/编程环境/index.html","3cc3f058b1789febac3e33290c817e5b"],["/tags/网络编程/index.html","df04b7b1b218e6ed875455d13b3fdec2"],["/tags/英语语法/index.html","8e7271e1f58fb10d17c1de3b132b02e8"],["/tags/论文/index.html","57e702e228d1c6629571d9d4c128501e"],["/tags/资源下载/index.html","22986b1ab245a41a7757f6deefd9a552"],["/tags/链表/index.html","9cd237c8c26f149ab58941ef9b1dacf0"],["/tags/集合/index.html","ddd08af864a3419315b4b227405b488d"],["/tags/集群/index.html","0db4fd4559c687af47b086ba89b564a0"]];
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
