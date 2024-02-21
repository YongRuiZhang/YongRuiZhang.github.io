/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2afb00265b3e1d47ade55cb1ff9fbf93"],["/about/index.html","e1cc442ae57e27a8486ec545676b04bd"],["/archives/2023/01/index.html","62e7fd3185e1d000ff992abc8230d255"],["/archives/2023/02/index.html","3f1b8ccae29da5c5f6c233012899da1d"],["/archives/2023/02/page/2/index.html","9291ef568c0f353eb7ab1c7567e5ba48"],["/archives/2023/03/index.html","c8083f02b097ed11ca87c7c3c9ab1597"],["/archives/2023/05/index.html","5e2ded8dae874582741e213057e12040"],["/archives/2023/06/index.html","5bc6ebf80785b267d5eb084d4aba9e97"],["/archives/2023/09/index.html","aa6c69593910dffdb780e52ec7bd6c27"],["/archives/2023/11/index.html","cc154d26b324ec0a9692872c1f301fb7"],["/archives/2023/12/index.html","bccbe7ff0353f9a96574db90fbb8b77e"],["/archives/2023/index.html","df03ac0e2d42a4bd679b3663eda3a3b5"],["/archives/2023/page/2/index.html","df559927782541c7be4ea8b3b017a3f8"],["/archives/2023/page/3/index.html","b02c0b6d1348eb040abc8510b9bb1227"],["/archives/2023/page/4/index.html","90d4efd958d38ad9c4c3d5408ae6c71f"],["/archives/2024/02/index.html","2d509bf06e2d6501978eb250ec73a8ca"],["/archives/2024/index.html","36e792302752b8a42a0259410bc1461b"],["/archives/index.html","0cfa7c0fd79a90b9e279b3cf8572c5c4"],["/archives/page/2/index.html","eb03a70d8b5a048720c63ad778955438"],["/archives/page/3/index.html","edcb17c8eb18a9b34c33adbe726bb80c"],["/archives/page/4/index.html","30169bb9e80a9208c14de66d496c7edb"],["/baidu_verify_codeva-qQP2iZOMLX.html","23dcead130ae2e73c83df5608d983aa0"],["/categories/Java/index.html","2a8b00d9d8de121ac5920804ff245898"],["/categories/Java/后端/index.html","9a6399fbfd9a8f3af200d8901e5156e7"],["/categories/Java/基础/index.html","22f6ac2198ce94c944afe7798238699c"],["/categories/Java/基础/集合/index.html","c7a9e2a9e8c40a6509e285e149dd2d86"],["/categories/Python/index.html","c1251ad7613cf7f9f33f4bc428cab92d"],["/categories/Python/编程环境/index.html","b66b619bc81b8757b09dc4aeee9cf3be"],["/categories/R语言/index.html","d98fdcab01346a360ee29441b18b460e"],["/categories/R语言/编程环境/index.html","ca84470999bdefce338e150286255fb8"],["/categories/index.html","9e75184a7656ed9980fda1eaea32c67b"],["/categories/中间件/index.html","05836dbd71fc73b153152d65d39f9487"],["/categories/前端/Vue/index.html","eef6ed6209508572cb7f12794959f34a"],["/categories/前端/index.html","2641c8c860399624925e53bedac248bb"],["/categories/大数据开发/ElasticSearch/index.html","2e11b4fc42abd7ff8008f7b1a8f6ee21"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3db03180275b751e16a24d4c1ccac943"],["/categories/大数据开发/HBase/index.html","75c76410267265dc5c1a329f9f339de4"],["/categories/大数据开发/HBase/学习笔记/index.html","28d7d87638d5d0d5912435f5f521af6c"],["/categories/大数据开发/HBase/环境搭建/index.html","3dfdd8bd12ed8dba9141ec6e021b16d9"],["/categories/大数据开发/Hadoop/index.html","c9d5221a992dbdde1393b84a0c6a29e5"],["/categories/大数据开发/Hadoop/技术/index.html","0e8b84bba01b27208bf73d783b758972"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d96f24a73b5711badc25c18a84a720c2"],["/categories/大数据开发/Redis/index.html","881d21ab2c0235b6e4a31dae186a5cb6"],["/categories/大数据开发/Redis/技术/index.html","64b42b99c5f4c736a7245382d5290aa8"],["/categories/大数据开发/Redis/环境搭建/index.html","54becb506592e98035636f3bbbbfde9a"],["/categories/大数据开发/Spark/index.html","66b4347f9f47e72d49b6a07174c9915a"],["/categories/大数据开发/Spark/环境搭建/index.html","55d89d1dd407a37a13172279dcf5acd9"],["/categories/大数据开发/Zookeeper/index.html","925e8cc12564398279df50be4b4b3096"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9dac9659dc45a6ce585ed4bc6eb71f06"],["/categories/大数据开发/index.html","c198056f996e5b2c2c89d8861a4a9282"],["/categories/学校课程/index.html","500c0b6a056e6eb215af4a538a6f5bb2"],["/categories/学校课程/计算机操作系统/index.html","6193ce519dbb4e9914f2d6b317db26a5"],["/categories/操作系统/Linux/index.html","0ccd0b07abf206f49feaf028b4b097fe"],["/categories/操作系统/Mac/index.html","56b0cc522dd66f4f18d99bd1137763ed"],["/categories/操作系统/Windows/index.html","3694df7bb1a2931b3c20758ab79a4929"],["/categories/操作系统/index.html","b7bc0d1257ebe340d997f3f55bb620ec"],["/categories/数学建模/index.html","1eaf1ccd48fa173bdad943e6319c42d0"],["/categories/数学建模/latex/index.html","e2c5e4f81f99da17b512422c3dd2e151"],["/categories/数学建模/优化类/index.html","5edb57f6064a2fe3b71a2d11d6cb9b3a"],["/categories/数学建模/优化类/现代优化算法/index.html","d391214825d16033f904055d7a4d1c21"],["/categories/数学建模/优化类/规划类/index.html","b75ed483b742d82fac76c04380fc07f2"],["/categories/数学建模/绘图/index.html","d592d53f4242f766bfeb2d5b2568e083"],["/categories/数据库/MySQL/index.html","f03336f2fb7977dd147ab4ac6538788d"],["/categories/数据库/index.html","a1d00e8ad865e86df3608a1beada8c75"],["/categories/数据结构和算法/index.html","d804e107778bc38a69ff988a51ea7720"],["/categories/数据结构和算法/page/2/index.html","a0891c63ef6f98965d01a7f8a46d8ba3"],["/categories/数据结构和算法/基本原理/bfs/index.html","4e435dbc39ff11a449e4359318456277"],["/categories/数据结构和算法/基本原理/dfs/index.html","a1b2bbbf848a73a20d93661f2907873d"],["/categories/数据结构和算法/基本原理/index.html","8268434d2ce1d78feedd95675482c110"],["/categories/数据结构和算法/基本原理/动态规划/index.html","6a32564a6f818347e2c3dcaa788fb7ef"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","833456e799cfd0fad8f6ccabd264c79b"],["/categories/数据结构和算法/基本原理/图论/index.html","ddc0aa110a82ca7f1ca1553b073af98f"],["/categories/数据结构和算法/基本原理/字符串/index.html","d6c31cdda7f84d2eb1fecca23bb84abb"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2619ca8e1a00cd422fa46ddc9e535ad1"],["/categories/数据结构和算法/基本原理/数论/index.html","d03efb7588694b3f6f7f0411d8d6faa8"],["/categories/数据结构和算法/基本原理/树论/index.html","57bca93b3f223818867dbf9ab50d3f90"],["/categories/数据结构和算法/基本原理/链表/index.html","d366c49c77cb2a0fdc284f39f926aea7"],["/categories/数据结构和算法/算法题/index.html","3c0d24d3b66864d75a64a6834a5c8f80"],["/categories/数据结构和算法/算法题/二分查找/index.html","06bf6a5ad096d3004b01d475c7f7fae6"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1b6559e2efd2fc2a0dd4f865fb45a8ed"],["/categories/数据结构和算法/算法题/动态规划/index.html","064006d747fac8ae78fd3567fbc21917"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","f54dc3df34e691af2513bed39d0ff93d"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","283082c335dbc52e7e141b0d24d8da05"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","acefab58e5e614af3ddee6bbfe016b25"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4ba678aa561b7c8bd84f359a68346a56"],["/categories/数据结构和算法/算法题/数论/index.html","1616f6d39c49dc4a8b4dd8967e810773"],["/categories/数据结构和算法/算法题/栈和队列/index.html","bd162631222cccbd490bbdf582b9f489"],["/categories/数据结构和算法/算法题/树论/index.html","d6b4343cd03e97f763d9fdc786af5935"],["/categories/杂七杂八/index.html","8e1d4f2b71ce3db2c4a53834a55be0d2"],["/categories/杂七杂八/博客搭建/index.html","8d31da27aeb993be388668be71e19f2e"],["/categories/编程工具下载/index.html","c4db4cfc1c4890a509a259e33dcaf252"],["/categories/编程环境/index.html","4a623b2b88ac7b7f66df3589d25b845a"],["/categories/编程环境/大数据/index.html","99e0c3a840a04591f0b6f1969d3707c6"],["/categories/英语学习/index.html","beec65c8db9ffee21ac9e3c592a0b873"],["/categories/英语学习/英语语法/index.html","4058f29955cd4e781556f474fecbb0cb"],["/comments/index.html","1810dc2769b6e35d467a533edcb4180e"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","6e6122e904b55b8d7a47c44bf5ccd0a7"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a77350dc075875c920a6435164f8f01d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","2e7f8b4ff317f86bad0a28eb53cc547b"],["/movies/index.html","731192b8d039e92014e3a8d4b423dce3"],["/music/index.html","4ca345fafd1cde27179e8395bbebb51a"],["/page/2/index.html","4ffd7f51a1aba5b19eb68d26a5c15598"],["/page/3/index.html","11cdca9feb0c0e725dd0d9ab95772578"],["/page/4/index.html","325986f91fb355540aefd0194ddc4906"],["/page/5/index.html","f77077e5f086dfbf6323b84c06317c04"],["/page/6/index.html","7d19bc61d9a992a1ec59ae71c9de67eb"],["/posts/1021360842.html","6baacab72aef08a8e1c82c4b7a7b6f40"],["/posts/1120620192.html","d73d586f4278295ac3d03c919881bbf5"],["/posts/1141628095.html","15c78c31b615170af6848aa6f6fdb58f"],["/posts/1168613674.html","f71133ab74e5a1e591880fb556d9d236"],["/posts/1219920510.html","4b171f03d297e9b616ce3878c669f80d"],["/posts/1222166338.html","e812fdb685b37d48d0883479ac40c326"],["/posts/1259097482.html","b88cc02cfa09516946ed3a2de0389e90"],["/posts/1271036369.html","fd2dd1d363e8a7d9c18f0994bbf739a9"],["/posts/1312847445.html","9a82ad493295fa2f725075be6451496b"],["/posts/135355774.html","c5bfd44fb5ede2a4baeab2417edd16eb"],["/posts/1375344716.html","ae606d5ed9cfd5620060d7090f29dea1"],["/posts/1388991698.html","b5bc391ec144f925b5c9dbb05449caa2"],["/posts/1410315814.html","3e7460d9f90c2488feccba84da58e082"],["/posts/1452790229.html","947178222c1b99f28dd5e7d5c3600496"],["/posts/1470079884.html","a3d7c386c758d85cc7873bf349aa06ec"],["/posts/1470079885.html","6091344cbf204e8cd6e9adef05be297a"],["/posts/1470079886.html","dfc93b0f81005397acf3bd7a1364591e"],["/posts/1470079887.html","dc842b79132405539ead1953e8d9d010"],["/posts/1498536549.html","583cc236b4ead4d8018576a1ec42e7b0"],["/posts/1539568593.html","58dd5c855736a09738112b5043933792"],["/posts/1547067935.html","cc79683b5182c712b15c27fb3ff8f6e4"],["/posts/1557866301.html","37a59038b70f4c72b67c5aaaf162403b"],["/posts/1571776361.html","95feea2ffe7d08262f380c6a9b1412d9"],["/posts/1605124548.html","cbbbf00400ce68651a12d175becd18bb"],["/posts/1633036852.html","92a85cf07d340776430ec2e38baad559"],["/posts/1674202625.html","b5a074d144c5b40d175a9678bcb91ec0"],["/posts/1765123828.html","6e1e9aa0dadb2c1fb5a76bbded4cfbce"],["/posts/1767336200.html","877056163493d4ea21394c38f9bd8efe"],["/posts/1776114197.html","f49c8a3fd225960e84a9024fcdeb181f"],["/posts/1817748743.html","dbbb3a4a869f61b98337845af7949f77"],["/posts/1925125395.html","c70487766d69804a2b2775f38c478738"],["/posts/1966191251.html","180c6bea4d524604b54c81f3116a551c"],["/posts/1987617322.html","e5c4c1dc93802ea32abcfb5c5e54d7d8"],["/posts/1999788039.html","f9c0b357cec00c840c6988d4b827a19a"],["/posts/2075104059.html","9b2b813e6278244db7b16bb8d51eff1e"],["/posts/2087796737.html","1adb0488373e10fd7dc710dde5b1b7e7"],["/posts/2106547339.html","8f9d611d05efd68202f286462ab52fee"],["/posts/2207806286.html","6142f705f6561a5a9c49612c0918c4a1"],["/posts/2225903441.html","0915351a9932e77c8ffa6ceef0dbc8a8"],["/posts/2265610284.html","f0b43916b5651036d59cc3656928fe88"],["/posts/2281352001.html","7f3ce10bc40da6aead37dee3cf900eca"],["/posts/2364755265.html","aab085df67b6844d8f961ad88b651e33"],["/posts/2414116852.html","e74e8f3fbc96af85f4a455f9203b3687"],["/posts/2421785022.html","8d8e05415a7eedffcdd8cc48b24112f3"],["/posts/2482902029.html","2c88b0b09f88f5c46d186eb4fb63d04b"],["/posts/2495386210.html","caf915f48a923aaecb14faa455510002"],["/posts/2516528882.html","fadca9e16783feac1af7484363953e92"],["/posts/2526659543.html","64f8f6f68a55b7348f6ee1c11f2b0005"],["/posts/2529807823.html","00e54a00887e36bcb7e5f0236c2a40a9"],["/posts/2596601004.html","cb9086eaf408d14984592893fe3494e5"],["/posts/2697614349.html","703367cb8b12181c1e6567e03d725d2a"],["/posts/2742438348.html","d671373bbeaa09631b16d70b6f41e9a9"],["/posts/2768249503.html","25d6c5c9856d54eca3075442f4573a3f"],["/posts/2864584994.html","11f85880238fb093b098adfb9901ed5f"],["/posts/2888309600.html","d1e42d37334d677fb5c9571db02f654b"],["/posts/2891591958.html","d671f8d9c4119fa8124789e8c8ebc52d"],["/posts/2909934084.html","1ad6031b3155a64cf341d906bc8a30c9"],["/posts/2920256992.html","f11159178cb65ea2e5affcce3a7b4cd0"],["/posts/2959474469.html","558a82a7a693e922745f1ecc953ec16e"],["/posts/3005926051.html","b08313e240d001d691ba1957dce16e53"],["/posts/309775400.html","f3fc6bb47fe932ce075b84a0b5d1d817"],["/posts/3156194925.html","b869c223794e94a9ec4aca87b73506bc"],["/posts/3169224211.html","c7351ea7aa8ffcb44f05a12fd4abd108"],["/posts/3213899550.html","23a380fe78c54e35297eb123f7de2f4d"],["/posts/3259212833.html","b6f8c0ab78f321249401c1b19027d5f6"],["/posts/3266130344.html","7ddc2fc4caebd5c260915c63bef4aee8"],["/posts/3292663995.html","5b89f34cbdfd17b606e3e7bd48b29b2e"],["/posts/3297135020.html","9aff174844d233120eeb1f6cc2722f42"],["/posts/3306641566.html","fd051f63d56b922ffa23e59381b28e9d"],["/posts/3312011324.html","a8e087b1515d9ee1b22b8689cc084df4"],["/posts/336911618.html","9c1178f8c275e0dbd5c9621614c67b1a"],["/posts/3402121571.html","3795908a92ed747499895682e2632f6a"],["/posts/3405577485.html","4fe2de98ba69d87297c3af9cda65e5b2"],["/posts/3498516849.html","19555c5f0301869cfb3405ad0ab57079"],["/posts/3513711414.html","794b2653e6f140c527d5d364f4e05a12"],["/posts/3523095624.html","4ffb79713c1f330276531fac521ed37e"],["/posts/3546711884.html","f53266eb6fb627cf9ab863eabe14359f"],["/posts/3731385230.html","8799d7d9a63fce273b0923bda7dead53"],["/posts/3772089482.html","e299834bb36a374a603b6539fc1e9b10"],["/posts/386609427.html","74d9c7adb8aacc7bcc536583ec5ac836"],["/posts/4044235327.html","04d88a075f28a5259543b12ed50a96f1"],["/posts/4115971639.html","dac5a4ef5deeb7ce3d08b8a00f5c6e55"],["/posts/4130790367.html","7a6239bf4b2e438abef7acf8c5f6d1f4"],["/posts/4131986683.html","96eeef846fe318cd5e691598da4f05d6"],["/posts/4177218757.html","c44675154d6f66bb3e49a58aa5f23794"],["/posts/4192183953.html","6fc3a1660f8d7722e1042fdf163f61dc"],["/posts/4261103898.html","b8d3f59fa0ead47ea897142aa84c24c3"],["/posts/469711973.html","598138ca728cc244a80704063ecd578f"],["/posts/482495853.html","34b8eb2de02d31e66147b429dc15fd2c"],["/posts/488247922.html","38c987770612483d124897ba4dc4c559"],["/posts/517302816.html","d79dbec74def672aac6db292d8417fc2"],["/posts/570165348.html","519344145fd5eaea3dcf0b13e49fa7de"],["/posts/595890772.html","59bbf1f21277485488a19861b3d686e2"],["/posts/67485572.html","f69bb33364f9fafe0270094a91d6d04b"],["/posts/694347442.html","a8b1203f2c674e6f4e75f9eab879b759"],["/posts/707384687.html","a5ee33b3721c37dc2e13e4b81c3d3eaf"],["/posts/71180092.html","432778f4e3afa5de05e49b55c3c4f253"],["/posts/716459272.html","5e4d27d0185a5ad4f9aa872e07307fcb"],["/posts/765481613.html","b8acb8de8037ca0ea2c08d525bc76b08"],["/posts/778231993.html","f46c8daa55e793ce1053ab6c39622414"],["/posts/795397410.html","c50fb33fe41e3fff16d0970c8931784e"],["/posts/820223701.html","f0c7676d8da93536e9e30e51091de6a7"],["/posts/830372185.html","f18c5dfb67c84ee6b64ed31b73fa90fd"],["/posts/88294277.html","250e2b569a6166d8510b3ced385ba5ae"],["/posts/939963535.html","950c8bed925d920edabe73bd8dcd3001"],["/posts/983786067.html","68448a51a91e25c20b8c73d44e451b6c"],["/sw-register.js","fae71b24ec595cbf780a9d978a4bb5d4"],["/tags/C/index.html","ecb4e9905c9074a1acfa85ea060ac343"],["/tags/C/page/2/index.html","3e12b5657fedaa523177220e706fd7a8"],["/tags/C/page/3/index.html","65ac2515464403e2a455f7deef1ce842"],["/tags/C/page/4/index.html","25a2bb4b472f13cb999233aa3b0dfb4e"],["/tags/ETL/index.html","8f5b56f856a6b253fbd5d5770ec1445c"],["/tags/ElasticSearch/index.html","c4e503ae09250fd5b8f999faad459f9a"],["/tags/GUI/index.html","1eb50cc84f432ed75ef5edb671b4d2f2"],["/tags/HBase/index.html","c58bc9afd6be38bf94e147a82038af88"],["/tags/Hadoop/index.html","edab7154659d6fcdbf8b25a1beeb183a"],["/tags/Hadoop/page/2/index.html","55afeb5ca7d725bc0812decd4f82d1bb"],["/tags/Java/index.html","4af56292945eda57f6aef3629ea75e25"],["/tags/Java后端/index.html","745b0f703b6acefd2be2b52e2ec4dac5"],["/tags/Java后端/page/2/index.html","4ef603e0732504cb727c96df917385a9"],["/tags/Java基础/index.html","dabee9431990dc5d4b0b173c5ff93fa5"],["/tags/Java基础/page/2/index.html","ac7d8bf3b7a846ed6d9fbd08b89f8311"],["/tags/Kettle/index.html","e4b1263714bbef9f7c74cd602654bb3c"],["/tags/Kibana/index.html","441aeb7cfe7c7683396609c63b3b104d"],["/tags/Linux/index.html","f16fba0c34cf6d98048ce5eea605bebf"],["/tags/Linux/page/2/index.html","fc6f0e73858bb18922fb6ce74f822ce5"],["/tags/Linux/page/3/index.html","791d35d02e2066b677892a50db7cc74c"],["/tags/Mac/index.html","88a068d3bbc19fe49539ff6d3262fd2c"],["/tags/Mac/page/2/index.html","b30dfb8404c6f37000bacbc409da6ad7"],["/tags/Maven/index.html","d326e1b26abd469ad625d0701026da1a"],["/tags/MySQL/index.html","b496eb28ced20618e97b2f015afa6868"],["/tags/Python/index.html","368490ab20b07d8624955677325a30d5"],["/tags/Redis/index.html","6e40902d9ece88ee74ad3ce2ba4f89e7"],["/tags/R语言/index.html","db7c345537bcd8ca01086e43e578f6f9"],["/tags/Spark/index.html","e518a96b2914c4e29a443d0b1dfb53d6"],["/tags/Ubuntu/index.html","07eb396db6dddf9c6b96fac440bede4c"],["/tags/Vue/index.html","8df78744e2b01a440349675874abea45"],["/tags/Windows/index.html","cfcfb11886d5c7df73aac62d4402146b"],["/tags/ZooKeeper/index.html","f45118a701205082e3f537eb2728a3c0"],["/tags/bfs/index.html","5e4e6ac0eda85d24026022a6829da102"],["/tags/dfs/index.html","f91081d897896cef7dfa803ed9081226"],["/tags/folium/index.html","ba220910d4afd571b328744383230bbf"],["/tags/git/index.html","3ea06a164e8e4d6601cafe1a2a74ea49"],["/tags/index.html","1b15fdbceae58f2fb470ff7970b064db"],["/tags/latex/index.html","b6c0e7d2ce5bf83f1f1a0ab5be6f9f58"],["/tags/中间件/index.html","133a7f99fad412f7693f80ca6264e7bb"],["/tags/二分查找/index.html","af285ae06fee681bfdc73de6257d84d2"],["/tags/优化类/index.html","20a67f1c82ab7d5cb453b9ad3b121129"],["/tags/前端/index.html","9de346dde7a1730a84552f95dff581c0"],["/tags/前缀和与差分/index.html","aef1235a075f151dff98e98d03e8f2a6"],["/tags/动态规划/index.html","7cc8122b07e5d9a3b157d0a146dc5919"],["/tags/动态规划/page/2/index.html","8d5dadacc04e380607698eab93e1d69f"],["/tags/博客搭建/index.html","d3b5bef7a7750a0a429e2ef23b704a3a"],["/tags/图论/index.html","7e369102ae599ed84074fd0002cc1a70"],["/tags/大数据/index.html","9819942466b9f36919d5a61c269b5833"],["/tags/大数据/page/2/index.html","d3c1252d78a079c4188bd1126c64a6b2"],["/tags/操作系统/index.html","35b9c56e784e6adb366eb194ec7a9110"],["/tags/数学建模/index.html","967c19cab9cb7cf87f5904ac3709fdf0"],["/tags/数据库/index.html","a9b0c31932925d9d5ff24f392de01593"],["/tags/数据结构和算法/index.html","b87d3fa9463178d6690e11054fc2ed25"],["/tags/数据结构和算法/page/2/index.html","384d3981f63998340e8c9936f8e24c80"],["/tags/数据结构和算法/page/3/index.html","939f8a4558cb057b50dd00984aacf78e"],["/tags/数据结构和算法/page/4/index.html","9b1e62edfa298364f9ab60cfffd265c6"],["/tags/数组和字符串/index.html","c7e12243eef281855ffba2dce32aee40"],["/tags/数论/index.html","f3b72719156b397e983db6ba6cc47e0b"],["/tags/枚举类/index.html","486c5b612cf5a95a9451ede9d952aced"],["/tags/栈和队列/index.html","b7fa7e39f271c4aafcbba9a6b76a91d3"],["/tags/树论/index.html","384984c011ca172432fa6c9d55f80916"],["/tags/测试/index.html","841e0dae8d7ea6b3b095a46db241c1d1"],["/tags/环境/index.html","04d1162d92570dfafa4ca0f93fdb9887"],["/tags/环境变量/index.html","95038d5a947eda777fa13db85b9c037d"],["/tags/绘图/index.html","f1ed8337f557cb8d0ac2a1b1fbe3a888"],["/tags/编程工具/index.html","f686354f0a203fa2ee426a6209421bcd"],["/tags/编程环境/index.html","5ab4be6032dd92d93fe2990384035e54"],["/tags/网络编程/index.html","57662ae846eda61cf3842967101c4004"],["/tags/英语语法/index.html","a6eb747e201de83c6f3c891c5a390176"],["/tags/计算机操作系统/index.html","d8bf61efd01f5a4cf76e6b0823a9b820"],["/tags/论文/index.html","ceb03d150af79b8d5f43e4ff69a7c27a"],["/tags/资源下载/index.html","ea0a2a9e0a424daea7c14262ac396cc1"],["/tags/链表/index.html","a3d4979cbed61d7089e61bf6591032df"],["/tags/集合/index.html","8c828fae859416f7367e8ea147904d7d"],["/tags/集群/index.html","d6bc7947a305ae2223c28e11ae92950a"]];
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
