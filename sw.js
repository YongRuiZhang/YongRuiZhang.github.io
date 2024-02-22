/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","c358ee3d432143a31fe2696cedfeeb61"],["/about/index.html","fbe1dfe8a84e25e5630a7eed513d7007"],["/archives/2023/01/index.html","382d999b53a054b5c7281b86cd01b00c"],["/archives/2023/02/index.html","774d9ac65db1716533d404f252979324"],["/archives/2023/02/page/2/index.html","de2543ad7d66ab29d8b594edac61b747"],["/archives/2023/03/index.html","c608ba304ba7ae0e262857921e458820"],["/archives/2023/05/index.html","adf598f1d29168f1ba8750a36f6267b5"],["/archives/2023/06/index.html","ba9e37fdba400cfe7e50ee2e9d553653"],["/archives/2023/09/index.html","99d5cef337fed0ac2dcf0d6af760e8de"],["/archives/2023/11/index.html","864765f9cc1ba824547cdf4a7d5484a0"],["/archives/2023/12/index.html","ce08781109939e64e55561516dadad6d"],["/archives/2023/index.html","956d6a6d70df9880ef0833e3934d25b9"],["/archives/2023/page/2/index.html","9e3be55f8a7d9177482f8b5ba3c023a6"],["/archives/2023/page/3/index.html","eacf7453311c399399010b236f9ac4b4"],["/archives/2023/page/4/index.html","bef926c03d63bb3acdf3a4de7f0f1899"],["/archives/2024/02/index.html","5881bbd6336f8a8c7aa7980c54cb5918"],["/archives/2024/index.html","a18500cdbbabf0e29dbcc6ae25343f78"],["/archives/index.html","3667c84de8bee0e7dd0821a95cd3079e"],["/archives/page/2/index.html","0fd2c046b72e290532333553a4fe62ef"],["/archives/page/3/index.html","9dd9fb771c2d1a92866452738400282d"],["/archives/page/4/index.html","fd1a44184d890f58944e325bba031735"],["/baidu_verify_codeva-qQP2iZOMLX.html","24b6669ee9bd5a9e6b55b282553f005d"],["/categories/Java/index.html","d325824a8e7b682c6ff5578586324a0c"],["/categories/Java/后端/index.html","7023f56f78d8ff11939e15bafb4c4aa8"],["/categories/Java/基础/index.html","5fde5ac60531359c9062bcff0b622c97"],["/categories/Java/基础/集合/index.html","9bc6ea3a2d8e623fff2014c0bbd1161b"],["/categories/Python/index.html","1b3c8672d37cb9f053816a410ed21f13"],["/categories/Python/编程环境/index.html","2e1e8a9b591e86a04f8f625b4d24bdea"],["/categories/R语言/index.html","f15a51c5ae70051ce0b16ba41a439d00"],["/categories/R语言/编程环境/index.html","4b127abd92cd63a3b000f72913c7601f"],["/categories/index.html","c5cacc817576d248b2e1f1befa36b6d8"],["/categories/中间件/index.html","9627f9c3293fd757172611b91577ca33"],["/categories/前端/Vue/index.html","b378865078ee081ced3ae0649552e8ff"],["/categories/前端/index.html","8a173f4266769d15d8f20c1a5cdf6931"],["/categories/大数据开发/ElasticSearch/index.html","2614a8fa1790d42cef9c29f2919c6ed5"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","1b41bf039ebcd57a403790e2a024b6a7"],["/categories/大数据开发/HBase/index.html","b1948329e4ae9d05e50399349e4fd10a"],["/categories/大数据开发/HBase/学习笔记/index.html","60d2b93cfb2fb9a493cf04b162435f3f"],["/categories/大数据开发/HBase/环境搭建/index.html","b43fcd8bce939463c663e636928db2a1"],["/categories/大数据开发/Hadoop/index.html","6b855ec1bcac6b7c9cf06e27e2002d5e"],["/categories/大数据开发/Hadoop/技术/index.html","25f3c020c55b7ac11a5e56b9235b406c"],["/categories/大数据开发/Hadoop/环境搭建/index.html","c7792102ce1f4c8319c59005fc79a3e0"],["/categories/大数据开发/Redis/index.html","a6619ee5f30848670a986d79d86df4b6"],["/categories/大数据开发/Redis/技术/index.html","efca4f031b0992ffc1f4fefacb038ee6"],["/categories/大数据开发/Redis/环境搭建/index.html","0973b984d804dada8d2524ac45f62956"],["/categories/大数据开发/Spark/index.html","75bc026e06a1072938c573ef85ab8a67"],["/categories/大数据开发/Spark/环境搭建/index.html","7b95e8c5cab4533361caece74e1d1bab"],["/categories/大数据开发/Zookeeper/index.html","2db76235aaba4fb48e501c16f68cdca9"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","215c122bdcc276ae3d70816bcb55e94e"],["/categories/大数据开发/index.html","dd098a532ae1351ccf0c3db6a65999c5"],["/categories/学校课程/index.html","6431fbd395270184c788434acaded3be"],["/categories/学校课程/计算机操作系统/index.html","600322c303f54c2c874ec089590af838"],["/categories/操作系统/Linux/index.html","79cbb952af2226bbcafe4396da44ac03"],["/categories/操作系统/Mac/index.html","d50c8f76d38411e990118410713d09e3"],["/categories/操作系统/Windows/index.html","58c7f74408acc3a5e7dd1fd53cbeb239"],["/categories/操作系统/index.html","20b78484ea136dd8d8b8cb34a45a4cef"],["/categories/数学建模/index.html","a46ef6a722de96acd1469f0d352e39d5"],["/categories/数学建模/latex/index.html","ecf797f9d83342cb891a9716a955b710"],["/categories/数学建模/优化类/index.html","2c34d7bae7d98d66ba2bdc405fe81ef8"],["/categories/数学建模/优化类/现代优化算法/index.html","e1b3ec569d085a9806afda7f9b781e5c"],["/categories/数学建模/优化类/规划类/index.html","c7debb13665819c70c1c2abc7ed09060"],["/categories/数学建模/绘图/index.html","51b76be746a6c30666465f6646a2f00a"],["/categories/数据库/MySQL/index.html","3f86218ac97c4592ec72857a71b1fd8f"],["/categories/数据库/index.html","6282b0669e39fb0b6a8ad43036cda68d"],["/categories/数据结构和算法/index.html","8a026200848c776028cd613e011b6789"],["/categories/数据结构和算法/page/2/index.html","b572a6f24c64d25ffbb401e97e29e24f"],["/categories/数据结构和算法/基本原理/bfs/index.html","437679727cf3f9cb22aa4f744c1a8aaa"],["/categories/数据结构和算法/基本原理/dfs/index.html","9ba88f6a2db032843e25de17468dc7f2"],["/categories/数据结构和算法/基本原理/index.html","58428c1ad6b8eb3016f7a710d64c406d"],["/categories/数据结构和算法/基本原理/动态规划/index.html","108b46e7b34955bb3fdfbc00455f24fc"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5e58ca5a9a48b2b8f5d904fb88502567"],["/categories/数据结构和算法/基本原理/图论/index.html","f246a9bb8e369db2f66bcd62fc2dc102"],["/categories/数据结构和算法/基本原理/字符串/index.html","f985d0977b0b99431d3ef76f1460f5c9"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","dfe1d04e7a4e5f1bd934de71ef7bcb97"],["/categories/数据结构和算法/基本原理/数论/index.html","a6d3139458654c3f76f7bb642f0cd3e0"],["/categories/数据结构和算法/基本原理/树论/index.html","4e3aa1a5dd97950a1f5c85cb29051c8a"],["/categories/数据结构和算法/基本原理/链表/index.html","56edbf9d657b03e99bde58e8b5af4444"],["/categories/数据结构和算法/算法题/index.html","ae52b4794312ff332aabc1a5c1296167"],["/categories/数据结构和算法/算法题/二分查找/index.html","6339993b90db5bed068d652debc958f0"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0f3c73e1471a2f3edac67cba8e4893f8"],["/categories/数据结构和算法/算法题/动态规划/index.html","50efa1714e3537285b30f0576e1a1f5a"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","98ad2005f6f6de03bd1d2d865ae1556f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","7da0ff5f753b40e5c56830452f747e9c"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","3de353f634735595d85ffed09f6cf019"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d1e726b8d9c7795d58ad986bec6f62b9"],["/categories/数据结构和算法/算法题/数论/index.html","ea55b9faf37edc11367b67658caa8a61"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8abf4aca7f7dbaa711c72cf616b4ad1b"],["/categories/数据结构和算法/算法题/树论/index.html","8fc52cc5cdf4ef86d8df42697ef7e77c"],["/categories/杂七杂八/index.html","9c540d2277e0d9b33b8ac39a18f5f77d"],["/categories/杂七杂八/博客搭建/index.html","b7484f1910d443c81ae906d8c605df91"],["/categories/编程工具下载/index.html","22216719d1ee8f8a946928794ab8b4bf"],["/categories/编程环境/index.html","af6dfd134a5f1cedbfd8c7fdebd46848"],["/categories/编程环境/大数据/index.html","2e65aee2e96de10b889cc610d3cd27d0"],["/categories/英语学习/index.html","4fd8518ab660b2853c667c9bb02ab491"],["/categories/英语学习/英语语法/index.html","9b4db31e5bfbf533efb5292cb4676cf7"],["/comments/index.html","5009a8e17f901021359f6cf17bdb9867"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","192cfc6a74f6e3f4e91a9098aa2f64db"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","26e6099f18a53de3e34b79553a55ff41"],["/movies/index.html","81aad9a6044462b54fa935959ef3708b"],["/music/index.html","d14c8442c085e58f669c885e24c4ec13"],["/page/2/index.html","77128fdadff5eecc8242b59cffe0cc9c"],["/page/3/index.html","bdc8dc7c4eaecef9952f511d5aea4bf0"],["/page/4/index.html","03b5e4997a503540e8d22ad25212533b"],["/page/5/index.html","319873264c64fa838e327f9868e39824"],["/page/6/index.html","f1e0bd905d9465d6a19659bd4636d04e"],["/posts/1021360842.html","c2c9964341f1e30c517fbc6fd4de32b0"],["/posts/1120620192.html","c41e5287ad61fca2dbd9739e4c32e0d6"],["/posts/1141628095.html","55f0ea7cdbfb4678d872c3d446ac904a"],["/posts/1168613674.html","63bdebc1cde6f55dbf4d1086271cad01"],["/posts/1219920510.html","a6be1f718321f94c0fdab64188ecce1b"],["/posts/1222166338.html","7cda322585db9a103b6317ffd3ed6339"],["/posts/1259097482.html","7942a06ade5b41b6ce75c81a0e2c3afc"],["/posts/1271036369.html","793e59410e81c992464145124b1366d9"],["/posts/1312847445.html","a6cab356db8b618b13d826a1d7ea4754"],["/posts/135355774.html","ffac757146ca32d885d7195d7cc3ad0e"],["/posts/1375344716.html","b946c55344c3ee823f121f7b1c022831"],["/posts/1388991698.html","231fd0ec2d2662364002c82ce31feece"],["/posts/1410315814.html","4fb604661eff4d4a28b06e5c06d68175"],["/posts/1452790229.html","32b8acd6eb8aa636d7ea49884c5fdad5"],["/posts/1470079884.html","e67bf34c4640c150cae08d73fab66af3"],["/posts/1470079885.html","c4c889c9c8ca599f45f5961cf73f4233"],["/posts/1470079886.html","818bc8a9778126079453162ee7adcf57"],["/posts/1470079887.html","386f7bd2a677ddd5cd8a46c16a567440"],["/posts/1498536549.html","8fd4702ba26c374d566c4ec4dbf5259e"],["/posts/1539568593.html","72ec3e019b95cbe7bf8b37aef48c1962"],["/posts/1547067935.html","40a4d184ada20bad31fa3d68e4e43fbb"],["/posts/1557866301.html","26d306e50b1de01a2f3786880d8a110a"],["/posts/1571776361.html","fb40cc0d554f3829aa371a7918b1dd0d"],["/posts/1605124548.html","ffbaa44e6e73e68953d95e10d0acd47f"],["/posts/1633036852.html","0d74bc00a1e8b27570407ec2a13e30db"],["/posts/1674202625.html","ec0174a242ad32359de841df1998e266"],["/posts/1765123828.html","dab4c6c6efc50c00b01b7f0e64a552e1"],["/posts/1767336200.html","2d696397a1e7df769ffb9a3f755ca13c"],["/posts/1776114197.html","e47306d2e34e7a0a20b507089b55ebea"],["/posts/1817748743.html","028c1e2ac8e0a2d3e9e23fe1025022b4"],["/posts/1925125395.html","4bba878aca5eb597a4a70009a54437f1"],["/posts/1966191251.html","bc046f0b1186b86d0ef20d27bf380fbc"],["/posts/1987617322.html","faab9a83d308f7699ffbbf49191f081d"],["/posts/1999788039.html","d405c90c4a4d024eff80f283b8ad0ef3"],["/posts/2075104059.html","57ffeed439d08be29e767b0539999f59"],["/posts/2087796737.html","b1e9121819b23da3b9a64164c5f8bc43"],["/posts/2106547339.html","a80a4dea630caec186b639771da84f4e"],["/posts/2207806286.html","cc7b55036e8bfbb578bc31ece6c5df72"],["/posts/2225903441.html","30bce87aedf52bd491dce7368c93a1bd"],["/posts/2265610284.html","0a789e4b47621ee42d75cb8c24e64892"],["/posts/2281352001.html","8d37abfa9e1009697c7c0880bd01c7f1"],["/posts/2364755265.html","848822b64b0b0c98d767e4c553e057f7"],["/posts/2414116852.html","38986831bc052467e7a65acee6b44d25"],["/posts/2421785022.html","5ac51a791b82d85c24499d71d2fc255d"],["/posts/2482902029.html","5e70214cad91f5db923d20d4a8c5aef5"],["/posts/2495386210.html","f0cae8046581f68113c8298c60dd0277"],["/posts/2516528882.html","514beb509cc83a76455188854015ec7c"],["/posts/2526659543.html","03bf34f90d733e17cdcf4ed24ae59f82"],["/posts/2529807823.html","b9d0881c2ceddd8c56faa2b5187c9244"],["/posts/2596601004.html","7b3fcdf194e72c2eab51f45eb6d16cfb"],["/posts/2697614349.html","8d6e3240e53afbce0013fc9f494b7ad2"],["/posts/2742438348.html","5de3e44d14177e9f4101654728e3335c"],["/posts/2768249503.html","f31f9a6608b72b638d18b3a2629f9b6e"],["/posts/2864584994.html","d87fb119d80f06a07fef46e069bc0bf7"],["/posts/2888309600.html","67b1fe285e0bea7d092c43c55767db5e"],["/posts/2891591958.html","90d1b2efbf22d33b3fe119df60d2121c"],["/posts/2909934084.html","10623333429ae037e78aa7d5e7d8c69c"],["/posts/2920256992.html","98e428655f4e5fe6b48b7a6e74e64276"],["/posts/2959474469.html","d2b0e6751c9e5a1c4343a50a8421d055"],["/posts/3005926051.html","89c015e1e89ad3576607603dd1bc1fcc"],["/posts/309775400.html","11b9d4a541d7a15b04930a1265f2becd"],["/posts/3156194925.html","fd85f3e4bb9dc02b0814a5b397db1329"],["/posts/3169224211.html","029993deb3130a67c869352e2aead7d5"],["/posts/3213899550.html","a20cb9c5297caecc366776e512a3c5b0"],["/posts/3259212833.html","b49b1a10577da7c524c988649e6d9319"],["/posts/3266130344.html","9b227cc917adc71bf552b400e736e993"],["/posts/3292663995.html","9cafab02fe24f76cc020a2da0675472d"],["/posts/3297135020.html","075ccba68581bc8fc892b39f6429623f"],["/posts/3306641566.html","729393c6f34bdc5ca07191b704136ecd"],["/posts/3312011324.html","7c95e65373965dee482b64d53889af36"],["/posts/336911618.html","7685f2b14000c9e7acb80b137dfe84d2"],["/posts/3402121571.html","14b0d51d4e5d9f66c54733a3ca05dc3d"],["/posts/3405577485.html","20713e747117b7ccdf3f0b34ba45e421"],["/posts/3498516849.html","2c933307d2d00c052a03c3bd9d984644"],["/posts/3513711414.html","446ab7a0d09313b7c28aa10f59f5daf6"],["/posts/3523095624.html","674ca50eb748db0f1f70746203a07f68"],["/posts/3546711884.html","e3a96404de3e4bc0d0efc75696d041e7"],["/posts/3731385230.html","f2425ff09d6e7a5660331fd6103cc268"],["/posts/3772089482.html","ac3088717827d75e487f24352423c299"],["/posts/386609427.html","9592b64cfdd424ef93ff4f97e98df620"],["/posts/4044235327.html","e94f9bcb1a60d01636927ed1ac36aa40"],["/posts/4115971639.html","4d8f17f1fa3d3d9db73c887564d4a5a4"],["/posts/4130790367.html","521447decfd5e2fc04bbb052771148be"],["/posts/4131986683.html","8aeda1806a095b7f0bd2bcfd8180047c"],["/posts/4177218757.html","bce54ba66ee37b7264676a0c76a5dc82"],["/posts/4192183953.html","e3033cb00f7be84ed8b8497ac169b441"],["/posts/4261103898.html","e3abb1d95300e7f338622b044e5e4d40"],["/posts/469711973.html","483cc60eb6f3d20ade2c2babbb8b26ae"],["/posts/482495853.html","e0277123858387cb70f9ef9b778424a7"],["/posts/488247922.html","3c5c34438c96c622a936691038649c1f"],["/posts/517302816.html","c65ac98d6d7316c86cc04244b7510932"],["/posts/570165348.html","a5506da02bea9d8ffc1800e9cfc2841b"],["/posts/595890772.html","15cdf31cc72f919996b7378098ff05a2"],["/posts/67485572.html","65a6f6055b12dd8caa503b71b0ff1667"],["/posts/694347442.html","d1441563d4bd60ed7c766a4bc2c3e64b"],["/posts/707384687.html","497e6c5edc22bfae69d44cb365e7143d"],["/posts/71180092.html","22f525db0a3c6c1fa23430f4f1fe902b"],["/posts/716459272.html","216ca8a45ef6e7fa5c11da752dbb6e52"],["/posts/765481613.html","3e0f8b68e43e1db87e5f499fc62ca32f"],["/posts/778231993.html","78128aeebc298fc8d0fc326f3a25ce27"],["/posts/795397410.html","0d3fa00330fe088d23e2c2eeb5b3c5e6"],["/posts/820223701.html","145eec815b4326d682fccea5233c8c2f"],["/posts/830372185.html","44e20313bd0fc71712b5a43f9d13dce9"],["/posts/88294277.html","9679ecdc43ea0fa3e8a96528b1651294"],["/posts/939963535.html","f07c22cbb53a8e0212d498cae28d6629"],["/posts/983786067.html","6555c70386b8c79ffce4ec288f56fc58"],["/sw-register.js","db9e0d748ab6299a83c78d817e717410"],["/tags/C/index.html","381727003aa131866569beafaff3f4bc"],["/tags/C/page/2/index.html","1439e527b1f62010a65b1427a4972550"],["/tags/C/page/3/index.html","e0ec13ac2b1cca1e51ba9ced39a1de43"],["/tags/C/page/4/index.html","34314060a553d6e097a7e48217118a22"],["/tags/ETL/index.html","5456585d1f1e8f1c5ba902a315a15c07"],["/tags/ElasticSearch/index.html","17bab36de449da58f24a7ad903dee467"],["/tags/GUI/index.html","88e47c39dbedee25caaae7a70cda904e"],["/tags/HBase/index.html","66f4005a57652124f4aa9eeb6df9589c"],["/tags/Hadoop/index.html","71d8192d624d50d2956557ed76f52066"],["/tags/Hadoop/page/2/index.html","6a08bfe9e483e8d5bf10b5242a51b530"],["/tags/Java/index.html","ab87fbe7e554fb207822ff1fd18d24bd"],["/tags/Java后端/index.html","ebed129bf8e5f5a075a7b6f1661eb36b"],["/tags/Java后端/page/2/index.html","31b1013b1b157d08311804f4ca8848d1"],["/tags/Java基础/index.html","7740dee4b0d29aa0b57f70533224b462"],["/tags/Java基础/page/2/index.html","eb930b6125717024227806d665fb8981"],["/tags/Kettle/index.html","53d63016134f9884ec928caf42abf313"],["/tags/Kibana/index.html","c3a14e6242b4f162a8a847374fb314b3"],["/tags/Linux/index.html","993386965be168673310b0aa5fe0d9fd"],["/tags/Linux/page/2/index.html","70deddb29d108d3693b4526bd088f2d6"],["/tags/Linux/page/3/index.html","19d398d461950ec9f4b6aa548db1b40e"],["/tags/Mac/index.html","d706754774561f79490d2fa27a8475b1"],["/tags/Mac/page/2/index.html","3f207fe84cc55e634b55ee5638520d00"],["/tags/Maven/index.html","a57201e36e443de986749468e3e86cbc"],["/tags/MySQL/index.html","1f0bb8439c21674a9c944e2420fcd153"],["/tags/Python/index.html","c22946d545c982f81fd8ae0820de0a40"],["/tags/Redis/index.html","e019db28ea44c88a99deefaef141a0d0"],["/tags/R语言/index.html","b27b38812fd15d893a2875237d446a41"],["/tags/Spark/index.html","5f682cbd1c33970d501737e38123ffaf"],["/tags/Ubuntu/index.html","f38940f4b68739d406cebad02a829073"],["/tags/Vue/index.html","b7737ba9f6d962ee2ec399e3cf28cc98"],["/tags/Windows/index.html","8a7ddaa5d04510074625015a4bf05c76"],["/tags/ZooKeeper/index.html","46098e73793f288e30c77b2460405793"],["/tags/bfs/index.html","aa29fefcae3c7bec8d26dd96df744fdf"],["/tags/dfs/index.html","695fb79eb5153f0ebdcdb30b580d5b24"],["/tags/folium/index.html","852286fe803974036d9c76009e6eff8c"],["/tags/git/index.html","f043ddc7d3980e87c799d29620f3d419"],["/tags/index.html","d8329b2d944c18a15778362d92be6e45"],["/tags/latex/index.html","bfa50b77cacb82ae16ca5b07dab3228a"],["/tags/中间件/index.html","c97fada5971c9b784ddaa686fcff5a9e"],["/tags/二分查找/index.html","1a2c83c5b7c8db0e8c6e3b72aa77117a"],["/tags/优化类/index.html","3a47c4238868062449aaf17d35dcf8e0"],["/tags/前端/index.html","f18f04219161b06295ae1ea8b9b6582d"],["/tags/前缀和与差分/index.html","0191b88461a9143fcc4b045656e12c85"],["/tags/动态规划/index.html","f48f8ed2d58c12dd981475a3616b529d"],["/tags/动态规划/page/2/index.html","a3306a04d4ab6058f3be12b4fa240cec"],["/tags/博客搭建/index.html","abff204c1062723257e9a91816ff929d"],["/tags/图论/index.html","63c22c74fe5578599d32d3efdb7c22e3"],["/tags/大数据/index.html","b0e308bbbd980d9f7a835e74d267bef8"],["/tags/大数据/page/2/index.html","fe71b4df7413f49305804ed7adc1e9a2"],["/tags/操作系统/index.html","8da3fb3aa4f9c46afd96cd69c4f42445"],["/tags/数学建模/index.html","a624a0ca88a42a3edb33d4582bb72346"],["/tags/数据库/index.html","045486b84ec6751299dea91ac36e48d4"],["/tags/数据结构和算法/index.html","59b6022c707275001c487219fb1c2cf0"],["/tags/数据结构和算法/page/2/index.html","e47e039f09007b70d5ea48ab12cfc4cb"],["/tags/数据结构和算法/page/3/index.html","274cfbba164165168bed5196a0fd8b64"],["/tags/数据结构和算法/page/4/index.html","d27929f8577fc00a3c6cb7f9a71ca79c"],["/tags/数组和字符串/index.html","07da2787b5ec4c9eb20c58840939b570"],["/tags/数论/index.html","7b8545b787c94f1252a8e8846967dce1"],["/tags/枚举类/index.html","9efbfc3219f0406eb05679a07ff9112e"],["/tags/栈和队列/index.html","b189afca28687683c2d76af971a99321"],["/tags/树论/index.html","45632b3031c98138adaa30f1c3d48dd7"],["/tags/测试/index.html","2139f88ab91475221bec4f5e082f9415"],["/tags/环境/index.html","d5662fc454ab23c82ec37b2b907cccda"],["/tags/环境变量/index.html","e6cb35e6b57454cbf4c2b5f89fa16ffc"],["/tags/绘图/index.html","a34e5077e04619a230a904ff124eb8c4"],["/tags/编程工具/index.html","ad102c7406e108398321c9585b111c51"],["/tags/编程环境/index.html","32fa8ac63cdf338e63e8922e470b15dd"],["/tags/网络编程/index.html","4fa42a63c4b31188d441b9b5e41d8323"],["/tags/英语语法/index.html","671a623bd213c4adcb110472c46700ec"],["/tags/计算机操作系统/index.html","c5614e3d494ca48c710bfb02d06005ee"],["/tags/论文/index.html","714758afaae1c48a435f7f3e522d144b"],["/tags/资源下载/index.html","3c0b05197f1f56b808d831ac4a43a457"],["/tags/链表/index.html","20dd1f8094010c7c390d8e4ddec8ea32"],["/tags/集合/index.html","4cdc8c0635004ef0a943eab89d4c234e"],["/tags/集群/index.html","52f1c8b8e1bf9abc4d1c35f7e7b4a052"]];
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
