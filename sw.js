/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","852ec369aa0e08458d1602061c72043f"],["/about/index.html","d8714f9ed92fb912665d5830661a1022"],["/archives/2023/01/index.html","541c66408de5d303a54c0dbf59062d28"],["/archives/2023/02/index.html","776f36715df55313eaee9f9062b6236c"],["/archives/2023/02/page/2/index.html","5fd7437bab0c7ad9ab88d208fb4c82ea"],["/archives/2023/03/index.html","cdd1571ac8831e50b88c264cc7c7ba89"],["/archives/2023/05/index.html","23ebdaff5e5ddabb040c7794f14a4416"],["/archives/2023/06/index.html","58f93969a68f9409bf76c25d59dfceb5"],["/archives/2023/09/index.html","cccb7a70f1eb50b95b4f1536cdad52c7"],["/archives/2023/index.html","32846bf7aa0476750dbd6bfd3f5ec32c"],["/archives/2023/page/2/index.html","6e3bf76ec1e940a7dcbe3d28f607cd8c"],["/archives/2023/page/3/index.html","fce5852758d780a628a97be8bd011e8b"],["/archives/2023/page/4/index.html","fe6f670b7ae4987ee5baa67bfb6054a4"],["/archives/index.html","8d5fbca091de93cbaadb0fc18eee8e35"],["/archives/page/2/index.html","3f43b633d88f6f3918b168007a3151fb"],["/archives/page/3/index.html","855f66ee4891838eb8d3e5bef2859d84"],["/archives/page/4/index.html","fa05a9992777c8bc7cb4ac295cb40f38"],["/categories/Java/index.html","e4905cf790d489499d037cf5819ceeb9"],["/categories/Java/后端/index.html","be3dc92b0ed14ea2e2288285b805ab8c"],["/categories/Java/基础/index.html","f57dac234e51846d6acbc18f22a2ea9b"],["/categories/Java/基础/集合/index.html","f7f661f8c525d4c9941d951c9eb1f7cf"],["/categories/Python/index.html","6e0a30fdf287798cfee2a4582db3f899"],["/categories/Python/编程环境/index.html","1d098d0bbe95e002e2c59f0c90378fcb"],["/categories/R语言/index.html","6e4d939edc6b88667eb912e51d907310"],["/categories/R语言/编程环境/index.html","21e6ccdc6434e740cfab9b59b0a854ed"],["/categories/index.html","a64956e5d265f5d3a31782367a410115"],["/categories/中间件/index.html","138ec30e017fd3dd3e7214d1301bb234"],["/categories/前端/Vue/index.html","94e0bd206b6dcdda8ef3b3453afbe035"],["/categories/前端/index.html","e387f4dc981d14b1a2f3eba83cd15fd0"],["/categories/大数据开发/ElasticSearch/index.html","3436f37773f29e92cfc08791daf4c9e0"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","e40498f05e6c632c1065735db8e79260"],["/categories/大数据开发/HBase/index.html","ca8742e4c7a811c1e4cf0a49e88cd75f"],["/categories/大数据开发/HBase/学习笔记/index.html","11d9d0b8483481355870c901ed0e41e9"],["/categories/大数据开发/HBase/环境搭建/index.html","785c44352443e67ff331e57e552ad856"],["/categories/大数据开发/Hadoop/index.html","ba84d91043b90e322ccfc3585d4a0be2"],["/categories/大数据开发/Hadoop/技术/index.html","e37276b0c89b8d835fb096243d370e05"],["/categories/大数据开发/Hadoop/环境搭建/index.html","fe4c924e4b98ec7a40388dafc2ca6d37"],["/categories/大数据开发/Redis/index.html","7b3b49f7d921ad34cf8b941b8fe54c96"],["/categories/大数据开发/Redis/技术/index.html","7e409aad8e7c3479715fd923ae6d1ab5"],["/categories/大数据开发/Redis/环境搭建/index.html","329a8a1ff2a4a363236e3783fd7b9fae"],["/categories/大数据开发/Spark/index.html","8b40c949a128d4056f825e8a94e0e3e1"],["/categories/大数据开发/Spark/环境搭建/index.html","3ad6ea32a53d19740b94c7f8d096f44e"],["/categories/大数据开发/Zookeeper/index.html","7166d01b2d2616eb139dcf30a4a843a4"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","0e1f108f3af269714ed326850ec1e028"],["/categories/大数据开发/index.html","15dd33fc45729a63ff09d36165c67171"],["/categories/操作系统/Linux/index.html","cfc6a8ad5b6620986c20e037087454a6"],["/categories/操作系统/Mac/index.html","06ad4632f8be98e6ec796d5dff8e6a18"],["/categories/操作系统/Windows/index.html","a12ce0c1961a7467ae4d5d1f475fa079"],["/categories/操作系统/index.html","cd797e1d6a9ca67bcc999abb90c8aa8d"],["/categories/数学建模/index.html","b964ea55de77a3d1524245cd16a93ff8"],["/categories/数学建模/latex/index.html","b392584fb798ca5d83a3752e543ceeed"],["/categories/数学建模/优化类/index.html","f6fc49ae6278de6b49117399265716da"],["/categories/数学建模/优化类/现代优化算法/index.html","b133eee62c77795ff3babdc65b686e4e"],["/categories/数学建模/优化类/规划类/index.html","2bc612818c513f4e9b5e0b72e452cb6e"],["/categories/数学建模/绘图/index.html","6353661b1ee37e183e2f6533b8d9e22e"],["/categories/数据库/MySQL/index.html","3f7a438cf0c6717d0733fbfd4e22e834"],["/categories/数据库/index.html","fa944ce667f216bece8f67037da2f686"],["/categories/数据结构和算法/index.html","ac2f03d56fcdf1ffa9528d540c44c45d"],["/categories/数据结构和算法/page/2/index.html","daabdaddb4ae7f98be2131dcf6143404"],["/categories/数据结构和算法/基本原理/bfs/index.html","4de872b734d1a9e2ec703493ae765429"],["/categories/数据结构和算法/基本原理/dfs/index.html","7cb34ab4b386c3b828202cd530356942"],["/categories/数据结构和算法/基本原理/index.html","8719e009a14de2b5bb3b6421e203029e"],["/categories/数据结构和算法/基本原理/动态规划/index.html","74dc9e3ab7bffe0b9d5e68b8d5354140"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0dd8d1951fa01e780f488ac52e1b361a"],["/categories/数据结构和算法/基本原理/图论/index.html","608ae4a0b4525b0080f699070358117c"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6b755cca89742090ef27ba1f113b91bb"],["/categories/数据结构和算法/基本原理/数论/index.html","956146a302f4b5d678aba3b21673e4b0"],["/categories/数据结构和算法/基本原理/树论/index.html","9fc10338634cf80717f78605af6b9607"],["/categories/数据结构和算法/基本原理/链表/index.html","44d3c85bf156ff123072bef834bc80b5"],["/categories/数据结构和算法/算法题/index.html","e2190aa0965dd320063c771da19c5bde"],["/categories/数据结构和算法/算法题/二分查找/index.html","52e240e5078f8c739f0304041adb2dcf"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","24aaddc9b20aa4f6185ef9945802fe0d"],["/categories/数据结构和算法/算法题/动态规划/index.html","34423f49a055441da0028b698c8043cb"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","524e59244746a96eda601b2dbcb3eb0f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","0469a10b4c109dd7e64278ed6a1a38cc"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5f9df1cb0f28ab32cbb835e3cf247023"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","420b0893f2ef8cc920ab32c9c96dedcc"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5c85c792e1a7409b3045736dd12a3d88"],["/categories/数据结构和算法/算法题/树论/index.html","3fcd4ea2fbac6f93d0f9ac36b3d1a6ba"],["/categories/杂七杂八/index.html","817b0e5ea0a074d6aa80e5e32e843e62"],["/categories/杂七杂八/博客搭建/index.html","2781b148939bf5e720d6566acd953eea"],["/categories/编程工具下载/index.html","4b08a52dad64366161e38c76109e8a40"],["/categories/编程环境/index.html","a1b595c2240aaf23cdfcf6cd9d2c3fa0"],["/categories/编程环境/大数据/index.html","540564ba30e0277291316bda4269d6f5"],["/categories/英语学习/index.html","7a5f262cc193d3ee3fc601d29cebfe58"],["/categories/英语学习/英语语法/index.html","2c41c50bdee48fcc901c1d0cbbf77f5a"],["/comments/index.html","563958c92570192645d5f3a50f406d56"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b2a9358a8de8a66dc6b8af815852281c"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4f579420ce4d13b272e53b8efc0b5bda"],["/movies/index.html","391e9d172be43a596a9fa941a27a0bcf"],["/music/index.html","59c90126d76e1c00a0ff220d170a6327"],["/page/2/index.html","a68635b1e650615d7d1013406a1f5986"],["/page/3/index.html","db52a32a8626414fc52931d59abe4618"],["/page/4/index.html","c37b62e936f25f7e95a66caa10047d9b"],["/page/5/index.html","7324e8166a7201b8a01c82879c5c7d4d"],["/page/6/index.html","a7cd46bc6fe029c011545552d5d03973"],["/posts/1021360842.html","30ee04aac7affde379ec0db34f8f9fbc"],["/posts/1120620192.html","61e89fbadfa379d6b3d497ba29485fe9"],["/posts/1141628095.html","c7803bd221d65a3959804d745d6f1df8"],["/posts/1168613674.html","5f86b7853c5e1e2ec4a54f3097fae023"],["/posts/1219920510.html","0760c504446b5fc3fd8ad9ad2f402748"],["/posts/1222166338.html","37053219c96c9acd0d34aa983ba1950d"],["/posts/1259097482.html","a03ee82bc33297291dd75a47fae07077"],["/posts/1271036369.html","9c4a8c28a87239c8b0d22e51383563d0"],["/posts/1312847445.html","bbf9c11a2f8671bd776cb65ac9f88004"],["/posts/135355774.html","8677af8d10426905048f151b594260b2"],["/posts/1375344716.html","a3d1342880eff1438136f95261509995"],["/posts/1388991698.html","bf95fc5acfff0a7963fd6cb007082717"],["/posts/1410315814.html","c6d32c65e9d39b7c1b44c785980df813"],["/posts/1452790229.html","00945e6e15ce15e5bc01f53186b1c9c9"],["/posts/1470079884.html","7d7b9f45fd311e403fb109403cf59a2a"],["/posts/1470079885.html","7749112f05249032b6903a13c38e4e6d"],["/posts/1470079886.html","3336ef6d88c7944157c5b320c4d070a2"],["/posts/1470079887.html","050c9b4aa7a68013f5f444eb9e550b30"],["/posts/1498536549.html","9588fb7634b1e24a4d75e537785b3aba"],["/posts/1547067935.html","c08baf4a22b7f7cc57b8f21c1aec1ebd"],["/posts/1557866301.html","d76fcb6501383cb125c2d3d0060a4e69"],["/posts/1571776361.html","01ee2ef319bc1de342c43c8dd3e4156d"],["/posts/1605124548.html","4fd2c539e7be9ac37fae6d48f2ac9134"],["/posts/1633036852.html","5eb02fabbdddaff23a933381abeb19d7"],["/posts/1674202625.html","a76d9fa3254aac20c088680aa745e065"],["/posts/1765123828.html","bc637c9cbfa941a5db3074872c7cc350"],["/posts/1767336200.html","0c525af5714cb52e95116a2274fe7454"],["/posts/1776114197.html","55882808faa9d826547b78bfff8fa4f6"],["/posts/1817748743.html","90bb0df45b3d5f449394acf38bad4807"],["/posts/1925125395.html","e3a387344c5fcec4ea98bcc521323dba"],["/posts/1966191251.html","6f859c4a28cb1fd9c5cfbc02a70bc0a7"],["/posts/1987617322.html","84110b15a2c8287b09636210c82eb804"],["/posts/1999788039.html","e6a3650af66c24e0d1555de3fcf08c0d"],["/posts/2075104059.html","c20beb3e0b4c74ac6ac55f3323c415c3"],["/posts/2087796737.html","b84672d48f0fc4d6cc08f14a956afb88"],["/posts/2106547339.html","0bf00108da4ad2154e9d7eb8aa76b0e8"],["/posts/2207806286.html","70eb99348a184f8a6e40f86408c694cb"],["/posts/2225903441.html","3f9f7ab7eb3b2e584179aabfd6e8a057"],["/posts/2265610284.html","cabeee58436406bd0fa5e25b1bff9276"],["/posts/2281352001.html","2c70055462c3fd54fbc25f863e38fbd0"],["/posts/2364755265.html","5f7c3b284f30c69dc6293ba411e06ca3"],["/posts/2414116852.html","f73039169d3c009fba766d52ac7264e7"],["/posts/2421785022.html","dcfeb141d3f7abaebc388021484c9d40"],["/posts/2482902029.html","023ca7af9397670bb986e600e226e9cf"],["/posts/2495386210.html","6f1f2ce17c5bf933f99c62507afed81d"],["/posts/2516528882.html","8ef221b53d6b866ec65aea6d8674fcc1"],["/posts/2526659543.html","b2a97a1dd7d78ff6a92c9aac99bb3d20"],["/posts/2529807823.html","5f3ca6f7fc1b45e22a91be2ada6e9d9f"],["/posts/2596601004.html","62442ba39dc27b68c9dfe7650a2b98e4"],["/posts/2742438348.html","d46ecb755286f2e405f7f7ec0a69129e"],["/posts/2888309600.html","1972167be3062a4138eaa78bd2d002aa"],["/posts/2891591958.html","20650c49dbe5dff6df14bb3b1ee4381f"],["/posts/2909934084.html","d73487d898889859a876498042e88259"],["/posts/2920256992.html","f056fe8c12cfa349a44116b1a907c8e4"],["/posts/3005926051.html","cdcb227dcfc294337dfbbea2df079be2"],["/posts/309775400.html","f149de1f6f6ed406564ee4288f75a269"],["/posts/3156194925.html","ded8aa2c88d5b1ac7985c68f19e24432"],["/posts/3169224211.html","6fcd113163a958c0c265d5aa67fa1623"],["/posts/3213899550.html","f9b172ba8d02e2606ca296d261c2a78c"],["/posts/3259212833.html","6cafce330d44e277038a73b5a3fa87f9"],["/posts/3266130344.html","e5530459c661c7cd04742644e51da672"],["/posts/3292663995.html","94408e814d86dc036305e85aa97498a6"],["/posts/3297135020.html","a5d95b239ac27add8299f2ea8b46c37d"],["/posts/3306641566.html","07ec1ef40fd79bc3cfa9740544b8e475"],["/posts/3312011324.html","6f427cf2fd4a61b997d339a2df96c805"],["/posts/336911618.html","510272eb0d0594686ddc73e6e27cb284"],["/posts/3402121571.html","cdbddfd21a3500bd10211e05fd2abc0a"],["/posts/3405577485.html","b6b64f43a9f00f5aa5683f8f86de8966"],["/posts/3498516849.html","af9e802cb72f274c980b88c113bd7600"],["/posts/3513711414.html","94bfe955d480312b69c867136ef3518f"],["/posts/3546711884.html","b0b117db53fee48d10a9f1b23b318a2e"],["/posts/3731385230.html","4c15358992c8dc51d32ce5ece7d8b2d4"],["/posts/3772089482.html","197395e92d1c07fdd3e69206d227b7a5"],["/posts/386609427.html","d0bca5b5d5d0c5e257ec0855aff00b58"],["/posts/4044235327.html","9c2d8b3c08aa4f421cc92fe098ae145f"],["/posts/4115971639.html","26f2866c55cff1b02db103b60f404f49"],["/posts/4130790367.html","80667b1d7f649ccc27dcaa408b1e5206"],["/posts/4131986683.html","5732c3434a1fe9c23f09c3c4efd1e555"],["/posts/4177218757.html","56021225f9dbf602fc1feee1ef8056d0"],["/posts/4192183953.html","c525f88dfc4391e93de519cbe4bb807a"],["/posts/4261103898.html","999355890545c181831d2802ac539c1b"],["/posts/469711973.html","2a8e4fc502345bbdbe357063f2336774"],["/posts/482495853.html","2c8712227341d9f37eb7142c1982f272"],["/posts/488247922.html","b7fec92e17d3be8adeff73a96d2daf46"],["/posts/517302816.html","8db68330e51aa011faa400fd68138e5f"],["/posts/570165348.html","49b497447bc8f0bfca881d4059729c45"],["/posts/595890772.html","a7f20109739574ad6a8327411d5f6855"],["/posts/67485572.html","425042efdd5242111748bce9da6caecf"],["/posts/694347442.html","888df080f940dc59316c2c5e6d7c22a2"],["/posts/707384687.html","4c8ffdfa6da34fa5e7deb2279197a27b"],["/posts/71180092.html","7bc090a3d7f18ed2c63ffc9995a5680e"],["/posts/716459272.html","6866f33bbf12f31c1cc19b707afabac6"],["/posts/778231993.html","f5ef7a7a6bccad9c140a28523f97f7a6"],["/posts/795397410.html","dfd50fd2ebd14d8c2562ee5db00fb28b"],["/posts/820223701.html","dc071c40965828ed51573ffe221cffae"],["/posts/830372185.html","1b171f20ef70548bb0cd3aa2f1218b2c"],["/posts/88294277.html","12405f5af1576559a0d1e8653def8164"],["/posts/939963535.html","00e277d9fd2a0981a6c45b8e95caaaf5"],["/posts/983786067.html","0c33db1a62031da516b5a9817688370e"],["/sw-register.js","70d1079a8454742328e71e0fbfe41e21"],["/tags/C/index.html","93124e48bdc69234915f4595be038816"],["/tags/C/page/2/index.html","21981887c5a41d81980781cb8b4bc35f"],["/tags/C/page/3/index.html","3339fb0fbbf745efc1ddaa513a66397e"],["/tags/ETL/index.html","7ae49ecb4652610924379fd41a4a8d87"],["/tags/ElasticSearch/index.html","77cb333c50652860a249aff5d3fdf049"],["/tags/GUI/index.html","e7fb3d1d01a1264bf1fd10b8b25b382e"],["/tags/HBase/index.html","4bd55e30bd159c4a69f6ff55cf5058d9"],["/tags/Hadoop/index.html","bdc122fa2da4ac197377060a99c8904a"],["/tags/Hadoop/page/2/index.html","6dde501d4cccffe965f5685878a2d0cd"],["/tags/Java/index.html","9ad2727c0724c3e215d42be409d6817d"],["/tags/Java后端/index.html","647ad4372707cde3c3b64cef77dc424a"],["/tags/Java后端/page/2/index.html","339dfa62c539ccfc095135fffd9c3b7e"],["/tags/Java基础/index.html","8a7b478c17d32bcd9dd93f513a2268a1"],["/tags/Java基础/page/2/index.html","467f1d9a19a408e1549e09967bcfc10d"],["/tags/Kettle/index.html","2a725c9594622e84d60ffe36a19cd531"],["/tags/Kibana/index.html","ca156ee474ccb6eb32285d85dfe48758"],["/tags/Linux/index.html","9aa99ceb3b37b896fa8ee06fd39dbf8c"],["/tags/Linux/page/2/index.html","f39adda4dcce596fd543272beb588533"],["/tags/Linux/page/3/index.html","b3c8ec7824b5877704ddeeb662e62d0c"],["/tags/Mac/index.html","d0004cca5d0a0963f87aa667fa8e987b"],["/tags/Mac/page/2/index.html","912cd3856a74d4c3fdc094278c8bd989"],["/tags/Maven/index.html","a7b8c9232d6275a0c894d5e1d06ce947"],["/tags/MySQL/index.html","c1c66b04c9895fc6407efea7fcb8aead"],["/tags/Python/index.html","6a26ecaa5ae11b8613f84f0141789273"],["/tags/Redis/index.html","cc00e4576e5b208809a77ad1abba253d"],["/tags/R语言/index.html","7e52477954a9470a61a057fc6551d4b1"],["/tags/Spark/index.html","460c817c74b1cd32d0c36432abe0aed1"],["/tags/Ubuntu/index.html","1df0c41f852e1241c98ef82554019d0a"],["/tags/Vue/index.html","0dca5531aff1ef480ca68e3d5fe385e7"],["/tags/Windows/index.html","5129676da904878a18b64dd0df18077a"],["/tags/ZooKeeper/index.html","01c0e9007191f6067d5059366d7690f0"],["/tags/bfs/index.html","840f9738de316f3de3b82674eaf866c7"],["/tags/dfs/index.html","6de59092feafb455d6a171b2d7263a2e"],["/tags/folium/index.html","d82f953ae058abff52fa95e95d9bbf82"],["/tags/git/index.html","dc19fb204d2e00baac9fd52890000b69"],["/tags/index.html","5e6712ea05cdd8df92a82b7682a0d071"],["/tags/latex/index.html","97f104fa4b2385588670b61559e5cac9"],["/tags/中间件/index.html","024a7fce919332925cd44e2ac3393663"],["/tags/二分查找/index.html","bd60114196b38e0022270ec3b159c23c"],["/tags/优化类/index.html","bca621528e736cd8dbee476b18be2995"],["/tags/前端/index.html","5c367ecd6c57d7d2fc34a24a8f107519"],["/tags/前缀和与差分/index.html","af6ca16560e7bcce52f3b9436c4e1ca1"],["/tags/动态规划/index.html","835b019e07cda8377a26ebdbe6d15f64"],["/tags/动态规划/page/2/index.html","8d45bad110e3255f5f8134789cbc4d08"],["/tags/博客搭建/index.html","67a18fa570908d6bb6a25a38a98d425a"],["/tags/图论/index.html","7c2cb9647a52f262aa4294b308138c8e"],["/tags/大数据/index.html","09aecc9c5cb91838cfa8af22c2d62dfc"],["/tags/大数据/page/2/index.html","6dca317b715851ca6db5bdb6a1f1e872"],["/tags/操作系统/index.html","ec146dae6e200252e2be841e5b41fc6a"],["/tags/数学建模/index.html","5f776ba15fc89ceac0e3b545623991c3"],["/tags/数据库/index.html","b023ce530c44eb12da7c6342ef7bddc6"],["/tags/数据结构和算法/index.html","e57584c76e0cecb076fb9d4903b8223a"],["/tags/数据结构和算法/page/2/index.html","c6de1da638bd42cf25ff55e0fda6950b"],["/tags/数据结构和算法/page/3/index.html","98080989ada9deacc8e8f21600486f8e"],["/tags/数组和字符串/index.html","0381ed1cfeb85037442c28101c8b624f"],["/tags/枚举类/index.html","5c42cf6bd6d0f7d63d9ce474099d2178"],["/tags/栈和队列/index.html","46a563db8e1b0e5be7eb213d5599ecab"],["/tags/树论/index.html","4df4c4ec9f2dfd93e26deaabaf43a2b8"],["/tags/测试/index.html","ceec37a5255a3715944cb85cb3e136a8"],["/tags/环境/index.html","919650d89d4e28272832160b2831780e"],["/tags/环境变量/index.html","0b9cd06244f688b8cbbbe5834d77b5e1"],["/tags/绘图/index.html","d77bd1fc54e7b324febc598102ddde5c"],["/tags/编程工具/index.html","7f9123142c33f044658cb6713bc88bf2"],["/tags/编程环境/index.html","31e109df1a136cb691c74e091c29ae39"],["/tags/网络编程/index.html","4f6b4b1ea46180e5d58e3de71fc08a89"],["/tags/英语语法/index.html","fd1f745e350aef3ed3796ab47c96cffd"],["/tags/论文/index.html","fcfa1d80c815d2a0e8cbcb86959a0e2f"],["/tags/资源下载/index.html","49fe1637ad68248ce5ee665e0b28f0c6"],["/tags/链表/index.html","c3cf61f5cd4a46e2f2409176067b1391"],["/tags/集合/index.html","62b398fb0468c1afe23521d649851674"],["/tags/集群/index.html","31e29f107c1ce55ad9e4486e42448228"]];
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
