/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","722f331d8e8a09fc1ea68e5be3f8ec25"],["/about/index.html","d5ca8a4bd35ec2f43cc9a6db95f3c0d8"],["/archives/2023/01/index.html","9d7905eb68ca6b69fb82217dc05709bd"],["/archives/2023/02/index.html","cee6f9772a7adb32b03be150bfa7ad22"],["/archives/2023/02/page/2/index.html","95fd58fb45d1240e26be89840be7efd4"],["/archives/2023/03/index.html","65060fc25a04a2f118a2fe0ecfcb5b6b"],["/archives/2023/05/index.html","d388058ccc6615be90d4a43db331205a"],["/archives/2023/06/index.html","40531b928ff8c5709521053e3c1327e2"],["/archives/2023/09/index.html","773d5d6804b0e1d54c146f00386d89d7"],["/archives/2023/11/index.html","00ebd6c6cd4c9af8c5bbdd734f768d6e"],["/archives/2023/12/index.html","69118cf13aa5ffe55a1daf50ffb850c1"],["/archives/2023/index.html","7bc010db44f6bc281ec1b1177c2fdc23"],["/archives/2023/page/2/index.html","176087c27fe7055e23e11fe5ac32755e"],["/archives/2023/page/3/index.html","ee8a755ab8d8fc5e5c94276f4b823ee0"],["/archives/2023/page/4/index.html","2b26f026a82126cafc03c0fd1d7fa59a"],["/archives/2024/02/index.html","6d91f2eb74448a47f6eb3d12ccc80938"],["/archives/2024/index.html","1d91a2b15e868c9ba2dd289fb764b8ed"],["/archives/index.html","d1e0fca926dc9ce355e302bfb4daaa38"],["/archives/page/2/index.html","08845f46f841801565ebcce3f7bf17f6"],["/archives/page/3/index.html","42a86aa058ac25f26f8855ac49fde545"],["/archives/page/4/index.html","bd5af60296d3c4af6c5261ece13d3ed4"],["/baidu_verify_codeva-qQP2iZOMLX.html","f63cf0b7e1e04cec9e58b09d7354be27"],["/categories/Java/index.html","9f07e20e0647188a197349e7f1f89176"],["/categories/Java/后端/index.html","ff88a2390db846d111efcf66815ae044"],["/categories/Java/基础/index.html","bbf366b5ae2feb46f0c112f35e2cba66"],["/categories/Java/基础/集合/index.html","8493d23051966f4a69222ced350eb379"],["/categories/Python/index.html","e22343093db0f8a5dc6eeff8c86f0bcb"],["/categories/Python/编程环境/index.html","9e4845632dcaaa47fbbedfce791386c4"],["/categories/R语言/index.html","8dc3069afe38fe8eae0230b33aecfb6a"],["/categories/R语言/编程环境/index.html","6e2f899c00502448edbfbab4b12de023"],["/categories/index.html","03f544261009eeb5ec251583840f9e34"],["/categories/中间件/index.html","fa539e7d359c694c95624a4cd1f60778"],["/categories/前端/Vue/index.html","2991b88ad8d3dfb4a564fcb741dd0543"],["/categories/前端/index.html","af54f01a306f553f952d382a9cfa48bc"],["/categories/大数据开发/ElasticSearch/index.html","318f54deef26b8b74a63ceb663d8a2f7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","f75622c5ee338732df2c4db53a850d78"],["/categories/大数据开发/HBase/index.html","479e03d7cc47efcc3005b40276b5fd1f"],["/categories/大数据开发/HBase/学习笔记/index.html","372d87a5bcb4ce6eecbceea359060d74"],["/categories/大数据开发/HBase/环境搭建/index.html","7ff54590be7cc6650d8335957455b6da"],["/categories/大数据开发/Hadoop/index.html","ce631a98af200f98a77c8b910f6c29ce"],["/categories/大数据开发/Hadoop/技术/index.html","9fb5363779ea90350b22128b03d79c7b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","eec483aa051833bfea819a7140ffa1fe"],["/categories/大数据开发/Redis/index.html","f133e298a8450491ddb6303077a877f6"],["/categories/大数据开发/Redis/技术/index.html","fda0c615f30632847bb66e437b325c54"],["/categories/大数据开发/Redis/环境搭建/index.html","4159ab00c66f2864bd75f27971d0ecba"],["/categories/大数据开发/Spark/index.html","d843dd7f3992c88e3ef49ff14fe10077"],["/categories/大数据开发/Spark/环境搭建/index.html","ba04f0e4fa5036543e35484d9567d305"],["/categories/大数据开发/Zookeeper/index.html","fa4fdbff4219b980c1f4310b73a60306"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","a29b8c3a65ede6f28acbb8abc6b5c7ad"],["/categories/大数据开发/index.html","5050e254b3fd580adc6d2122f6b03d92"],["/categories/学校课程/index.html","19d01eaac51e11c40f917bd4a317b721"],["/categories/学校课程/计算机操作系统/index.html","beac340d05dcdc42ad62563337d13e28"],["/categories/操作系统/Linux/index.html","62c4cc2b7c1b060aa3f47766488c0644"],["/categories/操作系统/Mac/index.html","40ee6845c7ac4b71109cf98a312a215d"],["/categories/操作系统/Windows/index.html","ac60894a59023a04a0f527ac3ffce808"],["/categories/操作系统/index.html","be26b13a5ca9572363d2f285290d48fe"],["/categories/数学建模/index.html","6ac7121c214d765926fa2224370bf663"],["/categories/数学建模/latex/index.html","c74e13c971e2c571620c45550232ad2b"],["/categories/数学建模/优化类/index.html","06fe2c6f0a677117eef3668921c067ab"],["/categories/数学建模/优化类/现代优化算法/index.html","45334dbb2c54e0a9306ce92e88f57aec"],["/categories/数学建模/优化类/规划类/index.html","883549d739e00300c2dbefafc64e7880"],["/categories/数学建模/绘图/index.html","8855f32ec15a1faf20f73e1a9194793c"],["/categories/数据库/MySQL/index.html","a934782c0280cc609af564de5b8ae80f"],["/categories/数据库/index.html","021e3ba57492313cdf3e9fc0cd190ff9"],["/categories/数据结构和算法/index.html","fb74392d61132c82d6bcfed3c71f9dd1"],["/categories/数据结构和算法/page/2/index.html","8e38268c0fa5af5a2a54af72dd42371a"],["/categories/数据结构和算法/基本原理/bfs/index.html","1f7f254c7cb89c3bc65d31f263625bc9"],["/categories/数据结构和算法/基本原理/dfs/index.html","054a2d2da7353ba08f61439a8b227e33"],["/categories/数据结构和算法/基本原理/index.html","afc783e9eb4143906938b1be049cd492"],["/categories/数据结构和算法/基本原理/动态规划/index.html","fe0984d38d0b25f85b2a933fc287e417"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ae013042804d71b3b785cf149e01a3b1"],["/categories/数据结构和算法/基本原理/图论/index.html","cfaab7054eba39e3107062f91eacdfc3"],["/categories/数据结构和算法/基本原理/字符串/index.html","509ba2b34b06b38afc85d134aec75073"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","5da506a3ceb7ca904b5d4f2ba6ff9a17"],["/categories/数据结构和算法/基本原理/数论/index.html","b8fbda136181d1a4639004d20ec5dd1c"],["/categories/数据结构和算法/基本原理/树论/index.html","9cf3f9a43952e94d9f84e126607b5765"],["/categories/数据结构和算法/基本原理/链表/index.html","4d0f11c04fc0f577417249419ec4ed4f"],["/categories/数据结构和算法/算法题/index.html","d113a3c5e82d7b96203dd606c42e1fac"],["/categories/数据结构和算法/算法题/二分查找/index.html","cba6d2a442326aeffee20780cb2df272"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0c37d989e68f387b54ffee14014811b8"],["/categories/数据结构和算法/算法题/动态规划/index.html","70b22544de0eb3aec84e29342efe938c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","641e9870ea67f9bd7e0bfded50fdff5c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","af4b3a74820872e48d592687d1f6ae27"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","2b2cde592ecd750f19a0d83f74a56f1f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4b21249028aa3bd6b5d49e6ab8fde29c"],["/categories/数据结构和算法/算法题/数论/index.html","34c12d7d5d3d32e1f5cae414195331ba"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f6802afcce816051f885bb669ac0437c"],["/categories/数据结构和算法/算法题/树论/index.html","3db84d6ebabb8d1082ec7329623ad4be"],["/categories/杂七杂八/index.html","a74d8b743810e7b391843917e83762a8"],["/categories/杂七杂八/博客搭建/index.html","1358137a2dad7cb12c304c2b527a74cf"],["/categories/编程工具下载/index.html","1910b71a417405a0b22b6aac6a0fa1c4"],["/categories/编程环境/index.html","86ab622a16ed822b43644e82d6b0c2cd"],["/categories/编程环境/大数据/index.html","1a4e523345742019d4aac5323a7c4579"],["/categories/英语学习/index.html","89115002c9f516ee6fda57f3635fde7b"],["/categories/英语学习/英语语法/index.html","0549af7f5756ac6d9e848539e13f5d34"],["/comments/index.html","36dbf3041ec8d48f7128bcb0d35df236"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","5787575254d84fb516dc0a0a33ab0664"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","470896d05fa611cc499f87ad8a86c065"],["/movies/index.html","316076b1e5457cfd6aebef71a3e8078d"],["/music/index.html","872a6f3c8cac0ffae8003015d262cb55"],["/page/2/index.html","0743edaff5a5675f059eea24887a30dc"],["/page/3/index.html","7917a775e82632cae4370c7c41b4f3c1"],["/page/4/index.html","f939af8cc3fea0c93336d52d939ba6e9"],["/page/5/index.html","602d673ad9097850e92ac58d25aa065a"],["/page/6/index.html","bdbe278ffb895764b835ad86a22dd89b"],["/posts/1021360842.html","c27359bdc1486ea51b2fd95b16247416"],["/posts/1120620192.html","7342da88cba78200522ae6af7626131c"],["/posts/1141628095.html","70ad3193e6d03c112ac776cb7a653b90"],["/posts/1168613674.html","1277d4f14a6497a861a9d8b463878cf2"],["/posts/1219920510.html","dd4530a20811636f17235f9e3bb374a3"],["/posts/1222166338.html","758cb9d6e629cc254a4266cbf74142bc"],["/posts/1259097482.html","2997e48ef2df615df685a4f503bd5f05"],["/posts/1271036369.html","b49498824898f6060192df019a5a7ccc"],["/posts/1312847445.html","abfa4ff7ffa1c293302c1845250d551c"],["/posts/135355774.html","f14fb8bc33ad7b920f8820938946aab9"],["/posts/1375344716.html","f4e3493dc11e7d9ed2a0b1b8c95bd595"],["/posts/1388991698.html","acd0010b5153ac02ee952c4fe23ad23d"],["/posts/1410315814.html","65ba260941c510722635bf6598c130c5"],["/posts/1452790229.html","030aa242fe7b094b2e855cd956ee521f"],["/posts/1470079884.html","ace456554dfcdb24fb4391c2327b5d70"],["/posts/1470079885.html","41c478d28a531df10880bf45d66ad314"],["/posts/1470079886.html","c3f42cb68dce509064f2239cf717ab90"],["/posts/1470079887.html","b8110a1d588e4155c5376c894e3f80cf"],["/posts/1498536549.html","8c1f8c9961f17d5992aa8c835a49e087"],["/posts/1539568593.html","3299e5b0b432b80db27a9331b7eee53b"],["/posts/1547067935.html","1e3740425b672f7a4b2f446888e5d2cb"],["/posts/1557866301.html","bcd94b63aa1d17812b4ede978083a34c"],["/posts/1571776361.html","6b373dffc236ff9f683d40cd3930d2d8"],["/posts/1605124548.html","377e4fc391c72c5b71d100c05f27a663"],["/posts/1633036852.html","2f435dd78824b5e548852cc8aaac1dd0"],["/posts/1674202625.html","a5ad0842b033a9103aafeab3a994ef76"],["/posts/1765123828.html","f697bc44613f66e7f3797b6bf26cf2c2"],["/posts/1767336200.html","62882a28e89bf91f7ecc9cc8cfe2bf7b"],["/posts/1776114197.html","d6fd8b39380ee152657e6526513c7d12"],["/posts/1817748743.html","f684221a9d8cc1408d46adb8350b88d9"],["/posts/1925125395.html","b401cab5fdde1a2654f03cc3933e0d93"],["/posts/1966191251.html","280205c3f04126a5bce69551fae9c965"],["/posts/1987617322.html","1bc95fe25a8f2ab9f389f046d52e8845"],["/posts/1999788039.html","dec328fdfc5818a374387aac6d04feaa"],["/posts/2075104059.html","42afb714a77ec4fa2a406bd90564ab98"],["/posts/2087796737.html","98603da02b1ff6fe3e5d5bd31fcd0d94"],["/posts/2106547339.html","391adb852ac6a46957bf650814a513bf"],["/posts/2207806286.html","a57c546f7154c912feda84ea0367c98f"],["/posts/2225903441.html","7547efd80d9445b03a5924c1dcc84a4d"],["/posts/2265610284.html","3691bd129baebb3e0ecac5c9b9eba17f"],["/posts/2281352001.html","512e920baddf043d66e89b68013b7624"],["/posts/2364755265.html","ccd6f34d5d2d0fa33eb8791fc585bb63"],["/posts/2414116852.html","fa368ab400eeca082e7a298684eeb300"],["/posts/2421785022.html","a68bd5548018458590cb02f764f73b91"],["/posts/2482902029.html","d8caac908b293c8698b35c3937740534"],["/posts/2495386210.html","bff877ba47017db4798a39c0d3be02b3"],["/posts/2516528882.html","aa86e6b0cf5649c9463dee310ecace50"],["/posts/2526659543.html","b6847224153c30d304fb096813bf30a5"],["/posts/2529807823.html","60e5d54c1040d23f6dd3f2370aee571d"],["/posts/2596601004.html","140204ab39aa917062923c98fe3dce54"],["/posts/2697614349.html","08b1e0e6cdee08e70c5b0a3bd95f9f7c"],["/posts/2742438348.html","4bf6bb9fb9ae4e2b9aace67e105caa00"],["/posts/2768249503.html","d3207a8d6d795eeb6c3b23c8226e6d34"],["/posts/2864584994.html","fb799749c776cfa1b9f9abd196d40b3c"],["/posts/2888309600.html","6034225537f35f222095a83bba9e0650"],["/posts/2891591958.html","36e3944921cffa63f293abe5ad48089d"],["/posts/2909934084.html","a429daaa6589fadb3f1099808e77e782"],["/posts/2920256992.html","032e675c294a1e60b25044bfae3a7404"],["/posts/2959474469.html","a85029034fc3a4f2328996606530a5a9"],["/posts/3005926051.html","f8e42068c9c0c2e67694b7d5eafc2837"],["/posts/309775400.html","448ddbdb362330455f8421ea41c0eb47"],["/posts/3156194925.html","a569477b749635d1a90627ad88c10421"],["/posts/3169224211.html","60f7c16069bc3453ac681cfa370d5832"],["/posts/3213899550.html","e4ba4ec86d53831ecb908ce285d0fae6"],["/posts/3259212833.html","eefa91e149f93ac0549aeb89ff773f24"],["/posts/3266130344.html","b1779151f5ac96b4fbcacfe1bb9d0278"],["/posts/3292663995.html","2998e2696c86409f0f113f89a57b8a3f"],["/posts/3297135020.html","fc6ad730d868aa58a47f5c7302fbcc47"],["/posts/3306641566.html","d647d38f8168bd9d8728a08cb0c6c9aa"],["/posts/3312011324.html","b1aeb1238d69eb92ba81e460c53a605d"],["/posts/336911618.html","a92cd9c8bad923a4cafb9811d5223fc0"],["/posts/3402121571.html","f96065d874bdabc64c4485d58f082cbd"],["/posts/3405577485.html","d0986f4b437e5b66434e5e77e39b6f28"],["/posts/3498516849.html","43fdacdc267aead34e0de48a29f4640b"],["/posts/3513711414.html","c4c1e00e879eca93550a5e0b3ca3a2bc"],["/posts/3523095624.html","b42846b0384532018f0f6c7fa6d5febe"],["/posts/3546711884.html","2a4aa201d8e0532dcc3c3405e0c67d78"],["/posts/3731385230.html","fd0328147fdae3dd7c2ae45031146b54"],["/posts/3772089482.html","215836dbb66ce79e72190f5477440376"],["/posts/386609427.html","b2f197f36a9b5f520bfaedf5312dd78d"],["/posts/4044235327.html","10347fa74309784435ccca55d691fb89"],["/posts/4115971639.html","a1416eb628cf0dbef3bfa5c404dc0b3e"],["/posts/4130790367.html","0d24a83e20dd909c58c5fe1a6dfcbae5"],["/posts/4131986683.html","ee8c171795f7676ee07fa005edd81cd6"],["/posts/4177218757.html","21504a8ccb85de7a16d3fbd64300142b"],["/posts/4192183953.html","fcdc27c2a48f3b922529e31cc6b46e0e"],["/posts/4261103898.html","9d699b63720ecbe02536bbcbb9f3571c"],["/posts/469711973.html","6672f69aa725610fe3fa8a1bb0ac691c"],["/posts/482495853.html","34b8a05aa7c8674cbb0051abcbd79425"],["/posts/488247922.html","204ffb19d06bbb0f902a6d9b9fee3e5d"],["/posts/517302816.html","77981773d6f38fa9a44845c429a7f87d"],["/posts/570165348.html","270e184c4fe2823549c1a69f36be4a75"],["/posts/595890772.html","2890dabfa507eda3459f7b86e030fd04"],["/posts/67485572.html","235365018d6f7b538e6e5fa97307a92f"],["/posts/694347442.html","e5f198a939c7bf027b3b9c13cdc17a25"],["/posts/707384687.html","5b84fa06e23ece5aff39a4e576ba2964"],["/posts/71180092.html","69085dda180218b646ce77caffeb4b37"],["/posts/716459272.html","3aae6a9aab5fe0091b047fcd2ffd182d"],["/posts/765481613.html","8c1337c84806bdc1fc2a0f8603877345"],["/posts/778231993.html","6b6301a03d2eda7924a432c3bf4813fb"],["/posts/795397410.html","0f148d6e28bb3f0ea3c60b93708295e8"],["/posts/820223701.html","9f5bb5372e30b3f19652668c9bed84a3"],["/posts/830372185.html","b4db76f3595b713d2f9486874d4820c0"],["/posts/88294277.html","d9a8dc6a10fb5f6f39036942a35d8447"],["/posts/939963535.html","33595e204610180e5226312e0d654002"],["/posts/983786067.html","fdc80d936de6ca9abca505651c2addaf"],["/sw-register.js","f76831ebdd5e0f816f05c5fa7d153441"],["/tags/C/index.html","fa4e54f322562b3e963831f0d252225b"],["/tags/C/page/2/index.html","ed2b9469c9ca85a292600496c057e094"],["/tags/C/page/3/index.html","0eca10aa7ad38ff38e2e2ff936174c58"],["/tags/C/page/4/index.html","a5c5ab5b19bb1aa8bded64d53ebe7438"],["/tags/ETL/index.html","e1fa4143133d6aa7fec013fca688b84f"],["/tags/ElasticSearch/index.html","dc91bfd451fb78aa9e7e054fe2386f6d"],["/tags/GUI/index.html","166e1ff1e0ee691fea81a4a83952ff98"],["/tags/HBase/index.html","f2daff31e722e5896e14ec690b8dbc67"],["/tags/Hadoop/index.html","94ba8bcd2bfb1d86c36ea54f57396aec"],["/tags/Hadoop/page/2/index.html","bf36ba2bf03f0f74482dd87a795c02c2"],["/tags/Java/index.html","71e00822dd32362e24eafd8d5527427c"],["/tags/Java后端/index.html","b82cf6b9079162104148f20b9ee0339c"],["/tags/Java后端/page/2/index.html","71c3ef3e1b7fb874480369bd5f9b80bf"],["/tags/Java基础/index.html","878719f05757b748199ca7157af51dc0"],["/tags/Java基础/page/2/index.html","949a1b3a1542bd7512ca70baf27dc28b"],["/tags/Kettle/index.html","153ea9baa0228cf1c72f8e13ff128545"],["/tags/Kibana/index.html","6c297b76bffc459b11b5ebbf42a2ad1a"],["/tags/Linux/index.html","fda997a69061e85f816cd0cad8d18737"],["/tags/Linux/page/2/index.html","2ed03a9739147b13a4138b9a9597511e"],["/tags/Linux/page/3/index.html","16aa2f727a7a7b79c5c3cbfdaf6c660e"],["/tags/Mac/index.html","2643c1aa4420d03d440f1bfac0724cd0"],["/tags/Mac/page/2/index.html","13a93346e59cc00473d0867d0c660ef4"],["/tags/Maven/index.html","4cd097a679b8427576bcdb0b65d6b3ae"],["/tags/MySQL/index.html","2676abd8c14eb1528ac81352ff0d9398"],["/tags/Python/index.html","dcd68cb3dde5c7ed49919cd612aa3661"],["/tags/Redis/index.html","0de2e80bd88df5953787eee3fdc64fc6"],["/tags/R语言/index.html","9aaf25677db2558d3a5a1cef8e7ff016"],["/tags/Spark/index.html","a66341a50678fbce9402ab438b7c7d7c"],["/tags/Ubuntu/index.html","6cd753fdf9ae4fb721f5f1c582b96a1b"],["/tags/Vue/index.html","989946d42251e954de52fbdd20c9cf46"],["/tags/Windows/index.html","f03247f7d44683b9fa91afa045ef94e3"],["/tags/ZooKeeper/index.html","f18451c724abd05f9d1d55ddb843af4e"],["/tags/bfs/index.html","c86660e7e0c6a184f108351cdc54ff10"],["/tags/dfs/index.html","1a5cb17ba261f6db860496c03309b3fa"],["/tags/folium/index.html","d9b7107a0b4d520f25db2df27d55cadb"],["/tags/git/index.html","84332ba1adf6977247121e99d03ccccb"],["/tags/index.html","44de415a285f0e257c91200d4496cba4"],["/tags/latex/index.html","5af344e8ca59227ba1dc8d35e85b5a47"],["/tags/中间件/index.html","01bb743a3d0dc1c2b5b0fdb789f38c09"],["/tags/二分查找/index.html","d8d92ca7de77cdbd1ca6a26e23da37b9"],["/tags/优化类/index.html","5e3e801b1fecae388de84acc3897982d"],["/tags/前端/index.html","631477f79592851d104bc4ba95b7771f"],["/tags/前缀和与差分/index.html","c98822c160827a60d26b21ba64fc7c31"],["/tags/动态规划/index.html","53a9d57bd987bbcfec15646bb9e51026"],["/tags/动态规划/page/2/index.html","3e021b98af059e55d23967c0a1cb80de"],["/tags/博客搭建/index.html","5c933dbc5382c26817ef1b55a06551e8"],["/tags/图论/index.html","7f0c186c1a1929a711a48fdec7a4458c"],["/tags/大数据/index.html","2264ddc0613c0936fb6fca4b99fe314a"],["/tags/大数据/page/2/index.html","1c02d5a89fa36c13d88900bcd75ad94f"],["/tags/操作系统/index.html","05e2fec785dbb2cec2cf08fb5d9b6fc9"],["/tags/数学建模/index.html","f6fbe2f477493d3c5793c3d7f935d31a"],["/tags/数据库/index.html","4de25fb9109a7c46d01fc4017bb2aff1"],["/tags/数据结构和算法/index.html","df9f00422859b8f1daac7711616ca51f"],["/tags/数据结构和算法/page/2/index.html","d64063ca3cea2a6deff3fcc33aa27680"],["/tags/数据结构和算法/page/3/index.html","7ce4a674414b11b327599c19de61f05e"],["/tags/数据结构和算法/page/4/index.html","2e9a1205b3cfa1cb2d22569f20943d5c"],["/tags/数组和字符串/index.html","972c0576a9c61637f2599a569ae562b2"],["/tags/数论/index.html","605be2eff5fb25d0601c7d140a4df406"],["/tags/枚举类/index.html","6294b1258cdcfc2af6569dd34779e119"],["/tags/栈和队列/index.html","3159ba7e2e1df6ea53e1c2d8f04ba07d"],["/tags/树论/index.html","8ce1977950a44c4cda14daf71f25e532"],["/tags/测试/index.html","896c7410dba780d9f833dc70b291dcca"],["/tags/环境/index.html","1d2ec94a15ffd2d5aef3db824bb4441b"],["/tags/环境变量/index.html","f2d4d2ef2f89abddf30218d835db4841"],["/tags/绘图/index.html","5bd140922c5ad169f3631ed6a3b71ab3"],["/tags/编程工具/index.html","9cb9d396512bcbb549887e76ebe55445"],["/tags/编程环境/index.html","17af65d4f047e1ea7a170f6bf2b5034d"],["/tags/网络编程/index.html","4ef3e9f91e9988e13bec9cb83e093d9e"],["/tags/英语语法/index.html","dc826cd7c70f460edcb5ac4ba756e215"],["/tags/计算机操作系统/index.html","d1c3249e7c2f88d3141d098174244dd9"],["/tags/论文/index.html","516b381fb620475f17e0e8066bf56742"],["/tags/资源下载/index.html","82b91b02a107767ddf474b02b3ea1e1b"],["/tags/链表/index.html","c73034dcf7bcd18283b36c13b4b99308"],["/tags/集合/index.html","cc38ee06ec3ac6eb6b34068365d8be47"],["/tags/集群/index.html","dbbc18938b34dd2b4dbc8959a1d39c8c"]];
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
