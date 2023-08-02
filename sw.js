/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","6fa0aee93a13692c1ed8bc65608e7c9f"],["/about/index.html","a269f2d5cf84a28ddf0993b02406956f"],["/archives/2023/01/index.html","c3aba09fef6c8d795c73649c5e49a2cf"],["/archives/2023/02/index.html","19b568b85afcbfa7f3419da7112a8ba9"],["/archives/2023/02/page/2/index.html","de9d144b721d9268ea59e01909beb5bb"],["/archives/2023/03/index.html","016d561e324b616f7bb4ae82b8cc94e5"],["/archives/2023/05/index.html","557bb40de2a3ee777d9b7c49e0fda70d"],["/archives/2023/06/index.html","e656b6082af6998b06174887f5d14040"],["/archives/2023/index.html","e5512148f7f207864848f1b31ed4124a"],["/archives/2023/page/2/index.html","32b32c31fdda97b28516f6f8155068eb"],["/archives/2023/page/3/index.html","6fdd7e9623e747ebdccfcd264f8fdee8"],["/archives/2023/page/4/index.html","ab6cef58de07723e9c5a733e50ee395c"],["/archives/index.html","55e430cf163a8450fbd13f4eb9d628a4"],["/archives/page/2/index.html","ae10e4cfad8d45d8a3a062f7e8b09e33"],["/archives/page/3/index.html","ecd2fcb84030fecc4932f82e2515b204"],["/archives/page/4/index.html","7e420a6e621b989e2a751d2ba59bd4c7"],["/categories/Java/index.html","cc707a783c5c29683c6dec6ea085a485"],["/categories/Java/后端/index.html","c05466ecd4bd2752bc55bd745e07637e"],["/categories/Java/基础/index.html","52d8af4100ca1a2bfda139254857cdd4"],["/categories/Java/基础/集合/index.html","b9a72e1fbb6a13309128a1c37c9dd76d"],["/categories/Python/index.html","4bd16b00a10ad77e2599656b45fb9af4"],["/categories/Python/编程环境/index.html","9782db2950e67773c0fbd8f40b620f03"],["/categories/R语言/index.html","180025dc029e21089fd4511140be385e"],["/categories/R语言/编程环境/index.html","5a59a74f610a2e3861a5fdc8716a7ab8"],["/categories/index.html","8292a5d49c21d54df2933c9b053dea17"],["/categories/中间件/index.html","e2973fa5e10dc1178ffea4f981f555c9"],["/categories/前端/Vue/index.html","748d687bebc8f839f188705c17af5347"],["/categories/前端/index.html","bb6867ed572a89217921557b08440e9e"],["/categories/大数据开发/ElasticSearch/index.html","c1405fe513389e6d0d41e6824edda8c4"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3df2a964d1a9f462c9b78edad02d3351"],["/categories/大数据开发/HBase/index.html","9e0035ca4bd6dc25b3383c939d7d074a"],["/categories/大数据开发/HBase/学习笔记/index.html","a87f323cabadd16820f79b03b289cc12"],["/categories/大数据开发/HBase/环境搭建/index.html","aebff2270ba15aef651f949071c6d876"],["/categories/大数据开发/Hadoop/index.html","ca223184976c5c984d7ee29082f80e3c"],["/categories/大数据开发/Hadoop/技术/index.html","590c25c63e85429a47c0dda5e1bde9d2"],["/categories/大数据开发/Hadoop/环境搭建/index.html","cbb485e468b3c207d5f2b771115d5fbb"],["/categories/大数据开发/Redis/index.html","0930f2338147d240bc7d1198d5a2518c"],["/categories/大数据开发/Redis/技术/index.html","ae55528557e80e30fd6aa563c9910546"],["/categories/大数据开发/Redis/环境搭建/index.html","cada0332fa670060165af43bfb5a71a0"],["/categories/大数据开发/Spark/index.html","afa5b1a5e13bbe0e43fea19550db4157"],["/categories/大数据开发/Spark/环境搭建/index.html","335898e6c429db7803154330f42ca042"],["/categories/大数据开发/Zookeeper/index.html","07b81ddfd694ab8855e14b13837b1de1"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b153b970467885e02e6cc7bf826c714f"],["/categories/大数据开发/index.html","393b4524a2c9336f4a37e5d80cafe806"],["/categories/操作系统/Linux/index.html","783679bb8399bac133d0d7de76654150"],["/categories/操作系统/Mac/index.html","a8ae1f0844921be52f6ab52b098cb80b"],["/categories/操作系统/Windows/index.html","3ab6937dd84b34b4d1c035f9d5d779d6"],["/categories/操作系统/index.html","32314d6c66492cd3660cebda3d77bed9"],["/categories/数学建模/index.html","1db0272fc7d96f3dee1c612478fa8283"],["/categories/数学建模/latex/index.html","a62635d3bf770a6a004248f7ea3986f8"],["/categories/数学建模/优化类/index.html","49fc0704ebedd7ed6e9fce58e2d61879"],["/categories/数学建模/优化类/现代优化算法/index.html","25efcbf5a49acfc4e9d2bb7b2d022c06"],["/categories/数学建模/优化类/规划类/index.html","039b781ec6b435a99b47c62f5500e3a7"],["/categories/数学建模/绘图/index.html","4c5dc967533318f4f2d6baf215816429"],["/categories/数据库/MySQL/index.html","9324b34de08f865f0c87c6df94998af3"],["/categories/数据库/index.html","6792ddf01406558ac51355a331feed9d"],["/categories/数据结构和算法/index.html","df5dc56d94e3fede067078c18d9fff56"],["/categories/数据结构和算法/page/2/index.html","852ce5809821ab64a4fa6421719697bb"],["/categories/数据结构和算法/基本原理/bfs/index.html","6dfd0adadfe920caba45a924b122ccc4"],["/categories/数据结构和算法/基本原理/dfs/index.html","f4658469072ccc3f621581ff30b03b88"],["/categories/数据结构和算法/基本原理/index.html","de6c645e06cca02d6a2a61d93e33979d"],["/categories/数据结构和算法/基本原理/动态规划/index.html","647df04db1244920b26f3db4ce82e375"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","22d5a800228cecea0770ba41dbf98199"],["/categories/数据结构和算法/基本原理/图论/index.html","8e88b98c12b5f47c90858de22a30aaa2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6cd8b779390974e3d34467b9973d67e2"],["/categories/数据结构和算法/基本原理/数论/index.html","325e6f470d051429a07c32779da7a413"],["/categories/数据结构和算法/基本原理/树论/index.html","e19118770d671b3a6764abad4791602b"],["/categories/数据结构和算法/基本原理/链表/index.html","51b7e245397554968d25a1f283cc6690"],["/categories/数据结构和算法/算法题/index.html","674e9a90895c0ee584a375b0dc91ba11"],["/categories/数据结构和算法/算法题/二分查找/index.html","c70934dc8698575e8a9b4ca11532d349"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","40e2eca230c10b0210a508f6e131438a"],["/categories/数据结构和算法/算法题/动态规划/index.html","81c2ecf92bd6cbd5ba581b09376581c1"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2b93ae3f65620b8dde0f06c6704765fd"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","e0819e1f8c165c20e0b5b3a9ef3420d6"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","3cbe5522d4e54a1559b8d37aa205a3ad"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2c4c03da80ec5efce88a00daa1cacab7"],["/categories/数据结构和算法/算法题/栈和队列/index.html","81343076c023d5d04f162726f7191ef0"],["/categories/数据结构和算法/算法题/树论/index.html","a9369a70d3effc385a736c007a5c3ae3"],["/categories/杂七杂八/index.html","d7da1085cefd10f1738a0f957f54add2"],["/categories/杂七杂八/博客搭建/index.html","4eed1e3ccc1a02c3dab490ee6ea0b98f"],["/categories/编程工具下载/index.html","9496f63db53cc85f6880ce64c7ba611a"],["/categories/编程环境/index.html","ec04c1cc3afeb75ef4fb5d943b419943"],["/categories/英语学习/index.html","2346a41726a72ef0704292489b99f5ee"],["/categories/英语学习/英语语法/index.html","a41346f31e92fd7954067154de55ea4f"],["/comments/index.html","e7cc851ea3d98ddf718fd97d4b1470bb"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cca3bd91887a610b9a498f2530b2e7be"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","293b15dd83b3b8b33aceda0d98c9aaa9"],["/movies/index.html","b73260bd30243afad0b8a58ddc4004ad"],["/music/index.html","9df98c991855e256b0ef5c816fda6344"],["/page/2/index.html","34387fd317ee08ce8a8a9d1213bea23a"],["/page/3/index.html","a1a2718dab190291d54d7e4c71987922"],["/page/4/index.html","66495bce76eebdacf8cba3bb23c3e099"],["/page/5/index.html","db1e9570b022505bf9e9c91a1e8d5a4b"],["/page/6/index.html","d80cdda7085c2c09c04865febc6c9770"],["/posts/1021360842.html","19a037285d192e8de8e0121868ace0fd"],["/posts/1120620192.html","0750808e0a27e8314823d5592de83bc1"],["/posts/1141628095.html","20a883fe99982d98048701f14066f42a"],["/posts/1168613674.html","4af19dd3e1e2e2fa6b09643a9dd3a558"],["/posts/1219920510.html","436b6f9d34d9873a7117a0fba43f4bf4"],["/posts/1222166338.html","bddb5f129ee92d2d721507edeea81849"],["/posts/1259097482.html","a25278fa372365ef98875651f31986e7"],["/posts/1271036369.html","05e7f4c1349fe7ec12f94463748f711a"],["/posts/1312847445.html","113d8d1b41a47620f912062e833495b9"],["/posts/135355774.html","70010a8dcc3c1f12b650e3d83ddddc4d"],["/posts/1375344716.html","6239710a627c07cc4f4a86b9e4bd06d4"],["/posts/1388991698.html","623472d3bef7c652beee4114339db48a"],["/posts/1410315814.html","2c1bc9cb697d52ab69add548a3b884e5"],["/posts/1452790229.html","a9afdd68765d5423d0c23ab26954d05a"],["/posts/1470079884.html","be5fd4dd211fdcd746a4f7c07b2eb29d"],["/posts/1470079885.html","72ba7340cb173d68e5635ebb82ef4900"],["/posts/1470079886.html","7e524c0821eb34d355d6585f76a249ab"],["/posts/1470079887.html","1f722f33d59f510b46f91feba748d240"],["/posts/1498536549.html","bfb924ddfe5cfa261e7aa15e511f9371"],["/posts/1547067935.html","ba16ef2f3ddf7ad0b533952d206d2366"],["/posts/1557866301.html","36766704ec85eb8b87eca17062fdcf7d"],["/posts/1571776361.html","4c696dbffb3cc5d097b6a3ba442dd908"],["/posts/1605124548.html","461afcba02471acd428d099eb526c622"],["/posts/1633036852.html","6c169f755fddfca50cf683b74ea098a9"],["/posts/1765123828.html","a831b2a38875910d166c2037ada4ebd8"],["/posts/1767336200.html","55d0bd95f1cd97ade21627830b158106"],["/posts/1776114197.html","b16e999155fc98e706013cb499ac0a46"],["/posts/1817748743.html","fef0df9cdb4948a4738d81486e9afa49"],["/posts/1925125395.html","ec73b5b972a1c0ddf5639f03373030f5"],["/posts/1966191251.html","cd5c5836d38a059fab5f0f201f114614"],["/posts/1987617322.html","65056b2d529e01187a0d129a258be45e"],["/posts/1999788039.html","659b0ed253ad51caba45f7b95496e35c"],["/posts/2075104059.html","fba610bb6457ade21c58b41f7a67c338"],["/posts/2087796737.html","61d875e3f04d11f1f240dcb3f418e9ea"],["/posts/2106547339.html","098405c80d6ea3982521ef93c3ee6b87"],["/posts/2207806286.html","1bf65584c4021719267f4b831fa880d5"],["/posts/2225903441.html","bef304fe46abb27e1f4e2f308afb4851"],["/posts/2265610284.html","762928ad89c6665de132fd333e0d8b05"],["/posts/2281352001.html","cc4cf3a7840b67d3ed183276386f43fc"],["/posts/2364755265.html","84374303ca2f680fb474c73a4535a268"],["/posts/2414116852.html","e2d2e7b13b40829832f01224f8c40a37"],["/posts/2421785022.html","9823a7dabd6396d470c8a1e75834a681"],["/posts/2482902029.html","2be96d74ab6c54fc9d1cc8106fa4e5c2"],["/posts/2495386210.html","4b80277f79f12534b64ffd301b145039"],["/posts/2516528882.html","60d3aec705680e9ff746d6066425501f"],["/posts/2526659543.html","293c5e7343a5411f180fd6eb55b51230"],["/posts/2529807823.html","4d5a24f74c57fbc2cbd17ed60317a89c"],["/posts/2596601004.html","0145d3fcd813165a268b5107601dad2b"],["/posts/2742438348.html","7bae1fade0c30a2b87ebee6ac4759ee7"],["/posts/2888309600.html","88cab37673919250bfef940d07ea0f2f"],["/posts/2891591958.html","46d16d642bdfa73dace0d2793cbecd7a"],["/posts/2909934084.html","b827621ace91a2a0e75696cad3c812c0"],["/posts/2920256992.html","a6d13e220ad04855cb39be761aa8a346"],["/posts/3005926051.html","ea33eebcc1f36e64017dcfe5a3d49fda"],["/posts/309775400.html","5e82cedf0c5fc70c3b87a7bf1589283f"],["/posts/3156194925.html","8009fd87fb7e5fa6cc4ee1d221609737"],["/posts/3169224211.html","d500dacddd4768c8ecc310acd690f8cd"],["/posts/3213899550.html","b3a04cfc3c02be6533c2098fd231567c"],["/posts/3259212833.html","d1844c4af3ec27ed1f108bad10f6deca"],["/posts/3266130344.html","e7dea9dfcf5c18d082f87e35d05ca068"],["/posts/3292663995.html","556dd1bc94bd4c1ccdfad3fe0a8af68e"],["/posts/3297135020.html","2df17f806c418936692a7b32b14b8cf1"],["/posts/3306641566.html","113e7a95e07d3ed43484f5dfe525bfd7"],["/posts/3312011324.html","5a07ac09e4915ad1af339248230a107f"],["/posts/336911618.html","bd5d3cb8879cdc27ed7e8f6acba94db8"],["/posts/3402121571.html","e544addd1dd1869f19b0d2e041d6325e"],["/posts/3405577485.html","0628fc78600087414fadef69bb250236"],["/posts/3498516849.html","e03d38011dda848ffd9b400981cda48f"],["/posts/3513711414.html","7ee2c801f80babe88c82c4256d82f03d"],["/posts/3546711884.html","af54a238fa1ee11e88a825c14dfaf1a3"],["/posts/3731385230.html","9da428c4405ad91b939af45dc5cbf5de"],["/posts/3772089482.html","09b32d67291ca30cad7dc9764947afb5"],["/posts/386609427.html","d808398573b0ef2038721f751378681f"],["/posts/4044235327.html","0ed76c9afff892a9fd7b36ceb6eda388"],["/posts/4115971639.html","52ab58f43e1a1113e4b518a4fd12490e"],["/posts/4130790367.html","f401ceb4b30e3a349a7e78cc8172fd26"],["/posts/4131986683.html","bfd094f2cc17e67595b605d654322697"],["/posts/4177218757.html","ff01fc93e0b31b0031dda1163887fef2"],["/posts/4192183953.html","bc79521fcb52ba8b0369f1990862e464"],["/posts/4261103898.html","f859b775f1c4978b0b138ab11f539ff3"],["/posts/469711973.html","3705480ce1b0ea30b8ebacf982fb50a7"],["/posts/482495853.html","30fd6d4b0b012975b1eee793535e86c4"],["/posts/488247922.html","84592d5c7be67d28746c0b3d18e75074"],["/posts/517302816.html","79c6db2952d63f4e2e42148f4e86572e"],["/posts/570165348.html","c1c858fa701b17306ebf64d35a9662cc"],["/posts/595890772.html","6ef7134bf559c4761b20c523a25c7b01"],["/posts/67485572.html","4c685a4c4d7eaad0de21beed1569ab9c"],["/posts/694347442.html","b0bb1e1fa33235f9db11047b8cb249e5"],["/posts/707384687.html","703e2b4f5491bcb083985ae63dd31aa5"],["/posts/71180092.html","58d3e1b31668e20c3aed195338cd0661"],["/posts/716459272.html","516a3637fd77b7b09580b7b3e1667a47"],["/posts/778231993.html","49e72547eb2c7e1c28d43666d849b89b"],["/posts/795397410.html","aaf194683cc4e3cc179898b5548f7c38"],["/posts/820223701.html","95d74b3305809cc6ee6c847989a026af"],["/posts/830372185.html","390789a420f97e9cdc556e5fc4c4880a"],["/posts/88294277.html","1d568bf1026b87a25af9d52471959514"],["/posts/939963535.html","26e09a76357458d52c072de144675294"],["/posts/983786067.html","59e32c6d8527feeb4fff107d648a4566"],["/sw-register.js","27472a11f14504ef4a7885db5d2ea345"],["/tags/C/index.html","bbf5bf535373a794615ba4fbb174224e"],["/tags/C/page/2/index.html","62797d32608691eae13c9955cc13ea8b"],["/tags/C/page/3/index.html","9c8a27d2c2cbd6aaec0c6a43518b4cc6"],["/tags/ElasticSearch/index.html","19d4f258553d64b81171acab06226714"],["/tags/GUI/index.html","1a0619bac43135f8937c208a359fab2a"],["/tags/HBase/index.html","70dfd8ccd80d39778e38e663afa1a312"],["/tags/Hadoop/index.html","cb19f613be79979a521d5f5e3e3ecac2"],["/tags/Hadoop/page/2/index.html","e4474d68d2b3b247f10200cc21499936"],["/tags/Java/index.html","20119c0952230170e35799bc43b03ccd"],["/tags/Java后端/index.html","7fa9d96cfe94209a1321c89bdd3f42b1"],["/tags/Java后端/page/2/index.html","1526d19d61b8abaccf779cb4ed2b184e"],["/tags/Java基础/index.html","0fbea0572c40840b8aca656fee61b589"],["/tags/Java基础/page/2/index.html","097839bfb454ffd9d4ae708b8455725c"],["/tags/Kibana/index.html","2bd751113904134863e78656d1b1f09c"],["/tags/Linux/index.html","a6a20cccf28cde3fe4f1ab9ee30fce93"],["/tags/Linux/page/2/index.html","214248cfcc54489cc81e05b88c3c0f34"],["/tags/Linux/page/3/index.html","203249fc4ea11f1d5baced4c03b80b22"],["/tags/Mac/index.html","1f6f1b9a4b6421af90a03c71ed264248"],["/tags/Mac/page/2/index.html","08d298ff6b4cec5eb769697c049c56f7"],["/tags/Maven/index.html","ec862682812988ba9b6eed4c4fabd46b"],["/tags/MySQL/index.html","b9694fbd0bbff02a645ec425346b9cd3"],["/tags/Python/index.html","c09ff244a11ec122928c127a8089857b"],["/tags/Redis/index.html","bcebffb4205bed19b57416e61eaee1db"],["/tags/R语言/index.html","7dd56e98bf1974f788bed2f90e165ee2"],["/tags/Spark/index.html","efb8a783dcefe840baf84acd1a8979e9"],["/tags/Ubuntu/index.html","c7678a883dc3da59b6b8fd2a846ac26b"],["/tags/Vue/index.html","1c964b905faa617dcbb5a4e0979a8482"],["/tags/Windows/index.html","1f0b6e73ea0e6613100c41398bc0b78f"],["/tags/ZooKeeper/index.html","2cd01b86a13b3677e537656065188cc6"],["/tags/bfs/index.html","7bdd4a8e2adf12c612d409e57df1c8a9"],["/tags/dfs/index.html","34c44ea52c5c9ca84e83fe89f9c5fedd"],["/tags/folium/index.html","f7bf4de1319c0a6ecc8532c1d3a1f791"],["/tags/git/index.html","103127e910f5810916e08332b5514d9e"],["/tags/index.html","15bae48e233a05c3ce087885176b4169"],["/tags/latex/index.html","1a772cf991525024727d6fed812193e9"],["/tags/中间件/index.html","3fd7300a1daee0d050a57358fefa2d1b"],["/tags/二分查找/index.html","31672333532616b33e67ecdf208e3f1a"],["/tags/优化类/index.html","4ad5efb54f56ba942e047f9c01069e1b"],["/tags/前端/index.html","d4a0a073c506a9011f1eac8b363a9416"],["/tags/前缀和与差分/index.html","b6f51fb67d4390f8c29f5d2b0e1a2bad"],["/tags/动态规划/index.html","39fb9776e7c333926a856db9c3557748"],["/tags/动态规划/page/2/index.html","6f6159f86714507782b85431a1cbbfe2"],["/tags/博客搭建/index.html","405ed54b023f774acb38d2c9e7b267b4"],["/tags/图论/index.html","efaa627e3c4b7a96f6d36437046bd98c"],["/tags/大数据/index.html","d0b65d1ab212672a17dae2fd33c4308f"],["/tags/大数据/page/2/index.html","566278d167175ca673f8a7e03daf27ed"],["/tags/操作系统/index.html","978024969b618ab955eb666d00ea6d96"],["/tags/数学建模/index.html","3390a462f4aaee86826ea46a66a87873"],["/tags/数据库/index.html","d7bd803b4d4703390a059cb7a4b87185"],["/tags/数据结构和算法/index.html","d1ba36c68c67678e6a7b21ef049a4ef3"],["/tags/数据结构和算法/page/2/index.html","a6029ce9e501364ec97b7faf4fcfc658"],["/tags/数据结构和算法/page/3/index.html","064d21e68434650a0256f1d298f04613"],["/tags/数组和字符串/index.html","5b534ef6a933b0412a091d9e3f318651"],["/tags/枚举类/index.html","d7cfb082c94e428ffbf935566e5f0078"],["/tags/栈和队列/index.html","9af73855c92296f056ee6fd3f8745395"],["/tags/树论/index.html","09121f31cc5c1b487018277d955d85d6"],["/tags/测试/index.html","e0f6ddab1a632a48bb37503e8da22dff"],["/tags/环境/index.html","c78c8feac5cc5e27b38094d0a747ae1b"],["/tags/环境变量/index.html","11c2300ebe4b916634d19e3f7c9664a5"],["/tags/绘图/index.html","a94156a377a62e7ba3649ac25036564a"],["/tags/编程工具/index.html","bdadb1851eb822833d937a0cd5d2f9c4"],["/tags/编程环境/index.html","9e1706ef7f655f0f2db1f05ffec854d8"],["/tags/网络编程/index.html","9a69547e0cd94e70acd4667fac3246bf"],["/tags/英语语法/index.html","a67b654dd9a199d5211aa9fa8607dcd5"],["/tags/论文/index.html","29969e66fb7c7fc7ed9826c7740b0d24"],["/tags/资源下载/index.html","496911650f0616e4eceb1a03f640a66a"],["/tags/链表/index.html","a03c056fbab9eba8d81b9c4ad136d359"],["/tags/集合/index.html","736dd666a51c553e56ce3c10b002bbab"],["/tags/集群/index.html","4eff0cf2e222540120dad6b866ea4884"]];
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
