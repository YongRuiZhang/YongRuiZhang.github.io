/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","8b3050583e13fb481fd480a356bc8a5c"],["/about/index.html","f2aab1dec6d6d1e11d5a7bec49fac579"],["/archives/2023/01/index.html","67d392442295ed041c26d93b7a2d7a72"],["/archives/2023/02/index.html","46a25e9899290a68b21839553d280e22"],["/archives/2023/02/page/2/index.html","8a9f2a0a4ddd02d744099eef6548ae89"],["/archives/2023/02/page/3/index.html","f7070912c8b7de9e02a85d56b2df9524"],["/archives/2023/03/index.html","91827ca6b11d0a7e8a4d81b5ae76415d"],["/archives/2023/05/index.html","ee2df5fa6b1f227b2a7b9c08730d6244"],["/archives/2023/06/index.html","0c684da45bd58056215716e28fa1664e"],["/archives/2023/09/index.html","c8f07b0cccc1f63136550d456c3cd5b4"],["/archives/2023/11/index.html","0d9c8f7cf0cd16abc8ff020d8219659b"],["/archives/2023/12/index.html","189c5067bc9f050291a667278b8404ae"],["/archives/2023/index.html","1bcc140c3fff4f0845b330fce8ebab7d"],["/archives/2023/page/2/index.html","a4142bc29f17c37b5a0046f25992dce7"],["/archives/2023/page/3/index.html","3ccde4d2becfd641a8194ab58ac968b4"],["/archives/2023/page/4/index.html","4c5b5c5ce0bfd7493c48fa7961764581"],["/archives/2023/page/5/index.html","3e4a79f7978a952314672a8c7de01e8b"],["/archives/2024/02/index.html","620e3c3398e6b59e33e0bda3cf47764f"],["/archives/2024/index.html","16253be13ed98b03a2e3d2b7db176e9a"],["/archives/index.html","d0e6a1a2a24a133182171bfc8483c170"],["/archives/page/2/index.html","36d1584b857f27ae67f29218eebdb9ce"],["/archives/page/3/index.html","8cb827d8b7518fb4ec6d96b87f0b299a"],["/archives/page/4/index.html","216e272f830839fec6dd5db0c77ca35a"],["/archives/page/5/index.html","038af880dec0e45b0f6790e4290ee2c5"],["/baidu_verify_codeva-qQP2iZOMLX.html","bee7e9f67461dd93ed05218005b5725b"],["/categories/Java/index.html","03cc181b75bcccb32e15108790e49113"],["/categories/Java/后端/index.html","8a69dce4e90e136c53a6dcfd5fa4ebf9"],["/categories/Java/基础/index.html","37ae71bf1aa209dfe016d3410e7617f4"],["/categories/Java/基础/集合/index.html","433b088291bac67386d35498d5d47058"],["/categories/Python/index.html","4153c8e32bd4693ed7722a86f0f5a4a0"],["/categories/Python/编程环境/index.html","08c00d09c0e3021d267a78e2f96e4343"],["/categories/R语言/index.html","56a4c3ea0cb7e4aa4c2d5bdee0bfa8a4"],["/categories/R语言/编程环境/index.html","603c804a9cb46e22a678eac2158dd30c"],["/categories/iPad/index.html","d3080a5dc2b51ed8b6b49283bb0c694f"],["/categories/index.html","5534b7fb6b3200548adc85d4d1140581"],["/categories/中间件/index.html","f5b29eb14de5584dd598b6682cee533f"],["/categories/前端/Vue/index.html","32e2a117f1ad39a06499700f2d35656c"],["/categories/前端/index.html","8cfc3dc133e4b30092a79bfbfcd7e070"],["/categories/大数据开发/ElasticSearch/index.html","28cc473c861081f552b9a2c10c3ddbbc"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2596b5461356b5cfa25f8f3f1a4e672c"],["/categories/大数据开发/HBase/index.html","13fd054762bd4bb2ee440fa34fb34f4b"],["/categories/大数据开发/HBase/学习笔记/index.html","2a0f987ae63295afb8c2307fff92997e"],["/categories/大数据开发/HBase/环境搭建/index.html","99bbb0e0188fd4d592cab7bf5829cf61"],["/categories/大数据开发/Hadoop/index.html","efd34bff773cb7cdea6b592b0540a0a8"],["/categories/大数据开发/Hadoop/技术/index.html","3a53f67675e19fbfd4bb2f3697c841bf"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e58c085ccd54c980e6231d6b020f83c2"],["/categories/大数据开发/Redis/index.html","bfabaad12d9ed57bfafe3d2dfadfbfe1"],["/categories/大数据开发/Redis/技术/index.html","dcc22071e68d7355f23c1dc49cd86a19"],["/categories/大数据开发/Redis/环境搭建/index.html","47bdc702b9da1701e07c2a7b17444fdc"],["/categories/大数据开发/Spark/index.html","c5db9982c2a8c30de4c531d8ac794c7d"],["/categories/大数据开发/Spark/环境搭建/index.html","a9538cf052d75e51ae981306f36250b7"],["/categories/大数据开发/Zookeeper/index.html","430d228ed87c39cbf734c0d8b29e18a3"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","cb472ddb94eb16896fa6ba45cb7f935c"],["/categories/大数据开发/index.html","eadea55539bf056044ba79d5e342beba"],["/categories/学校课程/index.html","ab30ac734940f4a85c34290589cd4a61"],["/categories/学校课程/计算机操作系统/index.html","8527d2782de917007859a868e8ed65c3"],["/categories/操作系统/Linux/index.html","c01b19e40f1a7a687063eed2e842f6d8"],["/categories/操作系统/Mac/index.html","79ffed5b12127e6efc427e1621314cf1"],["/categories/操作系统/Windows/index.html","d60b5946a9762de05edc2ed6d4d4fc47"],["/categories/操作系统/index.html","1a88e44ae93b8f7b142e3345423ed011"],["/categories/数学建模/index.html","1dfb4ee043cd71b874104fd2664fd44e"],["/categories/数学建模/latex/index.html","59fdaa19b1dc5be643ae491e7a4b5ecd"],["/categories/数学建模/优化类/index.html","4e2dc60f6c91699c82d304836ad71b9c"],["/categories/数学建模/优化类/现代优化算法/index.html","2ea431f28f20e41c9a684f342d03066a"],["/categories/数学建模/优化类/规划类/index.html","d9f880382eac41182fe30d7df5319db0"],["/categories/数学建模/绘图/index.html","4cf91157204247dcfa1da2d4ac761178"],["/categories/数据库/MySQL/index.html","5b02039b8a932c773e406f95a4ef6f13"],["/categories/数据库/index.html","faf239c04e3994f60202afce52107424"],["/categories/数据结构和算法/index.html","6f76b13831b96461529803d6e1405e29"],["/categories/数据结构和算法/page/2/index.html","da88aa751bfa8d5eba4591ccf806574a"],["/categories/数据结构和算法/基本原理/bfs/index.html","e493991ab5b6ab7d5f13ee4d8c8e1b21"],["/categories/数据结构和算法/基本原理/dfs/index.html","e1bc6e5667134bf42e3fd7aa12f0f6bc"],["/categories/数据结构和算法/基本原理/index.html","96b29660a41f5584f10288474c2c655d"],["/categories/数据结构和算法/基本原理/动态规划/index.html","362a41a40515667f7a5d9a659c351679"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","c19d8a903d9071dafc097ce992a418f2"],["/categories/数据结构和算法/基本原理/图论/index.html","9b198e92fa6e19e5fe5091b34a87eb37"],["/categories/数据结构和算法/基本原理/字符串/index.html","feb3e25651fb597d4dfe75e07da44280"],["/categories/数据结构和算法/基本原理/排序/index.html","430e5ca191722fdd4f2ad024ae488ca3"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","11f2f10422421d9f15a5f758c9d4cf90"],["/categories/数据结构和算法/基本原理/数论/index.html","1c63ca97695f0186c0cc8efeab01c972"],["/categories/数据结构和算法/基本原理/树论/index.html","9b8f76e52ba61fc2a4142d8432977a3f"],["/categories/数据结构和算法/基本原理/链表/index.html","8ba0723169015271b12eda359a446ed9"],["/categories/数据结构和算法/算法题/index.html","27cefda7b4a6a0ca00a620e54a30b12d"],["/categories/数据结构和算法/算法题/二分查找/index.html","07861d349d28bedd182a75281e676388"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","aba1f6446e3faa2f47c5aa954e381851"],["/categories/数据结构和算法/算法题/动态规划/index.html","316a3955006d961cfec0db7a2deea9cd"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","0b4cc9f97fe5ebc6435e0592b47998ed"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","ee3bdb1c91143d20094b01a5e87e2e52"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","499e855582eabd9c0fa3b6373057e11f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","206ebad1ea956a3d4a4815993eee372f"],["/categories/数据结构和算法/算法题/数论/index.html","d880c050ac4f01696c3ea0c17ebba448"],["/categories/数据结构和算法/算法题/栈和队列/index.html","dce7af8fd110e30065b12286c476578e"],["/categories/数据结构和算法/算法题/树论/index.html","89f9fc4798259b99ec1ec21ca21104bc"],["/categories/杂七杂八/index.html","55f4207512d603f24acfdfb1cf7350d9"],["/categories/杂七杂八/博客搭建/index.html","b477234b185e9f366c5413b75d43e3f7"],["/categories/编程工具下载/index.html","4df7c9a812c51ff7180d4def0e36e204"],["/categories/编程环境/index.html","7965a251c2f23e2f7ea8715ad467bb6c"],["/categories/编程环境/大数据/index.html","e6563eae5b0bd9be3cc88b524dbc7165"],["/categories/英语学习/index.html","d774b9fba8beb03e09e7e4a2c8b1209c"],["/categories/英语学习/英语语法/index.html","9d049bd89ab7ae32427be51a5a16b21f"],["/comments/index.html","d80f62c3e8dfe3f5830875241deb7928"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","dfbf862da746e71859caf1d4cc2732c3"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","6a115a03928e195fb752f4c65436bfa3"],["/movies/index.html","521572fc98ac5c379eb4772f7bf12455"],["/music/index.html","9092faaea4b6b9390d870dfad4b5d07a"],["/page/2/index.html","85d06d73f04ca1591dee5283f040816b"],["/page/3/index.html","a8575db0feffb2dbdff306113bb03c39"],["/page/4/index.html","8b36e6e3e75e513067b07b9283cb9f24"],["/page/5/index.html","51769709a14d004880c9c67216bd2f76"],["/page/6/index.html","0ca8086c2bc8465a1a875c6cb6f16478"],["/page/7/index.html","613913633912884552a36061dc1b71c0"],["/posts/1021360842.html","c06dfb6858bd68d51cec8e77529a7ba6"],["/posts/1120620192.html","7eef9fa815dbf34d2e59ac0d2c0b1232"],["/posts/1137707673.html","acea812efae9ee66e46cc14069963c7b"],["/posts/1141628095.html","2151bae799fddf279a9b9ded314dbf87"],["/posts/1168613674.html","b544a38f3fefa24a2c6e971bc770d01a"],["/posts/1219920510.html","ec8a106c2575cdb1301f9b6b9710f72b"],["/posts/1222166338.html","41646cd3f1ee0702fc077b72abad5d61"],["/posts/1259097482.html","a85f06b7a1d657ed4052e5df9b581b7c"],["/posts/1271036369.html","649e1d0034e49fb983c44c025dab8b8d"],["/posts/1312847445.html","fd7bb71fbf558cecc90a5fe8830cbdfe"],["/posts/135355774.html","08230e1577b342d0ec14c519a00ce1a6"],["/posts/1375344716.html","427b16c9738de42e1b1c885fba5f9e1c"],["/posts/1388991698.html","2799910bf7c3243ae821509a29ee2da7"],["/posts/1410315814.html","09e48159818d638146fc85b60281e300"],["/posts/1452790229.html","1b636fbf82eedf2df26ea2195f54925d"],["/posts/1470079884.html","a4e9d88a79cd86ac57923309c3cc1adf"],["/posts/1470079885.html","511eb75cb24db008b360175971c36b8e"],["/posts/1470079886.html","6aa6b111cff14cff6d98709899981106"],["/posts/1470079887.html","79dfafe1c3ee823457d67ad610f064f8"],["/posts/1498536549.html","87f140a0cc2d8d59ad0b89b2cbee2584"],["/posts/1539568593.html","18c7cae49da8c392f34fd6dc11fe0077"],["/posts/1547067935.html","d1ef04cbf29969a16cba4c24d13edeaf"],["/posts/1557866301.html","d0d0ecce64eba5bde1b2dc1ce50b968b"],["/posts/1571776361.html","d40db926402e90fdf24300cce483d661"],["/posts/1605124548.html","4e65616b86d4dfb9f4d53a4574709ef5"],["/posts/1633036852.html","a651ea437cf35ca4ffefc9b7bcf870e1"],["/posts/1667740714.html","ed1f17c29eb894d54faf6ca3fcc0b5af"],["/posts/1674202625.html","8e2a98af0dc2b3bc9aedc0c90f56f256"],["/posts/1765123828.html","fcfc8f458fef97051d1733a2a7ad4f35"],["/posts/1767336200.html","041b575d6b54cbd656b120b9c9f5110c"],["/posts/1776114197.html","90e0ed4abe6232baeb47a4e1abf4bdfe"],["/posts/1817748743.html","9c33cc6ba7c4de271cb3ce45e5ce2ba9"],["/posts/1925125395.html","710db218eaf0c50e5e6bce6a12def808"],["/posts/1966191251.html","f3b610ce0ece24e8c2162be691405ede"],["/posts/1987617322.html","062cc37ed97ec84089fc94c56813eaf9"],["/posts/1999788039.html","0d5730c61ed385ce2cf6e77a1dc8d2d5"],["/posts/2075104059.html","c9bf1f55a3320a6032f948049451ee4a"],["/posts/2087796737.html","93cc7fa8f86d43e5dc6910b062faadb5"],["/posts/2106547339.html","5b7569c945105de4e3f02a6ff230d66d"],["/posts/2207806286.html","04d794895fa8845fd7bb59aa6391c7c3"],["/posts/2225903441.html","6cc2eddadb8753db75db1bd48b679076"],["/posts/2265610284.html","aaedc15e0b8600ed7400e3a9d5112d03"],["/posts/2281352001.html","d6b8f72efbc15395771a8fb2f7bba9d5"],["/posts/2364755265.html","1c1bd55d23acb717fa443b0099974f1c"],["/posts/2414116852.html","62b5331b8dcaec98ec4952f1e8fe921d"],["/posts/2421785022.html","82f2157a61f4e7fcab5655ad1e99d283"],["/posts/2482902029.html","da3051f3c612d01d8d287485e90a91eb"],["/posts/2495386210.html","25ad3ee8b24f84e97c2a7e6f4207d5af"],["/posts/2516528882.html","099bba6347fdc3a9861b4ab73285543b"],["/posts/2522177458.html","4a497090e0e73fb1286fc40324bc7c8d"],["/posts/2526659543.html","3679ec47682dc56091a4b8a0e3a2ab04"],["/posts/2529807823.html","431f9f461ffbb7508e2ddf21af6bbbfe"],["/posts/2596601004.html","6a204170c2466f41a2a6d76e694bde2f"],["/posts/2697614349.html","c6ddcbcc576497e55fa25fdda8df318d"],["/posts/2742438348.html","b174f61b3f89003e7090d639a7304acb"],["/posts/2768249503.html","5c402792dbfa81dcf6901745072a7285"],["/posts/2864584994.html","20d7e65f52652a4cc654f9a140390109"],["/posts/2888309600.html","8e2562dd599984fd2e9a69244cb972f5"],["/posts/2891591958.html","a3c57d37c1f4f5d5aeb2e14f9587943c"],["/posts/2909934084.html","446dfee91ce145d9ee4dab3b8d88a302"],["/posts/2920256992.html","83cd627381737b3c5356944e7ac0bb84"],["/posts/2959474469.html","0544a2a33aba8fa911437f425bdde67a"],["/posts/3005926051.html","acf14850471ea10dba38c3e551c2c1d5"],["/posts/309775400.html","641ac9e8c369e275ce8dcdf007636a83"],["/posts/3156194925.html","94b97f7af5ed426e5da34153e41d7b4e"],["/posts/3169224211.html","5892564addd00b5a1b023cb12706b9c1"],["/posts/3213899550.html","c6adcec42389d873ab97d7d53f96f838"],["/posts/3259212833.html","94222c6ac39d12b7d87f27cab6880af9"],["/posts/3265658309.html","1ed9884b98b6fe37b905dfae1ad8ffe2"],["/posts/3266130344.html","58fecf32f6801ffda726583ea521c125"],["/posts/3292663995.html","9da53c65839ec66682cd668b0147abb2"],["/posts/3297135020.html","c367696788ac8ffa3360994951a79180"],["/posts/3306641566.html","de544ed49affe91f946e324d2be74c3b"],["/posts/3312011324.html","f90662cb438a908315e21fe4b3a7cadf"],["/posts/336911618.html","96228605eebdde3d3da712ffefb59c95"],["/posts/3402121571.html","663149918843dfaf27165925211d54bc"],["/posts/3405577485.html","f5ec0914abc4cdfa51e1ac66ff4fdd9d"],["/posts/3498516849.html","a1c10102124dd286dc7da51810f2a64e"],["/posts/350679531.html","0921d233bd8e5c47f9c9127375a1b993"],["/posts/3513711414.html","405bd155bd66e3c76148191bee19a347"],["/posts/3523095624.html","7680cf57cdfcef25036bfcfd9ae90f19"],["/posts/3546711884.html","51a9cdf9a6c74d838231c8fc884581e6"],["/posts/362397694.html","dfd4c3f58fd410a3ffc59c5e0c809436"],["/posts/3731385230.html","d5c99d9883eb3ab4c30136bfb25b878b"],["/posts/3772089482.html","5659523c4f52ce392d9bd152e7e9ff3c"],["/posts/386609427.html","dd05bdc27f021d688ade61432c96df74"],["/posts/4044235327.html","80278b74d567c88997fd28d4e0318cd7"],["/posts/4115971639.html","3fae45e97f418c8eb0f497997a2551c2"],["/posts/4130790367.html","2594d8dd02af1c36b2b5161bdba731fb"],["/posts/4131986683.html","798ce33ccf0de16367cc2eb5e5e32d55"],["/posts/4177218757.html","c94e25e41014eecb7d64bcb6e1dbfb32"],["/posts/4192183953.html","c063e6c4841361149d0d54a7130830cf"],["/posts/4223662913.html","b545ab85a7a1d69bc165b8dc6d57a50f"],["/posts/4261103898.html","6884db016f29e75fd3404128aac122e1"],["/posts/4286605504.html","8689b3578c59b6329b0d1bbb4e5f12f4"],["/posts/449089913.html","c0ede3a320a5da2e279839110a1d101e"],["/posts/469711973.html","4d8b2b254082c7d01bd0fae6b73cf1a2"],["/posts/482495853.html","1ac73c5cf8448a3cfbb0f43360d82ba8"],["/posts/488247922.html","80c3d5c2090db61957c1c0293a19bdb7"],["/posts/517302816.html","903dd5fb3396f998e800633b590bd759"],["/posts/570165348.html","03352059e5c87e53ca401d275fadf47d"],["/posts/595890772.html","2fb383238104941c617a9408e837fa2b"],["/posts/67485572.html","63b68b521915d0a95770a5bb141906b7"],["/posts/694347442.html","c1cb6f580104aa71e47bb6e924a38f23"],["/posts/707384687.html","5949bc2576a77e1240238ba3a308aca4"],["/posts/71180092.html","1ab21b92cde25a7ef3ad17fffb67f8eb"],["/posts/716459272.html","b616bcdfde7bdaade2d191618a6f90d0"],["/posts/765481613.html","cd39e0795d556f72eca955e7761bfb0a"],["/posts/778231993.html","1e4eacc93538088dc793e632c359a308"],["/posts/795397410.html","51880131093a9a13a3b2d623aecb05ec"],["/posts/820223701.html","cdb6603b16c31a7dfd8350d069e9023d"],["/posts/830372185.html","8c5537ede03293db6a9c75f60bf442b6"],["/posts/88294277.html","463d1197402d40221e89103f715f9e96"],["/posts/939963535.html","15a967e959123d76b7ec3bdcafe4ec0a"],["/posts/983786067.html","102b0dae2bdddd14d7d14c2ace2f4c8a"],["/sw-register.js","fcbd0b7c533695111890a906b69943c4"],["/tags/C/index.html","39eba47751caf732a0a94b9e5ef855d7"],["/tags/C/page/2/index.html","ecd9e75aff49eef1fcab576998178015"],["/tags/C/page/3/index.html","704aa2b21fa7272b4e30e0ed0d689e1d"],["/tags/C/page/4/index.html","b0cf47915cb9915a33bbca765cad9321"],["/tags/ETL/index.html","21b3174bf6911234b3233bb9e7ee0c11"],["/tags/ElasticSearch/index.html","3ee7f640a23334d75e183be1414d55b4"],["/tags/GUI/index.html","ad262f230d9c24b150b9cafdb3a566e2"],["/tags/HBase/index.html","b1384b3ea8a2c6a7f3231d3d48ffc59c"],["/tags/Hadoop/index.html","09b28b9a2ec53d4b6d69698a8e5965c1"],["/tags/Hadoop/page/2/index.html","e637f436daaa0ae7652dfee18d03cc31"],["/tags/Java/index.html","10551a63341f70e2e3a0bf5463327f95"],["/tags/Java后端/index.html","33569b7244ef056ab9125ad5a221171b"],["/tags/Java后端/page/2/index.html","8cf44522366cddc21e76ddaac83f7d0b"],["/tags/Java基础/index.html","520aadcab0c263f2f5aa923b8e4e065b"],["/tags/Java基础/page/2/index.html","039916a439148529f05089df6c92e66b"],["/tags/Kettle/index.html","94a42e5c4471c82898017875037c4975"],["/tags/Kibana/index.html","f17fa2995261a0e7e804adbddbf300f3"],["/tags/Linux/index.html","ac0ca8529eefd4c9e18c7d2b80092fbf"],["/tags/Linux/page/2/index.html","923d38a798557993e4f5194e5f94e76a"],["/tags/Linux/page/3/index.html","b534f5670373c1fcf86c5620bb22a5f6"],["/tags/Mac/index.html","9e9753409e5b73295c85af34f7b0c6e2"],["/tags/Mac/page/2/index.html","b1876230ff8abbdc38f1d453efc24266"],["/tags/Maven/index.html","3e8d34be7132476a5190e2144e6cb5bc"],["/tags/MySQL/index.html","55c8760d972b0f9afe36e3aad0b9d544"],["/tags/Python/index.html","4ec69a04bb0e10a8279cf1380907ebf8"],["/tags/Redis/index.html","838cc765826bd81eacdc5955b67b13b5"],["/tags/R语言/index.html","a9df1a57f7b1deb5a7e3b2f0df024789"],["/tags/Spark/index.html","09a79e0de06639f2f90c15e85fe6352a"],["/tags/Ubuntu/index.html","2279e406c1e10cc2a8d2f8b0a5b9e0a3"],["/tags/Vue/index.html","60b9912a60f520abf3b8f1ca116d3047"],["/tags/Windows/index.html","3b9f84ab521490cd8c556f25a015b9f3"],["/tags/ZooKeeper/index.html","58410a02fd06b354c0ee631d14585d24"],["/tags/bfs/index.html","5d7d1d6d757e71fb8000c65d706e4d9b"],["/tags/dfs/index.html","ae73f02f579e221724d6f61bab616710"],["/tags/folium/index.html","5ea91e4e4ec4146525ed5420ef883500"],["/tags/git/index.html","90e25b15f1bc86673f98d7f573dd308c"],["/tags/iPad找电子书/index.html","d2a94efaa12b583489c6d0586cf99d9f"],["/tags/index.html","f5826fa1796b87e30fe18b96a4092af7"],["/tags/latex/index.html","7f8ab32b00573c7cd447b6c65a8f3034"],["/tags/中间件/index.html","71575a723b50231ca774264d0db13178"],["/tags/二分查找/index.html","13d6e1a3f28d0970b223c73d4a235bc1"],["/tags/优化类/index.html","cd17b60016348994d305a341b5f257ce"],["/tags/前端/index.html","edc86f2e837084c01bacd7f79afb7102"],["/tags/前缀和与差分/index.html","29a1764b39e3d3002c46411d5d163231"],["/tags/动态规划/index.html","d4b423c8c024f0e2b85d425221317a05"],["/tags/动态规划/page/2/index.html","8d4fb98f5f36efb8689f8aedac65e4e1"],["/tags/博客搭建/index.html","369ec5bc5e019915fa2fb005b8403378"],["/tags/图论/index.html","adcb966d31f0284dc8f9a8fc47467f1b"],["/tags/大数据/index.html","21d6c673764fe3c8e2ad2952ce1f7950"],["/tags/大数据/page/2/index.html","77abe78184a2515010fec10cf53a564f"],["/tags/排序/index.html","e68ad952dd443def34c5e90419857601"],["/tags/操作系统/index.html","255808a5eef2089ba6b3950dd358f051"],["/tags/数学建模/index.html","3d3938aaa0448764edee6423c65ccf6b"],["/tags/数据库/index.html","216fd1e93a29d023f94681f075cd0bd9"],["/tags/数据结构和算法/index.html","268264d95f6fcb6207bce6504fec75f8"],["/tags/数据结构和算法/page/2/index.html","c9f4c11b88c680f485a88ed224b63adb"],["/tags/数据结构和算法/page/3/index.html","6925d8d8734d5b3df9c10781a56ec0e3"],["/tags/数据结构和算法/page/4/index.html","407ab2b0b6fc4b027b1156fcb3300b5b"],["/tags/数据结构和算法/page/5/index.html","2c9191c875b1d55d06519cdc0b615714"],["/tags/数组和字符串/index.html","ef68ea16f7d8a99268997bee23652ef5"],["/tags/数论/index.html","e53e6885ecbe6771483cd35cf30e4063"],["/tags/枚举类/index.html","c7d2a387af9a2835cfb761180be89a4f"],["/tags/栈和队列/index.html","564da1efc9f55700e60687587c0f9203"],["/tags/树论/index.html","502f1a01e583a91379416e621f179e57"],["/tags/测试/index.html","0a0cdad6f0c2654fab9f4f0272edfbe7"],["/tags/环境/index.html","68ceffd79763ed207fd0806c58077cd5"],["/tags/环境变量/index.html","ca7eee4bea3ab18918bd84943602ceb7"],["/tags/绘图/index.html","c14059164c25c42f4f9068e122dbf620"],["/tags/编程工具/index.html","7e1492426b6f2c3e9055934a9fa800e1"],["/tags/编程环境/index.html","b222dbde9e4c39ee1dd6e3133a49c5a3"],["/tags/网络编程/index.html","14f073f58a217e23f7321d4a598f4ef5"],["/tags/英语语法/index.html","e8e35aa913c41609143e7ebf2a406699"],["/tags/计算机操作系统/index.html","78d332b147abdf47b1925e52d0343e53"],["/tags/论文/index.html","9c669f65b26c05cbd85bb0ed2f187bde"],["/tags/资源下载/index.html","edc0d31940d7bf34404a28fefb313cde"],["/tags/链表/index.html","70d5ccc24e35b916a68823c5ab8f2291"],["/tags/集合/index.html","37f15bfddce56c4b98821342e89223f6"],["/tags/集群/index.html","42ae9d91a12a9f271ba6a788efc13876"]];
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
