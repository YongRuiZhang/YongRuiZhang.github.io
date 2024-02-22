/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d73987d0e1fff5000f67beb9899ae7b6"],["/about/index.html","1c389677cfce7cdffb8556ed7fcdf700"],["/archives/2023/01/index.html","a2d99786e333949cc3cfbe532765c467"],["/archives/2023/02/index.html","e95b5739808c8f88dda1566440906658"],["/archives/2023/02/page/2/index.html","0866454b784f450f78874431cfb88059"],["/archives/2023/03/index.html","a7ceb246a0ea072d87bbb66434c725a9"],["/archives/2023/05/index.html","b78e732d6fa85eef7915b237d3809e27"],["/archives/2023/06/index.html","d4887568015b338969a8b568699523fe"],["/archives/2023/09/index.html","d1df7dd8f3b3d0154e88102c92b5b947"],["/archives/2023/11/index.html","fd9ece036f8476e25220ed26e02ffc67"],["/archives/2023/12/index.html","3305539f7cb5a5fea3e20c21b6e1bfd8"],["/archives/2023/index.html","525781e96548ae18c2d3a775a47189da"],["/archives/2023/page/2/index.html","b37f00b491c810e30e883e534fbe0af7"],["/archives/2023/page/3/index.html","98b91f5f806664ad0b235568653d95e3"],["/archives/2023/page/4/index.html","7b40da1fed39cad92cd0aa7adb6f74aa"],["/archives/2024/02/index.html","52cbf85ab2b0722c1b2cfc6284f59076"],["/archives/2024/index.html","2cad6478d7b21aed61d197f6815e8df4"],["/archives/index.html","7009b45079398822d93bab5ef227368b"],["/archives/page/2/index.html","81ced122b36ca651596443fed96c8178"],["/archives/page/3/index.html","5a583cad951a7bfbeefca83bab6d005c"],["/archives/page/4/index.html","71f084e29b00b3658ab6502548866465"],["/baidu_verify_codeva-qQP2iZOMLX.html","e427e71b54844fd4f1267503d5f6b22c"],["/categories/Java/index.html","88bcfe1678febea029d9de4ac7194277"],["/categories/Java/后端/index.html","d4b6333c60a0f071261c24c4c178e0c9"],["/categories/Java/基础/index.html","3026be5bb2e69c411c9bb8900bfa1833"],["/categories/Java/基础/集合/index.html","0408c7016d62b7f39cd158b1afc97990"],["/categories/Python/index.html","cdefbfffdab914145327b7ca852f059e"],["/categories/Python/编程环境/index.html","27606f8257a3dccf66dfec05d32c276c"],["/categories/R语言/index.html","3c5debca57aef3dfa14bff7f58238e3f"],["/categories/R语言/编程环境/index.html","b1959b02898a1598da512a9b908e394e"],["/categories/index.html","2c720ca5c4ae521dbd21c0cc58b3b7c0"],["/categories/中间件/index.html","026aab9186d57b9ced0ef1f6748b58c9"],["/categories/前端/Vue/index.html","773c2d985834c80d837e015fe6dbfb35"],["/categories/前端/index.html","95ea153633b16c31d37a9d45c535a02d"],["/categories/大数据开发/ElasticSearch/index.html","321e496759c5290589a2d317be7a3014"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","88f1394f7956936770a6e94243bed589"],["/categories/大数据开发/HBase/index.html","3e772ddf278a5d054a24126eb072d7c7"],["/categories/大数据开发/HBase/学习笔记/index.html","cc541ebb5512e77f1dcc4d30d578eff4"],["/categories/大数据开发/HBase/环境搭建/index.html","8f884cd6bb226c984a08b55fb7baf5cd"],["/categories/大数据开发/Hadoop/index.html","f70faa706d6ce92165af046419a1f281"],["/categories/大数据开发/Hadoop/技术/index.html","b048cac44f1d983f11f6f9c3fa7c7427"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d0048cc8206755682de60902f9ab17f0"],["/categories/大数据开发/Redis/index.html","81b7c64eaafd5815064b189f8883ce7c"],["/categories/大数据开发/Redis/技术/index.html","ec12562b6f087e6a101f8fce6501723f"],["/categories/大数据开发/Redis/环境搭建/index.html","083de21bed600bf83dcc1ef81938bb7d"],["/categories/大数据开发/Spark/index.html","0bd7e840723cc06eb7dadb71e6114c68"],["/categories/大数据开发/Spark/环境搭建/index.html","a8ad68465684e7a8f6398bb65bbdeef1"],["/categories/大数据开发/Zookeeper/index.html","2cba09f72682829fbea301ccad49618f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","fd69f3660c5345c3fd32ff31f723599f"],["/categories/大数据开发/index.html","19815bc732c2dc9347d8f7b57fa07897"],["/categories/学校课程/index.html","09b5dae57507c81f7d7aac6129b0c35d"],["/categories/学校课程/计算机操作系统/index.html","7c6636fcdd8b5a16c6fb711ff6be34b0"],["/categories/操作系统/Linux/index.html","eb9604c357212ed4385190cf5f156257"],["/categories/操作系统/Mac/index.html","aaed8ad7d5d0590a628b3ee4214f41b3"],["/categories/操作系统/Windows/index.html","e800dea85493d24b159ffb86f982c003"],["/categories/操作系统/index.html","c7477bb1802eb15f46779ac0332849d6"],["/categories/数学建模/index.html","179905a9b8c0419ba6e8eb6b35fc30c9"],["/categories/数学建模/latex/index.html","d93b7eba1c066e0be9a8c50f64fa64ea"],["/categories/数学建模/优化类/index.html","740de2e5f2dd651de71570f44937b6b5"],["/categories/数学建模/优化类/现代优化算法/index.html","14405d0c07f54431cdfa625b6efaa6f1"],["/categories/数学建模/优化类/规划类/index.html","d78f9efbe995a8de700f0ec27c2aeb64"],["/categories/数学建模/绘图/index.html","965b5ab881600d28a92bfc79560b68b4"],["/categories/数据库/MySQL/index.html","3e5f7f0bf72f1a9654a2ea482761826a"],["/categories/数据库/index.html","dc482b730e33a23370719f8a79edea35"],["/categories/数据结构和算法/index.html","5d365847a003fcadf61feb26a4712072"],["/categories/数据结构和算法/page/2/index.html","424216ee8f7d803a0cc63ecc14dbc837"],["/categories/数据结构和算法/基本原理/bfs/index.html","77b562de4f430a12818f39b443a0aa36"],["/categories/数据结构和算法/基本原理/dfs/index.html","6b8a7fb790d2ec5ab96dd24e5802d63b"],["/categories/数据结构和算法/基本原理/index.html","e7fbff4ed95c9785312ee979963410d1"],["/categories/数据结构和算法/基本原理/动态规划/index.html","6bf466f7512924ed4174d957da77b13a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ebabfbf81fc710e191b6bdede2d9f29c"],["/categories/数据结构和算法/基本原理/图论/index.html","712749321a7169cde2aee83c33fbcab4"],["/categories/数据结构和算法/基本原理/字符串/index.html","93d5e987564b87e4985d2b5fa3bb68a5"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","17f3618549e5a4040609a4c8b83c01bb"],["/categories/数据结构和算法/基本原理/数论/index.html","d5ec47359d4d6aec037eb61de66324c9"],["/categories/数据结构和算法/基本原理/树论/index.html","c1628970ab9a8b9fb46173973038e24f"],["/categories/数据结构和算法/基本原理/链表/index.html","1156c00bbb612650770a4d03165730a6"],["/categories/数据结构和算法/算法题/index.html","3fbb100e1e441122b78e5318492b65f6"],["/categories/数据结构和算法/算法题/二分查找/index.html","89d23ca915afba0f65efc5d22b85115d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","cd350855a372d5d64939fb41d72141bd"],["/categories/数据结构和算法/算法题/动态规划/index.html","648c94a221e130d8f8cf2edde20525e9"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2087682710e6e57295d730ca2d58fd8d"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","da7027b26f1f8800833cb7a41a6e0b98"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","ec29b03a965819a73a7ccc605df88b9e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","91cd9dc715b86d69f1fa0c0943a80118"],["/categories/数据结构和算法/算法题/数论/index.html","e2425c097bc9bf65627a1a0e9eac938e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f34b7ad0d219cc42a542c2394094f00f"],["/categories/数据结构和算法/算法题/树论/index.html","568d377713153bac988f550dbb06eb11"],["/categories/杂七杂八/index.html","e0805043a36a8089b0ed3ad0a6475103"],["/categories/杂七杂八/博客搭建/index.html","f10857cc0ecf0b774ebc4352a9926d2e"],["/categories/编程工具下载/index.html","3613707d1028ce44636ecc30ed19ac1f"],["/categories/编程环境/index.html","046753d15180b8967624fe0e201726da"],["/categories/编程环境/大数据/index.html","15867325f661d3db049a7cf7af854170"],["/categories/英语学习/index.html","699472e2ce79f173807f3270b518c1cc"],["/categories/英语学习/英语语法/index.html","fb396088aec6ff15f1da3aca9b39c43d"],["/comments/index.html","803ff15fb5c308c58b4356c5f1e5d68c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","5989aa0ef63914efcfb2b48879b9b7ce"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","2db8789967824cd2ec5a93846635fc69"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4190cba954dbb38e096b9b6cc08c6823"],["/movies/index.html","773f7d934a3b0d784e8d8cba4601db51"],["/music/index.html","da4d7c43fc408026b273e05ad950a2a8"],["/page/2/index.html","6b5b0094d11486fb0543b8483d4a858f"],["/page/3/index.html","b54e7e9572eac8597ed60a14cfac6a52"],["/page/4/index.html","e52acf7051d6c53ad16bf1fb552de1ef"],["/page/5/index.html","e839a0482aa5f5b36f75bd11994c88a2"],["/page/6/index.html","a426cf3e3974049613771e57af989017"],["/posts/1021360842.html","75eeeef28487043308e2aa1e9f6b501e"],["/posts/1120620192.html","edfb0e93e609e8806a3a3d7f3d2cd9d0"],["/posts/1141628095.html","7987a7bd5a33f06784a2d024ca3a7160"],["/posts/1168613674.html","df1f9b2531c5315f5145b60f82072418"],["/posts/1219920510.html","4c64ad48eca61ffd6d6ce43977202f6b"],["/posts/1222166338.html","c534df3dc78d4ec753ab93a90be992dc"],["/posts/1259097482.html","63996706cac1aba4c2b9bca43d2a1402"],["/posts/1271036369.html","2c757cf828ed739be02c75aee0a4de0d"],["/posts/1312847445.html","039bcdc4999c8b5d96c54ff5b0075bf9"],["/posts/135355774.html","0292a4a3acc01d3ded31776b4fa6914d"],["/posts/1375344716.html","a88bc49e2036ecd99fa377d4302a8e99"],["/posts/1388991698.html","cfd39a310340ec40efb4422955327270"],["/posts/1410315814.html","5cbce05567a0cf278833887532988321"],["/posts/1452790229.html","2c020fbd8d000bfb8ead96bb0268fbc9"],["/posts/1470079884.html","bf80e2d19a87ffb9d1f2c7e8bd493456"],["/posts/1470079885.html","ce709e7cc8f47f1a58bd80cbbac95369"],["/posts/1470079886.html","90d491beae67f10fadb0fca3d7409cb7"],["/posts/1470079887.html","3b91db870bfd6e990bb5925883c069eb"],["/posts/1498536549.html","f2431ea1ac9532fa3d8280f9ef18ae37"],["/posts/1539568593.html","0cfe3f2751fe7ee0db2621c4db455ad1"],["/posts/1547067935.html","c11ccaa15f28d4d532b4dbce7d640c05"],["/posts/1557866301.html","7150b9ffc4120727cfe80b1579eda7c2"],["/posts/1571776361.html","696871f649e722abac8498649398ea0a"],["/posts/1605124548.html","0f4ef1b9802a5ae48632f1e590c627f8"],["/posts/1633036852.html","8087c34bea5cbfd31a0db0255b050d23"],["/posts/1674202625.html","088416138b4752593fdcff8b10c54fef"],["/posts/1765123828.html","29d8db86ce52a6923deac166e0018ea5"],["/posts/1767336200.html","f9a020e5d44fc99131ad0b0eed14144f"],["/posts/1776114197.html","e2c88fbd4dc3a44afa5d5ef177374eaa"],["/posts/1817748743.html","8a73a35f925dda4b34a2a5bbab1bff47"],["/posts/1925125395.html","4c8d543eb44fb070aaf35140b5789caf"],["/posts/1966191251.html","5bba522f8612c84f569682834fbec3ef"],["/posts/1987617322.html","49f604448781937ff52eaab2023b95e6"],["/posts/1999788039.html","7f405faf39c6f2999ea43bcb3ee2e3eb"],["/posts/2075104059.html","70a0cf5f91b54d794248571ba2566886"],["/posts/2087796737.html","616bbdfea3cc3f07047d023048a070d8"],["/posts/2106547339.html","5de0353f0b023f2bbc1757331ec6fae5"],["/posts/2207806286.html","094a29a79a3e9a0b61cfe1a710a3458e"],["/posts/2225903441.html","ac61ba0182e4287a380287a0969dc8d1"],["/posts/2265610284.html","3327e9789a9b4ac450e097032f904e9f"],["/posts/2281352001.html","7df4b9e280346d2e1d25de3617123b04"],["/posts/2364755265.html","1da750b6a33e966604e331c60d591c1e"],["/posts/2414116852.html","6ef4b8c8f50b2cb40805ed8284cc42bd"],["/posts/2421785022.html","19ce955a8cfe24d5d26e635c25a21091"],["/posts/2482902029.html","744e34f4854a1de2fbbd5865fdfdefab"],["/posts/2495386210.html","b8e5b6cf9116f66cc65f959e748a99ff"],["/posts/2516528882.html","83689797d8500cafeabd9ce89c1bcb34"],["/posts/2526659543.html","713c8583f1ef7fdb83dc7f92f4f08f7f"],["/posts/2529807823.html","4253b998f288ad5262c7f8c1b00915de"],["/posts/2596601004.html","a367dd72a39c612253c582d6d0a4cba6"],["/posts/2697614349.html","bc871315cf39b856a87796ec2c2013b4"],["/posts/2742438348.html","325d7dd59e852d2ad90de7867edc66ea"],["/posts/2768249503.html","dd46b9bf80f77cc31485ac53bd624ec7"],["/posts/2864584994.html","9b4d849bcd9a597bc2545a1b4a3dcae1"],["/posts/2888309600.html","99f36d6f8188d2db540ef41abbcd4f48"],["/posts/2891591958.html","0844f93a47042a7b71c6484eb3e2ef5d"],["/posts/2909934084.html","aec1a763be50edd9cf9cbddc4478a151"],["/posts/2920256992.html","0bbbe9bf14d67d04bc34a55b58008da0"],["/posts/2959474469.html","08b7001d5a4057d140377d4db05710a6"],["/posts/3005926051.html","154abe630f3c97cc1ececcab0922cb8e"],["/posts/309775400.html","b8e4717c9ce90dab3011247a67517b5d"],["/posts/3156194925.html","9e94e321eb2775dabc7241fc75937dbe"],["/posts/3169224211.html","60932f0f9f3dec8e33311097d1dbe243"],["/posts/3213899550.html","c019f80e537ed9baf4d17c27371db62c"],["/posts/3259212833.html","124c67eff355c4c9d86f31e5d5257186"],["/posts/3266130344.html","92fd04aa9072e5a3eb884eb4b15a73c0"],["/posts/3292663995.html","7e6e28f4677f4a5a4e65e58df3dbd212"],["/posts/3297135020.html","df128517b31b28f68cd47d6ba3c0d5b5"],["/posts/3306641566.html","d781f9cdc6021330fe3b190afd794706"],["/posts/3312011324.html","b21b89d317a57acd22dcb6f8768c5f9b"],["/posts/336911618.html","2ce8db6eda1952969fc3dc2999691663"],["/posts/3402121571.html","f9e709e4a2d435e48a4ba2f8923f0a9d"],["/posts/3405577485.html","ae5c745d5ccaa8795abb86b2f9568e4d"],["/posts/3498516849.html","ec67ab687c3304c4faaee46a341e7512"],["/posts/3513711414.html","22622b3e1cb26e50f36b85e5265bd5e9"],["/posts/3523095624.html","08d5f9c941a88acb73eff22da9ac2244"],["/posts/3546711884.html","d0c8ae9f32ea552da4939cbd3d369864"],["/posts/3731385230.html","d97cdb5adc65b62b1dd486b7ca67b3ec"],["/posts/3772089482.html","7529a5530bfc01f50add97c9004bc026"],["/posts/386609427.html","29fe9a76e3f40b11751f800cf3fcc414"],["/posts/4044235327.html","5982b246cd341423fcf94760f42acccc"],["/posts/4115971639.html","c7f847c1c4fec11661f3088a57e2b1f8"],["/posts/4130790367.html","4493c8f2f177bbfc1e739efbad55a608"],["/posts/4131986683.html","dc2291be905e427a6454d87e7c7e8c4f"],["/posts/4177218757.html","9fb774f3ec71fea5c2dc5fd312f9a7e7"],["/posts/4192183953.html","c49c2b8dec00ca651bc0b7dbfdbea6e5"],["/posts/4261103898.html","ffede8f24f503239ed8275d4b25077d0"],["/posts/469711973.html","adefd48cb070e82ca95becd15f6dc721"],["/posts/482495853.html","3a1e65b98d6083a6994a47514dc2b1c3"],["/posts/488247922.html","73dd17ee2a1e5679b12b5fcedc65dc50"],["/posts/517302816.html","96a564d72133ec2d9f74e8cf6e1a25f9"],["/posts/570165348.html","798270c33211a89b29bef2ee49925c7f"],["/posts/595890772.html","9940e77d5f2eacbe9f2eab6c6a4bf5bb"],["/posts/67485572.html","8f9d4f244ddd080892a61d2b8b6dcb89"],["/posts/694347442.html","172ef596e3b946769dbfeb654f6b3910"],["/posts/707384687.html","fe8661d8ce6108bdb7dea7583a9bfcc9"],["/posts/71180092.html","d76c3e097d6bf84613cb7a4cd960224f"],["/posts/716459272.html","fbf98259008e9f1e32b22f414d222097"],["/posts/765481613.html","28c629d8f992604b94b93c99809adfc8"],["/posts/778231993.html","dce813d9d4abd12d9979151c0956e3eb"],["/posts/795397410.html","ae75245bcd787de75431df4f96a848dd"],["/posts/820223701.html","168d618e34e7bb2c6f5d1d16d0e57209"],["/posts/830372185.html","cf8db822e670c6aa07c91c726d51af02"],["/posts/88294277.html","33448a41ee18680e9f8e8040fdb52de2"],["/posts/939963535.html","aab07e40afa5e64bcd28d1e106d711ad"],["/posts/983786067.html","47ade11ac04eeaa6d46ac266e006a3c2"],["/sw-register.js","db9606fd4f61250047bcd5709de82f74"],["/tags/C/index.html","b470433e298ca9b9bb510a39c9dcd63d"],["/tags/C/page/2/index.html","84cb275d6d8a6f1a450efb5d1d0c0d2a"],["/tags/C/page/3/index.html","796b1225d28aad8dc61548767c684811"],["/tags/C/page/4/index.html","4dbe2db02f4b6793c1da96e6c9754e7e"],["/tags/ETL/index.html","8141fa49ea5444c4ec7505401f137b7c"],["/tags/ElasticSearch/index.html","8216ce2cf0667fef11fb4b7b113f4ce8"],["/tags/GUI/index.html","3e3ebb72bfb256adec7d35fb377a9233"],["/tags/HBase/index.html","1d62f84b87743d35f5eeb4626b18307b"],["/tags/Hadoop/index.html","0fdc3990f82637969fa04a9eab4c712f"],["/tags/Hadoop/page/2/index.html","482fd18883ff17801ca9a1b99f94ce3a"],["/tags/Java/index.html","a61f518fe6ac55594ce40ec4f30c3849"],["/tags/Java后端/index.html","53fdbfa0ac0cd71ebc50d29b28cb24f7"],["/tags/Java后端/page/2/index.html","1ed60571015d387e9c6e25a6f5a4868f"],["/tags/Java基础/index.html","d02f52a2e48ca434cfe80b6f709f3dd3"],["/tags/Java基础/page/2/index.html","9cf21a1935ded6aa8397a9a6aa941ea0"],["/tags/Kettle/index.html","8aa05eed96245b5deb52ef2a234cd171"],["/tags/Kibana/index.html","aabd8864cea5ad5fa26a48502c18d4b9"],["/tags/Linux/index.html","00e319b71b9c4f3d6912296c6fb124f6"],["/tags/Linux/page/2/index.html","b0da34786fda4dae22760b0bf2af84fb"],["/tags/Linux/page/3/index.html","538909af0ac1d5fd4360b5a9634e2036"],["/tags/Mac/index.html","aa88b4e6f8e530255b23ca3e5353f7e4"],["/tags/Mac/page/2/index.html","b22d16c98200d6b3228492aff82e2ce1"],["/tags/Maven/index.html","b88b9448db9e446ca42571b2d3cbb084"],["/tags/MySQL/index.html","d685c67a189eed514690fe18cceec9d7"],["/tags/Python/index.html","56e1d3a29ca53aab65139c68b0eded8b"],["/tags/Redis/index.html","7294f9fb5f46df5eef2f61a14e17cdeb"],["/tags/R语言/index.html","9897c7fb9d4e8f97a959652c645dfa86"],["/tags/Spark/index.html","797878e6c89fd3334e849076491c6494"],["/tags/Ubuntu/index.html","6369f041ee30ae06c8f34706984ec20c"],["/tags/Vue/index.html","b55c6379af7241c7d1be74be0d81a024"],["/tags/Windows/index.html","1609b1b651f9d3a2c12e14371d3d8d23"],["/tags/ZooKeeper/index.html","2e7dc2454cd06e2d7b6d3ef0acc88c9e"],["/tags/bfs/index.html","aaaa9b7acbb7e57429ffe3f7eea70f9d"],["/tags/dfs/index.html","7f771eefe44aace2dc41ae79f76e948a"],["/tags/folium/index.html","a9cec5ac02c410832c8290820975ccbf"],["/tags/git/index.html","9d529ddb083d230a9be56bc8c6da13b0"],["/tags/index.html","bac78218ed3342d10c809c8dababbfa7"],["/tags/latex/index.html","4065ab812545df543c113de2ba55e1ea"],["/tags/中间件/index.html","e2a120055f5d644d8d39c6ac3987422f"],["/tags/二分查找/index.html","22e5c84deac6e3d53f514ec824ac4e6d"],["/tags/优化类/index.html","bcbe6733a3bcc6df7fd85cc7a1b0d38c"],["/tags/前端/index.html","9a064fbd01f29d5fce4d2e0daab482d0"],["/tags/前缀和与差分/index.html","25683655b8cfd71627edacf2f8814630"],["/tags/动态规划/index.html","1e61a00b1d7e744387a4224799ed4fea"],["/tags/动态规划/page/2/index.html","cb7d12a8179628cfcfd5ad950dc57264"],["/tags/博客搭建/index.html","06c00582af7d29a1ee3f1a5b42fce8ad"],["/tags/图论/index.html","9b80bd620471d1e1818046fb02cef9e9"],["/tags/大数据/index.html","f68d8bec3920018fa72ce96accc28429"],["/tags/大数据/page/2/index.html","dfad302d57980db1e96450458cd9bb1c"],["/tags/操作系统/index.html","2c022cd82740fdd0bd6a117367eb6cfe"],["/tags/数学建模/index.html","7c976b930872dd40c0015cc43b77a6b4"],["/tags/数据库/index.html","2588df33c233099e4987395db36c2140"],["/tags/数据结构和算法/index.html","13fbba29cd261ed78ffeabae28ab14be"],["/tags/数据结构和算法/page/2/index.html","d6a114c2a58983b351e514b461cf98c7"],["/tags/数据结构和算法/page/3/index.html","54d053ad463575ca93df2830f871f050"],["/tags/数据结构和算法/page/4/index.html","c4e4ea771f80d8cdc1d4c2db3e560669"],["/tags/数组和字符串/index.html","f2e349bea9116381d0b480b1db835f60"],["/tags/数论/index.html","3d68b6e579adfa5e67db96d006ad2769"],["/tags/枚举类/index.html","52b9d366de07657fc4abb9aabfc9209b"],["/tags/栈和队列/index.html","f4787e62043476158b2e35e1de091b9a"],["/tags/树论/index.html","f3722f8f4428edb345f07783a4d4935a"],["/tags/测试/index.html","d5426dfcb88ab9e2930c4f7afd07b493"],["/tags/环境/index.html","a0ef501d5c4209fdfc81513ff4b9fb0e"],["/tags/环境变量/index.html","365b37c231de57659b9485c3e73499c9"],["/tags/绘图/index.html","74bdafcc50b6fb210622ac36bebe2f8f"],["/tags/编程工具/index.html","bc261d6a29bc0859ab2d442a3a71c594"],["/tags/编程环境/index.html","e67681913e8743dec9824738ff3a3dba"],["/tags/网络编程/index.html","3e54f306cc035418d93c7b7988a892fb"],["/tags/英语语法/index.html","99852ec7e5a88b7b63de7283832d6810"],["/tags/计算机操作系统/index.html","c61ea458095ac9a7b7a66b6c7d0337aa"],["/tags/论文/index.html","d3571f8cf5032a52ff72cc3f5246977d"],["/tags/资源下载/index.html","6dde07d480ecb3da6a69eaf90bf3fdc3"],["/tags/链表/index.html","4f9899fb1ec4c9250dfddbaae1a8308b"],["/tags/集合/index.html","e30acdd047aaa969e1711d34165e529a"],["/tags/集群/index.html","c764e3f5b02bd6999e98a67f82ac40a6"]];
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
