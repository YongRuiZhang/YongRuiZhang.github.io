/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","fb093a24282433cd8aa48f87aa0192fe"],["/about/index.html","1c389677cfce7cdffb8556ed7fcdf700"],["/archives/2023/01/index.html","b8727f9cb72b0130251d455ee46aeb99"],["/archives/2023/02/index.html","ff6b6f6879c895a8bef0b7e6a26b52f9"],["/archives/2023/02/page/2/index.html","a078b7964b06991151801cd8a8cb9363"],["/archives/2023/03/index.html","fe026eb0ffcdcf43cfa502a54667fed2"],["/archives/2023/05/index.html","27b2d3ca8888f1b1fd9a4ee531842724"],["/archives/2023/06/index.html","8032f4f2958306a552888ca4f9076ec0"],["/archives/2023/09/index.html","cdd9bf4c7a216e474b98256709b095cf"],["/archives/2023/11/index.html","d8a60098e43fb3e83fe86d1164b248f0"],["/archives/2023/12/index.html","9e55bd9644433c5375500543e0da87b2"],["/archives/2023/index.html","39c5c36b5b5b5bb2b0c859f3659760bf"],["/archives/2023/page/2/index.html","a71bb565c66e68970accbe353e50ee6a"],["/archives/2023/page/3/index.html","c3f6a24921b64ee609a6c6cbcb0c96b3"],["/archives/2023/page/4/index.html","4801aca3245721c95295c2d4e23ee4b5"],["/archives/2024/02/index.html","6c4551ff3e04fad3c659d99cd9683b22"],["/archives/2024/index.html","7ad55165ef74f698d359671200e6f2cc"],["/archives/index.html","9cd820fcaa65c2c11b7b60dea530125b"],["/archives/page/2/index.html","fdee8e3313ad45a779b0dde53e125d98"],["/archives/page/3/index.html","50c1feae1020a80e908dd7cfb2c2676c"],["/archives/page/4/index.html","0828523b5d444855f72ba383e39c0656"],["/baidu_verify_codeva-qQP2iZOMLX.html","86107cd80928466f91931039f39b9bb2"],["/categories/Java/index.html","0cc3712347be03b409948844499c4cb7"],["/categories/Java/后端/index.html","cfcb21ab2a9174d2e8133914cf650a2b"],["/categories/Java/基础/index.html","b6d567659b4244a86617dd5fcb6e9c8e"],["/categories/Java/基础/集合/index.html","1c5596df82a90e0114a817a377d50e6b"],["/categories/Python/index.html","099f90b8031fcdd488ab49cf81c0f7da"],["/categories/Python/编程环境/index.html","173c840caba8ad66801c1c62c796e0fb"],["/categories/R语言/index.html","99467359a9a98d19e3638ae70df1d4f5"],["/categories/R语言/编程环境/index.html","a512f76bc88c47906ad78aa46b7f704e"],["/categories/index.html","2c720ca5c4ae521dbd21c0cc58b3b7c0"],["/categories/中间件/index.html","6fd876858a01b9f9346d4884973fa14c"],["/categories/前端/Vue/index.html","5779304ba30d3f1ad8b65ecf53ee4468"],["/categories/前端/index.html","8d3ecbdd9f922b6cb28d48e0c797b281"],["/categories/大数据开发/ElasticSearch/index.html","d4cc578da34e17d0b741e4beacc8db44"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","df51f8d56c719e0d0ad9d55d37c7c2e9"],["/categories/大数据开发/HBase/index.html","c94bed350f109e16af91834883332909"],["/categories/大数据开发/HBase/学习笔记/index.html","e523855ccf572b3de97779f58b6b4c23"],["/categories/大数据开发/HBase/环境搭建/index.html","bac40fac2314e21bf153d34da5d55d55"],["/categories/大数据开发/Hadoop/index.html","00f9f9a821783cb68aa7d3421d2f05ef"],["/categories/大数据开发/Hadoop/技术/index.html","2f7bb28352512a02fc5153ca95c28b7f"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b956be20120f38a8c0fd7ca506873be8"],["/categories/大数据开发/Redis/index.html","e3d84e6b6d8016a7c938feaf21caf659"],["/categories/大数据开发/Redis/技术/index.html","4dc37980f39bed60bb68869e68d23b0f"],["/categories/大数据开发/Redis/环境搭建/index.html","f2ffb72ff45b5be86d9b09fd96b96fc8"],["/categories/大数据开发/Spark/index.html","b4dcf1e012cd88420e367e4f14517870"],["/categories/大数据开发/Spark/环境搭建/index.html","85aceba5c5375c9c33db060c08825ec8"],["/categories/大数据开发/Zookeeper/index.html","a53154690694917391eff3a28b2e2101"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","2a95434b40f92dcfb0b889a4c5b572cf"],["/categories/大数据开发/index.html","bd528b21acd6a6f659ebf479513e7756"],["/categories/学校课程/index.html","21f47f02ad9969b388772df2444e3ccc"],["/categories/学校课程/计算机操作系统/index.html","7df02ff5e56fba657c634341b6d46bd8"],["/categories/操作系统/Linux/index.html","c498905f6f16c7b8e16ccfa9a7026095"],["/categories/操作系统/Mac/index.html","016a98e0eb1e412926098b9069b6cd80"],["/categories/操作系统/Windows/index.html","41f8ba6c1a9a8bb2840fe71d19c96134"],["/categories/操作系统/index.html","20a787cad157b092fd7d2629981e0a10"],["/categories/数学建模/index.html","a2bc5f56c3ba495af8125055e9600d4e"],["/categories/数学建模/latex/index.html","dd9b4167252cdd058553cefa481612d9"],["/categories/数学建模/优化类/index.html","eb97150b2e266a9ac9f6fd71145fecd2"],["/categories/数学建模/优化类/现代优化算法/index.html","59e02803893c9174f5a575dfde0bf54f"],["/categories/数学建模/优化类/规划类/index.html","63733236313c902c5ca182baa8143a55"],["/categories/数学建模/绘图/index.html","f6bd2538a0d97dcb332f23f1e96b7a88"],["/categories/数据库/MySQL/index.html","62135e223d8744263fc872c94912039e"],["/categories/数据库/index.html","81055502bb1cd7c41cbf179556ad019c"],["/categories/数据结构和算法/index.html","bd3410bccf940da3cd39bf2e20195559"],["/categories/数据结构和算法/page/2/index.html","f7a0d13274972ba76b0752e08b924c3b"],["/categories/数据结构和算法/基本原理/bfs/index.html","bd2d064d711cbbd01223b6be15469c03"],["/categories/数据结构和算法/基本原理/dfs/index.html","f93e4ef638f47fa91f69f89559756666"],["/categories/数据结构和算法/基本原理/index.html","d424a89f9d871eef46b4040e9daa9e2f"],["/categories/数据结构和算法/基本原理/动态规划/index.html","dddf056b46ea7da2815211aa35d9b530"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","72a8e6a3738cd9613b824f395d30e27b"],["/categories/数据结构和算法/基本原理/图论/index.html","7b63765df51f76ec00e7b186ba3590ec"],["/categories/数据结构和算法/基本原理/字符串/index.html","e76a20be992826f9bc50ebf554037e8f"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","e66e9522c445435ecd094e1fcffb2bc4"],["/categories/数据结构和算法/基本原理/数论/index.html","553a1d17c0d46b064727ca089debf5b1"],["/categories/数据结构和算法/基本原理/树论/index.html","f1acdcae95ae8401f9ffb48909f22ac4"],["/categories/数据结构和算法/基本原理/链表/index.html","be3e4b76ead07cd5f7bf946f497c8dd9"],["/categories/数据结构和算法/算法题/index.html","960ed97fa6cd0a240395680cea1946d4"],["/categories/数据结构和算法/算法题/二分查找/index.html","5f309cb23e4b820fc581f4171d9e46b5"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c19e68f60da6026f34d813a5765de2c0"],["/categories/数据结构和算法/算法题/动态规划/index.html","4e6231bbea05c07802ce5fa4f9d28bb5"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","62e97383b7cd02358f81b389f7d3b831"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","c9a9a522424ac4bc1741914e68f9e913"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","28b8280956726a427709ab7923c2260b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e2ad4e8748cac6771e37f05a9de2602a"],["/categories/数据结构和算法/算法题/数论/index.html","71f4c4f08bad7e6d25292dc02e7bc41e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b51cb4ce397782a9c2ee7d2fb041afb3"],["/categories/数据结构和算法/算法题/树论/index.html","be2ca114932da98fb5fc1c5daba73b44"],["/categories/杂七杂八/index.html","be5306836739542ce8b9dc1b48ffac87"],["/categories/杂七杂八/博客搭建/index.html","6d82f885c7946fc4c4a364ad6ea7490b"],["/categories/编程工具下载/index.html","9342b2ddbb0e11779f569a085e815185"],["/categories/编程环境/index.html","8fcac78d8880cdcb95b50fe381901ec2"],["/categories/编程环境/大数据/index.html","33ae0f223c4a79fd147d1fe2aae89824"],["/categories/英语学习/index.html","c75dbd78f59caaee6b0bda2b9026d4ba"],["/categories/英语学习/英语语法/index.html","7aa5051cb703beb8aae8f30d50204575"],["/comments/index.html","bc033f480cd3044ec0426162b3dc95d6"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","7921d3f46637be0ccc90a7794de5863e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","49528e0de54c275f6f7e3949ef36011b"],["/movies/index.html","52caffd56a8d426d87629de3c01b9a6f"],["/music/index.html","bb9892101965a5cf3dfd5cd45cfc5af7"],["/page/2/index.html","1e2ca00e2d1a040d387ea98490868580"],["/page/3/index.html","ca6dc5bea8bbe081b1ae013e195dd90f"],["/page/4/index.html","95311de1f1cfbf2dc8e13d5b87c8afe9"],["/page/5/index.html","781fd1029748bb86f1f62fb34654d864"],["/page/6/index.html","ccc7c4931d802747ff3e7a608e5addff"],["/posts/1021360842.html","75eeeef28487043308e2aa1e9f6b501e"],["/posts/1120620192.html","edfb0e93e609e8806a3a3d7f3d2cd9d0"],["/posts/1141628095.html","7987a7bd5a33f06784a2d024ca3a7160"],["/posts/1168613674.html","df1f9b2531c5315f5145b60f82072418"],["/posts/1219920510.html","4c64ad48eca61ffd6d6ce43977202f6b"],["/posts/1222166338.html","c534df3dc78d4ec753ab93a90be992dc"],["/posts/1259097482.html","63996706cac1aba4c2b9bca43d2a1402"],["/posts/1271036369.html","2c757cf828ed739be02c75aee0a4de0d"],["/posts/1312847445.html","039bcdc4999c8b5d96c54ff5b0075bf9"],["/posts/135355774.html","0292a4a3acc01d3ded31776b4fa6914d"],["/posts/1375344716.html","a88bc49e2036ecd99fa377d4302a8e99"],["/posts/1388991698.html","cfd39a310340ec40efb4422955327270"],["/posts/1410315814.html","5cbce05567a0cf278833887532988321"],["/posts/1452790229.html","2c020fbd8d000bfb8ead96bb0268fbc9"],["/posts/1470079884.html","bf80e2d19a87ffb9d1f2c7e8bd493456"],["/posts/1470079885.html","ce709e7cc8f47f1a58bd80cbbac95369"],["/posts/1470079886.html","90d491beae67f10fadb0fca3d7409cb7"],["/posts/1470079887.html","3b91db870bfd6e990bb5925883c069eb"],["/posts/1498536549.html","f2431ea1ac9532fa3d8280f9ef18ae37"],["/posts/1539568593.html","0cfe3f2751fe7ee0db2621c4db455ad1"],["/posts/1547067935.html","c11ccaa15f28d4d532b4dbce7d640c05"],["/posts/1557866301.html","7150b9ffc4120727cfe80b1579eda7c2"],["/posts/1571776361.html","696871f649e722abac8498649398ea0a"],["/posts/1605124548.html","0f4ef1b9802a5ae48632f1e590c627f8"],["/posts/1633036852.html","8087c34bea5cbfd31a0db0255b050d23"],["/posts/1674202625.html","088416138b4752593fdcff8b10c54fef"],["/posts/1765123828.html","29d8db86ce52a6923deac166e0018ea5"],["/posts/1767336200.html","f9a020e5d44fc99131ad0b0eed14144f"],["/posts/1776114197.html","e2c88fbd4dc3a44afa5d5ef177374eaa"],["/posts/1817748743.html","8a73a35f925dda4b34a2a5bbab1bff47"],["/posts/1925125395.html","4c8d543eb44fb070aaf35140b5789caf"],["/posts/1966191251.html","5bba522f8612c84f569682834fbec3ef"],["/posts/1987617322.html","49f604448781937ff52eaab2023b95e6"],["/posts/1999788039.html","7f405faf39c6f2999ea43bcb3ee2e3eb"],["/posts/2075104059.html","70a0cf5f91b54d794248571ba2566886"],["/posts/2087796737.html","616bbdfea3cc3f07047d023048a070d8"],["/posts/2106547339.html","5de0353f0b023f2bbc1757331ec6fae5"],["/posts/2207806286.html","094a29a79a3e9a0b61cfe1a710a3458e"],["/posts/2225903441.html","ac61ba0182e4287a380287a0969dc8d1"],["/posts/2265610284.html","3327e9789a9b4ac450e097032f904e9f"],["/posts/2281352001.html","7df4b9e280346d2e1d25de3617123b04"],["/posts/2364755265.html","1da750b6a33e966604e331c60d591c1e"],["/posts/2414116852.html","6ef4b8c8f50b2cb40805ed8284cc42bd"],["/posts/2421785022.html","19ce955a8cfe24d5d26e635c25a21091"],["/posts/2482902029.html","744e34f4854a1de2fbbd5865fdfdefab"],["/posts/2495386210.html","b8e5b6cf9116f66cc65f959e748a99ff"],["/posts/2516528882.html","83689797d8500cafeabd9ce89c1bcb34"],["/posts/2526659543.html","713c8583f1ef7fdb83dc7f92f4f08f7f"],["/posts/2529807823.html","4253b998f288ad5262c7f8c1b00915de"],["/posts/2596601004.html","a367dd72a39c612253c582d6d0a4cba6"],["/posts/2697614349.html","bc871315cf39b856a87796ec2c2013b4"],["/posts/2742438348.html","325d7dd59e852d2ad90de7867edc66ea"],["/posts/2768249503.html","dd46b9bf80f77cc31485ac53bd624ec7"],["/posts/2864584994.html","9b4d849bcd9a597bc2545a1b4a3dcae1"],["/posts/2888309600.html","99f36d6f8188d2db540ef41abbcd4f48"],["/posts/2891591958.html","0844f93a47042a7b71c6484eb3e2ef5d"],["/posts/2909934084.html","aec1a763be50edd9cf9cbddc4478a151"],["/posts/2920256992.html","0bbbe9bf14d67d04bc34a55b58008da0"],["/posts/2959474469.html","08b7001d5a4057d140377d4db05710a6"],["/posts/3005926051.html","154abe630f3c97cc1ececcab0922cb8e"],["/posts/309775400.html","b8e4717c9ce90dab3011247a67517b5d"],["/posts/3156194925.html","9e94e321eb2775dabc7241fc75937dbe"],["/posts/3169224211.html","60932f0f9f3dec8e33311097d1dbe243"],["/posts/3213899550.html","c019f80e537ed9baf4d17c27371db62c"],["/posts/3259212833.html","124c67eff355c4c9d86f31e5d5257186"],["/posts/3266130344.html","92fd04aa9072e5a3eb884eb4b15a73c0"],["/posts/3292663995.html","7e6e28f4677f4a5a4e65e58df3dbd212"],["/posts/3297135020.html","df128517b31b28f68cd47d6ba3c0d5b5"],["/posts/3306641566.html","d781f9cdc6021330fe3b190afd794706"],["/posts/3312011324.html","b21b89d317a57acd22dcb6f8768c5f9b"],["/posts/336911618.html","2ce8db6eda1952969fc3dc2999691663"],["/posts/3402121571.html","f9e709e4a2d435e48a4ba2f8923f0a9d"],["/posts/3405577485.html","ae5c745d5ccaa8795abb86b2f9568e4d"],["/posts/3498516849.html","ec67ab687c3304c4faaee46a341e7512"],["/posts/3513711414.html","22622b3e1cb26e50f36b85e5265bd5e9"],["/posts/3523095624.html","08d5f9c941a88acb73eff22da9ac2244"],["/posts/3546711884.html","d0c8ae9f32ea552da4939cbd3d369864"],["/posts/3731385230.html","d97cdb5adc65b62b1dd486b7ca67b3ec"],["/posts/3772089482.html","7529a5530bfc01f50add97c9004bc026"],["/posts/386609427.html","29fe9a76e3f40b11751f800cf3fcc414"],["/posts/4044235327.html","5982b246cd341423fcf94760f42acccc"],["/posts/4115971639.html","c7f847c1c4fec11661f3088a57e2b1f8"],["/posts/4130790367.html","4493c8f2f177bbfc1e739efbad55a608"],["/posts/4131986683.html","dc2291be905e427a6454d87e7c7e8c4f"],["/posts/4177218757.html","9fb774f3ec71fea5c2dc5fd312f9a7e7"],["/posts/4192183953.html","c49c2b8dec00ca651bc0b7dbfdbea6e5"],["/posts/4261103898.html","ffede8f24f503239ed8275d4b25077d0"],["/posts/469711973.html","adefd48cb070e82ca95becd15f6dc721"],["/posts/482495853.html","3a1e65b98d6083a6994a47514dc2b1c3"],["/posts/488247922.html","73dd17ee2a1e5679b12b5fcedc65dc50"],["/posts/517302816.html","96a564d72133ec2d9f74e8cf6e1a25f9"],["/posts/570165348.html","798270c33211a89b29bef2ee49925c7f"],["/posts/595890772.html","9940e77d5f2eacbe9f2eab6c6a4bf5bb"],["/posts/67485572.html","8f9d4f244ddd080892a61d2b8b6dcb89"],["/posts/694347442.html","172ef596e3b946769dbfeb654f6b3910"],["/posts/707384687.html","fe8661d8ce6108bdb7dea7583a9bfcc9"],["/posts/71180092.html","d76c3e097d6bf84613cb7a4cd960224f"],["/posts/716459272.html","fbf98259008e9f1e32b22f414d222097"],["/posts/765481613.html","28c629d8f992604b94b93c99809adfc8"],["/posts/778231993.html","dce813d9d4abd12d9979151c0956e3eb"],["/posts/795397410.html","ae75245bcd787de75431df4f96a848dd"],["/posts/820223701.html","168d618e34e7bb2c6f5d1d16d0e57209"],["/posts/830372185.html","cf8db822e670c6aa07c91c726d51af02"],["/posts/88294277.html","33448a41ee18680e9f8e8040fdb52de2"],["/posts/939963535.html","aab07e40afa5e64bcd28d1e106d711ad"],["/posts/983786067.html","47ade11ac04eeaa6d46ac266e006a3c2"],["/sw-register.js","fc70d9e3de75b76ca9ed6ebe7edee25d"],["/tags/C/index.html","fbcd2caf8f78a60c21bbdcdfebde536e"],["/tags/C/page/2/index.html","d6cc9fc58723b280d7c8a92b167d4c0f"],["/tags/C/page/3/index.html","363062e947323159f957f850cfad1eea"],["/tags/C/page/4/index.html","4a2015766751c0cc94733de5a72c6cb5"],["/tags/ETL/index.html","94e724cfe1a33a2859cb4cc7d8afa153"],["/tags/ElasticSearch/index.html","fe4b042e3647ecb6ac6bb33507eb97d9"],["/tags/GUI/index.html","11926c7e3489cea61674ad9781dc2cc9"],["/tags/HBase/index.html","f9023cd615b7a752b50cd7b2b6c00612"],["/tags/Hadoop/index.html","a2feaeb8b112bf189c66f18dfdb0f536"],["/tags/Hadoop/page/2/index.html","2033069d90febe2f59ee0a5fb80588f7"],["/tags/Java/index.html","bfa7cc8978676725932409e36c245e53"],["/tags/Java后端/index.html","0c6a04d712532cfd4ccaf41eb9bae8b5"],["/tags/Java后端/page/2/index.html","73f048f29b22894f7b72276d6b8fe63b"],["/tags/Java基础/index.html","64d6b9711f0804b60c1f063583b3deac"],["/tags/Java基础/page/2/index.html","3afbc4f242f58115d04e41f1392c1db4"],["/tags/Kettle/index.html","6d406ef7022b94aca671c8e148b563e4"],["/tags/Kibana/index.html","25af655df0665768c9ae67152036e1fd"],["/tags/Linux/index.html","56389f0e564f2f955f21b5323cab0d35"],["/tags/Linux/page/2/index.html","50671b58495e0061ce7fde4989a763a5"],["/tags/Linux/page/3/index.html","ac311ccbab0204bdc6e4a1cc7ee781d2"],["/tags/Mac/index.html","851cba00f0c4b0add06dc55b0b7f34ad"],["/tags/Mac/page/2/index.html","9bf1558f1aa62d35ea2a9a82628f19b3"],["/tags/Maven/index.html","4ebdff8e3005b777497d67d505c6c879"],["/tags/MySQL/index.html","ac1a7ca9bb4aec48e3c783ce8b701158"],["/tags/Python/index.html","173389219013d12cf676b6facfb24673"],["/tags/Redis/index.html","f77443ed8439ae507bd60b329607922d"],["/tags/R语言/index.html","03ba73379fda87138f23475f808a3627"],["/tags/Spark/index.html","75b64e8ec58870bae8f73cc4696d02fc"],["/tags/Ubuntu/index.html","02c1a155a1c95756ec6bd56fdb4ecbfc"],["/tags/Vue/index.html","fd8087a3219d122dfdbaf96b0da69684"],["/tags/Windows/index.html","26b962b320781240373a50ce9d4d0b3f"],["/tags/ZooKeeper/index.html","c8825dd8dcd0377e236fd5cf83cf26ca"],["/tags/bfs/index.html","986e4fd612ef7a7f770512c3d442a707"],["/tags/dfs/index.html","68c78ae63065f6b0b824f9e0b9dc3206"],["/tags/folium/index.html","48b329c89ea3904b67ddd394a895b450"],["/tags/git/index.html","be5d53abd99bb67b47e0eaeeac805647"],["/tags/index.html","c0881cbe316c5467d0575ac34cb2b0ac"],["/tags/latex/index.html","a0432e1f80afb0c0f2a1dacbee356d29"],["/tags/中间件/index.html","d1cf985a3bfe6b021877581d6d042be2"],["/tags/二分查找/index.html","6116b69be9f187c05e355e737db13c52"],["/tags/优化类/index.html","056582203e86157e438b7134f5e45004"],["/tags/前端/index.html","e528e0d6c7d2133d7137e240e43f5583"],["/tags/前缀和与差分/index.html","5e41f5eb563cc9c0eb4a37e693653b92"],["/tags/动态规划/index.html","34bc429cf847c2aa6f3caf6d96df98de"],["/tags/动态规划/page/2/index.html","c4dbd3764fe4dddeda67faae773f6f22"],["/tags/博客搭建/index.html","07d53924e46b8a604e5ec4aaeaa31099"],["/tags/图论/index.html","5ba1bd18be864f760c362458d32843ab"],["/tags/大数据/index.html","e58029402be60b39d1ffb4f4ba0b81a2"],["/tags/大数据/page/2/index.html","8d1e14f583ec5563e1028b0258697f09"],["/tags/操作系统/index.html","d077723f09326badb95d122517afdbcc"],["/tags/数学建模/index.html","7835580895d6eda659a916aebfcce4d4"],["/tags/数据库/index.html","63ae11641fdb494382d3e899c7a11905"],["/tags/数据结构和算法/index.html","89c6ad0b767e4809052035c608c74ca4"],["/tags/数据结构和算法/page/2/index.html","f414512aab0d7d7d53f91f2808f35282"],["/tags/数据结构和算法/page/3/index.html","428ac94331dace795496df12922e8874"],["/tags/数据结构和算法/page/4/index.html","07026245033c4fd43644dc800510a0db"],["/tags/数组和字符串/index.html","d04ad0d6e5b5f0a4700c3e5798ab0376"],["/tags/数论/index.html","32f8a137f6744f94967be1fcdbbf3f51"],["/tags/枚举类/index.html","17f684fc4b36125d09d3c701cdbf2a9e"],["/tags/栈和队列/index.html","1a59f5a79adf5eafaf8c58eaaa46303c"],["/tags/树论/index.html","c0fab36f06a47a07890a627b2aa0f658"],["/tags/测试/index.html","ed05c03881286eb3802876708b0e3d2a"],["/tags/环境/index.html","8cce2322056b7fe8730c5e8c67abec8b"],["/tags/环境变量/index.html","fa82de3e291893c3c6b1430c8357243d"],["/tags/绘图/index.html","3d1eabc2c814ae86efdea2dec9cdd1a9"],["/tags/编程工具/index.html","afc5cbfa71d87db2b43a565a8d193220"],["/tags/编程环境/index.html","c1c7b15a7efc9f5c88007a0a62ac7fe0"],["/tags/网络编程/index.html","b39d149e656ffa2ec91ec68dcf8e8ab7"],["/tags/英语语法/index.html","407353db5333919d6cdf2ac6b296179e"],["/tags/计算机操作系统/index.html","fca16e6a5eab5a9d0fc22701fbbd399e"],["/tags/论文/index.html","e2803f0121c1ccbe18d7e4f424eee0e7"],["/tags/资源下载/index.html","68ef039ed6ecd80dcc91c0721fe77924"],["/tags/链表/index.html","9fd3ceae3eaae071f819f16daee49f05"],["/tags/集合/index.html","263e76fdb48a635386a85f5b68c55a58"],["/tags/集群/index.html","63de66fe5bbd8d5b49447c9003cca683"]];
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
