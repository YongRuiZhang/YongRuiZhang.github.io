/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2ece9cf0a83e5eef1fb34aaab465af46"],["/about/index.html","1c389677cfce7cdffb8556ed7fcdf700"],["/archives/2023/01/index.html","e56b41204d95964a081ee713649714f5"],["/archives/2023/02/index.html","7dad38913f7f084b392e31d52a2704b8"],["/archives/2023/02/page/2/index.html","dc71b38c2dd06894d8eb1ba666795f1e"],["/archives/2023/03/index.html","b4a24dd20389a2ee712ff3e4e8f2a047"],["/archives/2023/05/index.html","d41943fee84d9df50bdf7aad971bab8d"],["/archives/2023/06/index.html","999170f8fa7c900d98907bf1af94590d"],["/archives/2023/09/index.html","586c5d6b8b40cee0bff6193d2ed2539f"],["/archives/2023/11/index.html","f5f87177f030f119022c4784b59bf620"],["/archives/2023/12/index.html","c4539e808048e0149a43eade963d6d1a"],["/archives/2023/index.html","0fc65521c3fdced00de1f95dae4d60db"],["/archives/2023/page/2/index.html","9ab401768f5b79610206105288404ff4"],["/archives/2023/page/3/index.html","e45c052b2734cc5a6487bec4720c737a"],["/archives/2023/page/4/index.html","be53504c5ab6f8e1436ff6cc514dabfd"],["/archives/2024/02/index.html","01a161647938a287fce88eb2f3b7d5ab"],["/archives/2024/index.html","d0481f74ff20c56064e08ebb9c69bbb7"],["/archives/index.html","d7c27d10b8527a5609fdf09563868b55"],["/archives/page/2/index.html","538463ec26e8b40f68ede23dee9345ec"],["/archives/page/3/index.html","23a4eb3a8954f25a831767c6bc8857a5"],["/archives/page/4/index.html","94983c31c6bd307dd24c6bb8ee293735"],["/baidu_verify_codeva-qQP2iZOMLX.html","454cfe27b855688168f25cd86658bf34"],["/categories/Java/index.html","14d78ac55848c405fa2813255cd38d64"],["/categories/Java/后端/index.html","41fd4036a41736074fe44b5ef47e0d73"],["/categories/Java/基础/index.html","506213e59bfd9f150ec5df0685283cb2"],["/categories/Java/基础/集合/index.html","81ad7f2943f2559ee655bbefe4c07bf7"],["/categories/Python/index.html","3876577f0d4122aabc559344d10a5c80"],["/categories/Python/编程环境/index.html","45f7f82b306a05555d15c295bade892c"],["/categories/R语言/index.html","69e0aa030989efc50fbad395e2c1d092"],["/categories/R语言/编程环境/index.html","467a198271ec45dbb4eeb4629de89da1"],["/categories/index.html","2c720ca5c4ae521dbd21c0cc58b3b7c0"],["/categories/中间件/index.html","ec1eb4db903248ef7387be2312aa2ffe"],["/categories/前端/Vue/index.html","33bcde345c8a595395fb89509b15d5da"],["/categories/前端/index.html","bf524cfe88ef510545c3bc80141ed5e5"],["/categories/大数据开发/ElasticSearch/index.html","d7c2b1ff0afb45d7cdcdaea082f9f62d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","28557fc12b2c94b6c88c29d1df08cadb"],["/categories/大数据开发/HBase/index.html","afad126891bb6509a986db6b85160653"],["/categories/大数据开发/HBase/学习笔记/index.html","886d4ffa55a4b9107d8aee6ebadfe52f"],["/categories/大数据开发/HBase/环境搭建/index.html","e1c9b8aebf1d4ab82a7aad9a220e9ff3"],["/categories/大数据开发/Hadoop/index.html","727c58f8af346251fab0da06df72b4ab"],["/categories/大数据开发/Hadoop/技术/index.html","4dcc7dcc32fba02f105e7bd2c41a62a5"],["/categories/大数据开发/Hadoop/环境搭建/index.html","7763bfe6d45daae8077a3d5a606e80cb"],["/categories/大数据开发/Redis/index.html","a9dd3d2ef7748493bd10bc754eb5dad1"],["/categories/大数据开发/Redis/技术/index.html","01e59de177f01f730890e29d0f8a000c"],["/categories/大数据开发/Redis/环境搭建/index.html","55178db22580f5910b4f2b79303f23f6"],["/categories/大数据开发/Spark/index.html","0a440d9c86f24755d9a78f8b1436ad3d"],["/categories/大数据开发/Spark/环境搭建/index.html","b96dbc4db8988b22f4fd83d96df2efaf"],["/categories/大数据开发/Zookeeper/index.html","75a8fe5cf780021125bdbfd20032e8db"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b10eb854ebd597e4c871a4a7d16654d9"],["/categories/大数据开发/index.html","0d88a01595efc6f3038e4069e0ae5993"],["/categories/学校课程/index.html","d96c6c17c1d7dcff0c73fd2c532927c8"],["/categories/学校课程/计算机操作系统/index.html","d64273981093a26e16e824d24c1e7891"],["/categories/操作系统/Linux/index.html","f54fda4e43633d0ab60e47ccbaa804a1"],["/categories/操作系统/Mac/index.html","f31094b9166f859715be94e41c6898ce"],["/categories/操作系统/Windows/index.html","70cf24c3a549a4257b47a12359a1b40e"],["/categories/操作系统/index.html","d4ccb75d2eb992a88dcb6863f0f15f83"],["/categories/数学建模/index.html","c7fbf85a4a291493ca1e92ece0cadf7e"],["/categories/数学建模/latex/index.html","50b947232b125c19588c307e2a9a2bc1"],["/categories/数学建模/优化类/index.html","143fd6dd3dcac69c71fa4857985be6d5"],["/categories/数学建模/优化类/现代优化算法/index.html","6974592aaa284d570ae03d3b599967bf"],["/categories/数学建模/优化类/规划类/index.html","2b41a15f80af31188959a4a44a601fb8"],["/categories/数学建模/绘图/index.html","ee7685610385df2f275b0185a14b2636"],["/categories/数据库/MySQL/index.html","ab7823306661a54d07f11591fab7a222"],["/categories/数据库/index.html","a2d0d21e5bf9fc1ca38c736075d1b9a9"],["/categories/数据结构和算法/index.html","ef4591d532d8c858a1d3e05592af042e"],["/categories/数据结构和算法/page/2/index.html","4867782a10534e791d42ec46ef6231dd"],["/categories/数据结构和算法/基本原理/bfs/index.html","8b821c90078bdcba0c3a7e3d3ff8097c"],["/categories/数据结构和算法/基本原理/dfs/index.html","be45618dcbf72a4df96c3185673daf31"],["/categories/数据结构和算法/基本原理/index.html","ba100471318eee7ee8cb96b3b1201029"],["/categories/数据结构和算法/基本原理/动态规划/index.html","48803f9be1e2517b95f4623b9a22510f"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","57c83d214f2e29bd6968a29d8590d954"],["/categories/数据结构和算法/基本原理/图论/index.html","346a8f06ce43ba6e0b326dd3ed4cd4eb"],["/categories/数据结构和算法/基本原理/字符串/index.html","8da97c1c0b196e42dd9f44f4c2a7b6ea"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1c85688108cb47d3538aef0f1a229703"],["/categories/数据结构和算法/基本原理/数论/index.html","f4e308612c48e0610018608c19fb098f"],["/categories/数据结构和算法/基本原理/树论/index.html","68b6223b09efc167cb60f177dfca43f4"],["/categories/数据结构和算法/基本原理/链表/index.html","c37855adaf714e7a914351ca6006a2bb"],["/categories/数据结构和算法/算法题/index.html","d7237c06c7b330bd1a9cdb254598c25e"],["/categories/数据结构和算法/算法题/二分查找/index.html","9003f73ca8c0776b0cc609d73d4ab3e1"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0ff081daebc70f17e7e11a096a9c9a07"],["/categories/数据结构和算法/算法题/动态规划/index.html","3d3a464aa7970c627452c5fee845960b"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d5217ed6d636b76372eee48344a03505"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","02d95e1417b046d12fd01369705aaa4a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c157a3a7ff944e5f666e76ab4f260e4f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4c6679fc2a95285113407fbd020c276b"],["/categories/数据结构和算法/算法题/数论/index.html","4e01bb348c0f5a498149364451844086"],["/categories/数据结构和算法/算法题/栈和队列/index.html","bf00032fd96de32e404a6c552fac04c5"],["/categories/数据结构和算法/算法题/树论/index.html","e619e5e4a03280def30a7ff140be58e3"],["/categories/杂七杂八/index.html","d0129a1e8eb03aab9b725eb4e9c97edf"],["/categories/杂七杂八/博客搭建/index.html","9243d7d353b1e14aa0e569e08ef4d03f"],["/categories/编程工具下载/index.html","976b7e7ef37e27414757d54c18235979"],["/categories/编程环境/index.html","df380e29cf527b70377f7378cbd16d65"],["/categories/编程环境/大数据/index.html","cd4255ac569c6b5d70c092dc2975c2ed"],["/categories/英语学习/index.html","fe801e918999a5ddcaab6dada3d84d84"],["/categories/英语学习/英语语法/index.html","3a2566e15159e35f5b2be6bc0e1728a8"],["/comments/index.html","9a5cc30eaf090b6c82f0d147e4e58cd3"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","afd87c955e12423a53f09274863d2d3a"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cc45ed3bc834070a92a5a50006109a09"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","820544e7d0700d9e2a3dc6119416dfc5"],["/movies/index.html","79175e8c6fc3fa0d5eb639933c4a81e7"],["/music/index.html","0919d613d1f42757495216705e08946d"],["/page/2/index.html","726a54d7ecb002b98d122a4bb027f5c9"],["/page/3/index.html","d1a7df39690d57b55971861d44a8c570"],["/page/4/index.html","563c15a3a5be3b35ea5915a1ce099ca8"],["/page/5/index.html","13f54b267fda9a4ad98d33e073001a2d"],["/page/6/index.html","a34e7d7a07de2ca97be05cddd1e26c3f"],["/posts/1021360842.html","75eeeef28487043308e2aa1e9f6b501e"],["/posts/1120620192.html","edfb0e93e609e8806a3a3d7f3d2cd9d0"],["/posts/1141628095.html","7987a7bd5a33f06784a2d024ca3a7160"],["/posts/1168613674.html","df1f9b2531c5315f5145b60f82072418"],["/posts/1219920510.html","4c64ad48eca61ffd6d6ce43977202f6b"],["/posts/1222166338.html","c534df3dc78d4ec753ab93a90be992dc"],["/posts/1259097482.html","63996706cac1aba4c2b9bca43d2a1402"],["/posts/1271036369.html","2c757cf828ed739be02c75aee0a4de0d"],["/posts/1312847445.html","039bcdc4999c8b5d96c54ff5b0075bf9"],["/posts/135355774.html","0292a4a3acc01d3ded31776b4fa6914d"],["/posts/1375344716.html","a88bc49e2036ecd99fa377d4302a8e99"],["/posts/1388991698.html","cfd39a310340ec40efb4422955327270"],["/posts/1410315814.html","5cbce05567a0cf278833887532988321"],["/posts/1452790229.html","2c020fbd8d000bfb8ead96bb0268fbc9"],["/posts/1470079884.html","bf80e2d19a87ffb9d1f2c7e8bd493456"],["/posts/1470079885.html","ce709e7cc8f47f1a58bd80cbbac95369"],["/posts/1470079886.html","90d491beae67f10fadb0fca3d7409cb7"],["/posts/1470079887.html","3b91db870bfd6e990bb5925883c069eb"],["/posts/1498536549.html","f2431ea1ac9532fa3d8280f9ef18ae37"],["/posts/1539568593.html","0cfe3f2751fe7ee0db2621c4db455ad1"],["/posts/1547067935.html","c11ccaa15f28d4d532b4dbce7d640c05"],["/posts/1557866301.html","7150b9ffc4120727cfe80b1579eda7c2"],["/posts/1571776361.html","696871f649e722abac8498649398ea0a"],["/posts/1605124548.html","0f4ef1b9802a5ae48632f1e590c627f8"],["/posts/1633036852.html","8087c34bea5cbfd31a0db0255b050d23"],["/posts/1674202625.html","088416138b4752593fdcff8b10c54fef"],["/posts/1765123828.html","29d8db86ce52a6923deac166e0018ea5"],["/posts/1767336200.html","f9a020e5d44fc99131ad0b0eed14144f"],["/posts/1776114197.html","e2c88fbd4dc3a44afa5d5ef177374eaa"],["/posts/1817748743.html","8a73a35f925dda4b34a2a5bbab1bff47"],["/posts/1925125395.html","4c8d543eb44fb070aaf35140b5789caf"],["/posts/1966191251.html","5bba522f8612c84f569682834fbec3ef"],["/posts/1987617322.html","49f604448781937ff52eaab2023b95e6"],["/posts/1999788039.html","7f405faf39c6f2999ea43bcb3ee2e3eb"],["/posts/2075104059.html","70a0cf5f91b54d794248571ba2566886"],["/posts/2087796737.html","616bbdfea3cc3f07047d023048a070d8"],["/posts/2106547339.html","5de0353f0b023f2bbc1757331ec6fae5"],["/posts/2207806286.html","094a29a79a3e9a0b61cfe1a710a3458e"],["/posts/2225903441.html","ac61ba0182e4287a380287a0969dc8d1"],["/posts/2265610284.html","3327e9789a9b4ac450e097032f904e9f"],["/posts/2281352001.html","7df4b9e280346d2e1d25de3617123b04"],["/posts/2364755265.html","1da750b6a33e966604e331c60d591c1e"],["/posts/2414116852.html","6ef4b8c8f50b2cb40805ed8284cc42bd"],["/posts/2421785022.html","19ce955a8cfe24d5d26e635c25a21091"],["/posts/2482902029.html","744e34f4854a1de2fbbd5865fdfdefab"],["/posts/2495386210.html","b8e5b6cf9116f66cc65f959e748a99ff"],["/posts/2516528882.html","83689797d8500cafeabd9ce89c1bcb34"],["/posts/2526659543.html","713c8583f1ef7fdb83dc7f92f4f08f7f"],["/posts/2529807823.html","4253b998f288ad5262c7f8c1b00915de"],["/posts/2596601004.html","a367dd72a39c612253c582d6d0a4cba6"],["/posts/2697614349.html","bc871315cf39b856a87796ec2c2013b4"],["/posts/2742438348.html","325d7dd59e852d2ad90de7867edc66ea"],["/posts/2768249503.html","dd46b9bf80f77cc31485ac53bd624ec7"],["/posts/2864584994.html","9b4d849bcd9a597bc2545a1b4a3dcae1"],["/posts/2888309600.html","99f36d6f8188d2db540ef41abbcd4f48"],["/posts/2891591958.html","0844f93a47042a7b71c6484eb3e2ef5d"],["/posts/2909934084.html","aec1a763be50edd9cf9cbddc4478a151"],["/posts/2920256992.html","0bbbe9bf14d67d04bc34a55b58008da0"],["/posts/2959474469.html","08b7001d5a4057d140377d4db05710a6"],["/posts/3005926051.html","154abe630f3c97cc1ececcab0922cb8e"],["/posts/309775400.html","b8e4717c9ce90dab3011247a67517b5d"],["/posts/3156194925.html","9e94e321eb2775dabc7241fc75937dbe"],["/posts/3169224211.html","60932f0f9f3dec8e33311097d1dbe243"],["/posts/3213899550.html","c019f80e537ed9baf4d17c27371db62c"],["/posts/3259212833.html","124c67eff355c4c9d86f31e5d5257186"],["/posts/3266130344.html","92fd04aa9072e5a3eb884eb4b15a73c0"],["/posts/3292663995.html","7e6e28f4677f4a5a4e65e58df3dbd212"],["/posts/3297135020.html","df128517b31b28f68cd47d6ba3c0d5b5"],["/posts/3306641566.html","d781f9cdc6021330fe3b190afd794706"],["/posts/3312011324.html","b21b89d317a57acd22dcb6f8768c5f9b"],["/posts/336911618.html","2ce8db6eda1952969fc3dc2999691663"],["/posts/3402121571.html","f9e709e4a2d435e48a4ba2f8923f0a9d"],["/posts/3405577485.html","ae5c745d5ccaa8795abb86b2f9568e4d"],["/posts/3498516849.html","ec67ab687c3304c4faaee46a341e7512"],["/posts/3513711414.html","22622b3e1cb26e50f36b85e5265bd5e9"],["/posts/3523095624.html","08d5f9c941a88acb73eff22da9ac2244"],["/posts/3546711884.html","d0c8ae9f32ea552da4939cbd3d369864"],["/posts/3731385230.html","d97cdb5adc65b62b1dd486b7ca67b3ec"],["/posts/3772089482.html","7529a5530bfc01f50add97c9004bc026"],["/posts/386609427.html","29fe9a76e3f40b11751f800cf3fcc414"],["/posts/4044235327.html","5982b246cd341423fcf94760f42acccc"],["/posts/4115971639.html","c7f847c1c4fec11661f3088a57e2b1f8"],["/posts/4130790367.html","4493c8f2f177bbfc1e739efbad55a608"],["/posts/4131986683.html","dc2291be905e427a6454d87e7c7e8c4f"],["/posts/4177218757.html","9fb774f3ec71fea5c2dc5fd312f9a7e7"],["/posts/4192183953.html","c49c2b8dec00ca651bc0b7dbfdbea6e5"],["/posts/4261103898.html","ffede8f24f503239ed8275d4b25077d0"],["/posts/469711973.html","adefd48cb070e82ca95becd15f6dc721"],["/posts/482495853.html","3a1e65b98d6083a6994a47514dc2b1c3"],["/posts/488247922.html","73dd17ee2a1e5679b12b5fcedc65dc50"],["/posts/517302816.html","96a564d72133ec2d9f74e8cf6e1a25f9"],["/posts/570165348.html","798270c33211a89b29bef2ee49925c7f"],["/posts/595890772.html","9940e77d5f2eacbe9f2eab6c6a4bf5bb"],["/posts/67485572.html","8f9d4f244ddd080892a61d2b8b6dcb89"],["/posts/694347442.html","172ef596e3b946769dbfeb654f6b3910"],["/posts/707384687.html","fe8661d8ce6108bdb7dea7583a9bfcc9"],["/posts/71180092.html","d76c3e097d6bf84613cb7a4cd960224f"],["/posts/716459272.html","fbf98259008e9f1e32b22f414d222097"],["/posts/765481613.html","28c629d8f992604b94b93c99809adfc8"],["/posts/778231993.html","dce813d9d4abd12d9979151c0956e3eb"],["/posts/795397410.html","ae75245bcd787de75431df4f96a848dd"],["/posts/820223701.html","168d618e34e7bb2c6f5d1d16d0e57209"],["/posts/830372185.html","cf8db822e670c6aa07c91c726d51af02"],["/posts/88294277.html","33448a41ee18680e9f8e8040fdb52de2"],["/posts/939963535.html","aab07e40afa5e64bcd28d1e106d711ad"],["/posts/983786067.html","47ade11ac04eeaa6d46ac266e006a3c2"],["/sw-register.js","acc89fe732c930cb32e22fb5ce65f85f"],["/tags/C/index.html","4d6dfc53313c966aa6885be2c40031eb"],["/tags/C/page/2/index.html","a3d798e1da093b1f1166c5b1ab8f84cf"],["/tags/C/page/3/index.html","177d2c91cc00e6b9641134098e711b97"],["/tags/C/page/4/index.html","0aa388fda110fc39916e7a1b12bf430c"],["/tags/ETL/index.html","143342c46bdca5d7dad1e3e9a6002e3f"],["/tags/ElasticSearch/index.html","e9fcb7b36939321666aba221e3eab986"],["/tags/GUI/index.html","6a93dd0259710cb9b8d3687389d16acb"],["/tags/HBase/index.html","3beeb81bcf1a2dca96515925ce43a8d3"],["/tags/Hadoop/index.html","02b1ea707d6f250b086eb00400577133"],["/tags/Hadoop/page/2/index.html","7cc3fecbd0d4b6219b366e1de24a1c4b"],["/tags/Java/index.html","f7465e434912e132e479815d095327d0"],["/tags/Java后端/index.html","cc8d4da58db1125370240074e051ed52"],["/tags/Java后端/page/2/index.html","5448f3cb71dd3bb62a6737ff84c19140"],["/tags/Java基础/index.html","c3edfcdeea104af2b528f9f0995644e1"],["/tags/Java基础/page/2/index.html","72eea9d848dd55001ab775c6d3b76faf"],["/tags/Kettle/index.html","8bcad72256bc832e3b0689940f1f1e5e"],["/tags/Kibana/index.html","48e177079c25103af2e1c5aa4a2de21a"],["/tags/Linux/index.html","505ab84058ab50fb7589d9194a174a09"],["/tags/Linux/page/2/index.html","80f4e006b99bb9b694e8043e33ab4a4f"],["/tags/Linux/page/3/index.html","6159414984275ad54ba1c8909a7a5854"],["/tags/Mac/index.html","a2e36690bfb08c2ddaa5783a637511ac"],["/tags/Mac/page/2/index.html","4e8eefd99f6e6ac96be38861beffe71b"],["/tags/Maven/index.html","af6a69d6350cdf45f9cd6b48c86d3436"],["/tags/MySQL/index.html","228d61f6486fb86b933ebc69797cb37f"],["/tags/Python/index.html","26145e63b3bf6bdb017ca51eb4d631bd"],["/tags/Redis/index.html","a2f266314e18a0b06ec4a7e398ddb2d8"],["/tags/R语言/index.html","52c86426d43fd7a20dc4481b8d9d7ba8"],["/tags/Spark/index.html","2d9d6686ede2b5a14050ec8a6d2a6b50"],["/tags/Ubuntu/index.html","c8dd154b6dbd4d72b2acb341a22555ae"],["/tags/Vue/index.html","6b0894645987b19b1e87624e7fe7179a"],["/tags/Windows/index.html","17cebc0f93e7bf2c9939920855f45e1c"],["/tags/ZooKeeper/index.html","9f0d388d51c1244e685ad40e3612fe85"],["/tags/bfs/index.html","7c8139e65e7634d55d7cdd6dad06e969"],["/tags/dfs/index.html","e1cc13816fad68ed4b559de780cc5b84"],["/tags/folium/index.html","92fc0d4dc1ba96da9bb8ffca19a272f1"],["/tags/git/index.html","bd180bcc68d346198cc1469d09c5d4b3"],["/tags/index.html","a1f543c50c60e4aab07d1506e35a7ae9"],["/tags/latex/index.html","2895c2d71aa6ba4793bd24a6e0e07a13"],["/tags/中间件/index.html","32807b9d650bbd32fb679c1359266596"],["/tags/二分查找/index.html","d60e86ef4a5667cb364a90d947b7f051"],["/tags/优化类/index.html","8e62dc3bc6cde3a6d8506ea841562ac8"],["/tags/前端/index.html","7c663633ac0c28235c33494d5a54be35"],["/tags/前缀和与差分/index.html","eca3e85d68bdebd1fbc63d3eb9e75493"],["/tags/动态规划/index.html","fda93140c2b4ff0c4fd2fb55e32c89d6"],["/tags/动态规划/page/2/index.html","d62906f06c7b6d0ca2c92588a04064ab"],["/tags/博客搭建/index.html","75d9d3b72803c6019d4a7985af5e489d"],["/tags/图论/index.html","3d3ed71634944099a6d3a1bb6d216e41"],["/tags/大数据/index.html","921ab54a3d5525941b4b39cfcb151cd5"],["/tags/大数据/page/2/index.html","d8142c917ea6b0c52bc5076eb594aca7"],["/tags/操作系统/index.html","3aff0bc90ad62f4170cf8924c44001d2"],["/tags/数学建模/index.html","ad2a52b721ea144919ec547f3f5ec0ec"],["/tags/数据库/index.html","018876845f75cb3e65c2163f877f258b"],["/tags/数据结构和算法/index.html","62c6e14c901e628dc9d9743202d3f630"],["/tags/数据结构和算法/page/2/index.html","07df9c63300a223fc83f601c4273692a"],["/tags/数据结构和算法/page/3/index.html","6ea36f8b0e949a46816d0b51830e84d1"],["/tags/数据结构和算法/page/4/index.html","3e7f387b608b0da8b7b7ca4cae6869df"],["/tags/数组和字符串/index.html","cec0307f35db23b5011237e12175b785"],["/tags/数论/index.html","ac6c6259f35340039e2759e0cb688e7e"],["/tags/枚举类/index.html","d8f5461d2dabfa95fdbe825c0e6ef481"],["/tags/栈和队列/index.html","c4fbcbd8f62cfee502596e55ae1f5397"],["/tags/树论/index.html","df1a6fb9eeca070bdaf3de58aa9f95c4"],["/tags/测试/index.html","7b115dea775218cc1db73a7e3e7122ee"],["/tags/环境/index.html","0b3cd509c6462b09896a0908590bcd86"],["/tags/环境变量/index.html","2a373cfa0fca4b9b4e56f390d264405d"],["/tags/绘图/index.html","975085a84bd08c137ad5e639b83f6d83"],["/tags/编程工具/index.html","e34fbc81a0e6e2f32354661f0db322b3"],["/tags/编程环境/index.html","5833ef1b1f03968c5267d9d98dbd1b41"],["/tags/网络编程/index.html","fa213779ccd74bbd77891e8a298a2c7c"],["/tags/英语语法/index.html","d4729088ab5ada94c9b255fddd01d2c3"],["/tags/计算机操作系统/index.html","2b98f7ab5b9dc98a8e0debf7c9b9fd18"],["/tags/论文/index.html","fe1f133df5b24a251810826c2b052d48"],["/tags/资源下载/index.html","7eaeb954d798d3db1f07c6e47e58e509"],["/tags/链表/index.html","87f5fbf6a3afe88cae34bc7234630784"],["/tags/集合/index.html","402718bf4ef1f42c0b2b1b40bf1e06ba"],["/tags/集群/index.html","caa5b78456a4eb37866aa7edb64f05ce"]];
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
