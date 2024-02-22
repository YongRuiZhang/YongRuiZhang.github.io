/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","a62d5a0daa5e08107e52cea232eb3bb0"],["/about/index.html","1c389677cfce7cdffb8556ed7fcdf700"],["/archives/2023/01/index.html","13195e7fc5e98c9d251cd311a258db23"],["/archives/2023/02/index.html","e404b8ad64038b08f5a6137cc3a0549e"],["/archives/2023/02/page/2/index.html","6eaf23e70f2efb3cd3619ba5bb4b4e0e"],["/archives/2023/03/index.html","07db16887b73c9752f36c4678405d639"],["/archives/2023/05/index.html","13bb7f592e080e1587e8a8c6e6083e00"],["/archives/2023/06/index.html","d12b5fb773c645964b9ecba35820c71e"],["/archives/2023/09/index.html","d96f6fe9c33fa1f907b08b7f226bea53"],["/archives/2023/11/index.html","6950f335ac6778f0af076cdc2524de42"],["/archives/2023/12/index.html","b55f43651b26bc90ff9e7715b8d1b862"],["/archives/2023/index.html","e9000e7f8b151fbec9b8268366211ecd"],["/archives/2023/page/2/index.html","e5f1c70872228173de14200624d4fb69"],["/archives/2023/page/3/index.html","1122675ca9494a5325b4d9f15ae8ccf2"],["/archives/2023/page/4/index.html","d80efbfa06ff755212ff3045428512b7"],["/archives/2024/02/index.html","ed9eefe17c6a881b5000e425a6bfdb26"],["/archives/2024/index.html","67bebb7099606215c5a29e17d079373e"],["/archives/index.html","d4291ea127e7687e9ce8bb96abf6e079"],["/archives/page/2/index.html","419519d5d4b310bea508dca163190573"],["/archives/page/3/index.html","6b99259449a33170cf951ca565fa0610"],["/archives/page/4/index.html","faf43787be57fe4efb02de02282f01b2"],["/baidu_verify_codeva-qQP2iZOMLX.html","ebb17c5688dea1be99de8ea9b177970e"],["/categories/Java/index.html","457248f1052073d46612bc47af2b2ab0"],["/categories/Java/后端/index.html","6295e9f8321a2e430e3fa53528a4bd82"],["/categories/Java/基础/index.html","c4310d504e8b4821f2cd55c500df47de"],["/categories/Java/基础/集合/index.html","b551861f249cd22d98b2226b2c6365df"],["/categories/Python/index.html","12c56a182290324f46bb2b4cb82adc8d"],["/categories/Python/编程环境/index.html","088618a1460783b5157d7c3d14a803fb"],["/categories/R语言/index.html","8b5828ac0c18dedd7170de8f7fbfc094"],["/categories/R语言/编程环境/index.html","a184b5e71c48b788ea51c905309b4f64"],["/categories/index.html","2c720ca5c4ae521dbd21c0cc58b3b7c0"],["/categories/中间件/index.html","1e2e560d2b575c7609aee354f89ef2d0"],["/categories/前端/Vue/index.html","a8bf0da4892dd860e1a197d233cc7258"],["/categories/前端/index.html","d509cf2a510c8c85cdf165836bd248a0"],["/categories/大数据开发/ElasticSearch/index.html","d183bd734253f7fb361f1001b35ce8a5"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","96dd2ce5859a08341423e8149edcaf8f"],["/categories/大数据开发/HBase/index.html","49b25940ddd6b7351cfb6d2e9ef4b291"],["/categories/大数据开发/HBase/学习笔记/index.html","fa99881ae9c692ba63d8d1b024c81881"],["/categories/大数据开发/HBase/环境搭建/index.html","9de3c0bb812b7573ab91def336703b0f"],["/categories/大数据开发/Hadoop/index.html","ccb33577a769e1a70f3c9b60f5d900a0"],["/categories/大数据开发/Hadoop/技术/index.html","a83852448ad892b8731f19f202f4eedb"],["/categories/大数据开发/Hadoop/环境搭建/index.html","671d5ae2a87b3713af763b3e9f1939db"],["/categories/大数据开发/Redis/index.html","d8b1f9da7ce2ceed11e75e4f59496507"],["/categories/大数据开发/Redis/技术/index.html","6de2afac0893b3b201448e0317be2aec"],["/categories/大数据开发/Redis/环境搭建/index.html","c4337b47350ca9887232ef4bcf6e9222"],["/categories/大数据开发/Spark/index.html","7f5d8bdc6f1690eaf23c0f5e86241ad0"],["/categories/大数据开发/Spark/环境搭建/index.html","42565e56acacdfb91514a6b484dc7a92"],["/categories/大数据开发/Zookeeper/index.html","feab5dd81247139e5b56c62d135079ad"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","72cf0150e5e52cdd3e54fed61762495b"],["/categories/大数据开发/index.html","78a36bbb690cdd0144284973c16b1c0c"],["/categories/学校课程/index.html","18d540f7e4cfaba83f7c230a14c6ad49"],["/categories/学校课程/计算机操作系统/index.html","5d7ae79b0ea265159de8aab1d8edb81f"],["/categories/操作系统/Linux/index.html","48e3553ee96ad1b4f6b6c105c3430a6e"],["/categories/操作系统/Mac/index.html","da0633f64a35f1ea5d2dcf34bbce8b5e"],["/categories/操作系统/Windows/index.html","2001bbc8376dde9dab71faf4befd6e32"],["/categories/操作系统/index.html","ab75f1bfed4e09eff1d8568420e0e8b3"],["/categories/数学建模/index.html","d9d0b24ae30276c6a64652af4a756351"],["/categories/数学建模/latex/index.html","c8093cc4c9e3000260a86e0828096f54"],["/categories/数学建模/优化类/index.html","565179fb24fe3a400d7bba1db87d8a9f"],["/categories/数学建模/优化类/现代优化算法/index.html","068eede5b5874da5625df92feeac1590"],["/categories/数学建模/优化类/规划类/index.html","92cab132a3e72c4be719e005e74006de"],["/categories/数学建模/绘图/index.html","b48213683128e52e6cef2e1dd4a63cfd"],["/categories/数据库/MySQL/index.html","d8c32570c78b3d25d05e0c8a2c9d7c25"],["/categories/数据库/index.html","cd66ada230bb5a03221a8ce3f2f61f68"],["/categories/数据结构和算法/index.html","be69668026bd2a4fc7cfb4c3c3ef08af"],["/categories/数据结构和算法/page/2/index.html","189eba63143ef03cfd74a5b1b377d35a"],["/categories/数据结构和算法/基本原理/bfs/index.html","ea1b43d07458eb3f77c7b8bc0b4e118e"],["/categories/数据结构和算法/基本原理/dfs/index.html","4191dd3e6f9997e9376993dc4a36956f"],["/categories/数据结构和算法/基本原理/index.html","f7783adec9be8cfa5d6b173bda04a195"],["/categories/数据结构和算法/基本原理/动态规划/index.html","18219ffb29ee1f104ddbb9362e34dcb6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","8631695d1b309c8fec37d5e51c605091"],["/categories/数据结构和算法/基本原理/图论/index.html","f177a21d54d5420009734914e69d7aa2"],["/categories/数据结构和算法/基本原理/字符串/index.html","7c9b8fee19e9903a4384dabccf90d64f"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","14d1ddb94306527b53b820c4583fc349"],["/categories/数据结构和算法/基本原理/数论/index.html","d9273fd3a4f6ec66c1f561ac43f15833"],["/categories/数据结构和算法/基本原理/树论/index.html","8946453a37b536787a31263a620f0435"],["/categories/数据结构和算法/基本原理/链表/index.html","8e44cc6815c9547c32d3e32a1420465b"],["/categories/数据结构和算法/算法题/index.html","065d896ed270aedd456ead5e930d26d8"],["/categories/数据结构和算法/算法题/二分查找/index.html","828e182c070165c31fe6866989ad219b"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f10af7215fff02767d1b6cbc22db7640"],["/categories/数据结构和算法/算法题/动态规划/index.html","022d5bb9948c76efb8b21c6141bba9bb"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","b4b1d6d02e2c20dea55f6ad5309d32fc"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","af68d43a88705e585c60e65df0ec9aa5"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","56bf350899feffe085473610c9646d16"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b473e2e75b27c93f069c4aa419f7c102"],["/categories/数据结构和算法/算法题/数论/index.html","2885f52d04c5c38888204ea9a6ad6719"],["/categories/数据结构和算法/算法题/栈和队列/index.html","cc27c8bd982285bf3a6385a4415e61a5"],["/categories/数据结构和算法/算法题/树论/index.html","fdb0603d11fa5956f88db065b49ebf60"],["/categories/杂七杂八/index.html","c42ff2c2bce7ceb31406e234114a7d7b"],["/categories/杂七杂八/博客搭建/index.html","e3d0e5c1be0ae549ee20b6aa5517df5a"],["/categories/编程工具下载/index.html","bb6d7d2bdef4fe22332b4316b23ec978"],["/categories/编程环境/index.html","c69aed91ce9d23e49d19fc93fa650c64"],["/categories/编程环境/大数据/index.html","16ed9eeb814d01343f80ae671759c4ae"],["/categories/英语学习/index.html","3c8d7e9ec9977f00f77b9b57bcff750e"],["/categories/英语学习/英语语法/index.html","04bed60b1a3b1b83173230a52d66e106"],["/comments/index.html","b23b3ca092b75ad8d1dc0ac3d4ca7d64"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","9779bf68f8ecfdc53284e6b6b3b60721"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","7a79738982fa8d70071087c68817a977"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ad866242be9ab7539b9e3f42a0571ca3"],["/movies/index.html","1797bf4bf4256d56bad88bf1ba5558b7"],["/music/index.html","48a587eca2bfeddd08489300c2f925d3"],["/page/2/index.html","ddc9667b4e3fd7b394709fa8cbd63ffa"],["/page/3/index.html","ea161c66aab54e9a64885d14f51eed15"],["/page/4/index.html","a0031b7a7bc1eb779ae2c47939ea7327"],["/page/5/index.html","6f8c140633f4aec22c95c6c61acb0fcc"],["/page/6/index.html","1d585c37c519b34c068defe0ab53badf"],["/posts/1021360842.html","75eeeef28487043308e2aa1e9f6b501e"],["/posts/1120620192.html","edfb0e93e609e8806a3a3d7f3d2cd9d0"],["/posts/1141628095.html","7987a7bd5a33f06784a2d024ca3a7160"],["/posts/1168613674.html","df1f9b2531c5315f5145b60f82072418"],["/posts/1219920510.html","4c64ad48eca61ffd6d6ce43977202f6b"],["/posts/1222166338.html","c534df3dc78d4ec753ab93a90be992dc"],["/posts/1259097482.html","63996706cac1aba4c2b9bca43d2a1402"],["/posts/1271036369.html","2c757cf828ed739be02c75aee0a4de0d"],["/posts/1312847445.html","039bcdc4999c8b5d96c54ff5b0075bf9"],["/posts/135355774.html","0292a4a3acc01d3ded31776b4fa6914d"],["/posts/1375344716.html","a88bc49e2036ecd99fa377d4302a8e99"],["/posts/1388991698.html","cfd39a310340ec40efb4422955327270"],["/posts/1410315814.html","5cbce05567a0cf278833887532988321"],["/posts/1452790229.html","2c020fbd8d000bfb8ead96bb0268fbc9"],["/posts/1470079884.html","bf80e2d19a87ffb9d1f2c7e8bd493456"],["/posts/1470079885.html","ce709e7cc8f47f1a58bd80cbbac95369"],["/posts/1470079886.html","90d491beae67f10fadb0fca3d7409cb7"],["/posts/1470079887.html","3b91db870bfd6e990bb5925883c069eb"],["/posts/1498536549.html","f2431ea1ac9532fa3d8280f9ef18ae37"],["/posts/1539568593.html","0cfe3f2751fe7ee0db2621c4db455ad1"],["/posts/1547067935.html","c11ccaa15f28d4d532b4dbce7d640c05"],["/posts/1557866301.html","7150b9ffc4120727cfe80b1579eda7c2"],["/posts/1571776361.html","696871f649e722abac8498649398ea0a"],["/posts/1605124548.html","0f4ef1b9802a5ae48632f1e590c627f8"],["/posts/1633036852.html","8087c34bea5cbfd31a0db0255b050d23"],["/posts/1674202625.html","088416138b4752593fdcff8b10c54fef"],["/posts/1765123828.html","29d8db86ce52a6923deac166e0018ea5"],["/posts/1767336200.html","f9a020e5d44fc99131ad0b0eed14144f"],["/posts/1776114197.html","e2c88fbd4dc3a44afa5d5ef177374eaa"],["/posts/1817748743.html","8a73a35f925dda4b34a2a5bbab1bff47"],["/posts/1925125395.html","4c8d543eb44fb070aaf35140b5789caf"],["/posts/1966191251.html","5bba522f8612c84f569682834fbec3ef"],["/posts/1987617322.html","49f604448781937ff52eaab2023b95e6"],["/posts/1999788039.html","7f405faf39c6f2999ea43bcb3ee2e3eb"],["/posts/2075104059.html","70a0cf5f91b54d794248571ba2566886"],["/posts/2087796737.html","616bbdfea3cc3f07047d023048a070d8"],["/posts/2106547339.html","5de0353f0b023f2bbc1757331ec6fae5"],["/posts/2207806286.html","094a29a79a3e9a0b61cfe1a710a3458e"],["/posts/2225903441.html","ac61ba0182e4287a380287a0969dc8d1"],["/posts/2265610284.html","3327e9789a9b4ac450e097032f904e9f"],["/posts/2281352001.html","7df4b9e280346d2e1d25de3617123b04"],["/posts/2364755265.html","1da750b6a33e966604e331c60d591c1e"],["/posts/2414116852.html","6ef4b8c8f50b2cb40805ed8284cc42bd"],["/posts/2421785022.html","19ce955a8cfe24d5d26e635c25a21091"],["/posts/2482902029.html","744e34f4854a1de2fbbd5865fdfdefab"],["/posts/2495386210.html","b8e5b6cf9116f66cc65f959e748a99ff"],["/posts/2516528882.html","83689797d8500cafeabd9ce89c1bcb34"],["/posts/2526659543.html","713c8583f1ef7fdb83dc7f92f4f08f7f"],["/posts/2529807823.html","4253b998f288ad5262c7f8c1b00915de"],["/posts/2596601004.html","a367dd72a39c612253c582d6d0a4cba6"],["/posts/2697614349.html","bc871315cf39b856a87796ec2c2013b4"],["/posts/2742438348.html","325d7dd59e852d2ad90de7867edc66ea"],["/posts/2768249503.html","dd46b9bf80f77cc31485ac53bd624ec7"],["/posts/2864584994.html","9b4d849bcd9a597bc2545a1b4a3dcae1"],["/posts/2888309600.html","99f36d6f8188d2db540ef41abbcd4f48"],["/posts/2891591958.html","0844f93a47042a7b71c6484eb3e2ef5d"],["/posts/2909934084.html","aec1a763be50edd9cf9cbddc4478a151"],["/posts/2920256992.html","0bbbe9bf14d67d04bc34a55b58008da0"],["/posts/2959474469.html","08b7001d5a4057d140377d4db05710a6"],["/posts/3005926051.html","154abe630f3c97cc1ececcab0922cb8e"],["/posts/309775400.html","b8e4717c9ce90dab3011247a67517b5d"],["/posts/3156194925.html","9e94e321eb2775dabc7241fc75937dbe"],["/posts/3169224211.html","60932f0f9f3dec8e33311097d1dbe243"],["/posts/3213899550.html","c019f80e537ed9baf4d17c27371db62c"],["/posts/3259212833.html","124c67eff355c4c9d86f31e5d5257186"],["/posts/3266130344.html","92fd04aa9072e5a3eb884eb4b15a73c0"],["/posts/3292663995.html","7e6e28f4677f4a5a4e65e58df3dbd212"],["/posts/3297135020.html","df128517b31b28f68cd47d6ba3c0d5b5"],["/posts/3306641566.html","d781f9cdc6021330fe3b190afd794706"],["/posts/3312011324.html","b21b89d317a57acd22dcb6f8768c5f9b"],["/posts/336911618.html","2ce8db6eda1952969fc3dc2999691663"],["/posts/3402121571.html","f9e709e4a2d435e48a4ba2f8923f0a9d"],["/posts/3405577485.html","ae5c745d5ccaa8795abb86b2f9568e4d"],["/posts/3498516849.html","ec67ab687c3304c4faaee46a341e7512"],["/posts/3513711414.html","22622b3e1cb26e50f36b85e5265bd5e9"],["/posts/3523095624.html","08d5f9c941a88acb73eff22da9ac2244"],["/posts/3546711884.html","d0c8ae9f32ea552da4939cbd3d369864"],["/posts/3731385230.html","d97cdb5adc65b62b1dd486b7ca67b3ec"],["/posts/3772089482.html","7529a5530bfc01f50add97c9004bc026"],["/posts/386609427.html","29fe9a76e3f40b11751f800cf3fcc414"],["/posts/4044235327.html","5982b246cd341423fcf94760f42acccc"],["/posts/4115971639.html","c7f847c1c4fec11661f3088a57e2b1f8"],["/posts/4130790367.html","4493c8f2f177bbfc1e739efbad55a608"],["/posts/4131986683.html","dc2291be905e427a6454d87e7c7e8c4f"],["/posts/4177218757.html","9fb774f3ec71fea5c2dc5fd312f9a7e7"],["/posts/4192183953.html","c49c2b8dec00ca651bc0b7dbfdbea6e5"],["/posts/4261103898.html","ffede8f24f503239ed8275d4b25077d0"],["/posts/469711973.html","adefd48cb070e82ca95becd15f6dc721"],["/posts/482495853.html","3a1e65b98d6083a6994a47514dc2b1c3"],["/posts/488247922.html","73dd17ee2a1e5679b12b5fcedc65dc50"],["/posts/517302816.html","96a564d72133ec2d9f74e8cf6e1a25f9"],["/posts/570165348.html","798270c33211a89b29bef2ee49925c7f"],["/posts/595890772.html","9940e77d5f2eacbe9f2eab6c6a4bf5bb"],["/posts/67485572.html","8f9d4f244ddd080892a61d2b8b6dcb89"],["/posts/694347442.html","172ef596e3b946769dbfeb654f6b3910"],["/posts/707384687.html","fe8661d8ce6108bdb7dea7583a9bfcc9"],["/posts/71180092.html","d76c3e097d6bf84613cb7a4cd960224f"],["/posts/716459272.html","fbf98259008e9f1e32b22f414d222097"],["/posts/765481613.html","28c629d8f992604b94b93c99809adfc8"],["/posts/778231993.html","dce813d9d4abd12d9979151c0956e3eb"],["/posts/795397410.html","ae75245bcd787de75431df4f96a848dd"],["/posts/820223701.html","168d618e34e7bb2c6f5d1d16d0e57209"],["/posts/830372185.html","cf8db822e670c6aa07c91c726d51af02"],["/posts/88294277.html","33448a41ee18680e9f8e8040fdb52de2"],["/posts/939963535.html","aab07e40afa5e64bcd28d1e106d711ad"],["/posts/983786067.html","47ade11ac04eeaa6d46ac266e006a3c2"],["/sw-register.js","077e8ce75acc2f924a0040e6dae8ffd7"],["/tags/C/index.html","abb08f9c53fd3c7f8ed8f6832dae75aa"],["/tags/C/page/2/index.html","f5f81e09b4de115c5645881f4e2e070b"],["/tags/C/page/3/index.html","409f28543fa46c1a9f0dfa5fa9cd3f88"],["/tags/C/page/4/index.html","b6f78ea7a05362bd67f410ffacd0cda3"],["/tags/ETL/index.html","b1b6e6ba49b844f39800b8671a9dca2c"],["/tags/ElasticSearch/index.html","92672d0d4e71087a300042915c04be85"],["/tags/GUI/index.html","8455ea57583734827700a11aa4938ed4"],["/tags/HBase/index.html","36d01309d14f07daf84012a2511d7593"],["/tags/Hadoop/index.html","dc0e6c826e08c11698cdba2ffdb01496"],["/tags/Hadoop/page/2/index.html","49d52d7e75f565bde39bfee839702e48"],["/tags/Java/index.html","925ccd0f00ce667018625e2aa190bf1b"],["/tags/Java后端/index.html","6f4da11670f18a6362b74a9454e07ff8"],["/tags/Java后端/page/2/index.html","8ab980acdbd0570cdc137faa0c18acdb"],["/tags/Java基础/index.html","71e5cb8491155b087765dfccca0c6368"],["/tags/Java基础/page/2/index.html","4359634921c742b9cf9afab2380547b2"],["/tags/Kettle/index.html","c122a7eb40b891d37dabcf9f596bed69"],["/tags/Kibana/index.html","7881affcdcf597c047f0c1aa11648e6e"],["/tags/Linux/index.html","0a6c4d858ee075418c73bb54c9707946"],["/tags/Linux/page/2/index.html","71471294c4d5e8a36a683b3e6bb168ce"],["/tags/Linux/page/3/index.html","743a13f0d778aa1ab64a6f7b89328d17"],["/tags/Mac/index.html","207b608a626f9c8835ef08590c522fe8"],["/tags/Mac/page/2/index.html","64104894a23a5e37e204fbb154ed3be2"],["/tags/Maven/index.html","edff7bfe45373359fcefe6f03ac59cb7"],["/tags/MySQL/index.html","220b383e0aea34be4853c8512e618865"],["/tags/Python/index.html","258bf5e91c3d3a6183d7cab50f7472f4"],["/tags/Redis/index.html","b7656489f4c7274f8770402c18ff2338"],["/tags/R语言/index.html","7b32cee2e6cb76b2aeab42ecbc3b77a3"],["/tags/Spark/index.html","3678c8c7f7786fcc23b934fdf556a6d5"],["/tags/Ubuntu/index.html","4f4333e14eb8ec3112f2cdfc404b380f"],["/tags/Vue/index.html","33b3e1e10c329d6fbb73067d8e81c48b"],["/tags/Windows/index.html","ff56c5c2a7bbab4c35e95dfebf62559e"],["/tags/ZooKeeper/index.html","f543681538a4366368c0a244319439fe"],["/tags/bfs/index.html","acd26faf1410ac1748244e27f3a0edaa"],["/tags/dfs/index.html","8feeec0f9bdbe8009b85e902ec1c85f4"],["/tags/folium/index.html","d3e9b301c55cb72e3c9fb25a0546fa21"],["/tags/git/index.html","9ed97f5fbc909ccd7e7f446cd25b70c9"],["/tags/index.html","24f7b5bbc8f9017c13d136cc0adbd768"],["/tags/latex/index.html","6a5fe15555aa72bb8002033a812e2f53"],["/tags/中间件/index.html","8e080a17f976a8d717eecd619854dfbd"],["/tags/二分查找/index.html","16578fbc7b6b66106613338a6e5f36b3"],["/tags/优化类/index.html","856223cb9fbaa4231843d093a2e2412d"],["/tags/前端/index.html","2b55e7d3382e608fdf50a267d07728d6"],["/tags/前缀和与差分/index.html","4ebdbfc164781f1aa0e46ebe79f92288"],["/tags/动态规划/index.html","f3cc23ea9f49a262100f5a78d74361ac"],["/tags/动态规划/page/2/index.html","2b39e601de27a5ed438290594647c9a9"],["/tags/博客搭建/index.html","8d6ce5f37fbf677e848d2d7bc82d9e6a"],["/tags/图论/index.html","18ea134701ba2ec6dbb9eb7d46da0f21"],["/tags/大数据/index.html","e26020ac08f39c4e9bef69690785c3e4"],["/tags/大数据/page/2/index.html","f30afc57216d1396f6c4c8292ed409d5"],["/tags/操作系统/index.html","08081b550ee433bed45d6baa0dc4a874"],["/tags/数学建模/index.html","71f23f0bda879e55e3e6533d8991d834"],["/tags/数据库/index.html","88da38e0378d1902a18a1c3b31c8e037"],["/tags/数据结构和算法/index.html","f38d53e34421f8d214ff8ab8fa96fadf"],["/tags/数据结构和算法/page/2/index.html","7cc9bc7971053e0025c956811186c671"],["/tags/数据结构和算法/page/3/index.html","e3b0af5db76db2c5ea38f633ebc161e1"],["/tags/数据结构和算法/page/4/index.html","32477bc7bce764791007b4ec8209512c"],["/tags/数组和字符串/index.html","ba91cb556f736bedd860801b005c1d54"],["/tags/数论/index.html","bd8288bb654923afd3751f90164a0d42"],["/tags/枚举类/index.html","9ef98f730c8f350bc0d73623ee4dff7d"],["/tags/栈和队列/index.html","a0bde483ec9e8c051c21105ceeb06617"],["/tags/树论/index.html","8c32749c4c19965627194170a02e478c"],["/tags/测试/index.html","fa0dcc2f2508046000c2b7d8d1b312c3"],["/tags/环境/index.html","4b94a73d7b6ea04c1e44493c9988d039"],["/tags/环境变量/index.html","727268801f7e7dc77e2b7198f450c1e8"],["/tags/绘图/index.html","90ac385382c2118d0e6f27c07221616f"],["/tags/编程工具/index.html","554a26a197d604d0795ef41c65b4e2d6"],["/tags/编程环境/index.html","53b77eaebefc78879192deea0694987b"],["/tags/网络编程/index.html","b068abe38f52ecbea6b6d629c6e4dc49"],["/tags/英语语法/index.html","3411b6f4a69a877b72a6f21af00bbe15"],["/tags/计算机操作系统/index.html","9ca4e7e6fece6b2d00cf223d3b4ac3bd"],["/tags/论文/index.html","7ab90fb610e84388b227b5280270f167"],["/tags/资源下载/index.html","06c8a766d5e411002d72981f38f2bc1e"],["/tags/链表/index.html","f2f94d0fd4a5fce3932e6561815f4da2"],["/tags/集合/index.html","0ca685d7ef1b7cb43be74c820e72eab2"],["/tags/集群/index.html","e19708158226f451f5f53c031ab4ee4c"]];
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
