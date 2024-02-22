/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e5c92e5ae6a622355a322b391b82af28"],["/about/index.html","1c389677cfce7cdffb8556ed7fcdf700"],["/archives/2023/01/index.html","d65cd05c59cce227b2653bbd7ea7b36e"],["/archives/2023/02/index.html","0f40186b51e740987b9798de60a34827"],["/archives/2023/02/page/2/index.html","9e0cf464020ba017cb330a420580287a"],["/archives/2023/03/index.html","c1b25d5e9172899262875f4d7f328579"],["/archives/2023/05/index.html","6e93b501f571c64f5cef1b1f22a42a19"],["/archives/2023/06/index.html","f6aeff745a60cae05ba19f08ac7b01b7"],["/archives/2023/09/index.html","b0db9055397fb148202512a4ccd1ed85"],["/archives/2023/11/index.html","7770188696ea5099bc011c3778611f74"],["/archives/2023/12/index.html","94118c73173feedb0f41f3d7c6903a98"],["/archives/2023/index.html","535897feebef3c9bd513e153021612d4"],["/archives/2023/page/2/index.html","e93d599df3d2e8605832b63ac044c32d"],["/archives/2023/page/3/index.html","4dcb33e3b7b63609368ebdd05ca6f137"],["/archives/2023/page/4/index.html","c4a013298bc5a257feb2f0eca3ff69f9"],["/archives/2024/02/index.html","4072715ffba8ec594b2fe95c7f8ab174"],["/archives/2024/index.html","febf7af46e5812a2163f9663fe1618de"],["/archives/index.html","80085dd077723222f62e91f37351fef9"],["/archives/page/2/index.html","77a32c99ebc7dbe02787e2cfc1c9dff1"],["/archives/page/3/index.html","50846163f766777012c5aa2882ed2dc5"],["/archives/page/4/index.html","f6a090f9842f82c4e1188a73c974e5c3"],["/baidu_verify_codeva-qQP2iZOMLX.html","39024899ffeffa3cb61c01b1e54f4b1f"],["/categories/Java/index.html","91e03f02e270b1c693fe22bb866481b9"],["/categories/Java/后端/index.html","71aaf2f12420c6a489582b24ef26b5a6"],["/categories/Java/基础/index.html","e88fad59373293512a7ee6290e2cecca"],["/categories/Java/基础/集合/index.html","ac5d30fa2e925d0720d0eee991aa3d87"],["/categories/Python/index.html","999fce84511713464f61bbd6ab1ab145"],["/categories/Python/编程环境/index.html","19b661bc56c4118acc39383295ce796d"],["/categories/R语言/index.html","4cae5db2b104261f3cc5ac945949fb1c"],["/categories/R语言/编程环境/index.html","90485844efbc6ba0051b78dd54f4a227"],["/categories/index.html","2c720ca5c4ae521dbd21c0cc58b3b7c0"],["/categories/中间件/index.html","4e947c7202af68789e129df2feeeb008"],["/categories/前端/Vue/index.html","616b40c09cff3d316d13fccd08cf9533"],["/categories/前端/index.html","69f0d133899737b61429661809af24be"],["/categories/大数据开发/ElasticSearch/index.html","cdcd9d2166d1f2e460ea3c5330904ce7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","98880252146899cc91f0bd456f723fa6"],["/categories/大数据开发/HBase/index.html","090e2fdd8ba8d878f8df37fdaba93217"],["/categories/大数据开发/HBase/学习笔记/index.html","8aa00d48b0fe9f2b12b534605c74d6df"],["/categories/大数据开发/HBase/环境搭建/index.html","0fe40e56786ca16487d3128fb00ad05c"],["/categories/大数据开发/Hadoop/index.html","5930cf7b8b3860f3f4dacbb6049b6ea4"],["/categories/大数据开发/Hadoop/技术/index.html","d9573132fa57c35e5298960f42b927c6"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5b6b3f661fd5c2c4b6655c0645ed77dc"],["/categories/大数据开发/Redis/index.html","aa57e83ae61ed982ca9a2010483c346d"],["/categories/大数据开发/Redis/技术/index.html","82738a8bd795d68b86f4982939967acc"],["/categories/大数据开发/Redis/环境搭建/index.html","d6aba2c8b434e3cec8083b9e5fdfec61"],["/categories/大数据开发/Spark/index.html","a01969566d45f8aa332f05bdc9ead777"],["/categories/大数据开发/Spark/环境搭建/index.html","294ff194fd52dcd3cf999308968249cd"],["/categories/大数据开发/Zookeeper/index.html","3bde5c37a9ec7650f7e3f29157516fac"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","21ba996b89f799b46b122d21c7800082"],["/categories/大数据开发/index.html","9e4e3df4086cc4140a442402142680ef"],["/categories/学校课程/index.html","02b44979991a90c9d9955113e6178ea0"],["/categories/学校课程/计算机操作系统/index.html","03e6f1085dae81a1be8f08f36716796e"],["/categories/操作系统/Linux/index.html","5e5d966c3c474fbac0b4a08524f9dfa5"],["/categories/操作系统/Mac/index.html","06aface18814c1750687d94c54bfb271"],["/categories/操作系统/Windows/index.html","6096dc6a4e67f03ee1c4b4616d21353a"],["/categories/操作系统/index.html","034b1b0fd0fd518eab2c2d5a834a4dc8"],["/categories/数学建模/index.html","9c9e5f7aa81c77c6c7eeebb150fd4df5"],["/categories/数学建模/latex/index.html","39d7319d984275b4a9fcc262f42238c5"],["/categories/数学建模/优化类/index.html","8efd6c822cd58dcc68c3a62a7b3edc5b"],["/categories/数学建模/优化类/现代优化算法/index.html","d32894e37ce5052699d197823ada0ad8"],["/categories/数学建模/优化类/规划类/index.html","bf30b9e5020c80080eb5b73aa5438e17"],["/categories/数学建模/绘图/index.html","f78976ccdfb2d009c196c895b9f9b4e8"],["/categories/数据库/MySQL/index.html","9e2f14ee1958b64dbf3ceeec732144d7"],["/categories/数据库/index.html","1bccd16a508620c3c4d74d75475bc35f"],["/categories/数据结构和算法/index.html","56389a5740e1b97052ae55b3ecb36b6e"],["/categories/数据结构和算法/page/2/index.html","f97ff3c4dd2675b4ebff191c5bc41a20"],["/categories/数据结构和算法/基本原理/bfs/index.html","a638a0eb74de2b34f09914f1f96edd1b"],["/categories/数据结构和算法/基本原理/dfs/index.html","4fd09ba975942505a9a29e11c34a7266"],["/categories/数据结构和算法/基本原理/index.html","be83e4382a82be9e8ce9f0aeb105ff94"],["/categories/数据结构和算法/基本原理/动态规划/index.html","238d0a5019fabbbf125f695f37f9419e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e75ed811c62d71845591dd8e1d98b816"],["/categories/数据结构和算法/基本原理/图论/index.html","f8d63239755d8bdb528f81f2ed603161"],["/categories/数据结构和算法/基本原理/字符串/index.html","e9e04d3dae05c1d5f503b47ca973f83e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a171fee9de8ab6cfd38770c6d096ec73"],["/categories/数据结构和算法/基本原理/数论/index.html","f2e1046c57b0c28f81da780cd5a8f3d4"],["/categories/数据结构和算法/基本原理/树论/index.html","9eb7e22ef5b3c5f981eb261c6e06a2f9"],["/categories/数据结构和算法/基本原理/链表/index.html","aec1f4e6e0261f7df1a8d2f8942b49b8"],["/categories/数据结构和算法/算法题/index.html","78027438116ec747a6c40c88ed6924ba"],["/categories/数据结构和算法/算法题/二分查找/index.html","7a83e443bf5f0087da1e7738fe7b10ac"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","b4509ff47d347663603927a6ec5a8eb0"],["/categories/数据结构和算法/算法题/动态规划/index.html","0949f9f220a78c834961a8871674fc25"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","db11eb77d156079381297fd90f9ad83b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","549af13e329ea44e20c864b4ec3d1e33"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","712b9662a78abf49ee0caed3ea06fae8"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b08706e5248a3108e068efb84f347538"],["/categories/数据结构和算法/算法题/数论/index.html","e82c88f64ecb684cb62e221ecf93d84a"],["/categories/数据结构和算法/算法题/栈和队列/index.html","38f62795c51549e81a0abb55e5ebd699"],["/categories/数据结构和算法/算法题/树论/index.html","b1a943c1b34ea3fc8d9a860a2a3aab87"],["/categories/杂七杂八/index.html","057b11e001a9ed7862ff9a678bd35239"],["/categories/杂七杂八/博客搭建/index.html","0f0f3bfbef17e2ac6069c68500c334d0"],["/categories/编程工具下载/index.html","80f20f0e720613a0c691cd440c4527ed"],["/categories/编程环境/index.html","357c71e45621740bd2776bfd43aaad63"],["/categories/编程环境/大数据/index.html","0955a9253039f0e67c53d77fa262a8bc"],["/categories/英语学习/index.html","a698735a610830dbd74c89f7a062a5fe"],["/categories/英语学习/英语语法/index.html","e49239b8c02570a1651f32c3966e2fdf"],["/comments/index.html","8a546f5d14c5206433bfbdfdd9e51a5b"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","5989aa0ef63914efcfb2b48879b9b7ce"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","550b4845f97027fc8a8c9e52f9c748c5"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4adbb5ce2107696ce24e445c559db1f6"],["/movies/index.html","daa045102155a57cffa640eb9957cf97"],["/music/index.html","3986182661b6ce7eb2f2ca83f03f21ec"],["/page/2/index.html","a1011296f3fada8dd6053711f28364ee"],["/page/3/index.html","ae2c508400bad994856b7c18dc5567ee"],["/page/4/index.html","f84c96768c792fd697ef999d91716e55"],["/page/5/index.html","179ebc9527e09ffa85c62801f65f4bec"],["/page/6/index.html","4b14473651ce9f461f7a9016f8fe9dc9"],["/posts/1021360842.html","75eeeef28487043308e2aa1e9f6b501e"],["/posts/1120620192.html","edfb0e93e609e8806a3a3d7f3d2cd9d0"],["/posts/1141628095.html","7987a7bd5a33f06784a2d024ca3a7160"],["/posts/1168613674.html","df1f9b2531c5315f5145b60f82072418"],["/posts/1219920510.html","4c64ad48eca61ffd6d6ce43977202f6b"],["/posts/1222166338.html","c534df3dc78d4ec753ab93a90be992dc"],["/posts/1259097482.html","63996706cac1aba4c2b9bca43d2a1402"],["/posts/1271036369.html","2c757cf828ed739be02c75aee0a4de0d"],["/posts/1312847445.html","039bcdc4999c8b5d96c54ff5b0075bf9"],["/posts/135355774.html","0292a4a3acc01d3ded31776b4fa6914d"],["/posts/1375344716.html","a88bc49e2036ecd99fa377d4302a8e99"],["/posts/1388991698.html","cfd39a310340ec40efb4422955327270"],["/posts/1410315814.html","5cbce05567a0cf278833887532988321"],["/posts/1452790229.html","2c020fbd8d000bfb8ead96bb0268fbc9"],["/posts/1470079884.html","bf80e2d19a87ffb9d1f2c7e8bd493456"],["/posts/1470079885.html","ce709e7cc8f47f1a58bd80cbbac95369"],["/posts/1470079886.html","90d491beae67f10fadb0fca3d7409cb7"],["/posts/1470079887.html","3b91db870bfd6e990bb5925883c069eb"],["/posts/1498536549.html","f2431ea1ac9532fa3d8280f9ef18ae37"],["/posts/1539568593.html","0cfe3f2751fe7ee0db2621c4db455ad1"],["/posts/1547067935.html","c11ccaa15f28d4d532b4dbce7d640c05"],["/posts/1557866301.html","7150b9ffc4120727cfe80b1579eda7c2"],["/posts/1571776361.html","696871f649e722abac8498649398ea0a"],["/posts/1605124548.html","0f4ef1b9802a5ae48632f1e590c627f8"],["/posts/1633036852.html","8087c34bea5cbfd31a0db0255b050d23"],["/posts/1674202625.html","088416138b4752593fdcff8b10c54fef"],["/posts/1765123828.html","29d8db86ce52a6923deac166e0018ea5"],["/posts/1767336200.html","f9a020e5d44fc99131ad0b0eed14144f"],["/posts/1776114197.html","e2c88fbd4dc3a44afa5d5ef177374eaa"],["/posts/1817748743.html","8a73a35f925dda4b34a2a5bbab1bff47"],["/posts/1925125395.html","4c8d543eb44fb070aaf35140b5789caf"],["/posts/1966191251.html","5bba522f8612c84f569682834fbec3ef"],["/posts/1987617322.html","49f604448781937ff52eaab2023b95e6"],["/posts/1999788039.html","7f405faf39c6f2999ea43bcb3ee2e3eb"],["/posts/2075104059.html","70a0cf5f91b54d794248571ba2566886"],["/posts/2087796737.html","616bbdfea3cc3f07047d023048a070d8"],["/posts/2106547339.html","5de0353f0b023f2bbc1757331ec6fae5"],["/posts/2207806286.html","094a29a79a3e9a0b61cfe1a710a3458e"],["/posts/2225903441.html","ac61ba0182e4287a380287a0969dc8d1"],["/posts/2265610284.html","3327e9789a9b4ac450e097032f904e9f"],["/posts/2281352001.html","7df4b9e280346d2e1d25de3617123b04"],["/posts/2364755265.html","1da750b6a33e966604e331c60d591c1e"],["/posts/2414116852.html","6ef4b8c8f50b2cb40805ed8284cc42bd"],["/posts/2421785022.html","19ce955a8cfe24d5d26e635c25a21091"],["/posts/2482902029.html","744e34f4854a1de2fbbd5865fdfdefab"],["/posts/2495386210.html","b8e5b6cf9116f66cc65f959e748a99ff"],["/posts/2516528882.html","83689797d8500cafeabd9ce89c1bcb34"],["/posts/2526659543.html","713c8583f1ef7fdb83dc7f92f4f08f7f"],["/posts/2529807823.html","4253b998f288ad5262c7f8c1b00915de"],["/posts/2596601004.html","a367dd72a39c612253c582d6d0a4cba6"],["/posts/2697614349.html","bc871315cf39b856a87796ec2c2013b4"],["/posts/2742438348.html","325d7dd59e852d2ad90de7867edc66ea"],["/posts/2768249503.html","dd46b9bf80f77cc31485ac53bd624ec7"],["/posts/2864584994.html","9b4d849bcd9a597bc2545a1b4a3dcae1"],["/posts/2888309600.html","99f36d6f8188d2db540ef41abbcd4f48"],["/posts/2891591958.html","0844f93a47042a7b71c6484eb3e2ef5d"],["/posts/2909934084.html","aec1a763be50edd9cf9cbddc4478a151"],["/posts/2920256992.html","0bbbe9bf14d67d04bc34a55b58008da0"],["/posts/2959474469.html","08b7001d5a4057d140377d4db05710a6"],["/posts/3005926051.html","154abe630f3c97cc1ececcab0922cb8e"],["/posts/309775400.html","b8e4717c9ce90dab3011247a67517b5d"],["/posts/3156194925.html","9e94e321eb2775dabc7241fc75937dbe"],["/posts/3169224211.html","60932f0f9f3dec8e33311097d1dbe243"],["/posts/3213899550.html","c019f80e537ed9baf4d17c27371db62c"],["/posts/3259212833.html","124c67eff355c4c9d86f31e5d5257186"],["/posts/3266130344.html","92fd04aa9072e5a3eb884eb4b15a73c0"],["/posts/3292663995.html","7e6e28f4677f4a5a4e65e58df3dbd212"],["/posts/3297135020.html","df128517b31b28f68cd47d6ba3c0d5b5"],["/posts/3306641566.html","d781f9cdc6021330fe3b190afd794706"],["/posts/3312011324.html","b21b89d317a57acd22dcb6f8768c5f9b"],["/posts/336911618.html","2ce8db6eda1952969fc3dc2999691663"],["/posts/3402121571.html","f9e709e4a2d435e48a4ba2f8923f0a9d"],["/posts/3405577485.html","ae5c745d5ccaa8795abb86b2f9568e4d"],["/posts/3498516849.html","ec67ab687c3304c4faaee46a341e7512"],["/posts/3513711414.html","22622b3e1cb26e50f36b85e5265bd5e9"],["/posts/3523095624.html","08d5f9c941a88acb73eff22da9ac2244"],["/posts/3546711884.html","d0c8ae9f32ea552da4939cbd3d369864"],["/posts/3731385230.html","d97cdb5adc65b62b1dd486b7ca67b3ec"],["/posts/3772089482.html","7529a5530bfc01f50add97c9004bc026"],["/posts/386609427.html","29fe9a76e3f40b11751f800cf3fcc414"],["/posts/4044235327.html","5982b246cd341423fcf94760f42acccc"],["/posts/4115971639.html","c7f847c1c4fec11661f3088a57e2b1f8"],["/posts/4130790367.html","4493c8f2f177bbfc1e739efbad55a608"],["/posts/4131986683.html","dc2291be905e427a6454d87e7c7e8c4f"],["/posts/4177218757.html","9fb774f3ec71fea5c2dc5fd312f9a7e7"],["/posts/4192183953.html","c49c2b8dec00ca651bc0b7dbfdbea6e5"],["/posts/4261103898.html","ffede8f24f503239ed8275d4b25077d0"],["/posts/469711973.html","adefd48cb070e82ca95becd15f6dc721"],["/posts/482495853.html","3a1e65b98d6083a6994a47514dc2b1c3"],["/posts/488247922.html","73dd17ee2a1e5679b12b5fcedc65dc50"],["/posts/517302816.html","96a564d72133ec2d9f74e8cf6e1a25f9"],["/posts/570165348.html","798270c33211a89b29bef2ee49925c7f"],["/posts/595890772.html","9940e77d5f2eacbe9f2eab6c6a4bf5bb"],["/posts/67485572.html","8f9d4f244ddd080892a61d2b8b6dcb89"],["/posts/694347442.html","172ef596e3b946769dbfeb654f6b3910"],["/posts/707384687.html","fe8661d8ce6108bdb7dea7583a9bfcc9"],["/posts/71180092.html","d76c3e097d6bf84613cb7a4cd960224f"],["/posts/716459272.html","fbf98259008e9f1e32b22f414d222097"],["/posts/765481613.html","28c629d8f992604b94b93c99809adfc8"],["/posts/778231993.html","dce813d9d4abd12d9979151c0956e3eb"],["/posts/795397410.html","ae75245bcd787de75431df4f96a848dd"],["/posts/820223701.html","168d618e34e7bb2c6f5d1d16d0e57209"],["/posts/830372185.html","cf8db822e670c6aa07c91c726d51af02"],["/posts/88294277.html","33448a41ee18680e9f8e8040fdb52de2"],["/posts/939963535.html","aab07e40afa5e64bcd28d1e106d711ad"],["/posts/983786067.html","47ade11ac04eeaa6d46ac266e006a3c2"],["/sw-register.js","20ebb252a7cd1bd673c9c2c93c443cf1"],["/tags/C/index.html","95e4fba051db5e660d0db25ea12dc613"],["/tags/C/page/2/index.html","899a282c8592e470c17355ec04467b27"],["/tags/C/page/3/index.html","f7b22fd43b99a6f1ebbb3b80ad2f36c1"],["/tags/C/page/4/index.html","d7a0bb6f341e5df7a941e18d3e8d0339"],["/tags/ETL/index.html","b8d00a5fb64dfec0b33615767d8570be"],["/tags/ElasticSearch/index.html","6f065472b6aac7232bd171f34e0a617a"],["/tags/GUI/index.html","2e7f5cfbbd0d6eb9afc79ec6aa2e921d"],["/tags/HBase/index.html","92867861721e10b449aee861954a7391"],["/tags/Hadoop/index.html","e559fb8f279a94b74bf905ef74287e52"],["/tags/Hadoop/page/2/index.html","e64e74d00754e32b933605b05f2adcc1"],["/tags/Java/index.html","7ad79b1c4ef938a30709e92b2bbe9003"],["/tags/Java后端/index.html","61fba19578be64a71c76bf9f82f5f953"],["/tags/Java后端/page/2/index.html","2def01be4309135ddbc2a99af82f6f7a"],["/tags/Java基础/index.html","7c78e226372bb4ac1ae0beb8ece4c345"],["/tags/Java基础/page/2/index.html","bf1171cf92faa9c5676d14f406f05325"],["/tags/Kettle/index.html","c1e2d7d05d0a9fd478654b17f7c9d59b"],["/tags/Kibana/index.html","363151660b394eada7e4ed4e217aa7e2"],["/tags/Linux/index.html","49cbfe0480b6997856b81a9bb17bfcc6"],["/tags/Linux/page/2/index.html","d7851961f0f7dcc0a566dcf92e7206c5"],["/tags/Linux/page/3/index.html","455ed8942783053371003eaebb08535e"],["/tags/Mac/index.html","2aa827d99126ffc258e7607b91d92a53"],["/tags/Mac/page/2/index.html","c9ea0938752a1cc2fde79894512d11b2"],["/tags/Maven/index.html","4962a021174bcde315a1efa871afd0b1"],["/tags/MySQL/index.html","3b2746cfeadd16e2c4c994301690e452"],["/tags/Python/index.html","4bfa1d6c3e258b198682791af1d613a4"],["/tags/Redis/index.html","d75ce5701144238fb7c5735191814795"],["/tags/R语言/index.html","a630b9bd1a46defe68b98d481525f4cf"],["/tags/Spark/index.html","10a9d0e32b08907a2a8f322b3783da18"],["/tags/Ubuntu/index.html","212e91104ade5153b96bb49ef657e21a"],["/tags/Vue/index.html","c3388549cfbc94c8aa8909fdffa27ada"],["/tags/Windows/index.html","3f240592941eab85251ae8d735dcc298"],["/tags/ZooKeeper/index.html","0bd93b422daf201074a20f849d3597e3"],["/tags/bfs/index.html","c6d20257d77bb95d4a1dc76eab2321e0"],["/tags/dfs/index.html","f2f878dfdadd46abfb12f0fd6c9aae2e"],["/tags/folium/index.html","0136f35658f3f06626be0a6ee2efb062"],["/tags/git/index.html","10651e3c9e1654dfdbb65128889da1d7"],["/tags/index.html","20744e435305bdf18f5c21c8943fdc4a"],["/tags/latex/index.html","d8bc6a67a18c05b830aa8a3b13607e41"],["/tags/中间件/index.html","be9199f235d4e615409c10d9a6be3168"],["/tags/二分查找/index.html","c3421e0f49605cc85aa20e873c785d88"],["/tags/优化类/index.html","cb3d05c3ae0b4c47620ab9310a4f08de"],["/tags/前端/index.html","d111bdfac9566b8b326996bc88c82f8c"],["/tags/前缀和与差分/index.html","b9a4288078742ad486db6212a8747b43"],["/tags/动态规划/index.html","0c83ab81d1fe4276cd482e2378aa1931"],["/tags/动态规划/page/2/index.html","069f87a88baaa36b8f2eeba7d7012d63"],["/tags/博客搭建/index.html","20e907240b3b6b638147faa1bfec743d"],["/tags/图论/index.html","3d8b8e4182794b9323959b1bb7f58a04"],["/tags/大数据/index.html","b076af1392816b3367ba5d34631af6f3"],["/tags/大数据/page/2/index.html","16571d68126cacd13ba417d4bc75052e"],["/tags/操作系统/index.html","abea42635233509226451f88e1a2e5c9"],["/tags/数学建模/index.html","4929a48bc1f0d1574b61972e87039879"],["/tags/数据库/index.html","eab769cd2970b8da599dfcac12f62154"],["/tags/数据结构和算法/index.html","46ade8c6ffcd330e2ee5bd3f45e44066"],["/tags/数据结构和算法/page/2/index.html","ba32eadd88d83d0b7960860c8bb2e3c8"],["/tags/数据结构和算法/page/3/index.html","771679888b24799c3f3e987982d22333"],["/tags/数据结构和算法/page/4/index.html","32085adfd1c6250f7b6b59c042459291"],["/tags/数组和字符串/index.html","ddbac6561614ecd8648724e9a7788d63"],["/tags/数论/index.html","940df53cffdd031d9e93957efc025796"],["/tags/枚举类/index.html","600fcb16b51ed54eda8ba930781719b1"],["/tags/栈和队列/index.html","0053ffff48580492e45def43da3593cd"],["/tags/树论/index.html","99cc5b19f680d465650e1522ea22642c"],["/tags/测试/index.html","7e25feeb733094e12fc28082b51d4517"],["/tags/环境/index.html","59574b697aeeea678a70a26af057c2b0"],["/tags/环境变量/index.html","72014b31320a2cd8357bd587d85506c6"],["/tags/绘图/index.html","c1da2104adbfd74b44b0796db815ba38"],["/tags/编程工具/index.html","66fad70d8653a2605dae72eba39361c1"],["/tags/编程环境/index.html","169ce43bd3d35df36edb6a9dd1a08be9"],["/tags/网络编程/index.html","f0ef46048d35d0de80eaa24b9f4259e7"],["/tags/英语语法/index.html","06e71606ffbee04500405e3b7331731c"],["/tags/计算机操作系统/index.html","6f6bb32cb83ef71b1593f49e9e0e9aea"],["/tags/论文/index.html","f4c5d0525d544e49eb3d27a3ca1603ac"],["/tags/资源下载/index.html","9dc7c24b1497fb1f9f6ef7dbdd932d8e"],["/tags/链表/index.html","2b791065877b15d85aa7ea8fea5f6506"],["/tags/集合/index.html","14592c103b160b380aea9a356142544b"],["/tags/集群/index.html","4d25fbf2105f4b483ff0b018195a3648"]];
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
