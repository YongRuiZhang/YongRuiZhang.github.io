/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","36e8a6193f35c28cc3fdc05372b80b8f"],["/about/index.html","4435204f0ebf8524ba015d5a34a111d0"],["/archives/2023/01/index.html","0d075ced5b630a25866d9b99d6457713"],["/archives/2023/02/index.html","517382bde9e2e620a7733ca467dc89c9"],["/archives/2023/02/page/2/index.html","626c8879a7e0c2349ba9bb27429c109b"],["/archives/2023/03/index.html","7a903de2f27cd4eafcea5554c84ead1c"],["/archives/2023/05/index.html","ab192fbcd5009585d6089a96fb494eb9"],["/archives/2023/06/index.html","5134a978ab1721fb24c949c3d4aee646"],["/archives/2023/09/index.html","7089d9437deba8750e9945b2db0ce8b3"],["/archives/2023/11/index.html","fb0b482501e993aade983aec238391df"],["/archives/2023/12/index.html","8af39e4c4e5b5f8d746514bfbb4ea527"],["/archives/2023/index.html","958afc27ea71263f4710f3eecd07b68c"],["/archives/2023/page/2/index.html","b52e3bd10690b99778d76ff5272e9232"],["/archives/2023/page/3/index.html","d5fc2f291f47b9cd482f9e169ffd4b01"],["/archives/2023/page/4/index.html","d1235241b76b9c8633255ed932181d0a"],["/archives/2024/02/index.html","04aa0001f91a8ac7aeed389e5f830334"],["/archives/2024/index.html","05ea29fd63af5ee6b314a1f144170a52"],["/archives/index.html","04a7ec8b4fec9171d0e0c958e280e530"],["/archives/page/2/index.html","c19491d81241692010fea875afce40a5"],["/archives/page/3/index.html","cae7027e77c4eed32fcdac5eaa2c72b6"],["/archives/page/4/index.html","e09550fd0f4bca2a8e4e8d57f36b6544"],["/baidu_verify_codeva-qQP2iZOMLX.html","1861883a75b1162ddb44933c60cca1e4"],["/categories/Java/index.html","578d4de48f2f52f9a26fcb87d45c1011"],["/categories/Java/后端/index.html","aacd2424b3a833af0bae540ac7df8d3b"],["/categories/Java/基础/index.html","5460ad094a7eed5cfbc9f31ab8e8f6de"],["/categories/Java/基础/集合/index.html","8d9a6501eca66d7ce87796d4ab5f2655"],["/categories/Python/index.html","0d637979b022d7abf87c0f83e8959d0a"],["/categories/Python/编程环境/index.html","81948d71d4c4c99228e918acd5923761"],["/categories/R语言/index.html","74ff2a2c1989b0c4a6e8882e2c844792"],["/categories/R语言/编程环境/index.html","e82fe3548654c63590e36be665656706"],["/categories/index.html","24a2dcccbfea320463ace722dd4dc30f"],["/categories/中间件/index.html","4d4a4c55bbdcb8b0f8545ab5ce4e8151"],["/categories/前端/Vue/index.html","7ac44dd39ffce38504f670c90d426f70"],["/categories/前端/index.html","473b8ed8b6be8a94932ca42176722134"],["/categories/大数据开发/ElasticSearch/index.html","7d777c26ad9244fc76d3dd93bdb1dee9"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","6278a5e517834b3ff8967f3328d77442"],["/categories/大数据开发/HBase/index.html","c89a44fee5bffa63ce379fadf084a669"],["/categories/大数据开发/HBase/学习笔记/index.html","3346ab4d7e66e5edcc351f0274a39944"],["/categories/大数据开发/HBase/环境搭建/index.html","f4f1b8f46d627c44076bf850cc9abf7c"],["/categories/大数据开发/Hadoop/index.html","7298546483f052d07cd2eb3c4372e524"],["/categories/大数据开发/Hadoop/技术/index.html","5e08da679acff0f79d0416102ef016a3"],["/categories/大数据开发/Hadoop/环境搭建/index.html","fb2b9d11da7ccef2dc95a4ae27486c3b"],["/categories/大数据开发/Redis/index.html","56c6f37d2d8e76270935048da898e3dc"],["/categories/大数据开发/Redis/技术/index.html","0babdff46eacac77fa975e33effcda53"],["/categories/大数据开发/Redis/环境搭建/index.html","57100a1b655e52f5320ee5346d4f4491"],["/categories/大数据开发/Spark/index.html","61cc2c1fc50feca787ae8793e5ff64f5"],["/categories/大数据开发/Spark/环境搭建/index.html","088f1798931d5e39509c0a6842a350aa"],["/categories/大数据开发/Zookeeper/index.html","506faf4c11e4fc4b0da18119ffd6eb57"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8d1536fa71e29bdb60bac24ada90e187"],["/categories/大数据开发/index.html","4f301892413f17364c4e8304eb2c03a9"],["/categories/学校课程/index.html","4f7f1a4efe253a9932b5f228e007e1c6"],["/categories/学校课程/计算机操作系统/index.html","c3d289bbf6202e6ac659347b1752ed03"],["/categories/操作系统/Linux/index.html","efd9197d126f25f9ec3057f7000d954a"],["/categories/操作系统/Mac/index.html","5fdb7aadaceddff89330a5ab471925b4"],["/categories/操作系统/Windows/index.html","87cfe06da233b92fa9821a0c599cfcfe"],["/categories/操作系统/index.html","cb9dad80d9ce0ef0e28d2f1279b23c70"],["/categories/数学建模/index.html","8568471e643bb5f3751e7a76fad5ffac"],["/categories/数学建模/latex/index.html","c24fdfc6eac96f098973e3b2a7212170"],["/categories/数学建模/优化类/index.html","33fabbdc33c53190e926a14c173591a2"],["/categories/数学建模/优化类/现代优化算法/index.html","dfb35d48deb5b36b1ac0d5b6d5717181"],["/categories/数学建模/优化类/规划类/index.html","da3e10795efffe0cae04a53fbf7619d4"],["/categories/数学建模/绘图/index.html","81fb38ef8134bc0298c433803761b2b6"],["/categories/数据库/MySQL/index.html","dc9a99d84ff0dc3ed58c4917d9a474f7"],["/categories/数据库/index.html","fd4064bfba2edb2e868587b4fb835245"],["/categories/数据结构和算法/index.html","81c0430f45a0e313320a226a52e67535"],["/categories/数据结构和算法/page/2/index.html","c0dfbda2c2a41208d251004e6616ebc1"],["/categories/数据结构和算法/基本原理/bfs/index.html","e7f1d5abe2f8959886de5f2c2a847c83"],["/categories/数据结构和算法/基本原理/dfs/index.html","8ee3c4e75d0b8d2e4f298029f18a324c"],["/categories/数据结构和算法/基本原理/index.html","6d59d27af47038827eeedb81509a4f35"],["/categories/数据结构和算法/基本原理/动态规划/index.html","947fb3a103d25c3a05916f5ce1bc2656"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","912050fcb6d78d0f1f55bb7b0ad697a0"],["/categories/数据结构和算法/基本原理/图论/index.html","a4c2d543325a9a418fa4ad8440203cc0"],["/categories/数据结构和算法/基本原理/字符串/index.html","68fa2682b789a46f2ed0abe36a421733"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2bdecb22a820559b07c3a74a7e14072a"],["/categories/数据结构和算法/基本原理/数论/index.html","857487cfcfa83924fff8ba04127d2857"],["/categories/数据结构和算法/基本原理/树论/index.html","cfb36654cb7fc7173a83f35dd3e4c963"],["/categories/数据结构和算法/基本原理/链表/index.html","42c5fe571792ff6c9d31a875d4e0ad81"],["/categories/数据结构和算法/算法题/index.html","6b306a1ac7b72de3e57e32850b8ffda4"],["/categories/数据结构和算法/算法题/二分查找/index.html","ac89dade20913a9f6f6e2bd1eb8bc51f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2103e9d7a5129dca90bc2244afc9911b"],["/categories/数据结构和算法/算法题/动态规划/index.html","64a68aabb99beda7a9dfe199673c7758"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","da6e8b1d0d2c2758390782750438489a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","9b6fa77905ee5d3d632f9777cc2a0a8f"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","ba70572724151ee08b0a2bb602444f81"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b1d6964ee4abc4241b5d19d281602f5f"],["/categories/数据结构和算法/算法题/数论/index.html","67d9ae647c06fa86a1e81a0d73d8e6be"],["/categories/数据结构和算法/算法题/栈和队列/index.html","feffc50c02eae5995ae1a3cb1438dc61"],["/categories/数据结构和算法/算法题/树论/index.html","c5096cc8b0d8d526ad4eddd5cdef4acc"],["/categories/杂七杂八/index.html","b0b301af022fe9e787d44c014eced8a1"],["/categories/杂七杂八/博客搭建/index.html","a16827e4357579ecb35c413b433d753b"],["/categories/编程工具下载/index.html","68c17681665497316b4cc7edfa308aee"],["/categories/编程环境/index.html","2bf0f67aeb2d302dc5966f3216290c5e"],["/categories/编程环境/大数据/index.html","5172b79d8092f335ac4eb0458cd2d8f2"],["/categories/英语学习/index.html","e459332f54d78289ff955eb89e708338"],["/categories/英语学习/英语语法/index.html","8166c26743f9d4af961fbe9e7a79e8da"],["/comments/index.html","0534802887118232166b917554c07882"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","6e6122e904b55b8d7a47c44bf5ccd0a7"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","5706c8ff550750d660dc3f6ef90c3e67"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cb6e36a482e18587845bd8764d0c2072"],["/movies/index.html","1654200bcf8e3816e01e5e55d54080db"],["/music/index.html","60ba6fba55df52379170d31d24f83344"],["/page/2/index.html","766bc33f514fbed9e3ed865c0d56c63e"],["/page/3/index.html","e1bd3256d57f46a12351eb4f8e6379ca"],["/page/4/index.html","afc27406b9e21cfb55005bed31e47879"],["/page/5/index.html","fb1d5d0e1b70e4dba09f8035fe255d60"],["/page/6/index.html","6028489cf1a9aaf975bcdb95a2dfbdbd"],["/posts/1021360842.html","5e7ce5b4b5b0329743571fecb401a3a5"],["/posts/1120620192.html","c38a33ab4f1f16305e13e455c788af65"],["/posts/1141628095.html","365bd4244b2ad666e9b3b1ef5dec1a77"],["/posts/1168613674.html","2ac36c848a72034808855436ee353b57"],["/posts/1219920510.html","31a74f61af56c192bfa3868675286431"],["/posts/1222166338.html","31bf2c851976c2cd0251486e50fc9f8e"],["/posts/1259097482.html","1a5ea0fd2ac672eb001bec0dbff1cabe"],["/posts/1271036369.html","33814a23b114db661dbfc741a0d12c24"],["/posts/1312847445.html","9962eb2e91f3ea143901c0f5a9836c3a"],["/posts/135355774.html","74002a04ae87e21db64d4ff34c70a6a7"],["/posts/1375344716.html","4b285305df80625de2c1f472271100da"],["/posts/1388991698.html","c068f5d5cba30546f1fa2a6fc1f6c347"],["/posts/1410315814.html","ab7f22b98df2ffa4b796c5f690d76eaf"],["/posts/1452790229.html","4b47e4026a03783a9bb46398c936eb0b"],["/posts/1470079884.html","d67293801b95f670d29ae9a8d7767f29"],["/posts/1470079885.html","3c76e112c6a7d464c201f69efe8565e0"],["/posts/1470079886.html","df44fc98bcb79c3b0e15d4da2cc7630c"],["/posts/1470079887.html","d30b43ff71baf8ee3c81824dd5cc4162"],["/posts/1498536549.html","b1b49ca9600a812fa2b587063027ef03"],["/posts/1539568593.html","42a73bcbb54f0a754e063b55b86caf6c"],["/posts/1547067935.html","8883b2b28f5c8e2904b9d2fec68863e8"],["/posts/1557866301.html","b5cba390e5f470c65569d7dfa79a19d5"],["/posts/1571776361.html","3b8ab18450e23cf980fc5dd392b1cd24"],["/posts/1605124548.html","61eef6f8beccdefd7cd928650ade4125"],["/posts/1633036852.html","345076777d8931caeb09d17880d53dad"],["/posts/1674202625.html","4374cb2cb48dc67f3ad55ffdc727b60f"],["/posts/1765123828.html","8398190976c7cca576a83502f7790980"],["/posts/1767336200.html","d82d3096f4f2bce996c23ee2c2185015"],["/posts/1776114197.html","e6e61deac61655968640f6124f3397bb"],["/posts/1817748743.html","245f6f273ca85fba3624233b749a6f23"],["/posts/1925125395.html","0f38d91c2e326939fcc807faa7cdf7a2"],["/posts/1966191251.html","ffb699e0a33b0d726b2ac31fddf04548"],["/posts/1987617322.html","b0583c0d76008274eee3d113129e5c6b"],["/posts/1999788039.html","8de3f59b580b649dabcabd03e0cb7244"],["/posts/2075104059.html","58d9f0acaa63006b9a4b2a11c6a20413"],["/posts/2087796737.html","090dbc8a56efd1c215292549087a2165"],["/posts/2106547339.html","4cd50bcfa20685962d6b9178a8613fb8"],["/posts/2207806286.html","917c353a17cea6e4d8a2d531a4672ef9"],["/posts/2225903441.html","5b3087c075049b8e720b586cbfe356e7"],["/posts/2265610284.html","c49d47e23822f29ae245124ff78563e4"],["/posts/2281352001.html","d0a090351a739f2236c176dcf8f4b94c"],["/posts/2364755265.html","db74fea696b339711b722eb568792f89"],["/posts/2414116852.html","5ee0ac4cdd78fa5e2dd4c0b7f1f84716"],["/posts/2421785022.html","337280f7dad4fa16662a5d3d7a962d4b"],["/posts/2482902029.html","1c27a92272f0587804ddf666c6276077"],["/posts/2495386210.html","dd874394c9dc6c22c802380f30740972"],["/posts/2516528882.html","8e71c56c8a0dbaa362f309ab8f454b98"],["/posts/2526659543.html","2d697ea76451b64ff013c2e62fb2012b"],["/posts/2529807823.html","773260190c7e1085bafed73be0b4497a"],["/posts/2596601004.html","54512184ae641e30ef784091e7eae47d"],["/posts/2697614349.html","19be5568c0aad2ada6cf1e2034508523"],["/posts/2742438348.html","8839deb3818da2455842895d019d44d6"],["/posts/2768249503.html","9b6a5b20b32a184d0ffee3018f33a17b"],["/posts/2864584994.html","75ff35edd6e65cc589cf67855583d281"],["/posts/2888309600.html","d2e376d77a9366f15b55568ce46ac8d0"],["/posts/2891591958.html","909b646a4af79d71f8af7d698f02f3a6"],["/posts/2909934084.html","d6160712958f36446bf28b8a3e2aef9e"],["/posts/2920256992.html","39ebcb8dea1fe66c0a670e9786b062b0"],["/posts/2959474469.html","595b368b99290eca8a72411c1c3da2d0"],["/posts/3005926051.html","4c64583d4e9319fd5336cc673f519a92"],["/posts/309775400.html","c2e789766ad0edc744784c028ffb261e"],["/posts/3156194925.html","6843fb929e175af36245e1b424ebf955"],["/posts/3169224211.html","524505ad53b7b8f7b5db4fee70f680c3"],["/posts/3213899550.html","37bd317d2acba2533031ee2b3ae7f650"],["/posts/3259212833.html","6ee6689322651eb331b88a0264b4528f"],["/posts/3266130344.html","0d0fde26103194d9ee5cbfb8929fdf39"],["/posts/3292663995.html","ba4a691b45b94e2ad423a034667d5bfe"],["/posts/3297135020.html","b5751db64ad6fca1da1e102d3fcd36fd"],["/posts/3306641566.html","36cd719de4d622909318b3810db0d952"],["/posts/3312011324.html","d2d0ce3fdb36954f2d00cbdb4152032e"],["/posts/336911618.html","79b33602a1270d6963ac3ea2fcc1533d"],["/posts/3402121571.html","bfb094ace871199879faf323418bdb36"],["/posts/3405577485.html","a52f560b0860c2dc6ee6036eb590166d"],["/posts/3498516849.html","6f10b46aac979aed4e86f5fc960d781c"],["/posts/3513711414.html","31ec551ebd0dda3577464171c384d31b"],["/posts/3523095624.html","67c00956a25c9d886f44476832110eb3"],["/posts/3546711884.html","996c24cea1cfb38f825e44ae4976081b"],["/posts/3731385230.html","dc9adca8182d2b1571490b04198c87b0"],["/posts/3772089482.html","79d224e663f88be77d2591489d62b63e"],["/posts/386609427.html","bcecc183b21463104dda378c64f173f1"],["/posts/4044235327.html","a22a4a1f81ca5f0a414b95a8d59e428d"],["/posts/4115971639.html","b3845f1fec7273f4ec998ee2ea19ab94"],["/posts/4130790367.html","7cd7b5d77f56864a91d6d825df832a06"],["/posts/4131986683.html","e378e704e3a39dcf27c6ee7ced289224"],["/posts/4177218757.html","3af3430bd0f51bc78447341d32f3c47f"],["/posts/4192183953.html","6c0563e0b0c06fa21c8717e03ec35331"],["/posts/4261103898.html","edaca398e37bc72c1c17d8d1cb5b93f8"],["/posts/469711973.html","507f7b919b5e9fc7b59553642c2eaa93"],["/posts/482495853.html","01f7d5d32fb73c9d976454ef7c52ed30"],["/posts/488247922.html","222a03e829f9fde0e6882b9a65b85ac0"],["/posts/517302816.html","4e61d3816fe1fa91a05339de58dd9016"],["/posts/570165348.html","04460b1151c86d2d2f008d11a82f251f"],["/posts/595890772.html","fb77b8c26bc0b8b26a3ab0e011d77225"],["/posts/67485572.html","d2c5078c4f4485097e9258c4b1221d9d"],["/posts/694347442.html","7baad0e488f63b81ed3fd9fcddc277a6"],["/posts/707384687.html","7d3bbb0f7f52ea00259b71597b04245d"],["/posts/71180092.html","782e5fb2ed7b144af9b8e72d631d41a9"],["/posts/716459272.html","a558875e9e0e40ddf510ee6b96798c11"],["/posts/765481613.html","668d1bbfdad92062f4f47df7e7e57d93"],["/posts/778231993.html","8a8f06b7a47f496a3050cd804f269981"],["/posts/795397410.html","04b1f7f2e9904f43aa34fa0bc25a10c8"],["/posts/820223701.html","45b2dc765903d754b485bb97157fb140"],["/posts/830372185.html","8b1d0a457eadd1574e5ce63fd8457f47"],["/posts/88294277.html","a33ea9aeb2931a12532147c5ad5dbe92"],["/posts/939963535.html","edf278b38f6655b4f9d70d4afd133a22"],["/posts/983786067.html","86f3de4f460de82d0de257460e5a1d94"],["/sw-register.js","40c9c99d674708d2ba5406cd1db589e6"],["/tags/C/index.html","dd81f8c000f9fd1fc6b22d0d290924d1"],["/tags/C/page/2/index.html","cdbe7ce883722a29d83e7d2e14e951b0"],["/tags/C/page/3/index.html","123610c86ab7a8dff5fbdc91c8d081ab"],["/tags/C/page/4/index.html","8743d033282e4af6200bc5a9340ce9a4"],["/tags/ETL/index.html","24ca81045554a3ec525f72559644a50c"],["/tags/ElasticSearch/index.html","76227380ec2929f4130a8b56d30f26c5"],["/tags/GUI/index.html","622804c63b3932633c21e55d8f66a7b9"],["/tags/HBase/index.html","2528b6501aba8762f0c3a7579c13264c"],["/tags/Hadoop/index.html","08ee6a7ec14ce19057700d8f2ffb0464"],["/tags/Hadoop/page/2/index.html","60c2381b96be18700bafbae3b217cf6b"],["/tags/Java/index.html","18bfe2676c1cf031172e3a5e7cf20ee7"],["/tags/Java后端/index.html","b79ca8fd4ee6ca0d15b8340d34bb47bd"],["/tags/Java后端/page/2/index.html","59a28da087ff54e8f884f343fcbe7fbb"],["/tags/Java基础/index.html","533d8365f7f85a6e6b26b55f6e5bcd89"],["/tags/Java基础/page/2/index.html","a713d6ad9494c53ac51e69bc964a5f1a"],["/tags/Kettle/index.html","0c3d749d3e29599e99f8d8929fc97321"],["/tags/Kibana/index.html","5767324a63d527a7785b57ddddd786d3"],["/tags/Linux/index.html","5b8b4a6a290bc6c90bc0c23d9280eb1c"],["/tags/Linux/page/2/index.html","dab0ed669f4eaf907104edafa67ef2e7"],["/tags/Linux/page/3/index.html","eb7fe77e9207a0504aa84cf43e8e96f3"],["/tags/Mac/index.html","1ad67ca7ad782f39a5cfb0a20c0f7755"],["/tags/Mac/page/2/index.html","81d02c79819566635f5b394b5898f35a"],["/tags/Maven/index.html","f5ec1f891b192032a658f32924d49242"],["/tags/MySQL/index.html","1048fb813eb065e2a47909751b970698"],["/tags/Python/index.html","36166b29c037aca26354fdae2d1b2aa5"],["/tags/Redis/index.html","7a24684750641d4d530cb33b49fc150b"],["/tags/R语言/index.html","58ca566c7b5b1fb7d6240779d21bb828"],["/tags/Spark/index.html","c48970d78e378064f3e2fa5116852858"],["/tags/Ubuntu/index.html","7ca3ae1aafac8895c55d1c895c9064c4"],["/tags/Vue/index.html","f5fbe8579872b319ef9eb19cc181600c"],["/tags/Windows/index.html","00b39520e2597b163c90be920cd93300"],["/tags/ZooKeeper/index.html","eea37ecb504dfc06a07b3df74a0a2c67"],["/tags/bfs/index.html","16ff24e88ccef1166def8eac4301d225"],["/tags/dfs/index.html","5b7558a26b258f442c54cb5641431928"],["/tags/folium/index.html","c477ab52dec504f84a6d46da6d232bb7"],["/tags/git/index.html","d3b79c37a5de0c786815c3016e0cb68e"],["/tags/index.html","5a015f492962209ac943cfdb1a742bad"],["/tags/latex/index.html","7d064233c3089ca4a7d0e996cbda5571"],["/tags/中间件/index.html","7a54a269915d6511d98d822244b45dae"],["/tags/二分查找/index.html","a32975f58a44602342892a802acb1b58"],["/tags/优化类/index.html","cdcd89c0744af7abcef2cf919da88875"],["/tags/前端/index.html","341a9886f4b3429d3bf47dd3613b71d1"],["/tags/前缀和与差分/index.html","94a8f7958ddb6cfb4802149d8bac4ac5"],["/tags/动态规划/index.html","85827e49e76f531f24bee33349abef66"],["/tags/动态规划/page/2/index.html","18128047cef11eac22eb17b15aaf6918"],["/tags/博客搭建/index.html","2bd3b4979d5bca1af5723e8ce5fe6adf"],["/tags/图论/index.html","cfb75c9dd3af5f82d60fb596ac60f194"],["/tags/大数据/index.html","d561ddfc7145c43566e48e480863cbf1"],["/tags/大数据/page/2/index.html","d8e434cc7b1c262fbf15b8f9b52ee24a"],["/tags/操作系统/index.html","8abc8c5533f13412cb636fd114604a75"],["/tags/数学建模/index.html","e5e12af8331bd31e6b85287f14a91b53"],["/tags/数据库/index.html","bebdac19d8869355e81ce5d3d1c99766"],["/tags/数据结构和算法/index.html","33d3aefc3961cc722a58bb3ca17e24e2"],["/tags/数据结构和算法/page/2/index.html","e0b15dcbf6a419a6d0a385701739dc0e"],["/tags/数据结构和算法/page/3/index.html","9452e45fc1ffdbb07c46cd5bc9230cda"],["/tags/数据结构和算法/page/4/index.html","ac2b5f85715f0780dc456a62ebecd686"],["/tags/数组和字符串/index.html","2c6a1451227a7a0c3e38d962508952b4"],["/tags/数论/index.html","24383ac33bd13c881599df418add7602"],["/tags/枚举类/index.html","ab0eafff4222a3f87f990d515223c717"],["/tags/栈和队列/index.html","498fa22b7e12104e8cea02fb4c9d64b3"],["/tags/树论/index.html","68ed8627fd66360ae0aa5ece80626960"],["/tags/测试/index.html","20d10e25452b26b773c812eb774e6a35"],["/tags/环境/index.html","74b9ce3c0be1ebbeb6b4c9d18945f598"],["/tags/环境变量/index.html","e823438585e1ffa9fa07f5e1678ffbb5"],["/tags/绘图/index.html","4891f442fc9beff1896b8bc4c6a1e204"],["/tags/编程工具/index.html","63304647560960bc04b2b7ea1dff52f0"],["/tags/编程环境/index.html","d5f2fc981c9f1b0c2493445c51dab319"],["/tags/网络编程/index.html","a345936c603d37758207b0b8f95abec3"],["/tags/英语语法/index.html","96c71e7796a85c6f694e97b84907cf8f"],["/tags/计算机操作系统/index.html","74900f02df98a809e8c6804f4370aac1"],["/tags/论文/index.html","7c618aa063b9bd4f09af09dae895ba95"],["/tags/资源下载/index.html","068943785814cb698968532befe79b0c"],["/tags/链表/index.html","a7813191c6acbbf5180813ac3b08cc1f"],["/tags/集合/index.html","7ed6c0d9ed53ca6fd384755240786cca"],["/tags/集群/index.html","746fbedcab536a715031bb5000bb7fc7"]];
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
