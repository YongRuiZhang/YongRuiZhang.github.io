/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","87d75f5a44ba6f538e69ecbe0a70db13"],["/about/index.html","4435204f0ebf8524ba015d5a34a111d0"],["/archives/2023/01/index.html","3b53404187fa6d34da373c83de3df279"],["/archives/2023/02/index.html","5536e5e6dc306f091544117ef11d2c5e"],["/archives/2023/02/page/2/index.html","7a2eedc08bf253891138bcc1a6419d22"],["/archives/2023/03/index.html","238a6a665f36f94fc8930511a322a23d"],["/archives/2023/05/index.html","4b6cee31015959225abfc196e7346303"],["/archives/2023/06/index.html","ffdd42e3834a7c33580dbb03b4db3755"],["/archives/2023/09/index.html","b846d47c977e2edca734d2aded7d2dd7"],["/archives/2023/11/index.html","6801571c3b70f7ea933b643afe62b59e"],["/archives/2023/12/index.html","ebfcf9a0e820cf07ab872de1d47aaa15"],["/archives/2023/index.html","92c702bc4eec6120eff74cd8bc3228f1"],["/archives/2023/page/2/index.html","342d1198e49674ba90a77453f8e225f1"],["/archives/2023/page/3/index.html","ecee8d8a86e819090908b808eda92b97"],["/archives/2023/page/4/index.html","a908208809415a56ec0258c09d8ace59"],["/archives/2024/02/index.html","54c2c31e507bf47bfad2cc52bcafadf8"],["/archives/2024/index.html","b1858fc38c2c57f9cd86008f7dee79ff"],["/archives/index.html","1f94417ac5d1a8582f4384de38db0be8"],["/archives/page/2/index.html","27279e1aca885b65a50c750783e7be40"],["/archives/page/3/index.html","5f59906b8f5fee37c06253cce16194d5"],["/archives/page/4/index.html","245ec95aa0092775c224421497c893e6"],["/baidu_verify_codeva-qQP2iZOMLX.html","3a05b474494a1b5441d4fbeb474983c7"],["/categories/Java/index.html","22d5a59852a30850ec213d566d8e3741"],["/categories/Java/后端/index.html","47ec0115ef492d704bb7aae79e01ac44"],["/categories/Java/基础/index.html","276a668988d604524c39479996dc34ac"],["/categories/Java/基础/集合/index.html","4797f9d9faedb08cc5cf07f50be3802d"],["/categories/Python/index.html","16d6c29dac53a07a34be0c2174efea20"],["/categories/Python/编程环境/index.html","90498c6a60a76c792500be39be64d955"],["/categories/R语言/index.html","3b40863e8b0e79d6673f7f6976568b22"],["/categories/R语言/编程环境/index.html","b6dd55070d2a50b212ddc80000c7f4c5"],["/categories/index.html","24a2dcccbfea320463ace722dd4dc30f"],["/categories/中间件/index.html","ddb0114197f28252f0eb5b30bbb5445d"],["/categories/前端/Vue/index.html","0efb4f09c5fe539de2d3829c7a8b71f3"],["/categories/前端/index.html","15ccba6f0a19ba39cb0daa7fe0f90c4b"],["/categories/大数据开发/ElasticSearch/index.html","6942364278b5b351dea5173195dd5216"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","66ad6623641e109b74c8e014371e1002"],["/categories/大数据开发/HBase/index.html","e28d90703b33e7e805610a3e374e7e0e"],["/categories/大数据开发/HBase/学习笔记/index.html","e79cf94b7856a49d085455c4c2af59a7"],["/categories/大数据开发/HBase/环境搭建/index.html","9ea89bbda59a9fc8889e2b9ec2cb25b1"],["/categories/大数据开发/Hadoop/index.html","bf4d510f937c3c082b63ed5b143e6c48"],["/categories/大数据开发/Hadoop/技术/index.html","edc7573d16edaf3a1eecd3458fb053b8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","cfe583f2d752eb6065efedfe719bdf3c"],["/categories/大数据开发/Redis/index.html","299a6fcd5b251c745944dfbfb249b38f"],["/categories/大数据开发/Redis/技术/index.html","cfe163150482b5ef9ebfc3def2f2cdd6"],["/categories/大数据开发/Redis/环境搭建/index.html","871c38e23a1aea627157106829a431ce"],["/categories/大数据开发/Spark/index.html","531a0d0d552ebdc076d7a6e6c34680da"],["/categories/大数据开发/Spark/环境搭建/index.html","51c55d74364341fa9f7c4a667f8f417e"],["/categories/大数据开发/Zookeeper/index.html","13a862de335d25aa11c3f8fda64dbf8b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","d3a2dd72601ed15de596d1d2e33e02b5"],["/categories/大数据开发/index.html","efc686f1e638d99077e115f6ff7769ce"],["/categories/学校课程/index.html","d30dbc50e5f034a2b824b8b2322f3ed9"],["/categories/学校课程/计算机操作系统/index.html","641fd04a7bb90b0cb070288cf73e4252"],["/categories/操作系统/Linux/index.html","073e2e357f9a800cd211e45f9eb0ac65"],["/categories/操作系统/Mac/index.html","1e71d40bc7464141dc20211997fe32cd"],["/categories/操作系统/Windows/index.html","47fdaafa38f16dde0ead5625ddb46e42"],["/categories/操作系统/index.html","17d3065bdc573b355286c9be96d0f81a"],["/categories/数学建模/index.html","05dfc155acb999d827b2ea89954fc417"],["/categories/数学建模/latex/index.html","e2a1ecd4ef61b38107e7b64b96935213"],["/categories/数学建模/优化类/index.html","f82e3d52c810b202b23387dc7f81e44f"],["/categories/数学建模/优化类/现代优化算法/index.html","cc85eb76c5771a40612ce98473dc4657"],["/categories/数学建模/优化类/规划类/index.html","7b136f44a9d72e60a66a319ebbc26b13"],["/categories/数学建模/绘图/index.html","798940cc2269e332c6ad8cbee39db60b"],["/categories/数据库/MySQL/index.html","32bd9c7074122c525494506109d10fdb"],["/categories/数据库/index.html","e9d829fccbd601fba0ee66dca3fc668f"],["/categories/数据结构和算法/index.html","3283aa1064a5b73989d843c4c3a8b89e"],["/categories/数据结构和算法/page/2/index.html","361f8f10c7b04f7d03ff099315faee71"],["/categories/数据结构和算法/基本原理/bfs/index.html","f277d83e8c766c85b441b7f9a7802604"],["/categories/数据结构和算法/基本原理/dfs/index.html","f4a7b387a09da910cf221324662ff41a"],["/categories/数据结构和算法/基本原理/index.html","33c2ff8c79dabad2298a0c89054d3429"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c346e8701501b29caecb990ee70cc41e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4dce84c420a3e8cf973587c8a3138dde"],["/categories/数据结构和算法/基本原理/图论/index.html","2c713e9ffacc27995bf3f01b8acfc771"],["/categories/数据结构和算法/基本原理/字符串/index.html","492147e92a3f27bac43eabd2e91323f4"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","f368543094581a2cbc8181410f023fba"],["/categories/数据结构和算法/基本原理/数论/index.html","602ecf83b9f4a85f502dfe62675d68c3"],["/categories/数据结构和算法/基本原理/树论/index.html","12afb9893b22e288d28f273451b72869"],["/categories/数据结构和算法/基本原理/链表/index.html","52d78f97c7e11de6cd2662f9e389b53b"],["/categories/数据结构和算法/算法题/index.html","31c2160f4ccce7aa40912f4be7e3bc24"],["/categories/数据结构和算法/算法题/二分查找/index.html","4ac993e71f81db541e53e97c6a44210b"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0fc7c05707b37f098a68d7caad9c4bfa"],["/categories/数据结构和算法/算法题/动态规划/index.html","e6eb7367b50b35dfffe751c02dd98f8c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","6a530ca3fdc6feb30e64d68879989ee9"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","7cfc36a868e553401ba0a675c0515c25"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","57ba764afb5806f772e21e87c3024de7"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","f9cb165130fe32e85c9779d882728cb2"],["/categories/数据结构和算法/算法题/数论/index.html","3bd1878199652dcf719317e46c369da6"],["/categories/数据结构和算法/算法题/栈和队列/index.html","218739e836b9a5b56cda5d2881eb37f7"],["/categories/数据结构和算法/算法题/树论/index.html","ec45acc974e929086d4d27f9fedce1ae"],["/categories/杂七杂八/index.html","b087454637d2f57678ada0bbe6cb548c"],["/categories/杂七杂八/博客搭建/index.html","55f322701a6c57fd42af5b03d0e66c38"],["/categories/编程工具下载/index.html","250725151cadd19e766046e573bebddd"],["/categories/编程环境/index.html","c4b9940eb4114bc54c375e0c06e3ddf9"],["/categories/编程环境/大数据/index.html","006045231690b801380d14a1a47cd94a"],["/categories/英语学习/index.html","2a8a5f080166b095ef1ac56658b036b3"],["/categories/英语学习/英语语法/index.html","8b4e5ebe0e1a66c500cc251742813dd3"],["/comments/index.html","5ee4b0126e3b52a60974dac57758a446"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","47e99f1392cf5acf7436d6f6b3e8c663"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1fe7b9591d38a99e09327761db288ed3"],["/movies/index.html","62d641a625fb1ae7fe8a34883b71db9b"],["/music/index.html","be36a3994de1ea367686422a896a06de"],["/page/2/index.html","45eb70ae54b8e45113f6e16ba9f0eb33"],["/page/3/index.html","28cb2d47b0a5643d2a23415b3b3d6612"],["/page/4/index.html","bf6cf6201e04593e2ff422b7cfbb5a44"],["/page/5/index.html","398dbc3d4989cdc277a9a8d3a456b682"],["/page/6/index.html","e79b66a5647e8db903531819e338edd5"],["/posts/1021360842.html","5e7ce5b4b5b0329743571fecb401a3a5"],["/posts/1120620192.html","c38a33ab4f1f16305e13e455c788af65"],["/posts/1141628095.html","365bd4244b2ad666e9b3b1ef5dec1a77"],["/posts/1168613674.html","2ac36c848a72034808855436ee353b57"],["/posts/1219920510.html","31a74f61af56c192bfa3868675286431"],["/posts/1222166338.html","31bf2c851976c2cd0251486e50fc9f8e"],["/posts/1259097482.html","1a5ea0fd2ac672eb001bec0dbff1cabe"],["/posts/1271036369.html","33814a23b114db661dbfc741a0d12c24"],["/posts/1312847445.html","9962eb2e91f3ea143901c0f5a9836c3a"],["/posts/135355774.html","74002a04ae87e21db64d4ff34c70a6a7"],["/posts/1375344716.html","4b285305df80625de2c1f472271100da"],["/posts/1388991698.html","c068f5d5cba30546f1fa2a6fc1f6c347"],["/posts/1410315814.html","ab7f22b98df2ffa4b796c5f690d76eaf"],["/posts/1452790229.html","4b47e4026a03783a9bb46398c936eb0b"],["/posts/1470079884.html","d67293801b95f670d29ae9a8d7767f29"],["/posts/1470079885.html","3c76e112c6a7d464c201f69efe8565e0"],["/posts/1470079886.html","df44fc98bcb79c3b0e15d4da2cc7630c"],["/posts/1470079887.html","d30b43ff71baf8ee3c81824dd5cc4162"],["/posts/1498536549.html","b1b49ca9600a812fa2b587063027ef03"],["/posts/1539568593.html","42a73bcbb54f0a754e063b55b86caf6c"],["/posts/1547067935.html","8883b2b28f5c8e2904b9d2fec68863e8"],["/posts/1557866301.html","b5cba390e5f470c65569d7dfa79a19d5"],["/posts/1571776361.html","3b8ab18450e23cf980fc5dd392b1cd24"],["/posts/1605124548.html","61eef6f8beccdefd7cd928650ade4125"],["/posts/1633036852.html","345076777d8931caeb09d17880d53dad"],["/posts/1674202625.html","4374cb2cb48dc67f3ad55ffdc727b60f"],["/posts/1765123828.html","8398190976c7cca576a83502f7790980"],["/posts/1767336200.html","d82d3096f4f2bce996c23ee2c2185015"],["/posts/1776114197.html","e6e61deac61655968640f6124f3397bb"],["/posts/1817748743.html","245f6f273ca85fba3624233b749a6f23"],["/posts/1925125395.html","0f38d91c2e326939fcc807faa7cdf7a2"],["/posts/1966191251.html","ffb699e0a33b0d726b2ac31fddf04548"],["/posts/1987617322.html","b0583c0d76008274eee3d113129e5c6b"],["/posts/1999788039.html","8de3f59b580b649dabcabd03e0cb7244"],["/posts/2075104059.html","58d9f0acaa63006b9a4b2a11c6a20413"],["/posts/2087796737.html","090dbc8a56efd1c215292549087a2165"],["/posts/2106547339.html","4cd50bcfa20685962d6b9178a8613fb8"],["/posts/2207806286.html","917c353a17cea6e4d8a2d531a4672ef9"],["/posts/2225903441.html","5b3087c075049b8e720b586cbfe356e7"],["/posts/2265610284.html","c49d47e23822f29ae245124ff78563e4"],["/posts/2281352001.html","d0a090351a739f2236c176dcf8f4b94c"],["/posts/2364755265.html","db74fea696b339711b722eb568792f89"],["/posts/2414116852.html","5ee0ac4cdd78fa5e2dd4c0b7f1f84716"],["/posts/2421785022.html","337280f7dad4fa16662a5d3d7a962d4b"],["/posts/2482902029.html","1c27a92272f0587804ddf666c6276077"],["/posts/2495386210.html","dd874394c9dc6c22c802380f30740972"],["/posts/2516528882.html","8e71c56c8a0dbaa362f309ab8f454b98"],["/posts/2526659543.html","2d697ea76451b64ff013c2e62fb2012b"],["/posts/2529807823.html","773260190c7e1085bafed73be0b4497a"],["/posts/2596601004.html","54512184ae641e30ef784091e7eae47d"],["/posts/2697614349.html","19be5568c0aad2ada6cf1e2034508523"],["/posts/2742438348.html","8839deb3818da2455842895d019d44d6"],["/posts/2768249503.html","9b6a5b20b32a184d0ffee3018f33a17b"],["/posts/2864584994.html","75ff35edd6e65cc589cf67855583d281"],["/posts/2888309600.html","d2e376d77a9366f15b55568ce46ac8d0"],["/posts/2891591958.html","909b646a4af79d71f8af7d698f02f3a6"],["/posts/2909934084.html","d6160712958f36446bf28b8a3e2aef9e"],["/posts/2920256992.html","39ebcb8dea1fe66c0a670e9786b062b0"],["/posts/2959474469.html","595b368b99290eca8a72411c1c3da2d0"],["/posts/3005926051.html","4c64583d4e9319fd5336cc673f519a92"],["/posts/309775400.html","c2e789766ad0edc744784c028ffb261e"],["/posts/3156194925.html","6843fb929e175af36245e1b424ebf955"],["/posts/3169224211.html","524505ad53b7b8f7b5db4fee70f680c3"],["/posts/3213899550.html","37bd317d2acba2533031ee2b3ae7f650"],["/posts/3259212833.html","6ee6689322651eb331b88a0264b4528f"],["/posts/3266130344.html","0d0fde26103194d9ee5cbfb8929fdf39"],["/posts/3292663995.html","ba4a691b45b94e2ad423a034667d5bfe"],["/posts/3297135020.html","b5751db64ad6fca1da1e102d3fcd36fd"],["/posts/3306641566.html","36cd719de4d622909318b3810db0d952"],["/posts/3312011324.html","d2d0ce3fdb36954f2d00cbdb4152032e"],["/posts/336911618.html","79b33602a1270d6963ac3ea2fcc1533d"],["/posts/3402121571.html","bfb094ace871199879faf323418bdb36"],["/posts/3405577485.html","a52f560b0860c2dc6ee6036eb590166d"],["/posts/3498516849.html","6f10b46aac979aed4e86f5fc960d781c"],["/posts/3513711414.html","31ec551ebd0dda3577464171c384d31b"],["/posts/3523095624.html","67c00956a25c9d886f44476832110eb3"],["/posts/3546711884.html","996c24cea1cfb38f825e44ae4976081b"],["/posts/3731385230.html","dc9adca8182d2b1571490b04198c87b0"],["/posts/3772089482.html","79d224e663f88be77d2591489d62b63e"],["/posts/386609427.html","bcecc183b21463104dda378c64f173f1"],["/posts/4044235327.html","a22a4a1f81ca5f0a414b95a8d59e428d"],["/posts/4115971639.html","b3845f1fec7273f4ec998ee2ea19ab94"],["/posts/4130790367.html","7cd7b5d77f56864a91d6d825df832a06"],["/posts/4131986683.html","e378e704e3a39dcf27c6ee7ced289224"],["/posts/4177218757.html","3af3430bd0f51bc78447341d32f3c47f"],["/posts/4192183953.html","6c0563e0b0c06fa21c8717e03ec35331"],["/posts/4261103898.html","edaca398e37bc72c1c17d8d1cb5b93f8"],["/posts/469711973.html","507f7b919b5e9fc7b59553642c2eaa93"],["/posts/482495853.html","01f7d5d32fb73c9d976454ef7c52ed30"],["/posts/488247922.html","222a03e829f9fde0e6882b9a65b85ac0"],["/posts/517302816.html","4e61d3816fe1fa91a05339de58dd9016"],["/posts/570165348.html","04460b1151c86d2d2f008d11a82f251f"],["/posts/595890772.html","fb77b8c26bc0b8b26a3ab0e011d77225"],["/posts/67485572.html","d2c5078c4f4485097e9258c4b1221d9d"],["/posts/694347442.html","7baad0e488f63b81ed3fd9fcddc277a6"],["/posts/707384687.html","7d3bbb0f7f52ea00259b71597b04245d"],["/posts/71180092.html","782e5fb2ed7b144af9b8e72d631d41a9"],["/posts/716459272.html","a558875e9e0e40ddf510ee6b96798c11"],["/posts/765481613.html","668d1bbfdad92062f4f47df7e7e57d93"],["/posts/778231993.html","8a8f06b7a47f496a3050cd804f269981"],["/posts/795397410.html","04b1f7f2e9904f43aa34fa0bc25a10c8"],["/posts/820223701.html","45b2dc765903d754b485bb97157fb140"],["/posts/830372185.html","8b1d0a457eadd1574e5ce63fd8457f47"],["/posts/88294277.html","a33ea9aeb2931a12532147c5ad5dbe92"],["/posts/939963535.html","edf278b38f6655b4f9d70d4afd133a22"],["/posts/983786067.html","86f3de4f460de82d0de257460e5a1d94"],["/sw-register.js","7f56a6bee20bb59df08813bb18b9ebe8"],["/tags/C/index.html","8bdfebced0f134069ecbc962d4024c47"],["/tags/C/page/2/index.html","d841e11d1cd69f64746be0ca4fe00868"],["/tags/C/page/3/index.html","0a669493156156d7b269fb6a1845dd18"],["/tags/C/page/4/index.html","1edc582e9f078fc1f8274015513d40e7"],["/tags/ETL/index.html","d1d672e3c98a8fc5dd2f2fe725710229"],["/tags/ElasticSearch/index.html","843189435f4e61eab9a4a059595965d2"],["/tags/GUI/index.html","da1c96bf1855fea70e109ef244f7c75c"],["/tags/HBase/index.html","4a6ce4f1618324726b249169c42cf53e"],["/tags/Hadoop/index.html","fc41a1bc6a7590dbf50c5e4e4509455c"],["/tags/Hadoop/page/2/index.html","82a36e73702935874c95f4b649beac85"],["/tags/Java/index.html","a02ba4e7be90afa708ce25d8590937f8"],["/tags/Java后端/index.html","2a700983a31fa5172529aac9b8429b54"],["/tags/Java后端/page/2/index.html","a7e8d1f7313bb46cd94a733e3954697f"],["/tags/Java基础/index.html","209c8fa8738c1ad511a270a91394b34d"],["/tags/Java基础/page/2/index.html","1ac0627da19f509fe97a7a350019cb6d"],["/tags/Kettle/index.html","daafc6b19e9aefc281ae6e67826be9f1"],["/tags/Kibana/index.html","1476f63457c69d456da1e82310649897"],["/tags/Linux/index.html","f86514e4d8fd514b00e08794cb04e7a8"],["/tags/Linux/page/2/index.html","25c35fce1e88399344c255a6853ee3d0"],["/tags/Linux/page/3/index.html","dd90b94020c88499509575dff68eb398"],["/tags/Mac/index.html","807893cbec73352566c78d7c46f610bd"],["/tags/Mac/page/2/index.html","aa9b25e96b6ebd8c88631bc2c963db15"],["/tags/Maven/index.html","c8f4a2b9dc3e50cf61dc6200d1b846a3"],["/tags/MySQL/index.html","18306bdbdf3a831b18cf1e409a109f34"],["/tags/Python/index.html","e7353a1bf542e573369aaf870ad4926d"],["/tags/Redis/index.html","d3394690a51aba7f24ee59b32aba06d6"],["/tags/R语言/index.html","43a0b6572664be638b09b26448ae4433"],["/tags/Spark/index.html","8e47c3f4ae1bd06e7656d4f2fc15c3ff"],["/tags/Ubuntu/index.html","4f3c49d9a5709350f3fd8222063faf29"],["/tags/Vue/index.html","d2aa8f9120744eabede2510602b2bead"],["/tags/Windows/index.html","acf4f56ee499a66685c800fa1df2405a"],["/tags/ZooKeeper/index.html","9a317ee4dba4717371c1e449dd0db8bb"],["/tags/bfs/index.html","ebd8273c57ec90e61266cc84255d5151"],["/tags/dfs/index.html","f6a39d6fcb30d92991e002da474b5e11"],["/tags/folium/index.html","5d25d974176a2cddf1b03556c44035dd"],["/tags/git/index.html","648c298303859bebc962df9647272515"],["/tags/index.html","9b0ff387c31a94dd886ee69b6c394768"],["/tags/latex/index.html","2fce427507b1a6405fd8b4bda11649ab"],["/tags/中间件/index.html","df70426e89e3b3dc6668b19733f23e5c"],["/tags/二分查找/index.html","c1d54cd85d921bfb03798ca7c833a80b"],["/tags/优化类/index.html","f6b9d1b8401b03320da42b0ff8409fc9"],["/tags/前端/index.html","6b91706294d569641a2a2285bf9f66c3"],["/tags/前缀和与差分/index.html","93dfbddbe21f645228160886f87a2360"],["/tags/动态规划/index.html","9419a587ca2f34e855e80047760ca03d"],["/tags/动态规划/page/2/index.html","5d9cdd0088ab662f571d66b3724c620d"],["/tags/博客搭建/index.html","d1c3d7d62ef4a7f835cb49d00eefd214"],["/tags/图论/index.html","ed14741aa63531f1e14331b49acc7f28"],["/tags/大数据/index.html","c13763303ca1860db93397e93e6abdc7"],["/tags/大数据/page/2/index.html","840a80289a30ecca2acea12d006e5505"],["/tags/操作系统/index.html","95eb993dadf01700be024327d76e7c29"],["/tags/数学建模/index.html","92b16da5afc5876f8959d63435c8bd04"],["/tags/数据库/index.html","0629d5cc67696db3353dd8215e2f6d68"],["/tags/数据结构和算法/index.html","aa13f0bebde67c55e7150a3c162aae8d"],["/tags/数据结构和算法/page/2/index.html","9cbb10a8ed7c878d28dfa94f60cde5bd"],["/tags/数据结构和算法/page/3/index.html","7e361980007a2a71ee2652e21d855da1"],["/tags/数据结构和算法/page/4/index.html","a5339376037007de12e758e1b67f9236"],["/tags/数组和字符串/index.html","5aa2c179995e7c5ef6e5db1ec5156eef"],["/tags/数论/index.html","6491b0d9a8e0bc9f14aa231beef6fdca"],["/tags/枚举类/index.html","95f4423cc8b42490b7192109cfb14cf7"],["/tags/栈和队列/index.html","c20593038c7b33619e4e3911a9281cee"],["/tags/树论/index.html","660dcc83014646e3edb605b352d9d1eb"],["/tags/测试/index.html","50e71b1987d247ffb02f684ae05bc7b4"],["/tags/环境/index.html","947f3f4cbd8dee062e1aa92012b2935a"],["/tags/环境变量/index.html","e0d1aa9d210a3b7697f4f1bb71ea1381"],["/tags/绘图/index.html","6110de5559c717355ceb9365c325bd9f"],["/tags/编程工具/index.html","672927b6f45a93a51342177a858c4223"],["/tags/编程环境/index.html","92e3fcdbddd9639e232177434b54edf4"],["/tags/网络编程/index.html","2e34a58550158de53f8096bccfd4810c"],["/tags/英语语法/index.html","a33ceb8c595c44453ef082743d0f8f3e"],["/tags/计算机操作系统/index.html","f2f1ab39fdbc5a119e1a04627789ca25"],["/tags/论文/index.html","38dcee2f290e12b99dbf75f8be3ba983"],["/tags/资源下载/index.html","781be25e001a9a41386b6891ec9fc9c3"],["/tags/链表/index.html","639c0e757033e51cea7926177845e50d"],["/tags/集合/index.html","44a9c73115b16ce4d9be1db1c1748a7e"],["/tags/集群/index.html","aeaa83a54411d8f2f32d3e1ab8d4d86a"]];
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
