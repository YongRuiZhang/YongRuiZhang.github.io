/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","86c33fe7c21ecfe4499013d79af0ae22"],["/about/index.html","4435204f0ebf8524ba015d5a34a111d0"],["/archives/2023/01/index.html","77a74d6b648a1c776966d11d861a2c52"],["/archives/2023/02/index.html","c30f6bc95aaf6f7fd49988a78049d5cb"],["/archives/2023/02/page/2/index.html","4cbf467afa17041592981feb62d8175c"],["/archives/2023/03/index.html","46c48ea3d58603e1aea0a881f0a35713"],["/archives/2023/05/index.html","9bfcca6ff67b7bc0e12c1f61425d6edc"],["/archives/2023/06/index.html","a86d988b89f0be631763c8aefaa52c93"],["/archives/2023/09/index.html","ad7f9f40567dec9870027c006b304b3b"],["/archives/2023/11/index.html","5ae7cd33317ac5ab3b1acfa3f5e1b3c9"],["/archives/2023/12/index.html","964c97d2dd5a376ffe88a332c484e260"],["/archives/2023/index.html","06ea491984acd6efa704b05289ff3b57"],["/archives/2023/page/2/index.html","0f79a71e0ca65bc55e7b9aea9caffb6a"],["/archives/2023/page/3/index.html","8b19401ad756bbbfe7c698e5da92917a"],["/archives/2023/page/4/index.html","127e8adaa1404cdc7ff8e9431a9dd57f"],["/archives/2024/02/index.html","0e35a6a87d87a9ceebdeb6bffbb2eead"],["/archives/2024/index.html","c8eabe2e46f16e3293659fce8068a194"],["/archives/index.html","de9e171f54621b61deb4dee91a770e73"],["/archives/page/2/index.html","103576e8545a0bcccaa158be7a751606"],["/archives/page/3/index.html","407b0b32243ef7c202bf6c1a0fa4645f"],["/archives/page/4/index.html","34c1572483817a176c45297a91991576"],["/baidu_verify_codeva-qQP2iZOMLX.html","76343b636370269c54aeca68c882ff31"],["/categories/Java/index.html","3e52728dcaad9f874e59a5e0b7b6c09c"],["/categories/Java/后端/index.html","17e19900a9a25b98c8b039eb4798b13b"],["/categories/Java/基础/index.html","92ffd0fbf73fd19333a82cc01ec288fc"],["/categories/Java/基础/集合/index.html","97d5d1ecff65bce8208e81b730b1fc54"],["/categories/Python/index.html","5ad2078fec3fef6d911953b114d72d95"],["/categories/Python/编程环境/index.html","09fc60b78a1f01f709aa5c9520ffa75e"],["/categories/R语言/index.html","c4ca2a05a07bab9ee3e50fdd00890a9d"],["/categories/R语言/编程环境/index.html","1d49f326c73bca968d3f6ab65949125f"],["/categories/index.html","24a2dcccbfea320463ace722dd4dc30f"],["/categories/中间件/index.html","556cc1b0f07eaeefd590c78ddc2c3941"],["/categories/前端/Vue/index.html","4dfdd93369e237e380bfb7a81a4bcc48"],["/categories/前端/index.html","2152a90495a32a0d4c37eab3853adc7b"],["/categories/大数据开发/ElasticSearch/index.html","b8087584efcfe8405ac949c707f6ac07"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","b0af5f18e79d2fb4f45b21e3f0058b72"],["/categories/大数据开发/HBase/index.html","db4045ce97e80daff4b36b74c659195b"],["/categories/大数据开发/HBase/学习笔记/index.html","c6570401d9adbd8dc605ddfc8a82c584"],["/categories/大数据开发/HBase/环境搭建/index.html","75eb6fe18d3042b392384f8926dd8bf7"],["/categories/大数据开发/Hadoop/index.html","c2866224e71d1195c858a5a9fc102f3d"],["/categories/大数据开发/Hadoop/技术/index.html","8fda01c061206ee269f22ab50ce9a79a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","58b65730907a94060c69ddcf30b84223"],["/categories/大数据开发/Redis/index.html","a48f45520728fd67ae49afb34a95c041"],["/categories/大数据开发/Redis/技术/index.html","87474d32bb1a87622e745bbea7b7f89c"],["/categories/大数据开发/Redis/环境搭建/index.html","84cedfd5df77b1e7f1ea1beecf76f2ae"],["/categories/大数据开发/Spark/index.html","fb2277f62b615a6a7f7e021541202d7d"],["/categories/大数据开发/Spark/环境搭建/index.html","862b64b3f747e62454dd7891d294935d"],["/categories/大数据开发/Zookeeper/index.html","de6a689455aecc0b73c8b6d95322da85"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ce6e38007eed296394fbd583747b3e6c"],["/categories/大数据开发/index.html","c615282e7372854a9553033f476d501b"],["/categories/学校课程/index.html","c79ee91aa6c8c1647b4505e847bc7f56"],["/categories/学校课程/计算机操作系统/index.html","5dec39efb8531198638d3d9a1e3ddbe3"],["/categories/操作系统/Linux/index.html","3c7d9b26b317eec7f9caebe5cf52ff97"],["/categories/操作系统/Mac/index.html","fdabbd276c8ab2cd2dea9557dea1048f"],["/categories/操作系统/Windows/index.html","bbe205bc8d677a1c77bb975682c0f0c8"],["/categories/操作系统/index.html","04519a5a2a8121bee0b0c34b4be4d3a5"],["/categories/数学建模/index.html","f9cf932cf5a9d4f8a00484a2bde6ee00"],["/categories/数学建模/latex/index.html","6600e448346dc399c1b5fe4ce45b8e8e"],["/categories/数学建模/优化类/index.html","b3a3df4daaf4924988a41bea99ba7fab"],["/categories/数学建模/优化类/现代优化算法/index.html","a9b0f14b788c71ce1b24a0368d6df667"],["/categories/数学建模/优化类/规划类/index.html","ae45028ab1679bc671975a4b3066d8ba"],["/categories/数学建模/绘图/index.html","b8ad35be776dfd5fae107c6da00870bf"],["/categories/数据库/MySQL/index.html","964772dee763be20c8bafa3e25b4ae7f"],["/categories/数据库/index.html","9453d3ac6adcbe7dfcaeb223e1a357e9"],["/categories/数据结构和算法/index.html","0312564acde40fa7d5a3c7c9d0db06d4"],["/categories/数据结构和算法/page/2/index.html","5434d902be4aafd8a65f4e484ab671be"],["/categories/数据结构和算法/基本原理/bfs/index.html","b30c102813b389b60de60376da28e350"],["/categories/数据结构和算法/基本原理/dfs/index.html","960b92eeebf8bb81b2eec922f16e95aa"],["/categories/数据结构和算法/基本原理/index.html","349e75e43fbe04dc5b54cc16c63c4eef"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e88b2991f70006657348efbb7831feee"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","397ea86f62be53a072b7e69a6606966b"],["/categories/数据结构和算法/基本原理/图论/index.html","3e6acbeee7d5c40732791379c5719a62"],["/categories/数据结构和算法/基本原理/字符串/index.html","33f60ce3360c2c7598d934cf328ea6f0"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","4cf0cfa648de436c18e9ea2ddbc66f9e"],["/categories/数据结构和算法/基本原理/数论/index.html","c12cfad0b6cd752fbe5013affd4c5e3f"],["/categories/数据结构和算法/基本原理/树论/index.html","b6dc2756a084fa36b68597336e30f34b"],["/categories/数据结构和算法/基本原理/链表/index.html","86c3d9c10c8ce3f04fbc93976fc0b049"],["/categories/数据结构和算法/算法题/index.html","b52174f4a64655a3f532dbf9385b1147"],["/categories/数据结构和算法/算法题/二分查找/index.html","f875cabc331f06ed97c65b68f5ed3d46"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c14090738739086165c50a8c279cf50e"],["/categories/数据结构和算法/算法题/动态规划/index.html","a4629719896833952951280bb204bc37"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","bd20a6e99bd49b7fea87dd8126ed8b05"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","1a1a9dd8a1564da6cbe39de23bb7dd29"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","098427ce7dc79cb3fd5e40c74376e4f8"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","bd87512d1cb416af4e2c3ea824637ca6"],["/categories/数据结构和算法/算法题/数论/index.html","b978767136ac1189db2648be58b6ecf4"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d90ee6359ee5a47176cf1549c8862f95"],["/categories/数据结构和算法/算法题/树论/index.html","524a2df5b6cf8b1e140406e869fa0197"],["/categories/杂七杂八/index.html","d31ded7187bd4185099e86c835ea89b7"],["/categories/杂七杂八/博客搭建/index.html","b7e14f21704b9ddf7b5ffa1f81a91885"],["/categories/编程工具下载/index.html","252925c004f976b75df159fe6972c99e"],["/categories/编程环境/index.html","1b883129172567f9a45cbd45d32cd872"],["/categories/编程环境/大数据/index.html","e0fdb65ebfee7bd1ace5e7a557710d3f"],["/categories/英语学习/index.html","2f4775abe84f9a0a36ad304a1246d3e0"],["/categories/英语学习/英语语法/index.html","7c4ffdf874a3947f8b4ec57dbc3edae4"],["/comments/index.html","37facef0ff52b24fa2e9d059839c7e1d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e89a07515b1e168b45461a7cfa0baa10"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","c963b6ee74d2759b8d8b3587a4bc94d4"],["/movies/index.html","c1823cfb5352771af2e4c865436c60da"],["/music/index.html","3b2ee10be575673137ebf0ea4bbd725c"],["/page/2/index.html","e4ed053683292cdedc9b21d84953eb09"],["/page/3/index.html","9ffb0fb48759d0e72f59a5dd0f2f6624"],["/page/4/index.html","a33e309fb415334b53b966f829ceaa2f"],["/page/5/index.html","03c3ffb07310f0f0950651db5d5f107a"],["/page/6/index.html","9c1383dee479abb1e7c2abfbb748640f"],["/posts/1021360842.html","5e7ce5b4b5b0329743571fecb401a3a5"],["/posts/1120620192.html","c38a33ab4f1f16305e13e455c788af65"],["/posts/1141628095.html","365bd4244b2ad666e9b3b1ef5dec1a77"],["/posts/1168613674.html","2ac36c848a72034808855436ee353b57"],["/posts/1219920510.html","31a74f61af56c192bfa3868675286431"],["/posts/1222166338.html","31bf2c851976c2cd0251486e50fc9f8e"],["/posts/1259097482.html","1a5ea0fd2ac672eb001bec0dbff1cabe"],["/posts/1271036369.html","33814a23b114db661dbfc741a0d12c24"],["/posts/1312847445.html","9962eb2e91f3ea143901c0f5a9836c3a"],["/posts/135355774.html","74002a04ae87e21db64d4ff34c70a6a7"],["/posts/1375344716.html","4b285305df80625de2c1f472271100da"],["/posts/1388991698.html","c068f5d5cba30546f1fa2a6fc1f6c347"],["/posts/1410315814.html","ab7f22b98df2ffa4b796c5f690d76eaf"],["/posts/1452790229.html","4b47e4026a03783a9bb46398c936eb0b"],["/posts/1470079884.html","d67293801b95f670d29ae9a8d7767f29"],["/posts/1470079885.html","3c76e112c6a7d464c201f69efe8565e0"],["/posts/1470079886.html","df44fc98bcb79c3b0e15d4da2cc7630c"],["/posts/1470079887.html","d30b43ff71baf8ee3c81824dd5cc4162"],["/posts/1498536549.html","b1b49ca9600a812fa2b587063027ef03"],["/posts/1539568593.html","42a73bcbb54f0a754e063b55b86caf6c"],["/posts/1547067935.html","8883b2b28f5c8e2904b9d2fec68863e8"],["/posts/1557866301.html","b5cba390e5f470c65569d7dfa79a19d5"],["/posts/1571776361.html","3b8ab18450e23cf980fc5dd392b1cd24"],["/posts/1605124548.html","61eef6f8beccdefd7cd928650ade4125"],["/posts/1633036852.html","345076777d8931caeb09d17880d53dad"],["/posts/1674202625.html","4374cb2cb48dc67f3ad55ffdc727b60f"],["/posts/1765123828.html","8398190976c7cca576a83502f7790980"],["/posts/1767336200.html","d82d3096f4f2bce996c23ee2c2185015"],["/posts/1776114197.html","e6e61deac61655968640f6124f3397bb"],["/posts/1817748743.html","245f6f273ca85fba3624233b749a6f23"],["/posts/1925125395.html","0f38d91c2e326939fcc807faa7cdf7a2"],["/posts/1966191251.html","ffb699e0a33b0d726b2ac31fddf04548"],["/posts/1987617322.html","b0583c0d76008274eee3d113129e5c6b"],["/posts/1999788039.html","8de3f59b580b649dabcabd03e0cb7244"],["/posts/2075104059.html","58d9f0acaa63006b9a4b2a11c6a20413"],["/posts/2087796737.html","090dbc8a56efd1c215292549087a2165"],["/posts/2106547339.html","4cd50bcfa20685962d6b9178a8613fb8"],["/posts/2207806286.html","917c353a17cea6e4d8a2d531a4672ef9"],["/posts/2225903441.html","5b3087c075049b8e720b586cbfe356e7"],["/posts/2265610284.html","c49d47e23822f29ae245124ff78563e4"],["/posts/2281352001.html","d0a090351a739f2236c176dcf8f4b94c"],["/posts/2364755265.html","db74fea696b339711b722eb568792f89"],["/posts/2414116852.html","5ee0ac4cdd78fa5e2dd4c0b7f1f84716"],["/posts/2421785022.html","337280f7dad4fa16662a5d3d7a962d4b"],["/posts/2482902029.html","1c27a92272f0587804ddf666c6276077"],["/posts/2495386210.html","dd874394c9dc6c22c802380f30740972"],["/posts/2516528882.html","8e71c56c8a0dbaa362f309ab8f454b98"],["/posts/2526659543.html","2d697ea76451b64ff013c2e62fb2012b"],["/posts/2529807823.html","773260190c7e1085bafed73be0b4497a"],["/posts/2596601004.html","54512184ae641e30ef784091e7eae47d"],["/posts/2697614349.html","19be5568c0aad2ada6cf1e2034508523"],["/posts/2742438348.html","8839deb3818da2455842895d019d44d6"],["/posts/2768249503.html","9b6a5b20b32a184d0ffee3018f33a17b"],["/posts/2864584994.html","75ff35edd6e65cc589cf67855583d281"],["/posts/2888309600.html","d2e376d77a9366f15b55568ce46ac8d0"],["/posts/2891591958.html","909b646a4af79d71f8af7d698f02f3a6"],["/posts/2909934084.html","d6160712958f36446bf28b8a3e2aef9e"],["/posts/2920256992.html","39ebcb8dea1fe66c0a670e9786b062b0"],["/posts/2959474469.html","595b368b99290eca8a72411c1c3da2d0"],["/posts/3005926051.html","4c64583d4e9319fd5336cc673f519a92"],["/posts/309775400.html","c2e789766ad0edc744784c028ffb261e"],["/posts/3156194925.html","6843fb929e175af36245e1b424ebf955"],["/posts/3169224211.html","524505ad53b7b8f7b5db4fee70f680c3"],["/posts/3213899550.html","37bd317d2acba2533031ee2b3ae7f650"],["/posts/3259212833.html","6ee6689322651eb331b88a0264b4528f"],["/posts/3266130344.html","0d0fde26103194d9ee5cbfb8929fdf39"],["/posts/3292663995.html","ba4a691b45b94e2ad423a034667d5bfe"],["/posts/3297135020.html","b5751db64ad6fca1da1e102d3fcd36fd"],["/posts/3306641566.html","36cd719de4d622909318b3810db0d952"],["/posts/3312011324.html","d2d0ce3fdb36954f2d00cbdb4152032e"],["/posts/336911618.html","79b33602a1270d6963ac3ea2fcc1533d"],["/posts/3402121571.html","bfb094ace871199879faf323418bdb36"],["/posts/3405577485.html","a52f560b0860c2dc6ee6036eb590166d"],["/posts/3498516849.html","6f10b46aac979aed4e86f5fc960d781c"],["/posts/3513711414.html","31ec551ebd0dda3577464171c384d31b"],["/posts/3523095624.html","67c00956a25c9d886f44476832110eb3"],["/posts/3546711884.html","996c24cea1cfb38f825e44ae4976081b"],["/posts/3731385230.html","dc9adca8182d2b1571490b04198c87b0"],["/posts/3772089482.html","79d224e663f88be77d2591489d62b63e"],["/posts/386609427.html","bcecc183b21463104dda378c64f173f1"],["/posts/4044235327.html","a22a4a1f81ca5f0a414b95a8d59e428d"],["/posts/4115971639.html","b3845f1fec7273f4ec998ee2ea19ab94"],["/posts/4130790367.html","7cd7b5d77f56864a91d6d825df832a06"],["/posts/4131986683.html","e378e704e3a39dcf27c6ee7ced289224"],["/posts/4177218757.html","3af3430bd0f51bc78447341d32f3c47f"],["/posts/4192183953.html","6c0563e0b0c06fa21c8717e03ec35331"],["/posts/4261103898.html","edaca398e37bc72c1c17d8d1cb5b93f8"],["/posts/469711973.html","507f7b919b5e9fc7b59553642c2eaa93"],["/posts/482495853.html","01f7d5d32fb73c9d976454ef7c52ed30"],["/posts/488247922.html","222a03e829f9fde0e6882b9a65b85ac0"],["/posts/517302816.html","4e61d3816fe1fa91a05339de58dd9016"],["/posts/570165348.html","04460b1151c86d2d2f008d11a82f251f"],["/posts/595890772.html","fb77b8c26bc0b8b26a3ab0e011d77225"],["/posts/67485572.html","d2c5078c4f4485097e9258c4b1221d9d"],["/posts/694347442.html","7baad0e488f63b81ed3fd9fcddc277a6"],["/posts/707384687.html","7d3bbb0f7f52ea00259b71597b04245d"],["/posts/71180092.html","782e5fb2ed7b144af9b8e72d631d41a9"],["/posts/716459272.html","a558875e9e0e40ddf510ee6b96798c11"],["/posts/765481613.html","668d1bbfdad92062f4f47df7e7e57d93"],["/posts/778231993.html","8a8f06b7a47f496a3050cd804f269981"],["/posts/795397410.html","04b1f7f2e9904f43aa34fa0bc25a10c8"],["/posts/820223701.html","45b2dc765903d754b485bb97157fb140"],["/posts/830372185.html","8b1d0a457eadd1574e5ce63fd8457f47"],["/posts/88294277.html","a33ea9aeb2931a12532147c5ad5dbe92"],["/posts/939963535.html","edf278b38f6655b4f9d70d4afd133a22"],["/posts/983786067.html","86f3de4f460de82d0de257460e5a1d94"],["/sw-register.js","7c884822ca93a9569ce1cbf23de9ae8d"],["/tags/C/index.html","a071e0d8187290fc2e73fa59bf2e149e"],["/tags/C/page/2/index.html","00d962dd4993c73759c92680e8349f0d"],["/tags/C/page/3/index.html","340c6b7e08a2b815239b9c677f02efeb"],["/tags/C/page/4/index.html","a65800562bdd7d0bd28ff7c9586cabed"],["/tags/ETL/index.html","35f427dd3748b5f293f56f578b6df170"],["/tags/ElasticSearch/index.html","df599f636d4f8cce53738f25ea52b4c3"],["/tags/GUI/index.html","32848d81ec2a671b3447fa9480002438"],["/tags/HBase/index.html","484863b321a96ab7ba2c20df035dac38"],["/tags/Hadoop/index.html","9109144898a8950604903faa9930ee41"],["/tags/Hadoop/page/2/index.html","4ffd4c64f1d89f123bf95ea27199dcc2"],["/tags/Java/index.html","d591f2b7f09d5321c981043b24305d2d"],["/tags/Java后端/index.html","f81cc63a9cc3fd34c0d5b9985839ab13"],["/tags/Java后端/page/2/index.html","5a3c3fe357bb56fb29e331322d11fabc"],["/tags/Java基础/index.html","da003f6b99d918b56334b2a2b122dc45"],["/tags/Java基础/page/2/index.html","255bd8bcaa4f6b789f4c264c88ee1400"],["/tags/Kettle/index.html","16b6ef5f87c1db31e0cdce6c546c6404"],["/tags/Kibana/index.html","70df1452a92f253ba62233e9273e51e8"],["/tags/Linux/index.html","54e1e2772f485a844a306774aea02b1f"],["/tags/Linux/page/2/index.html","97c8d2afb9a2f8c7a1f762ac3bb6529e"],["/tags/Linux/page/3/index.html","b7510f7e48c2b48a6f87287603d86ebb"],["/tags/Mac/index.html","0f79ea16923f54d52bd4d7cb924a8d7f"],["/tags/Mac/page/2/index.html","d2be830125a76522d1181e0a0f76ba78"],["/tags/Maven/index.html","97d32dbeca259fc52468dc096e9b7345"],["/tags/MySQL/index.html","8ef0aec8923eb6382676f1cd8385450b"],["/tags/Python/index.html","87b21d6ae1c5664145b68aef88119b34"],["/tags/Redis/index.html","691fd096beb150193ae4de0a869be276"],["/tags/R语言/index.html","37cb8d96fbe71019a241500b27ed179d"],["/tags/Spark/index.html","bac77f5da45e5c11c51d5ab953170475"],["/tags/Ubuntu/index.html","24dfa0efc61417b8ddabed795c579b96"],["/tags/Vue/index.html","5c4e77c0ef718732a9a7ab3f4a3ec691"],["/tags/Windows/index.html","100fec6e311069e03d8be6e52ed17baf"],["/tags/ZooKeeper/index.html","cc0fd09679dab205f718ecd078d36bf1"],["/tags/bfs/index.html","b86778fd102e4ee94567561e9e969d38"],["/tags/dfs/index.html","497cbbb004a1482d800d2a5ef4a85d81"],["/tags/folium/index.html","6b70d74803cda9d7a51589afa8808afa"],["/tags/git/index.html","5a0b88fe2bcf344ec7f8d88389406e58"],["/tags/index.html","e2610f2676776abb106f1a2fce1ed374"],["/tags/latex/index.html","4f3ae81dbfcba7a92d82d3cf78ae3f0f"],["/tags/中间件/index.html","a9eeb8ff701c031be390e3bd2c591113"],["/tags/二分查找/index.html","47c022be6b2cfb2e9fdcc84ced530cda"],["/tags/优化类/index.html","d699aaaecbf97c3a7765b53323858de3"],["/tags/前端/index.html","c15f32342222ccb92f21404a567e2aed"],["/tags/前缀和与差分/index.html","a618bdfd86a1cc3b8bf09472c658141f"],["/tags/动态规划/index.html","62b567d990e7d5166b7537175da6fb0f"],["/tags/动态规划/page/2/index.html","c1487ac8f6d5e4fd2fc12a031f24904d"],["/tags/博客搭建/index.html","4ffd6aba611acc599fb7d45d745dd47a"],["/tags/图论/index.html","8c58b7d060fd8266ea83c4635ad5fb3d"],["/tags/大数据/index.html","0e56268a37b222955b431b4b896f7803"],["/tags/大数据/page/2/index.html","595a803b4cfe6e533025f9c68b5dfa26"],["/tags/操作系统/index.html","206a456196fab5b73055df291195c1c8"],["/tags/数学建模/index.html","dd4415a2c2a8895cc9187a4ed76073e0"],["/tags/数据库/index.html","c901c55ccdbb7e02090c0b14b06fe991"],["/tags/数据结构和算法/index.html","35061c19988d19062e7995205560543a"],["/tags/数据结构和算法/page/2/index.html","279e4f71276f9f7fe9307adae63fc381"],["/tags/数据结构和算法/page/3/index.html","a63d2e98cb5e93fa08a9b2d905760e4c"],["/tags/数据结构和算法/page/4/index.html","689685986b3f39af2ddfa692912971a5"],["/tags/数组和字符串/index.html","54942a0e5a619ddc0ac504076ed1529d"],["/tags/数论/index.html","a1030b7b11a2391e078a4b506d0c1075"],["/tags/枚举类/index.html","f0198cf08773653f1432396ed98ad056"],["/tags/栈和队列/index.html","e681dd8cf166ff55ef572b9923a45092"],["/tags/树论/index.html","e71b047961f2ca6a0972196459e75118"],["/tags/测试/index.html","9f13909c6403d86b331dd228eb2140eb"],["/tags/环境/index.html","381a62d78cefd743158f926666a3bd32"],["/tags/环境变量/index.html","83ba5ee1261cdc2c79df6657c551eb96"],["/tags/绘图/index.html","ac5b73124ed973816b5caf549dabc940"],["/tags/编程工具/index.html","6b5655ca5c6fc3cb11b36220e9decd14"],["/tags/编程环境/index.html","5a37f53f2ab27618d3a5e1c125bd93ef"],["/tags/网络编程/index.html","e045f94f61af850b50fba060814d510b"],["/tags/英语语法/index.html","de54a4828888bb58b503dc2877d80c93"],["/tags/计算机操作系统/index.html","b6ea0ff8542b1b346db68a68bcd34ba2"],["/tags/论文/index.html","38c883d6564e48f67fb63454feb24258"],["/tags/资源下载/index.html","caabdf80c080f4b850a34e8b220001d2"],["/tags/链表/index.html","091f0ceacc759b38ac64ee13b1251c3f"],["/tags/集合/index.html","693ea486dce6d975ad76f3cda391f37c"],["/tags/集群/index.html","49fa663b5609c1c8230ff79f9682ad7f"]];
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
