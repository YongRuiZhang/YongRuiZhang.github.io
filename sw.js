/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e120e1954e2ec18b8d8b9517608b14fb"],["/about/index.html","4435204f0ebf8524ba015d5a34a111d0"],["/archives/2023/01/index.html","91147f6dbe9a65c0af7043c1e74bd593"],["/archives/2023/02/index.html","16e0fb4616d0449a507697011bbeb06b"],["/archives/2023/02/page/2/index.html","6e77e1895d79840a9b924b2af64e1cb9"],["/archives/2023/03/index.html","e9fe5d2bd4ed5ae30655881f56393c60"],["/archives/2023/05/index.html","0145327f596c5850abde0c5357c38868"],["/archives/2023/06/index.html","2c3bec52d66331788df914557cc9d366"],["/archives/2023/09/index.html","faa96fb6cb1a69f4811bb36738b47177"],["/archives/2023/11/index.html","3554743e2bddab74a2957ae13226afe6"],["/archives/2023/12/index.html","e170262e604bd6feae3f80c05c27926b"],["/archives/2023/index.html","3464ec1a78376a7d1ac60712cc5de746"],["/archives/2023/page/2/index.html","07fb9dedd278b51e52d8fcc803115603"],["/archives/2023/page/3/index.html","e75aac03476e9566820a32f3f36a50b9"],["/archives/2023/page/4/index.html","1eea323eb8c00878321daa7ffbb14979"],["/archives/2024/02/index.html","d3bcb0a7a2b2887908b44b53cf4b103a"],["/archives/2024/index.html","a6fe52d544a4ab409a0e8b3ae18b0f8b"],["/archives/index.html","1eeeae961703fd17ddbfa8ae5c6e714d"],["/archives/page/2/index.html","aae3c99805786c9d67d8b13d41922a15"],["/archives/page/3/index.html","82a86cbb5ce481f526e2599fffa93fdf"],["/archives/page/4/index.html","38cc2ca3c6975f78fade2957708dc448"],["/baidu_verify_codeva-qQP2iZOMLX.html","ccbd8010fb1ee4d76946251abef39900"],["/categories/Java/index.html","4c7c1be41f1b3fa3a83508c36b9abbf7"],["/categories/Java/后端/index.html","f55b85e40e4fef930434c710ecd0a6cc"],["/categories/Java/基础/index.html","ae0ac0e21099866c3133033b5764fc82"],["/categories/Java/基础/集合/index.html","7554a3b8c275af909bbcad13430e3aa4"],["/categories/Python/index.html","267b37e7f05caa848ccd69f9698fee6b"],["/categories/Python/编程环境/index.html","0b2f888d62d51e7a61976082fec3259f"],["/categories/R语言/index.html","2f6915d915aea86b3b38ab6a09e0ac1a"],["/categories/R语言/编程环境/index.html","0ebeedfb5d7f800c892278bbbd7ff613"],["/categories/index.html","24a2dcccbfea320463ace722dd4dc30f"],["/categories/中间件/index.html","c8e42da47162465b39e830b7406dba99"],["/categories/前端/Vue/index.html","e208238a64e4234bd615d4170a2a752e"],["/categories/前端/index.html","bf23d109fcec0f20c4cc5201b9392ee3"],["/categories/大数据开发/ElasticSearch/index.html","6ec11dd3c9048819f2795ca649fe860c"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","6fdc5d5b11a4f331afbcd4a35aa4d747"],["/categories/大数据开发/HBase/index.html","06c7ff909e0c515b0ed678f00a80de1b"],["/categories/大数据开发/HBase/学习笔记/index.html","f1d57393a947672f68c784ae6550797f"],["/categories/大数据开发/HBase/环境搭建/index.html","e7d53a7a19301c31c2565ba65d68b61c"],["/categories/大数据开发/Hadoop/index.html","c841412d9da2932f5acef08641bdd39a"],["/categories/大数据开发/Hadoop/技术/index.html","556ae9337577a666ab83fcff86521195"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d657e555c2e67370c453bfe331674003"],["/categories/大数据开发/Redis/index.html","d147219f93c57075bf3e0e6b594f5301"],["/categories/大数据开发/Redis/技术/index.html","d769ad9382a05bdaf02ff3cbe9e8023f"],["/categories/大数据开发/Redis/环境搭建/index.html","abc90b224ef9fde5293f30dd27d42a7d"],["/categories/大数据开发/Spark/index.html","2303fdde2b012b3aac52345dd59b69c4"],["/categories/大数据开发/Spark/环境搭建/index.html","a66e205518471d98883d9547b53100db"],["/categories/大数据开发/Zookeeper/index.html","ed46192679a35c5a627aba64ec575006"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","710482c7bb4f2fb3ed195e44d3d80ca8"],["/categories/大数据开发/index.html","90af0fded5693f4e0db44a55123bf703"],["/categories/学校课程/index.html","265e008ada6906806061f1fcdbaa91b2"],["/categories/学校课程/计算机操作系统/index.html","f6fbb898a12d94a966b7b7729dc0919b"],["/categories/操作系统/Linux/index.html","0f1256866be308a1c9f0de414c8d7a63"],["/categories/操作系统/Mac/index.html","1745a712d1a625255e33d74f720551be"],["/categories/操作系统/Windows/index.html","6efb51831d07deca97c8b8972994f663"],["/categories/操作系统/index.html","570f3e073bdc4e27bd50640b785022cb"],["/categories/数学建模/index.html","2320db142fd4bc09fdd8ab23569cc271"],["/categories/数学建模/latex/index.html","3fa6754802d41c4b3aedc5bed45c3c01"],["/categories/数学建模/优化类/index.html","497a3c5c6a95ab928a94306e63875075"],["/categories/数学建模/优化类/现代优化算法/index.html","a98d9d11d08a892f0922d8fc2499b496"],["/categories/数学建模/优化类/规划类/index.html","216f97535dd9e315ff24793ade9c8935"],["/categories/数学建模/绘图/index.html","f913d742df02f3cda144c2ff2b9d59e4"],["/categories/数据库/MySQL/index.html","808cbe8b4fbb8e7c3503f1e039dfe14b"],["/categories/数据库/index.html","23270154c74572a4f72f0c88df39ca03"],["/categories/数据结构和算法/index.html","2c51366324bbcd476f6f71c45f7b51b6"],["/categories/数据结构和算法/page/2/index.html","906f2503392d9cd82ca0d679c8e18dd2"],["/categories/数据结构和算法/基本原理/bfs/index.html","3dd32397e1ade757252d03fae7522043"],["/categories/数据结构和算法/基本原理/dfs/index.html","8abb2d62f0bba0cee97b8600fba0107a"],["/categories/数据结构和算法/基本原理/index.html","7d8fe6708ebf4c7a422c35ce8c733774"],["/categories/数据结构和算法/基本原理/动态规划/index.html","7ebe7b8b4bc93bad4dc46752ba7e54e7"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","976b2e10a7c4ae8ba046505d110327c1"],["/categories/数据结构和算法/基本原理/图论/index.html","6ad383e65b45354889bbb7f09a4e865a"],["/categories/数据结构和算法/基本原理/字符串/index.html","db64064a57f7f328f9c0b93ec7679555"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","796c350946315f8a28b5b8417249bb6c"],["/categories/数据结构和算法/基本原理/数论/index.html","980c213560301e99b1f274115bbe9504"],["/categories/数据结构和算法/基本原理/树论/index.html","82f439a6c2ca8bc6fa04214af1c60009"],["/categories/数据结构和算法/基本原理/链表/index.html","e38f46d7db77a327bce30b3d1f500246"],["/categories/数据结构和算法/算法题/index.html","93c648e66d3074cccc1e2e5345ce591e"],["/categories/数据结构和算法/算法题/二分查找/index.html","881b273a7d90e1a639f3cc57a705a954"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f1efbddb09dc835b169ca80933a349c9"],["/categories/数据结构和算法/算法题/动态规划/index.html","aba994f1df89c2b16b6ed1f97dc4fea5"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","86bbd08959943e87c2a1e8b306c4d3ab"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","6f4fdc89499b335c2d479d20b4741d00"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","51fc7a21dda2e28ef2bb3e841e196c3e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","8fd324d356569b82dd83d7871a7846b8"],["/categories/数据结构和算法/算法题/数论/index.html","a34ac1459a181e135dc8f1b209e49a5e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c27749e2c69eb3a3176c3a0fb8a69b2a"],["/categories/数据结构和算法/算法题/树论/index.html","11fbdf8036476140e29f5f436ac473f3"],["/categories/杂七杂八/index.html","7e56a599e35bd859954f75afa5d43d09"],["/categories/杂七杂八/博客搭建/index.html","c3971c33e28efceba08ee7ac3d9617c3"],["/categories/编程工具下载/index.html","c7efd220960fd2fcf423ac8fb69bf188"],["/categories/编程环境/index.html","6fc71b361390de925ac3781b09cbbcf3"],["/categories/编程环境/大数据/index.html","4c4a39c9c2f5b97db2ba37f3cc5796e0"],["/categories/英语学习/index.html","1f7bec5a141e5ca25945df51774c493b"],["/categories/英语学习/英语语法/index.html","535c83367dede829e08179f4162721d9"],["/comments/index.html","9a9a20913fa9fe80e2a81586c539d339"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c24bd3be2148326e472fd8108ba0c8e1"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","50e72e97f1b172fe57177837972e943d"],["/movies/index.html","45417794d044363caa5fc43200039a19"],["/music/index.html","f52d8732c5598b9c3d402bf67308bdb6"],["/page/2/index.html","9b9b060dd39c8e553f881b4540961447"],["/page/3/index.html","99e8f496ebda550dd0da78d9dd6d984b"],["/page/4/index.html","84bc4fa746871fd52da93c6fc57c3781"],["/page/5/index.html","c1ca20f283b455c14f50e5667762fc7c"],["/page/6/index.html","0140e64bb74204146904badd8f1656be"],["/posts/1021360842.html","5e7ce5b4b5b0329743571fecb401a3a5"],["/posts/1120620192.html","c38a33ab4f1f16305e13e455c788af65"],["/posts/1141628095.html","365bd4244b2ad666e9b3b1ef5dec1a77"],["/posts/1168613674.html","2ac36c848a72034808855436ee353b57"],["/posts/1219920510.html","31a74f61af56c192bfa3868675286431"],["/posts/1222166338.html","31bf2c851976c2cd0251486e50fc9f8e"],["/posts/1259097482.html","1a5ea0fd2ac672eb001bec0dbff1cabe"],["/posts/1271036369.html","33814a23b114db661dbfc741a0d12c24"],["/posts/1312847445.html","9962eb2e91f3ea143901c0f5a9836c3a"],["/posts/135355774.html","74002a04ae87e21db64d4ff34c70a6a7"],["/posts/1375344716.html","4b285305df80625de2c1f472271100da"],["/posts/1388991698.html","c068f5d5cba30546f1fa2a6fc1f6c347"],["/posts/1410315814.html","ab7f22b98df2ffa4b796c5f690d76eaf"],["/posts/1452790229.html","4b47e4026a03783a9bb46398c936eb0b"],["/posts/1470079884.html","d67293801b95f670d29ae9a8d7767f29"],["/posts/1470079885.html","3c76e112c6a7d464c201f69efe8565e0"],["/posts/1470079886.html","df44fc98bcb79c3b0e15d4da2cc7630c"],["/posts/1470079887.html","d30b43ff71baf8ee3c81824dd5cc4162"],["/posts/1498536549.html","b1b49ca9600a812fa2b587063027ef03"],["/posts/1539568593.html","42a73bcbb54f0a754e063b55b86caf6c"],["/posts/1547067935.html","8883b2b28f5c8e2904b9d2fec68863e8"],["/posts/1557866301.html","b5cba390e5f470c65569d7dfa79a19d5"],["/posts/1571776361.html","3b8ab18450e23cf980fc5dd392b1cd24"],["/posts/1605124548.html","61eef6f8beccdefd7cd928650ade4125"],["/posts/1633036852.html","345076777d8931caeb09d17880d53dad"],["/posts/1674202625.html","4374cb2cb48dc67f3ad55ffdc727b60f"],["/posts/1765123828.html","8398190976c7cca576a83502f7790980"],["/posts/1767336200.html","d82d3096f4f2bce996c23ee2c2185015"],["/posts/1776114197.html","e6e61deac61655968640f6124f3397bb"],["/posts/1817748743.html","245f6f273ca85fba3624233b749a6f23"],["/posts/1925125395.html","0f38d91c2e326939fcc807faa7cdf7a2"],["/posts/1966191251.html","ffb699e0a33b0d726b2ac31fddf04548"],["/posts/1987617322.html","b0583c0d76008274eee3d113129e5c6b"],["/posts/1999788039.html","8de3f59b580b649dabcabd03e0cb7244"],["/posts/2075104059.html","58d9f0acaa63006b9a4b2a11c6a20413"],["/posts/2087796737.html","090dbc8a56efd1c215292549087a2165"],["/posts/2106547339.html","4cd50bcfa20685962d6b9178a8613fb8"],["/posts/2207806286.html","917c353a17cea6e4d8a2d531a4672ef9"],["/posts/2225903441.html","5b3087c075049b8e720b586cbfe356e7"],["/posts/2265610284.html","c49d47e23822f29ae245124ff78563e4"],["/posts/2281352001.html","d0a090351a739f2236c176dcf8f4b94c"],["/posts/2364755265.html","db74fea696b339711b722eb568792f89"],["/posts/2414116852.html","5ee0ac4cdd78fa5e2dd4c0b7f1f84716"],["/posts/2421785022.html","337280f7dad4fa16662a5d3d7a962d4b"],["/posts/2482902029.html","1c27a92272f0587804ddf666c6276077"],["/posts/2495386210.html","dd874394c9dc6c22c802380f30740972"],["/posts/2516528882.html","8e71c56c8a0dbaa362f309ab8f454b98"],["/posts/2526659543.html","2d697ea76451b64ff013c2e62fb2012b"],["/posts/2529807823.html","773260190c7e1085bafed73be0b4497a"],["/posts/2596601004.html","54512184ae641e30ef784091e7eae47d"],["/posts/2697614349.html","19be5568c0aad2ada6cf1e2034508523"],["/posts/2742438348.html","8839deb3818da2455842895d019d44d6"],["/posts/2768249503.html","9b6a5b20b32a184d0ffee3018f33a17b"],["/posts/2864584994.html","75ff35edd6e65cc589cf67855583d281"],["/posts/2888309600.html","d2e376d77a9366f15b55568ce46ac8d0"],["/posts/2891591958.html","909b646a4af79d71f8af7d698f02f3a6"],["/posts/2909934084.html","d6160712958f36446bf28b8a3e2aef9e"],["/posts/2920256992.html","39ebcb8dea1fe66c0a670e9786b062b0"],["/posts/2959474469.html","595b368b99290eca8a72411c1c3da2d0"],["/posts/3005926051.html","4c64583d4e9319fd5336cc673f519a92"],["/posts/309775400.html","c2e789766ad0edc744784c028ffb261e"],["/posts/3156194925.html","6843fb929e175af36245e1b424ebf955"],["/posts/3169224211.html","524505ad53b7b8f7b5db4fee70f680c3"],["/posts/3213899550.html","37bd317d2acba2533031ee2b3ae7f650"],["/posts/3259212833.html","6ee6689322651eb331b88a0264b4528f"],["/posts/3266130344.html","0d0fde26103194d9ee5cbfb8929fdf39"],["/posts/3292663995.html","ba4a691b45b94e2ad423a034667d5bfe"],["/posts/3297135020.html","b5751db64ad6fca1da1e102d3fcd36fd"],["/posts/3306641566.html","36cd719de4d622909318b3810db0d952"],["/posts/3312011324.html","d2d0ce3fdb36954f2d00cbdb4152032e"],["/posts/336911618.html","79b33602a1270d6963ac3ea2fcc1533d"],["/posts/3402121571.html","bfb094ace871199879faf323418bdb36"],["/posts/3405577485.html","a52f560b0860c2dc6ee6036eb590166d"],["/posts/3498516849.html","6f10b46aac979aed4e86f5fc960d781c"],["/posts/3513711414.html","31ec551ebd0dda3577464171c384d31b"],["/posts/3523095624.html","67c00956a25c9d886f44476832110eb3"],["/posts/3546711884.html","996c24cea1cfb38f825e44ae4976081b"],["/posts/3731385230.html","dc9adca8182d2b1571490b04198c87b0"],["/posts/3772089482.html","79d224e663f88be77d2591489d62b63e"],["/posts/386609427.html","bcecc183b21463104dda378c64f173f1"],["/posts/4044235327.html","a22a4a1f81ca5f0a414b95a8d59e428d"],["/posts/4115971639.html","b3845f1fec7273f4ec998ee2ea19ab94"],["/posts/4130790367.html","7cd7b5d77f56864a91d6d825df832a06"],["/posts/4131986683.html","e378e704e3a39dcf27c6ee7ced289224"],["/posts/4177218757.html","3af3430bd0f51bc78447341d32f3c47f"],["/posts/4192183953.html","6c0563e0b0c06fa21c8717e03ec35331"],["/posts/4261103898.html","edaca398e37bc72c1c17d8d1cb5b93f8"],["/posts/469711973.html","507f7b919b5e9fc7b59553642c2eaa93"],["/posts/482495853.html","01f7d5d32fb73c9d976454ef7c52ed30"],["/posts/488247922.html","222a03e829f9fde0e6882b9a65b85ac0"],["/posts/517302816.html","4e61d3816fe1fa91a05339de58dd9016"],["/posts/570165348.html","04460b1151c86d2d2f008d11a82f251f"],["/posts/595890772.html","fb77b8c26bc0b8b26a3ab0e011d77225"],["/posts/67485572.html","d2c5078c4f4485097e9258c4b1221d9d"],["/posts/694347442.html","7baad0e488f63b81ed3fd9fcddc277a6"],["/posts/707384687.html","7d3bbb0f7f52ea00259b71597b04245d"],["/posts/71180092.html","782e5fb2ed7b144af9b8e72d631d41a9"],["/posts/716459272.html","a558875e9e0e40ddf510ee6b96798c11"],["/posts/765481613.html","668d1bbfdad92062f4f47df7e7e57d93"],["/posts/778231993.html","8a8f06b7a47f496a3050cd804f269981"],["/posts/795397410.html","04b1f7f2e9904f43aa34fa0bc25a10c8"],["/posts/820223701.html","45b2dc765903d754b485bb97157fb140"],["/posts/830372185.html","8b1d0a457eadd1574e5ce63fd8457f47"],["/posts/88294277.html","a33ea9aeb2931a12532147c5ad5dbe92"],["/posts/939963535.html","edf278b38f6655b4f9d70d4afd133a22"],["/posts/983786067.html","86f3de4f460de82d0de257460e5a1d94"],["/sw-register.js","93b8cbf3c1db9826d759797969dd925e"],["/tags/C/index.html","a4f6ff2fe5a85da2b264aff3532b3c87"],["/tags/C/page/2/index.html","f95ba45ba7417fba073cf2c2e63c125b"],["/tags/C/page/3/index.html","075a9965d3e19eed035122baf12375ff"],["/tags/C/page/4/index.html","c62fa336e5ac4905c4eedee0b3fc448a"],["/tags/ETL/index.html","ce7f18a78a4b684f9a471d4707891f2e"],["/tags/ElasticSearch/index.html","996ba1821615eedcac18934ae0e848dc"],["/tags/GUI/index.html","f89df791053d9475326bc7501228035b"],["/tags/HBase/index.html","1afdd9332fbf2f562bc2798236eec2a9"],["/tags/Hadoop/index.html","4fc8faccd3651bff48cbd87095fcf004"],["/tags/Hadoop/page/2/index.html","275fb1ccfff051b22542f36aa6e21195"],["/tags/Java/index.html","f8a8b412e4ab9c9db659c50cb6972bde"],["/tags/Java后端/index.html","4ec1ee4c64720007860203061a6512c6"],["/tags/Java后端/page/2/index.html","cd6cb1eff9f6e528ca84b5b10bf55e75"],["/tags/Java基础/index.html","6d04af2819c8eea515401891d3461ec7"],["/tags/Java基础/page/2/index.html","8b336b88b8fae1d7230806ad3c505b8c"],["/tags/Kettle/index.html","12f150eccd65fd81325152cb39df4fcc"],["/tags/Kibana/index.html","8afd42979d80795a8c4eb185c354b6cd"],["/tags/Linux/index.html","3ea096b6ad45e411975bd69c77dd324f"],["/tags/Linux/page/2/index.html","853dc9d8bfe9b21a925bbefaccf81a04"],["/tags/Linux/page/3/index.html","ed54ce2105100c64f4791bf3ef26b3a6"],["/tags/Mac/index.html","35a0e22d34a9b34fdd567e5e42f97576"],["/tags/Mac/page/2/index.html","06d81ee0796c05c1c889ab3ae99fa442"],["/tags/Maven/index.html","f4a762c6feda9a5964d8b8d58af175b7"],["/tags/MySQL/index.html","9187f7182cd98c6b8b5bcc3112748bbf"],["/tags/Python/index.html","10b459b69470b69112f6e3f737d00679"],["/tags/Redis/index.html","a055a57afdb83519fbfa6a8d20bb0104"],["/tags/R语言/index.html","56c6770c7202614d30e679d17b0b8e66"],["/tags/Spark/index.html","04084c73b9cf783f75d65cecd5893e70"],["/tags/Ubuntu/index.html","230d4f710ececd915e28eb1b57969c2d"],["/tags/Vue/index.html","fa475e74ee9c0fd08f65a13ff7d7398c"],["/tags/Windows/index.html","e9c8e60ed11b604445d434a2ed96c7d4"],["/tags/ZooKeeper/index.html","d107966e3ed954bb007e81a277726310"],["/tags/bfs/index.html","e221e7fcdd6b8ec0021be33a220e2aa9"],["/tags/dfs/index.html","12cc76715f40a3ece06620adfd70459e"],["/tags/folium/index.html","0b0886cb212c5af78a224f64696d665f"],["/tags/git/index.html","629e30820ce25979ea2bae907407152b"],["/tags/index.html","f42dfd49bf7814da8e618ad4bc526385"],["/tags/latex/index.html","bc9ac2bae6c51c6fafe2a5fb2bdba23d"],["/tags/中间件/index.html","cdbdcc61410723b9fdffa209cff20842"],["/tags/二分查找/index.html","cc710b3bd51ff60278e5760820857b91"],["/tags/优化类/index.html","69028cf40caf6005e87535f05a9aadb7"],["/tags/前端/index.html","732fbc795e3aac6aecedff80a2fe87ad"],["/tags/前缀和与差分/index.html","bb1e412c292ed7a05698fd631947d316"],["/tags/动态规划/index.html","86e3fc61de21ed405419e26a3894c1e6"],["/tags/动态规划/page/2/index.html","356fd362ef9101316c1f100a46a246c1"],["/tags/博客搭建/index.html","7cf128793e2e190e70711f92012ab974"],["/tags/图论/index.html","efb08a7db43e6bb5230421593111b030"],["/tags/大数据/index.html","9fbffd92d47242029921ab85dd620716"],["/tags/大数据/page/2/index.html","f756314cd0bed1c3867a20084698403b"],["/tags/操作系统/index.html","8b2beb55ac5032a3a9351f95197bbfee"],["/tags/数学建模/index.html","3bbcdf07871b058b7fa7b4621b1299a6"],["/tags/数据库/index.html","de1d2d5e790d69265504fa41720c15c0"],["/tags/数据结构和算法/index.html","e74dc3c01c1b5565c21566db895b2938"],["/tags/数据结构和算法/page/2/index.html","e6cf6bb72bdde042f227df136deef954"],["/tags/数据结构和算法/page/3/index.html","b97aca44cfb422f48a3a726c7bf341ad"],["/tags/数据结构和算法/page/4/index.html","d8f98abb74618b3c2e6a6dbdf55d01ba"],["/tags/数组和字符串/index.html","cb3ed0280df7cfe5fa98f7c5fb35497d"],["/tags/数论/index.html","125aa001b232ed6ef8921cd790ef4d32"],["/tags/枚举类/index.html","cf3137439029dd08613afcd95d3937bb"],["/tags/栈和队列/index.html","4b4b835e790facf2c94bce97511c3575"],["/tags/树论/index.html","9a7e3f6404f7e556c469b9aa05f21bbc"],["/tags/测试/index.html","533b1026a0e0662d69adf74ea164b6a0"],["/tags/环境/index.html","4a10d6e3c736be3d555691bff3eef5fc"],["/tags/环境变量/index.html","a727e39fc48a122b42be45120d8b3bbb"],["/tags/绘图/index.html","e5ae095b552a05767f70946916829121"],["/tags/编程工具/index.html","1a28264a2a07e466a968412b4c74f775"],["/tags/编程环境/index.html","942ab9410a1e28d0f93e2f83f74f24ca"],["/tags/网络编程/index.html","0b7614e18c3fd75bda4970fd19c46763"],["/tags/英语语法/index.html","bf0499d07b196fdd99b7f48dcab62a96"],["/tags/计算机操作系统/index.html","922a92caf2cd93c5c4b07a9cb4642e91"],["/tags/论文/index.html","fd8484f0bd49446cd37f0771f4ea6dfa"],["/tags/资源下载/index.html","e9c5e75ad478bef92c961360752533c6"],["/tags/链表/index.html","8a24b12f097b75bcd94667ed52994268"],["/tags/集合/index.html","a88815e34f1c26de4860c67ed22c9207"],["/tags/集群/index.html","5041738325b64089188b7c24fbb0416d"]];
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
