/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","3cb49b4ff95057314e0238b8dca8673f"],["/about/index.html","4435204f0ebf8524ba015d5a34a111d0"],["/archives/2023/01/index.html","66a97ef105ff5ed20a8099c8d9ebb34e"],["/archives/2023/02/index.html","aa8ff3e48798a115bb5d554a3e88ef3e"],["/archives/2023/02/page/2/index.html","d98392441f72eee5db94359d3eb26e76"],["/archives/2023/03/index.html","f927118b24396d59451dd739282fed9c"],["/archives/2023/05/index.html","40ac329286a9d65631d23eb4349bfaeb"],["/archives/2023/06/index.html","47f13997a830eafa12cd07f49a3f42be"],["/archives/2023/09/index.html","9a35080e8fd47799deb27778601c16ba"],["/archives/2023/11/index.html","886768ee17e1cf64e3971cb5ac7005be"],["/archives/2023/12/index.html","3d5e01801c8bcb89af3e364e321ce9f9"],["/archives/2023/index.html","d8fd34d0a2c6c53c081e7075d7366775"],["/archives/2023/page/2/index.html","c18a9378d6c2308dac271fd2c0785921"],["/archives/2023/page/3/index.html","9974c0c37f8d6d1beb0ed66d552b370a"],["/archives/2023/page/4/index.html","a2cf4741deef07b19374530c3febcace"],["/archives/2024/02/index.html","46872f6f0c8096952af2a04c6659fac7"],["/archives/2024/index.html","38c04a85e20c11cd801799c11dd69226"],["/archives/index.html","2e7b9892f0fb61b5fba68ccea07d37bc"],["/archives/page/2/index.html","e0ca1d3b697959c04d0f4f279205b25c"],["/archives/page/3/index.html","e64ebafe3d18355f65b237c6263d1901"],["/archives/page/4/index.html","16883468fa4d0f39108be4a7b69cb6ab"],["/baidu_verify_codeva-qQP2iZOMLX.html","8b3d8e56bf17fdf7312c86590cadbe03"],["/categories/Java/index.html","93deda771f38d00731f71b3ae671c306"],["/categories/Java/后端/index.html","50a2f3bdd8437293680684d56b5df899"],["/categories/Java/基础/index.html","bfd79a2a4fe5ea1bb132659883b0b25f"],["/categories/Java/基础/集合/index.html","ea1798f8d5ac1d8347d46d6e586ef5a6"],["/categories/Python/index.html","e06cbee3e30827bfcdb79898f593692e"],["/categories/Python/编程环境/index.html","160811722a60bce02aa862b49131443d"],["/categories/R语言/index.html","4736154c17a6292029ce98cae400911d"],["/categories/R语言/编程环境/index.html","192b3265b3c32c385b1942a821eb4fde"],["/categories/index.html","24a2dcccbfea320463ace722dd4dc30f"],["/categories/中间件/index.html","36e77b3b891128a64cd10d1fe5a1e3cd"],["/categories/前端/Vue/index.html","07993881ff985fab5d940af5ad01980c"],["/categories/前端/index.html","b97146b9c2848e43a7bc82972e62c499"],["/categories/大数据开发/ElasticSearch/index.html","def4e38d29fba8b5c9d14442fb87eafd"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","61bf7fea9a366c9e982461de99115c3b"],["/categories/大数据开发/HBase/index.html","40cbee438a840227d0fde567600b9799"],["/categories/大数据开发/HBase/学习笔记/index.html","098f4a8ff9a559a41af719a7fe0e02b6"],["/categories/大数据开发/HBase/环境搭建/index.html","71f58642fb5868d387fdd0d65ff2fe84"],["/categories/大数据开发/Hadoop/index.html","1c1053315e905e1c479ce818888d332c"],["/categories/大数据开发/Hadoop/技术/index.html","e1429eee2907287879e2eedf0b3a6553"],["/categories/大数据开发/Hadoop/环境搭建/index.html","050b580c7ccae5f0cc20d3a362b30241"],["/categories/大数据开发/Redis/index.html","8511182e5a9fbb70ceca8e42c9536fe1"],["/categories/大数据开发/Redis/技术/index.html","408a45f9c77f74eb96dbe0f44c2ece20"],["/categories/大数据开发/Redis/环境搭建/index.html","6654097a17cdafd63eff1a12de4ae33b"],["/categories/大数据开发/Spark/index.html","3d5a49e6539b73b0b8bae86c8357d582"],["/categories/大数据开发/Spark/环境搭建/index.html","994c1f0430eaa88733042e3eedfeb01a"],["/categories/大数据开发/Zookeeper/index.html","f2af338ab0845ac7e29c5ec7b73fe9b8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","42fed7511e77894fc1f2abeb58eb1411"],["/categories/大数据开发/index.html","558d6c68589ab01a1e2bb39b345e2edf"],["/categories/学校课程/index.html","b1dc6d6d693da7aa513bdc18d486e511"],["/categories/学校课程/计算机操作系统/index.html","f6d93163f96dacaaaebdae397fddfdf1"],["/categories/操作系统/Linux/index.html","2848599eac31fc82bd6a0dfda58a979f"],["/categories/操作系统/Mac/index.html","4d383b9d6866b5f0bb1d1a54b22c7ca1"],["/categories/操作系统/Windows/index.html","9b1c67a94f0c7e2a80a0c8b50c4ebfae"],["/categories/操作系统/index.html","763f1997ee04842cbbdd767374793733"],["/categories/数学建模/index.html","cec817c3059fb635a783e1c5e3a1804a"],["/categories/数学建模/latex/index.html","a60ac10b36399c73331d6c55f62d822a"],["/categories/数学建模/优化类/index.html","8dc8dd456bc8ec55546df8f44e4e91d7"],["/categories/数学建模/优化类/现代优化算法/index.html","81dc90bfcd8621f5b0cc88f4e729b454"],["/categories/数学建模/优化类/规划类/index.html","a3b1f9ab09ac6e76b3337854c3462cd7"],["/categories/数学建模/绘图/index.html","855a0f9ae4ece12958a1558c3d669fea"],["/categories/数据库/MySQL/index.html","1239be29d8775d0eed03d53a6ab883dd"],["/categories/数据库/index.html","d8e21070ad1091ae43df682ebfed876c"],["/categories/数据结构和算法/index.html","aac8910a12b9efde4b216f7a0352a660"],["/categories/数据结构和算法/page/2/index.html","806ad3a98ac9e370318029ae0d24708e"],["/categories/数据结构和算法/基本原理/bfs/index.html","b3249596e8776430df87597ecde9ad67"],["/categories/数据结构和算法/基本原理/dfs/index.html","930357b4d3588745dc409806ef9ff3b7"],["/categories/数据结构和算法/基本原理/index.html","b5a4babc4f44acb14113c43c37924dc2"],["/categories/数据结构和算法/基本原理/动态规划/index.html","39523a1dc497dd99e63872eeae462185"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0f14d0e897151d39df3342f609f20fb5"],["/categories/数据结构和算法/基本原理/图论/index.html","7a0afa542a284ce9f23d3d8f0c93946d"],["/categories/数据结构和算法/基本原理/字符串/index.html","45a5b1047690d058f00302bb48d0b167"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","153bca3b0d893ee28324863d5419782b"],["/categories/数据结构和算法/基本原理/数论/index.html","ebd19f94bc1b7b8d9981d974c75b5704"],["/categories/数据结构和算法/基本原理/树论/index.html","106151ab32f37c62dc1ef14476609dd2"],["/categories/数据结构和算法/基本原理/链表/index.html","2cfca5de1a19835652819283ecfe02ed"],["/categories/数据结构和算法/算法题/index.html","d5ae8faf2d9bb770d286938f3590fb46"],["/categories/数据结构和算法/算法题/二分查找/index.html","d155fac165f1ff0a1c4517fe77b3842d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","971f9ff1a58aa55d77b9c441b0eef85a"],["/categories/数据结构和算法/算法题/动态规划/index.html","56bcf6a4e2441809be19e7edca052bb8"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","f7f058e318a6b7447268262040461cfa"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","7aea5d45d93f1f491957ef4fe1a408c2"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a445b78e621c391cd77659157013a813"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e36a43868216f6bf016283f58b3b4ac3"],["/categories/数据结构和算法/算法题/数论/index.html","1cc1b9618cb228543d0256b4ca272d03"],["/categories/数据结构和算法/算法题/栈和队列/index.html","17351d015962b91ddb53e1a7ba7ca6c7"],["/categories/数据结构和算法/算法题/树论/index.html","ee743750a06d10a945288a994393035a"],["/categories/杂七杂八/index.html","cc302916a2d700ec8856004590bc540e"],["/categories/杂七杂八/博客搭建/index.html","aafb273902bf93e16f9864637e77732a"],["/categories/编程工具下载/index.html","4e3614677392d85dc4e0fe12a5ac2611"],["/categories/编程环境/index.html","79dadc2952b6288d0ff5b85c459dde9e"],["/categories/编程环境/大数据/index.html","151471525af0b3f851b387090417ad87"],["/categories/英语学习/index.html","5efd96985eb099960451b547eac3544f"],["/categories/英语学习/英语语法/index.html","8b6f93a8c5e7a8f12041a15fc87fb321"],["/comments/index.html","d71d97150278223f7ab142160b4dc461"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0dd5d37c606045abbbbbffd95b063245"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ef6d5dabfa1da9fa97170e70d8ed1e03"],["/movies/index.html","6139dadf027793d489a38f2e28b54a5a"],["/music/index.html","a7499b99e8ddd43ac486b8fe89c1e03f"],["/page/2/index.html","b42250980edc1a17f9759b762464ba02"],["/page/3/index.html","2fa036f8bcea0d2911bd939042c2ffba"],["/page/4/index.html","440020579b72947fbca85cd55b22e8ee"],["/page/5/index.html","4697b88e0434103d91b6402d94f6c85d"],["/page/6/index.html","53b6acd1ae5b92efe7d22bcd11f510ed"],["/posts/1021360842.html","5e7ce5b4b5b0329743571fecb401a3a5"],["/posts/1120620192.html","c38a33ab4f1f16305e13e455c788af65"],["/posts/1141628095.html","365bd4244b2ad666e9b3b1ef5dec1a77"],["/posts/1168613674.html","2ac36c848a72034808855436ee353b57"],["/posts/1219920510.html","31a74f61af56c192bfa3868675286431"],["/posts/1222166338.html","31bf2c851976c2cd0251486e50fc9f8e"],["/posts/1259097482.html","1a5ea0fd2ac672eb001bec0dbff1cabe"],["/posts/1271036369.html","33814a23b114db661dbfc741a0d12c24"],["/posts/1312847445.html","9962eb2e91f3ea143901c0f5a9836c3a"],["/posts/135355774.html","74002a04ae87e21db64d4ff34c70a6a7"],["/posts/1375344716.html","4b285305df80625de2c1f472271100da"],["/posts/1388991698.html","c068f5d5cba30546f1fa2a6fc1f6c347"],["/posts/1410315814.html","ab7f22b98df2ffa4b796c5f690d76eaf"],["/posts/1452790229.html","4b47e4026a03783a9bb46398c936eb0b"],["/posts/1470079884.html","d67293801b95f670d29ae9a8d7767f29"],["/posts/1470079885.html","3c76e112c6a7d464c201f69efe8565e0"],["/posts/1470079886.html","df44fc98bcb79c3b0e15d4da2cc7630c"],["/posts/1470079887.html","d30b43ff71baf8ee3c81824dd5cc4162"],["/posts/1498536549.html","b1b49ca9600a812fa2b587063027ef03"],["/posts/1539568593.html","42a73bcbb54f0a754e063b55b86caf6c"],["/posts/1547067935.html","8883b2b28f5c8e2904b9d2fec68863e8"],["/posts/1557866301.html","b5cba390e5f470c65569d7dfa79a19d5"],["/posts/1571776361.html","3b8ab18450e23cf980fc5dd392b1cd24"],["/posts/1605124548.html","61eef6f8beccdefd7cd928650ade4125"],["/posts/1633036852.html","345076777d8931caeb09d17880d53dad"],["/posts/1674202625.html","4374cb2cb48dc67f3ad55ffdc727b60f"],["/posts/1765123828.html","8398190976c7cca576a83502f7790980"],["/posts/1767336200.html","d82d3096f4f2bce996c23ee2c2185015"],["/posts/1776114197.html","e6e61deac61655968640f6124f3397bb"],["/posts/1817748743.html","245f6f273ca85fba3624233b749a6f23"],["/posts/1925125395.html","0f38d91c2e326939fcc807faa7cdf7a2"],["/posts/1966191251.html","ffb699e0a33b0d726b2ac31fddf04548"],["/posts/1987617322.html","b0583c0d76008274eee3d113129e5c6b"],["/posts/1999788039.html","8de3f59b580b649dabcabd03e0cb7244"],["/posts/2075104059.html","58d9f0acaa63006b9a4b2a11c6a20413"],["/posts/2087796737.html","090dbc8a56efd1c215292549087a2165"],["/posts/2106547339.html","4cd50bcfa20685962d6b9178a8613fb8"],["/posts/2207806286.html","917c353a17cea6e4d8a2d531a4672ef9"],["/posts/2225903441.html","5b3087c075049b8e720b586cbfe356e7"],["/posts/2265610284.html","c49d47e23822f29ae245124ff78563e4"],["/posts/2281352001.html","d0a090351a739f2236c176dcf8f4b94c"],["/posts/2364755265.html","db74fea696b339711b722eb568792f89"],["/posts/2414116852.html","5ee0ac4cdd78fa5e2dd4c0b7f1f84716"],["/posts/2421785022.html","337280f7dad4fa16662a5d3d7a962d4b"],["/posts/2482902029.html","1c27a92272f0587804ddf666c6276077"],["/posts/2495386210.html","dd874394c9dc6c22c802380f30740972"],["/posts/2516528882.html","8e71c56c8a0dbaa362f309ab8f454b98"],["/posts/2526659543.html","2d697ea76451b64ff013c2e62fb2012b"],["/posts/2529807823.html","773260190c7e1085bafed73be0b4497a"],["/posts/2596601004.html","54512184ae641e30ef784091e7eae47d"],["/posts/2697614349.html","19be5568c0aad2ada6cf1e2034508523"],["/posts/2742438348.html","8839deb3818da2455842895d019d44d6"],["/posts/2768249503.html","9b6a5b20b32a184d0ffee3018f33a17b"],["/posts/2864584994.html","75ff35edd6e65cc589cf67855583d281"],["/posts/2888309600.html","d2e376d77a9366f15b55568ce46ac8d0"],["/posts/2891591958.html","909b646a4af79d71f8af7d698f02f3a6"],["/posts/2909934084.html","d6160712958f36446bf28b8a3e2aef9e"],["/posts/2920256992.html","39ebcb8dea1fe66c0a670e9786b062b0"],["/posts/2959474469.html","595b368b99290eca8a72411c1c3da2d0"],["/posts/3005926051.html","4c64583d4e9319fd5336cc673f519a92"],["/posts/309775400.html","c2e789766ad0edc744784c028ffb261e"],["/posts/3156194925.html","6843fb929e175af36245e1b424ebf955"],["/posts/3169224211.html","524505ad53b7b8f7b5db4fee70f680c3"],["/posts/3213899550.html","37bd317d2acba2533031ee2b3ae7f650"],["/posts/3259212833.html","6ee6689322651eb331b88a0264b4528f"],["/posts/3266130344.html","0d0fde26103194d9ee5cbfb8929fdf39"],["/posts/3292663995.html","ba4a691b45b94e2ad423a034667d5bfe"],["/posts/3297135020.html","b5751db64ad6fca1da1e102d3fcd36fd"],["/posts/3306641566.html","36cd719de4d622909318b3810db0d952"],["/posts/3312011324.html","d2d0ce3fdb36954f2d00cbdb4152032e"],["/posts/336911618.html","79b33602a1270d6963ac3ea2fcc1533d"],["/posts/3402121571.html","bfb094ace871199879faf323418bdb36"],["/posts/3405577485.html","a52f560b0860c2dc6ee6036eb590166d"],["/posts/3498516849.html","6f10b46aac979aed4e86f5fc960d781c"],["/posts/3513711414.html","31ec551ebd0dda3577464171c384d31b"],["/posts/3523095624.html","67c00956a25c9d886f44476832110eb3"],["/posts/3546711884.html","996c24cea1cfb38f825e44ae4976081b"],["/posts/3731385230.html","dc9adca8182d2b1571490b04198c87b0"],["/posts/3772089482.html","79d224e663f88be77d2591489d62b63e"],["/posts/386609427.html","bcecc183b21463104dda378c64f173f1"],["/posts/4044235327.html","a22a4a1f81ca5f0a414b95a8d59e428d"],["/posts/4115971639.html","b3845f1fec7273f4ec998ee2ea19ab94"],["/posts/4130790367.html","7cd7b5d77f56864a91d6d825df832a06"],["/posts/4131986683.html","e378e704e3a39dcf27c6ee7ced289224"],["/posts/4177218757.html","3af3430bd0f51bc78447341d32f3c47f"],["/posts/4192183953.html","6c0563e0b0c06fa21c8717e03ec35331"],["/posts/4261103898.html","edaca398e37bc72c1c17d8d1cb5b93f8"],["/posts/469711973.html","507f7b919b5e9fc7b59553642c2eaa93"],["/posts/482495853.html","01f7d5d32fb73c9d976454ef7c52ed30"],["/posts/488247922.html","222a03e829f9fde0e6882b9a65b85ac0"],["/posts/517302816.html","4e61d3816fe1fa91a05339de58dd9016"],["/posts/570165348.html","04460b1151c86d2d2f008d11a82f251f"],["/posts/595890772.html","fb77b8c26bc0b8b26a3ab0e011d77225"],["/posts/67485572.html","d2c5078c4f4485097e9258c4b1221d9d"],["/posts/694347442.html","7baad0e488f63b81ed3fd9fcddc277a6"],["/posts/707384687.html","7d3bbb0f7f52ea00259b71597b04245d"],["/posts/71180092.html","782e5fb2ed7b144af9b8e72d631d41a9"],["/posts/716459272.html","a558875e9e0e40ddf510ee6b96798c11"],["/posts/765481613.html","668d1bbfdad92062f4f47df7e7e57d93"],["/posts/778231993.html","8a8f06b7a47f496a3050cd804f269981"],["/posts/795397410.html","04b1f7f2e9904f43aa34fa0bc25a10c8"],["/posts/820223701.html","45b2dc765903d754b485bb97157fb140"],["/posts/830372185.html","8b1d0a457eadd1574e5ce63fd8457f47"],["/posts/88294277.html","a33ea9aeb2931a12532147c5ad5dbe92"],["/posts/939963535.html","edf278b38f6655b4f9d70d4afd133a22"],["/posts/983786067.html","86f3de4f460de82d0de257460e5a1d94"],["/sw-register.js","b21afce1ae6cdb2bdaec6106ba20e771"],["/tags/C/index.html","0ed568fae7b74197618c0b7634225719"],["/tags/C/page/2/index.html","71c042ab2f91aeaadafe067d713e4b5d"],["/tags/C/page/3/index.html","873e49804f674bcec611c669f63974da"],["/tags/C/page/4/index.html","31d1d024b47a63713e36d681a5bce95c"],["/tags/ETL/index.html","e2fd7d085c55f2d3858dcd8d903523b6"],["/tags/ElasticSearch/index.html","19b43d2621ab6a76784fd6adf1f4e767"],["/tags/GUI/index.html","d993bf26c54cded0bea8423159bb0331"],["/tags/HBase/index.html","c5c2f77297e94c34e2d6885fbe845d9c"],["/tags/Hadoop/index.html","e33c2ef160bf7782ee01104947704b8c"],["/tags/Hadoop/page/2/index.html","7528f02d9a84356cb9e11129da8c819c"],["/tags/Java/index.html","d678c6c70cb0e10af0e72b982a3a484f"],["/tags/Java后端/index.html","015661b00357e5e33a3ff8c0d91dc62f"],["/tags/Java后端/page/2/index.html","65f60dac8d019b6f64cdace6a5300ba0"],["/tags/Java基础/index.html","29b0a25f51546ea8502884adf8a80052"],["/tags/Java基础/page/2/index.html","475037b8383ce22c4c2527338a5e52ad"],["/tags/Kettle/index.html","66f86d3eb87f50938732a8934c8064d4"],["/tags/Kibana/index.html","20bee29b861201ed4bd1384fd571c2de"],["/tags/Linux/index.html","d6d8d04ffe3a87d9502227844d7e7c20"],["/tags/Linux/page/2/index.html","427f1a07ec867b81ebea82ea63de8ba6"],["/tags/Linux/page/3/index.html","529aa611d117a1a9de004cdebe99cec8"],["/tags/Mac/index.html","2ac7083b1f45a73ea9de1649dd65aef4"],["/tags/Mac/page/2/index.html","998f17f2fe24147496bd1f0fd0114e0f"],["/tags/Maven/index.html","2715b2cbdc678f120e86e51f7c0ae304"],["/tags/MySQL/index.html","c250eceb2960ebf1f4a7edd9b3554a96"],["/tags/Python/index.html","45af097c0f4685f016754a12ede32113"],["/tags/Redis/index.html","71ca418adc9e1c4c84cea88846ace2a7"],["/tags/R语言/index.html","73b97a9b322738d5ebea130a8b4772c8"],["/tags/Spark/index.html","142c3cec3be13e5e2522a12299fe0728"],["/tags/Ubuntu/index.html","88fe1b9ce08079b17d51456cb9ef71c8"],["/tags/Vue/index.html","60390ffb92ec134dfa26c47583eba2d4"],["/tags/Windows/index.html","c61147c4319a59438dc9123506acaab1"],["/tags/ZooKeeper/index.html","619c4890316ef9f55305ef4397868687"],["/tags/bfs/index.html","187a620071f9eeee714117e8927a3c71"],["/tags/dfs/index.html","538d71a4ac30403195c538c964cf5e5c"],["/tags/folium/index.html","6c11791261bcf2fcb88be3b295387e30"],["/tags/git/index.html","491b4e6e99c16f97551492c4d71d9136"],["/tags/index.html","9ea1018fd9b53768ddfeebf60d401fae"],["/tags/latex/index.html","a3c9a11b9ce027c10950e9738f119e4c"],["/tags/中间件/index.html","d197a6bdcc98f7aa2e06be75273a7004"],["/tags/二分查找/index.html","875b9655b2070dc41ca9d858d72605ae"],["/tags/优化类/index.html","bb88fdc507d2db2291ae01edb1ffa57c"],["/tags/前端/index.html","d9a9eba108e990eb4128344096d39820"],["/tags/前缀和与差分/index.html","7c65960fe18a12af0ecf1d6f80907939"],["/tags/动态规划/index.html","1add1b1c192131ba91da0040a941bb1d"],["/tags/动态规划/page/2/index.html","03c55d4bd0c8354aa18b8d089334da72"],["/tags/博客搭建/index.html","46412ce8b4a9afcc1d0013bc47f5ccde"],["/tags/图论/index.html","e7b107d7662c4ce76357e620554d69aa"],["/tags/大数据/index.html","166eea8b49523a1cb97fc0e9d74479b9"],["/tags/大数据/page/2/index.html","da37f51edf5c32aef9e0f162e18aca87"],["/tags/操作系统/index.html","7dc59c838ba83b83418f02aa809708ed"],["/tags/数学建模/index.html","98e73f85416c6a8d0b88f67c5c166c32"],["/tags/数据库/index.html","4f3156478cadf01fcc5feb12d1dd254d"],["/tags/数据结构和算法/index.html","453b06baa81dbc61dfd1959e45ee2f7f"],["/tags/数据结构和算法/page/2/index.html","5244f1237a7742543710fe4cb7882630"],["/tags/数据结构和算法/page/3/index.html","4aa915d3b1ad998ba2d29884806139af"],["/tags/数据结构和算法/page/4/index.html","ccf3b54299271ad9751cfd957dfcd55f"],["/tags/数组和字符串/index.html","554005bf9be18b55e7657b171dac6387"],["/tags/数论/index.html","49e799231effa20b4e33f533b16cf23a"],["/tags/枚举类/index.html","52ba8b0797ff10a6168a8bbffc8f73d4"],["/tags/栈和队列/index.html","c828bd4c9ec264847fe653cdfc39d9d0"],["/tags/树论/index.html","9e20efcee03ade35da5c2b512c059db0"],["/tags/测试/index.html","29df194ab03ec22dac763be2e6efd41a"],["/tags/环境/index.html","747d88d01aeeb9856e5f45f440b32a8b"],["/tags/环境变量/index.html","6d61ff58c2041923d6bfe5e8f47e5cab"],["/tags/绘图/index.html","e975776da5bee1154911e8506fb17d1c"],["/tags/编程工具/index.html","4ec1b7dda7d5dba1e672a7273ce3b944"],["/tags/编程环境/index.html","17ebba3cb00e9203fc7556d373f56a4b"],["/tags/网络编程/index.html","5a9aaa7c13455b9c01bbce56954212e6"],["/tags/英语语法/index.html","18688deb8c184200d8ddd53da2c89601"],["/tags/计算机操作系统/index.html","f7db9f56f9de29cad6997f4eb81caa88"],["/tags/论文/index.html","dafa31fc324f61dc6c3d31eee2debb70"],["/tags/资源下载/index.html","ad273490394f79d96da1fe93fac51fc9"],["/tags/链表/index.html","73dd895fb8ebf1cf4ab8dd37662da608"],["/tags/集合/index.html","986be70317e775846cf3c49aa1dd41bf"],["/tags/集群/index.html","271b687d2bca2c31cd951ba6fda978c9"]];
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
