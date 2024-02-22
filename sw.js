/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ab4dcb8bb910f1f6ff7f9ac46cd8be0e"],["/about/index.html","4435204f0ebf8524ba015d5a34a111d0"],["/archives/2023/01/index.html","efbdc2368e04acf5d7d14a6a3ab360bd"],["/archives/2023/02/index.html","b31a75b928ef53e995f0a9aeda08406a"],["/archives/2023/02/page/2/index.html","1e1deb1bf95fc1521e5eaa8c26b79a2e"],["/archives/2023/03/index.html","1fbf3a22b5d9aac378eef6d0d96dcd47"],["/archives/2023/05/index.html","9732f1e2e6f754a85eb88ea57be764c2"],["/archives/2023/06/index.html","63b051bff71a5474252ced791409c31d"],["/archives/2023/09/index.html","ee9027795905dcb85166e1dca38ede6f"],["/archives/2023/11/index.html","3953861d16ca687ce280ecfabc8ed462"],["/archives/2023/12/index.html","9f5e562b993fe62f6a42f42af02d71ec"],["/archives/2023/index.html","deff7e83d4fef199bd5d559ec91c8511"],["/archives/2023/page/2/index.html","8d513fa9e27a81548c7c6eeb9649e28d"],["/archives/2023/page/3/index.html","ebfa33190e8136de86738e99bef18531"],["/archives/2023/page/4/index.html","eaeb3895717f8bd6abb52d3b49ea18c5"],["/archives/2024/02/index.html","81d07a54ba926a366710a1999f6c7478"],["/archives/2024/index.html","1e95ab78d9931e4018e61b3597356ef5"],["/archives/index.html","b574a86cf7c91fe7415fe04631236c05"],["/archives/page/2/index.html","ebf2cad4f7d2369515e256541c1693f4"],["/archives/page/3/index.html","5584cd538170f3d09fb9345331fa55a2"],["/archives/page/4/index.html","57d2d625ac32a0f5acdc84e99864d670"],["/baidu_verify_codeva-qQP2iZOMLX.html","b8c0f8fe835790f9ca2c0761c896c4e8"],["/categories/Java/index.html","122eb2d64445d4cce47dce78a12e42bc"],["/categories/Java/后端/index.html","5fe62c9ac2c75f4edd7195c54041eaac"],["/categories/Java/基础/index.html","e26b5357e59cab1ed12cf7f39a126209"],["/categories/Java/基础/集合/index.html","0db8fb4e7908a9db483a3d38bc30bcd5"],["/categories/Python/index.html","381fe9449e1a419dea8d6cdf7a887c54"],["/categories/Python/编程环境/index.html","cb0c75d86c746d2ab32965259fda391e"],["/categories/R语言/index.html","37b1df2813e0f1be0eb16fc9a7ca617a"],["/categories/R语言/编程环境/index.html","44cbbda0b92cb2bb3c76006ccd70e0b9"],["/categories/index.html","24a2dcccbfea320463ace722dd4dc30f"],["/categories/中间件/index.html","e6364e23baa2b0228c8672be8766f1d6"],["/categories/前端/Vue/index.html","b91dfde6d7dc755dddec3d440a9e0992"],["/categories/前端/index.html","b639e08d4c317d6a8d134757938a2e6b"],["/categories/大数据开发/ElasticSearch/index.html","295a1959f74f139f2d93344f1b97286d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","e780b946a38604cae8f1a08628e166e0"],["/categories/大数据开发/HBase/index.html","16b00e7d6eaa1cb16a7f1f3f6f09bb10"],["/categories/大数据开发/HBase/学习笔记/index.html","a413a34e91dc74fa1839d8f2f5f28b50"],["/categories/大数据开发/HBase/环境搭建/index.html","8aa0ec33f8595f606b5a104f5fcf840b"],["/categories/大数据开发/Hadoop/index.html","fa11af6d0a1df1a87edab3d7f21475b8"],["/categories/大数据开发/Hadoop/技术/index.html","15f871eb6211a7b0a72f07ae02ab9720"],["/categories/大数据开发/Hadoop/环境搭建/index.html","37757babe240fe5b56ca6187d5719193"],["/categories/大数据开发/Redis/index.html","11236ba6ab58099e5cf5449bca248c22"],["/categories/大数据开发/Redis/技术/index.html","5e2c19234918202fce22cd1b9b9dcafa"],["/categories/大数据开发/Redis/环境搭建/index.html","0bc2b110d319aec858f09c5fb518502d"],["/categories/大数据开发/Spark/index.html","e4beb5f6b720556be7d69caf3f053fb5"],["/categories/大数据开发/Spark/环境搭建/index.html","2f0a93a2b442aeeb726e9bfdd39281be"],["/categories/大数据开发/Zookeeper/index.html","8ed93f8967facc9d1ba197fad7c18d35"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1dead2f5c7667b3607f681db2552f0e5"],["/categories/大数据开发/index.html","a9220ccc014e877665ace18b47f76fc9"],["/categories/学校课程/index.html","33701b6997598e0b3a41d46fc33f6bfc"],["/categories/学校课程/计算机操作系统/index.html","49d8dc56a9d310be0c9dff42457b7de7"],["/categories/操作系统/Linux/index.html","2bb966246e9d826c3f75de9201e603b3"],["/categories/操作系统/Mac/index.html","24cfe1fb14232b28f2e0a76820cb992a"],["/categories/操作系统/Windows/index.html","3fb82ccc4bf0b719e840b08e65753be7"],["/categories/操作系统/index.html","2e7c65101ed94771ac39bc1923106306"],["/categories/数学建模/index.html","f9cb5e33d4d77e7269b27c4dcf9e4616"],["/categories/数学建模/latex/index.html","a6bc4c678c2665f707277130e3651f03"],["/categories/数学建模/优化类/index.html","f05e983fe23b0b6f4945cbc3a81c8c58"],["/categories/数学建模/优化类/现代优化算法/index.html","c9dc3b8d8ed375881b18941335d5b5b5"],["/categories/数学建模/优化类/规划类/index.html","1e02cc067d6388e3230d34a3be788fa8"],["/categories/数学建模/绘图/index.html","30cba848dc6cb1b679cca9b510a38270"],["/categories/数据库/MySQL/index.html","cad32225b0fdbf77d06d1602d20b2f7e"],["/categories/数据库/index.html","c8298737e82c66fe79867e24eeac37be"],["/categories/数据结构和算法/index.html","eebd0b8281ed7f6b72080874c50295d7"],["/categories/数据结构和算法/page/2/index.html","0574e07fa17aa1ff596a0c99dd4cc324"],["/categories/数据结构和算法/基本原理/bfs/index.html","ec6c1a3286f8e76b5f8fd307c7290848"],["/categories/数据结构和算法/基本原理/dfs/index.html","8e785b806a572f0580f3c7da02ca46bf"],["/categories/数据结构和算法/基本原理/index.html","60de8581d24c0b25b9804e0f11f831fb"],["/categories/数据结构和算法/基本原理/动态规划/index.html","5ff96294f5abd4e19b071988b344e338"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","2bf2065ae83f002ad187525bbde8f9cd"],["/categories/数据结构和算法/基本原理/图论/index.html","92c4e94108ccf0ca5f0349f8ca6086a9"],["/categories/数据结构和算法/基本原理/字符串/index.html","0bd7da82e236107e9bc0c661417ae6f2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1863c40611e19cfaf46e556e9de5651d"],["/categories/数据结构和算法/基本原理/数论/index.html","126f128a99f59cae942c9003cccb420c"],["/categories/数据结构和算法/基本原理/树论/index.html","a1f2ac64569897b02abae6f317520dbe"],["/categories/数据结构和算法/基本原理/链表/index.html","b104410f3764ceecdbf1a016e54f21b8"],["/categories/数据结构和算法/算法题/index.html","694277ab073e577728ea73aa87e8a610"],["/categories/数据结构和算法/算法题/二分查找/index.html","a81a6ec6c4cd7e0b648d60915190c706"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5cefe2328508cc4c39609e46a7cad6be"],["/categories/数据结构和算法/算法题/动态规划/index.html","bb0608ec738b25267094184d483c0492"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","607c39092bf25e6032d393e698dbf2b1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","4a8d9f3f754c9701ab736327cb00a111"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","0f7f72735be620469369bb220560b46d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b860e1714a9c5d1188c162c01b98f04f"],["/categories/数据结构和算法/算法题/数论/index.html","fdd387e1c5683f846fe23de77e980401"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d7575572e29a679f639048f2b0376233"],["/categories/数据结构和算法/算法题/树论/index.html","1c8d03dada78b9e3b6605babaeb9f685"],["/categories/杂七杂八/index.html","41a5fd9a6d14f83979fe684def97ce6c"],["/categories/杂七杂八/博客搭建/index.html","7174e724b6e5782c0c3e1712437f0071"],["/categories/编程工具下载/index.html","bc3d1d8e0bb131a67092485505f664dc"],["/categories/编程环境/index.html","4b990f2bca9ea6e8edcb7b672cf4e2e0"],["/categories/编程环境/大数据/index.html","3f0b9bbea163d0c7de12948d71644d78"],["/categories/英语学习/index.html","531a8a92518a9a03529d49414fcd41b3"],["/categories/英语学习/英语语法/index.html","9fa8d11eaeed45ff1d3d07b7d257ff1b"],["/comments/index.html","d9dbdb92f671ecd1268c974e7feeda00"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cf8e14f2183670692c0e3aaf7c853ddf"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ecaeb8177fe5a937ab127f3e1e94663f"],["/movies/index.html","3532138074bc7c3252ad03b4f0a8a416"],["/music/index.html","8da94e1ae20964575841eaeb3bf0c76c"],["/page/2/index.html","ba92c0ed55ab5bfde9d95e47b6243473"],["/page/3/index.html","d67a277417fcb6de105e1c659be1c0ff"],["/page/4/index.html","51e8e4c2c53525a363c93ff3380c6cd1"],["/page/5/index.html","704f1c9df1e8929fce3910473347286e"],["/page/6/index.html","958f75cedf1ce6569e32b3109b065f80"],["/posts/1021360842.html","5e7ce5b4b5b0329743571fecb401a3a5"],["/posts/1120620192.html","c38a33ab4f1f16305e13e455c788af65"],["/posts/1141628095.html","365bd4244b2ad666e9b3b1ef5dec1a77"],["/posts/1168613674.html","2ac36c848a72034808855436ee353b57"],["/posts/1219920510.html","31a74f61af56c192bfa3868675286431"],["/posts/1222166338.html","31bf2c851976c2cd0251486e50fc9f8e"],["/posts/1259097482.html","1a5ea0fd2ac672eb001bec0dbff1cabe"],["/posts/1271036369.html","33814a23b114db661dbfc741a0d12c24"],["/posts/1312847445.html","9962eb2e91f3ea143901c0f5a9836c3a"],["/posts/135355774.html","74002a04ae87e21db64d4ff34c70a6a7"],["/posts/1375344716.html","4b285305df80625de2c1f472271100da"],["/posts/1388991698.html","c068f5d5cba30546f1fa2a6fc1f6c347"],["/posts/1410315814.html","ab7f22b98df2ffa4b796c5f690d76eaf"],["/posts/1452790229.html","4b47e4026a03783a9bb46398c936eb0b"],["/posts/1470079884.html","d67293801b95f670d29ae9a8d7767f29"],["/posts/1470079885.html","3c76e112c6a7d464c201f69efe8565e0"],["/posts/1470079886.html","df44fc98bcb79c3b0e15d4da2cc7630c"],["/posts/1470079887.html","d30b43ff71baf8ee3c81824dd5cc4162"],["/posts/1498536549.html","b1b49ca9600a812fa2b587063027ef03"],["/posts/1539568593.html","42a73bcbb54f0a754e063b55b86caf6c"],["/posts/1547067935.html","8883b2b28f5c8e2904b9d2fec68863e8"],["/posts/1557866301.html","b5cba390e5f470c65569d7dfa79a19d5"],["/posts/1571776361.html","3b8ab18450e23cf980fc5dd392b1cd24"],["/posts/1605124548.html","61eef6f8beccdefd7cd928650ade4125"],["/posts/1633036852.html","345076777d8931caeb09d17880d53dad"],["/posts/1674202625.html","4374cb2cb48dc67f3ad55ffdc727b60f"],["/posts/1765123828.html","8398190976c7cca576a83502f7790980"],["/posts/1767336200.html","d82d3096f4f2bce996c23ee2c2185015"],["/posts/1776114197.html","e6e61deac61655968640f6124f3397bb"],["/posts/1817748743.html","245f6f273ca85fba3624233b749a6f23"],["/posts/1925125395.html","0f38d91c2e326939fcc807faa7cdf7a2"],["/posts/1966191251.html","ffb699e0a33b0d726b2ac31fddf04548"],["/posts/1987617322.html","b0583c0d76008274eee3d113129e5c6b"],["/posts/1999788039.html","8de3f59b580b649dabcabd03e0cb7244"],["/posts/2075104059.html","58d9f0acaa63006b9a4b2a11c6a20413"],["/posts/2087796737.html","090dbc8a56efd1c215292549087a2165"],["/posts/2106547339.html","4cd50bcfa20685962d6b9178a8613fb8"],["/posts/2207806286.html","917c353a17cea6e4d8a2d531a4672ef9"],["/posts/2225903441.html","5b3087c075049b8e720b586cbfe356e7"],["/posts/2265610284.html","c49d47e23822f29ae245124ff78563e4"],["/posts/2281352001.html","d0a090351a739f2236c176dcf8f4b94c"],["/posts/2364755265.html","db74fea696b339711b722eb568792f89"],["/posts/2414116852.html","5ee0ac4cdd78fa5e2dd4c0b7f1f84716"],["/posts/2421785022.html","337280f7dad4fa16662a5d3d7a962d4b"],["/posts/2482902029.html","1c27a92272f0587804ddf666c6276077"],["/posts/2495386210.html","dd874394c9dc6c22c802380f30740972"],["/posts/2516528882.html","8e71c56c8a0dbaa362f309ab8f454b98"],["/posts/2526659543.html","2d697ea76451b64ff013c2e62fb2012b"],["/posts/2529807823.html","773260190c7e1085bafed73be0b4497a"],["/posts/2596601004.html","54512184ae641e30ef784091e7eae47d"],["/posts/2697614349.html","19be5568c0aad2ada6cf1e2034508523"],["/posts/2742438348.html","8839deb3818da2455842895d019d44d6"],["/posts/2768249503.html","9b6a5b20b32a184d0ffee3018f33a17b"],["/posts/2864584994.html","75ff35edd6e65cc589cf67855583d281"],["/posts/2888309600.html","d2e376d77a9366f15b55568ce46ac8d0"],["/posts/2891591958.html","909b646a4af79d71f8af7d698f02f3a6"],["/posts/2909934084.html","d6160712958f36446bf28b8a3e2aef9e"],["/posts/2920256992.html","39ebcb8dea1fe66c0a670e9786b062b0"],["/posts/2959474469.html","595b368b99290eca8a72411c1c3da2d0"],["/posts/3005926051.html","4c64583d4e9319fd5336cc673f519a92"],["/posts/309775400.html","c2e789766ad0edc744784c028ffb261e"],["/posts/3156194925.html","6843fb929e175af36245e1b424ebf955"],["/posts/3169224211.html","524505ad53b7b8f7b5db4fee70f680c3"],["/posts/3213899550.html","37bd317d2acba2533031ee2b3ae7f650"],["/posts/3259212833.html","6ee6689322651eb331b88a0264b4528f"],["/posts/3266130344.html","0d0fde26103194d9ee5cbfb8929fdf39"],["/posts/3292663995.html","ba4a691b45b94e2ad423a034667d5bfe"],["/posts/3297135020.html","b5751db64ad6fca1da1e102d3fcd36fd"],["/posts/3306641566.html","36cd719de4d622909318b3810db0d952"],["/posts/3312011324.html","d2d0ce3fdb36954f2d00cbdb4152032e"],["/posts/336911618.html","79b33602a1270d6963ac3ea2fcc1533d"],["/posts/3402121571.html","bfb094ace871199879faf323418bdb36"],["/posts/3405577485.html","a52f560b0860c2dc6ee6036eb590166d"],["/posts/3498516849.html","6f10b46aac979aed4e86f5fc960d781c"],["/posts/3513711414.html","31ec551ebd0dda3577464171c384d31b"],["/posts/3523095624.html","67c00956a25c9d886f44476832110eb3"],["/posts/3546711884.html","996c24cea1cfb38f825e44ae4976081b"],["/posts/3731385230.html","dc9adca8182d2b1571490b04198c87b0"],["/posts/3772089482.html","79d224e663f88be77d2591489d62b63e"],["/posts/386609427.html","bcecc183b21463104dda378c64f173f1"],["/posts/4044235327.html","a22a4a1f81ca5f0a414b95a8d59e428d"],["/posts/4115971639.html","b3845f1fec7273f4ec998ee2ea19ab94"],["/posts/4130790367.html","7cd7b5d77f56864a91d6d825df832a06"],["/posts/4131986683.html","e378e704e3a39dcf27c6ee7ced289224"],["/posts/4177218757.html","3af3430bd0f51bc78447341d32f3c47f"],["/posts/4192183953.html","6c0563e0b0c06fa21c8717e03ec35331"],["/posts/4261103898.html","edaca398e37bc72c1c17d8d1cb5b93f8"],["/posts/469711973.html","507f7b919b5e9fc7b59553642c2eaa93"],["/posts/482495853.html","01f7d5d32fb73c9d976454ef7c52ed30"],["/posts/488247922.html","222a03e829f9fde0e6882b9a65b85ac0"],["/posts/517302816.html","4e61d3816fe1fa91a05339de58dd9016"],["/posts/570165348.html","04460b1151c86d2d2f008d11a82f251f"],["/posts/595890772.html","fb77b8c26bc0b8b26a3ab0e011d77225"],["/posts/67485572.html","d2c5078c4f4485097e9258c4b1221d9d"],["/posts/694347442.html","7baad0e488f63b81ed3fd9fcddc277a6"],["/posts/707384687.html","7d3bbb0f7f52ea00259b71597b04245d"],["/posts/71180092.html","782e5fb2ed7b144af9b8e72d631d41a9"],["/posts/716459272.html","a558875e9e0e40ddf510ee6b96798c11"],["/posts/765481613.html","668d1bbfdad92062f4f47df7e7e57d93"],["/posts/778231993.html","8a8f06b7a47f496a3050cd804f269981"],["/posts/795397410.html","04b1f7f2e9904f43aa34fa0bc25a10c8"],["/posts/820223701.html","45b2dc765903d754b485bb97157fb140"],["/posts/830372185.html","8b1d0a457eadd1574e5ce63fd8457f47"],["/posts/88294277.html","a33ea9aeb2931a12532147c5ad5dbe92"],["/posts/939963535.html","edf278b38f6655b4f9d70d4afd133a22"],["/posts/983786067.html","86f3de4f460de82d0de257460e5a1d94"],["/sw-register.js","81a4d25561cecb928086596853a14bed"],["/tags/C/index.html","7a9a9d083886c1cf2230f294654dce5b"],["/tags/C/page/2/index.html","60bcc72b530f7f03895b16f4277b618a"],["/tags/C/page/3/index.html","6ebdd3a75ba6b1d6fb3725cbf384a70c"],["/tags/C/page/4/index.html","5bb58a0e196b85081d37eee900fa2d60"],["/tags/ETL/index.html","7ff0d7936bc7d396d560218f9e8093f7"],["/tags/ElasticSearch/index.html","157ac6e9a8ae6fa3307cf3b497bfbde6"],["/tags/GUI/index.html","09d82dc111527d8ee964374e3a05e3bf"],["/tags/HBase/index.html","bffa7c41c517d9631fda979bb71665ce"],["/tags/Hadoop/index.html","b4763eb0522b4b3cec9eb9be24146fa2"],["/tags/Hadoop/page/2/index.html","f54e59f7204f01862a047e56ebcc58d9"],["/tags/Java/index.html","c7a2002270ce53fe6ad7b003cba910ff"],["/tags/Java后端/index.html","967ee98604af659709c531fe48d6e14f"],["/tags/Java后端/page/2/index.html","53ca6e40e729e6bb1b6b47f75d786f01"],["/tags/Java基础/index.html","e2cb629d069cf991553519bdb31fe538"],["/tags/Java基础/page/2/index.html","cb622972e67eb7678d3275c69166e0c8"],["/tags/Kettle/index.html","dea166b09f4fb861a740a92e5169ab51"],["/tags/Kibana/index.html","9d8018bb914b5694acc34ff37f734c30"],["/tags/Linux/index.html","649940255da3383e3430f8f13aeec712"],["/tags/Linux/page/2/index.html","511aa0ca9113e151d088956b0d0dbd60"],["/tags/Linux/page/3/index.html","886c6ee2084ce8136329bf105bce7176"],["/tags/Mac/index.html","17bf1834d91803c60f87764c4920fa30"],["/tags/Mac/page/2/index.html","5bbdbf3b70fcc316d98dcfaebdaf0df0"],["/tags/Maven/index.html","47ca1b0504b603eb53fae00129f67397"],["/tags/MySQL/index.html","863b7b9a2ee59abe0ce2f43838bc642c"],["/tags/Python/index.html","a5683bb2411cf5d73754aa670669344a"],["/tags/Redis/index.html","182602002283ce6079df8a5991ab44a4"],["/tags/R语言/index.html","a38a8930e540cec3d224e9330c2c5a58"],["/tags/Spark/index.html","31388e7a746b0e26fd770825d5cbc71a"],["/tags/Ubuntu/index.html","f748489cb2a68c2804f65a3d4bc9c5b0"],["/tags/Vue/index.html","4d3a1cb9f80b47aaa7516c13a8f31e16"],["/tags/Windows/index.html","7a7f46f17c61155711c90b62ff1f996f"],["/tags/ZooKeeper/index.html","ff92a4e729092d1080f931b54b4fe453"],["/tags/bfs/index.html","2b8d9b8fd8fdfddc05d1ba247cfbafd3"],["/tags/dfs/index.html","2a84d308a77626c84669ee3e931864b5"],["/tags/folium/index.html","88acb2a05a56e880c1363f8813231ad4"],["/tags/git/index.html","96d243fb2d75c2de747da319e4f46610"],["/tags/index.html","a635228ccba5f0079d4a6cbbe988e23c"],["/tags/latex/index.html","9120390c26db0b5e28749d514eb0f888"],["/tags/中间件/index.html","5d107108ca112819adb568144c3ebaff"],["/tags/二分查找/index.html","b7fb124c5c73ff1a370a4946ebc9c7f3"],["/tags/优化类/index.html","7d37635c2ecb5fd0259e41518cfe177a"],["/tags/前端/index.html","c71f780fb7d738177cd95a74408485fb"],["/tags/前缀和与差分/index.html","f25f8c8d89b561ddbddc9bc63e032be7"],["/tags/动态规划/index.html","d7be37911588e93517dca410b6687056"],["/tags/动态规划/page/2/index.html","3bc55d5d856bc44de680328dda47b49f"],["/tags/博客搭建/index.html","4fea33e9ad78323706151456cd34c2a3"],["/tags/图论/index.html","844f29fa31fb0cd1256f786a7a76744d"],["/tags/大数据/index.html","417a5a50513d1e31ae2b2f5494275e93"],["/tags/大数据/page/2/index.html","8c6d3c3ab5eb1b06dd966c900095f5a4"],["/tags/操作系统/index.html","4bc6fa518ede7d584e0389173b7ff1b0"],["/tags/数学建模/index.html","7eac40b2789557fce23c81cfab39c113"],["/tags/数据库/index.html","244ef32c49960d1c2806dc01813fb9b7"],["/tags/数据结构和算法/index.html","7b645bee67551f24cf1c2fe8db9e7dd6"],["/tags/数据结构和算法/page/2/index.html","80134dad5d1091d009f805e06f438a34"],["/tags/数据结构和算法/page/3/index.html","f2e575f00a2285e2d62a085e09422384"],["/tags/数据结构和算法/page/4/index.html","9dfe03222a406a67887443855e366ea2"],["/tags/数组和字符串/index.html","200a2b756257c836625fb81ab756110e"],["/tags/数论/index.html","ab17f04106a09343402406e0ad14efca"],["/tags/枚举类/index.html","8f51bb588fdb7c6acb8c3c1b114a631d"],["/tags/栈和队列/index.html","4208b5e8e216541921313be28684906b"],["/tags/树论/index.html","b3ea0bae231360e681ca893985ebf930"],["/tags/测试/index.html","bcfdcfb06e31264dc5ab4ce46ee3f824"],["/tags/环境/index.html","53d0227ddd414d41345e550660560609"],["/tags/环境变量/index.html","2a58684b623f404e49d1f95ebbdb6e1e"],["/tags/绘图/index.html","c688eef1e3e05de63901600ce8614318"],["/tags/编程工具/index.html","b8e33349ae4b79b6f28aae224d704f2d"],["/tags/编程环境/index.html","c7296a6322a27bfcd2b98aa3ad11f40f"],["/tags/网络编程/index.html","e82f2250fed003665374f8d5b5f6a781"],["/tags/英语语法/index.html","ee2be22a66ab281d73dabfcba5feb268"],["/tags/计算机操作系统/index.html","1d21d0131cc409b75774ce4ff65e4774"],["/tags/论文/index.html","053fc09e4e7ebd4b15ca62758c3e116c"],["/tags/资源下载/index.html","e0d52d72e868c9046c1b640b6933ac77"],["/tags/链表/index.html","e9fd0c1fd1224eb080f5db2dcaa91941"],["/tags/集合/index.html","825f5e55680f3958d8f60a80173f496a"],["/tags/集群/index.html","cd37a349ed595cffd7c12eb7111d2836"]];
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
