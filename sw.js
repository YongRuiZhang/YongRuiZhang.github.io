/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","25a500dd2152016b78088d7d86a1aabf"],["/about/index.html","ba05a65fe54eb1910c4aa8c1b6af7a2d"],["/archives/2023/01/index.html","e3934d33edd5c613f4eff3aa6d989319"],["/archives/2023/02/index.html","6a40f81585ddb591caf3b243758a4754"],["/archives/2023/02/page/2/index.html","0b492da1aabd5d338654eb2fa29a4ecf"],["/archives/2023/02/page/3/index.html","3d445edfa0f2b10df112d4a09cd2e22e"],["/archives/2023/03/index.html","a77517764d818bb33f1124d13043d3d4"],["/archives/2023/05/index.html","4b17712a3738d17620fa2bd9cfbbbe05"],["/archives/2023/06/index.html","0fc3b201cbc8b4001da64f7b93bec898"],["/archives/2023/09/index.html","d1d8c52b79ec88d56fe72ae93d7cd8bb"],["/archives/2023/11/index.html","1bce5307081afec989e7e92b88687753"],["/archives/2023/12/index.html","e38a79fa84c7eec1f9390a0fb78b0db3"],["/archives/2023/index.html","acc2f0d75f85e7f00adc8d930afeb264"],["/archives/2023/page/2/index.html","035e989d3d3e49c408f201b97bb37f21"],["/archives/2023/page/3/index.html","ffc81db3a64b80397db05a5d3866bb86"],["/archives/2023/page/4/index.html","38fec70974a1c52dad6954f66a2d5d22"],["/archives/2023/page/5/index.html","0068daf486ede88b2e019c969266d4c4"],["/archives/2024/02/index.html","f189a2663a58133e333ac35bdd4508b9"],["/archives/2024/index.html","cdaad5c54c8a5d827acf447f612d24f7"],["/archives/index.html","39ea67dd4edb2b59361c6d2234c5554e"],["/archives/page/2/index.html","17b58999c12b37cbeeb146a4d8aa169c"],["/archives/page/3/index.html","4b5913ed9083e3bb8bceeb13b4f200fa"],["/archives/page/4/index.html","b8a910b798d06634d72c026b7a08dbe1"],["/archives/page/5/index.html","52a425f9be2f0db865bac627f83dc9a0"],["/baidu_verify_codeva-qQP2iZOMLX.html","8b25e0b60d8eb899a00f2a4930bf8885"],["/categories/Java/index.html","46c8333392ad3820a218695403a2c50e"],["/categories/Java/后端/index.html","0d6cb53c0d3a2b9f1b6bc71e67ac256c"],["/categories/Java/基础/index.html","3e710d4fb15066b1344fba46c25ba95d"],["/categories/Java/基础/集合/index.html","c8d65e582849a6ee2f76758bef656836"],["/categories/Python/index.html","b6cf986a182e0d10a8c9c8cb1f2cc306"],["/categories/Python/编程环境/index.html","d4fd227cc8c806528468d08186cfc992"],["/categories/R语言/index.html","9b389893b441b50c317edb61aa2b8ed4"],["/categories/R语言/编程环境/index.html","4f8f5aec821492fdbaf7098eb9c70837"],["/categories/iPad/index.html","e7a4e2ee73c1826e930efe4bdf87157c"],["/categories/index.html","d001105ac6d2a3ef6700e41e1857e596"],["/categories/中间件/index.html","b3d07350bf6ce5afeee13c9efcd09cd8"],["/categories/前端/Vue/index.html","b01cdb431b94cc5176703561cf518bef"],["/categories/前端/index.html","f8e3585cd8618990559aa14b180b9f81"],["/categories/大数据开发/ElasticSearch/index.html","06b8199e0696edf61e0afc0895eeedfc"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","35040bab850e6fb509567a663ddbd15b"],["/categories/大数据开发/HBase/index.html","0aa270a9061678fcb2781b8f28a7e7bc"],["/categories/大数据开发/HBase/学习笔记/index.html","04b7f31aadde7a6ed08ab7ab846aa176"],["/categories/大数据开发/HBase/环境搭建/index.html","9ba19b84c4f4cfb042fcb1e55a02018c"],["/categories/大数据开发/Hadoop/index.html","3d1db0512a2e01479d399d92ffb8a6db"],["/categories/大数据开发/Hadoop/技术/index.html","cef67faf4cee042f8ad8fd2cf77419f0"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5bdfc86cb0f7fdc69977cfc96de015d3"],["/categories/大数据开发/Redis/index.html","6a7e1b74fc90d2b868ded87233957900"],["/categories/大数据开发/Redis/技术/index.html","ddd5caeb5a50494e0edda632bcb693c6"],["/categories/大数据开发/Redis/环境搭建/index.html","990cb8dfeb953d4f1343a464c6bf3613"],["/categories/大数据开发/Spark/index.html","b7554b41fd094410d19643b1cc698f6d"],["/categories/大数据开发/Spark/环境搭建/index.html","28a19bc0733d71ba0b9ce43f7f22ade8"],["/categories/大数据开发/Zookeeper/index.html","8cc71377c7029dda970d062a5eb4332b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e85ec59c8221d4edfc942fc2dbf88f5c"],["/categories/大数据开发/index.html","d42cc16a6f0353b00b52b54d6d2ca595"],["/categories/学校课程/index.html","a9c6451522664221fe8336fd765ff890"],["/categories/学校课程/计算机操作系统/index.html","81a0a905ea137ae65c92d884c9cfbccf"],["/categories/操作系统/Linux/index.html","53f56bb18962dd75b1dc17aefbcd23d2"],["/categories/操作系统/Mac/index.html","674869ead1034a20023218ecc673d14c"],["/categories/操作系统/Windows/index.html","4e1ec16c8f13f4e487b66007ce9e2fe4"],["/categories/操作系统/index.html","e697ff98f5f3bf78df8d9658dff577e1"],["/categories/数学建模/index.html","2f2f2c82be8f90250bc9ec066a64970a"],["/categories/数学建模/latex/index.html","f0d7b50717e519eced3f46d2b8c5edde"],["/categories/数学建模/优化类/index.html","f881a48bc85be89627cc2833628a66a7"],["/categories/数学建模/优化类/现代优化算法/index.html","40fe4945229d0af144d491cab1433700"],["/categories/数学建模/优化类/规划类/index.html","04fa57b3382f72fe495b657a3286e77b"],["/categories/数学建模/绘图/index.html","82dcea77a7f688e86ccf24866467fe30"],["/categories/数据库/MySQL/index.html","8451ecfd7b22891b8ca0d2ef7bdeb36e"],["/categories/数据库/index.html","d9c42af9074fcdd7529198e4679dd623"],["/categories/数据结构和算法/index.html","cb7abe789cc88eb0cae144cd6d5d0b39"],["/categories/数据结构和算法/page/2/index.html","b530906582126809402e7110bd3bf718"],["/categories/数据结构和算法/基本原理/bfs/index.html","0f7afb58c935a84c694212417101cfa2"],["/categories/数据结构和算法/基本原理/dfs/index.html","842f1c29c126e66c264a737b2af4704a"],["/categories/数据结构和算法/基本原理/index.html","2b74d359fe4ca306dba7078720869db9"],["/categories/数据结构和算法/基本原理/动态规划/index.html","2d96348dc8f2789a9e083fcae1e1e038"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","7a845d21dfaf37f973decb0c646bebee"],["/categories/数据结构和算法/基本原理/图论/index.html","2aff342cfbe58f507ec8198b25965ce8"],["/categories/数据结构和算法/基本原理/字符串/index.html","1593d459d2b9ab2e12a1a84c9d1d8966"],["/categories/数据结构和算法/基本原理/排序/index.html","c8a02866531c127e958e35ee8d6c2b7e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1377c8aa39f7c50b4be9b10ed9ff63e2"],["/categories/数据结构和算法/基本原理/数论/index.html","689fe0f280b19d6e718184d2f533f750"],["/categories/数据结构和算法/基本原理/树论/index.html","df5e91e9ed75842bed37e4379330f49d"],["/categories/数据结构和算法/基本原理/链表/index.html","48dea703dc6cd21038f891bb169ec770"],["/categories/数据结构和算法/算法题/index.html","60c97b068336e6e18c54bf7402a92e07"],["/categories/数据结构和算法/算法题/二分查找/index.html","47ff8b88b73e24b8938161edb0dc9d8f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6ddc5de9f86ac67677eec3b29d887bbb"],["/categories/数据结构和算法/算法题/动态规划/index.html","78dc28cacb86397fcf9c6efc2a389f34"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","8296d573c69d2ee0e52132647e4380d5"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","fda8a706d51c859664df2db25a54170a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4783ed8eca8856b0ddc608f261812da4"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e5f40ccdc62e7fe30606e0bb0d537320"],["/categories/数据结构和算法/算法题/数论/index.html","9b38c5d340601e25ea35844ede9b27bf"],["/categories/数据结构和算法/算法题/栈和队列/index.html","14f733a33a3c700d190bbab38c005c8d"],["/categories/数据结构和算法/算法题/树论/index.html","0aef8390c0ad504aa5d73d6e70927bfe"],["/categories/杂七杂八/index.html","40054516559322c73526956265a23f7c"],["/categories/杂七杂八/博客搭建/index.html","296d42cd2561aadedebbd9230c7d5fde"],["/categories/编程工具下载/index.html","1e5ba6962057347fbafc0d5393819397"],["/categories/编程环境/index.html","431c92380e6c1362145589b3bc84267a"],["/categories/编程环境/大数据/index.html","90297f6309d64f4cb327c005bf913694"],["/categories/英语学习/index.html","c9d65c8442b0e045d3d11bd611f5a565"],["/categories/英语学习/英语语法/index.html","b9256d0a9579ae91b31c549b4af6953c"],["/comments/index.html","5c46692063d011ee5333ed8b13b0d022"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","aceb7ffde6c38a3b51f33bd9a71100fc"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","87eaa273807a6b0e01e2db0abba3a35f"],["/movies/index.html","854fbc981fd1fb70cdae83776993baf9"],["/music/index.html","45d691ce852140eddd744b4f9898c387"],["/page/2/index.html","c87f2fa0e3f62c29bb8b3e90a38d9a43"],["/page/3/index.html","4eba65642587a587f89f8c66f5753b2c"],["/page/4/index.html","f428d123c6721ae49d223e20dbf6cddd"],["/page/5/index.html","3ce22bbc0e213f2957ef0a3eafdb39f7"],["/page/6/index.html","929d0c178bcc22d87c4131049e628030"],["/page/7/index.html","107bd62b5c723105f64d6820ec3e58d7"],["/posts/1021360842.html","218ec6fad695a284355e103abc6002a4"],["/posts/1120620192.html","d2361525a8e38b3c786dd6aa080b01f5"],["/posts/1137707673.html","e2a9596f57b0bcdfa49e42e484c24b6a"],["/posts/1141628095.html","4c5d5d34ba67ff8e552f07e4ae19b74a"],["/posts/1168613674.html","7491921b5a2d5e58d8d722dcbca0aab3"],["/posts/1219920510.html","c61eeb844e00b71f6caa230f68ce0924"],["/posts/1222166338.html","5121e72104b50eb5ca45bc1f555d0909"],["/posts/1259097482.html","488da3b5c87b28b356aa789f1d1e824f"],["/posts/1271036369.html","2d67cae8db49e4810b2758816b454281"],["/posts/1312847445.html","d76c2bd95f84f9029649f998b9c8fafd"],["/posts/135355774.html","9d21c763e5197e7ca904d5475eb81c52"],["/posts/1375344716.html","e3c365c451ced9d1200d0456964acd61"],["/posts/1388991698.html","12c495072c65d5ab63ae8963032e186f"],["/posts/1410315814.html","047cd9e918ab05f69b0c0c6818625678"],["/posts/1452790229.html","00657629ff4bbb4e839e925f4edfcf61"],["/posts/1470079884.html","b2060060d531827554934a8a6d1509ae"],["/posts/1470079885.html","3d54782446ad376c02a63778c95fdaed"],["/posts/1470079886.html","248da89fe1793c79e1795cfcfc5dacfb"],["/posts/1470079887.html","1dae9c047efe491d564fbef4a6a4244b"],["/posts/1498536549.html","9d49efe70a51870c9e6ac8248e60a98e"],["/posts/1539568593.html","6385f8bd2051d38ff2eae71d105235ef"],["/posts/1547067935.html","dfdc8c72b8d3f838230bd12a113c31e6"],["/posts/1557866301.html","9d5f43c147aa637b26e9afdfbfb33297"],["/posts/1571776361.html","5649b09aa1b92e7cd12850c43f1cea38"],["/posts/1605124548.html","101d25e74a1b99183f73899b4d6bc004"],["/posts/1633036852.html","dd8679a6dc449c2ac69eab3a21cbd6c6"],["/posts/1667740714.html","6e7daa2c2a3afac80fb620cb028905a3"],["/posts/1674202625.html","f80d186aec26f124e62dd9ff7230acea"],["/posts/1765123828.html","423389254d3c35ea27cfc9947e672be2"],["/posts/1767336200.html","3ddaf72a3c6142a737a9a013cab142ff"],["/posts/1776114197.html","347938fc460ed36998835987226be263"],["/posts/1817748743.html","6d7465aeeaf2a5e3a6d0fd022443b898"],["/posts/1925125395.html","10bf78f230123139b5cdb447de1d694c"],["/posts/1966191251.html","3d2362cee122e28cb0337462ee35d06e"],["/posts/1987617322.html","ab7854c7fcc53662cca15d9f10f189bf"],["/posts/1999788039.html","50441790c6871a5117f172105bd9c48f"],["/posts/2075104059.html","1ffd2743b5e9cb38023e72105bc2053c"],["/posts/2087796737.html","d25ab291107c3b994d022de86028d8d1"],["/posts/2106547339.html","f14ee3a384cbacac28fb00ba0f1e134f"],["/posts/2207806286.html","f3c35d011792dbff85d3cdfb95d2b8b0"],["/posts/2225903441.html","879ab70d99672cffdc938c7277db8dcf"],["/posts/2265610284.html","f14ffcbea5af3ef83154fe45833dac61"],["/posts/2281352001.html","70e21a357498ac27af47d5aff3e446e4"],["/posts/2364755265.html","d1acdb68c4c9052581424ffbe368bcd8"],["/posts/2414116852.html","caf4f72997dfb690f03d78e22423c1d5"],["/posts/2421785022.html","16a039ced43cdb9e210ea7e3e794c4f1"],["/posts/2482902029.html","310c1163b00bb9de8e5cba98a7d62f58"],["/posts/2495386210.html","25758a398c1dd5d1b259a63f37008094"],["/posts/2516528882.html","64dd403dd69910f0bdc0ebf3e52c1ed3"],["/posts/2522177458.html","65a7511193c5288a471a5ca0f7f6e41e"],["/posts/2526659543.html","e9b8d21b4805e01586c0e0df4c226b20"],["/posts/2529807823.html","0d13415ac68b5e42b9e850b69c1b3fdc"],["/posts/2596601004.html","f42a8863b448ff3b793aa901af3d1649"],["/posts/2697614349.html","f8112f2df1c5a8458cec7a6ed6e42f01"],["/posts/2742438348.html","7c86ad44e2d23d662269da75c93e9478"],["/posts/2768249503.html","07119d5da377dc2639e9e8a5d29b29aa"],["/posts/2864584994.html","00e519ee55ccb8d524622ab48a2d3d17"],["/posts/2888309600.html","2931786531bde10d09f6525409a33d32"],["/posts/2891591958.html","f1e93b33f7484e925c7540e9a3d808f1"],["/posts/2909934084.html","ce12c6e75f829b4e2db8003c3928a387"],["/posts/2920256992.html","6f5ea462c813b912c7df03c3b8aebb0f"],["/posts/2959474469.html","49288a5edbef3541ab78f74af9d0ae0d"],["/posts/3005926051.html","8d0a4910d08c3a418b4560311b91bd26"],["/posts/309775400.html","d369eef5756d72bdc8a1df5463118e8c"],["/posts/3156194925.html","282b57ae48e4a06b3003e69629295eee"],["/posts/3169224211.html","cb96698d0e3d4e06d3743df75203a7ca"],["/posts/3213899550.html","1ac999ea6f958f94f65a81c3abe13073"],["/posts/3259212833.html","0af9eb3c78808690b11cd662a16fe87c"],["/posts/3265658309.html","de361f20cbc6e71f80b2d4fb9cedca0b"],["/posts/3266130344.html","15d85dcd366e93a891a62e891b29af8d"],["/posts/3292663995.html","c93611f2c31d4b4973e31a38bce81714"],["/posts/3297135020.html","a2ee3476bcf2cadfb39e168e63e59727"],["/posts/3306641566.html","911ea657bd1ec6e8facdb7123cb096f5"],["/posts/3312011324.html","44a3566c8aa10f386a791f3f3d26741b"],["/posts/336911618.html","c953a31b6220067d0e7e0ca0625edf9e"],["/posts/3402121571.html","99fd0e0942ae50469ba922bfc7b28970"],["/posts/3405577485.html","e5eb64b814869d7016272e714629fdc3"],["/posts/3498516849.html","def400b02e04446a540b0a3ce6a9d84c"],["/posts/350679531.html","d43c58d2292004ee0afc1fb04b08f426"],["/posts/3513711414.html","b904198e3c6e9d7d6e83768750accf2a"],["/posts/3523095624.html","5782380498931a02d8107ea70f8f1ee6"],["/posts/3546711884.html","8ab7d2cc76e0fd1c06256c1b05de2ea9"],["/posts/362397694.html","ceb406748813614f0844eb19132dbfa4"],["/posts/3731385230.html","2c88919f2e8bdc4ef0733879128c8415"],["/posts/3772089482.html","9aea2293e135a0d79c9fe5f9f7888eca"],["/posts/386609427.html","3e4537291c9585e5d6d0820c053d3a1d"],["/posts/4044235327.html","8d8b774ebca79a57d8b07e97f8185960"],["/posts/4115971639.html","36854cbd5241ba9c8b615a426ce05e86"],["/posts/4130790367.html","c0445312fc6e8e2ae7ae6be7499448dc"],["/posts/4131986683.html","c2cf5162dcc8a2d20962b53b0ebbdb01"],["/posts/4177218757.html","b553510bba06398f34a4e35926ad2909"],["/posts/4192183953.html","e26b97259347d5c988188bbf7d6235e7"],["/posts/4223662913.html","3d1c47d9511124da01c309e48e7aee3c"],["/posts/4261103898.html","e15a2ec57614e69eb131c6fac9eaf845"],["/posts/4286605504.html","5666b3f962cabafedec5ff9f04313a15"],["/posts/449089913.html","5a729006f6ed83d6b41ea826682daf14"],["/posts/469711973.html","1f37ed8df5f895f6782ed6b3c61f7ae9"],["/posts/482495853.html","ebfdba7a9bf817ab5bc8ee7022c4ff58"],["/posts/488247922.html","7d90ebe4809367d57eba358800fc76d0"],["/posts/517302816.html","278331d0fd3637acf60ac7969918d9bb"],["/posts/570165348.html","68de7aa60b4b53f077ed278a3f7bacfe"],["/posts/595890772.html","8290391248e5ba66f68f43128b38318f"],["/posts/67485572.html","b424553e2539eba38a1da58ff4ae8576"],["/posts/694347442.html","ae9e0efc6e69941567945bf99f8d3f0e"],["/posts/707384687.html","55b31337e31f074c6d7c492f9941630f"],["/posts/71180092.html","66eea03dda5d75f20ec8daf13d4c64c8"],["/posts/716459272.html","94ab7c1c29bb22c72484dd5c5947035d"],["/posts/765481613.html","73ce7394bfabf9312c63c6518c0f02d3"],["/posts/778231993.html","4043d0bc0693629016d5a3905807c737"],["/posts/795397410.html","f03a7bac0d8a67c11bf0644276df6575"],["/posts/820223701.html","b0d8981dfbda848122f0677c6f79470c"],["/posts/830372185.html","afddd6854f0d1bcef979ba7cfdbc7d1a"],["/posts/88294277.html","61120f75249ec61114d8d2e9257dc0df"],["/posts/939963535.html","24796c97d48372079da611978d587bb2"],["/posts/983786067.html","3aaf73b77872ea4c1ab1dde9fd6e9f06"],["/sw-register.js","cb7616c95d7d318952130aa2b96d29b4"],["/tags/C/index.html","73f79d4bb29228b32aa41afae7bb42bc"],["/tags/C/page/2/index.html","30da5a11e845fc29565f682455540f07"],["/tags/C/page/3/index.html","964e8ee16da7eba395b6906a4b6919b7"],["/tags/C/page/4/index.html","a7d77f26a7ad8308ae1c65fc984bd996"],["/tags/ETL/index.html","25bd6bea872f0721653b6533d944da35"],["/tags/ElasticSearch/index.html","ea3eeb3803842d3396581742a0d618c4"],["/tags/GUI/index.html","72185ee69b725d6a6eabfc8200a64765"],["/tags/HBase/index.html","f0c225e6c6a45f3e04d5abbf9f9a02a9"],["/tags/Hadoop/index.html","6cfac5d558683c393fca2fb85123bceb"],["/tags/Hadoop/page/2/index.html","6f1ef2a4caf1c6ace5a7daa83ecb8462"],["/tags/Java/index.html","34ed2897c532fc5230681a0d41f07d3d"],["/tags/Java后端/index.html","76e25f85c73d145b7a4986ff25dd9da4"],["/tags/Java后端/page/2/index.html","5dc215ca2554772461764d4637233700"],["/tags/Java基础/index.html","f663e27f4f35f036d7d3da54ac377a1c"],["/tags/Java基础/page/2/index.html","22fb194f3922174cda7efbb9a34dc9c3"],["/tags/Kettle/index.html","f5dada6135aef370cdfb2bd0f89a1ae8"],["/tags/Kibana/index.html","4f0d876ca0e8b9cab170d4f2a0038e26"],["/tags/Linux/index.html","1ec33a6b43dee40833e997a3db6148fa"],["/tags/Linux/page/2/index.html","7f457249cf87e03b70692a65b4b8fd65"],["/tags/Linux/page/3/index.html","6054cfcd086ead9aa3f9bba4e31e9481"],["/tags/Mac/index.html","e7b2888b2644c55276c719966dd9f3ed"],["/tags/Mac/page/2/index.html","2689573e2a2ee0f0652e2ffada890b4c"],["/tags/Maven/index.html","b388878e99cdbbd420b8857ee8ba396f"],["/tags/MySQL/index.html","1ba90a2b0f2f4dc31dc58e48b64bb4a7"],["/tags/Python/index.html","4222eeaf1981d57334bfc6ade5a87a9b"],["/tags/Redis/index.html","723bf2b9cdd426d2a2750176bcb06d2d"],["/tags/R语言/index.html","77b7aa06354df7d1490e68efa38d5c76"],["/tags/Spark/index.html","a7b1e34525af7345bc943a421e17a9de"],["/tags/Ubuntu/index.html","249357ee8ab0f42d1bcd95d7db741961"],["/tags/Vue/index.html","07144a04844b4d2b6583a71321fddc41"],["/tags/Windows/index.html","9dc6ddfd7b760118cb4311b42419b31f"],["/tags/ZooKeeper/index.html","1572683fce3e69e5250f88e99531f3c4"],["/tags/bfs/index.html","7d402285c16099941e9fb3a3959eb2d3"],["/tags/dfs/index.html","25669196778b9229caa45e1c4e6f4573"],["/tags/folium/index.html","7c8e40eb09f70fd1634f31aa5cfb173c"],["/tags/git/index.html","542547a146d79bdd065a628b3c9f50bc"],["/tags/iPad找电子书/index.html","cfbb2b7d10f02f9b85dae25f1caa3c12"],["/tags/index.html","c27a6f6ba136b3ef2f99a88b9867a6a3"],["/tags/latex/index.html","969eb66e5118f873d81409ee8104d1e6"],["/tags/中间件/index.html","cb001722353ba8c23c34b52ab424012d"],["/tags/二分查找/index.html","fef385e54bb23a73bcad34d6d8e89cab"],["/tags/优化类/index.html","00129e91b7ec2f551d2fb3b7598bb545"],["/tags/前端/index.html","12b32726811b7e063a53161f62e6c246"],["/tags/前缀和与差分/index.html","839b9b41f55c7b9a702937061d16126e"],["/tags/动态规划/index.html","bfb3b53d9281f49a8a877d5be306e066"],["/tags/动态规划/page/2/index.html","0536f55bfc17708f14536101ef5c4d83"],["/tags/博客搭建/index.html","4111ed1f1ae06518e4ab4b4eac8fb1b0"],["/tags/图论/index.html","99f392bb6eb340e215f4802ed1a8a82b"],["/tags/大数据/index.html","17f04bee13919acb840287c142fa22fc"],["/tags/大数据/page/2/index.html","ff8d7cf376e479dde3c9683a4197c6bf"],["/tags/排序/index.html","c74e21819aa3175ea68b1dd6cbf8044c"],["/tags/操作系统/index.html","3da9fd24b2c842d1960699c46ef297b7"],["/tags/数学建模/index.html","87864150de4286daaa1c2467b32b4df7"],["/tags/数据库/index.html","fbcd0db250dff7b4a0e11f1c3e646655"],["/tags/数据结构和算法/index.html","55d1c11b3cd29b4f6a4add99fec188f8"],["/tags/数据结构和算法/page/2/index.html","742e3200d7da72b43d5738eaf7cd3e2a"],["/tags/数据结构和算法/page/3/index.html","aeb7b9f0b42dd5397a6f9bd5b52da493"],["/tags/数据结构和算法/page/4/index.html","93b67f47644904b83966ef19f9825181"],["/tags/数据结构和算法/page/5/index.html","e005016d3f274a248f47e6035ebe21e0"],["/tags/数组和字符串/index.html","55e8650685568a0b5ff0fe6345d9718d"],["/tags/数论/index.html","b8933d18bcfceba77daecfed57185fd7"],["/tags/枚举类/index.html","0015d502e55e2687f3c1aa7e7622a6b7"],["/tags/栈和队列/index.html","7bc8d3bdf946603046bdff9ad76a026c"],["/tags/树论/index.html","5c78e153205cb14428876e5c88445dc1"],["/tags/测试/index.html","d41cf9ed1821a56c86e8a71b779b3859"],["/tags/环境/index.html","a25d69d19fe65f1b2947940e2fb924bf"],["/tags/环境变量/index.html","06cb0b6613b58b1f21bdf91f583496c9"],["/tags/绘图/index.html","8e5c09d2551cfcabcb7a048c18ca2b0b"],["/tags/编程工具/index.html","276d601fea7729c40dbe99bb27aa4ec3"],["/tags/编程环境/index.html","28829814f6c2e9e417b2fb8a962ba17b"],["/tags/网络编程/index.html","69eeec9f6beeac14c5b0721b6fb3c3dd"],["/tags/英语语法/index.html","1cbeb8007d6df2efc87c963dd6445814"],["/tags/计算机操作系统/index.html","733bd444e8cf3ae270d3f87c46ffee47"],["/tags/论文/index.html","fab37d31a60b2c38e66ec41f3050465f"],["/tags/资源下载/index.html","4e845a573a35c39ba9646277a27dbde9"],["/tags/链表/index.html","f53ce3e1f3284e0169c9dd0d75f9df3d"],["/tags/集合/index.html","41fa8942ba9b26dafbb2e09bf18a4edc"],["/tags/集群/index.html","fdc675800b51e55aba7550f6d0bd12ce"]];
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
