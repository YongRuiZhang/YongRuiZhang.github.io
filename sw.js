/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","dca38b66d325ac5a24dfc5c19dd1028f"],["/about/index.html","9d6c492f346cc58c5ace90822ce51766"],["/archives/2023/01/index.html","2498b4fa79ed2d9a1266190ea6fd392a"],["/archives/2023/02/index.html","1cd87ce51be9db279b5695f777dbe60a"],["/archives/2023/02/page/2/index.html","c6cf78823be45b9dec35a0da639a81cb"],["/archives/2023/03/index.html","2ab5812f1855a9c87b146fe09ad7b74e"],["/archives/2023/05/index.html","ea2810d3a4d25e9d72132cb54c2a5258"],["/archives/2023/06/index.html","c823a110b6dde229661c8d65a2a76970"],["/archives/2023/09/index.html","0f36bb42eba3319141cf170ca44a1843"],["/archives/2023/11/index.html","fda27390f02b43b76a39f4c52f473605"],["/archives/2023/12/index.html","6921504144ba843ed49db7746bfdeab8"],["/archives/2023/index.html","a470f1356d140e42a9dbee5333985e3c"],["/archives/2023/page/2/index.html","a3983f0c1ce3c2a4fcb8f7cc7a39a2ce"],["/archives/2023/page/3/index.html","21dbd57a1cf4cf0b8a612e6ac07af7db"],["/archives/2023/page/4/index.html","90134af19abecc7c90f8caff19841ef4"],["/archives/2024/02/index.html","269aa48ba2ea3d74568646258a8ba7e8"],["/archives/2024/index.html","f3a8a961ebf59a788f0ff6e6d3b22a7b"],["/archives/index.html","04313b0452f4cc1ea38cbc7c2ece1995"],["/archives/page/2/index.html","2d3fe263802f1141dae806c96cd43e58"],["/archives/page/3/index.html","9a10f6b119325d80b2ffba529141b7e5"],["/archives/page/4/index.html","222d14dc057abd64e5131a81558c3889"],["/baidu_verify_codeva-qQP2iZOMLX.html","2fade654ba062f22cafe3876c4e76af1"],["/categories/Java/index.html","87d2065356fc6366bc6350b8d52aac89"],["/categories/Java/后端/index.html","d6e51edf28b5e4af8fed1e7db3e53165"],["/categories/Java/基础/index.html","ef5d760f3a2598ac00469306d350e2ad"],["/categories/Java/基础/集合/index.html","7f758fc528a538eff5d86abf3a8c4e3a"],["/categories/Python/index.html","106f2b22b84f03b60f2e8165a4675528"],["/categories/Python/编程环境/index.html","57d4f0f339b3cefd1367701b9e41d4ca"],["/categories/R语言/index.html","3bd1a19b85a8fb4f4c30795252a15896"],["/categories/R语言/编程环境/index.html","cdb15ba24755de9b913f90452edd8d44"],["/categories/index.html","54ee634b4ba1ed8bad96b7e858c21261"],["/categories/中间件/index.html","122b5b64cf7a7add617398390e0b466d"],["/categories/前端/Vue/index.html","c106d5cc3c090e509ee9cc06de4a219e"],["/categories/前端/index.html","593a049cd2c6caa0ddf70903a5d6ef67"],["/categories/大数据开发/ElasticSearch/index.html","3cb76d3bedf9401226c345bc84508c78"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","76540a841b71add2b9d35f884401f3ac"],["/categories/大数据开发/HBase/index.html","e5ceccfa3ef26bc8348e57e33a4d8f89"],["/categories/大数据开发/HBase/学习笔记/index.html","e843d0d95d8f6ce9a60e20e3dda8c841"],["/categories/大数据开发/HBase/环境搭建/index.html","88212880f45d7861d1243ce9cdc24ddd"],["/categories/大数据开发/Hadoop/index.html","173f70487b1b01c86bfc1c031a4624a7"],["/categories/大数据开发/Hadoop/技术/index.html","d2ddb7c8e4b6e66759e57fc381a16701"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1869725c9ee8149cab88c7b935ade58e"],["/categories/大数据开发/Redis/index.html","d125265938f6725434f592024bbc7f5b"],["/categories/大数据开发/Redis/技术/index.html","09fb52e47b6ebef10833b7c774d2f1b8"],["/categories/大数据开发/Redis/环境搭建/index.html","5eeac6927d622072cb437b14e5e6e5e9"],["/categories/大数据开发/Spark/index.html","1f337b3fea5c10afa829d4d184feb93f"],["/categories/大数据开发/Spark/环境搭建/index.html","4126311a4d5738ab58d4dc263878d9d9"],["/categories/大数据开发/Zookeeper/index.html","816e0d422c3194b8fb739cb159fd135a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3c9d90fb8f06737ccefb422eaef62d71"],["/categories/大数据开发/index.html","15dd48e2682b708a8e30caec894f2f31"],["/categories/学校课程/index.html","513f518319675a0370fe6e122f4a5372"],["/categories/学校课程/计算机操作系统/index.html","e4bf6e9ab62636f60cc2160f4732c841"],["/categories/操作系统/Linux/index.html","d92aa1df71eac4d2e5cc0285cdccbacb"],["/categories/操作系统/Mac/index.html","b668e3e9823d746cc4d2875705353d0c"],["/categories/操作系统/Windows/index.html","02ae981b9e1fd670a56f97c26d5b358c"],["/categories/操作系统/index.html","82edef4448815e7b5ce22f51e5f396c3"],["/categories/数学建模/index.html","d3341b42518aa810a3ed48e48f0504fc"],["/categories/数学建模/latex/index.html","a6e084a98fcce492840db78db15e17ea"],["/categories/数学建模/优化类/index.html","4e0ec40391ed81bcd1c449d5e2dce39e"],["/categories/数学建模/优化类/现代优化算法/index.html","b3f847141bfb903a9947ed869b746e9f"],["/categories/数学建模/优化类/规划类/index.html","3e74161a6787071b29f02fd2f33f52aa"],["/categories/数学建模/绘图/index.html","65d76f197c2b094f47190a02e65f2f50"],["/categories/数据库/MySQL/index.html","be8d3fcf147ea64849af1b63a3b8849e"],["/categories/数据库/index.html","aa714d23b774db64e4ae411e41dc6dce"],["/categories/数据结构和算法/index.html","d477a403ca7a9173abb49b5d25f55ac0"],["/categories/数据结构和算法/page/2/index.html","0713ba4720dcf48ca0e8ce73e5f40a85"],["/categories/数据结构和算法/基本原理/bfs/index.html","11c126d56326a148c26318f5fa323f71"],["/categories/数据结构和算法/基本原理/dfs/index.html","ebde711d86c5529f794e9adcef695de1"],["/categories/数据结构和算法/基本原理/index.html","1a068a84e4bcb3a58b0e07491f84b4a6"],["/categories/数据结构和算法/基本原理/动态规划/index.html","da51dffa1897296fd3fcd24099189625"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d5604a7d767f430489bdc7d3e96eacb2"],["/categories/数据结构和算法/基本原理/图论/index.html","3f4505381274fbf6c96d5127169fb505"],["/categories/数据结构和算法/基本原理/字符串/index.html","6d825e4e1db1ee88048e137ad5ba228e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","865148fb00a72e16a049bd5283f5fe91"],["/categories/数据结构和算法/基本原理/数论/index.html","8ecc3718c959d2d9c46269c0e2cea49e"],["/categories/数据结构和算法/基本原理/树论/index.html","335070b2933e83da5673b6293256d836"],["/categories/数据结构和算法/基本原理/链表/index.html","559f7e9a991265894b4052a0c929d523"],["/categories/数据结构和算法/算法题/index.html","0fd39a03e594b0dbd326f869b0821a71"],["/categories/数据结构和算法/算法题/二分查找/index.html","f14ce69c87c66c8a2e616eecf89781a5"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ec0667aa8b33590fc439d374e4d10492"],["/categories/数据结构和算法/算法题/动态规划/index.html","1e873cd1a5dc7959c65a09256b6e76d7"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","32fb8b8d772ff6de1cc13e3bf05e66ce"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","97b4652c48e5fc8de9362db49667403b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","19ee92ec3f0561c2fa428fcb36d90bb5"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","65e1fa9d5611b656c0d34477e90e91d2"],["/categories/数据结构和算法/算法题/数论/index.html","909c776be89545ad71f1e4bf2fb79184"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8205a6bd78f07ee05de6244aea941adc"],["/categories/数据结构和算法/算法题/树论/index.html","d369641694dac7ea54972d1f5e9b11e7"],["/categories/杂七杂八/index.html","1f8128a9c7bcf7715e1a4f7b78306706"],["/categories/杂七杂八/博客搭建/index.html","9d0fdf2f01eff97fb620f4858939f9fe"],["/categories/编程工具下载/index.html","da79a8bbb0de58cd16cc7af05237b672"],["/categories/编程环境/index.html","7d4870baac1954bd6cfa73b4712dda46"],["/categories/编程环境/大数据/index.html","45d083af75d7e73eced984862060a998"],["/categories/英语学习/index.html","0ae4b6e0377a4577ad2627ed0edf40df"],["/categories/英语学习/英语语法/index.html","837618bbe1e31e5a4e85165a6fb928f2"],["/comments/index.html","1ad1882b79d05499577d8ec9be8a2335"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","5cb18752799844acffc00859656cdb49"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","f62bbb0e93801b31a82717ddf340b001"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","843bf4b158f3558de63d8a0221cc8185"],["/movies/index.html","00e0705a4f9c1e9596689a7efa0f0ae2"],["/music/index.html","054ab3abafea0a268322995e64a347d9"],["/page/2/index.html","0e4e1c0c7187b4c5b897ad78214d78b6"],["/page/3/index.html","d8a0900cad3bcb0b98b87f32e307f4b1"],["/page/4/index.html","ff1c593571f974f0245148c09284c379"],["/page/5/index.html","f060a9ce37ea9495a2b79380103af044"],["/page/6/index.html","8fd249334419ba2d94bd2d31f276bcc0"],["/posts/1021360842.html","68225c2bf3a76f4c7fd2c30e38328a70"],["/posts/1120620192.html","3e3ff60020f63fbb7d825618ae1134e3"],["/posts/1141628095.html","6a4eef782f50f8856c5b13341180d867"],["/posts/1168613674.html","db9fd14738f592b9c3467af3899d9fa9"],["/posts/1219920510.html","937dd05e58256d10486d25c977495d1c"],["/posts/1222166338.html","2da244542c47961a67d6fb2b8fa4c85e"],["/posts/1259097482.html","6c0a59747b88ff7f804ab2b69bc7819a"],["/posts/1271036369.html","57653d43f8e8e71f738aa02fcd93d185"],["/posts/1312847445.html","1fe3e99f25b54991f5260c11bc4aa400"],["/posts/135355774.html","f3723a6531d68ac03e1902863abd4ec3"],["/posts/1375344716.html","233141979900f2a71ee2554f9712e62e"],["/posts/1388991698.html","187fbce7340b6f45658dcf73c11df76c"],["/posts/1410315814.html","91f8ea303b63077de1301d65cc91dc22"],["/posts/1452790229.html","676094657f313719d7f257cdf317f821"],["/posts/1470079884.html","d5de65f736f13eb88a160924204cb397"],["/posts/1470079885.html","5be0d0f8e2d6084f563ae22fc3c4553e"],["/posts/1470079886.html","d83893930ad4eb9cf6c6265d82d94054"],["/posts/1470079887.html","a7275bb9761f05062c393e1b6181e098"],["/posts/1498536549.html","77d9eb9ceba0c527ff62da2553892e12"],["/posts/1539568593.html","894852de1480a8f94bf29590dde5290b"],["/posts/1547067935.html","97ad94b46193e42993f17b607130bcdd"],["/posts/1557866301.html","d58bba2e75886391db69891357fd309b"],["/posts/1571776361.html","5d94719f36802db3363c172ec875b0e5"],["/posts/1605124548.html","cba5711ce5b513abb45325268833a6f3"],["/posts/1633036852.html","86e6566d0546036854410238de02956b"],["/posts/1674202625.html","00b3c118dc72c06d1a55d0c071568e0a"],["/posts/1765123828.html","b183a8b55781b227958e1c3d651d1aff"],["/posts/1767336200.html","fb2b7e7228a5e0e5a3312b89056d3bb0"],["/posts/1776114197.html","c0a0079dc9d7a2e1126dbc237f30fd1c"],["/posts/1817748743.html","0985f879932bd7a2788a16136f25fef0"],["/posts/1925125395.html","9690b94a276142aea49ca377d62e0e85"],["/posts/1966191251.html","6d50cd7eac65470cabb97d49daf032eb"],["/posts/1987617322.html","74446769bf0daa57d8be79d3319ceda9"],["/posts/1999788039.html","f9320df299d29f48d1b665283c1c30c6"],["/posts/2075104059.html","7161c76424075fb0a53079f9db44c4be"],["/posts/2087796737.html","cb80d9179d0ea78c5372b54ca724b316"],["/posts/2106547339.html","dc8933981e62a6a0d4a39953d4aedf32"],["/posts/2207806286.html","57534b9d722765d0133ebcfa599b81ea"],["/posts/2225903441.html","da989d4d4c37d162a46862d9af125579"],["/posts/2265610284.html","79f923d0684b8a0d5ab86a57bc78e3af"],["/posts/2281352001.html","a9bbeadb3fb3c75b3001756637cca7cb"],["/posts/2364755265.html","d18e915e8dc0609538cc7df4849aea2e"],["/posts/2414116852.html","f24d6757aca5163d8f4c6ebdc8e9f829"],["/posts/2421785022.html","a4356a39b7a54d7ad78dd9bd96ad1e89"],["/posts/2482902029.html","aca7f375aab14742c8f1badf7f283ade"],["/posts/2495386210.html","060483cd6bcf0f7e0264d31e01486b62"],["/posts/2516528882.html","e9a90d5e75792398e3de52fcfc7b72e1"],["/posts/2526659543.html","851a24cb479c5863078f14dca04469f6"],["/posts/2529807823.html","ae37fa8e629a20051fa08d390a7fe346"],["/posts/2596601004.html","91a53bd5adce2f027b56ecf3aad94387"],["/posts/2697614349.html","f2dd3fb2a756be8d6a6a1bb4a5494403"],["/posts/2742438348.html","648d41f61364f95a20603f099494c8a3"],["/posts/2768249503.html","18ac3aa440c61e15073178298781cd7a"],["/posts/2864584994.html","f1040bb9472c030002548527549cfa6f"],["/posts/2888309600.html","60711b1fda9045dcefbdd4618d493bf3"],["/posts/2891591958.html","6cc64b0f4d6c803cfb816f257ddff7e2"],["/posts/2909934084.html","3c5ce4fdc3d243832f90f77313d38b88"],["/posts/2920256992.html","af952808e2bf46868e673a1f3c841d36"],["/posts/2959474469.html","0f53bb9b76b06b730f4b9eb83ea36417"],["/posts/3005926051.html","b1c53f2e992dfae0b9e72fb1d6cabc97"],["/posts/309775400.html","0311912a9c1b9a2b3094b4b0f3f4c819"],["/posts/3156194925.html","1714c383b755b784151700b0cc4071cb"],["/posts/3169224211.html","bb62eaeaf30e9bc95d38b6efc511ea74"],["/posts/3213899550.html","2dbf3377c993e7c7cb035d8bc4cd9f7c"],["/posts/3259212833.html","35a4292841ed49f9bdbda05c7a93f258"],["/posts/3266130344.html","bf553dca5c746cbf4dc8722157266089"],["/posts/3292663995.html","556be129225eaafeac11a8d48e522140"],["/posts/3297135020.html","c99b1a21f2ff745e7f07fa90a45c05bf"],["/posts/3306641566.html","9c5289deb412286bd176e63fa7cf687b"],["/posts/3312011324.html","9b82046dc813523c7b0ac70a6e429622"],["/posts/336911618.html","ae1f6a111952919fab3e47bca80ebccb"],["/posts/3402121571.html","9c79f057dfb8fe4f386dfc9ec7fcb6b0"],["/posts/3405577485.html","a79841c3450af1fd3d6f6e1fdf4ab579"],["/posts/3498516849.html","c36c52339697f82204b0de0ca0eff108"],["/posts/3513711414.html","54d3f6e5fc6d464fe26a23299bd5d7a4"],["/posts/3523095624.html","3809476e113a8bb9eb87af6083e9ec3d"],["/posts/3546711884.html","34f79f072cbdaebbfb179a3826c0f380"],["/posts/3731385230.html","1585c8f6ed9602edbadc2b330014fcf3"],["/posts/3772089482.html","4ec8a9770d999e228c97ae2ac7b3ca06"],["/posts/386609427.html","f6328a8b540c02efa02b5a9fb7a3bc9e"],["/posts/4044235327.html","5acdfc3150ea1d76b9d4b006f195290f"],["/posts/4115971639.html","ac4e30ed39d82f172bb42a594243fa2c"],["/posts/4130790367.html","eddfde7acb447b186a15e81672482f42"],["/posts/4131986683.html","e5558d3c2976fd891e85f11159348dcc"],["/posts/4177218757.html","3bfc109171596a2d7a26b450c893381e"],["/posts/4192183953.html","9b9c881af60692523dd9aad87f593d29"],["/posts/4261103898.html","8736c2426911362036aa848b73872036"],["/posts/469711973.html","83c24280275b2860830b5cc1fcf47bf3"],["/posts/482495853.html","eff2a5f6100687d00bcb972080525d70"],["/posts/488247922.html","16441f2c7866683f5a168d9946cc9c90"],["/posts/517302816.html","a197ca9cfa507b71c8986a41b2974dcf"],["/posts/570165348.html","687676760571f653f6ff2be02ec7d01b"],["/posts/595890772.html","9e534f1803b60b1583ea54cde030161c"],["/posts/67485572.html","478699dbf75ca31aff5a644a637f315c"],["/posts/694347442.html","1096b038176fc2479a267baaab5b17db"],["/posts/707384687.html","ce1c4938cb18b119921242f474b1b4a5"],["/posts/71180092.html","b09e5b7d2f8aa27bfb6540f73bd2f5b7"],["/posts/716459272.html","08a11bb5cc8646f4cb3a25aea7c66cd1"],["/posts/765481613.html","1c8df5153fc62e8abe448f4879cd373b"],["/posts/778231993.html","571e60520626a70388ab54e1959a209b"],["/posts/795397410.html","7c146aad8cb9f1f634063248d4155caf"],["/posts/820223701.html","41066e977fbb845d083d450d3951b5fd"],["/posts/830372185.html","5a25d9ea6d0b1a46b5802deef904a257"],["/posts/88294277.html","a9b402ecbf2ab8063941f2b0f9a3bb17"],["/posts/939963535.html","72e2fa4ce4914558751ca186891ed1d9"],["/posts/983786067.html","34769d62251599bdcc90bd25d27cfd36"],["/sw-register.js","a2f346f1ca8bb9287eeee8f8765b1270"],["/tags/C/index.html","e5414d409b2a087256a22fd7668dd206"],["/tags/C/page/2/index.html","ed9385dd1a11877755f1cef445117ad6"],["/tags/C/page/3/index.html","f4cb3c73f751326524ec287426fa890b"],["/tags/C/page/4/index.html","4bce7edd9c2dfd9bbdc7145696e23c6f"],["/tags/ETL/index.html","56db561fb57aca03044adc7430e01609"],["/tags/ElasticSearch/index.html","b4fb9a359840c3bb307703d18537e4de"],["/tags/GUI/index.html","2e39f504e9da16b11aac8bd44def60c5"],["/tags/HBase/index.html","feebb02ab7de47ec3797ed57cec336f7"],["/tags/Hadoop/index.html","bcc7cdf2a22f270be96d9b019411d940"],["/tags/Hadoop/page/2/index.html","d09d0212b85a3e104693c4ed6cf575b3"],["/tags/Java/index.html","1b2b024d7855f34cf20c2ca26ab8e58e"],["/tags/Java后端/index.html","0777adc1474a1abbf6a2fb466b663874"],["/tags/Java后端/page/2/index.html","de99f41fb72874abeb81283b4a4d880a"],["/tags/Java基础/index.html","50565947b79c1f11979f69f919769b5d"],["/tags/Java基础/page/2/index.html","71188000081d777dccdc4e796f7279a5"],["/tags/Kettle/index.html","c40927376932b72c3a6dc30a69a81067"],["/tags/Kibana/index.html","502d67d4cd7cf099d996fdbe0bf60ade"],["/tags/Linux/index.html","e068366064a9da4305b6749fb30226ca"],["/tags/Linux/page/2/index.html","7d7fec091d484ea338d0ef0f806e8a4b"],["/tags/Linux/page/3/index.html","7d4402f9dce8fa238a5c6d114ceea885"],["/tags/Mac/index.html","db6d3721362c5a553b3aaac7b47b1bf8"],["/tags/Mac/page/2/index.html","d0b2904b0180bc26d465aa47dd5fb558"],["/tags/Maven/index.html","8c099bafbc450e499506c92aa7b951c2"],["/tags/MySQL/index.html","0609abd3f947bfe7637a0023e6dd8bdc"],["/tags/Python/index.html","b315b1964017e8aa790de8899f563d19"],["/tags/Redis/index.html","b49b8366663e8b45fd632c24e2a53bf5"],["/tags/R语言/index.html","3de76a3b6b7d4caf70232d9e88c832ac"],["/tags/Spark/index.html","db8b9a8bb94d689e4dee9ae4e3b4cc06"],["/tags/Ubuntu/index.html","59b1a4654fd8e4ecedee49ff1d676992"],["/tags/Vue/index.html","bda0a3f8da1a4d43b58144ece61f9cae"],["/tags/Windows/index.html","8a3be1ed8ce87a9aa81efabc947fb819"],["/tags/ZooKeeper/index.html","bfc8c4a22522f18e4575650192a8655a"],["/tags/bfs/index.html","69c703d84deb1bb50cb863f22f21a073"],["/tags/dfs/index.html","348ad803f89007f934b1dd79ecad2d64"],["/tags/folium/index.html","5671bd0ee0d88716b109672ad8203746"],["/tags/git/index.html","3ae0b731328980c447d677e3fcf9c3e5"],["/tags/index.html","ec1df977bf8db0c2ad900a0050fa4612"],["/tags/latex/index.html","5cb0039aa90212883fcfd843fca6beb0"],["/tags/中间件/index.html","b7be1b93f4052cd972bf7c23a4ac6cad"],["/tags/二分查找/index.html","8428fb6e6b1ccc8b922094836f21046b"],["/tags/优化类/index.html","d97aa23b9f353738f1888d2b2c87e042"],["/tags/前端/index.html","7266a320b6ddfc2ac360e32129bb5663"],["/tags/前缀和与差分/index.html","7ddfbe32eae4c452bd82359e1b8dbccd"],["/tags/动态规划/index.html","a7d5df63b1c28d345b083dd99da5dbe5"],["/tags/动态规划/page/2/index.html","793dffbd6af5592839a22432b09e4e91"],["/tags/博客搭建/index.html","002a7a7582bb178172047a656e16eb72"],["/tags/图论/index.html","f99982569e9914acf699875c4e19f3f3"],["/tags/大数据/index.html","80f0f6760c67d79575f77cd66ddaa1b7"],["/tags/大数据/page/2/index.html","1ca8072c501a403d7ff7fcdaf5cc5eed"],["/tags/操作系统/index.html","43e450b859ad403387536bc9bf86cf5c"],["/tags/数学建模/index.html","802a5b38b4edd53227248359ecd4eb65"],["/tags/数据库/index.html","7656992153f97569fe91ff24d9d296fb"],["/tags/数据结构和算法/index.html","0f3b309295e8b4578c3e349093ac9b96"],["/tags/数据结构和算法/page/2/index.html","8a177d372f38763e7018c1cf96e50c4d"],["/tags/数据结构和算法/page/3/index.html","e730d52322e8c4e5bce76bc4d1d72b03"],["/tags/数据结构和算法/page/4/index.html","30388227b0f4cc1ac31dff2c278f7840"],["/tags/数组和字符串/index.html","17233cf57c76bf508500643c94c1a9f0"],["/tags/数论/index.html","38cab92bb323c00259e348dfda936c3e"],["/tags/枚举类/index.html","40d4741122b7fb6cb05b144149d6d8b9"],["/tags/栈和队列/index.html","b070a380887ba0df8b0946a9f77fbc87"],["/tags/树论/index.html","91ab202e5c574ca3045c255e1b3587fe"],["/tags/测试/index.html","2b4e77adcd7b4a467e68079c7f6274fd"],["/tags/环境/index.html","dc1f40b8655d7b14ff67a9d4a89b52d1"],["/tags/环境变量/index.html","3aa59fda05b60865f5de2aa660ba5fea"],["/tags/绘图/index.html","c8e85060913471bc1174e8066347f9e5"],["/tags/编程工具/index.html","7de18b5ac615b24b049042616686dd7c"],["/tags/编程环境/index.html","736e49a2c2bdb881b19c3d0267ec1513"],["/tags/网络编程/index.html","40f470bd488294e9fc9b370acd2eec8e"],["/tags/英语语法/index.html","a46e289106a9e5039dd9082c9d23cd84"],["/tags/计算机操作系统/index.html","2de14be8b10dd408692caa036d10c2fc"],["/tags/论文/index.html","5640854b63add510cc9c182534b1159e"],["/tags/资源下载/index.html","65df7bd33e5d58d0d90e4fffdac7ea82"],["/tags/链表/index.html","4bcdfb01df7099638c963c343b74fd1c"],["/tags/集合/index.html","629409228be68e81d41ae96feda42408"],["/tags/集群/index.html","8b8fe5140c830bfa4ed8f9d25c31ff34"]];
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
