/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d71c734821e392751dc41e9eea83c427"],["/about/index.html","4d0961f78e23954f014146087058a978"],["/archives/2023/01/index.html","ed1833fc280a51900de9d0cb44b3416a"],["/archives/2023/02/index.html","c8d30b95e7103987326d9cf4c056991b"],["/archives/2023/02/page/2/index.html","819a99a5e1d0b95c7e0cf5d431dd3c63"],["/archives/2023/03/index.html","629709f9968e7271bd21eddf4cdf96aa"],["/archives/2023/05/index.html","7a9b3d89b68801c47cae36c74da381f0"],["/archives/2023/06/index.html","ac254916bac2832fcc1898af373d5044"],["/archives/2023/09/index.html","a07c512001657489755a1e82e629b69b"],["/archives/2023/11/index.html","3eb67018a1a1848ac1e9095dff0c356a"],["/archives/2023/12/index.html","046760c5864f3075bc9890ad91101817"],["/archives/2023/index.html","0076948677e4ed412f696e3a39dc112a"],["/archives/2023/page/2/index.html","138b9541a2991d9b7f052eee0aa7c148"],["/archives/2023/page/3/index.html","95256015e4549f67140c0360df0552bc"],["/archives/2023/page/4/index.html","3d0b3b47e188c6ec0ceb7d537f626849"],["/archives/2024/02/index.html","d683ea6dc6960f370b095c141e1d3c27"],["/archives/2024/index.html","e73137d0ea7cdb1031801b0540adafbe"],["/archives/index.html","b5aa801c337ce6c82a933f4ddad41b56"],["/archives/page/2/index.html","0b0e0c908208a91547b2f602c8b5b0e3"],["/archives/page/3/index.html","0422ae0c2906cf3e876adbe18d879c37"],["/archives/page/4/index.html","b12cc677215de7435b868faef85dd331"],["/baidu_verify_codeva-qQP2iZOMLX.html","13a0fd5e45146e1bfdf5d5083ccb5299"],["/categories/Java/index.html","4b40474def266cd5a5d84125ad33e8e3"],["/categories/Java/后端/index.html","48d3faaea0c95f6cf77caa6cc1c256a3"],["/categories/Java/基础/index.html","48812b118b644b91c9a150e3be8d8d4d"],["/categories/Java/基础/集合/index.html","4bdfc027ab3d39aa620e28d07ccf6487"],["/categories/Python/index.html","8839915700eb276af2b530f57c28494c"],["/categories/Python/编程环境/index.html","cecedbae7f5a1fd1b85b0797295b892b"],["/categories/R语言/index.html","dfdbc20e7b782abfdfec7a00892532a9"],["/categories/R语言/编程环境/index.html","52262591dbab9da24b73046454344da1"],["/categories/index.html","cbafdbeb8e37a6b9cbe1d745bd228196"],["/categories/中间件/index.html","7ed2448096f0d1091f2da6a5c12d9c9f"],["/categories/前端/Vue/index.html","0dd2fffa6ad123bf379a30ee47fe5f06"],["/categories/前端/index.html","16b8b7aed1dddf31a68908e4e5459abf"],["/categories/大数据开发/ElasticSearch/index.html","a1026872f4dbf1acbb11caa3db55ceb7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","930f1d7c8200cd5c46c1beb7bd1e5c9d"],["/categories/大数据开发/HBase/index.html","2ade3c9896bb0704ed8c918ee3a28899"],["/categories/大数据开发/HBase/学习笔记/index.html","c61c3d54e82dde1fc728e14e94fa524f"],["/categories/大数据开发/HBase/环境搭建/index.html","bcd538f4b41cd14476d24afef0b5f2c6"],["/categories/大数据开发/Hadoop/index.html","eb5a98030dad462385d1ed8951be5c49"],["/categories/大数据开发/Hadoop/技术/index.html","5c6969e8e69350e1a0f74a4761d80aca"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3bf24cc6bc411011a9b25739d3a03058"],["/categories/大数据开发/Redis/index.html","8f26f1c103ece87a216ef8693850148b"],["/categories/大数据开发/Redis/技术/index.html","293997a9fd04e7eb67ab349219fcb6a1"],["/categories/大数据开发/Redis/环境搭建/index.html","dffdb3029dc998a81f8ac599cdf2a1d0"],["/categories/大数据开发/Spark/index.html","6ad2499a65315f44c94826df6f5c677a"],["/categories/大数据开发/Spark/环境搭建/index.html","c754fa5e02f20686dda683dc10064d1c"],["/categories/大数据开发/Zookeeper/index.html","402f7b59ac0aa472a4ba79dabd0a9068"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","111db66a924ae63131eee54697bebb1a"],["/categories/大数据开发/index.html","882de9026f0f1d86901ee4f8b4448a9d"],["/categories/学校课程/index.html","ed8a837a02b6e9f6884538c5baeb7a18"],["/categories/学校课程/计算机操作系统/index.html","0cd6fc1e81bdcf87e60aabd244c31ba5"],["/categories/操作系统/Linux/index.html","06a169a4b9fde7913811d2ee28ff7a27"],["/categories/操作系统/Mac/index.html","230f520795591fc557191bd3393d064c"],["/categories/操作系统/Windows/index.html","e6ad9f8a414192fe22d85e880a07b9f8"],["/categories/操作系统/index.html","2641c867f5788f09e043b8205a28dd0b"],["/categories/数学建模/index.html","497f4e8057bdf9aff5d8e99f268386b2"],["/categories/数学建模/latex/index.html","b06cf6343512758bc89978cb9cee873c"],["/categories/数学建模/优化类/index.html","c61e05d08ac9b5f3b8bff5c527fb132e"],["/categories/数学建模/优化类/现代优化算法/index.html","4b6091b8190218894964f775b4cdc854"],["/categories/数学建模/优化类/规划类/index.html","b624930094150c375773d8d07e264c05"],["/categories/数学建模/绘图/index.html","2f11922a9d5828aba5721b27568e3954"],["/categories/数据库/MySQL/index.html","44aee5356f185322757e7ebf0905d4b0"],["/categories/数据库/index.html","37861aead52c2952a5c5d273757c4299"],["/categories/数据结构和算法/index.html","7be70c8dcdf1c505ef941808778e0f54"],["/categories/数据结构和算法/page/2/index.html","1de4a1aabdff2cf0efc90a78db7f2463"],["/categories/数据结构和算法/基本原理/bfs/index.html","701b56d4b579e299dc2cee5943a9a818"],["/categories/数据结构和算法/基本原理/dfs/index.html","ccf1576c8f185aa1ee2d16b973209a01"],["/categories/数据结构和算法/基本原理/index.html","abebff3dec387c8b814648e7e73f5770"],["/categories/数据结构和算法/基本原理/动态规划/index.html","61df6a33c5901b426f736ead5857b47e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4ddda91beb65a7bb50426493dee70f4e"],["/categories/数据结构和算法/基本原理/图论/index.html","18891acd06583aa5619fc657ff8f06bb"],["/categories/数据结构和算法/基本原理/字符串/index.html","afcc44c33bf356a14c96b35731259e05"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","7df99f47478f97365758228103bda950"],["/categories/数据结构和算法/基本原理/数论/index.html","71efb9c4731834ac72de18dc1769bebf"],["/categories/数据结构和算法/基本原理/树论/index.html","b8dc84d96814cfcea456d4dc1f235681"],["/categories/数据结构和算法/基本原理/链表/index.html","100826024c4eaa75c71dffce15a92ae4"],["/categories/数据结构和算法/算法题/index.html","f0f099bc71e2ef7a81f7ca40b655d955"],["/categories/数据结构和算法/算法题/二分查找/index.html","269139bf5199d4e7512f58651e6f07f2"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","80f41f30fc577badeec9ab95bcbd7ecf"],["/categories/数据结构和算法/算法题/动态规划/index.html","9ecb735b042f8926ef23c0273a69d218"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","6883850a6f5aaa2371e4c83e2f2d26dd"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f23484bf19486d0c840e7e1bbc9fcef1"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","67a4ef8130df352282036cc5a96933ff"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b49f8afd175d2d96fdca357783f8c296"],["/categories/数据结构和算法/算法题/数论/index.html","b940fde4285df6c204a0f96e7dfb99dd"],["/categories/数据结构和算法/算法题/栈和队列/index.html","204a495278a60c390b97b7be789c9216"],["/categories/数据结构和算法/算法题/树论/index.html","288a06383efa84709b9c22b7b853dbd8"],["/categories/杂七杂八/index.html","103ba421ab968fba766ad9c6089a691e"],["/categories/杂七杂八/博客搭建/index.html","5dee1967843170ef457a67c57b6ff564"],["/categories/编程工具下载/index.html","dbcea03602ba4cf08e800181d5795d5b"],["/categories/编程环境/index.html","0f7ebfec327cf8e1c5870a98325ff61f"],["/categories/编程环境/大数据/index.html","51b9e8ce463766f98ac56630ec663252"],["/categories/英语学习/index.html","afed96f6c939b9eeba12e38d282099b9"],["/categories/英语学习/英语语法/index.html","07b9688dc9e1bbdbdcd915ed01bbf686"],["/comments/index.html","01415d11ff671d6a39a21de8e75243dd"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","c96337d1b6ead12cd775d65d1feddeaa"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","206d2718f63f58805de64f813003b828"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","13c9735ae2dc9719a4f2392e0cc686c9"],["/movies/index.html","83eeae806750ca2dcddbe65fe0ae33ee"],["/music/index.html","c2d26fac0da4ea300e8a6c3ed2ad50de"],["/page/2/index.html","ae8a132004977ed8772b574f7683a281"],["/page/3/index.html","68c4e4b19211885994580e042a43f95c"],["/page/4/index.html","bdd74065c3f381d01f80aa5ea009ca94"],["/page/5/index.html","49705454c5c10232ba823235dac0bd7c"],["/page/6/index.html","7f4329a80a6fbbfdf5a4aea90bed0c8b"],["/posts/1021360842.html","82542f13b90e596351191c6000b1cb95"],["/posts/1120620192.html","fdaa00e63b452eb9989d369bc7a89e71"],["/posts/1141628095.html","8cb15530029596d96fa25e4ddd125834"],["/posts/1168613674.html","e85e2d381170050487956b8b7bb0a29a"],["/posts/1219920510.html","99a43d6996f860aa041f0aa7a2e102c7"],["/posts/1222166338.html","8008618ae10c48f21af1382074625205"],["/posts/1259097482.html","a763dc6e437c5d0e6e9f48eaff53cb3d"],["/posts/1271036369.html","008ceed30d86553e487299d2fe28729f"],["/posts/1312847445.html","c70ce64a3b14d7dfaad875397a62cbc7"],["/posts/135355774.html","86c1d0c7e1a204792ebf26b36e55ec5c"],["/posts/1375344716.html","de23285d6d3a73c5166e2698585100d6"],["/posts/1388991698.html","7aed96f95ab11290fa53f64e95c40451"],["/posts/1410315814.html","c6df727a4832c556a7351cd169289ff6"],["/posts/1452790229.html","41f2cf3b8d298be517c5b127fe560537"],["/posts/1470079884.html","c6f2b85045c78fa4649b24bb2f00143e"],["/posts/1470079885.html","52b853ddee9315b41b5ff953206ef7ba"],["/posts/1470079886.html","4106f57d4a958056cc5f1187f00b08bf"],["/posts/1470079887.html","ff8fa30b6c94fdac522d844a34a43871"],["/posts/1498536549.html","e4d45c09a24582ae3b1c4844f45be9fe"],["/posts/1539568593.html","adfd25301b189e3f44690f0552ea88e1"],["/posts/1547067935.html","08d531f661de8f27d5e6ec0decccda60"],["/posts/1557866301.html","3288094798c4dce826146c56d4847134"],["/posts/1571776361.html","fe1166c0c2559d0948d774f4bfd38c4e"],["/posts/1605124548.html","ce259c11d305ebc607f124d8951ffcee"],["/posts/1633036852.html","d30427b9b3fbca6bcc8daaab6c0e6104"],["/posts/1674202625.html","850cbe3a1d591ab74d3670d71b8546b2"],["/posts/1765123828.html","ca4c7ff577b182436f139bf42bc03868"],["/posts/1767336200.html","4b6e17fcc20577d0208526837fbc6cfe"],["/posts/1776114197.html","328c353521c55f0e7cbd3dd187602ff5"],["/posts/1817748743.html","bcca49d8ba6930fc975162a299990be9"],["/posts/1925125395.html","6a4a9aa3e9ff21a2e7b0c5b2980e8e5d"],["/posts/1966191251.html","3cea20468432dd77a2357e568680243b"],["/posts/1987617322.html","22df1a917fcba7591a0d4ad442c58ea2"],["/posts/1999788039.html","2d1a213a6a7afcc05eb82ec7a8ec46de"],["/posts/2075104059.html","6c3f7754989e99c1797d67bd8c5ac07f"],["/posts/2087796737.html","24e2a53ada8ac95dd2306e57c6fc980c"],["/posts/2106547339.html","23a62a39668b0bd32d324641729c7932"],["/posts/2207806286.html","3c8ac9bc6558e41d1852e6939e0c1dd4"],["/posts/2225903441.html","74380c28fac6ca0ec28bb1d25cf48839"],["/posts/2265610284.html","6f842bac6e715a98680ead0b54f0210e"],["/posts/2281352001.html","2fce6934f0ebabe32fb2f11d1482261a"],["/posts/2364755265.html","26f8b035fae9beeaadaef7a8e79c454a"],["/posts/2414116852.html","0c4b6ab708443ccd2b8d12353a82169c"],["/posts/2421785022.html","9986d989b18131ad33ad6f85d65d35f9"],["/posts/2482902029.html","617c3046b52d29eee2088cea9b98dff8"],["/posts/2495386210.html","095c877f9eaab680940f79d95bfd223f"],["/posts/2516528882.html","25d23d463aa7be012c4a15c5c5c03bff"],["/posts/2526659543.html","c656b85a4617627215503bcbf774da59"],["/posts/2529807823.html","addf68a2fab2b6f9784c2c3b1ad87943"],["/posts/2596601004.html","c9093b34adddf50402e73632d8979a13"],["/posts/2697614349.html","30c94ff2dd959094a113447ab11eb9d2"],["/posts/2742438348.html","eda535f63ea7570584b22debcc1104e7"],["/posts/2768249503.html","9fe22897b29f1670c47123773b346c92"],["/posts/2864584994.html","e51daac94fd028ea99f332fbb8e51f76"],["/posts/2888309600.html","4bb1eef701b9bae3bbd15177ea7ddbd9"],["/posts/2891591958.html","d45506fd5f30e4bc434c3cdddb47fb04"],["/posts/2909934084.html","79d856375be07843b840ece4abdd8188"],["/posts/2920256992.html","c1740c4e401df55c5cac5c2421bed94d"],["/posts/2959474469.html","a280fbb88549c0e2ded1b0a44df71cee"],["/posts/3005926051.html","fd03bc7f1485de16da70be40164fad4a"],["/posts/309775400.html","d275f35280bafca277d4544069a39040"],["/posts/3156194925.html","202d4d34ef8106f1b5ac2311c5cad0ec"],["/posts/3169224211.html","533edbec0a2ead2e1990a19bfa036a24"],["/posts/3213899550.html","23adefe3c611ebd3ae1f581f78f71bca"],["/posts/3259212833.html","91194567d88f44c5a86f5dff1b67aa71"],["/posts/3266130344.html","31436bf5808ac8dc0a69f8c22fa18368"],["/posts/3292663995.html","9ad7d17c16f07ce0bd272401eeecf52c"],["/posts/3297135020.html","d461d87b70075cfc7003299dbd0cd41b"],["/posts/3306641566.html","e453b5f8580e836f83c9162066bb3494"],["/posts/3312011324.html","5bd72aeefc209369fe269e69fe4ed97c"],["/posts/336911618.html","f2b69e34029669cf7037b2ee8f902e95"],["/posts/3402121571.html","9924b73ad2c74f59b7d352962bbb0a9f"],["/posts/3405577485.html","af851a39b02d6960e9f0603789436776"],["/posts/3498516849.html","01c32b2346ba408ca08a5a1f3561d47f"],["/posts/3513711414.html","3b1c8be18c12e3f2efc3acc121ed862a"],["/posts/3523095624.html","52e13e7f14e814d632ebd6047b28139f"],["/posts/3546711884.html","a450105737498d1301ee4980fa026205"],["/posts/3731385230.html","508e0f4d82170dc37b911cfb4e2fa1f7"],["/posts/3772089482.html","7afbab3d6f85e5ff3bdc682d1e5b52de"],["/posts/386609427.html","d65403453a11e196b3f9417aa0dcbdeb"],["/posts/4044235327.html","7bef2d37190aaf0a22c917f2cd487c45"],["/posts/4115971639.html","70f2252e49be244986ec3741e685c94e"],["/posts/4130790367.html","361448cdc5e221885aea0f3afa83364e"],["/posts/4131986683.html","16f54abdf567b77f90c8d93bdc145074"],["/posts/4177218757.html","b4ff245ad090484761f0b9b31bcfe5f0"],["/posts/4192183953.html","7903321d5214c07868700efb7608d561"],["/posts/4261103898.html","7cb70b92d10e4621c46ec55d9a378d12"],["/posts/469711973.html","4346b5fb465fad53f59645c2d8a9e263"],["/posts/482495853.html","d859750bb83ec8ae3b81de7f8a7d6a68"],["/posts/488247922.html","355c5ee077cf8f66680e3b87965c3d29"],["/posts/517302816.html","6732ccf3d1f4a5cd459351781bd5fa47"],["/posts/570165348.html","a06244ef47f585255496fcbc8e8c9ff9"],["/posts/595890772.html","6da107462a05ec160856abed2bfcf698"],["/posts/67485572.html","1aee66eacc9ba813a5f2eb8f0ceea8bf"],["/posts/694347442.html","a25b914ee031666dc53a074232a2f68b"],["/posts/707384687.html","2684dc3847dacb0388aee2661bbb46db"],["/posts/71180092.html","7fa8f4d56896fdaf3af419bd4a3e1332"],["/posts/716459272.html","c0727749ce1643c526bee71a71bec208"],["/posts/765481613.html","4880e229f3ee72cc4a4fcbd713233af6"],["/posts/778231993.html","8bdd7c625c41aacce405362a2bfa14c2"],["/posts/795397410.html","932b2901bd20cdd5beb5c08c4019dcc5"],["/posts/820223701.html","87387aa11f0940c3a916965064f7f79d"],["/posts/830372185.html","0f0a0721e025973032aede57646d9c0b"],["/posts/88294277.html","76b6f4b98dea67707ec63dc6aa9dbb8b"],["/posts/939963535.html","63483fa42765cade33f83aded8c2059c"],["/posts/983786067.html","a0c1d80a21ba2c7eca53d53c24b1c17a"],["/sw-register.js","5766c2543a5ec4bf17750909c030456f"],["/tags/C/index.html","3c14280cfec3ed302897ade4028ee401"],["/tags/C/page/2/index.html","8767534f0339be030f60ec94fb0a54c2"],["/tags/C/page/3/index.html","5780402ee4e5ab00f3d1ff81809321b7"],["/tags/C/page/4/index.html","30f6977d6879b9916e3736eaca4e0677"],["/tags/ETL/index.html","4fd316b28d8e248b9f70804d509399bd"],["/tags/ElasticSearch/index.html","5ab02313e7283326b8d3166b098b9017"],["/tags/GUI/index.html","80a3fc77f4b67a45879dcf7c7188d2ac"],["/tags/HBase/index.html","43ad613bec8bef9bdc7037ba61360e3c"],["/tags/Hadoop/index.html","6f715b743d0b78ea4e7ad90dd7a3cd50"],["/tags/Hadoop/page/2/index.html","ab9b4ccb6ab5865f4f6e1595125dd50d"],["/tags/Java/index.html","94241917760d556bd8ed36c7296ff591"],["/tags/Java后端/index.html","79523629269c23b5d4ed5e9a36ebb04f"],["/tags/Java后端/page/2/index.html","623dfef6df2b8b9c8108960cc7a63a73"],["/tags/Java基础/index.html","5ab31cbbcc7d304d83e41faa4e8e29ae"],["/tags/Java基础/page/2/index.html","ba1a261a0316878db54a4f95885460fb"],["/tags/Kettle/index.html","98012063fcb9f153038b19532d2548fc"],["/tags/Kibana/index.html","2f2e20775f97b23a4feb7e2f22b8d24d"],["/tags/Linux/index.html","0ed11888f89445e0fb91c7461bad9729"],["/tags/Linux/page/2/index.html","785894492462815a93373c785e5d8d3d"],["/tags/Linux/page/3/index.html","8a39751bfb5a67646b152ada44386a8e"],["/tags/Mac/index.html","a09d6e025e02f3817300c42a0ae0e07e"],["/tags/Mac/page/2/index.html","8354368ef35b347548eaa5a6cb8dbbf9"],["/tags/Maven/index.html","764f95e568bd57e6f9f4fe33ddb914f7"],["/tags/MySQL/index.html","b874d36a87b8b94c6b0ccb0281446092"],["/tags/Python/index.html","5131366765e58f97ac80162335e3d355"],["/tags/Redis/index.html","e5bf2e342af458fd15b46e0136d40038"],["/tags/R语言/index.html","905b0216504b87055f9c4a0663d00c45"],["/tags/Spark/index.html","d1baa505b713182f7fa1d5ea1c19ba40"],["/tags/Ubuntu/index.html","d8fd06b4fe6dd9c5962da6bfa49d9053"],["/tags/Vue/index.html","1e54f9b587454868fbefd7367a970559"],["/tags/Windows/index.html","0acb9f7958f6863ae5a20e9984ac9946"],["/tags/ZooKeeper/index.html","d8cf75493bfa3eb462b104b166d64638"],["/tags/bfs/index.html","b7eb58d1cd4680250a3d43fc59f40a9b"],["/tags/dfs/index.html","f71d0ee82f82e20dd1ce336b1fa50df1"],["/tags/folium/index.html","7a47696a51070f569cba0928b17ec1bf"],["/tags/git/index.html","6eeccdfb3ddaa38ebd6c8c9a25d01513"],["/tags/index.html","bc6ff59d2dfac321e98e5bfb0701c521"],["/tags/latex/index.html","83f970e7a409ed7f3501db2d3bc00c03"],["/tags/中间件/index.html","25b42089583ce77ee4591f0e2b72c96d"],["/tags/二分查找/index.html","fb738b759fbf35c2ca1f5d744d3cc3b5"],["/tags/优化类/index.html","84a71d053090e19776b93e9bb16ed90e"],["/tags/前端/index.html","c54a1306912bc4444661479f2de91c90"],["/tags/前缀和与差分/index.html","75bd803bd6d24ef3159eb43d5c04710b"],["/tags/动态规划/index.html","57d5ba0850a433d07814423a7b18b77b"],["/tags/动态规划/page/2/index.html","fbd45aaacd061b05b2b7903ef4a03ae4"],["/tags/博客搭建/index.html","bc7bd2cde88fae27b87553346fea8c19"],["/tags/图论/index.html","656d6661aa3d8b921f15270ba23d542b"],["/tags/大数据/index.html","ab94490c822e2fe99ecb4ad45cccb514"],["/tags/大数据/page/2/index.html","b979d669a15b95f2af4ec96686020400"],["/tags/操作系统/index.html","a3cda93cad5b3d90249d4f18b302d4bf"],["/tags/数学建模/index.html","567e994808370de3033ff615eb31933f"],["/tags/数据库/index.html","5435cb7d5287aad23818f3667a562b9e"],["/tags/数据结构和算法/index.html","756ce35abb4e016b6088e349e06c8557"],["/tags/数据结构和算法/page/2/index.html","ad26c446964c3e4bdbfb751b73d4d403"],["/tags/数据结构和算法/page/3/index.html","2426c99b69bdf165f2a426987e9d25f9"],["/tags/数据结构和算法/page/4/index.html","2a2f0a3137707d35dd946203ba4750d4"],["/tags/数组和字符串/index.html","6689be64bde5eab60caf8735d55ce2ee"],["/tags/数论/index.html","87dacb8e6bd67b3cbf58c39bb119847b"],["/tags/枚举类/index.html","8ef359e3a23f61e2648be392d2cb790f"],["/tags/栈和队列/index.html","8825b885921c77a7722a7f5ef2797b18"],["/tags/树论/index.html","178891f6377844bb3499584631dddf65"],["/tags/测试/index.html","e872154ee695b715894abf061a20b61d"],["/tags/环境/index.html","4a4a08ec1cb546f1380d196c20d22562"],["/tags/环境变量/index.html","4897a6c798dd0dc6d82c9ae1246d5494"],["/tags/绘图/index.html","1bb3a04813c66c5b715e440208e70bd1"],["/tags/编程工具/index.html","796adf94cd0184b9a2dad886e2b7e2b0"],["/tags/编程环境/index.html","819f6abcdd17852f1ebdac7f9d30f135"],["/tags/网络编程/index.html","89963e9dab65b082c1460a3e1702f820"],["/tags/英语语法/index.html","5bdced070d3bf629e2ea0d1617715994"],["/tags/计算机操作系统/index.html","dbb278ee705a67758eb3cd424d86dd1b"],["/tags/论文/index.html","8b4f5f27dba1278cf7f8a6bb705313b5"],["/tags/资源下载/index.html","b9c610b661e596cdb954f55410380890"],["/tags/链表/index.html","332b463d5fffa03cb1009d4a922b3bcb"],["/tags/集合/index.html","a3e85fb2f38e3a0f1da0a0aa1aa72479"],["/tags/集群/index.html","6a518061459dd9d6dc05505a4a86272c"]];
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
