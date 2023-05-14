/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","da20c6911e875f155d8a60cd20286b26"],["/about/index.html","6a852b3a49cb274e531aadc76e6bb7e3"],["/archives/2023/01/index.html","f47a3dc1fa88bb1bd274a38f1c38e1d4"],["/archives/2023/02/index.html","23229a8f224b8353b3e32fc2a3e6463d"],["/archives/2023/02/page/2/index.html","a50a2ad8fcd2d39ed4e2626a4dcb706d"],["/archives/2023/03/index.html","7d94ee0ddfbed04cedf8a5143cb81188"],["/archives/2023/05/index.html","2cf7ce4df66efd33bd7e1e5afee140cf"],["/archives/2023/index.html","1573dd0e1de3c32aec94fa72ffacfdb5"],["/archives/2023/page/2/index.html","655b3d4d2570503900c4e56d0b1b4abf"],["/archives/2023/page/3/index.html","6a235d107e05dc0097af6c8ab2fda58c"],["/archives/2023/page/4/index.html","0e38021efb18bc878656cd0fa44279af"],["/archives/index.html","a6b428b836cc51c0aadec7a78ea59613"],["/archives/page/2/index.html","ad2e9cd21465d5996027ca4a5ffd245f"],["/archives/page/3/index.html","62da88b27954c0dce897b1c02f6e2762"],["/archives/page/4/index.html","f73391278495b0be51a1a8b36951d630"],["/categories/Java/index.html","38d1296200215991922f61cdfcc082d6"],["/categories/Java/后端/index.html","98a086ecf3a2f4761b8365496c1cfd84"],["/categories/Java/基础/index.html","7b036227b15bd6c3208fd3f845a5a20d"],["/categories/Java/基础/集合/index.html","71e3c80f41c24e2f89efd266edba91e9"],["/categories/Python/index.html","c04d023fc5385a3020f26fc9422930c9"],["/categories/Python/编程环境/index.html","bc5e5dc0b8ec10bb4d4fa20b5fb9622e"],["/categories/R语言/index.html","d25ca27dd1bc879f25d05823184e2edb"],["/categories/R语言/编程环境/index.html","8661858baa77ca00c040277e35d019b2"],["/categories/index.html","3f52179497947888c6eb647da5140aa6"],["/categories/中间件/index.html","d0f2532a5cc235a54e55e1745c25fe20"],["/categories/大数据开发/ElasticSearch/index.html","c4310df0409a3cb05a66dac2645854df"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3b7495d1d81efc4d84d186a728432bf5"],["/categories/大数据开发/HBase/index.html","cfc257afd48434106ef80a8f5c2efd8a"],["/categories/大数据开发/HBase/学习笔记/index.html","b3ace506236675b7531853c4017d33ba"],["/categories/大数据开发/HBase/环境搭建/index.html","04ef160bbd0d77ec3347c8b34a272842"],["/categories/大数据开发/Hadoop/index.html","2635a4512d589262d1c28bf73a5b4ce9"],["/categories/大数据开发/Hadoop/技术/index.html","e32efbc8ff55b776207b01f9a5a84c3a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","873ced5a481687585f7c2c2bad8187af"],["/categories/大数据开发/Redis/index.html","610bdfecd5f2c1f48c7a825bb170ad00"],["/categories/大数据开发/Redis/环境搭建/index.html","680e661131d211d923670f2e08c29555"],["/categories/大数据开发/Zookeeper/index.html","fa410c1f4c19903596c54a388a5a5e3e"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3e10656a57f845a9b248728999004875"],["/categories/大数据开发/index.html","39c80e6d151691077dcc878d6d68582b"],["/categories/操作系统/Linux/index.html","e2d49469bea9e1b1a7c7fdb55c7e4ffa"],["/categories/操作系统/Mac/index.html","a57064135632c356ef871b6ae7e48a0d"],["/categories/操作系统/Windows/index.html","70030ea5995c79d159fd2700c74b4840"],["/categories/操作系统/index.html","f5de3fb45a2eb2ae65c0a28b91a0b530"],["/categories/数学建模/index.html","78ecc0917dd3f4334a6cbb4b5a03e179"],["/categories/数学建模/latex/index.html","e6684f368a7710e1fac96eee01a287a1"],["/categories/数学建模/优化类/index.html","b68aa95b12f28aa8e0bcc77aecf36597"],["/categories/数学建模/优化类/现代优化算法/index.html","64a34522977aab2b438cd254569eb58f"],["/categories/数学建模/优化类/规划类/index.html","d5b30cda4afefa8e04656690931ea6ba"],["/categories/数学建模/绘图/index.html","b0b80462ea167150f4fe7e476307377e"],["/categories/数据库/MySQL/index.html","b2b32d859d026961be43c49fd27b1b03"],["/categories/数据库/index.html","2d25c07cb42dfaba8e433558ebdc374f"],["/categories/数据结构和算法/index.html","f9b7b574d96b77e0b00312d3ce69406b"],["/categories/数据结构和算法/page/2/index.html","1de272b7b756584e454ad13b2bf2afca"],["/categories/数据结构和算法/基本原理/bfs/index.html","62c2572a9716c72f7829c681e26bb4b7"],["/categories/数据结构和算法/基本原理/dfs/index.html","8035b0ad6581f627949996a6c1a3bbc9"],["/categories/数据结构和算法/基本原理/index.html","17fa39847090a2d4cca33dba2da20cea"],["/categories/数据结构和算法/基本原理/动态规划/index.html","48040b9f9530b532a13e62e9fc239454"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","c3a6aaa9a746bb62f5d01dbdfdee1a9e"],["/categories/数据结构和算法/基本原理/图论/index.html","a2ac6a150fdf86082b4985b4360c0642"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","eb350ac04dcd49ce96aff0d1605096ca"],["/categories/数据结构和算法/基本原理/数论/index.html","153aa8b587be315bf54b05c1e4fb5853"],["/categories/数据结构和算法/基本原理/树论/index.html","6446488cc3998db9eca182ddf2a382be"],["/categories/数据结构和算法/基本原理/链表/index.html","cb4e8f323c87e420405c5f02ebe656e8"],["/categories/数据结构和算法/算法题/index.html","249c1139ebabbb2509e39fd4507f53d8"],["/categories/数据结构和算法/算法题/二分查找/index.html","01030ea95120ecc26bd24dd700d23ccb"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2d22d73e08f0958a2130b78d0baeff3a"],["/categories/数据结构和算法/算法题/动态规划/index.html","9e04701c73ff40b1e5a5a7c61cb19dc8"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2cf592500f985db12adad4953a3ef39a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","18ecd158bdf3202f048ddc8ce39a2d63"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","132c268a3e4c6ad4eb2dd0ae92a1da23"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","1b91cd023b93c6989b85f120c41a33f3"],["/categories/数据结构和算法/算法题/栈和队列/index.html","cabdc3c3fe5e89c535adabe959520e7e"],["/categories/数据结构和算法/算法题/树论/index.html","fbfb49462c4f87df61713b3d2561cc61"],["/categories/杂七杂八/index.html","1a25ca2f771b80b64c16dd3c01686ccd"],["/categories/杂七杂八/博客搭建/index.html","551f9e4f14aad4041851d3c695430a9d"],["/categories/编程环境/index.html","7dbdd2e820d02f8ac3d1dfd56d2e7295"],["/categories/英语学习/index.html","d9b2b74c7741c9dfcc2680e4da748364"],["/categories/英语学习/英语语法/index.html","a5ee39b42526cc24484980cf9ed3bd52"],["/comments/index.html","aff26b1207062a0348a9a365fac0d2c1"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","18de5fbe643d58bb130ee5c4e44926fa"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","f492ac9c51666e65da64f2372f58b854"],["/movies/index.html","a4609bfb20bb68972f501d3bc086ddd2"],["/music/index.html","4d1cce9ae207eecd2d01dd49facf0f49"],["/page/2/index.html","79673ae328ab04351640846fef6a6d35"],["/page/3/index.html","54dfc0d6e0dbc8caa98aab2c0d66c122"],["/page/4/index.html","991d4d6fd6635d4928a4691bb3305530"],["/page/5/index.html","13d088f71e31b39cc5c8be29eb1fdc39"],["/page/6/index.html","20a9720fdbb88d483502007c7c88c4da"],["/posts/1021360842.html","6d66d1bd78c9332ec83d595830bb1a21"],["/posts/1120620192.html","c0e2e808fa511e5e3a8b3d374ac7a5ed"],["/posts/1141628095.html","7a9c2a977c33a9730905ca00a9856b10"],["/posts/1168613674.html","31fe9a45fc6b4e03f7b8cffb9c0de6ce"],["/posts/1219920510.html","f28a6747fe922a45752c7c33326a17cc"],["/posts/1222166338.html","728106364ec0805e2ef3b8e528721c8a"],["/posts/1259097482.html","db0ba287763712b54aff04d3869659e8"],["/posts/1271036369.html","d17d2d6e2b8d67063aa482165f6a1e39"],["/posts/1312847445.html","f39d7293f44d3b75b79400f43d34a0d3"],["/posts/135355774.html","a6eb917368ecdca5531271dfd20f45c8"],["/posts/1375344716.html","a3c27fe9a84764599597359b4b3952bf"],["/posts/1388991698.html","73f5b3543db540898a732bc0f25042bd"],["/posts/1410315814.html","d610b369627aae25a74e7e0560882812"],["/posts/1452790229.html","aad5af9dd32713b7ac3833e6f980d3a6"],["/posts/1470079884.html","9a861a501b4763477c94d54e8ce6cd0b"],["/posts/1470079885.html","d1a2b68e82dff9c26f68bad7e7d5717c"],["/posts/1470079886.html","b10bf493e29bc9c1b10281c3ac75cf63"],["/posts/1470079887.html","393a17759671806219812b45dc4eb0ff"],["/posts/1498536549.html","1c5bbdd0a4663efac5ff498b86a7cee3"],["/posts/1547067935.html","a7c0e886e6f55a952ebb160444827209"],["/posts/1557866301.html","6a9dd85a572ff9099a75ee01b5675f38"],["/posts/1571776361.html","7afc67a7d22cdb026dd5ddc50093daa5"],["/posts/1605124548.html","203d12dc13cb9483ef40597cd623e2d9"],["/posts/1633036852.html","f4e060e5e9769d2da3849b258554d6a9"],["/posts/1765123828.html","3ba57de7bdafcfbe3552e1ea64dc8cdd"],["/posts/1767336200.html","3b2d44fd65d58a61b8b975056ee75505"],["/posts/1776114197.html","8f1801384532a396d313907b4ac152ba"],["/posts/1817748743.html","06f7ef0cf72e55dd3d9cb2af8d0d8be5"],["/posts/1925125395.html","13c11d5f0176d988aedfd4935589ca7e"],["/posts/1966191251.html","57655273d4f444156a51644b0ddc5c94"],["/posts/1987617322.html","2b1d1d9faa5b338fab11dadf076f2527"],["/posts/1999788039.html","47f3cabff5e1e05dd747781408f8b4c7"],["/posts/2075104059.html","3dc771551fa801eff25e0d6a557658db"],["/posts/2087796737.html","38c603ed9e6bfb62e1402c5b80845c33"],["/posts/2106547339.html","253524d843dfa5ea4d73ee602640c476"],["/posts/2207806286.html","08502204900f43f1ac7125158ac13b2e"],["/posts/2225903441.html","740d3f89a76e8762a8979c94ef626d15"],["/posts/2265610284.html","ff692bb06d563ed61230b587943974fa"],["/posts/2281352001.html","c36d5b268133912e2e3dd937bb5877de"],["/posts/2364755265.html","000886d363a122fd0225799ce9c27e5d"],["/posts/2414116852.html","8d6db6b95386c39b675efeca81f9c2ae"],["/posts/2482902029.html","010c232321bd7a6593231af03dece93b"],["/posts/2495386210.html","ad6ea9ba87916cd63247db4c510cfbf8"],["/posts/2516528882.html","229bc385840893a8d1833c238b69811a"],["/posts/2526659543.html","c73daf5c380610f4c202e75e5a3d3e31"],["/posts/2529807823.html","6c706b46067b6430827a1246b7c83523"],["/posts/2742438348.html","5ae18c399b2449d740a6991251daf013"],["/posts/2888309600.html","ee1293c93260d4b352e736647096d335"],["/posts/2891591958.html","9b3dd039dfd0684cd871f574124f1382"],["/posts/2909934084.html","8f052b8c07821c82fa98f8c65ae28849"],["/posts/2920256992.html","812033baa4881067f76371279d2754d3"],["/posts/3005926051.html","33e6c83767730ab57344eb997e194e38"],["/posts/309775400.html","be2cad8b1b6ff17328a781061c5ca3a1"],["/posts/3156194925.html","bf6cd4dea2c0a42db52d1cb3660e6716"],["/posts/3169224211.html","14857edfc4c3b72fd3c2af9a3d873f85"],["/posts/3259212833.html","772d029e39182f01c9ecf81a4c095ffe"],["/posts/3266130344.html","478ad385adb0e4dcbd98850dc89ef770"],["/posts/3306641566.html","121b13dd3b3e28f9f808e67f2aa29b79"],["/posts/3312011324.html","aeea494cffa0625cfa2c931b5f5daa7a"],["/posts/336911618.html","03d8451957aacd774432e18aae4ce26c"],["/posts/3402121571.html","4efc5802acbad4a73bb9827c85f00289"],["/posts/3405577485.html","3f5972a385d4db5625609d7a8604e10b"],["/posts/3498516849.html","9a704e28148d92a50c47efe39477b34c"],["/posts/3513711414.html","a06f317fedd43211b5c12857d3dc067d"],["/posts/3546711884.html","7b844a8d46decf91e09a791fc6373d27"],["/posts/3731385230.html","84473afbd99e6fddcc68c175e2c317ec"],["/posts/3772089482.html","d16f8c1b67ce053deed1f9158352294d"],["/posts/386609427.html","b214bbbc178dad8626412a0aa8f2a417"],["/posts/4044235327.html","0b6e93266e11e3cf6bb8a9fedfa5dace"],["/posts/4115971639.html","e404093afdd383bc4b27cd74dd536bc6"],["/posts/4130790367.html","fbfd729975b156089d2c65e20bf5ba53"],["/posts/4131986683.html","d9229d05dcd2f9c1fafd5b8114db3bf5"],["/posts/4177218757.html","0020d8b35061751064bdda0055c6e1a5"],["/posts/4192183953.html","7d0cc6ddd2acba477203fc40c173f634"],["/posts/4261103898.html","3d40cb7053d5d4d185130ee29e8ed355"],["/posts/469711973.html","323acb134c5915cd706cb813e17a0ef8"],["/posts/482495853.html","b68cddde9739c43b26e6d76a5f975245"],["/posts/488247922.html","8aed1c4da2f4ba23cda3143bc2baf914"],["/posts/570165348.html","59ea1262e0e9778efabfba855a669ed2"],["/posts/595890772.html","6dfa438a7615a2cdf9052fef8e7bd04d"],["/posts/694347442.html","c598cf43df3044801087adce2af10f1d"],["/posts/707384687.html","afd8668f1165bf146c68294d0a1aadba"],["/posts/71180092.html","bcb242136eabaa608621a362b7f3a9d8"],["/posts/716459272.html","9f3e360a6fe81e1295d88892859816c0"],["/posts/778231993.html","14a8746b5bf8b17fac9b40f37475f2a0"],["/posts/795397410.html","0329d41489238e59b33e23ab02b68fa5"],["/posts/820223701.html","bcdefbf9f36f8973a2845ce21b29265d"],["/posts/830372185.html","dc5edce82f214c726bb2d91efc246111"],["/posts/88294277.html","70ee7a8a626f45c9412e6af885893cfc"],["/posts/939963535.html","0fa4b6d07fb32f73372f155aa34dd1a7"],["/posts/983786067.html","0f99dd0a3c7f8ae14d020e73bd2b7e14"],["/sw-register.js","53ce920a96a6bbe33f502727d904442e"],["/tags/C/index.html","6f4325ecf8ffbccbf091798cf05962be"],["/tags/C/page/2/index.html","5bb5e8ad607b16004064f4335446e819"],["/tags/C/page/3/index.html","819b1defed3811a5ed15973305b781d9"],["/tags/ElasticSearch/index.html","3e52b5543021aa819b5d8f1358c6f273"],["/tags/GUI/index.html","9d1030d9a5b848788895f83c72510c8c"],["/tags/HBase/index.html","09e8a62ff234374d10d5d5fd78693cb8"],["/tags/Hadoop/index.html","452ce0d87eccecd632ea7bd397d49012"],["/tags/Hadoop/page/2/index.html","90d0c3632f8f55d6a37e16dba22b1bfc"],["/tags/Java/index.html","c8545f9563e49b2855b5b5fc01e160a7"],["/tags/Java后端/index.html","3256df2bde3bb65c0a32a08c6f49b5f4"],["/tags/Java基础/index.html","e9245426bc257a9e23ac98ee0e0ba30e"],["/tags/Java基础/page/2/index.html","d550e753cfbdd8316fce432dddd7fba6"],["/tags/Kibana/index.html","e1d3077c2e376ef76060b8f9345a0a88"],["/tags/Linux/index.html","6f04331b32b1e182cb08f275eb8289e0"],["/tags/Linux/page/2/index.html","f9298e98eb8a9c8ff0a4cc9b582ab620"],["/tags/Mac/index.html","3e3efbb00e989daff61a613a0878deca"],["/tags/Mac/page/2/index.html","d1de8a102956e61b11ef2b25dadcddbf"],["/tags/Maven/index.html","e882a1b844a2dd3102784c45a452f7a7"],["/tags/MySQL/index.html","0fa72d9ad32945f576e53c64fb1cd863"],["/tags/Python/index.html","8ed968350ba291958ebdfc5b6545bd94"],["/tags/Redis/index.html","7523e98f71d64df7969ce31dae62eba2"],["/tags/R语言/index.html","c0fa47752704500250bb59708806ed9e"],["/tags/Ubuntu/index.html","36a15693c1fddc3bab5832aa192d3f70"],["/tags/Windows/index.html","fa19c6e42220400671ce2f85c20f754b"],["/tags/ZooKeeper/index.html","bc10542ad79d37273ca388508c9817a0"],["/tags/bfs/index.html","f79518c313b9cabf70f558e7be29217f"],["/tags/dfs/index.html","43e08ee197f932217dfb2dd5e2b4c5ca"],["/tags/folium/index.html","6deb88855a85c1ad9f9ec7979e51a553"],["/tags/git/index.html","281e902ea771e8f869bf9ef4a216ec15"],["/tags/index.html","7c90c93c06aba49d3754ab29f7240b13"],["/tags/latex/index.html","7e085c423013b86273fd429962826a2b"],["/tags/中间件/index.html","629d6c3fe9c68be84ce5b80a6e7febf8"],["/tags/二分查找/index.html","800c5b202c7cc2318003e689a106606b"],["/tags/优化类/index.html","da372ed086c6969b4c6aa3bb6ac18154"],["/tags/前缀和与差分/index.html","028671190573a20b4ff57a23b9a5a1ec"],["/tags/动态规划/index.html","224600c404dedb18f4ead5e391db0919"],["/tags/动态规划/page/2/index.html","cec1b746a9987a6a8ef1c110437170c4"],["/tags/博客搭建/index.html","1383b593de5a7d868ff064e374a39a52"],["/tags/图论/index.html","ae9e2af3e8604c6f83583f47753acf5a"],["/tags/大数据/index.html","35f616e272a3a06565a51875a3a32327"],["/tags/大数据/page/2/index.html","7dfcd137c64e32c246ba3a31348009ee"],["/tags/操作系统/index.html","529d80e09d8bdabb7e4a7d84a0b43946"],["/tags/数学建模/index.html","d66d1457c6014aaf1aa569ad23bccd7b"],["/tags/数据库/index.html","c05dc53b04bdb2664e9c4d789addea21"],["/tags/数据结构和算法/index.html","c456ddf34d1ad7fea069c07e18eccd79"],["/tags/数据结构和算法/page/2/index.html","a0acee7826c6ef7351ae6d415a5dafb2"],["/tags/数据结构和算法/page/3/index.html","e32c07055b2cecb23e14b5bb4cd70b62"],["/tags/数组和字符串/index.html","1f5a9fc49a37c1b6f214f1a1c04ae735"],["/tags/枚举类/index.html","2db1e929a6835ab134ac1cb36bf19cf5"],["/tags/栈和队列/index.html","ed738a7032a56e7ad4b0d462415e1d17"],["/tags/树论/index.html","78b76bcc639b4866a494cf10e640be85"],["/tags/测试/index.html","ff77f653652bb6e34890dc36bc24dc2d"],["/tags/环境/index.html","991a0ae6106fafcf99f25d695a0cfdcf"],["/tags/环境变量/index.html","cb74ac5ce2e646a2be89807f0863b6aa"],["/tags/绘图/index.html","e032fb2fff53aafa6ff5c93d1767b5f3"],["/tags/编程环境/index.html","2194e61f1888c3755f82fddb3c91dc94"],["/tags/网络编程/index.html","e131d2516deb033cbca36bd396c7e2cf"],["/tags/英语语法/index.html","2a6e8c673d04ffbe4371f0f5fa03e7ba"],["/tags/论文/index.html","114b8277f772417bafefd4243e13618d"],["/tags/资源下载/index.html","7c2600fa62bd4342c5f86b19e161a85a"],["/tags/链表/index.html","27ef529fc9220543dddb978602056cba"],["/tags/集合/index.html","785c1164c515d155fe26f17b4bda21ad"],["/tags/集群/index.html","bf3998ee7bcbd13cdc9c42de844dc656"]];
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
