/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","46cd156e98fb60c870aed798c8a224dc"],["/about/index.html","dd743e8c74d3e3ca45df3f61ec5b5ece"],["/archives/2023/01/index.html","69e94c11af78bf3d660f08ea33e33934"],["/archives/2023/02/index.html","0122f746588915e5c3691d214bcbc9ab"],["/archives/2023/02/page/2/index.html","9d25d40c46e230b49676db12aec43ece"],["/archives/2023/03/index.html","5c05b4cb048062830a309a2b62ef07a2"],["/archives/2023/05/index.html","dc96f9d6934b0a00bd93651023ca0f7e"],["/archives/2023/06/index.html","ea2baf3f1528c9fba6a1c3a06ed466ae"],["/archives/2023/index.html","dc8a4e3d309332a72932a91d0721fa61"],["/archives/2023/page/2/index.html","0f711e3f6daea7f96889bb9cb53a68f2"],["/archives/2023/page/3/index.html","e6ca9b06e3ffa77577e2df03ed49c796"],["/archives/2023/page/4/index.html","585c858f881b526e09e6326ad977810f"],["/archives/index.html","dc85ac7462758271aad2f88c98b4de63"],["/archives/page/2/index.html","a9bd021c35a9baae1597d8830a17a23c"],["/archives/page/3/index.html","7889a0f6c9df2125e7f6e19a575586bd"],["/archives/page/4/index.html","8967ccd616c5fd24311e9d0b5d19dac1"],["/categories/Java/index.html","a218ab4ebdb83281348e4f917146a266"],["/categories/Java/后端/index.html","43224f06e0f367af4f1a0db80ddab666"],["/categories/Java/基础/index.html","008a2b67085efe7accd1aef568d83df4"],["/categories/Java/基础/集合/index.html","35273c33063efb6b7b7ad47e2626d554"],["/categories/Python/index.html","bf7ef292488397d846c7c0180cf35469"],["/categories/Python/编程环境/index.html","a327a8ce892f89755a3f7a828a7a0e00"],["/categories/R语言/index.html","716874e3f93a875eb27b5ea78d5eaa44"],["/categories/R语言/编程环境/index.html","0fc9a75a9a57f348a9d88a6d9d1823cf"],["/categories/index.html","c6f98730e57ebe6b90fb31614125c14c"],["/categories/中间件/index.html","c02dbd4073ad821490b8f6562493338e"],["/categories/前端/Vue/index.html","36d459d5827bf6d0828ef104669fe897"],["/categories/前端/index.html","8c736fffa824636c6e0606c6cd847699"],["/categories/大数据开发/ElasticSearch/index.html","9a6d117ddfcbc05b977cb9269e71b3d0"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","f0c8a4fc229c7e8ab0224c4b9b861d13"],["/categories/大数据开发/HBase/index.html","6aadfe97dd0f09e874abcae6026a100b"],["/categories/大数据开发/HBase/学习笔记/index.html","34196d7d60e2c1aa0940c7b06e5c8f26"],["/categories/大数据开发/HBase/环境搭建/index.html","cd178c70d0fee3f293951f5e5891c94e"],["/categories/大数据开发/Hadoop/index.html","3fa41f3047fa05167e6efc0dc567b435"],["/categories/大数据开发/Hadoop/技术/index.html","cad7813158b67ba441a3cd08c828ef14"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e4467413339da219b5e10c9f48e71791"],["/categories/大数据开发/Redis/index.html","77f9e332454fe63c267e187d5f5827b2"],["/categories/大数据开发/Redis/技术/index.html","c3f93a7c56e08929fec56ba6531d74eb"],["/categories/大数据开发/Redis/环境搭建/index.html","cb6d994c900a2662eb15a1987ea8a342"],["/categories/大数据开发/Spark/index.html","fab48995be598dc1660ca3363da9d809"],["/categories/大数据开发/Spark/环境搭建/index.html","792bcff7648bcc3e8f954890a6eeb9af"],["/categories/大数据开发/Zookeeper/index.html","233b1521a5f316aeac4a7baa4295d735"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","180fdf8bdeaacf1824acaf24e2aeae9e"],["/categories/大数据开发/index.html","46443852494f7d76c032cdaca1735981"],["/categories/操作系统/Linux/index.html","66a43f4a64185303c1b7e5b107d80e9e"],["/categories/操作系统/Mac/index.html","70904c2d0d30b4470b21c1551647ae36"],["/categories/操作系统/Windows/index.html","5e08296d7ca8bb71c482221fafa68bdd"],["/categories/操作系统/index.html","967d60929cb7ed599833f586232892c7"],["/categories/数学建模/index.html","70f3a4c12aff947b326db0e84b9a3309"],["/categories/数学建模/latex/index.html","85a99623da1cedac89c69b4c00170eef"],["/categories/数学建模/优化类/index.html","cf6aa914b69849b4b8f6d9919e43e99e"],["/categories/数学建模/优化类/现代优化算法/index.html","d92839862a24e37a7ac93475e20d77c1"],["/categories/数学建模/优化类/规划类/index.html","48cb94a39ae6a430e3fbeb56b43d3604"],["/categories/数学建模/绘图/index.html","c7fa31bcaf9a5bef0770ad76d8065c5e"],["/categories/数据库/MySQL/index.html","2a526180c2e49b7188176f325e2bd587"],["/categories/数据库/index.html","be960d7e5127944c771a16f953b88946"],["/categories/数据结构和算法/index.html","e1a21fdb04e3f900b3b74b03317e7e00"],["/categories/数据结构和算法/page/2/index.html","99d395c4f14625c5040cdbce27a3db46"],["/categories/数据结构和算法/基本原理/bfs/index.html","164b0a3e9f1a15312de28a0e6994750f"],["/categories/数据结构和算法/基本原理/dfs/index.html","7e6dd8c53e847b2d8e58965b273109af"],["/categories/数据结构和算法/基本原理/index.html","c345b956500f7bd50db009f8cfbeb9c7"],["/categories/数据结构和算法/基本原理/动态规划/index.html","dc3a8236db2ca9f3582c6633a4134049"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5627a28a2afbbd6a034cb6bfe80c1917"],["/categories/数据结构和算法/基本原理/图论/index.html","24e901a9537ff676f7801a9acaa2cd5d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","07dadd92c42e7c5bd4db5bc7373a1362"],["/categories/数据结构和算法/基本原理/数论/index.html","8fdb3b1befce626a4fae91f32d7fe85d"],["/categories/数据结构和算法/基本原理/树论/index.html","bd5bb96b1a1e8a9e8453d613f32e13e4"],["/categories/数据结构和算法/基本原理/链表/index.html","2a78c1229be386a8a1b243b94d0cc3f5"],["/categories/数据结构和算法/算法题/index.html","45d3fd8a5fc5c9cc3147b8cb1b058b5b"],["/categories/数据结构和算法/算法题/二分查找/index.html","dde5391c2795a8e25654d3f499793351"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6753a7679b084fb700d292287b4aa8c3"],["/categories/数据结构和算法/算法题/动态规划/index.html","7bd92046fa3d757d3eeaee5dcbb38611"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","68715f7ebace035bc0985bd242434c84"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","02d1b8f780871a454cc3d48069637213"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5e2cf10adf0229529db31f7b51049571"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","44445b7419f4387943c810d1123c3d22"],["/categories/数据结构和算法/算法题/栈和队列/index.html","fe2b6ada5ea95de6d2cb8ae47a91ebe3"],["/categories/数据结构和算法/算法题/树论/index.html","f8760985ca9c447ce6ab91c9ec3688b5"],["/categories/杂七杂八/index.html","0806a06c61618df70d6b96dac26f0765"],["/categories/杂七杂八/博客搭建/index.html","1b2131a333f7800c4039fe8d1878d515"],["/categories/编程工具下载/index.html","a5f16776244e74c0f5e74a9dde0af8e6"],["/categories/编程环境/index.html","6215768142344622cc6ec07003f338bb"],["/categories/英语学习/index.html","ca3f4eab687cce10d12de7d9a17a6e94"],["/categories/英语学习/英语语法/index.html","fe5b73e64b0cfa857bc6222f087b791b"],["/comments/index.html","4c6ac46c10fdeb594ec8fb3f38a3b3ad"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","f0c2c56f0d5d5e97c9d358f2e91802dc"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","958d2a66d9b1d0125b66dd17a9940630"],["/movies/index.html","a94c73e36e3d6c37506c555ee41e3b81"],["/music/index.html","2743494f0b8831c27f4444e775c1b965"],["/page/2/index.html","9b30e6b0a723ee24cd23dc85a72e928a"],["/page/3/index.html","872865db345761074b385c2ea9106c1e"],["/page/4/index.html","55a9a0f3ead0d5fd2bf69035d9a46b6d"],["/page/5/index.html","2c088e6e0fbb945756f8939976057cec"],["/page/6/index.html","ce6b06d116b07835093ba37ac941273c"],["/posts/1021360842.html","22dae59a35da718fe23adcfb1f6d07f3"],["/posts/1120620192.html","b69921991ca36624ae341b19252a26f3"],["/posts/1141628095.html","ce8fb2a342b254cfab35dff6f89ee04b"],["/posts/1168613674.html","84a1a73153ca72401af78a122d60ed23"],["/posts/1219920510.html","013bc6f0829d0141f667226d12ebac9f"],["/posts/1222166338.html","774c9abc0481967f6a85427b74d35a65"],["/posts/1259097482.html","2157de023d69317ec919a5f5c424434d"],["/posts/1271036369.html","fec749ecea797b03ef8b9e963da97339"],["/posts/1312847445.html","d2cd15d470933651731634e99ad4018d"],["/posts/135355774.html","54bd7596ac279ddef050f880d91b3c20"],["/posts/1375344716.html","a47a83b877e0b642b93971120ae76f2a"],["/posts/1388991698.html","a0b1699dc418474c9253166bcb550d71"],["/posts/1410315814.html","8c0201746696716dd8815c7daf58f984"],["/posts/1452790229.html","c78248909b2e7a9dd06699514487b0ea"],["/posts/1470079884.html","37cc6d0e8c90d3b31df415af96e62338"],["/posts/1470079885.html","40cc23a83b9ce276d1dc55a143a9ef5a"],["/posts/1470079886.html","a6a2d5300137d6391cf6cbea26a1a59e"],["/posts/1470079887.html","5acb84523dc86e2b91c30d334b9219ee"],["/posts/1498536549.html","24f0396a504f085539fa3dbbb8b94827"],["/posts/1547067935.html","7d27fe5408f98e60013cf779fcbff8a8"],["/posts/1557866301.html","b3aa7cb5ffa75b6ac2705cc46a437318"],["/posts/1571776361.html","c420a566a005c0dd61d8fa7e6195f8e6"],["/posts/1605124548.html","e5689236bc0babcb614ca5b4c259eaec"],["/posts/1633036852.html","d9af5c76b6714bf1b460fd91560fdd17"],["/posts/1765123828.html","2b9d8b76295a5d2ef8c600cd242075da"],["/posts/1767336200.html","47a59da8d04f0933ae34b8775e906be9"],["/posts/1776114197.html","9849b8c88e3ada5ab152fabce1d94ceb"],["/posts/1817748743.html","d44b497305afb70d61746743f3980854"],["/posts/1925125395.html","ca4664c7bd9bbf373c3e871c565c9bf3"],["/posts/1966191251.html","22abfff703fac2212cbf9887848b470a"],["/posts/1987617322.html","9cc2c1d0341042b7329174d7b96a4385"],["/posts/1999788039.html","6b19ec2d4c43bc5f316ee53f4a2d3841"],["/posts/2075104059.html","2b62fda871a01f568026804cdf609ef3"],["/posts/2087796737.html","841054fddbb076e05b56cdae4df7ded9"],["/posts/2106547339.html","970446fae34410de6e499f7ebf802590"],["/posts/2207806286.html","624735e6273bd1f51ee1eecf8c6766f1"],["/posts/2225903441.html","1f465b144ea353ac5447e4c847b7682f"],["/posts/2265610284.html","8b9c7a3cd14e4a3f52ecde758646a554"],["/posts/2281352001.html","1f712350227fe1ae18a349a59e9cb610"],["/posts/2364755265.html","651ae17e1ff9504c7028f743476132b5"],["/posts/2414116852.html","86ec29ccb7c784c0a5d3202ca08898fe"],["/posts/2421785022.html","85e7404844c9d2ed6c09e401f4622238"],["/posts/2482902029.html","66b8edeb47fb8135ff62830cd52a8771"],["/posts/2495386210.html","58e9e0dfeda760dbee503cba746cd1d3"],["/posts/2516528882.html","f68c94b99be418c0a8b53198186d0f09"],["/posts/2526659543.html","327f6258f3bc9df89837c70b9dd24828"],["/posts/2529807823.html","f7bbb4bedfbe9f1d6360a21cd66b3523"],["/posts/2596601004.html","7199207f903dbfced4cc93d8f0a1ddee"],["/posts/2742438348.html","741b69c3aec805d94b9505446cf7fe03"],["/posts/2888309600.html","48095c33618b56cd57b0d9ac4ea079a9"],["/posts/2891591958.html","ca5689e16626f7326a40f3f6b0b27666"],["/posts/2909934084.html","06edcde848552aee51024abd48163646"],["/posts/2920256992.html","6ef34e2b27c57249aad92b7fd8d8d67b"],["/posts/3005926051.html","a45757ed35a2963e866387288fe47bbd"],["/posts/309775400.html","fe7ab7de275c805019c741ac21f06d4e"],["/posts/3156194925.html","dea552bd2806b19ac6e997feb04d8314"],["/posts/3169224211.html","7e1dc807195e161bec0b91d710cf9e34"],["/posts/3213899550.html","a4eec770aec8721cbb70eed4de39d190"],["/posts/3259212833.html","376af2aef2f17b2b15e7ce040a6a3b6b"],["/posts/3266130344.html","56dbb311dd32ab3d601c810e3488c0c0"],["/posts/3292663995.html","5b63506af47fceb64328008f0b851e75"],["/posts/3297135020.html","987c0d0f5992a3ab82fa4f9a1375ab36"],["/posts/3306641566.html","3b817d6dca5acd4d8ced0a255bcb1d55"],["/posts/3312011324.html","5d1d22e3eb4499435c2443742993b8c0"],["/posts/336911618.html","667591ed20d9ad95bc21d76bb4a2a8b2"],["/posts/3402121571.html","33153b0fa52e69c47b3380e5d4d2dc9e"],["/posts/3405577485.html","c75c6fb6aef330127575b89a31fa0438"],["/posts/3498516849.html","72078da58030e4241297c029dc9037b1"],["/posts/3513711414.html","909e1b816a47ee1df2321e9103855ed7"],["/posts/3546711884.html","e2d82277b8d02e9599f50522f2117aa4"],["/posts/3731385230.html","41556267f3827e3e971bf284850a05cf"],["/posts/3772089482.html","ab067c272156ac62e953b05343511cb5"],["/posts/386609427.html","db970f18520fcf39130e95966ffabd1b"],["/posts/4044235327.html","dd82b6309fa4fc0a93383787dba1974e"],["/posts/4115971639.html","0c98e8630f76e3257739d5a091478fcc"],["/posts/4130790367.html","b3a389fcba7099f0ffc6663260095f95"],["/posts/4131986683.html","b0d92caa55ee023b2cef2b1b1e897a84"],["/posts/4177218757.html","67d4af75ca3f01f12b4e45b15732ac40"],["/posts/4192183953.html","478a9cbc9e1703fa6688699f2094271a"],["/posts/4261103898.html","3e52c88c29be052ec59d614466c4bd4a"],["/posts/469711973.html","801ee9e7b48896651828d9230951f762"],["/posts/482495853.html","09b9cde60d717cf73e908dc347569e9e"],["/posts/488247922.html","99a43ed549b7a03f680df4d16e7c5a5d"],["/posts/517302816.html","76ef585b8dca29e4bf1d795f2497ebe9"],["/posts/570165348.html","b6c105a74829af78483740fb4b252526"],["/posts/595890772.html","d022fe6efb69c09496c348f890954e61"],["/posts/67485572.html","4ce7e2f585f74f41880ecc74a3f5b5ad"],["/posts/694347442.html","9d657ad6fe2adacac1cc87c149c8397e"],["/posts/707384687.html","8306369d08608afab943ae6bd052ff5f"],["/posts/71180092.html","5c72eb5f9fcfde1da471be9e69a5076c"],["/posts/716459272.html","e4eee1fc4d006d67fcce516104f952b3"],["/posts/778231993.html","4f4a249437e6b5e4bff28eada7883132"],["/posts/795397410.html","1d53c5c810a5e4f2912be0c5cd525acb"],["/posts/820223701.html","5e9923acd51b6cbf3e0d4fc563359476"],["/posts/830372185.html","2f6b40641963cc64068ea9be7eef54f8"],["/posts/88294277.html","bce5bf101d663faa519ba28b60534eb5"],["/posts/939963535.html","d3ddbb5b72e41b54d6dc7e7cecb76714"],["/posts/983786067.html","5012d23fae9f083f1be2c59078a7362b"],["/sw-register.js","90fce9c10366e33b7562060ef5075552"],["/tags/C/index.html","400abe6448b3d2009d8425be35259541"],["/tags/C/page/2/index.html","0979c49a9f3d216333b3ee66d588ce50"],["/tags/C/page/3/index.html","ab103305cfea99df3bc98b54c5516422"],["/tags/ElasticSearch/index.html","9c225486955154c3fb287e2dca95b80e"],["/tags/GUI/index.html","b74140812f34236d2470b5d771f33b46"],["/tags/HBase/index.html","ea3dd6d59d3bfe50e63e36ec46a9fa8b"],["/tags/Hadoop/index.html","257538688704ed12f05a9c9b0ab5d776"],["/tags/Hadoop/page/2/index.html","50b41056298ab278bd974710d1845c61"],["/tags/Java/index.html","0de6cc1a3717cc4e608e632d1f98a13c"],["/tags/Java后端/index.html","339a08f8bd4ed6ccad2dd4cfbb7872c6"],["/tags/Java后端/page/2/index.html","03f8d3b77073356063cbd486a9763b56"],["/tags/Java基础/index.html","322006307efa5bb8e3dfed779b355cfe"],["/tags/Java基础/page/2/index.html","0db48bf953954caa8f4c686c02f6e8b1"],["/tags/Kibana/index.html","0a0692b283492fb48d8ec75f6fd77748"],["/tags/Linux/index.html","bddb8836b993461067150b8a5b172cde"],["/tags/Linux/page/2/index.html","8763aec33733463ea8921e5a1543bbf5"],["/tags/Linux/page/3/index.html","8156b3e293f592bb9f5f12af79a628ad"],["/tags/Mac/index.html","82aa5e0fb7d363f32e703519a2d9dccf"],["/tags/Mac/page/2/index.html","272097dfc4651ec25ea3b444d3371e20"],["/tags/Maven/index.html","a215df55b8dce7bcce5b141a95594af8"],["/tags/MySQL/index.html","33ae63ac2e4eb0f03456bf7aaa903e9f"],["/tags/Python/index.html","a1991d46dc63fd6b255b07435d6ba61b"],["/tags/Redis/index.html","e9d13673a1d125b0c3b8228428661339"],["/tags/R语言/index.html","2c4fb685c3ceb6110e2c322fd5b5ecad"],["/tags/Spark/index.html","7a06711fb2393158ae40a5cbd3b7567b"],["/tags/Ubuntu/index.html","70c20945103bb0232d3a69013a556deb"],["/tags/Vue/index.html","4afb8b2c95df46ea62b414158db48ae7"],["/tags/Windows/index.html","2e38aa4db8a36a36e98ff32fcebdd434"],["/tags/ZooKeeper/index.html","3c7826cc53fd4752deb29831c695af2b"],["/tags/bfs/index.html","3517e530c87f9cfdeee075744fd0514a"],["/tags/dfs/index.html","8e48cee46bdacbfcbf24f3ddd3080582"],["/tags/folium/index.html","01e07348a259a551a12347a0f7d38153"],["/tags/git/index.html","5fffd1e1b53457088e9bb40f9a16b5bf"],["/tags/index.html","abfe40f56d132189547df5a85fdfa5f6"],["/tags/latex/index.html","05e02331293245066d85b469a94b3edf"],["/tags/中间件/index.html","46233db6c2994fbc3174548d732784ee"],["/tags/二分查找/index.html","0a44e59dba7ee0b966fde0a477b8d49a"],["/tags/优化类/index.html","e28c0af1188559aa820e297350b81f31"],["/tags/前端/index.html","f5349c7f63aa7e6cd79e27cb3f37c961"],["/tags/前缀和与差分/index.html","1dc5077e7d6c869110f787675f41ab60"],["/tags/动态规划/index.html","6c094db72fffc0dd55ee209ac0c50484"],["/tags/动态规划/page/2/index.html","32149facc9142e62494b3e4be51290c7"],["/tags/博客搭建/index.html","d49adf8c8a440d25a38dcee283ccd67f"],["/tags/图论/index.html","6f93a0ce720020bb89f02f85e6794c1a"],["/tags/大数据/index.html","e43e40ca4e262f9cd0151035381ff05c"],["/tags/大数据/page/2/index.html","95e895cfefc57723ea7766cd5c67d611"],["/tags/操作系统/index.html","77119733f499db903e479af66f13a8f5"],["/tags/数学建模/index.html","cd0c8b442938cd9da9a9043da02cdb9c"],["/tags/数据库/index.html","6b36e891a8466401723e73df3efa5dc7"],["/tags/数据结构和算法/index.html","c517fb60fc106273966f530abbe33bfd"],["/tags/数据结构和算法/page/2/index.html","0efa09df9fa635acbe04bdafdd1fed6b"],["/tags/数据结构和算法/page/3/index.html","da5ca2a8c22662fe7b7c9ecc174cdc77"],["/tags/数组和字符串/index.html","041753e75547b9c9727c8a01003c6c35"],["/tags/枚举类/index.html","a51f8fbfc7c99ccde2da4a4352af1cc5"],["/tags/栈和队列/index.html","0b9c2518e8f76a937075ffae6ef6aa97"],["/tags/树论/index.html","a469e3a78627e0f7e8868b9dfa231d33"],["/tags/测试/index.html","40149ee19e78a5b17cce2885da951ffd"],["/tags/环境/index.html","e991c52696ec632dcc21187ab2f8b8e4"],["/tags/环境变量/index.html","729b26671b7de7b960b777edb978f1d3"],["/tags/绘图/index.html","77488948927f671e38fda7a747106c3f"],["/tags/编程工具/index.html","90543060b84800bfd93f1f90f4e88eec"],["/tags/编程环境/index.html","6e2cc4f0a3bc442cc0f3e411546bdf47"],["/tags/网络编程/index.html","edae585507b663d31be8de9a0c3f4a5c"],["/tags/英语语法/index.html","76dc6d64113053d43861d8674b01d550"],["/tags/论文/index.html","8a6bbd9a28998301614e4cb70dd08977"],["/tags/资源下载/index.html","bac276a05dc9f9fc10444d984297b635"],["/tags/链表/index.html","586f913ef8c152dd87b9dce94319c3d3"],["/tags/集合/index.html","7a98aba969e664e580b8a9d6298ad0f0"],["/tags/集群/index.html","bd7571ce55745cdd0d048e60e8e88d75"]];
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
