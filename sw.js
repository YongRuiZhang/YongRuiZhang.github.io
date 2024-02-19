/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","7dda455534387740d41752159a6a7266"],["/about/index.html","5cc07c1790c21e8aaa4aacb14ec0482e"],["/archives/2023/01/index.html","a3ebb36b698bfd62338dd2e3190c680e"],["/archives/2023/02/index.html","70ed890aa2a3f1746621553ebd0de961"],["/archives/2023/02/page/2/index.html","2ba5e64a42fb1664fb598afa06727d0d"],["/archives/2023/03/index.html","14e298000164cc76a4bc4aaaf8f1fb62"],["/archives/2023/05/index.html","cd7c3d47c29d358225bb33f49f38437b"],["/archives/2023/06/index.html","d16e612f5255fc2aa41315ca827d3055"],["/archives/2023/09/index.html","070264438f1a5cb4a354c2ae4dbbd0bc"],["/archives/2023/11/index.html","a2bb91ad1d3a76b6bbfb375ad06a5fc3"],["/archives/2023/12/index.html","55a56f4f881b68f5ee2740f3291f1911"],["/archives/2023/index.html","c785fed55ea206d12cf860e6f3b279b6"],["/archives/2023/page/2/index.html","10559cc46045f840c68ba7c8d93c1dd9"],["/archives/2023/page/3/index.html","cd6db720db8c2b1e90b5d8c49dba3122"],["/archives/2023/page/4/index.html","0daa933386c42b2e3a2e8feeedc9fb50"],["/archives/index.html","8769dde03dcffb61c71c607af98e6420"],["/archives/page/2/index.html","35664ec74fd52f07db22bc802ba525ab"],["/archives/page/3/index.html","6b428bdc79d9055c4949ce4c40bc4799"],["/archives/page/4/index.html","dd7da80375185efd70361d8cdbaa4fd0"],["/baidu_verify_codeva-qQP2iZOMLX.html","9dc04ab863d3b8e183a54d3ea25ba21f"],["/categories/Java/index.html","96906ad8b821564fe6bdba2ddba0ad53"],["/categories/Java/后端/index.html","d1f65c8ebc3eaca9a89317b635fbc09f"],["/categories/Java/基础/index.html","4fcb00d4c31e5cd5daf011bcae96da82"],["/categories/Java/基础/集合/index.html","d1ec1d0faa3f7b4c8640a6923c201b39"],["/categories/Python/index.html","46f726fe74fd9708b123fec6cdff0494"],["/categories/Python/编程环境/index.html","33e97f5505c175722c2f3e9e36bb430a"],["/categories/R语言/index.html","399afba863954d4deccd9d2b3cabec2a"],["/categories/R语言/编程环境/index.html","3ebf4b4c1423311c68f4bb73987c47ac"],["/categories/index.html","ca9a1dd26543e37bcf76efaf87cefa8d"],["/categories/中间件/index.html","4e5b8b6940d80d49cbee1249e2633dca"],["/categories/前端/Vue/index.html","99915240bba8cf28c1fa59fe4c148be9"],["/categories/前端/index.html","63ba345ea4b85fd38fba86fa9a6ed92e"],["/categories/大数据开发/ElasticSearch/index.html","589aa7fd5551b8695c0dd0feb636afb4"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","691df397ce438849fdcb148af24a527e"],["/categories/大数据开发/HBase/index.html","dd9e799ae7728b98beb08994bcbe1742"],["/categories/大数据开发/HBase/学习笔记/index.html","07d80fe3d0e9317fd4020d76f58b20d2"],["/categories/大数据开发/HBase/环境搭建/index.html","a5b1fc09a5fa836801327f3be6505f73"],["/categories/大数据开发/Hadoop/index.html","8d436fb0c2d677aab0ef1666d1c7352e"],["/categories/大数据开发/Hadoop/技术/index.html","1f365f33aded12eaa5689fb18ae7f755"],["/categories/大数据开发/Hadoop/环境搭建/index.html","cfda032325cf93ea59b835887e66a72c"],["/categories/大数据开发/Redis/index.html","b5e2fb1dbd95d59f761ec9adfa519df9"],["/categories/大数据开发/Redis/技术/index.html","124438938745549498c6091a56b67631"],["/categories/大数据开发/Redis/环境搭建/index.html","c71ff3c7d5788a102344e5ff51167c37"],["/categories/大数据开发/Spark/index.html","57512a69b0f93ba7729ddc969b6abb67"],["/categories/大数据开发/Spark/环境搭建/index.html","e77aa4ff6070101a2e524146ae35bb0c"],["/categories/大数据开发/Zookeeper/index.html","41d2abfc8d81b5ef58cd0bba00d14959"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","86eb72322fbcce6201e1d79ae196e882"],["/categories/大数据开发/index.html","f68480a40a0584d0f69d367afba79357"],["/categories/学校课程/index.html","a0e9d76f9b5d556d29d1863f9499209d"],["/categories/学校课程/计算机操作系统/index.html","4446d4fa05311afc4bb649b655c4ad43"],["/categories/操作系统/Linux/index.html","9eb3791c12cd7b679f69162761549c16"],["/categories/操作系统/Mac/index.html","dd88c24cbbfd398efc06a1702b53fd44"],["/categories/操作系统/Windows/index.html","3ba9ae06386f9ed49b92bc87ace4604e"],["/categories/操作系统/index.html","884e83309c1f023f31fa5e8867efcf84"],["/categories/数学建模/index.html","b23f7ebe0950c8f97077c47d6c10aae4"],["/categories/数学建模/latex/index.html","0021220129054660e3e61d8bef6f82b3"],["/categories/数学建模/优化类/index.html","9de0e0c999c8aae16a440a7849153f78"],["/categories/数学建模/优化类/现代优化算法/index.html","6c3ee2a2b55af819cf6d8a965ea68115"],["/categories/数学建模/优化类/规划类/index.html","f3fe82777199f12aba600303a249b572"],["/categories/数学建模/绘图/index.html","af21caaf85f63d259b8ff51aa7825613"],["/categories/数据库/MySQL/index.html","3af0f3b363926340de7495de51168e3c"],["/categories/数据库/index.html","14d6da1b59e4faa363878dfe38401945"],["/categories/数据结构和算法/index.html","174072d2c53d5cd6d2845bcb57d98c5e"],["/categories/数据结构和算法/page/2/index.html","f72af17fb7d6e4d68a26210f1f3fe7d2"],["/categories/数据结构和算法/基本原理/bfs/index.html","933a7d548370976f4344bdfb9e8e741d"],["/categories/数据结构和算法/基本原理/dfs/index.html","89912b5bd7dcf5cba3864c5914dc14ab"],["/categories/数据结构和算法/基本原理/index.html","a2a0f2d34382c26161928f2cbbebfb02"],["/categories/数据结构和算法/基本原理/动态规划/index.html","6eb3ddfdadbe393ebc6e2a99f51f10ca"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ee14ece07c798f71d92543a3c9c49088"],["/categories/数据结构和算法/基本原理/图论/index.html","86edc301dd4b67ba993f54b47fce9903"],["/categories/数据结构和算法/基本原理/字符串/index.html","adf69214016e398d390d11a3cb73f1a0"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","3e46f68d49201cff0f090d0c3cb3ba63"],["/categories/数据结构和算法/基本原理/数论/index.html","c80bf41c443fee176621cf7c96c4f6bb"],["/categories/数据结构和算法/基本原理/树论/index.html","2a288504e9a216575488173b132a2a72"],["/categories/数据结构和算法/基本原理/链表/index.html","7cc0c02115b1ea272e60851cc5b52938"],["/categories/数据结构和算法/算法题/index.html","f0af849d45d152f4d11b2b570e86c969"],["/categories/数据结构和算法/算法题/二分查找/index.html","f9c49623aa01019c4b5c7293d2621dba"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f1fb0108c1c59aa117d36e0291ab41ab"],["/categories/数据结构和算法/算法题/动态规划/index.html","3bc374e6757ead3cca2dad75dfdfd587"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","beb22d951fb942a4c7c116b47ab8a83a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","ac3ffc5641bbad3e279077e9dffd9e62"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","feec80d221965b46038be022faad4ecf"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","a50144e7653ee2408aa810611ecf237b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","1f6c935edb850362f6098090fa5c128d"],["/categories/数据结构和算法/算法题/树论/index.html","1a11c57eb5847901682b420704d3635a"],["/categories/杂七杂八/index.html","cb294abd392e03ed2158284646e0f62c"],["/categories/杂七杂八/博客搭建/index.html","6fa2ad33829f1a163a985de4ef4ab6c3"],["/categories/编程工具下载/index.html","bd9b14ccb5d5043a83671edfe5b9653a"],["/categories/编程环境/index.html","033e81043d9dbef0ec75967bcb7e845d"],["/categories/编程环境/大数据/index.html","0bfc2da7c56c5c82ef2c74a446b5b89a"],["/categories/英语学习/index.html","b40b74f44e48a13bba8e43910ac20bb1"],["/categories/英语学习/英语语法/index.html","9294392601d62ca0a5b92c1bdd816c47"],["/comments/index.html","4426a48bdc021abd8ba69d91fc6de2c8"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","617c1d83b01627ee3a572fa429df97bc"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","bc790443cca9f598a82517bf6ba7827b"],["/movies/index.html","9dea6ab3a0526f3b4ac2b99f6d089bb3"],["/music/index.html","83a939229a12f2b122fd97a164da9109"],["/page/2/index.html","6d4bb0b0e8b19d6ee53ea3c247483777"],["/page/3/index.html","984b2a87813eea66db53ae1029afcc1a"],["/page/4/index.html","50bca6a88a365557a6754b208d8ee81a"],["/page/5/index.html","062113bf7bde4b971baffc4f82798e5d"],["/page/6/index.html","46b801b106e2ca850a23e176a119e1e4"],["/posts/1021360842.html","0240b91eafc8a4884f5a038549000a56"],["/posts/1120620192.html","e68cb87030d466bd30b2a1c853e8fb3b"],["/posts/1141628095.html","9036d670e9c484026527fc06a925d1b4"],["/posts/1168613674.html","529fe3a76bb2ff1118d63ecc16156585"],["/posts/1219920510.html","b0709e34717c189a1c03046b80aed1a8"],["/posts/1222166338.html","dea2e14edc04f06b18b00aaeba4219cb"],["/posts/1259097482.html","a6c271704df45abef1f24d33f788eab6"],["/posts/1271036369.html","a870cc4bf6ff1671a8ea6490760040db"],["/posts/1312847445.html","1625549694e2bc58c6993fac9717c4db"],["/posts/135355774.html","ddb707f275536eda23e9789c86c4308a"],["/posts/1375344716.html","2d763e25ca5be6f5dbd2f09fd9f7ed7f"],["/posts/1388991698.html","23185baf5b50dde9bafe8b6e77348bee"],["/posts/1410315814.html","a96881e66c472228dc0422259edf2f8d"],["/posts/1452790229.html","c91d4e7e6a0c9662f9f226926dc7810d"],["/posts/1470079884.html","4dece0a8417c13ce4932c50449c58fd4"],["/posts/1470079885.html","9787d38b169c4e310b1db469ae22fdec"],["/posts/1470079886.html","ccb8329acd081fdc279f4163db838a72"],["/posts/1470079887.html","2acc25ace677c11e320fa87cdf345124"],["/posts/1498536549.html","2b3b93d6b2e5b7896b2f68eb25040e5d"],["/posts/1547067935.html","b055fc58c816cd7fe312e352d3bb8b46"],["/posts/1557866301.html","77c2f7f82c000b6e81fefeb8784cc7ef"],["/posts/1571776361.html","e3ae2a4e73d64f6ee2ca2a427f6bf296"],["/posts/1605124548.html","4ddb222bd61fdb1fabcdd386621e85c6"],["/posts/1633036852.html","906052f9c48e75b80aea7cceb21fc65a"],["/posts/1674202625.html","64eb6db76b06a3cbead3156c17492d1f"],["/posts/1765123828.html","9ac46632a8f56c9f80a24f965d686a60"],["/posts/1767336200.html","c7f1aa7547eda852f60f3b82909599a7"],["/posts/1776114197.html","8b89d8514d901e306488f10cc37cad2b"],["/posts/1817748743.html","f28ba14cccd4d5b61a8320c6ec9a54ba"],["/posts/1925125395.html","33e042b5e5ea42feec659f6c23f44abb"],["/posts/1966191251.html","50c48c662150b74ceff80ead9fc7287f"],["/posts/1987617322.html","780c8d069f44b09358e499e37196303c"],["/posts/1999788039.html","22e744c0ea3bc135332ec9e5ed79df7b"],["/posts/2075104059.html","70cd698d6289f0244258527a735e4e83"],["/posts/2087796737.html","c7b696974a5cedbc34b1eefabecf378f"],["/posts/2106547339.html","96f21392baafda07df3cfe301cc54740"],["/posts/2207806286.html","458cf341a8c5650a4592f6d57fc5e553"],["/posts/2225903441.html","7aaa9109823c3f7ce64e8757f15f30f5"],["/posts/2265610284.html","43445c3963bbbf31f284aded6d66fad8"],["/posts/2281352001.html","beb31c6143eeffd282cf9effb9fa696b"],["/posts/2364755265.html","b9554082039d0c9139a5340c13346848"],["/posts/2414116852.html","d29fa843a9b6ede8efa6e20a59846d0b"],["/posts/2421785022.html","cbbe6c7ece43388921fd2f93553d9402"],["/posts/2482902029.html","c291b08fbd823e7aa67d06621750b17d"],["/posts/2495386210.html","c10d8ff28337c044804e43d1ab194a07"],["/posts/2516528882.html","953a94f6930c5761489ea57bca38f16b"],["/posts/2526659543.html","c4051306e90d2fe89e99b43213c27bcd"],["/posts/2529807823.html","83d53f9cf32d954c4d71a644d04700a3"],["/posts/2596601004.html","d87a53b347520876796c0b3b27238bfa"],["/posts/2742438348.html","2f0edea814e01aa457cff2d31796c791"],["/posts/2864584994.html","317e62c38d57804d19463f3dd1a95a0a"],["/posts/2888309600.html","c4a45a45db305ee9dafe5b4f2bc46a0d"],["/posts/2891591958.html","9401eec74ab49c148c2928277bd6944c"],["/posts/2909934084.html","d08d24df3f09c48a633c935bdbcad78f"],["/posts/2920256992.html","3fbef879c872dc7f9fecab2337e3369c"],["/posts/2959474469.html","444ea02ef55716cf878850221e1207b6"],["/posts/3005926051.html","21a2e033cc9986f467dd81983672a2e4"],["/posts/309775400.html","0cd90e8f29f8c60cf5b5c4a6b0cf9254"],["/posts/3156194925.html","2b34639eea18a08f1e295a39a7e70fde"],["/posts/3169224211.html","0061e3d7c92340f14e0ce40656269d12"],["/posts/3213899550.html","fca1f0b5a56711cb8d65a97b615c3112"],["/posts/3259212833.html","17a4ef0afaddc0deeab7472f464ee3e0"],["/posts/3266130344.html","daad5477c9e43f5498ec2787b1ed5762"],["/posts/3292663995.html","155ffee423b57c260af1d01291efc0f0"],["/posts/3297135020.html","551ea25bce0c757bf0c48109d1d1f00f"],["/posts/3306641566.html","5931746da51574aba02b05a82f9d54f2"],["/posts/3312011324.html","a860f835379ffc7bc71bedda4436bb92"],["/posts/336911618.html","9868af40de3756c55a67fe6daf75cf12"],["/posts/3402121571.html","d8f34f3e40a62ec33c439c39f8eeb997"],["/posts/3405577485.html","5480585092a3b05017ee8f767b25a2e0"],["/posts/3498516849.html","fb5e9a439ff9be682c24c5f15707e4b3"],["/posts/3513711414.html","b6014bcd64fda1473eea3f6f38410ff2"],["/posts/3546711884.html","0a263746ef268513cea258dfddacafa9"],["/posts/3731385230.html","fd969c1ab7cbb4bdff5a0f9f7d6ecdf1"],["/posts/3772089482.html","1c9a7a0e1bc021ef0bedb7082566cb25"],["/posts/386609427.html","c1bd300ffdce7044705ee3e45033cdea"],["/posts/4044235327.html","6b370d2b6e49fc06eb8c51b1349ae4c4"],["/posts/4115971639.html","3bdac00d6febf5ef43f3f544eeeaf093"],["/posts/4130790367.html","725f404a035dc2dcc7a95bbc4f4ba0e2"],["/posts/4131986683.html","1e6707b8bbd32af091eb29a24f2e3e71"],["/posts/4177218757.html","246dce57a73a78135db3b8186141fc86"],["/posts/4192183953.html","e3591a28b18221518632868d443f33be"],["/posts/4261103898.html","90db3f07694657a2a98a0b389b004b5d"],["/posts/469711973.html","c1a89846ce7e3600d8fb68f2e67d6bf0"],["/posts/482495853.html","4ede1876a7388e59d39f1dafc37ba408"],["/posts/488247922.html","e55366f188a8e50e86376220014b5112"],["/posts/517302816.html","eab720efc3e48b502dc5c5f5f198d5d9"],["/posts/570165348.html","9f8e589538acaabbb44bb4fe771ed1f8"],["/posts/595890772.html","b788e674abc1be31a0ef1b2425331375"],["/posts/67485572.html","4bda3609c83d6275767517573a936948"],["/posts/694347442.html","2e0a3ff3923e0f89d6e9fe651525b95a"],["/posts/707384687.html","da0c25b16d480b65f6a088040da51927"],["/posts/71180092.html","b4b6e96712e6aaa492eac10de10ca0ed"],["/posts/716459272.html","3556e434a4dc007acb9f1cb93c20771d"],["/posts/765481613.html","a9d491333d847c8fb556073338fb54f8"],["/posts/778231993.html","1fb5fcff033339d7b46e820c706ff119"],["/posts/795397410.html","66f6335887261c1426d9b9ba834e24e8"],["/posts/820223701.html","fa49daa188dad122703a2130b0d5f7d3"],["/posts/830372185.html","cff20c9ae9676d33bd9434df1d6a2abd"],["/posts/88294277.html","7fb4223c22522a786f47fcb88c8c834a"],["/posts/939963535.html","15728dd8730523ae19cb09125cda658b"],["/posts/983786067.html","4c8ab111689d4024cb7f357c5abf2835"],["/sw-register.js","01c4a827f329b672eebdd36b7546bd42"],["/tags/C/index.html","c1300f9b3746e1acce2a7c469bce6203"],["/tags/C/page/2/index.html","393307d6687061f04412fe3e21d41a2a"],["/tags/C/page/3/index.html","9f9636181e84e58ee675712d95813e1b"],["/tags/ETL/index.html","7790c7468546f48bd2564122143e433c"],["/tags/ElasticSearch/index.html","6b677ff6775029930757e1c530441ab7"],["/tags/GUI/index.html","b615d159cbd63f20977474047d24a98d"],["/tags/HBase/index.html","38ec8d50903436b297e53971be70f0f3"],["/tags/Hadoop/index.html","c2479ff502005eaa5a59d9e02a7b7dd1"],["/tags/Hadoop/page/2/index.html","81e34e42e6ed50aec734265b21f445e2"],["/tags/Java/index.html","5f0689b8e1e2ccc73fc1828bcf3e02c6"],["/tags/Java后端/index.html","e71e81fc6d2cccfd824abd63038ec805"],["/tags/Java后端/page/2/index.html","0a0d84b064bb2e3c60ee758a2e7fc21d"],["/tags/Java基础/index.html","23b9c1b0278367c9e43828410708a3a0"],["/tags/Java基础/page/2/index.html","a5be56900d7ef50ade236d170d5aebbc"],["/tags/Kettle/index.html","cc36e61e54f580ebe80bf39a9a127c55"],["/tags/Kibana/index.html","d7b28fed000183bb1bafbc458b655350"],["/tags/Linux/index.html","6029c0ee8cd71d381c217e3a540d1b31"],["/tags/Linux/page/2/index.html","a2ff094d945e00c4e18fe3cf9b22d9e3"],["/tags/Linux/page/3/index.html","5c29c99575d2bc24bd8233343cfef76c"],["/tags/Mac/index.html","681a0376767dd6373c87bacbdfa9cf20"],["/tags/Mac/page/2/index.html","bd0f4768f79542f7e9e0e9ecf88e0a2b"],["/tags/Maven/index.html","caa8630cb47aa1a1d3cd0f2d90379187"],["/tags/MySQL/index.html","9167b8971b11b62851468496cbd1d34c"],["/tags/Python/index.html","46464ff2fdb81d43fdcde14aa0cae23d"],["/tags/Redis/index.html","2b6b6469627586a2ba05d477e2adca20"],["/tags/R语言/index.html","5aed512a14918899ca9c94d8b794d60a"],["/tags/Spark/index.html","9562d28957f56b61b77c1e49898d6f9a"],["/tags/Ubuntu/index.html","58f021dc063eaf3df025ece714ab844d"],["/tags/Vue/index.html","3656c365adc6cb28fbf3bfab169dd1e0"],["/tags/Windows/index.html","ada3a754dbc8c224e2bc8e8ea3d0e5fa"],["/tags/ZooKeeper/index.html","31282342c23a9944618c2f4cb3b8324d"],["/tags/bfs/index.html","491ec47b28011f10a6f4e9abc41352ab"],["/tags/dfs/index.html","f7bda67217a154da5bb0fbbff6482690"],["/tags/folium/index.html","a1a7b8129364bd8f563eccdd5340bd0f"],["/tags/git/index.html","20c3c5ce81412d9e96d2848dbd80548d"],["/tags/index.html","0018d978b72ed2dbbcc60daea68ce9e2"],["/tags/latex/index.html","217a62a0b4eb6590c3093d551a260900"],["/tags/中间件/index.html","6ab841d177f47ac941eb0e8259eb6709"],["/tags/二分查找/index.html","2e2a6e59c9f01bc82b8dfb987c920280"],["/tags/优化类/index.html","21f44c0dc678fc1d3fe2796286331da8"],["/tags/前端/index.html","e60762f59b1f9f26a7adbcd6ef5b1968"],["/tags/前缀和与差分/index.html","97ada46ad8a48d969f34983d2eb73eff"],["/tags/动态规划/index.html","d0d53ee8fa4e4832cc15234bb18ba484"],["/tags/动态规划/page/2/index.html","0061f6e14e65cfe70a20f0a07f1e4cf5"],["/tags/博客搭建/index.html","0a8a165a4c836fbe3c348f3e7c55721a"],["/tags/图论/index.html","0232057e036ce3a9a9383f9c3b321275"],["/tags/大数据/index.html","1a9748d82ef739f289969a9b0cbbaae9"],["/tags/大数据/page/2/index.html","f06b2fbe44e8cab70d999a718d3a8a16"],["/tags/操作系统/index.html","527c571f780a9393d1f26f5dd3a96f52"],["/tags/数学建模/index.html","4da41485f392ca35e087c03cecbefeb0"],["/tags/数据库/index.html","294d5658c0cff9c028a9532a0357f586"],["/tags/数据结构和算法/index.html","e4c7d2e213ff2f157bca43802e1e52cf"],["/tags/数据结构和算法/page/2/index.html","4bf588671042b5be74382fb0266e04c4"],["/tags/数据结构和算法/page/3/index.html","6d8941b53608981cc4dab1e90701c9b7"],["/tags/数组和字符串/index.html","b8383033bd7fbd3650cecf2e0c73320d"],["/tags/枚举类/index.html","f6d6258c9f662427501e6562afe6be3b"],["/tags/栈和队列/index.html","35959bcce3f595cf2c777af6a2912dde"],["/tags/树论/index.html","5388710a5e77c52d15280b9f2e46bef0"],["/tags/测试/index.html","81f53b1b1604efaf247ee3693fce7467"],["/tags/环境/index.html","e3822a2578bb87dbd7334cba75e51c9c"],["/tags/环境变量/index.html","f1b55fb30d0dab2b684fe292d98649ca"],["/tags/绘图/index.html","bc533c2e8bc34ac35fab92aa38052aa9"],["/tags/编程工具/index.html","8a15ea06d1ffd8a7251a8a2caad36910"],["/tags/编程环境/index.html","713fc682cbe16156e0aa302f891818d2"],["/tags/网络编程/index.html","13d17a8c16edb220318f914373db3cba"],["/tags/英语语法/index.html","556821929714ba6c5d8ebf1fdb488b19"],["/tags/计算机操作系统/index.html","c45def432655f8e8c734a346f6cdd076"],["/tags/论文/index.html","4ac467c6fad0ff3f736e7163fb413dc0"],["/tags/资源下载/index.html","3db7f23a19858eac43b974c4a95f787d"],["/tags/链表/index.html","47f752556021d8e0a5e2826ee8e2b634"],["/tags/集合/index.html","2bd60c407f1a71b89f4341af2c09d3e4"],["/tags/集群/index.html","d70e318f70b62fada9bbcbe7554688b0"]];
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
