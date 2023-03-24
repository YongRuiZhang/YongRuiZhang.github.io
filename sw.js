/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","34b477c8f6876095d1b14e2af048fcc0"],["/about/index.html","27839ff7b0eeb906e6102fab2a1cf8ef"],["/archives/2023/01/index.html","939a66a109ed7552afd9737598c2737d"],["/archives/2023/02/index.html","9aec591b10cdd79e0549741a29a383b3"],["/archives/2023/02/page/2/index.html","e3cbbf0f3e499e00e03f58d072cb6267"],["/archives/2023/03/index.html","452d8fd4ab5df1bde1af3c74f1849145"],["/archives/2023/index.html","9dc1be488e3df2da18a419eac81e31d1"],["/archives/2023/page/2/index.html","de2a1d4d1cc648a0f3a2118b97d5f730"],["/archives/2023/page/3/index.html","959075b6211da7002aec70873606e3e1"],["/archives/2023/page/4/index.html","3fac9ea153cded13bd8cdeaaf548f21c"],["/archives/index.html","a3f9e5b5c6e3d2e2bc51a164a72ead5b"],["/archives/page/2/index.html","5228c7b050c143b6ce5f555af2f5a337"],["/archives/page/3/index.html","33dd074c2539459684ecd9d6bf8b49f4"],["/archives/page/4/index.html","b79fcff4fdc68d014b8175f423cd8b9a"],["/categories/Java/index.html","3145fd2a4a51fe5e7f0f36799e461415"],["/categories/Java/后端/index.html","7d60dfb43b59c6b40eb708428f5b928e"],["/categories/Java/基础/index.html","8d1e2b9c65c0469e2ffd6fd8957fb2f5"],["/categories/Java/基础/集合/index.html","0d83210f6c1dad54f5e2aa2a61d607b3"],["/categories/Python/index.html","d1eb8475c345d7033582dc3460c5748d"],["/categories/Python/编程环境/index.html","9c028cb0c6d22512530d5d655e15fe1f"],["/categories/R语言/index.html","68b876e920e1774daccbe9c0d156a14b"],["/categories/R语言/编程环境/index.html","baa1d9f8c621e5bd510d63c2ccbf4151"],["/categories/index.html","15ab01e14d52423b69891ca4074c55f0"],["/categories/大数据开发/ElasticSearch/index.html","0e100e4fd2248d27da82b068c3ec1700"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","96838efb56f068ca1b133640a415a012"],["/categories/大数据开发/HBase/index.html","a253979d665bb3963c0e359ea6c3c363"],["/categories/大数据开发/HBase/环境搭建/index.html","e9f5aea03e16a37f54b973835b62c713"],["/categories/大数据开发/Hadoop/index.html","f751560440661cc4d5476902f6920cbd"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1302902b927a6fd1e69c3e6ef85f3280"],["/categories/大数据开发/Zookeeper/index.html","8de29d2fa0eee5a73a6118a82f82ac4b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ea0ac8ab26c3eba7892f5813fc42efcc"],["/categories/大数据开发/index.html","8831994d5e6595989083cfbcda68e915"],["/categories/操作系统/Linux/index.html","033da16cf554cf8a4fbdc5e9b4d38f1e"],["/categories/操作系统/Mac/index.html","20ea7a47b7c8199d1c6b4900a706229c"],["/categories/操作系统/Windows/index.html","ac231d5a83b3608207de6a83b48c9b11"],["/categories/操作系统/index.html","e35e97158dcff0a0be9b3222b320bf14"],["/categories/数学建模/index.html","b23bc0ac6a59b783bb3b8e7e74881481"],["/categories/数学建模/latex/index.html","551eac70a729dc54b4706dce7630f5e1"],["/categories/数学建模/优化类/index.html","b276a7557e5ad046db08ff1631698368"],["/categories/数学建模/优化类/现代优化算法/index.html","5d6fde0172c9975f0ba01d169f382c88"],["/categories/数学建模/优化类/规划类/index.html","53f978024f8bbc1effbfc5a91814742c"],["/categories/数学建模/绘图/index.html","d928c5573ede951ec55a42b796439525"],["/categories/数据库/MySQL/index.html","b331cf19990d62fcc50b40b8e513337a"],["/categories/数据库/index.html","dbbd3859f076a2392ddfe2185e9a773f"],["/categories/数据结构和算法/index.html","59490567d9bf07236481e2a8352e4cbc"],["/categories/数据结构和算法/page/2/index.html","a990ad93184401c2823682f7a0ce3799"],["/categories/数据结构和算法/基本原理/bfs/index.html","d94e7599e4b667240c06b02d075f8a99"],["/categories/数据结构和算法/基本原理/dfs/index.html","7d533d5a5a309e051e7d5e9255ef107f"],["/categories/数据结构和算法/基本原理/index.html","aa9591dffa2be99f723d0def3fd6be6a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f051a0a6158b82323270efdf609b170d"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","7e6c62004f3e83f90ef980c098f09a87"],["/categories/数据结构和算法/基本原理/图论/index.html","f5dac5fcb056f8d73dbe7c508c1909a5"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6ee9fcd91eb86c8f45a062bce9852b3c"],["/categories/数据结构和算法/基本原理/数论/index.html","32cce94078c4b03142d1cb8a4aeabae8"],["/categories/数据结构和算法/基本原理/树论/index.html","14b132ba586ae065c1d9b2c1df2a0a7c"],["/categories/数据结构和算法/基本原理/链表/index.html","1be0e0668860b8911f2c8a001bf59223"],["/categories/数据结构和算法/算法题/index.html","d4f383143441569050d67e5c36062396"],["/categories/数据结构和算法/算法题/二分查找/index.html","d254ccc5090fa0046435710d85d27206"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","b91f8e8055f2f84274968b23f32942cb"],["/categories/数据结构和算法/算法题/动态规划/index.html","52f81b3d66e30ca3d9ed944492a54810"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e67cc3332065f329339a6a617fdd23a8"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","6a33b21dd10ad95f82a7b9f97843c151"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c6350b4f31fe7edab33537f5bc14c634"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","fff92260e32884c90176898cc1217526"],["/categories/数据结构和算法/算法题/栈和队列/index.html","09f82a4e2fe88ea7cd6844ab595063ee"],["/categories/数据结构和算法/算法题/树论/index.html","7d23b7732fb6355751ad357c12f7831c"],["/categories/杂七杂八/index.html","2e377efd38b9c7b0e8f96d61087cfdd4"],["/categories/杂七杂八/博客搭建/index.html","3fac9d7de37f8b5bba4bea211617f164"],["/categories/编程环境/index.html","a8bd64169d143b5555c6d44fc0af59a2"],["/categories/英语学习/index.html","92dae8a3119666680b43574e073f2b07"],["/categories/英语学习/英语语法/index.html","d60313cca92edf2fa384a2a6ddfcc2ca"],["/comments/index.html","77418ce7ac3414210b6b46b5bd4f6b29"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e8da56bab26c5bdd916fbce51b33269e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","149c04a9fd6462f8b7239822dac2ac01"],["/movies/index.html","550063f6e0649090fa577da80b3ef42f"],["/music/index.html","cb250c0f8577f220dfbb28af0cf9d2ae"],["/page/2/index.html","7f5d57ed5589771801fc20c140947da1"],["/page/3/index.html","95fcacff73986747aee1f67858adc640"],["/page/4/index.html","0817399ccdf590ff1ec7deb327c7cde1"],["/page/5/index.html","b3972c4d9b88505576ce5b869a670785"],["/posts/1021360842.html","22b3c5696375e460664d06d60afc2d3f"],["/posts/1120620192.html","613afcc7c58be36097dc9cca2a131e46"],["/posts/1141628095.html","3f4f8e5786ca6173034fe0027b7dbe68"],["/posts/1168613674.html","80ffdfb8cd0f6aa14ffb83ada36298c4"],["/posts/1219920510.html","5ef21704209a602fbba0aa46e62b2510"],["/posts/1222166338.html","3ddd2db30aee0305250e551688539b39"],["/posts/1259097482.html","32aedadea1e79e43eb797330433acb19"],["/posts/1271036369.html","ad4c21139af078a6b8056e7197ac7d95"],["/posts/1312847445.html","887942dc9afcc2e87ac3229d7ab2ec68"],["/posts/135355774.html","9a030617adb8cb37d1e20e40ecbe560e"],["/posts/1375344716.html","9651b9257d26b24e37c65b691b74e592"],["/posts/1388991698.html","836bbedce476fc1033ff9f52f5c89446"],["/posts/1410315814.html","f8a183768101d50744b81d3317901f6e"],["/posts/1452790229.html","344c1aba1afe9031b701fc1511d4483f"],["/posts/1470079884.html","7750da12f089635257734f9d06060e2f"],["/posts/1470079885.html","58b33e4a21e1b9410cc1626921c04c84"],["/posts/1470079886.html","d725510631fb0b094e8efb6f330f341e"],["/posts/1470079887.html","503cfa576ccbd698bab01d14cef5a577"],["/posts/1498536549.html","916addbff2994e6e1db42d6281dcb6d6"],["/posts/1557866301.html","6cc5b646021a8db7b533209946ff946a"],["/posts/1571776361.html","cf71f5569aece0097f0494fbf7e69bc6"],["/posts/1605124548.html","dc782e88d1f40fba094514d461a37922"],["/posts/1633036852.html","a2df87be892df7259c66721cf1ac1487"],["/posts/1765123828.html","fd1c7c69e3c2ade0426223dbeeb6675c"],["/posts/1776114197.html","7c9095ac8c84fba24a65c79d5d07bfb8"],["/posts/1817748743.html","a1b3d64b110d72d596cdae192085346f"],["/posts/1925125395.html","ff7bf5e47f0b7b182f8a1ef274458d64"],["/posts/1966191251.html","962fa0374639112e8145ca56fcd7cfe2"],["/posts/1987617322.html","86ad453e16f24fc648668b6bec8b8aec"],["/posts/1999788039.html","1ed1fdbde0aaae69e0cbcd596a7cde3a"],["/posts/2075104059.html","fae76688ee5ff731302e221812881b29"],["/posts/2087796737.html","c69a930adf6fa7415c1281d1c72ba816"],["/posts/2207806286.html","0dfe5eb1f9d63374f6906e11d6dc05ef"],["/posts/2225903441.html","79c43ed21e95745d60e94e564cbe9c7e"],["/posts/2265610284.html","7fa4c84249ee9b48aa71d6f95dde0d08"],["/posts/2281352001.html","3211b126a42458e6c46e37b78ccb3565"],["/posts/2364755265.html","b085d90cc1e6cfd20da9eb85c118a3bc"],["/posts/2414116852.html","e9772ab63eb8b6b815c2dc92c91b6af8"],["/posts/2482902029.html","e42cd6565d37c547e32edc90c6e92a7c"],["/posts/2495386210.html","e04cac71fa59eaeb8a4dd6bf7f042a6a"],["/posts/2516528882.html","b5b33bc7ddbf8268dde55e769a57d99c"],["/posts/2526659543.html","349ad740d8e80f5b4877d11c0f36b8b4"],["/posts/2529807823.html","0f9a499f44c545e61092a1966edf06d0"],["/posts/2742438348.html","34504763b7135593dd2cd86721086125"],["/posts/2888309600.html","00226a78268c9d7ebd4212bc0fbe4c92"],["/posts/2891591958.html","82841405c334214bf9cec98bacf51c52"],["/posts/2909934084.html","ccbb5ae24a9d76f8637e07163e9f136f"],["/posts/2920256992.html","8186481d3b0c8baa7e8b5369305842b7"],["/posts/3005926051.html","16c1854ba565191bcaf3ee56bd972589"],["/posts/3169224211.html","0a55d2ceac560cd6ad53bb364c66b46b"],["/posts/3259212833.html","7a3af76d1ce0ebb3df1aa004bf9e4f02"],["/posts/3266130344.html","7b2300110748447c994d8bc66ff1ee51"],["/posts/3306641566.html","2d713d90098c73a283399468d1062102"],["/posts/3312011324.html","b78e561fa8915613c75caa5b8fc874d5"],["/posts/336911618.html","ec9bc6b1fb8b99fd506f37aaf268eaff"],["/posts/3402121571.html","c07c601900b711c593ed0f59b774b825"],["/posts/3405577485.html","068b7cb09785193f2f4d7c9ac40717e8"],["/posts/3498516849.html","05ee44464a74359a0118634365e856ff"],["/posts/3513711414.html","d7019afb04333b1db237b84db58a90bd"],["/posts/3546711884.html","8e89378b2b1991978ef68c65081c3910"],["/posts/3731385230.html","7160d0ab06c40bee99342b8e2a2305d4"],["/posts/3772089482.html","76dc5efbb63028b74149d7e375ccd2cc"],["/posts/386609427.html","a04286ab43a1bb664e9ca5141272be34"],["/posts/4044235327.html","68b461715eb569091d0a65c3e621d17e"],["/posts/4115971639.html","91fd226b204daa53ca1643fe0c51ac32"],["/posts/4130790367.html","8e632cc2ae2cad2845faa5602ebf5958"],["/posts/4131986683.html","ea7073093fe497482ddc13022984dda3"],["/posts/4177218757.html","82e149978a0ea514933936d18c22cc96"],["/posts/4192183953.html","e5fa7f02af6212464d412c13f6aa48d8"],["/posts/4261103898.html","641060fb0d9e517de75d50fb37e362f3"],["/posts/482495853.html","dce9b74bd553dad99b1fa42532f80f74"],["/posts/488247922.html","128f63b794a744830ea47ef609c2da0f"],["/posts/570165348.html","01758d713818cccb13f5c8dea4389e40"],["/posts/595890772.html","71d281288580eef3341b15963f85f86a"],["/posts/694347442.html","555bfd9409b8820c74f18d93900c014d"],["/posts/707384687.html","bc3c81b7b7d7a818b3f92f16c32f7340"],["/posts/71180092.html","b5393fe9421e9b410bcc4b399be60425"],["/posts/716459272.html","7f895a06ff3ae6da3f0f1942e9901be2"],["/posts/795397410.html","da013a1250f75d5d173d111efbb29281"],["/posts/820223701.html","017c1931044ebbac728bdc39416ce16e"],["/posts/830372185.html","e8bb9af79dd3647325d7195558d134cb"],["/posts/88294277.html","202bc8c7374bc745d667e56206445b8a"],["/posts/939963535.html","06f46880ea35486deb7b1f3dd7ab9dd2"],["/posts/983786067.html","811a6cd9c775ab2a75a1971b380a2c51"],["/sw-register.js","5efc799b4584d264f620f1aaccf36089"],["/tags/C/index.html","039642c5711496ad8ec61f61d593a030"],["/tags/C/page/2/index.html","2b1c6303db50aaaf21f8041fe967067d"],["/tags/C/page/3/index.html","62915d31069c9b9537639d54d999ed7c"],["/tags/ElasticSearch/index.html","e800d0934b6c0e69473718fc70f40d71"],["/tags/GUI/index.html","5d7052114dcf2f3248cedd06df9f2ab8"],["/tags/HBase/index.html","fc056eb62fd22bad6437b996cb4faf4b"],["/tags/Hadoop/index.html","85d046d72c06ffde4bfe2ebb8e74c851"],["/tags/Java/index.html","756315c3e1b151bbd18f596afa6fdbd6"],["/tags/Java后端/index.html","2e7f39af3fc7ace162f97a59e963b1a7"],["/tags/Java基础/index.html","79bc1bf49479e3a2e935d66ce5290095"],["/tags/Java基础/page/2/index.html","ba83f17c8faf30e75dc30b6e09a6a670"],["/tags/Kibana/index.html","e75d9ed43a591868d7605a469e461d11"],["/tags/Linux/index.html","55215cd9e6623f7d3b285002ce31561d"],["/tags/Linux/page/2/index.html","4a845f374928561f50e5d3715ad32b42"],["/tags/Mac/index.html","728676b63474d5e03c8ed0e5648c883d"],["/tags/Mac/page/2/index.html","e2e3d91137e17ba3f2da6dc01b4dbe74"],["/tags/Maven/index.html","3757e55f009984f94a9a641b28c3dd7d"],["/tags/MySQL/index.html","ba6e63bdd2854b603b1708d36d734285"],["/tags/Python/index.html","2e40b403ac965e1a656fa045d607714f"],["/tags/R语言/index.html","d69cfcfc552af4f611413ff4ae122c2c"],["/tags/Ubuntu/index.html","eb5bad7982738238847a601cf1885630"],["/tags/Windows/index.html","c0cce3beba20cb45d2ad1180c1f5d14d"],["/tags/ZooKeeper/index.html","ca6db23797d2fc2cc0a45d9d424809da"],["/tags/bfs/index.html","77acbfc077dbb726c7c22f15c0090dd9"],["/tags/dfs/index.html","c6728c5903a443fe4c08cc2484ec300b"],["/tags/folium/index.html","f1fa0b576d215f62301b3b2a4ff5945b"],["/tags/git/index.html","35b3d68311cbd38d3e22821718397ef5"],["/tags/index.html","8df45474df054f8cc351a54891de0e63"],["/tags/latex/index.html","f9e7c5774a0c11eb2638de5cf3bffeb1"],["/tags/二分查找/index.html","9699af21d7c4540c64b25d2a124b7c91"],["/tags/优化类/index.html","d3657ae485c17d64f3d9787aaebde05e"],["/tags/前缀和与差分/index.html","5855ae0e78e508ae3e38af326a99822f"],["/tags/动态规划/index.html","8fd94e12eaa9a9e92711bd2241729fc5"],["/tags/动态规划/page/2/index.html","1371cac99d1104497e22ac328efac11b"],["/tags/博客搭建/index.html","cf28156855ea82529fe882d5c3b6e82a"],["/tags/图论/index.html","deabc3004afbcb4a36513db9c97c202f"],["/tags/大数据/index.html","d1e6b083bfbe0a621f3f7b1d516e4e3e"],["/tags/大数据/page/2/index.html","fc698dfe5b68c074c50875e29c327641"],["/tags/操作系统/index.html","d63b9462e94776b720108f6fc567088b"],["/tags/数学建模/index.html","d3c96876cdab6459dc1373aab3cd8157"],["/tags/数据库/index.html","67278cf4e11229b3de35734f63ede65f"],["/tags/数据结构和算法/index.html","af84f3734169d32a94410ffd7b4232f7"],["/tags/数据结构和算法/page/2/index.html","63e8b74fe47a935a14d55430226302d0"],["/tags/数据结构和算法/page/3/index.html","34e4d8252e496decfdc3e575c2af93da"],["/tags/数组和字符串/index.html","6e35704aff6c250990a9520b1cf7c0db"],["/tags/枚举类/index.html","0d510419576c5498654e84817d170620"],["/tags/栈和队列/index.html","e69e35eec6c8279b3bb2d645cca35248"],["/tags/树论/index.html","8ededaa897daf3bd4ff78e13eda78f86"],["/tags/测试/index.html","4ffba2ed48b6a3a360741227363bdef0"],["/tags/环境/index.html","1fa9bf398396295eda0cbe779f96f7ad"],["/tags/环境变量/index.html","a00dd810f77452d0362f007e9da2f881"],["/tags/绘图/index.html","ce2699baf5859a0fd150162ad76f046a"],["/tags/编程环境/index.html","5f294841ea9ad37b725fa420386bb9d5"],["/tags/网络编程/index.html","2e11257d11a13ce8b4c4fe53bf24de90"],["/tags/英语语法/index.html","c06fce6a320c3de6d18fc1c25589358e"],["/tags/论文/index.html","591ca216b27caf43385cb90e5fc65bdd"],["/tags/资源下载/index.html","5e6cc0bc4be75fe932fe704ec7e620c2"],["/tags/链表/index.html","1de50a016247378315b12ed34f9dacaa"],["/tags/集合/index.html","a3777d2a5b4ebf9695dd6110cda64cbe"],["/tags/集群/index.html","a8752b5f0a2e070f6f7045c5d429765a"]];
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
