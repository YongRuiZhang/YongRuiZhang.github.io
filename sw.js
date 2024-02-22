/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d4907c27b2581fc6dab26b0a4cc5d709"],["/about/index.html","1c389677cfce7cdffb8556ed7fcdf700"],["/archives/2023/01/index.html","dd83ae02ea420058703f26464eac0dd1"],["/archives/2023/02/index.html","fcd8dd27140cf4713a73713c36c5e2df"],["/archives/2023/02/page/2/index.html","c2b978997ad669ca5565037b42ede684"],["/archives/2023/03/index.html","22d0238c0dca4f4a71b74d0bcf303961"],["/archives/2023/05/index.html","ca9786f91a6029bcf973f02b255a507b"],["/archives/2023/06/index.html","bfce592e9f9b7fd954452693ec2ba0cd"],["/archives/2023/09/index.html","822c424d1ee51795678ee9341c292d53"],["/archives/2023/11/index.html","7b504e5742d5bf7c8da52f43ba7e1ab9"],["/archives/2023/12/index.html","cb22afa0efbb11d92bfad39385f1a4e9"],["/archives/2023/index.html","168c21344e13d39c9291e2a946bdabaa"],["/archives/2023/page/2/index.html","c6ed19412954d238406bef125559b078"],["/archives/2023/page/3/index.html","fd340bc77394361db605596581f1b790"],["/archives/2023/page/4/index.html","2bbd6ffb33a33efc8d51f84d675c84a5"],["/archives/2024/02/index.html","237885c31fe70be0800549f8fa60c71a"],["/archives/2024/index.html","5625c7c8cc1d6a83ecc75c6fc87a794d"],["/archives/index.html","2cfde810811bd962d938195dff224a1c"],["/archives/page/2/index.html","a4d114711a1c4486c78a1f8b58d87fb4"],["/archives/page/3/index.html","ad0ad468838c53c0194c082056c6f599"],["/archives/page/4/index.html","113e3e3794e7869d9e900d9bc3d77661"],["/baidu_verify_codeva-qQP2iZOMLX.html","35db5619ec857bd45661a2b50d2495de"],["/categories/Java/index.html","608765e603756bb2b4e4f1a50c1a7c84"],["/categories/Java/后端/index.html","85cb78f196ba9b0269146dc40bc9f5b9"],["/categories/Java/基础/index.html","a5c2f44797c55edd4b5daa8ecf32f094"],["/categories/Java/基础/集合/index.html","8bc4d34437bc1f4f169a682d350432d1"],["/categories/Python/index.html","ad6496647996c6cd7e792dcf5df60f1f"],["/categories/Python/编程环境/index.html","8e2414a3dc3c64f10f100b4e275a8065"],["/categories/R语言/index.html","267c11192ffbbaa2c5d1c045fcd3a07d"],["/categories/R语言/编程环境/index.html","a0bab99f35c0a8d2c07e1b08572cf078"],["/categories/index.html","2c720ca5c4ae521dbd21c0cc58b3b7c0"],["/categories/中间件/index.html","556f7b7040c4ab1d11f108d1dd6fd4c7"],["/categories/前端/Vue/index.html","ea33f6ec128ce4d331cd05c51803a69c"],["/categories/前端/index.html","9447f48905fc4acb2723c17258ac2733"],["/categories/大数据开发/ElasticSearch/index.html","52d297c9478bfbac2ca9e315f656a6af"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","1a78a476286d7cc73da6a2caeaa7cf8b"],["/categories/大数据开发/HBase/index.html","9d2c8cac237c0e2a01e977e269498de7"],["/categories/大数据开发/HBase/学习笔记/index.html","cd8be0a5526859e532c9b51cd474b16c"],["/categories/大数据开发/HBase/环境搭建/index.html","93feb29752cb621e6925ecaecb8d952b"],["/categories/大数据开发/Hadoop/index.html","4bffd1645649320aa6ba82a93b4692fa"],["/categories/大数据开发/Hadoop/技术/index.html","a9f187d37889f14cc54b74b1b07c905c"],["/categories/大数据开发/Hadoop/环境搭建/index.html","fecaf25e943d248ad71660434883ead5"],["/categories/大数据开发/Redis/index.html","c34fdaaa615e6e09c0402ada18ef0576"],["/categories/大数据开发/Redis/技术/index.html","2e882be8a48fe10d93bc180b5bf307fa"],["/categories/大数据开发/Redis/环境搭建/index.html","3301cadae537505cbdca8e95d70a1bb8"],["/categories/大数据开发/Spark/index.html","d7c707ad050474b5697d1c78877327f4"],["/categories/大数据开发/Spark/环境搭建/index.html","c59a01b0adf4e5444cc592ff7c0bccab"],["/categories/大数据开发/Zookeeper/index.html","42336ef70df3ea7b61000683c71e77c4"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","861ffde558399dea36bf4e3f70d18312"],["/categories/大数据开发/index.html","b3ea8008003ba407378fd3e73112d2c2"],["/categories/学校课程/index.html","20e9cb9cf955bd7603f68c812124a848"],["/categories/学校课程/计算机操作系统/index.html","5c75647b78478027c1c47074e05f300f"],["/categories/操作系统/Linux/index.html","4517cd912b7c3d41e25858db2963c3dc"],["/categories/操作系统/Mac/index.html","30181b8aedb88199ca2453ebd790e7a0"],["/categories/操作系统/Windows/index.html","3558b0bfe420807c61a4d4e77f2e6b27"],["/categories/操作系统/index.html","7565e781386608c36db65c7f8b12e5ac"],["/categories/数学建模/index.html","ebfd27f1c1db66a3e0bcfb2a32978c35"],["/categories/数学建模/latex/index.html","026fd1162df23c077168ce0e104226a1"],["/categories/数学建模/优化类/index.html","13afedccd832f8637bbd8ecb2e9de795"],["/categories/数学建模/优化类/现代优化算法/index.html","d974e698759da0012cada525052298d3"],["/categories/数学建模/优化类/规划类/index.html","8ede44e41c4e53b868afb5f824a8fe6b"],["/categories/数学建模/绘图/index.html","85a87da7310f05b67c48043b8eb0b307"],["/categories/数据库/MySQL/index.html","d321667a5c28be6777fd553f14fd914e"],["/categories/数据库/index.html","4fc0caf0750885e523f657ce70f5eaf4"],["/categories/数据结构和算法/index.html","8d4cb21a80d9ce2e74c82fb6d8948f16"],["/categories/数据结构和算法/page/2/index.html","51c559a050e5df0226ee59b3bd80b5e3"],["/categories/数据结构和算法/基本原理/bfs/index.html","d26b1045e8a19fac92e049340dd2da41"],["/categories/数据结构和算法/基本原理/dfs/index.html","058a7286e5add70eb5d39dbcce18312e"],["/categories/数据结构和算法/基本原理/index.html","299e8157e39676a59c70069dc18ddf35"],["/categories/数据结构和算法/基本原理/动态规划/index.html","8b5395cd76fcd85f3338fb878063c9a2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0268fa33191c1e7ca7bee126ad087c5b"],["/categories/数据结构和算法/基本原理/图论/index.html","9d72ce9e2cc381be6a0c4d935c6c38c9"],["/categories/数据结构和算法/基本原理/字符串/index.html","06d4f1e3cb85b72cf68c625c738958c4"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fee23ab58e219f87c30bc2c8dc767c54"],["/categories/数据结构和算法/基本原理/数论/index.html","c74460c3acd9e7156939c6380712b484"],["/categories/数据结构和算法/基本原理/树论/index.html","da2d4474cf2e1e0e05418c8a4dea3df6"],["/categories/数据结构和算法/基本原理/链表/index.html","9cac8854dc878aa6b60d24e0b2fde905"],["/categories/数据结构和算法/算法题/index.html","dc6a2de59df67e1e38498c6b3cc94da6"],["/categories/数据结构和算法/算法题/二分查找/index.html","4459a272773e7a6600797959aae2ce5f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4c669d2b7c28a450cca99b17f48e6773"],["/categories/数据结构和算法/算法题/动态规划/index.html","987e2cf70cf719d966457a4e05a27cd4"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","7fbd2cda33e82d2078459b9a0dee1b0d"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","8c3060b9aecb388f23b839ab9aabb502"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","7907514fd2fa46b97d6a075d2cd9e01c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","207d6d824631afdf8e154f29a2f9b264"],["/categories/数据结构和算法/算法题/数论/index.html","481084859e356dbeb83ac12497138ed9"],["/categories/数据结构和算法/算法题/栈和队列/index.html","425f78d7f3d901c307e719bea01f157a"],["/categories/数据结构和算法/算法题/树论/index.html","2a3e931866435e86f2d2fc94b305f83c"],["/categories/杂七杂八/index.html","95c2554e6745bc0020df1a73cc7c646d"],["/categories/杂七杂八/博客搭建/index.html","17f5c8671f481c176c3a22f0c008b6be"],["/categories/编程工具下载/index.html","3f55ae23684365f09f6e30f1013b5e01"],["/categories/编程环境/index.html","41ff97ac870d8ad0b764a1794c476468"],["/categories/编程环境/大数据/index.html","698d2c6ab9d1b4e919f370ad92d58af0"],["/categories/英语学习/index.html","0ae96e7ca8fed6893876f826c06b7703"],["/categories/英语学习/英语语法/index.html","53babb00c2ce0252e9138b6056071f4c"],["/comments/index.html","a732ed8b996c76cf17fbf9e5d6d759e0"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","9779bf68f8ecfdc53284e6b6b3b60721"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","03f74bef58711540d908f86d668164fc"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","e3bb50bd603de37c769dbbfed5e8ce80"],["/movies/index.html","79612346be6e4edecf20bba3f8344596"],["/music/index.html","539a5addbfaa516769262ad170c059a4"],["/page/2/index.html","27c7c650a0b05533abd89eced7d9f537"],["/page/3/index.html","031fd8bacab837c8d0bff1749ca9ff16"],["/page/4/index.html","3d1da7369ec2f5a2f7bc8bafb1d3f39d"],["/page/5/index.html","f48ae6493bfbb068c60662b1635fa885"],["/page/6/index.html","c8cc919734fb572b90832cb35898b45c"],["/posts/1021360842.html","75eeeef28487043308e2aa1e9f6b501e"],["/posts/1120620192.html","edfb0e93e609e8806a3a3d7f3d2cd9d0"],["/posts/1141628095.html","7987a7bd5a33f06784a2d024ca3a7160"],["/posts/1168613674.html","df1f9b2531c5315f5145b60f82072418"],["/posts/1219920510.html","4c64ad48eca61ffd6d6ce43977202f6b"],["/posts/1222166338.html","c534df3dc78d4ec753ab93a90be992dc"],["/posts/1259097482.html","63996706cac1aba4c2b9bca43d2a1402"],["/posts/1271036369.html","2c757cf828ed739be02c75aee0a4de0d"],["/posts/1312847445.html","039bcdc4999c8b5d96c54ff5b0075bf9"],["/posts/135355774.html","0292a4a3acc01d3ded31776b4fa6914d"],["/posts/1375344716.html","a88bc49e2036ecd99fa377d4302a8e99"],["/posts/1388991698.html","cfd39a310340ec40efb4422955327270"],["/posts/1410315814.html","5cbce05567a0cf278833887532988321"],["/posts/1452790229.html","2c020fbd8d000bfb8ead96bb0268fbc9"],["/posts/1470079884.html","bf80e2d19a87ffb9d1f2c7e8bd493456"],["/posts/1470079885.html","ce709e7cc8f47f1a58bd80cbbac95369"],["/posts/1470079886.html","90d491beae67f10fadb0fca3d7409cb7"],["/posts/1470079887.html","3b91db870bfd6e990bb5925883c069eb"],["/posts/1498536549.html","f2431ea1ac9532fa3d8280f9ef18ae37"],["/posts/1539568593.html","0cfe3f2751fe7ee0db2621c4db455ad1"],["/posts/1547067935.html","c11ccaa15f28d4d532b4dbce7d640c05"],["/posts/1557866301.html","7150b9ffc4120727cfe80b1579eda7c2"],["/posts/1571776361.html","696871f649e722abac8498649398ea0a"],["/posts/1605124548.html","0f4ef1b9802a5ae48632f1e590c627f8"],["/posts/1633036852.html","8087c34bea5cbfd31a0db0255b050d23"],["/posts/1674202625.html","088416138b4752593fdcff8b10c54fef"],["/posts/1765123828.html","29d8db86ce52a6923deac166e0018ea5"],["/posts/1767336200.html","f9a020e5d44fc99131ad0b0eed14144f"],["/posts/1776114197.html","e2c88fbd4dc3a44afa5d5ef177374eaa"],["/posts/1817748743.html","8a73a35f925dda4b34a2a5bbab1bff47"],["/posts/1925125395.html","4c8d543eb44fb070aaf35140b5789caf"],["/posts/1966191251.html","5bba522f8612c84f569682834fbec3ef"],["/posts/1987617322.html","49f604448781937ff52eaab2023b95e6"],["/posts/1999788039.html","7f405faf39c6f2999ea43bcb3ee2e3eb"],["/posts/2075104059.html","70a0cf5f91b54d794248571ba2566886"],["/posts/2087796737.html","616bbdfea3cc3f07047d023048a070d8"],["/posts/2106547339.html","5de0353f0b023f2bbc1757331ec6fae5"],["/posts/2207806286.html","094a29a79a3e9a0b61cfe1a710a3458e"],["/posts/2225903441.html","ac61ba0182e4287a380287a0969dc8d1"],["/posts/2265610284.html","3327e9789a9b4ac450e097032f904e9f"],["/posts/2281352001.html","7df4b9e280346d2e1d25de3617123b04"],["/posts/2364755265.html","1da750b6a33e966604e331c60d591c1e"],["/posts/2414116852.html","6ef4b8c8f50b2cb40805ed8284cc42bd"],["/posts/2421785022.html","19ce955a8cfe24d5d26e635c25a21091"],["/posts/2482902029.html","744e34f4854a1de2fbbd5865fdfdefab"],["/posts/2495386210.html","b8e5b6cf9116f66cc65f959e748a99ff"],["/posts/2516528882.html","83689797d8500cafeabd9ce89c1bcb34"],["/posts/2526659543.html","713c8583f1ef7fdb83dc7f92f4f08f7f"],["/posts/2529807823.html","4253b998f288ad5262c7f8c1b00915de"],["/posts/2596601004.html","a367dd72a39c612253c582d6d0a4cba6"],["/posts/2697614349.html","bc871315cf39b856a87796ec2c2013b4"],["/posts/2742438348.html","325d7dd59e852d2ad90de7867edc66ea"],["/posts/2768249503.html","dd46b9bf80f77cc31485ac53bd624ec7"],["/posts/2864584994.html","9b4d849bcd9a597bc2545a1b4a3dcae1"],["/posts/2888309600.html","99f36d6f8188d2db540ef41abbcd4f48"],["/posts/2891591958.html","0844f93a47042a7b71c6484eb3e2ef5d"],["/posts/2909934084.html","aec1a763be50edd9cf9cbddc4478a151"],["/posts/2920256992.html","0bbbe9bf14d67d04bc34a55b58008da0"],["/posts/2959474469.html","08b7001d5a4057d140377d4db05710a6"],["/posts/3005926051.html","154abe630f3c97cc1ececcab0922cb8e"],["/posts/309775400.html","b8e4717c9ce90dab3011247a67517b5d"],["/posts/3156194925.html","9e94e321eb2775dabc7241fc75937dbe"],["/posts/3169224211.html","60932f0f9f3dec8e33311097d1dbe243"],["/posts/3213899550.html","c019f80e537ed9baf4d17c27371db62c"],["/posts/3259212833.html","124c67eff355c4c9d86f31e5d5257186"],["/posts/3266130344.html","92fd04aa9072e5a3eb884eb4b15a73c0"],["/posts/3292663995.html","7e6e28f4677f4a5a4e65e58df3dbd212"],["/posts/3297135020.html","df128517b31b28f68cd47d6ba3c0d5b5"],["/posts/3306641566.html","d781f9cdc6021330fe3b190afd794706"],["/posts/3312011324.html","b21b89d317a57acd22dcb6f8768c5f9b"],["/posts/336911618.html","2ce8db6eda1952969fc3dc2999691663"],["/posts/3402121571.html","f9e709e4a2d435e48a4ba2f8923f0a9d"],["/posts/3405577485.html","ae5c745d5ccaa8795abb86b2f9568e4d"],["/posts/3498516849.html","ec67ab687c3304c4faaee46a341e7512"],["/posts/3513711414.html","22622b3e1cb26e50f36b85e5265bd5e9"],["/posts/3523095624.html","08d5f9c941a88acb73eff22da9ac2244"],["/posts/3546711884.html","d0c8ae9f32ea552da4939cbd3d369864"],["/posts/3731385230.html","d97cdb5adc65b62b1dd486b7ca67b3ec"],["/posts/3772089482.html","7529a5530bfc01f50add97c9004bc026"],["/posts/386609427.html","29fe9a76e3f40b11751f800cf3fcc414"],["/posts/4044235327.html","5982b246cd341423fcf94760f42acccc"],["/posts/4115971639.html","c7f847c1c4fec11661f3088a57e2b1f8"],["/posts/4130790367.html","4493c8f2f177bbfc1e739efbad55a608"],["/posts/4131986683.html","dc2291be905e427a6454d87e7c7e8c4f"],["/posts/4177218757.html","9fb774f3ec71fea5c2dc5fd312f9a7e7"],["/posts/4192183953.html","c49c2b8dec00ca651bc0b7dbfdbea6e5"],["/posts/4261103898.html","ffede8f24f503239ed8275d4b25077d0"],["/posts/469711973.html","adefd48cb070e82ca95becd15f6dc721"],["/posts/482495853.html","3a1e65b98d6083a6994a47514dc2b1c3"],["/posts/488247922.html","73dd17ee2a1e5679b12b5fcedc65dc50"],["/posts/517302816.html","96a564d72133ec2d9f74e8cf6e1a25f9"],["/posts/570165348.html","798270c33211a89b29bef2ee49925c7f"],["/posts/595890772.html","9940e77d5f2eacbe9f2eab6c6a4bf5bb"],["/posts/67485572.html","8f9d4f244ddd080892a61d2b8b6dcb89"],["/posts/694347442.html","172ef596e3b946769dbfeb654f6b3910"],["/posts/707384687.html","fe8661d8ce6108bdb7dea7583a9bfcc9"],["/posts/71180092.html","d76c3e097d6bf84613cb7a4cd960224f"],["/posts/716459272.html","fbf98259008e9f1e32b22f414d222097"],["/posts/765481613.html","28c629d8f992604b94b93c99809adfc8"],["/posts/778231993.html","dce813d9d4abd12d9979151c0956e3eb"],["/posts/795397410.html","ae75245bcd787de75431df4f96a848dd"],["/posts/820223701.html","168d618e34e7bb2c6f5d1d16d0e57209"],["/posts/830372185.html","cf8db822e670c6aa07c91c726d51af02"],["/posts/88294277.html","33448a41ee18680e9f8e8040fdb52de2"],["/posts/939963535.html","aab07e40afa5e64bcd28d1e106d711ad"],["/posts/983786067.html","47ade11ac04eeaa6d46ac266e006a3c2"],["/sw-register.js","f8322268a591f5740a531998b86bb266"],["/tags/C/index.html","82db81bdd1efd0592ca79fe5c57c4acc"],["/tags/C/page/2/index.html","2fa5491fa6b6b571df834a17c3dee6dd"],["/tags/C/page/3/index.html","7508067fb2429705d174df811d6fd6f9"],["/tags/C/page/4/index.html","29e82716779d9ecb2faa93410ac07765"],["/tags/ETL/index.html","3f1c59eddd1b62dc75d40931e157142a"],["/tags/ElasticSearch/index.html","adc2374e1bd44077a3b60fb0cbcd0997"],["/tags/GUI/index.html","066259ea40db629eb6cbe34b997df7df"],["/tags/HBase/index.html","ce2ccda8ae0ad86a1af29f4d87f9718e"],["/tags/Hadoop/index.html","5bbe2ad081880e3feeca50f022b8a9e0"],["/tags/Hadoop/page/2/index.html","44d0d674c475f53ef9e5b6b615f02e3b"],["/tags/Java/index.html","c92b3fbc220470aaf753622f76beaed2"],["/tags/Java后端/index.html","0f8ba0a12b1663828527dd9752a282d4"],["/tags/Java后端/page/2/index.html","928fbd13d624843fae20a55a8f4e72da"],["/tags/Java基础/index.html","501921bc6d213c3a9bec350091e395dc"],["/tags/Java基础/page/2/index.html","db74cb4fcd0d804a8f5fa0febae1331b"],["/tags/Kettle/index.html","a5a87d07dad4ac2f61e2ac88222046f4"],["/tags/Kibana/index.html","ea598a52b362b6c794b933addae15020"],["/tags/Linux/index.html","e54498cb5453c4612596ef204181f6c5"],["/tags/Linux/page/2/index.html","00e0451d36c56335de66065717e6484c"],["/tags/Linux/page/3/index.html","0efa4296ecaca206753f9d3f1a62215d"],["/tags/Mac/index.html","307a85ec0d438ef7e7411ecd6336064d"],["/tags/Mac/page/2/index.html","f8bd820ea31a2ee8708cd20938f8a8fb"],["/tags/Maven/index.html","ca97af1e3de99c9ebf8a04341f819b0c"],["/tags/MySQL/index.html","87a8a37cfeff67b82839d193d060d34e"],["/tags/Python/index.html","b93a91bc0c6efcc4840a74cc9fc53d46"],["/tags/Redis/index.html","94374b06ee7986c5a43f8040fd7b258a"],["/tags/R语言/index.html","35581fd07e07ac15e4a953154a819d23"],["/tags/Spark/index.html","6834122825b3dfb0b94ff74284d49e58"],["/tags/Ubuntu/index.html","06e67133569f6fd05ff47b733787675d"],["/tags/Vue/index.html","6eba248945a0ed432b6f37526a20c923"],["/tags/Windows/index.html","cb672507e11fa445f64b419f125a7dd2"],["/tags/ZooKeeper/index.html","feaa56df9f72794d173cfde0e71c6182"],["/tags/bfs/index.html","518d40be2c409b1125b92df2c9528cfa"],["/tags/dfs/index.html","d30a2d949e973e9c558c6593cdcb326a"],["/tags/folium/index.html","19e89bfcc08057a4c6c91b1718baa395"],["/tags/git/index.html","4b4875fd34b4f4da6fca18e9a6fc3f57"],["/tags/index.html","dddf7d338be13495924c0618b9fc578d"],["/tags/latex/index.html","b905ad1c00bc7b004f8a07135efba896"],["/tags/中间件/index.html","f49debda82f5a4390562a65614d30db9"],["/tags/二分查找/index.html","ec4b291b0f96acc085794f846cdde720"],["/tags/优化类/index.html","dd8863a75b53dc24fdb7075f7523b0e3"],["/tags/前端/index.html","ed7a53d4d637a3609aeedf6f33633dc1"],["/tags/前缀和与差分/index.html","0a56f6099b552c7d7ff8466b1db0edba"],["/tags/动态规划/index.html","647039d2c4fa3073b47871a9dcd34b19"],["/tags/动态规划/page/2/index.html","cff9969815e5be1aa53a3d76a86145c6"],["/tags/博客搭建/index.html","4ed33d334bb59d1dbea6b934b68ea480"],["/tags/图论/index.html","02e934b799e2e5e95f3d97348e63cdf9"],["/tags/大数据/index.html","2c396c2d380a73838a29acd58fb1aac8"],["/tags/大数据/page/2/index.html","4df7cbfbd959b5911df3e8e573a0df02"],["/tags/操作系统/index.html","0e107ce6fd14311788818db42b40a4a7"],["/tags/数学建模/index.html","44457b4cc24098339f9681068c7e6d2d"],["/tags/数据库/index.html","b85e152be2badfc826231d2cb3dd9901"],["/tags/数据结构和算法/index.html","22fe67f649f6902872d77c401c2ff332"],["/tags/数据结构和算法/page/2/index.html","1f1580b19e20d616541dbaf4ef662dcf"],["/tags/数据结构和算法/page/3/index.html","ad60ef65311dea02dcdedcb0f5bf879c"],["/tags/数据结构和算法/page/4/index.html","4e8fb69424ad873fd9b5e616f2f2ba57"],["/tags/数组和字符串/index.html","18ca3e5ed3baaa5d791aa27a52d381df"],["/tags/数论/index.html","268c75282f2e5be08661df109298e4f0"],["/tags/枚举类/index.html","2ee218493b611127e08c56551057cd50"],["/tags/栈和队列/index.html","87e6b5e3cc5203bb87f78dc0849c8e00"],["/tags/树论/index.html","bc6f31582bd606a21080d6fd48b13cac"],["/tags/测试/index.html","b9a0cf3505e825b956beb8e25b770f20"],["/tags/环境/index.html","47a8b38580cf3fc46c48e924fe1f31d5"],["/tags/环境变量/index.html","659b401140f998a9a36cfc471c97ef97"],["/tags/绘图/index.html","b563010928db635cbb65d4295b730629"],["/tags/编程工具/index.html","aa3a9d5baac2d8df7e36a53e823cb61c"],["/tags/编程环境/index.html","1b0e35f9cd4f83c86357a7ff89a07608"],["/tags/网络编程/index.html","a309b4e5e5bfae2fb60a1ef09cd22eaa"],["/tags/英语语法/index.html","bcfdd64106423a24d8d6ed460169a117"],["/tags/计算机操作系统/index.html","ce96878b3c258cdd6029c99064e63409"],["/tags/论文/index.html","eb21c390496da5144e001a04eb6887d7"],["/tags/资源下载/index.html","b0cee6d8b016becf36f1417eb307a0eb"],["/tags/链表/index.html","27387dc18c81ad656aab9d35f36c599b"],["/tags/集合/index.html","b0887bbe0911dfd6b28e1e55464a0673"],["/tags/集群/index.html","808f108436a5eacc0c208d8fa3df8c83"]];
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
