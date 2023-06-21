/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","7659a251f81fc6715468aa53a6d9fda1"],["/about/index.html","b662cb5b6c315aee4a49859b27a46ff7"],["/archives/2023/01/index.html","8cca98ac3ec3733be3312ff09f4d91b0"],["/archives/2023/02/index.html","b53503f6059abd7cb5865e7f6dc9f219"],["/archives/2023/02/page/2/index.html","3f45bcc4513e2f5fcb7c18d55d1577bf"],["/archives/2023/03/index.html","3008bba644dee6254749caba109fffcf"],["/archives/2023/05/index.html","833f32de17242516f6486cf4180b2f48"],["/archives/2023/06/index.html","09abbbd77cc8a34c2e422fad61baaa5e"],["/archives/2023/index.html","327e37c79c7c93f843f11743f0b165a8"],["/archives/2023/page/2/index.html","cc3fd71747fac59b063e88ddeacaff9b"],["/archives/2023/page/3/index.html","9d96880105cd14042bd1596612212664"],["/archives/2023/page/4/index.html","99fb7345fdff81847180023ba677714c"],["/archives/index.html","7c6b3eae404b35d1d5285d6449713fcf"],["/archives/page/2/index.html","a4075e9b50b9b214b12501efbfdcbb1d"],["/archives/page/3/index.html","398a1810c906a22772bd85fcf29bda7d"],["/archives/page/4/index.html","d11caba297eeaf1c9281601e51a76cbd"],["/categories/Java/index.html","b40ed360e9ac156bfb269d78a96b5f27"],["/categories/Java/后端/index.html","a420dcc10fe8322f5d625820c010f17e"],["/categories/Java/基础/index.html","cd6b52d95faf21b6211c742ef3f37119"],["/categories/Java/基础/集合/index.html","fa0d93ee67b1b910d2e004d52d1972fa"],["/categories/Python/index.html","41fdb6afa97966ebe71015eaed429be4"],["/categories/Python/编程环境/index.html","96f5e5eb427ef8110e12e3ef9880deac"],["/categories/R语言/index.html","20e2fa0cb21f031c9201a50df3ab00fe"],["/categories/R语言/编程环境/index.html","23f15fbeadf81dd4b0d39149700921f9"],["/categories/index.html","5cd1eb367a190c325e2b98b17d02a60b"],["/categories/中间件/index.html","6093518a48b30eab60faac70dc24355b"],["/categories/前端/Vue/index.html","96cced3baf2c6b1671d50e32e1223ec1"],["/categories/前端/index.html","41d1da41fedcbdd59931e7715bda3a34"],["/categories/大数据开发/ElasticSearch/index.html","d293ce2b5a55e7df46264799af1774e4"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d9e22c8035c8bdb9aa9c2375c8c1faef"],["/categories/大数据开发/HBase/index.html","4065ab92aeedf5891888d9503bfe2b97"],["/categories/大数据开发/HBase/学习笔记/index.html","ff981af5c60bf80176c307de1b8adcb1"],["/categories/大数据开发/HBase/环境搭建/index.html","94b2f5cd666360bc5d4f0e34273d4bfd"],["/categories/大数据开发/Hadoop/index.html","6118fdfa9f5bdbe12720281700611963"],["/categories/大数据开发/Hadoop/技术/index.html","a29b63fad9e18680f244b6217ec97262"],["/categories/大数据开发/Hadoop/环境搭建/index.html","782c58f907ae72c26c921fe819745cb4"],["/categories/大数据开发/Redis/index.html","eaf038abb59961bb685ccc7ddb57ca76"],["/categories/大数据开发/Redis/技术/index.html","637342f7ee26beaa80a6f88f061c6e5d"],["/categories/大数据开发/Redis/环境搭建/index.html","f52e771d69cc9d52a32419ccffde1634"],["/categories/大数据开发/Spark/index.html","c4d0d916bad8aa41d778b9871aa5225c"],["/categories/大数据开发/Spark/环境搭建/index.html","1901ef191466e02e795fedfd753432d8"],["/categories/大数据开发/Zookeeper/index.html","4277b893e031bed03c4ee48e6f8ccd15"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ecdd53cde0d054e740553a73819e2683"],["/categories/大数据开发/index.html","faca6a5dabba167efdd9cd7550f88c14"],["/categories/操作系统/Linux/index.html","8af11301cf92248f687f7a8d5aa867af"],["/categories/操作系统/Mac/index.html","1b99bfd442de4d8a9ea98cc94522561c"],["/categories/操作系统/Windows/index.html","c37bb96601a2b1f632a55390ab34b821"],["/categories/操作系统/index.html","378713fdad3ba2205f93fb96dc21390a"],["/categories/数学建模/index.html","8ba4e228e1cca63db37ffbf2329968cc"],["/categories/数学建模/latex/index.html","adc1bef6fce54846fa2f88fc78c72b52"],["/categories/数学建模/优化类/index.html","3b8b2e0fb5f09f399a609c68acaab039"],["/categories/数学建模/优化类/现代优化算法/index.html","6e1723046e71d58c1e4cb2bc55396269"],["/categories/数学建模/优化类/规划类/index.html","a555a4614a81c1b35416436db95e6fa5"],["/categories/数学建模/绘图/index.html","81e329e3c6caf5d56aec5fdb13b52480"],["/categories/数据库/MySQL/index.html","6c2c35ce4f2d27b17ac5dfef70034700"],["/categories/数据库/index.html","4829691df6dd822bcdd3f6c9b891c3c4"],["/categories/数据结构和算法/index.html","8f77794ae5694d0a26544a941265a339"],["/categories/数据结构和算法/page/2/index.html","3fbef5040d1f69cb4354c429d8df8075"],["/categories/数据结构和算法/基本原理/bfs/index.html","20bcd11423699ff172e6415fe7e9733b"],["/categories/数据结构和算法/基本原理/dfs/index.html","a26aaed7e6e91377045302c86d6a6e4f"],["/categories/数据结构和算法/基本原理/index.html","322efb1fb939f9c886d270d5342413bf"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e8862749a00ec406e959987d5c4af0bd"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","2c3afa56a476360cc5bdadd68d62af01"],["/categories/数据结构和算法/基本原理/图论/index.html","4a8542a1ea0dbee5a2aaa76920765ed9"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","5a3fbe6be85adfb1a6e6e94ba8f1de7b"],["/categories/数据结构和算法/基本原理/数论/index.html","e86e4c974322d6636a3d13f9c0b40163"],["/categories/数据结构和算法/基本原理/树论/index.html","0dff65ff91fac7596fa776814a698786"],["/categories/数据结构和算法/基本原理/链表/index.html","dcb071c86c89833cd6e1384e69ec46f3"],["/categories/数据结构和算法/算法题/index.html","9e0764535755d4a1af0996f61d100b89"],["/categories/数据结构和算法/算法题/二分查找/index.html","c39da591e263033ddf313e23df97a374"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","30af6eb369e9157f6c53663916e18967"],["/categories/数据结构和算法/算法题/动态规划/index.html","76db6581593dcb5a117218e671147901"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a6ce17cadc761000d0e52389468806ca"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","a9cf33894174f3e5a5393db3f52ca9d0"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d5fef7ece21fe33768d1a7a220d9abcd"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","a931569abf0145b05b3420c6ce631d8b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","47a789c4a49305ae5146dea81077b503"],["/categories/数据结构和算法/算法题/树论/index.html","6ae484c1defccee5af9a2ce548abab33"],["/categories/杂七杂八/index.html","d0a949198cfbf39691ac1dd7d78b79f5"],["/categories/杂七杂八/博客搭建/index.html","34a440a6957455d1ec959e62827af938"],["/categories/编程工具下载/index.html","4878c51a406c253fd102d804016ac617"],["/categories/编程环境/index.html","52e3691d77b3e3d47fb4d5d04d04a36c"],["/categories/英语学习/index.html","75859e88dabaf1ab3e4a5293aabd99bc"],["/categories/英语学习/英语语法/index.html","89cb6f501c922f06ea1f573a184f7de6"],["/comments/index.html","1d122d0fb2a0d5009407ce718fcf6b81"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c3163aa67d8b7b154d5cd68592d1c34d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a9f019858a66435c83b61671d49c50e0"],["/movies/index.html","28e60238547c703335c7e544539d1c4f"],["/music/index.html","3e14c42ac0fca59989b13c4518eb3a0a"],["/page/2/index.html","7aec1899f2ebf2b25ff1aa17fc704a40"],["/page/3/index.html","40678d99c765da244a183938db549322"],["/page/4/index.html","47481643cd462618c8b606ab8866ad06"],["/page/5/index.html","794cac33f3d0cc365c42ec2ef7e12f0f"],["/page/6/index.html","70628bdc7e06c4483708d6adcc90242e"],["/posts/1021360842.html","8fb01e2504518448506b16c0f283f7ad"],["/posts/1120620192.html","42494cf068bfe4ccf252fbb40087e849"],["/posts/1141628095.html","584df6c2bb27a129fb6753076e998272"],["/posts/1168613674.html","87a871ac188beb7fac17474beae19b12"],["/posts/1219920510.html","d2bf3b78e521ec12b290f16738438275"],["/posts/1222166338.html","1bf0bf673d7ee00e59ea71832211e9aa"],["/posts/1259097482.html","d865e11b4ced1fa8fbf818f37d1595c8"],["/posts/1271036369.html","b16112df9b3596a2d02b31a85fa9b1dc"],["/posts/1312847445.html","46eab4799a7c9a2fed8b63f147fade09"],["/posts/135355774.html","4a770514b7955ecc8adf3c867d8d59c9"],["/posts/1375344716.html","07011d0ebfee59c0a42389b394188e88"],["/posts/1388991698.html","2c920fc68c1d93f69f7f19d33d11aebf"],["/posts/1410315814.html","155b9c32f6d18a79d58011a1d4e71521"],["/posts/1452790229.html","0cf8d698bd3a61cb0a1f0e6dcc30b407"],["/posts/1470079884.html","6b775b78f97b08e412c73755a920fdf2"],["/posts/1470079885.html","9722e09778d49a5e3a93ff85176bd4e0"],["/posts/1470079886.html","b6dee16fb35362fdb1a69ad9d6fdc5f0"],["/posts/1470079887.html","49dd048106e7642a69cf31811dff40be"],["/posts/1498536549.html","cc59fa8c312930c1fb6df13983285722"],["/posts/1547067935.html","a2e1f92f4633934e6f8b30ac9a302019"],["/posts/1557866301.html","d118482ff4c9f87ffa7d6e980e970b8f"],["/posts/1571776361.html","80b5b52643c10206e1225e4d7be7c4dc"],["/posts/1605124548.html","aabc5c4438d9ec5c34cd612adbec8aad"],["/posts/1633036852.html","0a066d6c38b9f912f2d016ba7c92c1cb"],["/posts/1765123828.html","6296f6500b0de24748c718d89f41e248"],["/posts/1767336200.html","8f5e8bc8175846e159e683172c6d23b3"],["/posts/1776114197.html","dbd2ab3eacdd478cfd982c5fa41fc51c"],["/posts/1817748743.html","51b59c4f5103be420de4542d6bf9df17"],["/posts/1925125395.html","1171ccd5db48f173441f368486183060"],["/posts/1966191251.html","f76e66a674d6961c16c28c86aaff25f7"],["/posts/1987617322.html","b24e16ebdb66f82a9a706c767f2f8638"],["/posts/1999788039.html","849dd52c46ae2c8716c8278a3217a63b"],["/posts/2075104059.html","89fb0d54eb6e92d33d966be1525f8abd"],["/posts/2087796737.html","497e75edc919b46d8c74e93ab89f9d6f"],["/posts/2106547339.html","ec55431d46aee9be2e8e523acdb61dce"],["/posts/2207806286.html","fdbdb837187b2739c388d4010eecf899"],["/posts/2225903441.html","f195ac3d06c456059e57b2d67d50a783"],["/posts/2265610284.html","e1bc5c0b838c83a60b1943a6f4833b86"],["/posts/2281352001.html","931a626595bd8e83bc7c746be973237a"],["/posts/2364755265.html","25e2ab4815b2aa4f974e07a09131e754"],["/posts/2414116852.html","a761b1d5eff83e8684abc15efac8fb16"],["/posts/2421785022.html","2ee944bb0c5ebe0ee96169b0de4b2eb3"],["/posts/2482902029.html","0f41a9e57c084f3cc038a2b9c9d5a969"],["/posts/2495386210.html","108ad2eadf5f0eda14ccc60f271c3b86"],["/posts/2516528882.html","e8f30c3f2e19a714a54057d3310bbb69"],["/posts/2526659543.html","b54f6b2a8b986ea50bc8675f2d989dc1"],["/posts/2529807823.html","a8562f1ece2ae4f5df1db56d923bf7ff"],["/posts/2596601004.html","5c433429a1c6b74be58a75f1d47cb612"],["/posts/2742438348.html","98f737b86f6a02896e8c062da24cec8d"],["/posts/2888309600.html","c59bdf8709dd572b8dc7fbce5b7d8eaf"],["/posts/2891591958.html","c0f738a580d07c2752a78c5ca9e9df6e"],["/posts/2909934084.html","e19e0e8a8686fbe95fe470bb25abaf1b"],["/posts/2920256992.html","2d28e59fd6f7abb2e20b0d80a1d866bb"],["/posts/3005926051.html","2960b6301c1290d7bb8ca7cdc472bbe6"],["/posts/309775400.html","8cd554e355984bc21637e24d654faf80"],["/posts/3156194925.html","333e6599b87a5c334364ebd353af4e6c"],["/posts/3169224211.html","b329447d2e91b9f880189e7b45d4f58e"],["/posts/3213899550.html","08e371b3940f3c62946ca5887e2b660e"],["/posts/3259212833.html","3ba5a90bab422c01c2f73ddbaf93b422"],["/posts/3266130344.html","e0f127fe6eecfc5c53461f7af8fdd88b"],["/posts/3292663995.html","3fba38a57a8a1ca9eb4b79f8d6eec818"],["/posts/3297135020.html","fe478628a46bbee9ae2fb844a54e78b5"],["/posts/3306641566.html","5626ba3409d386c711e4e167d14d819b"],["/posts/3312011324.html","85026f84981cbe7fa93e604faa4c0071"],["/posts/336911618.html","e0eb7ef3f3df2a6b4cb68acfbe88f3bf"],["/posts/3402121571.html","69cd4b62b254d665c116c1ff97f4aa68"],["/posts/3405577485.html","f376d0df1c298c207cf1d3e31f96f9eb"],["/posts/3498516849.html","d82b9b0039c6889c89f375238c59e15d"],["/posts/3513711414.html","7f6b6fddd99b9d5f02c5ae268aea2fe0"],["/posts/3546711884.html","734669d1296a73eba68eac420cc468e4"],["/posts/3731385230.html","7a8a00f5076231a309d34249a76619ae"],["/posts/3772089482.html","a64db40eefbfed6778b4738748c573b4"],["/posts/386609427.html","e64e29f7eebbb3b9b07433833bb3d8ff"],["/posts/4044235327.html","89dc5ec20d2ccc3d98b7d0354c0216b7"],["/posts/4115971639.html","b2a1a5f3ea29ea8fd7b2cd3485ad0723"],["/posts/4130790367.html","6b48aa611355894498ed3af1dcbb5ce9"],["/posts/4131986683.html","9fe31ce52f0b3a4e69ff2f05b3abcbd5"],["/posts/4177218757.html","6cf1c3be7876638d50a372c5ca0e920f"],["/posts/4192183953.html","9a06068ab5b8ed4ee22a3abb806352de"],["/posts/4261103898.html","eb959276ddd9dea35cb42c41b7172db7"],["/posts/469711973.html","9d125c91c7efeeea9cf2505654768e58"],["/posts/482495853.html","735c7b9e6b1ddd575cbe2a1306d60981"],["/posts/488247922.html","c1d9824d4d511aae37d9528c61708bee"],["/posts/517302816.html","5e48590fee6dde7c34c9d099e57f8678"],["/posts/570165348.html","7bbfc8885ca964eed2f229542e9647af"],["/posts/595890772.html","11396e04ee328ec62cd14f1baaf4b708"],["/posts/67485572.html","23fa17e53226cd8621f65a01dbc275e2"],["/posts/694347442.html","30823accf0276f0228879261686cd8e6"],["/posts/707384687.html","71b53b65c73fb8260c41de5de4a8efe8"],["/posts/71180092.html","476110a1bf8f3dcaed8ff00b866ec13f"],["/posts/716459272.html","4ee80250b07d0bae8b862026249aa22c"],["/posts/778231993.html","96a2d5e2cd4cba01d52f0ffdc5c685e4"],["/posts/795397410.html","5870fd0fff81a24348bc78b568752f8d"],["/posts/820223701.html","b91e2c8bc9acb100c7694e1b82dee2b5"],["/posts/830372185.html","3ccd1d2f1e9c471ce158b6163ee5d544"],["/posts/88294277.html","219e6b5c1b29ba76463ec61997d92472"],["/posts/939963535.html","7653265797202de6a6f3029ab9a8777f"],["/posts/983786067.html","b3806a9291b84f022631c6fa1b5e9d43"],["/sw-register.js","5ef26487cf3359de0b8c4c24f91a9794"],["/tags/C/index.html","0511ca74838ccc9c5d2e84192e0bb80d"],["/tags/C/page/2/index.html","7b0b95cb7fae120b9873fe7633b8bc02"],["/tags/C/page/3/index.html","a53020e8380cfdf9df2309ec23fcac9b"],["/tags/ElasticSearch/index.html","604fd1367276e91cc1cfa461d6598782"],["/tags/GUI/index.html","8bce431c05c181e8b5864b93aee1bc2b"],["/tags/HBase/index.html","e89172de9cb43480573a22b86682e1ac"],["/tags/Hadoop/index.html","338857f6b1f251104e259ca33bda93c4"],["/tags/Hadoop/page/2/index.html","7de8057d8df8647804b28dbec1b9ceb9"],["/tags/Java/index.html","3477a0828b41c2c08961714842a0527b"],["/tags/Java后端/index.html","317ca89d2f819d5c158594b0ae40a1a0"],["/tags/Java后端/page/2/index.html","3f3325c63bd3885b4c92bad7885e53cc"],["/tags/Java基础/index.html","c733353dca25781b0c32dab3734fae67"],["/tags/Java基础/page/2/index.html","8d07f044a1e84165ec9b05ba2373e146"],["/tags/Kibana/index.html","56643945b7671113b7462d69ec2ac5ac"],["/tags/Linux/index.html","b9524c62238360dbb88af2c01e7f1893"],["/tags/Linux/page/2/index.html","fdbe3c5d19b8e013d3bd058b3ff25110"],["/tags/Linux/page/3/index.html","0b3523abfb688fb3a44ca432e7ce1591"],["/tags/Mac/index.html","49b2588a8df8cd29d165168d6a03d121"],["/tags/Mac/page/2/index.html","86f7edd5d0755e9ea95c9da41a6df2c0"],["/tags/Maven/index.html","d6f84e7fbd0f777cdd38ce55f9ba3191"],["/tags/MySQL/index.html","03f69745442395484b82702b5b2c7326"],["/tags/Python/index.html","60b5dbbaaead3d9768ad36fa01e8e4ec"],["/tags/Redis/index.html","1c04d12ee7fb274a28af8fd1a4017678"],["/tags/R语言/index.html","0322220babaf64b89cf0e6c8ebf5a253"],["/tags/Spark/index.html","0ec1833d2793b49c925ba81ab0e680f3"],["/tags/Ubuntu/index.html","37ce8a2be68152b47276d5ea62e9b68e"],["/tags/Vue/index.html","cd7d98c76ee54a18228afae465b21381"],["/tags/Windows/index.html","4c05baa71930a18333525659b292b147"],["/tags/ZooKeeper/index.html","33669c961ab5cfad4c97b9d790045198"],["/tags/bfs/index.html","e56fc3b6b91fbb2f0d5aa1c29e569a98"],["/tags/dfs/index.html","51661643a3d441512c1ba1de4272d2e0"],["/tags/folium/index.html","edfcfd214dc30c40a18a5fd855b8d9f2"],["/tags/git/index.html","88f6df6feac76e70d474cfb8fe8f79e1"],["/tags/index.html","b48fd36cf149d3aa0b17962684cac4e2"],["/tags/latex/index.html","49fc02d194a69e34dcfd75308e9bec8f"],["/tags/中间件/index.html","f72753157324df77ee9bbc7806021d3d"],["/tags/二分查找/index.html","b6b08db9e4ffd4ff02daf4862f94a78a"],["/tags/优化类/index.html","58029ecebcef3f86621309304218908f"],["/tags/前端/index.html","96c3f17fe80add5e4135421336f7de5f"],["/tags/前缀和与差分/index.html","9d91453588afa503e4401c8216625c76"],["/tags/动态规划/index.html","63585134d16107ab56c4dc246df501ae"],["/tags/动态规划/page/2/index.html","fd238191bd5599601d966e25e0e84953"],["/tags/博客搭建/index.html","a779c319b7b6d1d07fd7b7845e126c4f"],["/tags/图论/index.html","590d5be262568f2ea254b703e08e16a8"],["/tags/大数据/index.html","150d43d164ffc7993c7dc94c08eb22c9"],["/tags/大数据/page/2/index.html","b51d2250efb3daa1578965a5bcff8baf"],["/tags/操作系统/index.html","5eaa70a3bc9668ce918d87e450777f3d"],["/tags/数学建模/index.html","587018e0982ef74db26ef4c6642c557d"],["/tags/数据库/index.html","af2b843b9120c0b5a4f6084bae655a8b"],["/tags/数据结构和算法/index.html","3083f3ada4522933820c150f04490d75"],["/tags/数据结构和算法/page/2/index.html","07dfe0f6c148466b77a414a038b06f22"],["/tags/数据结构和算法/page/3/index.html","d0bff25598c3cb94f01f5daf9ad54713"],["/tags/数组和字符串/index.html","907b4bfc104de8b49b8864975d4df13a"],["/tags/枚举类/index.html","adabcf1b2907805c7822a74e0fa45b3e"],["/tags/栈和队列/index.html","de552368037e116420bc447ad2eef1ff"],["/tags/树论/index.html","ac016378ddb9ec2b4366e64ddf9cfdac"],["/tags/测试/index.html","15f7d14e9f76e646b3928d3e5d7eb9ef"],["/tags/环境/index.html","a0a0573eea428a1f3a1ca82d972cdd86"],["/tags/环境变量/index.html","8177761b7acf9de2098a1e4f8d60d75c"],["/tags/绘图/index.html","a00657bfcfce97c9ce5f52445a756883"],["/tags/编程工具/index.html","8f80650a6f457ad60c0d8eb8cce2e039"],["/tags/编程环境/index.html","32014796adbfa420f99170eddaa60958"],["/tags/网络编程/index.html","30881bd8eaf82df06d24f40e9dfec014"],["/tags/英语语法/index.html","b24e532b035872f27e735e659be5d189"],["/tags/论文/index.html","71e035434ac2eb4a12801d55c32f6fc5"],["/tags/资源下载/index.html","9e4dfeb386c9a7bb5e5bd2d4fee95a92"],["/tags/链表/index.html","3bd916488f34c022c0cf58354dc15343"],["/tags/集合/index.html","4109c0cde81e0a0925de9b4255424bde"],["/tags/集群/index.html","d4263a708b5d4f3284ac7ea65c1db97e"]];
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
