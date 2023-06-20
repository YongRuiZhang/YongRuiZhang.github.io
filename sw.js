/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ce27f11da33de1bdb232cdda62f0a12f"],["/about/index.html","b662cb5b6c315aee4a49859b27a46ff7"],["/archives/2023/01/index.html","d0ccf8352ba487bd207d7756eff7c47a"],["/archives/2023/02/index.html","24d6ee09be4bedc907072c5b09bd2a7c"],["/archives/2023/02/page/2/index.html","a6d361c3abf599d30beedd940db1d982"],["/archives/2023/03/index.html","de9cb855a7ac4249d89ef85ac931de10"],["/archives/2023/05/index.html","efe1fb97fd28a351f9c456aa4325bd0e"],["/archives/2023/06/index.html","da842aa04c01e93df943627f58f3b353"],["/archives/2023/index.html","89bf13c0bca01db662e44cd79bbb5e29"],["/archives/2023/page/2/index.html","ad9218711ac0b22a04d5e5d7428de6b4"],["/archives/2023/page/3/index.html","d4b4b4ce0cc43f6884f203d8dec9d9dd"],["/archives/2023/page/4/index.html","0c588ee266b10a724cb0865163ea8674"],["/archives/index.html","130b199dd12206245969a2e86d96d70c"],["/archives/page/2/index.html","b0a4a8e62e2031f1e754b63c779bb348"],["/archives/page/3/index.html","46771b83f81dcd0dd42fc9b5c1ccdd01"],["/archives/page/4/index.html","ec01a10b853d79edac2d87bae315b0de"],["/categories/Java/index.html","085872e2b14603865bdfb1ad14cf216a"],["/categories/Java/后端/index.html","a73d9f43b5061b20f36c3460958394ac"],["/categories/Java/基础/index.html","2bd2659a5ea6101c000fce14cd224b1d"],["/categories/Java/基础/集合/index.html","d1c43e5c9fe7ff810dfc001d2e47960e"],["/categories/Python/index.html","4bacd397b5bc103728fb37623989b678"],["/categories/Python/编程环境/index.html","6cace2788b161b23c4370d2958741819"],["/categories/R语言/index.html","23ccd40ba4f4e4672391cb4d028b4bce"],["/categories/R语言/编程环境/index.html","aad1d289f2eb418d9612e6377fef2b3e"],["/categories/index.html","5cd1eb367a190c325e2b98b17d02a60b"],["/categories/中间件/index.html","80d84f24eb7500867142a15dde4d2a85"],["/categories/前端/Vue/index.html","51113a2738c403eebffd049a49b27263"],["/categories/前端/index.html","0a259d8648660e6f84e0f6de55917915"],["/categories/大数据开发/ElasticSearch/index.html","08d5496f78589096c17b6545c5b1d586"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","48151f85170699ca291b35c8ef06e9df"],["/categories/大数据开发/HBase/index.html","c72fed71aa89ed3c07373f8ad69ae0ce"],["/categories/大数据开发/HBase/学习笔记/index.html","0d1acfa35ca077954c32f6a16f699a8c"],["/categories/大数据开发/HBase/环境搭建/index.html","e70c7618e2da7dfc5ec61e8c5198dfe7"],["/categories/大数据开发/Hadoop/index.html","fe4f54cd6f0f92271e2ebf3c10e78695"],["/categories/大数据开发/Hadoop/技术/index.html","343b4239bd2cee7d04a702f759433518"],["/categories/大数据开发/Hadoop/环境搭建/index.html","17111533ba0f019f4358751a0e4cdbcc"],["/categories/大数据开发/Redis/index.html","52610b0abf00491122a39e8b9a5723b1"],["/categories/大数据开发/Redis/技术/index.html","b6b0d7dd69ee0ed149a04c51d0b64ec1"],["/categories/大数据开发/Redis/环境搭建/index.html","6781e5c10338640e78409c01bca092c6"],["/categories/大数据开发/Spark/index.html","e20c0b60db537310277e0b0f3ce90fe6"],["/categories/大数据开发/Spark/环境搭建/index.html","bcdc23965236beca8623c634feee7d12"],["/categories/大数据开发/Zookeeper/index.html","4b242b45690dc19e7c7a4c920ff1970c"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","c40a124a12062fb472336360fbfd112b"],["/categories/大数据开发/index.html","e09fc085f2491606fd2c82822b7969e0"],["/categories/操作系统/Linux/index.html","4e23e538b711a6e9a9cb087fa2939085"],["/categories/操作系统/Mac/index.html","6006f489b7e6c2a6496c2608ddca36f4"],["/categories/操作系统/Windows/index.html","161178428f3bba8f5ca6b93fb1543567"],["/categories/操作系统/index.html","6eef6a6acf7d60b6a11ca24b0e8e9860"],["/categories/数学建模/index.html","8f336fabb48e78b891bcd12b53be67ee"],["/categories/数学建模/latex/index.html","0b57f040bc39e6539c1d334d422df0f4"],["/categories/数学建模/优化类/index.html","c193e723c49b23fc86d42b4932d3b689"],["/categories/数学建模/优化类/现代优化算法/index.html","807152324b086f67dc3d5fd51eeaa521"],["/categories/数学建模/优化类/规划类/index.html","946114b55e9fe794391469833e4f9bd4"],["/categories/数学建模/绘图/index.html","73e4c4bfc674f4e8896db6f81b05b0f6"],["/categories/数据库/MySQL/index.html","ddcc425803b7a14b2caf9b2b1d273e2d"],["/categories/数据库/index.html","2940e123fe5817107d021502536cac63"],["/categories/数据结构和算法/index.html","494ec739ee20a293f678f636549ffc6e"],["/categories/数据结构和算法/page/2/index.html","b5bc11677623a1994fa9cb3c1b1ee277"],["/categories/数据结构和算法/基本原理/bfs/index.html","5c74cf4f1aaa9fcc7dc07fc3b0f4c730"],["/categories/数据结构和算法/基本原理/dfs/index.html","4ee52712738dabdd04c63009e0908e34"],["/categories/数据结构和算法/基本原理/index.html","b61a8b9527483f3ff202601f07528560"],["/categories/数据结构和算法/基本原理/动态规划/index.html","251d93425e495220198e45f8b66b193c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","96edda49c37984521c3626eab7104f32"],["/categories/数据结构和算法/基本原理/图论/index.html","a59b2a160f5ae1343bc6d4a097e53c6c"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","699c163b98d6c86a439d8e3586aba058"],["/categories/数据结构和算法/基本原理/数论/index.html","6e8039f7441ad1a43f12f8fefa72f69e"],["/categories/数据结构和算法/基本原理/树论/index.html","3f3906652ad974a4e8ea845d914e023a"],["/categories/数据结构和算法/基本原理/链表/index.html","aa55587bf393839a6ed4ae346c629cc8"],["/categories/数据结构和算法/算法题/index.html","0e4c81555ab9930745e093d7e6e44553"],["/categories/数据结构和算法/算法题/二分查找/index.html","5d5d12841b84119b1024d30bf2ebbf96"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1d2f6be43b5c6b7fbc434ce0f479a4ee"],["/categories/数据结构和算法/算法题/动态规划/index.html","6f02a649ad127d4660077f341dfa0d92"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3514f5dfd91e45e6dc22f71db3db0d2f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","947be28589511a66d308417ed68aaf22"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5554eb8206f119b35b1cd117b05bb17c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d52503acfdc687ff49a00ab5db8d7221"],["/categories/数据结构和算法/算法题/栈和队列/index.html","2226992e2f1c0150ba14b2953bd68c42"],["/categories/数据结构和算法/算法题/树论/index.html","58e1ad4810b7d93c7c818bdb7b98984a"],["/categories/杂七杂八/index.html","6cd1495a4ee06cbc088535ebb07e44b7"],["/categories/杂七杂八/博客搭建/index.html","ee88767c37667b96a18714d8116dfe84"],["/categories/编程工具下载/index.html","e1be699b6021de538599c9d640e53122"],["/categories/编程环境/index.html","e8badaffd917b0bdf87fb4266d8d0efc"],["/categories/英语学习/index.html","3a8376cb3c522f5e6497fc6f2bed7477"],["/categories/英语学习/英语语法/index.html","710404316754915bf1455777acab88a4"],["/comments/index.html","6402a8c5f26d99f2af565cc1c9348fd7"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","1169fa0953cb6fd52d1fb501f7b40e75"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1ffb37ad3c339f70de62859589b7d279"],["/movies/index.html","617033f1a3fdfa907f0de8d98d705efb"],["/music/index.html","d1ebddbaf7a6bae9fb9e23f124e6e14c"],["/page/2/index.html","f2895cc59bd8202ae82f15ae0d083db6"],["/page/3/index.html","d5efeb7ecb165b524bd93a83e22a46e5"],["/page/4/index.html","d65cd22279b66ab9b2fcc7e5cd8cd8e2"],["/page/5/index.html","c812714b9b1284a712e96e1dcf78bdd0"],["/page/6/index.html","2a69dd7722d7d298cb68851362da3704"],["/posts/1021360842.html","8fb01e2504518448506b16c0f283f7ad"],["/posts/1120620192.html","42494cf068bfe4ccf252fbb40087e849"],["/posts/1141628095.html","584df6c2bb27a129fb6753076e998272"],["/posts/1168613674.html","87a871ac188beb7fac17474beae19b12"],["/posts/1219920510.html","d2bf3b78e521ec12b290f16738438275"],["/posts/1222166338.html","1bf0bf673d7ee00e59ea71832211e9aa"],["/posts/1259097482.html","d865e11b4ced1fa8fbf818f37d1595c8"],["/posts/1271036369.html","b16112df9b3596a2d02b31a85fa9b1dc"],["/posts/1312847445.html","46eab4799a7c9a2fed8b63f147fade09"],["/posts/135355774.html","4a770514b7955ecc8adf3c867d8d59c9"],["/posts/1375344716.html","07011d0ebfee59c0a42389b394188e88"],["/posts/1388991698.html","2c920fc68c1d93f69f7f19d33d11aebf"],["/posts/1410315814.html","155b9c32f6d18a79d58011a1d4e71521"],["/posts/1452790229.html","0cf8d698bd3a61cb0a1f0e6dcc30b407"],["/posts/1470079884.html","6b775b78f97b08e412c73755a920fdf2"],["/posts/1470079885.html","9722e09778d49a5e3a93ff85176bd4e0"],["/posts/1470079886.html","b6dee16fb35362fdb1a69ad9d6fdc5f0"],["/posts/1470079887.html","49dd048106e7642a69cf31811dff40be"],["/posts/1498536549.html","cc59fa8c312930c1fb6df13983285722"],["/posts/1547067935.html","a2e1f92f4633934e6f8b30ac9a302019"],["/posts/1557866301.html","d118482ff4c9f87ffa7d6e980e970b8f"],["/posts/1571776361.html","80b5b52643c10206e1225e4d7be7c4dc"],["/posts/1605124548.html","aabc5c4438d9ec5c34cd612adbec8aad"],["/posts/1633036852.html","0a066d6c38b9f912f2d016ba7c92c1cb"],["/posts/1765123828.html","6296f6500b0de24748c718d89f41e248"],["/posts/1767336200.html","8f5e8bc8175846e159e683172c6d23b3"],["/posts/1776114197.html","dbd2ab3eacdd478cfd982c5fa41fc51c"],["/posts/1817748743.html","2844d9b6e8c40075fce12351b646ce02"],["/posts/1925125395.html","1171ccd5db48f173441f368486183060"],["/posts/1966191251.html","f76e66a674d6961c16c28c86aaff25f7"],["/posts/1987617322.html","b24e16ebdb66f82a9a706c767f2f8638"],["/posts/1999788039.html","849dd52c46ae2c8716c8278a3217a63b"],["/posts/2075104059.html","89fb0d54eb6e92d33d966be1525f8abd"],["/posts/2087796737.html","497e75edc919b46d8c74e93ab89f9d6f"],["/posts/2106547339.html","ec55431d46aee9be2e8e523acdb61dce"],["/posts/2207806286.html","fdbdb837187b2739c388d4010eecf899"],["/posts/2225903441.html","f195ac3d06c456059e57b2d67d50a783"],["/posts/2265610284.html","e1bc5c0b838c83a60b1943a6f4833b86"],["/posts/2281352001.html","931a626595bd8e83bc7c746be973237a"],["/posts/2364755265.html","25e2ab4815b2aa4f974e07a09131e754"],["/posts/2414116852.html","a761b1d5eff83e8684abc15efac8fb16"],["/posts/2421785022.html","2ee944bb0c5ebe0ee96169b0de4b2eb3"],["/posts/2482902029.html","0f41a9e57c084f3cc038a2b9c9d5a969"],["/posts/2495386210.html","108ad2eadf5f0eda14ccc60f271c3b86"],["/posts/2516528882.html","e8f30c3f2e19a714a54057d3310bbb69"],["/posts/2526659543.html","b54f6b2a8b986ea50bc8675f2d989dc1"],["/posts/2529807823.html","a8562f1ece2ae4f5df1db56d923bf7ff"],["/posts/2596601004.html","5c433429a1c6b74be58a75f1d47cb612"],["/posts/2742438348.html","98f737b86f6a02896e8c062da24cec8d"],["/posts/2888309600.html","c59bdf8709dd572b8dc7fbce5b7d8eaf"],["/posts/2891591958.html","c0f738a580d07c2752a78c5ca9e9df6e"],["/posts/2909934084.html","e19e0e8a8686fbe95fe470bb25abaf1b"],["/posts/2920256992.html","2d28e59fd6f7abb2e20b0d80a1d866bb"],["/posts/3005926051.html","2960b6301c1290d7bb8ca7cdc472bbe6"],["/posts/309775400.html","8cd554e355984bc21637e24d654faf80"],["/posts/3156194925.html","333e6599b87a5c334364ebd353af4e6c"],["/posts/3169224211.html","b329447d2e91b9f880189e7b45d4f58e"],["/posts/3213899550.html","08e371b3940f3c62946ca5887e2b660e"],["/posts/3259212833.html","3ba5a90bab422c01c2f73ddbaf93b422"],["/posts/3266130344.html","e0f127fe6eecfc5c53461f7af8fdd88b"],["/posts/3292663995.html","3fba38a57a8a1ca9eb4b79f8d6eec818"],["/posts/3297135020.html","fe478628a46bbee9ae2fb844a54e78b5"],["/posts/3306641566.html","5626ba3409d386c711e4e167d14d819b"],["/posts/3312011324.html","85026f84981cbe7fa93e604faa4c0071"],["/posts/336911618.html","e0eb7ef3f3df2a6b4cb68acfbe88f3bf"],["/posts/3402121571.html","69cd4b62b254d665c116c1ff97f4aa68"],["/posts/3405577485.html","f376d0df1c298c207cf1d3e31f96f9eb"],["/posts/3498516849.html","d82b9b0039c6889c89f375238c59e15d"],["/posts/3513711414.html","7f6b6fddd99b9d5f02c5ae268aea2fe0"],["/posts/3546711884.html","734669d1296a73eba68eac420cc468e4"],["/posts/3731385230.html","7a8a00f5076231a309d34249a76619ae"],["/posts/3772089482.html","a64db40eefbfed6778b4738748c573b4"],["/posts/386609427.html","e64e29f7eebbb3b9b07433833bb3d8ff"],["/posts/4044235327.html","89dc5ec20d2ccc3d98b7d0354c0216b7"],["/posts/4115971639.html","b2a1a5f3ea29ea8fd7b2cd3485ad0723"],["/posts/4130790367.html","6b48aa611355894498ed3af1dcbb5ce9"],["/posts/4131986683.html","9fe31ce52f0b3a4e69ff2f05b3abcbd5"],["/posts/4177218757.html","6cf1c3be7876638d50a372c5ca0e920f"],["/posts/4192183953.html","9a06068ab5b8ed4ee22a3abb806352de"],["/posts/4261103898.html","eb959276ddd9dea35cb42c41b7172db7"],["/posts/469711973.html","9d125c91c7efeeea9cf2505654768e58"],["/posts/482495853.html","735c7b9e6b1ddd575cbe2a1306d60981"],["/posts/488247922.html","c1d9824d4d511aae37d9528c61708bee"],["/posts/517302816.html","ca8a948eebe7fbf275c5112bfeac6433"],["/posts/570165348.html","7bbfc8885ca964eed2f229542e9647af"],["/posts/595890772.html","11396e04ee328ec62cd14f1baaf4b708"],["/posts/67485572.html","23fa17e53226cd8621f65a01dbc275e2"],["/posts/694347442.html","30823accf0276f0228879261686cd8e6"],["/posts/707384687.html","71b53b65c73fb8260c41de5de4a8efe8"],["/posts/71180092.html","476110a1bf8f3dcaed8ff00b866ec13f"],["/posts/716459272.html","4ee80250b07d0bae8b862026249aa22c"],["/posts/778231993.html","96a2d5e2cd4cba01d52f0ffdc5c685e4"],["/posts/795397410.html","5870fd0fff81a24348bc78b568752f8d"],["/posts/820223701.html","b91e2c8bc9acb100c7694e1b82dee2b5"],["/posts/830372185.html","3ccd1d2f1e9c471ce158b6163ee5d544"],["/posts/88294277.html","219e6b5c1b29ba76463ec61997d92472"],["/posts/939963535.html","7653265797202de6a6f3029ab9a8777f"],["/posts/983786067.html","b3806a9291b84f022631c6fa1b5e9d43"],["/sw-register.js","cdd7146d96d34be903f0fa4872b01af2"],["/tags/C/index.html","cc7ca9ca422cdf7d5cfefa4867473723"],["/tags/C/page/2/index.html","c048b1c691a1ad66faf46355d2b254f7"],["/tags/C/page/3/index.html","568ac78c9a0536959df4491cbfd4963e"],["/tags/ElasticSearch/index.html","47485c33d80422d5fb26775f3caa9ad0"],["/tags/GUI/index.html","fb200278e7646c31af098c94e539b313"],["/tags/HBase/index.html","89af1235c74bce6b48b8af747d4cd617"],["/tags/Hadoop/index.html","2781cc864c23fd49f99cc55723e4be12"],["/tags/Hadoop/page/2/index.html","44c506702391c162a5757a7a5dfe4086"],["/tags/Java/index.html","2c4abdd0c39a854fb65bc136fc314f93"],["/tags/Java后端/index.html","5051914e0e6265cb7cc199f0dc81dd3e"],["/tags/Java后端/page/2/index.html","cc49c9b6b0e5e3b630660e831783fd33"],["/tags/Java基础/index.html","0a9b2bee693a3a3cacaabec55ca06c5d"],["/tags/Java基础/page/2/index.html","dac936a51e4b686fe9346481867466b0"],["/tags/Kibana/index.html","4208b8b356795a148acef72c4fde2eb8"],["/tags/Linux/index.html","ddecb49cf76ea444197c90d13c4bc8b7"],["/tags/Linux/page/2/index.html","0ba9749d6e9d4425c726422e9ebad822"],["/tags/Linux/page/3/index.html","ce7a04b467935206a2eb71373ef8e663"],["/tags/Mac/index.html","47bd16d74abfe21275237547814cc058"],["/tags/Mac/page/2/index.html","c223fbdcd1cb8fd192f05e7ee060fdc2"],["/tags/Maven/index.html","f2345c66d8fe264291825cf248cee69c"],["/tags/MySQL/index.html","78db4568f615b49121cf5859a9c4aba4"],["/tags/Python/index.html","4fab04016e6f0d196006668ca5792bf1"],["/tags/Redis/index.html","c9024fe57c0e4f617ace1046bdc2233b"],["/tags/R语言/index.html","73e3d35c7a6418f4480eca9c2e0f6e63"],["/tags/Spark/index.html","ae88ee9595ec362501f2aa60ad8a2206"],["/tags/Ubuntu/index.html","b5d784e5be73c805789f1fc9bdc4b0e4"],["/tags/Vue/index.html","805069a3cc1297b94285d9edaad3b7f6"],["/tags/Windows/index.html","9c596006975db93e6a4dcbcc68d03823"],["/tags/ZooKeeper/index.html","dcbf70190d4d85c7de3e012b1208a97a"],["/tags/bfs/index.html","530224ae69101aacfa2be51e7dc15e67"],["/tags/dfs/index.html","7064f43e3404411943a48329ae875bcc"],["/tags/folium/index.html","bc583df62c3161cf72eb74492774076f"],["/tags/git/index.html","c51e35335b3a75c03c35460509ae246a"],["/tags/index.html","8f58634ebcb55388882c9ee4b954163a"],["/tags/latex/index.html","2bf457f00d0078dd5888ff0c19a6dbb5"],["/tags/中间件/index.html","341ddb39a5c779b26d4fd1408ac04860"],["/tags/二分查找/index.html","5b819cd340b8cb779f95a848ee231811"],["/tags/优化类/index.html","be29e8969f346b897f42fa1cb7a5b61e"],["/tags/前端/index.html","4108c7e019da2873d9cce339661fb9f9"],["/tags/前缀和与差分/index.html","80addd2e88d96d25841dce23f994d603"],["/tags/动态规划/index.html","e6c747cf0e4e8a4f8f7f7bf47423a815"],["/tags/动态规划/page/2/index.html","2e5113b1ef2ee6908997e2f161c8449d"],["/tags/博客搭建/index.html","00e0741aa9f2936501249524288931c1"],["/tags/图论/index.html","bdae327994c1457c1a9bb6c63acf0096"],["/tags/大数据/index.html","6babe421a55bb2e76db48dc0f899cc76"],["/tags/大数据/page/2/index.html","63246c0b3534a1233806fb64f42fdc6b"],["/tags/操作系统/index.html","f7174119597c603ce4f191dcfbb8eb60"],["/tags/数学建模/index.html","828ecf32c09704f37584dc5ff0a06191"],["/tags/数据库/index.html","436b78b1246854d8c5d5354dce3acf48"],["/tags/数据结构和算法/index.html","44284755bd398fb9f90084844f979695"],["/tags/数据结构和算法/page/2/index.html","7a6135d403d232d5e3aa4fb6c0b31764"],["/tags/数据结构和算法/page/3/index.html","8dbb327a2e145a40f1b95c14aa7e4b97"],["/tags/数组和字符串/index.html","774d09967f988544f17d18bb5dad5b7e"],["/tags/枚举类/index.html","60e8c7e8d5e7e83edd501f930fd49f2d"],["/tags/栈和队列/index.html","39c5bfef00ab35f786696232638cbfcf"],["/tags/树论/index.html","456137dcabd063c431af673cf4c9a333"],["/tags/测试/index.html","f7020c4d237c40f052f8a2ea05408b9d"],["/tags/环境/index.html","31bbe1375a0d83c7e0dd59443516eab4"],["/tags/环境变量/index.html","09928405b53b5945cdd5c7e69f1a94ad"],["/tags/绘图/index.html","36a9d5dd2651446123f6e90fcad9d12b"],["/tags/编程工具/index.html","694fc14d8d9a4025f4f6d95de909d3b7"],["/tags/编程环境/index.html","1ef6a7ec47920e901321531038806186"],["/tags/网络编程/index.html","1f3a85e79b5cf8809b362cdd98f65dc6"],["/tags/英语语法/index.html","47fe617658ede1cadcdece3be0e33bbc"],["/tags/论文/index.html","0c1292d33b20f8e08faf290bbeb373c3"],["/tags/资源下载/index.html","f207509a314c9df52845b864359a9f3d"],["/tags/链表/index.html","94b07e4ca7efa285406309aacea8261f"],["/tags/集合/index.html","2ee251de4ec4fb63f7d6bd36de4f1420"],["/tags/集群/index.html","b26ab40fb386faf97ff86f6921879c2b"]];
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
