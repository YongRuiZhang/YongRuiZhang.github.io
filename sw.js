/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e1360cd85a788d76cc323a892d08c443"],["/about/index.html","b662cb5b6c315aee4a49859b27a46ff7"],["/archives/2023/01/index.html","cc6b5c245d7dcc79cbc09c9885c3cf35"],["/archives/2023/02/index.html","d9c27180e1e2bb1a09cd6cb8ab44fbe2"],["/archives/2023/02/page/2/index.html","fadf6150141a7dc9c276e993d5e92d50"],["/archives/2023/03/index.html","a2e58ab14fef16da8c8291661bf21c57"],["/archives/2023/05/index.html","e83346801682735e0004c286e843e0c2"],["/archives/2023/06/index.html","fc7f058a19e57d149da62fc0b84a63f0"],["/archives/2023/index.html","578e04f8caf298aae40fd9c168b4ea04"],["/archives/2023/page/2/index.html","06a087fcb50c84f8398baab5cc19bd72"],["/archives/2023/page/3/index.html","d8cd6889e43518aa5db74e055ed62b37"],["/archives/2023/page/4/index.html","0bb9970b20e850d67cd296b4a29cac54"],["/archives/index.html","fed02dcbb3e27ca8aed59b2121b5ad03"],["/archives/page/2/index.html","95253b5040d7fe204feff047d30159c2"],["/archives/page/3/index.html","df0d526c4a515eda9d57a7fb7e74a0f3"],["/archives/page/4/index.html","9bdfd888d39e356bdbab64e36f8eddaf"],["/categories/Java/index.html","632b25bf54e69dff6a327a6fb32da8f4"],["/categories/Java/后端/index.html","593dd8b27cc0c147d9de205e9daa1947"],["/categories/Java/基础/index.html","32c44dd4c92a6bf4240f56c165c2307f"],["/categories/Java/基础/集合/index.html","428cfe1c7016e57dcc37db4bffed4246"],["/categories/Python/index.html","32992e788fbedb9d57e8251c125bb14d"],["/categories/Python/编程环境/index.html","60d823a762ebad5d2d592f8f0d36e0cf"],["/categories/R语言/index.html","b8f6e38764637179aaf2fffe468946f5"],["/categories/R语言/编程环境/index.html","4ce7ef4c2bba8a6b8d9d0b2fbfd61284"],["/categories/index.html","5cd1eb367a190c325e2b98b17d02a60b"],["/categories/中间件/index.html","2714ed323add56edc394bb2e77ee6c6c"],["/categories/前端/Vue/index.html","6800735f9e343b992ffa19834d4c3b24"],["/categories/前端/index.html","51a876bfb8f289e55c7afebf1414bed4"],["/categories/大数据开发/ElasticSearch/index.html","6c696f5a8ca1ff25cb6d3e3e630dcce4"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","997d5b76dd042ca00a1c5c6b1826aafd"],["/categories/大数据开发/HBase/index.html","c6219f15cd5bc15c08397f46f746119b"],["/categories/大数据开发/HBase/学习笔记/index.html","5dabec887f061fc7b64a5a5d0ffeef1c"],["/categories/大数据开发/HBase/环境搭建/index.html","9e22f2a37277a002c025075c89440b84"],["/categories/大数据开发/Hadoop/index.html","de4ff4236ab1869e791a2ceeca5b1f94"],["/categories/大数据开发/Hadoop/技术/index.html","296af9a1d3db29a0648b8a56c7886160"],["/categories/大数据开发/Hadoop/环境搭建/index.html","eb4b95c53ba4391b7044b13f1c3367bd"],["/categories/大数据开发/Redis/index.html","fd313c19331a3453569b188765bc3736"],["/categories/大数据开发/Redis/技术/index.html","5361c8e956e6558a41b2b338c0a4eb64"],["/categories/大数据开发/Redis/环境搭建/index.html","d1fc6236cf0f5f7fdabd0e08e19f80d2"],["/categories/大数据开发/Spark/index.html","695e1ca604ec381c2bb436b7e53cf3ec"],["/categories/大数据开发/Spark/环境搭建/index.html","32bc949c1e8c60e04440057796a686b9"],["/categories/大数据开发/Zookeeper/index.html","cd5bc746a58cc3d1116effd9117700cc"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","5d37209cfab9d070191f42d1723493c2"],["/categories/大数据开发/index.html","c962053f0dedfc55bc08c1ce2f64ce16"],["/categories/操作系统/Linux/index.html","902b584057018f6e47622aa2d6ef9249"],["/categories/操作系统/Mac/index.html","51b6cfb1daeb43dcac84fb332a4574ff"],["/categories/操作系统/Windows/index.html","4ea8a74c4656c8fcc9300fe93d29dab1"],["/categories/操作系统/index.html","1ed04324379213e0745ebd9ee65b7e11"],["/categories/数学建模/index.html","656d56ba693333594d5a1a63a98688d1"],["/categories/数学建模/latex/index.html","b19adddc827ae1c7c49c5d1e6c110d3c"],["/categories/数学建模/优化类/index.html","49d01816d2a85ac3da6571a5c51020f5"],["/categories/数学建模/优化类/现代优化算法/index.html","bde2e772657a1e9c2500f7311264b21d"],["/categories/数学建模/优化类/规划类/index.html","9e6a657a6a3b28c8f695eb65b37eeac1"],["/categories/数学建模/绘图/index.html","35c291797464cccab898498f95cd1568"],["/categories/数据库/MySQL/index.html","8b0c54620f075a7fb7245d54b9075017"],["/categories/数据库/index.html","d4050bc7df77930ff540bb80fe4fff4f"],["/categories/数据结构和算法/index.html","481a273701ba69fc02a774f2f1406082"],["/categories/数据结构和算法/page/2/index.html","134f40f31703023d4debd0c9e6696506"],["/categories/数据结构和算法/基本原理/bfs/index.html","43f9ee0d87caefeb0e97be1026ae9944"],["/categories/数据结构和算法/基本原理/dfs/index.html","f01413efbe197658e1a3589881cf95a6"],["/categories/数据结构和算法/基本原理/index.html","075a0b6eb17580035a7196f556a02944"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4176bbaab0c1ffc0ced35ca7cf32cdaa"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","246fbdfd9d0343a7b929db3f012d4374"],["/categories/数据结构和算法/基本原理/图论/index.html","ba8156aa69f0e605447662ec8e0f067f"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fc9cafe66e60a4f5d25feee85035c8ef"],["/categories/数据结构和算法/基本原理/数论/index.html","7d44fd07fc555306a8ab3676e42b1d87"],["/categories/数据结构和算法/基本原理/树论/index.html","5369e333b39dfde2f7c8376660604a06"],["/categories/数据结构和算法/基本原理/链表/index.html","889dcad28d29bbc491fa0581efc62079"],["/categories/数据结构和算法/算法题/index.html","faa6b60578db49fbc86480c0d5068980"],["/categories/数据结构和算法/算法题/二分查找/index.html","306bd65d7cf5584ac2db13f68b261c37"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2e20d1f7d34b959f06aa1b06de397658"],["/categories/数据结构和算法/算法题/动态规划/index.html","858db83584961d805517547a6cfafc98"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","27a9a7888db7ea5552f40e7a4a6de40a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b663fefb9b759b63031c9076488be183"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5e2bd76ce2ef71cae630d155e29048b9"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","feddf4d21666c00ab7ee45dbde76a953"],["/categories/数据结构和算法/算法题/栈和队列/index.html","69c5b38a7f9bc066bd7fca1477272387"],["/categories/数据结构和算法/算法题/树论/index.html","63c8c14634312c6a6585ed53575b1484"],["/categories/杂七杂八/index.html","4ffbaea4ac6d91cde7e1ad6803fb913f"],["/categories/杂七杂八/博客搭建/index.html","90947df8af0a67e48ba8c5657d836ced"],["/categories/编程工具下载/index.html","c9e1fba2b581211af484bbc0b5556a7d"],["/categories/编程环境/index.html","6bb8ef9864bef1318105645760c9932c"],["/categories/英语学习/index.html","4583cefcfbbeda828a2eb84471f5a537"],["/categories/英语学习/英语语法/index.html","ee38e6248f268d3338786386164d10ba"],["/comments/index.html","d5cc59cdb3a9a9788e1998d69b8043a0"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a1bdfc16a619e41497c113dc0c7bf31e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","507b72a2450346f83c55c0a61b395acc"],["/movies/index.html","9792a8159bd32b072e28cd6620f5aaf1"],["/music/index.html","9e55d45f0d95e0e451532a8a668f04d5"],["/page/2/index.html","c0f650c902ba878532498ab38e446e1d"],["/page/3/index.html","a49f3a64bcbc4e4081fe63cc47886e31"],["/page/4/index.html","4a1bff698cecfc4776c459d0cb122592"],["/page/5/index.html","5240317e7ac6d5aa92f1f81e980e4b1d"],["/page/6/index.html","a33e653faca48127c487d32d2e22d454"],["/posts/1021360842.html","8fb01e2504518448506b16c0f283f7ad"],["/posts/1120620192.html","42494cf068bfe4ccf252fbb40087e849"],["/posts/1141628095.html","584df6c2bb27a129fb6753076e998272"],["/posts/1168613674.html","87a871ac188beb7fac17474beae19b12"],["/posts/1219920510.html","d2bf3b78e521ec12b290f16738438275"],["/posts/1222166338.html","1bf0bf673d7ee00e59ea71832211e9aa"],["/posts/1259097482.html","d865e11b4ced1fa8fbf818f37d1595c8"],["/posts/1271036369.html","b16112df9b3596a2d02b31a85fa9b1dc"],["/posts/1312847445.html","46eab4799a7c9a2fed8b63f147fade09"],["/posts/135355774.html","4a770514b7955ecc8adf3c867d8d59c9"],["/posts/1375344716.html","07011d0ebfee59c0a42389b394188e88"],["/posts/1388991698.html","2c920fc68c1d93f69f7f19d33d11aebf"],["/posts/1410315814.html","155b9c32f6d18a79d58011a1d4e71521"],["/posts/1452790229.html","0cf8d698bd3a61cb0a1f0e6dcc30b407"],["/posts/1470079884.html","6b775b78f97b08e412c73755a920fdf2"],["/posts/1470079885.html","9722e09778d49a5e3a93ff85176bd4e0"],["/posts/1470079886.html","b6dee16fb35362fdb1a69ad9d6fdc5f0"],["/posts/1470079887.html","49dd048106e7642a69cf31811dff40be"],["/posts/1498536549.html","cc59fa8c312930c1fb6df13983285722"],["/posts/1547067935.html","a2e1f92f4633934e6f8b30ac9a302019"],["/posts/1557866301.html","d118482ff4c9f87ffa7d6e980e970b8f"],["/posts/1571776361.html","80b5b52643c10206e1225e4d7be7c4dc"],["/posts/1605124548.html","aabc5c4438d9ec5c34cd612adbec8aad"],["/posts/1633036852.html","0a066d6c38b9f912f2d016ba7c92c1cb"],["/posts/1765123828.html","6296f6500b0de24748c718d89f41e248"],["/posts/1767336200.html","8f5e8bc8175846e159e683172c6d23b3"],["/posts/1776114197.html","dbd2ab3eacdd478cfd982c5fa41fc51c"],["/posts/1817748743.html","a760112a978afc78aaa75447944c757b"],["/posts/1925125395.html","1171ccd5db48f173441f368486183060"],["/posts/1966191251.html","f76e66a674d6961c16c28c86aaff25f7"],["/posts/1987617322.html","b24e16ebdb66f82a9a706c767f2f8638"],["/posts/1999788039.html","849dd52c46ae2c8716c8278a3217a63b"],["/posts/2075104059.html","89fb0d54eb6e92d33d966be1525f8abd"],["/posts/2087796737.html","497e75edc919b46d8c74e93ab89f9d6f"],["/posts/2106547339.html","ec55431d46aee9be2e8e523acdb61dce"],["/posts/2207806286.html","fdbdb837187b2739c388d4010eecf899"],["/posts/2225903441.html","f195ac3d06c456059e57b2d67d50a783"],["/posts/2265610284.html","e1bc5c0b838c83a60b1943a6f4833b86"],["/posts/2281352001.html","931a626595bd8e83bc7c746be973237a"],["/posts/2364755265.html","25e2ab4815b2aa4f974e07a09131e754"],["/posts/2414116852.html","a761b1d5eff83e8684abc15efac8fb16"],["/posts/2421785022.html","2ee944bb0c5ebe0ee96169b0de4b2eb3"],["/posts/2482902029.html","0f41a9e57c084f3cc038a2b9c9d5a969"],["/posts/2495386210.html","108ad2eadf5f0eda14ccc60f271c3b86"],["/posts/2516528882.html","e8f30c3f2e19a714a54057d3310bbb69"],["/posts/2526659543.html","b54f6b2a8b986ea50bc8675f2d989dc1"],["/posts/2529807823.html","a8562f1ece2ae4f5df1db56d923bf7ff"],["/posts/2596601004.html","5c433429a1c6b74be58a75f1d47cb612"],["/posts/2742438348.html","98f737b86f6a02896e8c062da24cec8d"],["/posts/2888309600.html","c59bdf8709dd572b8dc7fbce5b7d8eaf"],["/posts/2891591958.html","c0f738a580d07c2752a78c5ca9e9df6e"],["/posts/2909934084.html","e19e0e8a8686fbe95fe470bb25abaf1b"],["/posts/2920256992.html","2d28e59fd6f7abb2e20b0d80a1d866bb"],["/posts/3005926051.html","2960b6301c1290d7bb8ca7cdc472bbe6"],["/posts/309775400.html","8cd554e355984bc21637e24d654faf80"],["/posts/3156194925.html","333e6599b87a5c334364ebd353af4e6c"],["/posts/3169224211.html","b329447d2e91b9f880189e7b45d4f58e"],["/posts/3213899550.html","08e371b3940f3c62946ca5887e2b660e"],["/posts/3259212833.html","3ba5a90bab422c01c2f73ddbaf93b422"],["/posts/3266130344.html","e0f127fe6eecfc5c53461f7af8fdd88b"],["/posts/3292663995.html","3fba38a57a8a1ca9eb4b79f8d6eec818"],["/posts/3297135020.html","feb0dd1a0439cfcd41a82192eab4a405"],["/posts/3306641566.html","5626ba3409d386c711e4e167d14d819b"],["/posts/3312011324.html","85026f84981cbe7fa93e604faa4c0071"],["/posts/336911618.html","e0eb7ef3f3df2a6b4cb68acfbe88f3bf"],["/posts/3402121571.html","69cd4b62b254d665c116c1ff97f4aa68"],["/posts/3405577485.html","f376d0df1c298c207cf1d3e31f96f9eb"],["/posts/3498516849.html","d82b9b0039c6889c89f375238c59e15d"],["/posts/3513711414.html","7f6b6fddd99b9d5f02c5ae268aea2fe0"],["/posts/3546711884.html","734669d1296a73eba68eac420cc468e4"],["/posts/3731385230.html","7a8a00f5076231a309d34249a76619ae"],["/posts/3772089482.html","a64db40eefbfed6778b4738748c573b4"],["/posts/386609427.html","e64e29f7eebbb3b9b07433833bb3d8ff"],["/posts/4044235327.html","89dc5ec20d2ccc3d98b7d0354c0216b7"],["/posts/4115971639.html","b2a1a5f3ea29ea8fd7b2cd3485ad0723"],["/posts/4130790367.html","6b48aa611355894498ed3af1dcbb5ce9"],["/posts/4131986683.html","9fe31ce52f0b3a4e69ff2f05b3abcbd5"],["/posts/4177218757.html","6cf1c3be7876638d50a372c5ca0e920f"],["/posts/4192183953.html","9a06068ab5b8ed4ee22a3abb806352de"],["/posts/4261103898.html","eb959276ddd9dea35cb42c41b7172db7"],["/posts/469711973.html","9d125c91c7efeeea9cf2505654768e58"],["/posts/482495853.html","735c7b9e6b1ddd575cbe2a1306d60981"],["/posts/488247922.html","c1d9824d4d511aae37d9528c61708bee"],["/posts/517302816.html","5e48590fee6dde7c34c9d099e57f8678"],["/posts/570165348.html","7bbfc8885ca964eed2f229542e9647af"],["/posts/595890772.html","11396e04ee328ec62cd14f1baaf4b708"],["/posts/67485572.html","23fa17e53226cd8621f65a01dbc275e2"],["/posts/694347442.html","30823accf0276f0228879261686cd8e6"],["/posts/707384687.html","71b53b65c73fb8260c41de5de4a8efe8"],["/posts/71180092.html","476110a1bf8f3dcaed8ff00b866ec13f"],["/posts/716459272.html","4ee80250b07d0bae8b862026249aa22c"],["/posts/778231993.html","96a2d5e2cd4cba01d52f0ffdc5c685e4"],["/posts/795397410.html","5870fd0fff81a24348bc78b568752f8d"],["/posts/820223701.html","b91e2c8bc9acb100c7694e1b82dee2b5"],["/posts/830372185.html","3ccd1d2f1e9c471ce158b6163ee5d544"],["/posts/88294277.html","219e6b5c1b29ba76463ec61997d92472"],["/posts/939963535.html","7653265797202de6a6f3029ab9a8777f"],["/posts/983786067.html","b3806a9291b84f022631c6fa1b5e9d43"],["/sw-register.js","f01b6246ba499ad58a97c08d9d58c6f7"],["/tags/C/index.html","2b6332ee3a9ce4e7b36ee9692ea40c7d"],["/tags/C/page/2/index.html","d36f5db9d424aceca5650b88242a62aa"],["/tags/C/page/3/index.html","9572c00e72dcb5ed5dd91ed147f32346"],["/tags/ElasticSearch/index.html","3a275fccf801e3f3dc2700f74705c5a8"],["/tags/GUI/index.html","b12d7ac46937668377e577e230f6499d"],["/tags/HBase/index.html","1934f13c13bd485033a36ecbdca1430d"],["/tags/Hadoop/index.html","2da074a862099a31f27c113552658d58"],["/tags/Hadoop/page/2/index.html","ea3a81ccc2e1e1ea08371c7f03c23cfb"],["/tags/Java/index.html","bfabf8dfd4e9baaed6ca0eb613bae7ec"],["/tags/Java后端/index.html","f9eacf6462fe88cc62fbd39adde3efcc"],["/tags/Java后端/page/2/index.html","6bfe0192704420b2f2f94515c4da47e0"],["/tags/Java基础/index.html","f9ceaf114035fbf0c84185c14f28da8f"],["/tags/Java基础/page/2/index.html","6fc20e5adf863ba12274ddd381b92b69"],["/tags/Kibana/index.html","cd4525063b32ba8a3d7cac626d2902a1"],["/tags/Linux/index.html","2a6418f3892e23a6655f5a7f6b8acdb3"],["/tags/Linux/page/2/index.html","bbe8ba5c0c84b4e22ab3dc0fd05545a8"],["/tags/Linux/page/3/index.html","27c4fc7ade205149be9830f428016084"],["/tags/Mac/index.html","a5e86bb3afce316edcdb84bc859ab30e"],["/tags/Mac/page/2/index.html","06bd86529c2f0840e0c8fee4f3116a3d"],["/tags/Maven/index.html","9693633a367c1a8f8c59c028000e234e"],["/tags/MySQL/index.html","b93c558caea0d43346f8e6da6a501106"],["/tags/Python/index.html","0e40c617bc5cebd755add1c3fbaae8bb"],["/tags/Redis/index.html","6a7bf448902f99ea4e497126f89cda3d"],["/tags/R语言/index.html","979bb96a649571703c00b98854eb859b"],["/tags/Spark/index.html","dcc48cae64c7b5ecee345eea6233e34f"],["/tags/Ubuntu/index.html","979add1296e59436b84d01ade8e6834f"],["/tags/Vue/index.html","d94e04c20aa676b870f276bc544465a2"],["/tags/Windows/index.html","bf8fe424e23fc80ae786c2b267883c76"],["/tags/ZooKeeper/index.html","3574c5d38c00134ea878125491aa7577"],["/tags/bfs/index.html","caa776153c1ef91372b559f42ab52b5b"],["/tags/dfs/index.html","fc56e0f2c6cb252774398a8cbded4557"],["/tags/folium/index.html","0d56ef6fb4e49d3d4981d7c0bf901d28"],["/tags/git/index.html","c6a24d2c7acdb461cd53fbdf4e305e63"],["/tags/index.html","9fea512a9dbd632686ce8bf91b26f578"],["/tags/latex/index.html","21b4f77a76c7e98212bbc48a2327328a"],["/tags/中间件/index.html","c0052e96736fa005f7ca641804f8c229"],["/tags/二分查找/index.html","295e5ed82458b68b035e0de46d4e98f2"],["/tags/优化类/index.html","dc45efb35a1505bb56c7b454c104c7d7"],["/tags/前端/index.html","d9ebcbffc95b59b31fe43607486d7650"],["/tags/前缀和与差分/index.html","ec357176bf9ffc9e506b54d610de7a55"],["/tags/动态规划/index.html","f8458297f4eba21907add8c929eaadac"],["/tags/动态规划/page/2/index.html","3d0a784986e3892dee1559306dbff6f6"],["/tags/博客搭建/index.html","160ae196ebd28b1541c26d44cb2badcc"],["/tags/图论/index.html","08f926eb986db8a4d13d209a738b1623"],["/tags/大数据/index.html","fb7c4f9b26e4d312b38d82ffca4f24b4"],["/tags/大数据/page/2/index.html","d582236185b3f7132b1ae2d012c62a7f"],["/tags/操作系统/index.html","12d45280b9b919dcfc12fa134294cd7d"],["/tags/数学建模/index.html","67fc6edef35784034f25faefb655d847"],["/tags/数据库/index.html","53aa1bbb8b20182dd2d915714803c5cc"],["/tags/数据结构和算法/index.html","9036da920139262fc0c387d92cfe547f"],["/tags/数据结构和算法/page/2/index.html","88ba11578f3f8739ec6425fc899851df"],["/tags/数据结构和算法/page/3/index.html","2a85740057ecdba461e9291e8cf7c13d"],["/tags/数组和字符串/index.html","62d6e494610ab4845b5a79d702b9d898"],["/tags/枚举类/index.html","6b13566ad0188dec211cb132a0e6a9db"],["/tags/栈和队列/index.html","3e3b9b43f227c751bf9b190d245e1670"],["/tags/树论/index.html","6ac260380952f25a91afa7d592869c33"],["/tags/测试/index.html","8de8b6ee3d0dfae2bf8ad2eeadc994f7"],["/tags/环境/index.html","3748fee17519f8f45be9d989ded19132"],["/tags/环境变量/index.html","06232013263b3da735ea532df83d600d"],["/tags/绘图/index.html","db207761b577379ea00e9d1bc2092566"],["/tags/编程工具/index.html","2975791bf9e09130299eeb47ebec1647"],["/tags/编程环境/index.html","f22bcb345c4b43e8d4440433397eae8f"],["/tags/网络编程/index.html","d8fdbfca991f842d3445aeff2b559777"],["/tags/英语语法/index.html","a35f235945121a6a252d5eb693c87702"],["/tags/论文/index.html","2223c10d554041691e25556630d2edb9"],["/tags/资源下载/index.html","c331fc32eae5b5bf429f0c1149dc2f95"],["/tags/链表/index.html","812f05d1c89cd511ac304bdda14d9e6d"],["/tags/集合/index.html","d65d6292ee20757fcd29ba41322a9e8b"],["/tags/集群/index.html","a0ab79b4dd1783f688387fe921cf6519"]];
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
