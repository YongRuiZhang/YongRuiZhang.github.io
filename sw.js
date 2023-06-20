/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","c20a5ecc645457fcc79d4dabcbe81dd7"],["/about/index.html","b662cb5b6c315aee4a49859b27a46ff7"],["/archives/2023/01/index.html","1987420faad4b7298ece2b65505d5519"],["/archives/2023/02/index.html","7aa7b0a635cc6c724bcf82254c4ae44f"],["/archives/2023/02/page/2/index.html","2c98a77fae85c442906e3b645fcbd1e9"],["/archives/2023/03/index.html","93ec351ceb13eaeabbfd5e21130e5ade"],["/archives/2023/05/index.html","d39a1ac659deae5b0560c0ef68c4e28b"],["/archives/2023/06/index.html","238585ade67934bd69ceb21bc2a44afc"],["/archives/2023/index.html","3235dce3df4757642c563062bf33b542"],["/archives/2023/page/2/index.html","ed1aa6fc17fc05703838020657b5980f"],["/archives/2023/page/3/index.html","d37fa4de8555dd630b1c550a04e44f4a"],["/archives/2023/page/4/index.html","c22b3872a10cfdcfc81cfe3bb9363c60"],["/archives/index.html","75f88d932e34e04cbd7af8b82662f9be"],["/archives/page/2/index.html","f82f22d153a10bf1d5513babc2ea036a"],["/archives/page/3/index.html","da81ceccca15a70e671c8a29b10fc50f"],["/archives/page/4/index.html","737b81ddd610b28ee91c143827cd51a4"],["/categories/Java/index.html","45ae2d217a2a663bdd6085697a0ce9d0"],["/categories/Java/后端/index.html","f7fa68fda99d3cc5ba76c3e2b7de8ec4"],["/categories/Java/基础/index.html","d9195b25ed1e176847c0f3e490375aa5"],["/categories/Java/基础/集合/index.html","06e96d42862d24e606373b2f5fe6722f"],["/categories/Python/index.html","281826d9f9c2bab926a9af9b51aef30c"],["/categories/Python/编程环境/index.html","f14664817660f9a384de6cb58ebd92db"],["/categories/R语言/index.html","580e51dec888640e8ac7ede215158484"],["/categories/R语言/编程环境/index.html","2cd3c058d2b4d3a9a82c027d6c949afe"],["/categories/index.html","5cd1eb367a190c325e2b98b17d02a60b"],["/categories/中间件/index.html","55262880d66c4af32ff42ebf81a6a0df"],["/categories/前端/Vue/index.html","e5bf3f65c73107a074126b28f728d1ba"],["/categories/前端/index.html","38a50bb551120a63b2a20dda76e12b85"],["/categories/大数据开发/ElasticSearch/index.html","53cebbf6dbddb6c5513ee28562e24a85"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","e165302c10b26e64e14d5b65e9189680"],["/categories/大数据开发/HBase/index.html","0d6912dc33642e7032ee41dd67818b3e"],["/categories/大数据开发/HBase/学习笔记/index.html","cf43ae2022eac46927e068c9d62dfb0f"],["/categories/大数据开发/HBase/环境搭建/index.html","2a4b7aa78f7d175404346e6da6cd8df7"],["/categories/大数据开发/Hadoop/index.html","5e6748c5b3d35539d1f6efce66452454"],["/categories/大数据开发/Hadoop/技术/index.html","594412b6448048121a3f88c767d10bed"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a6a8c06f24bc90ecf4889e987e1046c6"],["/categories/大数据开发/Redis/index.html","456d5d4f35ad48c06d9058eaab3fff45"],["/categories/大数据开发/Redis/技术/index.html","a9dc46fba6e77e3cf77ea244fd8fa130"],["/categories/大数据开发/Redis/环境搭建/index.html","8e6c2ee591230774c8e65b6b18294df6"],["/categories/大数据开发/Spark/index.html","47d169bd34001fbc2e3b40e8ac67efe8"],["/categories/大数据开发/Spark/环境搭建/index.html","dfec4a6b6186738a51aa4ff5e5571dcf"],["/categories/大数据开发/Zookeeper/index.html","c936207d73e39fe39944532afe5da268"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3ff198d7bc2b3dd97979994d0c559ee3"],["/categories/大数据开发/index.html","9152d0efb95bb1a74ffc4bd7a577a096"],["/categories/操作系统/Linux/index.html","aa88bc5cf4019d7d9f547d5f8bfe647e"],["/categories/操作系统/Mac/index.html","f57d1b7ade3ffdb3ed23b8d26bc430a3"],["/categories/操作系统/Windows/index.html","7ad83551766684401a3c1654f8f361bc"],["/categories/操作系统/index.html","278644582ab62eb4a07c8dca028f4cac"],["/categories/数学建模/index.html","ef4ec7261bf102345701b95188c28176"],["/categories/数学建模/latex/index.html","deec957c6e93a2578913896b5d4250f8"],["/categories/数学建模/优化类/index.html","d41e20630b3790bc845d0c7e228377de"],["/categories/数学建模/优化类/现代优化算法/index.html","297eadf8c2a0a41733adb109f56ad890"],["/categories/数学建模/优化类/规划类/index.html","19689c0aa991f0db181f843fa7e4c796"],["/categories/数学建模/绘图/index.html","0f195b36593d19aa3a56456d287b1cc7"],["/categories/数据库/MySQL/index.html","a3e91c3cc74913039eac41a306005a4f"],["/categories/数据库/index.html","88598e22158904b0e1713f05177656e8"],["/categories/数据结构和算法/index.html","9f961a5d37d67b95bbe14278c32ef457"],["/categories/数据结构和算法/page/2/index.html","8e39d7486f82108a458b5533cfdafce5"],["/categories/数据结构和算法/基本原理/bfs/index.html","4683077dde57f7b830daa08748b4a315"],["/categories/数据结构和算法/基本原理/dfs/index.html","909a616664b2d3c6f5c3ab99fa1cbc24"],["/categories/数据结构和算法/基本原理/index.html","2dca751d37801aa28ecf0ca67e719775"],["/categories/数据结构和算法/基本原理/动态规划/index.html","d4375966ecf0e9c69a3097018b95c65b"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","21e2c0bf550cc4a66592eefa9af25f6f"],["/categories/数据结构和算法/基本原理/图论/index.html","e8dadbc042a288087a52c8b5cce0cce2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b5dbdbffb352abbbefbb20982f372ccb"],["/categories/数据结构和算法/基本原理/数论/index.html","f1c5ec70b8d295495eb18e0694f6d995"],["/categories/数据结构和算法/基本原理/树论/index.html","c42d029d363e6dd3a839956e1e5b8841"],["/categories/数据结构和算法/基本原理/链表/index.html","8e246165fffe3331ecf78dc4f5f1f9bb"],["/categories/数据结构和算法/算法题/index.html","a41d65244c41ec26081ac8ee74739cd4"],["/categories/数据结构和算法/算法题/二分查找/index.html","9ffbfb1d028367a7ba55be574393c9ec"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","a708bca27e34ee4408f795e403fe2907"],["/categories/数据结构和算法/算法题/动态规划/index.html","88b6812aa1de0bca7f2d6401ddcf1edb"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e36f072773f50a9f76272779059a8a10"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","a1ef0c428b6bf42561427583f895390b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a0e462e919eb60b0f17c7d29980ddb9b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","10dcc50c4cdeeae6daf15d6eada39236"],["/categories/数据结构和算法/算法题/栈和队列/index.html","938e59166ec97063080ac8e8512e0370"],["/categories/数据结构和算法/算法题/树论/index.html","013fb9ab76a7c079b5e1ce42b957d089"],["/categories/杂七杂八/index.html","06c7f0525d9e37f8fe61137109d2a551"],["/categories/杂七杂八/博客搭建/index.html","4a98c1d72c2b5159d3cd7483b4b18c20"],["/categories/编程工具下载/index.html","2b4186908e7c4841f28fa5b5be93e53b"],["/categories/编程环境/index.html","8b0487e147b598b099a31747d7933023"],["/categories/英语学习/index.html","058997f6e7d69fb61e5a6fbb16347f3d"],["/categories/英语学习/英语语法/index.html","7957f75151e7a4e3b9ad9b62f96ea9b8"],["/comments/index.html","eebe6794029347d57036009c30b1d99c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","7d6157f712dd784975f63d5daa4bc47a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","464e078a8cc5e099bda02b02396a1474"],["/movies/index.html","4d2005fe6f07186b93ab9481f565ba34"],["/music/index.html","31ff6428df2294d10c131f486b129825"],["/page/2/index.html","dda78e196bf9530a5cef9db4994b96bf"],["/page/3/index.html","e435c14fa9f12441b9b7c6cf5ee6d5f5"],["/page/4/index.html","fbf5a0789ac3a6eec8c9669c8aab692e"],["/page/5/index.html","1337d2ef552ba669772f7c39ae06e310"],["/page/6/index.html","59158c4ca01a0e86c7f3aa2d920380dc"],["/posts/1021360842.html","8fb01e2504518448506b16c0f283f7ad"],["/posts/1120620192.html","42494cf068bfe4ccf252fbb40087e849"],["/posts/1141628095.html","584df6c2bb27a129fb6753076e998272"],["/posts/1168613674.html","87a871ac188beb7fac17474beae19b12"],["/posts/1219920510.html","d2bf3b78e521ec12b290f16738438275"],["/posts/1222166338.html","1bf0bf673d7ee00e59ea71832211e9aa"],["/posts/1259097482.html","d865e11b4ced1fa8fbf818f37d1595c8"],["/posts/1271036369.html","b16112df9b3596a2d02b31a85fa9b1dc"],["/posts/1312847445.html","46eab4799a7c9a2fed8b63f147fade09"],["/posts/135355774.html","4a770514b7955ecc8adf3c867d8d59c9"],["/posts/1375344716.html","07011d0ebfee59c0a42389b394188e88"],["/posts/1388991698.html","2c920fc68c1d93f69f7f19d33d11aebf"],["/posts/1410315814.html","155b9c32f6d18a79d58011a1d4e71521"],["/posts/1452790229.html","0cf8d698bd3a61cb0a1f0e6dcc30b407"],["/posts/1470079884.html","6b775b78f97b08e412c73755a920fdf2"],["/posts/1470079885.html","9722e09778d49a5e3a93ff85176bd4e0"],["/posts/1470079886.html","b6dee16fb35362fdb1a69ad9d6fdc5f0"],["/posts/1470079887.html","49dd048106e7642a69cf31811dff40be"],["/posts/1498536549.html","cc59fa8c312930c1fb6df13983285722"],["/posts/1547067935.html","a2e1f92f4633934e6f8b30ac9a302019"],["/posts/1557866301.html","d118482ff4c9f87ffa7d6e980e970b8f"],["/posts/1571776361.html","80b5b52643c10206e1225e4d7be7c4dc"],["/posts/1605124548.html","aabc5c4438d9ec5c34cd612adbec8aad"],["/posts/1633036852.html","0a066d6c38b9f912f2d016ba7c92c1cb"],["/posts/1765123828.html","6296f6500b0de24748c718d89f41e248"],["/posts/1767336200.html","8f5e8bc8175846e159e683172c6d23b3"],["/posts/1776114197.html","dbd2ab3eacdd478cfd982c5fa41fc51c"],["/posts/1817748743.html","2844d9b6e8c40075fce12351b646ce02"],["/posts/1925125395.html","1171ccd5db48f173441f368486183060"],["/posts/1966191251.html","f76e66a674d6961c16c28c86aaff25f7"],["/posts/1987617322.html","b24e16ebdb66f82a9a706c767f2f8638"],["/posts/1999788039.html","849dd52c46ae2c8716c8278a3217a63b"],["/posts/2075104059.html","89fb0d54eb6e92d33d966be1525f8abd"],["/posts/2087796737.html","497e75edc919b46d8c74e93ab89f9d6f"],["/posts/2106547339.html","ec55431d46aee9be2e8e523acdb61dce"],["/posts/2207806286.html","fdbdb837187b2739c388d4010eecf899"],["/posts/2225903441.html","f195ac3d06c456059e57b2d67d50a783"],["/posts/2265610284.html","e1bc5c0b838c83a60b1943a6f4833b86"],["/posts/2281352001.html","931a626595bd8e83bc7c746be973237a"],["/posts/2364755265.html","25e2ab4815b2aa4f974e07a09131e754"],["/posts/2414116852.html","a761b1d5eff83e8684abc15efac8fb16"],["/posts/2421785022.html","2ee944bb0c5ebe0ee96169b0de4b2eb3"],["/posts/2482902029.html","0f41a9e57c084f3cc038a2b9c9d5a969"],["/posts/2495386210.html","108ad2eadf5f0eda14ccc60f271c3b86"],["/posts/2516528882.html","e8f30c3f2e19a714a54057d3310bbb69"],["/posts/2526659543.html","b54f6b2a8b986ea50bc8675f2d989dc1"],["/posts/2529807823.html","a8562f1ece2ae4f5df1db56d923bf7ff"],["/posts/2596601004.html","5c433429a1c6b74be58a75f1d47cb612"],["/posts/2742438348.html","98f737b86f6a02896e8c062da24cec8d"],["/posts/2888309600.html","c59bdf8709dd572b8dc7fbce5b7d8eaf"],["/posts/2891591958.html","c0f738a580d07c2752a78c5ca9e9df6e"],["/posts/2909934084.html","e19e0e8a8686fbe95fe470bb25abaf1b"],["/posts/2920256992.html","2d28e59fd6f7abb2e20b0d80a1d866bb"],["/posts/3005926051.html","2960b6301c1290d7bb8ca7cdc472bbe6"],["/posts/309775400.html","8cd554e355984bc21637e24d654faf80"],["/posts/3156194925.html","333e6599b87a5c334364ebd353af4e6c"],["/posts/3169224211.html","b329447d2e91b9f880189e7b45d4f58e"],["/posts/3213899550.html","08e371b3940f3c62946ca5887e2b660e"],["/posts/3259212833.html","3ba5a90bab422c01c2f73ddbaf93b422"],["/posts/3266130344.html","e0f127fe6eecfc5c53461f7af8fdd88b"],["/posts/3292663995.html","3fba38a57a8a1ca9eb4b79f8d6eec818"],["/posts/3297135020.html","fe478628a46bbee9ae2fb844a54e78b5"],["/posts/3306641566.html","5626ba3409d386c711e4e167d14d819b"],["/posts/3312011324.html","85026f84981cbe7fa93e604faa4c0071"],["/posts/336911618.html","e0eb7ef3f3df2a6b4cb68acfbe88f3bf"],["/posts/3402121571.html","69cd4b62b254d665c116c1ff97f4aa68"],["/posts/3405577485.html","f376d0df1c298c207cf1d3e31f96f9eb"],["/posts/3498516849.html","d82b9b0039c6889c89f375238c59e15d"],["/posts/3513711414.html","7f6b6fddd99b9d5f02c5ae268aea2fe0"],["/posts/3546711884.html","734669d1296a73eba68eac420cc468e4"],["/posts/3731385230.html","7a8a00f5076231a309d34249a76619ae"],["/posts/3772089482.html","a64db40eefbfed6778b4738748c573b4"],["/posts/386609427.html","e64e29f7eebbb3b9b07433833bb3d8ff"],["/posts/4044235327.html","89dc5ec20d2ccc3d98b7d0354c0216b7"],["/posts/4115971639.html","b2a1a5f3ea29ea8fd7b2cd3485ad0723"],["/posts/4130790367.html","6b48aa611355894498ed3af1dcbb5ce9"],["/posts/4131986683.html","9fe31ce52f0b3a4e69ff2f05b3abcbd5"],["/posts/4177218757.html","6cf1c3be7876638d50a372c5ca0e920f"],["/posts/4192183953.html","9a06068ab5b8ed4ee22a3abb806352de"],["/posts/4261103898.html","eb959276ddd9dea35cb42c41b7172db7"],["/posts/469711973.html","9d125c91c7efeeea9cf2505654768e58"],["/posts/482495853.html","735c7b9e6b1ddd575cbe2a1306d60981"],["/posts/488247922.html","c1d9824d4d511aae37d9528c61708bee"],["/posts/517302816.html","70f1ebe2fdcf008571f9b82d007562f5"],["/posts/570165348.html","7bbfc8885ca964eed2f229542e9647af"],["/posts/595890772.html","11396e04ee328ec62cd14f1baaf4b708"],["/posts/67485572.html","23fa17e53226cd8621f65a01dbc275e2"],["/posts/694347442.html","30823accf0276f0228879261686cd8e6"],["/posts/707384687.html","71b53b65c73fb8260c41de5de4a8efe8"],["/posts/71180092.html","476110a1bf8f3dcaed8ff00b866ec13f"],["/posts/716459272.html","4ee80250b07d0bae8b862026249aa22c"],["/posts/778231993.html","96a2d5e2cd4cba01d52f0ffdc5c685e4"],["/posts/795397410.html","5870fd0fff81a24348bc78b568752f8d"],["/posts/820223701.html","b91e2c8bc9acb100c7694e1b82dee2b5"],["/posts/830372185.html","3ccd1d2f1e9c471ce158b6163ee5d544"],["/posts/88294277.html","219e6b5c1b29ba76463ec61997d92472"],["/posts/939963535.html","7653265797202de6a6f3029ab9a8777f"],["/posts/983786067.html","b3806a9291b84f022631c6fa1b5e9d43"],["/sw-register.js","c6de1f2d5b7d2d1875a4ee8740eefb69"],["/tags/C/index.html","a4387d7d77214fe87f08d3cb054cefaa"],["/tags/C/page/2/index.html","37bfddc18453ff69baa60705246fc29f"],["/tags/C/page/3/index.html","8cc8633107f4ea23ed49ce6d6f4302ad"],["/tags/ElasticSearch/index.html","ef7b66d1882c61867ff54498e1fabc7b"],["/tags/GUI/index.html","942afd6c6448c622d189501e7b9ae4b8"],["/tags/HBase/index.html","9238b1c058067c19ec788aaaa02ff564"],["/tags/Hadoop/index.html","ec2d4c4acb7f81c7a959c310af529b84"],["/tags/Hadoop/page/2/index.html","b133f1e68cc0f2ca4be73ae4cc92d45b"],["/tags/Java/index.html","59f0e9b794b231db5826910529539806"],["/tags/Java后端/index.html","0095e74a2db3d54370a0987efd1fabeb"],["/tags/Java后端/page/2/index.html","ea9c918304857d5df2b2b366d6fb8f2e"],["/tags/Java基础/index.html","a53306f28344630fe7ec46af71f57963"],["/tags/Java基础/page/2/index.html","d21fbf53f12f8f2c52bb3f8aabe14c54"],["/tags/Kibana/index.html","37c2560cccf1347a007f3d8572e70fb5"],["/tags/Linux/index.html","1dfd28f9cbb6f5058b7d83d6ebd67552"],["/tags/Linux/page/2/index.html","38b3ca6df6635a87a95ed90bf115651b"],["/tags/Linux/page/3/index.html","f6704c244fc31b2773439aba9d2a2149"],["/tags/Mac/index.html","75d7256a91eae7334217f1cd8695d111"],["/tags/Mac/page/2/index.html","6eaf7dced3549f70585610a86cdf25dc"],["/tags/Maven/index.html","50cb08db2d9f10570d48dc7bb9913952"],["/tags/MySQL/index.html","797285c0b064694bc34b30546ea406a5"],["/tags/Python/index.html","187935e7c89ba7c597c09cc8271b8b93"],["/tags/Redis/index.html","3ade5bb53e2bbcf47c56312c1d088d1c"],["/tags/R语言/index.html","b7b0e6e290c5dd5f758d17d2064893ba"],["/tags/Spark/index.html","10efd3244ee6685e42973a85e23ea9e7"],["/tags/Ubuntu/index.html","161903585fe40581459b8b3e2ca34132"],["/tags/Vue/index.html","98cb32e033e8cf9eb85e68d6291e6064"],["/tags/Windows/index.html","e248fcbbd4d84767f7fb39d3ed6011d1"],["/tags/ZooKeeper/index.html","3e770197fad62ab7ceaf8d9006556f18"],["/tags/bfs/index.html","40156f70a720f9d45399f5236edc0402"],["/tags/dfs/index.html","af41afdc9cc0767ae7fd56dc76f52743"],["/tags/folium/index.html","de91cd895f0858858f568cdf4a587041"],["/tags/git/index.html","67d928fa10c1a5d71723e3fa0d71b8bf"],["/tags/index.html","b17561434d51815acc6df113fedf6dca"],["/tags/latex/index.html","496c733ed80e836812e669b00e2246d7"],["/tags/中间件/index.html","9e1fd37174374dd8d40dcf35e63c0aa9"],["/tags/二分查找/index.html","51dda2aa742dc94a90536f49e5b32170"],["/tags/优化类/index.html","5000d428244a087fcf942d8b7ea20451"],["/tags/前端/index.html","5ae49b0036186dc1d2526ae30e052474"],["/tags/前缀和与差分/index.html","67366fa38a7f8b1b782dc901fa4edfdc"],["/tags/动态规划/index.html","49c9a360081a5b1f923342a1ac90570c"],["/tags/动态规划/page/2/index.html","cb4bf3ca6fc88eb8795d063b1fcaf495"],["/tags/博客搭建/index.html","6f2588f66c489550141d5a9c13321936"],["/tags/图论/index.html","fafb2abbf27628573718a04534090da5"],["/tags/大数据/index.html","8db61876896b54240271d9eb9cf903d4"],["/tags/大数据/page/2/index.html","14108b3b0a6ef30356960bbb70cd4e38"],["/tags/操作系统/index.html","f442c18d90262187885fd16e5958d729"],["/tags/数学建模/index.html","f120568878e3d9e23ebcfcd2033f6ebf"],["/tags/数据库/index.html","a375784e0bfcc49f74f6e5ff924ac6ee"],["/tags/数据结构和算法/index.html","222b27db25d2207f7ab1c1940c67c3b0"],["/tags/数据结构和算法/page/2/index.html","f02a90e8d3969689e0a3cfe225efaab7"],["/tags/数据结构和算法/page/3/index.html","84eb573e52a637ed0148cc31d6b12582"],["/tags/数组和字符串/index.html","44b51b69351dbe86bf180aa609515908"],["/tags/枚举类/index.html","98239b560023c2ef5f4b7839f6690c68"],["/tags/栈和队列/index.html","d86001f0953fefbff930450aacaee739"],["/tags/树论/index.html","abc6c20fb04cbc5dcdb0b455d4ce64a8"],["/tags/测试/index.html","f532d0e65c34864bb20ec553effa06b9"],["/tags/环境/index.html","6bf393e5040e83ea8eeb241792d3f20e"],["/tags/环境变量/index.html","a5513dac148a21614768ec664ab5e77f"],["/tags/绘图/index.html","259550ee4c8ea047615b4f93e1bd419a"],["/tags/编程工具/index.html","15d64d0067b9fd8996746a13a0c62032"],["/tags/编程环境/index.html","0db35967315db3af77149c3ba6b26035"],["/tags/网络编程/index.html","3360f05d12288a2bb0c5957d60963f5a"],["/tags/英语语法/index.html","9a2adfd634b1e23b824db9fea1bf28b6"],["/tags/论文/index.html","dbfbf4b612bee5a2d2c1fd8319aed680"],["/tags/资源下载/index.html","d737056064c522bd2c6a36e128534e12"],["/tags/链表/index.html","3608b85bb09dbbdc92153fb6027ccb31"],["/tags/集合/index.html","b88966b0d0a1cc2a4e5ac82a04908f7b"],["/tags/集群/index.html","621269f954b5784d0abd997c372f63cf"]];
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
