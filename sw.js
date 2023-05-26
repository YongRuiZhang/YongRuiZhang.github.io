/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","085e6379e202891004f6b44c1c304fba"],["/about/index.html","8d4a464b14e7e19612505bc0a572078f"],["/archives/2023/01/index.html","76fed8c19c0ec390f6c3b85dbf84e77d"],["/archives/2023/02/index.html","6198d8e606088a4beb278f13359637eb"],["/archives/2023/02/page/2/index.html","563764e689805b4caa9d892e148a7bf7"],["/archives/2023/03/index.html","c981f96ccb8222c698d736f7b8e1d0fa"],["/archives/2023/05/index.html","6176fd0f8003e892592fa60c52e79026"],["/archives/2023/index.html","e19affdde6cbb18bff8c15231b886068"],["/archives/2023/page/2/index.html","6a55cf41f8559ffaae98705ba4a17692"],["/archives/2023/page/3/index.html","bad87dcf690c0d56f58366b767729893"],["/archives/2023/page/4/index.html","8a57cd0d81534cf8a24eb7647a0acdcc"],["/archives/index.html","bcd2a02d3c5c9f6f422d99b7cdb5389d"],["/archives/page/2/index.html","dc3c564a5d8fdc6a8c69414372d0c867"],["/archives/page/3/index.html","9ced1062b67b8fb56bc3367f58ee6e88"],["/archives/page/4/index.html","3e9f72e046c115d445785e94a33693fb"],["/categories/Java/index.html","aab6940797bd258fec1243b933eec9bd"],["/categories/Java/后端/index.html","fbde735331efab259c5aa27163ccc2e5"],["/categories/Java/基础/index.html","28c0a33a3127e2913d622de86c3fcc2a"],["/categories/Java/基础/集合/index.html","095ab4b8fec6a9ff8e43fb0f3a86d9de"],["/categories/Python/index.html","6c8891f4fd68f117ca6f83ad024a05e9"],["/categories/Python/编程环境/index.html","dfbf73da65921c1dd4a153febf927d3a"],["/categories/R语言/index.html","a1c3c6e076ef4a2ffdebdbae26a7b44b"],["/categories/R语言/编程环境/index.html","5b4dab47419efd0c52e7fffcdb664b07"],["/categories/index.html","24cc0e549f0a27c6f7e190360f4dfa56"],["/categories/中间件/index.html","8e44af4773fa9827cb97e18ed7aee585"],["/categories/大数据开发/ElasticSearch/index.html","4c4a70a40486d41e3cb1d94bfc8faa74"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","72406279f8988e08dc948b0f806dce8d"],["/categories/大数据开发/HBase/index.html","fb8c9ae9907c3e82a58ef33f8287ec22"],["/categories/大数据开发/HBase/学习笔记/index.html","88f5dcd4ad35dc6850415217d3e00320"],["/categories/大数据开发/HBase/环境搭建/index.html","c05aafef01e4f14784fb4b6f4ff34f2b"],["/categories/大数据开发/Hadoop/index.html","56fb38d81fa4d0be92c63d04afc9c40b"],["/categories/大数据开发/Hadoop/技术/index.html","e191e51d83e343af236c9c32552177fc"],["/categories/大数据开发/Hadoop/环境搭建/index.html","01a18eb772089e539e2d2fab6dc5e5e8"],["/categories/大数据开发/Redis/index.html","7992081ca2c3723a990f0bdb9978ac4c"],["/categories/大数据开发/Redis/技术/index.html","c174f87f895b52a58fdd20c24b1d7f20"],["/categories/大数据开发/Redis/环境搭建/index.html","b68ba0c7dc47164e667a44591cbfd8d2"],["/categories/大数据开发/Zookeeper/index.html","9891e3ca17add1734bb1cc74b217265b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8ec4e0f75877756395b27fa567d6f74b"],["/categories/大数据开发/index.html","bb99b3be7e33cb7c4615c74815ef5a3f"],["/categories/操作系统/Linux/index.html","7dcf1ff25560ad1d2e799eecbb374fad"],["/categories/操作系统/Mac/index.html","cdc4f827fb0c7d427fd94ffc57405eee"],["/categories/操作系统/Windows/index.html","955da587b28bf807f1766aee90bc7ac9"],["/categories/操作系统/index.html","42885a8153617a44b11990e6f4f3e5b0"],["/categories/数学建模/index.html","13481f0fb5f4f72433390576b721bdbd"],["/categories/数学建模/latex/index.html","9f9a5b05d49362b30249a3458a779493"],["/categories/数学建模/优化类/index.html","59493c47174a16ea08b9c9d87cb661cc"],["/categories/数学建模/优化类/现代优化算法/index.html","2b63fda3e850353b725707af8e3e1f9f"],["/categories/数学建模/优化类/规划类/index.html","9eaba34ff3f5c36c30cf5ff9e13972a5"],["/categories/数学建模/绘图/index.html","726777c8ca021ef3f14e4fc16709ea75"],["/categories/数据库/MySQL/index.html","0f28a652b30643e7cfbdf3674ed6bb0a"],["/categories/数据库/index.html","2f54574d8e28c4a732daa90e0009e248"],["/categories/数据结构和算法/index.html","6cbda218cdad1513b12a878f65403337"],["/categories/数据结构和算法/page/2/index.html","6a78db14b38d900c99ae349b5df399ac"],["/categories/数据结构和算法/基本原理/bfs/index.html","acacf15fbe986bf40810ae74fa61ae24"],["/categories/数据结构和算法/基本原理/dfs/index.html","d1204a3deca61d48ad79217cff2d6f42"],["/categories/数据结构和算法/基本原理/index.html","d36294a6210ef43a90d148ec3b0d68ce"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b42254567bc68cc67672e3c81070c4f2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","40b987fb76c5366be758c83e73ec2bc3"],["/categories/数据结构和算法/基本原理/图论/index.html","baa094efef6b6904d0874a5456545fa5"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a243c393d650f008b9982f6c77ffb485"],["/categories/数据结构和算法/基本原理/数论/index.html","6795eb6f64b081770f3233bbd6a37a07"],["/categories/数据结构和算法/基本原理/树论/index.html","3def83755dc5fa824c24a3a9d38f9e79"],["/categories/数据结构和算法/基本原理/链表/index.html","9203712dd270c7734c705068f92c61d7"],["/categories/数据结构和算法/算法题/index.html","51031a15c56b659ee669e4f97bff3ef9"],["/categories/数据结构和算法/算法题/二分查找/index.html","8313ef2ce50992e0cec65e6b77d1f1f3"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","531cc54cf0a070f708210206918a6d87"],["/categories/数据结构和算法/算法题/动态规划/index.html","668a8c002a5320e0ca007ee38059ee68"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","614cb6687f93e8db6851cbbb0496ff82"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","207a62bc6ee3b595fae36bca0a701451"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","523a79f24c3343e9d10741b43503a5d7"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","454773c4c8de68e7e2eb8759457532d0"],["/categories/数据结构和算法/算法题/栈和队列/index.html","66b61136a4539a2d09ef482602688904"],["/categories/数据结构和算法/算法题/树论/index.html","dc07148261445351e0980c2b6c691f44"],["/categories/杂七杂八/index.html","4a130a28cd8ef68a32cf613a2c8f73a1"],["/categories/杂七杂八/博客搭建/index.html","6d7bd47a3ab088b26f678ef5ec7a159e"],["/categories/编程环境/index.html","fe9e116c6bebd65b2c8c439ed22d5d89"],["/categories/英语学习/index.html","78b08a32906621d64897815656a18f77"],["/categories/英语学习/英语语法/index.html","0b74c11db8458399fc8869b4da0205f4"],["/comments/index.html","459a6cd9ad35c01de68c6e7b8e0b37de"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","28988451f98d2bbb2abeb619376c3a05"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","0b2404fd7ec3b9ab95726eb5190b5954"],["/movies/index.html","34278bc707d544e5ab89a2f588bdb5c2"],["/music/index.html","650eb159b35fd292446e1e22bcaac580"],["/page/2/index.html","b771b9e1db8127bd90ab2779815b7f75"],["/page/3/index.html","d9f6b9554903de5773ba8f80192adf31"],["/page/4/index.html","2d6c8f0adc1bdaa732de88258905dbc1"],["/page/5/index.html","700a91857600acb0eecda796686b095f"],["/page/6/index.html","14370465309e88f8055d000e60eaef95"],["/posts/1021360842.html","2fec21aad4870c97412cc662a33341b0"],["/posts/1120620192.html","b86263c3fd12510eb7b525353d125acd"],["/posts/1141628095.html","6e14f225a5308cdeb2cac000537b458a"],["/posts/1168613674.html","231ce329d77de4ecde082b4323ffeed4"],["/posts/1219920510.html","95f28ac2993b3e7b33fcd2dfa9b2ca8f"],["/posts/1222166338.html","8b2cdb4602ccff90c4aeb5776907fddf"],["/posts/1259097482.html","9f7def62d19eead741310255cf208f0d"],["/posts/1271036369.html","78f48d25868a16cc91428e1f2e4947b9"],["/posts/1312847445.html","09611d0b89c2c8f21bfcc36d7de7aa38"],["/posts/135355774.html","fab00f8855a75c30382ad1ba489e0e29"],["/posts/1375344716.html","9088011c215903ed18fe43c479f5b5c9"],["/posts/1388991698.html","0957dd2780e98d180f8d23f0a0a2d414"],["/posts/1410315814.html","ad68c1cbcd5ac436341206104259265d"],["/posts/1452790229.html","e71ad45467a4d12d0df4ca3b757558db"],["/posts/1470079884.html","3550db57a30ac0530b82880b867046f2"],["/posts/1470079885.html","4e1c1a6a2c0b3708ace02f52cd99e1ef"],["/posts/1470079886.html","0621fb26fbec01a6a5b0405f2bee195f"],["/posts/1470079887.html","ea464ed8245f1b10984f9a854310cc42"],["/posts/1498536549.html","42fa3bb6b408809d91924f0a2b523611"],["/posts/1547067935.html","0c2be5e475ac6e40b9515e430d471778"],["/posts/1557866301.html","86da82764cc02b03cded18072220675c"],["/posts/1571776361.html","51564a4551398ce3477c8d800c821cad"],["/posts/1605124548.html","8a1c7dd1fe313811dc3f68346f47828d"],["/posts/1633036852.html","408c93daf366bcacdb414881a0f57d1c"],["/posts/1765123828.html","cc661f09449d4852d39514bb54e699e4"],["/posts/1767336200.html","ba174155f415fa0a4a23c352b71c66e6"],["/posts/1776114197.html","9800d9f5ffdddb531ccdf82d4a5739b0"],["/posts/1817748743.html","e49b76148ab89cac5c2b557ef03708af"],["/posts/1925125395.html","bb7f38af6c3bd49acd24a5eea6e5ed55"],["/posts/1966191251.html","bd253a25d5b35b64d78b482d177c3673"],["/posts/1987617322.html","68c9058c1fdf78a34908a926921e584d"],["/posts/1999788039.html","ce2565628198d2bc6e42be96dd877388"],["/posts/2075104059.html","025b2a3fbff3e3636e4829366df99990"],["/posts/2087796737.html","d84750486d4301f18f75c070d617fbd3"],["/posts/2106547339.html","8a1a18a88e264388ad4b61f9687e4c3c"],["/posts/2207806286.html","29f59286ab9d44009cfd4ac9e99c0b5d"],["/posts/2225903441.html","ddcf76c5a0836114b33be815caaf6a7b"],["/posts/2265610284.html","ca8e620dd1df459825c5500c07911a4f"],["/posts/2281352001.html","5ec23b3a71e83c3aa1c82aed86f903be"],["/posts/2364755265.html","c579e9f155bfa1197108621ab0c570e0"],["/posts/2414116852.html","276d0c1980cbf8024ec97ec06c1dc474"],["/posts/2482902029.html","b1f3cdcc494e8664ea78ed2dd002adae"],["/posts/2495386210.html","c316ae6fd49a987168629bdb9f7d4574"],["/posts/2516528882.html","71794c2b4c942c122f6aee861bf10df1"],["/posts/2526659543.html","914b7c08cbab0a241a34180c4fa4c728"],["/posts/2529807823.html","dffa1a5a02049adfda04f2492029d796"],["/posts/2742438348.html","1de3a8dbbc7176a4543887de3132e86a"],["/posts/2888309600.html","695802aea68264a7bbe1ffd70674d8b9"],["/posts/2891591958.html","853f93862047406acaeca1b41a8be078"],["/posts/2909934084.html","2a9c8bb65301d335a8d6bdfb3a7b7891"],["/posts/2920256992.html","b2edb6fa4322bd4f7aed8e7cddec021a"],["/posts/3005926051.html","d0c6f9b69d661461072cadbf6693b3c9"],["/posts/309775400.html","5596b4dc2dcc29922aa7aae851c03933"],["/posts/3156194925.html","46132a0c9b934e54b37639e870a804a0"],["/posts/3169224211.html","f4b27eb9e4a09e8a73585d94c9fe7c9d"],["/posts/3213899550.html","b357567dec4eeda76d6f015c9c57c8d3"],["/posts/3259212833.html","fd01d833db375313ac935e03bb3026ca"],["/posts/3266130344.html","63344ac4bae63e0dd7d8b7366859ac94"],["/posts/3297135020.html","376905cc461392f7d7070b559fb21b13"],["/posts/3306641566.html","6871e502076df241f58ce1a8af39029b"],["/posts/3312011324.html","f85351b3b18c2910b9a86010493296af"],["/posts/336911618.html","741a4c3a13c5b9ab3a6cba6a4700480a"],["/posts/3402121571.html","eff83fb82a50cfdc7f96dfb480cb8f64"],["/posts/3405577485.html","2ef5b417aae31fa26b5cca7c8f2e4946"],["/posts/3498516849.html","931fd62dc7c7f3aa02df4f1358104511"],["/posts/3513711414.html","ca6a0541c762f3ef32d6e2459f674973"],["/posts/3546711884.html","d9ac1c2ef650ed1fbd41872cbc04bacd"],["/posts/3731385230.html","d07578c30bcaa44e7e1ca9a88591a997"],["/posts/3772089482.html","f2819e606b6ac19c65cba9163ef91b79"],["/posts/386609427.html","fc65c40632a59e096835bd2ff002ec28"],["/posts/4044235327.html","02b8d5c3acb3e8c2b22ce6739a508ed6"],["/posts/4115971639.html","6a801c19a782f112cbf72b92abb71096"],["/posts/4130790367.html","d2a71e6320ca046e0200386a7b114607"],["/posts/4131986683.html","300af83ab9ade3ec5b8f3b6d11cf2f07"],["/posts/4177218757.html","abc859c17c43f18a54ec1e8c5d932708"],["/posts/4192183953.html","e9b77b1ab2bf0c82d99bc132ea1abc9b"],["/posts/4261103898.html","3517bd4f1a85f28eeae107e949517581"],["/posts/469711973.html","efcae2d77aa9cc4adfab9dace8d0bb88"],["/posts/482495853.html","ec3f74efcb75bce67d9b5db9bb8ab03b"],["/posts/488247922.html","d9b4f8d90cf462342940625292740fec"],["/posts/570165348.html","4864a2b2466935cdd2db57c8caf85113"],["/posts/595890772.html","43f4e23b1bb27fcc553b40f766043570"],["/posts/694347442.html","73c85b92a24071a039b672d5815fecbe"],["/posts/707384687.html","0b2c644d99962c596fdb0fb5dfacdf72"],["/posts/71180092.html","36fa5d0f9c57b9ea79b5ee4856d394fc"],["/posts/716459272.html","a16608fa399209671d3086a38637f9f3"],["/posts/778231993.html","dcce80c93cbca730747eb8457e7727cc"],["/posts/795397410.html","8410267098ace1a6792d39274eeac70e"],["/posts/820223701.html","3917ff2cb9fc779df8b854fd413fd3af"],["/posts/830372185.html","d199ad056e802c686709806503e1796a"],["/posts/88294277.html","b6f226029cb0f223cad0f4fcb046d248"],["/posts/939963535.html","c5f4fa891e2a32b47304c3948dcf5197"],["/posts/983786067.html","06f20a6cf6b4ce737774ada2def57409"],["/sw-register.js","d9a36cf897ca3870bb95147670c6e351"],["/tags/C/index.html","caf848847d9037f1758a697a9b875081"],["/tags/C/page/2/index.html","c1765699bff4fd36faaf5e484526c4e4"],["/tags/C/page/3/index.html","332930f9d1099417ec5502c99cde52b8"],["/tags/ElasticSearch/index.html","692f36e262df5eea3a7af48a3f75d521"],["/tags/GUI/index.html","203417afcdc50039a5f1659d1f0c70fe"],["/tags/HBase/index.html","598b6eae809ef09c021522f99ff7a6bd"],["/tags/Hadoop/index.html","df988d8425907ca3f72f62299f155c4c"],["/tags/Hadoop/page/2/index.html","efaf6e9f30c0e33c29df500caae5f0b0"],["/tags/Java/index.html","3af487582744ee417166d639ef913331"],["/tags/Java后端/index.html","a684796e0f7c148ebce5ecf7b6287dd2"],["/tags/Java后端/page/2/index.html","404ed15fce59e485a0c1a20da538b741"],["/tags/Java基础/index.html","6860d0f5bc252f07878d7f7518efc57a"],["/tags/Java基础/page/2/index.html","8dc0947e1e83cc1347d57c34932a2385"],["/tags/Kibana/index.html","83f14bf13b5a9220972bc91d1e34ed7a"],["/tags/Linux/index.html","d9d8824ba599711b72c393cdc3610efa"],["/tags/Linux/page/2/index.html","a56eb6bddc6741d75119373e22a91774"],["/tags/Linux/page/3/index.html","51e25ca495442d5906c68ed78b28134f"],["/tags/Mac/index.html","378a6a99cd2f4702afaa2dc74729b0e8"],["/tags/Mac/page/2/index.html","ab6e29f45aed1ab77e1602fed51b0123"],["/tags/Maven/index.html","8fbdf23667674876ca3eafd70137ee4c"],["/tags/MySQL/index.html","a953dd433594fe0635b637b851232363"],["/tags/Python/index.html","db48619a177e874d5f4d1715aef90db6"],["/tags/Redis/index.html","edee5f41a414211024628a0113b252fb"],["/tags/R语言/index.html","77aee8ab9adce4071650f8c2912f80ba"],["/tags/Ubuntu/index.html","ed52f49f5afeb7303c421841fa528025"],["/tags/Windows/index.html","f59969faf2aeb7900142000077c514c4"],["/tags/ZooKeeper/index.html","e93daac622671fd708003ea20a820636"],["/tags/bfs/index.html","67d6921d59e1d7c35c55f3b6ff127451"],["/tags/dfs/index.html","6ba506a4207376678f5ebcd83189803d"],["/tags/folium/index.html","45d19a8dd764c633a82cb42cfba2a981"],["/tags/git/index.html","28c37693d984e3c20bef67c90f0b2831"],["/tags/index.html","581cf2a2c8d1c0a26d20d401960e50ae"],["/tags/latex/index.html","1a579df80aa97c53d6f569015b1a9a6d"],["/tags/中间件/index.html","94c90ef030c7f802540dfd5061487be5"],["/tags/二分查找/index.html","504ea6955918bec7f404f79712c8d9e9"],["/tags/优化类/index.html","d793d9b4202bb34b911ab005915cdd1a"],["/tags/前缀和与差分/index.html","37cf5ae4d466834d058f11c729995b0f"],["/tags/动态规划/index.html","89b847b5fbfa084ccb190578ca66bb46"],["/tags/动态规划/page/2/index.html","8a7fa54df842ec3945a15e2f1ac4fbbe"],["/tags/博客搭建/index.html","b92ce77c57cabf68c3321ed62a6bcd96"],["/tags/图论/index.html","4e28b752ecf768d7ca549b766c9cd053"],["/tags/大数据/index.html","3bea9c0891e48eb3f4d193db2c6047cb"],["/tags/大数据/page/2/index.html","9f9b8edc1f2b97dad1dbe8abb2ad78af"],["/tags/操作系统/index.html","71b315c115042c43513ba40f212d2858"],["/tags/数学建模/index.html","6020d84a63213f03cafb2ecfe1f48c20"],["/tags/数据库/index.html","1eaadce169082c6ffaddafbf2cdc3389"],["/tags/数据结构和算法/index.html","7ab7e71298f5dce241f1df32a5c1e806"],["/tags/数据结构和算法/page/2/index.html","12b41836192355575c4568291d8097fd"],["/tags/数据结构和算法/page/3/index.html","203985f03594a51cfd262837fdf46c49"],["/tags/数组和字符串/index.html","1b3fbb5091324c047051625477df0d28"],["/tags/枚举类/index.html","91692e1b2a00cff29005660276864017"],["/tags/栈和队列/index.html","ed75f150a2f759f2061169018dbff6e1"],["/tags/树论/index.html","4b17854a9d1b6b533630ff82d1d94834"],["/tags/测试/index.html","38a5abf229ea394642012252d3c78ccf"],["/tags/环境/index.html","c9b52bc9a078c541a6c681647c0edf18"],["/tags/环境变量/index.html","efdfbedfbe04ff35d11d27df7f36077b"],["/tags/绘图/index.html","65fbf87946c61e75089b1829b4c04ce7"],["/tags/编程环境/index.html","700c015c8f29dbaf5381942970386d1e"],["/tags/网络编程/index.html","6764493b59bb3aabd7714ff18a5d13f6"],["/tags/英语语法/index.html","6931398eadac99bab4176802bd38d0ee"],["/tags/论文/index.html","121702edcd28100c8995a3964ead5f38"],["/tags/资源下载/index.html","386da84fb123d28f8ca9f2f6bd540a85"],["/tags/链表/index.html","2280de5529580cf34bcab36bfe6aa3dc"],["/tags/集合/index.html","c27a38abf40be5c0efae2e09ac9ead58"],["/tags/集群/index.html","e72c30f4d3248dedbca447139030b6f1"]];
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
