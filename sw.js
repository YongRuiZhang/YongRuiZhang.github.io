/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","a1814df371a2f067acee16ac67bda141"],["/about/index.html","8d4a464b14e7e19612505bc0a572078f"],["/archives/2023/01/index.html","0230cf2e0ddfd99a712efa9b617097f3"],["/archives/2023/02/index.html","2ef4f0c3d894cf27b4ebf420e7db7c95"],["/archives/2023/02/page/2/index.html","3d7a87732a4423b0f5864aead86debac"],["/archives/2023/03/index.html","d855735935c58200be5f51d6901155c1"],["/archives/2023/05/index.html","070150aad184badc9a567bc9161c0b4a"],["/archives/2023/index.html","64bc7a8411d7dd4fa3ddf099c93d6c71"],["/archives/2023/page/2/index.html","c883299646ba437d489e82e5e060b7c7"],["/archives/2023/page/3/index.html","cc2e48a46e8ee462e562726c8638cc90"],["/archives/2023/page/4/index.html","321fbf31fbb8f7a4aaad318caf0fe6f9"],["/archives/index.html","279a60f9a54c49bf35858acf735b7ec1"],["/archives/page/2/index.html","7839c3a045e30361bad5415d2914a014"],["/archives/page/3/index.html","f35ceedb393cb3e8dca5ef2be18a89d4"],["/archives/page/4/index.html","f62e7fc9a24fc4a3ba4bc7e425daddeb"],["/categories/Java/index.html","1be3859ed96c240cedb762e6dc026c25"],["/categories/Java/后端/index.html","f4068c21e6f6fc459716a75de65932ee"],["/categories/Java/基础/index.html","065f1dc6006c8a6ab80e6c1c73ca6654"],["/categories/Java/基础/集合/index.html","de3276c3b8db22959f16003c73ed830e"],["/categories/Python/index.html","aa7d8993af2fa1187fcc20f60b598541"],["/categories/Python/编程环境/index.html","75a8b3fe262354c6b24e3cfe599e7803"],["/categories/R语言/index.html","4a4c86085b2fc94fc833d96cd32292f3"],["/categories/R语言/编程环境/index.html","2ba2dcd21f71af90f2ef132ae9309be9"],["/categories/index.html","24cc0e549f0a27c6f7e190360f4dfa56"],["/categories/中间件/index.html","89b9821a05b5867da1f58d354ddf6c1e"],["/categories/大数据开发/ElasticSearch/index.html","ad3328f4fd67c39b542c09816b9abada"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","f58634d5a5bacf2e31c94aed0a769b86"],["/categories/大数据开发/HBase/index.html","5bc13ef672429b007f4a4c910e5aeeaa"],["/categories/大数据开发/HBase/学习笔记/index.html","47c6702a91e33d9952f8cbfc1fb335a1"],["/categories/大数据开发/HBase/环境搭建/index.html","009e7d710edd63c11cb56d8e7449461a"],["/categories/大数据开发/Hadoop/index.html","e284d4defdb3a5bae5235bf60dbd2937"],["/categories/大数据开发/Hadoop/技术/index.html","ce9670a4f4199950d9bcfdf8adf7a365"],["/categories/大数据开发/Hadoop/环境搭建/index.html","8377658ee6048eddbc7a65c3c4bedad1"],["/categories/大数据开发/Redis/index.html","9c79d5fbf3849332cc0ea118c43bbf0c"],["/categories/大数据开发/Redis/技术/index.html","63421caa496b89f15bd55ee376700c9f"],["/categories/大数据开发/Redis/环境搭建/index.html","31bb9dabb68f17a7e980d9a8a1a575a9"],["/categories/大数据开发/Zookeeper/index.html","236a8db011157709aa41716a205bb246"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e96ccfb01391beaaed78a601088109c8"],["/categories/大数据开发/index.html","7a7f86412e1c774008275ba89df3a679"],["/categories/操作系统/Linux/index.html","04e6ee52685b16e34c871065bcd4c7f8"],["/categories/操作系统/Mac/index.html","e0a3ed244c99909a9f8876b6c6ac1480"],["/categories/操作系统/Windows/index.html","c0f5e52e28ce371991b363148c64f732"],["/categories/操作系统/index.html","1dc42fd10fdccfce3b0a8bf29b05f8ef"],["/categories/数学建模/index.html","072e915ef6d9d3087bcd40e0cfba5745"],["/categories/数学建模/latex/index.html","25fd014acdda96f79ab3c75d8be4d58d"],["/categories/数学建模/优化类/index.html","494fd0374e0263e8a497ff57951335e9"],["/categories/数学建模/优化类/现代优化算法/index.html","c70b4c7bf2b5db70e767886e55502691"],["/categories/数学建模/优化类/规划类/index.html","c9baaf3262dde93b372cbbdde3da4a8a"],["/categories/数学建模/绘图/index.html","f3046173b7f8d2dd47b412e987511b7d"],["/categories/数据库/MySQL/index.html","06dc72c4b39109010abb415bd4199de5"],["/categories/数据库/index.html","e4f3595208ee796819066c41060d9e49"],["/categories/数据结构和算法/index.html","5277f7dd2732dbfd8e4886983d1f146a"],["/categories/数据结构和算法/page/2/index.html","d7df4643bdd3bcb44eb50bcc627a2c1e"],["/categories/数据结构和算法/基本原理/bfs/index.html","a8ef26832ef2f8312164e87239c01147"],["/categories/数据结构和算法/基本原理/dfs/index.html","cdc31e8d6e764d221a77d6a1ba628eae"],["/categories/数据结构和算法/基本原理/index.html","9bbd0d7679da4494ee2a0c3b3fd459ba"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b8227f9dd55b1b949c095ec38a493138"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4d517e9c9dfc95303a3fdbd3f9f6ab4c"],["/categories/数据结构和算法/基本原理/图论/index.html","267e4255100d8cf18defdb38b77f373d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","7552e088afbd6482d59a5cfe1c1a27a9"],["/categories/数据结构和算法/基本原理/数论/index.html","e74f8101d3c646c5d12f89305f5446f4"],["/categories/数据结构和算法/基本原理/树论/index.html","112954107331105b4cae49b438fa5028"],["/categories/数据结构和算法/基本原理/链表/index.html","73d7146900c0f91922c3899f66528423"],["/categories/数据结构和算法/算法题/index.html","9c20b145e05a5e98e09991e0a455e376"],["/categories/数据结构和算法/算法题/二分查找/index.html","7370d64428dceccd063a4678d999b6e7"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","fab358f838fb6e1bdd45bed9a6f7dae9"],["/categories/数据结构和算法/算法题/动态规划/index.html","355c5468db1976914d2fabfc3db182ad"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","16aec4f3f31af48ccd567c0a473583a7"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","789c9d6dcc75913f642b4f7e80e9fc84"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5ca50ef5fb336b3355b19f4ed5aab09d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","78609f2174a11d25f52207a7b9ea7fcf"],["/categories/数据结构和算法/算法题/栈和队列/index.html","4859b7ab1a12352a7a448db092a317f7"],["/categories/数据结构和算法/算法题/树论/index.html","2c392d1c09f7fe01af5fcfcc31baff9d"],["/categories/杂七杂八/index.html","801f77ede1641175e104e4a901f636a6"],["/categories/杂七杂八/博客搭建/index.html","6f2903c64e8613cb92a52f34f8a83819"],["/categories/编程环境/index.html","f34f7d7674295b16b586f20cf5211814"],["/categories/英语学习/index.html","0745bfc51af43b8ab330e98f060cd17d"],["/categories/英语学习/英语语法/index.html","b0cae7ae3295ade06975dcbe8ae3aee8"],["/comments/index.html","02eeb7c3c04521c030cde9a54f7e642f"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","3bcb2485757b1deda49735afec38dbac"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","fbfd2aff9b73029e156aeef905043567"],["/movies/index.html","c4403ecfeaefadb583e39750f98aeaa9"],["/music/index.html","a749a1177aab900b5f0b4a88ebdf9a3f"],["/page/2/index.html","d90b42a727a3400b3d5662c81ad9dca7"],["/page/3/index.html","94625fc0b280db750dc7e8dfff23a240"],["/page/4/index.html","8bac59819a20c1d9e2e1834bec03aa86"],["/page/5/index.html","726d8f0413972f7f1b91c761b1c9bc2e"],["/page/6/index.html","26732cd731177656469cabb7dfc4cfc0"],["/posts/1021360842.html","2fec21aad4870c97412cc662a33341b0"],["/posts/1120620192.html","b86263c3fd12510eb7b525353d125acd"],["/posts/1141628095.html","6e14f225a5308cdeb2cac000537b458a"],["/posts/1168613674.html","231ce329d77de4ecde082b4323ffeed4"],["/posts/1219920510.html","95f28ac2993b3e7b33fcd2dfa9b2ca8f"],["/posts/1222166338.html","8b2cdb4602ccff90c4aeb5776907fddf"],["/posts/1259097482.html","9f7def62d19eead741310255cf208f0d"],["/posts/1271036369.html","78f48d25868a16cc91428e1f2e4947b9"],["/posts/1312847445.html","09611d0b89c2c8f21bfcc36d7de7aa38"],["/posts/135355774.html","fab00f8855a75c30382ad1ba489e0e29"],["/posts/1375344716.html","9088011c215903ed18fe43c479f5b5c9"],["/posts/1388991698.html","0957dd2780e98d180f8d23f0a0a2d414"],["/posts/1410315814.html","ad68c1cbcd5ac436341206104259265d"],["/posts/1452790229.html","e71ad45467a4d12d0df4ca3b757558db"],["/posts/1470079884.html","3550db57a30ac0530b82880b867046f2"],["/posts/1470079885.html","4e1c1a6a2c0b3708ace02f52cd99e1ef"],["/posts/1470079886.html","0621fb26fbec01a6a5b0405f2bee195f"],["/posts/1470079887.html","ea464ed8245f1b10984f9a854310cc42"],["/posts/1498536549.html","42fa3bb6b408809d91924f0a2b523611"],["/posts/1547067935.html","0c2be5e475ac6e40b9515e430d471778"],["/posts/1557866301.html","86da82764cc02b03cded18072220675c"],["/posts/1571776361.html","51564a4551398ce3477c8d800c821cad"],["/posts/1605124548.html","8a1c7dd1fe313811dc3f68346f47828d"],["/posts/1633036852.html","408c93daf366bcacdb414881a0f57d1c"],["/posts/1765123828.html","cc661f09449d4852d39514bb54e699e4"],["/posts/1767336200.html","ba174155f415fa0a4a23c352b71c66e6"],["/posts/1776114197.html","9800d9f5ffdddb531ccdf82d4a5739b0"],["/posts/1817748743.html","e49b76148ab89cac5c2b557ef03708af"],["/posts/1925125395.html","bb7f38af6c3bd49acd24a5eea6e5ed55"],["/posts/1966191251.html","bd253a25d5b35b64d78b482d177c3673"],["/posts/1987617322.html","68c9058c1fdf78a34908a926921e584d"],["/posts/1999788039.html","ce2565628198d2bc6e42be96dd877388"],["/posts/2075104059.html","025b2a3fbff3e3636e4829366df99990"],["/posts/2087796737.html","d84750486d4301f18f75c070d617fbd3"],["/posts/2106547339.html","8a1a18a88e264388ad4b61f9687e4c3c"],["/posts/2207806286.html","29f59286ab9d44009cfd4ac9e99c0b5d"],["/posts/2225903441.html","ddcf76c5a0836114b33be815caaf6a7b"],["/posts/2265610284.html","ca8e620dd1df459825c5500c07911a4f"],["/posts/2281352001.html","5ec23b3a71e83c3aa1c82aed86f903be"],["/posts/2364755265.html","c579e9f155bfa1197108621ab0c570e0"],["/posts/2414116852.html","276d0c1980cbf8024ec97ec06c1dc474"],["/posts/2482902029.html","b1f3cdcc494e8664ea78ed2dd002adae"],["/posts/2495386210.html","c316ae6fd49a987168629bdb9f7d4574"],["/posts/2516528882.html","71794c2b4c942c122f6aee861bf10df1"],["/posts/2526659543.html","914b7c08cbab0a241a34180c4fa4c728"],["/posts/2529807823.html","dffa1a5a02049adfda04f2492029d796"],["/posts/2742438348.html","1de3a8dbbc7176a4543887de3132e86a"],["/posts/2888309600.html","695802aea68264a7bbe1ffd70674d8b9"],["/posts/2891591958.html","853f93862047406acaeca1b41a8be078"],["/posts/2909934084.html","2a9c8bb65301d335a8d6bdfb3a7b7891"],["/posts/2920256992.html","b2edb6fa4322bd4f7aed8e7cddec021a"],["/posts/3005926051.html","d0c6f9b69d661461072cadbf6693b3c9"],["/posts/309775400.html","5596b4dc2dcc29922aa7aae851c03933"],["/posts/3156194925.html","46132a0c9b934e54b37639e870a804a0"],["/posts/3169224211.html","f4b27eb9e4a09e8a73585d94c9fe7c9d"],["/posts/3213899550.html","b357567dec4eeda76d6f015c9c57c8d3"],["/posts/3259212833.html","fd01d833db375313ac935e03bb3026ca"],["/posts/3266130344.html","63344ac4bae63e0dd7d8b7366859ac94"],["/posts/3297135020.html","8860c06661aef7df70f656bd383ee427"],["/posts/3306641566.html","6871e502076df241f58ce1a8af39029b"],["/posts/3312011324.html","f85351b3b18c2910b9a86010493296af"],["/posts/336911618.html","741a4c3a13c5b9ab3a6cba6a4700480a"],["/posts/3402121571.html","eff83fb82a50cfdc7f96dfb480cb8f64"],["/posts/3405577485.html","2ef5b417aae31fa26b5cca7c8f2e4946"],["/posts/3498516849.html","931fd62dc7c7f3aa02df4f1358104511"],["/posts/3513711414.html","ca6a0541c762f3ef32d6e2459f674973"],["/posts/3546711884.html","d9ac1c2ef650ed1fbd41872cbc04bacd"],["/posts/3731385230.html","d07578c30bcaa44e7e1ca9a88591a997"],["/posts/3772089482.html","f2819e606b6ac19c65cba9163ef91b79"],["/posts/386609427.html","fc65c40632a59e096835bd2ff002ec28"],["/posts/4044235327.html","02b8d5c3acb3e8c2b22ce6739a508ed6"],["/posts/4115971639.html","6a801c19a782f112cbf72b92abb71096"],["/posts/4130790367.html","d2a71e6320ca046e0200386a7b114607"],["/posts/4131986683.html","300af83ab9ade3ec5b8f3b6d11cf2f07"],["/posts/4177218757.html","e19d3a5be57dab96433a03481e915a8b"],["/posts/4192183953.html","e9b77b1ab2bf0c82d99bc132ea1abc9b"],["/posts/4261103898.html","3517bd4f1a85f28eeae107e949517581"],["/posts/469711973.html","efcae2d77aa9cc4adfab9dace8d0bb88"],["/posts/482495853.html","ec3f74efcb75bce67d9b5db9bb8ab03b"],["/posts/488247922.html","d9b4f8d90cf462342940625292740fec"],["/posts/570165348.html","4864a2b2466935cdd2db57c8caf85113"],["/posts/595890772.html","43f4e23b1bb27fcc553b40f766043570"],["/posts/694347442.html","73c85b92a24071a039b672d5815fecbe"],["/posts/707384687.html","0b2c644d99962c596fdb0fb5dfacdf72"],["/posts/71180092.html","36fa5d0f9c57b9ea79b5ee4856d394fc"],["/posts/716459272.html","a16608fa399209671d3086a38637f9f3"],["/posts/778231993.html","dcce80c93cbca730747eb8457e7727cc"],["/posts/795397410.html","8410267098ace1a6792d39274eeac70e"],["/posts/820223701.html","3917ff2cb9fc779df8b854fd413fd3af"],["/posts/830372185.html","d199ad056e802c686709806503e1796a"],["/posts/88294277.html","b6f226029cb0f223cad0f4fcb046d248"],["/posts/939963535.html","c5f4fa891e2a32b47304c3948dcf5197"],["/posts/983786067.html","06f20a6cf6b4ce737774ada2def57409"],["/sw-register.js","704d5a166161ee9cddb8778b818af250"],["/tags/C/index.html","6df2817e5515e5dfa04802337bf0a9a8"],["/tags/C/page/2/index.html","dd4feeb207f44e010bcc7f3a86fbce43"],["/tags/C/page/3/index.html","23e46eeb274252083ad9a039d50ad6b5"],["/tags/ElasticSearch/index.html","46119d3dc2c89f060d5c56c9e93cc46b"],["/tags/GUI/index.html","d5e2ee9d42b819a4fb52e4d9d9dfaf3b"],["/tags/HBase/index.html","b66bc291e6a84169d8462251c0530608"],["/tags/Hadoop/index.html","5797433e151367f30dff0b357eb61bcf"],["/tags/Hadoop/page/2/index.html","2982df0fa45b289fa87cac29691f2f6d"],["/tags/Java/index.html","3f06dc3b966377ac8609d4558d374976"],["/tags/Java后端/index.html","d261c4e8e1df8b8b607850ed003f6990"],["/tags/Java后端/page/2/index.html","db2067e3092ef229e485151972b54ad0"],["/tags/Java基础/index.html","914cd578bc7b0319cfad5da81b0d5454"],["/tags/Java基础/page/2/index.html","5d439499edd6c860c089c58484a38b9b"],["/tags/Kibana/index.html","41c09d84e866f728e92fce85772f7cda"],["/tags/Linux/index.html","550d58655a0aa0a3c45eae77ffd7f990"],["/tags/Linux/page/2/index.html","f7931dfb4d4cbacb020d9f64cac0be6b"],["/tags/Linux/page/3/index.html","f689816ba502acc6bb876b2cf686f0e2"],["/tags/Mac/index.html","13402e152f0c0f0d8382da619da57b3d"],["/tags/Mac/page/2/index.html","b23400e0efc461fb9494fdf61e7576e5"],["/tags/Maven/index.html","be96d161eca385722da326f8572f5396"],["/tags/MySQL/index.html","7614cadd2eca972963c4f2b1d787e8e5"],["/tags/Python/index.html","1d15bf645bbba082ff348cbdeefc7bb6"],["/tags/Redis/index.html","47e1d70e071a3ef99fd6f18f174dcc49"],["/tags/R语言/index.html","bda4c7973165c48a61d34efee043763b"],["/tags/Ubuntu/index.html","495b8da954fe60c9b8350d6064514ba5"],["/tags/Windows/index.html","218fc1afe0fcd4415e186cb5fd910a15"],["/tags/ZooKeeper/index.html","b11b86f13e3f25d9bd3b36f8ff5aac93"],["/tags/bfs/index.html","e71cc8481c237cd223430acc7280d268"],["/tags/dfs/index.html","35c1073188bc2c7578fd1c01c766b5db"],["/tags/folium/index.html","9fd1c677b7e3815d3c8a1c363c1dac29"],["/tags/git/index.html","8c2360be33e531e22d3d4192ad2509a7"],["/tags/index.html","4a4a4da54207a92885d6e370e234e942"],["/tags/latex/index.html","e50b86419a62149239fac3450829d4e6"],["/tags/中间件/index.html","65fc09a58becd9bbfffc4661f33e86f2"],["/tags/二分查找/index.html","e8f7061aeddb8de069c0dc9cc5fe95c8"],["/tags/优化类/index.html","9471db4e54f3cf6438377251f3c354d7"],["/tags/前缀和与差分/index.html","079fcd65016a8eb633d3d130b9b8908e"],["/tags/动态规划/index.html","19d46e0d69106bac1a817ec9ca8b6f8e"],["/tags/动态规划/page/2/index.html","2ae10677664652ef701ddb1e693e9396"],["/tags/博客搭建/index.html","cc7e649598eeb3bbc5d12c5eb8bcefc9"],["/tags/图论/index.html","40a3d7d0d65106f94720efe6dab22150"],["/tags/大数据/index.html","4fc0c703f1cffddf24f50a5f5d42d801"],["/tags/大数据/page/2/index.html","527bd57935182373d930a32191e68de6"],["/tags/操作系统/index.html","41d9374fb32e77c5386696fe7b0a87db"],["/tags/数学建模/index.html","ffe8df22fc1a6a0ec81f8b104a699998"],["/tags/数据库/index.html","73c6aaeadd2d908e9dbd8c0fc896c2c5"],["/tags/数据结构和算法/index.html","857cc58e46a036386cca2bc2dc5b6912"],["/tags/数据结构和算法/page/2/index.html","fc8b1eeebe3a35aac39abf1a25cff232"],["/tags/数据结构和算法/page/3/index.html","252e0b1e541611d32af98799d5c6ae31"],["/tags/数组和字符串/index.html","e88b490b371cc1129d519ca9c0c1fc8c"],["/tags/枚举类/index.html","369b13c9e3a4cbb9d8f033b879c12f8e"],["/tags/栈和队列/index.html","fe9b494d5550c7024db6d207eb295481"],["/tags/树论/index.html","2ab40edd77b2576226203c3c0e560aca"],["/tags/测试/index.html","b7ad303d59a88d81581b4e566f82753d"],["/tags/环境/index.html","0fe80add0bb87f3b8e4a07fac03305d6"],["/tags/环境变量/index.html","8ae6d24f86a76f9a5f4f0470703bb843"],["/tags/绘图/index.html","1cf91e8615dca49a7ab4de145eb5c162"],["/tags/编程环境/index.html","e1c8d9810ab678ba8907a414e64bbabe"],["/tags/网络编程/index.html","b093a4d6b94d081758712bad83905a41"],["/tags/英语语法/index.html","a26ec469b8dc2105feca7d801be99750"],["/tags/论文/index.html","623e48076168ddfc8832a7fa847bc76e"],["/tags/资源下载/index.html","b85368d004cde3666f07337e5ecaf444"],["/tags/链表/index.html","93c307968dc990432f0ece05f2a3a115"],["/tags/集合/index.html","22d40be28129fabd4b7758d7d90451b4"],["/tags/集群/index.html","ccddc7243c65299a1a226bdd29a03662"]];
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
