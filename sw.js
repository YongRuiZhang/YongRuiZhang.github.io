/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","520b6866eac36b0082191184a9d02b57"],["/about/index.html","d115e7f3e47c4c0be425fa70016a2bb9"],["/archives/2023/01/index.html","13b65e3ad70e7adcaeffcfb7359fa84a"],["/archives/2023/02/index.html","7875c233c59b0cca2416cf27a3af5fb8"],["/archives/2023/02/page/2/index.html","1d0fd146b90168a85e9de1d26d0bb76e"],["/archives/2023/03/index.html","bc1e500760a16e999161a3882d931aab"],["/archives/2023/05/index.html","22d973a1b6f545b0472175d071b84be3"],["/archives/2023/06/index.html","fcab0bb9455ddff6e4df450b6363ce6d"],["/archives/2023/09/index.html","c22df31ba92bf3cf1d8a629faacc2263"],["/archives/2023/11/index.html","a980c33c23d46b5aee33cff6780c1714"],["/archives/2023/12/index.html","a9083e6aae14a8f0764492c2d0b58b19"],["/archives/2023/index.html","2266af690d2dbb30dfe89b79f690e070"],["/archives/2023/page/2/index.html","99a4c945782f31f0408a467f7efad754"],["/archives/2023/page/3/index.html","90d93727611e27f04aae8967ae2878a4"],["/archives/2023/page/4/index.html","2ed5d57c4c2be9f8cb414fbcafc479f4"],["/archives/index.html","9a4035a196e77b4697d4c8446189d299"],["/archives/page/2/index.html","fb321637f9caa5f56ebcae7495a7b55e"],["/archives/page/3/index.html","75eea422559ab55eeb7ec8c17edd1e54"],["/archives/page/4/index.html","126c4346b030c14f34740c9a507b3fc0"],["/baidu_verify_codeva-qQP2iZOMLX.html","4948e4b36610bc114023b4e6aafcb9dd"],["/categories/Java/index.html","668d0b2aef3a088e15bdc3068f68d2cd"],["/categories/Java/后端/index.html","7d30c2a879f5f776f09606d43484f8b2"],["/categories/Java/基础/index.html","8d8d11359d233a7e01d266b7c4187ada"],["/categories/Java/基础/集合/index.html","9b286b4e1fb52b01d30fd7faf78929c6"],["/categories/Python/index.html","2fdbefee3a6773a4067f1e1fb136e45f"],["/categories/Python/编程环境/index.html","59680eb826d87f67f830ac676edb19da"],["/categories/R语言/index.html","d73460d48ce3b370526ced2fd978927f"],["/categories/R语言/编程环境/index.html","dbbd6473d76a47f03e5721caa5bb7eac"],["/categories/index.html","1269bfd77fcbfc74f20be73843f7a1af"],["/categories/中间件/index.html","18cf721141a4b96b955c6c2f121fc388"],["/categories/前端/Vue/index.html","89dc238ad94351816f782d24ce7a2dfd"],["/categories/前端/index.html","1b9aec4c0b960ffdfd4d1a23384d646a"],["/categories/大数据开发/ElasticSearch/index.html","34bebbbf421fc3d440e34b417c04a83b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","17669ddaf3d8554c795cc1a29a3e27e6"],["/categories/大数据开发/HBase/index.html","2d8f84f71bda81a3e7ed5154c0f00929"],["/categories/大数据开发/HBase/学习笔记/index.html","bc6c967c31b72df2083f46a1d9bfab28"],["/categories/大数据开发/HBase/环境搭建/index.html","f46c081269fa358ce5c420848a7251d6"],["/categories/大数据开发/Hadoop/index.html","6982fcf8cc62152c7331c1c36bbb5a3d"],["/categories/大数据开发/Hadoop/技术/index.html","cf2a3638cfeaa157bb57f468ad540247"],["/categories/大数据开发/Hadoop/环境搭建/index.html","58f145b679d2cdb826131396cabd9404"],["/categories/大数据开发/Redis/index.html","13719a1436be92c983d12530a484dbb4"],["/categories/大数据开发/Redis/技术/index.html","c2a307fd4764ca3246ad8c0c1e5ffbfd"],["/categories/大数据开发/Redis/环境搭建/index.html","034cc054b3f99097ba4af4ee0b1c05f0"],["/categories/大数据开发/Spark/index.html","27b75ed28c7542ab06e002ac53055d0a"],["/categories/大数据开发/Spark/环境搭建/index.html","a2fd716960eac3ddd4ad94beaceb8fd9"],["/categories/大数据开发/Zookeeper/index.html","6ffc9587226c6cb70f2d6aee833c1b22"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f97ae81bc6d61a59a1ba4b0562706faa"],["/categories/大数据开发/index.html","f6ce0bf89419a960828731755fb825be"],["/categories/学校课程/index.html","2e4a09a26d0456c009b51818e2593b95"],["/categories/学校课程/计算机操作系统/index.html","7a7b627ee56afe403d8bbd8a4c778207"],["/categories/操作系统/Linux/index.html","2f2f333585e50904391c94388e16affb"],["/categories/操作系统/Mac/index.html","85e2cb5d2d02212f63c12decaefa0c5e"],["/categories/操作系统/Windows/index.html","99090eea2ffbc3b76161e1ff0a784905"],["/categories/操作系统/index.html","564bca4b2ed3ac2612695ed378ac16d9"],["/categories/数学建模/index.html","1c71b788b64b9a6bdd45afc3fd44c6f2"],["/categories/数学建模/latex/index.html","07d8aa051227fc406a546d9e6999355a"],["/categories/数学建模/优化类/index.html","890027db97c9bedef0154e5e45fbb7e4"],["/categories/数学建模/优化类/现代优化算法/index.html","dbbb2390919266551eca8c7e5f58bd38"],["/categories/数学建模/优化类/规划类/index.html","c3d79ac1b3e3931dd1f913d2b3a35c89"],["/categories/数学建模/绘图/index.html","142fc563d68b0081d82344f69d757d29"],["/categories/数据库/MySQL/index.html","12fb0cee1fdac662691c04ebb26eef7b"],["/categories/数据库/index.html","b8c9f41bf01657e1510c048757161d47"],["/categories/数据结构和算法/index.html","677c8252ab267682a42dd8df13d2b253"],["/categories/数据结构和算法/page/2/index.html","9abc645ea5b0f45cf31350b6f1c17151"],["/categories/数据结构和算法/基本原理/bfs/index.html","cdafa95dc03158b27a6729d6cd524609"],["/categories/数据结构和算法/基本原理/dfs/index.html","aa6c4898626db90dd627edeaa413e2a7"],["/categories/数据结构和算法/基本原理/index.html","67b32461ae1bac56a76e4e7445fb1ed8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c7f05d962d7d66a20d3b3dbdbde78b80"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","6a63b87a88ee11f72dbc92e51be396e7"],["/categories/数据结构和算法/基本原理/图论/index.html","fdfb6d70866398116d9845e97fc6c407"],["/categories/数据结构和算法/基本原理/字符串/index.html","f539b6d42d028620adfe3a3873432711"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","0727cc4af5e36731bcea4e155a8019e8"],["/categories/数据结构和算法/基本原理/数论/index.html","9795ceda2e95bbdfcd5c7220f56d758a"],["/categories/数据结构和算法/基本原理/树论/index.html","a21eacfd9583842336f12e685699f561"],["/categories/数据结构和算法/基本原理/链表/index.html","465903deb354696d9f295a8b098323bb"],["/categories/数据结构和算法/算法题/index.html","adcf2371c2de16f19ef5963d3e2a6245"],["/categories/数据结构和算法/算法题/二分查找/index.html","45c98355b4ecc6a641942d9b6199df36"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","e77ec74bc5e6efd444f9ea50a91c67cb"],["/categories/数据结构和算法/算法题/动态规划/index.html","ad1144a8171963c5aa992470a41f8909"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","8634060ba27b1b76c8047c4caf026669"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","45192cf9a669bf81ca597e234d27ecb3"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e059531c6e5bad38c39de11017fad58b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","91f97062d2f3fcd111bf9277963cb596"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d097de47037b59a0e0820cacf61b440e"],["/categories/数据结构和算法/算法题/树论/index.html","dc598e9eec2be1b82f14d80db7e51637"],["/categories/杂七杂八/index.html","4954f9459e656e2f7e865fd005e82600"],["/categories/杂七杂八/博客搭建/index.html","de43f02274016db139f3c222a9ca7dc6"],["/categories/编程工具下载/index.html","c25c97c17971ffa3bcb69bff78cbcde4"],["/categories/编程环境/index.html","77437108aa18972f40bdb4c3131bb15e"],["/categories/编程环境/大数据/index.html","565930a4277008359e600c10b0e70842"],["/categories/英语学习/index.html","8d61406420fa051518351daa16bfb779"],["/categories/英语学习/英语语法/index.html","0534026c196a093797a999ad96e9cb41"],["/comments/index.html","949c7b7c77b114f5ba75e88eda949902"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","dbb1195b62b1532e0d789703ff78bb06"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","25dc1658742a16a7fd4923ec0631e4a9"],["/movies/index.html","0b192e595e35b1b29c2df3cfc3a00966"],["/music/index.html","ae2679134acedb91314512246e513843"],["/page/2/index.html","8b899a5b696ae4cbc9d160814af49469"],["/page/3/index.html","d50034b7b9948e26953eb83754c1bb0c"],["/page/4/index.html","e7d6a9fd2894f756d1d28ee3d60932d6"],["/page/5/index.html","54e9edf6fcbc1bf3039ccbc9a79dcbeb"],["/page/6/index.html","e679dd34574939ea31f2ca5094ebc34a"],["/posts/1021360842.html","1cc538ad7b125cbf7b705084f2984d69"],["/posts/1120620192.html","5a25bedef86a0e8910adf191862afd70"],["/posts/1141628095.html","f21eab0bcad52c09f5fddee06a2f5914"],["/posts/1168613674.html","41ba3f79b9cbee741d3c812c44b52490"],["/posts/1219920510.html","9abbd1f68d782e65b8d0225715a2c2c2"],["/posts/1222166338.html","2478328cf574a1e8581bea12bf0004b6"],["/posts/1259097482.html","130b6d4a185b0ed9da23dd1cf4ccc19f"],["/posts/1271036369.html","4cf4f5b9173f481e32f048092db5b586"],["/posts/1312847445.html","67270c0933728bba5468222b24e36e6f"],["/posts/135355774.html","a507b382ccba87e58095fd7481f8048d"],["/posts/1375344716.html","670e0ec7006788de88201020a620cb12"],["/posts/1388991698.html","7a19e53e4a9a006ea1830b211453aabb"],["/posts/1410315814.html","573653721861a7fe4e7dd6d3ada02008"],["/posts/1452790229.html","99f049fbb1e104906598a8569203373f"],["/posts/1470079884.html","073a0dc4c5b2bc777ff7288bed6094c7"],["/posts/1470079885.html","d4b5ab9d3fdcef4e96f1f7283d8bbd8e"],["/posts/1470079886.html","9b8966abe2935a220649facef4f7b3e0"],["/posts/1470079887.html","bc8585c2016076ff7bed67b020959a72"],["/posts/1498536549.html","9a80ba1275bb0a1237ab94b47dcd3f3b"],["/posts/1547067935.html","78d0f4b8f09c6aae3f19db87d186e4dc"],["/posts/1557866301.html","168dc09336dbdca5285381c9cae58f19"],["/posts/1571776361.html","5c847dbb0b24b12a5f1b5cd3cbbaca86"],["/posts/1605124548.html","ac0b9a01908967128f9d6c23c8a5f32e"],["/posts/1633036852.html","fb15e7ae81504e6f58ab3bdcbda89bc4"],["/posts/1674202625.html","71fcf8975321b77347bcf43bc65094f7"],["/posts/1765123828.html","e13f69b41e263d64dd93dd6895c99994"],["/posts/1767336200.html","c54f9bb0729f860d05d804c67146a9a7"],["/posts/1776114197.html","0f25dd16bc6ec3b82083944da787ba29"],["/posts/1817748743.html","65a25b8f538a9d167b2936ff5c36a4ad"],["/posts/1925125395.html","d256abd0a924bbe576f040d9f4ddb5f3"],["/posts/1966191251.html","39372156bf2c1300d5cfecbace53a8e8"],["/posts/1987617322.html","6dc48738350fb5ca9b8ee25164a7ead1"],["/posts/1999788039.html","c4fd8d1d86f8d1dfa97d9ec59695ed0e"],["/posts/2075104059.html","5f4376a1d955b9cb56d3fd4ad3c94b7d"],["/posts/2087796737.html","00048113f9392b8227ce19f7389ab85a"],["/posts/2106547339.html","09d7bfc91923c7316e1cda640f16f15c"],["/posts/2207806286.html","6815fdea0ed0fd10ab374170276687c7"],["/posts/2225903441.html","3b9e2d7155dfd68c8b084d7c30f08b6b"],["/posts/2265610284.html","20de0295e85bf4b61453bdf9bbdd8549"],["/posts/2281352001.html","4d67b1e491acabd3fef78469fe60ecec"],["/posts/2364755265.html","f381998434783bc6281e42cabc195fff"],["/posts/2414116852.html","6511bcf5e16b64ef5909a04ac91b3c9f"],["/posts/2421785022.html","7ed92f490a43582d0ce4dbc66124044a"],["/posts/2482902029.html","2ef36b291824576c908d90936ab9ce41"],["/posts/2495386210.html","afc4b0dec8973a4a0b66cbc9ca167588"],["/posts/2516528882.html","98c68b32cd0de1a39d46139c66f780c7"],["/posts/2526659543.html","53e5a255009780f71165b34880e441ab"],["/posts/2529807823.html","77f1bc43ef33e964cbf628da1e880ea1"],["/posts/2596601004.html","aec552255d9f9a03df84e0f83b5190cb"],["/posts/2742438348.html","6e34333e550a25c7ef6634398a28704b"],["/posts/2888309600.html","af0786a402d7f66a07baa090c0ed913d"],["/posts/2891591958.html","a2c38e01ca9cb6a13c794080cdd1bde3"],["/posts/2909934084.html","f614d41cbc8c8436f7fe1562ba0c0f91"],["/posts/2920256992.html","a8ab9117304ed473d8d7da9e374007b5"],["/posts/2959474469.html","637253cddf8e781ee7d56ba6a9926849"],["/posts/3005926051.html","d486d912782b71fadff5e6cb319d0e76"],["/posts/309775400.html","ef4892a649a39c82497a210a508373c6"],["/posts/3156194925.html","ba2970e76d026389da21fdcb732b8456"],["/posts/3169224211.html","762959d2c28c62d5d9d460fe69b13071"],["/posts/3213899550.html","823b3f3d7a1b0de295b1d22ea892dbe3"],["/posts/3259212833.html","42db0317a25c6fcd6b98fb952e333aca"],["/posts/3266130344.html","c7df69c3d6db26d54b8ce195da373eb2"],["/posts/3292663995.html","201a060bb847c9db0b7b95a140877a46"],["/posts/3297135020.html","8e9ec129ceaf5d10c2eef576e5417b01"],["/posts/3306641566.html","756a3574c6a00f2be6c9c6bfdbc83adf"],["/posts/3312011324.html","1b502366d86f034930537448b440dd95"],["/posts/336911618.html","178c41842b959368e395c3ddf356fb6e"],["/posts/3402121571.html","cb8639e688aefc98417e8cf9cf0690c3"],["/posts/3405577485.html","40a88cfd14ae049e5e5b9be90c553df3"],["/posts/3498516849.html","64baae0821a4260f7f6936b04c38a5cd"],["/posts/3513711414.html","341deb40a8e5b1d9a68d68e9b51af8df"],["/posts/3546711884.html","d94108c340e5d022918a716d04d74ef2"],["/posts/3731385230.html","14ea1b20fa033445652204ec1184db4d"],["/posts/3772089482.html","1cb14a240a667558953bcb054d85f6b3"],["/posts/386609427.html","c9c317283fa69310fb94ed004ed783e7"],["/posts/4044235327.html","541b4c3293ff38ca3cac69fb9ebc1ec7"],["/posts/4115971639.html","ae83ec786491eccd3c1117615f9575f4"],["/posts/4130790367.html","a6e57535692db5f53cb672007db766e3"],["/posts/4131986683.html","f96833b2b7014e90c4d3376d1c413977"],["/posts/4177218757.html","700ad88c88d4667dfc8f8f34c344ca93"],["/posts/4192183953.html","dd69663a93067af21bd55861e9802248"],["/posts/4261103898.html","5257027674c94cadb5daaf980dd724bc"],["/posts/469711973.html","b9f9cab07b5f86d50d7137664ef4eaf3"],["/posts/482495853.html","ce44f2a36cc6a795e91611076a62d0e9"],["/posts/488247922.html","18a73141db514b13d575e9809bab4289"],["/posts/517302816.html","ad084157166dcbb27ce0de3f26641422"],["/posts/570165348.html","9196c74b6d26b7a43b6fbbbfca628bf6"],["/posts/595890772.html","6e11af41444e63d5bb35e400d1062188"],["/posts/67485572.html","42cc1898f665d2a672f27b65ad1866f0"],["/posts/694347442.html","6487d03b44d06884f05c354847bcdf85"],["/posts/707384687.html","9e80c8fa8cae66ecdcb30521ab1cb8ec"],["/posts/71180092.html","9cf0ccf10cd09013fb12ad82e988e37a"],["/posts/716459272.html","c7be555bf80aadf7fdb79f6480172e45"],["/posts/765481613.html","1ba5825aa053aa5c7f64c90a4d192745"],["/posts/778231993.html","00b51c7399bc3f2bcd460e4920723460"],["/posts/795397410.html","bffda3a1f8fa2a778022e18d7542d7d1"],["/posts/820223701.html","829cf673561320ba066b92906536d082"],["/posts/830372185.html","4c06635a2a7e8c419d0ff81fdb4d0d49"],["/posts/88294277.html","ad58ad2844055c46ce6e384a257ce5de"],["/posts/939963535.html","34309e2725fbbfd875c5b0a799afde2a"],["/posts/983786067.html","06cff7684e49b0738d697117b04fe00c"],["/sw-register.js","6204ecc11cb9574609bc54d286ea5890"],["/tags/C/index.html","d930489cf32f54418eca424d16e30966"],["/tags/C/page/2/index.html","32a11708a62bc3cd69d07fbce90ec273"],["/tags/C/page/3/index.html","a8eaba2156f2fabd1fb34b0fdb3eb095"],["/tags/ETL/index.html","de6235d71edf388b405d52dc8092295e"],["/tags/ElasticSearch/index.html","02b2cee39cab06f31fa3571dd04d1938"],["/tags/GUI/index.html","755b0282bb2ff66dd21eb6422b60db21"],["/tags/HBase/index.html","8337a3a2df7189fb5f4ab6a334655fdb"],["/tags/Hadoop/index.html","267acd738e5d7f289d1a3b30689339bf"],["/tags/Hadoop/page/2/index.html","06894274c0947a8ebbc61d265748c538"],["/tags/Java/index.html","7db01a10328ade7dacd7f0c9a582636a"],["/tags/Java后端/index.html","ebe7957ec3424772a53c2588d02345db"],["/tags/Java后端/page/2/index.html","cd1c2592ae41ddf1a0bfd069b250435b"],["/tags/Java基础/index.html","3b1dca4448990607570a704c3e7af156"],["/tags/Java基础/page/2/index.html","2fd6c7986c4cb51a8a0055e94a0592a1"],["/tags/Kettle/index.html","858470cc067846950cd98f2f2a78c2ea"],["/tags/Kibana/index.html","43d6a4c9a176c3273ddd4ad38dacf15d"],["/tags/Linux/index.html","5fc793f5dc703d37377fc0aec2d2c70e"],["/tags/Linux/page/2/index.html","5c828a37a7ebeffe30381d469e5e1d84"],["/tags/Linux/page/3/index.html","af48335e47a90a0a526c1c683ff9269a"],["/tags/Mac/index.html","0e3860ef4500a1a25522ebe61840cb33"],["/tags/Mac/page/2/index.html","ba616e8e0141bf5e6e27990ced938fd0"],["/tags/Maven/index.html","a0d68503710c0417d658b6e53b394587"],["/tags/MySQL/index.html","8ef3996f6712fbfd3d57f437690cb64c"],["/tags/Python/index.html","0275433366400a75fa06beb73039865d"],["/tags/Redis/index.html","7afaadfda4a078a4bb7cdbfef5317222"],["/tags/R语言/index.html","6d031d939aef8fbd3fe066d2638a6aac"],["/tags/Spark/index.html","640b4d116a8d3c1788329fd08ac1459c"],["/tags/Ubuntu/index.html","3da5f31005a893eb3ac4e5d70ed227eb"],["/tags/Vue/index.html","370b4621fedbe3d54074659f98622e19"],["/tags/Windows/index.html","a3ddb63ba3baeec2676ad7852f5cb26e"],["/tags/ZooKeeper/index.html","ce27e6d1ca4e2ddf80eb168a59082891"],["/tags/bfs/index.html","91b39877259aabf3412fdd9ca81293cc"],["/tags/dfs/index.html","380c595619923c998f6a72fafa97553c"],["/tags/folium/index.html","9cdcb2abed682d3d7f64469c25bed246"],["/tags/git/index.html","2db295a982973c9cf353fc99412314a8"],["/tags/index.html","57b917260f1761f9743eeb872f603977"],["/tags/latex/index.html","0111fdf58e21358c6db42d53e1c57687"],["/tags/中间件/index.html","b5c3e6cf3cd37dba30c14796db809bb6"],["/tags/二分查找/index.html","90f605a414638db958f22cbb79237b15"],["/tags/优化类/index.html","3dd9949886c9f194a735e919fd89526b"],["/tags/前端/index.html","caf6cadc412917969602195764de0a07"],["/tags/前缀和与差分/index.html","cf21af3ef43da35180668e2fc2e542a5"],["/tags/动态规划/index.html","c12b0d49e31f0f20c91c837c002c49c5"],["/tags/动态规划/page/2/index.html","d0c21d4bd33930230b97d3fd63a75bb5"],["/tags/博客搭建/index.html","cbb5174e13dbbc77d014a0a70da5760e"],["/tags/图论/index.html","458ff5c2ab9cae04843521bfbbd882f3"],["/tags/大数据/index.html","cfd15e834520e300ea176fa958803473"],["/tags/大数据/page/2/index.html","4f812086f19e5ce16dd99d502da4b98c"],["/tags/操作系统/index.html","325ef59a8384f09c2ce50384c86b96e9"],["/tags/数学建模/index.html","013cc4a4a5cb83caaa43cc104560a361"],["/tags/数据库/index.html","b9e7fe3e63dae307edb48d4a6ce62081"],["/tags/数据结构和算法/index.html","234346d9e0c209f3d078318e373275d9"],["/tags/数据结构和算法/page/2/index.html","36d959819db20d115514ddbcdc10022f"],["/tags/数据结构和算法/page/3/index.html","de0bb08ba920004f27b7195d7879584b"],["/tags/数组和字符串/index.html","a7b1007489f2ce8d9fabd0ef04f6535d"],["/tags/枚举类/index.html","b96e46751294b600e4771590d672eeac"],["/tags/栈和队列/index.html","0ffb4db85a84ae17ea5b0e61c434126a"],["/tags/树论/index.html","0f25a2be6a2f00c1d0a377baa8a14976"],["/tags/测试/index.html","3c6db401da5d0ce0dcefdf0fa598e33b"],["/tags/环境/index.html","2984b6a38ea2fd6c9b3130cc03e99f88"],["/tags/环境变量/index.html","db6c00771a6998cf887f46a371d7149a"],["/tags/绘图/index.html","966fbb4c33707c6db1d263c7d19b7981"],["/tags/编程工具/index.html","11728302cfd4e2644ebed5d0a08082b3"],["/tags/编程环境/index.html","b5b816395af24998927d274d839976a7"],["/tags/网络编程/index.html","d9925ec8d984968228a08c2f4608d336"],["/tags/英语语法/index.html","26eb21618b6e5588f7309e92457125b0"],["/tags/计算机操作系统/index.html","11209823144e6a2e147e7457f8823555"],["/tags/论文/index.html","77ed3f7d69a0e31f1dd24b9aa3670ba7"],["/tags/资源下载/index.html","44e9a7fe816f9a7e9df25eea8907265d"],["/tags/链表/index.html","a5aec8ef89d082f41ca75697687c96db"],["/tags/集合/index.html","ff112d49f8fd325f947cc7331d0d8b02"],["/tags/集群/index.html","8fdad6b1e0e1542151326945a7a373d4"]];
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
