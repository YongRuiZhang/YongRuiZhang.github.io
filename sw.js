/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","f5d0eadb4afcd89b6191f0fc07196426"],["/about/index.html","f79f070a2ce25a17dd1facdd45708f17"],["/archives/2023/01/index.html","857e0a4cd3f858bf257f2c9f47eb37bc"],["/archives/2023/02/index.html","9daad42bbd9bd8ba918443a86dd6525a"],["/archives/2023/02/page/2/index.html","f60dd2c0f16a8f143105f63af07831a8"],["/archives/2023/03/index.html","ebc87bcf4524074e2a809d06aad26bc6"],["/archives/2023/05/index.html","d5a831fa8664428e1323733618e49c7b"],["/archives/2023/06/index.html","8aa12da41e4f5d6f471810345c73504c"],["/archives/2023/index.html","5cc1de9354cb3f814c5dc939ab900160"],["/archives/2023/page/2/index.html","8111057d55d4764e82f1586d8eee81d5"],["/archives/2023/page/3/index.html","7a080a5fbfa566728078102257d9cdc9"],["/archives/2023/page/4/index.html","4fc2a43a8b9938781aad414a28be28c6"],["/archives/index.html","bc466c4f0e89c3b90fe473eddbe1415e"],["/archives/page/2/index.html","b19349c77c7ffda69b31b0d6e4cb2e52"],["/archives/page/3/index.html","614ba15b6812ef26c5b0d200083ecdc7"],["/archives/page/4/index.html","8df665a2220d892354f658a38184cb73"],["/categories/Java/index.html","dfa554fcea78874706b215651773f3c7"],["/categories/Java/后端/index.html","c60421241276a469110ac7125e8d91b3"],["/categories/Java/基础/index.html","19d9eac85934912e39176fc11065c487"],["/categories/Java/基础/集合/index.html","8771e41d0ab4bedba1a857380c139c7b"],["/categories/Python/index.html","1b7069ab78b7af75af62962ae18e1f5b"],["/categories/Python/编程环境/index.html","072b246133c1cca0ebc1046dca684696"],["/categories/R语言/index.html","7ca508865695028c29b8fdf3d057af6b"],["/categories/R语言/编程环境/index.html","3a3bf7176a3baca4bc5d1ed27e098a0b"],["/categories/index.html","28a8bc19cabc3bd531a34d3da9e57063"],["/categories/中间件/index.html","a6ed732bc70b36b23a336d58312f9c45"],["/categories/前端/Vue/index.html","07fccda9b85f76090d96793db87c9bf6"],["/categories/前端/index.html","bd4d2854c83b447b6f8771f08ca42cdf"],["/categories/大数据开发/ElasticSearch/index.html","f284b00ada7619f451196bbc4c730201"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7d47883170652210f9bf977746471e5a"],["/categories/大数据开发/HBase/index.html","be5cc3cfdac36fd73fdc6d74e59c7d79"],["/categories/大数据开发/HBase/学习笔记/index.html","8eb6fdd4e5af8ea5d38d11b59445966c"],["/categories/大数据开发/HBase/环境搭建/index.html","a5cce6ae48791d55588bb8493a9cd6dd"],["/categories/大数据开发/Hadoop/index.html","ecf6e07dd4ead7485c72b88d562d6b1c"],["/categories/大数据开发/Hadoop/技术/index.html","867bd8fae544959760efcf70c789556e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","27739e67b01e6c38e89688c7b190771a"],["/categories/大数据开发/Redis/index.html","9cbd37c42530272367a0fdca2307c22b"],["/categories/大数据开发/Redis/技术/index.html","83694b387f4d8affa8bc1d1dc3e40978"],["/categories/大数据开发/Redis/环境搭建/index.html","45050c46c00be7895f180e91e72b18c0"],["/categories/大数据开发/Zookeeper/index.html","cc54c60632422908441c256d797cc693"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b503307072ddc27026f931dff5466eac"],["/categories/大数据开发/index.html","ecf13ca395adeea6729105562b024670"],["/categories/操作系统/Linux/index.html","2a4180b52eee70f38150f92c02bc3045"],["/categories/操作系统/Mac/index.html","5e76799fe72c20ddfece1b6fcbbe7290"],["/categories/操作系统/Windows/index.html","3241ac1c342d26e9986f87528863aef9"],["/categories/操作系统/index.html","43afe0e844923cdc1dc40a46c682776a"],["/categories/数学建模/index.html","7826d9b7c76623e2f2be9f8542c6f84c"],["/categories/数学建模/latex/index.html","0021249a2308c20aead82c93c5b59c31"],["/categories/数学建模/优化类/index.html","988d6fac78f8b2b3399dfdd5119f471d"],["/categories/数学建模/优化类/现代优化算法/index.html","deb3b28838efa67bdc207ff6b9f5188a"],["/categories/数学建模/优化类/规划类/index.html","e7ded3be45a0e2ba7e3ea468b6a24f56"],["/categories/数学建模/绘图/index.html","4c1cf09d2cf9984b5b0813b1145e832d"],["/categories/数据库/MySQL/index.html","36886c3d841d083f4f1764676eed15cb"],["/categories/数据库/index.html","91adb90be393a92f195163461c11ea22"],["/categories/数据结构和算法/index.html","8b257376a8c1350f67eef7d5564d4b0a"],["/categories/数据结构和算法/page/2/index.html","e5dd127b74229b09f45081113d9e24c7"],["/categories/数据结构和算法/基本原理/bfs/index.html","64cfa0cbceb96eb5a41947c937150499"],["/categories/数据结构和算法/基本原理/dfs/index.html","8127c1dd866b020b1e49e3e6771bd7bb"],["/categories/数据结构和算法/基本原理/index.html","45ab3f1ca56055feeaa3705f24c9df2a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","1a4bd36ac8ab1410472e7da13cd9767a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e97ceacc19f05d620e21cdb585fdf9ec"],["/categories/数据结构和算法/基本原理/图论/index.html","34bd85d6d0acfb5cf9cd4213a891396a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a03e45ef6abd92df54597a1b5aa08378"],["/categories/数据结构和算法/基本原理/数论/index.html","4d85348ef8565f475adb1436dd34ee6c"],["/categories/数据结构和算法/基本原理/树论/index.html","b5e2bde2f2f590851b9db7f1ca584d64"],["/categories/数据结构和算法/基本原理/链表/index.html","9c3273e89210df00092d2d9fe3351553"],["/categories/数据结构和算法/算法题/index.html","84407dac7a4148f728cf9b7e53f93dec"],["/categories/数据结构和算法/算法题/二分查找/index.html","90dddf101e15907d880d29d781dd6b32"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ed3b2d3f19d62a9078ef610040c0d8b8"],["/categories/数据结构和算法/算法题/动态规划/index.html","a57bb153a234f96bb1ff645869647c5e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","10a4296d76d5fe5719fcac6e32c34fad"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","cacd57580c348238484f593b0f333c96"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","35a2a9578466a1c9c0501aac14e32746"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","593436de0ede667742f480b1ef476e38"],["/categories/数据结构和算法/算法题/栈和队列/index.html","af1e9b0b008578a49e2075442a46458e"],["/categories/数据结构和算法/算法题/树论/index.html","0dbfb5945c95e639e7ccc0d61673a490"],["/categories/杂七杂八/index.html","266f5412733671d3790b72d14bdb260f"],["/categories/杂七杂八/博客搭建/index.html","15b6494431ae53ccfc6d67bb0c7fef52"],["/categories/编程工具下载/index.html","b64808c2a61d2ae77224e9ccb3198da8"],["/categories/编程环境/index.html","0f85cd8804b6bcb08759fbad8a25ebdd"],["/categories/英语学习/index.html","3536c801dc9157b7ca8c0e7a1932b815"],["/categories/英语学习/英语语法/index.html","af8ba782036bdc67d1209cf7027d27d0"],["/comments/index.html","9e9e4101a1f89db6de48b719c071b7f4"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b72da45e68d06570f9b954e069a218a1"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","fa259edcf2faeef8c23d11fd101bc9fa"],["/movies/index.html","cba33a9cdece860b5d692ee9ae94c39c"],["/music/index.html","75cb935607ad48c15029bd3c9c7ca841"],["/page/2/index.html","ed7fd07171394e2257a3889ba77aa0bb"],["/page/3/index.html","ad3ce9521bbc0f183c8d13d70faa7511"],["/page/4/index.html","c730fe5ff7f4e507b1e11cc84df59f85"],["/page/5/index.html","7e0071046d1cc88a01ab253d66621de7"],["/page/6/index.html","cd78218af9a38fe7bc2465af1f21ffc9"],["/posts/1021360842.html","39ba291902c510a4fab62158722bff76"],["/posts/1120620192.html","799fffcd7b2e1d5628c651d6cfe664f8"],["/posts/1141628095.html","920d455012531a60073fa81a3dc490d0"],["/posts/1168613674.html","2d232a73f1ade5a3d1544850b778af63"],["/posts/1219920510.html","5dcdf428702f44d15b4a14f3a4adaef6"],["/posts/1222166338.html","9e0cd73390149dd446daa4ee23cf1de5"],["/posts/1259097482.html","d2d8a0b80255d1b9591965179fd2eca2"],["/posts/1271036369.html","c02f04ae6b939a78d20193a2d81f4bc4"],["/posts/1312847445.html","29f01f6b0d7323ab2ae609195d3e6e73"],["/posts/135355774.html","e24ed7cb1f16f10ce901d00c82410886"],["/posts/1375344716.html","bf2b8dd325757f7f625ea8ac0b621fcb"],["/posts/1388991698.html","d4022cf60aa0622c290b25aef83265fa"],["/posts/1410315814.html","da57ec157054b8309c86bad2510ef8b4"],["/posts/1452790229.html","702f682e43898c05931f1035938d03ed"],["/posts/1470079884.html","251b2c6c6db4ec674bf7e2e5ceca0f61"],["/posts/1470079885.html","be134f08a49b7ff074321a6a4ab0a7ec"],["/posts/1470079886.html","169ba67d24e8ef66d68bc20ec70281eb"],["/posts/1470079887.html","2beaa76f9df9f3842816c35b6a05d924"],["/posts/1498536549.html","523fc4fe43c17053f6839413567f4206"],["/posts/1547067935.html","3154f0c0d66df8e46b75848fd751dd8f"],["/posts/1557866301.html","d0b39097d6a47670c80a4607e5b8af4a"],["/posts/1571776361.html","0598246db7a09c1f55689cf7270b8ff7"],["/posts/1605124548.html","a95526e0644bea0480709cafe2666901"],["/posts/1633036852.html","41172142ea037a30828066d59ac3f3e8"],["/posts/1765123828.html","0084857560289bcfcdf53047fc9aa89d"],["/posts/1767336200.html","cdecfa237b9f828369c68646ded42aa0"],["/posts/1776114197.html","3173166dd7b8841ab472604ab6caf0c7"],["/posts/1817748743.html","48da7b25b16503ea5fc56f5fe8b0f824"],["/posts/1925125395.html","7827b19e833096160ed9116642035c9f"],["/posts/1966191251.html","ff82cfe0b682c31f0ef6a872635b78d1"],["/posts/1987617322.html","41be1ec9fb51cc99ce3f709f3422070c"],["/posts/1999788039.html","9d50dbbe82310096764b1a99fbcaf989"],["/posts/2075104059.html","43a0051d0b21fe198444f3b04bbab2a9"],["/posts/2087796737.html","bb414b8dd332186648a54a7ba846b74e"],["/posts/2106547339.html","6800d9d909034f60d7cfaaf12aab8e87"],["/posts/2207806286.html","bd1e1a95aa2f2c738f526c6f27d612b8"],["/posts/2225903441.html","876826760951d27ce648cffffe45755f"],["/posts/2265610284.html","4026eea60ef24aaf76a72b2909bd64cd"],["/posts/2281352001.html","a7a848715c82593a016b22f2b3210fd1"],["/posts/2364755265.html","e3b0a9b39e8f3570b550543f810690e8"],["/posts/2414116852.html","70a7fe83235517950e35f50933917211"],["/posts/2421785022.html","db0bbb81d5abbf0303319a4bf0b6a708"],["/posts/2482902029.html","16946f1c29c7d9457cd44027acedba9f"],["/posts/2495386210.html","e9c42c96fa260a97f1d77b2eb95c34be"],["/posts/2516528882.html","eb19ad4d7b7f40e45e8bbad420b5edee"],["/posts/2526659543.html","f7bd50a850a6df666ce5abe3b39f1057"],["/posts/2529807823.html","24991b5989d17ae2dbccc60cb1ac1336"],["/posts/2596601004.html","c8df2b2c8c38e865f76ae7a3cc63b5b4"],["/posts/2742438348.html","3a9a55b4929fd141a4ceb070cc6ae340"],["/posts/2888309600.html","cf43df8762ea1deeea4887c0e42f9b22"],["/posts/2891591958.html","d79ed8916a049cbedc86f2870c13d549"],["/posts/2909934084.html","1bab6e0965cdf8aa701d94579b6e5665"],["/posts/2920256992.html","3d6fb3ebca81da96f8781e1579f5ecd9"],["/posts/3005926051.html","1ec5fc3d1aa43c6d7ba103a4fd51d693"],["/posts/309775400.html","4b3e6995668f46f431d21d1cd0a6c435"],["/posts/3156194925.html","a0900b9eb6238924479cd9b307d6021a"],["/posts/3169224211.html","c129d54e8752c0df3831b17c58138334"],["/posts/3213899550.html","e86c14d6628a581de73e592d8758f8ad"],["/posts/3259212833.html","5ec568c5a595b649dcd7b720c11010de"],["/posts/3266130344.html","12d51410be29ca969735621ab9bc6b67"],["/posts/3297135020.html","f9462e26671e684623914db337b54f62"],["/posts/3306641566.html","4746b118e4ee0fa2011264c9107a71f9"],["/posts/3312011324.html","3fae758dfce16e819c2843f6c5970cc2"],["/posts/336911618.html","01a9ea7c72a6450eaf09c208992e5204"],["/posts/3402121571.html","8a054fa4a07c3754e2181a9d226da19f"],["/posts/3405577485.html","93b6dbb9ea91b83bc7659649e599778e"],["/posts/3498516849.html","b37bdd57887d2a8277a38d5780790451"],["/posts/3513711414.html","c1722f62186bb01a63d927b2445f5468"],["/posts/3546711884.html","8036e0ca7721e2f412c3bf06656bc7ca"],["/posts/3731385230.html","cdaaa03971bf000e7e3e39ca631bea55"],["/posts/3772089482.html","ec29914cab33f8b9e5fb0f56cec32fab"],["/posts/386609427.html","b510a6a90b44c9ed2ed8c9a317b7c2d8"],["/posts/4044235327.html","cd2cdd794b1710e25f153e9ea60de8a9"],["/posts/4115971639.html","2f8889551e39ad6c30ca2c4aa69acea4"],["/posts/4130790367.html","f235f65b0deb48593cbecac13e9f00d9"],["/posts/4131986683.html","0f5f2df7c6d1b7e1082f52279a00d803"],["/posts/4177218757.html","2c6a8efbc579e29edffbc26e213713f9"],["/posts/4192183953.html","92f6c2383ce645367dd41223a8e8a0eb"],["/posts/4261103898.html","4cb937accd3907ef0729aa5ecaa78dcb"],["/posts/469711973.html","44f0cb4f4ef189f2ed4931358279d5cc"],["/posts/482495853.html","81656691f40e3d38036a23cd597aed6b"],["/posts/488247922.html","92f5e2824fe9fa4ddbc2cfbec5bd2a07"],["/posts/570165348.html","e0e4c9e7195825522ec5f1bd71a8e3cd"],["/posts/595890772.html","6bea16b53f43b4a886a80522de3e669f"],["/posts/67485572.html","c1d923023a850430d8475455afaae81c"],["/posts/694347442.html","d868d44e0acb98d1b4485f3d0aff34b1"],["/posts/707384687.html","4899fd838be517f46c14eb66c2020aa0"],["/posts/71180092.html","8e76c781fc8cdb11a5c7cef4217c3a1a"],["/posts/716459272.html","faa083943c5798c16ba5a72906b2912c"],["/posts/778231993.html","db85ab3e17a5330a37c7d768f0659151"],["/posts/795397410.html","a7cd5e86386d8b6300869d5d075b8c40"],["/posts/820223701.html","37569866562dcd0cd546416271af5dbf"],["/posts/830372185.html","f889bc0a0130bfa80c859381fff296fb"],["/posts/88294277.html","934f14d5a73a5e8c7ab3711d0551fe48"],["/posts/939963535.html","94d4851f00f89d75238fe1cb5098f965"],["/posts/983786067.html","6c1f6f996f2d60af463df1416039d369"],["/sw-register.js","f5760eebee6a48123dd33a7a894a9b51"],["/tags/C/index.html","296d1de4e149ec80d454e70c7ad96a52"],["/tags/C/page/2/index.html","d23e8ccfe9d0e908d58c21cf01c1125f"],["/tags/C/page/3/index.html","513848f475261c8ad8bc51b4ba73f8b5"],["/tags/ElasticSearch/index.html","72b539dc38696fbd2a3a738a7b0d2b0b"],["/tags/GUI/index.html","3d9516622b0a25b5b8324392e587bd7a"],["/tags/HBase/index.html","5ed808acb7a70cef75a5f20c69d8e6b8"],["/tags/Hadoop/index.html","f55a8d82b75a0be35f6589ef7a423fc4"],["/tags/Hadoop/page/2/index.html","a7ce2df8f88b903793de4788c7bc3798"],["/tags/Java/index.html","eed8a78b9820a267e47cab19689709ba"],["/tags/Java后端/index.html","96584a9bf7dc1f9cd7666a04a52b254c"],["/tags/Java后端/page/2/index.html","c8b8382e1fac9cbd59692fbacdefd2e9"],["/tags/Java基础/index.html","6ccfaf1230a0c827f2fde6bf29cf22a9"],["/tags/Java基础/page/2/index.html","0e72fc71536d200c0b2f9740f18482c1"],["/tags/Kibana/index.html","c4351cd6e1811f7284ea8c1a870f0dff"],["/tags/Linux/index.html","e3b56983a34ecf090a4c126544ee778f"],["/tags/Linux/page/2/index.html","a1e10b5d4da8d60675ed883ce77f206f"],["/tags/Linux/page/3/index.html","3ed1c9e9eb7d308c6eab8567aa5494b4"],["/tags/Mac/index.html","ca6457b24199f26aee2f820864733f66"],["/tags/Mac/page/2/index.html","b1c1ff56bd5fab7caff9351d18556f68"],["/tags/Maven/index.html","f86eec9defac777421703e63f57745ec"],["/tags/MySQL/index.html","08e234b815a27770cf50578db5c7e446"],["/tags/Python/index.html","93adce714f556c55c4da916468c633b0"],["/tags/Redis/index.html","14db65139afa6d19903e28adacc5853b"],["/tags/R语言/index.html","8b4b941a2c356e7feb67c5cdd4295780"],["/tags/Ubuntu/index.html","a9a00fd5665d8058dce8e7297e4c6406"],["/tags/Vue/index.html","95404ca2ef4761578682775ebedccc96"],["/tags/Windows/index.html","7bb9d334a50e3786697590c286f8ffeb"],["/tags/ZooKeeper/index.html","ad16c36f2254630e76f2899fc50c71a9"],["/tags/bfs/index.html","bd7266baa419645901c7932260a3efb4"],["/tags/dfs/index.html","171fdaddf00723ac2a1931e9dc5c528c"],["/tags/folium/index.html","7e2e2cf7011e9e2d2fc77bdfc3f6bfae"],["/tags/git/index.html","291b5e74689078ad184f65c37ed8b120"],["/tags/index.html","4bbc0981737b29d52347946066b171c4"],["/tags/latex/index.html","339eab4791f82d9cae7d9915673d2cd9"],["/tags/中间件/index.html","b41f34671bebbb700711127396d2e3ed"],["/tags/二分查找/index.html","acef73d7e2051f2876b5772ef4d14481"],["/tags/优化类/index.html","b01e3f5a4e488d0a0fdbe9d7d6aee872"],["/tags/前端/index.html","c794847308ab91d38f55c7c1e2d25d66"],["/tags/前缀和与差分/index.html","b67a95e105e03a5bdf904f82d5337391"],["/tags/动态规划/index.html","12f5a79bd3d5f97f9973b05481178a38"],["/tags/动态规划/page/2/index.html","1c091ce578481235d09b8ec388a2efdb"],["/tags/博客搭建/index.html","52aebec713c1fbb50d353a08ccb36483"],["/tags/图论/index.html","868e618f00348c71c156e6175ddf2571"],["/tags/大数据/index.html","1fc1c7d34dec227c35f950749d431ba8"],["/tags/大数据/page/2/index.html","85b19bf2232f135406e707cd7fb5e271"],["/tags/操作系统/index.html","ec934cd8db6883a931f3ef42155da799"],["/tags/数学建模/index.html","eae2127e1690583d6a1bc0eabde2d84a"],["/tags/数据库/index.html","f4937e15a63818badbaf640db1d9ff5e"],["/tags/数据结构和算法/index.html","d0c156e062e7bc7f8f3dd42851575342"],["/tags/数据结构和算法/page/2/index.html","7d0f0be279b4baf3436b4942882dfee1"],["/tags/数据结构和算法/page/3/index.html","9a18cdaa1f3e5614015c11f3f3c9289f"],["/tags/数组和字符串/index.html","b90d25293fb354bc247b400731a3b4e6"],["/tags/枚举类/index.html","634cd6fa240c6908ff65e75afc5f7f88"],["/tags/栈和队列/index.html","b3610520d109747b4c757bcfceb5c1c0"],["/tags/树论/index.html","80b16af69ee570a97e8f3135c2a97864"],["/tags/测试/index.html","986d17aea2ef743eccede5aded845e9b"],["/tags/环境/index.html","3c570d4b0f7710c237ed95eb0567ec3b"],["/tags/环境变量/index.html","790a0e5be9ae1d6e14d58e7ac6033dad"],["/tags/绘图/index.html","a4ef0527d8b651e4e96ef00ff426dbfd"],["/tags/编程工具/index.html","85007e5ef574908a4c4791ce9988b143"],["/tags/编程环境/index.html","b6bc18f5f06969fce009245009747bf9"],["/tags/网络编程/index.html","8fe87ea8035990ba0093fbfd5f8e6e5b"],["/tags/英语语法/index.html","c228211723e5f305d483b2e920dcb1ed"],["/tags/论文/index.html","b9e474e02d69039447bb40f24b23cbb0"],["/tags/资源下载/index.html","e5dc7b80b4f66b0332cbc97e8e246159"],["/tags/链表/index.html","1177140e9d6fc7145b909c774ec286a0"],["/tags/集合/index.html","d85774cfef474b48e1cc124bf92f42f9"],["/tags/集群/index.html","00abb2b3f76cc0458705cbef92908148"]];
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
