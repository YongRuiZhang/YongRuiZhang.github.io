/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","920d1dfdaf67745d4b754d6787a36824"],["/about/index.html","06d8df25c9b575706788da2796af1798"],["/archives/2023/01/index.html","8dc883178a8f7eaacc1b7ea35407990f"],["/archives/2023/02/index.html","cb96d6f113d5f5cc379e20ab2a8e594f"],["/archives/2023/02/page/2/index.html","c1ffc71af50b61f8cb4312dd08391a05"],["/archives/2023/03/index.html","b4eadec54ad4396f7268c0cb77a561d6"],["/archives/2023/05/index.html","59b28b3393f7db2562378129f1f57cae"],["/archives/2023/06/index.html","b50b2b26a91069c75852ad716f752e9b"],["/archives/2023/index.html","9688b2928c1d58c5d4a44cdb907f8353"],["/archives/2023/page/2/index.html","913e3ccb4f47e9537bbb0b43d0ba830e"],["/archives/2023/page/3/index.html","2864b89e9db65a29ee82d69696547ae8"],["/archives/2023/page/4/index.html","1d40c3fdbc6bdf6e3322c8040d0728de"],["/archives/index.html","d2012e33c8b07fbb3ecb19e4212f60d6"],["/archives/page/2/index.html","fb6c0d546221c902066fddfff4c29968"],["/archives/page/3/index.html","4feb3f8b89543ee188ffef23c6827d7a"],["/archives/page/4/index.html","66486b6246684a49cd702d921a42d611"],["/categories/Java/index.html","3fd775820c4aa20946e77e6b724524f7"],["/categories/Java/后端/index.html","fc378361402f340254a1de6cc76b72b6"],["/categories/Java/基础/index.html","04a482121155369880d43959ad4807f9"],["/categories/Java/基础/集合/index.html","4451654ef4138cc8b3d92639d25edc7f"],["/categories/Python/index.html","c27df6f522c82460f8b59ac06f56ecc7"],["/categories/Python/编程环境/index.html","94c3fc25178a3fc92e771bdc828e74ad"],["/categories/R语言/index.html","797cedd671af52046126c2c72ef9284f"],["/categories/R语言/编程环境/index.html","d7503843eda2acb8c50969b911d99aff"],["/categories/index.html","88616d5fd9fd5e30454d67e280af40f5"],["/categories/中间件/index.html","51dd8a267e716352c30225a27847be23"],["/categories/前端/Vue/index.html","b8c426714d4dc3a8af3fa8e82f1e8a98"],["/categories/前端/index.html","bb7484b3648100239affbd785cf57d95"],["/categories/大数据开发/ElasticSearch/index.html","2911cafadde4e27caa71b7e64b6928c3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","45ed0f855276a83d18751d4033a3d915"],["/categories/大数据开发/HBase/index.html","07a9e705d111c3b54905614b42cd4f29"],["/categories/大数据开发/HBase/学习笔记/index.html","3bb62f293efcb19014821742500fa22b"],["/categories/大数据开发/HBase/环境搭建/index.html","8c9a4e4fcb10e212e44e67405dfc3e46"],["/categories/大数据开发/Hadoop/index.html","fcec6b3ea0988582d750a0b97d2be750"],["/categories/大数据开发/Hadoop/技术/index.html","bc60a17d27afd39aa89bef04b5be1240"],["/categories/大数据开发/Hadoop/环境搭建/index.html","ae83ddb4bb5e3b699b981b7bae755d29"],["/categories/大数据开发/Redis/index.html","86b6758a0786ff353904e048e4d15d0f"],["/categories/大数据开发/Redis/技术/index.html","6a76c9fcfa0af50a2afcdb2812e582c8"],["/categories/大数据开发/Redis/环境搭建/index.html","901ec1a12f8c0089acb5db0fa544a0d9"],["/categories/大数据开发/Zookeeper/index.html","d31fb38342698db361c0ef33394c5f5a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b3ef5279270d7e88bf2c8290014e189f"],["/categories/大数据开发/index.html","601b973ee772b347de491933c7c15d4b"],["/categories/操作系统/Linux/index.html","aafc7e60410f325c2ad1277ed2b15ab9"],["/categories/操作系统/Mac/index.html","eb51030250f5ae7d3c8c33846d3083c2"],["/categories/操作系统/Windows/index.html","bffc7191fe9a6ea0f6cd573dbbd1c6c5"],["/categories/操作系统/index.html","05f30a69fd8b8cd3bdd756bca9cae86d"],["/categories/数学建模/index.html","8f5a2947bf5fe2c0419b0293bb119a7a"],["/categories/数学建模/latex/index.html","f13f6f61495976c7cc909f08108ebaa5"],["/categories/数学建模/优化类/index.html","dbea4748604dd3a01d4e5646e2c076c8"],["/categories/数学建模/优化类/现代优化算法/index.html","1da539ec1f95122699c5998996658f8a"],["/categories/数学建模/优化类/规划类/index.html","114e23aaadf130fc17df0f0b4649f575"],["/categories/数学建模/绘图/index.html","cd49f0bf13dcc9a3b31442369ac64c84"],["/categories/数据库/MySQL/index.html","dfcccb06943c244e8451b229b39ab053"],["/categories/数据库/index.html","fe3a8a83b7eab39fddb837f311129944"],["/categories/数据结构和算法/index.html","d739f11bbeca78c72f6794e6eefc3f13"],["/categories/数据结构和算法/page/2/index.html","98d136b4a8e6a7d7c3565345bdd8c9e0"],["/categories/数据结构和算法/基本原理/bfs/index.html","8e11ca6bac7178147cce3456a9d6e33d"],["/categories/数据结构和算法/基本原理/dfs/index.html","9ae9280ec49ea2fea564e8bfdab31ee0"],["/categories/数据结构和算法/基本原理/index.html","505e569efb767604f694b8dc94555ef8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","48e6162e757514a1357bbc8fada7ecc6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","7c0e5d1600a860739b2837df0701e5e6"],["/categories/数据结构和算法/基本原理/图论/index.html","1750a441acb73e0f130a5175d8515482"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","bbed7ddc7db52f1dda879e7e9c19a0cb"],["/categories/数据结构和算法/基本原理/数论/index.html","dfb4e6556a2c89b6ca3d33ee6e94c9cf"],["/categories/数据结构和算法/基本原理/树论/index.html","550a90d05583d15025523cc349afb7a1"],["/categories/数据结构和算法/基本原理/链表/index.html","c31b561e5a67320c3bb7b5e3b9848501"],["/categories/数据结构和算法/算法题/index.html","476933754d1ee7435a0818d1ba94a5f2"],["/categories/数据结构和算法/算法题/二分查找/index.html","ebb1a9ae78ba2fd48fa6cb49eb301018"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","b3b233699be93638828e25b44f8c4691"],["/categories/数据结构和算法/算法题/动态规划/index.html","24f04e09583498b8b76f82f91da55fe5"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e9f321b08b2aac4c280edd4c8b8c2d2c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","40a827cf7ca32f93437d5c1e5fcb2638"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","aa2ec5caae56dc56cc3dc540d0c88858"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","df5d0a84f4736bfc8af64873ab9c3cb2"],["/categories/数据结构和算法/算法题/栈和队列/index.html","0877bc9649cfa700925a552468138d46"],["/categories/数据结构和算法/算法题/树论/index.html","c73e519e6bba610714aac38a7adf0bb0"],["/categories/杂七杂八/index.html","3bf1e8098e254834e423918c4d3e4202"],["/categories/杂七杂八/博客搭建/index.html","d3fbf18c5a230b7ef5a493e3a64db97e"],["/categories/编程工具下载/index.html","9a1e3ffb1527f087e608c9ef72185214"],["/categories/编程环境/index.html","2982e345f9a710d27a295b7db6068907"],["/categories/英语学习/index.html","ab198ff65809eb86ac80e0642a50f16e"],["/categories/英语学习/英语语法/index.html","3f5795079eda7e47b60f36a9a48a06a6"],["/comments/index.html","459dcbed818b0880a0d54e5e71e60e40"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","f52edea4a9ade319852e7400bb50e943"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","27724dd4b5bc133ca07b10706b2b3a4f"],["/movies/index.html","0833829206ee7f8207feca9f295523dd"],["/music/index.html","a1a98dc38aaa9f8b503f2e99ffa307d8"],["/page/2/index.html","dc723cba5bf701d5d0572cc00fe34d7a"],["/page/3/index.html","11bf02d488db2e42e904933d4d432661"],["/page/4/index.html","6f3a61975831d61193caafbf0140df38"],["/page/5/index.html","899a1635c006761bad8420f0c7b248c6"],["/page/6/index.html","52f475422a72a70013842ba8d787f5df"],["/posts/1021360842.html","2b6af3b3bb8500412e521f44f846d3c9"],["/posts/1120620192.html","f5600a611ef69e1ca7bcbc97871bb064"],["/posts/1141628095.html","a7c65b8780ea01f18157287566c9b485"],["/posts/1168613674.html","3445b8ab4499d105d892f51d7ce2502a"],["/posts/1219920510.html","cc8ef4006fc99802025d88a24c037556"],["/posts/1222166338.html","e8461677131f2954014e71da199b17b1"],["/posts/1259097482.html","7c2b47fad2093f7bbc98c253a81f8f9c"],["/posts/1271036369.html","634f41fd0582c1e9087399d43febc776"],["/posts/1312847445.html","5fd385abbeaf8071005591cf1c72d1ec"],["/posts/135355774.html","d1d3d70d1a81f86b80e5193bae45dc1b"],["/posts/1375344716.html","8b72a9d67a92e67d4e4fe1206cd4fb40"],["/posts/1388991698.html","e12708480e3bec504927945cf01c0680"],["/posts/1410315814.html","4c6aab5a5e1fb12f6b196f38050f1b9a"],["/posts/1452790229.html","d41b566239085e4ec7e45cd665da87ed"],["/posts/1470079884.html","d8ebee3b0ead7bff72f409c7e8a9731a"],["/posts/1470079885.html","1295873a4c9e09c3eed5bd4410d11ffd"],["/posts/1470079886.html","c028f591b5a75f90ad035d7ad007f0e0"],["/posts/1470079887.html","e69f0cc8d9aa83c208475508d8a2be26"],["/posts/1498536549.html","336487a683aaae5e5e5abd20278e4891"],["/posts/1547067935.html","8425f09c3a4088da75e12c4f3c011bc5"],["/posts/1557866301.html","5994470c06e68c851bf895ffb009f89d"],["/posts/1571776361.html","e0ad57ed05bfbcbf0be2f8303f70f2c0"],["/posts/1605124548.html","c9eeb34ee7d76dd7c18303876e65c394"],["/posts/1633036852.html","c0620d0533aacc4fb887122ebfc121f8"],["/posts/1765123828.html","df819e0e12caf7fc013106a3bafb48db"],["/posts/1767336200.html","a90d6283f4c1a968c3b174be795df518"],["/posts/1776114197.html","13a4843f0c524ab63fee2fb3c597cf4a"],["/posts/1817748743.html","d43c42d32b1520b1dc135cf53db8d982"],["/posts/1925125395.html","5159be0d449e6a02d4925803cbc1f2f6"],["/posts/1966191251.html","633dd474aeb45cbad103287f807cbefa"],["/posts/1987617322.html","59f2eb3404d8825e46abf87d80398e7b"],["/posts/1999788039.html","5e23d25d7eb7b0408f3153b92fe2277c"],["/posts/2075104059.html","d10a9e3d784b76a7cbed6e8466db7315"],["/posts/2087796737.html","b249d4b763393d2a385eea875a994721"],["/posts/2106547339.html","c537ca89401cd6bf2aba74fe78306dee"],["/posts/2207806286.html","f95c9e1adc120c2df8f367b056b94449"],["/posts/2225903441.html","998146d03e2f08b8a4721e1584eca7a0"],["/posts/2265610284.html","98bf91e6891b1f473328f456bc496fb0"],["/posts/2281352001.html","d04fe3a3dd2895ee84309bb1fa7d6c33"],["/posts/2364755265.html","3d040d69948b9615549bacac5028ee94"],["/posts/2414116852.html","db696436272364860eccd83cc2e2cb7d"],["/posts/2421785022.html","e9296500321ecb63e645be051ccee929"],["/posts/2482902029.html","65911a124ebf0bd91888a5717fb92a3d"],["/posts/2495386210.html","93d1b3c8b71c81e239a4256be4016fdc"],["/posts/2516528882.html","531fdb2bb21e6d0472a0f18288dc9e85"],["/posts/2526659543.html","d2b757a70cb1c2577afea7cad5177fe0"],["/posts/2529807823.html","7f051c2b16255492eb361c2d91a39829"],["/posts/2596601004.html","c5192575f9425294df63e5b54dbac7a4"],["/posts/2742438348.html","a14973738ce9c9986434d1ef36e6f7ec"],["/posts/2888309600.html","94235f56de7a996f9388ee08eccbfb8b"],["/posts/2891591958.html","2d64d85a9daead27333173c8467f8f1f"],["/posts/2909934084.html","fff61e17b45034cbe5b339c1e8493330"],["/posts/2920256992.html","4bd473dd6082be3169112820cbe75702"],["/posts/3005926051.html","a991d4ea15099fde5c619afed08a556e"],["/posts/309775400.html","1ac0483274a80e79141f550a539c4b0b"],["/posts/3156194925.html","c1e1a4914c3d0d47c0f9f32a9dd5154a"],["/posts/3169224211.html","cb7b0e56f83474966841707c3e6ccfc2"],["/posts/3213899550.html","0769ed64f9d734c5776f944ec870a7fd"],["/posts/3259212833.html","e95dd3c6f1206bdb33018861b592d24d"],["/posts/3266130344.html","3bc6d3cb27b5bb08e1c2612a802a8d1f"],["/posts/3292663995.html","a72497af9339dccaabb84efce1299691"],["/posts/3297135020.html","aee280012206ab9a27e6aff33a6ab204"],["/posts/3306641566.html","a2921e12805504177f44269365117f54"],["/posts/3312011324.html","f497881e4cd1ed8463dd7d279b68487e"],["/posts/336911618.html","2993ee74cd18c38931b107e2ae5966c3"],["/posts/3402121571.html","68de170d23e238c08183c35c660457fd"],["/posts/3405577485.html","b136049acfeb2909f9ae9603686a479d"],["/posts/3498516849.html","a5b83b79c4300cc09dd45483fa5c421e"],["/posts/3513711414.html","c94ff2c864ba549eb2b9a0dc242a27e4"],["/posts/3546711884.html","773b15fb71d1a409b507d3b6ec7a7c3d"],["/posts/3731385230.html","36f1ab5e51c2c785b5fbcbe4cbd36845"],["/posts/3772089482.html","43dad0677a4fdf569582ee62dc92301b"],["/posts/386609427.html","5b8e97ee7720b3669fbd9a8b73322331"],["/posts/4044235327.html","9427d7db8b3509cdf1e9a3494e6ad540"],["/posts/4115971639.html","7ee358992c857f59a7ed9684dbecf90f"],["/posts/4130790367.html","4c36eb139924ee4f8bc96b2e208dec51"],["/posts/4131986683.html","327cf83934cb771a77fa47167d03c614"],["/posts/4177218757.html","931ac060d22249148e4b38813bc1b141"],["/posts/4192183953.html","2729b2c58abb6ee447a0761eb31e6041"],["/posts/4261103898.html","ff876a79c960436fc1cfa5b77a2c3b93"],["/posts/469711973.html","d90b262093f1cff389e3f578a7a79eed"],["/posts/482495853.html","bf341a8d57d26beb3f283d652838ae9d"],["/posts/488247922.html","d330f1d1b77860ec0968a65d8bcef654"],["/posts/570165348.html","7f53e8e8e90485e326b181fee3ed00f3"],["/posts/595890772.html","ba7548f6b372a76e62d00561585f2fb1"],["/posts/67485572.html","325f1d69d12c6b134df01099b35a7143"],["/posts/694347442.html","69f66970e63ef14c2fd8b50863e56c3b"],["/posts/707384687.html","2774961033b2c3414f18d32e1860d514"],["/posts/71180092.html","c677aabc07b2d545271e77713b2f7558"],["/posts/716459272.html","fa07fb909ee7a3bb43b93f7f28b15249"],["/posts/778231993.html","673cd90873a73a71f823938d155d9969"],["/posts/795397410.html","492691407b0ed76ea70132313935e430"],["/posts/820223701.html","c1c62389558153284b4c961f5dad98fa"],["/posts/830372185.html","571b97ebcad71dba890998b19a966de5"],["/posts/88294277.html","095740980881ebb6da7977af1073b180"],["/posts/939963535.html","f5d36e2b61b88ad0ba191036b467c2b0"],["/posts/983786067.html","1a71ba6478a8a295376aafa7030b5a3e"],["/sw-register.js","c49a9efdd22a1b2828f17a6b6397f8bd"],["/tags/C/index.html","63c295e3d31378679fece67e1af79ab7"],["/tags/C/page/2/index.html","e010e9e0ad6f5909975918162083f308"],["/tags/C/page/3/index.html","70cf9a00f9eae8754f8e7df3acbf537b"],["/tags/ElasticSearch/index.html","db3bb78c317d49f894cf4ae626d83fb8"],["/tags/GUI/index.html","eafbd81eddde651a572bfecc247ab812"],["/tags/HBase/index.html","3ce820b53e6a8063ddc76469b5212eb0"],["/tags/Hadoop/index.html","4fae9c77230814409b98714fc96748a6"],["/tags/Hadoop/page/2/index.html","49afe7495930d0c1bdc6afb7b5ebe0c0"],["/tags/Java/index.html","47623751c4a8fcce7996ffc75332457d"],["/tags/Java后端/index.html","9340e9bf7c11a3762fc5bc6f3a7632d5"],["/tags/Java后端/page/2/index.html","2af8ab9bd2c6b47a83a7c9cd11e5a423"],["/tags/Java基础/index.html","a08e334b34cd997f499c0b8a1c610959"],["/tags/Java基础/page/2/index.html","4c77501de24732ca82fb8e83d044238b"],["/tags/Kibana/index.html","16c86fb402aee7b7ebcf3a7a2fecbc72"],["/tags/Linux/index.html","d8db74da35b2110274c4bf537ec5444a"],["/tags/Linux/page/2/index.html","532a0dbde426813739662f9def157d6d"],["/tags/Linux/page/3/index.html","2d954724538b26ba1788c18bcd2c2194"],["/tags/Mac/index.html","b7c3d22dff6717c495d08183eaabc248"],["/tags/Mac/page/2/index.html","422f00315450d493a417030bf769ca3f"],["/tags/Maven/index.html","762746ab5bbe4e03ffc98c7b6eed0ba7"],["/tags/MySQL/index.html","4be95e924205fb6e3c1a500f495c51d9"],["/tags/Python/index.html","c5ec11c00e7ebc24983a94aa918ab278"],["/tags/Redis/index.html","3327d25d6e10a0986ddd8890fc4b683c"],["/tags/R语言/index.html","c1c60a74f173d81c92e03e10b1b8f9a0"],["/tags/Ubuntu/index.html","46b339ca2d4233b46b1bd9c7b465eb5b"],["/tags/Vue/index.html","acbb2d741ae00116e0419e5816fc3b36"],["/tags/Windows/index.html","ca70491240b53f7ec1f94e9acf1e7ae4"],["/tags/ZooKeeper/index.html","4c73f656f4a168582be33f59fe01b28a"],["/tags/bfs/index.html","6aed062e69159afbdb60b94ac5156a99"],["/tags/dfs/index.html","c6c78230112b1dcd486618645174f579"],["/tags/folium/index.html","ead919cbbabc996da09cd59039d4c075"],["/tags/git/index.html","46780e12ac70b95c42f9c95f3217bc7a"],["/tags/index.html","c0e2387f7fa6184418728c531c16b084"],["/tags/latex/index.html","53c6c4820ecf31e0d837bbd5ee0ca04b"],["/tags/中间件/index.html","b8c8fa76ea1e8eab29c3b3111d859d65"],["/tags/二分查找/index.html","00e310c1d6462d02cd4f533346f6585c"],["/tags/优化类/index.html","d9ad17b1fbcc2a4fa50923d252263182"],["/tags/前端/index.html","3796bc84996d512456841d65c38258a7"],["/tags/前缀和与差分/index.html","707bf3bc15688bba77ad524db4573ab3"],["/tags/动态规划/index.html","5a17a7025ba4d9d6b8a117112030288a"],["/tags/动态规划/page/2/index.html","c69cc8425ae70fbecfabb05a2d0a91e8"],["/tags/博客搭建/index.html","6f0ebe9d08bea5a4aa9566292f4fef6b"],["/tags/图论/index.html","539fd465bf02ffe96d133cd8cd7bb2a7"],["/tags/大数据/index.html","6460109efbbd5b171b3e6c0f1c3bacdc"],["/tags/大数据/page/2/index.html","d13621d63e7de30eb9088fb0eee0717a"],["/tags/操作系统/index.html","f72691af99b3f82fe6c47881e36c861f"],["/tags/数学建模/index.html","3b55f9acf9838ac5cde66fd9c0aba1a3"],["/tags/数据库/index.html","c323c304abf1481bbbe924c1f3e560f2"],["/tags/数据结构和算法/index.html","c75650a46bd71000300097476cdf4bb3"],["/tags/数据结构和算法/page/2/index.html","749c36ea4d891bf1bebce9010ef2f16c"],["/tags/数据结构和算法/page/3/index.html","e6a2dda974a77a16df8c8c4f1da09d89"],["/tags/数组和字符串/index.html","5c1a9a7cc1d1137ee08e4678b8367922"],["/tags/枚举类/index.html","cb48d23a55aaac3fece45712eb4fc142"],["/tags/栈和队列/index.html","2ece2df16c135d5e30d7dc5378ef8d14"],["/tags/树论/index.html","1dd59b2e1beb7e8dcde58b469e5bf90e"],["/tags/测试/index.html","6cf9b337361a041745580cc74ccdc7e3"],["/tags/环境/index.html","db0a9edaf67d09942c161378de1e84b5"],["/tags/环境变量/index.html","c9def34344d112bd0b205a8969674d65"],["/tags/绘图/index.html","e758ef961ddd3db7e3a9053bb6297e19"],["/tags/编程工具/index.html","92ffd51f4561637796bcb33ba1fd74f3"],["/tags/编程环境/index.html","ad73741ea7c8881bc1fb2a8ec8353ecf"],["/tags/网络编程/index.html","ef9f42ee08453dfb238d6a5325a47667"],["/tags/英语语法/index.html","1cc025a87f7a9c4a5b0c473e6442b9c5"],["/tags/论文/index.html","1a9e05bb46c28d5cf1a4ccc796a846f4"],["/tags/资源下载/index.html","54e7700550527592dab2dbcc5bd493a3"],["/tags/链表/index.html","9d92cc90cf1efe0e452240b331e1465a"],["/tags/集合/index.html","75b08884a1977d13228e40d4eae37ed7"],["/tags/集群/index.html","0020a82ac3a1e2dbc292587cc1bcbcb2"]];
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
