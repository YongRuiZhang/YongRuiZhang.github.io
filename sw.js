/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","1fa5f881f2b18132ee636d749590f581"],["/about/index.html","da4713454a659ca9abc26a5f8f702e62"],["/archives/2023/01/index.html","01d80f7d458afd99cc6485c76ae57e30"],["/archives/2023/02/index.html","452327e76cdf9bfc3a1fc441bd6bab48"],["/archives/2023/02/page/2/index.html","681b7fe025d0ad404b82fb20f32638af"],["/archives/2023/03/index.html","208fe65b7db33f17ccf8f626c9145891"],["/archives/2023/05/index.html","5987f044386f70fbdede9f81c2274d43"],["/archives/2023/06/index.html","bfc7852a73bdcaf11edc6722e9ee840d"],["/archives/2023/index.html","34929cb486cbbad37a7cdfda9f6a666c"],["/archives/2023/page/2/index.html","4ac79329dfafbf9a970787b6a38f2439"],["/archives/2023/page/3/index.html","5794559a98974bc48190fcf724a6058d"],["/archives/2023/page/4/index.html","2130fd6f35a018ffea5801723ed540fa"],["/archives/index.html","c38d744aef7a962a957ee934ad163a5c"],["/archives/page/2/index.html","5d99b29a9a7d0c1b3b7f7667bb7ac276"],["/archives/page/3/index.html","098cc11adc009b01658b40bb984f6084"],["/archives/page/4/index.html","1676752fef6ea514bc8dba03c98cd196"],["/categories/Java/index.html","1d31dbf0d987fe5f90306b4b4dfb5c5f"],["/categories/Java/后端/index.html","7a0ff5c6053aa4d5fde81ec266f36768"],["/categories/Java/基础/index.html","0b2e96db1fc9e9626241c6acda784225"],["/categories/Java/基础/集合/index.html","4c7162d614c451867cdab88d80d1aece"],["/categories/Python/index.html","3cd20b7d8bb6a2fafc718111fa3b56d4"],["/categories/Python/编程环境/index.html","ba19667b7718a93742389719e28d0fe4"],["/categories/R语言/index.html","bf06f4daf1862cf7aa774a5bf0262344"],["/categories/R语言/编程环境/index.html","2ffb0cd075c9f8399001f3c0eb04c0ca"],["/categories/index.html","9de974e920ebe177f6b953e3585794f7"],["/categories/中间件/index.html","facef73e79fa605071a7cfa696508ade"],["/categories/前端/Vue/index.html","f5679f4ee226ad22657f5115f056e146"],["/categories/前端/index.html","553ccb594ceb5d6235e77c41d0ef4514"],["/categories/大数据开发/ElasticSearch/index.html","a748c4f00f91c8f586c81ba5cb202090"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3cae46ffe6bcb7151f6af460e30c8c1b"],["/categories/大数据开发/HBase/index.html","3a174eab0d26f0cd6607d2190d825ce4"],["/categories/大数据开发/HBase/学习笔记/index.html","e5279b718896e527ee1cc5bbf20d7fa8"],["/categories/大数据开发/HBase/环境搭建/index.html","20bacca7c304521bd710e240424c1b32"],["/categories/大数据开发/Hadoop/index.html","44b467e60e9e30b7841b6700665cf1a4"],["/categories/大数据开发/Hadoop/技术/index.html","fed3be78b13a3f7e2d2e5710959feee9"],["/categories/大数据开发/Hadoop/环境搭建/index.html","54dbafcd6d05abcc2037ee8b752586c9"],["/categories/大数据开发/Redis/index.html","d6d5d29b48791af4f8e8650fc20967f3"],["/categories/大数据开发/Redis/技术/index.html","a8d2698b299697efdc7a607f7bc5442c"],["/categories/大数据开发/Redis/环境搭建/index.html","d6287b768027fa8ce1e4b8545a7584b8"],["/categories/大数据开发/Spark/index.html","501657094fdbf63b2f6d4155ae92f887"],["/categories/大数据开发/Spark/环境搭建/index.html","f343cdeac2ed9462be7d5d2b67776dee"],["/categories/大数据开发/Zookeeper/index.html","920a4f2d9e2dba87f8aede748eb26888"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","5090fe39781f0a2574cf1610c145fa3e"],["/categories/大数据开发/index.html","c0b4749c568217e7e501d01c9136538d"],["/categories/操作系统/Linux/index.html","f98bce21a977e667f4bfd0f8b27ae52a"],["/categories/操作系统/Mac/index.html","499005413b69bf53a0b9dedc33876825"],["/categories/操作系统/Windows/index.html","bee5df88b11fe3e1a6e455ceb1bcb6ae"],["/categories/操作系统/index.html","bb3e0a692b5c43df60edcaeaebc8d270"],["/categories/数学建模/index.html","600140222a75206f5b884689361b9996"],["/categories/数学建模/latex/index.html","1efa0d8107f1e11a5851989ce10d4b37"],["/categories/数学建模/优化类/index.html","bd4c44bc2639fa0958cfc36142501982"],["/categories/数学建模/优化类/现代优化算法/index.html","14ca70b957b026ca7b5121ceb3e1aba8"],["/categories/数学建模/优化类/规划类/index.html","d3183d9676467876f68615e52ddbf718"],["/categories/数学建模/绘图/index.html","7a867b83b2e987c34d04427c0e7162c6"],["/categories/数据库/MySQL/index.html","716318fa02cf9007930d772a7922f741"],["/categories/数据库/index.html","3509b1f17c4b31bf7af5bf9d07d063ac"],["/categories/数据结构和算法/index.html","384f3d679c0fedd528f0b1375dd48f84"],["/categories/数据结构和算法/page/2/index.html","c474b58f14ff2956e7a59632946205a3"],["/categories/数据结构和算法/基本原理/bfs/index.html","748ba5dea6434e9d93754fc510bec7e4"],["/categories/数据结构和算法/基本原理/dfs/index.html","d87285ea919f14ab705a3dd97cee1ba5"],["/categories/数据结构和算法/基本原理/index.html","ab823e7cc23ae65a1f4afdf617b30756"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e050f382a07342c249bc0b75bb0005d6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","9e00160b2c924dfa95169de8673e91f7"],["/categories/数据结构和算法/基本原理/图论/index.html","2f928fcb3e387fc149b69d7926feced9"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","78ababb1fc9a18455455457565ed1704"],["/categories/数据结构和算法/基本原理/数论/index.html","5d545b5a962863423305a0afc0fd5ff3"],["/categories/数据结构和算法/基本原理/树论/index.html","1cf893abc73b6d0334ca7b369b4db67d"],["/categories/数据结构和算法/基本原理/链表/index.html","c09f3573fd60c6fd0bb05ec9eb7aac3b"],["/categories/数据结构和算法/算法题/index.html","2158645d9b23297f03a7e372cadab0a6"],["/categories/数据结构和算法/算法题/二分查找/index.html","f2e2ca857db845d650a023c201f65d13"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4217e52e4cb28204181344fbd8688c93"],["/categories/数据结构和算法/算法题/动态规划/index.html","e778444304d3b8e8240399bb717e2caa"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d9f6f96f129895ae34e8e862041c0cff"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f412217b0b15f40bb147003e5dd49dc7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a794bf1196835fdc7445fb5e290a15df"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2c7e0eed3034082ce5f5c3d42c83afa4"],["/categories/数据结构和算法/算法题/栈和队列/index.html","14f400e098b8b7808bf18848f59791de"],["/categories/数据结构和算法/算法题/树论/index.html","bba0589cc51abef07ee2cc17b6a287d5"],["/categories/杂七杂八/index.html","e3be92ae174feb238bc316d15695d914"],["/categories/杂七杂八/博客搭建/index.html","a35fc60dc2f19080d34afdfefaa44753"],["/categories/编程工具下载/index.html","52e06b73e083696160b480bb6b4fc63e"],["/categories/编程环境/index.html","9645d429cc04ccc50407407fbdedb3a4"],["/categories/英语学习/index.html","4173c4ea99709a1ef2fa9dcfd19e5741"],["/categories/英语学习/英语语法/index.html","ad79abe4dd350c28f08f703eeaaf28b1"],["/comments/index.html","890bfea3f8696357e2a23770eceba410"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","8caf93a9d15dd2868ff349f2f2fbe96e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","54cd09f9ae1524cbd2cf3df21f902df5"],["/movies/index.html","0ad98b76a9542f379ed755e1acc57e27"],["/music/index.html","22295b8f6f0f0e72d4470e2bcc6b5bc2"],["/page/2/index.html","6839f9397a5878665d87c146c161d56a"],["/page/3/index.html","22f124b95a9860b406580deafad00513"],["/page/4/index.html","a70e21bcfa4cbc23613263a4256d2ecb"],["/page/5/index.html","068aec844d1d95f532df8a26f1a54de1"],["/page/6/index.html","fce36101f95204be8ec612ef8eff7f1d"],["/posts/1021360842.html","b6bc9397610d9fdec1c8bfa5eef8d519"],["/posts/1120620192.html","aa4957b3471a908fdeb13efac1b988e7"],["/posts/1141628095.html","95203b45aaf735e659c86fe0ca3ca431"],["/posts/1168613674.html","5af47838758d95d0d711879a08c0e332"],["/posts/1219920510.html","680ec3015120a04f3009137a7cc9102a"],["/posts/1222166338.html","d12b7490655105021578e11325553410"],["/posts/1259097482.html","ed754aa7ab7bd2445d78be2cdf95b631"],["/posts/1271036369.html","50779b2bf703aff27dc560da73d57ef5"],["/posts/1312847445.html","b268a624682061c8458e87724300e5ab"],["/posts/135355774.html","e687c2ff2fd9010867dda3b03ba97c27"],["/posts/1375344716.html","e7cf41aaec4e81897af0e2a5441bc7e6"],["/posts/1388991698.html","1daf1c82f34a02297f4b8b6d67f88808"],["/posts/1410315814.html","66421dffe88c6355345c529bfd035451"],["/posts/1452790229.html","21702e4643cfd921d0ff906d2c12bb13"],["/posts/1470079884.html","647336cd9b6eba8741e2ce4aa188b13d"],["/posts/1470079885.html","36fb1a63897fc4a195958af6df1945c5"],["/posts/1470079886.html","ad9d0a050b8c0485088d2c9c2335749c"],["/posts/1470079887.html","809559db12d3bf24023aad8bc8397d5b"],["/posts/1498536549.html","a1c411e09f12fc191a1303c9a35b77fe"],["/posts/1547067935.html","568c0d997694f0ce971143e9f2359130"],["/posts/1557866301.html","4c6ea3c4e32da5c510d99daea4e7e036"],["/posts/1571776361.html","50eec17bdef4b20dbbe8eeaa081d8b3b"],["/posts/1605124548.html","38d53c2e553f80beeb34239a9cfd30e1"],["/posts/1633036852.html","1073679afca4cc433bc85fd4c66702b0"],["/posts/1765123828.html","62b74cd48d22a5279836e09ee345ffad"],["/posts/1767336200.html","5139eb4c9d89c9151bcbefb36780d323"],["/posts/1776114197.html","b4b153bcb45609a0d0e20f0e2e7f8631"],["/posts/1817748743.html","bcfd37b4117ec2790557239100677f66"],["/posts/1925125395.html","7d2c9fe3b6f4719ecbe3905a8caf20e3"],["/posts/1966191251.html","1fc98c35093fc6447ddc06a156ef8c8c"],["/posts/1987617322.html","3ed6dfb95bb35358cae595f659fdd783"],["/posts/1999788039.html","8f80a6bf13060d13797480b5375b9993"],["/posts/2075104059.html","4e7e18114b98bfbea36020b8b9c2b30a"],["/posts/2087796737.html","aa086dabca8880c57d392ea826cb269c"],["/posts/2106547339.html","8761e51b2166cf9bd7cdc9e0941e967c"],["/posts/2207806286.html","3eff658dd6af41bf44784e7eabde5879"],["/posts/2225903441.html","c0fbf10b8c8327e3e311ecbc1bd9212b"],["/posts/2265610284.html","bd89b061bd884025e2c079c2b94bf322"],["/posts/2281352001.html","61c42e254c46dced17812c94451d487c"],["/posts/2364755265.html","f0a83e7709b6d274e123dffcc9ab4acc"],["/posts/2414116852.html","531950de4a71ff738d508ad96514c406"],["/posts/2421785022.html","02bed2e9fffc2372a9838a9c0db660fe"],["/posts/2482902029.html","910d3ba709e7cddfb2fdf46152ec8f24"],["/posts/2495386210.html","70aa107b6e61a8e0536bb44ebfd71eaf"],["/posts/2516528882.html","abea2dc2f8bede51945eb23a38c9665a"],["/posts/2526659543.html","262c15f3c22d98742ecc5c19c0ca5b96"],["/posts/2529807823.html","940f46dcd405bdbe0ce9e80400c5ddd9"],["/posts/2596601004.html","dcd40f8a0a56900afd4ec2cd391b424a"],["/posts/2742438348.html","ee784ff17199c422886fde20f866db97"],["/posts/2888309600.html","47b57c6945a7097403766985c79bde21"],["/posts/2891591958.html","f4a3f8ad67627850c18dcfd3c8441f93"],["/posts/2909934084.html","314b5e90bc1a2640a3f2f400438ce798"],["/posts/2920256992.html","44d1a0c14f4c458b59737e767ac747b4"],["/posts/3005926051.html","18b67dbba120aed8de521cf003c82531"],["/posts/309775400.html","70e29639beaa72e200bb035a5be47e8b"],["/posts/3156194925.html","52e0cfe2ed1becce81f1e59d9aa64065"],["/posts/3169224211.html","317fca637f128ade92d3bd0f887b2be7"],["/posts/3213899550.html","eb862d97f3285f19ea1e1a703cfd2c4f"],["/posts/3259212833.html","d6dfd52e58af169c0c8c7058b37c3d26"],["/posts/3266130344.html","3569f58dff8569da6f7a654f4e1c8fb3"],["/posts/3292663995.html","1809b649039aa7809b3d89f40f494366"],["/posts/3297135020.html","5ee8ac76787812eba5560ac45de12fbd"],["/posts/3306641566.html","40d7c09eb7a78db5d97f51a273210abc"],["/posts/3312011324.html","b02b3ebf63d86895a5b1d56e5e12e0a5"],["/posts/336911618.html","1f2cd7c188160f2a9663fb60532654fb"],["/posts/3402121571.html","d0a64350347bb3fffad830fcc71865fa"],["/posts/3405577485.html","3788c8d1c14452447462a0e82690933c"],["/posts/3498516849.html","5bd95f37cbde9dd12e67feda24b68fbe"],["/posts/3513711414.html","87d549ece4675fa9a97404e23a1f1cf7"],["/posts/3546711884.html","a5bcd0d6d53ae1a473fc1010a229d795"],["/posts/3731385230.html","345767ce5348d1377a7738d53a085942"],["/posts/3772089482.html","01ee71ef9b53b22af58a465235a6ddee"],["/posts/386609427.html","25e9994a26c577fc42502948de07e2f3"],["/posts/4044235327.html","2ae42345cc81beebdf645bf79e8a8246"],["/posts/4115971639.html","da2ac01da60389de0b7572889255ed1c"],["/posts/4130790367.html","b3c2997e84c697bb37bcc21c77cffa37"],["/posts/4131986683.html","6e8494d102a0bb0980459bb8267fb057"],["/posts/4177218757.html","7c7bc21eb0e655001bee617b0098295c"],["/posts/4192183953.html","930f7ee64f42224c2ae1237a9267570e"],["/posts/4261103898.html","dd4911ae31fe3b182bb3c47de6593b7f"],["/posts/469711973.html","2754ff916131c6c5b0762b277c787c73"],["/posts/482495853.html","8ae175228abe96420baf24ff66d54911"],["/posts/488247922.html","5ba5d09af429c3dd92a4b488ab2a4bc6"],["/posts/517302816.html","0f2fb9b30bffec314d37ce12114a55b4"],["/posts/570165348.html","8e4ba4e4f599b0646467de7a83e49a3e"],["/posts/595890772.html","e642169477a0d8d4fa2ff9e4bc68e742"],["/posts/67485572.html","1016de586b3881f7a7f20e3abc0c5a53"],["/posts/694347442.html","9086a4eeb59219444760d1db5ba91773"],["/posts/707384687.html","dec392e9dda1b69dcf2c5a0d77e6ff11"],["/posts/71180092.html","e2d69e5f5257eb20daa2f91d1bffd5dc"],["/posts/716459272.html","68b86b11373c25795803bec3a974b62d"],["/posts/778231993.html","27ebbc6cfdccf4bc326c550abf2336b6"],["/posts/795397410.html","aa9b559fba2d8fcdc9ce8ea9478cac76"],["/posts/820223701.html","506ac4356ef14f75b95fb9f26a2e6aa9"],["/posts/830372185.html","39df97f85daf689d789b8276e4579481"],["/posts/88294277.html","41d6e127fd122b943ac76a9d0d768a2c"],["/posts/939963535.html","6466fefb086c92dbf3a3f5efd8ff13bd"],["/posts/983786067.html","eaa9daefe054746fc0a79e37c0d77af9"],["/sw-register.js","72220527cda15873399b76280b9ae1c7"],["/tags/C/index.html","3587ea2e7b4fa87055136ae957786b9e"],["/tags/C/page/2/index.html","5fb2016274c82b6d1ce3be78671f0ad7"],["/tags/C/page/3/index.html","4bb979895420d2451f0505fd56f648f1"],["/tags/ElasticSearch/index.html","a09becb5c6275e08745cafec575eb469"],["/tags/GUI/index.html","7f5e3495af808ce4dd01e155fe387070"],["/tags/HBase/index.html","7719013b9ea3dd58f903b87dc2de81af"],["/tags/Hadoop/index.html","b58b9edf4b78fc37486d3dddd9070b70"],["/tags/Hadoop/page/2/index.html","bc287a698b3be74dc0abd17aeb634ceb"],["/tags/Java/index.html","70f5b5f262beda2b8bbc039f855906a3"],["/tags/Java后端/index.html","5c74fcf5f29b3328ad6031f04ee1324d"],["/tags/Java后端/page/2/index.html","cfc58da118647c04f348ce31f8d0b981"],["/tags/Java基础/index.html","706d144915447d02a7b88b481e3bb411"],["/tags/Java基础/page/2/index.html","d2803adab59b6e2c7682090b2971614a"],["/tags/Kibana/index.html","97d371a00e93b96d5408f81312014d2b"],["/tags/Linux/index.html","92168ebf44e9ef14a9c79b47037be245"],["/tags/Linux/page/2/index.html","910beb08f389a20678e31eb6efb7e22a"],["/tags/Linux/page/3/index.html","eafff311164e93a45975bc7b5da772cc"],["/tags/Mac/index.html","5af9029ac716746ed305a224ccdaa6b9"],["/tags/Mac/page/2/index.html","81b1929e1c845f98353a14984d8f1d4e"],["/tags/Maven/index.html","7bf9967a64baefe158c58655edf3abab"],["/tags/MySQL/index.html","c26a640be24d81702489e566c07daaca"],["/tags/Python/index.html","8945f6e997ab9b45ec1f1eb60409522b"],["/tags/Redis/index.html","5e52ad1a612700c3c60a68fb2cf8c8fc"],["/tags/R语言/index.html","4908c1d28c055859fec24730c47d1a78"],["/tags/Spark/index.html","fe14dc71270980195aa578799cf13f81"],["/tags/Ubuntu/index.html","d08e5ec3e6ab57330cefc9598eb702d5"],["/tags/Vue/index.html","c68405eee92858058a27a9385f06c92e"],["/tags/Windows/index.html","b1c8a310305f433360fc96fbccea7bed"],["/tags/ZooKeeper/index.html","870544681e0afec9bf31d358cbc801a7"],["/tags/bfs/index.html","5a34db22ce83f09a3638be44f0ceb56e"],["/tags/dfs/index.html","8e0f17a2739f76b882ffd269280ae020"],["/tags/folium/index.html","daff4f9d29523b99177a1f2702bbba9d"],["/tags/git/index.html","f91662e7a5edaad4741d7a144035dea5"],["/tags/index.html","1f1f991292e80adfd16bdbe3af63abb4"],["/tags/latex/index.html","49db5b75f1176494940f3cb5b6ce27d9"],["/tags/中间件/index.html","635d7d8d191f9530dc0e557e28753167"],["/tags/二分查找/index.html","18b86d1f20ee756f3d549a13390176ae"],["/tags/优化类/index.html","106a3730f154044d6dfbab328993a4e1"],["/tags/前端/index.html","5dad66e18f377e730435e887bc171817"],["/tags/前缀和与差分/index.html","a07f84b69ebd8ec425b06cf82e7ee5e1"],["/tags/动态规划/index.html","875b80f0d7cfdf60000da609e9328d28"],["/tags/动态规划/page/2/index.html","cb6c654118c169fcfaa34b05e9338eb1"],["/tags/博客搭建/index.html","fdf679ddc6d1f5c45e075065ba6740c0"],["/tags/图论/index.html","5697fa72dd9f94988629da5a19395f50"],["/tags/大数据/index.html","6a05aed076df038266ff8cca66e5badd"],["/tags/大数据/page/2/index.html","f96d19ef6d9b983a2a4bd56db6e7232c"],["/tags/操作系统/index.html","7605d688f0cf41f2a9d523191e0f2df9"],["/tags/数学建模/index.html","afa6ce17754b9dc9b8aea028669fc0a8"],["/tags/数据库/index.html","711345be4513982980451df3fbf4a63a"],["/tags/数据结构和算法/index.html","469b7be91cdd278dcb2c1160e6bde3fc"],["/tags/数据结构和算法/page/2/index.html","8fd76a57e142e7eaaace629abd3080c3"],["/tags/数据结构和算法/page/3/index.html","46d3223d114585fef8fd4c8b27af798e"],["/tags/数组和字符串/index.html","7ff6e30066f237d3cac749f49eacb2d7"],["/tags/枚举类/index.html","3818fd6fabf1fbdd6a5f22805fc2f406"],["/tags/栈和队列/index.html","0548b84588a55227429c624c10c1f23e"],["/tags/树论/index.html","9e45df9adfa19870eab3f5fa9459f851"],["/tags/测试/index.html","42308aced11fdacac8923e451f8e590c"],["/tags/环境/index.html","74ddc36e92c1aa05d59ebae83260b5aa"],["/tags/环境变量/index.html","79d8de249993a97a45082952e30ce530"],["/tags/绘图/index.html","3382898cec85e58faa40212add979a5c"],["/tags/编程工具/index.html","9abe0578277b31cec25abc7d5318bc3e"],["/tags/编程环境/index.html","ba79a52619f219619698c025a7bf3bdc"],["/tags/网络编程/index.html","7dd0048b9ec6d9b9883ab859eddd22e1"],["/tags/英语语法/index.html","817b8d0c87578145c9fe89680a2872bc"],["/tags/论文/index.html","65690a6d2fe2d2adc684bd3b83179714"],["/tags/资源下载/index.html","2a29fa5585a0e85e091a85953e4bad85"],["/tags/链表/index.html","734409dff47a48a7df6c7604f31b74e9"],["/tags/集合/index.html","aa9c541852a11b9f1406157f57a37010"],["/tags/集群/index.html","6e4e60905cc8bbb91726e85f4dd7bcff"]];
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
