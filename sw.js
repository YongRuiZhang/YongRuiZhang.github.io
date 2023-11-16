/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","b29121464ce7fa95d2627b56e223ebfc"],["/about/index.html","632924077c9b4b1172d12398c8725b8c"],["/archives/2023/01/index.html","f893169a8b3cfb6d3d449f8c31644cb3"],["/archives/2023/02/index.html","09b1c7e3460a4ed81d4853e2578830c8"],["/archives/2023/02/page/2/index.html","13e5c06377ff8f3ea5e1550ddc2f871e"],["/archives/2023/03/index.html","6c372fa593888c3d8e91afd4a46286fe"],["/archives/2023/05/index.html","d6cecadd71ae200848b6de50b78cb69e"],["/archives/2023/06/index.html","80446fa6734e79500a042807df2a83f6"],["/archives/2023/09/index.html","d456ae875b3ac17fe2dc49f72c6c962c"],["/archives/2023/index.html","1a27f8f8df0b49c76ac8e7ad32831862"],["/archives/2023/page/2/index.html","75006bdd877af98063398dbfcbe5684e"],["/archives/2023/page/3/index.html","1aba0612defacb496d506ebb6fa38f8e"],["/archives/2023/page/4/index.html","dacca90a9e97e8a8712d93fdcb6d27a1"],["/archives/index.html","6384ebfe338b46a1105e764922dd0499"],["/archives/page/2/index.html","e898e19261f697810731fdd89139b8fa"],["/archives/page/3/index.html","6c1e4127bd82ea19794eb267037098ea"],["/archives/page/4/index.html","f474a29608c246d1d3d6882fe07b572f"],["/baidu_verify_codeva-qQP2iZOMLX.html","6fe8634ed8376b3ac2d80e7f1ff4460d"],["/categories/Java/index.html","4298ab340310222501bd9f32ee45947b"],["/categories/Java/后端/index.html","de2b124a53cfe0d9a4f3724712c69d2c"],["/categories/Java/基础/index.html","2b7b996c69cd9c13dfd2913d464bd6cd"],["/categories/Java/基础/集合/index.html","e581ebd7e051b4b666881dec69c6c501"],["/categories/Python/index.html","6f0ea070e12feca04727e62f85114bae"],["/categories/Python/编程环境/index.html","ad22ebcd5eafe467abd8223a35fb3026"],["/categories/R语言/index.html","cd5a50aaa1bbe9568dec3f232516bbb0"],["/categories/R语言/编程环境/index.html","91d914c8eb9de41bf79f749a9ad3564d"],["/categories/index.html","3c86de88879ea139438397c72c2cd956"],["/categories/中间件/index.html","635ca93ece65f648910ebebb460a986d"],["/categories/前端/Vue/index.html","74b0a554131bd88edce8dd4533b1ffb1"],["/categories/前端/index.html","03f2c1a55464bd7c5718576820719833"],["/categories/大数据开发/ElasticSearch/index.html","befdb085acf6ab37c366128115d7258c"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d88c7c22a559be0e776a30ef445f896e"],["/categories/大数据开发/HBase/index.html","074d45bce404eb32edc850a099facfaa"],["/categories/大数据开发/HBase/学习笔记/index.html","9ab0fb638ef093585357bedde99ab986"],["/categories/大数据开发/HBase/环境搭建/index.html","9bad38124628836fe20ae2adc8310eec"],["/categories/大数据开发/Hadoop/index.html","c7776a0220cb3d42bc54d5dcc82ca56f"],["/categories/大数据开发/Hadoop/技术/index.html","d20ebb91d89174fcdbefb4dc3973b006"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a4face7fdab564db4d6004b526266bcb"],["/categories/大数据开发/Redis/index.html","3f1c4b0578f42923c15179759bf9bbbf"],["/categories/大数据开发/Redis/技术/index.html","e80bb2321fd1fad2204bfcd433360eb3"],["/categories/大数据开发/Redis/环境搭建/index.html","979429be4b8b98be3401654393ce5de7"],["/categories/大数据开发/Spark/index.html","34638c79d42688d94d5133945074c643"],["/categories/大数据开发/Spark/环境搭建/index.html","09536f64bd4b0cf3136ba30f95868cf0"],["/categories/大数据开发/Zookeeper/index.html","6154f5b83800b749161eb76d3574209e"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","7c14f4d6ac567f15939f9067dc5afcad"],["/categories/大数据开发/index.html","a5df04d92dc3a0e8912991aa389d38e8"],["/categories/操作系统/Linux/index.html","f9aa57676d172e38222083de21ff6e1a"],["/categories/操作系统/Mac/index.html","33ccde45129a096f18ea612c50966bd4"],["/categories/操作系统/Windows/index.html","f93a6f04c01ef5530b8535ead4a43159"],["/categories/操作系统/index.html","2c20fdec540ec183e0f19ff8a4259c20"],["/categories/数学建模/index.html","de3532a598a22ff2658f8bb42da8c250"],["/categories/数学建模/latex/index.html","fad563d2a3506fe939cce5a0a860eeb2"],["/categories/数学建模/优化类/index.html","97b0924ec70424962d52d81db8cc6ebd"],["/categories/数学建模/优化类/现代优化算法/index.html","793d3b6b8ac1191150550fadebe0c838"],["/categories/数学建模/优化类/规划类/index.html","7cb1f65943c44f0271c33e9f718fbd63"],["/categories/数学建模/绘图/index.html","78a74fc1fe6bf0263cb2c711c76cbe94"],["/categories/数据库/MySQL/index.html","361427b3238d4bf4bf1cc12d3b6ec970"],["/categories/数据库/index.html","d6cfc91ba016cbe23f4775362fa57d43"],["/categories/数据结构和算法/index.html","982fa3fa606d6de908bcce7a74e6e90e"],["/categories/数据结构和算法/page/2/index.html","1a8d637f465db7aefd2102840056fec3"],["/categories/数据结构和算法/基本原理/bfs/index.html","64b69402dfe15ed9e7cd7540d5242289"],["/categories/数据结构和算法/基本原理/dfs/index.html","21ac7c9334d7863b6e173469eccb6dd1"],["/categories/数据结构和算法/基本原理/index.html","2a9c04f5fcb9482c56cab5c2ffb656f7"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b50fb4d4dc2f43e17831811da8a4df22"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","15f7dfc04316b464733ea316dd0f9f94"],["/categories/数据结构和算法/基本原理/图论/index.html","9f4cb2f1db9e9e21641f32667055bf64"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","e06a51302dabc50a8ff74dfe2cb54574"],["/categories/数据结构和算法/基本原理/数论/index.html","f0fdfd657c7f31d855686c83c4eff223"],["/categories/数据结构和算法/基本原理/树论/index.html","403c18ee48b3fc484889a9528b8d82c4"],["/categories/数据结构和算法/基本原理/链表/index.html","5772e915d6ebddba4b2e9f3c5a5ac302"],["/categories/数据结构和算法/算法题/index.html","c1ce24d6da675b35c56977ca5141c3f5"],["/categories/数据结构和算法/算法题/二分查找/index.html","3984f412a99276d0a1f07cf3f845653e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","44c73600408f37ca77a5445fec9c613f"],["/categories/数据结构和算法/算法题/动态规划/index.html","259c71fbd8957668eb0bb07e88c92fa0"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","ff96e288d2b031b84878b54e7c44977c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3234b14f8bf02e4b5dbd5df6f30570e6"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a1a289adaad593ba387feb8d5625c8af"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","87d8e0dcc943f195a1d641ce676a497b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c63019f0c90f193510ef8b711c29351a"],["/categories/数据结构和算法/算法题/树论/index.html","4289cb566c0a514c91946920722187fb"],["/categories/杂七杂八/index.html","585a9801b114de1ad3930228e6e5a0a5"],["/categories/杂七杂八/博客搭建/index.html","92633b0def267b53533a09ae073752ab"],["/categories/编程工具下载/index.html","ea078ba5a404f5d021033656cd39ff3d"],["/categories/编程环境/index.html","f8747ddfd76a54fe05ef091a50167c33"],["/categories/编程环境/大数据/index.html","a3c9742641f9d7a2474886c992643237"],["/categories/英语学习/index.html","da6d52e81fdf158eab910f732d5a923f"],["/categories/英语学习/英语语法/index.html","8308a43100639415a4186ed7771029be"],["/comments/index.html","33d17a9608dfd3b2d757d4095fb3817a"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","4239fac85d060742959e2fa7c726abfb"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","afba1caf1d94b4cfe58c771a5292492f"],["/movies/index.html","edd3bab6ee6b0416db1aeaf2cb1f3c0a"],["/music/index.html","fe35c1cc076b13f8c98b5a9e45b4a014"],["/page/2/index.html","df4ad071c5fecffbe7a2a9189cf64d73"],["/page/3/index.html","36fed0d8c0201d0e6f5c96d36fffe33b"],["/page/4/index.html","1ef0b1289548576f1d5fda776f019113"],["/page/5/index.html","97cdb84c5303c5b42c8314fda0c5f87d"],["/page/6/index.html","75f35dc80fae006a5add15446a3d0c15"],["/posts/1021360842.html","241d6bcf6cfb0f3c1f8327d143480a9e"],["/posts/1120620192.html","9b1aaeb7f6c9b937231b39ce352ca0ee"],["/posts/1141628095.html","6ca6e2cf66da2867bd64d8cbc8b17350"],["/posts/1168613674.html","a899e3704b1f28aa324cdb2e3a781841"],["/posts/1219920510.html","fde14f9c3dcb540ec8157f048fb77023"],["/posts/1222166338.html","6efd26fd09bfca3e89bdf1d363b5c6fd"],["/posts/1259097482.html","d332719b71a204412d36d3f7222545c2"],["/posts/1271036369.html","5990936bb834a895a974f4a4a4b449c8"],["/posts/1312847445.html","5331bd79d234e507fa1592ec6af022de"],["/posts/135355774.html","f515dbbdbea396ec99297c9c39eba489"],["/posts/1375344716.html","09267f499974cf9a7e30142dc7105549"],["/posts/1388991698.html","d694a5b514a1ead4c5abcefeac37d63f"],["/posts/1410315814.html","6721ea565bc89bb36bb9efffa3e62d73"],["/posts/1452790229.html","45c26bb264d21a4eeebcdb33c90c96c1"],["/posts/1470079884.html","c0c3530d64a9cf4bd0a7be0c5629bba2"],["/posts/1470079885.html","5a3213e85ad0d0cc91fd9790e1c6e361"],["/posts/1470079886.html","b1eb25e4627917d37ea474c93cd66e32"],["/posts/1470079887.html","7db929ec2133499f15dccdafa1b1a012"],["/posts/1498536549.html","4ed1f29de0809c793fbccf5d11cb25fa"],["/posts/1547067935.html","682d359e4749308f372ab9e37d9a92e8"],["/posts/1557866301.html","58a04b6f13aea982b01b6427ca9523d4"],["/posts/1571776361.html","1ecace31164e08e00e678d920bfeed55"],["/posts/1605124548.html","ac3346099d0b20d51e71573067c75035"],["/posts/1633036852.html","cfa56793ccd121ad660cd4b3d23fd332"],["/posts/1674202625.html","cd84aecc04bece621ecaf548e165d364"],["/posts/1765123828.html","c9b7bbea8c2a165720a628041c207f3b"],["/posts/1767336200.html","c515a106379420ebecb8c4bb9cc1b1fe"],["/posts/1776114197.html","c95a9f3432d49a3ff9ed4c687d4ca747"],["/posts/1817748743.html","6c026b981936b4314b39deacb21d4ec5"],["/posts/1925125395.html","5c53177605f172020fd6d225129ff959"],["/posts/1966191251.html","cad2cb992b1127fe25372a48da874deb"],["/posts/1987617322.html","68c97a11e78f04e75fd9bf561dd02f3f"],["/posts/1999788039.html","c88a9239d66a8053835ad58d3a1e3fef"],["/posts/2075104059.html","825f064c893c84e96a7b03a9a28f46ea"],["/posts/2087796737.html","20fa425f5977f7cf6cace4653271695e"],["/posts/2106547339.html","97b16b6a8808cacbbb63afc6c0989628"],["/posts/2207806286.html","3066c8e2624e33ba7d3870ea15075e75"],["/posts/2225903441.html","74a75b3f9302abf9bd4169d37027551b"],["/posts/2265610284.html","ddeae30b6b04c5135bdf0bb73ec71a61"],["/posts/2281352001.html","cfbcf114f012918d5f6dcff5e40fe4ed"],["/posts/2364755265.html","b672d0710e7e5df3e52dc305b2187580"],["/posts/2414116852.html","3a72cb40b797bb65274b2dab97d71bb2"],["/posts/2421785022.html","47403f5cb3389cb8a68fbc6fe0bc8090"],["/posts/2482902029.html","fa24bec853f6446f0948e83793dd4003"],["/posts/2495386210.html","aa28006c5510cff68911fcb0a68a7c3f"],["/posts/2516528882.html","4a73817bf567d0b7a5e9041ac2ff1e0d"],["/posts/2526659543.html","86309fa122819bb252904751ee74b9b3"],["/posts/2529807823.html","3dcbc423193c0aa5a3a3e00e88be343f"],["/posts/2596601004.html","a8263759b55a131d9dd05bd58fa2edf6"],["/posts/2742438348.html","6969a1fa2fbabc2149d2ef9f76204400"],["/posts/2888309600.html","6f14201f353ac2d5c89040325fcc15b6"],["/posts/2891591958.html","60f9461291c64896e6c88eacbd4eb87f"],["/posts/2909934084.html","d16cbef9a433223496db6d10c678f766"],["/posts/2920256992.html","9c29af62bdea1439de85c75d36b16362"],["/posts/3005926051.html","9c6dd980f9094b2155a512cbd5195800"],["/posts/309775400.html","ca869ed89ac36eba53040eac3beb1d3c"],["/posts/3156194925.html","6c7d3a847aaeec6d89d6e1403b38545d"],["/posts/3169224211.html","c5128d0ba8be4e053ae72d1523afb98b"],["/posts/3213899550.html","7def08f4161d9ac80dba76b522c615b5"],["/posts/3259212833.html","46a7360ebb4ce8a9e5520373a2f9ab90"],["/posts/3266130344.html","12b9c296183e9c41a1d233e9a5ec02c6"],["/posts/3292663995.html","ca7bb89096003d11338f43955e558053"],["/posts/3297135020.html","dde97b1b63bd05fd8125264ad51a20c4"],["/posts/3306641566.html","7fc52614268f8246518abaa921055b37"],["/posts/3312011324.html","15cbc9bf1b3b75de744cd7dd010262ac"],["/posts/336911618.html","0a17d85216b1b0c218feb9b7c5c06044"],["/posts/3402121571.html","69cdff19338702fb5788ee94d5b384ce"],["/posts/3405577485.html","561943c38a372303dfc1518f8ff3d7b9"],["/posts/3498516849.html","e6debce6cecc9260e02d3269c894403a"],["/posts/3513711414.html","47379b8ed723d71d02febb8c9ad43c11"],["/posts/3546711884.html","1d3c535ac2b98f88746c3b918ae0479c"],["/posts/3731385230.html","d3bde285cbe805216a9811f640ee377a"],["/posts/3772089482.html","b352c7ac349f60b4dd89c0d99f0aa3b8"],["/posts/386609427.html","c3018050d3ed4c685d3f60d017ddb828"],["/posts/4044235327.html","319aee1806c4c374da7ee328931a1407"],["/posts/4115971639.html","3cbd9cc9c8c50d6e78d7bb4721401269"],["/posts/4130790367.html","f245325ca000382f99d210c6b0422fcb"],["/posts/4131986683.html","691bc9f69b4674737fba982d9b3d71e1"],["/posts/4177218757.html","c217c4f9b4c9bb2c2edd65cb02a3f760"],["/posts/4192183953.html","982f57eb96fb23091389e92694b8100e"],["/posts/4261103898.html","bdba203f132fc86b5cfe002d16bc331e"],["/posts/469711973.html","9b386be1336a0249281f031ca5d6e560"],["/posts/482495853.html","4084ea37b432c3b8f2da60533e39250d"],["/posts/488247922.html","7ae40a260074b88a5da1177cf22a76e7"],["/posts/517302816.html","18ae9c12dc2598063ac425da06db9fd0"],["/posts/570165348.html","1c60e59a14f9e46e3874b32849f1e76c"],["/posts/595890772.html","d2239ad392f97611c602c682a7ed9f36"],["/posts/67485572.html","a2bd8a7f826a820d2b15d992305592b6"],["/posts/694347442.html","339493fa0cc6a99ea20fa04e5b2c0405"],["/posts/707384687.html","ec35cc7b4f318e6c49847bb1962de7e4"],["/posts/71180092.html","918c3e1d984f3a9601cff64d5bf3e604"],["/posts/716459272.html","46263ab06e6881eeb06ca8469849aba6"],["/posts/778231993.html","2842077eb080df7b5fc6b6ef45edd9a1"],["/posts/795397410.html","2e996f6966bd5a16addb1ac0a97e49a2"],["/posts/820223701.html","83b03071a8689880a815139febfab81a"],["/posts/830372185.html","7d1366846e0cba74b4d57008dde0e690"],["/posts/88294277.html","85b22cf3a51289f308286fcf7d69a985"],["/posts/939963535.html","dff9b40207d76acf50e0228b0e95c6fc"],["/posts/983786067.html","e535531475321bca6aff224d2f908db7"],["/sw-register.js","a526448218a717b7f2e3d847684c9279"],["/tags/C/index.html","9ecdc9ceb7475f6bcd36cc32dc89a15f"],["/tags/C/page/2/index.html","5506a3d3d6114162e46a2b145148d3af"],["/tags/C/page/3/index.html","f7b1426957a142286dbf1906e9ff0853"],["/tags/ETL/index.html","8109f6b69b55fb9ce0db5a44ab1a2db7"],["/tags/ElasticSearch/index.html","9dc5efefb3ee7c23f22dfc5426207735"],["/tags/GUI/index.html","57728680fb78870ffb16aaaf3209cc1b"],["/tags/HBase/index.html","9d16b1627ee13f796ca47e2572107e9d"],["/tags/Hadoop/index.html","2044b3d49f4734e390340278f7534c56"],["/tags/Hadoop/page/2/index.html","c42f6257357ea179a3cfb932691da12d"],["/tags/Java/index.html","bd2dfef6cedf81d35164132862b5b9c0"],["/tags/Java后端/index.html","23a7424e398c928988da4d4b8c9972c0"],["/tags/Java后端/page/2/index.html","b5dddf88cc875e5767dfc88b40f6656e"],["/tags/Java基础/index.html","13c8d89c0265bd3460c79efe36f3a903"],["/tags/Java基础/page/2/index.html","4ef9a19f034ebede6064dc3aa8730401"],["/tags/Kettle/index.html","11dc226ed120be775e69dacd96b27802"],["/tags/Kibana/index.html","7c74816cec01ac43d0aad442599c3c73"],["/tags/Linux/index.html","6a91950dceb455960c5fabd49fa611b2"],["/tags/Linux/page/2/index.html","50a25dd59558a150b3b75ce5b60c2ebb"],["/tags/Linux/page/3/index.html","84efe548644bcf005b60cd6ce1937320"],["/tags/Mac/index.html","b869b933712b98922b26043daa81ef3e"],["/tags/Mac/page/2/index.html","9c79afa528035398cbbe44f2059b72c7"],["/tags/Maven/index.html","baaa0d1f271d4d9d96c2d5ff3efab2a7"],["/tags/MySQL/index.html","fde54353c3549971fa25fe5080971fea"],["/tags/Python/index.html","6c3b8c6cbf6bfb0583d354e4afcf3e95"],["/tags/Redis/index.html","3b8b173becae19d22b2418ebe60ae35c"],["/tags/R语言/index.html","02a1d3cb3ef451e27bb4797fa0268e9e"],["/tags/Spark/index.html","c0061e2a5411efdb28e778ff7cdf32ef"],["/tags/Ubuntu/index.html","5ca8bf1b4a0d43ca455d38578f3420ec"],["/tags/Vue/index.html","4df411aedd71de2d15abd5ce22347292"],["/tags/Windows/index.html","62b3e03a85974b7d5a4393cded5c6187"],["/tags/ZooKeeper/index.html","4b0d58b2febbe5783065872a39060e28"],["/tags/bfs/index.html","a7ded39f410ad15a808b62edd24e1857"],["/tags/dfs/index.html","d46f8f4d4591461f43ae6dedc101ecf6"],["/tags/folium/index.html","a62e95a908da6b95628bc6d2fc1a6ef8"],["/tags/git/index.html","13a331b5d32d00765c4c7307a9565b95"],["/tags/index.html","d5c66bb19cf1b39a9aad8c21400933d0"],["/tags/latex/index.html","b11a34c6149fd40785a924288e57c78b"],["/tags/中间件/index.html","8aee31170beb1670cd27015351b65fa1"],["/tags/二分查找/index.html","2b9e8bd9d2117e6faef0ad79c45cd107"],["/tags/优化类/index.html","a2f4b5edb7c83bdce9dba4145537b660"],["/tags/前端/index.html","e43bb996ae4a5b1dc07a03d18fea2a4f"],["/tags/前缀和与差分/index.html","ddeb03a9b3ad5e8dd25906f7231d3a12"],["/tags/动态规划/index.html","49cb3d2b8dd6ccb3b0c3cec011ce85fd"],["/tags/动态规划/page/2/index.html","460f2732275b047024a27533288a051d"],["/tags/博客搭建/index.html","8a0b8d411bc63fde9d0265ea5bbe6c06"],["/tags/图论/index.html","d6dbb2930b52c40f17d8a9b964f93743"],["/tags/大数据/index.html","85b003d0e015c810abd68b17f2ecbf62"],["/tags/大数据/page/2/index.html","9dbb951eb89a3c41d45dc3ec77e681dd"],["/tags/操作系统/index.html","3aa4cd51df25995c9c4916795c699881"],["/tags/数学建模/index.html","b5bd6683dcd2d5f41f8a540e0d16d8f3"],["/tags/数据库/index.html","3c4ecb1f3a8191a791d7b6cfb59a8707"],["/tags/数据结构和算法/index.html","b615ec7159ff0e37651e4bea2761bc2a"],["/tags/数据结构和算法/page/2/index.html","2ac4747b051d519d3923032746453747"],["/tags/数据结构和算法/page/3/index.html","e7f19cb7a11b40d808a08146b56431f9"],["/tags/数组和字符串/index.html","3ba94f1e0b301360dd729c0605b5d78d"],["/tags/枚举类/index.html","ad08912867622c1cff073a8b18c5aee3"],["/tags/栈和队列/index.html","923ecaadbbcb1da0f5d8c85287f27ce1"],["/tags/树论/index.html","5d342e7d4561f721aba73dc57f9182bb"],["/tags/测试/index.html","340761f12164b0e7611e8f52cbbd5959"],["/tags/环境/index.html","8a7afc7c1f466d35f08a15b81c916a26"],["/tags/环境变量/index.html","31dbcd304687ea658931156611e26c4d"],["/tags/绘图/index.html","f248c214d2b1e1c91a5d1d87394032f3"],["/tags/编程工具/index.html","c32bc8321d415bcc8832f2cf46b5ae7e"],["/tags/编程环境/index.html","be1f32a0eff97854229115702efa2027"],["/tags/网络编程/index.html","bbd571a5bd4b7ce207077770c5b0b52a"],["/tags/英语语法/index.html","7f36198c2c0a21943164d828ca2d9a27"],["/tags/论文/index.html","a31681f8ad3ac102446a4e8567cd5627"],["/tags/资源下载/index.html","aa12e067b70e225de64337d0af787c32"],["/tags/链表/index.html","944ddbda0d746432561f676ad07e54e3"],["/tags/集合/index.html","1d99727ce7370dc25b5e47437b4c69a0"],["/tags/集群/index.html","9bce72019c6818b811b857d5eaf7b5ca"]];
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
