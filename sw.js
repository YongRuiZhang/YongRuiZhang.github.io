/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","eec676179dc6b1f88c67a2b682484d8a"],["/about/index.html","8dfd955db854a05214fa7f6674588ffc"],["/archives/2023/01/index.html","230e987ef1b7e18673367b8fb941919f"],["/archives/2023/02/index.html","f9e6ed106bd7814c84c0ce9b1616ed26"],["/archives/2023/02/page/2/index.html","23a56d539dafda54d53908447d4f2d04"],["/archives/2023/03/index.html","9d4e4a5fb8cb4ccf33e09daa1b7a5c65"],["/archives/2023/05/index.html","0e4ecf4cabecf5dbae66bc3a39748654"],["/archives/2023/06/index.html","28e7e1db40a0dfee2ac7db63e290b467"],["/archives/2023/09/index.html","f1561784d37a7a3eff19db9e1b7febc9"],["/archives/2023/11/index.html","1f074ceed1f3c9ea30af97c66576ce61"],["/archives/2023/12/index.html","559f11c073bf76cc536595120cc5da0f"],["/archives/2023/index.html","f9841a079e4bd4de9ef61b8d9b6b52f2"],["/archives/2023/page/2/index.html","1919c5780fb913d871ba3f340176930f"],["/archives/2023/page/3/index.html","508fbc75de1b2877958bf2f90124838c"],["/archives/2023/page/4/index.html","67619466af06fde7f1c30c46f6165cc4"],["/archives/2024/02/index.html","9ea535b2c24d59321b75cde29a92242f"],["/archives/2024/index.html","0ed5c8d8ab6e988b2968dd91c01d1f56"],["/archives/index.html","383acce9556fceb46cda47ff2b8e55f7"],["/archives/page/2/index.html","c2be2813634c9a256dda1519e9c06fa0"],["/archives/page/3/index.html","5106d06f62938ecd25231b46417b4e79"],["/archives/page/4/index.html","938630ed5585cfae4a3d5e9664e42e27"],["/baidu_verify_codeva-qQP2iZOMLX.html","66196b2123a1a610907e88cd1880a6c6"],["/categories/Java/index.html","f55f75df0d9159923f2a91d136c3f4bf"],["/categories/Java/后端/index.html","bb3d19f3fa8380e59de03cb09d2cadd3"],["/categories/Java/基础/index.html","8dbacd518511aa3124bd91467b2138ff"],["/categories/Java/基础/集合/index.html","736ed6668e29ea0fc858deae268885c3"],["/categories/Python/index.html","7be853be4e31d0f206787b99177c0c05"],["/categories/Python/编程环境/index.html","836717193cd1ffef98ba2212cc19209c"],["/categories/R语言/index.html","08397accd95ef2f119d509fd2118fb42"],["/categories/R语言/编程环境/index.html","918c3b35c03dc9636b6ce325232c8a27"],["/categories/index.html","a119e808c11c06610e0962c42e8a8e65"],["/categories/中间件/index.html","aa4252c72e472efe5aee0b76c0a2eb49"],["/categories/前端/Vue/index.html","9f0940d9def97f1d5f9ee0e2945e5023"],["/categories/前端/index.html","00b8fa9dd09ec2f29a1f36ad23608f46"],["/categories/大数据开发/ElasticSearch/index.html","a81250782775fb0cb23fe8d063cf39e0"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","f4cc7ce271b41951e7fa36e28517082d"],["/categories/大数据开发/HBase/index.html","f8b53b9e58cae99e49847a1bf3d8df46"],["/categories/大数据开发/HBase/学习笔记/index.html","88e8583a6d2b55c1cf5c1f0c385f8d33"],["/categories/大数据开发/HBase/环境搭建/index.html","d9382ccae9e6fecf4e1835231507a117"],["/categories/大数据开发/Hadoop/index.html","a2e44863619f75bd483d93d05f47069f"],["/categories/大数据开发/Hadoop/技术/index.html","05170363d7cf1432f828290d457724a1"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f2d9b8d044454925eeb1579b2e6c6dcb"],["/categories/大数据开发/Redis/index.html","94b3a866f84807c541631f0971177807"],["/categories/大数据开发/Redis/技术/index.html","8d8b81d475359a118f73f0ed4c9e137c"],["/categories/大数据开发/Redis/环境搭建/index.html","70c64bbc3cb1db7c1573feea4f7d0a4a"],["/categories/大数据开发/Spark/index.html","cab1ae98c0e8ba5066a5aee386e0161b"],["/categories/大数据开发/Spark/环境搭建/index.html","b72ea8ef6d76ffcadbdc981ef53af887"],["/categories/大数据开发/Zookeeper/index.html","2941a6850721a229e28ae6cb28af1114"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","312fb26df663142d1da339af2a1f7e8f"],["/categories/大数据开发/index.html","dcfacbf680bb509b0bbe50f40d4399d4"],["/categories/学校课程/index.html","3b9e891a1a55b09fb2334a09f3a90767"],["/categories/学校课程/计算机操作系统/index.html","b6150f3dd05f5eb0b2a0316ad345df87"],["/categories/操作系统/Linux/index.html","0a715d50d5ccf58f0292668b07a69c46"],["/categories/操作系统/Mac/index.html","b53301dbf7f3520e21cd243a1877f46d"],["/categories/操作系统/Windows/index.html","3905d85bcf384808642c297ed7a425ba"],["/categories/操作系统/index.html","fafb1a3dd8016f34de1dc629ef2d04c4"],["/categories/数学建模/index.html","d981a6dee7e941a5bab1bcb1cadcc3f2"],["/categories/数学建模/latex/index.html","443286db5aa222ef88b6d83ff5397fd5"],["/categories/数学建模/优化类/index.html","1320016e667d16705e912cffc7579f18"],["/categories/数学建模/优化类/现代优化算法/index.html","5af6e3aeb024db62a622d5154ee69f20"],["/categories/数学建模/优化类/规划类/index.html","920e8148b58afbe14c00d6efb9645a67"],["/categories/数学建模/绘图/index.html","aa540fec0971c60b2328755b2ea52ed8"],["/categories/数据库/MySQL/index.html","999d1b5027cd5cfac21f794d49934fd7"],["/categories/数据库/index.html","968e0e876956a691ac89ff692b020484"],["/categories/数据结构和算法/index.html","e27ebe42477d73060b50e15b9d5aeb74"],["/categories/数据结构和算法/page/2/index.html","a7ee12bb1d31f044fb540567b02414e2"],["/categories/数据结构和算法/基本原理/bfs/index.html","51503422dd4316ee05dbc2e668b57c76"],["/categories/数据结构和算法/基本原理/dfs/index.html","84c2830ab4c888476ed6faa83bef1286"],["/categories/数据结构和算法/基本原理/index.html","9342921eae05d4393fc973ce321a961d"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c5c1950e943fa1a4f6ac9d7394b2c845"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","9023a2a7c008a61e8055d421b72a4a0b"],["/categories/数据结构和算法/基本原理/图论/index.html","7607c6f1049bb7d80af11decda4e269b"],["/categories/数据结构和算法/基本原理/字符串/index.html","508f3562bbe2d5eae4478578776b1358"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fd8bece8d981e6b17603e08690ca8004"],["/categories/数据结构和算法/基本原理/数论/index.html","c41275246953e6f3fefed14f243e0f3b"],["/categories/数据结构和算法/基本原理/树论/index.html","5690d7bb8752efc93d8174322c86f9b2"],["/categories/数据结构和算法/基本原理/链表/index.html","1713eb87c5679719f0e1752c84a183f5"],["/categories/数据结构和算法/算法题/index.html","e3953592ea99ee56cf68a222cd7edcff"],["/categories/数据结构和算法/算法题/二分查找/index.html","3d54ee98e5aa3d6aef17e08eb9803106"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","01fb0c6a4b8e0f7629f997a90ee92054"],["/categories/数据结构和算法/算法题/动态规划/index.html","8027098e6050315ec93c4b54896d1c56"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","77dee1e72177231cc585d6f0797d4b2d"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","e06d821d9e8169aab6fbf4d01259e692"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","f5b621e2415377c0a22a26f79145df9a"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ff28cd7a5b3bd886a39f8dcc56bdc94c"],["/categories/数据结构和算法/算法题/数论/index.html","623c9a37e27d7b30e4fde15c6f7251f1"],["/categories/数据结构和算法/算法题/栈和队列/index.html","fb333494cd666144db1810ca5d7ef5b4"],["/categories/数据结构和算法/算法题/树论/index.html","578f5eb67990fbe2f02885e831adbd9d"],["/categories/杂七杂八/index.html","a318b3adf8f385aa9285389489930d77"],["/categories/杂七杂八/博客搭建/index.html","4bd90593c9e2986c295dda775adbfe50"],["/categories/编程工具下载/index.html","b1e92e4d3a4f12506da024ca265e5779"],["/categories/编程环境/index.html","b78b6599431774176c6ffbcb4abbca72"],["/categories/编程环境/大数据/index.html","4db27ccf3db06c5190af562003945bb1"],["/categories/英语学习/index.html","9e41edbd0bd4bc4161101c39b71dd9c7"],["/categories/英语学习/英语语法/index.html","489ca4769d4c32f993571020984fa1bb"],["/comments/index.html","ed04c48db5b1693eb864599b5b4fb2a2"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","9874f48a57ee4c475181de65d5131e54"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","90436b673d4da4ffd94da3cd8b6bb3e6"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","5b443117eab62ccd4d7c77f5d2e89231"],["/movies/index.html","b2b72310acff878a55c2d01b26618a05"],["/music/index.html","ba1545abb26e1f8a7bd6a1f196f15c89"],["/page/2/index.html","e11ffcbb0ca8fc332a026647aa8f2e02"],["/page/3/index.html","f9b02cea598dd7a569a541e3481dae75"],["/page/4/index.html","36221f52f850a074579f9ab8a0bd01de"],["/page/5/index.html","f616259050e0e41a14430c6aa53e21be"],["/page/6/index.html","1e36e976d30feae352a15fc338dc043d"],["/posts/1021360842.html","2bbb303e030c800c9128ae28194d65f6"],["/posts/1120620192.html","3cfd342539b1da35c9e043f9a00850ba"],["/posts/1141628095.html","fd8ba2d78ccff2e9207609dd92d51ffe"],["/posts/1168613674.html","356364eea86dc5381902825c66652423"],["/posts/1219920510.html","4d4b9f8b7542a530df18c8ff7654c291"],["/posts/1222166338.html","f8510ac2d239fdd3d28ba5c348fd7f75"],["/posts/1259097482.html","d8e0d8a0d43dc992d71819023e166542"],["/posts/1271036369.html","965ab06aa2dd2681692f1732b10e2cc6"],["/posts/1312847445.html","9068563c6205c7fc3cfbf2738cd39654"],["/posts/135355774.html","9fad93edaa9a1a61934d343f3539f0bb"],["/posts/1375344716.html","9b5e5fe76a48da07dc751354d6946b66"],["/posts/1388991698.html","f95863554946d9e8bee90a8e0cd9ac93"],["/posts/1410315814.html","50c0f3c193b623a6602a58bd9bc5e03a"],["/posts/1452790229.html","643f14bd5bbcb466aa0b1632e25f6cbe"],["/posts/1470079884.html","c79984ca1a19816a24caf5dc11d986b3"],["/posts/1470079885.html","4758970bf08099793d9a888c4be14121"],["/posts/1470079886.html","87e6671e670a9935c01c2a4954eb7269"],["/posts/1470079887.html","84e44323cb391a55a3a8496c1c9c7aca"],["/posts/1498536549.html","54cf8270e34860f5ef284bd0074ddd97"],["/posts/1539568593.html","e263599bbcb1247b0455b06917ee452c"],["/posts/1547067935.html","a1c5a7af9bb42cb77719c954bf63e0fd"],["/posts/1557866301.html","92369d04de935c5685d557ce155a5e97"],["/posts/1571776361.html","6c4a62dbac68692132947be7542fa93e"],["/posts/1605124548.html","e92b6bdbd5270de3d8623d136964b0e0"],["/posts/1633036852.html","17569e01b5d976ab94b28828d8041342"],["/posts/1674202625.html","57bf251f4b3ea70907a378ebca0e1f5a"],["/posts/1765123828.html","c5e71d7ee63c0b7bf6adacf8e3c3599e"],["/posts/1767336200.html","07a4f138c7fcb4efb72ac5441444701c"],["/posts/1776114197.html","483766321735519adb0fdfcd92b99c20"],["/posts/1817748743.html","c75d2842131dc696ebd7c00867a59da6"],["/posts/1925125395.html","75b62ccfd59e937f588d18a1bd2f128c"],["/posts/1966191251.html","37c1bb139429f96f16ff65d3e24a6183"],["/posts/1987617322.html","0c84bc7a2e21a2e14dbeda18f8dacd2e"],["/posts/1999788039.html","3785748d76e21764d6492d0059dccaa2"],["/posts/2075104059.html","4343c06d3f9a655cf184952d4016d206"],["/posts/2087796737.html","f67d3026e826c3b15a5cf515ab08b38e"],["/posts/2106547339.html","f3005e57df2592d81cc837c033394297"],["/posts/2207806286.html","4f4de1b3d694110c69d872785ae2a7e1"],["/posts/2225903441.html","79e35188c01063de1da73eeb75d26fcb"],["/posts/2265610284.html","364fefbfcc04e4df08b1815c7718a8b5"],["/posts/2281352001.html","071cb496bcf6815ea7c3667650f072ad"],["/posts/2364755265.html","079f5ec28ab4dcd4d96eec7f09dc5a96"],["/posts/2414116852.html","e24aba768866dc528134461ff1774511"],["/posts/2421785022.html","b0c70cb4910a74dbb587e2fee489db07"],["/posts/2482902029.html","8c4fbc4e16bc93fbb258c87abde6e580"],["/posts/2495386210.html","a0edae57dbbd61e1cf55109365abf129"],["/posts/2516528882.html","c8bd0d22bfc0e6078454bf96f7fb4388"],["/posts/2526659543.html","95aef8d5e19f66c14a00a65355517d4f"],["/posts/2529807823.html","151cf8283931705fc182812df85f63bb"],["/posts/2596601004.html","9234e8e0cba4f41ff6c01c6b988d33a0"],["/posts/2697614349.html","95da91f22adbf5221304ec8084557905"],["/posts/2742438348.html","1bb50f7edd7991c8d5273f989ab45528"],["/posts/2768249503.html","72373a5418be297b47d463920a80557e"],["/posts/2864584994.html","da9451e591072056cb00ba41712ad089"],["/posts/2888309600.html","cc03607fd45146fa92683386a22c4014"],["/posts/2891591958.html","d10c7f381cbcea80e903c1684a1fc544"],["/posts/2909934084.html","0b7f0c0a540195537179d2c55635715b"],["/posts/2920256992.html","d6f547631b19d24f3ffa99d0623eca1d"],["/posts/2959474469.html","88f472ddb59b8b640107b4c07405882f"],["/posts/3005926051.html","94004ac9cd0949f442f2c2a66f036e76"],["/posts/309775400.html","1329a3a2690457adc2026874a817f3ef"],["/posts/3156194925.html","2051d7f5352e2b0cdcaf86011839d963"],["/posts/3169224211.html","7b6664a03aea1a8a194534ab4e3dcc0e"],["/posts/3213899550.html","a0d5ce53fa0b426cb329a38052b2b0f0"],["/posts/3259212833.html","489a742694abc420e0960a1ea78f9c3b"],["/posts/3266130344.html","c35a1d9daad14b9484f5e5af4d3dcda0"],["/posts/3292663995.html","30ceb090fecf63f95e524124bbd26427"],["/posts/3297135020.html","1733c2995ed217462c93e2828389f4cb"],["/posts/3306641566.html","50ff9365fb9df447610447464e443442"],["/posts/3312011324.html","b97704fb8a7fb90eb332e5c85b288660"],["/posts/336911618.html","9468ba51758f8a91c97e89ed99250988"],["/posts/3402121571.html","d5b557c10c5d9726e61a75ca6b599f7f"],["/posts/3405577485.html","831377fc5867fc90a94b3ff0144e93a7"],["/posts/3498516849.html","12b420f73100217d83eb3cc0d66d23e6"],["/posts/3513711414.html","7af088505990f1f086236c339de36771"],["/posts/3523095624.html","97ca0f3a86bae5f6c3285fbaed1615f2"],["/posts/3546711884.html","dcb8ed93c35b6b8b26cff572e0017ee9"],["/posts/3731385230.html","207f9e6cd1b06ebb9fa39c7034d88d79"],["/posts/3772089482.html","b3a7f2e02ac94355846f1424b02b53a2"],["/posts/386609427.html","09d9d66e16ce25eb3ccf1d186156561c"],["/posts/4044235327.html","d3707bec882b5bb6b2b28c5baef1127c"],["/posts/4115971639.html","03a78001c0fa8e62cf2d70a14e0e756a"],["/posts/4130790367.html","59a8761e45ef74ec618e708f3ab99aa6"],["/posts/4131986683.html","5b3e471f71f1283724c0792a51c2bf5c"],["/posts/4177218757.html","eb077be6b796551627956d3d43edf845"],["/posts/4192183953.html","49a23f35a0caaac6f27221bb5e5d7037"],["/posts/4261103898.html","4149d8cca1c42e84049c7fb40c0d69b7"],["/posts/469711973.html","41a1d6b971a8180ace5b1d62a485e13b"],["/posts/482495853.html","2ec53372868535000cbd521c58da3d5b"],["/posts/488247922.html","84fe74ba90e8d8af3983cad11fa448bd"],["/posts/517302816.html","478f46eb0c5e80d3a0fad4221757961b"],["/posts/570165348.html","fa473bb425f45861f7cd5f858c604740"],["/posts/595890772.html","280ee2f47747f81031e4109140b618bf"],["/posts/67485572.html","ce7525ba9e4d7195cb704155666db20a"],["/posts/694347442.html","da9e2623327e448d7b1dc5c202229b9e"],["/posts/707384687.html","55622405eac3132b954838b61b913563"],["/posts/71180092.html","1553d26b2960feeb99116edf71eeddd3"],["/posts/716459272.html","3ddc797ee10a171197276be9669b0628"],["/posts/765481613.html","2a8f2283a32e0219fa15253ab863063e"],["/posts/778231993.html","4529f600a9d6781d38565d61d7533942"],["/posts/795397410.html","c09a3269ce1f3a83e541edd95c15f5d9"],["/posts/820223701.html","97a43b8a4aec8d5a3bb62de36a661139"],["/posts/830372185.html","c669b9dfcc4befeb05ec4ea06ab0bca1"],["/posts/88294277.html","431417fea3cdb4ca6cd21ab86e98b678"],["/posts/939963535.html","4155ad9a7cf8beb6e7a6fbf23f869827"],["/posts/983786067.html","15f1aa09a5469490e9935b6f8314a46b"],["/sw-register.js","2df5b95daf51024aec43e2b5999877cb"],["/tags/C/index.html","0ddd7d820ddf43fdf435243e15b4d015"],["/tags/C/page/2/index.html","ed2bf7230322e09ae9cbfb1508ed3128"],["/tags/C/page/3/index.html","724dbd96f1f185cba614355e9cda2be8"],["/tags/C/page/4/index.html","d955bdb9aa6481cd2a83553cc250bdd4"],["/tags/ETL/index.html","dc543a4cc35c14f7958f4ed33d2dbbcf"],["/tags/ElasticSearch/index.html","dc4af629de12be2a13ce4706d7851f51"],["/tags/GUI/index.html","018250576ecda126b08f1697a74afae3"],["/tags/HBase/index.html","899a95c606a49a55f6cd431f8588b331"],["/tags/Hadoop/index.html","de2a4f7874e037043bb739649d6b7709"],["/tags/Hadoop/page/2/index.html","1f5bb5a6114942f2941d63029de47f34"],["/tags/Java/index.html","af52f2c558764b1b80cfe92562410172"],["/tags/Java后端/index.html","06508502c736d2f60753cc58956f1b53"],["/tags/Java后端/page/2/index.html","2519b92ac3dc29e4f9976efa847ab774"],["/tags/Java基础/index.html","42f934c31488c76e3d12228b276aaf4b"],["/tags/Java基础/page/2/index.html","b5c125d4e963f0a2b52619300272134f"],["/tags/Kettle/index.html","39601b4f61fd4f949390fa3aacc79770"],["/tags/Kibana/index.html","37c04da85e0df6158ef7f8279d377165"],["/tags/Linux/index.html","85a54e6821ad4b3f8a729cdc4c6355c6"],["/tags/Linux/page/2/index.html","3b1122711256cb16ac3dd4af7a358927"],["/tags/Linux/page/3/index.html","6da040d7f25a78ef8663fb7a3b852401"],["/tags/Mac/index.html","ba7f1acdfdda2fa6ea6315bec4a5893d"],["/tags/Mac/page/2/index.html","606635b87f512a0daedf2ac6bb93cb38"],["/tags/Maven/index.html","6b72d3f56da6118ccf77eb5fd4299b4a"],["/tags/MySQL/index.html","d7d94b00de94d11eaaf5183e46dcf1d7"],["/tags/Python/index.html","a3672f60496b4a307f1a92fdebfe8fd6"],["/tags/Redis/index.html","e47d89269be1f6fae88a9e6f27572bff"],["/tags/R语言/index.html","8c721db2ceffc1a5d53498498c4f3a15"],["/tags/Spark/index.html","1cdc85bce8dc09e7a7a5391c1179218c"],["/tags/Ubuntu/index.html","12c2d257aa2241502f4e3a6f9d198ece"],["/tags/Vue/index.html","2ec57371de5eae6664ac7446c8d1ed04"],["/tags/Windows/index.html","190a41977d13300aeebbb6be52e6ec81"],["/tags/ZooKeeper/index.html","c06402f3182f92b2b4df899abd923650"],["/tags/bfs/index.html","0095b1665dfc9bcb1aecde462df3bc8e"],["/tags/dfs/index.html","05f4ef8448bfbcea9163667e38225191"],["/tags/folium/index.html","d0cecfeffa605a228342da71a2bff52d"],["/tags/git/index.html","d77135ba420b1966989001ccde233dc1"],["/tags/index.html","45bd1fa7311589208acae7b075ecfa82"],["/tags/latex/index.html","1b00e0d476e8f467ac65744cd21cdbbd"],["/tags/中间件/index.html","7a13461f026a002d9ecf055eccdd111f"],["/tags/二分查找/index.html","59c2d754633c9bb8a522b3ba73f01a37"],["/tags/优化类/index.html","fc4c9eb75c4eefdcb5e6f4b6cb1700c6"],["/tags/前端/index.html","02b166c5da72192727712b6f24903544"],["/tags/前缀和与差分/index.html","5ebcd690e7351c6c045947f984be79b5"],["/tags/动态规划/index.html","5b2bd316c2e2570d8252f1a69ccd94a4"],["/tags/动态规划/page/2/index.html","b01416126dec471a9481ddf6ba6d0802"],["/tags/博客搭建/index.html","7635b2af99db4d1576ed16e5253dce13"],["/tags/图论/index.html","d11ef26a9e36315ecad6adab4e7e199d"],["/tags/大数据/index.html","a179ddf715d706bff55cade3fbd81715"],["/tags/大数据/page/2/index.html","3685654a370e348dd470ff725f75038e"],["/tags/操作系统/index.html","970f841d5e6e3f050870dc4af8fd85b6"],["/tags/数学建模/index.html","9b19702922bb3e37671b2487e1913162"],["/tags/数据库/index.html","d1ecd4817369e22c8b095abf3706f365"],["/tags/数据结构和算法/index.html","eedfeac97de77ded156fec71d85b6fa4"],["/tags/数据结构和算法/page/2/index.html","3abcb357c9c0f38bd93bb6c2ced92703"],["/tags/数据结构和算法/page/3/index.html","50b3de463f3fcac52f67edcff5eba258"],["/tags/数据结构和算法/page/4/index.html","45f3d4c5dc2e980ca6fb80487edfefd2"],["/tags/数组和字符串/index.html","51c7cbccdf7d1ca26c852a70e1ccf143"],["/tags/数论/index.html","306db47e0fac49774a3c72fdc927bdd3"],["/tags/枚举类/index.html","c0b2d1a2fa7ccc3cbe85f9b50cce8a61"],["/tags/栈和队列/index.html","ea189fd8b8fdc5dffaf4aaddbab16105"],["/tags/树论/index.html","9b4c87fc67b32b97b6de69e1417f9d3b"],["/tags/测试/index.html","b6ef9ac920d488b0229fd8ce86fefd3a"],["/tags/环境/index.html","4dbf17860c3d6b212c1060026c52ab29"],["/tags/环境变量/index.html","4dd51ad2f2d21f000ecc513dded6c0f9"],["/tags/绘图/index.html","a19da7eeb3c4577d352a18f8fd7b9711"],["/tags/编程工具/index.html","f581f15f54373cb2a31ced8465821b63"],["/tags/编程环境/index.html","0e912780c15add5a08dbb411626189ee"],["/tags/网络编程/index.html","1382a680dbf0cfe6254bac4dbdc91632"],["/tags/英语语法/index.html","af03f1583b453b36ec7c04a186d3bddb"],["/tags/计算机操作系统/index.html","cac8dc34da746976904c451127ef03ee"],["/tags/论文/index.html","127a036d8059091b5533a6820179db67"],["/tags/资源下载/index.html","d68ecc1edd40767431010b413f141f86"],["/tags/链表/index.html","fe8b1add467e88ad2af3730e671f769f"],["/tags/集合/index.html","8a40b5fa4cf4d016a638b923c02d8443"],["/tags/集群/index.html","a7815cf8df0521896f2308996ec6d42a"]];
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
