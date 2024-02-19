/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","dd40c246074896f387ac048957e07b4b"],["/about/index.html","26ed1e10055323e1ba20e33c1d278e4a"],["/archives/2023/01/index.html","63d03f08a6d0d852553b1e0072a1f3a7"],["/archives/2023/02/index.html","f0e568dd251ecbfa43889c074da75eb4"],["/archives/2023/02/page/2/index.html","8b148899504ec04245da0b077a9be259"],["/archives/2023/03/index.html","4e47ad893dd6f4c0dc79b3c8b3a8e5b0"],["/archives/2023/05/index.html","671269883327bb22370cb8d4ace0d5a1"],["/archives/2023/06/index.html","1a73dfbfbcfbb4c49102227b2f69e77b"],["/archives/2023/09/index.html","0abde2c54a2b3e2afa685e04400cf7bd"],["/archives/2023/11/index.html","cc1f9b2aee635af3047d372494516870"],["/archives/2023/12/index.html","98df46f16f25e95cc5407be0108a2994"],["/archives/2023/index.html","2e2c6928d7fd90b23dc2e1a78efc95a1"],["/archives/2023/page/2/index.html","83e8ec56f0df02c4947175239b144dc7"],["/archives/2023/page/3/index.html","930f807345a2e187089b8e50d0195ed8"],["/archives/2023/page/4/index.html","2cded2ae08eaf11d76eced8563b93326"],["/archives/2024/02/index.html","80d0e211e31f803d8b6e47329e885371"],["/archives/2024/index.html","edd5ac677dec7d515f67542aea84da72"],["/archives/index.html","71efba7c138e23bee18e78a972e2a8b3"],["/archives/page/2/index.html","7c4f9a8d4f2ebc1f3ab2b4fd19d79df8"],["/archives/page/3/index.html","d4a83e0ca80f0a71a0f0a10a48478309"],["/archives/page/4/index.html","a79800df1e22b46a3bb7f2dfbf58331b"],["/baidu_verify_codeva-qQP2iZOMLX.html","b7ddd837bcff0384aa161e042f4e7ac8"],["/categories/Java/index.html","69e265c846ab08162ced99e3c3d975bc"],["/categories/Java/后端/index.html","5c001f05eebab856f9d9280ee3266b74"],["/categories/Java/基础/index.html","e98d81cc70312d25f22cc42d12570919"],["/categories/Java/基础/集合/index.html","3314bfcd7389f23f5508f6fa4c6d9071"],["/categories/Python/index.html","3681a65f545d0cd10324f14da3c88fc1"],["/categories/Python/编程环境/index.html","b54f72a2c7128834ba1416e2eab025fe"],["/categories/R语言/index.html","d81699a2b74a01a4e447eafc6fe67bd2"],["/categories/R语言/编程环境/index.html","a2488790599fa06e39d71a62189db530"],["/categories/index.html","4f5e3e536a28b7a8e3212c00c2825d2f"],["/categories/中间件/index.html","a2af21033029f391202bca576dcdb685"],["/categories/前端/Vue/index.html","fd72573b895d150ed34e0ca1a86d561b"],["/categories/前端/index.html","0acbcc427be093da96f8e410b7b66110"],["/categories/大数据开发/ElasticSearch/index.html","deedb94052bb7f8053f6b2b80604a53d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","009fbf824d9712aa8d1a09568d1b65a1"],["/categories/大数据开发/HBase/index.html","c41bf9bfd0e755f813952831bfe55a75"],["/categories/大数据开发/HBase/学习笔记/index.html","5bb4d4bee523850f950a742c447bd212"],["/categories/大数据开发/HBase/环境搭建/index.html","87af13006e89dfdacb0b3df04c7729f4"],["/categories/大数据开发/Hadoop/index.html","83b09e19105c1ea898240ec7deb35c14"],["/categories/大数据开发/Hadoop/技术/index.html","f942367987cd066c80031d4442b777b0"],["/categories/大数据开发/Hadoop/环境搭建/index.html","431374c3e34efbedcb65b56e35a9c05a"],["/categories/大数据开发/Redis/index.html","0b97776fd7df0e0c05df021bf9aec7c5"],["/categories/大数据开发/Redis/技术/index.html","9964c3f94697ade8345ab4ae6aeb3724"],["/categories/大数据开发/Redis/环境搭建/index.html","d454852c2375c006ed6c49af947a8edc"],["/categories/大数据开发/Spark/index.html","169cdeb1a25e0ac86a289b6dad1b7b9f"],["/categories/大数据开发/Spark/环境搭建/index.html","345c3706645db259ea166c672d18471b"],["/categories/大数据开发/Zookeeper/index.html","8df7e29d7467a8cae4738248bec54663"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e72c0838573270e48a6a7b7d001fdb0d"],["/categories/大数据开发/index.html","7cf427ac019ac676831d9b6147170154"],["/categories/学校课程/index.html","35c3c488c215e27b43827dc356e86e95"],["/categories/学校课程/计算机操作系统/index.html","3a508396ef35231dd8fd207589648f6a"],["/categories/操作系统/Linux/index.html","3f2340cc725cc1fe9469e7482ade2b7e"],["/categories/操作系统/Mac/index.html","1648be313999b494871194343156d790"],["/categories/操作系统/Windows/index.html","17ab66ad9582c028d9a1803c2c0601f0"],["/categories/操作系统/index.html","60bca7ce81169a88f561b4525d74e026"],["/categories/数学建模/index.html","063b245deaf51584b2918fe2204bfdad"],["/categories/数学建模/latex/index.html","0e817aacc1a221f6d3b181742f06cefd"],["/categories/数学建模/优化类/index.html","67258304164aa1698949e7c4cf9ed9a9"],["/categories/数学建模/优化类/现代优化算法/index.html","6feb960576c04de16a56814b1afc89f8"],["/categories/数学建模/优化类/规划类/index.html","f689e537e0ef4c2a3c4ddd777a0761a6"],["/categories/数学建模/绘图/index.html","bd43bce1b65d95ad7ae07999c4aa71f3"],["/categories/数据库/MySQL/index.html","88dec6402fb7e45556a220df5b917697"],["/categories/数据库/index.html","eb7bb397357b745a9017316c56675bd8"],["/categories/数据结构和算法/index.html","2a5b7908202a6ddf7a8694db767e1f98"],["/categories/数据结构和算法/page/2/index.html","223ba71a12ebceeb21e31f4b2fe8c127"],["/categories/数据结构和算法/基本原理/bfs/index.html","4a38f029cff80c374ef17f01c6ae8dd1"],["/categories/数据结构和算法/基本原理/dfs/index.html","1b605daa7c902441b0432a3c66b9ef57"],["/categories/数据结构和算法/基本原理/index.html","bf1872157c697f09af501993544091b5"],["/categories/数据结构和算法/基本原理/动态规划/index.html","19ae9c521f45348a85ed4851fbb34626"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","7714deb35091282758a5a496a977962b"],["/categories/数据结构和算法/基本原理/图论/index.html","d057091111ba5fa4c00777c385642dea"],["/categories/数据结构和算法/基本原理/字符串/index.html","cc85c58c01f18e6a8263b215460015ee"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","51d7dd4ecea7c77c95e5c12a5b30110b"],["/categories/数据结构和算法/基本原理/数论/index.html","1661105d7626235672ff4fd05c377208"],["/categories/数据结构和算法/基本原理/树论/index.html","d916fcd7707ff193ba59a16d1edb7ed8"],["/categories/数据结构和算法/基本原理/链表/index.html","034d691d9c80b67e89e5c9860c30668b"],["/categories/数据结构和算法/算法题/index.html","f27733655abcf76237ca6d87541fd945"],["/categories/数据结构和算法/算法题/二分查找/index.html","edcaf2d82ec2c3e2408f2b4203496395"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0ca9f32f3675117317e66deec5c4655f"],["/categories/数据结构和算法/算法题/动态规划/index.html","a5522b10a7ebb1eff6681b881c636ee9"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","57df1c8873eec30eb30198af4ddbf54f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","725756ee8264e92aab871db7a04280b5"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e16bc8644e0408e765352386a3d03670"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","292fa8edaab4d675f5921086fec092c5"],["/categories/数据结构和算法/算法题/数论/index.html","6a6ffc7c1c65c2a799c8460b118fc574"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8a8cf1f54745310c79887a2d6d90e0bf"],["/categories/数据结构和算法/算法题/树论/index.html","7fb5ff2c31c159357305910d14b5d295"],["/categories/杂七杂八/index.html","ddf65d3ee8f7fa5ca1778942de0f6f0e"],["/categories/杂七杂八/博客搭建/index.html","070796e6960b605dd527673a03235573"],["/categories/编程工具下载/index.html","7a7a759bf3fd07a5874d24ef2c24e625"],["/categories/编程环境/index.html","e34fe92db18e14b0504cc7b3611c9e26"],["/categories/编程环境/大数据/index.html","f58ce9177b24692f8af74d044b8322f1"],["/categories/英语学习/index.html","3d15242045794d5bf65f5a308826805b"],["/categories/英语学习/英语语法/index.html","2317e846aba604418cb8c5a0ecc7aeda"],["/comments/index.html","7eea0e84b10add133a8b7d06801119b4"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9087a736202b9f0f57617071643ff42a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","23dd1f8231e756fa2fd8863b366957bf"],["/movies/index.html","a6cd42ae1dc0fbd673d904c98fa3050d"],["/music/index.html","6f91282316403855cc09f63395f4f7ee"],["/page/2/index.html","58a6ff084e8afb11b98f8fa21180c050"],["/page/3/index.html","46e9412e6802555e256509591967d4cc"],["/page/4/index.html","98ea11b7c625077173c53683e9b8d618"],["/page/5/index.html","00b369a2adeaa5075128f2befd096b80"],["/page/6/index.html","bc84d10a2c8213d62b52334767f7d5cd"],["/posts/1021360842.html","cce170b86dff89c369f9eb42eb6a14d5"],["/posts/1120620192.html","b00551dd665ef088e9bc1be26b818083"],["/posts/1141628095.html","2b5ba206977546f5ebeb222b2e781906"],["/posts/1168613674.html","94f02bbf9c7187ce18012d9a09c57c0c"],["/posts/1219920510.html","7b80d2c923b1664c592ff8cd253cb13a"],["/posts/1222166338.html","40a27cf6cd1197083ba5aafbb94089c9"],["/posts/1259097482.html","a0341afbb65f0e42c1897e7a1a2aaafc"],["/posts/1271036369.html","5383d5843ba51ef8a5d13360ef120742"],["/posts/1312847445.html","4d5071f5e6aae22a641c5d74579375c6"],["/posts/135355774.html","a0a213cc181f5c729ef5a0c3ab06a374"],["/posts/1375344716.html","87c357530caa6b5f9be5603bc8b4ede5"],["/posts/1388991698.html","f07b71817d07f9b0c154a73c0bfb53d7"],["/posts/1410315814.html","ffeaa15b3d5a9d4ed7ee9c28fa1fea56"],["/posts/1452790229.html","61b15ac9b92bf1d49016967d9155d86a"],["/posts/1470079884.html","b180ad763ebd0a59f512e5925a38f120"],["/posts/1470079885.html","dab65a567fe66ebbe348d942c4692738"],["/posts/1470079886.html","b21cd431d74f052dfe49a50e94c42fdb"],["/posts/1470079887.html","a39dedc9cdb04c8cc257538af0354f45"],["/posts/1498536549.html","df6a74ab81835666b21032c9f7d08a23"],["/posts/1547067935.html","06d186a9dfcced6ddb734736ecdd63aa"],["/posts/1557866301.html","0bca50a802c486de0dd19a97ba4d8735"],["/posts/1571776361.html","131c7a1d2b93e1e67430de7482baa759"],["/posts/1605124548.html","8d57939fbb0abb9312230d43d6f53b94"],["/posts/1633036852.html","3bdbf6a0703aa16b4e4be2943c317e7f"],["/posts/1674202625.html","d4f09ce89bddd56c7f5c76d009eff94b"],["/posts/1765123828.html","c7e0f5908aa64dbbdfd7689576718d6c"],["/posts/1767336200.html","28b0a8e4e00d31e02290f7acad69c8e3"],["/posts/1776114197.html","75ca54962f2ade905c3364e10d4fe4c5"],["/posts/1817748743.html","9596ed2c4bb534e7e78d5d90f42541f6"],["/posts/1925125395.html","a32f3a9d81ff5d14dc15868b0a73208f"],["/posts/1966191251.html","b181d2ba1bbcda89a333b6b2e6f63135"],["/posts/1987617322.html","17a2dcc05ae594ac0bbed411cdfb003e"],["/posts/1999788039.html","b1828cfdc026f6544adf9f4c6df7084f"],["/posts/2075104059.html","50c04f3cebda6d67eaa7b8dedf1a67fb"],["/posts/2087796737.html","139ec307feede257c38297e6badb4a57"],["/posts/2106547339.html","c9736e0eb143378a966d04d314077493"],["/posts/2207806286.html","27ba78ea83260c4bb427124178292fdb"],["/posts/2225903441.html","59837ec8be8e25ad218522ab1d1beead"],["/posts/2265610284.html","96d8935eaa383c2b80d64d73dde88cf3"],["/posts/2281352001.html","dcc6e0e290366ca1b8f239dbd89a7116"],["/posts/2364755265.html","0ad5a995a8e25f9e3dc8229b38626807"],["/posts/2414116852.html","3a171907ba0520e9b50568c8f77a95c8"],["/posts/2421785022.html","178d535aa156b255f473f5b03e70f6a8"],["/posts/2482902029.html","8a01461437c4d2e70cf47c48e77420bc"],["/posts/2495386210.html","953323fcdd420b2cc2131e7249419628"],["/posts/2516528882.html","0b32a9b89fc2e800885dd144e940304e"],["/posts/2526659543.html","f838e6b8bbdba89ce033f7a92e9e6480"],["/posts/2529807823.html","019769af4103eca3385b8ce81fe6cb33"],["/posts/2596601004.html","dcb43850b4d2588d21a68014be766da0"],["/posts/2742438348.html","4ccf89ca5a5444685254a5a23d0cc94f"],["/posts/2864584994.html","b07698d04808109fe863e1620bf93514"],["/posts/2888309600.html","a3565e7b5784b3f8a757923a6c4f1a0c"],["/posts/2891591958.html","60b5d1c0250bcbf112fb6cf250f3f80b"],["/posts/2909934084.html","bec590889e4a7c1be128fc8e64fbe943"],["/posts/2920256992.html","03842c8eebffdc3e7438d50ed5480d90"],["/posts/2959474469.html","96dd328e6088834b3881ff0733bd72b4"],["/posts/3005926051.html","7ece09f171eb6f8f60f620a6006a073d"],["/posts/309775400.html","3dfcfc1ca7338b80b0291c60aea017db"],["/posts/3156194925.html","579630c9343d8352db91644b66b3d6e7"],["/posts/3169224211.html","e75e7b685f1ac8bc86b9175c3c65de44"],["/posts/3213899550.html","5d1603839109027328949b6b7e16eef7"],["/posts/3259212833.html","b6ed046fcac44f07877fc11ec67a35ce"],["/posts/3266130344.html","1f5cd05aec3861bed62ce2612f2f0d57"],["/posts/3292663995.html","37dd0afcfe0a18213042cf8fb05ffda3"],["/posts/3297135020.html","84b1477805f11cfa2ded430f2aa1313a"],["/posts/3306641566.html","526666cdbad1f3d3bda7cf4ad0655888"],["/posts/3312011324.html","9d56bee511dd4d71a935a9dec0a394e9"],["/posts/336911618.html","b3e4a69831212412719f3c41ecfb5818"],["/posts/3402121571.html","ff9cab2d75d8a349576f2cfe392b49d0"],["/posts/3405577485.html","482af8f1392987026884f7a3bd7db5dc"],["/posts/3498516849.html","a21328f4cab42d6559429aff754738e1"],["/posts/3513711414.html","47220b5d0f93102d75030f9d992022ef"],["/posts/3523095624.html","faebdd76a2baa53096a6b7e1faf75c79"],["/posts/3546711884.html","4d8264c213de6826c16b4eaf2df782f3"],["/posts/3731385230.html","6cb17c819d016beccbf2333f4b5cac5d"],["/posts/3772089482.html","e8d5a87f07f736220e8f7d6b32dbfd72"],["/posts/386609427.html","270fde7322519fbde2624174d0f91c53"],["/posts/4044235327.html","7bcd1c96aa81da16ed6010c79eb797a3"],["/posts/4115971639.html","d92e7b64a200f4af1977dcb4b5e40341"],["/posts/4130790367.html","bd5a39b732c7635e53f837222fb053c6"],["/posts/4131986683.html","eb2a9f4f27b0075419a5d8c167f12692"],["/posts/4177218757.html","a291cc33f41f27f21c7611d92c682599"],["/posts/4192183953.html","2c5d6fd12bd1f02189adcab1422e6b50"],["/posts/4261103898.html","216995df3949e8ef095b85441b5e1e2b"],["/posts/469711973.html","03cba90574906a759e3a7eb22547cf57"],["/posts/482495853.html","9475e09aef82bb5ba29704f0aaa86f2e"],["/posts/488247922.html","eb4e11a51fadb8944dc18f5df0c725a6"],["/posts/517302816.html","9a052d0d1bd61b93def0abcd78148006"],["/posts/570165348.html","e33f3458e4337dc8dd78871bf33c4d63"],["/posts/595890772.html","222fc9e45ae3611c01fabfc3baf68646"],["/posts/67485572.html","d323f9347d1875b45aba4b0410e94ade"],["/posts/694347442.html","13423127fbe5c79d1d48ec523dd1d40d"],["/posts/707384687.html","bee1b4bd838d509b99f1b6ae09e08504"],["/posts/71180092.html","c8fad62cd2110c298254ab8e043c94a6"],["/posts/716459272.html","dabd664cb9b56c582b67c99152998555"],["/posts/765481613.html","35189008bd5c1db5e5e721e92778964f"],["/posts/778231993.html","576c92e594f2a2a994afc4da3f046805"],["/posts/795397410.html","8de08ea22998e48bfeb776fe4cf88912"],["/posts/820223701.html","561b1eb63f998761dea52af81e2ff85d"],["/posts/830372185.html","efe8fcf929ff692362aac6422886b20d"],["/posts/88294277.html","0b1e96f09cc0b45228a8a62870e612fb"],["/posts/939963535.html","de2971d7a8eca90dd32d83e392ce0562"],["/posts/983786067.html","794023c2d64f4e4900915e1da951fe7f"],["/sw-register.js","777d38135f2f5b9c4d6b8d3800873a44"],["/tags/C/index.html","dc065421f77582797e02eeba0973ea1e"],["/tags/C/page/2/index.html","5db8957349ea77b8b235b216ae4a7a09"],["/tags/C/page/3/index.html","3dbceacc5e74bb26776106222e091676"],["/tags/ETL/index.html","f506de5bd577255fdea93df9a0c7ebc4"],["/tags/ElasticSearch/index.html","5bf4b9c31c14939758f278208aeb5276"],["/tags/GUI/index.html","7fd6bdb0671718a1d0f537c9e902f6ea"],["/tags/HBase/index.html","684728b311420f4f2c5ca66472025c4e"],["/tags/Hadoop/index.html","95b136ad9495b9e5b3bc61ccf05211ac"],["/tags/Hadoop/page/2/index.html","311b0cdd1007c4d31d92c22256c8abe1"],["/tags/Java/index.html","9508f767db082443dcec40f9779f814b"],["/tags/Java后端/index.html","1c826cc744eef66d4f2e7e8acdb1b837"],["/tags/Java后端/page/2/index.html","0080a38bbdbcfd779589275a7801c088"],["/tags/Java基础/index.html","06dc8a525fa6774a759f2916d948db10"],["/tags/Java基础/page/2/index.html","967eed7293c01b8b32fc528e5e510763"],["/tags/Kettle/index.html","10c810a95f78a8803771e439eee8f796"],["/tags/Kibana/index.html","305af28248d8c97c69a03147c92dda10"],["/tags/Linux/index.html","6f50211939aa5d88d1534f3d5efdf108"],["/tags/Linux/page/2/index.html","a96b22bfce4dcea44ac6d0aa59a0936f"],["/tags/Linux/page/3/index.html","cc307cc39c169b416f7ab2b911cb767c"],["/tags/Mac/index.html","563344986f6c6298ef167f14b66e8d0e"],["/tags/Mac/page/2/index.html","7f98388625f21c39a3238f0f77a597f4"],["/tags/Maven/index.html","bbd41fcbc0191270e6cbcba4044afef8"],["/tags/MySQL/index.html","f67100cd12d0a431e9a7f445f5cd4a09"],["/tags/Python/index.html","9e0eede3aee7feb095b48b5cfce1c1bc"],["/tags/Redis/index.html","6418eb2869b81b740fe9086d4e79ec8e"],["/tags/R语言/index.html","93421687c49fe9e84ef9f716d69f8cf7"],["/tags/Spark/index.html","21b1b50a5a0538b6d5de8f551738ba7c"],["/tags/Ubuntu/index.html","357156eb4ec3344e1a9a6e62f5f72e25"],["/tags/Vue/index.html","442ff59dce78ca1be093814e77fbaa96"],["/tags/Windows/index.html","68575bbe59879ad76c8858e934357450"],["/tags/ZooKeeper/index.html","51ea20e1fa12894c782f2f8d4e2a9f3f"],["/tags/bfs/index.html","28e4fb8556e43cedb35e38fc0775e3a4"],["/tags/dfs/index.html","b20f3b83aec136214fced2bb1c5f0b2f"],["/tags/folium/index.html","510b243680c307b11ebd87e9b2e12ba2"],["/tags/git/index.html","cc6f4cd7ef6cda81ac141ba2bc0389fd"],["/tags/index.html","7c1f94ab5ecee7a0e133e59e872faab2"],["/tags/latex/index.html","c6543efcb0a5149d14596a28e57d12a0"],["/tags/中间件/index.html","96981acf4ecd5d07a94508eec79a7e28"],["/tags/二分查找/index.html","3dbd55e2fbfc10164c938bdc76daaa00"],["/tags/优化类/index.html","3214ed55c214c2ce1a0807e7aa637846"],["/tags/前端/index.html","a83efeeca37519f14a33d0281a6b2b67"],["/tags/前缀和与差分/index.html","cb2ec838791c578103bdb44e661942a8"],["/tags/动态规划/index.html","21fd69374f2d0f657eeb85c11e94e2cf"],["/tags/动态规划/page/2/index.html","41861f423fcca55dac44b3945df7c1a4"],["/tags/博客搭建/index.html","47b6d3d9a3a403ca9fc9bf790b703e0f"],["/tags/图论/index.html","19518b8d897448d7964c9757e9f7da0c"],["/tags/大数据/index.html","7dd9e5eadaf1faba23899238a0588359"],["/tags/大数据/page/2/index.html","7876a638e7aa11a2ffc85f91bebe2ba0"],["/tags/操作系统/index.html","180071f743099545171ce2d4989277b2"],["/tags/数学建模/index.html","75a818995ee6de43d6df11f22d07e1d9"],["/tags/数据库/index.html","724ceaf4720c9d0e5f17f1fa4e8b0bad"],["/tags/数据结构和算法/index.html","6efc738f1f073156ed83a9c54182087b"],["/tags/数据结构和算法/page/2/index.html","0a86e9923b174117eec5f22a6e63854d"],["/tags/数据结构和算法/page/3/index.html","af80e96e71f65f14aa6d603fb33fc3b0"],["/tags/数据结构和算法/page/4/index.html","f531009e09e16a06e316e4afdf906f10"],["/tags/数组和字符串/index.html","fa438b02bec972bc10ba01a29f165880"],["/tags/数论/index.html","72cc5f801241c710c67806d5526c7a76"],["/tags/枚举类/index.html","af5c24b24ee3aab4e5b2977a5a663e7f"],["/tags/栈和队列/index.html","d71d7f4829469f6328deecccbdaf9ddc"],["/tags/树论/index.html","7d6549a33bddb1bc1a4c9626173d8320"],["/tags/测试/index.html","fad2fe0811cbb3301e585516bbc00da5"],["/tags/环境/index.html","bc23d54f86fbf5dfe9e692a3aeab41fb"],["/tags/环境变量/index.html","68ae48848988415b06137683389c2404"],["/tags/绘图/index.html","d54eaf93c9c08683570c9a8ffba7245e"],["/tags/编程工具/index.html","ae8902118586cf692ff8e21abbc17972"],["/tags/编程环境/index.html","08eedc30687876fbc51146f8b20bfbbe"],["/tags/网络编程/index.html","4225a81afa5f8fc9ae2d7c954b468259"],["/tags/英语语法/index.html","e32c5376c288a3fa952e897ddfc21844"],["/tags/计算机操作系统/index.html","6002cdfbfb9debea20e67037e1090435"],["/tags/论文/index.html","f1fd3fb2e914b398fac2318b775ef01b"],["/tags/资源下载/index.html","35a8bb04a1b102e5f0e3169950714be1"],["/tags/链表/index.html","bb38483bbd1c5bb40b61f299e75da413"],["/tags/集合/index.html","8c5e496afefa099f48658ed389366f9a"],["/tags/集群/index.html","0a21bc2e69c7a1c4eb74af7b2ee1c011"]];
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
