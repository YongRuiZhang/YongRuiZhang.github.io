/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","44ef317f7e9286bc8527355e55bdb792"],["/about/index.html","26ed1e10055323e1ba20e33c1d278e4a"],["/archives/2023/01/index.html","bc657a78642c76f9bd45040de6a96d4e"],["/archives/2023/02/index.html","e5b9b81995c606098d55c8be30088f1b"],["/archives/2023/02/page/2/index.html","26f28a4ab71c5dfa5c6193dc9ffe16b7"],["/archives/2023/03/index.html","f8db1f7e399d3a323eef4285eea27ad7"],["/archives/2023/05/index.html","04a147c38529f59f161416c088fce62a"],["/archives/2023/06/index.html","60444752fcafe751d61c5d8509d917c7"],["/archives/2023/09/index.html","efe4b8f1bd2d53426b1be9e18c14aeb1"],["/archives/2023/11/index.html","1952401803a38db00aa6494f3be6bce2"],["/archives/2023/12/index.html","bf61c2ac2d37eb9ecaa975aba3b407f8"],["/archives/2023/index.html","878b37da4bd94f9e593ca3324cd9996a"],["/archives/2023/page/2/index.html","f2e1b19c2d7cdbb47fb7ecdd5ee58b36"],["/archives/2023/page/3/index.html","25da74f4e3ecb9efce4ebf2babc269d6"],["/archives/2023/page/4/index.html","9302509bfd93e5c730416b21f7f0506a"],["/archives/2024/02/index.html","a5153c395e6043e921211e17c08385e8"],["/archives/2024/index.html","e46d382c7e620bc33cd85d876dc4ae01"],["/archives/index.html","418d5a53549fe4f84a6021d4cf352d85"],["/archives/page/2/index.html","0c37d349749a609c4e5224cba28c1401"],["/archives/page/3/index.html","5f2b2527e480c3ed179089a838db2189"],["/archives/page/4/index.html","6386c18ecbb398770965ee998929247c"],["/baidu_verify_codeva-qQP2iZOMLX.html","bd13ca25af12442930e0c473c7139a1f"],["/categories/Java/index.html","f97cde013d005e3afccd021061d55069"],["/categories/Java/后端/index.html","f87dd4a528538929fd1e9e8fc5781e83"],["/categories/Java/基础/index.html","cbf48d528a018477fff5be459c0b7256"],["/categories/Java/基础/集合/index.html","028b4368d8686e1309e24b9d67cd6da9"],["/categories/Python/index.html","c01954ae69419054da0e249130a372f3"],["/categories/Python/编程环境/index.html","d481e65cab6f945b681130c35a3eeea7"],["/categories/R语言/index.html","009323cf84b96a76fb5a5ef0b77de0c8"],["/categories/R语言/编程环境/index.html","3e6ef5be8a5f19713f186b5a116db3ac"],["/categories/index.html","4f5e3e536a28b7a8e3212c00c2825d2f"],["/categories/中间件/index.html","50993f1d9b31876c65f8e8ef658b96e2"],["/categories/前端/Vue/index.html","ee4b2a8747c707f2890700e8e4eebc49"],["/categories/前端/index.html","7313472050aeae10875d1cab65181ccc"],["/categories/大数据开发/ElasticSearch/index.html","0c17bdaf39c3f865170caad7fa8076df"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","37ccb0134f78397de890f5c63628c335"],["/categories/大数据开发/HBase/index.html","12dda1175603bd1301829d0b42d03ead"],["/categories/大数据开发/HBase/学习笔记/index.html","51ce6ac632e847a3ef0ccebdaea41dd7"],["/categories/大数据开发/HBase/环境搭建/index.html","d676ef1625afd724cdc2f9f91458a4bd"],["/categories/大数据开发/Hadoop/index.html","3a38cc95caee88b1d9ec5f81b1a530ad"],["/categories/大数据开发/Hadoop/技术/index.html","6978e0a427cc3476f5226b0c583540bb"],["/categories/大数据开发/Hadoop/环境搭建/index.html","2613d4e34d121503cb4f6873a1e23c32"],["/categories/大数据开发/Redis/index.html","9d8de4949d5cefab03a6bc4accb21198"],["/categories/大数据开发/Redis/技术/index.html","b573340c7ab76dc627287cdc273f1c79"],["/categories/大数据开发/Redis/环境搭建/index.html","2894c5087708ac0de0a1fde1aac03e50"],["/categories/大数据开发/Spark/index.html","66ecec63820645f41654e35a1bb2c087"],["/categories/大数据开发/Spark/环境搭建/index.html","b96f9d80e2fe978752d843fbaa61f0f5"],["/categories/大数据开发/Zookeeper/index.html","e780ccaff21f5000ffce82eaa0cbcc70"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e89ca166230a9637ea2fae7472831e30"],["/categories/大数据开发/index.html","a2fa28cb3a463b31fbf8e8cb006c8da9"],["/categories/学校课程/index.html","71f6e0264707c8f3c5f17597b515fc4c"],["/categories/学校课程/计算机操作系统/index.html","8f13c7a81c174294b560e1d4c9b830e1"],["/categories/操作系统/Linux/index.html","a9e6cc1cfe71f2f6705b11fa62bc9a44"],["/categories/操作系统/Mac/index.html","bbff2b37f947c3c90f8b729c7a817920"],["/categories/操作系统/Windows/index.html","02ebccf6178d23210b4958d82ba06c4b"],["/categories/操作系统/index.html","1c5166443e2ac782432abfba03885e17"],["/categories/数学建模/index.html","9c4be199a400c80449a45829f3a72ac4"],["/categories/数学建模/latex/index.html","de79303cbb00e8f5309924c03a038fa9"],["/categories/数学建模/优化类/index.html","7a7424a4d00977bd40ed9731ae249d20"],["/categories/数学建模/优化类/现代优化算法/index.html","cca2ec9a87ba33d67c236c78366d1e99"],["/categories/数学建模/优化类/规划类/index.html","9f9c7d14fd11c3e72cbc86d95447d577"],["/categories/数学建模/绘图/index.html","a620312f8533dc11f94ef0f7dc953654"],["/categories/数据库/MySQL/index.html","4cdc9147c304db4a32e50e725ef01b1d"],["/categories/数据库/index.html","31d2b6135b64618c2cd216eccf0c100d"],["/categories/数据结构和算法/index.html","66b8935816b0c0a26b39ffe5e7f1e24e"],["/categories/数据结构和算法/page/2/index.html","f7141318f591e56c37c92d80bd4018f1"],["/categories/数据结构和算法/基本原理/bfs/index.html","7e9d4af4319588f282593024a989eb27"],["/categories/数据结构和算法/基本原理/dfs/index.html","04e9d92673e175e588ae1fcd599bd2ae"],["/categories/数据结构和算法/基本原理/index.html","28cae62530313b8bc164dacf74e65aff"],["/categories/数据结构和算法/基本原理/动态规划/index.html","80a9fd646bdcb91dfe03b8bfda2637ec"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","59e3a88a542293f8dc6824ae57a97f4e"],["/categories/数据结构和算法/基本原理/图论/index.html","3939428494fa1fc8ea2235ddd7b0212b"],["/categories/数据结构和算法/基本原理/字符串/index.html","13696ed329ec8dca681c98529d1dbbab"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","f60c7eece1f864bfc60d307b896b4935"],["/categories/数据结构和算法/基本原理/数论/index.html","3eddbe6d348c8caef950a9646fc68d80"],["/categories/数据结构和算法/基本原理/树论/index.html","b1f723f20f42a8de42e3ce0a073211c6"],["/categories/数据结构和算法/基本原理/链表/index.html","4cf324626759bcdec0bcda7d54af76a8"],["/categories/数据结构和算法/算法题/index.html","8c0f77d50597d9561a735f94fcb193b8"],["/categories/数据结构和算法/算法题/二分查找/index.html","6eeaa060cedcb98d7380ecf00dfcf916"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","e5677b022a21eeebdec6c2763acead9e"],["/categories/数据结构和算法/算法题/动态规划/index.html","d4e854adf333974d75caa045701934fc"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","74e67c18ea825dfde8b120873563f07e"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","1a4c78cfb03bc2e9e6b832e06b28d593"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","f59eba9889708e274acfb458277dfdf2"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ba93eb64323d74ad5243448cd15d6a60"],["/categories/数据结构和算法/算法题/数论/index.html","beb3b6c408eb7734b3aa674656ca5505"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5cbb6477143f5adfccf97a2ea9f70351"],["/categories/数据结构和算法/算法题/树论/index.html","2a2b0682ebedc2ff6c1a90b861d2336a"],["/categories/杂七杂八/index.html","0fbff6fb84eece50734e9802a5f960a2"],["/categories/杂七杂八/博客搭建/index.html","6372bddd4d1b80d92ffbdd88ed0a32a5"],["/categories/编程工具下载/index.html","88d46e40da5e07c309307931ec224d7b"],["/categories/编程环境/index.html","aed0ccf0110e0e606715b878cfa04dab"],["/categories/编程环境/大数据/index.html","ef8b54bca5aff645a93fef55d785dee1"],["/categories/英语学习/index.html","d6629b15130a24d2c93c9d729ee2eda4"],["/categories/英语学习/英语语法/index.html","4e56e1e74fd1ff2b292e21cf95c9152d"],["/comments/index.html","e975bd7a051bf724a928939e910a3060"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","aef8bfee0f2a2354624f1f6d530af2f6"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","d3674197ca2792cf38fc03614fc1eb6f"],["/movies/index.html","e9f913a4ba7ddad70071314d379195d9"],["/music/index.html","0c5d6600b078c38bef28460ef91d2284"],["/page/2/index.html","14770f5b9d7e5ed26ec7c604a00d81f2"],["/page/3/index.html","c587eeac6d59b53a4db4d92f9cc79619"],["/page/4/index.html","bd50e2129436dfbec9d89fbc1a732096"],["/page/5/index.html","a228ad5fc6d3c40eae4e53190aa3e631"],["/page/6/index.html","54d3b4813aaa563b26a9801d4da46090"],["/posts/1021360842.html","cce170b86dff89c369f9eb42eb6a14d5"],["/posts/1120620192.html","b00551dd665ef088e9bc1be26b818083"],["/posts/1141628095.html","2b5ba206977546f5ebeb222b2e781906"],["/posts/1168613674.html","94f02bbf9c7187ce18012d9a09c57c0c"],["/posts/1219920510.html","7b80d2c923b1664c592ff8cd253cb13a"],["/posts/1222166338.html","40a27cf6cd1197083ba5aafbb94089c9"],["/posts/1259097482.html","a0341afbb65f0e42c1897e7a1a2aaafc"],["/posts/1271036369.html","5383d5843ba51ef8a5d13360ef120742"],["/posts/1312847445.html","4d5071f5e6aae22a641c5d74579375c6"],["/posts/135355774.html","a0a213cc181f5c729ef5a0c3ab06a374"],["/posts/1375344716.html","87c357530caa6b5f9be5603bc8b4ede5"],["/posts/1388991698.html","f07b71817d07f9b0c154a73c0bfb53d7"],["/posts/1410315814.html","ffeaa15b3d5a9d4ed7ee9c28fa1fea56"],["/posts/1452790229.html","61b15ac9b92bf1d49016967d9155d86a"],["/posts/1470079884.html","b180ad763ebd0a59f512e5925a38f120"],["/posts/1470079885.html","dab65a567fe66ebbe348d942c4692738"],["/posts/1470079886.html","b21cd431d74f052dfe49a50e94c42fdb"],["/posts/1470079887.html","a39dedc9cdb04c8cc257538af0354f45"],["/posts/1498536549.html","df6a74ab81835666b21032c9f7d08a23"],["/posts/1547067935.html","06d186a9dfcced6ddb734736ecdd63aa"],["/posts/1557866301.html","0bca50a802c486de0dd19a97ba4d8735"],["/posts/1571776361.html","131c7a1d2b93e1e67430de7482baa759"],["/posts/1605124548.html","8d57939fbb0abb9312230d43d6f53b94"],["/posts/1633036852.html","3bdbf6a0703aa16b4e4be2943c317e7f"],["/posts/1674202625.html","d4f09ce89bddd56c7f5c76d009eff94b"],["/posts/1765123828.html","c7e0f5908aa64dbbdfd7689576718d6c"],["/posts/1767336200.html","28b0a8e4e00d31e02290f7acad69c8e3"],["/posts/1776114197.html","75ca54962f2ade905c3364e10d4fe4c5"],["/posts/1817748743.html","9596ed2c4bb534e7e78d5d90f42541f6"],["/posts/1925125395.html","a32f3a9d81ff5d14dc15868b0a73208f"],["/posts/1966191251.html","b181d2ba1bbcda89a333b6b2e6f63135"],["/posts/1987617322.html","17a2dcc05ae594ac0bbed411cdfb003e"],["/posts/1999788039.html","b1828cfdc026f6544adf9f4c6df7084f"],["/posts/2075104059.html","50c04f3cebda6d67eaa7b8dedf1a67fb"],["/posts/2087796737.html","139ec307feede257c38297e6badb4a57"],["/posts/2106547339.html","c9736e0eb143378a966d04d314077493"],["/posts/2207806286.html","27ba78ea83260c4bb427124178292fdb"],["/posts/2225903441.html","59837ec8be8e25ad218522ab1d1beead"],["/posts/2265610284.html","96d8935eaa383c2b80d64d73dde88cf3"],["/posts/2281352001.html","dcc6e0e290366ca1b8f239dbd89a7116"],["/posts/2364755265.html","0ad5a995a8e25f9e3dc8229b38626807"],["/posts/2414116852.html","3a171907ba0520e9b50568c8f77a95c8"],["/posts/2421785022.html","178d535aa156b255f473f5b03e70f6a8"],["/posts/2482902029.html","8a01461437c4d2e70cf47c48e77420bc"],["/posts/2495386210.html","953323fcdd420b2cc2131e7249419628"],["/posts/2516528882.html","0b32a9b89fc2e800885dd144e940304e"],["/posts/2526659543.html","f838e6b8bbdba89ce033f7a92e9e6480"],["/posts/2529807823.html","019769af4103eca3385b8ce81fe6cb33"],["/posts/2596601004.html","dcb43850b4d2588d21a68014be766da0"],["/posts/2742438348.html","4ccf89ca5a5444685254a5a23d0cc94f"],["/posts/2864584994.html","b07698d04808109fe863e1620bf93514"],["/posts/2888309600.html","a3565e7b5784b3f8a757923a6c4f1a0c"],["/posts/2891591958.html","60b5d1c0250bcbf112fb6cf250f3f80b"],["/posts/2909934084.html","bec590889e4a7c1be128fc8e64fbe943"],["/posts/2920256992.html","03842c8eebffdc3e7438d50ed5480d90"],["/posts/2959474469.html","96dd328e6088834b3881ff0733bd72b4"],["/posts/3005926051.html","7ece09f171eb6f8f60f620a6006a073d"],["/posts/309775400.html","3dfcfc1ca7338b80b0291c60aea017db"],["/posts/3156194925.html","579630c9343d8352db91644b66b3d6e7"],["/posts/3169224211.html","e75e7b685f1ac8bc86b9175c3c65de44"],["/posts/3213899550.html","5d1603839109027328949b6b7e16eef7"],["/posts/3259212833.html","b6ed046fcac44f07877fc11ec67a35ce"],["/posts/3266130344.html","1f5cd05aec3861bed62ce2612f2f0d57"],["/posts/3292663995.html","37dd0afcfe0a18213042cf8fb05ffda3"],["/posts/3297135020.html","84b1477805f11cfa2ded430f2aa1313a"],["/posts/3306641566.html","526666cdbad1f3d3bda7cf4ad0655888"],["/posts/3312011324.html","9d56bee511dd4d71a935a9dec0a394e9"],["/posts/336911618.html","b3e4a69831212412719f3c41ecfb5818"],["/posts/3402121571.html","ff9cab2d75d8a349576f2cfe392b49d0"],["/posts/3405577485.html","482af8f1392987026884f7a3bd7db5dc"],["/posts/3498516849.html","a21328f4cab42d6559429aff754738e1"],["/posts/3513711414.html","47220b5d0f93102d75030f9d992022ef"],["/posts/3523095624.html","faebdd76a2baa53096a6b7e1faf75c79"],["/posts/3546711884.html","4d8264c213de6826c16b4eaf2df782f3"],["/posts/3731385230.html","6cb17c819d016beccbf2333f4b5cac5d"],["/posts/3772089482.html","e8d5a87f07f736220e8f7d6b32dbfd72"],["/posts/386609427.html","270fde7322519fbde2624174d0f91c53"],["/posts/4044235327.html","7bcd1c96aa81da16ed6010c79eb797a3"],["/posts/4115971639.html","d92e7b64a200f4af1977dcb4b5e40341"],["/posts/4130790367.html","bd5a39b732c7635e53f837222fb053c6"],["/posts/4131986683.html","eb2a9f4f27b0075419a5d8c167f12692"],["/posts/4177218757.html","a291cc33f41f27f21c7611d92c682599"],["/posts/4192183953.html","2c5d6fd12bd1f02189adcab1422e6b50"],["/posts/4261103898.html","216995df3949e8ef095b85441b5e1e2b"],["/posts/469711973.html","03cba90574906a759e3a7eb22547cf57"],["/posts/482495853.html","9475e09aef82bb5ba29704f0aaa86f2e"],["/posts/488247922.html","eb4e11a51fadb8944dc18f5df0c725a6"],["/posts/517302816.html","9a052d0d1bd61b93def0abcd78148006"],["/posts/570165348.html","e33f3458e4337dc8dd78871bf33c4d63"],["/posts/595890772.html","222fc9e45ae3611c01fabfc3baf68646"],["/posts/67485572.html","d323f9347d1875b45aba4b0410e94ade"],["/posts/694347442.html","13423127fbe5c79d1d48ec523dd1d40d"],["/posts/707384687.html","bee1b4bd838d509b99f1b6ae09e08504"],["/posts/71180092.html","c8fad62cd2110c298254ab8e043c94a6"],["/posts/716459272.html","dabd664cb9b56c582b67c99152998555"],["/posts/765481613.html","35189008bd5c1db5e5e721e92778964f"],["/posts/778231993.html","576c92e594f2a2a994afc4da3f046805"],["/posts/795397410.html","8de08ea22998e48bfeb776fe4cf88912"],["/posts/820223701.html","561b1eb63f998761dea52af81e2ff85d"],["/posts/830372185.html","efe8fcf929ff692362aac6422886b20d"],["/posts/88294277.html","0b1e96f09cc0b45228a8a62870e612fb"],["/posts/939963535.html","de2971d7a8eca90dd32d83e392ce0562"],["/posts/983786067.html","794023c2d64f4e4900915e1da951fe7f"],["/sw-register.js","f9d4a16f7ab93b9e8a2a90bc90b2bd3d"],["/tags/C/index.html","beb80dc5763d877b8e2c79188dc2a2fc"],["/tags/C/page/2/index.html","43791c36d09e5dd888a9b51aacd6d1c3"],["/tags/C/page/3/index.html","9a6ecc2113a7e35cf337c37ab39555c1"],["/tags/ETL/index.html","0efbbb5a6df742f66e4f7b50f53e2b9a"],["/tags/ElasticSearch/index.html","9248604f173d62eddb8e54b2b79be04b"],["/tags/GUI/index.html","798bf9ed6d865835fa26e1c1abe51256"],["/tags/HBase/index.html","e6c29e48b2ad5816e474639a667a89d5"],["/tags/Hadoop/index.html","f308b77904789c2ff3b9c979b7f312aa"],["/tags/Hadoop/page/2/index.html","de9366134dd1c58ec46df1930a1ae899"],["/tags/Java/index.html","aa317468d6dc3086e8c90dfcf86304e3"],["/tags/Java后端/index.html","d025e8e6882ed2ed33d3ae76557e197c"],["/tags/Java后端/page/2/index.html","49f360bb2387177fc27210a8ac9ae089"],["/tags/Java基础/index.html","750c24525c2b9afff99110748881ad75"],["/tags/Java基础/page/2/index.html","36e3223448b96f2ae59705caca259c9a"],["/tags/Kettle/index.html","67eb608979832751641bae16af02a69c"],["/tags/Kibana/index.html","133afbc1b4e8341b5133179df8bd5445"],["/tags/Linux/index.html","852d843b7544887fc15408d08df98782"],["/tags/Linux/page/2/index.html","2fca388e608b9e062dc547c3c91e1201"],["/tags/Linux/page/3/index.html","230930d5a32ff453294276b832e87c15"],["/tags/Mac/index.html","118dd7da617d1beb6272e060477198ae"],["/tags/Mac/page/2/index.html","207bc3d14b29dc9293fb932bd659347b"],["/tags/Maven/index.html","851fd7d54ea9b62f18988a9d82ea2ab8"],["/tags/MySQL/index.html","4043d3a2fcf9b36f5bbb38efebc46474"],["/tags/Python/index.html","a337d98e2969a639754f6c8e4b1a093a"],["/tags/Redis/index.html","b34959394e125c903c3c2559bc82f948"],["/tags/R语言/index.html","fd721d4006004340d1755c0faef05495"],["/tags/Spark/index.html","17119989f53e71ae429f8ff1d3826ef4"],["/tags/Ubuntu/index.html","478e00de9ad82835bf96d90ddf04ea92"],["/tags/Vue/index.html","6bd3578c502f2f8b746965ba8bcca0cf"],["/tags/Windows/index.html","2b258342a6dc55f9b013f3ea0bfe9287"],["/tags/ZooKeeper/index.html","ddf68fae68fe371bc790452571988ba6"],["/tags/bfs/index.html","9a07386086fe0a89a5b27d5aca21ef60"],["/tags/dfs/index.html","95286b5f034cdc078c9e6e681e37db14"],["/tags/folium/index.html","735ec6d6387a3d384134367528767eaa"],["/tags/git/index.html","d28a84729accc639434c4ed2bb177081"],["/tags/index.html","b59dfc97961fa30c5e9c0ecade0e5f4b"],["/tags/latex/index.html","6f613b7f0347bd0bf3913c92065f4acc"],["/tags/中间件/index.html","177e8f04ff84afecac2cfb706a49ff94"],["/tags/二分查找/index.html","1f5eeb46b3aefd52be477f8856ef86d2"],["/tags/优化类/index.html","46fb3dc09ad3afc816296c09f6eba59c"],["/tags/前端/index.html","ebbdfec78dcb1c4b253719deb21f332b"],["/tags/前缀和与差分/index.html","a57175dd16d521ea07edcb817ad2c945"],["/tags/动态规划/index.html","ffb82d8e16188d923cf59795553ab488"],["/tags/动态规划/page/2/index.html","ca667fb00bf7ca5ff88a0874c7eea095"],["/tags/博客搭建/index.html","3911914b304896f1b3902783aff52069"],["/tags/图论/index.html","9dcac4a711d0fc7deaab5844bd950681"],["/tags/大数据/index.html","3d67d984df12a41356b2ede72e38771e"],["/tags/大数据/page/2/index.html","19c078ae1f4552d6fb36b4e66c787474"],["/tags/操作系统/index.html","05153d9a2f0c71fdac3a9ea9a9594992"],["/tags/数学建模/index.html","2ca20db974df5e3019fe756bd1451124"],["/tags/数据库/index.html","c8847c6afdf59027500b30fa1cc18893"],["/tags/数据结构和算法/index.html","f35d01fcfae93be54ef150df5a7fee9c"],["/tags/数据结构和算法/page/2/index.html","964d70ad352cc1f21afb3a8f188fd070"],["/tags/数据结构和算法/page/3/index.html","da75c401bc52b7b2cdad2affcf85012a"],["/tags/数据结构和算法/page/4/index.html","80676b02139798a17df9df60f7ad0064"],["/tags/数组和字符串/index.html","243e4c606aac8904c6d5c40f7e0c3aa0"],["/tags/数论/index.html","532ffa3d8a688b85468dfb3d5d5b6d8f"],["/tags/枚举类/index.html","1f91ecebf42a2d4bccee5d16fdbc512f"],["/tags/栈和队列/index.html","a96d3c8090d121f36a8ffcd96c846bc6"],["/tags/树论/index.html","0dcd373c9741421bc21b8ec60a49b666"],["/tags/测试/index.html","873483488f15a476a26d31cf10ddb1e3"],["/tags/环境/index.html","1d85c02c34c69acc41a6d5e182b8fad9"],["/tags/环境变量/index.html","11442f532226366edd5707c6bb6a9e4e"],["/tags/绘图/index.html","9f7bdc8d0df95f075a1e2c12e41786a1"],["/tags/编程工具/index.html","7b22508c146936c0d4179e6d375584db"],["/tags/编程环境/index.html","53e04d149a20b5d60deac6035d96495e"],["/tags/网络编程/index.html","6bb1388eae2ce33819b1e02501bb6c4f"],["/tags/英语语法/index.html","efff9e5cc0c87747983fdd931db6b7b2"],["/tags/计算机操作系统/index.html","ba6926d03aafd59151978458a6bcf7d7"],["/tags/论文/index.html","2c79c3e399ff5d9dd5fa2f534430aa6d"],["/tags/资源下载/index.html","ad88712c080afb37750ff3c5d182eff2"],["/tags/链表/index.html","bda5d131fa107f92251f72cc34d75d1a"],["/tags/集合/index.html","20e7a4ed87ea05ae8ef73a314398b1a9"],["/tags/集群/index.html","c34492f5a6d23bb4ada433e82c92ca7a"]];
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
