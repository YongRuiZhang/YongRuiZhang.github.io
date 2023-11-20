/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","83ee6475673ca2335487765c71366abf"],["/about/index.html","65d239d3e98aaa44db181adc78417868"],["/archives/2023/01/index.html","0fcc860d3e47217d3b6ec40efb2d4d32"],["/archives/2023/02/index.html","3609cc9c408d34ff16306dde88f42c0f"],["/archives/2023/02/page/2/index.html","50b057b551a08ae262f8cf9606160166"],["/archives/2023/03/index.html","89e7e6b14b617faa415fcacb935eccff"],["/archives/2023/05/index.html","962c5b525bb67641e79e21db21b68fa2"],["/archives/2023/06/index.html","241886ab99bb817ea160092b33d01e2e"],["/archives/2023/09/index.html","28c4d7fcc4a515dbd6cacb1b8b949b97"],["/archives/2023/11/index.html","ae028392f9c81742f830759c0018cf00"],["/archives/2023/index.html","cf7c2756ce733e3cc313f041b806742f"],["/archives/2023/page/2/index.html","2c4707c295c3813cb365a34dfadd9527"],["/archives/2023/page/3/index.html","ee3a468f36566b7813fed81ed21d46b6"],["/archives/2023/page/4/index.html","2a385201627dc46c68572c5069c077c0"],["/archives/index.html","a3034da3adc4b5a99ed9a656d1cd8856"],["/archives/page/2/index.html","4fed07e76c889fa9098c61b16b5b2576"],["/archives/page/3/index.html","cfbbddebf3a773453f0ae57752c096c7"],["/archives/page/4/index.html","778b471ef6c54056b13dbfb99ddf7426"],["/baidu_verify_codeva-qQP2iZOMLX.html","ccac93c2fdab06faa3bee710fddcac1f"],["/categories/Java/index.html","ef68f73e531985ece2395f92d5990b30"],["/categories/Java/后端/index.html","745ddc888ec8de8b26503bb943a7d146"],["/categories/Java/基础/index.html","675be708cde69325e81b76ee490ebcec"],["/categories/Java/基础/集合/index.html","51285be34d40e2582b57dfc42f29e369"],["/categories/Python/index.html","752757ea7d007c65829bd43e61d4c232"],["/categories/Python/编程环境/index.html","793a57fe562651adfbb8677fbbab7e18"],["/categories/R语言/index.html","921b2e783c929c7a1e7f14d8014c462b"],["/categories/R语言/编程环境/index.html","fd1f4e0f92b6e25c381b15b4f77b5788"],["/categories/index.html","872d8ef167d7099e3fd346c6e1a9012a"],["/categories/中间件/index.html","646660efa915780323585a5582153635"],["/categories/前端/Vue/index.html","5de136ccd2a1c57f6cf92bd4cc5cc2b2"],["/categories/前端/index.html","65898071621fd6711b3ea986ef712153"],["/categories/大数据开发/ElasticSearch/index.html","ed1a8e7edd99dd685bf16911b3892207"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","145b4678b945b918d9891b7f2c76e95c"],["/categories/大数据开发/HBase/index.html","0b25cf77ba1dcd89d9924a337544ddfe"],["/categories/大数据开发/HBase/学习笔记/index.html","9f5235daed3db4b6ed7c0f70d86aac2e"],["/categories/大数据开发/HBase/环境搭建/index.html","df4cff45a87b00916e768d6aa3a0150d"],["/categories/大数据开发/Hadoop/index.html","8d69adb44d842b2da20ac5a2625e4cd6"],["/categories/大数据开发/Hadoop/技术/index.html","521360d21246361defcbf642ec5525eb"],["/categories/大数据开发/Hadoop/环境搭建/index.html","46e9c2a19681c89a959004d595e011c6"],["/categories/大数据开发/Redis/index.html","60fab571cd24acb3ef4a7a42361ff442"],["/categories/大数据开发/Redis/技术/index.html","f8a306a9c3686aa4e8270100db6e3f7d"],["/categories/大数据开发/Redis/环境搭建/index.html","6fa05617786ae671c78122a0525cff87"],["/categories/大数据开发/Spark/index.html","05705d2e3fa14bdc9b91f21cfbcbf6a0"],["/categories/大数据开发/Spark/环境搭建/index.html","8151413aeaf6c43f5d3981a25703453f"],["/categories/大数据开发/Zookeeper/index.html","cf138295acd445fd5343d2c418ffe28f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","70181bb0a79576821a0741efa46f9524"],["/categories/大数据开发/index.html","9bf391c603c0915c73b7fc46b14e2bb8"],["/categories/操作系统/Linux/index.html","165eec09ccb5de0527c082e970f7e5be"],["/categories/操作系统/Mac/index.html","3509c80af77ed0ef735481fcf44f2cf3"],["/categories/操作系统/Windows/index.html","60880b2409c626929d35afbe81f9928c"],["/categories/操作系统/index.html","a96544e3a0041acd437a1a363a0cb887"],["/categories/数学建模/index.html","1a79d68bfc05ee273f3ed5a4ecd1774b"],["/categories/数学建模/latex/index.html","0d9ffd0705e1d8cc7aa5e5cae39c03cd"],["/categories/数学建模/优化类/index.html","59063e7f355fd4abbbf253055fa4e1a5"],["/categories/数学建模/优化类/现代优化算法/index.html","c04b9d388b6c003e45e5428f2f511021"],["/categories/数学建模/优化类/规划类/index.html","63717ab29d7ca87e71e62fd3366f469d"],["/categories/数学建模/绘图/index.html","9ef94e42d650b3adb89901f4b8629e8e"],["/categories/数据库/MySQL/index.html","cb762f4bc0c3c03f20f9507aa5738570"],["/categories/数据库/index.html","92fd7b92e3c1dcdb9cbb4983fda0f685"],["/categories/数据结构和算法/index.html","87f6a7f0db51b171ba2e408e9da436a1"],["/categories/数据结构和算法/page/2/index.html","3bb68e62dab9414ad71aeb91d188a7cc"],["/categories/数据结构和算法/基本原理/bfs/index.html","cbcfce17bf1a9528417e20852ca3ad21"],["/categories/数据结构和算法/基本原理/dfs/index.html","d92fa71d9c26ff8dbb4c5889571d9993"],["/categories/数据结构和算法/基本原理/index.html","63bb0ebc245c6342bfee5026c7ea61d8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","207de99fe51656ec51de04b29d1124e6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5ad3d26d29946d9782e34c67aefe290c"],["/categories/数据结构和算法/基本原理/图论/index.html","621701233b35fef8d0a23fbfcd5cf355"],["/categories/数据结构和算法/基本原理/字符串/index.html","7440dff5e4a61809a69024c4c2388412"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","40ebf2c01c06dff0a87621c192940991"],["/categories/数据结构和算法/基本原理/数论/index.html","3c9580c052fc3162ae13e5dc0eb2788f"],["/categories/数据结构和算法/基本原理/树论/index.html","7f7ea164b2261b60ed4c906619fdda02"],["/categories/数据结构和算法/基本原理/链表/index.html","1d6abd59cb5c7c2bbee829d6400be3c7"],["/categories/数据结构和算法/算法题/index.html","6bacf36fc87f235a5e07afebbb580dc0"],["/categories/数据结构和算法/算法题/二分查找/index.html","7a752bcfbd8b4fbbde2059e6d6830567"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2cf05de9ada4a38355d4714f51ad270a"],["/categories/数据结构和算法/算法题/动态规划/index.html","304ab71e5e547eac4ba603bc6dcf7e3f"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","04a10a4fa151efbc9911faf5c18a82a8"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","91f86241e60e6ed77a05e457e8b0a6a7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","0e64ad3aa2ea0334c7289bc2045e5af6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","6c1c70ce0411a044ac2678de2cd76895"],["/categories/数据结构和算法/算法题/栈和队列/index.html","171fd4b3f8db5f435a5be6ca6b91941c"],["/categories/数据结构和算法/算法题/树论/index.html","3bba99813038490aa3e9ac0b91aef4c7"],["/categories/杂七杂八/index.html","c37e91874539644f7d416fe6cde4f2fa"],["/categories/杂七杂八/博客搭建/index.html","7cb10b2dca52a857cb028f4253e71808"],["/categories/编程工具下载/index.html","d24c689e48400199819746d345c46c85"],["/categories/编程环境/index.html","aa2e00ed723d966d605865894abfa8ec"],["/categories/编程环境/大数据/index.html","e4b79f5e693a467665ae8f2aa0950ca5"],["/categories/英语学习/index.html","a1b5b24b70bab3fd06f8b89bd4a30ed7"],["/categories/英语学习/英语语法/index.html","1fa595be2cb611ef6312df4cd4f09210"],["/comments/index.html","c64e2bb2b3286c79f6cc865057a4f6d8"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ca60a542accff4de5c048b62d6f9d804"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","64f580a0306333a217e63aa655969e38"],["/movies/index.html","2c610d52d67e0cc0d39784a8bc7c4072"],["/music/index.html","105a76313f7c4e4b80e8cd010db3144a"],["/page/2/index.html","fc018b7400d9ae01e2f7246795e3e6f5"],["/page/3/index.html","0281e84310d5048edbb5e23ac38b6bfe"],["/page/4/index.html","b218d1b5501ff10fc7765f7cfeeb813a"],["/page/5/index.html","3f3f4ae40eb6ecc250ded4947819acdb"],["/page/6/index.html","ae2598ec365bf7a42baef1abe022bac1"],["/posts/1021360842.html","b38cd57d6db513503276ac34d804a3b6"],["/posts/1120620192.html","d56e8d98c46472f5c4a82c455e5461b5"],["/posts/1141628095.html","4a29d36f76882fbd06d6ffd5084da96c"],["/posts/1168613674.html","99cc08a02bfa008ad4390e05f61de391"],["/posts/1219920510.html","db45efbe630560c648b536437b56609d"],["/posts/1222166338.html","03d00df1d1f91b3207e6e634d0b0257b"],["/posts/1259097482.html","99518320b6a0e1d5445c7b8a4120d5ce"],["/posts/1271036369.html","d04b306d396079dd1e02532862c5e8c6"],["/posts/1312847445.html","8b2c4f7e75db3c1630506ac03fa49ff7"],["/posts/135355774.html","18ddb3f9068d2e8b25cef70359db4b2d"],["/posts/1375344716.html","3d36b7662f7af9958e3afd39729d6d6f"],["/posts/1388991698.html","87700c6116818933d5903bacf7019e79"],["/posts/1410315814.html","2487596d22c02eb3732409bffef21911"],["/posts/1452790229.html","fdafed824c86095b20a13a54b795e033"],["/posts/1470079884.html","1ae28ace25978a9c402d0ed728476c2a"],["/posts/1470079885.html","994afce80a8d8b4e6dbf4500bca1ceb4"],["/posts/1470079886.html","4b9d5ee69072cca3907b8e79fcdf0750"],["/posts/1470079887.html","7186562193a336d536bd24cf916f86df"],["/posts/1498536549.html","efa9e80588c90288139eb1bbcf898d64"],["/posts/1547067935.html","e03e1b69c0d025b503ce43356d258364"],["/posts/1557866301.html","ee3ae749c2e9fd7257449d748da374f7"],["/posts/1571776361.html","82268dccf9493e9771eaf23644ad8327"],["/posts/1605124548.html","11c85452a0f24c31f4604f8ed77a3e85"],["/posts/1633036852.html","5e1ec324c0fcede54f2be6a15e846438"],["/posts/1674202625.html","c0da69bc6bb9d1cf18c2ad32db10b068"],["/posts/1765123828.html","6fdc1bdff069a0e1cea4b7ec7af77661"],["/posts/1767336200.html","b05d76bdedc6d25e9eb839a127522180"],["/posts/1776114197.html","30e417fe51c54e4ee8608577f2394de3"],["/posts/1817748743.html","483210dd71ef6d5ae63033946a64ed0a"],["/posts/1925125395.html","388daecca348575b38244d2db8524206"],["/posts/1966191251.html","498bb01150e293c944a4477eae8cbc78"],["/posts/1987617322.html","85a922c22c1d28b7dce32c2f08b4f610"],["/posts/1999788039.html","1337264eee0519ddedd3cfdcd5d4ad35"],["/posts/2075104059.html","f12c4ea163e8c72a931ff2c52657ef22"],["/posts/2087796737.html","94cfb9a0541cf4f27d061a5594210f2d"],["/posts/2106547339.html","4a3642db743ca76c562640a742d75b3c"],["/posts/2207806286.html","000e4b553e56b89c6207f49191a8be07"],["/posts/2225903441.html","b0dd28fb861d3a76c3bb0e53f57e1b74"],["/posts/2265610284.html","8cfaada833b6466d4228d2d3107101e7"],["/posts/2281352001.html","7c0e238338a588575e6fcad7c239ddef"],["/posts/2364755265.html","e2ec3babe719e91e56fc4dc490e0eb35"],["/posts/2414116852.html","5488c31820c47cf950648ccfa9c04eda"],["/posts/2421785022.html","010bce60bbcf41af854f3f673ff24743"],["/posts/2482902029.html","ded102a78ce25edaad1582890cfa28e7"],["/posts/2495386210.html","3e8b96a4d3b68b9a76d9383e4c8aa79c"],["/posts/2516528882.html","15112d8af7ca8d71aaf6832d20456e64"],["/posts/2526659543.html","b7e60e4d40f354b0f9e1104a33bd5de5"],["/posts/2529807823.html","baeb1e71290a788440d64618e58dc120"],["/posts/2596601004.html","3f0df13af9118e5a5c4cb4b781444a33"],["/posts/2742438348.html","6301e4b960d07b9b364aa8ca6307ac9b"],["/posts/2888309600.html","48cd1bc5118995a038ff2348a606b1db"],["/posts/2891591958.html","0d42fb059efb8a6b993f01ded671d754"],["/posts/2909934084.html","52439e0139bf189245695bf395790f87"],["/posts/2920256992.html","c99da50bc776fc4fc9a77cc87580b2cb"],["/posts/3005926051.html","917a1498fff96aae290e1af00d78b1f2"],["/posts/309775400.html","f364b017c4809d934605735006af72f4"],["/posts/3156194925.html","dedb6fd102714e1f6661aa8dba5c627f"],["/posts/3169224211.html","24cd1860a339a24470e191181470748d"],["/posts/3213899550.html","dc516729a27eb66f1d8f873968c99b21"],["/posts/3259212833.html","85a74778cac1277a157a07d7c6c25520"],["/posts/3266130344.html","fce6d4d08a7ef49eb547f021b10a488b"],["/posts/3292663995.html","15af2bc3204362dc50bf3ed3f98c7422"],["/posts/3297135020.html","177d2ca00410d8bdd405f01bf381e9ee"],["/posts/3306641566.html","ddd3a67fdc9b24610a49b75c6b594931"],["/posts/3312011324.html","536fcee8e973b4dfb0596a2713f172b1"],["/posts/336911618.html","f76940976f6e9c96266074483faca7b1"],["/posts/3402121571.html","072af5f0cf242ff2f9c1152a86baed74"],["/posts/3405577485.html","bba85ff3795c3c3249d3521299971393"],["/posts/3498516849.html","ed52883de65ecda0ecfff6740451d553"],["/posts/3513711414.html","be212b10779a08689ff518552e2b9a71"],["/posts/3546711884.html","9e5741c6f65d88e4ae41a01c6afca5b2"],["/posts/3731385230.html","a48eacf1d9c7343ff7ac83a9f97e7c80"],["/posts/3772089482.html","b98629650eb747db84197b39362b3bca"],["/posts/386609427.html","ff4a1e4c2fc0e5c40c18d3723d33b5a7"],["/posts/4044235327.html","78980cad06c490fa9b57ee877bd3fba2"],["/posts/4115971639.html","eb6c4d55c552706d0c097d60fd139295"],["/posts/4130790367.html","f16b0510aaa32f5345eb1fed7708ded8"],["/posts/4131986683.html","8372e8d98689fe1ac839db8bb78ba08b"],["/posts/4177218757.html","5cc2f2d3b8b843df854a517f343dfa2f"],["/posts/4192183953.html","54b546a69f74f9296c25a5f03b5af573"],["/posts/4261103898.html","55d3646c4697b8e1baaa13cd9f21415a"],["/posts/469711973.html","4ef6414b647d0d65820c442a21a26108"],["/posts/482495853.html","db87c38872783fadd144e14f5a235287"],["/posts/488247922.html","110a9d68de11d781a6da438aa5cf1d9d"],["/posts/517302816.html","88f8ab0878e4c44ac473e141c41456c5"],["/posts/570165348.html","ef800a7308dffb8ede416ed3887d4ede"],["/posts/595890772.html","dd1972b0883bbb7f70962f61b4fd2286"],["/posts/67485572.html","0d9796fd3fe16b40e3907aac6423bd87"],["/posts/694347442.html","2db3a7dd9c1100b98a1cc30023107b04"],["/posts/707384687.html","a51e33101eb012bedf8c288b5cd7f053"],["/posts/71180092.html","947bb0fd51348a4a8bb2d8aa154d1aa6"],["/posts/716459272.html","f7f8991ed896a7db2b9ccbe01e03473e"],["/posts/765481613.html","f44f548bec2957e8918c06d8167f44b1"],["/posts/778231993.html","341ae0591e843d6ad54239ca28cfdb21"],["/posts/795397410.html","4457b09b032b90694dae05cb1ca75241"],["/posts/820223701.html","b7529060f7d6cd7e3a9e875ba6b07397"],["/posts/830372185.html","4f93a9d10378655504edb6011cf71bc5"],["/posts/88294277.html","b24995da5c5bb8cd264ae2bc6223cdd6"],["/posts/939963535.html","50b3711f19106e049d84d23fd9825424"],["/posts/983786067.html","ad1f78ef98346bb67841a8b55f4dfacf"],["/sw-register.js","f00194b9ae44d5a84f0232c0625f33f0"],["/tags/C/index.html","bbb4d3a73df7aa37f9f57c61ccbe1565"],["/tags/C/page/2/index.html","188999e84da9aef64e6ea363cd231086"],["/tags/C/page/3/index.html","dbdda5ba351d44210f0798241c6e07de"],["/tags/ETL/index.html","2375a8bf7c6dddadfe50d22f4ad272f1"],["/tags/ElasticSearch/index.html","24e3427aa16d92466ad5d285b2de734f"],["/tags/GUI/index.html","6498598a83945a7109731d0a756a5c30"],["/tags/HBase/index.html","1fd1397b1d62d61cbc4ade5ac9288026"],["/tags/Hadoop/index.html","93bb8f67e3df0463db93a33c14c65ef7"],["/tags/Hadoop/page/2/index.html","d7c6167b425bfcf7885bba836b378545"],["/tags/Java/index.html","910a4c0d56a5ec36c665f1301c9f7c66"],["/tags/Java后端/index.html","07bf9dbfd80455aed17d2937b93642f8"],["/tags/Java后端/page/2/index.html","888822ac1a20851b54b1ddc9c1efea4d"],["/tags/Java基础/index.html","067b5405c451bad714efca690de8bcb9"],["/tags/Java基础/page/2/index.html","1471658d13634e9970709efc38a80aaa"],["/tags/Kettle/index.html","31ac6da29fae763ee8d5b6f123215154"],["/tags/Kibana/index.html","a4f5b8dd0898d7a10b096bab4d897f02"],["/tags/Linux/index.html","38a288ade7a5ceefe6c4198a9b1edd92"],["/tags/Linux/page/2/index.html","b21fb5f7fd4a84381c7232f7978b141a"],["/tags/Linux/page/3/index.html","fab846d892806ac32187becd331d25f5"],["/tags/Mac/index.html","bcdf0cb503a402294730b4e576256a4d"],["/tags/Mac/page/2/index.html","4078505e07d44c73183be0ae91a24afb"],["/tags/Maven/index.html","92315d3229862f09aad5afb80fe54720"],["/tags/MySQL/index.html","bf5590e550d2e279efde71ba4f502b79"],["/tags/Python/index.html","6caf802e9bcc04deaf4efc3444f3aeee"],["/tags/Redis/index.html","38a78a86721024b5d3d4c38a63968018"],["/tags/R语言/index.html","92b69645cf2f44b2d4bb7ba398fabbcc"],["/tags/Spark/index.html","ee49c4c4405671c72200b09126f03af9"],["/tags/Ubuntu/index.html","33d4d68b125e45a3446024d4239c8808"],["/tags/Vue/index.html","978dd695c4d716cbd1b39556685a2b44"],["/tags/Windows/index.html","e785e8d489ca125f08ffd206d24e8aa1"],["/tags/ZooKeeper/index.html","e2864166d6219a1e80251a2fc1ddc591"],["/tags/bfs/index.html","b8ddf2c393ab63e247b288e4678058c9"],["/tags/dfs/index.html","60706137a1daa8171e5581d0b09c15b2"],["/tags/folium/index.html","ee731026bb5608a034e811a4b93c6d40"],["/tags/git/index.html","af11e2464faac746916258e1d6203b92"],["/tags/index.html","4909f9fddb7aad05713b32fe9b921260"],["/tags/latex/index.html","8b3e4bb42bb75cfc24dd1fd128533480"],["/tags/中间件/index.html","bceb3cefe9a4bea603fa6b4b911132b6"],["/tags/二分查找/index.html","c3537079b59406a374b55124ec5f2965"],["/tags/优化类/index.html","fc318ac79b47f36a450da4ae0d068c44"],["/tags/前端/index.html","272a9109007392b3576de84c35ebf660"],["/tags/前缀和与差分/index.html","0820b5412135b53fd9c8e07530389f8a"],["/tags/动态规划/index.html","ae5b7311fb8d010ff41be7e88810db58"],["/tags/动态规划/page/2/index.html","6fae57b2417ab01f139c4b18f2a00cc3"],["/tags/博客搭建/index.html","ccfc3284c50cf565bda7608fdfd48334"],["/tags/图论/index.html","11b080d2bf7f82a6b65121f5ac39ecce"],["/tags/大数据/index.html","64c2ab3cb5ecc1e9e796216b19c93878"],["/tags/大数据/page/2/index.html","8357385bd070fc473b5066922372138e"],["/tags/操作系统/index.html","ce3803276b31c41a203528446f5a1bf7"],["/tags/数学建模/index.html","fde409fb4591b1b4f29d4f780578686b"],["/tags/数据库/index.html","7eeb6dff821cd44273c6b4b287bb0e8e"],["/tags/数据结构和算法/index.html","413a9ca3a71c25572bb1e6484c2c4af9"],["/tags/数据结构和算法/page/2/index.html","c1ea4b47d67b2560bf5cf306eb4d4233"],["/tags/数据结构和算法/page/3/index.html","0010d6bf420415df3629ef6c6c57b05f"],["/tags/数组和字符串/index.html","e7a3f45838656935b532487a8bbaf3c2"],["/tags/枚举类/index.html","366e91e0eec74aeafee4861f47123256"],["/tags/栈和队列/index.html","d56471d6ea9ae3607ad678acc2f2dcc5"],["/tags/树论/index.html","afffe2f3483fb734458b6a72a12a2f78"],["/tags/测试/index.html","a603513cd0b6da5618da96da6645c9b7"],["/tags/环境/index.html","fada13cd9b2ea43224a1f3fa1a2923ea"],["/tags/环境变量/index.html","adefd7a03a17d060fe4389c9c8e89f8d"],["/tags/绘图/index.html","4583cec36962198d830a81877ef5f96b"],["/tags/编程工具/index.html","d02e7f74786cc5f8a8dd4bc2a7ade975"],["/tags/编程环境/index.html","aaffa075f275aa6c2fb5019557554998"],["/tags/网络编程/index.html","e7752f310b4f3415a2717783b788b023"],["/tags/英语语法/index.html","0f24fc78038064e8f28b863fd3e5bd54"],["/tags/论文/index.html","d15578bc50d9dbcce2955ab62e612081"],["/tags/资源下载/index.html","6d26b5254b30edda358c34c2b8243e73"],["/tags/链表/index.html","5567868e083a1efff655748a6b4886c8"],["/tags/集合/index.html","2acde0ed23bc5138523700e03bfb5b58"],["/tags/集群/index.html","1abdc99556ee597f9a84d6d81b777981"]];
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
