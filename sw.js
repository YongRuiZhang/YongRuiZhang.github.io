/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","bf432901f8488ec63af9aa4976d41bbb"],["/about/index.html","09acac93a9a99538b9b8e533f4126556"],["/archives/2023/01/index.html","c2f81da7b13a061ea1a996a1d4276d82"],["/archives/2023/02/index.html","fdfaee5b2bb456ccc4889338d93c8938"],["/archives/2023/02/page/2/index.html","7a83849bc1d0e1a542cfdf2100332f3f"],["/archives/2023/03/index.html","2d7ca1da6a95fe5546be31a5bc8b29a2"],["/archives/2023/05/index.html","1de704ddd16d4735ba30ec7b6506584d"],["/archives/2023/06/index.html","aeef48fb74b0c47974ffa62792e7103a"],["/archives/2023/09/index.html","87a89f2e85f5cff71383f72c3a527e0c"],["/archives/2023/11/index.html","4754a4630e8fdd6f6a53c318c3b9cdfd"],["/archives/2023/index.html","6a2d67d8d701dbb8980f016e35cf63d9"],["/archives/2023/page/2/index.html","a8eaca1043037ae4a3e98abe0ea37e99"],["/archives/2023/page/3/index.html","b67f203dc863e2ab629bda28a18f7f4e"],["/archives/2023/page/4/index.html","fa2077b990de60d99686d6e9663998f1"],["/archives/index.html","c6b555eb4e30a72668d17cad1ffda38d"],["/archives/page/2/index.html","137d47d479c7f20a432f5ba22153625c"],["/archives/page/3/index.html","3443ab949b273ab0e30a934dd98c664c"],["/archives/page/4/index.html","aa2ca66c6ec8d57fb5b1718d98f62325"],["/baidu_verify_codeva-qQP2iZOMLX.html","11f3886d1cedc60eea44e2d8f672c860"],["/categories/Java/index.html","17f7c42a9039ca2bb79b6b711d883ed0"],["/categories/Java/后端/index.html","e8ab24c1ca7d2b7968f985fdd4ff51c7"],["/categories/Java/基础/index.html","83dd7320a5ccc29166e1287a12484a88"],["/categories/Java/基础/集合/index.html","304673de03f057ee5facb8320c1eda5d"],["/categories/Python/index.html","697e92bb1eca04cc8cc4c2ad83aa7458"],["/categories/Python/编程环境/index.html","ea9ef2bf302bd7a3bb84563277b66b85"],["/categories/R语言/index.html","dfcc8c3978ed3c00029bd50177940ed9"],["/categories/R语言/编程环境/index.html","aed792dd744884d1a7e3c006d115502a"],["/categories/index.html","c8fa615827316a4c3fe89f94bf1a8610"],["/categories/中间件/index.html","27f7a77dcd085aeaa450588e8fd2d1c9"],["/categories/前端/Vue/index.html","3057bd5d43e2d3bc7d4c849424491d97"],["/categories/前端/index.html","d1d28a1dbcaffeafd161c32aaa4eb419"],["/categories/大数据开发/ElasticSearch/index.html","773065b7cca96c48a630b9a4435946d5"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d76e44fc8f3f2ff8379569fedd6c776f"],["/categories/大数据开发/HBase/index.html","54585999d71eeb5d0e790f38793d1007"],["/categories/大数据开发/HBase/学习笔记/index.html","4d1a043110f50e36f92ee61d3db783f3"],["/categories/大数据开发/HBase/环境搭建/index.html","881b74ed52a5fa3b8f2e92f9dac5b879"],["/categories/大数据开发/Hadoop/index.html","cf8e8b414e57d94927ca83bf0738e535"],["/categories/大数据开发/Hadoop/技术/index.html","aa9b95a31f48c1fef9c5db0f5ea25d34"],["/categories/大数据开发/Hadoop/环境搭建/index.html","74673072d0eaf37162c57d2474fa99ca"],["/categories/大数据开发/Redis/index.html","2d54133558e45da3c209b844750f4675"],["/categories/大数据开发/Redis/技术/index.html","3e4d9a3dcbe6c3b08efde46121013381"],["/categories/大数据开发/Redis/环境搭建/index.html","1f2b8cb2304b5bb59ba9aebb4d37495e"],["/categories/大数据开发/Spark/index.html","8a209f3d6b093abc5b623320558918c4"],["/categories/大数据开发/Spark/环境搭建/index.html","aea705dc537775b592ce3e96e55cc917"],["/categories/大数据开发/Zookeeper/index.html","08eb8aeeb88cb0f164bb5f48879c8297"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3287792161df63b62f7d0a5a66583507"],["/categories/大数据开发/index.html","9df3f1550d4035ff21a5510f630bf8e5"],["/categories/操作系统/Linux/index.html","b2318b0e04e3a3e604fd67c2337b326a"],["/categories/操作系统/Mac/index.html","5c3eb47f21e02b8c159fc5d60bfefd5b"],["/categories/操作系统/Windows/index.html","f2c0c060147e4ddcd1a41818f5ec46f0"],["/categories/操作系统/index.html","c9724bad27714b529abcc5e54d3a6ae2"],["/categories/数学建模/index.html","ddb8f05cfbfc0df5d6624e5e2fad1c1c"],["/categories/数学建模/latex/index.html","c86b8b0b6880a31263e2aa16af61919b"],["/categories/数学建模/优化类/index.html","fd1e7273cd64e495b5608cf651494497"],["/categories/数学建模/优化类/现代优化算法/index.html","39d236eaf5e2a5218b11e95cb2771cfd"],["/categories/数学建模/优化类/规划类/index.html","1096f8afb9f77a8e64aa9f7dab0ae7f3"],["/categories/数学建模/绘图/index.html","b741f629695fef21d0796d2e006be011"],["/categories/数据库/MySQL/index.html","c5430889bd1a40eed6e32e8230c59ab5"],["/categories/数据库/index.html","5f8e4eb96260a66a7817a9f3f427d665"],["/categories/数据结构和算法/index.html","7ad5258e5f00277130d6c495a3b17c1a"],["/categories/数据结构和算法/page/2/index.html","e8bf43568c1e2d9b13f5d9e331e7e2ce"],["/categories/数据结构和算法/基本原理/bfs/index.html","3627f7d4e1c7477e6830ef6fb8fe9482"],["/categories/数据结构和算法/基本原理/dfs/index.html","9e24e5a258f54ed05a2d15fadff18214"],["/categories/数据结构和算法/基本原理/index.html","7c0a638fb09ff0e0a21839f86915511c"],["/categories/数据结构和算法/基本原理/动态规划/index.html","34e394a78660e0221132f611cc8b7eef"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","610a24bb5a99eb1126e773ee54870129"],["/categories/数据结构和算法/基本原理/图论/index.html","2b3f54f1fb40c97490dbb8ca3a431571"],["/categories/数据结构和算法/基本原理/字符串/index.html","ecca3b4aa205f676e2e0a69ca532ba89"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","90cc430450adff3d57cd5f811244eceb"],["/categories/数据结构和算法/基本原理/数论/index.html","3934002e1d9b0ecaa13d855cb829451a"],["/categories/数据结构和算法/基本原理/树论/index.html","a73c5ea8a001227c9d2c901f8b20c3ed"],["/categories/数据结构和算法/基本原理/链表/index.html","2691a13d0243952261e12705d144a350"],["/categories/数据结构和算法/算法题/index.html","6e91c2033488434b022335992ffae391"],["/categories/数据结构和算法/算法题/二分查找/index.html","700981f834d69d736ed59f193219b4eb"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1213c2fcc91972f56dc63f6d4628b2e8"],["/categories/数据结构和算法/算法题/动态规划/index.html","e547e9caaecd3d6dcb44b2352a100800"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","c86de48f6cda7e35798b6078fc071424"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","e791de9c5e46343506a7224c60288e21"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","71f38e732bb92f70731dbb96bd669f42"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d8360e09bc68a9973657ce9ac9cc53ed"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8805eba2795e9c865852a516030c8967"],["/categories/数据结构和算法/算法题/树论/index.html","280b7dab0db2d3464c9d313a24513cf2"],["/categories/杂七杂八/index.html","6f7d7c9f7430d381902b4f8cfa60232c"],["/categories/杂七杂八/博客搭建/index.html","e3b7d8097d33de3f502cd808077d04cc"],["/categories/编程工具下载/index.html","2d5f5806a08b9758e9a5be4178f47973"],["/categories/编程环境/index.html","1f2d80251ff9e60cf7883e1338aec7cf"],["/categories/编程环境/大数据/index.html","0eae7f6b657300ad7f62eef762d4636a"],["/categories/英语学习/index.html","5ab2c83e373ce65898ce92405238bfd5"],["/categories/英语学习/英语语法/index.html","892bd29a2e5dd9926f160ceba7e26d82"],["/comments/index.html","220c3d04188e6a783f4a595f074b294e"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","35e1c9c0f570af390a093b560c50937c"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","eaf373c5028d44a9d292450e7ec13330"],["/movies/index.html","d3a606dcd9fb52b2269702bac87e7d7b"],["/music/index.html","6c062cf139760e098face0f4b723c8cc"],["/page/2/index.html","ad4bcec98e1b15346f651c2de69e0301"],["/page/3/index.html","eed05c8711509217b88e959a906ae066"],["/page/4/index.html","091004038c4901c4dad3122a8c36e729"],["/page/5/index.html","44f09a00e9e39178d324f3d6a70c1c2b"],["/page/6/index.html","51b54657eb852b1d0ea75cd3e3d4af9e"],["/posts/1021360842.html","b38cd57d6db513503276ac34d804a3b6"],["/posts/1120620192.html","d56e8d98c46472f5c4a82c455e5461b5"],["/posts/1141628095.html","4a29d36f76882fbd06d6ffd5084da96c"],["/posts/1168613674.html","99cc08a02bfa008ad4390e05f61de391"],["/posts/1219920510.html","db45efbe630560c648b536437b56609d"],["/posts/1222166338.html","03d00df1d1f91b3207e6e634d0b0257b"],["/posts/1259097482.html","99518320b6a0e1d5445c7b8a4120d5ce"],["/posts/1271036369.html","d04b306d396079dd1e02532862c5e8c6"],["/posts/1312847445.html","8b2c4f7e75db3c1630506ac03fa49ff7"],["/posts/135355774.html","18ddb3f9068d2e8b25cef70359db4b2d"],["/posts/1375344716.html","3d36b7662f7af9958e3afd39729d6d6f"],["/posts/1388991698.html","87700c6116818933d5903bacf7019e79"],["/posts/1410315814.html","2487596d22c02eb3732409bffef21911"],["/posts/1452790229.html","fdafed824c86095b20a13a54b795e033"],["/posts/1470079884.html","1ae28ace25978a9c402d0ed728476c2a"],["/posts/1470079885.html","994afce80a8d8b4e6dbf4500bca1ceb4"],["/posts/1470079886.html","4b9d5ee69072cca3907b8e79fcdf0750"],["/posts/1470079887.html","7186562193a336d536bd24cf916f86df"],["/posts/1498536549.html","efa9e80588c90288139eb1bbcf898d64"],["/posts/1547067935.html","e03e1b69c0d025b503ce43356d258364"],["/posts/1557866301.html","ee3ae749c2e9fd7257449d748da374f7"],["/posts/1571776361.html","82268dccf9493e9771eaf23644ad8327"],["/posts/1605124548.html","11c85452a0f24c31f4604f8ed77a3e85"],["/posts/1633036852.html","5e1ec324c0fcede54f2be6a15e846438"],["/posts/1674202625.html","c0da69bc6bb9d1cf18c2ad32db10b068"],["/posts/1765123828.html","6fdc1bdff069a0e1cea4b7ec7af77661"],["/posts/1767336200.html","b05d76bdedc6d25e9eb839a127522180"],["/posts/1776114197.html","30e417fe51c54e4ee8608577f2394de3"],["/posts/1817748743.html","483210dd71ef6d5ae63033946a64ed0a"],["/posts/1925125395.html","388daecca348575b38244d2db8524206"],["/posts/1966191251.html","498bb01150e293c944a4477eae8cbc78"],["/posts/1987617322.html","85a922c22c1d28b7dce32c2f08b4f610"],["/posts/1999788039.html","1337264eee0519ddedd3cfdcd5d4ad35"],["/posts/2075104059.html","f12c4ea163e8c72a931ff2c52657ef22"],["/posts/2087796737.html","94cfb9a0541cf4f27d061a5594210f2d"],["/posts/2106547339.html","4a3642db743ca76c562640a742d75b3c"],["/posts/2207806286.html","000e4b553e56b89c6207f49191a8be07"],["/posts/2225903441.html","b0dd28fb861d3a76c3bb0e53f57e1b74"],["/posts/2265610284.html","8cfaada833b6466d4228d2d3107101e7"],["/posts/2281352001.html","7c0e238338a588575e6fcad7c239ddef"],["/posts/2364755265.html","e2ec3babe719e91e56fc4dc490e0eb35"],["/posts/2414116852.html","5488c31820c47cf950648ccfa9c04eda"],["/posts/2421785022.html","010bce60bbcf41af854f3f673ff24743"],["/posts/2482902029.html","ded102a78ce25edaad1582890cfa28e7"],["/posts/2495386210.html","3e8b96a4d3b68b9a76d9383e4c8aa79c"],["/posts/2516528882.html","15112d8af7ca8d71aaf6832d20456e64"],["/posts/2526659543.html","b7e60e4d40f354b0f9e1104a33bd5de5"],["/posts/2529807823.html","baeb1e71290a788440d64618e58dc120"],["/posts/2596601004.html","3f0df13af9118e5a5c4cb4b781444a33"],["/posts/2742438348.html","6301e4b960d07b9b364aa8ca6307ac9b"],["/posts/2888309600.html","48cd1bc5118995a038ff2348a606b1db"],["/posts/2891591958.html","0d42fb059efb8a6b993f01ded671d754"],["/posts/2909934084.html","52439e0139bf189245695bf395790f87"],["/posts/2920256992.html","c99da50bc776fc4fc9a77cc87580b2cb"],["/posts/3005926051.html","917a1498fff96aae290e1af00d78b1f2"],["/posts/309775400.html","f364b017c4809d934605735006af72f4"],["/posts/3156194925.html","dedb6fd102714e1f6661aa8dba5c627f"],["/posts/3169224211.html","24cd1860a339a24470e191181470748d"],["/posts/3213899550.html","dc516729a27eb66f1d8f873968c99b21"],["/posts/3259212833.html","85a74778cac1277a157a07d7c6c25520"],["/posts/3266130344.html","fce6d4d08a7ef49eb547f021b10a488b"],["/posts/3292663995.html","15af2bc3204362dc50bf3ed3f98c7422"],["/posts/3297135020.html","177d2ca00410d8bdd405f01bf381e9ee"],["/posts/3306641566.html","ddd3a67fdc9b24610a49b75c6b594931"],["/posts/3312011324.html","536fcee8e973b4dfb0596a2713f172b1"],["/posts/336911618.html","f76940976f6e9c96266074483faca7b1"],["/posts/3402121571.html","072af5f0cf242ff2f9c1152a86baed74"],["/posts/3405577485.html","bba85ff3795c3c3249d3521299971393"],["/posts/3498516849.html","ed52883de65ecda0ecfff6740451d553"],["/posts/3513711414.html","be212b10779a08689ff518552e2b9a71"],["/posts/3546711884.html","9e5741c6f65d88e4ae41a01c6afca5b2"],["/posts/3731385230.html","a48eacf1d9c7343ff7ac83a9f97e7c80"],["/posts/3772089482.html","b98629650eb747db84197b39362b3bca"],["/posts/386609427.html","ff4a1e4c2fc0e5c40c18d3723d33b5a7"],["/posts/4044235327.html","78980cad06c490fa9b57ee877bd3fba2"],["/posts/4115971639.html","eb6c4d55c552706d0c097d60fd139295"],["/posts/4130790367.html","f16b0510aaa32f5345eb1fed7708ded8"],["/posts/4131986683.html","8372e8d98689fe1ac839db8bb78ba08b"],["/posts/4177218757.html","5cc2f2d3b8b843df854a517f343dfa2f"],["/posts/4192183953.html","54b546a69f74f9296c25a5f03b5af573"],["/posts/4261103898.html","55d3646c4697b8e1baaa13cd9f21415a"],["/posts/469711973.html","4ef6414b647d0d65820c442a21a26108"],["/posts/482495853.html","db87c38872783fadd144e14f5a235287"],["/posts/488247922.html","110a9d68de11d781a6da438aa5cf1d9d"],["/posts/517302816.html","88f8ab0878e4c44ac473e141c41456c5"],["/posts/570165348.html","ef800a7308dffb8ede416ed3887d4ede"],["/posts/595890772.html","dd1972b0883bbb7f70962f61b4fd2286"],["/posts/67485572.html","0d9796fd3fe16b40e3907aac6423bd87"],["/posts/694347442.html","2db3a7dd9c1100b98a1cc30023107b04"],["/posts/707384687.html","a51e33101eb012bedf8c288b5cd7f053"],["/posts/71180092.html","947bb0fd51348a4a8bb2d8aa154d1aa6"],["/posts/716459272.html","f7f8991ed896a7db2b9ccbe01e03473e"],["/posts/765481613.html","f44f548bec2957e8918c06d8167f44b1"],["/posts/778231993.html","341ae0591e843d6ad54239ca28cfdb21"],["/posts/795397410.html","4457b09b032b90694dae05cb1ca75241"],["/posts/820223701.html","b7529060f7d6cd7e3a9e875ba6b07397"],["/posts/830372185.html","4f93a9d10378655504edb6011cf71bc5"],["/posts/88294277.html","b24995da5c5bb8cd264ae2bc6223cdd6"],["/posts/939963535.html","50b3711f19106e049d84d23fd9825424"],["/posts/983786067.html","ad1f78ef98346bb67841a8b55f4dfacf"],["/sw-register.js","9c78e91cd69ec34d4ab8fa7d2e5624a6"],["/tags/C/index.html","d40a8d91e641cb70c2fcb6b5efb274d6"],["/tags/C/page/2/index.html","c65d185973a4f6816cb55f4835bdc835"],["/tags/C/page/3/index.html","5b39be019a4468ee0ec487a1b55a5811"],["/tags/ETL/index.html","2b874220237281004b0acaa7b1567c21"],["/tags/ElasticSearch/index.html","d2887fe555b7e8e2e8139c92ca178d05"],["/tags/GUI/index.html","c802c4390f6dd44ca8f6d1a7957ac4ab"],["/tags/HBase/index.html","dc6006058bdf277939ac9b9411540967"],["/tags/Hadoop/index.html","4bf111e31d8bf318131d84692c3710d7"],["/tags/Hadoop/page/2/index.html","9725f7940982fb8eb5ca37cd1b30b52d"],["/tags/Java/index.html","dfda4551ea30e4cfeedbf0d71c087337"],["/tags/Java后端/index.html","1e6767d03df6ba8df8ca1886140fe683"],["/tags/Java后端/page/2/index.html","385efccd395ae1ea7c086c79c97d82e5"],["/tags/Java基础/index.html","312b7ef648050763e896f3cdb66739e2"],["/tags/Java基础/page/2/index.html","014f6fa29cb2e9a53a0e85438349e1fc"],["/tags/Kettle/index.html","3eded7cb6d78acab93b11b8ff04ef2cd"],["/tags/Kibana/index.html","57d5b458ccf3a641d95e56b305c0354e"],["/tags/Linux/index.html","cb00d642bb928ea6618b0ac4504bcf6e"],["/tags/Linux/page/2/index.html","bdfd56df07cf55f73ee374f7f8c4db46"],["/tags/Linux/page/3/index.html","9788fe3fee8a29bae9afd477ef5b3757"],["/tags/Mac/index.html","99a437ae3c7c446c5662d124ec2ae730"],["/tags/Mac/page/2/index.html","baed7d22cc52f5aa4691f37bb0dda091"],["/tags/Maven/index.html","ddefd99e21d104df2e400832a10a49e7"],["/tags/MySQL/index.html","8f2e8f5304acca8e8f7e9067847370b3"],["/tags/Python/index.html","d93befe69b1a0ec8f54e2a3f43c6ab72"],["/tags/Redis/index.html","53898579ab350ae9d3e33ed02de13be1"],["/tags/R语言/index.html","8430f2f373eaa5e90d5746cb2f9b5a09"],["/tags/Spark/index.html","9ec860e77ef0954fbbb695e1e067680e"],["/tags/Ubuntu/index.html","7cae02cb270dab84ffaf443fc15095d8"],["/tags/Vue/index.html","6e70078c785cbedb7b1347ac33dc90ad"],["/tags/Windows/index.html","a6bf5451db805f03d872513b11aca6e4"],["/tags/ZooKeeper/index.html","e1511ec1c72e54ef215d4cd114daa198"],["/tags/bfs/index.html","1c5264df0a3dcdfab9252a14b42a68a8"],["/tags/dfs/index.html","c7a227991937bf355e2880e4e1996119"],["/tags/folium/index.html","4c5bffd5dac470a2c4e4b3ac62f04103"],["/tags/git/index.html","beee2bdf7662ef1c7d558352d17fc4b2"],["/tags/index.html","df86cc4db338972ca271757d16966d1a"],["/tags/latex/index.html","75f746603e7664bb316a8be036dd8f6e"],["/tags/中间件/index.html","e848c24f8bba76e5b477d3e41963cf47"],["/tags/二分查找/index.html","3a4bba09c7e244e4df105bc774b20524"],["/tags/优化类/index.html","312df30854507886a9981f1571b3e6ec"],["/tags/前端/index.html","ff0455d6d1a9d71eecf5f03ddf620b4f"],["/tags/前缀和与差分/index.html","0dcf5d6637ff74f2ee1d8ac8ad47ba1e"],["/tags/动态规划/index.html","9915de78989872810822f16969a7a636"],["/tags/动态规划/page/2/index.html","b4919f9ca872ec1534d64bd4b5aa1c43"],["/tags/博客搭建/index.html","9437c7eeddaf6969b32b647eb9a2adc2"],["/tags/图论/index.html","936183afe8e0ad4c60a44f39395e9d6f"],["/tags/大数据/index.html","033340f31a6fbde7fb1912540e0706d6"],["/tags/大数据/page/2/index.html","bcc1ccd330cd98b5d40612c9b0ba4835"],["/tags/操作系统/index.html","9baf3df2526bdb46323d65eb84388781"],["/tags/数学建模/index.html","6fcab1db85fe2002e67c6f555f187413"],["/tags/数据库/index.html","d92458082e7b7b8e257acacc61ddb378"],["/tags/数据结构和算法/index.html","1355c1cb1ce89534bde26befe0958397"],["/tags/数据结构和算法/page/2/index.html","0611c22be56d1cebc3c0e04e1ea58a0b"],["/tags/数据结构和算法/page/3/index.html","48dcbd1c7d9cf08ac147b5010bf2cdfd"],["/tags/数组和字符串/index.html","2cc05696656d765238f43bce728d79f5"],["/tags/枚举类/index.html","93af3acc1b50817b05f62f2c0d883e19"],["/tags/栈和队列/index.html","c98a37375409b345c242e9d063419e42"],["/tags/树论/index.html","7885b83a60af218a68f84be54515acc2"],["/tags/测试/index.html","5472e3284ac4e647a8403436a3757ee9"],["/tags/环境/index.html","669efc1321eb4ddc4ed7d73a349d3734"],["/tags/环境变量/index.html","badcb7f9ad992742b2b198efac09c90e"],["/tags/绘图/index.html","c5fb0069f557eb2888ebf6e39f75c691"],["/tags/编程工具/index.html","069f379ce174762be14e8053bf57c7ea"],["/tags/编程环境/index.html","90777988096608136e820f58bb65b22a"],["/tags/网络编程/index.html","5d4d598c4148031995021615b88fbaaa"],["/tags/英语语法/index.html","d5ea7c2f7d2153e22f47409558de8199"],["/tags/论文/index.html","e096c707afd429290ba0e00efb962d1e"],["/tags/资源下载/index.html","eca9b4706232b48a5bb44f514c019705"],["/tags/链表/index.html","02948e79f436255044a8a282e1500449"],["/tags/集合/index.html","d2a417cd601faca4b8674227db600ac5"],["/tags/集群/index.html","fe81a1700226277b6bc0e797e52f0b89"]];
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
