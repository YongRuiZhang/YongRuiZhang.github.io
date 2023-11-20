/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","02e688cd36521a029c86576af3beb438"],["/about/index.html","598f4d0b32d2a51f129ed3109683dc49"],["/archives/2023/01/index.html","d4b6772c52bdd0c41294a3873e23ead3"],["/archives/2023/02/index.html","e44797bb04cbf3d2535f61f82e1be262"],["/archives/2023/02/page/2/index.html","2d1482d27c756fe9e294450d2bc89437"],["/archives/2023/03/index.html","c42c2ba93b304b989f8d70bdfbfc3ce3"],["/archives/2023/05/index.html","d494c7cc8ff8b4bdaad50a0bc73f5bd5"],["/archives/2023/06/index.html","cf72d95945e05f8428dd2866f466a1ac"],["/archives/2023/09/index.html","d3c3800644858ff884a9a468d4080d9b"],["/archives/2023/11/index.html","9be62978d2bbd75dd5847f9db1dc73d5"],["/archives/2023/index.html","a728b5c4ca2ec522e36e7e0a18936ebc"],["/archives/2023/page/2/index.html","b36bfd6414f0bc5d55a76c20fbaccbce"],["/archives/2023/page/3/index.html","ad3d18df03e2fd55e27f2c7dd282682a"],["/archives/2023/page/4/index.html","3feea51ee49410d53feea78aef07cb2d"],["/archives/index.html","6d45b87c05ee53f2714a67a2a37bd930"],["/archives/page/2/index.html","b943cbb6a74e3682307f05d12d38317b"],["/archives/page/3/index.html","dad4b811ea202ae742d6d16f8586bad1"],["/archives/page/4/index.html","71f0c6bb494b71e04cfbb355a05b1030"],["/baidu_verify_codeva-qQP2iZOMLX.html","2941812b60ccf059963e043d6dcb2697"],["/categories/Java/index.html","f920918772858a5f99d87b61deba4e4c"],["/categories/Java/后端/index.html","7a02b5806d33883aa93e301bbe389df6"],["/categories/Java/基础/index.html","2182cd1eed8348560c42867cc609f224"],["/categories/Java/基础/集合/index.html","7c6a69371fcde98f88a05750d05d9cc6"],["/categories/Python/index.html","460d89c65efcd68d353073ea6b8d5c69"],["/categories/Python/编程环境/index.html","3ee1cef040d19de9cc96a58e6e1a7e29"],["/categories/R语言/index.html","06d346ca104a8b25745cd6e08b609080"],["/categories/R语言/编程环境/index.html","90e01398a5d916384fb492e598dad793"],["/categories/index.html","c8fa615827316a4c3fe89f94bf1a8610"],["/categories/中间件/index.html","2615eb760a55a53a7e3bfcf98fa3bb65"],["/categories/前端/Vue/index.html","b8722eafa39ee7b07f823a4488b05361"],["/categories/前端/index.html","6726453b9190e8c946591a3bb2033f79"],["/categories/大数据开发/ElasticSearch/index.html","60686cd23dcb67e303db61bd58aeea78"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","9c390935d128861ef20171799df1ba42"],["/categories/大数据开发/HBase/index.html","dbd584d007947419613c46d50565f90f"],["/categories/大数据开发/HBase/学习笔记/index.html","d30dd0061e6cfa6c1a73ae35c1ca321a"],["/categories/大数据开发/HBase/环境搭建/index.html","724d89dab1874d3fa1b557560e24a39c"],["/categories/大数据开发/Hadoop/index.html","f04cfb39ee395c102d45543e19d2e2b7"],["/categories/大数据开发/Hadoop/技术/index.html","c934f2a92cb9be18517755c742e6aa96"],["/categories/大数据开发/Hadoop/环境搭建/index.html","29bfbf36b6dd415aa39e93599f407373"],["/categories/大数据开发/Redis/index.html","b787e9fcec8323cbe5a559c327b56e2e"],["/categories/大数据开发/Redis/技术/index.html","c03117d81a427ce68ab7b7ba69d9f056"],["/categories/大数据开发/Redis/环境搭建/index.html","cf5adf56256500f95ccee08ba6018b72"],["/categories/大数据开发/Spark/index.html","350a3244343514d093f9b8f21ec6167f"],["/categories/大数据开发/Spark/环境搭建/index.html","e1d77ec54f526222f7712473fa5088fe"],["/categories/大数据开发/Zookeeper/index.html","25ec60a6906ace25c03cff0ef638977b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","65e1a9d081a2a3cf22e39f230b94c812"],["/categories/大数据开发/index.html","c285c8ecd5d4fd19dfa51613aefe90a2"],["/categories/操作系统/Linux/index.html","6f6a5d2abb81332edc1702d9393e4fc3"],["/categories/操作系统/Mac/index.html","ffadd8e4b73e2344f468d16a4be5fc74"],["/categories/操作系统/Windows/index.html","9081eb8681a5c5f5e6c65dd044f34e88"],["/categories/操作系统/index.html","4d98deb3649ab63f109cc2e9c302a5f5"],["/categories/数学建模/index.html","2d20ec2c0f96eca18cc3b7b78ad98cfa"],["/categories/数学建模/latex/index.html","88df5038194c1177ad146efef62b2af5"],["/categories/数学建模/优化类/index.html","d9a13d708ce29a45374f6f6e38a93437"],["/categories/数学建模/优化类/现代优化算法/index.html","47a224add6d415ec9724e661628fd537"],["/categories/数学建模/优化类/规划类/index.html","38917c2f73330b1c859fcc55f9d50275"],["/categories/数学建模/绘图/index.html","45c7ac472d94817e846b4c846210d8f9"],["/categories/数据库/MySQL/index.html","f024fc3032b07796fbe0e0e6d32a2287"],["/categories/数据库/index.html","6036c3c475233b9e1d58921a27cb5dba"],["/categories/数据结构和算法/index.html","6a223dd1fa91942a90256413ab737c48"],["/categories/数据结构和算法/page/2/index.html","6ab56d623755a8a883231365ab951833"],["/categories/数据结构和算法/基本原理/bfs/index.html","60b2c469f1ca74ea4f77123b100f19dc"],["/categories/数据结构和算法/基本原理/dfs/index.html","08dc6b2be4944587f9e2ed97e4c64894"],["/categories/数据结构和算法/基本原理/index.html","657df6e631f29eb6dcc7472c65773a80"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9167502af12212c842506b94fdbe40a2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d25378cf76e8f9a1311973698f2f56df"],["/categories/数据结构和算法/基本原理/图论/index.html","03d061681dc5b4c84254e55305955a4c"],["/categories/数据结构和算法/基本原理/字符串/index.html","6fd67566474b8f77b2cb49d87797969c"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","651de912e00d1d3496646b019464121c"],["/categories/数据结构和算法/基本原理/数论/index.html","7c5dbc62d9800267997e77755b8a7de0"],["/categories/数据结构和算法/基本原理/树论/index.html","46929c0a8cc0a346d4c0cb4ef22fd6c4"],["/categories/数据结构和算法/基本原理/链表/index.html","70a31d7d20c0be3b6a5b4846f09777e7"],["/categories/数据结构和算法/算法题/index.html","85c86feeb77ce98a72755279b8b6b4d5"],["/categories/数据结构和算法/算法题/二分查找/index.html","8ae52f9743bb5c7c27fc553aa2f52702"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","10eb154ed454fa9f4304ccb955483e1f"],["/categories/数据结构和算法/算法题/动态规划/index.html","4ed57322de7eceb84b311a9f2f0e8453"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","b4db7cb723a29ef35b4497d63da1d392"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","dc3566112d8960d82a2c735d8c738771"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9a8547a39a201bda0e154a592b117e22"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","36fff230d7481ddb6d058d85cdbaf3f3"],["/categories/数据结构和算法/算法题/栈和队列/index.html","dbddc44e9cc65138b5c5eabe6ae332cf"],["/categories/数据结构和算法/算法题/树论/index.html","bda124c6c5e630436512e5bf0b43e3ef"],["/categories/杂七杂八/index.html","aba35c1aecf35d10b46c1cec65f53eff"],["/categories/杂七杂八/博客搭建/index.html","78fee7ecbb38b22143d6690a20e0fa34"],["/categories/编程工具下载/index.html","0d4efa9b2df297c12b14fa1ef677e9cb"],["/categories/编程环境/index.html","66089d4a696ef2b77bc234f630ad50ff"],["/categories/编程环境/大数据/index.html","9ee93d8556af6e0a116d8bd38fcb5d73"],["/categories/英语学习/index.html","1408f1be97d4669f560e21934e2f71e2"],["/categories/英语学习/英语语法/index.html","e37bdd7606a716f96c956bb3b1739e53"],["/comments/index.html","d15dd49bf1d1cbf97cf8a943ab007b36"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c300ff67d3505653c213634f760892ab"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","f787424576ea8b1b037529e6d4058941"],["/movies/index.html","f13b7c8616c249837750008a1c352e9c"],["/music/index.html","29a3bbc8309bc7109b789ed751ae068c"],["/page/2/index.html","7a718b6ab8a5c5de1f928b21f1b16823"],["/page/3/index.html","3fc9a3e8f0a4b13c84b0aa96a823e0f2"],["/page/4/index.html","4c80b3090d097bb9a2ea5e07efcc64b6"],["/page/5/index.html","8bc16875f158481d174258c34d392779"],["/page/6/index.html","aedb2b9a4112a5932178ee912af030f7"],["/posts/1021360842.html","b38cd57d6db513503276ac34d804a3b6"],["/posts/1120620192.html","d56e8d98c46472f5c4a82c455e5461b5"],["/posts/1141628095.html","4a29d36f76882fbd06d6ffd5084da96c"],["/posts/1168613674.html","99cc08a02bfa008ad4390e05f61de391"],["/posts/1219920510.html","db45efbe630560c648b536437b56609d"],["/posts/1222166338.html","03d00df1d1f91b3207e6e634d0b0257b"],["/posts/1259097482.html","99518320b6a0e1d5445c7b8a4120d5ce"],["/posts/1271036369.html","d04b306d396079dd1e02532862c5e8c6"],["/posts/1312847445.html","8b2c4f7e75db3c1630506ac03fa49ff7"],["/posts/135355774.html","18ddb3f9068d2e8b25cef70359db4b2d"],["/posts/1375344716.html","3d36b7662f7af9958e3afd39729d6d6f"],["/posts/1388991698.html","87700c6116818933d5903bacf7019e79"],["/posts/1410315814.html","2487596d22c02eb3732409bffef21911"],["/posts/1452790229.html","fdafed824c86095b20a13a54b795e033"],["/posts/1470079884.html","1ae28ace25978a9c402d0ed728476c2a"],["/posts/1470079885.html","994afce80a8d8b4e6dbf4500bca1ceb4"],["/posts/1470079886.html","4b9d5ee69072cca3907b8e79fcdf0750"],["/posts/1470079887.html","7186562193a336d536bd24cf916f86df"],["/posts/1498536549.html","efa9e80588c90288139eb1bbcf898d64"],["/posts/1547067935.html","e03e1b69c0d025b503ce43356d258364"],["/posts/1557866301.html","ee3ae749c2e9fd7257449d748da374f7"],["/posts/1571776361.html","82268dccf9493e9771eaf23644ad8327"],["/posts/1605124548.html","11c85452a0f24c31f4604f8ed77a3e85"],["/posts/1633036852.html","5e1ec324c0fcede54f2be6a15e846438"],["/posts/1674202625.html","c0da69bc6bb9d1cf18c2ad32db10b068"],["/posts/1765123828.html","6fdc1bdff069a0e1cea4b7ec7af77661"],["/posts/1767336200.html","b05d76bdedc6d25e9eb839a127522180"],["/posts/1776114197.html","30e417fe51c54e4ee8608577f2394de3"],["/posts/1817748743.html","483210dd71ef6d5ae63033946a64ed0a"],["/posts/1925125395.html","388daecca348575b38244d2db8524206"],["/posts/1966191251.html","498bb01150e293c944a4477eae8cbc78"],["/posts/1987617322.html","85a922c22c1d28b7dce32c2f08b4f610"],["/posts/1999788039.html","1337264eee0519ddedd3cfdcd5d4ad35"],["/posts/2075104059.html","f12c4ea163e8c72a931ff2c52657ef22"],["/posts/2087796737.html","94cfb9a0541cf4f27d061a5594210f2d"],["/posts/2106547339.html","4a3642db743ca76c562640a742d75b3c"],["/posts/2207806286.html","000e4b553e56b89c6207f49191a8be07"],["/posts/2225903441.html","b0dd28fb861d3a76c3bb0e53f57e1b74"],["/posts/2265610284.html","8cfaada833b6466d4228d2d3107101e7"],["/posts/2281352001.html","7c0e238338a588575e6fcad7c239ddef"],["/posts/2364755265.html","e2ec3babe719e91e56fc4dc490e0eb35"],["/posts/2414116852.html","5488c31820c47cf950648ccfa9c04eda"],["/posts/2421785022.html","010bce60bbcf41af854f3f673ff24743"],["/posts/2482902029.html","ded102a78ce25edaad1582890cfa28e7"],["/posts/2495386210.html","3e8b96a4d3b68b9a76d9383e4c8aa79c"],["/posts/2516528882.html","15112d8af7ca8d71aaf6832d20456e64"],["/posts/2526659543.html","b7e60e4d40f354b0f9e1104a33bd5de5"],["/posts/2529807823.html","baeb1e71290a788440d64618e58dc120"],["/posts/2596601004.html","3f0df13af9118e5a5c4cb4b781444a33"],["/posts/2742438348.html","6301e4b960d07b9b364aa8ca6307ac9b"],["/posts/2888309600.html","48cd1bc5118995a038ff2348a606b1db"],["/posts/2891591958.html","0d42fb059efb8a6b993f01ded671d754"],["/posts/2909934084.html","52439e0139bf189245695bf395790f87"],["/posts/2920256992.html","c99da50bc776fc4fc9a77cc87580b2cb"],["/posts/3005926051.html","917a1498fff96aae290e1af00d78b1f2"],["/posts/309775400.html","f364b017c4809d934605735006af72f4"],["/posts/3156194925.html","dedb6fd102714e1f6661aa8dba5c627f"],["/posts/3169224211.html","24cd1860a339a24470e191181470748d"],["/posts/3213899550.html","dc516729a27eb66f1d8f873968c99b21"],["/posts/3259212833.html","85a74778cac1277a157a07d7c6c25520"],["/posts/3266130344.html","fce6d4d08a7ef49eb547f021b10a488b"],["/posts/3292663995.html","15af2bc3204362dc50bf3ed3f98c7422"],["/posts/3297135020.html","177d2ca00410d8bdd405f01bf381e9ee"],["/posts/3306641566.html","ddd3a67fdc9b24610a49b75c6b594931"],["/posts/3312011324.html","536fcee8e973b4dfb0596a2713f172b1"],["/posts/336911618.html","f76940976f6e9c96266074483faca7b1"],["/posts/3402121571.html","072af5f0cf242ff2f9c1152a86baed74"],["/posts/3405577485.html","bba85ff3795c3c3249d3521299971393"],["/posts/3498516849.html","ed52883de65ecda0ecfff6740451d553"],["/posts/3513711414.html","be212b10779a08689ff518552e2b9a71"],["/posts/3546711884.html","9e5741c6f65d88e4ae41a01c6afca5b2"],["/posts/3731385230.html","a48eacf1d9c7343ff7ac83a9f97e7c80"],["/posts/3772089482.html","b98629650eb747db84197b39362b3bca"],["/posts/386609427.html","ff4a1e4c2fc0e5c40c18d3723d33b5a7"],["/posts/4044235327.html","78980cad06c490fa9b57ee877bd3fba2"],["/posts/4115971639.html","eb6c4d55c552706d0c097d60fd139295"],["/posts/4130790367.html","f16b0510aaa32f5345eb1fed7708ded8"],["/posts/4131986683.html","8372e8d98689fe1ac839db8bb78ba08b"],["/posts/4177218757.html","5cc2f2d3b8b843df854a517f343dfa2f"],["/posts/4192183953.html","54b546a69f74f9296c25a5f03b5af573"],["/posts/4261103898.html","55d3646c4697b8e1baaa13cd9f21415a"],["/posts/469711973.html","4ef6414b647d0d65820c442a21a26108"],["/posts/482495853.html","db87c38872783fadd144e14f5a235287"],["/posts/488247922.html","110a9d68de11d781a6da438aa5cf1d9d"],["/posts/517302816.html","88f8ab0878e4c44ac473e141c41456c5"],["/posts/570165348.html","ef800a7308dffb8ede416ed3887d4ede"],["/posts/595890772.html","dd1972b0883bbb7f70962f61b4fd2286"],["/posts/67485572.html","0d9796fd3fe16b40e3907aac6423bd87"],["/posts/694347442.html","2db3a7dd9c1100b98a1cc30023107b04"],["/posts/707384687.html","a51e33101eb012bedf8c288b5cd7f053"],["/posts/71180092.html","947bb0fd51348a4a8bb2d8aa154d1aa6"],["/posts/716459272.html","f7f8991ed896a7db2b9ccbe01e03473e"],["/posts/765481613.html","f44f548bec2957e8918c06d8167f44b1"],["/posts/778231993.html","341ae0591e843d6ad54239ca28cfdb21"],["/posts/795397410.html","4457b09b032b90694dae05cb1ca75241"],["/posts/820223701.html","b7529060f7d6cd7e3a9e875ba6b07397"],["/posts/830372185.html","4f93a9d10378655504edb6011cf71bc5"],["/posts/88294277.html","b24995da5c5bb8cd264ae2bc6223cdd6"],["/posts/939963535.html","50b3711f19106e049d84d23fd9825424"],["/posts/983786067.html","ad1f78ef98346bb67841a8b55f4dfacf"],["/sw-register.js","1076b1cbbeb63ae0b1ec7a0e41cb064d"],["/tags/C/index.html","34191475b5a45cdcd1fa315f9cc0deb9"],["/tags/C/page/2/index.html","f1b66d5a3ad8af80289462f6cf427ca6"],["/tags/C/page/3/index.html","7e90b317e869958d92a7fb93e32ca2a1"],["/tags/ETL/index.html","a7d1052896e5d3c1c12d2e8398e41e46"],["/tags/ElasticSearch/index.html","09e9eab6ec2d7d54badc0342fb7ffd04"],["/tags/GUI/index.html","8c1717adad43497bd61ce785d014ff3d"],["/tags/HBase/index.html","6331dee8ba339b753e5b033776a3a603"],["/tags/Hadoop/index.html","6d7138a01f686b84e99242f0eaa4a416"],["/tags/Hadoop/page/2/index.html","dfd84cbec12512654630a228f1781fa6"],["/tags/Java/index.html","d52c8c93b4e83291df3fcec9a8223d48"],["/tags/Java后端/index.html","70ca1f7cebbb8428fa15320513e5c48d"],["/tags/Java后端/page/2/index.html","90d7b107189282d48e8665a6705c0e1b"],["/tags/Java基础/index.html","c8ce19c2880da06de36135469d569f34"],["/tags/Java基础/page/2/index.html","8b8c8a75a8bbc2608c17ee7e0764175a"],["/tags/Kettle/index.html","85402a88a801977023074a56ade758e7"],["/tags/Kibana/index.html","0d0b86cb7dc6fb0a197a36e2510e9a6e"],["/tags/Linux/index.html","7994e9675a01c7d19114a5a01d8b7029"],["/tags/Linux/page/2/index.html","1c6e6dcbda77c62ca959c5731d5db299"],["/tags/Linux/page/3/index.html","dacc8878240761fe91f38ebb0a01b7e4"],["/tags/Mac/index.html","6d51cc2f0f6212c9a2cb8d56ef5c18ab"],["/tags/Mac/page/2/index.html","dd5f35a702ec9835307b78d28909605f"],["/tags/Maven/index.html","0326d0a998ff83b62bad7fa215113a64"],["/tags/MySQL/index.html","8cc7ba5019d6d188276bc82a31952057"],["/tags/Python/index.html","07c59ca0ecd536c799b4398b10ebc6f3"],["/tags/Redis/index.html","2547f7650056c09b9f4715310800e0e6"],["/tags/R语言/index.html","bbb33fc435e703cab28c404de0461452"],["/tags/Spark/index.html","2be367778ef2a4c75a98871ca038c822"],["/tags/Ubuntu/index.html","0c4ae34c3f94aad847666ef6d33e7d3b"],["/tags/Vue/index.html","bfdf3114c39527b158d3871537d7b6ce"],["/tags/Windows/index.html","1d911b7409d6121bc066c08de77f92d2"],["/tags/ZooKeeper/index.html","72664e04a2923a1b9570d051281fc79c"],["/tags/bfs/index.html","db6f7dbcb5670fda106f316e9a9ac24d"],["/tags/dfs/index.html","ab9bd55be38a0e122a9f08dbc06bb70d"],["/tags/folium/index.html","d14a5f592b7e64838e95f315f330a566"],["/tags/git/index.html","7dc2a42c0762101b2dc692087621e13d"],["/tags/index.html","0d2d59b8bfc4b0e29ec3958d6ed71a43"],["/tags/latex/index.html","1d39add078861faf15d39c85b5242a1c"],["/tags/中间件/index.html","42d78aa7f2990224d68348e104104b5a"],["/tags/二分查找/index.html","e47ccf29c65029c524359934ed7a38df"],["/tags/优化类/index.html","ac1a3e662f1fc3315873e1cd14bb5a78"],["/tags/前端/index.html","2d367b7721b65a2d77cee5eaa9fab1e5"],["/tags/前缀和与差分/index.html","e7de262a38bac7fbc42cc98001dba0a1"],["/tags/动态规划/index.html","551b2dd1f01039cdb271501be2706a1e"],["/tags/动态规划/page/2/index.html","f7186ac6f352e1b3dd5959a35eac9e01"],["/tags/博客搭建/index.html","a78d97601e448ebfc5f30f02fe3420a8"],["/tags/图论/index.html","e64ec2f7b5b774e0d9afb66a5fb3aa9d"],["/tags/大数据/index.html","466e4a506b6ff8102b67f7a42e0acee8"],["/tags/大数据/page/2/index.html","3edb7b52e75a1ad810159aabc62e3447"],["/tags/操作系统/index.html","a9a8de238812074440f1f4192ca3a8e2"],["/tags/数学建模/index.html","00aeb5c9a9bd7541ad37a87b0d4c1944"],["/tags/数据库/index.html","fe698aea981ce8e51a8af818991db973"],["/tags/数据结构和算法/index.html","a1511567bf09dc9eef55abba6bddbd47"],["/tags/数据结构和算法/page/2/index.html","0a4256e720de18d49ba09d865ac806f4"],["/tags/数据结构和算法/page/3/index.html","97bf902a3441002b1f9ab80eae6bfb2b"],["/tags/数组和字符串/index.html","f38472f9067929b126f0c271dc34958c"],["/tags/枚举类/index.html","05269935be5d8f7478911a778989a601"],["/tags/栈和队列/index.html","f4057bfa4727aaddf8c415c4317159ea"],["/tags/树论/index.html","d20ecdfebbe3dcff5984d7851f1cf3d9"],["/tags/测试/index.html","39c150541b0aec2e362e93764392bf7c"],["/tags/环境/index.html","e5306fb91fc2fea14a321644af4b0aa9"],["/tags/环境变量/index.html","fbc67c7ce7e8ac274095b626b5caf30e"],["/tags/绘图/index.html","4fe9e861bacb8e3df59731f78906814a"],["/tags/编程工具/index.html","78c45571a7fd371209715f71ea826ad7"],["/tags/编程环境/index.html","d27a4c70ed9a333ac13d2f00c50242e6"],["/tags/网络编程/index.html","921b78c016c97cf5bebfa2a53dd5e94b"],["/tags/英语语法/index.html","f07cfc322972793d0e5770f39641b2d0"],["/tags/论文/index.html","8c97404c75509b3c7dd1bec4a66926e6"],["/tags/资源下载/index.html","7649cc1c179e5c6746a486e42ec55fef"],["/tags/链表/index.html","17c1c67713da211059aee9bd7dede987"],["/tags/集合/index.html","550438175291a96f6b0705dca164861e"],["/tags/集群/index.html","d8a7b09abf0f1e9547a7891c1a10822c"]];
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
