/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","969cbb7f6ca304b7a4f885010c19fe4e"],["/about/index.html","4d0961f78e23954f014146087058a978"],["/archives/2023/01/index.html","93d7f0c54c205b570916a3ecd1a8d2d8"],["/archives/2023/02/index.html","6fb85498f9f2b9318c6f9e2a3653d6f5"],["/archives/2023/02/page/2/index.html","ef8f241d3262a9e4c2cb6e1c0f4dae91"],["/archives/2023/03/index.html","35111b2aee42d88061a4b199ddbedc5d"],["/archives/2023/05/index.html","a9933e2761e240441f6a9cbf3ec7f8c7"],["/archives/2023/06/index.html","4648b7d8c0bdadaa82ffcf4633d07d31"],["/archives/2023/09/index.html","af4ce77036e5ea9b35c6868d89103695"],["/archives/2023/11/index.html","5e8cfe10fca7d9f01be87396ea5804e1"],["/archives/2023/12/index.html","6f63414b8b569487ab99e6050ded4939"],["/archives/2023/index.html","c3440427e940ef3db776408c1b8507db"],["/archives/2023/page/2/index.html","9cf3d474b41197a25595fb08536a7229"],["/archives/2023/page/3/index.html","4570e83e5ba8994c6bb0dfdd63b8603d"],["/archives/2023/page/4/index.html","ad189102f577391fa80cd89420d34ada"],["/archives/2024/02/index.html","93457229e0e7444588e92d710e7d523d"],["/archives/2024/index.html","f727dcab34e00f3fa9cc095fe3fc0843"],["/archives/index.html","ed607770fad57d80208ef6386e45d616"],["/archives/page/2/index.html","6af43515e13595029aef67509379daac"],["/archives/page/3/index.html","fc051929b97939cf3c87902bd923e4ee"],["/archives/page/4/index.html","e877b626c591a95aec16f7d3c4548b84"],["/baidu_verify_codeva-qQP2iZOMLX.html","07b9908e3fd06f8185760e3eab2fdc64"],["/categories/Java/index.html","e9cd87718e40a635003d0923f8fad8a1"],["/categories/Java/后端/index.html","66d00e9e44178d361eb57313ecc95862"],["/categories/Java/基础/index.html","07bb5f7eeec51545737cf21741dfab2d"],["/categories/Java/基础/集合/index.html","75c3c7858bce2b179a7b0a532926dcb5"],["/categories/Python/index.html","97bcf36b9aa3e9ff7956b98277e12bec"],["/categories/Python/编程环境/index.html","c9bc8688c4c856410f6b5506cd53f1f4"],["/categories/R语言/index.html","a5e5377e187744d54b7f2f9c62ab145f"],["/categories/R语言/编程环境/index.html","65d30d2e30f38a1e56911dd16500bde8"],["/categories/index.html","cbafdbeb8e37a6b9cbe1d745bd228196"],["/categories/中间件/index.html","9ba4a8b2556d1722e267ab090f81ed83"],["/categories/前端/Vue/index.html","39d7abf10aadd68b79097c3e568bc33d"],["/categories/前端/index.html","9d44c434c3b4fea349e428223decfbe2"],["/categories/大数据开发/ElasticSearch/index.html","a52f325d4571194f88c31a8b54208c54"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d3c6fc02e32a17212f9503d02f9d109a"],["/categories/大数据开发/HBase/index.html","8986dc7c19c912f73ffebb96c7113f1a"],["/categories/大数据开发/HBase/学习笔记/index.html","fbaf7c0b4bcc8f9b8c09148ffd3766cd"],["/categories/大数据开发/HBase/环境搭建/index.html","df943d6875f98c4eda0cbed9473096b3"],["/categories/大数据开发/Hadoop/index.html","4dc8a232a04d1e9b2daa6aad2ab08290"],["/categories/大数据开发/Hadoop/技术/index.html","6a17d6255129e92d2ea0c0caa63f9d18"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3494e1b711418355de17a2bf933f8ac9"],["/categories/大数据开发/Redis/index.html","0a1960f01fd7fb6d6195b1128ba6193e"],["/categories/大数据开发/Redis/技术/index.html","589648cee6b877d8c018c75e670599df"],["/categories/大数据开发/Redis/环境搭建/index.html","b76c2cf0830f9fa0959394b1d9b6a836"],["/categories/大数据开发/Spark/index.html","2e3b054475962a3e2175ba1a4d3c6f29"],["/categories/大数据开发/Spark/环境搭建/index.html","27d8566a3d52922012283519b1f1ee5b"],["/categories/大数据开发/Zookeeper/index.html","ed119e80670fcbf8c35420cfdb179a75"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","38b4c8b5f46b45c51eb37506f2e36c8c"],["/categories/大数据开发/index.html","76071d7f54a5de834ec3c0759c55934c"],["/categories/学校课程/index.html","a1cd97f1e6a4b751ab72722cdc5edb95"],["/categories/学校课程/计算机操作系统/index.html","277c779129dd66ed4b4bf4f064b5a24a"],["/categories/操作系统/Linux/index.html","f4a39109c37cc8ae84b5da761dcc8513"],["/categories/操作系统/Mac/index.html","7898fdb08fc6a78e6489b942bf46d713"],["/categories/操作系统/Windows/index.html","16e93c3af736c012b1220d11a28d82e5"],["/categories/操作系统/index.html","5f053fbc218326875668062eb37ae25f"],["/categories/数学建模/index.html","aacfac5928c7a063ae8c0a627510d40a"],["/categories/数学建模/latex/index.html","9beb000f06e74243092389806569317e"],["/categories/数学建模/优化类/index.html","d50d2ad4d3f5eda44d26e3e9fceaa4a8"],["/categories/数学建模/优化类/现代优化算法/index.html","47909d1a6abee25f43f6ab16614b38ef"],["/categories/数学建模/优化类/规划类/index.html","4676a1cde01454b9e8139a8a1a500378"],["/categories/数学建模/绘图/index.html","e7c7f844c2ad79df32b1af6da4ba4abc"],["/categories/数据库/MySQL/index.html","17ffbe844e92844b5aee07363bbf3d8c"],["/categories/数据库/index.html","e1b076ff9816c8d755fd2890dbfba989"],["/categories/数据结构和算法/index.html","8905f305dad8639147a454c4fdc2c100"],["/categories/数据结构和算法/page/2/index.html","de63ff154689885f67b5af937e9cbdbf"],["/categories/数据结构和算法/基本原理/bfs/index.html","8202d5638b71c5e347fa86a8d72a5028"],["/categories/数据结构和算法/基本原理/dfs/index.html","000a58d13fb8733ddf5e7a3663b3cad9"],["/categories/数据结构和算法/基本原理/index.html","fd7717417f647c02b6bedb356094bd31"],["/categories/数据结构和算法/基本原理/动态规划/index.html","335ed80dba2738bdb0e47cad288af755"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","15268517339690de7bffa85df0111138"],["/categories/数据结构和算法/基本原理/图论/index.html","f1469c7947d9e1b9956f2a8c70599a63"],["/categories/数据结构和算法/基本原理/字符串/index.html","6a1de1bf496f3dafd58d3748b6470291"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","4574f277eeb33b4487eaacdc1f06bba3"],["/categories/数据结构和算法/基本原理/数论/index.html","2c510b495e9c0ac0e78c82ebf396fcfc"],["/categories/数据结构和算法/基本原理/树论/index.html","1c10ca1dd5db77d2be316616cc6aa01e"],["/categories/数据结构和算法/基本原理/链表/index.html","341026627ed5275c5ed6bbf5b4923de5"],["/categories/数据结构和算法/算法题/index.html","9d2448a1a8e0eb43a8f5fafeb3be613a"],["/categories/数据结构和算法/算法题/二分查找/index.html","301c26b5356842cabb9cc75e3632bd19"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","93afd832a2fec2c0005d37ac56b30dc5"],["/categories/数据结构和算法/算法题/动态规划/index.html","27d8f62622dec62deb774c5ea95f47a1"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","38f3be7622cb1fa736c8f5922f0b0d16"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","67424c662d72304dee7d689f4d7badd9"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c520a23572b045de9b6f04d170d77737"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b697ace6fa9364a8f718df4976899940"],["/categories/数据结构和算法/算法题/数论/index.html","da0ceb455e40b560bf59ea6246ab9026"],["/categories/数据结构和算法/算法题/栈和队列/index.html","1302942571a963bb9ab35ccded71b14f"],["/categories/数据结构和算法/算法题/树论/index.html","3aaebeb14f591c08cbeba61c6e9d5482"],["/categories/杂七杂八/index.html","36a373cb8eebb6a279be68db94d52d94"],["/categories/杂七杂八/博客搭建/index.html","3db586bd19b66624372b971d55bd9ce3"],["/categories/编程工具下载/index.html","55ccefe3bcee014a5abdd9ed7fd7e315"],["/categories/编程环境/index.html","12baaab9fb9e3f4f347e1abdc19b4a3e"],["/categories/编程环境/大数据/index.html","455bca8d984432e41d8d71d07c07eca7"],["/categories/英语学习/index.html","48037795ae1af05aeeb0f21e30d57b5d"],["/categories/英语学习/英语语法/index.html","a11d2234b4ded66db97f1b68454b3435"],["/comments/index.html","f1957724f0a43d794bf24a1811e0542f"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","c96337d1b6ead12cd775d65d1feddeaa"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","8314fa5346085470edebac4f9f075137"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","95afb071a93b85dc16c82eab3555e146"],["/movies/index.html","b09bb22c3c533d3866a5c79a07fb6f62"],["/music/index.html","f9e51cf63c07067d9feb466be52b8597"],["/page/2/index.html","94701477977d5d0416724ddc34335a83"],["/page/3/index.html","1b4870ce57f6d418278ccaa394eb7518"],["/page/4/index.html","f02fb5110bded7bd03452d044c9ab55c"],["/page/5/index.html","fc359918417b65cfa5bd0011a98f2800"],["/page/6/index.html","50ad4f06c7bb613724cbd7dddd302f65"],["/posts/1021360842.html","82542f13b90e596351191c6000b1cb95"],["/posts/1120620192.html","fdaa00e63b452eb9989d369bc7a89e71"],["/posts/1141628095.html","8cb15530029596d96fa25e4ddd125834"],["/posts/1168613674.html","e85e2d381170050487956b8b7bb0a29a"],["/posts/1219920510.html","99a43d6996f860aa041f0aa7a2e102c7"],["/posts/1222166338.html","8008618ae10c48f21af1382074625205"],["/posts/1259097482.html","a763dc6e437c5d0e6e9f48eaff53cb3d"],["/posts/1271036369.html","008ceed30d86553e487299d2fe28729f"],["/posts/1312847445.html","c70ce64a3b14d7dfaad875397a62cbc7"],["/posts/135355774.html","86c1d0c7e1a204792ebf26b36e55ec5c"],["/posts/1375344716.html","de23285d6d3a73c5166e2698585100d6"],["/posts/1388991698.html","7aed96f95ab11290fa53f64e95c40451"],["/posts/1410315814.html","c6df727a4832c556a7351cd169289ff6"],["/posts/1452790229.html","41f2cf3b8d298be517c5b127fe560537"],["/posts/1470079884.html","c6f2b85045c78fa4649b24bb2f00143e"],["/posts/1470079885.html","52b853ddee9315b41b5ff953206ef7ba"],["/posts/1470079886.html","4106f57d4a958056cc5f1187f00b08bf"],["/posts/1470079887.html","ff8fa30b6c94fdac522d844a34a43871"],["/posts/1498536549.html","e4d45c09a24582ae3b1c4844f45be9fe"],["/posts/1539568593.html","adfd25301b189e3f44690f0552ea88e1"],["/posts/1547067935.html","08d531f661de8f27d5e6ec0decccda60"],["/posts/1557866301.html","3288094798c4dce826146c56d4847134"],["/posts/1571776361.html","fe1166c0c2559d0948d774f4bfd38c4e"],["/posts/1605124548.html","ce259c11d305ebc607f124d8951ffcee"],["/posts/1633036852.html","d30427b9b3fbca6bcc8daaab6c0e6104"],["/posts/1674202625.html","850cbe3a1d591ab74d3670d71b8546b2"],["/posts/1765123828.html","ca4c7ff577b182436f139bf42bc03868"],["/posts/1767336200.html","4b6e17fcc20577d0208526837fbc6cfe"],["/posts/1776114197.html","328c353521c55f0e7cbd3dd187602ff5"],["/posts/1817748743.html","bcca49d8ba6930fc975162a299990be9"],["/posts/1925125395.html","6a4a9aa3e9ff21a2e7b0c5b2980e8e5d"],["/posts/1966191251.html","3cea20468432dd77a2357e568680243b"],["/posts/1987617322.html","22df1a917fcba7591a0d4ad442c58ea2"],["/posts/1999788039.html","2d1a213a6a7afcc05eb82ec7a8ec46de"],["/posts/2075104059.html","6c3f7754989e99c1797d67bd8c5ac07f"],["/posts/2087796737.html","24e2a53ada8ac95dd2306e57c6fc980c"],["/posts/2106547339.html","23a62a39668b0bd32d324641729c7932"],["/posts/2207806286.html","3c8ac9bc6558e41d1852e6939e0c1dd4"],["/posts/2225903441.html","74380c28fac6ca0ec28bb1d25cf48839"],["/posts/2265610284.html","6f842bac6e715a98680ead0b54f0210e"],["/posts/2281352001.html","2fce6934f0ebabe32fb2f11d1482261a"],["/posts/2364755265.html","26f8b035fae9beeaadaef7a8e79c454a"],["/posts/2414116852.html","0c4b6ab708443ccd2b8d12353a82169c"],["/posts/2421785022.html","9986d989b18131ad33ad6f85d65d35f9"],["/posts/2482902029.html","617c3046b52d29eee2088cea9b98dff8"],["/posts/2495386210.html","095c877f9eaab680940f79d95bfd223f"],["/posts/2516528882.html","25d23d463aa7be012c4a15c5c5c03bff"],["/posts/2526659543.html","c656b85a4617627215503bcbf774da59"],["/posts/2529807823.html","addf68a2fab2b6f9784c2c3b1ad87943"],["/posts/2596601004.html","c9093b34adddf50402e73632d8979a13"],["/posts/2697614349.html","30c94ff2dd959094a113447ab11eb9d2"],["/posts/2742438348.html","eda535f63ea7570584b22debcc1104e7"],["/posts/2768249503.html","9fe22897b29f1670c47123773b346c92"],["/posts/2864584994.html","e51daac94fd028ea99f332fbb8e51f76"],["/posts/2888309600.html","4bb1eef701b9bae3bbd15177ea7ddbd9"],["/posts/2891591958.html","d45506fd5f30e4bc434c3cdddb47fb04"],["/posts/2909934084.html","79d856375be07843b840ece4abdd8188"],["/posts/2920256992.html","c1740c4e401df55c5cac5c2421bed94d"],["/posts/2959474469.html","a280fbb88549c0e2ded1b0a44df71cee"],["/posts/3005926051.html","fd03bc7f1485de16da70be40164fad4a"],["/posts/309775400.html","d275f35280bafca277d4544069a39040"],["/posts/3156194925.html","202d4d34ef8106f1b5ac2311c5cad0ec"],["/posts/3169224211.html","533edbec0a2ead2e1990a19bfa036a24"],["/posts/3213899550.html","23adefe3c611ebd3ae1f581f78f71bca"],["/posts/3259212833.html","91194567d88f44c5a86f5dff1b67aa71"],["/posts/3266130344.html","31436bf5808ac8dc0a69f8c22fa18368"],["/posts/3292663995.html","9ad7d17c16f07ce0bd272401eeecf52c"],["/posts/3297135020.html","d461d87b70075cfc7003299dbd0cd41b"],["/posts/3306641566.html","e453b5f8580e836f83c9162066bb3494"],["/posts/3312011324.html","5bd72aeefc209369fe269e69fe4ed97c"],["/posts/336911618.html","f2b69e34029669cf7037b2ee8f902e95"],["/posts/3402121571.html","9924b73ad2c74f59b7d352962bbb0a9f"],["/posts/3405577485.html","af851a39b02d6960e9f0603789436776"],["/posts/3498516849.html","01c32b2346ba408ca08a5a1f3561d47f"],["/posts/3513711414.html","3b1c8be18c12e3f2efc3acc121ed862a"],["/posts/3523095624.html","52e13e7f14e814d632ebd6047b28139f"],["/posts/3546711884.html","a450105737498d1301ee4980fa026205"],["/posts/3731385230.html","508e0f4d82170dc37b911cfb4e2fa1f7"],["/posts/3772089482.html","7afbab3d6f85e5ff3bdc682d1e5b52de"],["/posts/386609427.html","d65403453a11e196b3f9417aa0dcbdeb"],["/posts/4044235327.html","7bef2d37190aaf0a22c917f2cd487c45"],["/posts/4115971639.html","70f2252e49be244986ec3741e685c94e"],["/posts/4130790367.html","361448cdc5e221885aea0f3afa83364e"],["/posts/4131986683.html","16f54abdf567b77f90c8d93bdc145074"],["/posts/4177218757.html","b4ff245ad090484761f0b9b31bcfe5f0"],["/posts/4192183953.html","7903321d5214c07868700efb7608d561"],["/posts/4261103898.html","7cb70b92d10e4621c46ec55d9a378d12"],["/posts/469711973.html","4346b5fb465fad53f59645c2d8a9e263"],["/posts/482495853.html","d859750bb83ec8ae3b81de7f8a7d6a68"],["/posts/488247922.html","355c5ee077cf8f66680e3b87965c3d29"],["/posts/517302816.html","6732ccf3d1f4a5cd459351781bd5fa47"],["/posts/570165348.html","a06244ef47f585255496fcbc8e8c9ff9"],["/posts/595890772.html","6da107462a05ec160856abed2bfcf698"],["/posts/67485572.html","1aee66eacc9ba813a5f2eb8f0ceea8bf"],["/posts/694347442.html","a25b914ee031666dc53a074232a2f68b"],["/posts/707384687.html","2684dc3847dacb0388aee2661bbb46db"],["/posts/71180092.html","7fa8f4d56896fdaf3af419bd4a3e1332"],["/posts/716459272.html","c0727749ce1643c526bee71a71bec208"],["/posts/765481613.html","4880e229f3ee72cc4a4fcbd713233af6"],["/posts/778231993.html","8bdd7c625c41aacce405362a2bfa14c2"],["/posts/795397410.html","932b2901bd20cdd5beb5c08c4019dcc5"],["/posts/820223701.html","87387aa11f0940c3a916965064f7f79d"],["/posts/830372185.html","0f0a0721e025973032aede57646d9c0b"],["/posts/88294277.html","76b6f4b98dea67707ec63dc6aa9dbb8b"],["/posts/939963535.html","63483fa42765cade33f83aded8c2059c"],["/posts/983786067.html","a0c1d80a21ba2c7eca53d53c24b1c17a"],["/sw-register.js","397eb9d50687066237aaf6f1ebadc013"],["/tags/C/index.html","cc4eae34db8988db90dc05b92442f701"],["/tags/C/page/2/index.html","251a8c2f68458066ccb3146af7ce9d9e"],["/tags/C/page/3/index.html","f352c9a54dea2438d05239862d94517a"],["/tags/C/page/4/index.html","fdcc0f8fb3648b93796574696e7694f8"],["/tags/ETL/index.html","b31e16abc267530c64e0b1417bf5be0c"],["/tags/ElasticSearch/index.html","4c29a3869730db958e58d36351c64047"],["/tags/GUI/index.html","b89de716d08480a13b6d152c285935c2"],["/tags/HBase/index.html","54c881130e6c269e6c33ec4a3bdfb048"],["/tags/Hadoop/index.html","8fe34f0da76c54295c44aa0307dca011"],["/tags/Hadoop/page/2/index.html","0d9d8dbdc01c36f94ca267a05498561f"],["/tags/Java/index.html","c94bd35e4363a797726aa86d5e61a74d"],["/tags/Java后端/index.html","07c518c45d210431997cc312a5dce274"],["/tags/Java后端/page/2/index.html","92c93f17bb564bbd7115b3c3d229abc9"],["/tags/Java基础/index.html","85066e9c0036fd0703952d1c8c89e2c4"],["/tags/Java基础/page/2/index.html","e5807379be8c5f1fde603640f3e850b1"],["/tags/Kettle/index.html","3ebe3a59061c75c32fda5e7241b624b0"],["/tags/Kibana/index.html","f58dc18e92ff3b3d35952eaa0602c119"],["/tags/Linux/index.html","1805050c63dcf5292768b34353aafb42"],["/tags/Linux/page/2/index.html","f3e5740fa4abe06385c9d7593010b1c8"],["/tags/Linux/page/3/index.html","89d7c90bcc62eaa21a3f78c738114c33"],["/tags/Mac/index.html","9395bd1de146106fbc822d71d1f97cad"],["/tags/Mac/page/2/index.html","019fd5f0d010be6bb001af52586c6dc1"],["/tags/Maven/index.html","1a76225647ced21397907493580910e0"],["/tags/MySQL/index.html","b5d5626f8bf530fb6cc5a0ab96723d39"],["/tags/Python/index.html","877d96adc81cde179021a23882ce3c36"],["/tags/Redis/index.html","fa205765f97db705a8cdbf8eddb60556"],["/tags/R语言/index.html","52dcb0b237873bcc818a8ea597764bce"],["/tags/Spark/index.html","39d08a698b3cc4dd7ce23c568070fe70"],["/tags/Ubuntu/index.html","d5bfc09936747eb47ae1b5a59f57264c"],["/tags/Vue/index.html","c09f78e9c6d6ecca8bda114d70280830"],["/tags/Windows/index.html","7af9066d5921328eb3e8a8cb82e45770"],["/tags/ZooKeeper/index.html","060abda7f30f8c72b8a4b66b5aa2c6ce"],["/tags/bfs/index.html","b13aa086e5adfc9880eb38f04c5ef562"],["/tags/dfs/index.html","e52d5c16d6cdc3dbe2d0c7f93a2ff93b"],["/tags/folium/index.html","4d297695503b2bf17b96221ea77ceae7"],["/tags/git/index.html","1229fceb442686a4f3ff7f2d3cf13f4a"],["/tags/index.html","1376d93c6190a8e844f9aa60301d7b6b"],["/tags/latex/index.html","8da3d8a081aca50d36ba2584a0a2db41"],["/tags/中间件/index.html","3a4c389202c14052ca7f8fe684794d2e"],["/tags/二分查找/index.html","79deefbacb084c013d537d576ff41412"],["/tags/优化类/index.html","de0560d8f0b158e017a212303c7db2f9"],["/tags/前端/index.html","9ac83c8d2979dcfaaddad1c5310e73ef"],["/tags/前缀和与差分/index.html","fc57f6de0175fa89c15a1125281e82d2"],["/tags/动态规划/index.html","a4843229ed162800056f29d4dd426af9"],["/tags/动态规划/page/2/index.html","a671d7eda5bd48afa89df762890f0c9e"],["/tags/博客搭建/index.html","12817b9bcebbbd4112e022b0a567d975"],["/tags/图论/index.html","25d13afb3c8e217c92ad57b0e78cfef7"],["/tags/大数据/index.html","d835678fdc16dde77ccad020fb1b1034"],["/tags/大数据/page/2/index.html","f5d356e0d01d7426ac3771be7fdbe76d"],["/tags/操作系统/index.html","ccfe7940784571c6c7dfab644902fc4a"],["/tags/数学建模/index.html","68df687065bb3cda3d2123e6bea23159"],["/tags/数据库/index.html","2247138de8eea1c501c54424c2f2e897"],["/tags/数据结构和算法/index.html","7f18cd24fa0b563fd9701473b767f596"],["/tags/数据结构和算法/page/2/index.html","925af7676e4e0d87f4f513f7a7956363"],["/tags/数据结构和算法/page/3/index.html","76c74490bc0ba69d07e65e2395be3984"],["/tags/数据结构和算法/page/4/index.html","4b3d1f53785cc0a15f61b9c17fd44869"],["/tags/数组和字符串/index.html","986cb059d77cd68cc1614d5449212f8d"],["/tags/数论/index.html","b84d76ebc4df8670cc099f7fff4c80ec"],["/tags/枚举类/index.html","7a25d5468a0ae0a30ffb8760e88bb486"],["/tags/栈和队列/index.html","f1ecd27ec298682274b3e29a776f0e7d"],["/tags/树论/index.html","3b66611614312d89591e1480c0b451d7"],["/tags/测试/index.html","886987c9daba7a1cff5e3213c4415c84"],["/tags/环境/index.html","f47e951651b50e54789d82a44a72266f"],["/tags/环境变量/index.html","1648583bba2737f1933663fc4554d6b8"],["/tags/绘图/index.html","bf99673819a62373aad323474149f5e6"],["/tags/编程工具/index.html","69097dcdeb609e44a237a398c0e5ea01"],["/tags/编程环境/index.html","576563715810286bfb255d4d844d892e"],["/tags/网络编程/index.html","9b8930bda8573c7b2e2581e33f576363"],["/tags/英语语法/index.html","0f7c76e2431418324573068ac9463a0f"],["/tags/计算机操作系统/index.html","73159b76e84ba874de11c7097f3347c9"],["/tags/论文/index.html","4f1e2227c699199023eef3efbc14694e"],["/tags/资源下载/index.html","f8ea1ba0e15c768843d58e261aea16c9"],["/tags/链表/index.html","9ff1bc73e16478febe4a8e75567057d5"],["/tags/集合/index.html","a0465cf16eb5442e1291306c82a8103a"],["/tags/集群/index.html","1dcdb7bb9a0795796422ca1461d49db1"]];
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
