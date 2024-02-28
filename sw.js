/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e10cca8af57209da67bc649c2f8560de"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","ebf9a9493d40c68d2fc38e927b570ecf"],["/archives/2023/02/index.html","999805361b81c9956bfc739c3c8488f7"],["/archives/2023/02/page/2/index.html","c19ade49d81ee93b1fc15df5cbb754c1"],["/archives/2023/02/page/3/index.html","2d29bbb00cecd804e2e01a3e8f217651"],["/archives/2023/03/index.html","d68fc243e913624b614127a8a619f48a"],["/archives/2023/05/index.html","f160f4125a2d988765b3f68873da3004"],["/archives/2023/06/index.html","2962982a2bb73643e5faebe4b01de4ee"],["/archives/2023/09/index.html","680985b6c09077529d5eb4cc25657717"],["/archives/2023/11/index.html","7090e723c23465fb6356e9f829fab2b8"],["/archives/2023/12/index.html","6f190fa7d796c64d8bbf1fc75974548c"],["/archives/2023/index.html","cf7862c2652f172c4758e3faca4fc91e"],["/archives/2023/page/2/index.html","dc27ab0a1339bea026856ed7260bfc7b"],["/archives/2023/page/3/index.html","e0b1d8092db406b9a33ba3b6a6986e1c"],["/archives/2023/page/4/index.html","470ac5d09de1c64eabc6a745849d48bc"],["/archives/2023/page/5/index.html","ab53f89cfa30b2f191df9b1042b77c45"],["/archives/2024/02/index.html","2bf2690f8af0bb0b08dec99c0e7d22ff"],["/archives/2024/index.html","2f82d1d832062e95af3b77fac0921044"],["/archives/index.html","9cbe8f0699b0535b5c0101c172cd334e"],["/archives/page/2/index.html","ca4c2bcff7e04c40b2df5d02dd949a12"],["/archives/page/3/index.html","dad9d4bd70941a7044acc11e4980f5d1"],["/archives/page/4/index.html","1e6c9dd352a91afec30aed8ce5f31396"],["/archives/page/5/index.html","1289c6eace28fc62f7a0843507077f83"],["/baidu_verify_codeva-qQP2iZOMLX.html","c8569a16846ae5929515cab3cf0c1e77"],["/categories/Java/index.html","014767ce6a0c8fc80aa57fef386a23b6"],["/categories/Java/后端/index.html","50b3321f58f47754be15a63a799f4a2d"],["/categories/Java/基础/index.html","41be56f6bc392b5347cc2e5bb4d6a214"],["/categories/Java/基础/集合/index.html","c0626617a15c4551ff7c617b92414e28"],["/categories/Python/index.html","5720489be9e2df4661c5c127a51034a6"],["/categories/Python/编程环境/index.html","c50466db334a443671362e7138a04eda"],["/categories/R语言/index.html","d82340a4d4d1d41bc02c6bfc1abd6941"],["/categories/R语言/编程环境/index.html","dd345c7533318896fa949089f0b0aa44"],["/categories/iPad/index.html","b1925fbf35a2c14e077c81a22100a543"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","46a573f035e11dafff75fdb01459fdac"],["/categories/前端/Vue/index.html","077131054bededd4ef3d37a050c2bbbd"],["/categories/前端/index.html","ed77909584263d6eaa1a9b87ebb2a557"],["/categories/大数据开发/ElasticSearch/index.html","8add9f050062231a84f52ac23dfc245d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","a1997b4c12bb856ef4586bc5d2ee19b4"],["/categories/大数据开发/HBase/index.html","4c77c08b9b44e4f5c4c69c9f5d559611"],["/categories/大数据开发/HBase/学习笔记/index.html","6d654f587c4db1b82a0ae06c22f874d4"],["/categories/大数据开发/HBase/环境搭建/index.html","d9eba8219ee90b367083792e16b1e10a"],["/categories/大数据开发/Hadoop/index.html","43aa13e3cb1c1a88df938008be7d2235"],["/categories/大数据开发/Hadoop/技术/index.html","104c7b9706f5ad55b5e2086a912da9ab"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5a29800516098c24d834561de62ef103"],["/categories/大数据开发/Redis/index.html","853b6e344e2eda3b8e3a48329053e27f"],["/categories/大数据开发/Redis/技术/index.html","2bc6807d4a4b2f6e8ad37bb6659e7096"],["/categories/大数据开发/Redis/环境搭建/index.html","e58a9bd61196eecff4d9c091165ff9ce"],["/categories/大数据开发/Spark/index.html","211e15c0a7c99380c24a3a84479992df"],["/categories/大数据开发/Spark/环境搭建/index.html","493dfb8e1926344db14f257a279ccfe0"],["/categories/大数据开发/Zookeeper/index.html","4b05eb8d63a53576367cdcd9c1eea901"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b2e5e328ee810278afb7663843c2042e"],["/categories/大数据开发/index.html","3569e0bf88540a944afb9a652b449d71"],["/categories/学校课程/index.html","8c1211ccfefccd9087639fd79661deec"],["/categories/学校课程/计算机操作系统/index.html","4d88407c489c40e5b0e6e00d90c1c3eb"],["/categories/操作系统/Linux/index.html","316097b5846bc18c67bd50bc9b04848c"],["/categories/操作系统/Mac/index.html","7665957a26565b04362aad610db8f7b3"],["/categories/操作系统/Windows/index.html","ec8b3bca1c465654208673db4941af94"],["/categories/操作系统/index.html","6429bfba55ee6552d0cb5e3b09c3b5da"],["/categories/数学建模/index.html","f393e3293abe9ea0f3dd2c088d4477a8"],["/categories/数学建模/latex/index.html","6f286514ee543b451ac1889c5e12cce5"],["/categories/数学建模/优化类/index.html","2391b6ab561f582ae599161adf7b2dc3"],["/categories/数学建模/优化类/现代优化算法/index.html","d9fb1ac30cdbee9eb848e77ebef67f0e"],["/categories/数学建模/优化类/规划类/index.html","4bb037821289e05ddf05d7a9375df57d"],["/categories/数学建模/绘图/index.html","948c5d3e410a25b035808ce8666dcf0d"],["/categories/数据库/MySQL/index.html","43cb2d55f7571687913067a675e9705c"],["/categories/数据库/index.html","1d584401f3e87bc467157dd8dcc74255"],["/categories/数据结构和算法/index.html","1b74f562ea3891a2a84739d6193a93d9"],["/categories/数据结构和算法/page/2/index.html","bbdf25d2c9f60e08cb402d450465384d"],["/categories/数据结构和算法/基本原理/bfs/index.html","ff893b7fb628cd48433fd09b096ae021"],["/categories/数据结构和算法/基本原理/dfs/index.html","751de1b7c3b710b3c544f8ef12f01f52"],["/categories/数据结构和算法/基本原理/index.html","e202d2e71fb45f766d39c79518b21bcd"],["/categories/数据结构和算法/基本原理/动态规划/index.html","74541a83f55d48e172c2656a3c4c70c7"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","09e61dd5a8564a8bce9c9dbd1688fd52"],["/categories/数据结构和算法/基本原理/图论/index.html","162078722a85fa275b7895a25fd46ad9"],["/categories/数据结构和算法/基本原理/字符串/index.html","a3b3396b523158078601513ca17afb1d"],["/categories/数据结构和算法/基本原理/排序/index.html","bed0fd1af583804de317934a800439b4"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1bb333fde05a2bcdde5a8aab5f54e4b5"],["/categories/数据结构和算法/基本原理/数论/index.html","199c12f59babe20412c069ce704cf78d"],["/categories/数据结构和算法/基本原理/树论/index.html","dc6f1146d889d859bea376d0dd3af9b0"],["/categories/数据结构和算法/基本原理/链表/index.html","925e58f27fcbcf03df61df884905cb9c"],["/categories/数据结构和算法/算法题/index.html","bf6e9bcaa1302ae157988c12b5bfb6d6"],["/categories/数据结构和算法/算法题/二分查找/index.html","3f5fd061a08a34c3624e9cc8e449bd92"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","d80a792aa44659e10a6cfcb7332663df"],["/categories/数据结构和算法/算法题/动态规划/index.html","4d415e0a179dffd52ac7f699ff286b69"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d8f7cf213cd14c172cd926eee11d0c1c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","9d1b67c2452d63949da694e564c6d44b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e14b77f8f22b55051d29c330ddec28e9"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ed01c355e1ceda32bb705b094eb9eef1"],["/categories/数据结构和算法/算法题/数论/index.html","a0af2e4d2f10ea11b56823273cc1c823"],["/categories/数据结构和算法/算法题/栈和队列/index.html","66bea4debf57b1c812a224d4072ae21a"],["/categories/数据结构和算法/算法题/树论/index.html","256a5b807c321b99498d67315810f05d"],["/categories/杂七杂八/index.html","8df11c0692085a0d7d1aa3d3de8e4e48"],["/categories/杂七杂八/博客搭建/index.html","1460cfacdec12e5b5dbf6a7da68ae184"],["/categories/编程工具下载/index.html","8d3c82701a6d900dee266f83380ae08a"],["/categories/编程环境/index.html","d1750e6564c850a030dbdb797adff82b"],["/categories/编程环境/大数据/index.html","bf07c4c0877181c63ec83e9d76770998"],["/categories/英语学习/index.html","a40f1e4f03cc49b61503f7c6e2313a90"],["/categories/英语学习/英语语法/index.html","4ed2a48862ba1c56abf59b127b65dd14"],["/comments/index.html","7254bfcb12b49e2e8cb65ab0fa6030b0"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","061bfe3df807b4130f0fe6495153ed4d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","20cf5263947c68d0c4c1465b53e9e434"],["/movies/index.html","9020ca860693ea852c127ce5194caab9"],["/music/index.html","905bda8960f8aabf6b7a4c878b2bcfdd"],["/page/2/index.html","1b285e2b1b044474d074151d391979b0"],["/page/3/index.html","f1f5828c643c8dc727fe521c8f06f2e2"],["/page/4/index.html","a00b08d2ad50e5c207f5d8b2ea5d6c51"],["/page/5/index.html","d75265d180d1e993ed23ba2649e181f5"],["/page/6/index.html","78e6a5aab5f097c311b3abfe69eef197"],["/page/7/index.html","12fa75344b8539512d6e7e774b771db1"],["/posts/1021360842.html","b67978bb820fe8db252920f427239249"],["/posts/1120620192.html","786c4f30b0afcb45b8c50dd1692e1c9a"],["/posts/1137707673.html","16c8b0fded531e8f7f82643ed3d20650"],["/posts/1141628095.html","b20d917b1c17ff6adb513a6c5e2a6c78"],["/posts/1168613674.html","db658a5e6e5f6b344c2fd7989f1faf3c"],["/posts/1219920510.html","dc4792e33f57ad06398322e255d007af"],["/posts/1222166338.html","4c04912244a975ff5f8dd1ec63adf9cf"],["/posts/1259097482.html","b5b74c2edd3d4369c7f112b2a86206f0"],["/posts/1271036369.html","1655bd94a3c693846e0caf84a604ef32"],["/posts/1312847445.html","a12440e4dbc3461cfb7c012041e16e3a"],["/posts/135355774.html","41d6b742aa1a2f0b7479e8fb09a8c9dc"],["/posts/1375344716.html","34306317c35424875d0a1badaf58af85"],["/posts/1388991698.html","7c5acdc769f42af9d500059666924136"],["/posts/1410315814.html","ba3b7af20cd411d5c1bca0582175c45f"],["/posts/1452790229.html","2246289852e185a5f736df996d84ed9a"],["/posts/1470079884.html","24a3d8d755d108357165e4ce7d98cade"],["/posts/1470079885.html","4cf2c9f1184e62a4e8c580aed7b4423d"],["/posts/1470079886.html","c0ff74fdc0fea517a6b3fb73d9f05f4a"],["/posts/1470079887.html","78cfdb1af35074a6db700b1d0f771009"],["/posts/1498536549.html","9d5b2b3c7b49bb6327846d1dae415139"],["/posts/1539568593.html","975cd120b365f407b5febc22081ee427"],["/posts/1547067935.html","e802daa9e82decc6078b10a1bdf0e90d"],["/posts/1557866301.html","54d21fa67347b0007cc3302ad30800c1"],["/posts/1571776361.html","250b7dbee6a15e08b3cbdb673fd1b6e6"],["/posts/1605124548.html","a1739d45c5f6560bf039f07ef11e9125"],["/posts/1633036852.html","d9aee778271ffaba374bf318b605b85c"],["/posts/1667740714.html","609a45616e6b746b80cace1bc525d4e9"],["/posts/1674202625.html","083268b01581ddb60dd487748fe33daf"],["/posts/1765123828.html","faa3fb46eb63f4f578e41dfc886fa18d"],["/posts/1767336200.html","7461a95b9554cabe4df4ea8caeb299c0"],["/posts/1776114197.html","1ee826450b1aade6a323020bf8d93771"],["/posts/1817748743.html","a6e7a921a64f6df814ab9d7255b08919"],["/posts/1925125395.html","196a71033afd1751831468fdd4244ca5"],["/posts/1966191251.html","2775bc41608c2e8f34a566212eb596bb"],["/posts/1987617322.html","2653c04743ab4d312ffd25bbb524f65d"],["/posts/1999788039.html","d980056ea795924d2f08afa170a42dbc"],["/posts/2075104059.html","8bfb0bced316871455003191f56c7444"],["/posts/2087796737.html","423e812125119e45de0b2eef36de479a"],["/posts/2106547339.html","cf36bfd990d11cf5671ecd542bd86b0c"],["/posts/2207806286.html","b2543d31292b7f88d9e97235581ec6ae"],["/posts/2225903441.html","464c6315541c1ce62664d0f7c3a5a71f"],["/posts/2265610284.html","1f0af706ab4faac3866583efda36883e"],["/posts/2281352001.html","9609c8b3b675b26a163ca30976af4ad2"],["/posts/2364755265.html","a6f8238535031bcbed0600f4b8cc7cba"],["/posts/2414116852.html","e3dd49f1415f93e1049071d711b73666"],["/posts/2421785022.html","e5077c7691b4166bfa6aedc40c239666"],["/posts/2482902029.html","e295b43be2e771718ff229410500e610"],["/posts/2495386210.html","424de7263dc6f23c1fdc25626af009a6"],["/posts/2516528882.html","7a38868e2c2dfa0340e3f1cba67edd40"],["/posts/2522177458.html","f7d9e6044a0f529ef11fe799c56b983a"],["/posts/2526659543.html","44b2edcfd3d93f64d3940dcaeb4047c8"],["/posts/2529807823.html","53c72253b79b3362ac505bc250456691"],["/posts/2596601004.html","e271cc6839cb5d69ca6eca567652a6d3"],["/posts/2697614349.html","bf35332317bb110c494fa92f4c972777"],["/posts/2742438348.html","2342225cc4094ab18f10ffbc15b825f4"],["/posts/2768249503.html","f1507fe07ccc1d3fb2c6842e84d7a984"],["/posts/2864584994.html","4b11acc1a5d61b66949297c4a04b45dc"],["/posts/2888309600.html","7fdae9a853f66cb40165cb759c592810"],["/posts/2891591958.html","39bfeff4908ed5cb811fa842b6003c2b"],["/posts/2909934084.html","af764515acf8b2ab3bee4022c3ab2edc"],["/posts/2920256992.html","bee927124eecc06bce1d5fe59fce0e78"],["/posts/2959474469.html","a8501ec11c8f0970cd7678e31a843bfd"],["/posts/3005926051.html","7e7d0c87681013c682354c1fc0a81887"],["/posts/309775400.html","cb88a81cf589a3cd1e119f3e824463b7"],["/posts/3156194925.html","06d07cd7b9256fec9e0da59783332c49"],["/posts/3169224211.html","9c077f501be7104e160eb50a2827cf66"],["/posts/3213899550.html","542c10985dee6824ee14864df11bdf7b"],["/posts/3259212833.html","9e4d627f802f3354efabcdedb54e1c88"],["/posts/3265658309.html","01101276abf2df9ef0ebd89f65210182"],["/posts/3266130344.html","b1cb5585afd5d7309a8343ab556208a0"],["/posts/3292663995.html","c29e4d0e2128e2e882a80d113aadcff7"],["/posts/3297135020.html","7f3d69a0bf0a6ef4d420ce25d081f7d1"],["/posts/3306641566.html","ddf9aa271fe606f7e81ba835ecc31405"],["/posts/3312011324.html","399e41f159f17a54b511c094dc5b5671"],["/posts/336911618.html","7adb454767a7ad055952f985c8071512"],["/posts/3402121571.html","5142a99298b7039ae93b4f76dcba35c0"],["/posts/3405577485.html","d38f217e580dbe896a98b3802ef8c466"],["/posts/3498516849.html","93b881b8c4dd7aa52db51c17c655f2ac"],["/posts/350679531.html","b4be1a86b635897b8a6fb47b8e43c393"],["/posts/3513711414.html","aad5282d5cca4353c03e21e770e33d54"],["/posts/3523095624.html","a56313a9f8043c3d6ff5ef8e7ae678eb"],["/posts/3546711884.html","a6d9fdea499abd5a7bd50c7485c363cd"],["/posts/362397694.html","4cef4e74e575418836e19973e3ac9be7"],["/posts/3731385230.html","e264a611e4b1bf76d36becfe1f7632d8"],["/posts/3772089482.html","e4412c5c8738bad9b88c2adf429fceda"],["/posts/386609427.html","acafc699a4c4cb0074c030204cff4708"],["/posts/4044235327.html","04b519558554375c1f3d446b106824fb"],["/posts/4115971639.html","5d070941d3b3eae4b7fb737080196d4e"],["/posts/4130790367.html","753d8e29b2990110bf52b0f8af4aab5d"],["/posts/4131986683.html","12bc3b3cef3a3bd3978504d7c54ee0cf"],["/posts/4177218757.html","44c2f3bf2f00776f7ff4d395078dd94e"],["/posts/4192183953.html","e5f2d633d04b7d65d5749c9af299a4f9"],["/posts/4223662913.html","1943ba862216f5078c3249ba7ea91c7e"],["/posts/4261103898.html","f7cbbdb20946aed84bcb52b2bdd60682"],["/posts/4286605504.html","4ac296154c8fe25390ec40293f3baa93"],["/posts/449089913.html","c4e53d53ec26b752cdb8b286d965aaff"],["/posts/469711973.html","a82a970b71e41f23c5a41247c2e9e40e"],["/posts/482495853.html","86e01345bb5f5d6c06e94c5cd7062b92"],["/posts/488247922.html","92378529382ddf8355ffe6d1909fd587"],["/posts/517302816.html","32acc0243a38a45731e9c8c06d4d6d2c"],["/posts/570165348.html","efcf9ba31ef8368c08ab72893f34bed9"],["/posts/595890772.html","201f6875a12f5fd3571a3621b47096ae"],["/posts/67485572.html","a2ae0f57df5af9b057fe15409e6a5a3a"],["/posts/694347442.html","a092c82173aefc58a0e83a60f9e9a1ba"],["/posts/707384687.html","4b269824d339c928441fd933c74c48d2"],["/posts/71180092.html","0dde103fcb9f1860269216b44c8cadcf"],["/posts/716459272.html","c7914c04f69c0f35e7a487c4aec51d6f"],["/posts/765481613.html","37ca3cf979b035dea212e8bae0ba66ce"],["/posts/778231993.html","5c9e1ca20fec9a19cd244f12f3ea8025"],["/posts/795397410.html","037883aaf4a222c4e00978ad641cf834"],["/posts/820223701.html","298f41d291ff843125eb48c5d5b1e248"],["/posts/830372185.html","81bdde78a40704d76e81b688da1d04d8"],["/posts/88294277.html","2195f07575b27e86967cf44e09384fdb"],["/posts/939963535.html","195b1d9e7aadf2ccae2ab6c37c98649f"],["/posts/983786067.html","b6b0307b13f3e8a1aa5417e6d473bb3a"],["/sw-register.js","f40a97057bd1750c464827d76e4c15d4"],["/tags/C/index.html","69672aabff61788d367d4318eb6d9130"],["/tags/C/page/2/index.html","5ccbb6362616433bcc3427df1997c306"],["/tags/C/page/3/index.html","96b87838be13ce0fba271b743d5cddca"],["/tags/C/page/4/index.html","f3a7f9379266b16358bfff10d2328b0f"],["/tags/ETL/index.html","e7263716fae8c9205e140003c013754e"],["/tags/ElasticSearch/index.html","d12b246ee0cb702fa934fe83bee4d022"],["/tags/GUI/index.html","a637070a09a410cd5c1cc7ef590c1e27"],["/tags/HBase/index.html","6efd20a1873f53daa442fc523d9d99d8"],["/tags/Hadoop/index.html","1c4a3b259fdb964850b8a59748bcce4f"],["/tags/Hadoop/page/2/index.html","691c8dcd67d94e387cdc1121e893ffe1"],["/tags/Java/index.html","3e85efc6969e2f483713bb6c569ded15"],["/tags/Java后端/index.html","dcf14c0d800563b1e87b4612a0256cfb"],["/tags/Java后端/page/2/index.html","72ed7642ea3d8ea27c54fea0e230afd5"],["/tags/Java基础/index.html","56f8e39082c7973e2294ff42e0d80ffc"],["/tags/Java基础/page/2/index.html","77aa2e1171d5869d9df275047a16cbcc"],["/tags/Kettle/index.html","b6e34c990f807c44cff1dda28502f682"],["/tags/Kibana/index.html","e2b475f2ed12a2c0274d30ac858ad025"],["/tags/Linux/index.html","19116782d3237433ef63904d0b28c4b5"],["/tags/Linux/page/2/index.html","724253379ad37876338a2fd30e0e193e"],["/tags/Linux/page/3/index.html","3d5c0b696455130ea4707dae69b298cb"],["/tags/Mac/index.html","cc07b92bfe64864d061901507cb1f24a"],["/tags/Mac/page/2/index.html","708e33183daaa994ff05dfb43a3ec6fb"],["/tags/Maven/index.html","4295b181138303f997a24b0e83953fef"],["/tags/MySQL/index.html","d8993f2d2ff914bb2ee068026014e5aa"],["/tags/Python/index.html","f4fcf3c5266c1bcf2867c4b7641a15e8"],["/tags/Redis/index.html","0b121e4d4ec03fec17dfdb468c97e284"],["/tags/R语言/index.html","b71a86fdcdd47d6e1d717b5a891bc34d"],["/tags/Spark/index.html","5c9326ec273a89edae418d35707eeae0"],["/tags/Ubuntu/index.html","b986b22734dfcb123e5199d91252c4a1"],["/tags/Vue/index.html","5c7fb7f6dbec9fc8bb0a251b9a090015"],["/tags/Windows/index.html","fe8dff01a4c8e57a329161dd7c0d86b4"],["/tags/ZooKeeper/index.html","1f86b4e98fa8969c26e9339dad0e30c4"],["/tags/bfs/index.html","a9914432b2caa5a5bc1f3fc4ebae12ad"],["/tags/dfs/index.html","67f6199a15dc6f62120abb26bc91af78"],["/tags/folium/index.html","e7a0f4e6947fd9db59a0816e605ee6df"],["/tags/git/index.html","d45a3b6e2cab0a7a84159d188e6dfd76"],["/tags/iPad找电子书/index.html","6fa6b9653be5beb7792ebe5827083df3"],["/tags/index.html","e931697f272d4781e2e3f6ae3e692b0f"],["/tags/latex/index.html","633b6de396263a80714e7a19837992ce"],["/tags/中间件/index.html","f9de326686af5bb7ee5cecd976b2be66"],["/tags/二分查找/index.html","58d7c4175c675d942f2cf48738bde55e"],["/tags/优化类/index.html","55cfa6e766478469945162ff64fefa2c"],["/tags/前端/index.html","635b6910931f32325d16eb8a463d0aea"],["/tags/前缀和与差分/index.html","a463a6e292273482fa84fd820b296b65"],["/tags/动态规划/index.html","973d8d610900fb25a8e94ef972ed3ea9"],["/tags/动态规划/page/2/index.html","2e6b6e3dfbca6bb7a0094b127bc59e71"],["/tags/博客搭建/index.html","84e80229d0e7b3fb0a75f8342bcb9fd5"],["/tags/图论/index.html","7a70c4360efd93d20e12423788c09311"],["/tags/大数据/index.html","2988819e814a4a1de99f1350b06f10a0"],["/tags/大数据/page/2/index.html","54d3af60cf3b3e0585f67de08a19ecdb"],["/tags/排序/index.html","9572c1acbe41a9defba8a24febc871b8"],["/tags/操作系统/index.html","ac55db380f44dec8329a170888d7158c"],["/tags/数学建模/index.html","2d9e9f8420bd5770fac4b430da7c6b35"],["/tags/数据库/index.html","47e8fced0f4414fb2c2b35fb8c6fb465"],["/tags/数据结构和算法/index.html","f565f226f5a658d92f616d72102e7d6f"],["/tags/数据结构和算法/page/2/index.html","24887dd21cdd91f8f36ef33e71e09fb4"],["/tags/数据结构和算法/page/3/index.html","a52e3a3f1404a2f82315087135e94c09"],["/tags/数据结构和算法/page/4/index.html","39bb8a5d9c0f6bdf36fbb921fb1685d2"],["/tags/数据结构和算法/page/5/index.html","babf389d76b25496a0624ecb5999bf40"],["/tags/数组和字符串/index.html","00634640e7e7e046ac9afda34061d0f6"],["/tags/数论/index.html","fe12cbd5f71f7c066e34e913066f9b4a"],["/tags/枚举类/index.html","5fcb52bf907a209af710788be136fbdf"],["/tags/栈和队列/index.html","86ee01ae9d513e29bd31a88bddb460a6"],["/tags/树论/index.html","4f95b0e4924d9435c8b60925e4f7e87e"],["/tags/测试/index.html","9c46e0cd428360a1384c379841d3a277"],["/tags/环境/index.html","bbe7ddfc980c1b1a2df7b8a6d05af1d3"],["/tags/环境变量/index.html","b95c79097419e6ea3afc4d119df00bcb"],["/tags/绘图/index.html","a80548132fdbc66229373ca1e091efd9"],["/tags/编程工具/index.html","380ee80b80c0f8c322c883d3d3fde613"],["/tags/编程环境/index.html","36e71b3c8a19b3ebb6a0a7bcbe865880"],["/tags/网络编程/index.html","a480f4da014cb79397a56b85987e0c94"],["/tags/英语语法/index.html","13749c59c9d3814a1c274fe11f12896d"],["/tags/计算机操作系统/index.html","ea0eb50170719ebccf09de69677fa0ec"],["/tags/论文/index.html","db5e37978353800de3ef0694529ef837"],["/tags/资源下载/index.html","b0f49ec4c470f02a31f20286d1afab5b"],["/tags/链表/index.html","09e319f9c528d233cbdd1078010b3dc9"],["/tags/集合/index.html","296f26d0e599a30d3c19b1a570bbbef0"],["/tags/集群/index.html","9a8c1ca0bf62167c8696ef809ce8ff08"]];
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
