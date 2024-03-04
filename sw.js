/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","93ab6f9db2302af1c76b3a54fcd211e3"],["/about/index.html","29b07b0fc8b87f7f632f30d917d0873f"],["/archives/2023/01/index.html","d8850357efdc2dbfab81203dc00bd80e"],["/archives/2023/02/index.html","eb5f5193ba45ac7673e50a90881322cb"],["/archives/2023/02/page/2/index.html","7cd55a35322eb022d002ad04500c594b"],["/archives/2023/02/page/3/index.html","8781fff7977f17208ac1dc6c2738d423"],["/archives/2023/03/index.html","1320f7dd83e654ec001488b35e5647b8"],["/archives/2023/05/index.html","a6d78212f1f8ae92fc272253105658c9"],["/archives/2023/06/index.html","94a1239741fff85337885b27998fa593"],["/archives/2023/09/index.html","81d8a5cf932c17096a402d15132ceee4"],["/archives/2023/11/index.html","af1d6ad1c95d19f70ddba9804e4a7552"],["/archives/2023/12/index.html","2ec68ce232e9ed06e7bdc5ad56d94422"],["/archives/2023/index.html","874b6a54fd5483d73c7cd9d280ce6f65"],["/archives/2023/page/2/index.html","615dde19642425c465ec2c569e75355d"],["/archives/2023/page/3/index.html","8ac222fd78b02580413a4220ab26882b"],["/archives/2023/page/4/index.html","a8f49a26de2e6ce7e36277f679fdb1e9"],["/archives/2023/page/5/index.html","a83d80a7c85b3ab3db7425b9dd23caa3"],["/archives/2024/02/index.html","8d78be30215296e18643d1a5fba270f9"],["/archives/2024/03/index.html","7f291552811e6957a4ee868fe3a1bbea"],["/archives/2024/index.html","b74c8ad180de3bb0c3d7cabcb13024fb"],["/archives/index.html","e1e565c29e74e9038681be73b75eba4a"],["/archives/page/2/index.html","fd834d6227009c9d3501f15c6b1633af"],["/archives/page/3/index.html","840e06539e37960ed6325dd4c929c3f4"],["/archives/page/4/index.html","b1504c2fb9d22c07c1d69872fe0fd602"],["/archives/page/5/index.html","3c91168eab587fe9a17a6f0b1fc806fd"],["/baidu_verify_codeva-qQP2iZOMLX.html","6d451b74d0657b7a7211f18d4dd451a6"],["/categories/Java/index.html","53216291bd0963f5eee27114e56b1245"],["/categories/Java/后端/index.html","3c397ff1aebbe8bf7ec41aeb32577bc9"],["/categories/Java/基础/index.html","9222d0f02c91e30408f2e00bcd994c0a"],["/categories/Java/基础/集合/index.html","33147ceb866e9909d8c18d1c4a0bce8f"],["/categories/Python/index.html","454241169c4d3108a78ea25477124f97"],["/categories/Python/编程环境/index.html","9abaff2ff856b54d455084cff5a85b18"],["/categories/R语言/index.html","7d76c5494356767553ca587569c234cc"],["/categories/R语言/编程环境/index.html","d2179355c23581415a9f1b8eb9f591bf"],["/categories/iPad/index.html","09dc23fce9c4dd001ba4b3f9d5250e56"],["/categories/index.html","352da50e8dfe834f1f4db47270bbd771"],["/categories/中间件/index.html","ad4ae31de819d14b9b27e7a4a0e6a15d"],["/categories/前端/Vue/index.html","80814c1a8da46df4b4d610b7c7b7a754"],["/categories/前端/index.html","97c50a36a96d625c405c4414690337ce"],["/categories/大数据开发/ElasticSearch/index.html","bd83b6fe9f4a725609b75e0df4304ef9"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","eea5bcb8419e53817f1ba576a6edb66d"],["/categories/大数据开发/HBase/index.html","db74790e4edeba62673bc4aac24b5d6f"],["/categories/大数据开发/HBase/学习笔记/index.html","6dee98268c1c59428236d99c109633a2"],["/categories/大数据开发/HBase/环境搭建/index.html","38e319169bebc9ca2ac44d130c8f4725"],["/categories/大数据开发/Hadoop/index.html","ba6ba162f98ab939bb79ca7d3dbba570"],["/categories/大数据开发/Hadoop/技术/index.html","9809a8ccfb9446dbec2ae035bbf81f2a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","fd2acad4097b1dfd261e6c23ca31601e"],["/categories/大数据开发/Redis/index.html","5e0c827fa1feb10573537b7c81f980b0"],["/categories/大数据开发/Redis/技术/index.html","206815acd9abb2c40d94032834d39ead"],["/categories/大数据开发/Redis/环境搭建/index.html","bfd4b22f90725628ef81b691395830f9"],["/categories/大数据开发/Spark/index.html","52142bda738529d16ca7896306dbccf5"],["/categories/大数据开发/Spark/环境搭建/index.html","6c95bd371948bdd77515943ec3112c07"],["/categories/大数据开发/Zookeeper/index.html","71f67f69d0a9b08e867be5c90960b959"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","649f028c6089a59cd7245361ee4fb53b"],["/categories/大数据开发/index.html","03b2242de65ab710d3d2163c9a75f8f0"],["/categories/学校课程/index.html","679de5b161bf1b16f702a72e1d408dee"],["/categories/学校课程/计算机操作系统/index.html","597f9983dcf86231d8f2a67560d3cc90"],["/categories/操作系统/Linux/index.html","265bedbdae893a0b539473ff17670625"],["/categories/操作系统/Mac/index.html","c76b6b1c6fc07b9aaf091655cb2e808b"],["/categories/操作系统/Windows/index.html","c9e056b867df0fafc8ff2ac0eb6f0fae"],["/categories/操作系统/index.html","cd71507b96f4f55884ce2f2e1cae3680"],["/categories/数学建模/index.html","92b6b16067ea944d87e2577171002fdc"],["/categories/数学建模/latex/index.html","1e1086c163e96698bda1dbafa30f6d00"],["/categories/数学建模/优化类/index.html","4e79832c18ed0a65233f9a27fadd2c9b"],["/categories/数学建模/优化类/现代优化算法/index.html","2c77dc6b7da3ef5ea51b7d0e2f2e54cf"],["/categories/数学建模/优化类/规划类/index.html","3d3b59d47e5eaa90db67cb2be908a7c5"],["/categories/数学建模/绘图/index.html","898fa9231e6b4f10b3b249bd7661c403"],["/categories/数据库/MySQL/index.html","1074bbab09029be04d32b5a4acbe07bb"],["/categories/数据库/index.html","133909e0551899a828af67c03c381c8a"],["/categories/数据结构和算法/index.html","168452d18b2932a0cb2222b82feb81b3"],["/categories/数据结构和算法/page/2/index.html","98c07ef0eb7bf10f0b66a7f027ed12c0"],["/categories/数据结构和算法/基本原理/index.html","aaa0fdfa79bc1439495398e91f1ea3fd"],["/categories/数据结构和算法/基本原理/page/2/index.html","9d65cf8ce025c70f0e62721985c3800a"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","b43f62805101ef34e81be5860f5cb151"],["/categories/数据结构和算法/基本原理/动态规划/index.html","1c6969aba682fbdcc9da65d504fac32b"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","1e797802d00e04d88ed49ca6fbd2ec70"],["/categories/数据结构和算法/基本原理/图论/index.html","6646506d038af0dfefc43b5a0b99132a"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","e84c984bb2b95601146a0ec77aa40290"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","68f556ce423e40be00babb91fc243c14"],["/categories/数据结构和算法/基本原理/字符串/index.html","2b2ce38e26e429ac1ccf6fb7bb5710de"],["/categories/数据结构和算法/基本原理/排序/index.html","e892c76f669cd86af523c4b981bc5d25"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","8c8b05fd49319584639e1963bf3cfe04"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","31837e6691cdb62a423b7098cccb88c3"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","89ab3eb7c0361b467ae4b980d16b7dcb"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ce7299c64b91f39fe847c278cb18ee67"],["/categories/数据结构和算法/基本原理/链表/index.html","aed03c3e93a359697bbc8ab0d65ba0d4"],["/categories/数据结构和算法/基本原理/高精度/index.html","a29b4b71448868a125f32d29e64d247b"],["/categories/数据结构和算法/算法题/index.html","5c6ad0bb9edb37ffde96617a59e9765b"],["/categories/数据结构和算法/算法题/二分查找/index.html","fab8c40354946ae2b2944644e841a845"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5a8f9d86226c1d1099316003a699276c"],["/categories/数据结构和算法/算法题/动态规划/index.html","62b2fade8e02829b806872607f4b2d9f"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","eb10aed6454fd9b456bed472f8ee10c6"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b6cd2c4211e57da01f7a116be256af5c"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c4ab9c5b289060fdfb34fb4537521379"],["/categories/数据结构和算法/算法题/图论/index.html","1398d1ef38e0c54bf93c62a0157853c8"],["/categories/数据结构和算法/算法题/图论/树论/index.html","787c5c89ba5649f5860c11275577a761"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ffac9a2fa108d319ebb836b92d4bd548"],["/categories/数据结构和算法/算法题/数论/index.html","641dc208433ddecf9e1b3b3ee443bec5"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b09d299e7ee9abced2f218c3dceaf585"],["/categories/数据结构和算法/算法题/高精度/index.html","249dbcbb86d3410c122ecffca0c14627"],["/categories/杂七杂八/index.html","bb143350d7b239cea371e5970f080146"],["/categories/杂七杂八/博客搭建/index.html","c6cd8e9f071ffb6765ebdec98adb0f06"],["/categories/编程工具下载/index.html","9249f60a5fa573d3da3eea69c94c9450"],["/categories/编程环境/index.html","149b20ee9df50506a39687c88a312b69"],["/categories/编程环境/大数据/index.html","7279253ac6a2fc270aa19736e8ae162c"],["/categories/英语学习/index.html","b1ce8fd754d518856108d57fed9bd2d4"],["/categories/英语学习/英语语法/index.html","dcd4eef3ced5bda0ba70fcba025b226d"],["/comments/index.html","a30e81ed8206ba101927965e86e42b9e"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","53cbbda82211a8f14636d093b2f9f4f4"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","3834c8f3640ba843fe33e82a27c25717"],["/movies/index.html","b3b3e2f7534a5805da4c880973c881e5"],["/music/index.html","62661d98c24dcfedf28ab721bb104eec"],["/page/2/index.html","76034f3c53e63f7cb6e3093de9810d8b"],["/page/3/index.html","007efd394e53ae44b628a2e4cc9edbeb"],["/page/4/index.html","c4caaa42f5c0b93490262a18be2116f7"],["/page/5/index.html","48c15fa3f6f3aeb7d09bd7024b446772"],["/page/6/index.html","546dff2e9503a2622ca55dbc36914ae9"],["/page/7/index.html","99875296f3b629e563e2e50677e94f3e"],["/posts/1021360842.html","3c34041060e5b3e9c449393423ffc7f1"],["/posts/1120620192.html","9454020d775dc59022f7a7f0b7f5cdf2"],["/posts/1137707673.html","f9d0a4a740408b1188d3192aade8fb3f"],["/posts/1141628095.html","4f0d30b28e881194e2460cecf3e44acb"],["/posts/1168613674.html","94243b224c5c6dfae49db9c718be203f"],["/posts/1219920510.html","365f00f811dbbdb908e98a7f9ed25643"],["/posts/1222166338.html","be5aeebb31f3974152685e5006003220"],["/posts/1259097482.html","d64438916b25d67dcd1db408d0416983"],["/posts/1271036369.html","a7e0d287e11d2b36b394d0054db990bd"],["/posts/1312847445.html","9ff05a9d7be05449bc5e0ddac34c7dc6"],["/posts/135355774.html","35d5760230d9449932a92448c83ecea4"],["/posts/1375344716.html","c2330601f2a52dec35f5c56bb822c65d"],["/posts/1388991698.html","9d868146717ce8d3be4829c6e7589019"],["/posts/1410315814.html","2ac994300a4ec90c6a20673cc465c9fd"],["/posts/1452790229.html","13e539ca967d777dbf3c8c100a9ecaee"],["/posts/1470079884.html","a6c13ca9f3c89d6b9141468145fab442"],["/posts/1470079885.html","43a62fc3aa5b993ec9170eb31aae34ac"],["/posts/1470079886.html","6797d684658c2eab85d6e6186bf0ad60"],["/posts/1470079887.html","7221ac3b721d333c758809c608a7d47f"],["/posts/1498536549.html","988618f834dfcea66542a994dc8e4bf5"],["/posts/1539568593.html","71b20f721ba2f7cec25999679caac3a5"],["/posts/1547067935.html","debdfda2a06b7a5e3f30148c181991dd"],["/posts/1557866301.html","299d9e5ef7f2af8edc089d3dbfc922b6"],["/posts/1571776361.html","0741b06c5021d65e90ac03dc4b77bf88"],["/posts/1605124548.html","bdc4977419fea55d90e9003fd52d1ec3"],["/posts/1633036852.html","7a4168ebb14422ef0e65935b17241dc9"],["/posts/1667740714.html","8c6dea8ed261f4f157089586b9e15958"],["/posts/1674202625.html","1016dcfb5c01d977bd356a54cd83a7dc"],["/posts/1765123828.html","2b37c5c16361ad8da523fb0672c110bd"],["/posts/1767336200.html","bd4ff0d5b9d7a5c104bc5329f10c0a28"],["/posts/1776114197.html","983c7f1c24c2b3d250dddc2aa2029a19"],["/posts/1817748743.html","3c4d328efbe2d30678dc6c2d03079f3d"],["/posts/1925125395.html","32f91aac7d0731bb1a569d5fbda838e7"],["/posts/1966191251.html","10a426507f73e066041c10cdab6b468b"],["/posts/1976734692.html","1d27fc8a6010f5a0ed25862f4d0d65f7"],["/posts/1987617322.html","7b2572201f2b151983b1c8b89e19ee15"],["/posts/1999788039.html","861ac0dbc6315128a55a84f8bdf6920b"],["/posts/2007534187.html","b61edb2e03f180bf10a9cde82c03b4a7"],["/posts/2075104059.html","16f27e2d41bd8cf354b79b1014ddb23b"],["/posts/2087796737.html","415a9d8451092a3f997ea9ee599863e3"],["/posts/2106547339.html","0713c0710bd4f26783b2c92abffca9ae"],["/posts/2207806286.html","d57624ceccd27c0529cdf9029131a290"],["/posts/2225903441.html","09975addc40e3f04a22fc1c2c1d554c1"],["/posts/2265610284.html","475b5eca2b15a5fce8cdbfa6c172e681"],["/posts/2281352001.html","57361f10df3223d74fe0251cd7e6fb89"],["/posts/2364755265.html","d73d2fc5dd2622a46e6c38ce08c8e7e0"],["/posts/2414116852.html","5062ea961a602e59893d0f2d1ed1b557"],["/posts/2421785022.html","43784c4af69333bf5f3ef02748c48ee3"],["/posts/2445543724.html","5122458e243cdf52e8c3b3ec1e72f7b5"],["/posts/2482902029.html","dad49895200c0e4f03842b3b0a9af3dd"],["/posts/2495386210.html","01b49a2e4ebf80a000112f92cfa6afc4"],["/posts/2516528882.html","3bddf3e3b9961046e20af70dd41e66ed"],["/posts/2522177458.html","295c35dbd34eb8445c3cfb8420d145f5"],["/posts/2526659543.html","623e5257bbc0fba2d3ad68002677b78a"],["/posts/2529807823.html","3b6cdcd31ada943b5fac03e9972e12e5"],["/posts/2592249117.html","789e71a82a069bf3ca693227965117a6"],["/posts/2596601004.html","52f5a54c62320d31d933131d886b6614"],["/posts/2697614349.html","1c3a5b397dcc54319ead262bca048a78"],["/posts/2742438348.html","991e0161f3b80eefb2743e94e1400c75"],["/posts/2768249503.html","6b3ce6972ecef65b690f391a767a9514"],["/posts/2864584994.html","5977e11ca8d6fe840c1baa32af6ae7f5"],["/posts/28838462.html","7f1ce278710af1ca32d86e67b2466caf"],["/posts/2888309600.html","30cc69799a97562acd6fabd3dc29e4cb"],["/posts/2891591958.html","e2b5a828026ac6eee9b498c407a2db6c"],["/posts/2909934084.html","485c6234f0ed25bc897fa157acff954a"],["/posts/2920256992.html","aea8c72d1836fc65cccdf98b9103da6a"],["/posts/2959474469.html","6f50e99aa70a1dbfb8e601514b194217"],["/posts/3005926051.html","6d88308a42a769c3df70f4f7f24a1d03"],["/posts/309775400.html","136db4b5b526955912e45c884c0bfe51"],["/posts/3156194925.html","9e814d3deeeff1a21414792b7e5d5590"],["/posts/3169224211.html","1b0b5126ef541f8abea93c2d8aced6c3"],["/posts/3183912587.html","b0b66b86448fd9d41d14a2e0f19eb304"],["/posts/3213899550.html","30006603ab51d0cf909da2d247b645da"],["/posts/3259212833.html","580a1cd2edbfea3a8c43b0e939988390"],["/posts/3265658309.html","63bd115ea79a05fe6a16255c87b3b796"],["/posts/3266130344.html","91c56c8ae2add408a76d005ac8c4a304"],["/posts/3292663995.html","4231be60e0d19bf842c736df8f4a0f7b"],["/posts/3297135020.html","386aa103d8a77529cdf8bc48d7488373"],["/posts/3306641566.html","dbf7c94be2ed2e5f8c089ead120005dc"],["/posts/3312011324.html","0fcb15d6d3f26515aaddac1806f2958d"],["/posts/336911618.html","a40d54bfb7e841b00abba6f429385cb5"],["/posts/3402121571.html","d3c6a11231915a4ccdc73ab0f94e5b86"],["/posts/3405577485.html","427b01484d02a346583b2e11bb499e06"],["/posts/3413737268.html","c5d7525c4803b58b5423333ccfb61cc1"],["/posts/3498516849.html","678848c805f399d5af553c8552912e22"],["/posts/350679531.html","d4142b881d390cbb313a23620e365917"],["/posts/3513711414.html","9810c99db994b868ee595ac61e1d15e5"],["/posts/3523095624.html","0db7663be8df4ccf1d51d70a1b7379dc"],["/posts/3546711884.html","cd541dffdee6a4cc91d2c92206e04e01"],["/posts/362397694.html","b13161aae3d3257c84b2a7d4fba141cc"],["/posts/3731385230.html","5f3d327c678fdbf70a08fdbc1aaba2c3"],["/posts/3772089482.html","1bb565e4b16ab43438a122159c18c9ff"],["/posts/386609427.html","03fe46330eb233246f206f9045b25ec1"],["/posts/4044235327.html","7d950edf6f3deed46780c605e6daf8e3"],["/posts/4098221856.html","fd5e1f3a977f07453855d74a839edd79"],["/posts/4115971639.html","9a0dbbc9238d0fe459e918bce788c5f5"],["/posts/4130790367.html","e8d41f0259130b32f7f064bb90663498"],["/posts/4131986683.html","83c1b09e7a897dbbb64dcf886fa5b278"],["/posts/4177218757.html","9e98a498e30f30a2fcabd5814db3391b"],["/posts/4192183953.html","1c32c2405434559790a47cc992b1b901"],["/posts/4223662913.html","f412c4f1e8bbb6bbc4da43f16c698d23"],["/posts/4261103898.html","acb757ba608927b9eed5d5757f05c6f2"],["/posts/4286605504.html","c1b697c583a8f906902c0fd49ec9e4f0"],["/posts/449089913.html","ea63812c4085aec7b12c6f845bd5eecd"],["/posts/469277133.html","867be9e9b2453116c9a3dc1e4d26f559"],["/posts/469711973.html","f477b2d6ed98ddf3934ed726918d2d15"],["/posts/482495853.html","611ea21b036b0696caae565bfa5e8f15"],["/posts/488247922.html","1522aba7cb3c1a3b68e8f05625647d8d"],["/posts/517302816.html","361613837f268c971d1e7e4d2d3fe569"],["/posts/570165348.html","0b22c5ddad7430966a648fdaf64045ae"],["/posts/595890772.html","ca72bff8eb833ec070d07b6f159c6579"],["/posts/67485572.html","e62670110a4cff3eba36b3d3e1e36083"],["/posts/694347442.html","7725a81baea7f617b68fdc142841b8be"],["/posts/707384687.html","d28cb696f3d4259f077aa9c17957eae1"],["/posts/71180092.html","9939db12e4b04a103c04e05e30e6b1e4"],["/posts/716459272.html","52da304c9ece7cc39209a182b6092f42"],["/posts/765481613.html","fef2e4ea268555ae79b8b2556fde2d1a"],["/posts/778231993.html","b7f7cb45c4567e358fa8a5a312fcb2d8"],["/posts/795397410.html","c439c757478ca9f09af0d6761a5e6dee"],["/posts/820223701.html","94ecfb34b05f67e9e7847fe44a30c390"],["/posts/830372185.html","92bec1ddd4912ae862024b53aba8583a"],["/posts/88294277.html","642178a9b124f786b1ff216ed62a30a1"],["/posts/939963535.html","73a31bf5f34a9b7da4d83204695f434f"],["/posts/983786067.html","0226e62a0397d31c93fdd0338f6ef7b6"],["/posts/999620786.html","fee06e58c48a11b665db1d88037b04f1"],["/sw-register.js","89dfcf352fd2ca0dbcfec688d5fb5a2f"],["/tags/C/index.html","9011c5900cd1db51bebbd44b83910775"],["/tags/C/page/2/index.html","d78fcd03268c5c273c6cd97da5bb834d"],["/tags/C/page/3/index.html","e82b5a4e065900e5a2cf9d5f55aef23f"],["/tags/C/page/4/index.html","cca768c3d00ea4603a77b90f809e125d"],["/tags/ETL/index.html","b5d3b423f5128c2386c614ae72432e26"],["/tags/ElasticSearch/index.html","74e9beba464bb519aed6cd047e4cc65e"],["/tags/GUI/index.html","c94f4fe5319570df518550c900787ac5"],["/tags/HBase/index.html","641b6eba0e48c44f6060eccaba97bcd8"],["/tags/Hadoop/index.html","2472d1d5163cdfa7544527bb10be3498"],["/tags/Hadoop/page/2/index.html","d4e1d55af7f44de524b4d6438f7bcb34"],["/tags/Java/index.html","e832c3b3a174dc92407d3ff1d404dd2e"],["/tags/Java/page/2/index.html","03db69af390824de7c76bba4222457b0"],["/tags/Java/page/3/index.html","b6e68fe0e350530c62710fec3db3dc6e"],["/tags/Java后端/index.html","b673014fe6583fda4bcf7004e578087c"],["/tags/Java后端/page/2/index.html","4f3c376b6f72a629ead00da9f8687e35"],["/tags/Kettle/index.html","532460957f534a3666461ccf10bdae25"],["/tags/Kibana/index.html","dbfd25b203baaa0ce05ee110ee70e867"],["/tags/Linux/index.html","694705671a0953daa3a0a1e81eda35e6"],["/tags/Linux/page/2/index.html","8d052487fd245f60458bb3fe6f5169a4"],["/tags/Linux/page/3/index.html","99c8426bec1ddabfe452beef8a4cd350"],["/tags/Mac/index.html","e7dae7f235b4d87294bc22bc000d973d"],["/tags/Mac/page/2/index.html","8e839635f7f0630e6fc30c9817a4d1c6"],["/tags/Maven/index.html","0acbb285ca4d6df4ec311cc725125137"],["/tags/MySQL/index.html","cd309e5675df84eddc71c3c1a82f0966"],["/tags/Python/index.html","e6c29a23f1a4fc2ef6786e3069d17ebb"],["/tags/Redis/index.html","59eedc5ee6b161fce5d67c7bd2df094c"],["/tags/R语言/index.html","fb72dcf944ae69d038af0890beb751bf"],["/tags/Spark/index.html","cc2b42d8bd2a4ac7ef2a06e27af652ca"],["/tags/Ubuntu/index.html","fd9b575a081125190fec29ef56520fd5"],["/tags/Vue/index.html","2e1c8ea0f6be68c52242d615fdc3b6af"],["/tags/Windows/index.html","3b88b36fc5523f67f1da5063bda347ac"],["/tags/ZooKeeper/index.html","a53656f7c44d41d07bec6724abd39597"],["/tags/bfs/index.html","75e94202860335ce1951f20e565dafd6"],["/tags/dfs/index.html","4023b30f1bd66db10d376c21d8b40475"],["/tags/folium/index.html","6f044f5b9efff1286064b6aa4a93cd58"],["/tags/git/index.html","a941a9e7b268f8135300ba4191cbc312"],["/tags/iPad找电子书/index.html","e80f43b26897bd4e3c72445fa64a917b"],["/tags/index.html","cf731c5eaaa6c553031bdc9d49a59d8c"],["/tags/latex/index.html","1867f7841dc733a355123c525a9cddf8"],["/tags/中间件/index.html","7f31d6d196cc0e0782012bec073077ad"],["/tags/二分查找/index.html","2042f841b4020610347a446dd8368e58"],["/tags/优化类/index.html","2961820b3aa12fa25770fb8c3dee21f2"],["/tags/前端/index.html","ecda73abe7d3cef7ed313d746f6676f5"],["/tags/前缀和与差分/index.html","49ed728b47114c449cb60a37440317f6"],["/tags/动态规划/index.html","07ef46a0687ae4a605032eb9a4830aef"],["/tags/动态规划/page/2/index.html","1dbdf3f4a9dcacfa8800f17b466ed8b8"],["/tags/博客搭建/index.html","a9ecf8c191e1d6cf133647f9cd078643"],["/tags/图论/index.html","9559f1781feb46a1aa7d720b1586f53b"],["/tags/图论/page/2/index.html","8e4346ae3a85df99a3415cc0cf6cd549"],["/tags/大数据/index.html","6e9178c20606f5faa7345cefc3f51b38"],["/tags/大数据/page/2/index.html","a5ce5ba51eabe56c975ebf6528e6ce75"],["/tags/宽度优先搜索算法/index.html","088ab4314d8ebe8aec5729ad2214e0b1"],["/tags/排序/index.html","c72cc997655b002d67f80ee2bd6f9085"],["/tags/操作系统/index.html","27aedd8a8422a7ce056db2f1275f1124"],["/tags/数学建模/index.html","51289aa26f57b4fe12dba42080c5c172"],["/tags/数据库/index.html","3d95a8c4db4cbcb2236efe514e593ceb"],["/tags/数据结构和算法/index.html","c6a8096a84fcffb6330bbe003dee93bd"],["/tags/数据结构和算法/page/2/index.html","08796af0998eee40842e9c3968bd8f3d"],["/tags/数据结构和算法/page/3/index.html","00afada121670c82e0d6b414ba086026"],["/tags/数据结构和算法/page/4/index.html","cf9a5f9b196dd4f2a8b66961bfe2d098"],["/tags/数据结构和算法/page/5/index.html","33540faff058a9c05610e17a40d6baa0"],["/tags/数据结构和算法/page/6/index.html","9391faf30b79701c86b5b71ec4e7395b"],["/tags/数组和字符串/index.html","4fe8d90c0bc1e89822b761e1c508dd29"],["/tags/数论/index.html","c430d8a1e53272bc890f6850f47ccb6b"],["/tags/枚举类/index.html","b5dcdc855655b5d672f04293e8e4d500"],["/tags/栈和队列/index.html","d53fa1dc339ea7e50099915e54a6ce1f"],["/tags/树论/index.html","ce784118c1f902c67aeaea4c8777f98f"],["/tags/测试/index.html","86a85fbec830fc5f1c29ccb7778a18bd"],["/tags/深度优先搜索算法/index.html","4bf0caa81e38c049769e2daa4059ec32"],["/tags/环境/index.html","537b28f66d6cc159d7b7d527af458e08"],["/tags/环境变量/index.html","bdd45190d86604bf50b820097d4e1303"],["/tags/绘图/index.html","3e80c8dbf5582bb47c5e896894302177"],["/tags/编程工具/index.html","1b254911dc4dfdbab4476e87bef1485a"],["/tags/编程环境/index.html","fd21e14e514ae9fc4284bcc6c1f24028"],["/tags/网络编程/index.html","a9600cdfbf66d756cbe2b461b2193980"],["/tags/英语语法/index.html","a5853fbd1a789c049eb3a372acb0aa37"],["/tags/计算机操作系统/index.html","b829d8eec732472b4fa34b7c66affdb3"],["/tags/论文/index.html","b89136dff7fb5887723691efbac204b8"],["/tags/资源下载/index.html","007759af764c5f0812c24ced891a3781"],["/tags/链表/index.html","50c1d46b182edbcf5c71d611bbceee90"],["/tags/集合/index.html","9804cc8078b29aa189f386f33c49742e"],["/tags/集群/index.html","61a9520c6d604d1f9ab36924d0ace0bf"],["/tags/高精度/index.html","fb97b833c9beb841daec49ed8324764a"]];
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
