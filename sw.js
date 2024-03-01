/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","469335007e07886d2959108cb4f5d7fe"],["/about/index.html","e2217bc89e354425e30f827c7e65fa93"],["/archives/2023/01/index.html","4ba3d48f936ef0259625d64a15cba385"],["/archives/2023/02/index.html","4c7dea7c80187731adaf26a7cf893467"],["/archives/2023/02/page/2/index.html","7b4b7e14fa3c20f6bd23d088f6b2ee00"],["/archives/2023/02/page/3/index.html","4408ed70a16c2877e67b655722a13958"],["/archives/2023/03/index.html","ca6845745988534b2c7dab0601781779"],["/archives/2023/05/index.html","07ea4cc30acdb8404e1358630a073db6"],["/archives/2023/06/index.html","8732f1aded64848f937a831e670a4297"],["/archives/2023/09/index.html","ee7c99eb00cab08d6e8298a90dcac0eb"],["/archives/2023/11/index.html","8187bff86cbc0eb2b91ab059d0c5056e"],["/archives/2023/12/index.html","75cb41b45b832b6772fdb513082c41f9"],["/archives/2023/index.html","db285466050b34c566951c7fa347f5e0"],["/archives/2023/page/2/index.html","00666d93f75d3b1b0d2e9b4b011e9244"],["/archives/2023/page/3/index.html","f665ce3d4d776fd7cf2bfa201fc774bf"],["/archives/2023/page/4/index.html","c768f3fcda70b81221bbd7efbcabf4de"],["/archives/2023/page/5/index.html","bb307299131b1bb9f0625321a1b97dfd"],["/archives/2024/02/index.html","16259449cfb187849e1895cfccb12c53"],["/archives/2024/03/index.html","2c1b3d128cbb2046cc041c3957617b9d"],["/archives/2024/index.html","2e83da63fee880579a74f817bcbbbe5d"],["/archives/index.html","3ceb18e6f8e385a81f99ecc88fa40c2e"],["/archives/page/2/index.html","be559865a5bdccb180dd5251b555dc2c"],["/archives/page/3/index.html","9b00547321e7e54b5af309969e6dd6f5"],["/archives/page/4/index.html","e371a8e877df90b4072c9449e976e900"],["/archives/page/5/index.html","6c98a0f4869404c91e5a32ef4f873185"],["/baidu_verify_codeva-qQP2iZOMLX.html","d00095bce3a5531db63fdc30dfb8ed23"],["/categories/Java/index.html","940d05cf85af2e430f8a1f52d736368e"],["/categories/Java/后端/index.html","d74818a98e71268eb419381330742b54"],["/categories/Java/基础/index.html","741554c08b545d3f7aba60b8b23493d8"],["/categories/Java/基础/集合/index.html","0d0167fe0a0ca655d3e2e80f24914271"],["/categories/Python/index.html","493f5bacbf86f293c23ce15136f0f0a5"],["/categories/Python/编程环境/index.html","e8d2010fb73f64f4b65f0b8911c1e3e7"],["/categories/R语言/index.html","8ce4d73add3afd434e6d47fa943f38ff"],["/categories/R语言/编程环境/index.html","5995987072be125b67230a4f7ed8c76f"],["/categories/iPad/index.html","d5138b020b704d347bdbf8ca5313c5aa"],["/categories/index.html","281d938b092497041664ef26a6d4ee80"],["/categories/中间件/index.html","36cb310daa6c45e3e797dfef2788d3d2"],["/categories/前端/Vue/index.html","62998b9d5308557a522ceb12a799c106"],["/categories/前端/index.html","ef0766a6ade8e7d4acbd87f4f6fdcd53"],["/categories/大数据开发/ElasticSearch/index.html","b70e5d4593d04ddb961723066cbacc53"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","bee490a5a1d7260cdb6af77e3849bd32"],["/categories/大数据开发/HBase/index.html","86b817af2395d21cba522151487f580a"],["/categories/大数据开发/HBase/学习笔记/index.html","8edea44a3e95ff52ae7860a8bc689e6b"],["/categories/大数据开发/HBase/环境搭建/index.html","ef9a9b420a55db612fadaafc2c564d34"],["/categories/大数据开发/Hadoop/index.html","57ad2d21ea029a221ac4b84569203d08"],["/categories/大数据开发/Hadoop/技术/index.html","965cd7275360a89483289ad30f83d529"],["/categories/大数据开发/Hadoop/环境搭建/index.html","eeadeaf0096f2be0630c685225613837"],["/categories/大数据开发/Redis/index.html","35d079f1819179689e8eb1b4440f630c"],["/categories/大数据开发/Redis/技术/index.html","00ef98ee046922e6c96cbdbc0b0c235b"],["/categories/大数据开发/Redis/环境搭建/index.html","3151bb0e2355147bd90b54850408e651"],["/categories/大数据开发/Spark/index.html","ce3abe47602aa4a4fa33e42f41567329"],["/categories/大数据开发/Spark/环境搭建/index.html","0c52863fe06f554a2d09a0f218d756ab"],["/categories/大数据开发/Zookeeper/index.html","9895918ab8322e05041c490192639946"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8bfbc4416f27b4e32adcb76e499083ec"],["/categories/大数据开发/index.html","be97b433c3148a89b8dc5c31f31ced55"],["/categories/学校课程/index.html","fd59c7158c781aaf4d5fca077f0a77ca"],["/categories/学校课程/计算机操作系统/index.html","0afdc71388eaf0b1716e62745d3e518f"],["/categories/操作系统/Linux/index.html","61c01a0f10edb283b342ad78ff9c5b69"],["/categories/操作系统/Mac/index.html","75b7f893c72b8a8ee8dd12186ba6eb5c"],["/categories/操作系统/Windows/index.html","66fd7111b6936a3da85ed6f64473c856"],["/categories/操作系统/index.html","618c8be2a150c9ea9542e9121a48ae2d"],["/categories/数学建模/index.html","5b0e6ab7a244989c854f1027c6309a80"],["/categories/数学建模/latex/index.html","7101b184d4460099236632581bfb6b26"],["/categories/数学建模/优化类/index.html","4ea2ebe6f8985ccf2234983f65b7f38a"],["/categories/数学建模/优化类/现代优化算法/index.html","46ebd2f0eec88853281813d4485d53ca"],["/categories/数学建模/优化类/规划类/index.html","afca4de51359b66ae9d684fb447b06de"],["/categories/数学建模/绘图/index.html","2c5d3398695440ed0cb8b459692366ce"],["/categories/数据库/MySQL/index.html","6d30c330841015ee39995cec7044bbdb"],["/categories/数据库/index.html","88ba27e008bd046d2ab564ff247df5d7"],["/categories/数据结构和算法/index.html","dd7ce098a61a53cfe1535d24caa9626e"],["/categories/数据结构和算法/page/2/index.html","2b36c08920769799ad3cab746eb19679"],["/categories/数据结构和算法/基本原理/bfs/index.html","a2403b156e698806168b7b32a9f7ef61"],["/categories/数据结构和算法/基本原理/dfs/index.html","74b944cf866aee73ca19432b1cb1f610"],["/categories/数据结构和算法/基本原理/index.html","07fe2824c0c7bc3afc519ff48ad51e05"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","08ad758f16c2f830bdb754e54cdcd550"],["/categories/数据结构和算法/基本原理/动态规划/index.html","2f2b17f10f7f55acf44185aa6d0d0263"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","329aa668b250c7a0fd5d810c383876e3"],["/categories/数据结构和算法/基本原理/图论/index.html","d369e9468f96fbd6d7069ef7a3a5a921"],["/categories/数据结构和算法/基本原理/字符串/index.html","3b2995447abf13b8fa9a174cfa16ec15"],["/categories/数据结构和算法/基本原理/排序/index.html","4ae46ab132f3a231e4a16b6efc1717e6"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b8163380b5938b76163a5c4c03b28de2"],["/categories/数据结构和算法/基本原理/数论/index.html","386a8aaa380205bd746b6836578aac10"],["/categories/数据结构和算法/基本原理/树论/index.html","380e4602c8dc10806f6d200a9f653c8b"],["/categories/数据结构和算法/基本原理/链表/index.html","214a828d2652662af574531b2336ed8e"],["/categories/数据结构和算法/算法题/index.html","cf930273034522fff2cbd9400dfd1bdc"],["/categories/数据结构和算法/算法题/二分查找/index.html","6c016aeda9e6c27f2b469eda1021db3c"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","05ccf6345848e6023cb24ad52f3ac729"],["/categories/数据结构和算法/算法题/动态规划/index.html","d0b903e03cd2e26d7279f2ed95f696ce"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","cbee826f6b69c0ae43ba6156e8224f53"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3c07f0ab0392fceb9aa1cb0a9d2f8f97"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","f165a516bcf1a6f96d39d59659675234"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d830eba358668f1aa4663f94027701e7"],["/categories/数据结构和算法/算法题/数论/index.html","dd12ef9c32748de367b0bf935bea1797"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8dee7f58d371f4287594d4568059c7e4"],["/categories/数据结构和算法/算法题/树论/index.html","9d49be2f74ab350836ebb5b4ec6e758d"],["/categories/杂七杂八/index.html","c529a4fb182db8bd94d26ba110691531"],["/categories/杂七杂八/博客搭建/index.html","876e8b84c6026499d9b82c86247618ab"],["/categories/编程工具下载/index.html","63361c056f4cb0914a48634fadc034e5"],["/categories/编程环境/index.html","1ed3e817d90f8ca77b3efd62861099ff"],["/categories/编程环境/大数据/index.html","264fe0a24270ca2b4e8fd70447031a03"],["/categories/英语学习/index.html","0d8528c208139d4ca22aa486560c25a2"],["/categories/英语学习/英语语法/index.html","4e6d32bd6019c567121f6cf2e2a2c664"],["/comments/index.html","682b4c3a834cc67bd340ba5b67c113f9"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0b1ae4a7cce1a0bbaf3f725cf2a00f41"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","61a3e808b1362166c7a366b3afc99f4b"],["/movies/index.html","d550b32e852e2d845b5f6a99fbae904e"],["/music/index.html","671304f61e6bea358d7eb70856c16614"],["/page/2/index.html","87398f822d0bb5476c0ad205cb264a06"],["/page/3/index.html","d21e5dd533cd977ee16df5ab17bed3af"],["/page/4/index.html","dd3520cdb2f4a14d75e0917378c62194"],["/page/5/index.html","5d888e732874f4f0e4cc679720cfef32"],["/page/6/index.html","0d65bc13044287ac61064e3ed976308c"],["/page/7/index.html","5f28c57242293e3c711666a315061711"],["/posts/1021360842.html","baae818c090a1074e23f818f0fb89e3d"],["/posts/1120620192.html","a33badae63c5b46a0a8b166a9e4c3c81"],["/posts/1137707673.html","d273ae800bf99bc41c83c50ead0923fa"],["/posts/1141628095.html","9471cea9fab4696092a7af8e1fe9f094"],["/posts/1168613674.html","1d3189f6f88d0bbe7524655ac5c2234d"],["/posts/1219920510.html","638d5c5dd164943bb3a2e83bce506716"],["/posts/1222166338.html","b7a0963e3959f3bb1b37f20d16eb0697"],["/posts/1259097482.html","41f5b507db8fcc197ec662f9ef5578e8"],["/posts/1271036369.html","9a54853bbf9c9c4c0e8b4ae84da7d51c"],["/posts/1312847445.html","cf188e6ce91605d5b95211fe53f68484"],["/posts/135355774.html","ae73b6e3a8924df63765e38232d48ac4"],["/posts/1375344716.html","7730d24db199a62700590e7fcf46209f"],["/posts/1388991698.html","808d2f7206ded67863cdd7af1255b633"],["/posts/1410315814.html","512e2557af63a9e1e85dfc82b9638036"],["/posts/1452790229.html","10e2dc5dcefbc8a8cf9a20a5e09f2df0"],["/posts/1470079884.html","ce182c5983a8daeec5fb894a14ca7748"],["/posts/1470079885.html","f3e6322e581e91210b92cf8ccbefd116"],["/posts/1470079886.html","7feb310b0c23691a071aad4038450b09"],["/posts/1470079887.html","2a17eb2260feb9a894290c2a087694a9"],["/posts/1498536549.html","de4c726280d2afc30781516fa4e14c45"],["/posts/1539568593.html","3a306e1793ca73f2f7fef54dccfbe299"],["/posts/1547067935.html","29aa6b42ab440edc74ee1b45b9f30a3f"],["/posts/1557866301.html","6dd13093ef9a8f752202405334deb082"],["/posts/1571776361.html","ecfef31d40d5665b1cab312257f0870a"],["/posts/1605124548.html","a5d620333c7e431d453ce9ce6be5375e"],["/posts/1633036852.html","48bc17c1f49fb6fbd1b9fb0aa4e72ecb"],["/posts/1667740714.html","146aec3a80def956e2d99ace45d307c6"],["/posts/1674202625.html","e181ac9a389c14e1faf91c84320220e1"],["/posts/1765123828.html","e8a7e5f5a07ba7fbe2e67bcacd087474"],["/posts/1767336200.html","5a5ab85f5894e13074aef9499bce6f4c"],["/posts/1776114197.html","a335ff8343b05508c08f0b79d514ed32"],["/posts/1817748743.html","4b3dc125129123a605e1690a3f8aa406"],["/posts/1925125395.html","a7dbcbf6bc0ede81ca447cb71be0e756"],["/posts/1966191251.html","991e9863eee3785de8a45afe1a30ff56"],["/posts/1987617322.html","39ed8e608b41da7f69fa86c27850c362"],["/posts/1999788039.html","0afc3ea6baa6c9ac5fd0f9ca78f2e06e"],["/posts/2007534187.html","c40b4889b43df213b39f3bf835b83e22"],["/posts/2075104059.html","2f4eb6007357da59a75e871b8732cd79"],["/posts/2087796737.html","d978332ab8f4f0e5a8b9ca5f8470753f"],["/posts/2106547339.html","bdf951f0af7a178beef8b72441ec21e9"],["/posts/2207806286.html","c37ea4b136bb72831b8dfb6b5e308570"],["/posts/2225903441.html","02c1d98e3912fc6aee69a011dcc79786"],["/posts/2265610284.html","0e28964bddb31acec68a7ddfd03df2a9"],["/posts/2281352001.html","4e607e481474000e57893ff5b255bcc2"],["/posts/2364755265.html","aa5a80c9b421525e3e9db2edd24a3555"],["/posts/2414116852.html","a42f10fce831ea22a78aff7771c09cb4"],["/posts/2421785022.html","3dbf31f4c7adcadd88cc396c9be0424c"],["/posts/2482902029.html","2bb121afa74523c857f99f549c8eac9c"],["/posts/2495386210.html","71f8f591a9fdf707f90702b9e22c30f2"],["/posts/2516528882.html","4c1bd5e31272eae3dfd9312be825c8ac"],["/posts/2522177458.html","79de2ee9c2d11a276a507ba3c573885d"],["/posts/2526659543.html","21c95daa00cdf89a75d6a156a9fefb8d"],["/posts/2529807823.html","72cb72764c9e82100ba9153d899ae694"],["/posts/2596601004.html","515f49a18eb7e0a83f88094ad1b6cbf0"],["/posts/2697614349.html","68582e9035b4ff57144962eae18c47dd"],["/posts/2742438348.html","a928c742058f2a83101cf0973d1543a9"],["/posts/2768249503.html","7c463613e6ae13205d757e014f51cd3b"],["/posts/2864584994.html","97e1c38a07d81fe4d5d0131973b24565"],["/posts/2888309600.html","2f2bd4733a86dad95cf36f98e5ed364b"],["/posts/2891591958.html","0c23311d6a0ef2f9e2ab65194fef2952"],["/posts/2909934084.html","d499e1229c219b499dfeae409f05bada"],["/posts/2920256992.html","e3c21423bbc8b25538cf31f2e5728a0d"],["/posts/2959474469.html","82437dad8228530a9f904bc17d0a8188"],["/posts/3005926051.html","71f5b73f1e08ac79b7ff387c795fe6a4"],["/posts/309775400.html","06bba4ec5aab3b656a03a338e1824b76"],["/posts/3156194925.html","2d02092d6deff90a30d24dc3fad1a09a"],["/posts/3169224211.html","a3f82cf24e286b90661ee83ac0c04598"],["/posts/3213899550.html","7834590a3cf746330478fa778a1045af"],["/posts/3259212833.html","1a3768b34a97316f56fc53f68eae58b2"],["/posts/3265658309.html","64efb9bb8eddd3a7fe2ef77aa3d941b7"],["/posts/3266130344.html","fa2ea7190bbf45842f74cacd84aa1435"],["/posts/3292663995.html","fe8524085b45c7ca1e420f46ed16278a"],["/posts/3297135020.html","72a27278a0267380c56ac273ff9acf9c"],["/posts/3306641566.html","1bfd185cb5d35493841d71ab758f6734"],["/posts/3312011324.html","d7b770e1feed4ab6d35c47ba7e90fa6c"],["/posts/336911618.html","bf0f7bf2a3dcd09d6b8c7c6b6abfa887"],["/posts/3402121571.html","10e62bd936ff6aedaffbfa1eab03833e"],["/posts/3405577485.html","b9a7945611415e41520e4f71a1275f53"],["/posts/3498516849.html","aeb5a8be0cfc52bbbf44731a5e39f64a"],["/posts/350679531.html","c0773d4faa5bf92a621b6008c7ceb28f"],["/posts/3513711414.html","4257966dba0c8a24f7f52df5cf3d8ca2"],["/posts/3523095624.html","a436431a9b1ad073ed762fd2504f1f6a"],["/posts/3546711884.html","242689b473992aff6f952355001acc33"],["/posts/362397694.html","7bccab2812697d21786bcec95539f856"],["/posts/3731385230.html","6be6d6f1db876f9c442995cdff015000"],["/posts/3772089482.html","794e8ca6222f9f3a555756039c97d5cb"],["/posts/386609427.html","013384199c0c9a587b5234af961c2e38"],["/posts/4044235327.html","77c8a2663972cebd52e63c86d2557a10"],["/posts/4115971639.html","75b75b0399acebd3f36470d9e5e896d1"],["/posts/4130790367.html","3920b971eb7f4a4455a49348d658521a"],["/posts/4131986683.html","54990b5e657e25bdad7b340b5983fdba"],["/posts/4177218757.html","508f8ed82e5cfcc94e07177ac2c7728e"],["/posts/4192183953.html","89f6676dd308cd83a8dbfa0a5753a9d1"],["/posts/4223662913.html","c758308c7f3cdfecbe608e6a9a289781"],["/posts/4261103898.html","c7cb0b250148ca870c9abeaba13c92c0"],["/posts/4286605504.html","e142776d57bd962a60955f7c2f06f329"],["/posts/449089913.html","a32fed9ed41a13180d7b3b65c25c7c43"],["/posts/469711973.html","4bc1d0d2b7200b813bf79ba8723fc0f3"],["/posts/482495853.html","013d2f40382b0826b6611ac8396089d2"],["/posts/488247922.html","2c135c08522c2cdd55debbda40cfd846"],["/posts/517302816.html","a24534f39f9249dc012f8b125c5c7dad"],["/posts/570165348.html","9252adb35263c2e74ca122518c0977a5"],["/posts/595890772.html","a7521e38d1235c7b0231cfc632545be2"],["/posts/67485572.html","389871427bd6ec8e4f52e8034d2f30ac"],["/posts/694347442.html","2c6077b2e4f634f914e24f7f21f361d9"],["/posts/707384687.html","375dc34aebbfd37dde28ecb71d5d6ae9"],["/posts/71180092.html","efe288411cb42b2d2df32e6aa26adea2"],["/posts/716459272.html","e915c5746c06f462a5f947ad493ceffe"],["/posts/765481613.html","b19098f157777fca1f58baeff4659311"],["/posts/778231993.html","1b413292429beedda67d13af94ad0e03"],["/posts/795397410.html","81cc192377e79a0a120f3032449025da"],["/posts/820223701.html","adc9c9c46e67f59b6b951223581b434d"],["/posts/830372185.html","4a158943a5c6ed5694973907d54f8e72"],["/posts/88294277.html","52a7f82fc9cbeafdcb29b05d774bf46a"],["/posts/939963535.html","137ded3e76fd9578548aac94d0b4f552"],["/posts/983786067.html","febf69ccf9b38d89df04f44a0063c308"],["/sw-register.js","f23c1b5657ae16842cdd6fff99c223ea"],["/tags/C/index.html","32241b035c2af1e2bb0dc7f7615ba53c"],["/tags/C/page/2/index.html","b73e0bbd159fd81ebb02aa81287f1dea"],["/tags/C/page/3/index.html","28e55499b34c15fc111f23e7233fffe5"],["/tags/C/page/4/index.html","a5bb7d56ac35420451748f1164990749"],["/tags/ETL/index.html","3111b967cec09f4e72bc07bbac07b1de"],["/tags/ElasticSearch/index.html","d5671ef1fe275d6abd0f6f25a7bd4967"],["/tags/GUI/index.html","a0da492496070816333d72ed3e9b6cf0"],["/tags/HBase/index.html","91b25575be3a8c25e107881e19103a99"],["/tags/Hadoop/index.html","f8c9eeafb6a1dec4c1726e325c804105"],["/tags/Hadoop/page/2/index.html","0362eb5b962be8126da9d171516ccdad"],["/tags/Java/index.html","ddc107d70c899ca8b4dc385d047f6a2d"],["/tags/Java后端/index.html","fa57ef4542a3eec91abf1d5481e4f23b"],["/tags/Java后端/page/2/index.html","4ecdf188af7f9c34ebfb4884506d7014"],["/tags/Java基础/index.html","1d0156fe4acd3005864dae7041581c3c"],["/tags/Java基础/page/2/index.html","88f2693e92f5335602cf8339d30c053f"],["/tags/Kettle/index.html","0cf1482837272f22e242c36af89553d4"],["/tags/Kibana/index.html","4a9cc08d21f7f8582c1dc92c1324ce14"],["/tags/Linux/index.html","b3898a13f411f4d453bb535d8e50cf0b"],["/tags/Linux/page/2/index.html","8559aeb7d272db1639abcc6248f314a7"],["/tags/Linux/page/3/index.html","1f7657b3b5dedeba3e860722e0f7ecb2"],["/tags/Mac/index.html","f97334eb646b7c25925c08014d78b1f6"],["/tags/Mac/page/2/index.html","9ebde4b4f8239e8818c863d51619529f"],["/tags/Maven/index.html","094f0860583314a4ed6fa8ba506bff8e"],["/tags/MySQL/index.html","b8bb64f094ee6de78446621adfb593dd"],["/tags/Python/index.html","90c305c551ca867de485faea4dbca0c8"],["/tags/Redis/index.html","41af50265aa55c64d352477505aeeccf"],["/tags/R语言/index.html","abc71449825002af09c3c9c30ea5f691"],["/tags/Spark/index.html","fecda7072089f5ab6b19d375aff5176b"],["/tags/Ubuntu/index.html","f365ed24cc831d073b08fcd9d9c72b42"],["/tags/Vue/index.html","a9edd9a350b452cd561692efb2bc44c7"],["/tags/Windows/index.html","84ab79391cfcb6715b281c7e39026a14"],["/tags/ZooKeeper/index.html","16a65fdfbc1ea36e9ebc52f5bd3d0474"],["/tags/bfs/index.html","2c46679f9b5e4185b6c9bd969e326624"],["/tags/dfs/index.html","c66e2fbb74c168d9071ba8aa0d8ab628"],["/tags/folium/index.html","774ea51635ff31df1e3deb4564165bf6"],["/tags/git/index.html","bf7a67e4c509b9f97011714e34630109"],["/tags/iPad找电子书/index.html","9dbf2e7d2a4df38b622695ddd590e1e8"],["/tags/index.html","16da860d6acbe8a0ca17a25d82cd3e74"],["/tags/latex/index.html","c36cb7de593b6eba3c9225147f50d816"],["/tags/中间件/index.html","1c16aaa1f50143652412c9dfbc24f793"],["/tags/二分查找/index.html","28d5800f1b5bd0a0329ce191e31b453b"],["/tags/优化类/index.html","2d739cc3767cdf0b63f8936d8058e388"],["/tags/前端/index.html","c19f617e9467e83e6914d1e78ee8a034"],["/tags/前缀和与差分/index.html","b8f20009d809e4bbd9b38e4443ba6656"],["/tags/动态规划/index.html","9d1b5a9e6eca9db24ef4d7f89613b4d9"],["/tags/动态规划/page/2/index.html","7e30bbd05669907142b11e3c327522bf"],["/tags/博客搭建/index.html","d767d742cbb419e8d49fef7842be660d"],["/tags/图论/index.html","23bc418fd5dd830c59da80da8e439607"],["/tags/大数据/index.html","ee8b3cd8e58bd2b7d7c9efa5f95a685a"],["/tags/大数据/page/2/index.html","5c754742bae924bd60bf43463261c574"],["/tags/排序/index.html","0af94005e34ea6cdaa59b0ef58e4c736"],["/tags/操作系统/index.html","92cd66568b8206116d58d5a6a3424d29"],["/tags/数学建模/index.html","9412dabf79f5d66e9b8a41b085ae7458"],["/tags/数据库/index.html","73ae74952fd5d029bf51a4f38487faae"],["/tags/数据结构和算法/index.html","fa6abed9ad4520d293ca3f55b3b0ec3b"],["/tags/数据结构和算法/page/2/index.html","78555cd87967a57ffce5405922189ef0"],["/tags/数据结构和算法/page/3/index.html","8031d31460e2ceb065a3934b54a8f72d"],["/tags/数据结构和算法/page/4/index.html","d7b3711cef59fe1e64055d42f01bca29"],["/tags/数据结构和算法/page/5/index.html","22ee593ededddcd898f4a3d64a2b7610"],["/tags/数组和字符串/index.html","e99fed906e8818e2efe5b3518b5d21be"],["/tags/数论/index.html","b04053ca8a3e3bbb77c7e12a67f25118"],["/tags/枚举类/index.html","d67c62f02b6ec5c6074d5779375ccc74"],["/tags/栈和队列/index.html","a2b4d8eefdffbeb1cc7ca4440ca967f6"],["/tags/树论/index.html","9a85f2d01937454bb52726e365d1199d"],["/tags/测试/index.html","93704308858f2c415e6358084a963bb3"],["/tags/环境/index.html","165c7f5eb638eb852a72f8ba3ea2092a"],["/tags/环境变量/index.html","f187dab10e37c1fbb0cef3d904a1d975"],["/tags/绘图/index.html","cc56c15f7c8f74f535bd1335c80d5f24"],["/tags/编程工具/index.html","ad9ec3bb0f664e609d9a72c097cfa1f0"],["/tags/编程环境/index.html","b4aa80a52a78a85972e20604c63111e8"],["/tags/网络编程/index.html","dc3fd7daf3f728b933f0476009c89120"],["/tags/英语语法/index.html","170a3edfc10b40085f3dbbd742b75cea"],["/tags/计算机操作系统/index.html","4c35ae08879e9b6e50092ed4bb315055"],["/tags/论文/index.html","40c7fd79af5377fda3134b4544ead009"],["/tags/资源下载/index.html","3de8016c7d567fba1a58f1d28df9f224"],["/tags/链表/index.html","d545812a4e7bf6bc51c29950a5bfc94a"],["/tags/集合/index.html","c036ab1a7d60bf6a171cf59988c18900"],["/tags/集群/index.html","1b25e6afcbcf1f18352589d2959e4e64"]];
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
