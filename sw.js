/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4be8681b8442c6df9663b43db2b82aec"],["/about/index.html","0d1430d69932a9daffc23dee3f6945fa"],["/archives/2023/01/index.html","ea70ceb3a5a589382d1cc9015e33ceb2"],["/archives/2023/02/index.html","8c13e4f7718ee377738d84a33868a049"],["/archives/2023/02/page/2/index.html","299f3979a4dc122990e728d48559053f"],["/archives/2023/03/index.html","8b589318a6156b8562a0f15409287131"],["/archives/2023/index.html","987124c333679ade5e8904e9caf37ba7"],["/archives/2023/page/2/index.html","f2e9c800fcb750f004c0d72746141d15"],["/archives/2023/page/3/index.html","f5f8ee8cdcf32f37cab8c744b8bc605c"],["/archives/2023/page/4/index.html","e65f836e2b1cad112d6ca072ce509294"],["/archives/index.html","170560ee2866d43231afeff8891cd55c"],["/archives/page/2/index.html","f07fb9c6b97d252defb5c23041749d4d"],["/archives/page/3/index.html","fef4f4b6e73e38b48fa943e53ebdd38a"],["/archives/page/4/index.html","bf39f7502b636fd34ec0d564b81506ae"],["/categories/Java/index.html","cba50fe290d5a52c32c7f8d4970aede1"],["/categories/Java/后端/index.html","9319f863a180bb393bc20852667503ce"],["/categories/Java/基础/index.html","9e487ade6d5a585f1f0bb9bdbed1062c"],["/categories/Java/基础/集合/index.html","1cd9711c111317601ddd4da92e78b2de"],["/categories/Python/index.html","d0ac7851c3c98022b24b5392d9456336"],["/categories/Python/编程环境/index.html","c7931da898440ba6ae8101c4c0bdac9f"],["/categories/R语言/index.html","5ee67af9aaac08bec9a3c524b9693740"],["/categories/R语言/编程环境/index.html","34f412841c08e3e74cd965622088362e"],["/categories/index.html","738a2440c96fd75c3c1b7c5179afc0f0"],["/categories/中间件/index.html","cc764fce1c76dd535e85a1c862435f1b"],["/categories/大数据开发/ElasticSearch/index.html","3e56dbf7a027438e1c58f868dd27cab2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","9226275050b1c670e307d9b67785f570"],["/categories/大数据开发/HBase/index.html","7d7987e0e29e0fd35511ba32d634dc25"],["/categories/大数据开发/HBase/学习笔记/index.html","7bc94fef6f21b8908487af340bb1652f"],["/categories/大数据开发/HBase/环境搭建/index.html","17250d61fc9cfb8a40de8424bd2d50c3"],["/categories/大数据开发/Hadoop/index.html","8ef88fdd96ceed8d2c501aaaa3013fd8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3e6d07f794a5b60c978c260bcd126332"],["/categories/大数据开发/Redis/index.html","72cce632a10f734664e6fafd6c08dace"],["/categories/大数据开发/Redis/环境搭建/index.html","b346b79f7e386d265d2c5c1ff3c0497f"],["/categories/大数据开发/Zookeeper/index.html","5ad8a47a41175ac985b729c22af60606"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f23ff98645f283596a76dd2ad52f1d59"],["/categories/大数据开发/index.html","fbd296b2de6cbc17d0ccb3835faf807a"],["/categories/操作系统/Linux/index.html","4a44a0ad83b82fd98c4e630f3a29840a"],["/categories/操作系统/Mac/index.html","b3b02e45c6f5499242db50dfff6051b8"],["/categories/操作系统/Windows/index.html","a06bffd70dc5ed398fc4a2b5b2e90f7a"],["/categories/操作系统/index.html","5f8ff2c6ae654e7e91118517eaf801eb"],["/categories/数学建模/index.html","c58f9144edde67c0b2eb69b1aced7b6a"],["/categories/数学建模/latex/index.html","31eccc9f3b84b8761d353de16f6eda91"],["/categories/数学建模/优化类/index.html","ca3d38cb7af10a642ecbba1672798c4f"],["/categories/数学建模/优化类/现代优化算法/index.html","b0ea33ee19ddc6bc42f1b27fe6035baa"],["/categories/数学建模/优化类/规划类/index.html","360ed48de59ea425b54d059b466276b6"],["/categories/数学建模/绘图/index.html","d50b884c529174544ae4c4a27015ff98"],["/categories/数据库/MySQL/index.html","490c2db1a507288e99ad695ca7a6b2f1"],["/categories/数据库/index.html","fb98dc291757f40f8ece9d55fed4fa38"],["/categories/数据结构和算法/index.html","e09ee23e45e553b4d9dac10792ee3c36"],["/categories/数据结构和算法/page/2/index.html","17949ad694adbb0f125ea916920bb745"],["/categories/数据结构和算法/基本原理/bfs/index.html","ca0ffbb5178f4c90fd5a9617a116af4a"],["/categories/数据结构和算法/基本原理/dfs/index.html","cb5cd1fcde4693a8f0fae7cf0c9fdf90"],["/categories/数据结构和算法/基本原理/index.html","66ea0cc061d74f6b354077a15224e3b2"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f1b2fab6887e31a4ce8362685de98942"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","26d28d2f6ea9a76fc6ef1fd52ab9ffb3"],["/categories/数据结构和算法/基本原理/图论/index.html","6b72f505954542e7af2dab01c966b6c0"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d19d6855237bba788c24473c5a051b54"],["/categories/数据结构和算法/基本原理/数论/index.html","b059ba361fdea3420e0628e291c8bea5"],["/categories/数据结构和算法/基本原理/树论/index.html","1566cbb21a8457ce6fb9d1f59133dea2"],["/categories/数据结构和算法/基本原理/链表/index.html","13b8f1180013eef8b9e7e63ce75fdfc5"],["/categories/数据结构和算法/算法题/index.html","c7680a5ab73257a13c69bfe9ff3a8785"],["/categories/数据结构和算法/算法题/二分查找/index.html","da75bf65e946e782505bffe7914d4889"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4ece0cabd860d0eb353c1d0dbb5859c9"],["/categories/数据结构和算法/算法题/动态规划/index.html","e0db20d175d41c3cc1d72e69c5c0aa63"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","169b3c22f1867ae2cec3609e13ff8e6c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","ab5786ca3248beadae7a451786e31195"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","323c3bfba5fcb395bef1c4cc315cb66c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","36ce15ad8ef10cb375473fd433e45713"],["/categories/数据结构和算法/算法题/栈和队列/index.html","0a73d2d3793bd31e435d60d871d0377d"],["/categories/数据结构和算法/算法题/树论/index.html","10bcf4d5e857ba00e1e1a753c78e39a9"],["/categories/杂七杂八/index.html","0ed39d953f721b6d30ce4e31c2652e26"],["/categories/杂七杂八/博客搭建/index.html","d78099832639ac4cbb04439ad2bf9432"],["/categories/编程环境/index.html","8cf28c53a4408e5d2dd637b846d73536"],["/categories/英语学习/index.html","3299a08fd249cd494c8267e4700fb08b"],["/categories/英语学习/英语语法/index.html","559c2faac51aaeb6e1397395767a9945"],["/comments/index.html","65ad75770daead496dde3390e6166880"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","eb6fe1b1ac4cc180e147c80c4b24c3cc"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cb666fd5a62c1fe19877330095dda472"],["/movies/index.html","f1e59d1117c2881655301378cbafb4e1"],["/music/index.html","2eed2507ce7da4339f1af3f2cf7b4b58"],["/page/2/index.html","5b85d6c4dc67b458fbe088cc7222cdd3"],["/page/3/index.html","cb6ac0b63cf6c3a819a8b8d844df2134"],["/page/4/index.html","b2a6d7d8301376f70325b9356b134614"],["/page/5/index.html","f009fcaa28c0deb1b8cb6daf5d7fdc8e"],["/posts/1021360842.html","27a92cd774170b2a483fdff56153e7af"],["/posts/1120620192.html","8367f980455fbd9382cf225f6beb51ee"],["/posts/1141628095.html","1a4597218850fc25c1c7a9179d793fe3"],["/posts/1168613674.html","0570875b62b28c559c05d6957404543f"],["/posts/1219920510.html","5b47f51f20a6c9178f08d91b6e8c3a30"],["/posts/1222166338.html","4152c8704c2938bc98f77d7a6d67be4d"],["/posts/1259097482.html","f21189687f1fa4f313987811fdf8487e"],["/posts/1271036369.html","63cf6d9dbb04b2bae1f181a49236c44f"],["/posts/1312847445.html","32a307995b040cb2e37911d489104ffc"],["/posts/135355774.html","cf38c68d8e4dbdf2165d08d7fa040e5f"],["/posts/1375344716.html","c0705cb56af75bc3a291f4c7a50de819"],["/posts/1388991698.html","45e8199c44c101fcb65e33bb8c175d04"],["/posts/1410315814.html","0efb3871487728f0dd7976adba015201"],["/posts/1452790229.html","6bd27ffea0cb1a2ef9feb1e5aa4bef5a"],["/posts/1470079884.html","a9613ef5322b73439933f7df499cf830"],["/posts/1470079885.html","d472b4d20efce17249ea0dcdc14d9b8b"],["/posts/1470079886.html","07782d6188a94a95cac05c98d649694c"],["/posts/1470079887.html","4d9debae66a77fd687ce73edb450addb"],["/posts/1498536549.html","09265d6fd56ed56c028d2ded80f02f18"],["/posts/1557866301.html","90ae9ace296e8060cd4011b0a366b755"],["/posts/1571776361.html","37c30fb1ee683addb60514f0ca408169"],["/posts/1605124548.html","e9411be2f71c34656efd2760e91ef3c9"],["/posts/1633036852.html","8ebd10d644b36dabdc32c626d803d27a"],["/posts/1765123828.html","2490dd1f9dc3bc4a332ca1e13bf6d20d"],["/posts/1776114197.html","0f43f4efe16b0d8f86f31f4fad1e15e0"],["/posts/1817748743.html","dbd5e41350323c3496aa6b0d46536e00"],["/posts/1925125395.html","ab1c8409b94cbc2d73e703af088e0f90"],["/posts/1966191251.html","ba5a8818c176b4ef8861749002a9395e"],["/posts/1987617322.html","a66930dd0b5040139dffee61fcb76dbc"],["/posts/1999788039.html","9385cd66706a6bab4172c332df8caa8d"],["/posts/2075104059.html","73dabfe0ee58a7b38b7e30804e953dff"],["/posts/2087796737.html","a45cc5ba207c5b3a9d1adf702d97a136"],["/posts/2106547339.html","8a144488dcc581609acc9acbd611927f"],["/posts/2207806286.html","19e7578f371810db95cf9e22e67bcac6"],["/posts/2225903441.html","574095741e9442d9f92294c661dd3f0c"],["/posts/2265610284.html","4b5c93becb810d749066585de24ecc71"],["/posts/2281352001.html","066d328703165c46d3e735a44f1bab9d"],["/posts/2364755265.html","e7341918a1bfe066c758ca11e8199da0"],["/posts/2414116852.html","30e0e71429a890fbeaa026a98de62e5a"],["/posts/2482902029.html","9be4ae8b42b1c5a4c25a3663d7d6094c"],["/posts/2495386210.html","ecf6ebf5fcf582438c306e434e4d3bb2"],["/posts/2516528882.html","9765d6feca32f4cf9f4adc76c779ec86"],["/posts/2526659543.html","e29b2b5771e6a94b585067a0a3b251aa"],["/posts/2529807823.html","bea6319ab4802fc1cd7efab71efa5f0a"],["/posts/2742438348.html","1adf6ec70d5ac5d8714784d35d124499"],["/posts/2888309600.html","6a352e3e9b4650b073dd6f9a3df9a4d2"],["/posts/2891591958.html","06856219d6002ed65fd6c64c48bf3839"],["/posts/2909934084.html","012d9498ea4f8972919de0aaaac74b19"],["/posts/2920256992.html","0a87e51dfbc5e38989265f1ab1427571"],["/posts/3005926051.html","8414b61fdedbe6862ef5c88a71770128"],["/posts/309775400.html","70d9dab8455a9280375f2e8441da019b"],["/posts/3169224211.html","483bbb44bfd867c37fa024a25d984a95"],["/posts/3259212833.html","f73c29aa5de175ca9563863c75c2d316"],["/posts/3266130344.html","5555815527e4aaa4f3f961a8f4434046"],["/posts/3306641566.html","aa5916f86de66709754c46fecaafbaf2"],["/posts/3312011324.html","c608ec2c544d7533053178250f4443fa"],["/posts/336911618.html","e2d656aaec365924ce282bbd2ef318e0"],["/posts/3402121571.html","d9af7572c6bcc38fc1050519cd604110"],["/posts/3405577485.html","f78cdacc84ea0ab55ba9fda8df62b3f8"],["/posts/3498516849.html","fe4c0eb4f3c3aa1eb048927406ea534b"],["/posts/3513711414.html","9edb34fb20061e561c0597b66a9b18fa"],["/posts/3546711884.html","632b3cc74714f49bdd442699e53614d7"],["/posts/3731385230.html","2a9f752dd3f8294ebb474a1a869caa48"],["/posts/3772089482.html","02d724a0c1759ed4461381997bdaee9e"],["/posts/386609427.html","30d0c20f3a55d41b50b687612bbe6b80"],["/posts/4044235327.html","6878210064abc70ad5c1e291b2f09fc1"],["/posts/4115971639.html","6a080acf5ec11ff5f54e68de7b719205"],["/posts/4130790367.html","c89e49af7a7823fae23ae878e1c2ce59"],["/posts/4131986683.html","345ad0ca6d00a491b3a12e566b0df66b"],["/posts/4177218757.html","50cf1813223f4c2b40e4c555d10eef79"],["/posts/4192183953.html","e06b63d119d28ecce795bb054dbf2f32"],["/posts/4261103898.html","3308d50c03d8f9e53f7e7bb9ffc47604"],["/posts/469711973.html","704e038ec9ed1531c8fa28ccc4adcc2c"],["/posts/482495853.html","f938c111380645a16940f0cc30f3bf59"],["/posts/488247922.html","26308c3eaf30dfef68097a1689c5452c"],["/posts/570165348.html","a1622a35c097cb46c6f5c379fca6df28"],["/posts/595890772.html","6d27196ce9bac078ad277b1c4d5c3a48"],["/posts/694347442.html","8cbcab11bcf5349fe924a837d7ef3004"],["/posts/707384687.html","19e7523f625382bd469d6bb24cd274db"],["/posts/71180092.html","6890fec01f497dc38376287519a2d317"],["/posts/716459272.html","bcd31f77e43fd3954e7fe1ba15b30d61"],["/posts/778231993.html","0aaf56b746e9ec833946551900730327"],["/posts/795397410.html","cdad8ba8583b33cdb263e7b4f2ed58e3"],["/posts/820223701.html","858cae81560a6a4a5cc86e9da67113f1"],["/posts/830372185.html","4043da6029f709d1ee9c14d35711fe30"],["/posts/88294277.html","eb62173c1b6af5190ff9e04fef687e54"],["/posts/939963535.html","858fc4b010a9314dc6821033fb74742d"],["/posts/983786067.html","6ef71f1d9f61c65075698f27f4b2fd23"],["/sw-register.js","d76975cf0c112c8562d1f051b2ec108e"],["/tags/C/index.html","61c31d6f93fd4d62a1a9ac22c08f6c37"],["/tags/C/page/2/index.html","3770e1628a66b2fad120925b97d31ec5"],["/tags/C/page/3/index.html","2a9f09a6babff7ba6fa91246752d54eb"],["/tags/ElasticSearch/index.html","78231c3b4bc99ac6b9501c3cec2151d1"],["/tags/GUI/index.html","61d3ec6794c18ee91e0f04d306e2285a"],["/tags/HBase/index.html","7dee4b7b148332384587684361ddd1b0"],["/tags/Hadoop/index.html","6e718d041666167735e84cec7931288c"],["/tags/Java/index.html","2c2ca6e10cdb97f6a5ce272461607f78"],["/tags/Java后端/index.html","020d01b28c7eea32ad0e27f691923da1"],["/tags/Java基础/index.html","d5978b89b9475eb4faf5d710e1a7e208"],["/tags/Java基础/page/2/index.html","7db1d75d8869278c9a0d5dd54bd0d5db"],["/tags/Kibana/index.html","4d2429f80595472891d47c527027e90c"],["/tags/Linux/index.html","c60f92e07a24fff133bc3c9176f9bf1e"],["/tags/Linux/page/2/index.html","fdd55c98119b51f4bc161815785769c2"],["/tags/Mac/index.html","202f8c36bf1af6aaa5983e5440f221c0"],["/tags/Mac/page/2/index.html","cbdf28d1189a021296452528cb6ffe14"],["/tags/Maven/index.html","8890f2abfd9956bf2b9b634c250e752e"],["/tags/MySQL/index.html","270236c73e82b0adb5decafa567a572d"],["/tags/Python/index.html","979185631f50fd40f18a308f7d3fedc9"],["/tags/Redis/index.html","fcb43f41bf4325151152c7cc9112739c"],["/tags/R语言/index.html","2c1d912ce9a0804e90f81ab034928c41"],["/tags/Ubuntu/index.html","ec4fd15fd032e1a64c8f391db9dc0d87"],["/tags/Windows/index.html","73331e514c30b824f6c35eb120952b7c"],["/tags/ZooKeeper/index.html","87c5f09339cc87ac02572e06a85b0a63"],["/tags/bfs/index.html","4f8248cae6971ef27272c19f7683ca8e"],["/tags/dfs/index.html","3432dce30dec7280d121e6265f6298cd"],["/tags/folium/index.html","d1fc35452943cbc2373353b1bfa91bf5"],["/tags/git/index.html","431b1a57d6af38a1fc9ac50bbe2894a7"],["/tags/index.html","8f5e867377c4e1386d314ba06034bd76"],["/tags/latex/index.html","6b6361a998be1b0f281068f0500a7499"],["/tags/中间件/index.html","3115f775059e918847c97b18c7d981be"],["/tags/二分查找/index.html","e4d0a0ff8329a8bfddc473a6b1a70030"],["/tags/优化类/index.html","923334d306889a03e391c44fe403d358"],["/tags/前缀和与差分/index.html","c1775aa17a116aeb474dbe88799bb94d"],["/tags/动态规划/index.html","09b81d3477467f13232752856da13cb0"],["/tags/动态规划/page/2/index.html","b42136257d52cbd4ae59c678e68211fc"],["/tags/博客搭建/index.html","ed2acc4fe26725b2e31f4aaadfa31305"],["/tags/图论/index.html","c61dcfb70060ee2b56cfc0ec5d9a1670"],["/tags/大数据/index.html","5dd4512b456a0d7eec66b2f69d33eb9c"],["/tags/大数据/page/2/index.html","3e60db0461e48d3670c6bfaca8c22468"],["/tags/操作系统/index.html","eceb033b05795fdcc7cd06aeb819391a"],["/tags/数学建模/index.html","5b4aaba4f8b1b4212efbf674827148d9"],["/tags/数据库/index.html","f9f453c874a57aceff3f0b536e731d4c"],["/tags/数据结构和算法/index.html","b49b8a68dfc5c35ab0c35abd3284163d"],["/tags/数据结构和算法/page/2/index.html","2e56d4d4a4260a2644d7cf6a92363a2c"],["/tags/数据结构和算法/page/3/index.html","2134ae2a3874add0ca15ed68d227ba3c"],["/tags/数组和字符串/index.html","72bbdc95cabe440fafcb1c485347e730"],["/tags/枚举类/index.html","35c795e6f0131e1614aef0778bfc881d"],["/tags/栈和队列/index.html","d7c699f795ff74bb2fe4ad0ff33adfbe"],["/tags/树论/index.html","91745b3cf4a30ae274f0ccd27905750b"],["/tags/测试/index.html","f6316172692fb3df6ce882d5d85578ac"],["/tags/环境/index.html","2b8fb39813292e6c0abb78d1519ec4be"],["/tags/环境变量/index.html","c071576e089d83a70d2739a1be5f5df5"],["/tags/绘图/index.html","b527256ddaec1d6d7f731760ecd9a484"],["/tags/编程环境/index.html","4f0c54710b323f01846f6b3ed46af315"],["/tags/网络编程/index.html","6f665791e56b79f7ce33c987a8110ed8"],["/tags/英语语法/index.html","07971f0c917e169834ee6fc91044e528"],["/tags/论文/index.html","2783088ef2648145de8c02cdd6b7d7fd"],["/tags/资源下载/index.html","40711fcba072e4878177eec5a45a2268"],["/tags/链表/index.html","4eb6cae6169fe72ff72ccbba9c3e3f8b"],["/tags/集合/index.html","1f8a9019da0ba02ad4da8d479bbc0984"],["/tags/集群/index.html","cdd8b5ff09d0440f982ffac79a400aa4"]];
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
