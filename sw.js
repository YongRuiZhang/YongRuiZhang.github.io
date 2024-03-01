/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","f456e44b14f957c15579c6182b6919f3"],["/about/index.html","11979ee55d6c285d93d31f21ea61ee94"],["/archives/2023/01/index.html","b3b9e16d36fd8f786518552716be7eab"],["/archives/2023/02/index.html","9e6916f531fdde5c6d2118b2d42df00e"],["/archives/2023/02/page/2/index.html","e2399df4e54f52e157145444095c1914"],["/archives/2023/02/page/3/index.html","9d9642cd47e8757e3172b49abfa057b7"],["/archives/2023/03/index.html","2bdc18dbe332a051c7189ae7c4af6a19"],["/archives/2023/05/index.html","d6f0b769a4f8a7ebfa5d302be64c8d55"],["/archives/2023/06/index.html","8022937eb90c8fcb5654a90914254d0b"],["/archives/2023/09/index.html","e0f3485e24ce2a80511e4bc4fd8f90a9"],["/archives/2023/11/index.html","edba05dc93588aad535c0cad1db46610"],["/archives/2023/12/index.html","39ad6a6f5c1a67ca3a4772fa14e410cc"],["/archives/2023/index.html","756d755c025dcba372979d918b58ceba"],["/archives/2023/page/2/index.html","9da77d1ee04585fa054b52fb679f4044"],["/archives/2023/page/3/index.html","d79bec325f817b4dc80409f626733faf"],["/archives/2023/page/4/index.html","d98fa41f46e7c565c4dd9685af74ac89"],["/archives/2023/page/5/index.html","be8c472d3e40eb7c4fbd0ec6e26e6276"],["/archives/2024/02/index.html","cf87c9bbf7918fcf641c27f70749b42f"],["/archives/2024/index.html","94d7cac3659c6d74e0564845ffff7ab8"],["/archives/index.html","9f0b9747ebfb6f51eef2182519202ca3"],["/archives/page/2/index.html","a6d4239990713ce443e092f8b94b8d6e"],["/archives/page/3/index.html","c0c0c57876e6882035160d05f2d45dc8"],["/archives/page/4/index.html","5ce667b5333f0eea158330623cda260c"],["/archives/page/5/index.html","64449a81c14f061be21ddb4925661c70"],["/baidu_verify_codeva-qQP2iZOMLX.html","06855b79c44d8b794d43aa05151c1bb6"],["/categories/Java/index.html","381dc93905d0f2210356f19c80087804"],["/categories/Java/后端/index.html","0e258261345e4fbc833ba0954abd1502"],["/categories/Java/基础/index.html","1a5290b6660e757f465088307f1b96e6"],["/categories/Java/基础/集合/index.html","cb619140add9ef92ded1333d8188bf33"],["/categories/Python/index.html","6407f13375c8d74eab11b8a9ceb25e1d"],["/categories/Python/编程环境/index.html","d70669b707295abd9a7f8d0a8fd5274f"],["/categories/R语言/index.html","61ad84da22b169eb0e13aa028ea59362"],["/categories/R语言/编程环境/index.html","77405325cfbbf2ef2cd48f14402831d0"],["/categories/iPad/index.html","7d137d744fcba9568c147618d56836b7"],["/categories/index.html","1afda85454dd76847f5d5dc988f3d456"],["/categories/中间件/index.html","16f3081b16ae6bd7b7216848f38e552c"],["/categories/前端/Vue/index.html","7736f3601d846514db070c4af5cb96de"],["/categories/前端/index.html","40bc6e4dd4692474dba607f5bdd9c4a7"],["/categories/大数据开发/ElasticSearch/index.html","f872902588dd22049ec7210914fbd396"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","971fe677e9be1bfd908935e36e4b7193"],["/categories/大数据开发/HBase/index.html","539e245148c122973a990c41adc557a6"],["/categories/大数据开发/HBase/学习笔记/index.html","df5e402c90e1066385fc6f640d7c5533"],["/categories/大数据开发/HBase/环境搭建/index.html","bbb6badc4fff079660e6379096687b9d"],["/categories/大数据开发/Hadoop/index.html","85ee53976d1d85e82c83525b9cbd768c"],["/categories/大数据开发/Hadoop/技术/index.html","edb5f1abceabbd786761f6074d85b26e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f3ae68be4c77cb0c42f98db4b90cbd58"],["/categories/大数据开发/Redis/index.html","d8296e4ae524453ab74e6d7d21e10b9f"],["/categories/大数据开发/Redis/技术/index.html","9694e0b17933ddf28422921ae1285846"],["/categories/大数据开发/Redis/环境搭建/index.html","7e140b3367958a57c2d7e7de0649dbc3"],["/categories/大数据开发/Spark/index.html","3c789e3cad6381bd2d59abe014ff0d10"],["/categories/大数据开发/Spark/环境搭建/index.html","3d3e1d71c3b574ff3285b3cdd77ca123"],["/categories/大数据开发/Zookeeper/index.html","9996245b8808ca6f7b37591f379f68b0"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","44b1ae2a04dbe4a73f972e30b9176a06"],["/categories/大数据开发/index.html","2ed508fcf251792dda6a82c29d632185"],["/categories/学校课程/index.html","a9c9a6f8db689e379be2b877b1b52ea8"],["/categories/学校课程/计算机操作系统/index.html","bbbbd2e80fb6f2123707584da83de253"],["/categories/操作系统/Linux/index.html","a379d5bd2a8d2e21ce2f6640d3640eb3"],["/categories/操作系统/Mac/index.html","b9f75cd3fd277322ba144b6035676b8d"],["/categories/操作系统/Windows/index.html","f963fbfc9a75aebebc1eeff5777bbcc2"],["/categories/操作系统/index.html","a05ba5b18b39391b03b2e0b1b16aceee"],["/categories/数学建模/index.html","2fa09cb89edb09ea13b60e0db4d34483"],["/categories/数学建模/latex/index.html","a89a7a1855d619e0fd68ec468cd05902"],["/categories/数学建模/优化类/index.html","06f1d4a7e310049d4afa64376c64c1a1"],["/categories/数学建模/优化类/现代优化算法/index.html","014b05d2cbdd0a70ecdda8b926610109"],["/categories/数学建模/优化类/规划类/index.html","da7eb290b53e38ca28e1228f59fa102a"],["/categories/数学建模/绘图/index.html","3a6b89bb86a3fb2718d15cfb38a13ed3"],["/categories/数据库/MySQL/index.html","e602cafc6aa272b75257cdde41872acf"],["/categories/数据库/index.html","53c87d62c999c01ad9db8d6478999a13"],["/categories/数据结构和算法/index.html","1a99bba740e2637d655f933a20c97d80"],["/categories/数据结构和算法/page/2/index.html","e38a64c3b1a6dfabdcf788ab9ce2b123"],["/categories/数据结构和算法/基本原理/bfs/index.html","5c054bd6df19ced7975328b1ac3844fa"],["/categories/数据结构和算法/基本原理/dfs/index.html","0d96df40889e91911e6673c0b334f635"],["/categories/数据结构和算法/基本原理/index.html","d859e501c32f3a565216dad30b15a991"],["/categories/数据结构和算法/基本原理/动态规划/index.html","ac5cca8f0734b0a11fdb2b37a6a5dbb3"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5b033051581fe70fe98da0a8b11eb516"],["/categories/数据结构和算法/基本原理/图论/index.html","392f0706723d0d986e692213da1ab702"],["/categories/数据结构和算法/基本原理/字符串/index.html","5b7463153ec9b6e7d8cf1733d8afc3db"],["/categories/数据结构和算法/基本原理/排序/index.html","025005edc174da3962431b5cb95fa39b"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","22454c81c941bc768e990111c8856453"],["/categories/数据结构和算法/基本原理/数论/index.html","c7f9e9f6249f9629c5f80e6533f39063"],["/categories/数据结构和算法/基本原理/树论/index.html","b154c044184d43ee283718080510bb11"],["/categories/数据结构和算法/基本原理/链表/index.html","7548b35a81d8ae7f597ac3b34279aab6"],["/categories/数据结构和算法/算法题/index.html","ff5af091ffe2de972f55fe3cf05ebc99"],["/categories/数据结构和算法/算法题/二分查找/index.html","cb61313e5ce8967daa85008b1b10f4db"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4aa6fb4ff2736ba3510e6501b8c0e802"],["/categories/数据结构和算法/算法题/动态规划/index.html","ed70c9e2922590d168d56fcf9aefdcaa"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3de6684ba82be57cd93ffe4943063667"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","0e629a9ba853b8ee7b837341268c1241"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","3f3ca035cad998baf7f5ed50f77c99f3"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","367dad4f4e2d456aee0e7ced42c36fd6"],["/categories/数据结构和算法/算法题/数论/index.html","d8c7f6a22676f224b257862adb05a130"],["/categories/数据结构和算法/算法题/栈和队列/index.html","62c4786b3bfe92b4e7bc86551a1980a2"],["/categories/数据结构和算法/算法题/树论/index.html","09e9aa514ab6db7eb01e87a33746c3de"],["/categories/杂七杂八/index.html","538d2056416ba11d85418f5441c00c8c"],["/categories/杂七杂八/博客搭建/index.html","eaa8882b9f7c2e1e34f2bccf67a8ed88"],["/categories/编程工具下载/index.html","14581e4a120c697bc3b0f8e7b92d486c"],["/categories/编程环境/index.html","c0144a758cb2218b5d6c7264b0782bf0"],["/categories/编程环境/大数据/index.html","5c15a965112aad954e93d08dc9f31add"],["/categories/英语学习/index.html","ffe83c85198664fdbc34692544d8f472"],["/categories/英语学习/英语语法/index.html","014e1ba95c1b799ae4139d329011a951"],["/comments/index.html","c44abebd2fb80f91b631d75a9acac1d4"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","f45fd7994f0b274912de5ecf3babf453"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","e37f14ccde94271235bd7703d9640589"],["/movies/index.html","a2e6860ad4f47b0372ee6a3ac96ccba7"],["/music/index.html","8c60311126049fe041cd7b9858c5ecf9"],["/page/2/index.html","1b6b0d27c687ccbd655d837acb1f2487"],["/page/3/index.html","aa40613879d5747a56f02d415e7defd7"],["/page/4/index.html","e01aea0012e5fa2fcc38fc074c350eda"],["/page/5/index.html","ebfa1dc6081f9e1a9cab944450710835"],["/page/6/index.html","590138c4801d1214810ee9d54513a58a"],["/page/7/index.html","e2098af3affcc689a763470457ac3c13"],["/posts/1021360842.html","ab27ad907944798bd8304b4d00ef87ae"],["/posts/1120620192.html","662d77572f9f4a58a033bcb0a1d29334"],["/posts/1137707673.html","42890fbd76e8376db7d069f2e1d9c5f1"],["/posts/1141628095.html","724ffa3aac0c686d96d431d517bd234d"],["/posts/1168613674.html","9803b7c03381bf56ff139e4ab5be2f60"],["/posts/1219920510.html","898dd3bce8c190948b1bb864f70c9617"],["/posts/1222166338.html","f57c32740f9d2774335dba0f8a14c50e"],["/posts/1259097482.html","7b00ef8294d240ee602bbe505b32e19f"],["/posts/1271036369.html","3a0e646c8e65f0db3c4bd754803e17f6"],["/posts/1312847445.html","bbf164af2ff973b4d08e892347e9ccb3"],["/posts/135355774.html","b7c8245f54da9831f46bec7b0da00f2d"],["/posts/1375344716.html","e99207499f81beddb17cf29f023abc0d"],["/posts/1388991698.html","a049a3e0c4562ba5df27a3a13c40304f"],["/posts/1410315814.html","c64fd146d133f56a5cd8220bdf3ffdab"],["/posts/1452790229.html","f1a58281e204f5dcff6f368bdf4900eb"],["/posts/1470079884.html","58fee0a3384ea7cf9a7edcf30b9d3f7c"],["/posts/1470079885.html","06686d4cf4238a883a57706eb8f12ad0"],["/posts/1470079886.html","e74c14891ce55087856e5ce9a93a1e28"],["/posts/1470079887.html","f593ec56879b98b72a2b267c29007b2a"],["/posts/1498536549.html","71cb11a9ebe0546f1c053dd139f6bf5a"],["/posts/1539568593.html","cc47bf57c1b9aae03d48234a17fadec9"],["/posts/1547067935.html","3f820cf4b816f8d6d052f419dbae61f5"],["/posts/1557866301.html","e5f2d2f83b01c4cd79e9806dd23ce2a2"],["/posts/1571776361.html","9a02dfb6faeb203ff653a25e99f1148f"],["/posts/1605124548.html","9a9fc19ce2e0a8b80f5366dd72d4df0b"],["/posts/1633036852.html","0420681dce0f569f7fabcf8c3a872c57"],["/posts/1667740714.html","078d8aab1d8c3ed7716d8dcc95457912"],["/posts/1674202625.html","5a303a545feca32ebab254a4bfbb612c"],["/posts/1765123828.html","d32cc2822a234ce82d5a32ea03dc1da4"],["/posts/1767336200.html","4c0c4563bb772d1b7072657f65c098c0"],["/posts/1776114197.html","ed30d8e2d8c7dd9d373a9c19d0ee1fc3"],["/posts/1817748743.html","33181525d8104b09ea6581872e843cf5"],["/posts/1925125395.html","89bf27a930d3337ea9baa334891b42a6"],["/posts/1966191251.html","41fe7017dad73a983efaa17fc2046aeb"],["/posts/1987617322.html","608fe2572a5e6970c6bdd26a09c7eb97"],["/posts/1999788039.html","5636a256c9c9795e1604dce19781bcc6"],["/posts/2075104059.html","f9fdc781465374758a6e87fdd940cac3"],["/posts/2087796737.html","bb4f479a39841600184c2f55a867e0f7"],["/posts/2106547339.html","53fd256b7daf3acc2e7327b225f98129"],["/posts/2207806286.html","0d60a9ea720d111ccd1e1a8de248d678"],["/posts/2225903441.html","9797e9331a3a3a10bb1f7112bb4539bf"],["/posts/2265610284.html","7746db445deabe61f5d8fdcba3a0f333"],["/posts/2281352001.html","bbe3295886af770c06846beb2d2a2102"],["/posts/2364755265.html","d28350342e6a983485c2c7987d920fe0"],["/posts/2414116852.html","a01479f0330c8e9fbc24c5118110cbe8"],["/posts/2421785022.html","ad6e01a87ade79eba0935b77e35affc1"],["/posts/2482902029.html","4eb2cf8b3a80caf2c5d6c8e419a58676"],["/posts/2495386210.html","5ce6c98737163741de53a98efe28ffc7"],["/posts/2516528882.html","ea99c1fc2b79fe45316c2f774f226d7f"],["/posts/2522177458.html","c7134b971ae50aad0207631c4dfa8adc"],["/posts/2526659543.html","4be1a12628731c1e30a43047a18df761"],["/posts/2529807823.html","b651454960800386fd5959397c74c2e4"],["/posts/2596601004.html","f9ae85683e3986e99fea0277565bde80"],["/posts/2697614349.html","1a776a9c9093e3e0d160c1891a214a42"],["/posts/2742438348.html","4634052916d1c35d90ac7c9ceae65f52"],["/posts/2768249503.html","4e0e0be5562de10cd3558183d7df84c9"],["/posts/2864584994.html","f770df2a57e379bede3b48301819a2f0"],["/posts/2888309600.html","a0e33fe45993f86639a1b8ddcb8c2330"],["/posts/2891591958.html","511f260ad082f9c2813de6efef720685"],["/posts/2909934084.html","e2f7443269e8c5106a63b7dae829d5b9"],["/posts/2920256992.html","1dea7ccf00be3ea39cc1bd8a9905d762"],["/posts/2959474469.html","ab0c46cb801cdb6fa23f6ff1c7d63983"],["/posts/3005926051.html","b53eb9f3cb5c24367730f6ca8d7ce181"],["/posts/309775400.html","24be45b54ca87f5bbe71c735edec30bd"],["/posts/3156194925.html","d2ebf62792210480fec539bf5c2b2c33"],["/posts/3169224211.html","55306fa2c8364e679486b167650b6310"],["/posts/3213899550.html","894a61ae9ab6c1f7e5c1993c72bf78d2"],["/posts/3259212833.html","045e4b90e9a81310e0d728ea2374cba2"],["/posts/3265658309.html","dfac6729dff9447c1d58d3e117d09d79"],["/posts/3266130344.html","d670c5668c82a1c9c5725fac55380d00"],["/posts/3292663995.html","e9ff9e0c567df196619c908eaf83cf75"],["/posts/3297135020.html","a9b0520a4d4ddaa61ff92b38b26a8cee"],["/posts/3306641566.html","39db0bcddd61411498cde8b42425e704"],["/posts/3312011324.html","862d992298f115c3e54f694d470df7fd"],["/posts/336911618.html","295abd951aa7922a40d533f503f918bd"],["/posts/3402121571.html","99cbace129dfe1738c804abf36c17576"],["/posts/3405577485.html","92c64f1444f703186bc417f680269140"],["/posts/3498516849.html","79ebdaf2f9673d12d41bd5ae24bd1577"],["/posts/350679531.html","41debb66b201452277d66f5edf5ad7b2"],["/posts/3513711414.html","83d37b0f338e283da1cc614311dac361"],["/posts/3523095624.html","69a1af460d037a2687f7905c481fb9b4"],["/posts/3546711884.html","4a9260716a74856e594763321c899d03"],["/posts/362397694.html","2fbaddab2032b3e356cdef5b5afc2bf4"],["/posts/3731385230.html","a5e880b647a1cedec9bab2690dd61360"],["/posts/3772089482.html","e6e4322c60339a9f8a46f841a026ffc0"],["/posts/386609427.html","44b56e20b2f743e75e424a7117bb5763"],["/posts/4044235327.html","a5dfcb241a04eb02642a0cc39cfaa4dd"],["/posts/4115971639.html","68dfd7e6320c48ee2c8f0f9f42e413ab"],["/posts/4130790367.html","900462ec5ee0acc3c8a32d6623cab7c6"],["/posts/4131986683.html","4f39648432f9b48d5f28c85a93801cbc"],["/posts/4177218757.html","4e364b9ac70c8b7c22ba101689f7e2cd"],["/posts/4192183953.html","a6e9621a10e0a6d980d360625baf05c3"],["/posts/4223662913.html","d4db8c46c399f3c1fae36815bdfa81b0"],["/posts/4261103898.html","6180953260c081e4902f2f3ad3798adf"],["/posts/4286605504.html","75b975cd65683e23aac3ecb7d36dd6a1"],["/posts/449089913.html","e51b404845c925b6b677fbecbbe56e29"],["/posts/469711973.html","abd03bcbe339c6ae54cf81bd9182364d"],["/posts/482495853.html","8a3ca8a827ff3feffba49b9cd6bae2d7"],["/posts/488247922.html","39b603442947d842a36e71901c7b3259"],["/posts/517302816.html","4ef9185c4414f7faa3b6dc7a2e5b434e"],["/posts/570165348.html","bd313f92789a62ada6cf8b227c5478af"],["/posts/595890772.html","518c5c037b33fc067a0d605235838026"],["/posts/67485572.html","9e289ea06f8b97e2ca263c1ec1b22c6b"],["/posts/694347442.html","3b5487902e407e0b278e03c96a86f287"],["/posts/707384687.html","077d1b2e42290530e2faf7b9cfbfce80"],["/posts/71180092.html","78d2cb98f1aeaa18fa8ea734f052962e"],["/posts/716459272.html","d309602dc04a6637f23ffaf5f604ba69"],["/posts/765481613.html","201b75d787763baab266551b2b43c1f1"],["/posts/778231993.html","d502aaf26649beb602fee0f26a27b5fe"],["/posts/795397410.html","b1662ddb3f1c147275daaf1a551ab0de"],["/posts/820223701.html","51846ae1234d4b3c52885363a3f55818"],["/posts/830372185.html","c9b38bc079ac327cfe2f5ab19ba3d11d"],["/posts/88294277.html","d70f073547628d6bbcaec85f16888e39"],["/posts/939963535.html","3f368bdea42dc8769887f70369e5ad05"],["/posts/983786067.html","31c3371dec5ec07903d06fe87155d78b"],["/sw-register.js","e694e8a877df8dd48f5b3dc6f6add727"],["/tags/C/index.html","ca8ea585dc9e8032e88e02f9c007703d"],["/tags/C/page/2/index.html","09dc8e088b03140df436923b42c31c6e"],["/tags/C/page/3/index.html","e09c8fa4c8eeb0f4011538ae6fb06f58"],["/tags/C/page/4/index.html","c83d64d43e408dc972fd7b12a108ec83"],["/tags/ETL/index.html","e1b08b3584c69d07483f95c733681f93"],["/tags/ElasticSearch/index.html","388f3e3cd92bcf4fd344c57118d4c4bf"],["/tags/GUI/index.html","94f3de95bb3ffe29367211081f39e7a8"],["/tags/HBase/index.html","3a9fdc7a29b08f3cf74f2e31c956dbe5"],["/tags/Hadoop/index.html","d44850202bcffceedf67145995cc72d4"],["/tags/Hadoop/page/2/index.html","2f8d6c00ff68c43eebf0552b00b3ccf9"],["/tags/Java/index.html","edf1ff94974e874e84fab01e64d8c3b0"],["/tags/Java后端/index.html","327070540b4d327c52fb2113da476645"],["/tags/Java后端/page/2/index.html","f1dac37f49d5a71e3fcd83c50c863977"],["/tags/Java基础/index.html","a9857bfb4c2f1b9da60f899f385bd68d"],["/tags/Java基础/page/2/index.html","8756ed5a397d1299a8598df14a8a23ea"],["/tags/Kettle/index.html","fbb2292fd82c97f37967ac4e6294a628"],["/tags/Kibana/index.html","cdc9d481a291dedeb191e258bdf4c9ee"],["/tags/Linux/index.html","6dac0fb136c4b7aaaaf1105a14a0f3be"],["/tags/Linux/page/2/index.html","16ad2bbe5c61cc4fe9f5703d006079f2"],["/tags/Linux/page/3/index.html","0d5c535adafd0e7b0bb27dcbf96f24d5"],["/tags/Mac/index.html","6a69707029c04f8e50ce3d6be3c6053b"],["/tags/Mac/page/2/index.html","5e53180a6f67c69a95eaa7bf7a06d7d6"],["/tags/Maven/index.html","09e1e62689e27d3c45cba3876eeaf25a"],["/tags/MySQL/index.html","f816ae001d0d11facff3d00df0bca57b"],["/tags/Python/index.html","6f2e6e8c3ed60bd10677d6c5ebe3cd48"],["/tags/Redis/index.html","de74f3bdbd7ca059c6d9d59b630eb6eb"],["/tags/R语言/index.html","d5de58c43f11266d2d5b10b8711d47a8"],["/tags/Spark/index.html","98b724128b3ea54250b2f425af8bb178"],["/tags/Ubuntu/index.html","7fef19cc0068832efefdbfeb08716bfe"],["/tags/Vue/index.html","4f09749d9d390eb2ec3a0dcb141cd9da"],["/tags/Windows/index.html","c8f28dc5e0c897b558679422fd057962"],["/tags/ZooKeeper/index.html","b73ecbd1eaddf5bb02d476304c51ec7e"],["/tags/bfs/index.html","d7ce5de0608cec7062df28cce60f1881"],["/tags/dfs/index.html","95cf23ba21be2d3d477a8288d413fbd7"],["/tags/folium/index.html","786391c5194711f1e3bd810c6dabfb96"],["/tags/git/index.html","ced2cb0bc2093f256594dbf8506a0b46"],["/tags/iPad找电子书/index.html","d4c20b5d0b698747899a7c0f87d52ee0"],["/tags/index.html","2d37cc4d9a47fb32f71df4b3d4a82214"],["/tags/latex/index.html","45f15c2b2fc9ea2ea0e50e1288ea3e62"],["/tags/中间件/index.html","0e0eede9d8543e26f35b991ef83208fc"],["/tags/二分查找/index.html","d630bb0a4acd920cc228cfcf4a415aaf"],["/tags/优化类/index.html","8f4e2ffeef46572b89f8890a96f5ed2d"],["/tags/前端/index.html","975662a0c77fd904bc914fd24e1a8759"],["/tags/前缀和与差分/index.html","0878906f47b11aafbfa4c7e26eacb41d"],["/tags/动态规划/index.html","f539e671f4cd311153be46fc306d6cbd"],["/tags/动态规划/page/2/index.html","ff770a7d30c6f7c8a382eddd57cea307"],["/tags/博客搭建/index.html","2f037db43fafb2f50979429576fcc2cb"],["/tags/图论/index.html","030325ac958d3bb40321e5e484e7e28b"],["/tags/大数据/index.html","f6ccdcbb4ea0b74758b9d57488cecb70"],["/tags/大数据/page/2/index.html","1c85220f4f06c730e2382650be514383"],["/tags/排序/index.html","4ffd83dccde8903fdcc4d2dd97ea0f26"],["/tags/操作系统/index.html","0e7f06e68b09d5b60a2ee3a83112aed6"],["/tags/数学建模/index.html","ac068b345c72a1cd118c5c41badd16ec"],["/tags/数据库/index.html","c4904351f7feff3622636e353263979f"],["/tags/数据结构和算法/index.html","67cecea1d0e75502ad745c4f3af519da"],["/tags/数据结构和算法/page/2/index.html","faadf2705abb1bf3d7bf77a749bcfbac"],["/tags/数据结构和算法/page/3/index.html","15cbdc9bf769c96925d2bc5dcf3b7de7"],["/tags/数据结构和算法/page/4/index.html","29191db0aa167d679388964f9ff8e627"],["/tags/数据结构和算法/page/5/index.html","7a59d730081f12695fe6b71408985d05"],["/tags/数组和字符串/index.html","746b8c539ea50ab14909b7a543ea0eae"],["/tags/数论/index.html","0141006e1b5c739833bca904bc317cf8"],["/tags/枚举类/index.html","6155196c16e746eafca4e27862fa4ee6"],["/tags/栈和队列/index.html","9020bd85b64a241ba5ae0e49a93b4837"],["/tags/树论/index.html","014a688445cca6ed4084a7b54a8e5806"],["/tags/测试/index.html","fa276b65dcfee05c94dc55144970919a"],["/tags/环境/index.html","3f710025c7121080ef35a38a626ddc52"],["/tags/环境变量/index.html","391a201f1fa94360e193c4d99a3c8dbc"],["/tags/绘图/index.html","9b10692ff1d638921c8ea2e1ddde0c80"],["/tags/编程工具/index.html","9f1495e718c41abb7d9db00fa848289d"],["/tags/编程环境/index.html","085654226f531f3d2c760a472941a5ae"],["/tags/网络编程/index.html","e572014de6869b80f5f3e2ebc0db7c2f"],["/tags/英语语法/index.html","6e41a952a439b6b91c08c167eae6cb94"],["/tags/计算机操作系统/index.html","84f45b58c79acb22a804a37f93768a99"],["/tags/论文/index.html","9ef09b27bc6a80d734a032595db41c58"],["/tags/资源下载/index.html","7561acc51a52960e56da8e32ef3dc406"],["/tags/链表/index.html","5226476d9211db71f813036ccd83b3a9"],["/tags/集合/index.html","3a2b1a3ced0bcb397f711c8f2b8f9100"],["/tags/集群/index.html","8e59e350eaa4f49b79e823d1d5f0d576"]];
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
