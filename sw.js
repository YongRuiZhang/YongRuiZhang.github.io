/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","5706a6132e998bf8c74341dc9a0ccd83"],["/about/index.html","5c839c18157030af4e17c3657f9bede2"],["/archives/2023/01/index.html","39bf9958587989e331b9245a94620240"],["/archives/2023/02/index.html","21d48f7366351e6fdce644fccbad2b09"],["/archives/2023/02/page/2/index.html","d18ab4ff20df09003b45a3050145d097"],["/archives/2023/03/index.html","32030aceb02c02541556bc3b953d56ce"],["/archives/2023/05/index.html","0df7b5f585280fb454071ed4de30af60"],["/archives/2023/index.html","d4ce28912cfbf7cd47587f507b28b0e4"],["/archives/2023/page/2/index.html","1cf832678fbd1fc35df7eae0641d7bbe"],["/archives/2023/page/3/index.html","8fe74c8a4206f594cb0a5f17dd64090a"],["/archives/2023/page/4/index.html","504bffcf52c9975d25ce9255892222af"],["/archives/index.html","96c23f7d473f832d9ba8b433f165b4db"],["/archives/page/2/index.html","48b02dddc328b2df4bc7384365ff2f7e"],["/archives/page/3/index.html","34eb1d5e4d65b93db3b230488be6b614"],["/archives/page/4/index.html","077f0f8386f1bbf07c548a1e80d4cc10"],["/categories/Java/index.html","6a5dc3ad2e406e27cadc5a42f0ac2f69"],["/categories/Java/后端/index.html","07c25a5a0ef6bf02bc26d5643ddbf13c"],["/categories/Java/基础/index.html","4db3bfae0c3a7081f6d44c6d51099d72"],["/categories/Java/基础/集合/index.html","a24c6604c82f602329dd1e4edc2971a8"],["/categories/Python/index.html","0494aeaa71f672df3ba09f690d5e2288"],["/categories/Python/编程环境/index.html","aacb7ded1fbe83205c8af2bbaea1b272"],["/categories/R语言/index.html","52ff97ff5b933ba50571ee7b7efb206b"],["/categories/R语言/编程环境/index.html","13f08920dc444413954c33306d45e28a"],["/categories/index.html","39a07bf053ecb7e3780d59d9fe7f1edd"],["/categories/中间件/index.html","7f4c0df0e2bfdc615d53c4a0a36892b8"],["/categories/大数据开发/ElasticSearch/index.html","bc0a5b85130043ba6ea12a359297ed92"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","b74abf7a2dc7210994d8764b335cf539"],["/categories/大数据开发/HBase/index.html","09c2ef5ea719ae10b8b87f1ccc0eee2f"],["/categories/大数据开发/HBase/学习笔记/index.html","e31fd75ec125edfab4cea91b2daa2032"],["/categories/大数据开发/HBase/环境搭建/index.html","3f96a785ace6b86f5c281ecb0a22f1b8"],["/categories/大数据开发/Hadoop/index.html","5e2b3903c64c73975416f4ce6943cc86"],["/categories/大数据开发/Hadoop/技术/index.html","bace6e2800314ebb19bb30b3dc9508a7"],["/categories/大数据开发/Hadoop/环境搭建/index.html","39493a35b19ddeca355d83f685e9ec54"],["/categories/大数据开发/Redis/index.html","d0c813764342634b3a09fadda6dc2494"],["/categories/大数据开发/Redis/技术/index.html","274bf9630fe5df43a883a12004fba5ad"],["/categories/大数据开发/Redis/环境搭建/index.html","71e8a7957852099651d394ab5ac93e92"],["/categories/大数据开发/Zookeeper/index.html","1233a06d4d06989da13e948193c93b15"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","c42d0b5f7b4e8b4cce87ebc8f8fc0b9a"],["/categories/大数据开发/index.html","efdb9676b6025c1d37c4a2552c62a855"],["/categories/操作系统/Linux/index.html","a78421b3dcdedbcae3a36753a409624b"],["/categories/操作系统/Mac/index.html","84e67e8639d3992430d8b2bec8eb91f3"],["/categories/操作系统/Windows/index.html","df763922f142459396f5fba753f90ed7"],["/categories/操作系统/index.html","726a1f0273354846b0789eb44993b5d2"],["/categories/数学建模/index.html","90735aea1db85e06c779e85345480bbf"],["/categories/数学建模/latex/index.html","d3cbfc18d42fe23ded23a03373063f88"],["/categories/数学建模/优化类/index.html","2949cae2f9873f5f873d36570bfe3a3a"],["/categories/数学建模/优化类/现代优化算法/index.html","8649c8735c068e4cdfe0f847c5642a15"],["/categories/数学建模/优化类/规划类/index.html","1bed55f8da956898e7a6b2c4d1df2c7f"],["/categories/数学建模/绘图/index.html","eb4c16d22290b17b96ed103d3d9c04eb"],["/categories/数据库/MySQL/index.html","835039c1a5851300cb70a32efc87a152"],["/categories/数据库/index.html","7b234f506be54f3197fc9ae38371fdc5"],["/categories/数据结构和算法/index.html","5010f0182c2b60994837d59ab1749bfd"],["/categories/数据结构和算法/page/2/index.html","6c98ceaac5c892112bd69a46fc6124f1"],["/categories/数据结构和算法/基本原理/bfs/index.html","5f8c70fb7fdb4f26b0aa63d9718cab5a"],["/categories/数据结构和算法/基本原理/dfs/index.html","34461e9299294d0b12c7b5f30ee09c15"],["/categories/数据结构和算法/基本原理/index.html","fa7cf08f86f7f2789466a68c6dee72aa"],["/categories/数据结构和算法/基本原理/动态规划/index.html","a8c5958a83fe3a3c6407febb1f88b1c3"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","330b04b4d51bd156d2d86761fdb2f775"],["/categories/数据结构和算法/基本原理/图论/index.html","d3dbeeb26e116b5bcd4f73094399cccc"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ed74a7d560566578be3a500d921a8658"],["/categories/数据结构和算法/基本原理/数论/index.html","b54112d1665ffeda16cae460b13cfe40"],["/categories/数据结构和算法/基本原理/树论/index.html","dceb541d37cf00c35f8260f5ed8d0e5b"],["/categories/数据结构和算法/基本原理/链表/index.html","3f367bc0818f97842299e462ad1344fb"],["/categories/数据结构和算法/算法题/index.html","82518a49b11797a50dc7479c1184ed98"],["/categories/数据结构和算法/算法题/二分查找/index.html","2859b5d956a80e40f484c93491fe09ff"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","37ef8bbb29375f4c4132ed80a7bf9436"],["/categories/数据结构和算法/算法题/动态规划/index.html","80bb0862fbfb902daa7d76ef8bc6abfc"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a41f6645f57c58f7c3950f4ec9a59fbd"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","4804d29afed6ea0ccac014dc8a063528"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","918f1dbfd9d631968f74bfbda99029a0"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","20280c9cdeb42864c2faa0df7f8aa5d0"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5f48de40efa84d1233a7df2c9314e89d"],["/categories/数据结构和算法/算法题/树论/index.html","383a71b400e59f8723db00293f836b8e"],["/categories/杂七杂八/index.html","78ba9285ab29f574d2f82ec5024511e7"],["/categories/杂七杂八/博客搭建/index.html","de023544b37ae4724274400d8ff4e94e"],["/categories/编程环境/index.html","b6b393fd87399dce161a66bfae04f51d"],["/categories/英语学习/index.html","fc4b5ce975831ed580093f6780405460"],["/categories/英语学习/英语语法/index.html","0d30a05b63f12b643e3a227260fb0881"],["/comments/index.html","77c92b49ef8b5436e9fa1f431ced05ed"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9795dee67760c55c0f649d2b01d405b9"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","42a10bc79d4c477b65c17e0123a2837f"],["/movies/index.html","df6257412de06c52c8db34e3e19d59ff"],["/music/index.html","4a0fd74eb1dd2c62f0e94c5ae6042f12"],["/page/2/index.html","d36a49819bd4db6827145f4c97f8e076"],["/page/3/index.html","e1a7c5c2903df3121e0ab53ad79d9bea"],["/page/4/index.html","adfa5df740274cef5f86ca60a27bc5b6"],["/page/5/index.html","d9be4ef7cc15ef016c809b5dc01adab1"],["/page/6/index.html","59180278d7360acb8e2429d12fa66b9c"],["/posts/1021360842.html","184e072d7c353753f827b63be8364566"],["/posts/1120620192.html","1208180df5d579cdc41fa3e863c4e8d9"],["/posts/1141628095.html","d4f6bcb3e1dbc1a8fa060264287d38d1"],["/posts/1168613674.html","ac22969d6997e1b8dfb92922d8fe7ece"],["/posts/1219920510.html","d1a78af299272dd62aac8acb930e80e0"],["/posts/1222166338.html","178c13feecfd68024b0fdc9f03535822"],["/posts/1259097482.html","ff6413ae3662bbddab2e43f9e0762681"],["/posts/1271036369.html","b13dba4d0c347eeb9626edd111b9536e"],["/posts/1312847445.html","85e1af8301cbff843e7a207b015808b7"],["/posts/135355774.html","cddcb38d62d2c6b6dd4ae91a83dcad68"],["/posts/1375344716.html","4e2ce0c643bc3ba3a82f5a71644a29ae"],["/posts/1388991698.html","2909e0c364b7e3772564662225e5ccc8"],["/posts/1410315814.html","5ece125a06e46a53d0faa29ff8001565"],["/posts/1452790229.html","62cd66bea8da9359eb1af312b82ea23f"],["/posts/1470079884.html","ffff24656b9808cbb2ca629372ba62a8"],["/posts/1470079885.html","398a172d357a37550b9c4ec6f418153a"],["/posts/1470079886.html","36a8ff7859297ebbe6615bdc1c33e2f8"],["/posts/1470079887.html","23cbf820d28bf66af1bbbaaf4615d189"],["/posts/1498536549.html","4c835f56626c1e320fe3d7fbbfe4d577"],["/posts/1547067935.html","9c0cd1ca6c94858c04b594458d1d21fc"],["/posts/1557866301.html","2849eb731dbda98aba976520a1a2d489"],["/posts/1571776361.html","5b981b9fef2a578f57949085456d6a8c"],["/posts/1605124548.html","a3116ceaa5f12dfe38332169b3da42aa"],["/posts/1633036852.html","74d79f3c5e1464f2ff339f874334e7a6"],["/posts/1765123828.html","a125d7dac41ab9dd315b8d9782c506ad"],["/posts/1767336200.html","d7c13ead426f6e4482051886ede8940d"],["/posts/1776114197.html","d5c732d0150837d47c21dbba64d04f93"],["/posts/1817748743.html","2f40982a69d1adbef0667e9b53451e16"],["/posts/1925125395.html","a6190cd53ad54c3cb4cd944acfa5a1a0"],["/posts/1966191251.html","21334ce1c84140660e136c7348ef77b2"],["/posts/1987617322.html","efd7d5067002bdcd13d4d6900578697a"],["/posts/1999788039.html","6df22416895f83e809dd1e033cd5c875"],["/posts/2075104059.html","9498c715bb1071d9a01db4c069b7261c"],["/posts/2087796737.html","3d091cb15adca9d10c7da2430dadddea"],["/posts/2106547339.html","7d7ea0a32405e25279b931ff52b257bf"],["/posts/2207806286.html","db9f0af5431aa2f38789c09afce4a545"],["/posts/2225903441.html","c676d43b68b0cfc4ff9fe6ca12040b7a"],["/posts/2265610284.html","e4c95f518874dd1c5e9d5b5255bf27bb"],["/posts/2281352001.html","1c16726a727b56de3340f225bfce8e34"],["/posts/2364755265.html","eb0d78e7cce1d992531e02036d8daec4"],["/posts/2414116852.html","a4ddb0c1cd6df19347a5f231acbba99b"],["/posts/2482902029.html","3e9be1b73fe6248953a168c0cfa07bf5"],["/posts/2495386210.html","2ad92ff4c84c3db0e24bc7ebf2c36c03"],["/posts/2516528882.html","4bc85a59c4cf076d80f0b90bdefe4487"],["/posts/2526659543.html","a79f45757fef76ab67c2eb8c3f4a82d3"],["/posts/2529807823.html","fd1594f46932f8f50120001565efd88b"],["/posts/2742438348.html","01fb6913cad0b59f12714c7de9c0dc1e"],["/posts/2888309600.html","d8b2781aa60952a2e8be4a7f82a09494"],["/posts/2891591958.html","d9cab1700caf6fc1d56fc447043c3bb8"],["/posts/2909934084.html","69c0ce37dc8d7a7d1ccb388feb5e546a"],["/posts/2920256992.html","6bb7be2989c02e6713e6bb495d17ad93"],["/posts/3005926051.html","3f0a7fba1cf406849f86c15913efad90"],["/posts/309775400.html","5537bc7ebeaa834e5d18c76ded1bcb3e"],["/posts/3156194925.html","fe4c561e8d8bed5cbe3c51e7eeb51e0a"],["/posts/3169224211.html","f1c08a982729ad468ca4c5b67df34075"],["/posts/3213899550.html","f5eb64d034a416eefc77511122697c96"],["/posts/3259212833.html","de6946d51f2f5d7f350c613e7ce5bee1"],["/posts/3266130344.html","0eba6db4677b1202126d387f85a1b960"],["/posts/3297135020.html","e621e46fc738bcfc8ad75e48be3157e3"],["/posts/3306641566.html","d8d2279048fa9d95e958ee94973a5956"],["/posts/3312011324.html","42f498dac834064206b3b9dcd058541d"],["/posts/336911618.html","2e368c3175a925f8cf6fa84cadf3f365"],["/posts/3402121571.html","9c4ae6e10090e4f15d383c51f9717be6"],["/posts/3405577485.html","84cfb097d93ce208210e4c5628b474b8"],["/posts/3498516849.html","4ac0ce235492f06818d5cffa0777bf62"],["/posts/3513711414.html","1631aa87d34a0ffc9d75c767554d9b35"],["/posts/3546711884.html","3095b3deef70ee71af3a21a52d28e33a"],["/posts/3731385230.html","6610766fbf919071ff62284be6dffb13"],["/posts/3772089482.html","adc28637475d287840ab2f315c76819c"],["/posts/386609427.html","f37cb9054f5f040ca6e497a4c010566a"],["/posts/4044235327.html","e0e7e1eff9601c495b3aa7a1ea68f804"],["/posts/4115971639.html","93c816e2ed85633523fcbe8b3091e6cd"],["/posts/4130790367.html","65590fb067b6a00560e31694ed2a24ce"],["/posts/4131986683.html","7a33a4bd7e7704faf17f5f68581c174a"],["/posts/4177218757.html","6b75fff7c9372cf319332932d2c8f3ba"],["/posts/4192183953.html","872493e50500fe19b37e28db1ad460be"],["/posts/4261103898.html","105e96c71059123c3dd62e672f4e7243"],["/posts/469711973.html","37303bb7075befbbdb42ac0bc53f23fb"],["/posts/482495853.html","95c5c4a05d29028e95fbd5bc529b37e5"],["/posts/488247922.html","6ecccf2d76e23b474beae6809ef8b76a"],["/posts/570165348.html","678f055d2a7e7d14acb2b0275aee89b9"],["/posts/595890772.html","758d689499d084b325cc4e4f24361d6d"],["/posts/694347442.html","97d824ccb5f48c1d58a9a8036a7b62f7"],["/posts/707384687.html","c42a5c3f0a11fae33e4e5a9e278b7eee"],["/posts/71180092.html","b6fccc37032dddcf597cc2f9a124ad51"],["/posts/716459272.html","86d8330cbec526fffb93bc7e74cbd262"],["/posts/778231993.html","77f9330aba96b9b302f55705bbd0aed9"],["/posts/795397410.html","60c81d69e81ca54ceb26db151fe7739d"],["/posts/820223701.html","8d2e88619563a4a4da31ed313f91d23c"],["/posts/830372185.html","ec559aca1cd612fe0477f427b99e3587"],["/posts/88294277.html","119b5099205df1f0fd546c86af13e8f4"],["/posts/939963535.html","3a9e7e3ebb4ec42e15fe8734b5dd9b8a"],["/posts/983786067.html","c9d1f1c84efed84cfa769b959a4626e9"],["/sw-register.js","5eca78535a631c7ab1d8472f5f27ad1a"],["/tags/C/index.html","841864b53891fb22654682e8fc5b9dc6"],["/tags/C/page/2/index.html","586d14c1734f66e3f74683e96ac0eed5"],["/tags/C/page/3/index.html","6b62ddde5b4b8ce240150faafb8b10e9"],["/tags/ElasticSearch/index.html","bb07ed39e2ad01f6cb3908d9990fb279"],["/tags/GUI/index.html","fb3becffc688c6f859a23471842705d0"],["/tags/HBase/index.html","f067fc7722169b0428301067b4a8c3a8"],["/tags/Hadoop/index.html","b94abea48ad8344f72f37a69e2eeeff6"],["/tags/Hadoop/page/2/index.html","f23acc1acc27df758a9e3911d92e0f38"],["/tags/Java/index.html","921260fc9b53acf4fb6f1b8486df7986"],["/tags/Java后端/index.html","2f88263184a322952ab19fbac979491c"],["/tags/Java后端/page/2/index.html","a383d8dfd6ff5adc8f171036b383d667"],["/tags/Java基础/index.html","d5c60b2332b514a323225be79489c653"],["/tags/Java基础/page/2/index.html","c3c72d6db3bd10e1afd05eaaf416668a"],["/tags/Kibana/index.html","cd19ba58bab7e50d630400b83082bace"],["/tags/Linux/index.html","6b2c0d6176a772250794db834fa410ed"],["/tags/Linux/page/2/index.html","01cca0bbc29131b8d6eb1232113d18d0"],["/tags/Linux/page/3/index.html","bb92b731b7fc0053838c88b0bb21c22b"],["/tags/Mac/index.html","76ab829edfbde83a19d6b2ef7c9aae17"],["/tags/Mac/page/2/index.html","2ff0e37951ca6e11141ffd6194b82357"],["/tags/Maven/index.html","da6888ef92380db23304f7e85d04feda"],["/tags/MySQL/index.html","60860f0629882b3fabc3192e8f15dbf9"],["/tags/Python/index.html","06f8273b7cec52867920a9b168511041"],["/tags/Redis/index.html","a020f9097d0ac4ad4b1f20324b8ed69d"],["/tags/R语言/index.html","38acd1315c5ab2c862b985460ec65974"],["/tags/Ubuntu/index.html","567cb30a68ab17037bf49493de2636ab"],["/tags/Windows/index.html","98f81dae98fe29b543eca737a3a1ef12"],["/tags/ZooKeeper/index.html","094061410b4184102ccb83844e87055d"],["/tags/bfs/index.html","d565c0d260660a9c92828047f029cfd6"],["/tags/dfs/index.html","5163c34650574c58ca303affe718cf55"],["/tags/folium/index.html","627b8f9ab0b815108a950fc312e3670d"],["/tags/git/index.html","b85342ee85276ca45f07782f3b0f6cb4"],["/tags/index.html","a8af03c22a6e06b5e8017f82a960fc95"],["/tags/latex/index.html","7a00e1b73ee7412dc4dec817e6e0f50d"],["/tags/中间件/index.html","28b86dad8c341fe8fc145dda591e8bc4"],["/tags/二分查找/index.html","039dd6de66ba7264c6e466fad689c284"],["/tags/优化类/index.html","12d76af39301838b36f7604a40a85337"],["/tags/前缀和与差分/index.html","0d1c27bea38634af9389935f8540dca2"],["/tags/动态规划/index.html","7b3776fd12506e9e50e23f4eacc215e1"],["/tags/动态规划/page/2/index.html","1724787fb5e9647e9721588bc9fc5bda"],["/tags/博客搭建/index.html","90b9b11a5b2bd72a895702c397551723"],["/tags/图论/index.html","4a716bbf228d83b0e4b892ec595468f5"],["/tags/大数据/index.html","4dd80b94c31d8473edf4602ebdd1824e"],["/tags/大数据/page/2/index.html","0286802bfddf1dc967e1f94a5376f126"],["/tags/操作系统/index.html","46e0b98ddfc1c572edb161d5a3dc606e"],["/tags/数学建模/index.html","b56b273252ace941ddc9a8a7612c76bd"],["/tags/数据库/index.html","97711afb854feba20d8ce440ceb212bd"],["/tags/数据结构和算法/index.html","862e1822b2687d27c69d1659cf9ef8cd"],["/tags/数据结构和算法/page/2/index.html","ef4c89cbf3cb6bedc0ad392d604bc3d9"],["/tags/数据结构和算法/page/3/index.html","1f2b016788cb28628f5bfa47c0f54cad"],["/tags/数组和字符串/index.html","99725b0f72efccc253761aa0e8fabf40"],["/tags/枚举类/index.html","964575937e0d4b12971d411cbd3425d3"],["/tags/栈和队列/index.html","f694ed6946a67688f9ac51ab46e046f2"],["/tags/树论/index.html","894cf7ccdbe3d351d5e73bee48707f7f"],["/tags/测试/index.html","f779d973edbc53886b4871a0d0521d40"],["/tags/环境/index.html","f1b0b3517a38d8f058677a94153ab57f"],["/tags/环境变量/index.html","ca87dd00907f705ea13c3646b95ad99f"],["/tags/绘图/index.html","f9393dd1e952d92dd94c539f8e0eccbb"],["/tags/编程环境/index.html","2f278b24b6347266eb38ca0f41b80b31"],["/tags/网络编程/index.html","0d81ffbccf418cd64bb4ef1bebad47a2"],["/tags/英语语法/index.html","19587483a2dd5869cb0645e633e12370"],["/tags/论文/index.html","2f2030a83674f2c8ebb2492a4fefe4a1"],["/tags/资源下载/index.html","347a9f1782a065d2a34d99c772d46def"],["/tags/链表/index.html","b77e952d0392419f2e475c593820af45"],["/tags/集合/index.html","4661163cb4e65d42212c2549e070b095"],["/tags/集群/index.html","17f76c57d2d42f0d6b98904e9b4a98e0"]];
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
