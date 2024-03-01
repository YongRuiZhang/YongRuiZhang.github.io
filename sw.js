/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","909655b935e788453cd4e0c823b347eb"],["/about/index.html","c35e03d5c28a3fd1df9423c73905be2c"],["/archives/2023/01/index.html","2fbbcf8977f1c798dc9d39c3334cf44c"],["/archives/2023/02/index.html","fb932a591b513d7f72d3623cb6a8c77d"],["/archives/2023/02/page/2/index.html","36aa876692226365bfc4f57ebb07a2cf"],["/archives/2023/02/page/3/index.html","bfebde2b1896aecd771a55c75ecd0dad"],["/archives/2023/03/index.html","96a3e39679098e6b8763684cc6d76d70"],["/archives/2023/05/index.html","a3e180c8071c3ad70c9629b03482e490"],["/archives/2023/06/index.html","8889ddfb151084da9ad4efa1489d21cb"],["/archives/2023/09/index.html","6690fdcde66796bfe0c02db89db708ef"],["/archives/2023/11/index.html","974b54296102ac5d1a7205c553d2a4ff"],["/archives/2023/12/index.html","33d98731512c3272e1ff12f6c6fa3c86"],["/archives/2023/index.html","76fb76d03600257349485995352b0587"],["/archives/2023/page/2/index.html","fb4018f0aa6ca564027ad6367a502aa5"],["/archives/2023/page/3/index.html","19ae9cd2320378ed3ccca4b85f65afce"],["/archives/2023/page/4/index.html","71c9fad218c7a8a5436e019b9ea6bcdf"],["/archives/2023/page/5/index.html","7004b7ad3f16d9ae11ded2c58d944bf7"],["/archives/2024/02/index.html","db8837b9874344d03da8e67bcea715c9"],["/archives/2024/index.html","3305fd806f5e6e9a826bb861bc61ee1c"],["/archives/index.html","049c813c0d3cbb86c624be56724d6565"],["/archives/page/2/index.html","d19223977a65a0cf42d27c8c92878418"],["/archives/page/3/index.html","411986f183b37d0ba4bc45be237f128c"],["/archives/page/4/index.html","a0d8c8438534a4bb0fc82c67dae3da90"],["/archives/page/5/index.html","1397678c9a5bd0110c5625644068902c"],["/baidu_verify_codeva-qQP2iZOMLX.html","f56496ddeefff9c4a93c7230ad101436"],["/categories/Java/index.html","692422e0e364697169827b95ca9114b2"],["/categories/Java/后端/index.html","67efcda92d2758dbe4923ec4a9f52779"],["/categories/Java/基础/index.html","191f261afea620c0895330f071d3e3ac"],["/categories/Java/基础/集合/index.html","ccde608d9bab26bc91f4792836427416"],["/categories/Python/index.html","758e03a7e99ef9f3987f8bb05f3c9223"],["/categories/Python/编程环境/index.html","405bdb4d17478228288d0fee22a91f5d"],["/categories/R语言/index.html","7eafb1d4b53cfac2dd22b7e2d25e0141"],["/categories/R语言/编程环境/index.html","9105464a55f57a5ecaf74a70e85e5f69"],["/categories/iPad/index.html","762995b2b48fc111689cbfbdb76c4f6f"],["/categories/index.html","18526a8130e1a4454262d12d6ef2f76e"],["/categories/中间件/index.html","5b4dc75cf2451345d9835e92c5a8e74b"],["/categories/前端/Vue/index.html","618b064f8e497c4d801de5e74e252f22"],["/categories/前端/index.html","8eab074bf20b4dcb3317ae30d40eeef7"],["/categories/大数据开发/ElasticSearch/index.html","e94b161b225414d044436e8147152fb8"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","1980add19745a8c1b4ea7e64ba809511"],["/categories/大数据开发/HBase/index.html","cb6f78d89a21e8f10438457ba00a98ba"],["/categories/大数据开发/HBase/学习笔记/index.html","821df4290d5ed2dbfa1b3a98559a26e6"],["/categories/大数据开发/HBase/环境搭建/index.html","47e1d1902e978da5ab22efa697f7a8e8"],["/categories/大数据开发/Hadoop/index.html","111f6175820089b8653ccc5bae486b6c"],["/categories/大数据开发/Hadoop/技术/index.html","0416cd60bd088e5b43a8ff9394cefd27"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3cb63f43c30f7162dcffbc70b202f9b4"],["/categories/大数据开发/Redis/index.html","c7dcff59ae11077c59595923b968d78b"],["/categories/大数据开发/Redis/技术/index.html","fa46c9c45a5f0627847aa199603b8a96"],["/categories/大数据开发/Redis/环境搭建/index.html","187721de3407913f1505115fc6cea073"],["/categories/大数据开发/Spark/index.html","917d381a69ec171d80b475975041c99f"],["/categories/大数据开发/Spark/环境搭建/index.html","cd1cd1f71db3e69ca1e23476e737a7a7"],["/categories/大数据开发/Zookeeper/index.html","f9e6e9bc3a6da714fae6b1af115d000d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b6dbe154af527ae9cd28a009acc21c71"],["/categories/大数据开发/index.html","aaeba8da6d9e17d737cb36392f7d90d3"],["/categories/学校课程/index.html","b1a4aa029245ae21248afec8db2e1831"],["/categories/学校课程/计算机操作系统/index.html","8d39c67e320088eba598430b9e9cc060"],["/categories/操作系统/Linux/index.html","0726fa87c0fb34dd0faa5804ebb10f79"],["/categories/操作系统/Mac/index.html","ec5c028f7ef02654856abdc81ceb4537"],["/categories/操作系统/Windows/index.html","6755df13bd09162f6bacce5c63ac73f9"],["/categories/操作系统/index.html","cbc31717bc4dffa2dfc582319005a17e"],["/categories/数学建模/index.html","500d1f389b46043d66dc8f808c4b7329"],["/categories/数学建模/latex/index.html","b0acf17721d0092af31149be3ba8284a"],["/categories/数学建模/优化类/index.html","d627ccef27d7f2853d75b5ea6ad995bd"],["/categories/数学建模/优化类/现代优化算法/index.html","1a9b45c47d9b5d54594f5761314d66fc"],["/categories/数学建模/优化类/规划类/index.html","f3c53e6e26242350da8f79246a6879fd"],["/categories/数学建模/绘图/index.html","db7108b08ae605e9b25d28a681248398"],["/categories/数据库/MySQL/index.html","2a39b4babf1e99913cf5bdfe6eacf000"],["/categories/数据库/index.html","30a0f722acf3fd37d73b47b252bbbe3f"],["/categories/数据结构和算法/index.html","29beaa2bbc4c3edca08732ec772b361a"],["/categories/数据结构和算法/page/2/index.html","92165013e32997ad8089dd9219c23c7c"],["/categories/数据结构和算法/基本原理/bfs/index.html","78182abcf4cfeac09c9b43a8244b5284"],["/categories/数据结构和算法/基本原理/dfs/index.html","717f684fda126b141cf55c5879a7460b"],["/categories/数据结构和算法/基本原理/index.html","ad6fbfb0b9d69653646dd2bd885c62ce"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","9e9ef5258aa97441a499859f4b49eed1"],["/categories/数据结构和算法/基本原理/动态规划/index.html","bd9dd3fb8b0d172156b1b8ae7cf11d83"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","01515b8566d6b58f17cc994f21135c56"],["/categories/数据结构和算法/基本原理/图论/index.html","426ef293f1708f2c9ea9d242d6e8d186"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","6fd139009c1e503ed5ea4a95443139a4"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","cdb4f34424efaee1536cbcddcad51fdb"],["/categories/数据结构和算法/基本原理/字符串/index.html","177bd1b4aa18971981cc9357576a673f"],["/categories/数据结构和算法/基本原理/宽度优先搜索算法/index.html","2115a5a59a3b2ed3c030b94b181efa3c"],["/categories/数据结构和算法/基本原理/排序/index.html","6c3658938820ec6f9ba37aac4a30df28"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","e42c4f41e4a650c34fc8b6068465e4ec"],["/categories/数据结构和算法/基本原理/深度优先搜索算法/index.html","745fc4975b92a329304f1f903387bb5e"],["/categories/数据结构和算法/基本原理/链表/index.html","f1714d99d7ebd3d4bc1475fba52c0839"],["/categories/数据结构和算法/算法题/index.html","ffdd3921d22de85995c7578791e13fac"],["/categories/数据结构和算法/算法题/二分查找/index.html","2ff4cbab201a664778f6ddbd50db8570"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","74ebbd0dd439fe33f12b62d902021d61"],["/categories/数据结构和算法/算法题/动态规划/index.html","40bff324e3b6827a2a8a31f37009cf87"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","ffd996f662c91a91e6d4dca271e1182a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","bb94a1ed15a05fa3137a4c1b9435d6f5"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","73e3281c469388adaf60f881a9f1b55c"],["/categories/数据结构和算法/算法题/图论/index.html","cca226469a5719e4d278aa7bcecc1b08"],["/categories/数据结构和算法/算法题/图论/树论/index.html","022957ee2ccfe6cefb4d6686e6245ecd"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","be5fddf6e3cfa2a6bfc3689e047023b9"],["/categories/数据结构和算法/算法题/数论/index.html","63878ef98826e35ddfb10eaf4bcf68f0"],["/categories/数据结构和算法/算法题/栈和队列/index.html","a731608d0233c278dc7757071b7fb43b"],["/categories/杂七杂八/index.html","e1e444c8600fc4f664b54b7f19c865bb"],["/categories/杂七杂八/博客搭建/index.html","5fcf48cb588a7f5435338b4dbc50f002"],["/categories/编程工具下载/index.html","24a2195a40e8af33f09f650c05f0fad1"],["/categories/编程环境/index.html","1b9efd8d70ff893c8a597365dfa2955f"],["/categories/编程环境/大数据/index.html","56f61ba8025c55403d3948f2c7329362"],["/categories/英语学习/index.html","1153f579afbc89011550826845ce2027"],["/categories/英语学习/英语语法/index.html","b8880a06d7d0cfd69556e493616f4176"],["/comments/index.html","03f813e1a5b424078d8104f37514738c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0cfcf040984286119ef4ed188fd0bfc4"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","0fbd29083058a249facd2f2d9c8d6d37"],["/movies/index.html","e67398226ce64fb3752ec9dfac6fcf0a"],["/music/index.html","e67b7ccb4732bece0a9b604c31fcb26c"],["/page/2/index.html","af753c5c8cee2becb02ad0f96b5c0220"],["/page/3/index.html","e39cfa17f132eab6f2f433fa4eb67f7f"],["/page/4/index.html","4ecb7df8182f3c69e820aa890e5a6d91"],["/page/5/index.html","e65a1d28cbbf28c00f1de5cfd33db6ab"],["/page/6/index.html","8cf0209f4cf902dfd299dfcb6fe722d9"],["/page/7/index.html","643f07b755af00eec848ea6f257c9a51"],["/posts/1021360842.html","a51db260124fe3eb3f43e9a6eba807b8"],["/posts/1120620192.html","e7b6c46bebaf43113dc7e1d5173c8262"],["/posts/1137707673.html","58e021914805d06cdb72bd9b75c1919c"],["/posts/1141628095.html","7465a0ec33d28636767a1b46b0441366"],["/posts/1168613674.html","bb6d8deabe4ca1f5ce8933e4d60f9d8a"],["/posts/1219920510.html","6a62f575e9ca07a22d9c2e93d47cd9c6"],["/posts/1222166338.html","0943a4c164465ef996dc7c1fd2431807"],["/posts/1259097482.html","cac004dc17b8dd05ec033b4512e6db7f"],["/posts/1271036369.html","0abdf683db8f934b5a0448ad8f87867a"],["/posts/1312847445.html","d64cb3934ad6ef9a562956ed6bfdbd65"],["/posts/135355774.html","89e5f46f42564c9c1a0675326cdef2c2"],["/posts/1375344716.html","313c830ae01d764c32f373b5ef2f48dc"],["/posts/1388991698.html","598dfc2146ffab1ea2dbbe304a85ce00"],["/posts/1410315814.html","e92ce2a3cfb7db6fe78daa257060202b"],["/posts/1452790229.html","fa823ab8f6541e40a8bc8b00ead5484c"],["/posts/1470079884.html","9625018f7f348e007e6dd92af83b55f8"],["/posts/1470079885.html","f740fbc671390a8cda36257e61c8e327"],["/posts/1470079886.html","3ffb1e8f235aab3f5c7cdc1fba3f7e31"],["/posts/1470079887.html","aaba973096a32947545f0bc9b63204cc"],["/posts/1498536549.html","f0a889f56faa8a58775e7b3fd04657c6"],["/posts/1539568593.html","a461529ebf5b267a34646b1a08b47bdf"],["/posts/1547067935.html","7bdd56f2c25ed294b2ca32dd58e899f0"],["/posts/1557866301.html","469ef498331cc39c0d785b6584193eae"],["/posts/1571776361.html","4fbbab5f3f4ad7db74c585134f45acde"],["/posts/1605124548.html","84bc4972da0b5439c65303ddc6466c60"],["/posts/1633036852.html","9b0d9c85f9541388a1c82fb67128ff99"],["/posts/1667740714.html","bb8d59f5ebdde4f08690205f47324a9c"],["/posts/1674202625.html","9e5e7352c105e0fc1e48a363fb49c49e"],["/posts/1765123828.html","b621cb2343173aac1cc2cce6884bdbde"],["/posts/1767336200.html","1399b067aa2e09c5536ef08fe1ac3195"],["/posts/1776114197.html","f2f5a637b6aa2d37f7b81c6678331d9a"],["/posts/1817748743.html","c699c4c6e5925f08e80deeb1364c9f93"],["/posts/1925125395.html","2d12012977eadfa9a174514561b8a298"],["/posts/1966191251.html","eaa2d4d4b5e4761a57fec618b2db643a"],["/posts/1987617322.html","94f2ec05b020b59d54b0e9143ff2d2e2"],["/posts/1999788039.html","775e94f31951a43daf7c465a229183d5"],["/posts/2007534187.html","5657f91ae19595c0aaf99ef6374e2490"],["/posts/2075104059.html","e71d1775ff3d4c472a3c9e17baaca73f"],["/posts/2087796737.html","7a0679609957dd863b6805f647fd0799"],["/posts/2106547339.html","accb46aa6d82272287e02f4eb680ae7f"],["/posts/2207806286.html","68905ff216507bcd0c08fe344819fc97"],["/posts/2225903441.html","ff59ff6ea27dab54b5630783bb11478a"],["/posts/2265610284.html","a8a1439b43888d35d96bef7fb08709d1"],["/posts/2281352001.html","6c0c6c965bf64286cdbc638e37cfeb94"],["/posts/2364755265.html","738365524509dee6c63fd7ac87910b50"],["/posts/2414116852.html","14e0fa7b3533e48abedd5c227e97a896"],["/posts/2421785022.html","e844bab476ea5e40ae8398a082c0dbcf"],["/posts/2482902029.html","858cc3d108bcbdc927f33be9efb8ba03"],["/posts/2495386210.html","ec9e0191dd46a67ae29359c518f4476f"],["/posts/2516528882.html","8c6ce37e9085c0003d6744804144eee1"],["/posts/2522177458.html","fb2bb0c1808fa9535f72415048ce54b2"],["/posts/2526659543.html","a64aec719c8f102c48e3ca6a9a4b9c2c"],["/posts/2529807823.html","b213dc8dae8dcb085be04045e7a33464"],["/posts/2592249117.html","327868e6202146c7adae3e49ee3d292c"],["/posts/2596601004.html","565b5df2f15093aef0718348387784a6"],["/posts/2697614349.html","e20c2cde51568d72080dc369116ffe8a"],["/posts/2742438348.html","3f335541c75fe8717c78d8f63f609183"],["/posts/2768249503.html","f58ed31e4dd95b7127bdc18fedc7d67c"],["/posts/2864584994.html","32a58df0a05e3e8e611ffb68547b4704"],["/posts/2888309600.html","68bac05a83d8593520cfbb33ad4cf9d5"],["/posts/2891591958.html","cc64daea6b6f2f947cdc412461737c7c"],["/posts/2909934084.html","64e13acd8e582cbf4f5a88e143309712"],["/posts/2920256992.html","5eb5b289702eff1be973617fb940c37a"],["/posts/2959474469.html","49935f2cc272dd81e35a0686d25fee32"],["/posts/3005926051.html","4c0bebf8ac236d974f534d1997e053a1"],["/posts/309775400.html","c2704256e984519fb9a02f5241921319"],["/posts/3156194925.html","4fe3d6d08a42bcb741a0bf2552b49afa"],["/posts/3169224211.html","705b10282f46e389f70b3ae4e1023bdc"],["/posts/3183912587.html","439d63cd9aff3f6fd7505129d969bc0c"],["/posts/3213899550.html","8ec053dfd32c320dcd8a2ac2b269a1a9"],["/posts/3259212833.html","9f98777fbd17b06f4726042d20a22e88"],["/posts/3265658309.html","049b5eae56148cfaa98b217e9c1aa191"],["/posts/3266130344.html","6d7bcb5f7ffe2d8ce93c5756abffce8d"],["/posts/3292663995.html","7f0a10e4671946775576d56c7043b467"],["/posts/3297135020.html","b4c8820fc65617f56fef38ea0c064b33"],["/posts/3306641566.html","8c6dbaa6d43ffd6f0be8d7859bb11f8f"],["/posts/3312011324.html","1e9b694a28b4a99ca3f7e45afdc1bce6"],["/posts/336911618.html","f82eb200acd2a7bbd8d9cefbf9ee4a86"],["/posts/3402121571.html","0e52e7996fcddbeb8e0fdc22074241b3"],["/posts/3405577485.html","759afcd1d9f882df7d585714d8207aa0"],["/posts/3498516849.html","c93cd170151b6cb8e93d4e818e76bf4f"],["/posts/350679531.html","299df956df2dead6cf99247f9ebed975"],["/posts/3513711414.html","a6e7bee270274b3a3b801bfb6146fbae"],["/posts/3523095624.html","8acc02177182d59177891f2bcc087de7"],["/posts/3546711884.html","e0f417122f525e3f9597f434d9486f31"],["/posts/362397694.html","a066e8bf1efae1c6c88a6c3ab5e8857f"],["/posts/3731385230.html","e2b52228ad5bb62d5427fa7d04eaa850"],["/posts/3772089482.html","97caa4e7bc79373d2c63bf6d201fea88"],["/posts/386609427.html","80ccdbbf78eaba252fbbd3409300253a"],["/posts/4044235327.html","a1be6ce5f3b712ad4dbaea7b6ce21077"],["/posts/4098221856.html","7f7949c4290ae76d4aba70d6ee83469d"],["/posts/4115971639.html","5572865a4de3a8a862613086484a8438"],["/posts/4130790367.html","a8d4b99b9a6f4a1983d07c1ae4208912"],["/posts/4131986683.html","f01e9e822c0e09dd07ea8aca00507a2a"],["/posts/4177218757.html","5a0f4bd3be7de04e64edfac2d9eef228"],["/posts/4192183953.html","0a7fa78d24fe4bcb65a5c20ed6697fcb"],["/posts/4223662913.html","74012ec2ca69d040031bd4f1e25a95ab"],["/posts/4261103898.html","ee94adce336b355f3bb35cf68229c29b"],["/posts/4286605504.html","bd194f03ae37798f148f2c0202f81455"],["/posts/449089913.html","3f56921a9f6b62fb38bf0e43813d79fd"],["/posts/469711973.html","b21b363d32415d4573c8e40a16402139"],["/posts/482495853.html","9de48f7f7a1982e22114751194c55405"],["/posts/488247922.html","6d8fcff4c47edb3a820b2d1c83bd994c"],["/posts/517302816.html","74df042dc8f153fdff1b726e6d4588ba"],["/posts/570165348.html","e906f31d5f8cb3a70a93b0d27662edf2"],["/posts/595890772.html","64fdbfac85ab609ba52d33e55302907c"],["/posts/67485572.html","cc2b3e3a42aa27ebd7f028bff3e0b403"],["/posts/694347442.html","2d97fd5f927e6ba65a7dba856d923888"],["/posts/707384687.html","950be09c197309852d7fb0f07da27506"],["/posts/71180092.html","c56dfcf212d3b0d7cc59b64f843d4d5a"],["/posts/716459272.html","1a5050edd8c60b23924c967ea73cfd70"],["/posts/765481613.html","2126b4d4ebfebe6f9e92b03f857205c8"],["/posts/778231993.html","042c951c0ee65302bb88da3fe3adb40e"],["/posts/795397410.html","7ca8c8e31738696670460225777c6c6e"],["/posts/820223701.html","1427e3d17b56acea82be22bec6334b56"],["/posts/830372185.html","2e4c1a756a817c51d0fb369f3f7d4dd7"],["/posts/88294277.html","93b7d2bd1928dac9643eb2956ad8d7de"],["/posts/939963535.html","1167d4635da3e7a7eafe68ddb98534bd"],["/posts/983786067.html","0f56711e288d5de1b510692162469f3f"],["/sw-register.js","04596a82f791f6b823583f7ab6bc7e17"],["/tags/C/index.html","c92a0fa7b1430be588bf212d59fc1fb6"],["/tags/C/page/2/index.html","f0d2aa000e6093d400341a82655c28a6"],["/tags/C/page/3/index.html","74ed0ba934365c681260a7cff5259d17"],["/tags/C/page/4/index.html","3bf462228bee0e0667cacfc558d17c98"],["/tags/ETL/index.html","7b9f7e72f8edab96c56efbf4ae7dd3a0"],["/tags/ElasticSearch/index.html","309cfde69b5960f8a24ff19aa8a6f161"],["/tags/GUI/index.html","62cf88fb5c3f814ff7daf610a0061607"],["/tags/HBase/index.html","d5f241617de159d74a0960bdcab1b8e4"],["/tags/Hadoop/index.html","5042f6d8e4a316bf09532f9e82c707d3"],["/tags/Hadoop/page/2/index.html","ea6a38f8a2f28926dc344cdeca5c8058"],["/tags/Java/index.html","9b16a54827c12af5313d937d32ada902"],["/tags/Java/page/2/index.html","078bb62bbb25bc8064be11e63d17da9a"],["/tags/Java后端/index.html","58d40b83e60863b84bca683dc1530b87"],["/tags/Java后端/page/2/index.html","924a47bad62f6be8d970ead2b5f4b70c"],["/tags/Kettle/index.html","ba2a242a0d6ba853a3b10672d76bb8f6"],["/tags/Kibana/index.html","90304683c83295c01c84f41f84eadd64"],["/tags/Linux/index.html","3bc435d3228b3d305ad96ae6858d611e"],["/tags/Linux/page/2/index.html","b3543b4c1cc36db3c503c3a9851abdfc"],["/tags/Linux/page/3/index.html","dcce593c92a6a182e42bdf547d52385e"],["/tags/Mac/index.html","50545659464327a25fdaee6378beda14"],["/tags/Mac/page/2/index.html","0d3c6408a90d8a2a843e909306b24b30"],["/tags/Maven/index.html","74d75cd0a52f509f8be76e39f8c9813a"],["/tags/MySQL/index.html","5c16efd9896051b5d86f43acb8a7d0cb"],["/tags/Python/index.html","700883c42481666ad05a9c03b5cbd9ab"],["/tags/Redis/index.html","aea26fcad9d2a14ab4266907a0c1e659"],["/tags/R语言/index.html","e0e386d1265cb50246fbfce66f983198"],["/tags/Spark/index.html","7bb79fa3eaf0d707d9891f7bdbce917b"],["/tags/Ubuntu/index.html","b974d46592b4c2cfb747262c4c5b3c55"],["/tags/Vue/index.html","65a6a6761f31240bce2015d17d36d8b4"],["/tags/Windows/index.html","cc1cf9fb3ecf9eee715ac8638a12e4fe"],["/tags/ZooKeeper/index.html","650dcd80ec9ef15fe5d08d83aace0505"],["/tags/bfs/index.html","dbe4fb4fc8f241b1f06563689896971c"],["/tags/dfs/index.html","da23f2dfbbf300ba05c581de97724860"],["/tags/folium/index.html","6287dd5b8893a10ff9f6a7acc0fc88e0"],["/tags/git/index.html","d05aca205cdd0df8e95d90440857d65d"],["/tags/iPad找电子书/index.html","c1b3c199a2630edb56718bece42d59bc"],["/tags/index.html","5cc2fa0806de12ac7c852c18652bd905"],["/tags/latex/index.html","07da74b0ef5a1a611aa2b63e9b83759f"],["/tags/中间件/index.html","c34a0338cabb8d8fe132c428ccbf5e1b"],["/tags/二分查找/index.html","b63d3e3910b9cfc6cc2c21c81f6b38a9"],["/tags/优化类/index.html","16bbc5789de6096b7843ea85b89c6229"],["/tags/前端/index.html","aab6567b0cf30d4a8c46282f9e9b6b28"],["/tags/前缀和与差分/index.html","9cc8f10546b9aece1bd419a0c046de4f"],["/tags/动态规划/index.html","325a667052fc5c8552c371ccfa762d84"],["/tags/动态规划/page/2/index.html","b7f5b6a1b7b229b1b1a1c202922c63a7"],["/tags/博客搭建/index.html","7a4a1a98e5aa95896aa74c4d5f1dfa57"],["/tags/图论/index.html","2f5e6eb2305c78acb50fbfc210708bc4"],["/tags/图论/page/2/index.html","796382da60f2cc2d5e48726b62641d9e"],["/tags/大数据/index.html","9313c72e4889ca61ee86a5c1570cc289"],["/tags/大数据/page/2/index.html","3d0cd2e40145ae2f5c6a6b61dd8d0966"],["/tags/宽度优先搜索算法/index.html","6cf1e867a492ae74dbae25a34531e16a"],["/tags/排序/index.html","4bb07ade1938850018f19188628ed94c"],["/tags/操作系统/index.html","6354daf35901a96e828126366979de77"],["/tags/数学建模/index.html","79efc35f873722cd0732d4665438e0fa"],["/tags/数据库/index.html","60b4a17c2bdb6d15d529b554803c8b44"],["/tags/数据结构和算法/index.html","0ff21827a6bb59a1b3c4ed8c97661447"],["/tags/数据结构和算法/page/2/index.html","d0e84fcb30e63332056cfdd6ddb51411"],["/tags/数据结构和算法/page/3/index.html","f58eef9962a676cf7bc6bdabf6bf6f77"],["/tags/数据结构和算法/page/4/index.html","c4ef3e993d660c1b1677f3ed5b1b4aee"],["/tags/数据结构和算法/page/5/index.html","072a7b7ace21aa6569b85594f9310f31"],["/tags/数组和字符串/index.html","28103641e507462bf4b3eccfac273fbd"],["/tags/数论/index.html","4350f7217159ca0f1becdc3dde6957dc"],["/tags/枚举类/index.html","b948aa14fb6299c2f0e0aa64284924e0"],["/tags/栈和队列/index.html","b7a926f24222caab3c4e83854e7d08ad"],["/tags/树论/index.html","7319da5be1af256ac3e76fbd5ba0479c"],["/tags/测试/index.html","541e7975b92b09d43365b9f065896fb8"],["/tags/深度优先搜索算法/index.html","1ec542a014a795eff47c57f7011cd8e5"],["/tags/环境/index.html","3289f8b754617133aebf566d83bd32a7"],["/tags/环境变量/index.html","b44aaf76b662a86e9a6c73dcd93c4736"],["/tags/绘图/index.html","c625e1811824c1ea7d63661dc44d71fb"],["/tags/编程工具/index.html","cd39f9fea0148b98959d590d20380909"],["/tags/编程环境/index.html","4224f8c63116a037e1b4951842c54d1a"],["/tags/网络编程/index.html","759016f2fd7d85e2c00ad0705d428b04"],["/tags/英语语法/index.html","40e36e1da4f9ae6175138a91ae42b962"],["/tags/计算机操作系统/index.html","8e728b915658f3c19f59b677c3891671"],["/tags/论文/index.html","275e1b264cdc39edc00afe04a18c998a"],["/tags/资源下载/index.html","3e97623a641c99a0a1483f44ab640468"],["/tags/链表/index.html","c03ff5e66bd457445a43c164d3d8143f"],["/tags/集合/index.html","33f1e49f8bf40eaff128e00873fa74b0"],["/tags/集群/index.html","4778e2575c82f28ae95261a0cfbb3786"]];
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
