/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","64f0368ccbfa63ace99b42b6f3933f22"],["/about/index.html","d391ffdff5982b08f1f276af4456a8ea"],["/archives/2023/01/index.html","bee3b7b1ce1f324cc50c6cdea4fa4857"],["/archives/2023/02/index.html","8aa1d4e0edc774a5fc048c04332ffd66"],["/archives/2023/02/page/2/index.html","a3f8f22e8efd79a775198513ea8e1e8a"],["/archives/2023/02/page/3/index.html","0473da7bd565d3a56db10176fa903ae1"],["/archives/2023/03/index.html","477a97c6871a0b9a317bef99b096d583"],["/archives/2023/05/index.html","ac4914d1c4719850f65f86cb7259a916"],["/archives/2023/06/index.html","57e61c3ce027f0883aeaf04a0bdcf524"],["/archives/2023/09/index.html","ad0f046c204016c47f631bc1a0166d01"],["/archives/2023/11/index.html","926046c3513e4b5c985d8362235e6de3"],["/archives/2023/12/index.html","1207b1f65d8b1dd6d330dfb27e46e7ca"],["/archives/2023/index.html","f18dfc28030ead622628575c6492b97d"],["/archives/2023/page/2/index.html","3df7d3fefb0a756d7a8acb651071fe00"],["/archives/2023/page/3/index.html","05ed276be1cc3424390a794bcc7dd43d"],["/archives/2023/page/4/index.html","8d833861cbef40ee294878ff89f9ed84"],["/archives/2023/page/5/index.html","07fd88bb1f45bc53ee5ae7469a4adcab"],["/archives/2024/02/index.html","bd25eba4aa28ee11428487b1b62c66fa"],["/archives/2024/index.html","858e5d50c60aa5c1e95cfae280cc6623"],["/archives/index.html","8cc377d3d8f23befc792f96df8961105"],["/archives/page/2/index.html","a0d0a88a4eff68ca0168a8cc709a1fc0"],["/archives/page/3/index.html","9be2c25d0213dd710579812ac484b466"],["/archives/page/4/index.html","dcb272df9a5a578333efd3709d9f7b55"],["/archives/page/5/index.html","0a9cb5b6bb9d5da555ef5dd3a34e8551"],["/baidu_verify_codeva-qQP2iZOMLX.html","1dede7a40f93e9349fa59f0e367aa1b5"],["/categories/Java/index.html","e13991a8dfdcec3176c28fd2b5fceec7"],["/categories/Java/后端/index.html","0df42ed2ca8504c7d122e0de4374144c"],["/categories/Java/基础/index.html","c649c79efa8185a54b252e074c078d8f"],["/categories/Java/基础/集合/index.html","b0c04e0c89d593e3cb7128f379eb23e1"],["/categories/Python/index.html","7f8e99a403dc3f81d660f65956eaaebd"],["/categories/Python/编程环境/index.html","c57dd41ec0346de4d1f0fcb9c11b6a23"],["/categories/R语言/index.html","da8424deb644cd45e0079c86db648bdb"],["/categories/R语言/编程环境/index.html","38118b1f06d580bfe34eaaa8ed205716"],["/categories/iPad/index.html","1557a58dc146ead91e2a22f16606d338"],["/categories/index.html","44a552252227c0a9628473da0d5c5189"],["/categories/中间件/index.html","b9a4dd71c38d75bec927b122ceaeb08e"],["/categories/前端/Vue/index.html","b3d83814496152fcda4280d6bad8f6e0"],["/categories/前端/index.html","1dfa9c8f6b9f29ca6d8a77859aaed798"],["/categories/大数据开发/ElasticSearch/index.html","7495222a718fa6651a8a995ae15119a2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","8e7c641bfae6150d63889c01320ad33b"],["/categories/大数据开发/HBase/index.html","cb92ba350b73a99cef7620476b280885"],["/categories/大数据开发/HBase/学习笔记/index.html","0f71a8ff9b4e84555d6ef3075d17acc1"],["/categories/大数据开发/HBase/环境搭建/index.html","3b92308f9a19958f9c5c322939d46393"],["/categories/大数据开发/Hadoop/index.html","a566ae8762cd0b9d30dc71fcc5de9120"],["/categories/大数据开发/Hadoop/技术/index.html","ec5ea42d343a453de2354098da5efcb9"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d1c7799052f2ac8988ad802e09fe9ad0"],["/categories/大数据开发/Redis/index.html","dd833ccb1ff61c9ad325a3f014fd6818"],["/categories/大数据开发/Redis/技术/index.html","07da07918cafa0bb1507668b7a092342"],["/categories/大数据开发/Redis/环境搭建/index.html","90326322360e5cb1b1cd412844c14b24"],["/categories/大数据开发/Spark/index.html","3cf25a2f780f9b4fb63e338ed8610d91"],["/categories/大数据开发/Spark/环境搭建/index.html","9606581f0af14fbb3504c4b09c80da18"],["/categories/大数据开发/Zookeeper/index.html","5048fe5349e3e8cb3ae08829e5887d25"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","2d2a428101355004ed9b1fb434bd13eb"],["/categories/大数据开发/index.html","7b5f0fa8631ada6bc38f471fc8790f77"],["/categories/学校课程/index.html","cbd01320bf1f44769bf7cc5f228587b2"],["/categories/学校课程/计算机操作系统/index.html","3d4b34fc34287c414b3ce147dc9b1e7a"],["/categories/操作系统/Linux/index.html","d70695efb9bb81b6028a772e086df0e8"],["/categories/操作系统/Mac/index.html","95bee7a7a4c214482129eab10f47fe58"],["/categories/操作系统/Windows/index.html","3c3b4b7d052b4ab0d339c57969cdd1b0"],["/categories/操作系统/index.html","0989a7109e7bc02ce5f67de040aff1c6"],["/categories/数学建模/index.html","782d0a148eac47b40b66eb9afa685675"],["/categories/数学建模/latex/index.html","680c07316a504ff2def8d81e298e9703"],["/categories/数学建模/优化类/index.html","4c6f60afb1d0d427773b1e37cfc30567"],["/categories/数学建模/优化类/现代优化算法/index.html","681c331ef5ac84a760c20b8103b3207d"],["/categories/数学建模/优化类/规划类/index.html","77b7bcf21f876e9ca032c4884d5c7bd7"],["/categories/数学建模/绘图/index.html","d2cc3b0e1026d914a34416fa8d7204ee"],["/categories/数据库/MySQL/index.html","0dd9617226f98ecb90a9942c9fce2f78"],["/categories/数据库/index.html","35c8bdc3f461b21c89274bee3382186c"],["/categories/数据结构和算法/index.html","3b0d6451062cc159f47270b3235bb61a"],["/categories/数据结构和算法/page/2/index.html","3305c0695b278ca4a07a03cec51864a2"],["/categories/数据结构和算法/基本原理/bfs/index.html","ac6dde453fb67d6a36ed5ac093b74f7e"],["/categories/数据结构和算法/基本原理/dfs/index.html","45c2b38ecda9a2a65060c41cffc19c63"],["/categories/数据结构和算法/基本原理/index.html","b395fe1a6f37134b936560af4d65705e"],["/categories/数据结构和算法/基本原理/动态规划/index.html","78d43537be2fc7dd71e54ccf93ab2b27"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","6041eba0d341d743b21cdb3993111270"],["/categories/数据结构和算法/基本原理/图论/index.html","8fe2a4194c4e1a828f25b051e0eb3cb2"],["/categories/数据结构和算法/基本原理/字符串/index.html","7aa9cd68199b6a5d7f53222844b9f522"],["/categories/数据结构和算法/基本原理/排序/index.html","3c342aa09a4e294f45cc0a5b6d738e6a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d379898e944466e609b68bca1a122acb"],["/categories/数据结构和算法/基本原理/数论/index.html","4ded88ba7d4627cb5caafd6caf783803"],["/categories/数据结构和算法/基本原理/树论/index.html","64ab44926b140277be1b96415c6f5973"],["/categories/数据结构和算法/基本原理/链表/index.html","b7d3f69cfdaebedcfb40844c929c71e6"],["/categories/数据结构和算法/算法题/index.html","e71d8444b44880c86f12d7fc0088c52b"],["/categories/数据结构和算法/算法题/二分查找/index.html","4f7351adbab9a8e4e971240d1441de10"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","551ed59e2529e1616de9405ec1c1511c"],["/categories/数据结构和算法/算法题/动态规划/index.html","52da5f8c49cf3092770f205e6bea85b4"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","835ead7e7c0a1690ecf0522f61b6c5ba"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","ab3d72ac0a48cb7e313d2c472184bd19"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4aae24693e94e55a3a0cf4d4c3b9c0cb"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","784049f5e7c5fa41a5064678e318a84c"],["/categories/数据结构和算法/算法题/数论/index.html","3498ed3427651e8ef2e8e8535f7ca5c5"],["/categories/数据结构和算法/算法题/栈和队列/index.html","87fe59f10680dca16ae99653bf65c5ee"],["/categories/数据结构和算法/算法题/树论/index.html","484f808046dd11dcdac7592ae92fc827"],["/categories/杂七杂八/index.html","59647db80e20d0889ff2faf0492115f4"],["/categories/杂七杂八/博客搭建/index.html","5bce8d36775bb1a1723fe9db5603b5f9"],["/categories/编程工具下载/index.html","a7c76d46402e8aa2ae37e7e706408f2f"],["/categories/编程环境/index.html","d11a9651a27465458a7e2bd8afba49c6"],["/categories/编程环境/大数据/index.html","e9739717cc1f00430ccf7a7510205606"],["/categories/英语学习/index.html","c31573f13f2e767f9036e1d86fd58b06"],["/categories/英语学习/英语语法/index.html","e2752bb6f4c252d67f5c216ebce5cd7a"],["/comments/index.html","fc3368e3788e2a96f72951006a5c569a"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","7b17f758c72fb6dca28ae5f8505bb272"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4afe2097c7d084ae12b68cebe9db923b"],["/movies/index.html","0b76dc56f7fe5482643f5a254f634fdb"],["/music/index.html","1d0d14b49618102df18cac522f9cb8b5"],["/page/2/index.html","62006992e8ecb5253c609557dd5b3056"],["/page/3/index.html","ed6ad6aa625d52201517b9ad3e6eba60"],["/page/4/index.html","a9464139dcc9f633ab69ee4401ada9ba"],["/page/5/index.html","b8029a03010e4519284c53a940260be5"],["/page/6/index.html","020d29cb3429f5fbf206609cc8e5f6c3"],["/page/7/index.html","49383a85bc64b94ccc187b548e838cf8"],["/posts/1021360842.html","b1b2d01c453a6f32766981bae02ded54"],["/posts/1120620192.html","02866a174514c43a388b4495ca68e727"],["/posts/1137707673.html","a854b912f29d0a370c49e24174293792"],["/posts/1141628095.html","0256261272000b5693a23fa5cb5358e1"],["/posts/1168613674.html","a7d5aecbd8476cbca54dd1ad27136e11"],["/posts/1219920510.html","35d9666dd15919cb0fc3d7bd54adc1d5"],["/posts/1222166338.html","e4473a17ea4498ae962be2de0a23396b"],["/posts/1259097482.html","6c145084dd14757e5789ee0f5a898d84"],["/posts/1271036369.html","c71a25da56acfaa1ca2a54e42518825b"],["/posts/1312847445.html","6e8d97be665ee30a8b130bc0b812b7e5"],["/posts/135355774.html","e7584495a0048b6d8e97b1c824e04a37"],["/posts/1375344716.html","00597c368dd76a3306398536997a89ca"],["/posts/1388991698.html","e5e95c7ef8518ff97936431eabe61b40"],["/posts/1410315814.html","19e552cc3a7956bbd4c0382454f6c3ec"],["/posts/1452790229.html","e3abe5110ed1ad2a45514fb0e61b2107"],["/posts/1470079884.html","d79f7682005fa9eb9cd894b007aeb807"],["/posts/1470079885.html","2b1f762dd59041dcff20751b320ec103"],["/posts/1470079886.html","58f87674acd8cf7153fe11d70970ae7c"],["/posts/1470079887.html","bd1cbec740a98942dc58ca53b58c75b3"],["/posts/1498536549.html","86d1e71796efaaccb5f9770240fa9b9c"],["/posts/1539568593.html","fea3a8f42e2e8881cd9226ce09c5950d"],["/posts/1547067935.html","939e8eb7f7f4553e8daa01d0e82ea4a3"],["/posts/1557866301.html","4b1a784b6f6bc3e9ae0028fa12272987"],["/posts/1571776361.html","8848563b46cc691217b3cad3c61dc3fa"],["/posts/1605124548.html","777f52464e0c621a6212f0d2a3ebb46b"],["/posts/1633036852.html","b4cdfb653993f399fd40615de82f6665"],["/posts/1667740714.html","a3cc300505f62e8fdbd4f3dd2c4ac5ef"],["/posts/1674202625.html","78e4cf9c1a2f39f3683870d32243129b"],["/posts/1765123828.html","0b95bd7f52cdf02520f9a6450e1d86e8"],["/posts/1767336200.html","4f126ba288ea543fdfc7a413c91e1f1d"],["/posts/1776114197.html","2adc795a49771bf1c715dcc2c88b2efb"],["/posts/1817748743.html","94793576cb14e557364c65f5c1b7bcfd"],["/posts/1925125395.html","84361210f65a33b93b2af4f6eddab156"],["/posts/1966191251.html","fb7e2b0b78c266862485f353ccf03fa5"],["/posts/1987617322.html","120d18b2b59292846be82842fdcea371"],["/posts/1999788039.html","b7c51d61db293f59ea32568a0a23253e"],["/posts/2075104059.html","2525ca70a7b4bce67b413be20b957db2"],["/posts/2087796737.html","427aced6ebd0846851434286c5bebde5"],["/posts/2106547339.html","72d8ff76a45621b81105332896b441be"],["/posts/2207806286.html","eb65a29f9a728598de1c83a6a1983567"],["/posts/2225903441.html","8800d173587218dfbf318778be90828f"],["/posts/2265610284.html","3ce1bffaba3e86aa8a66b45024636ee4"],["/posts/2281352001.html","15731d08a07821bfb22eda827f531a9c"],["/posts/2364755265.html","fcd5f77f7bb4a7351f9b9a71100e0c73"],["/posts/2414116852.html","5746b1766abea1302039a6702ab3d9dc"],["/posts/2421785022.html","cc07bbc1e5acc4fbacaa202f5b7138b3"],["/posts/2482902029.html","5c3a7b3c15eb1eba52f34944befc5c7e"],["/posts/2495386210.html","7da51bb8e5b838d153ad3ba1b3108595"],["/posts/2516528882.html","0495713873cc3d247bc7a8e04e159e06"],["/posts/2522177458.html","d85324ac4cc64ca1a01fb038b113ff96"],["/posts/2526659543.html","8c92a6ae24d771354ae894371f7e56d9"],["/posts/2529807823.html","be56ea9adfda2f09ef52ae90090c0a5e"],["/posts/2596601004.html","e6f1e8b1c0ab69d56217a3860789070f"],["/posts/2697614349.html","f6b041646efc5002980e2db6855c771f"],["/posts/2742438348.html","92202297e47aa3e7ecf16ad934bc8baf"],["/posts/2768249503.html","d96eb4a7fb34fc23b61617f12cc6a819"],["/posts/2864584994.html","9f71369cc49a1c4c750ebef06d8b724a"],["/posts/2888309600.html","fe32afd60180e704c4b00687359441bf"],["/posts/2891591958.html","b41bdab62a2131555a181cbdb9fe1ef2"],["/posts/2909934084.html","31ae49a5f451f078e9742de1998f6adb"],["/posts/2920256992.html","8ab86858ea1a708d1f29d23740ea20de"],["/posts/2959474469.html","1121212695e92257ed8aa62d99989280"],["/posts/3005926051.html","4021dc0eda6213296f3ebe076adf5fed"],["/posts/309775400.html","89ec42eafaeeb9a918bac6a1599f9184"],["/posts/3156194925.html","9cbbbd2b853afb6c3eb2dd9effb1d8c0"],["/posts/3169224211.html","8c9bda28b68c4927455518ebf9eff455"],["/posts/3213899550.html","c4e956827e5845cdd0ef12fdec72edef"],["/posts/3259212833.html","670021d4380769d404433106442bb51a"],["/posts/3265658309.html","004b9ec41bace18707d91dafc46814a0"],["/posts/3266130344.html","e187b376c23c2a86d3c3bf3b2edd4c44"],["/posts/3292663995.html","829d6100f1dc323aa186d469d951b3a1"],["/posts/3297135020.html","17746438828d3c44843383fa42e6436a"],["/posts/3306641566.html","8dff76511deff7cf169cdfe403794941"],["/posts/3312011324.html","b33ec288ae0ea454bea35b651c2619ed"],["/posts/336911618.html","50cb4da1dd5845b93a7eac4038643419"],["/posts/3402121571.html","1af5b898402c89d5e3f606077c64744c"],["/posts/3405577485.html","b1a6ece526d34aeff86af38887b07a56"],["/posts/3498516849.html","6630cd927952661613d08381ceaf1a93"],["/posts/350679531.html","e0f5310dcb940ba820641ae9e1ed78f4"],["/posts/3513711414.html","be24c0afe9d47515784fff1f66580f35"],["/posts/3523095624.html","2765886dd1c366557a600031bc8e09c9"],["/posts/3546711884.html","79c68342e0ce17acfcff993189a2ea07"],["/posts/362397694.html","46ec3385857b09b72c743ce5279c547d"],["/posts/3731385230.html","32784e4f5e6b58728da7d590644856ae"],["/posts/3772089482.html","a58e8a0fb43a36351ad97400dfde7879"],["/posts/386609427.html","865a15bcb6db53971c4dc06349430adb"],["/posts/4044235327.html","0369a57560566bdde1d967ff3f7e2ccb"],["/posts/4115971639.html","d9404727b5e2e571b053d202b38872ab"],["/posts/4130790367.html","237e36e6598e909608e6a8980265e82d"],["/posts/4131986683.html","94a42f9459ce025ff11ada15d26fd0a6"],["/posts/4177218757.html","254c68dfdb53c7cb74e481197366f05d"],["/posts/4192183953.html","8399367d06092bcd0505d855d4986726"],["/posts/4223662913.html","334119a91dd8da198faf1c3e129e2772"],["/posts/4261103898.html","2491b0a750543b762ff3c437e2663529"],["/posts/4286605504.html","8352e311d4dd3b7b43b09c24dd8de94f"],["/posts/449089913.html","2881673afe61fe0174f73f24ac89bc7f"],["/posts/469711973.html","90538898de991df66221b52bae5ab250"],["/posts/482495853.html","1f7a322df51fd0f4fd38b5f05de6990b"],["/posts/488247922.html","fbcf11d4409e808a718b3842e23ab8ad"],["/posts/517302816.html","45a743557aa507e09c14a2239e43cfe7"],["/posts/570165348.html","d42ad5ab18bcd02704ddfd5c31ae24f0"],["/posts/595890772.html","90fe62ec43b030a101149647bac81cb4"],["/posts/67485572.html","5d6c84553b0b0897675e2e5770633326"],["/posts/694347442.html","f7395897b5bc160d4611d5d35e74d45d"],["/posts/707384687.html","894de933978e0b3e962e52f84fccdc5c"],["/posts/71180092.html","97f8b678408fe7fdc95208f121dac447"],["/posts/716459272.html","084939194f583d3d9f0f954aebffadb1"],["/posts/765481613.html","877631c5445fdc4ff0b1d2aa6bff7ae6"],["/posts/778231993.html","406e1801dbf62cc56f05cfa8e469f947"],["/posts/795397410.html","ef6b8b328433a6e0a42b96fafb503cae"],["/posts/820223701.html","65cc2855859e584313799ae85a592e5f"],["/posts/830372185.html","5f8c6d454a3add1747aad9c2f2cbeece"],["/posts/88294277.html","867f1040007be0846fe4e2dde35df12a"],["/posts/939963535.html","9d48a4abdc534b887537d3110cad30ff"],["/posts/983786067.html","ae6253a7a89a36691a1ec263109a7b44"],["/sw-register.js","fbd6faa702197a9d87b8b01988d1d58a"],["/tags/C/index.html","047bd88034cb0e18769f89da565a36b1"],["/tags/C/page/2/index.html","a14ec3caa1d006ef4de94787cac2c585"],["/tags/C/page/3/index.html","3dedd80020b3109c4e32fb58419850c1"],["/tags/C/page/4/index.html","8482b06a63fddc38cd49b84977fdd35e"],["/tags/ETL/index.html","72565008b9963a0959133788f0dddee9"],["/tags/ElasticSearch/index.html","6532956c0c8fc353b3d396785d7ed085"],["/tags/GUI/index.html","9638e47f5262f0ddfa56855925f7072d"],["/tags/HBase/index.html","b433949e4a995d3228c2db04d8f0c0ff"],["/tags/Hadoop/index.html","69cbc392b0c4f1245a5973eb1ef38495"],["/tags/Hadoop/page/2/index.html","0db6375ad2adb0a461bf98ba0b85c470"],["/tags/Java/index.html","2efce1c6c9dc0763ba7c28d47a387a9d"],["/tags/Java后端/index.html","00a520358691aa11f90fba720fea1469"],["/tags/Java后端/page/2/index.html","ca519787712fabd4c18f557d10713703"],["/tags/Java基础/index.html","5c6d564cd51b646d8496e05bf6b316cc"],["/tags/Java基础/page/2/index.html","ea4a955ec4d5005882b4f8b3356b8a45"],["/tags/Kettle/index.html","cf08b7416bbcd1e01b748969565c37e7"],["/tags/Kibana/index.html","cb28d5a6be8fa635a1aeee4a94cef860"],["/tags/Linux/index.html","ce0a6443a0c911a19f07ba84d2e53efc"],["/tags/Linux/page/2/index.html","bc151a4c7daaee3961ed845181aeebb9"],["/tags/Linux/page/3/index.html","9b3e3c04531f9999a2f5554497f97bee"],["/tags/Mac/index.html","23cadbd5f2688e1b117cf63f690c3820"],["/tags/Mac/page/2/index.html","a437fcc4efa590501fb7372d1d287a58"],["/tags/Maven/index.html","07e6ea63d0f748804a67db0d62fd649c"],["/tags/MySQL/index.html","f8b6aea98369a875618345af2babf103"],["/tags/Python/index.html","73f301deee223fc18d6b8962d8e341c4"],["/tags/Redis/index.html","f91e9292bd93bdafd757ec8143a6c0a0"],["/tags/R语言/index.html","b65f9ed951c69be1a138ce8637f992a5"],["/tags/Spark/index.html","54519033e9da5200fdf3aef585c9108e"],["/tags/Ubuntu/index.html","dc15f2c779871012433f6c6bf5230207"],["/tags/Vue/index.html","a63c28dbbea46b838fc1eac25b396992"],["/tags/Windows/index.html","770e0a54c749c8d55532c8da715e8162"],["/tags/ZooKeeper/index.html","bae45f44fc69d0c51c33135e87bc0d8c"],["/tags/bfs/index.html","d564f28c4cd990c2628acfe95721730a"],["/tags/dfs/index.html","688711de268b52084c2e1b41b11919b0"],["/tags/folium/index.html","5ae23d0265ddb06671b1e415ec05c326"],["/tags/git/index.html","5e1b2dc3f52e42bba153cc268164ed33"],["/tags/iPad找电子书/index.html","d2739ab6e6bd16fcd6e94935afdafe9a"],["/tags/index.html","02f90eeb6c46b2101cccd9962a093c9f"],["/tags/latex/index.html","899ebaa3ffa5ba75b2054013655ebf5f"],["/tags/中间件/index.html","d7d8e34c8f2cce8758ae31cd5666b343"],["/tags/二分查找/index.html","12692620ddb7460872339b010a19f95d"],["/tags/优化类/index.html","4fcc06602bd625d63f3a74596c904887"],["/tags/前端/index.html","fd340fad05d15e4595af3f8a77fa19fc"],["/tags/前缀和与差分/index.html","3648ae472b432645e6a6fc20d78e5266"],["/tags/动态规划/index.html","e9b639639130397fbe05d08eb298ebe3"],["/tags/动态规划/page/2/index.html","6dad55814ce8b036c8cd0c8ee24d00c8"],["/tags/博客搭建/index.html","5117adae51657465a7e1157ec73819f2"],["/tags/图论/index.html","3baaf3384dcb7b80cfb473db12d3d30b"],["/tags/大数据/index.html","fe2f5ad19d1c59e79499cd66cb0f9864"],["/tags/大数据/page/2/index.html","79d838499ab05e7782cb6fd249a62786"],["/tags/排序/index.html","47769c3a9d8e2886a52dbac172b74b3b"],["/tags/操作系统/index.html","5e771759dbb2a66cc7c6a7edb0f69e9b"],["/tags/数学建模/index.html","ec05d8e52ae19eed81bfc60d8a10ff54"],["/tags/数据库/index.html","0a90e11de8c3929b8022bd9b328d6d26"],["/tags/数据结构和算法/index.html","00c84c4f4839d73a847b5d9f42e4c83b"],["/tags/数据结构和算法/page/2/index.html","cb457fbc376668bcfa087ba9262ddbf5"],["/tags/数据结构和算法/page/3/index.html","1e06620324c20ddaa9e9ed3e9e63e5f8"],["/tags/数据结构和算法/page/4/index.html","3ff4fede8f9ce760be7c58984f586feb"],["/tags/数据结构和算法/page/5/index.html","995a68a929172dec39af99613c4e2c69"],["/tags/数组和字符串/index.html","b2093f539fb4ff59d3ac3ee4354075f8"],["/tags/数论/index.html","69c2be48dcacb16e6e86d2979657260a"],["/tags/枚举类/index.html","4df5cd64ec149c0fa6a0f9ea17369101"],["/tags/栈和队列/index.html","d5cbec9bd9908e4fe22caa121cf98276"],["/tags/树论/index.html","a46524348f465eb123a21238fad97f19"],["/tags/测试/index.html","b412d4a99e37467ad2c6357bc819a2af"],["/tags/环境/index.html","413e443f3b01726aa0d407567189af95"],["/tags/环境变量/index.html","5a456cab4fbec2849d2d84a73200c7ef"],["/tags/绘图/index.html","7b3cf6adc0ce6dee78ec08841795be16"],["/tags/编程工具/index.html","6868c3660087d4dc37ed8a8f782ef636"],["/tags/编程环境/index.html","3770bf3d9d66d223042c197e5f24704a"],["/tags/网络编程/index.html","7f78b799c0c4e2fc0c7adc9d9702c211"],["/tags/英语语法/index.html","36b03dad69eeaf24bedf2cbbbb550b5f"],["/tags/计算机操作系统/index.html","2ed727becce4ce54c85c74063da18cac"],["/tags/论文/index.html","0cdab269210c18a77d5e9988aee19f9e"],["/tags/资源下载/index.html","c44770611c81c497f0f7b86f77d4ca48"],["/tags/链表/index.html","55569a2bcce48e4492bc482b25d356aa"],["/tags/集合/index.html","dcfd449a94b395e32b759a9d17444eb6"],["/tags/集群/index.html","9803eadcfa7dcd50e086963aae5dd3f7"]];
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
