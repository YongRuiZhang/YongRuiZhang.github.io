/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e522fe3421ffce78d7283ccce3a1c49e"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","29b28d6a92ca866ba3cd1c72863ac370"],["/archives/2023/02/index.html","757903ad56cc43bd0207fe3a331b0740"],["/archives/2023/02/page/2/index.html","1af410e2ae57848158a5a22693236107"],["/archives/2023/02/page/3/index.html","e54b9918821fe8953feac3a1f2f621a5"],["/archives/2023/03/index.html","03c18a6f95966ad1057341b56618efe1"],["/archives/2023/05/index.html","4151a03d17c0f6cf3d44ce9e28907991"],["/archives/2023/06/index.html","2c4f5c011cd92e4bc7e88af9dc83be0f"],["/archives/2023/09/index.html","649c026b1dac5c31a2ea154c6ee7d62c"],["/archives/2023/11/index.html","ca9936eb23a037ac87747ea53aa53e2b"],["/archives/2023/12/index.html","946a6e5a57c8bbbce8a647eeb0873b19"],["/archives/2023/index.html","7b554f447dde9050513b068f59ff2b77"],["/archives/2023/page/2/index.html","15c0598a5d2a630d398911d17e834673"],["/archives/2023/page/3/index.html","feefe4f4fb9fdfa5f4c8e337fae8fc15"],["/archives/2023/page/4/index.html","0eaea2f86ff9e32daf0a78bba7fb7e23"],["/archives/2023/page/5/index.html","772e12b7638b39c3e91c0fc11cb962f5"],["/archives/2024/02/index.html","7f22ec39d8e8fdc1af01a31e2d456d4e"],["/archives/2024/index.html","19158e5c6335cadd8624e5851cd6bf20"],["/archives/index.html","68695a20d01154c958f4de8b8325953e"],["/archives/page/2/index.html","e98e6e0da820d93d3c606031c22725f9"],["/archives/page/3/index.html","a341150a14135e2662ea2b70a0cb4685"],["/archives/page/4/index.html","b05557b8065521f736612780a749c9ed"],["/archives/page/5/index.html","68a74e5953d57015165bc12234aeb580"],["/baidu_verify_codeva-qQP2iZOMLX.html","27233ddc3c9951a2fb8fd03510216ce9"],["/categories/Java/index.html","c8ab5e3c0d36a30e8770bc4f68f57b2c"],["/categories/Java/后端/index.html","00e99b4e652658849664323e18da5ae9"],["/categories/Java/基础/index.html","309761cac54d7dd9cb59314f01e4aaca"],["/categories/Java/基础/集合/index.html","cd2de0714933853b23821c143c13b35a"],["/categories/Python/index.html","bdb60832ce6eb80a9ea0e8db3c41bebc"],["/categories/Python/编程环境/index.html","2668e16273d074cb8f0312d61ae8f200"],["/categories/R语言/index.html","e1369496e6f94e066d4551fcc1c6b50c"],["/categories/R语言/编程环境/index.html","9125fdfe89eba41528c40ca7f7b83d8e"],["/categories/iPad/index.html","4caf1dd53817a200de9e1bc6bc6e5bc0"],["/categories/index.html","b11a90e0c34c50f8858b32d0fd86ce5a"],["/categories/中间件/index.html","a5ba65f8cf9482f29f1ab2339c528325"],["/categories/前端/Vue/index.html","b62b9588a5bb9b0e36850a67f5bd0eb3"],["/categories/前端/index.html","1a7117e0e10d1119315b972cc623a937"],["/categories/大数据开发/ElasticSearch/index.html","5e714618d3ef171294e4416e649d9db1"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","98df9daad6f43a84c860791658bf28f9"],["/categories/大数据开发/HBase/index.html","ea641c0adf59b9ec6f0953afce9487c1"],["/categories/大数据开发/HBase/学习笔记/index.html","c21568d335b48b51a23e2105d88ba3c5"],["/categories/大数据开发/HBase/环境搭建/index.html","4f385a338ba0ca9527709fc32ee3b7df"],["/categories/大数据开发/Hadoop/index.html","c26c15222401032fbc924c2988ce3ea3"],["/categories/大数据开发/Hadoop/技术/index.html","0e71a767a0aa9cd22208b7613cdaae60"],["/categories/大数据开发/Hadoop/环境搭建/index.html","47e2c5d3ef2642428473167c191c2d0a"],["/categories/大数据开发/Redis/index.html","05078372ee63fc0e1551095293ccbf16"],["/categories/大数据开发/Redis/技术/index.html","1228d21f6a1ee5ba97735a9af92b39a1"],["/categories/大数据开发/Redis/环境搭建/index.html","36415bdf44e1ba45cab7b3629078b6ab"],["/categories/大数据开发/Spark/index.html","a56196f7eec114b545e4e9c961c0a1c6"],["/categories/大数据开发/Spark/环境搭建/index.html","39dc9555be09f217019a2384964136de"],["/categories/大数据开发/Zookeeper/index.html","47b8c78896550dace711a8eadb9f90f7"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","0fe2ce8c0fa3fca68c495f8f7b59c377"],["/categories/大数据开发/index.html","6065523f547d5032486e2898c59d26a5"],["/categories/学校课程/index.html","9ab15c88735528eee3b55ac1c9276f52"],["/categories/学校课程/计算机操作系统/index.html","ec58f59c82ee39f5dd39e7d1e1dc76e4"],["/categories/操作系统/Linux/index.html","e1002903756430757fdf1ff40ea72a16"],["/categories/操作系统/Mac/index.html","9144c8f655fe0179f9f5158f35e280ee"],["/categories/操作系统/Windows/index.html","319b8f4f9c74b91aedeb8b78f3e9e47d"],["/categories/操作系统/index.html","14be6da2b76e0e7b7b0237f3c278dcff"],["/categories/数学建模/index.html","c688c3f646368b44e2ad62e966bcae8b"],["/categories/数学建模/latex/index.html","23cca1074ce9b4a454633273000f76cc"],["/categories/数学建模/优化类/index.html","9e3d59183aab20ac24df817fdfa89ef3"],["/categories/数学建模/优化类/现代优化算法/index.html","cbb2a7ef0c8056f2ba5da330d2b71cd6"],["/categories/数学建模/优化类/规划类/index.html","0f7832f287fe6c2c4531818e8ae22e1a"],["/categories/数学建模/绘图/index.html","796a622d303eed8706ec74474b2428d7"],["/categories/数据库/MySQL/index.html","fea366b4d1c41a407366630a9cbf63e0"],["/categories/数据库/index.html","c3f0e59e4cf414f3aa8ba19f4546ba78"],["/categories/数据结构和算法/index.html","dc36377c860f5cfbb5c38273cd223c89"],["/categories/数据结构和算法/page/2/index.html","01a49f6d789e1da9829fe58a92dc6945"],["/categories/数据结构和算法/基本原理/bfs/index.html","506fe6ed0ddc45c6fbdc1b324a0e386d"],["/categories/数据结构和算法/基本原理/dfs/index.html","8ee9fec15c8273de631ca04bcf08f73b"],["/categories/数据结构和算法/基本原理/index.html","497a5fac505dbc89e077f7f794d21c61"],["/categories/数据结构和算法/基本原理/动态规划/index.html","6d4ac5ac93c8cd1a18db0a57cb814ed1"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","33aece9aade4213812c1dc239eaaffe8"],["/categories/数据结构和算法/基本原理/图论/index.html","73d3bddb649c19e93b37fec99d3a9db7"],["/categories/数据结构和算法/基本原理/字符串/index.html","8479717be93e28860e9b9617d61122d6"],["/categories/数据结构和算法/基本原理/排序/index.html","532b02c13fcc0dd9e602a8fbdd640e12"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","e285f1e80353e848b0ec582d78524ee2"],["/categories/数据结构和算法/基本原理/数论/index.html","4bf2898e4651be02ed58cc37af4da1d3"],["/categories/数据结构和算法/基本原理/树论/index.html","d478d3b322c1380d5a58e2c8c86f482c"],["/categories/数据结构和算法/基本原理/链表/index.html","74fa41f952658cf09ceafa2c7c838b2b"],["/categories/数据结构和算法/算法题/index.html","c0cfd6c0388853e805adf832108fd3a6"],["/categories/数据结构和算法/算法题/二分查找/index.html","faad0be02384e66ef81079cd7579eee0"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","62200176cc3c1fb7647a15be00e585bd"],["/categories/数据结构和算法/算法题/动态规划/index.html","b04466d54ced7045879043fd922f2b6e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","efcbf42395144999b8671214a931c277"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","9c856e40cd19cfa3db34dc79b644f5e2"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e2ff9a37a98ffdc02e6f3b762260f094"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c7e2cb846aaa7a4c4d20975b63131527"],["/categories/数据结构和算法/算法题/数论/index.html","717da5232a573fa55583bde4c36d171e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","03c12e8fac892482bade25bd9aa41622"],["/categories/数据结构和算法/算法题/树论/index.html","b4dca99e169ee1bc50221c9c4e522a58"],["/categories/杂七杂八/index.html","9fa4793e1f9fef5228b35893326f67e5"],["/categories/杂七杂八/博客搭建/index.html","7083f6d77235fd5901b3c4af7e725ca1"],["/categories/编程工具下载/index.html","a3122722a42eb1636a74c915f6dab02e"],["/categories/编程环境/index.html","5974b9012a25d2d3d1ba56b5e9dedcc2"],["/categories/编程环境/大数据/index.html","5d7c2d00c4a84608f2240a7bf0495349"],["/categories/英语学习/index.html","955bb9eb6a33468c8e43c817c45db276"],["/categories/英语学习/英语语法/index.html","317ed5e1549010d4639b5e4b7c6c7cb0"],["/comments/index.html","320324dcdf01ac92e06e3c59cd6ea271"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","bd77b23b16b101e7e700cd8cb4c2abaf"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4ef9e5b4fa9af526aae6d5cd2d2b99d1"],["/movies/index.html","9b0b2ae7594fa75eb2f5c1e652a90782"],["/music/index.html","1bed38acb9de716189bd93a99a77a746"],["/page/2/index.html","0f198b65b43847196f9e34b3d4e8a523"],["/page/3/index.html","56ec40a9d7aa62e33297212ae27da4a7"],["/page/4/index.html","7828fafda5ac0cd08f4ddb9166b707b5"],["/page/5/index.html","fa9a9c585ceaf90164c3d753131753bf"],["/page/6/index.html","ca8c98bcfd7980ca0c81d7c2e768b3d2"],["/page/7/index.html","f960ad4b5b2f0ea343ca290778747db9"],["/posts/1021360842.html","1e668a61845a007d7c8690a00ce7f168"],["/posts/1120620192.html","6050df1e5274f484ac53fbfa2e606ac9"],["/posts/1137707673.html","d8099683a703b0091088308cb23acba5"],["/posts/1141628095.html","28ce59e4dd547b609e5e15078c7257a5"],["/posts/1168613674.html","c197ecb888b8e59997864fd0f30b8f18"],["/posts/1219920510.html","42111a438de0f4bd4043ce844c1214ab"],["/posts/1222166338.html","dd2d1f9788bb021bc047478c4611b595"],["/posts/1259097482.html","04b115e69ba503549472ff567633dd24"],["/posts/1271036369.html","eac7bd9818e04a81017b0b084391324c"],["/posts/1312847445.html","3609eadf9748d1122d978f9e3902f546"],["/posts/135355774.html","81eaf8b3740567151be11107140a5a0e"],["/posts/1375344716.html","e642977ccf6a4a2d072b6d7124ef3074"],["/posts/1388991698.html","4e24f9dd7a0902154473427a296b395f"],["/posts/1410315814.html","a8ababa2143b3d8d713c7486ea53d8a0"],["/posts/1452790229.html","660d2dc92c9287f183c4163410097bfc"],["/posts/1470079884.html","dfb2a5f0a7e741859f582f41cf366718"],["/posts/1470079885.html","ec275c4fcef101909b171e52814c557a"],["/posts/1470079886.html","c4ca7c520d14d3438ea18401695245f4"],["/posts/1470079887.html","b67a68cff898df639f1f6b9e33772ae2"],["/posts/1498536549.html","4ecb4a2080b7d80a082208db5fc142fe"],["/posts/1539568593.html","f670afe707e91e193d5da179edbd2a3a"],["/posts/1547067935.html","75b01152e3c209971d9ed1521e38f22f"],["/posts/1557866301.html","72715218e49ef396cb152368396dcbd1"],["/posts/1571776361.html","8d272b0847a74616723caa81110b8142"],["/posts/1605124548.html","b137f0141abebfdd00c539baae7c806a"],["/posts/1633036852.html","22d805a283447cb779548cb190f27b46"],["/posts/1667740714.html","1bf134a14b5ec16b274d61f83febf0cc"],["/posts/1674202625.html","df9bf729f3f0c33039898a92f2b26006"],["/posts/1765123828.html","5b8f20a18d830bbb0f128354f27a73c7"],["/posts/1767336200.html","0fec66bcaec0f485b9e48f80aeced941"],["/posts/1776114197.html","72b6f5258f1e26e18e3e1634ef563710"],["/posts/1817748743.html","ad398404938f198a0d741b877b20d1b7"],["/posts/1925125395.html","a911da6c4fa87c6930166f9c7555539b"],["/posts/1966191251.html","e10db6edcf74f449cfe3ab7df1e7a114"],["/posts/1987617322.html","43fe1203bae80089967b8e093d7c2852"],["/posts/1999788039.html","c38ad0386b91b946abf914058c3c4e33"],["/posts/2075104059.html","8ae798e49f7a64242be63a264d3b0fa3"],["/posts/2087796737.html","cc80f644236ffd77697e878877e3b7b1"],["/posts/2106547339.html","8fa7fb7736a74a166dc12b65645e5417"],["/posts/2207806286.html","15b458da6fed1a35ce27a98af82a285b"],["/posts/2225903441.html","6e79caa6b1bff490a8801211a2bc0996"],["/posts/2265610284.html","4cc853adeca6b1527ea6c1fb0e73a7ec"],["/posts/2281352001.html","c61d6ea5e4f2d7aa342afc4f3f4a98aa"],["/posts/2364755265.html","2ac01fe17bab060c66f90e1626dc4c12"],["/posts/2414116852.html","74b3fd450147c23eff4601a7104b1572"],["/posts/2421785022.html","7a6391c27ef1c5b8127970606c51f135"],["/posts/2482902029.html","38fa353e346bc414d183afc503e90247"],["/posts/2495386210.html","41eb91a7233329e6cc8cae781bf5f617"],["/posts/2516528882.html","f6a0573ff19e24ea15a47e6e950e2c71"],["/posts/2522177458.html","2e280b3187180d1a360e01f1755f7933"],["/posts/2526659543.html","1cbde43fd4c7b2a82e0a1a19dee2c67b"],["/posts/2529807823.html","ff3f16daea40f1b8e243279d4f2ba276"],["/posts/2596601004.html","ca1f95adeed69216e5e90de1acc2f3c5"],["/posts/2697614349.html","bbcd2037db1d5e34570d0c273a7f7d0f"],["/posts/2742438348.html","0081f5150a782432cc62055321e63cda"],["/posts/2768249503.html","e52f923b0a08df8ec3cb4a95b1abdd8b"],["/posts/2864584994.html","1404fc5a9b4da2e948a92cc4043904b3"],["/posts/2888309600.html","1da031ba9bb6f326f21950fa9a505cef"],["/posts/2891591958.html","0518157229fdba79c71563e92c4d2463"],["/posts/2909934084.html","1a86e2eec95d8342e28123014dcadfb1"],["/posts/2920256992.html","325d079485de5b0b52b44ff6920616f8"],["/posts/2959474469.html","33512ec9da8fbc788606c0e89d8c20d0"],["/posts/3005926051.html","49a6d433fca275b546892c5f93d1c034"],["/posts/309775400.html","603982b29e635493e8809da7e157fbfe"],["/posts/3156194925.html","4649fee70fffe12889793b3f8d7503fb"],["/posts/3169224211.html","b2bf10fb2ed2b936aef289fa1ed759eb"],["/posts/3213899550.html","d91cd03736a9d7f0c8d7b105c8bb95c1"],["/posts/3259212833.html","a23cd78b461fbf7d2239bbf83ca1df3c"],["/posts/3265658309.html","3f4c642a1a44079a48c8ad376966abc5"],["/posts/3266130344.html","f94ae5e4d6d7d49190a718cad6f39521"],["/posts/3292663995.html","5f564b0991a79ddaae4cfc0f77923008"],["/posts/3297135020.html","2e81580aa23bdc4909d9993bbb1e5002"],["/posts/3306641566.html","f245e074c0d8e9e60d9a1500f66ce9dc"],["/posts/3312011324.html","632bdd05cc98607ab4639656e366e2f7"],["/posts/336911618.html","e406d29db8a1e1f7487024a9825aede9"],["/posts/3402121571.html","98a94532e9d0240b93ce92f537ba9fe0"],["/posts/3405577485.html","2112aacc9e45843115e083aa2715cb9a"],["/posts/3498516849.html","a0192e9d10260ecd4a6ebc61156dfef6"],["/posts/350679531.html","c753bc5c9e424b9d7963bc0d22f242a3"],["/posts/3513711414.html","04a72112602ac48d5ef875e0a0943b5c"],["/posts/3523095624.html","6794b4f0cbcd9325eca9cd6654edd6dc"],["/posts/3546711884.html","ad1b4d7e0f30c91cd0ec0794b4b41422"],["/posts/362397694.html","beea151189579fe5c5da12dbf9358e7f"],["/posts/3731385230.html","b938d40be7e8fdf53fb3b4ced6f473a8"],["/posts/3772089482.html","f8874b63e1ee0448eae2ecec1ab67f14"],["/posts/386609427.html","abd1074afc2a19047b4636812af31cb3"],["/posts/4044235327.html","e5dc444af92135c71b9b5b0ea1b6042c"],["/posts/4115971639.html","b6827e1a4ca0473a1076e5bb38f2c5e4"],["/posts/4130790367.html","0d15701f905e439fff9c0a6092a7a315"],["/posts/4131986683.html","5068bfbe1cd60f1397c8fa88013d846b"],["/posts/4177218757.html","e93e088905c2e1c6f2388e723098fa65"],["/posts/4192183953.html","53329074e50d65e3b2fd709e489fae0f"],["/posts/4223662913.html","9e40e3713a20c50f089f1ce9a5c6a101"],["/posts/4261103898.html","960301a35ca9cc9915889de83478c4a4"],["/posts/4286605504.html","101f1478c7045842337a2c2533728f7e"],["/posts/449089913.html","d00b9fda832d4f2230abfe86fa2fd263"],["/posts/469711973.html","af86d9b63a2fc6e6543f3eb28139df2e"],["/posts/482495853.html","8238b561f424d31a32d583e91f9868e7"],["/posts/488247922.html","746bb53c2272f5661d0a78580285a51d"],["/posts/517302816.html","2dae334c953ae884ab6b2eaf3567d4df"],["/posts/570165348.html","a8fcfef1cd85ad775dc1db198467c0b6"],["/posts/595890772.html","1b775e450db0131919603e5112ce5302"],["/posts/67485572.html","4788f9c10d2be74ad181b3763edda9ba"],["/posts/694347442.html","2dc67e205d5a88d8f5f6ff0c3c66d841"],["/posts/707384687.html","c1db60d77812b6e7aac99449b737c04e"],["/posts/71180092.html","65d685adb167285b8bdd4f244c3a4b91"],["/posts/716459272.html","c038cfd6deafb3ea6eb935dd88853aea"],["/posts/765481613.html","9b678d8a627bf89180b6b57d6b525c91"],["/posts/778231993.html","c2c9447c24d1048c3ab01118e932f6c5"],["/posts/795397410.html","6aff9409e5697e9856a70f4cea3dcbb9"],["/posts/820223701.html","afe399aa3e3c722757c41956cd85995f"],["/posts/830372185.html","9972a07fa589ba799e7791f16c0dfcee"],["/posts/88294277.html","622d39a1df986ef375fb8f5388673106"],["/posts/939963535.html","128caf4d88a59eb62ca613ec732d1a7d"],["/posts/983786067.html","49e446a23f599cb79c890dfddf44016f"],["/sw-register.js","1aba4b63d0ae7e3e21c3f1366f227684"],["/tags/C/index.html","96cb330214d15ccd3b4b055498c853b6"],["/tags/C/page/2/index.html","83e941bc9b4b8932a4bbdc71fc3f4172"],["/tags/C/page/3/index.html","eb57be8836b22fba6fababa1347bdba5"],["/tags/C/page/4/index.html","3cd2f6420b2b90fc2cd791fbd26db6de"],["/tags/ETL/index.html","62f2c81cebd3d048fc1fe10ab6882f0d"],["/tags/ElasticSearch/index.html","ed2fd314b75ff55f5e2c949a731e0ef1"],["/tags/GUI/index.html","a6004251630f198284cd7f59e1aed7c4"],["/tags/HBase/index.html","930476a774b7ae6096cba1b2639814b4"],["/tags/Hadoop/index.html","021354d7baf43546a6e581f6f2761961"],["/tags/Hadoop/page/2/index.html","5b9b7b33624e371f2bcba9b818df0696"],["/tags/Java/index.html","9f1cd7415a439828711cf0d9ee53cd02"],["/tags/Java后端/index.html","1fb7bebbe2ba49ebd776297ecdbf6a6b"],["/tags/Java后端/page/2/index.html","57dc79f5e2d719fee1fdda14eb201203"],["/tags/Java基础/index.html","a4b19ffcd55ec342e51f5dbedd3fe24b"],["/tags/Java基础/page/2/index.html","17fb01f995cb69d59fd34d4651b989f7"],["/tags/Kettle/index.html","b745a84b03103df74d90c71176dbbb28"],["/tags/Kibana/index.html","02b44416c2bae613f424e15588a42fb5"],["/tags/Linux/index.html","161dfc78190dacd9cd735dfaeab3c7c8"],["/tags/Linux/page/2/index.html","6eb65b7924268b551c046ae790568ba6"],["/tags/Linux/page/3/index.html","209ae8080513e9cb5ed5c6e4479a0ac3"],["/tags/Mac/index.html","aaf40af010eb4f94d11d7ccfde478ccf"],["/tags/Mac/page/2/index.html","25c0772ee04a94efaf25c00e75c002e3"],["/tags/Maven/index.html","26c13104e08e626a59a1b10fac763201"],["/tags/MySQL/index.html","8c1a5e07fdfda8fe1065d5701cb310e9"],["/tags/Python/index.html","384218887b78c0d4f6ad078381052b74"],["/tags/Redis/index.html","0ae7ab9d234b42c039c1ddc4986c3ef8"],["/tags/R语言/index.html","f0a0ebcd71e7c0cd8a444f7d9ceaa52a"],["/tags/Spark/index.html","70bac2764ff200292ca90b14164880e9"],["/tags/Ubuntu/index.html","f7ca8cb88cd87582d8ab8f382758b81c"],["/tags/Vue/index.html","a70930879ef669401dcbb2afdc961ee8"],["/tags/Windows/index.html","bd27fec51c37a6b196946aca67b33198"],["/tags/ZooKeeper/index.html","afe5a9bb6101a492447a55331177f389"],["/tags/bfs/index.html","27d1f921b83e1ca24b17f6eda3375083"],["/tags/dfs/index.html","17bb7860267eea61dbffc74fb29c0b7c"],["/tags/folium/index.html","d8aad21dfb64565ed5f3641166a25070"],["/tags/git/index.html","c995750112d1bfdab3afe6f452c4ccdc"],["/tags/iPad找电子书/index.html","8767db8d0d72a516840f6d5249735b76"],["/tags/index.html","e830c71c794572fdfb7a95c7f95f3e1a"],["/tags/latex/index.html","e0dae94494a501165a787fc23e315ed1"],["/tags/中间件/index.html","a591178857680fab95106473116ea825"],["/tags/二分查找/index.html","f5860b536fa3a26453c0e04ce6d0d525"],["/tags/优化类/index.html","46424eecee91a61d7a2c48cccef97747"],["/tags/前端/index.html","231b2d5364a48d242ece6b28e5d578d5"],["/tags/前缀和与差分/index.html","9579a9040abca3970b5fd81e59b91cf9"],["/tags/动态规划/index.html","e735c149c77249b369a2edd5eac38edc"],["/tags/动态规划/page/2/index.html","ba26864851105ff30010d55c02fe6c33"],["/tags/博客搭建/index.html","7e477a623ed9a00351242d0bf14e8951"],["/tags/图论/index.html","cdb81a465e0363574e6b30076e1b5c61"],["/tags/大数据/index.html","fa181fe9c65604939f04ebceb37d970b"],["/tags/大数据/page/2/index.html","ad17f9d900ca3e849e302b54def257f6"],["/tags/排序/index.html","dab083da8ff00aa827979f977849a747"],["/tags/操作系统/index.html","b620e09ce1cd4edd444beb5a3b0ae1cb"],["/tags/数学建模/index.html","053d787950c7910d9867b7eb3d65a4cb"],["/tags/数据库/index.html","ec727d92b2d8a0aadf07918bdc82f661"],["/tags/数据结构和算法/index.html","6b6c8dff7ce3b33fe80c4f0b6a7f76bf"],["/tags/数据结构和算法/page/2/index.html","509d69be8590869952614002baf0a60f"],["/tags/数据结构和算法/page/3/index.html","5fd2705cdf101ebc753962050ac99595"],["/tags/数据结构和算法/page/4/index.html","dc5b65eac21197f9df8d505a32a01c59"],["/tags/数据结构和算法/page/5/index.html","ef47537679a79bf49268d76af1f97366"],["/tags/数组和字符串/index.html","d08a2052c44cbd47dd06cff9ad862192"],["/tags/数论/index.html","dc4395937f66fbc9d9a2138db2352eaf"],["/tags/枚举类/index.html","71b9cd38e85615447b4f2092cc1c3a8d"],["/tags/栈和队列/index.html","3410fa805c3a6179a8c25c3a705ac9b8"],["/tags/树论/index.html","053cbf18f9aa54da9c9e916b9d7eb634"],["/tags/测试/index.html","03f86b11be34be073be7a85358c7b84b"],["/tags/环境/index.html","57a1e98d960753d4fb8f32aa339d54f5"],["/tags/环境变量/index.html","6b9cdc77c39524e2ab7ce6ed02294393"],["/tags/绘图/index.html","e044eceb53212dfa406229795473d74b"],["/tags/编程工具/index.html","8f80ed81512495dca4570b7d9bc7441f"],["/tags/编程环境/index.html","3770c1fca603f5bfb08055984f5171dc"],["/tags/网络编程/index.html","2a6b0bc22cd60450592bb9d19c459225"],["/tags/英语语法/index.html","de12d1e92e97a555d0e5035fa398c9cd"],["/tags/计算机操作系统/index.html","e8e826717801a2feb794c6ff261e6297"],["/tags/论文/index.html","3f501b3082506cccbe8d38ed83bc132a"],["/tags/资源下载/index.html","96215958bd5037431669d71e7b48b647"],["/tags/链表/index.html","e26b979f7f79fc85817e6dafad6cd5b4"],["/tags/集合/index.html","30cfe4ce259644d8d35b7288a21220f4"],["/tags/集群/index.html","0b9a92e42f7147c25bc707b41cc35296"]];
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
