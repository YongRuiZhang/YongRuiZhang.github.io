/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d46dbbf24544a42550b64b34b0d3c6b1"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","246642bc2fd71f6028440ff99b10af07"],["/archives/2023/02/index.html","79058bc35461253a98624008fe5c333f"],["/archives/2023/02/page/2/index.html","1ea3f3b1df3405b619d97bdf19907319"],["/archives/2023/02/page/3/index.html","c39d11f3f355325ab81b6453f94d11a3"],["/archives/2023/03/index.html","6a9645da408f709c9dfbc62519def100"],["/archives/2023/05/index.html","faef5175f8f433cbb220c3810d5f60b9"],["/archives/2023/06/index.html","b67bfb7ae52e8913f5d5eccad8e3d124"],["/archives/2023/09/index.html","e31d5af0184cff07f269a60f7f9752e4"],["/archives/2023/11/index.html","1d88844eaadb6863af2a560e05159ffd"],["/archives/2023/12/index.html","2468662f967c41c95e8cd89ac673ef51"],["/archives/2023/index.html","07f8cc473c7fe85e4296611e6d6c85c2"],["/archives/2023/page/2/index.html","365a81acd44eeae0d8beb3f198a8677b"],["/archives/2023/page/3/index.html","a238d396769f783e4cb72d4483360202"],["/archives/2023/page/4/index.html","30c52111b5d40f270bbbdfbba1bddd8c"],["/archives/2023/page/5/index.html","0d0c4c8a9f9c5127f109e33d33067e68"],["/archives/2024/02/index.html","8a887f72e3e804f673145c58fb67593b"],["/archives/2024/index.html","b6b916f506564ae3704144c258c58253"],["/archives/index.html","5e42a1a5ca94a6885096108b60059ced"],["/archives/page/2/index.html","03c7bea90b3a1c3743ff39247c31b891"],["/archives/page/3/index.html","f703a6382d94c8b6dcf7d92cb597a410"],["/archives/page/4/index.html","a4153b897d7c62089878d87fbbdad67c"],["/archives/page/5/index.html","26cb2f9bfa6993357385d3db782ce76b"],["/baidu_verify_codeva-qQP2iZOMLX.html","47e7a25398597a2f4b300c779e1b953a"],["/categories/Java/index.html","647148dd252414baf3ec834ac3307f2c"],["/categories/Java/后端/index.html","6d0910c528885cdeb4b9792f3383477f"],["/categories/Java/基础/index.html","8bba6989c3b6526942150c0a0680c1bb"],["/categories/Java/基础/集合/index.html","56e13d791a31da3fc0ffc203cf6f7f47"],["/categories/Python/index.html","59712457f2226654527d163973f9ef8b"],["/categories/Python/编程环境/index.html","e251d5ad3f423e164277fd9739981508"],["/categories/R语言/index.html","5a6366b7db5b3963bd60d388d54f0fbe"],["/categories/R语言/编程环境/index.html","3ea2e8c204d1a07a5532d7b4bd52520d"],["/categories/iPad/index.html","5a4690cb4e876c760fd878f89c29e62f"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","8191d4419d8b4d1b94b8587de42a210e"],["/categories/前端/Vue/index.html","e6913f9d5cbe21a9e322ec0a3ac1cfb7"],["/categories/前端/index.html","6c112947f96e8d71f7a6c5b3fd260739"],["/categories/大数据开发/ElasticSearch/index.html","9c90e9bd79e12a12abbd132812b14794"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","e414d4786d8e4c930c5b7c99526e18a2"],["/categories/大数据开发/HBase/index.html","5c9737849a77cc83ecfe768c112928a4"],["/categories/大数据开发/HBase/学习笔记/index.html","e6f4440f72f0a5acd3130c798daae56d"],["/categories/大数据开发/HBase/环境搭建/index.html","31f568738c278419ba56be012cc2d0ed"],["/categories/大数据开发/Hadoop/index.html","0abb9868ab428f56103c68ce3f041968"],["/categories/大数据开发/Hadoop/技术/index.html","aa9b46ccbf46b6e62ee51e116e25e1d3"],["/categories/大数据开发/Hadoop/环境搭建/index.html","8784f1468b0dd3a5678f68088103bb7b"],["/categories/大数据开发/Redis/index.html","5f67390afd4d4198333e460d4b7b20bb"],["/categories/大数据开发/Redis/技术/index.html","1bc0b3c5474147fec981f80dd3075d07"],["/categories/大数据开发/Redis/环境搭建/index.html","c0b06d2495aae03c3552e4629b5eabb7"],["/categories/大数据开发/Spark/index.html","635df020632272d737e10edba13a57c2"],["/categories/大数据开发/Spark/环境搭建/index.html","b32208e9ac8a93c30c721c955eeaac95"],["/categories/大数据开发/Zookeeper/index.html","6b012f09317a8580beb0698dd7b17943"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","075ce704e2331b8820703029cdeaa962"],["/categories/大数据开发/index.html","893a22a7fd7ea8aacfdb9742ec35d70f"],["/categories/学校课程/index.html","a030053c940eefb8e53e61f417078f95"],["/categories/学校课程/计算机操作系统/index.html","a5a4967caca983154b19f661d2fc082f"],["/categories/操作系统/Linux/index.html","24184d7999f112b1658e1d35c998be50"],["/categories/操作系统/Mac/index.html","a0d7f31b8dcd96a2ed345d0b0d2afd00"],["/categories/操作系统/Windows/index.html","fed813f1c8471f20ecc2c2d57559f300"],["/categories/操作系统/index.html","de494e64c82d3075ccaf13faa8dc6920"],["/categories/数学建模/index.html","26bb93216af91b352150ed892b1fd50c"],["/categories/数学建模/latex/index.html","97ad8a846c9ee61ca897c5b20246a17c"],["/categories/数学建模/优化类/index.html","8d8484c7676835c90ed4884899500987"],["/categories/数学建模/优化类/现代优化算法/index.html","4919089e80a8221eca0ab0a6f489fdbd"],["/categories/数学建模/优化类/规划类/index.html","a82d204f746861be204731b44989a5f5"],["/categories/数学建模/绘图/index.html","0ad8b874bdbf89ca4765592eb4e47b96"],["/categories/数据库/MySQL/index.html","f47fefe4960ae749da42da7be872ebd5"],["/categories/数据库/index.html","8c29b10ec40a3438548ce46eb1c26947"],["/categories/数据结构和算法/index.html","7f582e6e5bc657d652e78879d7c0fe88"],["/categories/数据结构和算法/page/2/index.html","7d8daf3ca9db37b60e89cf8f0157aa09"],["/categories/数据结构和算法/基本原理/bfs/index.html","014933165910f57aac70b3af39564231"],["/categories/数据结构和算法/基本原理/dfs/index.html","d84dceb432723e6e13bbf4138f06090e"],["/categories/数据结构和算法/基本原理/index.html","0becabc1862aec72f8f4131a63cf6ea8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","99f388eedc1d268b3e6b3a0515a61550"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","b89bb18571fd0a5edba50edd698c90d0"],["/categories/数据结构和算法/基本原理/图论/index.html","4a35f62c5038d3edbf194b93dfa53ad1"],["/categories/数据结构和算法/基本原理/字符串/index.html","58143dfa2994c6525af43a0074e07ddd"],["/categories/数据结构和算法/基本原理/排序/index.html","7e3014ac630ad40b7109a1fba2a7ccd5"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","bb2882c5c76095bb5ab318320c7b6620"],["/categories/数据结构和算法/基本原理/数论/index.html","5977cce33f09a4d8a354cb9c73d0ccdd"],["/categories/数据结构和算法/基本原理/树论/index.html","5ce892afe62f8a1c1b64288d12211fe4"],["/categories/数据结构和算法/基本原理/链表/index.html","54552a144122599c2266af1162a3e867"],["/categories/数据结构和算法/算法题/index.html","ac32ef5e716f1890afb1fbc160f6f402"],["/categories/数据结构和算法/算法题/二分查找/index.html","512c49d4af754826648c6836c0e76919"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6cce24c25604a2a633571bafa9ab18f1"],["/categories/数据结构和算法/算法题/动态规划/index.html","27f4a1508dd88c0e92ebab6e7e1fdd11"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","fd330b0b7f8aa5de480304b4863d9909"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","580c054ec9cebf5051ecc2e4388f8fe0"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","35b5e2bdb69b4c2d6499a97ce94b7d0f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","0485ec52e92ef00cd4ef82b90e32ef8b"],["/categories/数据结构和算法/算法题/数论/index.html","1a1203d1224b74e629b08dff46bcd4c3"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b1a05402f04b270f6e9066b468910761"],["/categories/数据结构和算法/算法题/树论/index.html","39c5c74cce8caf82bd5efa724882762b"],["/categories/杂七杂八/index.html","d1c7e39db519a10d32811eab174feda9"],["/categories/杂七杂八/博客搭建/index.html","2f188ea83e7a4343d04deb616036c06d"],["/categories/编程工具下载/index.html","247f6e2f79538c139452fb7c4a90b1ce"],["/categories/编程环境/index.html","b0d7f8b529d10cf7393bdc2e1a457001"],["/categories/编程环境/大数据/index.html","6fcfb824d179d9401d730c0c23faa9a9"],["/categories/英语学习/index.html","cde035020813e5baf87daf766da63227"],["/categories/英语学习/英语语法/index.html","a71407101da770da0a545a00c82e66cc"],["/comments/index.html","ba59f1add8426c596c34670ed6abb5ad"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","40bacbff490d4e0fb2a87872ade7f2f7"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","d731a9ae01f47d35d5de7eee201b7689"],["/movies/index.html","0fd5426dd62f862e7dce130df4d1f73e"],["/music/index.html","05c2370474d615783f1f02d4db8f6835"],["/page/2/index.html","8a724dd3a66762995fa4e231e54a89f1"],["/page/3/index.html","4fe664fe147b960ee99439cc1da53db7"],["/page/4/index.html","cd6902051e0b3172a39215f9c7a0f7e7"],["/page/5/index.html","d5211ebc06250248fb0c1f0ac807a9cb"],["/page/6/index.html","100f33ac77170f9d9d8bab1e46792599"],["/page/7/index.html","99b1eaff84f3572847f9f7fb983f5d36"],["/posts/1021360842.html","1e668a61845a007d7c8690a00ce7f168"],["/posts/1120620192.html","f548869a049480d383ff14687e4111d5"],["/posts/1137707673.html","d8099683a703b0091088308cb23acba5"],["/posts/1141628095.html","28ce59e4dd547b609e5e15078c7257a5"],["/posts/1168613674.html","c197ecb888b8e59997864fd0f30b8f18"],["/posts/1219920510.html","42111a438de0f4bd4043ce844c1214ab"],["/posts/1222166338.html","dd2d1f9788bb021bc047478c4611b595"],["/posts/1259097482.html","04b115e69ba503549472ff567633dd24"],["/posts/1271036369.html","eac7bd9818e04a81017b0b084391324c"],["/posts/1312847445.html","3609eadf9748d1122d978f9e3902f546"],["/posts/135355774.html","f8a0cfc95c5a7c34af7aa3f8c37a3efc"],["/posts/1375344716.html","1665afd6726d6371717c4ffac3783a68"],["/posts/1388991698.html","0055c97149e5ff38fe6786d85b752400"],["/posts/1410315814.html","a8ababa2143b3d8d713c7486ea53d8a0"],["/posts/1452790229.html","660d2dc92c9287f183c4163410097bfc"],["/posts/1470079884.html","dfb2a5f0a7e741859f582f41cf366718"],["/posts/1470079885.html","ec275c4fcef101909b171e52814c557a"],["/posts/1470079886.html","c4ca7c520d14d3438ea18401695245f4"],["/posts/1470079887.html","b67a68cff898df639f1f6b9e33772ae2"],["/posts/1498536549.html","4ecb4a2080b7d80a082208db5fc142fe"],["/posts/1539568593.html","f670afe707e91e193d5da179edbd2a3a"],["/posts/1547067935.html","75b01152e3c209971d9ed1521e38f22f"],["/posts/1557866301.html","6bdb83c482afeae63a35d326e861fe9d"],["/posts/1571776361.html","8d272b0847a74616723caa81110b8142"],["/posts/1605124548.html","b137f0141abebfdd00c539baae7c806a"],["/posts/1633036852.html","f2fea2f82104d925fd64633c767bd5e3"],["/posts/1667740714.html","1bf134a14b5ec16b274d61f83febf0cc"],["/posts/1674202625.html","df9bf729f3f0c33039898a92f2b26006"],["/posts/1765123828.html","f60637aa49af3288c07af5f4606b4d91"],["/posts/1767336200.html","0fec66bcaec0f485b9e48f80aeced941"],["/posts/1776114197.html","7ed54e2756f4401796a0f3dae27209cb"],["/posts/1817748743.html","ad398404938f198a0d741b877b20d1b7"],["/posts/1925125395.html","b9a19bab034c5744b61d08b0801c0472"],["/posts/1966191251.html","cf1dc16aad1a86a35f07881851c428f8"],["/posts/1987617322.html","43fe1203bae80089967b8e093d7c2852"],["/posts/1999788039.html","30ecd0802f6c3648e003bbfc3c64aa96"],["/posts/2075104059.html","8ae798e49f7a64242be63a264d3b0fa3"],["/posts/2087796737.html","cc80f644236ffd77697e878877e3b7b1"],["/posts/2106547339.html","8fa7fb7736a74a166dc12b65645e5417"],["/posts/2207806286.html","15b458da6fed1a35ce27a98af82a285b"],["/posts/2225903441.html","bfc72e328b6fe23f9e7ff72900373c14"],["/posts/2265610284.html","b2c1ac3ecb92039f9abe30ebe3020401"],["/posts/2281352001.html","c61d6ea5e4f2d7aa342afc4f3f4a98aa"],["/posts/2364755265.html","2ac01fe17bab060c66f90e1626dc4c12"],["/posts/2414116852.html","74b3fd450147c23eff4601a7104b1572"],["/posts/2421785022.html","7a6391c27ef1c5b8127970606c51f135"],["/posts/2482902029.html","0a2dc21fc6288472fc47e06d3c1227ae"],["/posts/2495386210.html","41eb91a7233329e6cc8cae781bf5f617"],["/posts/2516528882.html","f6a0573ff19e24ea15a47e6e950e2c71"],["/posts/2522177458.html","2e280b3187180d1a360e01f1755f7933"],["/posts/2526659543.html","417c4ad38c461509eb32cc68ab9877be"],["/posts/2529807823.html","a0af036c2422104754c24a3e21ad2a20"],["/posts/2596601004.html","ca1f95adeed69216e5e90de1acc2f3c5"],["/posts/2697614349.html","bbcd2037db1d5e34570d0c273a7f7d0f"],["/posts/2742438348.html","5fc59a5676be6e0a1cc9bd73428a2547"],["/posts/2768249503.html","e4b11b97739ca0360afb447cfab7743f"],["/posts/2864584994.html","53af91fc72ee77745584e3bfeb7d1d00"],["/posts/2888309600.html","1da031ba9bb6f326f21950fa9a505cef"],["/posts/2891591958.html","b360298dcb2fa417105247e9cb57d438"],["/posts/2909934084.html","1a86e2eec95d8342e28123014dcadfb1"],["/posts/2920256992.html","325d079485de5b0b52b44ff6920616f8"],["/posts/2959474469.html","33512ec9da8fbc788606c0e89d8c20d0"],["/posts/3005926051.html","49a6d433fca275b546892c5f93d1c034"],["/posts/309775400.html","603982b29e635493e8809da7e157fbfe"],["/posts/3156194925.html","4649fee70fffe12889793b3f8d7503fb"],["/posts/3169224211.html","b2bf10fb2ed2b936aef289fa1ed759eb"],["/posts/3213899550.html","d91cd03736a9d7f0c8d7b105c8bb95c1"],["/posts/3259212833.html","29d9b9a4a07cdf50865c222e87ddcf85"],["/posts/3265658309.html","3f4c642a1a44079a48c8ad376966abc5"],["/posts/3266130344.html","28d4bf9690ce2dbb5ba12c7c5771cafe"],["/posts/3292663995.html","5f564b0991a79ddaae4cfc0f77923008"],["/posts/3297135020.html","2e81580aa23bdc4909d9993bbb1e5002"],["/posts/3306641566.html","f245e074c0d8e9e60d9a1500f66ce9dc"],["/posts/3312011324.html","632bdd05cc98607ab4639656e366e2f7"],["/posts/336911618.html","7058043db9dd6f06770ddfdf36231d83"],["/posts/3402121571.html","be2eeaffb374a455924f3f67c64cc024"],["/posts/3405577485.html","56440f83941b5af7ac70d3b83056a79d"],["/posts/3498516849.html","6f90357fb7c5febec36d080a48f36325"],["/posts/350679531.html","c753bc5c9e424b9d7963bc0d22f242a3"],["/posts/3513711414.html","04a72112602ac48d5ef875e0a0943b5c"],["/posts/3523095624.html","c91b4d8fcf2f1065408744787d72afa5"],["/posts/3546711884.html","ad1b4d7e0f30c91cd0ec0794b4b41422"],["/posts/362397694.html","beea151189579fe5c5da12dbf9358e7f"],["/posts/3731385230.html","b938d40be7e8fdf53fb3b4ced6f473a8"],["/posts/3772089482.html","f8874b63e1ee0448eae2ecec1ab67f14"],["/posts/386609427.html","df000a0dbd79ee24a0daefc811836281"],["/posts/4044235327.html","3c14d9613242bd74d3e9c47a37097d09"],["/posts/4115971639.html","b6827e1a4ca0473a1076e5bb38f2c5e4"],["/posts/4130790367.html","0d15701f905e439fff9c0a6092a7a315"],["/posts/4131986683.html","5068bfbe1cd60f1397c8fa88013d846b"],["/posts/4177218757.html","e93e088905c2e1c6f2388e723098fa65"],["/posts/4192183953.html","53329074e50d65e3b2fd709e489fae0f"],["/posts/4223662913.html","9e40e3713a20c50f089f1ce9a5c6a101"],["/posts/4261103898.html","536c78e4e7339984ef6cfb560e61b66a"],["/posts/4286605504.html","101f1478c7045842337a2c2533728f7e"],["/posts/449089913.html","d00b9fda832d4f2230abfe86fa2fd263"],["/posts/469711973.html","af86d9b63a2fc6e6543f3eb28139df2e"],["/posts/482495853.html","8238b561f424d31a32d583e91f9868e7"],["/posts/488247922.html","6a56c72922c4d73ac838c96d75d10084"],["/posts/517302816.html","2dae334c953ae884ab6b2eaf3567d4df"],["/posts/570165348.html","a8fcfef1cd85ad775dc1db198467c0b6"],["/posts/595890772.html","1b775e450db0131919603e5112ce5302"],["/posts/67485572.html","4788f9c10d2be74ad181b3763edda9ba"],["/posts/694347442.html","2dc67e205d5a88d8f5f6ff0c3c66d841"],["/posts/707384687.html","c1db60d77812b6e7aac99449b737c04e"],["/posts/71180092.html","a4563ec7f39ad94b745709fd9965018e"],["/posts/716459272.html","1c00b81544fb003ee2de097238866854"],["/posts/765481613.html","503681b71bd5e057f59fa2c3d8930f62"],["/posts/778231993.html","c2c9447c24d1048c3ab01118e932f6c5"],["/posts/795397410.html","6aff9409e5697e9856a70f4cea3dcbb9"],["/posts/820223701.html","ca3be1a4b6c5f009181f4d689acee706"],["/posts/830372185.html","9972a07fa589ba799e7791f16c0dfcee"],["/posts/88294277.html","1d475c7970d4ffb74d1f44d31e65ddb3"],["/posts/939963535.html","128caf4d88a59eb62ca613ec732d1a7d"],["/posts/983786067.html","49e446a23f599cb79c890dfddf44016f"],["/sw-register.js","13d266ee0471c559b1e30b9d51c8ebde"],["/tags/C/index.html","acf665739e6c6f1705ff21f9c96e501f"],["/tags/C/page/2/index.html","5725a016adf5f2a6c7c2a80d62ae526f"],["/tags/C/page/3/index.html","dddb8d62fb2d74b8647111b5e8e06ba9"],["/tags/C/page/4/index.html","d6e6b2ab6bddc633427825baa303e044"],["/tags/ETL/index.html","55ad2ae9015a3252e1ac08a350563bef"],["/tags/ElasticSearch/index.html","b7faf6c82807fd31842d54ea2757c868"],["/tags/GUI/index.html","2269bf0d10b1b1c8748e6c3c6d0a2d41"],["/tags/HBase/index.html","eefe65a058a5b69ed92e223136230bc6"],["/tags/Hadoop/index.html","c360439bbc8600f9185a794acf338386"],["/tags/Hadoop/page/2/index.html","eb8f149d0965ecb3087186013d99f950"],["/tags/Java/index.html","7e920387b044da1a86f4990f3ff35b63"],["/tags/Java后端/index.html","72db82496c8e08f5f18c1d95e65ae100"],["/tags/Java后端/page/2/index.html","1dfc1ffa7c152aab20b75999c836ad5b"],["/tags/Java基础/index.html","0c9bd9ca5dc82501e9611b4e1ebf6b63"],["/tags/Java基础/page/2/index.html","8229edf6153c60ce47806d108ae5b810"],["/tags/Kettle/index.html","2ec5e2a6b6c9eb01674bd350a1f029ae"],["/tags/Kibana/index.html","e75b1bbb16a1f10995b837bf164b8274"],["/tags/Linux/index.html","ba5a984d22f5460a61e09374ab2bef64"],["/tags/Linux/page/2/index.html","49ac659fc3649778e98d3915a09b0632"],["/tags/Linux/page/3/index.html","98fac8ed417c35ad2383f23e14b3a908"],["/tags/Mac/index.html","6e430e4f670db0e23eea6166af748775"],["/tags/Mac/page/2/index.html","774e5b6a90c1cda57673e18123c517e0"],["/tags/Maven/index.html","b81e1d2a403e033fd40086869216f98f"],["/tags/MySQL/index.html","c100f16d5c078ebd254fd1124f66c3c0"],["/tags/Python/index.html","1b0933ba180ffcaed7bcf89fa648df80"],["/tags/Redis/index.html","0ce450083c8a0948e7be437ca40edb8f"],["/tags/R语言/index.html","ecd588fea9f4cfe986c8786fac6f61dd"],["/tags/Spark/index.html","ddbc20a3e62dc7d7f623998b85ea2877"],["/tags/Ubuntu/index.html","91673c4d61133fae0779f9222dad551c"],["/tags/Vue/index.html","5df3f543b334c8ae0b6c428d762605eb"],["/tags/Windows/index.html","d9aeb6228cd9ca2b8cbd4584cb437e13"],["/tags/ZooKeeper/index.html","7fea83c9f3025cd674a9761b5edbbedd"],["/tags/bfs/index.html","7158eb9bf1d6f9b6de8a640e10888953"],["/tags/dfs/index.html","39f3fe22a3ca1edda41c76567bc2cdf9"],["/tags/folium/index.html","9f3d25ddd32089cfa375daf9a19fdf1e"],["/tags/git/index.html","afe781e22d00ecaa0d5fad655f6e38b3"],["/tags/iPad找电子书/index.html","44c310c35cf1feb74a7057c99b24b179"],["/tags/index.html","8df5bce1ca2ecd73e3fb1d7891ce82a3"],["/tags/latex/index.html","09002e31f7dfe33ba853ecc240c5b2e9"],["/tags/中间件/index.html","2d7027776176297ff82721655af74891"],["/tags/二分查找/index.html","35f12b08c7e3cb0f3f44f61cb9f947a7"],["/tags/优化类/index.html","43262470727e19ace5f41a920f5128aa"],["/tags/前端/index.html","790efdeb22fc9a4e2f3aaefd6c1d94b2"],["/tags/前缀和与差分/index.html","9538db44366750560911333874a85dd7"],["/tags/动态规划/index.html","24109ba5523dabf6364782b4fccb9d1d"],["/tags/动态规划/page/2/index.html","b400c73c0959e39ab86b83b9815128b1"],["/tags/博客搭建/index.html","0dd3296a4663b476a1f1f4275c359fbe"],["/tags/图论/index.html","5e2f6b190ea5d99b3a5e8bbeb6c32c2a"],["/tags/大数据/index.html","2d18737e461635d0345ada6de6415f31"],["/tags/大数据/page/2/index.html","5c0cf6ee8bb34b2ef0717a295c37ae26"],["/tags/排序/index.html","c5f35c4583e930f6c9f4022d511a42bf"],["/tags/操作系统/index.html","bb5f68a29b9a584c7aab53c532830ffd"],["/tags/数学建模/index.html","f9719cab5f8304a25e3fd4ed65b60d35"],["/tags/数据库/index.html","f12588626ba45a6892a1cbf098bd5f0e"],["/tags/数据结构和算法/index.html","320b272fee84b00928a5514414922b23"],["/tags/数据结构和算法/page/2/index.html","7b3427f57b86c338065a66b568089998"],["/tags/数据结构和算法/page/3/index.html","995c1f992e70f5874557311586f8640a"],["/tags/数据结构和算法/page/4/index.html","c12d4b89458b87a217574fd427c90f65"],["/tags/数据结构和算法/page/5/index.html","6e4abda00e855ac3b2ad22b6e2aaa48d"],["/tags/数组和字符串/index.html","a8d11209025f6a399759737bb43ad220"],["/tags/数论/index.html","d0c68404fd648b03991a3f1043e73877"],["/tags/枚举类/index.html","012d54304caaf170c3bd454f6d33ba68"],["/tags/栈和队列/index.html","28b085fadb6b3f6e8e16e34840175c4a"],["/tags/树论/index.html","e6c2ffd51dbd50d9877ba5db0a6df319"],["/tags/测试/index.html","6c5aaf7f4731e0e9fbd60043502877f9"],["/tags/环境/index.html","6d6130a5c52e30f8d9d2499546d5213a"],["/tags/环境变量/index.html","f0db0c9afd0d1fb461382f13c3a65d0c"],["/tags/绘图/index.html","688e1721a753a1f15d5d4ce24fe387e5"],["/tags/编程工具/index.html","bfbc34a9ebb8ab56651384afabba20ac"],["/tags/编程环境/index.html","22522c1d334159028466cd65d9573d1b"],["/tags/网络编程/index.html","4f15473c1deb536cfdfd2e22a43fea79"],["/tags/英语语法/index.html","4cae84d5cd417957fff834840789cfa6"],["/tags/计算机操作系统/index.html","635e0a7022f06b1261d89747a8ef3de4"],["/tags/论文/index.html","a3e70496a3d9f3fd3b1f5261fdd2d0ff"],["/tags/资源下载/index.html","146a065086c703979b58b5368963e711"],["/tags/链表/index.html","a2bd952ed96a77cb5e5cc78996d025de"],["/tags/集合/index.html","4e41e684a3da15436906605d11b35ac3"],["/tags/集群/index.html","64fc63d3ddc3ba37a4fa2dd549eb36ab"]];
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
