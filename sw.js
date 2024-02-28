/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","88287073ee4d90224f7335f5cadd6c88"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","6f771545bc0f6c3290ed5b3134231a6e"],["/archives/2023/02/index.html","8b90dcc73052992dcabc90c742ecb8fb"],["/archives/2023/02/page/2/index.html","c408236232a3359286b77c36e46432ba"],["/archives/2023/02/page/3/index.html","1ab613c0f93a2a174f614a96f0f8cf7d"],["/archives/2023/03/index.html","8fe9ede9308e5ca7dd4beeddc50f95f2"],["/archives/2023/05/index.html","3c45a13ce87247ac001966dce1ea5e96"],["/archives/2023/06/index.html","18a239fca266e71d83848941287f961d"],["/archives/2023/09/index.html","da3e1545206c78d67e1bc574dabd4afe"],["/archives/2023/11/index.html","439140d9b1c535086d688ef29a013bff"],["/archives/2023/12/index.html","36721c4c09f06ad6a6197f8ee91eb58c"],["/archives/2023/index.html","b427d0b9c9cd3aaf0f874134283c1c2d"],["/archives/2023/page/2/index.html","575ee61af2a3997a25a714d6e9dd37b8"],["/archives/2023/page/3/index.html","0435ee208f32c8a1ea11317a32859a9e"],["/archives/2023/page/4/index.html","bb6c99f74e3603fa51857c06ce5fabb2"],["/archives/2023/page/5/index.html","73659285f3a916c11d8d11c74b3433f6"],["/archives/2024/02/index.html","9ae6213b865e5593cac9d3ea4ec5f6d3"],["/archives/2024/index.html","5958e36a0c0633cf7be316bc7cfed7b1"],["/archives/index.html","ade70456f2755d43117d7345e967433a"],["/archives/page/2/index.html","1bd393fd085ab5015fc415c0b629c5e4"],["/archives/page/3/index.html","dfbc27f50c42f8dea818c26a94b88788"],["/archives/page/4/index.html","81c14017a3b29c18887c61078212aa15"],["/archives/page/5/index.html","9fc7ac521d7927a37a2a4ffd8f3116af"],["/baidu_verify_codeva-qQP2iZOMLX.html","2d77d2e190259a93cb84882d717b7bfb"],["/categories/Java/index.html","ee344a73b78fe4fcab0d5425bf9edaff"],["/categories/Java/后端/index.html","e99cfa3f5424c653b21295163f3f9e6d"],["/categories/Java/基础/index.html","59359867077dc5d04204aceda9f208c1"],["/categories/Java/基础/集合/index.html","59b1ef5d3e43dd1e5ac607258bc099fa"],["/categories/Python/index.html","62fac0b7e1ec96a46cd35bb29e0b8984"],["/categories/Python/编程环境/index.html","3dc210742df2b4ba64ef7bfe48f06953"],["/categories/R语言/index.html","ec72f499ba5de84971fc43a0ee8b3b52"],["/categories/R语言/编程环境/index.html","da28cc06b0db558baf94e0f08b1b9aa6"],["/categories/iPad/index.html","66df152bbd93a29faff07ae6b5fdc490"],["/categories/index.html","8d56a761eae54438b04773a9bc5e241c"],["/categories/中间件/index.html","291c05a5de9234bd4237e08f34c9103c"],["/categories/前端/Vue/index.html","45d0fd02b2bf18b364bbd52c2f37197c"],["/categories/前端/index.html","7c5c234f82facc0c6ed119b60841db6b"],["/categories/大数据开发/ElasticSearch/index.html","2d5330112dc9686273cecb1371842540"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c4f91063864b383fc09f73f2285c40a0"],["/categories/大数据开发/HBase/index.html","5394e2f23b05d777644abf680fc3160d"],["/categories/大数据开发/HBase/学习笔记/index.html","19475ae952b72a4cad3903bf3a512fad"],["/categories/大数据开发/HBase/环境搭建/index.html","3169f671cc0176b352b2a8f4c8973cf0"],["/categories/大数据开发/Hadoop/index.html","b8601c559ae70811f61246cefdcdbce3"],["/categories/大数据开发/Hadoop/技术/index.html","d6c46a2e63c35ea3e3f0b3bd3c5640d8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e0ca5388648f1eb8b69fbbc4d13607b4"],["/categories/大数据开发/Redis/index.html","ad87d53efc23b603147edf4525388cd4"],["/categories/大数据开发/Redis/技术/index.html","2a1309f40fe88326eb9ea4d192da4a9d"],["/categories/大数据开发/Redis/环境搭建/index.html","44c891a952c6e0e3d3ff0f330773500d"],["/categories/大数据开发/Spark/index.html","100bc2303aacc1d748f1340c1e24bf8d"],["/categories/大数据开发/Spark/环境搭建/index.html","e11fa62a8febaf67c1c8433a728256c8"],["/categories/大数据开发/Zookeeper/index.html","bba65aac59622a3cbf23663a3683c046"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ab4224e0a76691d4d9b6d1453c31537a"],["/categories/大数据开发/index.html","94d1b6cbf4ae594f3e716edc3fdba91b"],["/categories/学校课程/index.html","aee3f890a8a9e406570e65414937adc4"],["/categories/学校课程/计算机操作系统/index.html","f024f40bb8c9dccb67d5019666662f20"],["/categories/操作系统/Linux/index.html","ed66adad57b7663a299f41d1863b4a9a"],["/categories/操作系统/Mac/index.html","e0e74b8a9375d365b69aeb9e4178ccf0"],["/categories/操作系统/Windows/index.html","c5a9298a5aa80be55d3a4798500ba168"],["/categories/操作系统/index.html","c9a9786e13da03fc9f7738a844c89edf"],["/categories/数学建模/index.html","a60d52d212591651004c05e3dbf13b00"],["/categories/数学建模/latex/index.html","7606716a8ba1ffd262743105301b0c79"],["/categories/数学建模/优化类/index.html","7475d9d6d35ef7dc6c530f9e6c9c850a"],["/categories/数学建模/优化类/现代优化算法/index.html","1b036ddf8e4502667806612a9d709e95"],["/categories/数学建模/优化类/规划类/index.html","4b8b2fbf1e670fc0af809656a055bb53"],["/categories/数学建模/绘图/index.html","91e7f934cfd987296097ffd188eb46eb"],["/categories/数据库/MySQL/index.html","c7ffe36acc8846855eef6efb5c3dc14f"],["/categories/数据库/index.html","97a7422ffcb07aba0449983e05c622a8"],["/categories/数据结构和算法/index.html","233929bc5ec67f97e121ca555302776c"],["/categories/数据结构和算法/page/2/index.html","18087ecb7c8a928d3602baeaa476c483"],["/categories/数据结构和算法/基本原理/bfs/index.html","7627404d53054d9266a3aaa1bd403ce9"],["/categories/数据结构和算法/基本原理/dfs/index.html","9bdc8a4469517b298ebcfce716091e3c"],["/categories/数据结构和算法/基本原理/index.html","e6ed4f61f98600b29bd389b74c284502"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f40ecdc8871a8ebf06bb850b31d0d21e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e6d0d5e895e23f18bb1bde1505a1bfdc"],["/categories/数据结构和算法/基本原理/图论/index.html","cc09c3838adbaf7efbeb7cd0f903a8d6"],["/categories/数据结构和算法/基本原理/字符串/index.html","3618b8c5c081b8c7c9a83f89b6f27a19"],["/categories/数据结构和算法/基本原理/排序/index.html","4eef7af083a093df5fde9d2322d75e0e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","764a0cc1cc0093a170934941f4370e76"],["/categories/数据结构和算法/基本原理/数论/index.html","e2e88c0384cfe5852545c8e2d98e3d54"],["/categories/数据结构和算法/基本原理/树论/index.html","8073cc756be1d46c4c787e930b901fcf"],["/categories/数据结构和算法/基本原理/链表/index.html","95327e1476ee179cc062a4a66c445245"],["/categories/数据结构和算法/算法题/index.html","7caee6f3602bf371eddfd675601356c6"],["/categories/数据结构和算法/算法题/二分查找/index.html","0e2181f9b7241539b503275329ad41e7"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","8a6271683c4d25da48cbaa959571024a"],["/categories/数据结构和算法/算法题/动态规划/index.html","df326033cd4be371500de91cdb8f3180"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a6207cf575f39b54b958be2c3c16ba0e"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","98015a3a97e0b6fa85ea683ea0cbdd44"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","44faf13aec68faac8238d07c55fde354"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","1af895342c5b0c63cdc0801510ac0215"],["/categories/数据结构和算法/算法题/数论/index.html","268a613375334da64c7e2a7ccecf2a17"],["/categories/数据结构和算法/算法题/栈和队列/index.html","adca206d8b492217309de90856069fba"],["/categories/数据结构和算法/算法题/树论/index.html","c6606210d70713b982af72a8f5d58f1f"],["/categories/杂七杂八/index.html","a8076ea9bbb214a82636078e24b330b7"],["/categories/杂七杂八/博客搭建/index.html","1dad27c5a75f212991b8ad227c1dae47"],["/categories/编程工具下载/index.html","25c500177d6c5a4a8ebd6995d1030087"],["/categories/编程环境/index.html","3bdf2dcef9c649b55b16729fa304a4ab"],["/categories/编程环境/大数据/index.html","a2f18c813eb03f63d4df0235be601fe7"],["/categories/英语学习/index.html","3c99e6a63ef54faadf2b9f6e50919705"],["/categories/英语学习/英语语法/index.html","a0f0bda97e08cb73df584c25d09c8792"],["/comments/index.html","07460ae0ee7e20a6b73a4faf0bb00fdf"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","828f07fb8ba3cfd34e411b793a7212b6"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","b82cd5a51de3e0c94f6cafeae10b823c"],["/movies/index.html","5dc8b9aff7010085924160cdac6d2663"],["/music/index.html","8184488dd988b57b2e9015672c270e0b"],["/page/2/index.html","29050ea0f265bb26b70edc9601ae4282"],["/page/3/index.html","62cca8ead3068cab82cf15d9e059cd86"],["/page/4/index.html","0d6e629170f844890c42527ddc782267"],["/page/5/index.html","5f2376f5503595d59f4987a2fb0d4825"],["/page/6/index.html","5e5226f689f5c7efe1e25d1025efbb20"],["/page/7/index.html","03a47c91a44cfbaaecca099280191439"],["/posts/1021360842.html","1e668a61845a007d7c8690a00ce7f168"],["/posts/1120620192.html","865b9f6f45478c70a4bded9e97407254"],["/posts/1137707673.html","d8099683a703b0091088308cb23acba5"],["/posts/1141628095.html","28ce59e4dd547b609e5e15078c7257a5"],["/posts/1168613674.html","c197ecb888b8e59997864fd0f30b8f18"],["/posts/1219920510.html","42111a438de0f4bd4043ce844c1214ab"],["/posts/1222166338.html","dd2d1f9788bb021bc047478c4611b595"],["/posts/1259097482.html","04b115e69ba503549472ff567633dd24"],["/posts/1271036369.html","eac7bd9818e04a81017b0b084391324c"],["/posts/1312847445.html","3609eadf9748d1122d978f9e3902f546"],["/posts/135355774.html","b80b81905eab430d1a89ef335021c8c8"],["/posts/1375344716.html","fe4de5bd23641d645e4386b7d35d70f6"],["/posts/1388991698.html","479596d4d420063ecc20310fd787280b"],["/posts/1410315814.html","a8ababa2143b3d8d713c7486ea53d8a0"],["/posts/1452790229.html","660d2dc92c9287f183c4163410097bfc"],["/posts/1470079884.html","dfb2a5f0a7e741859f582f41cf366718"],["/posts/1470079885.html","ec275c4fcef101909b171e52814c557a"],["/posts/1470079886.html","c4ca7c520d14d3438ea18401695245f4"],["/posts/1470079887.html","b67a68cff898df639f1f6b9e33772ae2"],["/posts/1498536549.html","4ecb4a2080b7d80a082208db5fc142fe"],["/posts/1539568593.html","f670afe707e91e193d5da179edbd2a3a"],["/posts/1547067935.html","75b01152e3c209971d9ed1521e38f22f"],["/posts/1557866301.html","dd099ee1ba50bab544846ca492584fa8"],["/posts/1571776361.html","8d272b0847a74616723caa81110b8142"],["/posts/1605124548.html","b137f0141abebfdd00c539baae7c806a"],["/posts/1633036852.html","0f20cc485a18da36e00608556c33b5d5"],["/posts/1667740714.html","1bf134a14b5ec16b274d61f83febf0cc"],["/posts/1674202625.html","df9bf729f3f0c33039898a92f2b26006"],["/posts/1765123828.html","6ce56de6da75d28266c8046f52dd1473"],["/posts/1767336200.html","0fec66bcaec0f485b9e48f80aeced941"],["/posts/1776114197.html","e0f3972852d3cb9d6e0f6a4d7f76bbe0"],["/posts/1817748743.html","ad398404938f198a0d741b877b20d1b7"],["/posts/1925125395.html","1c85b025809620736b8f0f8877b38b57"],["/posts/1966191251.html","8678693359b17d252202311b12024cc8"],["/posts/1987617322.html","43fe1203bae80089967b8e093d7c2852"],["/posts/1999788039.html","eb4f870a8c9bb4210a8adca27021bc98"],["/posts/2075104059.html","8ae798e49f7a64242be63a264d3b0fa3"],["/posts/2087796737.html","cc80f644236ffd77697e878877e3b7b1"],["/posts/2106547339.html","8fa7fb7736a74a166dc12b65645e5417"],["/posts/2207806286.html","15b458da6fed1a35ce27a98af82a285b"],["/posts/2225903441.html","6904eae7ea2cede9a0a52aebebe7ce87"],["/posts/2265610284.html","67e3d90b512b4b30161d0e09015998ad"],["/posts/2281352001.html","c61d6ea5e4f2d7aa342afc4f3f4a98aa"],["/posts/2364755265.html","2ac01fe17bab060c66f90e1626dc4c12"],["/posts/2414116852.html","74b3fd450147c23eff4601a7104b1572"],["/posts/2421785022.html","7a6391c27ef1c5b8127970606c51f135"],["/posts/2482902029.html","96470aef93208e5ea4f611219a8a4f62"],["/posts/2495386210.html","41eb91a7233329e6cc8cae781bf5f617"],["/posts/2516528882.html","f6a0573ff19e24ea15a47e6e950e2c71"],["/posts/2522177458.html","2e280b3187180d1a360e01f1755f7933"],["/posts/2526659543.html","1de309b8679f84882f8831cbf64b2716"],["/posts/2529807823.html","89b2c1265e9b6bf9ce26b2a2469d681d"],["/posts/2596601004.html","ca1f95adeed69216e5e90de1acc2f3c5"],["/posts/2697614349.html","bbcd2037db1d5e34570d0c273a7f7d0f"],["/posts/2742438348.html","485613d284bad9515409600e391f83a8"],["/posts/2768249503.html","7f166a2120d51acf02cc5006071690b8"],["/posts/2864584994.html","49c7cd2c98b31147a054de037d6c5d5d"],["/posts/2888309600.html","1da031ba9bb6f326f21950fa9a505cef"],["/posts/2891591958.html","9f7520c2a900884cf5b5f0cdfc380f1d"],["/posts/2909934084.html","1a86e2eec95d8342e28123014dcadfb1"],["/posts/2920256992.html","325d079485de5b0b52b44ff6920616f8"],["/posts/2959474469.html","33512ec9da8fbc788606c0e89d8c20d0"],["/posts/3005926051.html","49a6d433fca275b546892c5f93d1c034"],["/posts/309775400.html","603982b29e635493e8809da7e157fbfe"],["/posts/3156194925.html","4649fee70fffe12889793b3f8d7503fb"],["/posts/3169224211.html","b2bf10fb2ed2b936aef289fa1ed759eb"],["/posts/3213899550.html","d91cd03736a9d7f0c8d7b105c8bb95c1"],["/posts/3259212833.html","063f68d593f1618f909cf5f00f543e12"],["/posts/3265658309.html","3f4c642a1a44079a48c8ad376966abc5"],["/posts/3266130344.html","a52dfc471f058fbfe8772bd6e7b32f0b"],["/posts/3292663995.html","5f564b0991a79ddaae4cfc0f77923008"],["/posts/3297135020.html","2e81580aa23bdc4909d9993bbb1e5002"],["/posts/3306641566.html","f245e074c0d8e9e60d9a1500f66ce9dc"],["/posts/3312011324.html","632bdd05cc98607ab4639656e366e2f7"],["/posts/336911618.html","c753f689046e0448fbddf4bb5641df80"],["/posts/3402121571.html","6e0122a47a2abce4976fac13894fadff"],["/posts/3405577485.html","2dcc96ffab6d779d350e434fdbb22402"],["/posts/3498516849.html","563d98a648a6ea8d7551dd6edc0fc18d"],["/posts/350679531.html","c753bc5c9e424b9d7963bc0d22f242a3"],["/posts/3513711414.html","04a72112602ac48d5ef875e0a0943b5c"],["/posts/3523095624.html","dd4fe7cf98eaad5767c9ca45dd63c0be"],["/posts/3546711884.html","ad1b4d7e0f30c91cd0ec0794b4b41422"],["/posts/362397694.html","beea151189579fe5c5da12dbf9358e7f"],["/posts/3731385230.html","b938d40be7e8fdf53fb3b4ced6f473a8"],["/posts/3772089482.html","f8874b63e1ee0448eae2ecec1ab67f14"],["/posts/386609427.html","dd14bbb84f2714edd70982188f114355"],["/posts/4044235327.html","0224c045a5a0cb896a5526bbc2625a36"],["/posts/4115971639.html","b6827e1a4ca0473a1076e5bb38f2c5e4"],["/posts/4130790367.html","0d15701f905e439fff9c0a6092a7a315"],["/posts/4131986683.html","5068bfbe1cd60f1397c8fa88013d846b"],["/posts/4177218757.html","e93e088905c2e1c6f2388e723098fa65"],["/posts/4192183953.html","53329074e50d65e3b2fd709e489fae0f"],["/posts/4223662913.html","9e40e3713a20c50f089f1ce9a5c6a101"],["/posts/4261103898.html","4d8cf000c1803e77697a01d99f78da3c"],["/posts/4286605504.html","101f1478c7045842337a2c2533728f7e"],["/posts/449089913.html","d00b9fda832d4f2230abfe86fa2fd263"],["/posts/469711973.html","af86d9b63a2fc6e6543f3eb28139df2e"],["/posts/482495853.html","8238b561f424d31a32d583e91f9868e7"],["/posts/488247922.html","f4d4a8e1543d4889d9e15ec03771998d"],["/posts/517302816.html","2dae334c953ae884ab6b2eaf3567d4df"],["/posts/570165348.html","a8fcfef1cd85ad775dc1db198467c0b6"],["/posts/595890772.html","1b775e450db0131919603e5112ce5302"],["/posts/67485572.html","4788f9c10d2be74ad181b3763edda9ba"],["/posts/694347442.html","2dc67e205d5a88d8f5f6ff0c3c66d841"],["/posts/707384687.html","c1db60d77812b6e7aac99449b737c04e"],["/posts/71180092.html","1884e65149df98a8f16be10a8b1ed1f6"],["/posts/716459272.html","2ffc33390048be2d2810e86994d8c4f7"],["/posts/765481613.html","d1e5cc9595a22513817e3ca9699bd8f5"],["/posts/778231993.html","c2c9447c24d1048c3ab01118e932f6c5"],["/posts/795397410.html","6aff9409e5697e9856a70f4cea3dcbb9"],["/posts/820223701.html","0fd3ed6cf4b5709d477fee8af283a646"],["/posts/830372185.html","9972a07fa589ba799e7791f16c0dfcee"],["/posts/88294277.html","204551b3d23a88310cbe61c9c3e97ca3"],["/posts/939963535.html","128caf4d88a59eb62ca613ec732d1a7d"],["/posts/983786067.html","49e446a23f599cb79c890dfddf44016f"],["/sw-register.js","1c3d7f79b600ca637b85752dee94f0f2"],["/tags/C/index.html","86e3c9318223a434bd24bd58c69e5bd8"],["/tags/C/page/2/index.html","cc0024c7b41620f3ead08f422f1d36a2"],["/tags/C/page/3/index.html","33bdb24e1e68106a31a2ae075aaeb954"],["/tags/C/page/4/index.html","e2d1b91b5b6c20e4d5f6467cf5691479"],["/tags/ETL/index.html","d1097e77ba81f7ec88140211b8244183"],["/tags/ElasticSearch/index.html","baf8660b8062c0d93dcb61d597ad7214"],["/tags/GUI/index.html","a951294013a5e51713d4a8974358d4ca"],["/tags/HBase/index.html","a16a64dbc6176e14292cdf753dc40fc8"],["/tags/Hadoop/index.html","4ffeba489c717c87f2de374586ce6e2c"],["/tags/Hadoop/page/2/index.html","e9b44f8646f1faf74f4090862c0fcf81"],["/tags/Java/index.html","5338e1d34cce83a2174f5ec2dae62f9c"],["/tags/Java后端/index.html","f3fdb0671cfe4a4aadb82c4ce46601e5"],["/tags/Java后端/page/2/index.html","55bac7ab865ffbad543bd9f3f4cbdace"],["/tags/Java基础/index.html","822b93a3f8a02540df3713d90fa0252e"],["/tags/Java基础/page/2/index.html","3bef63eaf732abdcd1fd9e81bd550247"],["/tags/Kettle/index.html","592cc6799c3d8be804764ae47f99a0fe"],["/tags/Kibana/index.html","38ba9c35709d872aa37b52de077b15c4"],["/tags/Linux/index.html","f1520987a6a8a5ec46189e480423fde1"],["/tags/Linux/page/2/index.html","1f3abda24edfbb3fd685ac61947eace5"],["/tags/Linux/page/3/index.html","a848ce60e83c7d397235ee316f51c118"],["/tags/Mac/index.html","2c7df063690bea33b8a1996ed7b71ac8"],["/tags/Mac/page/2/index.html","a7fc73f9616b47cb10734ade128f49e8"],["/tags/Maven/index.html","660bfa9c4229dd3ed7785eca258f209e"],["/tags/MySQL/index.html","b900319cbd3f2b4d19d5684571e2c473"],["/tags/Python/index.html","71deea7045dff61dd06123a10762eecf"],["/tags/Redis/index.html","c97c9539bde7059cad120ff2c4a5e7d6"],["/tags/R语言/index.html","7027a21c9f34202900cea5c675fa7e22"],["/tags/Spark/index.html","f1606d90e9c12e855fa1797da9cdbad2"],["/tags/Ubuntu/index.html","22ce0eac2f28c24d817d220305671986"],["/tags/Vue/index.html","21efc7add84987e6ac413b7891e004f4"],["/tags/Windows/index.html","07ee65e3cf01455bfdac3a22eca8f5c1"],["/tags/ZooKeeper/index.html","a88419530cd69d06a59cfea2c8355a13"],["/tags/bfs/index.html","7980c5929c9ffc5e13740016049b62fe"],["/tags/dfs/index.html","a4eae9f08f392b76d76131b05e7ad29e"],["/tags/folium/index.html","e413a9c2f11f2f5c0ea4b5779b65b8e5"],["/tags/git/index.html","6975fcd06ed0b809da2d5857b98d3a67"],["/tags/iPad找电子书/index.html","d8d86cbc304c2c37055809bf0facf397"],["/tags/index.html","dedfec8365bcd101d3ae1b1bf3a09270"],["/tags/latex/index.html","436c294e9e1fd861420d6f1648df2656"],["/tags/中间件/index.html","d8463b478c72004b479ae6151460e85d"],["/tags/二分查找/index.html","6f5115d41939753115b3062e9cc7e220"],["/tags/优化类/index.html","32bc98fb949699e278d9257b1060b738"],["/tags/前端/index.html","a4023960fb47ca18e7629b34c458b1de"],["/tags/前缀和与差分/index.html","f3a1984b52b86284e9c43492259dff09"],["/tags/动态规划/index.html","b23aad329c373a9ffbdb903803cd1337"],["/tags/动态规划/page/2/index.html","a8b69545d0daf00e13be502264c95200"],["/tags/博客搭建/index.html","fd49010ec4709c64956cdf62bbd0fc4b"],["/tags/图论/index.html","e24c92408d4d3e9b3352ca827755bd0a"],["/tags/大数据/index.html","898a9ef2b75ff502d849ce729cbf0a8d"],["/tags/大数据/page/2/index.html","155bffbba89828e3a6360478dc79256a"],["/tags/排序/index.html","98bf2ec02de07c4a8f22f2b57d8d9908"],["/tags/操作系统/index.html","f558a528cb9f4d3d64b2016f7a26f597"],["/tags/数学建模/index.html","c9bf83448d85fda2ad579538e488146a"],["/tags/数据库/index.html","c8eea87c71aff203ed14de69f497e5f1"],["/tags/数据结构和算法/index.html","397bc8e97c4815002827fec313ea3f22"],["/tags/数据结构和算法/page/2/index.html","c3b0acec131c31094460d65853cf4c37"],["/tags/数据结构和算法/page/3/index.html","2e6bb850681a2012e75d179fafa7ba93"],["/tags/数据结构和算法/page/4/index.html","842be910885bd803e1e7752156922673"],["/tags/数据结构和算法/page/5/index.html","611141666d5203d7bbe2a81143abf20c"],["/tags/数组和字符串/index.html","ace3a759995864175b78d15f9411accc"],["/tags/数论/index.html","78cff3d42b5a0a759b6db30c1d2781b3"],["/tags/枚举类/index.html","e9c81683401ba3beec192effafa41771"],["/tags/栈和队列/index.html","7d675e3736170bbe35569d0fbd2a9bf9"],["/tags/树论/index.html","0f9a15b6ec912b6ee589c6662c1fffdb"],["/tags/测试/index.html","aa4e133ce2669139b704b0f553974daf"],["/tags/环境/index.html","60391924051f1f9a8da6f12f30dcbbdd"],["/tags/环境变量/index.html","bd76ae1d81071e5e69c9c48008a5a5c9"],["/tags/绘图/index.html","9a87d05f3ea3b01c56f12fcd0510e360"],["/tags/编程工具/index.html","87545552efa7a4253f2e72236cac4345"],["/tags/编程环境/index.html","b0bfcd1febd5a0b28962d8d9b730bac2"],["/tags/网络编程/index.html","f1c8d227950b40353cf2ad7b2e5f8c17"],["/tags/英语语法/index.html","eed613bdd016969747b49c2acbe7a499"],["/tags/计算机操作系统/index.html","7ec144b4ddb5bf01f798e03c74b181e0"],["/tags/论文/index.html","d16d851ad56ee0997c300c7751802fd3"],["/tags/资源下载/index.html","dccd751900a04beb93ab06b8fe1ff3c8"],["/tags/链表/index.html","dce19b32c2470e5a296d47044a24573f"],["/tags/集合/index.html","1c4b0996cce831748fd94c55ec500fdc"],["/tags/集群/index.html","0320f7d706e6989347a3678a0559f430"]];
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
