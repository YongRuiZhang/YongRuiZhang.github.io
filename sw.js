/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d468f3146cc4d78a04e0e1d2ba6b496e"],["/about/index.html","ef028eaa708f2854c1c55c36a976f153"],["/archives/2023/01/index.html","ef0382d123a037b8a3f75b25a9f0c810"],["/archives/2023/02/index.html","01c4bf549cc41ea6b3e90a3961ea8405"],["/archives/2023/02/page/2/index.html","5e833b1dd2d8385fe6e877ec5497e879"],["/archives/2023/03/index.html","efa082478981e5a1d536749ccfe0c466"],["/archives/2023/05/index.html","7799e52d60b84a184acc82ce7e285b8f"],["/archives/2023/06/index.html","5f461bde373ce76ed92d6b7c27f9769d"],["/archives/2023/09/index.html","5c35d81fb25d7d6042e8c1363fe0e6f9"],["/archives/2023/11/index.html","27efad340a27945c33b03ed638451cc3"],["/archives/2023/index.html","f3c0dbddb6b72b8a523c90781a8260f0"],["/archives/2023/page/2/index.html","cd972da749b58cbae16668a063f71535"],["/archives/2023/page/3/index.html","db03cff732b3ab23d6b53211acf81852"],["/archives/2023/page/4/index.html","2fe504b74ad0c65e67fbfc6470cb072c"],["/archives/index.html","1f25f030aa7717f5f97db50d74da994c"],["/archives/page/2/index.html","bf9c42419244c5f6712e63dbaaa2a0e5"],["/archives/page/3/index.html","b9b6cec44c884f4691c918e1656843fb"],["/archives/page/4/index.html","3e0f56d3403d05f5a62c48afda3efd7a"],["/baidu_verify_codeva-qQP2iZOMLX.html","db712ed773173d4aec221061510bf157"],["/categories/Java/index.html","d045d80b91ee80fa14abeacb46df6e97"],["/categories/Java/后端/index.html","9fba2e05c8860980de3052a567cf6151"],["/categories/Java/基础/index.html","ab54209cd255e80a67e63b7b7caa409b"],["/categories/Java/基础/集合/index.html","78795b13cc5a391940b0030d0f29f025"],["/categories/Python/index.html","272cc5305af373329dcc6fa9563b25c2"],["/categories/Python/编程环境/index.html","b8b331ae0d7c481748d3823d60239717"],["/categories/R语言/index.html","c21233a266bb9bc6010da8eea5cd4b54"],["/categories/R语言/编程环境/index.html","c699b9263ee8c885aeb54ae610986e3f"],["/categories/index.html","19e7576ff65c6f32ce5a06a9bd97a51d"],["/categories/中间件/index.html","2f7833699544d152bdbe07d1a4dfbbd3"],["/categories/前端/Vue/index.html","476cbfc40d085de064a91f891d689134"],["/categories/前端/index.html","51116a6c7443a0ecf39d00c2cb610977"],["/categories/大数据开发/ElasticSearch/index.html","398dbf4e7dd4513c96280acf520461f3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","cf08a89525f9651c4d778088ed7fb215"],["/categories/大数据开发/HBase/index.html","05a48dc49f88c98fcad6b3264955ac3c"],["/categories/大数据开发/HBase/学习笔记/index.html","141a442b6788ad3f7e4464536bdd8328"],["/categories/大数据开发/HBase/环境搭建/index.html","dcf986ae7981dc9fc8810d28a1f08731"],["/categories/大数据开发/Hadoop/index.html","d3b24b3ad26147dc5a991110f5de30f3"],["/categories/大数据开发/Hadoop/技术/index.html","bc574b644baf5c70bec2f10eb1a33250"],["/categories/大数据开发/Hadoop/环境搭建/index.html","fc390ff9f2b4f5be743e3f9a81671b55"],["/categories/大数据开发/Redis/index.html","0dc3a09434b240abce645ca630ec0e2d"],["/categories/大数据开发/Redis/技术/index.html","401370ff0127d343c4bad8a699578dd5"],["/categories/大数据开发/Redis/环境搭建/index.html","f8e0390f2ce9ca635cd79a835931638d"],["/categories/大数据开发/Spark/index.html","745bef94238fe509e761f7d171850daa"],["/categories/大数据开发/Spark/环境搭建/index.html","143ace80b886da345b92f04b0eb7bb39"],["/categories/大数据开发/Zookeeper/index.html","3a8e98c9128b3ba5e46e6b7226fdbb59"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","89fc2e1f9f83be1a028c9f826248d78c"],["/categories/大数据开发/index.html","6d078ddaed764f7f87816a5fce5d9a23"],["/categories/操作系统/Linux/index.html","bf4faf900bb13c0092de288ca131e606"],["/categories/操作系统/Mac/index.html","c37fbfb8c64cba44cc06aa81d9173748"],["/categories/操作系统/Windows/index.html","d044479954c6860a0f8c43dec5abd4f8"],["/categories/操作系统/index.html","d291ec114eadebe9e5e979c83054be2f"],["/categories/数学建模/index.html","f2b864b067ea50781c86c4c0f9c995b8"],["/categories/数学建模/latex/index.html","563f453cedb4afd082e09e7c97b2b13a"],["/categories/数学建模/优化类/index.html","551c23174bf2ed62cc67baf91ea6c4a6"],["/categories/数学建模/优化类/现代优化算法/index.html","a7f73724492c9c5836d6fd83cbb05a06"],["/categories/数学建模/优化类/规划类/index.html","dbce03ba53851dc2beefae1cd2431bab"],["/categories/数学建模/绘图/index.html","9d2db28b749df653d8bfd04d3bb36a78"],["/categories/数据库/MySQL/index.html","cfa31f2935d3c2080eb8aa0ce9008cd3"],["/categories/数据库/index.html","987b8217e8ad03618092ea8049c058cc"],["/categories/数据结构和算法/index.html","b0e3ca602575b7dc1ef226add2d63d55"],["/categories/数据结构和算法/page/2/index.html","a0646706201eedcaba8f872a15cd4617"],["/categories/数据结构和算法/基本原理/bfs/index.html","99ab1d8c4cfe030519350201b233ec04"],["/categories/数据结构和算法/基本原理/dfs/index.html","62e299d4eea1c441cd1e104ed1b5be31"],["/categories/数据结构和算法/基本原理/index.html","903b3028af8f6471bd91a7d000ccd409"],["/categories/数据结构和算法/基本原理/动态规划/index.html","affab633fda7f782f3160dc164f1f6f7"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","59e486d9fa3c5fb68caa736b7574cd85"],["/categories/数据结构和算法/基本原理/图论/index.html","289e062b1c42882a56baa0c8d19d5fa4"],["/categories/数据结构和算法/基本原理/字符串/index.html","83bd160c97df2e5eccab2e21a5882bb3"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a71a9ffb972e9f4e133de6b26cb72c93"],["/categories/数据结构和算法/基本原理/数论/index.html","3d33168b6608184e29ffc69b18e5587e"],["/categories/数据结构和算法/基本原理/树论/index.html","ca1d0cecb95a997e493cbbebd751eb92"],["/categories/数据结构和算法/基本原理/链表/index.html","df26e94504de0e2c3bfac5130ecc61be"],["/categories/数据结构和算法/算法题/index.html","bfc8512dff0ce4d6f4b1ee8f8aaad4d3"],["/categories/数据结构和算法/算法题/二分查找/index.html","7ab806f2404baf401d6a442bd42caec3"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","abcb093093d16f803fcb042dc347ee5b"],["/categories/数据结构和算法/算法题/动态规划/index.html","8e6ed6be9c3fd37dcf462d07133769e1"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","04ca6f24be8daffd51505ae7c693883b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","66867e493a3cb832ec0af3af3a1fdcb7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","13cb47a453efef88785764d10da3e104"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e525c5eab2e3e01329f8bae90dd5556f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d92e513271896fb06245ce3c9dabfc1c"],["/categories/数据结构和算法/算法题/树论/index.html","74ea0ddc61afe1bd0bbe9cf5d1e01015"],["/categories/杂七杂八/index.html","f51d38823a7e47d64ce1895d9d698901"],["/categories/杂七杂八/博客搭建/index.html","b5eb87252432663d1f1d4b468a91ad36"],["/categories/编程工具下载/index.html","27ede7d5855622357b570f2bccefe20a"],["/categories/编程环境/index.html","7334c2aa039acc331e31d811c2143a9f"],["/categories/编程环境/大数据/index.html","bf1bee2e4da1ddc6e613139201250886"],["/categories/英语学习/index.html","dbda1057071ce89bd7ed7d9a3269f2f9"],["/categories/英语学习/英语语法/index.html","434f1f9181a1819daf723374a643bf7c"],["/comments/index.html","cb17eb6631e5a5dee7ff4afe376ba304"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0faa3f052935361921cf3451402581da"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a690a2d684a256894b46592821e9d1ae"],["/movies/index.html","b6dcc7da997308eb37ad3874feda3dfe"],["/music/index.html","2b7b3e2b5b2137751a5a44bdf85bbda3"],["/page/2/index.html","f5aaf4ea346c01a78f5bc8489859b6b3"],["/page/3/index.html","98d5ada2bc222d2bf3c9bbd3f4cbb1b6"],["/page/4/index.html","0d7f0561b4fc5b98f07da0115d516031"],["/page/5/index.html","0be1de9638d83841148f643d45a5960b"],["/page/6/index.html","870d79ede0d0f5f3969a44aadfd77067"],["/posts/1021360842.html","75a20a2db2cbd83a8e6ddd69ce3b91d0"],["/posts/1120620192.html","e78b334bc2444d75ce6e2cb8ac7e6361"],["/posts/1141628095.html","4460d3d6b85f09e4875125eb6f0b969a"],["/posts/1168613674.html","5b16732b1d170188b567c54dc8b19590"],["/posts/1219920510.html","18d09a694f7b29d5bb371866b1a0e474"],["/posts/1222166338.html","b91bae38cf67812a409881fe34a7e8bb"],["/posts/1259097482.html","bb7764cbe9f98a63f570cce979f4c0dc"],["/posts/1271036369.html","fbee219c8223ef8735776ba8d3468aa4"],["/posts/1312847445.html","4c15c40dfedba6349ba95e1bcd647446"],["/posts/135355774.html","c7a0dd9705f576e7a34a29f2ad381c55"],["/posts/1375344716.html","196e7a10467db579099fd53e405deff4"],["/posts/1388991698.html","7701c5e895d0ff792948d3369bd39571"],["/posts/1410315814.html","63d8ca8dc21df15c2755139e3a967f1e"],["/posts/1452790229.html","d959144c01a43cb6e4b38393004de0b8"],["/posts/1470079884.html","b6709b41f1e728b298ca1a4caa93e186"],["/posts/1470079885.html","539b1410d9f2b98fd3becbfa9985ae3a"],["/posts/1470079886.html","e51b3520b71ac836da74dab9dd336da1"],["/posts/1470079887.html","7df42ff46028dad64b1041692232707e"],["/posts/1498536549.html","338984c25eb65a5857499ead2499dcaf"],["/posts/1547067935.html","b1d613ef02c13d00340a09beacd0f38c"],["/posts/1557866301.html","5461f441d5de2c19659e9e27e5fcf3f2"],["/posts/1571776361.html","673b8029fbcefe6de02f812d60b59e8a"],["/posts/1605124548.html","bc9097199eebc4e0fa03f82449cc1ac5"],["/posts/1633036852.html","81a5b7016c3204f1418bf5f413dd0b92"],["/posts/1674202625.html","f818996942744a3f8f832fa9f351b2f5"],["/posts/1765123828.html","c813009b2c0a7470359f675c022efc17"],["/posts/1767336200.html","93e3658149f1f9b596167826b9d184df"],["/posts/1776114197.html","f8f1fd4cf4b9fec343ee212889813e50"],["/posts/1817748743.html","4028189cbb68329de906a04e24504a10"],["/posts/1925125395.html","6299cb0b6117ee49fc536577c6d8a27e"],["/posts/1966191251.html","d3bb9ba064b0a8adcf55dff97bfa40e9"],["/posts/1987617322.html","d51a95b16e288f9310aa2b09f91f429a"],["/posts/1999788039.html","177019a73b5cf8bb54577a87ecb42c71"],["/posts/2075104059.html","b8746d6865a5760f75121622e9d54132"],["/posts/2087796737.html","d47e741fac6773a7eec23d65c999211e"],["/posts/2106547339.html","14d131578fa486813fee682495258a07"],["/posts/2207806286.html","b16fc603f0a3169b437159ff483e46fb"],["/posts/2225903441.html","4c912c72f3f44028db80078565234a9b"],["/posts/2265610284.html","7d2b03471ab96ff4c31f8de3aaee710e"],["/posts/2281352001.html","a3984cb1fedb293562e57a7c3a0ce138"],["/posts/2364755265.html","a034e5904025706a8cead050797eec1d"],["/posts/2414116852.html","9a29a5b9bff037dd7d5c6a00dafda4cd"],["/posts/2421785022.html","8a3dd547c737c3b0be11bf47ff3572eb"],["/posts/2482902029.html","9f72010cdd5b656225b89544adfc58ef"],["/posts/2495386210.html","ad5a35e9925bc3162283ddf3c4d1be93"],["/posts/2516528882.html","25fda8900d46e8e0803cccd2e6b2e578"],["/posts/2526659543.html","094b0a679f36c0d68cc14181d13f9472"],["/posts/2529807823.html","61b655b0094af49abea9a0f6c5a4f75a"],["/posts/2596601004.html","d3009fc27429a1fedab3b462258a5942"],["/posts/2742438348.html","88c32316b3cf26c4fd3462c597c08e7a"],["/posts/2888309600.html","0d1028dfed9bad06421ce0e9d066073c"],["/posts/2891591958.html","f8d9c808d0a5da41eec6f67a2533b510"],["/posts/2909934084.html","031edc94c17f007346c73636a99d54f4"],["/posts/2920256992.html","82b806dc8b36f566ecdeb833d785040a"],["/posts/3005926051.html","1534416a7bfb3b7400670df5e5fc899e"],["/posts/309775400.html","88325c7536c3b270c448e11363b0b317"],["/posts/3156194925.html","27ba9cd675d343882c8bd41b09f4ab40"],["/posts/3169224211.html","ea81c73f70bd5bca620f8a80762f5cb8"],["/posts/3213899550.html","1e56da47d9e9bf906bb569b531a09481"],["/posts/3259212833.html","efaf8f908483061da4a0103936a249d6"],["/posts/3266130344.html","dd63183d0a8fec46215d27144c598126"],["/posts/3292663995.html","e9f1abf0748460264a2d2226a3278f83"],["/posts/3297135020.html","d43fd94adb0f1109e137350147b05355"],["/posts/3306641566.html","b01612d001aadce05e37121e4ec36717"],["/posts/3312011324.html","9db919369362ebed91d0f1051680a69c"],["/posts/336911618.html","d7eef2d40c00cd085d40de4d0f9a3ce5"],["/posts/3402121571.html","7e06e82e051d729c9d6dc0191b1ee797"],["/posts/3405577485.html","65662c038fd82c21c78b908ea5ef18ee"],["/posts/3498516849.html","8a770a34262298feec8a4d1562850060"],["/posts/3513711414.html","19bcff58d3af987227765d3e58a8a9ba"],["/posts/3546711884.html","85f0972c19391357e5292a74a3d2b536"],["/posts/3731385230.html","44127429cb04fb621b12f474da6f37f5"],["/posts/3772089482.html","dc11c04ddf701652d17afaff38d0799e"],["/posts/386609427.html","de1447ce86e212d4214de7aa201057ef"],["/posts/4044235327.html","3fd21d543a4c87ec91b16b1297d0beaf"],["/posts/4115971639.html","6842f622344ae4628ed1bb4248eec274"],["/posts/4130790367.html","041031f8930c41725c4f5df043a02c2c"],["/posts/4131986683.html","6c5abd684de51281472c2fe8374fd749"],["/posts/4177218757.html","a1ded4cf421e019d3db178ceb18edaf6"],["/posts/4192183953.html","d8a2cbad4308c2e38b666ef0cfe6a684"],["/posts/4261103898.html","2264e146d7f1c6c6d8f7a7c6a34eced7"],["/posts/469711973.html","3a5c473e2ade295752e8972776e27418"],["/posts/482495853.html","a8ca47078bc6287058b09ba8cdb9e61d"],["/posts/488247922.html","9daeca07bf8bded76dca5317728aa946"],["/posts/517302816.html","70bec59b94dbe9819741b784356fb4bc"],["/posts/570165348.html","d3242aa577e481feb988e60c148f3e94"],["/posts/595890772.html","0f8f768392870d1cdfdf29dd85dd9b70"],["/posts/67485572.html","6f2aa9d59966b1ed30613c433192432c"],["/posts/694347442.html","aa86e7f3bab3ce4b0e5051265fcef3de"],["/posts/707384687.html","b64f99dc724b368052553c9a079598fb"],["/posts/71180092.html","1ce0dad593c3fd8a440692fefd6b6855"],["/posts/716459272.html","0af270b5dc778642294b7ff893ab9478"],["/posts/765481613.html","8bbf56d1a4bf2b95b66ebf2398ed2f59"],["/posts/778231993.html","908d6c0c305faf8429ad08f584ef7360"],["/posts/795397410.html","fcacbdc8a1b5b52a6b77ce0e54ae5a54"],["/posts/820223701.html","a4716cc5933a444c99d163d6bec3f6c5"],["/posts/830372185.html","36d6c58088490a3f61e486cc01350acf"],["/posts/88294277.html","839a13d8282bdea2962980c5ae3bab14"],["/posts/939963535.html","5d823b2fae27bf3cb86904b96fdd41fc"],["/posts/983786067.html","cdcd15ce4cb6450d2773b4e0fa3ceb25"],["/sw-register.js","a0bc0eac86277d28d355ba9c07719819"],["/tags/C/index.html","8fc236a2a23ce5a29fbb687276773a92"],["/tags/C/page/2/index.html","49040b792854b76170f3c972c453c1f1"],["/tags/C/page/3/index.html","abbad4542a7db6cfbeffdd5c9bf40eff"],["/tags/ETL/index.html","d7f5631f5bde5bb62b72e330ec2a9f5a"],["/tags/ElasticSearch/index.html","3427dca31812c3ffb7c6d3cfe54b4392"],["/tags/GUI/index.html","adfc7ceb43d797cdc7593904755cc163"],["/tags/HBase/index.html","a218d94002f31ce7c78262c403ad0a15"],["/tags/Hadoop/index.html","84be1e1946026681b71c1f6eeb7584b7"],["/tags/Hadoop/page/2/index.html","63b862d2854dfe24ad92b59eaafdc40c"],["/tags/Java/index.html","354721abe41a3a496b00942ded321464"],["/tags/Java后端/index.html","f056cec35d9918d57eff3c3df4eec7af"],["/tags/Java后端/page/2/index.html","daf6b3a231daeb19ef990beb191ae1ef"],["/tags/Java基础/index.html","923e9abfd7af7fdb7af08d8e8b62f34b"],["/tags/Java基础/page/2/index.html","9bb0952b153e573bc0561292820b01a2"],["/tags/Kettle/index.html","11ef211a5b8b34144643f006320f46c5"],["/tags/Kibana/index.html","16432cac4502ea5713eaafaa983b16ea"],["/tags/Linux/index.html","e342ef58b919a62a26976819f33d130e"],["/tags/Linux/page/2/index.html","36ab53ec8545dcc2ceccca033e4b6c8b"],["/tags/Linux/page/3/index.html","11a10757a24bd13fd85bc0521bbde84c"],["/tags/Mac/index.html","e536f9cb17f31748c5f0064915fd06d4"],["/tags/Mac/page/2/index.html","783e922dab7cd41c45927509600c4d62"],["/tags/Maven/index.html","92a83f45acd64289efea8049785c0b24"],["/tags/MySQL/index.html","6a36598b86ba492d8e6d4d7769fa0fb1"],["/tags/Python/index.html","7347cd61f75b579fd83ab19d39ebea52"],["/tags/Redis/index.html","0711da0ceb71b13f09836eda151d01d0"],["/tags/R语言/index.html","1583f52328199a423857489d784093b5"],["/tags/Spark/index.html","8901e8a1403275d9a4b7ba31c651983e"],["/tags/Ubuntu/index.html","34e8a9abe285eebf5fb68449623db181"],["/tags/Vue/index.html","94aad65d8b8aff261c25af07854b7e3d"],["/tags/Windows/index.html","2306eb42ce2af610ea00817d3233cc56"],["/tags/ZooKeeper/index.html","99c5c7282609d7772de665fc64181df2"],["/tags/bfs/index.html","b52dc92e663b3ea2cfeaeaa1268b11bc"],["/tags/dfs/index.html","ca84a61483645c7e4b7041e5e9fc0014"],["/tags/folium/index.html","309d46ff11bb95ccab1334ceb256484d"],["/tags/git/index.html","6a47c586b231d22a2467030cdccc9d7c"],["/tags/index.html","d445ccc20ed833d1e324ba36ab619065"],["/tags/latex/index.html","39841418cde385d58613ad999656ac66"],["/tags/中间件/index.html","b7b0d0ef461bca8f6f970536bb006903"],["/tags/二分查找/index.html","28a18fbe39f8cf2cd50de22ee43eea97"],["/tags/优化类/index.html","4fad45983b0e556bc050c979c4f34a57"],["/tags/前端/index.html","4c38d6f8a4bcdb8ee62b992faebce31c"],["/tags/前缀和与差分/index.html","d322e86772ffaf392accc55712149b61"],["/tags/动态规划/index.html","73674816d173bce0080a1eb142efce2e"],["/tags/动态规划/page/2/index.html","d1883dad274d16aa75c4c15d2cf6ed7b"],["/tags/博客搭建/index.html","95f9a05e0cfb13bf89812b8ae2e4bacb"],["/tags/图论/index.html","b1711587617e1b1998ac7caea04326ef"],["/tags/大数据/index.html","fb7444e3117446f4976ddfe34976eb08"],["/tags/大数据/page/2/index.html","5429caa6c6f2d07fb92a26afffeb660c"],["/tags/操作系统/index.html","f95db65b8e5f4b361019df2765b09ac4"],["/tags/数学建模/index.html","096d70e834284293b2876cf0ffbdd41e"],["/tags/数据库/index.html","f63594fd50184709ec70458eec05b7c3"],["/tags/数据结构和算法/index.html","fb363e7e6583e12ddac31421dbf1e15e"],["/tags/数据结构和算法/page/2/index.html","c348299a81ef94f440fb9ac03b7ec2a5"],["/tags/数据结构和算法/page/3/index.html","d701ea9835b18f6722f4446277bf2dfc"],["/tags/数组和字符串/index.html","eced45cebb6e8337568fbc9a99a8d95f"],["/tags/枚举类/index.html","bc82b3af37a75ac0297947fd0f6bd9d5"],["/tags/栈和队列/index.html","d5eed665622bdabce7dee3032a26ef5e"],["/tags/树论/index.html","176ac7a1fac3a9c8929dc1877d09a194"],["/tags/测试/index.html","d15bcf1f0659991ff89f9dbbe0ebd2d2"],["/tags/环境/index.html","5e4142c2b569a2d5c48900c41b860d65"],["/tags/环境变量/index.html","6a2cd9d235577cdc066e7378a0bdf731"],["/tags/绘图/index.html","5bc79495a7cc47b0c20cae66cf305588"],["/tags/编程工具/index.html","ec5b7c090a28cac32e377dbc2bd3195f"],["/tags/编程环境/index.html","94ad6831221d42967a49305aa0f3f0d7"],["/tags/网络编程/index.html","3186f90d954f8ae42364a229bd35f1d5"],["/tags/英语语法/index.html","bf922a21daf5f0a546ced4c6677e69d7"],["/tags/论文/index.html","e442d61528b01a6c95e5e154ccbca530"],["/tags/资源下载/index.html","9131b925374b2aed77007dda1dc55a50"],["/tags/链表/index.html","6306f6d2e6dc7865d363f2f92d6c691e"],["/tags/集合/index.html","abe1e9395455a9708a9bcfc07ea245c8"],["/tags/集群/index.html","d8eace0784c9184d071323e35301e4ed"]];
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
