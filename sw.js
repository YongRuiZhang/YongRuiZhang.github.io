/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ea3e4974b2775c250a2d21f7af5d5860"],["/about/index.html","f352247ce6f53b664ff03dad5f4f3117"],["/archives/2023/01/index.html","843466266b48d77805cb7b35ffa48407"],["/archives/2023/02/index.html","64a46a4949f34829e69587a0eb1c936a"],["/archives/2023/02/page/2/index.html","c3f7ad6e366627de76aa0ab128cd7d64"],["/archives/2023/03/index.html","7434ad456454d7b4e61c6c43f3f0a918"],["/archives/2023/05/index.html","95650a3ab50d662d44d427a9f07b43e2"],["/archives/2023/06/index.html","5afb1a6581f7409369891bb3cb83edd4"],["/archives/2023/09/index.html","620e772625a6bbff2c69b39f61d74b9b"],["/archives/2023/11/index.html","5d30b7142b642aa84c79e59031ae7ba4"],["/archives/2023/12/index.html","300e3df59efbfc08de4318685da7b54f"],["/archives/2023/index.html","def81aa2b197325cb28c9fa5198296f0"],["/archives/2023/page/2/index.html","eb2ea7d4355cc86f3ece22a9692d3188"],["/archives/2023/page/3/index.html","3608f415dd4582301ae4b24057a8dacc"],["/archives/2023/page/4/index.html","d39f7eb13c8cdd7cb58483ca4fa8b037"],["/archives/2024/02/index.html","bad6785f1bfc88747679cfb582252bda"],["/archives/2024/index.html","23ad3e015c2acc6aea68b69ed65485ad"],["/archives/index.html","b68f4dac2ab16e3bb76fe83761028337"],["/archives/page/2/index.html","eaa41ba23c5662b78e787962527fb69c"],["/archives/page/3/index.html","9ac2341031d05a5e71ff1a63a4f3f460"],["/archives/page/4/index.html","ae96a151b0c6cbafde2256a9edb65a3d"],["/baidu_verify_codeva-qQP2iZOMLX.html","84de93cbddf6faf92bfd393f259587b3"],["/categories/Java/index.html","6eea0f0df812ab54b99325a6cbca87cc"],["/categories/Java/后端/index.html","3d7a3d007313f92e4c823d610cb9abbf"],["/categories/Java/基础/index.html","cdddbbec4d832f7132ce2cbd6233ca14"],["/categories/Java/基础/集合/index.html","fcab87675ae4c8b7a316c8093480f189"],["/categories/Python/index.html","9005d39f3911031626039cc4410e9926"],["/categories/Python/编程环境/index.html","a6122dc631e9a92629af243d050a3e61"],["/categories/R语言/index.html","721b4dd7f66bf64ac355662e593731b9"],["/categories/R语言/编程环境/index.html","4e9cff69ca88f0be05ad9bcf5d596384"],["/categories/index.html","9ae045b99aa872c79ddcdbf94abb9567"],["/categories/中间件/index.html","08f635c6367bee64cd5cedd1d691b8da"],["/categories/前端/Vue/index.html","2370eeeec1b43fa777aa71a3762fbf55"],["/categories/前端/index.html","4f86ff7629734fb0e06bf8becc418bd5"],["/categories/大数据开发/ElasticSearch/index.html","5989480efcf914aa477ef45087134a64"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","38cd8cc090a435d18659725b8a87ed15"],["/categories/大数据开发/HBase/index.html","2dfa6cddce277163c091ff09aedcfa4a"],["/categories/大数据开发/HBase/学习笔记/index.html","576181ed06283adb4a8cdaf3cab77029"],["/categories/大数据开发/HBase/环境搭建/index.html","ac1c8572bf0945422af3f25b3e836132"],["/categories/大数据开发/Hadoop/index.html","ad7a6d2cd91a7db2e01ce4ae5842c50c"],["/categories/大数据开发/Hadoop/技术/index.html","48a19852b65a18ad51ce484f6fc1fe27"],["/categories/大数据开发/Hadoop/环境搭建/index.html","210ca84ef876397b36e1a26a33cb392f"],["/categories/大数据开发/Redis/index.html","af74334831f26c2f85fb6ff1653de472"],["/categories/大数据开发/Redis/技术/index.html","d726b9f07e2d884e28d0880e99b596a7"],["/categories/大数据开发/Redis/环境搭建/index.html","c98da7827b1fada89f0de45dd291556a"],["/categories/大数据开发/Spark/index.html","2d96ef56f014acc1e9286bd91674d757"],["/categories/大数据开发/Spark/环境搭建/index.html","6d6445c35c784a0f2cca11086b49a04f"],["/categories/大数据开发/Zookeeper/index.html","807ee53a4549b01e1663131b8dcf3cde"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1771ca4e90f066ccf71b88ef5ab7a33e"],["/categories/大数据开发/index.html","ae01064cb10f00be3ac50da2e666a494"],["/categories/学校课程/index.html","4644bcd00d447031e81475503439295b"],["/categories/学校课程/计算机操作系统/index.html","ddcd9007e962b16a095353eb98be6a92"],["/categories/操作系统/Linux/index.html","036ed75fefe6e815d23a5103bfdaae58"],["/categories/操作系统/Mac/index.html","5b38903aa5f17e7450ba5eb9ea535429"],["/categories/操作系统/Windows/index.html","d0580a70cabea8ad51354039335b04f8"],["/categories/操作系统/index.html","8804a84b16737eb26a4efe529e03ef55"],["/categories/数学建模/index.html","56518e0514e30b2882ce131ee9afe552"],["/categories/数学建模/latex/index.html","5376a84d94d8119a9ecb958b8a09d77c"],["/categories/数学建模/优化类/index.html","fa3bef0c5286b319aefcadbbd2c2d0a4"],["/categories/数学建模/优化类/现代优化算法/index.html","b446f42e3ab687d9dd734090e0ddad66"],["/categories/数学建模/优化类/规划类/index.html","0825fd52f9f3aaeb125ea961de82965b"],["/categories/数学建模/绘图/index.html","611d484a52ceaf89275e0fea964dddff"],["/categories/数据库/MySQL/index.html","4c4f421c97feff3605904235c39911b8"],["/categories/数据库/index.html","6f6f086dd6219ed2555ba4bf729d00b3"],["/categories/数据结构和算法/index.html","6cc57513b7e1b7eec48f847cefbc9094"],["/categories/数据结构和算法/page/2/index.html","e4de147bcb4070cdc187b5bf233a42cd"],["/categories/数据结构和算法/基本原理/bfs/index.html","4f6305def08cca07acc4bdcd07927a21"],["/categories/数据结构和算法/基本原理/dfs/index.html","d5d003a747e132e29ea3423ab207fc6a"],["/categories/数据结构和算法/基本原理/index.html","316c87377f77c595276f85afa37be360"],["/categories/数据结构和算法/基本原理/动态规划/index.html","97d0fa2e4560c25bdf948e86417559fc"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","707bc5b3d094ff0250bf4a53dda61048"],["/categories/数据结构和算法/基本原理/图论/index.html","1c5f7f1dea16a3a71d73b2f7344e7d48"],["/categories/数据结构和算法/基本原理/字符串/index.html","5fdd7ceda7a71c5f391b4bceadb6682e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","4fb59de6dc902a7bbaac4d5c9ce742db"],["/categories/数据结构和算法/基本原理/数论/index.html","738478a249584cb5da6f91f04409b2be"],["/categories/数据结构和算法/基本原理/树论/index.html","336d893863530d2b23880896109010b2"],["/categories/数据结构和算法/基本原理/链表/index.html","3d7e75fb1b2bb47bbf5cb063d1c29227"],["/categories/数据结构和算法/算法题/index.html","48fb4eeaf135a15fce953962c0e109f1"],["/categories/数据结构和算法/算法题/二分查找/index.html","b0dcdc8482ca1b4a0d66e0b92d9d5482"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0b1841d049e01602456c8b48bc5ccc69"],["/categories/数据结构和算法/算法题/动态规划/index.html","2272eddef0caddf4de871a2afc6e5eb9"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","5986352272956f6ebfed3e9b6e171eea"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f6261f66a2d8eec32506a1dafdfc76eb"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b43b6e83a0f0a45dd4aba43f940d84a7"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","35f815722acab405a118f9ef18527884"],["/categories/数据结构和算法/算法题/数论/index.html","2fd11a4a5957dff123208fe605a6af70"],["/categories/数据结构和算法/算法题/栈和队列/index.html","76ac0fe24caf6f8d85f4fd29be6903b5"],["/categories/数据结构和算法/算法题/树论/index.html","26d86ed2df3bd06e7c6e74a28d54496c"],["/categories/杂七杂八/index.html","8974ba99516da9f4359f176b1ba81881"],["/categories/杂七杂八/博客搭建/index.html","df689ebf9482c46dfcc1c2aadf0b1818"],["/categories/编程工具下载/index.html","8639765c73ef98d203b191fa7dda7096"],["/categories/编程环境/index.html","94f68c3be9ea70be58d3ed737cd4d172"],["/categories/编程环境/大数据/index.html","f54f4db1b585069670e3354d15d3b24f"],["/categories/英语学习/index.html","c334059df9ef19d463bbbc9a772817af"],["/categories/英语学习/英语语法/index.html","3af1db8186490931999e943c463e421e"],["/comments/index.html","4fa360b1b76a569b44ead907a50aa98d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","f5c384cbe13edba697f3f8f41448df84"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","44e238db24b3c22a0fb87977a4440bf3"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","838f2026fa8648d82a173039b2af32b0"],["/movies/index.html","c919af3735a9e5c9c8d837c442b84c2e"],["/music/index.html","038c1b9c5a873d82668029e685b234af"],["/page/2/index.html","7751505ed235cf5ee1b7a804d3b61a10"],["/page/3/index.html","1abd1feae1594467deec40b56fe373ad"],["/page/4/index.html","f62fa0c8afcceffa3c84aee80d31184a"],["/page/5/index.html","5ec9a6d9d81ec1bd92b5a2f65ae3b82b"],["/page/6/index.html","c52eb5cdf758e1132c1c27d337da3e00"],["/posts/1021360842.html","de757ccaf813c2eace7ae7fc8ad346a8"],["/posts/1120620192.html","f16c9fa796ffd10ed321da90b4d572d3"],["/posts/1141628095.html","b7043fb60c768b5fd51d5be7e986a732"],["/posts/1168613674.html","df9d81cfa03e5a1f6094ce59533ee00f"],["/posts/1219920510.html","302bc781fe02c0a1c9d8f1aa0b979c6c"],["/posts/1222166338.html","75278441a94a7f3d79ef659638a23c35"],["/posts/1259097482.html","09b54fcf07e6b9f80f93ae4fedc62eb6"],["/posts/1271036369.html","b5d1dea30ace42ee67a3769daafcc822"],["/posts/1312847445.html","95dc92053842870154221f15d32d0fee"],["/posts/135355774.html","418edeaf7381ccbe073775a5ce2c85e1"],["/posts/1375344716.html","cf56bd318efabb4433f55013b65fa481"],["/posts/1388991698.html","73ff8c3a4182be76a5e94f5a1cf108df"],["/posts/1410315814.html","95b65fb5b532d4d2fb2534002a7d99f7"],["/posts/1452790229.html","ef2e643e31456a8e8dbcb663a41bf4ad"],["/posts/1470079884.html","37ec48d6ebe5d1c39517c97db3b5e617"],["/posts/1470079885.html","41198a6d4178d6ad65a6747cb72ce1ce"],["/posts/1470079886.html","f7cd3eab7ca764430a925401921a55bd"],["/posts/1470079887.html","101221a4e24a117b3e2ef3e65317e79b"],["/posts/1498536549.html","13b520bdabc4ce0596a10434a7452a87"],["/posts/1539568593.html","e5999ed0ca7d29faebaf260592cf6787"],["/posts/1547067935.html","f21b4cc14eaa1f190f2fcc7607f3e9df"],["/posts/1557866301.html","0eae7d11da40a1ac01cd37312dafc2f3"],["/posts/1571776361.html","9da590c84e9ee6809fe47fd3fa17f9d6"],["/posts/1605124548.html","77b5c11794facd2e4a83efdbb9287285"],["/posts/1633036852.html","17e0ad41a76976c0869faffb6b6f7a50"],["/posts/1674202625.html","6c729caccac4a6f244b5d2ea37475eef"],["/posts/1765123828.html","632d1872cc82e99be789223df7f79246"],["/posts/1767336200.html","c677358da63104559f22384e781fe923"],["/posts/1776114197.html","b9e9f6d5524f4843036b28cde4bac713"],["/posts/1817748743.html","eb3d3d15eced0fbdd6b34b396b02cded"],["/posts/1925125395.html","f41f475dff2c99dae67fceede224c6aa"],["/posts/1966191251.html","25120d29daa431f99c4cbf5c67be79c1"],["/posts/1987617322.html","7c41f2787c785cc1212e55e6316c8207"],["/posts/1999788039.html","98fdd786e969c44973c25da9917e02d1"],["/posts/2075104059.html","ddaa5aa64c730a0ae46fad8c8179ecb3"],["/posts/2087796737.html","5e9485df21275d253731a79f7d3cfb70"],["/posts/2106547339.html","df97a93718c5f0bf4b928e909be811cf"],["/posts/2207806286.html","96869bffc736ec4a3511b650da2374bf"],["/posts/2225903441.html","cfa2e8a48864e91b2e490aafb2314ab2"],["/posts/2265610284.html","c2e683fab0df8ee9847a4ed835e9dd41"],["/posts/2281352001.html","4150fe8dea353b87c2701998ce3dd6c1"],["/posts/2364755265.html","5da771912a2880da39148ccbe0265af2"],["/posts/2414116852.html","cded905cb417c20c2b8346e28c63968d"],["/posts/2421785022.html","673ce43379e0827fc7ec1e257ffb1411"],["/posts/2482902029.html","c6312aad21a65c67d5431370b0d95f7c"],["/posts/2495386210.html","19c265802dc20526dd992e9fdcdec1ab"],["/posts/2516528882.html","290dc6af45e97243c064ac7626cef09b"],["/posts/2526659543.html","c36578192e1f81d608feecbbb530dc66"],["/posts/2529807823.html","8e2728c3d2a5e83472198a8958acbcc6"],["/posts/2596601004.html","30126fc184686db386902ffddba55028"],["/posts/2697614349.html","6e350fdfcf23db8cff5d5c1ac68e468c"],["/posts/2742438348.html","00ac12938be95c5470b3ab61a77fb0f7"],["/posts/2768249503.html","325d44a2cd89d9d8cd3da6f065430c78"],["/posts/2864584994.html","f3de6766378fe0a9ac33e6d95565aa4b"],["/posts/2888309600.html","6b2a359ac72f63db16d927ee6011de4b"],["/posts/2891591958.html","9ddbe0e5c6cae8403df4e41c7d2ae210"],["/posts/2909934084.html","813cb6b376189fc3fe5d0be3f91a92e3"],["/posts/2920256992.html","11e9db8b99bea36871b3c150532174bc"],["/posts/2959474469.html","6f898a6cbfedeaf823b04bb84f4135a0"],["/posts/3005926051.html","d43793c388942de041c2ce80c7836a59"],["/posts/309775400.html","49589cd8bb44c2059cc1bc44c401bd68"],["/posts/3156194925.html","c573b6421a7a5d8be6e698255f6a64e8"],["/posts/3169224211.html","690956378c7d9fdd5707cfb8943d24f0"],["/posts/3213899550.html","519a2386eea502754557ef0774187e5d"],["/posts/3259212833.html","42e57f3d58f707a0c9cf527b8d98233e"],["/posts/3266130344.html","2c784493db1f79862225880e9d40c983"],["/posts/3292663995.html","c9520577e4f6b4962e4b980e2be98718"],["/posts/3297135020.html","3c7d6e795fa9f23f8637987387b62b84"],["/posts/3306641566.html","707d6b5895063c20a53504662e13d020"],["/posts/3312011324.html","6b65b7db1f92859864329b0b9e5beac1"],["/posts/336911618.html","1e0287e039914b41152557b854708868"],["/posts/3402121571.html","9ee605cd732787566210ee7f56f314f0"],["/posts/3405577485.html","2c625dd292eacd123a5624f48ed7990c"],["/posts/3498516849.html","6bfe2090636fe1def96e73eb4e5b7853"],["/posts/3513711414.html","2738be60a384d65e00a6c4a66968b60f"],["/posts/3523095624.html","cf6b3c742c61d658c162bab3e647d08b"],["/posts/3546711884.html","fcbbb8d53267099436bb653a21f86f7e"],["/posts/3731385230.html","2511c22698903dfcedc879d2e3f2a0da"],["/posts/3772089482.html","7f0f59cbbdb4b47d12451ecc7f60206c"],["/posts/386609427.html","6d84604369ffcc4f9cd94621f470af5c"],["/posts/4044235327.html","9083202ceaa88fc1af21205df268a798"],["/posts/4115971639.html","392fc1b90cc9ccf35e6fc316bc1e8c09"],["/posts/4130790367.html","479366f2fba2e1c90f4ec2ad046e1e85"],["/posts/4131986683.html","cc9d9a051d3925366b4adc807d50ec99"],["/posts/4177218757.html","3402b66665d202bb7aa4209fc513a05c"],["/posts/4192183953.html","5edba3c28f82d65572a8cc9b7a759f16"],["/posts/4261103898.html","a3e46df1c5e0471f428163a15a7fabe0"],["/posts/469711973.html","332db1043f58bfe4499d12847dcb4a54"],["/posts/482495853.html","ee3653fd3139a83128d39b1386a35184"],["/posts/488247922.html","bd89e620e7e1f288d6714f6d8a7dfbe7"],["/posts/517302816.html","3b92276a357b9853a0ad8ba6371f99f7"],["/posts/570165348.html","c4220bd3930cc30bd145dde9075b17c8"],["/posts/595890772.html","1358649fe7d1288aca726db16ac51dc5"],["/posts/67485572.html","8d442408c5ab94054e820cf7383c713a"],["/posts/694347442.html","c3abc5358fb39f9bf048a32bd70939e2"],["/posts/707384687.html","924ff6c5f93e2f6f7266f06e2c6f82ff"],["/posts/71180092.html","c9799b7d1005e38dce01d411ff0cae5c"],["/posts/716459272.html","971a377763db0b8a5105b9fea420c89d"],["/posts/765481613.html","0a3f1883a541f0f7f49203d59bb65ef1"],["/posts/778231993.html","12f12980208b0fcc0e2e0b3d61fa21a0"],["/posts/795397410.html","c9aaf98d8b7cfe1014698240b92f5ade"],["/posts/820223701.html","d9db5ac17bfed9a6df0ee106d239fcc7"],["/posts/830372185.html","6814446a960f2dc192fa66cefe9231d4"],["/posts/88294277.html","2c9790408096abda5876e84ea5306eb4"],["/posts/939963535.html","430a7c750ca51e68a8bccf476674b5de"],["/posts/983786067.html","21101e288cc21c60a97e318ebdf766c0"],["/sw-register.js","f9e12276d38ed2bf562ad41005af3e27"],["/tags/C/index.html","33d2f11b2d664209a79277acf10633f2"],["/tags/C/page/2/index.html","321999550b8b1a25ac1fdf64a917a249"],["/tags/C/page/3/index.html","44869eb07dd1bb8b02f79e32e636c108"],["/tags/C/page/4/index.html","800396c4f8f77dcb6fb26ec82dc85c28"],["/tags/ETL/index.html","11a5b7921db5fd5bc46ab3c5c9654c8d"],["/tags/ElasticSearch/index.html","c51e967875288063a744b06c795dda1b"],["/tags/GUI/index.html","eabae59b7c56b2a38779e428e0c76c6c"],["/tags/HBase/index.html","3fdbfcf05e8d8e79a344fe077aa5c7f5"],["/tags/Hadoop/index.html","bf77db39f3a06c5e91af600534decd04"],["/tags/Hadoop/page/2/index.html","d9e28300a6869689320d35ceec59dfb0"],["/tags/Java/index.html","bc6ba40551781d3a70dad54ccd2cef1e"],["/tags/Java后端/index.html","4b9cd429123647cbed9b62e15807db28"],["/tags/Java后端/page/2/index.html","f9f02842ff12a488dc7939042816f5ce"],["/tags/Java基础/index.html","0073031aa1b6e4b7d11fe5af9ff96d02"],["/tags/Java基础/page/2/index.html","d80dd16f9a9e964cbdb29d3533fb3ce2"],["/tags/Kettle/index.html","47386bbbc6dcfd8e49a56e67ecd1d75a"],["/tags/Kibana/index.html","8587dbadfcc4c80a1b44e390f20b30be"],["/tags/Linux/index.html","27fd9019adbc0abc203e7bdd0e8aa1c9"],["/tags/Linux/page/2/index.html","686f244df530556c497981d956306cd8"],["/tags/Linux/page/3/index.html","82e72697f72a9dad0e20a916506e3fe3"],["/tags/Mac/index.html","b4669332b162ea4c6488233558c71421"],["/tags/Mac/page/2/index.html","637fd78614ad166db9adbbb6f1368cca"],["/tags/Maven/index.html","1cbce5cde4915fc44c61bf1493d09f21"],["/tags/MySQL/index.html","935e274edfbacbc98ca99de6cb08b5de"],["/tags/Python/index.html","b9bf2769c2ecf966291c7251f0ff3d94"],["/tags/Redis/index.html","5c2e81636dfefa39886bf6fefce7e8a9"],["/tags/R语言/index.html","b665c72b8f9cd1d31feb93c298fb9d4e"],["/tags/Spark/index.html","bd10507a5824b24d8248f85f78b7423f"],["/tags/Ubuntu/index.html","5407cace7382ca02545a1c41a40718b3"],["/tags/Vue/index.html","f5e143061d3eb2696d34820b5311ed32"],["/tags/Windows/index.html","227a956e3fce764d4636c7ddf9d9edc5"],["/tags/ZooKeeper/index.html","98e1afe9dd3c5175b17201169d60b4ed"],["/tags/bfs/index.html","1258ab8c5f30045d12969224e1686d08"],["/tags/dfs/index.html","9278ba13df830339b824f1570a2691e3"],["/tags/folium/index.html","5b502a456fb6c2ebb586a5b97f18fcb2"],["/tags/git/index.html","e31ad048290271c733d5a4cf3d9173dc"],["/tags/index.html","1579129d110d4c417f70379866c57b9c"],["/tags/latex/index.html","ccc1db1ed0ae929e8949d0c8df3315d7"],["/tags/中间件/index.html","f4b335e45dadff9b3c2ea5ea02395888"],["/tags/二分查找/index.html","5071ab5de7757e40aa015f47deb58cec"],["/tags/优化类/index.html","68a9d95d19c8dcea53a3790839e11175"],["/tags/前端/index.html","2c9f99986f62584665be6711b48df9bc"],["/tags/前缀和与差分/index.html","fd737ce635f98426a20cf901aa09dda7"],["/tags/动态规划/index.html","2446ef9ae9e44810bd516e7985b673d9"],["/tags/动态规划/page/2/index.html","e793ef58b2fe8de6838bcfe9e3b8e6d7"],["/tags/博客搭建/index.html","4a637779c20c81980e63f4c42c39adb9"],["/tags/图论/index.html","c1ceb7bd2065e129f320d7cbcdc1f859"],["/tags/大数据/index.html","d284cc0a571a64aeda068bcddc78beeb"],["/tags/大数据/page/2/index.html","06b7f0f848f9ee1bb6a593dac1a14a1e"],["/tags/操作系统/index.html","30934e2460ee28aea62ed9a3e0b88667"],["/tags/数学建模/index.html","c972202fa3eb7aec8c4d1f715904ad6a"],["/tags/数据库/index.html","77a0431d30cc0cf71197878a616c8805"],["/tags/数据结构和算法/index.html","3298ce48edd8ea279c6d266ffd318577"],["/tags/数据结构和算法/page/2/index.html","1d7390ff6ab61da0a22815d6a24edf3a"],["/tags/数据结构和算法/page/3/index.html","362155c20f4427e3504212d0ea18c1b2"],["/tags/数据结构和算法/page/4/index.html","01ade2055e6cefc10d78272b182538e5"],["/tags/数组和字符串/index.html","08e2fa0832ec690a6d66a06f00e4dbab"],["/tags/数论/index.html","cc8106a5a47d4d2f60386c2dc05b55cc"],["/tags/枚举类/index.html","e9b92621d3e833f4a83e9941f140ea05"],["/tags/栈和队列/index.html","522c24e25ebd5010b2d12870409e1f79"],["/tags/树论/index.html","ea23c12500f2a6c4598a4f587e6ee71e"],["/tags/测试/index.html","416f442f620ec78848ed4ae7bf1306cf"],["/tags/环境/index.html","c1e473171be8e23c51be666c2f7bbc56"],["/tags/环境变量/index.html","5e359ea081d41cc77b25a4cec27424db"],["/tags/绘图/index.html","a92bb4e054539d1b2e5092009ac5884b"],["/tags/编程工具/index.html","90b63adfeaece0093485627658842457"],["/tags/编程环境/index.html","3cf202a6cda82044e3549091e0166f4d"],["/tags/网络编程/index.html","b585111ff21e9e67ab0907e4249fd3b2"],["/tags/英语语法/index.html","9040ea3a41c31d76554a21a5d6214791"],["/tags/计算机操作系统/index.html","45199799907ff4edb76096bc114646f6"],["/tags/论文/index.html","216765d7d60335b696ac8c680aa073ff"],["/tags/资源下载/index.html","7496572c3125ec4c021fe49ad14ed928"],["/tags/链表/index.html","b28a8f282b2427f62d4c5ed7d7f7b043"],["/tags/集合/index.html","99c41e7c0c3344c123e9e31f3a8f0fd0"],["/tags/集群/index.html","3deaf2f6db3dcb4b5a27a7ec194f157f"]];
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
