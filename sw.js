/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4d61ce3519ce6576e46380e76766eb2a"],["/about/index.html","0c109d84f353e22af3af3f42bb86055d"],["/archives/2023/01/index.html","c3067929757ae4fa900c549fe087e837"],["/archives/2023/02/index.html","0f6860c70097dcf51046bf9c1ee07477"],["/archives/2023/02/page/2/index.html","2e5fd60739ea5d56c45a16fc7f96f285"],["/archives/2023/03/index.html","af16b847b24fd7546840d57ad116778a"],["/archives/2023/05/index.html","5454fb9de9c48c5b7519e78ed757082b"],["/archives/2023/index.html","ad3f70edb3ebe531b007bb9db7b6bc8f"],["/archives/2023/page/2/index.html","0706c854836704751f2eb21ba3db3fdc"],["/archives/2023/page/3/index.html","765382038e5ff0a4e777332a4cd934da"],["/archives/2023/page/4/index.html","83db7547770a99a721704a86e80fa83f"],["/archives/index.html","f0ca0df3b96acd60b2a11b62f4b50310"],["/archives/page/2/index.html","92a390c43fe5c2832697f9b937f37e08"],["/archives/page/3/index.html","8fc47082743e2078e63d104f04733e72"],["/archives/page/4/index.html","224a57fd21d13e56e28ccdc1b187d010"],["/categories/Java/index.html","ae2300ba308fa3b2e2314e1296528e62"],["/categories/Java/后端/index.html","0b25b4d41db094f3df60a4ec05a6d985"],["/categories/Java/基础/index.html","71bf82d88f93a84365901afdae1d02f1"],["/categories/Java/基础/集合/index.html","69442c81435f61d0dc76a37bf0bba245"],["/categories/Python/index.html","54c5e32e18119922a5d062f8b2f0d768"],["/categories/Python/编程环境/index.html","84768e50254a8c2c9025ac715942f4ad"],["/categories/R语言/index.html","6e5bc3e4c7118321ce7b0b020c3db1c8"],["/categories/R语言/编程环境/index.html","10c42743fa527d35c8a5d795fa0bda52"],["/categories/index.html","e4698185c7bbed22c0db6705c9c09f4a"],["/categories/中间件/index.html","d87d7bb39d9301e2fdc4a63ef425522b"],["/categories/大数据开发/ElasticSearch/index.html","933b2ed64c693563ec1ee49ea87e72a9"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","88dd44680ab94dd41453644aff96c2ad"],["/categories/大数据开发/HBase/index.html","ea4b8a0455a9aca3ee94d9705e12db4f"],["/categories/大数据开发/HBase/学习笔记/index.html","ab17fba0a701249c573ac731fcc55d06"],["/categories/大数据开发/HBase/环境搭建/index.html","6f1711b79852353c4a61b8c1e985209d"],["/categories/大数据开发/Hadoop/index.html","77f565ceafe41c69579068320cf11c25"],["/categories/大数据开发/Hadoop/技术/index.html","ce68e2f2b66d90e9aa27f6715a25e7b3"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f026150cdd637797aae21cb5d0f19cb5"],["/categories/大数据开发/Redis/index.html","cba3e45ab3a22f644ba6ea46270c5add"],["/categories/大数据开发/Redis/技术/index.html","d9ade6d828f83c2801c69be4d594954d"],["/categories/大数据开发/Redis/环境搭建/index.html","349ecb6e5c6ef703bf9f3610a28e78e1"],["/categories/大数据开发/Zookeeper/index.html","793958affdf7b4a3548f9728e3ec350c"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","753995bdedc695f196173cad530483d8"],["/categories/大数据开发/index.html","08bb59370bb74eb136b716f0d01267bb"],["/categories/操作系统/Linux/index.html","6834e07c958caac3fcdbd59e842edcb9"],["/categories/操作系统/Mac/index.html","2d25715152ddd1272dca9a780df2faa5"],["/categories/操作系统/Windows/index.html","3705a4bd7af637e190dacfa42d704dd8"],["/categories/操作系统/index.html","c544d81ffa68f222e570128450eb2496"],["/categories/数学建模/index.html","1b0e9147862d5758934a52e59a55ea01"],["/categories/数学建模/latex/index.html","786b9ad0c41b319ffeb440705feaf5a0"],["/categories/数学建模/优化类/index.html","4a7483abd01846c9828144c12f199037"],["/categories/数学建模/优化类/现代优化算法/index.html","559ea1042cc6d281312c64e4d52b4245"],["/categories/数学建模/优化类/规划类/index.html","ec3120c55d38cd48e89e87d38ed7ae71"],["/categories/数学建模/绘图/index.html","2a3a6b953cd1aaf66412c8b01b4b6909"],["/categories/数据库/MySQL/index.html","420057b70c7d62197b04a1d1eedaea7d"],["/categories/数据库/index.html","2d159d049afea335fa413f0fba5330ec"],["/categories/数据结构和算法/index.html","d3871acd62f9cec0585d95ddbde5361c"],["/categories/数据结构和算法/page/2/index.html","cc6bfa63512e48dc19ab6c94f6dec41d"],["/categories/数据结构和算法/基本原理/bfs/index.html","6278662d684d4feb4c9151f3d35a9b02"],["/categories/数据结构和算法/基本原理/dfs/index.html","67a18d6eff7f806a1b5d3d8c73a11f57"],["/categories/数据结构和算法/基本原理/index.html","beb481080a690ad66b73cdb024db505b"],["/categories/数据结构和算法/基本原理/动态规划/index.html","8e6094a4082f330fa709554a59901e13"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","40ca0421182bb65dc8ea4263a79837c4"],["/categories/数据结构和算法/基本原理/图论/index.html","8c17cd2d0d29f6e03b0c840c2560c481"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2351e6b5d22060728d14414726fd7dd2"],["/categories/数据结构和算法/基本原理/数论/index.html","accb2b90989903fa982536af7b9c2d17"],["/categories/数据结构和算法/基本原理/树论/index.html","48feb873e2c25f7efea5082450fe4c30"],["/categories/数据结构和算法/基本原理/链表/index.html","bc1371ab77474a75d3be6ccbafe90746"],["/categories/数据结构和算法/算法题/index.html","27a070e3f32c2777a01b7e8aeb37e5c4"],["/categories/数据结构和算法/算法题/二分查找/index.html","2ad76620721e0f814bf689155a3ad34f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","153ab354704672aaa0294ae524082fa4"],["/categories/数据结构和算法/算法题/动态规划/index.html","9fa6d9d7fd95e4e24ce9bc244779bdf8"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","32fb202cad3edff4d32473952165b28e"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","40a3abd5a9420a847d4f94b5ee994ac6"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e82238f5cfc69a415a92fba4cf7cc041"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","f2f409f6ab6cbe5880d22f639cddb25a"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c2d69236ac25170557b810d3fce05b1f"],["/categories/数据结构和算法/算法题/树论/index.html","347aba24d42eb15260de784e7018507f"],["/categories/杂七杂八/index.html","f6b4d05732c2904a3b87c05506abc323"],["/categories/杂七杂八/博客搭建/index.html","606acc8a2358d692783649ea57ea8d2f"],["/categories/编程工具下载/index.html","c6729424b0b11ca8d822197a19704d60"],["/categories/编程环境/index.html","abfe558d7a357839f1ccf4297d695c6b"],["/categories/英语学习/index.html","ac0f207bbe904739feb17c8d48e36049"],["/categories/英语学习/英语语法/index.html","4adc7ed4a833f558471a6eb67bd3e30f"],["/comments/index.html","0f002473589515bfbe08eee6db32e1d6"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","69e139ec401c96bd96cd99cb72659df7"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ba92108863fabdfaf4878010b0bd056d"],["/movies/index.html","c94ce8986ea6d658404d969ab68d2b4f"],["/music/index.html","3cb98a37c11d7d4685c7eaf6d1153ddf"],["/page/2/index.html","2db93333cf04292cc9c41c52f119b89f"],["/page/3/index.html","a438a84209a89b4abaf20a01f91da784"],["/page/4/index.html","b1f0d31be0706146621c2a80d130843f"],["/page/5/index.html","ac6ea755722a5e2f1421be66880a8352"],["/page/6/index.html","107238ab5911ae7a162a3ef51baf1ec1"],["/posts/1021360842.html","e8cd605060e9c5d08057ba23c58640a7"],["/posts/1120620192.html","c5a51b6e41057bb1133fe043b2310413"],["/posts/1141628095.html","fa6d2daf56151905d9e267e83ed3edfc"],["/posts/1168613674.html","82eb0162acaf3f7e24a2607f4d5558b4"],["/posts/1219920510.html","3d5b42d4f8fef86cfbea85cabe14075f"],["/posts/1222166338.html","04f713c5567a8c51f49f5d159105560a"],["/posts/1259097482.html","4008456d82f8a5d9e7b6506f9bfa1f3b"],["/posts/1271036369.html","2a51768f860b06d1ebdc0e2d2d84df51"],["/posts/1312847445.html","2b430e07e7b1c64cc31c8edd420a25ca"],["/posts/135355774.html","424e0b7992d3a5fdbf194788245e2500"],["/posts/1375344716.html","1601790b778339ecf93ab964b04d0f42"],["/posts/1388991698.html","23d96ad0ef8ddb3b3b9197b13be3afed"],["/posts/1410315814.html","f2916feb5b600e77a3d521089c58cac9"],["/posts/1452790229.html","55d58f0cfc04275640811c8df9e131e8"],["/posts/1470079884.html","ae729376bfe0f63f6d162bb5e90ed7d8"],["/posts/1470079885.html","c6950bb2110e04216809b259fe966ff1"],["/posts/1470079886.html","63de14579e9b55c82d2c3cc1d12d90a6"],["/posts/1470079887.html","e1308bd60a7c12ae1ca1c39b2109a783"],["/posts/1498536549.html","ae8818e3107e1ec7830190e74e9fa70c"],["/posts/1547067935.html","c3e3653527fc88e63ca7aa98d5b9d303"],["/posts/1557866301.html","2f36630aefb2fe6770b993d1a698a13c"],["/posts/1571776361.html","74664822b69b2638227f06553d833671"],["/posts/1605124548.html","e745a820c5a42ebc3272d687b73cda6d"],["/posts/1633036852.html","cfdc4606330d8a8715bcae25fe6e232b"],["/posts/1765123828.html","fb557b2891cd2dbc0ac03ae87ba25fad"],["/posts/1767336200.html","e2cc25659e69a18d1ff4bee28211afc7"],["/posts/1776114197.html","ab3fa5f47210c03f5674f55132940bf6"],["/posts/1817748743.html","a1e4f6c7385525579e355b427050e16a"],["/posts/1925125395.html","b56c58665973dec06665dbfab178f892"],["/posts/1966191251.html","51b60ffbfed7ba0004acba18b0b11d4f"],["/posts/1987617322.html","2ab66c4c2e867cf924c182265e6ef63d"],["/posts/1999788039.html","4e3d862c63f107f0949448a3223114c8"],["/posts/2075104059.html","a7a71b16bbbf43e8a9aadcfa78aea5b1"],["/posts/2087796737.html","0d0a868e6c2c414a8b21d71b16988afd"],["/posts/2106547339.html","bd8e048a4100755e360d5eaf49f659e4"],["/posts/2207806286.html","ac59d24accda5ff1e45f31e06458a930"],["/posts/2225903441.html","ba945b61ae784d5bb9e61d2014a467d8"],["/posts/2265610284.html","524e9b61ee76d476b43d50a9a520bfca"],["/posts/2281352001.html","98d0fdbf7054c1596f513979fa29b569"],["/posts/2364755265.html","5591b729eae9dfa80844b65b659624d0"],["/posts/2414116852.html","96fc3d38d738a157790e24c3c2c11743"],["/posts/2421785022.html","4f60224e4436325bf9b34652f34409d9"],["/posts/2482902029.html","9820745ade20b74ea21330acef0040ae"],["/posts/2495386210.html","0a7aadb5c21897fd0867f1f940bcc81f"],["/posts/2516528882.html","b80a34343d17edae3228b364190e5a12"],["/posts/2526659543.html","3390fec0615dd7705f5e3a28cfec593f"],["/posts/2529807823.html","0d02a21e7a30eed27050eccdba5d1eaa"],["/posts/2742438348.html","e20c19ccb323023df79319a019e96333"],["/posts/2888309600.html","f8e6077c291dc00767199adf8a31db91"],["/posts/2891591958.html","f9a17b1227028b92c0d3483a1fa081bd"],["/posts/2909934084.html","44eecebde66ef1bc74cd20d0bbf22c43"],["/posts/2920256992.html","133082323bced15749a566c647a0b1be"],["/posts/3005926051.html","c9b13a544d4aa244dca17951b8c417c4"],["/posts/309775400.html","6aae810de509fd47e7d56f7fac6e4205"],["/posts/3156194925.html","1302cb881744f3a6f4152034c281bd2f"],["/posts/3169224211.html","cfe453f9c9f465c8d7159bde92d4de57"],["/posts/3213899550.html","e8bd04665b54f33782713da7c5114e2c"],["/posts/3259212833.html","a958bb76c9577e5b0419e920b4a128d7"],["/posts/3266130344.html","31fa6580a34ceee9638f4255527e9d69"],["/posts/3297135020.html","35ce99416b697ee2f02aa8b7ee8f9e93"],["/posts/3306641566.html","545f43aee229142e0e3770a81748fb9a"],["/posts/3312011324.html","ef7725c5d10cb5826daa7e0794e46808"],["/posts/336911618.html","ba8ea5fb724e0ce4fb910c75581a6088"],["/posts/3402121571.html","9efbb0bd946c9f25477d70cdd4d88e15"],["/posts/3405577485.html","0c9d696429e9330c15b03921862b2431"],["/posts/3498516849.html","32e2592b2e2e0eabe9300ed3df5b1836"],["/posts/3513711414.html","9b8d1e17d2fa2fccda1ede65e8ffd696"],["/posts/3546711884.html","6c204aa53b1c6ea4c66e1a76ab8aa5f8"],["/posts/3731385230.html","acd6c0ceb7911525629afa0de4475206"],["/posts/3772089482.html","cb22eeef99b9d3f110b47494f70afe01"],["/posts/386609427.html","770675fe7ef27ee8945ac278da767231"],["/posts/4044235327.html","ce217d2652a956d1089d1df2b4337714"],["/posts/4115971639.html","4116e7077527e9154b8fbf42bcf7c8e5"],["/posts/4130790367.html","45fbd19a578e5b0c31527a4e89d416ac"],["/posts/4131986683.html","da1e74d9ddf60aa800e1c1f9a23e06d0"],["/posts/4177218757.html","c8541a655869bc47456bc0b00f2742ce"],["/posts/4192183953.html","f0cfd065c950394c79d424c59119a513"],["/posts/4261103898.html","f0568078828a231abea8c2aa6dd21ba9"],["/posts/469711973.html","0382c82f94fc93fc9880aec80dd292ae"],["/posts/482495853.html","6007c539e51d487b938e04df6cf46c1c"],["/posts/488247922.html","5bb952d80756b09e352f72e21974b524"],["/posts/570165348.html","b7c0927bcf9f9e50471fa955eda7b91c"],["/posts/595890772.html","b9a5fe806d6ea65174ee0094de7bb77d"],["/posts/694347442.html","822689e1ddd5a418ddb87e3e2db9a910"],["/posts/707384687.html","c538c7fda122ff139f485753f496dc4a"],["/posts/71180092.html","410ff64cf4702d7e88c77cebc20f4a05"],["/posts/716459272.html","5498c190808f2b1b5e3395db6ec210fd"],["/posts/778231993.html","40c3390178416615e22af8813d02431d"],["/posts/795397410.html","7b744408ca46bc349126d3f2f61e9e3c"],["/posts/820223701.html","1af0b7f9775c240f1f5deaa1f5877ee8"],["/posts/830372185.html","3ae80df76a012d5e42e86bad42476d05"],["/posts/88294277.html","4baebd81ab477d61ed75f618e013ee56"],["/posts/939963535.html","5bea2cf8d68690369d8c74a283681eea"],["/posts/983786067.html","9cc4f35f330d9a72a06f7c397acd59c6"],["/sw-register.js","bd104a548c42ad6f75044f3c4cd33164"],["/tags/C/index.html","05f910414a260cd9b81cdc6bc30d1809"],["/tags/C/page/2/index.html","49fa17693db770d9bcd478c53e0b7f7c"],["/tags/C/page/3/index.html","7a648a6d44ee9c2c68a0f2f8ac99ba25"],["/tags/ElasticSearch/index.html","8a205fb26217c8481548f1cc238f23ad"],["/tags/GUI/index.html","3a16bf4ff8482a56c530536e59582821"],["/tags/HBase/index.html","5af444be113a7d53d3cdfd50a852b99c"],["/tags/Hadoop/index.html","82d0678ff8c99c9eac820758c54229aa"],["/tags/Hadoop/page/2/index.html","177961892c5d90e8a70e0c40ef63805b"],["/tags/Java/index.html","b29862942d61f80c96caf7608b157cef"],["/tags/Java后端/index.html","7a0a3afd6c6a5c944ddbe49336ce6fd9"],["/tags/Java后端/page/2/index.html","e6365699a34dfe592f441f182e934d06"],["/tags/Java基础/index.html","4278676e941162c122899a9934aa4146"],["/tags/Java基础/page/2/index.html","2e2e44247a6a22edfafe68fd11255921"],["/tags/Kibana/index.html","40f5750ae78b64d0c61f45bcbfc46a91"],["/tags/Linux/index.html","150d49ea730fdc4ec3ff211105a9967e"],["/tags/Linux/page/2/index.html","88b0b38d39e7072151638c37e41e2b80"],["/tags/Linux/page/3/index.html","bc12d2c36ddd05e970e6b2b96cbf8657"],["/tags/Mac/index.html","51495f4bf8e97c0d628941719cc8bd09"],["/tags/Mac/page/2/index.html","54844d1cb832e2932397ab2012ce8f89"],["/tags/Maven/index.html","f23c31ea5fa64f8bed78faccf8866be5"],["/tags/MySQL/index.html","ba2b60eae4bb5067010920a8e777372d"],["/tags/Python/index.html","ee1897509e886327d91128e31030585a"],["/tags/Redis/index.html","51c167aa46a20b0817a5ecd9a03daba6"],["/tags/R语言/index.html","f3c0aec62affe45e5171d05431e0d9da"],["/tags/Ubuntu/index.html","1aebe8da0ac8e5985d89a7fd8d57288b"],["/tags/Windows/index.html","468882c34562ed0e836d4cc5cca4d86f"],["/tags/ZooKeeper/index.html","9836036500261d92a8ae08302a81381e"],["/tags/bfs/index.html","8fb690e3badd38fbcdacdceb35ea2df8"],["/tags/dfs/index.html","46a56442df918c975362615271e4f387"],["/tags/folium/index.html","1f8362a607a58903d4f74a914ff73b66"],["/tags/git/index.html","8b858007a9a2e25a47cf58128305c8df"],["/tags/index.html","9e9edeac86a28ae414bd81cb7d796eeb"],["/tags/latex/index.html","9cef1db8123d4f63b61f4ce49ad58445"],["/tags/中间件/index.html","80a2b6f82aaf3cbf02438b91b3b5fd43"],["/tags/二分查找/index.html","76d55f837b4f0610daf689ee4316df3c"],["/tags/优化类/index.html","1f7a780d5c33ae0d7b29a4b4a1caef88"],["/tags/前缀和与差分/index.html","cbf1516abc881c78ed50c8e40f6b4e62"],["/tags/动态规划/index.html","66d241c76d942f4288e0e07cb5141443"],["/tags/动态规划/page/2/index.html","6af5050c75ac253cf4d591bdd842b365"],["/tags/博客搭建/index.html","eed3a3bd6b70f86d9c3c050eb7a507cb"],["/tags/图论/index.html","66fef69b262f9d8dc5ba2eba4ad1e48a"],["/tags/大数据/index.html","3b3ae1ce127050c4d7c99b83f5d38c13"],["/tags/大数据/page/2/index.html","f5745f0fd6f7d7c5a4fa84dcc73c71a9"],["/tags/操作系统/index.html","7a10221086f460cda6da9510f6a42f6d"],["/tags/数学建模/index.html","ed59208235034a851b5e0c457fa9c2bc"],["/tags/数据库/index.html","9ad923fda8854f41c9a987bdf9fea895"],["/tags/数据结构和算法/index.html","1f980f18d8c05ece7e8f1195f0d58729"],["/tags/数据结构和算法/page/2/index.html","858bff04a7f055e8944619cbfafe6362"],["/tags/数据结构和算法/page/3/index.html","79674409b21e7ea841d3db3b58b5cf56"],["/tags/数组和字符串/index.html","42ce42b5d2c27946314ba272d37781c1"],["/tags/枚举类/index.html","94cff9bec6b20be1079be8e6bb4666ec"],["/tags/栈和队列/index.html","81e64cbbffec80d3c6c1de3bfe19dc95"],["/tags/树论/index.html","9e3f6b30daa6d3bba96751bab0e134f7"],["/tags/测试/index.html","34dc44346c90ac8874eb40989d560e13"],["/tags/环境/index.html","a7b8977c3a901fad0b30b34fbda16da9"],["/tags/环境变量/index.html","7c3f574c254c1458d4a97b1efefe4e89"],["/tags/绘图/index.html","6244e79e4ef3f84f019bc43185357615"],["/tags/编程工具/index.html","bbac0223e90f15481ecaaa67f1f2b4e2"],["/tags/编程环境/index.html","f1fbd4746d47916716064ce8aec4cff6"],["/tags/网络编程/index.html","4f56ee47f204ccefc6effb546cdcc55e"],["/tags/英语语法/index.html","0b58c7f27e8804e7c02f02ed3a952e40"],["/tags/论文/index.html","9bb7fb03969d43f92c8c70f70c99ce53"],["/tags/资源下载/index.html","ac8a93efde0ddf62a582447496558f44"],["/tags/链表/index.html","852fd6dda044ca411b5776929970590d"],["/tags/集合/index.html","05aaa224c4e096aa13a47730eabb2d8f"],["/tags/集群/index.html","216c0aa80925d67d69cbae706287d2dc"]];
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
