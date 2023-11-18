/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","b8ca7321aea340ed0b8d99240d42efc9"],["/about/index.html","09acac93a9a99538b9b8e533f4126556"],["/archives/2023/01/index.html","bfe67aacafdab374f0c84ac894d30750"],["/archives/2023/02/index.html","fa2c25d5b75b735d2b176d33538a906a"],["/archives/2023/02/page/2/index.html","57ced7524449cb23a713736a264a782a"],["/archives/2023/03/index.html","4116e0b131eb004881798f05f90986e4"],["/archives/2023/05/index.html","f250f0a8000c5b09fa9a98106ba68bf7"],["/archives/2023/06/index.html","2391c196b59d0a680ca6fcbcb09c3908"],["/archives/2023/09/index.html","9fa074d50055c9e10ef503a5d36c249b"],["/archives/2023/11/index.html","55e619b37671331a1f71eb484e4853d9"],["/archives/2023/index.html","691bf97bd53619260da8c7c4a17fed2d"],["/archives/2023/page/2/index.html","cf78cdbbaf39a66ffa1fa118b1c16b23"],["/archives/2023/page/3/index.html","3419d156c1f024964c2e68db8ca3c995"],["/archives/2023/page/4/index.html","669e73a3ea99bfeb906d7f6fddd20d88"],["/archives/index.html","13b71790e9ed5401a31ec953850548f5"],["/archives/page/2/index.html","45cd3470c5b1f77ae02de0dcc9ebacbe"],["/archives/page/3/index.html","92f233a1dd98c38c2516fdc8e464cd04"],["/archives/page/4/index.html","1e9521d48406e16ae9c5979363c4722a"],["/baidu_verify_codeva-qQP2iZOMLX.html","683f0135d697031df60f2a3ebfc7e305"],["/categories/Java/index.html","94875075209f10e0a49cb763c35e3897"],["/categories/Java/后端/index.html","a5c7a8088d9939f048f468f4a70ddcdc"],["/categories/Java/基础/index.html","3b56dd0aa33c904eb674c7bcadbcc73d"],["/categories/Java/基础/集合/index.html","64683a95bf631efd654b113dcfa9d5bf"],["/categories/Python/index.html","72b5d6c2e7b1a1350f1e67ca03c10342"],["/categories/Python/编程环境/index.html","807d189fb8a0452b8996421a9de68e8d"],["/categories/R语言/index.html","4d8b58017c4895531871e2259d3e98a3"],["/categories/R语言/编程环境/index.html","6c5d7d245c52041055fbd90d8cbb03ab"],["/categories/index.html","872d8ef167d7099e3fd346c6e1a9012a"],["/categories/中间件/index.html","e2e814b2446678a05eee4bb62c90e99c"],["/categories/前端/Vue/index.html","523e7fa0757998d6b7e3251fba4b4972"],["/categories/前端/index.html","18a2c1f5dafc918bd00d6a67d694b67a"],["/categories/大数据开发/ElasticSearch/index.html","360529d89dc8c3f6ef8a39ad6e18dd64"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","731f136613023c9fd1bdd844dfb0260c"],["/categories/大数据开发/HBase/index.html","43f8db631b8f02afe74acc02247063be"],["/categories/大数据开发/HBase/学习笔记/index.html","c697efc9ac2a22f804d990145beda2d2"],["/categories/大数据开发/HBase/环境搭建/index.html","2acdd382932fe2f716348a6eb44f5293"],["/categories/大数据开发/Hadoop/index.html","ea51fe0dc90c1c7ab56e8380613ee53d"],["/categories/大数据开发/Hadoop/技术/index.html","b07ec018d8cb7645e2087f4fe3edc05e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","ade16cf2c834776c7190d6ec3347fda7"],["/categories/大数据开发/Redis/index.html","09fad1695793653d3d7d89f42589d2c6"],["/categories/大数据开发/Redis/技术/index.html","02f11eb8b331f655df727ff49a0cf865"],["/categories/大数据开发/Redis/环境搭建/index.html","6ea0fd30a2b1c0ddcd8f2d25ce9a6345"],["/categories/大数据开发/Spark/index.html","47f80783fc36bda48c8afe7a84d11859"],["/categories/大数据开发/Spark/环境搭建/index.html","f36360b028855f67d05b89cb171737f9"],["/categories/大数据开发/Zookeeper/index.html","89d3aeba52bd86d8f35ed8bf9fddb87f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","33bb40b982315f996e7f0763778deccb"],["/categories/大数据开发/index.html","ce6b2b1ee7eb41c54443dfef59c1a330"],["/categories/操作系统/Linux/index.html","13a450e629b051be57966e41ddc6170b"],["/categories/操作系统/Mac/index.html","34b2f3ae25eca5595085e13e4a66b0ce"],["/categories/操作系统/Windows/index.html","be6187d779633a99d4cd532abe2ee6d9"],["/categories/操作系统/index.html","ad0cf233d70f080a7a4bd210c2a2f2c3"],["/categories/数学建模/index.html","cef51b43ff918f03b20e830ff0a52dd2"],["/categories/数学建模/latex/index.html","de8224ed179588ff1b34884e0b732218"],["/categories/数学建模/优化类/index.html","dc8f5110e18646896ed633d1c5358dcf"],["/categories/数学建模/优化类/现代优化算法/index.html","8c354453ccb4a4ed98c7d8f18b201098"],["/categories/数学建模/优化类/规划类/index.html","60c9e2106b7cdd636807f66d7da8b025"],["/categories/数学建模/绘图/index.html","06914d0930ec889ee1c48b772e249bed"],["/categories/数据库/MySQL/index.html","8ea7caed408fec6fbb93fcff2d9cb762"],["/categories/数据库/index.html","e595fa6fd9f4aa310cd1c2e502e631ae"],["/categories/数据结构和算法/index.html","4c274ac9b212beff7234734a604f6ac8"],["/categories/数据结构和算法/page/2/index.html","5b102b8b1fc36c40b22f825049784c88"],["/categories/数据结构和算法/基本原理/bfs/index.html","e9f53396d24fb74ada1d7323854244fc"],["/categories/数据结构和算法/基本原理/dfs/index.html","5cc91f4ab29983d9f932dfb169706347"],["/categories/数据结构和算法/基本原理/index.html","546ea58fe0e0fe574786274e0a16cfd8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","d08343e287b77e76957216e51d6c9eea"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a006a2c609a22ad65bc9a9f0e006243f"],["/categories/数据结构和算法/基本原理/图论/index.html","50c0422eea06fcd156aaab6a2d976ff1"],["/categories/数据结构和算法/基本原理/字符串/index.html","f6b74513208a53435fe081ce5e3f36ab"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","107f8cc00a513a4771add2281d274db2"],["/categories/数据结构和算法/基本原理/数论/index.html","301c2100f6b657b38128d1b9725702d2"],["/categories/数据结构和算法/基本原理/树论/index.html","6c637469ddac9b876bc7efe2c7c34bd1"],["/categories/数据结构和算法/基本原理/链表/index.html","2111fad94e70ffe41317d891c481f211"],["/categories/数据结构和算法/算法题/index.html","0002fae80a7718d2f1b4cabc7c8a59a8"],["/categories/数据结构和算法/算法题/二分查找/index.html","e71e76d7172d6a3c8cb9cf3f32cc2530"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","98d2a45d9af317289a40a1da871dd01c"],["/categories/数据结构和算法/算法题/动态规划/index.html","ab52e59076fb1960d7273ca3ac61af40"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d8e0c4a4fe7fd8532d6e1dab0b5aae0c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","0be5a9b82dc29aa76c030efb30bf1e70"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","dcdd99d741c8d5bd31fb05fa80ee8074"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","8e617dd069ddbbd39bf1adcf359f3d08"],["/categories/数据结构和算法/算法题/栈和队列/index.html","85894f6b4df47f486a5e6428e4137e33"],["/categories/数据结构和算法/算法题/树论/index.html","4a8e852bc95e6a439fbe1d7e80ec0d34"],["/categories/杂七杂八/index.html","a92473471a316951ec144d5525c39d75"],["/categories/杂七杂八/博客搭建/index.html","4ba095355a79ae3928ae25189bc46a91"],["/categories/编程工具下载/index.html","eafc1c239b26059c60f038285b7d776b"],["/categories/编程环境/index.html","b7dde8b5a423fb0af8d7c88820840e96"],["/categories/编程环境/大数据/index.html","f32f4e22d6bcb81c7d3482b96a3dd567"],["/categories/英语学习/index.html","7fb51782731c43226d81da4cfe3246de"],["/categories/英语学习/英语语法/index.html","f97d055430fc871ea25a8c856c43df87"],["/comments/index.html","6d2c7ecd466d1b8868589665a79406f7"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ade1be1f4c4b2bdf955dbc131036ddd3"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","6bc2b2ab8428ab2665eb3154f8be9988"],["/movies/index.html","5086a8593c08b2243e434dc07c6440c9"],["/music/index.html","80473ddad84a9d0099a1ebf9d074cce3"],["/page/2/index.html","3cb366a53711fcb9448ab0f0f89cf535"],["/page/3/index.html","9be3c835d01c57d7253bd619bba2afe0"],["/page/4/index.html","ab530434d6c337823e60d69e89f550b5"],["/page/5/index.html","42f720f7773742901648f37089890e48"],["/page/6/index.html","a7583d9f767b8ac0868b620cd4411a39"],["/posts/1021360842.html","2b6701e0d274ea27ea680e8672685835"],["/posts/1120620192.html","c2f69971c4c0f6842911dea3687b0cf4"],["/posts/1141628095.html","a337725050f8165d0c1db0c36a298438"],["/posts/1168613674.html","a893c1d3628c22e9c0121512b0127dcb"],["/posts/1219920510.html","09181ddeabbbc60be6b40ef714f19743"],["/posts/1222166338.html","56b38b4f4924ade16ae8da2589e9c597"],["/posts/1259097482.html","2f605c6f132f2f80b735ffcbe93fdd68"],["/posts/1271036369.html","2c28ed885ef67b1d1955c2046e1a9d9e"],["/posts/1312847445.html","d0d1e34a79d8e6bae420254966494162"],["/posts/135355774.html","1602716606179a4d54906f3fd1de40fb"],["/posts/1375344716.html","3ff5b151f2a68976a5f9034dd7a7ee07"],["/posts/1388991698.html","ffadba2b56ad0aa03e3663a88b6eeeb2"],["/posts/1410315814.html","a27210f90458ad95b89aa441c6385d41"],["/posts/1452790229.html","16693dd1b491740ccf78f1d204677eb3"],["/posts/1470079884.html","571ac67553fae0ced8544ff1365873e2"],["/posts/1470079885.html","5a1cbb3a3b60456a1b9c13bfd931a2ca"],["/posts/1470079886.html","11c3c7629f23e4701e23d260a6819cf4"],["/posts/1470079887.html","da056b6c1289ba1ad2e35c930093ce8f"],["/posts/1498536549.html","acf0e5f14a732f3acb939579718df02e"],["/posts/1547067935.html","6401006598bdc2ad33c1c1334871573f"],["/posts/1557866301.html","f0c12edb8b059bc07c8ce5f4589cf5cd"],["/posts/1571776361.html","fccaac7cf9a1a9f31048f7d21e16349a"],["/posts/1605124548.html","b678fceb25b82aef47a839b4e29dbc96"],["/posts/1633036852.html","4489f29e5321488979565e1929a29dd9"],["/posts/1674202625.html","78d3211262fd775b45ec501961c50afd"],["/posts/1765123828.html","47b087a67257014e1cfd7494730495fa"],["/posts/1767336200.html","aab6fc1650c69fa71bb88f8463744aa0"],["/posts/1776114197.html","280fabe0455a1756d0f34c3d9941faa1"],["/posts/1817748743.html","b25d8983eedcbe708e785f893a3e15d7"],["/posts/1925125395.html","5a86dbbc6bc144a7f300d1bcfc061531"],["/posts/1966191251.html","b7a3eb49dfe3974772f96da8e069f628"],["/posts/1987617322.html","9c6c3dd2f38d67938c82975951392987"],["/posts/1999788039.html","bfc40222eb1d47297848ee5f5f6f2217"],["/posts/2075104059.html","0099b38f9c564758d9930f98292283da"],["/posts/2087796737.html","f1a48dcd729cc3ff534bf82f5b99853e"],["/posts/2106547339.html","949f28e2ca144444426e77dce3691346"],["/posts/2207806286.html","cd8664786d97ad7090fdfb28a0f63e5e"],["/posts/2225903441.html","ef6f8651fe13b3851b93dec2305cbf23"],["/posts/2265610284.html","f255d80c80e64ca7004b2ede6f36816c"],["/posts/2281352001.html","c5402e02dc243dc9d374a42cd7802774"],["/posts/2364755265.html","687135efec64dcd9426985dbcab44b3c"],["/posts/2414116852.html","442bdf339015bf1bf1dc7ce6f7a53778"],["/posts/2421785022.html","e23b81c55f7bbedc33348484049e5c1d"],["/posts/2482902029.html","4e629b7032dbbb8e8bd737f3acc7e3bb"],["/posts/2495386210.html","fc21ebf813a87c60f1266f2addec3334"],["/posts/2516528882.html","6b542b6e2e81c15642a49fb7819c3931"],["/posts/2526659543.html","02f4cb1e772186d2327b04725890e47b"],["/posts/2529807823.html","3979db31f8ddf9cda52ca528e33e84e2"],["/posts/2596601004.html","9c646a74261428500c802d9c2f11972f"],["/posts/2742438348.html","611b6ac7fb0a7f47e9976fc42fba88d8"],["/posts/2888309600.html","1e5752bb8a24df57c1c7042ec6a7df98"],["/posts/2891591958.html","d94455354d3b28c8556540eb0f3bc9eb"],["/posts/2909934084.html","e0a30e4881ab45c253c2cbfc4c23550b"],["/posts/2920256992.html","78a91beff41059da544ba7c121c2fa6f"],["/posts/3005926051.html","c4aae37d2b0f442c280ee988c2358e76"],["/posts/309775400.html","aaa8a4ab0867d7eb510e29aaac8ec4c5"],["/posts/3156194925.html","c20f755cbae150373b2250e47bd2af6b"],["/posts/3169224211.html","a570a1183a41e3c2527ddd7e446a15a5"],["/posts/3213899550.html","26931927228a7204e49ee499a7351c1e"],["/posts/3259212833.html","96f559426bc549607f6ed388412255ca"],["/posts/3266130344.html","a538b288feb4dc93851824ff9db98694"],["/posts/3292663995.html","eb15f9c05f2b704f7f8b8872f1e07302"],["/posts/3297135020.html","7d1082678e59f89fa9fbb66eaa5de2b4"],["/posts/3306641566.html","5e33f5ba118a34961c1dcae8c2cac638"],["/posts/3312011324.html","4beb3618ba74926b399da3b0adb9fce6"],["/posts/336911618.html","29892e30988730c88f3ed3460d00cb6a"],["/posts/3402121571.html","4f2dfd72364a2268e58ff0161992f53a"],["/posts/3405577485.html","b3861ef5022006b689353c2eb046dd61"],["/posts/3498516849.html","4544c5e15bd9eef163275c04e57f2ddc"],["/posts/3513711414.html","2a975435efe841eed16ae1db8ec128f4"],["/posts/3546711884.html","b38f9c9b151dfe094bd9258ffd74d672"],["/posts/3731385230.html","f073cc8b9eb10537380c8b27dad220fa"],["/posts/3772089482.html","0d5c9c525f3fae855547427a84a3c68d"],["/posts/386609427.html","3658b4a37886d5283fca05a2cab444aa"],["/posts/4044235327.html","aa73d1e12f9368e8055270b77d4cfa54"],["/posts/4115971639.html","3cf55cf6b56f519a73d3348b6816445d"],["/posts/4130790367.html","c078024392cbbc48e54942ee0c712f5f"],["/posts/4131986683.html","b1a9ff0e3c6d47d6de475cf591a9d70e"],["/posts/4177218757.html","b234f96488f2288ca8f42940463d4aa0"],["/posts/4192183953.html","e47ec699bcdb8fe176aa79e4712eef65"],["/posts/4261103898.html","c490f4a759f4d070b824b6f4927ccb23"],["/posts/469711973.html","bc00adbe6048be2fbaecc3f36d5513d5"],["/posts/482495853.html","ccfe1e3e2d16df821694775351ad61cb"],["/posts/488247922.html","b4bf56f48976ce40e5c1bc91dd4e93ae"],["/posts/517302816.html","13447159cbc7df19d7fad56f626de668"],["/posts/570165348.html","e06b8627c3d8eb58f25c10ddca7ee8d1"],["/posts/595890772.html","264e9b3255ae1f41270be1eb38b8c789"],["/posts/67485572.html","49f7bc1b7060fb4b1b084ef499147b50"],["/posts/694347442.html","43f81e7073fa4a5266eb74a42798f94a"],["/posts/707384687.html","07da2e16a2c590098a8fae660d267b25"],["/posts/71180092.html","86aa616c21942a1efa95a5d6f96f4148"],["/posts/716459272.html","1fc40615b5df1542b9b18e4cf7b94089"],["/posts/765481613.html","dec885c1f34197bc5549a54d0b18c370"],["/posts/778231993.html","8d077ef2b64a39ec9f261194218da649"],["/posts/795397410.html","be9b6b175390d0a28c3c3243c95c842c"],["/posts/820223701.html","1d6062025c474a789605605a393beb59"],["/posts/830372185.html","20f6a06c0842b2d70960e3b589d39f59"],["/posts/88294277.html","0d77d07dbd7e6252f236f99c1fe19d87"],["/posts/939963535.html","770e5d190456c96dde331a288a4219bb"],["/posts/983786067.html","2f2dba014bd0743deadc756d3a94d1f2"],["/sw-register.js","7a7aafa6e91348ae287660fb7c355b1f"],["/tags/C/index.html","c88abd4bf63b2a325330ce30b61a58d8"],["/tags/C/page/2/index.html","85c86243f1a8f8d7465ec2a1cb670e4d"],["/tags/C/page/3/index.html","902df5087bc70e7344a196a192d76f06"],["/tags/ETL/index.html","cd89d74160cf73e7c0376e85fc2306cd"],["/tags/ElasticSearch/index.html","6aba749ee1645e03ef5854e74f16cee4"],["/tags/GUI/index.html","16377525e91aa1dfd37d10c60fa610fe"],["/tags/HBase/index.html","9be65ce22ce459a6dfd8f9d24e410da7"],["/tags/Hadoop/index.html","a2c17bc9d17f89fb837ad322af0d7053"],["/tags/Hadoop/page/2/index.html","decac08660105b0de9969ca63f8d33cf"],["/tags/Java/index.html","835e8e6c4b4e238b20e507bf00b8cdf6"],["/tags/Java后端/index.html","d0f2f9a8a3b4f49a239f1c3aa0863430"],["/tags/Java后端/page/2/index.html","4bf453b7796f3254129153d665255e61"],["/tags/Java基础/index.html","72601ea8ab7ef1eddc999b994939a071"],["/tags/Java基础/page/2/index.html","680f3260918794232c5a6231a8293802"],["/tags/Kettle/index.html","743f2b150b2a6a92e810d05f0bc9cc92"],["/tags/Kibana/index.html","15686dfd9a9bcf3266f4865c67e9dd3f"],["/tags/Linux/index.html","c5f724a17c3d9cecc3d12e27dcb286c7"],["/tags/Linux/page/2/index.html","39b8633ac223e0a27f69d1d17e43bca7"],["/tags/Linux/page/3/index.html","e8ab44c0b7d0eab8ffa199ef3a227dac"],["/tags/Mac/index.html","4235e4482b7cb19c5d3dba517394abf6"],["/tags/Mac/page/2/index.html","2c3df27a73e7ba6bbe38a1538ee7bd88"],["/tags/Maven/index.html","6dc672ffdb9ca47ad0329dae0d0d956b"],["/tags/MySQL/index.html","64c5bbf3ce93117ee78ab190208f1a2d"],["/tags/Python/index.html","af9c18dba470e1ceb154eb22c440c95c"],["/tags/Redis/index.html","f18008d68a92d09207d4d3ba488644e0"],["/tags/R语言/index.html","3585128c283b5543ee87a364b822a59d"],["/tags/Spark/index.html","b96852868d02d7336e5797aa9cd04c8a"],["/tags/Ubuntu/index.html","20869ad83b069c6ce7d6620bd0eae113"],["/tags/Vue/index.html","8a63d8bc00662d069481ce54bd1b2d99"],["/tags/Windows/index.html","bc25ddd3c5029c49b727eb17346104ca"],["/tags/ZooKeeper/index.html","32b621142cd2e4a8d8bc640750826523"],["/tags/bfs/index.html","96d03813ab9cc2b3078c5932539dea4d"],["/tags/dfs/index.html","3b554a3c98e7356f075731bf848ef707"],["/tags/folium/index.html","0a7b9d1ac1bf58925cd7b6aa1b3ce0c5"],["/tags/git/index.html","e24915088d7576288153b8b42ab3f50f"],["/tags/index.html","252a5671a86663e5fc0d243526eebf32"],["/tags/latex/index.html","1f09757a52bb98cdac7fc6e8e2701ca0"],["/tags/中间件/index.html","670f0944a94569494b6cf2c20038ee25"],["/tags/二分查找/index.html","5c8c7dc3fbfd2a8a88e4de9b48f8c715"],["/tags/优化类/index.html","e6dbeb917288d56fb1df9cd601cddc35"],["/tags/前端/index.html","9c7811a2641a337bf79f314ea9ea61ab"],["/tags/前缀和与差分/index.html","3b6646dd9b7b45fc474ecbb3d647313c"],["/tags/动态规划/index.html","8aeb637b925fa8af23c34d9e2fdcc72d"],["/tags/动态规划/page/2/index.html","41da6404c478cb0dcb31485ddb78a45e"],["/tags/博客搭建/index.html","fa7b6b43bd52c1f1051cf73b3c56a21f"],["/tags/图论/index.html","ba8aea1afd36d32165a61ca05855ed0c"],["/tags/大数据/index.html","c5c036fda4c649a8e45f4693bdc91c3e"],["/tags/大数据/page/2/index.html","20848088326585244c7416370328689c"],["/tags/操作系统/index.html","51c2666be2b52447452a9c3533cb5afc"],["/tags/数学建模/index.html","0129e4cb85350bfb72cd419122c3fda0"],["/tags/数据库/index.html","b454ad51dd51e2be31c3dbb771e8a681"],["/tags/数据结构和算法/index.html","a153e6981d6c86754ade5224bb41855f"],["/tags/数据结构和算法/page/2/index.html","1c5922a1739351f8c480d9e9734386dc"],["/tags/数据结构和算法/page/3/index.html","fcdff2a074db46fe8203b1bb95214d2e"],["/tags/数组和字符串/index.html","b01c3be8d5606cf61e6060386d6759ba"],["/tags/枚举类/index.html","10249aa63442a36f800138e4672e1a3c"],["/tags/栈和队列/index.html","079d18e353f880f4f812b16e3680cacd"],["/tags/树论/index.html","0840dced6b4af832d9ddc3bcff80fa2b"],["/tags/测试/index.html","8213a511daf9739bff3ec3632f84536b"],["/tags/环境/index.html","1937f8a495a749838e67f0ca08cf681f"],["/tags/环境变量/index.html","5883ce64661b32c1275fd0c03be7d647"],["/tags/绘图/index.html","219e618acf058c41b02f469be0f77354"],["/tags/编程工具/index.html","62da9c2d0bdf8cdefb247b7c1b771b12"],["/tags/编程环境/index.html","a692cc2334cdf142a15223badd18bee5"],["/tags/网络编程/index.html","50d5a9ac2b0cfb5923b9806494d33efb"],["/tags/英语语法/index.html","e83be23eb79ab384b3ad5ce3a17b988e"],["/tags/论文/index.html","3000890b938c61e8a61bcff5ff001a0c"],["/tags/资源下载/index.html","25fa1809a5d2bf62a71875942d11595a"],["/tags/链表/index.html","21cc3a2be798ad6e5f3c51f3eeabae45"],["/tags/集合/index.html","528191f1bcf7a284d0ad54aa8b9b236e"],["/tags/集群/index.html","67a9bf57c78a07b03c9ebb2e9546c53c"]];
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
