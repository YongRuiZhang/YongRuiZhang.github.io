/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","1bc96296e00cd786184b26f57a3a31e7"],["/about/index.html","599438f318f91646df1d53d78d6e68e8"],["/archives/2023/01/index.html","64f240113499af31e5adf49beb6a2128"],["/archives/2023/02/index.html","76bf1373aab58e950217bc8e0c9607c8"],["/archives/2023/02/page/2/index.html","29d1f631fd7f0a3643f3cc78017c1ba0"],["/archives/2023/03/index.html","f7e086252f6595301a6c7b7572c86fcf"],["/archives/2023/05/index.html","cc492955f467ab2a70c334e6441fc8e1"],["/archives/2023/06/index.html","0677541955ffd0fa57ff9cfddefb7e5d"],["/archives/2023/09/index.html","1f10e2162787cc4d2a384a1c9e58ed5b"],["/archives/2023/11/index.html","512be2042495b58c4ffc04e6a29b1d65"],["/archives/2023/12/index.html","850089330c49bdf4dcd9b6d1149d0e97"],["/archives/2023/index.html","f476b8d5401bb3cb7c8df88f3f678bd0"],["/archives/2023/page/2/index.html","5ab592f7b268c51d98d414c3e86f8a38"],["/archives/2023/page/3/index.html","cc1cb93234b526d7d0d1d10aeebf7310"],["/archives/2023/page/4/index.html","120fe027ada9ff1b144a582e20b19e50"],["/archives/2024/02/index.html","f72a257fbfd50402a378651b7f6ac8d7"],["/archives/2024/index.html","bb33d660450ec7f5df123085dfcd952c"],["/archives/index.html","f6a0e53ea3653c0616fcb2a244162a54"],["/archives/page/2/index.html","8b9cc26a56e9989158e0aa0814bb3735"],["/archives/page/3/index.html","3d1b1f1caa1f9b6b1b56635acab5a631"],["/archives/page/4/index.html","d09ce783ae16f61b7381fab1bf49a1f3"],["/baidu_verify_codeva-qQP2iZOMLX.html","b94539d666693bc6f9e00156e04c088d"],["/categories/Java/index.html","02b560496d7bf6fc314d7f113bcd8ec6"],["/categories/Java/后端/index.html","8129b7eac48cb517595664b978fc9b7a"],["/categories/Java/基础/index.html","3db595f848e524d0b2f55c4df2072683"],["/categories/Java/基础/集合/index.html","9d3cef0176aa02042d4538125db33c59"],["/categories/Python/index.html","fe07d3066518c2c967db8b2a2f6386dd"],["/categories/Python/编程环境/index.html","be763a3103d55bf05318cc9f55bf215c"],["/categories/R语言/index.html","b865ad0c750c0ce52a3f86088a5eb1c3"],["/categories/R语言/编程环境/index.html","cc38106de21a1e98fda2becf1e7f9916"],["/categories/iPad/index.html","121410804d731e556e76ddff16089a5a"],["/categories/index.html","5bd9eb7bc5866e4bd570906a3c04663b"],["/categories/中间件/index.html","9b52b0e1274b6a7d92a2a963a501c187"],["/categories/前端/Vue/index.html","94542631e5a0b1e86a87eae6da04fe19"],["/categories/前端/index.html","2229c2bf76126054e684909942bf78d7"],["/categories/大数据开发/ElasticSearch/index.html","cc5117e447658097738d3e8893bf8b5f"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","1399122ef060d072adb79d0fbfad4db5"],["/categories/大数据开发/HBase/index.html","673bca4c3eb6e66ad112ea58278ebb56"],["/categories/大数据开发/HBase/学习笔记/index.html","b3556ebf2e8e22a40a4309ed86c62cd5"],["/categories/大数据开发/HBase/环境搭建/index.html","a6d98e3fc5ddfb56e24d4a6c47129997"],["/categories/大数据开发/Hadoop/index.html","a0afc11db5fe71395922dbe3e6f16800"],["/categories/大数据开发/Hadoop/技术/index.html","7b5bee306bf16f3180af3ba3dceb4192"],["/categories/大数据开发/Hadoop/环境搭建/index.html","abb35915501128e37094f9eb3070a8aa"],["/categories/大数据开发/Redis/index.html","5ececee1178f6bb314c56dd10c8968c2"],["/categories/大数据开发/Redis/技术/index.html","0c5d6a1bb02f24c286f4de41fdffc7b5"],["/categories/大数据开发/Redis/环境搭建/index.html","1c2a2b41eddc6ca276af4be0a46f6b7e"],["/categories/大数据开发/Spark/index.html","39844197763efdb0bb4275a1f0776dc3"],["/categories/大数据开发/Spark/环境搭建/index.html","804e9cf3ee2cbeac947f9e6572220487"],["/categories/大数据开发/Zookeeper/index.html","eae28976f1380e204d5bc560a237bd9c"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","2347629970ab4355b6a1f043d93b8fd6"],["/categories/大数据开发/index.html","86087ce69951b08f327428e8da308edf"],["/categories/学校课程/index.html","df18732e3230422f875fbaf8512e9a6c"],["/categories/学校课程/计算机操作系统/index.html","4126eab037651ae39107526d91b961b7"],["/categories/操作系统/Linux/index.html","a7157e902c41e858636a53645de852a9"],["/categories/操作系统/Mac/index.html","5ebd61938c8dee042cb716dd67fd33bf"],["/categories/操作系统/Windows/index.html","c2edfc2266a63b2a1fe1c1cb49ab5f61"],["/categories/操作系统/index.html","d49fc00f4a3b72f08ffb2907aeae1dc5"],["/categories/数学建模/index.html","f442404d05e7572ab6e106dbb61cea23"],["/categories/数学建模/latex/index.html","df3460581c53f4a7c477f65f488f6758"],["/categories/数学建模/优化类/index.html","42636d8ccbb18e54195bdb725fa016b2"],["/categories/数学建模/优化类/现代优化算法/index.html","d7f5101eb71240a8515fca0525e4940b"],["/categories/数学建模/优化类/规划类/index.html","aec4f1564596f14b3c3b87a47402d01a"],["/categories/数学建模/绘图/index.html","9fc5b489c033bfab42e8a8580aeb472a"],["/categories/数据库/MySQL/index.html","00ec1ac784d953b05397581fed41c11b"],["/categories/数据库/index.html","1d322ff9319be512812c6550e4ac3112"],["/categories/数据结构和算法/index.html","2c4503ea092b332e5ba005cb7a31d76f"],["/categories/数据结构和算法/page/2/index.html","c5aa7fde315200f9d7c58f46a6274f15"],["/categories/数据结构和算法/基本原理/bfs/index.html","968cc4a1af881abed0e18fdb06d3d182"],["/categories/数据结构和算法/基本原理/dfs/index.html","f45ce5539a5da2b8ae20786e66a6fea5"],["/categories/数据结构和算法/基本原理/index.html","5db55a99e67b46ff9e8f1dd251ea91aa"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e5b59573f5023dc196e428d523aa7252"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","01f4853ab73cd250f5f1cc972af0b55f"],["/categories/数据结构和算法/基本原理/图论/index.html","6ddfabeac1f1c4e11c268b311aedaef8"],["/categories/数据结构和算法/基本原理/字符串/index.html","f5642788b7880c64006d5ddd0bc39e73"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","01c05e60bfad3904d11bd01d77f97d35"],["/categories/数据结构和算法/基本原理/数论/index.html","7141afaf325566005d3918f4b8b5d9d9"],["/categories/数据结构和算法/基本原理/树论/index.html","c62c0f432e939bcc85a4f3160c31046e"],["/categories/数据结构和算法/基本原理/链表/index.html","3cb2cbeaeb49430e470a3824bd5e5ff2"],["/categories/数据结构和算法/算法题/index.html","7c5b3f265b1742163a0013768b7d2897"],["/categories/数据结构和算法/算法题/二分查找/index.html","233b55728fbac44853844c69ed8773e2"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6aa634ed096c089f6298c039d38ff997"],["/categories/数据结构和算法/算法题/动态规划/index.html","c67dd6fc1cf39d258b907dcbf8e417f0"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","ad4d89e719d9c4727d0fb70930f2200a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","11eb4881de70cdde3db3273f627e3745"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","f9b08f0bfca0118de78ec4f0e7045881"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","5fe741eef27f1e575721040e0cfa1899"],["/categories/数据结构和算法/算法题/数论/index.html","0ebb4a9791495adbf1ebd16f8b2cfd62"],["/categories/数据结构和算法/算法题/栈和队列/index.html","6cee6d6fa933d14e71af93da102b4db2"],["/categories/数据结构和算法/算法题/树论/index.html","edd6ce678a3ee9133469786f535f46d0"],["/categories/杂七杂八/index.html","40e424ee2cf9a7777fc3ba1856754651"],["/categories/杂七杂八/博客搭建/index.html","8b8fe2770d4e7c64285be0b858c42747"],["/categories/编程工具下载/index.html","fa124e2036e3a3ccd3217da45edfdc38"],["/categories/编程环境/index.html","065fe934c13657d2975e1bad44fd08da"],["/categories/编程环境/大数据/index.html","b68d50550e3a1453c7e8b18ce8a0ba4a"],["/categories/英语学习/index.html","70432f64fd6eb3534812c5f2c617a57a"],["/categories/英语学习/英语语法/index.html","7f0975edfdaa72f6cfaac08146a0f5bb"],["/comments/index.html","72eace3d2be87d5a83f4839809c022a3"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","148fc7ff9fb7ad0a490c639baf797c1a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","841d14308254be9bf91b25c02afe25e1"],["/movies/index.html","a22ee04f4e3ab0534f7448959665ab0a"],["/music/index.html","f48387e1985ac902cd881188d1682d28"],["/page/2/index.html","efcb53a23a0cd0dfb47935ebac926a33"],["/page/3/index.html","81b8549837eaa17f5b47a9430597820e"],["/page/4/index.html","9fc54a406ed1dbfd57aedc78ef54bb3c"],["/page/5/index.html","70faf99734451a60884dd1e015952e89"],["/page/6/index.html","ce3bd3609d0320689492abfcba14c4e3"],["/posts/1021360842.html","26d57a0ca4911ef1a2f3ba4167da4bfc"],["/posts/1120620192.html","ca5ee7e9003fc12b95009d08ad470bd6"],["/posts/1141628095.html","bf46b289c58035632403eea40c3ece96"],["/posts/1168613674.html","6f59cf248fb3a9df1e304b5acf3a9b80"],["/posts/1219920510.html","99df033c2c2bd6db5a414e47644c2864"],["/posts/1222166338.html","49607a9dbd3c366c38432e8d8ff50ba5"],["/posts/1259097482.html","3fddec3ad9e4812a62950ba4e7e95ed8"],["/posts/1271036369.html","3ba3c7ea766ce2f27fdc5d45280e534f"],["/posts/1312847445.html","c124b6dfebf2388a8eb561041d189ee8"],["/posts/135355774.html","6d035d4d84f90f5942130aae2fdd3106"],["/posts/1375344716.html","e064e8c2c8ab5c5d726fab2285f8e083"],["/posts/1388991698.html","43cd8b682ea35cb9855a42c08e5978aa"],["/posts/1410315814.html","065c781d2a00fcf32ade0d1738553d70"],["/posts/1452790229.html","4a6457dc9fe29b6978667438ac2b6c2b"],["/posts/1470079884.html","338a9d8562222a16caf3b30e3fb575ff"],["/posts/1470079885.html","ac3a1a7f7a12fa3330396eb78caf8812"],["/posts/1470079886.html","693a52b29a0be4a5c4d43bd70e498c15"],["/posts/1470079887.html","89e9ffa40344e59ebdb8c347c051ac54"],["/posts/1498536549.html","10da7f337dee2d2875c47e62d9811c55"],["/posts/1539568593.html","fd74e99496ed50089a0fe6a52f7ebd81"],["/posts/1547067935.html","7355068a757903a17b38da2e25843067"],["/posts/1557866301.html","55b6bc0e62267d3e14d56cb483cb464a"],["/posts/1571776361.html","c1d36bf96afa0b7e91b568445d3f1e97"],["/posts/1605124548.html","f12f6da0a0f2f8b121f99b50b283b61f"],["/posts/1633036852.html","613d479249c066550c9caf570e56c56f"],["/posts/1674202625.html","76b8835eae696511fd5a0c79869a7b2f"],["/posts/1765123828.html","fcfc095ef7cfb01318839df8006388d1"],["/posts/1767336200.html","eaa4753a87cb9c43fe0c906e768313ac"],["/posts/1776114197.html","68967aa049ee8cd05448fc30255ba3e4"],["/posts/1817748743.html","72f1a488c3376b29484fc12d630714f4"],["/posts/1925125395.html","15646d3d1eb7d91aa52327c76a51971e"],["/posts/1966191251.html","923dbfd5a43a5982d183007180f66034"],["/posts/1987617322.html","d7f686e791408e2bdf18f364dea3d79c"],["/posts/1999788039.html","6c17202428f6aa24afb6bbd1c8440bfe"],["/posts/2075104059.html","ad9e9d70e4b4cd7e75a0da370f4a6453"],["/posts/2087796737.html","fea7c81b7c017c4df898708bc26eb9d9"],["/posts/2106547339.html","f164b92dacd6d922659a5241eaf96d58"],["/posts/2207806286.html","340c8ae11ee735508ab950765aad8056"],["/posts/2225903441.html","f6aac13d3ef906ae6022a5692e3e9629"],["/posts/2265610284.html","255e088e7090a3b9187a33c07de85de1"],["/posts/2281352001.html","51c0f14468a52b8b592e16f48d6d5af0"],["/posts/2364755265.html","ca3a16f415f1bf0e829f5b42a598dd93"],["/posts/2414116852.html","c2338c10aef0581163178d9156b71c63"],["/posts/2421785022.html","7f763c1b234cce38c5a0c20d5380394e"],["/posts/2482902029.html","457a3b040b9d8b34ead132a31198f5fc"],["/posts/2495386210.html","0ad83f5c0be46fad8d4f0e850f6d30ff"],["/posts/2516528882.html","531054ba34a3dbf92821866c4487fa0d"],["/posts/2526659543.html","7fbc75fe508c88ee2cf58474b1d89a58"],["/posts/2529807823.html","f18f63e10028ec17c8f64988196ffd34"],["/posts/2596601004.html","f429a4f59fa9e5d3ddb77eb210319db1"],["/posts/2697614349.html","0fd498178c7effaa801693231696ee59"],["/posts/2742438348.html","3e3a41acf60a644bab4c5cba0e9865df"],["/posts/2768249503.html","ae732384fe6c8b5ef7249ddcf7a62390"],["/posts/2864584994.html","80198b27ec90112e6b89af2583a9a734"],["/posts/2888309600.html","a202718c666a3aef89c25ecbba25c4e1"],["/posts/2891591958.html","d9363d0da8fe7185b1cde1270c123f55"],["/posts/2909934084.html","f761ab7693f90fa17e448728eb3f54e1"],["/posts/2920256992.html","705084cedc8966f146bde96321b9f3e4"],["/posts/2959474469.html","834aa257f8fd53b646de105ce972495b"],["/posts/3005926051.html","e92dd8556cbbeab5de9c541523c188d4"],["/posts/309775400.html","ecba207e52a411b4f2dcb3ed5449591a"],["/posts/3156194925.html","3f12d399f969bb5ff7155f56e20a3aae"],["/posts/3169224211.html","0242e01d44f4978326d6499c270a62b3"],["/posts/3213899550.html","376dd76c40e53b506345ae638df3c512"],["/posts/3259212833.html","3cc9ca0b295ea429f5f8a6cb5b6a8a40"],["/posts/3266130344.html","6a84230f64409cbda3305cee2e296772"],["/posts/3292663995.html","45e22d72fe095dc3a971eb337a5c3403"],["/posts/3297135020.html","1985cffbb9934074d616c464136fbe1a"],["/posts/3306641566.html","8c2eda7a445c21ff213f05e68e2417f7"],["/posts/3312011324.html","6707a9c62d6615387c0f89813257eab9"],["/posts/336911618.html","cb6d9d583cf2a7667a666d3e01dfcbcc"],["/posts/3402121571.html","b093bff0e7f63796bed5a14b78309a84"],["/posts/3405577485.html","62985692cce710deb742c8dffc26a2fe"],["/posts/3498516849.html","658141e01738c82288637e9e46bfc3b0"],["/posts/3513711414.html","fab4c6697d8b9905776cc139805eab31"],["/posts/3523095624.html","7c37d3723d324b889ebd2e968cef11cf"],["/posts/3546711884.html","b5b26c7263e171a2caef3aa7cb3c1380"],["/posts/3731385230.html","543c45e0c1642c65950dfc052f0dc796"],["/posts/3772089482.html","e7b53f8443c61e8aec5c31881bd55116"],["/posts/386609427.html","8fe6c3a390ecd0bea09818279bc9f632"],["/posts/4044235327.html","a6602b3b0daa34aefe5210a7f984971c"],["/posts/4115971639.html","a9f4ea777753dd018530ab83825b49f9"],["/posts/4130790367.html","3eb8d51a65ccaa302d74f674f341268d"],["/posts/4131986683.html","b94e1ed90951a15a15d0ed2887ace26b"],["/posts/4177218757.html","b463d5b15415e2d531719e3d2d05de36"],["/posts/4192183953.html","42fd58990c65d0a528983d1621c297da"],["/posts/4223662913.html","9561b190a300a156ca314c5f3f49325d"],["/posts/4261103898.html","ed8d1056baed307612e053b868806b0e"],["/posts/469711973.html","182b3e76b400336ee908eb4fb4d754d6"],["/posts/482495853.html","7b50ea02bc2aaa2367dc6a0b635dbc7d"],["/posts/488247922.html","f797f0d8e7a00bcb0be4f272236ad2dc"],["/posts/517302816.html","6226b56c91ee5f052754a7e5c275c85f"],["/posts/570165348.html","456ef016776a28ada1ee79bc66f1fee0"],["/posts/595890772.html","3e5ebd07d88303272fecd6915772ea10"],["/posts/67485572.html","eed9596e5cdd1bb35f2489ae0d4c6bae"],["/posts/694347442.html","ad87ff683381ae1e5c26cf5ea019e21d"],["/posts/707384687.html","0f29d84aad763d02e830b9e14f92ad5c"],["/posts/71180092.html","de4680bb6c0cf523393cb5a75b5777a1"],["/posts/716459272.html","9feee11e80d39100295570eaf857df98"],["/posts/765481613.html","fd796b6e59a3c41fbd56f19a6c891491"],["/posts/778231993.html","1ca742d9cee0f7797a7c8011037204d1"],["/posts/795397410.html","f835fa4af3df07134e3c82af8f2d493e"],["/posts/820223701.html","efa7e44ff2e1a331823bbc06d0919052"],["/posts/830372185.html","dadbc8fe40417a4c1a112b5e71943864"],["/posts/88294277.html","c460450c1b5b9fe1c2f9a5263ec694b2"],["/posts/939963535.html","1f2728431f00818d9a86bdf30bb75111"],["/posts/983786067.html","99c857693b3d904da17cf1ea807c34dd"],["/sw-register.js","28f59bef88dd6f20dbf0f90aaa96f08e"],["/tags/C/index.html","e3fd34eaf498590afc0e3ea5346488f1"],["/tags/C/page/2/index.html","4485e2137ca89fda4b6dab3857756690"],["/tags/C/page/3/index.html","a8b89370a0a6da226b724a76d1864225"],["/tags/C/page/4/index.html","5fe09cfa55d1136257a3554afabbac4a"],["/tags/ETL/index.html","575864692da04dae4c21fb34d0fd5a0c"],["/tags/ElasticSearch/index.html","cada1e04b7d37e80aa4d926e16b5703a"],["/tags/GUI/index.html","4b0290ce2483cb63b277b519eff5402f"],["/tags/HBase/index.html","2c83c196c4296fa886fcf296b212c592"],["/tags/Hadoop/index.html","5bfd157af0fe9d0c0b3222d05812cd75"],["/tags/Hadoop/page/2/index.html","3acaa70cf5a109e43b530431d39a7380"],["/tags/Java/index.html","4edf06441201301d65902adc57f2a823"],["/tags/Java后端/index.html","640c70cc2e0bae438ccfd4031d76d020"],["/tags/Java后端/page/2/index.html","5faf9a73570780b6c74a40997c3f9247"],["/tags/Java基础/index.html","ba44f47ebabd8566115ff3050d6006ac"],["/tags/Java基础/page/2/index.html","08f2fdf8eded5e5b3e9c23d6c9a1ebd2"],["/tags/Kettle/index.html","556d497478982fc8d1651e68ed53fa02"],["/tags/Kibana/index.html","f6e2669947f1438fb777174ae57a2345"],["/tags/Linux/index.html","61b658d0f5e44a6c0c701ff49c5610a2"],["/tags/Linux/page/2/index.html","3dc5edcff6320e113032721ed6f4c8c8"],["/tags/Linux/page/3/index.html","c4105d400b6e00bd6c690f618e8f8ebf"],["/tags/Mac/index.html","9becd8311112bc6d37192c4cc09ea3c1"],["/tags/Mac/page/2/index.html","4889d2df91c51d4f0aa55e6f82389923"],["/tags/Maven/index.html","27eef3965a5e8ed6ce48af8c00cefa95"],["/tags/MySQL/index.html","5d3b345eb6cb134dafd871e89ee541da"],["/tags/Python/index.html","ec545445afaa3bf7ef80a00507f61358"],["/tags/Redis/index.html","794f74651abe2040984599ee06395ccb"],["/tags/R语言/index.html","cf9c8bcbe61e8d256b610cb9e2a751d2"],["/tags/Spark/index.html","ff2cd9d8998c5796118579b538dfdd89"],["/tags/Ubuntu/index.html","16d1dc6651c9eedc7d2babe0e904302a"],["/tags/Vue/index.html","460d74329acd2f652d73a55a3b48ac50"],["/tags/Windows/index.html","4dedf482b82ec00c4ac769b2fe3c2bbc"],["/tags/ZooKeeper/index.html","02ea66b1608987d582a82393ebf1c7f9"],["/tags/bfs/index.html","a4f19408e5ef935d9f0d425f023b5394"],["/tags/dfs/index.html","be9dfb3cb418fa218d5e5358d9af1a2e"],["/tags/folium/index.html","bc40ca3ab55f60c74713888fa474912d"],["/tags/git/index.html","8736cdcf14348f7d6cd2de40645a0ce5"],["/tags/iPad找电子书/index.html","026b24194b1ed06d6cac156f6d588b33"],["/tags/index.html","5b9159f159079784d908b62874e7a21e"],["/tags/latex/index.html","31a9342ef6536630c5528d9794056f6c"],["/tags/中间件/index.html","bcab179869a551e00522312d433dbfe9"],["/tags/二分查找/index.html","473a33a368a86e0143888fe65cd88224"],["/tags/优化类/index.html","f7c9f95caa809c767ed86240d105a3b4"],["/tags/前端/index.html","6f5ab172b62b77610a59647406d2573f"],["/tags/前缀和与差分/index.html","5941641ae44ccd7ae976f02f1f2e95bd"],["/tags/动态规划/index.html","ad84b758673c5492816adbd1c622da19"],["/tags/动态规划/page/2/index.html","501ef208d22ddbd10928430345009857"],["/tags/博客搭建/index.html","a9d8ef248d9d973b15f816a9d0ecd1c0"],["/tags/图论/index.html","8be0dced1c03fc6d55e244c105197692"],["/tags/大数据/index.html","f0e648dcd7841353ea81d8f9360cb976"],["/tags/大数据/page/2/index.html","a0dcf724ba57cfb60a2bcc5b624a6ce8"],["/tags/操作系统/index.html","5e7f76be254c8c19f66e6fb21d9a7740"],["/tags/数学建模/index.html","8359cb9e586b140aa7d5dbd6e9106fd6"],["/tags/数据库/index.html","9c5b9c03822d4ce577d9dfcc710f0d0a"],["/tags/数据结构和算法/index.html","fd628db97a0f5cf89a2002f78df7d91b"],["/tags/数据结构和算法/page/2/index.html","aca89925473c1bbf8ba5f4bf7929e389"],["/tags/数据结构和算法/page/3/index.html","f954c77aafcbe019c9adcd567752f843"],["/tags/数据结构和算法/page/4/index.html","5a389df1a24079daae401d457446cf9e"],["/tags/数组和字符串/index.html","56d77b9bb277908f2ffbb4709857596e"],["/tags/数论/index.html","af3733d8ca8bfb146253670acaf71b0e"],["/tags/枚举类/index.html","4b5ae569430bd5ebb5ab21826ecc0804"],["/tags/栈和队列/index.html","77ec58459fff9035e728cb414e58e8a4"],["/tags/树论/index.html","16002913f76f55b35e65d9362192017e"],["/tags/测试/index.html","ffcf5f249d45dd4b8e7e7fe7b81d7533"],["/tags/环境/index.html","b554f3ebd41f48e9ffcf57cf9d427a77"],["/tags/环境变量/index.html","ea16649fc9eab03ab759dc2663286303"],["/tags/绘图/index.html","b7e206a090b6eef5c11d928ea94aafd4"],["/tags/编程工具/index.html","8ede2f3432009625205e7a81a24bbe88"],["/tags/编程环境/index.html","18adcb377df55b2b16e3c927c25ff473"],["/tags/网络编程/index.html","f2ec70fb906a3f7dc05c9588b96850be"],["/tags/英语语法/index.html","755bed7896351c6c431f4f8ffd119c36"],["/tags/计算机操作系统/index.html","bf535f84fa24150ba8956283aa6068a9"],["/tags/论文/index.html","cbaa3ce55e2e6c9575dddfe3c41e42d4"],["/tags/资源下载/index.html","e28ac0eab9aa517c8f3803ef5132c4a5"],["/tags/链表/index.html","b59572a45c49ca29a5e0d67e10a9c0b9"],["/tags/集合/index.html","e5e538831343adb244a9eb14a93d5116"],["/tags/集群/index.html","72bb6c8c0a623ee53b250b5c0635347d"]];
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
