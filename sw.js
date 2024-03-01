/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","9c20dfca3486bca9bee7108459d89187"],["/about/index.html","8b27792c62c63312f6cda047e50d3e40"],["/archives/2023/01/index.html","64a415275bf9212c44881507d951df57"],["/archives/2023/02/index.html","ab489c744435709cddfa9e7cc855a794"],["/archives/2023/02/page/2/index.html","7b76a54435d9e060082491f2cb315417"],["/archives/2023/02/page/3/index.html","14644f438af0fb952afa58905dc68bf1"],["/archives/2023/03/index.html","2f1647ce02cb33059855c0cc809c0981"],["/archives/2023/05/index.html","25e07ac05c0f0e36e8d9e0aeea8f62e3"],["/archives/2023/06/index.html","3bdcd3948fed6207d4f06275660f7496"],["/archives/2023/09/index.html","42a63d2b1d9a5e2de721e5175aa474c4"],["/archives/2023/11/index.html","2a059455334368dbf55e19a2ea7c359c"],["/archives/2023/12/index.html","d55634cf6308bcad854e3fcfe1df28f6"],["/archives/2023/index.html","dd28b5ff3bf77a33b415b5b4c98ac99c"],["/archives/2023/page/2/index.html","66dad2f2ca49320b76b5211353f6e04c"],["/archives/2023/page/3/index.html","9e34a5f0052e030ecafd46f5d075c6ea"],["/archives/2023/page/4/index.html","549e7f266a002a9f7892d58ac2321af5"],["/archives/2023/page/5/index.html","1a3fed44b0e7e1884a78acdc995bc1da"],["/archives/2024/02/index.html","ad33b63976f86f13c9d3b2b818c77fd1"],["/archives/2024/index.html","7bc33deaed7c50234fb63829f7a6617c"],["/archives/index.html","89308d3a0bcaaec01f7cc9e3e5dabde3"],["/archives/page/2/index.html","159dfea00b797a0905d05f83ef460f5e"],["/archives/page/3/index.html","eca20a4490d21fb96bffb4a2ac4334e4"],["/archives/page/4/index.html","b294f0d891ace222a51fae6638c87c86"],["/archives/page/5/index.html","22e30f40b7328f2e0a611186cc40714e"],["/baidu_verify_codeva-qQP2iZOMLX.html","ccfe802989846ac79df9eb62e9de01b2"],["/categories/Java/index.html","faa4ede669ddf4519ce0b84f65b8e896"],["/categories/Java/后端/index.html","7c02e91283f6796ffdd243c5365bc345"],["/categories/Java/基础/index.html","071fb127f3e85e225fac271af3a6b783"],["/categories/Java/基础/集合/index.html","3a8702b9e868136cc0290e6aa3604c1f"],["/categories/Python/index.html","09ac22c2a336f10218140e212239d9a0"],["/categories/Python/编程环境/index.html","697b49f5c93694390263735d01f64149"],["/categories/R语言/index.html","7afc3c2b3d0f164e2531f13c0b365b03"],["/categories/R语言/编程环境/index.html","fa93626455306202eea29acb8644e5b1"],["/categories/iPad/index.html","f872c0d0fcc2f39157e82021023d0e0e"],["/categories/index.html","4f22613fc0ee8d04779ffb94c70ba029"],["/categories/中间件/index.html","5d76f1faaa16bdc989ef87197ed0debb"],["/categories/前端/Vue/index.html","1cab37e115048658bed1da407030e318"],["/categories/前端/index.html","b6d888a3d594a64bfae463206175565b"],["/categories/大数据开发/ElasticSearch/index.html","b927c0aebff9e2c5bf4d2e1242b0bb8d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","bffd94f6d91f8410cd13e1b8e1c8f0d2"],["/categories/大数据开发/HBase/index.html","d07e956a8f98b6ae207a70a74318e774"],["/categories/大数据开发/HBase/学习笔记/index.html","cedacbb55a9ce8aed8c04df3941a6deb"],["/categories/大数据开发/HBase/环境搭建/index.html","371cd95f510a45dddbf149fe36876fe0"],["/categories/大数据开发/Hadoop/index.html","8b665fcc3919f52adf3e192dfab94d40"],["/categories/大数据开发/Hadoop/技术/index.html","a981a07412cfb9e1d02fd9d243300147"],["/categories/大数据开发/Hadoop/环境搭建/index.html","cf4f54a89da699d37090f208d43ae8af"],["/categories/大数据开发/Redis/index.html","3c6041c80d6d7543ab898c865720e4f9"],["/categories/大数据开发/Redis/技术/index.html","d2e766826ac0ed1909aa395ecd9886d3"],["/categories/大数据开发/Redis/环境搭建/index.html","33485dd487b8ebf72631c6e6706e3f96"],["/categories/大数据开发/Spark/index.html","acf35c256f27065ad227a9e3bfd2f274"],["/categories/大数据开发/Spark/环境搭建/index.html","2eaf1e0d49cec85cfb227af06163e706"],["/categories/大数据开发/Zookeeper/index.html","522842f0d751dad835714ca24773adea"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b84c016c06d0f8138c826b87aef245e6"],["/categories/大数据开发/index.html","2c9656456a6531c64445583806c568f4"],["/categories/学校课程/index.html","5e36c41146de590cc03c1d7134673a41"],["/categories/学校课程/计算机操作系统/index.html","388935aa8ef6388e583dc2ce7c23113a"],["/categories/操作系统/Linux/index.html","fe51466b443d9ca41604ef750d44c8c4"],["/categories/操作系统/Mac/index.html","4c4db33d4b395506d3857db5133bfa03"],["/categories/操作系统/Windows/index.html","bb17a22b7f24987828e9145ad74b8f54"],["/categories/操作系统/index.html","b70bb5b6cd5c5622cba0732ba060def5"],["/categories/数学建模/index.html","b481cb071f81678c883b425edf17b8c1"],["/categories/数学建模/latex/index.html","b9dea0c75f5432785c9df39fe1617b84"],["/categories/数学建模/优化类/index.html","4a5e0ec8a2023ca4891a55c5cc0c8b0b"],["/categories/数学建模/优化类/现代优化算法/index.html","3a445732246b6ff12490b5971f30a1a1"],["/categories/数学建模/优化类/规划类/index.html","ac805e826eee090dd5048fdf5cb8e3b0"],["/categories/数学建模/绘图/index.html","11fb2c6b84f1c2fb68b028e67113ddb6"],["/categories/数据库/MySQL/index.html","655ef5d64bf3570e34a6556732b418b1"],["/categories/数据库/index.html","ec7ac9e31ee8f6bb0c0956fdb0c81dd1"],["/categories/数据结构和算法/index.html","a136565199ba4245732caea6fa625a47"],["/categories/数据结构和算法/page/2/index.html","9fbc78003d06ca4a254caa932d55d29c"],["/categories/数据结构和算法/基本原理/bfs/index.html","15f041d423fe4606a993a03d6dbb0e0c"],["/categories/数据结构和算法/基本原理/dfs/index.html","f0b0f013d01adec38236380873590d4b"],["/categories/数据结构和算法/基本原理/index.html","dd7569db5061cc40a6ae8b5dbc776147"],["/categories/数据结构和算法/基本原理/动态规划/index.html","03d9bdbf1064ec63265fc3c7fa5df29c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3ee7acbf96d27416a2470d3f9df62043"],["/categories/数据结构和算法/基本原理/图论/index.html","4d02223250d1db7dc92644d668ae743d"],["/categories/数据结构和算法/基本原理/字符串/index.html","2cc723c04ca4dfa788765621b5b47437"],["/categories/数据结构和算法/基本原理/排序/index.html","0ae25631b46feeca99f4d578a451e07d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","719f0865a202920d7390c2fd94b509f3"],["/categories/数据结构和算法/基本原理/数论/index.html","b6306a0c750a1f2e0a4d675b4897c115"],["/categories/数据结构和算法/基本原理/树论/index.html","d1275d7383b51a188732986c3d2a4eaf"],["/categories/数据结构和算法/基本原理/链表/index.html","3a618efa01124b851fc8be9ac7bb344f"],["/categories/数据结构和算法/算法题/index.html","1f8dc01186249fb734a3e8826f38327d"],["/categories/数据结构和算法/算法题/二分查找/index.html","82aaee1e2bba61a46431983dcc1e7ed0"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","589866b663d6d90992db1e7da7aace55"],["/categories/数据结构和算法/算法题/动态规划/index.html","756204cf98e1e3e6bbdfae5dbb6cd596"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","297a0dacc92f3896742caa29a8a0f015"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","80c4785c0dd12e3b2b4557dd3f50633d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","08c979f8904be352e1a7ce567c33e2e4"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","8c74951571196d144c88acea5334c01d"],["/categories/数据结构和算法/算法题/数论/index.html","7f4de4a12bc61a206eadf25cebfcbbdb"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d686dd60846d433047a1cbadf5321ef5"],["/categories/数据结构和算法/算法题/树论/index.html","0aa30dc3cae00fa59ba776e056148895"],["/categories/杂七杂八/index.html","b69b4e3d2ea801b82729aa23ff48af55"],["/categories/杂七杂八/博客搭建/index.html","2b66d569f17ecb4630325c2ab9edd0c9"],["/categories/编程工具下载/index.html","66cec40aa089c9098bcc48b751272ffc"],["/categories/编程环境/index.html","2ed5d6199be9cf1222b979cc52e12cf2"],["/categories/编程环境/大数据/index.html","f29717c3add2b7db145f414eeb61d6af"],["/categories/英语学习/index.html","60d329efcb07b640e35028bafff9f2cf"],["/categories/英语学习/英语语法/index.html","8f3017ec1cbadab61e6defcde2a4412f"],["/comments/index.html","cb8cdd65bd3e5c951d2e92bd055964ae"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","342a5786aeae91e6d0ac53dd2e18a2f9"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4f9cac34518044db43819e714885ff06"],["/movies/index.html","b6f16d6dd512fe75ac5bb18ad1cbba7a"],["/music/index.html","39533be654b4e583e6f88b0d9285abcc"],["/page/2/index.html","c24c6b91f4626b88fa40eb89746554be"],["/page/3/index.html","da7886d3726365df0f63b67c4d9ad61f"],["/page/4/index.html","f7127096565eb8203cbea461507c5096"],["/page/5/index.html","f4464ebda40f3671aca4b6be05640637"],["/page/6/index.html","1c6caadb1419b71aa39635092e2b0caa"],["/page/7/index.html","4be110768da8be3ce08bc1702b055fcc"],["/posts/1021360842.html","16e4d1abdbe4b5311bfb16d58af7f4af"],["/posts/1120620192.html","d1a85485b992b0fc14bbf51520a115a9"],["/posts/1137707673.html","e94644af10f43921dcb5b7ff5556dd88"],["/posts/1141628095.html","3ef68c5a7139eb90269d84f85cee150b"],["/posts/1168613674.html","aaec2f552ea24ab8dfd64b0e0f994076"],["/posts/1219920510.html","b609ad5ad353abfe02b85fbb3a2581df"],["/posts/1222166338.html","471f906003f6a98cf92ef6234d345ca4"],["/posts/1259097482.html","3f0511b57c2a70aa55621afd5ec81c7f"],["/posts/1271036369.html","441ec125ca02d89b8c20f3230ef6c585"],["/posts/1312847445.html","a4584139abb43addcfafc571a8542ae7"],["/posts/135355774.html","29177525b71b68f36b5f6648e4123bb9"],["/posts/1375344716.html","02dc36b71b476a4f43d02491ff0bdd0e"],["/posts/1388991698.html","46c34f960b00dd509712531ab6b9cf81"],["/posts/1410315814.html","efa5e94d3e8352c2e8bc423238e06060"],["/posts/1452790229.html","49e86da9ebc64ea823f505478fcc905a"],["/posts/1470079884.html","cf209e1b946ac59231d2975c34adebb7"],["/posts/1470079885.html","f929f1b23e937ac49431d874b32795d3"],["/posts/1470079886.html","3a8025225bba5304576124dbdb597da4"],["/posts/1470079887.html","367aa118089eb02c68a6f96ac818d809"],["/posts/1498536549.html","7db03f6ffebb926e9b17cce6bfdae064"],["/posts/1539568593.html","2edb75f05528e49d2d0865ee3b695397"],["/posts/1547067935.html","f364b5e77f432c8578b17ea6f750f918"],["/posts/1557866301.html","bdd6794662ac38131238cb7c76511405"],["/posts/1571776361.html","1be859c06ebc69b6691262b1fdb91272"],["/posts/1605124548.html","b399a3b635bcfe080e84136b470a409c"],["/posts/1633036852.html","41c5132cafc9525b57c25d64ff663a86"],["/posts/1667740714.html","ff501b753d15fb0e6808351518cce2f5"],["/posts/1674202625.html","77e0bb8851ae038f44fa3e75581ced79"],["/posts/1765123828.html","cc126e5b9c160f73505799abfed7dbda"],["/posts/1767336200.html","41116382e27f8296ff48f10569e259ce"],["/posts/1776114197.html","3978fae081797b9c0d0249f702a749bb"],["/posts/1817748743.html","40457a3731579843cd931a63aa6e5519"],["/posts/1925125395.html","1856fb8250114ae0d71ea5f2b3d349b6"],["/posts/1966191251.html","0c69cf1fd28e2c29d437a32ead7b5bd6"],["/posts/1987617322.html","dd2c4ece41f5d431caf5792a6f94a030"],["/posts/1999788039.html","d7bd27388e68e05e14bf08d64b17cfba"],["/posts/2075104059.html","981f4fb0efbc3434200d44496204b85d"],["/posts/2087796737.html","c87f360d3b1351f57ec26282ebd2971f"],["/posts/2106547339.html","4c5ca88536597e51b3858ab5e320b53d"],["/posts/2207806286.html","5e65bae28bc28bdad6a2b1bd3dd40ee4"],["/posts/2225903441.html","fb20900d95b4a5cad5c6c66c4f1789a0"],["/posts/2265610284.html","4b2a20c65085b06e329a7e994fbe831d"],["/posts/2281352001.html","3ce2eb97d37e3d1149ce191803767cd4"],["/posts/2364755265.html","1cd79f485ae940de72ab00ccebb78365"],["/posts/2414116852.html","da13725cc7ba753ccc6672a48001f7df"],["/posts/2421785022.html","b8eb1c14efd4611b5b544d8cbfb1523f"],["/posts/2482902029.html","67e949eccbaf905bcae81e5b3875389b"],["/posts/2495386210.html","0f1d5692a6e41b18b814fe543d8393db"],["/posts/2516528882.html","43e2cb540252ebf920b85067c427ba82"],["/posts/2522177458.html","8a87fe3ce952cbc17d10ffa71f23439f"],["/posts/2526659543.html","aa102610de1112794f992ebb3737baf2"],["/posts/2529807823.html","13deae7cce7b47c0fb6db4a615263071"],["/posts/2596601004.html","099d12679993df4537e713f4fe99dd7e"],["/posts/2697614349.html","fe72a7afb1c455594906f86528df4ae7"],["/posts/2742438348.html","c20f6a1d60698713ce66795ef1607e85"],["/posts/2768249503.html","0b9c0ed9d932e4c79ed482c4eca0c855"],["/posts/2864584994.html","4e0dc1927bd6ad9cc2c8fddf3c011d90"],["/posts/2888309600.html","86e6cbaaaa97cbff7889b54779ba1d9d"],["/posts/2891591958.html","416adfa52be0f15e04dbb3fab95fe58a"],["/posts/2909934084.html","bf6f685e1eb1e01b294cc19d61a9d752"],["/posts/2920256992.html","1e925e2d4e1cec15571775865cb10ce8"],["/posts/2959474469.html","318e81a6f474205e573d25ee901d9cb1"],["/posts/3005926051.html","4fb07ef0cef1f4afe7587044a9aaaedb"],["/posts/309775400.html","d6675bce229fd286eeca1846f8a3c0a7"],["/posts/3156194925.html","63927c9f7c37db95567f63dd148f47c8"],["/posts/3169224211.html","41a932a69ffe4cdc29bc11551c9f62f7"],["/posts/3213899550.html","fce33a3e359310cc84ec2a6163f623b6"],["/posts/3259212833.html","19c28b1ee0d555c560eff9c613dd128e"],["/posts/3265658309.html","75e30ecdfde8e8a4e50149eca109f335"],["/posts/3266130344.html","f672f10582a32df96aa2eff730dbdd46"],["/posts/3292663995.html","bb750883ccc2e4cda3feed5680a5640a"],["/posts/3297135020.html","76605deaae75093f844b3e24802f6446"],["/posts/3306641566.html","123a5fc18feae989a5d13a973b628d0d"],["/posts/3312011324.html","9221e2454da0189cdc73ea56e6c7554c"],["/posts/336911618.html","0600d52f1c3db36b2822e813966f57be"],["/posts/3402121571.html","d226e57a6684013bfcd2a60dfc57d41b"],["/posts/3405577485.html","0b061ac884af4e9eb41f38b602b4bfad"],["/posts/3498516849.html","573d0c5bceca1776faf68bcb390cea2b"],["/posts/350679531.html","5dc3e9bea41cd774f86302bcc534302b"],["/posts/3513711414.html","61d09b51554ac060d826cd1cecfc6242"],["/posts/3523095624.html","9ca06788d0d3b5d84696296f4de13f21"],["/posts/3546711884.html","850aed4026afb11922e0dab0b232f210"],["/posts/362397694.html","68a6aa9458049b435375365e6410ffe5"],["/posts/3731385230.html","676ebd13ae7d34f1830acd9f0ef6387d"],["/posts/3772089482.html","ecec1c3c9f1a14184f77bf39324d247d"],["/posts/386609427.html","dde21f8e5e17691e2a4ec580a9cf903d"],["/posts/4044235327.html","0667da177dba621e1014044daffcf3c7"],["/posts/4115971639.html","fb191ac848f44b28afbdb44894837c3f"],["/posts/4130790367.html","d3463ead49637d0f7b033db8d2812de9"],["/posts/4131986683.html","1c9106b8de2d79549b130c8d18ddf471"],["/posts/4177218757.html","f23c77b7f93ab2f33b061dfe74b65237"],["/posts/4192183953.html","566a6d0293e31ae094658393771024ae"],["/posts/4223662913.html","0160d4eaf1cd8aa72fe70b36261d13b6"],["/posts/4261103898.html","1224e8955fde9f72cb1bf4da2b3e6023"],["/posts/4286605504.html","1473f44d2f698cee2405f99972c80ad2"],["/posts/449089913.html","a567e1d2f8fc0ceb8f020dd5b26fb775"],["/posts/469711973.html","20547dc8698ba0324e746bce9d8063b9"],["/posts/482495853.html","6626a32bb76d8f7c57883d63e4a4ae0c"],["/posts/488247922.html","d889ad745c578d0e49aa27cc10f16c9e"],["/posts/517302816.html","9732e23cc814d4373a80fa00086a7da9"],["/posts/570165348.html","510c5d0abc01b12e4317ea26e4e6875f"],["/posts/595890772.html","2d4af68df79b859fec8cb104b347fb11"],["/posts/67485572.html","d706f26231f7d595caaf495e38cc8f25"],["/posts/694347442.html","13c72a21189232673e8c1ded1fd0e641"],["/posts/707384687.html","6c3014566eaef311352c023395014bdf"],["/posts/71180092.html","bc6e7fdd42cc8dfa216443b7dcf9dda3"],["/posts/716459272.html","8128c811178f247aacc53b9a65870143"],["/posts/765481613.html","3ed0bf6f1ebe3e54a8eb85e85e81cfd5"],["/posts/778231993.html","7021e044093234db3cead2340f69f8be"],["/posts/795397410.html","0eb85c8c7b688d7bfee46608f2074971"],["/posts/820223701.html","4ecc3cc66df8776418a04551c3184cec"],["/posts/830372185.html","44cecbf3f7e26ae5d90a81aef00cf914"],["/posts/88294277.html","5f9dd9a6879045a2df8b346dd8e31f0e"],["/posts/939963535.html","6266b5b3150fee4ea5d2a2b221344122"],["/posts/983786067.html","6aabfe6e6d3daf8a05477d77ce649bf7"],["/sw-register.js","9aa6fb7882c6fc7d75b808d9da263927"],["/tags/C/index.html","4bea187e92ef56c67abcba2bfa44e334"],["/tags/C/page/2/index.html","76be582b08023c3d4c0ddaf531e9cb6c"],["/tags/C/page/3/index.html","87217b05f0162b512015a6643573f5fb"],["/tags/C/page/4/index.html","130b9bc0afd634a0a6c40c4fc59364c6"],["/tags/ETL/index.html","4416422426a8c82fd9c92b0267e9c58e"],["/tags/ElasticSearch/index.html","0b2be1fe70b539466e81024a49e1b4c6"],["/tags/GUI/index.html","11a60bb578d53aba3426c6e2754ad8c0"],["/tags/HBase/index.html","4c82f99b8d4bb1f31f855e447245bc6e"],["/tags/Hadoop/index.html","deff6ffa87d3cc807b331c5659fda0df"],["/tags/Hadoop/page/2/index.html","b83964d9cbe1615029dc97c1b01be182"],["/tags/Java/index.html","1d5ad136f824bd42ecd537666143d8e4"],["/tags/Java后端/index.html","0da19a342ba15e389f1084d7046e9bdf"],["/tags/Java后端/page/2/index.html","e61272e6f69a7006c8fa91f9318accb0"],["/tags/Java基础/index.html","a57562506bd86b62048a6849da404573"],["/tags/Java基础/page/2/index.html","772a8684a01a22113d03d0500777fbf8"],["/tags/Kettle/index.html","f1f37135b7041573af783a3d174fa131"],["/tags/Kibana/index.html","80116a6191ffaab0961674687984e868"],["/tags/Linux/index.html","f855d22d26d7d8a3bf317a553800f4e0"],["/tags/Linux/page/2/index.html","cba55c88891316a0e53c7f9c06f901c8"],["/tags/Linux/page/3/index.html","6f56323864ce5e26172bca13bd27b89f"],["/tags/Mac/index.html","8aabe2d917ae9146844c8a932437085f"],["/tags/Mac/page/2/index.html","eb212edfcdbea12dd061f123faf200ff"],["/tags/Maven/index.html","16dbf87a075bcde635e94d506a465690"],["/tags/MySQL/index.html","6c60962dd594c660c8fde594fced8344"],["/tags/Python/index.html","06018c267b475f112cf43a753f67ac52"],["/tags/Redis/index.html","6a23bc5d1f009fa1a8edfd13350f7d01"],["/tags/R语言/index.html","9cfee6ed855aef0a3f357a21ead2b906"],["/tags/Spark/index.html","c8917f37b5ecbde8f939b1876258de06"],["/tags/Ubuntu/index.html","54ad23d8355770eb7fd100938860e16c"],["/tags/Vue/index.html","2c2809e6f60288b4693a37b3b092dc5c"],["/tags/Windows/index.html","97b5eb29b5fa1de15a33765d26454e06"],["/tags/ZooKeeper/index.html","f7c2e1f9849899a61c1a50dc9d6539c0"],["/tags/bfs/index.html","2a1b82e342f3f30653c373d496fb766a"],["/tags/dfs/index.html","67c2f907337cbdc6dbb50890f700f1e4"],["/tags/folium/index.html","70678b9ace20098801e8d1ca8d987e73"],["/tags/git/index.html","aaa6290c287707d62496d7e171418a54"],["/tags/iPad找电子书/index.html","0d5e429feedc7a755fc8666944d8a772"],["/tags/index.html","12a86e9ca5301b4ee80f5c6b7c1944a4"],["/tags/latex/index.html","e5e1a4476ff470ca26d540b521fa379f"],["/tags/中间件/index.html","aa7e987370209d619c365616c6da8cf6"],["/tags/二分查找/index.html","5c98d23c5ae01b74c6e68aee7cd50811"],["/tags/优化类/index.html","b15ce9e77e87d1240057601a3459a998"],["/tags/前端/index.html","51a1ef80099256d199f903960ba4a2aa"],["/tags/前缀和与差分/index.html","f4d05c6101eface91c0aaff636975bb0"],["/tags/动态规划/index.html","5c8fe7915e4177c719c7b757fd33f0c1"],["/tags/动态规划/page/2/index.html","145ff6c927bad1dc21a1eb8ce494645f"],["/tags/博客搭建/index.html","c96dab9415de9cfa97e523f5617d96f1"],["/tags/图论/index.html","e23e6da8d728fb2a8a955c4ebc25817e"],["/tags/大数据/index.html","804ae831a086f2892348749ae65c2be6"],["/tags/大数据/page/2/index.html","0666e6b5ce16432efd6288da74220570"],["/tags/排序/index.html","7a94c759cd3293b4be41c4488a612594"],["/tags/操作系统/index.html","82c1483f771aed832dfd175150b3956e"],["/tags/数学建模/index.html","61f193cd8cc6264f287a93d882945aa2"],["/tags/数据库/index.html","9529d974fae8fb75d45bbfb37700c011"],["/tags/数据结构和算法/index.html","ef9cc134edd150285b3ef5b70033a8f4"],["/tags/数据结构和算法/page/2/index.html","0cc283977ab83d085c0f1315ed552e3a"],["/tags/数据结构和算法/page/3/index.html","353f840b8bd560f1476f20224baaceaf"],["/tags/数据结构和算法/page/4/index.html","43061d58419c5c644b5dbe660a1c38b3"],["/tags/数据结构和算法/page/5/index.html","471f00938e6d3a6746f9b7b9e0c663eb"],["/tags/数组和字符串/index.html","022c790dcdb20ad2ccb3963422d52dfb"],["/tags/数论/index.html","a3faf9399d1b59050c7715db1eaf8a09"],["/tags/枚举类/index.html","04865b4705ad86156e5eaf8715f2fb0d"],["/tags/栈和队列/index.html","4e814ecf6c0eb80fcf075f3e1d66687a"],["/tags/树论/index.html","346d52cf29dea2e14cc7d90492d53cbc"],["/tags/测试/index.html","2fcdbe71fad79c2d7369a72058b0e823"],["/tags/环境/index.html","655230e71119a448bfa1a48843f6f2a1"],["/tags/环境变量/index.html","a7909ca07a7cfb584f2dd558cc8d57ce"],["/tags/绘图/index.html","77caaead7422621b6bfde4b9d5e88171"],["/tags/编程工具/index.html","98db2d1aeebd3af0567d71f55975762c"],["/tags/编程环境/index.html","0a779ac51659a0f9dbf679cd6ce35ecf"],["/tags/网络编程/index.html","6ec45c3bc3d191cd75f0d3f851f09dd4"],["/tags/英语语法/index.html","24025cd78d9bdbd61fc354b25704b424"],["/tags/计算机操作系统/index.html","415aa9f386f26a440ddb4b5ec2312f1c"],["/tags/论文/index.html","ebc463025760cc0de3d81bd51526ad0d"],["/tags/资源下载/index.html","471bd8f447f6dc2391fed351b470c10c"],["/tags/链表/index.html","0f705ed076f898e8a4aee0fc4ef7208a"],["/tags/集合/index.html","6ce5774696ba508e543c85f68a2fa7e6"],["/tags/集群/index.html","65c2fda2c83b1d3cf6231fc4d3c0e218"]];
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
