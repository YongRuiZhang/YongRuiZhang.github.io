/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","7e46171419708747363a672c850529de"],["/about/index.html","c93293fad2434fbeb6e1bc2e7cbc8fd4"],["/archives/2023/01/index.html","3f00bfc49e2c36b6f2c94940e151ee2d"],["/archives/2023/02/index.html","0eec13ed19b976b60421bfb2703feb76"],["/archives/2023/02/page/2/index.html","a211d54bb3bbb40843bf76fbe25ab38e"],["/archives/2023/03/index.html","86e8c219eac271b5dcafdc7ea894461d"],["/archives/2023/05/index.html","c437dfd8db46d822cba5a50813a9e9e6"],["/archives/2023/06/index.html","6ae3f08777a7428e9cfb00dab2283c46"],["/archives/2023/09/index.html","6e4f8c59256544122645e52be975f8c8"],["/archives/2023/11/index.html","67a62e821fc3ed41d6f8ab43878f012f"],["/archives/2023/12/index.html","54a0dfc89a2a4562b358c1354f13194f"],["/archives/2023/index.html","62b0588df2e2dd4bd60af5ab95f48040"],["/archives/2023/page/2/index.html","ca7d8b9bb12c7906d39fb763e5d20f51"],["/archives/2023/page/3/index.html","a98bded83c82a927f7e0bea31f840cd8"],["/archives/2023/page/4/index.html","5496e2da62db721b61f10eb9f1ec9c8c"],["/archives/2024/02/index.html","edf9d67a0298b37b5406e2f908721094"],["/archives/2024/index.html","61cf7e2fe65ed0a4842d225ee615932c"],["/archives/index.html","493a88e3178798cbe4815a2fefbfdba6"],["/archives/page/2/index.html","82f675eaf96ccfa6466993fe126b31d1"],["/archives/page/3/index.html","e2eed4bcfa78e56c201ce3ee4b74507b"],["/archives/page/4/index.html","81a98a6ff92634b6f6a4025d8bda9138"],["/baidu_verify_codeva-qQP2iZOMLX.html","d3c2409aeed18bb11b719512d57ace52"],["/categories/Java/index.html","cc7a05cd1ed96a9d9ec147255bd7097d"],["/categories/Java/后端/index.html","23812d5d180503b069375cef3880a956"],["/categories/Java/基础/index.html","047d14fdf361b5c5ac887f9d62213877"],["/categories/Java/基础/集合/index.html","7b1ded7d3baea2f380005bf4273970f3"],["/categories/Python/index.html","fc4af389463f527d9d028d4bafa307df"],["/categories/Python/编程环境/index.html","6a3e4ee667a000fe24ab3e6e8120b320"],["/categories/R语言/index.html","7db0c1fe913ef978ef6059672750c70b"],["/categories/R语言/编程环境/index.html","13df324f7b53e91a89c87adf82acf6c7"],["/categories/index.html","9cc6a02ea93deb7a924d8260b069a877"],["/categories/中间件/index.html","3cdf8c8dd6b8c477f0795611f6cba259"],["/categories/前端/Vue/index.html","b68e076e631c0f45759f07afc7f0c2d1"],["/categories/前端/index.html","6ebbd490af5c4a48131a4153541a887f"],["/categories/大数据开发/ElasticSearch/index.html","c19f0fbfc77ee6e9ee50ca9fb11c0c1f"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","dc0100757cecfa074849cbe776b078bb"],["/categories/大数据开发/HBase/index.html","6ec6215a0b2574128475c7280ea75e24"],["/categories/大数据开发/HBase/学习笔记/index.html","d51e26b551e5b081e2b6e59b59de17c8"],["/categories/大数据开发/HBase/环境搭建/index.html","da8162342f3e8d09e4fd052a3dab9e0c"],["/categories/大数据开发/Hadoop/index.html","020785717257976e4cd08e6473e955f4"],["/categories/大数据开发/Hadoop/技术/index.html","fbd0a35c6bec760ca2b8d97857a1c115"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1da0c075a21e610be86bd41e467ee766"],["/categories/大数据开发/Redis/index.html","3332af81ffb8235a60f9700f0cdd1523"],["/categories/大数据开发/Redis/技术/index.html","04623b9a678505572ab42b4738e412c8"],["/categories/大数据开发/Redis/环境搭建/index.html","fb2ffa514689fce8b1baf4bfbf75867b"],["/categories/大数据开发/Spark/index.html","e081d0b4150cc21a1f43434c26cbf329"],["/categories/大数据开发/Spark/环境搭建/index.html","7b0de5f687b811643afa12c8ddeceacd"],["/categories/大数据开发/Zookeeper/index.html","0215ebedd8abad46c7fc3bd9deaa7023"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","c59ab23ab1065d2a9a1b03478e4957b6"],["/categories/大数据开发/index.html","21b409941ba36c6004da7b1b57c99401"],["/categories/学校课程/index.html","92662e739b456c119771da85b76c10d3"],["/categories/学校课程/计算机操作系统/index.html","82ccb35cef84024c158fdf2f11b37c1b"],["/categories/操作系统/Linux/index.html","ce40e413de4cc608ae4dd011ae6d1061"],["/categories/操作系统/Mac/index.html","75fb25eb9f0b2d8660b52ca087a88378"],["/categories/操作系统/Windows/index.html","996cac903406308428f141afd9bda48c"],["/categories/操作系统/index.html","2df85fc8aaa6e1abbde604574117c446"],["/categories/数学建模/index.html","bf24a1ebcd654d393e3fa479e4e2468a"],["/categories/数学建模/latex/index.html","71adb491509ba41f9d1f4c0c3763c57e"],["/categories/数学建模/优化类/index.html","988a43f73bd97918629ea35ae404228d"],["/categories/数学建模/优化类/现代优化算法/index.html","dd3e52d7a5dbd95c63df70fdce6f72db"],["/categories/数学建模/优化类/规划类/index.html","c6817d67661f6e1376c936867e48e928"],["/categories/数学建模/绘图/index.html","89277861616f6959238e4115faaced29"],["/categories/数据库/MySQL/index.html","4688d56e52b53188a1d76130133206a7"],["/categories/数据库/index.html","e23beda0cb895c5df525185989b743d8"],["/categories/数据结构和算法/index.html","cb018fd85846c05a08e5ccb3daad7b4d"],["/categories/数据结构和算法/page/2/index.html","488d5377a9389bf4f52e0de48421885f"],["/categories/数据结构和算法/基本原理/bfs/index.html","22b307df42b238b8687fcb44963ea5a2"],["/categories/数据结构和算法/基本原理/dfs/index.html","22e60d7361291ac9a3ec0d040d962880"],["/categories/数据结构和算法/基本原理/index.html","ae8d2f1f96a8067cee2cbe5b21705e76"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4e50c9ccef43bac5d821fd50c0044995"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4c6d0955eda79b09f9e7f43cd16e9dba"],["/categories/数据结构和算法/基本原理/图论/index.html","beee2d095f41ad8041c7c6b717528836"],["/categories/数据结构和算法/基本原理/字符串/index.html","64808af9520648746959ef4d89693301"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","c2d5e89f6e3f1e698dd15df2ced0b51e"],["/categories/数据结构和算法/基本原理/数论/index.html","9062359475474fd9fb6652ed114fb4cc"],["/categories/数据结构和算法/基本原理/树论/index.html","2f5a30a679b7265a8c53ad02b2e3b507"],["/categories/数据结构和算法/基本原理/链表/index.html","ec788c69ea36182045862b6166dce7e0"],["/categories/数据结构和算法/算法题/index.html","3df5603dc3b02309fc0615ff66c106d1"],["/categories/数据结构和算法/算法题/二分查找/index.html","56c372a61b1c54896e791be4fae89d99"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","64e09b4932a379f1a05c4c98ad1a6c1e"],["/categories/数据结构和算法/算法题/动态规划/index.html","551c097b71200357fa040b73fe74fac3"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","fdaf8654912d01f80ecd14e2e62bf3bc"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","008a3d3a5d6e3e920e78f8853080e5f4"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","81790277520367dbc7c4da6be2179462"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","743edad68a00c2e275b43e0f363a3a48"],["/categories/数据结构和算法/算法题/数论/index.html","dd9bea7162cc1f305cdf945892ce85db"],["/categories/数据结构和算法/算法题/栈和队列/index.html","cc48795039379eaf327569b363a52c94"],["/categories/数据结构和算法/算法题/树论/index.html","9530ca4324182b095d2c0ecfba2bfd1b"],["/categories/杂七杂八/index.html","2d58530193e70ec5d327d0ac1cfddfaf"],["/categories/杂七杂八/博客搭建/index.html","8c56f9cfb98a20395b07bebdcefd1d99"],["/categories/编程工具下载/index.html","bf8c26982fea725cc7828219921c6270"],["/categories/编程环境/index.html","30c1cea0c432a44d4d1d176d210c6ca1"],["/categories/编程环境/大数据/index.html","7970c3a8bc912d38cc2e659c2238b420"],["/categories/英语学习/index.html","7c6163020754a8d473c4ed2f654d232a"],["/categories/英语学习/英语语法/index.html","a17ef70a974b3f9b5448599b98b79b97"],["/comments/index.html","ecdd9c7f8fe9fbf0a489c5cb26ee9d19"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","af07d4675bd00830cc0b1269f9cd0995"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","028dbdeaec87029a7f4415bb36ef1676"],["/movies/index.html","8591a12ae5579ba240cf176d7d4fc2a5"],["/music/index.html","ce0f8302d7fb3ca3faa5945ef4e1b767"],["/page/2/index.html","428060fe429e645f9d7abee029349287"],["/page/3/index.html","f59563d68080d66f272871ef0ba93e43"],["/page/4/index.html","848a93c93acbda9827217db3b381b834"],["/page/5/index.html","363476c6daf7654f9035bf71809fbf98"],["/page/6/index.html","8897a1ddc58253aeb8e9ec4bbe17980a"],["/posts/1021360842.html","a9beac0db17fc12bdd1b6f2a864bb458"],["/posts/1120620192.html","6e2a04b02fe681f94e5703fd154a0c04"],["/posts/1141628095.html","94d51678cc31c961be6186962625b840"],["/posts/1168613674.html","ff9a6c9d3713f458e74065d3e621da61"],["/posts/1219920510.html","a3e9b581a4a58aee7e2bdc9f2e36ed17"],["/posts/1222166338.html","1dce875c05789cacedbe73a35f60aeb1"],["/posts/1259097482.html","9fca8eb402429c6276873e9a9f2dd954"],["/posts/1271036369.html","9934ba283e5744f15f28b27daa0158a7"],["/posts/1312847445.html","e7437b702cd7666cace5086b7be8bd34"],["/posts/135355774.html","ca36be3508b8f588481ac1896dc29a1c"],["/posts/1375344716.html","ab2aae9021f84c86486488aa3e8bd899"],["/posts/1388991698.html","96eb0f3a0a694e4d685582b3ec6c752c"],["/posts/1410315814.html","d64ccb489ddab59bdd8c1b903157cd73"],["/posts/1452790229.html","cc4389617b8d0764700fc19d437df361"],["/posts/1470079884.html","2c1e189c3723b968d08e3f486f4da1c2"],["/posts/1470079885.html","8109b2a31eb093deb03c7b303076bad1"],["/posts/1470079886.html","ac0d8af2981f256b468d556f0280b35d"],["/posts/1470079887.html","c3e0bb842ae890979625e1ac17ab544b"],["/posts/1498536549.html","5411ab166c50eac93330c51f52872275"],["/posts/1539568593.html","8279c4b5af79c5c623ab4f213d28c787"],["/posts/1547067935.html","c87f456177e7400b3585c6925766d6ac"],["/posts/1557866301.html","2987014b3e79bdab84a6efd91e5ff690"],["/posts/1571776361.html","373227d9adc32a7312b31f353cf7352b"],["/posts/1605124548.html","74944ee3a68066926e98397a6fd32fac"],["/posts/1633036852.html","8f3fb28fcd8d75a02333f9945e5004c8"],["/posts/1674202625.html","6ed3e6b05ccd4032b082598c9d3e8701"],["/posts/1765123828.html","88c304e925d69cde651662a93a576a97"],["/posts/1767336200.html","05eed9a71e21d8a4f34830dc249d19be"],["/posts/1776114197.html","a34279feaa96e1f2d6424d47f85c25b0"],["/posts/1817748743.html","96d4461ca24f252bb3577ef61f6953d8"],["/posts/1925125395.html","31ba5a5f73bb1d3caad009c3d49d8a4d"],["/posts/1966191251.html","33516d1f5656d1fb2aaab50077495485"],["/posts/1987617322.html","1b5b02ca6177efe923d9580af4ae8b23"],["/posts/1999788039.html","f6b60278db867a072807eb45c97871a7"],["/posts/2075104059.html","a2375e9b0d0f4cad3609eb02c56e79a7"],["/posts/2087796737.html","728547673dde321eee10c275e25be63c"],["/posts/2106547339.html","7be0b1fd16fe405889d711b7802b3629"],["/posts/2207806286.html","1e027c09a4b62b15583d4a7a90edd24e"],["/posts/2225903441.html","92939cc2bb7cf7c7f54d2f397738fb0a"],["/posts/2265610284.html","c6f6ae3e05d6647a020c3e1971ccd5b2"],["/posts/2281352001.html","bd9194a32193183e5f088a3d3321159c"],["/posts/2364755265.html","c140c1bb4f2be5932be5818dd9f85abb"],["/posts/2414116852.html","ef26f4b61c1f21d3113f406f851df8ea"],["/posts/2421785022.html","c5ada28718b595121504378e677b8a0d"],["/posts/2482902029.html","745b6b38417c88fb5dd0bb00f3f20d68"],["/posts/2495386210.html","efe40a2adc768d49b6f9fd3f8c7d460e"],["/posts/2516528882.html","a08a9b7e976a9748da466938d33ab75a"],["/posts/2526659543.html","5c673d55a69ac5de9b06c5a8e62adcc7"],["/posts/2529807823.html","34de451a75ab5a15baa4267f44c11f69"],["/posts/2596601004.html","01b8324c34a03477f890a8d088fb7083"],["/posts/2697614349.html","e837d2cd77b4a05df7dc42c37c5ac749"],["/posts/2742438348.html","e255bde4d8c772d3aae9281e58fe476e"],["/posts/2768249503.html","325bdbed6a937dbb4d948037feeb9070"],["/posts/2864584994.html","067502160936123014fa46758312ede5"],["/posts/2888309600.html","5b61282f5dd4bef556cc8b2beb4d25aa"],["/posts/2891591958.html","693d5c239b8d8cdcae6ee6c8fbaafb87"],["/posts/2909934084.html","1c0216de0583a02681c6dc9df9c17688"],["/posts/2920256992.html","69ce745d4ff563ef837ac5ac7bece734"],["/posts/2959474469.html","0532bc0b4712f4c3c40e75717289dfb8"],["/posts/3005926051.html","a60843c405cd00ce95dcb392006503ac"],["/posts/309775400.html","58484935e69374c7291165ff2179b132"],["/posts/3156194925.html","4ea0fcabe6124298698db1899e8ba093"],["/posts/3169224211.html","e0d8afebbbe25a3cdba5ef82deb4cf9f"],["/posts/3213899550.html","8c922891056c7fc999216470cc449a90"],["/posts/3259212833.html","e6bdff6ff235ad43d34dc935d1644323"],["/posts/3266130344.html","a6c09752cd84d73181e27a8da887e347"],["/posts/3292663995.html","0d497d0c2aba0548c48c3b4efcadcb98"],["/posts/3297135020.html","e5b5df9bb654c3f763b0e5b1781a0dd9"],["/posts/3306641566.html","5d8b1cf5c48a8d30bb02b3fd0effa25f"],["/posts/3312011324.html","568b5a816bd90a5e712f4fd08ad77baa"],["/posts/336911618.html","112889987fa2431479e410338e37485d"],["/posts/3402121571.html","b1009fbfc919dce951cadd26caa05130"],["/posts/3405577485.html","8923626cf76aa86f87d1f5f9f5befe6f"],["/posts/3498516849.html","12145f31c9016627861ce06936f0f39b"],["/posts/3513711414.html","714c7564cec140f7d3011c2a6bd4d2fc"],["/posts/3523095624.html","a18a32e6985b72c22e5c496e0839a635"],["/posts/3546711884.html","93e55e138c5710b450b8983daf14b777"],["/posts/3731385230.html","c2d0b4c8aaa023bd1cca60efb3c29384"],["/posts/3772089482.html","603b599be080cb58537d42d8ca7ef264"],["/posts/386609427.html","548084c00f83d1d11e7afa1f68649e78"],["/posts/4044235327.html","96dc8db32caf6905d80a4f6531dfcbfd"],["/posts/4115971639.html","16b81b25190627bf20fb005865874732"],["/posts/4130790367.html","168fbf7e1ad54127ecd13462d626e916"],["/posts/4131986683.html","8625b38c1491087fde435c3a6350bf1d"],["/posts/4177218757.html","ccb0a41bee29eef05929c805ce381c9c"],["/posts/4192183953.html","b6fe793eadc54c1dc7141de785f4db9f"],["/posts/4261103898.html","b0c3f9f7ea56425642d80ddb058afcee"],["/posts/469711973.html","1e0c47232f12204a6658b06051cd2c99"],["/posts/482495853.html","3096d63b734dc7cd0b5c4b4e8e888d6e"],["/posts/488247922.html","4f673b2b6273662c6fab9e51875642a9"],["/posts/517302816.html","d266ae132bbd53b2afce7caca346bc23"],["/posts/570165348.html","c9c3b34dcddab5c4c2811c6159a29e99"],["/posts/595890772.html","949715b469c0a3f698e5934581b81dbe"],["/posts/67485572.html","f75111cb3f1bce301f547c08af730755"],["/posts/694347442.html","56b7d465640be45b8a901d4dbd57ea63"],["/posts/707384687.html","56d60986ce1032352f82b0c53f9997a4"],["/posts/71180092.html","5f48be5b84ee804d832bc3890008a2c6"],["/posts/716459272.html","c72d4b85b0801e2ac31c11d89d0b4b81"],["/posts/765481613.html","7b2adf6a26003d0120b39d44a8e50e54"],["/posts/778231993.html","8869398fac927cf8ff04be8bd5b9b544"],["/posts/795397410.html","5b2d1908ded51f34d7f78d972ee727ea"],["/posts/820223701.html","100da06638c483bea85b23af26b5c2bb"],["/posts/830372185.html","8e90ab30c55aacab2dc250c98dea7892"],["/posts/88294277.html","8a6647546c262248d3536553de715fa5"],["/posts/939963535.html","f03baea5e0e214f939618e8c5a70a282"],["/posts/983786067.html","3ae770d4915ffd3b90bda212b82eb0df"],["/sw-register.js","9ab3ba7e89cdc6e9d8906e3f3b7c1511"],["/tags/C/index.html","69d88cb48ce4c9b8514a22dee872e4b2"],["/tags/C/page/2/index.html","45d35cb8db4212dcccfbc7239f360141"],["/tags/C/page/3/index.html","08690db112b7725923547b80471530d2"],["/tags/C/page/4/index.html","39798c0504bb36c60049a7f213f07b8b"],["/tags/ETL/index.html","476414bcfae4aab7a2b3ff8cbee0b7d6"],["/tags/ElasticSearch/index.html","52519e30883b5f2d3ec47ca07ff2722e"],["/tags/GUI/index.html","a29b69147ab6a584eb80f51e5d8c3f02"],["/tags/HBase/index.html","f53e4228fa05c600752a6088240d5635"],["/tags/Hadoop/index.html","9b9459968533850a2db58d02e1196560"],["/tags/Hadoop/page/2/index.html","6623a6e14bcfb0f105f5b3437dc2456c"],["/tags/Java/index.html","88d234b0605214672cc495382d75d131"],["/tags/Java后端/index.html","bad8ffa6012642961b4d9fd3bc18c999"],["/tags/Java后端/page/2/index.html","190d9f784ba1322c9bd4983ebcdae317"],["/tags/Java基础/index.html","39513c8e0e12ad560a990c53fae9b87b"],["/tags/Java基础/page/2/index.html","c73a106cd49aa20f9473938e311667b0"],["/tags/Kettle/index.html","19bfb75f921dd45cd48685695a847b59"],["/tags/Kibana/index.html","98f9886c88eb6c6303587beb5cc8cb2a"],["/tags/Linux/index.html","c237ed60cd2aa6f0ed932de6715ffad0"],["/tags/Linux/page/2/index.html","f2008f3d32a93740c803e3076bba59cf"],["/tags/Linux/page/3/index.html","87e42effbe181a55f129fa0f24bd97ad"],["/tags/Mac/index.html","ca62c057afc964462dbe3e0fc49f5c3e"],["/tags/Mac/page/2/index.html","710aba5dae15a1c4b4807b793bace6b5"],["/tags/Maven/index.html","3ae76c2a324af4b894e587a50ec6308b"],["/tags/MySQL/index.html","fc1e52c97dba540ffbb282cf3afd8f8b"],["/tags/Python/index.html","fb1e406a679775fbcb0f7ffa29926c7a"],["/tags/Redis/index.html","1a9ca9bf7b0c51648acec3db122fee95"],["/tags/R语言/index.html","3e602312829bc41fba6d6a4db6ff3e64"],["/tags/Spark/index.html","a4d3e1c363ee5bcc128713af5e60c0d6"],["/tags/Ubuntu/index.html","811a63dd81f5439e7e4f3d752b425620"],["/tags/Vue/index.html","b9f97036abdbb7ca7cfd89c6d711b6fb"],["/tags/Windows/index.html","de79452034dd59ab4d1497f9abd76fc9"],["/tags/ZooKeeper/index.html","ea789409be258d6703d3ae8a09e620c6"],["/tags/bfs/index.html","41cb4c6ce262fd36f8d17e64bcfda961"],["/tags/dfs/index.html","ab1ac1bf8eb2b29d52c215b1cca22a80"],["/tags/folium/index.html","006640da8e3f58ed71c34a1749631f1e"],["/tags/git/index.html","5b75bc0355447e11a867d1d406304a56"],["/tags/index.html","9425160c0d9c144aa3a756283bcf3152"],["/tags/latex/index.html","7ff4468db377af668d384baef2cdec17"],["/tags/中间件/index.html","5046fd48fbe5f6204567b17d27e1d8a5"],["/tags/二分查找/index.html","abc24d35ca641d71cf644f4daf2daed2"],["/tags/优化类/index.html","6dbf1787330660ec700a3b876d03bc1e"],["/tags/前端/index.html","b97eb891e08dc2465ca8612317bed971"],["/tags/前缀和与差分/index.html","1520595c94e6eadcb1f29f70e6463a4b"],["/tags/动态规划/index.html","b5b9d6588e0a5a3435ebc6eb586a4b77"],["/tags/动态规划/page/2/index.html","88aab548e2fb11b4b24102613e9d4996"],["/tags/博客搭建/index.html","cbc78d06c1ed137574f4c70c6158fc82"],["/tags/图论/index.html","dbc13df52839106abf418818a64b7146"],["/tags/大数据/index.html","56f689212a74bdd88ba0d2faa2618ab9"],["/tags/大数据/page/2/index.html","822c1c12b580326962598bb81d582159"],["/tags/操作系统/index.html","465577eb465e1064c62a1ce1e4242eb8"],["/tags/数学建模/index.html","6d7934e3987bd7df6d161d6049787141"],["/tags/数据库/index.html","715164ecee21f0a1236ba554aa869076"],["/tags/数据结构和算法/index.html","4386619dddd0a57bd239aa0cda3d408b"],["/tags/数据结构和算法/page/2/index.html","1700f4cdcd864814f9571231eb11fd8e"],["/tags/数据结构和算法/page/3/index.html","9d1cc47b539a33de73deb50c23181859"],["/tags/数据结构和算法/page/4/index.html","18c9d303af6411efcddd488bf6599c53"],["/tags/数组和字符串/index.html","d1d38c6c0b2ad650bbe61d254061a88b"],["/tags/数论/index.html","68d1c43a4c93ff74a246900cae4da5b0"],["/tags/枚举类/index.html","ff9e8c10b92c9f3d872653fd49c2dd64"],["/tags/栈和队列/index.html","00ab257c8bf5e669d9e5589258f95f35"],["/tags/树论/index.html","ffe516356b3282824ac59f050e3a0174"],["/tags/测试/index.html","27835bbc5026b7dcff1b931299eb2b89"],["/tags/环境/index.html","d5c329e008beefce93f3da71a9c76821"],["/tags/环境变量/index.html","a6cf2b68ae3052272c22a97d0c031306"],["/tags/绘图/index.html","7fcc3897fbaf4fb55648d52c754bbfc4"],["/tags/编程工具/index.html","16f7bdd3232edeb9e5a4bbfeac5e6033"],["/tags/编程环境/index.html","15338f24f0c6108991ffe608c2f51219"],["/tags/网络编程/index.html","5242e13094790239f99b7ebea2cbf512"],["/tags/英语语法/index.html","c74c3432374cebdce494da164eda90d0"],["/tags/计算机操作系统/index.html","93c7d90c0e7b7bfd0a62b63d1d39a231"],["/tags/论文/index.html","952f2ff8cd1ebc25f20fe095ee19f7fb"],["/tags/资源下载/index.html","d0505316c921a977c47673703c9cb705"],["/tags/链表/index.html","8b63baccfba229b291e5b69a6479d4fc"],["/tags/集合/index.html","dea3b5b0eb7401c78ca65e9977cdfdac"],["/tags/集群/index.html","0a465b7ac67ca519cb4ffcf60c53d223"]];
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
