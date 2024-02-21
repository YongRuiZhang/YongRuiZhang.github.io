/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","b979f83a9e07144cb1259ef3559c1efc"],["/about/index.html","651253cfe5aa2d102c965af552af2cc9"],["/archives/2023/01/index.html","fdb9503eecabb71c2a297a86f301429e"],["/archives/2023/02/index.html","21d54044d39b5f6d970cf78076e8b4d8"],["/archives/2023/02/page/2/index.html","aef3665ce44de4a80f268962c6de0d4d"],["/archives/2023/03/index.html","3d1d4499b6bfc92c16507a342ec7729c"],["/archives/2023/05/index.html","4337bcd133722f710c9306b87689ad04"],["/archives/2023/06/index.html","2c612561de1cdda1a86401b08a0583eb"],["/archives/2023/09/index.html","fac45c5e8cec4584db437f679e1d17a9"],["/archives/2023/11/index.html","eaba083fcff8dee031b831d669e3bd97"],["/archives/2023/12/index.html","0dbd28dd8163762e9a749a208c462086"],["/archives/2023/index.html","80f34811e84b4f947e9ef2f6da4ca8f8"],["/archives/2023/page/2/index.html","db6be797af7ea922d4c95f09009aebcd"],["/archives/2023/page/3/index.html","5b982f5db6dd1265c6c96211584652b1"],["/archives/2023/page/4/index.html","ac83673c851556c5882df74c19392c5c"],["/archives/2024/02/index.html","67cdfc799a68776ad3bcdbf4d716256d"],["/archives/2024/index.html","1d87ac945fd72c4f472eee99e86bde2d"],["/archives/index.html","91aea23950b043a308b5d35ae0dafaf7"],["/archives/page/2/index.html","782758655fc8f27d3346103f450ab6d3"],["/archives/page/3/index.html","3ecf15ae4f364c243b91e194d89ce718"],["/archives/page/4/index.html","53478b4a03f76d2f9752a6edad3cd2bf"],["/baidu_verify_codeva-qQP2iZOMLX.html","5779898e6076e1319548b4d92d782f40"],["/categories/Java/index.html","9ec9b307f1a3dfbcc5f28883a45e8f03"],["/categories/Java/后端/index.html","78b9ef78f35c48b4fd44a51855aba76c"],["/categories/Java/基础/index.html","c4f4b74bab69a25b7c19b261c36a528a"],["/categories/Java/基础/集合/index.html","3dc2cffe4907afb8032a45f66e18c0fb"],["/categories/Python/index.html","18f40bfa9d82960603bf4c0d9abe54c3"],["/categories/Python/编程环境/index.html","ab1b83a66f758012101901e8ed4412df"],["/categories/R语言/index.html","e6ad442a76d3f3abb11780595e60d7df"],["/categories/R语言/编程环境/index.html","239b75314376756e45c0b8c7ed368b06"],["/categories/index.html","200ab77c00171a80497ab8a168c60624"],["/categories/中间件/index.html","0780d53d692d4149ada87ec4dc9d5412"],["/categories/前端/Vue/index.html","e73eae4e2dcc6e1af9a005ce059aae80"],["/categories/前端/index.html","d54a430695671e337c45dea3bcf4f633"],["/categories/大数据开发/ElasticSearch/index.html","2bfbf2f829b978ad452c5d38e600b0b4"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d557a82b522afa4a625e72d01e32ddbe"],["/categories/大数据开发/HBase/index.html","f97f96ad18fd3718c77a0b56e1ba5d85"],["/categories/大数据开发/HBase/学习笔记/index.html","60e87c92dd4ebafe202ebf14e4cb4538"],["/categories/大数据开发/HBase/环境搭建/index.html","d98b5986e99281c5f9fcfa010abf1e83"],["/categories/大数据开发/Hadoop/index.html","35ef4d8a7c41d25e793ae6538b0abb8f"],["/categories/大数据开发/Hadoop/技术/index.html","bcda371276c8972a80f9de883fe75ad8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","c207008962a59b7b8dde2dc3080b4a2a"],["/categories/大数据开发/Redis/index.html","3c4ec80d09dfe1d42d72abb1072e6957"],["/categories/大数据开发/Redis/技术/index.html","28c8fe6eacdc4aba6b899ad4a7d8023d"],["/categories/大数据开发/Redis/环境搭建/index.html","b8c1aa94c7a786ca5c0d578c994b1284"],["/categories/大数据开发/Spark/index.html","c1770cfcdc25b83b49e6c61d67cb95a5"],["/categories/大数据开发/Spark/环境搭建/index.html","ff8efbcbb76dc6720caa371d580a6cf5"],["/categories/大数据开发/Zookeeper/index.html","fd0a04fd83f5c455b29881d6bd45a7e1"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","350dc926046daa479cf9ad3497d8846a"],["/categories/大数据开发/index.html","313137674103d7b97dfaec5957459f63"],["/categories/学校课程/index.html","6164ea423822bf5c343d50ddc0542b73"],["/categories/学校课程/计算机操作系统/index.html","3ec5fb7c2affe0428195884411e71103"],["/categories/操作系统/Linux/index.html","3d318ab4dc37ccd18933837de8bc1fe4"],["/categories/操作系统/Mac/index.html","b770d9c26770e00cc9e09eb3fc06506c"],["/categories/操作系统/Windows/index.html","ea11ad7ddf4baa1b7a2d162d1320d400"],["/categories/操作系统/index.html","ddcb05590fef632436610744b3e89583"],["/categories/数学建模/index.html","2413f0f61ef869b10ee8853f0be64b7b"],["/categories/数学建模/latex/index.html","3c53120434147603b9019e68c72cb5ec"],["/categories/数学建模/优化类/index.html","4b5298093500a2276cb47e5c26ba431b"],["/categories/数学建模/优化类/现代优化算法/index.html","fd824fda38e22f61ba0c4b4de06b3aa3"],["/categories/数学建模/优化类/规划类/index.html","f6d7fcf2260fc0054ba123591247c0d2"],["/categories/数学建模/绘图/index.html","3ee56316233935d8063a3828d0b441f5"],["/categories/数据库/MySQL/index.html","a10ecd15357889fb92ff11247bc5952b"],["/categories/数据库/index.html","570069214cdcc59fd325b0a67896120a"],["/categories/数据结构和算法/index.html","223ff4b6bbc92d077d9691ca27c1a80d"],["/categories/数据结构和算法/page/2/index.html","1ad25d37dc605f8c410f69b57ea8ddaf"],["/categories/数据结构和算法/基本原理/bfs/index.html","e49ac55ae30c00f870c4d87fefe1d2c5"],["/categories/数据结构和算法/基本原理/dfs/index.html","beae8a09db85f0457651bc1ba5c3c14d"],["/categories/数据结构和算法/基本原理/index.html","f75d5f6007b7f50a19184c077756c460"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9b12a5584f9e1e422903a9a7b9dbeb5c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","41f373e47f92b84d6610366ebf6570cc"],["/categories/数据结构和算法/基本原理/图论/index.html","f874c5bb0bc1932ec13b83cb9613a0e7"],["/categories/数据结构和算法/基本原理/字符串/index.html","5c3c04c309f85244d9c4819990fe2b5a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b7dd87cd7db13b53a203760f91db9ce0"],["/categories/数据结构和算法/基本原理/数论/index.html","8b9a6be2ae9824aba44bf0aacd28cc1d"],["/categories/数据结构和算法/基本原理/树论/index.html","a54f783727037e0af7e324dc4dd53239"],["/categories/数据结构和算法/基本原理/链表/index.html","283a6e17eed0095801bab2a34c824900"],["/categories/数据结构和算法/算法题/index.html","9a2144ec78fa6155108f4bb6ce062d8e"],["/categories/数据结构和算法/算法题/二分查找/index.html","64bab33d524d310a4fde829ab4f35355"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","e1ed0bf4493008bce4eba063a0b32bb1"],["/categories/数据结构和算法/算法题/动态规划/index.html","24442278ea7ce4fafbe3abe06afb962e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","97b05926d076fa5bdb3b247efb4f99f1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2697deb88fe90b82377cba88ee8be0df"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","3e2d7bef5dafe3049970db17bf6bbb6a"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","a7cc9566e72dfcdb862d005a24bd71fe"],["/categories/数据结构和算法/算法题/数论/index.html","e791feafd24c257e822ba50d520472d2"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f29c2430dffb83b3d7707e9604ff85d4"],["/categories/数据结构和算法/算法题/树论/index.html","518fec136a5ec4b6ccab27c2f1eb7793"],["/categories/杂七杂八/index.html","d52365eb441ee424f35b8ed147c370dd"],["/categories/杂七杂八/博客搭建/index.html","5a13775065d53d72039575e2aa05822c"],["/categories/编程工具下载/index.html","a6bb28148c2f019767c2897c642c4062"],["/categories/编程环境/index.html","8078ac83a869b7474bbd45f041c7fb3c"],["/categories/编程环境/大数据/index.html","f20012157001664da12e4fb45af41192"],["/categories/英语学习/index.html","9636536d45edcb836fe2eec0516e6549"],["/categories/英语学习/英语语法/index.html","0453c4a0f6cec20d98b44660c12b7995"],["/comments/index.html","cd545b1ecfff65959d9d1735d8b4a7d2"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","db86f300e26473b7825823607fa0647a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","62d9148555397074c7e66f767c0d885b"],["/movies/index.html","a9798b08a38ee250f4e145be11d791a6"],["/music/index.html","7b715a259199089579a8f2a840e61dfc"],["/page/2/index.html","3053dc2339773f3a2872f0569726f4c7"],["/page/3/index.html","08c84a1085c41743c7f41322922a008a"],["/page/4/index.html","dc09171a06e85200597fb0c85fe1d84e"],["/page/5/index.html","d8ec640247c1368f8cf87a3ba61a2803"],["/page/6/index.html","a1de27d3f7c8bb067fd83d6d5202e783"],["/posts/1021360842.html","5ef3d32904d2afe212c4629b864e7f12"],["/posts/1120620192.html","83db088a607ab755784f16ec83fe0132"],["/posts/1141628095.html","02e31d94ebea365069621d6f380ae4ed"],["/posts/1168613674.html","e792036bae444e0dcc6ad2865b560d80"],["/posts/1219920510.html","87c91675d7df4020eab206b0d548fe01"],["/posts/1222166338.html","bcab64c68266af5ff3976636843e394e"],["/posts/1259097482.html","1b065a6f59784686f9d2496252bbf837"],["/posts/1271036369.html","c00ef0aac1c8873d909ac483657c3784"],["/posts/1312847445.html","53d124fb0ede111b50be295625663dbc"],["/posts/135355774.html","681a9645877303f748a1787a2cdda39d"],["/posts/1375344716.html","b6461e802da4bd29e5c5be6c4a4f79ab"],["/posts/1388991698.html","e6d7c774e96220ec18c9e82cf2ac7ef3"],["/posts/1410315814.html","8281c41c86bdd3ecf8096c12941f4051"],["/posts/1452790229.html","081da84b00ae572764289a5c14848311"],["/posts/1470079884.html","ba7b16d5e5ad5e1d2af4e9951b9b9b34"],["/posts/1470079885.html","9f6a21f06b6a60b4f249bcbcb60466bb"],["/posts/1470079886.html","1d8a9fef83437429a0d92eac2aa16761"],["/posts/1470079887.html","66fcc501bd1bcffbd06a3f86120d5acf"],["/posts/1498536549.html","14d86376abd5189389307b0023f627fd"],["/posts/1539568593.html","c67860a730bf97b8ed588bdf730f1f07"],["/posts/1547067935.html","77a0e7f88482de72dc5b8e54ac3af2ef"],["/posts/1557866301.html","33ec6c9f1d172ab32be02e1ba7bb3125"],["/posts/1571776361.html","363b82112df44558c66c4f51ba217174"],["/posts/1605124548.html","429f219d023c077eaebb504d1a94c3e0"],["/posts/1633036852.html","56e1c84fab34a4fa48288fb2b0cb6eec"],["/posts/1674202625.html","8a59172a82686bfa97e2c7b1bec3a815"],["/posts/1765123828.html","725a62cb0a82aad83a8d6bf227e7a064"],["/posts/1767336200.html","3e45a7aaa8b92e88269e8117c0fcafb2"],["/posts/1776114197.html","8ae656d5b4f487bf321c62ea9caee9ae"],["/posts/1817748743.html","f63e5765a333bbdde789eb91d5a445e7"],["/posts/1925125395.html","e880bc98a32e25f341918e1dba666b15"],["/posts/1966191251.html","e94bddca9237c22dc1604ec2259a019e"],["/posts/1987617322.html","997f696bbd812cb39ab668c5a8a6082e"],["/posts/1999788039.html","4f4c85a4792f7167663bb7c3f5683b15"],["/posts/2075104059.html","1aac92d969babc45c40e607ed804cae1"],["/posts/2087796737.html","c74817b71249cf8ef4af58976ab8aafc"],["/posts/2106547339.html","901900e97bc84298715501d846166b6e"],["/posts/2207806286.html","ef8285ddeaf658ad6e57338eae23b0d3"],["/posts/2225903441.html","479bfa6e4af52be1ba416cfa47b4f271"],["/posts/2265610284.html","27b5d7b66ca2a68970035075d1908fd3"],["/posts/2281352001.html","dc874c8b6e0dcf35750df47e78951830"],["/posts/2364755265.html","2224e8f3ff2ce3b9e313e69161b38ce5"],["/posts/2414116852.html","f81591bb487fd0c129df75ab7b27ce70"],["/posts/2421785022.html","b1675cc00b4db004d5cdd9a0082a1f3d"],["/posts/2482902029.html","2d6572267c1a15238f5f22b962e51652"],["/posts/2495386210.html","ebd478c9ea01be80eddd5df3cdfc028b"],["/posts/2516528882.html","5f7c47fb508988ab56f3344e67315fb0"],["/posts/2526659543.html","1f9a7b8ae5b211f6a2fa4dee0edaf766"],["/posts/2529807823.html","e3ffe3c366f75ae5757ba625c1c4b25e"],["/posts/2596601004.html","036b0bb42440d83888d9faefbbb77274"],["/posts/2697614349.html","dccac1171adbe2b5c6aac54da437a000"],["/posts/2742438348.html","67a0ac9954c52b0d057c609acbf54a05"],["/posts/2768249503.html","8baa37f83a13aabd1cd2851d7b02294b"],["/posts/2864584994.html","e14ff6c878112e81772a0f10782f3df9"],["/posts/2888309600.html","baa3485b1f0b181f3ec92e562a3e86ec"],["/posts/2891591958.html","293ddb8e596b30742fcd33f16231d3d3"],["/posts/2909934084.html","0a4466c18d3b6ea8af0a3b3d3d485bb1"],["/posts/2920256992.html","702c55058bb0d15a05ac49e8ac9b1e9e"],["/posts/2959474469.html","4769b8a3a04b27f671b504020b471b55"],["/posts/3005926051.html","1d0431e49c5adf57543496721a2b0d5a"],["/posts/309775400.html","831b9c5151ed5ca7d1a524b5180a7f90"],["/posts/3156194925.html","5345063ed4582b5585d58c800958787f"],["/posts/3169224211.html","9a9dd17ba8db863aed452a34a2b8fca5"],["/posts/3213899550.html","333edfbcbe3c119fc3cf1c22c5e46725"],["/posts/3259212833.html","c3d36d92c326ced05e85c1898098891b"],["/posts/3266130344.html","61c8b5615809f462e2335b842212e450"],["/posts/3292663995.html","7fe66fcf9df9fc4388dfb712b18c756c"],["/posts/3297135020.html","682bd93383a7e3f5b5269c4333d51f07"],["/posts/3306641566.html","813513c7d202154ea7393147887fea62"],["/posts/3312011324.html","d91f7e055d3817d663d7225581e1a94c"],["/posts/336911618.html","7cf59d434a3253d06770c6949274716f"],["/posts/3402121571.html","d1c24dbc1878bddf5c82f37ddd6e6d65"],["/posts/3405577485.html","b179fdccb8d1f30270d4cd6f0d229a4b"],["/posts/3498516849.html","904a4c50026273c307933fd22f2eaf34"],["/posts/3513711414.html","309c0c55be65ffcdf5640c1f356f699e"],["/posts/3523095624.html","84793eb42fe5925608ee73b243d84552"],["/posts/3546711884.html","a2468217040f397e73871bab9abdc8a5"],["/posts/3731385230.html","d77ee134c02328e87d2778c72dacb8d0"],["/posts/3772089482.html","48f8d8629f3861309d5a939cce84764e"],["/posts/386609427.html","eac96a266ac4266e1813ccdf23cf0613"],["/posts/4044235327.html","5bc8f8b6b76182f3c87d9745ab02561b"],["/posts/4115971639.html","645ce7265c9dc1ffeea9778371cc6ed1"],["/posts/4130790367.html","f55b0c5065b89e8aa5325c9c0c038727"],["/posts/4131986683.html","5fc5a7a7d5afd6e79b93ee79756634b8"],["/posts/4177218757.html","fceffebe332b8313c29c3047346ab57a"],["/posts/4192183953.html","8f81d72c9299c9c9119a38a263fe42aa"],["/posts/4261103898.html","aedaabfbd6dcfd131072676740340ba2"],["/posts/469711973.html","d03085b8e53b09a70ef92e54d913b296"],["/posts/482495853.html","cb9532388180382d5995e9d35e4cf495"],["/posts/488247922.html","4285b91c2f8118407830920a6817303d"],["/posts/517302816.html","7e9bd96728a34b6bcf7b0afdcf9ccab2"],["/posts/570165348.html","61e731e95430f34a66d598e48c0ebae4"],["/posts/595890772.html","4f008cdefc35b68f10c14b667fa6b66a"],["/posts/67485572.html","f0aaab60c86ea94bbcb87c27d55f8996"],["/posts/694347442.html","2e03864144e5d8b55fe812d398653f3b"],["/posts/707384687.html","cb76febd3988a64f2fe834194f8396ed"],["/posts/71180092.html","10e0690cacc1195e9bc9918753dbcf57"],["/posts/716459272.html","7bc8920b19582d46d3c2bf7592edab98"],["/posts/765481613.html","5e41859c20574ac887387bbadd5b30b5"],["/posts/778231993.html","d5f644b935c302fe4c120388e62dff5b"],["/posts/795397410.html","4f9e485812823317ef3f727ccb785e1a"],["/posts/820223701.html","43ab330797f458daad6ccb423f6888e3"],["/posts/830372185.html","0666b7e814c2046eb2b92bab84d3ec2f"],["/posts/88294277.html","507197920adb89682a86600580713ec6"],["/posts/939963535.html","ea008f04f237549d20b519acba2b9c46"],["/posts/983786067.html","83e0dc8bdd06972a65a38a426cb600a0"],["/sw-register.js","685fa9fc3ed3f1728908744177ef133e"],["/tags/C/index.html","6122dd5d0b972432cc33df207e6dc4d9"],["/tags/C/page/2/index.html","e7d96bb1722b817bff13778817739830"],["/tags/C/page/3/index.html","d6f9c92d006d4eb587a727233b58cddc"],["/tags/C/page/4/index.html","b410c8add900f91904e8899ec77770c3"],["/tags/ETL/index.html","b1b7c3ce07b393b0ee63ccfec0d63863"],["/tags/ElasticSearch/index.html","19857420c73f9d3d54953c80e0a908dd"],["/tags/GUI/index.html","f77b9b6889eb2470ba796f35d84939c8"],["/tags/HBase/index.html","a3c401c56f40aaf16849eac8645df916"],["/tags/Hadoop/index.html","ac5fa73c28c886ab1c27750b4b4f6b22"],["/tags/Hadoop/page/2/index.html","88ffeaa81cd274671a237621772e679c"],["/tags/Java/index.html","61d01d83dcbd4d6983d46a5516d2a545"],["/tags/Java后端/index.html","c629959b9a58db54515968173d733fde"],["/tags/Java后端/page/2/index.html","82b714fe26d1dad3e848bbae76faf72f"],["/tags/Java基础/index.html","9c94769ef6aabdd82b42e2b01d68c51e"],["/tags/Java基础/page/2/index.html","8090c32466e8f89ec852e8f7bf5b3bdb"],["/tags/Kettle/index.html","8dbbe226435819c8edd13edcd26bcc82"],["/tags/Kibana/index.html","4a7d6f08a66dd3f358646d85fbab5147"],["/tags/Linux/index.html","4ba40a6d30c304e84c2c2c1b440de8dd"],["/tags/Linux/page/2/index.html","1ea465e47ff285cfe16c0e647151f635"],["/tags/Linux/page/3/index.html","06c56a4372d94582b7f1e5e6f6118bb7"],["/tags/Mac/index.html","e37fb45e2af3927bf74759451d24598f"],["/tags/Mac/page/2/index.html","078b683d67f62a49166465f3e693dc83"],["/tags/Maven/index.html","533d245c77c95d3e8dc1ebe1d4d3ba38"],["/tags/MySQL/index.html","f18698bb9bebb1fa53fb4ae5eae76ff6"],["/tags/Python/index.html","cee0c7b8be775ef190e032ad73c5981c"],["/tags/Redis/index.html","b31a86ae3e44e97365b2317ca308b352"],["/tags/R语言/index.html","ca0bf3bb78b6d5765b6c88467525e824"],["/tags/Spark/index.html","0eaa7f83686936a9fed3293ae5439d2d"],["/tags/Ubuntu/index.html","b3e69022b2ec216e6bfdc18ebcc1ae86"],["/tags/Vue/index.html","27c92e4d31abd38eb7b07228275002bd"],["/tags/Windows/index.html","feceb618c733a3a194da3019ad6797c3"],["/tags/ZooKeeper/index.html","d43967ccc44b283c817a84c158c4f51e"],["/tags/bfs/index.html","6c4dd80d1e4e3753905f4f6dd33c7400"],["/tags/dfs/index.html","7b9828de2fb8147d52c5847af2c14f5a"],["/tags/folium/index.html","d992658b5a4c981b4bc2872565d38ae7"],["/tags/git/index.html","b09bafff561abecf19e19172f5aac2e0"],["/tags/index.html","0d4891264c4a26713ac1f035c94460fa"],["/tags/latex/index.html","778a3bf11c369f8efe21d171dcdb7890"],["/tags/中间件/index.html","fd2f395978f112cf42f4c07d85936d45"],["/tags/二分查找/index.html","1234165d28b1f2e506614e5c33889224"],["/tags/优化类/index.html","3d2bcfaf1ef5ef5233670a96bf836df7"],["/tags/前端/index.html","66c978410d584994c7380797d886c007"],["/tags/前缀和与差分/index.html","231f57217c174606ea4d933c29510fa9"],["/tags/动态规划/index.html","523648c255c78259e5c7023da341dac9"],["/tags/动态规划/page/2/index.html","f13a892733ec11fd38db20f1b19a6533"],["/tags/博客搭建/index.html","7695dcad724eb4e93708bd62115cd372"],["/tags/图论/index.html","5f521269d7cedb7900dfa6302df60990"],["/tags/大数据/index.html","8e455299201296f599f08cd2cd40ff08"],["/tags/大数据/page/2/index.html","c381edb936ac757582d4e0858222d718"],["/tags/操作系统/index.html","5b50f884ed93027e16632bc324ebc875"],["/tags/数学建模/index.html","e3dd3aac9124c64dd12d32776db4ecd7"],["/tags/数据库/index.html","31e2e794670a4f9b7497ff848415618f"],["/tags/数据结构和算法/index.html","d5042280f780d66f905fbfb4223844fc"],["/tags/数据结构和算法/page/2/index.html","d95d6556797ae70a61ab543136b3fa89"],["/tags/数据结构和算法/page/3/index.html","a23082b451827d70b52eee1f22de4504"],["/tags/数据结构和算法/page/4/index.html","dab63962778b48d1c741b9fba67d5244"],["/tags/数组和字符串/index.html","cefcfaf3216f57ac46764ca54d0c2a45"],["/tags/数论/index.html","32397d4f33822ad50928b2f8f4507598"],["/tags/枚举类/index.html","40789ae9e677a28d46ec2a9dca69b5cc"],["/tags/栈和队列/index.html","b58fa0043794523ca2bac0df1ac83499"],["/tags/树论/index.html","8a4c16195bde8f02660d0c4494ac9747"],["/tags/测试/index.html","800435d0105b59b1747c48b16a5eabd4"],["/tags/环境/index.html","a1900c167ccabf24a40d5053bfa97862"],["/tags/环境变量/index.html","839405f06cb34338e5e746318e350268"],["/tags/绘图/index.html","d1af1b72c5f7e360a96d569ca49a03b7"],["/tags/编程工具/index.html","ef0cff21470cf8d0d621237f38abf557"],["/tags/编程环境/index.html","1f371f75629176204414f06e4fa5c4fc"],["/tags/网络编程/index.html","31674eaada718fbb0c6ea27bf90b2f67"],["/tags/英语语法/index.html","54c1d1f1e0ce6ce89173b97ce70f9113"],["/tags/计算机操作系统/index.html","d0a069fd3e0049f8cb376d9310f03f2e"],["/tags/论文/index.html","8562b8f4a6418f94f0a13787c9b04b3f"],["/tags/资源下载/index.html","e8bcdf9324878d2b991205c9d958fac9"],["/tags/链表/index.html","3bc906a9765db5e0829f3ba19fbf4fe0"],["/tags/集合/index.html","1a9a8927366e01bf0c4486d7d369eb55"],["/tags/集群/index.html","1d4c04cb6993898ae85e45f6c5783083"]];
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
