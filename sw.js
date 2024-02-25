/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","f211d9eb8d7d79e39e4460e9a29acaf0"],["/about/index.html","6d75185e2d3900f9fd3ea5064795c29a"],["/archives/2023/01/index.html","a4aaa7d504bbd4bee5426f598246cd3a"],["/archives/2023/02/index.html","f89c9e07c496f18814153cecf2e4fd45"],["/archives/2023/02/page/2/index.html","dc2f1f23478ea50da2a05097392b3020"],["/archives/2023/03/index.html","44f3e5c010f1203bb46fb76243169eb9"],["/archives/2023/05/index.html","d009eec48f0afd428899ef040f14e21f"],["/archives/2023/06/index.html","0dd73192bfb4d40f696f1b22c3b8324d"],["/archives/2023/09/index.html","22a0b65d977477681bde9cff9b8b0f80"],["/archives/2023/11/index.html","982b4c16334d178cad963ee8c5d03a2c"],["/archives/2023/12/index.html","24708da965962c69ca3ff94027391914"],["/archives/2023/index.html","69bdc04a10af7a8253b619a38aed9e8b"],["/archives/2023/page/2/index.html","83ed61a388bd49384bad0e0e7bb85792"],["/archives/2023/page/3/index.html","0d7bc81407816da924c7dbe3ae4c758f"],["/archives/2023/page/4/index.html","0cbffdf3c1fc8ad341e8791999976cdd"],["/archives/2024/02/index.html","143829b2eb415f5e104f586bf1218871"],["/archives/2024/index.html","2d136471dadc3e49e6d2a26de22ece0f"],["/archives/index.html","9087f31030bbae83ea65fe16dd205cff"],["/archives/page/2/index.html","5e9232cbbbc12e34862dc71891e36ff8"],["/archives/page/3/index.html","27cb95fad826911440733f46f2fb52c8"],["/archives/page/4/index.html","66b756e9f36be18fc0dce53e6a274b2b"],["/baidu_verify_codeva-qQP2iZOMLX.html","82a05e06db8d14cab55a689aa0b8c6a0"],["/categories/Java/index.html","8a0f83732391bb55f844b451ada2792a"],["/categories/Java/后端/index.html","b2f543c32d8b56dca623c636802d4e56"],["/categories/Java/基础/index.html","9afa88a916276d4fae676a75b0201b3d"],["/categories/Java/基础/集合/index.html","1269189e349f576ba196b27e6383d2fb"],["/categories/Python/index.html","13222ae8e3a4a75c300e0d51f2878db4"],["/categories/Python/编程环境/index.html","5afeea35532a6d379165b6cf35335ab2"],["/categories/R语言/index.html","8bf6620391b7141cf9cdda07a4d62a20"],["/categories/R语言/编程环境/index.html","90bed7b5433d818c5886429ca0278974"],["/categories/index.html","fd3d7e7e864d37672122b4323eca97e4"],["/categories/中间件/index.html","6dd078701731a8c3f28d345c3cdd3128"],["/categories/前端/Vue/index.html","3f751b1d7c91b4fe527146c1cc1a782c"],["/categories/前端/index.html","9647c1d774916a4854257890c28317c8"],["/categories/大数据开发/ElasticSearch/index.html","478ecc64eaee08846afd423aafa71dc9"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7475a5ae6863fd91892fa6f4524278f6"],["/categories/大数据开发/HBase/index.html","cf226c737fe8b26f6f1caf8254b072a5"],["/categories/大数据开发/HBase/学习笔记/index.html","920bdc67fe7c691f697e4f994763dcf3"],["/categories/大数据开发/HBase/环境搭建/index.html","db015fd210a1b7a161c94319e76fdcb1"],["/categories/大数据开发/Hadoop/index.html","280ec71dca0b7a22221978dc1c2fbf60"],["/categories/大数据开发/Hadoop/技术/index.html","0106b583afe1e11f3f53585ab7098da4"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9d7fa5c7e45a70d07111399615943201"],["/categories/大数据开发/Redis/index.html","12372da742ac7a065c29c97081a5cc9e"],["/categories/大数据开发/Redis/技术/index.html","e7bd55d11e2700118c176adb0c1e3b80"],["/categories/大数据开发/Redis/环境搭建/index.html","b59f1da2ad65bd6df5d67f73ba732b44"],["/categories/大数据开发/Spark/index.html","f781dafb5d59eab19da2bb9db0e4a724"],["/categories/大数据开发/Spark/环境搭建/index.html","3005ccc32f1356fdb0b78d788838aca2"],["/categories/大数据开发/Zookeeper/index.html","aeee4b875ec149b0da8caddd22be2887"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ab7cb6a6f92d786c8761b9415b20ca97"],["/categories/大数据开发/index.html","96028af21db8acb9621861a7a665b41d"],["/categories/学校课程/index.html","829ca7e74f9ba775bd3801d42b6b263d"],["/categories/学校课程/计算机操作系统/index.html","06d2a4a88d554c9f4d7389d072288a35"],["/categories/操作系统/Linux/index.html","a586d375296df2ffd9773cf6299c1f05"],["/categories/操作系统/Mac/index.html","1e5a4a0069c7daa6b8e735041d1595a8"],["/categories/操作系统/Windows/index.html","eac288cdd2997566b301238cfa3fdd4b"],["/categories/操作系统/index.html","8717f9da36dd55e228fb0f56d2181d56"],["/categories/数学建模/index.html","303447be0dc185cfb5809412cc6b991a"],["/categories/数学建模/latex/index.html","0a5d36b48481a678081d7e26a210145a"],["/categories/数学建模/优化类/index.html","88a781f37c74fcf65c7334cd18c08e50"],["/categories/数学建模/优化类/现代优化算法/index.html","e25352fb790f56a038e3b6d6c30037a7"],["/categories/数学建模/优化类/规划类/index.html","e7e43f5aa59202c34759af57de245f96"],["/categories/数学建模/绘图/index.html","13099d9e77ec97893beb6bb8b2e74175"],["/categories/数据库/MySQL/index.html","13a3b27011302fe2209af60a1cd74933"],["/categories/数据库/index.html","fe664cfb0f3bc70761a4a481f1340b4a"],["/categories/数据结构和算法/index.html","5127ff8a85119f770b05dce09afca713"],["/categories/数据结构和算法/page/2/index.html","4aa7bccd18a572235589c0e812b29621"],["/categories/数据结构和算法/基本原理/bfs/index.html","2841404e64ed5913356376f5f15f4e46"],["/categories/数据结构和算法/基本原理/dfs/index.html","36fb168468266665c8f315e3f662022e"],["/categories/数据结构和算法/基本原理/index.html","1ff1eda4fcddbce959987dfee2b349e4"],["/categories/数据结构和算法/基本原理/动态规划/index.html","05a319981d95304a16440bc1498c0090"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","f3fc3b91a730f287f407f6cec6d0e36e"],["/categories/数据结构和算法/基本原理/图论/index.html","963e868797faf3c1a134a9e369330dee"],["/categories/数据结构和算法/基本原理/字符串/index.html","84e1b08121b00c51cb11027517e8e7d4"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fe927c7ddd85862744498bcf4917d8a2"],["/categories/数据结构和算法/基本原理/数论/index.html","e25cb7c3bd9ef2362865b35735d39abb"],["/categories/数据结构和算法/基本原理/树论/index.html","dec66089e50c28729db3c84d194b40ce"],["/categories/数据结构和算法/基本原理/链表/index.html","12a8b07954ecadf67a6248607b5faf4c"],["/categories/数据结构和算法/算法题/index.html","883b8980aaba848bb74331f14443fcce"],["/categories/数据结构和算法/算法题/二分查找/index.html","ca8bd443745dad5de00bfc44689cc192"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","9fffb5032e4287224c20c392ca744725"],["/categories/数据结构和算法/算法题/动态规划/index.html","882d08f6ef5418681460546de69f9f65"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e88b289d77cd7355188df271dd3123a2"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2a0aaa0eb12568568ae1f0e569436837"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","46525e9b82be5a332ee1605f8c0a6e3d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e322a9f95b7a3d8a37db80d4eebe4d1a"],["/categories/数据结构和算法/算法题/数论/index.html","e48aa073fbcb885063a58b24ec935580"],["/categories/数据结构和算法/算法题/栈和队列/index.html","1db28022ce28190722de9af43e212e1d"],["/categories/数据结构和算法/算法题/树论/index.html","8aff3dd1f15e707dc0ed7ccbdee1845b"],["/categories/杂七杂八/index.html","a4a7a016264d5105dbfad036cc8ab7b1"],["/categories/杂七杂八/博客搭建/index.html","c76a2df14accb1371a32f7cac6f47a26"],["/categories/编程工具下载/index.html","a455fe333eb0caf769ed975742ea09e8"],["/categories/编程环境/index.html","6571396b7a3f5255e1d7206533942f82"],["/categories/编程环境/大数据/index.html","59eccfe661548a7e1bbfd0e4af285226"],["/categories/英语学习/index.html","9fd73e961d0bd63a03c89cde4691b872"],["/categories/英语学习/英语语法/index.html","9c33392e48ac959582bf059bbbea2453"],["/comments/index.html","5c7e21368400b34f13c000329bdd8135"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ab35be661b19e4e11c77b7ecb87a699e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","d656e6fe6e7c2d583f5f7c189b757c1b"],["/movies/index.html","6febc46b1d889e6a1ea68fe268fdebb5"],["/music/index.html","98ffc998793d4914d2ef12ac1393e9e9"],["/page/2/index.html","34a3f3b4230bab438ec3299953b3d2d3"],["/page/3/index.html","e8005640c1347451b09b06b22ae31e35"],["/page/4/index.html","51500e7ce78f347efac0f3391504f7ec"],["/page/5/index.html","893fb4fc2f691436bc2ff768eff99187"],["/page/6/index.html","27997b158f557919ed22e84195faf28a"],["/posts/1021360842.html","5fac2cded5c23c0eee514641d4eb82b8"],["/posts/1120620192.html","8a92c6869a2a1376d8d3b97d7513f380"],["/posts/1141628095.html","8c2fb1bee5ffbcfccf42d74ffa8c496a"],["/posts/1168613674.html","2db53719cd77df45c868b45a2c92d0e4"],["/posts/1219920510.html","575eb4bb9362b300859eb3f90a156752"],["/posts/1222166338.html","89ed961153af372b251d58270e7dc7e7"],["/posts/1259097482.html","f8cb7c56cb2ebf002ccfca9c2ce3feb2"],["/posts/1271036369.html","55860e59f8106db3e374cea7fbacc06f"],["/posts/1312847445.html","f9077d6de08a6c630dc59296487ff1e4"],["/posts/135355774.html","284512ceebc181d76ce524baea7f4789"],["/posts/1375344716.html","f20b221e885238f9c589b168e33a09b8"],["/posts/1388991698.html","ca7a7b5472961d51edf9ab6eb5eccb7c"],["/posts/1410315814.html","948d40a5554b381e1f1a67d7761d98b0"],["/posts/1452790229.html","4432046e84230b370bb693b9a3ce577b"],["/posts/1470079884.html","bb700f3bbace1ce4255289f37d87d52f"],["/posts/1470079885.html","50edb23b739c0c7a5219f12136bd9c3a"],["/posts/1470079886.html","ca40384c49bf58d27d83cdf2adb8e2ea"],["/posts/1470079887.html","794d8c33d1233448eae12c8a5d89b865"],["/posts/1498536549.html","be84630c3b33109fd717d32cabe8a516"],["/posts/1539568593.html","10bd686a7118f9125c3a0696d458f616"],["/posts/1547067935.html","15e740b998e9497e1b1d8ee62ac28c6b"],["/posts/1557866301.html","6ae4ae87682dd5a99011cca66a044336"],["/posts/1571776361.html","d6c261b787bfe88eb24b1abe1dbf6df1"],["/posts/1605124548.html","b17781596ec5eaa38928f17263d1635f"],["/posts/1633036852.html","c5aa1ff2a85b9ab03ad3ac913dabf96c"],["/posts/1674202625.html","ee6bcd513eee4ad0d3e212c4703ead95"],["/posts/1765123828.html","b7b8e15b4b014c69054db1dc7148b745"],["/posts/1767336200.html","e033714f8de9451517249983339d3dcc"],["/posts/1776114197.html","f3f52baebe8f878a27f3b2bd1d6633b9"],["/posts/1817748743.html","af37322c20906e403cd157ddd5c0adad"],["/posts/1925125395.html","3e0151c1ea962361d83e4ae8f23a463d"],["/posts/1966191251.html","939d2a702fac708c5b9f443e2f49406a"],["/posts/1987617322.html","f19eff29ba2b2885fa819b380960b726"],["/posts/1999788039.html","d126131d6e957d7c7fd9fe9f18b43a7f"],["/posts/2075104059.html","49dec388355f7350e2ba51651094f8df"],["/posts/2087796737.html","43078444ac900dceae036b8e04c982dd"],["/posts/2106547339.html","a87e0c86b60300c0841257598d936716"],["/posts/2207806286.html","546996b8764617ea2528e726855344dd"],["/posts/2225903441.html","65b8da4e4c18475db2ef19d917c37325"],["/posts/2265610284.html","5cf74745f129101f3f0e4adc078f3d12"],["/posts/2281352001.html","692372df4fd5c1375e75436af8f176b5"],["/posts/2364755265.html","7ce31972254cf885d94ad65e8234ac89"],["/posts/2414116852.html","62fc4e14dd7113aeabfe8784120990cb"],["/posts/2421785022.html","fb57dba8c6d5fa2ddccede438df59c0e"],["/posts/2482902029.html","b92bf663b11ef40fa0c0f98cd7183e09"],["/posts/2495386210.html","719ac6cab594a496652862caff9bc0de"],["/posts/2516528882.html","b00d2862d40c4550988b3f9c9b189f68"],["/posts/2526659543.html","2c418cbe7fa24a47301fe80f15c71ac5"],["/posts/2529807823.html","154fdcbbb24968894c3e3fab5ddbd3be"],["/posts/2596601004.html","92083c9553edadd889981595f7ea3d58"],["/posts/2697614349.html","1bb5cbc8d13c4bad891b983174e7b4a9"],["/posts/2742438348.html","9b7df6bbbbc2db280ccad1600bacf8c7"],["/posts/2768249503.html","79706ee804928970f17cbe9d315e0d97"],["/posts/2864584994.html","81ca1a0a5f01d6ddbda38fb4f9ab07ec"],["/posts/2888309600.html","eabdc718c0312ea57df7b2c63f2d69af"],["/posts/2891591958.html","92a3c62b66b90369ddcdd037f730ba9e"],["/posts/2909934084.html","2aa79e0e6835ea881d6ed400caecc29c"],["/posts/2920256992.html","1189790f7e1e87ea1656e6896499acae"],["/posts/2959474469.html","598cbd5457e391dc786597b4abb46aa5"],["/posts/3005926051.html","98582ae88b8ccc88d9af9f0ebaa1392c"],["/posts/309775400.html","ec512f58ec9e27cbfe98797ca73459b0"],["/posts/3156194925.html","0ab83aa7379b4d27b1d0399496e2c223"],["/posts/3169224211.html","05b8b8e39749658011e936f6e9c05b2e"],["/posts/3213899550.html","b5060106c61545a6f3d541cf6f50587c"],["/posts/3259212833.html","491d8b3ef45520a8858b37d9f28643ea"],["/posts/3266130344.html","ca180922f2c76ec82ee51f42e05ef364"],["/posts/3292663995.html","7ff94ad57049d9943bbc23424e936d1e"],["/posts/3297135020.html","35c1dcf3e87ce0f24fdb2b842e974f8f"],["/posts/3306641566.html","0a3cfb1fad8dbb4b20d441a2b96774fd"],["/posts/3312011324.html","0c188c725ef2af3ef55450d2248b6fe5"],["/posts/336911618.html","c1e978febaf14a9f9b94396cce1df7f6"],["/posts/3402121571.html","0f79d8c5724bf062f1796c391835c560"],["/posts/3405577485.html","fd2e133d1edd726e883d787baeb617a7"],["/posts/3498516849.html","9f09ca22874ced9692d46513393b6a1e"],["/posts/3513711414.html","e31feb282265c38db939b4bb86448cb7"],["/posts/3523095624.html","324f1833852eb761e3d858dc662b5b83"],["/posts/3546711884.html","ce62957942613ed8116442b058cd12dd"],["/posts/3731385230.html","4de58f1e66fb0bc3861b8ffa91deddb7"],["/posts/3772089482.html","fa311969e0fd22ce500579160d74bb47"],["/posts/386609427.html","c1e2e849027404b9f93da4c3ea9d6db5"],["/posts/4044235327.html","30c1b556ed4459f2568f98a3e40b5602"],["/posts/4115971639.html","8997eb00626693f812f06c5613efb54d"],["/posts/4130790367.html","cd01a9fa8d1d3b52fde56d114e244d38"],["/posts/4131986683.html","41c68bbc97949c65fafe397d24e1670a"],["/posts/4177218757.html","88f1901a9b31d85ff9d231057815e2db"],["/posts/4192183953.html","106512f630cd30c275810d24b54e4eb2"],["/posts/4261103898.html","f2aec119afa07ed1f073c93880ea041f"],["/posts/469711973.html","c9886d72ed5f10018a0d3745be1e785d"],["/posts/482495853.html","f9de3c4979f009f079f24d587721056c"],["/posts/488247922.html","7bd319adb588e13823d333b9c11221cf"],["/posts/517302816.html","567e779bd65738f87f140e658a59190c"],["/posts/570165348.html","96f473741ef359fe09f28c46a789c610"],["/posts/595890772.html","710e7a3750af33b9ca111f1e531c8aaa"],["/posts/67485572.html","6a1bea5e853e578bc2bc07d9e9467974"],["/posts/694347442.html","5936c842724358be7223b2ca5759ae82"],["/posts/707384687.html","4dbd43155bfdf60af49e13c58e5dff95"],["/posts/71180092.html","c17e2a7ca39416ec1d3a83d0d3f807e4"],["/posts/716459272.html","7fc3383fe318b11fa64f2e8e10b5038d"],["/posts/765481613.html","ba909dfa194489ae1932ba2df96c2da5"],["/posts/778231993.html","262cec5269c95072059441f5f0c34f56"],["/posts/795397410.html","3dde4b24b87f9718c161ac583b8dc25e"],["/posts/820223701.html","5ea751f3c402267123ebf2017fff3f3d"],["/posts/830372185.html","d782d577289e6f7c7678ec2a212dc209"],["/posts/88294277.html","d4a47ece66c2da5fb3313ec6c22e889e"],["/posts/939963535.html","7c66a0d6fe4e28047ef34adc33c28033"],["/posts/983786067.html","0a8f684f203f751b2bde967a8f14ab55"],["/sw-register.js","2a1e17d10a18ba46d252df637eb9671a"],["/tags/C/index.html","1407de0fd66b26cb600a2e75f70364e1"],["/tags/C/page/2/index.html","34c7f97d98c1814bc39ce88325bfad6d"],["/tags/C/page/3/index.html","a2490fdde9d1525c31db5caa1a89ee9c"],["/tags/C/page/4/index.html","ee5ed1ff842dd328f03f05dc88d8efdc"],["/tags/ETL/index.html","b50c7946d2aeac5ccc70dc506e216b0a"],["/tags/ElasticSearch/index.html","cad1d1b225157230e8c9498bab0d6938"],["/tags/GUI/index.html","77f1cb4924d757447c6ce91e588522cf"],["/tags/HBase/index.html","78da47fd89edab398cf5c0cdf1d96598"],["/tags/Hadoop/index.html","82e5b124c40f2c0a4e777488c9969012"],["/tags/Hadoop/page/2/index.html","4ce4de81602cfd18b569b929339ec265"],["/tags/Java/index.html","a08c3054b37b43f30f65c5c8062b3157"],["/tags/Java后端/index.html","c9ace515dbad179b47338e0cb57935e2"],["/tags/Java后端/page/2/index.html","b7dc76b1cb648dc49032dd989b2e5dd4"],["/tags/Java基础/index.html","26107e707141037ae8155718d0c0d848"],["/tags/Java基础/page/2/index.html","aac03a170ce460fed830593cbfea3856"],["/tags/Kettle/index.html","fb5cd7c867e72a178b26efdf6b40cf2f"],["/tags/Kibana/index.html","d1f2f4ff77ee34bc8e1766ae43680477"],["/tags/Linux/index.html","166b6f4088b5a65fdbff3d6a521ccde7"],["/tags/Linux/page/2/index.html","7793ecd677738b929b6409d1be624850"],["/tags/Linux/page/3/index.html","a607ceeebf50d0c11aa6d6b0ebbe591e"],["/tags/Mac/index.html","11e6a8b59f7ee2e0f694e066a5313aa4"],["/tags/Mac/page/2/index.html","d19c6becdb17827370048177dd798dd7"],["/tags/Maven/index.html","68e608b4b3cdcd4f074dee742accd4c7"],["/tags/MySQL/index.html","051ffe7983ad5b75039e7bd2b23512c0"],["/tags/Python/index.html","e87a06bcec3ddf4b3c7f214c0f5416c8"],["/tags/Redis/index.html","f3ad7faf18a7f5c5f784e090b7a4d48a"],["/tags/R语言/index.html","2c1f1ebea1ef8a83546ae6cfa74de6d1"],["/tags/Spark/index.html","6608c72922fe4b59c1335e0d816188ae"],["/tags/Ubuntu/index.html","266c748fa133272edd7cc74ba3abb8fb"],["/tags/Vue/index.html","21d6e4c087d825fa66f10200b14e1118"],["/tags/Windows/index.html","1e302c63fa49000772b7460504c1ab40"],["/tags/ZooKeeper/index.html","e19ce0e23351c4946c25aa07c5eda8d3"],["/tags/bfs/index.html","bc1c2d9243301d47cd6434fa7c540d0f"],["/tags/dfs/index.html","c762ad46c2a328f3f3b72433796f5b35"],["/tags/folium/index.html","93e9f18f2262a47a1bc780e291f0ce1d"],["/tags/git/index.html","97b3d85d3760f07ac597be7a6a46f5eb"],["/tags/index.html","4b77fa51504f9964a90958025a57a7f2"],["/tags/latex/index.html","0bda98a859c69ed152c64f9583a05e20"],["/tags/中间件/index.html","3a4111c1a92b862ce2d3e65acbc1a92f"],["/tags/二分查找/index.html","2564abc1f6200d83251bf2cbca4b0b5b"],["/tags/优化类/index.html","7117ff17a21ea9c8347d1267be9f7bdf"],["/tags/前端/index.html","929653ab870b050e4adce8c501ec1066"],["/tags/前缀和与差分/index.html","d1c01d5fc0fe9fcff9bd0c7276649cd9"],["/tags/动态规划/index.html","45a9b8d6101154524df2a6ceb1517f5f"],["/tags/动态规划/page/2/index.html","2e055ff5aed9b210e3699ceb92c0d324"],["/tags/博客搭建/index.html","6b324a173923b28f92246c339b584b04"],["/tags/图论/index.html","9898592f3e0d32ab113f3ee976aad235"],["/tags/大数据/index.html","04e0b5c3553983468f29190f3c7d02b8"],["/tags/大数据/page/2/index.html","75cf1296c9f3fd90d0c66a75869a68d9"],["/tags/操作系统/index.html","e0c2a844b17222bfb56da0a104bc208a"],["/tags/数学建模/index.html","1379fe24aa3a77ab504b5dfc3544676e"],["/tags/数据库/index.html","77061cab0bbdd9095df94b3575537e9d"],["/tags/数据结构和算法/index.html","a82c57eb260e096e1b448ff2430abba4"],["/tags/数据结构和算法/page/2/index.html","7c9e17404ad535bfbc1c35cff041c803"],["/tags/数据结构和算法/page/3/index.html","05ef7324041a67668f77c7b02409918e"],["/tags/数据结构和算法/page/4/index.html","9243c023e188c3e3f1b09a262a9f16d9"],["/tags/数组和字符串/index.html","94d9a4f6729d410797574bcde476b99a"],["/tags/数论/index.html","f1b10140e26a95c8e3536a786ba81529"],["/tags/枚举类/index.html","16768e6a9ff8f354d8de3ea27cd60761"],["/tags/栈和队列/index.html","600d288d80a042f255bdf280ff7cbff4"],["/tags/树论/index.html","5f11e6984f13bbaa38ec27c8dd9b0da1"],["/tags/测试/index.html","1341dcdfb9e0f83454a29805a2b54d83"],["/tags/环境/index.html","91baa51e877df32798a31dbc11e67704"],["/tags/环境变量/index.html","785003d150c27689fdb357f3b8512c96"],["/tags/绘图/index.html","47af1a1c248e8ece6bf152f02c1231a5"],["/tags/编程工具/index.html","ee3c7a36ce63379f4469581956a40eb3"],["/tags/编程环境/index.html","d2c1ac2d84643a524956c8ea6b27ae8c"],["/tags/网络编程/index.html","cf2a4967f88b7f3c3a4acd5e6cbe7f12"],["/tags/英语语法/index.html","333467e99f6ac5483e7894459936120c"],["/tags/计算机操作系统/index.html","51f5139384257d588e732417e07193d6"],["/tags/论文/index.html","ed26feb043fb91cfea799b59e31449b5"],["/tags/资源下载/index.html","678d135269126bce7d31d43b6c9cfcad"],["/tags/链表/index.html","4a9b81ec811c428acca48565181e6257"],["/tags/集合/index.html","d96b6acf8ced1f69e8f1ef2d4a3af54a"],["/tags/集群/index.html","3122eee6bde5106bc18df24691478e3d"]];
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
