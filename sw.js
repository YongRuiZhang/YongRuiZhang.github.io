/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","3b3e919f2bfca2a69fe51bd2a5ce3171"],["/about/index.html","9974103f685104e3448429d420180cbe"],["/archives/2023/01/index.html","220e9b51892a92e9a454a252799d9dba"],["/archives/2023/02/index.html","c279dbc11dd758114eab1e3e17b1db25"],["/archives/2023/02/page/2/index.html","97225fef9bb508ba7160588a7093804f"],["/archives/2023/03/index.html","5b47576b59d6e6d2a9d2c8d1d8b9a78e"],["/archives/2023/05/index.html","f5b25d2bbe547f0412fc701c2e34ebfc"],["/archives/2023/06/index.html","c29adb6fc29c84abc71bf3009695b269"],["/archives/2023/09/index.html","7517c7ec45953db1f57dd051f79de372"],["/archives/2023/11/index.html","6a7efcf7b40c51df248e39b6c136f5f2"],["/archives/2023/12/index.html","394d3ab4556b3be6c26481a822a87cc7"],["/archives/2023/index.html","ccd48a9356812146c81cb988e6ee2960"],["/archives/2023/page/2/index.html","fd58a0494397ece58103eb8728dd1557"],["/archives/2023/page/3/index.html","00706caee9139ea092061b494095cf77"],["/archives/2023/page/4/index.html","dffb690bd46ec0cc28c8d65780c9b163"],["/archives/2024/02/index.html","9ba292473baece278a20463b8ca22e5c"],["/archives/2024/index.html","232cd47c427a6929a8a2c44212c2beff"],["/archives/index.html","29503c4cf3ce1c60536eaf9f1eb36403"],["/archives/page/2/index.html","5ffc3dfb21d3bfdfa1da77d4b8abd63c"],["/archives/page/3/index.html","3f871c4768d1c13d383a15627d4c112c"],["/archives/page/4/index.html","94316dc88da10e03062d127e814d162c"],["/baidu_verify_codeva-qQP2iZOMLX.html","077838771cc8a90b8850d5aa8f1d604a"],["/categories/Java/index.html","34b2d7ef14425c5fab2fd27a5d1b217e"],["/categories/Java/后端/index.html","681655bd87dd5afdc6a7735e4b607ba1"],["/categories/Java/基础/index.html","c6c1a8ee938248c6c43a9d8fc68069dc"],["/categories/Java/基础/集合/index.html","79dee45f8687fc1bbeedfb8d51799c95"],["/categories/Python/index.html","50900a876134bf561e71e8bb3e6ceaa4"],["/categories/Python/编程环境/index.html","4e7bafce52734e1e3c7993d2b373cab6"],["/categories/R语言/index.html","90ea6480bfc4d15616c9ad6361e8d852"],["/categories/R语言/编程环境/index.html","702b0c156de4551a4345eded5f1d5b90"],["/categories/index.html","7e9fa816a507ce33712e6a797a70dfba"],["/categories/中间件/index.html","7878f29ebe70505d425723efb0958220"],["/categories/前端/Vue/index.html","4a1fb3b989bfb7999d9bb1a7ebb6cf88"],["/categories/前端/index.html","ec8bb4d603a13d303e978fa4dbcb91df"],["/categories/大数据开发/ElasticSearch/index.html","005e1cffa2391314b178b4c238c6d5ee"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d500662c375ff3378db3df02a6534a59"],["/categories/大数据开发/HBase/index.html","04edd132af7e60ae9c40e6bc7529b59a"],["/categories/大数据开发/HBase/学习笔记/index.html","84f4bc9f2c892032342f1daf19cdad41"],["/categories/大数据开发/HBase/环境搭建/index.html","1ecdae7761c558bdd98cbeed8e0a0b29"],["/categories/大数据开发/Hadoop/index.html","bfd9bcb45e450b0de6505cb11bf9d5fa"],["/categories/大数据开发/Hadoop/技术/index.html","08c3452f32c0c8779c8619afb96931da"],["/categories/大数据开发/Hadoop/环境搭建/index.html","6d11b4f25acdc3fa19cde0b7ef39a7e9"],["/categories/大数据开发/Redis/index.html","63df30438e5938a7e45b448d83840d6f"],["/categories/大数据开发/Redis/技术/index.html","4f5aa2479f86bb6b6f8b0b69dd634dc2"],["/categories/大数据开发/Redis/环境搭建/index.html","5abab5766abb09ed09f7d39d45e7b81f"],["/categories/大数据开发/Spark/index.html","975efa94f7df9d33915f8bebf0f23cac"],["/categories/大数据开发/Spark/环境搭建/index.html","0bbe31e69ee331eacb3328029b1203a0"],["/categories/大数据开发/Zookeeper/index.html","faeceb81cff0f33527940d7cbe03ae53"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","73253551e9fd0586336665d5e9eb56b1"],["/categories/大数据开发/index.html","0cfeaab7ee9010b4abee451dbb2b7b92"],["/categories/学校课程/index.html","6d4f96699a399b75518c84dc6e8b2ac7"],["/categories/学校课程/计算机操作系统/index.html","186012e778209256e153a8fff9d7bee4"],["/categories/操作系统/Linux/index.html","e98330ff6f637ab8bef80c95e3c4c16c"],["/categories/操作系统/Mac/index.html","2ed4c97b7bd0c0b9a8d1a63e343b2569"],["/categories/操作系统/Windows/index.html","0f56452e2f4f14112d7f962fd2d279b2"],["/categories/操作系统/index.html","3b86142fa4a5881ae6707d8fafbfd599"],["/categories/数学建模/index.html","bf82fd25361b7fbe0a6eb6569b229669"],["/categories/数学建模/latex/index.html","92e2817cb948bfc689159948f79303ca"],["/categories/数学建模/优化类/index.html","3fbbc1bf2a7ef94bdfc4d6def78c982d"],["/categories/数学建模/优化类/现代优化算法/index.html","af960b56f43f36051c2e6a0da916220e"],["/categories/数学建模/优化类/规划类/index.html","2c642168db20740547d5784c248cbaad"],["/categories/数学建模/绘图/index.html","edc8877bd001e2603a044341eb747b66"],["/categories/数据库/MySQL/index.html","eee4ed15298440714ca546a58d0279d6"],["/categories/数据库/index.html","4d7dfad4eb782ce0aa3e99448392b271"],["/categories/数据结构和算法/index.html","c25351395a753873a54f49e3efa7118d"],["/categories/数据结构和算法/page/2/index.html","946bf3efdaa5f555b750403d77b6ed4d"],["/categories/数据结构和算法/基本原理/bfs/index.html","eab8d500c6de5aec245aae3c6d4880f2"],["/categories/数据结构和算法/基本原理/dfs/index.html","2097ee76decb000ec829fc0bf846e277"],["/categories/数据结构和算法/基本原理/index.html","27b6917d33cfb2abd529c890c0c78c69"],["/categories/数据结构和算法/基本原理/动态规划/index.html","47aaf1d6b61a75b41ce0b98169e2628a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ac6ff95de8f3433601e9bcfaa2a4d0b6"],["/categories/数据结构和算法/基本原理/图论/index.html","2903e8782c0008a3c2669c22d3eda26b"],["/categories/数据结构和算法/基本原理/字符串/index.html","68956a52848e6347083e4f521721e1ab"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1af3bdff6fd93b753cebb421f9f21c71"],["/categories/数据结构和算法/基本原理/数论/index.html","d726f9c7e3b691b1696acb542492d8f8"],["/categories/数据结构和算法/基本原理/树论/index.html","99c4ed35a1bba8c63689620bb92b0dc6"],["/categories/数据结构和算法/基本原理/链表/index.html","9a0d715820d56b22a4add12c13e511ee"],["/categories/数据结构和算法/算法题/index.html","1a552fcc62f48eb27e20d07897197fbd"],["/categories/数据结构和算法/算法题/二分查找/index.html","8a8676795693c05fda4812216ecb07d2"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","58a84b133716342b90f992c6eb76537c"],["/categories/数据结构和算法/算法题/动态规划/index.html","eaf16f60dd646f58a07ccf4508cab08c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","9263dce2ace717063afb1438191ad2df"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","7ed74be59d677ef590be2dcb1b3fb8ae"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","96ddbafbd690f74933ffa527250e76b3"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","53a96a654f72441cb34fc499f3231677"],["/categories/数据结构和算法/算法题/数论/index.html","fd64fce809d326c2a0f42a3784d4528e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","dc1364fec382ee00162282ff555ed795"],["/categories/数据结构和算法/算法题/树论/index.html","2d914fa12cc930bd2d1ca58c7f9be88d"],["/categories/杂七杂八/index.html","eaefbcd129229671706465e0290cef71"],["/categories/杂七杂八/博客搭建/index.html","3a08fe4f396bc307d4a75a91b10c981f"],["/categories/编程工具下载/index.html","f574814de0e2215e95c3433bb144fd3d"],["/categories/编程环境/index.html","1509087bc62db68919c65be820829385"],["/categories/编程环境/大数据/index.html","b13bae66283d25302bad0c8c6c52e019"],["/categories/英语学习/index.html","69dfdf33168efa9c0c0cbd20ff4eac74"],["/categories/英语学习/英语语法/index.html","9c9e48f561cd3f91415b89dd7f94d163"],["/comments/index.html","49d301bcab86bec767352ac4bb5a64e1"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c5c2a06a9409e2ff96674ecb0c5d35d4"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ab3cd8c10953e4af4e70c5bff6301f2f"],["/movies/index.html","c5a6939c713234725eab7ee38395b3ce"],["/music/index.html","9b6eaa4aba93584b8f1ae2ed43fdd3d5"],["/page/2/index.html","90226bdca3acd2026e0fe5b7f7d69152"],["/page/3/index.html","3fd2c65a521542f00b7b5a539078ac7a"],["/page/4/index.html","0bb562196cab5db4e29a05cbca379fad"],["/page/5/index.html","49b69ace740fa283fbfded5436b647ff"],["/page/6/index.html","4462a6d8fd7ff2ea5067180a9d3acc9d"],["/posts/1021360842.html","f3fd06c835c00820a8f2fe97d606a981"],["/posts/1120620192.html","e922a1ee2e8d46c30377affdcc730ca2"],["/posts/1141628095.html","1135986b932111bb2de72d9d73553c59"],["/posts/1168613674.html","6ae23807100998bef477333435abd648"],["/posts/1219920510.html","00ba2d90d6d56de759a9b757ed5ea214"],["/posts/1222166338.html","1ab68fe78506a769a2163b56c84ebf8f"],["/posts/1259097482.html","953e65a6e4783711b4eb26f069824e93"],["/posts/1271036369.html","cb9f6075e96d01eb2463489ba4196065"],["/posts/1312847445.html","bc1fdcfa4722973482d7394f62d4e66d"],["/posts/135355774.html","6feb6ae11ecb4f3ab64a00cb2f45ab2b"],["/posts/1375344716.html","82f4e70dc9410311d3b3f1a5426388d5"],["/posts/1388991698.html","e29d370941f18c722ca1b2d81617fab4"],["/posts/1410315814.html","e214e48296ac91c46c4ca5e63234305b"],["/posts/1452790229.html","64bab1a785fee565c585972629ffa733"],["/posts/1470079884.html","7f9f5bffe60195ce32a392171d20c0fb"],["/posts/1470079885.html","9b755ce737e4330c1dab57665806aff6"],["/posts/1470079886.html","50dbbafea77eb772a066558fac584fd2"],["/posts/1470079887.html","3f6abd0308060ccf932220ffbc1bf497"],["/posts/1498536549.html","955164cf737ea03198274d67e988eb07"],["/posts/1539568593.html","13235a3dca14fdf80d9c5a5abf5d23b5"],["/posts/1547067935.html","afaff22ce6c4df302ec5107a58105e29"],["/posts/1557866301.html","e54a200419adab5360cc1abcfd49beb9"],["/posts/1571776361.html","5c4cebf03baaf5a55b5aec350e078f60"],["/posts/1605124548.html","a0d6101decf271a0394120a39026c6f2"],["/posts/1633036852.html","f80e99e85d80d53212aa832bafd40040"],["/posts/1674202625.html","294db59e59296b44e796d2188b36cce1"],["/posts/1765123828.html","fc305d6e4ef17b00149b27c469ccdb33"],["/posts/1767336200.html","446daa18689019d4920fcceadd1635f7"],["/posts/1776114197.html","2d67851df8bc4abcaccfae89041347b1"],["/posts/1817748743.html","abf1026661a6d2dcf91f8b2c21953b9e"],["/posts/1925125395.html","7c64f2b11839bc855a40b881a3a819fc"],["/posts/1966191251.html","678e1c5ebf8a5c70129e41f27823f351"],["/posts/1987617322.html","e99cd6ebca6a51799d9c4433e66f697a"],["/posts/1999788039.html","3d28d1b2acb06debe05fe0d18bd527f9"],["/posts/2075104059.html","e48317a66af66e65373c2a5880e1aaaf"],["/posts/2087796737.html","e17a99eb2cd430d92e81c60dcc36c0e6"],["/posts/2106547339.html","f9dcb70931111e8db59c2bf324eb532b"],["/posts/2207806286.html","022deec5b87691fe1e7ef1e88f5979dd"],["/posts/2225903441.html","435354221de040130c348066c68255a1"],["/posts/2265610284.html","123793a8211b80add1b92e2c78c5143d"],["/posts/2281352001.html","ccf84dfa9a957b147b57d217a92a6ebb"],["/posts/2364755265.html","7bd60cda286a1488e3f492141defb0e7"],["/posts/2414116852.html","bcbbd0ea44714749e64027fb322dde17"],["/posts/2421785022.html","390fda4ac6497848ab01f8959ea095d7"],["/posts/2482902029.html","1778ee3d50b50ff2d9012e28226287f5"],["/posts/2495386210.html","6558d76ff8adcdc1c09b229c236ae193"],["/posts/2516528882.html","8fbc0565f729c509622533dbd1d4791a"],["/posts/2526659543.html","72eceb0f39a995b82568e9bd82bfa87b"],["/posts/2529807823.html","da981ac38d8d6a1c3af9d7d175a3ec74"],["/posts/2596601004.html","8d793bb5e935bbeca537308bce5cb38f"],["/posts/2697614349.html","920033b033dfb74ff69c697b25c2a316"],["/posts/2742438348.html","0e93e832ac9a7a3edf2020053d47198e"],["/posts/2864584994.html","f9356e07b34f4444032ab050f9522e62"],["/posts/2888309600.html","e0cfd058fb18e9a13583a7f8fdc16149"],["/posts/2891591958.html","013d9e5c5004b084929b6e11b381c24a"],["/posts/2909934084.html","6c77acf74a09ccad1aaa720ed2ed81a1"],["/posts/2920256992.html","cc8025cf44b3a6a6d4fe8a84264857a9"],["/posts/2959474469.html","318d0c5cfbdc170ded2de1ab32b404b9"],["/posts/3005926051.html","3e86e5064a506e1ab2788c3dfe0dfd66"],["/posts/309775400.html","7b6f6300baa63bb58787a5473a178c19"],["/posts/3156194925.html","4b1e9566eef6e9b9003a043255c3ec5e"],["/posts/3169224211.html","be783e7a58698ffd3946098d07ad5ffa"],["/posts/3213899550.html","a03d23275c052d56150c7cc8c1b4a9a2"],["/posts/3259212833.html","c84bbb26ce845294e41900e51084897d"],["/posts/3266130344.html","829bf4b8540e7b2b420fb65aadfb0639"],["/posts/3292663995.html","3a441753e589ff9e9c3c41029dfbe87a"],["/posts/3297135020.html","fb78c3adc4a197abf7bbe7f004152b24"],["/posts/3306641566.html","aec4c64cd1b9faeeb5029745a3dc9555"],["/posts/3312011324.html","0b36a48dbaae2d0d862ec759add9ab6a"],["/posts/336911618.html","7d5c0652a5a8bc06e0a41e195fa26df7"],["/posts/3402121571.html","4ec2528de6cf9539e41bfe261d2acb2c"],["/posts/3405577485.html","4b5c5f62aa7016ed1dc054e2920b33dc"],["/posts/3498516849.html","65001d29335ef945bbe17ba54ac92994"],["/posts/3513711414.html","abef237f0bbc55515af9204a6be15a53"],["/posts/3523095624.html","b42a8176329f84c044be73f99790652d"],["/posts/3546711884.html","09acda62fcf77f611d03ab8508629720"],["/posts/3731385230.html","99145f71925ceee339d76c4d40287618"],["/posts/3772089482.html","aab18be29ed9dfe682e80a04784dafd1"],["/posts/386609427.html","3f522e93f4f8bd911a84767a2ad448be"],["/posts/4044235327.html","12c566f16539f0d7875b9f036d25438f"],["/posts/4115971639.html","0ff00ca90f20548a4dfa2aae74aa71fc"],["/posts/4130790367.html","bea5d0a0be52cb8aa6ebf0940f5626f6"],["/posts/4131986683.html","ed46ba2b10fa62e7c52361f07fcebe64"],["/posts/4177218757.html","d504edb7e4f8c776fbc6eef0ad2beec3"],["/posts/4192183953.html","ea6fea233dff3db9e3a078e1dc63683e"],["/posts/4261103898.html","193158a50fe7e9caa97a5beef2700848"],["/posts/469711973.html","abea777e6c901c9ac15edeeded3b7a54"],["/posts/482495853.html","632f2fd390a078c1ce6ce704ef235352"],["/posts/488247922.html","01a687dba14cb6ded97cd08e6a4c5093"],["/posts/517302816.html","9da816c03871068cf115dd14c86f607d"],["/posts/570165348.html","cf31d4543890fc657ffa672333de2794"],["/posts/595890772.html","5f3e0270d95c5c650d44cf1df6d84e0d"],["/posts/67485572.html","d81d5d64eacf9e2e148dcb2ad681ea0f"],["/posts/694347442.html","f2af430958eb0b2366ce4bce5efd4d15"],["/posts/707384687.html","b32a25a275f252e7ebf6413261b0a27a"],["/posts/71180092.html","4452bf9d12a31c84e5b109865aaf39e4"],["/posts/716459272.html","25ff2e8b566e19b340b26ce35802ed58"],["/posts/765481613.html","062971a799ce66727cf65581e7141ac0"],["/posts/778231993.html","14aa6ec691b09c52fc413940d67e6b69"],["/posts/795397410.html","bc33b4d1c035ef71873a34a6b41e2acd"],["/posts/820223701.html","17bf82078f21028726488b98562544ea"],["/posts/830372185.html","9f45e0d1f2d78fdaf8bc82af23c3cfc2"],["/posts/88294277.html","74cbd4553c39f6ee25c316d19b6dd68d"],["/posts/939963535.html","8b14b0bc43d2cef41cbb2a7619749c46"],["/posts/983786067.html","38159b084aec7983e446d1a3a5f14e4a"],["/sw-register.js","0aa63383aa46bee2c8cbf09837a8d892"],["/tags/C/index.html","43bb4861f7ee44cf8e2886a532a455f2"],["/tags/C/page/2/index.html","a73f593519e448b8278a7e421c681822"],["/tags/C/page/3/index.html","f260e33e5d9301d4660d326b798f9e8d"],["/tags/C/page/4/index.html","79c84faa2f39daba0e492dff3da7b773"],["/tags/ETL/index.html","d2aa1ab9d04402fe6e8fac969d6da4e0"],["/tags/ElasticSearch/index.html","d2cfa9b192ae95664312d6ef9673c2dc"],["/tags/GUI/index.html","1e84a9b6e60f7b4644a1e6ade94828de"],["/tags/HBase/index.html","89f164934048e331f5abe021a6f1c0b3"],["/tags/Hadoop/index.html","c12fa99386d194e846e392c94d492f83"],["/tags/Hadoop/page/2/index.html","a268f063fa4fdc3899f0db917c41dca4"],["/tags/Java/index.html","956988bca01f21915e95594d03d9b086"],["/tags/Java后端/index.html","17669a70423c3391b9f2b718b306ca40"],["/tags/Java后端/page/2/index.html","74d3c3cce6e42d21a104e6faf4797fc9"],["/tags/Java基础/index.html","8af4404445ec03081c428eca0e63fb0d"],["/tags/Java基础/page/2/index.html","dd58c9fa8ccb53f60b5c84f8e03dfef6"],["/tags/Kettle/index.html","0ddae0b2a07c969a5335dc15041bf305"],["/tags/Kibana/index.html","6012354459192331c7a098e124c6d6ec"],["/tags/Linux/index.html","45f4f2c0e5f85f5aff851722ac50e0be"],["/tags/Linux/page/2/index.html","380050ee76024102e87f777ce2af7d1e"],["/tags/Linux/page/3/index.html","fa1cd5c63582b505a2fc2483b1b48ba2"],["/tags/Mac/index.html","4302c5fc5717b342cf6f137d74bd6eda"],["/tags/Mac/page/2/index.html","6b8be3966767c9aea1b606f07afc5575"],["/tags/Maven/index.html","7e54e1732a5f2f37323f7c30ae70ab90"],["/tags/MySQL/index.html","4acd98a97acbb558610d8a60a2b605d9"],["/tags/Python/index.html","4d7da8e1415833b2c771d6dcfb9895da"],["/tags/Redis/index.html","8971fc70a0df6632efbc8aaf41709c10"],["/tags/R语言/index.html","4a1dd23ee930e580c3eba105499938f4"],["/tags/Spark/index.html","ad1a25dd4ad3a83b3355bbba20512714"],["/tags/Ubuntu/index.html","16221f8bd01771a712b1d6c9f59eeca3"],["/tags/Vue/index.html","b6db09adf4d8b7cc0185c131b795de6b"],["/tags/Windows/index.html","65978a20ddb353735bba7b9308a3fbd6"],["/tags/ZooKeeper/index.html","dd4a23320711f7d136be0786e4313c37"],["/tags/bfs/index.html","1607809095b6ceccf54be2ad7d4521af"],["/tags/dfs/index.html","9a8bed632d6b98d905abacd8c4a922f5"],["/tags/folium/index.html","cc1469136a872f9f743ecbcc133f3abe"],["/tags/git/index.html","5f542a8f5c9c0548ba63f0ac191dd1fd"],["/tags/index.html","d1a8795fb1d3d2e3bea25d1490029899"],["/tags/latex/index.html","1679a1437c2d05d03e0d8499154d8d52"],["/tags/中间件/index.html","dc6cbd65fbef6519e86822374f77389b"],["/tags/二分查找/index.html","7c0731d0333b32e224158b54a3e5fcfb"],["/tags/优化类/index.html","979a845ed672b67ab9d6fca4078a270a"],["/tags/前端/index.html","687244a3e494ae75ac1b1fcc63782aa2"],["/tags/前缀和与差分/index.html","aa76b9e55c5681c63ce11fd9a878fbb9"],["/tags/动态规划/index.html","14ca3ab079c67a51ce3ce807d06e1df1"],["/tags/动态规划/page/2/index.html","58932a399246354976870f1499453ce2"],["/tags/博客搭建/index.html","0e9bc5a5a441ff12f429264702f2ea93"],["/tags/图论/index.html","6e6ed4968e9ad0adb9cc4a06af1a3b3b"],["/tags/大数据/index.html","882d5092dcbaad9408ab3250345e7840"],["/tags/大数据/page/2/index.html","459eb96dddff929e3c2de55daec3d61d"],["/tags/操作系统/index.html","8c6f83399af840c322c31fde47a998ca"],["/tags/数学建模/index.html","e81a9981ab118a86b8517e7511913964"],["/tags/数据库/index.html","fa403c9c93aed061411e8127c37950f5"],["/tags/数据结构和算法/index.html","d03296700a980335ec12d5dfa606828a"],["/tags/数据结构和算法/page/2/index.html","7b805d353d9b88284ac52db587772843"],["/tags/数据结构和算法/page/3/index.html","3d8a1130369c92042f7fbada5ce440d4"],["/tags/数据结构和算法/page/4/index.html","c7ebe6d06352c66ae58a7d80ff9095b3"],["/tags/数组和字符串/index.html","923c64b43d92c07c48fa4d0633de78ff"],["/tags/数论/index.html","500571cd6a5945316391132e9641622c"],["/tags/枚举类/index.html","ae05c5360c7fba3d680ad518ffcd9dad"],["/tags/栈和队列/index.html","30ec80b55991397c6b1684974cb120fb"],["/tags/树论/index.html","ee0800751314947824175a888c7d198c"],["/tags/测试/index.html","600039f6f53cb7f00e1ecbea100efdc3"],["/tags/环境/index.html","dca089d673e623b6580d7a0f6c707ff8"],["/tags/环境变量/index.html","c00f801194b0bd307e9ffa764e329e96"],["/tags/绘图/index.html","adbad2bc9e34ea2163315929a42476b2"],["/tags/编程工具/index.html","923742cc38a29f5506877e927a77f48c"],["/tags/编程环境/index.html","6e4143eb3a851d02528982bce89a23e1"],["/tags/网络编程/index.html","71a0465ebb2491163f55fba88359bec4"],["/tags/英语语法/index.html","8a12c85da7bffbf48c013dd6103c123e"],["/tags/计算机操作系统/index.html","b9641ca7a97190c86b23bb50777763f9"],["/tags/论文/index.html","af631837e3bc7cee6e4698c6c676a9fd"],["/tags/资源下载/index.html","b807219e9fe5b1d79c265ec3ced845f8"],["/tags/链表/index.html","776e6e2762378c2e3ec5d781c39bd94a"],["/tags/集合/index.html","ae0da360577c4238b8060acda452329d"],["/tags/集群/index.html","8e84cff3fa9f9a895c288965ae92c065"]];
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
