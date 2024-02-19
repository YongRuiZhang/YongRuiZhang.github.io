/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","161298a99f47704f41061038fbdb71b8"],["/about/index.html","26ed1e10055323e1ba20e33c1d278e4a"],["/archives/2023/01/index.html","392a13322983aeebc3b2bf70517fa848"],["/archives/2023/02/index.html","01a4b34f140025e226300eeea43026ad"],["/archives/2023/02/page/2/index.html","7c9a22bb320cc1bd3fc7c1f6d7dc8f20"],["/archives/2023/03/index.html","583cefcd7a34b68adc8c7ef09e1abcef"],["/archives/2023/05/index.html","4e0e7aefb708786ab4602cd28d703c39"],["/archives/2023/06/index.html","f5516425b01b4cb23bf3347773cad637"],["/archives/2023/09/index.html","37d22435194bf7daea802a678052a592"],["/archives/2023/11/index.html","508db06bbb63111b6e3fdba7ffc4305e"],["/archives/2023/12/index.html","376da0e7070a0ebc93939c60717fffd2"],["/archives/2023/index.html","4a62709afd19a5436c69eded6cce99b6"],["/archives/2023/page/2/index.html","d2f39f6a52badc80f43c384991dbc9af"],["/archives/2023/page/3/index.html","0199abbeaca3efc29c1be397306b310c"],["/archives/2023/page/4/index.html","21b0e036596588de2d10b35971e9ffed"],["/archives/2024/02/index.html","0aa365d31c160b3c340f33b5b36c7907"],["/archives/2024/index.html","4ce7932356ce411371476251acec6c72"],["/archives/index.html","1076204eb6a1cca17d46a8f0f1ccfcb1"],["/archives/page/2/index.html","11fdea15f48ad9f6c3a6e82e168d1343"],["/archives/page/3/index.html","6272d6f80394bd3743e098a6ba7832d2"],["/archives/page/4/index.html","96686e0d7ba6f24a152b9975be244a0a"],["/baidu_verify_codeva-qQP2iZOMLX.html","cb40b9b982f77cc5f59ce657f55a8fcf"],["/categories/Java/index.html","27317bcc15bb101018dbc7b9f846e28e"],["/categories/Java/后端/index.html","8be87628e4f0c16dcbf78c0ee2417329"],["/categories/Java/基础/index.html","34c2de9593ef090fa82f3c6815bfd425"],["/categories/Java/基础/集合/index.html","8430b528afe941dbf8116a51eddbc180"],["/categories/Python/index.html","e9c741377c6e4032e312e9a4a4eb4b5a"],["/categories/Python/编程环境/index.html","8f242826a71d48881ad5396eb68c7d3e"],["/categories/R语言/index.html","1153bfbb006a56c68e39e121b2b96b5b"],["/categories/R语言/编程环境/index.html","70ae670531b67f498edc30a58fa310e8"],["/categories/index.html","4f5e3e536a28b7a8e3212c00c2825d2f"],["/categories/中间件/index.html","939ce823888c87da8cc7ea8127e1c234"],["/categories/前端/Vue/index.html","3b79039f8c2612d2578930df6aeac125"],["/categories/前端/index.html","9c3494cf7d99387047b8edae87845a34"],["/categories/大数据开发/ElasticSearch/index.html","331914d3385a1033f2fb0d221537b774"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","586cedd92e8317bfeb5ec320d956827b"],["/categories/大数据开发/HBase/index.html","6e04b64d8645f805e68591e0439e4117"],["/categories/大数据开发/HBase/学习笔记/index.html","653e1885a8d281c9634ad8473dfe1bd7"],["/categories/大数据开发/HBase/环境搭建/index.html","f99b29357acfa672a102849dcc13fa40"],["/categories/大数据开发/Hadoop/index.html","7ad9c37923373020ac1bd8a64fd98065"],["/categories/大数据开发/Hadoop/技术/index.html","104e7607ba42946e1c413db289d1fae9"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1cfe17fb93d6c6e6589115a56653ef01"],["/categories/大数据开发/Redis/index.html","9a02ffb611e79b68640c6c9b8d0570e0"],["/categories/大数据开发/Redis/技术/index.html","11799ba3bd870884699d6d5216f34486"],["/categories/大数据开发/Redis/环境搭建/index.html","bc992e5a7110cb9b301522a3066abea9"],["/categories/大数据开发/Spark/index.html","bb835abef46440de713dbfdaeefb4024"],["/categories/大数据开发/Spark/环境搭建/index.html","8d3c4f50c7e9657e3649d4ced054c40c"],["/categories/大数据开发/Zookeeper/index.html","f198bdd70c6d802379a5e1cfa4b4891d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8c97dcd0f4ea62065e2afc7f86255ef8"],["/categories/大数据开发/index.html","a7cb8c6f7c413c81259f45cd05ef9228"],["/categories/学校课程/index.html","036470221ead7df416a79d46eb90e152"],["/categories/学校课程/计算机操作系统/index.html","047f515487a584b1a9e225d6f9d31ca6"],["/categories/操作系统/Linux/index.html","88335c610240335392b03a5c07717210"],["/categories/操作系统/Mac/index.html","466ec72a3418ae18e46c5de9c8f92977"],["/categories/操作系统/Windows/index.html","0ed4154116b253202eba2d0c7ed7a6a5"],["/categories/操作系统/index.html","eab696ce5ebb0196ac680fcc4701a13d"],["/categories/数学建模/index.html","cfc044f7bcd2fe57969e7a0fa67d22bf"],["/categories/数学建模/latex/index.html","9ff7333ec945593ffca9be1f21bfe756"],["/categories/数学建模/优化类/index.html","6590cc0916d1284e9a581712390df207"],["/categories/数学建模/优化类/现代优化算法/index.html","7f474e9e2afdb4d15202da0168cf87a5"],["/categories/数学建模/优化类/规划类/index.html","cd15fe4138d003abb103dbda620d5f4d"],["/categories/数学建模/绘图/index.html","c6d0ba80b8866231a73d2a36852e91c9"],["/categories/数据库/MySQL/index.html","10380c0839cdfb3102937e022eed723c"],["/categories/数据库/index.html","d14ebede878aaee1ea61fca00694fde4"],["/categories/数据结构和算法/index.html","4a76c04b48edab8eb849c2f009d4325d"],["/categories/数据结构和算法/page/2/index.html","28460a88b3e8d2df7f46612eab1fb445"],["/categories/数据结构和算法/基本原理/bfs/index.html","3e6aea771bca818cb3e02d3c99eab31e"],["/categories/数据结构和算法/基本原理/dfs/index.html","8c65c7e4dad7a831c296c890c722d0fc"],["/categories/数据结构和算法/基本原理/index.html","39f15a3f44650221b19c48217122711c"],["/categories/数据结构和算法/基本原理/动态规划/index.html","7492b31d119812b8cb0540ee19def457"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","7e201163e854970cebe2a787c29a9467"],["/categories/数据结构和算法/基本原理/图论/index.html","4bbb78425d0b3426c25d8e90befaf893"],["/categories/数据结构和算法/基本原理/字符串/index.html","c4c48dcafe6a1705779734e532cb2d8d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","4b919e0ebaa69cedd66809d628ec92ee"],["/categories/数据结构和算法/基本原理/数论/index.html","29d69f7b16b8dbc500e0c693389cac6c"],["/categories/数据结构和算法/基本原理/树论/index.html","6cdb0a92ba6b6f5211e782683337d246"],["/categories/数据结构和算法/基本原理/链表/index.html","57319b0b5634020b735a3138e2dab211"],["/categories/数据结构和算法/算法题/index.html","e76d5c59918a8e7e36ebf56a1a53ddde"],["/categories/数据结构和算法/算法题/二分查找/index.html","bbcdff0be00fed1ba2cdde9811414e65"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","a99c9bea737fd7747f64b459408e4f5e"],["/categories/数据结构和算法/算法题/动态规划/index.html","814ef1e8e2baa621b262aede1b7380af"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","f30ef9d97f0e1a3e5a742df00ee96c80"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","60c123925c1e874e22240e5feeacee8a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","7de8b0a04f162adc7e79c1de677ad033"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","0cb025115ee8d41ca35fd7e3a0aa0d86"],["/categories/数据结构和算法/算法题/数论/index.html","38a6dcde7ed5474f29466fe5d93f39c5"],["/categories/数据结构和算法/算法题/栈和队列/index.html","47df4de1e8c5c9b0da2aa74fa7e2abc4"],["/categories/数据结构和算法/算法题/树论/index.html","e3d000934c3105b8041946eab858dc9b"],["/categories/杂七杂八/index.html","af46d43fcb528843d57d95ff3bd5cd38"],["/categories/杂七杂八/博客搭建/index.html","b50d766fab554af2160d2810d51a0d7a"],["/categories/编程工具下载/index.html","da6a80a8de623bdba85faf95f3c833a3"],["/categories/编程环境/index.html","650879bde717a1f08ea9fb12c32af6ee"],["/categories/编程环境/大数据/index.html","91cc2c8c3de5655d2435092b6945d6f7"],["/categories/英语学习/index.html","c30e2e83379fce3294ffc9459c5c3d0c"],["/categories/英语学习/英语语法/index.html","9ae1124b6d34bf973a68246847261d81"],["/comments/index.html","1b7d4188024ddbf64d2006d4c990513c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","294d70dc9719b0f575dc6d44e92ad42f"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","f601112049d1dc4f6096844ede7e7657"],["/movies/index.html","4290fbf02e62bce39aa36b8bdd56ec92"],["/music/index.html","a6e43ee13248f483e22049a18044fa16"],["/page/2/index.html","51ac50a0dd6e67f9082378f0a8c8a377"],["/page/3/index.html","38736ef3fb4461f2d4ddf35a8c73b8be"],["/page/4/index.html","8d1201f7789d9a2ca3b49729714b0284"],["/page/5/index.html","61861122a434f7b033d35e234443c347"],["/page/6/index.html","314795a01f98bc61e6c2d27e42e2dbba"],["/posts/1021360842.html","aaaaf1f91060ddf49f371ecfeb0aefce"],["/posts/1120620192.html","d3cb31cfe300c4e379487a1082140976"],["/posts/1141628095.html","944d6921d492fce49b79aaca85a6e816"],["/posts/1168613674.html","14c95b2a43370711bcf66128533785c8"],["/posts/1219920510.html","43cc3145eb6d515845d4b80c3c7f956d"],["/posts/1222166338.html","69b61838c8c06838fdfdc7a66be6ed3b"],["/posts/1259097482.html","3ad2c4035c9ab24d73b9e5eb0eb60310"],["/posts/1271036369.html","16617ac15b63685fccda0eda79112aac"],["/posts/1312847445.html","ae8e4fbbf05c9dfc7d987e228947f470"],["/posts/135355774.html","8d91dffb21a6f48a579c708a82e51a1f"],["/posts/1375344716.html","9f72d6f093ff7d1b1eaf0522edcbfbbe"],["/posts/1388991698.html","5e8b32dce54685493210e18a7fb5b7da"],["/posts/1410315814.html","633b46a512db0a7abf22b8167a5435e5"],["/posts/1452790229.html","18e20665ec6ed7b2b78091cab9786532"],["/posts/1470079884.html","9917ecf7b466a98832dd9b5a1b97785c"],["/posts/1470079885.html","cd3b5d9f23bc42a96b343f52f1cd81eb"],["/posts/1470079886.html","eb9b67cbdfa9aae0ba6927fc4e7a34f7"],["/posts/1470079887.html","0cb9a84b85078d8bf31502ccb91c7582"],["/posts/1498536549.html","e8686af46154e564bddf91152206fe65"],["/posts/1547067935.html","17187370095b9c7ccaede86f19b9674a"],["/posts/1557866301.html","0ca01743ca63d3f94e283cbced012972"],["/posts/1571776361.html","41dbe4ee0f528387ac68cf8b367897c4"],["/posts/1605124548.html","5d03663e6c8eae1b348f144ae23939d8"],["/posts/1633036852.html","e5738f41b3422c67bb5edaeba3121171"],["/posts/1674202625.html","39b4ca399e2c6e524fbe87ae7e0ac5af"],["/posts/1765123828.html","24e43291ba1b06372f5de3dcfeadcc6f"],["/posts/1767336200.html","50bf3c09d121748ddec0dc39c1a2d436"],["/posts/1776114197.html","ffdae643938ba005af2842bc8c9bd7f3"],["/posts/1817748743.html","cef2373c03cd591c5cc0e82b8a431350"],["/posts/1925125395.html","65e7baf492af75003653aa4faca8d500"],["/posts/1966191251.html","75c3eec60c11926c5a038c1f66b8192f"],["/posts/1987617322.html","2c6a1e76d2ee545451f4bad2f58e7d2d"],["/posts/1999788039.html","186c2b64f32fdfa1798a58847d0a303c"],["/posts/2075104059.html","8f5740a3f153af86f74b3e7cbea8c09a"],["/posts/2087796737.html","e873e3a26a3473113f5c0db06e8fdf89"],["/posts/2106547339.html","b005a941800ee118dbda6bea56fb3e1e"],["/posts/2207806286.html","7942ee77204d89c77978c383c60d141f"],["/posts/2225903441.html","35caca01cce2142244fece41876f9fcb"],["/posts/2265610284.html","0ec5e6151775601a3555bc63c57e82c6"],["/posts/2281352001.html","08adc9f17fe6b69c48e178a55fa1bddc"],["/posts/2364755265.html","9be588fe3890dd7a962191163def5adf"],["/posts/2414116852.html","48c10c332c3ec83b02d2b7b66e75b30f"],["/posts/2421785022.html","0f7300d375dcf32f66c7e581de1d765b"],["/posts/2482902029.html","589b6837c8bd2c41bba34fe43fbf4fac"],["/posts/2495386210.html","f1f85eaf7a36ce9420ca83eda47079c4"],["/posts/2516528882.html","6c9eb377b018457d520ab48ef1fa66b4"],["/posts/2526659543.html","e9246dd52c39a97bd0d63c4b0ede6edf"],["/posts/2529807823.html","f56799686df6470326d4ba515d98a5e7"],["/posts/2596601004.html","5a7f208134a88d1012394c0bf0f40cc6"],["/posts/2742438348.html","c435575f66295f5d6ea3e38359b16791"],["/posts/2864584994.html","d72d4fc175653e17280a31dc576d2450"],["/posts/2888309600.html","7bae066a1af71b854d15767bae8ed55e"],["/posts/2891591958.html","bde47a6e6166f7789335a17b4797f11c"],["/posts/2909934084.html","5c3abd5c0480955af3862f83ee5b825e"],["/posts/2920256992.html","83616d122f4bd74edc87608be791c7cf"],["/posts/2959474469.html","1a242c83c723d26d14f4bc8e7f0211b2"],["/posts/3005926051.html","4c8ae6a2710e385077e7e187d2f6eaf4"],["/posts/309775400.html","aab53a188acc22b33b9269ead9aa3ab3"],["/posts/3156194925.html","1f09cb68e3d6ed803a161ee6a3643aa2"],["/posts/3169224211.html","7602f86e4a8e958f4966e395fb1ca8f0"],["/posts/3213899550.html","b95a5a97bb85f6fb8e77f3b1d450fca8"],["/posts/3259212833.html","215614a000f1f74aa7ce70697f504ac9"],["/posts/3266130344.html","9b4f3e7c1c0cdefb1a96f1563d77b199"],["/posts/3292663995.html","d989a0f87cab7c86f426a7e110971289"],["/posts/3297135020.html","5255943def970af2cdbb3aacacdbbb32"],["/posts/3306641566.html","2fe2b9f6224fff819a9ec92a41d9a67d"],["/posts/3312011324.html","986b7a92e6f653022dc9011741fcc9d2"],["/posts/336911618.html","402e05051c7965ffcf4a64aa901b48d7"],["/posts/3402121571.html","dc4dafad33372d7a112c0dda82b6187b"],["/posts/3405577485.html","8ef9405788258b4bb0ed0d2182a3b1ec"],["/posts/3498516849.html","a025b9d63a5ab8b78d77ebcf3600e327"],["/posts/3513711414.html","74c24488ea5af066491d0f02a17b1888"],["/posts/3523095624.html","f035a53e29743c3a79fb3bb03de1399e"],["/posts/3546711884.html","7ef301654ff6eb262dcc7b6eaee2cfb7"],["/posts/3731385230.html","354fc235e98e4a0074ccc5fb78e0b900"],["/posts/3772089482.html","f13f3a6155d8283a475f1d405cfcd66e"],["/posts/386609427.html","04a6884fc958b63c24cf29c79a643511"],["/posts/4044235327.html","f186698c721280e44146a7f07226dbcb"],["/posts/4115971639.html","fa3adbb7a5c7a75af6913f77888c44a3"],["/posts/4130790367.html","54296cadbea6ff582c9f3ab335791c41"],["/posts/4131986683.html","991ef14f6038e8f3b92080ccc316d1ff"],["/posts/4177218757.html","0dbeadcfd7e366fe222d5a2baedcff25"],["/posts/4192183953.html","32eb2781f740c20cc3a9a9b8c8ad9741"],["/posts/4261103898.html","57f24e1ae3313f03a534203f37e7172b"],["/posts/469711973.html","234a3603185ea2a136686e33e6caf273"],["/posts/482495853.html","d276515eaef13a9a21117eb0c4580b3e"],["/posts/488247922.html","2d1818a83fd804b8189e0b8a42fe78a6"],["/posts/517302816.html","33acde3e005f73c5ebc044245c6f997e"],["/posts/570165348.html","d5013307cb20a420bf342dbdd3f04403"],["/posts/595890772.html","bd6d71ff58c58b9a17a658940a3d9a0a"],["/posts/67485572.html","0eedec4ebadd3df5a4554aac5cbc9934"],["/posts/694347442.html","babaf8c583a7aa8896d52a9af724e30a"],["/posts/707384687.html","af46a309a5de7c9b73dbcdd536765466"],["/posts/71180092.html","508fc573e5d51ac7abfa167b0d8a89aa"],["/posts/716459272.html","cf2b82dff8a6f141b161be24c5f47c9f"],["/posts/765481613.html","9546872e0228921c1bb4773c5cea7f67"],["/posts/778231993.html","eda474bb70e660c9075deefa2a4791ca"],["/posts/795397410.html","e63780e75400daefea961b260d55bbd9"],["/posts/820223701.html","be438a8149fd437103f34a3ac6dd881d"],["/posts/830372185.html","80e18ed86cba15f8ce038ddd1fa76ca5"],["/posts/88294277.html","1f3d45ed7258bd2ac91bae762a270f3f"],["/posts/939963535.html","a3a714b7de487e793d1eb8d1b056746b"],["/posts/983786067.html","20f9d32b970dfc63434eb8a275333524"],["/sw-register.js","0ffd0b57c4de8cbae8d33ae1d1a5f7bd"],["/tags/C/index.html","f8a1d1b4d0950b56df7a2513a7d18783"],["/tags/C/page/2/index.html","8f2c90e68c7f1b67ccdf50b53e945e5a"],["/tags/C/page/3/index.html","f614febb48b346b953fb0436a8b320a7"],["/tags/ETL/index.html","8cf298a627b7c63f9fd16b2725dcd7b9"],["/tags/ElasticSearch/index.html","79764e376d4244b9b70879294e0dc6c4"],["/tags/GUI/index.html","d2d607f20f4971efcb72201196637492"],["/tags/HBase/index.html","3c3a4204a57dde817a9c9185cfd44d05"],["/tags/Hadoop/index.html","0d83beb0cf28cbd7c563552ab476b096"],["/tags/Hadoop/page/2/index.html","59396b2c4e57579de3eed6fcc168d409"],["/tags/Java/index.html","c71346d6012c91cef415492cbf849d14"],["/tags/Java后端/index.html","78a23637747f4de6805822656110f77d"],["/tags/Java后端/page/2/index.html","b5b121b4e8fbffe70b1bc40588b3aad2"],["/tags/Java基础/index.html","44e017c10e479a314db08ccc200d757a"],["/tags/Java基础/page/2/index.html","61579e4b512498639068389597501d4f"],["/tags/Kettle/index.html","4df239b10e790418cc80692975940c68"],["/tags/Kibana/index.html","6d624b1cde8dee408b740023e774e9b3"],["/tags/Linux/index.html","d4837dabfc5f06128d0d79307a475876"],["/tags/Linux/page/2/index.html","1e45db95947e5df34840e2442ce0ba46"],["/tags/Linux/page/3/index.html","f59ce0bfed469797bf6851537fbd51be"],["/tags/Mac/index.html","7b5093fb871f25f3586881b53038cfdd"],["/tags/Mac/page/2/index.html","3725bc309235ff779158e0ed4a95739b"],["/tags/Maven/index.html","fbe70b892c776920960474b46a26030e"],["/tags/MySQL/index.html","4d7be54f3ff0975ba395436a6bbfb7e4"],["/tags/Python/index.html","4d1376094b3ed1c4dae3583530a6acd3"],["/tags/Redis/index.html","cc85022fc4f83a6bc2f6fbd84d8e75f3"],["/tags/R语言/index.html","ad3a4679350449f760ec6f045c4df285"],["/tags/Spark/index.html","40670d97eb8eee9d466d04473ee2ab9d"],["/tags/Ubuntu/index.html","8a983aa240422d23b3c96f60a359edb3"],["/tags/Vue/index.html","79b52400efa7ff3ca9666b01c1f55a3b"],["/tags/Windows/index.html","7e6cdec01befd87078b84acd91ce9dc9"],["/tags/ZooKeeper/index.html","93fd33413d288dcd20aa9db95395e1b6"],["/tags/bfs/index.html","b600f8e928166d776136afc002536e9f"],["/tags/dfs/index.html","96e51b6d69b5703beabdb26b21ca69b2"],["/tags/folium/index.html","2b2a286bf3f3d605655cbea259d36c51"],["/tags/git/index.html","7fcd8b2b0c4338005ae02ec361136030"],["/tags/index.html","63c1a1354b323121fffb49bce5692b1e"],["/tags/latex/index.html","1e30509f8f32164589abdc4c282c0743"],["/tags/中间件/index.html","2dca0122eaa82486adcf83fe8669dc24"],["/tags/二分查找/index.html","67f266263c4470b5e2960322b46d1edf"],["/tags/优化类/index.html","c78117f31e3c7b76e22894d31dffdb47"],["/tags/前端/index.html","7883f725e9662a5eca7f6d1a15cc92b0"],["/tags/前缀和与差分/index.html","34b543af6f1a51873bee9be265314c1b"],["/tags/动态规划/index.html","caa42b293859a5421bfac58cd17a820d"],["/tags/动态规划/page/2/index.html","eb08c7d689a6736654aca046f5d6148d"],["/tags/博客搭建/index.html","ef3f6628d4507eb47cf0f941112ec24e"],["/tags/图论/index.html","79cebd60638ed7e11b4ec51cce981974"],["/tags/大数据/index.html","0037e61c09c19d3e4315441708a06a93"],["/tags/大数据/page/2/index.html","004261a8271807cdd629e189fc9eecb6"],["/tags/操作系统/index.html","90df405161a2625375bc5e413bef3122"],["/tags/数学建模/index.html","07b2ef7ddda84d9405e210aca9095cdd"],["/tags/数据库/index.html","a04137556b79252f77ebecb3d7ed31b1"],["/tags/数据结构和算法/index.html","5701abc5247bd5b2e39b48b34c362fc1"],["/tags/数据结构和算法/page/2/index.html","ada6af8fa995e2cd170ec9aad98701c8"],["/tags/数据结构和算法/page/3/index.html","2cfae1fcc970eca52ebdc0e6b7a9110a"],["/tags/数据结构和算法/page/4/index.html","cb9306c61bbf376009dc30c36ddc265b"],["/tags/数组和字符串/index.html","197a4a285dcdcd33df210d07f7fd536c"],["/tags/数论/index.html","841c880ed0854e667557b4f8038ab846"],["/tags/枚举类/index.html","fe05db63ddecfe1b5a49492ce465541d"],["/tags/栈和队列/index.html","be4f2b1d5cc3189ebe7f4f23dd705c37"],["/tags/树论/index.html","f25ab72476c6bd9e1e13b68d37487acb"],["/tags/测试/index.html","8457edc9116caf07724152bdfbe24d75"],["/tags/环境/index.html","ecbfc3f163623ddc9814458c85810d2e"],["/tags/环境变量/index.html","0e79a1db441a9aae6b0970588d2cbdb8"],["/tags/绘图/index.html","b7c95150bc1d80081bf822baf15410b2"],["/tags/编程工具/index.html","9ef86849462a8ced825efe53dc383d19"],["/tags/编程环境/index.html","163453f703aa73a1f0fe760de3d5ec98"],["/tags/网络编程/index.html","7484eb21640e37f61249227989056a31"],["/tags/英语语法/index.html","990cba39a0cdaf035bea000f7bb23134"],["/tags/计算机操作系统/index.html","57eb1ebb290ed0bf171f37dbfd77a12d"],["/tags/论文/index.html","0e47dec1fae6e80536b798858d403add"],["/tags/资源下载/index.html","65f9f3ce1be3e0d478889f7eeaa9d934"],["/tags/链表/index.html","497634e6374094503764467123e5fe23"],["/tags/集合/index.html","60e6ecbcd45ea7b9c95f7e193435028a"],["/tags/集群/index.html","c7441ae50cd191c86cbea6645c6978e2"]];
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
