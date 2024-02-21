/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","28d348dde7dc0747859944df15a486cb"],["/about/index.html","8dfd955db854a05214fa7f6674588ffc"],["/archives/2023/01/index.html","ab548adccfb2e4bdfe472b9bd597ff56"],["/archives/2023/02/index.html","8ee9befb086d2a415e0fb64abf98bfe2"],["/archives/2023/02/page/2/index.html","e29e8495bc8affad791d777f6e976640"],["/archives/2023/03/index.html","0818e95b89e7c75feff2cb2d06a45f9a"],["/archives/2023/05/index.html","694dc690e8d7d64f3860e4fd07f9e0f8"],["/archives/2023/06/index.html","f471dd3b85c1f7937dc68affa1808187"],["/archives/2023/09/index.html","8b13740ad1fe87e4e6383021627e906a"],["/archives/2023/11/index.html","243286bb0b2704c8b605a8fc8cae4194"],["/archives/2023/12/index.html","be2dc0809a587a3ab6366f40046c09f3"],["/archives/2023/index.html","d9e6a2101a1af3110ee90fcb005f83bc"],["/archives/2023/page/2/index.html","76c1f9b0515bbc2b3a868aee3eb02203"],["/archives/2023/page/3/index.html","3b06bfcef8e831ab37153f0b5fb2b273"],["/archives/2023/page/4/index.html","be07bf2beb0a00d60ab9ccb8bbe5154a"],["/archives/2024/02/index.html","80ba21421e6e8ed1b417025ba571fbb8"],["/archives/2024/index.html","632450964343dee3f24da052a299bb00"],["/archives/index.html","4f7099f8fc7295e63bc97aa04ee8acff"],["/archives/page/2/index.html","46dfc3bda6351b5a759c93eed9bbf80b"],["/archives/page/3/index.html","d6b560726ad03fa8184127c1104fdf46"],["/archives/page/4/index.html","af3af5477a8002454f4e274c6d381b3e"],["/baidu_verify_codeva-qQP2iZOMLX.html","82d516f5d0fa66d501c33f9e1ac82fd7"],["/categories/Java/index.html","41a544008bd6e1b0d8e0fc412cd65955"],["/categories/Java/后端/index.html","47ce7fcd26c3c86ecd3bb7f91573d705"],["/categories/Java/基础/index.html","545b0c5608dd162283165a145aa3decc"],["/categories/Java/基础/集合/index.html","a832785900389bbeb74c885fdee013b7"],["/categories/Python/index.html","047103ea6077de91ca4aba0d63e3c0d0"],["/categories/Python/编程环境/index.html","b7d0f23f05b06506c503feb777f306d7"],["/categories/R语言/index.html","5e69f2b03c053b8abc2814a0b56fc429"],["/categories/R语言/编程环境/index.html","8b8e9151011475abf9ab18c4e807ffe0"],["/categories/index.html","a119e808c11c06610e0962c42e8a8e65"],["/categories/中间件/index.html","e1ca8481edf5e25cdbe3c2234d04aaf7"],["/categories/前端/Vue/index.html","29c892805d7e380d140aaca424f23e32"],["/categories/前端/index.html","3ef42fb2c2a0f3ce06dbc3d5dc34d3d7"],["/categories/大数据开发/ElasticSearch/index.html","8caa78376feefa1e95462dc6c2107116"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3d37dd0c40f2197558e75e2a29fd5d2a"],["/categories/大数据开发/HBase/index.html","953ba22cf99d40e8bf1f5cc06d218f8d"],["/categories/大数据开发/HBase/学习笔记/index.html","40a9f0668c22f46cfd1d4c2ceb6f8e7e"],["/categories/大数据开发/HBase/环境搭建/index.html","6ac90b22d1b0a20dabb0133ad8aa4090"],["/categories/大数据开发/Hadoop/index.html","a1c5b66cc704da074380c39f12bd27b6"],["/categories/大数据开发/Hadoop/技术/index.html","23f538d540adc510e0e9dd71e8e60be9"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5cc563102b44267e9891a4268fbf861b"],["/categories/大数据开发/Redis/index.html","3c67b3e27b07751c86df38a7d6224804"],["/categories/大数据开发/Redis/技术/index.html","4cb4ee988e70b6ca7a2fee476d5374fe"],["/categories/大数据开发/Redis/环境搭建/index.html","e2727e6f0134df9c63039333760e074c"],["/categories/大数据开发/Spark/index.html","356afe27164ca29fb735fc99cf6aa629"],["/categories/大数据开发/Spark/环境搭建/index.html","d8a2acb96fae29dc3d922609a96791a2"],["/categories/大数据开发/Zookeeper/index.html","767c4eb92727146ad679c0a0f5b127e5"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ec584e6eb396fc6f66de5a83c8fc8cd7"],["/categories/大数据开发/index.html","f8903c929e15a1dfeb9a916e3d5a5a78"],["/categories/学校课程/index.html","246f07fd7e4001a9aecf3a15d2e9e5e9"],["/categories/学校课程/计算机操作系统/index.html","9ff8a98d1483cd48e0c578513e913b9d"],["/categories/操作系统/Linux/index.html","112a4b379213c108b8ccb99c516e782e"],["/categories/操作系统/Mac/index.html","0483788c8fa75fb1798e83bb0e7ddaa9"],["/categories/操作系统/Windows/index.html","f80915b891bc90f5c792aaaf5d88507c"],["/categories/操作系统/index.html","cdf3f6bed2c2c150a895957fb74aa05c"],["/categories/数学建模/index.html","82c88d11413c2937dedc5e5a4b507e37"],["/categories/数学建模/latex/index.html","eabe9d19d7eeba6bf9cdc205ace65b02"],["/categories/数学建模/优化类/index.html","dcc9f161dbd2b745db3ebb022526289e"],["/categories/数学建模/优化类/现代优化算法/index.html","c65872833cdebcc24087f679fd5db42c"],["/categories/数学建模/优化类/规划类/index.html","2406c6b2075c3bfa2ae37690e578176e"],["/categories/数学建模/绘图/index.html","203d369359b668600330224ffe907b80"],["/categories/数据库/MySQL/index.html","e5f0d72a1f5af8061ff3f9ec79e88934"],["/categories/数据库/index.html","6ff3fbf0a1182731b5942e565749bdd4"],["/categories/数据结构和算法/index.html","75dff6975254ecc7f88f3cb92114ef7a"],["/categories/数据结构和算法/page/2/index.html","0131972652ce7a694c62b2786eef1ed0"],["/categories/数据结构和算法/基本原理/bfs/index.html","dcbce052d99e39a77b82c3e622f927e9"],["/categories/数据结构和算法/基本原理/dfs/index.html","c4e7035957d3460833e132d0f3177c73"],["/categories/数据结构和算法/基本原理/index.html","6c7789295a3f6c3a5f920540ee135f46"],["/categories/数据结构和算法/基本原理/动态规划/index.html","ce449ea4822812fbdb0b04fd62659020"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0f05d3a926c615b3bf6054c534a54e3b"],["/categories/数据结构和算法/基本原理/图论/index.html","1a4b0e46e516326122b4f6694d5a1604"],["/categories/数据结构和算法/基本原理/字符串/index.html","10ac7ba632d8dc5c4b522ee841aa0d9c"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","79d8c7e974b5beb6344770647b649939"],["/categories/数据结构和算法/基本原理/数论/index.html","14438e90654b4c1b26a4456ec86c559f"],["/categories/数据结构和算法/基本原理/树论/index.html","01a314d4c9ac87d8ef3dfc02c0f03e0b"],["/categories/数据结构和算法/基本原理/链表/index.html","7beda7ebdb7a377fcdcb16ebe93c532c"],["/categories/数据结构和算法/算法题/index.html","2069aabce3e000931b6c418b2213211f"],["/categories/数据结构和算法/算法题/二分查找/index.html","4beeee7ac447e2f0756e25087852d599"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c9626a0e1ad7d17f966b9898ab47f2b5"],["/categories/数据结构和算法/算法题/动态规划/index.html","c72df3aab09f8b5741f86e10e714b0a8"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","82fdb388b2614cc2ec684dbfee137638"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","e2eaa9dbc27bdf177cc44ddf4055580d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","444858aae3ebd2c3c868bdcd282b14b6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ffc1ac212c6426f29dbbef21f3225b3e"],["/categories/数据结构和算法/算法题/数论/index.html","56dc8cba5fc529fd49a1a0ceb7c39829"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5ead48cbea6bd3691cbc0c5699ea052c"],["/categories/数据结构和算法/算法题/树论/index.html","29b30935d5b567ee9389e42d955342a4"],["/categories/杂七杂八/index.html","37f9f0d3957940407270938d88112f8c"],["/categories/杂七杂八/博客搭建/index.html","079b08f890781af3114520077a545e37"],["/categories/编程工具下载/index.html","dc64f5a3600ec04331823c05750385f7"],["/categories/编程环境/index.html","baa5e0d80ed79bd1b624b4abd32cd403"],["/categories/编程环境/大数据/index.html","0edd3cebd8b1af4601741601263bb0bd"],["/categories/英语学习/index.html","095d56d85796c7915b0c7e9e45d74697"],["/categories/英语学习/英语语法/index.html","8a06f27d2b6c83b05eccab200e8a79ac"],["/comments/index.html","35329717eaee0995df3b1d2c8c7cd673"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","966f2c96a86faa171775196a0a7df493"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","8e29c10a0d9f62ded004c14f21bf5b87"],["/movies/index.html","de985828fa9c9dd7a5b48d159c7a3eb3"],["/music/index.html","81c35608d882f9e6bc3c76210997fec1"],["/page/2/index.html","50209db3c804ebaa1fe46bef525652cb"],["/page/3/index.html","b925db914884354f1204b20b0cf80e89"],["/page/4/index.html","0049715f69157b0c5ee059a2cca650ae"],["/page/5/index.html","cd948d72b6eff4df4224901afab11e0c"],["/page/6/index.html","07e0a389d7ca41000f23c6f1ef6a40c9"],["/posts/1021360842.html","2bbb303e030c800c9128ae28194d65f6"],["/posts/1120620192.html","3cfd342539b1da35c9e043f9a00850ba"],["/posts/1141628095.html","fd8ba2d78ccff2e9207609dd92d51ffe"],["/posts/1168613674.html","356364eea86dc5381902825c66652423"],["/posts/1219920510.html","4d4b9f8b7542a530df18c8ff7654c291"],["/posts/1222166338.html","f8510ac2d239fdd3d28ba5c348fd7f75"],["/posts/1259097482.html","d8e0d8a0d43dc992d71819023e166542"],["/posts/1271036369.html","965ab06aa2dd2681692f1732b10e2cc6"],["/posts/1312847445.html","9068563c6205c7fc3cfbf2738cd39654"],["/posts/135355774.html","9fad93edaa9a1a61934d343f3539f0bb"],["/posts/1375344716.html","9b5e5fe76a48da07dc751354d6946b66"],["/posts/1388991698.html","f95863554946d9e8bee90a8e0cd9ac93"],["/posts/1410315814.html","50c0f3c193b623a6602a58bd9bc5e03a"],["/posts/1452790229.html","643f14bd5bbcb466aa0b1632e25f6cbe"],["/posts/1470079884.html","c79984ca1a19816a24caf5dc11d986b3"],["/posts/1470079885.html","4758970bf08099793d9a888c4be14121"],["/posts/1470079886.html","87e6671e670a9935c01c2a4954eb7269"],["/posts/1470079887.html","84e44323cb391a55a3a8496c1c9c7aca"],["/posts/1498536549.html","54cf8270e34860f5ef284bd0074ddd97"],["/posts/1539568593.html","e263599bbcb1247b0455b06917ee452c"],["/posts/1547067935.html","a1c5a7af9bb42cb77719c954bf63e0fd"],["/posts/1557866301.html","92369d04de935c5685d557ce155a5e97"],["/posts/1571776361.html","6c4a62dbac68692132947be7542fa93e"],["/posts/1605124548.html","e92b6bdbd5270de3d8623d136964b0e0"],["/posts/1633036852.html","17569e01b5d976ab94b28828d8041342"],["/posts/1674202625.html","57bf251f4b3ea70907a378ebca0e1f5a"],["/posts/1765123828.html","c5e71d7ee63c0b7bf6adacf8e3c3599e"],["/posts/1767336200.html","07a4f138c7fcb4efb72ac5441444701c"],["/posts/1776114197.html","483766321735519adb0fdfcd92b99c20"],["/posts/1817748743.html","c75d2842131dc696ebd7c00867a59da6"],["/posts/1925125395.html","75b62ccfd59e937f588d18a1bd2f128c"],["/posts/1966191251.html","37c1bb139429f96f16ff65d3e24a6183"],["/posts/1987617322.html","0c84bc7a2e21a2e14dbeda18f8dacd2e"],["/posts/1999788039.html","3785748d76e21764d6492d0059dccaa2"],["/posts/2075104059.html","4343c06d3f9a655cf184952d4016d206"],["/posts/2087796737.html","f67d3026e826c3b15a5cf515ab08b38e"],["/posts/2106547339.html","f3005e57df2592d81cc837c033394297"],["/posts/2207806286.html","4f4de1b3d694110c69d872785ae2a7e1"],["/posts/2225903441.html","79e35188c01063de1da73eeb75d26fcb"],["/posts/2265610284.html","364fefbfcc04e4df08b1815c7718a8b5"],["/posts/2281352001.html","071cb496bcf6815ea7c3667650f072ad"],["/posts/2364755265.html","079f5ec28ab4dcd4d96eec7f09dc5a96"],["/posts/2414116852.html","e24aba768866dc528134461ff1774511"],["/posts/2421785022.html","b0c70cb4910a74dbb587e2fee489db07"],["/posts/2482902029.html","8c4fbc4e16bc93fbb258c87abde6e580"],["/posts/2495386210.html","a0edae57dbbd61e1cf55109365abf129"],["/posts/2516528882.html","c8bd0d22bfc0e6078454bf96f7fb4388"],["/posts/2526659543.html","95aef8d5e19f66c14a00a65355517d4f"],["/posts/2529807823.html","151cf8283931705fc182812df85f63bb"],["/posts/2596601004.html","9234e8e0cba4f41ff6c01c6b988d33a0"],["/posts/2697614349.html","95da91f22adbf5221304ec8084557905"],["/posts/2742438348.html","1bb50f7edd7991c8d5273f989ab45528"],["/posts/2768249503.html","72373a5418be297b47d463920a80557e"],["/posts/2864584994.html","da9451e591072056cb00ba41712ad089"],["/posts/2888309600.html","cc03607fd45146fa92683386a22c4014"],["/posts/2891591958.html","d10c7f381cbcea80e903c1684a1fc544"],["/posts/2909934084.html","0b7f0c0a540195537179d2c55635715b"],["/posts/2920256992.html","d6f547631b19d24f3ffa99d0623eca1d"],["/posts/2959474469.html","88f472ddb59b8b640107b4c07405882f"],["/posts/3005926051.html","94004ac9cd0949f442f2c2a66f036e76"],["/posts/309775400.html","1329a3a2690457adc2026874a817f3ef"],["/posts/3156194925.html","2051d7f5352e2b0cdcaf86011839d963"],["/posts/3169224211.html","7b6664a03aea1a8a194534ab4e3dcc0e"],["/posts/3213899550.html","a0d5ce53fa0b426cb329a38052b2b0f0"],["/posts/3259212833.html","489a742694abc420e0960a1ea78f9c3b"],["/posts/3266130344.html","c35a1d9daad14b9484f5e5af4d3dcda0"],["/posts/3292663995.html","30ceb090fecf63f95e524124bbd26427"],["/posts/3297135020.html","1733c2995ed217462c93e2828389f4cb"],["/posts/3306641566.html","50ff9365fb9df447610447464e443442"],["/posts/3312011324.html","b97704fb8a7fb90eb332e5c85b288660"],["/posts/336911618.html","9468ba51758f8a91c97e89ed99250988"],["/posts/3402121571.html","d5b557c10c5d9726e61a75ca6b599f7f"],["/posts/3405577485.html","831377fc5867fc90a94b3ff0144e93a7"],["/posts/3498516849.html","12b420f73100217d83eb3cc0d66d23e6"],["/posts/3513711414.html","7af088505990f1f086236c339de36771"],["/posts/3523095624.html","97ca0f3a86bae5f6c3285fbaed1615f2"],["/posts/3546711884.html","dcb8ed93c35b6b8b26cff572e0017ee9"],["/posts/3731385230.html","207f9e6cd1b06ebb9fa39c7034d88d79"],["/posts/3772089482.html","b3a7f2e02ac94355846f1424b02b53a2"],["/posts/386609427.html","09d9d66e16ce25eb3ccf1d186156561c"],["/posts/4044235327.html","d3707bec882b5bb6b2b28c5baef1127c"],["/posts/4115971639.html","03a78001c0fa8e62cf2d70a14e0e756a"],["/posts/4130790367.html","59a8761e45ef74ec618e708f3ab99aa6"],["/posts/4131986683.html","5b3e471f71f1283724c0792a51c2bf5c"],["/posts/4177218757.html","eb077be6b796551627956d3d43edf845"],["/posts/4192183953.html","49a23f35a0caaac6f27221bb5e5d7037"],["/posts/4261103898.html","4149d8cca1c42e84049c7fb40c0d69b7"],["/posts/469711973.html","41a1d6b971a8180ace5b1d62a485e13b"],["/posts/482495853.html","2ec53372868535000cbd521c58da3d5b"],["/posts/488247922.html","84fe74ba90e8d8af3983cad11fa448bd"],["/posts/517302816.html","478f46eb0c5e80d3a0fad4221757961b"],["/posts/570165348.html","fa473bb425f45861f7cd5f858c604740"],["/posts/595890772.html","280ee2f47747f81031e4109140b618bf"],["/posts/67485572.html","ce7525ba9e4d7195cb704155666db20a"],["/posts/694347442.html","da9e2623327e448d7b1dc5c202229b9e"],["/posts/707384687.html","55622405eac3132b954838b61b913563"],["/posts/71180092.html","1553d26b2960feeb99116edf71eeddd3"],["/posts/716459272.html","3ddc797ee10a171197276be9669b0628"],["/posts/765481613.html","2a8f2283a32e0219fa15253ab863063e"],["/posts/778231993.html","4529f600a9d6781d38565d61d7533942"],["/posts/795397410.html","c09a3269ce1f3a83e541edd95c15f5d9"],["/posts/820223701.html","97a43b8a4aec8d5a3bb62de36a661139"],["/posts/830372185.html","c669b9dfcc4befeb05ec4ea06ab0bca1"],["/posts/88294277.html","431417fea3cdb4ca6cd21ab86e98b678"],["/posts/939963535.html","4155ad9a7cf8beb6e7a6fbf23f869827"],["/posts/983786067.html","15f1aa09a5469490e9935b6f8314a46b"],["/sw-register.js","8db89c58a7417ff4fe5565d81a565de8"],["/tags/C/index.html","15a09113908512aab4938659ee0dc7bc"],["/tags/C/page/2/index.html","e2b7e5d08d4fdf73d5606bb7d9644071"],["/tags/C/page/3/index.html","3115e4574e2eb542f27ef02818652e3d"],["/tags/C/page/4/index.html","2b5d17f080ccf48824484efb4b4b26c3"],["/tags/ETL/index.html","1bb0a54d8d26976329ab54b165967db0"],["/tags/ElasticSearch/index.html","7ee7c84008f63e04c90b407b82d2b7e6"],["/tags/GUI/index.html","0042fb678d21d38b7d049e0ee6c218dd"],["/tags/HBase/index.html","7e97ed86bd205a3b84849e1f4260ca17"],["/tags/Hadoop/index.html","861a6ed32b8b2a615a98f3cbe61148f9"],["/tags/Hadoop/page/2/index.html","64550e6b71859164db44be3e2f7fb150"],["/tags/Java/index.html","ed3a8e6abd65110f8e6bc3fc75b76a2d"],["/tags/Java后端/index.html","c39a3f3ea09c979120f1b679a3b2dcaf"],["/tags/Java后端/page/2/index.html","4b41f9a6b8d52b31e2be116540b5208b"],["/tags/Java基础/index.html","87382bb0d2b10a4e82bebf2c365cc5fc"],["/tags/Java基础/page/2/index.html","eca5d780368f2778d68c920b9adb50b2"],["/tags/Kettle/index.html","6e2704757bef4192d87aa6bc62c52519"],["/tags/Kibana/index.html","b4d65c3bf364e52fb827b9a9c6caca26"],["/tags/Linux/index.html","b1865d5f920af74b0bea75fc8254f826"],["/tags/Linux/page/2/index.html","01771e66025845788acfb75875edd6a7"],["/tags/Linux/page/3/index.html","85eb295d4ddc2a89079977671d063c78"],["/tags/Mac/index.html","9ca74575ae2909443b35c31028634c9b"],["/tags/Mac/page/2/index.html","9bdf8243e55b088eb59d5b5e29ccde6d"],["/tags/Maven/index.html","f2f2680ea0a6bd1bd17478cbfac2bba2"],["/tags/MySQL/index.html","8b90f219ae3086bbd03d037c68f0b6a1"],["/tags/Python/index.html","ae298c64b0e9926fad09c9e128f661ce"],["/tags/Redis/index.html","f107e57e78f190d6a2b2b51f7a62e71d"],["/tags/R语言/index.html","7f66083368de80f73d463ac68baeb54d"],["/tags/Spark/index.html","8200fc6d779da69fb81d987538b4cd67"],["/tags/Ubuntu/index.html","d3809337a422238f6993b19def946319"],["/tags/Vue/index.html","75cf535e7f7ed4e3f07a122859a73b19"],["/tags/Windows/index.html","43dbac04f5edbc9543c987315f6cadcc"],["/tags/ZooKeeper/index.html","a5c6d61289a16957b33a9ef0d3289119"],["/tags/bfs/index.html","5a5c401a2a354e85fbbb551cf16e4fcd"],["/tags/dfs/index.html","991aab4c494a1e02928adb4f583f5e3d"],["/tags/folium/index.html","605df2bd876ff6b4b9d2733f7089a7cb"],["/tags/git/index.html","ccc59ff57ebdb6a181aa56c083b9bef4"],["/tags/index.html","a1772972f01ad520b6bdc5b9e4a9105f"],["/tags/latex/index.html","40f70d927c12db54944cab4845db7861"],["/tags/中间件/index.html","092f1208ed1ccb268391fe6e8bcbfd9d"],["/tags/二分查找/index.html","88a8aa3c92b2d8e7d90afc7f306ca20a"],["/tags/优化类/index.html","20b0607441685623d55a23371e89a332"],["/tags/前端/index.html","6e065b8a21c2b56471736144007af77e"],["/tags/前缀和与差分/index.html","cb4fc8598dde54eb6287c3129e2ddb68"],["/tags/动态规划/index.html","e3682abdf079da2741d8e6fef48557ee"],["/tags/动态规划/page/2/index.html","9239c7cceac37c91cf4c6a2312679ed8"],["/tags/博客搭建/index.html","ac1448dac5239d7a18bb5239c942bc2d"],["/tags/图论/index.html","deeb53f419c1ea58a0ca4c8c880fd20c"],["/tags/大数据/index.html","c69c61b406f33389630a01bb9038b4b9"],["/tags/大数据/page/2/index.html","faf46e3d7c7aeb5415197bf4e4a1d900"],["/tags/操作系统/index.html","40546dd0e89000685720d44d0261735c"],["/tags/数学建模/index.html","01e6af1719a0fc3a6bdc60122a00f894"],["/tags/数据库/index.html","1aac664cc9db3e92725af7f9961ed78e"],["/tags/数据结构和算法/index.html","983fdeb02f2d3a737ee51991d4f6c9b8"],["/tags/数据结构和算法/page/2/index.html","b6dfa7f919d572df863c6e8fb7073294"],["/tags/数据结构和算法/page/3/index.html","54bd488e32f36c7189611187a6d56c53"],["/tags/数据结构和算法/page/4/index.html","71b09e246d0a343b35945831fc338dfa"],["/tags/数组和字符串/index.html","be23da7cfcafe4ff244e5647e28f4606"],["/tags/数论/index.html","148b933796b97fed4eb5bdd072307c35"],["/tags/枚举类/index.html","ebecf471892fad7856b9875e80440cfa"],["/tags/栈和队列/index.html","1d198dd56c9c82378410a27733142580"],["/tags/树论/index.html","93f04da30f5ed199ccc9f4588295d5d8"],["/tags/测试/index.html","07851e0e5654998a03597400a31f117f"],["/tags/环境/index.html","e23d2250176af5205d490c6a763ce429"],["/tags/环境变量/index.html","a5038d0641582124c6b014f0d7eb8883"],["/tags/绘图/index.html","41ceff1aed78f5b2985ae7ec8278e5c5"],["/tags/编程工具/index.html","8cfeb482da5ff95c2c9ad8ba98493724"],["/tags/编程环境/index.html","293ead1648c96aafb16cb66aefdae115"],["/tags/网络编程/index.html","115ad097bdce7a897e1cfe641f9d1d69"],["/tags/英语语法/index.html","a656fd4fdfd96ab7997caca728ffbd6f"],["/tags/计算机操作系统/index.html","2c56fdb1336cf9664e05e2c656d170a4"],["/tags/论文/index.html","0e18905d402eef820fb2ba96b455a439"],["/tags/资源下载/index.html","593653af98c87875d227dcf1056f11b4"],["/tags/链表/index.html","c235e6b7451dee23d14d137a49038190"],["/tags/集合/index.html","8c251d47831f7ca773f60605a77dbab5"],["/tags/集群/index.html","af58286105d9232f67e44d4e6da30e61"]];
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
