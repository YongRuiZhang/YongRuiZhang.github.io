/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","bd222a75433797c08270404c3fe5d1c5"],["/about/index.html","f465716816d784712632c457ac571bf6"],["/archives/2023/01/index.html","746b45d8abffbe1ba6bdc6da081d55df"],["/archives/2023/02/index.html","390f3c5ce59d51041fa006e8fd65a7c5"],["/archives/2023/02/page/2/index.html","fd26bcca2385e1797aa6493d73abb9fa"],["/archives/2023/03/index.html","fc71be14595f4f34451bbdf026b0fb7d"],["/archives/2023/05/index.html","e32b9d41b6ffb97325338d4699241df0"],["/archives/2023/06/index.html","74413a8e88361c471ab52ccb3308ebfc"],["/archives/2023/09/index.html","ac4e47d033397136ba7b920cae03da62"],["/archives/2023/11/index.html","2349ca32b5cb5c2344302390a027b61d"],["/archives/2023/12/index.html","e806902e7831bcb73fdcc64af621ff6b"],["/archives/2023/index.html","5c7a3bf0546a8992f3af001556a85562"],["/archives/2023/page/2/index.html","95043b37a40220ec8ebe5aff453953b8"],["/archives/2023/page/3/index.html","f73666893d2b516beb1229fa5082b85d"],["/archives/2023/page/4/index.html","a0149bdfddf2271cd5a9c6a5987dee3f"],["/archives/index.html","76df997adccb5b38456f1291942ac5cf"],["/archives/page/2/index.html","83de1861bdb6f2bb2b47522144cf7d58"],["/archives/page/3/index.html","cd39337efec80c4c97e03019917dccc8"],["/archives/page/4/index.html","3fff62a4d074acfd926d2fb27c8d1045"],["/baidu_verify_codeva-qQP2iZOMLX.html","21960a15c813c81f4b430ddf295841c4"],["/categories/Java/index.html","06bd019535a8d446181f79e3c26626b6"],["/categories/Java/后端/index.html","e6c43b503030339f3811f9c82fcc5427"],["/categories/Java/基础/index.html","6b61d0bd20361a39b569d0439d26fd32"],["/categories/Java/基础/集合/index.html","dfb5545ff5cb8e2aff2006123a95f598"],["/categories/Python/index.html","c6347b3a26c21013bb515ac9c08e294d"],["/categories/Python/编程环境/index.html","435490262c07b4d05075abf3fdc0a826"],["/categories/R语言/index.html","7013615b2030be4e287f9b67a7afd9e6"],["/categories/R语言/编程环境/index.html","31204af1b960e785a74ca3481215c979"],["/categories/index.html","9ec87183891035004a32dfe6ca4fd79b"],["/categories/中间件/index.html","d0faa3ac11e993e46d7bc4268d3d5bf4"],["/categories/前端/Vue/index.html","a18ddb1bcf87c33c4e9672318c8789a2"],["/categories/前端/index.html","7bcc1c083d4ed373089cb59fc968697a"],["/categories/大数据开发/ElasticSearch/index.html","6f1c1d2351ad91fcc4c8aae3e8ab349c"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","517d5b31a824b6f8a77f274e2dccd538"],["/categories/大数据开发/HBase/index.html","0f802c7c77d3648213d72797770885f3"],["/categories/大数据开发/HBase/学习笔记/index.html","7dbd5442771d4a247bf85c90fb87438e"],["/categories/大数据开发/HBase/环境搭建/index.html","56beea8e5be02854777ef40ec8a0f9d1"],["/categories/大数据开发/Hadoop/index.html","0ba051fcb1f21daa61fabba1653bc7a3"],["/categories/大数据开发/Hadoop/技术/index.html","b79792c335fbb6c2a6d79b03cb06e525"],["/categories/大数据开发/Hadoop/环境搭建/index.html","20803cbe81f715d5d8bc2838836d44c2"],["/categories/大数据开发/Redis/index.html","9d39a65d89715e3021fb4b195a39889a"],["/categories/大数据开发/Redis/技术/index.html","c5c99be90256e0094cba8f39abdec7af"],["/categories/大数据开发/Redis/环境搭建/index.html","f30cc61c019a163d154a6ba665242fcc"],["/categories/大数据开发/Spark/index.html","1523eccd05dd0fddbf23213bbc1c990b"],["/categories/大数据开发/Spark/环境搭建/index.html","79084c40c3644602b585a6c95524cc23"],["/categories/大数据开发/Zookeeper/index.html","90207e0095e1d7381a7949fd5ac94290"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f3dfb27f3638e6d261613356d595d64f"],["/categories/大数据开发/index.html","02d15e7d095bfbdc36eb263bf455cc22"],["/categories/学校课程/index.html","33f09c3059a1db01c7148426453d48c0"],["/categories/学校课程/计算机操作系统/index.html","03e172666941c57c870770c433c8cd4a"],["/categories/操作系统/Linux/index.html","baba039e6e09d966897c03b732abd291"],["/categories/操作系统/Mac/index.html","dd841d3e813fe89e98251e81d73574fb"],["/categories/操作系统/Windows/index.html","b5184fad19db2bb15de41bffda7d0259"],["/categories/操作系统/index.html","30112d6e9995761e8a50a279119f217e"],["/categories/数学建模/index.html","c7d5b8f84eec568860a251af4fe1dca2"],["/categories/数学建模/latex/index.html","dac8db58970585e8f02c9e3ce8f35767"],["/categories/数学建模/优化类/index.html","3b9567f2062c368c2c121728bd4a9f34"],["/categories/数学建模/优化类/现代优化算法/index.html","5156d3526186cf3c81fa3d6e64a6cd72"],["/categories/数学建模/优化类/规划类/index.html","5e0eac135d699de7e95c10905152f98e"],["/categories/数学建模/绘图/index.html","55365ecc887fc293abdeefe3bb4f1e8a"],["/categories/数据库/MySQL/index.html","13cbbf19374cb41ebaf3c73138c823ab"],["/categories/数据库/index.html","442e4b6c4711bbd384d5715920e9f278"],["/categories/数据结构和算法/index.html","bb3da96bc26eafd19506b317b9d784ff"],["/categories/数据结构和算法/page/2/index.html","a18d20ba5b3d293e57f43da6a934872b"],["/categories/数据结构和算法/基本原理/bfs/index.html","b8d4e2328fe1c8540310ffd39d5e008e"],["/categories/数据结构和算法/基本原理/dfs/index.html","691c689db594941c9d10ab2cce9be8a0"],["/categories/数据结构和算法/基本原理/index.html","9b2d1394541816e0404876477fabc382"],["/categories/数据结构和算法/基本原理/动态规划/index.html","110d2163e375383e5a4f50a42a6be77d"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e11b23497d56ac7778773542866bcb29"],["/categories/数据结构和算法/基本原理/图论/index.html","d387d9211aee5e7ea88ba6af27e2f493"],["/categories/数据结构和算法/基本原理/字符串/index.html","eb5b32bae13110484702168b947718d7"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","eba6ecd3a00dc2e656f7337d12062d3d"],["/categories/数据结构和算法/基本原理/数论/index.html","1e2c2f9d18e5cd35ed0f84e0c3facd0c"],["/categories/数据结构和算法/基本原理/树论/index.html","5f4591cc03e477a51db38bd6e34015df"],["/categories/数据结构和算法/基本原理/链表/index.html","3a51aad1df9b230f8c4f369042a8ba00"],["/categories/数据结构和算法/算法题/index.html","fb73a16cc3f72502c74ccb64f57fa77a"],["/categories/数据结构和算法/算法题/二分查找/index.html","0cdda127c40b85cb29d8b2ca4fde1072"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","3ccbb388841e0b42342db39a0a44099a"],["/categories/数据结构和算法/算法题/动态规划/index.html","c15873a7c9e3d993f30ff3922cff4e51"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","af8010564150f4e0cbd946e792be762f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2e68267702bcead7d66178a576f1195b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","2afc2a086eb902796a2fc4c993441bc2"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e34138821fc5dda385d6cd568fb5c230"],["/categories/数据结构和算法/算法题/栈和队列/index.html","877b0b6411d01bd05b72c05f341b4f7f"],["/categories/数据结构和算法/算法题/树论/index.html","f679df3b2b70ff2dff83043721eeff5b"],["/categories/杂七杂八/index.html","87dc73f4e4cb3dee9147f291e3b2e072"],["/categories/杂七杂八/博客搭建/index.html","a05ae72a51c8d24ed5b4dc352d6a7d6b"],["/categories/编程工具下载/index.html","f4dac20514cf08ca982014e2a33c230e"],["/categories/编程环境/index.html","25fb99c98bf91eccce54a41111ed7ae9"],["/categories/编程环境/大数据/index.html","26cb5b43bd80bd6dbc336077c92c9632"],["/categories/英语学习/index.html","c0217018b2fe53307e227df0d8d0f7d5"],["/categories/英语学习/英语语法/index.html","3b0e7e262ba55876281f03eee34b7e92"],["/comments/index.html","5a06b56694faaffca3476610e6c69580"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","11dab9c2aa96735fca6266286fb037c3"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","28edd712f20cfa9714654253d8a12abc"],["/movies/index.html","9ad3f6256a03a99baa692daa775c0f2a"],["/music/index.html","13e2c7728882fec86892e43f6425f6a9"],["/page/2/index.html","e61c8a82bd1386a3cf007a931296a04f"],["/page/3/index.html","3f873f486d08b8427beddd5a2f791c27"],["/page/4/index.html","9021d55408cabaf95e2792715e7ef332"],["/page/5/index.html","5335518fef2111103c76bcde94b4fb49"],["/page/6/index.html","1eae25d58d5ae5c1ed852e3634c9eb98"],["/posts/1021360842.html","dc579d1cb443db1b3411465fa3c6020e"],["/posts/1120620192.html","88e44332f50e33fbdcbe098221801bee"],["/posts/1141628095.html","c1880581fac1a6ad3e8f4e1f245fac7d"],["/posts/1168613674.html","e586210273038e9727d118957fe0538b"],["/posts/1219920510.html","e67fdad9810f753b37d8ce413c5a0696"],["/posts/1222166338.html","51cb52b939d3c3fc927395964bd386de"],["/posts/1259097482.html","6e50bb7b7900cebe7228a7414d0d0638"],["/posts/1271036369.html","7f3d379912fbfa382257cc37d28c6093"],["/posts/1312847445.html","c75e2bcfc4df45387889b059db2f374b"],["/posts/135355774.html","71400d9b9e216e9bb7cd0f7d4ea7afe4"],["/posts/1375344716.html","756c97f8187708fa62efad55d895e5e0"],["/posts/1388991698.html","c52feb23e2ccee8f76d23650095f6ad5"],["/posts/1410315814.html","d927cc3dbfd3d721c99a73c960a41e5c"],["/posts/1452790229.html","a6688400fbb5cebd8e938de349228939"],["/posts/1470079884.html","9e98e8d6cf94e73601291c1706733d12"],["/posts/1470079885.html","1c9cd599a047712c6a9f74ba4f767d65"],["/posts/1470079886.html","2bb634c7f730290e902d12ea89fe2fa1"],["/posts/1470079887.html","07421fd6e1bcd3091e66c04fa44d141b"],["/posts/1498536549.html","74da04e30ae975f0bf28acfaa4e04bc7"],["/posts/1547067935.html","eed96b1f9dc6df5c317c32325ca6e7a3"],["/posts/1557866301.html","cb11e17f1e3c310ab7f88fc16b93f48b"],["/posts/1571776361.html","4ccfb90cc28d48d5da8f7bb02b957b6e"],["/posts/1605124548.html","1c5c397c8b740027e41d8d6e3f82cbaa"],["/posts/1633036852.html","50325a4509f3987ee45652cbf5aca2e8"],["/posts/1674202625.html","a7417603b9bfafdec82a04351ba487b6"],["/posts/1765123828.html","333dcbf3fd46af80d888d7c437553f04"],["/posts/1767336200.html","6a3e90e2dce3bd8588b95a940d89fa7f"],["/posts/1776114197.html","e150d7dd1ba97dcd1eb550a8c7745282"],["/posts/1817748743.html","b1b99ac51b4bb844e51e524fdd785ef8"],["/posts/1925125395.html","6f4027d3e13ad07bd86016a0fb625ff9"],["/posts/1966191251.html","6de53ee72b33284af44b56576cf5c11f"],["/posts/1987617322.html","1ef233ee4600568f8ae6064d2cb9da2c"],["/posts/1999788039.html","295bf27a1ea9a1802021a2a0f9dc3d4e"],["/posts/2075104059.html","6232cfe0715347785cc0886bcfaa1529"],["/posts/2087796737.html","f3d0d711ba874eab98055a91f2cd51f7"],["/posts/2106547339.html","fc679678494ef944c4a92f83c4f7878d"],["/posts/2207806286.html","7a6fea5cef173a32bb9dbb94a68987e4"],["/posts/2225903441.html","138600d63aad0c85c37df88f7cd286f0"],["/posts/2265610284.html","ce6503824c0e32aa5ec78f072d1ece65"],["/posts/2281352001.html","1bdb5afc3fe57b3da548f5eed760bd1c"],["/posts/2364755265.html","d97ea303ccd9cbe5cd72dae159256580"],["/posts/2414116852.html","9b746d37adbc1bd18d0f0bb328cf48f6"],["/posts/2421785022.html","25a3ede623e3a4612f92f1c6320743b8"],["/posts/2482902029.html","ccba8dd054ba0daded35944bc3dfffe4"],["/posts/2495386210.html","9aee19cc5f5c0b7d704247f62d0e64e3"],["/posts/2516528882.html","85ac25d6a9becf8df7fec442b7056919"],["/posts/2526659543.html","e5e256b46436f3c7dc25db637bd3efaf"],["/posts/2529807823.html","baa762f4406dd27ba716054bed97cd21"],["/posts/2596601004.html","d68623349360c7f780cfca27c3455c66"],["/posts/2742438348.html","a37ee53aa7548c33cca0fb5e62c36f9c"],["/posts/2888309600.html","9037c590320c05fb652e3406d22715cf"],["/posts/2891591958.html","3c5c6c4db36cd81b57c421b63db730cd"],["/posts/2909934084.html","bf6a2404a03af91963ab659a39bad4bf"],["/posts/2920256992.html","6be8098b3b76e32d361827c837f91c33"],["/posts/2959474469.html","132f858b67c2f71d4164a8c64236e5cf"],["/posts/3005926051.html","bf1d43066ac6be6b628eb17289d89e60"],["/posts/309775400.html","96284bb4fe7920c8d07ed80aa0ed53bb"],["/posts/3156194925.html","3fac22679b6f6d3b72237b9a27f2de9d"],["/posts/3169224211.html","ff804d838979e443848b17e7c75e2b98"],["/posts/3213899550.html","9ba0611661813a717d1277990243722e"],["/posts/3259212833.html","bbb7163ce5dbf2f9791950867aa1f5a3"],["/posts/3266130344.html","04e1e26c05f6f2dfeed4829826c68b11"],["/posts/3292663995.html","9666ea4edc0e3b07efadda08800ee07c"],["/posts/3297135020.html","809d0420db9bcfab458e4ec9f3e3f554"],["/posts/3306641566.html","817c8f4e97c02a007085e64ec13cfcdf"],["/posts/3312011324.html","eb9accf6982065d513a48d2ebaa754e0"],["/posts/336911618.html","a297eb042a6af6ee9e5e6788034a49a5"],["/posts/3402121571.html","ddceb3d5ab727f57129e0af0f1e0c93e"],["/posts/3405577485.html","4a2f63ce42f005e3f6f7bd40ecd793b5"],["/posts/3498516849.html","622015db8ab18ca4f274b72e474ffe45"],["/posts/3513711414.html","9dde3aa52a316cf9bdabccc13d455a57"],["/posts/3546711884.html","7d1e695fb75afcd25220b1530b53500d"],["/posts/3731385230.html","df7891979dd8ee461dbdd7436e89d736"],["/posts/3772089482.html","5e39d10e8cdbbaad2b398e7bb2fd0534"],["/posts/386609427.html","dbdc85b57796dd1e535d8ecf650fa0cd"],["/posts/4044235327.html","1d270cf0d09a1799d7c10f7afdd460fb"],["/posts/4115971639.html","9f4e755a3276758326b96642847b5cc7"],["/posts/4130790367.html","0698b60db27175b52db71cdb99ab0936"],["/posts/4131986683.html","20d29d12606400d187c1083948cc8089"],["/posts/4177218757.html","043825ccc99e7358df54b0d927a08389"],["/posts/4192183953.html","88ff7d40fc65b6d8d8dc84a1ead1c1dd"],["/posts/4261103898.html","be0eaf464bfa707756f1411fda6f4ee6"],["/posts/469711973.html","75ecd716327d0bb16320819e02bb6445"],["/posts/482495853.html","33d5623ad7aafe3a7f0093eab7853928"],["/posts/488247922.html","1ccbd36c397716cceba8ae2fc087a4c5"],["/posts/517302816.html","896698bc18e2693d09e02182f0a7404b"],["/posts/570165348.html","2661e346d9ead808ac99b6a0a6deed7d"],["/posts/595890772.html","34b8cbb0c3efa5b4387aaf26ddb0804b"],["/posts/67485572.html","5dce278a04ecc302075b70cd65d2963d"],["/posts/694347442.html","28bdedbc935f599ae414c022337d11f3"],["/posts/707384687.html","7deccde69fb5cc4c54c7ea08a3134a42"],["/posts/71180092.html","7a2afdb1879e135836c8f4d419a8d964"],["/posts/716459272.html","6261fe53bf06c70a16aeb4ca26881dd4"],["/posts/765481613.html","a4b8a99af36ca1bd15173f68bfe73d5f"],["/posts/778231993.html","57e408b174f2cab952ae903d8dd42536"],["/posts/795397410.html","fed8f7c19d9c64b0cc2e2ffac31af3e0"],["/posts/820223701.html","7fe192256ab05ca3e408e0e6fb30c26c"],["/posts/830372185.html","f523194d7591c2f18abf4c8129365004"],["/posts/88294277.html","7de6219a9b778359fafddaa666e685cf"],["/posts/939963535.html","d3d7c539812a413cad9d2348026c46d0"],["/posts/983786067.html","14579694dcb40ab221843b69438bdf70"],["/sw-register.js","65e240a71810d70347c6722beb1c954d"],["/tags/C/index.html","3795860f1433c88ae5a3b62e0d72c99c"],["/tags/C/page/2/index.html","d3e897f61e40de595072829213bbeb79"],["/tags/C/page/3/index.html","1fdbc975276bcc8bf3e2491cf7231cbb"],["/tags/ETL/index.html","1c993a8cc122242192361a40609b91bc"],["/tags/ElasticSearch/index.html","e400197f35572426f52d7f1a484ab2d4"],["/tags/GUI/index.html","94f52c1f12cea05b91dab43f77281321"],["/tags/HBase/index.html","096a3031e3315c33a5a5b7684176ccb8"],["/tags/Hadoop/index.html","a94ed49ebec63d82cd7255b2f42db756"],["/tags/Hadoop/page/2/index.html","2005ae01304d4b7a23743a1c5d5108c1"],["/tags/Java/index.html","96836b7d6278bbc13ef064ef6c4991e8"],["/tags/Java后端/index.html","504d4005d27a73f6883a7320c8da50dd"],["/tags/Java后端/page/2/index.html","8ba40c2421676e3b1b53baf617ec0d78"],["/tags/Java基础/index.html","931db0e58da28932f8346541ab675818"],["/tags/Java基础/page/2/index.html","c7bd586e355f1aaf13fa17643d67aae8"],["/tags/Kettle/index.html","e5448c76d3603d9ca23edeed3db174ad"],["/tags/Kibana/index.html","a510f6bec7c749713f971198c2e9009a"],["/tags/Linux/index.html","8b4291b0cb51ec9408cdf827ac85a217"],["/tags/Linux/page/2/index.html","611f992b62b1273cbb9dc5563b2e4069"],["/tags/Linux/page/3/index.html","c939ee4b06192c9076588aa169bff06f"],["/tags/Mac/index.html","8e642d90489eec947a5214c9ac9be6a9"],["/tags/Mac/page/2/index.html","a5fe1ba3a82f4342e2c4546773aa5574"],["/tags/Maven/index.html","7d4db288512a5449ba9585e006298d26"],["/tags/MySQL/index.html","aa098216b261e013ae490285c4c6ab7e"],["/tags/Python/index.html","ffee92f973cb5a0416dcadd6ac7c8e01"],["/tags/Redis/index.html","0d0d497b3b01c15bf94e21be06bedb4f"],["/tags/R语言/index.html","6182196a91ad5fa4d8acb105ac178255"],["/tags/Spark/index.html","c0aadba6e09c0ff5497ec8869274fa95"],["/tags/Ubuntu/index.html","b14ffb033de3d9def1c6038e234c9b8b"],["/tags/Vue/index.html","73dde499cf88666f0ce2dd295463ad8f"],["/tags/Windows/index.html","0e26eb5865fa7ac2f4f58c94acef500c"],["/tags/ZooKeeper/index.html","0c3fb4cc1fa5297a72b92b28ac9e2966"],["/tags/bfs/index.html","a955836dd6830c7c3d6f134a74d6d554"],["/tags/dfs/index.html","ef063bfd9eb632891f5d93244a1b33b6"],["/tags/folium/index.html","0060c1420deb9c2dc829eaa20bb37f53"],["/tags/git/index.html","2a8ed94cb8d5a326e62f71f6f703933c"],["/tags/index.html","f607226428fc1d542e11e3811ba28fc2"],["/tags/latex/index.html","4591504fd78529176d3b90c289ab7876"],["/tags/中间件/index.html","9795d518fc89533ab4dfba91418a26f1"],["/tags/二分查找/index.html","529810ba1def05643a8711862c2ee1b3"],["/tags/优化类/index.html","bfd892c66f7c12c27f60d702ffe6e11c"],["/tags/前端/index.html","d14453d2a253ef8d9b0c1cf229166a1f"],["/tags/前缀和与差分/index.html","33abaab4743fc97033631eb6e2d95f6f"],["/tags/动态规划/index.html","e488f379b9a4f3da75063e2e55eddd2e"],["/tags/动态规划/page/2/index.html","7ed98831c15f4b6ac8891fb8f4467d0e"],["/tags/博客搭建/index.html","833980068f308f10bd49bbff7eedac48"],["/tags/图论/index.html","df83f039a8743d1e2b1d60f6a6707df7"],["/tags/大数据/index.html","28df7d058d0d4853dd0e5b0fb6352ea6"],["/tags/大数据/page/2/index.html","a2cf1c5fc034ffe4bd21a09c98e6afc1"],["/tags/操作系统/index.html","6063e6786a29a27173cb37599b8a1170"],["/tags/数学建模/index.html","e89040bfbe086a0f9e8499e4bf540b46"],["/tags/数据库/index.html","d14218a3594daa90ac50d978da5c6f57"],["/tags/数据结构和算法/index.html","b81f2a22cbdef293ccefe6e8d083b127"],["/tags/数据结构和算法/page/2/index.html","72b2f6cec2f170b26937bedadf28c2fb"],["/tags/数据结构和算法/page/3/index.html","dc825af154c56c21e811b948804d8c9b"],["/tags/数组和字符串/index.html","ee4343319d5c13c7f30cc67413bfd8f4"],["/tags/枚举类/index.html","5086c9915395f2de03bac1a60879d8f6"],["/tags/栈和队列/index.html","9811955e43395b012b91874edec73ab2"],["/tags/树论/index.html","878779826f7a865a44c2cf929871d2bd"],["/tags/测试/index.html","ad370c60cab0a9cb133b1e803c2d0b08"],["/tags/环境/index.html","8b67459078c83b4f5d705bda9f96ca38"],["/tags/环境变量/index.html","46c056af79963655c883b74fee753119"],["/tags/绘图/index.html","7011d3265de5a32fd3f5d5710d7d9d68"],["/tags/编程工具/index.html","527007fd6249379e3fc86c685813dc69"],["/tags/编程环境/index.html","4f74ea04474dd597757c767dc3d8d7fc"],["/tags/网络编程/index.html","0d9cbcc625b1eaa03efaeb4b12e8fd87"],["/tags/英语语法/index.html","5e3e4eaced8f98869a6ec39d7358188a"],["/tags/计算机操作系统/index.html","04f12e03b7fdf415ff53857e110fca65"],["/tags/论文/index.html","8a6ff06f1f19f5034c35b958752664e1"],["/tags/资源下载/index.html","ca9bac741afcda0e52e3ba6e5c59deaf"],["/tags/链表/index.html","3e808f49ebfed0a44b10fb69669e1928"],["/tags/集合/index.html","f32c24d65c4aa580a8cb2782a2503340"],["/tags/集群/index.html","7bd5539af49786f64b123f4f7f5dcec0"]];
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
