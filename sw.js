/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","f1d6a5035319bd0c0fa482ab0a475b10"],["/about/index.html","ef99c041b12bb8bf2889ff26c286f531"],["/archives/2023/01/index.html","2c4ba2e8ac163b8e62c98e4a1723dac5"],["/archives/2023/02/index.html","bb9c830cb342ba95bd02fb2e99a0e10c"],["/archives/2023/02/page/2/index.html","d787837ffc5c857d39ca4259c213dba1"],["/archives/2023/03/index.html","9cb9f19bf0d3fa8ebad2d8e8fc56c82f"],["/archives/2023/05/index.html","cd7da71aa63b272498a24a7e1b260c37"],["/archives/2023/06/index.html","608725b3a247d31a05bfa58d7ace23d7"],["/archives/2023/09/index.html","11d46a9227c126884acd4c55d015d94f"],["/archives/2023/11/index.html","a479608b53a903ce9a5f27ea4bad80ae"],["/archives/2023/12/index.html","f94ef6b250dd9deb04fd073a3c8ee050"],["/archives/2023/index.html","48f2d2be2904c04f1fbe333b989d043b"],["/archives/2023/page/2/index.html","50fc5942cd0c37531616d8ca90d96f90"],["/archives/2023/page/3/index.html","41dad47ea61273f38c6b403d4760df8f"],["/archives/2023/page/4/index.html","ea211f56a7993272cbe19033b851d42f"],["/archives/2024/02/index.html","e0f514e9a4b21d4d1fc62a32bdedcd16"],["/archives/2024/index.html","b5e71d51a58ff58d52e48e786f8d7505"],["/archives/index.html","8484d39c86b6e61e1b1275bc5a322c23"],["/archives/page/2/index.html","8232601ec4940320f97059e8e468d05e"],["/archives/page/3/index.html","0446ec5a7cb56e26e3458a12cad52c19"],["/archives/page/4/index.html","9ba14caf1d18493d9704e59f7c5b638b"],["/baidu_verify_codeva-qQP2iZOMLX.html","5eebdff49374d7134bd0f3b7834d95b6"],["/categories/Java/index.html","0f2b9083fecf399da19837bf32e59c7f"],["/categories/Java/后端/index.html","b10771b6177127d7ff57b0e93fc7f482"],["/categories/Java/基础/index.html","dd1790f35f92324029c13ce73642bf3d"],["/categories/Java/基础/集合/index.html","8ea2db2fa1640fd692e77a4c68c44afe"],["/categories/Python/index.html","b37e553127d6b2289029db8462b0511b"],["/categories/Python/编程环境/index.html","ec97545bf4f8b7ec447ea58cc24a56d2"],["/categories/R语言/index.html","9da153a68d3b68072a53846b9abd42ba"],["/categories/R语言/编程环境/index.html","5792e6eff97791f2674c53871c83d015"],["/categories/index.html","380b9234bbea637e05f95b8f8e453228"],["/categories/中间件/index.html","61c9d738f5aeb720bc19a32829dbbc44"],["/categories/前端/Vue/index.html","3b7e67f7d8eb391df64ac1a93baaf15a"],["/categories/前端/index.html","ee6aab31fc947a9b32413b687caa917e"],["/categories/大数据开发/ElasticSearch/index.html","7f3f6556ec249087e0004e25a630d278"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","cd93f74ca1a41b7511e1f312555491db"],["/categories/大数据开发/HBase/index.html","c6356f46e2d23414593d6fa1226126c2"],["/categories/大数据开发/HBase/学习笔记/index.html","c78b8b64697308864c81d3e7b0e311de"],["/categories/大数据开发/HBase/环境搭建/index.html","bbc6cb28ff42ce9801827cc930df12c5"],["/categories/大数据开发/Hadoop/index.html","5d4773b44f882c154eb8e482f9205b91"],["/categories/大数据开发/Hadoop/技术/index.html","26eb21d8ea8205444361f39fe786f612"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b8c6705bdea8230e91d0d00eb92bcaca"],["/categories/大数据开发/Redis/index.html","071030166df277657f731a081b535b0d"],["/categories/大数据开发/Redis/技术/index.html","48de2d291a33a25e315bf715cd2d2a5f"],["/categories/大数据开发/Redis/环境搭建/index.html","f68ed8cc2eaa6ae58c17cc17690a0cf1"],["/categories/大数据开发/Spark/index.html","095f774d863282716f906fa0559b6564"],["/categories/大数据开发/Spark/环境搭建/index.html","e0c42c36333cc7ccf293e71b2770c41e"],["/categories/大数据开发/Zookeeper/index.html","e7361895f6e030dac6a9768a2c26b341"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","a8e129b26ca976a86ad2415d8c51fc69"],["/categories/大数据开发/index.html","a1f2ebe579e5fe28d710d3702b6e2642"],["/categories/学校课程/index.html","fb7935154b5a648abe0e47426dabaa6b"],["/categories/学校课程/计算机操作系统/index.html","a53b126629ba6355b43ccbdf162c520a"],["/categories/操作系统/Linux/index.html","fcfceb0257729ef535c7ecae5172bdaf"],["/categories/操作系统/Mac/index.html","b9e27a0a441ea99270d94921e38309f5"],["/categories/操作系统/Windows/index.html","7cbaee448707badd238b20d33c5657ac"],["/categories/操作系统/index.html","ccca376d12b53016b03bbb7201fc9038"],["/categories/数学建模/index.html","1da09065163754cd8ae89c0e1086376d"],["/categories/数学建模/latex/index.html","9d071d10373596499b32ddd1690ca543"],["/categories/数学建模/优化类/index.html","71a8f350ee3301fad2c7b29f39900d75"],["/categories/数学建模/优化类/现代优化算法/index.html","a27afe159b1881de1979c72b2624a007"],["/categories/数学建模/优化类/规划类/index.html","db51323367e4573da890df8d3710a870"],["/categories/数学建模/绘图/index.html","58e4e2207e7eb9b68353ff3370b88bf7"],["/categories/数据库/MySQL/index.html","7a89a9fd898e99eccbc981ed36f0a18b"],["/categories/数据库/index.html","9da40157034853e9c319b0cadb321f28"],["/categories/数据结构和算法/index.html","f9232f53e0b247f97508a5367691f436"],["/categories/数据结构和算法/page/2/index.html","a6d1010427fa5239941c034d8117b859"],["/categories/数据结构和算法/基本原理/bfs/index.html","959078fac044beb9d9ede96966a1d59f"],["/categories/数据结构和算法/基本原理/dfs/index.html","ef8d9423b34c6358d3f1e934287236e6"],["/categories/数据结构和算法/基本原理/index.html","f5f5404e76741d59cf44d3ce4ee56d24"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f9b0ae4ec3ca2cdfa762b26881979eef"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","1ffec79081a60a91833589a395594c03"],["/categories/数据结构和算法/基本原理/图论/index.html","fb1cadfa4cfccca8ecf469d19f23e86e"],["/categories/数据结构和算法/基本原理/字符串/index.html","ed4e8661319bdb630d65d2c552a65fdd"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","77af7031c804f733b20d83d09564c26c"],["/categories/数据结构和算法/基本原理/数论/index.html","242c9189164a03e108db5ccc53c03b7f"],["/categories/数据结构和算法/基本原理/树论/index.html","ad5f7453c5daf850d277819f08d7d4c3"],["/categories/数据结构和算法/基本原理/链表/index.html","865e9267e97a69d94d28e080f7dffc53"],["/categories/数据结构和算法/算法题/index.html","6181e3f7cd6b3dab9c08b8c13ab63376"],["/categories/数据结构和算法/算法题/二分查找/index.html","1540de36222267cce509b10798653165"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ce46c084518b0ed98c85e6ec6e76efb8"],["/categories/数据结构和算法/算法题/动态规划/index.html","bc109d82e0824e97fdb3a611adf06874"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","7e7bf52ae09b41b3da3ec1bca9c20274"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","a29434e2669311e2dd532c5472a1322e"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","089d08dfce0e540e4e64fab87f5db40d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ac00fa4eedcc0d849be1404eedc85194"],["/categories/数据结构和算法/算法题/数论/index.html","b576601b76718c87dc1980f84df7285c"],["/categories/数据结构和算法/算法题/栈和队列/index.html","7073a9af788e58d2ef7745b482180489"],["/categories/数据结构和算法/算法题/树论/index.html","1a34126650b74dd7a47da4e35be6cf91"],["/categories/杂七杂八/index.html","1b7763215eadaf016613f63588414966"],["/categories/杂七杂八/博客搭建/index.html","f9ee9ba19fd569dc4dadb7ca3b1d3bcf"],["/categories/编程工具下载/index.html","6c97aa29c25dc25c36e0b8abd69cc887"],["/categories/编程环境/index.html","7035de21f56893a95d09291291c1c3a4"],["/categories/编程环境/大数据/index.html","c789768bdbc144a2cf0473691cbd3e77"],["/categories/英语学习/index.html","df6952c8cd7496c88f7c5d80bdbc6df4"],["/categories/英语学习/英语语法/index.html","0adf3a3848ad455c4bf8e4318021bdf5"],["/comments/index.html","676649ae1eaf21394ec4045cf06fca14"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","5cb18752799844acffc00859656cdb49"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","1814a10d9b972f6c3a79cbdac528745f"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","6c9588f58b2acd48f3d0b8b78f6469b8"],["/movies/index.html","dbf6301cd9d9430fee048ad2dacd47a6"],["/music/index.html","2c983e645d52134b47abf5541344e84d"],["/page/2/index.html","644bce5f881e9eed9093d2daa62ace1c"],["/page/3/index.html","db83685072854b62bbc28ce4acb237c0"],["/page/4/index.html","60953707f68671b97c80fc6382f4e6be"],["/page/5/index.html","9ed5fa1caba7c5035bfbfb9026faa346"],["/page/6/index.html","5a2498592c30fdfaf5dca295a1a4568f"],["/posts/1021360842.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1120620192.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1141628095.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1168613674.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1219920510.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1222166338.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1259097482.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1271036369.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1312847445.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/135355774.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1375344716.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1388991698.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1410315814.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1452790229.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079884.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079885.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079886.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1470079887.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1498536549.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1539568593.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1547067935.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1557866301.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1571776361.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1605124548.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1633036852.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1674202625.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1765123828.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1767336200.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1776114197.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1817748743.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1925125395.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1966191251.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1987617322.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/1999788039.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2075104059.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2087796737.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2106547339.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2207806286.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2225903441.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2265610284.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2281352001.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2364755265.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2414116852.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2421785022.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2482902029.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2495386210.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2516528882.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2526659543.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2529807823.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2596601004.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2697614349.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2742438348.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2768249503.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2864584994.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2888309600.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2891591958.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2909934084.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2920256992.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/2959474469.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3005926051.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/309775400.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3156194925.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3169224211.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3213899550.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3259212833.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3266130344.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3292663995.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3297135020.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3306641566.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3312011324.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/336911618.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3402121571.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3405577485.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3498516849.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3513711414.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3523095624.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3546711884.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3731385230.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/3772089482.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/386609427.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4044235327.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4115971639.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4130790367.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4131986683.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4177218757.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4192183953.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4261103898.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/469711973.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/482495853.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/488247922.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/517302816.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/570165348.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/595890772.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/67485572.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/694347442.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/707384687.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/71180092.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/716459272.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/765481613.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/778231993.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/795397410.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/820223701.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/830372185.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/88294277.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/939963535.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/983786067.html","d41d8cd98f00b204e9800998ecf8427e"],["/sw-register.js","8d9285bb3943331dee9be042495a9bbf"],["/tags/C/index.html","50d9b9872aa0f80be906df3261813e89"],["/tags/C/page/2/index.html","31dd0fecebb77230fd133130a25e859d"],["/tags/C/page/3/index.html","c7a3a67a96594342fbf1f7e4086eca34"],["/tags/C/page/4/index.html","c5fc5673c375cc33fb9f8a7154052c5c"],["/tags/ETL/index.html","c6c2129665646cd0643f7bf9eb6e9bd7"],["/tags/ElasticSearch/index.html","b4dd76fbbe25e70a699c532dccb2a3f3"],["/tags/GUI/index.html","189257b3beeaad675d2b3858249241e3"],["/tags/HBase/index.html","43dcea08627b626ef1ad5e9ac3f6e3f7"],["/tags/Hadoop/index.html","788b18bc2062d4a6c16c5666406a8f12"],["/tags/Hadoop/page/2/index.html","7a567f083734da016e9b5befe49f6739"],["/tags/Java/index.html","006e3833b0613bb5091bf95a3316fdd9"],["/tags/Java后端/index.html","34356bd23bf5c0ff87fbf5d6e76d9e64"],["/tags/Java后端/page/2/index.html","14a0b7450b01459b7ecfac70e59b5b0c"],["/tags/Java基础/index.html","82e22c420003de13121337f1e87d9c86"],["/tags/Java基础/page/2/index.html","3885a49c2cf77abbb36f002a2e25efb7"],["/tags/Kettle/index.html","c737150d52d059e53115699610a602fc"],["/tags/Kibana/index.html","fd8c37cd42aecc840cec7a5a246f2179"],["/tags/Linux/index.html","fc200ee14fc95e7fc2863d0f25bd6548"],["/tags/Linux/page/2/index.html","eb2a2aff7e768fab9bc19b8ce3cdf07d"],["/tags/Linux/page/3/index.html","90cd0c126ac3fefb3ceae0e50e2582fc"],["/tags/Mac/index.html","6ced57e7f0be4b86d33e32c3f798ee4e"],["/tags/Mac/page/2/index.html","c36c9cde5e5e88c14448342c815f164a"],["/tags/Maven/index.html","99c48d982835dfc86ac7c93e37c4b4ef"],["/tags/MySQL/index.html","ff42308b06323a5d188c6b26e7d53b12"],["/tags/Python/index.html","42fec3848978377add7342dd3885af8e"],["/tags/Redis/index.html","10bce4d4215d64c9868bdcbe1b81f539"],["/tags/R语言/index.html","af6d216e4c26d37c77404dd685810af9"],["/tags/Spark/index.html","8228c17c7fe2eb2df5e45fd112bceea4"],["/tags/Ubuntu/index.html","97f82778ec9a4c273efdf63ec08a8715"],["/tags/Vue/index.html","9a5f52b7c51c27e4eb17c4ed6133b21a"],["/tags/Windows/index.html","2766f4b72b407ee48554d663b421d119"],["/tags/ZooKeeper/index.html","d68221666dd8edcb68a682a09c839f44"],["/tags/bfs/index.html","c3facd3fbc83050976a12e680ed602de"],["/tags/dfs/index.html","1afc6008322d31c879d18c2adb09667b"],["/tags/folium/index.html","4a599c680a7791127740ec9db1d3c440"],["/tags/git/index.html","c73e49e3047f0a43641f79a84820104c"],["/tags/index.html","449d406afe125553fd87d6f4d2f46c05"],["/tags/latex/index.html","19b8791d8db45d52129ae30d9e4c45e3"],["/tags/中间件/index.html","9a2d8265a6ebfaf82e5727bd569474ae"],["/tags/二分查找/index.html","16a32c0e2eac18439c2ee551bc354f83"],["/tags/优化类/index.html","53e1092cbcc25421178270c229c5b210"],["/tags/前端/index.html","8597a2f161a0bd1b198ce59fa4562366"],["/tags/前缀和与差分/index.html","8708207d3934405c16f4efd9ffa4f31e"],["/tags/动态规划/index.html","5896b1299982df7e6edc7fa6a18ac986"],["/tags/动态规划/page/2/index.html","32eec6d13e4aa4e487d4c79e92e21980"],["/tags/博客搭建/index.html","807c8a618f0f00bdf7d2c55549276d18"],["/tags/图论/index.html","e50272ee383e62bf4d23ecbe4b9dd515"],["/tags/大数据/index.html","c3959fb29a5bf5da3efb376d5dd773ef"],["/tags/大数据/page/2/index.html","38d98b9bbc0e143ff6c337b5dd674a51"],["/tags/操作系统/index.html","9d29c7a33faaa64e3e6875f9c0d78686"],["/tags/数学建模/index.html","43a9d9732cbec0fffd0c31338affe58a"],["/tags/数据库/index.html","0ed19cbc8fdaad99d91b76dc74854563"],["/tags/数据结构和算法/index.html","c56a94fe141bd00c43ad1a6cde1200b4"],["/tags/数据结构和算法/page/2/index.html","91967ea4774ae400bdd2d854c54cad4a"],["/tags/数据结构和算法/page/3/index.html","5de7228d2369778d3dfdd87f282f1fe7"],["/tags/数据结构和算法/page/4/index.html","f7bd5e5c217b158e13ee2adfe4a28128"],["/tags/数组和字符串/index.html","705e4e270e5aa16ee6bacfc65c9aacbc"],["/tags/数论/index.html","cf3bd7f3afdfa566350126325ddae5a3"],["/tags/枚举类/index.html","499ff79bff9f4df8b2948204b09838d5"],["/tags/栈和队列/index.html","bc5b20a1664c4507f34e5b8e5c01e8ac"],["/tags/树论/index.html","c154b535a5c1f955160a14542b10c265"],["/tags/测试/index.html","d4d53989b7976f144c03cc7e132cc628"],["/tags/环境/index.html","e4c682f43a7b6989cb9df442e32b7045"],["/tags/环境变量/index.html","d37629b9ed0fa24ec8950cdab4769cc8"],["/tags/绘图/index.html","02fafc3c28b8907f17cd3e7dd1431093"],["/tags/编程工具/index.html","c338d50beed34c685e5628ae29df997e"],["/tags/编程环境/index.html","5c804080c0aef9675c36c9d8455e634c"],["/tags/网络编程/index.html","84a6247c1617aa43af16b59f04c64111"],["/tags/英语语法/index.html","a239f22c9747852b46bcaf1da8ffce05"],["/tags/计算机操作系统/index.html","f8d3bf0adea423364d704746feb65294"],["/tags/论文/index.html","fe8d7cab428b1fb5695c7b648a67c07b"],["/tags/资源下载/index.html","857b7647e4c8d7dd198c14a8b7c4f722"],["/tags/链表/index.html","bb457ffe5122c4c87c50079755a310d0"],["/tags/集合/index.html","df7fc12e8ced00db54a67955187929fd"],["/tags/集群/index.html","d3e03a1254ed8438f9700645a56aeb29"]];
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
