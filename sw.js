/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d609f64fdf9748291580824138d93a7b"],["/about/index.html","30e9dc3db0fdf08209cab96716cb02cd"],["/archives/2023/01/index.html","79d704461d66199f72f271e9ddb88fff"],["/archives/2023/02/index.html","3173a8a5d8fb1d736cc4502c448a6034"],["/archives/2023/02/page/2/index.html","22507dbd5ebccf8118132378d2570a64"],["/archives/2023/03/index.html","0f4945161c4b5d29b9dfa3ac18acaaac"],["/archives/2023/index.html","9a5cd399e3c2303b3670679c875ac867"],["/archives/2023/page/2/index.html","ccdeac9d25a281174e767282cc441556"],["/archives/2023/page/3/index.html","a7a0fa24b8f634c8eaa6a1af55058a79"],["/archives/2023/page/4/index.html","9e04f2090c582663e1badb5dc6eb3858"],["/archives/index.html","fa93466faf9b4008d086a2c305761e72"],["/archives/page/2/index.html","eba38ef7a7879a8a14f82b421cd1960c"],["/archives/page/3/index.html","6ff8051acbf6245eb524426e6a3d4a90"],["/archives/page/4/index.html","fe8cd39d4f3d293b2bbd9c102021b241"],["/categories/Java/index.html","2ca8ffc419ec2408097db8347ad86fcf"],["/categories/Java/后端/index.html","8811f0bd818cf0e4c8ad20eb85a857fa"],["/categories/Java/基础/index.html","9d8bf69a3b4e405e4ce036b5834832d9"],["/categories/Java/基础/集合/index.html","f88f8e07953a1bcb2d6297997257e1a1"],["/categories/Python/index.html","27c9ece50fe04994252c6599f8cba828"],["/categories/Python/编程环境/index.html","374f163b9cbb9160641d8b6397770d86"],["/categories/R语言/index.html","1452a46b321c2761f06526d0c079746d"],["/categories/R语言/编程环境/index.html","1c31b1ec17393202684ea83400677ae1"],["/categories/index.html","3c4f679f1a69a00fcdb2368a50d1efa5"],["/categories/大数据开发/ElasticSearch/index.html","7a68e9b8c3e8a6711b21b3eff3e0e1cf"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","f823e49c0e01fc283daf5e5edbc2beef"],["/categories/大数据开发/HBase/index.html","af52d6c2cb2ecd4f80bc8fb4a80ccb4f"],["/categories/大数据开发/HBase/环境搭建/index.html","5d22f98354a4b6c80965145e68b30432"],["/categories/大数据开发/Hadoop/index.html","7794625af1ad84161a50cdd15f85816c"],["/categories/大数据开发/Hadoop/环境搭建/index.html","cbb7f7c51de17878fa17cde914b5c16a"],["/categories/大数据开发/Zookeeper/index.html","12b5c3da5082e1bcd08938e2514da98a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","a768a11e8e593d85af52959d5ed537b3"],["/categories/大数据开发/index.html","aefe6f71a3bd2bde497f84851cc7148a"],["/categories/操作系统/Linux/index.html","49a57717c7bd979ad551f8560d0c20c3"],["/categories/操作系统/Mac/index.html","2a5267ebe6e86bbb1a7dea765078b98f"],["/categories/操作系统/Windows/index.html","a0b4d38a9769661b2219dc0d0923050a"],["/categories/操作系统/index.html","9efa426a4da2ff824119672123e449ae"],["/categories/数学建模/index.html","937f1b85cacff21d1e06688e1490cfbc"],["/categories/数学建模/latex/index.html","a32104a832f3b0d70a89e368f9a255a9"],["/categories/数学建模/优化类/index.html","40847496a8df291fe0c02b0b2433ec33"],["/categories/数学建模/优化类/现代优化算法/index.html","150d473205abb783e0f36e34b4c02604"],["/categories/数学建模/优化类/规划类/index.html","05e2c54db047892676f0330b3a0372a6"],["/categories/数学建模/绘图/index.html","7abe6e00c21a737114948b0b38c73844"],["/categories/数据库/MySQL/index.html","163fbfa3a6162b86e7360d233fb764a6"],["/categories/数据库/index.html","59a42244e1bcac94c27b5a507e18760b"],["/categories/数据结构和算法/index.html","3ea7b160d2573d7919ff47293ea2d554"],["/categories/数据结构和算法/page/2/index.html","39c454e7fedbcdadb6354e187124b4c8"],["/categories/数据结构和算法/基本原理/bfs/index.html","789bd97064acb0c00cb53423869c5e38"],["/categories/数据结构和算法/基本原理/dfs/index.html","d81275e48600fb539d9c62115adae9fb"],["/categories/数据结构和算法/基本原理/index.html","8f68fbcb957af769c254a91174ef6d23"],["/categories/数据结构和算法/基本原理/动态规划/index.html","03736d7c948f1faedd79bb522c2be0c3"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","9d5fb3cfe888ce8e281151dd328220cf"],["/categories/数据结构和算法/基本原理/图论/index.html","8a4eb846138f02a572dbe7d0ca3def95"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","bc8df34af101d35688feb1acdcd70136"],["/categories/数据结构和算法/基本原理/数论/index.html","e5f5227b540eaaa3ab3ef1be34a41593"],["/categories/数据结构和算法/基本原理/树论/index.html","8971cafa464d0275455de7baf8921644"],["/categories/数据结构和算法/基本原理/链表/index.html","3550d1ca65de284fc4e1cb205e510507"],["/categories/数据结构和算法/算法题/index.html","b025d57d0ce3e6459144729b17f5e5af"],["/categories/数据结构和算法/算法题/二分查找/index.html","50f36d327c9118b71c471776321ea9fc"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","381e88871ff2f327b3be63be694d3a74"],["/categories/数据结构和算法/算法题/动态规划/index.html","ffbbce0d98c4846e97e0bfa3fa8d8f7d"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","95f9c0073f9f87b25fdec7b0ab277049"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2cbf0d189a6ff48eca7924b05fdac9f5"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","63c94ee617add514575b95f1f4d8cda1"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4491c98738d5c8366ce2aa675662dbee"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c61f646950714add7b1e57c293386f48"],["/categories/数据结构和算法/算法题/树论/index.html","fac595c4579a8d9a1b8c4d7137e7461c"],["/categories/杂七杂八/index.html","135f006a44f7530971bbfeee64549fb1"],["/categories/杂七杂八/博客搭建/index.html","48d18ed51c07c799dfc145f184512ef1"],["/categories/编程环境/index.html","9768c4b64bf88a844a9c979ebb23f080"],["/categories/英语学习/index.html","7b851bab4ad5b18adbc570810c2f536b"],["/categories/英语学习/英语语法/index.html","7f5b39f12e6a26351dee64187cf43a1c"],["/comments/index.html","6f3a4d1f77665c9bb0eb8f88c98abd13"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b2a7e9b08289b1f06b9bff47210c4b21"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","311e7cba2c77a244e7f65d7be7c0efdb"],["/movies/index.html","6f36c3e839db9eaaf4ef5672d4944766"],["/music/index.html","55fe09e7ad56e2c771453fa7c07eef1f"],["/page/2/index.html","450149e955c97c76b08b93a320d64a0a"],["/page/3/index.html","90a8499a709e188a2da5480b3d92bee2"],["/page/4/index.html","4c1e7f6ad1b881052c76486e99f37881"],["/page/5/index.html","69e3c9a2c0ccebd3404f7f594f3276de"],["/posts/1021360842.html","6b33a8e915507e523977051f6eaf169c"],["/posts/1120620192.html","bc261ebc95cca3eac379cbe14575e85c"],["/posts/1141628095.html","487ad9c14aec331c9473f9928932b5e0"],["/posts/1168613674.html","98bd5b93f9eb12ca2b892fa6aadb51f0"],["/posts/1219920510.html","4ceda36cad5fb6aafbe20f009ea4b3e0"],["/posts/1222166338.html","3ec21743257c03cee53503713eec6db1"],["/posts/1259097482.html","dec5c058483f6ad780b35a2763c5ec60"],["/posts/1271036369.html","2202076a306b669a2c0a8851735a99c3"],["/posts/1312847445.html","d4b504b799aac6f184e0f386bce67aee"],["/posts/135355774.html","2a3f028b55f7f62a529eaf86132b4e61"],["/posts/1375344716.html","852ea6e32fab7139fb9c0aa87d0708f8"],["/posts/1388991698.html","7869b82ad3822c4400c98bee7fa109df"],["/posts/1410315814.html","9b0ceeedde51be29889a5c3a409ce379"],["/posts/1452790229.html","d859f8b42b8fd551294f1508216e91ca"],["/posts/1470079884.html","2fcd5a60e83d69bc0214bd36d4af9c5b"],["/posts/1470079885.html","664fbae4c39e987c33484160ff5fbf11"],["/posts/1470079886.html","3b91d4f8d8a8ffdc493a86e502d180d1"],["/posts/1470079887.html","7b3a896e1195ba08c3fd81f02aea9f1e"],["/posts/1498536549.html","0125e7e0034155cb5ab40d067329fe5a"],["/posts/1557866301.html","6160bcb6c19ddf9116db469779f88483"],["/posts/1571776361.html","d274e70577daa7cf46ccac9035b0da73"],["/posts/1605124548.html","da92eecb5f4bf6e78586cb26c14511c5"],["/posts/1633036852.html","84656e7e3a6cf565341cc77e706b7262"],["/posts/1765123828.html","6ff7e015f94e2b0cfaaaa3be92cef331"],["/posts/1776114197.html","347870b7f7d851747d84688855f9c57b"],["/posts/1817748743.html","6dea450789dedde9d98def794c7ad386"],["/posts/1925125395.html","25b8027fd388723b1293228b204344eb"],["/posts/1966191251.html","dfdfe0d14240a8124a64648c3b78bb39"],["/posts/1987617322.html","84ccb743966a423c39d7e93ce254f054"],["/posts/1999788039.html","150de77dc17492ebda70fa63f76262d1"],["/posts/2075104059.html","8b9d272614743dd8f150e146ffd8167d"],["/posts/2087796737.html","0703fc61ac9ea95fd068a600fae74209"],["/posts/2207806286.html","494765ef998817a870cc2c4ba8e505d8"],["/posts/2225903441.html","94ca9067dd7ec04152b9df982f3c0eaf"],["/posts/2265610284.html","48940f8963130105548c618f58aa09f4"],["/posts/2281352001.html","8aca3d2e9ca897968f8bdc01e4129014"],["/posts/2364755265.html","7ab994cfc148bff35858572fcda189c4"],["/posts/2414116852.html","5bc4716e959b925150adb061f712783b"],["/posts/2482902029.html","bbae996f773450c47ea3462257ec9473"],["/posts/2495386210.html","09da2983a4ce494fa66b39d1bf3eaff3"],["/posts/2516528882.html","c45e5dbe2a092a488abf5d2af0656a89"],["/posts/2526659543.html","c4ee210a76942f6702f5fa03894c3598"],["/posts/2529807823.html","e1441ebdaf959e877ce0cfa2306b2e55"],["/posts/2742438348.html","85915359c8f0fb340e1cd406754ec9b5"],["/posts/2888309600.html","58fd015d29bae26ef4f67cb59ff298e0"],["/posts/2891591958.html","ce772f9dbe36fa71386e677c89983623"],["/posts/2909934084.html","27e7d9042b12c79d8e486c0c8862e08b"],["/posts/3005926051.html","80aecfff9005eb745897fd6bc38b3d87"],["/posts/3169224211.html","de05e01b83d32988949bf9088bf33704"],["/posts/3259212833.html","bae9e56c085a538d76abae9edfbf737b"],["/posts/3266130344.html","611e851feaa11bc9d3df35e72d5acff3"],["/posts/3306641566.html","9cac6c32e76392b8cbae6296a03ce8a5"],["/posts/3312011324.html","984e9c737f3f3e3f3d5dd14e47934959"],["/posts/336911618.html","625c97d10942add398e22aa131b55eac"],["/posts/3402121571.html","3017e2705ab876c1b60537fefdd2b5d7"],["/posts/3405577485.html","f173649f01fef8d8e151b0058b41667b"],["/posts/3498516849.html","21e7fa05a400d407a751119364890e40"],["/posts/3513711414.html","441ee5aa5aef1c030deb50596cb5a5e1"],["/posts/3546711884.html","b4debf907ed23ddf2e49c50c32dd011f"],["/posts/3731385230.html","ab9819bdf854b5bf110f0dba6078ffd4"],["/posts/3772089482.html","d5524d1bab5613c323eb6c16fc3837f5"],["/posts/386609427.html","8c4518dc9aa8522eb29d74dd067dae6a"],["/posts/4044235327.html","ae2d043520bc39e484663f6e03f6e6d8"],["/posts/4115971639.html","584263a1ec64f1e6c517aca7eda881fc"],["/posts/4130790367.html","7f235092e24a30d2d1d45ca310134c96"],["/posts/4131986683.html","0f2ee4510d0aabd3cb123ff527b944c6"],["/posts/4177218757.html","5518b3488508098514538ef381087e84"],["/posts/4192183953.html","67add06d93257749b8696ee6798ea1f4"],["/posts/4261103898.html","15765e0565c7b7103dd5a66d9025dcd3"],["/posts/482495853.html","af0ddaa52953f700fe67667eb3fc791a"],["/posts/488247922.html","d6743344e5d4a3b46b999e242e077016"],["/posts/570165348.html","885ca30aca1c54df51626989ef9cecd8"],["/posts/595890772.html","d3519f5b877babe749d0277698b93055"],["/posts/694347442.html","8c85bdc591e0da783e58f4114baaa2a1"],["/posts/707384687.html","48b0a9330bd5faf02aea0a8e57e0e477"],["/posts/71180092.html","82f14eb8b7a13a131ef925b20a126db6"],["/posts/716459272.html","e1016c55e4a99c136e135565161ed7db"],["/posts/795397410.html","7d3d5b1d5dd3ee4b0931460e4527e243"],["/posts/820223701.html","12717328bf93f0edc4346daeacaddfa1"],["/posts/830372185.html","35d3299441225b5a26258ce6540d7e2c"],["/posts/88294277.html","d300d1c3ad3e066b9c90c9dcb4f061cc"],["/posts/939963535.html","235942db2ce8d90d14168c78e6ba540a"],["/posts/983786067.html","ec286b5be368fe4bf3d8fe260ec9d440"],["/sw-register.js","85ca327913972a5ad9a5114c84e1ae73"],["/tags/C/index.html","e98d1e0703cc2b32db607bd33d3733d7"],["/tags/C/page/2/index.html","66f350de3db959a300d05fee7bfef71e"],["/tags/C/page/3/index.html","2285fc8b0e29595bfe6c7c6efa3cbaf8"],["/tags/ElasticSearch/index.html","fbd86e6dcb1cca0b7394bedebd54e9a1"],["/tags/GUI/index.html","bf08045e60f65c9509e1a004d495fab3"],["/tags/HBase/index.html","39a732868d73d8cdf3166519eabb7a2e"],["/tags/Hadoop/index.html","02009c0003c974c805e3b1c48c0630af"],["/tags/Java/index.html","ba317ac4afa74c70cf531ee41a731b70"],["/tags/Java后端/index.html","e6df9f39142b13ad7a33008e9834b50b"],["/tags/Java基础/index.html","95fc4e89b32472b1758d1159bc2b2e99"],["/tags/Java基础/page/2/index.html","90499c0f3a87edd9a483f6acda9a7415"],["/tags/Kibana/index.html","80b19142841030c3945505720fed40fc"],["/tags/Linux/index.html","471184823bcb79e09725352a15705d32"],["/tags/Linux/page/2/index.html","a3dca7124621428c8b8c8844cbf1794c"],["/tags/Mac/index.html","1a768bb53f3dd94a8f8a468de0787cd2"],["/tags/Mac/page/2/index.html","fbbb9761f9f98a9b429ac91afac585d7"],["/tags/Maven/index.html","3d247fe28d4d3ef2c372365a58351237"],["/tags/MySQL/index.html","0bf3338a1fc0588be6533afff17ae3b8"],["/tags/Python/index.html","de6d17fb573e464f48693b400b413aec"],["/tags/R语言/index.html","49b2bea60d3a5a3457821a56b76d85bc"],["/tags/Ubuntu/index.html","432f3c798e1d7ca5efefca48abd7446a"],["/tags/Windows/index.html","c7b496fca5e129fe56538563cf52baff"],["/tags/ZooKeeper/index.html","45c9c08406c8823144d83eb61f9064cc"],["/tags/bfs/index.html","4636016eb467942114309626686149bf"],["/tags/dfs/index.html","2b71503a37dd1488c4dc36fece61dd30"],["/tags/folium/index.html","ce553aa9b722db39b3451078025774ef"],["/tags/git/index.html","7a49aa4ce13b946c523749faf367ebda"],["/tags/index.html","4a4f886aa8a25fe18ebc52f2c39fed31"],["/tags/latex/index.html","8c444f393e6744bd27e34beb3240c215"],["/tags/二分查找/index.html","c89ec98fb2ebc0905cfa8ebd1e64d686"],["/tags/优化类/index.html","dbbc627a3289a327b0c74e32e117d383"],["/tags/前缀和与差分/index.html","63b0714cdb7dbbe3cc4f4e3d5247ed52"],["/tags/动态规划/index.html","dd1852b844051b08192ae703a63eb55f"],["/tags/动态规划/page/2/index.html","17357e79512c7386d4b32fbc5c44f9cf"],["/tags/博客搭建/index.html","ca07ddebe5765532bd6d974ea463892c"],["/tags/图论/index.html","85f3044d2ca2b88e3d0c2b03734b1369"],["/tags/大数据/index.html","729e18ec87b860127425dca6b8a041a0"],["/tags/大数据/page/2/index.html","977e4c7beb2b1f8ec3bff4868d932183"],["/tags/操作系统/index.html","ea7d90622b1aa15aadd60e97f8f29551"],["/tags/数学建模/index.html","9dc627bf5b4c49e725a0b96ce30af3db"],["/tags/数据库/index.html","5b39e1f808caf42585bae5fc41c6f7f3"],["/tags/数据结构和算法/index.html","b3954790215ef91d676f45139334a330"],["/tags/数据结构和算法/page/2/index.html","595715c6549a044c28f04dee18ed1f9d"],["/tags/数据结构和算法/page/3/index.html","01634a7e7bb806ff509bf92c6be3d4d1"],["/tags/数组和字符串/index.html","5d425f2310da52a64c7774369ac5a9b1"],["/tags/枚举类/index.html","24e573e80e8d2b9bbcee9755efb81e1a"],["/tags/栈和队列/index.html","0d5c3539b12e6a97a0f98f4fb6d814b2"],["/tags/树论/index.html","8308033be57444521db6c3b0afc0dc23"],["/tags/测试/index.html","344898ddd9f69e2a2acf185d84aa01e4"],["/tags/环境/index.html","328e6dc7f8ff30f6b7eda18576487b2b"],["/tags/环境变量/index.html","71b0ddb0e857d882647c61aaff96596e"],["/tags/绘图/index.html","7fbb415cbdd8657f21259fb2ca8468bc"],["/tags/编程环境/index.html","cebdf52de2883fadfd34c36c4fd37613"],["/tags/网络编程/index.html","ed5b42b6bf4217af47b9d09598704ba5"],["/tags/英语语法/index.html","3c4c58777b07de02f6809adae7c9c372"],["/tags/论文/index.html","31c458df8d83b16b8b5b350e871dd717"],["/tags/资源下载/index.html","ab5710473912c165a45ddf88f529fef9"],["/tags/链表/index.html","c95fda93d97f6484c0d921400bff3375"],["/tags/集合/index.html","df89de12c75292e505e3e6c5415395ef"],["/tags/集群/index.html","f32e984fee0ed11ca850737624d9428a"]];
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
