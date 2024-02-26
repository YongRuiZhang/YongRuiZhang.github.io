/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","0fa3d8a69d29b3a8ce658d5c8a78be62"],["/about/index.html","599438f318f91646df1d53d78d6e68e8"],["/archives/2023/01/index.html","c81c789aa0430e0c3b2de5731437cfec"],["/archives/2023/02/index.html","f7e2518d47db042f02c478ab122e319c"],["/archives/2023/02/page/2/index.html","ff1330158cc78e156720fb6b3f95ae91"],["/archives/2023/03/index.html","06288fd3ce89eb895ad3f27f7611692c"],["/archives/2023/05/index.html","d434e5933f72103d1b528d958f445fb5"],["/archives/2023/06/index.html","dbd6e210868101010c1f1f5998297d96"],["/archives/2023/09/index.html","e32d7e329b20eb77138d5ac717b593d1"],["/archives/2023/11/index.html","b045e19fe4fee6c032f6e6f9cfb21968"],["/archives/2023/12/index.html","3c6052d637581d04d66bc67300272f7c"],["/archives/2023/index.html","f981d0a34590bf975f9589d037d21530"],["/archives/2023/page/2/index.html","aa8d23b67e08264ee653d41b1f4dfd0b"],["/archives/2023/page/3/index.html","234f72b6d4032bd8544a4e36ce903a72"],["/archives/2023/page/4/index.html","2c64271a598a63d91b77b7f8152b8bee"],["/archives/2024/02/index.html","37c3fbb7d2c09444e96ab2e1cff80775"],["/archives/2024/index.html","1a810451752e1afcd3a7097c57d55694"],["/archives/index.html","98ed6871a5f1d3d282c9cb50cc5b77e7"],["/archives/page/2/index.html","fcda740b79cff6fb89aaaeb747b5f2c6"],["/archives/page/3/index.html","797d6e02ea159f0c56af82aeda3e155e"],["/archives/page/4/index.html","faf88435f655c6935a122842c8fa4271"],["/baidu_verify_codeva-qQP2iZOMLX.html","5ad5974042120025cf75e505032f1b09"],["/categories/Java/index.html","8ea9241693285eb7cf821c56e4ced932"],["/categories/Java/后端/index.html","869e7984ee6d21b69afd77c6340d2437"],["/categories/Java/基础/index.html","e1221d5047bc84ae17e9d45b1dabd21f"],["/categories/Java/基础/集合/index.html","cffe588000754743db7a6cb56122c1e5"],["/categories/Python/index.html","d2061925cf80103e57e69072f2b22091"],["/categories/Python/编程环境/index.html","9e1ec2128e020920a9c7fdfebd398b92"],["/categories/R语言/index.html","227ef1b887c5e8f89925aa54cd7cd524"],["/categories/R语言/编程环境/index.html","6c81d777f5689f290759f01fc227251b"],["/categories/iPad/index.html","8e7855eb1f4acf713661d8fe5691d96d"],["/categories/index.html","5bd9eb7bc5866e4bd570906a3c04663b"],["/categories/中间件/index.html","1b86dbce21412c679c4d6e69ef7c0eeb"],["/categories/前端/Vue/index.html","63d797ac2f8f1d15cb37d12ca239a1f1"],["/categories/前端/index.html","3a888f7ab72f488173225eac5d52abd9"],["/categories/大数据开发/ElasticSearch/index.html","79e387cd48755144f379de1c9b327497"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","80170f3dc5d59427404d9f484d581857"],["/categories/大数据开发/HBase/index.html","1799e706f551a8c1f0a5965558e8b995"],["/categories/大数据开发/HBase/学习笔记/index.html","d3a5fc052364eb0514cbb2a81cdc9c7b"],["/categories/大数据开发/HBase/环境搭建/index.html","49a365c1dc1284a8bc4e7b62ed2dfc4c"],["/categories/大数据开发/Hadoop/index.html","da3b7088352383d778582ad51cd0a633"],["/categories/大数据开发/Hadoop/技术/index.html","18230bed5bf360c422cdc8ea2b8b2a40"],["/categories/大数据开发/Hadoop/环境搭建/index.html","7786133210c55e660515d24c3e635255"],["/categories/大数据开发/Redis/index.html","5faa72e7181f68e8f59167518f2cf817"],["/categories/大数据开发/Redis/技术/index.html","1d87e9f2fb72d42e890612fd17f80079"],["/categories/大数据开发/Redis/环境搭建/index.html","3d3e9cdf7a59d8097974533395c3e3d8"],["/categories/大数据开发/Spark/index.html","c99093a6310e20f996fc9990e17ebf08"],["/categories/大数据开发/Spark/环境搭建/index.html","235ecdb9ad175c5bb65b321fb08ce698"],["/categories/大数据开发/Zookeeper/index.html","ca38f996c4007929ec5a4ca28a961549"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","909305db4cc11367a70a6fe10ad8c9e4"],["/categories/大数据开发/index.html","1a317c1cb8c701bb9283a388628ded9a"],["/categories/学校课程/index.html","16822550db594e6c4816d5177fbae422"],["/categories/学校课程/计算机操作系统/index.html","d536ed8f2b33c7ac12c85726dc55ba10"],["/categories/操作系统/Linux/index.html","3f39a953bbfcea63bad7118ea355e034"],["/categories/操作系统/Mac/index.html","07f072660753225d349d6904cf4e65ad"],["/categories/操作系统/Windows/index.html","dbf6023f98aee6d5d66f46a82f49a1ac"],["/categories/操作系统/index.html","d3317e7e2284098e9c7b504348031ddc"],["/categories/数学建模/index.html","729e5815d0eb2d98623e385ff461efff"],["/categories/数学建模/latex/index.html","30d9faff333bd0d47c00012d435d0fcf"],["/categories/数学建模/优化类/index.html","3c7dc1773a95d53a7600dc00bb115786"],["/categories/数学建模/优化类/现代优化算法/index.html","d94f21a3d5a1c3a97bfae428be827359"],["/categories/数学建模/优化类/规划类/index.html","f7806dc08bf3fc5e04f785f6bfd5d0ea"],["/categories/数学建模/绘图/index.html","faedf9c05126b38095bcae5a577e58e9"],["/categories/数据库/MySQL/index.html","c8fd3416f4a1bf7b1b49593ab308119e"],["/categories/数据库/index.html","93dc67882519f0603978a004a852a44d"],["/categories/数据结构和算法/index.html","ec84f651717a60a0093f4e06961f8a62"],["/categories/数据结构和算法/page/2/index.html","98acd6d6f576348f964d1de473a49e39"],["/categories/数据结构和算法/基本原理/bfs/index.html","81aaa634f353e30e9b5dda159bb31458"],["/categories/数据结构和算法/基本原理/dfs/index.html","155d38c9441f511feb5c7b5d21830be1"],["/categories/数据结构和算法/基本原理/index.html","869a53338a3745a33a489b39343fd815"],["/categories/数据结构和算法/基本原理/动态规划/index.html","70e283dd124d48dffe4d2b411613ec11"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","99837013c50aaf3e459264f1becbbc9f"],["/categories/数据结构和算法/基本原理/图论/index.html","35fd76ec92a3e8127fa5de47068dc76b"],["/categories/数据结构和算法/基本原理/字符串/index.html","d576586c978658f66c7179e9ed744804"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","cad5d2e79cf11b98de269e32178ec156"],["/categories/数据结构和算法/基本原理/数论/index.html","050fd0bece5a9c145024e73203f4d179"],["/categories/数据结构和算法/基本原理/树论/index.html","7f215541acd5b12b2ddc1ede688ee159"],["/categories/数据结构和算法/基本原理/链表/index.html","6290833b4d6ab963a1dfd99b608fa2a5"],["/categories/数据结构和算法/算法题/index.html","0d7b0a67982e018da203862ef47a314c"],["/categories/数据结构和算法/算法题/二分查找/index.html","3a50bcae2d8b92d293b97af0368138c9"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1a61002fc5b4330d6df92b3a84d9fddc"],["/categories/数据结构和算法/算法题/动态规划/index.html","2a365758e21ecf1f5568a1180de2f3dd"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","278b18ffda028494509e5ae325c22262"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3928d4c3f1e65977247f19a4d74d1b54"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4c57986a488222f116f599213ed04f52"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","8443a45f37781bd702dac7d0b88c18a9"],["/categories/数据结构和算法/算法题/数论/index.html","5a6fc13ee803fb2f271aba62344eab1c"],["/categories/数据结构和算法/算法题/栈和队列/index.html","3be10aa431ac05b259e0fd1b672fd422"],["/categories/数据结构和算法/算法题/树论/index.html","6a37ecdf2fe0fe6755381e766edcc7a1"],["/categories/杂七杂八/index.html","e24c634bb45ab64532be77c71a17b0f4"],["/categories/杂七杂八/博客搭建/index.html","617c0ca366738e1c292ca910d50ea381"],["/categories/编程工具下载/index.html","ad9f869d4ce6c9080caeaecdd31710a2"],["/categories/编程环境/index.html","8c84999032713efa2123101fa571d204"],["/categories/编程环境/大数据/index.html","308cc19600bd94efda1d3c47402f398f"],["/categories/英语学习/index.html","5a3ffc01a9d58ce5683b2bc3be67fbc2"],["/categories/英语学习/英语语法/index.html","e1c8eadee51d74920853439e266c2571"],["/comments/index.html","2ed29e081266201b73d6afc84948941f"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","386b4b9c8340d943df880ebff802025d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","479841c993010dcc192a2e623959c578"],["/movies/index.html","f68b8efff197462adfb4e56c12b80c8a"],["/music/index.html","66ae3696b15c7bea5ce5a8ce21b7052a"],["/page/2/index.html","4897e7dc0e150a8b10352ac9a534ea95"],["/page/3/index.html","72e9f2343ffeee71625a5d7d80c65685"],["/page/4/index.html","f572f3aac76c9914be2bb70f9b50d507"],["/page/5/index.html","2dc1065edc9bc04fb528426ae632fbe7"],["/page/6/index.html","ad175761ffaff0c23daef83cc0719dbd"],["/posts/1021360842.html","6b93d98ecb9993ce5aeac32cf4b5647c"],["/posts/1120620192.html","3d169552bb26f473c4e4298f00771b76"],["/posts/1141628095.html","d8c1f055ae3600f87169ac0657278557"],["/posts/1168613674.html","f2218a99d834cc12258c4defcce98bb3"],["/posts/1219920510.html","d7225efa2beb5e192e8e9f455cb00c9c"],["/posts/1222166338.html","f78324a699e08e969e2af6f78c4bd65a"],["/posts/1259097482.html","46bf85bad4c673d910842b87a60dbd2e"],["/posts/1271036369.html","3985b8ab92eb42484ea49212873c6f06"],["/posts/1312847445.html","86daf88cf19fc0f6f99ff10a4f4f36ce"],["/posts/135355774.html","2f0c0dd5b914a6d5b7f597da9ea8855e"],["/posts/1375344716.html","a5ccb18716f5404ceeec05bf4203a052"],["/posts/1388991698.html","9d2d41ed1457137fb4374af36ea4b78e"],["/posts/1410315814.html","6771b13450827cbd50e02fc37ab67e63"],["/posts/1452790229.html","4d93d62ec299e0c93d0118cc88317c9e"],["/posts/1470079884.html","0c67cf518352a2612563de2853849dd9"],["/posts/1470079885.html","3ca4618dfc50a1a034a44ca2d128f624"],["/posts/1470079886.html","c5733129abb71914226f8ffb2f71c9af"],["/posts/1470079887.html","c78bccc16921edd6cc6668f3084242fa"],["/posts/1498536549.html","6bd71c1cd31f55c36963c55ff0d3ad8d"],["/posts/1539568593.html","d5ed6a3a5dd6ed99e0effb893be6f956"],["/posts/1547067935.html","92dcd6a0cd40bc63e7f20fbf52988556"],["/posts/1557866301.html","ab658fd920fc8f7a409ee8e54dcb4688"],["/posts/1571776361.html","6c616dceadc44c526d534694b0bfa597"],["/posts/1605124548.html","cf85db44ca49e64b214b88c741099606"],["/posts/1633036852.html","719a0c19cf14e16eac03fe52960a5de7"],["/posts/1674202625.html","988b19030d1c5e6cb67b4e1926616129"],["/posts/1765123828.html","55b5283d71f4ed8c58780508e0fa5e68"],["/posts/1767336200.html","0ca7f93f59f2ce1e20602610d2052a03"],["/posts/1776114197.html","224cd9628e095970cb1bab96f170e335"],["/posts/1817748743.html","83633b8c12e43968403c8ab61786cf50"],["/posts/1925125395.html","d28a94adce953db5984a036e8437382c"],["/posts/1966191251.html","5911ccf623e7cbec2e1183e8dda7d5f8"],["/posts/1987617322.html","2f9b67b8efd7953d34dd6975c59c5ca9"],["/posts/1999788039.html","423a588a5becc87c96e96d1b0a19121f"],["/posts/2075104059.html","ebbc2d78e45e7b5f24fd7a271536b015"],["/posts/2087796737.html","36cf0f829035b9089003dd76ae8b9506"],["/posts/2106547339.html","8159df59aa626c6765eafe754e8c3d07"],["/posts/2207806286.html","1077300be3162921ad2ded886ea7a7c1"],["/posts/2225903441.html","1547323fae317a752bf289b9c3da5e84"],["/posts/2265610284.html","89e814899c617933b539a0f94ab518ed"],["/posts/2281352001.html","9409274eed897636785660ec8a070f9a"],["/posts/2364755265.html","2f71c853f8dabcbf4360a03de5ff876d"],["/posts/2414116852.html","ea2e83747a579a10fd1c55c67b659af3"],["/posts/2421785022.html","02f0124384d285c62de2f7009617da59"],["/posts/2482902029.html","f42b15f77f64881958c2dde88f1c8b79"],["/posts/2495386210.html","4c90d67cfbf304c106ade0eb0a5ac19c"],["/posts/2516528882.html","a5ef29efd4c27318504210f0c0ca4c00"],["/posts/2526659543.html","2baf49a1fdc88bf4b3733059291f5a1f"],["/posts/2529807823.html","4258abeb12ecc48ccd26ae1c17487f16"],["/posts/2596601004.html","05fe0f134ed99166e5dab6c7bdf7c0b6"],["/posts/2697614349.html","11d1729fc238e64c96e43d583bac040b"],["/posts/2742438348.html","5665481a026765ea35e294ab9d235f4f"],["/posts/2768249503.html","293e624114b5bde1e41e2305a2048828"],["/posts/2864584994.html","5f25b0363be165ff20eec6fb1e7a0e5f"],["/posts/2888309600.html","8c5e9213c00a84c12641cc01e38bb854"],["/posts/2891591958.html","063e20add63d5f234c3c0b1aa3b61968"],["/posts/2909934084.html","eb7f7e47d33130ce758927d0433e5a70"],["/posts/2920256992.html","4d7cba041d330282c6b7dc1810c53e24"],["/posts/2959474469.html","2c0e05423d0b9afe3445cfd60ed8c372"],["/posts/3005926051.html","22de1011b6f64152fdc2e78ee9a0a201"],["/posts/309775400.html","65936c05e5f3eb3a91b2065205e76bb0"],["/posts/3156194925.html","8f949f95eba2c3b29e8e77cdee4576b0"],["/posts/3169224211.html","f8025088de95fe23958b2c95996f183b"],["/posts/3213899550.html","9cad4446089fc1722c9dfc775a361443"],["/posts/3259212833.html","f96a9c6d3d3162cf62972fa2d6977d8c"],["/posts/3266130344.html","8c71cc0131104f6afdeb092c2e8f3fb8"],["/posts/3292663995.html","9f18a16bab3937bb61acafa62145cf8e"],["/posts/3297135020.html","ed827c6477af37632ac5f654d8832e49"],["/posts/3306641566.html","7891b070f10c54522a855cf1cf4f7e96"],["/posts/3312011324.html","0db5ec98c218d9943c439d9ec0921846"],["/posts/336911618.html","a79000c0500bf77dfa328ebce055807e"],["/posts/3402121571.html","c9e801f065dcd0366d0d98428cd12187"],["/posts/3405577485.html","ad53e9481f9c37c785862a0ca7ab6768"],["/posts/3498516849.html","cbe653ea66567e6a16e6075c6cb766e7"],["/posts/3513711414.html","7888c7a512a57bfdfb790b66a72f7789"],["/posts/3523095624.html","3a9cce0b9ab71a97ab816e55b11236ea"],["/posts/3546711884.html","ce968df4e68f221e39f6c906a6073d51"],["/posts/3731385230.html","e9941a5a3a9b86530d81a8e1fc227361"],["/posts/3772089482.html","779b4d9b6cd83337292828ed83e4e9a3"],["/posts/386609427.html","a24dcc746f9600970d2a680619e93d95"],["/posts/4044235327.html","405cd4ecddd454ad7d5b5c42522aa779"],["/posts/4115971639.html","33a5c192b6d67ac4bd242208af1beec0"],["/posts/4130790367.html","966be0f919b2bf755d1ad400fadb1421"],["/posts/4131986683.html","777ae5b3f0ececc4492ce587d1a05a78"],["/posts/4177218757.html","1792d0b56a835ba79e4f8b3e96eab898"],["/posts/4192183953.html","3982065edfcb83cdcc4dc273537599e4"],["/posts/4223662913.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4261103898.html","0f15477cbc209e4f94440db4b0472b77"],["/posts/469711973.html","ef955745877de7eee5961fd633f951b3"],["/posts/482495853.html","f3e78f36b6223bdebe4e05fa6f932094"],["/posts/488247922.html","fff7339b036fa0fe4a16347386684a7c"],["/posts/517302816.html","f788e574c0c189289c3d105e6ad4f311"],["/posts/570165348.html","531ad2bab37e4c650da5417644f285bc"],["/posts/595890772.html","48a5710c68b2e75403a7f97e2405b6c3"],["/posts/67485572.html","022a4e755d28ddcb263af062e8f7ac74"],["/posts/694347442.html","042b6ea70c1ddd8ceff2912fe7aa02f9"],["/posts/707384687.html","37158971530ef51cb922e47198a4a928"],["/posts/71180092.html","c217f44423636f96d4d7e79b36ff8463"],["/posts/716459272.html","abf1247e2c43e9a34f9caa10c3341634"],["/posts/765481613.html","0426a376cd5bce458737c9993ec92256"],["/posts/778231993.html","49bf4d8b5729fc4574f25f1fbf169457"],["/posts/795397410.html","cc25cdd361d5948f64db3392ed89387a"],["/posts/820223701.html","679b9583790de7924df9959724b6735a"],["/posts/830372185.html","ad0dc790bd8bbb3a2cfb800a0d59728e"],["/posts/88294277.html","0c1a8abb48c885b0e8f979065f6d8cef"],["/posts/939963535.html","0fd604f043bbd8110a3326fd342dd5c1"],["/posts/983786067.html","05559f31d1aa096f0d36b876979c1dea"],["/sw-register.js","5f3eab66c0985473f528c5b1d8ca1400"],["/tags/C/index.html","2f9828b8f512af219dd17a55f6d5f735"],["/tags/C/page/2/index.html","98f91f633f1e72da3109d71325e67759"],["/tags/C/page/3/index.html","ebd37664af81857a303881f72b2474a2"],["/tags/C/page/4/index.html","023a996edba98618e4d52aaec36801ae"],["/tags/ETL/index.html","53634e39b1c64fdf83bcf3696d97c36a"],["/tags/ElasticSearch/index.html","7fcb5aa67003a2a83b23ddcdf1784b2a"],["/tags/GUI/index.html","064028d183f02b6c6407c7ffddc909bb"],["/tags/HBase/index.html","a3d39d8b4c4c71c4ea172ad977a60442"],["/tags/Hadoop/index.html","cf3bef6baadc486fa74f475bb09cd9c0"],["/tags/Hadoop/page/2/index.html","cee8596ea0d5b9a1e3a926e93f2e0b9b"],["/tags/Java/index.html","3ae4c3d07b5664a6257a7becdb008031"],["/tags/Java后端/index.html","2d6bc67f8351e9218d96fbb3f6a6c272"],["/tags/Java后端/page/2/index.html","78aadccbda069551eeadf2290db077f5"],["/tags/Java基础/index.html","c5963dba781738abd982f6299da54392"],["/tags/Java基础/page/2/index.html","968976a1a7188e40ef7170c56c10a6bd"],["/tags/Kettle/index.html","4a3343563fe370516eb062feb8ac0781"],["/tags/Kibana/index.html","e5effe5b5e8f8d0520d60def59204f20"],["/tags/Linux/index.html","e6cc86b9df368527a940b23d7d2dcacd"],["/tags/Linux/page/2/index.html","8300ad32a7d9d4097469a418424cabf3"],["/tags/Linux/page/3/index.html","8ba8ffb13627933192c1dd700f572a9d"],["/tags/Mac/index.html","8c55f5d258b3b1bc992c126e267cd5fe"],["/tags/Mac/page/2/index.html","2b6c183223ff8910559f39441a31504f"],["/tags/Maven/index.html","d755c187aa9591bbf5d7712bc0611712"],["/tags/MySQL/index.html","79073a2fa4560c26b3978031d5e28915"],["/tags/Python/index.html","1ec591ae4239e9cfb0bc7180a51be482"],["/tags/Redis/index.html","15945d4d1b1eba8d7644e0d5bc5199f8"],["/tags/R语言/index.html","e75a55f3012da0bbb266fb729f15a4a1"],["/tags/Spark/index.html","755e3533b7dabc1681ec504b2db0cc63"],["/tags/Ubuntu/index.html","33af0b46c87a94a806d1b2e6615a8eb3"],["/tags/Vue/index.html","2f94e123844f65fd42d9cbae12096dc4"],["/tags/Windows/index.html","c2cfa7536d99113c59fb01b246f44d32"],["/tags/ZooKeeper/index.html","1477739b2cfa724919b856eef50cb7eb"],["/tags/bfs/index.html","c223114ec133dea1c58fab35ad6fd470"],["/tags/dfs/index.html","4e340f7d37be03d78d454ba2f7387a57"],["/tags/folium/index.html","38a264265251af541bdac1e0a876c99e"],["/tags/git/index.html","aa3f2ffc689adec553e905017c6cb9eb"],["/tags/iPad找电子书/index.html","7951d490ec0ab9e963a4a381d683a44e"],["/tags/index.html","159de912bb7dcae519a098e2041d44d4"],["/tags/latex/index.html","c99ee6fd4f139fc8529714e0472df8ae"],["/tags/中间件/index.html","f0fcd1f42c771595dd25133c7b3991e1"],["/tags/二分查找/index.html","ef74208c2761e48529bc8046c2fb77a7"],["/tags/优化类/index.html","79f415b39eb44366c84669b62316e6c2"],["/tags/前端/index.html","33edcf3327fe2724851dff1f4cf99d67"],["/tags/前缀和与差分/index.html","5ae85749f1ea671bf4c5679c87a541f7"],["/tags/动态规划/index.html","453975830c8dd93c1aaaaa249835ebd3"],["/tags/动态规划/page/2/index.html","b36a11436aa262d9985863d894b60d72"],["/tags/博客搭建/index.html","bc1b42ef5a2a4a9f164ecee74a6e0f05"],["/tags/图论/index.html","5034775aa8590a6ec17ac1a6cc112746"],["/tags/大数据/index.html","72ec6eb9af5012b550c4910c7a6b1c63"],["/tags/大数据/page/2/index.html","e6d4b102f1315e8c526b4616ead8faae"],["/tags/操作系统/index.html","540039a5b9cac61f937aeaea81f0c947"],["/tags/数学建模/index.html","c1604080ea9acb22582165fef661956f"],["/tags/数据库/index.html","4e19a84b6df50959dc3d8dac26dc6f76"],["/tags/数据结构和算法/index.html","94f026d3c10ad34db6c40c1cb46695b2"],["/tags/数据结构和算法/page/2/index.html","93b73ab0bde48eb273cab877d3946da4"],["/tags/数据结构和算法/page/3/index.html","47f1a18e4f14e56eeea20e476c917ac1"],["/tags/数据结构和算法/page/4/index.html","67e23ceece2297c8ba4e141a00783edc"],["/tags/数组和字符串/index.html","69496bffac11dfcf20fd1143bdc511fb"],["/tags/数论/index.html","ce4191d09a3f7bfe42305ccc86053f78"],["/tags/枚举类/index.html","0e19e050fc0a9a25a894a5fa74f7b5e1"],["/tags/栈和队列/index.html","ddfac2d718af370da775e34d873d54e6"],["/tags/树论/index.html","defa5a5a0c39bb7bab9734a050185a35"],["/tags/测试/index.html","516b60310d4679786747c69175ccd148"],["/tags/环境/index.html","b2cfa4b1b5ca139fdc6130bbca1c17f7"],["/tags/环境变量/index.html","58196ca9636c2c9fd90dc7c7814e0ec2"],["/tags/绘图/index.html","de16ffb8100fe5da5e7ab5e1456b5ba0"],["/tags/编程工具/index.html","ff04feb62047893d31304797223bc2e6"],["/tags/编程环境/index.html","f13402e3b43cb75cc7da59296032c66e"],["/tags/网络编程/index.html","a5f94e2f2d0cbd8a54b0a51f06a74e90"],["/tags/英语语法/index.html","6ce5ed602b5bc5b4799e2a8261ef2e08"],["/tags/计算机操作系统/index.html","288cca431e81f2ab632755c0fe2a8e5d"],["/tags/论文/index.html","21f884153c09048b526e668ca1bfcfac"],["/tags/资源下载/index.html","19a9c8bcf325e76950ff4593529e2c15"],["/tags/链表/index.html","01c5c21abb8b1b9169824d83993d04e0"],["/tags/集合/index.html","24bd79c53bef6bab2c3ef5ac6d25a411"],["/tags/集群/index.html","dcdce72ce39f5d609a6f867d1df2cddc"]];
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
