/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","dfdf068481603783641b8b2a7860116a"],["/about/index.html","599438f318f91646df1d53d78d6e68e8"],["/archives/2023/01/index.html","d1520acd83afbbd0046a927267e88656"],["/archives/2023/02/index.html","71c211819a4e7e0a6bc5781f6c85d997"],["/archives/2023/02/page/2/index.html","29a6a2b848b126ce75e2080459bb9bba"],["/archives/2023/03/index.html","2c31a7cd60294b1483e009edec11da5e"],["/archives/2023/05/index.html","4d54b673158eb37d47633a0b6fef9942"],["/archives/2023/06/index.html","b53901dc3e81e0573230069155bec560"],["/archives/2023/09/index.html","a99c77efca8e5fb3398429a19760e100"],["/archives/2023/11/index.html","78c8be187d1f61ebb248117a5fba0c65"],["/archives/2023/12/index.html","94b318358787570749923a19929e21d4"],["/archives/2023/index.html","5151b98cca650dc85bda25ff8eb53006"],["/archives/2023/page/2/index.html","ce76a01b4e2be0c4a1e5e47dfcbd1b8d"],["/archives/2023/page/3/index.html","2f5470a3c707a1f326dcff1ce4e7b6ec"],["/archives/2023/page/4/index.html","e9a81ce661e14896b40265ef93cbedb5"],["/archives/2024/02/index.html","3ffa1099f9f5deb9ddbd4af34a628402"],["/archives/2024/index.html","23174be53ae863b9d337b6498a4d3f58"],["/archives/index.html","c8cfe6b6b67c21c5422206bf713e3c66"],["/archives/page/2/index.html","548e8af52ed1abfe2ecd4ceef9ce508b"],["/archives/page/3/index.html","2e2d29872ce690c37cc2064c7e1dce16"],["/archives/page/4/index.html","d294785ed7e497e34e3f8ffa93fcc5a8"],["/baidu_verify_codeva-qQP2iZOMLX.html","b2d5968f7f3c43d14634e980182b28b9"],["/categories/Java/index.html","371a6d290b46119894b60fc6c8a9d694"],["/categories/Java/后端/index.html","52c39e54e2490c3aaf6d96a731674872"],["/categories/Java/基础/index.html","dc9d3cbbca8efebc2a596f3235d23e2a"],["/categories/Java/基础/集合/index.html","3bb70bb62eddd9d15945fc89cbb91679"],["/categories/Python/index.html","5a97d53bb003e877220d9453bcdbb59e"],["/categories/Python/编程环境/index.html","f273a3d33408d8bdf319f94bd4fd7153"],["/categories/R语言/index.html","36eca178bdd4402893d747b5e3ad2b6d"],["/categories/R语言/编程环境/index.html","c6b2e9de99573ece36eaaa62bfec634f"],["/categories/iPad/index.html","34f292bcddf844d8a7828f0a169973a8"],["/categories/index.html","5bd9eb7bc5866e4bd570906a3c04663b"],["/categories/中间件/index.html","0f34da14a2b959e23540405cc69821ef"],["/categories/前端/Vue/index.html","72179b5fd734028b97b81adf8d0dddd5"],["/categories/前端/index.html","4da31dcf31902e0a11bb63b3266377af"],["/categories/大数据开发/ElasticSearch/index.html","190cbbf5f8f662ab17b0d94efe9b1ed3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","99a8a6095f35af78304da0be1cf56b6d"],["/categories/大数据开发/HBase/index.html","2411e463278092882ce203e4e1d0bd89"],["/categories/大数据开发/HBase/学习笔记/index.html","c295953dd9fba670cf8b3f9a023c9bc3"],["/categories/大数据开发/HBase/环境搭建/index.html","98ec97629cf7313b6230bf3454e22f72"],["/categories/大数据开发/Hadoop/index.html","519ae6fa2f720f3dfb7e8ef90308833c"],["/categories/大数据开发/Hadoop/技术/index.html","861227ec89227093949df2d82f3cec41"],["/categories/大数据开发/Hadoop/环境搭建/index.html","958ea6b25bf75b1acce121995fbeb466"],["/categories/大数据开发/Redis/index.html","cfa30738a3960dcd75b32513f4beaba1"],["/categories/大数据开发/Redis/技术/index.html","221b22eb54929d96e02068a4ce8cbd90"],["/categories/大数据开发/Redis/环境搭建/index.html","60db2ab36694e28377d617363e7e02aa"],["/categories/大数据开发/Spark/index.html","da29eb3393208d221d13406a49381abf"],["/categories/大数据开发/Spark/环境搭建/index.html","582cc108028a14f15d4f4003513ad259"],["/categories/大数据开发/Zookeeper/index.html","2d5608c2aefcc3f7ec51b14da352a843"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","df838e104ab86b018063d9288114199f"],["/categories/大数据开发/index.html","bd4cabd81e8255e66366af0cde3bfb20"],["/categories/学校课程/index.html","8ed651f12e66fe51a9858eed46bd9a7e"],["/categories/学校课程/计算机操作系统/index.html","b3b1f14bda07c9f6504e3431ec68156f"],["/categories/操作系统/Linux/index.html","9093803de0dd0a9e9ffa8aadb2c992c6"],["/categories/操作系统/Mac/index.html","9e16f709f36a4c61cc77adbd90deb1a7"],["/categories/操作系统/Windows/index.html","3cc5c1c878fe9795dd154506b928cd52"],["/categories/操作系统/index.html","f27159244ed8eb1f69dc4d8324808809"],["/categories/数学建模/index.html","1dcc80564bade081a81e34edfc66c3f1"],["/categories/数学建模/latex/index.html","1b0885ba0d8645bb582c209365dc6fe8"],["/categories/数学建模/优化类/index.html","bedca0b0e5ca950d141528b0f5a54432"],["/categories/数学建模/优化类/现代优化算法/index.html","1ee8227331de6cd1b426d1d98fb2b9a8"],["/categories/数学建模/优化类/规划类/index.html","82d11b91f809092794dcb7eabea3dc18"],["/categories/数学建模/绘图/index.html","8ca60782e35ba05fa54136432c1037b4"],["/categories/数据库/MySQL/index.html","b5f6785c1745f35cdb6635be7fe100f8"],["/categories/数据库/index.html","4efdf8cd489c5125cf6077f0b658bc11"],["/categories/数据结构和算法/index.html","5d6f87f4db95a940f3c5e1869cd2a4db"],["/categories/数据结构和算法/page/2/index.html","47e321a47d4a2acceba97f280cd2a781"],["/categories/数据结构和算法/基本原理/bfs/index.html","2e6292b821609e1ccfe414ad97ff47a8"],["/categories/数据结构和算法/基本原理/dfs/index.html","b34deb08fe483ebd2e007f71a68fb2e1"],["/categories/数据结构和算法/基本原理/index.html","5aedca57ac80d057e1d3586fd0ce6696"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4416673dbb7084c0344bbf2e83fbd1e0"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","57150a234dce63d8fcbc9b8cec4a9c41"],["/categories/数据结构和算法/基本原理/图论/index.html","43069b5e10d944ede3255f484854f68c"],["/categories/数据结构和算法/基本原理/字符串/index.html","9c5c62263e1cb380cafbbbdb9a800afd"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","3f999f40bc52a5f72bc657a9e13f34b6"],["/categories/数据结构和算法/基本原理/数论/index.html","06f576fc0d9929c702f54f276010d361"],["/categories/数据结构和算法/基本原理/树论/index.html","af41109b35bfdec2f765debe76507e8f"],["/categories/数据结构和算法/基本原理/链表/index.html","92cc9bad9c5892b17c1d0d8d79c9dd6f"],["/categories/数据结构和算法/算法题/index.html","b7f475a04ae9ce1e7c30f01d4ea11fdd"],["/categories/数据结构和算法/算法题/二分查找/index.html","e6f4ba35d97301f4da75990799251549"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","655f40d9883ecc47c014fd23b5579ecf"],["/categories/数据结构和算法/算法题/动态规划/index.html","f53501a61189b8982b10d7abbdd8569c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","5202f0fd511483d22933f9c391de5eb1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","a15309b110ba86f16038c8e37614c2f0"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4a42d4180c0370db596c9c10e041206f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","bd70e8b7e7f59203cd0ef79a58b46a12"],["/categories/数据结构和算法/算法题/数论/index.html","7070abce64d66a8a784578b06c65428f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8fe2ce10a5a0f9905bbb2e0685f21803"],["/categories/数据结构和算法/算法题/树论/index.html","b40ecc6efd13b5745c6cfe1643f25fe1"],["/categories/杂七杂八/index.html","f4e66d08ecf0ec2de59d6cd47bd12b62"],["/categories/杂七杂八/博客搭建/index.html","158a9f79eb18db01fe2ffef44aa1dfe1"],["/categories/编程工具下载/index.html","2a0f06bc2cd3384649f00821c6d32873"],["/categories/编程环境/index.html","3449e1298d885fca88c2b5c01853b3f9"],["/categories/编程环境/大数据/index.html","24e9e90825dba0d29175611eea9d1818"],["/categories/英语学习/index.html","b5b7913818b0ff7c9f1696d90b5a89ae"],["/categories/英语学习/英语语法/index.html","70aaf643370eed7d94616660d1ee8d07"],["/comments/index.html","a987c99d7ddbfceb3ae9d2fcd8177d1b"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","1513f20ec56af9a17385ee79cb49b5c1"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ec5cdfcfb8b0e861b86c789e35e75bbc"],["/movies/index.html","fcecdd8bd0762a8bf79953500ca9f9dc"],["/music/index.html","238d1c0ea6b7f15b42b920d74a5d3333"],["/page/2/index.html","38ccd9b1787588a4fba2fa56dceb3bbe"],["/page/3/index.html","44fca39645ba2f0125ad54a802f467bc"],["/page/4/index.html","dbc98d94ab88b524dacf3194d4fdfabe"],["/page/5/index.html","dcdc95a1e68e14c9986b72762b0da560"],["/page/6/index.html","7f730b5bc36b13e139ab418a248368cb"],["/posts/1021360842.html","6b93d98ecb9993ce5aeac32cf4b5647c"],["/posts/1120620192.html","3d169552bb26f473c4e4298f00771b76"],["/posts/1141628095.html","d8c1f055ae3600f87169ac0657278557"],["/posts/1168613674.html","f2218a99d834cc12258c4defcce98bb3"],["/posts/1219920510.html","d7225efa2beb5e192e8e9f455cb00c9c"],["/posts/1222166338.html","f78324a699e08e969e2af6f78c4bd65a"],["/posts/1259097482.html","46bf85bad4c673d910842b87a60dbd2e"],["/posts/1271036369.html","3985b8ab92eb42484ea49212873c6f06"],["/posts/1312847445.html","86daf88cf19fc0f6f99ff10a4f4f36ce"],["/posts/135355774.html","2f0c0dd5b914a6d5b7f597da9ea8855e"],["/posts/1375344716.html","a5ccb18716f5404ceeec05bf4203a052"],["/posts/1388991698.html","9d2d41ed1457137fb4374af36ea4b78e"],["/posts/1410315814.html","6771b13450827cbd50e02fc37ab67e63"],["/posts/1452790229.html","4d93d62ec299e0c93d0118cc88317c9e"],["/posts/1470079884.html","0c67cf518352a2612563de2853849dd9"],["/posts/1470079885.html","3ca4618dfc50a1a034a44ca2d128f624"],["/posts/1470079886.html","c5733129abb71914226f8ffb2f71c9af"],["/posts/1470079887.html","c78bccc16921edd6cc6668f3084242fa"],["/posts/1498536549.html","6bd71c1cd31f55c36963c55ff0d3ad8d"],["/posts/1539568593.html","d5ed6a3a5dd6ed99e0effb893be6f956"],["/posts/1547067935.html","92dcd6a0cd40bc63e7f20fbf52988556"],["/posts/1557866301.html","ab658fd920fc8f7a409ee8e54dcb4688"],["/posts/1571776361.html","6c616dceadc44c526d534694b0bfa597"],["/posts/1605124548.html","cf85db44ca49e64b214b88c741099606"],["/posts/1633036852.html","719a0c19cf14e16eac03fe52960a5de7"],["/posts/1674202625.html","988b19030d1c5e6cb67b4e1926616129"],["/posts/1765123828.html","55b5283d71f4ed8c58780508e0fa5e68"],["/posts/1767336200.html","0ca7f93f59f2ce1e20602610d2052a03"],["/posts/1776114197.html","224cd9628e095970cb1bab96f170e335"],["/posts/1817748743.html","83633b8c12e43968403c8ab61786cf50"],["/posts/1925125395.html","d28a94adce953db5984a036e8437382c"],["/posts/1966191251.html","5911ccf623e7cbec2e1183e8dda7d5f8"],["/posts/1987617322.html","2f9b67b8efd7953d34dd6975c59c5ca9"],["/posts/1999788039.html","423a588a5becc87c96e96d1b0a19121f"],["/posts/2075104059.html","ebbc2d78e45e7b5f24fd7a271536b015"],["/posts/2087796737.html","36cf0f829035b9089003dd76ae8b9506"],["/posts/2106547339.html","8159df59aa626c6765eafe754e8c3d07"],["/posts/2207806286.html","1077300be3162921ad2ded886ea7a7c1"],["/posts/2225903441.html","1547323fae317a752bf289b9c3da5e84"],["/posts/2265610284.html","89e814899c617933b539a0f94ab518ed"],["/posts/2281352001.html","9409274eed897636785660ec8a070f9a"],["/posts/2364755265.html","2f71c853f8dabcbf4360a03de5ff876d"],["/posts/2414116852.html","ea2e83747a579a10fd1c55c67b659af3"],["/posts/2421785022.html","02f0124384d285c62de2f7009617da59"],["/posts/2482902029.html","f42b15f77f64881958c2dde88f1c8b79"],["/posts/2495386210.html","4c90d67cfbf304c106ade0eb0a5ac19c"],["/posts/2516528882.html","a5ef29efd4c27318504210f0c0ca4c00"],["/posts/2526659543.html","2baf49a1fdc88bf4b3733059291f5a1f"],["/posts/2529807823.html","4258abeb12ecc48ccd26ae1c17487f16"],["/posts/2596601004.html","05fe0f134ed99166e5dab6c7bdf7c0b6"],["/posts/2697614349.html","11d1729fc238e64c96e43d583bac040b"],["/posts/2742438348.html","5665481a026765ea35e294ab9d235f4f"],["/posts/2768249503.html","293e624114b5bde1e41e2305a2048828"],["/posts/2864584994.html","5f25b0363be165ff20eec6fb1e7a0e5f"],["/posts/2888309600.html","8c5e9213c00a84c12641cc01e38bb854"],["/posts/2891591958.html","063e20add63d5f234c3c0b1aa3b61968"],["/posts/2909934084.html","eb7f7e47d33130ce758927d0433e5a70"],["/posts/2920256992.html","4d7cba041d330282c6b7dc1810c53e24"],["/posts/2959474469.html","2c0e05423d0b9afe3445cfd60ed8c372"],["/posts/3005926051.html","22de1011b6f64152fdc2e78ee9a0a201"],["/posts/309775400.html","65936c05e5f3eb3a91b2065205e76bb0"],["/posts/3156194925.html","8f949f95eba2c3b29e8e77cdee4576b0"],["/posts/3169224211.html","f8025088de95fe23958b2c95996f183b"],["/posts/3213899550.html","9cad4446089fc1722c9dfc775a361443"],["/posts/3259212833.html","f96a9c6d3d3162cf62972fa2d6977d8c"],["/posts/3266130344.html","8c71cc0131104f6afdeb092c2e8f3fb8"],["/posts/3292663995.html","9f18a16bab3937bb61acafa62145cf8e"],["/posts/3297135020.html","ed827c6477af37632ac5f654d8832e49"],["/posts/3306641566.html","7891b070f10c54522a855cf1cf4f7e96"],["/posts/3312011324.html","0db5ec98c218d9943c439d9ec0921846"],["/posts/336911618.html","a79000c0500bf77dfa328ebce055807e"],["/posts/3402121571.html","c9e801f065dcd0366d0d98428cd12187"],["/posts/3405577485.html","ad53e9481f9c37c785862a0ca7ab6768"],["/posts/3498516849.html","cbe653ea66567e6a16e6075c6cb766e7"],["/posts/3513711414.html","7888c7a512a57bfdfb790b66a72f7789"],["/posts/3523095624.html","3a9cce0b9ab71a97ab816e55b11236ea"],["/posts/3546711884.html","ce968df4e68f221e39f6c906a6073d51"],["/posts/3731385230.html","e9941a5a3a9b86530d81a8e1fc227361"],["/posts/3772089482.html","779b4d9b6cd83337292828ed83e4e9a3"],["/posts/386609427.html","a24dcc746f9600970d2a680619e93d95"],["/posts/4044235327.html","405cd4ecddd454ad7d5b5c42522aa779"],["/posts/4115971639.html","33a5c192b6d67ac4bd242208af1beec0"],["/posts/4130790367.html","966be0f919b2bf755d1ad400fadb1421"],["/posts/4131986683.html","777ae5b3f0ececc4492ce587d1a05a78"],["/posts/4177218757.html","1792d0b56a835ba79e4f8b3e96eab898"],["/posts/4192183953.html","3982065edfcb83cdcc4dc273537599e4"],["/posts/4223662913.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4261103898.html","0f15477cbc209e4f94440db4b0472b77"],["/posts/469711973.html","ef955745877de7eee5961fd633f951b3"],["/posts/482495853.html","f3e78f36b6223bdebe4e05fa6f932094"],["/posts/488247922.html","fff7339b036fa0fe4a16347386684a7c"],["/posts/517302816.html","f788e574c0c189289c3d105e6ad4f311"],["/posts/570165348.html","531ad2bab37e4c650da5417644f285bc"],["/posts/595890772.html","48a5710c68b2e75403a7f97e2405b6c3"],["/posts/67485572.html","022a4e755d28ddcb263af062e8f7ac74"],["/posts/694347442.html","042b6ea70c1ddd8ceff2912fe7aa02f9"],["/posts/707384687.html","37158971530ef51cb922e47198a4a928"],["/posts/71180092.html","c217f44423636f96d4d7e79b36ff8463"],["/posts/716459272.html","abf1247e2c43e9a34f9caa10c3341634"],["/posts/765481613.html","0426a376cd5bce458737c9993ec92256"],["/posts/778231993.html","49bf4d8b5729fc4574f25f1fbf169457"],["/posts/795397410.html","cc25cdd361d5948f64db3392ed89387a"],["/posts/820223701.html","679b9583790de7924df9959724b6735a"],["/posts/830372185.html","ad0dc790bd8bbb3a2cfb800a0d59728e"],["/posts/88294277.html","0c1a8abb48c885b0e8f979065f6d8cef"],["/posts/939963535.html","0fd604f043bbd8110a3326fd342dd5c1"],["/posts/983786067.html","05559f31d1aa096f0d36b876979c1dea"],["/sw-register.js","604fd235b9fc14423f98131a4581f47b"],["/tags/C/index.html","85d530ea8076424abf1de32323e3860c"],["/tags/C/page/2/index.html","385008b8f519ecaaed2f2ae07db9c3d6"],["/tags/C/page/3/index.html","4a2167d7fe2f221ebea50476c54057ce"],["/tags/C/page/4/index.html","d6095583e598cca2921707e68d1d02ca"],["/tags/ETL/index.html","ed0cd4c3eb135480b9a88326ed34cfcf"],["/tags/ElasticSearch/index.html","3409ce80e56502e8b667a78780753c41"],["/tags/GUI/index.html","a94ce7c76f778303aee0f814a87957b1"],["/tags/HBase/index.html","26d9b78657d80e39ec58c65eeceb09d5"],["/tags/Hadoop/index.html","65880c1d61d6f0b2c4747ef022e74e47"],["/tags/Hadoop/page/2/index.html","c09858685a2a4971885428f62382c503"],["/tags/Java/index.html","46870a0ee5f34605deccc8be31b9f9f8"],["/tags/Java后端/index.html","97ec8e1fddd2f72350052e6e34269504"],["/tags/Java后端/page/2/index.html","2acb3f0b2d96cbe55db3292691823e80"],["/tags/Java基础/index.html","2cc72a252d1b525e37dd6aaa5a0859ca"],["/tags/Java基础/page/2/index.html","233512ccdc5d16d50120a027d08ba1ca"],["/tags/Kettle/index.html","24cc651f5007cdc59d0460f9ca71ae98"],["/tags/Kibana/index.html","0edb90b4c59ddada9a574ce8202001c9"],["/tags/Linux/index.html","4775f720355e0c468424f18aec428e07"],["/tags/Linux/page/2/index.html","90248270444a3644fda307cbf300328d"],["/tags/Linux/page/3/index.html","da6cb92f905788036c9611d96dcb5d3e"],["/tags/Mac/index.html","6ccce25e08c9d74015c477ab91abfabb"],["/tags/Mac/page/2/index.html","2a95d0dc2e7d11787782b2379b17b5f0"],["/tags/Maven/index.html","e2ba02042304a7c9146c9c6edefd290b"],["/tags/MySQL/index.html","5a9928795163fee7dbfae2e1bd64e2eb"],["/tags/Python/index.html","e50e2c8d2d9b199c1726e6499abd37aa"],["/tags/Redis/index.html","febbb031a0945ee09868f90f1800f896"],["/tags/R语言/index.html","1200bae737d8249b2fbb41d96a119924"],["/tags/Spark/index.html","f8c4aa126ec974f5cdc2f98a75515f52"],["/tags/Ubuntu/index.html","2bbabefb21d2abd50d3fa7e6453a65e3"],["/tags/Vue/index.html","178d3ff4ffa9d99f05cb1a7a238df41a"],["/tags/Windows/index.html","358b54523a34282ce8e8d3c3af7e3e42"],["/tags/ZooKeeper/index.html","6737ac09f6444039ca519f01c0c2716b"],["/tags/bfs/index.html","a4c49f3ae3f15802d1fbaa271e5bde24"],["/tags/dfs/index.html","ddbd6deeeee3767a6bda4feaa04105ef"],["/tags/folium/index.html","4a90867b809b4871d29a094d3fcc9c6e"],["/tags/git/index.html","bf23d100c241c7ec3ebcdb35064f6acc"],["/tags/iPad找电子书/index.html","dcfdcfaf5434e4c762eb985bac50a0bd"],["/tags/index.html","c483cbca168fa13d778c8239d8ea9199"],["/tags/latex/index.html","5f8d4933ac1b597aee0e45244792e2c0"],["/tags/中间件/index.html","a8cc71bb0857e7ce12c6b68e9110903d"],["/tags/二分查找/index.html","a429390666675c83ea364085c8603be1"],["/tags/优化类/index.html","c293b20f647d39c55ade91d59975f341"],["/tags/前端/index.html","0ea6720a270329d997343e794cd1d60a"],["/tags/前缀和与差分/index.html","58cf5b263a5f3414da494df3a5e84468"],["/tags/动态规划/index.html","e3a5ad2385706e7879bab69e9826d002"],["/tags/动态规划/page/2/index.html","e1d0feb1da5e96614ea5a74d8d89f076"],["/tags/博客搭建/index.html","515a29e1e617ab39db842fc05cd63ee4"],["/tags/图论/index.html","104c8ba6cde4ddc5ebd2bed543949780"],["/tags/大数据/index.html","d7510e8c143eeab611c2e0075d1944ce"],["/tags/大数据/page/2/index.html","5d289fea75486059882d1de6fe3057eb"],["/tags/操作系统/index.html","eb742704d5a669ec48a58b8aa1f03c4d"],["/tags/数学建模/index.html","510084abfc75bcbf2a29283c52c8d12e"],["/tags/数据库/index.html","3f41cdd6d7d13a05c06878a3131c7841"],["/tags/数据结构和算法/index.html","6564a44229ef9d916cf5c6efbc76578e"],["/tags/数据结构和算法/page/2/index.html","fa27239719b8dcd691e6f9a29d7fdd6d"],["/tags/数据结构和算法/page/3/index.html","350c2c5c97f8faa6d408cc2a40bd804f"],["/tags/数据结构和算法/page/4/index.html","87e046ca6266b03042aea79fc57b41b7"],["/tags/数组和字符串/index.html","f2438c3f1cdb72e6bead5a399f525c5a"],["/tags/数论/index.html","893ca0f19a65f35afea0c7574aaa18be"],["/tags/枚举类/index.html","8fffdcc745a4b9a54ed5a8409506ebe7"],["/tags/栈和队列/index.html","f034f05943c2d2b51413f6ded251f79c"],["/tags/树论/index.html","3c744dce259e8ccd96423b19e6a4e53f"],["/tags/测试/index.html","f44f10c98c75bece3cfa891b3f5d5fb9"],["/tags/环境/index.html","41439450a8e04acff4e5783638c511e5"],["/tags/环境变量/index.html","2d1dad81b528c2fcb12848158c92ca4a"],["/tags/绘图/index.html","ba6b8940bd438cce195b0a612b59ca75"],["/tags/编程工具/index.html","5d9b67c67fbde593c6b99be594b30bda"],["/tags/编程环境/index.html","402d9ddbc648316486f141a396cde762"],["/tags/网络编程/index.html","bcf832e1aedbe072faa2e4715754fd29"],["/tags/英语语法/index.html","add3d13af66ce4b118c3fab97b968d71"],["/tags/计算机操作系统/index.html","ca7d65dc149efa30e9847a3b269cc5f2"],["/tags/论文/index.html","dc197e0641693238c1459d82041dbdb9"],["/tags/资源下载/index.html","f894d1755e97a1be093eb20535ca219f"],["/tags/链表/index.html","4419e96662f16ec22aee5f61edd9d84a"],["/tags/集合/index.html","8dfec220bc1d2c23a3e45b7996c29714"],["/tags/集群/index.html","5d566758db77cfd3110706f5fd0b657a"]];
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
