/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","c83b518662cd6d1eabc373c2f4947db6"],["/about/index.html","c8f66e764959b55f7fb3433a1c12eaaf"],["/archives/2023/01/index.html","243771780e3ab500929affa65f4bf1b6"],["/archives/2023/02/index.html","1f86bb72c3418fb881ec60f2a738b022"],["/archives/2023/02/page/2/index.html","30fda52e0ab6ae446bf545c409ee794a"],["/archives/2023/02/page/3/index.html","69b5a6c91242287487f750f733d3f014"],["/archives/2023/03/index.html","4114f1aebf2300c998e0a86172ac7ea3"],["/archives/2023/05/index.html","6c761a4f3c8ead594fb55b455237a74e"],["/archives/2023/06/index.html","56b810658f07d174a4f3af6fdfa913b2"],["/archives/2023/09/index.html","8c93867076652750a11752e5486d814f"],["/archives/2023/11/index.html","8eabb71138fe089a7cb16a524ae53166"],["/archives/2023/12/index.html","bc42d9f0310b4c318d4a2d29d75c60b9"],["/archives/2023/index.html","50a7620073daae5381c2e8cd78636e7b"],["/archives/2023/page/2/index.html","2e8068affade9b891f5a10afaffc3c12"],["/archives/2023/page/3/index.html","605da61b34d19a6657947eddaf947a6b"],["/archives/2023/page/4/index.html","760a17d08e0a04789489128a4d95890b"],["/archives/2023/page/5/index.html","5970748a1426afed79718da8dca2b1e4"],["/archives/2024/02/index.html","898cd491e77ffb5532840aba2aa0334c"],["/archives/2024/03/index.html","d8c713d2db0762940cbedd5873b9bbf4"],["/archives/2024/index.html","a9c0485a5d450b7e70c4b961498ad3b4"],["/archives/index.html","43e212b9159b874482bf8a33045b1698"],["/archives/page/2/index.html","8d95c9fdeabcb7abb54d5284ccb2fee4"],["/archives/page/3/index.html","0f4fbe6d0a138b569a02f28118937331"],["/archives/page/4/index.html","114f28ebc47edd09b664ca5089ced30e"],["/archives/page/5/index.html","3c78e69d845b28cbc3d7affa90977d7e"],["/baidu_verify_codeva-qQP2iZOMLX.html","02dc2262ba9c517b171ec40fe7771b18"],["/categories/Java/index.html","a635e4aa0c048513983cde7d469ded7b"],["/categories/Java/后端/index.html","600b1317bcf1daebe383492b171bd50b"],["/categories/Java/基础/index.html","a8b0e27487508a74dacb102b2c914397"],["/categories/Java/基础/集合/index.html","c653bf8ddfd1c82a99708780ad4570d2"],["/categories/Python/index.html","c932ffb691bc1bd137c25f3b45fbabae"],["/categories/Python/编程环境/index.html","f987ab1f33370c7fc363472bf261e689"],["/categories/R语言/index.html","85d3e051a08abd18d4041eba58134ec7"],["/categories/R语言/编程环境/index.html","985c27e7032db2f563727fec0dde4791"],["/categories/iPad/index.html","dda4611f793117d94b143db5f2748cb6"],["/categories/index.html","295b23c90ba8f2d8ba57e5740c5803eb"],["/categories/中间件/index.html","a5142118ca39298e61324307774bd4bf"],["/categories/前端/Vue/index.html","a515e034d80e4102489eb4345fa7fd4c"],["/categories/前端/index.html","1249b1f5ed46d6460034c1940e974623"],["/categories/大数据开发/ElasticSearch/index.html","5b6ae500e7cd1acc47a718e71cd1111a"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c1519d3d6058178d560b502eeedb2974"],["/categories/大数据开发/HBase/index.html","8848b93b0073e0136b11a39097abb543"],["/categories/大数据开发/HBase/学习笔记/index.html","52e3fd3a364c29c7deb44add37c84437"],["/categories/大数据开发/HBase/环境搭建/index.html","65c2f93ea841ac1538adb39193b2912a"],["/categories/大数据开发/Hadoop/index.html","29bc9ad078c2ff6f41fd3bc34c4ba09b"],["/categories/大数据开发/Hadoop/技术/index.html","6d6ee8a3e568c10c0c6496dee870d507"],["/categories/大数据开发/Hadoop/环境搭建/index.html","df9031b20a21a5765eee1544525d2a6d"],["/categories/大数据开发/Redis/index.html","1ad46ed9a81983c9f54da9037bd1fd0a"],["/categories/大数据开发/Redis/技术/index.html","16c4260a553422cb511c904cf5697a83"],["/categories/大数据开发/Redis/环境搭建/index.html","87c1a3ced7e736b80f3f68c1e8439b72"],["/categories/大数据开发/Spark/index.html","dbbf050a8c35ec80f331b4b1a7ac89ab"],["/categories/大数据开发/Spark/环境搭建/index.html","2951fc634f53dca3f31383f0918c25ae"],["/categories/大数据开发/Zookeeper/index.html","5f3151b730d7507f4dc461feb35a64a4"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8112734651a73d653807274e534c98db"],["/categories/大数据开发/index.html","24c8f660d0217e6f018d19432de49042"],["/categories/学校课程/index.html","51fae82fe017c9685b4f713118603199"],["/categories/学校课程/计算机操作系统/index.html","181df4c3c4766fd9ae7fc7da181cfce0"],["/categories/操作系统/Linux/index.html","0479906504bbcd7e55781109af817806"],["/categories/操作系统/Mac/index.html","9a931b81704641bf9c915e6acea9f776"],["/categories/操作系统/Windows/index.html","7899b847db1e17ee143a110e6093fbd4"],["/categories/操作系统/index.html","8da8a04710004f084e7b9ad91ca07f5a"],["/categories/数学建模/index.html","98e9b62d79b6d51ce50db795903b58ad"],["/categories/数学建模/latex/index.html","cb521c86b46ead2952f3d7f1d01a8495"],["/categories/数学建模/优化类/index.html","d014efd2afead4bbac187c6b32bdd318"],["/categories/数学建模/优化类/现代优化算法/index.html","3541c16c29ddd1a05aa932c947957b3e"],["/categories/数学建模/优化类/规划类/index.html","b04bea20c2efcfdd3b5a211b209ec55f"],["/categories/数学建模/绘图/index.html","07a4618c0e828769336855e58d938f73"],["/categories/数据库/MySQL/index.html","16ad045f65deed7f47e5263ce7f4c726"],["/categories/数据库/index.html","e02b3eb4d553e4821b4451941375b162"],["/categories/数据结构和算法/index.html","cf5d6ef90ba14c230eb01aca64c46728"],["/categories/数据结构和算法/page/2/index.html","777e55a2c2b57d0ffa3e475b2ce6050a"],["/categories/数据结构和算法/基本原理/bfs/index.html","531ec359d6e36a1c75f600b0ec33b5af"],["/categories/数据结构和算法/基本原理/dfs/index.html","381ef86c2e966ceb6ae8a72cb332aa26"],["/categories/数据结构和算法/基本原理/index.html","952b160577277bae94b649d331c44e7b"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","a1861a8cc41aafe7c46ce26e331cfc6a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","553bcea378589d6250628fb00fe9e02a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","bf1d69ee606c70aed00f879c7f5de482"],["/categories/数据结构和算法/基本原理/图论/index.html","f57148addccbe7b9b2a3b1432a195c59"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","1bce7a7e802fb243d1fb32eae06332c1"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","982ba78446c72d943e4677efb542f633"],["/categories/数据结构和算法/基本原理/字符串/index.html","e4eb6941fbdba8b9398303e1523a1b2a"],["/categories/数据结构和算法/基本原理/排序/index.html","df7e6215bae409289405c045b3be3805"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","83c795659b0c14f9c4d182b4036210ff"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","755c27358d0f7c4ed82d793bcdae3e67"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","657d6da86fede7e09fb8b2e75be30ff2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a24ca731b7312c5a638e819b45923c4b"],["/categories/数据结构和算法/基本原理/链表/index.html","88cdbd8969e65900b6ac54c124b55590"],["/categories/数据结构和算法/算法题/index.html","a485a554c9b8ea4e49ba10738cba9633"],["/categories/数据结构和算法/算法题/二分查找/index.html","5c36a410b798d25be3f4a36d91803f56"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","012931504fe240041e1d5d6f3d112ab7"],["/categories/数据结构和算法/算法题/动态规划/index.html","8d46a447c9fe675ff7abffc3c0a08a94"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","1190f5154344c95d77b50b8e2548c12e"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","01be73397e3ec521b621ab07290b044c"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a7a7f3588d8af06080e1fd53c02e71d0"],["/categories/数据结构和算法/算法题/图论/index.html","b4cfb28cd084b8334c5648459354a997"],["/categories/数据结构和算法/算法题/图论/树论/index.html","a489a2a59a244abfebfb5813cebc9182"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e5036f80084df946076e01dd1094d0fb"],["/categories/数据结构和算法/算法题/数论/index.html","a263e531c5f17aafc846e1fd6207c009"],["/categories/数据结构和算法/算法题/栈和队列/index.html","cd89bece41d8a0738de229cc97b4469f"],["/categories/杂七杂八/index.html","1772320f17098aee93dc4f0624ca0be4"],["/categories/杂七杂八/博客搭建/index.html","65f6143581710fce8f41d17d87ffa745"],["/categories/编程工具下载/index.html","6bbdef15a5e02a56769a0fc620476a5b"],["/categories/编程环境/index.html","0e580b9f32d663c87e65e65a10b3ba2c"],["/categories/编程环境/大数据/index.html","6594d5ce4013603ae6f92b0a980600c1"],["/categories/英语学习/index.html","a4226489684e9e643f68105b4a1190d3"],["/categories/英语学习/英语语法/index.html","a5621aa46b20a0004fe2a8cd59eaabd5"],["/comments/index.html","35ed2a37198f71ae1abaaca7c29026c4"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","4d6529b0728543d8f76006fd2cafdf5f"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1b1bef6df06ef3d9fd567793655b3fe7"],["/movies/index.html","2530f9797ab4296a48918b2aa869bc8d"],["/music/index.html","512d8b513c5ba0ec0ef86a379f95ec12"],["/page/2/index.html","b3405f713bee771d4fd52b766558d99f"],["/page/3/index.html","6b47f2c8b314501bc82f3b3ace2b35b8"],["/page/4/index.html","501a6ab058e0e5f2fe315f960bac91e0"],["/page/5/index.html","d3ec86c928cacc38fbbe59bf9c54b89a"],["/page/6/index.html","9aec8494b8bff51e2c1e5f5c1f050c81"],["/page/7/index.html","d456e8628c91f11d0f352929364f646b"],["/posts/1021360842.html","080d38ee4c98b5e66fd757cd78cabbde"],["/posts/1120620192.html","2d4a5e30f1aa8c2160c7df04871308c1"],["/posts/1137707673.html","dda2270cd93ac82d5f53f0460f4d353e"],["/posts/1141628095.html","29217115d520a7cca249c6d16fe2e7af"],["/posts/1168613674.html","9f9ce7444fc6a8ea76b8c252b04321d7"],["/posts/1219920510.html","f47d5887f8f3ac0686da1f7825aaf640"],["/posts/1222166338.html","a455b29f556f984303070ab28a7d5dd4"],["/posts/1259097482.html","c3f8b2fbf49a53563f5714f5e46aceb9"],["/posts/1271036369.html","14d85799eeeb4c027bc77d065b5733fb"],["/posts/1312847445.html","6e66cb581f314aacd71dcc5a18c1f75f"],["/posts/135355774.html","7c669b025c75be929192a4cbf4abe056"],["/posts/1375344716.html","5940ba21ca9320c7dcffb9875911bf51"],["/posts/1388991698.html","9fe55372a04f3428655710e5f6eb3670"],["/posts/1410315814.html","868074ccc3a278b9839e4175dbe151e2"],["/posts/1452790229.html","bddc85cb6229b237f6e9d2d1170a0b32"],["/posts/1470079884.html","00d472b0053e9e0e44924a659af9b34e"],["/posts/1470079885.html","d525ec9c990f49e1c9b09abffd14b35a"],["/posts/1470079886.html","98231892a42bcc28e6a2d8251ffff6ac"],["/posts/1470079887.html","8760847cbfc7cfe95ddbba3f1408550b"],["/posts/1498536549.html","eb333d6ab9d69f7c6350c8c076a2a26e"],["/posts/1539568593.html","c14df50b9c6c6b8d2cf1050be220f310"],["/posts/1547067935.html","961e3ca924639abdb765450d4cb4fffd"],["/posts/1557866301.html","ae88297e2b2e63866b617aeb9eb28801"],["/posts/1571776361.html","74e49d4dc957eaeac28856eb368c52e1"],["/posts/1605124548.html","65dc3ee3b3cf3c7f7b9880c266605ece"],["/posts/1633036852.html","d7543b8198643982ba61e1d5aed1e3cb"],["/posts/1667740714.html","7052400353901f84b4828ac7073f4147"],["/posts/1674202625.html","7c0aa887621fc780fa06113bddc7ca01"],["/posts/1765123828.html","3de2698df4755734c5708490ec7a5b88"],["/posts/1767336200.html","33b07d00273543f8a3dde1fecd5ba64e"],["/posts/1776114197.html","9c3a2f12666e38139ca9380f26de5025"],["/posts/1817748743.html","dbfa1b5bef2ce26477b9c729b6c5bb45"],["/posts/1925125395.html","c94ea8492c8f38f4fefb4bf64a2caf79"],["/posts/1966191251.html","c1e5dec46d602cacd5c26604ece16827"],["/posts/1987617322.html","5841bac52bacf5e1b05b56723c936836"],["/posts/1999788039.html","6a981485e77595ab922d2145ca7d84cc"],["/posts/2007534187.html","659248db9cbc879f4c0f523193849373"],["/posts/2075104059.html","c7ebb9170fa3731bf016eefa821aa46f"],["/posts/2087796737.html","d683de1477fd4f0948f3c4e1863664ca"],["/posts/2106547339.html","a92f109e20780c0aeced389c0e0dba29"],["/posts/2207806286.html","a3ed55c95344d4f360481e502b393446"],["/posts/2225903441.html","2e626a1c7eeeb1ebdf81defe5145af0a"],["/posts/2265610284.html","0d112420be07b5570141737d6697ec1d"],["/posts/2281352001.html","adb1a5612e72f6301907a3364cd6b666"],["/posts/2364755265.html","056de43e74a148fd6fa56e24a3811a70"],["/posts/2414116852.html","252bef58d81ce024b8f8c077ab3cc1f0"],["/posts/2421785022.html","d439cdb63c4ca9c26b8840c953c24805"],["/posts/2482902029.html","23c201412c5b14518324b081074201a3"],["/posts/2495386210.html","4bcae64be42f94e7ac56796253478c42"],["/posts/2516528882.html","bb9e37e6432089a37163cca1e8007190"],["/posts/2522177458.html","28078af542e60317242bb16e674fa32e"],["/posts/2526659543.html","bdd02f886fc6c23e773fa55afe1c6f96"],["/posts/2529807823.html","869d2f1f5164ae620bb3fdcd3a04732a"],["/posts/2592249117.html","663b9affe07dc70565d8b1d9e05f3975"],["/posts/2596601004.html","9effd5c92126e039c4072e1d28f38c49"],["/posts/2697614349.html","41e36402cff0ac625c91c4066f0f9bd0"],["/posts/2742438348.html","810f13edf6ebe92af1d527d7b1edc7d5"],["/posts/2768249503.html","ab6d33fff5ad5a435a85e0b7d8f86eae"],["/posts/2864584994.html","85b98e9f2253d7afcff8cf385de73fa6"],["/posts/2888309600.html","cc8f73f38daad1ff300adaf2e3f28c4b"],["/posts/2891591958.html","ff83116e2fcef2b284fd68c370a7d0e4"],["/posts/2909934084.html","a561ba2b464075684a0c149b5533aff5"],["/posts/2920256992.html","7001636d1490932572a16caa6a88113e"],["/posts/2959474469.html","164d61b320394601199da26d8fefa936"],["/posts/3005926051.html","da51b262d513586e00469790e0d65833"],["/posts/309775400.html","331726f0eb164e7eb9ed207b4bc8c8f8"],["/posts/3156194925.html","3a64f1815253e9b7e4083dd4939aff73"],["/posts/3169224211.html","da9a73000551faae1d2bec352d26b94d"],["/posts/3183912587.html","9d607673aa5d9b10c16daf0d3666a94e"],["/posts/3213899550.html","0db7008f7e8416b03de3f1f6f7c91fbb"],["/posts/3259212833.html","692d51be1e7e054c899924b9a661b7b4"],["/posts/3265658309.html","82fe99260271df3cc4237c9092551269"],["/posts/3266130344.html","59241c760c84836303c4f1aa8afacd15"],["/posts/3292663995.html","df02eb145b19d8de3b35422a87a4741e"],["/posts/3297135020.html","ce210a29655783d2fecca2717bf88a50"],["/posts/3306641566.html","14107b82f7c8c67ff02514ebe8bcbdc2"],["/posts/3312011324.html","3215228e4e96c4ccbe3aeb9d232732da"],["/posts/336911618.html","bbd827dc86fde9524ac4260c76732c20"],["/posts/3402121571.html","31daa2db3430d8c09c484f59372b3b08"],["/posts/3405577485.html","d81c4b2e8ec6f781d457eb4f41a175fa"],["/posts/3498516849.html","477a453165a19a1ae740a9c84e2affc8"],["/posts/350679531.html","db79f6077ba9f55aed86b212ba13f1b9"],["/posts/3513711414.html","e3f4a118d7b42c1a8372d69071321bd0"],["/posts/3523095624.html","7eb02cf71c7b5d35e8fb4e3c0dd17ae0"],["/posts/3546711884.html","c182815a510eb9c5e18cad1a5bb3b734"],["/posts/362397694.html","0ab72744531e4103982729f8ccfcd91c"],["/posts/3731385230.html","d8e70c51e3126466b27044d1f56e42b7"],["/posts/3772089482.html","a228245ce4ebdcb57cebcced8b3709c5"],["/posts/386609427.html","0ad641e10c9d71101040103d8ba55f00"],["/posts/4044235327.html","5ee4dd42cbe09be0b23d8aba3b5fb546"],["/posts/4098221856.html","30cb5651b55b5a49ec513865c2ee0358"],["/posts/4115971639.html","9ed8cd78cb8c5ea617946dfebd61618b"],["/posts/4130790367.html","729b27eb0b075d6c7f2921712ee0d33b"],["/posts/4131986683.html","5f5e2ceadf466e4de6d63921c4cc8826"],["/posts/4177218757.html","1b155f1fc9c98319bc7c568203ca8fb0"],["/posts/4192183953.html","7e46df654c7a4dd84892c5cced7af981"],["/posts/4223662913.html","d077c39428e6a0f5f35147df92717a8b"],["/posts/4261103898.html","9295bfc48bfffdb237510dc548e8926e"],["/posts/4286605504.html","762a9965adaba1dd99c1b160326f4e3d"],["/posts/449089913.html","957f56b9701bff509321b768ba976dd3"],["/posts/469277133.html","0eeeb2c83c03feb793c46b5647f2f581"],["/posts/469711973.html","5a9dec1961ad1f140e03eb6c1f0140a3"],["/posts/482495853.html","a689f35da2c003f50ac2aa0b439e8826"],["/posts/488247922.html","e8091968b01072282b966d0a72c4cf02"],["/posts/517302816.html","7a70b61b2a9b4aae965f06e235ac63ff"],["/posts/570165348.html","989a764188f7ccce4e71ef0794e1247b"],["/posts/595890772.html","8f4118a6526236682110e8138bcc320a"],["/posts/67485572.html","57a45c6d9964376c4e78cb38792f98bf"],["/posts/694347442.html","2f78d7e1a7b3618897582f4e13ec637c"],["/posts/707384687.html","f6d8364ede0b6d44656cfb27e3a34ecd"],["/posts/71180092.html","31702d3e2320c61c2ee6da5a441c1e72"],["/posts/716459272.html","dddc9c6cf6fac679eac72b963368524e"],["/posts/765481613.html","2aae4a9338cb63df39e51deaa836e2ea"],["/posts/778231993.html","cc2a08dbce2533836843823284377180"],["/posts/795397410.html","571e1b6fe480b2a80eee38b1914c61fa"],["/posts/820223701.html","1386737c3289e774471d45a7c4b38bc4"],["/posts/830372185.html","10dfc07994c7d1a72d6381b1294151ab"],["/posts/88294277.html","5bc174f085dee7c53ade05d3befc49b0"],["/posts/939963535.html","07d4f4640013ee7ed412f477fd80cb03"],["/posts/983786067.html","4ba6bca0fdd86ee2a0b56bfe3db6c4d0"],["/sw-register.js","e3a17453a0d23e792da4b4ae669f5cb3"],["/tags/C/index.html","1d93337e180c0b0b9c1c8b41b359c075"],["/tags/C/page/2/index.html","0edb65232deb792616173744b7bf03ef"],["/tags/C/page/3/index.html","a13a403ac0a633e5e98627823ad50797"],["/tags/C/page/4/index.html","fde302e4ffc67b8abf32473842d2b0a9"],["/tags/ETL/index.html","a9884f7f8e7eb6afd0c657cc15c4d5ae"],["/tags/ElasticSearch/index.html","48dabf85d17bdeec43a272e58f78e88e"],["/tags/GUI/index.html","0605bc3671c3bb26c7a5c92db30b1d7f"],["/tags/HBase/index.html","f36ede1c9ad64723f21a2e9af8540e00"],["/tags/Hadoop/index.html","5beb545446a51ee757b84e664e648d16"],["/tags/Hadoop/page/2/index.html","2f386c7ea53e18e8fd49a2078e9a8e18"],["/tags/Java/index.html","9c4357d75898c8b54a72deadade6223a"],["/tags/Java/page/2/index.html","60fdcc7040004411493c4e5511ce45a6"],["/tags/Java后端/index.html","97fff404fd26ffa4842d026a565a3267"],["/tags/Java后端/page/2/index.html","c84a467cc96954e93e0276c2ab62f598"],["/tags/Kettle/index.html","dddf648ee5b34ed5a4a2e6cfbbe283ac"],["/tags/Kibana/index.html","6d0b731c64c37742380d5b155b7ae8a9"],["/tags/Linux/index.html","e23601572ad9b9e864864e2a27d8db8e"],["/tags/Linux/page/2/index.html","9d137e5e0392970f3073a63016a5dd1e"],["/tags/Linux/page/3/index.html","64454a4de5cd8d2ebd3fcb2fd57153de"],["/tags/Mac/index.html","300c76a6a0f57a99284fdcc7b9c9742a"],["/tags/Mac/page/2/index.html","545a5dc3e7c48abb55708fa90de8ee95"],["/tags/Maven/index.html","511abdc8831f7a0ccd525369baaa4f22"],["/tags/MySQL/index.html","b10ea7a8978f235a276e57edd90a9f51"],["/tags/Python/index.html","38ac4b29f9cad8d6bd94bf0735bc722a"],["/tags/Redis/index.html","e7b0b0c9a3710ae97e6d796ff3994169"],["/tags/R语言/index.html","a37d47abdd67a92a989ce93a0f7f1c2a"],["/tags/Spark/index.html","a28a303ac1a41a4edb111defea71623f"],["/tags/Ubuntu/index.html","dc5dc1847ff9990086b5b602c87e599c"],["/tags/Vue/index.html","82c3ea8413dd2d98184a0368101ecf1a"],["/tags/Windows/index.html","592cf4b7e2fe2b8078d0264c4e9e886e"],["/tags/ZooKeeper/index.html","d84136d0a3683425a57c9e583ab2c1ae"],["/tags/bfs/index.html","d4a45f030872e3c892bd0c6e3ddeb070"],["/tags/dfs/index.html","e17d5cf780807f30d1abd52fbccef391"],["/tags/folium/index.html","c72e81fc5f3c0ed54c3e7fb7c29b9c19"],["/tags/git/index.html","b37c36bdc95e1d718e8a8c5db721c1ec"],["/tags/iPad找电子书/index.html","c9acd7bae890640cf3ae51fa84140fde"],["/tags/index.html","206ab3ec2f911400fc6e61bc0d848c04"],["/tags/latex/index.html","c1a0afffdbcb738059a28f1ea295ee6d"],["/tags/中间件/index.html","84374602d1ca13b88c6bbaf022ef5403"],["/tags/二分查找/index.html","219887d6cf41cd5907f2b6a15d66e2a6"],["/tags/优化类/index.html","528429a83358434070702f69c090fa5c"],["/tags/前端/index.html","b395bc125a47ac09f771fab9ca46d1f3"],["/tags/前缀和与差分/index.html","5f862ee5f818f62facb88313491cd20e"],["/tags/动态规划/index.html","92f5c984ee2ca15e6a5787fd1bab4a46"],["/tags/动态规划/page/2/index.html","556cbb8721531a0adbd6fecc11979270"],["/tags/博客搭建/index.html","1145ceb052dbe40585ade8405c1fc6ba"],["/tags/图论/index.html","e8f7e5169d054b37b79f3da6da98c74c"],["/tags/图论/page/2/index.html","e947240f8de92ce37ce3d1b92d77c3f0"],["/tags/大数据/index.html","2d339478e03d0c5fefcf44acbbd4c4a6"],["/tags/大数据/page/2/index.html","da0f607d0ac2d7c66e956aa6323fd24b"],["/tags/宽度优先搜索算法/index.html","fdc01e2d4c58d63d71cc30ca79120ab7"],["/tags/排序/index.html","3af4eb7d5b5d9052394c6e98b97f7407"],["/tags/操作系统/index.html","ec4bb38bf599fa2c94819525060b09d4"],["/tags/数学建模/index.html","e792c2c3b4232a0af6500ca675539180"],["/tags/数据库/index.html","2cccf855a0df9fd7ad8fe85bcdb96414"],["/tags/数据结构和算法/index.html","91e2fa437cb8a5d554cf5a0ce8fb4a3b"],["/tags/数据结构和算法/page/2/index.html","fac91cc31b94bdb1f2700a2fec67799c"],["/tags/数据结构和算法/page/3/index.html","b541f3bedf9339c199fe19220a07011f"],["/tags/数据结构和算法/page/4/index.html","9daecbdcb65d757c39443481a7a6fdab"],["/tags/数据结构和算法/page/5/index.html","4c3bfbf2d6b8ffb0aba0f8ca26a8105a"],["/tags/数组和字符串/index.html","62a9aace47b8273c6d2147c2967346ee"],["/tags/数论/index.html","950df4509b4dee580e346ce1cdbd0245"],["/tags/枚举类/index.html","692f873587b4bd29272749c4d505a5ae"],["/tags/栈和队列/index.html","5e6b7b91ba29eb0f3a8652dc96542c19"],["/tags/树论/index.html","3c8928bd8558f41571c7199580b2ad8e"],["/tags/测试/index.html","fbe04c23a5ce10ba1a1be1c7adda5fdf"],["/tags/深度优先搜索算法/index.html","5c42bc18708cdac5d831db8ee7135170"],["/tags/环境/index.html","295465abcd31a00e5128d8c8869d3467"],["/tags/环境变量/index.html","9d096dc264e182c2a27b1baf2b2645ff"],["/tags/绘图/index.html","b383bd04ca7064c9cab707168b29611e"],["/tags/编程工具/index.html","d5c975b0a3a388cadb8e07ec97ad8ee7"],["/tags/编程环境/index.html","1c458ae224da99e23e78d8be1bacbf42"],["/tags/网络编程/index.html","6efa528ddba02f4714040d659373badc"],["/tags/英语语法/index.html","eef2a2af3eed985f76f2f33a5d034258"],["/tags/计算机操作系统/index.html","950e3ac2b7b7158c3729422f283d8e10"],["/tags/论文/index.html","e5386f2976d4bd1d74636b942bf20b29"],["/tags/资源下载/index.html","f4d683f32389348c721160aa368fce98"],["/tags/链表/index.html","ff181036feb0832ee080be74a1b19da8"],["/tags/集合/index.html","1329ff5d84d760610e55ef46f9c22fcf"],["/tags/集群/index.html","1b62bdc402ff2e6f8eb845e387a081af"]];
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
