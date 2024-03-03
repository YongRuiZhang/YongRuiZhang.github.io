/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ed86029eab301e831f47834b43f7eff0"],["/about/index.html","1cae2cbd9304aaa374dec9404df8b6f9"],["/archives/2023/01/index.html","493e03b991493a01f8ae57f733bd4535"],["/archives/2023/02/index.html","b93db48f6a5b466c386ed9ae79162cd3"],["/archives/2023/02/page/2/index.html","4230d6d4d2fb4386e5fac44934fb3de7"],["/archives/2023/02/page/3/index.html","62320fd417110b8c35b2c187607c8e96"],["/archives/2023/03/index.html","06fedd95faeaaefb6bd7c31da575bfa7"],["/archives/2023/05/index.html","d73fff2162072c9e2dc2968150a8e523"],["/archives/2023/06/index.html","aa05fa0d273523154e36b8fb5a46cc47"],["/archives/2023/09/index.html","dba067427269bd97970823917c620a37"],["/archives/2023/11/index.html","8ffa657c4ad708deb13e8801c7e6f80b"],["/archives/2023/12/index.html","c25e98082fcc36aa14f9ad72a785ade9"],["/archives/2023/index.html","3bf4763f32431a6735187f7ee3b265dd"],["/archives/2023/page/2/index.html","418c2910ad64bbc48964faaed157da9d"],["/archives/2023/page/3/index.html","d846bc9b5109f2044226b98fd9ae7bdb"],["/archives/2023/page/4/index.html","65217887318b6247cae818a65eb5f8d5"],["/archives/2023/page/5/index.html","fb5500c816d3601d158880203145a94d"],["/archives/2024/02/index.html","1cda0b6928ad058f0abed7f7a7b8b647"],["/archives/2024/03/index.html","bb3c346e1a13f995359711ad9a0a0bb4"],["/archives/2024/index.html","e87c80598cfbb66b2d934a0c3f168960"],["/archives/index.html","30ac820d6811449e03c9f8675a08d87c"],["/archives/page/2/index.html","c5aee394c911b3147d101484c7046080"],["/archives/page/3/index.html","9aeb08883cc3bef2e8e1b835c61a6d02"],["/archives/page/4/index.html","c76b6b65807d54473a204032c4031274"],["/archives/page/5/index.html","c00d3e521ced8ab3137a2d9a59d4b348"],["/baidu_verify_codeva-qQP2iZOMLX.html","1ba4be63c2202bc7e3d47f967c9d299f"],["/categories/Java/index.html","e1821dbcb77151643ab7a331b0f8300a"],["/categories/Java/后端/index.html","ddc297b86fef4be9b7f423ea408cd019"],["/categories/Java/基础/index.html","e72a5aaa7584c7f1c41004265e550687"],["/categories/Java/基础/集合/index.html","8fc3aca56f865081dc7b3b6e6c469394"],["/categories/Python/index.html","20d953fc637b7d3950e1fd5b69ae37e4"],["/categories/Python/编程环境/index.html","0b9f44e53941dbdcd9ad1f77755bf9d5"],["/categories/R语言/index.html","c83b83d6580256d9fc0b06fee3a73359"],["/categories/R语言/编程环境/index.html","6c63587fa2999564c8223cb4dbf4e7da"],["/categories/iPad/index.html","1d2cd8f337d3481c2e5e02e5f5136f13"],["/categories/index.html","295b23c90ba8f2d8ba57e5740c5803eb"],["/categories/中间件/index.html","e0e87d1cb0f859da083de8e88898bf73"],["/categories/前端/Vue/index.html","cc2f1db2c6bc524e106233847b026b77"],["/categories/前端/index.html","5c7678dc83759e1505a4f543e08d9696"],["/categories/大数据开发/ElasticSearch/index.html","aa6a4de4c319c63c0cca6f0cf1edc97a"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","37cdebc0b51bda6ca9aa4ccb83db72a4"],["/categories/大数据开发/HBase/index.html","8c507a44287394df26f4cf0ee87b31f1"],["/categories/大数据开发/HBase/学习笔记/index.html","d45eedeedc53d2897c2b59cd28ef8568"],["/categories/大数据开发/HBase/环境搭建/index.html","69ff88a0e034cfcda2ea893263e1e171"],["/categories/大数据开发/Hadoop/index.html","2022ddb01d77acc668646a65760f4a48"],["/categories/大数据开发/Hadoop/技术/index.html","744ed4f521735755e7d8ab70d12f777c"],["/categories/大数据开发/Hadoop/环境搭建/index.html","acaf21aa7a064599bfef9c7d52580cec"],["/categories/大数据开发/Redis/index.html","bb8424af7a2ae501f73d5ab58b812058"],["/categories/大数据开发/Redis/技术/index.html","6f09b1a0837a3871a959c9d4010f60c7"],["/categories/大数据开发/Redis/环境搭建/index.html","40894ab08d7384b8893c6789a568a1c8"],["/categories/大数据开发/Spark/index.html","cff1868918263334ae0f8051981e3d38"],["/categories/大数据开发/Spark/环境搭建/index.html","3fa1417f038e681d8dec955a5393a9ee"],["/categories/大数据开发/Zookeeper/index.html","e6e3f262e1b5b5edac27e9ff19a9658e"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","bb0b977842a6e7c6dc3d22f144cd83ce"],["/categories/大数据开发/index.html","d25220d208faec324665283cafe3feb4"],["/categories/学校课程/index.html","5832efa506f6ff0d4d7945b38314986e"],["/categories/学校课程/计算机操作系统/index.html","f5d898605a8206ad8baa7d5ea4b682d0"],["/categories/操作系统/Linux/index.html","ccbcf8a48a1ef4b1045df1df71ca9eb4"],["/categories/操作系统/Mac/index.html","cf66e1e0fbad48bef2078bae869052c2"],["/categories/操作系统/Windows/index.html","6ea2a2f106c9a20d026594548019b175"],["/categories/操作系统/index.html","7a5bdc54aaf05e88de5bca82129c88bb"],["/categories/数学建模/index.html","cb31f963f23b626138e078f35d0928e9"],["/categories/数学建模/latex/index.html","e1c3ce8a2fc603ffbd2f416b0eb202cb"],["/categories/数学建模/优化类/index.html","f3e67375cb3d73175e5e68503b97fa2a"],["/categories/数学建模/优化类/现代优化算法/index.html","83ddff79adfb62fe3618153685c3648f"],["/categories/数学建模/优化类/规划类/index.html","1fd757c58ce08a1c0f209abcf3f9a3bc"],["/categories/数学建模/绘图/index.html","bca1f2117f2b44ff9d1503346f12cd97"],["/categories/数据库/MySQL/index.html","ff6f7b9a24a0309a4b8f95fe6becea8c"],["/categories/数据库/index.html","58f79ca6b0d3350b59e0ca4ec8eb51d1"],["/categories/数据结构和算法/index.html","1e4d8fcf1ff5d083808d5a4accb96b8d"],["/categories/数据结构和算法/page/2/index.html","73dfa94d0ec4d7a6af7f479463f8b4d5"],["/categories/数据结构和算法/基本原理/bfs/index.html","aa260f191cfdc9f584ae2edbfbb5a3c5"],["/categories/数据结构和算法/基本原理/dfs/index.html","aa7bf929eb7514235227787f0ca82a6a"],["/categories/数据结构和算法/基本原理/index.html","4c72cf5605197d3c5980b88c667d8703"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","b06086956dcff8a95f802af9601f7e14"],["/categories/数据结构和算法/基本原理/动态规划/index.html","1001b0c7bedf53ae2f58d1230888b81a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","98dbcca52248c8613a4f3f31821b141b"],["/categories/数据结构和算法/基本原理/图论/index.html","53d62de6811feb178e89433e25d33c20"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","9e59fddc46e75b0ee1a78fc890d14440"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","a3141eba986506febdaca322d2ae7a56"],["/categories/数据结构和算法/基本原理/字符串/index.html","11ce53b28afd464cd6497d4403018ced"],["/categories/数据结构和算法/基本原理/排序/index.html","c9e35c7bf41f5c5afb96b1df5a81295d"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","678bba1c2389dc0acf8b3f1e1ed38ee2"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","0c0239f88c5f3cc4071dc80f045e49b0"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","916325c17030488d8cecb5d53b37f1b4"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ae52ea399852f92b24f072dfc607b1ee"],["/categories/数据结构和算法/基本原理/链表/index.html","6b3e55e420831f04e4a04ce2ff566594"],["/categories/数据结构和算法/算法题/index.html","dffe1daa30c1b765ed8c458ce04cb1c5"],["/categories/数据结构和算法/算法题/二分查找/index.html","1de0d83f02fca2a5eca44a59605ddaa9"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","dbecb3929f0833869ba6fac8e2a68240"],["/categories/数据结构和算法/算法题/动态规划/index.html","d86acfca9cbba9f0158e97a3b3ffa767"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","9a2b469da98ad20a69c4beda79f6ee86"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","286ad7d8d3cd38fae2dbc7461547c76b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c9dd2b2c6d4dd851357e3d2247ba0dfb"],["/categories/数据结构和算法/算法题/图论/index.html","706e22d1fc8ae3a6eb4f4bd34bd1e5e3"],["/categories/数据结构和算法/算法题/图论/树论/index.html","38ba7bcdec83bf87da5b0a6ec18284c6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","1dc3397acf902be5416056fa48ae6f49"],["/categories/数据结构和算法/算法题/数论/index.html","b321df358fe856ad94c1a7df076016be"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d35983beabe22b8c9ad0788bcb0a7ac8"],["/categories/杂七杂八/index.html","52f9df27bad222108aa638e103726404"],["/categories/杂七杂八/博客搭建/index.html","e1a749088d6286357c784ab2878a1e66"],["/categories/编程工具下载/index.html","1e219d473cf0e7adfafb9a8e26aee197"],["/categories/编程环境/index.html","c3de4d3ff3646874acdf8cca036918cb"],["/categories/编程环境/大数据/index.html","b156a7fb892b2aab88027eb997e057fe"],["/categories/英语学习/index.html","a734199d1cdf2735f6b67ed03de3d9b9"],["/categories/英语学习/英语语法/index.html","7fcb56b0a5d2faa144b733af824956b6"],["/comments/index.html","2daae4205683c26875438d6a4343f409"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","81e0ba16fdf81ab3e400da6ccedcb68d"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","b4f2ea5f6c751a147b78ffe6ea28eb2c"],["/movies/index.html","d23c9c52267657897d1acee6d995e71e"],["/music/index.html","22e250785f922b313b0ed21c17f293a5"],["/page/2/index.html","b338738885b9639b25a94216ef52fd10"],["/page/3/index.html","ca7f79a6817ea531a2b2672349b180fc"],["/page/4/index.html","43c44e191e1ba24621507a566fd5deb1"],["/page/5/index.html","8cdc9835b4db93a79707947c0b4eec15"],["/page/6/index.html","a713ddafb2298e0d3364c449168ad7eb"],["/page/7/index.html","1be8597619637225aea1a5f9ceb1fb08"],["/posts/1021360842.html","080d38ee4c98b5e66fd757cd78cabbde"],["/posts/1120620192.html","2d4a5e30f1aa8c2160c7df04871308c1"],["/posts/1137707673.html","dda2270cd93ac82d5f53f0460f4d353e"],["/posts/1141628095.html","29217115d520a7cca249c6d16fe2e7af"],["/posts/1168613674.html","9f9ce7444fc6a8ea76b8c252b04321d7"],["/posts/1219920510.html","f47d5887f8f3ac0686da1f7825aaf640"],["/posts/1222166338.html","a455b29f556f984303070ab28a7d5dd4"],["/posts/1259097482.html","c3f8b2fbf49a53563f5714f5e46aceb9"],["/posts/1271036369.html","14d85799eeeb4c027bc77d065b5733fb"],["/posts/1312847445.html","6e66cb581f314aacd71dcc5a18c1f75f"],["/posts/135355774.html","7c669b025c75be929192a4cbf4abe056"],["/posts/1375344716.html","5940ba21ca9320c7dcffb9875911bf51"],["/posts/1388991698.html","9fe55372a04f3428655710e5f6eb3670"],["/posts/1410315814.html","868074ccc3a278b9839e4175dbe151e2"],["/posts/1452790229.html","bddc85cb6229b237f6e9d2d1170a0b32"],["/posts/1470079884.html","00d472b0053e9e0e44924a659af9b34e"],["/posts/1470079885.html","d525ec9c990f49e1c9b09abffd14b35a"],["/posts/1470079886.html","98231892a42bcc28e6a2d8251ffff6ac"],["/posts/1470079887.html","8760847cbfc7cfe95ddbba3f1408550b"],["/posts/1498536549.html","eb333d6ab9d69f7c6350c8c076a2a26e"],["/posts/1539568593.html","c14df50b9c6c6b8d2cf1050be220f310"],["/posts/1547067935.html","961e3ca924639abdb765450d4cb4fffd"],["/posts/1557866301.html","ae88297e2b2e63866b617aeb9eb28801"],["/posts/1571776361.html","74e49d4dc957eaeac28856eb368c52e1"],["/posts/1605124548.html","65dc3ee3b3cf3c7f7b9880c266605ece"],["/posts/1633036852.html","d7543b8198643982ba61e1d5aed1e3cb"],["/posts/1667740714.html","7052400353901f84b4828ac7073f4147"],["/posts/1674202625.html","7c0aa887621fc780fa06113bddc7ca01"],["/posts/1765123828.html","3de2698df4755734c5708490ec7a5b88"],["/posts/1767336200.html","33b07d00273543f8a3dde1fecd5ba64e"],["/posts/1776114197.html","9c3a2f12666e38139ca9380f26de5025"],["/posts/1817748743.html","dbfa1b5bef2ce26477b9c729b6c5bb45"],["/posts/1925125395.html","c94ea8492c8f38f4fefb4bf64a2caf79"],["/posts/1966191251.html","c1e5dec46d602cacd5c26604ece16827"],["/posts/1987617322.html","5841bac52bacf5e1b05b56723c936836"],["/posts/1999788039.html","6a981485e77595ab922d2145ca7d84cc"],["/posts/2007534187.html","659248db9cbc879f4c0f523193849373"],["/posts/2075104059.html","c7ebb9170fa3731bf016eefa821aa46f"],["/posts/2087796737.html","d683de1477fd4f0948f3c4e1863664ca"],["/posts/2106547339.html","a92f109e20780c0aeced389c0e0dba29"],["/posts/2207806286.html","a3ed55c95344d4f360481e502b393446"],["/posts/2225903441.html","2e626a1c7eeeb1ebdf81defe5145af0a"],["/posts/2265610284.html","0d112420be07b5570141737d6697ec1d"],["/posts/2281352001.html","adb1a5612e72f6301907a3364cd6b666"],["/posts/2364755265.html","056de43e74a148fd6fa56e24a3811a70"],["/posts/2414116852.html","252bef58d81ce024b8f8c077ab3cc1f0"],["/posts/2421785022.html","d439cdb63c4ca9c26b8840c953c24805"],["/posts/2482902029.html","23c201412c5b14518324b081074201a3"],["/posts/2495386210.html","4bcae64be42f94e7ac56796253478c42"],["/posts/2516528882.html","bb9e37e6432089a37163cca1e8007190"],["/posts/2522177458.html","28078af542e60317242bb16e674fa32e"],["/posts/2526659543.html","bdd02f886fc6c23e773fa55afe1c6f96"],["/posts/2529807823.html","869d2f1f5164ae620bb3fdcd3a04732a"],["/posts/2592249117.html","663b9affe07dc70565d8b1d9e05f3975"],["/posts/2596601004.html","9effd5c92126e039c4072e1d28f38c49"],["/posts/2697614349.html","41e36402cff0ac625c91c4066f0f9bd0"],["/posts/2742438348.html","810f13edf6ebe92af1d527d7b1edc7d5"],["/posts/2768249503.html","ab6d33fff5ad5a435a85e0b7d8f86eae"],["/posts/2864584994.html","85b98e9f2253d7afcff8cf385de73fa6"],["/posts/2888309600.html","cc8f73f38daad1ff300adaf2e3f28c4b"],["/posts/2891591958.html","ff83116e2fcef2b284fd68c370a7d0e4"],["/posts/2909934084.html","a561ba2b464075684a0c149b5533aff5"],["/posts/2920256992.html","7001636d1490932572a16caa6a88113e"],["/posts/2959474469.html","164d61b320394601199da26d8fefa936"],["/posts/3005926051.html","da51b262d513586e00469790e0d65833"],["/posts/309775400.html","331726f0eb164e7eb9ed207b4bc8c8f8"],["/posts/3156194925.html","3a64f1815253e9b7e4083dd4939aff73"],["/posts/3169224211.html","da9a73000551faae1d2bec352d26b94d"],["/posts/3183912587.html","9d607673aa5d9b10c16daf0d3666a94e"],["/posts/3213899550.html","0db7008f7e8416b03de3f1f6f7c91fbb"],["/posts/3259212833.html","692d51be1e7e054c899924b9a661b7b4"],["/posts/3265658309.html","82fe99260271df3cc4237c9092551269"],["/posts/3266130344.html","59241c760c84836303c4f1aa8afacd15"],["/posts/3292663995.html","df02eb145b19d8de3b35422a87a4741e"],["/posts/3297135020.html","ce210a29655783d2fecca2717bf88a50"],["/posts/3306641566.html","14107b82f7c8c67ff02514ebe8bcbdc2"],["/posts/3312011324.html","3215228e4e96c4ccbe3aeb9d232732da"],["/posts/336911618.html","bbd827dc86fde9524ac4260c76732c20"],["/posts/3402121571.html","31daa2db3430d8c09c484f59372b3b08"],["/posts/3405577485.html","d81c4b2e8ec6f781d457eb4f41a175fa"],["/posts/3498516849.html","477a453165a19a1ae740a9c84e2affc8"],["/posts/350679531.html","db79f6077ba9f55aed86b212ba13f1b9"],["/posts/3513711414.html","e3f4a118d7b42c1a8372d69071321bd0"],["/posts/3523095624.html","7eb02cf71c7b5d35e8fb4e3c0dd17ae0"],["/posts/3546711884.html","c182815a510eb9c5e18cad1a5bb3b734"],["/posts/362397694.html","0ab72744531e4103982729f8ccfcd91c"],["/posts/3731385230.html","d8e70c51e3126466b27044d1f56e42b7"],["/posts/3772089482.html","a228245ce4ebdcb57cebcced8b3709c5"],["/posts/386609427.html","0ad641e10c9d71101040103d8ba55f00"],["/posts/4044235327.html","5ee4dd42cbe09be0b23d8aba3b5fb546"],["/posts/4098221856.html","30cb5651b55b5a49ec513865c2ee0358"],["/posts/4115971639.html","9ed8cd78cb8c5ea617946dfebd61618b"],["/posts/4130790367.html","729b27eb0b075d6c7f2921712ee0d33b"],["/posts/4131986683.html","5f5e2ceadf466e4de6d63921c4cc8826"],["/posts/4177218757.html","1b155f1fc9c98319bc7c568203ca8fb0"],["/posts/4192183953.html","7e46df654c7a4dd84892c5cced7af981"],["/posts/4223662913.html","d077c39428e6a0f5f35147df92717a8b"],["/posts/4261103898.html","9295bfc48bfffdb237510dc548e8926e"],["/posts/4286605504.html","762a9965adaba1dd99c1b160326f4e3d"],["/posts/449089913.html","957f56b9701bff509321b768ba976dd3"],["/posts/469277133.html","0eeeb2c83c03feb793c46b5647f2f581"],["/posts/469711973.html","5a9dec1961ad1f140e03eb6c1f0140a3"],["/posts/482495853.html","a689f35da2c003f50ac2aa0b439e8826"],["/posts/488247922.html","e8091968b01072282b966d0a72c4cf02"],["/posts/517302816.html","7a70b61b2a9b4aae965f06e235ac63ff"],["/posts/570165348.html","989a764188f7ccce4e71ef0794e1247b"],["/posts/595890772.html","8f4118a6526236682110e8138bcc320a"],["/posts/67485572.html","57a45c6d9964376c4e78cb38792f98bf"],["/posts/694347442.html","2f78d7e1a7b3618897582f4e13ec637c"],["/posts/707384687.html","f6d8364ede0b6d44656cfb27e3a34ecd"],["/posts/71180092.html","31702d3e2320c61c2ee6da5a441c1e72"],["/posts/716459272.html","dddc9c6cf6fac679eac72b963368524e"],["/posts/765481613.html","2aae4a9338cb63df39e51deaa836e2ea"],["/posts/778231993.html","cc2a08dbce2533836843823284377180"],["/posts/795397410.html","571e1b6fe480b2a80eee38b1914c61fa"],["/posts/820223701.html","1386737c3289e774471d45a7c4b38bc4"],["/posts/830372185.html","10dfc07994c7d1a72d6381b1294151ab"],["/posts/88294277.html","5bc174f085dee7c53ade05d3befc49b0"],["/posts/939963535.html","07d4f4640013ee7ed412f477fd80cb03"],["/posts/983786067.html","4ba6bca0fdd86ee2a0b56bfe3db6c4d0"],["/sw-register.js","49d66d89f896016c5f9ff20dbf45c14f"],["/tags/C/index.html","aa7b150303830cc0849dd14489729ba7"],["/tags/C/page/2/index.html","df19b651127ff3a839db050060be9cc8"],["/tags/C/page/3/index.html","d3668f812ee4b7068205f705d2158cc7"],["/tags/C/page/4/index.html","eda8a0a7e07e99ebc2a9bce57e11f42b"],["/tags/ETL/index.html","2f4b34e0fe456a116d1e18401ec2b7fd"],["/tags/ElasticSearch/index.html","c978a038619f9fc8a64e4630c622f6fe"],["/tags/GUI/index.html","b156d13fe47a9c497b5e2578459df711"],["/tags/HBase/index.html","1f916e0c9269abf5113e7d8ab703182e"],["/tags/Hadoop/index.html","d3e45d2e03cf18270ee52101d3cca096"],["/tags/Hadoop/page/2/index.html","232d1df148615292be5eb1a060c6797b"],["/tags/Java/index.html","a4223ca03ab60b08ec4bd71e9f3a3158"],["/tags/Java/page/2/index.html","1811d06a7bfd4f85ae32ced5a0cca49c"],["/tags/Java后端/index.html","0e9c9f7f8d4f479f5ab789e0ace6a2f2"],["/tags/Java后端/page/2/index.html","9af64d2f26e1ab7f11a0ffb753e285b4"],["/tags/Kettle/index.html","be185bb15b6ac4729d5adf354a7215a1"],["/tags/Kibana/index.html","ef2b4942ffe6dae7fb4bf7da71ff91d4"],["/tags/Linux/index.html","7f333c6fdbe2dd4a858fc221997c55ba"],["/tags/Linux/page/2/index.html","48a245e998acdf9e6c9db54b86a35224"],["/tags/Linux/page/3/index.html","d546e8c6a9cc214a1f5cbcdc2cf9fe09"],["/tags/Mac/index.html","47c95b99f5845e8dd02f0f7eadf2a6f4"],["/tags/Mac/page/2/index.html","5061fd0a4ebeb8b83f9a559ac162ba21"],["/tags/Maven/index.html","ea41e20f08d0b5fecc539132a3306c39"],["/tags/MySQL/index.html","44a2354bd1f6e5adf0ae8a0b2d9a293d"],["/tags/Python/index.html","1944a2a6b770695e59b6c3a06041b909"],["/tags/Redis/index.html","cbb80c9e126f9e5a4f549d3597b89a97"],["/tags/R语言/index.html","b1ca2f89625dc22cf52e01d96ad09de7"],["/tags/Spark/index.html","cab4e7e48b871a189f88dad738e63737"],["/tags/Ubuntu/index.html","2fe6cc096d34a297196657a93a059bb7"],["/tags/Vue/index.html","2c21adc596c33af83c73c095e898759e"],["/tags/Windows/index.html","0c9cdfc03ef03e7bcd2853b19ac9cb4f"],["/tags/ZooKeeper/index.html","1fa7c4cd2859fccd1167bd1e07e039a1"],["/tags/bfs/index.html","cfab71e326f006d098f44dbdb383a805"],["/tags/dfs/index.html","024edbf669b7b64cf2a87133b6afdbfb"],["/tags/folium/index.html","3dffaab3f70af5d3f9fad18141044037"],["/tags/git/index.html","6949d5a62e06659590a3a44c780c5dcd"],["/tags/iPad找电子书/index.html","f51c3e072c281950f169a80d64420252"],["/tags/index.html","d14cceed3062cf240f190893126a7170"],["/tags/latex/index.html","35b0fdfb8457e236bb51a198e5d5cd1b"],["/tags/中间件/index.html","a6bdf846228c0ae6de888c91208e4611"],["/tags/二分查找/index.html","42b694cab91ea8c8d07dc55c1098592e"],["/tags/优化类/index.html","05ced79fdf369518583bdc2cddeeb92e"],["/tags/前端/index.html","f84bfb797d3aa2b684290ef1e5c5b839"],["/tags/前缀和与差分/index.html","d7d38572df409ef2283d514e8cf289af"],["/tags/动态规划/index.html","e8ea4084b17fba4aa1295db71b0cfd12"],["/tags/动态规划/page/2/index.html","26f81a336eddb3e088e979771d27f2c6"],["/tags/博客搭建/index.html","47478f3f29df0b878166816b584cae14"],["/tags/图论/index.html","be04cd7e1107fa161c2c6d6ea23873d0"],["/tags/图论/page/2/index.html","9bc254d110e82cd27cd584e00884a89a"],["/tags/大数据/index.html","2226b57f7e778ff64a4a5aeb2b11e635"],["/tags/大数据/page/2/index.html","672fcde18273269b7aff441baf44f2cd"],["/tags/宽度优先搜索算法/index.html","a6635001deb3fad3474cca6374b91571"],["/tags/排序/index.html","f8db7519d4dbca987f0c8de6b3b64908"],["/tags/操作系统/index.html","1c53f61ea9c6c3f1bbe41a9edb76515c"],["/tags/数学建模/index.html","1362b3a1755e015e1f8370e7144c3f00"],["/tags/数据库/index.html","47c9bddffb1d44530465f4685be807d2"],["/tags/数据结构和算法/index.html","b4c6bddb94cfca3487890a7713d73cf8"],["/tags/数据结构和算法/page/2/index.html","a3275c439d2dbecee7d1ab54c1708c64"],["/tags/数据结构和算法/page/3/index.html","9fd6861a17b4bf7d13905de7f3bf011d"],["/tags/数据结构和算法/page/4/index.html","021a4dfe4016ff233494eef4c060ecb8"],["/tags/数据结构和算法/page/5/index.html","ce8fd445aea0b8d3d8a1c43c760faaae"],["/tags/数组和字符串/index.html","64b00d47a8428014ff651a9ad80ca1d6"],["/tags/数论/index.html","96eeee98c9d80f4f52d0613c3757e325"],["/tags/枚举类/index.html","e8d6f071a9ad23bf21beb954f5b83c04"],["/tags/栈和队列/index.html","7a9563dcdae4fa3d6c958bbf237015c4"],["/tags/树论/index.html","0add5c163dab56ee047369a79ffaf8db"],["/tags/测试/index.html","a25710897c20e96a98e9ac5ef75988a8"],["/tags/深度优先搜索算法/index.html","23fa5a3fafb4049e3bb9334992c58814"],["/tags/环境/index.html","cf08d759eabe3318321022b7aa752beb"],["/tags/环境变量/index.html","1ceb0bc6a98d19cc9585a3289cf26ba4"],["/tags/绘图/index.html","1e41beaf060c9fb4e1e7f4861021a870"],["/tags/编程工具/index.html","9c24ebe9a11503667818472865332b71"],["/tags/编程环境/index.html","8830a5fc0adf6859786f393015be6dfc"],["/tags/网络编程/index.html","533c2155c6aca8a8d10501d677f31845"],["/tags/英语语法/index.html","0c74953b1b233d61d7538dc29581cf2b"],["/tags/计算机操作系统/index.html","8e7348e7956adbc46bd4cf608c536d58"],["/tags/论文/index.html","850d02706762ac064246cbd59d385f41"],["/tags/资源下载/index.html","b1a729813c8966743b5b1cdf9d5b963b"],["/tags/链表/index.html","ab2dcf3e0d2bf5ee0efdef678a68f4a6"],["/tags/集合/index.html","52dacc6996a01e7ee29c17315d4dd02d"],["/tags/集群/index.html","db90427ffe7b441d42a5d7499bb8719f"]];
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
