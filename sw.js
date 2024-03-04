/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","74034c3618b7e5f2b8c703217bd2c64f"],["/about/index.html","434e3f710e718de903f9fe4b1d7cc6c4"],["/archives/2023/01/index.html","2c548171054c4f779c8cfe8363841498"],["/archives/2023/02/index.html","a656ffeff994382695a0e0534de388fa"],["/archives/2023/02/page/2/index.html","bdce83395d205dcf9c14b78d4422212d"],["/archives/2023/02/page/3/index.html","1b178646816745f39836b3ff6c6804e9"],["/archives/2023/03/index.html","cee9e95e3cb6fa9678c1c62498996b9c"],["/archives/2023/05/index.html","3a6e77d7307974ad32a705a73568ca7a"],["/archives/2023/06/index.html","7b8df358d8259dc5f21f6e34e22708a3"],["/archives/2023/09/index.html","2f5c318e915004881613a3b84c1b1e6e"],["/archives/2023/11/index.html","72cb8a560eb04ff64b4351dcf5ae39c6"],["/archives/2023/12/index.html","69a3e36f911e73a7edaf17abbf94cf99"],["/archives/2023/index.html","07bfb680f8e47f722dd997a5a16a2e7d"],["/archives/2023/page/2/index.html","4cd9339085b6bfaa7160cb706848bc2a"],["/archives/2023/page/3/index.html","976efeec230d06d09e91fbdc71bb3e65"],["/archives/2023/page/4/index.html","4f028aba383f2193aa9934ad248bad0e"],["/archives/2023/page/5/index.html","d2b293870d64f5ba05388b34918897f9"],["/archives/2024/02/index.html","92642ffae65315b669f74697575cac06"],["/archives/2024/03/index.html","92b3a3f7c98892617d752c7126caf0fc"],["/archives/2024/index.html","36d0633aa55e7ccb8fe539cff888def8"],["/archives/index.html","13f7ce73e48d996b2702843b37805490"],["/archives/page/2/index.html","210b474d6ea1e5e62b16774a373e7749"],["/archives/page/3/index.html","d21a7fdedd4c6221292e8ebb7bea8378"],["/archives/page/4/index.html","1c66fa977ada06b7adfe02141e8b865f"],["/archives/page/5/index.html","65748227ccf5dffce114f6e19d8a8283"],["/baidu_verify_codeva-qQP2iZOMLX.html","04d1553dd12239cc2dc9841c02a4bd7f"],["/categories/Java/index.html","9c9bb04be91043182de63c79ae50658d"],["/categories/Java/后端/index.html","53174f38d72efa83cebb1dc4ac445994"],["/categories/Java/基础/index.html","bdc3bd0be519878646226fc1bf2b080e"],["/categories/Java/基础/集合/index.html","029f8031e91f5687d743b5364f41c627"],["/categories/Python/index.html","f6fddbe6a44500b04a8a5451a1f1613a"],["/categories/Python/编程环境/index.html","4ff79e1c95ca7e8947edd3f6e2d36dfa"],["/categories/R语言/index.html","32e553b03c0235209f46da3cf3e9f9bc"],["/categories/R语言/编程环境/index.html","88ec35694a504189c65ebe543106c31e"],["/categories/iPad/index.html","4245f015999902a4cd75d03026af238a"],["/categories/index.html","5e2fa877d4d5578393ccdaddcdede542"],["/categories/中间件/index.html","2ed44f8bcc2fab23129bd8212d67476f"],["/categories/前端/Vue/index.html","9ec49aa659a485fef9e98ca427219235"],["/categories/前端/index.html","0e48d29290926b899b8a3d07abd1865f"],["/categories/大数据开发/ElasticSearch/index.html","40173d905cbbc8b538a336603398e3fe"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","8cebd71a126456bc6245858d38d1601e"],["/categories/大数据开发/HBase/index.html","b71cd016a2e7126267fed35fab9f7e06"],["/categories/大数据开发/HBase/学习笔记/index.html","7871654db2e9b0d5fbe36c069f336286"],["/categories/大数据开发/HBase/环境搭建/index.html","948aab6a364905953e8f26a9bdd36c10"],["/categories/大数据开发/Hadoop/index.html","e622150e994a1d227820e59e96bda649"],["/categories/大数据开发/Hadoop/技术/index.html","0b2e9595e06d93d11c1c7eaa8ced8590"],["/categories/大数据开发/Hadoop/环境搭建/index.html","af6a73dcf722957a9a79df7406748d62"],["/categories/大数据开发/Redis/index.html","200d4993283c978fda614b7ff6ad9b54"],["/categories/大数据开发/Redis/技术/index.html","559e6c0441755c62d147155a17d51b6d"],["/categories/大数据开发/Redis/环境搭建/index.html","07795f0df7e946677d50d5645196dceb"],["/categories/大数据开发/Spark/index.html","b720c9e1fce8cfb36fdd2930e0399be0"],["/categories/大数据开发/Spark/环境搭建/index.html","6863923761f41aa2bf299aa5a013c4d7"],["/categories/大数据开发/Zookeeper/index.html","bcfab4f5ce92698c6f897d7726f033bd"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","5944d7ff57d7ebb59da2fa81e5af5707"],["/categories/大数据开发/index.html","4ff0bcfc2fb1f693a2be440cb743cb56"],["/categories/学校课程/index.html","dade33b7ac72e0ddd91c63bb32530fab"],["/categories/学校课程/计算机操作系统/index.html","aed12638b68f45c3043f8bc25da91c21"],["/categories/操作系统/Linux/index.html","4cede207611d2593f496fab60b6762b6"],["/categories/操作系统/Mac/index.html","a665f888aa382e56e2872d05104adb3c"],["/categories/操作系统/Windows/index.html","c2e25d027fd3b22e9e2a7835e021433f"],["/categories/操作系统/index.html","71628a4c581c3713c4f80b2770e97337"],["/categories/数学建模/index.html","8bf5d1c4d59cc603cd31ecc3673d8010"],["/categories/数学建模/latex/index.html","21b5d278f5011a3db2b366bebc2e74c4"],["/categories/数学建模/优化类/index.html","d4458439016fe75483eddf7bbd20e9ab"],["/categories/数学建模/优化类/现代优化算法/index.html","245d19259fe8d9d225f3f1e2f9cc6bf6"],["/categories/数学建模/优化类/规划类/index.html","bf6a39908ba7e273718813300e47f03d"],["/categories/数学建模/绘图/index.html","82f43146df8e31e4e52ec167f33221ae"],["/categories/数据库/MySQL/index.html","57569774d4099c038d68548f1ecd9a99"],["/categories/数据库/index.html","7f521c69ad231f36fb772f5d68819f41"],["/categories/数据结构和算法/index.html","61ade1ff6b6933f50b1a1e066b71db99"],["/categories/数据结构和算法/page/2/index.html","f19cccf925500f37855c1a14b2aed162"],["/categories/数据结构和算法/基本原理/index.html","0ed81559b0bf6c2f92647294d0713838"],["/categories/数据结构和算法/基本原理/page/2/index.html","8bb1e0a3aa734be90e76de11f847ebc1"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","3c6ca10510957b5c44ab167c37db2c3a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f3c5ed4c4c1f55b49430f925cf6a9aa2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","b6ce283036ef94132dd5761b3785b292"],["/categories/数据结构和算法/基本原理/图论/index.html","1a1453e2ef212662243cfb5964ff9725"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","156c49a8f23bae6ab84588f56161a59c"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","975c2a9763edcd973a24fc4601768a21"],["/categories/数据结构和算法/基本原理/字符串/index.html","9feecc9a733ee445d6634e175dac3c6f"],["/categories/数据结构和算法/基本原理/排序/index.html","ad770428d89087ff871bb9eec52e46ca"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","2410dd4989e90c4a01fb7cd35234b87c"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","ddabb1652f01cc029d6e0ec47830a3b1"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","3fcfb383fd34d2bc77868f6ea1a10315"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","8deca5ecd7c1a85b1687b299bc8d97e3"],["/categories/数据结构和算法/基本原理/链表/index.html","281088bedc4720dd6450e6df5e9cd5a5"],["/categories/数据结构和算法/基本原理/高精度/index.html","d0045fb4a9120c532c789b9145c6c210"],["/categories/数据结构和算法/算法题/index.html","5cd391d89d7d2f6769154d149651ebe6"],["/categories/数据结构和算法/算法题/二分查找/index.html","d235c51daa1a9f041d2ecd0745a581d9"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","d6732f8e773dcf6e06e242307923d820"],["/categories/数据结构和算法/算法题/动态规划/index.html","e3ead4750ad64dcdde94c6064af8a067"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","c85a02f47c4d9625f744c50d12c5470a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","6544cf9f3f7974aaa172d8fafe17ca8d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9d6c0efed86eda99a46ce6fbbc6ebe95"],["/categories/数据结构和算法/算法题/图论/index.html","9893b985cbbc0056f2582f6ccebebdcb"],["/categories/数据结构和算法/算法题/图论/树论/index.html","2ceb9f2d15785c70012536a5a19917b2"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","5c30091eddb6fdf33ca4c90767206c65"],["/categories/数据结构和算法/算法题/数论/index.html","d005ba15de0261612fa04983a462a1c5"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8f22a6de6428e4f7ce3748046b78570a"],["/categories/数据结构和算法/算法题/高精度/index.html","f88780f9b1409dfdad976e89fc606fca"],["/categories/杂七杂八/index.html","6adc508aca6ece687e9be5a7ddc3fa76"],["/categories/杂七杂八/博客搭建/index.html","4502aa1542f41f47d33f9aa4229b884a"],["/categories/编程工具下载/index.html","784859614fa950c384205e3041f28b8d"],["/categories/编程环境/index.html","d4dac700bbaaed94256c76c621db7fd5"],["/categories/编程环境/大数据/index.html","4a452c4ec54d2d86ba2479da79bb678f"],["/categories/英语学习/index.html","40a8a3aa6d62fc0554292193fcf079a6"],["/categories/英语学习/英语语法/index.html","bc9208916025651f445eb995367322fe"],["/comments/index.html","3638b51594aabbdafa933286df3ff3b5"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b9ff7b70678146d35108fc0a87cd117e"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","9c4d6cb614b8750090453dfece8b0511"],["/movies/index.html","526e7bb6dfcc39ea79b50b6d6c16b709"],["/music/index.html","73b40c980ff0fdc402d5937582608fc0"],["/page/2/index.html","29343e04ac9c0efd11712132793e6c02"],["/page/3/index.html","4c8b3b37a3876b013da19c6a8867124f"],["/page/4/index.html","0160a76c97fd4cc2f13b798c8fb6cb48"],["/page/5/index.html","c4a72e9b70ff3098020797d11697afe4"],["/page/6/index.html","357edc8a78cbf72dd09f34640b6c17cf"],["/page/7/index.html","2e6e9923d3a1b2734eb20c6eaead5c99"],["/posts/1021360842.html","0834ff58d72f54241dd24ebec5cb6da0"],["/posts/1120620192.html","e0ec9fa91f29f1191e809c0a6a695a4e"],["/posts/1137707673.html","8cb07077634e54fc79b505538579585a"],["/posts/1141628095.html","de412ca15ee428e41836ad1aa7be80f0"],["/posts/1168613674.html","bf7478ce573bceb58c6a88b0898f8e00"],["/posts/1219920510.html","ec385e5f62f90f1da8d6b07dfe63fa7a"],["/posts/1222166338.html","848bb0ab985620d38c8997f9cc657842"],["/posts/1259097482.html","71598f91fbcc8129e9092083397ed73d"],["/posts/1271036369.html","eeee0de2a00e2837a333838ea9556d8a"],["/posts/1312847445.html","819912c7efc6281e120391d25b4ab497"],["/posts/135355774.html","a6481fb03cb100d8dd7767588a74d248"],["/posts/1375344716.html","69bb4904023f409f84f93365ec0b4c17"],["/posts/1388991698.html","bf8926d3821b1a1356023e48a0ff6b3f"],["/posts/1410315814.html","8835535a7d518e163ecfa3d8cc179577"],["/posts/1452790229.html","49f02613508e2dab4a009dba8e47dca2"],["/posts/1470079884.html","8b1297d775001ac734798e1781fb81b6"],["/posts/1470079885.html","49534d2ac2e5de6b82f8dc1483930099"],["/posts/1470079886.html","2924c3df7be9c3a32a31b09a394cce63"],["/posts/1470079887.html","7081115d6e0e344380cbdef75726d0e2"],["/posts/1498536549.html","9a2edb0c12649653a73bb350e40797cf"],["/posts/1539568593.html","1078b4dfdf0f6aa3743a3b7d1753765a"],["/posts/1547067935.html","adf9a4b38e8935417a5e7e2f2686fab5"],["/posts/1557866301.html","ba91a7d7debcefb2cbe20279d1c43eb3"],["/posts/1571776361.html","60a924464f6e09313b7a4b3f09f6f99c"],["/posts/1605124548.html","1a4bb2ddd090d83de28834a56c96afc4"],["/posts/1633036852.html","943c9747d26f7c88b0ec5cf9018a3043"],["/posts/1667740714.html","381dd1df24c06879fb19427fc26308e8"],["/posts/1674202625.html","33591aea8c2104b31d456242e1556842"],["/posts/1765123828.html","60928c1a32bf9596fbe8e46c557fc686"],["/posts/1767336200.html","90d3976e71c0f499d63a3579a96bf9ca"],["/posts/1776114197.html","34c49d19d1a1f4e2442da903758459de"],["/posts/1817748743.html","05ab20a2653865577a2a7c1d6917f2c3"],["/posts/1925125395.html","e3ed559a5359f639ca58e7787ddf23ec"],["/posts/1966191251.html","b71b77f68a5bc34be681fb1bfffeda68"],["/posts/1976734692.html","36c5a415dc43bfdb2fa0c1809fb79567"],["/posts/1987617322.html","80ab6d8907864df307d3e1b341d3ee5a"],["/posts/1999788039.html","3d18624476a6306257ec478941405ce4"],["/posts/2007534187.html","43593a1944df427cad316ff1ee654d9a"],["/posts/2075104059.html","fd9ad8f9a4277a6f61e4aaa5747b4281"],["/posts/2087796737.html","6d45d98e122857fa3956d8d6f1b49dd8"],["/posts/2106547339.html","e25895e9f869d3de554f96d0a1c10d29"],["/posts/2207806286.html","c06080347ced35f556f90571aeb47def"],["/posts/2225903441.html","6697cbd3311d0927f662041d3da79902"],["/posts/2265610284.html","ac60ff40b5d764c7f2ede7d9e861104b"],["/posts/2281352001.html","02c44ce351e83e1bc4d23e21133f12b3"],["/posts/2364755265.html","be997565a1115c51fa2049460f7f4f63"],["/posts/2414116852.html","373128fadb912549beaf23f70533ddac"],["/posts/2421785022.html","a501a23fce6bf87082efc1abb60be31b"],["/posts/2445543724.html","2786824b43f6b5baba8d71a979361886"],["/posts/2482902029.html","361a47dd036ed29821ec8ab133480984"],["/posts/2495386210.html","69c6201c9f117c0bc67dfcd143a73c8d"],["/posts/2516528882.html","5e3519e4962bfbe211042f71b2c638e7"],["/posts/2522177458.html","e38feb85c778e206bdc4c8181d64fef5"],["/posts/2526659543.html","c8e3853e41ee48f6d57fdfb97e18a5f1"],["/posts/2529807823.html","e71a1d83538aaca5273e3be787b26c8b"],["/posts/2592249117.html","c1eca4ac90d89255d9d1c71165aa0f9a"],["/posts/2596601004.html","2683c7e88e01d10a8a6b5542191d753e"],["/posts/2697614349.html","1a0cd54264b8f98acee462a0de00ca09"],["/posts/2742438348.html","c37bc9911817590815355b36f36259db"],["/posts/2768249503.html","647fdadada01b18e0b5e52111f90233b"],["/posts/2864584994.html","ece4f6cd2eaf1b5261f686a80b139651"],["/posts/28838462.html","d3be81eef4e7071b0bbdaa0922f23573"],["/posts/2888309600.html","069ad885f5cf9955c46ede267a860707"],["/posts/2891591958.html","60f931f39d4427df3b7c08c8a5dea664"],["/posts/2909934084.html","65e1c1224cd19948799b7dc3d224b7b9"],["/posts/2920256992.html","ba1c0580e4e06fad9f1f3007bc0fd1e6"],["/posts/2959474469.html","51d531306818310c596ce515bd8b1b78"],["/posts/3005926051.html","6cd27fac5d08a71682eef3af651e9781"],["/posts/309775400.html","0d98c2916d0f984376d92bb42d2d1247"],["/posts/3156194925.html","e40802fdcae728eef8a4bef7b05a2fe4"],["/posts/3169224211.html","a5200a2c34b67ca23126b5006e20275f"],["/posts/3183912587.html","3195e2dcca578a21c8357155b5065a6b"],["/posts/3213899550.html","c95e60c664db008287cd2b54c1b89d61"],["/posts/3259212833.html","c8a049695b582ad25b5e8b3c23802424"],["/posts/3265658309.html","cd0adec519ae42e2038622be8f33bacf"],["/posts/3266130344.html","f3fed4b2b8c18f473282dee51f113eba"],["/posts/3292663995.html","b128dceb94743646a107cf925f014af4"],["/posts/3297135020.html","f01f7fef2aaf754739b7d3046cfada85"],["/posts/3306641566.html","954840e32c7c1dca1d0ea4d9c5f1350d"],["/posts/3312011324.html","ef32df089b85f5e37b6e7d2e89b695c2"],["/posts/336911618.html","b65ba5762ef5008c3de7ceceacde5b0c"],["/posts/3402121571.html","da94219dd4acf0e6245d3b41067d2fef"],["/posts/3405577485.html","d019b3e36a539bafcfe46df580c2634e"],["/posts/3413737268.html","4c83988efd2332c78d37fe8c738ad86c"],["/posts/3498516849.html","5a86c2b239b2759351f2cc7eef0163a4"],["/posts/350679531.html","97590e1a61d4c81c4616d08b223b6de6"],["/posts/3513711414.html","9209ca58809406bf5c2bfd95148cbfda"],["/posts/3523095624.html","72ee2f4290467a2b11b25266085d2685"],["/posts/3546711884.html","8798cada1c657ca9ec07ba1fa85bd28e"],["/posts/362397694.html","96446eff649933dd43f91c46797743e5"],["/posts/3731385230.html","b9bbd1054d0664f592183f521495d8c8"],["/posts/3772089482.html","2fc631add15211e396b85c78648c9e64"],["/posts/386609427.html","11f23ce3d86f9c896d0e17fb716702af"],["/posts/4044235327.html","b125de0b921aafbff97fab516811b305"],["/posts/4098221856.html","33fdc088f590ffb716f20d4e6fd03491"],["/posts/4115971639.html","214ea989af382b01b1781b4ccbf34c1c"],["/posts/4130790367.html","26e2236951cf5cd6e1738626bdadbe2c"],["/posts/4131986683.html","3f4c60659932e8bf40b447076334a900"],["/posts/4177218757.html","d411f758248222f6ee084b8efa70328e"],["/posts/4192183953.html","c7394468f68641b98826ca484c40ac48"],["/posts/4223662913.html","4e89dbcd836c7a8219b55520954b2520"],["/posts/4261103898.html","ea6874e4d305d9d87dbf4de770fdae9d"],["/posts/4286605504.html","fd3bcf49302b8a3e7114a3c8155b3463"],["/posts/449089913.html","c725ad6522710f7f3df5ae6eabe9b42a"],["/posts/469277133.html","47fd0279e70b5221ad9510a76023f2dd"],["/posts/469711973.html","20188a8726c4298564e8507471139582"],["/posts/482495853.html","407e2f7dae0b657f92108502725a91af"],["/posts/488247922.html","68de0944a475b6628829d9f29135ff9a"],["/posts/517302816.html","07bf24bec8dff341b9c54405f9b6a9d6"],["/posts/570165348.html","3da3aa61051941f4a3bdf3e2d8d43c53"],["/posts/595890772.html","c840395f5ecaa9d01481ebee01471015"],["/posts/67485572.html","1ff499c8e7baf74572d058da7675dd63"],["/posts/694347442.html","48e60402d4f356e7c28b37e9ab060063"],["/posts/707384687.html","09415d1cccf9c436f48fce99b0400e64"],["/posts/71180092.html","e00b612ce5c6c0c6a9e0b6940e0abfe5"],["/posts/716459272.html","db14271f6fa65f5561dcb8fe8b96853a"],["/posts/765481613.html","7b35d75214f7859fc4e59d78d83c9c63"],["/posts/778231993.html","988f334477953c8ac5614ae92213d4c9"],["/posts/795397410.html","471d72428ea656e057d021e59370c6bd"],["/posts/820223701.html","953e2a75a95ec67b1d6f0e9d06cffa06"],["/posts/830372185.html","2a5ede94ae728dff80a9759c7b6c57ac"],["/posts/88294277.html","3b6155797cefcd504aeb8a4af80c6710"],["/posts/939963535.html","654fb9f162c000b78ca98b6039dbaa27"],["/posts/983786067.html","e19253070a83a9288f83cf7f7c4c3a6c"],["/posts/999620786.html","6a2db5e3b3cfe4ca36f7cba8da8dfd3b"],["/sw-register.js","db3d3a8dec26a43badffd5992e3a6cbb"],["/tags/C/index.html","c30b2baaf577216486781213012ecf6b"],["/tags/C/page/2/index.html","7bb3a5478f860435aa62161abfa1e27f"],["/tags/C/page/3/index.html","9c3deadfc4fb7a23050eb302eff0fccf"],["/tags/C/page/4/index.html","f6a0dc8353ebe44af4fc50d27801ef83"],["/tags/ETL/index.html","574e0f2bd98922da99e07a1d2c74a558"],["/tags/ElasticSearch/index.html","a12af4cd5c8b78b1784e15cc8cc29d3d"],["/tags/GUI/index.html","f6011a7c9e7e201c7ed070519b2df760"],["/tags/HBase/index.html","f653e55ba49c685c6ababf96d79b6aa3"],["/tags/Hadoop/index.html","2ac8768ebfa920734b95a8132a481ad8"],["/tags/Hadoop/page/2/index.html","03f7a8f5982c587631c1cb573afd56bf"],["/tags/Java/index.html","10fb9293451f7afb5b6e5bae21de002d"],["/tags/Java/page/2/index.html","8736ae8da7009b42d73c9d0bddfc73e5"],["/tags/Java/page/3/index.html","1f0d12988de0449725885da104ab0cd8"],["/tags/Java后端/index.html","5300cf6a104b6beac06900bdee2fa717"],["/tags/Java后端/page/2/index.html","23d404179dc59c9d869329c81741c9dd"],["/tags/Kettle/index.html","38bf55f852f73f200afca4af8290297d"],["/tags/Kibana/index.html","f78be055fcc7de6ab6f05e4a504ba362"],["/tags/Linux/index.html","66c3303019f9a515207a76edc0ef005e"],["/tags/Linux/page/2/index.html","2c5a088df9299c36851c9688eb243500"],["/tags/Linux/page/3/index.html","51a6897f783319dacaee457357899392"],["/tags/Mac/index.html","1d8ac43da355cbb8010b0de23527b737"],["/tags/Mac/page/2/index.html","e11af7fdfb63ae517caa979d4531ef61"],["/tags/Maven/index.html","e989ea18c8a0ae7270eb95b0c0f0e82b"],["/tags/MySQL/index.html","546e0855acc58f5f9fbe55d983268325"],["/tags/Python/index.html","8dd1ad636286c6ba5fe07683e5442deb"],["/tags/Redis/index.html","b267646f087e0c9b9f648a9ea1d236d0"],["/tags/R语言/index.html","dfcc0cf98ced9bcd6af07c017e86582f"],["/tags/Spark/index.html","0d40f6b00441d2e6f9c94d7d646a11d3"],["/tags/Ubuntu/index.html","ceb9a72ac0db0806324f5f504a289018"],["/tags/Vue/index.html","8699b30f342e44af726fd3b7d077da9e"],["/tags/Windows/index.html","152da272ecff43b496c3dee5212a8e22"],["/tags/ZooKeeper/index.html","e882cc9c74db27e0765c55bd1732f3d0"],["/tags/bfs/index.html","0b20e304f4fe6f90526056c2d6a2d581"],["/tags/dfs/index.html","7751dc31221ec582acae2918b01ee588"],["/tags/folium/index.html","2f6bf2a90775955a477481f989ef5e3e"],["/tags/git/index.html","5f1c9803cf856b18a632dc0a027336e3"],["/tags/iPad找电子书/index.html","4073ba3d2b09e33dd5a015a864e22445"],["/tags/index.html","620c55fbf6796d45be4a314ebae4d446"],["/tags/latex/index.html","78a2ed9b8faf4edd2579a522d823ba6a"],["/tags/中间件/index.html","dc55a040152836540a2202e87fa32948"],["/tags/二分查找/index.html","c5383dc0a5c4c319f3db633ff2c71d96"],["/tags/优化类/index.html","61d0c03f480842cf30c968506420f7a8"],["/tags/前端/index.html","1126f5ded217ec57db0c6671a7091968"],["/tags/前缀和与差分/index.html","115b4c7f9f6ac1a9b25536799157ab9b"],["/tags/动态规划/index.html","9de62686d985a8f0b6613f296598395c"],["/tags/动态规划/page/2/index.html","b67523ca0ae1deed0ccbc674d9276c46"],["/tags/博客搭建/index.html","cca3696ce7328eb4a821f2dd407a1b62"],["/tags/图论/index.html","fdaa311c8685eba9ad379137d83e86b6"],["/tags/图论/page/2/index.html","156147e1e4a7be64daec3fe6bd874690"],["/tags/大数据/index.html","1dfae0c9f16fa7525a732780a6f46b05"],["/tags/大数据/page/2/index.html","22780fd706ccb8c9f4c94f5df6be87ae"],["/tags/宽度优先搜索算法/index.html","095df129509c7f53699d3ca599e489e3"],["/tags/排序/index.html","e3751db5bf44f7217ad9608527effff3"],["/tags/操作系统/index.html","d0e83f5dd21586bdf5e2c7cf30824023"],["/tags/数学建模/index.html","c3b02c21ff9adb3fb4fa57886822be61"],["/tags/数据库/index.html","5c40895e16cda4e91bbc6d82d591ca54"],["/tags/数据结构和算法/index.html","b636db0676d2c9a3adce355d023b4d90"],["/tags/数据结构和算法/page/2/index.html","8558447b5f8de04191b6fb7c9ae33274"],["/tags/数据结构和算法/page/3/index.html","5e2cd59f870a4affd0d665731de208bb"],["/tags/数据结构和算法/page/4/index.html","7bd9e2cdfb9d845e3db171eb0c23cbb8"],["/tags/数据结构和算法/page/5/index.html","bd41528068549a2843fb7b5daa8af9a4"],["/tags/数据结构和算法/page/6/index.html","0d059672cd7522f906c41dadeb931796"],["/tags/数组和字符串/index.html","09e3c20b23d79fd540bb329c6f852d7b"],["/tags/数论/index.html","2aaeadfb4363b25596e7e05cbc636652"],["/tags/枚举类/index.html","75c0bdea3a696e172361a34a51e46fcb"],["/tags/栈和队列/index.html","090466b8aee3053c13a8fe468af4e25a"],["/tags/树论/index.html","9058b3cbefe813ad0cb4109273ce855f"],["/tags/测试/index.html","5016975651b59f7d4bb45762024dff82"],["/tags/深度优先搜索算法/index.html","23a418f8fd2a96fd55993c9ccf442e54"],["/tags/环境/index.html","e35627bc0ef29ad70db4247bf2456cb4"],["/tags/环境变量/index.html","17090df1c49cb00346436b72d29ea36d"],["/tags/绘图/index.html","5459fb8c3fd0c2aec333d80162ca1117"],["/tags/编程工具/index.html","320808d5661034c6cd3afcb9d73cbe4c"],["/tags/编程环境/index.html","01370b4b24db42efc4b3ffc8e5e2a51a"],["/tags/网络编程/index.html","922163ce4d75fe48bd8fbad942cc55e1"],["/tags/英语语法/index.html","81f3fa6597115f9f61bae358b9b768e9"],["/tags/计算机操作系统/index.html","dc62c487b4faa38e94eb55a65d0219ac"],["/tags/论文/index.html","7c0f1f822fc2e79565b4aa1d0cf93f2a"],["/tags/资源下载/index.html","46995391e504af2b46622954b7e8eff7"],["/tags/链表/index.html","58ffd1675408fb150b3bd0f2d3dda35b"],["/tags/集合/index.html","1260043be69848f1762e81bb5f827ba5"],["/tags/集群/index.html","aca8477738ef2c35adcd0577b9433987"],["/tags/高精度/index.html","1e86a1ce4ef2935fde931277f02f93a7"]];
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
