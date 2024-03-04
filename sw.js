/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","c496042a7c58cd54e971ece2589adc41"],["/about/index.html","09cdc5d7570ce7ea41b825d130714a6d"],["/archives/2023/01/index.html","c634a144e8d4cf10131c8e6d7bfc1cf2"],["/archives/2023/02/index.html","c42413d95cddf2774493322415994b5c"],["/archives/2023/02/page/2/index.html","c2c425127f55e3883fa902127c3f54f8"],["/archives/2023/02/page/3/index.html","a6837ad8ffeabd700bb3b59f2edcfe43"],["/archives/2023/03/index.html","fb0afe7e8c29a976c00d8946d92a6522"],["/archives/2023/05/index.html","0672c6fb3f270218b61cbce714268695"],["/archives/2023/06/index.html","2669b0b4764f234a8b7453ed0a3fa446"],["/archives/2023/09/index.html","b27d4293f39ca5b89441b976f2bb0950"],["/archives/2023/11/index.html","90460b24dc7b21eb3de01d8bad1b98c8"],["/archives/2023/12/index.html","8e220ab62782c8125e41325fbec05b39"],["/archives/2023/index.html","506fff35fd798c025cf614ead55e6882"],["/archives/2023/page/2/index.html","ee1811ccdf087b30660847b1770ba311"],["/archives/2023/page/3/index.html","22dd79956b17d9e4eb635540c03bbefb"],["/archives/2023/page/4/index.html","d54d43b3d71c308c0cf44cc4363e6464"],["/archives/2023/page/5/index.html","0dde7bc83353b40feabe907daf17e8e5"],["/archives/2024/02/index.html","0228592f0d448423efcaeba622d49533"],["/archives/2024/03/index.html","d94170c2bc7ebbc63147d63b6dd3f5cc"],["/archives/2024/index.html","863548f4d64410eed49397db991278df"],["/archives/index.html","48623fa53a4b6815f6bfb1db8faee1e1"],["/archives/page/2/index.html","57b6355727cefd4e2def379fa30d0f1a"],["/archives/page/3/index.html","8c09cf9b0795f9f99fb361598dbbb6a7"],["/archives/page/4/index.html","646207e3da44a83ad3e1e368b8ef28b8"],["/archives/page/5/index.html","4839600e05c6f6de5201487302bb43d7"],["/baidu_verify_codeva-qQP2iZOMLX.html","d1952421c4bbd175ca7de73acdeb69b3"],["/categories/Java/index.html","a15384ac2f553cd2acc677954453867d"],["/categories/Java/后端/index.html","ba37f5c33fcda9bdcb30ab7512446221"],["/categories/Java/基础/index.html","8af487e7ec0491321700d9b96fb23d53"],["/categories/Java/基础/集合/index.html","2a75c44aced643a091c5d5c62fb77721"],["/categories/Python/index.html","bbd492213460112fa9fb8ad9afa911cd"],["/categories/Python/编程环境/index.html","c1fe218d8ca128dd71f7d664cb6707a3"],["/categories/R语言/index.html","eeb22b1159dbda79b6161a766dc1077c"],["/categories/R语言/编程环境/index.html","2b68b596a6df67161378b01dfb3b00b1"],["/categories/iPad/index.html","9e44ce032315854dc58836b70c6f8812"],["/categories/index.html","fd87571d9da9a1c92789f83efec2e82f"],["/categories/中间件/index.html","1911bcc85e3f968b707ce66478718ceb"],["/categories/前端/Vue/index.html","32c7f475874b95470dd948ba42f17191"],["/categories/前端/index.html","55cf598fad2266b2b70cb5d8a179fcad"],["/categories/大数据开发/ElasticSearch/index.html","2d4a9f53aca672d998f0db77ec7cd102"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","78ab062a3894a3f332908852c0d956d6"],["/categories/大数据开发/HBase/index.html","a448b7b8c04f72b78865aa86ae4fac7d"],["/categories/大数据开发/HBase/学习笔记/index.html","f2af2c1bdffa4112c9cbd32d24ebb1e2"],["/categories/大数据开发/HBase/环境搭建/index.html","07ebb7409a7f37a7b2aab54a26f06250"],["/categories/大数据开发/Hadoop/index.html","6e4070d8e5f3ad29f8b59ec5453515a8"],["/categories/大数据开发/Hadoop/技术/index.html","469329aee28cbdadfbbf50bbeb893de6"],["/categories/大数据开发/Hadoop/环境搭建/index.html","8ed42c2c733843a2281b876f408bcc24"],["/categories/大数据开发/Redis/index.html","b1c86e3feb479b1ddd6f441f2ce31f8e"],["/categories/大数据开发/Redis/技术/index.html","4f2d7c36218d3b973d7b1af7255c7811"],["/categories/大数据开发/Redis/环境搭建/index.html","e0570758c51ce0f359044a5867dcfeb6"],["/categories/大数据开发/Spark/index.html","6bf73d879848b4a58efea818328e934b"],["/categories/大数据开发/Spark/环境搭建/index.html","b0053058860f7e7fa12aa06a76e5287d"],["/categories/大数据开发/Zookeeper/index.html","4713778d4fd20d3cc7958d8edb19f9c1"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","220c63a557cb58502dd60e7b560894bb"],["/categories/大数据开发/index.html","c8a9a468baa9e80f5d0d1b650e9e44d8"],["/categories/学校课程/index.html","2bba9b6e71335f04cf60f111690db8a8"],["/categories/学校课程/计算机操作系统/index.html","a320d8286b92036f027cab2f28d2b020"],["/categories/操作系统/Linux/index.html","baa101922e4ef6b2d637d9eb3cb4baab"],["/categories/操作系统/Mac/index.html","af430e0083be5bdd227c7de121ed9d1b"],["/categories/操作系统/Windows/index.html","0df81fd802d9592263fe14fd205c7b36"],["/categories/操作系统/index.html","5fbaee4efec0a8e7d07321a217d4deb4"],["/categories/数学建模/index.html","3fb64b63bb65ee0bf24782d1c7281f64"],["/categories/数学建模/latex/index.html","c188b8395f02fb6905c6072252251bb6"],["/categories/数学建模/优化类/index.html","ac90b4db1fa719f706e62eed4f0eecef"],["/categories/数学建模/优化类/现代优化算法/index.html","3dee2c62a61f797f4c70b49af161e0d1"],["/categories/数学建模/优化类/规划类/index.html","8d1544a7d514173aa1449fc4ecf42346"],["/categories/数学建模/绘图/index.html","c23bc04ec925e769bbb858326ea75ea9"],["/categories/数据库/MySQL/index.html","cd5d8aa9773c15912ec9c06c58f2e08e"],["/categories/数据库/index.html","60efec68fc33942556ee11611ba3faf2"],["/categories/数据结构和算法/index.html","4882885d566850027fd586ed048c3357"],["/categories/数据结构和算法/page/2/index.html","60d0498507a39af39cd3729e85c8d760"],["/categories/数据结构和算法/基本原理/index.html","8af44ccdeddeeddd1570dfc688ca14f8"],["/categories/数据结构和算法/基本原理/page/2/index.html","500471b1585361037d554887f7e77209"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","93efe97d862ce0e741b285a3fc3fbbd3"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9acfcc349ca113aea3e80c70ed201f26"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","48ec71b766bfd8f01724b5d7db22de50"],["/categories/数据结构和算法/基本原理/图论/index.html","bd719795940a5d6d1aaaa34238b3cc53"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","430007270ed7cf549f79f4b5de2c380f"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","0a7b57c5aa667e3432fa367be8465e19"],["/categories/数据结构和算法/基本原理/字符串/index.html","267edbb1bd4bec31113be7f64e29da6e"],["/categories/数据结构和算法/基本原理/排序/index.html","8d4c9469de17a17c621082c64bda9545"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","60c9f33f72438bc4af8cef4ce763d94e"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","39295b38f15b3da7f2630dbc3dcfd608"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","67528ce77297c52404ada540341897b9"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","53d5ef92e221a7cb7521d7eaf12c8c18"],["/categories/数据结构和算法/基本原理/链表/index.html","f90b99c14e2ec40fbc30370062340f7d"],["/categories/数据结构和算法/基本原理/高精度/index.html","e91c87d4c8395a7e590958e9aede899a"],["/categories/数据结构和算法/算法题/index.html","15e284ae48ec76027eede05858867bd7"],["/categories/数据结构和算法/算法题/二分查找/index.html","5e693cda438d42261898c401d0e9fe5d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5a7e2f9971d74a38d65dd76ce36b3f08"],["/categories/数据结构和算法/算法题/动态规划/index.html","2036de096b8ee6d8ae7fdf2cd3995512"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","eff6e150ce0927d92eb605e0229e1847"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","0e072b75bb2551e8402dc7747253128f"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e49298d745b6675a05e18b9902d19e2f"],["/categories/数据结构和算法/算法题/图论/index.html","90b4f23413c2cd9f9807efb9fd4e51dc"],["/categories/数据结构和算法/算法题/图论/树论/index.html","43ea7fcc3b4cc9840ef2d1e59c1eb8b9"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2c78c482b16ce5378d8d280f9bc19768"],["/categories/数据结构和算法/算法题/数论/index.html","9d55161ccc2ded33890212a49bd822b6"],["/categories/数据结构和算法/算法题/栈和队列/index.html","accbe4c233f256c40cf2e86b799e3ac8"],["/categories/数据结构和算法/算法题/高精度/index.html","8b404a46d61cd29651483c6fc77feb24"],["/categories/杂七杂八/index.html","26a06e1242c4bf77bf76073a78a15719"],["/categories/杂七杂八/博客搭建/index.html","3b632ffecbfa45ec6c641abec042a49e"],["/categories/编程工具下载/index.html","c19048e0e133ed74f8da82d6149308e5"],["/categories/编程环境/index.html","02106899a4e0330b96eb0f9f878a5a94"],["/categories/编程环境/大数据/index.html","991171d7a30a4b154325f911befb9604"],["/categories/英语学习/index.html","bbbf9eb816e9813b75d97eeb6ba3429c"],["/categories/英语学习/英语语法/index.html","20e2fe7a64ddf6f01a46f57886c55f9d"],["/comments/index.html","d0266860fcc0fd869cbf88806396aeed"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9153e69bc2d1ee4e434a4489b3ce4899"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","acac5523a3f622d4600ed352197c8370"],["/movies/index.html","9382475672bacb4a396461f62a6cb234"],["/music/index.html","ebe6136dcab0364b838c24e017d0dd38"],["/page/2/index.html","bf1110eac49ff5b7e310d95e360c4d7a"],["/page/3/index.html","66d89242df387aba0e7ff4843eac9ef2"],["/page/4/index.html","49390646129a49674711b1fb268d964d"],["/page/5/index.html","f65d4dcf27f67bf39590d40909216db7"],["/page/6/index.html","5cfeeba82bfae2f81b448642d61687fc"],["/page/7/index.html","3f1f0da8c2d34aa622792d104c765348"],["/posts/1021360842.html","2292bbfd6d238cf4e99ca65a38490f5b"],["/posts/1120620192.html","52b065d7ad34707a8cc37e5c3bbb8369"],["/posts/1137707673.html","5ff38b752df269ffac472cb50be1697a"],["/posts/1141628095.html","64981d99e36ee75cfeef2eb1d72ec05f"],["/posts/1168613674.html","423815545222b2604fb2e64d57141c19"],["/posts/1219920510.html","479b9f3028fb319ae6e1e85cade3c2d1"],["/posts/1222166338.html","1365e2f072bf667fc082a73b48aef23d"],["/posts/1259097482.html","4abfa876c2a008eecb61a8e70ba20582"],["/posts/1271036369.html","801bb1e12c28b4cdcb0a7dac392eaefd"],["/posts/1312847445.html","9a85875110599c40ebc8312523cc5569"],["/posts/135355774.html","6aa700860ac6a0da6fe0c34955c02228"],["/posts/1375344716.html","5cc327075f81916b1271d9389908cf60"],["/posts/1388991698.html","fe8877b2cdd7bb40a93ff6b82927b6fb"],["/posts/1410315814.html","7c69ac5d122aeb6f1444d687b0ca757e"],["/posts/1452790229.html","27e77dd9e24f0a2022071e82441b1a7b"],["/posts/1470079884.html","e9cf24e02a192b67c45f3b3804306abd"],["/posts/1470079885.html","19502d9a660eb55cbe3e4354939999d5"],["/posts/1470079886.html","0acfe5a9af99f75a94ba394116d8b3bb"],["/posts/1470079887.html","f6b7f7630d3fb55db1c206930fa8449b"],["/posts/1498536549.html","101a70df170e890ac689a67796d188cd"],["/posts/1539568593.html","8d50ddde07a65110e46210f31d56c3e6"],["/posts/1547067935.html","fe1dad97fc83173047f4652870f5295d"],["/posts/1557866301.html","b7638ffd5f9e08f9060e066ecc28e37f"],["/posts/1571776361.html","ba623d2c048fb07f0ecb3a8ed0d121b9"],["/posts/1605124548.html","7ce09535bf7c628d4821c36b02b9981c"],["/posts/1633036852.html","3f3322d297f3b9cf26268ace16f1714a"],["/posts/1667740714.html","7cd9b2b68ed3b0a65187f859f06123cb"],["/posts/1674202625.html","85e60853d99519d931287fdf87b7781e"],["/posts/1765123828.html","32cd3c157fed988bf181c5d8a6f4a061"],["/posts/1767336200.html","acf318943fb86adfc5a1eaca0e9c435d"],["/posts/1776114197.html","a3942f8fd623a586328d079974c10deb"],["/posts/1817748743.html","05f656556a74f35723ea52df8d258c8b"],["/posts/1925125395.html","90c79202ff5660d2488137d7c70e661c"],["/posts/1966191251.html","467a904ca9e40b7e0690ba0219935601"],["/posts/1987617322.html","601aa635471bddc9ddf75f0fc23fff78"],["/posts/1999788039.html","c5896371e66666025fac921a0c7cc4c3"],["/posts/2007534187.html","00679dc6b5bfc142b1763ec3d0b58bb8"],["/posts/2075104059.html","7e698403b9db88fe87e586274eeb7faa"],["/posts/2087796737.html","a95e8579761f1ca825f4ad6e3b2e41d1"],["/posts/2106547339.html","972a7f520fc49a3f657154d837e84d19"],["/posts/2207806286.html","7986d77c5c8d25110d6bf68145dae533"],["/posts/2225903441.html","b37c142a2aee6e9a7af0ae0f2e045aeb"],["/posts/2265610284.html","dc77a6dec3ac11fc27ccfd36d919462f"],["/posts/2281352001.html","64c5000709dea96ed3e4142339f77870"],["/posts/2364755265.html","63eba444455fad81c95a4f44ed13ee27"],["/posts/2414116852.html","25ab19f9d15cd504c1ba34143ed31137"],["/posts/2421785022.html","4b0cbf5b5a60ad4f22592715c17030e2"],["/posts/2445543724.html","e52f7a83bb11dfcff4451574df7d6464"],["/posts/2482902029.html","9212cf8613821325d3815aff300a7401"],["/posts/2495386210.html","13633720bbcb6b15be3c8a75f02aa8ac"],["/posts/2516528882.html","1cb772231b2c59a6f8308fa0f26ca760"],["/posts/2522177458.html","a06d96d6838dd5d3ee9bb9d0c7c654b4"],["/posts/2526659543.html","fd80c6ce50ccfcb2c1821dfadb62f6dc"],["/posts/2529807823.html","1bbf715fa86539531606c47939e7449f"],["/posts/2592249117.html","a7c267c1b75788ed793dcad9cf6ec4ec"],["/posts/2596601004.html","5422475902db7bd43cd3b97312c47dcd"],["/posts/2697614349.html","31f2b6becec326fbc80915ccff40427d"],["/posts/2742438348.html","ba0e1851ddc907e09f509f9d431c5489"],["/posts/2768249503.html","0c259a7104ab56e955bdc85d6e7ce3bc"],["/posts/2864584994.html","3e5c1410251cb049532eaea4940a97ca"],["/posts/28838462.html","18edbb834ca9ed44d27d2cc3b7c23f83"],["/posts/2888309600.html","227ff9092ac64ff4478fd840b1ffb741"],["/posts/2891591958.html","2eba4af7db405a08375fd6b18c8aef93"],["/posts/2909934084.html","a60576d3cf28cad46b7d5dd9af01d2cf"],["/posts/2920256992.html","ee48e813eb44dfd7613b671a7b31d624"],["/posts/2959474469.html","2896ea6fa4c61fa9baac32d68e2a58fe"],["/posts/3005926051.html","f63dd384528c7cd589e1c0b38b92568a"],["/posts/309775400.html","9d2a37a75d9f98b468632b6386657918"],["/posts/3156194925.html","c9a4eb9eed4fbfcda7fececb56c03411"],["/posts/3169224211.html","86bb9bf73a2aff682c8f775466127817"],["/posts/3183912587.html","10f721de6a3db0e6b75d73d9e710ed35"],["/posts/3213899550.html","a69d60de116759be96089d777bf19c41"],["/posts/3259212833.html","a27670086c12452dcd4d043b7a22d649"],["/posts/3265658309.html","01fd654e2fe0e14301b7fe184834b383"],["/posts/3266130344.html","9c0f87181a87066adf99ab851ee30d5c"],["/posts/3292663995.html","bddcfbf55d778ffbbc616c1a3f779d83"],["/posts/3297135020.html","c96bfef00ae80255a4640964564ccfa3"],["/posts/3306641566.html","9705440d8da3bee45076b8eeced8204a"],["/posts/3312011324.html","086c168a844540c119685b666b0a314b"],["/posts/336911618.html","4528174e7d04e2be4134c51725d4fcb2"],["/posts/3402121571.html","5cae97f0cf4a63fa5b12c32a05bd6e6a"],["/posts/3405577485.html","795aab035951db9171c095479fb17c31"],["/posts/3413737268.html","3b1ffdb4c0096f7bd1b14f1e3a322d89"],["/posts/3498516849.html","ea22ffbe47590b3f0530a4e5336fecff"],["/posts/350679531.html","4b160ae25be4bccfb6a72e3e08cb5aa2"],["/posts/3513711414.html","d4f628c212cd60b690c0204c7e0fb98b"],["/posts/3523095624.html","38864656a91fa9b3bbc45355383a86cc"],["/posts/3546711884.html","5a96935528c111b706a98fa0f9ff1017"],["/posts/362397694.html","5440ee455524d259381bb432a81e32f0"],["/posts/3731385230.html","6113d71a707a24dde5bd7f3cdbb84dab"],["/posts/3772089482.html","b239068e57121a6f3d6064c818ed2d30"],["/posts/386609427.html","32ac90b26c5264797768d251a51e2d15"],["/posts/4044235327.html","481a61fbca5d464a510c3f3707b5eb23"],["/posts/4098221856.html","af3f6bf2991504aacfaed3f93c8eff85"],["/posts/4115971639.html","a8014fe0509e8e91b895271554deb304"],["/posts/4130790367.html","d979d7ffc28527b03a49ef98915fcc52"],["/posts/4131986683.html","a277a4abaa1de052ec37b76b70754a44"],["/posts/4177218757.html","745a044009a789e8d9d9fb2c9a2a630e"],["/posts/4192183953.html","a8fde8e0984513d9a26b7812618cf873"],["/posts/4223662913.html","54112594d090f5c23d20e7ccffe598e2"],["/posts/4261103898.html","4048922edb7d0efd4ebbc928800e694b"],["/posts/4286605504.html","48f42877ace9ed8b4ad007b33abf393a"],["/posts/449089913.html","cfb6878a776083b2d726afa479ac8e82"],["/posts/469277133.html","22ae5f420f619a72cc697ffcf2933671"],["/posts/469711973.html","292b0dda9587eecf7c49861316c51895"],["/posts/482495853.html","cb247aa866fb15bbaa4eea6848d8ce89"],["/posts/488247922.html","b90f0156d5ff5aa1a871918daf48c797"],["/posts/517302816.html","78486df2999da621c108ce88122a57d1"],["/posts/570165348.html","feff078a8b9b445c3c4f833ea5bc49ef"],["/posts/595890772.html","51cacd96feb7a5557fe29c34adf9a7aa"],["/posts/67485572.html","aeb433aaf8e6d55f282ca48a8a7bdbaf"],["/posts/694347442.html","922565deefc916429d432dc4dcce2893"],["/posts/707384687.html","fccf989553648ba579aeece86850aa17"],["/posts/71180092.html","e4563f09dffea51e7ac6562dd17d216d"],["/posts/716459272.html","340a401c9c4b8c63571b84a216d20871"],["/posts/765481613.html","c1b225ca07079f7e94a1864014b6e06c"],["/posts/778231993.html","f4a153d03572257c9b246b22608abc75"],["/posts/795397410.html","6691dcb76e15ca7998432d31180f125b"],["/posts/820223701.html","d6dd983f62c83798c816993d23b2d1ef"],["/posts/830372185.html","05bf64ff068cf9338163cff7eb1f7af4"],["/posts/88294277.html","047697809dce2cce2bb84701b7f3a31e"],["/posts/939963535.html","e8dcf72afbe7b2119a6af2270406132b"],["/posts/983786067.html","6738ab08401207653350c8d71b02f8d3"],["/posts/999620786.html","621601a3a592dfbaee1c07bd20e55d9f"],["/sw-register.js","6410f22e90dc2104abcb692b582160f3"],["/tags/C/index.html","263320f1d78a3f538b26f375919e2eef"],["/tags/C/page/2/index.html","68aeb20c3751b77f4f744a5bd969cc55"],["/tags/C/page/3/index.html","e5d2169b8ca389ec2533860f25437138"],["/tags/C/page/4/index.html","f625e1b90e3b0618d54809d5042cf74b"],["/tags/ETL/index.html","49339cd95439218efc91942dba80de26"],["/tags/ElasticSearch/index.html","47bb4a274f5e4cb60423601426d4074e"],["/tags/GUI/index.html","56620b21a0594bbf2dfc5c0725d4ba24"],["/tags/HBase/index.html","81ceed6c0513f1bb4f718fa7c02fa951"],["/tags/Hadoop/index.html","020b7eb54fbfa6e664dc3de05b734c9d"],["/tags/Hadoop/page/2/index.html","898cb2ca19e7a1917b7248e798447a50"],["/tags/Java/index.html","e633c0dbcc94c17cc2f99d31846d48f8"],["/tags/Java/page/2/index.html","2c4dd34c1c6ab199ff58abf3b35ea4ed"],["/tags/Java/page/3/index.html","d4e6f08b0d2daa71a850dd3dedeb6d76"],["/tags/Java后端/index.html","c4d5391116c11c4bf4ecd59c2eb87ff3"],["/tags/Java后端/page/2/index.html","305d001a5e7ac92c8e7dfd7e880b438e"],["/tags/Kettle/index.html","5b8104641e3a092d80b93b0914974cdd"],["/tags/Kibana/index.html","b3e44792ca4747a8b0da83185dd8b5b0"],["/tags/Linux/index.html","0b15905acea3945128c92c95ad99e25f"],["/tags/Linux/page/2/index.html","56588027b790557028dbafde75b96a49"],["/tags/Linux/page/3/index.html","4b1f196f55683f0476e68c5fd54ff781"],["/tags/Mac/index.html","97217b3db3c9c521153dbadb6a4e4a5c"],["/tags/Mac/page/2/index.html","25e7321b88f8f70f07ea18438cd319aa"],["/tags/Maven/index.html","b6ce1f562b4e6f92fe859c99902679c8"],["/tags/MySQL/index.html","775129f98b89eb89545f00d117e82bc9"],["/tags/Python/index.html","a7d12f0991f32824ea5fe57430def0c7"],["/tags/Redis/index.html","0b65199fef36ed81bbef62151e4c419e"],["/tags/R语言/index.html","9032c553d5dbda171bd93fdd7c0d1102"],["/tags/Spark/index.html","79f5b2c59ac8355959c6ab33fa5ffe51"],["/tags/Ubuntu/index.html","dc86f513ffc54126367262a9c23bc80c"],["/tags/Vue/index.html","d9619ca42d0efa41ea778ccb23cb7b72"],["/tags/Windows/index.html","603bd3e82138de4c28b36b77b11ea851"],["/tags/ZooKeeper/index.html","5eb2721eeb3497ecd9e4fa939fafa096"],["/tags/bfs/index.html","b67c3a21a9c6587bfc14071a6f67dbb7"],["/tags/dfs/index.html","00fdb17b7d0040ceecc60db9aa26dc2f"],["/tags/folium/index.html","b12167c79f08031a8a2a99c637c92e72"],["/tags/git/index.html","01793e9b36005d1b7011f7268725125c"],["/tags/iPad找电子书/index.html","65a1ff5e967a72f848fb434b1af7891e"],["/tags/index.html","396c92093f73c37b8c01f658cb4ae4e2"],["/tags/latex/index.html","2d30bfad7de273e6cf9e6ce69d6ff0b3"],["/tags/中间件/index.html","517894516514ae8a37bee2cf23376e17"],["/tags/二分查找/index.html","1cdefaf897f68f1c93efe37b82b4f618"],["/tags/优化类/index.html","8645a071ddb968d738e6a73c7afe518a"],["/tags/前端/index.html","1aa9729598c5e1cd9eb54fdc74c12421"],["/tags/前缀和与差分/index.html","63719ada75250fc37db5373965709df4"],["/tags/动态规划/index.html","f88e57bc8fa0a984e0012bf7e386b6ab"],["/tags/动态规划/page/2/index.html","c647e5bb8d826dbdb864b1db0fc6df55"],["/tags/博客搭建/index.html","28f2b4ccb2e0575c7ac3372e5e9703d2"],["/tags/图论/index.html","82faae6f9461879d54d53e9e5e1f443e"],["/tags/图论/page/2/index.html","5d526242ea6cb8286da770f5043856fa"],["/tags/大数据/index.html","7fd8599355a1dc317050a06c05ce6322"],["/tags/大数据/page/2/index.html","1c36f9df9c5e38f8cb9d7c10f1673605"],["/tags/宽度优先搜索算法/index.html","57ac1e870e4e4d15ca22997085d5dcf5"],["/tags/排序/index.html","a6f629a78e5b89ada13cabb4d44a6915"],["/tags/操作系统/index.html","ab893b1fb848799911ae15cfee13cdb8"],["/tags/数学建模/index.html","2d1f075e7759f8771c33821115c64af8"],["/tags/数据库/index.html","b8a15e805a12c9a0e17d72d12d90756b"],["/tags/数据结构和算法/index.html","b033da2c03cd660363f0350851b7f25d"],["/tags/数据结构和算法/page/2/index.html","33caa3f34450c79a80533b446de2d775"],["/tags/数据结构和算法/page/3/index.html","f4282ff41f53f39197a614760b6777b9"],["/tags/数据结构和算法/page/4/index.html","50e39c159674eb80919c5bd7e860ec31"],["/tags/数据结构和算法/page/5/index.html","e660b74b19c6083643df8f86a1fa8b52"],["/tags/数据结构和算法/page/6/index.html","4dee17e05a80b55bfee29379300a3172"],["/tags/数组和字符串/index.html","6272b8367a8d2c085217f96935f836a1"],["/tags/数论/index.html","a45ac99be74cdc4af2a555edb5fadca9"],["/tags/枚举类/index.html","b8fed44c4d640ca09cb3a05318b6b192"],["/tags/栈和队列/index.html","39e3a7ad389fc7128ee3130a6e1e88ec"],["/tags/树论/index.html","99a6059adce272fdf8f8072ccf3719be"],["/tags/测试/index.html","4af8302f85b26d4abcef8b2a571cc530"],["/tags/深度优先搜索算法/index.html","7be3512e7250258311dfd8f7ed487854"],["/tags/环境/index.html","ef17abcb06c4d6077df84642ca576087"],["/tags/环境变量/index.html","22d8bb48f5a3977e56449d068e2a8c1a"],["/tags/绘图/index.html","23da3674245ed943cf437cdaae4de649"],["/tags/编程工具/index.html","e94a62c5d2fefa42a712fd04dabe8b72"],["/tags/编程环境/index.html","10b8b14971d77ce7d296066b845d4fb0"],["/tags/网络编程/index.html","d84952d3596926380447423202c1b274"],["/tags/英语语法/index.html","5c134cd463c4724114cce5c67f2ad6f2"],["/tags/计算机操作系统/index.html","b636a374e72340734be60b985654f098"],["/tags/论文/index.html","0cfa642a233a247b0c92a20303b2ed9a"],["/tags/资源下载/index.html","46094162c489347f966e32da2e00953f"],["/tags/链表/index.html","2fa94fb35d2e8ff9bff3eb6f5e351f4e"],["/tags/集合/index.html","15c4eae2a38b3222ad11b4586a83075f"],["/tags/集群/index.html","1927f81aeef2cb5f3db42bd2bb03abec"],["/tags/高精度/index.html","7fdb033eec8a5fb54ebba3e263283749"]];
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
