/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","838e43dbf93ba2d60e13c01d7b01ddaa"],["/about/index.html","09cdc5d7570ce7ea41b825d130714a6d"],["/archives/2022/02/index.html","aca997c49a59258090a2374db7fdd45f"],["/archives/2022/index.html","99789f8f26d6c6b42a279acff86d05bd"],["/archives/2023/01/index.html","12f79ad2868fcb79bf0657fe4ea89c68"],["/archives/2023/02/index.html","947486c084bc0397a5cf04eaeb192ab2"],["/archives/2023/02/page/2/index.html","7aaafc671bb802fcee520a089a867eb4"],["/archives/2023/02/page/3/index.html","33d187f7c094893815278b6b8aa51fb3"],["/archives/2023/03/index.html","a0d738bb62c43658c0e11e3e3941b184"],["/archives/2023/05/index.html","178fdfe7a8ea3d2e55745b9297ceb135"],["/archives/2023/06/index.html","1193408de6207573efca604df6a19c4b"],["/archives/2023/09/index.html","d66a2090863191f3b434d14de0c7fda4"],["/archives/2023/11/index.html","c0d9a5ba8f0b5980d17184eca2d3cfc1"],["/archives/2023/12/index.html","72358fdc4f97e81f447d4fb1af6c2e56"],["/archives/2023/index.html","cebee461fab07a87ff0202c3e78d0e4d"],["/archives/2023/page/2/index.html","8aa15158ded3739530e0de5c2fb66707"],["/archives/2023/page/3/index.html","ae5b29791200002e72d6e04d6469bd8e"],["/archives/2023/page/4/index.html","0128853ab9183d9a69b16c24d3e482c4"],["/archives/2023/page/5/index.html","1cea1e755c48fe926f2993dd879a3562"],["/archives/2024/02/index.html","45722720270cb27142fc5e741f44e448"],["/archives/2024/03/index.html","38dbc6706ddc13ba07d336fa2b3e7aac"],["/archives/2024/index.html","dad9e1e7b5abd86a7984a8d62591839f"],["/archives/index.html","6357d836b27a1e188793f3bfb18c73b7"],["/archives/page/2/index.html","063fcaaadcc3afa36f9e603182fd51a9"],["/archives/page/3/index.html","16a4b372d2851d501c9ccf41fbdfb0a2"],["/archives/page/4/index.html","2ce7c97c56f90c0168aea06cbd5e8b74"],["/archives/page/5/index.html","6e81e984738723aa43c53f7e43e948a8"],["/baidu_verify_codeva-qQP2iZOMLX.html","f83ebc0319079a879f200231dec296b0"],["/categories/Java/index.html","2f6f7655783526da299541f39f35dc54"],["/categories/Java/后端/index.html","a1c4a533e0f61bd7a01baf52b096ee67"],["/categories/Java/基础/index.html","d202eba74831d1d0be7cf52aece789da"],["/categories/Java/基础/集合/index.html","1b1a7e071d2a68ed14772818e9e3d11a"],["/categories/Python/index.html","d8ee56a814d7a6ea7997989eae2fe899"],["/categories/Python/编程环境/index.html","14c225072e71b753c4ef5e4bd492e084"],["/categories/R语言/index.html","f284e4350c4674242332390298d21f3a"],["/categories/R语言/编程环境/index.html","17f64f66911194ea80de4d5fa275b5a1"],["/categories/iPad/index.html","a28d4a90a74cc07559194a210878cc0e"],["/categories/index.html","fd87571d9da9a1c92789f83efec2e82f"],["/categories/中间件/index.html","7c1094d94efda2710ec804743cdbb741"],["/categories/前端/Vue/index.html","afeb823d020ae540d72ecab44d938b7c"],["/categories/前端/index.html","40952974aa756f93cef4519fdd2d2d69"],["/categories/大数据开发/ElasticSearch/index.html","9e46de3bc86aa80a10b90f3971e71837"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","910e7a959d86ee3647520517d32c38fb"],["/categories/大数据开发/HBase/index.html","24c680ae38cf678b297957eecec47d93"],["/categories/大数据开发/HBase/学习笔记/index.html","e704ef5d0b8e0e36dc96b5a859d43d75"],["/categories/大数据开发/HBase/环境搭建/index.html","b46fbbca4aa9dd2326c028864e6827d3"],["/categories/大数据开发/Hadoop/index.html","2315bfdb08ca2ad2d16a99fb4acf7d15"],["/categories/大数据开发/Hadoop/技术/index.html","e1feb1ba087b58383111f9b3174234b3"],["/categories/大数据开发/Hadoop/环境搭建/index.html","56298e1c472054d80d7eb1e550f23c1c"],["/categories/大数据开发/Redis/index.html","e2c0b092f50467aa32472e9cce9216ba"],["/categories/大数据开发/Redis/技术/index.html","fd0c063d7baaba9423e5ad3b63917fc9"],["/categories/大数据开发/Redis/环境搭建/index.html","af0498110450656fa66d97dc5469e9b6"],["/categories/大数据开发/Spark/index.html","8dc1d002795f41091e10780cc3f987f0"],["/categories/大数据开发/Spark/环境搭建/index.html","6de9b9dc2a30b0491869d54a70add9f3"],["/categories/大数据开发/Zookeeper/index.html","d7e04eb3a81dee6a29333931db287b1d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","2e65ec12ef6ec31bd3668f4e03203558"],["/categories/大数据开发/index.html","648f154e65f108105e03745151e8b347"],["/categories/学校课程/index.html","c5c1ba533bb77aaa317fc219fbbbf5d4"],["/categories/学校课程/计算机操作系统/index.html","6145b8a88607631f6d74a1453219e6eb"],["/categories/操作系统/Linux/index.html","040450d99b43e98e093418fec52baa57"],["/categories/操作系统/Mac/index.html","cf03ece3a66fbba0d9f48aa300830393"],["/categories/操作系统/Windows/index.html","0cf53876d73b39373baedfa3e4737b9c"],["/categories/操作系统/index.html","e4b3247543971d6c92ba925f471dfde8"],["/categories/数学建模/index.html","5347a68e895896b8121651608d026ab5"],["/categories/数学建模/latex/index.html","1480578462f5440991354e2ab32d8a3c"],["/categories/数学建模/优化类/index.html","ff5fc98885da80631c6a6249d58fe04b"],["/categories/数学建模/优化类/现代优化算法/index.html","46ca2b3d821f2558ea885ecb25ad7d88"],["/categories/数学建模/优化类/规划类/index.html","f564b0f28de11bf79151a631346e9cf3"],["/categories/数学建模/绘图/index.html","92c61df45e94c61b8837ef860ed6ea22"],["/categories/数据库/MySQL/index.html","d204800c49c79de0312e006ed6bf71f2"],["/categories/数据库/index.html","386cc15c6dea45391c0044c9ac554cae"],["/categories/数据结构和算法/index.html","138667d89600636b658f861b30c7a1e7"],["/categories/数据结构和算法/page/2/index.html","4bfe57e67a484a53d892385ffbe123d1"],["/categories/数据结构和算法/基本原理/index.html","ebea546d589b13e9459d2f36639ff420"],["/categories/数据结构和算法/基本原理/page/2/index.html","27a8111fa25f0429068382557d0022e9"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","2eac7936b0e4d429d8b61b511efd6a3c"],["/categories/数据结构和算法/基本原理/动态规划/index.html","0cb98bd9eb8d7c1d3b78014924a38381"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","b915501ba6ab5fbc3ce4bcf3f2158b35"],["/categories/数据结构和算法/基本原理/图论/index.html","0ea8f51a1f09a68fe5e791f8735e99b4"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","b10d23c0b880654ab9f1760714727fae"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","c4e4a989cd06add64fb6af53652d8b7d"],["/categories/数据结构和算法/基本原理/字符串/index.html","44c6d668a25bd1ac48939e787975ca6f"],["/categories/数据结构和算法/基本原理/排序/index.html","2aa9a0addb70aba98416588795e67bb7"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","78bc90ce03e995b95e28d8079ae15cee"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","9de5c9e0238617254394548c178c64c2"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","2eddd97214f2fbaa0d527ac673429133"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a63668a52d8fa844a36b8a1ce2e602d0"],["/categories/数据结构和算法/基本原理/链表/index.html","81b204256fa5ed58087e2260e527b791"],["/categories/数据结构和算法/基本原理/高精度/index.html","6cc0f3ae12eaa8a8e0d99b1fc6a93a0a"],["/categories/数据结构和算法/算法题/index.html","4967d86c4afe20f2fa901dea087248a4"],["/categories/数据结构和算法/算法题/二分查找/index.html","4bfc538782b9ada3c24cd9d64888998d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","a935ee188a5c293c1fb60a960c9bc87c"],["/categories/数据结构和算法/算法题/动态规划/index.html","59466c211bd984b7080fe8646c76239b"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e6b3f8f3bc69d3fd2745d156eb3c82dc"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3732bf66128fc57926bbb197ac17567c"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","6cb22fdb79e4b7621234d79c4efad2a5"],["/categories/数据结构和算法/算法题/图论/index.html","038e3f3f454d67a2e235af21965252f7"],["/categories/数据结构和算法/算法题/图论/树论/index.html","02289b788560fad9984e58187fcd5a73"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","81676428804b32b518d2110de3f3ecae"],["/categories/数据结构和算法/算法题/数论/index.html","dcea02a114bcbbf899d15d609dd3dc5e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","9f1a18a3bdd741ca03e1098035e3f91a"],["/categories/数据结构和算法/算法题/高精度/index.html","e4050531e221619168bc077f698949de"],["/categories/杂七杂八/index.html","f927023b2614bad8c21704b063939bf9"],["/categories/杂七杂八/博客搭建/index.html","4a399c8f24e107d972a1ef65802cf6e1"],["/categories/编程工具下载/index.html","c36dc59ddc5fc80d4bf4d293efd3b9a1"],["/categories/编程环境/index.html","0bc57ed2ea49d01a37a989fd279ea9a7"],["/categories/编程环境/大数据/index.html","d3eeb46fe28234725a0a127ac0c3a722"],["/categories/英语学习/index.html","bdae80d5365c59e8f973eb8c74db81f3"],["/categories/英语学习/英语语法/index.html","5423a8b628db2d699a4ce3e77d00482c"],["/comments/index.html","313a60078848aaeb5b107d4c0e0ace15"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e1bec76a395fd34f6432416c90a580b6"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","e804bf8071f99ab61ea567134d96e024"],["/movies/index.html","09867f5a77275f5d8de74f259c060ab6"],["/music/index.html","1e9aedacb6045c23a6175338d294b4a6"],["/page/2/index.html","1c7c9285f88b23a984e5daaf49bd67f4"],["/page/3/index.html","6be0ace32cde1b25aaf6322ccbdcd5ca"],["/page/4/index.html","6c21d027b2fbdb48ca7414865c09a16f"],["/page/5/index.html","3ddfc00c3170d9893e53ea7f25611b4e"],["/page/6/index.html","68ed3cf5d90cfc747833f8579992a0ca"],["/page/7/index.html","6917acd3f84d0ec3bcff9c4853fbc287"],["/posts/1021360842.html","2292bbfd6d238cf4e99ca65a38490f5b"],["/posts/1120620192.html","52b065d7ad34707a8cc37e5c3bbb8369"],["/posts/1137707673.html","5ff38b752df269ffac472cb50be1697a"],["/posts/1141628095.html","64981d99e36ee75cfeef2eb1d72ec05f"],["/posts/1168613674.html","423815545222b2604fb2e64d57141c19"],["/posts/1219920510.html","479b9f3028fb319ae6e1e85cade3c2d1"],["/posts/1222166338.html","1365e2f072bf667fc082a73b48aef23d"],["/posts/1259097482.html","4abfa876c2a008eecb61a8e70ba20582"],["/posts/1271036369.html","801bb1e12c28b4cdcb0a7dac392eaefd"],["/posts/1312847445.html","9a85875110599c40ebc8312523cc5569"],["/posts/135355774.html","6aa700860ac6a0da6fe0c34955c02228"],["/posts/1375344716.html","5cc327075f81916b1271d9389908cf60"],["/posts/1388991698.html","fe8877b2cdd7bb40a93ff6b82927b6fb"],["/posts/1410315814.html","12020cc5c5524d6614a205913fccff35"],["/posts/1452790229.html","27e77dd9e24f0a2022071e82441b1a7b"],["/posts/1470079884.html","e9cf24e02a192b67c45f3b3804306abd"],["/posts/1470079885.html","19502d9a660eb55cbe3e4354939999d5"],["/posts/1470079886.html","0acfe5a9af99f75a94ba394116d8b3bb"],["/posts/1470079887.html","f6b7f7630d3fb55db1c206930fa8449b"],["/posts/1498536549.html","101a70df170e890ac689a67796d188cd"],["/posts/1539568593.html","8d50ddde07a65110e46210f31d56c3e6"],["/posts/1547067935.html","fe1dad97fc83173047f4652870f5295d"],["/posts/1557866301.html","b7638ffd5f9e08f9060e066ecc28e37f"],["/posts/1571776361.html","ba623d2c048fb07f0ecb3a8ed0d121b9"],["/posts/1605124548.html","7ce09535bf7c628d4821c36b02b9981c"],["/posts/1633036852.html","3f3322d297f3b9cf26268ace16f1714a"],["/posts/1667740714.html","7cd9b2b68ed3b0a65187f859f06123cb"],["/posts/1674202625.html","85e60853d99519d931287fdf87b7781e"],["/posts/1765123828.html","32cd3c157fed988bf181c5d8a6f4a061"],["/posts/1767336200.html","acf318943fb86adfc5a1eaca0e9c435d"],["/posts/1776114197.html","a3942f8fd623a586328d079974c10deb"],["/posts/1817748743.html","05f656556a74f35723ea52df8d258c8b"],["/posts/1925125395.html","90c79202ff5660d2488137d7c70e661c"],["/posts/1966191251.html","467a904ca9e40b7e0690ba0219935601"],["/posts/1987617322.html","f1ca179476fe84ff142adcaa5a6a1fc6"],["/posts/1999788039.html","c5896371e66666025fac921a0c7cc4c3"],["/posts/2007534187.html","00679dc6b5bfc142b1763ec3d0b58bb8"],["/posts/2075104059.html","7e698403b9db88fe87e586274eeb7faa"],["/posts/2087796737.html","a95e8579761f1ca825f4ad6e3b2e41d1"],["/posts/2106547339.html","972a7f520fc49a3f657154d837e84d19"],["/posts/2207806286.html","7986d77c5c8d25110d6bf68145dae533"],["/posts/2225903441.html","b37c142a2aee6e9a7af0ae0f2e045aeb"],["/posts/2265610284.html","dc77a6dec3ac11fc27ccfd36d919462f"],["/posts/2281352001.html","64c5000709dea96ed3e4142339f77870"],["/posts/2364755265.html","63eba444455fad81c95a4f44ed13ee27"],["/posts/2414116852.html","25ab19f9d15cd504c1ba34143ed31137"],["/posts/2421785022.html","4b0cbf5b5a60ad4f22592715c17030e2"],["/posts/2445543724.html","a7b4a9edc4aa8a99cebaec8a10592945"],["/posts/2482902029.html","9212cf8613821325d3815aff300a7401"],["/posts/2495386210.html","13633720bbcb6b15be3c8a75f02aa8ac"],["/posts/2516528882.html","1cb772231b2c59a6f8308fa0f26ca760"],["/posts/2522177458.html","a06d96d6838dd5d3ee9bb9d0c7c654b4"],["/posts/2526659543.html","fd80c6ce50ccfcb2c1821dfadb62f6dc"],["/posts/2529807823.html","1bbf715fa86539531606c47939e7449f"],["/posts/2592249117.html","a7c267c1b75788ed793dcad9cf6ec4ec"],["/posts/2596601004.html","5422475902db7bd43cd3b97312c47dcd"],["/posts/2697614349.html","31f2b6becec326fbc80915ccff40427d"],["/posts/2742438348.html","ba0e1851ddc907e09f509f9d431c5489"],["/posts/2768249503.html","0c259a7104ab56e955bdc85d6e7ce3bc"],["/posts/2864584994.html","cde567b147ea501632fe102f8ca22bfb"],["/posts/28838462.html","638d27a168845f68e16f91d014143bfb"],["/posts/2888309600.html","227ff9092ac64ff4478fd840b1ffb741"],["/posts/2891591958.html","2eba4af7db405a08375fd6b18c8aef93"],["/posts/2909934084.html","a60576d3cf28cad46b7d5dd9af01d2cf"],["/posts/2920256992.html","ee48e813eb44dfd7613b671a7b31d624"],["/posts/2959474469.html","2896ea6fa4c61fa9baac32d68e2a58fe"],["/posts/3005926051.html","f63dd384528c7cd589e1c0b38b92568a"],["/posts/309775400.html","9d2a37a75d9f98b468632b6386657918"],["/posts/3156194925.html","c9a4eb9eed4fbfcda7fececb56c03411"],["/posts/3169224211.html","86bb9bf73a2aff682c8f775466127817"],["/posts/3183912587.html","10f721de6a3db0e6b75d73d9e710ed35"],["/posts/3213899550.html","a69d60de116759be96089d777bf19c41"],["/posts/3259212833.html","bc611f0a2fc843418dbcba62597c8535"],["/posts/3265658309.html","01fd654e2fe0e14301b7fe184834b383"],["/posts/3266130344.html","9c0f87181a87066adf99ab851ee30d5c"],["/posts/3292663995.html","bddcfbf55d778ffbbc616c1a3f779d83"],["/posts/3297135020.html","c96bfef00ae80255a4640964564ccfa3"],["/posts/3306641566.html","9705440d8da3bee45076b8eeced8204a"],["/posts/3312011324.html","086c168a844540c119685b666b0a314b"],["/posts/336911618.html","4528174e7d04e2be4134c51725d4fcb2"],["/posts/3402121571.html","5cae97f0cf4a63fa5b12c32a05bd6e6a"],["/posts/3405577485.html","795aab035951db9171c095479fb17c31"],["/posts/3413737268.html","845a4fd4d047c54799e8d6ab682fa177"],["/posts/3498516849.html","ea22ffbe47590b3f0530a4e5336fecff"],["/posts/350679531.html","4b160ae25be4bccfb6a72e3e08cb5aa2"],["/posts/3513711414.html","d4f628c212cd60b690c0204c7e0fb98b"],["/posts/3523095624.html","7c0930b2e7174b170b625ce79a312cbb"],["/posts/3546711884.html","5a96935528c111b706a98fa0f9ff1017"],["/posts/362397694.html","5440ee455524d259381bb432a81e32f0"],["/posts/3731385230.html","6113d71a707a24dde5bd7f3cdbb84dab"],["/posts/3772089482.html","b239068e57121a6f3d6064c818ed2d30"],["/posts/386609427.html","32ac90b26c5264797768d251a51e2d15"],["/posts/4044235327.html","481a61fbca5d464a510c3f3707b5eb23"],["/posts/4098221856.html","af3f6bf2991504aacfaed3f93c8eff85"],["/posts/4115971639.html","a8014fe0509e8e91b895271554deb304"],["/posts/4130790367.html","d979d7ffc28527b03a49ef98915fcc52"],["/posts/4131986683.html","a277a4abaa1de052ec37b76b70754a44"],["/posts/4177218757.html","745a044009a789e8d9d9fb2c9a2a630e"],["/posts/4192183953.html","a8fde8e0984513d9a26b7812618cf873"],["/posts/4223662913.html","54112594d090f5c23d20e7ccffe598e2"],["/posts/4261103898.html","4048922edb7d0efd4ebbc928800e694b"],["/posts/4286605504.html","48f42877ace9ed8b4ad007b33abf393a"],["/posts/449089913.html","cfb6878a776083b2d726afa479ac8e82"],["/posts/469277133.html","22ae5f420f619a72cc697ffcf2933671"],["/posts/469711973.html","292b0dda9587eecf7c49861316c51895"],["/posts/482495853.html","cb247aa866fb15bbaa4eea6848d8ce89"],["/posts/488247922.html","b90f0156d5ff5aa1a871918daf48c797"],["/posts/517302816.html","78486df2999da621c108ce88122a57d1"],["/posts/570165348.html","feff078a8b9b445c3c4f833ea5bc49ef"],["/posts/595890772.html","51cacd96feb7a5557fe29c34adf9a7aa"],["/posts/67485572.html","aeb433aaf8e6d55f282ca48a8a7bdbaf"],["/posts/694347442.html","922565deefc916429d432dc4dcce2893"],["/posts/707384687.html","fccf989553648ba579aeece86850aa17"],["/posts/71180092.html","a92758c64f0561591961a13bddc2d96b"],["/posts/716459272.html","340a401c9c4b8c63571b84a216d20871"],["/posts/765481613.html","c1b225ca07079f7e94a1864014b6e06c"],["/posts/778231993.html","f4a153d03572257c9b246b22608abc75"],["/posts/795397410.html","6691dcb76e15ca7998432d31180f125b"],["/posts/820223701.html","d6dd983f62c83798c816993d23b2d1ef"],["/posts/830372185.html","05bf64ff068cf9338163cff7eb1f7af4"],["/posts/88294277.html","047697809dce2cce2bb84701b7f3a31e"],["/posts/939963535.html","e8dcf72afbe7b2119a6af2270406132b"],["/posts/983786067.html","6738ab08401207653350c8d71b02f8d3"],["/posts/999620786.html","3869a8a9b5f378eb8e0342bf9bda7957"],["/sw-register.js","7ce0f38b569fd696c611c47cf381243f"],["/tags/C/index.html","88ffd60f06263976b53d0e4fc539db6c"],["/tags/C/page/2/index.html","989cc694ad4b3e20331023e33654e73b"],["/tags/C/page/3/index.html","f34a5017620e9123b005833819ca10fb"],["/tags/C/page/4/index.html","dc1f78a81e95cdde830d3419915b3e85"],["/tags/ETL/index.html","07b1b64184a75204a72313d466cb297a"],["/tags/ElasticSearch/index.html","a1bcdb0c9300daf1f4e911b1a92bf71b"],["/tags/GUI/index.html","223fde25f1b342bb26f80fc54d21947a"],["/tags/HBase/index.html","a7b59ac75dd6bf67ab2696aaf7f9c9c0"],["/tags/Hadoop/index.html","6e83d36ed379cd729e990421e38b1af3"],["/tags/Hadoop/page/2/index.html","a7c811d4835fb8f1a0403c2934735e22"],["/tags/Java/index.html","84cf404b231c3abb8e0402100c7a78f3"],["/tags/Java/page/2/index.html","68ed847abf4348bb47413b415cc2e592"],["/tags/Java/page/3/index.html","391ed7b853cd469b95abd0dc44ec1c4e"],["/tags/Java后端/index.html","126e14f58b71a4c343479a32a29dfd88"],["/tags/Java后端/page/2/index.html","55fb538d723e67348c633dee61db43e2"],["/tags/Kettle/index.html","2038514f24034e727d434e139404a407"],["/tags/Kibana/index.html","81996aea407d3683710b2e1b18d4f23d"],["/tags/Linux/index.html","5068dfdd5c3fe186a9a3c1c3d1fe88ba"],["/tags/Linux/page/2/index.html","9a6f0f0061c6fc151acfdaf4a49f16a1"],["/tags/Linux/page/3/index.html","e1ab0748d9205f4cc15cf94b816f42d5"],["/tags/Mac/index.html","4dd3512eddd3d2b7676865b6a5263a2b"],["/tags/Mac/page/2/index.html","770a63cc25c6fe257a4da8e1813ab24b"],["/tags/Maven/index.html","fb24a738e6bce898b81ef05eb3b18a00"],["/tags/MySQL/index.html","542fb337da4b0f4a3f33a29460347f97"],["/tags/Python/index.html","c496e471bbbd5b4714b55d486b12e3be"],["/tags/Redis/index.html","fce3b154acb8bc281f216a7a29f62adc"],["/tags/R语言/index.html","29721d44c54b2cbf8c9ece22380dbcca"],["/tags/Spark/index.html","ccbbcec12dd16e7ef7f648be3528d446"],["/tags/Ubuntu/index.html","58a0bff704fc37ad1bdf68c4dedd98d1"],["/tags/Vue/index.html","c672a6935d68747272c2844cd45817a5"],["/tags/Windows/index.html","f354c1a486fd269ca4fb47b4263b7771"],["/tags/ZooKeeper/index.html","0b13e694fd942322aadbbddaeaee7639"],["/tags/bfs/index.html","31a7c1c7baccd5afe6f142bf60ae7e4b"],["/tags/dfs/index.html","a4290933f7582d5e7cff67513420c039"],["/tags/folium/index.html","a512f0016a5ce3abd41516e6c156e0f1"],["/tags/git/index.html","ed5f874a66886cf7985288af606817cd"],["/tags/iPad找电子书/index.html","c745f6b2f14c373b1e647b2d08f9a7ac"],["/tags/index.html","eea7464943e9bf2bd993705a33ae9cf3"],["/tags/latex/index.html","f1ec7553a909f8bc219e2bbb4b1f191a"],["/tags/中间件/index.html","d6abd2e42cc51db3ce73aa0f20cb27ad"],["/tags/二分查找/index.html","184fec82b49b8007f5c179feaec555c5"],["/tags/优化类/index.html","d2846d3e81e398e7877723b1cf829386"],["/tags/前端/index.html","cf03a1e2c980077e5315ea4dc14fb73c"],["/tags/前缀和与差分/index.html","6d6505dc02dee702020d410fcca13aad"],["/tags/动态规划/index.html","a1235aa948185ac26b7c3a1bc6c795ff"],["/tags/动态规划/page/2/index.html","1c4826b19197b0e3a71565f08f339b14"],["/tags/博客搭建/index.html","94783fe9d98e929b99fc294afe71b5c0"],["/tags/图论/index.html","b20fd9e80803c30d56f99451b2a52f22"],["/tags/图论/page/2/index.html","af6ef00adb116d73d6f2c454997cb684"],["/tags/大数据/index.html","d27e40e557f9ae7022d3a6fda3ae64bc"],["/tags/大数据/page/2/index.html","c2b7137629ac59c824f60d023ed7239a"],["/tags/宽度优先搜索算法/index.html","69160dc3dcc72bfaed094452dca6c191"],["/tags/排序/index.html","db258967a4f771baebfe9c5f34ecd30c"],["/tags/操作系统/index.html","21faef5220b283a7a60672e46ab50f95"],["/tags/数学建模/index.html","ebd79b7ee112ec5b4af83625810cfb19"],["/tags/数据库/index.html","e4c2888878b72c02ae9901449efa43b6"],["/tags/数据结构和算法/index.html","8eb44011d6bba24ca5343f6bb1527b7d"],["/tags/数据结构和算法/page/2/index.html","d59bada59f75042cd2bb7b6315aaa6b2"],["/tags/数据结构和算法/page/3/index.html","d4079756766bb85da313305b5bf43ded"],["/tags/数据结构和算法/page/4/index.html","b9719af877e519268f71462f0c3656ea"],["/tags/数据结构和算法/page/5/index.html","ea6195bccc6e796e3d98dcbecd1fda06"],["/tags/数据结构和算法/page/6/index.html","04f2afba7efe0d5243042ebf2f663ccf"],["/tags/数组和字符串/index.html","be5ca1cfb5545c65d12922b84ec61c67"],["/tags/数论/index.html","adf49027b831ebd9898676d4ec271418"],["/tags/枚举类/index.html","a8c67dc6ca611d9a346968e212519002"],["/tags/栈和队列/index.html","c819fcadfbd8970efc368e0c49245041"],["/tags/树论/index.html","919f867d968efc34dae63518c5e89a84"],["/tags/测试/index.html","7993e7a9a4744c6a894ca39af665555a"],["/tags/深度优先搜索算法/index.html","afb432820b1aa690d9bb87b0a6c3463f"],["/tags/环境/index.html","fe7b52947935e7009c471bbc3b1047b6"],["/tags/环境变量/index.html","886664bd61e950b39b3867b841a94c81"],["/tags/绘图/index.html","3bc3b765c75a0e43604ec6c5c8678ea4"],["/tags/编程工具/index.html","9eb56f9e78cd516ca3c8f3ee3f22c707"],["/tags/编程环境/index.html","bff3b220f4c269c1b2ea8bedfc025409"],["/tags/网络编程/index.html","31c9ee2d371e2aa47746c6e282e64b4d"],["/tags/英语语法/index.html","a6bd43cf64675087a94e28fc15e1fc42"],["/tags/计算机操作系统/index.html","8d30bd577394d03fb794b3d82a578fa5"],["/tags/论文/index.html","9e3aa19e5c5b2151f3f2efd35a1b022b"],["/tags/资源下载/index.html","8b1f0552363109a30b6ea2767242d620"],["/tags/链表/index.html","20b89162c14ff890858653624c8aa222"],["/tags/集合/index.html","b6c4a5473d6af5e9a4366397017801b3"],["/tags/集群/index.html","bb845ff0d7a24c1531ac9bf31d19d30d"],["/tags/高精度/index.html","dbe3ce2175bd83ccffaf64f582d320f8"]];
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
