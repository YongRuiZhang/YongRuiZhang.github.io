/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","89a5cccc6d7b5d45da1880806debf6f0"],["/about/index.html","8dfd955db854a05214fa7f6674588ffc"],["/archives/2023/01/index.html","55a0171a9abf04b21e9b3163ef8dfc49"],["/archives/2023/02/index.html","a5eada35c683bbdc9ee4f621017a0557"],["/archives/2023/02/page/2/index.html","fbee509d822f3c3531c57f560d53067c"],["/archives/2023/03/index.html","b54ca5000a1325bc036522ef0ef04487"],["/archives/2023/05/index.html","9a62a7bd000af90f2a051aa146c3d8a9"],["/archives/2023/06/index.html","6637cc0952fccfc9e39a5d448a270a9a"],["/archives/2023/09/index.html","7cc2f4bc9e07c1d7165350fffe3d53ee"],["/archives/2023/11/index.html","3d739388085cac97195b1cf2cbc7f621"],["/archives/2023/12/index.html","26a0404b83e0d09aabfd23b6f65c2045"],["/archives/2023/index.html","50c075504382e3260e1e2bf4f4c04412"],["/archives/2023/page/2/index.html","7430bf9b966e9d206a888edd537c8897"],["/archives/2023/page/3/index.html","821e1dbf18f00739fb44be0bfae782e0"],["/archives/2023/page/4/index.html","18f526e5967b284b28b0a5ade8c3947d"],["/archives/2024/02/index.html","936604399bcefbc002ab88c755e7abd6"],["/archives/2024/index.html","52df9150f248876b9b4cd1c459470afb"],["/archives/index.html","3e9b1c586a61459e39a24a73d9313840"],["/archives/page/2/index.html","46366e882f5ddc54f58dc199d0802428"],["/archives/page/3/index.html","6dd73d3ac6b25ef33dc5a88f5fc8bdcd"],["/archives/page/4/index.html","235830f181956414d98a121a6a025a29"],["/baidu_verify_codeva-qQP2iZOMLX.html","92a30d8b4df6433c0f273b278edf2182"],["/categories/Java/index.html","d595ba56454e72fcc27df70b8aa5425f"],["/categories/Java/后端/index.html","1894d78b34e7ee66575151ced88ab2bf"],["/categories/Java/基础/index.html","9bf68d96df4696e35250eadcda8a59b2"],["/categories/Java/基础/集合/index.html","51b507fc8523f75cc0ac0e589fb66e9c"],["/categories/Python/index.html","db32cf9fb1c6472fad0ab41b23876bd5"],["/categories/Python/编程环境/index.html","80278a0dec8b204eaa24ecc2df728f60"],["/categories/R语言/index.html","963b6c04f20797821cc7e2c0fb46e15e"],["/categories/R语言/编程环境/index.html","bd3aa711c41d690e147bdb7130304f5e"],["/categories/index.html","a119e808c11c06610e0962c42e8a8e65"],["/categories/中间件/index.html","80856102c9be21dc0001bb266f55799f"],["/categories/前端/Vue/index.html","5d0ac067e5a91b29d316fa6638de22bf"],["/categories/前端/index.html","6dd0acd0e140a1f1928b1a44a7d27c00"],["/categories/大数据开发/ElasticSearch/index.html","1c9ea8b036ea9d76546b0be5c0d27339"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2b438ba1e96bb82614add89e20ccd2dc"],["/categories/大数据开发/HBase/index.html","2de2c92951a1ff9c61f2cb759ffc77b6"],["/categories/大数据开发/HBase/学习笔记/index.html","fc39ade89fb2fd2f40b12da357e9db3a"],["/categories/大数据开发/HBase/环境搭建/index.html","9212a78a7a6a4a198952819b74ee74d4"],["/categories/大数据开发/Hadoop/index.html","42e2f382f9d7d567d41e130abb27a75b"],["/categories/大数据开发/Hadoop/技术/index.html","5c3cb117ff9a33a54631428139d894f4"],["/categories/大数据开发/Hadoop/环境搭建/index.html","0dfbe9e56889e94a4caa015113673856"],["/categories/大数据开发/Redis/index.html","5ca2259cc1a59bbb35a63286af1ea2ec"],["/categories/大数据开发/Redis/技术/index.html","82130dbaab7d5a93184ac58af726650e"],["/categories/大数据开发/Redis/环境搭建/index.html","9d303aeb6fe7d331e33a1c69a1e79914"],["/categories/大数据开发/Spark/index.html","554f23038ac538963a3fc8df36783d58"],["/categories/大数据开发/Spark/环境搭建/index.html","547b6cf71724408adf01e425ba67258f"],["/categories/大数据开发/Zookeeper/index.html","c32ebecc680f6b436758b7231e976254"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","afae1f7126a4069f88cc4a3fe9487c44"],["/categories/大数据开发/index.html","305993826c7491ac066553cd7f91f950"],["/categories/学校课程/index.html","ee38b817841ff68161ff5d2fd28d3ab3"],["/categories/学校课程/计算机操作系统/index.html","7f8729d2d8ad09939863295ae450a6fa"],["/categories/操作系统/Linux/index.html","9afd3dfe46fd7c9fb9cdd054f5c8b0a4"],["/categories/操作系统/Mac/index.html","d562b2d17de60d52f28ec13e9478a73e"],["/categories/操作系统/Windows/index.html","6f84da92eb46f2dec3def7b79abbd1af"],["/categories/操作系统/index.html","36c9e47a5ae8046d9864ba0c8216e87e"],["/categories/数学建模/index.html","c33eac7be59d7865bbeb1bf8afd8d45c"],["/categories/数学建模/latex/index.html","db07a9af0f413d71ab22488a350f7070"],["/categories/数学建模/优化类/index.html","66fda1e1a0242cfbfd8ac2404cda6e02"],["/categories/数学建模/优化类/现代优化算法/index.html","febda59e34a80b5582328d99b2e8ecac"],["/categories/数学建模/优化类/规划类/index.html","7ba8708e105f494b1b1b561a4462e708"],["/categories/数学建模/绘图/index.html","a39fa32026a7965ba64d2119496bc845"],["/categories/数据库/MySQL/index.html","76b1cd18cfa3c8b6ed6da58c38ee1dd4"],["/categories/数据库/index.html","9f2720bba6dc21585126746aeb35622f"],["/categories/数据结构和算法/index.html","2d527a3a686f152e93d7f2a2329881bf"],["/categories/数据结构和算法/page/2/index.html","e55c7f4eafb3521ad323677da2491ec4"],["/categories/数据结构和算法/基本原理/bfs/index.html","569f009c7617fd189426364a59fce0ec"],["/categories/数据结构和算法/基本原理/dfs/index.html","c837d13048efd81ea9e3f3f6b63c899d"],["/categories/数据结构和算法/基本原理/index.html","aa1d2919b7925330a491788408fb61bc"],["/categories/数据结构和算法/基本原理/动态规划/index.html","773a07e6c23611ad27b098dde1402be2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ddb07f437325aa3c59047233b801e0de"],["/categories/数据结构和算法/基本原理/图论/index.html","011d15b762c204f903bf0b4b1736b9d4"],["/categories/数据结构和算法/基本原理/字符串/index.html","4f52336e478b941ac277db3431a0f7e2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","5c9db992141534eee414341cc5e6b505"],["/categories/数据结构和算法/基本原理/数论/index.html","08c8cf44d9f350309e54564426c41961"],["/categories/数据结构和算法/基本原理/树论/index.html","542a11849b8248762dcb9379e9677250"],["/categories/数据结构和算法/基本原理/链表/index.html","4daad831a3aff47962da1020e630dac7"],["/categories/数据结构和算法/算法题/index.html","ff3177075b7311e256acf9cf106fa365"],["/categories/数据结构和算法/算法题/二分查找/index.html","3f28e79b48e8c58e8280f22711399ea7"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","7ea3044ec721b7687c44c6f12184142b"],["/categories/数据结构和算法/算法题/动态规划/index.html","78ef107fe2c42e60b19067ec900e4e20"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3bd2e9b700bd790fd79eed195dc76c18"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5c61137680730e3ac1ce1326dd4091ff"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","208ad844bf6ba2e15c7369f7a91c4efc"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","79cd8c3fc42d8673f400c274fe0b3c18"],["/categories/数据结构和算法/算法题/数论/index.html","d831df616a243606d696fc1dc7e45c5f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f8fe7ca18e239488a09c550e91fc7f26"],["/categories/数据结构和算法/算法题/树论/index.html","05ae1690f60eef2aff87ddf618d60ec5"],["/categories/杂七杂八/index.html","dc1332f9632f7031094409ae2ea67003"],["/categories/杂七杂八/博客搭建/index.html","16ab708f8d54df33eaa0c2ec95121ce2"],["/categories/编程工具下载/index.html","20b0a22bc72831863bbe32dc72aa4e0b"],["/categories/编程环境/index.html","e937c48831d736b8c4068fb896378e15"],["/categories/编程环境/大数据/index.html","1b97a7884a1ff702938d004917dd91af"],["/categories/英语学习/index.html","a1ea9c647c28409ed83017235533c862"],["/categories/英语学习/英语语法/index.html","6837d6308b933d847551c50b19355471"],["/comments/index.html","5cf82d599c3d8e21175d6e37e4555997"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","fc6a0de2c2265bde7b7de079eb7b4061"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","6cfb4a30e169e0ea17b5caa689bd08b8"],["/movies/index.html","3f1bb2a0ac9f849985589093407e69d8"],["/music/index.html","23edad956c4957caa8ccf2ae9222f89d"],["/page/2/index.html","fff7f6315592b25cbbddf0ca841d5953"],["/page/3/index.html","6147ff81ebd7d0e855132835384502d4"],["/page/4/index.html","162dd0e19163a675d42e4759537a6a1c"],["/page/5/index.html","8cbf7e892b4c890db87946a16f76e0fe"],["/page/6/index.html","dda4612a44d30b94f4309d553b08864c"],["/posts/1021360842.html","627441040c622aad92e995aca1502ac5"],["/posts/1120620192.html","b3747cc5052f53739f13e1adab5c39f5"],["/posts/1141628095.html","bb5352f1f0f68f04e8812a176ca2bb43"],["/posts/1168613674.html","dd504529b0bd46912325028092ceba5b"],["/posts/1219920510.html","2934ab45860bd3152406bb9ea673b328"],["/posts/1222166338.html","207a303e980edaf80b109edef36d632c"],["/posts/1259097482.html","d837555538d5c11d4dcad158607ec442"],["/posts/1271036369.html","2d8f19e112a85a68c40df2d7a2f38ab3"],["/posts/1312847445.html","89e3125a2c1aaf818db96ce78e219ad6"],["/posts/135355774.html","6d98d3e948ed9d59c623ea5376687b7f"],["/posts/1375344716.html","b79388998e120a6fc5c49919409c677d"],["/posts/1388991698.html","3a66d7ae77212ec47c6bceb42b3be021"],["/posts/1410315814.html","f6d5384886714bf056e53f8907017f33"],["/posts/1452790229.html","38e9d44593c100c1b09737b491da0e16"],["/posts/1470079884.html","4e100de0ae85b22ce6c9028c76562ba8"],["/posts/1470079885.html","39e83799698fb3e2fd38b3642a080795"],["/posts/1470079886.html","25eb1b43324f8a632da08e4a9147e31d"],["/posts/1470079887.html","cbe8c3203b502b90a86db0e5a4987065"],["/posts/1498536549.html","6202ed0adf3d626bb0bfa8c206741fbe"],["/posts/1539568593.html","4ccbef461f3b05c75779d48d380672f3"],["/posts/1547067935.html","6763e432b141e757244555eeb5c08aec"],["/posts/1557866301.html","c0458e4edc36dcaf8f93c162e0d5273b"],["/posts/1571776361.html","54854ed5a03243e4edb058c1af4a8dae"],["/posts/1605124548.html","dc47e9e7e99d29e9c34fed9fb2b5b21f"],["/posts/1633036852.html","329b8b7ff9d4b5d65b2a2d7250c2226b"],["/posts/1674202625.html","128cf27cea2e8e984385ce621140f78a"],["/posts/1765123828.html","f08e185dcc4686c20d7e5341f5932d81"],["/posts/1767336200.html","f4ab1f946c5dc3d9a402e3c7010f168b"],["/posts/1776114197.html","2fbc74fc67c997319ca1d69ee5bf459e"],["/posts/1817748743.html","e3830ddbe47ca4a5620abaef249b45af"],["/posts/1925125395.html","56cf80bdc77ff957097dbc512a531da6"],["/posts/1966191251.html","202ea29db59b87476c137b9d0c77bd7e"],["/posts/1987617322.html","9ca0a84d90568c8d637a6e30a74c48ed"],["/posts/1999788039.html","1dca7710c10ee3bd0b93ebffcd791582"],["/posts/2075104059.html","ea948a987633411cd7f4d38726f96d6e"],["/posts/2087796737.html","8920389d615dbfe15fd030dcb679b9d1"],["/posts/2106547339.html","0a88b0f85a957da4fa92fb2119f15b8e"],["/posts/2207806286.html","b889067ed5d3b022e504b77e856c730d"],["/posts/2225903441.html","45aad270ca78a7228e228e3ac39ae8a5"],["/posts/2265610284.html","ce711284b90b5c0e2882e5ac200d098d"],["/posts/2281352001.html","1f37b7af0ae688417f4504f7c9d69984"],["/posts/2364755265.html","6e3934a6f6226056d7b9b0184bf83da0"],["/posts/2414116852.html","b39b22150df3417904857b1d8e9ca4b0"],["/posts/2421785022.html","1f0a83e428618eeb5538cdc777ad1685"],["/posts/2482902029.html","161e6f1ecdbfe7e3874553a4b3c722a1"],["/posts/2495386210.html","5f6b59e78733774432d9040d3002d06c"],["/posts/2516528882.html","6471513191584cf03e0a02e31de34a1a"],["/posts/2526659543.html","247f96ddbfc8bfb45faa42e0b04d8fbc"],["/posts/2529807823.html","05c32ee1ab978a653ec9ca0b21f7a23b"],["/posts/2596601004.html","3089bb334d3465fa2c79ad884137f253"],["/posts/2697614349.html","2ed616e908dbd2e6ae01a7cb102e9796"],["/posts/2742438348.html","e78bd8265cfeb302001963e4bfab03ad"],["/posts/2768249503.html","1c930f4f82ad3732b700101fc92cca71"],["/posts/2864584994.html","f6d2c0acc824e5250cfd5cf8bf0a7339"],["/posts/2888309600.html","e3e27812eb9df298c59d4eacfef39ecb"],["/posts/2891591958.html","5cd0f4f4f721af9f705ce7b1f32281db"],["/posts/2909934084.html","a1c8d699fe88270d168120ebb43044d8"],["/posts/2920256992.html","375fd2c3109e7fe3c673493120e97fef"],["/posts/2959474469.html","4ef91f95c4670ac00dcd67151758c3ef"],["/posts/3005926051.html","2ec404316830c7bb94efba279b77de13"],["/posts/309775400.html","c51ed61e54cfa32ba50d5a798aef4585"],["/posts/3156194925.html","f76bf67bca04a45ff691b9c365c86e5b"],["/posts/3169224211.html","c25a810b427bfd06b8dac4c7f359c2d8"],["/posts/3213899550.html","2c1edcc453102a508c38591e081a9ec3"],["/posts/3259212833.html","fad46655e7793391cd776a3b77cd26a6"],["/posts/3266130344.html","b38dfdeee7a9d2a1852960351ad53bf4"],["/posts/3292663995.html","73f6ede78e935920104ff93006bb6164"],["/posts/3297135020.html","6d2b85d3034ddd57ca923612d52a50b2"],["/posts/3306641566.html","d1f58f553d31235b7445255627212fee"],["/posts/3312011324.html","989cd24105feceacb33e7874df48726d"],["/posts/336911618.html","85d22b5bc1b72dbe5efcb9739ebc333e"],["/posts/3402121571.html","f4a5044148fe3d7da6503c740947a768"],["/posts/3405577485.html","66d1729cf78936cd92acfa3e1a1b4585"],["/posts/3498516849.html","23c214fa98fef9278666abd07b554aac"],["/posts/3513711414.html","63cb0985dd1628758f58c6a3a49f3c08"],["/posts/3523095624.html","0f5aa347b485b40e5943c0121e805521"],["/posts/3546711884.html","7f1e74574d482e41de5c1b7e89a78198"],["/posts/3731385230.html","9eec693e412731a49020dba523c1462e"],["/posts/3772089482.html","b7a8d24b9a4c76be9b545e0a8a3bab05"],["/posts/386609427.html","a72fd088a8b7086e5de1a98dc2147043"],["/posts/4044235327.html","d3968f4daf8f35c43ae474d5a2c88c79"],["/posts/4115971639.html","cae17f664b57c573c8b0d5dfbdc8504d"],["/posts/4130790367.html","c24a430cd100db17f4ae0b1040af6084"],["/posts/4131986683.html","dad428f1abcfd69040bb7fd666854121"],["/posts/4177218757.html","48f9bb5ccec4fc789c396a1916106dbb"],["/posts/4192183953.html","4ce7fc350769bd68e8bc4462e7dc1674"],["/posts/4261103898.html","f679f181c6b4dd3fc8c2dd9531043726"],["/posts/469711973.html","9c60f68defd09122a087d51290f7d700"],["/posts/482495853.html","42696cfcaff93785bceb4ae7d933ebf3"],["/posts/488247922.html","515d785cc6974a0953c61601c7a5160f"],["/posts/517302816.html","eac247eda62683bc2c1fad00e9ed5d92"],["/posts/570165348.html","0df9737ae7c58288b608a177cb8804c9"],["/posts/595890772.html","3ac2ac1ebc7ad5791ebd75f775dbdf35"],["/posts/67485572.html","7d3ba3d9d551128e367a2c440841c3b7"],["/posts/694347442.html","430bd40d2d2a1225d7f8f8446910e30d"],["/posts/707384687.html","fa0371878a6ecdbce0d76e6b2abe43c5"],["/posts/71180092.html","79e2c23730a92e18bfb7a13ec56dd2fa"],["/posts/716459272.html","5ec4698ff4edd49e1c7c3ac45643fe9f"],["/posts/765481613.html","74d935a1a334c5baa84c6151a80654c7"],["/posts/778231993.html","f8ec2183bb221d280f7195ad5602aba3"],["/posts/795397410.html","9c83997d714e1a3c7f37aea368dc6190"],["/posts/820223701.html","6b8454ac743725239b0ba98842f0a26f"],["/posts/830372185.html","4c686ee8b88babb94272cc8e1868fdc0"],["/posts/88294277.html","d2bc25e2985e1d35613d3dbf4cceea36"],["/posts/939963535.html","b2f2d32eb2129517d2b902fe7964bb9d"],["/posts/983786067.html","7c080c7d9b85711da9ebfae68d7d5d1b"],["/sw-register.js","bcc130d2785419b5d4bce10a99b010d0"],["/tags/C/index.html","8f80b8ede91364241a897559eb446382"],["/tags/C/page/2/index.html","9c6f5949e31a830496b9584a55e24c47"],["/tags/C/page/3/index.html","c5f6b33a29f640320cebdaab97d3ced2"],["/tags/C/page/4/index.html","8e134421da5ab603f289196d0cf95185"],["/tags/ETL/index.html","26d3a95f36aa2a9870444e091dea603b"],["/tags/ElasticSearch/index.html","377536491b0bc896f62c86ff04a30dee"],["/tags/GUI/index.html","4699ec64c18b17b61d49e0ee850cdd39"],["/tags/HBase/index.html","9ca39382481de3410da7d52df9500a67"],["/tags/Hadoop/index.html","0dfc3a0d81bf606442b69a3d08f79dd7"],["/tags/Hadoop/page/2/index.html","c6c3f9a60bcbd78f318dcaf82e9f9329"],["/tags/Java/index.html","d8ad9ac7850b7577da54bfa84dfe4326"],["/tags/Java后端/index.html","778b8407026441b6a2ebb0e3d79b792d"],["/tags/Java后端/page/2/index.html","8f63d486032cdb7d144100e439218a83"],["/tags/Java基础/index.html","6b43e4426f07b298dee399e9833aeed7"],["/tags/Java基础/page/2/index.html","66ca8f563df9e1da4a8d60b107396483"],["/tags/Kettle/index.html","c45401b02cc9863c42fd937dcecc91f1"],["/tags/Kibana/index.html","dbe15c9bd7ab25302d93631ec7da839c"],["/tags/Linux/index.html","18e1c6b7b23a76f46de094dad0bf14b2"],["/tags/Linux/page/2/index.html","46dbc5cab1309a2f94b9b1db482e53fd"],["/tags/Linux/page/3/index.html","a0a40e104cfce4e5e603d95da6c98d0a"],["/tags/Mac/index.html","949637fc13ac4a26ca93d7016111341c"],["/tags/Mac/page/2/index.html","5b916b9bba565b24373c94f9b35366b7"],["/tags/Maven/index.html","64366bc516877e8ea3f807fdaecdbbd2"],["/tags/MySQL/index.html","2e5e92bcf9402e86203e2c4729bad145"],["/tags/Python/index.html","1062d0255999166a6ba1bc05aae0d75d"],["/tags/Redis/index.html","38b16fb3b1e09d0a19ba12e73468ae79"],["/tags/R语言/index.html","22b3d9ee2ffd52d3481e4c547436828f"],["/tags/Spark/index.html","2e95fe700e14871a0837d15866344e6f"],["/tags/Ubuntu/index.html","d4840842241e078589a608e5c6d7c6c2"],["/tags/Vue/index.html","4af0f9692722bf302e55725c5eb3db82"],["/tags/Windows/index.html","b3b12534804f5bcc3c092a9f8cdbabc7"],["/tags/ZooKeeper/index.html","82b2ba9ad5791ba89f28136dcd20a2d4"],["/tags/bfs/index.html","43ea7f5f8f8970e483143694972d5edf"],["/tags/dfs/index.html","305356100fb6e2327f7000c839225bb4"],["/tags/folium/index.html","9ec638db95ba2ab8cc0e2ced0196b116"],["/tags/git/index.html","0e99a1bfbcc02a4678fcb54953aabef3"],["/tags/index.html","e2154df4d41d57dc27ace0ab64a1783d"],["/tags/latex/index.html","ba52f0a7378d5fabed43909766ab03f8"],["/tags/中间件/index.html","6514fcf2a4e938b51d43379cbee52096"],["/tags/二分查找/index.html","bcf56e332cec2c6ccab02ce282c5bb31"],["/tags/优化类/index.html","00843a6230581a2f5006113666b01474"],["/tags/前端/index.html","a9b387b897e382e4be4520d29f0d53f5"],["/tags/前缀和与差分/index.html","54e833bdb2ccfe4b98a5fd52f462af44"],["/tags/动态规划/index.html","a5d686c65bab1c304923435683df2ab1"],["/tags/动态规划/page/2/index.html","49108ece79c1d31308b47d2102689198"],["/tags/博客搭建/index.html","abe7b9c00b7b7cbe653781c0ddaecc23"],["/tags/图论/index.html","a40f314e6449d956c5b27cd4cb0521d6"],["/tags/大数据/index.html","5d2d414adc193db03e4360aae86c9a38"],["/tags/大数据/page/2/index.html","6119f6696552212a3c7a68a85bd359d0"],["/tags/操作系统/index.html","5f51a04696be96a2e9893a52a7e73fcf"],["/tags/数学建模/index.html","d6032eee0e6cb668112bea359cce0c68"],["/tags/数据库/index.html","0540286ec977e8c8fbc30484cb5e3bb9"],["/tags/数据结构和算法/index.html","3a68fe17893fcf37f705d1439cd973b5"],["/tags/数据结构和算法/page/2/index.html","a037cec34983c8f1b4291fb8d6aca28d"],["/tags/数据结构和算法/page/3/index.html","d10f073cd63b022722a5bc847ddbc399"],["/tags/数据结构和算法/page/4/index.html","179fce95f94dd23a9cf8b7262063542d"],["/tags/数组和字符串/index.html","28c381407d82f385aeb09d3d44518fd3"],["/tags/数论/index.html","22ab91fa09a1b3cf3c03b2d560e16ee9"],["/tags/枚举类/index.html","292624f669e7aa650009f85faf377668"],["/tags/栈和队列/index.html","c0537b278aca1b084ec0fd8a06bf8da0"],["/tags/树论/index.html","4bcb1195126a7dccb4c376a63dfc9f45"],["/tags/测试/index.html","1dc7ee4d38b573d51bce6c60f1e8cb74"],["/tags/环境/index.html","2ea4a781911aed2cd791542e51e306ee"],["/tags/环境变量/index.html","09d4fd6f4e089c418000ecad1df7cd62"],["/tags/绘图/index.html","7a63b8373e4b3962967ec2b3e2e3cefa"],["/tags/编程工具/index.html","a491e73a31f1e4f8d19a269884d5b4aa"],["/tags/编程环境/index.html","e835e5d49a460a89934862dc907c7f14"],["/tags/网络编程/index.html","14861cc05b99cc165bef6e21d8fb177b"],["/tags/英语语法/index.html","3ca52f536bdfbff806d4a5e793ad9b69"],["/tags/计算机操作系统/index.html","f73f0c722337c53d4e8a41ddfd9af11b"],["/tags/论文/index.html","69d2d96a9b497b2f23f2b8938c81f910"],["/tags/资源下载/index.html","b76d51fce74c4be78fa4d28b35eb4e2d"],["/tags/链表/index.html","a07f127396ce3bf06f820a57a5203ee6"],["/tags/集合/index.html","77cbbfd691c8663164ec78f9b4c0db24"],["/tags/集群/index.html","cb6892bd6f15afe7150f887dcfef3f08"]];
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
