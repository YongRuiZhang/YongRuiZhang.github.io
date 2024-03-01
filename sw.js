/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","a65a2049bf2785318216471f877b6970"],["/about/index.html","af39817ed62c031358bd3437b1a269ac"],["/archives/2023/01/index.html","02be219acfc2d5571930c2961aab2393"],["/archives/2023/02/index.html","b48d275a51e05375e8e51094392a2880"],["/archives/2023/02/page/2/index.html","3a84d69882646d6dfe876f27553fca93"],["/archives/2023/02/page/3/index.html","e3cd3ff1746f39e0a86c2d9e98f3c9fb"],["/archives/2023/03/index.html","614fe6136632cfbf36d3f6c8cab70c81"],["/archives/2023/05/index.html","9663a5e689f267a882efda5018f7b8ad"],["/archives/2023/06/index.html","6befd28e13fc1a26609999f2940f68c3"],["/archives/2023/09/index.html","1e46196361c0f7d0576b82fb4e9ef459"],["/archives/2023/11/index.html","27ca85e3f8cfd58b4682d1f48714cc79"],["/archives/2023/12/index.html","a6820cf5fd5f1e96eb4601b569d47bec"],["/archives/2023/index.html","c15f457a3c3b4e37d606297024205370"],["/archives/2023/page/2/index.html","5768a90338410eba2c248d4030c20780"],["/archives/2023/page/3/index.html","1363eaf25d67fd6abe406dd0042df670"],["/archives/2023/page/4/index.html","3359066fe9dc59f0414fed87b28964b9"],["/archives/2023/page/5/index.html","66a38dfacfccdd7d50838e8db43ee7b5"],["/archives/2024/02/index.html","4684bfa77f75de0ebff07a9c1c85cdc0"],["/archives/2024/03/index.html","0e992a9906e52e00f5b77bae01559dbf"],["/archives/2024/index.html","30c7efa6339d4709122ad5fe3e718b5c"],["/archives/index.html","6083f30934e9d5a87238bbb6575fce2c"],["/archives/page/2/index.html","e6876ce393e64206d283eddb1e7edb0d"],["/archives/page/3/index.html","ded1b4eb1c20eb309a4ffd94942ed07b"],["/archives/page/4/index.html","2431b660ca897226989c3969d1033037"],["/archives/page/5/index.html","5f4159d7421fea1e7b06cb1cc9f59956"],["/baidu_verify_codeva-qQP2iZOMLX.html","869fb6de609b485d7ecadb73f31eab4e"],["/categories/Java/index.html","619885d25e552080cdf32ae34fcdc25c"],["/categories/Java/后端/index.html","6eef3404fc911d2692ebd544de0b1b0e"],["/categories/Java/基础/index.html","c8a6a9ba1c6b0ba818f5e754fdf93b4b"],["/categories/Java/基础/集合/index.html","9f4c69f667d7074d0cfdb12743939c9a"],["/categories/Python/index.html","523d46d582fe4fe58c986bdc31bb95ce"],["/categories/Python/编程环境/index.html","a577daa30997332abd912b7e1f843b6d"],["/categories/R语言/index.html","98f4b6b36add9bb297e1167165920933"],["/categories/R语言/编程环境/index.html","a9f1357858419af454489eafe36241f5"],["/categories/iPad/index.html","ea37ea6424747f6b2154e44f80e2ec12"],["/categories/index.html","3715ca1efae1614f4c6ad4988f4df041"],["/categories/中间件/index.html","493e12edbc0dfc5cc135f270fbd762d3"],["/categories/前端/Vue/index.html","c89ba7c3c479f90557f661156cc79c5b"],["/categories/前端/index.html","261396284e098937049659fd0eb9aa64"],["/categories/大数据开发/ElasticSearch/index.html","76d668956c865dcbd23384ef2f937bdb"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c44663119e6e63061bd9699692db8e78"],["/categories/大数据开发/HBase/index.html","36eaca835891d395b800fe15a3eea03a"],["/categories/大数据开发/HBase/学习笔记/index.html","e85d0d0904febacf045d6be496015ade"],["/categories/大数据开发/HBase/环境搭建/index.html","45d17caf6b303b666166dbeccd2577ad"],["/categories/大数据开发/Hadoop/index.html","e27286df0906ab9eb0c8290e7e8a2e75"],["/categories/大数据开发/Hadoop/技术/index.html","8573228aec5f6d98909fc7b28b5d05d8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","6ee0e1fec6906c10455f8d29df75b8b3"],["/categories/大数据开发/Redis/index.html","18e39f85e9736d83bc57e08513132352"],["/categories/大数据开发/Redis/技术/index.html","4b984124c059636d14c0693275d7f0fd"],["/categories/大数据开发/Redis/环境搭建/index.html","47affdedb56b0161435674795d3285e5"],["/categories/大数据开发/Spark/index.html","157370277b300522713fad3417fc7f08"],["/categories/大数据开发/Spark/环境搭建/index.html","d96218e64b3a24899aa61306c130b6f6"],["/categories/大数据开发/Zookeeper/index.html","929ed1c86dee1968e841f6e41f624a69"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1850efe500ccb0e5ffbfaaee84d705a0"],["/categories/大数据开发/index.html","18a42d89545840c362720d273f1920ce"],["/categories/学校课程/index.html","9ad939b54702fc6b21168d375acd8076"],["/categories/学校课程/计算机操作系统/index.html","a142eb72d837fbe2d75ce134b2601428"],["/categories/操作系统/Linux/index.html","ba59dc5324d133a1fb554bc0ccc84ef7"],["/categories/操作系统/Mac/index.html","690f6a6ba8e3e5d07c185589571d3eba"],["/categories/操作系统/Windows/index.html","c6582dffc6b3781eede089c57c228728"],["/categories/操作系统/index.html","e2dae74bd1933040663417f1a0a0c095"],["/categories/数学建模/index.html","3ac937f8034c79ea7584e7d70800956c"],["/categories/数学建模/latex/index.html","2edf63b05b90295279e78544f214cecd"],["/categories/数学建模/优化类/index.html","5144c7a64cbd6b9d2b0b8cd8198276ef"],["/categories/数学建模/优化类/现代优化算法/index.html","a6bc49bf473d5145d4e0d464a664b565"],["/categories/数学建模/优化类/规划类/index.html","4430eee10a29d3a596b8baa7e75dd025"],["/categories/数学建模/绘图/index.html","a16b97e9a3152c04ea7ce683f2fc75ab"],["/categories/数据库/MySQL/index.html","40e197f4c522eef2738326be7fb60333"],["/categories/数据库/index.html","a6134fce81e1efdd7f775ad886244645"],["/categories/数据结构和算法/index.html","224b84e386bd438e45d1f8441abd282f"],["/categories/数据结构和算法/page/2/index.html","9eb5f6df16a3ae9f4aace30179a38967"],["/categories/数据结构和算法/基本原理/bfs/index.html","4d137a6b0d64d14de4deeac961cf7ff7"],["/categories/数据结构和算法/基本原理/dfs/index.html","a5dbda194554a440efb7c28fbc79afb4"],["/categories/数据结构和算法/基本原理/index.html","3e0e516a03f78d35f022794fb95a51e9"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","74b4dfd9250fe38dfb6ff25b8cf8b1bf"],["/categories/数据结构和算法/基本原理/动态规划/index.html","524e3e03b42741ceb513c889ebc617ff"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","544a326ba3bbcb24474677496a0858ec"],["/categories/数据结构和算法/基本原理/图论/index.html","d90ca636799c1bf0835042237f1d90d1"],["/categories/数据结构和算法/基本原理/字符串/index.html","559bb5096ba722ca300f6b04d47be12f"],["/categories/数据结构和算法/基本原理/排序/index.html","2b722231bb77e66d41fa3d55deee0c46"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d4e4e38e28112a80bfe834c16e2a065e"],["/categories/数据结构和算法/基本原理/数论/index.html","87ce86530011cc6aa9bb6c2b5ad5ca2d"],["/categories/数据结构和算法/基本原理/树论/index.html","03b02bd62dfa8933e7071512346046f2"],["/categories/数据结构和算法/基本原理/链表/index.html","80abc9b2e1c9acf531f87517465e626d"],["/categories/数据结构和算法/算法题/index.html","63d20f21bea523224a85043378d760d5"],["/categories/数据结构和算法/算法题/二分查找/index.html","75f12aad4b8814e67f1b15b54e51668f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","3d5cf4a0a41f5d8b1d505d3a892bd2ec"],["/categories/数据结构和算法/算法题/动态规划/index.html","70c6bfae76e5b0bebce26ebfc6f4e0c0"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","52c3c9ede2b6b965dc84e4bddb6bd444"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","0cfba7936bfab030d7bf78ec5e26f45b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9e4a04cf1e2791ef0ba167def049c5ad"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c7f9dad3ba646319382a7c955dd7655b"],["/categories/数据结构和算法/算法题/数论/index.html","2f6f919881b452b9f85363a7c8861c08"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b29b6f93f5e216914cffc5bf307bde60"],["/categories/数据结构和算法/算法题/树论/index.html","1a1b2d51633a320b5ad2e9a03643d89f"],["/categories/杂七杂八/index.html","bdc828346abe0366480f27e45df4777c"],["/categories/杂七杂八/博客搭建/index.html","d229882d729fa06a1442664715e30e38"],["/categories/编程工具下载/index.html","dddfc3268dccbfc96543cf4272d5e0c4"],["/categories/编程环境/index.html","76e707510c41e5d1c7f0338659f9c38e"],["/categories/编程环境/大数据/index.html","e94a2e240b862d5e796606b328bc76d7"],["/categories/英语学习/index.html","f91528376605f4f74f73fdd1c393a399"],["/categories/英语学习/英语语法/index.html","0aeff79e801102029315fcad671c6d2f"],["/comments/index.html","e52b75eb8c856a82a101b5bdbb31b47d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","957e2900cfccd96ea728dd1905f5f38e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","7c640e47df727dafb2c036453dce448a"],["/movies/index.html","84ba35487f3083656b4a0ae0d153a2c6"],["/music/index.html","18035b18530131c75248e29292627357"],["/page/2/index.html","2cc9e962e7a2f619be26a9ce19158e82"],["/page/3/index.html","038e88b9e45c8ee0c03340e9fb472c8c"],["/page/4/index.html","71e8cd76de48ca3a99fa02cacb82daa1"],["/page/5/index.html","6390f817ad801116a832cef72a320a25"],["/page/6/index.html","3dffb12dd6c07b21b0e0294599963bd4"],["/page/7/index.html","3b3df897e58c927ac8fec17648fa928e"],["/posts/1021360842.html","eb4bd2c74001c50fddf1136a641564d1"],["/posts/1120620192.html","6eac9382a53c1d5a3c4ebedd2fba7c0b"],["/posts/1137707673.html","8bca787cf5bb023cc0fbd6ee87b0ce59"],["/posts/1141628095.html","50ffe5aeedcde352c5f8f021e970931c"],["/posts/1168613674.html","6c0e7201ba498d0db50f9316d5810e48"],["/posts/1219920510.html","5cfa010856d01c33ae74d4ecaabd0d94"],["/posts/1222166338.html","10514d8f1dfbd6873055cdc208a319ca"],["/posts/1259097482.html","23d82c00993e759a825235eb05904612"],["/posts/1271036369.html","a3884ce2435446c00efe9204059f87c6"],["/posts/1312847445.html","c4b4a20e2869cd60fcabab50b17bc9c1"],["/posts/135355774.html","7da54bc99586502fc8bd56fd48c5a2e9"],["/posts/1375344716.html","06f8f2694a0a549b561dce19cfd2edb5"],["/posts/1388991698.html","9059b1cf1d604255f047f99ce3fc76b1"],["/posts/1410315814.html","a106f38ed0f447a9dc2c696ec7d700b2"],["/posts/1452790229.html","dd2315d93fbd129021050ffaab72c631"],["/posts/1470079884.html","c10130b79ce3df251c1cb1a18ed03729"],["/posts/1470079885.html","c461b16e587438b431f6970e5f97bcc1"],["/posts/1470079886.html","89b16c54a813aecedad9659446874020"],["/posts/1470079887.html","a3189aa16d89a85086388f653811e8da"],["/posts/1498536549.html","7610a9bafab0de36cfd361ea42e4c694"],["/posts/1539568593.html","d4ca7e1e8e3e2eff185846a57e273fc1"],["/posts/1547067935.html","5da04b86a4da952a25aa2ccd683e54b6"],["/posts/1557866301.html","bb16648a7d3e276e556b6509bb49117a"],["/posts/1571776361.html","416ba72cd7ff764e8f5b758975c65a7c"],["/posts/1605124548.html","4af3b453cc21e16136fbc3d0615f368c"],["/posts/1633036852.html","5dca3358677dc1d7378584d33908cbd9"],["/posts/1667740714.html","70178f6744b78a734535d996ef16acd4"],["/posts/1674202625.html","b9fafa01e9802a7b223660645aed0d86"],["/posts/1765123828.html","11e00a808eadb04daf5be06de7151d3f"],["/posts/1767336200.html","f4e9328dacd4d7b103a15f9c42d8d905"],["/posts/1776114197.html","5dbf932736d53581284f699a9cd1b49d"],["/posts/1817748743.html","2f260acf642c381d8a25060021dae51f"],["/posts/1925125395.html","66a8450db13d8205f9f2202e04257675"],["/posts/1966191251.html","caf5f111201a68066963b3d1c361e1d5"],["/posts/1987617322.html","c3a7e1ff5213a791dc4c35f0eb1f98da"],["/posts/1999788039.html","0888e05256148f1a7dc82dff393e41e2"],["/posts/2007534187.html","aab31bb1d5549bd5aa6a1b9ef1cc47fa"],["/posts/2075104059.html","75e0fef5c8e4d7cb7f5a1fd7797821f5"],["/posts/2087796737.html","6272565f877b695920fd8daf585cd987"],["/posts/2106547339.html","bf6eafb4e33a4ad78084cbb2ee0e43f6"],["/posts/2207806286.html","599d4b8cf3caacdf001ea142bae7d7a9"],["/posts/2225903441.html","6166d8c08e82685864388b4f3ae3b0a6"],["/posts/2265610284.html","1e5830a36e8246ddd4f7d6b18ecb6033"],["/posts/2281352001.html","7545b3e62223be7fdd8f782790772364"],["/posts/2364755265.html","730f30a5c2a3ec9e92c8106b62920881"],["/posts/2414116852.html","1ba4d188d449c7a8b8131ae140539aad"],["/posts/2421785022.html","6cb2f1f304d1eb46e03ad095ec637006"],["/posts/2482902029.html","32fdf4d796390eb9ade5794c52ac450b"],["/posts/2495386210.html","9a9d3de0480d945bfb5d889a805fece7"],["/posts/2516528882.html","3f3bcf786f45fed547154ddd0be1943f"],["/posts/2522177458.html","82b6c34d5a6e16841fbfaa5b4fd77e6b"],["/posts/2526659543.html","f170340ea48bbb9f99c223cb55d9a0d8"],["/posts/2529807823.html","610cf465e32d8485cf4c8693bd0ecabc"],["/posts/2596601004.html","a887be4d23cb973efef0d7a7e30e8e37"],["/posts/2697614349.html","0cbfe80dc0a846cb811d091306c4a8db"],["/posts/2742438348.html","396716281f00b7ebdee663d1db23bbc9"],["/posts/2768249503.html","8d0d1887fc3012c9b055c4a334471cbb"],["/posts/2864584994.html","c83062f8fb702edd575a246f121bbde4"],["/posts/2888309600.html","364d3a2082e806f583ebe2cba4c231ea"],["/posts/2891591958.html","f9b9ec0f96e5306ff54efaacffb8e95b"],["/posts/2909934084.html","c903862f457bdbe80f7623c079080aa3"],["/posts/2920256992.html","a405148bebfe67cf8776e879ddb4aa1f"],["/posts/2959474469.html","1430583fff71bf49f8a08871fa8e49b4"],["/posts/3005926051.html","2bf91c2fe776ea3354c76cf6e554eb6b"],["/posts/309775400.html","3aca55bcb7ea749c4ae017575c6d47d1"],["/posts/3156194925.html","ceac4cc611dc63c2b95b51cf63d1ce2f"],["/posts/3169224211.html","f23a0d9fc330d6ddea72a685b29b27cb"],["/posts/3213899550.html","9c4d62549957a8f95e64eb2f5b7d2f60"],["/posts/3259212833.html","700cd9d763ea362270223e8287d8fbed"],["/posts/3265658309.html","a5cd199c9ec38684d4b79e232fbb6e32"],["/posts/3266130344.html","9f770cb2bf773421eddfab7c66babafc"],["/posts/3292663995.html","f723e8c32becf3811bac377d5d3fb211"],["/posts/3297135020.html","7c24f44e5b4814552ebf08cb9b8b82c2"],["/posts/3306641566.html","2556984d9b8b098fd51c6694e93a7229"],["/posts/3312011324.html","0ebdbd2b3ae31abe0a54dc181e22be8f"],["/posts/336911618.html","8a0fb0f37b0776570e02322d1b9a438a"],["/posts/3402121571.html","787d94b5258ffe454837760a990ef8bc"],["/posts/3405577485.html","aa6c0129c417b8045f9138d0616d5ebd"],["/posts/3498516849.html","5622b109aa73ee8e8ce0e003af446135"],["/posts/350679531.html","07b05ebae24de78c5e48513f66f51afb"],["/posts/3513711414.html","97fd423b707da912116e8bc10c7164da"],["/posts/3523095624.html","6f60b5acd1a38a6ba299fbde8f4423c2"],["/posts/3546711884.html","4085111631f75de92e3ea396a7f74396"],["/posts/362397694.html","006da6ba8a742d2b90ee5af3e0eb94a4"],["/posts/3731385230.html","4a4ee0e969cc466ebeb3f349b371cbd7"],["/posts/3772089482.html","bf3cd5827c1ce7158dae4a5a2b98b79c"],["/posts/386609427.html","f49d5f274a26b99639a72bf975364d57"],["/posts/4044235327.html","562a2d6dfde380c860cb903e33b30211"],["/posts/4115971639.html","7cb8b23c9bd0104c96cabecfd937035e"],["/posts/4130790367.html","25db3cb40e398ad887e0d21dec9fe1a8"],["/posts/4131986683.html","62b7f99f99c9cf3bd0dbb031164e3709"],["/posts/4177218757.html","33626311182d85c30c2244f398ff6546"],["/posts/4192183953.html","b714ac52062a7e0a6bc30101ce834211"],["/posts/4223662913.html","b7daf551443aaea28659ec8be209f7a4"],["/posts/4261103898.html","e3e8ddaa06b8dcc8233d84d8f7cfb180"],["/posts/4286605504.html","75e701aa57b5ce16ed1ce2f8be77d029"],["/posts/449089913.html","e67a09ad2d3bf211f5ac43756d47bad0"],["/posts/469711973.html","2132b284014905c481ff326ffd6f0b92"],["/posts/482495853.html","b193880ff2537daf4c6faecde74688e6"],["/posts/488247922.html","59ba8a209595d4702f2c5a70372739b4"],["/posts/517302816.html","e8a39aa9c5374494a5c0645652f1f676"],["/posts/570165348.html","569edba6427a463d9f050f1dab1959e2"],["/posts/595890772.html","07e595e52bf7618f69c0265c52bef3f7"],["/posts/67485572.html","0a5cdefecf51de7a2a1a3e7200404d3f"],["/posts/694347442.html","dfc6d97c4706b3460c88aaefab7547bd"],["/posts/707384687.html","594a55972223f697489f1bab55bf8824"],["/posts/71180092.html","9c6201abfb1d3d85983104cd1d33e452"],["/posts/716459272.html","5b74d81512b85c20787e7ee255519fdd"],["/posts/765481613.html","33688e29fbc071c4ab5518a1a958ab92"],["/posts/778231993.html","918632385a15569a1cf9644587ad0f0a"],["/posts/795397410.html","dc11a90092c91bcce5d830e441a9cd13"],["/posts/820223701.html","df3c81f8d4b46d7447d8127e28120e46"],["/posts/830372185.html","dcb756b11629185300d199730be19e98"],["/posts/88294277.html","8bfca46b40604191c99ded3d34b66698"],["/posts/939963535.html","4326f17e75ecf8ded1694b6da9e8f541"],["/posts/983786067.html","065df83b16365d0bf408259879769f9a"],["/sw-register.js","0ce45ea3abd93d303fcf3fdd6f313230"],["/tags/C/index.html","c94de2604e048146405954345c5c0d6d"],["/tags/C/page/2/index.html","e95185ff82f9e7be45681c522d3a5307"],["/tags/C/page/3/index.html","fd459164391504b0fa614b34fcc37a31"],["/tags/C/page/4/index.html","a30feb3d09424e3c76d4685b824fa89f"],["/tags/ETL/index.html","b3ec393c91505d3a897dbcb91d5fddfd"],["/tags/ElasticSearch/index.html","4bc5b8c163bbdffb04586420d25fbf78"],["/tags/GUI/index.html","8645d3b3612507c004213c18c7e207b4"],["/tags/HBase/index.html","c20ae3a2c3048ba08eeb7d4ca87f5160"],["/tags/Hadoop/index.html","45fb0b14f9b1e59f9f038b6d07edfce1"],["/tags/Hadoop/page/2/index.html","49e47d866188e0a4330a2d00c80530b8"],["/tags/Java/index.html","2e3359f43bfb18d09df95bd6d90bbcfd"],["/tags/Java后端/index.html","b4855fbc89c8a895738969c21dd7da23"],["/tags/Java后端/page/2/index.html","8a7495e30d3cc1541a15b6daeb0cdd5f"],["/tags/Java基础/index.html","50b8019f7a0386310d520a3895ffd820"],["/tags/Java基础/page/2/index.html","a0dd9ee5b059cafa55481504dce939c4"],["/tags/Kettle/index.html","b724806b512074bdf276106b0bf71f2f"],["/tags/Kibana/index.html","0c0482516654ac0bba22bf2632fd81dc"],["/tags/Linux/index.html","0d005b43caf2fb9a912e0ee94e7fe2d1"],["/tags/Linux/page/2/index.html","f7d6078ca80e8d075fd08c5edf8ea527"],["/tags/Linux/page/3/index.html","20406e347628ca389c31746a696ebe25"],["/tags/Mac/index.html","6d2d5ef1b29154546b94a85926f6cf40"],["/tags/Mac/page/2/index.html","f76b630b4e26885c0ad1cad38d8921c5"],["/tags/Maven/index.html","5dbeef6d3f07f59110bf0b624b9b1813"],["/tags/MySQL/index.html","cb3b92395452e8575a8c9579b5595e4f"],["/tags/Python/index.html","1de907fe2fc5eebe924191f3a5227258"],["/tags/Redis/index.html","02400028ba4658aadfc6f2c18d263c82"],["/tags/R语言/index.html","0cdfc2ecbbec8cc084703459e6d6e911"],["/tags/Spark/index.html","ee09d3d6c8dd758be1f8f81567f12521"],["/tags/Ubuntu/index.html","beefd1dd631e797dd394ee204f520de1"],["/tags/Vue/index.html","eeec8829a5fec9c29396796730b29abc"],["/tags/Windows/index.html","44436c02de1ee513379e9f8aa8930dea"],["/tags/ZooKeeper/index.html","8770f617ef2fcda28410830df20ce7f3"],["/tags/bfs/index.html","1cfb76d2477a88e79dc46f134fd434a1"],["/tags/dfs/index.html","2e3e7b61b7268cf673ed203f5f7dee16"],["/tags/folium/index.html","3394a5b4d76369cef17ffb57926561fa"],["/tags/git/index.html","c721acab612cce853ac7478e6bde7181"],["/tags/iPad找电子书/index.html","7ccd139dc0a024a37b9ba7f7bd9cb0c8"],["/tags/index.html","02364097f1a1bfc7558fd38c1c2e2dd1"],["/tags/latex/index.html","2dc3d5b107787f60705035cbbc8fdaa1"],["/tags/中间件/index.html","7d69499d373b1d2331cf8d807e2c9fd3"],["/tags/二分查找/index.html","2a91b4d8268a99910963781587f7d43c"],["/tags/优化类/index.html","5b18f95f47c83dd10e8dab84df770c4f"],["/tags/前端/index.html","21b8879a83aa4bdfe0fd03bad3093a19"],["/tags/前缀和与差分/index.html","01fa8ccf198b133c3895e02d00d783b9"],["/tags/动态规划/index.html","7565d4c2e5f2a4d0007c399f954c6f7b"],["/tags/动态规划/page/2/index.html","3bcb77b87959dfe2b436379b63b087e9"],["/tags/博客搭建/index.html","f50f65983d42aec7516a90a8e5e7e239"],["/tags/图论/index.html","326e8d489ec30b42ed90efa5f14fd38f"],["/tags/大数据/index.html","6b3fd0426e01c9f1d0cba75b8393d0f7"],["/tags/大数据/page/2/index.html","8488ea88abf857f4b74aef8f63313b57"],["/tags/排序/index.html","44216f63a08cb8e38a6c4fe6c83dd96e"],["/tags/操作系统/index.html","2de83a42b8b84b148a82c4ebf7a822de"],["/tags/数学建模/index.html","817a474f6a97c6aa50111547bfa213d7"],["/tags/数据库/index.html","2272c0fe7af7a537486c5c1dc806a696"],["/tags/数据结构和算法/index.html","0fc57e492469000827b91ca50afe99be"],["/tags/数据结构和算法/page/2/index.html","c15c51d05e766aaa8dc4310136f0e9bf"],["/tags/数据结构和算法/page/3/index.html","8a893c91433883b5461cbd0ce2fa8c04"],["/tags/数据结构和算法/page/4/index.html","b13ed3910bcd285e46b4382dfbbcbe72"],["/tags/数据结构和算法/page/5/index.html","e3277a80d9b6240d8e38968fd53304ab"],["/tags/数组和字符串/index.html","9023f3f4786f6d16852d459510621435"],["/tags/数论/index.html","6c978436b093cbb180777d7d3f300618"],["/tags/枚举类/index.html","14bbc0527fd36f64258c9c04693807cd"],["/tags/栈和队列/index.html","594b337a56254a895064696a33a743fd"],["/tags/树论/index.html","5c1b53983ed88935cccbf50dd6ed7a17"],["/tags/测试/index.html","37fd0434fab3679fbd4bcc16cbdacfa5"],["/tags/环境/index.html","cf184ca37fd9aa3a2d98aaf9fd4abc8e"],["/tags/环境变量/index.html","cd537d942d0ee9aba1e4c4e1813baba5"],["/tags/绘图/index.html","8a617c8ff122dd0097df9a023bb31957"],["/tags/编程工具/index.html","70a09e69786c11202e6e103a953a8e05"],["/tags/编程环境/index.html","7faf26372ace2b74aa3d767388f51d22"],["/tags/网络编程/index.html","1ed2d66cacf8d743bdc5d1bf56dc1632"],["/tags/英语语法/index.html","7142403cc2e20546e9c6ae7ac0485893"],["/tags/计算机操作系统/index.html","de7d9166da933c64839b2b6e89148633"],["/tags/论文/index.html","2210b847725ee4b1e6797785c327c7fd"],["/tags/资源下载/index.html","8c7651b1055fafd63cf8f729035d3d4b"],["/tags/链表/index.html","5c5a9117166669be4dd78c1f1e6f121c"],["/tags/集合/index.html","aaeef36bfdf35ba15468e0c4f5bb4d53"],["/tags/集群/index.html","823a80ca8fa5206d3886bbd39196c484"]];
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
