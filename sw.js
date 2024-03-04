/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ed0f977d6fc494868c6b8c6a406ef5fa"],["/about/index.html","09cdc5d7570ce7ea41b825d130714a6d"],["/archives/2023/01/index.html","330ca393e3bfefb53739228cd9e8590c"],["/archives/2023/02/index.html","e405d39c58fc62e249d49c24805e0a85"],["/archives/2023/02/page/2/index.html","683137d2d38d3fe14c975dda3fa09bd3"],["/archives/2023/02/page/3/index.html","c0f700ad1f7ca935a9ee6c22f2a4d19f"],["/archives/2023/03/index.html","78d4cd1505f7fc36eda7bdc83754d81c"],["/archives/2023/05/index.html","3c958d8a0235b8ef51aaf5ba43bfb5e0"],["/archives/2023/06/index.html","fde5358ae3e8f48f3c38007c378954c5"],["/archives/2023/09/index.html","bed8f9ad5dc993fd0c89cee10b14516e"],["/archives/2023/11/index.html","8bab954d8a70753d95c2c23df36a6d67"],["/archives/2023/12/index.html","ae4caac3cab33e9dd7d003beb414cbf4"],["/archives/2023/index.html","9d15a44374940766dbf678a5ee25bc17"],["/archives/2023/page/2/index.html","4e96d659a481de6d611793eab8d941ed"],["/archives/2023/page/3/index.html","359d94b6ab31524ec279837edd484e8d"],["/archives/2023/page/4/index.html","91163c29bf33691603fd46e0d2be691d"],["/archives/2023/page/5/index.html","10fcc8077f506594f95f5f058a509d7b"],["/archives/2024/02/index.html","225077eef85f3bec1779f450fcd55e0e"],["/archives/2024/03/index.html","56c5a17e2da545346dc946d63e3a870b"],["/archives/2024/index.html","0f26b695ff0e415cd3396d4064e5a5c7"],["/archives/index.html","673df406bb0a3fb5ad7bf34eec89eb07"],["/archives/page/2/index.html","b54416a502c57a3dd49f6c38e4c85f94"],["/archives/page/3/index.html","c9ada973279d85e94a9295f2f3f90a7f"],["/archives/page/4/index.html","8f3b18e324ff5a318f54a0988ff53b8d"],["/archives/page/5/index.html","6658155ad2112de06606e127788ac2af"],["/baidu_verify_codeva-qQP2iZOMLX.html","1c440dac96d00b240e6023e217631c3f"],["/categories/Java/index.html","6f040b930165b438c233578622720769"],["/categories/Java/后端/index.html","b40e026a2c2b34dbf22f789d9555f531"],["/categories/Java/基础/index.html","dd958183ba38081926af9363c2e018f2"],["/categories/Java/基础/集合/index.html","695196b57dc8b212221ac4a25813e7af"],["/categories/Python/index.html","04286d2edf237ab01618f1091704d843"],["/categories/Python/编程环境/index.html","96c3b6d97bb5e8e05e437cd7cffe0eda"],["/categories/R语言/index.html","a3b9b0ff6728018012008c63e7d08b1f"],["/categories/R语言/编程环境/index.html","fd10177d19634d116f8a438df4364f8e"],["/categories/iPad/index.html","66b3f785ac148348592cfc9f735068a4"],["/categories/index.html","fd87571d9da9a1c92789f83efec2e82f"],["/categories/中间件/index.html","e07acce0a96976030a772cf0dd3d11fa"],["/categories/前端/Vue/index.html","deccad05b2f9b3ebdda74bf4820f8b23"],["/categories/前端/index.html","c23505a75e3d97fc39766c32c4e58f9e"],["/categories/大数据开发/ElasticSearch/index.html","9e9dc5453d68442db1a091f29d985023"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","6bdc551274f01fe396e21ce533e73207"],["/categories/大数据开发/HBase/index.html","fd490622fe08ae8bb4ab47f229928f90"],["/categories/大数据开发/HBase/学习笔记/index.html","09196cffa819fb92628a399e35199595"],["/categories/大数据开发/HBase/环境搭建/index.html","bf0c14d0e0545d7eb608583860dc1014"],["/categories/大数据开发/Hadoop/index.html","08884935cd47880885b44ba164edb1f2"],["/categories/大数据开发/Hadoop/技术/index.html","33b8d04b9160cc2ee0f2b7e9b14dfd04"],["/categories/大数据开发/Hadoop/环境搭建/index.html","66508d914d138c9c658dc4d7f7aba1e7"],["/categories/大数据开发/Redis/index.html","d9c534e76b20293ab3e63c0086bf7dc3"],["/categories/大数据开发/Redis/技术/index.html","0fa2758b845325c572e095d3a669facf"],["/categories/大数据开发/Redis/环境搭建/index.html","0afb01e3d1d50b45d6bd4e98722ec1df"],["/categories/大数据开发/Spark/index.html","7142fba91cf83bb4b08dd099c135fb46"],["/categories/大数据开发/Spark/环境搭建/index.html","f1d404d1716229de803394f8e596c68d"],["/categories/大数据开发/Zookeeper/index.html","32f393f789b3c95d763859d4920ee907"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","0627b9a5426c91c94e5a8e9a1ab9b920"],["/categories/大数据开发/index.html","02127942bee06f8352630a70450ec6c9"],["/categories/学校课程/index.html","9e0cb80415bb884ced39767148166766"],["/categories/学校课程/计算机操作系统/index.html","7ce7507610a228fc0d9f266631079514"],["/categories/操作系统/Linux/index.html","5dea5638b512557efbd188dea6b8e86b"],["/categories/操作系统/Mac/index.html","2c803c0dbb0e6d8a9efb85c68de451ac"],["/categories/操作系统/Windows/index.html","3c399573847417b1c9f8c6074176fa47"],["/categories/操作系统/index.html","5a43bfe822800287bf71c42adee51364"],["/categories/数学建模/index.html","3b99a9704189c1f895689ce1ed0d1169"],["/categories/数学建模/latex/index.html","e9ac0d56679bde5d273b99091f6a8f96"],["/categories/数学建模/优化类/index.html","249fa6a380c9831be5813bde141f3375"],["/categories/数学建模/优化类/现代优化算法/index.html","df9faa32a6c97a8bc846ccf1d3d181de"],["/categories/数学建模/优化类/规划类/index.html","c0763e254c10663167cccf81d07a3068"],["/categories/数学建模/绘图/index.html","4393c256851f4a4317498c9ba24c5a69"],["/categories/数据库/MySQL/index.html","fb429802ade773fe6e322c31ae81f7b9"],["/categories/数据库/index.html","a6612c987fb3012c14d07dc1010c8a64"],["/categories/数据结构和算法/index.html","8136d410fcb2bcc59bfba6ca6fd4e03f"],["/categories/数据结构和算法/page/2/index.html","eb51cb8b2d5fad52de25612761e5a5e1"],["/categories/数据结构和算法/基本原理/index.html","ad1acd3c72e858424a4f9276f4e822f9"],["/categories/数据结构和算法/基本原理/page/2/index.html","a9919de6771e8d793c104f5ebaec1e42"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","5e80446b536c4bac78844ab4e0aac2b5"],["/categories/数据结构和算法/基本原理/动态规划/index.html","fb8e569e6d0ca9421ece4afff1b3b974"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","2d202bde691ad76209429bc92889ac89"],["/categories/数据结构和算法/基本原理/图论/index.html","617a803c2ef0777aa76bcb54938144de"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","8def2638a1cf56e71ffc2d5a9b614c81"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","516afbfed9c225ac8d95c543c454b002"],["/categories/数据结构和算法/基本原理/字符串/index.html","aafbfe358bfd8f53daa241647860372d"],["/categories/数据结构和算法/基本原理/排序/index.html","8d15217ba99fcf839fe1d912e0636001"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","1a5de9e67b4554cca15abd807d75950f"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","408e3205d50da3651d036884074d0db6"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","bb64a8b4732d41b513c1b6fc0e5e22a5"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","603dcc2090db242cb08c52717a7a1dc1"],["/categories/数据结构和算法/基本原理/链表/index.html","9b99f73db8452f9d000dc1104c1fefb9"],["/categories/数据结构和算法/基本原理/高精度/index.html","26678301b7ac32265c53457d3b9162a6"],["/categories/数据结构和算法/算法题/index.html","c72aafdfb488116f17947bf95b9c6f59"],["/categories/数据结构和算法/算法题/二分查找/index.html","818dc7e4382d1ddab76b4e32028eba51"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","d41cdd1de2e485b181ba3c908c3930fc"],["/categories/数据结构和算法/算法题/动态规划/index.html","00aecd9c327e90db78708ae47e10252c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","b1d28a12234c6222712ceda2e8ccf5e1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","d71df10a76168f2fc3fe47e1ae5ad049"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","7e27a21852cd5a6874d10dd82f37e88d"],["/categories/数据结构和算法/算法题/图论/index.html","c2950ef01bb962490dc8627caecc78a9"],["/categories/数据结构和算法/算法题/图论/树论/index.html","b7db996f73fe03b234fdd8af94777495"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","db1a55471e240dee99944e10061a9379"],["/categories/数据结构和算法/算法题/数论/index.html","6d53d8ce3fcdafca225c8929914cd8de"],["/categories/数据结构和算法/算法题/栈和队列/index.html","12840aaf73565a7b54f7456e902f4971"],["/categories/数据结构和算法/算法题/高精度/index.html","3b4f71eb93dd7b63277a2368693b8f90"],["/categories/杂七杂八/index.html","84b246b6f11eac4376b9bc2e587d061a"],["/categories/杂七杂八/博客搭建/index.html","c56061bfe97e8b5eeeab0a9a912d9b78"],["/categories/编程工具下载/index.html","4f5c7ab165356e940708dd45e5c0409e"],["/categories/编程环境/index.html","7bb5b40eb0d4b3239ef5187ce50faf76"],["/categories/编程环境/大数据/index.html","44c03a9ae559e127dc288a712c5d10c6"],["/categories/英语学习/index.html","091d49dd9e51f0ca5d933da200b9c036"],["/categories/英语学习/英语语法/index.html","32fc6c58aba64bc18db743fc05203659"],["/comments/index.html","52436a3358b9066617bbf3e269c44f55"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ccfb9e07f413e5441204d0811fafb922"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","594400a85370b77f8e862e478c212316"],["/movies/index.html","06239430ffd307a60c64f1204d1f3246"],["/music/index.html","20371487b196e46c3e0424f03e6aa72c"],["/page/2/index.html","ae2cb85d10105009c449a5fe8f4d9781"],["/page/3/index.html","8a77392d17bd761c8962d14e44b50c27"],["/page/4/index.html","81b2d52710be0b77cf99fd4cca5919e5"],["/page/5/index.html","ec7b639f3fa051891eef6380fbb17fdb"],["/page/6/index.html","80d8576d2dd7f95516c5549d1a0368e4"],["/page/7/index.html","4a9119cb3b0bd19208e8fd2e34358ea7"],["/posts/1021360842.html","2292bbfd6d238cf4e99ca65a38490f5b"],["/posts/1120620192.html","52b065d7ad34707a8cc37e5c3bbb8369"],["/posts/1137707673.html","5ff38b752df269ffac472cb50be1697a"],["/posts/1141628095.html","64981d99e36ee75cfeef2eb1d72ec05f"],["/posts/1168613674.html","423815545222b2604fb2e64d57141c19"],["/posts/1219920510.html","479b9f3028fb319ae6e1e85cade3c2d1"],["/posts/1222166338.html","1365e2f072bf667fc082a73b48aef23d"],["/posts/1259097482.html","4abfa876c2a008eecb61a8e70ba20582"],["/posts/1271036369.html","801bb1e12c28b4cdcb0a7dac392eaefd"],["/posts/1312847445.html","9a85875110599c40ebc8312523cc5569"],["/posts/135355774.html","6aa700860ac6a0da6fe0c34955c02228"],["/posts/1375344716.html","5cc327075f81916b1271d9389908cf60"],["/posts/1388991698.html","fe8877b2cdd7bb40a93ff6b82927b6fb"],["/posts/1410315814.html","80872179ff74828ad84aa692616e433c"],["/posts/1452790229.html","27e77dd9e24f0a2022071e82441b1a7b"],["/posts/1470079884.html","e9cf24e02a192b67c45f3b3804306abd"],["/posts/1470079885.html","19502d9a660eb55cbe3e4354939999d5"],["/posts/1470079886.html","0acfe5a9af99f75a94ba394116d8b3bb"],["/posts/1470079887.html","f6b7f7630d3fb55db1c206930fa8449b"],["/posts/1498536549.html","101a70df170e890ac689a67796d188cd"],["/posts/1539568593.html","8d50ddde07a65110e46210f31d56c3e6"],["/posts/1547067935.html","fe1dad97fc83173047f4652870f5295d"],["/posts/1557866301.html","b7638ffd5f9e08f9060e066ecc28e37f"],["/posts/1571776361.html","ba623d2c048fb07f0ecb3a8ed0d121b9"],["/posts/1605124548.html","7ce09535bf7c628d4821c36b02b9981c"],["/posts/1633036852.html","3f3322d297f3b9cf26268ace16f1714a"],["/posts/1667740714.html","7cd9b2b68ed3b0a65187f859f06123cb"],["/posts/1674202625.html","85e60853d99519d931287fdf87b7781e"],["/posts/1765123828.html","32cd3c157fed988bf181c5d8a6f4a061"],["/posts/1767336200.html","acf318943fb86adfc5a1eaca0e9c435d"],["/posts/1776114197.html","a3942f8fd623a586328d079974c10deb"],["/posts/1817748743.html","05f656556a74f35723ea52df8d258c8b"],["/posts/1925125395.html","90c79202ff5660d2488137d7c70e661c"],["/posts/1966191251.html","467a904ca9e40b7e0690ba0219935601"],["/posts/1987617322.html","2b59cd9e32aa109dce27f081439695e2"],["/posts/1999788039.html","c5896371e66666025fac921a0c7cc4c3"],["/posts/2007534187.html","00679dc6b5bfc142b1763ec3d0b58bb8"],["/posts/2075104059.html","7e698403b9db88fe87e586274eeb7faa"],["/posts/2087796737.html","a95e8579761f1ca825f4ad6e3b2e41d1"],["/posts/2106547339.html","972a7f520fc49a3f657154d837e84d19"],["/posts/2207806286.html","7986d77c5c8d25110d6bf68145dae533"],["/posts/2225903441.html","b37c142a2aee6e9a7af0ae0f2e045aeb"],["/posts/2265610284.html","dc77a6dec3ac11fc27ccfd36d919462f"],["/posts/2281352001.html","64c5000709dea96ed3e4142339f77870"],["/posts/2364755265.html","63eba444455fad81c95a4f44ed13ee27"],["/posts/2414116852.html","25ab19f9d15cd504c1ba34143ed31137"],["/posts/2421785022.html","4b0cbf5b5a60ad4f22592715c17030e2"],["/posts/2445543724.html","9b1e95d32e949df2e4a3b772953baad0"],["/posts/2482902029.html","9212cf8613821325d3815aff300a7401"],["/posts/2495386210.html","13633720bbcb6b15be3c8a75f02aa8ac"],["/posts/2516528882.html","1cb772231b2c59a6f8308fa0f26ca760"],["/posts/2522177458.html","a06d96d6838dd5d3ee9bb9d0c7c654b4"],["/posts/2526659543.html","fd80c6ce50ccfcb2c1821dfadb62f6dc"],["/posts/2529807823.html","1bbf715fa86539531606c47939e7449f"],["/posts/2592249117.html","a7c267c1b75788ed793dcad9cf6ec4ec"],["/posts/2596601004.html","5422475902db7bd43cd3b97312c47dcd"],["/posts/2697614349.html","31f2b6becec326fbc80915ccff40427d"],["/posts/2742438348.html","ba0e1851ddc907e09f509f9d431c5489"],["/posts/2768249503.html","0c259a7104ab56e955bdc85d6e7ce3bc"],["/posts/2864584994.html","48c1a6c68d5d1c4967bb195a65229a20"],["/posts/28838462.html","67224964812ad82686552bfcdd087096"],["/posts/2888309600.html","227ff9092ac64ff4478fd840b1ffb741"],["/posts/2891591958.html","2eba4af7db405a08375fd6b18c8aef93"],["/posts/2909934084.html","a60576d3cf28cad46b7d5dd9af01d2cf"],["/posts/2920256992.html","ee48e813eb44dfd7613b671a7b31d624"],["/posts/2959474469.html","2896ea6fa4c61fa9baac32d68e2a58fe"],["/posts/3005926051.html","f63dd384528c7cd589e1c0b38b92568a"],["/posts/309775400.html","9d2a37a75d9f98b468632b6386657918"],["/posts/3156194925.html","c9a4eb9eed4fbfcda7fececb56c03411"],["/posts/3169224211.html","86bb9bf73a2aff682c8f775466127817"],["/posts/3183912587.html","10f721de6a3db0e6b75d73d9e710ed35"],["/posts/3213899550.html","a69d60de116759be96089d777bf19c41"],["/posts/3259212833.html","9071f71bf5e16269d9cafa4716ac63b9"],["/posts/3265658309.html","01fd654e2fe0e14301b7fe184834b383"],["/posts/3266130344.html","9c0f87181a87066adf99ab851ee30d5c"],["/posts/3292663995.html","bddcfbf55d778ffbbc616c1a3f779d83"],["/posts/3297135020.html","c96bfef00ae80255a4640964564ccfa3"],["/posts/3306641566.html","9705440d8da3bee45076b8eeced8204a"],["/posts/3312011324.html","086c168a844540c119685b666b0a314b"],["/posts/336911618.html","4528174e7d04e2be4134c51725d4fcb2"],["/posts/3402121571.html","5cae97f0cf4a63fa5b12c32a05bd6e6a"],["/posts/3405577485.html","795aab035951db9171c095479fb17c31"],["/posts/3413737268.html","39bfab30ee02544d174ae3c681071e4e"],["/posts/3498516849.html","ea22ffbe47590b3f0530a4e5336fecff"],["/posts/350679531.html","4b160ae25be4bccfb6a72e3e08cb5aa2"],["/posts/3513711414.html","d4f628c212cd60b690c0204c7e0fb98b"],["/posts/3523095624.html","02543cc81ebf703d78deec3d3531abe9"],["/posts/3546711884.html","5a96935528c111b706a98fa0f9ff1017"],["/posts/362397694.html","5440ee455524d259381bb432a81e32f0"],["/posts/3731385230.html","6113d71a707a24dde5bd7f3cdbb84dab"],["/posts/3772089482.html","b239068e57121a6f3d6064c818ed2d30"],["/posts/386609427.html","32ac90b26c5264797768d251a51e2d15"],["/posts/4044235327.html","481a61fbca5d464a510c3f3707b5eb23"],["/posts/4098221856.html","af3f6bf2991504aacfaed3f93c8eff85"],["/posts/4115971639.html","a8014fe0509e8e91b895271554deb304"],["/posts/4130790367.html","d979d7ffc28527b03a49ef98915fcc52"],["/posts/4131986683.html","a277a4abaa1de052ec37b76b70754a44"],["/posts/4177218757.html","745a044009a789e8d9d9fb2c9a2a630e"],["/posts/4192183953.html","a8fde8e0984513d9a26b7812618cf873"],["/posts/4223662913.html","54112594d090f5c23d20e7ccffe598e2"],["/posts/4261103898.html","4048922edb7d0efd4ebbc928800e694b"],["/posts/4286605504.html","48f42877ace9ed8b4ad007b33abf393a"],["/posts/449089913.html","cfb6878a776083b2d726afa479ac8e82"],["/posts/469277133.html","22ae5f420f619a72cc697ffcf2933671"],["/posts/469711973.html","292b0dda9587eecf7c49861316c51895"],["/posts/482495853.html","cb247aa866fb15bbaa4eea6848d8ce89"],["/posts/488247922.html","b90f0156d5ff5aa1a871918daf48c797"],["/posts/517302816.html","78486df2999da621c108ce88122a57d1"],["/posts/570165348.html","feff078a8b9b445c3c4f833ea5bc49ef"],["/posts/595890772.html","51cacd96feb7a5557fe29c34adf9a7aa"],["/posts/67485572.html","aeb433aaf8e6d55f282ca48a8a7bdbaf"],["/posts/694347442.html","922565deefc916429d432dc4dcce2893"],["/posts/707384687.html","fccf989553648ba579aeece86850aa17"],["/posts/71180092.html","9d1af47e202ccfb2e7b639ee7bdb6d66"],["/posts/716459272.html","340a401c9c4b8c63571b84a216d20871"],["/posts/765481613.html","c1b225ca07079f7e94a1864014b6e06c"],["/posts/778231993.html","f4a153d03572257c9b246b22608abc75"],["/posts/795397410.html","6691dcb76e15ca7998432d31180f125b"],["/posts/820223701.html","d6dd983f62c83798c816993d23b2d1ef"],["/posts/830372185.html","05bf64ff068cf9338163cff7eb1f7af4"],["/posts/88294277.html","047697809dce2cce2bb84701b7f3a31e"],["/posts/939963535.html","e8dcf72afbe7b2119a6af2270406132b"],["/posts/983786067.html","6738ab08401207653350c8d71b02f8d3"],["/posts/999620786.html","f6cae18d82bdc04207ce17c0d8312463"],["/sw-register.js","9d709f3226c34db2c9d2d34401786550"],["/tags/C/index.html","ad09e46e126c3d5aff87002461ab5092"],["/tags/C/page/2/index.html","8f9be359412c313e7354f69db47f23ef"],["/tags/C/page/3/index.html","d7c0c9f0f8f81ad7c183f14532953afe"],["/tags/C/page/4/index.html","b31469818e3a7e61ed1e59c9914182b7"],["/tags/ETL/index.html","4df93e4ec160a5a88bca646807cf531d"],["/tags/ElasticSearch/index.html","b3d81b200427b7598e2e2806238a75ab"],["/tags/GUI/index.html","d26cb30f80f23f81886c29277df92a09"],["/tags/HBase/index.html","ba3a43ad06753064119902eb5c1c39a2"],["/tags/Hadoop/index.html","deab659a1be737ec461a9bd3020c1226"],["/tags/Hadoop/page/2/index.html","0176157d8dd06ac19431a480867268fe"],["/tags/Java/index.html","7a6b724e80ec2589a25b038a318eafb1"],["/tags/Java/page/2/index.html","40c0ec7955459ab12176176081468cb9"],["/tags/Java/page/3/index.html","8b7a0763b84c48b6235fe97f4b0198d6"],["/tags/Java后端/index.html","cc2c05b0f7d79fad6ab53689b343b922"],["/tags/Java后端/page/2/index.html","08285cc514166b22df16caf9950ae4d2"],["/tags/Kettle/index.html","1d46ab72c5edd1180c301e8cfeb1a6fc"],["/tags/Kibana/index.html","1f574ed9ab07e8c447cff8459e5d54ae"],["/tags/Linux/index.html","ed728d1379bad0ad4758aadca5922e2a"],["/tags/Linux/page/2/index.html","54e9a69649a27b82773f30a8283c7181"],["/tags/Linux/page/3/index.html","2ae2be6f1ca39ebcc0097a2d769744d8"],["/tags/Mac/index.html","9925e2406cc33689e144fb8dc86a71e4"],["/tags/Mac/page/2/index.html","291f26041d7410e9aaec334c02a7485e"],["/tags/Maven/index.html","d53d42151418d321ed6a88945d1d0ab1"],["/tags/MySQL/index.html","efde9ba053f10c69620d1f0eeeb014fe"],["/tags/Python/index.html","4621232610bcdc8d8caca57debb48632"],["/tags/Redis/index.html","e8632141a3a73187f4eb8cec01eea7dd"],["/tags/R语言/index.html","e6b0fc612622113b4b49040027329105"],["/tags/Spark/index.html","d9c52eb0cc3d2a0ad5b2a5d5b78a98f2"],["/tags/Ubuntu/index.html","16185d71089fd94f63e4dafa4b9db498"],["/tags/Vue/index.html","2aece8946f5d4c58a63ce4f7f76c248a"],["/tags/Windows/index.html","c5e9ddf5ce6888cb64158cf6fbe8a945"],["/tags/ZooKeeper/index.html","10558ae9de29f56a1e72b1e41564da68"],["/tags/bfs/index.html","c5e10c04c07172412dcae87d58930f42"],["/tags/dfs/index.html","cacc40def85f57a3a913799d52246fde"],["/tags/folium/index.html","360294f351dfd7d09f2da387e67465c0"],["/tags/git/index.html","74d4d25c65758750ac7623af07fab6f8"],["/tags/iPad找电子书/index.html","c0b0752b59881d8f9d06fa044c1a879b"],["/tags/index.html","d38b92f42b361eae22e4f50211e72545"],["/tags/latex/index.html","28751ad0bcfe01cb5f07d520cb6caa3f"],["/tags/中间件/index.html","821909bbf1588154cc9eb4bc8208a4e9"],["/tags/二分查找/index.html","7cb7d702c0a1c666d7e53acc2153ca6e"],["/tags/优化类/index.html","8513b83599d4e200e1e42d867d0d8a0e"],["/tags/前端/index.html","b03846383ef7a9f2e51aba593d959980"],["/tags/前缀和与差分/index.html","c83949b74ab427fad9f811bfa417b888"],["/tags/动态规划/index.html","bc661fa7d8e2cb723b130dbaf061bff6"],["/tags/动态规划/page/2/index.html","1bff51364144c3415522bcd73d77980d"],["/tags/博客搭建/index.html","a7ac5aa6acbb983aa26526aa1bb203f7"],["/tags/图论/index.html","57aa0210b23ff07701f04dc8abf95e56"],["/tags/图论/page/2/index.html","9949a38f5791c99de7fafbf6146f2261"],["/tags/大数据/index.html","e308bba4266bb96d4c7e62537906bf15"],["/tags/大数据/page/2/index.html","be39e675107a376af18f944ec1b69290"],["/tags/宽度优先搜索算法/index.html","74ab914d40e41e33dbbf17ce71644349"],["/tags/排序/index.html","047120e98782e4df1a4dcde980d989d1"],["/tags/操作系统/index.html","d3abf0e8928e7b13965fe166ef6070c5"],["/tags/数学建模/index.html","bb23cdee35d77610418502ee73e77ccc"],["/tags/数据库/index.html","94ed16d2c30000ee15607392cc49f4b6"],["/tags/数据结构和算法/index.html","44356be8f17d1b10fd9d8f04f900ea5b"],["/tags/数据结构和算法/page/2/index.html","92d5299f17bcfb8302525aadce933a0a"],["/tags/数据结构和算法/page/3/index.html","b492b24ee7deb5ca81c412f022725054"],["/tags/数据结构和算法/page/4/index.html","f799853ef470834929d83ee4679883fd"],["/tags/数据结构和算法/page/5/index.html","cd1d49cc54e23e60aeaf267dc3fb798f"],["/tags/数据结构和算法/page/6/index.html","85a8e9f763a6d275df85d758452f4c7f"],["/tags/数组和字符串/index.html","31107a0a8a5f11f52fc736317a7e6eed"],["/tags/数论/index.html","51be08da98bb8a623ae59190127c5f90"],["/tags/枚举类/index.html","90a6cee24f23f622d03a442511767990"],["/tags/栈和队列/index.html","48b9ff44bb92a557f3d866f42728e3f8"],["/tags/树论/index.html","e9d03c93756b96b071a3fe0b72e129f4"],["/tags/测试/index.html","f1e985fe8b6547a97093b8d6fa4e307e"],["/tags/深度优先搜索算法/index.html","4ec41db46e16b4cf72483a93812cfb12"],["/tags/环境/index.html","e31e0316e4bd74363975a39fda782642"],["/tags/环境变量/index.html","ea4c7dd03aaaa84fe52497b38e8316cf"],["/tags/绘图/index.html","1dde72ea54fc43d7c975d00d5558ebaa"],["/tags/编程工具/index.html","d29bf43c6673b0d3c2e0d70a0f33b769"],["/tags/编程环境/index.html","35f159971c7b405fdd06a1cbc7052516"],["/tags/网络编程/index.html","9f83d7af5a3422cb166ed8e4983602d9"],["/tags/英语语法/index.html","6575a13b8c0fcc4b0c21941c9c7b7f14"],["/tags/计算机操作系统/index.html","5c682ccb4e8e21e8bbe2bd60849aef95"],["/tags/论文/index.html","641738756fa3390b20f2eac9e139c9b2"],["/tags/资源下载/index.html","442f6823cfc2d3ecd8c748ec73c644d1"],["/tags/链表/index.html","fec237ac8659242650ada20c09ae3401"],["/tags/集合/index.html","662f844ea79272f7513edfae934beee6"],["/tags/集群/index.html","a8296252e7a1c7932594a832804c40c2"],["/tags/高精度/index.html","133acf447ce19e7d5201e1c59f7626b9"]];
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
