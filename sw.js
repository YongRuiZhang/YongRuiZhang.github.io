/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","6102057581a7307ea8c6a9012b89f5bd"],["/about/index.html","afcf9ccd327fcb366795e262997fc139"],["/archives/2023/01/index.html","427a081a32e5adb6d850e89235b5005c"],["/archives/2023/02/index.html","f2e768bc60fe900b0fc9fd33c56468a3"],["/archives/2023/02/page/2/index.html","cc3a8939405eb03dca091891947775c7"],["/archives/2023/03/index.html","f6ea60e655df5a6c798c827b317285e6"],["/archives/2023/05/index.html","6f263e2d7afd84d942590d5994d9a257"],["/archives/2023/06/index.html","d16321aef10a3d8e38d9c71bb89076ae"],["/archives/2023/09/index.html","35659eec901aa4d2e555037c3cdb6649"],["/archives/2023/11/index.html","61eb469426b6fdcadd0b2458bb362155"],["/archives/2023/12/index.html","d0adaa862287553dd59b9819e0c71400"],["/archives/2023/index.html","913bed1128ace36f53e5cd6fcb84bdba"],["/archives/2023/page/2/index.html","d4981510b90d83402489d48d36cafd8d"],["/archives/2023/page/3/index.html","8a38d828a12c2f599f374b4448cc33bc"],["/archives/2023/page/4/index.html","35b7d6532c0069005dc73061cce63abf"],["/archives/2024/02/index.html","fabb1d31ceb069f50733712a98bd0dbf"],["/archives/2024/index.html","689a5ea88ba0df4be99585914384456b"],["/archives/index.html","5b35652dd2b3fa0f15f63a648efceba2"],["/archives/page/2/index.html","aea39b0fb7d898402fa5824aba23ba41"],["/archives/page/3/index.html","440834117180dc4a842fedb57757338e"],["/archives/page/4/index.html","9ecb67bfc51cf540265ae14625b36acc"],["/baidu_verify_codeva-qQP2iZOMLX.html","b1c5ee6fd00992f04ccb1aad693703c8"],["/categories/Java/index.html","2059426446b41ec8ca0bd888e58d72e2"],["/categories/Java/后端/index.html","a0e547d1000e1673cfcd774950164426"],["/categories/Java/基础/index.html","ae40d086fdcab5d12b7a5a10ab604c33"],["/categories/Java/基础/集合/index.html","b4352b79f89283d02a806cf8e2b35f20"],["/categories/Python/index.html","da9d80c846d2a44fec593fac74c61373"],["/categories/Python/编程环境/index.html","36071ba6c65b53b4d2a77bfae7dad08b"],["/categories/R语言/index.html","d2671f69d7750636980b0f1371554811"],["/categories/R语言/编程环境/index.html","ec4b24b596b0f935b33d0695b761610f"],["/categories/index.html","242bd490ba9cff020e487ab3457e543f"],["/categories/中间件/index.html","ac64e3b5cf2370d898617b51bf3d5f05"],["/categories/前端/Vue/index.html","bbbc9658d9b4519a1fcf8715d609f962"],["/categories/前端/index.html","195d30c84b71c9202a99791cfcf53bff"],["/categories/大数据开发/ElasticSearch/index.html","1428625c4b935e58b413d75eb55c3c76"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","a6909c47a82e18b49dbb598b7cc98725"],["/categories/大数据开发/HBase/index.html","d792ac21b1f7b1071e6ca9093ae7ce56"],["/categories/大数据开发/HBase/学习笔记/index.html","ac078317602fa98b1698783c67872f2a"],["/categories/大数据开发/HBase/环境搭建/index.html","d050c7c6c88f473d153b6c43f81ee605"],["/categories/大数据开发/Hadoop/index.html","d1453a854816354dc480e5714d3da962"],["/categories/大数据开发/Hadoop/技术/index.html","1f5d38d2bc9c8c4397951710e237009a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5460b32b78b3e82036cff97e7fa48672"],["/categories/大数据开发/Redis/index.html","0e7b862e8d338ff85ef597503887db78"],["/categories/大数据开发/Redis/技术/index.html","6937607061b57daef9bccab1925726db"],["/categories/大数据开发/Redis/环境搭建/index.html","be7b8f23c0032953dd3901c56a4d804e"],["/categories/大数据开发/Spark/index.html","0a5b25600e9fd2da893eae6c7a6375c7"],["/categories/大数据开发/Spark/环境搭建/index.html","0ed69ca5a5e3bb3b79a2c4c846791ee5"],["/categories/大数据开发/Zookeeper/index.html","74dd1a7bd6063b5e618952b19444eccb"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","abc7b51129cb6a885b903484ccd42d1f"],["/categories/大数据开发/index.html","89dcdc32fab5d6e53047567714f3c358"],["/categories/学校课程/index.html","bbdd19dab29879f5a43c5156f10a8ece"],["/categories/学校课程/计算机操作系统/index.html","71c20f2bf4a29f963d3fd6ac83ff8993"],["/categories/操作系统/Linux/index.html","e64ce3f0c3c74f61881f899c725d89af"],["/categories/操作系统/Mac/index.html","2dec2f6156a88d2258bdcd1040b0401c"],["/categories/操作系统/Windows/index.html","34e2d41e6b5646163d35dd81ff577477"],["/categories/操作系统/index.html","5bcec930f964704d69005300c40adf6f"],["/categories/数学建模/index.html","ac8b1d4815967aea5f93df5f24a13e5f"],["/categories/数学建模/latex/index.html","5ce48e9740b244fcacee8484b4647874"],["/categories/数学建模/优化类/index.html","9b20e96a7da66163b553913e46e82320"],["/categories/数学建模/优化类/现代优化算法/index.html","21e34ea7efaccb92911c812416449c10"],["/categories/数学建模/优化类/规划类/index.html","c73eb487fb4abd208ac5acc20197146c"],["/categories/数学建模/绘图/index.html","1c1755f9f38f954cc4c8c916f742356a"],["/categories/数据库/MySQL/index.html","83a2ab23a830a9bd76ac2ef332c22434"],["/categories/数据库/index.html","73b7a78ef01be69478fae09890c561a5"],["/categories/数据结构和算法/index.html","91c64f5a6cf4439e586ef70ee99c378e"],["/categories/数据结构和算法/page/2/index.html","f720ddbf60edd23e8c450dcc33b6c019"],["/categories/数据结构和算法/基本原理/bfs/index.html","4ce86e56db692822a94de426c69269ef"],["/categories/数据结构和算法/基本原理/dfs/index.html","f53a059cb3e6092ba319ba6a9ed62ad5"],["/categories/数据结构和算法/基本原理/index.html","1de5648669c4302f7c188a6e1101f328"],["/categories/数据结构和算法/基本原理/动态规划/index.html","fb69a1e0ed1c1429cfd3133cdf212c2f"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0213b9a61d2053b5e8f9053a530698e0"],["/categories/数据结构和算法/基本原理/图论/index.html","099cacb313cf2b408894209c1c3deb34"],["/categories/数据结构和算法/基本原理/字符串/index.html","c69c9e6bc6862f93c8ef875e36869a68"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","450ae806f3e0874ee1847a0372c171b2"],["/categories/数据结构和算法/基本原理/数论/index.html","bb0bb89580ae791c2c37d9cbe64c41fe"],["/categories/数据结构和算法/基本原理/树论/index.html","1b7e2445026357fbc4db09c70b9d3ed3"],["/categories/数据结构和算法/基本原理/链表/index.html","22036595d5406edda9db08fcded44cd5"],["/categories/数据结构和算法/算法题/index.html","cda2f5697315a9ea3063ca462c756cb8"],["/categories/数据结构和算法/算法题/二分查找/index.html","eadc4890fabdf32150eaacbd97234114"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4698dcaee6129e4254505c6e4ecc8357"],["/categories/数据结构和算法/算法题/动态规划/index.html","2367f4f16a5476deb2fa4f46dd3eac16"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3e064049b9e7d9922d7befae0903afe6"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","76433af1fdf6bec4fa98bed3aa44f1de"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b6bd8dd09b630030549e74b63b2e0ea6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","1fb7fb05023f26cf6dbd8287f36b83b4"],["/categories/数据结构和算法/算法题/数论/index.html","6aebe6951d4abbaba36e134fbfb27b0f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","32f19adc6f0dc3f5cbf02997efcfa497"],["/categories/数据结构和算法/算法题/树论/index.html","3de9607f496edd15353179a8b7b45527"],["/categories/杂七杂八/index.html","1464a62473f529cfb23a1cec7dfd2e8f"],["/categories/杂七杂八/博客搭建/index.html","eb6c7a7cf96df2492ea8dbb2c8f34250"],["/categories/编程工具下载/index.html","ceeed0f54fce4a9e4805cb7cc8f6223b"],["/categories/编程环境/index.html","5d5962f11ea41cc9d5d814d88528232e"],["/categories/编程环境/大数据/index.html","ab02f95c31aabea80da544942c1450a9"],["/categories/英语学习/index.html","87f584fb843effaa7d201740735373dc"],["/categories/英语学习/英语语法/index.html","80bf8a0c6a54937fa73dcebb0d6ec982"],["/comments/index.html","f0443273eaaff5bc618dae6188e467c9"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c7c197ec05846dc30f1b85039a55e6ed"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","b7d9ef4e823c07af25b5f582ca329c56"],["/movies/index.html","1c25ab68cc4671106cb6a6764d9f8db6"],["/music/index.html","c7ac8b74d493d66a67b1c4a40f51caf3"],["/page/2/index.html","1a410a856e0255e6815c24292aa96c6e"],["/page/3/index.html","5c4073fd6f6b8fe2fc14062f0de269f6"],["/page/4/index.html","beeab686ca4be63798ed11661e5a0814"],["/page/5/index.html","b78b137523ed9523143fdebb2ad86d8c"],["/page/6/index.html","fecf21638c13e288ace6aecc603bb6e7"],["/posts/1021360842.html","03d5392f40c95f7fcb7fce3736a26ad4"],["/posts/1120620192.html","78a13e38afe1553c5c836499f7f58423"],["/posts/1141628095.html","71886b91b0ebf8123186123002cb04ee"],["/posts/1168613674.html","f50236134ca7962e60662fc5bcf65a7c"],["/posts/1219920510.html","91e92a2f87ebc7c32a42c1b60605d230"],["/posts/1222166338.html","68c7bffbf56f973fbd7e14d579484800"],["/posts/1259097482.html","8dbdac3c75ebdd9ecf18c2332eb31156"],["/posts/1271036369.html","db322f0b46d2242ff9f8d6ae48ad29b1"],["/posts/1312847445.html","8a55e96c3a6c11dab208cf13a294ee75"],["/posts/135355774.html","32ccd9c0cbb528b8f74d5cdcfe657b0d"],["/posts/1375344716.html","7453a3b534b535a00293bb838700d64e"],["/posts/1388991698.html","8f9f9fbff502f0224ec25e57427001b4"],["/posts/1410315814.html","2a5492d5f7554e50227f8eae6ea45586"],["/posts/1452790229.html","eb66ce1925fc36fa61eb5eccac16df48"],["/posts/1470079884.html","aaa94b91364ea5c4528ac7690a322455"],["/posts/1470079885.html","56c755d12a058e13550f0d042ff24f27"],["/posts/1470079886.html","8ab3a02483cf1998363acea18c3ac56e"],["/posts/1470079887.html","6c01eebbe60abe6b1a581c4514e0e1fe"],["/posts/1498536549.html","2eb4b3b4a5bfd1475bf9e23610e3c229"],["/posts/1539568593.html","0198ab0604fc30668fc334e992510c94"],["/posts/1547067935.html","02a2fa53e248f5a4688312c2d826a881"],["/posts/1557866301.html","bbcae7e68034347d470e98cee8b35da5"],["/posts/1571776361.html","fd1a26e1e0e9f862d9d69347ede292e5"],["/posts/1605124548.html","98d755a5f85a2f8b11dbf0886917fd6c"],["/posts/1633036852.html","8e3b0e7adc4023bf81f050b46b454383"],["/posts/1674202625.html","32a274f018ea743794cbaf117ad2fd6b"],["/posts/1765123828.html","2d1a37d73eb9195e72e504e0b74b8e33"],["/posts/1767336200.html","9ff601889518aecf05528ea9d4c4dae7"],["/posts/1776114197.html","f198a8dd314c02901835f7f40aa3a2ec"],["/posts/1817748743.html","fd93c63b8cb5bf2931a467a20cfdeb3b"],["/posts/1925125395.html","70568d6b94ca2b03045e2bdc6482b91a"],["/posts/1966191251.html","4e54d798a74ef1fa48169a9920593e34"],["/posts/1987617322.html","972fcbaab275493dfe6f14e0936ea41e"],["/posts/1999788039.html","bf44713c420d2c4c4b21b023b6140556"],["/posts/2075104059.html","06858a3e3184c6948f4d96af6867ffbf"],["/posts/2087796737.html","80d344a8f01ee41366fa33645f49ed8d"],["/posts/2106547339.html","941d83701a53b29468d1a205f0a785d6"],["/posts/2207806286.html","821cd12c3d892e949dd3a0ad2e6cde1e"],["/posts/2225903441.html","e9231f4c21ead342d9903e8aa8e79121"],["/posts/2265610284.html","3dab24b67e48b7f45bfc30359b584de0"],["/posts/2281352001.html","4b9c4546b8332a562f0b7f2fd13008cf"],["/posts/2364755265.html","d3f84ccde3df58df89b36c70968527ca"],["/posts/2414116852.html","8a4cb86d9edbfec02d14ac122d1045ff"],["/posts/2421785022.html","c5a38640e148bde2e196695b66debd50"],["/posts/2482902029.html","08f598b0254be3a972b628c1502c9b5c"],["/posts/2495386210.html","b31c9cb4761c065665d5050a5f5cbe79"],["/posts/2516528882.html","62f0d0b7886b016846b0cdfb4f5160d2"],["/posts/2526659543.html","2f0577fb16ea1c83817e9ccd769679ac"],["/posts/2529807823.html","bdf3d8da63d239e760bcb1f53a859402"],["/posts/2596601004.html","de85633546154534df95889ebff65f78"],["/posts/2697614349.html","5a15a54cdb19086359d7c3710bf4633e"],["/posts/2742438348.html","11eedc6dfdd7d946b3cbc99839a2f165"],["/posts/2768249503.html","fa9ef1e4c3a277ee6be08dd92f9891fa"],["/posts/2864584994.html","020486f0a647b2181eb3ba5b37505adf"],["/posts/2888309600.html","4e787c699d8139b97208f45283bdaa75"],["/posts/2891591958.html","57c2b2e3872536106ec7c8f012af0374"],["/posts/2909934084.html","2a206be82d75620953ca6288235b04bb"],["/posts/2920256992.html","d0e7098fdf18a106218413b717e0196d"],["/posts/2959474469.html","092400911128e11a841512b7b0cd5c72"],["/posts/3005926051.html","2e0117f5a050b93651221f257f2265c2"],["/posts/309775400.html","138e992e6d4f1a7fd4006c7236ce4846"],["/posts/3156194925.html","c76dc30c266117f150651f5c7e75dfca"],["/posts/3169224211.html","df5e40f49e8dc044abe2768cae70c960"],["/posts/3213899550.html","100eafeff1464ad4604cccea1e7a3944"],["/posts/3259212833.html","bc96891b3e373f57f39f31967516792e"],["/posts/3266130344.html","0400227bc0aafd448852e21cf48bef1f"],["/posts/3292663995.html","91ec88fea1b12faff8fd61bb1fae5861"],["/posts/3297135020.html","ffc7fc9252707eb6a7ac131a8d654ff6"],["/posts/3306641566.html","6c0ff665ff761d2b2d9ae191ede04cd5"],["/posts/3312011324.html","71dbe04b2a62838563b88a0ed14a332d"],["/posts/336911618.html","936f78b08cfb29c89df576a366423ffb"],["/posts/3402121571.html","61d3cc9b65a7bd5debe92841e5822bc1"],["/posts/3405577485.html","c1ae9d5e4b95758c169688e65c8fb0ca"],["/posts/3498516849.html","80aed7f499f642bfd6b925193e67de2b"],["/posts/3513711414.html","d45fccef6c2a57777e5dd2358b9b99d1"],["/posts/3523095624.html","c3ed9b80e71a7b7340c6374b43af873b"],["/posts/3546711884.html","56b2e865ff0c6a4b0cd60a925e5523fd"],["/posts/3731385230.html","00a7dd4792e79737e673f732f685a62f"],["/posts/3772089482.html","ed138a643140d4f9ece1b4760536e186"],["/posts/386609427.html","20b10d0c639c6e72783e1aa01b605a1c"],["/posts/4044235327.html","caf7bbdab1f4ef2228f94944e4b68d37"],["/posts/4115971639.html","5f22e9b885d47c0389e4da5492d6050b"],["/posts/4130790367.html","5bf6da41edb236c50fe0d051bb677c2e"],["/posts/4131986683.html","88f339f1bf0ad0b9bd802bd29021f77b"],["/posts/4177218757.html","d5b257209460cfbebdac03a43763db40"],["/posts/4192183953.html","2b7a175594289e3708b239f7d8311bc1"],["/posts/4261103898.html","0ac459051b11c12955dae6df602fa549"],["/posts/469711973.html","023307aa28986b54ba144f401637990f"],["/posts/482495853.html","97859a3c1c3276b4dd80d2b51e739c73"],["/posts/488247922.html","b6226c9f6247f92d1b61ab4eacc3ab83"],["/posts/517302816.html","1f381a7e770c7fb1b9ba8a4c3e895381"],["/posts/570165348.html","5300f74be42005f98d07607c8e4dfe3d"],["/posts/595890772.html","b3579f032f523b20889af93e8443df4b"],["/posts/67485572.html","1dd091f7f4014412e8023d4ec6f0f35d"],["/posts/694347442.html","3dbeb328fba41c5187ad657cc6e5d0a9"],["/posts/707384687.html","daf5af9bd77f836148c6d91d23950870"],["/posts/71180092.html","05057e8e9dec5d634df55bafe012e2d8"],["/posts/716459272.html","e16a9a03f9e776ef9d4a54b4a3eaac5d"],["/posts/765481613.html","f27e3408e0dcd6eeb5e03abbd6fc78f2"],["/posts/778231993.html","a9d1ff7c2364a68e33e527eb25eccaf1"],["/posts/795397410.html","28b0e43520717bd544eb16a80190fc06"],["/posts/820223701.html","698f75b81e54851f06c93d78d4f0a814"],["/posts/830372185.html","61b778cc3f2b4fa255703b48ba98fc46"],["/posts/88294277.html","4728c56f72434ffaffd6d437356683b9"],["/posts/939963535.html","c3dbc4a6f342ff0891a5c3c42848d195"],["/posts/983786067.html","40619faa0fcf95f0395f3b6ab9497cce"],["/sw-register.js","cd05f9013fdcc3a7c404d81df9f9623c"],["/tags/C/index.html","ecb309038e67d2310de507c3122dae74"],["/tags/C/page/2/index.html","68703577581fa16591a1077d2ca796ce"],["/tags/C/page/3/index.html","076eb5269fba1b91f833a28cb23f2499"],["/tags/C/page/4/index.html","1a38f998e89fdd48dd4ace0849d3d042"],["/tags/ETL/index.html","f52363d9d9fb3413356eac8705864efb"],["/tags/ElasticSearch/index.html","a44efbecc4e7e96fd4bb43a20dfae801"],["/tags/GUI/index.html","a9cd0529c5f3e031325768f81f4a0bb5"],["/tags/HBase/index.html","0c311b3aadee4cf8362f466463b501b8"],["/tags/Hadoop/index.html","1f9b916f11c2fa4ec29019f2af93cd2e"],["/tags/Hadoop/page/2/index.html","f5421d0375da5c34cf6c3c71519d8d3b"],["/tags/Java/index.html","78dac3190e93604e4485d16ff8e47972"],["/tags/Java后端/index.html","76af487be5adc277067d831949e6d395"],["/tags/Java后端/page/2/index.html","4c860d5b192f647c4e60418969b055c0"],["/tags/Java基础/index.html","a19ba8ac056f79b0f3436c270029f995"],["/tags/Java基础/page/2/index.html","4cf4385b83d145a96d7f83f0192682ea"],["/tags/Kettle/index.html","47f483707ff4b7a462581db750becca9"],["/tags/Kibana/index.html","3e2328bdc4391f8f1c65f44843d34f0a"],["/tags/Linux/index.html","4420fa79fc9199860ed2f0dc458974a8"],["/tags/Linux/page/2/index.html","763c57925b7ac4786cff46d3326a9f51"],["/tags/Linux/page/3/index.html","888c302157cdc8e81cb6e597e7468a6c"],["/tags/Mac/index.html","420d934fe4c751889ddc01e1d55780c9"],["/tags/Mac/page/2/index.html","5757c1215a2c64e05f2cfa963e0d2d0e"],["/tags/Maven/index.html","0e33491b915498dbccd63a9ce08e64f5"],["/tags/MySQL/index.html","342f951f5460592e8486504be296237a"],["/tags/Python/index.html","fa5e2c06a990462cf4d9e9cce8d29fce"],["/tags/Redis/index.html","15c83a7134cb6f70af519fc0491b48ea"],["/tags/R语言/index.html","749567ba700469e49e9454369d77b324"],["/tags/Spark/index.html","382192397e8eee7b032c6c016f233fbc"],["/tags/Ubuntu/index.html","7c44edd7dd3641457ed7bc95a4897c67"],["/tags/Vue/index.html","2d9e44801f8fbc396069c6a1915e91fc"],["/tags/Windows/index.html","0c39a3bbc9f9b61c8d01841cfa0a62c0"],["/tags/ZooKeeper/index.html","3e13c8260aec69a3482ea4c881eb5bb2"],["/tags/bfs/index.html","56b905541fb77c9cec8f829a8c6a4276"],["/tags/dfs/index.html","6bb19314fc8b04c344144442948063d6"],["/tags/folium/index.html","de7669930c9c1a1cab3dedb67f6e04f9"],["/tags/git/index.html","b9226b1cc174bc01368c87925da09599"],["/tags/index.html","7e7b2c72726a4d633cf28c02b0ee379e"],["/tags/latex/index.html","6776f51f7ce2d6841838b7d5f7c63b58"],["/tags/中间件/index.html","fb37f3b3c286d1d54ddb6d56dc58cbf9"],["/tags/二分查找/index.html","0796158e6fc4bc96f1bf5a8658791f6b"],["/tags/优化类/index.html","55ae1726ac3e5954cfbc9ed9c3fec5f1"],["/tags/前端/index.html","7d6df1f79b03793e749454ec84955f41"],["/tags/前缀和与差分/index.html","42ec615802c790dc7f8167a8098f3a8b"],["/tags/动态规划/index.html","cb1dda0593604414f32ed070ee2affed"],["/tags/动态规划/page/2/index.html","ede7842f8430662f63c171ce2690ac11"],["/tags/博客搭建/index.html","0d430198028205554751b528a67f2fec"],["/tags/图论/index.html","27e6feb3b09b439fb3454e647a9b2752"],["/tags/大数据/index.html","f46aaddb25fb4a56219224472714fe4e"],["/tags/大数据/page/2/index.html","b7723718fbca84ee068c8b6d787b83c7"],["/tags/操作系统/index.html","22459c752abc1cda035628197322531c"],["/tags/数学建模/index.html","0789d590bf470c966aa17d6945f2deca"],["/tags/数据库/index.html","6ca2eabd697d2e4817eb060be65e2b15"],["/tags/数据结构和算法/index.html","487019a33ca71074fd96534464cc3f45"],["/tags/数据结构和算法/page/2/index.html","56b951843d6670c9f74f2bcf454d6be9"],["/tags/数据结构和算法/page/3/index.html","943bc5966731f0cd9e429ae77dbc20e8"],["/tags/数据结构和算法/page/4/index.html","88e8ddc9002f7c3c7f78d037ccde68ed"],["/tags/数组和字符串/index.html","865bd755de22b9ddab33e09837c77556"],["/tags/数论/index.html","3648f1253abb1591125f94bca69b8804"],["/tags/枚举类/index.html","8cfa35b8a000e53a2643240b234b92c0"],["/tags/栈和队列/index.html","6d24b6691cdf564f2425e42f97e7a6b0"],["/tags/树论/index.html","fbc0b11dfb04afa9517e5e3caf2e842e"],["/tags/测试/index.html","5afbefe4b789ee6e5bec9594e844da73"],["/tags/环境/index.html","7db08181b9c6f808dbbba0568e3a897b"],["/tags/环境变量/index.html","b9507d0f68bb89530b532d71f33bce0e"],["/tags/绘图/index.html","2b1507a7021fd84ce91bdeede4cdf8db"],["/tags/编程工具/index.html","c663d387f8468cb36c5a65b32f13153e"],["/tags/编程环境/index.html","fc0a9988f1d81f7559534559db186499"],["/tags/网络编程/index.html","1c83c90bbf962059db10f3fec18ebc76"],["/tags/英语语法/index.html","fd72851c05d078e2ef60d8e5f8dfaec8"],["/tags/计算机操作系统/index.html","7ea635f32783ff405c706f61525b09f2"],["/tags/论文/index.html","ffd25d12e9a891c59570f8595598d25f"],["/tags/资源下载/index.html","7c7e43548ed10a069bb508190db21832"],["/tags/链表/index.html","b95993079e066232d5e291d6a3dd0f3d"],["/tags/集合/index.html","2bffb9379f7506097e277ca7d4818b72"],["/tags/集群/index.html","80ddc8f0cb5a043af557ed0d2c189aea"]];
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
