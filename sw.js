/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d14bcf3c9ed8d1994bcc3b0755dea214"],["/about/index.html","cc85ae8d0b2c45522a8ef5eb1e89382d"],["/archives/2023/01/index.html","503b19d192dd1a7259e8b7b65952b881"],["/archives/2023/02/index.html","1c17aadae67d7a31be5988f7c42d72da"],["/archives/2023/02/page/2/index.html","aacdf0ff205c8f37f52c88d36672d73e"],["/archives/2023/03/index.html","fbd2b5c4f113d7abac4963024d0101aa"],["/archives/2023/05/index.html","d1b8a21476b639384d0696fb825ea36a"],["/archives/2023/index.html","414216dce447fc746d8a6f4bbefbc69f"],["/archives/2023/page/2/index.html","2a4e57cf103f4982ad0bab540835c7d7"],["/archives/2023/page/3/index.html","2979e3096a1b5a43a4e424541d07d67e"],["/archives/2023/page/4/index.html","3332e613ade025430d2a5dde4d1f50af"],["/archives/index.html","e496d8d97384a4e622503c1398e4290b"],["/archives/page/2/index.html","2320bf0b65de45f749e9b15c474232f3"],["/archives/page/3/index.html","a4ff50db1ac1dbfc50a5377ff8383673"],["/archives/page/4/index.html","03d6bc93d6419857bb5191dccb7d1d55"],["/categories/Java/index.html","bfa52aecd6fc26eb3a49ceec4f64a658"],["/categories/Java/后端/index.html","2eb7a80a1d467cc712ab59c1831b9fa7"],["/categories/Java/基础/index.html","f7619c2667a94fdefd620255040ecdb5"],["/categories/Java/基础/集合/index.html","56fc6156911ca069fa3fdc425dfb0dff"],["/categories/Python/index.html","ca6bdb4cd1c59e64da11a33cdc45be20"],["/categories/Python/编程环境/index.html","c1773348bac3581832ac3b38ddd6f591"],["/categories/R语言/index.html","92d38fe0ee521a60ea3cd510d501456e"],["/categories/R语言/编程环境/index.html","d1d51b24ce774574ac8b5738f31cb26d"],["/categories/index.html","510823d6bb7865259fa0b451a6d34228"],["/categories/中间件/index.html","974d04e4954a0ee26432de37d3320c77"],["/categories/前端/Vue/index.html","0a453b6c69766a29fa6165fdff8c022e"],["/categories/前端/index.html","b4c6a60bf509215cd3b60c0ab4c8c7a5"],["/categories/大数据开发/ElasticSearch/index.html","0def91559651bf17845a8d1090164f71"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","633267c19dff2ffd44c611bdf6b54d83"],["/categories/大数据开发/HBase/index.html","267c22560bdf5fbdc7a12eb3f9474314"],["/categories/大数据开发/HBase/学习笔记/index.html","7e47fc536beb33829f9114f8d66fa1c6"],["/categories/大数据开发/HBase/环境搭建/index.html","805deef0e92ba9af58f8ccbc81e4ffe7"],["/categories/大数据开发/Hadoop/index.html","c51031c34100cb7ef99e9638ec105221"],["/categories/大数据开发/Hadoop/技术/index.html","775f4100094ce2a1ebd243362d43b740"],["/categories/大数据开发/Hadoop/环境搭建/index.html","249e0362930a9371ed550e13697c2ae2"],["/categories/大数据开发/Redis/index.html","fa58c18e2594b702abd9e4e1aaf519ba"],["/categories/大数据开发/Redis/技术/index.html","2db5eb134d808ee741779adf3e5ac238"],["/categories/大数据开发/Redis/环境搭建/index.html","93a3c8374590eeec56ac5a04f13a3004"],["/categories/大数据开发/Zookeeper/index.html","5b32624bafca30ec27b530e19bd65d4a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9127c89b23b9293db3cdc994edd14766"],["/categories/大数据开发/index.html","5f6891314e0ff03d6b00e28c59f4b4c9"],["/categories/操作系统/Linux/index.html","a544b21c093074919baab182cd4702c6"],["/categories/操作系统/Mac/index.html","5635ca4371e75aadfd43d925341922df"],["/categories/操作系统/Windows/index.html","4365d4e57e3fe2d97f68a91ad6edef66"],["/categories/操作系统/index.html","ba65bb13853db63d019ebc665154674e"],["/categories/数学建模/index.html","17cbb9b57e0f1a2ec631f998a0d6123e"],["/categories/数学建模/latex/index.html","70782bfed43d1c03f750666ae6666e4b"],["/categories/数学建模/优化类/index.html","d3ba916f827bea62e336d447b68e9bb2"],["/categories/数学建模/优化类/现代优化算法/index.html","4de7a8940fcf7f2e153dcdf34c98c55e"],["/categories/数学建模/优化类/规划类/index.html","355fca2b291d11693009a61c4397380a"],["/categories/数学建模/绘图/index.html","408c5e553f153254fd04a688664f7506"],["/categories/数据库/MySQL/index.html","411e6ea7e14458590f8d0dd476e962b4"],["/categories/数据库/index.html","e797377da66db7f835da1c6ed8ca72f4"],["/categories/数据结构和算法/index.html","f4bf5896585532d20a70b4d18b3c9df5"],["/categories/数据结构和算法/page/2/index.html","5dc33150c0e5e3669e8c2ba6578cddab"],["/categories/数据结构和算法/基本原理/bfs/index.html","e3f0e992fa3cea36f68eaebe70c0f34d"],["/categories/数据结构和算法/基本原理/dfs/index.html","b2d42fa05e125d0c5bac886d9a3fc250"],["/categories/数据结构和算法/基本原理/index.html","7a62a419a23bd95842d20df7374ab5ca"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b4b5a6319910c3510fdd1c2e7076d0c2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0cb7a51ca26e58a20ba169eb75bc773a"],["/categories/数据结构和算法/基本原理/图论/index.html","88bc5bb22c197c28ce17182b3fedbaf2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","645bcfc39731e96e23c8c4b9be0fbc63"],["/categories/数据结构和算法/基本原理/数论/index.html","845ebf11b9aa5b35d1387d827fec38ca"],["/categories/数据结构和算法/基本原理/树论/index.html","e7028cda166f3a43d9d414036bce20bb"],["/categories/数据结构和算法/基本原理/链表/index.html","4cc43fe75af908370fb1a5236a01cded"],["/categories/数据结构和算法/算法题/index.html","0ea0f4b05db7e7af91241ae5787ebe80"],["/categories/数据结构和算法/算法题/二分查找/index.html","9f40310288a2a2e58af2c8b0ec1e8aee"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6431a3f76369af342c6d4c66b489f2fe"],["/categories/数据结构和算法/算法题/动态规划/index.html","af9d8187b46c0a1348b3914e67960e99"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","500a2f849417878ffd6a970280da9be6"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","c87c5a9b0862b4d389946644f529f4a8"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d475b6600f298e543478ff5a881884c5"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b4e4d6fd0fe0db934410125d6ab3af70"],["/categories/数据结构和算法/算法题/栈和队列/index.html","77ce455b58a6d6e8bd876c9955c25f2c"],["/categories/数据结构和算法/算法题/树论/index.html","03a0de2b7e69bb3d796bbaa78644246e"],["/categories/杂七杂八/index.html","b220eb7732e0a1bbffb50eca941f3f75"],["/categories/杂七杂八/博客搭建/index.html","66f11ba975b647bfb7cff128822f1602"],["/categories/编程工具下载/index.html","2e10ccbe63b40fe7adffa55bc4e9f743"],["/categories/编程环境/index.html","97bdef96b0beccd5c74536c813c8e72e"],["/categories/英语学习/index.html","294d97564e43e5af0134eaeb245db565"],["/categories/英语学习/英语语法/index.html","47df3a4ac6dc269f23947713b8490a8c"],["/comments/index.html","7bbb0e071dfafb8636a9a3486cdeecfc"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","4092131c8202f79619b648317fac195f"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","465f9d8e980f163a99de12b770d9ffcd"],["/movies/index.html","444cebea5b83c258d49b7da4a27ed379"],["/music/index.html","00a0c95fe3ef8b5290d6239049048e80"],["/page/2/index.html","dd0521aac5425705701d2114dbe7a6eb"],["/page/3/index.html","698241cdbab477378af537e7785c4de2"],["/page/4/index.html","d753d93f3d731cdfa4e180b296e36060"],["/page/5/index.html","a6e2fdcdaa64a73e0a92207cbc357baa"],["/page/6/index.html","acd1817b416d2c075d6ab9616964e680"],["/posts/1021360842.html","53f3c44a398941c6db12f197b60c68ac"],["/posts/1120620192.html","05f75a1ed9bfea9d7bbd066a332b0aea"],["/posts/1141628095.html","b689a48891c74ff1d08ba7cda0931d09"],["/posts/1168613674.html","01d394abaf850494eb202f694df54929"],["/posts/1219920510.html","f49cf6464c24e34e96247c30a502a42e"],["/posts/1222166338.html","34fafc407aa70114d4dc72648352b8bf"],["/posts/1259097482.html","754df886a9ea48e5e1de205689dc480e"],["/posts/1271036369.html","e9f6a8460ea8e849b816224677b7815b"],["/posts/1312847445.html","3635dc94e4778dd9a0a2ff5ccd67de49"],["/posts/135355774.html","b159d54d6485680b8a869fa63a88bc45"],["/posts/1375344716.html","796c17ad545385c4ec8aacabc485245b"],["/posts/1388991698.html","4d46af3668dd8ea369eccdc3a1db633b"],["/posts/1410315814.html","5b88755a31cf86af3e3eb376062beea6"],["/posts/1452790229.html","6869e241f7cb49ae54a3495444c81b65"],["/posts/1470079884.html","9b044878be7e37d23c4b749d8ede4b25"],["/posts/1470079885.html","0a6aded735686041a19ebf88d245de50"],["/posts/1470079886.html","d8f3cf4c51d48de548fa79abaaed5c5d"],["/posts/1470079887.html","17b335908c9bdfc94cad62734055d688"],["/posts/1498536549.html","28ab616b6f0227f98e33092c7e08458e"],["/posts/1547067935.html","5d83ac07fcf79bbc5725ee423211cf77"],["/posts/1557866301.html","188f85260a80c57b2f511da75b4094e2"],["/posts/1571776361.html","8de59e93ef9be97b68e02dab2f2401a7"],["/posts/1605124548.html","27ffaba61bbab601a87b32b45bd43af0"],["/posts/1633036852.html","08722fd79fc6f9f8cd905cfb0d9d8402"],["/posts/1765123828.html","991cbeaeda97eb25241f6bbe8dabb3c8"],["/posts/1767336200.html","27b1e5c29cfb0db884e68dadb2c37d12"],["/posts/1776114197.html","c150ebca46859f0ac04035ac831657c3"],["/posts/1817748743.html","2fd4f1951ddc3dd32163eb26e9f1eeda"],["/posts/1925125395.html","c39776741074d2514c73204bdc2788d3"],["/posts/1966191251.html","3bd227f52dcefc371ca77a94df8b52f1"],["/posts/1987617322.html","7afb8575bb928b7e6f03fab9b464035b"],["/posts/1999788039.html","9d0cad11216149101189cd96bf61f786"],["/posts/2075104059.html","fea27e95424f06405404e37f9004b3a7"],["/posts/2087796737.html","124566b84a43476b2c6d8afbe1aa42e9"],["/posts/2106547339.html","a179c7db02d178b4e261964dafe1368f"],["/posts/2207806286.html","003e7d451d7ce2fa04470c524da8a176"],["/posts/2225903441.html","539dfd3d96d78c405487334a1f8cc975"],["/posts/2265610284.html","631c853adb58345ef7ddaf6349e1b4cc"],["/posts/2281352001.html","e45cf583e640af7a64981f8bbe1ccef6"],["/posts/2364755265.html","a9b4052a7a77a5fb14f631e5c1c0a579"],["/posts/2414116852.html","bd5ad7751c570a9cc6f3717d757e791a"],["/posts/2421785022.html","7c4df2bf47d8cc0ef12d4a1623822778"],["/posts/2482902029.html","76a7750ed8874c92edbc741eb316ccf5"],["/posts/2495386210.html","6faa180d4efb136632359916ced6507d"],["/posts/2516528882.html","18a68ebcbd91095f3e53c748da658c84"],["/posts/2526659543.html","295cf831fb9fa8bb1d7344848bba41ee"],["/posts/2529807823.html","9bfe267965878751ee3833dd26d91f19"],["/posts/2596601004.html","04809849aa6c951bcc5ded7b0f6a777e"],["/posts/2742438348.html","18029dded3d125dac1544861c603225f"],["/posts/2888309600.html","263607d65683cd0dadbe7d91227f7038"],["/posts/2891591958.html","6cfdfb5bb7e2af4cae43f853b942c7a8"],["/posts/2909934084.html","5f1d120c2b5af394d5e9a2a0cfb5b9c3"],["/posts/2920256992.html","04fcebe68a740425186f72cca96c4309"],["/posts/3005926051.html","7a16b03a04a443ac518d0c715155033c"],["/posts/309775400.html","42f351151d4af5e7fc8f50ccf0c1f8e3"],["/posts/3156194925.html","1c009e54501168666aadc02917c0e76c"],["/posts/3169224211.html","8f5efc67f2cb13155b6747462bdb1041"],["/posts/3213899550.html","8c718e64f94929bd5b05f71c27ce5fda"],["/posts/3259212833.html","37ee24bb6f595f4e61ee45ca0b484f78"],["/posts/3266130344.html","1a54018e1fc4d4141ec4786a3d90651f"],["/posts/3297135020.html","cda1a7ef0d9f18cb34cf26f2d33c7c2f"],["/posts/3306641566.html","892828651eda3004fa645303123a693c"],["/posts/3312011324.html","60d82f21d95d41d95e2aa3cd0dde9775"],["/posts/336911618.html","188db00dfcd32543dbe072e2a6c212b7"],["/posts/3402121571.html","116876f6df8f752504141e906d9b290d"],["/posts/3405577485.html","6ff1a80f6983eb60999161f9e5cfe598"],["/posts/3498516849.html","5d3e89a18dfdf10c8d407c789d4058a9"],["/posts/3513711414.html","3a999261b02a2f447ea361c825641aea"],["/posts/3546711884.html","d50742dfffa7522aec182f6dc983edca"],["/posts/3731385230.html","94dc6373dc54b570b78b80e75335b32c"],["/posts/3772089482.html","c20a95105be976ba622d7c9bde7f1803"],["/posts/386609427.html","db6df9e2359d709920ebf52f7bd6dd3d"],["/posts/4044235327.html","4eb31bfdee9c69b2eb99c34fa7977c06"],["/posts/4115971639.html","0ca14a6d85030973a7bb6523b412f85b"],["/posts/4130790367.html","1b5d7b40a94e8c0500f6b3f2798e6703"],["/posts/4131986683.html","782e0f72337ba598e77a8f0aeb8a1bfc"],["/posts/4177218757.html","37920497eb4aaf3e653ae652a8730835"],["/posts/4192183953.html","1e97bc9e330f0d401304df82b65bcbeb"],["/posts/4261103898.html","b30a049809cb70f52f942b4152fbd148"],["/posts/469711973.html","d3881c40c90420217a6abe4dd50e77d5"],["/posts/482495853.html","2bc5c6d6c2ba6d6d665f01f515dc7ca3"],["/posts/488247922.html","bcb7373efe17e332bff216d1bd255450"],["/posts/570165348.html","d963899ab81c27d3f96555901703aa42"],["/posts/595890772.html","3166ca037d9b3774ea435b3064078119"],["/posts/694347442.html","89eaf0fda41ca04dd9a29821d1ed76dd"],["/posts/707384687.html","e8ac2586533526a06c05ffaa33378748"],["/posts/71180092.html","a5daa43801daf135edad08a3a47f1b06"],["/posts/716459272.html","51ee2087146c21533c890f8824115e45"],["/posts/778231993.html","1ebafcd4ae95804dcb90414232c4eb2a"],["/posts/795397410.html","e146588dcd0ea837eedae91bdbf44ac7"],["/posts/820223701.html","f5bfd83a22a1ad1ba59b30bf9eb4d92a"],["/posts/830372185.html","ffba000c841a31adbb29d3cf96812631"],["/posts/88294277.html","671c097dd4a3a815fdeac233aaa5664b"],["/posts/939963535.html","692f8b358665073096cff3f888b66933"],["/posts/983786067.html","99a57160ffa220a545c1aed0489b06b8"],["/sw-register.js","92d92918f0fa060730ba9df740cb8277"],["/tags/C/index.html","8f91b2ec7c0d95badc1476100c31c8c9"],["/tags/C/page/2/index.html","384779ee195ff3a5b232c33ff622ce5c"],["/tags/C/page/3/index.html","d04ec3c82d6d9acfd40dee6cbf51d49b"],["/tags/ElasticSearch/index.html","b61c87a8f8904e5980c9fc917f9accce"],["/tags/GUI/index.html","c1eea4fb9ebf4d4f5c76a8567d84f3ef"],["/tags/HBase/index.html","ee57f0e056fd42a13060ede0aa7dcfc7"],["/tags/Hadoop/index.html","e4882d6343c92ee2db9765eb16d5c684"],["/tags/Hadoop/page/2/index.html","5016a71af5a00d9e8caa7c4f2261ab49"],["/tags/Java/index.html","7a9733d2a024abf378d53b27ac5689b9"],["/tags/Java后端/index.html","d6256ba7fc64ae03d96b9f57ab97128e"],["/tags/Java后端/page/2/index.html","ef75b0323563d6d6c7ce12810f51b823"],["/tags/Java基础/index.html","52b8964f286845208f0cf535f7c5ac91"],["/tags/Java基础/page/2/index.html","f850e4ef1c9677318c3b1ce4803a97f8"],["/tags/Kibana/index.html","3537bcdafacb1956ec631ef32d1f4705"],["/tags/Linux/index.html","7ef4fb873700f1a24aa7a543695fe875"],["/tags/Linux/page/2/index.html","cfca2f04aa3daae968f96635dd1a2cd8"],["/tags/Linux/page/3/index.html","60ae1b1f885de7512203a29d46db5ba0"],["/tags/Mac/index.html","67fce61f378b8dccbfa695c426ea67ae"],["/tags/Mac/page/2/index.html","2571d9f3046e90775d2cec12d341c1a7"],["/tags/Maven/index.html","47a9cbe3d51436c97419ae86987d60f8"],["/tags/MySQL/index.html","5c0c785b307f5510b2ba85038c20ee47"],["/tags/Python/index.html","46ec5e39663a5c2b45f23f16b0dcd72c"],["/tags/Redis/index.html","b5ea319c3adbacbfbef3840de8ff3c44"],["/tags/R语言/index.html","b501310df04fbfdb0476f5bdcee31b8e"],["/tags/Ubuntu/index.html","8b8f0f3675d14741cb57bafe7aca3c63"],["/tags/Vue/index.html","831661ce9a6f17a85cc3c3d3e77e6545"],["/tags/Windows/index.html","7c4d322ea9e2adfc26831157d14ba5dd"],["/tags/ZooKeeper/index.html","4bf0de93a97982fc74f3d87d879f672f"],["/tags/bfs/index.html","9049470d1f198ca6733a230f4cbb1c4e"],["/tags/dfs/index.html","b3d5a0ed74f1e05de813b4ae9f4aa89a"],["/tags/folium/index.html","b18c46f14a9cb6847268ae3f4fa722f4"],["/tags/git/index.html","76dd42fc47df799b75cf820dc8414551"],["/tags/index.html","3a950d4bd76137bfd6a69bbfd8b0ba82"],["/tags/latex/index.html","505afe273934741bc155387a119a99da"],["/tags/中间件/index.html","f01ca7768f0ebb1bc56bc508aa546bf4"],["/tags/二分查找/index.html","e1fc625ce46d5c0790fdcfc11ccff6fc"],["/tags/优化类/index.html","a7038f1b24b81409c5c81bb80196ab9d"],["/tags/前端/index.html","45ac722e1c375c49c8478416c65d1609"],["/tags/前缀和与差分/index.html","d09293f85a429287f20428f7031d9783"],["/tags/动态规划/index.html","6a0bde8b365f7bb24aaca9662bc8de86"],["/tags/动态规划/page/2/index.html","87f043f9369f3da6be654d7e1995d36e"],["/tags/博客搭建/index.html","f22a747776da0b5e68c884678a196076"],["/tags/图论/index.html","3753a4b9437b1f803a375ab3b2b6a7f5"],["/tags/大数据/index.html","d75b4c30fe06b3728848d739ae56dc90"],["/tags/大数据/page/2/index.html","8bd3dc9d7d6d98d78ccb19629ea96e06"],["/tags/操作系统/index.html","fa232571e4218e8efca47a8ae51ceb07"],["/tags/数学建模/index.html","1d7f60718a2385637e370eecbcb73d0d"],["/tags/数据库/index.html","4e2c37cb246180e677454fabeb862533"],["/tags/数据结构和算法/index.html","de2a53938a47cd1f426be7d0fb7d6da8"],["/tags/数据结构和算法/page/2/index.html","13f67c1ea82e8ae70efaae4ec7867052"],["/tags/数据结构和算法/page/3/index.html","233df93d371811e1c4cbefae361c453e"],["/tags/数组和字符串/index.html","6100b778826413fb2488654359af9c5f"],["/tags/枚举类/index.html","dce7c31e38d15d5f722b3239047ee754"],["/tags/栈和队列/index.html","2dff3d218151af4f99f98b12e72f06a0"],["/tags/树论/index.html","2ec3af8bcec9bee58ba6097e75d7637a"],["/tags/测试/index.html","ff2af546a5a106cac57ad46fc17d40a8"],["/tags/环境/index.html","b3d485392ff84640cb985f98d25c1fc7"],["/tags/环境变量/index.html","d7f60f6888031efe24e039612d45b22d"],["/tags/绘图/index.html","acf5c6425d8e9c42e8be7986cf1ef979"],["/tags/编程工具/index.html","d953e055eb771709ec1ec6ef15ece0ef"],["/tags/编程环境/index.html","aed0766458ee7e3127f92a3cfb9e9a83"],["/tags/网络编程/index.html","e8941891eb7d04738556d2b9ddf4871c"],["/tags/英语语法/index.html","8251afd3a6fb684ca8c64a8f95f3a557"],["/tags/论文/index.html","6f60f81f5bdf85d86a093e2220c4de88"],["/tags/资源下载/index.html","0eb8404443e3df3fc8a7156b8bdccbe1"],["/tags/链表/index.html","75f967389a8423206c6cc961765daefd"],["/tags/集合/index.html","8931a9d59920293acc26b65d6d2e48b2"],["/tags/集群/index.html","a0983b67b125fdecea39691482e0609f"]];
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
