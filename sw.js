/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","95cd395548b8180ebf97f52dac9b9fbc"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","792531e6edb2df240defc664f4eb3c5d"],["/archives/2023/02/index.html","5056df638bcea8eac43afb9172ab926f"],["/archives/2023/02/page/2/index.html","530714923e70446f5099b985b48633c5"],["/archives/2023/02/page/3/index.html","8cb7a601aae3a70e8c6ffc7806c050d3"],["/archives/2023/03/index.html","3a4a506e2587fb7a17d390c5268243df"],["/archives/2023/05/index.html","2d1c01c2787693b1ea00f14e4400def8"],["/archives/2023/06/index.html","b2464aa19ef386341eb9c4dc39249c5e"],["/archives/2023/09/index.html","ac453e05c9c94ce9b50f773abbc686e3"],["/archives/2023/11/index.html","a522d865f5eab6bf83dcd799dafc49fa"],["/archives/2023/12/index.html","9043b6e2977359a2b2ba9a5087fc1588"],["/archives/2023/index.html","8e8b90c9cce8544e5fdd338a46b0f947"],["/archives/2023/page/2/index.html","d1e005bd6bba59cc335b553a80bc2b39"],["/archives/2023/page/3/index.html","edb4278d88204a745fcf4a93a1c163f7"],["/archives/2023/page/4/index.html","b866d8e17f303143d19ab655ba49d74c"],["/archives/2023/page/5/index.html","8cc7bd62b5f1d7f89a8c95c534ef490e"],["/archives/2024/02/index.html","6c4d1dd34dae4d5aea075336a1eae61b"],["/archives/2024/index.html","26213dfbf5b678cf0e61a2a9f4151f58"],["/archives/index.html","e1bdf2f8d66f3bc3e953a17c4742f362"],["/archives/page/2/index.html","0bd09015e69b8f18e5284a560255834d"],["/archives/page/3/index.html","b0746e6c6fc712e85c1784bb6d050041"],["/archives/page/4/index.html","832a1ef65268979d94bd1a49ab713881"],["/archives/page/5/index.html","2d74348d5a0b7b286772ed8856591af3"],["/baidu_verify_codeva-qQP2iZOMLX.html","f1fa6f87774b67f454358882ff41aedf"],["/categories/Java/index.html","5147f6524b955fef8b566e431f8571f3"],["/categories/Java/后端/index.html","74df29b4d650150c9af0db5d30d0ec13"],["/categories/Java/基础/index.html","5ba27cffead60246d6feb0385980a446"],["/categories/Java/基础/集合/index.html","063a2f7deec14392dd6f20cdedbdf06a"],["/categories/Python/index.html","3a90b62052a1d11bf8358d2d94687454"],["/categories/Python/编程环境/index.html","f2e1cbdd599fc0f32f651683916dbbdc"],["/categories/R语言/index.html","017a7502b3c5d207e0d5c6d41d6fb684"],["/categories/R语言/编程环境/index.html","57d7de42da11e25b851ba1ab7fbfb57a"],["/categories/iPad/index.html","e058b4ea22f981260fdd6ff6ac190048"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","cfe8214ae8829b9157af3e77a3155b06"],["/categories/前端/Vue/index.html","3fdc3a795348aad67ae132beb159d1a1"],["/categories/前端/index.html","abd0ce8c51b601da53107b54b9940dc2"],["/categories/大数据开发/ElasticSearch/index.html","51aebd6a935707560515cf41ea32a3d1"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c5234b887f29a431db4c7679c7b1aa96"],["/categories/大数据开发/HBase/index.html","8050807388c934d56c5a0eaa02550a80"],["/categories/大数据开发/HBase/学习笔记/index.html","3ed47956a8526b4102b8e7783e3e6f06"],["/categories/大数据开发/HBase/环境搭建/index.html","d89f2495d2a5a20f0e2ae289af5da06a"],["/categories/大数据开发/Hadoop/index.html","40bd88690e1b7e95aa26c1d91daea75c"],["/categories/大数据开发/Hadoop/技术/index.html","4fedc52952be37d52ce48bd53b2c28b7"],["/categories/大数据开发/Hadoop/环境搭建/index.html","02632e65792012dee2418158342e5755"],["/categories/大数据开发/Redis/index.html","48448949a1490053a4b21a4bf0921e31"],["/categories/大数据开发/Redis/技术/index.html","4f563705913ac6b2b5f2421a67449861"],["/categories/大数据开发/Redis/环境搭建/index.html","0d25d4e65a16b3a16b1b91dfa7403355"],["/categories/大数据开发/Spark/index.html","ad2839b5687841f93209c9c89d12027f"],["/categories/大数据开发/Spark/环境搭建/index.html","e7265da58d754810a1d0df7476434659"],["/categories/大数据开发/Zookeeper/index.html","105a431b06e6b6d5b5e0b1f1b723b0bf"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","39c29e95008130d5b88a34b72c947d5d"],["/categories/大数据开发/index.html","70135dcfa92eafceba8664e78e773531"],["/categories/学校课程/index.html","4a9a562d881532d0b7523d9f080ab1f2"],["/categories/学校课程/计算机操作系统/index.html","70f110004aeab032b8e864d1f2727f15"],["/categories/操作系统/Linux/index.html","48771ad6b9bd29a3a1e6eea8737522b6"],["/categories/操作系统/Mac/index.html","aad1ac9bc81c92395dda85bf3c454db8"],["/categories/操作系统/Windows/index.html","a7e7cb4a9ab225ee6dae66b86f7fe10e"],["/categories/操作系统/index.html","6b55728d6c3e5e540f56a32a921249cb"],["/categories/数学建模/index.html","19657932a40db35cd177a618909c8092"],["/categories/数学建模/latex/index.html","46a271bad08efa9a6f97580a862a75b6"],["/categories/数学建模/优化类/index.html","ffbf3d34f2bbecde41bb124c3bff8adf"],["/categories/数学建模/优化类/现代优化算法/index.html","a209dbd3b19cf4db9f1e134a27c8c943"],["/categories/数学建模/优化类/规划类/index.html","d9fcddea740bac9162751f7b3345ad00"],["/categories/数学建模/绘图/index.html","c3a402ec54737f0ad487f0bbe83ebf13"],["/categories/数据库/MySQL/index.html","8190ca8115cb94eb64e29b25780e5e08"],["/categories/数据库/index.html","a4a619f524e518fdf9eb0b91dbaa440c"],["/categories/数据结构和算法/index.html","98ade64701c05fc1e500a2e6d530bf4b"],["/categories/数据结构和算法/page/2/index.html","18976cc00ff7d707619a733165fcdca3"],["/categories/数据结构和算法/基本原理/bfs/index.html","d78c0cd9bec9f39f1bb0d0d8c2175ead"],["/categories/数据结构和算法/基本原理/dfs/index.html","7f3b01825fed2aeb7bedc4b0fab9f8e2"],["/categories/数据结构和算法/基本原理/index.html","db84f0f1b318a67851bfb0aac773c670"],["/categories/数据结构和算法/基本原理/动态规划/index.html","5be0d68474e56abc7bf264d4d64f6d38"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a530e60e9d6748c9c59ef353a4c5f105"],["/categories/数据结构和算法/基本原理/图论/index.html","29b5f1eebb72b9175aca6cf33cc3c778"],["/categories/数据结构和算法/基本原理/字符串/index.html","4ca9cec8e5759c799b35f80f5e947de2"],["/categories/数据结构和算法/基本原理/排序/index.html","17d31cd369c40177db69305912af4a64"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","76a8e5344724eca76fab59b799667366"],["/categories/数据结构和算法/基本原理/数论/index.html","4ad23de378c27f956b7a6786373052a9"],["/categories/数据结构和算法/基本原理/树论/index.html","ef5774d370996879797d28466e9ace29"],["/categories/数据结构和算法/基本原理/链表/index.html","7fe47836b950ae4635f1e7df243b0385"],["/categories/数据结构和算法/算法题/index.html","f51ee6f34abae8ef6d634050c1ce944b"],["/categories/数据结构和算法/算法题/二分查找/index.html","059cb66eb9f55ab35934ba3c55ec38dc"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","65c85cc7086d829049280da44fbf5b10"],["/categories/数据结构和算法/算法题/动态规划/index.html","7267621006d1083affdfdadb4f3e50aa"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2b340eef94cdcd0893f6427a23753aa5"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","dffbba9f52370f96d3e693d02fc7d251"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","1ce514c4dcd291c55989d8fd942ebc0f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","0d0e0b44aa8868f255b7847c1ab097d8"],["/categories/数据结构和算法/算法题/数论/index.html","ac9c6d34904beec999902891ddff29b6"],["/categories/数据结构和算法/算法题/栈和队列/index.html","2700b2eefeccbc0216163cbfdbec545f"],["/categories/数据结构和算法/算法题/树论/index.html","c10604ff9e26f31cc25c460442b4b79f"],["/categories/杂七杂八/index.html","2758cd73ff4be5e867bdf9ff9d7fe269"],["/categories/杂七杂八/博客搭建/index.html","b1030084f70ef99b6c114b88162d0ab9"],["/categories/编程工具下载/index.html","01f25aa84d5ccc871a5f7f227ea89c0b"],["/categories/编程环境/index.html","c946caa6dc342591784c4bae26626846"],["/categories/编程环境/大数据/index.html","d4450d9a69d2a0657fd9a5112bf5ff01"],["/categories/英语学习/index.html","4db35538da45e71bdf26364a1e48de36"],["/categories/英语学习/英语语法/index.html","be6ddb110993552acb9e0e1134181251"],["/comments/index.html","3dc72424ca0347f2445d048549a393bd"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","bedfb39b7691852731e99af61cc04857"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","9ea23e81d89c724e05b67361a6b51a40"],["/movies/index.html","9ccf61c9b4c82c89c4baee6a166d8319"],["/music/index.html","5976c5ebb6b3298ad231186ba66c3ca7"],["/page/2/index.html","9ff232973775c66f795a8a621a2a3b87"],["/page/3/index.html","e29f117220e953a3d63324d0ecfd7643"],["/page/4/index.html","54590c7c5d99621b7239a9ca6efaf983"],["/page/5/index.html","4f65dd641bba1d39123af649df10e919"],["/page/6/index.html","7a8e702f88000da3c6526100421412ba"],["/page/7/index.html","e2a60efadd6cc41e9aabd1a2afc4d061"],["/posts/1021360842.html","764c30c47e976bda9da6863a8a41c158"],["/posts/1120620192.html","0f5de62325f2112657494df9eda89991"],["/posts/1137707673.html","eaa78dc29399b2265b5ebd7dd8057987"],["/posts/1141628095.html","e948ed541a661acdc4d44d268b4082d2"],["/posts/1168613674.html","944db807ff51ad7d2354f34e166e33b5"],["/posts/1219920510.html","b329726430842561cd77cfa004d0fa6c"],["/posts/1222166338.html","f75825796391b310c24f138f15189f00"],["/posts/1259097482.html","805f20e3e8e48a3aff02c73c35d0f0a4"],["/posts/1271036369.html","84b396872ec7907f2a059c760fc5d090"],["/posts/1312847445.html","37946394ade898e19153f51526ca4dcf"],["/posts/135355774.html","b57bdf9bfdce7ffb9f065ae24818e11f"],["/posts/1375344716.html","ee3142333927c7e2de8ca71b68669e67"],["/posts/1388991698.html","08315270797bd6caccd79063463fc02d"],["/posts/1410315814.html","1a721dd53fcdef034caa7f4267615488"],["/posts/1452790229.html","5e2f4abc2c8e39964e5c34c3cf082836"],["/posts/1470079884.html","46f64932311abf56aa9c48aa283f360e"],["/posts/1470079885.html","4738206fe2c3aa08253e180ce4f9fb7f"],["/posts/1470079886.html","7405444af9664a207de0ab8ed982a94d"],["/posts/1470079887.html","bae17620ae614c1b84b4beae20eba1ac"],["/posts/1498536549.html","a3ea16b4b88acbddc3060d2a09ac59df"],["/posts/1539568593.html","461bafc9869eb2a7ceb053f7e7be7d52"],["/posts/1547067935.html","8f99d7301bc9cebcba8366e52f363777"],["/posts/1557866301.html","52a1cb7d8305871665f6c46e18d27885"],["/posts/1571776361.html","7bc364982d54d31a12791d2c3b7f5a3d"],["/posts/1605124548.html","ea856f680f542799a0ab8a7e6ffdd93c"],["/posts/1633036852.html","5177bea4ba68dfe6ad2d0502464956d5"],["/posts/1667740714.html","e5cc7ac21925fa7590e2fb2bee6885d4"],["/posts/1674202625.html","e1b49c4b1d0680dd4f99f255d18aae07"],["/posts/1765123828.html","d723dea3e265b81da836d58ee8b67192"],["/posts/1767336200.html","b4da912a88e7a5a689ea1e794f59f25e"],["/posts/1776114197.html","19aa03028dcee544f2263f24e96453a5"],["/posts/1817748743.html","aa0289e724c4e4cfa879428dd94a42f8"],["/posts/1925125395.html","4a5ee4a2d1f1b5cfd4560b61f1816875"],["/posts/1966191251.html","16bbc8ddbc673c3d705fc5ee94f12a89"],["/posts/1987617322.html","79abe4cb07e79182b1103f904172af0b"],["/posts/1999788039.html","d60abb1137840ee24da9cacf911b4cfe"],["/posts/2075104059.html","d851da1ec49b88f096c52f8b7e7d1e6f"],["/posts/2087796737.html","d0781142da1815b0f18f0562990a5591"],["/posts/2106547339.html","daff2b11b983c6a32103fd4394d35b5c"],["/posts/2207806286.html","46ef020656b9d5561ee432c41640b129"],["/posts/2225903441.html","aa89fa9ad9acb9beac647951b63a7bdc"],["/posts/2265610284.html","ec95cc7908fa8db11cb45ccf52e8f95f"],["/posts/2281352001.html","73fd834a1f884ce6887c0961cf3150cd"],["/posts/2364755265.html","3f86849a917ffd44077c89c01a154d7a"],["/posts/2414116852.html","39e5a0b49cc9c8fc922911daa52cfe3e"],["/posts/2421785022.html","11dfdb7fedf252cba4bd330f5aaf6abc"],["/posts/2482902029.html","11a12067659fd09eb0032395b0507046"],["/posts/2495386210.html","90da2992fe484a9d3fa25dd35bd53f63"],["/posts/2516528882.html","c75893f74476d08edbfc5a49d48e52f3"],["/posts/2522177458.html","d6d7324c50d971625bc17f32c39a147a"],["/posts/2526659543.html","c3b496fc0c1cb1fd0d6aa4c1d1b6acf2"],["/posts/2529807823.html","72496c5bd2ca4f8103b6ef922c90aefc"],["/posts/2596601004.html","a060b767d636f295f4298354b8bce63a"],["/posts/2697614349.html","17a27deee0bd6f96eaaa876d5ebb22a6"],["/posts/2742438348.html","b4ff17fa7d4f2ec9a81551ae89f59aba"],["/posts/2768249503.html","ddc16a77c64ef331a9cd5adaeced7d90"],["/posts/2864584994.html","a6148662ece1d1222d17f5f3fbd6136d"],["/posts/2888309600.html","bb43b3696895db9fb814acf450683e99"],["/posts/2891591958.html","004bf3e1b9f2333e7347935cc597ac34"],["/posts/2909934084.html","539c140ab12307731ae96ec556cfa1eb"],["/posts/2920256992.html","34bdd6c32bb78a4cfc7587e3ed9fcbdd"],["/posts/2959474469.html","3f11f1b6a2f4e3c90ef862d7c2480c10"],["/posts/3005926051.html","07f3d7d91ba64f034241b6239552c4e9"],["/posts/309775400.html","2edd2871a0e88a70ff712a475fd4b49c"],["/posts/3156194925.html","4891e6530adeb72dbd9f4d1674294da7"],["/posts/3169224211.html","0306ce6469b53aac5b94e0e50db298a4"],["/posts/3213899550.html","7bc1af210d2a7b623b542818b7efca57"],["/posts/3259212833.html","b7da8e94100b9aa136737884c6609976"],["/posts/3265658309.html","a0d35b5f5e0deb9a13bfbaa09291b3f2"],["/posts/3266130344.html","595c90d0e300a3dfb04a556e25e808e1"],["/posts/3292663995.html","0f17b1f149fe21861154ebac8310c32c"],["/posts/3297135020.html","2784e1f80909666ce2414d5c62fa5f99"],["/posts/3306641566.html","af8156306e50596f0ae872e7c9053c99"],["/posts/3312011324.html","e5e54009aa75c87e7f47996fb5d4fe07"],["/posts/336911618.html","5619d099ae606dbce19fb7f412a1043c"],["/posts/3402121571.html","ec37bfc45ec8873a07d501c0659405f9"],["/posts/3405577485.html","e8009c4e1571103692626a754ddfc2e4"],["/posts/3498516849.html","04d2e402cfa265e8d6a25df792c536c2"],["/posts/350679531.html","02f210ea49c77c081b275f2a00076d90"],["/posts/3513711414.html","ffb2c1c8d88a824c32495e4f3228d826"],["/posts/3523095624.html","c31570dd580886c6f0bab098dcdb506c"],["/posts/3546711884.html","c2f21439e66ab805e94709c350b6a01e"],["/posts/362397694.html","093c172b24e615eb061e9f9482da942d"],["/posts/3731385230.html","a1df2f155f017316cd65423feba74407"],["/posts/3772089482.html","57e7101313933b87e74abe37118fc202"],["/posts/386609427.html","a1627df42d25275bcaa173372b6f3fa6"],["/posts/4044235327.html","35850a582f8421694fd27533151dd458"],["/posts/4115971639.html","99eb39a665882e21aff75e4c1c963c04"],["/posts/4130790367.html","29ab12f0bf3a3de83e66c9a9b1419862"],["/posts/4131986683.html","aa028b990c33cbfceb732b45d4eaa2c1"],["/posts/4177218757.html","04b8d8df08559c69690ddc61383a0fd6"],["/posts/4192183953.html","12c155fde41b87804b71d60d48f78bbe"],["/posts/4223662913.html","e2028b273c0ddc23c1473d18a4ae1da7"],["/posts/4261103898.html","7273cf8c0da364a5654a506da0ef8118"],["/posts/4286605504.html","5405b1fdca189946f09984c9c7a0c8e1"],["/posts/449089913.html","b71504baa24fc56034b57c869465e222"],["/posts/469711973.html","c50418f04e028a0a12039c46ad97057f"],["/posts/482495853.html","edeceddb9c8292d511a93f18ece9ec14"],["/posts/488247922.html","8ac1887e20d8457939d3bbc4d6662045"],["/posts/517302816.html","2bf75ca761898d9207c85d33e53c3628"],["/posts/570165348.html","a31b98472c5ae9e1fd030c5aae9d0b75"],["/posts/595890772.html","43280e5de447559974708289d7e1656d"],["/posts/67485572.html","c7b733f4d0ef466bde8822ec1cc6ff3f"],["/posts/694347442.html","92872a8415738abfd9e2b5bdb013053a"],["/posts/707384687.html","a6b4adbb7a42cf3fdfcbd3f0c1ec39c8"],["/posts/71180092.html","75b5aa67bf85c51153419267ec710d61"],["/posts/716459272.html","d58a6a85a7bbfb2098fd74130e1805c8"],["/posts/765481613.html","4b4b3ad9c8ffbd0769b58ba8e9b9e8f1"],["/posts/778231993.html","32a33b60d9c758e4bca89f0d2b1d5486"],["/posts/795397410.html","8f371fac1af97f7e48e2a97ba8f53484"],["/posts/820223701.html","8f7401d0de9d0e804a16ef2e3c9b84b8"],["/posts/830372185.html","6ac9d6b47b3d63740074783983b36e4c"],["/posts/88294277.html","33d8b996762328fa4dd46d4bdb5bef2e"],["/posts/939963535.html","fc69e6fbb91ee9dfd43100c34ebaa26d"],["/posts/983786067.html","79af2dd3a0ba700e2616b48db959d970"],["/sw-register.js","854d65762b6cb9dd6ccde572964e7fee"],["/tags/C/index.html","2ea551c513edd5fd343c3a0b09734c0b"],["/tags/C/page/2/index.html","a6634465af4d170e0271a698879f6fe3"],["/tags/C/page/3/index.html","21eb6b3a4e8799bd474935070fb8b87f"],["/tags/C/page/4/index.html","19a2f4d42060ad0dc6411786f0e1c359"],["/tags/ETL/index.html","dede94584a2722817c6fa2541497b0ba"],["/tags/ElasticSearch/index.html","86b03275c62f8c942445227fabeff6e0"],["/tags/GUI/index.html","65610ee3934dae8dc93d784d307e1380"],["/tags/HBase/index.html","898324015b35a9d16b875d45e3fc1093"],["/tags/Hadoop/index.html","2e5a839beb9517b95c33a9d94df90cc5"],["/tags/Hadoop/page/2/index.html","9110189dfa8d7b6781749def34c7aeeb"],["/tags/Java/index.html","db196ef8f17af77932da51b3e8de6bfa"],["/tags/Java后端/index.html","ef4c9ac822e74057eb675b8505eb56d4"],["/tags/Java后端/page/2/index.html","2be15753e914e8823b6a1f472bb8c0e7"],["/tags/Java基础/index.html","a1d8dca65880eb5f114079d5929e1b97"],["/tags/Java基础/page/2/index.html","acaaec4ca42766ae5c4ff9149a05cf95"],["/tags/Kettle/index.html","ab203a81181fce808a7b54333815e8a2"],["/tags/Kibana/index.html","c1f3e899edf0e0e0bb5fea8e89825e06"],["/tags/Linux/index.html","07d8723807cf0889c9ff11e01ca06ace"],["/tags/Linux/page/2/index.html","e370f8ba624dda42c798b5dc556dd369"],["/tags/Linux/page/3/index.html","a51c037b504e9b637984e8413f26892e"],["/tags/Mac/index.html","fd34c6fa02e5a1a00125630899edf481"],["/tags/Mac/page/2/index.html","55f6234e0545c8b9ff08c7f7c94d55fb"],["/tags/Maven/index.html","8b5dae1872976bf319a6588db719d067"],["/tags/MySQL/index.html","85894c0a25e8881a839ba0329c6d374b"],["/tags/Python/index.html","074c4e4945ee337cbcf7548a623004ce"],["/tags/Redis/index.html","1d92936ee9ae3b2e3ef37a93ae9026da"],["/tags/R语言/index.html","6674215641e9a6d3686c452bf24303ff"],["/tags/Spark/index.html","76086624f741be5bcd0a5236aa9bd30a"],["/tags/Ubuntu/index.html","92ebba66dc0a28563a076f0713add22f"],["/tags/Vue/index.html","51a38b0977300bf327ebb98d15c08a6c"],["/tags/Windows/index.html","04b1ad34128ad67c3612f38303582bcb"],["/tags/ZooKeeper/index.html","718bd58b2996f07d6cb2937b72cdbeb5"],["/tags/bfs/index.html","68ce12c2e2da81fd2a4fbf61ce68a2e4"],["/tags/dfs/index.html","33c7c584aec9faec65eff3f4ba88ad8b"],["/tags/folium/index.html","cf57e8ba7e8092e5052d4785be4ca211"],["/tags/git/index.html","b16a5c4aff9a9d38d17d0f15b6c0b6b8"],["/tags/iPad找电子书/index.html","f8d0919d027fa5799d8f9b2b36926d83"],["/tags/index.html","9f27dce3a7c83f22e002c10ad76fc19c"],["/tags/latex/index.html","cd0d4a601e2194ce9cc00341e1b763ef"],["/tags/中间件/index.html","accea23fb61d0192c4b893b7903830b4"],["/tags/二分查找/index.html","d89e31d0c8fc294aac8b3b5335b4a699"],["/tags/优化类/index.html","b470d1f0ea03c877b6c07c18f742c8f6"],["/tags/前端/index.html","41b619c71716116b0b671fa48dfc5aef"],["/tags/前缀和与差分/index.html","3e8613ea42507f9842e1f6214ed3b53b"],["/tags/动态规划/index.html","e54492f12b69efb1c5f5d63fa3ce635a"],["/tags/动态规划/page/2/index.html","d6db9a8c07e69f3b0681e0b91d3f5601"],["/tags/博客搭建/index.html","680f15ad3a508dc1608df934c731bf18"],["/tags/图论/index.html","a309da7e551cba6b9e1560ba79861017"],["/tags/大数据/index.html","57bc82547cb67ccdacd3873eed838bfc"],["/tags/大数据/page/2/index.html","b423f78c0691b1830198d6cfc6043baf"],["/tags/排序/index.html","800acd1c4db368e2335c46cbb6d3f8ae"],["/tags/操作系统/index.html","bd417bc3cb4ef034b86a08927ed4cc90"],["/tags/数学建模/index.html","750b95a07cb8ffcac6b59ff312e074bc"],["/tags/数据库/index.html","17bc81f2a736ddec684c13ab48366ccc"],["/tags/数据结构和算法/index.html","a40ff3d8b696f76e35ffd2b1c42a72a5"],["/tags/数据结构和算法/page/2/index.html","2fe1afef4f525c18e89ae587cef790be"],["/tags/数据结构和算法/page/3/index.html","c94e3a82204c7ca78900d5e1552108a3"],["/tags/数据结构和算法/page/4/index.html","bc57059132d280f98c2ac1b8fc6ef556"],["/tags/数据结构和算法/page/5/index.html","ebbfb280858d1ec22d87a582770897a9"],["/tags/数组和字符串/index.html","d24450c0d99d51f2e347e685aef37fff"],["/tags/数论/index.html","f0ede97e1df3400105c9968c957db14f"],["/tags/枚举类/index.html","d14c8076a17bef70c89bc234d22cabc7"],["/tags/栈和队列/index.html","d2bfcf3f2229329a30c76667a742a743"],["/tags/树论/index.html","2477aacbb0089ee7459d32d70def7bde"],["/tags/测试/index.html","2754ac76a90082dd4c2dfcd08adcf188"],["/tags/环境/index.html","6a8921f33dc9e33c4578772037c1d05a"],["/tags/环境变量/index.html","2920365250fa04d64efbe36210c31ed2"],["/tags/绘图/index.html","be7ad8dba39be3199e773454e5bac1d5"],["/tags/编程工具/index.html","99bedca7e20b08a73df76b72ca40b2cc"],["/tags/编程环境/index.html","3fbcad8ce311d55ae1311f0897f89083"],["/tags/网络编程/index.html","1ee30d7d510ab343b3cb3239153e0c92"],["/tags/英语语法/index.html","eaf7c52bac787efd67c835e015a3e3d2"],["/tags/计算机操作系统/index.html","99bdedf02d93c2a538861ff0289f4f3a"],["/tags/论文/index.html","55ed4dc299685d92981da53b87ef0038"],["/tags/资源下载/index.html","94208a6859a70ab75672b0b4604a439e"],["/tags/链表/index.html","abc89a8ddb5f688b18481aa23214cec6"],["/tags/集合/index.html","40dbb4b101531bb3805ce1272ea2f23c"],["/tags/集群/index.html","ac701353f33c0fe2562f795f5a332e47"]];
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
