/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","3fde51ed0ffa45c34a44cdb4140f986a"],["/about/index.html","e2a5e468d238f8d585a54ce03f598432"],["/archives/2023/01/index.html","a91870220c93295905dca9e25e4eba8e"],["/archives/2023/02/index.html","001e28c5b73304c1ecd629423817e2a9"],["/archives/2023/02/page/2/index.html","654df65ef9bd49ac373d1be27eceb8d2"],["/archives/2023/03/index.html","a99b304574f571fb18b4136e0d157c31"],["/archives/2023/05/index.html","537ea2b580f1413c43bdfe180cb1b6ff"],["/archives/2023/index.html","2bd7304c5f707d99578e5c2c225e70a8"],["/archives/2023/page/2/index.html","f911505e8c7e1aabc654fdef0f5290dc"],["/archives/2023/page/3/index.html","2a966111d596a37d1f462a9b8e18925a"],["/archives/2023/page/4/index.html","c0f20fdf991c412cd5b6b1796d990972"],["/archives/index.html","5739a20aab7a70778af493c06dee82dc"],["/archives/page/2/index.html","f1990ade5935ca4c3892c26ffa56d93c"],["/archives/page/3/index.html","0aff05dc31542b67ce4183e904bbe03a"],["/archives/page/4/index.html","21ed51e8b4dff964affef4dea2db2c05"],["/categories/Java/index.html","1b0b7d4c531cca411f45b7a96c28b064"],["/categories/Java/后端/index.html","b46c9a1afa35aa12d91f688820d262d1"],["/categories/Java/基础/index.html","bd60536d2112af16cc52eb67a3122240"],["/categories/Java/基础/集合/index.html","1cd390549057408146bcdfd167c5f717"],["/categories/Python/index.html","6fa63413b96fd6a1e5130df8e7d5bcd3"],["/categories/Python/编程环境/index.html","bb4d20c9899078793dc790d9ded23968"],["/categories/R语言/index.html","7b13d90164cae19d793eee71a3b1e282"],["/categories/R语言/编程环境/index.html","44c8345a3636ddaf0a7a7f605be91ec5"],["/categories/index.html","25e48f5bdbb812c6c83545519a1fc65c"],["/categories/中间件/index.html","34e3be570654baba972761962ec81503"],["/categories/大数据开发/ElasticSearch/index.html","5431a7101922859cc712c157f682c2f2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","996f93b4976025cfae7de904ea10c00e"],["/categories/大数据开发/HBase/index.html","29d8813aaf8a43200e6428687e9ef110"],["/categories/大数据开发/HBase/学习笔记/index.html","8bc62c19c21bd5de2ccc9cff618f726a"],["/categories/大数据开发/HBase/环境搭建/index.html","2aad688875dd4d023f7953e685b0b241"],["/categories/大数据开发/Hadoop/index.html","581a947240e57c6a15e4b43f4915f40d"],["/categories/大数据开发/Hadoop/技术/index.html","802ee7692f939ab82b8afcd242c13d25"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a43b98d2cfb0ed2d739c47ac0086b448"],["/categories/大数据开发/Redis/index.html","09b32f6a6f50cf43525e779c3af2f07d"],["/categories/大数据开发/Redis/技术/index.html","54a3796dabcefa2b8d6f6d482c0ed1ae"],["/categories/大数据开发/Redis/环境搭建/index.html","118ff8db4d76b55c0f3b4000f7cb7a55"],["/categories/大数据开发/Zookeeper/index.html","2cb2da36fae70d62bc8c219c56ce51dc"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","910582436b8c4cabf89b1315ddbefdf7"],["/categories/大数据开发/index.html","39d687e57ce864f853fbad596c488ac9"],["/categories/操作系统/Linux/index.html","b7e5b416dd83d8afb5191b6f7465b3f7"],["/categories/操作系统/Mac/index.html","c52cbb1d67297259efb2e676b3af37e3"],["/categories/操作系统/Windows/index.html","258d07d5adf987f1b02acced3f640aec"],["/categories/操作系统/index.html","c8fda38e22866a5d4a790791846b7570"],["/categories/数学建模/index.html","bd6297fab523030f7551b288d04ad0ba"],["/categories/数学建模/latex/index.html","41a69da101a2b21805ff2a0282078d99"],["/categories/数学建模/优化类/index.html","85dfca4091951f69198b78022f60f8be"],["/categories/数学建模/优化类/现代优化算法/index.html","abe6183254a6ac6b86ab8ccfd234072f"],["/categories/数学建模/优化类/规划类/index.html","8a797f3e85e512e9cd19c4038cfb3071"],["/categories/数学建模/绘图/index.html","93553504ca98966608591274beeeb37d"],["/categories/数据库/MySQL/index.html","e005c8b146c461ea2400e9164e49dcad"],["/categories/数据库/index.html","82acdddd34ade84ff7b452dd380ebbfd"],["/categories/数据结构和算法/index.html","09bd479ec97f3eedf47bdc14fec8b1cd"],["/categories/数据结构和算法/page/2/index.html","3614eb619408911a77a021ce2c632b00"],["/categories/数据结构和算法/基本原理/bfs/index.html","de26878ffb680a79e043956419aa6517"],["/categories/数据结构和算法/基本原理/dfs/index.html","d4d2cbb85d122cbf57fbc4892d34bb3c"],["/categories/数据结构和算法/基本原理/index.html","3d0d2d10a886ae337acc165d6cb73032"],["/categories/数据结构和算法/基本原理/动态规划/index.html","05e73dee91d2d4144b7cbd21d6bc9da9"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","27cfcb091411e409b7613afea534e9fd"],["/categories/数据结构和算法/基本原理/图论/index.html","9f2fcb855c6f64246338b612e1e12f35"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","df976fbb38537af624c256e65a190e89"],["/categories/数据结构和算法/基本原理/数论/index.html","be29abc00c2c05ade060cdc0adb8faca"],["/categories/数据结构和算法/基本原理/树论/index.html","0616c6b921510d1525014ccc981376a6"],["/categories/数据结构和算法/基本原理/链表/index.html","f9f9f90fd87ebcbd7789bbec1e30e9ed"],["/categories/数据结构和算法/算法题/index.html","c0e19af72ff9cc74cb498a6c28b4cd4d"],["/categories/数据结构和算法/算法题/二分查找/index.html","157cf876f8709b9601eabacb3c1d05d4"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","baeedcbba98c5c1f02d43ac0bce4ae02"],["/categories/数据结构和算法/算法题/动态规划/index.html","22a206d88a78dd1f29fa82715488a65d"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","34d7f0797514f1d6bbd69313fb41e8ec"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","71c913c48871e4958d86267a6dc794ae"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9faf5dc0b243641778eb467f1be9006d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","599662a0fa1aecb534c9fd37216bc346"],["/categories/数据结构和算法/算法题/栈和队列/index.html","e7271d738a8e27b7654432ebb8496031"],["/categories/数据结构和算法/算法题/树论/index.html","71b81b574097f5aac7d51adeb99fd5da"],["/categories/杂七杂八/index.html","3f7dda4e7d3f828720bcd21d636b4935"],["/categories/杂七杂八/博客搭建/index.html","298bd3cc9791314a86cc36d4aa2cbfca"],["/categories/编程工具下载/index.html","f499c3281c9e50fb09d5e4f4231dd930"],["/categories/编程环境/index.html","5947eface3a4a334e69411aac6ed017f"],["/categories/英语学习/index.html","edbcd2c80e32bc15573b92553f348bb9"],["/categories/英语学习/英语语法/index.html","b60db4aa44a199659c8f7ab691270e1f"],["/comments/index.html","7cd81a5c3c3af298751abd3f5c7bbc76"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","3dcb1c96b46db877b15e008aeb7a8728"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4e7ed1be38f48f6225a52970242819ee"],["/movies/index.html","a71b6fa0b94c4c2c9c16b0b8c53b3808"],["/music/index.html","29653855173afa2b26ce5b8b5e59abe2"],["/page/2/index.html","daba58118d8610be5e5198ac579e8dfe"],["/page/3/index.html","944be8cb0e133a97de02e39a6e6f4010"],["/page/4/index.html","de57003b88a58fb400a9f090517264a9"],["/page/5/index.html","3a90565913d496751ebb16b96ebd296b"],["/page/6/index.html","6f54cb29a167a79b4bdc5fa2795a8320"],["/posts/1021360842.html","a486ab64caa13c4bf6b7d2d3e3554d71"],["/posts/1120620192.html","d486e0e18fce80eea78a4c9ec31eb567"],["/posts/1141628095.html","939b3d9a7f577abebd0fcf23982566cd"],["/posts/1168613674.html","7a942cd862b913f284f68a1dcc5b2bf6"],["/posts/1219920510.html","4e1f16308c9708e685f05ed301066a75"],["/posts/1222166338.html","f1c70b86b69d03932730a1a8becb775a"],["/posts/1259097482.html","4c42940d12bad9bc59f544c803688492"],["/posts/1271036369.html","56b1226c7afebc7479edeb941bb93611"],["/posts/1312847445.html","1f73de3d6d6ad77d79c44dc71f6d160d"],["/posts/135355774.html","d9266f49f2c19ac5870f24c13323cd46"],["/posts/1375344716.html","6a8a42dc5cfe9bf6277f70edfdef1188"],["/posts/1388991698.html","22129e78a3e02a121c109e5c3572abe3"],["/posts/1410315814.html","da833c0ac012a40cdf6a995d202ce0ac"],["/posts/1452790229.html","168bab825419123188e6d787cd8911b5"],["/posts/1470079884.html","a10786ba3335be45c6051368e8fc6b1e"],["/posts/1470079885.html","8dbb96ea9d7d62a329e63e7686c1bec4"],["/posts/1470079886.html","6e3158b17faa4ea76d5497e57a331c67"],["/posts/1470079887.html","8544de4a5c31cf9ecd7a7a20ad26c984"],["/posts/1498536549.html","03c592787114901db57f09f15b7672ec"],["/posts/1547067935.html","2ef52c8fcf71a57f251403b583711822"],["/posts/1557866301.html","7170f730e7b9f2b6fb75e147e51ce9f8"],["/posts/1571776361.html","c687b9d239802223981109083d7af55d"],["/posts/1605124548.html","a3d90fadc8f4dfe4fad5960c44ff798b"],["/posts/1633036852.html","f09837206b5f05ea6be2584c9604dbe6"],["/posts/1765123828.html","3a92c22bbb5f289eac6062da6a6e3200"],["/posts/1767336200.html","b99d52b315883e22c431eeec8c4534e7"],["/posts/1776114197.html","4913feaf7d1db12b6addea858b0ea6b4"],["/posts/1817748743.html","66a0a703c929ed6cdb1896bae0d436ee"],["/posts/1925125395.html","362c7e5c9b6ef1eca512a2189b03a5c8"],["/posts/1966191251.html","ec6b860943918449e71108e91ad95de9"],["/posts/1987617322.html","5f5b80b634fd102b059543952916cb66"],["/posts/1999788039.html","5a4c78e507e4111d7035b0f7a6b7dcbb"],["/posts/2075104059.html","00de89664314dfcaf1579fcd3f1535bc"],["/posts/2087796737.html","8dc81c8d96b0db7d83a4692146e26eb6"],["/posts/2106547339.html","1cb2642f32f5a09f4a714b04f0f2e90f"],["/posts/2207806286.html","76ddd807c5798f0d5564a07500d7fd81"],["/posts/2225903441.html","d4dc84a12def095cf62de72c74b3e79e"],["/posts/2265610284.html","6bff6fb773d5261a75a32cbe7155bf80"],["/posts/2281352001.html","140bfd2c5236a98863ecbd7c38e437c7"],["/posts/2364755265.html","7c198156b4f712ae123272be539fa3d3"],["/posts/2414116852.html","149ba65e5eeb8cedee1893958007e5c0"],["/posts/2421785022.html","7572cd837a61bc650bf8a864bbcf3062"],["/posts/2482902029.html","4722ce33b7db7a544cbdaa1f73f6d384"],["/posts/2495386210.html","f14b5b7e4070be604a0e5bc2fc797f7d"],["/posts/2516528882.html","f54e6ca8aff9316611395d23d50aad77"],["/posts/2526659543.html","ace44011a5d38fd12aaaa706e7aff9ff"],["/posts/2529807823.html","4f9a0ed27f3d493ef13afc36bf190cb2"],["/posts/2742438348.html","3b5ef81fedc121326799027659ac57d0"],["/posts/2888309600.html","eec85894bb57f64b13416f00918cb3e8"],["/posts/2891591958.html","41fcacbc328028e050718fa8ea195276"],["/posts/2909934084.html","59105fe3cebc1c78f1c73535d392688d"],["/posts/2920256992.html","cb192a46dc06083e03ecbc6ffbe45663"],["/posts/3005926051.html","18a700ae4282c77418d4f7ed7e5df686"],["/posts/309775400.html","3bf6378837835a02fcc089617cffef55"],["/posts/3156194925.html","dd6d36fa65e9a9c63129532612dd54f0"],["/posts/3169224211.html","cafa472582ca9972dcf39b9b6f3117f6"],["/posts/3213899550.html","dcfe872fd50c1b773578352d7fd7158f"],["/posts/3259212833.html","de944f83c111a274300d67eaab7ff358"],["/posts/3266130344.html","ee0a459aab9ea73b0e977131f0053470"],["/posts/3297135020.html","7781af90e37a5c1d9f9c0dd6f535f6ff"],["/posts/3306641566.html","bb57acaba7bd7ed4cdb488a2fb6d3b85"],["/posts/3312011324.html","ffdfe25c6a39b193575092ecb2b0ae25"],["/posts/336911618.html","38a733a84404b5cbee99fc187f7bc196"],["/posts/3402121571.html","64140a60bc7958a28bd53b6cc175ca8e"],["/posts/3405577485.html","7cca6cfa90c15d8aa6c649265af3cfce"],["/posts/3498516849.html","c883d13e29adfb49c619ce9c2e378c1d"],["/posts/3513711414.html","9d6ff356caf05f9ae888cfa59db97afb"],["/posts/3546711884.html","d5984ce55a0589d6051c450d3c651dd4"],["/posts/3731385230.html","f3852c7c868b532e2bdce758f9d45c5a"],["/posts/3772089482.html","ee0840407cbeb78a18a0e283924e4966"],["/posts/386609427.html","ddeb13fe3ee4b91bc81b7ca46d5ba985"],["/posts/4044235327.html","718d01a3a4a5eb061fa75089cd0c2465"],["/posts/4115971639.html","443b726fc6811487af69e8726928b775"],["/posts/4130790367.html","27fcf1c571b05e2aa9218b8074b87524"],["/posts/4131986683.html","a90ad426d66dc1866ee881a406c6e410"],["/posts/4177218757.html","cb5d50b81659b9eaa2ea7d5b20e86396"],["/posts/4192183953.html","80058222e496a57486e9cbf330daccce"],["/posts/4261103898.html","2b606e96b314380982808a7cf00f46dc"],["/posts/469711973.html","4254e9cc951da678a7f35e76e5dcba09"],["/posts/482495853.html","ca790f10d84c24e3ad7fe170b8045ea6"],["/posts/488247922.html","48aa4c72245184a1403a40dc029c974c"],["/posts/570165348.html","b9d38674c8c38a8065efe5972d3b5d1f"],["/posts/595890772.html","18f02d4e42d0f62adf3ddd57bfc1e5a7"],["/posts/694347442.html","e943183dda76f248afef64df364aa38d"],["/posts/707384687.html","d866bd65d26d45129e5002bb4d5b0ec5"],["/posts/71180092.html","cec46bcb2b904c22c5294eb2fcdcc352"],["/posts/716459272.html","090e373c5621ef41e77df6daefd79dc3"],["/posts/778231993.html","569402d76516eafe89ccff6f682802d8"],["/posts/795397410.html","13a2a07bac98380c33fe0ce9e45e9c07"],["/posts/820223701.html","010b72d393b69e451ad33c07cb12e490"],["/posts/830372185.html","eacae46b688bf09fba320f13e8104d46"],["/posts/88294277.html","df1677866114f94c4358d34885b7df83"],["/posts/939963535.html","31701191b8107d1f80b963f283a6cb24"],["/posts/983786067.html","950ea6a4b4b03a5fc1ee997bf787eed8"],["/sw-register.js","c773785914ed3cea169c2c29bdf47924"],["/tags/C/index.html","3d843d2330e0c9039e519a0e3187d53a"],["/tags/C/page/2/index.html","f5d89847ff835ffaf6a7d11f87adb7d4"],["/tags/C/page/3/index.html","610948b570df7274efef0f02a541a226"],["/tags/ElasticSearch/index.html","0c221901739374da13ae6593f0688ba9"],["/tags/GUI/index.html","929565cc534d813cbb32d549accd2f5a"],["/tags/HBase/index.html","03f05bce28b0695abd488efe61ed3fba"],["/tags/Hadoop/index.html","a26020de8f741037bcd8863d725e4ac9"],["/tags/Hadoop/page/2/index.html","191f7205953b3d66cf727104cc9ce764"],["/tags/Java/index.html","8764a10923eb09e3c967f54d05d13b8d"],["/tags/Java后端/index.html","804b078f1d733896a2ca9068eb026c16"],["/tags/Java后端/page/2/index.html","a62dbcf1a1a7a12abf9a96fb1a0055ce"],["/tags/Java基础/index.html","a7e2a66f17a180c346dcaa5a04832e14"],["/tags/Java基础/page/2/index.html","c9496760778f10f9a04cbfe7605462d9"],["/tags/Kibana/index.html","5acc040f9f4e2a2941f5413518a3577b"],["/tags/Linux/index.html","f932acfa33d1ac81c0dd338662caf51e"],["/tags/Linux/page/2/index.html","fbff97a7d22818604ea4ac32d2a3558b"],["/tags/Linux/page/3/index.html","8d4659410885ad9b8114ab924b9ed7a6"],["/tags/Mac/index.html","bfe802822fc2074927136d626cfd4df1"],["/tags/Mac/page/2/index.html","134d8eafc713120a72f6d97e6e3bb129"],["/tags/Maven/index.html","637f6788a36424c517009a23f4fd0ef2"],["/tags/MySQL/index.html","914e2127cd171ed8762423c8ab74fecc"],["/tags/Python/index.html","f73b44c6681626b48ca92c0c92dda99a"],["/tags/Redis/index.html","decbe21fa73c1766519e461667ec71ae"],["/tags/R语言/index.html","3629a6e97a51d7038b0f657a93b52507"],["/tags/Ubuntu/index.html","35d225997c2d1be18eeb93bff2c4eb18"],["/tags/Windows/index.html","f9195fe53dedd1aa4f1d04ecd2d7b72b"],["/tags/ZooKeeper/index.html","218ed74f81de1a7094e9588ab367db98"],["/tags/bfs/index.html","4b1cace605d74dd830d0900886bc3833"],["/tags/dfs/index.html","f3d00089322607714d212ca1b6af9f15"],["/tags/folium/index.html","d1fb8fe952ea0d49c49ef174f56a23d4"],["/tags/git/index.html","83cf518b29c8372e6748ec3d4449c118"],["/tags/index.html","c1402f783450502edc5973f7887ca501"],["/tags/latex/index.html","6bde14787200323049710e2fff1d84c2"],["/tags/中间件/index.html","ac30e92788d9cb9dd4d1aa6c8ae406fd"],["/tags/二分查找/index.html","c95580f8a0dfd3aec4577485e3054768"],["/tags/优化类/index.html","63573f48adb50a21565df690ba144796"],["/tags/前缀和与差分/index.html","803e1e3f93b035ef00ca91dca46afb35"],["/tags/动态规划/index.html","cc3d0dc0e9b19ab2135e6bcf8565e503"],["/tags/动态规划/page/2/index.html","03987a1ce19aa1f14d98978746b96a96"],["/tags/博客搭建/index.html","679f163424c17cba2cb6aae9fe484366"],["/tags/图论/index.html","abf7cfed27b2074975671e5937870bdc"],["/tags/大数据/index.html","342fbc6219d0f48f801faa36a3e8a6dd"],["/tags/大数据/page/2/index.html","81956e415c1967aed574ef38ba2bcde3"],["/tags/操作系统/index.html","577ec486da09e430f991f7b668b522ab"],["/tags/数学建模/index.html","8bae1342bebdd9da235251ada70c1c3a"],["/tags/数据库/index.html","e2fc0c4264d6ba7362567dc7869e2e7d"],["/tags/数据结构和算法/index.html","89112db002c84a564b6b1fd26476693a"],["/tags/数据结构和算法/page/2/index.html","6bcdbaddcbe74cd7895bbe0bbd61b363"],["/tags/数据结构和算法/page/3/index.html","f65d14d5f580db99019b87298a7a0444"],["/tags/数组和字符串/index.html","2bb75f2d0fb59a68f157ee22bc824237"],["/tags/枚举类/index.html","9920d4e02497168131bf2e29e7aa8424"],["/tags/栈和队列/index.html","346d2d17be00f96963563697531ac55f"],["/tags/树论/index.html","0081d15d453fc4c36b8eada1648bb2ce"],["/tags/测试/index.html","e594a4725ae30afaebf6c97bc491a14b"],["/tags/环境/index.html","d26e00dba738e48baab0e3ef6414d9dc"],["/tags/环境变量/index.html","e713775d1ef12316e557f41973537124"],["/tags/绘图/index.html","5708cde1d3a2d3bf19a10e92de61d783"],["/tags/编程工具/index.html","06a8f664a70b317767a21694f0b951f0"],["/tags/编程环境/index.html","072841364c12847590325c043bd077b2"],["/tags/网络编程/index.html","941f0cea0708c2401bfccfacda10b6ce"],["/tags/英语语法/index.html","56bf5caaeb7845ea32e42eb3c8b476bf"],["/tags/论文/index.html","817ebc74d1f240d749f71b7c396ba3c9"],["/tags/资源下载/index.html","54fc144286f6bed8be4ec3c58464e0b5"],["/tags/链表/index.html","102e374c4a6925be50443d89672bf84d"],["/tags/集合/index.html","6770d5775d8446691fcd301a6abccc52"],["/tags/集群/index.html","2a19d97df0a59a66ce4bf12ae5bf99b2"]];
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
