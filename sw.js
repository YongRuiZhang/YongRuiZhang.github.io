/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","c2ff6c1f6df68abb254be3e446c0f15c"],["/about/index.html","1f8cb8a6e9f1da7b937799ff9adb8639"],["/archives/2023/01/index.html","8b1df42a4004a0c6a380ef93b8332ff8"],["/archives/2023/02/index.html","9d6fb7d92c12b960ca14d6011e0ae843"],["/archives/2023/02/page/2/index.html","82029f91275e6f1d829f0c6e72b40a56"],["/archives/2023/03/index.html","a959f1a3fd30f9174d9493bcbe255077"],["/archives/2023/05/index.html","bc56c818f6e910b242bb07daab1082b7"],["/archives/2023/06/index.html","46879272e08a3cf7d1484fe4ac8db1d3"],["/archives/2023/09/index.html","0985a8b9a81479528955f3d4d1a38468"],["/archives/2023/index.html","2bb187de22fff6dd86ab66655de640d1"],["/archives/2023/page/2/index.html","7bd5835a712ba26f1bfb280a35459dc6"],["/archives/2023/page/3/index.html","0d726d2cbe9bd7348c35d4500ce3e8df"],["/archives/2023/page/4/index.html","ed88c2aceebeb3b10013f9223bd848ce"],["/archives/index.html","6f2b9f3caee176580418f903ad070070"],["/archives/page/2/index.html","56540348084b9e20d0dde34ce27b2d6d"],["/archives/page/3/index.html","d54aae703bc74c43bed7349bfd3623c5"],["/archives/page/4/index.html","986d9a7566341e829edc660320120d81"],["/baidu_verify_codeva-qQP2iZOMLX.html","7837b36b260893b088d3bb6b545adc1f"],["/categories/Java/index.html","ea0503de76ebad967bf3a47b373b4e62"],["/categories/Java/后端/index.html","e77803413dc6ffaf61db27e790697ea6"],["/categories/Java/基础/index.html","4cac18b53d67b74675bfa0a70f19e300"],["/categories/Java/基础/集合/index.html","ae0abdb0d4eccee7966ca0eb6b218235"],["/categories/Python/index.html","83ce8327b949ae9493de4fa1aaaa1978"],["/categories/Python/编程环境/index.html","ea51723eb3e2a8bdcc230d7a00a0cdda"],["/categories/R语言/index.html","84e6e1aefcbacf14c7a67ba6cd66ee3d"],["/categories/R语言/编程环境/index.html","f7b14ebc9f3b37479997ba9ebcf45903"],["/categories/index.html","1a7fb2d50d65911e6adf1a1b3da64c91"],["/categories/中间件/index.html","015f3274fc6d7be18e35029e3f47d8fc"],["/categories/前端/Vue/index.html","dc46ce922357d65863f77cd663189b3a"],["/categories/前端/index.html","1ead0ebc90c90735472dc5f5d330e624"],["/categories/大数据开发/ElasticSearch/index.html","f70c145903aa2105aa827f82d31697b9"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3fcc1f0429bcdcefa7c102b6419022f5"],["/categories/大数据开发/HBase/index.html","57b7e936d14ae6fe4cbd56c69e07f6a7"],["/categories/大数据开发/HBase/学习笔记/index.html","f4c0ed7b2ed3b346df50c0a38441913d"],["/categories/大数据开发/HBase/环境搭建/index.html","d000221f826ae724aa28d05cf644eef5"],["/categories/大数据开发/Hadoop/index.html","deaa597b1ff60b8eb2575283cf1bf106"],["/categories/大数据开发/Hadoop/技术/index.html","b379d12f5f26e3c838cd66e8ada0b3dc"],["/categories/大数据开发/Hadoop/环境搭建/index.html","4930d3fa6537383b6f2e7704fa5b6dfb"],["/categories/大数据开发/Redis/index.html","37e526b08416cc2f3685175f71a3e2c5"],["/categories/大数据开发/Redis/技术/index.html","34cdbef7712ef2430f1f2f73b9fae81c"],["/categories/大数据开发/Redis/环境搭建/index.html","1d1931ae783d23e10581b60140483f22"],["/categories/大数据开发/Spark/index.html","9383269d251136d1b72e45d1ab47c833"],["/categories/大数据开发/Spark/环境搭建/index.html","35c38eaacee8189250902aab8b2994f1"],["/categories/大数据开发/Zookeeper/index.html","891023ff821598e1826a9d28357ec741"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","a25f2a8d5d2b9f790816093ea66d9684"],["/categories/大数据开发/index.html","c96a8c18cfb201d9481bdc39ec2a2573"],["/categories/操作系统/Linux/index.html","5a8937809ec68c4d9ea5f8e0876f4f75"],["/categories/操作系统/Mac/index.html","50d78df630cfdc4fc0b073c4b05bda66"],["/categories/操作系统/Windows/index.html","272b8367568e7f264265b5e7a3036086"],["/categories/操作系统/index.html","d3448138cdff1da6c128232f9f043a02"],["/categories/数学建模/index.html","0c156640c95a20c07a3aec1ed2014057"],["/categories/数学建模/latex/index.html","e4e0d07af12117a1fc9d155a998f363c"],["/categories/数学建模/优化类/index.html","2ff5aaa2dd096ca1e1909911016fccde"],["/categories/数学建模/优化类/现代优化算法/index.html","2d2905a16dc65f2fcf667a6890b19598"],["/categories/数学建模/优化类/规划类/index.html","04ebe1eba2b50754ea4a66458a87599e"],["/categories/数学建模/绘图/index.html","bd3f0d74b09cfe8ec71f286be0df4305"],["/categories/数据库/MySQL/index.html","54ecb7f34366ab737b5b764dc7e9dea5"],["/categories/数据库/index.html","27316c4965cc8cfe0657c039764f2287"],["/categories/数据结构和算法/index.html","78454a050440918414e69cbe0f867be5"],["/categories/数据结构和算法/page/2/index.html","bfd6f8502e122375e153584050f6fb84"],["/categories/数据结构和算法/基本原理/bfs/index.html","e680535e2560cdfecea251707cc0b962"],["/categories/数据结构和算法/基本原理/dfs/index.html","0e6f2a5a7fd47f6a176f7da9a8343c6f"],["/categories/数据结构和算法/基本原理/index.html","41d713590d2c4f7a62a569ebad690383"],["/categories/数据结构和算法/基本原理/动态规划/index.html","af584e414c3885319bb11372a41fdb7f"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","6b8c19f03d166f0cd3b61f21c1da2314"],["/categories/数据结构和算法/基本原理/图论/index.html","64c5d8f4cc6937cd9df13f25e3f9e749"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","0c431cfe56eef8e1b06e5baa82b25808"],["/categories/数据结构和算法/基本原理/数论/index.html","ab65df958eacd8abd9673862a6ec5c57"],["/categories/数据结构和算法/基本原理/树论/index.html","43fa5c6bf554e1f5e7828ee7904034e9"],["/categories/数据结构和算法/基本原理/链表/index.html","d56c5f30962d313b93bdab5b12dfbe61"],["/categories/数据结构和算法/算法题/index.html","3ff231a54966284505558919877b77f9"],["/categories/数据结构和算法/算法题/二分查找/index.html","94202a527de22fc3bf8827d55445f09a"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","9ff9bad8cd3e92e9f7a70a8778148e57"],["/categories/数据结构和算法/算法题/动态规划/index.html","45cd063ec9ea1ddb1ddfd20203e37597"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","ea81f4789f0a14b53c15c3eddf315feb"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","35225e494f0dcc047c187a4acfd91ca3"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","cd64df6003e4cdcc5894f4948721b4e4"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e85f891291a16249c3c003a548f7b286"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b45f8ee0b6adc832ce337bbb986d7836"],["/categories/数据结构和算法/算法题/树论/index.html","ca9dc3953116e3cdf94af0aee8cd5081"],["/categories/杂七杂八/index.html","0eb3a3f4cd5f8c4ce0d6d6ff6ca7892b"],["/categories/杂七杂八/博客搭建/index.html","e690fa3e6ebb4b57402a88ead8003a71"],["/categories/编程工具下载/index.html","649d6ba5dd0661d4ea77ee3f5537e4b0"],["/categories/编程环境/index.html","4f45fac01f784e7d2700e0d0f6ca6491"],["/categories/编程环境/大数据/index.html","1ff2db92fc60baf4ec5d35e52973d718"],["/categories/英语学习/index.html","c108a7c1311d62caaa6072aa7c28ff5c"],["/categories/英语学习/英语语法/index.html","978a93c7fcb6e6b9bf99caaf86b6611d"],["/comments/index.html","6ae30121bd670bb440d02682aa20188a"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","22521f19182b6f2a3c89909f28e98290"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","c9927202800e941e63254914b3c4b2dd"],["/movies/index.html","aa7da38b40e91b981511f34c97fc6971"],["/music/index.html","7204eaf3cbac3406ecf894d64cef403a"],["/page/2/index.html","9d465c41a530ac9428390b875e115fab"],["/page/3/index.html","fbec2ceed1ccfcadd74cffdbf2202fdc"],["/page/4/index.html","3659c5c244ab9ca70aac429763ace0ed"],["/page/5/index.html","c2a0a8afc66061db5c610c51ae4cf2e8"],["/page/6/index.html","243396d3994c4735d5f8eea5ab59772b"],["/posts/1021360842.html","c93e943ab74fa5374439be21d7ce5359"],["/posts/1120620192.html","eaee96c80efbcf249c42ab434731a19b"],["/posts/1141628095.html","f81811d2937f539884411a461937cfb4"],["/posts/1168613674.html","d61506c2d83b1d847ebc7b8d8487f5c9"],["/posts/1219920510.html","64a4f8af90c3c4c82ae2dab16fc93ae9"],["/posts/1222166338.html","16d36c564c9bb1bafe6495efc58b3378"],["/posts/1259097482.html","898ebf7dd5646afe2d41470a95735b6d"],["/posts/1271036369.html","6dc5824e6ce0af4a4b2c5427c4d23abc"],["/posts/1312847445.html","32121a7448efeee56c1583e56a9be3ec"],["/posts/135355774.html","ffef3fecb68ac70f33ff1cceafd0db00"],["/posts/1375344716.html","35fccedfadba5cb573411391d560dded"],["/posts/1388991698.html","b89c59ab3813a97acf0a51e69bc32c70"],["/posts/1410315814.html","970170295b42748c8298505f6e416fd8"],["/posts/1452790229.html","5e028eef1cc54217882fc2ad76fd6658"],["/posts/1470079884.html","95c25d3fe2afb73f827de0867c54c159"],["/posts/1470079885.html","f32d4a56bce6a511a950c166dc83b3fa"],["/posts/1470079886.html","0855ccdbf54096dc184f9a79622ab1d3"],["/posts/1470079887.html","a9c1d5946fff2e8c567a8046e32667fe"],["/posts/1498536549.html","51c17686a9f0ce45e25ef7873b41ac2a"],["/posts/1547067935.html","31bc8a3cd796772b6621d64fba524f22"],["/posts/1557866301.html","c61d1c24705069daa1cdfb070ee7823c"],["/posts/1571776361.html","c0cc2f1fd557932e5bcf0853d2250e45"],["/posts/1605124548.html","5fa9c0fc28c2f9eaf4a2938df33b2f6d"],["/posts/1633036852.html","1d3b0d59bbfb1d44d1107e3d5a695ac4"],["/posts/1674202625.html","3fbd6393a9392439f21464b12d3579f9"],["/posts/1765123828.html","128f346bcb98e05e9e21979bab1373ec"],["/posts/1767336200.html","7f06d10899bc2a3cf40f6156d9e5d294"],["/posts/1776114197.html","9494820c576a650cf54843d79c33a75e"],["/posts/1817748743.html","e4acb41f1f1bf1011109183d248c2282"],["/posts/1925125395.html","6d85c1bf15b4e1bf43f7cd00aa44ba9d"],["/posts/1966191251.html","b88dea8663d9effc10e8688d2ae01236"],["/posts/1987617322.html","5df3e4872087271f4bee81d18f8b366b"],["/posts/1999788039.html","aec3673eb0c64e61de2e30617172bd27"],["/posts/2075104059.html","86f1c97df4b0283d967905e380c05bbd"],["/posts/2087796737.html","455bb63bec01866b0ba4d244429307bc"],["/posts/2106547339.html","f909747288848aceba8aa45bcd9b751f"],["/posts/2207806286.html","c965be4497d4b34c7f2fb40339dad581"],["/posts/2225903441.html","6906454bf4907b085f7ae837b7f954be"],["/posts/2265610284.html","8a207adb6a5d9901bcaa38a2e92af257"],["/posts/2281352001.html","9815aa71c0f88e31a68d807aa15aa15c"],["/posts/2364755265.html","f221587df588b26ab0fc91e8349baf2d"],["/posts/2414116852.html","351ea5f68f0cf28b4cfd90255c0e0651"],["/posts/2421785022.html","dc188bdacf733425acd53bb0fdca6169"],["/posts/2482902029.html","6f0d0e8f47d7d8a278b7f0e13ff7f82e"],["/posts/2495386210.html","1c30e2e62db30102c6f40684122316f5"],["/posts/2516528882.html","4d70de7e3f9ef9eb57e2e951008cea56"],["/posts/2526659543.html","06273e4356676ab18ad4d00846a6b4b6"],["/posts/2529807823.html","0751393958932d11f8c64bc791ce1edf"],["/posts/2596601004.html","5d9a90e421429eaeb4137c03de01f2af"],["/posts/2742438348.html","88015b9cd14b90f90391474cf568ea1b"],["/posts/2888309600.html","6aefa699343561a113f89ce98ebe428f"],["/posts/2891591958.html","058184232245bd7b4a19edbdcbc4829f"],["/posts/2909934084.html","81f08af2f76c888f0c96b8e07cc15549"],["/posts/2920256992.html","98123d8960d34ba0e4713e3bb7401023"],["/posts/3005926051.html","6ea6b791a7204e6100c926146c20482c"],["/posts/309775400.html","670151ba18fce3baf03bafee1d737d20"],["/posts/3156194925.html","034e978df0f03b894c7ff1685ea0e4c7"],["/posts/3169224211.html","2670061068f7a4fcc7448b707bae6ef5"],["/posts/3213899550.html","88091c734e47635e21c43d5b50eace2e"],["/posts/3259212833.html","59154ad9ad13a92d77fb68984d50dee4"],["/posts/3266130344.html","3417535a44ae1dddf6bce2748194bd68"],["/posts/3292663995.html","402bb5b9d4b75f3d4c8c74bdbcba2548"],["/posts/3297135020.html","2d0172510b6425ae2d9bf1393d88b7d2"],["/posts/3306641566.html","92f762bb539c4a744e99af3f5a20033e"],["/posts/3312011324.html","6ef403d60162faf3563cb87b39a0b558"],["/posts/336911618.html","a8d98b9a82ebe654fe580a426ed39951"],["/posts/3402121571.html","ab29e99f94dd9c30d7d29ede82e83786"],["/posts/3405577485.html","61500aadc65ba7e619b2936a46c46da3"],["/posts/3498516849.html","db1ece225a9ad8e098b9c0de655c9992"],["/posts/3513711414.html","8e53d2494187a2f28bf2830f93796765"],["/posts/3546711884.html","751cba68889415a157573b132a7f4b09"],["/posts/3731385230.html","7c986ea627dfd4263d252e586b8e057f"],["/posts/3772089482.html","feefbe08431ec9dfad243d72d51f3f00"],["/posts/386609427.html","bfb774421731d19ff47752546150688b"],["/posts/4044235327.html","9b17c915dacb0cb268120e65ba9eb96d"],["/posts/4115971639.html","9982b42f366fc9ce3ddcd9c4ddb65c42"],["/posts/4130790367.html","55748a496ce4a4c9d1c7342dab51e57f"],["/posts/4131986683.html","853f47114f73a8bf6f25e846b8f442d9"],["/posts/4177218757.html","0f7e9c7d4c945e4d7e04107d2f911f0f"],["/posts/4192183953.html","97b60d65ae9d768a1855896ca8637177"],["/posts/4261103898.html","46207ad5a52af6cc4a1d4a9fc71dadec"],["/posts/469711973.html","c431dab78445884884d78e4bd992ef8c"],["/posts/482495853.html","03c9b755384501f119fbf4506f958b80"],["/posts/488247922.html","562e6585ae4259dba2fa89acb0751305"],["/posts/517302816.html","92173f2f6af22df2d9a60a8f08099439"],["/posts/570165348.html","54ca4e3e2dfb1e05ff84637e4d75eda8"],["/posts/595890772.html","3f340f89490e8929c377e938f109bc94"],["/posts/67485572.html","92ac4306ca1edb76bde84c35f6a75cfb"],["/posts/694347442.html","9d3c037c15d1b9391b58cffb642e5846"],["/posts/707384687.html","d431bac15b3063f2d4c0b315c570ea8b"],["/posts/71180092.html","2a7ec5778dab8489501a96ffd9dddb9b"],["/posts/716459272.html","bd7a0f99922c939d15aa8fcc7ce8ec8f"],["/posts/778231993.html","d94874aba6b9cb14128b4b53efbe6f46"],["/posts/795397410.html","e966afbe76d6957d129734c00ae9f80e"],["/posts/820223701.html","a6b7c20dd07a97c26ae9fb58dacfac8f"],["/posts/830372185.html","d3348040d1cf24db5cfd89f53090ce6c"],["/posts/88294277.html","0077103f2a22bfd006054a0fe1a18eae"],["/posts/939963535.html","15fe64e84c7be7c43826592c3ee409a0"],["/posts/983786067.html","dcf8a6b7761e62443e78c2f03ad8ec02"],["/sw-register.js","4621a633d9346cbb41fb03e52e7e0bc3"],["/tags/C/index.html","d1e98c7becd5632328151b4ef03566ee"],["/tags/C/page/2/index.html","aea65550f615c2666ab215787a08c7b5"],["/tags/C/page/3/index.html","418c96b07c79af6efdc93fc3b56e9454"],["/tags/ETL/index.html","f07a966c5a399401f79fe6f8b1100988"],["/tags/ElasticSearch/index.html","e7c7c9cabe8356a4b33060de25a35435"],["/tags/GUI/index.html","d903026d23a9e4387a23016178be4948"],["/tags/HBase/index.html","b397c7cc2d54245b85861cbdea4c3d47"],["/tags/Hadoop/index.html","506a4c7e3bbff29d30bc0e8e4e3ed03c"],["/tags/Hadoop/page/2/index.html","7f13dd582a753bc7b48de651734c7382"],["/tags/Java/index.html","e519e3eb33087d62077ff0ab01bf9e3f"],["/tags/Java后端/index.html","9ea88e3fd26b882d12dcfd35518da510"],["/tags/Java后端/page/2/index.html","1dc4305c79f202349375d099e025de2f"],["/tags/Java基础/index.html","c16c462127af27bcdb3817fcb3e5d526"],["/tags/Java基础/page/2/index.html","3881cf0a48fde69903e387e9edb78bfa"],["/tags/Kettle/index.html","872925210331d625fbec3d7523362bab"],["/tags/Kibana/index.html","23de41190998b203b7a060ba2c761473"],["/tags/Linux/index.html","67e84987219f868e1c1a92a7e441237d"],["/tags/Linux/page/2/index.html","c743521ee4e816de33e8a48c1bd3b9bc"],["/tags/Linux/page/3/index.html","b1caa213a7677801d25ba606cf2df369"],["/tags/Mac/index.html","77c554feff6500482bb34181478cbf75"],["/tags/Mac/page/2/index.html","63e3636479a6016a85f0fe231914b067"],["/tags/Maven/index.html","7304786ebfbf7717c6aa5dfe5e606110"],["/tags/MySQL/index.html","d2c04e34820afc46ba41f11c2a68c54c"],["/tags/Python/index.html","4aabcfea95628acafd7229f7b93f0c69"],["/tags/Redis/index.html","f6b67dab1d6b25746d2825a333fbbf1d"],["/tags/R语言/index.html","cb5ff2b5cafcce1d26ea906549acfaa7"],["/tags/Spark/index.html","90f2f5ff817aa7ee839e0477aeb6e126"],["/tags/Ubuntu/index.html","56cc4f5811c48eab5f2faf57f1ce8c81"],["/tags/Vue/index.html","733e543729aebe31dd2053e0f0d78490"],["/tags/Windows/index.html","2d26bb70dd7a3ce06520431658138e61"],["/tags/ZooKeeper/index.html","7bc4670de5b7f77ebfda0c0b78527368"],["/tags/bfs/index.html","a7a1cc83d140940c1e24912a5bff2f40"],["/tags/dfs/index.html","2144e2c309b114a70ebefadb42f23327"],["/tags/folium/index.html","052cd86e629d9a3b4ac73db493ae6d6a"],["/tags/git/index.html","3c7bf554c9a383eacadd24b0a8f9ac52"],["/tags/index.html","3617f87540e9a9ee3f10f6ec72c48d17"],["/tags/latex/index.html","392db1cccd3eb6e4b584eba1faf3fcca"],["/tags/中间件/index.html","f25d82dc2675e8b7b86664aa87bbb22e"],["/tags/二分查找/index.html","2a8671e8728e3fd32a9ddcbef2728954"],["/tags/优化类/index.html","c187879f68c1f5590ce0eadd339c7242"],["/tags/前端/index.html","e6bcfbbd2be29427718a7d68e2eb2168"],["/tags/前缀和与差分/index.html","bac8e2334bc97d265cda784933cf7495"],["/tags/动态规划/index.html","deacf8948758de54b732983a7cebc6e2"],["/tags/动态规划/page/2/index.html","a57ad295820f13abb703362215c7882e"],["/tags/博客搭建/index.html","3e53394d3f61dd517e67e9ff9ec5d7a8"],["/tags/图论/index.html","4ab49750680587d9936190548295c87e"],["/tags/大数据/index.html","c05f3852e8c332ad8137354cd585e6f4"],["/tags/大数据/page/2/index.html","608cdb96f678fd8c82e932aa6816d022"],["/tags/操作系统/index.html","8e58139365cbeeee7f570d8f33c77780"],["/tags/数学建模/index.html","14c73b42bb344d6b0e9052a50b673654"],["/tags/数据库/index.html","d9929efb8cd83faa6f89cd1e453141b5"],["/tags/数据结构和算法/index.html","a419874acb1f99f9f8a9ff501d57a456"],["/tags/数据结构和算法/page/2/index.html","faa5b50722c8c7d15802bf1fbd8eccf0"],["/tags/数据结构和算法/page/3/index.html","1db383ceecbecac9f46dbc8b9189df7c"],["/tags/数组和字符串/index.html","dba672fa6af30c1dd1bca986f4ee2df8"],["/tags/枚举类/index.html","db9d56268743d60f19a298cb56879f4a"],["/tags/栈和队列/index.html","c9484a8091d20a13067b4bdb78d89c9a"],["/tags/树论/index.html","99852c9b0138fc720db46725cb904b0b"],["/tags/测试/index.html","c7b3b0de1f2b721a5b3b87e2ba2ba831"],["/tags/环境/index.html","51e376fa840e85a026471905b46096e7"],["/tags/环境变量/index.html","addf6fad1708cf82f5aba45180b836d7"],["/tags/绘图/index.html","21e3a716e34ed026afd35811c123961a"],["/tags/编程工具/index.html","c53c6a39f93afaaf30671a6f77d93795"],["/tags/编程环境/index.html","22cd7eb43cb100a9f1b5eccdda326f7c"],["/tags/网络编程/index.html","e97586e43c8ffceb6cecaf0973081289"],["/tags/英语语法/index.html","8e0605a17409df79e30a656b9837eebd"],["/tags/论文/index.html","396970d6e7da56f450ee052ece115a49"],["/tags/资源下载/index.html","ab595ef6d16d4bd6262f8b7a890f8aad"],["/tags/链表/index.html","0b9a94d5f5426553b443e790819fee7b"],["/tags/集合/index.html","a0767b62f5e55b8c9f2252d23edeed6a"],["/tags/集群/index.html","3543207a39739b19035849ce7bfb2074"]];
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
