/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","5ac4fdefa07808e6a7aba44402c7919a"],["/about/index.html","dee010e6bc4255b3ff9a1c090b96aa90"],["/archives/2023/01/index.html","939f80f2b9b140ba82bdbe68fad25782"],["/archives/2023/02/index.html","d26fcc807a986d2526de925fdb01a2e3"],["/archives/2023/02/page/2/index.html","22d2fd1b95607e5e2eda997c85de13ec"],["/archives/2023/03/index.html","0b56fce7121d445d159eed7a5bb820d9"],["/archives/2023/index.html","964e5c7a9c849aaef76cbf20f8bbbf52"],["/archives/2023/page/2/index.html","c9f5651c596ab67bf806257768112e60"],["/archives/2023/page/3/index.html","15e400e070e5d7ae7fc176f2772508ab"],["/archives/2023/page/4/index.html","f4f78c339c01d3f21119019373550c04"],["/archives/index.html","a0b225a7b9b11ee656b1bbb02149b35a"],["/archives/page/2/index.html","85aeb8577bcf9ac3a056fa1aa54d6b69"],["/archives/page/3/index.html","7397405b1e67bc6d60ed315b76f1ac19"],["/archives/page/4/index.html","90fbc5feecb4b19324f860686d9082c5"],["/categories/Java/index.html","7cd04a85f743108797f2569bdd81b635"],["/categories/Java/后端/index.html","b0ae540df9e40211b72f75b383687a8a"],["/categories/Java/基础/index.html","846bde7a2ba2a02996f6424aabed6729"],["/categories/Java/基础/集合/index.html","5dd60e05e2a1d1abfa3a30086d980349"],["/categories/Python/index.html","2cb55710b10c550719fe1f4091f9daaf"],["/categories/Python/编程环境/index.html","8294b3d3597875f733486ce405c903d6"],["/categories/R语言/index.html","9f85c5485c15a1ba23ca2fff36b5d373"],["/categories/R语言/编程环境/index.html","f4efda2ae9500827331476c7eb0f23dc"],["/categories/index.html","35e5c7b1a1432d4789adeda8d34a5ace"],["/categories/大数据开发/ElasticSearch/index.html","1671ac5cf877de177ef030de6a8f374c"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","0e936731e85f60407828e3887d231c76"],["/categories/大数据开发/HBase/index.html","47e22b28210b1b730984ce89c2533234"],["/categories/大数据开发/HBase/环境搭建/index.html","d5f98e3baa2ae1fde8e94085b90f1077"],["/categories/大数据开发/Hadoop/index.html","a188e6c2bfdeac3888c70b589cb2831b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","26f4986ef62ad763c23a6b4cd5615c2f"],["/categories/大数据开发/Zookeeper/index.html","93d7c85342ea17afde23f65982668c79"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","39b70703bf0b94b14001c3b74228678b"],["/categories/大数据开发/index.html","f71144bb2a4fac6668853f925e262f7b"],["/categories/操作系统/Linux/index.html","569973541e0e362aa6beffe7e95ef869"],["/categories/操作系统/Mac/index.html","f1489d2f76e5e899cc2a6c727c385cf7"],["/categories/操作系统/Windows/index.html","9af0468842f44aa2a817c51e641fe872"],["/categories/操作系统/index.html","aa356c881cac965655942c58848fcc8e"],["/categories/数学建模/index.html","d51dd6db821836a5ecacf55d81efffe8"],["/categories/数学建模/latex/index.html","521133864aebb2416325123e89337166"],["/categories/数学建模/优化类/index.html","c59a01a56933aa116c08a02de506b9c2"],["/categories/数学建模/优化类/现代优化算法/index.html","8e4c6305bd481559128177a1e3a606d5"],["/categories/数学建模/优化类/规划类/index.html","2b56260720f1e5028e2a670981eff5fb"],["/categories/数学建模/绘图/index.html","5470db1913027a5639313ee2931b728b"],["/categories/数据库/MySQL/index.html","d7832c7e5c1c106013bf92b23a109306"],["/categories/数据库/index.html","c2470fb1dee62a502220ef03ca37a700"],["/categories/数据结构和算法/index.html","ae35abdd584e89b2b21afacef9ddf7ed"],["/categories/数据结构和算法/page/2/index.html","bc8ac68e83ac2637aea629776d9e8b3d"],["/categories/数据结构和算法/基本原理/bfs/index.html","3e6ac555c963d0ce37054c37b7df3b61"],["/categories/数据结构和算法/基本原理/dfs/index.html","a9f0edcfac26f1d19fc74ebedfc1154e"],["/categories/数据结构和算法/基本原理/index.html","c971e7dbf8ff2bdedcb2486c338a6009"],["/categories/数据结构和算法/基本原理/动态规划/index.html","22f6d8bf1a403cdd4ae2e01fef687406"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","89807305f8ec322dc7d4f2e3678e582e"],["/categories/数据结构和算法/基本原理/图论/index.html","f69c01dc51b6e042018346858f853eb2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","c30d8d2d9185311d4d14c4b95f64e65a"],["/categories/数据结构和算法/基本原理/数论/index.html","81e876b30ed4700143b8f6f657d4c34e"],["/categories/数据结构和算法/基本原理/树论/index.html","837253c041be3e60f15a2ce663859550"],["/categories/数据结构和算法/基本原理/链表/index.html","1f39bea6441333758537690963d8c816"],["/categories/数据结构和算法/算法题/index.html","315df0495619f25e16fed98d9be05eae"],["/categories/数据结构和算法/算法题/二分查找/index.html","56abe4b2298d5f1078408d163c2a06eb"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","24172a451ea8f6d496cc58a6d54755b5"],["/categories/数据结构和算法/算法题/动态规划/index.html","193a2b5a0fab178486601fce6d7105a9"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","71ccc39d064f5f5cc80c0e5f42427cfb"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2a98367c27a4ff592dac42ed762509bb"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","bd01e68702c269a7560fbc091631e896"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","07e144253a9da4997ec422f0cef93296"],["/categories/数据结构和算法/算法题/栈和队列/index.html","65f50cc82b2e0572bd269b27e448fd41"],["/categories/数据结构和算法/算法题/树论/index.html","e5ec65087e4dbf3b5b22ce5aaccae3fa"],["/categories/杂七杂八/index.html","4143355852250927e2e1eccd6310f03d"],["/categories/杂七杂八/博客搭建/index.html","179f6413e438a1aa80a504011aa1f98a"],["/categories/编程环境/index.html","ed531c1ef7268d6f8bc3366a75830f1f"],["/categories/英语学习/index.html","1b28940e5acf1590d37716b4216de808"],["/categories/英语学习/英语语法/index.html","7d9f2683890d8d3f36468907957925c6"],["/comments/index.html","f7eb701c182409068b8f6c512405839b"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","470225721130db0336e85e3200d87835"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cdde30e72d32bf7cdd6a967ac798ff8d"],["/movies/index.html","ec8562b12b13a4f1a3ca2d2592258e23"],["/music/index.html","00be7159885098194fe6b3dae9ec5716"],["/page/2/index.html","3fd26a255a59ec1fffd85dfc358c1c2a"],["/page/3/index.html","85c1b4bb0e66330ed7a23f538e7d44e2"],["/page/4/index.html","f1e5793ee563bddaa01a297fb96e3e70"],["/page/5/index.html","b9a8fe45a7286247d03b1ad33d3724e6"],["/posts/1021360842.html","a64b931593d5edfc0a501fa391a0526e"],["/posts/1120620192.html","15b60a0aa4d295daba062bd7b5e47343"],["/posts/1141628095.html","23a52bc2ed220c533e1c8fe4334802e0"],["/posts/1168613674.html","136e9508f20f91745e3e9635b10ab722"],["/posts/1219920510.html","d0cfba1325309b3b97b5d9e1aae672b8"],["/posts/1222166338.html","a0f4578a0f9a192e7905440c69972ff1"],["/posts/1259097482.html","251647acc768ede5581d5e3b6b142fd8"],["/posts/1271036369.html","f9e5d8fee2dd7da9a2cdc22bb15395d9"],["/posts/1312847445.html","beef7d50f79f87c3c123501969e68661"],["/posts/135355774.html","c6ff7519c9735a308349f1b157a604ae"],["/posts/1375344716.html","995fc45fded9a1dd78f04ee9c3c0d66f"],["/posts/1388991698.html","a6a39d4034b8ec84c24d10f8e02f7ae7"],["/posts/1410315814.html","0276ff761da1dfb41f8c2a3e4566fb08"],["/posts/1452790229.html","d1d7e73b7cbc8d5e09cf578017d59397"],["/posts/1470079884.html","d8c110837f16162b78efeafc6d0d0236"],["/posts/1470079885.html","30447e7001a48737c4639dac489fa99b"],["/posts/1470079886.html","7521945753b4a3d08354c1c05a65c0dd"],["/posts/1470079887.html","36d43394be8e975bce8e6a7986c297c4"],["/posts/1498536549.html","f4985f122626607fff31b3ccb598313d"],["/posts/1557866301.html","8eac5b79bbe1463f72e7a0b276635f3e"],["/posts/1571776361.html","9b173ef08398e2928087c76286d2570c"],["/posts/1605124548.html","b620161b5095727395ee089891f60ed8"],["/posts/1633036852.html","bf52c5eb24f068a1ff3f4dabfb473165"],["/posts/1765123828.html","94e150072b7f0506c120316711670cdd"],["/posts/1776114197.html","a508857a27520a1172f60f89974c647f"],["/posts/1817748743.html","d81ea97bbd78d8f0c0bae74cbda2136e"],["/posts/1925125395.html","4be8240485a3225eea091424daa57525"],["/posts/1966191251.html","28379ef29ddcbfafdb418fcfff49e8a3"],["/posts/1987617322.html","c3e0ec3f8c6366a17e608e4dd76ecb47"],["/posts/1999788039.html","1956c1d3f6d7ff2d308dc023ce673ebd"],["/posts/2075104059.html","a6479dc4fa8a50e6e4d0e8c3cfcaf6eb"],["/posts/2087796737.html","6acc79a7a12fb7b9fbac3d3aaa03d6e6"],["/posts/2207806286.html","2504abb542cdd66ad08b80b754e4672c"],["/posts/2225903441.html","98df374e710624edda3d55757acb67e0"],["/posts/2265610284.html","b98914727937d0ca9775682ed0c03ae4"],["/posts/2281352001.html","33456afd87004a07e475b39acd58bcc2"],["/posts/2364755265.html","43954290ae3eaaa3aacef476abceec9e"],["/posts/2414116852.html","3b29add0562f5db905b52b404588ed55"],["/posts/2482902029.html","4d5c01b6991af6f4ed35d901be01bec8"],["/posts/2495386210.html","8a8320b32bf08148bab17f32db548576"],["/posts/2516528882.html","daa229246c3c3730d190e04b205e39e0"],["/posts/2526659543.html","ef68a39ef96e5665a89683c3e1ebbaac"],["/posts/2529807823.html","68a4934e8796b81bc7e3fdbf325337fc"],["/posts/2742438348.html","6ab0eb2475438eb9b83439d4a87a9253"],["/posts/2888309600.html","15ab0b3f9baa1a1230284e023057725b"],["/posts/2891591958.html","448c8949bd0663c1f5f1d1e449d0c719"],["/posts/2909934084.html","7b3676c364d524bb4d02707c88935fde"],["/posts/3005926051.html","d2794404109de4bef86150c5bd8fbc4f"],["/posts/3169224211.html","a78e2a6f037a4364ad232193726dbe6a"],["/posts/3259212833.html","bb776056d3312c7d3880c1784d5e5526"],["/posts/3266130344.html","c5b27c894d29e56e2c798515ecd506c6"],["/posts/3306641566.html","d58c6461f53495707e63c92d2fce6c75"],["/posts/3312011324.html","70b5a5609c17d2b75d51f44cb99eb4d4"],["/posts/336911618.html","6e0e448ab43d725a161fc1c059ad90b7"],["/posts/3402121571.html","745b8837871f5013acd137710c7eac0f"],["/posts/3405577485.html","e7c76295a5fd550dfd9a8d677e7b5a78"],["/posts/3498516849.html","219b529992049a8ba8e18f3a4ddb52e3"],["/posts/3513711414.html","ee8c584502bf8d81165a34e268b4db38"],["/posts/3546711884.html","5fa13dd386594e7737a10af9898e0643"],["/posts/3731385230.html","2992216698f7e2284e62a761b6fc006d"],["/posts/3772089482.html","aa593ffebfa8e14fa2ef913d1f51e191"],["/posts/386609427.html","d0bf6626e3c4850091e22af0088c3984"],["/posts/4044235327.html","4434dd9ff39621e25fcf663797fcca52"],["/posts/4115971639.html","afb6e94c5c15984448085b08241dff7c"],["/posts/4130790367.html","3c9a080b3eb83f2984fd22fb0d060ea6"],["/posts/4131986683.html","6c7dae687f88e22d7334dfcb9c5158ff"],["/posts/4177218757.html","b5081468e514bb263922b7f312432c46"],["/posts/4192183953.html","d1709f0f9e712d26b4b13400b9f519ef"],["/posts/4261103898.html","db0fd93600806ec4e3913fe32f3d7a1a"],["/posts/482495853.html","0e0a7891543c343e23e21b3e79c0c0a3"],["/posts/488247922.html","54c8db7de31e6d1f51241c4a72eaaa5a"],["/posts/570165348.html","6e008c5ac46f6ffebd62dd07f94a1bf0"],["/posts/595890772.html","7df38e77430c5ff4e8ea29468f62bdfc"],["/posts/694347442.html","adcd74a45a0c3ac045b2cb014c4d60ed"],["/posts/707384687.html","50c99b6952bfed041d28136ea7b545dc"],["/posts/71180092.html","2321f91690c3a2540daaa73f753911bc"],["/posts/716459272.html","cc553ab098d1369c98253c2f709fe79d"],["/posts/795397410.html","cc5c7acb2c43601601f0c9f909e55415"],["/posts/820223701.html","0f60dc2041618c6f049509a9c8b83aa8"],["/posts/830372185.html","6c17b78e7578f9e4494ec6436596c656"],["/posts/88294277.html","e4625fe1a31313ac8a823cf6f21ea435"],["/posts/939963535.html","7bb1660407be455f61d5fa549bb571b4"],["/posts/983786067.html","61e9f8040415f67cb4314687873171cc"],["/sw-register.js","b79ba956b239005098b8f69773f42e4a"],["/tags/C/index.html","bd2fcd765b8111b1fa22ee4f51b12b83"],["/tags/C/page/2/index.html","0e043c1142e37c9005a27634287b6012"],["/tags/C/page/3/index.html","a2e7137e5627a4d5b4c0c44176e595d5"],["/tags/ElasticSearch/index.html","23ab4947838b3a057a5d8dd8b888c07c"],["/tags/GUI/index.html","84618e4ef9619547555525fb193bfd0b"],["/tags/HBase/index.html","a63f5ac13099b07524143ebb82212022"],["/tags/Hadoop/index.html","49725ce0082aa1b5de9e5cfdff192c61"],["/tags/Java/index.html","9e34ecb8bbe3e5572dbce690f5bf1405"],["/tags/Java后端/index.html","e2af7ac69e6f5464e8e0b454bb217a7a"],["/tags/Java基础/index.html","280c6aba52d16ae29b04c18c0403e658"],["/tags/Java基础/page/2/index.html","2b0bfe28877ec96eb90195467c4ce455"],["/tags/Kibana/index.html","95d8ac8ad9ec3b1816cca8954031d1ba"],["/tags/Linux/index.html","27f08b444ce1117e1dbf5129b081e6e2"],["/tags/Linux/page/2/index.html","e20fddd27e1d3b925056c6749f447e21"],["/tags/Mac/index.html","0cb469c7f5a7838ea9f351b0003c092c"],["/tags/Mac/page/2/index.html","d135083b1c8b387024361a8b362ea50e"],["/tags/Maven/index.html","ee18e3b48315a59b3a014477a962ee85"],["/tags/MySQL/index.html","bf81eff2719ea8e4150ef5d7bc136c01"],["/tags/Python/index.html","379453c7e5c962353c86940fe648597d"],["/tags/R语言/index.html","6d7d1b960b6e921d3543508f0d98c4b7"],["/tags/Ubuntu/index.html","17f1926be28cef7c599d1c767e7fbd8d"],["/tags/Windows/index.html","b12aaab1596e374201ebd3ea779e5cdc"],["/tags/ZooKeeper/index.html","ea9800824804e4b5797d769eb1a07681"],["/tags/bfs/index.html","a29e8ddf2e6d38725be2c46d36b843ab"],["/tags/dfs/index.html","11fcad74583232e38d38b91fbe6eccc2"],["/tags/folium/index.html","128bfe570b1f71d77827e50a9f5b75e1"],["/tags/git/index.html","5641c441447d8dc2783128f4958e77f9"],["/tags/index.html","cb8260a222ae339dc325d6adc0d417e8"],["/tags/latex/index.html","25f21699785788d76119651b16d295e7"],["/tags/二分查找/index.html","23046cafb953c9075b90a3eda7f35249"],["/tags/优化类/index.html","c46e3a6de6191318dd35c1b2be191ae4"],["/tags/前缀和与差分/index.html","6f9c5f283853d06da3bbe9a4f536cdf3"],["/tags/动态规划/index.html","4ed3b91fd0e96cb80bc891e5fd9c4db3"],["/tags/动态规划/page/2/index.html","39e77db191ddafaaf8695947cd2a23ce"],["/tags/博客搭建/index.html","d16f7b63e5fb7451da60d1663f95811e"],["/tags/图论/index.html","f1e057215c6feda772b93eb5d9717de1"],["/tags/大数据/index.html","8d07169c2a6863462722eb524fee0f5f"],["/tags/大数据/page/2/index.html","83f0ea636d5806918c333c8dee9caa2b"],["/tags/操作系统/index.html","3af30ccc4f3648b90b9fc44e6ae167cf"],["/tags/数学建模/index.html","7c50eca4ff1a87dbe5d92ae28bdc040b"],["/tags/数据库/index.html","47658993e5602e823c4e07b3c149c9d9"],["/tags/数据结构和算法/index.html","647a1b6e844956727fbbdf786316cd32"],["/tags/数据结构和算法/page/2/index.html","8529761d09fff8a4107a783ec8a92a62"],["/tags/数据结构和算法/page/3/index.html","2a28819ae2b25142d1afb2c55a6e9dbc"],["/tags/数组和字符串/index.html","09a95f3e95db7650acee721f8aa6bcb5"],["/tags/枚举类/index.html","4a398c7446904fb1f7976ce4cbadce93"],["/tags/栈和队列/index.html","814a69a7f63f107e610dca95703a76ab"],["/tags/树论/index.html","6e3ba6b53f00e899a456b7bf13d89b18"],["/tags/测试/index.html","f6fc6f13fde73ce2d1d75c776c614d47"],["/tags/环境/index.html","72b553b654b0d8ae0bcb3c5753aafbd6"],["/tags/环境变量/index.html","16a45f63b33ff13222e7d5bc1196878c"],["/tags/绘图/index.html","cdae1119833b5f3bf542fd69539758f7"],["/tags/编程环境/index.html","59413fdcc6c1454e5b3a6a67ae99fc7a"],["/tags/网络编程/index.html","c78adb4eb5ba5de9d69a4c39a0e2616b"],["/tags/英语语法/index.html","255c4be93aaf937b3eb579a1962ae66d"],["/tags/论文/index.html","2c5cb600d99dd71ca205b5a128c1c8d6"],["/tags/资源下载/index.html","293fb9a3f9e4c00518c4530b9a043385"],["/tags/链表/index.html","e7db71565dea62cec014562ebb4f1404"],["/tags/集合/index.html","b7f373f89e58259cc680c26e8306466a"],["/tags/集群/index.html","8faefef8ecd189c923c7982fce045e64"]];
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
