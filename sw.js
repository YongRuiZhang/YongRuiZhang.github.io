/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","67624edd1ca2c225d53013399b6f2c7e"],["/about/index.html","dee010e6bc4255b3ff9a1c090b96aa90"],["/archives/2023/01/index.html","6e6f3918f0bab85b819f7266af3f561c"],["/archives/2023/02/index.html","0cb2901ff3406a4e9d96f95ce8bdbafc"],["/archives/2023/02/page/2/index.html","62d18c7062fa164f7161ce90a99b522a"],["/archives/2023/03/index.html","fd4ec08a6e7e2a46e2b8729e90104247"],["/archives/2023/index.html","89866d086e66f994f8b4b69fb3a55c1e"],["/archives/2023/page/2/index.html","cfde3e6d5209f9f3c68e0b8259e25322"],["/archives/2023/page/3/index.html","cc99cdeefc39da3b89619aa022dbabb0"],["/archives/2023/page/4/index.html","708401ec64328c8b5a0e44dd9e19cdc1"],["/archives/index.html","3768016e6f8d08e9f624766dde776d56"],["/archives/page/2/index.html","06dfbd66a9bf76295f62c6ef29433a5e"],["/archives/page/3/index.html","bd3cde66c446d6a978d0926ebd4c39b6"],["/archives/page/4/index.html","17960ce8395d02904883fb32551c26c1"],["/categories/Java/index.html","2c665bef8d4be268364c51fededd7687"],["/categories/Java/后端/index.html","d38334bc20077615200ce621385571c5"],["/categories/Java/基础/index.html","8361c8588ea4307cfd623f3ba8cc78c5"],["/categories/Java/基础/集合/index.html","f925708b66a9eae6a205f7705254f14d"],["/categories/Python/index.html","3eaba88f17c3d86f1415ff6cb0e695c7"],["/categories/Python/编程环境/index.html","83985fafd7996c9f2cf19d100016e89f"],["/categories/R语言/index.html","1b22d678ea03e524d71fff0463213f72"],["/categories/R语言/编程环境/index.html","f98c972898aa5d700ac74fbe5c6a4a89"],["/categories/index.html","ef4f80b221e1fe5b2fee5bdc5c275b98"],["/categories/大数据开发/ElasticSearch/index.html","cc32356c8fb4b98f0733a86b9198dfff"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","a6e045ff2b85b0846cd94e36de331351"],["/categories/大数据开发/HBase/index.html","8c62eb680d77bd5e2db70cedd603ab36"],["/categories/大数据开发/HBase/环境搭建/index.html","5c91278bf92e36dd4cd3dde09c7a327a"],["/categories/大数据开发/Hadoop/index.html","0af0d91444f3afba915a76ef5796a87f"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a0e53510ffc33d6fbf93214a9376447d"],["/categories/大数据开发/Zookeeper/index.html","af0cbc5e297e76830cc4f1a05202d19e"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","7cd2209daee96156107eab945491077e"],["/categories/大数据开发/index.html","536c94fd2fecd008a7196e63b629f9bd"],["/categories/操作系统/Linux/index.html","67a5470c815cfb50e9c134ad79e114cd"],["/categories/操作系统/Mac/index.html","67d57af91451974751b76d722481fade"],["/categories/操作系统/Windows/index.html","8e77c6d6a336b938edccc5ec0037e19b"],["/categories/操作系统/index.html","bc050c6679e5050658d6c522b285ad50"],["/categories/数学建模/index.html","05aa4c418d74d9a402e8c17a4c34ed01"],["/categories/数学建模/latex/index.html","9b3c0f43cbaa196a08921e5f8b2e6f8e"],["/categories/数学建模/优化类/index.html","8915f22d4581a8ff8a3e1665e72d218a"],["/categories/数学建模/优化类/现代优化算法/index.html","c464ffecb658a0f28bb4f571e100b722"],["/categories/数学建模/优化类/规划类/index.html","b411cefba04a35ed5a8e8cf2ff06acbc"],["/categories/数学建模/绘图/index.html","03f9a3b615120d086db01bc7ff7f30b3"],["/categories/数据库/MySQL/index.html","efdd0f818c11d5332908c95ff695df95"],["/categories/数据库/index.html","b4917e03af94c7e67e9763acc36e6a51"],["/categories/数据结构和算法/index.html","04ff12cf47e1c9ba59bd93b03810d74b"],["/categories/数据结构和算法/page/2/index.html","82a330aa6ff3c39358fa9723e7864960"],["/categories/数据结构和算法/基本原理/bfs/index.html","c052896a562d32198ed09da0c3254b8f"],["/categories/数据结构和算法/基本原理/dfs/index.html","abfa6ce52fda26f02d20568bb35bd012"],["/categories/数据结构和算法/基本原理/index.html","c2bcb7cf7f74223adfd7e1e3d17e8f48"],["/categories/数据结构和算法/基本原理/动态规划/index.html","5d247afe4aa77e1e8c2b38eb7e76f207"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d90cf0117c76cbcafcc54349d22e473c"],["/categories/数据结构和算法/基本原理/图论/index.html","cd44fb0ba229e52406859fb74df5fdce"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","51b91a1f9555f8730b7bb2b871b200a9"],["/categories/数据结构和算法/基本原理/数论/index.html","e510b692ed133be22f2bf4f9fe47751c"],["/categories/数据结构和算法/基本原理/树论/index.html","a21b33f735c71ca96d5972bbe17c6def"],["/categories/数据结构和算法/基本原理/链表/index.html","4e70ab5c49b7f802602e006a3dfe42b6"],["/categories/数据结构和算法/算法题/index.html","4110e9717012cc81c6de3fbb01f09510"],["/categories/数据结构和算法/算法题/二分查找/index.html","f4781c35b45d283ccdcb8f216823868b"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5ef9a692833076bc2260279b99e6c5a3"],["/categories/数据结构和算法/算法题/动态规划/index.html","0d7c88ae804e61926625258d5e142f3e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","03975b7126a0620904d53648af6d927d"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","cfdc99481c37935ee5a9114d696ed6d1"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","07956324ba3baf6e04c27dfc6d446920"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d541e063b5250ac52b85cc2c68ba3dff"],["/categories/数据结构和算法/算法题/栈和队列/index.html","37bd412187739ccd937e7894be3d9bed"],["/categories/数据结构和算法/算法题/树论/index.html","c20c0beadd7743f12e45e83b92acd020"],["/categories/杂七杂八/index.html","bacba0cc459dfdea227d16d66c107fc5"],["/categories/杂七杂八/博客搭建/index.html","6c72fb0fe9a4a59970b774341799512f"],["/categories/编程环境/index.html","163c2507f222fd0f5a59ba0ff7479190"],["/categories/英语学习/index.html","3dc57a954509c0967fc6dcc98243a5cd"],["/categories/英语学习/英语语法/index.html","3fec28e6d1e4335a552070fb85fc2cda"],["/comments/index.html","6a7a5588afc5a4cdba6043c3aa3c91d0"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","becfe5594f77be25dfed68f59428f73e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","70a9a1dec2409fa7a0ac4e420a0e4259"],["/movies/index.html","202d51c835396c13050b65fe3c091d6b"],["/music/index.html","63bd0518ab19980ba481128e9d7a6073"],["/page/2/index.html","6cff2b677b39afc0c448f1195e01d0eb"],["/page/3/index.html","84410473d16cd34a3d30f0c33965f550"],["/page/4/index.html","9a9e7f9308b90ab8a245e6d0ebab6e73"],["/page/5/index.html","6467b847a5116d0dd690e0b992671705"],["/posts/1021360842.html","18e1b6c672837f3e865cd706f9b85aa5"],["/posts/1120620192.html","8148640f9f7270a5e0990cbd116c565e"],["/posts/1141628095.html","dc93345d247b05baa7ca97cb488fe63a"],["/posts/1168613674.html","9a6f68cde74fbd30283a036994ec8c00"],["/posts/1219920510.html","b9cbfac54d0c393b11c1aa4dfdeee7a2"],["/posts/1222166338.html","f999152cfc24c85f333df22c644528bd"],["/posts/1259097482.html","aee67d553cab219cb2f352594624f894"],["/posts/1271036369.html","748918ac58958aa1fd9c099c5bf5b6b3"],["/posts/1312847445.html","e6c2c5ee5dd79fed6a61e435c4b51f95"],["/posts/135355774.html","38cfea5fe939c514ce17103345a51ff5"],["/posts/1375344716.html","ead9bc27a380ffd259cdf04754454355"],["/posts/1388991698.html","17caa2edc0f7537668b1b549e527c103"],["/posts/1410315814.html","b14a0d112b9c4026077712b5cc2fcc40"],["/posts/1452790229.html","dc37a579ec00503a1668888e8f27bccf"],["/posts/1470079884.html","0c081b05016b3ddbc94b6d96a0beebab"],["/posts/1470079885.html","10a5d0d117da1c992828c29537941e99"],["/posts/1470079886.html","b710abcbf0bd5895526c16967f908380"],["/posts/1470079887.html","ab23849138eefddca6e4f34b19683a23"],["/posts/1498536549.html","1310c4136d547bbecac2cf9eeb7d0baa"],["/posts/1557866301.html","50121bceb58dcd29b9d2aa8183404edd"],["/posts/1571776361.html","6ad5ee8d99b0e9597c86156a684f9239"],["/posts/1605124548.html","faef229833d458b624378390c68a8408"],["/posts/1633036852.html","48e567b5b06f79b14b5b19ef47a8b4bb"],["/posts/1765123828.html","bdcf68290b995aed69957a735c5b7219"],["/posts/1776114197.html","a33aa121473c9ad85ad81fcc768faa7c"],["/posts/1817748743.html","033b91e0dc14913d898ded0b18137991"],["/posts/1925125395.html","d5854f912995989ebf26556cdad5e952"],["/posts/1966191251.html","7fdf3c8a53f26321d123a721593f838e"],["/posts/1987617322.html","780198f7c10f2276f1ebed7f3a5afaba"],["/posts/1999788039.html","a5827ed3c00b727266c6c0f19f97cd3e"],["/posts/2075104059.html","0fb18a3f96e7f0166231d7b14ad9fa66"],["/posts/2087796737.html","359dbcf66beaf09b137b2fae1e30b042"],["/posts/2207806286.html","70e02b1ca400cbe4e0fc80282648d9cf"],["/posts/2225903441.html","fa6fec7414811b63c6e7e3d97fff3caf"],["/posts/2265610284.html","a5e6642c92eeb32a18577a2bd4d394a0"],["/posts/2281352001.html","b0e28336332cc6bf3c6239b07a6803d9"],["/posts/2364755265.html","ef64b02d329269cd15f02b86de63945c"],["/posts/2414116852.html","6999a80b133523721fffa00f0e3d1963"],["/posts/2482902029.html","d46095cbaa2ef3ed5436868bb7e6f488"],["/posts/2495386210.html","391808145250f582c2cf18d1e58bc56e"],["/posts/2516528882.html","24e0e4948f45c032011d86af71096cfd"],["/posts/2526659543.html","fef8b5ce1df4cb5df9df7d9af33871a1"],["/posts/2529807823.html","773b78622bd625dbe3ff9fce8a9e7799"],["/posts/2742438348.html","3bac5cdfc1aa4afc61888d4eda095a75"],["/posts/2888309600.html","6e659f32c8643745af06d5e04c1c6135"],["/posts/2891591958.html","60b42774d8e62b3fadd1264a3facf378"],["/posts/2909934084.html","6a38dc883f8e070387142b051485d373"],["/posts/3005926051.html","e99d87edc8d9b325411bae89aa00158f"],["/posts/3169224211.html","bb1e03ea018dd6a37f46f0f5780d661c"],["/posts/3259212833.html","64a6315919e53f579e94625a8ae5f238"],["/posts/3266130344.html","3525d1e4ff42deec2f879f3ab15b9ea5"],["/posts/3306641566.html","a19def0e95b454ae860bca7b8b5b2a52"],["/posts/3312011324.html","9100f97fb72ae99dca0e5e8228448613"],["/posts/336911618.html","f8e7704a6e8120eb37a0702a214da181"],["/posts/3402121571.html","801273e84fbd45201d0f9fc43ed3fedf"],["/posts/3405577485.html","a98ef9d8f6143b399e7fa9341b3e99c8"],["/posts/3498516849.html","3495b10148651f1e66fc93ffeb593be3"],["/posts/3513711414.html","b64b6eb34e96b05e0e90fdcd7f635ebb"],["/posts/3546711884.html","27bd8223ddc58d732498d23a343a5e75"],["/posts/3731385230.html","08c4da0ae2393c54064fd5fa5b9097cc"],["/posts/3772089482.html","d1a5614aa3c762410308f8d27d3b07bc"],["/posts/386609427.html","2b55470a215e563e8db9a72bdb04eac5"],["/posts/4044235327.html","af56070409a9da3254aac57f68973037"],["/posts/4115971639.html","54965a7f4542d6225c74711bb045370a"],["/posts/4130790367.html","60d54a2dc03d5dc22a66f1d102203bb1"],["/posts/4131986683.html","987da2709279929b162c12f8bc9acb56"],["/posts/4177218757.html","2479fa2fae9fc8cfba83706e0500eadb"],["/posts/4192183953.html","14ead79f89fab107c21c630dd71fcea2"],["/posts/4261103898.html","62ad708d19d31bbcc120bda84e65a40e"],["/posts/482495853.html","c91badb4ce9077b8d55403eba0b75541"],["/posts/488247922.html","98420277b1394698cecfdfb40feaa4c7"],["/posts/570165348.html","72d007ba3024cd3853a5e662478ddd4c"],["/posts/595890772.html","08e7cf1fd59696e2010a946af31e3b41"],["/posts/694347442.html","ddd472b7843c2fffb3719ebe1d719364"],["/posts/707384687.html","528d76a169803e17a8d661b589d38216"],["/posts/71180092.html","3a78d015da237ba527954313bf8ab569"],["/posts/716459272.html","c7c418ff15aa76201e8fb5e5bdbb08e9"],["/posts/795397410.html","39daf89f10cb4829c6b47a4ef0ad2425"],["/posts/820223701.html","3fcb70ad45c18802e53277b382280f99"],["/posts/830372185.html","d4dd448ad252483cba1b4c8706fe039f"],["/posts/88294277.html","590900027ac09d44c876b477267ed584"],["/posts/939963535.html","f31e878120f3836e2f510abf1c8aae0b"],["/posts/983786067.html","bd093afaf7f65bbfa1f8ee726a4c713e"],["/sw-register.js","b51a4f8fa4ed91a81bbe8dba1d06a7b9"],["/tags/C/index.html","c4bf1a8dbff0d5f8dcf8addc439fe5be"],["/tags/C/page/2/index.html","ccc7401d24512da2068d47254c3d560d"],["/tags/C/page/3/index.html","8ec6374d4f56c23a8886e50e60678c74"],["/tags/ElasticSearch/index.html","a7124817c6be3686c698c242251dc669"],["/tags/GUI/index.html","60902ecc0dd8df6d1324fa840849c81d"],["/tags/HBase/index.html","58eae59d870f648c367b7461d2526e54"],["/tags/Hadoop/index.html","e9992a26ca48ce5852062665a0f8d6e1"],["/tags/Java/index.html","f60dfd926bd3a87008ba2874e6f50216"],["/tags/Java后端/index.html","952ed0dce53408a3e2b7cf569756f109"],["/tags/Java基础/index.html","9e20bdc9a6cd1a42b298fd9206539c15"],["/tags/Java基础/page/2/index.html","877d4f9e425ca7c0306a7c8473b334f5"],["/tags/Kibana/index.html","794c342b8e1eda9ae958edf3397a5274"],["/tags/Linux/index.html","add0ac02b870b9b93b3a4bd455d1f1d8"],["/tags/Linux/page/2/index.html","2465d10a7b9857ec7e402000f1bc8eea"],["/tags/Mac/index.html","f34d67a7faebe4fe43e70777977a2355"],["/tags/Mac/page/2/index.html","6c2d3b9a1468a5c6ed7b68b294875f59"],["/tags/Maven/index.html","812f135da68ea9035a2db909d762e268"],["/tags/MySQL/index.html","78c7f79025d516d7f21c19c7649c59a9"],["/tags/Python/index.html","408ce3cfd3aa505187ace63e93de1ea7"],["/tags/R语言/index.html","de06abbf0d577eedd05dd033a5b4f65d"],["/tags/Ubuntu/index.html","4d30a136751cea9194b4f81fcb658c52"],["/tags/Windows/index.html","38b666bb5274e4eb132f3272ea3f2454"],["/tags/ZooKeeper/index.html","58f481d7cb9827ce3fda8c1591cc5d58"],["/tags/bfs/index.html","d99c8f7440f9d86627a53972a3629bc9"],["/tags/dfs/index.html","d15b744752756c891e3d69ee40ddbf44"],["/tags/folium/index.html","278e0f36c38649f8e04a04114cea4d93"],["/tags/git/index.html","99d0490ff74d742f91ee453ec5dc8dfd"],["/tags/index.html","bfec36ea68daeed8e647d095a75926cc"],["/tags/latex/index.html","22140538d00fbebad26539ef0399942f"],["/tags/二分查找/index.html","8e9d83515724fefc7437b5de6492b64f"],["/tags/优化类/index.html","eef00af846cc710fde70b4a73581aa02"],["/tags/前缀和与差分/index.html","d62b0a8e0a0d075735217320ddf8091f"],["/tags/动态规划/index.html","b4a43a6a41d76ca8f81f3b3416038cad"],["/tags/动态规划/page/2/index.html","0a442d517b58270f7e5bf7a5aa35c321"],["/tags/博客搭建/index.html","8f271fa60c5334fdf206ec252129a1b1"],["/tags/图论/index.html","0cd5a79e543abcdca4cd47b370d6ab0f"],["/tags/大数据/index.html","8b3157bff9711e3b5df4e714b7eef9d6"],["/tags/大数据/page/2/index.html","0f8fb1a4b3ff429b07900cad28854e93"],["/tags/操作系统/index.html","59f4d4e71b8f085f6adfda12e027d5ec"],["/tags/数学建模/index.html","736ef5920b0f715e2612737c52ae0224"],["/tags/数据库/index.html","bc984c194bd1144b67a13a911efad938"],["/tags/数据结构和算法/index.html","77424c107e38564a9b06feeefe621b2a"],["/tags/数据结构和算法/page/2/index.html","7bf9a3d8397ce51fca7bc063adfe2a04"],["/tags/数据结构和算法/page/3/index.html","af682426b42ff6537a8f035e7f539c5b"],["/tags/数组和字符串/index.html","1cf9b9866fff08fe5c169c4e9754c065"],["/tags/枚举类/index.html","316c6db184745d2f1192a63f5daed850"],["/tags/栈和队列/index.html","4afdef69097650848a39c05f8b032eb2"],["/tags/树论/index.html","78982d60c6c01183f74f2fc2e0b2d849"],["/tags/测试/index.html","85548aaafb3ffe0f73588a10ff6d2e08"],["/tags/环境/index.html","81ce66811ef0313ae9d23bb055503ded"],["/tags/环境变量/index.html","4cd45efa7c61a898f2b97412b280ff8c"],["/tags/绘图/index.html","f199e97f8a21e930266b926f34a86add"],["/tags/编程环境/index.html","d9d11f0308ffd81c54c7a92b1b94af6d"],["/tags/网络编程/index.html","64eb3eb8994bdcdc8f2ed83315542737"],["/tags/英语语法/index.html","a61e523cd2076c8465e0d3bb6f5dae2d"],["/tags/论文/index.html","e5a128afe2f5283b734698e7def7f016"],["/tags/资源下载/index.html","b3166931d88be22c3f49ee66e1a56c76"],["/tags/链表/index.html","81c9dbcd737ee2b03b9b120868114fb2"],["/tags/集合/index.html","48afd2af6e5c1e2c006ac334d6bcb0b9"],["/tags/集群/index.html","1077f7bc132b1866cbe9fc15473aaa2a"]];
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
