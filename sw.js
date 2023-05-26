/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e5a97ec15036164a4435a9a92a2a6aa5"],["/about/index.html","b21326ed70b992e6e6d4e1de46789d46"],["/archives/2023/01/index.html","dff51b46f21f8db636a69a73205d5860"],["/archives/2023/02/index.html","ca2745344b55d9d1ba9180a6bcfd4bc7"],["/archives/2023/02/page/2/index.html","5304469c7af70bac0e19c21421de3373"],["/archives/2023/03/index.html","e5c1f46987b3580ebda26ff690a2adda"],["/archives/2023/05/index.html","146924d912cf9723e41cbe6781987213"],["/archives/2023/index.html","50779e934c475363d6c698d05d1801f9"],["/archives/2023/page/2/index.html","666ed1e09e58870b011f3ef95810ae19"],["/archives/2023/page/3/index.html","16564aba0535cfed05bf7806c32de291"],["/archives/2023/page/4/index.html","0624cdf6317521a1dc4d4546bf059262"],["/archives/index.html","f3c4d2dae0a2f3e27752e6b2f27ffb9b"],["/archives/page/2/index.html","be705098529678e7e6c90b298f36d531"],["/archives/page/3/index.html","4c0ca1c30353802756bf4138b3ecf899"],["/archives/page/4/index.html","64151e5a390ccd5528954bc5f2d8aca2"],["/categories/Java/index.html","a9c8a41c8c1d4bb2b51ba878469bd0e8"],["/categories/Java/后端/index.html","2236a64ce93e0c1966e943aebc790ee8"],["/categories/Java/基础/index.html","08f9a880b7527a129add9fa390282b2b"],["/categories/Java/基础/集合/index.html","5f9c1265343e0583e69a9495665d5c93"],["/categories/Python/index.html","b42dd88102bfed3a334eca49c8342e5f"],["/categories/Python/编程环境/index.html","37ca67abeabb3a5f16dfa44237bcac08"],["/categories/R语言/index.html","16aa20e299b8dd310ae59af143d26816"],["/categories/R语言/编程环境/index.html","c53607d0ec8ab3a3d33df934ddef64c7"],["/categories/index.html","6a091226ebdd2673f420cffd1a3193c8"],["/categories/中间件/index.html","4be17363240a254420ceccdcdf993b88"],["/categories/大数据开发/ElasticSearch/index.html","73be940cb4dad3d9ae08d5ec7f297941"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","24b2a4c1535ceb07573e3f362d37df39"],["/categories/大数据开发/HBase/index.html","a51ea8f8c5cdd73c9971d0c8ae45f6df"],["/categories/大数据开发/HBase/学习笔记/index.html","2b187763f3e530d3b323e4fb1c4565a9"],["/categories/大数据开发/HBase/环境搭建/index.html","9add030f2ff565b4266ab55f2de75920"],["/categories/大数据开发/Hadoop/index.html","2c4867aff3d324afedb85d551148c82b"],["/categories/大数据开发/Hadoop/技术/index.html","860efbf0a696c7faf85f05dea132bbb8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","06c66b5655fdbfd506992d4aba082141"],["/categories/大数据开发/Redis/index.html","359f4701c6a57e75e126ebf8a06c9863"],["/categories/大数据开发/Redis/技术/index.html","2ac9f494d98e410655ed8c135fe6e46f"],["/categories/大数据开发/Redis/环境搭建/index.html","5cbd4282cf77d33ae9144282c2e42ebe"],["/categories/大数据开发/Zookeeper/index.html","db59df806e246d3efe79ae058eff7070"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","844008e331aeb94baa36b32fb740f719"],["/categories/大数据开发/index.html","2c32dfd545e83d308c35961b79db5253"],["/categories/操作系统/Linux/index.html","236806ef96fdca602781b749fee7593f"],["/categories/操作系统/Mac/index.html","265ceb5a425d31d1c269bc798d4894aa"],["/categories/操作系统/Windows/index.html","5a4af33cc04882c514607b3783df732f"],["/categories/操作系统/index.html","26cbc9476434e0a2ddcbbcbcf90eebd6"],["/categories/数学建模/index.html","4c0f6e52d4b3e19b32d9fbb202c09ce9"],["/categories/数学建模/latex/index.html","7ecab6a0f39fcbb0b29144dc6cddaa48"],["/categories/数学建模/优化类/index.html","7b0701f1595ab96a19c02937824d0ab8"],["/categories/数学建模/优化类/现代优化算法/index.html","bbc0b5d8e8f3fb40a2b494ebe07ef527"],["/categories/数学建模/优化类/规划类/index.html","94f86e75df7622fcd55ccc2638156a56"],["/categories/数学建模/绘图/index.html","cfd8b2439bdc8fe85379d6d4dcaa2cb4"],["/categories/数据库/MySQL/index.html","aee71ad55f7c97c4cec34e8f906ed72f"],["/categories/数据库/index.html","c3d68a4bf538eb91f7f46b88f5c9881c"],["/categories/数据结构和算法/index.html","725275cf174f0f4ba6b2b5fee0c67817"],["/categories/数据结构和算法/page/2/index.html","51c5f08f5dc4a0031ee77431a9f80447"],["/categories/数据结构和算法/基本原理/bfs/index.html","fcbcd22df5d73df99fee2dd38ce922be"],["/categories/数据结构和算法/基本原理/dfs/index.html","74f7a6e81a67cffaa9c5ae89a2cb18f1"],["/categories/数据结构和算法/基本原理/index.html","8e2eeaefbbfb87714ac4ff4889419805"],["/categories/数据结构和算法/基本原理/动态规划/index.html","433f204e5a2432e72c705f68e85e59d6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","80de94d3ad7e4f0c95bb8cf7eb6c90e3"],["/categories/数据结构和算法/基本原理/图论/index.html","a244294538231aabe703ec8155ef6407"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","3c7cf3b67eaf28a5a3594109e0a72b7b"],["/categories/数据结构和算法/基本原理/数论/index.html","35e3ab179826791b45e58a1e2b74e70e"],["/categories/数据结构和算法/基本原理/树论/index.html","5e9e1eede289dcba55b419627bc6b49e"],["/categories/数据结构和算法/基本原理/链表/index.html","36ca38af89e2f3c1b8407ca6ec4fb8ec"],["/categories/数据结构和算法/算法题/index.html","332f835947b5b8b443eaed19a5b9ec63"],["/categories/数据结构和算法/算法题/二分查找/index.html","5628038b68a9a34a98a0ab2afb7b874d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6e9a7583380fbb3ce5440180819f9499"],["/categories/数据结构和算法/算法题/动态规划/index.html","aeecaa88990074fb00f9b85cd38ebf06"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","98f870b0d0252c46de10fddbcf548445"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","7d8eb4fb0cc57a2bcee42385d1a4093d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","fa3d0eb03906ea40860610ab4c919f6f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","216283ef9c0564059fe5030c16d1037d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","161a184d2cb89f23c87a99494b6d13a8"],["/categories/数据结构和算法/算法题/树论/index.html","b60ac71237e36cfa2782b3249858468d"],["/categories/杂七杂八/index.html","892352ae9a22557a1b5bdc4f830107b3"],["/categories/杂七杂八/博客搭建/index.html","e59398b6ee058a29f13205acbfd59fd0"],["/categories/编程环境/index.html","39c137a313f1ccbbc748c38b37af3486"],["/categories/英语学习/index.html","86150618b2b09efca1630fd733f2e835"],["/categories/英语学习/英语语法/index.html","aa64301629100074d8f04fd3c9d86680"],["/comments/index.html","7db9bfbafdcb36b9fe8a2573d1b4f33d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","db07c0a9b69adb8dd9608c2caef3baf6"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","49e3ecd64ed068495baf816f545364b5"],["/movies/index.html","93dd659d85a81cc00ab464d70800dc49"],["/music/index.html","97b5b8a20ff17126b637b92eba84f028"],["/page/2/index.html","5c63c0a8d88bde28ce26ca381cc04fe2"],["/page/3/index.html","6d750300ac905bd1f3d9c490d27df566"],["/page/4/index.html","1fee92bd98b6532c9dcb6a4c7baa222a"],["/page/5/index.html","77bcc59dd75d4161103c7f6d47305c5a"],["/page/6/index.html","da4ea0b61cacad8c458fade89105a010"],["/posts/1021360842.html","816543a7fa70048aa23a9b636f65dd1d"],["/posts/1120620192.html","d7abf0bf4f31b8843587ccffbe88b511"],["/posts/1141628095.html","724d6eba0b696b1ff5edce715e18a5ef"],["/posts/1168613674.html","9d0cf84b3054570263489e59ed0d8bc4"],["/posts/1219920510.html","9f9f8072d3a87e6d30ecc212e928dc5c"],["/posts/1222166338.html","b8e891dc0d643e8f3df85a9ae8dc2049"],["/posts/1259097482.html","93328f5944d3e1cd754ba82b42eb7c5a"],["/posts/1271036369.html","a0ea3620f036d5fadc30c90c9aab7ee9"],["/posts/1312847445.html","f25dfa4b051d90bf831eaebd9b73a497"],["/posts/135355774.html","a792b8ee7604c2363421bfafd82530bb"],["/posts/1375344716.html","eb192981eeff2e5e392bd89b27a996a5"],["/posts/1388991698.html","9ccf8d0b64c5c3e1177e2efd331f4f29"],["/posts/1410315814.html","63c3f7efc7c3b5a200169f9d71cfa742"],["/posts/1452790229.html","3f33d9f79a3c59584cba3523cfb3c7e0"],["/posts/1470079884.html","ddbab358f94aa025df8729c9adabee16"],["/posts/1470079885.html","1454f98b5bfb2514e74040505b040942"],["/posts/1470079886.html","ec4e5a8b11adbe53f5a1ae084460edc0"],["/posts/1470079887.html","058b95e51995368bb3861d7e0f7c4f53"],["/posts/1498536549.html","df9af12bbdfc4815d1c6c6fce5173546"],["/posts/1547067935.html","e58d9632b919619ed6bdc5fa0aac393f"],["/posts/1557866301.html","38d0fb98dce488829a7921d9442afda6"],["/posts/1571776361.html","620e824761e1e713bc7edcf609811aa8"],["/posts/1605124548.html","3e73348688d536fcfb8e468f5465e522"],["/posts/1633036852.html","b224382e04f82d26a571d3e896ab20d8"],["/posts/1765123828.html","8e6690df260b0e859e80bec121c36e8d"],["/posts/1767336200.html","dcd7ac5a2e6a454dfe374e740769af26"],["/posts/1776114197.html","eac34423ecb42098a0a6b43684523d25"],["/posts/1817748743.html","240e7f3a57921773542c951660df7cf0"],["/posts/1925125395.html","0231d99aa41fad12397076fc3f212145"],["/posts/1966191251.html","6a48e286da26774936bf8e322f9f3fed"],["/posts/1987617322.html","3cf3b7579e62dd4d20be4e323b509a07"],["/posts/1999788039.html","9e5ada2bb100e9644738a635a34c3c95"],["/posts/2075104059.html","a2bf6090358eec3c6a8dd087e008df4a"],["/posts/2087796737.html","e2c8679c9806f93867952c73f60b2113"],["/posts/2106547339.html","b77c006da619b2f3ccb867c1a8649702"],["/posts/2207806286.html","aa487f6102b6d166a9c1043548e34803"],["/posts/2225903441.html","f079abc8e1db338590d05ba806f347a9"],["/posts/2265610284.html","ede43f87397d0a20b148385699d35d5a"],["/posts/2281352001.html","9ee5160a93fa4a90bdab0947e0efd18c"],["/posts/2364755265.html","162574c316825d9172ee3b634636b554"],["/posts/2414116852.html","60c71b89aa31af09a33b44b54bac2ceb"],["/posts/2482902029.html","db97e969d2ca2d82b20722ecf1a27e25"],["/posts/2495386210.html","22610a6a876fb896bbd9e139ac368992"],["/posts/2516528882.html","1d3a8f9d8ad832ff50193fef7e1366ec"],["/posts/2526659543.html","f5ea5e753663069dddb69e21ee49367c"],["/posts/2529807823.html","ab01ede2e4fec958bfebdc9185adfb60"],["/posts/2742438348.html","c61ef87b8f74f1cca9efe5e886650b4e"],["/posts/2888309600.html","8f2def973258ce28c578dbd76ab6e3f9"],["/posts/2891591958.html","7cf533f3bfcfabe8cde9df80b04a9ff1"],["/posts/2909934084.html","cd7d1655edcdf3e702dcb7f7b97f485c"],["/posts/2920256992.html","414defdc08d7bb5a952f3cdefa562e3b"],["/posts/3005926051.html","9309c6b5ab043aabcd6bf3ec52a5d40a"],["/posts/309775400.html","75c7e035279c0f18689a56e20117abe8"],["/posts/3156194925.html","6e59290fb84ded2442325e0ee14b690b"],["/posts/3169224211.html","c18718d9f939b9556f8332191113c7f2"],["/posts/3213899550.html","b00179dc399c595c20191048a062ac3d"],["/posts/3259212833.html","3da788660638b13203aff21f8742cbff"],["/posts/3266130344.html","c8bfe2b318c549aad85a16007f8fa8ad"],["/posts/3297135020.html","48f184e19a82f266b80aaa3ddb4d5634"],["/posts/3306641566.html","681393e2cb0e2f4ff8702aaee21ccb8e"],["/posts/3312011324.html","b6b53be2ed2ad3eac9a7dc0d088176dd"],["/posts/336911618.html","a65615afe8435f0f3cefb30399767244"],["/posts/3402121571.html","2ffa32802ea39c0eae531abf2b858aae"],["/posts/3405577485.html","9eda9756cea8e949b13478f6c4052167"],["/posts/3498516849.html","292460a0b6bb2a58448cb0f57c59c086"],["/posts/3513711414.html","1c160b141a532261be652ed9d640bd4d"],["/posts/3546711884.html","d21b8c3d17a8b1a48d07a7f39ea27b65"],["/posts/3731385230.html","6bde3ed3028eadb6c8e65234591bb805"],["/posts/3772089482.html","92f04e9a378a783c8901739be8ffc826"],["/posts/386609427.html","321915a61a85bea47d02fed423320cb7"],["/posts/4044235327.html","9beec771cc384421b38131873be394fd"],["/posts/4115971639.html","8e86c8a0dc14e6a6b412aa4730ce1d61"],["/posts/4130790367.html","dd4cd4c4891c8610784a985009468f8c"],["/posts/4131986683.html","d9ac3ddfbf23a61ac2712181f6fbab05"],["/posts/4177218757.html","a3006f1750781b6152e0a058277b737c"],["/posts/4192183953.html","8ff0f79aeae8b8228ed65fe71bbf96d9"],["/posts/4261103898.html","8a8b3d821b8681162d661d21d0e21002"],["/posts/469711973.html","1ba3f83efc79abba9cdbbd77272f499c"],["/posts/482495853.html","d10f13d24cedc8b7063f2a7a23a6d56c"],["/posts/488247922.html","a2214b56805803d9c98b67383a0fbcc3"],["/posts/570165348.html","f0f2709841e5247d3bfc827a82d2632d"],["/posts/595890772.html","00616ee0bcac87b31f0ca8a18216dd23"],["/posts/694347442.html","f2ccd88dfbec413f84c8dce7124422f1"],["/posts/707384687.html","6417b17609db352e00afe55644ebd2ad"],["/posts/71180092.html","75a592a11e708ecf33d6ef10cf3f74e8"],["/posts/716459272.html","5d54fbe8348e64f621aae9268eb4d876"],["/posts/778231993.html","4481c08b795b331ff0ab3366d9d9ef99"],["/posts/795397410.html","23275a5a8c92085342b490f5316c9846"],["/posts/820223701.html","c0d1be9a1da704d186b9672176d22aa3"],["/posts/830372185.html","7afada8909856ee515bc617c919e7676"],["/posts/88294277.html","c2830fa824bc2960b0e16142d9a6522b"],["/posts/939963535.html","d69fcaf300d4299e15afd9652d0c9633"],["/posts/983786067.html","922d242654c1d47b6c953010b0519d15"],["/sw-register.js","17696b8115b2b5151fe2ae478e7a5fee"],["/tags/C/index.html","04b64726fffa3e757bfff432a0972989"],["/tags/C/page/2/index.html","f7fe205dc56a31d95c2acfe60d503836"],["/tags/C/page/3/index.html","25335dcebd8dc8317469aff71c21eb20"],["/tags/ElasticSearch/index.html","0788d6be477a803c9e78b3a85a0b7bb8"],["/tags/GUI/index.html","313de574fef40546eec41cc2dc72a869"],["/tags/HBase/index.html","1ddf781c0c9131da15a81e754b78f726"],["/tags/Hadoop/index.html","b0040fabd0d8518bda23e5256e8395fd"],["/tags/Hadoop/page/2/index.html","5abde5c067c074e27198b3680092d5c9"],["/tags/Java/index.html","0bcd443eb0c989ead1e7c2c138bc75eb"],["/tags/Java后端/index.html","0b3ee1ff4d8ff79f1f0e1b439220153b"],["/tags/Java后端/page/2/index.html","a6371333e648beb96ac5729754f8b45c"],["/tags/Java基础/index.html","4d52fc5a953284cfa901266308a0de85"],["/tags/Java基础/page/2/index.html","bb3143524599ee2612069b365421d80f"],["/tags/Kibana/index.html","368a82b16b8117ba67edce60b36b041e"],["/tags/Linux/index.html","e959b5511896bd297a786f13cabd1148"],["/tags/Linux/page/2/index.html","f66b085019366ce7e875ff8f10f50c79"],["/tags/Linux/page/3/index.html","17dbcfc29fdd5cebd1e5445bd0cc166f"],["/tags/Mac/index.html","3bca5b08e6e5e14ff94f6a273b3dd383"],["/tags/Mac/page/2/index.html","69e65e84ea57d9679ccd86dbb9a810a1"],["/tags/Maven/index.html","bacb71af1080b8d46ae0b0673b1d618b"],["/tags/MySQL/index.html","4954338b2801e6c97dae26d50f20b3e5"],["/tags/Python/index.html","6f95564ae36ad57c9422aa96d89aeedd"],["/tags/Redis/index.html","2806b0b3fd3f9572f1d27e28b4166741"],["/tags/R语言/index.html","ffad6cf321a73f7bdb92c4fcef8e312d"],["/tags/Ubuntu/index.html","4dea251f981273040cbd3326e3b4993a"],["/tags/Windows/index.html","4cb2c1e3e09adaf00766b58f7307cbb0"],["/tags/ZooKeeper/index.html","a0f3c14552ea67d3e11052b17268fb88"],["/tags/bfs/index.html","619cde77b848e81a5b3e0bcb32bbd07e"],["/tags/dfs/index.html","c22ae4f25e3db7c04edab7f08a9482c8"],["/tags/folium/index.html","1336834b8105dc2403af7363b764c95f"],["/tags/git/index.html","c1feea051b0182c715b7de8def08fa50"],["/tags/index.html","3f96ec1286e5fd44f244c4d321178ff9"],["/tags/latex/index.html","aa5c53c40fda77fcea2b9b6f868cc19b"],["/tags/中间件/index.html","99d4778114cb5d0b430c92382a815e5f"],["/tags/二分查找/index.html","871c3e01f0bd50723ed89fe9d0ede8f5"],["/tags/优化类/index.html","b13cf1c833d6deca4cf684f09235c2b5"],["/tags/前缀和与差分/index.html","ec162b7c2c4fc74f52b189c9fdb494dc"],["/tags/动态规划/index.html","d83af7f78cb5608a70eb65b7bf8fcc55"],["/tags/动态规划/page/2/index.html","17b3a92ec762676da03a2b6a14e439f1"],["/tags/博客搭建/index.html","4951a8b7dfdc52b0abda646b2adfceed"],["/tags/图论/index.html","a8eb0691c6ffd71e0a437ffd619f50c7"],["/tags/大数据/index.html","ca2ecfefb13039139186b2e162fcde6f"],["/tags/大数据/page/2/index.html","6b4f06581b9c193f777eb830c9e6fd5e"],["/tags/操作系统/index.html","c28b2fff20cbf6d3e976620a48ce95d7"],["/tags/数学建模/index.html","3220737676d7c06cfe3a18d78429cc78"],["/tags/数据库/index.html","feb7d5efef6e049438957af23a352f87"],["/tags/数据结构和算法/index.html","1fdef9f5b208056b66ccf0ee9dd52f27"],["/tags/数据结构和算法/page/2/index.html","307a9dc628c8d658c0613f48348ce01a"],["/tags/数据结构和算法/page/3/index.html","3723b80c2c16907aca8849a70fc3ad3b"],["/tags/数组和字符串/index.html","82daf8308da8ab3ee5e19e20f5c395c3"],["/tags/枚举类/index.html","778d5ec68205c5d5b1e3cf63cf7d75fe"],["/tags/栈和队列/index.html","20936df95b8e8067d9a2ee2d093f1d82"],["/tags/树论/index.html","c8c7661e8aea54d72069a0929952f75b"],["/tags/测试/index.html","cc984a4e145b460029caffc5a26c5126"],["/tags/环境/index.html","c6fb286eb36b4d1dd44e6bb08a0bfdab"],["/tags/环境变量/index.html","2d2b0693eb681dc5b5d1e9b49491af0c"],["/tags/绘图/index.html","8c4333b495edf41dde882ccdaf9c536f"],["/tags/编程环境/index.html","9062cbaafaf58feb2958fef4027d2e81"],["/tags/网络编程/index.html","1ef784bea751d15821b1565ab4508c94"],["/tags/英语语法/index.html","f05178a9e77fe2ce32ea20aeb5ce71dc"],["/tags/论文/index.html","a7b3c202419db7398eece0c4cd55d8ea"],["/tags/资源下载/index.html","c3ef424d5bec4b6af7769ebadbbeb656"],["/tags/链表/index.html","2d31cf3e1a8c15c513d8b43da5b33900"],["/tags/集合/index.html","6bc3fa1766f714e9e516b4d0de36b2be"],["/tags/集群/index.html","ccc014b5c17b5ab918facfefb98688a2"]];
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
