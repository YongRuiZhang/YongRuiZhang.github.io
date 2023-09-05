/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","c1d661721453043585007cc19038391a"],["/about/index.html","20dde1dbadfaf84f13ddce8a15d76977"],["/archives/2023/01/index.html","61488b9bbe0bed349c350d7f3232dc18"],["/archives/2023/02/index.html","dfcaa1df7f7b23b1d7ee5cb83f1e9f67"],["/archives/2023/02/page/2/index.html","08329ea4dcdc5d2f01fb78f6aebc575d"],["/archives/2023/03/index.html","d2d9906a28017a74ce9c8155396eef65"],["/archives/2023/05/index.html","5db90b98e8f73abd5b4851cd990bc590"],["/archives/2023/06/index.html","7936ad40c6dcf10c55bd7082984381b6"],["/archives/2023/09/index.html","75785a2318272ae609afbb95f53225fe"],["/archives/2023/index.html","4191816582889ecd1030274c0630754e"],["/archives/2023/page/2/index.html","999e0a00f535f227bfe61c42f056d286"],["/archives/2023/page/3/index.html","a1c28e46ae883d5212164067b2ae7e9a"],["/archives/2023/page/4/index.html","feb7c3c60502e7b143b436407ea76da8"],["/archives/index.html","80869b8b4d1ce40b842b5f3ad6163ca3"],["/archives/page/2/index.html","362b95d5346d9fc6acd4d638b8c884cd"],["/archives/page/3/index.html","55899f8fd9289a1d4e1d225f95b9844d"],["/archives/page/4/index.html","c4769ce5b971e64b3c6ca983ec79ad80"],["/categories/Java/index.html","5bb98c38d4be575f3e4bd20b7345d85c"],["/categories/Java/后端/index.html","070c5a9282c5a51eeff616c827a2df08"],["/categories/Java/基础/index.html","5389eaa4e2ec93baa829433fd4db0ba5"],["/categories/Java/基础/集合/index.html","8bed6a513fd4e220aa18c43065d793c2"],["/categories/Python/index.html","4192f9d59070772af2b25678ea1ea2cd"],["/categories/Python/编程环境/index.html","9e1c11a38c1354c672a68ace79d92d94"],["/categories/R语言/index.html","6b3d13cd279abc17fc856b84817e7aa4"],["/categories/R语言/编程环境/index.html","b297460628f1903c7219961437938f0a"],["/categories/index.html","9ee4a5d76250ddb5f4208bdfe671e79b"],["/categories/中间件/index.html","b2ea6b25fa92d4a8113ff6ff8b3ab980"],["/categories/前端/Vue/index.html","57f5711fadaa1f5991e28833ddb36038"],["/categories/前端/index.html","530d74078143de12916c7e8359daa77e"],["/categories/大数据开发/ElasticSearch/index.html","1ccd1d595d42f6e4b42ae2f82020f7dd"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","25d88a76b508ceac8e052d016ae3b960"],["/categories/大数据开发/HBase/index.html","69e27ec04aff225611802405c2f9514c"],["/categories/大数据开发/HBase/学习笔记/index.html","d5c23ca0ed9ae149e0f61e00ca2e8788"],["/categories/大数据开发/HBase/环境搭建/index.html","ba95bd7fb7397078c64409b1996ea725"],["/categories/大数据开发/Hadoop/index.html","22a629020c4c458526931971aa999a6a"],["/categories/大数据开发/Hadoop/技术/index.html","c2fef467bbedae29eb5678afa51ebc43"],["/categories/大数据开发/Hadoop/环境搭建/index.html","4e0ff63c01751f63e9e26697a0ab2510"],["/categories/大数据开发/Redis/index.html","60569b7b1b7005fe07666e416241a15d"],["/categories/大数据开发/Redis/技术/index.html","0fa3e421479548523a1441c29ab8c8c6"],["/categories/大数据开发/Redis/环境搭建/index.html","4cfb94a244a94a0ed27418ada4de4c06"],["/categories/大数据开发/Spark/index.html","86ffb249e1783d6677682718e022a086"],["/categories/大数据开发/Spark/环境搭建/index.html","eb6418d7017968e98292d2b50004cafc"],["/categories/大数据开发/Zookeeper/index.html","a5dbcbd6abf44582cc5a690d4820fdb9"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9e0118a2b45a74856fa39b46e917e6a6"],["/categories/大数据开发/index.html","a439452fbe381a7e434f646704bf91cb"],["/categories/操作系统/Linux/index.html","898ed5ffb53ea242d54f7f8f18cc8d2e"],["/categories/操作系统/Mac/index.html","e9cc8b6ea5019576a94b20af697b0fbd"],["/categories/操作系统/Windows/index.html","92a60fc02c606879f93d850ca89b8003"],["/categories/操作系统/index.html","480a2b6a373f058a65860f61889ae0b6"],["/categories/数学建模/index.html","647da76b89510ebdecef10aa51bb6e21"],["/categories/数学建模/latex/index.html","1bfdb0a9376943be03cf5536fa061a0f"],["/categories/数学建模/优化类/index.html","4220f2181a66b151d1891c24d561ddf2"],["/categories/数学建模/优化类/现代优化算法/index.html","d13af03b620f4bc05d6e3f4f3aae8af3"],["/categories/数学建模/优化类/规划类/index.html","4955ab0d5e710e247da0e1fcd06b3595"],["/categories/数学建模/绘图/index.html","824f6986cf794ac16f85918cbb3f927e"],["/categories/数据库/MySQL/index.html","bd8e9d0d106b7fa7523790bb045894e6"],["/categories/数据库/index.html","4532e4c3295fe50397a9b047188194ef"],["/categories/数据结构和算法/index.html","21d54cffbe81f5740f8342ba35487471"],["/categories/数据结构和算法/page/2/index.html","d0a307374e1be605a9b61717797370f5"],["/categories/数据结构和算法/基本原理/bfs/index.html","b25034879a4505c19ff3c12a1168ac7f"],["/categories/数据结构和算法/基本原理/dfs/index.html","5b69fe8aa76135b11e2969289c324e32"],["/categories/数据结构和算法/基本原理/index.html","35052082dd4ce655bd0d578176937f50"],["/categories/数据结构和算法/基本原理/动态规划/index.html","91601034ab5f71b131b8dade65732f9c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e793bfe9a3ba75d506bdf32076bba644"],["/categories/数据结构和算法/基本原理/图论/index.html","e43fe80d0ceab1ae4c5cd4b4f193614d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b80a06629804ab697f514c69813d5465"],["/categories/数据结构和算法/基本原理/数论/index.html","29be0b53c3bf5354920a47afd78eada3"],["/categories/数据结构和算法/基本原理/树论/index.html","192d3a805a8defb8b2b336ba7f4a396c"],["/categories/数据结构和算法/基本原理/链表/index.html","ad1b79929b834c061a33d974f55be3a7"],["/categories/数据结构和算法/算法题/index.html","84f8df675449eddab0bf88cab0f5a005"],["/categories/数据结构和算法/算法题/二分查找/index.html","31574df68c4a880466da62e81d7aba9b"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","a5bdab45d027120050eb4651da894c65"],["/categories/数据结构和算法/算法题/动态规划/index.html","c2247ece3bd83c9652521bd7507a2537"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a353115b9035e81cc51dea20fe303506"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","ac80ade54cfd235c87b5705e232901be"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9942797bc463b2b5c5caf4e10b398a93"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","9a462b73a14086b6c79d2abdf26adfd0"],["/categories/数据结构和算法/算法题/栈和队列/index.html","af70a308ce13ebebf1933513fc8d2689"],["/categories/数据结构和算法/算法题/树论/index.html","eb9c751c4bc07f6d1a462d725fa0a865"],["/categories/杂七杂八/index.html","61391c23b94d9d03e668e7412441ed44"],["/categories/杂七杂八/博客搭建/index.html","ae0cd828706e4d85bf59a8838cc39492"],["/categories/编程工具下载/index.html","9aa06a5f7d9ad31c981a5c401273a83a"],["/categories/编程环境/index.html","e238cde123c8fc8bc55e33cb53298a19"],["/categories/编程环境/大数据/index.html","c34c413c33ea8c9586b223e6f3a36464"],["/categories/英语学习/index.html","787df5c9ff06547a287e33cd57dd3d1e"],["/categories/英语学习/英语语法/index.html","0dcac7322f521992f46fc9b81ac86d04"],["/comments/index.html","45d17c5247150a2eb2fcef463ab371ad"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cf19f90c36bb1e50750cb24aa02aa8a0"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","081e7ca4a4ef3bfe3c141467abd48a27"],["/movies/index.html","78a734f0c606cf3e29b2d81fa22ad9b8"],["/music/index.html","e2546dc94a82c84906f71124b49783e1"],["/page/2/index.html","4e47fc7a2fc0b6b1b4ba3664b18b5faa"],["/page/3/index.html","28b668c40db17f4bfa3de558d3a3e6fc"],["/page/4/index.html","d4570148e5f1f474d2c16ab641f27b94"],["/page/5/index.html","dc5dfdb929c914cd6946f86b0626dcc4"],["/page/6/index.html","200b21abcecc9b5c311d688a7d4c68a3"],["/posts/1021360842.html","29ff29c9415957f19c3067221571b82e"],["/posts/1120620192.html","cce79a8c105750fd18fc802b3934f914"],["/posts/1141628095.html","cc169e80ae54ed99c8671fdefc8db48d"],["/posts/1168613674.html","9f317e598a15e0ce2db8d0279b3ba9bb"],["/posts/1219920510.html","7e3ca2d74dc9fba0e8e84a3be56388f6"],["/posts/1222166338.html","8ef35e9e7464d044785f80419a531e8a"],["/posts/1259097482.html","d0f9623ac3cc22a95c2a968fb4d4f415"],["/posts/1271036369.html","40e85817f7a98622f02e332fd9527e19"],["/posts/1312847445.html","888dd94baf86929d42ee77f3d0f074dd"],["/posts/135355774.html","7e8d9e27fed2319a12d023fd7f0b7dce"],["/posts/1375344716.html","1caa23a81fd6471752e24d4c5f3970fe"],["/posts/1388991698.html","81bd0924d42fe4aef495824735a80c1f"],["/posts/1410315814.html","99094eee5002133a71d8c320acaded99"],["/posts/1452790229.html","d5f3c9527ea3ad3da3e2040121ffea0b"],["/posts/1470079884.html","5a111c839d5222afe8c95859179544a4"],["/posts/1470079885.html","84adbd722e4ba8cee4e4d9d42d5308e6"],["/posts/1470079886.html","5747766a1f7cb9131c1c3ea15b8e0a40"],["/posts/1470079887.html","69d93f1f6c72391ad7080cede1585c85"],["/posts/1498536549.html","5e6eeb53fd94b2d9a15c22c5802d39a2"],["/posts/1547067935.html","e638593e2c4831e3eacae830d17add6f"],["/posts/1557866301.html","95c5c7655d11beaeb4b6ff03c1604ec3"],["/posts/1571776361.html","b7f96457b528a5938ff03a83ebdc8ca2"],["/posts/1605124548.html","5e753694b12dc058c17f2bbda2259a72"],["/posts/1633036852.html","db6791aae8861a36767901bd008ad94d"],["/posts/1674202625.html","4ee675c1cc6ca471fe301cf951519623"],["/posts/1765123828.html","c702a6d3192520a9a2cc5272a473ab88"],["/posts/1767336200.html","c34445ebbfa9958c81930ff2fbdc9d29"],["/posts/1776114197.html","e925d0626dd442792f6b91687e1f4363"],["/posts/1817748743.html","7ac90da7160693e6066041555c5d1e7f"],["/posts/1925125395.html","556ee78b2ca7374faaeabc89bd0115be"],["/posts/1966191251.html","b2bc7147becbe8f246a64bbea2e06867"],["/posts/1987617322.html","afa5e992d2b5f0d8bb01b40970758914"],["/posts/1999788039.html","6209c920616e269a50ca3d9e5b11a2ec"],["/posts/2075104059.html","4b3ada0acba16fc76e5f29243e5387d9"],["/posts/2087796737.html","71e658e2a2b1ef621000ddbe40ea27c8"],["/posts/2106547339.html","f79aaad2aa90d4bccd5523831a60fdcf"],["/posts/2207806286.html","e2c6d19ef6fed65f57df913b292be718"],["/posts/2225903441.html","056835c7b18eaee97bc94d37aa57328f"],["/posts/2265610284.html","add4cf7b68e138f47c5b398e4cea4a7d"],["/posts/2281352001.html","3254e2b508fb6cbf75dbaa5c4fc5cd04"],["/posts/2364755265.html","ffde4b1bd312eb380e74ed2fb5b680de"],["/posts/2414116852.html","31f35ab4e57bc9e3c71878f7d1d4c169"],["/posts/2421785022.html","328c2e790b53ac85f24240fe2cd2be51"],["/posts/2482902029.html","06604ad6fb278d519d33ce9679f284c0"],["/posts/2495386210.html","a5a1562e028974ce09d56aecdd52911d"],["/posts/2516528882.html","6696b4258a4ee4056ee1da45edebbc91"],["/posts/2526659543.html","b778f7a0ec1e1482e2998828f8d8874e"],["/posts/2529807823.html","1b48031375d3fad7d34a1bef519938c5"],["/posts/2596601004.html","7b53a1b41cc481f397a8c090a417cd64"],["/posts/2742438348.html","78b313831b23ed53e17b3d6a94934578"],["/posts/2888309600.html","ae8825fade96be781e188ac2d9b74042"],["/posts/2891591958.html","d7f08a6aaef22fc3fa2ff9d23420a8ae"],["/posts/2909934084.html","6d12cef26543d4133fcf6a765f8bab36"],["/posts/2920256992.html","af22667561ad2ec836c83fc1cd96c7be"],["/posts/3005926051.html","5920e6b0c765f454b8b31e5f93e03355"],["/posts/309775400.html","528394432af0d08a2a7020ba20caf124"],["/posts/3156194925.html","570ca8a1aeaef379fb98fafa06fb00f1"],["/posts/3169224211.html","9cc46732f70cac22d7703f26c063d09c"],["/posts/3213899550.html","0e0b4df268cb376a080eb2d16308ff70"],["/posts/3259212833.html","4284017ba7cfef18d12e0be5bcc95128"],["/posts/3266130344.html","a4d57ddf4c629b155d087c328324b3b3"],["/posts/3292663995.html","7e5159a9b6b62d7e35474c6404ae4189"],["/posts/3297135020.html","089cf0c3c29dab1e38365fa99ad9bf16"],["/posts/3306641566.html","27161ad849aba6be161f926500ebb321"],["/posts/3312011324.html","5b41605809f47d44e3272c5ae3b72a76"],["/posts/336911618.html","2aeb37a2065dd7a00999981d8cbc7515"],["/posts/3402121571.html","fb9d311df9dcb4be133e28d1d053e547"],["/posts/3405577485.html","7f0c4c4f9ce27d4fafddf50f8518eec6"],["/posts/3498516849.html","18f6d46f27b8363f759e63f7b63dc6ad"],["/posts/3513711414.html","a2ed8d153f93f47d36ee9b875398b4da"],["/posts/3546711884.html","b6ab95264e51b4380e902f3de4bc931c"],["/posts/3731385230.html","610e460466c731e571b31fc2d151c306"],["/posts/3772089482.html","036599cc63a7ce06ff36c17586c6d10e"],["/posts/386609427.html","eb616460b20ee764710918e27c5f75cc"],["/posts/4044235327.html","25fa8d7b7b309cdbaf7c9f69053569d4"],["/posts/4115971639.html","045d6a654488745d56486f2ebc740037"],["/posts/4130790367.html","8dfda48dec3970392f2b6af7de1a9ad4"],["/posts/4131986683.html","ef48c67523e96a3237257b007d43e0ff"],["/posts/4177218757.html","721c9e21212fb0b0d8014208fb1ad105"],["/posts/4192183953.html","37651e2ba11b5b4452042fb97300316e"],["/posts/4261103898.html","bbe0a4f7d6f2ca5411ce667226d993d8"],["/posts/469711973.html","cd51dcc30d96b66bbafc7b94bce7dcdb"],["/posts/482495853.html","7cd0aced151d4d862c3efcf3b9cc4552"],["/posts/488247922.html","a6beb7731f52b3319ad4de38e1ae0288"],["/posts/517302816.html","c35cfb54e2c28852ba18532e9e33c085"],["/posts/570165348.html","4aad1cb5a0314762e36b59428f6e7550"],["/posts/595890772.html","36a175bd7342852f527a90b359bedbf5"],["/posts/67485572.html","3b2a870c661af6b9b78457a6d9af14bc"],["/posts/694347442.html","52680e50bff15aebd6d5ebc8148451ad"],["/posts/707384687.html","6c1bba4b3ce194231a62575b082b5717"],["/posts/71180092.html","10bb845ba64a5f247989dba4f5eaa719"],["/posts/716459272.html","0d9ed8b4c7cea8cf3d1e717c4720555a"],["/posts/778231993.html","35fdc3af45c6efc3827785f60122c503"],["/posts/795397410.html","6e2d40117d3202db3aa553bb74b3679c"],["/posts/820223701.html","109d49574e7c583c241e09c6e6689318"],["/posts/830372185.html","dea9b626cc325e98c71a17a11ab6002b"],["/posts/88294277.html","ec075a1d8c71ab9d3dc17d4947291497"],["/posts/939963535.html","99aa24a9c5906e42291d8c3ac435456f"],["/posts/983786067.html","3696e96082155ff8ce04fca82a17e338"],["/sw-register.js","5c1836f4a45567c2cb7adf1912a90ef9"],["/tags/C/index.html","bbe99a09d2f14762f16372b4a97990ac"],["/tags/C/page/2/index.html","c4da9d73a4305a646e9064ece3a8a4b1"],["/tags/C/page/3/index.html","2d569f9c5598f2b4a1055e144dbb11b4"],["/tags/ETL/index.html","b972ce23bd2728b37dbd227097ba7878"],["/tags/ElasticSearch/index.html","263e724c485023ca64a99d394f02ee27"],["/tags/GUI/index.html","8cb1da2edf0284701c2e79fbe0c0db66"],["/tags/HBase/index.html","cb50ed1a5419bef078abae6db8ffdf81"],["/tags/Hadoop/index.html","9287acbd9e274a0da73539c21176bf80"],["/tags/Hadoop/page/2/index.html","42528a65f5b247d49b35e944f99541b4"],["/tags/Java/index.html","6791026ee35cf6b83f3493963047ee0d"],["/tags/Java后端/index.html","4d2aad6af9831d569dae2901ec594597"],["/tags/Java后端/page/2/index.html","d27d43f05f814c241a2ff157d2ab5211"],["/tags/Java基础/index.html","cfe43d74b78a6660de4d7101d08b8c75"],["/tags/Java基础/page/2/index.html","ba88615b6e7149378c95bd54066ad4d5"],["/tags/Kettle/index.html","3cd3a2bcff97ecf3250b2ab9efd913dc"],["/tags/Kibana/index.html","b142e84d6fe23d5fa223dd8e6ba35f23"],["/tags/Linux/index.html","95cb6b15592b6dbb36ab3d66c03968b2"],["/tags/Linux/page/2/index.html","b05254bd7345bd35c44e43fd4d6b98e0"],["/tags/Linux/page/3/index.html","860dc345140f86a7c1d81bc5920fc361"],["/tags/Mac/index.html","2652565985f5c039adcc96983810eef0"],["/tags/Mac/page/2/index.html","850755db638fc49a9699075c882116d1"],["/tags/Maven/index.html","e1fcc0328d2fb71f11f3557fe323531e"],["/tags/MySQL/index.html","22c150ea86ab37390748007eb930e3dc"],["/tags/Python/index.html","66b826993905ee926eb76194c9ea310f"],["/tags/Redis/index.html","b1d67652f618405759d9ffef11cda788"],["/tags/R语言/index.html","53ec6963b5651c1f745bf910aa649d06"],["/tags/Spark/index.html","1354cf25789636335fdee03a84694d79"],["/tags/Ubuntu/index.html","bac4d3a7f8f5d8d4092358b12281937c"],["/tags/Vue/index.html","cf7f2c18eb124046ed6f4391d131988c"],["/tags/Windows/index.html","02bc9dcf9658b150a585e865d10660ca"],["/tags/ZooKeeper/index.html","f68a859ec1d7fd6a8c9a3513aefd57d7"],["/tags/bfs/index.html","d63d933729367557a8e6911ddb94e67f"],["/tags/dfs/index.html","4c9d1b1742b415ef37f466fa6919d348"],["/tags/folium/index.html","1837d7bfa9254d86287b3d7ee9e1d42d"],["/tags/git/index.html","b582ddbdd353907b0027c4a2a9a27d0c"],["/tags/index.html","494d045328616cda439d48d68b58ba3e"],["/tags/latex/index.html","1a88f781007b68ec144c96f53cf252d3"],["/tags/中间件/index.html","9ed4ae85d8f5fbc98fa91d2f490131a2"],["/tags/二分查找/index.html","be773e5277d3fc8b29825de4c1fb94b5"],["/tags/优化类/index.html","083410096f507131867ed84c907bf46d"],["/tags/前端/index.html","adc27551c856a7daa65caed50303b727"],["/tags/前缀和与差分/index.html","1af2862e725e899bd3e3ad97a966f9fb"],["/tags/动态规划/index.html","492652e40483ecf386aafa84108bcbc8"],["/tags/动态规划/page/2/index.html","aa35d29ac591621461b7779b1dfaf5e4"],["/tags/博客搭建/index.html","e9997d879a32177f025e1d69f27800e5"],["/tags/图论/index.html","74069af851469d15998841cf3ba25a4c"],["/tags/大数据/index.html","84a933c895bb2847921a324963ee121f"],["/tags/大数据/page/2/index.html","412089e52736aa6d761ca88bb9d0b56e"],["/tags/操作系统/index.html","aa8e9fbd031b72dd176aa130f4c7a933"],["/tags/数学建模/index.html","0c3c9ae32eeb5234531af9367c84281a"],["/tags/数据库/index.html","c6c703a43965f373bc1cf9ff553e700c"],["/tags/数据结构和算法/index.html","f2f85fe43f5452750daf37f0739fc7bb"],["/tags/数据结构和算法/page/2/index.html","6deb8e10124f06f38a5258f91705e3d6"],["/tags/数据结构和算法/page/3/index.html","5b09ae04adde058a8e30ef95fc2acfc3"],["/tags/数组和字符串/index.html","065227ea218c926db4cb636c614cfb90"],["/tags/枚举类/index.html","aa6ffbdc875c17b7028309a313b44472"],["/tags/栈和队列/index.html","0e6a3c534aade1140424198f8ebabd81"],["/tags/树论/index.html","8589ac4dd2c4b5da1c4d6220c525697a"],["/tags/测试/index.html","7cb6522f8c069f94d90f0bb0994b924a"],["/tags/环境/index.html","618c8e6ff50294e9892fc8d4d01b8904"],["/tags/环境变量/index.html","fd7d5543ea383a24f691a19b09859398"],["/tags/绘图/index.html","cb978e00993730a41dfb35bdcce7d4cd"],["/tags/编程工具/index.html","9e106b89b043332c784243beb440cd39"],["/tags/编程环境/index.html","cd8afcf3c3a8204c3e90c7ccfbd5dcba"],["/tags/网络编程/index.html","f161ed3e7f8532b8cf5a152d95f85850"],["/tags/英语语法/index.html","fc33f7ce50ee11d0c2bcfe64a223f00c"],["/tags/论文/index.html","6f44275058aeb42ea6ff8ad2d4c17979"],["/tags/资源下载/index.html","9be14ce10a5d0abb1cbc2b1706757aa2"],["/tags/链表/index.html","ed75978268c711384363ab7922dba33a"],["/tags/集合/index.html","43af886fa87c767401ad7366a682d648"],["/tags/集群/index.html","2663e7607a3d88292883ffa522156a96"]];
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
