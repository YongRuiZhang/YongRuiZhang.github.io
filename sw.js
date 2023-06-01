/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2504cb7670f517d458e2176735935dcd"],["/about/index.html","d17083f03b804393aacac9aa29d92550"],["/archives/2023/01/index.html","50d20b1e1e99eb3e8bce3f8232af2d25"],["/archives/2023/02/index.html","18b47305051b9c5a52f19f986dce7c66"],["/archives/2023/02/page/2/index.html","9196a18acb152107a639eb36d739c71b"],["/archives/2023/03/index.html","ec8c6b7a6b65c0a68a664948c6e55569"],["/archives/2023/05/index.html","4abd457ea4cb3bff5a2aef4d1ba97cd7"],["/archives/2023/06/index.html","225f504cc2321c682b5ce5cfa4b18e59"],["/archives/2023/index.html","03119a4d88005b5eece22a16ca852334"],["/archives/2023/page/2/index.html","bb56db30c09973e2ee9c3370eb9feb40"],["/archives/2023/page/3/index.html","7de71c8fca43488679a1ae3c21a0a0ac"],["/archives/2023/page/4/index.html","625ce3c66acf47218db1c53cec47933c"],["/archives/index.html","1a3ccbe6aa3efe9c60fe4e7ed45d281b"],["/archives/page/2/index.html","e6761693f3280b613d7ec5fe471b0d9d"],["/archives/page/3/index.html","08c05bad10faf77fdd0baf5f724729ca"],["/archives/page/4/index.html","ac083908582fba787e56ccd0ebe7873f"],["/categories/Java/index.html","80db2b22574f930650679ef5fd9b5c89"],["/categories/Java/后端/index.html","53521389ecfd3656d2e213b8e76b5207"],["/categories/Java/基础/index.html","8d3aaf500b4fba74835ddfce5cdf9afb"],["/categories/Java/基础/集合/index.html","619c6bfa9b54318bb9b50beea5ef203c"],["/categories/Python/index.html","1f72b41a63c7d91c7ce067350eca5737"],["/categories/Python/编程环境/index.html","579d6a43573768c80780aa0c03548c6f"],["/categories/R语言/index.html","9e653e097801e42adab28a2b5cb0c32e"],["/categories/R语言/编程环境/index.html","8a62249732ad0b3703f6cd95a073049a"],["/categories/index.html","903c4f71622618a4d491485d7ae4378a"],["/categories/中间件/index.html","993cb086ce78875dc9968865fed84ec1"],["/categories/前端/Vue/index.html","953b2853c258fa35e5c2365abd551a14"],["/categories/前端/index.html","797e4d414731c62b93b59ae5c5f7490e"],["/categories/大数据开发/ElasticSearch/index.html","06ab445e8a4bad8d894fcc345222324b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2201f5cf9a573c2c17742ebeeef78757"],["/categories/大数据开发/HBase/index.html","c0f56a5aba38f8fb8717a4714d239ec9"],["/categories/大数据开发/HBase/学习笔记/index.html","047dc4f99ab2bc1793b94b5bbecb5a0c"],["/categories/大数据开发/HBase/环境搭建/index.html","ebe0601eabf1599b9ddd32be16486008"],["/categories/大数据开发/Hadoop/index.html","a52d6d07e891c81a2d660487c04179ec"],["/categories/大数据开发/Hadoop/技术/index.html","b4da695afc62e7c5cfc2dbf1cdfc9cbf"],["/categories/大数据开发/Hadoop/环境搭建/index.html","03c0a9549b0808f9f262a5f3d826097e"],["/categories/大数据开发/Redis/index.html","58fca7b27a6874a11da3094113fdbead"],["/categories/大数据开发/Redis/技术/index.html","db6dd3bd37d7553aba5fe7ac45369ae5"],["/categories/大数据开发/Redis/环境搭建/index.html","cc56a8fb5cf3b2a05e102f253bd5495b"],["/categories/大数据开发/Zookeeper/index.html","8d23c9783d656d33c805dc1254018434"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e912d0aca87783a211a29cee466acbec"],["/categories/大数据开发/index.html","37903e289adb0bafa826e47eac344890"],["/categories/操作系统/Linux/index.html","d4ef234dfefe5f19497d7c4d617ad7d8"],["/categories/操作系统/Mac/index.html","927723f48ab86e009de4d81bd5e3c84f"],["/categories/操作系统/Windows/index.html","382400c8f5fa8f8b34e9bc1ae0cb8c18"],["/categories/操作系统/index.html","fb73bbcf08380946e67bee9f3206f778"],["/categories/数学建模/index.html","928024ab9c47a408295f5378d3505247"],["/categories/数学建模/latex/index.html","2cbc69caa5db62819b7b7614359ffad5"],["/categories/数学建模/优化类/index.html","c0f248bd13c6a325b58945206d8d27db"],["/categories/数学建模/优化类/现代优化算法/index.html","32d927b11e5488beb755c94d3cc2ac55"],["/categories/数学建模/优化类/规划类/index.html","f74b0864f40b27b160697f605c742e18"],["/categories/数学建模/绘图/index.html","67f6365588e1efe1f7e92bd8b6426967"],["/categories/数据库/MySQL/index.html","4a6d82741e4dd28385d41a4ef9e54fed"],["/categories/数据库/index.html","5837055c3220b2a5793a951c6d64484b"],["/categories/数据结构和算法/index.html","635bbc5ecc7195a8a7d52a9bfe0c3add"],["/categories/数据结构和算法/page/2/index.html","428dc22f6db9eb14c2528ec445267e5b"],["/categories/数据结构和算法/基本原理/bfs/index.html","8ea2b2423581d4a3658c5e3719927225"],["/categories/数据结构和算法/基本原理/dfs/index.html","97a05704d39c22875b54ceb132bbc011"],["/categories/数据结构和算法/基本原理/index.html","ac69db8287a05e509b75c2a29c786772"],["/categories/数据结构和算法/基本原理/动态规划/index.html","3f83855fa3c58a1367df227cb62431f3"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","901090489f2d6547c182d32f5003252b"],["/categories/数据结构和算法/基本原理/图论/index.html","96d23b5e857bd54c121ad0b4d9611b23"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","f497eac4d34972b09ba457808164b1c5"],["/categories/数据结构和算法/基本原理/数论/index.html","acb6567ddc083154364af358901edf20"],["/categories/数据结构和算法/基本原理/树论/index.html","ce7e02a11a4c4580aa189419f02fe13b"],["/categories/数据结构和算法/基本原理/链表/index.html","d8faf9f2e36b7a544a34500aab82d275"],["/categories/数据结构和算法/算法题/index.html","44d4b130d855ce72f4b1065d4fabc978"],["/categories/数据结构和算法/算法题/二分查找/index.html","98d4f878217ffd9a7731021dcb078a00"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2a620883b43b0d92f88ef1d33ceaf5d8"],["/categories/数据结构和算法/算法题/动态规划/index.html","fdf7d04decfa47a560f115907270b9a3"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","89846b8202d1426b073af385f2c81479"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","8b69883fcc887c7f9893f7e8ec17e4d6"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","309d929ec13c78388baa7b9312ae783f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","14a6845af33660c4265580bee87dbe7a"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b120dd3548651662438b06dd26d114b2"],["/categories/数据结构和算法/算法题/树论/index.html","1232b2489399eff2a9a3e13800ad5de0"],["/categories/杂七杂八/index.html","0f6d52d1aa7545b0a86d1229fc2664b7"],["/categories/杂七杂八/博客搭建/index.html","f6b15dffe2e65b6d70c8e5f90379e3df"],["/categories/编程工具下载/index.html","1806d02147186a13a67fa8f89f03ca2f"],["/categories/编程环境/index.html","5ae281752bbd6491e4393a2e3d44ec7e"],["/categories/英语学习/index.html","21b9386bb537f652044a971bbd91609c"],["/categories/英语学习/英语语法/index.html","d565b587d3b7bad77d7e5cc0e8c83813"],["/comments/index.html","ad9b8c489af65ce790f26e53a9e7be3d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b51ba5c2cd29400d260b0416e3ed0626"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","52f892e7bbc7cf95d8fc48fa9ab333ff"],["/movies/index.html","f23abd9a600a4d3956b5cb3d41a05485"],["/music/index.html","1af5ac21d416f514292520959115bf59"],["/page/2/index.html","c6b39f53cb7ad2a0550bf3b984bd3b7d"],["/page/3/index.html","375e93500d6ef9a40a004f20edf0000e"],["/page/4/index.html","3597c80b47300712d4dbf61ed39e3784"],["/page/5/index.html","cd28d77423ba22820684e6f98c9e8ef1"],["/page/6/index.html","f5b469a089142d74221e30d90a593482"],["/posts/1021360842.html","b7806c7b0e4e84b2c2fd75507c06e18b"],["/posts/1120620192.html","c98fdd0942a168764e37604d36816c0c"],["/posts/1141628095.html","c6f2c4017f3ebac8e00efc44eb792b70"],["/posts/1168613674.html","768253e7a39da59371656c1a0257cb3e"],["/posts/1219920510.html","30c30534124872995c19ec56824cd3a4"],["/posts/1222166338.html","074b6769646d68ad9de782b3acaf82ed"],["/posts/1259097482.html","5d063ab5ab63425577d718fec979c122"],["/posts/1271036369.html","027a1fa1dacc904f1a8b999e665420ec"],["/posts/1312847445.html","834918c79b88c70901d619d870ecdbdf"],["/posts/135355774.html","3bdc21d35416fbe3c985f30169be0b0f"],["/posts/1375344716.html","f240b61a75ebd86c0f38d72736701d19"],["/posts/1388991698.html","32ee39b76439a44f0b05606a322b57fc"],["/posts/1410315814.html","04fe7bf886de39cd6b0b208bc45ff4c9"],["/posts/1452790229.html","081680878bafde546d8f94c93e548498"],["/posts/1470079884.html","c76ab87a7d8b4ac597994087048307e4"],["/posts/1470079885.html","dc2741aaad6b3ad6e596e4a4f901966d"],["/posts/1470079886.html","44a20b0bdcdb7ffdccbc98f9fe5a2865"],["/posts/1470079887.html","b36145b5fffd997c7f42a91afb42a4c3"],["/posts/1498536549.html","16a5338bdab665b90412c16857ff9ad1"],["/posts/1547067935.html","186422dd96aa33daecd6736f37c43374"],["/posts/1557866301.html","d100f9008d9f5b4de97a4f443ea56028"],["/posts/1571776361.html","9bebbcd603e1aeb578cf98fb02134b7d"],["/posts/1605124548.html","d76c19b9e4eb33479aaad10e392ef0d0"],["/posts/1633036852.html","f98f4188f1c2feb701ab87d1d3910dd3"],["/posts/1765123828.html","e8bdd4304b93146eeb62bd6c4d720d5b"],["/posts/1767336200.html","37fa222e3977c49b13215b0fc869c466"],["/posts/1776114197.html","b4e18c08f4d613f24dd8d959235a42d9"],["/posts/1817748743.html","301573eca9f18bd861fd65e67e89b484"],["/posts/1925125395.html","ef6a0dd7029e87880740b701587c8d81"],["/posts/1966191251.html","f60aaba2ebd9f3856f52473ebdc82f07"],["/posts/1987617322.html","4b42827e063ae5b461d9f2add0f81bdb"],["/posts/1999788039.html","6c91b785709c054999a2654085282e1f"],["/posts/2075104059.html","85a786a4f9ce97647c19c3111d4d52b3"],["/posts/2087796737.html","2245185a10156434d477848a4f6f4dc4"],["/posts/2106547339.html","2e0e4b288170c798c9ebe3c4313a0809"],["/posts/2207806286.html","7ecd2b781e6e6bf156bf46aa2624b914"],["/posts/2225903441.html","119c9a552c5e18fcf285e94c8b55eced"],["/posts/2265610284.html","b8b0546262b7a5adff3bd0c9295e5315"],["/posts/2281352001.html","8224e30757169949a7f1b3535dee3cb1"],["/posts/2364755265.html","0f3ed93b49aed444fbc702eec2c3e8d8"],["/posts/2414116852.html","ccdf729e95f24970fbef79566ee89513"],["/posts/2421785022.html","1352efd6c1cd4d1f1bbace23fcd4d8a4"],["/posts/2482902029.html","a692a1112ac4265be085cfaee5ce3f02"],["/posts/2495386210.html","ecd1d734b86c2c7125c385cccbaba4d4"],["/posts/2516528882.html","cc4e9715c6e53f497f147f22c3e7f5a5"],["/posts/2526659543.html","085f1730165ff5d2d0249930f607e588"],["/posts/2529807823.html","24e58c46bc56dbacbe2953cd56fedd22"],["/posts/2596601004.html","25c2c7428c05aebddc7bf29409f029f2"],["/posts/2742438348.html","697a45dd32b47b52d1ff6c0570c8338e"],["/posts/2888309600.html","ae16f51abec48d2e9df76df4a488c6e4"],["/posts/2891591958.html","4a75d5c40b4145500cb07395224d70ef"],["/posts/2909934084.html","fc2442d20130cd62c7db376937001221"],["/posts/2920256992.html","18918a1366b77af428c15d6f27188894"],["/posts/3005926051.html","6bc07bc720f575f3559aa38fce3b1baa"],["/posts/309775400.html","ef7e020a226f5652537c7a939ecedfae"],["/posts/3156194925.html","b9d2b9b2760463136b51571649a757dd"],["/posts/3169224211.html","f9da8b9865f67acaacf53fc618bceea0"],["/posts/3213899550.html","bb97f4347c5b6d506d83a25c188ad0a7"],["/posts/3259212833.html","92109cb806e4e9fa8226b3516cdbe506"],["/posts/3266130344.html","6d0209b033ebe9f8cfd294fdf8d5b246"],["/posts/3297135020.html","a831342d5359092dc54b8fe006173c29"],["/posts/3306641566.html","e8caf421fcddb2e0ad1955abea215c64"],["/posts/3312011324.html","6b101312872f09faf735f24725454ee7"],["/posts/336911618.html","f0157a4c8eb4bff33553d309e46c46fc"],["/posts/3402121571.html","b74447e0bbdfe2e94e6a9a59212e0a75"],["/posts/3405577485.html","32a8adc7ccfeec4e7646ea082659b655"],["/posts/3498516849.html","2ff7270c31edb3b99a6f6a6fcb1421b4"],["/posts/3513711414.html","6c1efac6fbd2006ebf681a751240dcac"],["/posts/3546711884.html","35403e82aef436dd531e158e66d1ed44"],["/posts/3731385230.html","3426c34c20fd0ba0b839898503e2d411"],["/posts/3772089482.html","492a64a8991d69662519da6ed0bb4c76"],["/posts/386609427.html","8dfab4dbbd53fa7dcd7371e2dfaff0cf"],["/posts/4044235327.html","836bf1d350d2ffef7eeaeca28e7bcc1b"],["/posts/4115971639.html","4c7b0559acfe67baf47ea0fd7cf21288"],["/posts/4130790367.html","12abbc80b52ae69247589906c0e4ed2c"],["/posts/4131986683.html","cc14a6b811bfe269032795a6f4af0841"],["/posts/4177218757.html","3d87f01c3eb3ef90f9c6135167e5a7f3"],["/posts/4192183953.html","f6aeb522d64e911a86c6ac9f1ad48d6a"],["/posts/4261103898.html","78cc8fc1a7a0c1fcfc4c3ea050e115cb"],["/posts/469711973.html","67d942fd5ba1bbe662588a7844982d4e"],["/posts/482495853.html","07c5e395310a7938fc385e3ee9eef479"],["/posts/488247922.html","75fee5f74988602e5ce456667491d67c"],["/posts/570165348.html","4cab6c3dfef36e29d107670ed1d15821"],["/posts/595890772.html","71d6edfadb863a59c52efba069eec910"],["/posts/67485572.html","a1ee885af5570b015bea9d2acea5d5ff"],["/posts/694347442.html","73672aa5f61c8be2944eb4a248c21d42"],["/posts/707384687.html","162d9d428355814368e2a97e0e2d181f"],["/posts/71180092.html","f2dc7ca128e0465eb0636dd8ff5620e2"],["/posts/716459272.html","056230a8a14d9786b2e6c00200a3a590"],["/posts/778231993.html","8daf00f519114f6f01384bae7438de7c"],["/posts/795397410.html","3490b6b0733d667e184ceeef16d3e61a"],["/posts/820223701.html","e93c329f06d1d15607b6472374ee928c"],["/posts/830372185.html","a7e5e3a4743074f1d67ad4ec27013237"],["/posts/88294277.html","dfca2c8c7ea1709657bbf982e5ee662b"],["/posts/939963535.html","f72dddc78539e6378cac0403a936ddcd"],["/posts/983786067.html","508a4aefcc9dadf7ee914c1c3c37c481"],["/sw-register.js","40541422e8811632edd0ae52687474f5"],["/tags/C/index.html","cd481a16b568366104cf85a51bbf3410"],["/tags/C/page/2/index.html","d8ba15e0111117e72109aa52dd147a2a"],["/tags/C/page/3/index.html","9ce26c4e46d91857337bc5272086f274"],["/tags/ElasticSearch/index.html","4d79ce8b08399147f69b25c27652a976"],["/tags/GUI/index.html","9760073c7fddc41ee6fe2e8b287d0637"],["/tags/HBase/index.html","49960dc4ff28b80475746380008ac2e7"],["/tags/Hadoop/index.html","8fc253d5b347ccbb7a86a3e5831a25c4"],["/tags/Hadoop/page/2/index.html","88e46f28cc8a4f9d8e53d1061b7333b0"],["/tags/Java/index.html","2db2bff9cec6654f43fce09973980c15"],["/tags/Java后端/index.html","ff056528d9383e9aa78d3333d3c91e33"],["/tags/Java后端/page/2/index.html","70a28a938db687b4127754c2e7c36db2"],["/tags/Java基础/index.html","b4c8bb40059cc62c95a5de5c4d3c4a9a"],["/tags/Java基础/page/2/index.html","1247009daf49a1e8bc4014a781626526"],["/tags/Kibana/index.html","c0a07c00fc5d11374aead8fa9b280edc"],["/tags/Linux/index.html","90635957a27e196105ea39c975e5a8c4"],["/tags/Linux/page/2/index.html","4ad318440242226403f52cfd141b325a"],["/tags/Linux/page/3/index.html","9ea2a6000a5b166c1b1997c1dc1680cf"],["/tags/Mac/index.html","17fe27b3fae11058e0fabec5023be912"],["/tags/Mac/page/2/index.html","e2adc3d10538f63f39801ea651563533"],["/tags/Maven/index.html","df9c61fdd48f641f398e0fcd0cadb4fe"],["/tags/MySQL/index.html","26cf9dfc84e7549e3292555890349981"],["/tags/Python/index.html","d29f9be4b9ea155c987d75aa442f8434"],["/tags/Redis/index.html","7cf2cec52ddbf20c728ab7bcf0ede912"],["/tags/R语言/index.html","47b728f5ee23d86280112e4f0044b277"],["/tags/Ubuntu/index.html","621fad7e5241616f3e293da3afb831eb"],["/tags/Vue/index.html","436ab2fdf28d7cf10475432ee085aad7"],["/tags/Windows/index.html","b6f38ebd713bae82714fcb81d27574e8"],["/tags/ZooKeeper/index.html","19184e70fcc36dbcad247048421c3260"],["/tags/bfs/index.html","537143e249f467f3657ac073e2e1e048"],["/tags/dfs/index.html","838acaf396fddd59ac8416518b25fec7"],["/tags/folium/index.html","b4e6dc80caead6a6467820ff5e84586e"],["/tags/git/index.html","a2bc55cd1ba73c3fa4a9b585cb44046f"],["/tags/index.html","ce32f9e94dadb31e69da85f3d45a1b35"],["/tags/latex/index.html","fd165fa6c6c1bb51fa6d9c0067331af3"],["/tags/中间件/index.html","87df3e79c9065c6abd5bdebb71297ad2"],["/tags/二分查找/index.html","b94ae49b109881edabaa1fb67cd2fe1f"],["/tags/优化类/index.html","2500f15743909e5fa66b8b99be6ac8e0"],["/tags/前端/index.html","eff097770b69cbe89ac7519eb5fecde5"],["/tags/前缀和与差分/index.html","f9ad6f788a5504874335ab7d4e0ccf61"],["/tags/动态规划/index.html","deda4664c5795408065e54a9aa989142"],["/tags/动态规划/page/2/index.html","a4ea8fae0562818caf468f62087dd5ad"],["/tags/博客搭建/index.html","c5eceee57a7d5f3ade491415c0b4ea21"],["/tags/图论/index.html","4620be1f2a6e789386b98d6643923449"],["/tags/大数据/index.html","a71f6f20b3da8c5c33f90866d0a21527"],["/tags/大数据/page/2/index.html","afe54a9cddd8ae1934b4f2adb47cf172"],["/tags/操作系统/index.html","8ae1c2d69813d5839b6be8dc1b89fc05"],["/tags/数学建模/index.html","1d95175334d77a0817979ebcfd875742"],["/tags/数据库/index.html","3e1b8505f9bb60ffd4ed5ae560f92b7d"],["/tags/数据结构和算法/index.html","4e948fb476dba42f989025e12dabd6fd"],["/tags/数据结构和算法/page/2/index.html","0bb264b18c3a6c914bd925cc8bcaf08f"],["/tags/数据结构和算法/page/3/index.html","437793bd5debaa0dcd38b6a1e93cff19"],["/tags/数组和字符串/index.html","2b0db08539fa306fa3cd251f0c31dbf4"],["/tags/枚举类/index.html","4b9985d7109a9bd593a0040e265fb5be"],["/tags/栈和队列/index.html","1883224f8f3c157e6e901251a2722b8c"],["/tags/树论/index.html","46114a4ae00bcc78b1ea8ce1c457790e"],["/tags/测试/index.html","85cdbce55400ed75a60456218f41c099"],["/tags/环境/index.html","cf55c94c3a36916ceb3dd40c35dd260c"],["/tags/环境变量/index.html","428bcdc2a20f75c05e33ec0411bd6e3a"],["/tags/绘图/index.html","e6cb627a8ead4aa3ecba3c44630c1fc3"],["/tags/编程工具/index.html","bdf8d54afe0b86c4b98f7cd53848ab1a"],["/tags/编程环境/index.html","e9d81630e03e89f6cff83d9184167372"],["/tags/网络编程/index.html","4de348e824daf31295cb833ea50d8ddb"],["/tags/英语语法/index.html","980fc9e998b58a0266cf3e05993f77f9"],["/tags/论文/index.html","9f98cf22ed56a4033bfe60594002a65e"],["/tags/资源下载/index.html","f1efeab3d503ff98b5c470f5b7fd9f5d"],["/tags/链表/index.html","b9518c984ed1761ac21fa7fec047ab9a"],["/tags/集合/index.html","10c772932eb4fb0e9ee37c51e0f11101"],["/tags/集群/index.html","27695499e994c177cea9f3ce2dde1775"]];
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
