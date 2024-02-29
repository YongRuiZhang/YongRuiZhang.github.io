/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","561b48fae3d6e2dc6bc916421550ff3f"],["/about/index.html","8b27792c62c63312f6cda047e50d3e40"],["/archives/2023/01/index.html","60060e7bbaaad40be7c914bf27b6f4e0"],["/archives/2023/02/index.html","a783bf67eb952d30eb5c5bda9d0dc847"],["/archives/2023/02/page/2/index.html","ab7594ecaa472a9153e45580644e380d"],["/archives/2023/02/page/3/index.html","235cd1b4e42d995839b7d8e18d4b3288"],["/archives/2023/03/index.html","4b8d7b72db4f2fc3f5d5c74f0fe16400"],["/archives/2023/05/index.html","b04b421c590ce23bb5a4630a19e8fd70"],["/archives/2023/06/index.html","ea3585a836346080fcdfcbc0012fbd3f"],["/archives/2023/09/index.html","95efbc9ac2c5b02816d97b81f11c8061"],["/archives/2023/11/index.html","5d4fd7b077bdd25e23c10a1b53399ed0"],["/archives/2023/12/index.html","a3f30640bdfbfd99a71abcc50db5f9dc"],["/archives/2023/index.html","61407e10731d5845d1ac2d4ba3b79a44"],["/archives/2023/page/2/index.html","7a536d14aa400b9da9afcc71dbe435a9"],["/archives/2023/page/3/index.html","e9a802f43989b0fe2092903902e74234"],["/archives/2023/page/4/index.html","66c6c670cc7013dc3e7ae2f02e0b2371"],["/archives/2023/page/5/index.html","6d92282925b51bfd40bfa44e85daf927"],["/archives/2024/02/index.html","a272c0798860f0e77a6fe3bfb04bf4fd"],["/archives/2024/index.html","16e61d62f79affae8c1a619c3e16af84"],["/archives/index.html","859008a851b5f00d8a752347e471a23f"],["/archives/page/2/index.html","f4450ea1ec092883591f16d454bd5ee2"],["/archives/page/3/index.html","b507d940394daf2d1999f6c82faf8583"],["/archives/page/4/index.html","578af3f03c29ce2c0f6bfd73a6c9d796"],["/archives/page/5/index.html","de6b28d532c95bd22dcbcd55ded3b05f"],["/baidu_verify_codeva-qQP2iZOMLX.html","25031611f54678dddb7f0e77441ea020"],["/categories/Java/index.html","0f174b893bc6a85fc30f2552f1dec063"],["/categories/Java/后端/index.html","0fd20124059bc4021c1924e98b90ff72"],["/categories/Java/基础/index.html","2dda1ab89ba5337a0460405b2d7c6f76"],["/categories/Java/基础/集合/index.html","1e683e39b6f56ed102e311ae02560cee"],["/categories/Python/index.html","51f8d4840afab251734c2ade717b641b"],["/categories/Python/编程环境/index.html","e35303788cb111f4e37f5baa886460ba"],["/categories/R语言/index.html","4621405b6cc6c60398695763f69ca550"],["/categories/R语言/编程环境/index.html","65a95ae9bd593fd58876c99be7b03eaf"],["/categories/iPad/index.html","7e16bed1890292f6eb7ac336da963108"],["/categories/index.html","4f22613fc0ee8d04779ffb94c70ba029"],["/categories/中间件/index.html","75e2316897817890a4183620bdc920d0"],["/categories/前端/Vue/index.html","c62ef1a709060bd7d34097e3d9a2362a"],["/categories/前端/index.html","95ebce373d619762dc86ae4139d4f794"],["/categories/大数据开发/ElasticSearch/index.html","95db22d5bb867b2d27e9567de829d4c8"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","491a1015037c9aad343236bc71188e73"],["/categories/大数据开发/HBase/index.html","b013fcde4f3b277cc80b04bc6ba61435"],["/categories/大数据开发/HBase/学习笔记/index.html","44004c83113cb7bff5a9251340dac25e"],["/categories/大数据开发/HBase/环境搭建/index.html","c30cf9ae0e2c464ed9a797fc3bfe9bf5"],["/categories/大数据开发/Hadoop/index.html","dcb99ac8e013b590e5ffaa8f58db2280"],["/categories/大数据开发/Hadoop/技术/index.html","cf052b906599059a5773cf90d20afb55"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1e790fbdd4c35a32ad2acd36da8019ca"],["/categories/大数据开发/Redis/index.html","6df8ba5a580b73df0e95e6b83f240a45"],["/categories/大数据开发/Redis/技术/index.html","7a51c98df046c8f448383e84fda0d522"],["/categories/大数据开发/Redis/环境搭建/index.html","77d4e8e6a971cbeab2358fed7e2d5a61"],["/categories/大数据开发/Spark/index.html","d52648ffb8ca5ca4c3ead31fbef90fbc"],["/categories/大数据开发/Spark/环境搭建/index.html","3361c95c4b0af1b02af5381b649ba3ff"],["/categories/大数据开发/Zookeeper/index.html","c74257ccbc4156012f0621de0f9d1a82"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8f46785c5fdd6b3b015ac3c05eaf8a17"],["/categories/大数据开发/index.html","f38c168d2f2090d75ae54e0e1ecf2377"],["/categories/学校课程/index.html","93eba207aa88cf3b045814f6c27ba641"],["/categories/学校课程/计算机操作系统/index.html","7d8c02e72a4dd20238ed29a424cb95b3"],["/categories/操作系统/Linux/index.html","9cd0883d4d49a62f59cf9e9075bb20d3"],["/categories/操作系统/Mac/index.html","2052a5a65fedf7e361343fc80ffb4927"],["/categories/操作系统/Windows/index.html","547dbeea7f1b5e4de6bc87a9bd7d9c40"],["/categories/操作系统/index.html","a3512157a014f54dcf21212a30aa1441"],["/categories/数学建模/index.html","0fb387fc6f9cf98db8e039ef9a21b3fa"],["/categories/数学建模/latex/index.html","9c9e5baa111ee3dd95e74ca2d937a1b4"],["/categories/数学建模/优化类/index.html","92245c5909c4e570f8abbed4e4f6bbb9"],["/categories/数学建模/优化类/现代优化算法/index.html","742e47751545364203da9ed4766a32da"],["/categories/数学建模/优化类/规划类/index.html","6115c0c895059726680c760c44814cbf"],["/categories/数学建模/绘图/index.html","960c9703d9e1097421aac421234e2a19"],["/categories/数据库/MySQL/index.html","34570f896763bebbe4058a0a8cc75168"],["/categories/数据库/index.html","86c5d2682640e783701a9aafc9004cac"],["/categories/数据结构和算法/index.html","b897434e7e043c43603b89f0e4c8e297"],["/categories/数据结构和算法/page/2/index.html","c606809d3e2c6ffbf43236812254fadd"],["/categories/数据结构和算法/基本原理/bfs/index.html","9e6c092e70849e6829e29e327baf2ea8"],["/categories/数据结构和算法/基本原理/dfs/index.html","593dffce3e9a1912dc3b1a20372d5408"],["/categories/数据结构和算法/基本原理/index.html","d0eff30402763d4faf44f93df32fc54a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4bba35e7de587a810f22730555d71569"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","dd14de52d14340cbe572f1666426a31e"],["/categories/数据结构和算法/基本原理/图论/index.html","6307de7d95e1ec8b86b81ffbff60516c"],["/categories/数据结构和算法/基本原理/字符串/index.html","cf42370e17217fe2a699459e0f8115a9"],["/categories/数据结构和算法/基本原理/排序/index.html","f504089382133c27e73cb5e73604953f"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fe9d3f69aba54175eaf4fa9d6f9f6b2a"],["/categories/数据结构和算法/基本原理/数论/index.html","88965d2c32efa2a10459608f524c2d62"],["/categories/数据结构和算法/基本原理/树论/index.html","754f5a363c7e9354f9294f891157f744"],["/categories/数据结构和算法/基本原理/链表/index.html","60fb0c3cd019a679fe443471339cdd4b"],["/categories/数据结构和算法/算法题/index.html","5dfcad97b4083ffe6d82068e93fb4b0d"],["/categories/数据结构和算法/算法题/二分查找/index.html","4489b79a32339d7f14420fc839cbfbfa"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f1ccb5a925370628ba071a55ba15ef15"],["/categories/数据结构和算法/算法题/动态规划/index.html","b2b905b05c3e12e5f6943e409bbfcecb"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","910899e392c80e570b46090d8abf6816"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","41d89be76f9f4d69315c0779e2d9c930"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a5227ac38d7cb0920c0e36a97174b007"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","68dcae7e7df8a5c1be1ccdd17bfbb2b9"],["/categories/数据结构和算法/算法题/数论/index.html","c4ecb336292e088370e8f200e389bb0e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","eeba9b56fdb64df512681bbeec7ec94a"],["/categories/数据结构和算法/算法题/树论/index.html","ae4a7a8525d0ce3a5c3d893367b675c2"],["/categories/杂七杂八/index.html","f34a891e234f538294dfa8058da5f8de"],["/categories/杂七杂八/博客搭建/index.html","7df78d49a778cc5c2198e9411fd5158e"],["/categories/编程工具下载/index.html","f89621668a049d0c280c4350826bd471"],["/categories/编程环境/index.html","a0847517c8928624ad61ec973c3cc00e"],["/categories/编程环境/大数据/index.html","36741cc18648f5685a49fa10e472e090"],["/categories/英语学习/index.html","2220c18287ea5c7267cddecbea706d43"],["/categories/英语学习/英语语法/index.html","25ec33cc6ef2cea25ad8b98650f2afbe"],["/comments/index.html","302c97b9c5a2283150630dcd5c63ad69"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0871caa60ceea564abd2438fc6f2227c"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a60d6b0ee32aa6489094548dde874245"],["/movies/index.html","5d1c6ae29967f63bbd00e6ce9f862da2"],["/music/index.html","cadf9c5608c746c9581d5acb33ca290d"],["/page/2/index.html","5888278224111a2c999f2b398d12dc1f"],["/page/3/index.html","c58b92aa5cfe1165e107a402e2d43db2"],["/page/4/index.html","022c2dc7a04b79da8336737c0bd55205"],["/page/5/index.html","3da86e11f374bd0837edb211c413be95"],["/page/6/index.html","a8f2c4d15e07ce54a9670824f68d3180"],["/page/7/index.html","8d832e03ae0aca80e846f9058c0ba6f0"],["/posts/1021360842.html","16e4d1abdbe4b5311bfb16d58af7f4af"],["/posts/1120620192.html","d1a85485b992b0fc14bbf51520a115a9"],["/posts/1137707673.html","e94644af10f43921dcb5b7ff5556dd88"],["/posts/1141628095.html","3ef68c5a7139eb90269d84f85cee150b"],["/posts/1168613674.html","aaec2f552ea24ab8dfd64b0e0f994076"],["/posts/1219920510.html","b609ad5ad353abfe02b85fbb3a2581df"],["/posts/1222166338.html","471f906003f6a98cf92ef6234d345ca4"],["/posts/1259097482.html","3f0511b57c2a70aa55621afd5ec81c7f"],["/posts/1271036369.html","441ec125ca02d89b8c20f3230ef6c585"],["/posts/1312847445.html","a4584139abb43addcfafc571a8542ae7"],["/posts/135355774.html","29177525b71b68f36b5f6648e4123bb9"],["/posts/1375344716.html","02dc36b71b476a4f43d02491ff0bdd0e"],["/posts/1388991698.html","46c34f960b00dd509712531ab6b9cf81"],["/posts/1410315814.html","efa5e94d3e8352c2e8bc423238e06060"],["/posts/1452790229.html","49e86da9ebc64ea823f505478fcc905a"],["/posts/1470079884.html","cf209e1b946ac59231d2975c34adebb7"],["/posts/1470079885.html","f929f1b23e937ac49431d874b32795d3"],["/posts/1470079886.html","3a8025225bba5304576124dbdb597da4"],["/posts/1470079887.html","367aa118089eb02c68a6f96ac818d809"],["/posts/1498536549.html","7db03f6ffebb926e9b17cce6bfdae064"],["/posts/1539568593.html","2edb75f05528e49d2d0865ee3b695397"],["/posts/1547067935.html","f364b5e77f432c8578b17ea6f750f918"],["/posts/1557866301.html","bdd6794662ac38131238cb7c76511405"],["/posts/1571776361.html","1be859c06ebc69b6691262b1fdb91272"],["/posts/1605124548.html","b399a3b635bcfe080e84136b470a409c"],["/posts/1633036852.html","41c5132cafc9525b57c25d64ff663a86"],["/posts/1667740714.html","ff501b753d15fb0e6808351518cce2f5"],["/posts/1674202625.html","77e0bb8851ae038f44fa3e75581ced79"],["/posts/1765123828.html","cc126e5b9c160f73505799abfed7dbda"],["/posts/1767336200.html","41116382e27f8296ff48f10569e259ce"],["/posts/1776114197.html","3978fae081797b9c0d0249f702a749bb"],["/posts/1817748743.html","40457a3731579843cd931a63aa6e5519"],["/posts/1925125395.html","1856fb8250114ae0d71ea5f2b3d349b6"],["/posts/1966191251.html","0c69cf1fd28e2c29d437a32ead7b5bd6"],["/posts/1987617322.html","dd2c4ece41f5d431caf5792a6f94a030"],["/posts/1999788039.html","d7bd27388e68e05e14bf08d64b17cfba"],["/posts/2075104059.html","981f4fb0efbc3434200d44496204b85d"],["/posts/2087796737.html","c87f360d3b1351f57ec26282ebd2971f"],["/posts/2106547339.html","4c5ca88536597e51b3858ab5e320b53d"],["/posts/2207806286.html","5e65bae28bc28bdad6a2b1bd3dd40ee4"],["/posts/2225903441.html","fb20900d95b4a5cad5c6c66c4f1789a0"],["/posts/2265610284.html","4b2a20c65085b06e329a7e994fbe831d"],["/posts/2281352001.html","3ce2eb97d37e3d1149ce191803767cd4"],["/posts/2364755265.html","1cd79f485ae940de72ab00ccebb78365"],["/posts/2414116852.html","da13725cc7ba753ccc6672a48001f7df"],["/posts/2421785022.html","b8eb1c14efd4611b5b544d8cbfb1523f"],["/posts/2482902029.html","67e949eccbaf905bcae81e5b3875389b"],["/posts/2495386210.html","0f1d5692a6e41b18b814fe543d8393db"],["/posts/2516528882.html","43e2cb540252ebf920b85067c427ba82"],["/posts/2522177458.html","8a87fe3ce952cbc17d10ffa71f23439f"],["/posts/2526659543.html","aa102610de1112794f992ebb3737baf2"],["/posts/2529807823.html","13deae7cce7b47c0fb6db4a615263071"],["/posts/2596601004.html","099d12679993df4537e713f4fe99dd7e"],["/posts/2697614349.html","fe72a7afb1c455594906f86528df4ae7"],["/posts/2742438348.html","c20f6a1d60698713ce66795ef1607e85"],["/posts/2768249503.html","e9b7c919ea5e8b00112f6d164eaa1ecc"],["/posts/2864584994.html","4e0dc1927bd6ad9cc2c8fddf3c011d90"],["/posts/2888309600.html","86e6cbaaaa97cbff7889b54779ba1d9d"],["/posts/2891591958.html","416adfa52be0f15e04dbb3fab95fe58a"],["/posts/2909934084.html","bf6f685e1eb1e01b294cc19d61a9d752"],["/posts/2920256992.html","1e925e2d4e1cec15571775865cb10ce8"],["/posts/2959474469.html","318e81a6f474205e573d25ee901d9cb1"],["/posts/3005926051.html","4fb07ef0cef1f4afe7587044a9aaaedb"],["/posts/309775400.html","d6675bce229fd286eeca1846f8a3c0a7"],["/posts/3156194925.html","63927c9f7c37db95567f63dd148f47c8"],["/posts/3169224211.html","41a932a69ffe4cdc29bc11551c9f62f7"],["/posts/3213899550.html","fce33a3e359310cc84ec2a6163f623b6"],["/posts/3259212833.html","19c28b1ee0d555c560eff9c613dd128e"],["/posts/3265658309.html","75e30ecdfde8e8a4e50149eca109f335"],["/posts/3266130344.html","f672f10582a32df96aa2eff730dbdd46"],["/posts/3292663995.html","bb750883ccc2e4cda3feed5680a5640a"],["/posts/3297135020.html","76605deaae75093f844b3e24802f6446"],["/posts/3306641566.html","123a5fc18feae989a5d13a973b628d0d"],["/posts/3312011324.html","9221e2454da0189cdc73ea56e6c7554c"],["/posts/336911618.html","0600d52f1c3db36b2822e813966f57be"],["/posts/3402121571.html","d226e57a6684013bfcd2a60dfc57d41b"],["/posts/3405577485.html","0b061ac884af4e9eb41f38b602b4bfad"],["/posts/3498516849.html","573d0c5bceca1776faf68bcb390cea2b"],["/posts/350679531.html","5dc3e9bea41cd774f86302bcc534302b"],["/posts/3513711414.html","61d09b51554ac060d826cd1cecfc6242"],["/posts/3523095624.html","9ca06788d0d3b5d84696296f4de13f21"],["/posts/3546711884.html","850aed4026afb11922e0dab0b232f210"],["/posts/362397694.html","68a6aa9458049b435375365e6410ffe5"],["/posts/3731385230.html","676ebd13ae7d34f1830acd9f0ef6387d"],["/posts/3772089482.html","ecec1c3c9f1a14184f77bf39324d247d"],["/posts/386609427.html","dde21f8e5e17691e2a4ec580a9cf903d"],["/posts/4044235327.html","0667da177dba621e1014044daffcf3c7"],["/posts/4115971639.html","fb191ac848f44b28afbdb44894837c3f"],["/posts/4130790367.html","d3463ead49637d0f7b033db8d2812de9"],["/posts/4131986683.html","1c9106b8de2d79549b130c8d18ddf471"],["/posts/4177218757.html","f23c77b7f93ab2f33b061dfe74b65237"],["/posts/4192183953.html","566a6d0293e31ae094658393771024ae"],["/posts/4223662913.html","0160d4eaf1cd8aa72fe70b36261d13b6"],["/posts/4261103898.html","1224e8955fde9f72cb1bf4da2b3e6023"],["/posts/4286605504.html","1473f44d2f698cee2405f99972c80ad2"],["/posts/449089913.html","a567e1d2f8fc0ceb8f020dd5b26fb775"],["/posts/469711973.html","20547dc8698ba0324e746bce9d8063b9"],["/posts/482495853.html","6626a32bb76d8f7c57883d63e4a4ae0c"],["/posts/488247922.html","d889ad745c578d0e49aa27cc10f16c9e"],["/posts/517302816.html","9732e23cc814d4373a80fa00086a7da9"],["/posts/570165348.html","510c5d0abc01b12e4317ea26e4e6875f"],["/posts/595890772.html","2d4af68df79b859fec8cb104b347fb11"],["/posts/67485572.html","d706f26231f7d595caaf495e38cc8f25"],["/posts/694347442.html","13c72a21189232673e8c1ded1fd0e641"],["/posts/707384687.html","6c3014566eaef311352c023395014bdf"],["/posts/71180092.html","bc6e7fdd42cc8dfa216443b7dcf9dda3"],["/posts/716459272.html","8128c811178f247aacc53b9a65870143"],["/posts/765481613.html","3ed0bf6f1ebe3e54a8eb85e85e81cfd5"],["/posts/778231993.html","7021e044093234db3cead2340f69f8be"],["/posts/795397410.html","0eb85c8c7b688d7bfee46608f2074971"],["/posts/820223701.html","4ecc3cc66df8776418a04551c3184cec"],["/posts/830372185.html","44cecbf3f7e26ae5d90a81aef00cf914"],["/posts/88294277.html","5f9dd9a6879045a2df8b346dd8e31f0e"],["/posts/939963535.html","6266b5b3150fee4ea5d2a2b221344122"],["/posts/983786067.html","6aabfe6e6d3daf8a05477d77ce649bf7"],["/sw-register.js","946c8ce9ead4b61b70eb533be9785daa"],["/tags/C/index.html","b4bff64f774acdee2996a3547353cf32"],["/tags/C/page/2/index.html","42f7bf99a6b44b41266f1103f6cfd070"],["/tags/C/page/3/index.html","adc2b02b2b13e859348a13907183f503"],["/tags/C/page/4/index.html","be0f493b3b13f3eadcfb0e5bc39003e0"],["/tags/ETL/index.html","c47fbb0730a9b6d5be40629ce0ecfbea"],["/tags/ElasticSearch/index.html","756070b2def3f4db9d764b125447b7fd"],["/tags/GUI/index.html","41cb5395a53b1d4b66c144452082b6a0"],["/tags/HBase/index.html","112c8acedfffe4f3a7837fdadd9de48c"],["/tags/Hadoop/index.html","09f9ada7e3114f696614f18b21482be6"],["/tags/Hadoop/page/2/index.html","171cd56e82ab5a2dacebc09ab6c51c51"],["/tags/Java/index.html","a800056cd8f6e1bc6e7f7be619c8a420"],["/tags/Java后端/index.html","8b5b29d0edf9cb72247d4afe24e5398b"],["/tags/Java后端/page/2/index.html","9fa85b671dd83783bfcbf39ae194eb6c"],["/tags/Java基础/index.html","1da506e905b407cf6f4cf2a07192d013"],["/tags/Java基础/page/2/index.html","65693a127dc726158c982c3649bec917"],["/tags/Kettle/index.html","0f9e8ece03eaf662bdd6c109202fc801"],["/tags/Kibana/index.html","4bc98402a94f03b342609a17d0a4623a"],["/tags/Linux/index.html","94ab9c008941f24859777bb43d57a480"],["/tags/Linux/page/2/index.html","fd96a084f3f503590b4bc023b059b3a2"],["/tags/Linux/page/3/index.html","4c1aa0e7a5b9ec1b16bbaa620d723865"],["/tags/Mac/index.html","2fdbae9c1efa5c4cfd839b0c3b10c139"],["/tags/Mac/page/2/index.html","61aca18aef1682c263b33d6bcee87438"],["/tags/Maven/index.html","a55545f058c69cd8292f864f20939309"],["/tags/MySQL/index.html","4d54cf2cf5ab6546bec1ed6b3f104c08"],["/tags/Python/index.html","6fd183333b4002f02e64280c6e43e984"],["/tags/Redis/index.html","925fbbccda5335a0644683cec2f4827c"],["/tags/R语言/index.html","e13fff39fc7b1d0559898f0bee37b4ad"],["/tags/Spark/index.html","e70220037c67fa7cb5cc7523be945cde"],["/tags/Ubuntu/index.html","353173b87bcc4029ef2f71e9ca829135"],["/tags/Vue/index.html","3fe4640632b1d86ab815dcde27a64e9e"],["/tags/Windows/index.html","ead40768f934fd7d636dcce2674e74f2"],["/tags/ZooKeeper/index.html","d9107375d9b1bf0a9b01a0423f410b24"],["/tags/bfs/index.html","10b8a92a88a95d2cfd91971c719f8ce0"],["/tags/dfs/index.html","802cc25cd35d13e77815bd2e8c055601"],["/tags/folium/index.html","a85c61e427cc189645d6c901fc037d80"],["/tags/git/index.html","f494153dfe43409eabd85f27ae83507b"],["/tags/iPad找电子书/index.html","c74c19914fc8c6fccad765a96453d4c7"],["/tags/index.html","d5a7eb18126b752d48a788b354a2c2fc"],["/tags/latex/index.html","89b9d8a02acf3e61665b697541676fa6"],["/tags/中间件/index.html","46ce0bf535332e7d295500ea94dc358a"],["/tags/二分查找/index.html","ba52396a92c22762ead85a6fe96e2256"],["/tags/优化类/index.html","c0330cff42a9fd5c0fe0ee40231fbd85"],["/tags/前端/index.html","a82ed2851fd7622bc6a95574bda634b6"],["/tags/前缀和与差分/index.html","88810498f78278f9eddcbc911973ff7c"],["/tags/动态规划/index.html","91da73565a3ff8bba313dab48bcc4bd5"],["/tags/动态规划/page/2/index.html","2ed326b185d316450f31a70e07383b81"],["/tags/博客搭建/index.html","69a2fbd0c10c314f145a909da2f79205"],["/tags/图论/index.html","3540ac8875dc519b9fe7dcbb19fece97"],["/tags/大数据/index.html","982a894ce95dfc10895b5f0d33cf852f"],["/tags/大数据/page/2/index.html","2f43d5162f62cde51369a6126d3b642f"],["/tags/排序/index.html","2601231897dacec519d4255e648ef1bc"],["/tags/操作系统/index.html","409c7f0b13c1000f2e54b7b4727e03af"],["/tags/数学建模/index.html","b21636d0b530559298b83454547f7332"],["/tags/数据库/index.html","5a5dfab3acd037c6115e2ddcbd904587"],["/tags/数据结构和算法/index.html","f92423103e1a19d32b02027260aa35a1"],["/tags/数据结构和算法/page/2/index.html","645fc17fb53f7bc2951dbab33028301f"],["/tags/数据结构和算法/page/3/index.html","a6f9d431afee71f2c7b8ec6c2dcd6aff"],["/tags/数据结构和算法/page/4/index.html","8a8e9c01b607fecb5d192a4e1eb2ecdf"],["/tags/数据结构和算法/page/5/index.html","b966f4c5344cbc7519b26f11605f1441"],["/tags/数组和字符串/index.html","f6c91f199b0112a8241bfde0884b9242"],["/tags/数论/index.html","9affc31c321e37075ce0176651919c2c"],["/tags/枚举类/index.html","26a1bf2a72c85275924721a8419e456c"],["/tags/栈和队列/index.html","07fb3d5c6d3747d1fb4e9a2ddfa18f34"],["/tags/树论/index.html","e1735e90c485c89a3d35d1a1fe741027"],["/tags/测试/index.html","0ef0ad28dd2832e716bbfccf3090211f"],["/tags/环境/index.html","a0256a8000974e72680411a7b07ae051"],["/tags/环境变量/index.html","2d15f0dfd66ef60c0beb654cb3650b71"],["/tags/绘图/index.html","d53ee08bef9ea9def4590eaec32e1a75"],["/tags/编程工具/index.html","1f00b18c3ddb2418cd173a6305efd96a"],["/tags/编程环境/index.html","d73f724b24fb663f9b6b2e0d42be0c20"],["/tags/网络编程/index.html","3d0d2e67e3233b2a09c9257a59fb2641"],["/tags/英语语法/index.html","d421fe34dae7262da43e28f8f8a87312"],["/tags/计算机操作系统/index.html","adb55940570fdda3b077df5759c6da5a"],["/tags/论文/index.html","b7073d5c3e5c49458e7060b9a5f0efd1"],["/tags/资源下载/index.html","e928d057dd6b4f0d55c26ee552617376"],["/tags/链表/index.html","447b03ad2a5b5ba8e0332e8b3a069394"],["/tags/集合/index.html","0ff415ccef1f56b56255fde832d1d638"],["/tags/集群/index.html","a526ce1ac7f7f3a792beed275ad19e6e"]];
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
