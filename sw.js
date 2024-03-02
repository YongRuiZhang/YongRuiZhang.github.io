/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","38809bbe186686853b7dc15c7b8c70ee"],["/about/index.html","c35e03d5c28a3fd1df9423c73905be2c"],["/archives/2023/01/index.html","cd18f8c56fbf89939f2320992344903e"],["/archives/2023/02/index.html","21f21d4733667fadec26f8f578663af0"],["/archives/2023/02/page/2/index.html","783a5a0b9f8fd70a1a5f07c0c7166f1b"],["/archives/2023/02/page/3/index.html","f702d726f6b3dfa547c30d63b4f3da99"],["/archives/2023/03/index.html","5f269d42d275eeb82b7fd22d6cbf5028"],["/archives/2023/05/index.html","1c290d8146356f25f9da9fcb0a6e4f5e"],["/archives/2023/06/index.html","ce57a59128e76a3757dcbdde6933b885"],["/archives/2023/09/index.html","137b67a8ec08d327377795c6f8472c52"],["/archives/2023/11/index.html","4dd474865cfc366d1c38d73e034aa4a1"],["/archives/2023/12/index.html","fe76ce9fd1809b38786305d0cedb9645"],["/archives/2023/index.html","594321819c31a59b48860bb18dcd8b75"],["/archives/2023/page/2/index.html","85e9450e35586bac1aa840ddd84eb5e5"],["/archives/2023/page/3/index.html","f180890749e37643c4fa4355cfdd9646"],["/archives/2023/page/4/index.html","1008001df8cec94dcb84adee01cb50b4"],["/archives/2023/page/5/index.html","3fc119924cfd6840adcf2ead097d7dbb"],["/archives/2024/02/index.html","675a2ba02b62dd09a39599a7237360dc"],["/archives/2024/index.html","93029828eb3269d3a0348c69af97b345"],["/archives/index.html","4bd25ce929373239e1b87f1c39879652"],["/archives/page/2/index.html","c00916b49dcff97a1276c7c24958d2ed"],["/archives/page/3/index.html","1b2a756b888c3c90cd26300cd4166db4"],["/archives/page/4/index.html","85af171a57d98d27d8eb5cb0e9508229"],["/archives/page/5/index.html","e09977ef71c1127489fb581461835985"],["/baidu_verify_codeva-qQP2iZOMLX.html","baddc14391b50f7bd549f6f2232aeae8"],["/categories/Java/index.html","93d6a2b7a9d69cb6b3404dfc92dfc754"],["/categories/Java/后端/index.html","83eeb7cf3ab98cbb98336cd7aea227cb"],["/categories/Java/基础/index.html","a522173b680c5ae4002d0ca8b40f8b8c"],["/categories/Java/基础/集合/index.html","d13e2276e08c4df66203d571773e4354"],["/categories/Python/index.html","d67a89785da3068f878170100a3a2c66"],["/categories/Python/编程环境/index.html","469e2c311926a9c012b2e0d8ba256b5b"],["/categories/R语言/index.html","3d66434c61286a01e0ef74b9ec682382"],["/categories/R语言/编程环境/index.html","ce4a5cdb017ec08f88f30cad18bdeda0"],["/categories/iPad/index.html","20c105c85d7a04d190c2785bea07e3d6"],["/categories/index.html","18526a8130e1a4454262d12d6ef2f76e"],["/categories/中间件/index.html","0f7a331583249179ae199a112ab6dd9e"],["/categories/前端/Vue/index.html","949063abd3c485a922f72c3c811b471e"],["/categories/前端/index.html","17a737352244f8d1272adc4931f8fc3f"],["/categories/大数据开发/ElasticSearch/index.html","583c97e67ffcc2560b0913eb347ffae7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","8fef794526481e7826737188bc2adad8"],["/categories/大数据开发/HBase/index.html","57d4169e2e6e972602e8a3811108e9e0"],["/categories/大数据开发/HBase/学习笔记/index.html","772d8766682ca265637114ec612fff6c"],["/categories/大数据开发/HBase/环境搭建/index.html","6aee2f2b3eeb9a657f9c6e9223de9eaa"],["/categories/大数据开发/Hadoop/index.html","547a91e7222337fd6a0cfceb7caf4473"],["/categories/大数据开发/Hadoop/技术/index.html","4248f3c0088f8dc4405e967bb08e4d4e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","cdaf45b3e86abe69b5484e09339f4646"],["/categories/大数据开发/Redis/index.html","a839997973e4dc1b46afdf5c901390ca"],["/categories/大数据开发/Redis/技术/index.html","03c02c4cc5f67938623bb99b46e100fe"],["/categories/大数据开发/Redis/环境搭建/index.html","1bdd1b5620d16de2d2e2a16c3add62b6"],["/categories/大数据开发/Spark/index.html","5df45ff67e901db8401d2b77e2a9c665"],["/categories/大数据开发/Spark/环境搭建/index.html","c008d5a4ac3dadf07e6832e146c1986d"],["/categories/大数据开发/Zookeeper/index.html","67327225a69e87e13fa5c72850dacd85"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","909d047c49b7b1b8352957783dba638c"],["/categories/大数据开发/index.html","b887f160096ef0753646f8f7e9a5a48b"],["/categories/学校课程/index.html","04f8b0dc3ffa30a89b2286935ad5c63f"],["/categories/学校课程/计算机操作系统/index.html","2eaebfb66993fd6c07c38d29b103c086"],["/categories/操作系统/Linux/index.html","3591ab011256d53abecb3ba98bbd9bf6"],["/categories/操作系统/Mac/index.html","730fb2049c800f4a7658997b5240e31d"],["/categories/操作系统/Windows/index.html","8b8be94d4b380275961724b70a62ae05"],["/categories/操作系统/index.html","1ba84e566107910a0dfa43a4f007c570"],["/categories/数学建模/index.html","b31c1c74e13a439a65fc3fd2e2c72ab5"],["/categories/数学建模/latex/index.html","5fffeb851772242b6d2e1a595a23b179"],["/categories/数学建模/优化类/index.html","95050a032ec8eea93c703f68f689d66f"],["/categories/数学建模/优化类/现代优化算法/index.html","3be9b40672bf04492e861e72bbcd9501"],["/categories/数学建模/优化类/规划类/index.html","511c25a3959a031b7b715453d20beade"],["/categories/数学建模/绘图/index.html","c4263714faa20843fdd310ad1716db46"],["/categories/数据库/MySQL/index.html","a4c4da9782f6bdb2f8974c8ba1d6752e"],["/categories/数据库/index.html","054eeeafa35880f15482d30b2dc46f8a"],["/categories/数据结构和算法/index.html","511c326dfc07eb2de4b6b2596431e493"],["/categories/数据结构和算法/page/2/index.html","a05cde98a8b6c67e70e163558372e935"],["/categories/数据结构和算法/基本原理/bfs/index.html","6480e1991bcece9dd0b303aeb6c2e2e3"],["/categories/数据结构和算法/基本原理/dfs/index.html","06b7a2997c6bab13806213301a97b655"],["/categories/数据结构和算法/基本原理/index.html","c74403161016de07440a418d431cfe8d"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","91e4aab0aa98f3ec76ff62561f3fbacc"],["/categories/数据结构和算法/基本原理/动态规划/index.html","8a48ea24769ecc925ec9be03f6cfa0c6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","6281ec6bbcf23454e191033395c3241f"],["/categories/数据结构和算法/基本原理/图论/index.html","a3cf88c8ae8f79eedcead00a6afff313"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","60058a100906b806553382b7493614f0"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","a8dc6d3b82d581271290d59848c4d50d"],["/categories/数据结构和算法/基本原理/字符串/index.html","d31583b1ef5094f4c751d91b553b2c6f"],["/categories/数据结构和算法/基本原理/宽度优先搜索算法/index.html","50d7cd1663ce603c6fe8777ff1c5a2a7"],["/categories/数据结构和算法/基本原理/排序/index.html","807817b1840a50da7f554aef77b3d956"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","425e36fe4485b7d72300e85bf5cd98b1"],["/categories/数据结构和算法/基本原理/深度优先搜索算法/index.html","47b13c93c73902ef3e9cf19e5d1a694d"],["/categories/数据结构和算法/基本原理/链表/index.html","773c7f2ff43cdb7bd09b803334c4c1b0"],["/categories/数据结构和算法/算法题/index.html","085d92a468ae24e6d02606adf7c08324"],["/categories/数据结构和算法/算法题/二分查找/index.html","3e23ecad290b097b6b2798ec16cd9165"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","baf87f41af9e4f59d11b740ffda8b12d"],["/categories/数据结构和算法/算法题/动态规划/index.html","f0fac21708313388244b0b93c649373a"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","490375f06884737fd291fef4ebeae09f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f64e1acc80f2215b71a68617e7f13ea7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5252cc5cf28d737e133bd4c4b1c7592d"],["/categories/数据结构和算法/算法题/图论/index.html","1b618041f9cdd5c48a009bbbf1c814c4"],["/categories/数据结构和算法/算法题/图论/树论/index.html","8f472d7f966f986a469f88c0bc3597c8"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","94bb82460dba6485df9a13ff7fc74648"],["/categories/数据结构和算法/算法题/数论/index.html","6c12018c8ee6cee67fc81bef0f79ffb6"],["/categories/数据结构和算法/算法题/栈和队列/index.html","9c5b254acf716747121fdb914151adc5"],["/categories/杂七杂八/index.html","1f172048f407326da553058f83839aa7"],["/categories/杂七杂八/博客搭建/index.html","069dba2fb3128ec60ae452283e9ae0ce"],["/categories/编程工具下载/index.html","f61a77987d6eae5437b9df3279b915f7"],["/categories/编程环境/index.html","9b0d1b310fefa12d9e648921d3318ae9"],["/categories/编程环境/大数据/index.html","69e02c9e5a1c1ff9918425ef38fac1a7"],["/categories/英语学习/index.html","d9a07185c1849e77c93880a5134e8ae0"],["/categories/英语学习/英语语法/index.html","7ba5227b50a6241484d421f3f08293ae"],["/comments/index.html","916d90aa3d485e0fa042f68fd5be95a7"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0aca1c572d09aa014f9a246a848622bd"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","2e0b20cf9351bab172f70fff6e19d4aa"],["/movies/index.html","472a831e2d9e9869797e992fa894a268"],["/music/index.html","2effc1adf7bf638084b0831369ea3935"],["/page/2/index.html","ca528e0d661b83be5c75687420d8c6cd"],["/page/3/index.html","c1842b377df4847641fb77e8a85b8371"],["/page/4/index.html","8ee077ec57d1446dce5030020b8dc45f"],["/page/5/index.html","e1667ece77b41ebcb318b4681299aeda"],["/page/6/index.html","33832fdd18dbbf4f0ef8908c9bb20d0f"],["/page/7/index.html","444bfdb6dbd44b46eeb5c48bf0e83b27"],["/posts/1021360842.html","bcb7b04220c78eebb53ff46c5af41c3d"],["/posts/1120620192.html","c0ec23ee67e9e3f04154048caaef62b5"],["/posts/1137707673.html","423305ccda0044a596c8c2e6ef327ee5"],["/posts/1141628095.html","4476889715fdb93b63974b0dbf50db94"],["/posts/1168613674.html","ff70fac31df52cfc4cdb480e4f96df27"],["/posts/1219920510.html","9f32cb896cd42155a872dce980880efb"],["/posts/1222166338.html","f414c67e3ac122ff3a67d5ad37f85e59"],["/posts/1259097482.html","31516ed3cc137276ef61bbad70e232fc"],["/posts/1271036369.html","a1a357d46ab173cd26e781fd7c1c7260"],["/posts/1312847445.html","36d9930a67af69e22ed68cd164ec81c4"],["/posts/135355774.html","5566c720dc0f86486c2a35f3dee5a16e"],["/posts/1375344716.html","2beed1914d00b78e4675292fdb30a207"],["/posts/1388991698.html","7848a14e0da517360e10f2ca62cb2e54"],["/posts/1410315814.html","4216e838b1f7ae0758bcea69b4ea73de"],["/posts/1452790229.html","af9f2b7dd8c117c44bee9652d3139842"],["/posts/1470079884.html","02ce97c363dea5f4481511c1a2103716"],["/posts/1470079885.html","265963cae600a98b5b2be347d3316185"],["/posts/1470079886.html","998c08528783ade5b35bd689c1ff30db"],["/posts/1470079887.html","f0f5317ebff7ce8e10cce65ec9d174a2"],["/posts/1498536549.html","7890a1c5c41de1ecda9af51a06f5d025"],["/posts/1539568593.html","5ee2cff6c11f4b043c94adc69599ed43"],["/posts/1547067935.html","3c55223afc4544449be375ca1b613566"],["/posts/1557866301.html","5a89aff231fc674689f1d9ec03309544"],["/posts/1571776361.html","5ceec3a641410ad9e609d51fc3345638"],["/posts/1605124548.html","cfd10994157bfc6fd98e570fa08c19c7"],["/posts/1633036852.html","c8ab625a09de13e6b8cf49cccaa42be3"],["/posts/1667740714.html","4cac3ad725e1cd94e024375be08553a9"],["/posts/1674202625.html","031268a961fe3c13d3151eb700451c42"],["/posts/1765123828.html","9043a22fb57b17e4e91b8f4f68bbdd8c"],["/posts/1767336200.html","c9c0833f0e0890d07ad1367b78cdbe2a"],["/posts/1776114197.html","969491baf34830dbe5f597ebaa7ec334"],["/posts/1817748743.html","f209d35f42af0e406241e9e277322f20"],["/posts/1925125395.html","f382608109a4579ca34ad314eb6a07d3"],["/posts/1966191251.html","c46b56ecf7634c5560e417718ceff146"],["/posts/1987617322.html","4bd41a2e8bcbe907d2f7693392a3fd44"],["/posts/1999788039.html","f6f9e2502d7f27138a3c0f33c6e12b26"],["/posts/2007534187.html","0085092e5234478af26b34e0b7cdeb05"],["/posts/2075104059.html","8689372411e24de111bf7f0427876414"],["/posts/2087796737.html","ef3bf3f964600141a9655586df0f377c"],["/posts/2106547339.html","3e07c5065db3cd11cc05c5cd4729ad25"],["/posts/2207806286.html","0634ee3b8dd90129893deaa8bed70573"],["/posts/2225903441.html","bc5a780c4d3e2275c449de62d6c61ddb"],["/posts/2265610284.html","4c74d64f49653953bb6ea3698bd50794"],["/posts/2281352001.html","56875daae639686ded02df968e789291"],["/posts/2364755265.html","78ba89d3d4fd83c6d3e5331f6097beb7"],["/posts/2414116852.html","1c63cd0929eaad43e8c247100d99b76d"],["/posts/2421785022.html","93988c44b520a30ea977839a72eac47c"],["/posts/2482902029.html","b68ae89affdf2ac3a13a9c041f1888b9"],["/posts/2495386210.html","ddc618b3e3c9a0ec4ff4e8f88d36ad09"],["/posts/2516528882.html","54e0c84388195a3879a2dd791779dfce"],["/posts/2522177458.html","402ab73c365f177ced887b5f2b8e3713"],["/posts/2526659543.html","015a5e7e3eba3988711c17a1f9008835"],["/posts/2529807823.html","5f6fcc4f39b51967696b044269a5ed4d"],["/posts/2592249117.html","9f2d16f2225757de30bf7918a8d9e8ad"],["/posts/2596601004.html","5dba39b22f07d063d161b5ff96d78a95"],["/posts/2697614349.html","76940258c399fd7d224b6f374a22729d"],["/posts/2742438348.html","876f236c1b0c1bf04588f718e5d0ff22"],["/posts/2768249503.html","35ee37b7a25b0f6135078159e6423fb4"],["/posts/2864584994.html","8e6369c37232ed33c96e1311045d85ea"],["/posts/2888309600.html","578e99a3cda730622c63576cfa9d5df9"],["/posts/2891591958.html","e0c7f19b1311429130213f8a74ca175e"],["/posts/2909934084.html","709667c35a0f3cdb528e261da3fd6f29"],["/posts/2920256992.html","7a9d96c3b3d3264e9e156681ffbb7179"],["/posts/2959474469.html","21a8bdd25b3abbb83b2c20cfe82ecce3"],["/posts/3005926051.html","b2086f3f5ced5ff6e5e175fe769d1a44"],["/posts/309775400.html","bb7f4b7b1983dee02561c42ffa2984df"],["/posts/3156194925.html","431668dd233a30b0d758a894775c2aa2"],["/posts/3169224211.html","ec1ad8209eaa5e69cd965ac1bad48887"],["/posts/3183912587.html","f5b7c54fe1efce14cf8251dbd28d6540"],["/posts/3213899550.html","2b5c59b0330ebea7be6738f25bf9aefb"],["/posts/3259212833.html","047a4c9311f75e96a8dbb0d3efe9c094"],["/posts/3265658309.html","7570d4b40172a8749c806c4d6f7ce2b6"],["/posts/3266130344.html","41d3462f4aaf99de27cd8d0e5dd9e74d"],["/posts/3292663995.html","0774bcbf8642782724961e2f28053e0f"],["/posts/3297135020.html","182c7aaebf5cd7b91613aad81959c353"],["/posts/3306641566.html","4acee72616805aa5d9424b1171f1c90a"],["/posts/3312011324.html","fd46d9d15c26c374d9b999894bd4af92"],["/posts/336911618.html","4e771e388acfbb05d5afe61245c240ae"],["/posts/3402121571.html","7555092d960489be3e2f9e16c6ba4d5f"],["/posts/3405577485.html","bf9f08d1f460979bef8de6b2947ccc74"],["/posts/3498516849.html","e363172ebee08a6dac3ad3690c294164"],["/posts/350679531.html","2383e602280e458032035771bd1016a0"],["/posts/3513711414.html","b4301a1789ec77fa2a9b1d6900b9a602"],["/posts/3523095624.html","15fe2a03ed728fed680b256349aff353"],["/posts/3546711884.html","a19ac8a4c6746d53aef4b8b4ef53b3e8"],["/posts/362397694.html","9cd886ddab3ec2be618c1d9b88967faf"],["/posts/3731385230.html","9d9cb877ce0f786c093cfe36c16d5e1a"],["/posts/3772089482.html","845d7ba6b0cc61d78038e6386f2a75c1"],["/posts/386609427.html","6028a39d137e6e7f5afceed8e79fcbbd"],["/posts/4044235327.html","966bf6e12b95a5a48d11ab7be2f549ab"],["/posts/4098221856.html","495755931f72a50f73419e970f9abca6"],["/posts/4115971639.html","1a7a789609bc45ffbdbde9e72dd889b5"],["/posts/4130790367.html","30711660f5affae096ef9e6ae7695806"],["/posts/4131986683.html","8d6664b99b3dcec9ed61421c5c4770d8"],["/posts/4177218757.html","f1bfd5bfa8263dfe25d07cc0fb9d9d20"],["/posts/4192183953.html","62be1e7afa38f7df58f8c10371c9e811"],["/posts/4223662913.html","90bbaf24d36f9d9498d4cbecc10b9871"],["/posts/4261103898.html","aa2fe8ca3900ee8102a738c2245be6bf"],["/posts/4286605504.html","0465dcab63a682a58665757072627357"],["/posts/449089913.html","c25827434ef58bd7de861518a507aa6b"],["/posts/469711973.html","d48eeaad9b2cb52b78f645b749b62643"],["/posts/482495853.html","5e48b5e37ec64f2df4d8c70c6633401b"],["/posts/488247922.html","96b8adfebc97f62a9e96e6a15c63614f"],["/posts/517302816.html","af229b21a3948c0c886ab49959dbe4d8"],["/posts/570165348.html","b469649d0f5921ac036f5e9dba6f8cd2"],["/posts/595890772.html","2bd175d782f125416a3f8518fd2e1e2b"],["/posts/67485572.html","45023280a7437877e36932dca0e1992d"],["/posts/694347442.html","34c966f30077c7f0d5c7a8038a89fab8"],["/posts/707384687.html","1ddced5eb6038021f0b2417d02654901"],["/posts/71180092.html","ab1ce6b6d18565d88b02462b280b745d"],["/posts/716459272.html","ea15b097478f584ed7f1a15b770fcad9"],["/posts/765481613.html","4b8df79fa9dfe54c29f9e2c6b4431836"],["/posts/778231993.html","8c73a5df3dc7b8034138e7b6e61c2baa"],["/posts/795397410.html","049ba9f9a54420eb3177a880b56150c4"],["/posts/820223701.html","e63279d616d889129a0a292b1981cdb2"],["/posts/830372185.html","539265af9f834b295ee7fd6c7a9e24f6"],["/posts/88294277.html","00b73ee349f2f0cd9528a5f49f5e2c92"],["/posts/939963535.html","827799a400b45966f2c75f83185cd603"],["/posts/983786067.html","61eb1824007982c2cec32f19c30edd71"],["/sw-register.js","e4de69eca0746514f38bceabaa947986"],["/tags/C/index.html","b758a1eebd5688203060346b67cbc8a9"],["/tags/C/page/2/index.html","04ea630962b4c979ddef09d08e5d2430"],["/tags/C/page/3/index.html","07263a4de19fed1271f042d1628bd960"],["/tags/C/page/4/index.html","c90b8155109cbe918ce9f2022d6e7928"],["/tags/ETL/index.html","028c1a4fa9a48d78cdf83e91baec204f"],["/tags/ElasticSearch/index.html","14954c022cbfe74e04d4630e06b02b66"],["/tags/GUI/index.html","e4c60ce2c6fa6809af0130664421d002"],["/tags/HBase/index.html","83f301f4c7748f7fe12da725334745e7"],["/tags/Hadoop/index.html","3af51465a3ff9a503bc8e177b28b8a51"],["/tags/Hadoop/page/2/index.html","28d3db9a2528993c03900e04b20dd38d"],["/tags/Java/index.html","e8455342924ddf45e1b4b4a626628275"],["/tags/Java/page/2/index.html","e56c8c128c6c4069e91eac31d9511e6f"],["/tags/Java后端/index.html","8340761027fbb1abec21ae8982566505"],["/tags/Java后端/page/2/index.html","f3adb7714578e147f4ad8948aae45c88"],["/tags/Kettle/index.html","298f4319530c87ff42d83cc2291fb3b5"],["/tags/Kibana/index.html","24a3151980865c361a0273c3fdba302a"],["/tags/Linux/index.html","2ed2296251e3fbd053f0c2cb96ff28bf"],["/tags/Linux/page/2/index.html","854050cb34bdde0e6384ddc1010427a4"],["/tags/Linux/page/3/index.html","665d6674356edb8e129c928b6fb0133d"],["/tags/Mac/index.html","a1b353d5af05419a84eb7213a9fac36b"],["/tags/Mac/page/2/index.html","79fec7b63fe4629ae79bfafd9dee02fe"],["/tags/Maven/index.html","b084c1eec7a7d36f0bf2fbad1d9008f5"],["/tags/MySQL/index.html","fca76a0d2a79bc067a9ddc7ff6ddf729"],["/tags/Python/index.html","1872d2b9472c7fae8f143fac6a2ff774"],["/tags/Redis/index.html","952b22d0de9ae89fd2edbebc3b7f2710"],["/tags/R语言/index.html","c1582e546ca486d2f9af1801482b6efa"],["/tags/Spark/index.html","96dd0e1765c8b708c983133466550b38"],["/tags/Ubuntu/index.html","88efb5283a5bf5b8aad3f317d9ca5e46"],["/tags/Vue/index.html","9d78326b4a644baecc3205511ca0a472"],["/tags/Windows/index.html","be65113fc2a1f7275d60f71c360f53b9"],["/tags/ZooKeeper/index.html","e39c99841cafa047b3999ec1b7afcba4"],["/tags/bfs/index.html","f2b32931df2b1bf582f7198f809cf467"],["/tags/dfs/index.html","28350199fadaec13bf1b47b3f283bd86"],["/tags/folium/index.html","fc670a44c4def8372778074e01645c72"],["/tags/git/index.html","3a14425af71fa0c8de85173a84976f51"],["/tags/iPad找电子书/index.html","232c4006108447ad306f8f42b7c0a948"],["/tags/index.html","8ac8e0f4ba16e85c9b12a9678673f293"],["/tags/latex/index.html","a355df79ba79dcb0bfb6e47463a767c0"],["/tags/中间件/index.html","b116d4ffa6e9e678b48d2154f7bdfbb8"],["/tags/二分查找/index.html","71700419fb81e6bdf48929ee5b7478d5"],["/tags/优化类/index.html","98b45ccf090f8e7cc8dfc73d393ac144"],["/tags/前端/index.html","cc601b1aa1de8d33f8417754fd560d09"],["/tags/前缀和与差分/index.html","8508dc04e215123b5ca8b87b5bea0b93"],["/tags/动态规划/index.html","c9447480766668e47646ac0acfdfb296"],["/tags/动态规划/page/2/index.html","804c81f02178c77255f9a3fa5f0d1589"],["/tags/博客搭建/index.html","147bf29d94450d78cc83222e957a273a"],["/tags/图论/index.html","8bea9c645b55d90125b080aed252afdd"],["/tags/图论/page/2/index.html","fe91a1c54af27415f9b9593107ceb33a"],["/tags/大数据/index.html","2e7643fb5a7cbea1e77af366cb773420"],["/tags/大数据/page/2/index.html","f791a85add1c375aa48db7d669e6ce4b"],["/tags/宽度优先搜索算法/index.html","4418399f9aed2b9fa502b7a2ff461965"],["/tags/排序/index.html","9a7f5bb3156a57ecdab9fbd9ff58b2cd"],["/tags/操作系统/index.html","33463be2c2660d3a58874677b40f78fc"],["/tags/数学建模/index.html","19f46cade63513e3fdd60daf4e19883e"],["/tags/数据库/index.html","e58beaecc98d80f185a3db490882f833"],["/tags/数据结构和算法/index.html","ced6ada9672c718215b5e6daeebadc5c"],["/tags/数据结构和算法/page/2/index.html","9f0841cd7f2c3c21eb4996e29505e300"],["/tags/数据结构和算法/page/3/index.html","0c22af0237ba288377037682fbf7dcff"],["/tags/数据结构和算法/page/4/index.html","43c075c6f05f03275f937817fb9b8649"],["/tags/数据结构和算法/page/5/index.html","ca4a191dce31943bcfe2b567a05be6a2"],["/tags/数组和字符串/index.html","c5a2ff62f559bb7d2ffe465b8832a6f9"],["/tags/数论/index.html","23bdd988eb52bcdda007aaf663c10c67"],["/tags/枚举类/index.html","2e0d67668bf5c858b29b323cb63f8fb5"],["/tags/栈和队列/index.html","d35d0e8f28ded97d0039073e20ef0e47"],["/tags/树论/index.html","08b3facd8751c524eee154d5cdc71593"],["/tags/测试/index.html","ffa20e43b21643a8b306d703d6cd5551"],["/tags/深度优先搜索算法/index.html","7eb9d9f9d546e32eaf2caf91b065807c"],["/tags/环境/index.html","66ef39795da9baadd9cda200e452f054"],["/tags/环境变量/index.html","17fce750895779796f841e3027da7acc"],["/tags/绘图/index.html","5eb6c7ca4bc5321112070f2c7723ebcc"],["/tags/编程工具/index.html","8ae8cde68f6675c69c9b3209fe89cf32"],["/tags/编程环境/index.html","9d1cfa1f724d4c004268ee540688e1d0"],["/tags/网络编程/index.html","c14df09625d9613a97c98121afc21c67"],["/tags/英语语法/index.html","937b75e5eb0a6d4c61958051f63082d9"],["/tags/计算机操作系统/index.html","c2b976cd88772ad07fac30c8dae403a3"],["/tags/论文/index.html","a97b573a00733462a0c1c050e950f8b3"],["/tags/资源下载/index.html","e362fbfa61a206839dbacea13ad2611e"],["/tags/链表/index.html","4733eb43a274297bad3e2233808056bf"],["/tags/集合/index.html","a6a3c43f61041923b6981d429f1a1588"],["/tags/集群/index.html","b0e0dda26a6f2d5547c4c18579aff341"]];
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
