/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","04d059705f5a79970b7c92a98f9bac87"],["/about/index.html","af39817ed62c031358bd3437b1a269ac"],["/archives/2023/01/index.html","19bc67237e6fcf8ab9f0e133ca3e40ab"],["/archives/2023/02/index.html","60aef8430835b44f91ce4bc30f78609f"],["/archives/2023/02/page/2/index.html","dabade89c493a48ffb106b94db8a27eb"],["/archives/2023/02/page/3/index.html","4ce9b38eda726c70d4d4595b1f35fe51"],["/archives/2023/03/index.html","93970614760dfed2c768d61b6feed3d1"],["/archives/2023/05/index.html","26157e77381512f8e7f3e383869eb58d"],["/archives/2023/06/index.html","a2201273bace4c0a06a58b7823406e89"],["/archives/2023/09/index.html","afc0468acbc87259c8569591c269ca03"],["/archives/2023/11/index.html","db30a2da7f4afd2fd48d2b8e0da61977"],["/archives/2023/12/index.html","50efce61c9d6af6bb1f6154fcb54690e"],["/archives/2023/index.html","5e9b62b39f3f8e89499fd9769c6a7a56"],["/archives/2023/page/2/index.html","40e9a49a1f0f5e083b5a33687bae299a"],["/archives/2023/page/3/index.html","07137a9b7c96adc56fd45912cfdf6038"],["/archives/2023/page/4/index.html","9d0df6ac0df8f1bceb69277f78fc2884"],["/archives/2023/page/5/index.html","fd1e55a547f4ccad2c4d603c0f1c3cfe"],["/archives/2024/02/index.html","6b891f9b0ac6f233674e714e4823ee54"],["/archives/2024/index.html","a336f1d68d53abcf62bc60090e98805c"],["/archives/index.html","94d39a0f7e739f0893e7fc40703a54de"],["/archives/page/2/index.html","e6bde07cb112ce5ecb7acf75f013d1e2"],["/archives/page/3/index.html","2d9bb3a353a0198aa1a22040365bf063"],["/archives/page/4/index.html","99c2e2b47d85344623d56cf3e61a3163"],["/archives/page/5/index.html","dfcec5f6057b229dfe1c22409ffe061d"],["/baidu_verify_codeva-qQP2iZOMLX.html","0341022a04e42fb8ad00746433ee0e5a"],["/categories/Java/index.html","36fc09476c7e7ac4fbdffa0b265098c7"],["/categories/Java/后端/index.html","cbc11af1d8a160dae59ccd54066a8369"],["/categories/Java/基础/index.html","9202689ab73313a78f931c7d94dacb13"],["/categories/Java/基础/集合/index.html","422dee241640faaca9a5a626f9e7a41a"],["/categories/Python/index.html","513c6c59725ffb2991d1bfe607a3322a"],["/categories/Python/编程环境/index.html","50fc860659e3940b18479dc823e2a0a4"],["/categories/R语言/index.html","a8ffff4f687a00fc0e5eaa83a6f7f7cc"],["/categories/R语言/编程环境/index.html","9a2fa249f407c6b508e4d6b63f04bd0c"],["/categories/iPad/index.html","28ba6afd81baae0966da5f06e33a32b5"],["/categories/index.html","3715ca1efae1614f4c6ad4988f4df041"],["/categories/中间件/index.html","d4ea08aeb0a60568d9d90b1f4a11905b"],["/categories/前端/Vue/index.html","48eb19e6baed3fde1ed90c6704270c9b"],["/categories/前端/index.html","c353949ef0ca32ddde4f4b01680387db"],["/categories/大数据开发/ElasticSearch/index.html","a59331045c612cff9e6a6df51beb0edf"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","22819a5149c0f83747c55da7c7b87387"],["/categories/大数据开发/HBase/index.html","6b7dcff7619119c9b3014fafc03217e3"],["/categories/大数据开发/HBase/学习笔记/index.html","f2112c0d02d32cbafa8f0fd87c698990"],["/categories/大数据开发/HBase/环境搭建/index.html","d433246378a2d85abda6ed692f336c01"],["/categories/大数据开发/Hadoop/index.html","9a25915c94be4b052033c82de2102747"],["/categories/大数据开发/Hadoop/技术/index.html","bd01e724621512928a8cc077f59eda49"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b50d369c89d165d543b1d3eae05b7932"],["/categories/大数据开发/Redis/index.html","601a9f4c15b7ec2ab5e4d4b1395f3d92"],["/categories/大数据开发/Redis/技术/index.html","77cc427cd48075920853a298cf600bc2"],["/categories/大数据开发/Redis/环境搭建/index.html","4f7836eed50ecb46cb658b7befb4f037"],["/categories/大数据开发/Spark/index.html","a484b2820ccde37dc9e1dd0645fdab1f"],["/categories/大数据开发/Spark/环境搭建/index.html","7e7cf701b1fce447c3b13fcb6ec90793"],["/categories/大数据开发/Zookeeper/index.html","95fbd64c5b7d40d7c243113fba2937f7"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","15ebdb3a584f3150daa4bb281aceb4bc"],["/categories/大数据开发/index.html","6917b155dd3c5cd96a0f3a2544ca5661"],["/categories/学校课程/index.html","447cee7512a31aeb52cbd760ae7ec5d3"],["/categories/学校课程/计算机操作系统/index.html","1b724fccb4a9f7902b672339d97eefc3"],["/categories/操作系统/Linux/index.html","bcf6ea2fb21c560bf3f0a1931d44dc61"],["/categories/操作系统/Mac/index.html","6986120e76fecdf4ebbf89eeeea98c89"],["/categories/操作系统/Windows/index.html","272577a953ca65273aad7f35c7469dc5"],["/categories/操作系统/index.html","a24b2461503aa43b0d524e5879326fab"],["/categories/数学建模/index.html","d1c3a84fa188f4fecfd100282cc68d64"],["/categories/数学建模/latex/index.html","6a98d8bb1688a1e1edfe7f1a2c565b84"],["/categories/数学建模/优化类/index.html","548007d0fedf74e403227ad6903e938d"],["/categories/数学建模/优化类/现代优化算法/index.html","a235038fd28d029075b75326f90e93d2"],["/categories/数学建模/优化类/规划类/index.html","4e63b1622f3dd3ae7590084e32081903"],["/categories/数学建模/绘图/index.html","9568b52d2ec655df162feed6b1b05211"],["/categories/数据库/MySQL/index.html","36d79a337129312beec3492b220d44a1"],["/categories/数据库/index.html","2da77b2b9ce30526c94b0d214f798950"],["/categories/数据结构和算法/index.html","60581c44cb5e1e988e8df65d75d10a88"],["/categories/数据结构和算法/page/2/index.html","be18850380cc41ea7a2060e9b5d9fc51"],["/categories/数据结构和算法/基本原理/bfs/index.html","17a6288ecfaec76a405883409311cf2d"],["/categories/数据结构和算法/基本原理/dfs/index.html","51e42ce5c1a9f2ea57a7b9c10aee724f"],["/categories/数据结构和算法/基本原理/index.html","9160673bdfc53d76a39dcfd2a571734c"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","3a770e040934b9072cfbb94811a0db8f"],["/categories/数据结构和算法/基本原理/动态规划/index.html","2948f41c4af1395e78d48ed709ae36a9"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","04850e831c1d88fb94240c2d2cd7b27f"],["/categories/数据结构和算法/基本原理/图论/index.html","c74d8682f7b0b643f5f82d447b1b8bea"],["/categories/数据结构和算法/基本原理/字符串/index.html","2db6f5cf1f4a7e37756a9dbd9dd2864c"],["/categories/数据结构和算法/基本原理/排序/index.html","110401304b1e68713e8795414a91d7c6"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d2ae43eb07147bbd47b3ba98e8366997"],["/categories/数据结构和算法/基本原理/数论/index.html","f03a1ed6e6409f77602d2e8ce0b9112b"],["/categories/数据结构和算法/基本原理/树论/index.html","b9c0947685ecc2a788e1e9f417b32c48"],["/categories/数据结构和算法/基本原理/链表/index.html","395a81a6d3476b1cd79afb36ad8ee959"],["/categories/数据结构和算法/算法题/index.html","7938f55b633a60607dc472328c2a97bb"],["/categories/数据结构和算法/算法题/二分查找/index.html","1f119731d691dfa437192618e0e5d166"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f0e69a3befb8667fcd93f2fc513a7129"],["/categories/数据结构和算法/算法题/动态规划/index.html","27d4a302bf4eee3814f90c2116fd0b25"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","daf0dace757ce4998dd7533c431ab400"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5826dfe2581e0f96f0ccd2e558729542"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","770bcef9dad267372e65a8666bd1c454"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c6f894fe63f79ec2d907d2058267d007"],["/categories/数据结构和算法/算法题/数论/index.html","572021e62e4b9bd0422b6b0a628f3f72"],["/categories/数据结构和算法/算法题/栈和队列/index.html","71b2d62902cca5e1997fdafb717d7ad2"],["/categories/数据结构和算法/算法题/树论/index.html","26ef0a4f75cd21b5b95e9d78dec39c03"],["/categories/杂七杂八/index.html","d7d593b38f608b15d1a470ff602d8e4f"],["/categories/杂七杂八/博客搭建/index.html","e95404fb989f43e0829dad3a0546b39c"],["/categories/编程工具下载/index.html","58ac27c2d4758793269bb45abd89de53"],["/categories/编程环境/index.html","f490863b335d7d27a4f3ebb1d437cf7c"],["/categories/编程环境/大数据/index.html","1d2dc3cc9ca4b26d9353c13e9d2130de"],["/categories/英语学习/index.html","b36ce94a900f158daabec4d1196f8207"],["/categories/英语学习/英语语法/index.html","d6d1278aeb9ee7d87dc01e3d4b9b8a56"],["/comments/index.html","4bfb1a8b37203eba0ce817bee479ccce"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","8de07d9085ede6ae191315e6ad6b9dd9"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","d7b95d1f706495976654df3c351e6d2c"],["/movies/index.html","b686db3a1cac9a55138bb1baf73cff40"],["/music/index.html","7488cd379b43119554c670378b4deb7d"],["/page/2/index.html","dd9ed4fd37fa9eb84402fedf42f35041"],["/page/3/index.html","f3a92cd7f296cb27800943adb382daf4"],["/page/4/index.html","6f0f1047b0d185ea5879768580297a66"],["/page/5/index.html","bb18b18f32fa20d0ee5e7fd37cb4d903"],["/page/6/index.html","a6d87455c2dd16b9d858de1472421277"],["/page/7/index.html","09dbb35f8f9ad38759e0155c940b988e"],["/posts/1021360842.html","e1487a9a3a932bc1e055689d3b1a1f7d"],["/posts/1120620192.html","09792b23cf2fd809455ca8aaf0564504"],["/posts/1137707673.html","0ba8001b19c9f61f64543ffd2cdeacd1"],["/posts/1141628095.html","f2cb472d46a414f45d545980028c6223"],["/posts/1168613674.html","72c212cb8cba5cb7a56d86b7c77f4313"],["/posts/1219920510.html","add13bec5dbdcf3b4abccc87cb9d5598"],["/posts/1222166338.html","9f132e6c5fda914acd7b43533481305e"],["/posts/1259097482.html","5f5228e868c0319e33f1452bc41c5c8e"],["/posts/1271036369.html","d11e3c54fdaa3aed3c4bd5e73d16bbb8"],["/posts/1312847445.html","0e1e21cc84927117f1eec86e7fe6da0b"],["/posts/135355774.html","461f2b122a69de7768522f47ac8aee3c"],["/posts/1375344716.html","146fbe87b3786c37319769a56ab32037"],["/posts/1388991698.html","ce17e745de980fbe0e92a2002d5c562c"],["/posts/1410315814.html","d57bf07afb104830de35c7b954a000ca"],["/posts/1452790229.html","adaaf953ad846ef00a6ef3ca19c15316"],["/posts/1470079884.html","e4d06f48eb069c348fecf75ad4a89bd8"],["/posts/1470079885.html","189fcfab2606e4c1495b9af15e991860"],["/posts/1470079886.html","94bfe92ac2e9a21dcd88fe4b146efe92"],["/posts/1470079887.html","02a778c261f1b549d352b335b8f13109"],["/posts/1498536549.html","7588d954fd6d4153a9fe93bc35692c6b"],["/posts/1539568593.html","356144f635c1559d0c700123d02a7a81"],["/posts/1547067935.html","2028800040f677b570f42efd9e17b4c3"],["/posts/1557866301.html","be991938fad62f4dce9fe490ab00f17f"],["/posts/1571776361.html","8207aa3ec584a191f76c4a95c84f5d9c"],["/posts/1605124548.html","e82f0136a41a6bb9d9ff865a19cd1e05"],["/posts/1633036852.html","dfe67f26d112348704fd9b3a42e50e12"],["/posts/1667740714.html","52c46a4e5b78b7b9ff979edfeb1df442"],["/posts/1674202625.html","a31dd031a347967c128c9a1793ba6a9a"],["/posts/1765123828.html","c2b09904d034c5e3339bc2034335b3cf"],["/posts/1767336200.html","668b70d01a5114045b3eebeb5de35c36"],["/posts/1776114197.html","33a9a73233ed9fbd559a28133336d6ac"],["/posts/1817748743.html","943773c01c5168f254bedf1a1beee98a"],["/posts/1925125395.html","9d49c218fd87c34fe8d15958eff433be"],["/posts/1966191251.html","8744d344bf5f7e7e69fc6f4907fe0562"],["/posts/1987617322.html","1c3c9ed93d6962fc8b836428bf456f12"],["/posts/1999788039.html","ed7c6352ed15ee21689ab913f2c46e5f"],["/posts/2007534187.html","935fbb41fd0e84f834ff634d84177ac1"],["/posts/2075104059.html","ffdba1bd5bebffd078322a2a085a4c6f"],["/posts/2087796737.html","2ced55e6769ab17ccbefc353cbea2f91"],["/posts/2106547339.html","cbe2cdfd503990e08a04c60f4738b1cb"],["/posts/2207806286.html","8dec49f1a5ae3307a6cec4d4104ce1e3"],["/posts/2225903441.html","aa09c635a515fff547cc41ceef2f0197"],["/posts/2265610284.html","f913bea4cab0c2caf413b6b8eed32e26"],["/posts/2281352001.html","30b058f4840af9b6b97fab24a3e245c0"],["/posts/2364755265.html","84bc2f0e3b111dedc69cc055cdbe746f"],["/posts/2414116852.html","49aba93205a3c98154036ca07eada907"],["/posts/2421785022.html","597072a5cfb453140de3dea539cb0c23"],["/posts/2482902029.html","c8832d9d051ae4c79692ec06547e6058"],["/posts/2495386210.html","5aadff71cf4ea56028fdd29f4b62d59a"],["/posts/2516528882.html","40d039881607bffdf148cec4b56d2f20"],["/posts/2522177458.html","cafd2765afb34a6391a7f9b6b2de4c7e"],["/posts/2526659543.html","50c0526a424076d5905e443a9680023a"],["/posts/2529807823.html","230334e3ecbd457875882ed497c9cc98"],["/posts/2596601004.html","2bb2972ebb3033db48c2958861334d18"],["/posts/2697614349.html","cf2430d3a491685111bd4bafcbe876ca"],["/posts/2742438348.html","0c9abe6fc545ca46a75977eeeef6e3a7"],["/posts/2768249503.html","b1f4c95367c510ee85a0a705b621a236"],["/posts/2864584994.html","ba7d03cb764c284c631f147a744ce7b5"],["/posts/2888309600.html","6bc6fb785766174291036450bdd22233"],["/posts/2891591958.html","9dacee7049f635de3be0843c17e89c34"],["/posts/2909934084.html","1c622bfa7b8094c77d4127730034e4fb"],["/posts/2920256992.html","66509f3688c2d04d99d6d22b88ba44ad"],["/posts/2959474469.html","c09cbaf20d0d13a5278b2e28be3ba12d"],["/posts/3005926051.html","e3c359ba9c562a04c861b492cc9085dc"],["/posts/309775400.html","4e90e85a0f4c8ce46b728b010409599c"],["/posts/3156194925.html","c60f0adb1701f1356be99edce5daa78c"],["/posts/3169224211.html","51022d198c70820079d9d3face226f24"],["/posts/3213899550.html","42637094d37a289631679bc9f51534da"],["/posts/3259212833.html","b49d7b7a40788ba534ae68b7ab8ff8a4"],["/posts/3265658309.html","873d565b13a43ef5d0f98be5a1f99a9a"],["/posts/3266130344.html","c3a51cadeb51fea934c90192523ea644"],["/posts/3292663995.html","eaca88dbe4763a41aa5a207d118898be"],["/posts/3297135020.html","d4fce8af98726197b713b4f2fdbca9dc"],["/posts/3306641566.html","5a65551b7b661f778454d2fe3e13e8ed"],["/posts/3312011324.html","8f82df6c7cb8401f5f6686fc62173178"],["/posts/336911618.html","56064cc3c68d43eedf4a30a93e0b4266"],["/posts/3402121571.html","24f8fbb597295c0c7bdb6ed22d3ba5b6"],["/posts/3405577485.html","8c1caa9c15545416a101f87c9562ddb0"],["/posts/3498516849.html","8c3f5fce102c5a0f3259337405090508"],["/posts/350679531.html","9199cee544da1b85431340a04b237cc4"],["/posts/3513711414.html","896c9edb59c18fbe417baf19613f8422"],["/posts/3523095624.html","8fc873fee75e2840d23f75e01041f8b9"],["/posts/3546711884.html","104dd189dd98a8e5146539eec315d570"],["/posts/362397694.html","3e63a67b965134a8ebb9c001df8b2a94"],["/posts/3731385230.html","4a7c9fa39ed57ae75df2b6867900a5af"],["/posts/3772089482.html","4a5e3638a1a628e2aba614b6660d8626"],["/posts/386609427.html","ded2fb31c9a3f1f29499f90c204c0df4"],["/posts/4044235327.html","b7a4e26aec072d461b5d8eb755d4bce6"],["/posts/4115971639.html","09415fbfa01d5e5618ea386078b9b229"],["/posts/4130790367.html","8c694e2e7626afe98c791c583d9cb53a"],["/posts/4131986683.html","ed79a07c00087456b4ad48b89ba1cd76"],["/posts/4177218757.html","31db0ce6168594f11d9e52050cadbbec"],["/posts/4192183953.html","866f6863d65a09cb60e1133788a53bff"],["/posts/4223662913.html","5a2cf1c51c6b186802157864caabc80b"],["/posts/4261103898.html","1702b5cf2994dfc132d686f5b4fb1387"],["/posts/4286605504.html","3e35d9b85b008cc589afa87a4d789b95"],["/posts/449089913.html","ea32e0bcece6738c0b316eaa6c899608"],["/posts/469711973.html","06f6fc85763ba2b01da89e329790b74f"],["/posts/482495853.html","2bc6ba41a1d7c0ff0d5c404ec903cab7"],["/posts/488247922.html","48ef426c3a51d031f3b50a18cd385c98"],["/posts/517302816.html","9f1da9392d1cb5fb3bc3654a693c6dff"],["/posts/570165348.html","eaf001b787f838bdc58619c3b8e0d3ab"],["/posts/595890772.html","fdf9ec4700686c5d1970fb29884e2a75"],["/posts/67485572.html","3e8d2d60e68e356b76dbd967d06b0969"],["/posts/694347442.html","04d2f3fdb80fbf2b75e38fb3d5e17653"],["/posts/707384687.html","6a34ce9e3e34474af4adb58d9ecb75f7"],["/posts/71180092.html","389082fb3675fe2cdc3298bbac755f57"],["/posts/716459272.html","60ce127d52301e56b1e9dc704be7b37b"],["/posts/765481613.html","c37c3e8de45edc9d3b8c3c455a0bc8a9"],["/posts/778231993.html","8c41d492273d5cd9026f8e28fefffbb9"],["/posts/795397410.html","6dc4d8e4eda3960d0fc42c024716aa97"],["/posts/820223701.html","e422fcf35045dba1df6e341d8bebbf21"],["/posts/830372185.html","2322d9de0320eece68940d6fd99c57b8"],["/posts/88294277.html","8deb48510d437d85bedc5465570e4ee3"],["/posts/939963535.html","3f6ab692d892bf40282055e847072098"],["/posts/983786067.html","d056b8bc23f1749892607df0846ae725"],["/sw-register.js","42bca6ac63af4fdf67f93b888f69ef93"],["/tags/C/index.html","a76d2a44f35273d9688ea83b2fbb91aa"],["/tags/C/page/2/index.html","3ec1db871d5c5e7875e396c1e25ae8f2"],["/tags/C/page/3/index.html","df223cc39c1154100e5f1e004a0ffb24"],["/tags/C/page/4/index.html","7dcd8f0aded00e7afc155469bc4d09b9"],["/tags/ETL/index.html","9bd8b980514eba71cd78222f7f12dab9"],["/tags/ElasticSearch/index.html","cffc3c808fbe9ff18a89037801eadfc7"],["/tags/GUI/index.html","06a0cfc50de0d8c006a6e6e1ad9e3e7c"],["/tags/HBase/index.html","7ee9e8ab2c2b3d119c77bdb33f407f13"],["/tags/Hadoop/index.html","5450ffc3cef53144180a28e85e1d40f3"],["/tags/Hadoop/page/2/index.html","c864068827c219bb6d4a029b9ce2e02c"],["/tags/Java/index.html","2a33fd51eda7a66c76dd53bfce7a46bc"],["/tags/Java后端/index.html","72dc3d830cd2b2e0914073f9ed41513d"],["/tags/Java后端/page/2/index.html","139f3b49a8d8ecfde9a650d8b5637c57"],["/tags/Java基础/index.html","303f4d7113039ab74169d05ccde0057b"],["/tags/Java基础/page/2/index.html","9fed3e062a3d5d0c41f831d667221b72"],["/tags/Kettle/index.html","239ca18c59a12e1efb2e80a394772eba"],["/tags/Kibana/index.html","9744a1e2623489e01255f1efa4a9767a"],["/tags/Linux/index.html","2b8ba11906e6d7cf47f3d0b43cb23d14"],["/tags/Linux/page/2/index.html","3c1a6a572e618ebf4a822b6015c13b4b"],["/tags/Linux/page/3/index.html","0ab8bec50f2d9aacc62caecd2e458b5e"],["/tags/Mac/index.html","0eeac1dcc64d820a0964bc9093b03067"],["/tags/Mac/page/2/index.html","67cd114eb17a307a7277eb73a8edc40b"],["/tags/Maven/index.html","dd3ba900f10d18b89fe46408fb8ba207"],["/tags/MySQL/index.html","bf72c9459a15d7fb41775317402ce871"],["/tags/Python/index.html","2d7334c2bf30d1f35e0b4079aab1f8db"],["/tags/Redis/index.html","0430871aeeaa30ad2a41dcebb2904f14"],["/tags/R语言/index.html","f2ba0d3a14306f430246921f7d6b7b3c"],["/tags/Spark/index.html","041cbaa37e4185263fde172b6e722abf"],["/tags/Ubuntu/index.html","9e54a14d73d5addc8cbbfcdb7f93c541"],["/tags/Vue/index.html","7f3f71f6f67d1bab0860bd699622509f"],["/tags/Windows/index.html","58bbccf3b6c91fc4a50aeb22dea589fa"],["/tags/ZooKeeper/index.html","038fa5f679c889646e06237b6092dbfe"],["/tags/bfs/index.html","d66ccdb928cbf9e4248c88262c3bbfd9"],["/tags/dfs/index.html","1ddd10505e78e0f11db04b3e877898ad"],["/tags/folium/index.html","f5671ef682c8fd50d60fe05593c6916c"],["/tags/git/index.html","0764e26c45829ac0d263857bbe1e8c49"],["/tags/iPad找电子书/index.html","c90b92ee448d328be18a848b727d1ffd"],["/tags/index.html","9d7f05f93f9a858fc8ea494f72577db8"],["/tags/latex/index.html","e15212490518f61a292e6852e3c91036"],["/tags/中间件/index.html","418529f0321df568abfbf9914f56c3fc"],["/tags/二分查找/index.html","40c0b4337a8da09ba5620d9acd7ed50b"],["/tags/优化类/index.html","05e68196fc945a60e958e4d1141cd027"],["/tags/前端/index.html","48eca34290c0ceaa727d033f759696b5"],["/tags/前缀和与差分/index.html","7a235751dd9c2e54d4ca6e7e318d2a41"],["/tags/动态规划/index.html","d307689c14c915dabb30212ec34a85d1"],["/tags/动态规划/page/2/index.html","a90b3c7d19bba52c575bcfb4a3c4cb66"],["/tags/博客搭建/index.html","949d19515d7a1ba447d15f3de17facf5"],["/tags/图论/index.html","96451e2db157fe16fd6656c91aa5a27c"],["/tags/大数据/index.html","96d41261b5f10f52cdb0ef2dbed3f755"],["/tags/大数据/page/2/index.html","d94de5cbcdc66f06d44c3089f2722aa0"],["/tags/排序/index.html","6060f1e6540e66a510234a7901899f48"],["/tags/操作系统/index.html","11f88de98325c96973ead1c97855dfd7"],["/tags/数学建模/index.html","2c59b5c7597b5b7b3bf11f7802886470"],["/tags/数据库/index.html","5abe1dd997a255f1ab56bff316e20b77"],["/tags/数据结构和算法/index.html","1df9c2c1fce0ee2c4f1ae9df135f0d96"],["/tags/数据结构和算法/page/2/index.html","8f504fee6260da33fc00fdfc3c368d19"],["/tags/数据结构和算法/page/3/index.html","d828fa57e4441c1e12fda2c7b1ef91fd"],["/tags/数据结构和算法/page/4/index.html","97db999784b061d4aa0dfe908d1ab9a5"],["/tags/数据结构和算法/page/5/index.html","37a4ade4179c205f8830175c5436698a"],["/tags/数组和字符串/index.html","3502e03182c44799013349844762a178"],["/tags/数论/index.html","9d077a17517c90f67f81a9f0b55cf793"],["/tags/枚举类/index.html","98a413b35ae4749dc92169e9c210ee07"],["/tags/栈和队列/index.html","a74ea290a5dc10257a73c645ae932184"],["/tags/树论/index.html","36f83b01fe81134ab8ae43860a9788a7"],["/tags/测试/index.html","487afb9b4f9a9fe93b66a4525eb598bd"],["/tags/环境/index.html","d4f0bd8eaa0b8a31bf7cd6932fba2d79"],["/tags/环境变量/index.html","27c8ec29aeb79a55ac1af7e1c03451e2"],["/tags/绘图/index.html","84eb60bacb0a82c4eb2a9251ce6c4bcb"],["/tags/编程工具/index.html","0e31fe332e946ee6c919d2807a83355f"],["/tags/编程环境/index.html","f3434384446a4163764aae2617cdf903"],["/tags/网络编程/index.html","bc24117670200aecc3f7709c41247569"],["/tags/英语语法/index.html","fd69e2943c13f6479e63008a24d95b62"],["/tags/计算机操作系统/index.html","1cdbb403e8cdd7409995f41bda489f46"],["/tags/论文/index.html","9746a69d14e5b133381fef5af691b44f"],["/tags/资源下载/index.html","e29d63cbc835792272950a1decc281c7"],["/tags/链表/index.html","2a917689b23cc7686ce0fd6ea438e6a9"],["/tags/集合/index.html","f0ab3cbd77102be2b6e452e534cbd16b"],["/tags/集群/index.html","7612dbd542760dc6ffe76e13a609cca0"]];
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
