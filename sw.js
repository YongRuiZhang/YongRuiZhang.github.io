/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","5b9f0e9e2cbc0ee2d242bcb5930f8638"],["/about/index.html","b382ae5a88b912b5589bf1a6526cef54"],["/archives/2023/01/index.html","3b23adb2d6f8d4b6467603fcdff00401"],["/archives/2023/02/index.html","8430c7a7c39575235ce4f675025ebb0b"],["/archives/2023/02/page/2/index.html","e04dbf488c5c98f1290408284b6bc17b"],["/archives/2023/03/index.html","e16778e87124721f2df0e11067f8b4ad"],["/archives/2023/05/index.html","993ef159c418e4c693a803e28359cd83"],["/archives/2023/index.html","f35c7434d6bb05ab79faf86ce47eafbf"],["/archives/2023/page/2/index.html","3ea23c9592af3d82492bc9c5e05a7d5c"],["/archives/2023/page/3/index.html","f0acfd27c91704dd75a9448730f1389d"],["/archives/2023/page/4/index.html","7d6ab71e886b14243200a161d93da98d"],["/archives/index.html","62952ab65cb030447b7a87f59eba406e"],["/archives/page/2/index.html","22ec5a7e9f84f10f08c4a8d1e24a2bfc"],["/archives/page/3/index.html","0b0d5e66a3ab1cb5749261df6bdde7b1"],["/archives/page/4/index.html","ad59e39b5f9c3807225be9d1ca999e4c"],["/categories/Java/index.html","4b6f52cf98e1888c276f3f563409f69a"],["/categories/Java/后端/index.html","f353777a6638ad84aecea7ebf5148a09"],["/categories/Java/基础/index.html","d87c0919d819c1b91bda04a9c5ea31c0"],["/categories/Java/基础/集合/index.html","b5e31498c4fa30f9a1cbefed2fe5982b"],["/categories/Python/index.html","326f80d7c42a5756d59161587ed46760"],["/categories/Python/编程环境/index.html","52106738c1f14abf8fb42df6f2cc0b89"],["/categories/R语言/index.html","dd0a75755764e073ee65ba1b2a2fe608"],["/categories/R语言/编程环境/index.html","15bb7355819deef823844f07030105d8"],["/categories/index.html","7ddc5e582f9414f6e0361f3fbc6fe2bd"],["/categories/中间件/index.html","0ea210c80d29821388f364e3f076569d"],["/categories/前端/Vue/index.html","19ce312cb4325c432cbc1e5fbbf15263"],["/categories/前端/index.html","8a8430eafe147e64f3f0fce602bc89f7"],["/categories/大数据开发/ElasticSearch/index.html","df9939eb37937bf275e03948b78aa958"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","bd032271857a312dc2fdf20470d68213"],["/categories/大数据开发/HBase/index.html","20fe96b8588f2d4cb5578ad15e253e8d"],["/categories/大数据开发/HBase/学习笔记/index.html","9d7392266c7538ac4a71dce4e7dbe390"],["/categories/大数据开发/HBase/环境搭建/index.html","b84da10fb089e32b1ffe46dd1ba72163"],["/categories/大数据开发/Hadoop/index.html","f9c96ee912673272edefe6e9f08da218"],["/categories/大数据开发/Hadoop/技术/index.html","759de7cdfbe43a3265109af4dde38083"],["/categories/大数据开发/Hadoop/环境搭建/index.html","338dab9aafe8a4baf087d1f353fc99f8"],["/categories/大数据开发/Redis/index.html","a7316f120de415347e65985736bdc541"],["/categories/大数据开发/Redis/技术/index.html","7fc7a36612d1ad77ce95a4aac201d804"],["/categories/大数据开发/Redis/环境搭建/index.html","a50802700d579ddb841a90241c918ce8"],["/categories/大数据开发/Zookeeper/index.html","32972ca7498ed29e5267ab5726603b05"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","205bd99c675300af3d98db37f68aeca7"],["/categories/大数据开发/index.html","e954e705cdbff54ac3d9f6ee6e763a73"],["/categories/操作系统/Linux/index.html","f096424e88d8b3b6ac0b3aeef91ffd38"],["/categories/操作系统/Mac/index.html","e683a79f01fced9226a1d6222b310271"],["/categories/操作系统/Windows/index.html","e09cff0a791d759acf07467c0cd08835"],["/categories/操作系统/index.html","af216da3277550dac30b5e2ae90759fb"],["/categories/数学建模/index.html","c012e7ebd274c3b691ccf28acde1363d"],["/categories/数学建模/latex/index.html","bd1ba99b92fc0663eb0fe7605567cf87"],["/categories/数学建模/优化类/index.html","85c162e7fdbaf39522b2abe23355bf64"],["/categories/数学建模/优化类/现代优化算法/index.html","60fbc160f531437ba4c8887e7f9406b0"],["/categories/数学建模/优化类/规划类/index.html","ccc0ece9050d7fc39d319613b1cf3cc1"],["/categories/数学建模/绘图/index.html","32f90ea104df2e1e804f540bb381ee81"],["/categories/数据库/MySQL/index.html","589c56320337c4b62c8f11cc5700198e"],["/categories/数据库/index.html","262fc4ac29d989d4d4d3ae12217f194d"],["/categories/数据结构和算法/index.html","f9609af06beebbffd4c597db06293b58"],["/categories/数据结构和算法/page/2/index.html","76d374d0a87b78fc9925faac49bfba92"],["/categories/数据结构和算法/基本原理/bfs/index.html","0765719551365757123051bb2cf5f6c2"],["/categories/数据结构和算法/基本原理/dfs/index.html","616d7459c9a092c9ebe585275749d23c"],["/categories/数据结构和算法/基本原理/index.html","032f145b68a042bb8d16827cce12fac5"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b23e77a80878f044311d10f908e78f72"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","27c3086e41ef29ea5829f9a89637686c"],["/categories/数据结构和算法/基本原理/图论/index.html","bcee5185f26f2490048f24e2848a69a2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","79ad94beea2d97173b36052814d4bc19"],["/categories/数据结构和算法/基本原理/数论/index.html","526cc02b928e553572d70c185c9d62ca"],["/categories/数据结构和算法/基本原理/树论/index.html","28637beb2c0a607e148179fa09604983"],["/categories/数据结构和算法/基本原理/链表/index.html","c12faaaf5770febf80773a3d76880a0c"],["/categories/数据结构和算法/算法题/index.html","8ad2882c1ea4b44aac63ebfd26695f78"],["/categories/数据结构和算法/算法题/二分查找/index.html","040defca44e18d6c8006c8933164b5ef"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","e2bc17ced4044a84ce49ca2bc807895d"],["/categories/数据结构和算法/算法题/动态规划/index.html","300b04b631854bac8bb956dc3aa8a88e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d3c0ca022bf0476b43d063b13b1cb62b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b47bc5e28b4df11ea21f7c48d7e75de6"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","adf549d407c526b943fe4c5c64367886"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","1e09a13a0bedaef09681f3f003e5d18b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","cf3580dd2ae6430a04b886e5e6eb234a"],["/categories/数据结构和算法/算法题/树论/index.html","5ad2f9f295933c9dbe446422cb36e9c8"],["/categories/杂七杂八/index.html","e59cefd1911e3cda13121518ae9da943"],["/categories/杂七杂八/博客搭建/index.html","4d374f0210144ed4e0e834a5f526853a"],["/categories/编程工具下载/index.html","dd2a6f8476d30adc64d2e67760cdf8ab"],["/categories/编程环境/index.html","872bb4d0cbfd2166f42a67a535d91a09"],["/categories/英语学习/index.html","2f93fe4c16ed83d08105b9b64ba5f740"],["/categories/英语学习/英语语法/index.html","2b18fa718daf1f36f38b899d67a3b69f"],["/comments/index.html","63f2d15b2e075645a577a871c71ef4b5"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9b294561a83385c456e20fd123a62a3c"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","8a26b17d35f8761e5e596dc31e33f698"],["/movies/index.html","3b03989ed69a861b7327025e161b1bac"],["/music/index.html","3677a750fcaf25f06469306f13dfe9d3"],["/page/2/index.html","c81cbe6911d6ed10d4a52d1046d4c5f0"],["/page/3/index.html","d1ce3389f9b748ec34d4f7fe9f0bffad"],["/page/4/index.html","57a93aa32cc6f022bfc0d8d9682ade1c"],["/page/5/index.html","b3ebd1189b031ba1ea7c7b49c956d04c"],["/page/6/index.html","7d6a06534bd2f70910110f3d334c580f"],["/posts/1021360842.html","8b7b5b203c1b50556fe33141fac4007f"],["/posts/1120620192.html","f724840b55768c0220a5702f979b6049"],["/posts/1141628095.html","00c74b67bb22364797d3110448e29f13"],["/posts/1168613674.html","e013d547666712d3d754929de397c4d7"],["/posts/1219920510.html","326417c1784f9d8c88cc7d6cbd265145"],["/posts/1222166338.html","1eda7c5fab1f71c42ed3e333634b59e3"],["/posts/1259097482.html","9dc91ba95fc860534be9d3ea9211cf29"],["/posts/1271036369.html","742fc0f7bb879545c77f9955d7163b7b"],["/posts/1312847445.html","86d2a6ec1c768bb747fba23794c03b87"],["/posts/135355774.html","80ed235a4751dc0279f2efbf7d2531f2"],["/posts/1375344716.html","25e04cfe49894391b3bb10f89a976e1e"],["/posts/1388991698.html","16273b3c95aeb423b754e3ce78c989a6"],["/posts/1410315814.html","50fb6c33f1e39b44100e0c549b42d5fd"],["/posts/1452790229.html","84269fa518710178109bc6d9ed398da9"],["/posts/1470079884.html","e03cc8fd783351d0bc0a68a4ddbc3e9e"],["/posts/1470079885.html","4d4b4189c3fc9321ec1547abdf7c3590"],["/posts/1470079886.html","e21649ad30f1842557d626c524057b75"],["/posts/1470079887.html","3d2e330ccc03c53ccc81a7f42516d56e"],["/posts/1498536549.html","0ad8bed83ed73342d11ebbd56b599750"],["/posts/1547067935.html","63081b64749d8a32614fee47c84e06f7"],["/posts/1557866301.html","59e3d3d4c24bdd4236ec2f38264630a0"],["/posts/1571776361.html","e657ec80ec2bdfeb3055115e8f611183"],["/posts/1605124548.html","859b2265cb0ebeaa2b5743422ad6e9a6"],["/posts/1633036852.html","7a57542170b0d36387def0c8415dd1c2"],["/posts/1765123828.html","476022e8b0074faeb1bc31df4c8f6e25"],["/posts/1767336200.html","d5704de45097d9a7ccc50b25ff20fab5"],["/posts/1776114197.html","46a53dcb17738d3c08a392547cb77819"],["/posts/1817748743.html","6be4e750c148165983bb36f50773bf9f"],["/posts/1925125395.html","297cb9c21216cd4c181b7c697679baaa"],["/posts/1966191251.html","ad3350edd6903c2772d8493893b8214c"],["/posts/1987617322.html","3e52316c6c2ec88e137e4efdefb08289"],["/posts/1999788039.html","65a0d98a1fbf53e67870c2c851ca1336"],["/posts/2075104059.html","09360e289d1005ed8e76d56cd6d55ed4"],["/posts/2087796737.html","ac19b600e82544a037c9f968434b537b"],["/posts/2106547339.html","21db66770153d8ea3b2efbcecc68ef4a"],["/posts/2207806286.html","ecc51fe86d59071e3d7ed321eb0094eb"],["/posts/2225903441.html","c777ba760d3d7578b5eba4c502dc651e"],["/posts/2265610284.html","5d10930306a30df411259911c911feee"],["/posts/2281352001.html","61624b50703b84610500f974b2cf6365"],["/posts/2364755265.html","317f85bb38ea75dd95a45fe283127586"],["/posts/2414116852.html","a86095ae7856e971bbcbe9d671c1dd13"],["/posts/2421785022.html","cf7b7e1bb7b36053529b55ae7810752f"],["/posts/2482902029.html","78fa9d0ebdae2882689b37f6b698cd08"],["/posts/2495386210.html","e9e1d8893a7a228c6fb31d2d0dfcea71"],["/posts/2516528882.html","91d65c810c53cab2eb761aae78b375d2"],["/posts/2526659543.html","2ee86d18d61550078d9c47fb3d4b5732"],["/posts/2529807823.html","5e3b1bed895a3ff9f970f2288b277c12"],["/posts/2596601004.html","acc137c5f9dd833b55c3e25db801c092"],["/posts/2742438348.html","ef824ed8e522f7370c0697edc8436d05"],["/posts/2888309600.html","7eb0f4b9aa94a9f61052ac17fb607f1d"],["/posts/2891591958.html","612af9b2a13936225d4376dc6dd7b965"],["/posts/2909934084.html","a1a078b6064bcd008d31087b9c1cbf8e"],["/posts/2920256992.html","f4d11630051ee4c246d1e80332fef6d6"],["/posts/3005926051.html","994cd8f072a09dae13b0b0301aaed3c2"],["/posts/309775400.html","8145d6e8b5423173406262fca84f61bd"],["/posts/3156194925.html","c2ac898c9591bde73baf0f63be7957eb"],["/posts/3169224211.html","1f9f175ba8cb4d0ff5ed18d413942bb8"],["/posts/3213899550.html","70a030b14bf77cd3bbe513b5b261fcec"],["/posts/3259212833.html","3737aeb1431e18cd550cd0852f31ca11"],["/posts/3266130344.html","12baa73722f6c6005fdd9ed17df5201e"],["/posts/3297135020.html","e518b29f6f3c2ca12c96416c944604d6"],["/posts/3306641566.html","af54c09d35b1ba662ddc760982821cb1"],["/posts/3312011324.html","f29dd77d884bf17349e1dc670a7824a8"],["/posts/336911618.html","45feca3e0cf6dfeacf0ab49987cfa9fb"],["/posts/3402121571.html","4b0b9a5ac5a0a1ee384b93894684fa7a"],["/posts/3405577485.html","2dfa86fe637c31fc79b9351f9cfb2bcd"],["/posts/3498516849.html","daddb0594ae95ae282908da324573992"],["/posts/3513711414.html","fc8be7eb38c7a5da6fe93f786b0df6cc"],["/posts/3546711884.html","17d214deee23d817a3cf662cd1814532"],["/posts/3731385230.html","fc1d6921ec1c14ee1d16d60e50ed75b3"],["/posts/3772089482.html","e79bdb04bbd89d5e5a214601b60502d0"],["/posts/386609427.html","27eede6b77f3788a7a25a0ef1b9649e4"],["/posts/4044235327.html","61246fc3bced773dd89375767bb14f0a"],["/posts/4115971639.html","0262b91ff75f9679fc1740d74f65eacd"],["/posts/4130790367.html","631dc41ed56d4f2392e4a0f05548eeeb"],["/posts/4131986683.html","89b5ea75bfcb60ca2d1de7d1a48e94dc"],["/posts/4177218757.html","a8da2cfaa4b53457171b67fd2a56816e"],["/posts/4192183953.html","e0c06cc70ac2de0730c8654067fcb304"],["/posts/4261103898.html","26c4eecb0fb33ab209820832e7c3f37a"],["/posts/469711973.html","21b07680c4c895d3b9a537c6048efbdf"],["/posts/482495853.html","cf31e2634cf6379aef30616bd25a3fe6"],["/posts/488247922.html","499c4942766a88996efa51978ad1f4c6"],["/posts/570165348.html","a17d9f497caf93e0193db979b41c0740"],["/posts/595890772.html","ce03372e10a3a615cfe984094bd0fd1a"],["/posts/694347442.html","2cdca59bc38e42d8ec32d9663542788d"],["/posts/707384687.html","f557cdc4ac3abe7cd85478f0624005d3"],["/posts/71180092.html","88982c850d14b0b3d48e340d3df8ee5e"],["/posts/716459272.html","d122dc4bac07d544791535f8af63b8a1"],["/posts/778231993.html","f3f3a63ff00f83cce464b586c734f5fb"],["/posts/795397410.html","bb54150dbbd3b0c9aa4d58244c8ffa2a"],["/posts/820223701.html","d695005011263605571a74dc930f093e"],["/posts/830372185.html","a1370e55df55a90f6a9734f5cf1d29a3"],["/posts/88294277.html","dfe75209fac90f6f5872f0a26411ed55"],["/posts/939963535.html","997cbf2dea3879ccd11ae4a7b6bc4976"],["/posts/983786067.html","5b4ad2234f553af684b759cd950cf608"],["/sw-register.js","ef40662f542bdd88be76834fc4ac12c4"],["/tags/C/index.html","77986d389e68b3c806bdb43addf93655"],["/tags/C/page/2/index.html","27ccff7237ab356d1464bb734715ffab"],["/tags/C/page/3/index.html","281e861fde6d4abd849fc67ddbd65246"],["/tags/ElasticSearch/index.html","a0d68b0fca0f61f343bda98ffad6111a"],["/tags/GUI/index.html","5bd09c5dde1fa362c20b48c19dc568ce"],["/tags/HBase/index.html","a99d307fac0f64a64bc63b3229caefa9"],["/tags/Hadoop/index.html","2c54968e17bda7ac62092f31f04e3cd8"],["/tags/Hadoop/page/2/index.html","922ef8f66c4f6de7ffb7e43ab4abe39e"],["/tags/Java/index.html","bd68ba301b944a5083465b8b619ec23f"],["/tags/Java后端/index.html","f1e5d2a9ed40b268f05e52689017da7c"],["/tags/Java后端/page/2/index.html","983ebeed1a8dbe2327cd5aed91b833ba"],["/tags/Java基础/index.html","3b1040d18abb07107737059438b97b82"],["/tags/Java基础/page/2/index.html","805fcc7d8e5775d6044766f5e66ef1f5"],["/tags/Kibana/index.html","bf8105221b858e06756822842492a331"],["/tags/Linux/index.html","63cf099065c1435f224940c3f153a7be"],["/tags/Linux/page/2/index.html","f8644e340d93aff41e5584c338ec5fbb"],["/tags/Linux/page/3/index.html","455e298c017b174e0f9739b15385f0d6"],["/tags/Mac/index.html","390f81e1e0b03f289ec3b6668d608c51"],["/tags/Mac/page/2/index.html","02a578be915b1c539a4cd238d556d859"],["/tags/Maven/index.html","1989396705b0d6d16e355a690b3b19d5"],["/tags/MySQL/index.html","2a11adfbeb9ccb49bdb0261907c1777f"],["/tags/Python/index.html","8c41b06f00cc9a0f96e124789d988db8"],["/tags/Redis/index.html","1ee19b66adb5606b22f15fccf0ec8287"],["/tags/R语言/index.html","a2975f20b2fb6b3a27f0384caee1534f"],["/tags/Ubuntu/index.html","b1d1f309de19d993af96cfc50ceb0c0c"],["/tags/Vue/index.html","611ba2732ac146eb32f4cb6fd47344e1"],["/tags/Windows/index.html","f6915af84efe8f6559109f8a21425faf"],["/tags/ZooKeeper/index.html","499809c9c21cb0fac1190cd323110ae9"],["/tags/bfs/index.html","da8d33ee9542777f01f97fd280b043df"],["/tags/dfs/index.html","5476323de1b4f1bc839d0b6baaab0a03"],["/tags/folium/index.html","256d59fdd2ccdd31b1afda88a239e873"],["/tags/git/index.html","05392869a87d0ce6c58103ff01e6b221"],["/tags/index.html","b27c6bb478843b6af13d9dda94acee4b"],["/tags/latex/index.html","8bfbd866e999a1d5dac00862cfefdb3b"],["/tags/中间件/index.html","9f62d80bb946e87e8c07839e17a97662"],["/tags/二分查找/index.html","e9cbb1b0fdf8384df3a4eb701b2108e6"],["/tags/优化类/index.html","af97df8e3ca7de981a4e2a99e01a8a61"],["/tags/前端/index.html","1378a2c2c702673574b5dcc3340519a6"],["/tags/前缀和与差分/index.html","a82dd15aaaf5e5981ffccfbb9220cac3"],["/tags/动态规划/index.html","44780f50ab7514a0033749ae3ac529cf"],["/tags/动态规划/page/2/index.html","941133a1435f13572257a8095807df55"],["/tags/博客搭建/index.html","b8d76c69de0b802774a52219de4d6c9f"],["/tags/图论/index.html","2b54f33479fb46b23e5247a72342821b"],["/tags/大数据/index.html","0b2741acb971abb50e14b4ffe5843aeb"],["/tags/大数据/page/2/index.html","e5454854ad362cf7ef62ab37cfd876f4"],["/tags/操作系统/index.html","78a91bf4b049915ecc93eec286b97ca1"],["/tags/数学建模/index.html","f01cb06d6ac569723ebf05d9917a51af"],["/tags/数据库/index.html","a7563e8c612236510fe28556fbfe7f69"],["/tags/数据结构和算法/index.html","68a5fd4a9e686fed9955637150e73640"],["/tags/数据结构和算法/page/2/index.html","35d05d14817c5cf14ed30162cc07e96c"],["/tags/数据结构和算法/page/3/index.html","bf00e233a2dc7dfadaa2fc33a9ed5c72"],["/tags/数组和字符串/index.html","a6cdf14e28f40b126f693c9ca0d9ef56"],["/tags/枚举类/index.html","b83155f97ef1c2e44b883d6d7c443302"],["/tags/栈和队列/index.html","2fe42eb8bf6f06bea93f3c54d5f0c28b"],["/tags/树论/index.html","66e733eecb75b93ea050abadfa0de68d"],["/tags/测试/index.html","9bd75880441e2df068288580a80d0325"],["/tags/环境/index.html","c78351145a5680623a7ee00231c8792b"],["/tags/环境变量/index.html","81a46c526f8ed62094c9ab732a4fb896"],["/tags/绘图/index.html","7371ca9070a6c34c67e105fbcdad691f"],["/tags/编程工具/index.html","c0457e7f2c214424e5e91a75ecade220"],["/tags/编程环境/index.html","9ad6127f557651b7168970caae9d3fbd"],["/tags/网络编程/index.html","1e3845ae0535eb70578620fd82ee7586"],["/tags/英语语法/index.html","e8cc47d85c5e987cdc0b708a7324a649"],["/tags/论文/index.html","9222cb2045c07a82f63df091c8a622f4"],["/tags/资源下载/index.html","cac3ff280dadcf5feddc70be33a6de24"],["/tags/链表/index.html","7faf308f1f93274b26604a32ce661a0c"],["/tags/集合/index.html","fa07312cb1bdfff6c1626b9ae2e66559"],["/tags/集群/index.html","f88b210d23b507414d1fabab16cf64c1"]];
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
