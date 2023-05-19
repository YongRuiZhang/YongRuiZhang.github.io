/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","962ee7df7fab6794205ae8ed25370f1d"],["/about/index.html","607d5d512a1837be714032c1917ce684"],["/archives/2023/01/index.html","01546e0e18aaa82541656b55304de6b2"],["/archives/2023/02/index.html","32338929335501776ed1d4dfa75191fe"],["/archives/2023/02/page/2/index.html","557671f2e19186f384421f36d89adacb"],["/archives/2023/03/index.html","84165a2650339ed512bbbdf4d5343f2a"],["/archives/2023/05/index.html","54a7641350e52bd802852ad5b1593c43"],["/archives/2023/index.html","36e6e66d35cfae8a867deae1d04c7817"],["/archives/2023/page/2/index.html","2a77f29b01058cc57f01ae3dbcefb936"],["/archives/2023/page/3/index.html","185f8dfa848f1260a3c1c7ae7b737d3b"],["/archives/2023/page/4/index.html","369adf87c28b592c5925d41f441ef26e"],["/archives/index.html","5e64a1b0f56d39a7484ef77a1830501c"],["/archives/page/2/index.html","fa7c30673ff6cf779577982a75bedce1"],["/archives/page/3/index.html","0beaaba0bc76d8aeba41cbaf2e091e32"],["/archives/page/4/index.html","dc41f5933faf836c1b2d9b1731801ef0"],["/categories/Java/index.html","be8b8d660095a54d95d032e884b5eda3"],["/categories/Java/后端/index.html","a4097f7196d5e52ab90883130fd1d1ab"],["/categories/Java/基础/index.html","a11601a6b88409d95319a186e7dfb42f"],["/categories/Java/基础/集合/index.html","6dd67e3848fbada852341440dcb8d159"],["/categories/Python/index.html","45f0edda206e0fd1d171c754a3f8d8d4"],["/categories/Python/编程环境/index.html","bec1b09441be82e526dd46bd3669fe8b"],["/categories/R语言/index.html","aa10df9518cfe15f8ef01a6036ed0502"],["/categories/R语言/编程环境/index.html","a0932b00271842ffe8f2cd7fe2d6d295"],["/categories/index.html","1bcf86f425e8d55dd1ae9b56a6291373"],["/categories/中间件/index.html","dc7c3fc4788296d1c522350a90b40342"],["/categories/大数据开发/ElasticSearch/index.html","7fb9d637f168f7f5552dff11800af872"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","4e1a1f51fad21f47d00e389155ee77f3"],["/categories/大数据开发/HBase/index.html","15ad97f63e21a271d6caa6e9de5357a6"],["/categories/大数据开发/HBase/学习笔记/index.html","6d6c02dbcec6eb94ad44e8d375a04f7c"],["/categories/大数据开发/HBase/环境搭建/index.html","27dd299ca3ad07738eaa4784a8e0343d"],["/categories/大数据开发/Hadoop/index.html","f12d6de92642bc3d10543e88433e7b89"],["/categories/大数据开发/Hadoop/技术/index.html","c5ef38a8e063b7eb3153c16a31491ab8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","2c26028dd0cbb7af843b9d7f496319c5"],["/categories/大数据开发/Redis/index.html","f5a528179378a7b8718dd216752a2236"],["/categories/大数据开发/Redis/技术/index.html","71d1e0c7e79831337915ce0d3b8799bc"],["/categories/大数据开发/Redis/环境搭建/index.html","a348b82f9096bc0b7b3f8bb704a3b6aa"],["/categories/大数据开发/Zookeeper/index.html","c52e30f9a9d0b83f1769ef074bfb5394"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","6d236fcf50c1161d28eb42ffc0262899"],["/categories/大数据开发/index.html","282e97ed7a2ba87d734d166113f9c129"],["/categories/操作系统/Linux/index.html","3d352c94d59fd73f5cfcf18e13a532cc"],["/categories/操作系统/Mac/index.html","e513d4cefe2d66d6a954609b396c12e4"],["/categories/操作系统/Windows/index.html","57eb352062ebf7631d407290a8bfe4a0"],["/categories/操作系统/index.html","9ec7798998c69123164846950de3e5b2"],["/categories/数学建模/index.html","ed9d7d9f75fcdb6e307fcfca4df61d91"],["/categories/数学建模/latex/index.html","5f48c11fa9884317b30ebeeeac66872e"],["/categories/数学建模/优化类/index.html","8aa7929ea6ffbf7b36b89f16b24773f6"],["/categories/数学建模/优化类/现代优化算法/index.html","a0d4a6c3d32e43a68b230d5eacdd6791"],["/categories/数学建模/优化类/规划类/index.html","a01e6d0062e1ceaf80acc103d7d0d649"],["/categories/数学建模/绘图/index.html","7dcdefdbc89198ee1ea34f7fa633189e"],["/categories/数据库/MySQL/index.html","8dc0fad5f9e11cc02dbac16ffbb27b1f"],["/categories/数据库/index.html","09ccc768803323132a69ff8543326277"],["/categories/数据结构和算法/index.html","a004955e26f02fe675656ae4b91608ee"],["/categories/数据结构和算法/page/2/index.html","e0ac61ae42209d178ad504ba790034c0"],["/categories/数据结构和算法/基本原理/bfs/index.html","3616d9d4f42ba7ef552f7eeb747ce385"],["/categories/数据结构和算法/基本原理/dfs/index.html","7f61594c0e58e7ece1d15f59d651f9f1"],["/categories/数据结构和算法/基本原理/index.html","9602d780c3195fe43b10106b821e5d86"],["/categories/数据结构和算法/基本原理/动态规划/index.html","895f8b4e3c4c9b58e784521997bff8b1"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","16d9c10a4907b70630dc34dd3580ecd6"],["/categories/数据结构和算法/基本原理/图论/index.html","24022012946abb89ab72c422f0b12d47"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2bac823d0ea0914ebd66ed960338b99b"],["/categories/数据结构和算法/基本原理/数论/index.html","40a04d567e9575a5d85c4fc3caa28c5c"],["/categories/数据结构和算法/基本原理/树论/index.html","6cffa7c5ebdfb2d59fb61c1ccbf7e89f"],["/categories/数据结构和算法/基本原理/链表/index.html","635007a5d15fb36f568e9ed4e9416017"],["/categories/数据结构和算法/算法题/index.html","9533b38856bacd674cfcc3499a4ace02"],["/categories/数据结构和算法/算法题/二分查找/index.html","3a2d307293c24cb727f23c24b2a67578"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","60f1ac00a3f94775efa73a970b7786d3"],["/categories/数据结构和算法/算法题/动态规划/index.html","261038b4b71ffdfb00abc21e4a9951da"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","bb3cf7c4cd0547e0022f172a6a713a53"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","d760c45d5f42a41bb0bfc8ddb79d2bdb"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","ab96c3b961f1158cac22ddd9a1ee9704"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2a9bc230f6dcc38a10964bdce03e8dd9"],["/categories/数据结构和算法/算法题/栈和队列/index.html","98854828d8bbbe944e63a84944282632"],["/categories/数据结构和算法/算法题/树论/index.html","08261455df4f7faf9db0c7329f6a27ee"],["/categories/杂七杂八/index.html","d0d183c4895071b4717659d07051513f"],["/categories/杂七杂八/博客搭建/index.html","152eda34685edeb1158edb2623db3c00"],["/categories/编程环境/index.html","af33b37218300c26fec20a7fea597fae"],["/categories/英语学习/index.html","aba12012e66757f12d64e3d623b86b73"],["/categories/英语学习/英语语法/index.html","13c411690fd94d58c82ae0d0e8bdc52c"],["/comments/index.html","9b8dc0ecd3caf2d4cc432c2de8942310"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","1acddfa00cfb0f506edd1f40e9673120"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","8005068c5b2a72355fbcdb1177a43321"],["/movies/index.html","2933a89a1b5f5fad73f08c23bc2b51f8"],["/music/index.html","635fa4116d171239564b3bc95c95c55e"],["/page/2/index.html","817ea8aa38ed5abf7fb760f452d320da"],["/page/3/index.html","f9f65a96313da9cb219385306dec2b53"],["/page/4/index.html","4b4609c4b05f0d652063791f6652a822"],["/page/5/index.html","8a7df2d8db76d6a27d4026c95b2b40c0"],["/page/6/index.html","6ed36ee70714280b9497d3a2664b6561"],["/posts/1021360842.html","e52cfdb547631dc9ce70fbc54717d1dc"],["/posts/1120620192.html","e0ba459b9db75193a29bf7078a69a582"],["/posts/1141628095.html","608c25bf99b7ad7e208e3688d8adfc41"],["/posts/1168613674.html","a176754ba85e5f967c48f237818840db"],["/posts/1219920510.html","cb2b893cd2e3d9eb3685ddf476890f76"],["/posts/1222166338.html","3c6a4332a0f7e9c051569732858b7a77"],["/posts/1259097482.html","75fd8568452c0b80c70736d8676fcd08"],["/posts/1271036369.html","8e0610bf7d7805e66ec9b11277e69f12"],["/posts/1312847445.html","c6e89987b1faefaa320b55e5de0b71c0"],["/posts/135355774.html","0a3385d8de3977bf58ec4af1c47c92ed"],["/posts/1375344716.html","699248e437065d74d4476e0e0b95336f"],["/posts/1388991698.html","c1a4f9a851bbe6ce28af689863723b59"],["/posts/1410315814.html","d0fdc6856d5f0b8f78d7b81f55d08574"],["/posts/1452790229.html","094cd282e3b5dd99dd4b18be20187164"],["/posts/1470079884.html","d6d563804da664342920846a549655b7"],["/posts/1470079885.html","acb0a4375ab3bf60b3330e969ea711d0"],["/posts/1470079886.html","ccd0b525ce103f6a43a86118dc62a729"],["/posts/1470079887.html","2c315aa89d01346456bc66859b93f769"],["/posts/1498536549.html","f6e093fb9cd2949d65abfeb3a2f25458"],["/posts/1547067935.html","65c8fa930b0955d025ec9b6981cfac70"],["/posts/1557866301.html","37bb60ba6b2ff8c83044f515de5bd044"],["/posts/1571776361.html","a597c22b9102af0a3e22ce2a54df385c"],["/posts/1605124548.html","343e558fb21603a3833359ad59fba1f1"],["/posts/1633036852.html","6f93d63b86eba0aa54323a6123cb4e05"],["/posts/1765123828.html","1867ba04cb49c96eb372c08608180723"],["/posts/1767336200.html","98e281d7bf1d34f5f92bb97cbde3932d"],["/posts/1776114197.html","b3b524c1971e573b8913dfe69b12a0a1"],["/posts/1817748743.html","9d679260f65287b257f577ee220a91c7"],["/posts/1925125395.html","0517c606772c3b286e638d5e7848a1ce"],["/posts/1966191251.html","b62a6681596fb756dd33686d87418c36"],["/posts/1987617322.html","d334ee07f8475d80b828a3a7bb13c450"],["/posts/1999788039.html","7b6d0076e5951793a18a78545698186b"],["/posts/2075104059.html","67b6f5898ba62446e249147ffedb9332"],["/posts/2087796737.html","008d1953934eafe18cc7a52a19198f38"],["/posts/2106547339.html","61417194b1f3fc7f3d76245c2fbe7f7f"],["/posts/2207806286.html","56edba8f15c808ed4e9ddbbca34a7e5c"],["/posts/2225903441.html","6e7c138e4178f97a7f4e9dfcace2d2d9"],["/posts/2265610284.html","6e37dd13d64f84b7200fb3cd91bf2a6b"],["/posts/2281352001.html","a8bad4e8f0a82cdf64d08d49d113cd30"],["/posts/2364755265.html","aed722469c31a7f7c2f362aac170e73d"],["/posts/2414116852.html","8de98d1bd5d785aef4ed63ee54738bcb"],["/posts/2482902029.html","b777d4c04b9b62f12f0d17809def3f47"],["/posts/2495386210.html","f5188c1a6d0ae3e3cf05517b8e51151f"],["/posts/2516528882.html","6b91600fd53fe23ce2a8d3b4cbf9a9f5"],["/posts/2526659543.html","d80a5de6a14c936d6eb7b480405aecc0"],["/posts/2529807823.html","a4b0b5b10ed150d989633ebf5d8fccdd"],["/posts/2742438348.html","5161da9e4ba3cd2528886858392306f0"],["/posts/2888309600.html","80d9b9ba821a73e86609b65167b8dfcf"],["/posts/2891591958.html","931c6eeffa84074790c4a867e9e35cb2"],["/posts/2909934084.html","7dc3e43eaf0faca06038e6de819e6caa"],["/posts/2920256992.html","fca8f5ed2aec7f67bc40cdcff8d9f0d3"],["/posts/3005926051.html","6ee46e2e60ac10375ba13a1373e18449"],["/posts/309775400.html","80907fec706f859f0b7d18d21ba0dd75"],["/posts/3156194925.html","0526972a7396e77fb2826b38fa34452c"],["/posts/3169224211.html","df8509f0aa1a74fc6d82aabafaf6e479"],["/posts/3213899550.html","ce53e5c42cd055fd67b59d687b98473c"],["/posts/3259212833.html","7d04c5a0523b40ec48556c63e71c2375"],["/posts/3266130344.html","5517bab805db1b523afa51c6e0adf5f1"],["/posts/3306641566.html","6a96674ef68b21153e5f288e8c6404c8"],["/posts/3312011324.html","02743deb6d6efdd79b5e79491a08e288"],["/posts/336911618.html","764a784c7ecda88a8d3e7de243defa8c"],["/posts/3402121571.html","eded7ad13023c093984d62858ae9b859"],["/posts/3405577485.html","79929dab343dbab15c7bd4449e813312"],["/posts/3498516849.html","06f77d067853318f16bf5b471a0c6fba"],["/posts/3513711414.html","8c29281d0769c240b6a52bb99f200bf8"],["/posts/3546711884.html","60e253cb824b3c797ef0e545c26abb4b"],["/posts/3731385230.html","5d516f419aea664623917c3536fa5f95"],["/posts/3772089482.html","cfe659c584d2fbb58507aea0f9e46679"],["/posts/386609427.html","e46a54c1ca8f2b1016b4825ca4298e94"],["/posts/4044235327.html","0aca5cd7a43e977814976003fcab850c"],["/posts/4115971639.html","5859ed6b35df8a0db5090599b5678749"],["/posts/4130790367.html","7d1e0a086ee03dd75190b3b935711a7e"],["/posts/4131986683.html","ba99dc36cde3a4af7005033eee3cac9a"],["/posts/4177218757.html","5ab388efa079b5e2a6db6a2103a2563c"],["/posts/4192183953.html","a1b7fcaa65cd5c5d6e7eaf8f61ac8406"],["/posts/4261103898.html","863f02c5ef71d34e5cded55dfc4e483c"],["/posts/469711973.html","0d4e38e742bc8f41c42c82f14414f383"],["/posts/482495853.html","8fcd170c5ffe81bc133f1327971bbc35"],["/posts/488247922.html","200816312bcf43de806b709b65df96ff"],["/posts/570165348.html","91e6fe7969b2812d4b1f4fb14be85ad4"],["/posts/595890772.html","efed023ea13066e1e7512bfe9503a0c6"],["/posts/694347442.html","87fc6341152767c1d9c6d565ec000071"],["/posts/707384687.html","ac2fcc1b2b0e807a87ed62f6b6a28f1d"],["/posts/71180092.html","a22feeed9c76e8db698596ebf0b2ee6b"],["/posts/716459272.html","cb03dd037bf95a810a899c287db1c502"],["/posts/778231993.html","b8b8fb45a7c6327e29fa791da75a0a78"],["/posts/795397410.html","dc7898be064715c0fd654450cc4a21d8"],["/posts/820223701.html","b818e7bfe2dbbc83f869a6aaa423db5e"],["/posts/830372185.html","a103d167c15000f0215ad11e894ef851"],["/posts/88294277.html","c3da2fd8ed4733358664ff65ef35ae0d"],["/posts/939963535.html","627297e0fbf9fc9ee8763d204e1351db"],["/posts/983786067.html","ae72b76472518d102b41179ed5caf127"],["/sw-register.js","44866bdc5034a96aabcfe717b979f4e5"],["/tags/C/index.html","1738a612beb7acf5dc150dac905784f2"],["/tags/C/page/2/index.html","4b444b8c7b897d292924aa9c9d825be5"],["/tags/C/page/3/index.html","727120adec7ead31c0f2a6802891d418"],["/tags/ElasticSearch/index.html","9aaf0daf7fec8283700fb96ea520231d"],["/tags/GUI/index.html","4c7a2393d0951efb4236fad2bcfcddd3"],["/tags/HBase/index.html","59e60cc6e5951bdaa02bae5063c869f8"],["/tags/Hadoop/index.html","6042aa83cd948e4b33b0b6e83a9fc090"],["/tags/Hadoop/page/2/index.html","514ed1731451a1ca58e64cd966f07484"],["/tags/Java/index.html","b0f92ae012a03529835e17e67f2fcce4"],["/tags/Java后端/index.html","3348642c6d1fe291cd46a69c118b4f88"],["/tags/Java后端/page/2/index.html","5e9c1d37b94c8356c9fe82d28bd04752"],["/tags/Java基础/index.html","032d751642218226f4a66c9f2ea7413e"],["/tags/Java基础/page/2/index.html","94eb6a88d3afbe1b08f0e5426f0f7394"],["/tags/Kibana/index.html","c84a6230daf50abc23e74bdeee7ea326"],["/tags/Linux/index.html","8dc5fa39d52f69a602edb0fb66a2cf6c"],["/tags/Linux/page/2/index.html","627920a41676f115edf3e1c7b64d67a4"],["/tags/Linux/page/3/index.html","2e4a519b7f15d2e8ec88589d4b472c44"],["/tags/Mac/index.html","18cdfb19eabcec33c5dc1fa8bb1db00b"],["/tags/Mac/page/2/index.html","c7d0380ed6fa5163c32f9990509ee396"],["/tags/Maven/index.html","8f620d5545681edcb426c3c076b69930"],["/tags/MySQL/index.html","917d6c2356b294887e85ccbfe13f5d3b"],["/tags/Python/index.html","d8a8b2e1fbf23eda40137fbf4a463344"],["/tags/Redis/index.html","52e53f688ed1002ad468a91c819b97a0"],["/tags/R语言/index.html","4b5bb6b5b7841dc0f289ac33c64c734a"],["/tags/Ubuntu/index.html","474a35bf2a6cddbb5cfe53ac7f7afbc2"],["/tags/Windows/index.html","993b0e8153831b18b0947a12777a1b65"],["/tags/ZooKeeper/index.html","ce6471b183f1671939c26a34e48f659c"],["/tags/bfs/index.html","97b01d8edf6958e193237313ca88f40f"],["/tags/dfs/index.html","945049e38be3f092104b2df21a3ce8c4"],["/tags/folium/index.html","f954a5e9e0e0a56744987c721480c4c1"],["/tags/git/index.html","b634c53ba828ab22a86582945bf9d91a"],["/tags/index.html","e933f8046163c197ded0de50ceca383c"],["/tags/latex/index.html","6b999e772e4924ca66918eaa32d12a93"],["/tags/中间件/index.html","48a496ddb5994ca550aa333809acea39"],["/tags/二分查找/index.html","bfa8c8dc6d8e7909001ee8216a71c1b1"],["/tags/优化类/index.html","156b58af18d1e977ad48b400919cffb8"],["/tags/前缀和与差分/index.html","d02213ba72820b6fe376a042df79e188"],["/tags/动态规划/index.html","24f086336f76a40a4caf7598bc17fb48"],["/tags/动态规划/page/2/index.html","48a715baf5aacf5e4c8702c035b2ebdf"],["/tags/博客搭建/index.html","2331d4df5952556767ab7ddea0b0291f"],["/tags/图论/index.html","595d5b77a9e248f10cb8bf4cf8ec1c51"],["/tags/大数据/index.html","ca0e0185a47e5bd660a72cc6aca47b43"],["/tags/大数据/page/2/index.html","0bfdcc71571863c8a22b23bbdb020f17"],["/tags/操作系统/index.html","54217a7258d68930c94816970fad6bde"],["/tags/数学建模/index.html","fd63f3e910e380635201dcce62ed8e88"],["/tags/数据库/index.html","37c7d048d955549280df14e54d91e43f"],["/tags/数据结构和算法/index.html","1b908c02952d2af2a88e3dcec192d8d6"],["/tags/数据结构和算法/page/2/index.html","8fedb1f081a651ef131f45afb7c04241"],["/tags/数据结构和算法/page/3/index.html","b083218a74d91741bc7c8f9f65e70ef1"],["/tags/数组和字符串/index.html","91932b8dcb8d2a78a64abc360648c359"],["/tags/枚举类/index.html","64fb1acc6f6208ba2227c2f1d4ffe295"],["/tags/栈和队列/index.html","f0dd065c50a92f8a30bd9eb87a76d26a"],["/tags/树论/index.html","887e463ca197bec8acb0590db1f07962"],["/tags/测试/index.html","7901376e2cadc4bebb8dc18a757b2f35"],["/tags/环境/index.html","5bf6c66e767cf35cd260cca2e1a7e219"],["/tags/环境变量/index.html","c2e29c2f85bfb3cf7c814777cbc3a01f"],["/tags/绘图/index.html","daa860876fad1d65b4d9e9621bcc98a2"],["/tags/编程环境/index.html","96b6909796a93ea2d70bb3a7c89d1dab"],["/tags/网络编程/index.html","5e7cf5b5c8c62a53321c5536e730ff6b"],["/tags/英语语法/index.html","b09b0ced7229cc1a32ed256799b8c47a"],["/tags/论文/index.html","d0d5dbf403a056cf9adadf1ca9cfa31f"],["/tags/资源下载/index.html","1a67bd896813b17eb1f8a8ccaefcdd08"],["/tags/链表/index.html","1cd535f5942f5629f25aab3cc3543200"],["/tags/集合/index.html","91096d5949a0f70b77fc349c70055605"],["/tags/集群/index.html","45cc7b2ff468f3868a0a512d635b3b89"]];
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
