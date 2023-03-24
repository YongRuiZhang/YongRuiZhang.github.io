/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","9cc26f59e44e24fd78048de0ea27c8b2"],["/about/index.html","cdfbe76e0f6a887f6bd31ecd927cbfc8"],["/archives/2023/01/index.html","0062aee58fd8b68c3bef1e0e7bd5bf76"],["/archives/2023/02/index.html","b616b95909bc8e15e09a51e5c6bfcdb4"],["/archives/2023/02/page/2/index.html","b5007848fbb955528b2fe0cea6b506b7"],["/archives/2023/03/index.html","8c94db18a507151aec908ca435b82603"],["/archives/2023/index.html","041b45e5590b73329fef3b3be2d73381"],["/archives/2023/page/2/index.html","8853a297b0f36342c48acfad98bf538e"],["/archives/2023/page/3/index.html","7a21d1f5a6c90e874bfb93a3214c48b6"],["/archives/2023/page/4/index.html","0adf4e375411ee900e30328564ee4a5e"],["/archives/index.html","d3fb65a3ea1dc0b03c35f07dc06b8af6"],["/archives/page/2/index.html","fda83dde0067c7ef2a1081ec4defa4e5"],["/archives/page/3/index.html","229d197eebb3840cca0c83298fa86e27"],["/archives/page/4/index.html","46e948b45bcf2db3110a685478c4b82d"],["/categories/Java/index.html","eb26a5b6e2472b09996813975521c8cc"],["/categories/Java/后端/index.html","610236829508527198281bb187aad522"],["/categories/Java/基础/index.html","867a1ef73fcb4bb4db255df33c69666a"],["/categories/Java/基础/集合/index.html","090a8a48c647206478435b0818e0d6d5"],["/categories/Python/index.html","5e77076c71d39379495120fe6d015e6a"],["/categories/Python/编程环境/index.html","5239948a9e3d4b907dc0c9f9f03b9d88"],["/categories/R语言/index.html","ed15b27a0c745b8d345849cecc1930be"],["/categories/R语言/编程环境/index.html","338119b85b5f203a9fd8788b49201f8d"],["/categories/index.html","6be221e36eb521acaf8ed27e2ae28e45"],["/categories/大数据开发/ElasticSearch/index.html","dfc0665de6b82682c9f0b0f76fc1bba3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d6f299ffadc9fa308a6da418a808e012"],["/categories/大数据开发/HBase/index.html","af9a3a59487de59dd0a6835a7f71a87d"],["/categories/大数据开发/HBase/环境搭建/index.html","0fca1e4c8df1de84f853a99e55f88e5d"],["/categories/大数据开发/Hadoop/index.html","413ee758842d3bcbf33c2de22a31db04"],["/categories/大数据开发/Hadoop/环境搭建/index.html","53612a339c950439082281d781a777f5"],["/categories/大数据开发/Zookeeper/index.html","28126b8fa9c90e34b85ceb27b28e536d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","780df31e113dc451df08db812d2b02db"],["/categories/大数据开发/index.html","93013f87d85927a6fa8c03dbf552a722"],["/categories/操作系统/Linux/index.html","4c21de339891586eba1b7239cbee52d2"],["/categories/操作系统/Mac/index.html","31233bc8a2fcf99c2caf8bfac60bbf50"],["/categories/操作系统/Windows/index.html","b85d93e9273a6c9ceb4979dac4664c2a"],["/categories/操作系统/index.html","dfa757428244584eb05f63b81a276af1"],["/categories/数学建模/index.html","c2d560a84f2145b6b77e340b94a051a0"],["/categories/数学建模/latex/index.html","6371cb034bc56e5aac2bddc325cc5327"],["/categories/数学建模/优化类/index.html","6d57ae7898c82cbeb9a93f422136d367"],["/categories/数学建模/优化类/现代优化算法/index.html","88d56af93621b0dc737a70148be689f4"],["/categories/数学建模/优化类/规划类/index.html","3ed7811a01c92f064168600323c5056b"],["/categories/数学建模/绘图/index.html","758d0a628f3a98cf0347fb2abafb4e84"],["/categories/数据库/MySQL/index.html","601479bc8a515c8a382fbfbbf597209f"],["/categories/数据库/index.html","0edfecde67f17d025e12259812db19fd"],["/categories/数据结构和算法/index.html","b12ab0550c4617faabe30a94c2a6f371"],["/categories/数据结构和算法/page/2/index.html","80d356001d12380b38380645353528ef"],["/categories/数据结构和算法/基本原理/bfs/index.html","61940125a834004f801328be5bcce1ff"],["/categories/数据结构和算法/基本原理/dfs/index.html","2569fb8a7b4bb46dda2dbbcd39aa0f63"],["/categories/数据结构和算法/基本原理/index.html","ece66f7970060b752b23ebe36e1d6888"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f1e7e553fcac229765f2702e27f13a38"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d5d84129e54c274affd677e97dc22929"],["/categories/数据结构和算法/基本原理/图论/index.html","67376ce260fa87dde56d5a4785005274"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d14772ff1ff42e2bcbad0dab72122359"],["/categories/数据结构和算法/基本原理/数论/index.html","21c84a2ddce2d9035a7b50e22ac27a87"],["/categories/数据结构和算法/基本原理/树论/index.html","6209d8f005a68d6f10b3f48b21d19f80"],["/categories/数据结构和算法/基本原理/链表/index.html","cedb69df0e30a9771a62efd619d0124a"],["/categories/数据结构和算法/算法题/index.html","1502d5ce236f373124ca6ee36d1599e4"],["/categories/数据结构和算法/算法题/二分查找/index.html","b1c0862d8253c1710220845110effb69"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1bad41003229c0ce4ea078a4612f5601"],["/categories/数据结构和算法/算法题/动态规划/index.html","1d9791d6b091c8d27db64e2a1d233a81"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d00db7112057b133c47f73024b10bf8a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","77466acfb7410528e24b6ec4374e02bf"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d3c21d9d2c0f8df04f9ce7676f5fcac5"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","f5d33c2b3ef6f978de0ef6153e499d5f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f515fbb25503fc48fc47ccbad4065681"],["/categories/数据结构和算法/算法题/树论/index.html","bffe9e20e90919a635d1f03ec9fe1e5b"],["/categories/杂七杂八/index.html","11176c654bbaded2bb7d5f2d77b102aa"],["/categories/杂七杂八/博客搭建/index.html","95a6c296e72e45b532f878b8e465d23e"],["/categories/编程环境/index.html","52ce75a8c6df7f302c07e7a9c78854c6"],["/categories/英语学习/index.html","dcc6213a87e29726ecc50f4ef6f0555c"],["/categories/英语学习/英语语法/index.html","c81833e8a4912eb5e8aa5788c6c61cde"],["/comments/index.html","f2d432bb2cef6d710e0ed652cca328ec"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","7c1786ef76b836d3e8e05040e1be7686"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4ad8f422a9c1d2606bfae9579e3450b1"],["/movies/index.html","cdf3ed022e30b73c7cbc6eb283b60ba0"],["/music/index.html","ec4c6d6abd23af5cdd6f8932dfd6401e"],["/page/2/index.html","5594ebcfb2e6ccee17fbfce9dafaad66"],["/page/3/index.html","d2f10438edb19f8878602afb3df5746a"],["/page/4/index.html","bac016eaf087b14688d5c46f606a0d2c"],["/page/5/index.html","cfba9a0d3225910ea63a608b88225087"],["/posts/1021360842.html","9764201c735ba34bb431b09ec38de1e2"],["/posts/1120620192.html","c4c019ad0d06cfd8664d16ac1f4c66c5"],["/posts/1141628095.html","dfd04dfad86501389200f035fafc5227"],["/posts/1168613674.html","960f55c63f3c03094813fb77706e5dfa"],["/posts/1219920510.html","d42f3f5def414bc71a68d09e74f0ec80"],["/posts/1222166338.html","27e2ab3356ca212aa3943db67723c8cc"],["/posts/1259097482.html","d4854e83d078c102f9fb45e7261812dd"],["/posts/1271036369.html","85546a9f10ebc056746f5e71b9dafcb1"],["/posts/1312847445.html","aec400f3f039e047b6926eff827a009d"],["/posts/135355774.html","290c1cf9f8ed95adc0e1b5664864dcd0"],["/posts/1375344716.html","ff6607c01bfd62d2ac5d7aa4d55d25bb"],["/posts/1388991698.html","e5846dbfeeaf1b0f625ff2d6ec7c33b4"],["/posts/1410315814.html","055ea84304fc29cf7dcbee04e5484c00"],["/posts/1452790229.html","d3b2bc6f403c18c34ceebfc9dc39e0a9"],["/posts/1470079884.html","2a47af694e84fed657c9f4ad82dbb645"],["/posts/1470079885.html","6898893183825c82c344c58dae1b4017"],["/posts/1470079886.html","f9824b68f19c3eb65817d63bd9dad539"],["/posts/1470079887.html","56d2dcde88c2ad2af0ce4c947ad04cf3"],["/posts/1498536549.html","fa0c1bcaaa347dce27529522fc6ecab9"],["/posts/1557866301.html","9e4a4dde5e9234cc7eeabb8333e6afb4"],["/posts/1571776361.html","4d9049b2e3853dd53c03a97c90e7bf9f"],["/posts/1605124548.html","cf6d7f4224d369af74b3dc05fbd53822"],["/posts/1633036852.html","41acb5616b3612dd770bfacc4c96dedb"],["/posts/1765123828.html","5472fb5b84197621f6efdb0a3aecc84e"],["/posts/1776114197.html","b793b7564dd918a8030af378fb46eef0"],["/posts/1817748743.html","e897aafc02dca968e61d386f6530a576"],["/posts/1925125395.html","600d95c630beef71511527259c8915f0"],["/posts/1966191251.html","3f55d2e70ecef69c6c4102fbf821d4b5"],["/posts/1987617322.html","4fc6dabbcd017f03ce436048ce5a6677"],["/posts/1999788039.html","1bb3d6cdec7aebdd7e7a28be947caf95"],["/posts/2075104059.html","1618b2c3e3a9bfb9aeb8e4c29dc29444"],["/posts/2087796737.html","5e36427c6411f3587c3a8714bd2f68d9"],["/posts/2207806286.html","f748423972edba5031a356c239492317"],["/posts/2225903441.html","5a6e8e82dde32b6127d2a02f860aeb11"],["/posts/2265610284.html","963fa96260fd461cc2a528d353eb7297"],["/posts/2281352001.html","b1bbaddd91871913e16be21fa8fadba1"],["/posts/2364755265.html","b4ff3c7a6a79ed929602bee2a7468c2c"],["/posts/2414116852.html","4b85e9bb4c50b7332567ae92d0332c59"],["/posts/2482902029.html","b2ec6e5bc34efc19b5508eefd8825471"],["/posts/2495386210.html","c79b7b18f3653c96ffa691f5bf907543"],["/posts/2516528882.html","4efb47623bb435f569aa93da9730bb48"],["/posts/2526659543.html","f0ab34b939152dddd1bd6ec4b43a63e4"],["/posts/2529807823.html","c70f1eda15cda85e923694d93a02dab9"],["/posts/2742438348.html","850597ebecbe36f58936d5a8badbaf6c"],["/posts/2888309600.html","20fa01cbee93b536583d4cc117bbae14"],["/posts/2891591958.html","bfab9efa039b49f3e50de79e9c8b237b"],["/posts/2909934084.html","d0091e10e7cf70bb19e22968ee0311c9"],["/posts/2920256992.html","f66bc91cfb97abf020ee7b271e3846dc"],["/posts/3005926051.html","2bb24f7a10edfea9ba0b39b3bdb0c410"],["/posts/309775400.html","42a2e12c8a75755a62153fd531197568"],["/posts/3169224211.html","52ec9070819e69a6a140b0ce256afb1c"],["/posts/3259212833.html","31d1ff61e5deeec9b2fab6f41dc01b5c"],["/posts/3266130344.html","f8a5b038f27760ea14b2c05b8b66202a"],["/posts/3306641566.html","e576ab809c5ec04bcd7c67ebfb23090c"],["/posts/3312011324.html","6c56d51cccbd3360853083cc008da508"],["/posts/336911618.html","1b3b4f9ae8b5e0c8b10b9f4e016818da"],["/posts/3402121571.html","86eecc7b4d90c3d0b35dcf4e245201b6"],["/posts/3405577485.html","7381bca383d04f5798f962e1dd0b8312"],["/posts/3498516849.html","d670a7b5245af03f0faef585acdd061e"],["/posts/3513711414.html","9ae6983dabc5f4b6ed9af18bc2f66dd7"],["/posts/3546711884.html","ae6e678d8504cab1560c80ac1fe5ecc5"],["/posts/3731385230.html","c3455063304120e360fb0a4e8c277df5"],["/posts/3772089482.html","aa0c8f2d41665dd34b9fd46dd20f658a"],["/posts/386609427.html","95f28ce33d5ed238d92261170883df8c"],["/posts/4044235327.html","c440df7f4fd429c795622d9f36223d42"],["/posts/4115971639.html","4c53ab3d2fe65c132225dceace2cb904"],["/posts/4130790367.html","0dc8e1d24593ed794b235a695c1fd202"],["/posts/4131986683.html","583172dc3d155789f308ec39605504a1"],["/posts/4177218757.html","f2b670491661901f3714d1b7d3f0c6e0"],["/posts/4192183953.html","9afa4166cb9cf81dbc6e0b75c3195a33"],["/posts/4261103898.html","c0cd9b3b8a9266325563dcd330493dda"],["/posts/469711973.html","3c4648dab1d9acab72d5826ac4594527"],["/posts/482495853.html","2cf0390deda45d77b56b16197daa6091"],["/posts/488247922.html","a9b77bd8f0eb2c2def928136d6404137"],["/posts/570165348.html","bf8eb1cd5938c8d968c76a47ab43e3ff"],["/posts/595890772.html","9f6f190c7ad3727cf2596d471e372a4c"],["/posts/694347442.html","5a12cfb3a95844cc09ba684317a54c8f"],["/posts/707384687.html","2551a06f98a2433a9f3a230b7ad732a5"],["/posts/71180092.html","eaf1d46e3e709648d5a69eb5b9eda72c"],["/posts/716459272.html","5d681c6cf0e84883b2c14b7d1d392d84"],["/posts/795397410.html","76095c8d6c8a26b432d013f52ea2fb78"],["/posts/820223701.html","c7f970aa41d77330271972568b232dd2"],["/posts/830372185.html","d3cc65de60004b9c78834b0dc68073b2"],["/posts/88294277.html","2411e4c0790d7db81c1d676a47d6fb93"],["/posts/939963535.html","82d0ca87c3c0311954bf527ce747cd30"],["/posts/983786067.html","cbb00421e4fb666c9b43175fc949d2f7"],["/sw-register.js","09d89d0b637fd23a93d9b685bb419265"],["/tags/C/index.html","5eeb8e7dc39b971ce7691152ef17ad38"],["/tags/C/page/2/index.html","5de493473030a8721db44ba57b3f74a3"],["/tags/C/page/3/index.html","d8c01ace57b13ff3b6cb7a8c7150f1c5"],["/tags/ElasticSearch/index.html","8788795a854de88d7b65da3861849ae1"],["/tags/GUI/index.html","7523ab76d5f8c9c6ecaf662d33e65a4f"],["/tags/HBase/index.html","5a8ffe089af45f54facb21e87ab8bc25"],["/tags/Hadoop/index.html","3c365804c88d163787fefd20192d93a0"],["/tags/Java/index.html","4de73a8def40fe6d5ae05ea64ada119f"],["/tags/Java后端/index.html","902f238b14c01ee5195ee5d7a279e26e"],["/tags/Java基础/index.html","5fb385eca6c4ca6a16169bc4def223b7"],["/tags/Java基础/page/2/index.html","8322b96e0fc6df772cdad6846e259544"],["/tags/Kibana/index.html","6358371b45706ebe7cb4b74283baa863"],["/tags/Linux/index.html","5655df8a23b26e89a8b62a0e7ca57ec0"],["/tags/Linux/page/2/index.html","9bb406b65fe04b3a3f90fec4cb332002"],["/tags/Mac/index.html","e4193e9b62f643915afade18679c8335"],["/tags/Mac/page/2/index.html","f998d9e44e4d516bb67e3889c0f2490d"],["/tags/Maven/index.html","5d66a775e0898f4f349f468c03208cc9"],["/tags/MySQL/index.html","b346b2b1f872c08b5efc495931a8d49d"],["/tags/Python/index.html","fdfde057b4145febc0c6d1804875c2e5"],["/tags/R语言/index.html","a8de24f0d20b47a76224d6f0fb0df3e5"],["/tags/Ubuntu/index.html","39c4d206de9562e4e246d465a2a89e31"],["/tags/Windows/index.html","8228ec05156db0657345912f3294c7fc"],["/tags/ZooKeeper/index.html","2d580cb5254f4a9172e89c148212e50f"],["/tags/bfs/index.html","6fd7916d2a588b3d7fd033333de8977b"],["/tags/dfs/index.html","1d209b9cdd3aba805f973959c9108090"],["/tags/folium/index.html","aef4fadabc158362076d441b8552ec36"],["/tags/git/index.html","da4a592afe39b6254f18e0c2a280587e"],["/tags/index.html","a7564070e3d5979fe531cabbc06d67f4"],["/tags/latex/index.html","acda67592e989cb5a6eb11fd0b2999e0"],["/tags/二分查找/index.html","95419d1c9fefbf622ba17d1272c795d2"],["/tags/优化类/index.html","284087ac3abb94fcdd702a852ea50043"],["/tags/前缀和与差分/index.html","4bbbfdeb0b59edca3f808236016efc20"],["/tags/动态规划/index.html","d9787a81d6ea9c858ff5fb7f0e960103"],["/tags/动态规划/page/2/index.html","1d9522fcfb77adf24843be10ee89856d"],["/tags/博客搭建/index.html","eb5bccec8530bf9c2f9ba46f5eff93a6"],["/tags/图论/index.html","fecd83c1589a23b3257f9326efdecbdd"],["/tags/大数据/index.html","664842d37a6552a058423baa0bede071"],["/tags/大数据/page/2/index.html","0c2a4f7defb97d7fa3c1358ea307e20b"],["/tags/操作系统/index.html","ced8932944419843fa9651a1ab1a21ed"],["/tags/数学建模/index.html","89c980efbf40fbd48f24a3a0053e3f5c"],["/tags/数据库/index.html","6f3af4ce17ccc8e3f8a31eb458d22cfe"],["/tags/数据结构和算法/index.html","82165d322cad927301e9cbbaa65fde32"],["/tags/数据结构和算法/page/2/index.html","73676de5a529061068f74851b52c725b"],["/tags/数据结构和算法/page/3/index.html","916d2e12f7c53954d3d3c5b7d38c161f"],["/tags/数组和字符串/index.html","88bcabf5a54d74b081a9d9895221b681"],["/tags/枚举类/index.html","61f5e770ca3a8a6fa57ffbcc515650a1"],["/tags/栈和队列/index.html","a40dd52083b326ecc9035b74cb3e3e0a"],["/tags/树论/index.html","2f7c26ed81e858ea61d54a89aa4d3682"],["/tags/测试/index.html","794d963c592909ac1dea588982d0f230"],["/tags/环境/index.html","dfddb61705153282ea23f9076452b9bc"],["/tags/环境变量/index.html","ec638af64634048da0e6ab5ff87c7fd1"],["/tags/绘图/index.html","76733561fdd0e77a26756462da9f0c44"],["/tags/编程环境/index.html","24ddedc47bb060dc854ecd2e2a5d3905"],["/tags/网络编程/index.html","8d4a70ec66a1cf5a4608f774855f68c4"],["/tags/英语语法/index.html","a3696ae22d53db9f062d109b72736649"],["/tags/论文/index.html","c191dfcfed3f24ee0b60df603ed29286"],["/tags/资源下载/index.html","5aa392c500a18a9d02aa144e2a166c13"],["/tags/链表/index.html","b8455338908d5cab5b093f4a290642c4"],["/tags/集合/index.html","e1ddbc8e80408f543d4e1506fbaa9e77"],["/tags/集群/index.html","3d2b533b5a22b79406f1f9a4bc0208a0"]];
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
