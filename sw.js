/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","14e537e8d93def544c1a10e2f24bec01"],["/about/index.html","599438f318f91646df1d53d78d6e68e8"],["/archives/2023/01/index.html","03e62b51433eb5986e52b59bf9ace915"],["/archives/2023/02/index.html","bbfff1896ecad2ebcc0550162019be55"],["/archives/2023/02/page/2/index.html","a3dea9524cc0089b58df4f2081ac046f"],["/archives/2023/03/index.html","827d216e2b56ec2b96d19f0253dbd59d"],["/archives/2023/05/index.html","7f7fa322c8087bf01d6fc93bd60787e5"],["/archives/2023/06/index.html","b2a981a5ea701fc9047146dbe3f3beff"],["/archives/2023/09/index.html","c65787fc11022f0dfa8d6c8fd5ef8d68"],["/archives/2023/11/index.html","7dd485e21799e293e4af95434ab64c47"],["/archives/2023/12/index.html","0b9380b579b3fb6094370098ac61e23c"],["/archives/2023/index.html","b633f501fd68bb6e2298e84a05985243"],["/archives/2023/page/2/index.html","ad541b34ae520e61ab0920ee61f85ed6"],["/archives/2023/page/3/index.html","bc81e0761591a876214fcbd0596fb93e"],["/archives/2023/page/4/index.html","12b72cf832db81db9136b4794c9ef9a6"],["/archives/2024/02/index.html","f670e4eb1959cb7277c75f49983bbbd4"],["/archives/2024/index.html","cfe490c58a9dbbe714e5d212a501011f"],["/archives/index.html","80f1087c784b601f591a15ebf240c720"],["/archives/page/2/index.html","379edf26f4eeea29c025d29db01b47cd"],["/archives/page/3/index.html","c2a11fd5835e4051235af1a1d57ad3a6"],["/archives/page/4/index.html","fcd55707aa47a8cc620c540b7578a9b2"],["/baidu_verify_codeva-qQP2iZOMLX.html","baa0148eab2548c67ed647733f1ebf7d"],["/categories/Java/index.html","69d9f2f26acf9c1067329b95b38c20eb"],["/categories/Java/后端/index.html","e79a4b8a25f52ece0c9a92ff592906cd"],["/categories/Java/基础/index.html","3ab6e3eb4340233ce5acd33cdb8ebc22"],["/categories/Java/基础/集合/index.html","582d7654fd52eec235272e7eacbff3a2"],["/categories/Python/index.html","7c4c7c585fed0c107564960db45d216e"],["/categories/Python/编程环境/index.html","14084f45a5d79446954e8d93fee001a7"],["/categories/R语言/index.html","1bed1f73ac8cd7878a65346b1bf53d43"],["/categories/R语言/编程环境/index.html","4f7a5e4f34c59e4faa4c6d2779abe160"],["/categories/iPad/index.html","2de0fb054faf54f65a1e2eb0616ce228"],["/categories/index.html","5bd9eb7bc5866e4bd570906a3c04663b"],["/categories/中间件/index.html","1edc1d9961b1c43a3b07b438cea2a3ae"],["/categories/前端/Vue/index.html","8250f6245d910baac0da98f8e8bbca74"],["/categories/前端/index.html","484fe3137e966de68371ff0b712b66f4"],["/categories/大数据开发/ElasticSearch/index.html","5beec0481ff583327d559f358c3339d3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","8b30f0481c42a8e5a8ad0dca47ede473"],["/categories/大数据开发/HBase/index.html","1c1ec0c9b25698ffd498aedefb5693c2"],["/categories/大数据开发/HBase/学习笔记/index.html","1d8c38a10569a49b3856ed56ca56e389"],["/categories/大数据开发/HBase/环境搭建/index.html","8abbbf069dfef3ad6192caef72dd4e0f"],["/categories/大数据开发/Hadoop/index.html","17bb81770fb31078424af144cab07aa1"],["/categories/大数据开发/Hadoop/技术/index.html","7c94b129b6082f2bb914073f2ca664fc"],["/categories/大数据开发/Hadoop/环境搭建/index.html","497e4e7f35f0bc4ec3fe3509e663fe66"],["/categories/大数据开发/Redis/index.html","b5b46b35ef2710a35712252f54742b61"],["/categories/大数据开发/Redis/技术/index.html","4aafc86b5b5844df32b13eacbbc867b6"],["/categories/大数据开发/Redis/环境搭建/index.html","7da115dac2caafc86e0abcba27031c10"],["/categories/大数据开发/Spark/index.html","3d151a81ceb84fc40c74437bf6948ab6"],["/categories/大数据开发/Spark/环境搭建/index.html","ce3bd2a218785eea1a1ce9c28c58780b"],["/categories/大数据开发/Zookeeper/index.html","9da99c93cd8c07346c18ae3258b9d007"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3982a03798f9422e9918fae8c1effe60"],["/categories/大数据开发/index.html","d5568b4482fe19767293e4b2d7831545"],["/categories/学校课程/index.html","47d9d71f88842390258acbb6b44b2550"],["/categories/学校课程/计算机操作系统/index.html","69fbd238a5730d5e60ec149d3ee798c8"],["/categories/操作系统/Linux/index.html","0d2b22cf6da1bb481e39021102853f62"],["/categories/操作系统/Mac/index.html","02424c891c57a9f652db4c7626b0e252"],["/categories/操作系统/Windows/index.html","fd91402330969ecbee5f040c71a4e9b2"],["/categories/操作系统/index.html","d7353e0ff5f9d84c4e39cfa1cdafa51f"],["/categories/数学建模/index.html","fb30423359658f6c99eaced0fd2c9518"],["/categories/数学建模/latex/index.html","536fc9409b39e51a4161c9e2f78cfce2"],["/categories/数学建模/优化类/index.html","9ad0adf744a305718816e9082a2f8d4b"],["/categories/数学建模/优化类/现代优化算法/index.html","b08d1b93ee1324c12b8037d447353f4c"],["/categories/数学建模/优化类/规划类/index.html","34e1db212b482676fa35c7ec6d694175"],["/categories/数学建模/绘图/index.html","0b3a8874e39c69bbbdabf93ad6101079"],["/categories/数据库/MySQL/index.html","70870039dfbfd9e5a219e04033e960ce"],["/categories/数据库/index.html","649e940256bb68970ca312a1a3f3a8f4"],["/categories/数据结构和算法/index.html","335b2cc3ca8a80878c0eb6e0c0dcdffe"],["/categories/数据结构和算法/page/2/index.html","38cbba938291e4c011eec20e019ec893"],["/categories/数据结构和算法/基本原理/bfs/index.html","cc3e688ba974f0526a8655f59d706583"],["/categories/数据结构和算法/基本原理/dfs/index.html","29c5c6997b380d81c45f4480c66eccc9"],["/categories/数据结构和算法/基本原理/index.html","542d8a2842797c0b153e04f310a8eebb"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4279364b77449e57177018edc69fd354"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","9770cfcca7b826aed6cdb5f865713a64"],["/categories/数据结构和算法/基本原理/图论/index.html","3f0a3264ff441e411aa496a1b3309f55"],["/categories/数据结构和算法/基本原理/字符串/index.html","4c1eff8653a967cd5d720cdc896dafb3"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2513fe39c6b1fc40f17e9d6ecbd4eedf"],["/categories/数据结构和算法/基本原理/数论/index.html","6d795e0482918a0db6fb3c021a2c1771"],["/categories/数据结构和算法/基本原理/树论/index.html","4931aa028117a00c5983288ac6ba62ba"],["/categories/数据结构和算法/基本原理/链表/index.html","8ccc3a4f9f30adcd61b9fb10501bd1b0"],["/categories/数据结构和算法/算法题/index.html","790f4adbf7ddcf53bc094d603e9138bb"],["/categories/数据结构和算法/算法题/二分查找/index.html","3b4cf2ba6c6fb0d37f6d626b70550c06"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","51b0638301f710a72f6a597f1bff986e"],["/categories/数据结构和算法/算法题/动态规划/index.html","8ad80087a450eaac4005863365a32694"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","bff64cca9646f4917a7eaabbe396f67e"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","159fbecc8ff120da0a5bc3d9b2c51393"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4991ad835385c61bfdcff1ce2d305339"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c001aa30e2aef3451d6ac522e14493d2"],["/categories/数据结构和算法/算法题/数论/index.html","f993f7682cf608f26eec63b44685b2ca"],["/categories/数据结构和算法/算法题/栈和队列/index.html","11fa63d86d18b03ffc7cb7529bd84286"],["/categories/数据结构和算法/算法题/树论/index.html","f53ac42433b792b12d4be8ed6ec77a97"],["/categories/杂七杂八/index.html","412a216310f4379dfb41634dc0389dad"],["/categories/杂七杂八/博客搭建/index.html","c5aa954b1224a8941cd18139b9822af8"],["/categories/编程工具下载/index.html","57606c3bc4bdf17c1a85c6a8ca564ddc"],["/categories/编程环境/index.html","d4442157f27560b81e1b91f0fea30fec"],["/categories/编程环境/大数据/index.html","ecf2232720735a4c50eaf7a5bdd8a48b"],["/categories/英语学习/index.html","0c97d495e102436c737dbb1d32f96fff"],["/categories/英语学习/英语语法/index.html","07af78c16ad70a7ef27c3819aa3363d9"],["/comments/index.html","4f3247922814476b06b2566c13b64190"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e7ea92b321d54bdb6787a3d742390339"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","bc105b81906d2d1dfe7d47da80a92edf"],["/movies/index.html","166b608a043a98011c5bc60c2de801cc"],["/music/index.html","df5664d009e91de7524bf1128b66939b"],["/page/2/index.html","110c38c85c4342636d385ab6ca1f0b00"],["/page/3/index.html","3c9012e61356a8eda2dfa708a670bc78"],["/page/4/index.html","57caecccd68f95652c6bbe2c3906025b"],["/page/5/index.html","36cb87d896cfd867ba54d968717ab4c9"],["/page/6/index.html","0fbd909c7c4ed83b8b12d29a964a8391"],["/posts/1021360842.html","6b93d98ecb9993ce5aeac32cf4b5647c"],["/posts/1120620192.html","3d169552bb26f473c4e4298f00771b76"],["/posts/1141628095.html","d8c1f055ae3600f87169ac0657278557"],["/posts/1168613674.html","f2218a99d834cc12258c4defcce98bb3"],["/posts/1219920510.html","d7225efa2beb5e192e8e9f455cb00c9c"],["/posts/1222166338.html","f78324a699e08e969e2af6f78c4bd65a"],["/posts/1259097482.html","46bf85bad4c673d910842b87a60dbd2e"],["/posts/1271036369.html","3985b8ab92eb42484ea49212873c6f06"],["/posts/1312847445.html","86daf88cf19fc0f6f99ff10a4f4f36ce"],["/posts/135355774.html","2f0c0dd5b914a6d5b7f597da9ea8855e"],["/posts/1375344716.html","a5ccb18716f5404ceeec05bf4203a052"],["/posts/1388991698.html","9d2d41ed1457137fb4374af36ea4b78e"],["/posts/1410315814.html","6771b13450827cbd50e02fc37ab67e63"],["/posts/1452790229.html","4d93d62ec299e0c93d0118cc88317c9e"],["/posts/1470079884.html","0c67cf518352a2612563de2853849dd9"],["/posts/1470079885.html","3ca4618dfc50a1a034a44ca2d128f624"],["/posts/1470079886.html","c5733129abb71914226f8ffb2f71c9af"],["/posts/1470079887.html","c78bccc16921edd6cc6668f3084242fa"],["/posts/1498536549.html","6bd71c1cd31f55c36963c55ff0d3ad8d"],["/posts/1539568593.html","d5ed6a3a5dd6ed99e0effb893be6f956"],["/posts/1547067935.html","92dcd6a0cd40bc63e7f20fbf52988556"],["/posts/1557866301.html","ab658fd920fc8f7a409ee8e54dcb4688"],["/posts/1571776361.html","6c616dceadc44c526d534694b0bfa597"],["/posts/1605124548.html","cf85db44ca49e64b214b88c741099606"],["/posts/1633036852.html","719a0c19cf14e16eac03fe52960a5de7"],["/posts/1674202625.html","988b19030d1c5e6cb67b4e1926616129"],["/posts/1765123828.html","55b5283d71f4ed8c58780508e0fa5e68"],["/posts/1767336200.html","0ca7f93f59f2ce1e20602610d2052a03"],["/posts/1776114197.html","224cd9628e095970cb1bab96f170e335"],["/posts/1817748743.html","83633b8c12e43968403c8ab61786cf50"],["/posts/1925125395.html","d28a94adce953db5984a036e8437382c"],["/posts/1966191251.html","5911ccf623e7cbec2e1183e8dda7d5f8"],["/posts/1987617322.html","2f9b67b8efd7953d34dd6975c59c5ca9"],["/posts/1999788039.html","423a588a5becc87c96e96d1b0a19121f"],["/posts/2075104059.html","ebbc2d78e45e7b5f24fd7a271536b015"],["/posts/2087796737.html","36cf0f829035b9089003dd76ae8b9506"],["/posts/2106547339.html","8159df59aa626c6765eafe754e8c3d07"],["/posts/2207806286.html","1077300be3162921ad2ded886ea7a7c1"],["/posts/2225903441.html","1547323fae317a752bf289b9c3da5e84"],["/posts/2265610284.html","89e814899c617933b539a0f94ab518ed"],["/posts/2281352001.html","9409274eed897636785660ec8a070f9a"],["/posts/2364755265.html","2f71c853f8dabcbf4360a03de5ff876d"],["/posts/2414116852.html","ea2e83747a579a10fd1c55c67b659af3"],["/posts/2421785022.html","02f0124384d285c62de2f7009617da59"],["/posts/2482902029.html","f42b15f77f64881958c2dde88f1c8b79"],["/posts/2495386210.html","4c90d67cfbf304c106ade0eb0a5ac19c"],["/posts/2516528882.html","a5ef29efd4c27318504210f0c0ca4c00"],["/posts/2526659543.html","2baf49a1fdc88bf4b3733059291f5a1f"],["/posts/2529807823.html","4258abeb12ecc48ccd26ae1c17487f16"],["/posts/2596601004.html","05fe0f134ed99166e5dab6c7bdf7c0b6"],["/posts/2697614349.html","11d1729fc238e64c96e43d583bac040b"],["/posts/2742438348.html","5665481a026765ea35e294ab9d235f4f"],["/posts/2768249503.html","293e624114b5bde1e41e2305a2048828"],["/posts/2864584994.html","5f25b0363be165ff20eec6fb1e7a0e5f"],["/posts/2888309600.html","8c5e9213c00a84c12641cc01e38bb854"],["/posts/2891591958.html","063e20add63d5f234c3c0b1aa3b61968"],["/posts/2909934084.html","eb7f7e47d33130ce758927d0433e5a70"],["/posts/2920256992.html","4d7cba041d330282c6b7dc1810c53e24"],["/posts/2959474469.html","2c0e05423d0b9afe3445cfd60ed8c372"],["/posts/3005926051.html","22de1011b6f64152fdc2e78ee9a0a201"],["/posts/309775400.html","65936c05e5f3eb3a91b2065205e76bb0"],["/posts/3156194925.html","8f949f95eba2c3b29e8e77cdee4576b0"],["/posts/3169224211.html","f8025088de95fe23958b2c95996f183b"],["/posts/3213899550.html","9cad4446089fc1722c9dfc775a361443"],["/posts/3259212833.html","f96a9c6d3d3162cf62972fa2d6977d8c"],["/posts/3266130344.html","8c71cc0131104f6afdeb092c2e8f3fb8"],["/posts/3292663995.html","9f18a16bab3937bb61acafa62145cf8e"],["/posts/3297135020.html","ed827c6477af37632ac5f654d8832e49"],["/posts/3306641566.html","7891b070f10c54522a855cf1cf4f7e96"],["/posts/3312011324.html","0db5ec98c218d9943c439d9ec0921846"],["/posts/336911618.html","a79000c0500bf77dfa328ebce055807e"],["/posts/3402121571.html","c9e801f065dcd0366d0d98428cd12187"],["/posts/3405577485.html","ad53e9481f9c37c785862a0ca7ab6768"],["/posts/3498516849.html","cbe653ea66567e6a16e6075c6cb766e7"],["/posts/3513711414.html","7888c7a512a57bfdfb790b66a72f7789"],["/posts/3523095624.html","3a9cce0b9ab71a97ab816e55b11236ea"],["/posts/3546711884.html","ce968df4e68f221e39f6c906a6073d51"],["/posts/3731385230.html","e9941a5a3a9b86530d81a8e1fc227361"],["/posts/3772089482.html","779b4d9b6cd83337292828ed83e4e9a3"],["/posts/386609427.html","a24dcc746f9600970d2a680619e93d95"],["/posts/4044235327.html","405cd4ecddd454ad7d5b5c42522aa779"],["/posts/4115971639.html","33a5c192b6d67ac4bd242208af1beec0"],["/posts/4130790367.html","966be0f919b2bf755d1ad400fadb1421"],["/posts/4131986683.html","777ae5b3f0ececc4492ce587d1a05a78"],["/posts/4177218757.html","1792d0b56a835ba79e4f8b3e96eab898"],["/posts/4192183953.html","3982065edfcb83cdcc4dc273537599e4"],["/posts/4223662913.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4261103898.html","0f15477cbc209e4f94440db4b0472b77"],["/posts/469711973.html","ef955745877de7eee5961fd633f951b3"],["/posts/482495853.html","f3e78f36b6223bdebe4e05fa6f932094"],["/posts/488247922.html","fff7339b036fa0fe4a16347386684a7c"],["/posts/517302816.html","f788e574c0c189289c3d105e6ad4f311"],["/posts/570165348.html","531ad2bab37e4c650da5417644f285bc"],["/posts/595890772.html","48a5710c68b2e75403a7f97e2405b6c3"],["/posts/67485572.html","022a4e755d28ddcb263af062e8f7ac74"],["/posts/694347442.html","042b6ea70c1ddd8ceff2912fe7aa02f9"],["/posts/707384687.html","37158971530ef51cb922e47198a4a928"],["/posts/71180092.html","c217f44423636f96d4d7e79b36ff8463"],["/posts/716459272.html","abf1247e2c43e9a34f9caa10c3341634"],["/posts/765481613.html","0426a376cd5bce458737c9993ec92256"],["/posts/778231993.html","49bf4d8b5729fc4574f25f1fbf169457"],["/posts/795397410.html","cc25cdd361d5948f64db3392ed89387a"],["/posts/820223701.html","679b9583790de7924df9959724b6735a"],["/posts/830372185.html","ad0dc790bd8bbb3a2cfb800a0d59728e"],["/posts/88294277.html","0c1a8abb48c885b0e8f979065f6d8cef"],["/posts/939963535.html","0fd604f043bbd8110a3326fd342dd5c1"],["/posts/983786067.html","05559f31d1aa096f0d36b876979c1dea"],["/sw-register.js","969780bf76b0aecfe46ac7dde53e6c02"],["/tags/C/index.html","ed7a9c53fda40bb152ee9f7b927b9fd3"],["/tags/C/page/2/index.html","1f6bf00c8f1e4c9922b071a058df8e2d"],["/tags/C/page/3/index.html","fa329a9ebd2b38e7efc6efbf1dec4007"],["/tags/C/page/4/index.html","5df6b22bc7013f4fb98786b104767949"],["/tags/ETL/index.html","f5e7b4bc5ae0371325555d04a33940e7"],["/tags/ElasticSearch/index.html","1928600265a6d3ea014bd0ebd0eff237"],["/tags/GUI/index.html","c8480174eaf1ffa8dde73b3d2dee48a8"],["/tags/HBase/index.html","b5e06571b5eae63c31f5b0491ca2532b"],["/tags/Hadoop/index.html","0484512e06708d700e0878cc786820cf"],["/tags/Hadoop/page/2/index.html","884f3fa96ef237ebc3c5dfc90648a14e"],["/tags/Java/index.html","457f82d8c143e7a30de1c7a7482a665c"],["/tags/Java后端/index.html","7cb92f639fbb7babb6c46da6bb0b172c"],["/tags/Java后端/page/2/index.html","f5e7565c7aae7610f903131e6ba104d5"],["/tags/Java基础/index.html","1134ae2029db04b20f8fe2866b881cbb"],["/tags/Java基础/page/2/index.html","d09f35519fa3395320ea9811725641fe"],["/tags/Kettle/index.html","95b815bfd6bab3b2d68bca313041e18d"],["/tags/Kibana/index.html","da70aadcefb296bfb5f905de604b2986"],["/tags/Linux/index.html","774d7839a3807453a76113038c753a78"],["/tags/Linux/page/2/index.html","b57dfbadd5cad19765238db5f7ff7c48"],["/tags/Linux/page/3/index.html","adc563c8106c4a00082b4527ebd3ec18"],["/tags/Mac/index.html","896c453a03f06f1310cf95d6b3ece4e5"],["/tags/Mac/page/2/index.html","04b02c023568877cdf40d79e97bb6fda"],["/tags/Maven/index.html","f551b76d7fd9b76b0d5735bc1e85f778"],["/tags/MySQL/index.html","e4daa4cc9ddf1efd66d2177d435303dd"],["/tags/Python/index.html","230c5850c1d5560ae46cff841d29e451"],["/tags/Redis/index.html","33c62233cb3066b128dd2d534939882e"],["/tags/R语言/index.html","f354dc0ed778109e08c47d21f0444c53"],["/tags/Spark/index.html","b672239ce8b85257cfe33b156069c038"],["/tags/Ubuntu/index.html","9c9336f096ffe60be1d7d2a12c66da63"],["/tags/Vue/index.html","5ac1703dbd1f795b65f75ac26fa73afa"],["/tags/Windows/index.html","60e4edb899a458377c43508c50d93fac"],["/tags/ZooKeeper/index.html","bed692bf960700db779c7a5bcb0c44f8"],["/tags/bfs/index.html","744007f0f687e4b3ca383abcad50fc0f"],["/tags/dfs/index.html","9a7175117a8644fe12579f566c675678"],["/tags/folium/index.html","ef6a565d2c905d765d58f6c83a636e0c"],["/tags/git/index.html","f223a0484be36bfe37eb8965f7e7c49b"],["/tags/iPad找电子书/index.html","ccc71830a027814b5eda2ee04d2b004e"],["/tags/index.html","33daf8cd40103d43580ada84988d3627"],["/tags/latex/index.html","f11217a3a1494437c9b1be5802f01e3d"],["/tags/中间件/index.html","acc6e303b30e5b9d096df46358a1d8a8"],["/tags/二分查找/index.html","5c31f419c0e12b40d03d8b042e819133"],["/tags/优化类/index.html","1eaaf474d896a8268ee988077ca55e76"],["/tags/前端/index.html","c106f9068ea8b5e1b7a92c9be8b1574a"],["/tags/前缀和与差分/index.html","3e6a1a5a3aa73f671f2aac9f5d676f72"],["/tags/动态规划/index.html","932ed478a105610cd7f93ff6789b8938"],["/tags/动态规划/page/2/index.html","7d07de80242452c0fdc47f8f21baa7f9"],["/tags/博客搭建/index.html","6e0d47d17a4bd975fdf9e1085e40a79f"],["/tags/图论/index.html","0b25584e5ff29738f2fe361a13804754"],["/tags/大数据/index.html","bb95e59244fddeb3f836427030bf2912"],["/tags/大数据/page/2/index.html","584c17015919e2be58038cfe8030acc9"],["/tags/操作系统/index.html","f980d467c9c181fe3a69b828b5505f13"],["/tags/数学建模/index.html","297a27f59359d5b963d47fd370ae4d3c"],["/tags/数据库/index.html","b598fd4003439786c8925a0c045d196b"],["/tags/数据结构和算法/index.html","710e2f0796f45784064bdf23adb4219a"],["/tags/数据结构和算法/page/2/index.html","59f8a1f32c1d69760e0903e7822bffe4"],["/tags/数据结构和算法/page/3/index.html","ca8afaaa3a4c32505cce7250701aa859"],["/tags/数据结构和算法/page/4/index.html","44c8ed1f592a6ad263f8778537401261"],["/tags/数组和字符串/index.html","0901c9ac47270abbdc089afb4a881bda"],["/tags/数论/index.html","34f28f9833321a8d64c01179da3039a9"],["/tags/枚举类/index.html","590d7c8c0b1f9207e3c133b0e65168b9"],["/tags/栈和队列/index.html","796156eb7c510b7ecb18bca8a04e381b"],["/tags/树论/index.html","68a022cad05d7e5621c4f11b78e907b5"],["/tags/测试/index.html","21f2b6e86695cc4d264399280f811ce1"],["/tags/环境/index.html","d4f7af064c8d1cb4497c697fa506baa5"],["/tags/环境变量/index.html","38cc5c30ac933e47431fe8187f1a9afe"],["/tags/绘图/index.html","a1f0f0ce3069ea4c2808d1cb3d321db9"],["/tags/编程工具/index.html","acb13c71476e8d38543ba1f93f3a2cb2"],["/tags/编程环境/index.html","24c8ecd1cce59b419511698de4d398a1"],["/tags/网络编程/index.html","9f7334aadc8db7c40dc223289ef8d594"],["/tags/英语语法/index.html","6c6c9ab15a5881ef7bae76ee1ef9dfb1"],["/tags/计算机操作系统/index.html","b88d84784fa034ff91cf687419c707b1"],["/tags/论文/index.html","1f6c4330cdeb89c5e8ff4335d07c131f"],["/tags/资源下载/index.html","bb76cc463864f95aab663b6297281209"],["/tags/链表/index.html","9c825ff567bfeff4b2e8d2d300d4f2c7"],["/tags/集合/index.html","a438fd193d9b9e9c0f56b100f2b316ee"],["/tags/集群/index.html","2c7976e20edf50d8350c789ba812b105"]];
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
