/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ae84c96ba18788c10861f7b1c1ce3085"],["/about/index.html","96dbe576739943d47b0f2d9e2c27fc11"],["/archives/2023/01/index.html","835e88b02c0b2aad1c35082755bee29f"],["/archives/2023/02/index.html","451c79ed5e6694bd48dab75b3fab253e"],["/archives/2023/02/page/2/index.html","b51fd77a9f7a4190d7b09ed743dfb933"],["/archives/2023/03/index.html","17033400c678d0b738fcdf27637869b9"],["/archives/2023/05/index.html","a32a43de974365b5c46e446600f72beb"],["/archives/2023/06/index.html","faa00f6c28cca940c9e8f0815ebb4808"],["/archives/2023/09/index.html","c6ae1a9f85af04bf7c03193322e8d1bc"],["/archives/2023/11/index.html","ba4f9065f2785dfc12e2cc6fb8f10c5c"],["/archives/2023/12/index.html","bf4217517f4ba822ab9163abfa12b6be"],["/archives/2023/index.html","7dff71153abc7a62488f2b91b49f9e76"],["/archives/2023/page/2/index.html","9b7528c605e55421b6821d1c1f0046f6"],["/archives/2023/page/3/index.html","66c4050951d13a3df638efe0776a3a78"],["/archives/2023/page/4/index.html","d36b313897c96c06ed047f3a2b5f2390"],["/archives/2024/02/index.html","9507db82e8557a9ec530f3ec69b6e1fe"],["/archives/2024/index.html","e8c9e9507cc7d2c07893e4dcfa8a3c5c"],["/archives/index.html","02ca9180b238f7d9e879c1b30382dec4"],["/archives/page/2/index.html","7040831f9e1b1f96390ef547f41d8513"],["/archives/page/3/index.html","0d2093277581a89f776a2736ce1ffeb3"],["/archives/page/4/index.html","0bd13e8ad75974cee27e78638a54bac9"],["/baidu_verify_codeva-qQP2iZOMLX.html","191504334e1fda9752b48ccb2d37c37c"],["/categories/Java/index.html","1c99bf38e59a85e669c4cf440dea0d17"],["/categories/Java/后端/index.html","e8d27e36dcd038295e2af3d1f3d0e1ea"],["/categories/Java/基础/index.html","ab4c590f8fc920aa9ce889d5024dad03"],["/categories/Java/基础/集合/index.html","b3947274741b2f228426f51baa3627a3"],["/categories/Python/index.html","9177b3452a41c4da1a93902adb00d860"],["/categories/Python/编程环境/index.html","c32ee503c421cdb4b338cbcabad816e3"],["/categories/R语言/index.html","ddef3d662493051e0b394b6b86c87666"],["/categories/R语言/编程环境/index.html","1493041d02392bd2277b9896cbf9217c"],["/categories/index.html","5c1a119a5f0c3ef050ff33519fd4311f"],["/categories/中间件/index.html","25dc8eac4c57f6f6682d6231bce5e14a"],["/categories/前端/Vue/index.html","435fb057cb2c0ff54fc07f2e1e8b1287"],["/categories/前端/index.html","530e6a39afb0979ce08355abbe29a167"],["/categories/大数据开发/ElasticSearch/index.html","b2a4b04254b3b093944187ab080cc410"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","5dd0f76cf3415b86ebb2983ba3adf0b8"],["/categories/大数据开发/HBase/index.html","4636821fb55ae9cffe755e454f2d1233"],["/categories/大数据开发/HBase/学习笔记/index.html","c75f586cfcfe9b32db539341eb265104"],["/categories/大数据开发/HBase/环境搭建/index.html","70668926b3ed00ee61bed08958ec0ae0"],["/categories/大数据开发/Hadoop/index.html","c93425280222922948800f53ce6e1680"],["/categories/大数据开发/Hadoop/技术/index.html","b7105e6929fdd4a769aaf006b3188db4"],["/categories/大数据开发/Hadoop/环境搭建/index.html","dd618fe5bb9677600bd4af2f1ffafbd4"],["/categories/大数据开发/Redis/index.html","2ea93fb127adc16c588c8e3aa2f3036b"],["/categories/大数据开发/Redis/技术/index.html","e3488cfbb8f6094d5540512d10b7ada8"],["/categories/大数据开发/Redis/环境搭建/index.html","8038be0af838e767c05ea45a66000a86"],["/categories/大数据开发/Spark/index.html","10a64a22f3477a3be796a2e3b9c643a5"],["/categories/大数据开发/Spark/环境搭建/index.html","3ca1fe86c7d53919c0a924f1c273a5d0"],["/categories/大数据开发/Zookeeper/index.html","5178845f6740847c038ae8576a8e6529"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","5738b51e2f73cd6ea020607471b8c0a0"],["/categories/大数据开发/index.html","511f8533840a012f511b0bb96a8c3171"],["/categories/学校课程/index.html","bebe44d7292bed0306d61b6a3b366213"],["/categories/学校课程/计算机操作系统/index.html","e6142aadb308d2be78384520c24519be"],["/categories/操作系统/Linux/index.html","99a180924c04a9a9dd25405a20b8f0c3"],["/categories/操作系统/Mac/index.html","ced405ae792fa001450b90f68ca8b307"],["/categories/操作系统/Windows/index.html","ce016baf10609e8e8f9e92cd58d66022"],["/categories/操作系统/index.html","bc36e88ceb5678ea71b8506fe45549b9"],["/categories/数学建模/index.html","799aaa06717dbddb553480fd350b27d5"],["/categories/数学建模/latex/index.html","d5ce26a00d1aaa29b23d819a64404d4b"],["/categories/数学建模/优化类/index.html","06de3609ab9c2ea26f1bdb151e4b2793"],["/categories/数学建模/优化类/现代优化算法/index.html","73a16538e58e8c7c50d8f98062c472ad"],["/categories/数学建模/优化类/规划类/index.html","a46e940c79c445ef29bd3c7f28ecbcf2"],["/categories/数学建模/绘图/index.html","66fd0b9db1621c18600acb887fbf3383"],["/categories/数据库/MySQL/index.html","0f79ae7d28051a718def6b5f348fbd0d"],["/categories/数据库/index.html","f12e6a51e7b12dac14847ebe3cf7d8eb"],["/categories/数据结构和算法/index.html","7002b8790a104f673baf101bb1a7ba05"],["/categories/数据结构和算法/page/2/index.html","16f7e2edf5bdd90adfcfcc53329e4e7f"],["/categories/数据结构和算法/基本原理/bfs/index.html","f2001ecc906ad0c42e96ccfa249333e9"],["/categories/数据结构和算法/基本原理/dfs/index.html","6a26a0705e52434687857cff70f5ca71"],["/categories/数据结构和算法/基本原理/index.html","73327ff250d240e9279c2710df5dbe39"],["/categories/数据结构和算法/基本原理/动态规划/index.html","4fc85ed4509301a7b261d4677f3aa801"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","89555f438c0ea9fd94a6e109b06cd0c5"],["/categories/数据结构和算法/基本原理/图论/index.html","411a67b17cadcb3297d8bfd244bcc7f1"],["/categories/数据结构和算法/基本原理/字符串/index.html","9377892310e62cdd961954167650b5ba"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","7b568b1c1303555a17984c2cbcf3241c"],["/categories/数据结构和算法/基本原理/数论/index.html","f4988dacc450c2c83446e931526420bd"],["/categories/数据结构和算法/基本原理/树论/index.html","b5bd1b09a01321bb3f85bbc29eabdd29"],["/categories/数据结构和算法/基本原理/链表/index.html","79b3449fdea757d3916d72893a94f608"],["/categories/数据结构和算法/算法题/index.html","103686364b49779efa5abc58bd0a7c5b"],["/categories/数据结构和算法/算法题/二分查找/index.html","94d9b8945ee26b02929530a287dd6cfc"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","49d53f3723eb5b3e096a51672068f943"],["/categories/数据结构和算法/算法题/动态规划/index.html","b54b4bf60e2848d65d1005de217ada29"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2f5b8f06f7d427538c67b4b209753466"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f1a6d1df9ea4a1ff87f5fcd0232707e9"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c794ada92e4d482ac402bc4c167cbaf7"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","3ca57e7f5ad681a1db3f0eae8ae1bb27"],["/categories/数据结构和算法/算法题/数论/index.html","3cd67c881bb1d9a426507fc1fdd3d1d9"],["/categories/数据结构和算法/算法题/栈和队列/index.html","3f3e560d8b73be8964861d27984f548b"],["/categories/数据结构和算法/算法题/树论/index.html","8c2ce115d4370ad1034ec5c070c0e831"],["/categories/杂七杂八/index.html","10d12c5d8db921f26df68a61f6c79b34"],["/categories/杂七杂八/博客搭建/index.html","8196ba33228c711ff95d5e09343b3590"],["/categories/编程工具下载/index.html","c434b69beb8cc7904de2eff2b02eb5ce"],["/categories/编程环境/index.html","666a338f4580e1b0cd0bac43e49f6f79"],["/categories/编程环境/大数据/index.html","33862b0a0119ac8eea7badcb7fc79c5c"],["/categories/英语学习/index.html","ae8b53b0567e47e4f3867cc27be9dc66"],["/categories/英语学习/英语语法/index.html","82a7adc9a12fc5da98dfa5f1fbd311e2"],["/comments/index.html","679cb04e58ea6aca11f875d60fa12801"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","49f75a3c26975625632a1a8def4008bb"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","5f16fa46238802c20d7fc97cd1a798df"],["/movies/index.html","3466d379fce05fc4190773b8299dadb0"],["/music/index.html","1a8c9b9c619f3713b93e45a7941dbcc2"],["/page/2/index.html","b19f9b9b978fe215ca8493fb0aa83d0c"],["/page/3/index.html","33f4ef58b270d397246564370640521b"],["/page/4/index.html","98d9164fc76c3f2503be5a1815688389"],["/page/5/index.html","7da7157b56163bd80887e8f3d02e9a95"],["/page/6/index.html","a5f531d7271e8a9abda5aaa4ddff6be6"],["/posts/1021360842.html","c42a56824376309c50070d190b4195c1"],["/posts/1120620192.html","b4deb92e384974ff5d6971ae0af9856e"],["/posts/1141628095.html","c259e8501a9eeff60a997faa42e3dcb1"],["/posts/1168613674.html","8a70522dd3be2f14534b16d049074a6e"],["/posts/1219920510.html","841c01c1a2a213b075e32e24f71c4e3c"],["/posts/1222166338.html","9175750535954e025481dcde14aed74a"],["/posts/1259097482.html","812aba25f0553e555b81cb2dc2bfb9d6"],["/posts/1271036369.html","a6ad1ef892ee87284fcadd097433c811"],["/posts/1312847445.html","918b99a43e5474fb29c992cd2784a6e2"],["/posts/135355774.html","d709a42b26d715ca90ab5753d462820b"],["/posts/1375344716.html","a444160dabf4edaeefcdc8496073b2a8"],["/posts/1388991698.html","dc8c8e1f3811ea7bde7631d811c7f7b2"],["/posts/1410315814.html","f53d01de91db7e6512d13e00dbef4464"],["/posts/1452790229.html","9b9257e6bc19f98f719b6f2ee2c8603c"],["/posts/1470079884.html","91e71fdb4d4ccfa51c2a9c2c1c8d5efc"],["/posts/1470079885.html","91f65f055d2490a1b57510918ce25737"],["/posts/1470079886.html","12e8ec9cf952213a6ab2ec9b84086c68"],["/posts/1470079887.html","81cca2e168098271fa21a2e5531039ca"],["/posts/1498536549.html","a0e8b0c5c21c9572db0f0eb772c3b959"],["/posts/1539568593.html","cc48fc088d98d04ef7e33c72794e51cf"],["/posts/1547067935.html","f694b62c72c2a2cc02f55faa1bb9381a"],["/posts/1557866301.html","cbb79ca7e6baee59ac6d09eb0f786c25"],["/posts/1571776361.html","999842cb2e7fa50a0cf3f5b2a6bcacb1"],["/posts/1605124548.html","60558312c6e01079dca3448f3559db18"],["/posts/1633036852.html","c652b71d45d2362f58dafe841d84b820"],["/posts/1674202625.html","ac06a1431ffec9e2b1ac8d838c84ca21"],["/posts/1765123828.html","5c319d5eca93f0e65a9f55ea7a58777f"],["/posts/1767336200.html","8bf01554195acc92c94b399b49bef220"],["/posts/1776114197.html","57daf17b5d8af39e27c537c1c76c91ec"],["/posts/1817748743.html","e69dde11923b548a5c395a2c0799a319"],["/posts/1925125395.html","d35ccc8bdb513a2ab060a1e7ffa4ace5"],["/posts/1966191251.html","e9833b3da7b0cdf7824b30c1ac0f8c88"],["/posts/1987617322.html","cbc518b10b49697ae9d3ebcb3abb0f72"],["/posts/1999788039.html","91e8d9d40bea420b327f0c6479364b03"],["/posts/2075104059.html","915da9e55d58ee49c12b2bc1162d3d38"],["/posts/2087796737.html","7ce460c15ba7cd9c9d0ecd87e3115b5d"],["/posts/2106547339.html","6505c649700500aeec49ca4fcb2a3c57"],["/posts/2207806286.html","7eee8149b072c432ee33eeee3c3ea1b7"],["/posts/2225903441.html","5ccc8c341101f67bd69dbd4a320b0700"],["/posts/2265610284.html","171bc5de7145c530079b248b3947934a"],["/posts/2281352001.html","50300e5fad7435e9b8115bb9be4cf23a"],["/posts/2364755265.html","1cc8bacbf2e6522d5adc10ac940989c1"],["/posts/2414116852.html","5bc7c29ea3537dfa729f78d832de35a3"],["/posts/2421785022.html","c5b1d7bbb76947bbde46215adef88f29"],["/posts/2482902029.html","3354fd0c90f5e0fe2483bc9e61029e60"],["/posts/2495386210.html","9964a7989b17fce3dfcec56a98132620"],["/posts/2516528882.html","87a78e9888c5c6203daae1f7038f7bfe"],["/posts/2526659543.html","ade8f6045df7685b2d72698fcd536816"],["/posts/2529807823.html","43af7c11093e6c8ae0d8f83b6a53ed65"],["/posts/2596601004.html","40e6fbafa7891146ace10345fcbaacc4"],["/posts/2697614349.html","13a22119ae6411ec7cdaf1defdd0d1d5"],["/posts/2742438348.html","e5784feab4a0774d2e1d3d5c76495845"],["/posts/2768249503.html","44ad0712ce4a6c06a8eb5b3674bb3d4d"],["/posts/2864584994.html","10e07dad247b8f1b3161c7d63b2cdada"],["/posts/2888309600.html","62c6724f7aa5071868a346b5c6063458"],["/posts/2891591958.html","a249461beba3b8b530f54f9e37311277"],["/posts/2909934084.html","c34c2bed6ea2f1e0d25bc3776fe7f155"],["/posts/2920256992.html","e2a7f8cd2beab13372cf8e5a3deec6a0"],["/posts/2959474469.html","2cbfb6869980e7e63024a29e58bf31c7"],["/posts/3005926051.html","e7db6ac562c16722b417b6c325598576"],["/posts/309775400.html","e0f6f0d27cef67cb0f9b23044a31123c"],["/posts/3156194925.html","cfa1acefcb99712435ef50f6c4a82658"],["/posts/3169224211.html","f5c416fbc633d3fe76c2907a50d173e6"],["/posts/3213899550.html","0c2ed1b2431f293ecfbb885bdcf411f9"],["/posts/3259212833.html","296b4a120b8b705f2ec9c39f52f31449"],["/posts/3266130344.html","3bbdac181a5d68519dbdc878b6ff1540"],["/posts/3292663995.html","247949dba39272f334de3ef46c17544b"],["/posts/3297135020.html","d0a88f7cbe4a210793dcd952caf7cf61"],["/posts/3306641566.html","9ead0710cf039e77ec6ec87d8ddd2cef"],["/posts/3312011324.html","70580d4b842a8f78953aad334ad5781f"],["/posts/336911618.html","07fca4bd32c67596aea5c725cc135dce"],["/posts/3402121571.html","72b2060e4fc1e958eac985f1329a1339"],["/posts/3405577485.html","97f64985316ebab739697c9dae4b8dc5"],["/posts/3498516849.html","4ed26c26aac5356005bb66702337e64c"],["/posts/3513711414.html","aaaec1178c3dda3e315ca4ba94fac10a"],["/posts/3523095624.html","c15866084ae065f2138c557af16b5881"],["/posts/3546711884.html","6fb8da85607e28b9ee1943194025d616"],["/posts/3731385230.html","117990282783e1ece699a81fa7b6a150"],["/posts/3772089482.html","5fa23ab1ebe1caf46fed6b3ee806489b"],["/posts/386609427.html","0623858a475adfa744b928336ae0f318"],["/posts/4044235327.html","7f09d166587afa906f065a1d7791057f"],["/posts/4115971639.html","fa6c62579a3ef7eb275f113518c1efbf"],["/posts/4130790367.html","39dccf8deb2d1435a5614b7e9ca91c06"],["/posts/4131986683.html","db3eec4f286f3b58a024769986bc567b"],["/posts/4177218757.html","7ee449deb982673d5d7b56a290bb7cf7"],["/posts/4192183953.html","ae17bf511dd20eb7e0ecf4c1df6de510"],["/posts/4261103898.html","542c6d6a94315441a5b3ae463f74a4bb"],["/posts/469711973.html","a5ee7cd849232d7d779db4682b079064"],["/posts/482495853.html","1233e2e4cc21e68a20730116bff481dd"],["/posts/488247922.html","bd688a20f2b60f4e42ad1a2af90eff94"],["/posts/517302816.html","5add64176822cf84cd145fb1b9b186e4"],["/posts/570165348.html","49eb27f86cef87335f0ada053dfb7439"],["/posts/595890772.html","af7b15e0f91c74867a13623280f691c9"],["/posts/67485572.html","4b7e21ca7575c495821db2afa672f095"],["/posts/694347442.html","b3db222bda9470e00d2d5d6a5c7d7289"],["/posts/707384687.html","6ed7958b464f40bf0be4690dc62b9421"],["/posts/71180092.html","ca1e5e4a44314398f76af2160ceed70f"],["/posts/716459272.html","5b4080ef25a54fe2a33ca7e12ab7d250"],["/posts/765481613.html","abfefbf0405511adbc469e21fd643953"],["/posts/778231993.html","7b440473e56bbeb809a4d1942eb0d884"],["/posts/795397410.html","9b807e7661e388a41ee5b70ce173d163"],["/posts/820223701.html","0b717b54f488839920dab899a90183bd"],["/posts/830372185.html","5a96fa84b006417cfea7a0c1b2802908"],["/posts/88294277.html","fee289dfb4f9aa2772c30255f652feac"],["/posts/939963535.html","389b64cd4b95957ff1f9a02f73b125b2"],["/posts/983786067.html","da2934405345059b2772d4b2cc04125d"],["/sw-register.js","d0ffd30e06f2707767f0631eb8a7cb81"],["/tags/C/index.html","6fddcaab46ec7c46b0c714fc43c70e5b"],["/tags/C/page/2/index.html","b125446d51a62f99bed855d5a243df48"],["/tags/C/page/3/index.html","9f7e91232f044679113cb56d0c90d749"],["/tags/C/page/4/index.html","9627b2ba408649f849462ab719d24866"],["/tags/ETL/index.html","8b00eb089e081d78d9aeda64e2ab288d"],["/tags/ElasticSearch/index.html","a65dfd68df23c008f30b5179c279dcd6"],["/tags/GUI/index.html","bf1b7be5ae58df90b929a9b8dcd50246"],["/tags/HBase/index.html","9b9c41edc74e0d63aa1c51de86be7289"],["/tags/Hadoop/index.html","ed0cc43691425bfd13ce5caa4b04b567"],["/tags/Hadoop/page/2/index.html","bcfbd0447b05f800862270e21e3db6e6"],["/tags/Java/index.html","5da8121f9f6235c1a53415e7a7afbdb8"],["/tags/Java后端/index.html","52ac427c0a8e3cd260c73736f9726cec"],["/tags/Java后端/page/2/index.html","bdc23b470705fcc39a7ed5fce0e0a20f"],["/tags/Java基础/index.html","706d9d4f55fbf876d851c37ad7cfafc1"],["/tags/Java基础/page/2/index.html","be0474f50e3a0f9e01f5a3ff73fa87da"],["/tags/Kettle/index.html","c517cd8465f5c2d6d353ff9a481f8e39"],["/tags/Kibana/index.html","aeeeee3eebc672c96ca0d9e6456e15ad"],["/tags/Linux/index.html","202e8f1e1212ee79dc9f95a9036d3dc6"],["/tags/Linux/page/2/index.html","34d93d3cfa369c8cce61b92902cdfc0c"],["/tags/Linux/page/3/index.html","75f43d1e420253937b8fd987f4a8d819"],["/tags/Mac/index.html","5098d5dc59dd0cac71bbc32381e2f582"],["/tags/Mac/page/2/index.html","db9b7cb63b0e3ba501505736d87a402d"],["/tags/Maven/index.html","789e086735f366c28df909fcfbc98827"],["/tags/MySQL/index.html","6788bbc2cf75af6a02c48a232e7eaee6"],["/tags/Python/index.html","377a75ecd63a9efa7aaca7991d3fc300"],["/tags/Redis/index.html","20025cc70a16295b4791f6c5e2f88f12"],["/tags/R语言/index.html","1b1d707a766858e33ae7938a068a85aa"],["/tags/Spark/index.html","efef6521caf1600d494c794bdac5cb92"],["/tags/Ubuntu/index.html","59ff5e0072e0949f78072b780d4b3a95"],["/tags/Vue/index.html","c31b6525fa151a47a95a5cafcd497654"],["/tags/Windows/index.html","288588376b01c1c624e3ab4b441eca52"],["/tags/ZooKeeper/index.html","b92ee86cd24c3518d3549f058cf51859"],["/tags/bfs/index.html","06305ec5c34bf28fab94108b1fa3164f"],["/tags/dfs/index.html","c7ee7a9b7d01f33a75f339df9aa72ca7"],["/tags/folium/index.html","f421ff929f79864f50805003d48f3015"],["/tags/git/index.html","9e19fe468a157bd4937b60e0b53fe708"],["/tags/index.html","3cc1229b1c5a16264ccac894e1793897"],["/tags/latex/index.html","c5afa5794ff4a1400ec27e3ca6710775"],["/tags/中间件/index.html","ef730b4dae24cd9dfee6dcb013f3cc6c"],["/tags/二分查找/index.html","3ae5a0e98c4db420bfb6eddb122be7f0"],["/tags/优化类/index.html","97d46920f0203dfff115c1f2d562b11c"],["/tags/前端/index.html","d8d6ab9bc4a05a434d8eb2e007ab4557"],["/tags/前缀和与差分/index.html","90734f260debb4fc4f69661ed8f0b73c"],["/tags/动态规划/index.html","043f55c8ce4cd14b0ed500152bca5b6e"],["/tags/动态规划/page/2/index.html","34da480995ae6fd481f81d250cf3997f"],["/tags/博客搭建/index.html","90f083fc97e9db4586658e07a8fef0f0"],["/tags/图论/index.html","534a7d265181f8378f8ea7f055df1796"],["/tags/大数据/index.html","74fa8e0e6e466486fed7f9d9c2502a43"],["/tags/大数据/page/2/index.html","cdade9697c0011a48f2611fbd8297020"],["/tags/操作系统/index.html","4982523fae15170e7bdf46fcf54f5fc9"],["/tags/数学建模/index.html","fabb918656b2b9dfd20a6632dfd189a4"],["/tags/数据库/index.html","fd2294bd8235fc0c492c07c9121273f8"],["/tags/数据结构和算法/index.html","4d09c047d89fca6014a5ea4834c89d73"],["/tags/数据结构和算法/page/2/index.html","fe74b6326cc1888a572c8bff61fcdaea"],["/tags/数据结构和算法/page/3/index.html","96c71a48ceeb4c33b4d986b7b6f2d6b8"],["/tags/数据结构和算法/page/4/index.html","2f2454b04262b68b2d6cd5e7631785b7"],["/tags/数组和字符串/index.html","b7e4387f4f4a6390daee1f342306dc16"],["/tags/数论/index.html","6c2b81e6429fb5720210faf2825cc795"],["/tags/枚举类/index.html","3bc34933ac85ba1df99823681e5f7d16"],["/tags/栈和队列/index.html","a9de003b83e654023b340d3773642e44"],["/tags/树论/index.html","1572cf08d3d4888310608bdf27a7dce9"],["/tags/测试/index.html","5eb82ff7b75dffb784b557ac8802b806"],["/tags/环境/index.html","07ada59c08009ce4a6d9679ba6488ab7"],["/tags/环境变量/index.html","856bbbf5c1b0a56be6308c151c045a38"],["/tags/绘图/index.html","71ef1cd2b25e07a65801d8b836b23646"],["/tags/编程工具/index.html","bb9ddbebbfa402fc9785f6231a18ed49"],["/tags/编程环境/index.html","08f38abbc5a93cf63e6d80d9ce4ee52b"],["/tags/网络编程/index.html","43c076ad847573df60d702c38e721c73"],["/tags/英语语法/index.html","75eb44eda3d0ffe54dbfe99060167717"],["/tags/计算机操作系统/index.html","8dbec43c1fab9b8f32f57e69e0b12837"],["/tags/论文/index.html","2c824e62ef6a7ab05fdd8900b944e68f"],["/tags/资源下载/index.html","95c482518b46a4031fc776e1b6e59324"],["/tags/链表/index.html","212c30f59ac0ca430df44aa894d36ca5"],["/tags/集合/index.html","0c63e59e9270bc24ca0d5af88b33a0e6"],["/tags/集群/index.html","70ed8fc539b02a81f4c123220359c9cc"]];
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
