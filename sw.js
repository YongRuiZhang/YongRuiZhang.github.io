/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","0b02fde76921d618a7495452fc4a79f3"],["/about/index.html","96dbe576739943d47b0f2d9e2c27fc11"],["/archives/2023/01/index.html","6698d302ffa2f6fb36aee7155b11c7d9"],["/archives/2023/02/index.html","3320a1f475e181a216894551ba43db9f"],["/archives/2023/02/page/2/index.html","b83273611ba1d8c2bff7198a52f72347"],["/archives/2023/03/index.html","e859e56af3148cef4d53fb610547c0fa"],["/archives/2023/05/index.html","d11c25dda1b2bf104a66cea8faccf271"],["/archives/2023/06/index.html","500c51e67f84279458f9af8359b633c4"],["/archives/2023/09/index.html","01f381ce7a043c0ec63b184f2168c77e"],["/archives/2023/11/index.html","6ec4086a84e4795a645ff8e165d037c8"],["/archives/2023/12/index.html","cda941458a91f7762d764073c85d62ba"],["/archives/2023/index.html","fe638e183cc0a8dc6b13f2271622986d"],["/archives/2023/page/2/index.html","9bd534b51ec8de7669bcc2530dfe0aac"],["/archives/2023/page/3/index.html","41dc2652aefad2ea9f7bb0090f1dd215"],["/archives/2023/page/4/index.html","0cdbdd9b06cf07f855e2a4b8925e0863"],["/archives/2024/02/index.html","ad5e89f0cd86da5d0e71bd0d9f50a2b9"],["/archives/2024/index.html","16f494fcccf82eb9225d1af60587742d"],["/archives/index.html","3dfac511e8ad505410ae145671c73ef4"],["/archives/page/2/index.html","022d4033bac428da6daf01d272c11a4c"],["/archives/page/3/index.html","c3ce1358964bc3f8cfc36e65e1f58604"],["/archives/page/4/index.html","ff8a6e44c8c05e220837736a39753ca6"],["/baidu_verify_codeva-qQP2iZOMLX.html","eca3028a177011f4c76ec33ac2876461"],["/categories/Java/index.html","6bae09bad63f10c2004baf2795ca0560"],["/categories/Java/后端/index.html","f7e3b608125190e2c4a4dfea2d13be12"],["/categories/Java/基础/index.html","90c8467ed2d63e85e6a2fdee68b0b949"],["/categories/Java/基础/集合/index.html","f86a524099bc7fedb646bd22b9c9e137"],["/categories/Python/index.html","e831f02ab1a2f22556156509e75ca0a2"],["/categories/Python/编程环境/index.html","b59361a16e75308955d934ed87951d60"],["/categories/R语言/index.html","9ec6cc1b53161062e464543e6b089bbe"],["/categories/R语言/编程环境/index.html","18daacbc6425d6c8f73b8fdcf77a0def"],["/categories/index.html","5c1a119a5f0c3ef050ff33519fd4311f"],["/categories/中间件/index.html","5fe3568f572895997285574d8571d4e3"],["/categories/前端/Vue/index.html","1401ff6934d9e42bee8081b587a43f10"],["/categories/前端/index.html","b3a6c7fd52e7bda28aa2e264d7db14cd"],["/categories/大数据开发/ElasticSearch/index.html","b585add61516e4b1061f28118e64d9f2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","dd001d576e80418506d51dcc93702792"],["/categories/大数据开发/HBase/index.html","ba5b5a132ed0b79275393461c39fccd3"],["/categories/大数据开发/HBase/学习笔记/index.html","a8afb31a71a7bbd488ebd589258a1f5e"],["/categories/大数据开发/HBase/环境搭建/index.html","8d9bb000fbf11228591d9af386c28164"],["/categories/大数据开发/Hadoop/index.html","bb75d37f7cda173ecef141c87940ee2c"],["/categories/大数据开发/Hadoop/技术/index.html","6f9f49fc8f8ff0e5aacd335ae6b192f1"],["/categories/大数据开发/Hadoop/环境搭建/index.html","339f5fae4290b949aab2a173e7ff1a45"],["/categories/大数据开发/Redis/index.html","5488cac3a7050f7d1c39ba0f055116f1"],["/categories/大数据开发/Redis/技术/index.html","63339832832114f349b60f9694b6cb9b"],["/categories/大数据开发/Redis/环境搭建/index.html","e063864e8e09fedf9bd98a58733105d6"],["/categories/大数据开发/Spark/index.html","f1c65041c38207899f7af07589b6dd5e"],["/categories/大数据开发/Spark/环境搭建/index.html","b790e8e9b59e0a166fda034cdfa94280"],["/categories/大数据开发/Zookeeper/index.html","9bb60d9821bff9e7190023c6aa4d2ac5"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","6c950e9de58c1b880ad2d9eede752726"],["/categories/大数据开发/index.html","d0e6cd556678b850f92f6577e49a6e0a"],["/categories/学校课程/index.html","5ac2d6ad115f2ebdb5c4d43fad954080"],["/categories/学校课程/计算机操作系统/index.html","db1010e69cfe786944eac36db15e604d"],["/categories/操作系统/Linux/index.html","716037f63b4db5132ff32e2ce26395ff"],["/categories/操作系统/Mac/index.html","35c35c472020afb4dae66db510997964"],["/categories/操作系统/Windows/index.html","77e1d595440811a5a9878a35df3066ad"],["/categories/操作系统/index.html","8b02e49468125e7230d132ba2e380809"],["/categories/数学建模/index.html","703e405963995004ea2d60123cdd9d21"],["/categories/数学建模/latex/index.html","b7475204feef02484f2ad4da31d3badd"],["/categories/数学建模/优化类/index.html","ee9e1a16c03b7d56409e19564b224192"],["/categories/数学建模/优化类/现代优化算法/index.html","07bdbbdad4111503ada47292aecdfbce"],["/categories/数学建模/优化类/规划类/index.html","4b1f573671b507b47f2a1309763266d5"],["/categories/数学建模/绘图/index.html","6b3bfe5b7e865c034518510d0430cf40"],["/categories/数据库/MySQL/index.html","c5b877f9b8739986c85a4ee361f8fbb1"],["/categories/数据库/index.html","f8911ec9407a1c21afe3bb074a7cd061"],["/categories/数据结构和算法/index.html","70878a89bd4d6e2d7d210418201f5410"],["/categories/数据结构和算法/page/2/index.html","9388a8d0bc91390c7e71c8eb394e4df7"],["/categories/数据结构和算法/基本原理/bfs/index.html","2f2c05a84ce3f994176975591a74ef7f"],["/categories/数据结构和算法/基本原理/dfs/index.html","150b8581ef0f86d1c8d35826c4ba1d0c"],["/categories/数据结构和算法/基本原理/index.html","387f2f750b49ad9bbc0b0b8535efd309"],["/categories/数据结构和算法/基本原理/动态规划/index.html","ce2ca8b38c570852e8af976bf9e9c89c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","667bf2bb0d7431702c045a9fad824e65"],["/categories/数据结构和算法/基本原理/图论/index.html","401a7b839775f601bcee04c8bf3f807b"],["/categories/数据结构和算法/基本原理/字符串/index.html","5dfd0ab2e2a85d808aae8d2e6bfb0708"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","12421e32c76d5d8006a5b3b91749395a"],["/categories/数据结构和算法/基本原理/数论/index.html","1235465665e3c1b9a3911fa2da525f67"],["/categories/数据结构和算法/基本原理/树论/index.html","00c6a01157bbb8aec6ff3eaf05ce1bd2"],["/categories/数据结构和算法/基本原理/链表/index.html","32c2fb84ff39faa3ba270af065bc22fa"],["/categories/数据结构和算法/算法题/index.html","10d5ce987d312d5311f20d39edb54d5a"],["/categories/数据结构和算法/算法题/二分查找/index.html","29611f6b357a2ef0e07815b83fa592c9"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","87981eb2d530f7718717872b3ebb6700"],["/categories/数据结构和算法/算法题/动态规划/index.html","fdac134c2a10f1afbc006031d82948d4"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","7f10859515f55f627f5f326c6ccc2fc0"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","85bd9d04d598b7647b697d3766bafa47"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c494444dc2f410e4626052533d5db45e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","0f31217f47460c16e0803a7481fd21bb"],["/categories/数据结构和算法/算法题/数论/index.html","57ac5b5e120c6a9d457e64d90544aa7d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c63393853929b5ba9353e829a408d4f8"],["/categories/数据结构和算法/算法题/树论/index.html","410e1e2330edf4b7af0bf40362e0b8c8"],["/categories/杂七杂八/index.html","949e55ed04af457b54c345eebc1ef970"],["/categories/杂七杂八/博客搭建/index.html","2ff13f8ce8b1c86a2740494cd2c309cd"],["/categories/编程工具下载/index.html","9696785870f3e881ea83ad0e5c3d1808"],["/categories/编程环境/index.html","22c988b5cb69a30a2db6a310c79d3556"],["/categories/编程环境/大数据/index.html","a9c4a4ad6c94740db2b9bc6e9084f1ae"],["/categories/英语学习/index.html","0956f1335740841212ea97e48bf15fa2"],["/categories/英语学习/英语语法/index.html","8c85254aed7866beacce1ce541156410"],["/comments/index.html","0c9648119eb661be2ac80ac0e33111f2"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","1fec69835dda71facc3097c8e6f74546"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","da6ecf0def77f0840d39c691130fc1c7"],["/movies/index.html","67470455a0e651616d09b184eed01f92"],["/music/index.html","9783cf166c65b4293d300ec7cce3f941"],["/page/2/index.html","1a4384273fb5a3bfb2f1fad428c0915b"],["/page/3/index.html","6d624cb200348420e194c014abdc18ce"],["/page/4/index.html","4b48296d1d2c6f585e43fcf110f52e2a"],["/page/5/index.html","6a693537119f9158dfb30a948a475744"],["/page/6/index.html","6020a2d2ea592587667988473b8586bc"],["/posts/1021360842.html","c42a56824376309c50070d190b4195c1"],["/posts/1120620192.html","b4deb92e384974ff5d6971ae0af9856e"],["/posts/1141628095.html","c259e8501a9eeff60a997faa42e3dcb1"],["/posts/1168613674.html","8a70522dd3be2f14534b16d049074a6e"],["/posts/1219920510.html","841c01c1a2a213b075e32e24f71c4e3c"],["/posts/1222166338.html","9175750535954e025481dcde14aed74a"],["/posts/1259097482.html","812aba25f0553e555b81cb2dc2bfb9d6"],["/posts/1271036369.html","a6ad1ef892ee87284fcadd097433c811"],["/posts/1312847445.html","918b99a43e5474fb29c992cd2784a6e2"],["/posts/135355774.html","d709a42b26d715ca90ab5753d462820b"],["/posts/1375344716.html","a444160dabf4edaeefcdc8496073b2a8"],["/posts/1388991698.html","dc8c8e1f3811ea7bde7631d811c7f7b2"],["/posts/1410315814.html","f53d01de91db7e6512d13e00dbef4464"],["/posts/1452790229.html","9b9257e6bc19f98f719b6f2ee2c8603c"],["/posts/1470079884.html","91e71fdb4d4ccfa51c2a9c2c1c8d5efc"],["/posts/1470079885.html","91f65f055d2490a1b57510918ce25737"],["/posts/1470079886.html","12e8ec9cf952213a6ab2ec9b84086c68"],["/posts/1470079887.html","81cca2e168098271fa21a2e5531039ca"],["/posts/1498536549.html","a0e8b0c5c21c9572db0f0eb772c3b959"],["/posts/1539568593.html","cc48fc088d98d04ef7e33c72794e51cf"],["/posts/1547067935.html","f694b62c72c2a2cc02f55faa1bb9381a"],["/posts/1557866301.html","cbb79ca7e6baee59ac6d09eb0f786c25"],["/posts/1571776361.html","999842cb2e7fa50a0cf3f5b2a6bcacb1"],["/posts/1605124548.html","60558312c6e01079dca3448f3559db18"],["/posts/1633036852.html","c652b71d45d2362f58dafe841d84b820"],["/posts/1674202625.html","ac06a1431ffec9e2b1ac8d838c84ca21"],["/posts/1765123828.html","5c319d5eca93f0e65a9f55ea7a58777f"],["/posts/1767336200.html","8bf01554195acc92c94b399b49bef220"],["/posts/1776114197.html","57daf17b5d8af39e27c537c1c76c91ec"],["/posts/1817748743.html","e69dde11923b548a5c395a2c0799a319"],["/posts/1925125395.html","d35ccc8bdb513a2ab060a1e7ffa4ace5"],["/posts/1966191251.html","e9833b3da7b0cdf7824b30c1ac0f8c88"],["/posts/1987617322.html","cbc518b10b49697ae9d3ebcb3abb0f72"],["/posts/1999788039.html","91e8d9d40bea420b327f0c6479364b03"],["/posts/2075104059.html","915da9e55d58ee49c12b2bc1162d3d38"],["/posts/2087796737.html","7ce460c15ba7cd9c9d0ecd87e3115b5d"],["/posts/2106547339.html","6505c649700500aeec49ca4fcb2a3c57"],["/posts/2207806286.html","7eee8149b072c432ee33eeee3c3ea1b7"],["/posts/2225903441.html","5ccc8c341101f67bd69dbd4a320b0700"],["/posts/2265610284.html","171bc5de7145c530079b248b3947934a"],["/posts/2281352001.html","50300e5fad7435e9b8115bb9be4cf23a"],["/posts/2364755265.html","1cc8bacbf2e6522d5adc10ac940989c1"],["/posts/2414116852.html","5bc7c29ea3537dfa729f78d832de35a3"],["/posts/2421785022.html","c5b1d7bbb76947bbde46215adef88f29"],["/posts/2482902029.html","3354fd0c90f5e0fe2483bc9e61029e60"],["/posts/2495386210.html","9964a7989b17fce3dfcec56a98132620"],["/posts/2516528882.html","87a78e9888c5c6203daae1f7038f7bfe"],["/posts/2526659543.html","ade8f6045df7685b2d72698fcd536816"],["/posts/2529807823.html","43af7c11093e6c8ae0d8f83b6a53ed65"],["/posts/2596601004.html","40e6fbafa7891146ace10345fcbaacc4"],["/posts/2697614349.html","13a22119ae6411ec7cdaf1defdd0d1d5"],["/posts/2742438348.html","e5784feab4a0774d2e1d3d5c76495845"],["/posts/2768249503.html","44ad0712ce4a6c06a8eb5b3674bb3d4d"],["/posts/2864584994.html","10e07dad247b8f1b3161c7d63b2cdada"],["/posts/2888309600.html","62c6724f7aa5071868a346b5c6063458"],["/posts/2891591958.html","a249461beba3b8b530f54f9e37311277"],["/posts/2909934084.html","c34c2bed6ea2f1e0d25bc3776fe7f155"],["/posts/2920256992.html","e2a7f8cd2beab13372cf8e5a3deec6a0"],["/posts/2959474469.html","2cbfb6869980e7e63024a29e58bf31c7"],["/posts/3005926051.html","e7db6ac562c16722b417b6c325598576"],["/posts/309775400.html","e0f6f0d27cef67cb0f9b23044a31123c"],["/posts/3156194925.html","cfa1acefcb99712435ef50f6c4a82658"],["/posts/3169224211.html","f5c416fbc633d3fe76c2907a50d173e6"],["/posts/3213899550.html","0c2ed1b2431f293ecfbb885bdcf411f9"],["/posts/3259212833.html","296b4a120b8b705f2ec9c39f52f31449"],["/posts/3266130344.html","3bbdac181a5d68519dbdc878b6ff1540"],["/posts/3292663995.html","247949dba39272f334de3ef46c17544b"],["/posts/3297135020.html","d0a88f7cbe4a210793dcd952caf7cf61"],["/posts/3306641566.html","9ead0710cf039e77ec6ec87d8ddd2cef"],["/posts/3312011324.html","70580d4b842a8f78953aad334ad5781f"],["/posts/336911618.html","07fca4bd32c67596aea5c725cc135dce"],["/posts/3402121571.html","72b2060e4fc1e958eac985f1329a1339"],["/posts/3405577485.html","97f64985316ebab739697c9dae4b8dc5"],["/posts/3498516849.html","4ed26c26aac5356005bb66702337e64c"],["/posts/3513711414.html","aaaec1178c3dda3e315ca4ba94fac10a"],["/posts/3523095624.html","c15866084ae065f2138c557af16b5881"],["/posts/3546711884.html","6fb8da85607e28b9ee1943194025d616"],["/posts/3731385230.html","117990282783e1ece699a81fa7b6a150"],["/posts/3772089482.html","5fa23ab1ebe1caf46fed6b3ee806489b"],["/posts/386609427.html","0623858a475adfa744b928336ae0f318"],["/posts/4044235327.html","7f09d166587afa906f065a1d7791057f"],["/posts/4115971639.html","fa6c62579a3ef7eb275f113518c1efbf"],["/posts/4130790367.html","39dccf8deb2d1435a5614b7e9ca91c06"],["/posts/4131986683.html","db3eec4f286f3b58a024769986bc567b"],["/posts/4177218757.html","7ee449deb982673d5d7b56a290bb7cf7"],["/posts/4192183953.html","ae17bf511dd20eb7e0ecf4c1df6de510"],["/posts/4261103898.html","542c6d6a94315441a5b3ae463f74a4bb"],["/posts/469711973.html","a5ee7cd849232d7d779db4682b079064"],["/posts/482495853.html","1233e2e4cc21e68a20730116bff481dd"],["/posts/488247922.html","bd688a20f2b60f4e42ad1a2af90eff94"],["/posts/517302816.html","5add64176822cf84cd145fb1b9b186e4"],["/posts/570165348.html","49eb27f86cef87335f0ada053dfb7439"],["/posts/595890772.html","af7b15e0f91c74867a13623280f691c9"],["/posts/67485572.html","4b7e21ca7575c495821db2afa672f095"],["/posts/694347442.html","b3db222bda9470e00d2d5d6a5c7d7289"],["/posts/707384687.html","6ed7958b464f40bf0be4690dc62b9421"],["/posts/71180092.html","ca1e5e4a44314398f76af2160ceed70f"],["/posts/716459272.html","5b4080ef25a54fe2a33ca7e12ab7d250"],["/posts/765481613.html","abfefbf0405511adbc469e21fd643953"],["/posts/778231993.html","7b440473e56bbeb809a4d1942eb0d884"],["/posts/795397410.html","9b807e7661e388a41ee5b70ce173d163"],["/posts/820223701.html","0b717b54f488839920dab899a90183bd"],["/posts/830372185.html","5a96fa84b006417cfea7a0c1b2802908"],["/posts/88294277.html","fee289dfb4f9aa2772c30255f652feac"],["/posts/939963535.html","389b64cd4b95957ff1f9a02f73b125b2"],["/posts/983786067.html","da2934405345059b2772d4b2cc04125d"],["/sw-register.js","7e0db10b8c5d17a8de25910bd6117968"],["/tags/C/index.html","d7c7f62fe8149cbe689d30c697aff2d4"],["/tags/C/page/2/index.html","8a1b1a4723a394679a1b2571bc91a08b"],["/tags/C/page/3/index.html","d6861748389b598715b3b8c9baaa78ff"],["/tags/C/page/4/index.html","e1cef693c5cd11b9ec6f2dd6e8927d41"],["/tags/ETL/index.html","7e5bb56e601edda806d8f132d872c9de"],["/tags/ElasticSearch/index.html","1dbcec2ca0e64bd3dd261aa9830e974b"],["/tags/GUI/index.html","821f2c9884202e37675783e5313e5421"],["/tags/HBase/index.html","e5dca14403bc629bf7eb7c0023255acf"],["/tags/Hadoop/index.html","2923cc20ac11ec2c916dc9462662f0dc"],["/tags/Hadoop/page/2/index.html","a857c763d1581ebb8792160e81d7a8f6"],["/tags/Java/index.html","320fd6274a483c8f02a5235a95f4bff5"],["/tags/Java后端/index.html","dddc04186f92f2bca46a4d277863fb57"],["/tags/Java后端/page/2/index.html","5fd62a3b50ae42651456a2f30d03fd64"],["/tags/Java基础/index.html","b090d533eb6ec68fb2799a3125eeda1a"],["/tags/Java基础/page/2/index.html","447b28f5f6463d954b37194f22f2a6ad"],["/tags/Kettle/index.html","b317cd9481a923f229df24e0ddf15738"],["/tags/Kibana/index.html","16b8ca2caccd15b135094b5f519258bf"],["/tags/Linux/index.html","b94a2d81e2480dc7bce6408b4e8d789f"],["/tags/Linux/page/2/index.html","28c8b2d057d2bf3d539fa0606d9872eb"],["/tags/Linux/page/3/index.html","882ee34528d04404bae2b8a2fc62ef78"],["/tags/Mac/index.html","362df709b75ba1587e2b176390383551"],["/tags/Mac/page/2/index.html","8aa85ba5c2a0e8ecd26192651b58312f"],["/tags/Maven/index.html","57a2dbb3d5f1104a4c2d1b0517e827e7"],["/tags/MySQL/index.html","13e33480f45a60406ffe1476bed551a4"],["/tags/Python/index.html","438011ef68491024999c556e128b7532"],["/tags/Redis/index.html","c9db816ed6640a9ce3d474dd97aaa932"],["/tags/R语言/index.html","ebbb1b409405aae875cef70b098bffd9"],["/tags/Spark/index.html","0d2299cf3335281ba6f181c2137dd57d"],["/tags/Ubuntu/index.html","705a324bbbfd0119f00320bbeb1f9c17"],["/tags/Vue/index.html","0c41f1484f6015b03249f740c57ae5d1"],["/tags/Windows/index.html","fd9bd0e41d0db40c8e2ac66b500a4d0e"],["/tags/ZooKeeper/index.html","a752eb65142bf81ec099f74ea0d04f6e"],["/tags/bfs/index.html","ffe1d181cf6244197f474bd3bcbf7075"],["/tags/dfs/index.html","f7a2e09c3de1b912a60cf9869d7fafb2"],["/tags/folium/index.html","99f18a4b4a465b4f51a10c8452f17bfe"],["/tags/git/index.html","411ec5160f13992a69d75a68b5691ec8"],["/tags/index.html","ff00e81bf0111fbb9542949dfe99b547"],["/tags/latex/index.html","c2c1bfc4229a60e9eb7846f46ffa6d79"],["/tags/中间件/index.html","2421a36641e25e83d4dc4164b08904fb"],["/tags/二分查找/index.html","db765d4b7ba4bddeeccf6fd80e51177f"],["/tags/优化类/index.html","eb08c5d324e9e53d0624ea4b76c51f94"],["/tags/前端/index.html","25be5082bb351d6e4c5d13bdeb440dfb"],["/tags/前缀和与差分/index.html","2a0bcdbaf36f29421c0f2b90bc0dbc82"],["/tags/动态规划/index.html","fa289a1a0031eddbff6abf68ef55a968"],["/tags/动态规划/page/2/index.html","7923921f9bebf96650bad032930ea9b8"],["/tags/博客搭建/index.html","60219ae281fe51f47abd2397b1716f1b"],["/tags/图论/index.html","5981f4de9a5928cc0011af79cd349910"],["/tags/大数据/index.html","1f057014ae2f1c8a2fad20ce9fa35c94"],["/tags/大数据/page/2/index.html","a2b4a762f34e83ceba3317e662ea3f96"],["/tags/操作系统/index.html","66078021e42ee310aeefdd17706c455f"],["/tags/数学建模/index.html","4423b91d5e240ec62f9f3a72c7664da9"],["/tags/数据库/index.html","c9d02815396ac19603480f530151a392"],["/tags/数据结构和算法/index.html","068b49b150bbf0180ac1f3e6ac328793"],["/tags/数据结构和算法/page/2/index.html","2908e7dfcee2f303bfe6b9888d32c6cb"],["/tags/数据结构和算法/page/3/index.html","f17476d447356650039bdb96d8a9ba0c"],["/tags/数据结构和算法/page/4/index.html","08b7c2cf9ce28e3e487bc662e854030c"],["/tags/数组和字符串/index.html","2c7a19fc8f1392a6375783f7b049e7bc"],["/tags/数论/index.html","ced23ee91a2ccdd10ef7e5275f6d31b6"],["/tags/枚举类/index.html","8da6e52d9b817d2e610bb0be4bbccc22"],["/tags/栈和队列/index.html","0bee14f57bde11c0d896472560b41483"],["/tags/树论/index.html","9b1e7a7052266a1b6c66f99458f39aea"],["/tags/测试/index.html","56689dbd104882396d988ac893e389fd"],["/tags/环境/index.html","7f43612d38be4be9ef85c35e576cc630"],["/tags/环境变量/index.html","bd89275810bfc2b95101b32fff99673b"],["/tags/绘图/index.html","e051e727283b020e9a2130df085edbdd"],["/tags/编程工具/index.html","af30f827bc76974c6bcb81fa3c97d1ad"],["/tags/编程环境/index.html","3dc33248617c5db1b8d4b1fa8084382e"],["/tags/网络编程/index.html","89deee537a0163b2959a635fd7777519"],["/tags/英语语法/index.html","5c733affee2bfad87575de5ac57095e4"],["/tags/计算机操作系统/index.html","354644a89c17de251f8da24ad64926c1"],["/tags/论文/index.html","88bcf518c67b1e2d2f2891b2e3ba8d09"],["/tags/资源下载/index.html","6a62e5675c790392d4dada538a1ffff6"],["/tags/链表/index.html","2eff28ecec483dc59f4c54ac5d5ecfe8"],["/tags/集合/index.html","48705e59c6b91dc3ca0cf43a0f7b67bf"],["/tags/集群/index.html","7f4279874d784b847b93507bf9c4291c"]];
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
