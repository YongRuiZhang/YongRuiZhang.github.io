/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","732583e05a5d0591af2ee8c28a352c7e"],["/about/index.html","5cc07c1790c21e8aaa4aacb14ec0482e"],["/archives/2023/01/index.html","ce0c27009b211eda0f75806b368870e5"],["/archives/2023/02/index.html","0eb3aed729068ab5e84df6b51abb1e16"],["/archives/2023/02/page/2/index.html","bea96cb53626a97ca95616fdeabbc0a9"],["/archives/2023/03/index.html","54cdaeb40395d74fea19f040f47b3bfb"],["/archives/2023/05/index.html","cd6380a5fc77fe931bc4d6478a9bbd89"],["/archives/2023/06/index.html","97ad982e368a3459cc5a7e91f2bb1092"],["/archives/2023/09/index.html","1e0c46f5c702273af5d37bf8f88573c7"],["/archives/2023/11/index.html","7e10b06628209f5ab7e9a981a90b4909"],["/archives/2023/12/index.html","ddc059b7a05eb2cb4d4df8ae3a4eb40d"],["/archives/2023/index.html","4825e6f6d32d59847256057e2e0e8eba"],["/archives/2023/page/2/index.html","b1a0be72414a19c8a7150b06ef31ba0e"],["/archives/2023/page/3/index.html","723e3de7f610e33c23c123943dfade5e"],["/archives/2023/page/4/index.html","8ff5ec77a910a6ecfb5c15052622757a"],["/archives/index.html","4eef2bd11cffbd4677f4c44130d1c978"],["/archives/page/2/index.html","2d392a6711848a056072a470304858d9"],["/archives/page/3/index.html","2c9116f5150e7c008504a12ded76b3d0"],["/archives/page/4/index.html","55db446174e394e3d9aca8166dde3bd4"],["/baidu_verify_codeva-qQP2iZOMLX.html","5e7316974a3f21caeef8a8580239c149"],["/categories/Java/index.html","d0fb0cfd9e6054dbe470236c8b7dbec0"],["/categories/Java/后端/index.html","b27c05c03e7077228ce4a5f8a6ef2e95"],["/categories/Java/基础/index.html","41b71a33862f25bc99b2b8ee09913217"],["/categories/Java/基础/集合/index.html","c0a2ea605e1497dbc96cde77ac06636b"],["/categories/Python/index.html","222122b11b02e03aa9f4cdb3e73cc8ee"],["/categories/Python/编程环境/index.html","5e9a4a40cac55cdde45dbcfb799fc0d9"],["/categories/R语言/index.html","a4a0332094d99623939fae8332dd7ffb"],["/categories/R语言/编程环境/index.html","bd0643eeeef18bd9b20fb9569b7d0319"],["/categories/index.html","ca9a1dd26543e37bcf76efaf87cefa8d"],["/categories/中间件/index.html","362ebe96955d5b52722b1007ee2ae1b3"],["/categories/前端/Vue/index.html","06a9b56394b9e8ccfae339c40202e632"],["/categories/前端/index.html","68b083478e1816d70ecf49ad0edb5217"],["/categories/大数据开发/ElasticSearch/index.html","f339d27dd1a5c2648955842057ba5e03"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","91cda5dfee202d8b6c440e8584d30b9e"],["/categories/大数据开发/HBase/index.html","9315650788faf178380cedb1e59e3afd"],["/categories/大数据开发/HBase/学习笔记/index.html","8d3a156ccca9154b1207ac813d217c04"],["/categories/大数据开发/HBase/环境搭建/index.html","ca24502871132c1077a15b0db52e446f"],["/categories/大数据开发/Hadoop/index.html","14aed8fda2f1377bfc7396b55ab6f117"],["/categories/大数据开发/Hadoop/技术/index.html","52f3b61982ae38bd3ed4ff859c9850dc"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b15222c3632c11cc405bca59c718cb3d"],["/categories/大数据开发/Redis/index.html","6e8ad12a0362285e64fd9562c445edcb"],["/categories/大数据开发/Redis/技术/index.html","222495ae4e426be529234cebb5804201"],["/categories/大数据开发/Redis/环境搭建/index.html","2f0abe6cbf4930502ffc3441807857d1"],["/categories/大数据开发/Spark/index.html","b795dab120f01260ce0809bd6bdbb6af"],["/categories/大数据开发/Spark/环境搭建/index.html","7287d7e40643ba1478ba20fdd815faa6"],["/categories/大数据开发/Zookeeper/index.html","2e567d94e55578a6f96e6e5587e436a0"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","6bd481e31aeae43d9337f981dafc8420"],["/categories/大数据开发/index.html","119d1cba82fdf039b01f0f094b10777a"],["/categories/学校课程/index.html","fdb2d9664f8744fc07b7cfcfcd9ce847"],["/categories/学校课程/计算机操作系统/index.html","10fcfc6eb136445567c13a8a21e2e8cd"],["/categories/操作系统/Linux/index.html","83adebe11d9ce5a55acd1c90e00ed74f"],["/categories/操作系统/Mac/index.html","157085f5937ea1fee0ab4f1aca2afdf4"],["/categories/操作系统/Windows/index.html","c8275efd80ff8551cd8a2e273281e55e"],["/categories/操作系统/index.html","038411210a1d983676ed0bda77a95440"],["/categories/数学建模/index.html","74792b605c2a627a9bed8e4117b97d7d"],["/categories/数学建模/latex/index.html","7d5989ea2b7569ba09c822968a030b0a"],["/categories/数学建模/优化类/index.html","4fb4d8b1d6eb762c453ea8d0d024b1cb"],["/categories/数学建模/优化类/现代优化算法/index.html","410f236d5111da78a80703acd2753879"],["/categories/数学建模/优化类/规划类/index.html","04de09cbc4a2827da72f63f597518a2c"],["/categories/数学建模/绘图/index.html","017872d1988b10f772b01978dfbc6601"],["/categories/数据库/MySQL/index.html","1981e78b7aaf98cc051c8e4fee2a3623"],["/categories/数据库/index.html","4973db75fa731fb807cec50c7666b778"],["/categories/数据结构和算法/index.html","d147b36a96ea0aaffbe3e4ced1e72515"],["/categories/数据结构和算法/page/2/index.html","66b2564544b5e3063788c7e73ff3eeda"],["/categories/数据结构和算法/基本原理/bfs/index.html","14df5fdc8aeac2f454e65fbee2973457"],["/categories/数据结构和算法/基本原理/dfs/index.html","47de7b5b06a58d2459e9c76f6fc7afef"],["/categories/数据结构和算法/基本原理/index.html","01f9f0baa3f61ab8aa272315708bbc95"],["/categories/数据结构和算法/基本原理/动态规划/index.html","adf5c29c2b3069b8a794a51e8e8b224e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a1dcf39b42a9475f3998d62ce7ba503d"],["/categories/数据结构和算法/基本原理/图论/index.html","c5c0ed639b1db21de74a09d37e49799f"],["/categories/数据结构和算法/基本原理/字符串/index.html","3ec1184a12f78aeb579eb9ddc48a5bf9"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b35ec16cd4e85e08a4f66306f663b037"],["/categories/数据结构和算法/基本原理/数论/index.html","c366ebce097c0ec0acb5b78895b68925"],["/categories/数据结构和算法/基本原理/树论/index.html","ad74c9ef151b7724f70f468ce9cc4ce0"],["/categories/数据结构和算法/基本原理/链表/index.html","eba71fed1470c400be3a1077132a8031"],["/categories/数据结构和算法/算法题/index.html","3b69a84a7b2886da61bcef39d67e6f23"],["/categories/数据结构和算法/算法题/二分查找/index.html","232b7db761a4f55d72c8292a43308eae"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f3b3b2b1cd4f72af1de88e74b9f59ac2"],["/categories/数据结构和算法/算法题/动态规划/index.html","d8074eef63a174753f9d966a9d302113"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e00c91e93583d54962c2afebaf8d2e9f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","416f0553cc3e2ae10e77738c0e15afcf"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","63026f2cd4cbec2b6e1f13242d344264"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","cf9aca004431b19be70cf0013e259cbc"],["/categories/数据结构和算法/算法题/栈和队列/index.html","4b879d3450366c11066ddf5a9136beab"],["/categories/数据结构和算法/算法题/树论/index.html","4124f0e50fff225a35cb3f7380df5d2f"],["/categories/杂七杂八/index.html","b3ccedd952dc8000174e023d15b6fdde"],["/categories/杂七杂八/博客搭建/index.html","38ce5186591f97a9f84c5942d8d0d0b7"],["/categories/编程工具下载/index.html","c73200e724434865a39de633c10f991b"],["/categories/编程环境/index.html","1243969e95be2e36e9d3a18fa5984b53"],["/categories/编程环境/大数据/index.html","d917bd6ba15c713ae5a6824055ba0579"],["/categories/英语学习/index.html","4a5712afd4c8199c157fadef5b450e26"],["/categories/英语学习/英语语法/index.html","f891ef819ecfe105419899ab23070a79"],["/comments/index.html","93fd8c750432b9fce59012c230319aac"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b61687b3f75eda2818e3a9664f6b0e48"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","3f1ea12aaf5c59fe34afa6757d133c09"],["/movies/index.html","6b05faf7d3432deee2f97a6582a52028"],["/music/index.html","1a4346b333a21d63dce6b8ccf874ef62"],["/page/2/index.html","747408e996d632b284439c0813579372"],["/page/3/index.html","89bd7358cb1ed2b6322fc04bd5e9c14b"],["/page/4/index.html","2f7aa7cb3e44daed581affe0e9b0fb14"],["/page/5/index.html","0f34205238d9ff44fce8e9bb6d73ac05"],["/page/6/index.html","899db20cc41ee530bd85e2f53a54b23b"],["/posts/1021360842.html","98bbc9ba701a3456a2badd0d25daa498"],["/posts/1120620192.html","9a093abd69921ad6a1c968afb16b1772"],["/posts/1141628095.html","135b704247143f0ceaa10e54962f422e"],["/posts/1168613674.html","34b2458eda72d4a7667b025b33077977"],["/posts/1219920510.html","260f307b33211b4acc0d8b394e983bd2"],["/posts/1222166338.html","8313fca9cdd1dbdb9245105c02d2bb75"],["/posts/1259097482.html","c3bdd7f48b337372feef7457d89d36b4"],["/posts/1271036369.html","2871c33ec1244ac15f13ca13ec530287"],["/posts/1312847445.html","2660ce4597a7f08380c86126bfd01b41"],["/posts/135355774.html","3da63224346f04328a4935537e6c7594"],["/posts/1375344716.html","f59810c8568f13957f81228dfca58721"],["/posts/1388991698.html","00cfb5da82fb564efaf075c9ff338bfd"],["/posts/1410315814.html","c23d922ad299569486b3b1a9415aab46"],["/posts/1452790229.html","4b4657af31562b9507694aeaa7afe3be"],["/posts/1470079884.html","2c2b60c1691219edb64007d7dc201558"],["/posts/1470079885.html","f796f95cd8b38040f4f06114666bfab5"],["/posts/1470079886.html","df12afa55b2a01f077cc3212c43d37e3"],["/posts/1470079887.html","3cea6bd8a8dec42e7971ffa81ba821ec"],["/posts/1498536549.html","852e96d93cb3145f4ec5e5a86888dee5"],["/posts/1547067935.html","ecc3d1e1add50901b8e18b8bddd15cca"],["/posts/1557866301.html","845d9584583ab114c4c3d41fee045175"],["/posts/1571776361.html","6981c612f9633bfefa3c5e98db785456"],["/posts/1605124548.html","40e4f1b5d8f521a346b3a0e6c7fa074d"],["/posts/1633036852.html","d5237e5ef5666954d9ab9cd46b41c5cb"],["/posts/1674202625.html","cf6fd3ada5dee1084428cbcc280b7c0c"],["/posts/1765123828.html","977b25992c8f5e64cc6895dbb60167ac"],["/posts/1767336200.html","bd4480a3b4541a72db22d2e69b0df304"],["/posts/1776114197.html","e734ba8e5c97f7568b7f7dcbf43a51c0"],["/posts/1817748743.html","1e6a56526e9aa5c55798aefffa5063dc"],["/posts/1925125395.html","5cc7599511bd1c71ecb2647f94c8b639"],["/posts/1966191251.html","653c834d7f20e8f0b3beb87f7b984e3b"],["/posts/1987617322.html","b41ad35b0c60618564a59e0d625e0381"],["/posts/1999788039.html","590608694f5a057e63b8f45025376dd5"],["/posts/2075104059.html","0a4f8339b0b51f9c8a46255da3c56f2b"],["/posts/2087796737.html","1cae67c0574de2f57359e6001e476345"],["/posts/2106547339.html","6c7cc6ca5b1fa9a6df40c345969acf5e"],["/posts/2207806286.html","f66d3270ab164195329612bf688ad907"],["/posts/2225903441.html","f8cbf8e3447347069b376b38224d2bdb"],["/posts/2265610284.html","9e025cf032bd84e01725c645b1239eb5"],["/posts/2281352001.html","40107d278b16ebd0daf782be8edbda60"],["/posts/2364755265.html","bf93dac89205057bb733a31092d50707"],["/posts/2414116852.html","3f06444744b824d56501caf25275ee7e"],["/posts/2421785022.html","8923e161fe16455a5d44fec98ea6c9af"],["/posts/2482902029.html","f050871545ca60c55d4ececd2073e95b"],["/posts/2495386210.html","fd33a740536ffe6acdba52e042b4d662"],["/posts/2516528882.html","24d1aeafd940d5aff2bd16a8d963c66d"],["/posts/2526659543.html","7c4c938cda3cf3ad1fa28124f0d977f8"],["/posts/2529807823.html","c996dab8f340c244e07e1afd6376dca8"],["/posts/2596601004.html","001b2e6be292c0429456d5dcdd417324"],["/posts/2742438348.html","07c997af07ec05869b167f3fb1734848"],["/posts/2864584994.html","81aef53826a65c046b5bcb8f8d1d76f5"],["/posts/2888309600.html","362d0c79abb667f2940926f0a38a623f"],["/posts/2891591958.html","864ba286815f97c337c74b6c47d4c076"],["/posts/2909934084.html","9861346e7f5f7841c3a3f4829125bbf6"],["/posts/2920256992.html","d76eeaee2da1cf6538c52f924665b032"],["/posts/2959474469.html","6943ebb7fe95fd6d9ec6395e18b3e6e5"],["/posts/3005926051.html","6f4ea950dea1742b2bc24e27300cc545"],["/posts/309775400.html","970a85ecb93ba7d15c3900efcb4ae70e"],["/posts/3156194925.html","07ad15411d4a4b88af30a5510e5e7f38"],["/posts/3169224211.html","5f7eb111751ba96bec2a3cf9910f5dda"],["/posts/3213899550.html","b073d81690f8df75b3ba1afca39e7a3a"],["/posts/3259212833.html","6735264d6fdc640503fad84fb11a88e7"],["/posts/3266130344.html","05e40cb7e632e6c0880f6d736bb13333"],["/posts/3292663995.html","9880a8fde3e2df5a5fb534902acf357d"],["/posts/3297135020.html","20becd258c00ee6663e30a9a7682d899"],["/posts/3306641566.html","73589f16d82aaacceaba6e5493adbc2d"],["/posts/3312011324.html","ca6c3a71077fe4772e448e4376b6456a"],["/posts/336911618.html","5b0adb0db416824a159c978b4fd682b9"],["/posts/3402121571.html","75a35db8d33de2842ebf8eaca56a93e1"],["/posts/3405577485.html","1b46d20c684cc2a8ac866a72b4d72156"],["/posts/3498516849.html","cba40a14fc9235d75d93862144a6b9d0"],["/posts/3513711414.html","9be4769769203afcc26e6d64e49267df"],["/posts/3546711884.html","6f44e60ee89e8b1f86d16032441725b1"],["/posts/3731385230.html","8f72ee0a014cb0cd03da689fd5b0ee4d"],["/posts/3772089482.html","3a8a917a114858e21368fa9c41a32b58"],["/posts/386609427.html","274d369fb2dfa33633887a05b69dc8e5"],["/posts/4044235327.html","6f93dd74b31b0e3aaf9b44cd1a051cc3"],["/posts/4115971639.html","25f42862badbb661d52a70ba1118a656"],["/posts/4130790367.html","830ba4233c4c2e873abd15e8a6b78e8c"],["/posts/4131986683.html","41b79a569a01b60c4c4bde7bfcb7476a"],["/posts/4177218757.html","11d70a0f11604d611970962799f53bca"],["/posts/4192183953.html","8fcfc8e5522a81febc282060381972ce"],["/posts/4261103898.html","148727bd259366bcf77f867e195c2d63"],["/posts/469711973.html","9e45b205735cb21bf3020a99e4330bf6"],["/posts/482495853.html","ab80bb6d1fb178809b5e52bd86d09084"],["/posts/488247922.html","1d9d3b7e10c71f416d4dd9558772fd16"],["/posts/517302816.html","3ce93050c2ef91b29cfba9afc68b02fa"],["/posts/570165348.html","9f8789ff222d1fcfe0a7a67fc532c54f"],["/posts/595890772.html","3b6d3753b8330fade282ac394edfbb61"],["/posts/67485572.html","18a018278adaee868e06d5fdd1868bfc"],["/posts/694347442.html","454220b7e050202e811191ee92d024d2"],["/posts/707384687.html","983c02f30859f0f4e843ad59a8afb7f2"],["/posts/71180092.html","b71bc9305a4e2e926885e0ff42ffdca3"],["/posts/716459272.html","2387ca1cfaabaeb2ec36b6c6d8cf6bcc"],["/posts/765481613.html","e6ab327761f4bdf276b4939504835c64"],["/posts/778231993.html","54257f33d549a4f6c752a694be2dca3e"],["/posts/795397410.html","18bab38ff418fdd87790922738cdd3f7"],["/posts/820223701.html","04cb1e8f13240512a4235700f7c65830"],["/posts/830372185.html","a7a12e21c51b9843b80eb431ad575ae2"],["/posts/88294277.html","6bb84e64a4cd0b29758e31bc8613bd98"],["/posts/939963535.html","ee82e8633158d548d3b89474ce63dcdc"],["/posts/983786067.html","e8a2cff0d3949d9c6bec5927fce69186"],["/sw-register.js","24921ea677427ca65ab19950287bfaf9"],["/tags/C/index.html","5adc045d338e952a64088e12cc1cbbb4"],["/tags/C/page/2/index.html","0cee12cfaa840d21be5a9b2fa7d02236"],["/tags/C/page/3/index.html","dff1bcfe9c226d17763321638f62d8d2"],["/tags/ETL/index.html","19c2179a1821e1de8b590e89a577ee87"],["/tags/ElasticSearch/index.html","c371c383a02e7eec2ca0514aaec7c7ed"],["/tags/GUI/index.html","0db329fb277b69240cdc3ede79cee1f8"],["/tags/HBase/index.html","dd15d38df145708dfe490d387e6ef7d1"],["/tags/Hadoop/index.html","4261d5329e1e5ab0b78b905aa545ec39"],["/tags/Hadoop/page/2/index.html","2af19581f3d479a7e9ba9312bf28f67e"],["/tags/Java/index.html","bed7161e54fd4646452b07d4d0dc99fc"],["/tags/Java后端/index.html","cc7a2d258823dad9d654af6b464081c5"],["/tags/Java后端/page/2/index.html","fcac2cbf3f49cef04e4eff9228955d5e"],["/tags/Java基础/index.html","011eaf6a8c43269fbd3cf59a8019a1b6"],["/tags/Java基础/page/2/index.html","a1ffc3d7417811e0a79cc1df0ed5134d"],["/tags/Kettle/index.html","3e9c939357e71c71ce9dba4663b245ad"],["/tags/Kibana/index.html","9efd36e4bf66c3e6f52472c7617c6a34"],["/tags/Linux/index.html","0d1bd2d701a28af91417b67eed942d6e"],["/tags/Linux/page/2/index.html","b262d6e8825b12c6a3c727ef7762f185"],["/tags/Linux/page/3/index.html","1c87fd0521014611284febdd1e6b5f9b"],["/tags/Mac/index.html","d71681171d24762e776295b2e4807c0f"],["/tags/Mac/page/2/index.html","ca498de4507c42c9e54ad1cd77ccc380"],["/tags/Maven/index.html","a8cfd7d8e262fdf5b6fbec1e5b2f211e"],["/tags/MySQL/index.html","5a60ff1c3be54df8a61b86aab3723d6a"],["/tags/Python/index.html","5e7284a4c50cd40b23a5fbd277df47cf"],["/tags/Redis/index.html","ac42416457ddf68ddd8da90596411d76"],["/tags/R语言/index.html","787f53fd7d353a825b86282737af6115"],["/tags/Spark/index.html","c4cbb9fec5e65aa91034b4200185fddd"],["/tags/Ubuntu/index.html","6e2eba1516ea2139f8eabaf73d601672"],["/tags/Vue/index.html","98b125653e80c0febf30e8a4b709733c"],["/tags/Windows/index.html","cd267e2b426d26361f605a44cf96c272"],["/tags/ZooKeeper/index.html","16fca748437dd9d9c54e159c7ded2b89"],["/tags/bfs/index.html","b1ddc3a9b2fdc13b17a443845f446b86"],["/tags/dfs/index.html","a9749477051b877c5255a01cd7d3a2cc"],["/tags/folium/index.html","c9f91a2e2b0439c24a1ac4cece387c66"],["/tags/git/index.html","70897d2694c1a9db8bec5744919ff6d1"],["/tags/index.html","479532eec358e33745c64eedf3b19a9e"],["/tags/latex/index.html","1fbdc9a74c73e20e09014c43ce1b7006"],["/tags/中间件/index.html","6d6a348727aff3a0af62a62c01d4aa92"],["/tags/二分查找/index.html","130eebd574f9cd79c9122a472fb94d0c"],["/tags/优化类/index.html","d5f53d3fd096ca96c66dab4edf8c2a0e"],["/tags/前端/index.html","fed343fdc39e4face277d9cfa77598a9"],["/tags/前缀和与差分/index.html","1be47d5f4291bd0cbd5aaec7a330c6ff"],["/tags/动态规划/index.html","4a4844faf6bdc3121c9526511615ca68"],["/tags/动态规划/page/2/index.html","b0e8287674fd8ffd85f3624f73689e67"],["/tags/博客搭建/index.html","32fefb0f3cfd8ea1878f3051647e63d1"],["/tags/图论/index.html","c44a978db3a780e215947dc0e927eada"],["/tags/大数据/index.html","ea30b46e00f17d58674927808bdd2d26"],["/tags/大数据/page/2/index.html","94c4c6809167a5324bd44c81f7b2f546"],["/tags/操作系统/index.html","47117cd47b640b265cefceb24bb977b1"],["/tags/数学建模/index.html","46c0c1b8615cdd04dd265cccf31f7952"],["/tags/数据库/index.html","8e87b3e5164ff2811d7361fa9278b178"],["/tags/数据结构和算法/index.html","78fe9255a048314a17ea64bc26689744"],["/tags/数据结构和算法/page/2/index.html","dea458b4b42ad8bef8298e85cd9b76be"],["/tags/数据结构和算法/page/3/index.html","230ad2210d3d3bf929e78d719d81d402"],["/tags/数组和字符串/index.html","ef4d0481367f27507a845e9d2b73296b"],["/tags/枚举类/index.html","f3aecd378b7693744f386a17d794efce"],["/tags/栈和队列/index.html","5b980ce0ccb705995dd0bb4e8c50741b"],["/tags/树论/index.html","5d6d22246e453f5a7566e1093eec78f2"],["/tags/测试/index.html","0333fa93a943588b525830cb8e89ccaf"],["/tags/环境/index.html","e179226f465be1b3ec7b487c0f40b7c9"],["/tags/环境变量/index.html","612f2bdc67f4f730182bf0d89016bb30"],["/tags/绘图/index.html","3989b9205510922f9bcbb32813adbddd"],["/tags/编程工具/index.html","a4cfc438ad367a082146960d91f3f371"],["/tags/编程环境/index.html","a76659977a7ba89f5cc807f8ee1ab177"],["/tags/网络编程/index.html","9539d046d033f28f24782624fecf4dc0"],["/tags/英语语法/index.html","3e6f2cf724c833565565ccf90426de66"],["/tags/计算机操作系统/index.html","c669052bbbfa52284e2cac75428ccd01"],["/tags/论文/index.html","a4015ab7f578b91a10d5a48532bab885"],["/tags/资源下载/index.html","db391a2a16aaee36afe35bb338267aac"],["/tags/链表/index.html","78d7fe4f482cb780aef3e8374de5e4c3"],["/tags/集合/index.html","a8ca310150ee7832d8676d4f3cef81e7"],["/tags/集群/index.html","794eb3902eb99121d13366c78d5ce6bc"]];
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
