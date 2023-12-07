/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","747bb66267afdbe68650280c9425c051"],["/about/index.html","5cc07c1790c21e8aaa4aacb14ec0482e"],["/archives/2023/01/index.html","43cd25a95517c9aadabfac2cb199195b"],["/archives/2023/02/index.html","4e0db945ca34ee5f99e27f1be931b3a2"],["/archives/2023/02/page/2/index.html","9597153778dcec88497d721ce69b3004"],["/archives/2023/03/index.html","28c29bb0098af3bf32eee377d7a478e4"],["/archives/2023/05/index.html","a54312737e63941d1f56e6740733180f"],["/archives/2023/06/index.html","1b16809892dbef9f4210ee1147ca05d1"],["/archives/2023/09/index.html","2b55822b6a5dc4cbd90807b1e53d2df3"],["/archives/2023/11/index.html","4da7cdd9434f9f4bfce1bf1344a37008"],["/archives/2023/12/index.html","b974d337b3c3408746fd5582d522058d"],["/archives/2023/index.html","32300b55d8aea0de31f1f583d5142747"],["/archives/2023/page/2/index.html","6d4ce3c18eb5e93dc88b457e3d47d3f7"],["/archives/2023/page/3/index.html","9a47cff6af687db488b1fe257570aa13"],["/archives/2023/page/4/index.html","fe9b09ce135166e4d3f7425fe426d6c2"],["/archives/index.html","9df8c80f40f0ccfc292818ad7f1c151d"],["/archives/page/2/index.html","8ce19843c718ec2bea7bd5fd8f7459b2"],["/archives/page/3/index.html","00fe38da85fd069224155db511e59eb9"],["/archives/page/4/index.html","cb6b628074ffb9f027d264e2262fbd92"],["/baidu_verify_codeva-qQP2iZOMLX.html","c579bcb9e1da877b49b19ac6a9b8a966"],["/categories/Java/index.html","860a453793c9a22d3348194d42af2b6e"],["/categories/Java/后端/index.html","37b15e2c87407803658b2d25b0e7a1d2"],["/categories/Java/基础/index.html","0afff6cf20ff6f9e61f3c9971e0c8d6f"],["/categories/Java/基础/集合/index.html","cbb43b8ec80a10a623c6c3433c1894f0"],["/categories/Python/index.html","e610f3568b84cdcfc74e4648b27c16f0"],["/categories/Python/编程环境/index.html","69f035a2ca818bc02adfc1185e6b764b"],["/categories/R语言/index.html","15efc9ee83cb3c919f5b8c416a5a04ed"],["/categories/R语言/编程环境/index.html","51ad56420beb098609f7848b6b0642a1"],["/categories/index.html","ca9a1dd26543e37bcf76efaf87cefa8d"],["/categories/中间件/index.html","a6d2bdca90d8c45156f6b46c43de36cb"],["/categories/前端/Vue/index.html","ca0360f7582191dcd68fd6684193d0ff"],["/categories/前端/index.html","64bff0f7794d6e4707533beb9e9b37a1"],["/categories/大数据开发/ElasticSearch/index.html","1a523d019c72f692358a30a530325d09"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2c4d3b3e54cc14010b670eca7bf88126"],["/categories/大数据开发/HBase/index.html","b7e083dad40bc79dc2a449acce55479d"],["/categories/大数据开发/HBase/学习笔记/index.html","bf0bb1ba2573da2622dbda53eeb94856"],["/categories/大数据开发/HBase/环境搭建/index.html","73a068d17fb2d2dedfaef1493fcdf500"],["/categories/大数据开发/Hadoop/index.html","cbb983863e85f896284d5b0787825c33"],["/categories/大数据开发/Hadoop/技术/index.html","67bd0a2b4196bfdfa82f1d0b6e784bbe"],["/categories/大数据开发/Hadoop/环境搭建/index.html","34bb7ceaedb893efd41cef8481881b4c"],["/categories/大数据开发/Redis/index.html","e9dc3fc6248a01fa6a9993c67aa09688"],["/categories/大数据开发/Redis/技术/index.html","555221cd9a1eeedcdc23d11c1127a129"],["/categories/大数据开发/Redis/环境搭建/index.html","e957d6da5c2dbeff1347e072c798cc75"],["/categories/大数据开发/Spark/index.html","09fe9b791a9a488bc027d9021aff8d12"],["/categories/大数据开发/Spark/环境搭建/index.html","6200eb1e2d86a9aef42882ef892945d7"],["/categories/大数据开发/Zookeeper/index.html","be495177bc51afd88fca6e312562e814"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9105baf9219563cbf29183cc0b13876c"],["/categories/大数据开发/index.html","4645bfd622621ecf76a27066e5ffa625"],["/categories/学校课程/index.html","17ff900071dfd5011d248487ac5b741a"],["/categories/学校课程/计算机操作系统/index.html","3d01225a160823b35f34745ae82d6559"],["/categories/操作系统/Linux/index.html","dedf3b00a11e6560cce58d086ff4021e"],["/categories/操作系统/Mac/index.html","8e63eda69c555c7aaf385d45f86ca050"],["/categories/操作系统/Windows/index.html","b19eab20aee89b722f141cee7168fc20"],["/categories/操作系统/index.html","bd62c43eb1e2cb69d49c3ec078b04d69"],["/categories/数学建模/index.html","291e3bc61df0ad36e9964cc9c9608493"],["/categories/数学建模/latex/index.html","7b4bc2429eaa024b72cccb2cfc9f6269"],["/categories/数学建模/优化类/index.html","3df84781580233e34bae8a711f87ee42"],["/categories/数学建模/优化类/现代优化算法/index.html","5affeac85b241a68f43c0a37651f472a"],["/categories/数学建模/优化类/规划类/index.html","e85a36370a6a246162b0b416746eeffc"],["/categories/数学建模/绘图/index.html","71bb315f09baeede59558a4169024ed3"],["/categories/数据库/MySQL/index.html","cdce1cb51083540615ac928105f8dd44"],["/categories/数据库/index.html","53ff746823e8d7f951ca9be146638ff4"],["/categories/数据结构和算法/index.html","ad5893a3ab15466c25d715f9246681aa"],["/categories/数据结构和算法/page/2/index.html","ea9c8593cc0cde7a57a1da7b1151e4bd"],["/categories/数据结构和算法/基本原理/bfs/index.html","a8ec7f6e2cc555afc6e2129b069eeeb2"],["/categories/数据结构和算法/基本原理/dfs/index.html","433380caf551a483afe53c3249dae508"],["/categories/数据结构和算法/基本原理/index.html","6258c4da645a6838d3c5cb06d1699337"],["/categories/数据结构和算法/基本原理/动态规划/index.html","37ba69748e79427ca2ba7cd952b8778e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3a95cc68e13a174aae62552ac92c2a99"],["/categories/数据结构和算法/基本原理/图论/index.html","3a52b3e74314c9a7fb9709c76ae6c0fd"],["/categories/数据结构和算法/基本原理/字符串/index.html","03f3697116f36a0130adfcb785c1b229"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","44f6afd47778f9ffe236cfc0cf9002bc"],["/categories/数据结构和算法/基本原理/数论/index.html","e5fb0a906f650185192e443f3124331b"],["/categories/数据结构和算法/基本原理/树论/index.html","57ec28b77a25839dd220d34b06b09fe7"],["/categories/数据结构和算法/基本原理/链表/index.html","ab9fbe9b7124fa63cfd42ddd6ae400b8"],["/categories/数据结构和算法/算法题/index.html","fd2bff0cf1ac8ae5ec67b954428418b4"],["/categories/数据结构和算法/算法题/二分查找/index.html","1ba4d9ba02a6564d22449777751023c0"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4e701296f8efe39f013c3a0c822a827e"],["/categories/数据结构和算法/算法题/动态规划/index.html","47fb2f18714000c70d9247841cd4856f"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","766d1fe3aca305234c62c00db17e5029"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","471f95e62b2772101101f1ac657d4fb5"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9b6696545bd30f88e9e3bb6c6125fd98"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2838ef22b935026d5dbf320aa5fd3a3f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","55e79074da4fce1abebb38e5172748d8"],["/categories/数据结构和算法/算法题/树论/index.html","0259d66ebb281111cf8b40e21f6301a2"],["/categories/杂七杂八/index.html","de1ac2c866e3031da9867f8b6cb34e49"],["/categories/杂七杂八/博客搭建/index.html","529077806b923f6deb908108576ecce4"],["/categories/编程工具下载/index.html","4a56593d7e975419f90a31fd64cabc9d"],["/categories/编程环境/index.html","8f95b801d256e5c7721396730461b84d"],["/categories/编程环境/大数据/index.html","db9a0f051b6b0cddd406f88728bd6de3"],["/categories/英语学习/index.html","f633702850fbab41076e5dc9874168ed"],["/categories/英语学习/英语语法/index.html","2748338e98cf766e63aa9d64149d3d72"],["/comments/index.html","4c706871559ced07f2363e71414c892b"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","45cdc1cfc8889599682742b345f55612"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","62be23d5a74f537f8e0098e228bfa774"],["/movies/index.html","515aa6bff51ff96778c0347bfc1f17a2"],["/music/index.html","f7de7e43c762de6d81e8a2860394e05b"],["/page/2/index.html","a2e62a4c8943d98a6248331de38a90ab"],["/page/3/index.html","75632a22a7381a1faf36f6e039742955"],["/page/4/index.html","c307479dd501620670e130f40eb7687f"],["/page/5/index.html","b599c1b2849094f6b811fbd1f1716b54"],["/page/6/index.html","3b569a77c87a0732c3a601a5d4181482"],["/posts/1021360842.html","75e27f2cf9a5c952776c06a0d76b6a8b"],["/posts/1120620192.html","9ba8dcf4f0f76d7f1a7b87c140ae8d79"],["/posts/1141628095.html","135b704247143f0ceaa10e54962f422e"],["/posts/1168613674.html","34b2458eda72d4a7667b025b33077977"],["/posts/1219920510.html","260f307b33211b4acc0d8b394e983bd2"],["/posts/1222166338.html","8313fca9cdd1dbdb9245105c02d2bb75"],["/posts/1259097482.html","e09ce336f63c393ec5c4a5f194cc42bd"],["/posts/1271036369.html","2871c33ec1244ac15f13ca13ec530287"],["/posts/1312847445.html","2660ce4597a7f08380c86126bfd01b41"],["/posts/135355774.html","3da63224346f04328a4935537e6c7594"],["/posts/1375344716.html","f59810c8568f13957f81228dfca58721"],["/posts/1388991698.html","00cfb5da82fb564efaf075c9ff338bfd"],["/posts/1410315814.html","c23d922ad299569486b3b1a9415aab46"],["/posts/1452790229.html","4b4657af31562b9507694aeaa7afe3be"],["/posts/1470079884.html","2c2b60c1691219edb64007d7dc201558"],["/posts/1470079885.html","f796f95cd8b38040f4f06114666bfab5"],["/posts/1470079886.html","df12afa55b2a01f077cc3212c43d37e3"],["/posts/1470079887.html","3cea6bd8a8dec42e7971ffa81ba821ec"],["/posts/1498536549.html","852e96d93cb3145f4ec5e5a86888dee5"],["/posts/1547067935.html","ecc3d1e1add50901b8e18b8bddd15cca"],["/posts/1557866301.html","f0011345145e14f5eb51e77af25237dc"],["/posts/1571776361.html","6981c612f9633bfefa3c5e98db785456"],["/posts/1605124548.html","40e4f1b5d8f521a346b3a0e6c7fa074d"],["/posts/1633036852.html","1913a51bce87cad54df4394683a26b15"],["/posts/1674202625.html","cf6fd3ada5dee1084428cbcc280b7c0c"],["/posts/1765123828.html","e899b8cbcf7e93f166560fecdb886ebd"],["/posts/1767336200.html","bd4480a3b4541a72db22d2e69b0df304"],["/posts/1776114197.html","55999917ad7587ce9431cf7185b231d5"],["/posts/1817748743.html","1e6a56526e9aa5c55798aefffa5063dc"],["/posts/1925125395.html","5cc7599511bd1c71ecb2647f94c8b639"],["/posts/1966191251.html","7f5450e3340c80afabd50a5ab782ba9b"],["/posts/1987617322.html","b41ad35b0c60618564a59e0d625e0381"],["/posts/1999788039.html","f8e18bae8bb5a448ceb59eb7baa2d68d"],["/posts/2075104059.html","0a4f8339b0b51f9c8a46255da3c56f2b"],["/posts/2087796737.html","1cae67c0574de2f57359e6001e476345"],["/posts/2106547339.html","10cb7419ce920ca154cdf32293b939a5"],["/posts/2207806286.html","f66d3270ab164195329612bf688ad907"],["/posts/2225903441.html","d7b22d657800e3792e78c6a16a3d1c18"],["/posts/2265610284.html","6a025cdd56943bc26269bb2185212986"],["/posts/2281352001.html","40107d278b16ebd0daf782be8edbda60"],["/posts/2364755265.html","bf93dac89205057bb733a31092d50707"],["/posts/2414116852.html","0e1ec281c14d47d14bf5b25592923dd0"],["/posts/2421785022.html","8923e161fe16455a5d44fec98ea6c9af"],["/posts/2482902029.html","0a9571c24b1387bb8050bfbc3eeafa2f"],["/posts/2495386210.html","fd33a740536ffe6acdba52e042b4d662"],["/posts/2516528882.html","24d1aeafd940d5aff2bd16a8d963c66d"],["/posts/2526659543.html","eef7c09d4379b5b7ef14c7efab6edac3"],["/posts/2529807823.html","c996dab8f340c244e07e1afd6376dca8"],["/posts/2596601004.html","001b2e6be292c0429456d5dcdd417324"],["/posts/2742438348.html","2f208e4b87ef87b0d9fcda1765fe1c7a"],["/posts/2864584994.html","4d143d4663c9b557f9116b8f8d70cf7d"],["/posts/2888309600.html","362d0c79abb667f2940926f0a38a623f"],["/posts/2891591958.html","88260673094a6e06e93a351a13233dd7"],["/posts/2909934084.html","9861346e7f5f7841c3a3f4829125bbf6"],["/posts/2920256992.html","2611f703657ce1326f66f33f87c0afcc"],["/posts/2959474469.html","6943ebb7fe95fd6d9ec6395e18b3e6e5"],["/posts/3005926051.html","6f4ea950dea1742b2bc24e27300cc545"],["/posts/309775400.html","e6870e722cc44c268c9692574989ac47"],["/posts/3156194925.html","9b14441e0c986bb44e26c4f8c3576be4"],["/posts/3169224211.html","f6e8017bf94832fc9b224449465a9872"],["/posts/3213899550.html","bdbadab42dc0b9e8f8bc8d2a5d0534b3"],["/posts/3259212833.html","0bd97f71d9fa6e8cc78559c38cfa8194"],["/posts/3266130344.html","26c5f5f9ba1fa8bbc3adb4efb466a9d9"],["/posts/3292663995.html","9880a8fde3e2df5a5fb534902acf357d"],["/posts/3297135020.html","8e9c7e2133294aaab9c3a990059ef4d4"],["/posts/3306641566.html","73589f16d82aaacceaba6e5493adbc2d"],["/posts/3312011324.html","ca6c3a71077fe4772e448e4376b6456a"],["/posts/336911618.html","6a6ce3570c4f4612dc3e7d4d518c9af3"],["/posts/3402121571.html","663524612fff0b3ab4947534916eeb3a"],["/posts/3405577485.html","f5e7a9d35390406b41b7a16682927dec"],["/posts/3498516849.html","bbfa700b292d3e10768eb8cf23751662"],["/posts/3513711414.html","9be4769769203afcc26e6d64e49267df"],["/posts/3546711884.html","6f44e60ee89e8b1f86d16032441725b1"],["/posts/3731385230.html","8f72ee0a014cb0cd03da689fd5b0ee4d"],["/posts/3772089482.html","3a8a917a114858e21368fa9c41a32b58"],["/posts/386609427.html","02621696467375281ac6824e39e6ba2a"],["/posts/4044235327.html","8996ba755e818f73bc08d7cb0e2d68cd"],["/posts/4115971639.html","25f42862badbb661d52a70ba1118a656"],["/posts/4130790367.html","830ba4233c4c2e873abd15e8a6b78e8c"],["/posts/4131986683.html","41b79a569a01b60c4c4bde7bfcb7476a"],["/posts/4177218757.html","11d70a0f11604d611970962799f53bca"],["/posts/4192183953.html","263fd33c4576288496ab0774ab7bee83"],["/posts/4261103898.html","42fe0055465f47b287ed815ee8b5065a"],["/posts/469711973.html","19d7c38e85fce11b8f371c844c645f67"],["/posts/482495853.html","ab80bb6d1fb178809b5e52bd86d09084"],["/posts/488247922.html","b6900a8c1f9e35960100b0525188cba5"],["/posts/517302816.html","86759287615e3cf0704504edd79cba2f"],["/posts/570165348.html","9f8789ff222d1fcfe0a7a67fc532c54f"],["/posts/595890772.html","3b6d3753b8330fade282ac394edfbb61"],["/posts/67485572.html","18a018278adaee868e06d5fdd1868bfc"],["/posts/694347442.html","454220b7e050202e811191ee92d024d2"],["/posts/707384687.html","983c02f30859f0f4e843ad59a8afb7f2"],["/posts/71180092.html","1d55b4f6b06ad2d46c956a78481e8a4f"],["/posts/716459272.html","2a58e94d78ff8f5b84a7bc9c294bee30"],["/posts/765481613.html","e4b10ae05fe9203b81f13f950489b3b8"],["/posts/778231993.html","626a95d57875b49bc04f7dea186088f6"],["/posts/795397410.html","18bab38ff418fdd87790922738cdd3f7"],["/posts/820223701.html","4257a381f02ab463f812bdba810c09ef"],["/posts/830372185.html","a7a12e21c51b9843b80eb431ad575ae2"],["/posts/88294277.html","2b4fdd87de26fbe174d4bac1afb72291"],["/posts/939963535.html","ee82e8633158d548d3b89474ce63dcdc"],["/posts/983786067.html","e8a2cff0d3949d9c6bec5927fce69186"],["/sw-register.js","ae35c93dbc05bc626382e483864e5750"],["/tags/C/index.html","ab8e95db0e37957ef1212a48512f515e"],["/tags/C/page/2/index.html","bc0ed09d44c87c1d68225c4855291c9b"],["/tags/C/page/3/index.html","0813c28f4a3965a53544b57f5ff6b9cb"],["/tags/ETL/index.html","99eac3c349a2ba97e3626aabdfe361ae"],["/tags/ElasticSearch/index.html","ca7e9d7dbdb076a16570b936cd27f157"],["/tags/GUI/index.html","8a9cff91d76fcc47ce7054164949475a"],["/tags/HBase/index.html","0b45a8bb22842b7ae27276e04721bff2"],["/tags/Hadoop/index.html","3dd7f63d484f3a9376cd2ba09dfe7e36"],["/tags/Hadoop/page/2/index.html","347153a4c24ce7a4176d18adca6cec6b"],["/tags/Java/index.html","05c0ab2dd58ec7587c7a50a0e0707306"],["/tags/Java后端/index.html","ac7471898d0bb18fac7aa51a2afd5ebb"],["/tags/Java后端/page/2/index.html","d8df5cefd4a3372b5393dbaf46e4569c"],["/tags/Java基础/index.html","4be5871419d4b2753d70c8c292878ca6"],["/tags/Java基础/page/2/index.html","8dd65bedfdcd5ca1e34fe0f431f0cb4d"],["/tags/Kettle/index.html","a0187d2049d3be8651d203169f5598ac"],["/tags/Kibana/index.html","93056762fec9377b3ad59f29d8ee2893"],["/tags/Linux/index.html","7ecaf88cd7c43118f3806332057ad774"],["/tags/Linux/page/2/index.html","1966538cc6e92fd4224fa633671ff580"],["/tags/Linux/page/3/index.html","b49df07644d4da06c179583599ff224b"],["/tags/Mac/index.html","e30d9976390cad2926c83d094778f49f"],["/tags/Mac/page/2/index.html","1f351e9b6e122575a8b1f71d0419a688"],["/tags/Maven/index.html","a46e3fa39c57c98e53bde8c6c06f0cfe"],["/tags/MySQL/index.html","b97c0a936f86eda549397521499463ce"],["/tags/Python/index.html","766b81b5caac58fe8d3c5a125d7dea30"],["/tags/Redis/index.html","fe87eeb1d4e4ea201c6fb21024150acb"],["/tags/R语言/index.html","459eda4819af5ec17e9b477a379f6388"],["/tags/Spark/index.html","337826647fcf019b0916143278ad9803"],["/tags/Ubuntu/index.html","8429e2df8ac0a306a2c3196d76995a60"],["/tags/Vue/index.html","727aff2fd9b0072df45f87e3b1fe799c"],["/tags/Windows/index.html","387016bb96d564ccf88ea693cab8e62c"],["/tags/ZooKeeper/index.html","eb04bb466203801e25501a30d78f3c7c"],["/tags/bfs/index.html","f235b96ec96210036c5de6172d66f0f1"],["/tags/dfs/index.html","127b3ef39373fe949d1760cf10d50c04"],["/tags/folium/index.html","a89ed7a7805600111b3692ca6c30c17d"],["/tags/git/index.html","1408f3c5e48b071a31ade1e227106d9a"],["/tags/index.html","f202bd68b303a4bc4df0cdebb924f82a"],["/tags/latex/index.html","ae4d1779236339698659b79fd6929a63"],["/tags/中间件/index.html","df30d4b8384b9ac3f4059e2f6ac73628"],["/tags/二分查找/index.html","44aea77a9b42ec4ac1db4cdb88838012"],["/tags/优化类/index.html","35c215caacc9a41c20325355f9525151"],["/tags/前端/index.html","bc6083025e23b5de4852761e92f9a40d"],["/tags/前缀和与差分/index.html","20d4fd27c8620c35c63795a61bb82f7d"],["/tags/动态规划/index.html","8b8c791744e8c1de3a0ab49829261720"],["/tags/动态规划/page/2/index.html","cf416b1d32ffdd4da8c33815499f459f"],["/tags/博客搭建/index.html","0f8caecbbf056f914530397c3a6fe04a"],["/tags/图论/index.html","5d4d6dbbda6dc1c29659b714a1f3621a"],["/tags/大数据/index.html","5479854ca79c58fb315a81b8b57f4b12"],["/tags/大数据/page/2/index.html","9917f1a8a1365814344b43408cc04c19"],["/tags/操作系统/index.html","a3583fcd677b2a5b9e30dadaa8648319"],["/tags/数学建模/index.html","909b2f030c2b72f7a3594ac914a98e78"],["/tags/数据库/index.html","3516137d3ba950b7d785f44266cdd6a4"],["/tags/数据结构和算法/index.html","d6b78993839d78013b23569dd384949b"],["/tags/数据结构和算法/page/2/index.html","256fc090dd6046b88f807fc956dcbe1f"],["/tags/数据结构和算法/page/3/index.html","50f6dcade483938e4076aec5d607cd8e"],["/tags/数组和字符串/index.html","357b1fef9268362d2b3e8b486b79c653"],["/tags/枚举类/index.html","a4bb627e473cb2dc244d7e3a6d4d490b"],["/tags/栈和队列/index.html","cb5294ab061de3cecac9962f8461adb5"],["/tags/树论/index.html","fddcd7b32ba657c8b0fb2e449785d479"],["/tags/测试/index.html","7818d02e730cc8ef5f86db6d2eaa04d3"],["/tags/环境/index.html","663df31aae4517acad81910ef043cdc1"],["/tags/环境变量/index.html","448a50f9778c55423381967b0c951bf2"],["/tags/绘图/index.html","3b80c5d101d1cdf82bbf2ee7e99845f5"],["/tags/编程工具/index.html","4fbcaa6d285aed818b1cb7094b5d2c92"],["/tags/编程环境/index.html","592ba791d991ebbd15a1a810f4abcfe0"],["/tags/网络编程/index.html","a593426aea1e038225dbb0d15bf52c31"],["/tags/英语语法/index.html","528acc3a1934407ac25a7237c9d49c20"],["/tags/计算机操作系统/index.html","b3748cca6a52bf03bcf5faac31fc4367"],["/tags/论文/index.html","aef540f3ec2115058608b72c1982233d"],["/tags/资源下载/index.html","7c170ba327654596b547d4a3bc287091"],["/tags/链表/index.html","c125f53f8f47cd10c1ebc8d7abf50517"],["/tags/集合/index.html","789c4202d104706f648311f1eb0e4d94"],["/tags/集群/index.html","299a4d67faf650c454a3d3365594d2e1"]];
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
