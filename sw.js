/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","dd7f868d17b3bc836990210693212ff8"],["/about/index.html","5fad74b84bcaac7c3e181a29925d1a88"],["/archives/2023/01/index.html","025aa00ffb9c6c49000546defe38534b"],["/archives/2023/02/index.html","261a8614b4db56d60408efc7f9787555"],["/archives/2023/02/page/2/index.html","198a93478b816cca8ce76aced10a8594"],["/archives/2023/03/index.html","a15e3c6aed1de93b6f0a27a457f51258"],["/archives/2023/05/index.html","05c72eeae808de3dcabc7638514a2443"],["/archives/2023/06/index.html","d8e9c0ac9d98e3ae6ef96cfc67a64fcf"],["/archives/2023/09/index.html","1552dc32fc25f165e7387940c0c85ac7"],["/archives/2023/11/index.html","8311d88149154405fbf80845ec292100"],["/archives/2023/12/index.html","08acb3ad5da2d097069279e288765d0e"],["/archives/2023/index.html","c09e6a3aa8a5002cfac3fa4d60d19944"],["/archives/2023/page/2/index.html","8594a4618c21a1ca44ace864e237992c"],["/archives/2023/page/3/index.html","57b9da711aa634b10959e2aa838faceb"],["/archives/2023/page/4/index.html","418369e5a00e4eafb60cdf3c47bc8266"],["/archives/2024/02/index.html","63e278bf49c3889f8c160d753f711518"],["/archives/2024/index.html","e13f586ed788b1733151161300d7a2c0"],["/archives/index.html","27a317c6ada5d9e00dc54e346e03af90"],["/archives/page/2/index.html","5f15357f3d14ade8f65a991b360db01e"],["/archives/page/3/index.html","9dc4c8bb5aa564612007b6f068ec92f8"],["/archives/page/4/index.html","721ef72be1a36e677f84e247a2d223cb"],["/baidu_verify_codeva-qQP2iZOMLX.html","cf1742420b1b69628e230ec368df5d8a"],["/categories/Java/index.html","bf0b7726be1ae4850c239129a6ea5126"],["/categories/Java/后端/index.html","c750dd397b81522fc2c9e154d5dea9b4"],["/categories/Java/基础/index.html","a9a3bc63ac4c9e8199db21f7749926c3"],["/categories/Java/基础/集合/index.html","a526ffca76fdd717b1e778c8575852be"],["/categories/Python/index.html","ca4eaba56fd69e1fc8f7be3ccb3832d0"],["/categories/Python/编程环境/index.html","56511d4644c07fa2eb5ddf5fc72e333d"],["/categories/R语言/index.html","40b425aaefe18d3a76b1c38df8786d6c"],["/categories/R语言/编程环境/index.html","f595e0a05547570b1e7908f5c47e389d"],["/categories/index.html","cab192c41f5772fa20bfb060d201ee28"],["/categories/中间件/index.html","202a3598cc461f7255940da3d2946f02"],["/categories/前端/Vue/index.html","cbb6bdd69c2366aaf50c830fe25a0076"],["/categories/前端/index.html","68be52fe6ef5bd07f4fa841312936c5c"],["/categories/大数据开发/ElasticSearch/index.html","876bb0670e6a600ab9ac6b9e6293d57f"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","64d57802775c0e80802921d73a6a4552"],["/categories/大数据开发/HBase/index.html","34e6c3dc1b0e69067e3212c88dcb5112"],["/categories/大数据开发/HBase/学习笔记/index.html","f91c1903a4469d949bc3202e70daacc5"],["/categories/大数据开发/HBase/环境搭建/index.html","7ed1a24b1e6ecbdb0369939f6b5926f0"],["/categories/大数据开发/Hadoop/index.html","47e17710f807ce2aae9bee45d0894373"],["/categories/大数据开发/Hadoop/技术/index.html","5d64173455224e7d49779a0aa9976b7f"],["/categories/大数据开发/Hadoop/环境搭建/index.html","96f166b1f02ea49385fa727712fe27b1"],["/categories/大数据开发/Redis/index.html","918c6d5a43de8bcb4c3d18dc7582c9b6"],["/categories/大数据开发/Redis/技术/index.html","90b271a6ad4fc640c66704b00f7b3add"],["/categories/大数据开发/Redis/环境搭建/index.html","6cfe3fb61a3f9d62c88a77443e3197c7"],["/categories/大数据开发/Spark/index.html","2e5c0a216565fd1d202c2a1f1fd2ee3e"],["/categories/大数据开发/Spark/环境搭建/index.html","1c602706e5315c86cc05153662fecaa1"],["/categories/大数据开发/Zookeeper/index.html","462f3d23b05a1cc224c28234b07d1042"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","00289b74bcab2693d5cf2e2e71ad1574"],["/categories/大数据开发/index.html","88187c3cf2709bbb09656302c816576d"],["/categories/学校课程/index.html","c4c46da964f40409c0dc0d4c620fe185"],["/categories/学校课程/计算机操作系统/index.html","3c73ed99fb0affa7fde300a95b5715f5"],["/categories/操作系统/Linux/index.html","cc09a521efda534cef3f72609f7d895b"],["/categories/操作系统/Mac/index.html","d2275e47f111da8e2eb5d9eb3f6e0fc1"],["/categories/操作系统/Windows/index.html","37b349c811785b09f6f9651c22c883ef"],["/categories/操作系统/index.html","7ce7acf665acc3fe5da96df6f975c5c0"],["/categories/数学建模/index.html","a0c7b71c5d6340a0ac33e53185168224"],["/categories/数学建模/latex/index.html","371d4c4405fcd2c3bae9ebcc26112ce3"],["/categories/数学建模/优化类/index.html","b23805877f7796bc6e719811cafceee3"],["/categories/数学建模/优化类/现代优化算法/index.html","ba7f897ab93f4050a1ff0cc8bcdacabe"],["/categories/数学建模/优化类/规划类/index.html","2030006a0753eb3953011e052f2dba16"],["/categories/数学建模/绘图/index.html","1c9d422160241b76a6a127af0029cd63"],["/categories/数据库/MySQL/index.html","e2630acd410b4fa29dfdb09f3889414b"],["/categories/数据库/index.html","2c0f22c0ef5e4c8da8b5b45ef00c9258"],["/categories/数据结构和算法/index.html","26ca78f94c78cdf2ac64f7ac25842935"],["/categories/数据结构和算法/page/2/index.html","6b775fd847459ed0f88a4a527b53461b"],["/categories/数据结构和算法/基本原理/bfs/index.html","606c37b8078a30558914560b7ebff384"],["/categories/数据结构和算法/基本原理/dfs/index.html","24818a918613b1c06f2667783ccae9c7"],["/categories/数据结构和算法/基本原理/index.html","d07ff9cdb36718f451f22117cbac7e6e"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9fcc97db454f2452a0c8d0d5afe09fa4"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4deeaa2a8ab653528bec962527927c3c"],["/categories/数据结构和算法/基本原理/图论/index.html","282be6863a0ffb41e97b78cec63c8078"],["/categories/数据结构和算法/基本原理/字符串/index.html","fe272ac5760d7e9032ba3a1e9412172a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","672ee7b94f61a98672e5626c1eca97b3"],["/categories/数据结构和算法/基本原理/数论/index.html","f43df33782c7901d0b3bc10649c29309"],["/categories/数据结构和算法/基本原理/树论/index.html","b3d5d2f2d42b991c7edb0c320f3c6b78"],["/categories/数据结构和算法/基本原理/链表/index.html","2314ee2f154d7b238ddaaf97f216b898"],["/categories/数据结构和算法/算法题/index.html","45306bc72e89b9500ce67fadca007ae0"],["/categories/数据结构和算法/算法题/二分查找/index.html","66fe8db55a15a68b4c6390c47c0979db"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ad9365ad0730050a0ed3b0b880efa7bf"],["/categories/数据结构和算法/算法题/动态规划/index.html","af0da7deee4892b0297fa9d006244d4d"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","ec85e36c21f06ed356b7b581aa709bac"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","6366aea621d3761fa89ee4dad75dbf90"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","804b4b057ed5fbf3a48c8e0c33a0b302"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","dd28a9fd54e51fbad1e0028db410cbd7"],["/categories/数据结构和算法/算法题/数论/index.html","b8a29d89a7a5c916d163b89d16d2b459"],["/categories/数据结构和算法/算法题/栈和队列/index.html","115b51e9a88e9df8f395d1abf5fb0261"],["/categories/数据结构和算法/算法题/树论/index.html","5a3e75b49dbf6b1c78da87c677dbf242"],["/categories/杂七杂八/index.html","eb329ae39ce7dd816eb0d57811ddf86d"],["/categories/杂七杂八/博客搭建/index.html","f00f918ce0fb546dcaf5a24946734431"],["/categories/编程工具下载/index.html","0671db894f9dafaa50a952f4a8b54783"],["/categories/编程环境/index.html","77200a9c2500a8781e01f3663be9f9d2"],["/categories/编程环境/大数据/index.html","5769262acacacf6562fb1f2ac1f2246f"],["/categories/英语学习/index.html","9af92c4729fc347e8ed6b64066967324"],["/categories/英语学习/英语语法/index.html","4ad7764f3e5286cb0d5b0c815df06e07"],["/comments/index.html","ee0584256a7db383e951d91c8f2da9f1"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ab5612a1e2807a6e2443430d9a653d77"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a84fdb737c6bdc4eece7daff6d393c6d"],["/movies/index.html","c41ae72ef430277bdd1af9cc8f7079d1"],["/music/index.html","d7ac6384b4a9d6ec1d716c57b0c13d94"],["/page/2/index.html","679d34f31bd7cb05ab1f1e4a89f326d3"],["/page/3/index.html","321a0a13076409d0f9d798efb8d5b043"],["/page/4/index.html","41a27754027d222df6016ace4b214384"],["/page/5/index.html","501fcd80de88973dd831cfc7bbdeb104"],["/page/6/index.html","5c128d978e284b317f75dc926117a709"],["/posts/1021360842.html","5b06a492c88a17177dec3749c9977dbc"],["/posts/1120620192.html","640eecbc0602e85505bfc26103b51416"],["/posts/1141628095.html","66ed2a391e0428e8eed1ca0fadba9d36"],["/posts/1168613674.html","51fa2d37f9d3cb851f1733d07a558e30"],["/posts/1219920510.html","bbeb5e9256022e15c9403be2bbf2fbac"],["/posts/1222166338.html","0e448feeeda2ed52736297443fb623ac"],["/posts/1259097482.html","ad936b12af2575bccce73b0455972d2b"],["/posts/1271036369.html","d24ebd7631a434bc90d4b895a9baf2ec"],["/posts/1312847445.html","6d8a6778a5541ca0e51fbd61cf94ef9f"],["/posts/135355774.html","a96c5c37e052aa49ded261ea9cfc65d1"],["/posts/1375344716.html","258457d4bbaabd9b53aca829a2b151e5"],["/posts/1388991698.html","e41448eae7dababf9cbd90376be33990"],["/posts/1410315814.html","99e6271aceaec60b874c7e40ac2872e4"],["/posts/1452790229.html","4881f722db2871b4e25a7d53d6fe1243"],["/posts/1470079884.html","278740c28b94552bfa92a804414cb8e8"],["/posts/1470079885.html","69bcf58520f5a105fb73fa47fc72b42d"],["/posts/1470079886.html","3847c9c88cf74da7bc30a6d69fe100e7"],["/posts/1470079887.html","1bb3267ed555b256d0cb5ea8b7c439bd"],["/posts/1498536549.html","8caecaee5c4d81d0a8a2cd28c0f1ba6a"],["/posts/1539568593.html","92cbb4c940d29ecdf5dc0266449be48c"],["/posts/1547067935.html","8787f33decbd18123d6f2b514a62c67a"],["/posts/1557866301.html","9c29d794eafddd86516a15ed15a63293"],["/posts/1571776361.html","86b23b4025d80282a63830ea69dd8348"],["/posts/1605124548.html","ddf256b1a9c3fc23afb6bff7a4023e11"],["/posts/1633036852.html","9932b4f796c656157b5045c53486b763"],["/posts/1674202625.html","8dc31c978d55c7b1b390a642144fac4e"],["/posts/1765123828.html","31f3e1e28a9216b2e2503549b080223b"],["/posts/1767336200.html","11629869f5d19e0eadcf08b931019ebf"],["/posts/1776114197.html","b3ebce785c82b74e89c89fe506f9beaa"],["/posts/1817748743.html","b17974f58a93b8facee2d4700f26f9ca"],["/posts/1925125395.html","86b2194daccecc75bc690903518585c8"],["/posts/1966191251.html","c7ab6935ac319bc7a0f74cb3931451d3"],["/posts/1987617322.html","d8b253fd17876d186b07d5b7994d1afd"],["/posts/1999788039.html","ae47edb4908f89abb93a8110b90cbcf2"],["/posts/2075104059.html","76d5fb98b5c7ac5fe36815094367cfbe"],["/posts/2087796737.html","a3d1a05e9ed5369e0b09c4a96153489d"],["/posts/2106547339.html","14234377e191ad54b9a6e6acc776e377"],["/posts/2207806286.html","f1c524afa288972d18c7694d832f40d2"],["/posts/2225903441.html","306534721966bc63cf023c68a8fca18f"],["/posts/2265610284.html","60f6667daf480368093be0f0e58ed9b4"],["/posts/2281352001.html","8b433a409b74f558ce09d1bcb7ee6659"],["/posts/2364755265.html","c554267ab5a0f2e78cee9ea191bfb11f"],["/posts/2414116852.html","c18bb6b6b5f56d29ad8877156d4899dd"],["/posts/2421785022.html","84feaae7975767434a37c3db27fdcbc4"],["/posts/2482902029.html","31f8d52db115568db989b34d0e8bcd07"],["/posts/2495386210.html","c7cab0b87434b0377c2aff18bc3aeef3"],["/posts/2516528882.html","b26dff3eda3fd05c0d2cfd714e796d89"],["/posts/2526659543.html","a441aac9c487513b79413ac492b2d8db"],["/posts/2529807823.html","d9136e3845121251649d1993e7ce4487"],["/posts/2596601004.html","f549eb059b9036828a4ee94a547d297a"],["/posts/2697614349.html","746a283131ab8388ac6fa3e23bff67af"],["/posts/2742438348.html","7257eb1f53d2745965f162673745844a"],["/posts/2768249503.html","47007dd02ba9c9cca898fd1813efa187"],["/posts/2864584994.html","c0625c7e042d38d722b387adf8b83709"],["/posts/2888309600.html","e7f14e8a693e1764a0990a39e6f6b7f7"],["/posts/2891591958.html","e1e194628f6e7ec3e49f36840b9f712d"],["/posts/2909934084.html","adb001b829dace3d00acc1c0b036aca6"],["/posts/2920256992.html","af168eef48e905aaf92e40b90948f524"],["/posts/2959474469.html","1758590c20186fe43eb743c3cd1e358a"],["/posts/3005926051.html","982b128bf488afb13dffc5f978b0417c"],["/posts/309775400.html","294c57c4d93e647ed3ca79b62d5ae022"],["/posts/3156194925.html","f9c3868b2f7f2e1a2bc901bdef7e5d50"],["/posts/3169224211.html","be15770b1f9ca5d705af6bf43fb48e78"],["/posts/3213899550.html","d2eb3c274dddf882e685fa732504e979"],["/posts/3259212833.html","63e5f3c3e4357c972ac0796819040fea"],["/posts/3266130344.html","906a81c1f23fd1410d458d9c79c13c14"],["/posts/3292663995.html","393ad6e8341865e6a2cb33c17f9b4eb2"],["/posts/3297135020.html","e80df8353ec18dc2c9fe01e27ff7bfaf"],["/posts/3306641566.html","dd8446e6695c2efe9d677ec466dc6e66"],["/posts/3312011324.html","09b4b75f6c74a9436103a87ed55433e8"],["/posts/336911618.html","9878d0b033458d1e8fc12f86e10b26f4"],["/posts/3402121571.html","6f06cdc2455a9b8fae6866329c97d11e"],["/posts/3405577485.html","2b4f11d01c4e8055e43eeb994478fa32"],["/posts/3498516849.html","0353b65af9019f212746fb4bf88d5b08"],["/posts/3513711414.html","e9ed533a013652a755a643bfb4a8c1fc"],["/posts/3523095624.html","1018c658f5dbc0a651eef05aa42ab815"],["/posts/3546711884.html","05641867476128e0154a7fd052294d44"],["/posts/3731385230.html","423ea22806aeccce26df615e435936f8"],["/posts/3772089482.html","074e15dc6bc038f541952c678972d3fd"],["/posts/386609427.html","acdb37e3fff993327a616d80bc3a7b02"],["/posts/4044235327.html","258ae83a4743bddb1698aec61532b6c8"],["/posts/4115971639.html","393ff276f1a0d4359e419cb09686c525"],["/posts/4130790367.html","11f67c6c5ce20d4e70c6fd294eae2d1e"],["/posts/4131986683.html","fc878233aa3e09da328555fd40112ba9"],["/posts/4177218757.html","9f1a0b95416a06e7bbac18e58198120a"],["/posts/4192183953.html","473773ef446b83076cddde11ef6890dc"],["/posts/4261103898.html","cd5e27894ee36dc7002b349a929955ac"],["/posts/469711973.html","00c3cdf0f311aabf5525e9e555987ae7"],["/posts/482495853.html","55bc0db0f0e42fb1291de3b4bb672e3d"],["/posts/488247922.html","87a3505d5a5b72fcb9c0fa55f3864109"],["/posts/517302816.html","e6cd289fa5bdec096030e012bc7f5c10"],["/posts/570165348.html","d0e61b4fe31d5acef606e5a2c9ef9e42"],["/posts/595890772.html","4a3bd36c085a4d4e6495abd938debd1b"],["/posts/67485572.html","52eb4e0feec016d56c0ad23489d504f1"],["/posts/694347442.html","8beaab16fae9fc92a751dc1bed65bcfa"],["/posts/707384687.html","e4d3605e0602eada1f3dcc283ff17513"],["/posts/71180092.html","4ee219f271cc1df4123145323ffda904"],["/posts/716459272.html","02b74bd0398b7d6a19ba3c9c0ab74ef6"],["/posts/765481613.html","4eb9237940792e09c33adbf4c78d27cb"],["/posts/778231993.html","50e897d20d0ebdcef7abad3643d9ba65"],["/posts/795397410.html","e2ef1fdd77c879406624674a1d31f355"],["/posts/820223701.html","2ae1a5ce6401ecbe82de4f59fe5c9e66"],["/posts/830372185.html","280c2abefb300c83e4c433d39ac728cf"],["/posts/88294277.html","71a0cc8b1641d3478a01756b1192cfcd"],["/posts/939963535.html","c0093e29e2d47065d70a6c774ad9e577"],["/posts/983786067.html","2429c4d9cf776e8917f69abd9903b955"],["/sw-register.js","cd48fc4ccc9be87780b1b56295817383"],["/tags/C/index.html","90e675da82b3f58a93299a017afeafc2"],["/tags/C/page/2/index.html","db3fcff5670ff685698c4810a2037a16"],["/tags/C/page/3/index.html","9e8ddfbdfffd753ed51aabd44f04fc03"],["/tags/C/page/4/index.html","ce40b2b83549691f53c6e904cd9d5761"],["/tags/ETL/index.html","2aa0197ff0f246225c512d6b6b48d09e"],["/tags/ElasticSearch/index.html","47e6e9d9727534e2b061be0b5637e70b"],["/tags/GUI/index.html","1fe0cd173044aafe362acfda8ec83bfc"],["/tags/HBase/index.html","740c6460f61b3ca3f777944b2c05e2d0"],["/tags/Hadoop/index.html","20f5615d94aa89554724ca112551d769"],["/tags/Hadoop/page/2/index.html","50440dc56fa6c54c7d205e7094cfa9b7"],["/tags/Java/index.html","45a38ae9894f3ee84c58f31ee2f87ad2"],["/tags/Java后端/index.html","6bf7c0ca245efdaa9374f8c7b57496b6"],["/tags/Java后端/page/2/index.html","196fba72cb32f0eab94819e208cbd34e"],["/tags/Java基础/index.html","41ebc83b76f4541a44598edee5392b7f"],["/tags/Java基础/page/2/index.html","e6d90edd8dc1b9422c8b15ef31136705"],["/tags/Kettle/index.html","fe6789563002220f2f3c892fe1f9ab1f"],["/tags/Kibana/index.html","f991697c5918cc5ff67958751d315187"],["/tags/Linux/index.html","295916c3bb94e5d9939f13dfcf79db17"],["/tags/Linux/page/2/index.html","ba7ea1ddce9d20e4cdd1fa5e3382d23b"],["/tags/Linux/page/3/index.html","f53b7e35b795f7bf256cf2db933f670b"],["/tags/Mac/index.html","c96ed089823d93d9c19c1070339915c5"],["/tags/Mac/page/2/index.html","fe6a625049138087383c2a453c7c2485"],["/tags/Maven/index.html","60fdb78cbd19ceacdeec5154a2edc103"],["/tags/MySQL/index.html","a887108666c715b5385f66540fc6071c"],["/tags/Python/index.html","4c1fc920ec3e1d874d6acc646a24f364"],["/tags/Redis/index.html","9a57dd7948928eeb9aa65f47ca5309d9"],["/tags/R语言/index.html","05756d48eb3481c651b968266cca00c1"],["/tags/Spark/index.html","17002db7a71cc071ef0b08184a668acc"],["/tags/Ubuntu/index.html","18574b70dfae5bc8a900bd2c3e29b1ff"],["/tags/Vue/index.html","ce223d9fc7e80e730482cb72fa890040"],["/tags/Windows/index.html","8f6ce912572ff76715ac33c793d3a9ba"],["/tags/ZooKeeper/index.html","dc7c01d3e30da2b199ce5c376228dad6"],["/tags/bfs/index.html","d2d567658c76bb84379c211f49596a9f"],["/tags/dfs/index.html","80a81b289192c961bb3a258b53d8ebd7"],["/tags/folium/index.html","3bf083508131dfcdacefad70c2e2623f"],["/tags/git/index.html","2dba1851c1b2407c85818e982ccd96c8"],["/tags/index.html","4f7091735ce70661a4a06582bf832d39"],["/tags/latex/index.html","2b925cfa4a1f6c2319b502d991d8196a"],["/tags/中间件/index.html","08261a9b44a9dce78a530a69dda20c14"],["/tags/二分查找/index.html","6cc825ba8bce732a4d77507c9c5c77e8"],["/tags/优化类/index.html","78dc4557fb89f550de9675160ff6f570"],["/tags/前端/index.html","022399199f32b35616154dcd1d16d2f9"],["/tags/前缀和与差分/index.html","3e6d8aed112a7a8e97ce1149e9578a57"],["/tags/动态规划/index.html","82b1c98fbc596805709d503e69ae6155"],["/tags/动态规划/page/2/index.html","d0e06db660f0c5a50ef656334b54877b"],["/tags/博客搭建/index.html","5434ff243c57313ef31f69a77477a663"],["/tags/图论/index.html","6c31f6ae489dd4862917b305b89cfa2c"],["/tags/大数据/index.html","f8d5df7cd2b233e9e214b7356726b22f"],["/tags/大数据/page/2/index.html","3cbc8cd0534890ab92f9e8e272d47ee3"],["/tags/操作系统/index.html","8746b292caaabcc232ed16921e566bd9"],["/tags/数学建模/index.html","c93bc6a3c1a9528dd715e2951df954cf"],["/tags/数据库/index.html","697a2cd4d0b98fe09f7d6b553c7c81eb"],["/tags/数据结构和算法/index.html","a4e94b414c4043eb368ae544155681a7"],["/tags/数据结构和算法/page/2/index.html","079fbca85d163523bc8235b515dee8e8"],["/tags/数据结构和算法/page/3/index.html","f98102222bc7aacc36260a139ae1945f"],["/tags/数据结构和算法/page/4/index.html","d19c3705a0988ce563ddd871d06db26e"],["/tags/数组和字符串/index.html","75474b0a10e00c828c9a9fc3fafa57b7"],["/tags/数论/index.html","497a0e14953884e5a2332c1188e17fa2"],["/tags/枚举类/index.html","7de582444cebc724fa0d3ee06185b02b"],["/tags/栈和队列/index.html","72fd3d8bfff08ded0bf58b2c8fb0dfbd"],["/tags/树论/index.html","f6be3f1d299fc9d2eb80c944447e4433"],["/tags/测试/index.html","23d342518f85e8d754a1f5f6a64e1221"],["/tags/环境/index.html","3bad85e63d18e6c0043c28ca7287246a"],["/tags/环境变量/index.html","eb3f6cf4acb56825e4d6081e7574f12a"],["/tags/绘图/index.html","dc1580ae2f636bca7903cf3d8eb10628"],["/tags/编程工具/index.html","2bf8f69526c57bdf065f453065b235b7"],["/tags/编程环境/index.html","008278dbc4b7af5bdaa994347d90bff1"],["/tags/网络编程/index.html","8e0578d9d396b3dcb8720f90c8678743"],["/tags/英语语法/index.html","cba2cdc1215efba24bfc531d78a536fc"],["/tags/计算机操作系统/index.html","a273da90ba95967ff3271be555c7f54b"],["/tags/论文/index.html","954f888a23c7666710a7cb418d4f5623"],["/tags/资源下载/index.html","0f6357ca41770fbf31b96f616675571a"],["/tags/链表/index.html","5c0b2b5f42155b81ee319e58f5a71dd4"],["/tags/集合/index.html","6977ae84724fe72c7523a691d9ea3bc8"],["/tags/集群/index.html","795c28ee1e5a33a1759af5bc79591527"]];
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
