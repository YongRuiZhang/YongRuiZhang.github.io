/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","0fe9fa5b42a9675fc13ecf5d55d9ec16"],["/about/index.html","8d043c637c1b71f7318bd8bc84acd0f1"],["/archives/2023/01/index.html","1a109856f8b33b0e6a4b1b59546d8b62"],["/archives/2023/02/index.html","8222a8e5885c1c97eb9bf8a8cb3d0b48"],["/archives/2023/02/page/2/index.html","17c2ffbcb1d8500ec6843d297c2c14ae"],["/archives/2023/03/index.html","a04169d11220532a51835c8d318f5978"],["/archives/2023/05/index.html","c9812c36ac2449282348b18068a375b6"],["/archives/2023/06/index.html","ed0c406d6da8bb48d267d1f7b38f7600"],["/archives/2023/09/index.html","51afb676935792b3ab452c26381c6a5c"],["/archives/2023/11/index.html","c9e5539f0f56b3c13aa2f6f08490fd0c"],["/archives/2023/12/index.html","a2545e9699d3f2cfc0607e1035354db2"],["/archives/2023/index.html","0e5b37c3c9b862e0a26ad6e3cc4ad563"],["/archives/2023/page/2/index.html","5cf5127b7a21df47d74b646f985e2f92"],["/archives/2023/page/3/index.html","6358d99964e8d7fa070ba844ff4195ef"],["/archives/2023/page/4/index.html","7db5eb549de4e046a484cf9c7aeb5e31"],["/archives/2024/02/index.html","204c8c382673ea4cdb26c37d80e86b13"],["/archives/2024/index.html","3685343b5010cd0832576d4222a72f30"],["/archives/index.html","02ba2ce363183f4ecf4632f3416120cb"],["/archives/page/2/index.html","3665b6d33f31ad4d15eca6dee4973f62"],["/archives/page/3/index.html","042b19edc581e051e67713de7b33ee3e"],["/archives/page/4/index.html","e24104785c2f3decc0c3992ca9402a66"],["/baidu_verify_codeva-qQP2iZOMLX.html","8393db3ad92882a40ee17b52b138123b"],["/categories/Java/index.html","8450f20b5464372d40b8b50801bbe2fa"],["/categories/Java/后端/index.html","4285334050606e2c63e009055e4edb38"],["/categories/Java/基础/index.html","a9d8de3d291d9ea6ea103730db1f2f1c"],["/categories/Java/基础/集合/index.html","084d086f2b817413286d4f44a3b0b2a7"],["/categories/Python/index.html","50ada196306a8a2294ccc6788ac8afa5"],["/categories/Python/编程环境/index.html","18e6ecb566673377aa9a66bd08b9d4ae"],["/categories/R语言/index.html","a2f3827e80ed7f8b164f093f9b997432"],["/categories/R语言/编程环境/index.html","32d7e56ef9e961896972779593faa69b"],["/categories/index.html","b654c0ea6ff62903b1b01cf7bd87662c"],["/categories/中间件/index.html","ec3eaa91bfc92b804173dff2ee9ffc01"],["/categories/前端/Vue/index.html","3a0e37bbbe945b2429353a4ebc5bd42f"],["/categories/前端/index.html","326c854ecefdf7682c53b8289f75cdf6"],["/categories/大数据开发/ElasticSearch/index.html","0600d9b871c2b058f7006bafd63825d3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","6254306ec97943d5d2ec4e5da2661204"],["/categories/大数据开发/HBase/index.html","fb0a71e6ba78ae1ec8157bc444838ba5"],["/categories/大数据开发/HBase/学习笔记/index.html","81363b93d827be1891c576c37e00ab06"],["/categories/大数据开发/HBase/环境搭建/index.html","58ec048e605bd3cda2822dc9a29f23cf"],["/categories/大数据开发/Hadoop/index.html","4a071ed4e9533d9bb1b3f8ec0c439e2f"],["/categories/大数据开发/Hadoop/技术/index.html","1dd617ef0515db78c7343199f1e86e68"],["/categories/大数据开发/Hadoop/环境搭建/index.html","4d334fecf4d04199d061e734759671f5"],["/categories/大数据开发/Redis/index.html","3c38566c5a2c9d425adca1f5bdd040bc"],["/categories/大数据开发/Redis/技术/index.html","262039caf302adb1b1e84d4316efe6ac"],["/categories/大数据开发/Redis/环境搭建/index.html","055b03a13e877a5d1fd6e4c752e7269c"],["/categories/大数据开发/Spark/index.html","d43b1a74a12db101bbc9464b60eb81d0"],["/categories/大数据开发/Spark/环境搭建/index.html","4dd929e1e6c1e6438ce3d26ccf6f0a7c"],["/categories/大数据开发/Zookeeper/index.html","84672b9db56ef2e512c6a8429fdcd500"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3ba5154e7e2b815367540ec889343ed4"],["/categories/大数据开发/index.html","525e80d0c143c1d425506293de473bf0"],["/categories/学校课程/index.html","503ef5bd31d157c00573723f3c81285f"],["/categories/学校课程/计算机操作系统/index.html","3f014ebf4c08049c0a2da58d6a09fd00"],["/categories/操作系统/Linux/index.html","f7e4ad997c0b5aba5b680b819a6f8ddb"],["/categories/操作系统/Mac/index.html","afb0927fd860278228ca4fcf64810672"],["/categories/操作系统/Windows/index.html","7853736e03b57e55be808e5c02168657"],["/categories/操作系统/index.html","8c66d1207f9bb2ca3443eefc90780dc6"],["/categories/数学建模/index.html","7101b8937961f2edc774b641b8caf6dd"],["/categories/数学建模/latex/index.html","d3652c1167e98eaa520bd87ed4d1de60"],["/categories/数学建模/优化类/index.html","3f06d3d89741243c8c8a536d69f243b0"],["/categories/数学建模/优化类/现代优化算法/index.html","53c39ccfb052bceb04e4f2817210db8b"],["/categories/数学建模/优化类/规划类/index.html","59d2e92e091de3245fbd2d3aab8373c6"],["/categories/数学建模/绘图/index.html","39421df718ed2956ff5bac1bf257921e"],["/categories/数据库/MySQL/index.html","b4d750057be5456a29ef6992e5228348"],["/categories/数据库/index.html","07acb33b1949342e76a4c2e50b4a48e7"],["/categories/数据结构和算法/index.html","b5f373e67ac0ced91439dfa568b22712"],["/categories/数据结构和算法/page/2/index.html","d075190b58222d8b0baad0cdf741d389"],["/categories/数据结构和算法/基本原理/bfs/index.html","146462189bf58cd61d22f04149fb7ad1"],["/categories/数据结构和算法/基本原理/dfs/index.html","91b8401ca7f98d6a5d14b3b0fb46f723"],["/categories/数据结构和算法/基本原理/index.html","4d175ec234d24b58ccb354ad868e10d2"],["/categories/数据结构和算法/基本原理/动态规划/index.html","892015b61deded6d164b468f7756bf5f"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e7b970d36daf40fc81cd3373c68c76f4"],["/categories/数据结构和算法/基本原理/图论/index.html","a2ad06bd3b8cc5b8ea009c48562ba170"],["/categories/数据结构和算法/基本原理/字符串/index.html","03eb27cc7d5d56d9f7937f0390d9773a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a702e60b5f1f44450a657f45b37478b9"],["/categories/数据结构和算法/基本原理/数论/index.html","ae0aab7398e5d05e279a6b310c1c4f4b"],["/categories/数据结构和算法/基本原理/树论/index.html","d0d7ae2431a2654f38de48a993f1033c"],["/categories/数据结构和算法/基本原理/链表/index.html","0a790eff039001751dd3d72f19bd3d92"],["/categories/数据结构和算法/算法题/index.html","b4eee18a44a0365567c9c7b60882bab3"],["/categories/数据结构和算法/算法题/二分查找/index.html","23fad3ea4ade68cbe8aafa87ad0597a4"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2710b72a72199d42cabf247a6eabda6b"],["/categories/数据结构和算法/算法题/动态规划/index.html","763af23ff8c8c81977e570a163543b06"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","080f240792a912c457ca463d42ea7192"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5dffcd21d1863d8ca921811ded05a425"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","131068e455b6a14bce2fbce4ce0d9b7b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","71039b65643f82b11e47098b8c63d68d"],["/categories/数据结构和算法/算法题/数论/index.html","6a450e1edf91b530470f00c797fda6f4"],["/categories/数据结构和算法/算法题/栈和队列/index.html","2829d7efd4df44baed396bff647982dc"],["/categories/数据结构和算法/算法题/树论/index.html","8a678b9c67aa09bdd95faa86d55acbc7"],["/categories/杂七杂八/index.html","e134559bff47a0d6327eb21f8e05341c"],["/categories/杂七杂八/博客搭建/index.html","8621c94a17acee84beb8a88569f7219b"],["/categories/编程工具下载/index.html","244acc18a2f12487d66645d008cad528"],["/categories/编程环境/index.html","512b558e20c4f1e322f60a4e576a26b0"],["/categories/编程环境/大数据/index.html","2b62c9df130ee9985820ce966dead8b2"],["/categories/英语学习/index.html","135c88625c0d0e940f9c9d33327985cc"],["/categories/英语学习/英语语法/index.html","1cc01963d0c20a264a88bf89e65dab5d"],["/comments/index.html","9c985f24e539f0bf3e97db20b25b5da3"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cd0966d90425ab2fce84669b3ccff68e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cf3f27591024d846fd024b4881a67886"],["/movies/index.html","5b4ee32c61ad41e5a64a9bb3b4f34e24"],["/music/index.html","3e18c8c87cc3e3212f76b1e72885666d"],["/page/2/index.html","31cfd72948d9b99698ae9fe52abb1118"],["/page/3/index.html","5f9f4e7761024d415e7406e9804634dc"],["/page/4/index.html","586f6e69e31adb2da09a0b3f39f287ed"],["/page/5/index.html","63a57927e161a05e73b6c55800fa93b1"],["/page/6/index.html","dc28d67e6a8eb7964355f3b2cc7737fd"],["/posts/1021360842.html","1de4391a4dc18a9535b254961dcd8653"],["/posts/1120620192.html","7cc978d37360d38bc59ab0aff4e5ae56"],["/posts/1141628095.html","844ca485e95d478e085593cd02dd9aba"],["/posts/1168613674.html","f6f582996824304ad8fd6b62b324d280"],["/posts/1219920510.html","d327291a902f8db45d14f5fdc550b35d"],["/posts/1222166338.html","495431dd0e65934ce926e83bd3457600"],["/posts/1259097482.html","305eb56058ede065f9be8d2c3357cedb"],["/posts/1271036369.html","dbb474979c73b51a21e32d921b210de8"],["/posts/1312847445.html","b0b92d05b666b74f8afe6599d30b81a6"],["/posts/135355774.html","31759e84f7004e03d78ee3200838a933"],["/posts/1375344716.html","1a583913a5b3b35ee010e7ba905494b8"],["/posts/1388991698.html","ce9ed0db125386c65b349216847ff092"],["/posts/1410315814.html","7541e884bd2be3b3272141bee3db6710"],["/posts/1452790229.html","45f05011c8758816c1c264ad1970a42f"],["/posts/1470079884.html","3606c01372afad51ea40b97b45bc6fa3"],["/posts/1470079885.html","58f3c7e33e50deb28331629260a7f3ab"],["/posts/1470079886.html","bcf12d5e15a8b53949274b21d166a514"],["/posts/1470079887.html","f78fb8351c0e03133a6f1f4a535ff762"],["/posts/1498536549.html","ba017445ebad65877cc32ae6922525db"],["/posts/1539568593.html","6f9012ee932797af5c7c1831ddb0e1e4"],["/posts/1547067935.html","d6db0bb64e2491f511ad30e405216f06"],["/posts/1557866301.html","e44e1052e75b758bddc4f6ad906c5f78"],["/posts/1571776361.html","bc23237fcd5f3b10bd18c6de9c37773c"],["/posts/1605124548.html","d130b97b35aff4d2e609f39afdbb07b0"],["/posts/1633036852.html","397d344247a20948911b0c4a96094f98"],["/posts/1674202625.html","14614d088d993e3e44c695e5b9434501"],["/posts/1765123828.html","836246d0fc99695782f019ba180a0a57"],["/posts/1767336200.html","625876c5ba1c29182cff88df36170355"],["/posts/1776114197.html","98641baa82fa02cf8b4c617e5bed2fe3"],["/posts/1817748743.html","26ab28a007524324a98215eba0e55704"],["/posts/1925125395.html","9b218455439902d04dfeaefdd18daaca"],["/posts/1966191251.html","9c6425330ce2659372b2591e138bcf6f"],["/posts/1987617322.html","f511b4c5d0d072dbb81eebfa174b0ed5"],["/posts/1999788039.html","5f2fccd2d13ad524c7288b9eec216d99"],["/posts/2075104059.html","72da10577a270dad242769a40d410e96"],["/posts/2087796737.html","b3b41fc4388e9c4e4ab34ec2b2b098a4"],["/posts/2106547339.html","b7d1de11741beec42cc2206745ccd568"],["/posts/2207806286.html","4b6902ff8c458acf621c3cc0ee989384"],["/posts/2225903441.html","517d29aefc5a32fc9a1f38dcf926bcf3"],["/posts/2265610284.html","1f7d692770fab7e65838b193c999fea9"],["/posts/2281352001.html","0fa8fa21c7f438fe4c4d17c81f112f38"],["/posts/2364755265.html","666ba7f3625d258d3b1f8926636bd2b9"],["/posts/2414116852.html","91afecedbc0ad8d755e01c45df4713fe"],["/posts/2421785022.html","2c66b270ad4d42a28f9ffd781aeea9c2"],["/posts/2482902029.html","9d830574cea461893e9cbbc02d5a7bfa"],["/posts/2495386210.html","7ef716302486d4bc2b20aa4c325e7455"],["/posts/2516528882.html","a3bdb811167d6637f97deb9364eaac45"],["/posts/2526659543.html","6730e4ca956bdd3d1cfcf67066894ef1"],["/posts/2529807823.html","1479d8322b515a3ebdd767141b3ffb47"],["/posts/2596601004.html","91045dbea0c2c297a8c8a1082cfccba3"],["/posts/2697614349.html","1470fb12ff8dc479654ea4f9ed803835"],["/posts/2742438348.html","85f60d9c846f8b7d19e83203e05e6dd0"],["/posts/2768249503.html","e046f55d94a8b82a7afdff6e5bff7738"],["/posts/2864584994.html","1d3833196c01ff87ebbc882008ad05f7"],["/posts/2888309600.html","b105e765af151c819091de69322bded0"],["/posts/2891591958.html","de9c36425e210c6db6f4aa062ca5e30e"],["/posts/2909934084.html","234eae37b92a7164d7d4d771eb1f4728"],["/posts/2920256992.html","804f6bfaf2d3e27068b6a4531429f52c"],["/posts/2959474469.html","51050e99e5d833a329aa610048b0a33b"],["/posts/3005926051.html","55d1aed9780c49a73c0b93dcdb02a01f"],["/posts/309775400.html","fb920d0cba0a41f9441cf95cbaa6e292"],["/posts/3156194925.html","f1fe47ebec00aa94d1acf31400dd1b70"],["/posts/3169224211.html","b981d521b9041e301c187bf9e11afb15"],["/posts/3213899550.html","68d7d12726d442e76f97e30e2b18c46d"],["/posts/3259212833.html","c899288a335d7c2bb40383668941b357"],["/posts/3266130344.html","bfc4ee24b88250da791cbeb42c9cad37"],["/posts/3292663995.html","125910846e8b03f207c4284ccebd0c9c"],["/posts/3297135020.html","727ab10ac9bc7233e433d97c8eab7ea2"],["/posts/3306641566.html","d4db8c182bfdd58ef6e2d8a9c6ace1e8"],["/posts/3312011324.html","a622806a6fac4d78cd237201c0b34c6d"],["/posts/336911618.html","c8e3150a13c2fce77f42a07f0a8432c7"],["/posts/3402121571.html","fc484662111cd30b1c4036e447a05b25"],["/posts/3405577485.html","fc798f2ea072e7f50ea59bb9a7a80376"],["/posts/3498516849.html","fd642e55e34611e3ddbf83124ee841a4"],["/posts/3513711414.html","b3e238a7b129a13db0443b1ed9719a5a"],["/posts/3523095624.html","a1b93aa0a4f5de19991a12f17ab905b4"],["/posts/3546711884.html","d96ff52f03e716419614c5f263142cd4"],["/posts/3731385230.html","b9adc36e5f7a2850949795c5c2602f01"],["/posts/3772089482.html","2879c6cca35b7ed2b36fd5aa375cba16"],["/posts/386609427.html","c1d387976620ed60fc3ba70b16ae4f08"],["/posts/4044235327.html","b2cc3d0ce3c9314f358fa46955187ea1"],["/posts/4115971639.html","e2350ee21cdb216adbe5863f5a25247c"],["/posts/4130790367.html","ea1ada17d8d6c280ed4b2119500759b5"],["/posts/4131986683.html","02cceca8622c58fba329b68a55628893"],["/posts/4177218757.html","d5dd34758cf7138446c6fc0c618b17cf"],["/posts/4192183953.html","fa4af03a7c0341cf386f0ed8885ffb3b"],["/posts/4261103898.html","14046df62ea041392e25cdba30f82da8"],["/posts/469711973.html","bb9c0685a362364ddb66abf8977f20d3"],["/posts/482495853.html","7dffbda973e28b0cda219ecfe52f5765"],["/posts/488247922.html","1b8f91897d4d4fb38b13f1eb8077511f"],["/posts/517302816.html","0ddb05c47b08d3372672c7139f8b8caa"],["/posts/570165348.html","6bebda77613daadca32bad3ec4b020cc"],["/posts/595890772.html","759b0f2eec7c8d38ede687916385e512"],["/posts/67485572.html","2ed725700097d996c6e69cde9f37f0d6"],["/posts/694347442.html","8c527f073980aab53882452ff84f3f2a"],["/posts/707384687.html","94e1fe028b74b30103611b519e3dd00a"],["/posts/71180092.html","9b1960629b6cd08772a18ac339c1c805"],["/posts/716459272.html","88d5b82f77cfcbb7fb8836e51e069f9c"],["/posts/765481613.html","548c8404686f27bf8a942b8e53f12dce"],["/posts/778231993.html","d8b2659f360761db369eda2238f68a8e"],["/posts/795397410.html","7fbe83cf8c393932155d46474a4aa561"],["/posts/820223701.html","9b46d616aef0a3d881ee210717b0a022"],["/posts/830372185.html","8883294ca080d60c26dfd29d3a23349d"],["/posts/88294277.html","ef395a4824e790595aa73512774f20af"],["/posts/939963535.html","ae1c8f73f7572a8f7eed1b98b0febf38"],["/posts/983786067.html","b24f726856c3d5e7efd49b9033c578f4"],["/sw-register.js","b7846f06e142b9e41c4dd01c6efc617f"],["/tags/C/index.html","bc525dd909276b37b657cfb2b12f0cc5"],["/tags/C/page/2/index.html","b9b9649411b2f7fa604279abc4c0e71c"],["/tags/C/page/3/index.html","08af31e231469e71e4bf8330a3b9ab89"],["/tags/C/page/4/index.html","62ac9d9424e3b987687d366bce4eed80"],["/tags/ETL/index.html","182f214dd7f86405f54f8a305730a18b"],["/tags/ElasticSearch/index.html","669c2c51b34389e5450c913953e768a3"],["/tags/GUI/index.html","df545ffa885d5080e6a3f01fd4b1c877"],["/tags/HBase/index.html","0731a48ebb149e3a5fac1de4933062a2"],["/tags/Hadoop/index.html","1fd7c3fa297fadb3aba686c73674ddaf"],["/tags/Hadoop/page/2/index.html","0e11816f1f51ef77be73872421591cf0"],["/tags/Java/index.html","5b09bef38cd0142146881aeba9e70561"],["/tags/Java后端/index.html","f9a7e0ef97a4645b7016cb0354d349f8"],["/tags/Java后端/page/2/index.html","bf1b0353d2ca7e63c1d4f2be7fd43569"],["/tags/Java基础/index.html","48cf3b53356d75b75c2d57cd75cc812f"],["/tags/Java基础/page/2/index.html","a65757828e67fdc3c0d7c4031b422407"],["/tags/Kettle/index.html","5921cbb9def8f866baec7d3b2d70ec43"],["/tags/Kibana/index.html","60f7bcd4524f95d1155cea02ec5008df"],["/tags/Linux/index.html","5a3b3fd698358681f6bf708b3257a3e8"],["/tags/Linux/page/2/index.html","2c36d068249bced623e3507fb5486b1a"],["/tags/Linux/page/3/index.html","43913c84db539a5f0d89dd7275cab751"],["/tags/Mac/index.html","e407647a0df2bd6346d9e4f41e46d495"],["/tags/Mac/page/2/index.html","486dba2f24d388da2247e2ebf38b19b3"],["/tags/Maven/index.html","5b31868be7dcacf9d8a717367675f00d"],["/tags/MySQL/index.html","d269fd59e07afa56f184b81487c54054"],["/tags/Python/index.html","30e8d336493818f9d984b56572d89bf5"],["/tags/Redis/index.html","41e0dfb132e46d5903236a23ee24a367"],["/tags/R语言/index.html","00d7b0a010743e758598043ae15d3f5e"],["/tags/Spark/index.html","825ba6fdc2378298dc10c91275a74cea"],["/tags/Ubuntu/index.html","e0bac2d74042d2a457f046aa19c22d7b"],["/tags/Vue/index.html","282d588fc188517a09db6bdf8213fcd1"],["/tags/Windows/index.html","98846d90a89d35e9b9b6d678d37b0ba9"],["/tags/ZooKeeper/index.html","646c130303b7f52198a6cf340092c3f0"],["/tags/bfs/index.html","f898e8d6847c6002a04b523b50d2002c"],["/tags/dfs/index.html","6e89132c951e2655afd2433321771884"],["/tags/folium/index.html","f0ce615a6093d0c0af97bf0a8c62cc1a"],["/tags/git/index.html","4856aa082b9b6c26f9f3b2a831c9b741"],["/tags/index.html","8a64f6ad6661118aa9ef8ce1e2c28539"],["/tags/latex/index.html","785ecba32be29aa6d08c5b6366d27b2d"],["/tags/中间件/index.html","92b42eaeb373d72210c016b00d31a4ff"],["/tags/二分查找/index.html","dab574276251f6cdff2a0c6d9f876b6d"],["/tags/优化类/index.html","58e6efd1254735007ba31dc69a4a7959"],["/tags/前端/index.html","77da00a87f9bbb4018af69db4bcc21ac"],["/tags/前缀和与差分/index.html","ac46712ee55cfcbf470d8481155ab612"],["/tags/动态规划/index.html","9dae0eca7604022853579aed3a73d6c7"],["/tags/动态规划/page/2/index.html","e0b2062c50ad0f6593f4830f0b6d947b"],["/tags/博客搭建/index.html","524f4eb69b2461b3c70443b7a838cc60"],["/tags/图论/index.html","5b42985976ea78a4ddbc8516909e5322"],["/tags/大数据/index.html","ba5b20ebcac8bcc3971ddce9ededbe7f"],["/tags/大数据/page/2/index.html","d579cd1e052e08fc37d112549563fc5d"],["/tags/操作系统/index.html","24d5e08788d582eddc4cc8d3aeecfe5a"],["/tags/数学建模/index.html","1a6ed8942576b60ddf8cfd36e1a58867"],["/tags/数据库/index.html","297e14e00a380083f813f03943b2a842"],["/tags/数据结构和算法/index.html","37849a96d527a73b7d9d977fe8c1290b"],["/tags/数据结构和算法/page/2/index.html","c234ea9d5e4ad0b6ff70ceafd5a76da2"],["/tags/数据结构和算法/page/3/index.html","451f06dc9f7893361de62159f73ee008"],["/tags/数据结构和算法/page/4/index.html","04426357ee999d622484db4b601a68bd"],["/tags/数组和字符串/index.html","fb140f811be7fc7d5e7cdaf50c2ad92a"],["/tags/数论/index.html","3e8ab3634c103445bd9a84d9eb7aea5d"],["/tags/枚举类/index.html","67ab72067c9e155d1880c5081b015b06"],["/tags/栈和队列/index.html","5cbc30bdbd08a0589a8f1fc48c258510"],["/tags/树论/index.html","03d3f0071eb898b121ce4348b021de0e"],["/tags/测试/index.html","bd96e00b72edf6f4940212e045c588ae"],["/tags/环境/index.html","303548b887912e4cb546b0fccd5a13a7"],["/tags/环境变量/index.html","f1b02398bd2c2d359c326ec8f49dd8f4"],["/tags/绘图/index.html","bafeaa8f53b28b620bed691b151da312"],["/tags/编程工具/index.html","0e7599896e4eaf0fa1ec86c02899e443"],["/tags/编程环境/index.html","bc1b39d452a5bab6fb31182d08d7bc50"],["/tags/网络编程/index.html","cbcf0f0bd4eec2ea879c9ef93def2fd7"],["/tags/英语语法/index.html","be12976348966a278b0721527273e1c7"],["/tags/计算机操作系统/index.html","92e574199420c3146907913a93a43fbc"],["/tags/论文/index.html","d2eb175613440657a4cf8b16991d4a22"],["/tags/资源下载/index.html","ad20bfd9e9129f2fa13835791bb924e8"],["/tags/链表/index.html","fe91077c4975018b39d19bb40d47f395"],["/tags/集合/index.html","43ffb7469e1ffcf5f8591d353e95c215"],["/tags/集群/index.html","4a31ca9626f3571b21dfec906ea77821"]];
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
