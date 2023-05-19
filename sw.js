/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","cf4c415fba3c16e93f732bafac240ef3"],["/about/index.html","07fe86fc125377701a9ceabb8cc74a7b"],["/archives/2023/01/index.html","484e221d18d7c0371727943de596dfaf"],["/archives/2023/02/index.html","e745c43b6cec09082feb4c4d2f22f687"],["/archives/2023/02/page/2/index.html","6ffa49eab8c8a302c1f5f021bc3a58a6"],["/archives/2023/03/index.html","0d35de60c7b883a56dd68b614780ae7d"],["/archives/2023/05/index.html","5a0df90a48a389151db3502e5e210d8d"],["/archives/2023/index.html","64559c26a4cb14c4a3c5fe6872b96258"],["/archives/2023/page/2/index.html","07f4ff0e0d1ecf41c23d5eb675c54c0d"],["/archives/2023/page/3/index.html","22479d7b2cef4f63d82470d0886d9f66"],["/archives/2023/page/4/index.html","8651408310c3577df24e0e75f22cd477"],["/archives/index.html","4ec5509cf3215bfef15ddbc683d0304c"],["/archives/page/2/index.html","fbf39bba030a2d6579a13d9353c20283"],["/archives/page/3/index.html","ba5a9c01c63e26e96325f5db9584c18d"],["/archives/page/4/index.html","fa1218afbb6004c5fb1ce23b74302e20"],["/categories/Java/index.html","05b06f9be5903843042a03658aa97274"],["/categories/Java/后端/index.html","19d153d70dd6fd56eef2b8bf25e86c74"],["/categories/Java/基础/index.html","ce2cc8623f1fdb07da2df0bdee892611"],["/categories/Java/基础/集合/index.html","d9e6e2b7af8826e544657d2c017ec344"],["/categories/Python/index.html","c8f425cf29325b193753f24d7933fba2"],["/categories/Python/编程环境/index.html","bebdbdbe799d64ef4814111fa1753bce"],["/categories/R语言/index.html","a9fb30681a44eef2ddd0c7a59399f2ba"],["/categories/R语言/编程环境/index.html","bd4689ecf9888ab9a3b2c68f913a93b9"],["/categories/index.html","2146d59e634cce04098251635faefca1"],["/categories/中间件/index.html","2330ab2bc05666474e7277c898ce3709"],["/categories/大数据开发/ElasticSearch/index.html","e310bae53c561bd211c451eb9c48386a"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2953207c8ac6330642f2770542714f64"],["/categories/大数据开发/HBase/index.html","8ffd6fa5ff029f569a89074fd492bdb8"],["/categories/大数据开发/HBase/学习笔记/index.html","174bc70898eada60f713bf20a69c1731"],["/categories/大数据开发/HBase/环境搭建/index.html","ddeab683b653fa1b95dccba86810219e"],["/categories/大数据开发/Hadoop/index.html","6b0362dd84f79024effac135cae95a3f"],["/categories/大数据开发/Hadoop/技术/index.html","62ae1de602ac7e781b5020efeeb5bd5b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","532346380df3e66c3e83130ccf48b0ea"],["/categories/大数据开发/Redis/index.html","d6b2edfea8d1b6c7d4c7000a7cebfb3b"],["/categories/大数据开发/Redis/技术/index.html","a4a959898472f2d9b12563ab4b06ec2c"],["/categories/大数据开发/Redis/环境搭建/index.html","442991582ad2a949f18e493abd5b7cab"],["/categories/大数据开发/Zookeeper/index.html","994854958f323461b7c0539a47df74da"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","07856dcbe3825c8fe47de803e8b7173f"],["/categories/大数据开发/index.html","2a78bf0ce97e3d925896e24cb80d568d"],["/categories/操作系统/Linux/index.html","cea57fb06f7a57c33a5b0bda87800a55"],["/categories/操作系统/Mac/index.html","a2ef6b3a36d5c4331a8b75fe959ce038"],["/categories/操作系统/Windows/index.html","53ebcf3efdbda08a6ec80d966676888e"],["/categories/操作系统/index.html","68af0dc8634566603290acd219ba3315"],["/categories/数学建模/index.html","0dfa4671324d8ece408a0645b12f9b13"],["/categories/数学建模/latex/index.html","5ed001a8d50c5def88ee058ca396325f"],["/categories/数学建模/优化类/index.html","c082170d00f438669917a5f3547fff60"],["/categories/数学建模/优化类/现代优化算法/index.html","ccb6ad3f5c173646c363a98ec976cc37"],["/categories/数学建模/优化类/规划类/index.html","c95b7a3d36b9b3d43976da160334c35b"],["/categories/数学建模/绘图/index.html","7381c0b45ecc30dd70f5c6da09a6dffb"],["/categories/数据库/MySQL/index.html","3523ff0c0fbd9ff8fdf67b406f6ee05b"],["/categories/数据库/index.html","ab72610056485905a338619740312875"],["/categories/数据结构和算法/index.html","1221110bc28912c88cebfe3d1b554f66"],["/categories/数据结构和算法/page/2/index.html","f5bfa8edec541fe2b73c6e69ed9dcdde"],["/categories/数据结构和算法/基本原理/bfs/index.html","83abb3dd85ca8ba0079bc635a61fa9e5"],["/categories/数据结构和算法/基本原理/dfs/index.html","eb40c0c4c04e80838ff75e5f4a592f35"],["/categories/数据结构和算法/基本原理/index.html","8b8e36ac39f842a5b0685857a30bf5b1"],["/categories/数据结构和算法/基本原理/动态规划/index.html","065d042d29035014ad09fa8a19926537"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","cd4c869461fccca138d0142eb31ffd8b"],["/categories/数据结构和算法/基本原理/图论/index.html","551baccf47bcf97fa290eae260e08b3f"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","688903abd14276da4d92227f99d68cd0"],["/categories/数据结构和算法/基本原理/数论/index.html","b2f430e5d1f226845e5e15c48b88c41e"],["/categories/数据结构和算法/基本原理/树论/index.html","a556229df77de50fba0f056dfa882f82"],["/categories/数据结构和算法/基本原理/链表/index.html","a63aee412e32efaa87ae285cf145882d"],["/categories/数据结构和算法/算法题/index.html","b1948ec08c2d18b3b188fe5658159bf7"],["/categories/数据结构和算法/算法题/二分查找/index.html","a9b8fa5efe29a5f37fc5f7d9105efa3d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","7d51c053ea83a4b7b963571d11de61c3"],["/categories/数据结构和算法/算法题/动态规划/index.html","051c69535e511a0dd9987c19a9794f2d"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","997b846d06648657df64443f611cedb8"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","6f9134a6125d41ccb93a4344e0b51315"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d61ea68847fee3b3fa89c8d06a21dda6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b382854c3c8043b125ebd62caec38003"],["/categories/数据结构和算法/算法题/栈和队列/index.html","1d4dac9c86d78d835d9d982fddf20c34"],["/categories/数据结构和算法/算法题/树论/index.html","4d00eba6a062a5692d25d55eba780615"],["/categories/杂七杂八/index.html","1c36df6f1e0e9ae5dec9d9a3446ad5f7"],["/categories/杂七杂八/博客搭建/index.html","e99e0807b69f8b1a33874dc2e18245fa"],["/categories/编程环境/index.html","85d3be35e939461ce9c4fc2ba04220c8"],["/categories/英语学习/index.html","98e2f610864f0e141f936670183ef73c"],["/categories/英语学习/英语语法/index.html","34526262a49c3f151efcb4ede8b480c6"],["/comments/index.html","44e2ca60cd9bdf30468a836ecceb3604"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b38c8e8df79d5e7b7597f3d486fd1b72"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","05677ea4fddd9090e8d02ce1b306c5f5"],["/movies/index.html","10c17b61106d645d01e101917417f3d9"],["/music/index.html","8d9de15876060c3e6cf08fd8695147fa"],["/page/2/index.html","b4875b574411282f54e6379fad0e24d2"],["/page/3/index.html","9e3adaae35a38e8170fc74375ac7433f"],["/page/4/index.html","60d30f161a67f59bd7ec2508a42cb863"],["/page/5/index.html","850eb4c0709bcc4bcb84d981e223ebec"],["/page/6/index.html","b5f17370ab58682841a3f7d079bbffdb"],["/posts/1021360842.html","8998c1228f19ba508fd9c472ac9c7212"],["/posts/1120620192.html","c9a6c8cb0a659c873233f9358749f7bb"],["/posts/1141628095.html","2735796c9a3ff5669d79022cfd0581d3"],["/posts/1168613674.html","97a58c0c97a438489ffd4b54519b0ede"],["/posts/1219920510.html","f6697f056efcc0fcff9307aff8b1eabf"],["/posts/1222166338.html","cbd32fd260c12ea2bcb252ccf38f9d60"],["/posts/1259097482.html","3785c777ad1663a89c7f14ca7fbe2c12"],["/posts/1271036369.html","9833f51f4f9531c6940761ef98b50073"],["/posts/1312847445.html","dbbbed9f86ad8a14471ac2340b4afc30"],["/posts/135355774.html","3d4347d383eb4ce8c7f42764ea2ac7d0"],["/posts/1375344716.html","616ad87533494cc324861d47c3c76c72"],["/posts/1388991698.html","724042ff02c60c4fdd677fcaf5e586a7"],["/posts/1410315814.html","f4864448727bf5c2fb35005c49fb0b90"],["/posts/1452790229.html","90c7f0b0c15022adf9a392a4e80fb053"],["/posts/1470079884.html","d6ca133306e12a781d339930a629f574"],["/posts/1470079885.html","4d1d18e9d0d13ee270e6e7bf3912a3af"],["/posts/1470079886.html","f9808199fe8a8429d9496dd75329f207"],["/posts/1470079887.html","0788183ac29d70e23ef24adb9efc368e"],["/posts/1498536549.html","79239c97b4fea8ae996b5c242756a718"],["/posts/1547067935.html","86388e53feaf072b939a999b9932d0f6"],["/posts/1557866301.html","1194549655ab686791cf47dd125b47ae"],["/posts/1571776361.html","dfe71355512feef6dd37bdf81f835997"],["/posts/1605124548.html","ce3ee9be049a12ebfbbcb8830d809767"],["/posts/1633036852.html","c95c2dc5801c841354027ce639e5a181"],["/posts/1765123828.html","604b4526d453d3929fcc7aabfeb8a331"],["/posts/1767336200.html","1f2474722c88d427d233c89e97d82dc2"],["/posts/1776114197.html","f44d1810622def86f655865d5d21c893"],["/posts/1817748743.html","30b50d4e6c3aedcaca0b94e92a663b92"],["/posts/1925125395.html","5dea09273b781fd535054c031f60ef0d"],["/posts/1966191251.html","6566db720aba54baa9250c0329f1512e"],["/posts/1987617322.html","49f319d277386501fbc559e17e44400d"],["/posts/1999788039.html","5bc7f343420dec397920d2d018ce1796"],["/posts/2075104059.html","506e04d54034062c1e0f543cecef97d5"],["/posts/2087796737.html","449add9a89cf188109128855e6d6925d"],["/posts/2106547339.html","98ef3363e04b91b24b5395e40228a9c0"],["/posts/2207806286.html","a5169a293c7b01f4d1624e70d868ed29"],["/posts/2225903441.html","33f64aaa4536e274595466b1bec1d206"],["/posts/2265610284.html","908be3fb6cd8f6747288737a49b8cf1b"],["/posts/2281352001.html","e373d8f79f21f2591fd2ce4ee00a70c4"],["/posts/2364755265.html","2e8f3b262e9c01cb81afaa48bf8e77c6"],["/posts/2414116852.html","1b2ac49f8af73d830d12fb0fc4161903"],["/posts/2482902029.html","e9bf6c7c9d8621f747a32beb3047764f"],["/posts/2495386210.html","296d7bcf1ce41a72bc4175fc0c9c3d2f"],["/posts/2516528882.html","dbb738d1e614fddfb61e9c6564c80675"],["/posts/2526659543.html","fa31ae301ce279ac1d291bb79a7f52f6"],["/posts/2529807823.html","93855f27703d9066f64630b2a4e477c7"],["/posts/2742438348.html","50723e485fc3adf7f0a3bdbc65b59a80"],["/posts/2888309600.html","423449a2ec80c3f734692555c2c43747"],["/posts/2891591958.html","cc09da12e876bcc9f3b05d86d4cfc8a1"],["/posts/2909934084.html","f248d0c71acec039177ff22763afcc99"],["/posts/2920256992.html","f7375102fbf5e540dc8dbe8654491c52"],["/posts/3005926051.html","13dabf85fe66e3478e8ec2d6f8889a7f"],["/posts/309775400.html","aabcdd6b321158dc5184e067215f1314"],["/posts/3156194925.html","adcea071740329d377bc9562013194aa"],["/posts/3169224211.html","f601b261f1767bfa7866ae729c8e774f"],["/posts/3213899550.html","4846cd9d7f6b32dccf09052dc6c8d78a"],["/posts/3259212833.html","63054f048a348c58ae6c866769abb327"],["/posts/3266130344.html","72af1fb05454380cfb47bc02bf4344d4"],["/posts/3297135020.html","b074f671796390e7d9406f9c587cc7d2"],["/posts/3306641566.html","4663a9f3386e9948cebb599cf53aa360"],["/posts/3312011324.html","7604b8d2a07053cb974888501b5973ed"],["/posts/336911618.html","60d3aa355b1024dca75cc675d0c8c62e"],["/posts/3402121571.html","e8e9270109b64cae0c771fc6c7769cc4"],["/posts/3405577485.html","51e7d0c99cbf966042c5bfb0ab693526"],["/posts/3498516849.html","8de57f9ebcf02aae6bda2d942f646019"],["/posts/3513711414.html","848c46eb5e525eb138cc464616e00d8f"],["/posts/3546711884.html","dcee6ef68708c7f007ed4e617bad38cf"],["/posts/3731385230.html","2de69398f61aead74f17a4a5f13968ef"],["/posts/3772089482.html","b0aab5b4280e95bcebe2bc9d30f42db1"],["/posts/386609427.html","fb0c18d7a068eb6c25eec3640d774b43"],["/posts/4044235327.html","0d17d39307ffa910dfbdb922951f8595"],["/posts/4115971639.html","df9a6de5bb5ddc8f2d236c2861c8b162"],["/posts/4130790367.html","958f53d8b901bd4eb2ea817f464a414c"],["/posts/4131986683.html","8dcdaa6c89cb30dd9c6cb4a1ce26c8cb"],["/posts/4177218757.html","96edcc0fabccdce40534b1fd9209fb8a"],["/posts/4192183953.html","acc3f0cc1a041416a8ba6e9de3d59af9"],["/posts/4261103898.html","0216b10928dddf3828da62f202a93884"],["/posts/469711973.html","df205f3cf1f8b191e42fde40df362b1b"],["/posts/482495853.html","bcc41e706d363487743efe28319595da"],["/posts/488247922.html","2792d78abacf9032d9c10251e7f665b4"],["/posts/570165348.html","750391f77ac43c6c324d1861d6bae709"],["/posts/595890772.html","8e65d934cf8c04681d2e56e3a184cf90"],["/posts/694347442.html","5f3cb0d581f0a61e405d13333bbaf4f5"],["/posts/707384687.html","3dfe3555f8bb27f6a0c96416a6ad76c4"],["/posts/71180092.html","4eeb11c095a28312e959270d6c8e61a3"],["/posts/716459272.html","9e8873f7feb319d7714e0c5e47fa8124"],["/posts/778231993.html","47f7598c014efddc857505ef236f2c63"],["/posts/795397410.html","7d3a3fc0207f861a49c3c74d68632667"],["/posts/820223701.html","a552fc18a42eee5bb9efe98f460d8c73"],["/posts/830372185.html","900383c2414962be3ac1fafa7c7f5b3b"],["/posts/88294277.html","d6e34a3024e2a605b688a76c7f439d2b"],["/posts/939963535.html","94ef7d7ccf5704f4265867bcda8623a8"],["/posts/983786067.html","1150b28913cc878989764c7c817b023b"],["/sw-register.js","0b5a83a804752067a34f02a3d0004244"],["/tags/C/index.html","9be38400c89a0d32d66d7b32e922f8ae"],["/tags/C/page/2/index.html","2babbb88f1f44237c6ea8860f3ff379e"],["/tags/C/page/3/index.html","0a1e9df7b3f347716793e56409e16175"],["/tags/ElasticSearch/index.html","a26522557b5bb3efe87d99c52e2f5917"],["/tags/GUI/index.html","37700c8d8d72407edf6904ca66af6bf4"],["/tags/HBase/index.html","1d3fb1bf4bf9a5b99802dd875325665b"],["/tags/Hadoop/index.html","9c4bcaf01ef6170eec4291b6a961c76d"],["/tags/Hadoop/page/2/index.html","05b7d25c884aacf8ad85ffb772a7b687"],["/tags/Java/index.html","cec7b5da5c0d14f1137e4e3557df0951"],["/tags/Java后端/index.html","b8ac024fd544c2eb2e4c742d8cc8442f"],["/tags/Java后端/page/2/index.html","25051fb72c055206ae8ba6f9527128c2"],["/tags/Java基础/index.html","04228d189b94f273bf55177b1a611639"],["/tags/Java基础/page/2/index.html","7024fea3d2013891d653d19026e42921"],["/tags/Kibana/index.html","85dd61dff9327af54c90476b63a6523f"],["/tags/Linux/index.html","f704bbe8f512cd98e42526c123fd582d"],["/tags/Linux/page/2/index.html","b3b5a105ae6e740108a8fec6fbd08b44"],["/tags/Linux/page/3/index.html","c761f5d7a5c971f0717332fdf3ca0cc7"],["/tags/Mac/index.html","e5fd989507feb6b9e798c6465da2c7fd"],["/tags/Mac/page/2/index.html","09d07e73fe378cec8b9cee12188dbce3"],["/tags/Maven/index.html","06b649cc4b83f340232dc26448443a90"],["/tags/MySQL/index.html","28e9bb71c4694b6efba56348fa2dab9f"],["/tags/Python/index.html","4daf642737a648415564d206dc098be7"],["/tags/Redis/index.html","4f856b5f04bc93b7218d7039cfdc163e"],["/tags/R语言/index.html","e4390193bba7845706addd35061cf645"],["/tags/Ubuntu/index.html","5b72ad4d4432aec2cd12a19eef8c85af"],["/tags/Windows/index.html","18e2635b6e5d43d643c99cf20dfb5574"],["/tags/ZooKeeper/index.html","2dc7e5456d955a79600379e11331c9c4"],["/tags/bfs/index.html","4a588cebf139d2e5957f96031691a78a"],["/tags/dfs/index.html","6241d69134411feadd5a5b7f8206a97b"],["/tags/folium/index.html","6cbe21dedf22d2aeb9fb7ccf0a6b755e"],["/tags/git/index.html","884f69efc38c10a80c6db70638be5388"],["/tags/index.html","d8824209fe9b74b9cf978862e38144a7"],["/tags/latex/index.html","2198e4e23afcde9751e186124b7cb904"],["/tags/中间件/index.html","5f525a140dbb621d1b221786fe58e542"],["/tags/二分查找/index.html","f0f79eb344d0e9f82e218c45f3784cd8"],["/tags/优化类/index.html","031dcda8834914ab1b581cc4f4c6c334"],["/tags/前缀和与差分/index.html","3e2201db1fe77a1622b696cb74a9cc4e"],["/tags/动态规划/index.html","95cf7d05d39d82b56269c276c5aa9a70"],["/tags/动态规划/page/2/index.html","799d9999d785050256ace05f57cb0f9f"],["/tags/博客搭建/index.html","9eb96d790ab97645a44ed25ffae39599"],["/tags/图论/index.html","af59ac4411098b898a656398333b6265"],["/tags/大数据/index.html","37949d167df718126237f89dc560facf"],["/tags/大数据/page/2/index.html","8e3f6bb3daa6ff4bd66ecfd130c7ffc5"],["/tags/操作系统/index.html","76e7bff42ec4fe207afe51af7dae941b"],["/tags/数学建模/index.html","655399902bd52f225f0ac78ae9050899"],["/tags/数据库/index.html","77aa1725cc6e734d18f6507c1fbdc12e"],["/tags/数据结构和算法/index.html","a5106e96a8cce31e0e81fe5f6316fe7d"],["/tags/数据结构和算法/page/2/index.html","e201961aa392f81cc732aa4eca2b6c3b"],["/tags/数据结构和算法/page/3/index.html","e89943432a9a860a6b419e15ffa0aa9b"],["/tags/数组和字符串/index.html","3ba418f04795351fdadf232f8a97bc2c"],["/tags/枚举类/index.html","091236f4cf08f0683d7fb88d9cac3b9c"],["/tags/栈和队列/index.html","69ac857c714f932b4626f30a89d30684"],["/tags/树论/index.html","7efdbd8e0d7588ceb7aa3e612271e94e"],["/tags/测试/index.html","40d3f9cddd44b947dfe73625cf288d59"],["/tags/环境/index.html","68c88380ad03046d79f229295217ecb2"],["/tags/环境变量/index.html","be546b10559a38f9a8c2085d94f828e0"],["/tags/绘图/index.html","4ac5ef29061f79471ba9c88e88133b78"],["/tags/编程环境/index.html","aec3b0fe0789a7ad21979856e812631b"],["/tags/网络编程/index.html","503fec9ffec8358e685f89be4a58c6ab"],["/tags/英语语法/index.html","18bfccb44d52a5d667fedddb26f548d6"],["/tags/论文/index.html","765b5ca71775363d0afca0defc56be38"],["/tags/资源下载/index.html","7303feedd62723bb0f4abda9728e9c1f"],["/tags/链表/index.html","116299f751eac91afb8080623a18c768"],["/tags/集合/index.html","1e592a0610a3cddae0e1c55d358d69d2"],["/tags/集群/index.html","c79c31c99fb404dc43ca48f7175eb2b3"]];
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
