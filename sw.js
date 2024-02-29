/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","496dc177e00fb32753cf9d2de5f9b575"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","1b43bea040747eb7a4872a03495994ed"],["/archives/2023/02/index.html","b078a9fd9d1072d32ce4e92b7b4b8654"],["/archives/2023/02/page/2/index.html","201df470c4948e3bca1cc0a6a8fc0441"],["/archives/2023/02/page/3/index.html","a48f5142ccbec5af5ab935681359d644"],["/archives/2023/03/index.html","2a041ec13282a3a9717c21a666a55a04"],["/archives/2023/05/index.html","3274a34e76a20a15c106f2b9e18d6a18"],["/archives/2023/06/index.html","dc3d8b91153daee45c16b1d77afed6cd"],["/archives/2023/09/index.html","23f2902c65a67dd6d2a2fba56f70743a"],["/archives/2023/11/index.html","8a34223780a0932e0479c6d4d93b9c26"],["/archives/2023/12/index.html","12f04c7969ed5f6aa08aaddb048ebe23"],["/archives/2023/index.html","7f0b9930268b1d65515eb4234cae1251"],["/archives/2023/page/2/index.html","04d083eb32beac98c672083418b280b8"],["/archives/2023/page/3/index.html","c51ce2b67a37f1cd244484c677d56695"],["/archives/2023/page/4/index.html","693957f7250aa02cfe7f68cd216597cc"],["/archives/2023/page/5/index.html","b8e2a2e6b6e8c58fa2e1c3cd359b0c5a"],["/archives/2024/02/index.html","3710a8d2c0a09cc4a01c2efc352541e0"],["/archives/2024/index.html","074fc7fd6778372441747ddb6dfa8094"],["/archives/index.html","50c8aaddb2ff38cf96ba8d0831441314"],["/archives/page/2/index.html","aa1d85aed4836f485e2cfb7de3322583"],["/archives/page/3/index.html","1ab37c168b7da4578c299520fd5fc24a"],["/archives/page/4/index.html","22b195249fe784f12550150e256ecbfc"],["/archives/page/5/index.html","0fc0780d83de1026c41b64ccb95c52bf"],["/baidu_verify_codeva-qQP2iZOMLX.html","08c83f8a3c7215f0c7b15fb91d554258"],["/categories/Java/index.html","2e81553ac8e592fba5345e18ba017441"],["/categories/Java/后端/index.html","bd31851cb6e616ff69f06b59a95be74a"],["/categories/Java/基础/index.html","4dc226a1f69e451842498d061004b0a1"],["/categories/Java/基础/集合/index.html","73dd9e087d8779ece5d7d378a195cc4f"],["/categories/Python/index.html","97e836297928fe85da4b4de52ee8c5f9"],["/categories/Python/编程环境/index.html","a7d2c827a3c05d93df7680378c201578"],["/categories/R语言/index.html","ff14a5b681ef3669187595a722290740"],["/categories/R语言/编程环境/index.html","0fffdc6da7327926f7efc0c81b7bbcf3"],["/categories/iPad/index.html","f5b86086b7be1dcaf979e4e850c999eb"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","f861a9d5e174e7cce260172bca9bc3f1"],["/categories/前端/Vue/index.html","a1519f3270f328c53f291f2dcc826fc8"],["/categories/前端/index.html","854847f84a58f7eae93bdaa7da70ef57"],["/categories/大数据开发/ElasticSearch/index.html","a80f5d8132ed313b97270d93c853bbed"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d4532d8d5dc11f763d3002a6ebd76016"],["/categories/大数据开发/HBase/index.html","a8ba591362b22786b94d8f7f7308995c"],["/categories/大数据开发/HBase/学习笔记/index.html","0df46d3997c7fb81ce8b829d1f0d2f17"],["/categories/大数据开发/HBase/环境搭建/index.html","488b108b8c3a70da77c3be8d18ea95a6"],["/categories/大数据开发/Hadoop/index.html","2209349e2349b62ddbd882b3055f405c"],["/categories/大数据开发/Hadoop/技术/index.html","ca45bc8f8a79f3daecfa2b28664d7f25"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b8d67a96a70000b32fb1760058e00983"],["/categories/大数据开发/Redis/index.html","a8ec286e1bc89e4eda464a0062b5c16a"],["/categories/大数据开发/Redis/技术/index.html","c4bf804051b933c980a672f170fcc111"],["/categories/大数据开发/Redis/环境搭建/index.html","3ca70acd0637c09b41a4e7abffc2d64c"],["/categories/大数据开发/Spark/index.html","125f029e2da6221c3543c5807044d145"],["/categories/大数据开发/Spark/环境搭建/index.html","c44c6510698f146dd495fe4e07f66d5d"],["/categories/大数据开发/Zookeeper/index.html","2b22efad8762729065c7fee7b2fa121b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","13a5f70d0c8bd8f794238aca8c395032"],["/categories/大数据开发/index.html","ae30cf24bf3212470b2e712665a4f70a"],["/categories/学校课程/index.html","7f970090f6d34fb3d25786b5d6df2767"],["/categories/学校课程/计算机操作系统/index.html","8fd574d2993c55e4954abed6da17ed85"],["/categories/操作系统/Linux/index.html","dd90548ae285e5deb0788fdd5620f0b1"],["/categories/操作系统/Mac/index.html","46bef45a866c9c35de417e19cf68337b"],["/categories/操作系统/Windows/index.html","ef239021b78e2502bf9bf7399ac152e0"],["/categories/操作系统/index.html","da5298cb6cdae58a746bb00a90ddf9e8"],["/categories/数学建模/index.html","91947f795b9f11b340cdabd1983eb520"],["/categories/数学建模/latex/index.html","7268b718e7766b48d187849a115a1ff1"],["/categories/数学建模/优化类/index.html","c1cc9250a03094a16592d78ae302e9b8"],["/categories/数学建模/优化类/现代优化算法/index.html","eda11d7179e150a53b4d29ffeca7d707"],["/categories/数学建模/优化类/规划类/index.html","f661e69954b7fa812a2e5f050096261f"],["/categories/数学建模/绘图/index.html","7a8a758895b03e6ba3f513354c420dbc"],["/categories/数据库/MySQL/index.html","0ca154a8a34873a4d6dfb2e5943ef4c4"],["/categories/数据库/index.html","85b057dffc9c26505742db0b62826039"],["/categories/数据结构和算法/index.html","551212730e0dce6083ca4e06e30b300d"],["/categories/数据结构和算法/page/2/index.html","20d0370dd1fece7c3ec728a9c8a411cd"],["/categories/数据结构和算法/基本原理/bfs/index.html","185dfba8e6748517a466c97ffe6228a5"],["/categories/数据结构和算法/基本原理/dfs/index.html","ae25af36bc429b9ece7d9e69460f32b9"],["/categories/数据结构和算法/基本原理/index.html","ac8674bf9840af61e11966f1848f305c"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b6c6f243fab83db07df10eb9c1ebcf0d"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d7f21c9761638048b2f04948a10c387c"],["/categories/数据结构和算法/基本原理/图论/index.html","33d61f915cb0bae83a399a06675d3087"],["/categories/数据结构和算法/基本原理/字符串/index.html","5e762ddbc69d0ad2aaead8f76adae701"],["/categories/数据结构和算法/基本原理/排序/index.html","eac8ec7532ea45768b009e9e04e9dc65"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a22a540ea6665fd4d779e901a963ec5b"],["/categories/数据结构和算法/基本原理/数论/index.html","30e4e0367be1de365617dbe8c6386032"],["/categories/数据结构和算法/基本原理/树论/index.html","c45f510ee12407d6f9b222b6c0929342"],["/categories/数据结构和算法/基本原理/链表/index.html","619a3060a099e1f85299eab5f86c6bf5"],["/categories/数据结构和算法/算法题/index.html","5ffc9210c5a4ef3edce5c253fda60aec"],["/categories/数据结构和算法/算法题/二分查找/index.html","28cefe803d04fa7641be8505dcb18232"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","42256baf70a379f6c9d2ffb82bc4afe6"],["/categories/数据结构和算法/算法题/动态规划/index.html","54b32646ab9948dca6f5ed5d0271aa7b"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","1dc3916885390c0fb2e47d1e02fb0d9b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","27ec989b27ad7e2cac3cc3d6687df279"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b85e323c7245e1d7822f6f3a504a7aad"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","6752ee660af8cddb9631c386fdb8286f"],["/categories/数据结构和算法/算法题/数论/index.html","d74a505a903d9280edd78f1da522edff"],["/categories/数据结构和算法/算法题/栈和队列/index.html","fa93688120a8a74d8c41c4a9c815a8fc"],["/categories/数据结构和算法/算法题/树论/index.html","486581135b374dab10e79dbb9c976558"],["/categories/杂七杂八/index.html","39362d80162494cd88ae8f0af7a9706e"],["/categories/杂七杂八/博客搭建/index.html","c579c0426be05aa32394da93375eb814"],["/categories/编程工具下载/index.html","18ed8685608b371d49cb9f215afe13c5"],["/categories/编程环境/index.html","4356646ab53b0602bd7f9de3fad4e9df"],["/categories/编程环境/大数据/index.html","cc140b6f14d00f8c827083411c0d171d"],["/categories/英语学习/index.html","1f6ff83d74b97dd6522bd2e9923e18ec"],["/categories/英语学习/英语语法/index.html","7beddda2312eea4098045a55d6048be7"],["/comments/index.html","211feb9e6d7fd4d2f061c2a4060ca093"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","3734790b334c73a51af9d8228e815a25"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1b876a159e5db5601edd1080d1f5072b"],["/movies/index.html","fad4019b02225d1c31605088e277af5d"],["/music/index.html","30a009310cc204dd0d41c6f3397baf05"],["/page/2/index.html","7aa07f774322f967b73d07469a9b3ff1"],["/page/3/index.html","402c339afc78df727da611d240c87693"],["/page/4/index.html","e469e45ad6f78216def55de12c28d8b3"],["/page/5/index.html","7fd7812021ca3a435bdb5371d687f393"],["/page/6/index.html","6f4fb755f8fbc85814b465e45197a72a"],["/page/7/index.html","258b2aa400a0321ada828a0c56f97824"],["/posts/1021360842.html","aaf0af95b24241a2b742d8342bcb24c1"],["/posts/1120620192.html","b1ab37f32d274a243f4883bf0f3d7343"],["/posts/1137707673.html","6508ac7f2acb6c09943dcda29e46d996"],["/posts/1141628095.html","03026a5ccb8ce2280e2b7b86a62457eb"],["/posts/1168613674.html","e53a8732ee4f1ba0ef49239c40746d15"],["/posts/1219920510.html","fcef288363182dde68355387055f6188"],["/posts/1222166338.html","98d4285a317f82bf075e82ab1455f6f2"],["/posts/1259097482.html","b2ee5a6256e90192355b29f67b35c181"],["/posts/1271036369.html","7750cc85e9470d585e23825d99021ff1"],["/posts/1312847445.html","0d9219c6e413c2ea65f457aa1e509bf9"],["/posts/135355774.html","f988b55e4debcb78c9805e4eaa572b5b"],["/posts/1375344716.html","ab6664c732a77b0ad3f03bb6f8c49829"],["/posts/1388991698.html","06027f2d9ce1b5885c20938b7111b994"],["/posts/1410315814.html","63b4bbc9d521cbd0a6281f721295e8dd"],["/posts/1452790229.html","726c81e3028acc29c1d956e433c7a393"],["/posts/1470079884.html","ae0a0ade1d4da634f085f1e5adafc7ef"],["/posts/1470079885.html","1d881fbc32a1cb8771ea8b4f1d29480d"],["/posts/1470079886.html","c78be96d2ff261c53fe639e4d065cfba"],["/posts/1470079887.html","b725d7c09cc0bb29ac43a26c73cf7eac"],["/posts/1498536549.html","7701150ed53a95681bc7c8b951682842"],["/posts/1539568593.html","c80c7b1aee479088bd9eeeea4e26bab0"],["/posts/1547067935.html","3d9445ed014ef49caf65c8d6db0370e8"],["/posts/1557866301.html","0ff6c1e950a27b5ab4d4975146824161"],["/posts/1571776361.html","ebfcfc45ef34739b56a37afa23313c9c"],["/posts/1605124548.html","0bfaf106721538009cd6ffa45abcac7b"],["/posts/1633036852.html","f57346f70d2c981c14eda03f96f519aa"],["/posts/1667740714.html","774f53d8c83c151a045b9a67bec58334"],["/posts/1674202625.html","5cfb254e6898c8287d95735dc8d12d60"],["/posts/1765123828.html","fe3ca505212b032538cdc4389f8c524e"],["/posts/1767336200.html","89ad1bd28ddb5099e954b074cbc426f8"],["/posts/1776114197.html","569c7737f767b2dfc593b3f87f1c15d3"],["/posts/1817748743.html","48fbdd703030ceb057d73de42c950a29"],["/posts/1925125395.html","2e481eac13dc8bf7298c1417cda5bbce"],["/posts/1966191251.html","13606823b47f7d05daeda753fee8103f"],["/posts/1987617322.html","55acc2702babcf7b1a3ae7cb2ab8a8c2"],["/posts/1999788039.html","432613ac842d81b68ff8ca8b5602c5e7"],["/posts/2075104059.html","dbbb6d156136faf9d9d386bd62e6e040"],["/posts/2087796737.html","20d507cb31db0ea13ebca65bfec0593c"],["/posts/2106547339.html","a490b23219471c9a42c2a6ba2badc2bd"],["/posts/2207806286.html","adb987a362f8b0821577709fc9a4da1c"],["/posts/2225903441.html","fbca67a2b740cd0f2682e51b9165ee6d"],["/posts/2265610284.html","77bd21abc4eeeafd5009abfec97d3384"],["/posts/2281352001.html","01e353fd706bd9a9af8764f95c3f097f"],["/posts/2364755265.html","460c272fbac5ceece8f005fd5be03d81"],["/posts/2414116852.html","874eccbad0443c1216ea84eba829312d"],["/posts/2421785022.html","ea20877909683277b50248f170bb3ed6"],["/posts/2482902029.html","10ce14136385c8a02ec0ac2a56b8e3cb"],["/posts/2495386210.html","7df9ebf839676d2f2e8682ee3204833a"],["/posts/2516528882.html","d3e866a4758b1a53bb8520d5c2b83571"],["/posts/2522177458.html","9073a3f3008a971b7f3a2667b85b0583"],["/posts/2526659543.html","8dd30c8348a3ea1a0ee5191399ecaa16"],["/posts/2529807823.html","bdce8893783cb61e9d2b0427e442e053"],["/posts/2596601004.html","672076f81459211c8da036749f6e89c1"],["/posts/2697614349.html","4e7826bdd8a90437733f530b9b424d00"],["/posts/2742438348.html","0ca874bec53474caad1f31ed2cc15da3"],["/posts/2768249503.html","50932f76d37a7e3f3c1119f8a66e02c4"],["/posts/2864584994.html","cd0e1e8d6cfc9b78daec760a2451568f"],["/posts/2888309600.html","8e2043e8add564bb2e01114eaaf12c46"],["/posts/2891591958.html","ed57709fce50f5ec41a794592b8ab53c"],["/posts/2909934084.html","3a5b8d85f55e9c3eea51e42da4a7b866"],["/posts/2920256992.html","bc3635b2d958d5892353d4a2f8a7ce82"],["/posts/2959474469.html","5933b9223a9a7f97ded0c4f486861a71"],["/posts/3005926051.html","2315d8697946923e55ec9ff4be97426c"],["/posts/309775400.html","bec52307faebb7148f953b4d18698867"],["/posts/3156194925.html","e322099cb15649284cf966d976f1e489"],["/posts/3169224211.html","a397a819d209d2553b6ffb0fbb44c870"],["/posts/3213899550.html","9d4b00d1c82aa77b15a3296d187f8cea"],["/posts/3259212833.html","44a591237ab8626b95b9dfad33c350d9"],["/posts/3265658309.html","aa2a836224a2cb02d8c9ce3ed8ded5b9"],["/posts/3266130344.html","7de1e4aa9997872086d0d2cfdfb9f712"],["/posts/3292663995.html","eb87df728d01b6addc4e89508410a071"],["/posts/3297135020.html","e3394513018d3c7146cd0dfa6595d19c"],["/posts/3306641566.html","e09cffe18981507854eab59474f486b1"],["/posts/3312011324.html","eecdd0758eb5643afd5209d3c1a85416"],["/posts/336911618.html","69f182942aed5df2f49b836b4ac0753f"],["/posts/3402121571.html","3766c426e67e4983001b22aa4b025d49"],["/posts/3405577485.html","3f609b89fc3d4844cdf1f3b83cd1559e"],["/posts/3498516849.html","a9a08ad53513de780ba2055227866ac5"],["/posts/350679531.html","9ecac169665d9f0a0025b9df3bff64ae"],["/posts/3513711414.html","c8deb8972802a58b6f1b5e959f8d58da"],["/posts/3523095624.html","f5beed6bc64ab98d78c199baf5a151b3"],["/posts/3546711884.html","4fe76c9804c4eb621a361c100151e077"],["/posts/362397694.html","e073f45b80296ed48938f8c046e6547c"],["/posts/3731385230.html","d5e23c1fd5630d6c70a2f3bfa5ae1da4"],["/posts/3772089482.html","38560f604a6384480098fc347fdfa385"],["/posts/386609427.html","bfd4ee78b41b09bfa43f695055026ef8"],["/posts/4044235327.html","1a58baf2c83a1f3c86046a45145bdc28"],["/posts/4115971639.html","5235fe2c76bfffde89c73f2e5a0d16cb"],["/posts/4130790367.html","1fc283cdfcc546b3e20d97ab93cf9e20"],["/posts/4131986683.html","7118964981293eca466e3c7bbef4a89e"],["/posts/4177218757.html","459fa0347386034cf77779e8e3d3c67d"],["/posts/4192183953.html","f6dd24019241590e2df3941a43fffde5"],["/posts/4223662913.html","96b806f86c2a7a93225f2dec9ecdbe02"],["/posts/4261103898.html","3450485835f4eb4b1e5e39ae9e8a1e5a"],["/posts/4286605504.html","8fb0438a80e8191cac118e4b30cbd86a"],["/posts/449089913.html","029914257105a8b94c4a9ae095c63056"],["/posts/469711973.html","0cf9cd6f82f95dc0610ae8d2195ebd92"],["/posts/482495853.html","ad20f3267f1198472b4aeab616a5eab7"],["/posts/488247922.html","e38a7c5675454ebc3323ae866697b156"],["/posts/517302816.html","480da5962df9193c2defa44dc3fa9dca"],["/posts/570165348.html","12df4cc237fa0d54eb3c5433ae0e942d"],["/posts/595890772.html","9155efc00944bf4684d2488ba2259364"],["/posts/67485572.html","d008e8eafd7761e179099555268cb09f"],["/posts/694347442.html","a453d0d7c8fffe1114709b2bb8cbf88a"],["/posts/707384687.html","daa061a57f53121e609721ac14d2d5c4"],["/posts/71180092.html","6ce70e23fae0b1b305dcfb3965c296c7"],["/posts/716459272.html","3557661cac285fe12f252eb0cbc5ab64"],["/posts/765481613.html","2064e9fca740d00e5efc29e64e3acf1c"],["/posts/778231993.html","13afa66d79982cb23711ce5e39c146eb"],["/posts/795397410.html","1dd37be8658b000d8f50dcd861e721b3"],["/posts/820223701.html","9994caf4ec075beffd1c9b3ecd4a3eaf"],["/posts/830372185.html","983b62403ba10fc2ba50fd1e475f22fb"],["/posts/88294277.html","29840b8725b0ceb977b94fcdf1df65e2"],["/posts/939963535.html","7b946715222d7d66bae589f899ce632f"],["/posts/983786067.html","f1a8ef280bbee9c0c38efb749050e4ad"],["/sw-register.js","d7a608b8dbfd60dd478aa980d9244a3f"],["/tags/C/index.html","cdfcf166428c897e667bec8c3cd682b8"],["/tags/C/page/2/index.html","e24c453eac1cd233dab9811d1b0c8395"],["/tags/C/page/3/index.html","16f622e5e0926e3c1551ed75075748bc"],["/tags/C/page/4/index.html","ec675f54928622e29c6dd9ea685ce00f"],["/tags/ETL/index.html","1e5714e9c3f129b9e0340ef181a84993"],["/tags/ElasticSearch/index.html","6234628600fdb0a321011dc16d29981e"],["/tags/GUI/index.html","da0486e8585b861abb83fba7e3fd9454"],["/tags/HBase/index.html","665ec26d44c4d56530423a83c27e0ccb"],["/tags/Hadoop/index.html","07267349e464dad2c5fc4d39acdea8e2"],["/tags/Hadoop/page/2/index.html","a6f7fca54493f7c6015a761a86f0cb14"],["/tags/Java/index.html","0e5e0a233a43457b7e4881193d90f232"],["/tags/Java后端/index.html","49abeae205bb284a3ae998d2ed3ab34e"],["/tags/Java后端/page/2/index.html","3ff15abeb2dcee92858e34c3886efc25"],["/tags/Java基础/index.html","8e8f8b126e563105bc6bc9106041911e"],["/tags/Java基础/page/2/index.html","ed2058aa211af00a36350d38305a450b"],["/tags/Kettle/index.html","2130613eaa249176cf25be01e56a9efb"],["/tags/Kibana/index.html","125328910fda65d96fcc6283950b3d19"],["/tags/Linux/index.html","cc57dd792bde6de3f68e7c4235d4f080"],["/tags/Linux/page/2/index.html","52913b7edc7da6ede59e767371343107"],["/tags/Linux/page/3/index.html","fefc99eb97e6bc1e275e79864d1bb0f9"],["/tags/Mac/index.html","b829622461c53e6023c4bf0b656df480"],["/tags/Mac/page/2/index.html","865b643eab3da6e9937cf7fa204271c7"],["/tags/Maven/index.html","fc29c78f3ac4ca2622db043549e85a3d"],["/tags/MySQL/index.html","56f38818d8eb0195550dc04642265cd7"],["/tags/Python/index.html","a7ffa51f366bd4ec3e0637e5023bc10d"],["/tags/Redis/index.html","fc92f63f26a83fef558d272dac559ab3"],["/tags/R语言/index.html","12eb0ab56e94f3b82c071a3286111d38"],["/tags/Spark/index.html","bff8b0f9d5a41726d7e1708ef42654d8"],["/tags/Ubuntu/index.html","e124a88a3822e8464b38aa63bf384ba5"],["/tags/Vue/index.html","88fd6cbb4b969d346e3fe365646acfdf"],["/tags/Windows/index.html","edb344510d0b94ed9063abb356614323"],["/tags/ZooKeeper/index.html","06572f56925d5eb7a7d6516e167107df"],["/tags/bfs/index.html","5ab0120238f9e1f080e86fdbb9a58997"],["/tags/dfs/index.html","b982fda66557763f65298a6ead208a2d"],["/tags/folium/index.html","2fa6466e38698a13f97cc73277cd4086"],["/tags/git/index.html","2adae17fee166d7d3ccd89b5b57f31f1"],["/tags/iPad找电子书/index.html","a406aa529cfaa4f796b02bcf9d8b2ff3"],["/tags/index.html","73b0ae40df958cdd6d03bea5f52021fc"],["/tags/latex/index.html","e5b330881e6a169bb26bc85039417970"],["/tags/中间件/index.html","1847ed7e1c2d17cfc76047e13419dfb9"],["/tags/二分查找/index.html","0a129d4ce6ec1fd06e517ea260c0207a"],["/tags/优化类/index.html","fd6abd86cf449b5ea081b2d7466a9583"],["/tags/前端/index.html","1dd2224ef49c2132ce0e8b7c89422a1c"],["/tags/前缀和与差分/index.html","8c5e274c1340ff902c20d7b13d1a4208"],["/tags/动态规划/index.html","19ff66b5ddcaaffca1b1cd7085354b85"],["/tags/动态规划/page/2/index.html","cb90f340f200990b7b105094352fa7b3"],["/tags/博客搭建/index.html","c7df0aee46dc5892fbce02f278104998"],["/tags/图论/index.html","1b412e3591a35178d070672183fac9ad"],["/tags/大数据/index.html","767e67e6a6d51e6dd239928b1233c636"],["/tags/大数据/page/2/index.html","96cb151b75ca53e2a98c0b85d8dd9eca"],["/tags/排序/index.html","1bd6b5eb877859593d42a88c2cc09590"],["/tags/操作系统/index.html","c3e9ec4ecaf3d73f6d4442f2f9e8abb1"],["/tags/数学建模/index.html","2689aa303dd844e4f9cc346355cf84c6"],["/tags/数据库/index.html","9e7e9160e23e142c0543ce079cebd4ea"],["/tags/数据结构和算法/index.html","b0067f1884de35e27d34464ae4e87c07"],["/tags/数据结构和算法/page/2/index.html","b3d964ebda534ff795147aeae0ea0065"],["/tags/数据结构和算法/page/3/index.html","eaa533ec0082feccd0460f93e8a2ad7d"],["/tags/数据结构和算法/page/4/index.html","237ec99a3ac5bd39d9ab1d5f950131e6"],["/tags/数据结构和算法/page/5/index.html","976194466b1803b91981c8cfc50b13e6"],["/tags/数组和字符串/index.html","14ca75db51dc36b7867c9389973ad378"],["/tags/数论/index.html","704ad9f994ad808cb5563881e3490b96"],["/tags/枚举类/index.html","e173abc9c6f5a3a2c12a94bfb84138dd"],["/tags/栈和队列/index.html","63cf737f87b7ef931199e2264ddcefe3"],["/tags/树论/index.html","1506f7df8d277cd9217aaf8a38318ad4"],["/tags/测试/index.html","19471b13e85ebe371d191e159b30b45a"],["/tags/环境/index.html","5a029be4c563c468220c734321f3f1bc"],["/tags/环境变量/index.html","b30f8111bb22de32ef05160651ce5331"],["/tags/绘图/index.html","ec15a05c1a236c19704f0764b0c50c36"],["/tags/编程工具/index.html","bb588dd51bb44517c27ae63b8a666c36"],["/tags/编程环境/index.html","b7febe315c0f0829701d198ddbbfd059"],["/tags/网络编程/index.html","637ceb8b9cb6ab4f0727aafce5ee1e55"],["/tags/英语语法/index.html","3f54348a4ad32146375097345a13da24"],["/tags/计算机操作系统/index.html","abcaeab15a2534d1b4704127c18996b8"],["/tags/论文/index.html","8b33b2507214468dcafefdbb7f9c520b"],["/tags/资源下载/index.html","88f72c857f3a980a670ce2f198a6ac6a"],["/tags/链表/index.html","e5783946fcd45faf7ea303c692764d64"],["/tags/集合/index.html","f8e66a96d5022a7596ebf246c32254d1"],["/tags/集群/index.html","ca30f95eddc4c23fd6a4b7c7fdc85bac"]];
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
