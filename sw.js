/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e1f2e4f0160a20702b9bb38016446745"],["/about/index.html","26ed1e10055323e1ba20e33c1d278e4a"],["/archives/2023/01/index.html","759424d137211699a57645bf6bfddbcd"],["/archives/2023/02/index.html","98046e187fbb5960216ffc6192a7f467"],["/archives/2023/02/page/2/index.html","58009112e01b2b4d3c2d197d0ae53039"],["/archives/2023/03/index.html","d2a06f779bf76615e083113028589ed1"],["/archives/2023/05/index.html","f711aca5b668380e85ec8f65c6475b53"],["/archives/2023/06/index.html","ef2ec18dd7debe94d14a454ecd5f1c32"],["/archives/2023/09/index.html","94efa9760571f5e723ebbf60fb68e89a"],["/archives/2023/11/index.html","d934cbbb54bb6f2fa9bb571861fe00fe"],["/archives/2023/12/index.html","de318f21a875df0f566bcb3b919b0fef"],["/archives/2023/index.html","e458fc66cef9d0e92fe7aff0fe71bcb7"],["/archives/2023/page/2/index.html","9164eb71645ee20a75bdd19fe5196fe3"],["/archives/2023/page/3/index.html","1dfc22c36204f5ecfe40593d674e9e22"],["/archives/2023/page/4/index.html","80089d7574cf831fa3ac5fe1af505b8e"],["/archives/2024/02/index.html","40131380ea66b117f25914efd714bd74"],["/archives/2024/index.html","c882c44532104f02d351507a5f4c852c"],["/archives/index.html","4b951d0b5049f12aa0e3bfed35f7c0c9"],["/archives/page/2/index.html","c3a0bc2bf9ac9d7f3170c0c85f926e0d"],["/archives/page/3/index.html","989cd0f09ff0386467cc042dfbb1c015"],["/archives/page/4/index.html","d588c165e3e369600164604d07a40f09"],["/baidu_verify_codeva-qQP2iZOMLX.html","c1216417b84e73525038b3dd9ab50d15"],["/categories/Java/index.html","f22d134ba376af9263e3ac7f40d6d3e1"],["/categories/Java/后端/index.html","117a026ede1352589d9c714d232a4790"],["/categories/Java/基础/index.html","25a5007ba854281cd4956689a24853ec"],["/categories/Java/基础/集合/index.html","98a892ce932679a0e3f40830b473fe38"],["/categories/Python/index.html","899558e6dbb6cf92e345c1d559239286"],["/categories/Python/编程环境/index.html","972a89d15ec09555e1d326bb86491b64"],["/categories/R语言/index.html","2a54d63dcf9e695c44caa4d54eb9b2f9"],["/categories/R语言/编程环境/index.html","1683007e2bf077c083dff915210d7274"],["/categories/index.html","4f5e3e536a28b7a8e3212c00c2825d2f"],["/categories/中间件/index.html","0aa3789d254ea4dbbf51a16104d64f91"],["/categories/前端/Vue/index.html","eab85b59a8d7b428b6ee64ea2883821a"],["/categories/前端/index.html","b04c4eb9e5f3ace811bb3f124575eecc"],["/categories/大数据开发/ElasticSearch/index.html","70dbff0b51b51e4f8a76fa7af2e9e2f3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7d2923088e70bbb048747f01237f2f59"],["/categories/大数据开发/HBase/index.html","5c9dd170ecefb0c46b7d95caf272fa5f"],["/categories/大数据开发/HBase/学习笔记/index.html","caa75612b512e95f323e217bd37f1d23"],["/categories/大数据开发/HBase/环境搭建/index.html","665348eaadb6fee8704a680b282c56a1"],["/categories/大数据开发/Hadoop/index.html","d5011b5026c5deec41a09133e38db4fd"],["/categories/大数据开发/Hadoop/技术/index.html","058627df66061400ef575527bd2fe04b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","ebc5993d101b274fad9d0ca51f0a3832"],["/categories/大数据开发/Redis/index.html","86ea504055e4adcce08150ece9e0cb28"],["/categories/大数据开发/Redis/技术/index.html","eb80800dc7c2cc030b7d9d7985c57dab"],["/categories/大数据开发/Redis/环境搭建/index.html","3fb92debdaffae16a23543d9439f5baf"],["/categories/大数据开发/Spark/index.html","d3d625170e074646ff405c4e3aa3ac1f"],["/categories/大数据开发/Spark/环境搭建/index.html","1f92c5a94b8f20fc5f31b2c42729899f"],["/categories/大数据开发/Zookeeper/index.html","534a2f99a790e6e65fed6efbe63a975f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8e9403bf237f8ff77b362b45325123ff"],["/categories/大数据开发/index.html","ff72696dd3bae0d1494d0cbb698e4df1"],["/categories/学校课程/index.html","a504ab959640aaaf5696eb496301e7d7"],["/categories/学校课程/计算机操作系统/index.html","4de0bfe48fc97337a54a404097dfa2ac"],["/categories/操作系统/Linux/index.html","27b69d938b197c502d84173cecaf37d5"],["/categories/操作系统/Mac/index.html","4e0e1293bd713051e4823b70ba9db344"],["/categories/操作系统/Windows/index.html","fe900d4c99d9811de5e6700fc0c712df"],["/categories/操作系统/index.html","2c374d37409dcf4e7403fc84849e7c19"],["/categories/数学建模/index.html","9714018fc283a22ae510488b4ffc8f3e"],["/categories/数学建模/latex/index.html","b96e24c0a1bd8c76e3900f4cd06051e7"],["/categories/数学建模/优化类/index.html","0adbcbea36f81f0ab85a1268c78d0e3a"],["/categories/数学建模/优化类/现代优化算法/index.html","93754cd1971f83fee5ea58776415b4b5"],["/categories/数学建模/优化类/规划类/index.html","391e36e15754812e9f9c003a97e93f7c"],["/categories/数学建模/绘图/index.html","7ca6301ea4dd240a0b19f8e843830d34"],["/categories/数据库/MySQL/index.html","bcd994536982f2777f4109f49a4d518b"],["/categories/数据库/index.html","d13704c5175c0adbc6dfe9f1ff12c4f4"],["/categories/数据结构和算法/index.html","3bf7094f57023a798697d0254f231df0"],["/categories/数据结构和算法/page/2/index.html","903f911fba8d69dc610454460c49644d"],["/categories/数据结构和算法/基本原理/bfs/index.html","01fd37de39b66068e1a7b5af3f23d106"],["/categories/数据结构和算法/基本原理/dfs/index.html","a254f0e708a8df896ee4700bdbaed8f4"],["/categories/数据结构和算法/基本原理/index.html","300b7e3eab85134812dd263667d9c0ef"],["/categories/数据结构和算法/基本原理/动态规划/index.html","54f587751e24cf8e2024181cdf557152"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","2f793ba57f07788a6e88d64889273db3"],["/categories/数据结构和算法/基本原理/图论/index.html","f23c793e2efbc57a9b8be4b3d0beb957"],["/categories/数据结构和算法/基本原理/字符串/index.html","0a94869d75cef5a8fd8551995885ff16"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","f45fbf51b87d3c027b6cd2af887ac641"],["/categories/数据结构和算法/基本原理/数论/index.html","0a5b03ea2b51ba446574bcfc57cfcd21"],["/categories/数据结构和算法/基本原理/树论/index.html","f7d51f8f430f464a48e20284abf9a15d"],["/categories/数据结构和算法/基本原理/链表/index.html","8976ada0599769679d709862cbdb047c"],["/categories/数据结构和算法/算法题/index.html","d124989f26a982505c27da61373b95f0"],["/categories/数据结构和算法/算法题/二分查找/index.html","d4f7173191d36a96fd10d804785e2e1a"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1c414f99bba560b904a0b4a89b7dfa53"],["/categories/数据结构和算法/算法题/动态规划/index.html","1d332250bac7808fabb651e3b8e596a3"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","f19cfa7eb696cdeac0e629a3c94cc2ab"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3350aa3a5ce18ae120d50353bf549cf3"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","163d2364b9470f0c7798d3d49f602a8e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","3bc695ef2dae231bd4f6a29df186201e"],["/categories/数据结构和算法/算法题/数论/index.html","f784809d89df6e675ce455f4fffc7754"],["/categories/数据结构和算法/算法题/栈和队列/index.html","1fd3edffc1a11f81f22a3f10af7bed00"],["/categories/数据结构和算法/算法题/树论/index.html","d023371d526ee0b504a134b506392822"],["/categories/杂七杂八/index.html","499f75d6a20b5d553cb6ddeb2320b050"],["/categories/杂七杂八/博客搭建/index.html","30bb3840e1e33f123cee9bcad1a2f99b"],["/categories/编程工具下载/index.html","051411441b60a673247350e7a663cc58"],["/categories/编程环境/index.html","dfeb96cd51d674f5e0603e7f8aaede9d"],["/categories/编程环境/大数据/index.html","a3082a21f8ca4595c0171dff2c89e1fe"],["/categories/英语学习/index.html","1997fa76f208fca477863bd953d91a5e"],["/categories/英语学习/英语语法/index.html","bd613bfa6157befa94d25c718a5bbaf2"],["/comments/index.html","7c050d8d372dcc6a8a216c921f9bf397"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","fe83b84e19d099580132033c65af15f0"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","822332e04d91e49c92c14998dffc58eb"],["/movies/index.html","43847345f88260b1c6f29b56125bb91a"],["/music/index.html","9910ce6297f48885e487d9a5b6edefcc"],["/page/2/index.html","b19893177cd323ec8a428c40b7ddd9be"],["/page/3/index.html","334def9fccaf0c089f45127f9dc5122d"],["/page/4/index.html","0f2cecfd5093a778e1a4884056a8f342"],["/page/5/index.html","8b3fd24821b634851ee1e3ac1e1be1ec"],["/page/6/index.html","4cfff95d1f2db378723e3d179c0954ba"],["/posts/1021360842.html","cce170b86dff89c369f9eb42eb6a14d5"],["/posts/1120620192.html","b00551dd665ef088e9bc1be26b818083"],["/posts/1141628095.html","2b5ba206977546f5ebeb222b2e781906"],["/posts/1168613674.html","94f02bbf9c7187ce18012d9a09c57c0c"],["/posts/1219920510.html","7b80d2c923b1664c592ff8cd253cb13a"],["/posts/1222166338.html","40a27cf6cd1197083ba5aafbb94089c9"],["/posts/1259097482.html","a0341afbb65f0e42c1897e7a1a2aaafc"],["/posts/1271036369.html","5383d5843ba51ef8a5d13360ef120742"],["/posts/1312847445.html","4d5071f5e6aae22a641c5d74579375c6"],["/posts/135355774.html","a0a213cc181f5c729ef5a0c3ab06a374"],["/posts/1375344716.html","87c357530caa6b5f9be5603bc8b4ede5"],["/posts/1388991698.html","f07b71817d07f9b0c154a73c0bfb53d7"],["/posts/1410315814.html","ffeaa15b3d5a9d4ed7ee9c28fa1fea56"],["/posts/1452790229.html","61b15ac9b92bf1d49016967d9155d86a"],["/posts/1470079884.html","b180ad763ebd0a59f512e5925a38f120"],["/posts/1470079885.html","dab65a567fe66ebbe348d942c4692738"],["/posts/1470079886.html","b21cd431d74f052dfe49a50e94c42fdb"],["/posts/1470079887.html","a39dedc9cdb04c8cc257538af0354f45"],["/posts/1498536549.html","df6a74ab81835666b21032c9f7d08a23"],["/posts/1547067935.html","06d186a9dfcced6ddb734736ecdd63aa"],["/posts/1557866301.html","0bca50a802c486de0dd19a97ba4d8735"],["/posts/1571776361.html","131c7a1d2b93e1e67430de7482baa759"],["/posts/1605124548.html","8d57939fbb0abb9312230d43d6f53b94"],["/posts/1633036852.html","3bdbf6a0703aa16b4e4be2943c317e7f"],["/posts/1674202625.html","d4f09ce89bddd56c7f5c76d009eff94b"],["/posts/1765123828.html","c7e0f5908aa64dbbdfd7689576718d6c"],["/posts/1767336200.html","28b0a8e4e00d31e02290f7acad69c8e3"],["/posts/1776114197.html","75ca54962f2ade905c3364e10d4fe4c5"],["/posts/1817748743.html","9596ed2c4bb534e7e78d5d90f42541f6"],["/posts/1925125395.html","a32f3a9d81ff5d14dc15868b0a73208f"],["/posts/1966191251.html","b181d2ba1bbcda89a333b6b2e6f63135"],["/posts/1987617322.html","17a2dcc05ae594ac0bbed411cdfb003e"],["/posts/1999788039.html","b1828cfdc026f6544adf9f4c6df7084f"],["/posts/2075104059.html","50c04f3cebda6d67eaa7b8dedf1a67fb"],["/posts/2087796737.html","139ec307feede257c38297e6badb4a57"],["/posts/2106547339.html","c9736e0eb143378a966d04d314077493"],["/posts/2207806286.html","27ba78ea83260c4bb427124178292fdb"],["/posts/2225903441.html","59837ec8be8e25ad218522ab1d1beead"],["/posts/2265610284.html","96d8935eaa383c2b80d64d73dde88cf3"],["/posts/2281352001.html","dcc6e0e290366ca1b8f239dbd89a7116"],["/posts/2364755265.html","0ad5a995a8e25f9e3dc8229b38626807"],["/posts/2414116852.html","3a171907ba0520e9b50568c8f77a95c8"],["/posts/2421785022.html","178d535aa156b255f473f5b03e70f6a8"],["/posts/2482902029.html","8a01461437c4d2e70cf47c48e77420bc"],["/posts/2495386210.html","953323fcdd420b2cc2131e7249419628"],["/posts/2516528882.html","0b32a9b89fc2e800885dd144e940304e"],["/posts/2526659543.html","f838e6b8bbdba89ce033f7a92e9e6480"],["/posts/2529807823.html","019769af4103eca3385b8ce81fe6cb33"],["/posts/2596601004.html","dcb43850b4d2588d21a68014be766da0"],["/posts/2742438348.html","4ccf89ca5a5444685254a5a23d0cc94f"],["/posts/2864584994.html","b07698d04808109fe863e1620bf93514"],["/posts/2888309600.html","a3565e7b5784b3f8a757923a6c4f1a0c"],["/posts/2891591958.html","60b5d1c0250bcbf112fb6cf250f3f80b"],["/posts/2909934084.html","bec590889e4a7c1be128fc8e64fbe943"],["/posts/2920256992.html","03842c8eebffdc3e7438d50ed5480d90"],["/posts/2959474469.html","96dd328e6088834b3881ff0733bd72b4"],["/posts/3005926051.html","7ece09f171eb6f8f60f620a6006a073d"],["/posts/309775400.html","3dfcfc1ca7338b80b0291c60aea017db"],["/posts/3156194925.html","579630c9343d8352db91644b66b3d6e7"],["/posts/3169224211.html","e75e7b685f1ac8bc86b9175c3c65de44"],["/posts/3213899550.html","5d1603839109027328949b6b7e16eef7"],["/posts/3259212833.html","b6ed046fcac44f07877fc11ec67a35ce"],["/posts/3266130344.html","1f5cd05aec3861bed62ce2612f2f0d57"],["/posts/3292663995.html","37dd0afcfe0a18213042cf8fb05ffda3"],["/posts/3297135020.html","84b1477805f11cfa2ded430f2aa1313a"],["/posts/3306641566.html","526666cdbad1f3d3bda7cf4ad0655888"],["/posts/3312011324.html","9d56bee511dd4d71a935a9dec0a394e9"],["/posts/336911618.html","b3e4a69831212412719f3c41ecfb5818"],["/posts/3402121571.html","ff9cab2d75d8a349576f2cfe392b49d0"],["/posts/3405577485.html","482af8f1392987026884f7a3bd7db5dc"],["/posts/3498516849.html","a21328f4cab42d6559429aff754738e1"],["/posts/3513711414.html","47220b5d0f93102d75030f9d992022ef"],["/posts/3523095624.html","faebdd76a2baa53096a6b7e1faf75c79"],["/posts/3546711884.html","4d8264c213de6826c16b4eaf2df782f3"],["/posts/3731385230.html","6cb17c819d016beccbf2333f4b5cac5d"],["/posts/3772089482.html","e8d5a87f07f736220e8f7d6b32dbfd72"],["/posts/386609427.html","270fde7322519fbde2624174d0f91c53"],["/posts/4044235327.html","7bcd1c96aa81da16ed6010c79eb797a3"],["/posts/4115971639.html","d92e7b64a200f4af1977dcb4b5e40341"],["/posts/4130790367.html","bd5a39b732c7635e53f837222fb053c6"],["/posts/4131986683.html","eb2a9f4f27b0075419a5d8c167f12692"],["/posts/4177218757.html","a291cc33f41f27f21c7611d92c682599"],["/posts/4192183953.html","2c5d6fd12bd1f02189adcab1422e6b50"],["/posts/4261103898.html","216995df3949e8ef095b85441b5e1e2b"],["/posts/469711973.html","03cba90574906a759e3a7eb22547cf57"],["/posts/482495853.html","9475e09aef82bb5ba29704f0aaa86f2e"],["/posts/488247922.html","eb4e11a51fadb8944dc18f5df0c725a6"],["/posts/517302816.html","9a052d0d1bd61b93def0abcd78148006"],["/posts/570165348.html","e33f3458e4337dc8dd78871bf33c4d63"],["/posts/595890772.html","222fc9e45ae3611c01fabfc3baf68646"],["/posts/67485572.html","d323f9347d1875b45aba4b0410e94ade"],["/posts/694347442.html","13423127fbe5c79d1d48ec523dd1d40d"],["/posts/707384687.html","bee1b4bd838d509b99f1b6ae09e08504"],["/posts/71180092.html","c8fad62cd2110c298254ab8e043c94a6"],["/posts/716459272.html","dabd664cb9b56c582b67c99152998555"],["/posts/765481613.html","35189008bd5c1db5e5e721e92778964f"],["/posts/778231993.html","576c92e594f2a2a994afc4da3f046805"],["/posts/795397410.html","8de08ea22998e48bfeb776fe4cf88912"],["/posts/820223701.html","561b1eb63f998761dea52af81e2ff85d"],["/posts/830372185.html","efe8fcf929ff692362aac6422886b20d"],["/posts/88294277.html","0b1e96f09cc0b45228a8a62870e612fb"],["/posts/939963535.html","de2971d7a8eca90dd32d83e392ce0562"],["/posts/983786067.html","794023c2d64f4e4900915e1da951fe7f"],["/sw-register.js","075aaa656c242160facd28a6bb4fb77b"],["/tags/C/index.html","a3c918678e5cc9ba0ea5bfff8b357b90"],["/tags/C/page/2/index.html","625c2791f5b7095a4fb33a32ca6171fa"],["/tags/C/page/3/index.html","c9605e14d6f580d3fe4d83de37937032"],["/tags/ETL/index.html","60683221b0f44beb277c890aea710878"],["/tags/ElasticSearch/index.html","be34ad31f8c5ab2baf8c26ff45233b68"],["/tags/GUI/index.html","ffc6601e4bebdae591d1b3d50865c7a6"],["/tags/HBase/index.html","0b273c32ecce9ef6667597a29d3a76e4"],["/tags/Hadoop/index.html","c54794ad7b3de2f436a01d6b8da211d2"],["/tags/Hadoop/page/2/index.html","bc2184376feef4d887498fe3c853a27d"],["/tags/Java/index.html","54c19da95265eb0925a9fe6b69ec44af"],["/tags/Java后端/index.html","329d89936ed95566c29170a0e5220c43"],["/tags/Java后端/page/2/index.html","ce8bf7a9b7fcff8371eaadad0c2318c2"],["/tags/Java基础/index.html","b3823126d1bc5a008a1cbba5fa769026"],["/tags/Java基础/page/2/index.html","d4fe680eafc21daf98f93652e6ce1b57"],["/tags/Kettle/index.html","913677adc8146a1618681b05ff44b84b"],["/tags/Kibana/index.html","d10cb1be9859854c38cc443aac69d13a"],["/tags/Linux/index.html","2b21a898aaa11229269bea208212005b"],["/tags/Linux/page/2/index.html","112b870fb64caf60b220347592c62662"],["/tags/Linux/page/3/index.html","c13670074683fdea4152d7c93bcaa010"],["/tags/Mac/index.html","e621b46cd6db1a31eec4fe27daaa0d24"],["/tags/Mac/page/2/index.html","ac3d4fbdcbc07ea73e58ef69b3bc3b64"],["/tags/Maven/index.html","c80ded8eb8328c26b9e1fd3693479011"],["/tags/MySQL/index.html","e36ccbdbcfc304d5a666a500eb3441ab"],["/tags/Python/index.html","c7d465982edf3a552a64224a45531c77"],["/tags/Redis/index.html","0fc9214fe52e451ee911ac7f124278f9"],["/tags/R语言/index.html","6abb0d284a07b5426a840f6e9994151c"],["/tags/Spark/index.html","1c3f38c19b4f1dca0b5ab7e94cab5b5f"],["/tags/Ubuntu/index.html","e3a9ff0b2b09988aff41dac8e3ae48d3"],["/tags/Vue/index.html","6f265241c18ab507c6296c6d68e36cbc"],["/tags/Windows/index.html","c9b44da882c2386b40c22c2ff69cd750"],["/tags/ZooKeeper/index.html","a163aa0863c18fb98ee7003a02e2b3a0"],["/tags/bfs/index.html","b9001e8746f334c51356a01489c99822"],["/tags/dfs/index.html","f74a12437dc89a638a0c2e74f802f1f6"],["/tags/folium/index.html","1036bcb6c5ab6b555a6f63e5f21b3e91"],["/tags/git/index.html","af73d81d48b3abd0e0b09144038faacf"],["/tags/index.html","184affdb119ba6f61c6e8305be4e34d5"],["/tags/latex/index.html","44d76a271741b18703cc83bf62000dec"],["/tags/中间件/index.html","ae6ad51f0f6235f82d0bb318b720e452"],["/tags/二分查找/index.html","86796250e01ae5b5003bc59859f8e931"],["/tags/优化类/index.html","6cd1c8e3e843246a495abcc9d8f46aca"],["/tags/前端/index.html","595358691369f44bae3c1b78033349de"],["/tags/前缀和与差分/index.html","6dde4d2802ec9228b4685c1fe0cca3c1"],["/tags/动态规划/index.html","ec10bc5786aa716eb4b78191fa32c0c2"],["/tags/动态规划/page/2/index.html","1385dd45c7ca387958502e6a3289d733"],["/tags/博客搭建/index.html","e543b29345727a70db5262cff5d50b09"],["/tags/图论/index.html","4f34cceb6b264f0d189433eb0ffc5557"],["/tags/大数据/index.html","ebbd375853964c9bef2a59f306de4ce5"],["/tags/大数据/page/2/index.html","d418d6d707daa94463a2173700708d20"],["/tags/操作系统/index.html","6787e450551b22f892d2e660160354b4"],["/tags/数学建模/index.html","faef9b82431199263c93d36b58ec77c7"],["/tags/数据库/index.html","e9a1b154cdcd504a489ebaf4a7124896"],["/tags/数据结构和算法/index.html","ea3850c1ee193c7a0e118cfc19c1b469"],["/tags/数据结构和算法/page/2/index.html","690132843f6e6aeafed9e857ddf91457"],["/tags/数据结构和算法/page/3/index.html","cc439f0ceb833bbae7f2f66c903d3f58"],["/tags/数据结构和算法/page/4/index.html","034cec71bddf5e921b94d967aca1d6b2"],["/tags/数组和字符串/index.html","102b37120adb93145234a06cc23e6588"],["/tags/数论/index.html","865675a3003720d623bbda12ed13c7e5"],["/tags/枚举类/index.html","869366321e1e9e1bad34e2145e541b2e"],["/tags/栈和队列/index.html","94df381c8636e22c5947ed4cb0b5188e"],["/tags/树论/index.html","7ce12298fb9fdd63dbb44b84eedb5565"],["/tags/测试/index.html","fb9bcde90b36396dd77149b8f055c851"],["/tags/环境/index.html","b137481332637284706ee5600b93cf25"],["/tags/环境变量/index.html","6e48865e8fbdda2dba91a4c954f26376"],["/tags/绘图/index.html","9917be164536b25fa818a022a51f9933"],["/tags/编程工具/index.html","923785d537e989d2013ad4ba956625cb"],["/tags/编程环境/index.html","623599c281711a279c441e7002068b86"],["/tags/网络编程/index.html","6650f9715fff9e15fcd3db5a1f26a78b"],["/tags/英语语法/index.html","38cac2c82cc7f0434c5f942d9f19c2db"],["/tags/计算机操作系统/index.html","0b26f478c84ed26082adbc1f8b12b431"],["/tags/论文/index.html","feffe5c90ee4764a3664af4db19bdfa3"],["/tags/资源下载/index.html","a9a41b3f2ecbb4170f0758a0f6c5af61"],["/tags/链表/index.html","ef122fc390066002e0b21196960cd98b"],["/tags/集合/index.html","eafb1d95ec2e070125374825b59d1c4a"],["/tags/集群/index.html","7c2fe48601c6867c1980aad15ca8d75e"]];
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
