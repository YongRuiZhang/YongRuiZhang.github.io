/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","cda93b8f092231c765c8788193254e92"],["/about/index.html","26ed1e10055323e1ba20e33c1d278e4a"],["/archives/2023/01/index.html","11d42fdfa364c47f508e784f1e559b29"],["/archives/2023/02/index.html","c4f841f47a82b5a32158e07379b239b0"],["/archives/2023/02/page/2/index.html","62fe270e22a1491cedeb0724a0c92130"],["/archives/2023/03/index.html","eed0d72d00c95d0069c5fec9db2ff299"],["/archives/2023/05/index.html","62b990ff6436d844e086e217c8a9cdb9"],["/archives/2023/06/index.html","61c7cf290a2e61407a99fe5b96ede563"],["/archives/2023/09/index.html","70a1f511bb358c117690018452eca477"],["/archives/2023/11/index.html","cd5cebec925403bccc822d75e1e6de64"],["/archives/2023/12/index.html","5492bb6f2a551a52a8a958933d2c9701"],["/archives/2023/index.html","d39b8cd762507d7dcf32e58ef5fb2292"],["/archives/2023/page/2/index.html","b7a5d59cf2e46fa852cf3928495b3faa"],["/archives/2023/page/3/index.html","b84c08f93a6efe87eb6904f761c087d7"],["/archives/2023/page/4/index.html","036092e412c42b799bfb947c7af7771e"],["/archives/2024/02/index.html","77d3bbfbd97fba382c3bc6cc05f23200"],["/archives/2024/index.html","f7e3d78478a6ff826f1e2841391ca3b2"],["/archives/index.html","76ee7ae34da1873a185d0d7851ccb829"],["/archives/page/2/index.html","815cf291dee04ae119e12d8c390c09c1"],["/archives/page/3/index.html","b926a83ea5aa635d9bee564c2891cdc8"],["/archives/page/4/index.html","ea4d412f9d3af780156d63fd91f11bb6"],["/baidu_verify_codeva-qQP2iZOMLX.html","b5b6fecb1c06f6c955314d988c6659b2"],["/categories/Java/index.html","271fc22f573fd0128e5b6803a4330237"],["/categories/Java/后端/index.html","404166b8cc3ddb62ae5dc7df8f395e19"],["/categories/Java/基础/index.html","5d87d2516a641954c34f9bdf2ca1056b"],["/categories/Java/基础/集合/index.html","df5957c08dfb9ed8757e83265247cafa"],["/categories/Python/index.html","0667a26c83fa299c60339ddcb3e3c679"],["/categories/Python/编程环境/index.html","35d3fff6c84f312512e7363609a7cf19"],["/categories/R语言/index.html","d2d943ab8129c4075018130f0a4ac297"],["/categories/R语言/编程环境/index.html","dd0c6836b68d449453514380ccef2f7f"],["/categories/index.html","4f5e3e536a28b7a8e3212c00c2825d2f"],["/categories/中间件/index.html","0404b705f5616eea03b3b2a1457b9de0"],["/categories/前端/Vue/index.html","0182101d9958552235d2cfeebd168214"],["/categories/前端/index.html","cdc8c9a265d28e340c7b9fd88736a008"],["/categories/大数据开发/ElasticSearch/index.html","364064b3887a40cbdf2b61163ac29eb7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7323ab64f25cf63f608272f8551993d0"],["/categories/大数据开发/HBase/index.html","30fdd31ac045927acd6f36cac4db5301"],["/categories/大数据开发/HBase/学习笔记/index.html","df3c79ddbb67e93c8f1fbab36a35b68d"],["/categories/大数据开发/HBase/环境搭建/index.html","8de8b719f0fa1cadc8618673ecefe840"],["/categories/大数据开发/Hadoop/index.html","24807684106fe6b417a018d3a7f3a6e2"],["/categories/大数据开发/Hadoop/技术/index.html","f9af1c317f7d57e1576265d033b6f246"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f9b15b91ced0b60f8960492e76ce46c2"],["/categories/大数据开发/Redis/index.html","949f70570858276aac16b9f447ed412b"],["/categories/大数据开发/Redis/技术/index.html","b1e0e7487edcf183cb08dc893d52ff61"],["/categories/大数据开发/Redis/环境搭建/index.html","bc8acbc14ba394c6eb516689beb669da"],["/categories/大数据开发/Spark/index.html","59f348eb4447084c1d1f356697bf9929"],["/categories/大数据开发/Spark/环境搭建/index.html","356a55334d407e025b7778c4d6dd1f76"],["/categories/大数据开发/Zookeeper/index.html","03602878c96966e88d56ce8b0e714d83"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","3947605a63dca8052bfff8cf18137486"],["/categories/大数据开发/index.html","dc131c6dc5d43111597c156923f10174"],["/categories/学校课程/index.html","aeab5a2e7756b93353e2123d659281aa"],["/categories/学校课程/计算机操作系统/index.html","e003da24a92fb6235947cf14bdeec9d8"],["/categories/操作系统/Linux/index.html","07b17b9b623626041e8054b04ed9d4b7"],["/categories/操作系统/Mac/index.html","e71a0770d5563553764b2f7cbdd596d3"],["/categories/操作系统/Windows/index.html","480234b03399c82c7b773d19bae268b0"],["/categories/操作系统/index.html","195387cdc0a4a2b0feb5c4b4a4838a1f"],["/categories/数学建模/index.html","b0d9e672074c35610b831b1e7c05848e"],["/categories/数学建模/latex/index.html","63712c63c72176702a4c9dc153faad4c"],["/categories/数学建模/优化类/index.html","d0ccf70e35a782aac5d1cb528de76f35"],["/categories/数学建模/优化类/现代优化算法/index.html","1d170784c1f9a90f5883a788085a0221"],["/categories/数学建模/优化类/规划类/index.html","ad47312b20e769211e7eb027a6702092"],["/categories/数学建模/绘图/index.html","d67c048400f75ae03f20da733f448187"],["/categories/数据库/MySQL/index.html","3d2bc7a549a1e21ec8d2c9bba8fca708"],["/categories/数据库/index.html","2e2989222cd26ef889285bd9dcef3943"],["/categories/数据结构和算法/index.html","bcc0ed24ad4ed71d454f3d2cbfd14cbc"],["/categories/数据结构和算法/page/2/index.html","479cc0e9792fa75443ad676132b053f6"],["/categories/数据结构和算法/基本原理/bfs/index.html","ff1702b57af323a59ad88f44a48498b5"],["/categories/数据结构和算法/基本原理/dfs/index.html","44aaf5d4cbbca1d993a00cbebb9398e1"],["/categories/数据结构和算法/基本原理/index.html","b6bb182b0e9e4fb0ed68c1a3caa7dd00"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b0580eda99a874f6a11067e3292410a7"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d65fc891029b24e4f676126541542397"],["/categories/数据结构和算法/基本原理/图论/index.html","254bb7095bb1663b3dac5aa2b119de9d"],["/categories/数据结构和算法/基本原理/字符串/index.html","a8bc31c37f5e9d531bcdaa60aed6cf4d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b21735eeca8cc4742cefdaedec8aaf09"],["/categories/数据结构和算法/基本原理/数论/index.html","0e2c16171a6a5cf27f1fe90ea103cbc9"],["/categories/数据结构和算法/基本原理/树论/index.html","0e9e2bc16f0a5eee8faba6b2eb2864cc"],["/categories/数据结构和算法/基本原理/链表/index.html","b9d7c2189c6507df90a4b92c9842dd9b"],["/categories/数据结构和算法/算法题/index.html","106c76097a8c1108e03598e2577806f3"],["/categories/数据结构和算法/算法题/二分查找/index.html","e59f27b939890765f1ab52573aafd411"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ba91384237bd526d556213eec367661e"],["/categories/数据结构和算法/算法题/动态规划/index.html","732a2afd3060a15488d083f4e35c17b5"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3961025242e936e885e64d0243ba2d2d"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","e8e2872138c761741e7b3aab93474977"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","03bb6db9eaae0b9455d032062501f630"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","729a9f2dd28457f7780daa3b720da415"],["/categories/数据结构和算法/算法题/数论/index.html","559e5afb459b0770db6d7ec62b05c4e6"],["/categories/数据结构和算法/算法题/栈和队列/index.html","e6534f10dbb84c76a45656ce70dba6d5"],["/categories/数据结构和算法/算法题/树论/index.html","1326134a0587fce905a0ee2b9cf53644"],["/categories/杂七杂八/index.html","0b58e4e4888ca3dc04975cbb92aa07e2"],["/categories/杂七杂八/博客搭建/index.html","e9b45a6eb6ae95f9fcde71f68bcf81a3"],["/categories/编程工具下载/index.html","7aaabd27bc8047655faea5f416b92aa3"],["/categories/编程环境/index.html","5e3ce2fd0ce9137299b3e2d1e1ebd914"],["/categories/编程环境/大数据/index.html","b969bb8555815889f70845942dbf7e36"],["/categories/英语学习/index.html","033fdb454e2da98856a0f239140567fa"],["/categories/英语学习/英语语法/index.html","a864537893f1a9b66e065a8e5e2e4832"],["/comments/index.html","e6074255cbc6e966e6068e2db3f6cebe"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","462e725af66e6bcd888c2d5bbeee8867"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","623cfd408b4c45ac9a5dfbbffa1cec20"],["/movies/index.html","094bb1ef7ca992a9a412cc0294b4b6dc"],["/music/index.html","f4a48bff69597bedaafaea40a7e24bb2"],["/page/2/index.html","d1be3bb239d2a9de1e45fc08bddf9d1f"],["/page/3/index.html","dd743a63a197b75fe397eafad71dd040"],["/page/4/index.html","0ced94e7c88c7b7072e6a248e2cb6475"],["/page/5/index.html","baae2e51b2f57602aa7073155eccf9e3"],["/page/6/index.html","b70479b7d40fb620a82e280622a63ae4"],["/posts/1021360842.html","cce170b86dff89c369f9eb42eb6a14d5"],["/posts/1120620192.html","b00551dd665ef088e9bc1be26b818083"],["/posts/1141628095.html","2b5ba206977546f5ebeb222b2e781906"],["/posts/1168613674.html","94f02bbf9c7187ce18012d9a09c57c0c"],["/posts/1219920510.html","7b80d2c923b1664c592ff8cd253cb13a"],["/posts/1222166338.html","40a27cf6cd1197083ba5aafbb94089c9"],["/posts/1259097482.html","a0341afbb65f0e42c1897e7a1a2aaafc"],["/posts/1271036369.html","5383d5843ba51ef8a5d13360ef120742"],["/posts/1312847445.html","4d5071f5e6aae22a641c5d74579375c6"],["/posts/135355774.html","a0a213cc181f5c729ef5a0c3ab06a374"],["/posts/1375344716.html","87c357530caa6b5f9be5603bc8b4ede5"],["/posts/1388991698.html","f07b71817d07f9b0c154a73c0bfb53d7"],["/posts/1410315814.html","ffeaa15b3d5a9d4ed7ee9c28fa1fea56"],["/posts/1452790229.html","61b15ac9b92bf1d49016967d9155d86a"],["/posts/1470079884.html","b180ad763ebd0a59f512e5925a38f120"],["/posts/1470079885.html","dab65a567fe66ebbe348d942c4692738"],["/posts/1470079886.html","b21cd431d74f052dfe49a50e94c42fdb"],["/posts/1470079887.html","a39dedc9cdb04c8cc257538af0354f45"],["/posts/1498536549.html","df6a74ab81835666b21032c9f7d08a23"],["/posts/1547067935.html","06d186a9dfcced6ddb734736ecdd63aa"],["/posts/1557866301.html","0bca50a802c486de0dd19a97ba4d8735"],["/posts/1571776361.html","131c7a1d2b93e1e67430de7482baa759"],["/posts/1605124548.html","8d57939fbb0abb9312230d43d6f53b94"],["/posts/1633036852.html","3bdbf6a0703aa16b4e4be2943c317e7f"],["/posts/1674202625.html","d4f09ce89bddd56c7f5c76d009eff94b"],["/posts/1765123828.html","c7e0f5908aa64dbbdfd7689576718d6c"],["/posts/1767336200.html","28b0a8e4e00d31e02290f7acad69c8e3"],["/posts/1776114197.html","75ca54962f2ade905c3364e10d4fe4c5"],["/posts/1817748743.html","9596ed2c4bb534e7e78d5d90f42541f6"],["/posts/1925125395.html","a32f3a9d81ff5d14dc15868b0a73208f"],["/posts/1966191251.html","b181d2ba1bbcda89a333b6b2e6f63135"],["/posts/1987617322.html","17a2dcc05ae594ac0bbed411cdfb003e"],["/posts/1999788039.html","b1828cfdc026f6544adf9f4c6df7084f"],["/posts/2075104059.html","50c04f3cebda6d67eaa7b8dedf1a67fb"],["/posts/2087796737.html","139ec307feede257c38297e6badb4a57"],["/posts/2106547339.html","c9736e0eb143378a966d04d314077493"],["/posts/2207806286.html","27ba78ea83260c4bb427124178292fdb"],["/posts/2225903441.html","59837ec8be8e25ad218522ab1d1beead"],["/posts/2265610284.html","96d8935eaa383c2b80d64d73dde88cf3"],["/posts/2281352001.html","dcc6e0e290366ca1b8f239dbd89a7116"],["/posts/2364755265.html","0ad5a995a8e25f9e3dc8229b38626807"],["/posts/2414116852.html","3a171907ba0520e9b50568c8f77a95c8"],["/posts/2421785022.html","178d535aa156b255f473f5b03e70f6a8"],["/posts/2482902029.html","8a01461437c4d2e70cf47c48e77420bc"],["/posts/2495386210.html","953323fcdd420b2cc2131e7249419628"],["/posts/2516528882.html","0b32a9b89fc2e800885dd144e940304e"],["/posts/2526659543.html","f838e6b8bbdba89ce033f7a92e9e6480"],["/posts/2529807823.html","019769af4103eca3385b8ce81fe6cb33"],["/posts/2596601004.html","dcb43850b4d2588d21a68014be766da0"],["/posts/2742438348.html","4ccf89ca5a5444685254a5a23d0cc94f"],["/posts/2864584994.html","b07698d04808109fe863e1620bf93514"],["/posts/2888309600.html","a3565e7b5784b3f8a757923a6c4f1a0c"],["/posts/2891591958.html","60b5d1c0250bcbf112fb6cf250f3f80b"],["/posts/2909934084.html","bec590889e4a7c1be128fc8e64fbe943"],["/posts/2920256992.html","03842c8eebffdc3e7438d50ed5480d90"],["/posts/2959474469.html","96dd328e6088834b3881ff0733bd72b4"],["/posts/3005926051.html","7ece09f171eb6f8f60f620a6006a073d"],["/posts/309775400.html","3dfcfc1ca7338b80b0291c60aea017db"],["/posts/3156194925.html","579630c9343d8352db91644b66b3d6e7"],["/posts/3169224211.html","e75e7b685f1ac8bc86b9175c3c65de44"],["/posts/3213899550.html","5d1603839109027328949b6b7e16eef7"],["/posts/3259212833.html","b6ed046fcac44f07877fc11ec67a35ce"],["/posts/3266130344.html","1f5cd05aec3861bed62ce2612f2f0d57"],["/posts/3292663995.html","37dd0afcfe0a18213042cf8fb05ffda3"],["/posts/3297135020.html","84b1477805f11cfa2ded430f2aa1313a"],["/posts/3306641566.html","526666cdbad1f3d3bda7cf4ad0655888"],["/posts/3312011324.html","9d56bee511dd4d71a935a9dec0a394e9"],["/posts/336911618.html","b3e4a69831212412719f3c41ecfb5818"],["/posts/3402121571.html","ff9cab2d75d8a349576f2cfe392b49d0"],["/posts/3405577485.html","482af8f1392987026884f7a3bd7db5dc"],["/posts/3498516849.html","a21328f4cab42d6559429aff754738e1"],["/posts/3513711414.html","47220b5d0f93102d75030f9d992022ef"],["/posts/3523095624.html","faebdd76a2baa53096a6b7e1faf75c79"],["/posts/3546711884.html","4d8264c213de6826c16b4eaf2df782f3"],["/posts/3731385230.html","6cb17c819d016beccbf2333f4b5cac5d"],["/posts/3772089482.html","e8d5a87f07f736220e8f7d6b32dbfd72"],["/posts/386609427.html","270fde7322519fbde2624174d0f91c53"],["/posts/4044235327.html","7bcd1c96aa81da16ed6010c79eb797a3"],["/posts/4115971639.html","d92e7b64a200f4af1977dcb4b5e40341"],["/posts/4130790367.html","bd5a39b732c7635e53f837222fb053c6"],["/posts/4131986683.html","eb2a9f4f27b0075419a5d8c167f12692"],["/posts/4177218757.html","a291cc33f41f27f21c7611d92c682599"],["/posts/4192183953.html","2c5d6fd12bd1f02189adcab1422e6b50"],["/posts/4261103898.html","216995df3949e8ef095b85441b5e1e2b"],["/posts/469711973.html","03cba90574906a759e3a7eb22547cf57"],["/posts/482495853.html","9475e09aef82bb5ba29704f0aaa86f2e"],["/posts/488247922.html","eb4e11a51fadb8944dc18f5df0c725a6"],["/posts/517302816.html","9a052d0d1bd61b93def0abcd78148006"],["/posts/570165348.html","e33f3458e4337dc8dd78871bf33c4d63"],["/posts/595890772.html","222fc9e45ae3611c01fabfc3baf68646"],["/posts/67485572.html","d323f9347d1875b45aba4b0410e94ade"],["/posts/694347442.html","13423127fbe5c79d1d48ec523dd1d40d"],["/posts/707384687.html","bee1b4bd838d509b99f1b6ae09e08504"],["/posts/71180092.html","c8fad62cd2110c298254ab8e043c94a6"],["/posts/716459272.html","dabd664cb9b56c582b67c99152998555"],["/posts/765481613.html","35189008bd5c1db5e5e721e92778964f"],["/posts/778231993.html","576c92e594f2a2a994afc4da3f046805"],["/posts/795397410.html","8de08ea22998e48bfeb776fe4cf88912"],["/posts/820223701.html","561b1eb63f998761dea52af81e2ff85d"],["/posts/830372185.html","efe8fcf929ff692362aac6422886b20d"],["/posts/88294277.html","0b1e96f09cc0b45228a8a62870e612fb"],["/posts/939963535.html","de2971d7a8eca90dd32d83e392ce0562"],["/posts/983786067.html","794023c2d64f4e4900915e1da951fe7f"],["/sw-register.js","fa0e0c51abfd6d753ec63d3795c478f3"],["/tags/C/index.html","e3ec8df1cf384f7de360eb5056d6accb"],["/tags/C/page/2/index.html","7277e9cab6ce50f74dfc910a53177d87"],["/tags/C/page/3/index.html","820eb8c9115f3ff2f60e66824c0ac6ae"],["/tags/ETL/index.html","a59c2c0ce2c6222329a5ff5e53679f92"],["/tags/ElasticSearch/index.html","0edbc03d8817299adf3dba31a902a2f0"],["/tags/GUI/index.html","8bd6ada6508c9d285167a3ea8d99e472"],["/tags/HBase/index.html","3099e5ba7a71cc08bed3274afe062a17"],["/tags/Hadoop/index.html","5ec55233b55ee0592e910e037efdbce8"],["/tags/Hadoop/page/2/index.html","741a2765411bb3332ba2bee816def27b"],["/tags/Java/index.html","f65edede71a13a485b9464bd1588fb2e"],["/tags/Java后端/index.html","77fb1574d78d3beb4fe891225c1cd5cc"],["/tags/Java后端/page/2/index.html","4452dd28e33ada1728dae283768f6b06"],["/tags/Java基础/index.html","dbbf20b86852c76ec6260d9eef6af0d2"],["/tags/Java基础/page/2/index.html","1ac36c8221f72f6772dfabe1121692c2"],["/tags/Kettle/index.html","4b78ec0f912ff63c76cde537684febcd"],["/tags/Kibana/index.html","4dd9591b4e5de37f18554e82332e8811"],["/tags/Linux/index.html","fae59525fb5719ea5a772348f8734267"],["/tags/Linux/page/2/index.html","681fc48c65607f64d42a96c0464599e2"],["/tags/Linux/page/3/index.html","7dc6e36e3884c700cdc8cae39aaf286d"],["/tags/Mac/index.html","62578d67ed0397ede0191c71d8e6d164"],["/tags/Mac/page/2/index.html","b07898ae198b54d0bce9d5eb36a2534a"],["/tags/Maven/index.html","9c352ebfc9861df645977625b751d320"],["/tags/MySQL/index.html","250d649fcd25c45442a9ca66fa81bf4b"],["/tags/Python/index.html","cdebfd55f12528943d93258923e1e780"],["/tags/Redis/index.html","121ce55e25337449e8d8a4cbf3565252"],["/tags/R语言/index.html","8a727da17f15a84b16c78126d2e1d66d"],["/tags/Spark/index.html","2282a280a44567910ff3e1014b3204ef"],["/tags/Ubuntu/index.html","d0d8cc2a5511344273394f98e205e32e"],["/tags/Vue/index.html","affb79715417d3bd37b0c38698c64ba6"],["/tags/Windows/index.html","029d4592efeb6476392e327eb7d92689"],["/tags/ZooKeeper/index.html","8d30283db6001b28fab93601c62be91f"],["/tags/bfs/index.html","42861668686dd1eaf001a8eeadafcc91"],["/tags/dfs/index.html","d5a0b4a28291650fb9b668f7b9f915fb"],["/tags/folium/index.html","41b6b82c700bd3820015fa3f6f7ccc32"],["/tags/git/index.html","f396dcb17de2d29efcf52917e9a1d91a"],["/tags/index.html","7a5c75933d6f801767442fd9902c4515"],["/tags/latex/index.html","11354deacda0633d0676746e31d92d31"],["/tags/中间件/index.html","c909b6adfab3341ab34d1de77010a79a"],["/tags/二分查找/index.html","702079048022aef930fc9d0bf4012e65"],["/tags/优化类/index.html","198777f42daaf3091665c26dbdf58e9e"],["/tags/前端/index.html","d54092f714079628c6d3b89f9a2e36f8"],["/tags/前缀和与差分/index.html","8e213d65dc542f7035fee76624fe7755"],["/tags/动态规划/index.html","13fd18924d31c61c5194694e2acc05b2"],["/tags/动态规划/page/2/index.html","0005678e198a59912d875ce04270f79e"],["/tags/博客搭建/index.html","375212e45d89a5c89996f5d232c66062"],["/tags/图论/index.html","727ccfd3de76738d3d24a15ccf38ee77"],["/tags/大数据/index.html","dd1b70c9e4b8a4e60b6d5fe7993180eb"],["/tags/大数据/page/2/index.html","79063b21269ac058fb5a0c2da3bff146"],["/tags/操作系统/index.html","5aab52772b6e06199519799b5c4df117"],["/tags/数学建模/index.html","dcb47f6640f7b23fcd47339a8b50be9c"],["/tags/数据库/index.html","b6e05ad5c96a4dca1769b9939603ae72"],["/tags/数据结构和算法/index.html","4400db70a9bb295acca590245760e9ea"],["/tags/数据结构和算法/page/2/index.html","16aafcace2e6b7cdb730e3eb78ea4a66"],["/tags/数据结构和算法/page/3/index.html","bff2432e0f01fbddc6313f8e473e0192"],["/tags/数据结构和算法/page/4/index.html","30b84f31cba4f58de5b429d94d49eda3"],["/tags/数组和字符串/index.html","abdf8f0389cdba5b5469870f9b9c5dd8"],["/tags/数论/index.html","8c4a3eb1e1edafffd749425fe5af8610"],["/tags/枚举类/index.html","f416fdf90f1705f0e6a3ae8ed91992f1"],["/tags/栈和队列/index.html","c21e0406069c84ccca9286b6504a5147"],["/tags/树论/index.html","cbe4ae7c88977a36fd50d5b46079f7c5"],["/tags/测试/index.html","a91bd9a32b78ce9436db5f0d8f6abca9"],["/tags/环境/index.html","31bd14063431330ecfaf55e03890a2a0"],["/tags/环境变量/index.html","fa73e03516f99b7ebc7490923529c98d"],["/tags/绘图/index.html","7098eee136d22a784039b2b49d4659d3"],["/tags/编程工具/index.html","9c7ab76bdf8945f3a6fe37012d702392"],["/tags/编程环境/index.html","d9c798bfd558d6f8fb17cdddd3ce73f4"],["/tags/网络编程/index.html","fdc43a7def1b4f66e92ca7b39f587300"],["/tags/英语语法/index.html","4b40255b33ed68cc0d528a7763be1e74"],["/tags/计算机操作系统/index.html","779abab58f004d1740cab6eb6949014d"],["/tags/论文/index.html","a2c9e8bacc98da3e8ace0ed5ce408f1b"],["/tags/资源下载/index.html","ad2d289d619ac2734e4a9b9a546ce382"],["/tags/链表/index.html","f7c8b92e3b82e43e8d37152cc5ffa69a"],["/tags/集合/index.html","5496bfeea383cebd95bd2622b4dc4df4"],["/tags/集群/index.html","938d3f43e3fa08d2d94ce262b6cdab52"]];
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
