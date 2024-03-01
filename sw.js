/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","a3475081eddb12f996f87d94601686c4"],["/about/index.html","c35e03d5c28a3fd1df9423c73905be2c"],["/archives/2023/01/index.html","179732f838f66f05c4e180551abaa619"],["/archives/2023/02/index.html","d05c3cfed404b58f56b45630ec023ebe"],["/archives/2023/02/page/2/index.html","9166bdb04f50d5ec8b76c4e75fa1014e"],["/archives/2023/02/page/3/index.html","ed337abed57ca4e61dc3d55bbd39f384"],["/archives/2023/03/index.html","9e91e0674640ab22d9712ede1d9e9c04"],["/archives/2023/05/index.html","0015c52069903433a8e6b7c7404f211d"],["/archives/2023/06/index.html","f6ddb0ea132cd2682bce32a2f8b0eef1"],["/archives/2023/09/index.html","f32eb08fbf8d58bb3b98bee5d471c52f"],["/archives/2023/11/index.html","5573a240cf6f329fc8201b0cc81c7c47"],["/archives/2023/12/index.html","82bd521e36fe1bba9698381adf631a79"],["/archives/2023/index.html","9a2d14943564bc1926508d36d1dcd863"],["/archives/2023/page/2/index.html","c8a667f703ffa9d422148ffe77ee4370"],["/archives/2023/page/3/index.html","32cc24656e8e6eda67e99de8b0d51b91"],["/archives/2023/page/4/index.html","f21ce2e6594c9edd989724aced70dad9"],["/archives/2023/page/5/index.html","20e2d956b8eab96e56685ac4b24faf9e"],["/archives/2024/02/index.html","80e0d38ac7f66305d28ffb6484c5d46a"],["/archives/2024/index.html","0a4a682718b089f802e138ccbce7a5fb"],["/archives/index.html","24341cf1ce7dea6740fb27aede4e4346"],["/archives/page/2/index.html","1c8d7ee78af04c0f8bafe6f698f616ef"],["/archives/page/3/index.html","59b7f28a6b2e0b639d132beb2e54cf6e"],["/archives/page/4/index.html","4f253049d73593167e178d9f732e6436"],["/archives/page/5/index.html","78b291e977305ff12d4f9551fcf569e7"],["/baidu_verify_codeva-qQP2iZOMLX.html","eefb02f9d919a13af109fe0074cac9ac"],["/categories/Java/index.html","0d1e88d62b0afd0ce663b308cef5b891"],["/categories/Java/后端/index.html","e07c708e94584d816e2f5bc7698e1bc0"],["/categories/Java/基础/index.html","742f809669176590fe51c7463bc04f0e"],["/categories/Java/基础/集合/index.html","924f4b3ab00f8b5eaf8501c40841cafb"],["/categories/Python/index.html","217f927de6bf4f654bb936a166ebfd89"],["/categories/Python/编程环境/index.html","f0a0dd4468c27d170cab323dea84b668"],["/categories/R语言/index.html","d3957841580296a8d672fea4bb328e39"],["/categories/R语言/编程环境/index.html","48a653d812f1d7603f7d1b6e6b139596"],["/categories/iPad/index.html","df1eacafc12d28036f75f36a8cb7f37c"],["/categories/index.html","18526a8130e1a4454262d12d6ef2f76e"],["/categories/中间件/index.html","335558e6e09deca59f02b36c93186863"],["/categories/前端/Vue/index.html","2eeb5fdb3ae3aafc5153b6ff638e6c9f"],["/categories/前端/index.html","47ac449298538f2a96eb784e863c6672"],["/categories/大数据开发/ElasticSearch/index.html","64134e5addf5eb4b21f50be4f797a6af"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7d357e2774409376f9d69335a2ad4c1a"],["/categories/大数据开发/HBase/index.html","b0403e347e2d023f192e4ac369edac5b"],["/categories/大数据开发/HBase/学习笔记/index.html","73e0794cd44031b5c3c399ae1ecdeedd"],["/categories/大数据开发/HBase/环境搭建/index.html","25b9cffa92a0376592e696c69cd740cd"],["/categories/大数据开发/Hadoop/index.html","12f9347704e93988da836b3b88a62892"],["/categories/大数据开发/Hadoop/技术/index.html","a96747835b1a1be887e31419b0ad87d5"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9938a0946e8aa0944345b4f88ac68afc"],["/categories/大数据开发/Redis/index.html","d6ff6ff20e4bd452fe3652e92c777f3e"],["/categories/大数据开发/Redis/技术/index.html","b6c189fbb93439bf741782a649a0ed82"],["/categories/大数据开发/Redis/环境搭建/index.html","ddd7cd9369568a2f3fb15000e4029c62"],["/categories/大数据开发/Spark/index.html","c9b3629d4db5781ee27f92c35c881288"],["/categories/大数据开发/Spark/环境搭建/index.html","da7991a930726fcda2dcd4a37d08b534"],["/categories/大数据开发/Zookeeper/index.html","5cf38b67effb9dd90320955de49d72a8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f8f5179c0e8efec6799b904a7515182b"],["/categories/大数据开发/index.html","8cd3c4795fa330220824a187982b9698"],["/categories/学校课程/index.html","d7de8565966fd30e30ff41beab7efdcd"],["/categories/学校课程/计算机操作系统/index.html","201e0e93ecf68c93f3cab39b631ebb6f"],["/categories/操作系统/Linux/index.html","916603843d345e0562f2b7a946acda49"],["/categories/操作系统/Mac/index.html","8332beb87e81eae234072bfe48ff9e41"],["/categories/操作系统/Windows/index.html","9b23f6b120d6988b40e7c8596cd2c885"],["/categories/操作系统/index.html","9cc25b6af23da786a2a15881a55a479c"],["/categories/数学建模/index.html","0bc8cbb0fc14c27c6aa640c848d3b1f4"],["/categories/数学建模/latex/index.html","fc628e3ce4e5598e92017b4468eac775"],["/categories/数学建模/优化类/index.html","5a5a57a3faec1488179fa10d3394eb7b"],["/categories/数学建模/优化类/现代优化算法/index.html","bec42c4649a56d648dc7edf2cd82bb86"],["/categories/数学建模/优化类/规划类/index.html","0e4d9bafc31744407c909b6168fb5e36"],["/categories/数学建模/绘图/index.html","13eac12dfd0ac63fca874c26e8811706"],["/categories/数据库/MySQL/index.html","ded2eec29e8609a99907170cf38bd87f"],["/categories/数据库/index.html","e270e3e8e6ebd71f2d068896a7a0e30b"],["/categories/数据结构和算法/index.html","34a8c84aa5ac0fcb9942c863c4aa8e54"],["/categories/数据结构和算法/page/2/index.html","43d9f52ff1b0875cdb512ab1d4dd4e5d"],["/categories/数据结构和算法/基本原理/bfs/index.html","d8dda1777372d473b2e8d3f6dfba473c"],["/categories/数据结构和算法/基本原理/dfs/index.html","db51dc90a5f9606a66dc2162f57b7592"],["/categories/数据结构和算法/基本原理/index.html","708ef589b3a2235ebc21b4b739cd3bde"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","5892e78a521b6e364afb2a728394acfe"],["/categories/数据结构和算法/基本原理/动态规划/index.html","8cd1e5d1a2ba7b718dbe9a78690f20ee"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5c644fdb9199d8d6e7d911dd53464305"],["/categories/数据结构和算法/基本原理/图论/index.html","84fad72662e6363d58489c5b54370264"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","feaba2e572bbee6abd0df6e9ecb4e77c"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","9ffd9c85a530327c2e0badfb78dc58fb"],["/categories/数据结构和算法/基本原理/字符串/index.html","d0d04f976702ee46a365e48b982355b0"],["/categories/数据结构和算法/基本原理/宽度优先搜索算法/index.html","1c5a83fe82769e3b87452545eafbcf7b"],["/categories/数据结构和算法/基本原理/排序/index.html","0bc8aeb7fea091f20c02930b6b5ad42a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","c6eb036ab216f7da657112fef49d99a1"],["/categories/数据结构和算法/基本原理/深度优先搜索算法/index.html","c4d48fa7fb5eddf40063556310ae065e"],["/categories/数据结构和算法/基本原理/链表/index.html","061f8e7a2aa3840208426f9933af777f"],["/categories/数据结构和算法/算法题/index.html","6cfe236cb699b410f8b2b9ba39541ac0"],["/categories/数据结构和算法/算法题/二分查找/index.html","5823633877c0bf56e41952576e83a7ac"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","8d8165b4a933aba4d28839a09ec0c789"],["/categories/数据结构和算法/算法题/动态规划/index.html","6bd804fa7f2ffbf6b115e9b429b87923"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","4dcf0ffb506291277ba50d432bb5aadc"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","809ba9c37dd986f32d8e23286ad8522d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d977d3dd106349bfd4b567da22c8e170"],["/categories/数据结构和算法/算法题/图论/index.html","16835dd4ff169c740c4f175dc85cb4ac"],["/categories/数据结构和算法/算法题/图论/树论/index.html","073ebb667bc2ee54cd3511c828cca8ac"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","8386ad14bffb24396c23c12255e7c669"],["/categories/数据结构和算法/算法题/数论/index.html","84b0ba82e27c44e640c999ccf9e80754"],["/categories/数据结构和算法/算法题/栈和队列/index.html","71282897f78232a973b52cba339747c1"],["/categories/杂七杂八/index.html","20ba4692b4a24ec8820e83a909ab5e90"],["/categories/杂七杂八/博客搭建/index.html","bfaa4fc1225c33541e72c98f9d931d09"],["/categories/编程工具下载/index.html","a55238e0ec8cf468bb8e6cfa39c044f6"],["/categories/编程环境/index.html","c1bd7fda8f6779ac7dcbae0b4da56a27"],["/categories/编程环境/大数据/index.html","8eaac046b2455c231a8558bb34f43291"],["/categories/英语学习/index.html","0900a8f80ee6cac54bc6d4fa0b326e63"],["/categories/英语学习/英语语法/index.html","79ddde1eabebe2f2495dcc377dffd0a8"],["/comments/index.html","71885f9bf3f1a9015b58e302bef93079"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","75d302729eee9459b5cebd03291584de"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","bc86a1e6a49978f5d9c12b6888a6320c"],["/movies/index.html","11f66fab1e89fc81d6c38f1ae4734532"],["/music/index.html","9833f219228a382edc591b87f5f026d2"],["/page/2/index.html","244176e9f06b472e98274f16f6f99296"],["/page/3/index.html","49f1abdb43014bc80607836aa83dcd4e"],["/page/4/index.html","adb8724e5580b07bb2789ce11e3413e5"],["/page/5/index.html","c7f7351b76f4b0c3e4ee66aa3adb8a08"],["/page/6/index.html","00a7af35284a72a341a2edc651b03d9d"],["/page/7/index.html","30186992eea586f10819fa748432d4b0"],["/posts/1021360842.html","77bcec29c94109b4997ea2e23cf35666"],["/posts/1120620192.html","0ad9cdc89022c950665f3fb69a4e601b"],["/posts/1137707673.html","4b361cca261bb70610763f14df05d85f"],["/posts/1141628095.html","0b8651ecfbea467b884f7449a8350ced"],["/posts/1168613674.html","f295b00aaddb1594f64cdb4e8d4e29bd"],["/posts/1219920510.html","fb884f5c33103d35b54000c4b7e19462"],["/posts/1222166338.html","ec8c256776e24b096bfe33544e534345"],["/posts/1259097482.html","0074e784433695ec59f3965325700583"],["/posts/1271036369.html","91570b2ec412612663c7bc733e7aad9c"],["/posts/1312847445.html","90e175dbb0984859141600f2e91aa2f1"],["/posts/135355774.html","1dbf7c76ca6f6451313faaca449ff848"],["/posts/1375344716.html","7b1f6edd47ddd7c3240fa4e61f872a1c"],["/posts/1388991698.html","6a0dc7228e538e291ba885f0be4dfc60"],["/posts/1410315814.html","0de16a2d2f37a6cd7afb2a775386c9f9"],["/posts/1452790229.html","213c70456b3d427a811a670a902861ee"],["/posts/1470079884.html","1003ebdbfc246765e1daa84aabeab73b"],["/posts/1470079885.html","239c9ffe6fc78676625bdd03d93ce038"],["/posts/1470079886.html","6e75139ac2497d5794797204b09140dc"],["/posts/1470079887.html","47e8c4f903597ddbab6a097e1a5313f2"],["/posts/1498536549.html","3943645ac5ec66a297dfea7415d5f009"],["/posts/1539568593.html","90117d5eba9b52e542ebad15339a90db"],["/posts/1547067935.html","f733739147a1d92b35d4d1c184fbc0c7"],["/posts/1557866301.html","c7a9c395896d8dd049c255ae9267dffd"],["/posts/1571776361.html","6314a396dc195550a3cc5a0f942498da"],["/posts/1605124548.html","36dbb41799281539f249e5614df77f5c"],["/posts/1633036852.html","8dd59be2b3ba6531bf6546c411c3a095"],["/posts/1667740714.html","1a4d0158cf5d498da76c8f2f7e363293"],["/posts/1674202625.html","41f15277dfccb3f311515d667aa2ddbe"],["/posts/1765123828.html","f2d9d8bdc9484b25f3c7ec88469d7626"],["/posts/1767336200.html","f0b7bf00558b87edd2fecd7c992d820e"],["/posts/1776114197.html","c0b41a08cd286f18ff7dff8fe125ab37"],["/posts/1817748743.html","d987a9bfd3728839024ade31a222ce9e"],["/posts/1925125395.html","8323b4c0332c355bb8ec4056392cd73a"],["/posts/1966191251.html","ef62fe0b9ccb750369f7c0dfbf0b4617"],["/posts/1987617322.html","b2e05bcf195f67783b2d145a48f161f0"],["/posts/1999788039.html","08673eef7f83fb7dd424b04338bd2376"],["/posts/2007534187.html","0399f1fe4bb4e1759f3c6672eed78666"],["/posts/2075104059.html","aba323809c2772531730b79992a8d1da"],["/posts/2087796737.html","66a4499bb236744eb7b395364c3e7815"],["/posts/2106547339.html","a50691d4eb5baad47a5cfc92988150f0"],["/posts/2207806286.html","1c75a024a5f1f53116d1f60e40f17c08"],["/posts/2225903441.html","55dd2b49ab846f5089cf982c835144e1"],["/posts/2265610284.html","3c93349b47dc176b8b24495dced63092"],["/posts/2281352001.html","4c97f21757712fac121343c3918c6191"],["/posts/2364755265.html","47672949c839382d0991c256dba967ec"],["/posts/2414116852.html","6787e3721f71f9bdbd38a49ab67382bb"],["/posts/2421785022.html","2b67b422656c317ff38405272809c47e"],["/posts/2482902029.html","831d92810d8edfb438ca752d41a5f424"],["/posts/2495386210.html","5a6a1bddbdc42b17127c4c187c32abf0"],["/posts/2516528882.html","7be7f175cac96d5488548ef1f8045747"],["/posts/2522177458.html","1ca86dd02ea6f8036e6870662a0afb7f"],["/posts/2526659543.html","bb449e95b988796d2554034e29241fb9"],["/posts/2529807823.html","4e4d548e44a220644ae434c9f51ee48c"],["/posts/2592249117.html","7ef23f49af29acfc61238cb97dea9e1d"],["/posts/2596601004.html","331446ce99c49857f0688281c8095387"],["/posts/2697614349.html","a1732a33014eb3ac43628d25dd9f177f"],["/posts/2742438348.html","1310d491de239b5893181302f7ce6cfc"],["/posts/2768249503.html","c7b9c75cd64a18651b3a240fb0a4c640"],["/posts/2864584994.html","dd9805dc384caa6d09d120e838490e6f"],["/posts/2888309600.html","14adbf03f673f90459f60e721fce8c01"],["/posts/2891591958.html","d23842833232f729d0e9c0e0671248ce"],["/posts/2909934084.html","9e8bca7cda28fccc353cf5faf1925462"],["/posts/2920256992.html","d477e417820d98aff35959dfdce02846"],["/posts/2959474469.html","2357a1c8df7af18d35c79af2ed091c99"],["/posts/3005926051.html","5408bada9cbaff60f393024edb4cfc71"],["/posts/309775400.html","1c886202fad9f5a3d68a3690d3d5bb2b"],["/posts/3156194925.html","ca3e6994eee9bfb9c4e708e169607497"],["/posts/3169224211.html","3db2c2e02f426c6ca4ba85f18b93da28"],["/posts/3183912587.html","725050998c2db0043d3faf37f46ac225"],["/posts/3213899550.html","33b32b65f04fc887366ed38cb4b330a4"],["/posts/3259212833.html","83626338fefbe6d575a033360c04f2e2"],["/posts/3265658309.html","59c5dc35a3d256e4029c00ffcfed520d"],["/posts/3266130344.html","95b5c461036fed4f08b247d14121c839"],["/posts/3292663995.html","427a3a458e10fe705a81ce768f3bb370"],["/posts/3297135020.html","3c811729582bf9784ff59a5c4bac1cf1"],["/posts/3306641566.html","885b4a451a9048ccd01981dcf84f760c"],["/posts/3312011324.html","e33794a3de36c8eea8d02b3209ba2e6a"],["/posts/336911618.html","dc44a411ac72e3b90ba4ced395eb2445"],["/posts/3402121571.html","abf45dc041e63aeafb118bcebd89dcc2"],["/posts/3405577485.html","efe417132d8843582094f267191396e9"],["/posts/3498516849.html","a3fd9f14f6a77a53d89a9bd9ed2dded9"],["/posts/350679531.html","8fb767c08640fdcdbd034b9eacc0dc05"],["/posts/3513711414.html","cc316fdd8383adc16c3f76833a52f7b9"],["/posts/3523095624.html","08a76140ec39704ef2e0d42c075f89be"],["/posts/3546711884.html","c1d6c50d480ca8e6d5e15c65cf482c45"],["/posts/362397694.html","0b3a8d0398df3ea1fc8c682bf9ef37d2"],["/posts/3731385230.html","a4f6844a6462c46ab599ef4fd9ec4247"],["/posts/3772089482.html","886fe3e98ce297f11c8125ce907a47af"],["/posts/386609427.html","96316995dd6916706aba871719cc5b38"],["/posts/4044235327.html","e72b129cf8be79549deee8b02d4e1857"],["/posts/4098221856.html","c6838606121a9e867c99ca52c7447bd3"],["/posts/4115971639.html","236c8ca03e2093378b64ebaa3f1937aa"],["/posts/4130790367.html","54e2e0dd7875ac8545756fc080efc40f"],["/posts/4131986683.html","9e2db17cad2227ca9101b17959610432"],["/posts/4177218757.html","ddc51f62efb88bd018882f1314774fd0"],["/posts/4192183953.html","5e2a6f9c850c5cfbb49153800ea3d261"],["/posts/4223662913.html","78b9bc70c8f2cc3d0e45f8ba86374e21"],["/posts/4261103898.html","abde70efb4df4b1e7ec47a79a21e6154"],["/posts/4286605504.html","5df29b8b8a52d8a7f9b7e3f9b26db9ae"],["/posts/449089913.html","8540dc605537eb08b033ea13ede118a3"],["/posts/469711973.html","d32d045a458105d0bfcc5fa51d82fb00"],["/posts/482495853.html","02aadb83e1bd2acd3b68e4bcd48bcf03"],["/posts/488247922.html","8b6d701417a0e04249fe36b7fa41e2b7"],["/posts/517302816.html","bc6ee38d60587315de2bb5b62841f493"],["/posts/570165348.html","4803db63bce1052ac56aa9f658f2aa9b"],["/posts/595890772.html","e9b0ba73bac8b27fe24a72706b2229de"],["/posts/67485572.html","d23b62fa4ce96a07737b9c3968ec0c96"],["/posts/694347442.html","8df9987fbd8141729ca1f03d021684cc"],["/posts/707384687.html","2a516ee82d3ea3952f9f53d097088162"],["/posts/71180092.html","da63eec608f3820b42efa6b180a7eae9"],["/posts/716459272.html","d6de0f5f36589e9f8908b7950294c640"],["/posts/765481613.html","ad3c4b4ea6e15539b5dfe1f42ee5087a"],["/posts/778231993.html","85ddf7540b57ce7733054dff9106dead"],["/posts/795397410.html","3d2d8fc8ea291a5076a14a9961fb1536"],["/posts/820223701.html","ed827cb05228b17da1ef7b72bdc79135"],["/posts/830372185.html","c2eac07d4b18fb6445fee79f34bf0573"],["/posts/88294277.html","19588e3e1d47389e525e964a8a1c2ae3"],["/posts/939963535.html","dee8ca31e8279cbd8e3e695dfe8a56f5"],["/posts/983786067.html","2fb99ee326d6e3e92ea4da3f69933318"],["/sw-register.js","11db68a653eb50c55e3fc01f0fa3a686"],["/tags/C/index.html","8f4e9edb5a8793be8e6ef0405cb1c3da"],["/tags/C/page/2/index.html","cd6576f4abecbf473b161f500af65444"],["/tags/C/page/3/index.html","bc2659499c9a83f4d159a138b537a814"],["/tags/C/page/4/index.html","e186166d73f5844347486ddad02b4237"],["/tags/ETL/index.html","55b6588f3f83c0792ec2b67d62f91f99"],["/tags/ElasticSearch/index.html","150e864d31cfd889aa79b58551242fe2"],["/tags/GUI/index.html","56e444b4c3dcea6a1c8f5455e812b609"],["/tags/HBase/index.html","0cb150a30bc256e44fceb7efa88c6008"],["/tags/Hadoop/index.html","cf727150a0fe154b238074a4c102ed85"],["/tags/Hadoop/page/2/index.html","c34c6e22a27b7abb1cad7480bae17502"],["/tags/Java/index.html","d7f973c59f74d6c6fbb485b650932d75"],["/tags/Java/page/2/index.html","28166fe402cfb5ff3e6f548f8e4bc9de"],["/tags/Java后端/index.html","32ae401f112506fcf36cf11bbaf296c2"],["/tags/Java后端/page/2/index.html","d3b4b959c3e94b657c649491a996cb8e"],["/tags/Kettle/index.html","d5918817d9a343b27aa150269ebb2153"],["/tags/Kibana/index.html","2dc514c6712da0dac044414563a5a3ba"],["/tags/Linux/index.html","883bef6be1853f01001e73dbbf15a882"],["/tags/Linux/page/2/index.html","84e02191e1c083449226be4475a7f8cb"],["/tags/Linux/page/3/index.html","0b50790883d82b078ff6cb7714820846"],["/tags/Mac/index.html","c83989567b13852ef30cae33e486270e"],["/tags/Mac/page/2/index.html","c9da206780c1da86bad4362345cb768c"],["/tags/Maven/index.html","b5d17e91b8f55319dae04d37233e9fc2"],["/tags/MySQL/index.html","4b2b0d2a7e73344d47d44b3faa0d1a18"],["/tags/Python/index.html","0a705c4dd80ec75453dadae84fa425ed"],["/tags/Redis/index.html","ceef45f9bfcf171aba71ec18b762a2f0"],["/tags/R语言/index.html","ebb09b7d076fbbcc12cd9f6d7e170d73"],["/tags/Spark/index.html","ccbae9283505bb878d7d523b205b0444"],["/tags/Ubuntu/index.html","8026ff4e8cdc706c0a83f6735b42a3ee"],["/tags/Vue/index.html","c9d91cd411b70ea98ebfe87ccf97462f"],["/tags/Windows/index.html","671ed72175478332b2dbd7fb23376740"],["/tags/ZooKeeper/index.html","4baa142d215d5fab9b78730872d8607e"],["/tags/bfs/index.html","6e4373b4cfc884fc3fc0c9a76ebdec68"],["/tags/dfs/index.html","afd1a67aac2dbf61c09ebc3f5d2f61a7"],["/tags/folium/index.html","9dbe088bcf37cfd204b3654a8a877d3b"],["/tags/git/index.html","ad45de05215facdca3555aee46d92cda"],["/tags/iPad找电子书/index.html","ddbdf698c65df3c5db893c6fb97be10d"],["/tags/index.html","0c5dadee7183e8e0c58d45d3cbb93b05"],["/tags/latex/index.html","5c3ecda3c961b530473237d16d9b48c3"],["/tags/中间件/index.html","695953ccb666df4b2ea5c88e635284b5"],["/tags/二分查找/index.html","e0cf8f1c34940e8b4661122f71c73824"],["/tags/优化类/index.html","5609490d4becc627afebe215f994a0e0"],["/tags/前端/index.html","460989628be2e6c9943fb535c459e574"],["/tags/前缀和与差分/index.html","96165fb7a2f4b5f96bf9e2d5db3a07b2"],["/tags/动态规划/index.html","66cd73ec98427808818b5d190d001439"],["/tags/动态规划/page/2/index.html","932a93c761a53a17736433688aa02e87"],["/tags/博客搭建/index.html","c8fc87335b82939ab73106f83214385f"],["/tags/图论/index.html","eccdf908630534e469c3e0904ce604be"],["/tags/图论/page/2/index.html","dcdf06413e5e0e47800b6cce9b23538c"],["/tags/大数据/index.html","750050caf54a993ba685b0f3517c7f26"],["/tags/大数据/page/2/index.html","b30f3fca35a83ffcf6138dece34b739e"],["/tags/宽度优先搜索算法/index.html","09135d14ee116026af5d0890ce011f7c"],["/tags/排序/index.html","7c646022d10ee95ac29378d5cb67f4fd"],["/tags/操作系统/index.html","05a129fb039357d084d5cc3e0e0fdda5"],["/tags/数学建模/index.html","b5fd97575472f99b20d73efc658a5daa"],["/tags/数据库/index.html","3c30fed16d2d98ad73ec4fd67f660ff3"],["/tags/数据结构和算法/index.html","2baab2f9cae554039721280b059220ad"],["/tags/数据结构和算法/page/2/index.html","a4790fa0e76afd7d934752f2edf6aa53"],["/tags/数据结构和算法/page/3/index.html","af4c0ba58614404eb5761b3e993eb45d"],["/tags/数据结构和算法/page/4/index.html","5d98098f9cf3d4ec05fa196f06aa2f65"],["/tags/数据结构和算法/page/5/index.html","134237c9ccdf6457a2919f6ad07d91cc"],["/tags/数组和字符串/index.html","ad4f7dc8ea2571aa28aea5c10a118b5f"],["/tags/数论/index.html","e50794de97545c82d57cb9b62ee0ad44"],["/tags/枚举类/index.html","b9ac2629010beba7f557de262cdd752a"],["/tags/栈和队列/index.html","d31d49bad3dfa53895eb99fff56aac58"],["/tags/树论/index.html","9e8711d513d1a316fe068b2227b7c961"],["/tags/测试/index.html","ff543ea1afff0faf698649cac80c22a2"],["/tags/深度优先搜索算法/index.html","105659df9c2523b6f8a32534311ed1a9"],["/tags/环境/index.html","21e0046f096f2b0357d475f7ec6f6c1d"],["/tags/环境变量/index.html","c68d7842754422be7550d7f1d1609c0a"],["/tags/绘图/index.html","8b874cc73c9a83c20644da881e510280"],["/tags/编程工具/index.html","526757933664b4c8176cc6ee517aaa7e"],["/tags/编程环境/index.html","200a542657413b69d85bae8a7c168595"],["/tags/网络编程/index.html","52170bd84832a32bf8a622fe08067d81"],["/tags/英语语法/index.html","721f2e42f4ea6a965890afdb53f79774"],["/tags/计算机操作系统/index.html","00f54c34c322e583374a4a761c808e03"],["/tags/论文/index.html","832c53987f7faeb01d645d3415b85f24"],["/tags/资源下载/index.html","8403ad80397c5ea6687da134a1cc352d"],["/tags/链表/index.html","92242cc390fe6ae6a7a7585bb74dd22e"],["/tags/集合/index.html","cff0b2eb68808ad811d4f497dde784f2"],["/tags/集群/index.html","f0f49aaab17f5e387c8816150bd27744"]];
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
