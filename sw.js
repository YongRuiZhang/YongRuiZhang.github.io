/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2925537f7be451f06067db64a463cab1"],["/about/index.html","931dfccaea981f435593fe9093f64465"],["/archives/2023/01/index.html","857bda4842f2b88d4ed6bc39a17b9acf"],["/archives/2023/02/index.html","c5188058aaa8c60192b877569776f0a8"],["/archives/2023/02/page/2/index.html","92d0b4612054fd4ff0249ad005d4465b"],["/archives/2023/02/page/3/index.html","e1403c1b6dabdf47a220c887305b89d4"],["/archives/2023/03/index.html","ff28371cd630ffc819a357287d9f62b2"],["/archives/2023/05/index.html","4319ae68a6671d1a61d76c6ce454e71a"],["/archives/2023/06/index.html","babe8fa81043982fb577b8ada69fbfd1"],["/archives/2023/09/index.html","36381cc5348aec5e8aa5e22d22adac6b"],["/archives/2023/11/index.html","e6aa3da0bc3565bf73de8b35a46b34d5"],["/archives/2023/12/index.html","9748c2407fd515df2fb38d34659c03f6"],["/archives/2023/index.html","0b985eb1b15bd9be7dcadfb70200e454"],["/archives/2023/page/2/index.html","c69f76b709aedc5bb044dbbc4106bc79"],["/archives/2023/page/3/index.html","1fda6eb0f0c9aedae4208255ca1caaf6"],["/archives/2023/page/4/index.html","76106e74828017c2ebad398c2155dc69"],["/archives/2023/page/5/index.html","8ff0c009c29030d8bd33e837155d5af4"],["/archives/2024/02/index.html","c4315633ab879e95d7ba869f7e888452"],["/archives/2024/index.html","e5b7e46e918bf8b89c78e5e73c1be240"],["/archives/index.html","b36c42ff6e9c32fa21fb5fdd645b7f2f"],["/archives/page/2/index.html","f0ff5c2af36f77e17bd89f3a144791fb"],["/archives/page/3/index.html","748a545314b453b8ab2996ec9efaa076"],["/archives/page/4/index.html","a1eec1267538309549e112d93dba7cbd"],["/archives/page/5/index.html","df02162981f17572411afa3189a715d4"],["/baidu_verify_codeva-qQP2iZOMLX.html","dbf9d13c69ec7bbe80a3b893eb2356cf"],["/categories/Java/index.html","833c48f8bdd97d1ffe20a84f08a38ebd"],["/categories/Java/后端/index.html","fa6d49e1e52223b2e3262b47282a075a"],["/categories/Java/基础/index.html","2ac239a76ca33fd683c3145d5f00c8f7"],["/categories/Java/基础/集合/index.html","84b1005e43db058c5ad5f203a9d64e95"],["/categories/Python/index.html","7c24e232a916e6c457637f9c15bfe4b5"],["/categories/Python/编程环境/index.html","e6fd1ccfe8355f155d8b2e352ad23f68"],["/categories/R语言/index.html","70c1e5b2924af0c5fb74cbd3f2c42b85"],["/categories/R语言/编程环境/index.html","41514b31b2619ed850435d2ae9f47fe2"],["/categories/iPad/index.html","7dffb812390542e36fd4dc06024e0306"],["/categories/index.html","30b145fb837a9e72ad81163ed6bb8f27"],["/categories/中间件/index.html","0c7cce0dd7ae107b4e119bd6963b115a"],["/categories/前端/Vue/index.html","36ed5c4e9d65389b4dd328c8d496ffff"],["/categories/前端/index.html","2e59d0e7397a9c09225ca0fc3d9428af"],["/categories/大数据开发/ElasticSearch/index.html","64b3f0856b3e43e8e3266d4ef8612709"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3283b854405182edd8740ff71ea148ec"],["/categories/大数据开发/HBase/index.html","10e8b92a3bbdac4f967655b3b4a2b5bc"],["/categories/大数据开发/HBase/学习笔记/index.html","6d20a9578478fea81a339ca0b9a1a021"],["/categories/大数据开发/HBase/环境搭建/index.html","688ad547fbe3d14fd24e45273e0f668f"],["/categories/大数据开发/Hadoop/index.html","9f9233d59de40c6fcadbefc3c53c3017"],["/categories/大数据开发/Hadoop/技术/index.html","617a50895031673559fd5033b2d3e7c8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","32e3c9e7997dfab760b11f12ada6ccd9"],["/categories/大数据开发/Redis/index.html","172bbefdceaa90a639fdc00e0bf20214"],["/categories/大数据开发/Redis/技术/index.html","af0f03af6d723507ae29abf3b05d4f57"],["/categories/大数据开发/Redis/环境搭建/index.html","1d752aeb8bd52164a25d8f3f9f06b2ce"],["/categories/大数据开发/Spark/index.html","7aab80f2312eb1a7a18054ab2e0a5019"],["/categories/大数据开发/Spark/环境搭建/index.html","52ad0a1d8ec995db4aa1b1156e033f38"],["/categories/大数据开发/Zookeeper/index.html","01f9945144b74af167f23cd54005c05d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","7979da99c1dbb72b2161aa808d6f7280"],["/categories/大数据开发/index.html","14a54c4f3fe66ef58f834ad9416203c3"],["/categories/学校课程/index.html","860a735143ee01a7115542ed00f9d774"],["/categories/学校课程/计算机操作系统/index.html","3da11861b2b2f993e3de3ef128a9b736"],["/categories/操作系统/Linux/index.html","a0faacacc7e72b8229a7b8064eeb9726"],["/categories/操作系统/Mac/index.html","f406727e289b635632313d2345b0c306"],["/categories/操作系统/Windows/index.html","82ab9ce44a173a2cc2fdb2d8210ba333"],["/categories/操作系统/index.html","de8f5a628a1d43b7c70e6434a6d76c55"],["/categories/数学建模/index.html","3468681abecbc1c13d5597ab9723266a"],["/categories/数学建模/latex/index.html","dde7e15316dda6230134920ed62c32f8"],["/categories/数学建模/优化类/index.html","77419134f92dd6ac3988990375a987c0"],["/categories/数学建模/优化类/现代优化算法/index.html","f2527c5e47d39c52e6b7128be1df049a"],["/categories/数学建模/优化类/规划类/index.html","6be8157bd0a9db4daaa1c651f9419aed"],["/categories/数学建模/绘图/index.html","15823e605c7bcd30b08463f591f8f561"],["/categories/数据库/MySQL/index.html","e71c27f6225441d5f1d00b2331af8af4"],["/categories/数据库/index.html","1a116c6a30c9a059e4be50cde33285bc"],["/categories/数据结构和算法/index.html","d3b86cf376fbc96ae09170575d3f8005"],["/categories/数据结构和算法/page/2/index.html","5e22cb3df24189ac44f3c325b2f84509"],["/categories/数据结构和算法/基本原理/bfs/index.html","6c6f4ac648d9f81435682925fea8bced"],["/categories/数据结构和算法/基本原理/dfs/index.html","f956735e33c9fb03d587f23d466f2f26"],["/categories/数据结构和算法/基本原理/index.html","90683a0143549ccc7a8a9fddda3de6f2"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","210b1a6ff3916294916f1cb595d720df"],["/categories/数据结构和算法/基本原理/动态规划/index.html","597fbd72f8a90de61061c540a47c73e4"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","f93b4dcdfb889124e298912ba4837a06"],["/categories/数据结构和算法/基本原理/图论/index.html","669ad9672f7919bbbb90f9e925ddbce2"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","107f4d0b9ecee6cf050dc2a93a62504c"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","f029b857281f70ed9fb506c31ebdfd52"],["/categories/数据结构和算法/基本原理/字符串/index.html","5668c91c0191c529b080c66689683d20"],["/categories/数据结构和算法/基本原理/排序/index.html","388e88d0cd4f4798d887ff71c26108c6"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","87fa413e77b7759ce16911768b01dfe7"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","7d7c68bdb041a7525843e333fce05cf1"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","b3ff3b2f43c3fef0e86de33a56426192"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","172f9a1cd3009309afbf4bcc7157083d"],["/categories/数据结构和算法/基本原理/链表/index.html","1d31a6e8e44604777080114a7ca3d994"],["/categories/数据结构和算法/算法题/index.html","23f48c7649c5d68c518ccf53699251fd"],["/categories/数据结构和算法/算法题/二分查找/index.html","864eebb2dce4eb5dc4956fc48c28ebcf"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","599d9ce639ba0bb5b72538513ed986ee"],["/categories/数据结构和算法/算法题/动态规划/index.html","23c67b04797d5c8bd8c6560e30971b37"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","4808cdfd129ccf9027e005d59100947f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","502cd22cfcde380213a4c132e49c3937"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5e272515ce37e32a7cd8bfd5bcea601c"],["/categories/数据结构和算法/算法题/图论/index.html","d8fe646a4c124197e58fee9755f532e1"],["/categories/数据结构和算法/算法题/图论/树论/index.html","28ffc4bd10467d9699423ff1ebe70f29"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","75e4a08a41cb0b428a4d87fbb008493d"],["/categories/数据结构和算法/算法题/数论/index.html","684d63a130da156a444688751d193ca6"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5f667573c789caede538d8ca8c227a0d"],["/categories/杂七杂八/index.html","4600d4a9d990fc207e901e836d6e9457"],["/categories/杂七杂八/博客搭建/index.html","56eaaa8968de2666f596105df117297d"],["/categories/编程工具下载/index.html","e4256820bb0f40a8ecc468660cf57e5f"],["/categories/编程环境/index.html","35031853dc532277a2e695cf488d60a6"],["/categories/编程环境/大数据/index.html","ef2a516132851be3e7f07adf249d42d7"],["/categories/英语学习/index.html","43d1d489e5f84b2da8d23323df4710ee"],["/categories/英语学习/英语语法/index.html","2ec8fe6591c50e928eb33be0670c0c37"],["/comments/index.html","0ff40e4f863bd7c72a70c2f0f72f2f28"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","701c8bb25c3c5bd6f42c4bc67966916a"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9c7b683e0e33b7e1799d895eba52513d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","124390c366b6dcaf6a31b001c3ba56fe"],["/movies/index.html","7854c0d1bcd9eace91c5f22371fdcbb4"],["/music/index.html","cd2cca70e2b074674ec6e2e7abbd9230"],["/page/2/index.html","cb378854ecb997eaf70adb2eccbb27a5"],["/page/3/index.html","052225e54dd8571adc559981917c710f"],["/page/4/index.html","b1c280e3d7cfd692d3d9d6aa89a29faf"],["/page/5/index.html","db644bac05ec25eb106ac0eb3809576a"],["/page/6/index.html","88469477c597428f598f5870d70a98f9"],["/page/7/index.html","8ff1522af54a6f03a420d8addae50d8a"],["/posts/1021360842.html","898166621b0150c5f84af119ee93701c"],["/posts/1120620192.html","7bf606fe055ae770508a9e5b1413f790"],["/posts/1137707673.html","a5149345ee7c409975fdc9c6a0017dcd"],["/posts/1141628095.html","0879e32907639d56e4ff9d0e7650ab6f"],["/posts/1168613674.html","ab4a57a4e6399663fbd2b6a9a745a1fd"],["/posts/1219920510.html","c99a98cd763b12d99e018f5b19562865"],["/posts/1222166338.html","00edbb5cd91116309b676d54c8709c32"],["/posts/1259097482.html","d9e9e6fbd2298d6b80ff56d968557496"],["/posts/1271036369.html","3b401e81b9efda2833d19f9154c803f5"],["/posts/1312847445.html","c2253542f6cc2ede41f33db1dac2f66f"],["/posts/135355774.html","70e5ea0690322d2f5ea48c5a5cd178c9"],["/posts/1375344716.html","ff7d1557baac1f9053951b3a325cfcdd"],["/posts/1388991698.html","782f5ee25965b54256c459c34d45914e"],["/posts/1410315814.html","650d94ddbc53be0d99a392a0989b4a04"],["/posts/1452790229.html","8b5a8ad586ddafe5c7e1a158e796b572"],["/posts/1470079884.html","ef3ef497fd2bbeeb0f515eed71c5f5dc"],["/posts/1470079885.html","5cace85e8c098a295aec9a2a83678c09"],["/posts/1470079886.html","c302c8ef192f9cf8d64c1a7e82e82cb4"],["/posts/1470079887.html","df54679d2f54d35adcde9bb62830a583"],["/posts/1498536549.html","70f77cee804deb81afa4831121db91a1"],["/posts/1539568593.html","a643c2f3fd6a93228b401b3597f0eca9"],["/posts/1547067935.html","fc9150650f15aa22bc8fe5c43f64a4b5"],["/posts/1557866301.html","ded6f71c9ddeb4e097cfccc4b08e3705"],["/posts/1571776361.html","3931a7155064f0a9f27828600a61af2a"],["/posts/1605124548.html","4aa26d0add5e28dfaa1a3e37ce8438ab"],["/posts/1633036852.html","8bc7024819aac2f638297ee0608a406a"],["/posts/1667740714.html","4eec944915ba6c48bd5529277435f7fe"],["/posts/1674202625.html","4ffd57061a018a5720b1c787331e1a20"],["/posts/1765123828.html","bb0db37d337043a9738e066fee2c0805"],["/posts/1767336200.html","ba70ef60c51aa78a07253abbaa4df6ab"],["/posts/1776114197.html","2869be502bc2ae4748d5c2506c05ba02"],["/posts/1817748743.html","b73ff9dc49608e46a179b7058587763d"],["/posts/1925125395.html","4d994e96b0471284e187f2a9e1f80b16"],["/posts/1966191251.html","80c563ad4f5007b7b6084ffd88985e3b"],["/posts/1987617322.html","6e787a1bdabd2629c70575314d7177eb"],["/posts/1999788039.html","52d19bc5b494aa80120a9493f9d0560f"],["/posts/2007534187.html","5978ce80c3b756855e10f75b166ff986"],["/posts/2075104059.html","6d394b91f4002de565c00e1f64f31bec"],["/posts/2087796737.html","650b6a8568f605de25a0047253bd5d21"],["/posts/2106547339.html","23eb50d1b074e5ba800d3e239a236a90"],["/posts/2207806286.html","45fda3d2e76f1c9d968133d4a72aa6d5"],["/posts/2225903441.html","f4a73fe588f9f977a265d2169175bd45"],["/posts/2265610284.html","4099c4a4da601d97e617cc5d4517d74c"],["/posts/2281352001.html","1ea9bd95324546abde342aba6bad1a4a"],["/posts/2364755265.html","4f96fd7a2c20411993029302c7762826"],["/posts/2414116852.html","36e8a3866c3fc05b75f1bd3232331d13"],["/posts/2421785022.html","69fa446448458f1d134b1baeedfe2b7b"],["/posts/2482902029.html","99948d10502b230cb25835c145cd7a61"],["/posts/2495386210.html","f09fe80ef516abcadd91522ef7df1e4e"],["/posts/2516528882.html","f163e29a69dfba9861396127ad56a615"],["/posts/2522177458.html","6b2acbe83f61b6246598f75aca47d0d4"],["/posts/2526659543.html","03e5a9fd806c234869c559d059830dca"],["/posts/2529807823.html","c66bf081d1dce60310f4b6e5214f0624"],["/posts/2592249117.html","d7276d2d50a62c9418b942e202511ec5"],["/posts/2596601004.html","939890133eb5e21aa7f40f1fe7ac29cb"],["/posts/2697614349.html","df8cdea1db3df8866a0c3bf21cdc8118"],["/posts/2742438348.html","63ab0d18089cec74a11a50df92a1d53c"],["/posts/2768249503.html","c08af20cbd84953a6119a6dfbbfeab88"],["/posts/2864584994.html","cac0c5f0ddae5146f8cd3397530b91f2"],["/posts/2888309600.html","17d291ed46c98aca83d28e0d90d2a12d"],["/posts/2891591958.html","60539f28f56413c1297b5c7495e658d7"],["/posts/2909934084.html","5945f7376dd57bcb4fe3f5755e1c848b"],["/posts/2920256992.html","2294c6187aa56ac0c585e3c2b03048f4"],["/posts/2959474469.html","bcbbab659800bd8428693b381dcdb172"],["/posts/3005926051.html","ce408cccaf500a73f0ee4f2690629cb2"],["/posts/309775400.html","18de6bc1549157d0230c1354a2d17020"],["/posts/3156194925.html","0df9fb9b8b9fd69a44dab4efefb25ca9"],["/posts/3169224211.html","4606760841cf90055e4d04a663f1a915"],["/posts/3183912587.html","8e974f46f66569e413b523873d387243"],["/posts/3213899550.html","a15c95748961cef0e9459cf5f7419922"],["/posts/3259212833.html","8e93acb0292d70a5ad6976c3ebc21108"],["/posts/3265658309.html","beb5070c0c45f658682a48ca0f01ce7a"],["/posts/3266130344.html","fe2536f6e987eaddf5e229e6ea34ca92"],["/posts/3292663995.html","10606abb2980262c70f4bc2eb9f735ac"],["/posts/3297135020.html","8018d0d624071916af9b4f48c9580af7"],["/posts/3306641566.html","181534659f5a6b5176f52eddfedd278f"],["/posts/3312011324.html","329fa3cb5c4e5c0073a142d5c534c146"],["/posts/336911618.html","c279e4c9e8152a64f65ca44dc980daf5"],["/posts/3402121571.html","e61cfc0060377c8c91d4121a54c233b8"],["/posts/3405577485.html","c3e1a1d57b8c443d198d18f37d652335"],["/posts/3498516849.html","e3c56c0a12c9679d904de78a5c129f98"],["/posts/350679531.html","206c9d5639dd5ecfcc245d2f7fc84f40"],["/posts/3513711414.html","08b78cf5605f213ea43fe37bd50afaf8"],["/posts/3523095624.html","502f6408c9f6832405bf18e484f30154"],["/posts/3546711884.html","74a2d260e2da4b5450bf1929bca6085f"],["/posts/362397694.html","51a5259a6c521a0c51c4b30d51e785a9"],["/posts/3731385230.html","f2aa653a3735f2c0d7afa4433cb280aa"],["/posts/3772089482.html","4cf1de3edabfc06b5267c1995e41404b"],["/posts/386609427.html","6f3e87e9886dc103f06437404d4d4b6e"],["/posts/4044235327.html","8e577008ccc5fcf6cb8584822f18f012"],["/posts/4098221856.html","1481b465cd75451bfc2d587f9befa5c5"],["/posts/4115971639.html","750f8da23141a07e573d0fe16ea507bd"],["/posts/4130790367.html","794e5a5edf858c2e3aa76820cfc31936"],["/posts/4131986683.html","5e4aa4844441b7723e94524ee03aea84"],["/posts/4177218757.html","972b5bfc5af45328dcb10573c6a483d4"],["/posts/4192183953.html","2a6d4086c93a4ad60d1660000a269e6b"],["/posts/4223662913.html","fe640035df0638c8b9c66d60d2682273"],["/posts/4261103898.html","12ffeb45acb94138ec1805f416daea2c"],["/posts/4286605504.html","9cfae4c7a7d1bc46ac6716e5600a3ec0"],["/posts/449089913.html","5fdfbabdd65f7e07a8cc600aad38685f"],["/posts/469711973.html","43fa1c06e615dad6506d36688a7c7a13"],["/posts/482495853.html","d66b25b47b00c9b391287ab058d7c9ff"],["/posts/488247922.html","0de049ea4335eaf3f14d3bb5d0643ddb"],["/posts/517302816.html","0f47d3f8be4ba8eab74b668b7bd7896e"],["/posts/570165348.html","b464398bdcb5d47e3540467a410561e5"],["/posts/595890772.html","9b69b0081baca1680d2a9aa19ed110c6"],["/posts/67485572.html","fca611e902aaf41bdfae3ba7fa1d90e6"],["/posts/694347442.html","77df9c38a6eaea7e144a5503798819a5"],["/posts/707384687.html","4d711dfd5198c3e9491a695ded39a4c3"],["/posts/71180092.html","c19bccf28906220ad5a009f5811f0d14"],["/posts/716459272.html","ed2f285114bffd23d96d1364c77983cc"],["/posts/765481613.html","b9e4c082399aa9e3ab4727b71df0af89"],["/posts/778231993.html","261942ba8d50d9f4e363a3a7bc16cb5c"],["/posts/795397410.html","9114475f2c2612551a6b127bfe54fe5c"],["/posts/820223701.html","40771c749007916483459ca3ad7a2bc2"],["/posts/830372185.html","09741fe3a791bf78e160fa9bba06ac12"],["/posts/88294277.html","401878c557ee93ac82db7843ab1759be"],["/posts/939963535.html","a26964e4b2ed0388c502b12793c1ddb7"],["/posts/983786067.html","440b8a86c67e71a385d4eaeceb056c7f"],["/sw-register.js","56116aed0f6405863fe396c818d54c05"],["/tags/C/index.html","a2f22be76c9cbbafa62ada7d3b71d2fc"],["/tags/C/page/2/index.html","5eaef4fb8ac2d76caf82dab39d3b44d6"],["/tags/C/page/3/index.html","4d10a1d8cf1b5434a5908d9e2bbd3184"],["/tags/C/page/4/index.html","12b70fde327ba587c350ae9c164c94aa"],["/tags/ETL/index.html","aacab0240295c70a57e5d7149ccdb604"],["/tags/ElasticSearch/index.html","107a1eaee2e5c1c74269cfd11e48fe69"],["/tags/GUI/index.html","33ac1c0c7ac20943906ccf9ad8c2ec34"],["/tags/HBase/index.html","9fa1509fdb0f00c84e7a6ac3173a042a"],["/tags/Hadoop/index.html","55da295918a4e6264a4e2b502280a7ec"],["/tags/Hadoop/page/2/index.html","c63207b524f3950abbca59bf268ee46e"],["/tags/Java/index.html","c631f2d0d73ba751636c1ff05e594595"],["/tags/Java/page/2/index.html","82b84ce0bbf563ba016d2e3d503f32a6"],["/tags/Java后端/index.html","ebcc1b0d680521af20f8ec0cc363572c"],["/tags/Java后端/page/2/index.html","11124ef33b51fb540d943b844aed353d"],["/tags/Kettle/index.html","884c1e77d811011b916f699e959d5b64"],["/tags/Kibana/index.html","371ea99f1b98d9f5246183fafcc152e1"],["/tags/Linux/index.html","c9f635356c9730eca734b4457557f14e"],["/tags/Linux/page/2/index.html","d2785d7cf39b105de8ff483f41bf8f4f"],["/tags/Linux/page/3/index.html","84a738148553e8596afceef2b3b4c2fe"],["/tags/Mac/index.html","afd585222243c19c9569be4e820fe934"],["/tags/Mac/page/2/index.html","ddbe044777a552b88dd6d8f4850dd607"],["/tags/Maven/index.html","3731c873c3c06e3f710acc45d2a94e86"],["/tags/MySQL/index.html","e55ded5dd21ffa7f80029133e1b35c76"],["/tags/Python/index.html","ee9c66ac9625feb82657f8790eb4d93e"],["/tags/Redis/index.html","2a9b1234536b56e4ecbb6b7b5a4fbe75"],["/tags/R语言/index.html","b6bb297428fc2e481aa4ed907e96eb81"],["/tags/Spark/index.html","99de062068651f8b5e0b4e6624ed44d6"],["/tags/Ubuntu/index.html","0f656fa4103d50ac78c2ce5925bf329c"],["/tags/Vue/index.html","45541a8b571f876d985159409512eeb4"],["/tags/Windows/index.html","a604a6f2fc5ffecae638aa2c68b41481"],["/tags/ZooKeeper/index.html","ca658a1340f7b737adb569bac6a911ac"],["/tags/bfs/index.html","1db6de8c50f013e2ad1224ec215d3a78"],["/tags/dfs/index.html","5d79622a97e7a0cc0d03a05023d46e16"],["/tags/folium/index.html","4530dacaf4101e074fab0a347ee2ac9f"],["/tags/git/index.html","b9147862df21427c2925643a751cced5"],["/tags/iPad找电子书/index.html","61b166c727782a06606bb05b4509a7ef"],["/tags/index.html","e588306357aa68044ae169b92dfae357"],["/tags/latex/index.html","17eea0dbb81fe821b4c2639543354e37"],["/tags/中间件/index.html","97a159f2242696d8ba6959bc1db2e3d1"],["/tags/二分查找/index.html","feeffb7ef323d905d5744575f19dd9fe"],["/tags/优化类/index.html","e8e3284e62b74becb92929af04293346"],["/tags/前端/index.html","66fd82502a6c8c1451ebd4bbe8c296c7"],["/tags/前缀和与差分/index.html","6fa34087ddc11770d6ade781394d2fd0"],["/tags/动态规划/index.html","64e314a570095d160a1f6bc208a36fa8"],["/tags/动态规划/page/2/index.html","f18fc9f392c5a93fecbd4694c2a475e6"],["/tags/博客搭建/index.html","b7beef8ee6bdbe84916d2fea42294b2a"],["/tags/图论/index.html","5b8bf755b09ae15c7f1cf8921c0bd2e3"],["/tags/图论/page/2/index.html","b95ad133c2b701735887f267492e5883"],["/tags/大数据/index.html","5c01064f5d66fbc782fe4a7dca762862"],["/tags/大数据/page/2/index.html","79469a1e5ef9f0af2631ae9959be0376"],["/tags/宽度优先搜索算法/index.html","4dbcbf5cad854407a6a697b0c9807242"],["/tags/排序/index.html","7a01afd91ccf650dd3e4c5fff8b418f5"],["/tags/操作系统/index.html","1f48e2fd1198bd5679bdef76a3b36b2e"],["/tags/数学建模/index.html","dcd26935ecdcae2070b18a7cd6a32b7a"],["/tags/数据库/index.html","d5a12e7a8f9d688de911627685aec5d5"],["/tags/数据结构和算法/index.html","8201f4d1691d10dc15314dcf4f2ce258"],["/tags/数据结构和算法/page/2/index.html","6457fd4ae9d7139f3b1ce9d3451dd4e4"],["/tags/数据结构和算法/page/3/index.html","181afe87214e51d7b7e3ff0deab2e83b"],["/tags/数据结构和算法/page/4/index.html","8e4751986b461ff86af0575ae3917c5d"],["/tags/数据结构和算法/page/5/index.html","6f8b12fa0f86dd2cb9bc119093259d9d"],["/tags/数组和字符串/index.html","42231ea97a1134ad11de9d826ff58053"],["/tags/数论/index.html","5d0c9622efaad09cec44437553d5ee8f"],["/tags/枚举类/index.html","a9c5ab5e39cde4d96b0d476f013ce758"],["/tags/栈和队列/index.html","8765085746b0afe9d0a2af2d23e1b69b"],["/tags/树论/index.html","6a8092c15b4fd2aa6bf74a77b5c45000"],["/tags/测试/index.html","2e6600309c9e731d69a274d0a0a8f5cb"],["/tags/深度优先搜索算法/index.html","bc7463d073cb4a393d6c23c638beec34"],["/tags/环境/index.html","7b21dfa51456f73eca5a1e1e5f7e3edb"],["/tags/环境变量/index.html","cde02e6499442db336b70193dbe9ff52"],["/tags/绘图/index.html","e16ec1a6a10c358fa21429455839a54a"],["/tags/编程工具/index.html","0e2ed933f7beecf80317862343a57396"],["/tags/编程环境/index.html","06b9cc0153e93d6cd670525812ac6eb2"],["/tags/网络编程/index.html","b6a6fdbcf0e7a906ec058d61faafd1dc"],["/tags/英语语法/index.html","c37250d63a949e28ca5e4b5107e635d8"],["/tags/计算机操作系统/index.html","851fffdcc923fbf567e4812ba767f617"],["/tags/论文/index.html","0851808dc3cb32bc2e9b7db63c86104b"],["/tags/资源下载/index.html","a0e8161813de0768b26c5ceec8a3f555"],["/tags/链表/index.html","0c6f1a085deb534d95acc3c76edf68cc"],["/tags/集合/index.html","c08076507c69a2828b530f51d96f62e9"],["/tags/集群/index.html","e0a2f3a2eb6c7a0a574be64ff87ba7ae"]];
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
