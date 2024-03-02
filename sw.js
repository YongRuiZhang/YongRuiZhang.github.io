/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","605853f29010676f5a67f9d856e643a9"],["/about/index.html","931dfccaea981f435593fe9093f64465"],["/archives/2023/01/index.html","5e9527257dfc2dfc98da1cad828f52c9"],["/archives/2023/02/index.html","13353972f5f6dacb347c4beb1fe4dea2"],["/archives/2023/02/page/2/index.html","6beb29cd50b20cc491a1779e4bdd6b71"],["/archives/2023/02/page/3/index.html","031ed911aea59f94f1f1c1b337dd09e5"],["/archives/2023/03/index.html","c67d282de6499f04dcc052530a2c31d5"],["/archives/2023/05/index.html","bed71db7fd06de571c31607566eec2f7"],["/archives/2023/06/index.html","4dcf007eb3f2b78debaa2552714c06b2"],["/archives/2023/09/index.html","9d9883b85f87a6fd2d45cd9e8bb010c5"],["/archives/2023/11/index.html","1db6379b2ad4510df685bdf09441d869"],["/archives/2023/12/index.html","322d29883fdb95006918bef3d10ec691"],["/archives/2023/index.html","3293e8974a00fe688f6b06bdb75f76ef"],["/archives/2023/page/2/index.html","25d8bc8be8f2efb5c44f86a3724d2232"],["/archives/2023/page/3/index.html","ce3cc0a1bd56f7378c5514d093ebcd82"],["/archives/2023/page/4/index.html","edc9c653758bfcd6ca46d5dad7274f55"],["/archives/2023/page/5/index.html","2651ada3c7f5825ae5ead4f04f21be0a"],["/archives/2024/02/index.html","d0f02f43a3e560254a3eebc9cc8b717d"],["/archives/2024/index.html","c64f23d365385657b4f783c1b9af84d2"],["/archives/index.html","d538dd550ae2c2509b7a97e85fa7e5f1"],["/archives/page/2/index.html","5169e8bf87ca4fe00cebd00ffbb8ab23"],["/archives/page/3/index.html","ad2d67198252e41585bdd4e19d091f67"],["/archives/page/4/index.html","406c5c7aa48dfdb5a73c607e3a7b5f49"],["/archives/page/5/index.html","4fe41d13cf1513a9e5fdf76df6fde83f"],["/baidu_verify_codeva-qQP2iZOMLX.html","e6fd12fa9a833005784b65efd01da079"],["/categories/Java/index.html","f5bdb2a47f71d385766fef001ebaf366"],["/categories/Java/后端/index.html","d0e134a09e1a8272f48f96d24f8b9fda"],["/categories/Java/基础/index.html","7b7c0234489c15baca164b62af0e58bb"],["/categories/Java/基础/集合/index.html","b78255c5c37f0a1b996c55cfa0a91834"],["/categories/Python/index.html","3a71ecfd0769c33292c9469972f224c3"],["/categories/Python/编程环境/index.html","b5815ce86f6fe2e0f21ceb9aabb17731"],["/categories/R语言/index.html","47ccb67b2fa5adaf73b90af213df900a"],["/categories/R语言/编程环境/index.html","29d2bd66833a154cb34bbf82d832cff1"],["/categories/iPad/index.html","ee72f038f3327e2460488de79d94def9"],["/categories/index.html","30b145fb837a9e72ad81163ed6bb8f27"],["/categories/中间件/index.html","8a6023ce958ee487e49a128f1c97b265"],["/categories/前端/Vue/index.html","63fbd89e7776a2a92f2226c914810b6a"],["/categories/前端/index.html","6b153d5608ea642ab3621738994c96d0"],["/categories/大数据开发/ElasticSearch/index.html","7c3a9ba82e4d712ee3255a677aeb5089"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","5bd4c6723476fd7b9c08d15edc4cd686"],["/categories/大数据开发/HBase/index.html","27fda4efc73bd664c8ca04ffc3d039e6"],["/categories/大数据开发/HBase/学习笔记/index.html","5740d897b65996a5bbc83eda512b3705"],["/categories/大数据开发/HBase/环境搭建/index.html","323df93ac228a4f4b71f51e755e13638"],["/categories/大数据开发/Hadoop/index.html","6ae998c4804bf00870ece234e9218d86"],["/categories/大数据开发/Hadoop/技术/index.html","609350df2fe23f90ddeab4b8e1f6a90f"],["/categories/大数据开发/Hadoop/环境搭建/index.html","0ae93e9ef9ebac0e3dbb38c790d57238"],["/categories/大数据开发/Redis/index.html","245da5220fe0cb52d88595163f2e4f69"],["/categories/大数据开发/Redis/技术/index.html","b7fc9830e308e1d1093f1fad96f36802"],["/categories/大数据开发/Redis/环境搭建/index.html","82d652d584f46db0b7a1b631946ed7f2"],["/categories/大数据开发/Spark/index.html","fd318f340581ef74c950ddfdfdcc5449"],["/categories/大数据开发/Spark/环境搭建/index.html","bbfbbe491d2a4959b253cf281b8689d7"],["/categories/大数据开发/Zookeeper/index.html","695b59fbb9103a01bf9e2d88b2376ba6"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","29be315fbd48cf86b2bbdf31d6a8f9de"],["/categories/大数据开发/index.html","7876975baf8fbbe2510434bc5c18991e"],["/categories/学校课程/index.html","d36221229aeb7dbcc1f850ba40d4cd21"],["/categories/学校课程/计算机操作系统/index.html","f5af4bdd81b5987bad6034bd50c8cfb8"],["/categories/操作系统/Linux/index.html","43853c93cb9186260aefbade6b0b3f5a"],["/categories/操作系统/Mac/index.html","eebe6cf3c1c7d7b5f1ca9b4b8b2e374c"],["/categories/操作系统/Windows/index.html","1de9416ec80dd5f219fe13f277b66a03"],["/categories/操作系统/index.html","a6d0b172102284c5d391bd984263f466"],["/categories/数学建模/index.html","75a0b67acc32b0ef826d0dd62259020e"],["/categories/数学建模/latex/index.html","50c4f84bce98bae55cf34109730366de"],["/categories/数学建模/优化类/index.html","6a7c91c3acc652b522eede2667e2f3bd"],["/categories/数学建模/优化类/现代优化算法/index.html","3a83dc9af30e042b082928d8759ecb2a"],["/categories/数学建模/优化类/规划类/index.html","7b5341aeb6d28ad8ef14b48873b9f70e"],["/categories/数学建模/绘图/index.html","6cdb8006c063518d236edad319b349c5"],["/categories/数据库/MySQL/index.html","f85bd86e7f7164e7854a154625dbfb60"],["/categories/数据库/index.html","b7c6200bfd6d6d1706b7ac1ae8fe602c"],["/categories/数据结构和算法/index.html","e03de07547ffa17f3abe494f20ed7dd4"],["/categories/数据结构和算法/page/2/index.html","3ad7ce5dce5619e1152ba01dda65a028"],["/categories/数据结构和算法/基本原理/bfs/index.html","8c6211d6403f7de313dc08d05112a12d"],["/categories/数据结构和算法/基本原理/dfs/index.html","4dd46d205fa9e87b18e548c68a8a62e9"],["/categories/数据结构和算法/基本原理/index.html","d717ad218734adb1bba5d2b8220b1c98"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","848c4998c7b0162076af290fd037feee"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e1166e3e9108782351965d5da467a5cf"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","fa32e641071ecd84ebda5422c523f437"],["/categories/数据结构和算法/基本原理/图论/index.html","2e1b1e48f5331626373100a9d0461538"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","ab0ed5df67dc7e363e6c9ac9f54833c1"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","4efc4838d1087167efd413479adcb3b5"],["/categories/数据结构和算法/基本原理/字符串/index.html","6ea66757f24b9c59242c57bb1be2594e"],["/categories/数据结构和算法/基本原理/排序/index.html","9722c5b13bb8fb3cba3913d5078a522f"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","cb88df05160c69d61702be147ec9c94d"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","4c77c61b3b4d4bc3f5efaadd925b5102"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","d09f610f987cf968ad9e3d94a8991fce"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ff0a42d13cc46025cc2e1d83f12178be"],["/categories/数据结构和算法/基本原理/链表/index.html","0c4b4e7df0c43bc7a62e846bb82a4455"],["/categories/数据结构和算法/算法题/index.html","a2289baa011bc861874baa1705b20036"],["/categories/数据结构和算法/算法题/二分查找/index.html","7c067049be3abbc37641cceae4589a15"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","b5f2ebac08a29baa7401e33e081e8f29"],["/categories/数据结构和算法/算法题/动态规划/index.html","39531b70a9bf376de50b9df38803afad"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3bffb701fe16e69ba202580913fbb29e"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","ca5f04ba276dd22e6e56131a03936bb4"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","14ac36f77d6f029c3061cec25e9b7c20"],["/categories/数据结构和算法/算法题/图论/index.html","c094c590f900de819705e8639f6adcbe"],["/categories/数据结构和算法/算法题/图论/树论/index.html","579fd671d403bec666277ee762431f5e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","da433ed802dc7bdbec7ab8067b228e37"],["/categories/数据结构和算法/算法题/数论/index.html","465c62edc5f9f2c8e44174e2a631dfa3"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d771b299ce1c7369358070722e2463d5"],["/categories/杂七杂八/index.html","04418923ffefffafbdfa094a1c15c651"],["/categories/杂七杂八/博客搭建/index.html","366ae8aee68df2c08f4e9a7dbca1e56b"],["/categories/编程工具下载/index.html","6a97592e15fd4c838c1f401f0b76dc22"],["/categories/编程环境/index.html","8828889baf291ac0cfff89f0130052ed"],["/categories/编程环境/大数据/index.html","ebad3a42890c85681dcf929f293595cd"],["/categories/英语学习/index.html","dbe7b2cc3e2fa698a5850af79336ddc0"],["/categories/英语学习/英语语法/index.html","8dc4c8fab8d5bf0793c3562854931e86"],["/comments/index.html","7ebd2b4d0d0ff497527ffa4d7ffd4bf9"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","ff127c7d2f64cbc31a1d58eb2be3193b"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","18b711514592d1ee64b9d2deb6b8fb5a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","b2e44db4af2c1d0ca65b26287e9ca4eb"],["/movies/index.html","f4a26a3ded3a5051aa3ca91c4a90a036"],["/music/index.html","759732ab7e6135492c9c51a0673e4ad5"],["/page/2/index.html","bf3c9edad0ba8dda47873a6593cf9e37"],["/page/3/index.html","4f9ed310b872361f4876ac1d61014992"],["/page/4/index.html","cd97dca85f33e993922a669a194b6d07"],["/page/5/index.html","fb94761fafaf70de6f783240a954a931"],["/page/6/index.html","5271fdae220a848cd61615b47943e238"],["/page/7/index.html","9c33acf4b0d528e2ce80953f8a8427d1"],["/posts/1021360842.html","898166621b0150c5f84af119ee93701c"],["/posts/1120620192.html","7bf606fe055ae770508a9e5b1413f790"],["/posts/1137707673.html","a5149345ee7c409975fdc9c6a0017dcd"],["/posts/1141628095.html","0879e32907639d56e4ff9d0e7650ab6f"],["/posts/1168613674.html","ab4a57a4e6399663fbd2b6a9a745a1fd"],["/posts/1219920510.html","c99a98cd763b12d99e018f5b19562865"],["/posts/1222166338.html","00edbb5cd91116309b676d54c8709c32"],["/posts/1259097482.html","d9e9e6fbd2298d6b80ff56d968557496"],["/posts/1271036369.html","3b401e81b9efda2833d19f9154c803f5"],["/posts/1312847445.html","c2253542f6cc2ede41f33db1dac2f66f"],["/posts/135355774.html","70e5ea0690322d2f5ea48c5a5cd178c9"],["/posts/1375344716.html","ff7d1557baac1f9053951b3a325cfcdd"],["/posts/1388991698.html","782f5ee25965b54256c459c34d45914e"],["/posts/1410315814.html","650d94ddbc53be0d99a392a0989b4a04"],["/posts/1452790229.html","8b5a8ad586ddafe5c7e1a158e796b572"],["/posts/1470079884.html","ef3ef497fd2bbeeb0f515eed71c5f5dc"],["/posts/1470079885.html","5cace85e8c098a295aec9a2a83678c09"],["/posts/1470079886.html","c302c8ef192f9cf8d64c1a7e82e82cb4"],["/posts/1470079887.html","df54679d2f54d35adcde9bb62830a583"],["/posts/1498536549.html","70f77cee804deb81afa4831121db91a1"],["/posts/1539568593.html","a643c2f3fd6a93228b401b3597f0eca9"],["/posts/1547067935.html","fc9150650f15aa22bc8fe5c43f64a4b5"],["/posts/1557866301.html","ded6f71c9ddeb4e097cfccc4b08e3705"],["/posts/1571776361.html","3931a7155064f0a9f27828600a61af2a"],["/posts/1605124548.html","4aa26d0add5e28dfaa1a3e37ce8438ab"],["/posts/1633036852.html","8bc7024819aac2f638297ee0608a406a"],["/posts/1667740714.html","4eec944915ba6c48bd5529277435f7fe"],["/posts/1674202625.html","4ffd57061a018a5720b1c787331e1a20"],["/posts/1765123828.html","bb0db37d337043a9738e066fee2c0805"],["/posts/1767336200.html","ba70ef60c51aa78a07253abbaa4df6ab"],["/posts/1776114197.html","2869be502bc2ae4748d5c2506c05ba02"],["/posts/1817748743.html","b73ff9dc49608e46a179b7058587763d"],["/posts/1925125395.html","4d994e96b0471284e187f2a9e1f80b16"],["/posts/1966191251.html","80c563ad4f5007b7b6084ffd88985e3b"],["/posts/1987617322.html","6e787a1bdabd2629c70575314d7177eb"],["/posts/1999788039.html","52d19bc5b494aa80120a9493f9d0560f"],["/posts/2007534187.html","5978ce80c3b756855e10f75b166ff986"],["/posts/2075104059.html","6d394b91f4002de565c00e1f64f31bec"],["/posts/2087796737.html","650b6a8568f605de25a0047253bd5d21"],["/posts/2106547339.html","23eb50d1b074e5ba800d3e239a236a90"],["/posts/2207806286.html","45fda3d2e76f1c9d968133d4a72aa6d5"],["/posts/2225903441.html","f4a73fe588f9f977a265d2169175bd45"],["/posts/2265610284.html","4099c4a4da601d97e617cc5d4517d74c"],["/posts/2281352001.html","1ea9bd95324546abde342aba6bad1a4a"],["/posts/2364755265.html","4f96fd7a2c20411993029302c7762826"],["/posts/2414116852.html","36e8a3866c3fc05b75f1bd3232331d13"],["/posts/2421785022.html","69fa446448458f1d134b1baeedfe2b7b"],["/posts/2482902029.html","99948d10502b230cb25835c145cd7a61"],["/posts/2495386210.html","f09fe80ef516abcadd91522ef7df1e4e"],["/posts/2516528882.html","f163e29a69dfba9861396127ad56a615"],["/posts/2522177458.html","6b2acbe83f61b6246598f75aca47d0d4"],["/posts/2526659543.html","03e5a9fd806c234869c559d059830dca"],["/posts/2529807823.html","c66bf081d1dce60310f4b6e5214f0624"],["/posts/2592249117.html","d7276d2d50a62c9418b942e202511ec5"],["/posts/2596601004.html","939890133eb5e21aa7f40f1fe7ac29cb"],["/posts/2697614349.html","df8cdea1db3df8866a0c3bf21cdc8118"],["/posts/2742438348.html","63ab0d18089cec74a11a50df92a1d53c"],["/posts/2768249503.html","c08af20cbd84953a6119a6dfbbfeab88"],["/posts/2864584994.html","cac0c5f0ddae5146f8cd3397530b91f2"],["/posts/2888309600.html","17d291ed46c98aca83d28e0d90d2a12d"],["/posts/2891591958.html","60539f28f56413c1297b5c7495e658d7"],["/posts/2909934084.html","5945f7376dd57bcb4fe3f5755e1c848b"],["/posts/2920256992.html","2294c6187aa56ac0c585e3c2b03048f4"],["/posts/2959474469.html","bcbbab659800bd8428693b381dcdb172"],["/posts/3005926051.html","ce408cccaf500a73f0ee4f2690629cb2"],["/posts/309775400.html","18de6bc1549157d0230c1354a2d17020"],["/posts/3156194925.html","0df9fb9b8b9fd69a44dab4efefb25ca9"],["/posts/3169224211.html","4606760841cf90055e4d04a663f1a915"],["/posts/3183912587.html","8e974f46f66569e413b523873d387243"],["/posts/3213899550.html","a15c95748961cef0e9459cf5f7419922"],["/posts/3259212833.html","8e93acb0292d70a5ad6976c3ebc21108"],["/posts/3265658309.html","beb5070c0c45f658682a48ca0f01ce7a"],["/posts/3266130344.html","fe2536f6e987eaddf5e229e6ea34ca92"],["/posts/3292663995.html","10606abb2980262c70f4bc2eb9f735ac"],["/posts/3297135020.html","8018d0d624071916af9b4f48c9580af7"],["/posts/3306641566.html","181534659f5a6b5176f52eddfedd278f"],["/posts/3312011324.html","329fa3cb5c4e5c0073a142d5c534c146"],["/posts/336911618.html","c279e4c9e8152a64f65ca44dc980daf5"],["/posts/3402121571.html","e61cfc0060377c8c91d4121a54c233b8"],["/posts/3405577485.html","c3e1a1d57b8c443d198d18f37d652335"],["/posts/3498516849.html","e3c56c0a12c9679d904de78a5c129f98"],["/posts/350679531.html","206c9d5639dd5ecfcc245d2f7fc84f40"],["/posts/3513711414.html","08b78cf5605f213ea43fe37bd50afaf8"],["/posts/3523095624.html","502f6408c9f6832405bf18e484f30154"],["/posts/3546711884.html","74a2d260e2da4b5450bf1929bca6085f"],["/posts/362397694.html","51a5259a6c521a0c51c4b30d51e785a9"],["/posts/3731385230.html","f2aa653a3735f2c0d7afa4433cb280aa"],["/posts/3772089482.html","4cf1de3edabfc06b5267c1995e41404b"],["/posts/386609427.html","6f3e87e9886dc103f06437404d4d4b6e"],["/posts/4044235327.html","8e577008ccc5fcf6cb8584822f18f012"],["/posts/4098221856.html","1481b465cd75451bfc2d587f9befa5c5"],["/posts/4115971639.html","750f8da23141a07e573d0fe16ea507bd"],["/posts/4130790367.html","794e5a5edf858c2e3aa76820cfc31936"],["/posts/4131986683.html","5e4aa4844441b7723e94524ee03aea84"],["/posts/4177218757.html","972b5bfc5af45328dcb10573c6a483d4"],["/posts/4192183953.html","2a6d4086c93a4ad60d1660000a269e6b"],["/posts/4223662913.html","fe640035df0638c8b9c66d60d2682273"],["/posts/4261103898.html","12ffeb45acb94138ec1805f416daea2c"],["/posts/4286605504.html","9cfae4c7a7d1bc46ac6716e5600a3ec0"],["/posts/449089913.html","5fdfbabdd65f7e07a8cc600aad38685f"],["/posts/469711973.html","43fa1c06e615dad6506d36688a7c7a13"],["/posts/482495853.html","d66b25b47b00c9b391287ab058d7c9ff"],["/posts/488247922.html","0de049ea4335eaf3f14d3bb5d0643ddb"],["/posts/517302816.html","0f47d3f8be4ba8eab74b668b7bd7896e"],["/posts/570165348.html","b464398bdcb5d47e3540467a410561e5"],["/posts/595890772.html","9b69b0081baca1680d2a9aa19ed110c6"],["/posts/67485572.html","fca611e902aaf41bdfae3ba7fa1d90e6"],["/posts/694347442.html","77df9c38a6eaea7e144a5503798819a5"],["/posts/707384687.html","4d711dfd5198c3e9491a695ded39a4c3"],["/posts/71180092.html","c19bccf28906220ad5a009f5811f0d14"],["/posts/716459272.html","ed2f285114bffd23d96d1364c77983cc"],["/posts/765481613.html","b9e4c082399aa9e3ab4727b71df0af89"],["/posts/778231993.html","261942ba8d50d9f4e363a3a7bc16cb5c"],["/posts/795397410.html","9114475f2c2612551a6b127bfe54fe5c"],["/posts/820223701.html","40771c749007916483459ca3ad7a2bc2"],["/posts/830372185.html","09741fe3a791bf78e160fa9bba06ac12"],["/posts/88294277.html","401878c557ee93ac82db7843ab1759be"],["/posts/939963535.html","a26964e4b2ed0388c502b12793c1ddb7"],["/posts/983786067.html","440b8a86c67e71a385d4eaeceb056c7f"],["/sw-register.js","73770edb3b3cc093022ed69022fa6a79"],["/tags/C/index.html","5f0f2cbddddbe1d05a21801ebc7b4b5b"],["/tags/C/page/2/index.html","922d3fa7f32aee066ace8b148c8bbfa6"],["/tags/C/page/3/index.html","2eba0f7cab9f9761c7a517fb745da486"],["/tags/C/page/4/index.html","c308905aaf952e2fc03220c10fb6e94c"],["/tags/ETL/index.html","04bab5445057afeb25aa547ab84ecc6d"],["/tags/ElasticSearch/index.html","7f83eabcec4f08bb67678d3eb73230eb"],["/tags/GUI/index.html","d569a3f84a5815cca3c7987f7bb18372"],["/tags/HBase/index.html","8b681c551d95bd08b106f766ec5bac6d"],["/tags/Hadoop/index.html","992f258a8d5bf134dcd36e2c1577ba41"],["/tags/Hadoop/page/2/index.html","6b0932203e6b4eaba2075fe0c1c4d363"],["/tags/Java/index.html","d7be4e2e5eac27a476b7b84f5c1173dd"],["/tags/Java/page/2/index.html","aa6bc9cf76f83361ab49c28c04aa60cb"],["/tags/Java后端/index.html","62e4a7ba00d3b48c6e8c774d666a6228"],["/tags/Java后端/page/2/index.html","711ada81a62de943172934fd9e6152c4"],["/tags/Kettle/index.html","b5e601a25e25ef13bb08afb1cb1b9d15"],["/tags/Kibana/index.html","9793d4dccbc020fbcd013f499ff625b9"],["/tags/Linux/index.html","1e7fba8f785c9e0e7590538699be2768"],["/tags/Linux/page/2/index.html","adfd3602f50237f8914dd624e57f6917"],["/tags/Linux/page/3/index.html","42698398c81d7b036978f0ef46a1ba94"],["/tags/Mac/index.html","15bb39320988669e54429e56e115c29b"],["/tags/Mac/page/2/index.html","0729984f517e8e87b3ed915c5bab5b42"],["/tags/Maven/index.html","34d893f0f7d03ebac206fe55806ec3cd"],["/tags/MySQL/index.html","12b9044ae9c3ec36841bc7612ae00a0a"],["/tags/Python/index.html","17978fde1477435ab0cc31daf89875a0"],["/tags/Redis/index.html","d6887fb824f18a0799c2191f29241eab"],["/tags/R语言/index.html","a89d091bab2d00032088caebc772d5f5"],["/tags/Spark/index.html","0765e0ba61e90967b10574cec9a5a477"],["/tags/Ubuntu/index.html","e5959b218976f6811d4c135feb55add7"],["/tags/Vue/index.html","9c24c632d5104ad211100ae81255ba0a"],["/tags/Windows/index.html","e16cddb5f1f2b073d0d64b0cc88117ea"],["/tags/ZooKeeper/index.html","dd247f05e0561c550f61106e6ece7ded"],["/tags/bfs/index.html","fe2e37d59c1b8fa300671158c97e75a4"],["/tags/dfs/index.html","65be214e6ee5f2a13b44df3236702860"],["/tags/folium/index.html","fa07fb2af3fbbe0a897247084cbd80ba"],["/tags/git/index.html","d302cec0b2dc34005aacfbc922ee1c0e"],["/tags/iPad找电子书/index.html","873906891fc41ee24523cb7c868de3f8"],["/tags/index.html","bf1bb09d56632f84e7dc631ea8229bf5"],["/tags/latex/index.html","4c873bdeea463da8218008bea6515f22"],["/tags/中间件/index.html","3af36cd062bdfb3f838a2e969b14896e"],["/tags/二分查找/index.html","cff16c50ce2a1ff678c21964ab1bf440"],["/tags/优化类/index.html","6e654f19d95bf8e0cfff8605b4c9c478"],["/tags/前端/index.html","991b3518c3bb46da383289fe93615944"],["/tags/前缀和与差分/index.html","61ea3738cafc5e838b7b3e063b28fc82"],["/tags/动态规划/index.html","21baa5f83b4df2d3fd502ce128f21798"],["/tags/动态规划/page/2/index.html","439f047733397f96eb89e693ffca2e0e"],["/tags/博客搭建/index.html","3cb607ece5841186dc636a1488fb16c9"],["/tags/图论/index.html","391d53e80c65ff8e1d44932733e1b57b"],["/tags/图论/page/2/index.html","3655050aae8e12594ebd66382c8da344"],["/tags/大数据/index.html","18d35a0feef49b687b47887a741e874f"],["/tags/大数据/page/2/index.html","bd608c3c80c983d6d0b2eedf5d2f989f"],["/tags/宽度优先搜索算法/index.html","24540076e04a612d2cb28e23c7206af7"],["/tags/排序/index.html","9aa174cf9c67cd3c52de1f10cc272cc1"],["/tags/操作系统/index.html","5afa1129990fe0951f2a67feee551724"],["/tags/数学建模/index.html","fcb48e580f85b58d7feb58f80b4c3a39"],["/tags/数据库/index.html","43df45e38f11cf318696dc699ee11821"],["/tags/数据结构和算法/index.html","40c9f9b85e532683efe13a6af52c16c1"],["/tags/数据结构和算法/page/2/index.html","245d74f27d4b285916cc91e1de36d146"],["/tags/数据结构和算法/page/3/index.html","64ef764510ef2e27999f293f6f1e2897"],["/tags/数据结构和算法/page/4/index.html","91212820f3d95d0603ea31609433eb2e"],["/tags/数据结构和算法/page/5/index.html","193ae6999726f1d0d287dc4a44918ddc"],["/tags/数组和字符串/index.html","c1bd3eeec607fd4892368a7f3aacf1d7"],["/tags/数论/index.html","f7585d5456770107c3fcf283a912452f"],["/tags/枚举类/index.html","6ba9424d8e740400a0005d0fe7492ea2"],["/tags/栈和队列/index.html","5cf9faa5ed8a194fb75cf92fcb1835b4"],["/tags/树论/index.html","3ad4f1a9d9b77e58e60ae74816a09763"],["/tags/测试/index.html","c6ad8352476dde8ca6b220b9972dc0e4"],["/tags/深度优先搜索算法/index.html","84bbe4eb29ef2b79cefa729664dd248e"],["/tags/环境/index.html","f7ae5b7042cd25cc66fa21fd14289eed"],["/tags/环境变量/index.html","07b9e57c570fff218a955b69edb8ac16"],["/tags/绘图/index.html","b9d1c50c6082dac81568f016ddf32eb8"],["/tags/编程工具/index.html","7d8a1b84599b8d68ea3cf51a4863a321"],["/tags/编程环境/index.html","598c8182ccef5d5987dd9f5c83e505f6"],["/tags/网络编程/index.html","e7673ec7a867be29a5eeedfe1201499e"],["/tags/英语语法/index.html","802075a40453e261cf75be4dda27356f"],["/tags/计算机操作系统/index.html","e60d0d8943ec8c57d52e662fa0ad20e3"],["/tags/论文/index.html","ca5922d549e6e71dbab6f7415507f2ee"],["/tags/资源下载/index.html","a26892d58312cc496613e3476632fcdd"],["/tags/链表/index.html","f45867c371dbffe6a6964c404b8c5a67"],["/tags/集合/index.html","b0aaf7fe4b1c554de5473e1105e1b177"],["/tags/集群/index.html","32a600aacc9a2bbb1cb5afdabaa7ab7b"]];
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
