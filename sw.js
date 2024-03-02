/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","cc629bbd775622695faac36b1f2b5797"],["/about/index.html","931dfccaea981f435593fe9093f64465"],["/archives/2023/01/index.html","f089aaa676fa49efb0c4431753cf8b71"],["/archives/2023/02/index.html","065ff60faa57c508e4694e15cddfd92c"],["/archives/2023/02/page/2/index.html","51f82cf6311045d7055b89d53e5f6f16"],["/archives/2023/02/page/3/index.html","cc9f901cf019bb6feba47d2abc8d1b10"],["/archives/2023/03/index.html","f5759670d234edd0ab927ed455d1a831"],["/archives/2023/05/index.html","c4a5482719e8835bc7eba35b99645683"],["/archives/2023/06/index.html","0e5893c3aa854d9be025fa6f77bb4787"],["/archives/2023/09/index.html","6e8f0036498ab35ffdc832f73d22a7da"],["/archives/2023/11/index.html","1ace26a33755d0b39856fbbefc0ff4fe"],["/archives/2023/12/index.html","0c03a242f6e6b85a07dec9758b0d8101"],["/archives/2023/index.html","c452870dcceb4006c81217930d111b2a"],["/archives/2023/page/2/index.html","e8b8a6f984be15085c7290e672938940"],["/archives/2023/page/3/index.html","2a3d59805cbd02e8b8ce3834317a74dc"],["/archives/2023/page/4/index.html","81cb116678a55306e36aa0b4716b637e"],["/archives/2023/page/5/index.html","042dedd6a829aae445da377c44ca5045"],["/archives/2024/02/index.html","7f5d52f3bbf20c51460c9e7c9a2dab0b"],["/archives/2024/index.html","933a843dbe8a2fe3a297ffa50cb80a43"],["/archives/index.html","8904cf8733a37bee1ea5e1c7c01731fd"],["/archives/page/2/index.html","f96617a4ec5ed9cd50a8073881b9c992"],["/archives/page/3/index.html","a382ecd520ec604ad4e329d61324fc61"],["/archives/page/4/index.html","852b00ddb8d61cf4656a8e5916ad7281"],["/archives/page/5/index.html","dd6102554db08de92ef425e4324bb243"],["/baidu_verify_codeva-qQP2iZOMLX.html","aaa130d1a4f4a8fcfb4c676341fef395"],["/categories/Java/index.html","15821fba4a2f82554b0031da6762bf26"],["/categories/Java/后端/index.html","f717847f339b3f4de56010f31e4c590f"],["/categories/Java/基础/index.html","61a2fa94110959f3f395ea36488a5607"],["/categories/Java/基础/集合/index.html","a2ac98edfe88786e259b21a8a8ff6ac9"],["/categories/Python/index.html","8f5cdd0490de08af3453cf6aaf07934b"],["/categories/Python/编程环境/index.html","495d66016d0e9259ce641f8e2993788c"],["/categories/R语言/index.html","8e4c926dce434e200e3d7bb589f23a62"],["/categories/R语言/编程环境/index.html","8a6f37e9d67c33408cbda06c3a2b702a"],["/categories/iPad/index.html","2f736e97569429186cdf50774c194d3c"],["/categories/index.html","30b145fb837a9e72ad81163ed6bb8f27"],["/categories/中间件/index.html","d26208165d4e4cd9f3422e339008d80b"],["/categories/前端/Vue/index.html","b97f9c2734a7896f97dbcea62e3e56b7"],["/categories/前端/index.html","0ee4b9ea0ba05fa6b91996d1f9de6d7d"],["/categories/大数据开发/ElasticSearch/index.html","a0f620ea77e87dca16963a6380acaf4f"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","353e94db32c2517c2734aa1d59299186"],["/categories/大数据开发/HBase/index.html","7924ffd4c42f2620bc906349de6a8fa5"],["/categories/大数据开发/HBase/学习笔记/index.html","8bcdcb46f9891bd250d5d117b916cfc8"],["/categories/大数据开发/HBase/环境搭建/index.html","cc73fadddaad387225ad2abfceb72086"],["/categories/大数据开发/Hadoop/index.html","920207e07b966836d0058bcb5f8b1a07"],["/categories/大数据开发/Hadoop/技术/index.html","418fa14c733d66ff99eca3196572d57b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","ae7d84db9ed459d59a31c75456d24487"],["/categories/大数据开发/Redis/index.html","1a44d6d77e98917cf53cbaf22703c39b"],["/categories/大数据开发/Redis/技术/index.html","e2a0fbe6f4d64e420a3533064ee308da"],["/categories/大数据开发/Redis/环境搭建/index.html","3ec0810fd67ce0cdcb267dfbe0be9226"],["/categories/大数据开发/Spark/index.html","238a58d79e697d5129db515fb8efe3fd"],["/categories/大数据开发/Spark/环境搭建/index.html","00b88c13dc8a84b9807db0ecd8f29ea0"],["/categories/大数据开发/Zookeeper/index.html","cb1cd28bc9bdbed9380f0d55443cc39e"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e359bf825b9c0fc9c00a0654a983f96d"],["/categories/大数据开发/index.html","b31d8ba87c66f4a2c4441166de545037"],["/categories/学校课程/index.html","bc2a2c70c89cced91b56ca7551038f18"],["/categories/学校课程/计算机操作系统/index.html","c44890e64473c814fe987ff71d8d9ea9"],["/categories/操作系统/Linux/index.html","ebeede765d799166c4e738adf681022a"],["/categories/操作系统/Mac/index.html","560baafc4f5e217a5ea49f960d4b38ab"],["/categories/操作系统/Windows/index.html","e1afe3e70e91b7a15f40b44406d552ae"],["/categories/操作系统/index.html","1cb1e5b168180913920937d0300e5934"],["/categories/数学建模/index.html","41a68939687d382850fd93e2e898732c"],["/categories/数学建模/latex/index.html","e1fbcae95a812e7214decb5e70327beb"],["/categories/数学建模/优化类/index.html","0bf1895d1e178234e57621adf13a6a9a"],["/categories/数学建模/优化类/现代优化算法/index.html","c3043ab670569fbd95a5b363ec4e5bbe"],["/categories/数学建模/优化类/规划类/index.html","5dea9a0616047107458fedb9034e3ae8"],["/categories/数学建模/绘图/index.html","289712f72980593a58f1f229c311b828"],["/categories/数据库/MySQL/index.html","1b3825565b4835b1b49acf0847a70f24"],["/categories/数据库/index.html","4ba2b12a16b43270bbc5011a6496775d"],["/categories/数据结构和算法/index.html","3aea8d231e16fefe0b2f5acb389ad6e1"],["/categories/数据结构和算法/page/2/index.html","a32b34b725ac601fc57b484e1767d64e"],["/categories/数据结构和算法/基本原理/bfs/index.html","86ff0b788d7003b4aac65a7ef483ee24"],["/categories/数据结构和算法/基本原理/dfs/index.html","b656ffa661ab41c4fa63623876e2b1a5"],["/categories/数据结构和算法/基本原理/index.html","9de0e8da7f4a4009c9c14516e6972752"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","3190f215effd1016e3606ada9b985365"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f2ec7c20485c42a9843025f8f8d12f33"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0940c86c24fda1cc024ef34079fa0ebb"],["/categories/数据结构和算法/基本原理/图论/index.html","1fc77bbe693d25551b6e570d0fa32a3c"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","2c460a1d8d6c852b25236ddb7ec73d33"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","e15344be6935d266b8f50b4af5140b6b"],["/categories/数据结构和算法/基本原理/字符串/index.html","14d17291058c25ee1311ffe0372f0418"],["/categories/数据结构和算法/基本原理/排序/index.html","6208ee23637c603129783e4757876f56"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","87c22860812fa77deafef08f402c04e9"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","660649b6f20d93126d958b701e200dd7"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","c84cf9ef4758dbd49bd6208519ebe2ef"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ea4a92bfa68ebac24deb7a8e92e0c30e"],["/categories/数据结构和算法/基本原理/链表/index.html","f4e73d9842d3940d89269037d56fffc7"],["/categories/数据结构和算法/算法题/index.html","e4a9fc76c697ce6815b1b7b93975d29a"],["/categories/数据结构和算法/算法题/二分查找/index.html","f8d0afa26f638f4fc4b53338e9a9409f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1d49d1f0b1595bb99e5e322b369a60d6"],["/categories/数据结构和算法/算法题/动态规划/index.html","3b08e3defc6db951abf30bc44db9e6a7"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","476c54cb626c01885b84a3f38b5a595c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","eba954ea09809d91bea06789c69aa37b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","396e788a5c5864be4f4ae7ca241685a4"],["/categories/数据结构和算法/算法题/图论/index.html","face20938080d53f5d9691b7efcbfdf0"],["/categories/数据结构和算法/算法题/图论/树论/index.html","2e72f8a2c33cc88700a0307ab7c9cf00"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c1e525ad4850452e795eaefc0626c48e"],["/categories/数据结构和算法/算法题/数论/index.html","1f5b4b416427892b12a9f0580ee470fd"],["/categories/数据结构和算法/算法题/栈和队列/index.html","ed5f39ae8ceac4c5afbda29fd3747490"],["/categories/杂七杂八/index.html","cdb771468f4323073c6127e3bad76b1c"],["/categories/杂七杂八/博客搭建/index.html","67d8511898c499b286d5553813bc0b38"],["/categories/编程工具下载/index.html","058c64f5bf390b778a3d9cd86287d358"],["/categories/编程环境/index.html","36aef9371148385637b52f94e424dbf2"],["/categories/编程环境/大数据/index.html","44356a595135e1f0368fe0f242ddfd58"],["/categories/英语学习/index.html","bcefbf1fe31ddf3f62e353e6ea090ba4"],["/categories/英语学习/英语语法/index.html","a1e838a39adb1f309cdfb449d77145a2"],["/comments/index.html","083c012e96cfd09a095930ded63c3178"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","3c2159a3ef0a2f6df72d5529b3e9732b"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","2e1bda18624a0459e3c23b61805a7691"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","7968732533b7d96b8e85e3abc0c8a168"],["/movies/index.html","4ce7d7d1e6cef7c724157a162df53550"],["/music/index.html","a34e25ec7b933acaad18182f9efecf41"],["/page/2/index.html","9a8c8c7a77406b272151713d48083981"],["/page/3/index.html","35e8a3c1a51f70286b2a16064477c1ed"],["/page/4/index.html","4c0552c35c785ab34d6c0588366c4f97"],["/page/5/index.html","d8021f564997af69ee56502886dfa979"],["/page/6/index.html","f21564842942509de78f15a3a0212806"],["/page/7/index.html","abdac65a1bb554f9d39ea8246af15e12"],["/posts/1021360842.html","898166621b0150c5f84af119ee93701c"],["/posts/1120620192.html","7bf606fe055ae770508a9e5b1413f790"],["/posts/1137707673.html","a5149345ee7c409975fdc9c6a0017dcd"],["/posts/1141628095.html","0879e32907639d56e4ff9d0e7650ab6f"],["/posts/1168613674.html","ab4a57a4e6399663fbd2b6a9a745a1fd"],["/posts/1219920510.html","c99a98cd763b12d99e018f5b19562865"],["/posts/1222166338.html","00edbb5cd91116309b676d54c8709c32"],["/posts/1259097482.html","d9e9e6fbd2298d6b80ff56d968557496"],["/posts/1271036369.html","3b401e81b9efda2833d19f9154c803f5"],["/posts/1312847445.html","c2253542f6cc2ede41f33db1dac2f66f"],["/posts/135355774.html","70e5ea0690322d2f5ea48c5a5cd178c9"],["/posts/1375344716.html","ff7d1557baac1f9053951b3a325cfcdd"],["/posts/1388991698.html","782f5ee25965b54256c459c34d45914e"],["/posts/1410315814.html","650d94ddbc53be0d99a392a0989b4a04"],["/posts/1452790229.html","8b5a8ad586ddafe5c7e1a158e796b572"],["/posts/1470079884.html","ef3ef497fd2bbeeb0f515eed71c5f5dc"],["/posts/1470079885.html","5cace85e8c098a295aec9a2a83678c09"],["/posts/1470079886.html","c302c8ef192f9cf8d64c1a7e82e82cb4"],["/posts/1470079887.html","df54679d2f54d35adcde9bb62830a583"],["/posts/1498536549.html","70f77cee804deb81afa4831121db91a1"],["/posts/1539568593.html","a643c2f3fd6a93228b401b3597f0eca9"],["/posts/1547067935.html","fc9150650f15aa22bc8fe5c43f64a4b5"],["/posts/1557866301.html","ded6f71c9ddeb4e097cfccc4b08e3705"],["/posts/1571776361.html","3931a7155064f0a9f27828600a61af2a"],["/posts/1605124548.html","4aa26d0add5e28dfaa1a3e37ce8438ab"],["/posts/1633036852.html","8bc7024819aac2f638297ee0608a406a"],["/posts/1667740714.html","4eec944915ba6c48bd5529277435f7fe"],["/posts/1674202625.html","4ffd57061a018a5720b1c787331e1a20"],["/posts/1765123828.html","bb0db37d337043a9738e066fee2c0805"],["/posts/1767336200.html","ba70ef60c51aa78a07253abbaa4df6ab"],["/posts/1776114197.html","2869be502bc2ae4748d5c2506c05ba02"],["/posts/1817748743.html","b73ff9dc49608e46a179b7058587763d"],["/posts/1925125395.html","4d994e96b0471284e187f2a9e1f80b16"],["/posts/1966191251.html","80c563ad4f5007b7b6084ffd88985e3b"],["/posts/1987617322.html","6e787a1bdabd2629c70575314d7177eb"],["/posts/1999788039.html","52d19bc5b494aa80120a9493f9d0560f"],["/posts/2007534187.html","5978ce80c3b756855e10f75b166ff986"],["/posts/2075104059.html","6d394b91f4002de565c00e1f64f31bec"],["/posts/2087796737.html","650b6a8568f605de25a0047253bd5d21"],["/posts/2106547339.html","23eb50d1b074e5ba800d3e239a236a90"],["/posts/2207806286.html","45fda3d2e76f1c9d968133d4a72aa6d5"],["/posts/2225903441.html","f4a73fe588f9f977a265d2169175bd45"],["/posts/2265610284.html","4099c4a4da601d97e617cc5d4517d74c"],["/posts/2281352001.html","1ea9bd95324546abde342aba6bad1a4a"],["/posts/2364755265.html","4f96fd7a2c20411993029302c7762826"],["/posts/2414116852.html","36e8a3866c3fc05b75f1bd3232331d13"],["/posts/2421785022.html","69fa446448458f1d134b1baeedfe2b7b"],["/posts/2482902029.html","99948d10502b230cb25835c145cd7a61"],["/posts/2495386210.html","f09fe80ef516abcadd91522ef7df1e4e"],["/posts/2516528882.html","f163e29a69dfba9861396127ad56a615"],["/posts/2522177458.html","6b2acbe83f61b6246598f75aca47d0d4"],["/posts/2526659543.html","03e5a9fd806c234869c559d059830dca"],["/posts/2529807823.html","c66bf081d1dce60310f4b6e5214f0624"],["/posts/2592249117.html","d7276d2d50a62c9418b942e202511ec5"],["/posts/2596601004.html","939890133eb5e21aa7f40f1fe7ac29cb"],["/posts/2697614349.html","df8cdea1db3df8866a0c3bf21cdc8118"],["/posts/2742438348.html","63ab0d18089cec74a11a50df92a1d53c"],["/posts/2768249503.html","c08af20cbd84953a6119a6dfbbfeab88"],["/posts/2864584994.html","cac0c5f0ddae5146f8cd3397530b91f2"],["/posts/2888309600.html","17d291ed46c98aca83d28e0d90d2a12d"],["/posts/2891591958.html","60539f28f56413c1297b5c7495e658d7"],["/posts/2909934084.html","5945f7376dd57bcb4fe3f5755e1c848b"],["/posts/2920256992.html","2294c6187aa56ac0c585e3c2b03048f4"],["/posts/2959474469.html","bcbbab659800bd8428693b381dcdb172"],["/posts/3005926051.html","ce408cccaf500a73f0ee4f2690629cb2"],["/posts/309775400.html","18de6bc1549157d0230c1354a2d17020"],["/posts/3156194925.html","0df9fb9b8b9fd69a44dab4efefb25ca9"],["/posts/3169224211.html","4606760841cf90055e4d04a663f1a915"],["/posts/3183912587.html","8e974f46f66569e413b523873d387243"],["/posts/3213899550.html","a15c95748961cef0e9459cf5f7419922"],["/posts/3259212833.html","8e93acb0292d70a5ad6976c3ebc21108"],["/posts/3265658309.html","beb5070c0c45f658682a48ca0f01ce7a"],["/posts/3266130344.html","fe2536f6e987eaddf5e229e6ea34ca92"],["/posts/3292663995.html","10606abb2980262c70f4bc2eb9f735ac"],["/posts/3297135020.html","8018d0d624071916af9b4f48c9580af7"],["/posts/3306641566.html","181534659f5a6b5176f52eddfedd278f"],["/posts/3312011324.html","329fa3cb5c4e5c0073a142d5c534c146"],["/posts/336911618.html","c279e4c9e8152a64f65ca44dc980daf5"],["/posts/3402121571.html","e61cfc0060377c8c91d4121a54c233b8"],["/posts/3405577485.html","c3e1a1d57b8c443d198d18f37d652335"],["/posts/3498516849.html","e3c56c0a12c9679d904de78a5c129f98"],["/posts/350679531.html","206c9d5639dd5ecfcc245d2f7fc84f40"],["/posts/3513711414.html","08b78cf5605f213ea43fe37bd50afaf8"],["/posts/3523095624.html","502f6408c9f6832405bf18e484f30154"],["/posts/3546711884.html","74a2d260e2da4b5450bf1929bca6085f"],["/posts/362397694.html","51a5259a6c521a0c51c4b30d51e785a9"],["/posts/3731385230.html","f2aa653a3735f2c0d7afa4433cb280aa"],["/posts/3772089482.html","4cf1de3edabfc06b5267c1995e41404b"],["/posts/386609427.html","6f3e87e9886dc103f06437404d4d4b6e"],["/posts/4044235327.html","8e577008ccc5fcf6cb8584822f18f012"],["/posts/4098221856.html","1481b465cd75451bfc2d587f9befa5c5"],["/posts/4115971639.html","750f8da23141a07e573d0fe16ea507bd"],["/posts/4130790367.html","794e5a5edf858c2e3aa76820cfc31936"],["/posts/4131986683.html","5e4aa4844441b7723e94524ee03aea84"],["/posts/4177218757.html","972b5bfc5af45328dcb10573c6a483d4"],["/posts/4192183953.html","2a6d4086c93a4ad60d1660000a269e6b"],["/posts/4223662913.html","fe640035df0638c8b9c66d60d2682273"],["/posts/4261103898.html","12ffeb45acb94138ec1805f416daea2c"],["/posts/4286605504.html","9cfae4c7a7d1bc46ac6716e5600a3ec0"],["/posts/449089913.html","5fdfbabdd65f7e07a8cc600aad38685f"],["/posts/469711973.html","43fa1c06e615dad6506d36688a7c7a13"],["/posts/482495853.html","d66b25b47b00c9b391287ab058d7c9ff"],["/posts/488247922.html","0de049ea4335eaf3f14d3bb5d0643ddb"],["/posts/517302816.html","0f47d3f8be4ba8eab74b668b7bd7896e"],["/posts/570165348.html","b464398bdcb5d47e3540467a410561e5"],["/posts/595890772.html","9b69b0081baca1680d2a9aa19ed110c6"],["/posts/67485572.html","fca611e902aaf41bdfae3ba7fa1d90e6"],["/posts/694347442.html","77df9c38a6eaea7e144a5503798819a5"],["/posts/707384687.html","4d711dfd5198c3e9491a695ded39a4c3"],["/posts/71180092.html","c19bccf28906220ad5a009f5811f0d14"],["/posts/716459272.html","ed2f285114bffd23d96d1364c77983cc"],["/posts/765481613.html","b9e4c082399aa9e3ab4727b71df0af89"],["/posts/778231993.html","261942ba8d50d9f4e363a3a7bc16cb5c"],["/posts/795397410.html","9114475f2c2612551a6b127bfe54fe5c"],["/posts/820223701.html","40771c749007916483459ca3ad7a2bc2"],["/posts/830372185.html","09741fe3a791bf78e160fa9bba06ac12"],["/posts/88294277.html","401878c557ee93ac82db7843ab1759be"],["/posts/939963535.html","a26964e4b2ed0388c502b12793c1ddb7"],["/posts/983786067.html","440b8a86c67e71a385d4eaeceb056c7f"],["/sw-register.js","2de4fca62f9831f6024504eedda44c9b"],["/tags/C/index.html","ea2bbcfee0b9c08da524ed57b71d22ff"],["/tags/C/page/2/index.html","01b03f8ac2d0d25350a623039645889b"],["/tags/C/page/3/index.html","efeac6779eb9cd064e5853b76758e5e8"],["/tags/C/page/4/index.html","2ea3f915992b1fe70a373857903f4d0f"],["/tags/ETL/index.html","720edc7a1137bef340e10000881b0472"],["/tags/ElasticSearch/index.html","3da624c3ebe0673f4485d94da5d4509e"],["/tags/GUI/index.html","69630a478b49d38097f6da91335c1dd2"],["/tags/HBase/index.html","a144504c9da406f3ccec7c2fecb3f4c5"],["/tags/Hadoop/index.html","7c7c0a8dfb41ca2a582003ae899999a8"],["/tags/Hadoop/page/2/index.html","d06e92840fcb7e5495a9eb6eadd8e38c"],["/tags/Java/index.html","6c6dd9fbf3e94a833b53b753cb1c112b"],["/tags/Java/page/2/index.html","99b1d2e5f411b2942d545995a54d353f"],["/tags/Java后端/index.html","6cfa2c8b26136469a490225b2c3229bd"],["/tags/Java后端/page/2/index.html","7193d5015c3ca3b3e8c14f50d5bf5a6f"],["/tags/Kettle/index.html","31fe5ea0d5dad100dba326db2abc4f8b"],["/tags/Kibana/index.html","ed70615d60ad35d1abdc1959f52c1b3e"],["/tags/Linux/index.html","a554f09433991187eab21d4db32f1d3a"],["/tags/Linux/page/2/index.html","b78e695e94c319d9063320f87fc28877"],["/tags/Linux/page/3/index.html","9291933b4f8b39e05081a5806aa910e1"],["/tags/Mac/index.html","f43ff24a32207770f0a34eb8df83bc91"],["/tags/Mac/page/2/index.html","f864890364cfe157a7780c79936d722b"],["/tags/Maven/index.html","c98e1a326dbe0149e839821e0c662d57"],["/tags/MySQL/index.html","bfe48a002d7867713069eccd824d232c"],["/tags/Python/index.html","c09ea1803a5b8ec11cc3ce9265d6b8e3"],["/tags/Redis/index.html","27a2cce097962a5d1f8b60c701baf4b8"],["/tags/R语言/index.html","ae1d23fd1ac571c745b2a0e6640837c1"],["/tags/Spark/index.html","f520cef7ed2da4bfdfb0674eacb03bcc"],["/tags/Ubuntu/index.html","eb7b48622f45cf0394b10ccf27f04948"],["/tags/Vue/index.html","d57dc526d26ae3b90d5817bbe9740640"],["/tags/Windows/index.html","2e87731ceb76de80c983f4fb8aefc254"],["/tags/ZooKeeper/index.html","33fbd5b7e94fa23fc08d2cd76e9f2ad7"],["/tags/bfs/index.html","b391c9c91036c538a3d6987e9945ad33"],["/tags/dfs/index.html","f2ee885cb3378de61368b9feed052bdb"],["/tags/folium/index.html","debea60f4b9cdddf2cd650d114274e18"],["/tags/git/index.html","d5032116dbf3432a10cb46ae19c1bd82"],["/tags/iPad找电子书/index.html","a67da94e3d74915356d0be00ef065a74"],["/tags/index.html","09d9862cd74ed95c21418898bac5c16c"],["/tags/latex/index.html","53c5dce97d813983a07edd1324f8d43a"],["/tags/中间件/index.html","55e6874b5a3d33f4ec45b7578fac1d62"],["/tags/二分查找/index.html","45c66fe6fc6dcd5992f4fcf8a1e8bb9e"],["/tags/优化类/index.html","c61715c5c163adc85db29fb5fcf1197b"],["/tags/前端/index.html","7260a4a6b5da842f840c15a3dcca0915"],["/tags/前缀和与差分/index.html","f407c811d10c64f70538a142797a8692"],["/tags/动态规划/index.html","dd53b8e582dfd0035269ecff2da78974"],["/tags/动态规划/page/2/index.html","3a0b4fbb030b0c9cd4e0dd9ce5a6cb0a"],["/tags/博客搭建/index.html","3034bab5d1f2c06e13f20751e0dfd5ba"],["/tags/图论/index.html","a380089ab93da1c95cffebf9bade2692"],["/tags/图论/page/2/index.html","5fa6b15476c6ea9dda7f64c8f14be1a8"],["/tags/大数据/index.html","908a47f9e112532b3a9557a8752c2a5c"],["/tags/大数据/page/2/index.html","6e239a8ddc6f7ac63974d075461f1256"],["/tags/宽度优先搜索算法/index.html","0110666be3918d09adea019909ff70d5"],["/tags/排序/index.html","cd95001600e0824e811b7a43398b3638"],["/tags/操作系统/index.html","f13ddd7371690e4278b022684a0bb47c"],["/tags/数学建模/index.html","a7e22f8c7c04b4641872b80478f8c3a5"],["/tags/数据库/index.html","e839bf1d98990400b8e3c2bc9857d7fd"],["/tags/数据结构和算法/index.html","c86d29f3bb01722acc9b40691162e946"],["/tags/数据结构和算法/page/2/index.html","ad66e2e2a64c9de42e07d793da9c4795"],["/tags/数据结构和算法/page/3/index.html","66984c2301f8be45d344bf253c21866e"],["/tags/数据结构和算法/page/4/index.html","f7cdbf28f51443949c2de4e1e5c0fa89"],["/tags/数据结构和算法/page/5/index.html","755405d90dde6e14b679fabac1e37a00"],["/tags/数组和字符串/index.html","7f397fbfa7f2b4a287e647df14011929"],["/tags/数论/index.html","d54139c2496358be5fc6f1ea7437434f"],["/tags/枚举类/index.html","825e6624db5cd147699a30380f597f3d"],["/tags/栈和队列/index.html","b9fa7db4d552662846454fb8f7ed0db5"],["/tags/树论/index.html","f575da8c41957b1d9f4fd07acd407578"],["/tags/测试/index.html","44916e844900d2c864c48b9e307e99a1"],["/tags/深度优先搜索算法/index.html","726bbbd77a3d2aca6ea51029e857b862"],["/tags/环境/index.html","b0c7a6e31bcf11ac85e9ed7b5cf5a65b"],["/tags/环境变量/index.html","7ce5a02813815d7c6de1977bd1ea7a4a"],["/tags/绘图/index.html","64798d17c133d18ca6ec4d9df0c6b7a6"],["/tags/编程工具/index.html","ab4550d8ded1e3820261ff8af75209f6"],["/tags/编程环境/index.html","f15fc38e263075ec7b65e32168393984"],["/tags/网络编程/index.html","4a1358ce4be6d9a1bf1b7003590fb3f9"],["/tags/英语语法/index.html","4dcdcc3b6f3cebdcae713d2fec476ab2"],["/tags/计算机操作系统/index.html","58e2b9c55e389f2e06454a5ae33c90b3"],["/tags/论文/index.html","9501538696bd37f2a4f8fe042adcbf9d"],["/tags/资源下载/index.html","5baaa640a585ce656042dc725a1485ef"],["/tags/链表/index.html","7afe454c5388a03fcfbc141170f1a142"],["/tags/集合/index.html","53fb857597ff83254d5cd1ca4670a4e5"],["/tags/集群/index.html","4d6bb482afe4da00f9e0502d1717f6d4"]];
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
