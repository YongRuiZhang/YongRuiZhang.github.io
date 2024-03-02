/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","64f7540e5e388363419ae6a0e972536c"],["/about/index.html","931dfccaea981f435593fe9093f64465"],["/archives/2023/01/index.html","ff78ac388126ad30001323040d505ca6"],["/archives/2023/02/index.html","ce53a421de7448432d0c5158c255d0ec"],["/archives/2023/02/page/2/index.html","d0950713193cda9ed324d5e88c9138d2"],["/archives/2023/02/page/3/index.html","69a6cf709077260df729a426581514f7"],["/archives/2023/03/index.html","16b257d3ce7111285e054731c6e9051a"],["/archives/2023/05/index.html","761a21987c2b5731a637fc6a93d5c5ad"],["/archives/2023/06/index.html","5cc1756b80c0bdd891da6b2197203323"],["/archives/2023/09/index.html","47fea1ddf9e5fbb290686492cc0219a9"],["/archives/2023/11/index.html","8eb3e7831adbc3d501806aa088c34e59"],["/archives/2023/12/index.html","17f8a838b5ac1a550062f80feb795124"],["/archives/2023/index.html","a2bdabdc5a7820b91095430984fdc4db"],["/archives/2023/page/2/index.html","c301a86596d6f4ea6945418f70652267"],["/archives/2023/page/3/index.html","5d62b20cbdbbdb41992f2d92471d05c2"],["/archives/2023/page/4/index.html","00932f0fa77023b47a9aa749736aa4cf"],["/archives/2023/page/5/index.html","df429dbe5d889db5e2fc29bfcad9569f"],["/archives/2024/02/index.html","bc4c378f8c7a116f499da803a344cd33"],["/archives/2024/index.html","785eba36081b3cabc0f5e8125414aa53"],["/archives/index.html","7800604bf6398333f1cb47229204ea91"],["/archives/page/2/index.html","d98629996116385da1b2a6f023d8be45"],["/archives/page/3/index.html","99982b8ef024b2f18b3e9632eb5dbbc4"],["/archives/page/4/index.html","ca8f657818ddffde488c40604add6fc6"],["/archives/page/5/index.html","4978007201b05ef38e0b88f593f6c93a"],["/baidu_verify_codeva-qQP2iZOMLX.html","ea6f984241a060fa09eaf6fb50249837"],["/categories/Java/index.html","7bfeebc2ad15652b3b16c3328bc5d8e2"],["/categories/Java/后端/index.html","23675af6bab03ff2e73b875913e9cbd4"],["/categories/Java/基础/index.html","f29de225223a9cb7cebbeae3ddadf808"],["/categories/Java/基础/集合/index.html","c0eae26efca9a7c8003b6fa983df4746"],["/categories/Python/index.html","f1ea46912e22c451ef9d3c70e0af1c10"],["/categories/Python/编程环境/index.html","b31896eff28f2c08c7e08a0ce5de5975"],["/categories/R语言/index.html","7a245ad6466b724856e86c7fccf0e48f"],["/categories/R语言/编程环境/index.html","e4fcdcd1659935c469cb34efb2bda85b"],["/categories/iPad/index.html","96997a1cdd18a6610331bccde2d18175"],["/categories/index.html","30b145fb837a9e72ad81163ed6bb8f27"],["/categories/中间件/index.html","86c71bf83faf12c952f41813a3927c44"],["/categories/前端/Vue/index.html","8e4a5634eb77cc6659939b7841418503"],["/categories/前端/index.html","da9f3ab907975786bbc8acdc13ad4767"],["/categories/大数据开发/ElasticSearch/index.html","4989c412c94054370c0be5eea359fa87"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","92dfae356c75f8c1fd1d0b694fa91f5e"],["/categories/大数据开发/HBase/index.html","77b2f00e4bdc51826e1aabf59bbcbe43"],["/categories/大数据开发/HBase/学习笔记/index.html","ec1c8aeb7cb4926a1aac5b83fc17222b"],["/categories/大数据开发/HBase/环境搭建/index.html","7f375e02f50561a3f7ba77a3fe8db153"],["/categories/大数据开发/Hadoop/index.html","06b3d2e314e87388d9ca3586837dfafd"],["/categories/大数据开发/Hadoop/技术/index.html","913fcfa6a17234b0a8bcb2db0a24e8d6"],["/categories/大数据开发/Hadoop/环境搭建/index.html","ffd9a7c79af3ecab91d6269ebd33d04d"],["/categories/大数据开发/Redis/index.html","f9ceb4b6ff721ce44b17917294c66c5e"],["/categories/大数据开发/Redis/技术/index.html","a2b9342afede6afe02b105de1ec0af48"],["/categories/大数据开发/Redis/环境搭建/index.html","b8069097cdcc47f3de85ebfb93e0a85e"],["/categories/大数据开发/Spark/index.html","452bf1586d84a0d254905f507ddb8ece"],["/categories/大数据开发/Spark/环境搭建/index.html","54a8872bdf50785a59c80ae351ce2ca4"],["/categories/大数据开发/Zookeeper/index.html","662cadbddb1ac21f874513a4ed69b217"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","74b163a6ca5bf2686d4bcada723a2052"],["/categories/大数据开发/index.html","678d39fb1b722c903f68cbe7bf2333a3"],["/categories/学校课程/index.html","b90d2041c8431f9c179b20ed5cd57346"],["/categories/学校课程/计算机操作系统/index.html","006e4479572f491a374e1aacaff9df4b"],["/categories/操作系统/Linux/index.html","ad5929de9c4e0541f427dbc534263791"],["/categories/操作系统/Mac/index.html","5875fe3bd618e3ce96d9ada4be509bf6"],["/categories/操作系统/Windows/index.html","e2d524872be7361537ace1079ad7c79e"],["/categories/操作系统/index.html","79e6b3f8826b27bc82947e9a11b839d3"],["/categories/数学建模/index.html","719877addc1364536a5725121586bd61"],["/categories/数学建模/latex/index.html","a3bf68ab0d31cc8ef7544ee94bfabec6"],["/categories/数学建模/优化类/index.html","ed2afe09d50c2502d662c3a1943bf023"],["/categories/数学建模/优化类/现代优化算法/index.html","c9be539ead2fde9d731ab8995defa0b7"],["/categories/数学建模/优化类/规划类/index.html","5ae8e0e1b9e0f8d24e2aac74d4a1b065"],["/categories/数学建模/绘图/index.html","977445e5a4fe3c54aa934ffbab884652"],["/categories/数据库/MySQL/index.html","d96572f5f7021e5b35e307eddb41f5a1"],["/categories/数据库/index.html","f97b28e4f118a8489976c10d6523b78f"],["/categories/数据结构和算法/index.html","9d39f2caeec6ef81bc325f181e2ebe18"],["/categories/数据结构和算法/page/2/index.html","fa3a4c0d2f2dcb7e934f12a2e473066d"],["/categories/数据结构和算法/基本原理/bfs/index.html","b2b74528c8db2101c88586e0579c3df0"],["/categories/数据结构和算法/基本原理/dfs/index.html","68592d47812014006e7834108459e3e3"],["/categories/数据结构和算法/基本原理/index.html","f86eba61e65f547678b8c94e2441995c"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","4c298d34383aa828dfb6ed75a503be21"],["/categories/数据结构和算法/基本原理/动态规划/index.html","8433e3a0df330bb3e8200568a183c46b"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","230e851e6d823ff10f360cf1c86738bc"],["/categories/数据结构和算法/基本原理/图论/index.html","6cdb8674c2a1651989e8984253a9cf6e"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","700f766a4e777e28b0b1c5fd60606b87"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","5d68d2c2279d389c1a558b8e5cebb89c"],["/categories/数据结构和算法/基本原理/字符串/index.html","7358d093ec553e5203a9e960c4abc203"],["/categories/数据结构和算法/基本原理/排序/index.html","9b8b8bbb1b41746a1f6a29404bc23453"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","be77bd3209adbeb4066dff227a03c1a2"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","e35b489921506c4ca36f87697a601c85"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","196a1fdfea34aeeafada6063026c3c6e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6f4ce4030c30cd3113bfc5a2f54cc801"],["/categories/数据结构和算法/基本原理/链表/index.html","9a8813ccbade1cec3522407f079c7fef"],["/categories/数据结构和算法/算法题/index.html","1752e2e81833f3254f482b9765ebeb05"],["/categories/数据结构和算法/算法题/二分查找/index.html","b6db5164770f66a28376c75d2c61fd05"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","d298718fa07625d76b0b61b0458ef703"],["/categories/数据结构和算法/算法题/动态规划/index.html","05cd417b3fbc03520ee35e2986109620"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","c6c67417ce6a595f85defdc5af15b3d0"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","dbc39a3cd09adda877db9225ed37a08d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5257203a56ca111895d006c5cab9aa77"],["/categories/数据结构和算法/算法题/图论/index.html","02eaba665ea1fce612c8bf3ecd7c8cd0"],["/categories/数据结构和算法/算法题/图论/树论/index.html","f7f6b2ed0e6a088855bb110de88ef80b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","6799225d5c358226ead8fb0a0fce350e"],["/categories/数据结构和算法/算法题/数论/index.html","e69e7805102b8a9d390c4290d54f451b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f6a808f47d4a99e552114b45acf21093"],["/categories/杂七杂八/index.html","fdc951f599a7638d213cf531628c12be"],["/categories/杂七杂八/博客搭建/index.html","15ec06c5b3cc6f306720ea7c7233f916"],["/categories/编程工具下载/index.html","82f67e33339230487526f247a2909c27"],["/categories/编程环境/index.html","858652df73d001f8a572053453ddab1f"],["/categories/编程环境/大数据/index.html","a105320a4b4a98aca380e801f2fd72cf"],["/categories/英语学习/index.html","899cb03898e8bbd92c7d76552dd471bd"],["/categories/英语学习/英语语法/index.html","e2d2b3d6a09bca79831f47bb5213113f"],["/comments/index.html","8c44353866b6c2df18290db36c01040b"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","1ec56283cd942971b5aa566c78a9aa74"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9bc6c0076bbf7ea8f60883bdec38ac98"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","df1a5916a7899ce93df13ea5166be648"],["/movies/index.html","b20d84d8b529dc072b6cfe49cbeef9eb"],["/music/index.html","42f1c96f72eb91b92b19991be9678324"],["/page/2/index.html","6eaff0d30b2e4b15c48c8dfa29bf0024"],["/page/3/index.html","120c18c6521d518bcc8b9c0ddb262447"],["/page/4/index.html","e0222634c5732a7f3f99f6f85b5b3951"],["/page/5/index.html","49506f2eda1972607fb783ed389d037d"],["/page/6/index.html","38cc9954a964041ec4012ae6c5144ed0"],["/page/7/index.html","4ef8ca31d04364294d66c18ed52bdce3"],["/posts/1021360842.html","898166621b0150c5f84af119ee93701c"],["/posts/1120620192.html","7bf606fe055ae770508a9e5b1413f790"],["/posts/1137707673.html","a5149345ee7c409975fdc9c6a0017dcd"],["/posts/1141628095.html","0879e32907639d56e4ff9d0e7650ab6f"],["/posts/1168613674.html","ab4a57a4e6399663fbd2b6a9a745a1fd"],["/posts/1219920510.html","c99a98cd763b12d99e018f5b19562865"],["/posts/1222166338.html","00edbb5cd91116309b676d54c8709c32"],["/posts/1259097482.html","d9e9e6fbd2298d6b80ff56d968557496"],["/posts/1271036369.html","3b401e81b9efda2833d19f9154c803f5"],["/posts/1312847445.html","c2253542f6cc2ede41f33db1dac2f66f"],["/posts/135355774.html","70e5ea0690322d2f5ea48c5a5cd178c9"],["/posts/1375344716.html","ff7d1557baac1f9053951b3a325cfcdd"],["/posts/1388991698.html","782f5ee25965b54256c459c34d45914e"],["/posts/1410315814.html","650d94ddbc53be0d99a392a0989b4a04"],["/posts/1452790229.html","8b5a8ad586ddafe5c7e1a158e796b572"],["/posts/1470079884.html","ef3ef497fd2bbeeb0f515eed71c5f5dc"],["/posts/1470079885.html","5cace85e8c098a295aec9a2a83678c09"],["/posts/1470079886.html","c302c8ef192f9cf8d64c1a7e82e82cb4"],["/posts/1470079887.html","df54679d2f54d35adcde9bb62830a583"],["/posts/1498536549.html","70f77cee804deb81afa4831121db91a1"],["/posts/1539568593.html","a643c2f3fd6a93228b401b3597f0eca9"],["/posts/1547067935.html","fc9150650f15aa22bc8fe5c43f64a4b5"],["/posts/1557866301.html","ded6f71c9ddeb4e097cfccc4b08e3705"],["/posts/1571776361.html","3931a7155064f0a9f27828600a61af2a"],["/posts/1605124548.html","4aa26d0add5e28dfaa1a3e37ce8438ab"],["/posts/1633036852.html","8bc7024819aac2f638297ee0608a406a"],["/posts/1667740714.html","4eec944915ba6c48bd5529277435f7fe"],["/posts/1674202625.html","4ffd57061a018a5720b1c787331e1a20"],["/posts/1765123828.html","bb0db37d337043a9738e066fee2c0805"],["/posts/1767336200.html","ba70ef60c51aa78a07253abbaa4df6ab"],["/posts/1776114197.html","2869be502bc2ae4748d5c2506c05ba02"],["/posts/1817748743.html","b73ff9dc49608e46a179b7058587763d"],["/posts/1925125395.html","4d994e96b0471284e187f2a9e1f80b16"],["/posts/1966191251.html","80c563ad4f5007b7b6084ffd88985e3b"],["/posts/1987617322.html","6e787a1bdabd2629c70575314d7177eb"],["/posts/1999788039.html","52d19bc5b494aa80120a9493f9d0560f"],["/posts/2007534187.html","5978ce80c3b756855e10f75b166ff986"],["/posts/2075104059.html","6d394b91f4002de565c00e1f64f31bec"],["/posts/2087796737.html","650b6a8568f605de25a0047253bd5d21"],["/posts/2106547339.html","23eb50d1b074e5ba800d3e239a236a90"],["/posts/2207806286.html","45fda3d2e76f1c9d968133d4a72aa6d5"],["/posts/2225903441.html","f4a73fe588f9f977a265d2169175bd45"],["/posts/2265610284.html","4099c4a4da601d97e617cc5d4517d74c"],["/posts/2281352001.html","1ea9bd95324546abde342aba6bad1a4a"],["/posts/2364755265.html","4f96fd7a2c20411993029302c7762826"],["/posts/2414116852.html","36e8a3866c3fc05b75f1bd3232331d13"],["/posts/2421785022.html","69fa446448458f1d134b1baeedfe2b7b"],["/posts/2482902029.html","99948d10502b230cb25835c145cd7a61"],["/posts/2495386210.html","f09fe80ef516abcadd91522ef7df1e4e"],["/posts/2516528882.html","f163e29a69dfba9861396127ad56a615"],["/posts/2522177458.html","6b2acbe83f61b6246598f75aca47d0d4"],["/posts/2526659543.html","03e5a9fd806c234869c559d059830dca"],["/posts/2529807823.html","c66bf081d1dce60310f4b6e5214f0624"],["/posts/2592249117.html","d7276d2d50a62c9418b942e202511ec5"],["/posts/2596601004.html","939890133eb5e21aa7f40f1fe7ac29cb"],["/posts/2697614349.html","df8cdea1db3df8866a0c3bf21cdc8118"],["/posts/2742438348.html","63ab0d18089cec74a11a50df92a1d53c"],["/posts/2768249503.html","c08af20cbd84953a6119a6dfbbfeab88"],["/posts/2864584994.html","cac0c5f0ddae5146f8cd3397530b91f2"],["/posts/2888309600.html","17d291ed46c98aca83d28e0d90d2a12d"],["/posts/2891591958.html","60539f28f56413c1297b5c7495e658d7"],["/posts/2909934084.html","5945f7376dd57bcb4fe3f5755e1c848b"],["/posts/2920256992.html","2294c6187aa56ac0c585e3c2b03048f4"],["/posts/2959474469.html","bcbbab659800bd8428693b381dcdb172"],["/posts/3005926051.html","ce408cccaf500a73f0ee4f2690629cb2"],["/posts/309775400.html","18de6bc1549157d0230c1354a2d17020"],["/posts/3156194925.html","0df9fb9b8b9fd69a44dab4efefb25ca9"],["/posts/3169224211.html","4606760841cf90055e4d04a663f1a915"],["/posts/3183912587.html","8e974f46f66569e413b523873d387243"],["/posts/3213899550.html","a15c95748961cef0e9459cf5f7419922"],["/posts/3259212833.html","8e93acb0292d70a5ad6976c3ebc21108"],["/posts/3265658309.html","beb5070c0c45f658682a48ca0f01ce7a"],["/posts/3266130344.html","fe2536f6e987eaddf5e229e6ea34ca92"],["/posts/3292663995.html","10606abb2980262c70f4bc2eb9f735ac"],["/posts/3297135020.html","8018d0d624071916af9b4f48c9580af7"],["/posts/3306641566.html","181534659f5a6b5176f52eddfedd278f"],["/posts/3312011324.html","329fa3cb5c4e5c0073a142d5c534c146"],["/posts/336911618.html","c279e4c9e8152a64f65ca44dc980daf5"],["/posts/3402121571.html","e61cfc0060377c8c91d4121a54c233b8"],["/posts/3405577485.html","c3e1a1d57b8c443d198d18f37d652335"],["/posts/3498516849.html","e3c56c0a12c9679d904de78a5c129f98"],["/posts/350679531.html","206c9d5639dd5ecfcc245d2f7fc84f40"],["/posts/3513711414.html","08b78cf5605f213ea43fe37bd50afaf8"],["/posts/3523095624.html","502f6408c9f6832405bf18e484f30154"],["/posts/3546711884.html","74a2d260e2da4b5450bf1929bca6085f"],["/posts/362397694.html","51a5259a6c521a0c51c4b30d51e785a9"],["/posts/3731385230.html","f2aa653a3735f2c0d7afa4433cb280aa"],["/posts/3772089482.html","4cf1de3edabfc06b5267c1995e41404b"],["/posts/386609427.html","6f3e87e9886dc103f06437404d4d4b6e"],["/posts/4044235327.html","8e577008ccc5fcf6cb8584822f18f012"],["/posts/4098221856.html","1481b465cd75451bfc2d587f9befa5c5"],["/posts/4115971639.html","750f8da23141a07e573d0fe16ea507bd"],["/posts/4130790367.html","794e5a5edf858c2e3aa76820cfc31936"],["/posts/4131986683.html","5e4aa4844441b7723e94524ee03aea84"],["/posts/4177218757.html","972b5bfc5af45328dcb10573c6a483d4"],["/posts/4192183953.html","2a6d4086c93a4ad60d1660000a269e6b"],["/posts/4223662913.html","fe640035df0638c8b9c66d60d2682273"],["/posts/4261103898.html","12ffeb45acb94138ec1805f416daea2c"],["/posts/4286605504.html","9cfae4c7a7d1bc46ac6716e5600a3ec0"],["/posts/449089913.html","5fdfbabdd65f7e07a8cc600aad38685f"],["/posts/469711973.html","43fa1c06e615dad6506d36688a7c7a13"],["/posts/482495853.html","d66b25b47b00c9b391287ab058d7c9ff"],["/posts/488247922.html","0de049ea4335eaf3f14d3bb5d0643ddb"],["/posts/517302816.html","0f47d3f8be4ba8eab74b668b7bd7896e"],["/posts/570165348.html","b464398bdcb5d47e3540467a410561e5"],["/posts/595890772.html","9b69b0081baca1680d2a9aa19ed110c6"],["/posts/67485572.html","fca611e902aaf41bdfae3ba7fa1d90e6"],["/posts/694347442.html","77df9c38a6eaea7e144a5503798819a5"],["/posts/707384687.html","4d711dfd5198c3e9491a695ded39a4c3"],["/posts/71180092.html","c19bccf28906220ad5a009f5811f0d14"],["/posts/716459272.html","ed2f285114bffd23d96d1364c77983cc"],["/posts/765481613.html","b9e4c082399aa9e3ab4727b71df0af89"],["/posts/778231993.html","261942ba8d50d9f4e363a3a7bc16cb5c"],["/posts/795397410.html","9114475f2c2612551a6b127bfe54fe5c"],["/posts/820223701.html","40771c749007916483459ca3ad7a2bc2"],["/posts/830372185.html","09741fe3a791bf78e160fa9bba06ac12"],["/posts/88294277.html","401878c557ee93ac82db7843ab1759be"],["/posts/939963535.html","a26964e4b2ed0388c502b12793c1ddb7"],["/posts/983786067.html","440b8a86c67e71a385d4eaeceb056c7f"],["/sw-register.js","4fdc669549a8242a199882510eb9840c"],["/tags/C/index.html","7c03776e05d038d974545ba56297b820"],["/tags/C/page/2/index.html","345e3c7adb8c3e2102a660f65010fdc1"],["/tags/C/page/3/index.html","f497416365aceb2ba7363b4244a97ff4"],["/tags/C/page/4/index.html","3193d79dd28c3408737f07bec015c7c3"],["/tags/ETL/index.html","b998cce39bd68c42590228755f8e5020"],["/tags/ElasticSearch/index.html","d39ed976baed698654bae7c40be0e4fc"],["/tags/GUI/index.html","363800df6f6723892d7957de1a70f110"],["/tags/HBase/index.html","a965b1fd2442007e0740f689adff90f0"],["/tags/Hadoop/index.html","c6dc58dcdb3d9b0ce41058e59a0ac67a"],["/tags/Hadoop/page/2/index.html","783c7aeebe5ecd7f71141ee959606448"],["/tags/Java/index.html","c138434e1291e10e804d6d9bc13076a1"],["/tags/Java/page/2/index.html","7eff80824aba516a400d3efffc004542"],["/tags/Java后端/index.html","d1b98c89e89e8297cd8fbc4e926e71ef"],["/tags/Java后端/page/2/index.html","f80670016312c12b0a6668693721c335"],["/tags/Kettle/index.html","621338db6f6116e8e8a5161554daf32e"],["/tags/Kibana/index.html","c1dbff14f9b9f6260bd7fed0f9f6be9b"],["/tags/Linux/index.html","07ea47f5d8f2ca6a612a8df84b5d4e5f"],["/tags/Linux/page/2/index.html","a52c1d8c0b9ce4dfaeff5b6a39b589e2"],["/tags/Linux/page/3/index.html","24812db167c9f200460afe7e91d99611"],["/tags/Mac/index.html","c0600f9da9b843149971d5088312198e"],["/tags/Mac/page/2/index.html","4e63bb6d7f01449be20f72c2e038614a"],["/tags/Maven/index.html","801c0eca8b4c8ebc8b56add65d9462bb"],["/tags/MySQL/index.html","2af569f772527ce0d0cc65f470b912cd"],["/tags/Python/index.html","094db083b8c1ab05a6550ba78ac18008"],["/tags/Redis/index.html","f1e2e38e47ab0677f7f66fd1c4da7026"],["/tags/R语言/index.html","c4b4e4b0e0dee08e2f054a1ec6bdcc70"],["/tags/Spark/index.html","9e0f2cf3e31083775fbfd1916d16da6d"],["/tags/Ubuntu/index.html","0eddf149bd081d9b8cfc8c50a1f7359a"],["/tags/Vue/index.html","f15f3964473e594f0454d4c5b14bbdeb"],["/tags/Windows/index.html","d6cd104c0687c1d8a93bb40172543698"],["/tags/ZooKeeper/index.html","f26b10ce39aff8b7253ecf36abd13629"],["/tags/bfs/index.html","12f0cec468c339cec53628c06bb4464a"],["/tags/dfs/index.html","3b856f7370ed03df56bf9d0da58c274a"],["/tags/folium/index.html","5504533b14418a9e70a0d76593a1fce4"],["/tags/git/index.html","f54a25d062e19eaad14ae5fc1230ee81"],["/tags/iPad找电子书/index.html","e632f3278b491054ca47f450b714e082"],["/tags/index.html","5d85be3b9bf3ae7aa4c316c1673cf56d"],["/tags/latex/index.html","a61a789037e52cb89770a87d6285f924"],["/tags/中间件/index.html","085755648448685f943786bce8bc5ed8"],["/tags/二分查找/index.html","8a836f376dd4798841d4657b2c1287c3"],["/tags/优化类/index.html","50bfdbd2979449bdc873ffd2b7df08df"],["/tags/前端/index.html","cf05ba90a490fb509296c9e815538a13"],["/tags/前缀和与差分/index.html","df96ff0c90070b5c2aaa898015c44151"],["/tags/动态规划/index.html","66063d1f88e8a064b95b87922f41758a"],["/tags/动态规划/page/2/index.html","d7809a30d7a9fa4609e0248ff18c5354"],["/tags/博客搭建/index.html","6f1d5338683cfb2a31b1a1a80518fc70"],["/tags/图论/index.html","07acc8b219919d746493741a918f27ab"],["/tags/图论/page/2/index.html","9ef5aecdb7ab6865ecd533e859681b43"],["/tags/大数据/index.html","f87838905b9b54207af76ae679e59325"],["/tags/大数据/page/2/index.html","21164eb0c0b2edd97748249b1957d9ed"],["/tags/宽度优先搜索算法/index.html","48744c19bafe3edc0c33afdcaf360ab8"],["/tags/排序/index.html","b2c522912ac0585cefdce2285971990e"],["/tags/操作系统/index.html","e49fbc063c33a07159ce21b62618c0f3"],["/tags/数学建模/index.html","c1483a875957c74501564df6e5147a3e"],["/tags/数据库/index.html","82db2e39d77ec1c9143aaf71dc5de9d4"],["/tags/数据结构和算法/index.html","9dac3ecdffdb4a4f868a9ac5a339336f"],["/tags/数据结构和算法/page/2/index.html","9d41ded4249b547a25aaceb51639741b"],["/tags/数据结构和算法/page/3/index.html","e598e5b43382f4dc13bcc7ece7493685"],["/tags/数据结构和算法/page/4/index.html","a00c5620b4f6af55ca6e85ec791a82df"],["/tags/数据结构和算法/page/5/index.html","d1dfec6ab152d26b0f1bf2d6d0a88b05"],["/tags/数组和字符串/index.html","68e3f83f62413d7a51205ebcb5b65a45"],["/tags/数论/index.html","0723d90b2f5bf5f43dc24f753affdc85"],["/tags/枚举类/index.html","696b6078f25236aa87a2c3caaf60e4fa"],["/tags/栈和队列/index.html","5a42bf8e03282c19879b259cb74fab42"],["/tags/树论/index.html","4e28cba7188b2f7f9ff8c707145152b4"],["/tags/测试/index.html","009d72ad5d591462d4a5d539231157d2"],["/tags/深度优先搜索算法/index.html","265dc0f03acf7abf87f21bb72ab1ad6c"],["/tags/环境/index.html","0c521f75fe63477684334357d1a74ee8"],["/tags/环境变量/index.html","6914195acc22f7def5af30da7f158f64"],["/tags/绘图/index.html","999b14306c4e0d404937e690a95fac44"],["/tags/编程工具/index.html","c4e3cb1988f212b4200c02fecf9bc62e"],["/tags/编程环境/index.html","f83a9ee373286a91791cac584df1b04c"],["/tags/网络编程/index.html","abeecde3acf4a876e8a54bf201b0e75d"],["/tags/英语语法/index.html","6aa7e59188b19a500331ab6783a0ab22"],["/tags/计算机操作系统/index.html","208b052145165cc43a3afc67bfa5daed"],["/tags/论文/index.html","03275edaf57cb849dad1cf3f62d40776"],["/tags/资源下载/index.html","42ab743348915ed5499c6d003d974795"],["/tags/链表/index.html","81bb08781f03bc64be5a47fd4d764ab8"],["/tags/集合/index.html","368e1dcf4132d8f88e2ef8ba40d4f009"],["/tags/集群/index.html","acf5012b3c770afdecbbc88ef233e145"]];
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
