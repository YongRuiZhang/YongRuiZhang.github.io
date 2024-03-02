/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","07037c1ae3c118291e384270891952f9"],["/about/index.html","931dfccaea981f435593fe9093f64465"],["/archives/2023/01/index.html","21932fc62dfb5684d8fb10bdf04dee8d"],["/archives/2023/02/index.html","57f9b29393943fd3d0198185b040b5df"],["/archives/2023/02/page/2/index.html","7303399e438e10ebd32e1c1a70ba9bdf"],["/archives/2023/02/page/3/index.html","a4c820e8207a5b3042765896f1bee0f2"],["/archives/2023/03/index.html","eb80ad1a03b1cb1eb9f87219649ac03a"],["/archives/2023/05/index.html","d20b54c26dcd8591181f229fd1c09048"],["/archives/2023/06/index.html","37a06acd58b5957415a64af7d1435f1a"],["/archives/2023/09/index.html","43130504bdeb83f921ac7758f05ccde5"],["/archives/2023/11/index.html","23dd1a4051ecf7ac9630332a20bbbba3"],["/archives/2023/12/index.html","8a07166e49b1ebbcd422866772881647"],["/archives/2023/index.html","66291873b6cfa91ac03ca6f6f1f2b49e"],["/archives/2023/page/2/index.html","d30ec29e118b73a13e9928061eaf4c7e"],["/archives/2023/page/3/index.html","f581765aef7dde281fa54a728433583a"],["/archives/2023/page/4/index.html","b16013400aad442496c633e90904f009"],["/archives/2023/page/5/index.html","932ff04c50085b38a97451cd649a50e3"],["/archives/2024/02/index.html","f79785d67ec0c4608def58a660c4e79f"],["/archives/2024/index.html","9bcd400aee36d39f785f4a812b5f9db0"],["/archives/index.html","3be15dcc774afe0bdc82619631be57f0"],["/archives/page/2/index.html","966052c43d8f5d03ddd8533273b5dcf6"],["/archives/page/3/index.html","25f81a67dac0a71e1a4d4faaac12c5fb"],["/archives/page/4/index.html","cf055d6498f8458b3c079f339c796a3d"],["/archives/page/5/index.html","9752a04f7c428f092bcd5e9193ea6c70"],["/baidu_verify_codeva-qQP2iZOMLX.html","89534077e422cb914e096835aad43330"],["/categories/Java/index.html","95edf0ec9808b89856ee1e3c193b7f9d"],["/categories/Java/后端/index.html","988ee5ff8d5c6c0d57c06dba06118c6f"],["/categories/Java/基础/index.html","fc161b60742ada283b583460bc3bfd83"],["/categories/Java/基础/集合/index.html","2fa13c91d5073056640ebf62e7dc1c5d"],["/categories/Python/index.html","00a694cc28ee569f0cf3d64a82e08b14"],["/categories/Python/编程环境/index.html","5bbf6942cdd544b53849282168e10d7e"],["/categories/R语言/index.html","5c6d67974501368e67771001c8864852"],["/categories/R语言/编程环境/index.html","74d2ef8d69653ab177029deaf631fcde"],["/categories/iPad/index.html","e9b0beb7c07c010dcacd0d6750e3e0a7"],["/categories/index.html","30b145fb837a9e72ad81163ed6bb8f27"],["/categories/中间件/index.html","a8f8066ed4aa06baeed086d40cc99677"],["/categories/前端/Vue/index.html","d43c17e92baefb66478748fd439d6701"],["/categories/前端/index.html","e686e6980de1e30d7c92bee5e99f6b81"],["/categories/大数据开发/ElasticSearch/index.html","3c1e60e82fefffda63c4051533e1df34"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","79f5e51f09434b7bb655bbcf0b14d875"],["/categories/大数据开发/HBase/index.html","3b7df25448a2211a5b00a3db18c23415"],["/categories/大数据开发/HBase/学习笔记/index.html","4aff164b3943785c73c79098645909b3"],["/categories/大数据开发/HBase/环境搭建/index.html","2c667bf9414d7a0b2ae035446924aceb"],["/categories/大数据开发/Hadoop/index.html","df1ed673435b2aca9bd417d4fd25bcb9"],["/categories/大数据开发/Hadoop/技术/index.html","f69e48c39ca0b5e07d22b782a6f7e6b4"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f04705f9d95d21d3822bfa4aeec7f571"],["/categories/大数据开发/Redis/index.html","55bd12531ba3a83dca8485c20cdfe7dd"],["/categories/大数据开发/Redis/技术/index.html","048652f3f4ac3c41487f8bf7ecafeab7"],["/categories/大数据开发/Redis/环境搭建/index.html","1b8afd86daa2f6ab97740554e0dbaeaf"],["/categories/大数据开发/Spark/index.html","13bd7447f42d91a3ec80e9cf396f2704"],["/categories/大数据开发/Spark/环境搭建/index.html","a3764cacacccddb0761c9fa820c41c04"],["/categories/大数据开发/Zookeeper/index.html","703a8e16a083214831cc71ee4a1a139d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","be1b2cac4cb28d5f25f6975746d479c8"],["/categories/大数据开发/index.html","5539f2eded2575eba97b09773a80a097"],["/categories/学校课程/index.html","2e7b89d0b1e76474e1f68c2a9ed44b64"],["/categories/学校课程/计算机操作系统/index.html","44ac65b8bf31d854d423e7a881eab8ad"],["/categories/操作系统/Linux/index.html","d58a12b14b88607debeb5553c18af76c"],["/categories/操作系统/Mac/index.html","c9eba59994bbb5cd6ab1cff3b4f255be"],["/categories/操作系统/Windows/index.html","959b9a879370a5c8ce0a567fa12da351"],["/categories/操作系统/index.html","625631b65d32cc9788001c1a1a12ecbd"],["/categories/数学建模/index.html","012b846c11cc57291e926605513dd81a"],["/categories/数学建模/latex/index.html","bd76656156e550ee687429f812cc1e5e"],["/categories/数学建模/优化类/index.html","8b301389c201a8eb36b3dcae52029d88"],["/categories/数学建模/优化类/现代优化算法/index.html","aead61801978b50fd87c566543abcdf5"],["/categories/数学建模/优化类/规划类/index.html","3cb3e99c9ec90f9df42a52f700ef0a2c"],["/categories/数学建模/绘图/index.html","aba8b7d1c692ccd35bd51cac8acf8ae8"],["/categories/数据库/MySQL/index.html","f84ca6d8c2e9317b15bc5f3e2493ec9a"],["/categories/数据库/index.html","3f407e266e044ba272db94c94ccb47c8"],["/categories/数据结构和算法/index.html","166bc404ae95f29b23e88c17eeec85fa"],["/categories/数据结构和算法/page/2/index.html","2dffc8a63209e65bab43c92866a7b919"],["/categories/数据结构和算法/基本原理/bfs/index.html","aaab2cf90e03ad611528bcb6fae130fd"],["/categories/数据结构和算法/基本原理/dfs/index.html","1113fce7c1128bf96ae38462991f86c5"],["/categories/数据结构和算法/基本原理/index.html","f94b5025fc7aaf9a76d322336815e90e"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","a2b48fa949a11323f57ff3c7b4e48dec"],["/categories/数据结构和算法/基本原理/动态规划/index.html","d6ebaa7ca87dd69056a9c221814684b6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a16912503f0a1428d9ba8bbc34c388bb"],["/categories/数据结构和算法/基本原理/图论/index.html","0daa232ec48942c90699fe7d2f7af5bc"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","e3f2eb9427899a89adfb5593a1307169"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","9e19735bcb49dd786d02d5d01be3ce86"],["/categories/数据结构和算法/基本原理/字符串/index.html","37c3ea889a13f696690dae7dd236f2e7"],["/categories/数据结构和算法/基本原理/排序/index.html","74f4ebd99fe8d196feebaa945636f08c"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","466d8b2abd130b85611906f98b03d979"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","05901581ece6ac5f9d41c98b5a8e62b6"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","1ecfc2a103dd3735898e038b3bdea96d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6b904910db15354b6a8c8b42331766e4"],["/categories/数据结构和算法/基本原理/链表/index.html","596a7410785fbe95a84161f0c2e785b1"],["/categories/数据结构和算法/算法题/index.html","6d0a183fb5af27db8610e85f554b0402"],["/categories/数据结构和算法/算法题/二分查找/index.html","b995967966571fc97dcb666f8b8c12b8"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c7bb4bff4621a5b5068018aaaf79917d"],["/categories/数据结构和算法/算法题/动态规划/index.html","fd3647578b629548e343523f65fddec3"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","7baa6786cd94ffa9fba31c9490785d01"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","78fe036647088c707175369f2e0f461f"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","cfd43bfdaead3b877d25fa59b99f7a33"],["/categories/数据结构和算法/算法题/图论/index.html","e1cd44c907f415932c2293c02f0e2dfc"],["/categories/数据结构和算法/算法题/图论/树论/index.html","93dff68aefb928fdd161e58e6d8eae6e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","59def5783b1e5a7d168fc1a7ec71a6d5"],["/categories/数据结构和算法/算法题/数论/index.html","1248edc3dceef34b46a3ad0d3818dce2"],["/categories/数据结构和算法/算法题/栈和队列/index.html","304ea56a78f218b618d4612677131303"],["/categories/杂七杂八/index.html","fbc24c075e5665ee72acf36615aa1634"],["/categories/杂七杂八/博客搭建/index.html","ae469ea343dba523ca33f5d637d69a3f"],["/categories/编程工具下载/index.html","a25c0997dd5136b69cc2e4104b64b0ac"],["/categories/编程环境/index.html","c39d89e11167f5ced1dce8ee0538065a"],["/categories/编程环境/大数据/index.html","728d455da2e79f2dfeb7f8583ac91684"],["/categories/英语学习/index.html","7c4700b34a351079ccbc5cb2aeaba313"],["/categories/英语学习/英语语法/index.html","3e8b5a04a5afd8a817b47f030df95fdf"],["/comments/index.html","81896b50e4b13f038b798ec5cffe0f71"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","3c2159a3ef0a2f6df72d5529b3e9732b"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e0861b5ae134ca1880065683b5704c63"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","130e93c063328a15b48a512caa3a2780"],["/movies/index.html","b8de901753d7f4f8fd9f6ed4fd413d2d"],["/music/index.html","90adac7f00040d797d98fc6b5c79add3"],["/page/2/index.html","ea57d1787f3fbdf750a02bc56646678f"],["/page/3/index.html","33abc5c9e819314f03dea945e18b07a8"],["/page/4/index.html","ed911ed9cbe890cdbecc8cb0482beb98"],["/page/5/index.html","e8ca8c808f7022e82f14523ae28c17d3"],["/page/6/index.html","6f01b0786ce49ebd458e039c6aef03bb"],["/page/7/index.html","b7daa6afb9228adc20cb00e16e93bfff"],["/posts/1021360842.html","898166621b0150c5f84af119ee93701c"],["/posts/1120620192.html","7bf606fe055ae770508a9e5b1413f790"],["/posts/1137707673.html","a5149345ee7c409975fdc9c6a0017dcd"],["/posts/1141628095.html","0879e32907639d56e4ff9d0e7650ab6f"],["/posts/1168613674.html","ab4a57a4e6399663fbd2b6a9a745a1fd"],["/posts/1219920510.html","c99a98cd763b12d99e018f5b19562865"],["/posts/1222166338.html","00edbb5cd91116309b676d54c8709c32"],["/posts/1259097482.html","d9e9e6fbd2298d6b80ff56d968557496"],["/posts/1271036369.html","3b401e81b9efda2833d19f9154c803f5"],["/posts/1312847445.html","c2253542f6cc2ede41f33db1dac2f66f"],["/posts/135355774.html","70e5ea0690322d2f5ea48c5a5cd178c9"],["/posts/1375344716.html","ff7d1557baac1f9053951b3a325cfcdd"],["/posts/1388991698.html","782f5ee25965b54256c459c34d45914e"],["/posts/1410315814.html","650d94ddbc53be0d99a392a0989b4a04"],["/posts/1452790229.html","8b5a8ad586ddafe5c7e1a158e796b572"],["/posts/1470079884.html","ef3ef497fd2bbeeb0f515eed71c5f5dc"],["/posts/1470079885.html","5cace85e8c098a295aec9a2a83678c09"],["/posts/1470079886.html","c302c8ef192f9cf8d64c1a7e82e82cb4"],["/posts/1470079887.html","df54679d2f54d35adcde9bb62830a583"],["/posts/1498536549.html","70f77cee804deb81afa4831121db91a1"],["/posts/1539568593.html","a643c2f3fd6a93228b401b3597f0eca9"],["/posts/1547067935.html","fc9150650f15aa22bc8fe5c43f64a4b5"],["/posts/1557866301.html","ded6f71c9ddeb4e097cfccc4b08e3705"],["/posts/1571776361.html","3931a7155064f0a9f27828600a61af2a"],["/posts/1605124548.html","4aa26d0add5e28dfaa1a3e37ce8438ab"],["/posts/1633036852.html","8bc7024819aac2f638297ee0608a406a"],["/posts/1667740714.html","4eec944915ba6c48bd5529277435f7fe"],["/posts/1674202625.html","4ffd57061a018a5720b1c787331e1a20"],["/posts/1765123828.html","bb0db37d337043a9738e066fee2c0805"],["/posts/1767336200.html","ba70ef60c51aa78a07253abbaa4df6ab"],["/posts/1776114197.html","2869be502bc2ae4748d5c2506c05ba02"],["/posts/1817748743.html","b73ff9dc49608e46a179b7058587763d"],["/posts/1925125395.html","4d994e96b0471284e187f2a9e1f80b16"],["/posts/1966191251.html","80c563ad4f5007b7b6084ffd88985e3b"],["/posts/1987617322.html","6e787a1bdabd2629c70575314d7177eb"],["/posts/1999788039.html","52d19bc5b494aa80120a9493f9d0560f"],["/posts/2007534187.html","5978ce80c3b756855e10f75b166ff986"],["/posts/2075104059.html","6d394b91f4002de565c00e1f64f31bec"],["/posts/2087796737.html","650b6a8568f605de25a0047253bd5d21"],["/posts/2106547339.html","23eb50d1b074e5ba800d3e239a236a90"],["/posts/2207806286.html","45fda3d2e76f1c9d968133d4a72aa6d5"],["/posts/2225903441.html","f4a73fe588f9f977a265d2169175bd45"],["/posts/2265610284.html","4099c4a4da601d97e617cc5d4517d74c"],["/posts/2281352001.html","1ea9bd95324546abde342aba6bad1a4a"],["/posts/2364755265.html","4f96fd7a2c20411993029302c7762826"],["/posts/2414116852.html","36e8a3866c3fc05b75f1bd3232331d13"],["/posts/2421785022.html","69fa446448458f1d134b1baeedfe2b7b"],["/posts/2482902029.html","99948d10502b230cb25835c145cd7a61"],["/posts/2495386210.html","f09fe80ef516abcadd91522ef7df1e4e"],["/posts/2516528882.html","f163e29a69dfba9861396127ad56a615"],["/posts/2522177458.html","6b2acbe83f61b6246598f75aca47d0d4"],["/posts/2526659543.html","03e5a9fd806c234869c559d059830dca"],["/posts/2529807823.html","c66bf081d1dce60310f4b6e5214f0624"],["/posts/2592249117.html","d7276d2d50a62c9418b942e202511ec5"],["/posts/2596601004.html","939890133eb5e21aa7f40f1fe7ac29cb"],["/posts/2697614349.html","df8cdea1db3df8866a0c3bf21cdc8118"],["/posts/2742438348.html","63ab0d18089cec74a11a50df92a1d53c"],["/posts/2768249503.html","c08af20cbd84953a6119a6dfbbfeab88"],["/posts/2864584994.html","cac0c5f0ddae5146f8cd3397530b91f2"],["/posts/2888309600.html","17d291ed46c98aca83d28e0d90d2a12d"],["/posts/2891591958.html","60539f28f56413c1297b5c7495e658d7"],["/posts/2909934084.html","5945f7376dd57bcb4fe3f5755e1c848b"],["/posts/2920256992.html","2294c6187aa56ac0c585e3c2b03048f4"],["/posts/2959474469.html","bcbbab659800bd8428693b381dcdb172"],["/posts/3005926051.html","ce408cccaf500a73f0ee4f2690629cb2"],["/posts/309775400.html","18de6bc1549157d0230c1354a2d17020"],["/posts/3156194925.html","0df9fb9b8b9fd69a44dab4efefb25ca9"],["/posts/3169224211.html","4606760841cf90055e4d04a663f1a915"],["/posts/3183912587.html","8e974f46f66569e413b523873d387243"],["/posts/3213899550.html","a15c95748961cef0e9459cf5f7419922"],["/posts/3259212833.html","8e93acb0292d70a5ad6976c3ebc21108"],["/posts/3265658309.html","beb5070c0c45f658682a48ca0f01ce7a"],["/posts/3266130344.html","fe2536f6e987eaddf5e229e6ea34ca92"],["/posts/3292663995.html","10606abb2980262c70f4bc2eb9f735ac"],["/posts/3297135020.html","8018d0d624071916af9b4f48c9580af7"],["/posts/3306641566.html","181534659f5a6b5176f52eddfedd278f"],["/posts/3312011324.html","329fa3cb5c4e5c0073a142d5c534c146"],["/posts/336911618.html","c279e4c9e8152a64f65ca44dc980daf5"],["/posts/3402121571.html","e61cfc0060377c8c91d4121a54c233b8"],["/posts/3405577485.html","c3e1a1d57b8c443d198d18f37d652335"],["/posts/3498516849.html","e3c56c0a12c9679d904de78a5c129f98"],["/posts/350679531.html","206c9d5639dd5ecfcc245d2f7fc84f40"],["/posts/3513711414.html","08b78cf5605f213ea43fe37bd50afaf8"],["/posts/3523095624.html","502f6408c9f6832405bf18e484f30154"],["/posts/3546711884.html","74a2d260e2da4b5450bf1929bca6085f"],["/posts/362397694.html","51a5259a6c521a0c51c4b30d51e785a9"],["/posts/3731385230.html","f2aa653a3735f2c0d7afa4433cb280aa"],["/posts/3772089482.html","4cf1de3edabfc06b5267c1995e41404b"],["/posts/386609427.html","6f3e87e9886dc103f06437404d4d4b6e"],["/posts/4044235327.html","8e577008ccc5fcf6cb8584822f18f012"],["/posts/4098221856.html","1481b465cd75451bfc2d587f9befa5c5"],["/posts/4115971639.html","750f8da23141a07e573d0fe16ea507bd"],["/posts/4130790367.html","794e5a5edf858c2e3aa76820cfc31936"],["/posts/4131986683.html","5e4aa4844441b7723e94524ee03aea84"],["/posts/4177218757.html","972b5bfc5af45328dcb10573c6a483d4"],["/posts/4192183953.html","2a6d4086c93a4ad60d1660000a269e6b"],["/posts/4223662913.html","fe640035df0638c8b9c66d60d2682273"],["/posts/4261103898.html","12ffeb45acb94138ec1805f416daea2c"],["/posts/4286605504.html","9cfae4c7a7d1bc46ac6716e5600a3ec0"],["/posts/449089913.html","5fdfbabdd65f7e07a8cc600aad38685f"],["/posts/469711973.html","43fa1c06e615dad6506d36688a7c7a13"],["/posts/482495853.html","d66b25b47b00c9b391287ab058d7c9ff"],["/posts/488247922.html","0de049ea4335eaf3f14d3bb5d0643ddb"],["/posts/517302816.html","0f47d3f8be4ba8eab74b668b7bd7896e"],["/posts/570165348.html","b464398bdcb5d47e3540467a410561e5"],["/posts/595890772.html","9b69b0081baca1680d2a9aa19ed110c6"],["/posts/67485572.html","fca611e902aaf41bdfae3ba7fa1d90e6"],["/posts/694347442.html","77df9c38a6eaea7e144a5503798819a5"],["/posts/707384687.html","4d711dfd5198c3e9491a695ded39a4c3"],["/posts/71180092.html","c19bccf28906220ad5a009f5811f0d14"],["/posts/716459272.html","ed2f285114bffd23d96d1364c77983cc"],["/posts/765481613.html","b9e4c082399aa9e3ab4727b71df0af89"],["/posts/778231993.html","261942ba8d50d9f4e363a3a7bc16cb5c"],["/posts/795397410.html","9114475f2c2612551a6b127bfe54fe5c"],["/posts/820223701.html","40771c749007916483459ca3ad7a2bc2"],["/posts/830372185.html","09741fe3a791bf78e160fa9bba06ac12"],["/posts/88294277.html","401878c557ee93ac82db7843ab1759be"],["/posts/939963535.html","a26964e4b2ed0388c502b12793c1ddb7"],["/posts/983786067.html","440b8a86c67e71a385d4eaeceb056c7f"],["/sw-register.js","859b015f670a5fe12e9c7f5327f0d47e"],["/tags/C/index.html","f5ea90f6ff5fff4cdcaffd0c005a51a9"],["/tags/C/page/2/index.html","21406afe974eb71c05481c8a58a34f2e"],["/tags/C/page/3/index.html","4ffd4204c4190c97c495220d650d1dd3"],["/tags/C/page/4/index.html","ec80ac7af781ba38c8bd884f33469aff"],["/tags/ETL/index.html","7c672ef0e5f1e5b9d64e6bb1bbc956da"],["/tags/ElasticSearch/index.html","f508f9e2a8b37ca5ffbb855b223e7066"],["/tags/GUI/index.html","c93e2fbdec72cdf8b2b64818b6c2edbf"],["/tags/HBase/index.html","86b8abd59611b26012f0761e1d83ddb1"],["/tags/Hadoop/index.html","57d2e6008bcc01c1df93523b5d6bf12a"],["/tags/Hadoop/page/2/index.html","3fad3dddd822c523876e9a0637b06991"],["/tags/Java/index.html","564a141c1cf3bb62438eed321500ac70"],["/tags/Java/page/2/index.html","19fab2da8e3d68f4d766e51ea20afdaa"],["/tags/Java后端/index.html","9d396e1b32f9ac7119eed7d29ed184a1"],["/tags/Java后端/page/2/index.html","b982f0b4331085ae45d925f51c2e5ca8"],["/tags/Kettle/index.html","daca42b94d4067b6e5b7ae5c3af34ab1"],["/tags/Kibana/index.html","429db1b71ee68250e7feb81ae50c596a"],["/tags/Linux/index.html","b1af3c8ba2bdc47da66e386bb5dc1e44"],["/tags/Linux/page/2/index.html","049b347a163674069f5c90a26c07a2ca"],["/tags/Linux/page/3/index.html","52805cfb400f9e85d606ff28e35b7712"],["/tags/Mac/index.html","64341bb3c31f5d61b7cd45cf266f3f09"],["/tags/Mac/page/2/index.html","1ba8eed16f57b50668b0ded464ad717d"],["/tags/Maven/index.html","c71b1741b5dd80b0606aaf40a26e01a4"],["/tags/MySQL/index.html","41469aa044dd4e18e71ad8e5fdb88ad5"],["/tags/Python/index.html","cbd8c876a2a693f728ff61d2e65bfec3"],["/tags/Redis/index.html","63b8b47fe517f7fb654314daa7d21067"],["/tags/R语言/index.html","0975f4ba42c363f306f22aab9540c112"],["/tags/Spark/index.html","9c8c3755b99b7147814c9e9e6f7f1563"],["/tags/Ubuntu/index.html","906fa232b4f6f310e770c94f5526df6d"],["/tags/Vue/index.html","51a7ca9955804a2ac3f66a5052417b82"],["/tags/Windows/index.html","e773281b0eab9671561984c69ffb07e4"],["/tags/ZooKeeper/index.html","22d25faef0c53dc67d3699b279d3cdf3"],["/tags/bfs/index.html","d3c451fd47817a66c693e944acc1a502"],["/tags/dfs/index.html","24bce4096beb04c8b08c2a051fcc2f86"],["/tags/folium/index.html","7a634984854eb2d1aad3da0ddc82fa77"],["/tags/git/index.html","ec51ecd0b10f5ae09bf7e535ef0eb2f2"],["/tags/iPad找电子书/index.html","f51a7953d31f028608669f5ba0e2deaa"],["/tags/index.html","f2ff5b03d3a56b59f03b883390ef2c10"],["/tags/latex/index.html","c6b386e91e7a0ea8a0cac45b99dc7523"],["/tags/中间件/index.html","d3aa778c1df71b8b145ce6b5f590ff75"],["/tags/二分查找/index.html","1a672cc0eb58d0bc8070f013522f6c06"],["/tags/优化类/index.html","f89f77d437a9099437fe5309c99e9bd6"],["/tags/前端/index.html","bc41cacbb75a2e5b9f6c0e683f13874a"],["/tags/前缀和与差分/index.html","85fb17ea5422262c483cf98fd8a38e50"],["/tags/动态规划/index.html","d326836451f7621af482d4597b07cc0c"],["/tags/动态规划/page/2/index.html","58e67fdee4b9529bac5ce2ac345d4482"],["/tags/博客搭建/index.html","4caddb8106418ec079d28a5aca3e7358"],["/tags/图论/index.html","9bd59f2f194c9e3e1eddb7ef9a4b693a"],["/tags/图论/page/2/index.html","6054fb20c86bf29d202d70e797d5b1a7"],["/tags/大数据/index.html","66fcf2f7e2379b56aac07d4a0cc9a8a6"],["/tags/大数据/page/2/index.html","00d1c21306ebf3df8a3f8e10167ab56c"],["/tags/宽度优先搜索算法/index.html","79a1fbbec5cc1b3118a61b4afa9a4fc6"],["/tags/排序/index.html","f844c0dc4cd9120f2886f147850576cd"],["/tags/操作系统/index.html","59e9dcf6de9cff4826bf16ccda103277"],["/tags/数学建模/index.html","a752a8c56f713c8a5b9201fa30855e6d"],["/tags/数据库/index.html","edef324e78971bdeea40c868a595c31f"],["/tags/数据结构和算法/index.html","114e1d9db9bf32ef6fa75ed0999f4a47"],["/tags/数据结构和算法/page/2/index.html","2d8ddb923fb6bd19c7024b33f8a38aa6"],["/tags/数据结构和算法/page/3/index.html","401db62311da3ca80d37f68b2f113d04"],["/tags/数据结构和算法/page/4/index.html","270c4da94fcb20a838a498573c0ab26b"],["/tags/数据结构和算法/page/5/index.html","bb965e39d0b65a2de4cd158efa247bab"],["/tags/数组和字符串/index.html","1e321f05dc7ddd81ab6957e7e8aadfe7"],["/tags/数论/index.html","141a72e59184e2a5ef497db4d3aba287"],["/tags/枚举类/index.html","ae48a94881b87d450d65a454d49893a9"],["/tags/栈和队列/index.html","efb7a355e4d512adc444627d212010b2"],["/tags/树论/index.html","6c9ca58b9b349d8297d575246b567f04"],["/tags/测试/index.html","68010477b54b57ff83e4600124c95a28"],["/tags/深度优先搜索算法/index.html","8814ff0529c6efb1e07e54d6f86caf5c"],["/tags/环境/index.html","8ec39506b8665813796ee9ca14fe9e0e"],["/tags/环境变量/index.html","b25660f875de26f11383a956bf2bf2f2"],["/tags/绘图/index.html","6870ba7056d8bbd1342c004cce100512"],["/tags/编程工具/index.html","4be4b9c755a017e7e057565bcd166e7b"],["/tags/编程环境/index.html","8a345292155bfef2faa76578772192dc"],["/tags/网络编程/index.html","278ff93669ac93067a937272f14f4d39"],["/tags/英语语法/index.html","a459d0fdbaac3aa600fe3ffada8d0842"],["/tags/计算机操作系统/index.html","8bc576d8a77095223160c788393b6e78"],["/tags/论文/index.html","6f03c1d20a3f7aa76445ccc8e4e2f396"],["/tags/资源下载/index.html","79b6d016ad8acbf0d6f7d363ff25aa5a"],["/tags/链表/index.html","9efc0eea3d45b7036e85208a5c07b98b"],["/tags/集合/index.html","0f76dc45e01f676c01cfe20d3bf91dc2"],["/tags/集群/index.html","3b9eddb351b2e7918aa0ffd3829ebcef"]];
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
