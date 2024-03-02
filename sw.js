/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","dd8d76ce9c01a237071599b2ee5f63c4"],["/about/index.html","931dfccaea981f435593fe9093f64465"],["/archives/2023/01/index.html","8704ca90d2ad662dee76ba990dbf6ec6"],["/archives/2023/02/index.html","24431b764f6255f993cae12f7aaa5532"],["/archives/2023/02/page/2/index.html","f8ec3d4a86802acd8e75b349aca5cc8c"],["/archives/2023/02/page/3/index.html","404137b9497649ff84f92c485619a97d"],["/archives/2023/03/index.html","d275ece575e17a07b1103886fca8ade3"],["/archives/2023/05/index.html","59783dee71cb4566307a20a15d8c59f7"],["/archives/2023/06/index.html","39ff3e8014434a915082f36316e1377e"],["/archives/2023/09/index.html","137375f82f9ce03c66d42bf47419e709"],["/archives/2023/11/index.html","834aa2579101a8a5b2baa89be2416354"],["/archives/2023/12/index.html","9a475977f92ee1d61cd4c61d507d1325"],["/archives/2023/index.html","2dcbf614e12f3e4c7e283b81d8ea0035"],["/archives/2023/page/2/index.html","1657400e92f6a29db4c0c1ab67fc400b"],["/archives/2023/page/3/index.html","81234994e28f58472a710fe52e933584"],["/archives/2023/page/4/index.html","db89ac62b32d3b074855915821b32139"],["/archives/2023/page/5/index.html","bc9c0b85ec33d07edcd45195a7ee85b0"],["/archives/2024/02/index.html","658dd96a2e29946069962bd1da197f66"],["/archives/2024/index.html","60f55b907532e70e0fd2da5877f13117"],["/archives/index.html","46ee0705c62dedf14d80e9ce73beb668"],["/archives/page/2/index.html","794c4b443fb8c1f468f73d1de2cdb5ae"],["/archives/page/3/index.html","385d0f94a24a9a828b76c8864ef02eed"],["/archives/page/4/index.html","c676e8760dd5e68d0627ee0507fbfffa"],["/archives/page/5/index.html","bf6142421c2c536ff03187ff5ef84d12"],["/baidu_verify_codeva-qQP2iZOMLX.html","880b9f97a9f3491863ff3591c67be76f"],["/categories/Java/index.html","8a43b24683883c67f18933ece143571d"],["/categories/Java/后端/index.html","1354349c98e843bcbe350e5382a2a1c4"],["/categories/Java/基础/index.html","dfd2969864a58e3f05fa9dc33efdd532"],["/categories/Java/基础/集合/index.html","7d2c81b51325a7d71790c133952d820b"],["/categories/Python/index.html","5e305a2b8276447b96fcfed1678fae6d"],["/categories/Python/编程环境/index.html","cce7ddcbce393aa7d81c6064e745f26f"],["/categories/R语言/index.html","cbb6f24fc64f2c7db42bc56f1b20c614"],["/categories/R语言/编程环境/index.html","32c0831b40e72d6108cb00110e2072ca"],["/categories/iPad/index.html","2518905c21e43a6eecfee7bbcb2263df"],["/categories/index.html","30b145fb837a9e72ad81163ed6bb8f27"],["/categories/中间件/index.html","4fd843c7c0d1c512523296d638f98a20"],["/categories/前端/Vue/index.html","6830684e745afebca3ee1011e62bf82d"],["/categories/前端/index.html","8e35a92dc1260af12947d9766e423723"],["/categories/大数据开发/ElasticSearch/index.html","dd15ab220b678d34fb6b40bb30d5d40c"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","029368cf19a2243e3c80d4ca622e8c1f"],["/categories/大数据开发/HBase/index.html","88aeacc9e69e0fad750357e3da3cf21d"],["/categories/大数据开发/HBase/学习笔记/index.html","332d4c215d2df5f45a74345f07ec986d"],["/categories/大数据开发/HBase/环境搭建/index.html","913c6530c467735636480983dc046529"],["/categories/大数据开发/Hadoop/index.html","42d91d0fb91fafb79fc87f1acd000ed0"],["/categories/大数据开发/Hadoop/技术/index.html","fbc93d3854d48322022f75789e74a4ee"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b7fa9826f95eba97bb718bd09b93e74f"],["/categories/大数据开发/Redis/index.html","e9162a440901f9adb2dbefb287549616"],["/categories/大数据开发/Redis/技术/index.html","253238b7a87adb612a011b56d23ab801"],["/categories/大数据开发/Redis/环境搭建/index.html","02e1e8b63206aee84b2cd8f72abb9f38"],["/categories/大数据开发/Spark/index.html","3ecd013e8385a9a1361724fa66d6996d"],["/categories/大数据开发/Spark/环境搭建/index.html","2736a13843e063548f3c1148d8b4ca3b"],["/categories/大数据开发/Zookeeper/index.html","f6240d6d80117c2e06f2d2105e4ed76e"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9cf1232d0eee3b4e5f7530212842273b"],["/categories/大数据开发/index.html","5cace4592b487f849c35e5262145f6bc"],["/categories/学校课程/index.html","8c7454138b4a91016a5a3dcaaefd007f"],["/categories/学校课程/计算机操作系统/index.html","59968a6c5a98a4391e802b239983ae68"],["/categories/操作系统/Linux/index.html","37cfebef337df9dd767bc70b0a2fb8f2"],["/categories/操作系统/Mac/index.html","e4755b943a056faf7e5173d25c135dc8"],["/categories/操作系统/Windows/index.html","01dda85694872f07b2b38cadeb0ffaf7"],["/categories/操作系统/index.html","e9b77625a4495e800993c9b1099e8d46"],["/categories/数学建模/index.html","2c92a5d663629c55b0a9da24eeb8444e"],["/categories/数学建模/latex/index.html","69a7bfffd57d3315c3a960dc2e7b87cc"],["/categories/数学建模/优化类/index.html","22a5428f63178e991712fa9f6ab0bca6"],["/categories/数学建模/优化类/现代优化算法/index.html","edbde18d089b6842e1873ded5964991a"],["/categories/数学建模/优化类/规划类/index.html","101520262b416112c67cb1af384505cc"],["/categories/数学建模/绘图/index.html","65af84ae2921ba116a5233474c1be7bf"],["/categories/数据库/MySQL/index.html","19cf45b5b050b14bce2fee7a5a4158a2"],["/categories/数据库/index.html","71a7c0eb8db56c14c83e3d8f0c7a76f1"],["/categories/数据结构和算法/index.html","ec6b8aaed158cfd679e6fd8fe4e52d28"],["/categories/数据结构和算法/page/2/index.html","6f2409a5bb288218c8d485eb049557f6"],["/categories/数据结构和算法/基本原理/bfs/index.html","94f518ecfec0b4ebb41d5473956b3270"],["/categories/数据结构和算法/基本原理/dfs/index.html","51cc1af053b95aba976a9acf7fc94c34"],["/categories/数据结构和算法/基本原理/index.html","611635602f41d1bfe907dd7fe7f6ac40"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","c95c875f281a2e853f05c6027ba7bc6e"],["/categories/数据结构和算法/基本原理/动态规划/index.html","bec95532548f78ddb33ec569e2dcefc2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d4633be82ebd14fe095923cc5c9b663a"],["/categories/数据结构和算法/基本原理/图论/index.html","0c8988c7c685a1a95f47fec540ae602f"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","79d435dbb2daa7f41c246c362dada9f5"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","2d1ec60ff7b256b7e2378b76ceefba09"],["/categories/数据结构和算法/基本原理/字符串/index.html","641b59bb48daa8337b1b717c7e14c9ac"],["/categories/数据结构和算法/基本原理/排序/index.html","760df1d1dd7fc0a24187117991955574"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","2c63b0ab27658b5de396a88de7b253a8"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","28d2849d20c259e2f1a0568a832503ad"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","e6ba6df928048073ad96ca64486700b5"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","292dd8a1e3e141379bc6d414659a0624"],["/categories/数据结构和算法/基本原理/链表/index.html","26f69e16e3719229c8772bc61ee4ee76"],["/categories/数据结构和算法/算法题/index.html","997e1164c280caa422e20e6309596f03"],["/categories/数据结构和算法/算法题/二分查找/index.html","73c36747642e67a5c3e3b33881c63e73"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","153dbe113455fcd03dfe4e08ceabb8f1"],["/categories/数据结构和算法/算法题/动态规划/index.html","e9f6f3942e9ed977abc7e27fc524f322"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","275ddbd1c2ec4d533af56d5cb7856dbb"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","092085d8621ffbb4d7358cfd71626657"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","ae64711d617a6568e901f4e1a1d586e1"],["/categories/数据结构和算法/算法题/图论/index.html","5ec9de69ce904849068aed8f1ab69452"],["/categories/数据结构和算法/算法题/图论/树论/index.html","dfedc11650b8b1fa6a6d415608c0f86c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","f8342b55d396b81025041c405ab1b87a"],["/categories/数据结构和算法/算法题/数论/index.html","40f6dd185281be9622ecb1562d69e15b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","57f8990eb19caf1c564cdc25bb1e0201"],["/categories/杂七杂八/index.html","1e4af7e9e49866b0f20cb1531687861b"],["/categories/杂七杂八/博客搭建/index.html","cae693dc761f98f78ff5ac376f6040ae"],["/categories/编程工具下载/index.html","50eb66b26775ede8e71b8575808c6f2b"],["/categories/编程环境/index.html","bcc43332e6afd6ccc5e8a1c2811d2937"],["/categories/编程环境/大数据/index.html","4b3bd33d066e7ebf2155099277177cfc"],["/categories/英语学习/index.html","cfffe60d524ed5fbdf2fbfab0c343af5"],["/categories/英语学习/英语语法/index.html","eeb55b4ee72e88be3f72df2835766629"],["/comments/index.html","c12a2f212ee2688a2cdba723229c8d55"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a3d4f27e0cc086e1a9b41dd1f18f71b9"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","808e10f51fe0ad05d76388bbadb9f9b1"],["/movies/index.html","8c656f3f6d4d81b1703ec7b9a900b5f4"],["/music/index.html","1fd23621bfd201502176cb0784c0e2b1"],["/page/2/index.html","a7ecbaeb714542b67e3baf31189f29cf"],["/page/3/index.html","09fc574658d37280b02aa677e515e00e"],["/page/4/index.html","6e0db120f248e2468588c1f67761faec"],["/page/5/index.html","cc68d4cccea1f86d23ceac9131d0ffda"],["/page/6/index.html","66d41ddf465cafe83cd07fae56e81cb6"],["/page/7/index.html","a68bae9c7fef8ce7af00c30a03613c26"],["/posts/1021360842.html","898166621b0150c5f84af119ee93701c"],["/posts/1120620192.html","7bf606fe055ae770508a9e5b1413f790"],["/posts/1137707673.html","a5149345ee7c409975fdc9c6a0017dcd"],["/posts/1141628095.html","0879e32907639d56e4ff9d0e7650ab6f"],["/posts/1168613674.html","ab4a57a4e6399663fbd2b6a9a745a1fd"],["/posts/1219920510.html","c99a98cd763b12d99e018f5b19562865"],["/posts/1222166338.html","00edbb5cd91116309b676d54c8709c32"],["/posts/1259097482.html","d9e9e6fbd2298d6b80ff56d968557496"],["/posts/1271036369.html","3b401e81b9efda2833d19f9154c803f5"],["/posts/1312847445.html","c2253542f6cc2ede41f33db1dac2f66f"],["/posts/135355774.html","70e5ea0690322d2f5ea48c5a5cd178c9"],["/posts/1375344716.html","ff7d1557baac1f9053951b3a325cfcdd"],["/posts/1388991698.html","782f5ee25965b54256c459c34d45914e"],["/posts/1410315814.html","650d94ddbc53be0d99a392a0989b4a04"],["/posts/1452790229.html","8b5a8ad586ddafe5c7e1a158e796b572"],["/posts/1470079884.html","ef3ef497fd2bbeeb0f515eed71c5f5dc"],["/posts/1470079885.html","5cace85e8c098a295aec9a2a83678c09"],["/posts/1470079886.html","c302c8ef192f9cf8d64c1a7e82e82cb4"],["/posts/1470079887.html","df54679d2f54d35adcde9bb62830a583"],["/posts/1498536549.html","70f77cee804deb81afa4831121db91a1"],["/posts/1539568593.html","a643c2f3fd6a93228b401b3597f0eca9"],["/posts/1547067935.html","fc9150650f15aa22bc8fe5c43f64a4b5"],["/posts/1557866301.html","ded6f71c9ddeb4e097cfccc4b08e3705"],["/posts/1571776361.html","3931a7155064f0a9f27828600a61af2a"],["/posts/1605124548.html","4aa26d0add5e28dfaa1a3e37ce8438ab"],["/posts/1633036852.html","8bc7024819aac2f638297ee0608a406a"],["/posts/1667740714.html","4eec944915ba6c48bd5529277435f7fe"],["/posts/1674202625.html","4ffd57061a018a5720b1c787331e1a20"],["/posts/1765123828.html","bb0db37d337043a9738e066fee2c0805"],["/posts/1767336200.html","ba70ef60c51aa78a07253abbaa4df6ab"],["/posts/1776114197.html","2869be502bc2ae4748d5c2506c05ba02"],["/posts/1817748743.html","b73ff9dc49608e46a179b7058587763d"],["/posts/1925125395.html","4d994e96b0471284e187f2a9e1f80b16"],["/posts/1966191251.html","80c563ad4f5007b7b6084ffd88985e3b"],["/posts/1987617322.html","6e787a1bdabd2629c70575314d7177eb"],["/posts/1999788039.html","52d19bc5b494aa80120a9493f9d0560f"],["/posts/2007534187.html","5978ce80c3b756855e10f75b166ff986"],["/posts/2075104059.html","6d394b91f4002de565c00e1f64f31bec"],["/posts/2087796737.html","650b6a8568f605de25a0047253bd5d21"],["/posts/2106547339.html","23eb50d1b074e5ba800d3e239a236a90"],["/posts/2207806286.html","45fda3d2e76f1c9d968133d4a72aa6d5"],["/posts/2225903441.html","f4a73fe588f9f977a265d2169175bd45"],["/posts/2265610284.html","4099c4a4da601d97e617cc5d4517d74c"],["/posts/2281352001.html","1ea9bd95324546abde342aba6bad1a4a"],["/posts/2364755265.html","4f96fd7a2c20411993029302c7762826"],["/posts/2414116852.html","36e8a3866c3fc05b75f1bd3232331d13"],["/posts/2421785022.html","69fa446448458f1d134b1baeedfe2b7b"],["/posts/2482902029.html","99948d10502b230cb25835c145cd7a61"],["/posts/2495386210.html","f09fe80ef516abcadd91522ef7df1e4e"],["/posts/2516528882.html","f163e29a69dfba9861396127ad56a615"],["/posts/2522177458.html","6b2acbe83f61b6246598f75aca47d0d4"],["/posts/2526659543.html","03e5a9fd806c234869c559d059830dca"],["/posts/2529807823.html","c66bf081d1dce60310f4b6e5214f0624"],["/posts/2592249117.html","d7276d2d50a62c9418b942e202511ec5"],["/posts/2596601004.html","939890133eb5e21aa7f40f1fe7ac29cb"],["/posts/2697614349.html","df8cdea1db3df8866a0c3bf21cdc8118"],["/posts/2742438348.html","63ab0d18089cec74a11a50df92a1d53c"],["/posts/2768249503.html","c08af20cbd84953a6119a6dfbbfeab88"],["/posts/2864584994.html","cac0c5f0ddae5146f8cd3397530b91f2"],["/posts/2888309600.html","17d291ed46c98aca83d28e0d90d2a12d"],["/posts/2891591958.html","60539f28f56413c1297b5c7495e658d7"],["/posts/2909934084.html","5945f7376dd57bcb4fe3f5755e1c848b"],["/posts/2920256992.html","2294c6187aa56ac0c585e3c2b03048f4"],["/posts/2959474469.html","bcbbab659800bd8428693b381dcdb172"],["/posts/3005926051.html","ce408cccaf500a73f0ee4f2690629cb2"],["/posts/309775400.html","18de6bc1549157d0230c1354a2d17020"],["/posts/3156194925.html","0df9fb9b8b9fd69a44dab4efefb25ca9"],["/posts/3169224211.html","4606760841cf90055e4d04a663f1a915"],["/posts/3183912587.html","8e974f46f66569e413b523873d387243"],["/posts/3213899550.html","a15c95748961cef0e9459cf5f7419922"],["/posts/3259212833.html","8e93acb0292d70a5ad6976c3ebc21108"],["/posts/3265658309.html","beb5070c0c45f658682a48ca0f01ce7a"],["/posts/3266130344.html","fe2536f6e987eaddf5e229e6ea34ca92"],["/posts/3292663995.html","10606abb2980262c70f4bc2eb9f735ac"],["/posts/3297135020.html","8018d0d624071916af9b4f48c9580af7"],["/posts/3306641566.html","181534659f5a6b5176f52eddfedd278f"],["/posts/3312011324.html","329fa3cb5c4e5c0073a142d5c534c146"],["/posts/336911618.html","c279e4c9e8152a64f65ca44dc980daf5"],["/posts/3402121571.html","e61cfc0060377c8c91d4121a54c233b8"],["/posts/3405577485.html","c3e1a1d57b8c443d198d18f37d652335"],["/posts/3498516849.html","e3c56c0a12c9679d904de78a5c129f98"],["/posts/350679531.html","206c9d5639dd5ecfcc245d2f7fc84f40"],["/posts/3513711414.html","08b78cf5605f213ea43fe37bd50afaf8"],["/posts/3523095624.html","502f6408c9f6832405bf18e484f30154"],["/posts/3546711884.html","74a2d260e2da4b5450bf1929bca6085f"],["/posts/362397694.html","51a5259a6c521a0c51c4b30d51e785a9"],["/posts/3731385230.html","f2aa653a3735f2c0d7afa4433cb280aa"],["/posts/3772089482.html","4cf1de3edabfc06b5267c1995e41404b"],["/posts/386609427.html","6f3e87e9886dc103f06437404d4d4b6e"],["/posts/4044235327.html","8e577008ccc5fcf6cb8584822f18f012"],["/posts/4098221856.html","1481b465cd75451bfc2d587f9befa5c5"],["/posts/4115971639.html","750f8da23141a07e573d0fe16ea507bd"],["/posts/4130790367.html","794e5a5edf858c2e3aa76820cfc31936"],["/posts/4131986683.html","5e4aa4844441b7723e94524ee03aea84"],["/posts/4177218757.html","972b5bfc5af45328dcb10573c6a483d4"],["/posts/4192183953.html","2a6d4086c93a4ad60d1660000a269e6b"],["/posts/4223662913.html","fe640035df0638c8b9c66d60d2682273"],["/posts/4261103898.html","12ffeb45acb94138ec1805f416daea2c"],["/posts/4286605504.html","9cfae4c7a7d1bc46ac6716e5600a3ec0"],["/posts/449089913.html","5fdfbabdd65f7e07a8cc600aad38685f"],["/posts/469711973.html","43fa1c06e615dad6506d36688a7c7a13"],["/posts/482495853.html","d66b25b47b00c9b391287ab058d7c9ff"],["/posts/488247922.html","0de049ea4335eaf3f14d3bb5d0643ddb"],["/posts/517302816.html","0f47d3f8be4ba8eab74b668b7bd7896e"],["/posts/570165348.html","b464398bdcb5d47e3540467a410561e5"],["/posts/595890772.html","9b69b0081baca1680d2a9aa19ed110c6"],["/posts/67485572.html","fca611e902aaf41bdfae3ba7fa1d90e6"],["/posts/694347442.html","77df9c38a6eaea7e144a5503798819a5"],["/posts/707384687.html","4d711dfd5198c3e9491a695ded39a4c3"],["/posts/71180092.html","c19bccf28906220ad5a009f5811f0d14"],["/posts/716459272.html","ed2f285114bffd23d96d1364c77983cc"],["/posts/765481613.html","b9e4c082399aa9e3ab4727b71df0af89"],["/posts/778231993.html","261942ba8d50d9f4e363a3a7bc16cb5c"],["/posts/795397410.html","9114475f2c2612551a6b127bfe54fe5c"],["/posts/820223701.html","40771c749007916483459ca3ad7a2bc2"],["/posts/830372185.html","09741fe3a791bf78e160fa9bba06ac12"],["/posts/88294277.html","401878c557ee93ac82db7843ab1759be"],["/posts/939963535.html","a26964e4b2ed0388c502b12793c1ddb7"],["/posts/983786067.html","440b8a86c67e71a385d4eaeceb056c7f"],["/sw-register.js","c791578952b7c6a5893b802780c8ead4"],["/tags/C/index.html","37cb9aa1935fd19b6e87a9aab89bd518"],["/tags/C/page/2/index.html","6916ba82c117494b1d8de9d35f3fa042"],["/tags/C/page/3/index.html","aae72bda00506d015113698bcd570cff"],["/tags/C/page/4/index.html","cf9151c8380228547dbff2a97da73b89"],["/tags/ETL/index.html","964a5f81765f10e1e1d1507b4233abbf"],["/tags/ElasticSearch/index.html","6585aede3ce7715688c2fc356dd88c16"],["/tags/GUI/index.html","807400c5546f8c01cfa45b66f865fe07"],["/tags/HBase/index.html","9ef49c93d2b444f65f7ca94b76e80b04"],["/tags/Hadoop/index.html","fd00357338181f38812e2fb59afb91b5"],["/tags/Hadoop/page/2/index.html","f445bae0bf2c620055c1fbc30b2aac07"],["/tags/Java/index.html","9b1c6f83d4986637707161d5c9944521"],["/tags/Java/page/2/index.html","5e67554dd803ac88980da007640dc228"],["/tags/Java后端/index.html","8e102e9a3387c3bb3e05500f258d8edd"],["/tags/Java后端/page/2/index.html","13a7e816d6d1df2c5ef0bfc941654b27"],["/tags/Kettle/index.html","f76b71b9a7f2ed42aeca808a5e2ed292"],["/tags/Kibana/index.html","a6c89485ed17c789932e73c8af0514b3"],["/tags/Linux/index.html","cc0872dd5bec46cf6da25ab8a51e3b53"],["/tags/Linux/page/2/index.html","fd89610a8aa38d04d387efc11f11aa73"],["/tags/Linux/page/3/index.html","f319c275813bc3bb28af84db5d68e540"],["/tags/Mac/index.html","8f47a9ea4a8a17398b4d0eda07ac1df0"],["/tags/Mac/page/2/index.html","17fd7bf70bb9de0396296618647e0e71"],["/tags/Maven/index.html","697ca53a418fcea3a7106c7cb035572a"],["/tags/MySQL/index.html","16272442aa6aee6eb46b498ec838a639"],["/tags/Python/index.html","4eae985a1294a4d941d894027b43eee1"],["/tags/Redis/index.html","7923e5e9e72ff072019fcef9b64c4547"],["/tags/R语言/index.html","244676a99282d4c71987b3abc43f6ad4"],["/tags/Spark/index.html","02eeabc1262f8392eacb8642e5b14dcd"],["/tags/Ubuntu/index.html","f8b4a626d828cb0d78a008d1ac502c1e"],["/tags/Vue/index.html","1677b7fa8ccdbfbe3b210e803c2b9039"],["/tags/Windows/index.html","b85cd938951ef6b44abcfecb78bcd3b5"],["/tags/ZooKeeper/index.html","49a64813a9b9a7084af8577f5dde7862"],["/tags/bfs/index.html","44b8489dfb45e0a35962da28ff9e521e"],["/tags/dfs/index.html","0bf664d246ad0d6cc4057e99ac33c3ff"],["/tags/folium/index.html","ced73c150ee4c869d0cf63501c179b87"],["/tags/git/index.html","380a71e7336a1b95e0c617ecf1975096"],["/tags/iPad找电子书/index.html","cbf8e2743d9c3101bd3b5ec0561ebe05"],["/tags/index.html","b3252f5e283911038ba9b01ed324d4a3"],["/tags/latex/index.html","59928452e856e8a9c73d68bcdf60627b"],["/tags/中间件/index.html","8d7dfece85f208fbae0339f45c3a8f31"],["/tags/二分查找/index.html","185607b7a9998f3acc746348000f1945"],["/tags/优化类/index.html","af9b9e636bf40c9d5b86bc3f80e7386e"],["/tags/前端/index.html","b7303c8b0afbd07331000fee531f15ae"],["/tags/前缀和与差分/index.html","878e556fa2d24d66c4d1b8398d2f257d"],["/tags/动态规划/index.html","b8c1109b8d48ec7531542e39c82c6713"],["/tags/动态规划/page/2/index.html","7a339014cce6b9384c2b069a7dc44a38"],["/tags/博客搭建/index.html","775b909a35caf6e13d16f8c44faeb756"],["/tags/图论/index.html","426ae65f3f764ba71063157d39b53b84"],["/tags/图论/page/2/index.html","fbc0a3bc955085369bace6d739fe02c1"],["/tags/大数据/index.html","fe8d161f2c22411780e9ff8fc55a9e9d"],["/tags/大数据/page/2/index.html","e794d69f71dd09e3aeff49218d3fb9b0"],["/tags/宽度优先搜索算法/index.html","c477121afe3f6600fe8764799b906ad1"],["/tags/排序/index.html","4b482214f69c4737b866eef05bf55e4d"],["/tags/操作系统/index.html","86ffb265ed8b5cb1812ff80034e47838"],["/tags/数学建模/index.html","90a6198942c30dde367fdc730ba9c5e1"],["/tags/数据库/index.html","c547b43be41433d3e95edb07730756df"],["/tags/数据结构和算法/index.html","4fba80d01e4b04d79724df1bea17750f"],["/tags/数据结构和算法/page/2/index.html","5eb65d0f10513794062f7ab4a6308af4"],["/tags/数据结构和算法/page/3/index.html","5479fc409fa4c4ec2c169c5de75cdff9"],["/tags/数据结构和算法/page/4/index.html","14c775fb8a3a70a568a72819f89d902a"],["/tags/数据结构和算法/page/5/index.html","53c326869245413cbcd437f518bb1655"],["/tags/数组和字符串/index.html","6df63abe92591bf21b0954154998b1a2"],["/tags/数论/index.html","77da469dce18d61a1fcc0340e766ad5b"],["/tags/枚举类/index.html","b9e3daf6baa699fcfaa997a5ad5ced2b"],["/tags/栈和队列/index.html","0fcf4ca73eb1d7e994005f927779a7f4"],["/tags/树论/index.html","ece275117ae8ac6f6dd6fc191a1fac85"],["/tags/测试/index.html","7f951f3ec8cc4f799c25bd3cd7608b41"],["/tags/深度优先搜索算法/index.html","14976b5fe9fc7f0e9a29ea3d60fc7795"],["/tags/环境/index.html","72a740abf3701375952f7e8d349b4721"],["/tags/环境变量/index.html","403d1c627c9d132633390a7c49ffd16c"],["/tags/绘图/index.html","e327187c92dc8b1ba1a45fe15ea9742a"],["/tags/编程工具/index.html","d3696cfd8910074766807aeb9725d086"],["/tags/编程环境/index.html","7948b05cf7c0ca2c1177fa9ea397b952"],["/tags/网络编程/index.html","e9f41fdeb18519746765f4cb39d3edd0"],["/tags/英语语法/index.html","7097dfecc5d1a2a45dc3e29c7eb85249"],["/tags/计算机操作系统/index.html","1a36f6e8391a953374b18c3fdd8b6714"],["/tags/论文/index.html","6009994fcf31d1fd5e7fd1f58f907c8c"],["/tags/资源下载/index.html","3190174a0a96a7d26d571d3c17cc18d2"],["/tags/链表/index.html","75afd9dc108906430e6e4d61c1b5fa57"],["/tags/集合/index.html","a750e8bb24d318eb568e168fbc827c9d"],["/tags/集群/index.html","ffb9b18324562157ffd0a86e904f7ed3"]];
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
