/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ac0103fbe8fd3d097181e1f1903ac961"],["/about/index.html","3667123b197a7f3b0e96838e68cacf95"],["/archives/2023/01/index.html","ea3cd5dcc6b61c403d110f1e9ca7c155"],["/archives/2023/02/index.html","536aaee5dd9ab17b40cad2d38012907e"],["/archives/2023/02/page/2/index.html","7980deeb985788eeb0a78c6826721f4f"],["/archives/2023/03/index.html","348ed77e42425309f6bb367f2c06fc80"],["/archives/2023/05/index.html","545009a9f83b6383075fd49ecd70346d"],["/archives/2023/index.html","327918f8e1e36b6452ace946212d3373"],["/archives/2023/page/2/index.html","387a539c62af716efc5361fac60a7b36"],["/archives/2023/page/3/index.html","8e1fb4c0832a800314e4a57525267b06"],["/archives/2023/page/4/index.html","d828a39768577bb6a4e091ba896a538c"],["/archives/index.html","fccefcbb2879f2d732ff82fc7ee06b91"],["/archives/page/2/index.html","530b0cd1ede59be6490f51f358250f0c"],["/archives/page/3/index.html","c6838cd7926c2897ef413945437ec7b0"],["/archives/page/4/index.html","91b55c47e116fafd4690f1d39d697588"],["/categories/Java/index.html","ac1a678d1f759b40a34c0b7148cc27ce"],["/categories/Java/后端/index.html","b2b1f87e37a964a0ae989fff7a7d2c77"],["/categories/Java/基础/index.html","191f92b5a699662bb548b6fd9cdc57b8"],["/categories/Java/基础/集合/index.html","4e7d57d93c3b8db7858d94a424c30620"],["/categories/Python/index.html","da94945ff49c4887531e28217a44a337"],["/categories/Python/编程环境/index.html","17b9aef141b50dc1883912da7d0f8a87"],["/categories/R语言/index.html","bf86cc873f0b25a9800b55d3b32e12a1"],["/categories/R语言/编程环境/index.html","afe56501452c3e228002f63851319290"],["/categories/index.html","ed5c7763843c7d2875e45aa829da6fa9"],["/categories/中间件/index.html","80b60dda269964350e8adacae3652a71"],["/categories/大数据开发/ElasticSearch/index.html","cba975a4d13b47dfdb1148cbad3c63e5"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","55bfdc60ae05f85e0c19dd2ab0583bab"],["/categories/大数据开发/HBase/index.html","9ba1a6a42958ecee3fb5a6defcc77ad5"],["/categories/大数据开发/HBase/学习笔记/index.html","a360e68d9746bd9d4aada5b65ae779e1"],["/categories/大数据开发/HBase/环境搭建/index.html","2f50e14d688237c5dd566c18f15ae47b"],["/categories/大数据开发/Hadoop/index.html","653db6bb0faf9be540a5d25e300389ec"],["/categories/大数据开发/Hadoop/技术/index.html","a65cc85e66518180351795e85bb7308c"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b0e156e1f443b7690a24f3e1a9db843e"],["/categories/大数据开发/Redis/index.html","9c51787357d989f2b2c893f82e267f2b"],["/categories/大数据开发/Redis/技术/index.html","7572db896a74e77836372dda769b400d"],["/categories/大数据开发/Redis/环境搭建/index.html","e448beae232a2fffa8dd94241044632c"],["/categories/大数据开发/Zookeeper/index.html","1c11c7ec1fbacee31e563e2f7439618f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","d340727c4a6a48fa64d60e50322c20dc"],["/categories/大数据开发/index.html","bc5a01e4577b23a8da41d75d6bbb2abd"],["/categories/操作系统/Linux/index.html","88c9950315e9bc2bebc69eb9a44b67e9"],["/categories/操作系统/Mac/index.html","3c0bd46638cbd1881a58cece26297f87"],["/categories/操作系统/Windows/index.html","b90b713445fe434b2853278ba719bc3f"],["/categories/操作系统/index.html","a4c34bb64e1a931b74a113ca60dca614"],["/categories/数学建模/index.html","ba16714b13ed0a68af4a40e6a3dbfe53"],["/categories/数学建模/latex/index.html","6bcd3d8f42dbca3db3bd20ef053a8337"],["/categories/数学建模/优化类/index.html","8a34399f46a6727d23ca9bd34d4a20d1"],["/categories/数学建模/优化类/现代优化算法/index.html","20c8f4425906049186995be8a260535f"],["/categories/数学建模/优化类/规划类/index.html","86b8878ac9b38f8e07a58db4615cfacf"],["/categories/数学建模/绘图/index.html","88edbc7579d1c7af5389074a181c930e"],["/categories/数据库/MySQL/index.html","a312ad46b88cbf4905bd41bc802c51ff"],["/categories/数据库/index.html","f518dc585e6a6dfdbefef8d51443f527"],["/categories/数据结构和算法/index.html","753e8b009ede73fc7e83014cce48a50c"],["/categories/数据结构和算法/page/2/index.html","3d25f68a288198482947dac0745dea1e"],["/categories/数据结构和算法/基本原理/bfs/index.html","db3e164521ec73cdf4b99fcf9e00aa74"],["/categories/数据结构和算法/基本原理/dfs/index.html","d46bb818fbc2262dc25a16db42c003a9"],["/categories/数据结构和算法/基本原理/index.html","1b5a4eb55adf4fc9724ed14e313f4e29"],["/categories/数据结构和算法/基本原理/动态规划/index.html","229dc26ee4d9e2a846b8757dcfc0e045"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","fdb8a78e29221d13121f5cf481fbbb3f"],["/categories/数据结构和算法/基本原理/图论/index.html","1b9fc83d7cbd28bf54758275f23ab0e8"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","f398e64319fbff510ac86c19d9d53e1d"],["/categories/数据结构和算法/基本原理/数论/index.html","580031e54b8dcefd132c97fdfb3bf8e9"],["/categories/数据结构和算法/基本原理/树论/index.html","6cf09ba23dfe4d1e43972253b09be8e7"],["/categories/数据结构和算法/基本原理/链表/index.html","3ce8e177ffa8c68923162a3e97163ae4"],["/categories/数据结构和算法/算法题/index.html","84915dd01e62e37e6726d540c86005ac"],["/categories/数据结构和算法/算法题/二分查找/index.html","caaaa59b44036249ce03be717f416392"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","df21fb807e7a118282c4f4e94ea266cc"],["/categories/数据结构和算法/算法题/动态规划/index.html","0e4ed321acfd284cebd53b199d31739b"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","193bab448c54a3bf4f412a42c8e5fbe3"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b412a16879ab6913e934aabb0f669d8f"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d0b7d459cb6f497575f926ecf2391a20"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ff342de9808ca860ab23140aad8cf789"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c41791aeb8a4ae63fb15561a56be205c"],["/categories/数据结构和算法/算法题/树论/index.html","ea68def4b0d28d1e717538ffade8e78d"],["/categories/杂七杂八/index.html","2b2b3713cb6639aa706bf4d31e0d769f"],["/categories/杂七杂八/博客搭建/index.html","64fa3cf4dcfcc1fd214a5e8d3ad0f039"],["/categories/编程环境/index.html","e8a44c78dea24518d1ad98460c276967"],["/categories/英语学习/index.html","42d2893ee752b64252dce83dfc986b4a"],["/categories/英语学习/英语语法/index.html","7a07213ec3580d68b72587d72bbc0e38"],["/comments/index.html","f86cd600e3314aa395ca7cecde3e45a3"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","f5838772da83bab5617004e735978c80"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cd152c846926dd0ab86ceb82f9101d91"],["/movies/index.html","54e13e6a91235433920a0c80ece7e48e"],["/music/index.html","b5025db3bad3d2b708bce2d2b531df9d"],["/page/2/index.html","a6e38cdc9374e2e72d89e35e6ec034ab"],["/page/3/index.html","cae7e923eda52eb7927ac995989b0d17"],["/page/4/index.html","526cc51e8a49762a117d1e748f0a86d7"],["/page/5/index.html","cb8408bb2f5e1150d600ba6bf04edf97"],["/page/6/index.html","55dd7486f27880a8e407e7159e9be7f8"],["/posts/1021360842.html","f5d0f2490f22ef0a0bf31b3e15afd1b5"],["/posts/1120620192.html","62d5d7718524d710618ff7ad0082d143"],["/posts/1141628095.html","89a7456352daf74cd49ab53533f1acc9"],["/posts/1168613674.html","3be7c8f562cad62116440e4930cb1e64"],["/posts/1219920510.html","42ec7c5cd0163c53f52a63f1a7851681"],["/posts/1222166338.html","37d4b5cf79aff4064f800d11454c74dc"],["/posts/1259097482.html","df161c71c6c63aa62bf6feadbb71c5ea"],["/posts/1271036369.html","aae7cfb60f36d2e4d790668b840ec2ab"],["/posts/1312847445.html","ab81252d1bbbad4e3e40980b5edbf535"],["/posts/135355774.html","747e9b3a215bdf0790056cf1e08e2734"],["/posts/1375344716.html","fc4234e906ad5823902869dfba368bad"],["/posts/1388991698.html","50e7ace6950bb816b248e0ead2d66029"],["/posts/1410315814.html","933cb70015de038b19572226c234e7f0"],["/posts/1452790229.html","2826a8b3e44346c95b1b3cbe1da423c8"],["/posts/1470079884.html","97f2606f271a0f22ca3909f26f457a98"],["/posts/1470079885.html","5fb6964fb878bf4090a2d0cbf5bc6c53"],["/posts/1470079886.html","162fc4fe1c03bc0079e868c28c3adad9"],["/posts/1470079887.html","4afd24ed14622f196943c8f217e18fc6"],["/posts/1498536549.html","feb613b9aa4eb925ef44c189e208ce2a"],["/posts/1547067935.html","e21f0e1c3495dc398d692ee84df514e5"],["/posts/1557866301.html","4265667cf5a86caba66c109d74aebe14"],["/posts/1571776361.html","1e7d8487f8e95cc56620b6ef0d02c7d7"],["/posts/1605124548.html","40f3bb1c4c2d6005cf96593f79ac1463"],["/posts/1633036852.html","4a23436390a72a2988ee87413e8ac9c1"],["/posts/1765123828.html","a4c8d1026e269742659df79c69f4ee14"],["/posts/1767336200.html","c3aecdf021c64a26b616881e4c83ec6f"],["/posts/1776114197.html","4a21d2143e6585a06099ca17034f0274"],["/posts/1817748743.html","c2849d5b63f9bd9a181920a75f4b0869"],["/posts/1925125395.html","55d955568eef25f8a53da1b6acf4112c"],["/posts/1966191251.html","88f3f728f1fe94a3e9994fbfae92e629"],["/posts/1987617322.html","f06944b40585a817d1fef1f19ad23b6b"],["/posts/1999788039.html","8be87c5a489dfa4d220de2ab24603c02"],["/posts/2075104059.html","2e59a5dc3c92b8dc80f45800ac3b0b55"],["/posts/2087796737.html","14933645b076f3657a4859aabcbccf53"],["/posts/2106547339.html","1030a62b3c26023226f944b68c2c5c92"],["/posts/2207806286.html","17b4f61d6a106a75300e5805ea54ba3c"],["/posts/2225903441.html","7a38d1b3d2f2d64f5c3d152d33b75dfb"],["/posts/2265610284.html","7a12d1842a01076163adae4df61d299d"],["/posts/2281352001.html","ca47506d5ef439535449621a188328a5"],["/posts/2364755265.html","93121254ec33a51a39467c08230d9e19"],["/posts/2414116852.html","2638b6c999333b95c0182f0184e68eec"],["/posts/2482902029.html","724f686d58d37919b8ca598bf726fc64"],["/posts/2495386210.html","55d1d80a6ec5aea930b37e42f12282cc"],["/posts/2516528882.html","826a1823b2953f8742ec72acbadd82eb"],["/posts/2526659543.html","0bfa76089208db645388e54bd6bbf580"],["/posts/2529807823.html","b9a9b1e985051b94eed755e6a1ea92ee"],["/posts/2742438348.html","5e74e72df909aa2508e606642fa30f76"],["/posts/2888309600.html","b167b84e8816afc4e8a89fd565f56ec9"],["/posts/2891591958.html","0675aee4aa05092811c81cc258a2bad7"],["/posts/2909934084.html","d2e6a5ffbd3b46cd42c84bc9875da4ff"],["/posts/2920256992.html","110892dc7feae3fd2621db99c9e6cca7"],["/posts/3005926051.html","03cc6f38de0142d31a98db48e06d17d8"],["/posts/309775400.html","a75baa5cacd539e7448be0823d9cae44"],["/posts/3156194925.html","f0edbf7585da42af8ebacca420eb6ee1"],["/posts/3169224211.html","ac015ca66a372c99d3456c0d0125f480"],["/posts/3213899550.html","575f42aac654769580cb6fde75c69be0"],["/posts/3259212833.html","6be368310faf61e1698752f605370413"],["/posts/3266130344.html","092cd108248fd51feaff764b3253d5e5"],["/posts/3306641566.html","9e0d5c225461fc79531855ff4a26dada"],["/posts/3312011324.html","801dddfd00abb2ceb9a2d752278598db"],["/posts/336911618.html","4cffc13154b2a85cbf439e4b3435abd5"],["/posts/3402121571.html","7d981c911b8a1df36947e28b49cd3ed4"],["/posts/3405577485.html","1f2742c6cbc2ca69e041144c2a1bcc1a"],["/posts/3498516849.html","f27c52ca987780a1d4f4637e8d2d0cfa"],["/posts/3513711414.html","eba75b5ec3f90bf93389ee4fbe14071c"],["/posts/3546711884.html","e80554bc383d2e0b42a352c8dd888a9c"],["/posts/3731385230.html","cf276478d3a327bca57e483da5691918"],["/posts/3772089482.html","5fe213ea2c462db48755f42a075413c7"],["/posts/386609427.html","25a556929b5a462451edcd0dc1d95398"],["/posts/4044235327.html","3d8c4d6f6f48614024b4b2976645321b"],["/posts/4115971639.html","158f6653f6fbd1809aa5e685089bbc71"],["/posts/4130790367.html","2a329d8f8f02bbd78371c0334decd22b"],["/posts/4131986683.html","66a7c44776cfcf3675690d382737ca96"],["/posts/4177218757.html","5dcfa339ef94f5c8d7e0ed631f1790e9"],["/posts/4192183953.html","c08e0f32e5c198549b8487640ab017e1"],["/posts/4261103898.html","6d3be07fbc5ff4b04efa7f85aed17d6e"],["/posts/469711973.html","00313012736eadc8058f4661ac45bbd8"],["/posts/482495853.html","4093b71cc9615c3da572aaa6ed9e6629"],["/posts/488247922.html","dcf17276d9982f61df7b4e34fe0b9fcf"],["/posts/570165348.html","0f33f7b953d470e5006cacdb5eb868f6"],["/posts/595890772.html","d14927d2ada0a914e7c308afcf865eb5"],["/posts/694347442.html","d7e05028e097d8feb9020e612544f211"],["/posts/707384687.html","0161279ccaf82aa64282b0f8f6203e35"],["/posts/71180092.html","f7c1ba44f75e6f23cfa11c237b465ec4"],["/posts/716459272.html","8844bd3c35407c4f037d3e82c539abbd"],["/posts/778231993.html","ebf96001711f63ef2b142b26ae59b133"],["/posts/795397410.html","30fbb2033ce30c3b398eacb0487b246b"],["/posts/820223701.html","2af1a87d39438e0242c910eff725197a"],["/posts/830372185.html","23fffbc733c6879c2ae58f6da3a37504"],["/posts/88294277.html","300765cdafa3d892a56b57be3415bdcb"],["/posts/939963535.html","5f8dca12f0943a12b7bdcc94ac191922"],["/posts/983786067.html","cb2a05e81178433ec2bd91e240a4dd6a"],["/sw-register.js","dba167e412d11bd97510b20706444524"],["/tags/C/index.html","bfef38fea2c9a92c363e8592d0a2753e"],["/tags/C/page/2/index.html","bb4dc3ef5f3e28fa78febc3e953279d8"],["/tags/C/page/3/index.html","5ddc91affdd81092fa6981a5e81ea595"],["/tags/ElasticSearch/index.html","c00a831e7b4bfeb93c7ea3011cd1712b"],["/tags/GUI/index.html","d27435aacf85df7fb56ba4cc989ce69b"],["/tags/HBase/index.html","53e0c0ac717570f22ab338bb2496b761"],["/tags/Hadoop/index.html","d7f94daf0e69d1dae32f888cdac05b41"],["/tags/Hadoop/page/2/index.html","1513e20bb115385dcf4ab80682a54a5c"],["/tags/Java/index.html","e6aace0ccbc8378b4b03f4704361ea5b"],["/tags/Java后端/index.html","406fa03f28b6bdb0513734f1cc34bae7"],["/tags/Java后端/page/2/index.html","ab92bcd9fd9ce01a7a7a8b99327b60af"],["/tags/Java基础/index.html","4331ea630f997dcd960eb8aedda85e59"],["/tags/Java基础/page/2/index.html","773a50b70968e5ff402e1748aff56895"],["/tags/Kibana/index.html","d5d15eb30969145de59bc1c90468e02d"],["/tags/Linux/index.html","b41b4c7c056c18c1246ef5231ae675ad"],["/tags/Linux/page/2/index.html","fb8445cc577aef0e8bd9db697658d5bd"],["/tags/Linux/page/3/index.html","7f568d5ddad84070407adaaa0d1d91bd"],["/tags/Mac/index.html","a391c65dd30e455f4e5d9f0811cbac64"],["/tags/Mac/page/2/index.html","cd784343e121c870cf93e54eda1661c0"],["/tags/Maven/index.html","ed8a8f3673ecda294d3ca11ff016acd5"],["/tags/MySQL/index.html","b955170dce7da580e0bd56adb4e0c72f"],["/tags/Python/index.html","1d1ea36d60f0bbb094b7405c18c4c989"],["/tags/Redis/index.html","b92b73891591a90f8e3f86ce9b25ecaa"],["/tags/R语言/index.html","653171a736ede68daa402cac78be112b"],["/tags/Ubuntu/index.html","b94f47683706de0b6f0488793939322c"],["/tags/Windows/index.html","bd75b2f7836928bb8327184946adaa7a"],["/tags/ZooKeeper/index.html","6d9aafaf60984b02022d4ca8952b0208"],["/tags/bfs/index.html","7430140a36523055603a6af95ee37e4f"],["/tags/dfs/index.html","22b58d7fdd336789d5298430fe8bbb91"],["/tags/folium/index.html","c863a1357ac348ca5d542d265d641e3e"],["/tags/git/index.html","0686afb92a6df89dfeafc209e98ea17e"],["/tags/index.html","a27e83fc682a3dce8a1e6b49638d39dc"],["/tags/latex/index.html","e5333567d3f08a9dc5171dc784660762"],["/tags/中间件/index.html","f14e4844456f32008b94652c83ea016e"],["/tags/二分查找/index.html","9b6ad724b9f7414345e23e4a58a277fd"],["/tags/优化类/index.html","8835732415358697f266e22f6cd506d0"],["/tags/前缀和与差分/index.html","dd52f1adb078855b66dcc21f6ae978c3"],["/tags/动态规划/index.html","f01df5233fa95918f61ae46031812152"],["/tags/动态规划/page/2/index.html","f76be19a60bafaeedc4595676ebedb5b"],["/tags/博客搭建/index.html","0399ce4028e2c50c0fdfe720e5ba89b2"],["/tags/图论/index.html","e0216488947971518b69105eeb5d5044"],["/tags/大数据/index.html","f48d29992422c9163e9daddb0edd6427"],["/tags/大数据/page/2/index.html","c6acfc309d1c2f8de33a85f36e14ebd5"],["/tags/操作系统/index.html","9b63426c1429ea7a215bd244272e29ca"],["/tags/数学建模/index.html","23bf97d01d0dfcc250d8b95020ba42de"],["/tags/数据库/index.html","c14503dc25ee6e5bb2d157960e9714e2"],["/tags/数据结构和算法/index.html","069f6598104e8def2591a66800604954"],["/tags/数据结构和算法/page/2/index.html","8a727cbcfd065660eae942c71020e921"],["/tags/数据结构和算法/page/3/index.html","3b022e9cd0141ab509fd519edc3c6006"],["/tags/数组和字符串/index.html","62cee899f578be9053a41fc73e901c93"],["/tags/枚举类/index.html","71edc45dda0699575fb8464bc2d6ae4b"],["/tags/栈和队列/index.html","de11c6957459e13149bd6eb1aa19a419"],["/tags/树论/index.html","4757caa8714590d5387adc41f28f2b91"],["/tags/测试/index.html","2faf0ba3a9e128be7995ae3166f22e49"],["/tags/环境/index.html","9838eb6fcf3c7bf24def8484d6533a5c"],["/tags/环境变量/index.html","cd87507391849cf8af24227da37a3e5f"],["/tags/绘图/index.html","bd2872a8bfed6af4a31f717cff5d25c2"],["/tags/编程环境/index.html","ee928afb23385ce756cc79788643e1ab"],["/tags/网络编程/index.html","c5b459cf6f82480e24fba84a759f45e6"],["/tags/英语语法/index.html","18660069d172dffa0f0b516c65345e56"],["/tags/论文/index.html","41d08bcd47019c9498095f52dc75c92b"],["/tags/资源下载/index.html","52fe4dcf4a91f9ff84dbe823ba5152d0"],["/tags/链表/index.html","d99e4a29c2ba40b78177e1a899039a1f"],["/tags/集合/index.html","f813c97866b25f35a89867a828dea1bc"],["/tags/集群/index.html","2784c79cf3a83e6b0f7468f889045020"]];
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
