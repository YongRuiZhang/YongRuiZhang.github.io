/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","88448058e78f4b9875437d344a620dd7"],["/about/index.html","1186cae347d59e08db60148c25b094bf"],["/archives/2023/01/index.html","079436547bb64e6577e760709ce19d9d"],["/archives/2023/02/index.html","59146ca624c763832081dc454b4270da"],["/archives/2023/02/page/2/index.html","ac8ac49bb2f2250280ecef755dfe45d3"],["/archives/2023/02/page/3/index.html","f14141b81989b98cd59e4edab2f64f44"],["/archives/2023/03/index.html","1d667aad019dba42532973a5f95b4116"],["/archives/2023/05/index.html","35123440cc3d0603aa2f4563672ee594"],["/archives/2023/06/index.html","88dd3ee256af9e3ea0bc08ba65185916"],["/archives/2023/09/index.html","b906f37458246e3fa02458d36b921138"],["/archives/2023/11/index.html","9612f1460d51b800e702d732c2cfb657"],["/archives/2023/12/index.html","5e80a3a3e7c176c632375bc478632bb3"],["/archives/2023/index.html","a0912057f5e53f44ee5eefcf70321a2a"],["/archives/2023/page/2/index.html","a9713a05b1b4009b24924828fd5f26fd"],["/archives/2023/page/3/index.html","632132c041c083790abf705fbdb3f33a"],["/archives/2023/page/4/index.html","26d3f78cae0f08c9b8b45ccf780aec5a"],["/archives/2023/page/5/index.html","9b11ccf7267a4dd03075bc7fcb5ac7a7"],["/archives/2024/02/index.html","e714b30eb529c4d241411707632e11ec"],["/archives/2024/index.html","5fd3da0b4113752e5ca9d7d5c3c275b5"],["/archives/index.html","bd8b6d8243f00730896644162423f766"],["/archives/page/2/index.html","cf9b7d95ab2ecddf20f8e72064456780"],["/archives/page/3/index.html","76e08c243bb2a262ef6628ef4c6893db"],["/archives/page/4/index.html","9f3acb8b9cbf8d3e5569a1e0cd10ae4a"],["/archives/page/5/index.html","3f556db05e305d9dbe041ac1bef47607"],["/baidu_verify_codeva-qQP2iZOMLX.html","f60c29e711ab4b7aa7a6244789ce2ea3"],["/categories/Java/index.html","dfd4635619cbceefff584042e7ddfa1e"],["/categories/Java/后端/index.html","5006ff1764c1bb9c9833c56ce4ecc48a"],["/categories/Java/基础/index.html","82deba5333ade50850ab277396452679"],["/categories/Java/基础/集合/index.html","81f2f892fb38753350835fd97d339528"],["/categories/Python/index.html","35a59a85d3f0bab61fc79158877a275a"],["/categories/Python/编程环境/index.html","1a3b83ab94b6f41129f8aa51911155f6"],["/categories/R语言/index.html","6398bf043be915bd00d832bfe1722a66"],["/categories/R语言/编程环境/index.html","9f2e79c9a1541efe5bd9a9fd68c520ee"],["/categories/iPad/index.html","520b5148043ed39f1c5404a3c92f4bd6"],["/categories/index.html","b9a5220631733396b412afe19af9a1e6"],["/categories/中间件/index.html","9a636a55b1811a802ab3a3c6397a4468"],["/categories/前端/Vue/index.html","e696c20c33a9563627a0f634f5d7a1ec"],["/categories/前端/index.html","07eb49057d8e48d93a103ddaf3b60949"],["/categories/大数据开发/ElasticSearch/index.html","5f7aeab3441759280ad59fd1eed0cb20"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","a69bfcf0d5dbaa8d36004ebb52ff954f"],["/categories/大数据开发/HBase/index.html","e465bb7543cdf2a3f807f617163674c0"],["/categories/大数据开发/HBase/学习笔记/index.html","88875ff6fec9ae705266c1be8241f665"],["/categories/大数据开发/HBase/环境搭建/index.html","1206dffb6ce63582ec8d10979846f53a"],["/categories/大数据开发/Hadoop/index.html","3bbc9d53863dfcba67beaf933f055abb"],["/categories/大数据开发/Hadoop/技术/index.html","f7e68e1b9b89a91753da7779d8934044"],["/categories/大数据开发/Hadoop/环境搭建/index.html","02277f3051e3e3ba481b246a11a6b964"],["/categories/大数据开发/Redis/index.html","e26c7aef6eef51f46c334bbf13585714"],["/categories/大数据开发/Redis/技术/index.html","89243fba64a139cd85d257f732484813"],["/categories/大数据开发/Redis/环境搭建/index.html","ff94b6d7348ed86a0207821f57d0389f"],["/categories/大数据开发/Spark/index.html","45c8a7398e86633e069867b9bc824220"],["/categories/大数据开发/Spark/环境搭建/index.html","0f7fbd36d374a3996055ce567f4c60b6"],["/categories/大数据开发/Zookeeper/index.html","efbcd5b6fcb9a54b304d7118edbb9f79"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","4afeee08f8584b745f3014125d62e6da"],["/categories/大数据开发/index.html","62be8e28b6042707addfec1b54b809ea"],["/categories/学校课程/index.html","668efd52d03f7acf380a64034cc2d757"],["/categories/学校课程/计算机操作系统/index.html","9512d8d1290a14b6beab92a1e41d2f7c"],["/categories/操作系统/Linux/index.html","b953b41d56f6a13dd486472e6e9b5b7c"],["/categories/操作系统/Mac/index.html","e660009f9d4b4a370ea126f46353bafc"],["/categories/操作系统/Windows/index.html","5bf164b047f0d0c4fdc208b0191fb3f5"],["/categories/操作系统/index.html","ceff002782b1dc1cd5bafc87d3d8f795"],["/categories/数学建模/index.html","cda7f43bd9cdfe6a1670f53f9fd2d1ae"],["/categories/数学建模/latex/index.html","2fcd1674d11c6897591a6117b5a89370"],["/categories/数学建模/优化类/index.html","f9e1ab97e9d3915e1a250912d909af87"],["/categories/数学建模/优化类/现代优化算法/index.html","dd4ec31667b341fa4b3af83c1447350f"],["/categories/数学建模/优化类/规划类/index.html","b5811692ed4981546e4c8f1eaa28598b"],["/categories/数学建模/绘图/index.html","a6e0571599185d0cf623e09a6bbb1b4f"],["/categories/数据库/MySQL/index.html","abb9053d16b7d88e02258e2543064dbd"],["/categories/数据库/index.html","bcd1a9b954eeb9bb4712a714160bb260"],["/categories/数据结构和算法/index.html","697824ccc9b85392f1260a1a3d449472"],["/categories/数据结构和算法/page/2/index.html","db8151d73337e1d5238b5cd16511c0e7"],["/categories/数据结构和算法/基本原理/bfs/index.html","8b176f49bc93544b096682a86daf3ed8"],["/categories/数据结构和算法/基本原理/dfs/index.html","4a235670b81ccc2c3dbf103cb6f8de6c"],["/categories/数据结构和算法/基本原理/index.html","81cfbf532f13389761271cb52068752a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","560c99462afc6c3077dad8e3f903d991"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3c7ee28fbd3aa624749880db3b736803"],["/categories/数据结构和算法/基本原理/图论/index.html","fec6c2d831551ccebe53aafbdaf319c0"],["/categories/数据结构和算法/基本原理/字符串/index.html","cad4362a88d399777cecca2d04230262"],["/categories/数据结构和算法/基本原理/排序/index.html","df56e0f79518950a4dae566d4d14684c"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6128e255ac822ff911fbcf0a57768516"],["/categories/数据结构和算法/基本原理/数论/index.html","7d9b6cbeae37aea48e95337e0fe3c5b1"],["/categories/数据结构和算法/基本原理/树论/index.html","3c52a9439027188c5b813160a7efcabf"],["/categories/数据结构和算法/基本原理/链表/index.html","34c8f7cc3bef33cefdc4fab3662e20d4"],["/categories/数据结构和算法/算法题/index.html","fd6d63ef719a7c4d0d9220222dcb322e"],["/categories/数据结构和算法/算法题/二分查找/index.html","4f8e7310d8a8bb832777b5d6815afe97"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f7886fca8fdc92b33bca3d828b99f71c"],["/categories/数据结构和算法/算法题/动态规划/index.html","4a94b6f0d92badf529945d7119e2bd77"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","bead74dbc1fa479b9c8cdd10efa12a4d"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","785920531156b68f2828f79e090eb501"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b4319b084a335763b9c9f9643a612a8c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e9b334c3b79b29490bf72d448427f8d0"],["/categories/数据结构和算法/算法题/数论/index.html","a91f7311251e4abdac22564155633c66"],["/categories/数据结构和算法/算法题/栈和队列/index.html","311f773345cc6f6c83778e6d18f881cd"],["/categories/数据结构和算法/算法题/树论/index.html","aff84034cfe91a117ff369be53f41b09"],["/categories/杂七杂八/index.html","f814851a0b2bfa2df48dbb5f0eda0aaa"],["/categories/杂七杂八/博客搭建/index.html","d0065f419b6e302a46a1e5dd0ec5bf8a"],["/categories/编程工具下载/index.html","dc4feef102998ecb7d6a75b9fa67576e"],["/categories/编程环境/index.html","cc79e8f5ebbe7a591076706c32444d04"],["/categories/编程环境/大数据/index.html","4aacfb58163cd98857e97bbc10ffd251"],["/categories/英语学习/index.html","abe1b8b4d438baf7fc9285ae68fd119c"],["/categories/英语学习/英语语法/index.html","5feefbd85d1b9ca03e22d4f7a4162a78"],["/comments/index.html","e7ab2f76db6fb7552a07203e231f03a1"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","532faa1cf7d1ba53dda13cb03773443f"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","50ba0fd38016df069e0cb01d07481b8c"],["/movies/index.html","d8f847bfa3783109b78a8b2e822015af"],["/music/index.html","5f90db940dbc663cade39b671f55fcb8"],["/page/2/index.html","9f81b0c478ce0f62b91c6d86595a1c27"],["/page/3/index.html","c83f0a2db8062efbbaed694afdcf89ab"],["/page/4/index.html","f22547442838a9c0e534a4cc403d6aed"],["/page/5/index.html","ac776a8adaa1afb2c1df619262217a2c"],["/page/6/index.html","73c3efdc58286c9dfd4088db191a8b94"],["/page/7/index.html","223c1b512dcbc4a91c1f8a6ad51aec1a"],["/posts/1021360842.html","953025462f28de2b007ae8669f73b2aa"],["/posts/1120620192.html","21ea427140722209e03b0b98eb0db528"],["/posts/1137707673.html","448eeabe63cd51f0f4afcfa05ad3a09d"],["/posts/1141628095.html","36c990b5460f98dd4b161923c1ad6f14"],["/posts/1168613674.html","6705b3de0760bbae286051b66e4cfc6b"],["/posts/1219920510.html","186c948264bb5e6d12a1b97604b516e3"],["/posts/1222166338.html","e050948810260eddcd5f19d3b48b2fcc"],["/posts/1259097482.html","49bb122378fc323005da8f7f21927712"],["/posts/1271036369.html","3c76218e50f530fffae261732fce6ca5"],["/posts/1312847445.html","967f425ae78a78c6f255e7f547337dbd"],["/posts/135355774.html","b5869a26d40aecbfb459b5f2189f40d3"],["/posts/1375344716.html","3163a539a89a6514f47a3ade357403f4"],["/posts/1388991698.html","c353aaafef0bfdbe1a4ddda3d86f67a4"],["/posts/1410315814.html","450918ed637629254e062ca21632e9ce"],["/posts/1452790229.html","a0158b6064f481d7e278ea522cba74b0"],["/posts/1470079884.html","763bd787fe002a0d26a93ac2f758672c"],["/posts/1470079885.html","a1d91d1c7cb001608750102a68424bfc"],["/posts/1470079886.html","5ac75edc8431b2499e3d2d2103ddc95a"],["/posts/1470079887.html","d88cd63368544b05b073e9574ccfb92a"],["/posts/1498536549.html","5c2b8fe4da9e69569f9cb545408b9dfb"],["/posts/1539568593.html","b1642f26cbcb5334f5b69ad5cc2969cb"],["/posts/1547067935.html","6e49d11478dda01701db0a2ead8f4dba"],["/posts/1557866301.html","056c1578c516f746e1fc759fc2dfecb2"],["/posts/1571776361.html","26e47445d5fde0bcb0385ae1996f2912"],["/posts/1605124548.html","23e0f7a46ef3db8d0738a240a1df60cc"],["/posts/1633036852.html","1cf33ce54f9f9518944b25dea0411429"],["/posts/1667740714.html","afcc26eceb2682cebe0f695760f6f103"],["/posts/1674202625.html","ceb361cfb5e6fa77d958b63017d641a4"],["/posts/1765123828.html","34dde827894c4f661d65c2667030dd48"],["/posts/1767336200.html","43d955af3534464c19ed7730618f8fa4"],["/posts/1776114197.html","15e9f2acab70dade12cc2446ef39c551"],["/posts/1817748743.html","4ef1fa9d879c6e246d178b096e0a49cd"],["/posts/1925125395.html","4b3e47f17a1ed7a8c06e2dccd9c5a7e9"],["/posts/1966191251.html","f43954d722ef22a0efda53f7390fb319"],["/posts/1987617322.html","f63a9a97efb5c481f3e4c75884b3d634"],["/posts/1999788039.html","b21647604d4619844d29be779f548d65"],["/posts/2075104059.html","001493e22c543df256fb8bfe593f6d1f"],["/posts/2087796737.html","6b8cb9e0d17f0fdc6ec28b01f0f422c4"],["/posts/2106547339.html","5a1ff35968982be59310a2b39e720f56"],["/posts/2207806286.html","dc30f407dcaab6950ee12a06b87ffcd9"],["/posts/2225903441.html","73c86323e898e63acaa78e6438e3920e"],["/posts/2265610284.html","8e5405ccd9e099ea1ad04a092d38bd57"],["/posts/2281352001.html","3a416f179ed569a170f0941991bf665a"],["/posts/2364755265.html","3e1460c44a020f7b029e6df55d487d0a"],["/posts/2414116852.html","eed34620885db9d7b0cc362d0c41806c"],["/posts/2421785022.html","53119b32c6edab7bc8446daf5dfef37c"],["/posts/2482902029.html","9cf863bd23ffff4d13dfb0dce82f26bd"],["/posts/2495386210.html","b3edde1f5b333a405e135e1758d49894"],["/posts/2516528882.html","6c20bb9bf9a91af48aee6e6d0e4891e0"],["/posts/2522177458.html","5f6475ced7ee96d9c7b12f58879f5d3d"],["/posts/2526659543.html","6280487610538cb1cef986f4da70b8bc"],["/posts/2529807823.html","ee79a05e76d51e258594656c0e6bba13"],["/posts/2596601004.html","dc3e6ea0387b60ac1ffb5895252c1e9d"],["/posts/2697614349.html","06de5694576699a9d0be25e32241bf50"],["/posts/2742438348.html","ea05e5c2244d976ea8f9498aff013969"],["/posts/2768249503.html","e4aeeb5d4e28e8abfce2cc99e3f10ec3"],["/posts/2864584994.html","58c6ccfe82f6f55b14659abf20d07409"],["/posts/2888309600.html","d1714334ababe855e21e28200735d9ec"],["/posts/2891591958.html","8f02a4dec05ed2979d5d42ef16b5c351"],["/posts/2909934084.html","758eb56d843e2a1aad39be2154ea8331"],["/posts/2920256992.html","e0e2b82bb0707532f8156ea91389f2ad"],["/posts/2959474469.html","5a439666e93fdf2151b784e451a8f1ba"],["/posts/3005926051.html","816f11c4373b2c2a0ef07f7c763b22d3"],["/posts/309775400.html","243c0eb3fed3f4f04130f03916ebbfd3"],["/posts/3156194925.html","344ab1e4312f993fa57b69982cb7d5e9"],["/posts/3169224211.html","8dba5d5952733971fa09cea694866476"],["/posts/3213899550.html","2711dbaa54283f39df4920844915d4d1"],["/posts/3259212833.html","96b78369c19a0664eb1f82dc06cb4f53"],["/posts/3265658309.html","e313dfd60e9b54bea31263162120013d"],["/posts/3266130344.html","3bc87626acf3d88fd3532bbdb9468633"],["/posts/3292663995.html","e6a943018eb70f01908c7be84781881e"],["/posts/3297135020.html","19bca89c446cc8c510eaeadbc35eecc5"],["/posts/3306641566.html","9519a55a394aba1cd767f43d69219197"],["/posts/3312011324.html","9d3a89c8ebd633bf10ab041be0db887a"],["/posts/336911618.html","3e11641f9256d8929ba6fb8e473b6cab"],["/posts/3402121571.html","8004488a861c0195debf919870e0b965"],["/posts/3405577485.html","b89321e43adf04ec41e1fb4345ca7964"],["/posts/3498516849.html","1904dcbaa78b6d0164e07c8588187e5f"],["/posts/350679531.html","83f2f1b997ada67394bd21b1011255fb"],["/posts/3513711414.html","222e1b1c888792de0539c840526efe27"],["/posts/3523095624.html","06357b126750a5c61fc476eb189e48bb"],["/posts/3546711884.html","a42812cea78bf3bad6d5d2c26706b694"],["/posts/362397694.html","8b59ffdd5c4029a5bf7e87a56fb1ff03"],["/posts/3731385230.html","edba311f3bcc5f8ac92933a8c69dfd08"],["/posts/3772089482.html","0dc78885354da82805d14a36c3830420"],["/posts/386609427.html","c74032925b15bf7d66a8cbf037ff3207"],["/posts/4044235327.html","6b451de51d464d0c24582ac6abbb7aa0"],["/posts/4115971639.html","4103b80ee535bd65c85b022527a2756f"],["/posts/4130790367.html","559cd77d1c75a1d56274694f1b529f18"],["/posts/4131986683.html","57412557fbd6da6882dde98945183b96"],["/posts/4177218757.html","140a6b33d751b83b4455bd24e2b03981"],["/posts/4192183953.html","17dd00c7712115c09a874e458ebc8a3b"],["/posts/4223662913.html","52b624a14c9220a35297811a51763a42"],["/posts/4261103898.html","487680919b52a0aa1f358c20ef7188d8"],["/posts/4286605504.html","2c856cd4f3019ca38d6e88b8a25120ad"],["/posts/449089913.html","f2902b06e988af9757704adc1f00154a"],["/posts/469711973.html","62284bb2393616d6a1511122f9dcdf90"],["/posts/482495853.html","5f386dc9d1833a8b7fa598a66a723d2f"],["/posts/488247922.html","90d0943f8cbbf8b1e89c099ac59711b1"],["/posts/517302816.html","98c18ae84f8d54a34e2f2dd579acb7f3"],["/posts/570165348.html","860dc7987d9af05a62cf2f2711061408"],["/posts/595890772.html","7336e62b703857903fd3b5d5a90cee3a"],["/posts/67485572.html","e491568b9fab982d7917bf621e06fef8"],["/posts/694347442.html","746a77678ff42b23f32d786b3ade4ed7"],["/posts/707384687.html","3287e8d8109127f2caccf69767fdea1a"],["/posts/71180092.html","9fe933d8c3ac38cf462996c4d2a742f0"],["/posts/716459272.html","cd3944bc40c08137644a709e2d8e7ccf"],["/posts/765481613.html","d75da6135321f73d14270f2fe53b96b8"],["/posts/778231993.html","bd42ae16e9cd3d49469ff2fc5e34f28f"],["/posts/795397410.html","ae9a12f414336c2e14b7a5df9f461175"],["/posts/820223701.html","0593903e6034093eb3fd545b8f1b71dd"],["/posts/830372185.html","8319bf2b1569e917c23d23fc6e0eac44"],["/posts/88294277.html","22f777f536c9f4fbb23cb74103a20c5c"],["/posts/939963535.html","f11543eaf1f05bd68c2858134144aafa"],["/posts/983786067.html","4cd80aa841e161040b3482847812c416"],["/sw-register.js","3ca71afd7ae169ddd431ec8e71e51d21"],["/tags/C/index.html","7b2f873c37f44ce278ecb724d69dc3d1"],["/tags/C/page/2/index.html","7ab9dae31813e60aea7332e1e88cf5cd"],["/tags/C/page/3/index.html","ef202d0271d64db85d76ac76764c7af7"],["/tags/C/page/4/index.html","20df57f6f13b8a7c432577de3a84ba97"],["/tags/ETL/index.html","12676690ada61dba9189db894e46c264"],["/tags/ElasticSearch/index.html","857c3e503baf0915c7ce4ccb08c30d75"],["/tags/GUI/index.html","ee55241ac6e8f48c1ab3371e625aa79a"],["/tags/HBase/index.html","3f33f759c7236a54cfc3dbcf786a2f7f"],["/tags/Hadoop/index.html","3de945d33b15fa9e55191d3b5cfce5eb"],["/tags/Hadoop/page/2/index.html","e22132d6b2b9f0bcd687ad413ffae232"],["/tags/Java/index.html","9f459bb45de54d7930c456d29e7beaee"],["/tags/Java后端/index.html","4d059bcb9f976c0e8786910f07042b6f"],["/tags/Java后端/page/2/index.html","c63bfa79f4728fe8fd84d31187727c1e"],["/tags/Java基础/index.html","d168e1d26780e17175107d2518ecb17b"],["/tags/Java基础/page/2/index.html","be5ee8db5d1edc4635865e25c2b8a759"],["/tags/Kettle/index.html","d64e582c5c2aaa9dd1f6f5ec9faf5261"],["/tags/Kibana/index.html","147c02e4669c5a839e2f5d108fb7d283"],["/tags/Linux/index.html","2a0219cac21bc265fac14d5a79251f61"],["/tags/Linux/page/2/index.html","b2b14c11647f326ffd24a644d3aa33e9"],["/tags/Linux/page/3/index.html","4cdf6f7a3e0e78eb6f6d2483e9752af9"],["/tags/Mac/index.html","77c441202ab5bef3b19f2bb412f361a4"],["/tags/Mac/page/2/index.html","96beb4cd26caae8a1d07fc88f7a85a7d"],["/tags/Maven/index.html","dad661746107351f0a844d34566f6b91"],["/tags/MySQL/index.html","c462f2c74d3b9c0834ab14450002d36b"],["/tags/Python/index.html","fd4dc739e313f2870b065e6526153ed4"],["/tags/Redis/index.html","b8296c8348db98b01998e6c2470a8a5e"],["/tags/R语言/index.html","08974a9aac585196e6a0ecae80294216"],["/tags/Spark/index.html","609d8a1d701f793d5da31371e560a4fb"],["/tags/Ubuntu/index.html","4c3e7dd83aae08929fc852977af5dad0"],["/tags/Vue/index.html","faaeee1a5f2dbc69bb8ae3c208646ef4"],["/tags/Windows/index.html","7d23147ed1d66ecce9dbb9964dbd8ec6"],["/tags/ZooKeeper/index.html","6a99ebd09406a771f44db26a704bc711"],["/tags/bfs/index.html","bc8ae927ed7629536c736f09c9fcdf5b"],["/tags/dfs/index.html","e037a7699a33468f14b850078329afed"],["/tags/folium/index.html","6f23c523c98dc8aa92436638dc790867"],["/tags/git/index.html","869f7b4269a3f421bc0832e6dd049d15"],["/tags/iPad找电子书/index.html","bae249538372c3e40c7b2d28a590471b"],["/tags/index.html","fc647b3cdbf9b6442c31003dd19929f6"],["/tags/latex/index.html","5849a9a697da6a47f4eabbd9ff47bec6"],["/tags/中间件/index.html","b97f4b2a8b1c9cc43f059f7499ade679"],["/tags/二分查找/index.html","eca02b0efc72b48ae7fca96ca116d8ca"],["/tags/优化类/index.html","0500bbb4623adaab499d1300b8082eec"],["/tags/前端/index.html","a5327b1b13bd2b19f84e6bf511041042"],["/tags/前缀和与差分/index.html","9d92eb0430972bf633822a1606079761"],["/tags/动态规划/index.html","0506c658b9e72e7c758604de470739fd"],["/tags/动态规划/page/2/index.html","2e64c4f159ff7b14c3215c66d46eea93"],["/tags/博客搭建/index.html","bdb72c73dc20d14a61a0ed7c7aed5b54"],["/tags/图论/index.html","1dcaa5148224cbdc9692234a59b4b8aa"],["/tags/大数据/index.html","4f07087dca2a8109a74faf7cb064c9e8"],["/tags/大数据/page/2/index.html","2a29af4b9d72734f292980f4f8376c66"],["/tags/排序/index.html","45a19ca4948e5d17c6043c473a838af9"],["/tags/操作系统/index.html","4d0f42313c5cdcec3c4795068ce356ee"],["/tags/数学建模/index.html","3ec7c4699985b58531e4977cd28ccc3c"],["/tags/数据库/index.html","9f74207f770396ee677a894234e552ad"],["/tags/数据结构和算法/index.html","67c202a347872968e098f65268347598"],["/tags/数据结构和算法/page/2/index.html","e4b439a7efd62a4d90b84eaf9de8d409"],["/tags/数据结构和算法/page/3/index.html","10ef935d95e88d97274b4d31a511ba7b"],["/tags/数据结构和算法/page/4/index.html","ac20b021d18b79d474aafec5e4a10d9d"],["/tags/数据结构和算法/page/5/index.html","f50762152e22bbcea83946e09bc08e51"],["/tags/数组和字符串/index.html","9fffa507b32c5d42358abca6aaee607e"],["/tags/数论/index.html","e1dad2585bb20f0b4e99899fd5d6d1c0"],["/tags/枚举类/index.html","ca08c549016c919a378085eaa19475d8"],["/tags/栈和队列/index.html","d06da422a850424dd50c13d27fca203b"],["/tags/树论/index.html","e60a3ef880c2f5d61cad29f3381e51d6"],["/tags/测试/index.html","e51c75ebe3703180c733e029e34513ff"],["/tags/环境/index.html","05240dc7ca2ae5be760c00b15f470c65"],["/tags/环境变量/index.html","af10a7d482e6aea86bc2fb32ac623ba6"],["/tags/绘图/index.html","3a0cafdad48ba677229753df4cea7b84"],["/tags/编程工具/index.html","43f1fbb011925bf9bf0b6bf0e2eae6ec"],["/tags/编程环境/index.html","22fb0e961673962a42de51bd81a884c6"],["/tags/网络编程/index.html","dd4e77ff6621a077e70ff145433a7e5a"],["/tags/英语语法/index.html","839de4a64573685aea1aab47e6006950"],["/tags/计算机操作系统/index.html","17f2b1a7810123d63a8670fdcea93100"],["/tags/论文/index.html","e976142890e8a6fb53585b8c81ff4481"],["/tags/资源下载/index.html","ed9295bb7f319f3c45854f4226eda08d"],["/tags/链表/index.html","97d303baecefe39540cbe3cb74ee1c97"],["/tags/集合/index.html","7be64fcb43a4da680d4e8e6a886efee0"],["/tags/集群/index.html","5beee3116e03d46c2ece040d570ba928"]];
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
