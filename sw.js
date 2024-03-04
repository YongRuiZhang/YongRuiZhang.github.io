/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2b4414533b996dc83eae9822ac6d6b39"],["/about/index.html","746df457fcb978276ee08a80a737240c"],["/archives/2022/02/index.html","6dc44c2e447bb723c5caf677b6663732"],["/archives/2022/index.html","b688bac86a6b9cfb6625fb84042f1053"],["/archives/2023/01/index.html","98ac024c421139d4362f8c0713f9e311"],["/archives/2023/02/index.html","4f5165bf664b04abd1801cbc6c90327e"],["/archives/2023/02/page/2/index.html","ee302cead844c4622ff9634a2a5d7fe7"],["/archives/2023/02/page/3/index.html","eda1e1ea6b1b3b3084f798346fa962f0"],["/archives/2023/03/index.html","7ec81f51e71b3de22d40263ccd169ff3"],["/archives/2023/05/index.html","b906a836343fb3fd8c7590f7ca9b3fd4"],["/archives/2023/06/index.html","197185d33b930887bbd57c199bcf766a"],["/archives/2023/09/index.html","9aab9d90e6cce0d1bad56cc5731f3791"],["/archives/2023/11/index.html","434c1eaee26707a63e1eb3fbfc0f6934"],["/archives/2023/12/index.html","8a064151695bffe934f4787eb9b3f231"],["/archives/2023/index.html","cd68634fa2425fb2217c3d2c9f5ccc30"],["/archives/2023/page/2/index.html","9622ff714778be3ca0435eda4aff2e6d"],["/archives/2023/page/3/index.html","7e700c3c241d05b79fcb28d07de03c29"],["/archives/2023/page/4/index.html","3aa21e494cee83543d82a172a33588de"],["/archives/2023/page/5/index.html","7c12ebe8995a4367d8af0b3af6ce6b4a"],["/archives/2024/02/index.html","1b7329b075aea3abab10366210d27a53"],["/archives/2024/03/index.html","5de9f7c87634f71c3ba0a55af0ffc05e"],["/archives/2024/index.html","bba00a3cc15c6aaa03bd7d333511c0ff"],["/archives/index.html","c97eab682e948a9bf66b6f50623b41bd"],["/archives/page/2/index.html","27a7861941aa0c56d30e2b4129ecc58c"],["/archives/page/3/index.html","b1a1225cc8c1a251622d3e32f1616fa1"],["/archives/page/4/index.html","91f9e8ff151f0e6fafb5f98a149f7ccd"],["/archives/page/5/index.html","42498b81276f7c88c28953a5134d6176"],["/baidu_verify_codeva-qQP2iZOMLX.html","cb0de7ee7d715cc995f791a383bad4a6"],["/categories/Java/index.html","023b6d91f22006f7638929d29d5d2c8d"],["/categories/Java/后端/index.html","1ec6922d37fd9d4e2e8bd3f0d22233d3"],["/categories/Java/基础/index.html","f828f1b9ae8f281108c2ac4cb0e61e91"],["/categories/Java/基础/集合/index.html","a276ecd24cf03b30330a1fcf6ccffdec"],["/categories/Python/index.html","225f1a21d9f0810517b6498a550aec1a"],["/categories/Python/编程环境/index.html","b4ed64fded8f4d5696d8f8a307a800fd"],["/categories/R语言/index.html","06a6b84e0f5def44d185548d4f796eb5"],["/categories/R语言/编程环境/index.html","463c645fb147d031e1cac930b0b3deae"],["/categories/iPad/index.html","f337df1603515a0b5f48181fc396cb75"],["/categories/index.html","13637d9f5096cd4bf35f6cc181638386"],["/categories/中间件/index.html","54b5941c9d88d46d5edd6442f99b4c69"],["/categories/前端/Vue/index.html","d14b63329dd15797848743b5cca0ed2b"],["/categories/前端/index.html","34a804201842e62d981b194430fb5999"],["/categories/大数据开发/ElasticSearch/index.html","0685e15a94642f0f8964a671fb871002"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","a48b5f4b06db8f9dfb72d63463733624"],["/categories/大数据开发/HBase/index.html","e3a531b10fd987d4e80ec8db507f9cdd"],["/categories/大数据开发/HBase/学习笔记/index.html","00327cb9be124135f513ca21ef186287"],["/categories/大数据开发/HBase/环境搭建/index.html","ea07b774974e8c9a09eb2bf9a9ff1ae1"],["/categories/大数据开发/Hadoop/index.html","990b510783bc8a1b6dfe3ed6fb016ac3"],["/categories/大数据开发/Hadoop/技术/index.html","44c9205e30ea83d07802d69daf83bc03"],["/categories/大数据开发/Hadoop/环境搭建/index.html","0858542d164da84479f08f62575d0f08"],["/categories/大数据开发/Redis/index.html","d584265cf7657552179fea1d4aa6cc4f"],["/categories/大数据开发/Redis/技术/index.html","3a17ae27632fce02b926192db3f21620"],["/categories/大数据开发/Redis/环境搭建/index.html","1a9ba516ec4daf9a026e052007ca1547"],["/categories/大数据开发/Spark/index.html","5efeea595d53b362c493fea1ec7a98d0"],["/categories/大数据开发/Spark/环境搭建/index.html","60e87582b53adb178f56a14ecf760c16"],["/categories/大数据开发/Zookeeper/index.html","4ddd4054d754b51be7eb2f0488a81106"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","7ec31ceb189afc3466f18a898688be87"],["/categories/大数据开发/index.html","2b8fb2dd4360db58d091d2dea5d1bbfd"],["/categories/学校课程/index.html","ea8f941c5919bd40757952f6da3327aa"],["/categories/学校课程/计算机操作系统/index.html","8d6f9d73e605029ad6e50c7d70b7e909"],["/categories/操作系统/Linux/index.html","5aa3aad5493bceacdd4dac0bfdec3ef9"],["/categories/操作系统/Mac/index.html","bb3bc0ddd2d042b1c7bcfde0ff6fbcc9"],["/categories/操作系统/Windows/index.html","7ac5278778bf3f6992c68dcdd2ae6bf0"],["/categories/操作系统/index.html","6e2105183acb08b300f2f4bdd5fd931c"],["/categories/数学建模/index.html","793d9ad3a6d1d772014d4a3266f1c4a1"],["/categories/数学建模/latex/index.html","8672d654f3c18c5c625c699810664ecb"],["/categories/数学建模/优化类/index.html","96b1a3c44eb4d3b0620c20abb95b416e"],["/categories/数学建模/优化类/现代优化算法/index.html","43fb2567e4b3fa9452074cbc5dcf8be4"],["/categories/数学建模/优化类/规划类/index.html","ced3eb27c51e4e46ef364bc5b3635995"],["/categories/数学建模/绘图/index.html","2e55ce6e6480a7397f95976742ec3ef9"],["/categories/数据库/MySQL/index.html","f0baf4d3e52fa0bf74fa410a648a9296"],["/categories/数据库/index.html","579f7e5616ecfa8b62e02925969860ba"],["/categories/数据结构和算法/index.html","8e135cb61cc5e03150df8a0a82305c5d"],["/categories/数据结构和算法/page/2/index.html","ce335919635f192979ef9dbbe7d3c623"],["/categories/数据结构和算法/基本原理/index.html","46d360f32032fc7f17f657ae4e6c3cb3"],["/categories/数据结构和算法/基本原理/page/2/index.html","ad81a751ddbcee4550f252561cdd0a04"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","c9cd357406330b057a6c1c8b57ce58a3"],["/categories/数据结构和算法/基本原理/动态规划/index.html","fd45fd417b141b9c87a85f0b4a9fbe6d"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","2e11c4b6df3634e62075442f42493b3a"],["/categories/数据结构和算法/基本原理/图论/index.html","9a89602da2321382cb5c57ff7324ab4f"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","3bd3639f2c7ae969200ad024e6a7dc2e"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","a503a919fa11844863bb3c0f19b2e4d5"],["/categories/数据结构和算法/基本原理/字符串/index.html","184c44bb694bdaeacba4a6cec5899051"],["/categories/数据结构和算法/基本原理/排序/index.html","c44b7ed8c8990d5f1f53c7c60bc9be8c"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","ccca9ae00fa7c194acd2136c5cb76c92"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","63e43afe6e65a43941dcee6f4bff0b17"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","1450b03d623c4c993f669de9dc495a6a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","05e4d86ea6798dbaf3b094922d15a21a"],["/categories/数据结构和算法/基本原理/链表/index.html","fa9cb3f93139cd86ff10bb363c6b470a"],["/categories/数据结构和算法/基本原理/高精度/index.html","8820d0224feba161d79ffc486d6e0f90"],["/categories/数据结构和算法/算法题/index.html","fb58cab1df2c1f933cffadf20a5893df"],["/categories/数据结构和算法/算法题/二分查找/index.html","b420e842499e840a136c530c3fbbf068"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","036de38ab5fcbcd6ef77d3a83e83b49a"],["/categories/数据结构和算法/算法题/动态规划/index.html","b2bfc940eed28194cc2334bffa44bdcb"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e3f6cec7dfac1907749615a43f6ccad4"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","da0140cbb0affc8b579f346e4697ba86"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","27c0bea3b7867b98c4c71a29064ec729"],["/categories/数据结构和算法/算法题/图论/index.html","b7a891c7f0c81799f6497c275edc013e"],["/categories/数据结构和算法/算法题/图论/树论/index.html","dcc8ceed91919ea76f7f99e2317cc730"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","e5b1b4bdc1eb3103a74bbe7fdeccfaeb"],["/categories/数据结构和算法/算法题/数论/index.html","146ff1445013fe9012de373f6db52b5f"],["/categories/数据结构和算法/算法题/栈和队列/index.html","17cf1ea4207de4ebc4825146d8c9ee22"],["/categories/数据结构和算法/算法题/高精度/index.html","7f9a045b431b3e4852371a575453e0ce"],["/categories/杂七杂八/index.html","aee553edba120a939a6a4fad264dadc9"],["/categories/杂七杂八/博客搭建/index.html","72439b2eefa3655529ef244b67dc82b2"],["/categories/编程工具下载/index.html","e25d0ddad62aab70c8b9306bb80f04d9"],["/categories/编程环境/index.html","f4ce84067e751c9b069638ae80b4e178"],["/categories/编程环境/大数据/index.html","22cef263b48931393755c209cc39bde5"],["/categories/英语学习/index.html","a2db86a73367ad14da424b5f067d3437"],["/categories/英语学习/英语语法/index.html","ea6996498cb89504c71f8f90fb94c98b"],["/comments/index.html","bea2831b9411510fd7979794a9e8b790"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b49f6bd4c744be5128bc4808e766293c"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a9a0d6fc96a1f8acb5e0018921f65955"],["/movies/index.html","c87911356e47f0ba709fbda6e9ee8fc9"],["/music/index.html","afa02a3b063672ddfe5d3bc207ffe757"],["/page/2/index.html","5c1015a1e73db7c3abbef65a304c5fa7"],["/page/3/index.html","2057d91dd83d76a05d05254578c80304"],["/page/4/index.html","246384942edb68929898fff91249f62c"],["/page/5/index.html","0e04112740be6a4c22d44292cf40c092"],["/page/6/index.html","96ed2f6203abd7e6c2b70cc43d381786"],["/page/7/index.html","4d2b6704676853ba672b2a122ad3c87b"],["/posts/1021360842.html","edbf75a8d8cb33d4917849e791469dc9"],["/posts/1120620192.html","ab84a371e48e10dc66508d633a821659"],["/posts/1137707673.html","7ffec3aab9a3b2987e385f74fc1db74c"],["/posts/1141628095.html","34e05f5dec519712d0429120ec9d8088"],["/posts/1168613674.html","d18c7e0583d7abf3c85b874471b0d6c8"],["/posts/1219920510.html","c1e3e183a8de980f45cecedd7c7a8b39"],["/posts/1222166338.html","95df7d3c68a5f2fa2668724a196fa8dc"],["/posts/1259097482.html","0d3f34831e44caf180893fd41834264d"],["/posts/1271036369.html","1b18582162729b4baf2b55b1f22654db"],["/posts/1312847445.html","be95f9a872e856e2681331b7ab31296f"],["/posts/135355774.html","336deb6ca56b4e27ba2d7beaddb94862"],["/posts/1375344716.html","09717a670271f220ae040d2cca452011"],["/posts/1388991698.html","c757bd179dd016def9394fbd8167bc0b"],["/posts/1410315814.html","f25c446a1437fdceda07d62a6d710c4b"],["/posts/1452790229.html","7bedfb680ef00104eb0081aa04e67799"],["/posts/1470079884.html","7b344fcfaefc719be314f3897078956a"],["/posts/1470079885.html","ddafd8a1d39ad2766eb9a3342b124a58"],["/posts/1470079886.html","e08e041c2a4e75b8961f74db6544038c"],["/posts/1470079887.html","d311325f37b56ba5389ff01557b4d796"],["/posts/1498536549.html","06b6c795ec5d2ff74ffa91ad23c5a4bd"],["/posts/1539568593.html","b9a2a56c24d30c2765794232a42d410f"],["/posts/1547067935.html","9377683c57fe3fe4bc2184801a0b9a1d"],["/posts/1557866301.html","fee983c954d935d302209e31201b1d43"],["/posts/1571776361.html","64636e0645bbb2e7dd3523268b89e9a9"],["/posts/1605124548.html","2dd67b870420f6e9ed6b4f14ab5fd14d"],["/posts/1633036852.html","882aa67f4c91bc5b55abd6cdc691898d"],["/posts/1667740714.html","2422594826db9bc3a9485060f264a50c"],["/posts/1674202625.html","adb6462853c52dc48dfafc3ae38ca24c"],["/posts/1765123828.html","4737aa60a7ec6f2a9d6f747643cf599c"],["/posts/1767336200.html","2fde451b8f00cd819b8bac5d2d1115b6"],["/posts/1776114197.html","b733b208701a04207b6d34f9bb447164"],["/posts/1817748743.html","b5fabe5cb45b1c7ae3689794bbc381f4"],["/posts/1925125395.html","8c2921e44fe0875a806b83be71943100"],["/posts/1966191251.html","09203c4e99e20857819c19b6d73fa4e0"],["/posts/1987617322.html","340b6b94e4c28a044e9ff892e2e86d58"],["/posts/1999788039.html","ecc8835a82da60f131cbc6e5c04d88b8"],["/posts/2007534187.html","097d38725c30102e8ebf4f039895e7bf"],["/posts/2075104059.html","04cbeb4ffe8cb67abb7960b1160effb8"],["/posts/2087796737.html","8edf5605dc9a8cf4fa0d9dc1bd578b6e"],["/posts/2106547339.html","801b98515611f75413a243ddc32b1596"],["/posts/2207806286.html","7667d10349d97089b0beb939628afda7"],["/posts/2225903441.html","8554b4a92274331368caf03fa8a98a78"],["/posts/2265610284.html","4d88dc6ae9a6d4c60546d44e9a7217a7"],["/posts/2281352001.html","b2f6884dc94df113d6f6a735a991c1ce"],["/posts/2364755265.html","71e14203de0c62bd72900907dbc851f4"],["/posts/2414116852.html","22a5e4fce5a8bc289ec3527847b60d9b"],["/posts/2421785022.html","bcd65ef29f37975c687a412abac75589"],["/posts/2445543724.html","0e65565a594eb5f22d308a175edce351"],["/posts/2482902029.html","a89f43a7aca1e16c5ec422d816e10def"],["/posts/2495386210.html","94d60858176c9faedb1ae1916577a57c"],["/posts/2516528882.html","4e45e223d90fb7711b93d5a95294052e"],["/posts/2522177458.html","a96c9c55118402ddb67f195cfae6fc85"],["/posts/2526659543.html","a043fdaf3b5e8aece3c95b10eaeac958"],["/posts/2529807823.html","daff13eb2ecd323486e854040849223f"],["/posts/2592249117.html","57d583f4319738417f0ca280b5b947dc"],["/posts/2596601004.html","9e5d5a770ab8a02c40f4e112a41685a6"],["/posts/2697614349.html","20a30c773231f487a9755bd2e84cc59e"],["/posts/2742438348.html","bfa2216da771a9e4449884ba7e7827da"],["/posts/2768249503.html","5bcf45c0d62f4f71ce4802011c30615b"],["/posts/2864584994.html","47df6cf39fb94fab8dace5cc643920a8"],["/posts/28838462.html","b4eae06d1dd27fcb8f84ffc5d53fd3af"],["/posts/2888309600.html","4118a04e6a763e9b2084fd73ab41d7b2"],["/posts/2891591958.html","0db1e613a1ada0d108e58b7989ad9ae2"],["/posts/2909934084.html","4e3e36f3fad6590807859fbf47bb2844"],["/posts/2920256992.html","752b5203bc7f6787df3130d20582fff8"],["/posts/2959474469.html","5bde127dfe96d6127ab2a8e90b22c6d7"],["/posts/3005926051.html","956e281bdfa41eff516c95c00af61724"],["/posts/309775400.html","fbdbf94d73478e9820e5a11afad960d7"],["/posts/3156194925.html","947def094c352e9dc8083e1d01af6245"],["/posts/3169224211.html","d923028f5de90f5c68de1b21388659cc"],["/posts/3183912587.html","ba63bee1e5a441ffa09cc31c10ea0e90"],["/posts/3213899550.html","e1b6c55c5aa83f4576e93c67077dca04"],["/posts/3259212833.html","c73111a440b138ccf4805b1908702bde"],["/posts/3265658309.html","78123282f07ec14e94c24d85e2bc522b"],["/posts/3266130344.html","db9440f8ca8692e6b242110d1b4d56bf"],["/posts/3292663995.html","28ae13072942cde20416b2e5119837aa"],["/posts/3297135020.html","6b53f49b2523177ef33ef3b0f9a93e49"],["/posts/3306641566.html","f7a9960c46bc89ce203cac587936cf6f"],["/posts/3312011324.html","cea78c138f86f11ca559b60a103c7cd7"],["/posts/336911618.html","1892bf9007dd635a35c9a214907a7e8d"],["/posts/3402121571.html","5cd334e016a3ce5c8ee57a9a7aeb8413"],["/posts/3405577485.html","91577b4e1543dafc913f1f3d8aa6d480"],["/posts/3413737268.html","a2474628e06da1076fecea0067d55800"],["/posts/3498516849.html","7d9de8b81f12b8cff528a331b79780de"],["/posts/350679531.html","15875e08c9567f64e5711b44a4fc7a5a"],["/posts/3513711414.html","18815061374774135f1e1540f7478086"],["/posts/3523095624.html","c013b48f8b95897012bf2e2a32acff7a"],["/posts/3546711884.html","9b42fcf306a3cf4d44a7e60e78d03464"],["/posts/362397694.html","78ab5140dad3ff9dfebf2a31a5eddcb7"],["/posts/3731385230.html","7020ee7bbf39bfdb702ba1367b84ad95"],["/posts/3772089482.html","8e80b5b9b2e0c6093c6486878a781667"],["/posts/386609427.html","9ee5fc3d54615f9c6ec80d16fc455209"],["/posts/4044235327.html","751e5a8aac4af0183008102690fc15cc"],["/posts/4098221856.html","f084ff88cefe8e9b54db9478dd883fa6"],["/posts/4115971639.html","b004574cd5f623335d512e8b13c0c0c1"],["/posts/4130790367.html","2262ae07b61871d939d3d0622f9cf818"],["/posts/4131986683.html","2e879a5cd2533bbfae1e387f6b1b67f7"],["/posts/4177218757.html","67b572a802bfd3718828d8075bb75387"],["/posts/4192183953.html","f0ea4c4b23e22b6dbb9120d6049a2cd5"],["/posts/4223662913.html","cc096a88b5b03c889c694142f2fb78d7"],["/posts/4261103898.html","58122960b0f4924061c3084c3fe53443"],["/posts/4286605504.html","c01f4766dd7d2f23ab35e249d4c8a290"],["/posts/449089913.html","510eaae0a656d7cea658cf6d64cbd97f"],["/posts/469277133.html","3b075597376c55f0c3015ec9b6dc862d"],["/posts/469711973.html","8882ba752b5c0ad8bbc4383c5802e6d4"],["/posts/482495853.html","5ea88162c9031cd2edaefc6c42892d67"],["/posts/488247922.html","342f8621b4ece37cdd90f3675f083047"],["/posts/517302816.html","eb66c00206093e85871f6ef1ed137e03"],["/posts/570165348.html","8ca94a2d8b27811dc0897f35a9f12342"],["/posts/595890772.html","ef0e55e32535510955a856780da2cc9e"],["/posts/67485572.html","2bc8a4593c4623214a8e3107449c7027"],["/posts/694347442.html","2cae62f355f5ca251461a14e18aa11cc"],["/posts/707384687.html","006e1da50ec670e0cccb7a0571ea08cc"],["/posts/71180092.html","4f0999f0b9e59a3fc54ce1130a602e56"],["/posts/716459272.html","c4f0220799184284c58cfb9bd74d62cf"],["/posts/765481613.html","868ada09a1b1a75847090dd03aee7868"],["/posts/778231993.html","5828874b4f9654ee35dc61274f735c5c"],["/posts/795397410.html","6bf14c3a768293773513a2f3e328b69d"],["/posts/820223701.html","6865cf2eb91f15560ccd6877740783fd"],["/posts/830372185.html","170cf9471f635853b343b10845baade4"],["/posts/88294277.html","f9228a7c5eb4df9b659a0ecf374f4bf8"],["/posts/939963535.html","883faf49b50d927e1e19cc467ff42757"],["/posts/983786067.html","e1e9993f714c1f0d1ce53a6aee32070a"],["/posts/999620786.html","64b73ae1dfb439bcad6a98eacacc167f"],["/sw-register.js","30f9ef1cb998b45219d324bc08583153"],["/tags/C/index.html","a9ed0f8e9ef6085c53f6c5a1ed3bb876"],["/tags/C/page/2/index.html","6e85416a2a7e59e491c3a37f01b64c87"],["/tags/C/page/3/index.html","04ab14856b44e7a01afbbe6871302fb6"],["/tags/C/page/4/index.html","a119070d0faa208bcd4f710fac7cc0a7"],["/tags/ETL/index.html","ee293be59132abef378328c36493ee7d"],["/tags/ElasticSearch/index.html","56ba9a381fc0d5b8061f2dde33e501e9"],["/tags/GUI/index.html","5a5c0ac73e7032d6b3e400de47c88d3a"],["/tags/HBase/index.html","413b4fb490c8f96c1fd9f89f40efa19b"],["/tags/Hadoop/index.html","2510de5526328c90d0e92769000f8fde"],["/tags/Hadoop/page/2/index.html","93daf33407af550aaa50fb43651a7a4d"],["/tags/Java/index.html","c498a72a38bd5079dbbe7f1ae28c1d04"],["/tags/Java/page/2/index.html","e7a8cf031c7878be413d8308fbc75f10"],["/tags/Java/page/3/index.html","4b3f3c0763a8a6805d464034fa420e54"],["/tags/Java后端/index.html","afb4266b48a6e928db6d5d16b0dd5e7d"],["/tags/Java后端/page/2/index.html","5ad30c975ba9607de9d2d8858433f9ae"],["/tags/Kettle/index.html","f36ac2a53cd2898922d882c260ab8aaf"],["/tags/Kibana/index.html","713c8d5494ff7e4fc2181dcd54483248"],["/tags/Linux/index.html","d43419ed832a54e5512f50c072d4817c"],["/tags/Linux/page/2/index.html","fd8e0e37f30ef85872bf3ac396e92e82"],["/tags/Linux/page/3/index.html","806a48d6013137732f1f9040c4ff8073"],["/tags/Mac/index.html","fcb72cd6a5969740a8b96d2e50a9cbab"],["/tags/Mac/page/2/index.html","b8a666a17c5a257df6351f76461a5245"],["/tags/Maven/index.html","95a3aefdb8ebd0bb939571dec236ec5e"],["/tags/MySQL/index.html","ffc64056c21970384bb573da84008061"],["/tags/Python/index.html","e2d7be8e358e41960c3a476da22abfaa"],["/tags/Redis/index.html","8e5bb02058d9f26f917ea98b230d37cb"],["/tags/R语言/index.html","604fa7321965a04a029f3b861ed8f245"],["/tags/Spark/index.html","258f5469a7b2e9ec19dde1b0d61830ba"],["/tags/Ubuntu/index.html","42a6cccc3c7ce6e2a4280a9b3aa44d18"],["/tags/Vue/index.html","2e3b4ab5c37837f420a15e67f57084f4"],["/tags/Windows/index.html","eb82056b774b4cb1237ee7d7e5b8480d"],["/tags/ZooKeeper/index.html","b9613a1dd75ad86e6334afa6d58b7314"],["/tags/bfs/index.html","1782b1e0c28f6d016cc56ca9cb06b3f2"],["/tags/dfs/index.html","2757bf36ea59597d3267780d5ec8637c"],["/tags/folium/index.html","fed2b986bbf5b8c692c4038c7653960d"],["/tags/git/index.html","1e9f05c9ff676bdc3e9554e2ef9fe3af"],["/tags/iPad找电子书/index.html","c08d5a8af0d495cfb0c6bdc0f1249740"],["/tags/index.html","978b696b441e0d4599ba956948b0d608"],["/tags/latex/index.html","fb11eb8bdc8e01477667e22fa98b1a20"],["/tags/中间件/index.html","6c9ea8434651b4db26fee06d4634fd2e"],["/tags/二分查找/index.html","91f08e4728c58c792177da99207654fe"],["/tags/优化类/index.html","b29c5c52489d2285b3d61b35f05cd5f8"],["/tags/前端/index.html","7b7c4fbf12e2afc16f9d7aa75190d5e5"],["/tags/前缀和与差分/index.html","15135f026ec4f2c498b7f40aebe96782"],["/tags/动态规划/index.html","51d7bf36ee896852f184a594ac689326"],["/tags/动态规划/page/2/index.html","556131e4f7c6023e8af34dce95b620b8"],["/tags/博客搭建/index.html","eaf439adf72aefe461ccdb8c3e501b13"],["/tags/图论/index.html","83d8bbe73086e2da30f5bcb77084524e"],["/tags/图论/page/2/index.html","df15e031536b82c290900f05e5d301f3"],["/tags/大数据/index.html","3847bacc482da44ca94cb6b910dd8cfb"],["/tags/大数据/page/2/index.html","e52ed6833a0d20d4ff664c44e667fc9d"],["/tags/宽度优先搜索算法/index.html","f2fea5a005f398ce573a42e15d95eb1f"],["/tags/排序/index.html","a1562ddcdf11268d3853569bd0eaa46e"],["/tags/操作系统/index.html","b42954fc74b1995ed0c1d3dfb45e8c89"],["/tags/数学建模/index.html","9650840e215c9d6ce40f9213f01c42db"],["/tags/数据库/index.html","c0841d5549220f98cf1a6cedd88a46b9"],["/tags/数据结构和算法/index.html","d83055b9257320156b3d6cea6657e80f"],["/tags/数据结构和算法/page/2/index.html","c708c46b20d4ff5bf6704f018deeeec0"],["/tags/数据结构和算法/page/3/index.html","d7c1e988d7641ae28c82c258039d78b4"],["/tags/数据结构和算法/page/4/index.html","b16ad194c6c386085b71e382ccbce839"],["/tags/数据结构和算法/page/5/index.html","66bff6830a023e844eb6f4b5bfb588f7"],["/tags/数据结构和算法/page/6/index.html","8aa50aecacc536574386291996d76e01"],["/tags/数组和字符串/index.html","5dd7a9a20d932baa6cc84af6f2f220b3"],["/tags/数论/index.html","5e5878425e46f64ab28e94659d2a32b4"],["/tags/枚举类/index.html","d1955010e34392a684d3a828038ddb16"],["/tags/栈和队列/index.html","00b444b69fef904cce09652eaa44798e"],["/tags/树论/index.html","baac47132268a534c66ae490b5476546"],["/tags/测试/index.html","1515aeda3b4150231393d77bdadf286d"],["/tags/深度优先搜索算法/index.html","63c9d876c857fa98ffe36b7d33b28131"],["/tags/环境/index.html","8ea093d8b273802497adae31ff7667f6"],["/tags/环境变量/index.html","dfecad8ad28391139c1774c40c40bb94"],["/tags/绘图/index.html","f3d3dc78bd7421175bfb742e41c26701"],["/tags/编程工具/index.html","1c9d89c384c0ef227d7199bee98d4ea4"],["/tags/编程环境/index.html","5251c0622c0253a8bfa346a77f33f027"],["/tags/网络编程/index.html","f691fc5745a2db407e06a15d46e9281f"],["/tags/英语语法/index.html","a5b0c7aae90280a7fff382a286518160"],["/tags/计算机操作系统/index.html","7c43a3cb26be011df73c4d97d93e120c"],["/tags/论文/index.html","95d7f0b11f65fa6448f174320042c93f"],["/tags/资源下载/index.html","28fe43788066704d152b74169c2472e5"],["/tags/链表/index.html","3184adb36461d52f558adb115e34bada"],["/tags/集合/index.html","95604192277350be4b82dcfd1320b5f5"],["/tags/集群/index.html","1c78f21a54fccfb85e43238911353a6a"],["/tags/高精度/index.html","28d47fb0066596eaca3217d8a4253864"]];
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
