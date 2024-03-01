/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","0b1ad13becbed0cfa06d7c47d9ba49e5"],["/about/index.html","a24598e991a7ae5e233c773d426aa605"],["/archives/2023/01/index.html","1353bd586e8d42040e42be05bb74c700"],["/archives/2023/02/index.html","8423dd4d8a35d14c9ba9b90ce3f47527"],["/archives/2023/02/page/2/index.html","0373dddad9db2b3b74b1c7c8cc72a411"],["/archives/2023/02/page/3/index.html","0714da727a88de2664481f48758a578f"],["/archives/2023/03/index.html","6b5eefad31129277b916073a534b4ce5"],["/archives/2023/05/index.html","fad05dfba893cdfd1033b82418f799c2"],["/archives/2023/06/index.html","b5b6aaca03d381e34544d4c58222f0f5"],["/archives/2023/09/index.html","de3da1dfe279fa12bf145c232590279e"],["/archives/2023/11/index.html","12db7d36d2f020d13e595634344fa32f"],["/archives/2023/12/index.html","b0aaf75af65ee833be533eb18d3d9410"],["/archives/2023/index.html","cece64a8ed10114e4799ef9f49b9e351"],["/archives/2023/page/2/index.html","a3382ee3f4cad35de690647df4066e71"],["/archives/2023/page/3/index.html","72dae39e78ce888ccc88e70adf65841f"],["/archives/2023/page/4/index.html","468b44bafd6a288ac5407ddbe2f99ed9"],["/archives/2023/page/5/index.html","7ea2be145cb51a3b6b620d24b86c3d59"],["/archives/2024/02/index.html","c69494468e0acc8c17a18486c069a8f6"],["/archives/2024/index.html","263c41997c24a34ce424a39185a2f593"],["/archives/index.html","3d438d6997f165a8b9b8a39567e621ce"],["/archives/page/2/index.html","f22ac2178a2cb615b888630b57dfab82"],["/archives/page/3/index.html","8d6ee4859bc89429d78377403e92e4da"],["/archives/page/4/index.html","25004d52805705684741395dc50beeb4"],["/archives/page/5/index.html","75fb06b2eddd2df01ec456a9ec59d88b"],["/baidu_verify_codeva-qQP2iZOMLX.html","5a028943aaa0802f2bf0551503940efa"],["/categories/Java/index.html","07aee56711e7ff39eb6316aec06be004"],["/categories/Java/后端/index.html","5f7ffb196e705a3e9ce3722ea8caf44b"],["/categories/Java/基础/index.html","191fedc5521bfd0b27733a7703e24525"],["/categories/Java/基础/集合/index.html","1949bd4d16eb0efe53e31684163867dd"],["/categories/Python/index.html","cf97e1eb51606f8897dfd277b0a620d5"],["/categories/Python/编程环境/index.html","a38867dd3602f103cd0688832bf26247"],["/categories/R语言/index.html","ee6daf32fb98a852599780fea007d708"],["/categories/R语言/编程环境/index.html","af9e39c375643c72587f1966bbd7a2ce"],["/categories/iPad/index.html","1c743ecc101f117e4f69ecc46c4ee10f"],["/categories/index.html","dad0e222371a7a87174ec99fe25dbdbc"],["/categories/中间件/index.html","3fc9a54cb7174e6e7ca07d8cc3793524"],["/categories/前端/Vue/index.html","67797393b98d1e083439166c4dbcbc18"],["/categories/前端/index.html","c4c743f040bd5156c1e97c84ed83ef90"],["/categories/大数据开发/ElasticSearch/index.html","2d281fb203c9caf4dd59795c85fc1d7b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","05e7a2f2f0d44b32f6c065a3a546b3b7"],["/categories/大数据开发/HBase/index.html","48a35ef92effe8dba578489848afe14d"],["/categories/大数据开发/HBase/学习笔记/index.html","36237f2b93099d028afc37ae798791b2"],["/categories/大数据开发/HBase/环境搭建/index.html","af7745ca358db74db032cdb03adf7e51"],["/categories/大数据开发/Hadoop/index.html","f387de3b33af3a13b787b89d2df8cc2d"],["/categories/大数据开发/Hadoop/技术/index.html","031ef386ee11bb38fd254c7b18792835"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9916ac661c7919a65468d313a90cfe9e"],["/categories/大数据开发/Redis/index.html","d1c66799e615ff4bce74e2a199ac6ae0"],["/categories/大数据开发/Redis/技术/index.html","494e3c8ec238d662275f852b16638260"],["/categories/大数据开发/Redis/环境搭建/index.html","afa4936c1601477f5de0b4418516f335"],["/categories/大数据开发/Spark/index.html","efd790d87c12fff9b841ae4eb2fa08ee"],["/categories/大数据开发/Spark/环境搭建/index.html","3bc8ad8a31e8291c3b9c7fe191bed226"],["/categories/大数据开发/Zookeeper/index.html","4162d2be46ce9a961ec7138c98b46ee3"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","4136830e7408794d52e98a3a50e6aa8a"],["/categories/大数据开发/index.html","8c1ab7afdd88980440c0cc4c410b1a03"],["/categories/学校课程/index.html","7ee20277fee97345c61eeb895013b2f7"],["/categories/学校课程/计算机操作系统/index.html","7fc1846abfe8d7758c29b1b433b27f91"],["/categories/操作系统/Linux/index.html","08f3068da509bb45313bcd02f93ca57f"],["/categories/操作系统/Mac/index.html","de07269bb1a1112a67e76f27f739b1c9"],["/categories/操作系统/Windows/index.html","ceb7e90de0b2789219a762447351f348"],["/categories/操作系统/index.html","f21e305e8dc61d030f2b61c93186606c"],["/categories/数学建模/index.html","e4a504a971c1fd2d42ab6ea245c18c3a"],["/categories/数学建模/latex/index.html","9f4883fab6ce64cdd93705b6628b70c7"],["/categories/数学建模/优化类/index.html","b63165a3e9a1c70c936aa38984bf89e0"],["/categories/数学建模/优化类/现代优化算法/index.html","f4c7d574c646403e94afecfb29441ceb"],["/categories/数学建模/优化类/规划类/index.html","8f9e7a2f4ba2f15cb932f441789ec69d"],["/categories/数学建模/绘图/index.html","de7c8a4ab648a87f9be7f72b9d993d08"],["/categories/数据库/MySQL/index.html","cd068239112ebacf6ef2165314cbc327"],["/categories/数据库/index.html","bf9c6ba1bab3a96b7323ff086297980f"],["/categories/数据结构和算法/index.html","ab9d6ca468f421f3f32a3afbd2649b3f"],["/categories/数据结构和算法/page/2/index.html","e2bb57617d0cc37f7eda648af5079eb3"],["/categories/数据结构和算法/基本原理/bfs/index.html","d8580dfdda4ffd88d02eb718235ad75f"],["/categories/数据结构和算法/基本原理/dfs/index.html","bab0a3f958436ade453b235ec26eb5d7"],["/categories/数据结构和算法/基本原理/index.html","1e9a492f5237d684be8f2e4f1184f521"],["/categories/数据结构和算法/基本原理/动态规划/index.html","5bfcccffad26360955f53964e88c99b6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3aaf69f9e47a80faaf30d7ceb15e6029"],["/categories/数据结构和算法/基本原理/图论/index.html","4a1e20458d858fa84fa1240cf036961f"],["/categories/数据结构和算法/基本原理/字符串/index.html","2f2810795c5061ecb7c7db2e3efb37d4"],["/categories/数据结构和算法/基本原理/排序/index.html","5d05f39f1698fd550f825e1493194204"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","db6548de95381cb92d417e4b674b4c49"],["/categories/数据结构和算法/基本原理/数论/index.html","4dbf9f555ebeb4666a19546b0b304352"],["/categories/数据结构和算法/基本原理/树论/index.html","cb4c9c362e49dbfbd88229bc5a96d3df"],["/categories/数据结构和算法/基本原理/链表/index.html","4332037851179ca97b4c826688fe212d"],["/categories/数据结构和算法/算法题/index.html","534b8d5832b4aa6022d935bc4a07fa77"],["/categories/数据结构和算法/算法题/二分查找/index.html","3ed1be89e74e325c125fdfcb36f73a12"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","cf18c81009869a5954bbe4a44545b273"],["/categories/数据结构和算法/算法题/动态规划/index.html","859436be6683186862a4c5223d3635e3"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","9c9401ab77f1eea4ee4cbc43faffb600"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b9f3a830e571e4a513c2a97843ef9864"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","206badad3cfa7c046e7f99cddfff805b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","0a8bfe0fc2bb0c20405e613167edaa8f"],["/categories/数据结构和算法/算法题/数论/index.html","d5ed4709919886d51c06fe73c16e5c06"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d0c97f7293f35d3c9938ec7a3d90bd81"],["/categories/数据结构和算法/算法题/树论/index.html","ee3587569efeb36ac992e561b7999e32"],["/categories/杂七杂八/index.html","a02f1659e8655bc951ce9e4505e87702"],["/categories/杂七杂八/博客搭建/index.html","b5053212657f4f86e07a434a202870fb"],["/categories/编程工具下载/index.html","4507ccdfb0d5910ae753b954d0ce81aa"],["/categories/编程环境/index.html","36017599ef9a59da16132f53ecaca14e"],["/categories/编程环境/大数据/index.html","120ac93150af9005883a7b43f3af21cd"],["/categories/英语学习/index.html","b52e3abdd68762040f4a33ca92c82dfb"],["/categories/英语学习/英语语法/index.html","51726e69ad45737664b111a881acda87"],["/comments/index.html","15c8fa2d5f70ae7b5b01a97bfc6fb81a"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","bc31958af75a46acb20fc7ed6bd70934"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a5e8b0d19daeff1b32451f1c3de7bc86"],["/movies/index.html","a971a5314a0558561093661552e681cc"],["/music/index.html","ec73fdb68db66c5bd83a05d27b294c01"],["/page/2/index.html","8a30bc58b8ac20a915538a903e2fee58"],["/page/3/index.html","144d3e3afae182844756efdd6db0648e"],["/page/4/index.html","9c45a7bcde54c807739dff61622dfaf6"],["/page/5/index.html","3c6e041b0e3d87963c21c538546918c3"],["/page/6/index.html","016b6d20dac7f4517c58ce7f718725dd"],["/page/7/index.html","5b5029b6b4e387ce222edccf362a36da"],["/posts/1021360842.html","755c453ea02d9b6205895c247fdc64b0"],["/posts/1120620192.html","4405915a7eb59ee4b4e683afaff90c79"],["/posts/1137707673.html","470c380903330cb62b4f331bf6a0c4d7"],["/posts/1141628095.html","b070c4c2af9bea067215412f607b4b01"],["/posts/1168613674.html","990522474fa19ae9163363c2d24f91f4"],["/posts/1219920510.html","7904549941b0fefc22bfce3367dee19c"],["/posts/1222166338.html","2637d154dc42948aa0ff68a3dd2b3686"],["/posts/1259097482.html","caad9c101235ca8b204565b672bf2de6"],["/posts/1271036369.html","07b8bcd5e11433c82fc9efb8bd89526e"],["/posts/1312847445.html","5836bfdb136cf5381d9259c26e424817"],["/posts/135355774.html","cc0babf01ca5efb08a9fbc99a5feb80a"],["/posts/1375344716.html","b1b4951057455e6a7940440313a41fe7"],["/posts/1388991698.html","fabb2675af6f060d9b93bf4cc490a3c6"],["/posts/1410315814.html","5b122f10c4d97045a301417a81a28e9b"],["/posts/1452790229.html","f0442b93163bf4f0e930eececcc8334b"],["/posts/1470079884.html","e87eb8ebecd3dc83502a2ce1566f88b4"],["/posts/1470079885.html","db71d5d55bc69143a3a35ae7ab054670"],["/posts/1470079886.html","2a3004aac8bc5df4d8fd3ae0d37baf04"],["/posts/1470079887.html","11a37f244a9ea3640fbe98ec84a344cc"],["/posts/1498536549.html","daa8f7b1b6a262f451a7971ad5ff0fb4"],["/posts/1539568593.html","06cfeb7986229c382ef750f9f5a17da7"],["/posts/1547067935.html","cd66658e395d1752b39148da204e4ce3"],["/posts/1557866301.html","81338e483b1a365603f55c246d9f2980"],["/posts/1571776361.html","ea1d682bc8dadd7a27bb9bb46676e8f2"],["/posts/1605124548.html","2a540d348c5f6361aa1bc32483153819"],["/posts/1633036852.html","d7a5df42bf57ffae3c397b3242253a1a"],["/posts/1667740714.html","286f0ffe05b0b8a2aaf533e66de6dffa"],["/posts/1674202625.html","143cbfff08edd8b259e89361cffba604"],["/posts/1765123828.html","017b0e39bcd6f565b1e5062fd9757b6e"],["/posts/1767336200.html","92ebd93d92956a80d8dc85e793ee13c3"],["/posts/1776114197.html","00100e4853b818fdfcb9a738cecf471a"],["/posts/1817748743.html","8471b928efa60955699aa8467962b36b"],["/posts/1925125395.html","e04edbe54d130fdce3fdbb8671939c29"],["/posts/1966191251.html","28d8263fbe910bb76f6abcce37810285"],["/posts/1987617322.html","7a91c61797a7381f04887b0fb970b678"],["/posts/1999788039.html","85dc3db981c608160756183ca08037d3"],["/posts/2075104059.html","1d7feb1db6ecfefd0eba27e128c812f7"],["/posts/2087796737.html","755d98e90bdeda9b71f649a2971a4e7f"],["/posts/2106547339.html","7b6a6a633b412a25d18f61dda8fc74ff"],["/posts/2207806286.html","ea47ae41e65c083ddcabf1042d3d7a36"],["/posts/2225903441.html","0a6018755763c6679f26a7cf7911112d"],["/posts/2265610284.html","449dc01b6be4629ce87b92051ca1f07d"],["/posts/2281352001.html","83bbf95fc738e7b1a060dd58dd7ed973"],["/posts/2364755265.html","24178d13b2bc2ca4dd8cbd6ebbba40eb"],["/posts/2414116852.html","0f3d3a224fe955a2925a861cd02cd47b"],["/posts/2421785022.html","a0ec43c9e5a668d09ec93e003964be35"],["/posts/2482902029.html","d03fc3cc230cb6657c01d5442f088b4f"],["/posts/2495386210.html","f50b0f34fe4dc448eaada77134bc656a"],["/posts/2516528882.html","a4a31e853b9917d41cfaffb3bb676f4d"],["/posts/2522177458.html","5730c821e2abcd7b47028cef1cabf807"],["/posts/2526659543.html","d673bb40ebee86e7a3693e051905a25d"],["/posts/2529807823.html","b4d3bf562fdfd32153bec968eed7a256"],["/posts/2596601004.html","e6b40d823f7f9c87124c60b1de388e7c"],["/posts/2697614349.html","23a1abc3524de3b07fa4eaf4fb6fa690"],["/posts/2742438348.html","eaf215cc1052998b43760dfd241f4c73"],["/posts/2768249503.html","ecb72f756525df65a6031384a91eb29e"],["/posts/2864584994.html","11ec84c9b25709f9ae9664eb432122f2"],["/posts/2888309600.html","7082fb992881fffad17af1c31bc5b3b1"],["/posts/2891591958.html","522f1af06156d73e249657c5c1970684"],["/posts/2909934084.html","2520c9363d8e365bd6bb33f31780b190"],["/posts/2920256992.html","ddd1676fd61d0aec5ea22c9160f1592d"],["/posts/2959474469.html","37f6cc183c2f3d56d9ae54a88f31ef2e"],["/posts/3005926051.html","72956012ed3a2de5f517f734e11dfc54"],["/posts/309775400.html","eb52ac28aa99ee7d06630e2003403dd1"],["/posts/3156194925.html","dd10e9eccde81564f98888984142b7bf"],["/posts/3169224211.html","d231571382d0e67fd04f9d5db1ba1e4d"],["/posts/3213899550.html","f53d2e427eb40fb50d439e8db81dec6d"],["/posts/3259212833.html","01cf83265cad3ff2e0b796210d5ed5c2"],["/posts/3265658309.html","b3a1ebc20b4eeb5ddc1d665397b39cd1"],["/posts/3266130344.html","e0e5730e773b3461193f2c7d5cc46535"],["/posts/3292663995.html","ab2d06825438906ab9aa14087b64f130"],["/posts/3297135020.html","3721eb869f83ddadfd64fd687a0043e1"],["/posts/3306641566.html","1f1237f235ee859f2c3849da152b64fd"],["/posts/3312011324.html","66b74f2e649bd8fa90ae1a1fddcfc0aa"],["/posts/336911618.html","6ad65d2bd37e05ab65070aabbbb408a6"],["/posts/3402121571.html","7c79c50e8dacf579123e7adcb02176f2"],["/posts/3405577485.html","7f056f9ff3cd18aff0d78c2617ac2394"],["/posts/3498516849.html","0f9a3e5d49fb34b629fcc5c96bc44851"],["/posts/350679531.html","54742bec1f8b902f3e02e14a3e48bbe6"],["/posts/3513711414.html","2ef67b1f5450b5c333c43276656a9ca2"],["/posts/3523095624.html","c7fc2cd42c6785b28d71ffaf9d1582f0"],["/posts/3546711884.html","039075a5214929b740fdad2df928dcb8"],["/posts/362397694.html","3aa05866d13acf1e6993ec75c9b67dd1"],["/posts/3731385230.html","746447b1d8178243483ab564f53d2383"],["/posts/3772089482.html","476536c3992b4faa376be13643ed7d47"],["/posts/386609427.html","2d3f57cacacaae974e9d2eb754486883"],["/posts/4044235327.html","7075526098080cc73e421778dd4bd9f8"],["/posts/4115971639.html","2253031a722165d69359ff8d470e9ae2"],["/posts/4130790367.html","8500d26322adae743503f40a20284440"],["/posts/4131986683.html","8578cf18aec2c8472036d7da53eb4367"],["/posts/4177218757.html","afa7201d8fd6b1ecda33c793389c1918"],["/posts/4192183953.html","a82931c68a07ecc56f4f8de96ca54293"],["/posts/4223662913.html","9f72dbf31c7403b61f3cfb82f4e5528b"],["/posts/4261103898.html","7da60cfb325f8b4fd3bc8dfdfaf18fff"],["/posts/4286605504.html","0a06ddef3d510bff64cc2042e807c9b8"],["/posts/449089913.html","19f683f4bd50616fe2dd2aad64cfe5af"],["/posts/469711973.html","820563ae5c25c9bdbb162c7fef82e8e0"],["/posts/482495853.html","dcb1c5b6094530ad20eac5a49b207491"],["/posts/488247922.html","1b0d1acec48b1579e26b261f1c8d7d85"],["/posts/517302816.html","253537056c6e53f4fadd7eb97948aac1"],["/posts/570165348.html","a771f78e0a7d4816fd65d1dc4e22eccc"],["/posts/595890772.html","2b33c899ca39257790005958e1fa90af"],["/posts/67485572.html","f14726d9a749d62ba82ff9d3320d4235"],["/posts/694347442.html","c1ff12d51a2a14c53a80790529f5579d"],["/posts/707384687.html","668811d288166f9ea6e4b092c0353837"],["/posts/71180092.html","a9543059682ea3a7457c4088d9dd9b3f"],["/posts/716459272.html","1c2e42a94a2312de94964c069e0ce088"],["/posts/765481613.html","f5e76a7d7a3bc57662a4abe0d5d45333"],["/posts/778231993.html","0239a7d78c9fa04c16f897b3667599be"],["/posts/795397410.html","230314ebe60fd5a5d7c6429b6feb2a1e"],["/posts/820223701.html","160386b835833e5ab352fb8320f32475"],["/posts/830372185.html","f8cd60969728d419e92ddd895da15d0c"],["/posts/88294277.html","8999a367eecf76f5a56cd59a412a7832"],["/posts/939963535.html","bf476c7d6417bb502a30d48a3ccad417"],["/posts/983786067.html","83eb7c000f5c0976d54f4a91425b0de8"],["/sw-register.js","f6d36e468ec66ee0e463a6f7298c6aac"],["/tags/C/index.html","f52f4b02725efd855bf011dec203e7c3"],["/tags/C/page/2/index.html","137eef4b4df39b48b78f838bc1d486a8"],["/tags/C/page/3/index.html","647c71de19dc9721e1fc053ab3d5998d"],["/tags/C/page/4/index.html","d97f80bcbdba1331c0f19cbd4b325cf5"],["/tags/ETL/index.html","75091c610992606207d8ec012ec50879"],["/tags/ElasticSearch/index.html","d8c983aeaf6fbf4e07516bd207a62742"],["/tags/GUI/index.html","c80832024dc79d7117bb121ca2899971"],["/tags/HBase/index.html","24e03cc54384ccb89451604c0afba1b5"],["/tags/Hadoop/index.html","85b60bf7a28acbdad1b53f4bd2802233"],["/tags/Hadoop/page/2/index.html","c89be988887e4bc036c88165612a1310"],["/tags/Java/index.html","2a620c0445c9b0c91cb513faedde6677"],["/tags/Java后端/index.html","c8e0b7786af0ca38853312de59563d4f"],["/tags/Java后端/page/2/index.html","0335378995a24aa8c3a65cf8e0336000"],["/tags/Java基础/index.html","3ba27f6bc8e68fed392b987e2098d5d0"],["/tags/Java基础/page/2/index.html","fda38970ca20776c751f1c0b656a61fa"],["/tags/Kettle/index.html","c671649e6dd3b678e08c3fc43c2aab6a"],["/tags/Kibana/index.html","e92a3f886bd7c3efa1aa5f0b792f82e0"],["/tags/Linux/index.html","603f16ec1d757319b9da7d68410d9905"],["/tags/Linux/page/2/index.html","8dcb7b51ae747211ef9462f23eaf6cdb"],["/tags/Linux/page/3/index.html","8154af587a6b3fa463b683d34ec30adf"],["/tags/Mac/index.html","0a75e292b002d3007dcfe9fa2143105e"],["/tags/Mac/page/2/index.html","0bf14a06cab8646c61516346ed227a93"],["/tags/Maven/index.html","1608687e572bab279c5d576e8cd47260"],["/tags/MySQL/index.html","8601615d49f4d30411b4c03aac98d9d8"],["/tags/Python/index.html","47e7a089436b246094d29bb2f35e946c"],["/tags/Redis/index.html","151520199d9c76d037032b5617280d49"],["/tags/R语言/index.html","5cdafa307f524a92857eea3775aaf59f"],["/tags/Spark/index.html","f2f8a88cdce4c58eef5b0c47ccd635bf"],["/tags/Ubuntu/index.html","a3afae80bee1e456c5b0e3793f3b7072"],["/tags/Vue/index.html","53a7ddb74d1d0c0d1fa4a8cae44dbab5"],["/tags/Windows/index.html","139a554eeef4f3b0bff061ab8164a813"],["/tags/ZooKeeper/index.html","e05750ed80f18e84a66f3b14b048527f"],["/tags/bfs/index.html","64c7edee9a19d53db2e0a190873a361d"],["/tags/dfs/index.html","bb5e6276023282fd95641bb9b966d996"],["/tags/folium/index.html","eeb8eb80c054525d909734d49dec5a65"],["/tags/git/index.html","0e8a30fd46b58bac6ad4578c96cbf6c8"],["/tags/iPad找电子书/index.html","7540a10434d3f6d8cae4a15c87c3fb62"],["/tags/index.html","1886209dbb976756476001a9f17c35fe"],["/tags/latex/index.html","cea805a03a0fc0948efd82acdefc897b"],["/tags/中间件/index.html","340d8954d8245bd5253ca9cc5c9a9dee"],["/tags/二分查找/index.html","a483b6919dc11148b27739ee0d5a735e"],["/tags/优化类/index.html","c05ccdb16ab7a6af68e5f7e8f28fd5a4"],["/tags/前端/index.html","0822a904fc292772e65164be8ff134ef"],["/tags/前缀和与差分/index.html","37ef5b23dc0cee5f2ca62e2df723354b"],["/tags/动态规划/index.html","b7dc09eae6599c3e8b31f8bb5cf3ba34"],["/tags/动态规划/page/2/index.html","9f2abe68a676821c48f58f9057ea0403"],["/tags/博客搭建/index.html","92a2a568fcc846be7ecb2ceecb54ac89"],["/tags/图论/index.html","6e5faddd4420ecf45da11c55ed4706bc"],["/tags/大数据/index.html","046b99748eb895572352f61ffc8960d9"],["/tags/大数据/page/2/index.html","c2a288ede421dab0464d68e929c5aab8"],["/tags/排序/index.html","e63815774d52450bd74af75d85d18ee0"],["/tags/操作系统/index.html","6dca8d5ec4dbc2dd70f38d0a9869bef8"],["/tags/数学建模/index.html","7747f23d952db2d2d2a18280b60a7fdc"],["/tags/数据库/index.html","0841ae944501872da86e997bc7b5e15c"],["/tags/数据结构和算法/index.html","0053325b990cfe9956d6341e1863d27f"],["/tags/数据结构和算法/page/2/index.html","3ec914e3983a49e4d0449d1c8957dd5d"],["/tags/数据结构和算法/page/3/index.html","af6942ac8af3ead9d1aaa9db9bd1a0a3"],["/tags/数据结构和算法/page/4/index.html","f01b0261d39308844d98fa3c738e74d1"],["/tags/数据结构和算法/page/5/index.html","8866ea380c1e4cf59306c5a416cbd395"],["/tags/数组和字符串/index.html","3316bf827c991169c8c7d2c58dc648c5"],["/tags/数论/index.html","6fe13da3cff93183f115c98f7f611992"],["/tags/枚举类/index.html","c4f779d9db1ee8f52798b0afac4da4bd"],["/tags/栈和队列/index.html","047a1d7036d554343722403f7b3a9671"],["/tags/树论/index.html","aca2737e296e0539903bcc54f14066f8"],["/tags/测试/index.html","633659a6f7d1c0613407d010d82385d6"],["/tags/环境/index.html","9163f96d517ee068b86e4eb6faaeef12"],["/tags/环境变量/index.html","d89680e15604a60da2371770063fd5b3"],["/tags/绘图/index.html","c3eef762d5ca0798e7c5e954f1b3d810"],["/tags/编程工具/index.html","3eab5641ed61ae5be6a4535b4f91a49a"],["/tags/编程环境/index.html","885a341ce1777f257278b41e11c0ec3e"],["/tags/网络编程/index.html","2152321ddb36eb90dd86ae6a9ec96c3c"],["/tags/英语语法/index.html","82351a3684f44ebb737b4497454754e9"],["/tags/计算机操作系统/index.html","0ab8a355b1a8d7b909648e62738e7781"],["/tags/论文/index.html","0dd733ba662baa5af5555c9bf899d573"],["/tags/资源下载/index.html","f10af827c98b9d9fefba4e51aa3aede8"],["/tags/链表/index.html","c33d5b99fd47a45795b59cc27b40a3cf"],["/tags/集合/index.html","c05936d5a67bd5f1b57c382f237e8cde"],["/tags/集群/index.html","f09ff03ce2c257b68ee16ce6a8acfc0e"]];
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
