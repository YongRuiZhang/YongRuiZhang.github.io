/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","12df442c297fdaf38996b74e08d5b83f"],["/about/index.html","8b27792c62c63312f6cda047e50d3e40"],["/archives/2023/01/index.html","9b38c642e10ec8c8b1f1ae07ebcb323c"],["/archives/2023/02/index.html","cb17eef9026eaf2499f8501fb15c4a5d"],["/archives/2023/02/page/2/index.html","9ed49ac337f7f818813363a480cd408a"],["/archives/2023/02/page/3/index.html","47fe7c996b4b30688620a5d27e008ab4"],["/archives/2023/03/index.html","bc615c26c1cd72a8e3ce048ef53c4131"],["/archives/2023/05/index.html","8ea6d2ee9f957afd10fb77b8d4c484bc"],["/archives/2023/06/index.html","e36527c67e2ca570bd98d14b40f13b4c"],["/archives/2023/09/index.html","8e729e6af0ce416474c846af860628ef"],["/archives/2023/11/index.html","dbbe2a69d0c93b86e9d37e74acdc06ee"],["/archives/2023/12/index.html","867cb11e6bcd87f8a87db86c249f0a5a"],["/archives/2023/index.html","06b4b29b1deedd1e7bac2fb61308eb6b"],["/archives/2023/page/2/index.html","46379954d6a790a03e438bedd69eaf40"],["/archives/2023/page/3/index.html","f6d96cd6a29c95e1fa61b7a07936c286"],["/archives/2023/page/4/index.html","13404dc28532237711cb7f07cd125a14"],["/archives/2023/page/5/index.html","25d467988fb3ba3dfc1a475823ed2bf2"],["/archives/2024/02/index.html","9e9ff37be4af02fdd96f4155b20fd75d"],["/archives/2024/index.html","c2c58b5ecd2c039cba1022442fd2aea2"],["/archives/index.html","817a4f49b1970f6e8e5381fff8c2105f"],["/archives/page/2/index.html","a7f4899237a5ee751c4257df0f839b63"],["/archives/page/3/index.html","92f2322fb54db17927da4305050bb0cb"],["/archives/page/4/index.html","b59c681ae62c58765346c60550af3aa7"],["/archives/page/5/index.html","0c49ffe4e2c8b2baa8f0bfeb7190ef2d"],["/baidu_verify_codeva-qQP2iZOMLX.html","11e5fa99c316e8962b26ad6d3f6a3ac1"],["/categories/Java/index.html","fc877747314599fbd5d8ad0273024ca6"],["/categories/Java/后端/index.html","e3cc541e7626e1f57a19801f12cedb45"],["/categories/Java/基础/index.html","65717e6ee0a570c99fbb58d3ae23c200"],["/categories/Java/基础/集合/index.html","0680370d4af00c61715c7b5118fa1117"],["/categories/Python/index.html","e71d44fffc8c6c9f9f1d3efd02531cce"],["/categories/Python/编程环境/index.html","cc8304f68c5a45b6149446f85e26a012"],["/categories/R语言/index.html","859d4f5175f9b4063e97e0471dfcc954"],["/categories/R语言/编程环境/index.html","d142f3a9f1ab9600069322eccff65f57"],["/categories/iPad/index.html","d6e2b2fe5601aeb2268374bcf5a9a929"],["/categories/index.html","4f22613fc0ee8d04779ffb94c70ba029"],["/categories/中间件/index.html","116e267db54946212e499a97fdf0ccc0"],["/categories/前端/Vue/index.html","a40a2e9f823c8b3b8eccfd04e241f07d"],["/categories/前端/index.html","154517af93fb9f8de8551fad0cc8fe2e"],["/categories/大数据开发/ElasticSearch/index.html","8ee3adff3540edac10950291edf77c1e"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","40d14824b0e5a7d8ab22abc37880a04b"],["/categories/大数据开发/HBase/index.html","d7195e8eca08a3afaaa2ab5200313bac"],["/categories/大数据开发/HBase/学习笔记/index.html","1b8108ed1074199166d929db4103063e"],["/categories/大数据开发/HBase/环境搭建/index.html","69047fb0ea9ad605623798a0b57f4c90"],["/categories/大数据开发/Hadoop/index.html","975ba39ce301a8f2956c8dc3ccca679e"],["/categories/大数据开发/Hadoop/技术/index.html","2e585a09dea1db1172303c7fc3198ea6"],["/categories/大数据开发/Hadoop/环境搭建/index.html","8594f233ce1cd1bb6b61b2f6be9dd0b6"],["/categories/大数据开发/Redis/index.html","5c8bdfe2092ca393f133a6bc405f2ae9"],["/categories/大数据开发/Redis/技术/index.html","b8d5239fdd0278808324cebf75be70ba"],["/categories/大数据开发/Redis/环境搭建/index.html","f94ce1a8cdefe3b12f905b273f37fe19"],["/categories/大数据开发/Spark/index.html","5a825393e315671c8a3237f3782d5d76"],["/categories/大数据开发/Spark/环境搭建/index.html","8b4adbe753ae4cacbebb2ccab2f12b59"],["/categories/大数据开发/Zookeeper/index.html","0e1f059cae98312cd3d2bf4b37fed0dd"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","7870419a8f7dee66112077056bdb9774"],["/categories/大数据开发/index.html","2d8cf2a1881796aa3b702faec7e1a87e"],["/categories/学校课程/index.html","ac173a9243f319d506f605d04920b1fd"],["/categories/学校课程/计算机操作系统/index.html","196341ebd2e3205fa65931765982b78d"],["/categories/操作系统/Linux/index.html","c33210f5441b9730382ab552d33b944c"],["/categories/操作系统/Mac/index.html","32fda3db8cdef998b6e5d04b0d74b93f"],["/categories/操作系统/Windows/index.html","9ab2db41927143a0beac524ebbf0a678"],["/categories/操作系统/index.html","b270521cafe74ed269b8328e8a59eaf7"],["/categories/数学建模/index.html","2401a33c0edd952cbf765a07b0a38994"],["/categories/数学建模/latex/index.html","577b2e3eb5203322a5afb4389c5b7ec8"],["/categories/数学建模/优化类/index.html","576e4211e624335bd97000e7dc065f9a"],["/categories/数学建模/优化类/现代优化算法/index.html","f341fe2b8c13fb25e22a42b5fc33cdc4"],["/categories/数学建模/优化类/规划类/index.html","7e75c5da104845fd65f4641d8982d80d"],["/categories/数学建模/绘图/index.html","bab407853c63eaa7fed6e0f85ae7a19d"],["/categories/数据库/MySQL/index.html","7c86457e6b840404ba890b37e0aa6a79"],["/categories/数据库/index.html","6aefb6df547783f17ffd4143dedbae3c"],["/categories/数据结构和算法/index.html","4f7794cca80504b9ed7b01da86776021"],["/categories/数据结构和算法/page/2/index.html","2a3d72a637abbfc76568349ec1ef40fd"],["/categories/数据结构和算法/基本原理/bfs/index.html","d458196df7697dd63fdcc063b8660f69"],["/categories/数据结构和算法/基本原理/dfs/index.html","f6d832e7f49e2e6386b0e822657f1220"],["/categories/数据结构和算法/基本原理/index.html","3a5b1aa1b90d1338cae406f469537e39"],["/categories/数据结构和算法/基本原理/动态规划/index.html","68353c25639532f2c5e3a3f10f095c56"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","8aaa846111e3ba5929e5f7941d22b463"],["/categories/数据结构和算法/基本原理/图论/index.html","21e42806b43310631f5f2462031241ab"],["/categories/数据结构和算法/基本原理/字符串/index.html","f5d616351372e5645dd29928283cd749"],["/categories/数据结构和算法/基本原理/排序/index.html","5e8d662a8d32491f5934f3784c52a3f3"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","11e1f33acdc2de5ab802d17e7a4b1bda"],["/categories/数据结构和算法/基本原理/数论/index.html","a8c96f93da8f5c2a9d293ac2f36f2c70"],["/categories/数据结构和算法/基本原理/树论/index.html","b5692fd13198c3d539c5f990a35282bf"],["/categories/数据结构和算法/基本原理/链表/index.html","f3a0b28b42d85a31da516a2c47138963"],["/categories/数据结构和算法/算法题/index.html","eaa19ab70328a1b9530cc2d7ec624187"],["/categories/数据结构和算法/算法题/二分查找/index.html","8f7e2b6e7390cfed2b67cf696c311929"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","e3d7223fa4cbb264d35b4e5bb0b7b071"],["/categories/数据结构和算法/算法题/动态规划/index.html","1c728687f9c1a96bb05a69ffcfcd4a6e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","1579d8ad5975071ae074dcd23c358eb1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2df3506c0be7de23a140e4837dfe041b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","172a0fdc3398edfd99419a7aa12733c9"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","dc075d06d7718dafac0e76b2337c863f"],["/categories/数据结构和算法/算法题/数论/index.html","1d3c91960dd9297456de8d6ea9757ffe"],["/categories/数据结构和算法/算法题/栈和队列/index.html","e9475094c253e84da8bd8715ca036683"],["/categories/数据结构和算法/算法题/树论/index.html","af6544c9d9ee12acaed7f791e181b574"],["/categories/杂七杂八/index.html","603d034d9b85006d125fc7125c1372ba"],["/categories/杂七杂八/博客搭建/index.html","729a06081a1f97b9f90f125ed6821ed9"],["/categories/编程工具下载/index.html","0949c4eecd2071045798f16db055002e"],["/categories/编程环境/index.html","c94c54e7683a4eeadb79a6dc61c48efc"],["/categories/编程环境/大数据/index.html","44f90475bd8be18ffd4b6847086d4d5e"],["/categories/英语学习/index.html","35f417788d2a1597133e55592ec02b05"],["/categories/英语学习/英语语法/index.html","c1d2eb729ea9c245b6e5afb2ef787daf"],["/comments/index.html","bd3cf01162a46a5531046de37c6643e5"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a14af0dbf2dab737753a090d63512492"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","560b5c23dd7ccb49613e442038629cb3"],["/movies/index.html","6b8eff864fbf4a5c960622d7f53d21ce"],["/music/index.html","5eaad5fdeb74da909f11a6b645e029b5"],["/page/2/index.html","1650c8c1fba5af2186c1b3da270101c2"],["/page/3/index.html","4e693ee85e2d3ed2e2e752b033f3f93f"],["/page/4/index.html","4c1b57d0cedd8f470a940457c728059a"],["/page/5/index.html","fb3af963ab3be53357eb023267913c45"],["/page/6/index.html","7f94b14ba9678056de4de07e515405e9"],["/page/7/index.html","780806b286e83bca6e0102b991c50b08"],["/posts/1021360842.html","4a303439162d73983f364f8727f8b408"],["/posts/1120620192.html","4db92fcab425900fa5c091022d639050"],["/posts/1137707673.html","b9a37a30dda66552cf6dc044af3dd0c8"],["/posts/1141628095.html","778a0a7aa554f1494c92eaabf395456e"],["/posts/1168613674.html","eb0761c02d237b857027ab512e84e181"],["/posts/1219920510.html","745bdd6f772735dd88e7c6b862ebb381"],["/posts/1222166338.html","63273aad7dfe2be9f19710eb239c1931"],["/posts/1259097482.html","6ecdded6be8407817bad45458792db08"],["/posts/1271036369.html","ed8cdaf663360f1e39615aa5c2a45fbe"],["/posts/1312847445.html","e7494620adb1f7a17a6c9505e19ffc43"],["/posts/135355774.html","3b6897d7a6c9681ade94c8248c49daf7"],["/posts/1375344716.html","727d73bc4f91936f4265e6ebc2249ba5"],["/posts/1388991698.html","c4704d467e6bd92eba83a4c1daf4751f"],["/posts/1410315814.html","aaeeb8e1da067145453bbe45fff3da2f"],["/posts/1452790229.html","4517630b3821830eee2e59127eaa2888"],["/posts/1470079884.html","802be2a4e04281f31f326b4e5d273fb4"],["/posts/1470079885.html","1ad0e201c52dca2ba317f2413ecd87b1"],["/posts/1470079886.html","3db741cf3578b3b72586e873b5b1a391"],["/posts/1470079887.html","d9330ad3057d7cff5ed9e9a3d1b5e94c"],["/posts/1498536549.html","aca5cb30e42fe4cf6877a9dcbefa2ac2"],["/posts/1539568593.html","e2d4ad39bd8c25099aec6449300795c4"],["/posts/1547067935.html","99c97eecc72b3d8656b66eae1ae5adf9"],["/posts/1557866301.html","5b21b803ba986100e83ca6266991f9ae"],["/posts/1571776361.html","a657efbf30fcadd3f3c33fa9524ecbdf"],["/posts/1605124548.html","4a78f0ff88c57c167db29f76e6021da1"],["/posts/1633036852.html","c2e934dce805c9e3eaa2606455796b4d"],["/posts/1667740714.html","a7571b8beb8bc43f467e2a3242427d54"],["/posts/1674202625.html","c4d60fb7d14172caf8a47c6ee107bd7c"],["/posts/1765123828.html","6d0b9cfa9d3d269fe9f829463842f73d"],["/posts/1767336200.html","581fdcd108a972389f14b2e8140a5375"],["/posts/1776114197.html","77cceec8671c448c7ae2fddfffa8b9f4"],["/posts/1817748743.html","a7d72ada96dd798cfd53b6a5da954285"],["/posts/1925125395.html","be8f6c1405c04c38a616a767b4cea697"],["/posts/1966191251.html","c3e61bfd3ec52cb8c17bd9406c21e7ee"],["/posts/1987617322.html","42d9ae7696b73fcaa9859e0294246a0c"],["/posts/1999788039.html","ed16f3465f36b2d5fa398d127bec17ba"],["/posts/2075104059.html","bf6b35138aa17fce3d9c4b33f1d8c3e3"],["/posts/2087796737.html","6c95531917788e8424d0557461b1d016"],["/posts/2106547339.html","ba3e0635e84a1cdf987e1bacb1a5e4bc"],["/posts/2207806286.html","797169f1f423f978dee6c1e41335f075"],["/posts/2225903441.html","e412eebe31650cd16a3687891c52ffa0"],["/posts/2265610284.html","78e2c36d56785f7196150429b54cb1ad"],["/posts/2281352001.html","3d5ecbc76e162af35e6a7cbc4f15369a"],["/posts/2364755265.html","f88d5a7eddee7f3b11f0b7b386a90693"],["/posts/2414116852.html","0468de4c3ebb603077f4f1b01590623b"],["/posts/2421785022.html","5f65796dffbb22205eea2f72ff279a2e"],["/posts/2482902029.html","16da3ef1d261ae581f63cf509903d2da"],["/posts/2495386210.html","af528a0dff44c07aa312e738d2b9573c"],["/posts/2516528882.html","96d8470a162771a2005715027ade6cee"],["/posts/2522177458.html","62ce9389f960bc1fd7c6cfe0db27792f"],["/posts/2526659543.html","3f866dd7bda640acc084e9fc368fb65b"],["/posts/2529807823.html","616b28ee180b9564ca56cf7e7ca3ea1d"],["/posts/2596601004.html","559063c3483c7ba84aed53a443520c59"],["/posts/2697614349.html","2047d45f7c51116f4a2d722137cc2ef5"],["/posts/2742438348.html","b86650a628eabecb3804c9c0c49b49e3"],["/posts/2768249503.html","3b1cf1021ab9799441fbf247ea7dd160"],["/posts/2864584994.html","276c51524fde13a6cedc8b74640d049f"],["/posts/2888309600.html","a336b29e3c637753252d8509d1419212"],["/posts/2891591958.html","ff77720f6c8a87585866d53d538b0b65"],["/posts/2909934084.html","bc71d5143ac7205547af3130e31cbf69"],["/posts/2920256992.html","064c75862f204432c79cea7bee01df19"],["/posts/2959474469.html","bf11dd658d04ae23725f6b06810845d7"],["/posts/3005926051.html","59eda30183aa3205bd3ebe3f710073cb"],["/posts/309775400.html","c108a57978c8dd70e51c4b05a373b11c"],["/posts/3156194925.html","2d2aef9c3eaf59c4f7c38ded90083e7c"],["/posts/3169224211.html","e664a456a010ae852e643be3c6f1f9fe"],["/posts/3213899550.html","87b0f6b8f606c5de5c23a8578e3045f8"],["/posts/3259212833.html","f2c033d2e81db96428c112752bc1d8d9"],["/posts/3265658309.html","e698a6007c94742521ad951ad3f8c03f"],["/posts/3266130344.html","9edb2b7b4c25f7ff34e76039181e0d8f"],["/posts/3292663995.html","770e4f6235702b95262e83e1f5f83533"],["/posts/3297135020.html","d8c6ce8639c7d62f7c3389ed29a6172a"],["/posts/3306641566.html","397b35d38dfa63955e684e1fd4d6e97a"],["/posts/3312011324.html","df04e685695e1c1b2c2a83e63e563501"],["/posts/336911618.html","292b0421dd22c9132f82c72297e803b1"],["/posts/3402121571.html","ec01e4087df4da9a8b00475fe634b15e"],["/posts/3405577485.html","8ace064b0539f3b2bbae2551132d2136"],["/posts/3498516849.html","b35a54b48b69fbed3c1ca7f22bcae9e9"],["/posts/350679531.html","104cd9edd8d521eb43fc0ea33b7286e0"],["/posts/3513711414.html","784b140ae5c2825b5748f18cafb43375"],["/posts/3523095624.html","e27af1924642f3d8372d9f05e0f4ae55"],["/posts/3546711884.html","83c9cb29042a1ecaef960609076857ab"],["/posts/362397694.html","959e7b0eaf7dff33085d79b9d91d6371"],["/posts/3731385230.html","268c64ffa8717e89f26fcf93da59d0a6"],["/posts/3772089482.html","f5a439a7dede2886ae5ea9ed5c83cff2"],["/posts/386609427.html","acd4444d555152795539ee090ef2c465"],["/posts/4044235327.html","4c2b3e5663e5dddcc4f301cac7033ccc"],["/posts/4115971639.html","10440f02c0b8e076e5862a2ab90da65d"],["/posts/4130790367.html","9c7a692eb3df73076effb56bd14be6ac"],["/posts/4131986683.html","802e9004f1db722bde8101d409f9e65f"],["/posts/4177218757.html","aed2bbfaf84c63f769baf86317d26911"],["/posts/4192183953.html","6809a4087932b0bda82b0bc15255d3c0"],["/posts/4223662913.html","535f8b2aba717b721a01ae3e968e4680"],["/posts/4261103898.html","de9e4559f5f4a555332dba86b04a595f"],["/posts/4286605504.html","521c84c9a483de0f443f1788e315552e"],["/posts/449089913.html","178a880c3f96c76bb56f8f1181b266e1"],["/posts/469711973.html","1708dde0898e7c2977f6aa214b36e0df"],["/posts/482495853.html","9765d81f5c964a2507f75712d69b5f4b"],["/posts/488247922.html","afaa504952af3da286d5eb72aaf33fae"],["/posts/517302816.html","39de0a05bb8ce0e0d29ba19f676955a8"],["/posts/570165348.html","a9a5e83ad66ce45c910dd2270fafce51"],["/posts/595890772.html","09ec21f5642a5112d1a568a36ecfbe67"],["/posts/67485572.html","deea8e91dd6b990ad0313ac05d3bc0f9"],["/posts/694347442.html","c54821360a5999e81537a4489d26d88d"],["/posts/707384687.html","98a1e75ef6bae768d412315ee8c46543"],["/posts/71180092.html","5ed426b5d4a29115e7928372e4b67cf6"],["/posts/716459272.html","1b6d115b0bfe20ce2e5f806c24fa2625"],["/posts/765481613.html","9ac0a2f56b0b1721baa8d74209d97f22"],["/posts/778231993.html","77e692d83aa590d8bd12c7b70051e589"],["/posts/795397410.html","8fcf8c8ef6334c7778e4bdc1a8502d6b"],["/posts/820223701.html","b92a4b3ebe08793b0468066fb4490c80"],["/posts/830372185.html","e134e7983287b0f091f5ee8cf2cdbed7"],["/posts/88294277.html","dcae10cf47d48ebbcb0b33b1f12f5c82"],["/posts/939963535.html","9a53f574c9f18fb2581cd1e0644f3577"],["/posts/983786067.html","406eb4e5545fdd956b8e60348f91363d"],["/sw-register.js","3f8883dc75509bc5b13b485977f1d3b1"],["/tags/C/index.html","6f54240c556a1753d03c47dd5d1c7264"],["/tags/C/page/2/index.html","c84f62c0e920319071f0dd352b5fd044"],["/tags/C/page/3/index.html","76582c8f969e57b4f62a6eec95a03d2b"],["/tags/C/page/4/index.html","0162d77dd92f0febf85d2e72655b9b05"],["/tags/ETL/index.html","0dbbbef683770f155bbe5008dab62ff2"],["/tags/ElasticSearch/index.html","ccb9011843e452d6056d4854104f3476"],["/tags/GUI/index.html","d115ec85a5e7dd07a43a0beadf458aa2"],["/tags/HBase/index.html","47ee8b480def7283481026dad5a95cdc"],["/tags/Hadoop/index.html","223857b399c71d53f35482df731868df"],["/tags/Hadoop/page/2/index.html","7b7e7b5eaafe96152bc3771bb57118c2"],["/tags/Java/index.html","1af29df275cca0689865c5403103c8d7"],["/tags/Java后端/index.html","1c5d606d027e4202e6f71d6610c2da77"],["/tags/Java后端/page/2/index.html","cd3a5f11cb0e72d64a311f8fdfcfe173"],["/tags/Java基础/index.html","f60be8a13df3b2986bbd1d58eacba451"],["/tags/Java基础/page/2/index.html","c45a981d19c58008ae8e82b980280766"],["/tags/Kettle/index.html","d7cabd91088a5da7c61257ba72c7b8e5"],["/tags/Kibana/index.html","96f488c85df17e0acf6c9786d4c31642"],["/tags/Linux/index.html","63d58eb9aae788b3bd3101f0acfe7e9b"],["/tags/Linux/page/2/index.html","65a413a835d1fdccae52d68bb3db7438"],["/tags/Linux/page/3/index.html","1d4a580ca0623a01ef025709cb460e40"],["/tags/Mac/index.html","068cfd64ce3139c4e7b516936362253f"],["/tags/Mac/page/2/index.html","cb0c104aab182681305a591b42591d83"],["/tags/Maven/index.html","9d63881ee8dd1bc7bc449b4a105a763c"],["/tags/MySQL/index.html","88d11d785a13c00b8edcb48b36e46aab"],["/tags/Python/index.html","440170f57a485e1269b17d64bd93f910"],["/tags/Redis/index.html","d48158464a13b85b498bbcc2c927e059"],["/tags/R语言/index.html","c98012bf34223d841ee72a002e7e1632"],["/tags/Spark/index.html","e0cb067e308cdebb92f6d4c39dfbbdc3"],["/tags/Ubuntu/index.html","bbd794a0de11a6cc0daf2f609602ae7a"],["/tags/Vue/index.html","ba673268ca894647dfe24b1d3ae324fe"],["/tags/Windows/index.html","d7b0af6804a88060ca8f84979e266ced"],["/tags/ZooKeeper/index.html","ecf7392068f0442a25497be5a1d4626e"],["/tags/bfs/index.html","1031d971da6e6b3015cb4389284e0166"],["/tags/dfs/index.html","249daa150cd20181a605dd6a2aa12159"],["/tags/folium/index.html","5e53be9a5a5c70d2a80af5088073c244"],["/tags/git/index.html","f3fe859c1104772dd5d4cfad72afad42"],["/tags/iPad找电子书/index.html","b79a2a54934c80c1630c5a3c7c2e6117"],["/tags/index.html","bce9929953410df1ca8b7ca516741d00"],["/tags/latex/index.html","1671bcc4172f2dae2ecc669eb549d644"],["/tags/中间件/index.html","d3ce4337d50670cd10db5363ef065265"],["/tags/二分查找/index.html","fb588c53ae042866d7b6be8d470e8a66"],["/tags/优化类/index.html","2f5620c6dcf8441d24b981e0665960c6"],["/tags/前端/index.html","369f9a3ef69d05c8da09c9f448501597"],["/tags/前缀和与差分/index.html","45d515be680be6f7bb81a87c45f4dc9f"],["/tags/动态规划/index.html","b8e74807740e519fe47787292f3592a2"],["/tags/动态规划/page/2/index.html","e9a866b57a0f58764973e69385ecfd14"],["/tags/博客搭建/index.html","c1bedd761cb2f699cdf3065447802d20"],["/tags/图论/index.html","e452bdc043f71d84dde7832cdbd18a93"],["/tags/大数据/index.html","6ba087618a0a906bb55a75e169b9e01f"],["/tags/大数据/page/2/index.html","ad3c0f726c35d8815c8554693ef359be"],["/tags/排序/index.html","888ea7fd77c62fa0b78447e55cb90e2a"],["/tags/操作系统/index.html","e45fe3cec130ddfcbe1816dd6c9dc517"],["/tags/数学建模/index.html","4c65fb4d114e7c3a530342a9d62dfba5"],["/tags/数据库/index.html","b9fb677fe897cee71e2566a779c8538d"],["/tags/数据结构和算法/index.html","8f587da86b44a24410edc82a5f4e25c1"],["/tags/数据结构和算法/page/2/index.html","49ab20b3c237828cd446a36f966fe08c"],["/tags/数据结构和算法/page/3/index.html","f272c4f1689c1e82a3c5b641c150575e"],["/tags/数据结构和算法/page/4/index.html","841476bf145333dcbf6bdd3d8e25365b"],["/tags/数据结构和算法/page/5/index.html","7b6476bbb730a8e1ee08958eb4a465db"],["/tags/数组和字符串/index.html","51f14ec30269ab700c9c641a8d7d5b1e"],["/tags/数论/index.html","abc88c8a5519e40b8f8476ef9f073b0d"],["/tags/枚举类/index.html","1b57977669e1548ccabfd013c4594741"],["/tags/栈和队列/index.html","db0744e19fa019fdb450f33d608e73d0"],["/tags/树论/index.html","47b01ac94a5735837cf3da6cc646a633"],["/tags/测试/index.html","6656fd9b8409fd93f7cbb6e4935e8204"],["/tags/环境/index.html","f8e1348a61faeb67fc41be806a294641"],["/tags/环境变量/index.html","d800ba582feebd55c4cc38722370d25a"],["/tags/绘图/index.html","e3a83b8db5336e17687c45ecd01b82d5"],["/tags/编程工具/index.html","9782226aeb4b8f62754b3b3e04f03ac3"],["/tags/编程环境/index.html","73d49b666ca5922692596da43adcaa21"],["/tags/网络编程/index.html","8501e529d5193293c55e0503bf68a97f"],["/tags/英语语法/index.html","eb0df8acf8c57536d53010aa8d21358b"],["/tags/计算机操作系统/index.html","6c4b1023177696ca6d1f2bd62ec1a1d2"],["/tags/论文/index.html","e7fd64a3dc99303fb42ff64e02339e5b"],["/tags/资源下载/index.html","1b886726ee665630e42b17030467ea51"],["/tags/链表/index.html","a569e1f051a8a88384edb82d870d732a"],["/tags/集合/index.html","c6b6a8f1c49d95627c3e6aedfa7cf2d0"],["/tags/集群/index.html","abb770436322aba87200e00c3f5319ec"]];
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
