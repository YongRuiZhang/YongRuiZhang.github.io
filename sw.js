/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","3b3cc09c18d085139578679509fe8d8e"],["/about/index.html","863f6c3751cf7bc05ea480e1ac9fd410"],["/archives/2023/01/index.html","450be3e245440e0a46d6e5138a4a54b5"],["/archives/2023/02/index.html","cc2a710df0e1f9c80e5c4e92a93ffa1d"],["/archives/2023/02/page/2/index.html","b57fbe9e3353d8e6bc82025499b38138"],["/archives/2023/03/index.html","884c049d63293a0740bbd0f4c9118097"],["/archives/2023/05/index.html","45b1fd730a93d062ce8b23772c67c2b6"],["/archives/2023/06/index.html","f547f3d16e74ceb13acf85b5710f10af"],["/archives/2023/09/index.html","29e7a4dc4fefed99061fcf4d261733c2"],["/archives/2023/11/index.html","2fc7d990d4280e577688b3feaf56bffb"],["/archives/2023/12/index.html","2d5e36e2680d50041d937fa74d4cd2fc"],["/archives/2023/index.html","242a4658b207f3ed4f3eb0ec651d4cce"],["/archives/2023/page/2/index.html","bc76fa7715e122d4d11f772a2a930741"],["/archives/2023/page/3/index.html","255242c0e8999e4029c952a3a6e9dec0"],["/archives/2023/page/4/index.html","f490a87d47344c8474c4bb4d90c8e67e"],["/archives/2024/02/index.html","09d21a5fcd960426a8388a04935a11c8"],["/archives/2024/index.html","bee037ad378c8cc0bb213f205dcd6a80"],["/archives/index.html","ac0b03f4a1a5750cc0a32bc22c8093ad"],["/archives/page/2/index.html","703d5bb1ab0b9b6b2bc86d535a2eb794"],["/archives/page/3/index.html","c3de2755d72d9a1e0ab53cc4775320dc"],["/archives/page/4/index.html","d2a8ebcdbf7c1c8ae0c4776ec324ff9e"],["/baidu_verify_codeva-qQP2iZOMLX.html","c6f55a8c485c32f47c03fad59659ec55"],["/categories/Java/index.html","dc20438afd76769bf820a054f4d8b6bb"],["/categories/Java/后端/index.html","147341371722dccdd1caba722c5a0ed7"],["/categories/Java/基础/index.html","80b84b48e74f7045907d74494e376aee"],["/categories/Java/基础/集合/index.html","548ce54b879f3736390813be79784aae"],["/categories/Python/index.html","b88ae3987d60f569b90b3da74a89dfbe"],["/categories/Python/编程环境/index.html","0af5f26bb1ec6ddb467762c647888377"],["/categories/R语言/index.html","4cec0a104be4bccb475991cc83cf93ce"],["/categories/R语言/编程环境/index.html","accdaeb7c42446efacf30d085f33c733"],["/categories/index.html","b5878b8dd1272cda58e35b82cc89fa6c"],["/categories/中间件/index.html","4e08f4c96cd6797a9a036a478aaa30cb"],["/categories/前端/Vue/index.html","4e2a4ec0f1c3ca75fb71c9652d5d10e6"],["/categories/前端/index.html","0d6632bbe81ea79ec7e1d004b34d7c50"],["/categories/大数据开发/ElasticSearch/index.html","f044a163ca186e90bf3170f2da5d31d6"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2de840fad964b330ece31e3b8b285377"],["/categories/大数据开发/HBase/index.html","73069ec04b6e0cbc594bd3cb201a5707"],["/categories/大数据开发/HBase/学习笔记/index.html","7f05523f9596afaf6baac8348391c9e2"],["/categories/大数据开发/HBase/环境搭建/index.html","d49ad0b860dc77c0f01f6e6f6f7028ba"],["/categories/大数据开发/Hadoop/index.html","cf0fca3f0a55c0b4b33222cb4aad2436"],["/categories/大数据开发/Hadoop/技术/index.html","18c1f43edb7ce7de1d216368a8c57ccd"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9dfd234681dddd66bdd932cec5a1a211"],["/categories/大数据开发/Redis/index.html","876ba7b6dc697725ba9de224f30c68cc"],["/categories/大数据开发/Redis/技术/index.html","26495481b88a4e3d143e06eee63f240e"],["/categories/大数据开发/Redis/环境搭建/index.html","6314a6131a9c5b21bf9e070e60fb32cb"],["/categories/大数据开发/Spark/index.html","51e2441d4f3f3e89c682d5f87b1fe4a5"],["/categories/大数据开发/Spark/环境搭建/index.html","9ff678bf7a4269a2ea702e50d830242c"],["/categories/大数据开发/Zookeeper/index.html","769ffdce887ad0f9e9841552529b0cae"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8ad0075c0bb8243cb2f01b19d7837c87"],["/categories/大数据开发/index.html","04df9f1b4076b9a858f9beab724e5672"],["/categories/学校课程/index.html","6798c81ba2f70d7e96ee0ca4784d288b"],["/categories/学校课程/计算机操作系统/index.html","c0513adcf2284594fc322f7ec2d99a9a"],["/categories/操作系统/Linux/index.html","86a1837f44cfb6e05dad6381b84e8171"],["/categories/操作系统/Mac/index.html","9301de6d7f7a8a3e1800708d421426ef"],["/categories/操作系统/Windows/index.html","ec786cb94c98cc84a49d3a18b43f390d"],["/categories/操作系统/index.html","44caecaecf3bb02052ddf4f21c333930"],["/categories/数学建模/index.html","f1cca33959cf9ebd7351600f97c96504"],["/categories/数学建模/latex/index.html","6ebfa4c7792f7fe5fe3ad8c392d2c0af"],["/categories/数学建模/优化类/index.html","8571b04c324a243be9edc03f55085dde"],["/categories/数学建模/优化类/现代优化算法/index.html","6e63cc2f02e7fa20564cd76f137b9373"],["/categories/数学建模/优化类/规划类/index.html","e041663394de0f528415eaeb867dcc99"],["/categories/数学建模/绘图/index.html","7a45a6d1f2a85722e6b092d00ed43ee5"],["/categories/数据库/MySQL/index.html","cd9b22e9b976e3246f45e418b6ebe5aa"],["/categories/数据库/index.html","deeda909106f82bf5ed362c91cd2bdfe"],["/categories/数据结构和算法/index.html","661e65b7264205de33e2d034cf02968b"],["/categories/数据结构和算法/page/2/index.html","a1c259982eea81a426857779d47144c2"],["/categories/数据结构和算法/基本原理/bfs/index.html","19e42d3f068ce8a6cc176dbf880b933d"],["/categories/数据结构和算法/基本原理/dfs/index.html","fb2cf0b046050008a1488bac1ad27324"],["/categories/数据结构和算法/基本原理/index.html","574568a0bc34fe1b833b9cb850df66b9"],["/categories/数据结构和算法/基本原理/动态规划/index.html","a36de6eb7dcf375262383a09baa3e812"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","0dbe99a7982f264ff19663b2badc9962"],["/categories/数据结构和算法/基本原理/图论/index.html","099a001d2d5b68c5aa0020fa8e464ae1"],["/categories/数据结构和算法/基本原理/字符串/index.html","6f15a3d0a930fe3c8a6c98a0cb50cb4b"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2fe545053ae6b97bca0490fda6c39a6e"],["/categories/数据结构和算法/基本原理/数论/index.html","dfd640ac0bcf57de5ba1ecc8a1f54ebf"],["/categories/数据结构和算法/基本原理/树论/index.html","f5727da8e07a2d060294a88c71213226"],["/categories/数据结构和算法/基本原理/链表/index.html","974352cb314fbd260e26122e1875892f"],["/categories/数据结构和算法/算法题/index.html","1446669de38563ff9eb57109b2d90e92"],["/categories/数据结构和算法/算法题/二分查找/index.html","34e2f41312d8841c6576db0f6602698f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","50e765f38c8be2f144988fc4233c35fe"],["/categories/数据结构和算法/算法题/动态规划/index.html","a298dd4ac7c643627ae87b5a3c53eff7"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","038a44cb971871755c56c5d5baab627b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","c70781b9801788429289f79e649adc5b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","ace9767ddfeab88c98eb08b950fc0770"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","56a23345f7f0c2b6fd3ba0237ce4b61e"],["/categories/数据结构和算法/算法题/数论/index.html","ec03f6b6697e35cb1b0857894e0e6931"],["/categories/数据结构和算法/算法题/栈和队列/index.html","060d73fac75d6c15888712a2ef72202c"],["/categories/数据结构和算法/算法题/树论/index.html","cd199352705471d863f24bda50401d41"],["/categories/杂七杂八/index.html","f2248c1497e3724f398397e403335d77"],["/categories/杂七杂八/博客搭建/index.html","87057ecce8775be0f59751f806087227"],["/categories/编程工具下载/index.html","186213738260fc00e8816e360d8d8ba9"],["/categories/编程环境/index.html","4ffc80f8f8019d688dbbedaeeafa85e1"],["/categories/编程环境/大数据/index.html","a782a17c5d26b6ef2df70321db84b61f"],["/categories/英语学习/index.html","3165b0e05afcc190d409410c1abc1703"],["/categories/英语学习/英语语法/index.html","8f8b20a699979e5c2163c63e9ecd2fdb"],["/comments/index.html","35a90cae642c4021eef29f4e736ea685"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","82fab309dc59397081ce8d9127010938"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","765cbd9aa0cff16828bb49abc25e573c"],["/movies/index.html","f789793598462fef1a6c1cf648caa4ca"],["/music/index.html","ee46e2f305e3a05809af7e69492c8e9a"],["/page/2/index.html","92c76082971b26943058fd1cae98555a"],["/page/3/index.html","ecd0088a7dd4fefb583c5dba3d87be1f"],["/page/4/index.html","0bd32d112c9350086fbc417ed7b2110f"],["/page/5/index.html","91c5e178758f0c158ec75a0332abfb56"],["/page/6/index.html","533566dd2aeee8850441bb08bb229559"],["/posts/1021360842.html","445150de59cd768bc3721f84793b7a89"],["/posts/1120620192.html","b63f6eb314a04519dc22fd557469779b"],["/posts/1141628095.html","9997c454d6fbcb6c38bef5545dcda3bd"],["/posts/1168613674.html","73538b880509b629300861db65c4e0ec"],["/posts/1219920510.html","d22e7151a57a3d5dccd1c785e5975ba3"],["/posts/1222166338.html","06f0c27f038d1d9f7847f052e5a51cc0"],["/posts/1259097482.html","9a32d2534d32baa47987638e31213bb1"],["/posts/1271036369.html","fad8ec69b5be4ce2455ed7dd88b20bda"],["/posts/1312847445.html","0e53ec8f4398f35e6511eaa722662e81"],["/posts/135355774.html","4d34ddf72edeb403f1d7f4c750c358fb"],["/posts/1375344716.html","6ef0788ab22a8276d3ea0cd0cf573683"],["/posts/1388991698.html","30c2b9011856c89fdf26c7096eda6b40"],["/posts/1410315814.html","42440046950eb6a9ffbdd501cfa14ccc"],["/posts/1452790229.html","5417d5cf47a7471ba51ffabf2b92cd07"],["/posts/1470079884.html","054980036147889cbd08c1724c9d38fc"],["/posts/1470079885.html","bb0e55d8ee5f8a9a1f83c282a62da6ed"],["/posts/1470079886.html","5c006d15e30462075e18dd2167999145"],["/posts/1470079887.html","9daf9a5d734d20c432dc137835c34853"],["/posts/1498536549.html","c0bfc463481d1959be909927ce06b477"],["/posts/1539568593.html","1de7c6223c99442004f650f445734854"],["/posts/1547067935.html","9d20e4a145cbdb5cd9e4cff13bb89fd1"],["/posts/1557866301.html","e64c4bca21bbb20b9098ebf31e0fad09"],["/posts/1571776361.html","5dfc4bd8f549680cdb1abda863472371"],["/posts/1605124548.html","e93aa3f1495bd22772ce2d0ec19becfd"],["/posts/1633036852.html","375087090177b3449259c8a8a67bc91e"],["/posts/1674202625.html","7acb8e82b0546dd1cf1e07f44d01d790"],["/posts/1765123828.html","34840a034e01a6c695afc8f22a4b3042"],["/posts/1767336200.html","1d7dbd9c413b83edf8f5517638ec7099"],["/posts/1776114197.html","c70b8bc75ca54067098a7475b278b6eb"],["/posts/1817748743.html","df2c803862915538b6e3a73d147f4a02"],["/posts/1925125395.html","83e4960372e74c421d2f600de0a1b197"],["/posts/1966191251.html","296c1b792803401d756b4608b3118542"],["/posts/1987617322.html","2decc50ead32e0456e174848faebda5e"],["/posts/1999788039.html","be51f5b1543c006f39be87e3b0eec69a"],["/posts/2075104059.html","cb398d210df972cdb485b2429fc2c53f"],["/posts/2087796737.html","dbc5ea8564e5510b68eb3fee9720dffd"],["/posts/2106547339.html","ebe4e58682501f2ba055f9d446fe2f97"],["/posts/2207806286.html","fcd9ff69fc6cda49646ed8242aa7a403"],["/posts/2225903441.html","02aaa72f6533bdd468351e8f488e594d"],["/posts/2265610284.html","0b7736ffcf7d66b5ad8b917e73cd402f"],["/posts/2281352001.html","8f79b466582be6412f0a368f69624496"],["/posts/2364755265.html","7384b07ceaeaf9021146d8dbd27ec93a"],["/posts/2414116852.html","9998a18610a27c65a6980ebc1138bda3"],["/posts/2421785022.html","453e4c7810ee838876eaff62b1ba0077"],["/posts/2482902029.html","14665f72f3810b2c37a3078fee6074dd"],["/posts/2495386210.html","47ef381c2b7ffe9c17546d7db90ee206"],["/posts/2516528882.html","f1fb827d01bf9dcabc662435fb6ce019"],["/posts/2526659543.html","c0861ef27741d82805302293ad37a918"],["/posts/2529807823.html","65074dd27db488d364a6ca13e2a23d3d"],["/posts/2596601004.html","bd9e23f501100bccad5ea4f721e59d09"],["/posts/2697614349.html","7fecca316e023bfc5d66ccb0664fc0a5"],["/posts/2742438348.html","23754725ef88bf251c39f4b1b9320ba7"],["/posts/2768249503.html","15a4199af8325411fb5e3dbc96c40a59"],["/posts/2864584994.html","905775436179432d70377b331e53f32d"],["/posts/2888309600.html","a23ae8cf329731e4ed66aeb6fceffdd9"],["/posts/2891591958.html","f854ab02383cf79c4e7e093ee2dd1156"],["/posts/2909934084.html","90514dd96154b6f2e42094105b779ea5"],["/posts/2920256992.html","a1830a9edcb3d99b98b8e29dd19528de"],["/posts/2959474469.html","ac4b25eb1c52fe9c53b4eac990af8412"],["/posts/3005926051.html","a4aae1916556b605a993649eea5e2c2e"],["/posts/309775400.html","e38755fcff00526991e31f518ecbad7e"],["/posts/3156194925.html","52e21d86fbf5781bf451fc3db73b93df"],["/posts/3169224211.html","dcab8187d940df80ea7e3733311ba906"],["/posts/3213899550.html","961a6e4f40653dcd352d2371b47801e4"],["/posts/3259212833.html","90bf573d4533cca4098e6cf3f5c87f13"],["/posts/3266130344.html","0a1069ce6260010388dca4471ec47219"],["/posts/3292663995.html","4143d27ecdc8503c6c36a9180944415f"],["/posts/3297135020.html","855eb5f2de7f974eca0164a06d2384ee"],["/posts/3306641566.html","fca2325db303d7de23b15fc34a9d2ca8"],["/posts/3312011324.html","1e6b7d751cf38e7479d0e5f3de72b21d"],["/posts/336911618.html","85b35478dfe00b128bc5c109fa736045"],["/posts/3402121571.html","a16a18bbaa3fea8d895f8bce51c3c52e"],["/posts/3405577485.html","6f494e2bc729747274f4b90ae532b4b7"],["/posts/3498516849.html","4d2e23bfc625d773b5dcea3d8730cd15"],["/posts/3513711414.html","f0534b7daf0d6f9eec4c6b22f8ee4462"],["/posts/3523095624.html","99a140a21a09f33950fdc5d0623ba3b3"],["/posts/3546711884.html","f8885e9de4b08b6b2fa7a30871636b70"],["/posts/3731385230.html","ecf091bd1efe1510fb48189d77d6c18e"],["/posts/3772089482.html","d1b60673d25c39875e9419574e6158e7"],["/posts/386609427.html","681e8ba8c31be3bbf3bd38ba8996ee30"],["/posts/4044235327.html","b5652563c3669b300f83d36e28cbdfe2"],["/posts/4115971639.html","ffedf826ef9a72ea26a96a6830d5831c"],["/posts/4130790367.html","c51abd5df8737dcdc7752b76a84203aa"],["/posts/4131986683.html","8bcf0eeb61d9507dba3706dfb734b576"],["/posts/4177218757.html","21702fbd9012366450942e3f23eaaa37"],["/posts/4192183953.html","0d5c931a7dd2979db6663bd36d2b0911"],["/posts/4261103898.html","1ab216957b4349f85869dfed348a7532"],["/posts/469711973.html","e37c4ae1148c682e7ff9c8f185b0a39f"],["/posts/482495853.html","772b32ba90a5fff8083542685e4de939"],["/posts/488247922.html","59d02c96ceb9171b15af820ffa6628a4"],["/posts/517302816.html","ccc188edb3c8fb6e07fbcb31b0e0ae09"],["/posts/570165348.html","e86dd652240bf51016744a68c2c2f5be"],["/posts/595890772.html","55bf6ff165dd6f1f8dbf799abd3bd382"],["/posts/67485572.html","ecd0c590066433908e41025f85f5c00a"],["/posts/694347442.html","9915b61a18bd33df8565484a2120edd6"],["/posts/707384687.html","7e9e058f8271b7a665f323c053f157fd"],["/posts/71180092.html","86728f8c16fb2d870ee14eef8ffe1625"],["/posts/716459272.html","be453d6d8134975e521cad966c4ae129"],["/posts/765481613.html","b86782852c1fd8f46d4bec136703473e"],["/posts/778231993.html","e087eddf5db0900e6c44867cc4c3ef15"],["/posts/795397410.html","4afabc144f82d557c27a16a469df9c43"],["/posts/820223701.html","cf1255b02c15efb93df91f8ffa9df09e"],["/posts/830372185.html","c0dcd91b522f665c88ab45c8105ea7b4"],["/posts/88294277.html","721eb8769125931665bf02c0dea689f8"],["/posts/939963535.html","d7ad02c38f79fe0531ab35ed24b01e19"],["/posts/983786067.html","c3b5b883d3be8e809b2c7801aa4725cf"],["/sw-register.js","b8906b1d2921ed011eba9e3aa89405b8"],["/tags/C/index.html","34080b7d672aca7ee7b45622638e1fa8"],["/tags/C/page/2/index.html","ade31aa11fbeb78fa170160b9b6ca1a6"],["/tags/C/page/3/index.html","aeaa35495f9be394d81806da4343cd40"],["/tags/C/page/4/index.html","eeb343a776fa34d673845cc6b6fbf3a9"],["/tags/ETL/index.html","99c97d0203ad7ce19ec41f6ad271c378"],["/tags/ElasticSearch/index.html","2fd8227f1c408bb4bac006ef83d51e9a"],["/tags/GUI/index.html","68f969c986120e2d50fac98aed4f6927"],["/tags/HBase/index.html","04737787a69a47c89352f61cb5ccabb7"],["/tags/Hadoop/index.html","105ff6f6ba96daa66e378e5a8b2d1e65"],["/tags/Hadoop/page/2/index.html","0a50b4935ac490565bc518c480da8610"],["/tags/Java/index.html","f113d2dea77a51daaa459e6191c57f95"],["/tags/Java后端/index.html","521b182ee5fabe76eb4d7c959a27409c"],["/tags/Java后端/page/2/index.html","855def27e0e9117d8340e8445fe42133"],["/tags/Java基础/index.html","3a84e636e4be5a415f73c8459df3b874"],["/tags/Java基础/page/2/index.html","e114c24e09b3fc80d9716530301644ed"],["/tags/Kettle/index.html","014ff163e1733e934c0d2e0350a6ad7d"],["/tags/Kibana/index.html","3d96052df1ec0e1804e9578b765dfb1c"],["/tags/Linux/index.html","dce8907616ec5cbfb6a1dffa9da07670"],["/tags/Linux/page/2/index.html","6d59c40dcca7c65b8c662c94f99f16ab"],["/tags/Linux/page/3/index.html","4ad560ad03894358059cce487160131e"],["/tags/Mac/index.html","68733005fd95da19ba63a220ce3264cd"],["/tags/Mac/page/2/index.html","7a0d10a059fa082c417f81cbd3b4464a"],["/tags/Maven/index.html","bed17cb7d0bccafe3f517eb49b149faf"],["/tags/MySQL/index.html","6708a8b4ab2886fe1cf6bd25a744a37c"],["/tags/Python/index.html","be3121d60404158aa044a26ead2bbdf9"],["/tags/Redis/index.html","dc8229cd69432097a1b9d024ccb0d0e5"],["/tags/R语言/index.html","c6d8af81121f6f09916e8a3d3a6739a2"],["/tags/Spark/index.html","be8f218886519dc85e740fe1e921d719"],["/tags/Ubuntu/index.html","0eb33ae73ad4b8616c7ab4e5e3b787fe"],["/tags/Vue/index.html","8ef41c51fba7c3cbd2a9f8212b6d558d"],["/tags/Windows/index.html","c96fd1b2ee148dd8eece7e775392ee10"],["/tags/ZooKeeper/index.html","fd99c2e1e6188d3ac568965645e1fa78"],["/tags/bfs/index.html","692ca3f0f7cdd52926ba6757ddd4bbdd"],["/tags/dfs/index.html","3dbfb408b106270ad2e3e25f7cc8614e"],["/tags/folium/index.html","a071021d21d5d1aeee343d53cfd12919"],["/tags/git/index.html","c13b2512af83d7b078b05f4b9e821f8a"],["/tags/index.html","1b129695d398fdf9e5bab48e246cf384"],["/tags/latex/index.html","c6c30dea43a4aeb2eeb0fdc6a30d742e"],["/tags/中间件/index.html","c0ee02ab4ac0aedb2dae23689fe11ce1"],["/tags/二分查找/index.html","b518f1efec7d9bfabe30d3a2942093cd"],["/tags/优化类/index.html","853912a7f6751e3b81b8988f078c0163"],["/tags/前端/index.html","49128c067f092a74e487c273ebf000bc"],["/tags/前缀和与差分/index.html","f29582703db7c033ed7cafba657f6754"],["/tags/动态规划/index.html","e427184fcab5e5d3d27b0f555d01858e"],["/tags/动态规划/page/2/index.html","8d521baf22ffc3dcb351400938f6cf66"],["/tags/博客搭建/index.html","9a8717970204d39af0f010a74e2df932"],["/tags/图论/index.html","b0679604c404f7b79df35cd9ff417eb7"],["/tags/大数据/index.html","f9db63fdce58736730e954a0fa0fdfca"],["/tags/大数据/page/2/index.html","31e9bcc1f98a36f315069f4b98d4d185"],["/tags/操作系统/index.html","af2381a82f2ac0af2227f19705676527"],["/tags/数学建模/index.html","424ce824fb085c4e37ab071ef84541da"],["/tags/数据库/index.html","57ca81d192e8e24dfaaf4bc8fd4db471"],["/tags/数据结构和算法/index.html","88d2e8b43425d001d6c476ce40ef4f88"],["/tags/数据结构和算法/page/2/index.html","e006b8e778551882291c0fc1a12a98f9"],["/tags/数据结构和算法/page/3/index.html","b3cfd86c88a5d107094ac00bc3b32f83"],["/tags/数据结构和算法/page/4/index.html","b2842f49b64c4209672b3ea40a25dff1"],["/tags/数组和字符串/index.html","61dc5c11d7b99babd15e51b1ba0af6eb"],["/tags/数论/index.html","d00a433768d36be76a8cf428478efbf9"],["/tags/枚举类/index.html","615fa4f2ee93206507b4f6a491670931"],["/tags/栈和队列/index.html","ccb640f711e214e84216b576d1ad39cc"],["/tags/树论/index.html","649e2f797118b9d8df3d73c3b8978df4"],["/tags/测试/index.html","66d1a6785ab40485b85434adfd5b28eb"],["/tags/环境/index.html","2a766fe24ebb5ea7290a9d33b2375b0a"],["/tags/环境变量/index.html","11a9fddbfafb773034b4d94d728f1392"],["/tags/绘图/index.html","2a45669ada25175e19e4f9771add12b2"],["/tags/编程工具/index.html","d4ceea9d64d9e6105bb8e7b1adebff1e"],["/tags/编程环境/index.html","3aed91628480f6345ce3d80021aab480"],["/tags/网络编程/index.html","4de467b187d958d968cd923be1e7e76f"],["/tags/英语语法/index.html","eaf9556efd7a25f600bdcf94e36c46f2"],["/tags/计算机操作系统/index.html","c1d2c2b8709f023a5fdc6f01edb18aa1"],["/tags/论文/index.html","e902ddcc517522c1e77cdbed3221c106"],["/tags/资源下载/index.html","c328df4a6c9472180b1066bb5576ed1f"],["/tags/链表/index.html","c49bf17e60a96380cf7e59d0e754e6e3"],["/tags/集合/index.html","b18bc253638cc85c2f9d2e753abf4505"],["/tags/集群/index.html","bf2ec41a600f9085d79bbd6c03bd36e2"]];
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
