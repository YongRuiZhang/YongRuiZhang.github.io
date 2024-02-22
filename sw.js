/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","94b7bc03844a76dab03cbb60c5f44df5"],["/about/index.html","638ec9bdf44a97e00314d5da410cb077"],["/archives/2023/01/index.html","f28c9d1f49448e5a329aabf59148423d"],["/archives/2023/02/index.html","4bcc49ecb982e342b6705f2482b8de72"],["/archives/2023/02/page/2/index.html","2620fe4edc8d9a34f3ce68c0f68e581e"],["/archives/2023/03/index.html","8a120f224ce093a0518140ac9a51c649"],["/archives/2023/05/index.html","00f7cb884bb22ac48d8bf9b7c36097a7"],["/archives/2023/06/index.html","b2c3d413d10df1ed69c9fc0e2b423fab"],["/archives/2023/09/index.html","4d4fb8691a78118260460f2913a9c639"],["/archives/2023/11/index.html","31239499ca646391aee9d0c48c9b290b"],["/archives/2023/12/index.html","28463aaf2b142c5141cd50b45fcb8b78"],["/archives/2023/index.html","5e4cec110526195fbcb0179716f55ce1"],["/archives/2023/page/2/index.html","4e612ee19626352ed3b6ae96a5e15270"],["/archives/2023/page/3/index.html","d6f5d1e194c5c0b145027ee4e807a770"],["/archives/2023/page/4/index.html","e4ad90d99864b15f7f0174ed0d86c867"],["/archives/2024/02/index.html","9f3d57461581bb0b47bb9ac9f4a8b4f0"],["/archives/2024/index.html","6b725fccbd1d4d648c0da9ca351df37b"],["/archives/index.html","f6b8ee139dee4f2eb7ce2ca4fad30f98"],["/archives/page/2/index.html","1116535c586bf7db3408937557310d14"],["/archives/page/3/index.html","219e327e7187947af0875b46f94f5934"],["/archives/page/4/index.html","811c8e281188a46f99f7ed9ea15002a4"],["/baidu_verify_codeva-qQP2iZOMLX.html","256f3a236f698ffdda52093b639fe1fe"],["/categories/Java/index.html","6a4b8d4e1a29eb0d99445b848f9b615c"],["/categories/Java/后端/index.html","3ea5112afe0d0a4840500e1c47fbd268"],["/categories/Java/基础/index.html","95c142dbc9884f4a6e6bc829449cc09a"],["/categories/Java/基础/集合/index.html","d852abe056c7e07ba43861a5d9d36fb8"],["/categories/Python/index.html","39a03d599926e864f4f3fa3bda4ddd25"],["/categories/Python/编程环境/index.html","bcb923061a44abd77a8d08d1935d9945"],["/categories/R语言/index.html","dcb9b8a56c94961124aadcb700202ee1"],["/categories/R语言/编程环境/index.html","06ef635ea3d8e66cd9141c87bc52876a"],["/categories/index.html","43548fbc2c9cbc5b7eaf19e66fc4d40f"],["/categories/中间件/index.html","75caed80e52776ff901fcf030b4437cf"],["/categories/前端/Vue/index.html","2cfc5477dce86c1be59b153db98064ab"],["/categories/前端/index.html","fc5447853871b4f7d8dc0a3127909e8b"],["/categories/大数据开发/ElasticSearch/index.html","ccd166f58befd0aeb34259f8b74041d6"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","1174f7679f08c3abbdf98691d81d1c20"],["/categories/大数据开发/HBase/index.html","1291ac4a3795eedfca8ac152380fbfe9"],["/categories/大数据开发/HBase/学习笔记/index.html","03ff65502880716b28461ab854f5ab95"],["/categories/大数据开发/HBase/环境搭建/index.html","91eb21a8c69cc6d9255475027d3adf21"],["/categories/大数据开发/Hadoop/index.html","fa391d88ea03709d6a88b356cbb7e66a"],["/categories/大数据开发/Hadoop/技术/index.html","5574b95d4d9f8ce4540b5e298743a4ea"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3df2ae202bcd00243c2db6a0db5f29c6"],["/categories/大数据开发/Redis/index.html","44ffd873949d5915932a3c815c479449"],["/categories/大数据开发/Redis/技术/index.html","27ad2a7b646222184e569ac8255f646d"],["/categories/大数据开发/Redis/环境搭建/index.html","a5435a0e834f539f51787199c1ae637d"],["/categories/大数据开发/Spark/index.html","c05853f1b3a468c822baa1a9920e2ebc"],["/categories/大数据开发/Spark/环境搭建/index.html","480901c90bd654a44ce37ef457e04134"],["/categories/大数据开发/Zookeeper/index.html","459b11e472351aa2a818e2a550d8eced"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","0b91eb1e90703d913385949df0cb051c"],["/categories/大数据开发/index.html","fd9ee98d6eba89aeeb08b78884f8f393"],["/categories/学校课程/index.html","3662e530434472b81fb663422a5b6436"],["/categories/学校课程/计算机操作系统/index.html","22111eea18477f555b7b297468f497c6"],["/categories/操作系统/Linux/index.html","faaa17e3d736ea5be9526f62c10fc50b"],["/categories/操作系统/Mac/index.html","dce57228b895d263ea36c33d1e970c89"],["/categories/操作系统/Windows/index.html","77be16df7eed9a533cf8708bf5cdeea2"],["/categories/操作系统/index.html","f016bc4f45cc40cc93e584d4a87954dc"],["/categories/数学建模/index.html","18d452f3bd1123ee798acd9ab133fb11"],["/categories/数学建模/latex/index.html","71e542749947a28f35e9798a1992d1f6"],["/categories/数学建模/优化类/index.html","bce2d876960e99fcc0584a6147be0c3c"],["/categories/数学建模/优化类/现代优化算法/index.html","9db233d41d4370acc51689f81b277fd6"],["/categories/数学建模/优化类/规划类/index.html","c8502bafa44fbcc27b98ffc54f4f5da0"],["/categories/数学建模/绘图/index.html","79fe05f33534549c303616017990f04d"],["/categories/数据库/MySQL/index.html","db18dca8aa200e834038f10d0e9da773"],["/categories/数据库/index.html","da84e784b77f97d389d14bb3eaf18988"],["/categories/数据结构和算法/index.html","5a6bc32cba1d9e5e8369cf0937a9e8b8"],["/categories/数据结构和算法/page/2/index.html","33a0ae86e939b979f6eef84c3148b425"],["/categories/数据结构和算法/基本原理/bfs/index.html","4df4b97b02fbf95802efb1a290747af8"],["/categories/数据结构和算法/基本原理/dfs/index.html","2896acd7c23efe05c0a15b6573a2e952"],["/categories/数据结构和算法/基本原理/index.html","a23fa46a6ee32940074f7ef4d4d6c61f"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e19968b5d0f479c3aa91930bb405d357"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","f0f82654702b83a4aa2092ec81d070d3"],["/categories/数据结构和算法/基本原理/图论/index.html","5e285968df7d3e02a7a562a1071bdffe"],["/categories/数据结构和算法/基本原理/字符串/index.html","988b4102fd6b90103eb08eae1e3d40f9"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d0e17c7370374c97d19bf08876506c52"],["/categories/数据结构和算法/基本原理/数论/index.html","37d33d5826e7ff2b1d941c99e58db2a7"],["/categories/数据结构和算法/基本原理/树论/index.html","82da2cc8b8d0f3b7745bfa75af6441de"],["/categories/数据结构和算法/基本原理/链表/index.html","4a7a0864480f82563d0021619b399144"],["/categories/数据结构和算法/算法题/index.html","45b5da7e3c979413fd1e2e70e9ac0ab8"],["/categories/数据结构和算法/算法题/二分查找/index.html","fe7bccd5fd3f45700f03c8837ac2f1f4"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","7a924a47553f0c932c7a61c5f2062f88"],["/categories/数据结构和算法/算法题/动态规划/index.html","7c6ed80070eb2141403553f1f0ceb6fd"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","c19c49723ea80dfb60b840d3f70b2a10"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","84ce46c23ff37ec4fe1c4010d973ccbd"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","6fbd1c667b36c3c2183de4e459b877ae"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","8dcb9380bd6abc9a823f74ab4a532646"],["/categories/数据结构和算法/算法题/数论/index.html","6be58e0f8c6a9ffc46b5f2a9c259a1f5"],["/categories/数据结构和算法/算法题/栈和队列/index.html","34a68aff61726199b1867ffc1385c4e6"],["/categories/数据结构和算法/算法题/树论/index.html","3e331d3846ccac3ac192d4e6efb04cf0"],["/categories/杂七杂八/index.html","7e5d7d6b6eb1e565d61034776e6a3fa6"],["/categories/杂七杂八/博客搭建/index.html","644c94854043f9fa65beebdde4aa6ed1"],["/categories/编程工具下载/index.html","a2ded063255e7b14aab9c70f33563dbf"],["/categories/编程环境/index.html","3e671fbb82da2c8e12bf22cb191f66ed"],["/categories/编程环境/大数据/index.html","903f837059cadf45061a4ab42da592b3"],["/categories/英语学习/index.html","14b197f090a912ef9789cb1ca6f5dc71"],["/categories/英语学习/英语语法/index.html","eab3777346b4834dcc5b473337ce9635"],["/comments/index.html","99284de009c7c122a8003e56b2becde7"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","fa8bebf26f66c9c624c80c4d6448d6cc"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","37ed8bb1d48d0636cf69010d215a69b7"],["/movies/index.html","876fc7fb0ed575d72bc358658ff62a2e"],["/music/index.html","437f717e893749369e52352c412fd22c"],["/page/2/index.html","7f9ae3dd0ad36e1d7ff24b0b503dcf36"],["/page/3/index.html","b0b75512f016a9f4ffbb71c0c6f3f1c7"],["/page/4/index.html","2d5d692bc2b2c9d902eac6217fdba7b9"],["/page/5/index.html","21ad9603ae2d9f36b3f6bb0fae634908"],["/page/6/index.html","73fe700cc66ba30a7523b69ad0f3846e"],["/posts/1021360842.html","ef0c3862fc6878355049c67e992d75e4"],["/posts/1120620192.html","76f08e1e48a9b05ec91a3df653d34f42"],["/posts/1141628095.html","c43e875a750bb32c23ed845763d667f2"],["/posts/1168613674.html","da936a481908a7a310201a037f95b4c0"],["/posts/1219920510.html","d71782c3233806b2c81e3142aa276514"],["/posts/1222166338.html","f234e3755729dcc7c4474e97a17c8556"],["/posts/1259097482.html","e97bb9cb42bd98a243c6b7330d7e0c52"],["/posts/1271036369.html","e9d722dd18d7aa7f0db982445c2f21f1"],["/posts/1312847445.html","f5ac5275c57ed1da9eca45ae14fcae34"],["/posts/135355774.html","6289a73347f57361437d123b8d14628b"],["/posts/1375344716.html","05e13f93303cb71eda00cac86f26e74f"],["/posts/1388991698.html","f64bdb5b44c21f86e96941b511060892"],["/posts/1410315814.html","ef0956fdf87158b71f11045fa3d64e02"],["/posts/1452790229.html","2f86f8dc5089a7367873e37400fa038a"],["/posts/1470079884.html","606eeaa99c59414d98748359991e1f08"],["/posts/1470079885.html","a1dbf3c19caa6607ec3421fd65c0e711"],["/posts/1470079886.html","21771d3bf7b11ee9dff28433eed55236"],["/posts/1470079887.html","ab8b26ac2d7dea2381d8b8b3febebf94"],["/posts/1498536549.html","05220dabad9528bbb78542c07e3fd687"],["/posts/1539568593.html","88ce6cd8297df82c774ba8243ac1f670"],["/posts/1547067935.html","b7e9838c29b0b610e09ef2d28f9d0263"],["/posts/1557866301.html","0c7026588003c13db9000fe0be6e3900"],["/posts/1571776361.html","192d7add2ebefad5fc5d193bd8fd8b34"],["/posts/1605124548.html","8965ef91d71547eb2634cface82f4e73"],["/posts/1633036852.html","d7160c1713501689e82c88e99543a497"],["/posts/1674202625.html","7375eea2976c75fde2a122dd14af8b1e"],["/posts/1765123828.html","4937c0726caaa67647e3c1ee25d638bd"],["/posts/1767336200.html","a8a0d7b2f53171efddb791c3fa5ac465"],["/posts/1776114197.html","e989e046f05dd8f910d0d7114492bb9f"],["/posts/1817748743.html","241fc8433587c9288120f820af9159dc"],["/posts/1925125395.html","be2c8c6507c6bdeccd37d629c34fe58c"],["/posts/1966191251.html","fbd71f896790fc50de7f4f08c5a05702"],["/posts/1987617322.html","07f7ff0bf24374fe78c5c4e9b84099ca"],["/posts/1999788039.html","7512c3d5820b9f7bb5a9506c0d8df671"],["/posts/2075104059.html","1ca7782861c30f555ec4096d00353be8"],["/posts/2087796737.html","0a8eecb1bbf89b5d1c353f56c1692dee"],["/posts/2106547339.html","79c5d97f4bad55216a746bfa70dce3e3"],["/posts/2207806286.html","fd10a5d4e71c062d6fbd8038ca59166f"],["/posts/2225903441.html","71dcb8b8ef110563cb8aee34a85c9486"],["/posts/2265610284.html","93f9afd5e5a3f4d67cd9897c18835ce9"],["/posts/2281352001.html","d9de4fce908bfe3cef242fb943ee89a9"],["/posts/2364755265.html","e578f81ca3551a513d0d88a5a50db5f2"],["/posts/2414116852.html","322b7d6198fa1f7e8a24035f521eb02d"],["/posts/2421785022.html","1147229b02b0c3c5602036f8820efb82"],["/posts/2482902029.html","cbe727ec523ccc111b7a65d6aebbb503"],["/posts/2495386210.html","7539c90d75f213a98b8e64a8559845f7"],["/posts/2516528882.html","7280f526a3816a098a6657c09c9ac73a"],["/posts/2526659543.html","90661c76c7b18282c754432ce3df3003"],["/posts/2529807823.html","25ddfef57cc89f3ba7c53566b84568af"],["/posts/2596601004.html","c484f920c276331122022654045ea622"],["/posts/2697614349.html","7d13c789bdde327456ce57998ef06781"],["/posts/2742438348.html","bdb72a0e5ff77f515d04012583ec9ef8"],["/posts/2768249503.html","5dc5201d8ecc57df75686637628e54f9"],["/posts/2864584994.html","71ea302487b194a7af68bb670c338975"],["/posts/2888309600.html","aaf005297a3e498bc50b2eeae244fd3a"],["/posts/2891591958.html","41ea716e9b2c8b3a1cf55c8ee401dcda"],["/posts/2909934084.html","fc054193cd5aa8a12c269690e0e11b43"],["/posts/2920256992.html","38cae9808835d00bdfbb1bc55879a45e"],["/posts/2959474469.html","d38cc897e3bcbcd4ac0e1db88e7f1c5d"],["/posts/3005926051.html","c2813b45f6ac5f80b2611ce062e52bc6"],["/posts/309775400.html","7774dba47c2c6b5e3d33f1d6999ac8d6"],["/posts/3156194925.html","992b126b02306be0e71d01aab5c7fc45"],["/posts/3169224211.html","3baf2c86a6a2c33093c674d997cf93ad"],["/posts/3213899550.html","7d665de28c3e34eb08d717fd841a6766"],["/posts/3259212833.html","5215556c8a0464d1dbc155b43578ea2c"],["/posts/3266130344.html","542d9bdd561927c993669ef44d94357b"],["/posts/3292663995.html","2479d2c6dde7e7ebc6b247d434471fd7"],["/posts/3297135020.html","6e20c1228637828f64d64e5d56761bdc"],["/posts/3306641566.html","87a788335833779e2ac76128917f188c"],["/posts/3312011324.html","32f3d147c88866961154f57e9083c281"],["/posts/336911618.html","de7a7a4a0549ce7d1014a34ffbf9265a"],["/posts/3402121571.html","941b69b1dc405e30d7920507ed3c9523"],["/posts/3405577485.html","5e50e52c761543f91a01d795706780d9"],["/posts/3498516849.html","726b4095db9aa9bed75755c2b2699f65"],["/posts/3513711414.html","65b00d890c9dec46a06ce5364489e3f4"],["/posts/3523095624.html","f040afb958596ac43dc08cdcce1a050d"],["/posts/3546711884.html","95befd28c63c19c922c2c95879be6f9d"],["/posts/3731385230.html","c66c6926b4124b49fba0343e5fde11a7"],["/posts/3772089482.html","b9577b6fa478417bdb2ebbd632e168a4"],["/posts/386609427.html","16c7c027408fdac23691e2f6aa0b6004"],["/posts/4044235327.html","8d78e13e9871eb9c8c146907d52ebf4a"],["/posts/4115971639.html","a185a4ae6af75a93083213720e474fc6"],["/posts/4130790367.html","45652fb9856e1de76dd0590c03533c4b"],["/posts/4131986683.html","8f40c174ec5c491999cbe09d7dae3574"],["/posts/4177218757.html","f92d625705d34594a50666f823d02d00"],["/posts/4192183953.html","f92f066c9905da569656bbc6894d3cc7"],["/posts/4261103898.html","8a9ec5635cb81e3c60760ec66b21593f"],["/posts/469711973.html","b819834fbfe02af752eb6b1eea7002d2"],["/posts/482495853.html","f148c3c29f6504ba4a180a6a81cdaab6"],["/posts/488247922.html","f4b6101928b63900e71720e05a25c015"],["/posts/517302816.html","a0fec15e2199d13957f97a93690bc107"],["/posts/570165348.html","17a0d643aade35e945545af26bb7799e"],["/posts/595890772.html","88047d5818378ef655b96e4ee4646b86"],["/posts/67485572.html","d835192d3d5c29e61ba447e2643b163a"],["/posts/694347442.html","236b24bf7f13b6e50f760efb2b9cccb4"],["/posts/707384687.html","6949ce1d500ddfd77b59246b4515dad4"],["/posts/71180092.html","f7cfa275aaf2afeda098f4eb439371e3"],["/posts/716459272.html","1865aecf5ebf55f9d2bea7eee3838bef"],["/posts/765481613.html","49f94ec2e610b3254a3fca7b75c2f1d1"],["/posts/778231993.html","c7182e62b4c3bd0193df619c26da8cc9"],["/posts/795397410.html","5d59804c15af9579eeacfad336186f61"],["/posts/820223701.html","5ab612ecccd5df73ecfaaad595845ccd"],["/posts/830372185.html","9d1439635ced21f53100ec65ac0be0db"],["/posts/88294277.html","dbe13bd5960512117f6fc996f2057771"],["/posts/939963535.html","bac5e755f39d5213343a336639d64e79"],["/posts/983786067.html","be60098fd23bd3075bd4f65cf2afe64a"],["/sw-register.js","dc1632a236d45c5cb7de55273103d237"],["/tags/C/index.html","3149df69fcd1bf2595bdf87e31c6f562"],["/tags/C/page/2/index.html","14cf26d10f9ca2034d58b533c7c9fcff"],["/tags/C/page/3/index.html","048e5da9ecbc34762eb181ec5f289094"],["/tags/C/page/4/index.html","7ce7419b417482ab8915ec97b503743a"],["/tags/ETL/index.html","913f7a3bd0a619a8e46b84bfd1ab7a69"],["/tags/ElasticSearch/index.html","3138a65288b12819db58d085175909d9"],["/tags/GUI/index.html","24e94667e8cfaf26b8f7b22e451891d0"],["/tags/HBase/index.html","8a78aa1af373493c5bfaae50e77cefd9"],["/tags/Hadoop/index.html","8c1907bc3b9076920693300f730aa127"],["/tags/Hadoop/page/2/index.html","0a7721c04bd08467f1bd9f9de6455842"],["/tags/Java/index.html","b616758b76b1050eed87663d94355ff5"],["/tags/Java后端/index.html","0e561d592b0951a70e1d075226d0ec7c"],["/tags/Java后端/page/2/index.html","7a47aed72ebedc480c090521d866cadb"],["/tags/Java基础/index.html","47c1a713a1441df51cae868d1543f02c"],["/tags/Java基础/page/2/index.html","d5d334387762b37add4f9e672be77f76"],["/tags/Kettle/index.html","0b5c2db77b734e9bb2008f904978f718"],["/tags/Kibana/index.html","a5015c8f1613766e28ae9a073100777a"],["/tags/Linux/index.html","601c90a123d4d40a18e4e42bdc675134"],["/tags/Linux/page/2/index.html","0918519fdf0b2afb9af31de9c53777e9"],["/tags/Linux/page/3/index.html","4ef8fe1ed9aef6b66ac7a16c7a4e97b1"],["/tags/Mac/index.html","897264cd9b458d86ab676e5b92bd3226"],["/tags/Mac/page/2/index.html","f04eb8bc41f69616446bfe96c996fb51"],["/tags/Maven/index.html","6ab2cdf633446898e644defdd068a6f5"],["/tags/MySQL/index.html","879b03cca99b099d4a56185735084c29"],["/tags/Python/index.html","51bacf8251228b3cc03d906a1b606214"],["/tags/Redis/index.html","22ccca6595bce57a56370720d513a581"],["/tags/R语言/index.html","b3ffc956e25947d6104ea1bc440aec32"],["/tags/Spark/index.html","a2c3164fd3170c3e1bb246761bc58aa3"],["/tags/Ubuntu/index.html","6e205eb7e8a9dda01be7932e3eabddf6"],["/tags/Vue/index.html","c198c42f806b97bc1b427cbe66910089"],["/tags/Windows/index.html","f696e0853107faf5eacfb94d9e2ca3f3"],["/tags/ZooKeeper/index.html","7c6cf8ec44cb6a3d9a5b4ce2b8cfad27"],["/tags/bfs/index.html","8f4794693afadb2e50df3e8f43c86c31"],["/tags/dfs/index.html","c0dc452351bab9cf4d568bdf9b6bff7f"],["/tags/folium/index.html","402d3bcc4cc47c23b0ecbfc246ea60b4"],["/tags/git/index.html","6a9d10edb0ceed45f330ab9e278a47da"],["/tags/index.html","53ceefa35f68c6ad0cb516efd0fadd38"],["/tags/latex/index.html","373a4476904bd686319b762f211666d5"],["/tags/中间件/index.html","6268c27f64ef9c4a4e71f391b47e10e5"],["/tags/二分查找/index.html","1b4626571f09e733aab540c2098ce8bf"],["/tags/优化类/index.html","a43adc2a9e86646700ded59af3e8222f"],["/tags/前端/index.html","4af11656c9d08102444770c0e86e6784"],["/tags/前缀和与差分/index.html","6e85c301ccfa9375ca8a5dd567183f0e"],["/tags/动态规划/index.html","1e68b4c5c2bc4c3e7f5c18cb170e41ca"],["/tags/动态规划/page/2/index.html","f12f68c19943dbb116f6077789e24f7a"],["/tags/博客搭建/index.html","2d2025424c833d8ec58fbcd09f6617ab"],["/tags/图论/index.html","b62a2a94f16a95739ee73e46014cdddb"],["/tags/大数据/index.html","dc7444450ac790f0acba374523ef67b4"],["/tags/大数据/page/2/index.html","3b5e4045dd4d93f9f5c7371715327dec"],["/tags/操作系统/index.html","8a12d1f532b1495b72742a8db284f40b"],["/tags/数学建模/index.html","1ba97a449912e154d82efcc6a44a5baa"],["/tags/数据库/index.html","0f04ee5cbf223b087e77d06853c7df50"],["/tags/数据结构和算法/index.html","ab6ebe1fb7e54f0404a32bc82ea73716"],["/tags/数据结构和算法/page/2/index.html","0ed0e57582d2c1b4ec75edaecb07b1dd"],["/tags/数据结构和算法/page/3/index.html","631eb2377d8160f81b5777a693073c34"],["/tags/数据结构和算法/page/4/index.html","3dcbe3155fdd3d280e5166e20fa2c4c9"],["/tags/数组和字符串/index.html","42e7255b483cbf618e583389025e8b5d"],["/tags/数论/index.html","2cc7ec08fcd4c8cd75b96824ada32361"],["/tags/枚举类/index.html","8b695088a5e1df89f1c4049ded4c1c31"],["/tags/栈和队列/index.html","3712aed93c8f6b96e61169a635b3dbcc"],["/tags/树论/index.html","063df2dd8997811336e536dc34acfc9b"],["/tags/测试/index.html","6e209697c51c0d35d9662b8c79735e78"],["/tags/环境/index.html","84804b3fbdb497a9ba8d22220f0b0d2e"],["/tags/环境变量/index.html","904223309222cf8ba16659631a3f6ba7"],["/tags/绘图/index.html","cf3259d2ff8dedac7bedb8b89bdcfa8a"],["/tags/编程工具/index.html","9fce853d1e788c9f51c26032275b74f8"],["/tags/编程环境/index.html","a0b662fbfc5549eb6178914109464172"],["/tags/网络编程/index.html","48aa9541d7b84500d3bab7fbad1dc025"],["/tags/英语语法/index.html","7a9613380b5f31fb3ea5dce01db584ad"],["/tags/计算机操作系统/index.html","9d863d359897c4b0480baf0886a45e2f"],["/tags/论文/index.html","1553f7fb842bfabafd3e64c36b32c6fe"],["/tags/资源下载/index.html","6fc4f5bbcb5f4ef295c060b39f36db41"],["/tags/链表/index.html","cdd49fe38eb37f42358684a8f82dc2e9"],["/tags/集合/index.html","10c5b3e7bad5d7679219168abead5aa9"],["/tags/集群/index.html","6c0445043c719248013cd255e75b5df8"]];
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
