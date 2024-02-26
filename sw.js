/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","58c1e96284113235affc97cec5ec1c60"],["/about/index.html","a27a18c0cb2fb5beddaa2f9f3cf682ee"],["/archives/2023/01/index.html","bf67cb57886668783c63768795d65706"],["/archives/2023/02/index.html","447a3e3bb22d29ccd3222da1039d06e4"],["/archives/2023/02/page/2/index.html","17e62acb54c2fab56251058e3e5abc99"],["/archives/2023/03/index.html","93fdcaf24c0a0141e26b1f6570128d9c"],["/archives/2023/05/index.html","3340f10dee296b9bcf7f97c8197a65bf"],["/archives/2023/06/index.html","5486f7b983379c39d7052ca5e1a4b0ca"],["/archives/2023/09/index.html","4d88f35feb9c14eb9e83519de9a4eeb6"],["/archives/2023/11/index.html","316beaf92cbf9b2a9d7ba35698a0d2bb"],["/archives/2023/12/index.html","d3bd723e6a7426488cc036912134f140"],["/archives/2023/index.html","2938d12854a8fcb50ccf933509c763ab"],["/archives/2023/page/2/index.html","3e593d60f2891d7a184b50a15dcb6d26"],["/archives/2023/page/3/index.html","2b6e460aa18caba44deefe6b089daa1b"],["/archives/2023/page/4/index.html","57043a4ac810865e92fd38eaa84b68ee"],["/archives/2024/02/index.html","45e6419dac11327ea5bd1080cc0f93e2"],["/archives/2024/index.html","ed8a0ded862da9fecd22b655b0f723a4"],["/archives/index.html","29a36468b2a0930f59f12cdcb78afe57"],["/archives/page/2/index.html","95a2ba133882823f04dc5808610293f9"],["/archives/page/3/index.html","899d8c9b825dfec28e06e2a56a488ad1"],["/archives/page/4/index.html","01fa542967819c3dcc329d2fdca45e13"],["/baidu_verify_codeva-qQP2iZOMLX.html","099928659dbd0ad371d44fa36a973039"],["/categories/Java/index.html","21700edfa5140498809447ea8e286f29"],["/categories/Java/后端/index.html","d9357ce45cb524187dc1c8f0445a190f"],["/categories/Java/基础/index.html","8fb521a067c8717613b5a9222f2c8f2b"],["/categories/Java/基础/集合/index.html","bdcd6295ff9ef21c95ad71d4f1e339e8"],["/categories/Python/index.html","3f66cf6e7c89eff81dcd91d3f4754b89"],["/categories/Python/编程环境/index.html","d5a142af91b5f37b683fa4e55a66a067"],["/categories/R语言/index.html","297acd040c268be8f325bc6630c25244"],["/categories/R语言/编程环境/index.html","aaa4ad65072d66299b4b1aec9cb5ec1b"],["/categories/iPad/index.html","520cea736ec785fb23339f0759c617fe"],["/categories/index.html","2b50978a4de18b2d15ed737fec4cb152"],["/categories/中间件/index.html","b58515d421282bdec94760674f622e67"],["/categories/前端/Vue/index.html","ee2d3cc36bd5ac2f471d7f5376ce6cfb"],["/categories/前端/index.html","1d523d5e4c193b6096937b1479e36b7e"],["/categories/大数据开发/ElasticSearch/index.html","147b49f5ddc29c2caeab3f040b21dc1d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","e1f9cc78161c9150ea79ae90fcf54d9c"],["/categories/大数据开发/HBase/index.html","866e37702c1f720631c30b4eb7caa09b"],["/categories/大数据开发/HBase/学习笔记/index.html","25dc6e1a80bbe146c2df02cef6f269df"],["/categories/大数据开发/HBase/环境搭建/index.html","40f7ffd3e1d8d90ed80f32745c58b5ac"],["/categories/大数据开发/Hadoop/index.html","ab11bee0e177321b17e54d30d913510e"],["/categories/大数据开发/Hadoop/技术/index.html","0c070f05f347c91ebaca216551cbf2ef"],["/categories/大数据开发/Hadoop/环境搭建/index.html","8a2b587fc4e47647589e863f8a4a088c"],["/categories/大数据开发/Redis/index.html","b3573041786ca00cdbc90cba2a9da323"],["/categories/大数据开发/Redis/技术/index.html","3bdfda4b2a69e6e9a13cc991e2612af0"],["/categories/大数据开发/Redis/环境搭建/index.html","020d320a5884fc26c62c7737e7ada55f"],["/categories/大数据开发/Spark/index.html","bfd56af4c49d79c2f4361057ab84fb2b"],["/categories/大数据开发/Spark/环境搭建/index.html","fb8262d45e62bc2894a2c19cbf5ad93d"],["/categories/大数据开发/Zookeeper/index.html","0cdbb5132617d6fa75d7461ab35dac69"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","41bc06690ce5eb5a4067880681f401d7"],["/categories/大数据开发/index.html","cd1e51d94a9c3312efd57b8d0de66297"],["/categories/学校课程/index.html","5bd0d88666ee7eca5b2926cbe6b9d3c3"],["/categories/学校课程/计算机操作系统/index.html","d8f8aa387cc4132b9ddd5f3fb18c1007"],["/categories/操作系统/Linux/index.html","f914c30f378fbb5b6819c1cedcd6e6ae"],["/categories/操作系统/Mac/index.html","4ba60d1617e7c1015b919d54e628d607"],["/categories/操作系统/Windows/index.html","a84ee67d33ebf6c17efed6f61bd256e7"],["/categories/操作系统/index.html","7ef29289c1cb3c910c3035c284c3d119"],["/categories/数学建模/index.html","28c98e29e430efa19954bbe1da0a8a6f"],["/categories/数学建模/latex/index.html","b218dce51e0e01367226b3b6070fe1c5"],["/categories/数学建模/优化类/index.html","e05d6b841712d4928c9c14f6e251b09e"],["/categories/数学建模/优化类/现代优化算法/index.html","5d44d0ad430967d4ee4efb508ec44f0a"],["/categories/数学建模/优化类/规划类/index.html","5ef957e74df94362de2d832460f1a19d"],["/categories/数学建模/绘图/index.html","594a5ff62ca75c942ee67af6662aec4c"],["/categories/数据库/MySQL/index.html","ac3f123e2231ad6af00472f44f10d3b5"],["/categories/数据库/index.html","e9261b17f952504d982c1153128a7fc4"],["/categories/数据结构和算法/index.html","5938fb58a75491c236f98be5d1e13abf"],["/categories/数据结构和算法/page/2/index.html","f785e73ee83ac55929995832204ae9a0"],["/categories/数据结构和算法/基本原理/bfs/index.html","c9cf84a8285b05d5750cd8f038c94bff"],["/categories/数据结构和算法/基本原理/dfs/index.html","1d07588baceeab311cb371dbd3325e39"],["/categories/数据结构和算法/基本原理/index.html","1bd83ab88ff661fed0ac44de2a6e5a63"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c9e1c187a493b3fd39bd946dad27855a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","31c2d25bfddb3cc5b724b8a906738fb2"],["/categories/数据结构和算法/基本原理/图论/index.html","cb43e52a8600d276a0695a4e9cda0edd"],["/categories/数据结构和算法/基本原理/字符串/index.html","a9aa050638fc4fbde70029ea527cff68"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a5421258d4de1e73102e8ccdb2e5ee12"],["/categories/数据结构和算法/基本原理/数论/index.html","66c1fe363218f03016782c1d37b64023"],["/categories/数据结构和算法/基本原理/树论/index.html","08aa73a2e5c256507045c8aeecd39d5c"],["/categories/数据结构和算法/基本原理/链表/index.html","53227acb7315de397833792fcb279604"],["/categories/数据结构和算法/算法题/index.html","6bdec174e00f921ea93e25216a3b4ace"],["/categories/数据结构和算法/算法题/二分查找/index.html","876ffecac07c1138685d86ff44e0b16e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","8550a4f141af2e95f9afaae3553c4fc0"],["/categories/数据结构和算法/算法题/动态规划/index.html","16de0f8a26b7b8304bb95f08744f922e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d684cbefaf5db65373ef59ae2896d691"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5b5cadc37102b93384e2bf8bbde10386"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","48b9f2b39a7ec4e296568545b2ed31f6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","837990edc5168078d75a2a454ebac313"],["/categories/数据结构和算法/算法题/数论/index.html","6b951545cd9259433bb754fa38d2dece"],["/categories/数据结构和算法/算法题/栈和队列/index.html","708949b641218427357a893d623da2a1"],["/categories/数据结构和算法/算法题/树论/index.html","45781f9794a8e70fa9ee95efbf46dd28"],["/categories/杂七杂八/index.html","b63247680087473cfd97f4c064934ae8"],["/categories/杂七杂八/博客搭建/index.html","1f079af192d8cb39b5ac0a2418e29df8"],["/categories/编程工具下载/index.html","c85c1a0fa360a73ea753ee3640adab08"],["/categories/编程环境/index.html","1972440026c8cd39974a874327b7b543"],["/categories/编程环境/大数据/index.html","62338119cd3f4861db99ba11deb0f895"],["/categories/英语学习/index.html","bfd7a8916bbe6bb09ebb253eec203173"],["/categories/英语学习/英语语法/index.html","bb5173dc6d15c2e234ad5aec4d62c9de"],["/comments/index.html","43c981a57b47caa41f2b9f9e4080eae6"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0850e7ed30432361a99554fdcfe126e4"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ca2db0530a1a3d1f4bbda886d8d4ef46"],["/movies/index.html","1d74a648681cc64120b7c8085edb12e1"],["/music/index.html","69a00c0495d4a7811f5b0829f425d35b"],["/page/2/index.html","46c4b2994269e9e1f6cef99b8dd6da25"],["/page/3/index.html","325d746f15890458b5d425bb617efdea"],["/page/4/index.html","02637d0a5a26dcaa8e6b701799bc9fee"],["/page/5/index.html","94c9d5e24686cb0cd2e5e22f323a2aae"],["/page/6/index.html","ab3d1ba35623442a372c0af78b3e502b"],["/posts/1021360842.html","391792bec6af4bb4ce25e4ca5f950e4f"],["/posts/1120620192.html","df551471d9a44ca1a276fd852b3729e4"],["/posts/1141628095.html","c3068954a907a0a783727d186e7e905d"],["/posts/1168613674.html","d9c6ea3f725b890aac624276cfa1e79b"],["/posts/1219920510.html","694204d871b430f657860569d13f9bf8"],["/posts/1222166338.html","8f007030b355db5d69b3269e7a5f2d59"],["/posts/1259097482.html","0292b6654208407195a15f1f60505769"],["/posts/1271036369.html","27bc66545d9f704cfab0a346288ef450"],["/posts/1312847445.html","9fee76f7801f18619587f3fe5b42eba1"],["/posts/135355774.html","d01030a557df2c1c826239d3eee41f8c"],["/posts/1375344716.html","ba76300e6c447cf99c3cbea70ff152a1"],["/posts/1388991698.html","e7fa49556d1382adeea7d43062a45bd9"],["/posts/1410315814.html","4c5dae80d4cecc9e1036f01bd8f3fd1d"],["/posts/1452790229.html","8fe38ae9e842cce9081f1bd0e01632b2"],["/posts/1470079884.html","6a1d89139b47cd733bbaeebdc1cf6c70"],["/posts/1470079885.html","6ae8f7883218dc28f6ce8458e5a9beb9"],["/posts/1470079886.html","fb195b268c4b9830c9fe733a48661845"],["/posts/1470079887.html","570ab21337c36f36a64e153ff1589115"],["/posts/1498536549.html","f1b7602b85f4f85750a358542e542225"],["/posts/1539568593.html","818332027cc483967521945fc445bf94"],["/posts/1547067935.html","336463b329f738791996f428bf554c78"],["/posts/1557866301.html","123f874cbf89640d4ed7718e2bbd5d9a"],["/posts/1571776361.html","15100078469bd4a97d05ae51047186ad"],["/posts/1605124548.html","f9481a2b71d99a23392c7397ff864565"],["/posts/1633036852.html","87db16aabea417417beb0a004e2d1dce"],["/posts/1674202625.html","c2ddaf84db4d05941bbef70c1e3f5e97"],["/posts/1765123828.html","a60b10d247a58ac6fa0f7fc03bbf9904"],["/posts/1767336200.html","491fe37f940d741c96f87192287ac012"],["/posts/1776114197.html","99b5334a80fd0641cc759a2765489e4e"],["/posts/1817748743.html","b6ae039873b51e2c60d39aa6992e4743"],["/posts/1925125395.html","ad3732348fa3c2fad378c62347a9b8fd"],["/posts/1966191251.html","be3203aa3fc8fe210f57e2c1851c69ee"],["/posts/1987617322.html","a3afa24c84b6341876841ce1bcfd3381"],["/posts/1999788039.html","1f4bb8fa320c0298fead1896a364e992"],["/posts/2075104059.html","709cc7076abe9aa6f1bee2ed59fd47e9"],["/posts/2087796737.html","ba226e89ebeafcaad04746be847ccd0c"],["/posts/2106547339.html","3ac643a9faefa685a3c1420f3dd18580"],["/posts/2207806286.html","a4835cb6f1fe7236b7b244ef5398d7e6"],["/posts/2225903441.html","3ddc79e8c134ef65dcd916082918fe34"],["/posts/2265610284.html","1678037685892b505b6b16474563145f"],["/posts/2281352001.html","8ca3cdcea5e98bb80fc003548ad133de"],["/posts/2364755265.html","2e837c0c883f8e5f53c8d66f0ba9d63b"],["/posts/2414116852.html","ef171b6396030a7719c515c05f765137"],["/posts/2421785022.html","6b815a9ecec0ce969ed0b29917ae1e50"],["/posts/2482902029.html","0053e802dc4e1014ef93fa9b03150720"],["/posts/2495386210.html","92171a2146d724c01d748b4ec0ff1582"],["/posts/2516528882.html","cb87fca6e5c7018cf804cbf685e1bceb"],["/posts/2526659543.html","e50562bd6dbb5cf5375de0cb016f09bb"],["/posts/2529807823.html","409be3ae66950cca8bfd9bbfa33ebbda"],["/posts/2596601004.html","92c8f27a080c4c0ef3dd8ac2e484032d"],["/posts/2697614349.html","ba56bd901bd5e1d4e771fc143ca2b057"],["/posts/2742438348.html","a2bfb6567ea79386a5f6fa3ef0868813"],["/posts/2768249503.html","04342c9a8107668e051b57ec00e9e490"],["/posts/2864584994.html","2ceafbb3ba68459ed890468682963d51"],["/posts/2888309600.html","1d320651020d94046dde5fb3bfd0498c"],["/posts/2891591958.html","78bf4f77010cc779e07ddd9f64709376"],["/posts/2909934084.html","34dfc893be8eec6559aa6487dc37d797"],["/posts/2920256992.html","65f480b971e1ca6134dae866f2cba5b2"],["/posts/2959474469.html","9d643db0e338dd5b4f1ef46dcaa1feee"],["/posts/3005926051.html","038376f7d27d7b3973c3d03b96e57827"],["/posts/309775400.html","69964061d23b80c3ca316a2a2806e163"],["/posts/3156194925.html","e53cb6b12cc562eb00166e395793f09d"],["/posts/3169224211.html","ca1f68e1d9b57ee65b01e925c9ffd0b6"],["/posts/3213899550.html","0a5d02f3a24b389a2bc965f15dc9079c"],["/posts/3259212833.html","578bbffbf8c3eb9c9815d39a353ad843"],["/posts/3266130344.html","5e96c2178a7c4373f5775d58b4b7010f"],["/posts/3292663995.html","23e3b24c4872a324d781179f9f0d5ac1"],["/posts/3297135020.html","57225762b9822a67ea8eb09a29bc1a37"],["/posts/3306641566.html","7e25d7db8ca815370da28a1d1a03c8a8"],["/posts/3312011324.html","75d2af6897229af250c5dcd65cbb44a6"],["/posts/336911618.html","fe72f2066b80d79e3799dde8176eccd1"],["/posts/3402121571.html","8147833c3721940e085c3c634ca2ac4e"],["/posts/3405577485.html","fab33935fffdd984cb1e9bdf65c8cb79"],["/posts/3498516849.html","d9055a22203563f3e21495ace404155e"],["/posts/3513711414.html","29e94c174d46c5c5acda5a6281f80743"],["/posts/3523095624.html","7eed32f7a0eb11376643d16736551534"],["/posts/3546711884.html","5ccc5a66913be9c2d945e311705e02fd"],["/posts/3731385230.html","1f4fb695bf9fc3b2bbefd128ff524ae8"],["/posts/3772089482.html","5f371232aac9a9a089623736452736be"],["/posts/386609427.html","5232baa81061549d5e9d14c76a1139ae"],["/posts/4044235327.html","3b2b9c5919ab953ec7022e41aa4fd6fb"],["/posts/4115971639.html","97719cd8b744379f17084568931190a7"],["/posts/4130790367.html","87e2adb72819d7a4714fed55d6f79c9f"],["/posts/4131986683.html","8fa1c0d7a8e520a9aa397ada69f082ce"],["/posts/4177218757.html","3db33a8324841b4f71ce87fff9253b5b"],["/posts/4192183953.html","70ea182286477f54066eb46d9f1e0b4b"],["/posts/4223662913.html","d41d8cd98f00b204e9800998ecf8427e"],["/posts/4261103898.html","eb675dc0f548f428f542ec4329b636c7"],["/posts/469711973.html","ecdb0e6205e60064415a726f2c199766"],["/posts/482495853.html","a6e363e523834eb33f47bdb8395284e6"],["/posts/488247922.html","6f2dd7b87406d7b7cf5fba07ed99a0b0"],["/posts/517302816.html","8db7796263a40af6be9d7283b1035edd"],["/posts/570165348.html","327ab4d6bed6817c0aeca8dff109d985"],["/posts/595890772.html","2691231b134666596d6fef347627c768"],["/posts/67485572.html","572f0b628145a6d7363151d29f50a982"],["/posts/694347442.html","614ba4a2fdd727a73465782051069fa1"],["/posts/707384687.html","dc58a7b48b877ae2e76560f4d884b563"],["/posts/71180092.html","e3712e230563d9c933bf6aea6cb102af"],["/posts/716459272.html","1756d99c9eed29792c760945f40d5a3b"],["/posts/765481613.html","703e1b65c1741c455ac1596560cd49a5"],["/posts/778231993.html","0edb888ebbb8672a2fa6a8ea115e0483"],["/posts/795397410.html","66108c4be600b509e003f33bfecd0d76"],["/posts/820223701.html","9691854588598d5baa666102e9f58849"],["/posts/830372185.html","4f2b7c8d7d16f770c11d1a9473997b3e"],["/posts/88294277.html","3ef1b4395680a6cd7b876b1cba29398b"],["/posts/939963535.html","53687227e8648e1a1b7754770fc340ab"],["/posts/983786067.html","719029d9d2b641b9190770379b2e252b"],["/sw-register.js","1b2d5249d6fc50b43ffd87f9293097c5"],["/tags/C/index.html","f8ac175a409dc772f1b04def501eb9ab"],["/tags/C/page/2/index.html","ff09fa639fb098d64767362b9cf973a5"],["/tags/C/page/3/index.html","2495521c8fd88f0395afbea56ddb03ba"],["/tags/C/page/4/index.html","2a1a57ae1e051df93caef92a444a3346"],["/tags/ETL/index.html","b7fc26c32e4b645cd6e0e6d8ca4125ae"],["/tags/ElasticSearch/index.html","00bbccbd8e58f58beb9120701d9b3b09"],["/tags/GUI/index.html","e60f66a7c8b01ae3661a8f26478c9fc7"],["/tags/HBase/index.html","f47a2206284d2372b0cd0d82917ca08a"],["/tags/Hadoop/index.html","b690ea47430f861a0abc1933e0ebc695"],["/tags/Hadoop/page/2/index.html","c3e8c354704d5c713dd2224a240a5a4e"],["/tags/Java/index.html","9daed87c48fef1df04a09291b3aa5a56"],["/tags/Java后端/index.html","e6ce5ee2ece21e54b48d8dd77db1da88"],["/tags/Java后端/page/2/index.html","75ba9ee36dfc788707696dd4625f2373"],["/tags/Java基础/index.html","03ec994dc46f556b859a54c69ce0c303"],["/tags/Java基础/page/2/index.html","aafd70e703f97d2f82b892d6cbf77f91"],["/tags/Kettle/index.html","52e5076f892dee12698706d4cbe0981e"],["/tags/Kibana/index.html","0ce8a39875a34a596c69b1606ebd9aff"],["/tags/Linux/index.html","6110fd98134e8af0340958e9557f342d"],["/tags/Linux/page/2/index.html","74147eb28ff0fa698f42d83fcaf1ccea"],["/tags/Linux/page/3/index.html","2bcd92ae8b741d63624664574696b6dd"],["/tags/Mac/index.html","6081ca8dc0fba43bdfb06f735d458d33"],["/tags/Mac/page/2/index.html","207e381cbc814dfe0a614a2391f59e1c"],["/tags/Maven/index.html","053cf33b2370c7ad18be75c7492fd381"],["/tags/MySQL/index.html","980ab7c1c36f32963d1535269707ddab"],["/tags/Python/index.html","7cce4c3434487957489c753f00725682"],["/tags/Redis/index.html","7ada9fcf1915dae9142572bf65a21b52"],["/tags/R语言/index.html","21535e96aea23fcd3e89cfbe0060b1db"],["/tags/Spark/index.html","41391c25be1c68c71a61652b1b0631ff"],["/tags/Ubuntu/index.html","a84ee7a44028ec02df9546f731d53868"],["/tags/Vue/index.html","191786116b103463e49300f5f1a2a281"],["/tags/Windows/index.html","1a862a74e67713108ba405a3d5c98729"],["/tags/ZooKeeper/index.html","b4285ecf6f78db4c0a2e5a2505d332c0"],["/tags/bfs/index.html","7a2c291cefba9ab2e5738303973b0d5b"],["/tags/dfs/index.html","4d73b083f23dee5a1e8bb226ecf1f35f"],["/tags/folium/index.html","d2dbfabf69bff710b9b14d011a2c8684"],["/tags/git/index.html","b2c6ad9e776b895c773871fffb2edabe"],["/tags/iPad找电子书/index.html","0a931cfcd73a23fda6629b86ca54a991"],["/tags/index.html","e919ab794546b8d8d53d83c8438bf43c"],["/tags/latex/index.html","0586b6c38b276e8421b73e0b6a962780"],["/tags/中间件/index.html","92290cc572151a997ff1d6082b75e3ee"],["/tags/二分查找/index.html","e27590e77e5a4c58202f4973db0baa77"],["/tags/优化类/index.html","512940cc4646a03dee48be2dc1f36dca"],["/tags/前端/index.html","249fe624357331270f99742a35d57bd0"],["/tags/前缀和与差分/index.html","a4c5aa3021933ec3d081ddbeb1d3df2d"],["/tags/动态规划/index.html","868c5f7d11b474b00d3dd9f419d17061"],["/tags/动态规划/page/2/index.html","8411c80a59fd5022c0f841197141db2e"],["/tags/博客搭建/index.html","2ae102887310e6dc707f4d4e025e8346"],["/tags/图论/index.html","32988c86d845abac7402343a4b09c767"],["/tags/大数据/index.html","30148f515371859c7ffda2e8630bdde3"],["/tags/大数据/page/2/index.html","cda3d6a492b92101c29015ba8e0376a6"],["/tags/操作系统/index.html","15c8a6083dae40d4b101216606a39a57"],["/tags/数学建模/index.html","cc5cb50b42f17ed0fee4fb5259671719"],["/tags/数据库/index.html","41496ac6037c33d1251b007fe0b94e5f"],["/tags/数据结构和算法/index.html","eb13269385ea586402a0124da4f63586"],["/tags/数据结构和算法/page/2/index.html","0ede316cf42b1676c70158a2a9435b1e"],["/tags/数据结构和算法/page/3/index.html","4e4e3ca9c0982757216572d6d1574487"],["/tags/数据结构和算法/page/4/index.html","3b38f43f28058a682ea44ce7dc779d55"],["/tags/数组和字符串/index.html","756d4556c3feeb112d303da975fe8425"],["/tags/数论/index.html","ad2389f95541cca169dc05df337a0066"],["/tags/枚举类/index.html","9f9a5c84b2b4a40f3b41e4e41279b37e"],["/tags/栈和队列/index.html","462c7b77d267fe2f0558518d26569cb1"],["/tags/树论/index.html","f2d33fad7cfcb2b7fe008c640ca27ce0"],["/tags/测试/index.html","7d09042a3b6e4cbf93590df8c5b8b61e"],["/tags/环境/index.html","046bb610e7c5dbbf36d052a91791fdf8"],["/tags/环境变量/index.html","21cbfd1757776aedc17754cb44cd0aab"],["/tags/绘图/index.html","2560e81aba2f9dc428a794422533ce39"],["/tags/编程工具/index.html","4ce69a737e6db15d39213728fd5821ea"],["/tags/编程环境/index.html","081c8d2aaf34ca03b858125ef4c63d7e"],["/tags/网络编程/index.html","046870dd3b1efb69731a75e89b687b45"],["/tags/英语语法/index.html","ae161728841ce51ed33128f8cfa6da0a"],["/tags/计算机操作系统/index.html","2b2bf5dc9fcdc84c4166994ff0573d9e"],["/tags/论文/index.html","1a51ee4c6672f2d1d1d46000509bdb2a"],["/tags/资源下载/index.html","38fe32cd604c48cf14280a933797995a"],["/tags/链表/index.html","68d7a81aa45e02e2ba3d7424fd122f8d"],["/tags/集合/index.html","521bf28b80aaeef09079ecc7743c2c9c"],["/tags/集群/index.html","13d79bb1054cfd79201d62dd1fd3a2ec"]];
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
