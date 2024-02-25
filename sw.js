/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4b1408951538faa7ef7fa627bc2dfafe"],["/about/index.html","dcdb3f9f27662732c50fc59afa6e63c0"],["/archives/2023/01/index.html","053eaaed5a258f49d13c670b889c5944"],["/archives/2023/02/index.html","6e8aae70c91834a0c11cd30c05ec1da5"],["/archives/2023/02/page/2/index.html","0f2d1dde9f83ea03c04fff9b7a1b23c5"],["/archives/2023/03/index.html","aa891e8d417db3e73f227664eb818d62"],["/archives/2023/05/index.html","bd2c694a8783cd55ef26ca9568e783c9"],["/archives/2023/06/index.html","b760d5350f42f0001ebd542c55266421"],["/archives/2023/09/index.html","0ce950590b7c564c07a2cc2b86f983d4"],["/archives/2023/11/index.html","7b07815a86e40ab5583ab7589f87fb77"],["/archives/2023/12/index.html","c343f2d8642ff6d288891aa7b841a98c"],["/archives/2023/index.html","88f3d082891722fe4ad9eccb4693b4b3"],["/archives/2023/page/2/index.html","74046a43dd386acf712c75091dd98731"],["/archives/2023/page/3/index.html","7fc63090bbabd5d8fc143ade359d10ca"],["/archives/2023/page/4/index.html","6b6342968cfc5e25fddb3d1cfb4fc2aa"],["/archives/2024/02/index.html","a6306cfa723f203e6ed0f0652255e6c6"],["/archives/2024/index.html","09c37e6d44c33243752c4cf3a145a113"],["/archives/index.html","6de4db05b4755258c696632cddca0d56"],["/archives/page/2/index.html","3c5b4c189cb75390090308fbc94c4b0b"],["/archives/page/3/index.html","ad7a9e24702c39a6656303f5c125c58c"],["/archives/page/4/index.html","66afd2ae5d022af2e03e566306f6876b"],["/baidu_verify_codeva-qQP2iZOMLX.html","3b455895faf0a711a8436f1fd975e098"],["/categories/Java/index.html","41a21ae973b8727b3b7531dfb71af6cb"],["/categories/Java/后端/index.html","9aa9c8414809028a988e027b30e1a7c3"],["/categories/Java/基础/index.html","4699944436b11e510356d2568751faad"],["/categories/Java/基础/集合/index.html","60ded04e1fc304e9bccde132abcfca4d"],["/categories/Python/index.html","c7d31a000e06950f09616eb3fe4d32ac"],["/categories/Python/编程环境/index.html","0066b4c9a070b2b58f3d0ba218dc3b49"],["/categories/R语言/index.html","836aeb0fa46c2fd9b17031406c511a23"],["/categories/R语言/编程环境/index.html","878ef80c1ff9a6b6e4c567e6a2f5cfbb"],["/categories/index.html","c6df86b98fe752d15d1fb9fbaf17ae17"],["/categories/中间件/index.html","3ee1a7132691dc3d64ba9633fec9552b"],["/categories/前端/Vue/index.html","6a465d048a2e572108caaad60c619525"],["/categories/前端/index.html","adbd1726b27697f992ec4af846790eaf"],["/categories/大数据开发/ElasticSearch/index.html","8d8efa2ca4b67fa50c23e990b6e8273d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","21000d8e9efe734f4c3bc909939ab7d1"],["/categories/大数据开发/HBase/index.html","719f60d736e0bdf79e8fb5f363325d33"],["/categories/大数据开发/HBase/学习笔记/index.html","296838d64271ea270e75e50318192a29"],["/categories/大数据开发/HBase/环境搭建/index.html","b84c0ac264c90de1bc12a50055052eaf"],["/categories/大数据开发/Hadoop/index.html","aaecee3e6e9a5f48cc1ca9c099b8cea0"],["/categories/大数据开发/Hadoop/技术/index.html","c442957b79f580e3b3307712be688380"],["/categories/大数据开发/Hadoop/环境搭建/index.html","188e653003bfc0f5428233f9b6892ff2"],["/categories/大数据开发/Redis/index.html","7dc656357015bb0c1e4104721c19d75c"],["/categories/大数据开发/Redis/技术/index.html","905ed8c0aff9f231670757faf2e47fd1"],["/categories/大数据开发/Redis/环境搭建/index.html","6fef4a291e8c9855c790fda2da7a4180"],["/categories/大数据开发/Spark/index.html","2e20daeafe0a0deda016ef32340d85e7"],["/categories/大数据开发/Spark/环境搭建/index.html","2db9bbaa5af79e3644525a47e9e65123"],["/categories/大数据开发/Zookeeper/index.html","5e97521333d49f59a0229daf88a3dbf8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","a91f5114ace0b39a9fa8bd54ac842b5c"],["/categories/大数据开发/index.html","072c7ddca11944818261fbfc215587f8"],["/categories/学校课程/index.html","5c79f6d4add130b4c071bf359d34f873"],["/categories/学校课程/计算机操作系统/index.html","a369d299fa6f3091a80540d389a4b34c"],["/categories/操作系统/Linux/index.html","4e6a0bfdd467bf8acc25bccee09fa6bf"],["/categories/操作系统/Mac/index.html","6699be845fe4f7bfb44e59201e578b12"],["/categories/操作系统/Windows/index.html","9cb28e335afa762cdc7859e618086b42"],["/categories/操作系统/index.html","3b64b77990c9827b8193feb8a12409c6"],["/categories/数学建模/index.html","d9155fad2e06a4e055d50246d15e034b"],["/categories/数学建模/latex/index.html","361a788d70d8ecec700bdb188ff62f13"],["/categories/数学建模/优化类/index.html","cfdf57fdf4b455c0febd70f7a771cf23"],["/categories/数学建模/优化类/现代优化算法/index.html","dc6a0971435c46e331b7c3ecf60e8780"],["/categories/数学建模/优化类/规划类/index.html","8274cdd7644b72f731b9c93d7d14564f"],["/categories/数学建模/绘图/index.html","30b0b4b770f6b42b9efea20d64db02f2"],["/categories/数据库/MySQL/index.html","554f1616f0f8473f83d919be2a90b7ee"],["/categories/数据库/index.html","bf512e10e5a6611aab5986504569b4a3"],["/categories/数据结构和算法/index.html","395c11ba568fd84d2d5dafc855b49e77"],["/categories/数据结构和算法/page/2/index.html","2352bda1dea3846e1ee86d16facb4ba0"],["/categories/数据结构和算法/基本原理/bfs/index.html","e2803facaf98b7d263c8464593cf6b40"],["/categories/数据结构和算法/基本原理/dfs/index.html","4c9bc46223d9fffe7e334ca2c4d44f5d"],["/categories/数据结构和算法/基本原理/index.html","3427dce0d8fe4ff91323b94304c8e4c4"],["/categories/数据结构和算法/基本原理/动态规划/index.html","cdc6d079c4ad1fb88a17a4075874ea14"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","80179ee410f76dfca37867fa25097a37"],["/categories/数据结构和算法/基本原理/图论/index.html","1960004a38e1c19cb904d71b10e49812"],["/categories/数据结构和算法/基本原理/字符串/index.html","4b32a42941a39ed47df548692876f2e7"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","e5c6535602b19013a8c8e94558859de1"],["/categories/数据结构和算法/基本原理/数论/index.html","ccd8727abce99d53a7ff134e6aa13ba8"],["/categories/数据结构和算法/基本原理/树论/index.html","bc841becc5d64780d04a71eb85436ae8"],["/categories/数据结构和算法/基本原理/链表/index.html","33e9e17ba987f792d6b33bf2f8e05951"],["/categories/数据结构和算法/算法题/index.html","f6652215b228120dd2cbcee6cb95d628"],["/categories/数据结构和算法/算法题/二分查找/index.html","16ba7565200cbfcae8f9af56e33f1b05"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","06bd87a754f64cee2b78186b2903c254"],["/categories/数据结构和算法/算法题/动态规划/index.html","22ad8badd439a07b93d75e74b7ded1be"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","4e0d6c3467f546ddcef185cabe259901"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","0107a8a5138a6cfb827c1bad5f84490d"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a433421e4ebd178c761e04338f814328"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","74cc27af871961a8c9b860c6ff0d8652"],["/categories/数据结构和算法/算法题/数论/index.html","76e43ed419d780140f99dce50f78a26c"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f3b8153c1fdbe3328373802ba67c47f9"],["/categories/数据结构和算法/算法题/树论/index.html","ff757e760ca479a114f351ac3ef6f821"],["/categories/杂七杂八/index.html","0ff063a8f038fa484c1083bf3edb1bf2"],["/categories/杂七杂八/博客搭建/index.html","0b8f1c2ef70ddff74e2b526e684320f3"],["/categories/编程工具下载/index.html","9681d426aa52715bca3e510da1c05993"],["/categories/编程环境/index.html","798ff8c22f6ebf318224b77cd4b00c51"],["/categories/编程环境/大数据/index.html","3e742bc56d6d59ad46e314b2c9109264"],["/categories/英语学习/index.html","2326c85f991887a95e2ae6e7959e9532"],["/categories/英语学习/英语语法/index.html","c5bf9d749a9264f77c0b26095f18b80a"],["/comments/index.html","084cc0d2c65b8129f2524dc16bf615f3"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","eb0836e474a86a9b73f9e804c93bac51"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","8a2ecbb51ed53b1c841251185eca5c87"],["/movies/index.html","7032ef9098a2777dfac2fd1bc4ce539a"],["/music/index.html","c8ff5ff5c5d91f2024f0e9771e4495a9"],["/page/2/index.html","422b9cdf05946865340cf292f463e558"],["/page/3/index.html","b00e749837ded1aa8b6f7f9b559ad8d0"],["/page/4/index.html","882197d41a147ba2333c6388865ecab8"],["/page/5/index.html","8521fd2f47a69044afd2766a8fdde8a3"],["/page/6/index.html","10d02d0a4faa8b58f1e046e65beeae26"],["/posts/1021360842.html","b8bf59876b79f9e864ec82389a7160ea"],["/posts/1120620192.html","7c987889090ed24cfb5772773716078a"],["/posts/1141628095.html","33eca482686fec2dd8f1dc13cd8cd70c"],["/posts/1168613674.html","2b74691c308dd5616f37d56f4243c4b7"],["/posts/1219920510.html","addaeb79d6e996af8d0134d01cdf0705"],["/posts/1222166338.html","ecff68411588ed5712269081c2621b18"],["/posts/1259097482.html","b0bad70ddae85201ca252dc0e66c6e74"],["/posts/1271036369.html","c0c5a90ccce2b3564484d9f0da19c6f7"],["/posts/1312847445.html","cefbcb92f80c752d658ab72b3aea00b0"],["/posts/135355774.html","153e32390d9bb651920d803b7b38d719"],["/posts/1375344716.html","bde36dc8eb215357f9284f04195d2834"],["/posts/1388991698.html","adad2b6a4549444e48b3aa10ff14ba77"],["/posts/1410315814.html","f3adc263a699328e0c3088a7a5dc7243"],["/posts/1452790229.html","ae4857538f356da453be08ba79cb699f"],["/posts/1470079884.html","dbf70236266876a66c9b81f69f0717a1"],["/posts/1470079885.html","08a7fcce79e2bc89a373368411ecefcd"],["/posts/1470079886.html","2e34bcce303c9d19fd857dc93d1a53c0"],["/posts/1470079887.html","1d4c2bef4da1132cea8b46bdcaabb560"],["/posts/1498536549.html","1d9e2ffd80830215f0a42bc025e1dd29"],["/posts/1539568593.html","0a4e2e707abd38dd7dca8acbb4461ba7"],["/posts/1547067935.html","5f15ad4351ba1478ebf4a48cf83694cc"],["/posts/1557866301.html","45562a450c93ed8ba2e46eecfea2a54c"],["/posts/1571776361.html","b99a8002e7d27860aaa0f8ff12a16bac"],["/posts/1605124548.html","2b23878ec00b580ac1838e8f1d65e8d3"],["/posts/1633036852.html","91523037dc2dc44e6b7dcaf79cd46cb8"],["/posts/1674202625.html","ceef095f95fe561570717798a1dde1a0"],["/posts/1765123828.html","a4ee9f586212ee7c0e7ef96807d896d5"],["/posts/1767336200.html","60ddfc276246febdf3a33614b85da4a7"],["/posts/1776114197.html","364770337671796ab92b3e696dcfeee4"],["/posts/1817748743.html","2ecae2c3dd3e536778c4969de7705554"],["/posts/1925125395.html","76b2b69cf529c9dce3469e145a98d530"],["/posts/1966191251.html","a97354607a27f0bba4b2fd69c466b2d2"],["/posts/1987617322.html","63857bccd450d3405862b27a80ae82ca"],["/posts/1999788039.html","b6a47cc30489c114cd856b6d56d39385"],["/posts/2075104059.html","5eebd55fc276e0eb03e107e09278c2a5"],["/posts/2087796737.html","ac0219a895aefb27bf7602ea1c57311b"],["/posts/2106547339.html","7e6082f4b333cc852e0d32690a89d0d6"],["/posts/2207806286.html","b41d5a889022a3d89d3d0a236f3edff6"],["/posts/2225903441.html","18b97cfe45a625c018c3aed1d866f95e"],["/posts/2265610284.html","196da5d9d8c71948a2348fe09f3febba"],["/posts/2281352001.html","d65a2b7af454004f956d0b9fa5c89b8f"],["/posts/2364755265.html","09f8ff340316e88755d2d2eb6b7f66e0"],["/posts/2414116852.html","82c5ddb3b6458c0683554f7850049f5a"],["/posts/2421785022.html","83f92f5ff870521e81567186a62db624"],["/posts/2482902029.html","f7a3984f365ae972afe3b43dfb5aebce"],["/posts/2495386210.html","e862cb4d5ff0d277731a4ea7dca3fcb2"],["/posts/2516528882.html","0bac857224dcb157251b58526212dda5"],["/posts/2526659543.html","a6ececef1bb08e45816fbf92f5bac328"],["/posts/2529807823.html","eadfbd32c85318cbcf9117646ba0a3de"],["/posts/2596601004.html","17ed3103cf88b48d08f733947245f2db"],["/posts/2697614349.html","e21d3c3f3c9bd577d1a756ff45aca3a4"],["/posts/2742438348.html","68292579f2caca7bfb4f374d6b1a16f5"],["/posts/2768249503.html","12c0d44780736ea404d4f7907ede29ec"],["/posts/2864584994.html","0c78e6eeeb16e5a1e975bc72b38b99c8"],["/posts/2888309600.html","e1c1af6cdf3dbbf84a3648a6de51b55a"],["/posts/2891591958.html","72949149e2b7ee0fa352da24c1038226"],["/posts/2909934084.html","24c05184e4ee84950f13f97402f27e90"],["/posts/2920256992.html","0d5570f4daad1834b38a12acd166d128"],["/posts/2959474469.html","ed3e89104c1d15d58e269d3243905e6f"],["/posts/3005926051.html","0146e467695e67f8b036be0128fa515c"],["/posts/309775400.html","af5d4245aa36acc1c4d92e639dcf16e4"],["/posts/3156194925.html","8f60ec110fd3bd73e876480a422ccd87"],["/posts/3169224211.html","4d78b21f3299a4fc16ccdb1325dd3eab"],["/posts/3213899550.html","6d8032ec9426a0d493b092b2f8429188"],["/posts/3259212833.html","eaed21a8298bbc425a7eb63235fdbe1b"],["/posts/3266130344.html","73419abd40c894ec18b612e62803e03a"],["/posts/3292663995.html","3e3470ddc846ad7bfc4df2c2d8120425"],["/posts/3297135020.html","8e9ba00586f07cc4c8edc0466c3bf7aa"],["/posts/3306641566.html","69bc64aeba2a5d834c789083858eb041"],["/posts/3312011324.html","95851dfa94b687d487a01d783c043f8e"],["/posts/336911618.html","de029f28e0f101c605e5492f580b9e6d"],["/posts/3402121571.html","289050e3e6e1e27fd5f4473210b58e06"],["/posts/3405577485.html","50356152b0dc95fcec212cb9182dd5b1"],["/posts/3498516849.html","429005700d212c130b15cf7f8ac9f21a"],["/posts/3513711414.html","5ff2d822f10af667a229fd6aac33f996"],["/posts/3523095624.html","dd17c8aee96c1423ead431e0e505460e"],["/posts/3546711884.html","0743f8b6c9b83a733ace699f4ee88b50"],["/posts/3731385230.html","def80a47e40647493399ba391d9d13be"],["/posts/3772089482.html","b5e6afb6c936c61296f4fa999117bba7"],["/posts/386609427.html","e467023634688de19818bc4c31d4288d"],["/posts/4044235327.html","e39f7c08ff903672db60580e40e377cc"],["/posts/4115971639.html","7bc2e730c40c7f433ee097cadc634724"],["/posts/4130790367.html","3eea4ab8e26a8243f09b8a49a852228b"],["/posts/4131986683.html","c4611c8fb19334b204e25b3b6fd5a286"],["/posts/4177218757.html","a84ed576a80aa356dc8b603bd1213145"],["/posts/4192183953.html","605e5a416aa4c6a54fe4f357d9ae8b9a"],["/posts/4261103898.html","e5679f3f4ba57342baaa47bdd023721f"],["/posts/469711973.html","9c805f501d7474e561e7e1eafe5926bc"],["/posts/482495853.html","6c41f8926aeac462f91b0f4df3f309ed"],["/posts/488247922.html","22288aecfca5fcf0f04dd816db9ca251"],["/posts/517302816.html","209064e1a88c75e33df253e82b3ae5b7"],["/posts/570165348.html","6c9287f115694baeaa7136f47d19c0c1"],["/posts/595890772.html","d51308967fcd63a483c6ba83a445d28e"],["/posts/67485572.html","d915b57e05c857b004dc8aea98fdee5d"],["/posts/694347442.html","6aee74c2f56f2652730532858c1b0eac"],["/posts/707384687.html","48adcb17d6c590cc6189abf7be01a107"],["/posts/71180092.html","7110ffec3082d79332f506132d9eb916"],["/posts/716459272.html","e63edc14561fe393a35b9917417e4d0b"],["/posts/765481613.html","90a1ec6af197d41b730dc2577e5d1653"],["/posts/778231993.html","bea7c6d9fc6f55e731502b3554723039"],["/posts/795397410.html","76af3f01d47ddc5d79ec6fa358546655"],["/posts/820223701.html","3bf21de31682f4897137ec9cc033af73"],["/posts/830372185.html","070666bd15919eb0eca6e8517c0e8ccb"],["/posts/88294277.html","38b349abd2da50034e65afcd64275d37"],["/posts/939963535.html","60e55c72eaee1aed124f9000218145d1"],["/posts/983786067.html","095cedffeb503babe6a64b4b22ba43f5"],["/sw-register.js","98315e6e60c32bc8f21034a1198c2ce6"],["/tags/C/index.html","e7c955b8d95915514929c0aa867b0a58"],["/tags/C/page/2/index.html","f8e2145654ea6a8f5e6f60819ba5df19"],["/tags/C/page/3/index.html","d7f68c6a05a26c23bbec58150114d710"],["/tags/C/page/4/index.html","92fe943a3d1d954965d33c3569ca5f2f"],["/tags/ETL/index.html","ac0ef612c1ad56263c04735621aeef1a"],["/tags/ElasticSearch/index.html","aca48b0c37b81d2cc9225040313abd95"],["/tags/GUI/index.html","a505c778ae8f6418334764d265aef06e"],["/tags/HBase/index.html","8376eaec1e66fbf5023ef87d9482b65b"],["/tags/Hadoop/index.html","a81894964099d818cb78dadd7d1e3d18"],["/tags/Hadoop/page/2/index.html","b60173fde104fdf481339286744690d0"],["/tags/Java/index.html","0c2b28835c053e758bc7f5e32c64715e"],["/tags/Java后端/index.html","9c3785d2bfef905a12d0900fff0b54c2"],["/tags/Java后端/page/2/index.html","242a3deaee02c51dfacb4548da09d240"],["/tags/Java基础/index.html","bdb5a93fbf368879f44ff3baa2fe6034"],["/tags/Java基础/page/2/index.html","466825cac5a30440f435337127d896e6"],["/tags/Kettle/index.html","153111bf74a94436fffd223013dcee6a"],["/tags/Kibana/index.html","b58b2eb95a6107134998604f26a7d4e1"],["/tags/Linux/index.html","69d53f7ad0e4a7809dd1cb6e49f9d54f"],["/tags/Linux/page/2/index.html","1e213df8caa8c483499128adc7e022a4"],["/tags/Linux/page/3/index.html","510d22207d148c9b8081f5b394c591d2"],["/tags/Mac/index.html","8cf0f36ae5978782863c8b56ac11d24b"],["/tags/Mac/page/2/index.html","d93d9a68f770b11f79ef9911d4bb231a"],["/tags/Maven/index.html","6ad8a3dc0f7f95c8ced3942f5695833d"],["/tags/MySQL/index.html","e8fddeffbd4876d2b1f536151d235dc5"],["/tags/Python/index.html","2703d0d6fb624db034abe86bf905e6d9"],["/tags/Redis/index.html","29a8c8ca66ae0989f56a85863c052b2e"],["/tags/R语言/index.html","371b844fc5ad6b44d2970744c2255319"],["/tags/Spark/index.html","af24181754fe8bc2289d4750f13fead9"],["/tags/Ubuntu/index.html","e402e27df6af68368daa615d078968cc"],["/tags/Vue/index.html","061d70cfc0ce7b85b1865b1637831811"],["/tags/Windows/index.html","a238035c48253b8ba3d146009178154f"],["/tags/ZooKeeper/index.html","46cd98bb6f604854554b5500b2df1540"],["/tags/bfs/index.html","58a9cb18f62bb04e9e38b2d847efc7f3"],["/tags/dfs/index.html","5b33a4882ee7274ad12ea01bcc2e912b"],["/tags/folium/index.html","1645229a90f3d8702ebe68fa3a4c9fb9"],["/tags/git/index.html","6ae211f487e4570c8d98dbb2cfe8131c"],["/tags/index.html","c8bfb62f378b755c397b7caad6e8b2af"],["/tags/latex/index.html","e4fdd641c9fe034a7ae41ac413eec23a"],["/tags/中间件/index.html","708f9f6613b41e9b8849f210de9d10f7"],["/tags/二分查找/index.html","2e5b40b07b65ad7b7b5cbf4b0fdbede6"],["/tags/优化类/index.html","b9e728de218e2071800981a68fdd324e"],["/tags/前端/index.html","33252cb5522395c4e128d606f10775ce"],["/tags/前缀和与差分/index.html","0f0709e08c3c079836fafb5a0bf27e03"],["/tags/动态规划/index.html","76f465f950129ae05f46c6f43779ee39"],["/tags/动态规划/page/2/index.html","8b16b935f61f56a210332b29ee96287f"],["/tags/博客搭建/index.html","09daffaa92de17af413616227df30991"],["/tags/图论/index.html","a2f40995fb262eda6661af5362aeba05"],["/tags/大数据/index.html","b32e6102872e51d6690488d0da2916f8"],["/tags/大数据/page/2/index.html","1f8ffd2822113a2fd0c2360b95448fed"],["/tags/操作系统/index.html","0d8ac8efdb0b62d460a57e1c5784a35f"],["/tags/数学建模/index.html","9e7b37749d666725c713d2572f4a8feb"],["/tags/数据库/index.html","22fe7a6496116cb04303ee1dd5cf65b8"],["/tags/数据结构和算法/index.html","2b8a58b757fd67595a09f908ff113479"],["/tags/数据结构和算法/page/2/index.html","e8e45123ebf8202742506cfc061d0e3a"],["/tags/数据结构和算法/page/3/index.html","2235388b67adaf2c4bc7c1435e720b5b"],["/tags/数据结构和算法/page/4/index.html","4daead65f1a4736b3d31d761fa27cb39"],["/tags/数组和字符串/index.html","78c333da99c0d52a229bf977bc36ad1f"],["/tags/数论/index.html","5acb41566274f40365492dbe35f3e5ef"],["/tags/枚举类/index.html","13d87be88b47355f1b465e2facc33f57"],["/tags/栈和队列/index.html","5a110f52590dcaf380af8b7ecaac390f"],["/tags/树论/index.html","f2d48c66752558fd7b5d23abacb969d9"],["/tags/测试/index.html","e9321f3f78a7d6b793bd54f838d890ea"],["/tags/环境/index.html","548c0708b328248b6ad1d3717c5c2371"],["/tags/环境变量/index.html","b56f39818cc1aaf9bbaa5ba6c35b604a"],["/tags/绘图/index.html","619dbb346214eeda5882a5e0df64f10d"],["/tags/编程工具/index.html","846cbc38e0d563cfb4adced99804487c"],["/tags/编程环境/index.html","4044d8231eba77dfb4b8385455bf065c"],["/tags/网络编程/index.html","9aff228f84e6b914daf6afa8a210204b"],["/tags/英语语法/index.html","0fe86642101f6ae0a43b383013ad1acf"],["/tags/计算机操作系统/index.html","7982748a605efe173cf3fe1e6657b1f5"],["/tags/论文/index.html","be61cb01b29176a8c92033ed38eea0a4"],["/tags/资源下载/index.html","034fc14f25d082ee767385f3b7218fc8"],["/tags/链表/index.html","ea7abca50aa8a4f927ce57f246cad332"],["/tags/集合/index.html","14e1314f7d1da22642cca20af20b077e"],["/tags/集群/index.html","bfcf6f4d68fd2676254c05737c79dbbb"]];
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
