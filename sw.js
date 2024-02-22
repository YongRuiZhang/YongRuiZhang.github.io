/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","931303154c1370f8037c5df9e21d3a59"],["/about/index.html","638ec9bdf44a97e00314d5da410cb077"],["/archives/2023/01/index.html","3b13ac653f82948d77168ffad0cf06ea"],["/archives/2023/02/index.html","87f117049622d61be488efa01e8be9ec"],["/archives/2023/02/page/2/index.html","3c752e852a2a7ed773de1529e7bbbf3a"],["/archives/2023/03/index.html","8bd5497ee99e734b756147a3ed0e78c2"],["/archives/2023/05/index.html","3064bfcc12766ced317bce95b32722a4"],["/archives/2023/06/index.html","2d718997dc104f2d2febf663ee537c1f"],["/archives/2023/09/index.html","9d2bad37f4d40ec56abfdad78f4eb79b"],["/archives/2023/11/index.html","71b7b3eda8af9b4fe5d427bf05a02d4a"],["/archives/2023/12/index.html","77c1aa740c2e7e2446148971693b6318"],["/archives/2023/index.html","832f35992e47489314a7666691776b9c"],["/archives/2023/page/2/index.html","128461165ce540a815f517fab1ce04b8"],["/archives/2023/page/3/index.html","b1f0e3be8f50cfed843e375f6f0b0120"],["/archives/2023/page/4/index.html","54b90e67510e2164805c5e35da5bccd9"],["/archives/2024/02/index.html","487d98ae888c403620a6ab94f7491159"],["/archives/2024/index.html","8892492b370304f0ebd56ce9efa3d319"],["/archives/index.html","2adbb31e9569b121a0a1ac8efc3283dd"],["/archives/page/2/index.html","07f1501f556e76ddf03af9ced6893736"],["/archives/page/3/index.html","8a675e3e6fbda73fb6a26a7c101d5310"],["/archives/page/4/index.html","898f6d0c24762e85e1f4220b6b14b1dc"],["/baidu_verify_codeva-qQP2iZOMLX.html","a5b0092fb3e2b9703b20301b229e1d0a"],["/categories/Java/index.html","4196d1c9d394ed284cd9e2ffa21ffbec"],["/categories/Java/后端/index.html","0d21ea01f47cdd01da960a6801efc599"],["/categories/Java/基础/index.html","2757d42b882d3f82f807fe2c21b35724"],["/categories/Java/基础/集合/index.html","7cca2436f75a24419ce365e6d62a297c"],["/categories/Python/index.html","d724f49070bcf824a682603f17e07b54"],["/categories/Python/编程环境/index.html","ceab6cf0b4d4525d1545a24153e0285c"],["/categories/R语言/index.html","a09e1ae0d79b3079e60eb010dd07ba43"],["/categories/R语言/编程环境/index.html","4b63181c4f10c4bb829f2283e783b0a8"],["/categories/index.html","43548fbc2c9cbc5b7eaf19e66fc4d40f"],["/categories/中间件/index.html","31a0d4f08263abbce241ff2aaf1086fd"],["/categories/前端/Vue/index.html","0a6bdb84e36cf765a7bc7e8ded493578"],["/categories/前端/index.html","4fde914673e4d0ae5db7088ec04be0d8"],["/categories/大数据开发/ElasticSearch/index.html","0150714100705142ab3f6008bfb00ecb"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","370be1f2639d745e3fb47d6912c715d7"],["/categories/大数据开发/HBase/index.html","745fbae2cc8674171cec400754c4c8b8"],["/categories/大数据开发/HBase/学习笔记/index.html","316ae31b0bdec4b2fc9072b10d207701"],["/categories/大数据开发/HBase/环境搭建/index.html","b6e21c8e18744f758fb3438f108afb70"],["/categories/大数据开发/Hadoop/index.html","80c148c61d6127523d10a23d1e63a3ce"],["/categories/大数据开发/Hadoop/技术/index.html","0a61fd041cc3ed68af21cc8c72f95e95"],["/categories/大数据开发/Hadoop/环境搭建/index.html","906dedc042d72845b722cbd3a6095be3"],["/categories/大数据开发/Redis/index.html","5719d5c1dbf5abc4bbd136931093ee4a"],["/categories/大数据开发/Redis/技术/index.html","1e2721a87fd6ce9b2e2f90dc2b3e143e"],["/categories/大数据开发/Redis/环境搭建/index.html","5c283916f9826413036d956dc2996eab"],["/categories/大数据开发/Spark/index.html","3fad2011690366df3bfb0b66cf2c950b"],["/categories/大数据开发/Spark/环境搭建/index.html","d66305369a7ed3b401bf79064c08fae3"],["/categories/大数据开发/Zookeeper/index.html","b2479564f1bbc5718165286998f6d8ee"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","bd2c6db8c838f3bf025f01f208ef3aa4"],["/categories/大数据开发/index.html","5f08bdd075e6875e3257fde69b9057aa"],["/categories/学校课程/index.html","7814ad6b5c85ad1d86d35fe7ffbfb959"],["/categories/学校课程/计算机操作系统/index.html","354293a9b9e42a2ba281e767eb3e17e7"],["/categories/操作系统/Linux/index.html","9c48f219400b88287e24d73924d83694"],["/categories/操作系统/Mac/index.html","81c9fb5c854b9e41999a1daa3fd524ec"],["/categories/操作系统/Windows/index.html","f239ec1b4538fa6fb37bb015d630993f"],["/categories/操作系统/index.html","ea04c68bf5e2781e752ada14604c8f71"],["/categories/数学建模/index.html","dd287784926464c495e57b2cbf70c15c"],["/categories/数学建模/latex/index.html","2ee2976c92d6552a48d674ec2df7a9f5"],["/categories/数学建模/优化类/index.html","96de537eb24336137f3a4e1f38da39fe"],["/categories/数学建模/优化类/现代优化算法/index.html","a0c77f4fa650c6e600e580516d3ec5d3"],["/categories/数学建模/优化类/规划类/index.html","7b8e43ec206386af13bee4062523d4ae"],["/categories/数学建模/绘图/index.html","dfcc9b86ede419417aad546381a5bf41"],["/categories/数据库/MySQL/index.html","bcd6c862e40c9b99673c2d958cfcf10f"],["/categories/数据库/index.html","c2821d8b64d19dece7f75471a70de0be"],["/categories/数据结构和算法/index.html","7ecc507d4d2b899ab2676ca1ac29d1d2"],["/categories/数据结构和算法/page/2/index.html","e9134af00d0a9331187f40fdedcef66b"],["/categories/数据结构和算法/基本原理/bfs/index.html","a5c094cfc2bcea5006a57e3a6427eff7"],["/categories/数据结构和算法/基本原理/dfs/index.html","d5103914977c653051a0fd99bf5c2565"],["/categories/数据结构和算法/基本原理/index.html","c7792e28615296b390b8ab0703660997"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f8574827ca58cfdabc00039ab224d29a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e3af3d94f5da6ac028948fb7952500c9"],["/categories/数据结构和算法/基本原理/图论/index.html","e67e248107600c74187ebbc8faac259d"],["/categories/数据结构和算法/基本原理/字符串/index.html","abf8a93054be820161f1dd7c24194b51"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","88ddcc0600354e75b0830cf5ef5ade2c"],["/categories/数据结构和算法/基本原理/数论/index.html","77f2a94965c788173fbd02d1d349cd42"],["/categories/数据结构和算法/基本原理/树论/index.html","04dbb8ea35ba23a95cc6897da5485035"],["/categories/数据结构和算法/基本原理/链表/index.html","615b88c0c928ddd85c6709d2dfe456e9"],["/categories/数据结构和算法/算法题/index.html","bfa384236d579afb6c5fef3e07d4f200"],["/categories/数据结构和算法/算法题/二分查找/index.html","5e8b58b7a61a2074599c44988cf96659"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f4e96067728be40ffb1f54da2db1af0f"],["/categories/数据结构和算法/算法题/动态规划/index.html","f9ca10c0e8e5d0195ffba87ae8c36837"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3e1fc214e3197cfacb2cbea310d3d928"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","88be63a12719aac5dfe694c53c341800"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","6534cd9aaa5e4888c5f5390d2c663e72"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","bf13fdd795cda52aa067273fc8c486d5"],["/categories/数据结构和算法/算法题/数论/index.html","cf4d08000d029ec75356aedd47c37054"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c99047eaf97d7f0324122eed7ea8f698"],["/categories/数据结构和算法/算法题/树论/index.html","e51d14647d95a1be4293deadcb942027"],["/categories/杂七杂八/index.html","a4d11384f499a096020476e2f3241a05"],["/categories/杂七杂八/博客搭建/index.html","dda3b27e8237c57b9ba70f2dc43917cc"],["/categories/编程工具下载/index.html","91d5263adf4cb560de5440610391108b"],["/categories/编程环境/index.html","678921bd592930abf1109628c8919805"],["/categories/编程环境/大数据/index.html","0f4488972f21f805cb9c6cae92ce4fdb"],["/categories/英语学习/index.html","7b1a8aeea62eefe4f22027cbd6183037"],["/categories/英语学习/英语语法/index.html","305f261c60755253c37ff4b98354f73b"],["/comments/index.html","dffa8e30413262787179ebd85a10e265"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","22788204a8d3564d8cc70c80cdb1ed15"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cd148ae26d4fa6f8e8556ff6e8ded2c7"],["/movies/index.html","92e5ade93e05db1c506396dce78412c7"],["/music/index.html","c49df0291e7a3235dedd31bea3d604f3"],["/page/2/index.html","ea49aa965c6b1cf2c4e7343d65f2763e"],["/page/3/index.html","b9e1485091052ff85d6b6ed670d0deaa"],["/page/4/index.html","6495f494415fdfbb6aaa8b49e66dab98"],["/page/5/index.html","9322eb32d1671022216285637aff7045"],["/page/6/index.html","7de55ecdab2773a5b7073bdb130e617f"],["/posts/1021360842.html","ef0c3862fc6878355049c67e992d75e4"],["/posts/1120620192.html","76f08e1e48a9b05ec91a3df653d34f42"],["/posts/1141628095.html","c43e875a750bb32c23ed845763d667f2"],["/posts/1168613674.html","da936a481908a7a310201a037f95b4c0"],["/posts/1219920510.html","d71782c3233806b2c81e3142aa276514"],["/posts/1222166338.html","f234e3755729dcc7c4474e97a17c8556"],["/posts/1259097482.html","e97bb9cb42bd98a243c6b7330d7e0c52"],["/posts/1271036369.html","e9d722dd18d7aa7f0db982445c2f21f1"],["/posts/1312847445.html","f5ac5275c57ed1da9eca45ae14fcae34"],["/posts/135355774.html","6289a73347f57361437d123b8d14628b"],["/posts/1375344716.html","05e13f93303cb71eda00cac86f26e74f"],["/posts/1388991698.html","f64bdb5b44c21f86e96941b511060892"],["/posts/1410315814.html","ef0956fdf87158b71f11045fa3d64e02"],["/posts/1452790229.html","2f86f8dc5089a7367873e37400fa038a"],["/posts/1470079884.html","606eeaa99c59414d98748359991e1f08"],["/posts/1470079885.html","a1dbf3c19caa6607ec3421fd65c0e711"],["/posts/1470079886.html","21771d3bf7b11ee9dff28433eed55236"],["/posts/1470079887.html","ab8b26ac2d7dea2381d8b8b3febebf94"],["/posts/1498536549.html","05220dabad9528bbb78542c07e3fd687"],["/posts/1539568593.html","88ce6cd8297df82c774ba8243ac1f670"],["/posts/1547067935.html","b7e9838c29b0b610e09ef2d28f9d0263"],["/posts/1557866301.html","0c7026588003c13db9000fe0be6e3900"],["/posts/1571776361.html","192d7add2ebefad5fc5d193bd8fd8b34"],["/posts/1605124548.html","8965ef91d71547eb2634cface82f4e73"],["/posts/1633036852.html","d7160c1713501689e82c88e99543a497"],["/posts/1674202625.html","7375eea2976c75fde2a122dd14af8b1e"],["/posts/1765123828.html","4937c0726caaa67647e3c1ee25d638bd"],["/posts/1767336200.html","a8a0d7b2f53171efddb791c3fa5ac465"],["/posts/1776114197.html","e989e046f05dd8f910d0d7114492bb9f"],["/posts/1817748743.html","241fc8433587c9288120f820af9159dc"],["/posts/1925125395.html","be2c8c6507c6bdeccd37d629c34fe58c"],["/posts/1966191251.html","fbd71f896790fc50de7f4f08c5a05702"],["/posts/1987617322.html","07f7ff0bf24374fe78c5c4e9b84099ca"],["/posts/1999788039.html","7512c3d5820b9f7bb5a9506c0d8df671"],["/posts/2075104059.html","1ca7782861c30f555ec4096d00353be8"],["/posts/2087796737.html","0a8eecb1bbf89b5d1c353f56c1692dee"],["/posts/2106547339.html","79c5d97f4bad55216a746bfa70dce3e3"],["/posts/2207806286.html","fd10a5d4e71c062d6fbd8038ca59166f"],["/posts/2225903441.html","71dcb8b8ef110563cb8aee34a85c9486"],["/posts/2265610284.html","93f9afd5e5a3f4d67cd9897c18835ce9"],["/posts/2281352001.html","d9de4fce908bfe3cef242fb943ee89a9"],["/posts/2364755265.html","e578f81ca3551a513d0d88a5a50db5f2"],["/posts/2414116852.html","322b7d6198fa1f7e8a24035f521eb02d"],["/posts/2421785022.html","1147229b02b0c3c5602036f8820efb82"],["/posts/2482902029.html","cbe727ec523ccc111b7a65d6aebbb503"],["/posts/2495386210.html","7539c90d75f213a98b8e64a8559845f7"],["/posts/2516528882.html","7280f526a3816a098a6657c09c9ac73a"],["/posts/2526659543.html","90661c76c7b18282c754432ce3df3003"],["/posts/2529807823.html","25ddfef57cc89f3ba7c53566b84568af"],["/posts/2596601004.html","c484f920c276331122022654045ea622"],["/posts/2697614349.html","7d13c789bdde327456ce57998ef06781"],["/posts/2742438348.html","bdb72a0e5ff77f515d04012583ec9ef8"],["/posts/2768249503.html","5dc5201d8ecc57df75686637628e54f9"],["/posts/2864584994.html","71ea302487b194a7af68bb670c338975"],["/posts/2888309600.html","aaf005297a3e498bc50b2eeae244fd3a"],["/posts/2891591958.html","41ea716e9b2c8b3a1cf55c8ee401dcda"],["/posts/2909934084.html","fc054193cd5aa8a12c269690e0e11b43"],["/posts/2920256992.html","38cae9808835d00bdfbb1bc55879a45e"],["/posts/2959474469.html","d38cc897e3bcbcd4ac0e1db88e7f1c5d"],["/posts/3005926051.html","c2813b45f6ac5f80b2611ce062e52bc6"],["/posts/309775400.html","7774dba47c2c6b5e3d33f1d6999ac8d6"],["/posts/3156194925.html","992b126b02306be0e71d01aab5c7fc45"],["/posts/3169224211.html","3baf2c86a6a2c33093c674d997cf93ad"],["/posts/3213899550.html","7d665de28c3e34eb08d717fd841a6766"],["/posts/3259212833.html","5215556c8a0464d1dbc155b43578ea2c"],["/posts/3266130344.html","542d9bdd561927c993669ef44d94357b"],["/posts/3292663995.html","2479d2c6dde7e7ebc6b247d434471fd7"],["/posts/3297135020.html","6e20c1228637828f64d64e5d56761bdc"],["/posts/3306641566.html","87a788335833779e2ac76128917f188c"],["/posts/3312011324.html","32f3d147c88866961154f57e9083c281"],["/posts/336911618.html","de7a7a4a0549ce7d1014a34ffbf9265a"],["/posts/3402121571.html","941b69b1dc405e30d7920507ed3c9523"],["/posts/3405577485.html","5e50e52c761543f91a01d795706780d9"],["/posts/3498516849.html","726b4095db9aa9bed75755c2b2699f65"],["/posts/3513711414.html","65b00d890c9dec46a06ce5364489e3f4"],["/posts/3523095624.html","f040afb958596ac43dc08cdcce1a050d"],["/posts/3546711884.html","95befd28c63c19c922c2c95879be6f9d"],["/posts/3731385230.html","c66c6926b4124b49fba0343e5fde11a7"],["/posts/3772089482.html","b9577b6fa478417bdb2ebbd632e168a4"],["/posts/386609427.html","16c7c027408fdac23691e2f6aa0b6004"],["/posts/4044235327.html","8d78e13e9871eb9c8c146907d52ebf4a"],["/posts/4115971639.html","a185a4ae6af75a93083213720e474fc6"],["/posts/4130790367.html","45652fb9856e1de76dd0590c03533c4b"],["/posts/4131986683.html","8f40c174ec5c491999cbe09d7dae3574"],["/posts/4177218757.html","f92d625705d34594a50666f823d02d00"],["/posts/4192183953.html","f92f066c9905da569656bbc6894d3cc7"],["/posts/4261103898.html","8a9ec5635cb81e3c60760ec66b21593f"],["/posts/469711973.html","b819834fbfe02af752eb6b1eea7002d2"],["/posts/482495853.html","f148c3c29f6504ba4a180a6a81cdaab6"],["/posts/488247922.html","f4b6101928b63900e71720e05a25c015"],["/posts/517302816.html","a0fec15e2199d13957f97a93690bc107"],["/posts/570165348.html","17a0d643aade35e945545af26bb7799e"],["/posts/595890772.html","88047d5818378ef655b96e4ee4646b86"],["/posts/67485572.html","d835192d3d5c29e61ba447e2643b163a"],["/posts/694347442.html","236b24bf7f13b6e50f760efb2b9cccb4"],["/posts/707384687.html","6949ce1d500ddfd77b59246b4515dad4"],["/posts/71180092.html","f7cfa275aaf2afeda098f4eb439371e3"],["/posts/716459272.html","1865aecf5ebf55f9d2bea7eee3838bef"],["/posts/765481613.html","49f94ec2e610b3254a3fca7b75c2f1d1"],["/posts/778231993.html","c7182e62b4c3bd0193df619c26da8cc9"],["/posts/795397410.html","5d59804c15af9579eeacfad336186f61"],["/posts/820223701.html","5ab612ecccd5df73ecfaaad595845ccd"],["/posts/830372185.html","9d1439635ced21f53100ec65ac0be0db"],["/posts/88294277.html","dbe13bd5960512117f6fc996f2057771"],["/posts/939963535.html","bac5e755f39d5213343a336639d64e79"],["/posts/983786067.html","be60098fd23bd3075bd4f65cf2afe64a"],["/sw-register.js","03acae0049f8f346e70faf36f3ee503b"],["/tags/C/index.html","eec7695534afdbf8ea07017d9ea99b74"],["/tags/C/page/2/index.html","a0ed723a63a3a76fff5edf4b2a257040"],["/tags/C/page/3/index.html","5bc911a73e2bd3f1c693a111fbe6b330"],["/tags/C/page/4/index.html","1cc74e8e0fffe5474898a06b4d243142"],["/tags/ETL/index.html","1537d4b1da756a033bf1462a961884b7"],["/tags/ElasticSearch/index.html","66a576d4928165762c1695143d5c46a2"],["/tags/GUI/index.html","7d03df6cb25c1e6df8040bb2f456b119"],["/tags/HBase/index.html","8333920b56c4ba3bb9d51068c50175d6"],["/tags/Hadoop/index.html","ec2525c6e1b77e78f9f15803d70d6f9e"],["/tags/Hadoop/page/2/index.html","c37a11f1f99738a2fb89f9e4efbae044"],["/tags/Java/index.html","70f569f3659a9fcf684915ac45327f1a"],["/tags/Java后端/index.html","11e1ea733fb4db84a584381707fb86f5"],["/tags/Java后端/page/2/index.html","18970f3e303742dfbda98d429f3b771a"],["/tags/Java基础/index.html","cd116c2c4a99b2ff3cc6128e57ef98ed"],["/tags/Java基础/page/2/index.html","fe545fe94c30b43355d756ece61578e0"],["/tags/Kettle/index.html","14fa0a1ae4581ac7dceca24e739a444b"],["/tags/Kibana/index.html","543205ffe5de3f9d0657e3a11f6f68ae"],["/tags/Linux/index.html","58c71e32b89d394c5830c14b8a8f2fd6"],["/tags/Linux/page/2/index.html","beda1d109acd133d11e7b58d8588cbd8"],["/tags/Linux/page/3/index.html","466dcf4d050e8d66bf8ec50c86068152"],["/tags/Mac/index.html","edb4d44cfd54cfc88aa40599ef1ec5fb"],["/tags/Mac/page/2/index.html","3f2b8b68aeeb41f316bf058b97ff91bf"],["/tags/Maven/index.html","450906ad63484fda2f7e75b3d34a53f6"],["/tags/MySQL/index.html","78a2781717961459a5e0fcd12187ff95"],["/tags/Python/index.html","66a9d7ff22cf326775e9ac786e98791e"],["/tags/Redis/index.html","4decff25d6d6be99e33090d9aa59b0a5"],["/tags/R语言/index.html","c8da622a00a946db68ae4fb97198b04f"],["/tags/Spark/index.html","b2ed7184c9c7506fa720704a3b9a63f1"],["/tags/Ubuntu/index.html","8f71c226013f513e4ea0e9c004b78212"],["/tags/Vue/index.html","ee58424eaa9d31a269c7c09bbee08ac1"],["/tags/Windows/index.html","58fa24bf4af3291831d667d5bc716d5a"],["/tags/ZooKeeper/index.html","7fb01d8600be1f657a87fbc325a2c518"],["/tags/bfs/index.html","f142f05c030e9a55928b004014c2f84a"],["/tags/dfs/index.html","a49ed342e126c22c1ca755666eebfe15"],["/tags/folium/index.html","bf7dbdc1cc0cecd72720364d1ff08869"],["/tags/git/index.html","973f112a1e1550b7adde07b7c25b6b12"],["/tags/index.html","204eb35b7948806edd06cb80997b4c68"],["/tags/latex/index.html","ecea52154955256de37eb8f1a6d3cd3f"],["/tags/中间件/index.html","b822410597055b9da879bb04ebee91ac"],["/tags/二分查找/index.html","b16c4aa79351423109b467b1929ef248"],["/tags/优化类/index.html","1faae662e40b03e3b0da35a96ba42a85"],["/tags/前端/index.html","32d1f841dc42bed9fb3a84d2a4023020"],["/tags/前缀和与差分/index.html","13e473fa82c9f4c7107ff2cc96f46c24"],["/tags/动态规划/index.html","d7248c6c60d84f3630864ebeaf78ae33"],["/tags/动态规划/page/2/index.html","87e33ebbb589a1484593c152d240243d"],["/tags/博客搭建/index.html","859f007de15d9f85644e927c0efe0edf"],["/tags/图论/index.html","38a4eb4fed74c5e0e3f03772ec70e60c"],["/tags/大数据/index.html","c5806e80a0172777d8dd48b7e012c0cd"],["/tags/大数据/page/2/index.html","d24524bab655c1cb997d42d5d627e8da"],["/tags/操作系统/index.html","ffdc7b7f74369fbf9c94cc5ea216a194"],["/tags/数学建模/index.html","7b5f973998bd21557ec537144307445a"],["/tags/数据库/index.html","aedc9db63cee4a4cba1cbbcdee14ebdf"],["/tags/数据结构和算法/index.html","9d2c450b94739829918191a0918e2912"],["/tags/数据结构和算法/page/2/index.html","f31cb0eff3aed8d072e8bdeb29c3b76f"],["/tags/数据结构和算法/page/3/index.html","df86e7aea728e7479af661d022ba2202"],["/tags/数据结构和算法/page/4/index.html","355c6268d5732028bfd0c9f55c60d134"],["/tags/数组和字符串/index.html","df0844c313e8449b236ff55d9bda9b77"],["/tags/数论/index.html","14534837cd85ac8326712fe9f6c6b5b4"],["/tags/枚举类/index.html","171dbe1ed242b0a06c3455cfdff4d652"],["/tags/栈和队列/index.html","18372ab1262f51302ef9673315d5165b"],["/tags/树论/index.html","55044d27843524b860319f53c01a6c55"],["/tags/测试/index.html","1976c36fc5142f5ad0c52e5941d9f446"],["/tags/环境/index.html","5db5ef23bd1dfa91b1be6e275053f332"],["/tags/环境变量/index.html","c24c55549cbde06923286e9083678461"],["/tags/绘图/index.html","a3149ec2001ad1ca5dfd4d596f7ee02e"],["/tags/编程工具/index.html","82fbf7af2fe72c7bd450d38681053588"],["/tags/编程环境/index.html","441337fabc83053fceffda105c86313c"],["/tags/网络编程/index.html","b6e5379a5dab1d67a49f738912d76b27"],["/tags/英语语法/index.html","e9eced9c09a29cf2222c1fe20a255cb9"],["/tags/计算机操作系统/index.html","0bb9c692eb70144e61bd65586a9497ec"],["/tags/论文/index.html","6b4aba812ddd7263335d19e6311a888b"],["/tags/资源下载/index.html","565ea19d931ef586513f2e1c4bcc18f1"],["/tags/链表/index.html","d3fd57683fa96bc97e687b4fe8d2d6ce"],["/tags/集合/index.html","7bdd61a68a6870ba8e8e455e0c25f1ed"],["/tags/集群/index.html","f48093b41ff70099b5dfe6bf9b9fd259"]];
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
