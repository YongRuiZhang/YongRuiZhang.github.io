/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4c707adce6f0f9a88bc262de1ce81c36"],["/about/index.html","a0bbeae2b4425056db5c14a773c0e0b8"],["/archives/2023/01/index.html","01c9c078e111b460c9c1964799d24904"],["/archives/2023/02/index.html","babb392285fec8cee9773465aae9df48"],["/archives/2023/02/page/2/index.html","91a67b05fcc8a3461474135059dba65a"],["/archives/2023/03/index.html","cf623d9da1ff997f65cab26380aa3b71"],["/archives/2023/05/index.html","4ed9714ec0f64634d4f090091247dcae"],["/archives/2023/06/index.html","9100b7fbcd24639ae7793ae8d10e4307"],["/archives/2023/09/index.html","b4179392f6d67e36ddf55830042e1afb"],["/archives/2023/11/index.html","88ac9564836bb66641ddc9e0615746b2"],["/archives/2023/12/index.html","dc14c7a0d138fd3731946e63197109e3"],["/archives/2023/index.html","e9d5bb7416de02aacd81781e6c1e67ca"],["/archives/2023/page/2/index.html","f57cc63c10190d31e8b326941b8ffcc5"],["/archives/2023/page/3/index.html","d60df0370ff2a20cb518ae13d01cfead"],["/archives/2023/page/4/index.html","e6f0144aebade9e4a09e874ce07b1b27"],["/archives/2024/02/index.html","bf8b91df4e1886689e53f78e25b9f27b"],["/archives/2024/index.html","ed54b3e7f58ceca532d39b071aea5feb"],["/archives/index.html","eab5f2a2f9d567446e8ae0e6a646c947"],["/archives/page/2/index.html","46bc0ee77ae0af683c182ec6ed631997"],["/archives/page/3/index.html","8a3abe7a14e8abc269214f5b0c71ac28"],["/archives/page/4/index.html","29e527179c10b0aa145ca4a085fd057a"],["/baidu_verify_codeva-qQP2iZOMLX.html","569abefb271451cf9b125fa602263120"],["/categories/Java/index.html","c33f1c23473c78afa9a80fd5de547408"],["/categories/Java/后端/index.html","5e7f99b0c9a42f09245a7b7e265fdcdc"],["/categories/Java/基础/index.html","79fa634e056a858602ce1a807690c6e9"],["/categories/Java/基础/集合/index.html","bf199bb927f2d573b952293be71876bc"],["/categories/Python/index.html","bc051c143ebc0694f8591415b3254fbe"],["/categories/Python/编程环境/index.html","0d64990bed9161b882b652f67d4586b5"],["/categories/R语言/index.html","f8789b47175c8dc0cc253295de411a96"],["/categories/R语言/编程环境/index.html","0926f5dcce61d71ff5f51a303df902c0"],["/categories/index.html","5f2f7003d4c2f4dc7847d5e4b3ba8368"],["/categories/中间件/index.html","56c87d9022d00187acf40f031a944589"],["/categories/前端/Vue/index.html","8479c63ac0128eb76d216297f15dfd3e"],["/categories/前端/index.html","87acd9addd66e39191b63cec61b5a4ae"],["/categories/大数据开发/ElasticSearch/index.html","381b724291efb14eb38bb03170ebc9c1"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","df233d67bb74bb153f0862d76806ceca"],["/categories/大数据开发/HBase/index.html","2b1646e0d4c233c11209069fca826c6f"],["/categories/大数据开发/HBase/学习笔记/index.html","eacbbb82a5776d42debf12116b78fffc"],["/categories/大数据开发/HBase/环境搭建/index.html","18c17d950f139b54dab7f11115a9d528"],["/categories/大数据开发/Hadoop/index.html","c7be536caf4af8b4a02daf1f24b9a5d5"],["/categories/大数据开发/Hadoop/技术/index.html","252346c4a657fa7125f7e95e455810f7"],["/categories/大数据开发/Hadoop/环境搭建/index.html","21ff104b5ff1c6fbf594e72ef27bbed3"],["/categories/大数据开发/Redis/index.html","05a5b0f8f89214a1c48200e7a986756b"],["/categories/大数据开发/Redis/技术/index.html","efb40e763e0da11a36b4fc8e5f1c257c"],["/categories/大数据开发/Redis/环境搭建/index.html","4637868f821e37fd6d39a59fe2fa68ab"],["/categories/大数据开发/Spark/index.html","9ce5118ac74852e60ea02a7a909a38aa"],["/categories/大数据开发/Spark/环境搭建/index.html","3c375ff57c78c4fd5e513033571b2e80"],["/categories/大数据开发/Zookeeper/index.html","650853945eccaed09cd90c17cfe0d34a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","fa6db0dcf03e415f2a35c8e251f550e0"],["/categories/大数据开发/index.html","1c87aea15aac0cff8924a65206c0862c"],["/categories/学校课程/index.html","a560e49a33393efadcd1c7e893bbac96"],["/categories/学校课程/计算机操作系统/index.html","4d2f94701f8b5f292faa67ad39792c4a"],["/categories/操作系统/Linux/index.html","3e63b882d4fc6a3cca7c56591c32491b"],["/categories/操作系统/Mac/index.html","e73502722a0e4d83bf8303d98c18d667"],["/categories/操作系统/Windows/index.html","3ff4d65e2f8d0c6442ed7f83994caffd"],["/categories/操作系统/index.html","cd6c37806aba55508fac46e6948027b7"],["/categories/数学建模/index.html","14c2ea33e268f35e33bceaa542bc85a7"],["/categories/数学建模/latex/index.html","f82fc47d27247d66bbc57697a9f94409"],["/categories/数学建模/优化类/index.html","83cc0a7ca91380ae9d8ac2c1b8678d1d"],["/categories/数学建模/优化类/现代优化算法/index.html","079fefbfe03bcf2a1751023a63663d8f"],["/categories/数学建模/优化类/规划类/index.html","c01dc1b1cf1a56359c16d1e70ddb2ec7"],["/categories/数学建模/绘图/index.html","b230491d1030bf7e2f356b5fac91670d"],["/categories/数据库/MySQL/index.html","d4a4893c627fadb464b653849657444e"],["/categories/数据库/index.html","b7dd2485f058937cdb72765f191ff1a9"],["/categories/数据结构和算法/index.html","0ba3f2cfc1892fdaf3493dfe6e52958a"],["/categories/数据结构和算法/page/2/index.html","1951c083056c72a6e3b765cc97a4e133"],["/categories/数据结构和算法/基本原理/bfs/index.html","cd9dd0673431f0f433ac0e5571bb6a58"],["/categories/数据结构和算法/基本原理/dfs/index.html","c206145c84f157d348a72bc6647a091d"],["/categories/数据结构和算法/基本原理/index.html","02c16082055755efd27ed9c3c0119180"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f8a0d5d856921baeab27bada7daf617c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","8781f309670ca8e7350748849ae98939"],["/categories/数据结构和算法/基本原理/图论/index.html","4ef1ad7993113cb9dcfae0b50a2d4e1b"],["/categories/数据结构和算法/基本原理/字符串/index.html","3d266b11d97f72ed463fec0791b042f8"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fd811c446b4bb32e30b7d8fc816fa059"],["/categories/数据结构和算法/基本原理/数论/index.html","97c4e80f753d1be2c6b5c482223c2fac"],["/categories/数据结构和算法/基本原理/树论/index.html","b1fcfd8e39c5d99b1833ac4fdc4c9c32"],["/categories/数据结构和算法/基本原理/链表/index.html","093d8e36ffe4f3e74d292882f0644ad3"],["/categories/数据结构和算法/算法题/index.html","be7064df0ab34ee5e5024aa61f633956"],["/categories/数据结构和算法/算法题/二分查找/index.html","003a2a57b96af1039fe3b5bc4bcbcdee"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","fac22a0deac452d3cee9fffb5a6db6aa"],["/categories/数据结构和算法/算法题/动态规划/index.html","bf2fd7b02f1305df246a1ded5aabda48"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","55730138cb6e78f1144e88b957a5a1a9"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","1b35c2eede3d29c554586a3941f7f5b2"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e30bc5a34790ba0ae8772683441c5d9e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c3dcb8c1df66cb38819379708b3685cb"],["/categories/数据结构和算法/算法题/数论/index.html","bcb158a3316004aee9faf052e1988910"],["/categories/数据结构和算法/算法题/栈和队列/index.html","cc46ff852b18a523615bf2029a817169"],["/categories/数据结构和算法/算法题/树论/index.html","b1c7b2ac11f1783bb491995afdb8f69a"],["/categories/杂七杂八/index.html","4540fa82bbe962fdf69b1161af5c92c1"],["/categories/杂七杂八/博客搭建/index.html","c388ee1b2954b38496e8bde856b50fc1"],["/categories/编程工具下载/index.html","f65f6b6b6acdcf78f5e6cd08408bd008"],["/categories/编程环境/index.html","992e3b747a99fb0a5f07c733c5496789"],["/categories/编程环境/大数据/index.html","dd0bb95a6766be52498756ce78cb9785"],["/categories/英语学习/index.html","e9737d1275c9d08970e93d9cc684519e"],["/categories/英语学习/英语语法/index.html","b914291c403d96c1a4e0def526c7f3c0"],["/comments/index.html","1ad2dee7cebfe51c609073396db7b11f"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c97ee740ba8428f473bd2de04df9e76d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","70b561a2807220adbb814160ae6086bd"],["/movies/index.html","89cf76639092c6c915f61dd3aa5cb156"],["/music/index.html","a8698775fc20f0b39598337a239d2288"],["/page/2/index.html","9ad65b3b12a8ae5f408eb8c3658a9ad3"],["/page/3/index.html","f36ad78e6b6be16fe3a21b792bbd9fc8"],["/page/4/index.html","28144c721839f2a9915d5e7ea4deb276"],["/page/5/index.html","a7f5b7c3e2c52b18cb7404cb042e0bfb"],["/page/6/index.html","a15ac36d0ea843031a43224206d32c8a"],["/posts/1021360842.html","6174c39a56165e96827b405e7bcfa7bb"],["/posts/1120620192.html","860e8d8fc1f9e683edfa77417d4d89df"],["/posts/1141628095.html","19917df82018d2492ff677059f9e68fc"],["/posts/1168613674.html","cf3e70861886871aabfc6bed9d19ec9e"],["/posts/1219920510.html","e15ef558151c6e5198f2621e1fcc49f4"],["/posts/1222166338.html","b29fc14a356c66b188eb0e544a47b875"],["/posts/1259097482.html","4478744cfa26599cc44c16d6a5766b1c"],["/posts/1271036369.html","b78909386ec00f5be5767ac40f14c040"],["/posts/1312847445.html","c42a385e714e66a6829af2f17c9e5a2c"],["/posts/135355774.html","1452062b22edcd37855e0f075c92381f"],["/posts/1375344716.html","c2dc1a3d8d75c4eb98c27b0f51e7d15e"],["/posts/1388991698.html","91202f9b3b331a546449fd7ead2831ba"],["/posts/1410315814.html","26b592fb3ef73cc3823d308415fc64ef"],["/posts/1452790229.html","f27e9dc3a8a2ea114af8e3d1e647ca5f"],["/posts/1470079884.html","483357de49e4c05acf044f576797effa"],["/posts/1470079885.html","07b01af0d31563bef9228775c6592d3c"],["/posts/1470079886.html","384db04833d93eb63a922753fad654d7"],["/posts/1470079887.html","ee184949fa505d915005d2651ecc7075"],["/posts/1498536549.html","807195632768eafd261c3504fcab5966"],["/posts/1539568593.html","b9edf6a9be5a51a4fe8245cfd1691913"],["/posts/1547067935.html","5f910d4dd6dd4f77f891d75e0d246a2b"],["/posts/1557866301.html","19f26764445eadf4f22fc62ac340a4aa"],["/posts/1571776361.html","77688cf38b7366a2c1a6e77c3d132e4a"],["/posts/1605124548.html","9881fed7ca06cb4101c2a8d97c0ca4a3"],["/posts/1633036852.html","f8fdf52c44c1f6efd040d27d43de2c07"],["/posts/1674202625.html","58cf9c452fba3e4ce93b1178605c6966"],["/posts/1765123828.html","b493c854ab59e63a73fbcd5a0fcd337f"],["/posts/1767336200.html","98d661b896c50e9ae47fcaaad242dee2"],["/posts/1776114197.html","24c63cb14888b04693cbb84d5604601d"],["/posts/1817748743.html","4699611f146227da434a0d52ec13aa11"],["/posts/1925125395.html","c0085dfea6c771e00e07042e737cfe47"],["/posts/1966191251.html","288aebfd9f21bcd0b40ac9e24716bff5"],["/posts/1987617322.html","14cba8da49c0cc4adec10dee4ac4251d"],["/posts/1999788039.html","d1c62376c2468a1354f23671228f31fb"],["/posts/2075104059.html","acbee70db3fe104660134682d2f420a9"],["/posts/2087796737.html","b73d8e9b6c8bc70325bfd4212f75946f"],["/posts/2106547339.html","a61c6737e40d7919b3b1f1d91904b294"],["/posts/2207806286.html","9e9232f6dc11dc4e308dfc355f35cdbf"],["/posts/2225903441.html","f57ea8ef852a931f63d2aab3136e869d"],["/posts/2265610284.html","34719278072276f2a087b7f579567f23"],["/posts/2281352001.html","d14d96d7a95d2ba55827aa0749c82798"],["/posts/2364755265.html","3e7ccfe3b1d73f8f75986731ae118d6c"],["/posts/2414116852.html","090e460194e7a200752e06d6f6109d73"],["/posts/2421785022.html","02100a76f6a8ca1e34d41f85632b0bb0"],["/posts/2482902029.html","38cb8b4f14ed85603b2c38b0e5cca18c"],["/posts/2495386210.html","ac4d75cd588eb1b51f352b5d7766cdc6"],["/posts/2516528882.html","d56efada1d06983aa73847369a4668b1"],["/posts/2526659543.html","635a82ee7c1cd4b508a44ea1061844a6"],["/posts/2529807823.html","a174c5e98f6b551d845052f727ed5e22"],["/posts/2596601004.html","4dd46e35ab63aebfc6383b5f30f76d90"],["/posts/2697614349.html","0a00760f8caca45ece02f1f2f3d7c1bf"],["/posts/2742438348.html","d9829c330b8412deb66fa6235ab82ae5"],["/posts/2768249503.html","f6969895db2c11d906e84c1cf579af51"],["/posts/2864584994.html","b91983c101c67fab45225076f6651afe"],["/posts/2888309600.html","619f88ad975ad26e56f9edb230592976"],["/posts/2891591958.html","9efc65c3885b4eb5f388d0aa937b1eda"],["/posts/2909934084.html","b538aa2f2171a66da83c1847f79c246e"],["/posts/2920256992.html","7ed5f1e35838850b6fb97646f92e395a"],["/posts/2959474469.html","175e46b841f12df0b6b43ea94b351b1d"],["/posts/3005926051.html","45428514979ebd6388bb73e6f5ab96c2"],["/posts/309775400.html","d61ba153ac43596726d79a9caaf3766b"],["/posts/3156194925.html","0211b7d0e9c9077bfe0edcfaa12a90db"],["/posts/3169224211.html","61b6ada4a5c770ade27f1e2f94e56cc6"],["/posts/3213899550.html","1cbb28061263479cd4c89fd5bc00fdc5"],["/posts/3259212833.html","54ecd2fe47c7047337fa679590c586d8"],["/posts/3266130344.html","898955d363a703b41ba3e1862cac36d4"],["/posts/3292663995.html","738c015db71faf5d6548b8c46471f6c7"],["/posts/3297135020.html","afad0703686e5beb978ce0049b344fbc"],["/posts/3306641566.html","893d8ed2a48a2417b9b2843b3317afcb"],["/posts/3312011324.html","ea8e34f6914c1aff79cb8bddb9bc820b"],["/posts/336911618.html","aba3ae85d9e9f7350ec5cd1378da7a2a"],["/posts/3402121571.html","b968d4d612d47473c483d3af95d44506"],["/posts/3405577485.html","68d7a790a275c2f74cb13d2e85fb2b36"],["/posts/3498516849.html","a46c68cd7eb2d8e6f1259237bc0840bb"],["/posts/3513711414.html","12d0647166dabe4be5a844675940c599"],["/posts/3523095624.html","6658f8a2f9b6fbbf9d3dda002abdb24a"],["/posts/3546711884.html","506b4a818696624f129bb9e0c04aa88e"],["/posts/3731385230.html","8012f0150ec3ae0e218b3cf9fa3f7468"],["/posts/3772089482.html","e305fdfec471e7d4f93962afda24b485"],["/posts/386609427.html","9bb0e6a15dd796bd998445c9204d5f0f"],["/posts/4044235327.html","6646baf36c6b5b1c5d4bb81c872a605c"],["/posts/4115971639.html","13a5b46186a2c53d50fb92e4579f8a18"],["/posts/4130790367.html","94482b478ff1d38bfe84270ea13126ef"],["/posts/4131986683.html","5b1e4a48cdbe0950f67d7ef3daad6bda"],["/posts/4177218757.html","e3a388d98525d05b028cf31c51e30adf"],["/posts/4192183953.html","9df10770ef515380d7ce37e6d6e47c01"],["/posts/4261103898.html","227a57c7d9e55843e07324019696dbad"],["/posts/469711973.html","2186a59b2293ab0eddceebc69e72a98a"],["/posts/482495853.html","d54771d24790ce256cefe95346fba0d3"],["/posts/488247922.html","8fd2a9d1458df521e93461db5ddce4ce"],["/posts/517302816.html","4f48cb8d3cba71f8db44bb1afda5ddde"],["/posts/570165348.html","53d14257efa7066a597ead006c5a7c9b"],["/posts/595890772.html","65663b5a0ccc121e21c65483fa09610d"],["/posts/67485572.html","60f95f7f15b7abef19c0044b31ba2321"],["/posts/694347442.html","f089fbfcfc0a40d69ef334407e17749f"],["/posts/707384687.html","614bf3e5c27924aaad4e03345f894830"],["/posts/71180092.html","bddcae8f3b8cb83d6be59484bac5d984"],["/posts/716459272.html","a3dc5c357032703ddb3ab721a403f7ea"],["/posts/765481613.html","0a617028f2ebb9893df6b0f699da24a7"],["/posts/778231993.html","ee12752fc0e2e1f9315f60580d260f49"],["/posts/795397410.html","beca5551bcbe4151602086de40de7625"],["/posts/820223701.html","481ef156e72baa3b4be52551f72a66a4"],["/posts/830372185.html","a3a6bca24670c4178dc71d2a81469bf1"],["/posts/88294277.html","fcca04962671a960575a679d9aba6d23"],["/posts/939963535.html","4e39b7a48ca06e61027ffb1447c76256"],["/posts/983786067.html","ac5157d5b4c87bf6dcf4226700722854"],["/sw-register.js","2b31fe745d902bfe1c05c91f79f65273"],["/tags/C/index.html","24898d5d1855e8016c75f2c7f8fec2e0"],["/tags/C/page/2/index.html","210dc71d8fc3f299d1f693a1e0c90f14"],["/tags/C/page/3/index.html","5897384cc78b8ae06b847260f7a53ff5"],["/tags/C/page/4/index.html","eaeac5af7fd9143a10978c3f9a5912e8"],["/tags/ETL/index.html","f5999cac8a54f1139b45d3218193df9c"],["/tags/ElasticSearch/index.html","da903578ce634c05ed89de6514c9cca4"],["/tags/GUI/index.html","76c846abbe3c8d503ba1d660ecfe226e"],["/tags/HBase/index.html","773cd00a325db3876d8c90cab928f3ee"],["/tags/Hadoop/index.html","0f74612427ce6fdd133555f7621c8789"],["/tags/Hadoop/page/2/index.html","9aff809f61bdc082d1095fa8e5cbc5c0"],["/tags/Java/index.html","27e11200b858d22cc96aa98830f7fcf2"],["/tags/Java后端/index.html","51119573fc9f789caa771d5ca69f25b2"],["/tags/Java后端/page/2/index.html","8980ae1ba9eee5648e77dc915ada737a"],["/tags/Java基础/index.html","770637428fb57b70cea96c39eb58cc25"],["/tags/Java基础/page/2/index.html","2b9efe882934a18d84f64a09d35e6cc0"],["/tags/Kettle/index.html","6e0a8398efea39d7b2320b9ee2a6dc34"],["/tags/Kibana/index.html","c586d244f9cf27379afa95bba1c0d70e"],["/tags/Linux/index.html","83e951e9a8655dd68004027298733c10"],["/tags/Linux/page/2/index.html","bf9b01a4754cd6653b1ddb16866c8bca"],["/tags/Linux/page/3/index.html","6f0af29374faf393911013b828916b10"],["/tags/Mac/index.html","9881c33a69af2e18131a3fd39346ca04"],["/tags/Mac/page/2/index.html","f650e5cbfa9a1ca435ffc87fec94d6f9"],["/tags/Maven/index.html","b0ef7b04c130180cc1610dd78af54ec0"],["/tags/MySQL/index.html","eac0b100d7c411561db16254485921f8"],["/tags/Python/index.html","7f3dcc82b8edd86c74e1180e2dd7f087"],["/tags/Redis/index.html","b9caf9c2211330d70d3ca7ffcb60b446"],["/tags/R语言/index.html","94b290a7872c7f22f18276362a5876b0"],["/tags/Spark/index.html","d99f0ac96918b98f29cfa606310d1944"],["/tags/Ubuntu/index.html","2edcb2c9157d96bbad618748f3da7b24"],["/tags/Vue/index.html","bcf3b9c28f334a898e90e83ae578c197"],["/tags/Windows/index.html","6c982989ad4df0a271efd31fdfbc9bdb"],["/tags/ZooKeeper/index.html","c12495d05e78a9f46360622a53dd8144"],["/tags/bfs/index.html","5181dd06fc82f63daead6f612610160b"],["/tags/dfs/index.html","87834d23d33fbd6bb622939ef3c75fae"],["/tags/folium/index.html","8c246f9af0d95992c5fb401249fd682d"],["/tags/git/index.html","82b3afe6bf57b081db8bb954303c4f01"],["/tags/index.html","d6d9d6188ef3240bfce09c52b8ecddb6"],["/tags/latex/index.html","ab755c50245126aaa6c61a638550624f"],["/tags/中间件/index.html","270d423426a851749da69e64fc34a23f"],["/tags/二分查找/index.html","e4e92cd76aa65fca069d3887779354b6"],["/tags/优化类/index.html","23db79a3f73fe551d0540dfbefe949a4"],["/tags/前端/index.html","337616dbe697b07c206c9ff81b7aa223"],["/tags/前缀和与差分/index.html","bd44ea8e1cbcef90f3ee502b11447e8c"],["/tags/动态规划/index.html","addfee9946d142695836a0a42549e1c3"],["/tags/动态规划/page/2/index.html","52eed75819ac071d332569cc9e2f15bc"],["/tags/博客搭建/index.html","597305d9c4bccce7378bdd980cdb3229"],["/tags/图论/index.html","c51cc08bffa605d286523c9695a3b1dd"],["/tags/大数据/index.html","ed7b6e4ffdff08f88ef9c90239731618"],["/tags/大数据/page/2/index.html","1397cb9e47835be1ebaef3bf1f66479f"],["/tags/操作系统/index.html","8ab3dece267e28c6d8e26ed1241ff97e"],["/tags/数学建模/index.html","33c2d5dd7a48c44d39084a742f8aaf9e"],["/tags/数据库/index.html","6adaef1510c5b80e9cf77cb29e57be6e"],["/tags/数据结构和算法/index.html","ccace461fdb3698085e06354151019fc"],["/tags/数据结构和算法/page/2/index.html","484d9d9c0ca73ecd1e948f066490b504"],["/tags/数据结构和算法/page/3/index.html","06613bc872faaffaccdb47be617423f1"],["/tags/数据结构和算法/page/4/index.html","bde3b7a5df23807a1b7bf6acb5ded9cd"],["/tags/数组和字符串/index.html","3b5486308c766006a615778c5ee0f72b"],["/tags/数论/index.html","a82bb0e4b6d519c12a119cdfcbaef35e"],["/tags/枚举类/index.html","4fb4b7ef41cd1c100615ac752af62a0f"],["/tags/栈和队列/index.html","5baaaba7ed2ae2b3d05fe7c7d5a8b773"],["/tags/树论/index.html","7ae762095180c159c09c4e7635f59c19"],["/tags/测试/index.html","ca61f65a1b6396f81e4d00d92e769ed5"],["/tags/环境/index.html","4c1b6ea365a87cb80dc1a2feec690638"],["/tags/环境变量/index.html","08086d6547256e5cb91461fd12861317"],["/tags/绘图/index.html","d1441ff215095a658fad3d3d04352ca3"],["/tags/编程工具/index.html","7a3376aa0cc217cc8e976a5dd95d0c7e"],["/tags/编程环境/index.html","10646842e6354d91651cd36375c718ef"],["/tags/网络编程/index.html","d7767f8ebe311120b58099876ed9c281"],["/tags/英语语法/index.html","8b5e44532208b49397f3cdefff13550a"],["/tags/计算机操作系统/index.html","9cfbbeb7551a22bb36f2733abbfba01d"],["/tags/论文/index.html","f1f167bd0351d4995322d6b88cbc8870"],["/tags/资源下载/index.html","a6579f081685a58e5ce624b21f68dc08"],["/tags/链表/index.html","f7a3c250d1c554aa3de27436dee815c8"],["/tags/集合/index.html","b519daf7113687a67e26818152421705"],["/tags/集群/index.html","25c1daf005c6df5f4b18d1a870c4129e"]];
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
