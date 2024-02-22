/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e43a85ae293b0cfc60b4b84f498d5cec"],["/about/index.html","d5ca8a4bd35ec2f43cc9a6db95f3c0d8"],["/archives/2023/01/index.html","ed6725a45d55e7058b9869e6bada9b14"],["/archives/2023/02/index.html","c6c9c70730386d97551ede75913dd7f5"],["/archives/2023/02/page/2/index.html","578a5d8639337f59f438663d6393e924"],["/archives/2023/03/index.html","36115181793f7f6bf52051341478e150"],["/archives/2023/05/index.html","72af913057ff22187eda628b8f0c9076"],["/archives/2023/06/index.html","a1f46cd8345dded777ca95f60ea3534a"],["/archives/2023/09/index.html","6cb2876fae845842a7428ccd7e4dd12d"],["/archives/2023/11/index.html","6fbe7ce83eb7c186407faecf3ff74a07"],["/archives/2023/12/index.html","42abeb614818bcad5448f8d046ded08e"],["/archives/2023/index.html","69f4bf5d9c486d0bd739413f81a38ed7"],["/archives/2023/page/2/index.html","880885065dca1c4e9096686d5bbe82d2"],["/archives/2023/page/3/index.html","a55b2d52e33375c153b583bd22760d50"],["/archives/2023/page/4/index.html","09cc4a73c0f6a64abf4e114decc5c61f"],["/archives/2024/02/index.html","3c321aa4ff61fe1dfa31a464c429891b"],["/archives/2024/index.html","44f3459c61c03faea55ac3f9a00661cf"],["/archives/index.html","8219a3c64e9e82ec82096c543828e61b"],["/archives/page/2/index.html","4daabf7d1a1bd0d7ccfe284cd784da7e"],["/archives/page/3/index.html","8236eeb6992f449766a38a58c7470d15"],["/archives/page/4/index.html","9c392def0f227e8cf4c3cd03696e7e2f"],["/baidu_verify_codeva-qQP2iZOMLX.html","9facad8029c5c9703cb6139e9939a492"],["/categories/Java/index.html","5193edde977b5ec380349dd21a06d0b0"],["/categories/Java/后端/index.html","8f201cea991fd830b063dedbfc5da33e"],["/categories/Java/基础/index.html","a5b868ed6a7011db8cc8023c0ab0c08d"],["/categories/Java/基础/集合/index.html","96f2304b66c460e9731e5f1cb375cf16"],["/categories/Python/index.html","5e4ae2719999035afc8df62e9525b443"],["/categories/Python/编程环境/index.html","4cbe8a73072453de32bde15921616627"],["/categories/R语言/index.html","6d37b9686f0f78cb19e8e41296587cb6"],["/categories/R语言/编程环境/index.html","1009c84f656b4a0425ecc9547763ce15"],["/categories/index.html","03f544261009eeb5ec251583840f9e34"],["/categories/中间件/index.html","d18192ccf7aea2dcf477caffc8cbfcf3"],["/categories/前端/Vue/index.html","22020b1f6bba11ead8452599bd8a2e5e"],["/categories/前端/index.html","472276ece10d07a94b3f573eee8a1c4b"],["/categories/大数据开发/ElasticSearch/index.html","81a96ff0621733b29c35fa8446806d59"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","33c73927303c4d126cb1f2a96d81f477"],["/categories/大数据开发/HBase/index.html","2df31c27a6a5922e5c57c06e31929d60"],["/categories/大数据开发/HBase/学习笔记/index.html","d0915b9c92711e9e76c845b09f504c13"],["/categories/大数据开发/HBase/环境搭建/index.html","7328d5a1b048cf586723ffac0480fdb7"],["/categories/大数据开发/Hadoop/index.html","269ac409415fa4bda7b7dd4a9e139f15"],["/categories/大数据开发/Hadoop/技术/index.html","8d14a47e86dc18f2e541d4b3da16082f"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d3f5dfc6153f7ba8db247bb0c0fcb308"],["/categories/大数据开发/Redis/index.html","c1e2d996af16fd6963151c9c0175a6e6"],["/categories/大数据开发/Redis/技术/index.html","1318059a1026fb360e44196e7558410c"],["/categories/大数据开发/Redis/环境搭建/index.html","0ba28af12815ca6a81bdadede00028f1"],["/categories/大数据开发/Spark/index.html","c73835042f11e1ea6d9aefb20202b5e6"],["/categories/大数据开发/Spark/环境搭建/index.html","d6fdca3443f5e45f46829be66f6995eb"],["/categories/大数据开发/Zookeeper/index.html","96ef05f417c14cb3faf01ab9e105d206"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","572bfdaf5197e0a15954e3e1f0080a89"],["/categories/大数据开发/index.html","3e6c4416c1f5978979d8a4d44f05e7f7"],["/categories/学校课程/index.html","b2b121c35fbe3700db6b893763cf5af8"],["/categories/学校课程/计算机操作系统/index.html","dcaa7868f56b18675abd6943693c4aa3"],["/categories/操作系统/Linux/index.html","171e9aecd6c3a8dd6046a5fe9e6b5670"],["/categories/操作系统/Mac/index.html","08fa101a8980d35f0c2a30f171afdda8"],["/categories/操作系统/Windows/index.html","969bf18db929af6bf6b33a970070eb85"],["/categories/操作系统/index.html","e149f88c6f18f0c0901d0f2cd7e443d5"],["/categories/数学建模/index.html","c06c6cd99e38ff8eaac948e2dfe31f07"],["/categories/数学建模/latex/index.html","f17f9a19dbb2a1c378e5ff52b390b4a7"],["/categories/数学建模/优化类/index.html","6ee21ae91eab66d0d6b5fd8c8ab356c1"],["/categories/数学建模/优化类/现代优化算法/index.html","97cfc6d9c3b915947f21179bc13d67db"],["/categories/数学建模/优化类/规划类/index.html","f8043d9156beee307c9532880a9354e4"],["/categories/数学建模/绘图/index.html","cf487406edf9b25ae8013da59ff09e95"],["/categories/数据库/MySQL/index.html","f8e655217d5f6422a40ca7912b25cb6f"],["/categories/数据库/index.html","a68ed80e01eaf88bb2a9b2365b60298b"],["/categories/数据结构和算法/index.html","5ec98f73ff2bd8c8a6ca87050f5bb810"],["/categories/数据结构和算法/page/2/index.html","ef381f132a004b462d27313ad7a8c4ed"],["/categories/数据结构和算法/基本原理/bfs/index.html","3785fe44a5e2e34d95c45ec583ab3ff4"],["/categories/数据结构和算法/基本原理/dfs/index.html","11dfed7fec97aa837a91633eb0f76c45"],["/categories/数据结构和算法/基本原理/index.html","28c0302efa425c01a52e8c63c7b88d5a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","09d469bd170ec3bfa18c730b0812d11f"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a41cd10df871c46ffbc7fba218b0010c"],["/categories/数据结构和算法/基本原理/图论/index.html","7a6b7353aa7465795f3c79d95128fc97"],["/categories/数据结构和算法/基本原理/字符串/index.html","d626ec4bf158f30211d4580c85875752"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","00cea4666fb0796513bfb68713df5b23"],["/categories/数据结构和算法/基本原理/数论/index.html","33c2bc518c7e3e5eef663550d4c34359"],["/categories/数据结构和算法/基本原理/树论/index.html","14c6129b8d258f1d5e8d882f23e6474a"],["/categories/数据结构和算法/基本原理/链表/index.html","5b39180d1791277955177ab0b1a097cb"],["/categories/数据结构和算法/算法题/index.html","dd8d27cef11383291e91532b2918ad76"],["/categories/数据结构和算法/算法题/二分查找/index.html","b72ab9d573be4387206d3a65d8545d4e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1fef641b269e92b2e645a981152324e1"],["/categories/数据结构和算法/算法题/动态规划/index.html","43084ac74a4babaebc542178941432be"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e2c5c447f907f3724540eb3db756354c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","021d9976a4914314a6282588f60076c2"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","3b6d3de4bd5e2cb5d99ce39264529165"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","87fe6673944301d6db5baa724d615357"],["/categories/数据结构和算法/算法题/数论/index.html","d5343c94768e4da8acb9a5218c81f13b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","062d0e1e2fb97d545385d2054d798be6"],["/categories/数据结构和算法/算法题/树论/index.html","fb04a6ab5a4c0d9cdd5766419fb60c73"],["/categories/杂七杂八/index.html","f10391f4082f9f6fb176de15d4e51e12"],["/categories/杂七杂八/博客搭建/index.html","003bf27c9cc6957532a6ad937c3cfd2b"],["/categories/编程工具下载/index.html","1f57c78a1a0659a9b771a4613c58ceb4"],["/categories/编程环境/index.html","8f661975103863d183276b966b9fa970"],["/categories/编程环境/大数据/index.html","9850bbbd83ee8abdc4479e672a58f2a2"],["/categories/英语学习/index.html","0784ed5cde0eb724e0438caa88ae0aab"],["/categories/英语学习/英语语法/index.html","5047d2b86bce5a96d226700b7da551ca"],["/comments/index.html","d201ec9e9cdbfe5fd1e621070fd29f34"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","20dc676a74ec597021daf4981a3f0a3a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","78d50f5a55dda62ae74f6e918bfa558a"],["/movies/index.html","fed52bde7d3814fdb5112096a945a247"],["/music/index.html","2a7026485f49be98c374dda30540eb8d"],["/page/2/index.html","81d6d25b3354dac96868fb6e3ff1a95f"],["/page/3/index.html","17ab3f8b558ad894b3915edaca773c34"],["/page/4/index.html","2e54412cd343d5585bfe07ab407165a3"],["/page/5/index.html","778bb13e14a2b3cf5c7433f38d638eae"],["/page/6/index.html","eb0e056a93aa99145d6a6f4f408aba80"],["/posts/1021360842.html","2f4ce8f69ad9da30274986815b4fc37a"],["/posts/1120620192.html","a98e6a0bf9d5b31f6d235b1f061c8678"],["/posts/1141628095.html","8d77c4decfd2fc896a50caad712827fb"],["/posts/1168613674.html","9da5ae74f9a2246aa680374ebb57ea31"],["/posts/1219920510.html","c045f5533a6f621a4634d4fb287f556d"],["/posts/1222166338.html","9dd52288a15613045584347f225f317d"],["/posts/1259097482.html","d717a6eb80b21a112c86f745e3ef13e6"],["/posts/1271036369.html","e9b9d4f5213dae311d414e40eb012fc7"],["/posts/1312847445.html","808425016991696314e41c9cde4bd808"],["/posts/135355774.html","1346e207e7e1734b619952cb2c0e78cb"],["/posts/1375344716.html","bf467ba1b23ad632811f18f78b72f3b3"],["/posts/1388991698.html","9aeb5e599f91ff0c5637ed1dd47fed5c"],["/posts/1410315814.html","cc1db642e0a2f770818dcdbc7c59df28"],["/posts/1452790229.html","f2ab0f724aacc5350d0834de0cf5a17f"],["/posts/1470079884.html","0616b410112234fb2fde066c42f08048"],["/posts/1470079885.html","f75347fe18f9cfc6fd47cdc8ada2c05b"],["/posts/1470079886.html","e6c035beecdfa0cb671a58833471f7e9"],["/posts/1470079887.html","ed70d31d6f8b5d95ee7f281dea1cbed3"],["/posts/1498536549.html","5f1f69726c8ec3a8eee09b35f3155324"],["/posts/1539568593.html","0af8fa7ca5d98b016c8ecc2060083c54"],["/posts/1547067935.html","342804bf0211fe3fc3c8da908547c84c"],["/posts/1557866301.html","e70b1ed2f9642a9040538e0f3bcc0b13"],["/posts/1571776361.html","5746483cf70fbca670043733e9ec9b70"],["/posts/1605124548.html","113ee2243bdb5f37a67115c24cb50d7c"],["/posts/1633036852.html","ed446d6dd01201146c004079fdaf1c09"],["/posts/1674202625.html","884c9fb4ebb4e1f521480c6996089278"],["/posts/1765123828.html","444dc05bc6b3a8ee4453684bbcce258b"],["/posts/1767336200.html","856f5dee3da0292c79ac978fc65767b9"],["/posts/1776114197.html","776f76fc8a6d9dfa0cc17414e82e28c6"],["/posts/1817748743.html","4867e4b4fc04908302dab6a1668f1cd1"],["/posts/1925125395.html","60a9277616f63e7ce4eafcb0c2515545"],["/posts/1966191251.html","f3dff1e0b63157abb63260830383ddb3"],["/posts/1987617322.html","23c695e5f2b807fe0d7705939aab47b6"],["/posts/1999788039.html","f6c0997ef1d751100ddbad57cfeeb7bf"],["/posts/2075104059.html","5974fe11f0040a587b2be2f2d054547f"],["/posts/2087796737.html","41e2b4305f243c8ebdb74518e34c1a98"],["/posts/2106547339.html","4723795293480c082b1f5f36e5d59a0f"],["/posts/2207806286.html","413365c838fce7df1e44165b60ed835d"],["/posts/2225903441.html","06505f3f4161fa003daffa9fdcb5818f"],["/posts/2265610284.html","29543b6af0e462093cd1547496137815"],["/posts/2281352001.html","317b7e35a922cade4e02f4dfbf0963af"],["/posts/2364755265.html","00dc440aa931ec8dd25ba07573be5fdf"],["/posts/2414116852.html","a354caf17b2cb993093884a6805d3ece"],["/posts/2421785022.html","6499708145dc988bcc52254d1cc86417"],["/posts/2482902029.html","488f79226d7389dbfd41413a0fc01692"],["/posts/2495386210.html","67a88981abcdaf815fe7c91450c9e564"],["/posts/2516528882.html","84af59f3aac0680523143773f450a9da"],["/posts/2526659543.html","62d67f313c5430661f24ae93de93fd02"],["/posts/2529807823.html","c83382c65bf5de23efbc9b2d920565bc"],["/posts/2596601004.html","5ddb39893f29df9152f4161ba688de2f"],["/posts/2697614349.html","2271edf1213559693ef6a151fca514a9"],["/posts/2742438348.html","6f6ed8b5ce493a99b17a63bbf7051895"],["/posts/2768249503.html","89d391346e8832d02ef107860a8cb7fb"],["/posts/2864584994.html","4f7e2cdff9aa802a4447c87077fa5ee7"],["/posts/2888309600.html","bc79684b7360e71e8538c35151b7cee6"],["/posts/2891591958.html","17f7229af35fdfda3c225b6fcf57bcb8"],["/posts/2909934084.html","001523f8f41c868792b9f9d9c7059b35"],["/posts/2920256992.html","cad3866d93428230db5e7c91a4b49902"],["/posts/2959474469.html","d679c57e050887602b628a9bf5f9b36f"],["/posts/3005926051.html","78af8911634c757dad7706885bdf9083"],["/posts/309775400.html","3a22ab77afa3ee4853af1493f34a6f3a"],["/posts/3156194925.html","5583bd2e79df68ae633da89953a8f8cc"],["/posts/3169224211.html","842254d82a724bb169c80992b35e51f9"],["/posts/3213899550.html","1fd4e1e97f8852bc2888db9692a2f3c9"],["/posts/3259212833.html","95f7c8ac54e455caa27134d928af46ae"],["/posts/3266130344.html","4c1fcabcae52d0de6dfdbf8104c13916"],["/posts/3292663995.html","66f508400e0e8445ced4e0441dc42f0f"],["/posts/3297135020.html","6b12247735fb2e8ac5b98f9580cb7e1b"],["/posts/3306641566.html","81ed586e05f058a5b7d8307c6b042803"],["/posts/3312011324.html","c7a003dbd618d4662bacdd04c0922d4c"],["/posts/336911618.html","2a6e7334167c94eb91c82766738ae1fe"],["/posts/3402121571.html","d70b23f246e2ff3f3ade32feb3297457"],["/posts/3405577485.html","8bc8c02620ab7c382c770fc4fdd8e4ce"],["/posts/3498516849.html","43156cd75da1ee50a6bdf756bf4a4c9a"],["/posts/3513711414.html","330f904e9277ee0629447ff999473015"],["/posts/3523095624.html","d9c0b9b9713be3a838eff749e6402eac"],["/posts/3546711884.html","fda696bf0cd8ee45ffee859ee135ebcb"],["/posts/3731385230.html","0d18a9eac712da92f83946d92364357f"],["/posts/3772089482.html","33f7e8d6df4a4b65228ce5b7564ed412"],["/posts/386609427.html","f8d73fc0e8db882cb223a81224533bb6"],["/posts/4044235327.html","6d1bd82731165fefa80bbeb6cad298c2"],["/posts/4115971639.html","148d23ab8ab3f0f89caa358480fa03c1"],["/posts/4130790367.html","2c0aadd6557185b2750dac66d558de79"],["/posts/4131986683.html","6e7ef9c7b3808eb2a7692820f9dbbbb9"],["/posts/4177218757.html","38f90ea2a8679d701406702fcbf220e0"],["/posts/4192183953.html","5c2f8234ff338466e9068cb7f0380c62"],["/posts/4261103898.html","fe0bca1a4590ff66db552008f984e774"],["/posts/469711973.html","cc0e8569e1156fa322c638157b3087d8"],["/posts/482495853.html","9e3d62e900972c6c9eb57129f22f147f"],["/posts/488247922.html","9ceec2b6d13700d03c8bb308b3d7b7bc"],["/posts/517302816.html","3158f00853075cdddb8e6c392913bc12"],["/posts/570165348.html","b912c4eaf4d634a8c63995acf485afdf"],["/posts/595890772.html","fe25891db212a1ada5d5f95c5c50c741"],["/posts/67485572.html","d4b617523a8e56bb407fc6bff15881fe"],["/posts/694347442.html","d36d43abf126c54f2ba1cebe79168dc9"],["/posts/707384687.html","8e7d32cd37a8936bbd1fe75259cef593"],["/posts/71180092.html","6748a7262d62bd817a1f346d8272a845"],["/posts/716459272.html","51e53907d7dadb7ddd031ccaa2eb9e6b"],["/posts/765481613.html","afbe8a588f8ab5e3162d0c40d1e2849e"],["/posts/778231993.html","42c09e693295a206c15b602cdc9b237a"],["/posts/795397410.html","8eb5d122e40fe6c4501222fd7de90c51"],["/posts/820223701.html","568c4cf9e3a81805bdfa5705d604f6f9"],["/posts/830372185.html","fb35b5a7973de613a7f2dd1aad19c973"],["/posts/88294277.html","90bce9bc8a86f82d397b493bf04fa79a"],["/posts/939963535.html","647a6410033a2d23ce5ca17630a12b3d"],["/posts/983786067.html","d9fa721fdf4bc5435a053a8ca9db4076"],["/sw-register.js","990cc28005fb4d9a49fa594f41b68e45"],["/tags/C/index.html","f7c7c8dc1cd747524aeb80f912ea86ca"],["/tags/C/page/2/index.html","525c202e5c64357b28b5baf7eae73712"],["/tags/C/page/3/index.html","83d6c282e6e30b7d9bb320e4c03c0cd1"],["/tags/C/page/4/index.html","85261f6accf3d2c0256005f5dde3f6d3"],["/tags/ETL/index.html","2c6a83c530bd1e81ac26c91524d6fdc6"],["/tags/ElasticSearch/index.html","413e689d43d9abc4b2320613d8825316"],["/tags/GUI/index.html","8d2a8cd116538aa3261c43d28df31602"],["/tags/HBase/index.html","2ebf1ae049119b6cd06647e1b091f7ae"],["/tags/Hadoop/index.html","b12b1672cf3be8770a7525e9699697f9"],["/tags/Hadoop/page/2/index.html","18520b594c7c734c87a58ed91a1ef276"],["/tags/Java/index.html","28be880c10ef75388a17af7df9661396"],["/tags/Java后端/index.html","33bdb05c8eaa61b9b01cab19f8a3757a"],["/tags/Java后端/page/2/index.html","2dd73311f2d574363f2a2049182a216c"],["/tags/Java基础/index.html","50dc95fb4a908332b5ebd08d10e95e04"],["/tags/Java基础/page/2/index.html","650ca2f0031cef200881c32ee3b4d01d"],["/tags/Kettle/index.html","09d24a56a8e27bc19dc1fb4e9065b55e"],["/tags/Kibana/index.html","3b5ea295c8f612d3eceb911fb73068d5"],["/tags/Linux/index.html","3ad7440e8e2c4cb65bf3309f7c766598"],["/tags/Linux/page/2/index.html","fb943504c55e4b1e52bf18b62f2e6b3a"],["/tags/Linux/page/3/index.html","bc9a12bed5deb73fb1a841a4db026627"],["/tags/Mac/index.html","6165e1b1c006824e33b165729c38b3a8"],["/tags/Mac/page/2/index.html","fc17b8823c16384c3bdf6f9303a5797f"],["/tags/Maven/index.html","1d72e1723978c1cee8e458fe43633103"],["/tags/MySQL/index.html","9df3f567b324923ef9b964171356e98f"],["/tags/Python/index.html","4ccd8a01022d92c19fc2e2b55a14fabe"],["/tags/Redis/index.html","6deb69a08ed594bd73de343c8de81258"],["/tags/R语言/index.html","398a7472f3386da5c92f79239e59c015"],["/tags/Spark/index.html","b3b5f43ac3063aff803f05b7a58639ec"],["/tags/Ubuntu/index.html","8342a314ebe3fb05e2af7b86c57d4446"],["/tags/Vue/index.html","94d43e0145bcac3f0cfbba7fe981ab62"],["/tags/Windows/index.html","85a386cacd6ebdd52867563e038e03de"],["/tags/ZooKeeper/index.html","32b515b0f5ed439f6c4a6e05fa12aea8"],["/tags/bfs/index.html","d2e6f58a2ca8f78ccb6c8f3e3a6600aa"],["/tags/dfs/index.html","d08b85488f378f68d71b720a0563e37c"],["/tags/folium/index.html","2796f2b518d20f0f69864e633a8556c3"],["/tags/git/index.html","983bb9105b5266fdcb2e8e1780865558"],["/tags/index.html","cf3176a85ea55bb765dedee9bab2d7cb"],["/tags/latex/index.html","7e0fe96d10ebe4d0f92156d29312b8d3"],["/tags/中间件/index.html","8097adad3e600eb013be2d67552ebd31"],["/tags/二分查找/index.html","cbfab14bb8ae58abd5f34e85ff7541a2"],["/tags/优化类/index.html","09b91290d91bd7b05bf890f31242a024"],["/tags/前端/index.html","01666ce768487211b8bcf6ecc561bcf0"],["/tags/前缀和与差分/index.html","a28c5965dbce16cbc9d5d06f5cadf479"],["/tags/动态规划/index.html","794794c31b640a60aa8fc1f22f356be4"],["/tags/动态规划/page/2/index.html","7b7f9381ba80483bb31f9e862179a343"],["/tags/博客搭建/index.html","b9553f1887d1bc68039697b9e25f486a"],["/tags/图论/index.html","9f57c50e1ed398d6c64c8d07480b6683"],["/tags/大数据/index.html","ed651372247baa75b36660301d1a48a9"],["/tags/大数据/page/2/index.html","e0d151e9e376af28507219b714190945"],["/tags/操作系统/index.html","f2e15ca35bb5472fd093f8a32c2fb861"],["/tags/数学建模/index.html","a80c998015ffe77b9c5733e554fdb403"],["/tags/数据库/index.html","5eb934ec4e74831e912f9cc1dfa9b292"],["/tags/数据结构和算法/index.html","b362a0254c9b16904e5a719ee0b71115"],["/tags/数据结构和算法/page/2/index.html","48f7897af044f5e59db6618ceae396cc"],["/tags/数据结构和算法/page/3/index.html","77f991c9c04a59074b2b89a23130fb20"],["/tags/数据结构和算法/page/4/index.html","802306ea53b5b3ce319b94459aa38b14"],["/tags/数组和字符串/index.html","c9f8c8d88d25b40cffeac98c811d7bab"],["/tags/数论/index.html","e61c29b2c42dc33eae4a73b42b7a6b9b"],["/tags/枚举类/index.html","c4e818bd4366d573ec667285e729adf5"],["/tags/栈和队列/index.html","33f558c0016475e5c4b7e78e31ef55ec"],["/tags/树论/index.html","441399cb9df31d87eba5895d19a83056"],["/tags/测试/index.html","94e5e710f3508b23a650e89720bf31fe"],["/tags/环境/index.html","4a582c506c060c305e0d4f962d690f5b"],["/tags/环境变量/index.html","bd7120766d0262005ded208bbc623044"],["/tags/绘图/index.html","da4578eef0e9a6ae3a538a4c0fddb8a9"],["/tags/编程工具/index.html","28661b4a7bf003f86b748ed67d71ff34"],["/tags/编程环境/index.html","5e8dbd5a294829e8537279a093629bd0"],["/tags/网络编程/index.html","ba0a108c74ac15388cdf0a04ad8202cb"],["/tags/英语语法/index.html","ead045a653caa670d895cb6bf296daf0"],["/tags/计算机操作系统/index.html","dde2c390587925c9c0862587607f53a7"],["/tags/论文/index.html","04ed0bc3389741427e534776d74f967c"],["/tags/资源下载/index.html","cf42ee2f98179e62cbee990769d222a4"],["/tags/链表/index.html","edce2c4fd501d626922643bdc1cb54f0"],["/tags/集合/index.html","4f4619645a6bfc8403eccd6db6520e52"],["/tags/集群/index.html","e5111bc54931db184f51da6228b42d16"]];
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
