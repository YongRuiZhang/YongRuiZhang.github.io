/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","2f154c2cea7d19fc20e5a2ebe9b7837d"],["/about/index.html","e02a342176586644851cd7c220ce0f1c"],["/archives/2023/01/index.html","ea80c2a3d9a2c3a7d680ed7571173fe2"],["/archives/2023/02/index.html","8cf962e5738fc8505cecea3a20ba383a"],["/archives/2023/02/page/2/index.html","842cbf561e7a09ceb6c13941e2e1f324"],["/archives/2023/03/index.html","d2db5cfff0f43bfe6934177e2b6b7135"],["/archives/2023/05/index.html","72920d1516524d21768f0ffb8d23e02c"],["/archives/2023/06/index.html","75da48ee8f3658de239ee48417d19870"],["/archives/2023/09/index.html","effaf271405d2c86e47e1ee7aca2a978"],["/archives/2023/index.html","83e57396b6d106a7b655c621f5c98fa4"],["/archives/2023/page/2/index.html","bd0f0dac46521634687cffaa0e618b9e"],["/archives/2023/page/3/index.html","27e9a9ee5d6eb17714cc0a7fd9343857"],["/archives/2023/page/4/index.html","b8511120525b62e017333b5ab33ce0f0"],["/archives/index.html","7822111f206f6da02fa2052207205c49"],["/archives/page/2/index.html","4194b3eb495ae08f2a3c0d7ba8f44ec8"],["/archives/page/3/index.html","610dad8b2320590dbbd6bed862cc1875"],["/archives/page/4/index.html","b10502ba7f5851cb954865c0c9dd106b"],["/baidu_verify_codeva-qQP2iZOMLX.html","d737d5ac4b685bcaa88a351939fe16fe"],["/categories/Java/index.html","6777feef294c84b554e2a870e365246c"],["/categories/Java/后端/index.html","672847883d7262907cca6c261b38ba1d"],["/categories/Java/基础/index.html","b09c641e9880a35eb14493776096a8bc"],["/categories/Java/基础/集合/index.html","7b61ac8ee1b433e87e1624a83d00ca9c"],["/categories/Python/index.html","8a462bb5b41c06b945134ccffae6d1cd"],["/categories/Python/编程环境/index.html","6d32c9ca84b08e2e7a506086dc0922fd"],["/categories/R语言/index.html","6ea841fc10ef6f9a2418f247ef9a716d"],["/categories/R语言/编程环境/index.html","77a0f6350631fdc25d858cbc0e73ca78"],["/categories/index.html","834197390a06208c4c57b1894820aa2a"],["/categories/中间件/index.html","9355d8fa1ca6cd63555471e63b04a931"],["/categories/前端/Vue/index.html","75a3c0098b43aeb691cce0541174ff60"],["/categories/前端/index.html","26915d9a2d35fd7f64e34ee0a67819dd"],["/categories/大数据开发/ElasticSearch/index.html","4ec7a99670a27140820f3076588652fe"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","87201ba590020f0001086669a6703aae"],["/categories/大数据开发/HBase/index.html","5e79617623687ba4c82bd9ea3fa2aee8"],["/categories/大数据开发/HBase/学习笔记/index.html","00e644f3005552a0d0990fa2893b0288"],["/categories/大数据开发/HBase/环境搭建/index.html","4c42fde9718207cff98b1c86e48b8a64"],["/categories/大数据开发/Hadoop/index.html","34031c1e96e895e60cf9715b552dba37"],["/categories/大数据开发/Hadoop/技术/index.html","533256750adf27ca3cf68c519f1e7699"],["/categories/大数据开发/Hadoop/环境搭建/index.html","67b74396d6483e8645556f43dab63342"],["/categories/大数据开发/Redis/index.html","8d08740c091c04878f7d8b7314f1e746"],["/categories/大数据开发/Redis/技术/index.html","300bce03ec106b742885ec57551c4b06"],["/categories/大数据开发/Redis/环境搭建/index.html","8b5d5370cfad6c71a054c74e810f467f"],["/categories/大数据开发/Spark/index.html","fadd6ba328109d3f73c8a880c733789c"],["/categories/大数据开发/Spark/环境搭建/index.html","efd9ce1c381662431c66869532a69901"],["/categories/大数据开发/Zookeeper/index.html","d8674638e68520d1fed28a3a44002b38"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","4401ef94f903602fe6f800cefb350e3f"],["/categories/大数据开发/index.html","543e35ddbeee0485d2d2ee0f8f6663aa"],["/categories/操作系统/Linux/index.html","59480be2474a24e56ef2524a47d0acbd"],["/categories/操作系统/Mac/index.html","03e79006e42638d8b6d28136724b1864"],["/categories/操作系统/Windows/index.html","7fe82d886b488a8efb63af0f0c5ec6d1"],["/categories/操作系统/index.html","8f4e9ce9490dedba4e929ecf2ca169e2"],["/categories/数学建模/index.html","c103b09adb0b26530e69e4fa06ac94ed"],["/categories/数学建模/latex/index.html","8a2ac3a5fa3ab5371a86dd522b2b3bd4"],["/categories/数学建模/优化类/index.html","796efbff45456ed92791cc6f99dc5447"],["/categories/数学建模/优化类/现代优化算法/index.html","8babf134b681c23896c14a9879558159"],["/categories/数学建模/优化类/规划类/index.html","f8197ebcceabb2443efa4f5d23b1c51a"],["/categories/数学建模/绘图/index.html","cf69325fa3056150b9361a38dd2d63b2"],["/categories/数据库/MySQL/index.html","a3708a118ff939a547d740500a6dd2a8"],["/categories/数据库/index.html","cfe47ef11f331d4ff54d6a66f9b4c809"],["/categories/数据结构和算法/index.html","3480f56bca847881b3ccde945b871c07"],["/categories/数据结构和算法/page/2/index.html","0ce159a80869535eeae04169ded4427f"],["/categories/数据结构和算法/基本原理/bfs/index.html","212f21ae64b37413f6783516d94b9ab6"],["/categories/数据结构和算法/基本原理/dfs/index.html","c83019e023971eb0c941722596e95e65"],["/categories/数据结构和算法/基本原理/index.html","2e246300d6b75b93c5c4a6fb32fb43aa"],["/categories/数据结构和算法/基本原理/动态规划/index.html","735a226ac0109b0eec2c4fbf49cb6214"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4248097339ad724c0db06207713f804b"],["/categories/数据结构和算法/基本原理/图论/index.html","7742c74edfb6d0926d25747c2c4e0424"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","781ce25dc578b8d38d150765477c9b4b"],["/categories/数据结构和算法/基本原理/数论/index.html","25f6787c55f734143cce7d787ad185c9"],["/categories/数据结构和算法/基本原理/树论/index.html","ca6ac704776e2d82e44a9737f6c0f262"],["/categories/数据结构和算法/基本原理/链表/index.html","f93d779a895bf8ba16d0594e18e49ea1"],["/categories/数据结构和算法/算法题/index.html","2feaf521be2af708fd55267c6acbd50f"],["/categories/数据结构和算法/算法题/二分查找/index.html","3ee8fc4f4a4644bdd8e04ddea2e13ad8"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ad8205084bdb3bdc91773ba2ffef264b"],["/categories/数据结构和算法/算法题/动态规划/index.html","49d3f39af3f94bbd88ded0ad7c28fc2c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2ca3922991e729ca5faabf9387c4cfa0"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","dd9f36d27bfb31dd317c22a83c28c1b1"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","0cc973187fbacc84241a634229dc352a"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","59648d24a3bae97cc1a5ea22cca9caad"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f8f6d0bdb7ca987080235553d2ba9321"],["/categories/数据结构和算法/算法题/树论/index.html","97f5f77bcb1466137ce5d492a5339470"],["/categories/杂七杂八/index.html","5b73f213f160383a3b2c721db94a5074"],["/categories/杂七杂八/博客搭建/index.html","ea04956e88f402fa9c141365dd737cff"],["/categories/编程工具下载/index.html","2ad2438f342e49dc8d0be34545986f5d"],["/categories/编程环境/index.html","226f4aaccd64791ab0e49ed062345a4f"],["/categories/编程环境/大数据/index.html","835eb0badce7a20a8be0e7e78d1dc4bd"],["/categories/英语学习/index.html","99f6e6bec5f6ece7a8a374ba5f555f1a"],["/categories/英语学习/英语语法/index.html","4bcc4f8d16e23b921eabd610c7dda52f"],["/comments/index.html","5a6661e8fa78caa2b2fee39d1ae667fd"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","949e0d72fc760af824c42966795d5d93"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","9c02164200f17f48a1dc7797a9d6f95f"],["/movies/index.html","c26aa3caad96d511fc924a163042d456"],["/music/index.html","5adf6b76fa17b5120fb4128d4edcd798"],["/page/2/index.html","5460419a08bfb75bfc7e290ca809a743"],["/page/3/index.html","82a699ad71a384301b5687d594df8f93"],["/page/4/index.html","66ec11c4cc5d3a36681b12e45e23689d"],["/page/5/index.html","122fbb9bd2e227504bcb38828b1dd78c"],["/page/6/index.html","fac2220d651c45a2dc7db2e5f9e7c9f2"],["/posts/1021360842.html","12c2c260aa572bc199b8fb3ca722e8bc"],["/posts/1120620192.html","06151e791360c514c52c2b06b28ffcdd"],["/posts/1141628095.html","592f3620d2ea21e759b855abebb24308"],["/posts/1168613674.html","b1b481d0f5566cf77e11de6c851d73f6"],["/posts/1219920510.html","3fd1b4616929ef07c192db1c9736a774"],["/posts/1222166338.html","cd3d780a7d4cc26df62c12665039d235"],["/posts/1259097482.html","0aed35ed1bd139817d1145692c89bf94"],["/posts/1271036369.html","ced746b90a7cb315862f2e7402feb65e"],["/posts/1312847445.html","fca2a97ec7cc88d23aa981758add28a8"],["/posts/135355774.html","502c763220cb77a4d72246fc357a37b7"],["/posts/1375344716.html","03306173b39976881e7128c405c7d552"],["/posts/1388991698.html","a99433b122a9652064f0f46c917becea"],["/posts/1410315814.html","91c1d7302031ca9ba8ebf4e21fbdd0f5"],["/posts/1452790229.html","6b8d55bd5238371c7643cb69c7ded60c"],["/posts/1470079884.html","0525ac26284b51cf4a2025668f6a5a76"],["/posts/1470079885.html","b21b4bdae6d6fedf86dc3cc460b8806a"],["/posts/1470079886.html","ec4010121ac13ccff288ebf1440c188f"],["/posts/1470079887.html","3649959523149d8fa57de1382a633ba1"],["/posts/1498536549.html","98812a636d4c09eb793664d528ca9385"],["/posts/1547067935.html","1e771a780ef1b43e4591101b0c28c4ca"],["/posts/1557866301.html","b3ad95deee32ac8b97e2edfec1653aeb"],["/posts/1571776361.html","1fec70970fa9badd95212a2d364550aa"],["/posts/1605124548.html","0660cced2739c84d98a4cf124a1283e8"],["/posts/1633036852.html","cf594b9df10858a0eb9446de05080ab6"],["/posts/1674202625.html","fab959323af73e72c87f8357ca9bf031"],["/posts/1765123828.html","cc6c44d754f961a5f54db3b9310d3bd2"],["/posts/1767336200.html","46fd484a8558c93687777d52b251aa6c"],["/posts/1776114197.html","4c57bc3f4e4a6cf0c5036f24714102f3"],["/posts/1817748743.html","2a2488f7a0b6c5e93f1191178c500e01"],["/posts/1925125395.html","a6ba8535256e0f814b289adb405edfa7"],["/posts/1966191251.html","72ee715d696683e22c5600a719a1448f"],["/posts/1987617322.html","19a1500de9244c52ed252e8000635711"],["/posts/1999788039.html","591a9ae678d57b45c2b07264d7a381cf"],["/posts/2075104059.html","2665804f3ec2b1b37b9dafadb9944053"],["/posts/2087796737.html","0a1b985de78aef0f6f8e89102b7e5eba"],["/posts/2106547339.html","062ee51d5a63b84a61113df6fbf7cda6"],["/posts/2207806286.html","3ceeff5d543ca4799ba5819100669e2f"],["/posts/2225903441.html","1e650359b5de578ed098681bdea48c11"],["/posts/2265610284.html","e91332fb474098eea62918a4e3c6c420"],["/posts/2281352001.html","af1cf17a7720ad53ea5d5e0276473407"],["/posts/2364755265.html","5fba18b28f7db75b76615e77a75190f7"],["/posts/2414116852.html","4fe00f24f5caad03f6e986e6cc857fab"],["/posts/2421785022.html","65ee113eb15f305f9b61b1338517bd7d"],["/posts/2482902029.html","7c808c8c19dd67077fd88e5daa12a7e3"],["/posts/2495386210.html","c6fdd72150233ead4557180e1b85f9be"],["/posts/2516528882.html","9e0f4d1066d5ea5d4d0a0f36f349548f"],["/posts/2526659543.html","76025e165fa2b0f6f773038e054a9a81"],["/posts/2529807823.html","34f535b985a63d6a07df6be16e64197a"],["/posts/2596601004.html","2b9903239b7f407c7cb52af153bb0898"],["/posts/2742438348.html","7fb49f60d14d53c471b8ce557d20fb3d"],["/posts/2888309600.html","eb7c6a2e0bfa2e0f423cf96397fa7e68"],["/posts/2891591958.html","75d7ef91dad881972d69dbd8eb062d92"],["/posts/2909934084.html","7ed793d037d315d722517206a2dd3a38"],["/posts/2920256992.html","ad6369241fc65e9baee902edcd140417"],["/posts/3005926051.html","510cff923c07498b0d66a726f965dcf9"],["/posts/309775400.html","3623cb910898bd5b08ce42974e213e80"],["/posts/3156194925.html","e23fa71111ceae715d3d79e73f916e2d"],["/posts/3169224211.html","0731658cba18770c5b3571bd12b21e5b"],["/posts/3213899550.html","f185dbabf31824797ff24df734d64202"],["/posts/3259212833.html","4f6a11e87981641e73e3c06858ec6a69"],["/posts/3266130344.html","d96f7b2635b6545353477a22dc862bac"],["/posts/3292663995.html","340af0b6e97a2f1fa8d4129378243746"],["/posts/3297135020.html","d6be793fbe248e5c3619cf23bbda6976"],["/posts/3306641566.html","ea8d07ed70ae732917e645996e664bc4"],["/posts/3312011324.html","21be681fcf7765167d158900940fe8cc"],["/posts/336911618.html","f5dda08bcf3437fee4063e7c2d819381"],["/posts/3402121571.html","1bee462b9d5f23b21ff88de9d9b37cf4"],["/posts/3405577485.html","751886e93c696a7d8ca6ae482cccc3c7"],["/posts/3498516849.html","b6350b47f39bd0bbad52ec7a01dc0820"],["/posts/3513711414.html","5a4095b0a057ea8c1913b494a63ef34d"],["/posts/3546711884.html","ac1424bde53f13c87393715a141980cd"],["/posts/3731385230.html","ff9c8ded2883fa1664208a5fc636cc56"],["/posts/3772089482.html","5dbca3a0293ae04e81b294fffe11f37d"],["/posts/386609427.html","67bf44b9a42f3c723688dcb4a68805d2"],["/posts/4044235327.html","12da9772de546aa24223683d50df9960"],["/posts/4115971639.html","18b67de349a8a17908d0a5b65fea1d93"],["/posts/4130790367.html","a7ae973ab80881d5189a2dc9d0fb8a79"],["/posts/4131986683.html","56bf922774c965051244603cde1ffb8d"],["/posts/4177218757.html","208680b5df847534e5c0dff3b959429e"],["/posts/4192183953.html","56573fc79dd790f056a1826a779cd533"],["/posts/4261103898.html","3e8cf9c605e8f3ad7d2bfd28f8a98c8b"],["/posts/469711973.html","3f17e257d42a58230a686412756918ca"],["/posts/482495853.html","918f586345271e21782557455557582f"],["/posts/488247922.html","451ce2ad4c9381fbdae1aebfa425ba8c"],["/posts/517302816.html","f341c7de6467cc84393bc6c1df6f7496"],["/posts/570165348.html","7297d62bb6ed55b61083d0fa514b913b"],["/posts/595890772.html","e7738ab2a2d141011d0cb403c3044592"],["/posts/67485572.html","975d4460ed2752270b1f18d73aae5d15"],["/posts/694347442.html","d14c7699cee26d979a8659ba6a529e5a"],["/posts/707384687.html","201bc9fcc24ba5fcf5d10b1bb23e1994"],["/posts/71180092.html","4884efefc384bdb3595e7031f3be5eed"],["/posts/716459272.html","3f1b31c66a55c2cd4e020a680b851687"],["/posts/778231993.html","d0e0367e02c4e131e009bb0c63739f41"],["/posts/795397410.html","fd4f858ff186e4cfb0957c7a25365551"],["/posts/820223701.html","4e3bfa61ae193562dd908a1733d37e0b"],["/posts/830372185.html","55c485ae0144c5bef0d9db41a7657628"],["/posts/88294277.html","6b15a5351bdcbf3217209c26724f4536"],["/posts/939963535.html","515246d1db6805d746fd14a85c0baee7"],["/posts/983786067.html","d4fbd081f281bd62b3e7e2746b8f3638"],["/sw-register.js","0012a67d8701b589563e426e1b093ecc"],["/tags/C/index.html","acdb28caefdee22371258a1da8c8217c"],["/tags/C/page/2/index.html","b6a582abca35512bf64a3c651e6cb196"],["/tags/C/page/3/index.html","a1ca956a94aa77563d782e137b881abb"],["/tags/ETL/index.html","f07e03382ad870d3f81f380b92799e1a"],["/tags/ElasticSearch/index.html","7646566b5c8ce7608503a13e68cfed36"],["/tags/GUI/index.html","1aa161a12b2efc8095188fb4e5ad9e2c"],["/tags/HBase/index.html","4d6060db933898ab18993dfcbc106641"],["/tags/Hadoop/index.html","57e5adc2144fac6da7e79cda6769f052"],["/tags/Hadoop/page/2/index.html","62278a50c460f437c40594186d680150"],["/tags/Java/index.html","326cedca7c6708f5ef3b13e30136068f"],["/tags/Java后端/index.html","a148caab88d52dbc61b2ad5dac01135f"],["/tags/Java后端/page/2/index.html","38034c74ebcf32c776ed6c4cef090f74"],["/tags/Java基础/index.html","957a4b5cb7812377e6009b42e987c0b5"],["/tags/Java基础/page/2/index.html","b06b2d90cc498170aa76451c54324279"],["/tags/Kettle/index.html","0872ffe23084bdd6baacfe7afe337442"],["/tags/Kibana/index.html","c9e79ab76dbdbc7fab3231ed32fa76bb"],["/tags/Linux/index.html","e622cdf73c6fd8c2a5afdd0ffc4155bc"],["/tags/Linux/page/2/index.html","393a1aef6c09b120a60770c9f5424572"],["/tags/Linux/page/3/index.html","0555f378f3005810fc10afc6c4f62873"],["/tags/Mac/index.html","406d68ef0e177a73f5bcb441b4d64529"],["/tags/Mac/page/2/index.html","fa30b39bc4be51d876c1038247a10f21"],["/tags/Maven/index.html","ea4afa1f5019562b6846686b68eff1c1"],["/tags/MySQL/index.html","23ee6174ddea13190a2b7abf10a2a9a5"],["/tags/Python/index.html","2dd3028cf3d1e76ea26d4ecf890e6957"],["/tags/Redis/index.html","563d898088c5c064a89229da5bae0071"],["/tags/R语言/index.html","aa5b4e528d8906eadc2162b6e8dd5f27"],["/tags/Spark/index.html","9ad73cb6c222992d6a3db81e27c65526"],["/tags/Ubuntu/index.html","611900ab2fc8d448fa792b4b859a3661"],["/tags/Vue/index.html","ec19e3c0a0306385037633b32b568935"],["/tags/Windows/index.html","5b0c6ca5711f2e514c64285fc4a5749f"],["/tags/ZooKeeper/index.html","068679b5e1774ea3317ed05af0374f4b"],["/tags/bfs/index.html","7eddd736f642b536382b510c4f581447"],["/tags/dfs/index.html","d488f0769a8131cb02bab5ceb0455f12"],["/tags/folium/index.html","065dd80dc8c6bb7fc387eb6fec7148a0"],["/tags/git/index.html","87986efe82693755acb31167d2e713fb"],["/tags/index.html","33c301c08d35b5d660d167d62715141a"],["/tags/latex/index.html","64c402feae38c812797a1851c0a13a7a"],["/tags/中间件/index.html","2c8cce10d70a1682c1b804060c4b439a"],["/tags/二分查找/index.html","82715cbb40ef09f2d889ab52a9ee2205"],["/tags/优化类/index.html","d4a278e049e550ead2b312f7b464b133"],["/tags/前端/index.html","4270bec8142f9cd58ddd7eb3dc6f9de8"],["/tags/前缀和与差分/index.html","0eb243060763ee3a24b9d2d69ca9de8c"],["/tags/动态规划/index.html","8ae10a9f4c6c82c1669ce2682b471fff"],["/tags/动态规划/page/2/index.html","af9b348a3330c4a8ecf206d81856bb05"],["/tags/博客搭建/index.html","ff24b616907ecb224aa7ee01524be31e"],["/tags/图论/index.html","4735e1d6eee241c1effa4f2ed2a0424e"],["/tags/大数据/index.html","4ee67de9636c9575c62d62d7c3958096"],["/tags/大数据/page/2/index.html","b2d89ee8b0dff9abe6bb215b23de2a05"],["/tags/操作系统/index.html","931a86bf5685c96eaab6f3ad956bfa88"],["/tags/数学建模/index.html","497c2dc1e90330da050202b5ea3a64c8"],["/tags/数据库/index.html","61588cc4ac9adf1741648bd994c12d8b"],["/tags/数据结构和算法/index.html","a8e9ca9517e97d27e7a65efa9c0a22ec"],["/tags/数据结构和算法/page/2/index.html","023719a1d2e85505b873c20fec300fcc"],["/tags/数据结构和算法/page/3/index.html","c8bf73eb452b41d5c1e547002937faa7"],["/tags/数组和字符串/index.html","fe418727b59e573cf75d80af50f39de1"],["/tags/枚举类/index.html","c51d1f95f36066adf8086573adb757a9"],["/tags/栈和队列/index.html","41bc82cdf53385801947268fa4aab43e"],["/tags/树论/index.html","5ae617c505b8a4c08caffb838b6edc4b"],["/tags/测试/index.html","4a57401108deea4f2dd44102c2c98e50"],["/tags/环境/index.html","64c607dea378be91d79ba18053430b8d"],["/tags/环境变量/index.html","278b6cf4a5a263dfb30cea9aca28af69"],["/tags/绘图/index.html","e8c5520a4e25dfa06ff1530f336575b1"],["/tags/编程工具/index.html","a90bb5fc8b63216ad242ca59550df1fa"],["/tags/编程环境/index.html","70edc1b067a5de4c872a6c8c382c68ae"],["/tags/网络编程/index.html","fa1a21ef2680886c07c24a62dc5fb828"],["/tags/英语语法/index.html","93988f0d6f4b1122056a2ecbc017492f"],["/tags/论文/index.html","5159a67c820d9fa8d2c54d97457f940a"],["/tags/资源下载/index.html","a1d9540625e23896de965435ff6d50b6"],["/tags/链表/index.html","81037c95c0893cd5c06d41625dd38a2e"],["/tags/集合/index.html","a1d11664f9f39618a7242206b8699600"],["/tags/集群/index.html","072b999bf8b36dd55228b0538713f080"]];
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
