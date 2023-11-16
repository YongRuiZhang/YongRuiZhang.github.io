/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","247493c4075514535e90a075e6544354"],["/about/index.html","35bee1f5d29c67011a0039e1784006ae"],["/archives/2023/01/index.html","0c50ab2d5c575ed0e24fbcd0a5ac6173"],["/archives/2023/02/index.html","3408259cdcc121ce720eb7278fa76fae"],["/archives/2023/02/page/2/index.html","94761e42e7d04295db8b678f9d70a767"],["/archives/2023/03/index.html","239a3fc86a5a8fc48dafe95956647edf"],["/archives/2023/05/index.html","d8ef209ed36f4b87133d25e8c7d34a1c"],["/archives/2023/06/index.html","aaeee384f4d751746050bd5987b4c865"],["/archives/2023/09/index.html","9590df6b699383316b7afdcff6d76508"],["/archives/2023/index.html","53585872622124c387408aaed852b8ed"],["/archives/2023/page/2/index.html","6dbc48bb7bf96395e3a3094c5c5680ce"],["/archives/2023/page/3/index.html","02669c0f9d91d9a60d65d96300847071"],["/archives/2023/page/4/index.html","22944db15fc60c359a9202ebbc1fb80f"],["/archives/index.html","bcdb3d22ddfc3d4cb87072d0c6b0cf86"],["/archives/page/2/index.html","8da833b8abd31b5e1f9e29924260ea4f"],["/archives/page/3/index.html","57d74868f3d93fcb0437abf543287f14"],["/archives/page/4/index.html","cc42caa189dedf6b141008661bc18905"],["/baidu_verify_codeva-qQP2iZOMLX.html","9e6044cdc36c5a784c5286c85db54380"],["/categories/Java/index.html","0cb1a6432f0653aebaed75ec183927b7"],["/categories/Java/后端/index.html","2514249d49e17b805372ee0db54e1549"],["/categories/Java/基础/index.html","c76e3098244012827dcb5dc5c95f2973"],["/categories/Java/基础/集合/index.html","9dd05675ed7caa4044f313cf22e72590"],["/categories/Python/index.html","1f6cf4854e4925af4ea58589448c45ad"],["/categories/Python/编程环境/index.html","ac41041cbe955a9aea9626b620fe0fcf"],["/categories/R语言/index.html","837637d8a559066d6a120453375bb59b"],["/categories/R语言/编程环境/index.html","f441b1ce59e1eac37004c9e42f39c3ef"],["/categories/index.html","9907d17b9c675153aa477053aa74f460"],["/categories/中间件/index.html","44cd5a722d0e1d7542403d18f4bee228"],["/categories/前端/Vue/index.html","5ec2fc34540638f87422988ea854874f"],["/categories/前端/index.html","9cdc8c0c94622509c3c81021ada11697"],["/categories/大数据开发/ElasticSearch/index.html","5cd7c94ac509491fe302d36a71cef0de"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","46b89f5253a580a854bc3450be4c3108"],["/categories/大数据开发/HBase/index.html","5930db85e41e03bbe79f4ef6f60b4c06"],["/categories/大数据开发/HBase/学习笔记/index.html","6bcf9c08f635beabd4dd3f6c7a37f849"],["/categories/大数据开发/HBase/环境搭建/index.html","8270e9571eef9603926e91f3c13a5d5b"],["/categories/大数据开发/Hadoop/index.html","1b3abf77fe202bcc9c4c9464ef85b826"],["/categories/大数据开发/Hadoop/技术/index.html","a82d0b4b741a5ca80eb2c20b45d31690"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9687d2d56171d408c35b812a569c929a"],["/categories/大数据开发/Redis/index.html","19ed406001b8d8cc8f5d35bf1c385f9a"],["/categories/大数据开发/Redis/技术/index.html","72741db3b197aacbbb317db4440183a8"],["/categories/大数据开发/Redis/环境搭建/index.html","97c93503ebdf2bbd6d96f1eb299dd80b"],["/categories/大数据开发/Spark/index.html","92a0dd68c6de31a0662b70b5d9600ecc"],["/categories/大数据开发/Spark/环境搭建/index.html","eac436d2f8d42839c5e798b8c0862fd6"],["/categories/大数据开发/Zookeeper/index.html","d5090336370050a69dc01487ddf52c94"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","fa74bf8272e8d542c45aec6db7bc7e7d"],["/categories/大数据开发/index.html","b783677725bf994370510d0842b5fdf3"],["/categories/操作系统/Linux/index.html","b78854f3da56fa1819fae568783b2102"],["/categories/操作系统/Mac/index.html","a88db4c46c1cb5b50f932e7c1febc6fe"],["/categories/操作系统/Windows/index.html","71f5798f897db906e443952c0f695c16"],["/categories/操作系统/index.html","56e15254f161cd141132ce31f6a59b3d"],["/categories/数学建模/index.html","df83afcb895e7a9aeb9220ea42317f35"],["/categories/数学建模/latex/index.html","a4fa3d8207d5f9eea15f59518849ed11"],["/categories/数学建模/优化类/index.html","715d22ee2c177ec5d54aecc1587b40d4"],["/categories/数学建模/优化类/现代优化算法/index.html","1dab359ec32d8aef7e2f4becb600f0d1"],["/categories/数学建模/优化类/规划类/index.html","53293c694b9d02dbb811848c76e07b72"],["/categories/数学建模/绘图/index.html","89b75bd9c11d90311dda821d59c0e57b"],["/categories/数据库/MySQL/index.html","a2446f3c9c250c96635d06432995fd0e"],["/categories/数据库/index.html","df3b5c93f1495dc6869e8e86f3826f0e"],["/categories/数据结构和算法/index.html","70a7cf5b48915865f55df344049b4aac"],["/categories/数据结构和算法/page/2/index.html","b1d161a5885b7d25a63e9f6edb6a1595"],["/categories/数据结构和算法/基本原理/bfs/index.html","fdc49974c9b7344c6f9d1f6e21def63d"],["/categories/数据结构和算法/基本原理/dfs/index.html","2fe9f1e204848d070c0536be66b37f91"],["/categories/数据结构和算法/基本原理/index.html","e3ee6c460c741c240fd88092f9267f07"],["/categories/数据结构和算法/基本原理/动态规划/index.html","32b36b72f3eb8bae1af75d4fc9c2787a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a339320d3dd92cd2333af8aaa6e39870"],["/categories/数据结构和算法/基本原理/图论/index.html","f29fb1ceb606d62da21889984da92b9c"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","6a579115fa8b85f236ed710e5b767f2b"],["/categories/数据结构和算法/基本原理/数论/index.html","9f68cc45d13a00e7ef08a8a633df7665"],["/categories/数据结构和算法/基本原理/树论/index.html","eb8afc42f580ca41147aa0ea08a91a0f"],["/categories/数据结构和算法/基本原理/链表/index.html","d1bab0321ec2107b9122325685f88b76"],["/categories/数据结构和算法/算法题/index.html","07b263e6f979af99cbe218ad4468679b"],["/categories/数据结构和算法/算法题/二分查找/index.html","3e58460d47ff87d72728454806a7c76e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","d2059f1fda7aff23d933f371d730f312"],["/categories/数据结构和算法/算法题/动态规划/index.html","4dd17fe0c6dc9ec222d8f1213912f5d6"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","40ca4b1983c935a7421e938c72a23f17"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","9dff98abc2b0b560231a42f4d12a2a73"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","66c41c8e32021e4992b42eb6d2f2a5e4"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b9591e19dc49fbc6d54176b47ee192e8"],["/categories/数据结构和算法/算法题/栈和队列/index.html","bcdc09948d71e3e4757436617b2d08da"],["/categories/数据结构和算法/算法题/树论/index.html","6f0b0b40d9702c965b39c9df7828d45c"],["/categories/杂七杂八/index.html","9a511cae8b950631eb26e564c9cc30f2"],["/categories/杂七杂八/博客搭建/index.html","0590358c0ff0b6499644c5c1737d640b"],["/categories/编程工具下载/index.html","da881a38d28425cd3deb504bb7ad65f1"],["/categories/编程环境/index.html","fd1914f5243273f4ba6402e286f26811"],["/categories/编程环境/大数据/index.html","dce4a539f82a653df73bcb9b1e38a28f"],["/categories/英语学习/index.html","cef961fe15b30089f250b8d4e108276a"],["/categories/英语学习/英语语法/index.html","f7a9140b75a7d11d5613f00a33188caf"],["/comments/index.html","f21fd2ce5f3f50eeb283d96d416fda06"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ccd0b010f2d2c34b458f0ab713562bd6"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","51493c9613702d1f94ce266f8948b60b"],["/movies/index.html","d6dbb31946d3a7150aedff5530e6a88f"],["/music/index.html","a86ca576762ee3f66b91725c58caf023"],["/page/2/index.html","2b00fd2b5cc13f6e61709fb7a3ac938f"],["/page/3/index.html","8115dbd7ffdce2ad9eff3c75b85b2b16"],["/page/4/index.html","9b49cb6753c517570414891f0f93e4a0"],["/page/5/index.html","06d74bc9f2c28cb524a5778431ffeb08"],["/page/6/index.html","e8bb4bb0cab710054a33e7e27952ef4d"],["/posts/1021360842.html","c7907df74d280d961d3af1f5cf5dfbcc"],["/posts/1120620192.html","75355fcb9ac3224c76d15f04b29ad86d"],["/posts/1141628095.html","e7f4d4c1be494caf2f79c9fa7359371b"],["/posts/1168613674.html","8857b9b31e4f06f4bd954ed7eacdfc9c"],["/posts/1219920510.html","aebeaa83094a5d5c21dee2063ac07f30"],["/posts/1222166338.html","940fb10471ca64d4aade7645e6ee2562"],["/posts/1259097482.html","201a4e1caaf2f620f87bed5f05c24cd9"],["/posts/1271036369.html","1c3e38a7369c50df61df83dd737969fb"],["/posts/1312847445.html","339758e4296403eb27ae8e4dcc0cd4a3"],["/posts/135355774.html","0b5d343b6ef815500c4c4a4de50ed448"],["/posts/1375344716.html","83c59eb008788a22e217164343123c20"],["/posts/1388991698.html","3482784bbf527282d082d81083d6f4b0"],["/posts/1410315814.html","6b6c2a2dac18711cbda00f287836bcef"],["/posts/1452790229.html","0442adcd7d342d2b2c44eb4b3fbf8987"],["/posts/1470079884.html","57c7344ab6dd3b202acf32c54dbef16b"],["/posts/1470079885.html","f5de822bdb4c7cec3ffb68e64db9f28a"],["/posts/1470079886.html","2a3ece1a83e9421eb4acae3a24a7d274"],["/posts/1470079887.html","583e2f1a82d631a06384b056ed0b7b43"],["/posts/1498536549.html","e9434864adf2e5f7d6f98036787c326a"],["/posts/1547067935.html","c9ca7220ed79dba2cec1be4c6881f1f3"],["/posts/1557866301.html","4efe77087e6f4de647b050105a544ef2"],["/posts/1571776361.html","70c81c05803374ac8c5a6593319c5e47"],["/posts/1605124548.html","4bf3607bc08e759aa2ebe2825647ad99"],["/posts/1633036852.html","9980c929ce24ce380e4ec39519f3bbbe"],["/posts/1674202625.html","4c7186508088bf08465648eedf202aba"],["/posts/1765123828.html","ea8cc9a3c046f16ffcabedbe95b56482"],["/posts/1767336200.html","e2ae9f6b973e67e7ca1b364a993d4287"],["/posts/1776114197.html","75c6a79f17e9906ae02502590d8f8cc2"],["/posts/1817748743.html","60e1d707956a023c0ed0dc7379959268"],["/posts/1925125395.html","04dfe48e648644b0c99b7c3705241ec1"],["/posts/1966191251.html","2268dfdf8a459a5a3cac2e8fa5c5d7ab"],["/posts/1987617322.html","c92efb205f8899a2c522734326f6045d"],["/posts/1999788039.html","b479dfa9926750ce8bf13b16853ef07e"],["/posts/2075104059.html","ac228a102471b3334af886ae5bbb57af"],["/posts/2087796737.html","68992f34347ee05c1c934a929bc994d0"],["/posts/2106547339.html","c420dbc897a6309fa29d3c2b9eb3904d"],["/posts/2207806286.html","aecf04eeb085421e2d7b95a2d9ca7136"],["/posts/2225903441.html","66e3c1d0ddaf9d04f3f2e886e022bdd2"],["/posts/2265610284.html","b4c7400f96cb15c95fe2886185a3b135"],["/posts/2281352001.html","fdbabb9abbd55ab714c793a295f1ff75"],["/posts/2364755265.html","20bc2911619fa883d6e673b1ddf8c123"],["/posts/2414116852.html","5027f33bd6cd93d47d76f7082acea197"],["/posts/2421785022.html","b948ba5af29394c6b30922f1b25cdd40"],["/posts/2482902029.html","3b39d8e3cbf657eba2da8c5cc6a75781"],["/posts/2495386210.html","bd0089473b1ea68bd01ccbb2676dfe08"],["/posts/2516528882.html","c217871b0d84bd5288cb552b903f51a4"],["/posts/2526659543.html","5b40d566b3f9eaf27ad9f9447ff076a5"],["/posts/2529807823.html","93be0d62fbf3994716c0b017c8c0846b"],["/posts/2596601004.html","6d5131810efd683b3c515ab200929a11"],["/posts/2742438348.html","83aa4dfd3a9ef2b76d414c978e4778b7"],["/posts/2888309600.html","0721d0132c390bd761c6ab57495e6857"],["/posts/2891591958.html","f779919736cd47c3848067ce104bfd91"],["/posts/2909934084.html","0542c88df4968a9b24fa47eecfce9ea3"],["/posts/2920256992.html","45db21bf97449de99affd76d0f3b1b93"],["/posts/3005926051.html","59c6294476e839336f04bb9ce15e0c42"],["/posts/309775400.html","b47a0223bb7b3f317a2b1a70fe63aa4d"],["/posts/3156194925.html","341d195af5d0431887baee43606789c2"],["/posts/3169224211.html","19e1a80f63ee6b025229db5f0fc07709"],["/posts/3213899550.html","82816cf1ba38e93ff277d236be3d2c0d"],["/posts/3259212833.html","811522a352f6a3c02b2795bf755ea3e6"],["/posts/3266130344.html","b606a9944e57d2b1dd33285da1169535"],["/posts/3292663995.html","b342b7abe7ed344b33388d79da33bb91"],["/posts/3297135020.html","4fffe24ad4fc01739f77c8e31fe26b96"],["/posts/3306641566.html","d270212e869c55946744fd785f10af60"],["/posts/3312011324.html","3496a864e8649a404228bdb602ef090e"],["/posts/336911618.html","cd9fed26ad21e9e12c5851f576f5a7c4"],["/posts/3402121571.html","ceb63a98c2995e68e815e2078576ff41"],["/posts/3405577485.html","f13cae44a7fd4384f0f87d5f14806fec"],["/posts/3498516849.html","254b4c43365d5f817bb166f57a9a4134"],["/posts/3513711414.html","1bbe983737476b40a146950c0a844876"],["/posts/3546711884.html","9ecf6e6d42360453fcb087dedc6ea7d9"],["/posts/3731385230.html","d8fdaa99741f66b1ec8f7a347ba5209b"],["/posts/3772089482.html","385b812d61dc9feb39fef07626bef59f"],["/posts/386609427.html","4978de9263d0e7e965ea55fc5a668694"],["/posts/4044235327.html","81b9c6dc27b4323a05c4c9d96b4cb7bc"],["/posts/4115971639.html","a8455684ad79cbd42719d2bf89e118c7"],["/posts/4130790367.html","fcf8256511bfb10e05463ebe959e7876"],["/posts/4131986683.html","102e2a81aa8ab467204f077eb2c7a67f"],["/posts/4177218757.html","dd5d41e62fe7faf8e63c976210303dd6"],["/posts/4192183953.html","4dfe29da02fe21b233865d5f4ed3a851"],["/posts/4261103898.html","4a732ddf82984459dc23b11e70c93cf6"],["/posts/469711973.html","87ba2ca56926b741306fd77e3f77def8"],["/posts/482495853.html","2b04934c24beb5f6db8b24b65dba0b46"],["/posts/488247922.html","713e02f1d4613d115410e4535e10b1c7"],["/posts/517302816.html","4200319c96be61e154f66c4e259172d2"],["/posts/570165348.html","1eb5298e9f92fbf1ab8a62e9c4d5f041"],["/posts/595890772.html","955a88c8729de144d5e6181f145485ec"],["/posts/67485572.html","5a8b498317d88046106cc34d32a4b656"],["/posts/694347442.html","9c203058319b6922ed684bcfcc23424e"],["/posts/707384687.html","ff98e049869f10e19c54d6860b7409cd"],["/posts/71180092.html","73e3a3192b6d5099c21358c53ff12658"],["/posts/716459272.html","1379f10c3525b8469c2ef261831b832c"],["/posts/778231993.html","60ae890e708c796ab3fcc573e38b7175"],["/posts/795397410.html","96a4ca360a3354178b0fea3cb19808bd"],["/posts/820223701.html","51d416539a810e682a5b62ee0eb25e65"],["/posts/830372185.html","f5bcd06637fb886c3c5449dbc8a737b3"],["/posts/88294277.html","a018ca715e5f5e2efe4b94c04ce30328"],["/posts/939963535.html","d15f629c817142af833284ee16d95050"],["/posts/983786067.html","b4af565362ec3b0e5fa6d06412bc8ffe"],["/sw-register.js","783db12b8e43c331b2b82d4843cf64e2"],["/tags/C/index.html","8c918e902a905c28824b4c0814a324b2"],["/tags/C/page/2/index.html","d53634ea006b533b86497e091adc38ee"],["/tags/C/page/3/index.html","f07b41545a79ea870a4b560692658789"],["/tags/ETL/index.html","beec6cf073aca99dda08bfd4435a33b6"],["/tags/ElasticSearch/index.html","e949a1d07239e7e7e2c51d19053f8a18"],["/tags/GUI/index.html","92cfbbf55d478cf24f58f59cbbf92821"],["/tags/HBase/index.html","12d124e4a5c8bf45f4734de628a3815c"],["/tags/Hadoop/index.html","f9042218b1e610ff5eb441f5bad45efb"],["/tags/Hadoop/page/2/index.html","75ec19d06d1690d769d661046688321a"],["/tags/Java/index.html","6fbfb2b9fdd973f9fe97703cb40e122c"],["/tags/Java后端/index.html","c18724afef15536bc05cb170adcddd9a"],["/tags/Java后端/page/2/index.html","4ae304abba64b64791a5d7d1561b84c3"],["/tags/Java基础/index.html","26f10b14e0739f3d863c7cdfdbdec8ed"],["/tags/Java基础/page/2/index.html","2be4af564c0a2631dec638dad96f68a0"],["/tags/Kettle/index.html","56d470199e7553d3e1c0b4127280fed1"],["/tags/Kibana/index.html","33c2de9c8d8f28802126633b8bc68915"],["/tags/Linux/index.html","120503804a3e20246f48125d6e4175bc"],["/tags/Linux/page/2/index.html","977c626d864294703af3b772c61f46c4"],["/tags/Linux/page/3/index.html","aff8c89d00759c23237ff3893274cdea"],["/tags/Mac/index.html","9430a751d433657b89a788f0d2597062"],["/tags/Mac/page/2/index.html","81d73d130a07ab20628d76857942f540"],["/tags/Maven/index.html","ee9192cbceb7712fb89c3a259ff365db"],["/tags/MySQL/index.html","c59cbd558c3d6dce063dd3b803249633"],["/tags/Python/index.html","7d82700867feaf21909ecf5fba89979a"],["/tags/Redis/index.html","bef13993285073a1ebd5c4f660d1a8f7"],["/tags/R语言/index.html","a824e9d930b7d3965fff4a87bcd1f013"],["/tags/Spark/index.html","b3c54876508ca6259225207977d4ce90"],["/tags/Ubuntu/index.html","1f059180a2c1ef35bf236a6f026b8912"],["/tags/Vue/index.html","7092fbe66f11c55b964c4fa28c2c26c3"],["/tags/Windows/index.html","6882afed998cd894439e11f08d454a2b"],["/tags/ZooKeeper/index.html","917cc306fc25980a3b878aa9f197cdd8"],["/tags/bfs/index.html","60282b4c46921b7eb4050d41dc71eb0e"],["/tags/dfs/index.html","4e6f3726e02e5cc8aeea765166dedbfa"],["/tags/folium/index.html","5fe77c57e58533d150e70c26e07799d2"],["/tags/git/index.html","fc834d6666069ef59b97ff25902767ed"],["/tags/index.html","8bc55928aa8949186339aee522b7db63"],["/tags/latex/index.html","fe75ffccc48e5307ba7e5cc739a1a799"],["/tags/中间件/index.html","631b689186f320c4991adc2ae4856bb7"],["/tags/二分查找/index.html","ef46a99c369316f47e64c771ea95581f"],["/tags/优化类/index.html","cda739c9b681741f2457ca7c49a0a668"],["/tags/前端/index.html","c65ddfa937dd0eee4ed6f2851d4c566b"],["/tags/前缀和与差分/index.html","24484040cb464fd4f6f07d1723cd9381"],["/tags/动态规划/index.html","09baf1e26122b18a6d5006e417e3c613"],["/tags/动态规划/page/2/index.html","d19e0f464a1216d9b7033c68a60ab924"],["/tags/博客搭建/index.html","8c16e5358b2fb52ae1e48674d1a1c3f2"],["/tags/图论/index.html","7e769f3231ee26f7246cf7f87eb53275"],["/tags/大数据/index.html","32e088b5dd37fb2bb1bde7a7c78f9d75"],["/tags/大数据/page/2/index.html","52136620e4b12d0fbb1dcc3cec2bd50f"],["/tags/操作系统/index.html","f6dc776a7c3ff2bb1f361ee8387bc82e"],["/tags/数学建模/index.html","1d28b9c12f34aed2a45e0b9c80897144"],["/tags/数据库/index.html","d5fbb8e345ac4276d09d992390a21cb5"],["/tags/数据结构和算法/index.html","0a420e039282f57115cf96cff74c3ab9"],["/tags/数据结构和算法/page/2/index.html","abf57a1a985c5e75134520dca9af3a3a"],["/tags/数据结构和算法/page/3/index.html","d235ed80a8f0b08c1dc0c1ad7d4e0718"],["/tags/数组和字符串/index.html","f168e1997b23b5bd57192eadcfe5bc76"],["/tags/枚举类/index.html","e82c438d34b506a63a205325d6772b40"],["/tags/栈和队列/index.html","54cc0a6bfef0c31f2769563534cffdcc"],["/tags/树论/index.html","cefd59678db10c8e513ff2f466e47e79"],["/tags/测试/index.html","716f25d15f268d37eae04ef1f6800511"],["/tags/环境/index.html","032a6047939b1887ae96eee2a448c496"],["/tags/环境变量/index.html","ed874c3e4d7d9a2f3a137afd3e7f89cd"],["/tags/绘图/index.html","be6fcfe2a62e55c55946ea10b74510a9"],["/tags/编程工具/index.html","24a11c39b4dfaa7b9f1f688202327067"],["/tags/编程环境/index.html","800ded1050255963786abd1e2da169fe"],["/tags/网络编程/index.html","e1f78b439fd9747314488be9319089e6"],["/tags/英语语法/index.html","4ad593905b6b918e3b86da6ffd02e1e5"],["/tags/论文/index.html","7792f7476e7578dc6876eadafc423575"],["/tags/资源下载/index.html","e61164d3c10c9f1713cc213832840a71"],["/tags/链表/index.html","014cdb093718953c5187e051f377a387"],["/tags/集合/index.html","9489555e6279b28ef9f2b28c9ce9de5c"],["/tags/集群/index.html","305c79a58717c77ffd151cff67a4064a"]];
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
