/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","1aab26e633aef946b127ce0d2b4705c6"],["/about/index.html","51ae658cc74a90898d9425b04fa61719"],["/archives/2023/01/index.html","f1e98c4906f110841154555b4d06b43a"],["/archives/2023/02/index.html","79f3bb401d7f7a629f165ed3497a82ec"],["/archives/2023/02/page/2/index.html","9c0621e2517412b1143a201ab1629d56"],["/archives/2023/03/index.html","bbd4c19bc48cda8f91980cef287d2bc2"],["/archives/2023/05/index.html","8e698fe66b315a7de7d69b7d98c650b4"],["/archives/2023/06/index.html","4eee0672a29bbe5f3e53f24a83199ee1"],["/archives/2023/09/index.html","a921e4711054098804555214b637f182"],["/archives/2023/11/index.html","616a4c2336045d7f081e9afb8462b30c"],["/archives/2023/12/index.html","5eae4c2684c22daef7b59b7034048838"],["/archives/2023/index.html","7566646de3e18ea060beaf2783bc129e"],["/archives/2023/page/2/index.html","314afb4547bdd863adda6386d759d908"],["/archives/2023/page/3/index.html","c9afcab1198bb8d99c970fb4a6669479"],["/archives/2023/page/4/index.html","03da8fed474b58d9ed596a3590640339"],["/archives/2024/02/index.html","30d6763a3738ca492d3448315183659f"],["/archives/2024/index.html","6ba478db32ace38bf3cd92f80b0197ff"],["/archives/index.html","51562d5ba9cf8cec6c871814abfee3c5"],["/archives/page/2/index.html","906281c047f7ff8ff7f0011703aa8ce3"],["/archives/page/3/index.html","66b85523b76e377f21f1bedeadc7abcc"],["/archives/page/4/index.html","b27089b0f7c825a66ee6543562171850"],["/baidu_verify_codeva-qQP2iZOMLX.html","830a607f9cab019f7b331959711dcd35"],["/categories/Java/index.html","3bf19193f2662a0caa22e77477737151"],["/categories/Java/后端/index.html","04561b6a58d1664ef96b43309cf12078"],["/categories/Java/基础/index.html","22846b69822e16712957918db1b6aefe"],["/categories/Java/基础/集合/index.html","2e893ef74ceedfed4f6ba235f1078adb"],["/categories/Python/index.html","8b8c3e02d72b1cc1c684007244ca6f53"],["/categories/Python/编程环境/index.html","ee15f7be2e446deead25b46deda33bb1"],["/categories/R语言/index.html","701bd4159f337ab609106319885388a3"],["/categories/R语言/编程环境/index.html","e1c620a0f8588c3dd25af4e8d72c0272"],["/categories/index.html","fbe3d2594fa03febe442d419899504af"],["/categories/中间件/index.html","56f76a4e3d5414f1ae68afd5350041ee"],["/categories/前端/Vue/index.html","3583bce3bdf7b55e2cf8424301950e4c"],["/categories/前端/index.html","c62b6be0beb29527960364560eca983d"],["/categories/大数据开发/ElasticSearch/index.html","2a02b8c348879060082c5df8f90dd973"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","260468b32a9f543a6924277e7bdab221"],["/categories/大数据开发/HBase/index.html","c77b8427cd45ef83b97d6cc192d25380"],["/categories/大数据开发/HBase/学习笔记/index.html","923bae90af722d045f08fd46eb3898f3"],["/categories/大数据开发/HBase/环境搭建/index.html","cecbf1f0c78463e9785801775c380aaf"],["/categories/大数据开发/Hadoop/index.html","f51639986fd3aa09c469c356cf1bc92c"],["/categories/大数据开发/Hadoop/技术/index.html","53aaa71b003322afb0a5c014058b07d2"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5f5664e800f6f2b3d8c4f90d707d84fc"],["/categories/大数据开发/Redis/index.html","1837315309e6a50404030105365a17a1"],["/categories/大数据开发/Redis/技术/index.html","a03f4d55873161ff83ef46c0a4a3d159"],["/categories/大数据开发/Redis/环境搭建/index.html","305316155bf585ebb4b291b68497edb4"],["/categories/大数据开发/Spark/index.html","be740e172b8a4b6ccf946b1655e23dbf"],["/categories/大数据开发/Spark/环境搭建/index.html","bffffdd96f7f3344baef32c0fcf3b806"],["/categories/大数据开发/Zookeeper/index.html","e44feac6d513b6d3a6029a7cbdcb10c1"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1d0fc3b08c8925e791dc4e722b0c50f4"],["/categories/大数据开发/index.html","be57c3914b273f3cd1a982cfffe819f1"],["/categories/学校课程/index.html","a73d29b2fe8d4c7180f9ac0bb5ca32f8"],["/categories/学校课程/计算机操作系统/index.html","bc3555315baa971f91303030f5ea7dee"],["/categories/操作系统/Linux/index.html","c71676620d4b11046bdea3b4170438ba"],["/categories/操作系统/Mac/index.html","bcbea78e4dc32bd1050ff3a428008ade"],["/categories/操作系统/Windows/index.html","c750276a15dbd80c7557ff1d9535c2f1"],["/categories/操作系统/index.html","c1e7df4c1e7948e97f385d7844893c76"],["/categories/数学建模/index.html","fa029eb5c2841cd4ce083e78696beb6d"],["/categories/数学建模/latex/index.html","b7305d131b2a8c0075468ce532b71f4b"],["/categories/数学建模/优化类/index.html","88f6fcef9c54463ee7ce86d2e8b651a8"],["/categories/数学建模/优化类/现代优化算法/index.html","f0f8f5d5b605a7131ecb04ef71ae5672"],["/categories/数学建模/优化类/规划类/index.html","b1fde193856f1d7fe6ee34949fb860b2"],["/categories/数学建模/绘图/index.html","e31bd78214d5f5f5a23278877186cced"],["/categories/数据库/MySQL/index.html","a94f24d0921bc808b3fcc1f81a98c7e9"],["/categories/数据库/index.html","763bb4a66c52ef8dd84b56b9facbc657"],["/categories/数据结构和算法/index.html","4e89c4799fcc2654ad7074ea14a94390"],["/categories/数据结构和算法/page/2/index.html","b57d9ca0d77331df87a83a4da30b1f05"],["/categories/数据结构和算法/基本原理/bfs/index.html","8e9342c4ac332d6cffa94d7c70bda75a"],["/categories/数据结构和算法/基本原理/dfs/index.html","cdd493ddb3ea3096ecedb2756e77d705"],["/categories/数据结构和算法/基本原理/index.html","98deff737cae6a233c2402bc5f50868a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c689d239b007e768bdc513b4d7432180"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","cf7ffb1655f8afe6f40668e193926381"],["/categories/数据结构和算法/基本原理/图论/index.html","4e91502948b9b5cce691b5d81a1a8b59"],["/categories/数据结构和算法/基本原理/字符串/index.html","388ab0e5b4d00b8efe61bda62dc6c3a1"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","82b371a2b9fc6e2c8f11e3826f6ed539"],["/categories/数据结构和算法/基本原理/数论/index.html","9360029360aa906328c6cda35e37e8c5"],["/categories/数据结构和算法/基本原理/树论/index.html","5cd4d99449fde01fef1c773974f2e029"],["/categories/数据结构和算法/基本原理/链表/index.html","d0a46f4498d33dad12bb1419a5ccb951"],["/categories/数据结构和算法/算法题/index.html","06571e8028c4bad4e98100d218c657da"],["/categories/数据结构和算法/算法题/二分查找/index.html","24a64bc3df6839698708823312e7dfb3"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f5803e7720eab56f504f150a292ae5bd"],["/categories/数据结构和算法/算法题/动态规划/index.html","ccd48c5dec0a28daf63145c7792e86b0"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","80e7f19debcbc1c3a197ccd11916e3b3"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f8c2091b44aaf5b93b0325784bf54de7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","1c0d20340804ee0a8202f81b614c61ee"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","c8eea35256a49822b858580454e86656"],["/categories/数据结构和算法/算法题/数论/index.html","6dfdaf536f6d800845072b7815ea8b73"],["/categories/数据结构和算法/算法题/栈和队列/index.html","2839d904b674bb9fbb652c479481dfd3"],["/categories/数据结构和算法/算法题/树论/index.html","87cf75fbf6399948b14960e81275d16f"],["/categories/杂七杂八/index.html","4e94eac3d66e3f2ddc532b29b81bcc10"],["/categories/杂七杂八/博客搭建/index.html","edd0c55db7b32544c1e3daed8c5c1c05"],["/categories/编程工具下载/index.html","182bb944bff07e9a50cb4bc0b8c3f59d"],["/categories/编程环境/index.html","33f45c8875f0de597f1658ce4852ef59"],["/categories/编程环境/大数据/index.html","94fee56c5a366b44bf70c8475a9fab86"],["/categories/英语学习/index.html","f5a0b88aa634530c9b296a8c2fc5343c"],["/categories/英语学习/英语语法/index.html","da9829a671d650c7f7cf99e75ba1637a"],["/comments/index.html","178c43fd4ec29547b6ede1fcf09dd548"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","c96337d1b6ead12cd775d65d1feddeaa"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a0a5c247ee10d860a5cf83c62e125336"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","6f11aabc95c353b1b485baca7c0254cd"],["/movies/index.html","8b6db7d6067ff24aca9f140cc8d28706"],["/music/index.html","3bed7706a2213b6b14b7fdbaede2a17f"],["/page/2/index.html","1fbadb7102a193af3db06306e3b5f523"],["/page/3/index.html","dc1269bdd17ebb3c7f1cd93f7f8f7644"],["/page/4/index.html","3e6bc5bea74d9dacd5fc4a75a126ffca"],["/page/5/index.html","d2d9f615c534cc8f8e438324080bea3c"],["/page/6/index.html","49912a1702bd9134bc4aafce3c614add"],["/posts/1021360842.html","a75b80e6c216cca852247a0671c40abb"],["/posts/1120620192.html","59e04ce6049744e61386c256c00d2603"],["/posts/1141628095.html","3680a36b3998a4bf5941c6fd741e077c"],["/posts/1168613674.html","1d73ad4d95052dbeddbbc494649f79af"],["/posts/1219920510.html","1e31a416ccd381ed6f27b4f7d77a10ea"],["/posts/1222166338.html","a80ffd94e2ecffe46072a2d38afd3338"],["/posts/1259097482.html","195fb0e05d237067c8a095719923a058"],["/posts/1271036369.html","7c090a0df00a727f5c6dda03d000f26d"],["/posts/1312847445.html","b5483deb65bc8ee3cd85577bfd93a24c"],["/posts/135355774.html","e98fc2719c3c43a7585cbd0e56104807"],["/posts/1375344716.html","11ccc85dde087350a86ff2ac96b14610"],["/posts/1388991698.html","9411b69e90e3333781399ec4450a858f"],["/posts/1410315814.html","019e018b419e7eeafe18fe8ed1a0e228"],["/posts/1452790229.html","db50ed3f498871cc142c45bc36769897"],["/posts/1470079884.html","16aab1fa0792b1de9309b480c62b524c"],["/posts/1470079885.html","83dc892ac42287f3656cbafbc8be8237"],["/posts/1470079886.html","ff803d731e7c2da2e331b0974d459daa"],["/posts/1470079887.html","9f268f93c59991857274a6f85640869e"],["/posts/1498536549.html","617e578c8c2d1f90013af70c351ad8c7"],["/posts/1539568593.html","cf021dc2cd137a6a131b3aa83f667992"],["/posts/1547067935.html","ae1cf1a57ae79f5e4b83881d7ae4259f"],["/posts/1557866301.html","0c48eca9ccd9a23298147846176978a5"],["/posts/1571776361.html","9c88f5279eaf9965a53dea62b8473a69"],["/posts/1605124548.html","fa5a0f2cf967548c382e5ca73c65ed4a"],["/posts/1633036852.html","43be2a3c72e34d6a767d628167373e0e"],["/posts/1674202625.html","3d46852e6cf9f341e3b9c458ce417792"],["/posts/1765123828.html","949179b516ac3f0c03d8ea47f45e775d"],["/posts/1767336200.html","709935627b5518fce5bd2d037ec8cfa6"],["/posts/1776114197.html","fd340eff491ef857fb73dc6716816452"],["/posts/1817748743.html","f0d263acc857bb3c3b741b39ea2ce426"],["/posts/1925125395.html","d979fb2383b7e60ceadd25e65852272a"],["/posts/1966191251.html","16f92e9a0082451970067bfe19bd5a6a"],["/posts/1987617322.html","de1593377a67a704bc5b62772b2ef01a"],["/posts/1999788039.html","8e397ca30ae29a2895c267362ad8d7a2"],["/posts/2075104059.html","c15093c7970ecb6bb9c3d1521e4e7b3b"],["/posts/2087796737.html","8613eab2069627da92cc56c0f1a204ca"],["/posts/2106547339.html","28459cf2bf462b630cc7ad94e3ec2d9f"],["/posts/2207806286.html","67931cff8b9d8a912d46dc82c840dda3"],["/posts/2225903441.html","fb81950f0cc3449210afc4af6824dfcb"],["/posts/2265610284.html","53440fbbdabc76095ca627669455222a"],["/posts/2281352001.html","9854e2a533f7ee2aa6da93f74f2de65a"],["/posts/2364755265.html","1cf3914d30b4f4fa380dcab4ba36192d"],["/posts/2414116852.html","8648b661c3c37e20933557f07b3fc891"],["/posts/2421785022.html","f4813905a38bb01d61a7c8cf8ac4617b"],["/posts/2482902029.html","799dc6b5accadd5bc21236d9cf7d1181"],["/posts/2495386210.html","7f511154c3615590dd31190cce786119"],["/posts/2516528882.html","9b08621b4f554127bde1b1f0e6998527"],["/posts/2526659543.html","cd9906021045279c5db01d01faec1b95"],["/posts/2529807823.html","2d7edc1eb1eb8791e64b5ef90faadd95"],["/posts/2596601004.html","204813a8f5abc7c56eb8fa6a14e509a0"],["/posts/2697614349.html","4333ca7b43b0fa3600612a1293ebf327"],["/posts/2742438348.html","d7ace91ba1cc3d4c73602c7a0a33a478"],["/posts/2768249503.html","744723be0fd71dcf678169ce0b7207ce"],["/posts/2864584994.html","5fb73a1e60f029660ac3a94773d8d018"],["/posts/2888309600.html","936e780724bf1a73e333f0fa81f3d83d"],["/posts/2891591958.html","64e69e2f3fb43578978a2edc26e03ad9"],["/posts/2909934084.html","e4a21f374ed4bab3ad0fe5a3c368d491"],["/posts/2920256992.html","192390862d401b52db59db68fc2ffb77"],["/posts/2959474469.html","92106ae9b9f3308be82e85de8945a523"],["/posts/3005926051.html","d1991af9607fc465c8ecd21f66fb240e"],["/posts/309775400.html","1f916746d78b8ac2affe54d6d2728b14"],["/posts/3156194925.html","30c325aaa36aa6ad8a3f500f3d90419a"],["/posts/3169224211.html","9cc8c8d3f2bf65f617eba323bd4c98f1"],["/posts/3213899550.html","3bf0aef194e172b6c8233f72efe62c7a"],["/posts/3259212833.html","bec241ee6f67a4cac8ea4a296e0e2b2e"],["/posts/3266130344.html","c3d9521c865bc181623ce642ee7da115"],["/posts/3292663995.html","b152aba56500ccb727d0f4ccea44c4ea"],["/posts/3297135020.html","70cd032d1635da395a5ccb2f4e1cce16"],["/posts/3306641566.html","3307be73fd30f720b3b8f56b649d0487"],["/posts/3312011324.html","643cf6b29a8b49e1edde5874e5cf67c4"],["/posts/336911618.html","8e7c126aec1614083a711dc6d7f53612"],["/posts/3402121571.html","22964a454f91b4d0f10afc04c7df010e"],["/posts/3405577485.html","31757ffa55d82bdb204a338c4b4ebb9b"],["/posts/3498516849.html","2ce7c8291176b6b0f8828ea805788d0e"],["/posts/3513711414.html","e93ae92a3f42ffb04d241fa14c6b99fe"],["/posts/3523095624.html","684c72b56dcd794ed7fb21fd1aa6745f"],["/posts/3546711884.html","254e8bda92d8eb10212c7b7b4cf1b806"],["/posts/3731385230.html","7ab2ca685c1643265c3730f713830a00"],["/posts/3772089482.html","4034666662a6ea6d6956d2cfb3d75d63"],["/posts/386609427.html","7f56da44db1574bfeef3593643abe0e8"],["/posts/4044235327.html","fcc8ed162b361db0364ba7b79f3acd17"],["/posts/4115971639.html","e71058ea90ecea3eb906134b9f2c1b5c"],["/posts/4130790367.html","94f2f13f8e5e26d0c9a5c2a9ee053d7d"],["/posts/4131986683.html","4b17aedec1e816a56ffd79b1c18ea93b"],["/posts/4177218757.html","bf2cfedba67de909f64f33818511a7d5"],["/posts/4192183953.html","48af612a400bd0b93ae74370e49a6770"],["/posts/4261103898.html","091b912dc035327ac393f6637404bd8e"],["/posts/469711973.html","01c82725f3268f9e1c38a7141d64d063"],["/posts/482495853.html","7f47fd16dadf743af69cceedb14fc4f9"],["/posts/488247922.html","ddd763d99b7fdd9df26b81cb65d1d04c"],["/posts/517302816.html","1c993b58a5d3cf4170f29c4cbb3acf1f"],["/posts/570165348.html","d3c55bc3f1a5dddc92140783843b6d44"],["/posts/595890772.html","de585774b7318e2b55f644e32700fc86"],["/posts/67485572.html","ce45f29f3673101fad15cf5e1b696125"],["/posts/694347442.html","4962b243d2eefe880846de53d16828ff"],["/posts/707384687.html","976b38e9eec2611177c3f2abc21e29b1"],["/posts/71180092.html","15c4ca39fbd731af421d7ca1ca0c3d9c"],["/posts/716459272.html","290c3372676ea808b2e711f81f974ff1"],["/posts/765481613.html","614deb5a8b0e5d04a1e63d0c5017b7f1"],["/posts/778231993.html","c9757a3cefdea9bfa2015785e822e63a"],["/posts/795397410.html","447c619b90a556e563c784ea24ae4f57"],["/posts/820223701.html","c2d7e80182988a4bd9a63627e6ef7510"],["/posts/830372185.html","1c1924fba7e3a678e81302ae79d07bf1"],["/posts/88294277.html","01f8de4ec872ebe1d45e175887c4b11d"],["/posts/939963535.html","9416960a9e5c1bc606ace02309872007"],["/posts/983786067.html","353932bae7ebea562981595306872146"],["/sw-register.js","e1da7a4a2bdf64a28a7eb69d78520317"],["/tags/C/index.html","a364c0c3af51c5418a64dab3a2c71a91"],["/tags/C/page/2/index.html","0511e1f88f7d175a5b545568db4151bf"],["/tags/C/page/3/index.html","8fa35bd2f89e00c7842bfdaf68807733"],["/tags/C/page/4/index.html","50a333c49ca9108b490e0a33fc65eef2"],["/tags/ETL/index.html","4cbc8d1110038e8a8f5cecb4ddc7da06"],["/tags/ElasticSearch/index.html","0634402635044929fc36804aa2e20632"],["/tags/GUI/index.html","c2949daf92bfa2a2443a92a278a17652"],["/tags/HBase/index.html","326be50a180047ef69544d8ae0e8f0ab"],["/tags/Hadoop/index.html","806b3d00d40060a662bd728f826e565c"],["/tags/Hadoop/page/2/index.html","1791426cf98638f26fcfcf76682330fa"],["/tags/Java/index.html","3f6271accda124b145938e2f5a1063ca"],["/tags/Java后端/index.html","7a3771a601a0914bf8e5bb36da6e9a55"],["/tags/Java后端/page/2/index.html","980d351c3dd232a569d02d08e043744b"],["/tags/Java基础/index.html","ad41fb537456326650b6b5126f5271ef"],["/tags/Java基础/page/2/index.html","c9cb71d07d0e94afaae119a9310a1c2d"],["/tags/Kettle/index.html","d1ebc2fe07ffb43c5285871d22b5b2e6"],["/tags/Kibana/index.html","e13b1c12b242f92d869e3103df7bb524"],["/tags/Linux/index.html","f7a7248f1836d8ebafa5985f602f3913"],["/tags/Linux/page/2/index.html","6d5b8bd2350d3f0c61aca57cd72ec284"],["/tags/Linux/page/3/index.html","6129d2cc9bd2a72ddebfeffbdb5ad580"],["/tags/Mac/index.html","ed2dbd0b1496026268083bf731ec4fd2"],["/tags/Mac/page/2/index.html","9c14d25e6aabef6aecc807d29c3bbcfe"],["/tags/Maven/index.html","bee7c62fb9998870f3addd542798d0bd"],["/tags/MySQL/index.html","b65643ba32d5034caa0610e65f19cac9"],["/tags/Python/index.html","581ba627767d1e2785cc2fd8e4d0f43a"],["/tags/Redis/index.html","bf22d41a3a0033968cd1c3ccd73e489a"],["/tags/R语言/index.html","165afedcdfb9e276751b888824eaf73a"],["/tags/Spark/index.html","6fd64fb621fdfd2f29d992605f5a4f0f"],["/tags/Ubuntu/index.html","fccaf88eb9b66b5d9418d35a6f6d4bfc"],["/tags/Vue/index.html","eae6f0e73eb2ba94b456a8b0aabd360f"],["/tags/Windows/index.html","d53ac2a50d65b2d995f1661f80946aab"],["/tags/ZooKeeper/index.html","9705db3e14169ff437badf76bb60ae3b"],["/tags/bfs/index.html","e147ae535fc8443d501c7cc7cdb9954e"],["/tags/dfs/index.html","1b3abfe81b1485c8f9a1ccb2702ccf59"],["/tags/folium/index.html","a51c38a5f32cf1d4b38ac2b3f71932ab"],["/tags/git/index.html","dd52817255084b05f09735df899d0a68"],["/tags/index.html","24af10cc37010039faa5bc50468fa021"],["/tags/latex/index.html","33264cc2852aa03b9910c2a441547520"],["/tags/中间件/index.html","ce1e1674f937aa40666557c124f74c11"],["/tags/二分查找/index.html","99ee3b40f408f27da8c91198d3f9a61b"],["/tags/优化类/index.html","eb13c20a2fefe2380983dc7f7c2c1295"],["/tags/前端/index.html","6d733d4aa5ab0bfad47b598c157160c2"],["/tags/前缀和与差分/index.html","c312fbdbe7fcabb8d90190b17c744d16"],["/tags/动态规划/index.html","68d16e69bb88541d887dc8c5e5a34d6f"],["/tags/动态规划/page/2/index.html","c30cf7200ed0f4cfd8bcc95426a3b5af"],["/tags/博客搭建/index.html","ee86bde84db48de930509fb2f0dc3b1a"],["/tags/图论/index.html","794c42685e453cf7f06cbf50296ed41f"],["/tags/大数据/index.html","e3b535563c316e13e51e0e42fc500876"],["/tags/大数据/page/2/index.html","fa7a94273536f39bcbcbed16210af4d5"],["/tags/操作系统/index.html","1fa5408f71724e025cd9fe68db629f8e"],["/tags/数学建模/index.html","283e81cd1e98facf15493ca874319fab"],["/tags/数据库/index.html","ff6979076341434ba2429c9840ced79a"],["/tags/数据结构和算法/index.html","6cac5ce22ccd613ebe36f80b1c5f7e2a"],["/tags/数据结构和算法/page/2/index.html","599da469865acba1199c77ae586485b6"],["/tags/数据结构和算法/page/3/index.html","4d040468dcf419b109e3580184e34300"],["/tags/数据结构和算法/page/4/index.html","22e12efe132029548705c6896c44c3d7"],["/tags/数组和字符串/index.html","435cca4da3f28588290019de1bbeccb4"],["/tags/数论/index.html","4a329bb6be79d5b5db65b1d3b8a8861c"],["/tags/枚举类/index.html","b7434d4ba2f19ff76d56210b05471f94"],["/tags/栈和队列/index.html","8d0874f478b2353f545075dabe39b4b2"],["/tags/树论/index.html","24fb8ef79f7e808f46427fc7b78293be"],["/tags/测试/index.html","1ec84c1e5637f963df010041408f7ea0"],["/tags/环境/index.html","c04f400a96857c0c6c02d8607eded4f4"],["/tags/环境变量/index.html","d661cd46f16a2868f5791034c7549c83"],["/tags/绘图/index.html","599636f2f01e1493cd76b3496ad7615e"],["/tags/编程工具/index.html","aa104e6047a8fea501b9690dd80257eb"],["/tags/编程环境/index.html","55b7bd914eab0fade13dd2ca55d7f13b"],["/tags/网络编程/index.html","ef76bf7701598e6452028867fe229633"],["/tags/英语语法/index.html","c2f24bcee9c4b819fb29e0a4b037732a"],["/tags/计算机操作系统/index.html","abfc66656ad6299736693d3b65fc5a5b"],["/tags/论文/index.html","69f1ece99e5f78d152e110da2476e03d"],["/tags/资源下载/index.html","5d1e285088e7c8f88d69326c4e9e2b2d"],["/tags/链表/index.html","5328aa49d1e6c4e685a260220fd98040"],["/tags/集合/index.html","a98362b0f8eb6a56e507b2da86ce2aac"],["/tags/集群/index.html","3e42d72e120ce24543e63505a9311d23"]];
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
