/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","19446af200e0853344fc113480f04992"],["/about/index.html","183bddddaecc1118e9401581185f0665"],["/archives/2023/01/index.html","50a8b571bbf777d228f1eadf6e8ec058"],["/archives/2023/02/index.html","ae187b331bb19847733dba8ea3b58824"],["/archives/2023/02/page/2/index.html","8eb1ff7e2fd9a45c7f04737410f3d151"],["/archives/2023/03/index.html","a6fc08c7273147835c1c499ef9ee9451"],["/archives/2023/05/index.html","fb6664c3d0887d92c02a49d5426a538d"],["/archives/2023/06/index.html","022e55ee9a4f80ca355202d91df574aa"],["/archives/2023/09/index.html","0b0c735c7036522c5eda43789716ea16"],["/archives/2023/11/index.html","f7073fd9c4785f114e489c7244566b10"],["/archives/2023/12/index.html","716fbea65f8aa2a2f849408ad3fdf3e5"],["/archives/2023/index.html","a065a706c182b14caa47a93b298f9adb"],["/archives/2023/page/2/index.html","6eecc423bb933c5b67ba990db8f306d6"],["/archives/2023/page/3/index.html","ac380930eaf23438ae5c2bc85f3558c0"],["/archives/2023/page/4/index.html","759f696e96089442049a180453eac6be"],["/archives/2024/02/index.html","02340ed9fb9995c6837a8da0416e9b28"],["/archives/2024/index.html","38d49d0770681c82b943d8121a6b4cbd"],["/archives/index.html","9c65d7adbb17043fde00be06a20d0efa"],["/archives/page/2/index.html","253e3256c7ff93ed5341828530b8ee46"],["/archives/page/3/index.html","dbd198997a26037df4e544506a0558bb"],["/archives/page/4/index.html","e4c6bc718a70b7c00157ca150b160a99"],["/baidu_verify_codeva-qQP2iZOMLX.html","b9ee5378034825856bf0e684bdeabf1f"],["/categories/Java/index.html","4cd3702b03fbddb974e3641d2c929272"],["/categories/Java/后端/index.html","7e89c05633a2b89a3dd1c14467862e60"],["/categories/Java/基础/index.html","ac071e0b868585bfd10ad9678ec72a58"],["/categories/Java/基础/集合/index.html","8fb4bf4243446132109ffec280170d5b"],["/categories/Python/index.html","e19eafae652d837879e7b48f85bbeb4c"],["/categories/Python/编程环境/index.html","26f17c666e14c26d04464f24be221904"],["/categories/R语言/index.html","769150d7bb30dc8780a40aeec5161497"],["/categories/R语言/编程环境/index.html","85b3779a789d8c2256a8b5de20159f9f"],["/categories/index.html","fbe3d2594fa03febe442d419899504af"],["/categories/中间件/index.html","34b29dd2dab02f6f4a5403367441f03d"],["/categories/前端/Vue/index.html","dc34ae5185502167cb2a76e220658b52"],["/categories/前端/index.html","4c3cdeee4bf2ac598e7539d4e68984db"],["/categories/大数据开发/ElasticSearch/index.html","cc0ebcc009b7594435232f7f7fc592fc"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","5f4d650869c092fbfa4748d821db8d5f"],["/categories/大数据开发/HBase/index.html","8ddfb57e0b56022540f959f8599f9fac"],["/categories/大数据开发/HBase/学习笔记/index.html","b2c7718a1f36d97270caff2cc63245e4"],["/categories/大数据开发/HBase/环境搭建/index.html","4e8bdae5d8d4964cd6510a6904d3c75f"],["/categories/大数据开发/Hadoop/index.html","ac581dace1eacc201d09d2570497b172"],["/categories/大数据开发/Hadoop/技术/index.html","2f157eaebe9502936827c6bdc39eab0a"],["/categories/大数据开发/Hadoop/环境搭建/index.html","de00d0da27daa4cbd4d22e41bad1539e"],["/categories/大数据开发/Redis/index.html","b1bad7b4ebbfa795d869d40a6e936355"],["/categories/大数据开发/Redis/技术/index.html","422827f637f43edc05699481f0b78e37"],["/categories/大数据开发/Redis/环境搭建/index.html","5dfcc816722776e2cd3fb1251c3d43bc"],["/categories/大数据开发/Spark/index.html","c563b4ec8ba514fc11d784a5c30bf167"],["/categories/大数据开发/Spark/环境搭建/index.html","89a7b2031276bee19cca3880e6eb5413"],["/categories/大数据开发/Zookeeper/index.html","fbd4e34379a93b7d8b2266d8945cb7d8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","bc693f160606415e744495bfa0459d12"],["/categories/大数据开发/index.html","ba1118a156d9f47a045be9e9518da1eb"],["/categories/学校课程/index.html","c27c0e506bc011c3291ae98486151be9"],["/categories/学校课程/计算机操作系统/index.html","2c305d9fac2e7cf5d22d1b98704cba40"],["/categories/操作系统/Linux/index.html","9b008adaa6cdd2006ebb25806a104a7b"],["/categories/操作系统/Mac/index.html","7dd166a979491277a5c81c96ccbdcc0f"],["/categories/操作系统/Windows/index.html","d1f02a59e04f847dcc9cf1e5b75fec0d"],["/categories/操作系统/index.html","2fcd95bf6ef4f6f235158ce97275844d"],["/categories/数学建模/index.html","25fbcaa96a39fba048d1b6912bdd54b9"],["/categories/数学建模/latex/index.html","324d7a9036ac2550b5cf6a70becb5bba"],["/categories/数学建模/优化类/index.html","2466f5c496390a0861bc6283120bfd37"],["/categories/数学建模/优化类/现代优化算法/index.html","6b9b5ae32ee4e280298ec81f3a11480c"],["/categories/数学建模/优化类/规划类/index.html","f1d16b33a9fe81f9516a01965dadbf91"],["/categories/数学建模/绘图/index.html","75e1695f9c654963c654aa7891e28b64"],["/categories/数据库/MySQL/index.html","1faeee5c94e1b2b00b4fcb86276bc892"],["/categories/数据库/index.html","cdcbaa773c332855ba294f82bc4b7ba9"],["/categories/数据结构和算法/index.html","1e96422710df571de0182da70af095d2"],["/categories/数据结构和算法/page/2/index.html","3c4c13aea04acd5c289883275a9037ae"],["/categories/数据结构和算法/基本原理/bfs/index.html","42026a5fea6032a80b44ec0f3ec9c7fd"],["/categories/数据结构和算法/基本原理/dfs/index.html","1cd7996ad73faa59b27df134ca8afb9f"],["/categories/数据结构和算法/基本原理/index.html","f0a065afdbac119ddebaf1d5a588513b"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9647824e5f51c7e094be406278a7366b"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","7b65696b0a43e299a783efc859222ef0"],["/categories/数据结构和算法/基本原理/图论/index.html","03fc1b2fb387fc1df25c497123189738"],["/categories/数据结构和算法/基本原理/字符串/index.html","df1158b267677b0f0c3500da2e4768f4"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","54dadfc11808d3928256396f465b2456"],["/categories/数据结构和算法/基本原理/数论/index.html","ebc169d405873640d44440b8db57fc3f"],["/categories/数据结构和算法/基本原理/树论/index.html","8d70ff1e55c01805dee4c129be31cdc9"],["/categories/数据结构和算法/基本原理/链表/index.html","dfa8fd61bcbf9c9af54963bb27c87e20"],["/categories/数据结构和算法/算法题/index.html","8eb48a4fdca4b5309f1e1d381bcc8b82"],["/categories/数据结构和算法/算法题/二分查找/index.html","f1f95b5080e64d833253ad7b42bde643"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0430fa5f7321b8043c55cc3cab47cc39"],["/categories/数据结构和算法/算法题/动态规划/index.html","cb1b4b008a139c77326b2187c9b88130"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","1a6085b2acab71f2f903a1784054e41c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f09c1afb5aaaa64bdd4041642d09cd12"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4c2eb1dcbe8fa2f91cdf5f10069addae"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b668ca278279984ce3b1ec0a59e54dd0"],["/categories/数据结构和算法/算法题/数论/index.html","c4323f5cb2074e4fad22af6cda545785"],["/categories/数据结构和算法/算法题/栈和队列/index.html","e77bb0c48ba65d940accb7be93f56a57"],["/categories/数据结构和算法/算法题/树论/index.html","3d452ab5ca2efe8c665f56936ee407e3"],["/categories/杂七杂八/index.html","559e1798aad8d719aff8af9ec280a2a7"],["/categories/杂七杂八/博客搭建/index.html","a249317f8be6864f647b823365910f42"],["/categories/编程工具下载/index.html","406f3b619b46c5b38413b608015ac300"],["/categories/编程环境/index.html","df0cc9c95341fffd235971a15fa4edca"],["/categories/编程环境/大数据/index.html","7631967fc67892d97f4dc92506693573"],["/categories/英语学习/index.html","14b0690460046ef5ad0befeaf6c1e548"],["/categories/英语学习/英语语法/index.html","7c2797eebe2cb3c83678426e7fe2bb3e"],["/comments/index.html","70c01c62597ad3bc8f852e23fe2f3a17"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","97cf13907117c803ac0802de4d66294d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","addcf5ad59bf1ec172ec5ccdabbced15"],["/movies/index.html","075cee3ab7032f9c2d6fc2d19eeea438"],["/music/index.html","92eeb464459e70ca39aa6374ce8224a4"],["/page/2/index.html","6bf10c20e2f4f34a64d48b721d75cbec"],["/page/3/index.html","60d64674aea565e99a3bee98d2252139"],["/page/4/index.html","618655ba3561ad5547f9258af80ff8e1"],["/page/5/index.html","3c715b523904dfc3f54058452a14fb76"],["/page/6/index.html","2a0ac66adf77e24a453f8c8b6c3140c4"],["/posts/1021360842.html","a75b80e6c216cca852247a0671c40abb"],["/posts/1120620192.html","59e04ce6049744e61386c256c00d2603"],["/posts/1141628095.html","3680a36b3998a4bf5941c6fd741e077c"],["/posts/1168613674.html","1d73ad4d95052dbeddbbc494649f79af"],["/posts/1219920510.html","1e31a416ccd381ed6f27b4f7d77a10ea"],["/posts/1222166338.html","a80ffd94e2ecffe46072a2d38afd3338"],["/posts/1259097482.html","195fb0e05d237067c8a095719923a058"],["/posts/1271036369.html","7c090a0df00a727f5c6dda03d000f26d"],["/posts/1312847445.html","b5483deb65bc8ee3cd85577bfd93a24c"],["/posts/135355774.html","e98fc2719c3c43a7585cbd0e56104807"],["/posts/1375344716.html","11ccc85dde087350a86ff2ac96b14610"],["/posts/1388991698.html","9411b69e90e3333781399ec4450a858f"],["/posts/1410315814.html","019e018b419e7eeafe18fe8ed1a0e228"],["/posts/1452790229.html","db50ed3f498871cc142c45bc36769897"],["/posts/1470079884.html","16aab1fa0792b1de9309b480c62b524c"],["/posts/1470079885.html","83dc892ac42287f3656cbafbc8be8237"],["/posts/1470079886.html","ff803d731e7c2da2e331b0974d459daa"],["/posts/1470079887.html","9f268f93c59991857274a6f85640869e"],["/posts/1498536549.html","617e578c8c2d1f90013af70c351ad8c7"],["/posts/1539568593.html","cf021dc2cd137a6a131b3aa83f667992"],["/posts/1547067935.html","ae1cf1a57ae79f5e4b83881d7ae4259f"],["/posts/1557866301.html","0c48eca9ccd9a23298147846176978a5"],["/posts/1571776361.html","9c88f5279eaf9965a53dea62b8473a69"],["/posts/1605124548.html","fa5a0f2cf967548c382e5ca73c65ed4a"],["/posts/1633036852.html","43be2a3c72e34d6a767d628167373e0e"],["/posts/1674202625.html","3d46852e6cf9f341e3b9c458ce417792"],["/posts/1765123828.html","949179b516ac3f0c03d8ea47f45e775d"],["/posts/1767336200.html","709935627b5518fce5bd2d037ec8cfa6"],["/posts/1776114197.html","fd340eff491ef857fb73dc6716816452"],["/posts/1817748743.html","f0d263acc857bb3c3b741b39ea2ce426"],["/posts/1925125395.html","d979fb2383b7e60ceadd25e65852272a"],["/posts/1966191251.html","16f92e9a0082451970067bfe19bd5a6a"],["/posts/1987617322.html","de1593377a67a704bc5b62772b2ef01a"],["/posts/1999788039.html","8e397ca30ae29a2895c267362ad8d7a2"],["/posts/2075104059.html","c15093c7970ecb6bb9c3d1521e4e7b3b"],["/posts/2087796737.html","8613eab2069627da92cc56c0f1a204ca"],["/posts/2106547339.html","28459cf2bf462b630cc7ad94e3ec2d9f"],["/posts/2207806286.html","67931cff8b9d8a912d46dc82c840dda3"],["/posts/2225903441.html","fb81950f0cc3449210afc4af6824dfcb"],["/posts/2265610284.html","53440fbbdabc76095ca627669455222a"],["/posts/2281352001.html","9854e2a533f7ee2aa6da93f74f2de65a"],["/posts/2364755265.html","1cf3914d30b4f4fa380dcab4ba36192d"],["/posts/2414116852.html","8648b661c3c37e20933557f07b3fc891"],["/posts/2421785022.html","f4813905a38bb01d61a7c8cf8ac4617b"],["/posts/2482902029.html","799dc6b5accadd5bc21236d9cf7d1181"],["/posts/2495386210.html","7f511154c3615590dd31190cce786119"],["/posts/2516528882.html","9b08621b4f554127bde1b1f0e6998527"],["/posts/2526659543.html","cd9906021045279c5db01d01faec1b95"],["/posts/2529807823.html","2d7edc1eb1eb8791e64b5ef90faadd95"],["/posts/2596601004.html","204813a8f5abc7c56eb8fa6a14e509a0"],["/posts/2697614349.html","4333ca7b43b0fa3600612a1293ebf327"],["/posts/2742438348.html","d7ace91ba1cc3d4c73602c7a0a33a478"],["/posts/2768249503.html","744723be0fd71dcf678169ce0b7207ce"],["/posts/2864584994.html","5fb73a1e60f029660ac3a94773d8d018"],["/posts/2888309600.html","936e780724bf1a73e333f0fa81f3d83d"],["/posts/2891591958.html","64e69e2f3fb43578978a2edc26e03ad9"],["/posts/2909934084.html","e4a21f374ed4bab3ad0fe5a3c368d491"],["/posts/2920256992.html","192390862d401b52db59db68fc2ffb77"],["/posts/2959474469.html","92106ae9b9f3308be82e85de8945a523"],["/posts/3005926051.html","d1991af9607fc465c8ecd21f66fb240e"],["/posts/309775400.html","1f916746d78b8ac2affe54d6d2728b14"],["/posts/3156194925.html","30c325aaa36aa6ad8a3f500f3d90419a"],["/posts/3169224211.html","9cc8c8d3f2bf65f617eba323bd4c98f1"],["/posts/3213899550.html","3bf0aef194e172b6c8233f72efe62c7a"],["/posts/3259212833.html","bec241ee6f67a4cac8ea4a296e0e2b2e"],["/posts/3266130344.html","c3d9521c865bc181623ce642ee7da115"],["/posts/3292663995.html","b152aba56500ccb727d0f4ccea44c4ea"],["/posts/3297135020.html","70cd032d1635da395a5ccb2f4e1cce16"],["/posts/3306641566.html","3307be73fd30f720b3b8f56b649d0487"],["/posts/3312011324.html","643cf6b29a8b49e1edde5874e5cf67c4"],["/posts/336911618.html","8e7c126aec1614083a711dc6d7f53612"],["/posts/3402121571.html","22964a454f91b4d0f10afc04c7df010e"],["/posts/3405577485.html","31757ffa55d82bdb204a338c4b4ebb9b"],["/posts/3498516849.html","2ce7c8291176b6b0f8828ea805788d0e"],["/posts/3513711414.html","e93ae92a3f42ffb04d241fa14c6b99fe"],["/posts/3523095624.html","684c72b56dcd794ed7fb21fd1aa6745f"],["/posts/3546711884.html","254e8bda92d8eb10212c7b7b4cf1b806"],["/posts/3731385230.html","7ab2ca685c1643265c3730f713830a00"],["/posts/3772089482.html","4034666662a6ea6d6956d2cfb3d75d63"],["/posts/386609427.html","7f56da44db1574bfeef3593643abe0e8"],["/posts/4044235327.html","fcc8ed162b361db0364ba7b79f3acd17"],["/posts/4115971639.html","e71058ea90ecea3eb906134b9f2c1b5c"],["/posts/4130790367.html","94f2f13f8e5e26d0c9a5c2a9ee053d7d"],["/posts/4131986683.html","4b17aedec1e816a56ffd79b1c18ea93b"],["/posts/4177218757.html","bf2cfedba67de909f64f33818511a7d5"],["/posts/4192183953.html","48af612a400bd0b93ae74370e49a6770"],["/posts/4261103898.html","091b912dc035327ac393f6637404bd8e"],["/posts/469711973.html","01c82725f3268f9e1c38a7141d64d063"],["/posts/482495853.html","7f47fd16dadf743af69cceedb14fc4f9"],["/posts/488247922.html","ddd763d99b7fdd9df26b81cb65d1d04c"],["/posts/517302816.html","1c993b58a5d3cf4170f29c4cbb3acf1f"],["/posts/570165348.html","d3c55bc3f1a5dddc92140783843b6d44"],["/posts/595890772.html","de585774b7318e2b55f644e32700fc86"],["/posts/67485572.html","ce45f29f3673101fad15cf5e1b696125"],["/posts/694347442.html","4962b243d2eefe880846de53d16828ff"],["/posts/707384687.html","976b38e9eec2611177c3f2abc21e29b1"],["/posts/71180092.html","15c4ca39fbd731af421d7ca1ca0c3d9c"],["/posts/716459272.html","290c3372676ea808b2e711f81f974ff1"],["/posts/765481613.html","614deb5a8b0e5d04a1e63d0c5017b7f1"],["/posts/778231993.html","c9757a3cefdea9bfa2015785e822e63a"],["/posts/795397410.html","447c619b90a556e563c784ea24ae4f57"],["/posts/820223701.html","c2d7e80182988a4bd9a63627e6ef7510"],["/posts/830372185.html","1c1924fba7e3a678e81302ae79d07bf1"],["/posts/88294277.html","01f8de4ec872ebe1d45e175887c4b11d"],["/posts/939963535.html","9416960a9e5c1bc606ace02309872007"],["/posts/983786067.html","353932bae7ebea562981595306872146"],["/sw-register.js","373e322fa04f7faa7acb718c7ae6fb93"],["/tags/C/index.html","bfe385d5fd5f44bb65b2e0e879b8108e"],["/tags/C/page/2/index.html","d8b94c00c2f1d8ec3d9584bf13694813"],["/tags/C/page/3/index.html","3c875a8f3ebe60e9627c70a1037e44b5"],["/tags/C/page/4/index.html","c02c9851d2bab8651e9b8df360dcabc6"],["/tags/ETL/index.html","13acc0c8d3c5a53c3fd3ca88059db8b4"],["/tags/ElasticSearch/index.html","4007ff88601e39df74bc264f965909b4"],["/tags/GUI/index.html","0ad250e96194cf12e606e58bd2e9ea17"],["/tags/HBase/index.html","53f77a1da25c087df79bb1e225352542"],["/tags/Hadoop/index.html","ac5cec5a1c705314a45595b419607b05"],["/tags/Hadoop/page/2/index.html","d5be3c230a1e74810ef4bebec0e6c8b4"],["/tags/Java/index.html","7be2c76dcf285fadd0f71392e1c170d2"],["/tags/Java后端/index.html","2edafd21eee523595927e2c6ae5c2bb4"],["/tags/Java后端/page/2/index.html","e7aea2da5b6dd39319f43181ddd3808a"],["/tags/Java基础/index.html","bebbe6e23ab98ad21522bafc1a3a1b92"],["/tags/Java基础/page/2/index.html","ae6e98bd61f07df83b68eba3cb263a4c"],["/tags/Kettle/index.html","043f9c12ca88e8dd296556343123ec61"],["/tags/Kibana/index.html","20d824c287f1d9537cbebacf8e2b03fa"],["/tags/Linux/index.html","7fb3cd1f56471efbfaca2a68d0292d59"],["/tags/Linux/page/2/index.html","ff71b1f0ba59548dd52c5fffb7e464bf"],["/tags/Linux/page/3/index.html","f85ddb172446172dae182d351fbb9439"],["/tags/Mac/index.html","3d34f68d91d293acf0d82976a6b5fec2"],["/tags/Mac/page/2/index.html","300e3cefd6c6358f91cedac271aba0d5"],["/tags/Maven/index.html","dd8e645806cc94d2f2fe6c48c5e95a68"],["/tags/MySQL/index.html","dae15d0271057178912640619c006a06"],["/tags/Python/index.html","0022dfd9834dc8d15d06e59b757d8e91"],["/tags/Redis/index.html","70b29ace0901654703b7befe65b48523"],["/tags/R语言/index.html","e90f262eb5400c18b559b2d4c382a9b9"],["/tags/Spark/index.html","90fdef0e65528b9be86942e9a83416fa"],["/tags/Ubuntu/index.html","0b00243945315a70522eab5bb04d1d33"],["/tags/Vue/index.html","47955c9900279297afe6e6c56d6fae6e"],["/tags/Windows/index.html","90a8a358079da5c69684cab813713ce8"],["/tags/ZooKeeper/index.html","dda4a88581a478cb82ec3a19e53726bc"],["/tags/bfs/index.html","82c3c9a2eacaead33b2b3b89f7877616"],["/tags/dfs/index.html","8b458d5c5f809f8cfd37d241fcfaa557"],["/tags/folium/index.html","85c452f2a80c6cafed134bde2174a12e"],["/tags/git/index.html","004c7d5dc13330a904c1b0fda058567f"],["/tags/index.html","babafecaef60156710d3e32ffa053bbf"],["/tags/latex/index.html","6941534af8b119045fcd8106139f9689"],["/tags/中间件/index.html","fc205eb54badd10cb4fdc24faf051d99"],["/tags/二分查找/index.html","3c6a1109965af628e228b2f3bbbead3c"],["/tags/优化类/index.html","5e4dda74b66ccc009f11c15d8c9fb4b8"],["/tags/前端/index.html","e4eda77fa69015ab97220cc640042ec7"],["/tags/前缀和与差分/index.html","3157705ad654fcea823657b4ba9adf9f"],["/tags/动态规划/index.html","c017b59ca449ff6092a9a9e757f1aa03"],["/tags/动态规划/page/2/index.html","ff5b5021e18637bd38b42d23444d6fbf"],["/tags/博客搭建/index.html","fe876e27da9485bfa4e632ecb1a47a20"],["/tags/图论/index.html","d4951d21a301549b7fa723d693026139"],["/tags/大数据/index.html","43156d0f6f250ce378225b787dd5b84c"],["/tags/大数据/page/2/index.html","45ad1800189e7f32299d526879ba88b4"],["/tags/操作系统/index.html","5ace30a991446e0a0706fab00c29f5b0"],["/tags/数学建模/index.html","ff644fcb665069e0391f662442f86214"],["/tags/数据库/index.html","d7ae9ee814c7d3ce7d196c116680bdc6"],["/tags/数据结构和算法/index.html","3b64f5479eedc75e67e962ec3615f3a3"],["/tags/数据结构和算法/page/2/index.html","369bcebfb0e3aa642a4a1db0f47a76b7"],["/tags/数据结构和算法/page/3/index.html","bb34134c659df8669b177ab933b0c148"],["/tags/数据结构和算法/page/4/index.html","7211216af93c22e4851bc332839f1f43"],["/tags/数组和字符串/index.html","7fb2a5a6124b9fee02c0af67c205b917"],["/tags/数论/index.html","eb12ddf1e3c96394ecb74c009567daf7"],["/tags/枚举类/index.html","edc2fd52547fbc8ff3a1ce24ff463e3b"],["/tags/栈和队列/index.html","54c0afff9dc641a60b5f2907b4d89d3c"],["/tags/树论/index.html","3f760ad066949cf9c6c1e593ae7861f0"],["/tags/测试/index.html","f554e4f453b99fe4a11e10fb2eb2c52b"],["/tags/环境/index.html","a0bc7b163dfd05588093a8df66a4d9f7"],["/tags/环境变量/index.html","14544b165c897c9e14e6f65b93eb27ae"],["/tags/绘图/index.html","49fd5300982af353bc21c80a41fe7a30"],["/tags/编程工具/index.html","6a771dba108f4a689970c0d0cec0d289"],["/tags/编程环境/index.html","c1342b7ede9aa60f9f7abd5cd1dcae38"],["/tags/网络编程/index.html","23b3c620c1d948b3db0f8460665b45e3"],["/tags/英语语法/index.html","1c808f14e17db342e2550d94e9fea4f9"],["/tags/计算机操作系统/index.html","72a2be66039cb7732d15253b65590b2b"],["/tags/论文/index.html","c6c3f1a1f0a43586e8cf49ab7f2173f1"],["/tags/资源下载/index.html","c2dce6a2557e801e0fb87a31ed2e5290"],["/tags/链表/index.html","d925ff894e6443f662bedd8d1d10ec19"],["/tags/集合/index.html","7f43966d2c518f323af0a1965467b2ab"],["/tags/集群/index.html","98659bd2b3bc518fc5c5e5786330f221"]];
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
