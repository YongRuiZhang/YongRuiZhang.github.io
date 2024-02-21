/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","3694200fd1b27e626c430c9583ec01ec"],["/about/index.html","a31307244975ce6ad35573a30b231a78"],["/archives/2023/01/index.html","2563db0068562943b7a8731e06f7d13b"],["/archives/2023/02/index.html","7252b716bb07ff61578fc6c8f00ec307"],["/archives/2023/02/page/2/index.html","9087ffe3816f1ca3f65c4e524ff4c798"],["/archives/2023/03/index.html","09c4141a26d0323a89d8a2df2e9b5fdf"],["/archives/2023/05/index.html","c3158c3507fa05564e8e9a935064218d"],["/archives/2023/06/index.html","5a6170c8431ed416648e22b4cdba75e8"],["/archives/2023/09/index.html","1d0ea61a81689b915b752291b672659c"],["/archives/2023/11/index.html","fd6795403455bb26b9a3cd32f63709b3"],["/archives/2023/12/index.html","301d131ef948ba18cecc85ae737172e6"],["/archives/2023/index.html","4127a20ef90ec133506618a04396a3e8"],["/archives/2023/page/2/index.html","45c70d80c3bd6137e1fd9e66915243e5"],["/archives/2023/page/3/index.html","01b108eeb840c55bf8d3b34778f38c41"],["/archives/2023/page/4/index.html","83fc2f55ff1606819d7960f335f21d69"],["/archives/2024/02/index.html","fc1eb626b2703035f99871d17d3ebffd"],["/archives/2024/index.html","2a19dbac59e0c2b33412e571012d24d1"],["/archives/index.html","8fa63a201f6961d73596ec34210c4c73"],["/archives/page/2/index.html","c00683a6af587975eb2faf215c362cc1"],["/archives/page/3/index.html","a69b9da58724aa0902b656a98e1b103a"],["/archives/page/4/index.html","3e846fce9a3c55dabcb593fefb75c952"],["/baidu_verify_codeva-qQP2iZOMLX.html","fcde0fc6fd60c0edea5ec367dd3cddb8"],["/categories/Java/index.html","d8dbeb887b4ccc6f7dda5326f57b9967"],["/categories/Java/后端/index.html","2b2d8bdc189040314ffd4a8820aa0a18"],["/categories/Java/基础/index.html","3632945dccb9354a9ea9a34c461912ca"],["/categories/Java/基础/集合/index.html","57dd2e6593a6b78cf4a31bc367889449"],["/categories/Python/index.html","989a2e11545c5f24bada2ed50e740cc0"],["/categories/Python/编程环境/index.html","d35f5bc8c747ec2fe2b4cc51741c9bce"],["/categories/R语言/index.html","80db4a92a76d6e9d623a9e603495b0ea"],["/categories/R语言/编程环境/index.html","7eb0f83b6e2457b37d31943ec5807754"],["/categories/index.html","bc03df87ded713b29ae80bd8fef4822a"],["/categories/中间件/index.html","8f9eb209ca561bad1c5cd6f6cd061338"],["/categories/前端/Vue/index.html","8779e3a68127810e6ba30aff48522a81"],["/categories/前端/index.html","05bb89228dc831a053558cec51fafc3e"],["/categories/大数据开发/ElasticSearch/index.html","cf34ff2bab80f717d5dff299c484229e"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7897cc2f65ae24ea9ec732d2a6fbfb29"],["/categories/大数据开发/HBase/index.html","668151a5bc6b1e86e3a8c6f5285f44bd"],["/categories/大数据开发/HBase/学习笔记/index.html","fbef3e7c8ea903c4d497692e9ea8ae9a"],["/categories/大数据开发/HBase/环境搭建/index.html","b1f10a19052982669ee00a81803de00d"],["/categories/大数据开发/Hadoop/index.html","8cee9d227623b4336d34cda8f103bca2"],["/categories/大数据开发/Hadoop/技术/index.html","f98c3999ba10fe66a6bd929f69fe97c0"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d80fd925237e39f953b694f7fa368b99"],["/categories/大数据开发/Redis/index.html","946d7c8dfd66e5c560556979aae7e5de"],["/categories/大数据开发/Redis/技术/index.html","24014da226f7060fcc1ffe1e32dc4631"],["/categories/大数据开发/Redis/环境搭建/index.html","375182e1074139cc6f258ceee9b25a83"],["/categories/大数据开发/Spark/index.html","dff27b5488afb66748631dad2f7045c2"],["/categories/大数据开发/Spark/环境搭建/index.html","0c6b9ee93dc8d41a898f17155ff1211d"],["/categories/大数据开发/Zookeeper/index.html","c99a6954558b153221c64077044cdc7c"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","421900cf584dd0ea0896d3a85d8011b4"],["/categories/大数据开发/index.html","0a772d14db4eb064d27eca920860e4cb"],["/categories/学校课程/index.html","9c473f5b2ea2db091092d21990e8ed33"],["/categories/学校课程/计算机操作系统/index.html","48aff2aeacd60476cddaa1117f314b1e"],["/categories/操作系统/Linux/index.html","c4ee17844d0ccf99a4bd83e350bc56e9"],["/categories/操作系统/Mac/index.html","06181099f27d64d8735f197dcf28f151"],["/categories/操作系统/Windows/index.html","b300bbd30a3851df163c0e1d8588688c"],["/categories/操作系统/index.html","96b08b2106868248a372449cdd4ffd84"],["/categories/数学建模/index.html","812d8606b8e507080e74b54a32dac8b1"],["/categories/数学建模/latex/index.html","6cefaf9fe51682ee80b8e63dd7ade19c"],["/categories/数学建模/优化类/index.html","6b530757057881d0051fce81a2868bc7"],["/categories/数学建模/优化类/现代优化算法/index.html","4c854bed8cc818ecdad49764596324eb"],["/categories/数学建模/优化类/规划类/index.html","b7b178c68bdf99077e190f6f0acf7f28"],["/categories/数学建模/绘图/index.html","7188c5b8a2ef51a1279d0979a64a3700"],["/categories/数据库/MySQL/index.html","36fe790c282c24f7258e321ce8ebb074"],["/categories/数据库/index.html","e2554030a0be78573c188d240d9d7d0c"],["/categories/数据结构和算法/index.html","7e1ac21acce847b05faab718b048472c"],["/categories/数据结构和算法/page/2/index.html","51e49e3713ac374179aa4b3b92e13f6e"],["/categories/数据结构和算法/基本原理/bfs/index.html","a6da21ca2174d8eb4270360b1158267d"],["/categories/数据结构和算法/基本原理/dfs/index.html","fc8f9d025f6770e46a7cefc4f8ceceda"],["/categories/数据结构和算法/基本原理/index.html","f877a5b8179eed9499540c140e1afb76"],["/categories/数据结构和算法/基本原理/动态规划/index.html","7958045aa1e2533629f92fa2f1cb7672"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","9d6eff6c9ba256f6a70a86af17586696"],["/categories/数据结构和算法/基本原理/图论/index.html","0d2e4339bad14891eb7112ff2f39ae84"],["/categories/数据结构和算法/基本原理/字符串/index.html","4fa2361dee9cf78f335c1af05bd2f1a2"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a88bc2060157900659d2145d62ec25f9"],["/categories/数据结构和算法/基本原理/数论/index.html","b378d79bfe3ad726299ff1c5c8f0d5ee"],["/categories/数据结构和算法/基本原理/树论/index.html","d27aa3594617f534a551ee05c214b156"],["/categories/数据结构和算法/基本原理/链表/index.html","7ac3621508beaf221df32352376038e9"],["/categories/数据结构和算法/算法题/index.html","a03adcd3db1fe62829acc4255d3d8cfc"],["/categories/数据结构和算法/算法题/二分查找/index.html","d0bf2a842fdee3e6d30f56aacb5b578e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5ff19a5d4f3aac24b6e28d95cc2c5271"],["/categories/数据结构和算法/算法题/动态规划/index.html","700a53a13dcd966c6fe65859f78f3ee6"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","956d1acdff07acb17c19520d3d121290"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2f9143d973bdc0ad7b7a8403c4ffa5e1"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","53228190b345c624d800e9214de075ad"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","0557dad22b9b77ea40228152004a465b"],["/categories/数据结构和算法/算法题/数论/index.html","cdfba42c71fd66a31ba22af613ebd525"],["/categories/数据结构和算法/算法题/栈和队列/index.html","49cc8409b182d707fb96e15b45c5ede6"],["/categories/数据结构和算法/算法题/树论/index.html","721de5ef2a04837c4ed679ba45933ea5"],["/categories/杂七杂八/index.html","4be8dac43f41a1138bb41ffebebfbaed"],["/categories/杂七杂八/博客搭建/index.html","14fe20ce39c02b5e77c656ddc0913b00"],["/categories/编程工具下载/index.html","73f907959f0cbd424193e8a0970e5ce7"],["/categories/编程环境/index.html","54fd4064f92a501023dbaed17a85c100"],["/categories/编程环境/大数据/index.html","e6d3d5498464531c8fae40322f7132a9"],["/categories/英语学习/index.html","35f1164357b85881c2f9eb357198632d"],["/categories/英语学习/英语语法/index.html","44ff747a30bb0eb20c4b5c5beb9502e6"],["/comments/index.html","a526a6a5afbdd262cb105096813cfd27"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b18302994954eb259f955208aef205c9"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","048079860dc543f8fa054da17fc08adf"],["/movies/index.html","f6d839e6f811b2d203cad1931f075c41"],["/music/index.html","c40b2e62a74e92fa62e135a0e9a835ee"],["/page/2/index.html","fd7a020389591fa33a8a23ffba8d7279"],["/page/3/index.html","09bda8e428074367de579ec1281a3d0d"],["/page/4/index.html","9404d86336bb02f164379c5ee84edcdf"],["/page/5/index.html","3d4cfe23a189e2d2d2687860829c499a"],["/page/6/index.html","7461197f4bd2a21161158973d1aebe7b"],["/posts/1021360842.html","5e2fe079084b29dd3169034791ea0923"],["/posts/1120620192.html","6c029148bbdd167bed7985e82dba8b7a"],["/posts/1141628095.html","a7e44653394c2073870eb05d718e7681"],["/posts/1168613674.html","2554c8f52845e0aa5fdf0dd95879a752"],["/posts/1219920510.html","e17fbf13007887cf82682477960e56a0"],["/posts/1222166338.html","600da41ea11429c361235757f0d06873"],["/posts/1259097482.html","40604809b7071b0eb7a05edf81dbd978"],["/posts/1271036369.html","9d82c1d4ef3a9e958d5dc55cb3ec11cd"],["/posts/1312847445.html","bccbd57e222f742fad48f7081d4d1673"],["/posts/135355774.html","6457820f51d3e761efeea116241c0d67"],["/posts/1375344716.html","80121e53426ac1ca6eb8d9ac225fec97"],["/posts/1388991698.html","fdd66b2cc218cab78ad626ffc4845c6c"],["/posts/1410315814.html","1e38706696f217045654ee4f99061107"],["/posts/1452790229.html","f23bac3a69e4302c3b3b6b834f7e1c20"],["/posts/1470079884.html","e42f44d58f3d6a391b077bf493cad7b0"],["/posts/1470079885.html","6ba620a41a04db75454cd01ca169a53d"],["/posts/1470079886.html","bbaf15356d20b07348c1f7cdde867421"],["/posts/1470079887.html","7aa83ead624a379f5f835cc326dbc029"],["/posts/1498536549.html","53360a5667dfa889f821967910a41a7f"],["/posts/1539568593.html","5c1227c0baacb52b98582e7649e6426d"],["/posts/1547067935.html","360887b2d8ca1dfe2a42e40b0fc0794e"],["/posts/1557866301.html","d5fd5959354cbe3fe1fa04f46fd5c90e"],["/posts/1571776361.html","5b5e39e28f634b6982a208bc65a3b39a"],["/posts/1605124548.html","434d8eefb068a5f08c19101be6392723"],["/posts/1633036852.html","13fc8ffe641559dc4a2ea35b2939e995"],["/posts/1674202625.html","b9c2dbfdabb1960a5a6356acc06cfbfa"],["/posts/1765123828.html","1d6377bad523d780da61043223efcedf"],["/posts/1767336200.html","28fa0d303183c4ba55da204ce1de71bd"],["/posts/1776114197.html","93fe28088f23ffdebd51b16df2a05939"],["/posts/1817748743.html","21b102af714af20bc1e2fdffa674d1df"],["/posts/1925125395.html","9ef0211b8f5be3260a1f5e8abca033b5"],["/posts/1966191251.html","2e31e6a7df15fa1680d7451a8a821342"],["/posts/1987617322.html","b8da41ce5981b0a93686beb346d9825a"],["/posts/1999788039.html","ae03f0b2d6dc3532a4ca805acc760916"],["/posts/2075104059.html","a29e88bf9a552bf95b24e24076e48a81"],["/posts/2087796737.html","03260f53973c1f126191e02578cf899a"],["/posts/2106547339.html","69d2b6cc6d8cc768591cb9a5f1e11874"],["/posts/2207806286.html","754779c8ced545f834b6c71b012a7c16"],["/posts/2225903441.html","69c9524a0f8dfb78ec64625b800347b1"],["/posts/2265610284.html","8d909bdc1a3f3f0a412e535105771cbe"],["/posts/2281352001.html","2632d2e43290725ed49f36a24dcbd4dd"],["/posts/2364755265.html","b56872d339f7ef9ddb9a040e062b4f38"],["/posts/2414116852.html","2a309e7bd53014664cac020c5de3d8fd"],["/posts/2421785022.html","3cd506411e1ad66d0ef98d89e0793e63"],["/posts/2482902029.html","321a0e4489c438ee391bada693fc9c94"],["/posts/2495386210.html","0b3b63dd5428edac93bba59f0c7c4c13"],["/posts/2516528882.html","7d821108f2250daf2ee63804dd131a1b"],["/posts/2526659543.html","960de7b142e96bc6a6116400292e0d50"],["/posts/2529807823.html","cff204cea60447793e28e69ef8341cce"],["/posts/2596601004.html","549aeaaacfc59229c68fd6b24c07f178"],["/posts/2742438348.html","73ca5d6eaa85f70b579d61a8984fa728"],["/posts/2864584994.html","f67bf1b5272009d31baa216b75f3cb49"],["/posts/2888309600.html","fc539c475945dd944ac16582c70284a2"],["/posts/2891591958.html","c072719521b630549e2f6e3cc9d4c802"],["/posts/2909934084.html","833dd69015ad45ab256f6bc646583f1e"],["/posts/2920256992.html","8db7f7cc909e90245a5dd3ae15adef7d"],["/posts/2959474469.html","43ddd92fd91de2deacca62ee385515bc"],["/posts/3005926051.html","15e430a3ec961fb083f452cd3174662f"],["/posts/309775400.html","0a19bbf378056e9379a34d8273f6b7f2"],["/posts/3156194925.html","d443de3d6e1439ca3763ed9fb0e23dde"],["/posts/3169224211.html","893351f611d94919631c3831b49ba45b"],["/posts/3213899550.html","b8ba3447536382157d71f3148c8155d5"],["/posts/3259212833.html","86c867b6cf9d91d78868226f3c3f07f8"],["/posts/3266130344.html","9412eacce45d3c4cdc986baae824f80e"],["/posts/3292663995.html","b3b2bfa635188e797a6549a6581bc66a"],["/posts/3297135020.html","bcaaac2b110b12e33d9eaff27dc0579f"],["/posts/3306641566.html","db50295efdc9aa100d7b175b1de9e50a"],["/posts/3312011324.html","f367c559a0ca8e8f04d0880d97c55ee0"],["/posts/336911618.html","c184c040a354bbdde42958e25c49bbde"],["/posts/3402121571.html","dc691f911336b2839a8741a23115d0de"],["/posts/3405577485.html","5086e1026f555309824c4fc8574d70fa"],["/posts/3498516849.html","29d278672ad43b76b3969123b1b1bf92"],["/posts/3513711414.html","d2c8dff527959b54ede1ac3be9c963d6"],["/posts/3523095624.html","af828d7bb559c6759b0b2afad7e64f78"],["/posts/3546711884.html","37ec7718646dc21230c7dda0808ae349"],["/posts/3731385230.html","b611dc25765eade11a8ae0fe7f7b5997"],["/posts/3772089482.html","0b9264727078e154cfee843b54097454"],["/posts/386609427.html","5b48e2c58e88738b23bc7b1787c10595"],["/posts/4044235327.html","6aca22155e49053fa727ac42b355bab7"],["/posts/4115971639.html","79c8ebe087e08ca7ef0e1d7d9d3294b9"],["/posts/4130790367.html","bb9e88fee2cc05d7b14d382262da0658"],["/posts/4131986683.html","9d5d7629189e67728a5d808cb8a92501"],["/posts/4177218757.html","f7aa6edd872a24a52ea6ee3b30ec0466"],["/posts/4192183953.html","10ce37f358ecbede477612b7b3e1d834"],["/posts/4261103898.html","2f0d7798f3cf58d0ec63bad8df58f6a7"],["/posts/469711973.html","2a1dba493cbc331004d87d09cf8b0869"],["/posts/482495853.html","f278f0f4277f7db3203083e269b44f2f"],["/posts/488247922.html","5afeb2e4227e5ed340bf221e447b5486"],["/posts/517302816.html","8eaacd7d3c1888e47d5298a31a07f348"],["/posts/570165348.html","84e5dfb41668c23e89cd7c229add07a7"],["/posts/595890772.html","bc1bf3e999051505d620f1cea32404c1"],["/posts/67485572.html","6d77fcbf4759d6ef4da7826574f2ecdd"],["/posts/694347442.html","d07f8a2f8af016e5ec8d73b79a321316"],["/posts/707384687.html","dbfde34c95ca43f2e15f81151de34f48"],["/posts/71180092.html","1f176ddb50c1d5514fe39c5b5b8924fb"],["/posts/716459272.html","53887623cfcfc722eae4cf88d6b633fe"],["/posts/765481613.html","ff8a21f20b5ef569581940cc188626b6"],["/posts/778231993.html","1539c21c9ad2dcd065cae776cda9fa51"],["/posts/795397410.html","71b607edb50ccb8545de1d02354c1bf5"],["/posts/820223701.html","c9bd96471e98871959d4949b17d43232"],["/posts/830372185.html","b36a6b046448b863620141ed9303fd54"],["/posts/88294277.html","ecb68f4a8c590234affcbb96f2a09fc5"],["/posts/939963535.html","579ea87b91a1dc74950053e049c734bd"],["/posts/983786067.html","3b607c194faddf2cd9f8078e1a622efd"],["/sw-register.js","6c75067208f5468c1345280e1566013d"],["/tags/C/index.html","8cc3c0ecef807a73fbeba146bf87b746"],["/tags/C/page/2/index.html","230505a8a38527fa704dedf3603d58ea"],["/tags/C/page/3/index.html","b87019bd5717e293655e19d6359f37a1"],["/tags/ETL/index.html","3541127a29af9ea1ce4994794ac47237"],["/tags/ElasticSearch/index.html","6934ca50b5643459c9bf017e949a530d"],["/tags/GUI/index.html","07e1aa396109845ff2e80808fb519290"],["/tags/HBase/index.html","5c922ab2fdf456e2a00726cdc9b48121"],["/tags/Hadoop/index.html","11a77252d01a5f55b9a98a15e62b9fc0"],["/tags/Hadoop/page/2/index.html","629ee7eadb5b1da493697cdc6ebf5c9f"],["/tags/Java/index.html","576968f7fe27495d23206408b2f5ff2b"],["/tags/Java后端/index.html","cb087c4e4a4440fe37a480358d766143"],["/tags/Java后端/page/2/index.html","a04eff70c5d6ab658ea09d79d47b8ec2"],["/tags/Java基础/index.html","f84f8ac5c236619302bd95dd06fe6815"],["/tags/Java基础/page/2/index.html","2d2e07aa6da936e3023eaf1c43483f15"],["/tags/Kettle/index.html","32777f182cd04ddfbb9b51ea01f5f790"],["/tags/Kibana/index.html","eefe7e781f397c3ab23d09975ae1da35"],["/tags/Linux/index.html","bc4072885bce68f0daa6c84980fd0404"],["/tags/Linux/page/2/index.html","508b3a1fd3c571bbc29369f0e28c82e3"],["/tags/Linux/page/3/index.html","bddab178044d98abba2b7b76e20e4cbf"],["/tags/Mac/index.html","8214f447d50f45def3d6cd4f54c67508"],["/tags/Mac/page/2/index.html","64aff29e5bd7110f4e2c2988a7e54d01"],["/tags/Maven/index.html","dc7489c37519e254286cf30de1a97004"],["/tags/MySQL/index.html","dda55516ae9327f86d6b3096d4402fd4"],["/tags/Python/index.html","b3beb6132c1ab40071e6cc74c64e21e3"],["/tags/Redis/index.html","19686006db59e4971ea446e0cc848385"],["/tags/R语言/index.html","13fefdb249d9685d1e8a33cc7c4962fc"],["/tags/Spark/index.html","291bf239ab53ba794376fe3419b6a33e"],["/tags/Ubuntu/index.html","30e9fad2c2ce900616e9f5a222dc512a"],["/tags/Vue/index.html","0a6770bdd8c5fa0aa03f5398bf9b0a63"],["/tags/Windows/index.html","4a5544bd1766c592bd185447304cb5b7"],["/tags/ZooKeeper/index.html","090d2f66e240b9c2efaff0a2778fb01c"],["/tags/bfs/index.html","2f8c65c6585224219c15191416da5810"],["/tags/dfs/index.html","ba3bde5a6a161630da18121e670b46e3"],["/tags/folium/index.html","cc2e63dbbe0d56b7adb2a013de81f3f2"],["/tags/git/index.html","5d755f0932e6655b53705af1a340b080"],["/tags/index.html","6f148267e851f1e707766507e467af3d"],["/tags/latex/index.html","7c5253690f93e685a6a3aa0700751ba1"],["/tags/中间件/index.html","c57f9f979c252e59411fb1d34aa189db"],["/tags/二分查找/index.html","4833468b91a15757aa38b0f22df20af1"],["/tags/优化类/index.html","42c23df4a9710e5d4f21d9d54e21a6a2"],["/tags/前端/index.html","da4c0f11edef5d4153ad5c65a1eae87a"],["/tags/前缀和与差分/index.html","207bf8ba46be30ae457b400d3b0c102a"],["/tags/动态规划/index.html","1b3ac380d31e78a6da0c0b10a59fb177"],["/tags/动态规划/page/2/index.html","d5811d8ae3d2c9c19068495191ff8725"],["/tags/博客搭建/index.html","0a4742c295df4cfcd36462d5c94a2cd1"],["/tags/图论/index.html","9d2b316ca43b5efbcd1f915c34ebce1d"],["/tags/大数据/index.html","2d2568fcdf230d3616364ff014f18506"],["/tags/大数据/page/2/index.html","e609d02c962a78f8c06aa7c8c501d775"],["/tags/操作系统/index.html","bf40d58ccb882bb840864995d65b1615"],["/tags/数学建模/index.html","59f6aa4fab2a49c12679be5810d62b41"],["/tags/数据库/index.html","b4686892dd66c9a844f95bcf14bf395e"],["/tags/数据结构和算法/index.html","286ba0ae59077214f76675da0cc84abe"],["/tags/数据结构和算法/page/2/index.html","171ee225e53d51b769e1ce09a01b2a9f"],["/tags/数据结构和算法/page/3/index.html","10ee55b596d0c94bdf33d1090ac71d82"],["/tags/数据结构和算法/page/4/index.html","c3324f5f98da77f7bbdc9f9fdfcdccde"],["/tags/数组和字符串/index.html","f65cda8ae778093cd94acd22040c3daf"],["/tags/数论/index.html","bce1eeafe5cc921a5466040d7a7f26b7"],["/tags/枚举类/index.html","a98a1bb0c5440cc345a9c53ee5778814"],["/tags/栈和队列/index.html","5477e726d50a89ec51727458036dbfeb"],["/tags/树论/index.html","8d60aaab358d78ed152ef8f64bf3878e"],["/tags/测试/index.html","8bcd66231483f8aa6f0bef062eaac2a9"],["/tags/环境/index.html","9d39465ac8c4c8c9fccc67d2a329ee8f"],["/tags/环境变量/index.html","203e58736f9bc5c0fd9711b1960ee136"],["/tags/绘图/index.html","d1446838ca541f474133016ebfeef9a6"],["/tags/编程工具/index.html","2e41f063bbe3105977175a9c4178cbaf"],["/tags/编程环境/index.html","8583991e6a590d9a33f643f100a4eb09"],["/tags/网络编程/index.html","19c7b768ca2b84a1b8d490b389e53aed"],["/tags/英语语法/index.html","468f733faae418c54eef822b90c68115"],["/tags/计算机操作系统/index.html","20d429e6ae9a3e2623ef02fc47e9b06d"],["/tags/论文/index.html","dead7a1ac2ca6b74d331b90c40289d86"],["/tags/资源下载/index.html","d62d883a32364c7e5247d561fccf1ee3"],["/tags/链表/index.html","0959be233bfb9a0d18d226a954701e10"],["/tags/集合/index.html","d2609f596f7e9323fc3cd0d105ad65e0"],["/tags/集群/index.html","db407e0cb80913cb6024a07096e0feb5"]];
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
