/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","59e390f054bbb75892f9b4a38864b9ad"],["/about/index.html","632924077c9b4b1172d12398c8725b8c"],["/archives/2023/01/index.html","34b2356995351d57794de79572f50be5"],["/archives/2023/02/index.html","65369707eee60bce10435d33df2fc306"],["/archives/2023/02/page/2/index.html","efe18babe99291ef0c792db674494736"],["/archives/2023/03/index.html","0400ff2740a2f2859bb0fe140a0e3df5"],["/archives/2023/05/index.html","d33aca183d6558e5b3b595d0d02463bd"],["/archives/2023/06/index.html","02d7b397ed1196b4fa789f3c403243d9"],["/archives/2023/09/index.html","79be37438ba53ea25ce49aa37c673a37"],["/archives/2023/index.html","f737d611f254d2b224594ff23d8a807d"],["/archives/2023/page/2/index.html","7d3d766d78ce86cd4a448f37214abc44"],["/archives/2023/page/3/index.html","3c1f44f5fb539879cdf444273d68b76b"],["/archives/2023/page/4/index.html","9bdca7b06147e632945bf537a63c36ef"],["/archives/index.html","d703d6a35c8ea12398ee029464f0d29d"],["/archives/page/2/index.html","9761e3b05af9d81291ed9eed06628030"],["/archives/page/3/index.html","c4531bafb64be1f406f5d701bfb748c9"],["/archives/page/4/index.html","2a97d7da37716ef10c181b0ab656512b"],["/baidu_verify_codeva-qQP2iZOMLX.html","5ecc29e661501bf066838d345220f11b"],["/categories/Java/index.html","c0346d398ece5c90be6b400e10151000"],["/categories/Java/后端/index.html","0f4fc69107a04266e680a66e9a5c0f77"],["/categories/Java/基础/index.html","345732a946fd8317d16d80aaacdddfd3"],["/categories/Java/基础/集合/index.html","93bdd3cc3fa03fbf5c4faf4385dba89d"],["/categories/Python/index.html","96e9ae0f9512cc6bba3c4b74cee3e9f8"],["/categories/Python/编程环境/index.html","825a64ba5aae5557141731e0a29acabb"],["/categories/R语言/index.html","92bb5a532159c1f39c97d9cfc0854a0f"],["/categories/R语言/编程环境/index.html","5f7588f7152c8ef8446aa1670fa4594b"],["/categories/index.html","3c86de88879ea139438397c72c2cd956"],["/categories/中间件/index.html","ffcf2aad6fa76928708adb8069520d9f"],["/categories/前端/Vue/index.html","5d86b3e91ac1d82e1b6e6c358e81c6d1"],["/categories/前端/index.html","edc6606d54dcff94d3577fb431739ec6"],["/categories/大数据开发/ElasticSearch/index.html","33f4bffe85388313935ef059b445cb28"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","278caea34bf87ca59f44319ad555c5cf"],["/categories/大数据开发/HBase/index.html","0cc05a15f08f96f45d3da66bd23e22b3"],["/categories/大数据开发/HBase/学习笔记/index.html","f37388992eb3a66bb3b5c4a9d4f7e041"],["/categories/大数据开发/HBase/环境搭建/index.html","8ee659aa82abaedc7bf5ae58bdfda4a4"],["/categories/大数据开发/Hadoop/index.html","df735028145307cdd50641fd77c37efe"],["/categories/大数据开发/Hadoop/技术/index.html","72a25f5253f4e6276ee713ecf258c1a5"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3e93ac34d31abd85474e90ee0728aace"],["/categories/大数据开发/Redis/index.html","b9b2876a695b272697811fc9811de687"],["/categories/大数据开发/Redis/技术/index.html","0e7ed2734898082e72c011b0eac7715a"],["/categories/大数据开发/Redis/环境搭建/index.html","81bfb0c59708fb9b74a1bd562d891214"],["/categories/大数据开发/Spark/index.html","553324956d666e622124ee6dd1621edd"],["/categories/大数据开发/Spark/环境搭建/index.html","422b597831b9fe67ff6c6e18b49d9e78"],["/categories/大数据开发/Zookeeper/index.html","cc7fe554085403b66279fb8b191d937b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","ec082f1a7cd7177641785f314766f482"],["/categories/大数据开发/index.html","faf32d45957d266c099c28c6d2207e0f"],["/categories/操作系统/Linux/index.html","f4ab9eeabce0932ef2bdb7d688b4d8b4"],["/categories/操作系统/Mac/index.html","a8eb621b8e2923a4a128d3648288c3b4"],["/categories/操作系统/Windows/index.html","ff5bec3bc496689ed2d637c7ff455bfe"],["/categories/操作系统/index.html","dc5370957ee8fa714b8efc42fee72e06"],["/categories/数学建模/index.html","061102aac29a26138a51371db023b789"],["/categories/数学建模/latex/index.html","0b0f3b08d517d07d9b4ddee2079d7e4d"],["/categories/数学建模/优化类/index.html","4a2bcdaed5c22f7c2e708fa280004fd3"],["/categories/数学建模/优化类/现代优化算法/index.html","fd896bb4efac4849523ed2cfbd9bbf9f"],["/categories/数学建模/优化类/规划类/index.html","807370a931a20a4bfdf1b0ce7d5d8202"],["/categories/数学建模/绘图/index.html","c87c24a21f0ed0f1392b1b822226ce28"],["/categories/数据库/MySQL/index.html","c96c83b2e697779639f4f9fcb36f7c8a"],["/categories/数据库/index.html","ae48410551778fb0633bb3a1f602be89"],["/categories/数据结构和算法/index.html","7448a83fa8054bd44026d9423fff161f"],["/categories/数据结构和算法/page/2/index.html","10c7303e9abd7c4ab2976f1dbb2e0ca6"],["/categories/数据结构和算法/基本原理/bfs/index.html","ff35aeaeb82605594abdaf65842f6b75"],["/categories/数据结构和算法/基本原理/dfs/index.html","dc7a893395aa8fb51ed1b278b3df3980"],["/categories/数据结构和算法/基本原理/index.html","3438b04c4137072b00fa44d28db6feaf"],["/categories/数据结构和算法/基本原理/动态规划/index.html","0639cd47c8dfb292e5042fd60d473816"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","61500899e58ef49ef4560d25c661b841"],["/categories/数据结构和算法/基本原理/图论/index.html","0c9df847190498f05e3a917ca7d80048"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1accdb4845c91fb8edd931c581627ea5"],["/categories/数据结构和算法/基本原理/数论/index.html","66e025e644f4612d6b5744ad8aa015c8"],["/categories/数据结构和算法/基本原理/树论/index.html","980c832e675d529beca04a1fc8189852"],["/categories/数据结构和算法/基本原理/链表/index.html","12fb9b4ad542458f0afcd35ea5e78ce8"],["/categories/数据结构和算法/算法题/index.html","957990405db57bbcd2fd72dee8614986"],["/categories/数据结构和算法/算法题/二分查找/index.html","d30ee49e4e4dc9143d0501d876cf1927"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ca66e4f1e5917061560210f990dae790"],["/categories/数据结构和算法/算法题/动态规划/index.html","1e4338a7b8bccba35e62ebbc5f873228"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","7c9c581c9a8a7eee98911bb9e83dbd22"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","15eade8de3aefc7d269c1826c8f44dc4"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","41151be5a18490c149edc4553238f82b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","94e8a1567eb2efb8430fadf4ac712faf"],["/categories/数据结构和算法/算法题/栈和队列/index.html","9cf3f408fbff87f3816dfad017327b73"],["/categories/数据结构和算法/算法题/树论/index.html","af69f6aa402fd90826083aa75619f191"],["/categories/杂七杂八/index.html","28ba5e6a4c723c75ff894b8e240218c7"],["/categories/杂七杂八/博客搭建/index.html","98fb4641dcd5f916cbfa934067a95335"],["/categories/编程工具下载/index.html","79cb31752c9107cab8577655288f536c"],["/categories/编程环境/index.html","a14c5785de0c14e49381151d65c364c9"],["/categories/编程环境/大数据/index.html","048cb9c7356416b948492a8dcf0e01ac"],["/categories/英语学习/index.html","256c4407a43ed415f7a92b989079d4a0"],["/categories/英语学习/英语语法/index.html","6e18f6775d11749947c341f927bdf419"],["/comments/index.html","041ea4d792481c19b0777ffe324dfc86"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","509f65447d3c07417dbf0b10a33c2444"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","9ab12916652828335a71de7dea448564"],["/movies/index.html","9be694eb2ac8198f1fd05721600692f0"],["/music/index.html","30e0d0ba7f238975c9fad7a1ab2f9a0b"],["/page/2/index.html","fb33a66170f0145cacad1c7087e00ed5"],["/page/3/index.html","05e1ab00490ab534f689c11410a0979c"],["/page/4/index.html","0dae34d4c15288628c58698dafb7c7ec"],["/page/5/index.html","fc405f40632af23372fced98ec7941fe"],["/page/6/index.html","8433123a2d6add2d23ac91b5d2d95a64"],["/posts/1021360842.html","241d6bcf6cfb0f3c1f8327d143480a9e"],["/posts/1120620192.html","9b1aaeb7f6c9b937231b39ce352ca0ee"],["/posts/1141628095.html","6ca6e2cf66da2867bd64d8cbc8b17350"],["/posts/1168613674.html","a899e3704b1f28aa324cdb2e3a781841"],["/posts/1219920510.html","fde14f9c3dcb540ec8157f048fb77023"],["/posts/1222166338.html","6efd26fd09bfca3e89bdf1d363b5c6fd"],["/posts/1259097482.html","d332719b71a204412d36d3f7222545c2"],["/posts/1271036369.html","5990936bb834a895a974f4a4a4b449c8"],["/posts/1312847445.html","5331bd79d234e507fa1592ec6af022de"],["/posts/135355774.html","f515dbbdbea396ec99297c9c39eba489"],["/posts/1375344716.html","09267f499974cf9a7e30142dc7105549"],["/posts/1388991698.html","d694a5b514a1ead4c5abcefeac37d63f"],["/posts/1410315814.html","6721ea565bc89bb36bb9efffa3e62d73"],["/posts/1452790229.html","45c26bb264d21a4eeebcdb33c90c96c1"],["/posts/1470079884.html","c0c3530d64a9cf4bd0a7be0c5629bba2"],["/posts/1470079885.html","5a3213e85ad0d0cc91fd9790e1c6e361"],["/posts/1470079886.html","b1eb25e4627917d37ea474c93cd66e32"],["/posts/1470079887.html","7db929ec2133499f15dccdafa1b1a012"],["/posts/1498536549.html","4ed1f29de0809c793fbccf5d11cb25fa"],["/posts/1547067935.html","682d359e4749308f372ab9e37d9a92e8"],["/posts/1557866301.html","58a04b6f13aea982b01b6427ca9523d4"],["/posts/1571776361.html","1ecace31164e08e00e678d920bfeed55"],["/posts/1605124548.html","ac3346099d0b20d51e71573067c75035"],["/posts/1633036852.html","cfa56793ccd121ad660cd4b3d23fd332"],["/posts/1674202625.html","cd84aecc04bece621ecaf548e165d364"],["/posts/1765123828.html","c9b7bbea8c2a165720a628041c207f3b"],["/posts/1767336200.html","c515a106379420ebecb8c4bb9cc1b1fe"],["/posts/1776114197.html","c95a9f3432d49a3ff9ed4c687d4ca747"],["/posts/1817748743.html","6c026b981936b4314b39deacb21d4ec5"],["/posts/1925125395.html","5c53177605f172020fd6d225129ff959"],["/posts/1966191251.html","cad2cb992b1127fe25372a48da874deb"],["/posts/1987617322.html","68c97a11e78f04e75fd9bf561dd02f3f"],["/posts/1999788039.html","c88a9239d66a8053835ad58d3a1e3fef"],["/posts/2075104059.html","825f064c893c84e96a7b03a9a28f46ea"],["/posts/2087796737.html","20fa425f5977f7cf6cace4653271695e"],["/posts/2106547339.html","97b16b6a8808cacbbb63afc6c0989628"],["/posts/2207806286.html","3066c8e2624e33ba7d3870ea15075e75"],["/posts/2225903441.html","74a75b3f9302abf9bd4169d37027551b"],["/posts/2265610284.html","ddeae30b6b04c5135bdf0bb73ec71a61"],["/posts/2281352001.html","cfbcf114f012918d5f6dcff5e40fe4ed"],["/posts/2364755265.html","b672d0710e7e5df3e52dc305b2187580"],["/posts/2414116852.html","3a72cb40b797bb65274b2dab97d71bb2"],["/posts/2421785022.html","47403f5cb3389cb8a68fbc6fe0bc8090"],["/posts/2482902029.html","fa24bec853f6446f0948e83793dd4003"],["/posts/2495386210.html","aa28006c5510cff68911fcb0a68a7c3f"],["/posts/2516528882.html","4a73817bf567d0b7a5e9041ac2ff1e0d"],["/posts/2526659543.html","86309fa122819bb252904751ee74b9b3"],["/posts/2529807823.html","3dcbc423193c0aa5a3a3e00e88be343f"],["/posts/2596601004.html","a8263759b55a131d9dd05bd58fa2edf6"],["/posts/2742438348.html","6969a1fa2fbabc2149d2ef9f76204400"],["/posts/2888309600.html","6f14201f353ac2d5c89040325fcc15b6"],["/posts/2891591958.html","60f9461291c64896e6c88eacbd4eb87f"],["/posts/2909934084.html","d16cbef9a433223496db6d10c678f766"],["/posts/2920256992.html","9c29af62bdea1439de85c75d36b16362"],["/posts/3005926051.html","9c6dd980f9094b2155a512cbd5195800"],["/posts/309775400.html","ca869ed89ac36eba53040eac3beb1d3c"],["/posts/3156194925.html","6c7d3a847aaeec6d89d6e1403b38545d"],["/posts/3169224211.html","c5128d0ba8be4e053ae72d1523afb98b"],["/posts/3213899550.html","7def08f4161d9ac80dba76b522c615b5"],["/posts/3259212833.html","46a7360ebb4ce8a9e5520373a2f9ab90"],["/posts/3266130344.html","12b9c296183e9c41a1d233e9a5ec02c6"],["/posts/3292663995.html","ca7bb89096003d11338f43955e558053"],["/posts/3297135020.html","dde97b1b63bd05fd8125264ad51a20c4"],["/posts/3306641566.html","7fc52614268f8246518abaa921055b37"],["/posts/3312011324.html","15cbc9bf1b3b75de744cd7dd010262ac"],["/posts/336911618.html","0a17d85216b1b0c218feb9b7c5c06044"],["/posts/3402121571.html","69cdff19338702fb5788ee94d5b384ce"],["/posts/3405577485.html","561943c38a372303dfc1518f8ff3d7b9"],["/posts/3498516849.html","e6debce6cecc9260e02d3269c894403a"],["/posts/3513711414.html","47379b8ed723d71d02febb8c9ad43c11"],["/posts/3546711884.html","1d3c535ac2b98f88746c3b918ae0479c"],["/posts/3731385230.html","d3bde285cbe805216a9811f640ee377a"],["/posts/3772089482.html","b352c7ac349f60b4dd89c0d99f0aa3b8"],["/posts/386609427.html","c3018050d3ed4c685d3f60d017ddb828"],["/posts/4044235327.html","319aee1806c4c374da7ee328931a1407"],["/posts/4115971639.html","3cbd9cc9c8c50d6e78d7bb4721401269"],["/posts/4130790367.html","f245325ca000382f99d210c6b0422fcb"],["/posts/4131986683.html","691bc9f69b4674737fba982d9b3d71e1"],["/posts/4177218757.html","c217c4f9b4c9bb2c2edd65cb02a3f760"],["/posts/4192183953.html","982f57eb96fb23091389e92694b8100e"],["/posts/4261103898.html","bdba203f132fc86b5cfe002d16bc331e"],["/posts/469711973.html","9b386be1336a0249281f031ca5d6e560"],["/posts/482495853.html","4084ea37b432c3b8f2da60533e39250d"],["/posts/488247922.html","7ae40a260074b88a5da1177cf22a76e7"],["/posts/517302816.html","18ae9c12dc2598063ac425da06db9fd0"],["/posts/570165348.html","1c60e59a14f9e46e3874b32849f1e76c"],["/posts/595890772.html","d2239ad392f97611c602c682a7ed9f36"],["/posts/67485572.html","a2bd8a7f826a820d2b15d992305592b6"],["/posts/694347442.html","339493fa0cc6a99ea20fa04e5b2c0405"],["/posts/707384687.html","ec35cc7b4f318e6c49847bb1962de7e4"],["/posts/71180092.html","918c3e1d984f3a9601cff64d5bf3e604"],["/posts/716459272.html","46263ab06e6881eeb06ca8469849aba6"],["/posts/778231993.html","2842077eb080df7b5fc6b6ef45edd9a1"],["/posts/795397410.html","2e996f6966bd5a16addb1ac0a97e49a2"],["/posts/820223701.html","83b03071a8689880a815139febfab81a"],["/posts/830372185.html","7d1366846e0cba74b4d57008dde0e690"],["/posts/88294277.html","85b22cf3a51289f308286fcf7d69a985"],["/posts/939963535.html","dff9b40207d76acf50e0228b0e95c6fc"],["/posts/983786067.html","e535531475321bca6aff224d2f908db7"],["/sw-register.js","c76bba28525bb0d103368815cb6ff51e"],["/tags/C/index.html","2a3075e547e5f5faf46caf7ac8dad9ce"],["/tags/C/page/2/index.html","819f3308381bddcbe56bb335373e8f9b"],["/tags/C/page/3/index.html","268ae55cdfbabe3503fe861ba18336a8"],["/tags/ETL/index.html","defcd3c314da1b323e88b4121ad44167"],["/tags/ElasticSearch/index.html","af128b5fa8f371f23cad91823eeb35b1"],["/tags/GUI/index.html","0266e831bb39320810319e7da7f1db24"],["/tags/HBase/index.html","e199ca5588761e1155b4cef4d4d337e3"],["/tags/Hadoop/index.html","20ecd5793a81de3e9316923330eb110b"],["/tags/Hadoop/page/2/index.html","a71aa3494b5e7d5ff3456fca67b1d16b"],["/tags/Java/index.html","115a44fa16fc980665785ff5afb854a0"],["/tags/Java后端/index.html","1783930a9c6ee619d5bd843873d15375"],["/tags/Java后端/page/2/index.html","9ad48aebce19bfbeaa0ea97229813bbc"],["/tags/Java基础/index.html","0b11637af8156aa0bef0783804990c80"],["/tags/Java基础/page/2/index.html","56daa9243cfe5a875d2807695df9de00"],["/tags/Kettle/index.html","ebb0280656a5fa8a4dc0be379a19667a"],["/tags/Kibana/index.html","7d847ca389d121641e0490a90170f0e6"],["/tags/Linux/index.html","467aae2a51745b64925ac5f50b351a7f"],["/tags/Linux/page/2/index.html","3df2d1e912f1a41664f0470add411bf3"],["/tags/Linux/page/3/index.html","68dffe426bfc097bb399a6f4d05bfcf6"],["/tags/Mac/index.html","d1c24004f917335bbfe8a6e02817f24e"],["/tags/Mac/page/2/index.html","f444190102bd24854a9dd93f24048094"],["/tags/Maven/index.html","ebd9e3aa38efd397b145c2a8cd4a6553"],["/tags/MySQL/index.html","95c0509d7a4923b977f8a8704b6a0d86"],["/tags/Python/index.html","8811e32a35f228cc15e2c2c4fd7e6ab1"],["/tags/Redis/index.html","6ec39a249fd039e06ccf2a42e0a48d1e"],["/tags/R语言/index.html","69f73ae12b3cb863a016cc002caeb634"],["/tags/Spark/index.html","ebe3d1e75d193b2e7e88504ad9f8fc37"],["/tags/Ubuntu/index.html","61f8a1eb7e37f11b73e35f13755b4a5a"],["/tags/Vue/index.html","043a151b7976bd2d9003e75604946c44"],["/tags/Windows/index.html","39ca7881f787c0f06553fac79e906961"],["/tags/ZooKeeper/index.html","9a06776b8e746c8cc62513dcc6c6c0dc"],["/tags/bfs/index.html","16a7e5cba8e5765c13335bbecc3e46bb"],["/tags/dfs/index.html","92d9eb2d130321f3c42bed360ee945f6"],["/tags/folium/index.html","ccf350b346bf133543694de8dc7ff146"],["/tags/git/index.html","1931ec1f5b202c312f8d4fe5a278cb7c"],["/tags/index.html","c27483a60156ab7efc1e5b7fbe633fe7"],["/tags/latex/index.html","fd68dadc0349a1002b078bbc065c0219"],["/tags/中间件/index.html","540536ea69f8f53828fbccb0302a6610"],["/tags/二分查找/index.html","37c5464de94d15bef112bc555199bcf1"],["/tags/优化类/index.html","b6a7b496058ad79914b5c95f52f75657"],["/tags/前端/index.html","196c20af260b4922137b25419145002d"],["/tags/前缀和与差分/index.html","64aa667e9c047c5e2b921a071545e546"],["/tags/动态规划/index.html","b2112f31fa8b8fd781ac2d0da4edb890"],["/tags/动态规划/page/2/index.html","0ccf297886c65b7a3343d78ec33fdbbd"],["/tags/博客搭建/index.html","4f9e10a91fb0656cf4ead1969eea642a"],["/tags/图论/index.html","3dd1d5cf7c1636b95d44003f3215a6b9"],["/tags/大数据/index.html","86e02c3e4c7b0ae19a4e7a5798f4222f"],["/tags/大数据/page/2/index.html","f335c4aea4f616e82804e9650fb278fb"],["/tags/操作系统/index.html","66ecf2b58a6c446b01c0e0a48cf88b7c"],["/tags/数学建模/index.html","15857f714f7cda6f33cddd6f956ca2a2"],["/tags/数据库/index.html","ee0f3b76d1a9042072135d01ca849b93"],["/tags/数据结构和算法/index.html","2deb33f1ca0de09804d8921cd6e1f375"],["/tags/数据结构和算法/page/2/index.html","ba511a3248b23fe81f202f04013bf376"],["/tags/数据结构和算法/page/3/index.html","e2e8e8d7d8f242c04e36519f4b963b7e"],["/tags/数组和字符串/index.html","26ba5766ea70150d6cf3cf75d8ebc31a"],["/tags/枚举类/index.html","ad899e2b7bf4ac2d728c3f0eb8a85e60"],["/tags/栈和队列/index.html","2972ffcb857e88263e75cf0df3a36d10"],["/tags/树论/index.html","068c372536073e12fd32723d7dd600d7"],["/tags/测试/index.html","116fc5136775e8791d28288c2f53bd4b"],["/tags/环境/index.html","b186050440aba8874b79e6095cb6c455"],["/tags/环境变量/index.html","5be16fe47965f953a5faf0d6b1fb14a3"],["/tags/绘图/index.html","4f2846407f9ac5370e22a49d46af6ac2"],["/tags/编程工具/index.html","1171d8e840b69b05226c0ed25afe8d0a"],["/tags/编程环境/index.html","c5085213b733383ebd76eebf13bc8e9f"],["/tags/网络编程/index.html","bbd026cbccff5ed9997d1ee1bfcb77b2"],["/tags/英语语法/index.html","9c04938cbc1b5ca1c3114dc8d92f4f48"],["/tags/论文/index.html","e389b5d9c0f4ffb0d3cf5fe9befc1fc4"],["/tags/资源下载/index.html","2f313817b56aed7f74093be291d262a7"],["/tags/链表/index.html","e2dd963e029d86344f0c23cf70e2dd0e"],["/tags/集合/index.html","300ec575e6810f2d4c882cd452727202"],["/tags/集群/index.html","b5f0c1dfec8c8eb82b223b41bb819b4c"]];
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
