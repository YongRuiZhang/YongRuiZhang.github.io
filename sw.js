/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","760955787c3f98d5d01004c436a365cc"],["/about/index.html","f352247ce6f53b664ff03dad5f4f3117"],["/archives/2023/01/index.html","d0e0ccff80c21f7e40a1fc40b68a4c53"],["/archives/2023/02/index.html","26b3c51e8cdbc5ee886aefd5fbe670de"],["/archives/2023/02/page/2/index.html","f84ab57964900e0aef2024644daf5997"],["/archives/2023/03/index.html","3abfe86401c43edef5aad169ac7303a5"],["/archives/2023/05/index.html","1378ee4a0746744d0b0ff15e9506051a"],["/archives/2023/06/index.html","fd8fb668b6690adc54df443cd8c3e053"],["/archives/2023/09/index.html","2890514c7dd3956c50c3bd614c9ddd45"],["/archives/2023/11/index.html","d1ec33141413af0ec68b827193ee427f"],["/archives/2023/12/index.html","c64c8d657a2e78e97f24350ed7ed62c7"],["/archives/2023/index.html","1e3accca82117385e1a216fa1e64652a"],["/archives/2023/page/2/index.html","8d7cb3ba593c677ebb3e78c5ea04f3ac"],["/archives/2023/page/3/index.html","d2a6a7999afe25b4fe3ffb274cdf252d"],["/archives/2023/page/4/index.html","905ffa88a20ca09d7170022396fb4900"],["/archives/2024/02/index.html","f02c20799c25016c12f752949ce911b5"],["/archives/2024/index.html","1228397cbf9db6b4a524e676c6348ae1"],["/archives/index.html","f9a466e12a7ff8dee5bf45db3fb04dd5"],["/archives/page/2/index.html","2cac95962a75cee4528e1fe35a27ed5d"],["/archives/page/3/index.html","585083ba3439f65fd379476176f1625f"],["/archives/page/4/index.html","cf7776e0767b35a5fc70ffa57060f772"],["/baidu_verify_codeva-qQP2iZOMLX.html","cfdfb172fa5c53d8549d41af267f47a6"],["/categories/Java/index.html","84fc05e974a591acf58054bb9fbf76c3"],["/categories/Java/后端/index.html","ffb20314c8ed4668552840ee72abd17d"],["/categories/Java/基础/index.html","ae9013c8332cac311ac8b53e58cd1548"],["/categories/Java/基础/集合/index.html","b7a6b725634df2503bbef7ced48ca249"],["/categories/Python/index.html","e4fe845286832122e9ccbb0dc3d03f8b"],["/categories/Python/编程环境/index.html","b668d26031b8db1b92449de7218a7809"],["/categories/R语言/index.html","1bb125c3b1afe905c3f95d89f651dadb"],["/categories/R语言/编程环境/index.html","a19dbc1fa2486e1f509d72f09737cf79"],["/categories/index.html","9ae045b99aa872c79ddcdbf94abb9567"],["/categories/中间件/index.html","3dbe6851c8e64b0fe534c0e43d3f10d0"],["/categories/前端/Vue/index.html","a017a3d0963caa5277985ea41d635646"],["/categories/前端/index.html","e9a37f74c9ba523cfb71e866f4548819"],["/categories/大数据开发/ElasticSearch/index.html","063e78826e8e758f99a05d4b1fff85b5"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","4f3368a7097da62055a43d004a0dcf2c"],["/categories/大数据开发/HBase/index.html","a6bfc75060370cfb1098d1108c6bfb26"],["/categories/大数据开发/HBase/学习笔记/index.html","e09854efcd4f68b24cbc0e97faf60708"],["/categories/大数据开发/HBase/环境搭建/index.html","cf196f551cd6faf6b059cd5cb4ac379e"],["/categories/大数据开发/Hadoop/index.html","e0b161b157374832382217c4a9cfca61"],["/categories/大数据开发/Hadoop/技术/index.html","9e32bb7b378ae9da91feda38ef89d048"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a93d6986793386d3f74f02cb98bde48a"],["/categories/大数据开发/Redis/index.html","cc9e869902186ed04c71dbec4feaee83"],["/categories/大数据开发/Redis/技术/index.html","ef44d2fe274da9eeec743191426f4926"],["/categories/大数据开发/Redis/环境搭建/index.html","beba6af70a6052d977199dc5557a7f53"],["/categories/大数据开发/Spark/index.html","ddf71ce922562d2981c73fee110e0b2d"],["/categories/大数据开发/Spark/环境搭建/index.html","cd0ffed477e64a5358c697f9f27b3233"],["/categories/大数据开发/Zookeeper/index.html","1895950a94341ff906d7ea0fb2c3601d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","06b62478f491cbcd75288e0ff45a486a"],["/categories/大数据开发/index.html","4e124288649bce34ee20dbb84240571d"],["/categories/学校课程/index.html","51e954ffea05a432c35efe4109ef6f82"],["/categories/学校课程/计算机操作系统/index.html","9eea9ec667fa04cc5e364045f5daef5c"],["/categories/操作系统/Linux/index.html","f031a0c0270e57f5050f66a25439be5f"],["/categories/操作系统/Mac/index.html","993e51c6946c4b2dfa3bfbc9fe0ad9f7"],["/categories/操作系统/Windows/index.html","54899c1e6b259171dbf6193d6619d98f"],["/categories/操作系统/index.html","a2f69874f7ff504402e4d8990a3867d2"],["/categories/数学建模/index.html","a1fa149a1a50e02aad0bd25e4b5d97e6"],["/categories/数学建模/latex/index.html","c0bb12e4c82072330c99b5450f1b28c9"],["/categories/数学建模/优化类/index.html","0d97b2cbde8914dc2bd0ae6023ad7bc7"],["/categories/数学建模/优化类/现代优化算法/index.html","d25c51c2cedb6ccf4b2371b9316db47a"],["/categories/数学建模/优化类/规划类/index.html","415609a2fb5e50c4cd35894eb8f66ad7"],["/categories/数学建模/绘图/index.html","c0e3dc68f198a99ae5045fdebf513983"],["/categories/数据库/MySQL/index.html","0496aba9fbd90d6616223c37ea9d5c5b"],["/categories/数据库/index.html","4e89a6c280373f21d44a1bb66843e394"],["/categories/数据结构和算法/index.html","b0062c03166f5f31d11cff2a75da59a6"],["/categories/数据结构和算法/page/2/index.html","cdc0cc6925f06ba79252abce68afe350"],["/categories/数据结构和算法/基本原理/bfs/index.html","864f97e75a04a80fab4e957e23be744a"],["/categories/数据结构和算法/基本原理/dfs/index.html","7106206c605967f78f1530c5684e36e5"],["/categories/数据结构和算法/基本原理/index.html","945f85f0c30c49c7fa296b4d1e16f065"],["/categories/数据结构和算法/基本原理/动态规划/index.html","1096669bdf81abff947ef0574dd485c5"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","2ab9721ca1087e9d678ea2a443c2669a"],["/categories/数据结构和算法/基本原理/图论/index.html","404bf11d739ad53ad2cd269a25e02bed"],["/categories/数据结构和算法/基本原理/字符串/index.html","e7a0bb9d8c59f7955c5cc10271df1419"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d0aaf7aaa2bbd66dfb70ba13d1e46071"],["/categories/数据结构和算法/基本原理/数论/index.html","63b2fff030835ddbd2daa09d7dd0e212"],["/categories/数据结构和算法/基本原理/树论/index.html","a15033992f0de7c1f93dba648e82519b"],["/categories/数据结构和算法/基本原理/链表/index.html","2f732792719b2046d630141957f33178"],["/categories/数据结构和算法/算法题/index.html","bf843eb9653e881b3cffaa59e8bc493e"],["/categories/数据结构和算法/算法题/二分查找/index.html","efe5abcda1ea2f0fedcfc2f20a359dc2"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5f50ef5065579c5fd6960ea38870921e"],["/categories/数据结构和算法/算法题/动态规划/index.html","9a809560867d4e9f9e2ee56d5367b67f"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","77a877e00c17096bd78d692e8201d25f"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","fc92827cb7699a1be90d3f8be1b93c17"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b0b7a113c2dc4e5907dadea17b4ad15c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","3ad59f4c660cea32902aadd1f6bd1c8c"],["/categories/数据结构和算法/算法题/数论/index.html","5f190d59265b3de77fa3f95464efdc43"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c4ffc1a1fb61cda2492737a3b6bd2ff3"],["/categories/数据结构和算法/算法题/树论/index.html","2b59fb730cd9ca69c7ec85c4519daf98"],["/categories/杂七杂八/index.html","d3acaf48c0d1ddc32546ddc3034a8ab2"],["/categories/杂七杂八/博客搭建/index.html","352ae2edc2b6fefa39feca96b9c11db1"],["/categories/编程工具下载/index.html","37aa008e9119a97d5270a901f97f544f"],["/categories/编程环境/index.html","1aa525111c454bd550e9ea3429f2abb7"],["/categories/编程环境/大数据/index.html","941bfcb74d08235df79b53a9cab30e8c"],["/categories/英语学习/index.html","d6778908fdadcb61079697fc767fc3c3"],["/categories/英语学习/英语语法/index.html","5040a15105a5b63265188f91a29eaf5a"],["/comments/index.html","5b79480e6c580a4184b5cd156231999d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0ea993148eb820fd5b57d420bbaff9c8"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","01b3b9775f14fba36f6b98578a8cafa8"],["/movies/index.html","18d5dfb4a38f3ff4d1a21d21e48f447f"],["/music/index.html","c022a812995da8ed07d9d1469a1da8af"],["/page/2/index.html","399e967d0c894d087df79d7c4613174e"],["/page/3/index.html","de6c31af5e7e5e442eac73cbf4133d05"],["/page/4/index.html","7e80298b049915a8835197b6176fe97f"],["/page/5/index.html","e30de6bda238542bf7d621d57bbb0605"],["/page/6/index.html","6a7edafbb53444a324ed6b5b9e415aa0"],["/posts/1021360842.html","9690dd5ee113b5d3d647874453bfec2c"],["/posts/1120620192.html","e49e60c7c251d93385599e5eb7d79b49"],["/posts/1141628095.html","5b1627c6516d0e86ead0694c6e8466ba"],["/posts/1168613674.html","18375df2a82236e53eabf4e622b32165"],["/posts/1219920510.html","9435b969aa089dc422525ece1692aeb0"],["/posts/1222166338.html","da04b7bdf456edafe0a77bf6916e7541"],["/posts/1259097482.html","34e110cab4e11b835511efed0e23349e"],["/posts/1271036369.html","1ad144b700441778ac2f243dc45ed9b2"],["/posts/1312847445.html","bfe6fca3da8f71dc940b76513676a79c"],["/posts/135355774.html","e810349d4357b6e5ee3efad7386b5ccc"],["/posts/1375344716.html","094e02843b88eae090afc540bc1733ac"],["/posts/1388991698.html","2ad94a00817a76f48ea7d1edd28c6bb8"],["/posts/1410315814.html","258f634fc96b45cd5a56a657cccbb91f"],["/posts/1452790229.html","e517ce993aab42d7644fcefa266bb220"],["/posts/1470079884.html","01da8da3da51fb46f2cd8421465a865f"],["/posts/1470079885.html","72cff5a4ff0565a5f1b3bf17352d9d56"],["/posts/1470079886.html","052f218374703b02e26b20016e826427"],["/posts/1470079887.html","487635f3e9dde7d7af855ddf86962500"],["/posts/1498536549.html","a271b9f1f3d8fbb20665556211074c29"],["/posts/1539568593.html","97a65f17d8fc264857eeb82bcf5ffc81"],["/posts/1547067935.html","372b82fc70db58c211636305517adbb3"],["/posts/1557866301.html","90a555983220a424dea3501f658ebcff"],["/posts/1571776361.html","f64c7ec04823d8774ff7452238518f7c"],["/posts/1605124548.html","d352fb45159e78a81bd3247f0bc57c70"],["/posts/1633036852.html","47b95a751638b94436a31d4cc9e02783"],["/posts/1674202625.html","37173cc1e35333c453355a70c6efb296"],["/posts/1765123828.html","85b002adb1c732d60af9e3a5bd65e9e9"],["/posts/1767336200.html","c9ea47a8ec96039a4cd2ebf1e133381d"],["/posts/1776114197.html","e903099a5dfa3ae625860e8b879ed99a"],["/posts/1817748743.html","93ebccd1097fe400542cd61eb7e0ca47"],["/posts/1925125395.html","3d8b1865b4256a37b9ebdf9b9565384f"],["/posts/1966191251.html","28249418d6c017ecb763c05e4455d239"],["/posts/1987617322.html","a38293d75e23b03c60e8e30fe2e5f9d3"],["/posts/1999788039.html","54668c8f28b7ed6ab99babd48316a19e"],["/posts/2075104059.html","b4c516e5f0cd3d42490fb0f43f681d7b"],["/posts/2087796737.html","3ba7509e92b32fd3d1543b9c73737c68"],["/posts/2106547339.html","4d6b3464411947a754f0c1087c580ed9"],["/posts/2207806286.html","509e1e7156142b436aae26bbad3ebabb"],["/posts/2225903441.html","8ffb8b552cbc202cc6ef63d609bbc104"],["/posts/2265610284.html","3f226a3ff534a9c8833339eb3ea10eb2"],["/posts/2281352001.html","bb95579cca1f51ecfa85a70568c3e082"],["/posts/2364755265.html","9d569e6853a945eb8567063dab0a3864"],["/posts/2414116852.html","f0a7e27b68916778ec43a84c5d03bbdd"],["/posts/2421785022.html","7e12823f3df69cb652c128815a9edfc4"],["/posts/2482902029.html","ccab4faf5fade345c29c79a8bb58aeba"],["/posts/2495386210.html","3d16991f2360a82448cef72dd8d46d6d"],["/posts/2516528882.html","a2b9fd6e78cfe22773f5f6ec599840c2"],["/posts/2526659543.html","5cb21ee3289e616e0bdf0bf49d8534ee"],["/posts/2529807823.html","3cdeda6714a7ae82269c6fcb56aae183"],["/posts/2596601004.html","374ed3ab20f7707a50cd3f4088448ec5"],["/posts/2697614349.html","c8e112e7ada51b2cfa984044b3f6a97d"],["/posts/2742438348.html","ae959dbcb1e26a2e09d23a411dbc641c"],["/posts/2768249503.html","2f2cce0e7a80b7d0f3e44c339f6171c3"],["/posts/2864584994.html","abf96f93eb67ad2c54a80d57898c05a2"],["/posts/2888309600.html","2f65faf2c7c3042dea9271bca4445cdb"],["/posts/2891591958.html","cc27ff27aa2f7a1f6478ca0ce1d614d6"],["/posts/2909934084.html","768b6222c6498840d3a4b5bc7765781d"],["/posts/2920256992.html","5e8c0b07fd7b68718b29e018f7d66ee5"],["/posts/2959474469.html","3884b227619ebe0880d18e16ae954bc3"],["/posts/3005926051.html","b48e2f70f012137d270f86abdc27d0de"],["/posts/309775400.html","833b056263b9239a975773e1269975da"],["/posts/3156194925.html","7023872dce99a340f906772c6dbbaaa6"],["/posts/3169224211.html","60d8dbb4bde70d6a574d32fd60685bcc"],["/posts/3213899550.html","53944ce8b77c74cb27e8598df021ff2e"],["/posts/3259212833.html","420c0d924ace746de4072308120843ba"],["/posts/3266130344.html","03c8f6900b386dbda49fff8b9178717e"],["/posts/3292663995.html","48267619a24b2938b684aca45d1b8d02"],["/posts/3297135020.html","74c650362d02f4481b92565f702b863b"],["/posts/3306641566.html","008eb373e87e930368912d218eaac363"],["/posts/3312011324.html","24c4a543c665a783ab3716bda4931296"],["/posts/336911618.html","1d96add0d7115bbc7152f1b49c7ef770"],["/posts/3402121571.html","ecd994cbac09dd57519e8f69a1942786"],["/posts/3405577485.html","fbeb73ce9c82cb3faa4f4da20de96580"],["/posts/3498516849.html","0fc29621aeb99f389ac77ae281ada3f6"],["/posts/3513711414.html","56aff457fa2cbeef120cebb0d4f89f3b"],["/posts/3523095624.html","6eed4f09dd201d81504b028009ba529f"],["/posts/3546711884.html","f86f3e8dc090c2201d840e4de7e2aefa"],["/posts/3731385230.html","93e36e5f07d5082fb150ade5fa4f5804"],["/posts/3772089482.html","359b9bb1d38a121abeda256543d57296"],["/posts/386609427.html","fc5cac026e852d5ddb403d87016214b2"],["/posts/4044235327.html","e32536a12c1c121799718c1c93fc0277"],["/posts/4115971639.html","32c2fed10cb17a9211cce35091d0f314"],["/posts/4130790367.html","bd981a5031c1c8e873972234e73378e1"],["/posts/4131986683.html","76ebef90b94f38b4d233a7e5a0d8ae6a"],["/posts/4177218757.html","5a03c7e904cd5b9db2661ea38e8e4424"],["/posts/4192183953.html","27b1a63a91398ed9ca3e5b1c1edf494e"],["/posts/4261103898.html","178cea7854c5662343739de21c2be12a"],["/posts/469711973.html","2779e94721e09d36fd6ac620f4d9474f"],["/posts/482495853.html","9c1f772d4177ce474b2b2e16712f20e6"],["/posts/488247922.html","72269ca494b645ad7e3ee3b529131de1"],["/posts/517302816.html","9d3015c1c6e8429d0acc1cbe3be71cca"],["/posts/570165348.html","891ff33ad3bc1066c3bc3f8612dfac8a"],["/posts/595890772.html","8125a08472c29aca953545134e86c6d9"],["/posts/67485572.html","32d8e80080059a2af48f8ab82cad8911"],["/posts/694347442.html","ef1aa61a4ad9c70c2434f14e7af2160c"],["/posts/707384687.html","629c5403c84f153900d4c395b0c7392c"],["/posts/71180092.html","a4af1e744686dff0ae9318bef94976f0"],["/posts/716459272.html","940456a8cbab57648e73c5e270513801"],["/posts/765481613.html","858ee1763aebd526a39f6d4fb79beb67"],["/posts/778231993.html","5bf121c9e98cc92876ab7b9d8bb4a6a1"],["/posts/795397410.html","3857d35c52f34c94080129bd03bfc08e"],["/posts/820223701.html","e8f76efcd7891a738790e6f4d09a6cea"],["/posts/830372185.html","df4f0658c78d774c4796e0c369aa5c7c"],["/posts/88294277.html","8024a48119f322a690aff68a58904fc4"],["/posts/939963535.html","b8f0d58ca908ce602432a516d3826aa0"],["/posts/983786067.html","d1358d27e78e1fe72928fdabf8fb2ed1"],["/sw-register.js","ca08a540da679b11887e145abb86485a"],["/tags/C/index.html","507162314a272a046261fb1b6b4b4b99"],["/tags/C/page/2/index.html","72ce21fd721911495063b819deb9993a"],["/tags/C/page/3/index.html","9640dcbcc492a250a027375a6479d9a4"],["/tags/C/page/4/index.html","793b9a697973f46f1d6abc3eb95d5d67"],["/tags/ETL/index.html","bfe85fa51714d0372b77ce7fb3ed832f"],["/tags/ElasticSearch/index.html","478b953908e9fb4279d4f5f6a2bdc309"],["/tags/GUI/index.html","e87c505945c2e1f829cc20db205cea6d"],["/tags/HBase/index.html","724b7b89aca01cca9b8eeadc62005f9e"],["/tags/Hadoop/index.html","74758bf910ca7e922891db306d0da980"],["/tags/Hadoop/page/2/index.html","bdf710c57976765d67e13f128c1706fc"],["/tags/Java/index.html","45b76e57ce24821d3a4c43fd08f09766"],["/tags/Java后端/index.html","296a8c19e40caab147dda96fa5e620e6"],["/tags/Java后端/page/2/index.html","d662427e36022b0a274694c1b0b15275"],["/tags/Java基础/index.html","be0c5dc18dab1ed4021d65bb66394eff"],["/tags/Java基础/page/2/index.html","ca7f0c4a2c55d5ada520d0bf4047f6bf"],["/tags/Kettle/index.html","0928cb8af8525dd0644d33fbc3ec3b13"],["/tags/Kibana/index.html","ee62bf29296e1d42d841659fdcba52cb"],["/tags/Linux/index.html","c0652496c4cd5d955e49ff55c3cbef8a"],["/tags/Linux/page/2/index.html","a9a4cc1b6692464a75c1dd3faa290551"],["/tags/Linux/page/3/index.html","7e8301e2bae032673e5aacb449fd8a35"],["/tags/Mac/index.html","327504415087f67c0a5922880bb082e2"],["/tags/Mac/page/2/index.html","b1dfd6a52da5afca67f7d564c2bd528c"],["/tags/Maven/index.html","90d72980e9f3256a2f2cf203fe4c8cfa"],["/tags/MySQL/index.html","5e5e7d229b9853bdfa9bab4a24c8d2ca"],["/tags/Python/index.html","4cde5b24d4f32ff293dc803fcf984853"],["/tags/Redis/index.html","dfdbdbeca0da419ccd1222905ed567b6"],["/tags/R语言/index.html","b542b6b0b36c77584dfb2988321c9920"],["/tags/Spark/index.html","b7e4a5814e367f3698a81d23162d7337"],["/tags/Ubuntu/index.html","26efe45da598b78e893dc431843fa225"],["/tags/Vue/index.html","3bf3efc223d02011035e81cdbcaaaa8c"],["/tags/Windows/index.html","3e6f0354f49fbfb1f6fcb6052d0db9ff"],["/tags/ZooKeeper/index.html","4650fe630a4101cf85c6f8b596268cd3"],["/tags/bfs/index.html","66f54f23e6afbd61d158ae0dc699da8b"],["/tags/dfs/index.html","60a98b8111dd821880a14bcbac8ec5b7"],["/tags/folium/index.html","9e4de47deeabe95f7bf45bf2eadbbdc8"],["/tags/git/index.html","253a2ef77188a3524451416b051e3507"],["/tags/index.html","f3ca109b7542342cb409c5ef375040fb"],["/tags/latex/index.html","03d9f937c2ba461dec18a09dad19695c"],["/tags/中间件/index.html","7c0f03d25a4bf4b4578c77db164e1fef"],["/tags/二分查找/index.html","7de04fa7d1a5e6d4502be1f10c24c566"],["/tags/优化类/index.html","c995acacdd70a9608a6f0b4d05e52560"],["/tags/前端/index.html","510476a752555d1c9721756df86d497b"],["/tags/前缀和与差分/index.html","7c790560b8c23a306236234b17e4a570"],["/tags/动态规划/index.html","b15b04e20606b7dec4f71637043f88c0"],["/tags/动态规划/page/2/index.html","197d9ecf7acba5b5bcf3b0fa473c56d7"],["/tags/博客搭建/index.html","ea9527d11dcb1da6f5165eb6c6f987b5"],["/tags/图论/index.html","f91b2e82587e895c02da3dc6b6155b4e"],["/tags/大数据/index.html","1273edeaafe8f8925658f5d34ae6adc2"],["/tags/大数据/page/2/index.html","6a084ed806dfbdccf37eb83938e5b67c"],["/tags/操作系统/index.html","c891731c3d9ead1e8470d298985afd3e"],["/tags/数学建模/index.html","fcaf78ed3120468e6c06d62fe57b8866"],["/tags/数据库/index.html","8d00c0b0e3af1731f115cd9895a1e9b0"],["/tags/数据结构和算法/index.html","0f46b46f70dffb7d8fe6894ebdbc54a5"],["/tags/数据结构和算法/page/2/index.html","baf10f0c0bb551b69535b7be2380aa31"],["/tags/数据结构和算法/page/3/index.html","105d838daea59fbc0d9f74f27457d50c"],["/tags/数据结构和算法/page/4/index.html","2cebf48a9af133b94b270dd0d90f2456"],["/tags/数组和字符串/index.html","52f978ed96e3b8babb6a49db6e7efc32"],["/tags/数论/index.html","2ed1392f241c698654eba34a8f18a04d"],["/tags/枚举类/index.html","1315c4d87518d8af0c2b5c8ab77a226c"],["/tags/栈和队列/index.html","632caa94d6d2a9d7ae44890975eb83b9"],["/tags/树论/index.html","ec25cb1a5521e3a5efef2f106cfe49f4"],["/tags/测试/index.html","96bc94a8e8e0f0bfe52632926a410f5f"],["/tags/环境/index.html","5b36eb2e03024acc00b7699510e68ae9"],["/tags/环境变量/index.html","0fed1686890eaa3efd1dc96e5093a533"],["/tags/绘图/index.html","8665b295f56a12e4d8aa21f265fee062"],["/tags/编程工具/index.html","9dddc401a4656bafc627e8bd39090741"],["/tags/编程环境/index.html","8c1a50cc9864070a5bc02ebf63e8ca53"],["/tags/网络编程/index.html","2ed8302107399c1ba6b518d82cffd655"],["/tags/英语语法/index.html","30112c06367347ee5178f99c56c8d9b0"],["/tags/计算机操作系统/index.html","3b9cb5723fabf7dab85ca161691f910d"],["/tags/论文/index.html","bb6f9aab47dd92c80286f96fc9720075"],["/tags/资源下载/index.html","10d3f812b2fe3a2fa179a0f52f38bf46"],["/tags/链表/index.html","1f202f5e2bfa0cec4bf8f3e19b3eebe4"],["/tags/集合/index.html","aab1c025f67e76d8f08893b58f57e729"],["/tags/集群/index.html","d360cd545521137c145d75c1633a8b35"]];
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
