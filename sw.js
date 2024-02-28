/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4cecfef69c454683f717e212ce82aed1"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","6e4864223e8af7c8dcc9f5ac96ed4f87"],["/archives/2023/02/index.html","bd605b756553a56b221a14f81517e816"],["/archives/2023/02/page/2/index.html","3ee2321fd6d14532d78eae7464254b9e"],["/archives/2023/02/page/3/index.html","33cbd4397f7014150742c49afbb7a30b"],["/archives/2023/03/index.html","70b441e6879507e94ba15ee8e5455203"],["/archives/2023/05/index.html","f77eccdda26a754a671660c93d552abc"],["/archives/2023/06/index.html","1b2afb6d4415a0c9a26a7942781fcb71"],["/archives/2023/09/index.html","999e6b3939f8be9d0cc696bb23b4680f"],["/archives/2023/11/index.html","88a207bd5641cc06a40cdd1438acefd5"],["/archives/2023/12/index.html","c12e91f11be27f141cd66e9d6972883c"],["/archives/2023/index.html","c6afaeffc01ca7ca1345ecb231eab9fd"],["/archives/2023/page/2/index.html","4a3325da1b56885378f2d77928a96342"],["/archives/2023/page/3/index.html","6cea7190825116fccd45cb5401fc08da"],["/archives/2023/page/4/index.html","8cccd47ed0122152e1b7db70a9cf7f0d"],["/archives/2023/page/5/index.html","24f62fb8caf05c099801e0441dcd49b1"],["/archives/2024/02/index.html","1fe93ad81be85f4109ec857843e83aab"],["/archives/2024/index.html","619d7b15d496648e4b97f18934e4f926"],["/archives/index.html","5422b6fdc5be4db2926435678d166f91"],["/archives/page/2/index.html","ac8c71cdbd0fbeb9fe9ed3a1b5a395e9"],["/archives/page/3/index.html","5255474f6c512ac97c6b39777239e8fe"],["/archives/page/4/index.html","d0484448be2113081f1121205fd16b01"],["/archives/page/5/index.html","3ee2bf15b51c240a5e2d0b0f98275ad0"],["/baidu_verify_codeva-qQP2iZOMLX.html","526a2771b9f921cd3b2c82422613872c"],["/categories/Java/index.html","bf92c6d21a1591518b34a7f90c27e24f"],["/categories/Java/后端/index.html","cccd71d83b26eb5165d210ecda772cff"],["/categories/Java/基础/index.html","1c9bd48838ee0a5113fde4299aac40bc"],["/categories/Java/基础/集合/index.html","af4e6f3da079c636c9cdd3c679a3ce82"],["/categories/Python/index.html","7da2ef0fece61d3cec7f0f5e7f646688"],["/categories/Python/编程环境/index.html","e310646d006b1ce2aa7a1247a22cfdcd"],["/categories/R语言/index.html","2ec19ac68708a613e46f0032e8ee36a1"],["/categories/R语言/编程环境/index.html","18b9714731f52a6f6bf7b64db1f18cb7"],["/categories/iPad/index.html","6d1141425feb82898985a4270a3904ec"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","b5aa56434661a35c2a084f192be2b803"],["/categories/前端/Vue/index.html","882400b4f7aa86525a3cbc997c838916"],["/categories/前端/index.html","5e492e6b8de1860db4050a2b32055474"],["/categories/大数据开发/ElasticSearch/index.html","e27232c74cb5a3c37dd5a920272b3f33"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","5bcb924824c0df8acef43e879bb9e99a"],["/categories/大数据开发/HBase/index.html","d0084edda3fd4ed9ee74315074d738b5"],["/categories/大数据开发/HBase/学习笔记/index.html","26b609a0ba10190cafab674d4e296193"],["/categories/大数据开发/HBase/环境搭建/index.html","fbeba3138ae70f73df07fef5c3e177d6"],["/categories/大数据开发/Hadoop/index.html","7b9e794a49b63a696e019a0e9191806a"],["/categories/大数据开发/Hadoop/技术/index.html","72f106f6e3b497374b47b193282867bc"],["/categories/大数据开发/Hadoop/环境搭建/index.html","71567327c4262e3b40b7887015a19509"],["/categories/大数据开发/Redis/index.html","1db3d5d085bf9a5e2cd9232eaaf26f06"],["/categories/大数据开发/Redis/技术/index.html","814d3ae3526034b519a7ac8970221b24"],["/categories/大数据开发/Redis/环境搭建/index.html","ebf7edecdde462b5784cec1b34117381"],["/categories/大数据开发/Spark/index.html","3077159d315ab8f091dbd0a8a499987c"],["/categories/大数据开发/Spark/环境搭建/index.html","c1b601c01287e5a58a9e747cff432f71"],["/categories/大数据开发/Zookeeper/index.html","ac5d3a748ca1101728d60b8319bb74ea"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","448f2598b9461e6c5b0033a02e440d41"],["/categories/大数据开发/index.html","aac869632c313361f8a760463dc9eb00"],["/categories/学校课程/index.html","638b12699f0aaa71897744d9475a4c6a"],["/categories/学校课程/计算机操作系统/index.html","8d364a004dbc046f0a0613a722f3ec76"],["/categories/操作系统/Linux/index.html","7869cc5ebc7bac665278cfbade888ece"],["/categories/操作系统/Mac/index.html","d2c27b097b2b3fe1e9117fba07eff4dd"],["/categories/操作系统/Windows/index.html","ac1fd66fd16a4d33b3f002e0e3521f2a"],["/categories/操作系统/index.html","104d95a3f93c434f09098ced9a083708"],["/categories/数学建模/index.html","6b4ec39ff056af36b054ec00f1f32874"],["/categories/数学建模/latex/index.html","ad8b56a653f76c2903aaee98a90f118c"],["/categories/数学建模/优化类/index.html","9d4ac77002a3e0c8cf83712bfecd31a8"],["/categories/数学建模/优化类/现代优化算法/index.html","eb783c8c51b629070cf9f6dc51c1c97e"],["/categories/数学建模/优化类/规划类/index.html","da4ec896cb3b134e6f1b7c5befcea507"],["/categories/数学建模/绘图/index.html","5164b2c64afc6d0600ab0c28afdc4ae6"],["/categories/数据库/MySQL/index.html","6b3931f476b8aab2337d36dbdb269aeb"],["/categories/数据库/index.html","8d56025a1efb847359d8e8a000d4b321"],["/categories/数据结构和算法/index.html","f253610556e3df0e4095b09177e87ed6"],["/categories/数据结构和算法/page/2/index.html","c6dfc63b2b5b7db3286536aeb4d44824"],["/categories/数据结构和算法/基本原理/bfs/index.html","5017f792ad02497f29d5f94cdbadabdd"],["/categories/数据结构和算法/基本原理/dfs/index.html","bb0ae0acebe9fdc269a58c3c6949c58f"],["/categories/数据结构和算法/基本原理/index.html","2998864bfb25fbf8a7cd115715e52193"],["/categories/数据结构和算法/基本原理/动态规划/index.html","6a50aa3bf1b6936d54a5e4848615ff30"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","be48da5e9c56ccb0e58c8092c420e8d4"],["/categories/数据结构和算法/基本原理/图论/index.html","1e26c13e68d1c871efe7ee226809496a"],["/categories/数据结构和算法/基本原理/字符串/index.html","26f1d2c4a5f8b0398b5246188484b7e3"],["/categories/数据结构和算法/基本原理/排序/index.html","8f0c3cef002a3965f92672c038b40607"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ed03f447ccaf6a66f30f4b109dde8cce"],["/categories/数据结构和算法/基本原理/数论/index.html","8f7d7831a08271e7684dd7d2eac88ec0"],["/categories/数据结构和算法/基本原理/树论/index.html","f7b03f66ae940f555ee91bac3c6eadb0"],["/categories/数据结构和算法/基本原理/链表/index.html","9a0982763cb717dce5b78f468c3fac21"],["/categories/数据结构和算法/算法题/index.html","37177fa2f9f8193e589a0b74fd214825"],["/categories/数据结构和算法/算法题/二分查找/index.html","a6da6c0c0ddb2545de328df193ed8899"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c1d81a24176a1b55e5f5e4afbb2c33e7"],["/categories/数据结构和算法/算法题/动态规划/index.html","d55ddfa267f841e2b8aa96488568986e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","8a264c318a42a42e10a5450bcf750c70"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","936e69007e0e98e4a45d150f8d9a1067"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d2c5c3a864ebc1ccab7c8ef82742fdeb"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4e9f3030cae08d6226475c6fd3671cc2"],["/categories/数据结构和算法/算法题/数论/index.html","81699ffb02431a163fcf8c84bdbd231d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b227ef2f5f2270158153ba14411daaa7"],["/categories/数据结构和算法/算法题/树论/index.html","5469ccad7671b5bd62cd6400ffbb7d7b"],["/categories/杂七杂八/index.html","26c88c6875aa063595b60feb9a16c80b"],["/categories/杂七杂八/博客搭建/index.html","e93f5f9a90a62e82417cf4d5f0d7dc7d"],["/categories/编程工具下载/index.html","081b7b1d6aca02c552bb5940d861383d"],["/categories/编程环境/index.html","e850bcbb04b53181ffa8b6d179f68b3f"],["/categories/编程环境/大数据/index.html","23f9dec06d5622fd8ae2cd86a758ded6"],["/categories/英语学习/index.html","cb461a3d9b3935d6d93cae7313d00376"],["/categories/英语学习/英语语法/index.html","3697f1490389292161f22ecbdea21a07"],["/comments/index.html","5367d0a98d741f72516b1e2652be7e77"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0dec2e2ecce6393bee5aa1b2e8d1ea37"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","8b6febd48921d18066b4c428808a74ac"],["/movies/index.html","063d45beef2fa964d8bdd780db00c9e5"],["/music/index.html","3c4b8531d2ae884e471aae736ee3425e"],["/page/2/index.html","ddd4c91e5d5c97deca8768127030a679"],["/page/3/index.html","c319b10136d3180ff5398bd8c86f9d4d"],["/page/4/index.html","ce8e7dbf13e76ba0d23ebd4985e47042"],["/page/5/index.html","26ca7f81f919ceabea0b7009b11b2ded"],["/page/6/index.html","5e81e0179aed7b57c89b92c5d9865ea5"],["/page/7/index.html","b638f93200a7de51440747c6c59c1ba7"],["/posts/1021360842.html","c574745085af8f5ce68de85be9ba4bb4"],["/posts/1120620192.html","b7dfc680f57f7156eab03e250d2be750"],["/posts/1137707673.html","b8049e0d1914427b5ca8ef658956da7f"],["/posts/1141628095.html","af1c2b8734df1d4f39a840fe0f59f5cd"],["/posts/1168613674.html","d2e4978cc51c907678ca3e064e459e70"],["/posts/1219920510.html","c1b76cce0d2a1d0f950637341d339700"],["/posts/1222166338.html","dc36bf1d7179615962c26c5c96b271c9"],["/posts/1259097482.html","bf6b2e29a809291b41703ebc6f98a9f3"],["/posts/1271036369.html","91a37618320f0d0af92469af48f2c715"],["/posts/1312847445.html","a6b1e6a6a95f8602be3431c0d6950592"],["/posts/135355774.html","eced9d863ae9d4eb98eb8446ae2d28cb"],["/posts/1375344716.html","7c777c06578c735ab163666233ecbff8"],["/posts/1388991698.html","f92045dae324da0439ab396b7245d971"],["/posts/1410315814.html","c4ba15475733b9d53a72225118f666c2"],["/posts/1452790229.html","4cd2a8791988d6d2e218d459aa79b307"],["/posts/1470079884.html","3cbdd493895db1985b36bec1788f32d9"],["/posts/1470079885.html","e08ace6458e7913a275abb0845e34eca"],["/posts/1470079886.html","1bd41e2832586a3d68ff0a3685891404"],["/posts/1470079887.html","63ea0b7eb101046297110e3c00be9af5"],["/posts/1498536549.html","0f96dfaafdcc8d04968d3b8d84d300e8"],["/posts/1539568593.html","f976938e775a4925deb5486a8379d763"],["/posts/1547067935.html","93d165adafd38f693182e268a44f3300"],["/posts/1557866301.html","cd0328a3a03d9a2f4bde46caa6aa23a7"],["/posts/1571776361.html","4a5d7535520fc740a56addc8af4506f8"],["/posts/1605124548.html","5e3e42e3e93be8488d45a68d050906e5"],["/posts/1633036852.html","cbb4cd0585baa3dadde8cd0798b45ca7"],["/posts/1667740714.html","8ea86a9800b011c877179cdb3d2ecbd2"],["/posts/1674202625.html","ed2ede6b993dee450083d8c63d78fcc2"],["/posts/1765123828.html","47eb41a599499d0f64eecb3a26ca8dd8"],["/posts/1767336200.html","ab39e655aa5d4e9d4ede88f485d96b3f"],["/posts/1776114197.html","377dab3f0be081decdcc44f5ef8657f6"],["/posts/1817748743.html","b5d198e5dfb39fa822a0bf8278f09cd0"],["/posts/1925125395.html","cfc3607054069117b9cc3ea6d9f7b738"],["/posts/1966191251.html","70f27280d7636df6b57d584cf187c36e"],["/posts/1987617322.html","490ff5ecdbd32ca93e49414a9f7d76be"],["/posts/1999788039.html","12555ecb028914e12b4b706940e1e47e"],["/posts/2075104059.html","3ba631bf07b7fa89c35728a28d4f0a57"],["/posts/2087796737.html","06cf10c26f2ede5d277bc00dad75571f"],["/posts/2106547339.html","dfc1866ee67821bf6fbe7dbcbe61d637"],["/posts/2207806286.html","1f98f9c042968446332e581fe582c5e2"],["/posts/2225903441.html","00b270a2ab431c7f01ff6dcf2e0dfd45"],["/posts/2265610284.html","0c1b35c6a831e4f09165feea240b2c1b"],["/posts/2281352001.html","a0211e478450f0e9a0255b4c8aedf6ad"],["/posts/2364755265.html","e4090bb7a1479ab749454c1a64619b09"],["/posts/2414116852.html","3ecbdf95a1602dec768b21e7d3b4feed"],["/posts/2421785022.html","969ed281cfcf9d6c4e9844cdbf629f52"],["/posts/2482902029.html","0551eb20e3537873b47b4b7351a0cf8a"],["/posts/2495386210.html","83f7279702b86a81672f1b79ed340bbe"],["/posts/2516528882.html","4b524b7a6d989147e20aa566f3508dbd"],["/posts/2522177458.html","afd8a9a475073611f477ed33f286453d"],["/posts/2526659543.html","3bd109958f5783bfdea5210fb12248ff"],["/posts/2529807823.html","4fc4c68fcb371ca307f713d41f139e60"],["/posts/2596601004.html","dac7d2f47591d580977a1388df46fd2f"],["/posts/2697614349.html","2f5204b1c54c9b8306e68e3a8e3daca5"],["/posts/2742438348.html","eeb039965466030310f4378bcf5e1959"],["/posts/2768249503.html","3cd371188f44a5bffdf92509f1333507"],["/posts/2864584994.html","65f69af9f63ed9f418c97ebc9101f7da"],["/posts/2888309600.html","2ac74e1da54e29c26eff3090217fd1ab"],["/posts/2891591958.html","b3a3a5913771fbd158a8343bcc3b2213"],["/posts/2909934084.html","4461de5d31bbbecb86f132b5b6b0af3a"],["/posts/2920256992.html","573e1f88209341321095ef87ce05941b"],["/posts/2959474469.html","e42ea0abe711f07ee0790fb1cba37217"],["/posts/3005926051.html","57615b36d1b83a095d3a3be46184bcd3"],["/posts/309775400.html","43453d76906cd5703c22457c8917caf4"],["/posts/3156194925.html","d301123532fff8820cba9da783b5915a"],["/posts/3169224211.html","4603ed5da10a2d6568783a763401f43b"],["/posts/3213899550.html","2c7e87056ccd1b55d047a6bded289dbc"],["/posts/3259212833.html","6c85361f84e4b5d2472c337f772c66ab"],["/posts/3265658309.html","85371cea9180189732463fb49efb5450"],["/posts/3266130344.html","6267fc5518688494567c035807e61d88"],["/posts/3292663995.html","fcec0a3762e2c25af49a17824e8ca608"],["/posts/3297135020.html","35f680b7a30ac974d2ac0c47ac165373"],["/posts/3306641566.html","f4cbbd9ca659d1b3219fa66816c992e4"],["/posts/3312011324.html","ccbdab05305e26ec320baec81c5e196b"],["/posts/336911618.html","11cb618d710bf264314f14aa8a5b307e"],["/posts/3402121571.html","8b1f10f50a57700319d21d0f27ffe2a9"],["/posts/3405577485.html","e95e4369b6477fc2c87a0fdc5266c533"],["/posts/3498516849.html","95e8f8311759939ac9be8a0452b30640"],["/posts/350679531.html","012576cc1ca98aabba51552ce00b5cc8"],["/posts/3513711414.html","3b4b69a7b962c3d11f3d62e9adbca763"],["/posts/3523095624.html","2ba06589f6258850f99e0924b2ba736e"],["/posts/3546711884.html","0fc7e168a1792210be6fd465a792b741"],["/posts/362397694.html","69007ce96aae29382f956e1dda0a9cd5"],["/posts/3731385230.html","cead2b7b74e00801cef9b15efd06a3c0"],["/posts/3772089482.html","c6651aa245cb5b0187d10110fa69ace1"],["/posts/386609427.html","2d46d0230adebe323a11fdfafa549535"],["/posts/4044235327.html","faa5e5df25fd47979c6f380d80e883c8"],["/posts/4115971639.html","2c766019dedf35f81eb649a4c30c66bf"],["/posts/4130790367.html","4bdbf89164df3f1ee8ff076fa56c7dce"],["/posts/4131986683.html","ea5ce4fa951fc147b26596b426dd8ba0"],["/posts/4177218757.html","111e914fe2acf0620942f34fb5e6043e"],["/posts/4192183953.html","229cda42b9e2edf8c2ba8ed8ea76f92f"],["/posts/4223662913.html","5f2a8675ba55b45603e27f0b2bbaf84b"],["/posts/4261103898.html","f6e8a70398560b12ccab03b5047a9928"],["/posts/4286605504.html","8525b388069a34a516b0ef132d468bdb"],["/posts/449089913.html","09c69f8589d2c2c3cfb5c1ef04aec390"],["/posts/469711973.html","1e114d451203754812da472add173159"],["/posts/482495853.html","931cd5d893ca2e9405f1b6f51a1ac018"],["/posts/488247922.html","b9505aaad7e3270b245f59d777ce4675"],["/posts/517302816.html","655dcf9dfbe079d9fb49d800e625fc54"],["/posts/570165348.html","2f377b45c96f6057f885e98511025d16"],["/posts/595890772.html","f25536dca9caa05c62871976f8042492"],["/posts/67485572.html","0683cc8c986127d6f95fa06fe1cefb29"],["/posts/694347442.html","dabdea0f21cdda0e719818fa448c9575"],["/posts/707384687.html","0bbf230f0063095048758111b98afb79"],["/posts/71180092.html","b0b4d7953f7fe61bdb978705e3dcd7b2"],["/posts/716459272.html","72922085dd3d5cfdbcaabcd708351885"],["/posts/765481613.html","f7c19359ee5a5ff2cb3c4a0d3c8dac4f"],["/posts/778231993.html","5e1cf431b9711388b7d39031d6f642ab"],["/posts/795397410.html","89df64b5ed957010687bc5a5e67e6afa"],["/posts/820223701.html","ae47e2257d81388d11a5a19ad4bd6e6c"],["/posts/830372185.html","ad7a502a0d548844bbf94b7ff524063e"],["/posts/88294277.html","46725fddf90362641e21346235ee2961"],["/posts/939963535.html","1974e11fea6b02e0f6538f4a7122828c"],["/posts/983786067.html","66fdb8b0770969bf54d30629ba2fd699"],["/sw-register.js","1eb8dd03741cb130cd9b9736d3d9cfb8"],["/tags/C/index.html","1a08fd2b2dc5d5890430e27248be0ede"],["/tags/C/page/2/index.html","1a3d992beb662386d6ea21a50b25fd6e"],["/tags/C/page/3/index.html","e38ad35bcde1570987f84d5e1d9e3a37"],["/tags/C/page/4/index.html","d8e676307960d0256dc4b264ba494df5"],["/tags/ETL/index.html","76a0bdcff02f9d45db4a68ffd5a29e81"],["/tags/ElasticSearch/index.html","b2ed43fa8f9603038191ee2ba2edd4f4"],["/tags/GUI/index.html","39b37668b7c5cfe1f772e37e7f3079f4"],["/tags/HBase/index.html","a88c48d8310ccbf1ce1cf081f816b9d3"],["/tags/Hadoop/index.html","0c4a8ea8d28d214e6782a90ebcf33424"],["/tags/Hadoop/page/2/index.html","c066ff202e3ca1690a79e0acf2519dea"],["/tags/Java/index.html","7487dc2c9d753f69075f1b7193237893"],["/tags/Java后端/index.html","f0dd057c003342da3669c5ccbd21045b"],["/tags/Java后端/page/2/index.html","7abef7df627b4afca86be4f0d77aa17e"],["/tags/Java基础/index.html","cc31794fe916011e64bf2c621e94dadf"],["/tags/Java基础/page/2/index.html","1c56f3584ec59d21ba5d769cd146cc85"],["/tags/Kettle/index.html","1e6369c0e4dfaa0ebfbd09d0487a8134"],["/tags/Kibana/index.html","7e739a3e26d9ed0a9732c56fe6796ad3"],["/tags/Linux/index.html","332cd306d856be5c41de3612e78d8cb2"],["/tags/Linux/page/2/index.html","9d175a6da4b865a1c6e6493e9f9e5546"],["/tags/Linux/page/3/index.html","a5e05d68ead27c2f1c6c46d93e1267b3"],["/tags/Mac/index.html","1c1606c1b8a573a70d39ceac160e251a"],["/tags/Mac/page/2/index.html","f41042164eafb99769ba7ae0bed25a28"],["/tags/Maven/index.html","705622c3bc5c1a5f0642a1cd2ccf7e22"],["/tags/MySQL/index.html","3309ec23a8d6a90db772edb8c72d61e4"],["/tags/Python/index.html","89a9eae349615e611e1b72f30c9c91b9"],["/tags/Redis/index.html","e97fd5971f19029164c1fa655481b400"],["/tags/R语言/index.html","527408ea8d5d9c2656689f62bb6103ad"],["/tags/Spark/index.html","d18033616234d9c1c617fdd83b52b75e"],["/tags/Ubuntu/index.html","54c703c7086e03a4914645ab7f4ba107"],["/tags/Vue/index.html","a2b01b22da4ecc1efe37f07dfc80433f"],["/tags/Windows/index.html","7c6766059080ed86f44ac8a2685de3cd"],["/tags/ZooKeeper/index.html","76bf29e2947b95e2da066e57eead4773"],["/tags/bfs/index.html","8f26c7e44e1868631398f485a79769ca"],["/tags/dfs/index.html","3bd10167e7a1262dd86ecdc830e27b04"],["/tags/folium/index.html","f5165f4b8931310b35382a1d2623a1d0"],["/tags/git/index.html","0598f895879493c6c6114ea5abff6f06"],["/tags/iPad找电子书/index.html","aadf4dbfb7f404c715d7a4ed77f6681a"],["/tags/index.html","ea64562a33e816619694de9fced7ff57"],["/tags/latex/index.html","fff425e459c712ed100e474868c95ae7"],["/tags/中间件/index.html","e80adcc495a2233d09b2f683bcc52e7d"],["/tags/二分查找/index.html","777ce2251e59b7dbfef58cbd9d4c3f8c"],["/tags/优化类/index.html","7e2d12f649265103298e05bbb1f9d221"],["/tags/前端/index.html","6b1b89564836523b5e3e89a6140bc6ea"],["/tags/前缀和与差分/index.html","3e435413614b81628c992073a5885fa0"],["/tags/动态规划/index.html","63e049520fa5e9f55a15c970ba0ac01e"],["/tags/动态规划/page/2/index.html","bad4222d1a5003e3de0c2f3e61aac00f"],["/tags/博客搭建/index.html","076b0f8380210d4141325effed01a892"],["/tags/图论/index.html","4226a033295b6e2b9399abd0a1cb471a"],["/tags/大数据/index.html","c977c18b48f2401248cb16c8f16a578b"],["/tags/大数据/page/2/index.html","affc8c9ced34e705b0fbfd03d2d78f0f"],["/tags/排序/index.html","1388ff64db150ae4deeca8fecff116b5"],["/tags/操作系统/index.html","53ad631d004c42de9170c7c83445613c"],["/tags/数学建模/index.html","8bb6f2c4214606b516d027d060492706"],["/tags/数据库/index.html","8dce02827d0cb95f8afec305efb485aa"],["/tags/数据结构和算法/index.html","451492df29beb56a2673e92870bc5c71"],["/tags/数据结构和算法/page/2/index.html","e8fb4a591a4632daff2a15a1b0e55a80"],["/tags/数据结构和算法/page/3/index.html","bd60618ebef237cd4d16d6fbfb27deaa"],["/tags/数据结构和算法/page/4/index.html","64dfac2ed70ec77ece9fcd8e5b354039"],["/tags/数据结构和算法/page/5/index.html","ed06d0ec3925194b1e71ba8bfab96a87"],["/tags/数组和字符串/index.html","2372ff121ec94469523f4b455475ac32"],["/tags/数论/index.html","e0eefae87478c63d23f4736189854b9b"],["/tags/枚举类/index.html","301c4b9e21836533232b1caab73ece8b"],["/tags/栈和队列/index.html","e44a752f6203f5f599199fca21189118"],["/tags/树论/index.html","ed4ac0cb8fc44834fc89613a9625cf6d"],["/tags/测试/index.html","9a03c8f8b6b1de559ffbf920f6405dec"],["/tags/环境/index.html","bd7641d4b4e1f0567bf4877aa585b837"],["/tags/环境变量/index.html","15ba1d0ccf953a1106735f13934e3415"],["/tags/绘图/index.html","bada783a6c9c10add33954d0ed6777bc"],["/tags/编程工具/index.html","9c6889980143f091e32f371155bcdce1"],["/tags/编程环境/index.html","bca78e9c7c46d5a5ce6b8d24ea0c2b8b"],["/tags/网络编程/index.html","dace6a408e185623469dc8bbbc1b6352"],["/tags/英语语法/index.html","0330cc8c4af2b1a2f78b2e472bd44c8b"],["/tags/计算机操作系统/index.html","a1bf5707a35ea3bf1d17f96e34412118"],["/tags/论文/index.html","8f7027430616e21049390dc693845a8e"],["/tags/资源下载/index.html","5f85427f3f1ecfb19b19f34dcce44e20"],["/tags/链表/index.html","306e4f190a4c7f0551ce41a3a3003e9e"],["/tags/集合/index.html","02aab1d7c20ba16fa18e3350bfe08c11"],["/tags/集群/index.html","5806e268c4937cd5b5dff67d8e9d538d"]];
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
