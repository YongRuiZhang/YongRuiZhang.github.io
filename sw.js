/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","f03dd63152609fb762f1cd21eacc8bd9"],["/about/index.html","9ea1d4873810c453823e9647995ac979"],["/archives/2023/01/index.html","61430c17c4611937b2093da96dd741c4"],["/archives/2023/02/index.html","8aec89b01dd2cd29cfcd7edcc3eb2141"],["/archives/2023/02/page/2/index.html","288b0c671bf586697186399f64f86473"],["/archives/2023/03/index.html","29e83043db57bbcd47ec6963306ad765"],["/archives/2023/05/index.html","8bf5971e44d004fd7286b4b0eb4895e1"],["/archives/2023/06/index.html","c61c241cfa2adbac4f7a7afad47c455e"],["/archives/2023/09/index.html","54c822acb79e59885d6ad89f82a2c362"],["/archives/2023/11/index.html","20cae737845a4df8acc55be98d3ec285"],["/archives/2023/12/index.html","f01e545eb30178e9cd3a7afff55233d0"],["/archives/2023/index.html","1a18d5b80a025ce3084313691c283142"],["/archives/2023/page/2/index.html","ff97065caf8707955749da486cfb2eaa"],["/archives/2023/page/3/index.html","45fd53b9807babedf276c6d038ed8e64"],["/archives/2023/page/4/index.html","da12c55288466dd998eff90572bf1d9e"],["/archives/2024/02/index.html","0ad8826d3f85f46a43d28d11dacb5d13"],["/archives/2024/index.html","6111420e9be29521c74cbca766e0ea06"],["/archives/index.html","c64eff29b3d41f880152d9a536ecf6cd"],["/archives/page/2/index.html","814d323d76bcb9a12183c3c53a47e891"],["/archives/page/3/index.html","9e17b59af4923970ca29220827d4e250"],["/archives/page/4/index.html","0ceeb9c54cf91c1c47e3cc921ad66c3d"],["/baidu_verify_codeva-qQP2iZOMLX.html","7f88d651c3785dfbde989a08e7b5e5b1"],["/categories/Java/index.html","4993ddffdd978e407b973fbb99864630"],["/categories/Java/后端/index.html","3a75013b956552c9e7f5dc21095b7e92"],["/categories/Java/基础/index.html","90526dc43d036b4e1aa2ed3442bc0a14"],["/categories/Java/基础/集合/index.html","c68bf6440ad6ada902a4287b603458f0"],["/categories/Python/index.html","1f559505b61832dc5729b62c723d9fd2"],["/categories/Python/编程环境/index.html","bf9e36c350d8f349b7388d65fc3d6a48"],["/categories/R语言/index.html","b54a0b64177a8cd21fafe6619036d776"],["/categories/R语言/编程环境/index.html","aa5a2a681f9d95da6f91f7c55b99f612"],["/categories/index.html","c6aa6d7124f13d81b2d1205d0e0453df"],["/categories/中间件/index.html","772f0af7e366a42d34f155efb6c59d3d"],["/categories/前端/Vue/index.html","9ded02c55893ed49849f7605abca3112"],["/categories/前端/index.html","00ff8168e52eee124392ac322d8f5c0e"],["/categories/大数据开发/ElasticSearch/index.html","cccd7059c85a445f34633f9dab96413b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d0d523b3d1e16e2f36b5818f92c42881"],["/categories/大数据开发/HBase/index.html","22da977ec7a4813cd7c5c86a8f5fdf79"],["/categories/大数据开发/HBase/学习笔记/index.html","519730e24a313d01d0f12f089258c4fb"],["/categories/大数据开发/HBase/环境搭建/index.html","602384768a1f09c2531660ea2084299e"],["/categories/大数据开发/Hadoop/index.html","1c94b59836d35686ae537d1637646feb"],["/categories/大数据开发/Hadoop/技术/index.html","96c162946cbbd5e71d1f4d15ac98c1d1"],["/categories/大数据开发/Hadoop/环境搭建/index.html","743f433e8cb14dd4a1e138c5cf3b6e27"],["/categories/大数据开发/Redis/index.html","26b8559c5784d79d825b75bc2f550ac2"],["/categories/大数据开发/Redis/技术/index.html","61a9ce2d74376b51ad88697b4a2af848"],["/categories/大数据开发/Redis/环境搭建/index.html","90fd4fbc90f22ed06b2ce59bfb1482ba"],["/categories/大数据开发/Spark/index.html","0bb75b68a3a6717f6805916e5821bc90"],["/categories/大数据开发/Spark/环境搭建/index.html","dba67b30317902378c0c0dd2e6703944"],["/categories/大数据开发/Zookeeper/index.html","77f76431e2e6f6b9530e5fad5d1ff73f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","14d827ee42668e6cbbf50eda7bfb41e3"],["/categories/大数据开发/index.html","127f88ddb548435706e14cd48c8aa84a"],["/categories/学校课程/index.html","2674b7fbd2199d0a2948e26bba5962ac"],["/categories/学校课程/计算机操作系统/index.html","f5ecb3edc64c904f2c7ad0f4734a83d5"],["/categories/操作系统/Linux/index.html","5fe4463a9348f916a3e8c0e0dc733331"],["/categories/操作系统/Mac/index.html","42d4b749678d0dfa208d03ac6f8e2333"],["/categories/操作系统/Windows/index.html","ca6b974ad74447baa793e88ad1bd834c"],["/categories/操作系统/index.html","236f4184f2af1c6ead6f266978123e17"],["/categories/数学建模/index.html","c55d830b924cae6498c35dee6147356c"],["/categories/数学建模/latex/index.html","861c242a75191ac74008dfbc7521a758"],["/categories/数学建模/优化类/index.html","36031743fdb48198e8f3e9dd36710ef5"],["/categories/数学建模/优化类/现代优化算法/index.html","294b8c44bcd5b2891014268bb53832c7"],["/categories/数学建模/优化类/规划类/index.html","ce114a7870c430ea0b9a85ce2e31e10a"],["/categories/数学建模/绘图/index.html","4f521aae484583efd74ef7065561b3b5"],["/categories/数据库/MySQL/index.html","cb16b4573b05e9aa79a7072f97f693cc"],["/categories/数据库/index.html","34795a8d1f88fd6366d59b047a54b031"],["/categories/数据结构和算法/index.html","ce20f6d34b3d6498acc51c85f78c8910"],["/categories/数据结构和算法/page/2/index.html","a4e582dca9c6a1e7eb37d88b1a746374"],["/categories/数据结构和算法/基本原理/bfs/index.html","08e769a5017b2c3d4869e4c87f94fb22"],["/categories/数据结构和算法/基本原理/dfs/index.html","4fdd980e0ba8adf37bd14f4d9f7572ad"],["/categories/数据结构和算法/基本原理/index.html","b60b0717d367dc611a05612c721ceadc"],["/categories/数据结构和算法/基本原理/动态规划/index.html","eeceea6fd17fc0609edbf87bc44097c8"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d72ee2ba208d31b70f4b734638b58a9f"],["/categories/数据结构和算法/基本原理/图论/index.html","b1d672249ff72973076e41515f1f3371"],["/categories/数据结构和算法/基本原理/字符串/index.html","51a5e423ad2dc99cef326ece58645e37"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","be04a2f241c1649da73d39d71db1ae4e"],["/categories/数据结构和算法/基本原理/数论/index.html","5369a734ba0aa26f2dbd386081c405a7"],["/categories/数据结构和算法/基本原理/树论/index.html","e89b24a9f3bb950542ebb811cba5cbc9"],["/categories/数据结构和算法/基本原理/链表/index.html","bfe3accf42b14f05542092b4323aea8a"],["/categories/数据结构和算法/算法题/index.html","ea845b7b7f05f36d77346b4fe3bbf6d6"],["/categories/数据结构和算法/算法题/二分查找/index.html","0d423a5eb7304923024312510b971282"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0f333010a8b14dbfb5a466f2767fd9c6"],["/categories/数据结构和算法/算法题/动态规划/index.html","881dbb62615cfb2c63ea02fa10748035"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","df8f2f7d1ddf51016c3e08953d81601a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","819c68230d926afc158571b1b8879aad"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d5fdd7d1dff52e39ea989261732c6323"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","bc16ddd4519fab84ec66b9096de6f14a"],["/categories/数据结构和算法/算法题/数论/index.html","475d49430a7aba3b2b34408e9e4cc55d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","0eda1b7539827e2e998e3bf26ddf5bdc"],["/categories/数据结构和算法/算法题/树论/index.html","7d4414403b035e4702a7298fb1590e7d"],["/categories/杂七杂八/index.html","57329a2fb9b22ebf3ee7454456bcb67a"],["/categories/杂七杂八/博客搭建/index.html","a8ecfd5acbf5c4c3483ee98fe9a72bd9"],["/categories/编程工具下载/index.html","7e22f467cc75f0ecac9b4111065f2351"],["/categories/编程环境/index.html","e5fad9131862beca7cfc19d89ee390bb"],["/categories/编程环境/大数据/index.html","671d0c2e99c6b122e91c10465408932f"],["/categories/英语学习/index.html","5cb2e19884bdcbe54470292b583aaec4"],["/categories/英语学习/英语语法/index.html","20a3bdcb55ac612e71a3461579ef2b23"],["/comments/index.html","756916e2790aa5bfaa29d403526508f6"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","4588e95b416fa9c1fbe12fab9ed1c343"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1f6d4c38a1bd5eeec10c8d8ecc1477e7"],["/movies/index.html","d0d147c4e263423276af0607d8a87345"],["/music/index.html","41312d98a155fab60fa385222b4964af"],["/page/2/index.html","d63c325b678d8135f363d5595327302a"],["/page/3/index.html","8f25f9a7da5f59cd7e0bc28c7e59056c"],["/page/4/index.html","90461566fe796cc0df8121f8c9693462"],["/page/5/index.html","d5e5728412778078a6da094c30f4927c"],["/page/6/index.html","ae0487c487559476cd78c75d8e7043cb"],["/posts/1021360842.html","2f2ede27f0eed41e4cc41cbc340c4ff2"],["/posts/1120620192.html","00d347fcb4a351fde805d81e691f8a26"],["/posts/1141628095.html","ff4a5802e9379b8474cf5a968c7b647f"],["/posts/1168613674.html","ccc53a84f1c3cba87ca8201750530bea"],["/posts/1219920510.html","570cc5538192c684ce978ec2ef1e3eb1"],["/posts/1222166338.html","2cab6fc188451ff40799b3e3c0523e32"],["/posts/1259097482.html","7a1c80521a076fa61bfd86d0cebf6fce"],["/posts/1271036369.html","b7ff8229df33552ce03299cd5e0e01cb"],["/posts/1312847445.html","baa6c691f02aafe980282940ac81cf3a"],["/posts/135355774.html","c021ff0cd76c68a094f20f1d2a7a4e19"],["/posts/1375344716.html","8ad19039d4630161bad89121a8651faf"],["/posts/1388991698.html","b0b3f03c2d24d7678a4257faa2e093b0"],["/posts/1410315814.html","fe759bda53e2098863bd5ba28ebe6e64"],["/posts/1452790229.html","86c3615f592aa023ee414b488da95c8f"],["/posts/1470079884.html","f78900cd7e636c8f7e62435ac72849b6"],["/posts/1470079885.html","a159d75c97018c7bf98f8a9aeea70909"],["/posts/1470079886.html","e850ee89172d8a71bb74b07322f61279"],["/posts/1470079887.html","fafbf0174cc86cd017bc649e1a15dd79"],["/posts/1498536549.html","53c03ac7837288c12fc2d92ef2ade485"],["/posts/1539568593.html","6bd51fd898d1e3cdd9b440d282ca4d96"],["/posts/1547067935.html","a15956db8c586f356ae204fa630b209d"],["/posts/1557866301.html","458edb89ae6c6990560af2ec34df01f8"],["/posts/1571776361.html","b4d782d7437eadc0f1327f2a826ac4d3"],["/posts/1605124548.html","e2ca54e83ee95d41d8a8d668e0efcf9a"],["/posts/1633036852.html","bbe0708470d5e887c06757cc49a620b2"],["/posts/1674202625.html","9ddb44304b3c3cee45894cfde43c5493"],["/posts/1765123828.html","246a3cab10f36ecdfff903e26f555a6f"],["/posts/1767336200.html","8b446cbfd0eb3e3f01b81321184699ad"],["/posts/1776114197.html","33c314c22913f96c0e700e338eb69123"],["/posts/1817748743.html","a8e02488f1647cc73545893ab74d3401"],["/posts/1925125395.html","935b8666d7f2a7227e2e397abf48f082"],["/posts/1966191251.html","bb88d7bee1ea952c662189323b078621"],["/posts/1987617322.html","96b6e93a8dac302088ea33dd46476265"],["/posts/1999788039.html","d7d29ef7ccc2fea99a996d2e87d55814"],["/posts/2075104059.html","b56afc80224eeb0ddbbeed4e4109c62b"],["/posts/2087796737.html","be662900fbe02643446976f696a21518"],["/posts/2106547339.html","4072f4e2dd0d56c003544945b3573e07"],["/posts/2207806286.html","0c9cfc227431ddb0e8133c3a88116af7"],["/posts/2225903441.html","bd6794537125e29fa2fd668ea0fb7802"],["/posts/2265610284.html","410eb8ed88f9b2f5c2c32af551a78d04"],["/posts/2281352001.html","4d153549851c0f29ae372317ba160d95"],["/posts/2364755265.html","525ec8d2a23d397097ad472e87643523"],["/posts/2414116852.html","b2cec44018a78ab799a58dba1440bfe4"],["/posts/2421785022.html","f32e892374b049b016125ab21f3c9966"],["/posts/2482902029.html","42e4a5b67889aa561e861f41908292c7"],["/posts/2495386210.html","f7f34f35fb3fac15c57d776112885088"],["/posts/2516528882.html","370d4fa1c0b332282ffebab7d6937e8c"],["/posts/2526659543.html","3df4a801043aa9172da96465c97dc2e8"],["/posts/2529807823.html","ccaba886f977fc720bc8ddf6dc51cfbf"],["/posts/2596601004.html","440666763fe90436eb4012120e16d68d"],["/posts/2697614349.html","a9084c4fce2ae3d49a9eff5334503992"],["/posts/2742438348.html","2499abeffe23bda619c5513482d25535"],["/posts/2768249503.html","a36299906e8f4e83763ce31ad5fa9f6d"],["/posts/2864584994.html","74c58e89fa72987efb7f90f8179ff225"],["/posts/2888309600.html","0351d7141f12bb6c41707e92da1cf501"],["/posts/2891591958.html","878e6d48a6c5aa86f60cebe11df41f26"],["/posts/2909934084.html","e429f75420b47f544fe56c33ad6f9662"],["/posts/2920256992.html","ab55f089c106421a25c4b3c66333512d"],["/posts/2959474469.html","4103f943094322a27c85652fa5d792da"],["/posts/3005926051.html","5532bfea3361a2577997ff7d61b1d535"],["/posts/309775400.html","588fcbb2455a809fe4bb9479622932f9"],["/posts/3156194925.html","fd326da61572ad2b3b9b5f01825039c2"],["/posts/3169224211.html","635fcfb87136ac7f77fb6ab860e9d4e4"],["/posts/3213899550.html","601612898a8fcefe9770f445d7871062"],["/posts/3259212833.html","dc3e17013cae9807a0d5170a6c4cab7b"],["/posts/3266130344.html","5d33bc371bca5eff5e96befc7baf6652"],["/posts/3292663995.html","878bc7daeffe2525016ef43ce700b42c"],["/posts/3297135020.html","248049b0de0bde1bfe5f5f3e985d4ce9"],["/posts/3306641566.html","6e1812e17ff6342616cb7682f016d28c"],["/posts/3312011324.html","f377dcad7e471bc0b726ee4e84947142"],["/posts/336911618.html","dc38792e34ba8ca6151453c594846dd6"],["/posts/3402121571.html","912857375e42ea7ae9ba4094aabc0179"],["/posts/3405577485.html","988b9d055fd8c0c3c887c74dfe2f9ca7"],["/posts/3498516849.html","95094eedc7dcc402c8dffcea83bb64a9"],["/posts/3513711414.html","23f393accfe0fa17aa2f35060bc93a40"],["/posts/3523095624.html","ca56aef465cb3e553ddad777a1a13558"],["/posts/3546711884.html","78ba2bd0bb6c3a7c3faf87c49c59ca12"],["/posts/3731385230.html","f4e8d16b3295085e09bf5daf82d7d830"],["/posts/3772089482.html","62a6212621228abd19df66bda054b583"],["/posts/386609427.html","605ef219e1e2164b66f460fc1060b671"],["/posts/4044235327.html","60ade0bcc43aee859c33980fd958e6c2"],["/posts/4115971639.html","7e88ac34b215d0e19c2859e191c8b348"],["/posts/4130790367.html","aab90854274dee26d9c1e49885021548"],["/posts/4131986683.html","52643e5d5b9b689d4fd0a9b076801706"],["/posts/4177218757.html","3805e148f398428c28482289f54780b7"],["/posts/4192183953.html","a399b4e75520bd8bb259645221c1e52c"],["/posts/4261103898.html","1d9f4bb5a5c11038169ffb03a2932c70"],["/posts/469711973.html","0b628e81f32e76eab2579d069d036f93"],["/posts/482495853.html","5daba13eeda84921932be0d4b88c98e8"],["/posts/488247922.html","8121742d1fa99ef99bf3644f0216ff5d"],["/posts/517302816.html","8d902e0c90fb70ecd960c0e8e40fcfd2"],["/posts/570165348.html","a17cd430b906e58ab7b7c8b63e4692a8"],["/posts/595890772.html","aa69ded53fe13cead2a25d8056495e54"],["/posts/67485572.html","8ba92188cb7714827dcd5a3f0e55dd0f"],["/posts/694347442.html","f64d216b5a2eb0a96c1a03ebb3ad66e7"],["/posts/707384687.html","a82cd1b60b41e10d860f3569c5d4df04"],["/posts/71180092.html","d1842f6cdf93e1f67eb72ef2c154ca37"],["/posts/716459272.html","949e8fda3a318b8cd564b9876c06afb4"],["/posts/765481613.html","7e27d8e0b1362d1da61880ff5d265097"],["/posts/778231993.html","d3c36b3ad92826b4a60f2501768b21de"],["/posts/795397410.html","44231deb905ef28e6ba1cbd60f1389f8"],["/posts/820223701.html","22d18c07f55e876023ca4d7e4a79af70"],["/posts/830372185.html","effa025b233b56e8df1a3418d5987f7f"],["/posts/88294277.html","4278bb8a91ce9fac96b9ef9d8c5f728d"],["/posts/939963535.html","46f0cb8f54fa87605d4951ca33652a38"],["/posts/983786067.html","922e1d08e35c47d0bb309d7b1937f1ad"],["/sw-register.js","c5b2f7ead45798609676a2a6e8712ab6"],["/tags/C/index.html","eadeaa6373fd8c9b2a3c1b5d1f156b92"],["/tags/C/page/2/index.html","21427238a021f5f8597ef8f9e31c4cf2"],["/tags/C/page/3/index.html","0c1dcc50e346ef02593e48ba537d5d63"],["/tags/C/page/4/index.html","1aea8a40ce06b0e825d3690e4ce21b10"],["/tags/ETL/index.html","85d4dcce7750048c0f56783d67203763"],["/tags/ElasticSearch/index.html","4fa6f664864ffeb2bc0366e04793bf0d"],["/tags/GUI/index.html","2793777cc16f1dd08055d68cb476db16"],["/tags/HBase/index.html","cd096d01983fb09ca96fe85f764f2675"],["/tags/Hadoop/index.html","8bbadccf5160da405ad488c33fb506cd"],["/tags/Hadoop/page/2/index.html","0e9c3dde45570432ed545d0206871be7"],["/tags/Java/index.html","5963a8f66286e1bcc2083f2305ef2674"],["/tags/Java后端/index.html","23e35a3fe0915a3b1d19276e29103de6"],["/tags/Java后端/page/2/index.html","7307433ecfa43bd2bb6eabd8afad3d9e"],["/tags/Java基础/index.html","e1be3e30df569dbefd9f9f3253909a07"],["/tags/Java基础/page/2/index.html","be5d4b6e4560d25ebf6c95f5eb5b399d"],["/tags/Kettle/index.html","3543aef4c3dca77811b7b49478b6f09f"],["/tags/Kibana/index.html","973a42aed5eab7104cd9256173736a6e"],["/tags/Linux/index.html","e357b8f923920d0e8cbf2fc58e3fdd3c"],["/tags/Linux/page/2/index.html","fada231d0822ac205486cebd9a3a57b8"],["/tags/Linux/page/3/index.html","9796e02f71b628c7bfd098882f3b6c38"],["/tags/Mac/index.html","0cfa00569c409a3ccb1f2e4aab56b45b"],["/tags/Mac/page/2/index.html","a2033f6547992c14f0e165c2668a2d24"],["/tags/Maven/index.html","d10433bed61f39e5093da044eee3cad7"],["/tags/MySQL/index.html","a60afa038f0219164a0aa017d7e29323"],["/tags/Python/index.html","f502441f27ea12eef209450bbd798ebe"],["/tags/Redis/index.html","c1a500f5e19ed6252b434f24ae9c43dd"],["/tags/R语言/index.html","ade209a98f8dcbaa5b5a8204e2c8e687"],["/tags/Spark/index.html","9df5e7ad85232f41e623f7853698e435"],["/tags/Ubuntu/index.html","cd546152b599c5e094685edb30442ad6"],["/tags/Vue/index.html","85614e5ffb804e42de642f663a5a2e3c"],["/tags/Windows/index.html","f89e5c7fa951ab6c5721c1c1fb2af60a"],["/tags/ZooKeeper/index.html","55e6954310233edceae1bbf2bb805dce"],["/tags/bfs/index.html","cd54b3ddc0f6d96629327cd4ba8b7958"],["/tags/dfs/index.html","88def7197ded31f6ddb1f2fbfdbabcfe"],["/tags/folium/index.html","3332d03ddec8c7bd1de34748d863617b"],["/tags/git/index.html","ca0dec782302ba0a3959948f27f0df90"],["/tags/index.html","4d739670375d0ab9af77b6c416ed0d6e"],["/tags/latex/index.html","67eda0231b7335c6b3b8e7a7cccdad98"],["/tags/中间件/index.html","83aeeee92cc56ffbbf7a254ca8b88836"],["/tags/二分查找/index.html","ff0ebc9fabe67d371c9fb1ef7b6c8dae"],["/tags/优化类/index.html","484b624fe48fc496be341a06c05d1af2"],["/tags/前端/index.html","32a0482b5b6b0574656fa5982417f9a0"],["/tags/前缀和与差分/index.html","1de289a3f14013df7edf5d46d9997e9b"],["/tags/动态规划/index.html","32468b312f1fe5b48181ebb575fb5096"],["/tags/动态规划/page/2/index.html","df8d758dd584ec3a15fbac95a3f8c688"],["/tags/博客搭建/index.html","a3344d3e8cd2203375b2c010a5c91cc9"],["/tags/图论/index.html","1aa0865397ef0d06fdf0bd1844a993f8"],["/tags/大数据/index.html","9c8f7819efdb4cd87ebef0ca10db52ce"],["/tags/大数据/page/2/index.html","b2d8aba907d1185577de609021424f41"],["/tags/操作系统/index.html","198f744e0b06975d4fb05183634f5ad6"],["/tags/数学建模/index.html","3880145dcb563998575c49bfe7a3a797"],["/tags/数据库/index.html","de452672bc8537e72995331df71db6ee"],["/tags/数据结构和算法/index.html","52befd9459567b9bb0dabc1627d692ca"],["/tags/数据结构和算法/page/2/index.html","a6705c413b5399cca2b432ba5489fba0"],["/tags/数据结构和算法/page/3/index.html","556bf9c3f13e24ca53fe15bf60b9faac"],["/tags/数据结构和算法/page/4/index.html","649d7c2d550fe04edc6a6eb4c7cc9d69"],["/tags/数组和字符串/index.html","f18a9bbc1c71ce8ba9992e1064e5453a"],["/tags/数论/index.html","39fa7a27c04783472c2ea1f9469780fd"],["/tags/枚举类/index.html","148734e1cb87c6b91d6f5a73493fd684"],["/tags/栈和队列/index.html","e017c3bdb3eec7c420da58012193ec55"],["/tags/树论/index.html","02e5a062d34a284268d9efdb17451895"],["/tags/测试/index.html","18e467515b18a96fe0b3e839b09d2afa"],["/tags/环境/index.html","dfc99e02fbb6c209309807dc1d8abce5"],["/tags/环境变量/index.html","b38e268c2e6cb50703c3a59fb227ab91"],["/tags/绘图/index.html","72036da170fc0576bd93fb1b4753327c"],["/tags/编程工具/index.html","64933f9aace6c49f2f9c7d56c2be7ac7"],["/tags/编程环境/index.html","af31494bc1b12f86d7b25ee6b96a41ef"],["/tags/网络编程/index.html","2dcfb8addb88a42ebc8bbfb045f1b45a"],["/tags/英语语法/index.html","a1da080eda64509380a0971c4bafb291"],["/tags/计算机操作系统/index.html","c188ef192979a01e4325fa9891590430"],["/tags/论文/index.html","7a0f1e89ddb4e5b885c3152a3f41bf57"],["/tags/资源下载/index.html","28c360b41e7c47b80669987dc1885513"],["/tags/链表/index.html","8c71e951089d7a53939ce84e68d05d21"],["/tags/集合/index.html","0e45eda7c7e6fef7bea75e5dd20cc5eb"],["/tags/集群/index.html","ea4e2052edbbf58049d6f1bd878b7bc9"]];
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
