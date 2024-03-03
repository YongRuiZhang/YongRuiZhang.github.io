/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","c1f73dd62f90640ffbfe023ac92b74c1"],["/about/index.html","8ac8cd505f53e7e1a81cc014d6fbb354"],["/archives/2023/01/index.html","a8e09a670488f2aa6eeb310cce187225"],["/archives/2023/02/index.html","b9b6636768eaa92b6ab9e7743092deea"],["/archives/2023/02/page/2/index.html","e3aeae197006e0167837f723f133556c"],["/archives/2023/02/page/3/index.html","bdedc32669fff338f7182f660b7e6d4a"],["/archives/2023/03/index.html","a8d86b68e26ae8f96fb6e2c563f7a907"],["/archives/2023/05/index.html","11226fb184584eeea6083863be757db9"],["/archives/2023/06/index.html","4040928e9e128d3045b627bf71db439d"],["/archives/2023/09/index.html","d2be5961375dc67e0dce1d3919d9d242"],["/archives/2023/11/index.html","6026b65511e003d1b6debab5ccc49d73"],["/archives/2023/12/index.html","4f86643a824dd1ef217a29d1126ec115"],["/archives/2023/index.html","dae8ea0363d0dd3e2d73f97030e2b77f"],["/archives/2023/page/2/index.html","95736e229dc27a7d5f4957781df587fd"],["/archives/2023/page/3/index.html","b0198b835faa13542b3ee9b61d829c31"],["/archives/2023/page/4/index.html","3163c8891cde2169ef9a6daa7b6748b0"],["/archives/2023/page/5/index.html","985eb0119436f81e8adc8c88f9c1938d"],["/archives/2024/02/index.html","5c12425bf9a6048c123e63e2f07d6eb3"],["/archives/2024/03/index.html","bd1a2b3a600914d49f5aa89c9fe23a53"],["/archives/2024/index.html","3055f2a14c92b5fae91283ef9426a6d4"],["/archives/index.html","b1913b98294cffd8cd17737cae5f5e11"],["/archives/page/2/index.html","d453c73fae9a9822c7da8517f3b5409e"],["/archives/page/3/index.html","8c62876a844053d2791491922ab7f705"],["/archives/page/4/index.html","88ba5398eb0459d00e66fbbd63e95add"],["/archives/page/5/index.html","db2556b34694929f860d72a2f0a5f45d"],["/baidu_verify_codeva-qQP2iZOMLX.html","a5367c0eef24aaf686237f0fec3a81ae"],["/categories/Java/index.html","a271bdcdf07c2adf69a1e19b186e840a"],["/categories/Java/后端/index.html","1c95b9f846fae4410145cf3a5208be8e"],["/categories/Java/基础/index.html","520c877ee8bb0ed46b508bc4c40ef83a"],["/categories/Java/基础/集合/index.html","93affdb18037adc351309fb83ccdb04d"],["/categories/Python/index.html","e55136fe82aa05605ed4f244343d984a"],["/categories/Python/编程环境/index.html","1b198ded30985543c93d7f1a2262faef"],["/categories/R语言/index.html","0b1b5963dd9733f3947e345e4f76de68"],["/categories/R语言/编程环境/index.html","41f55fa927c0f1b21ee96c163cc6f6a4"],["/categories/iPad/index.html","d512c2f538d9d049f5c3be16410cb140"],["/categories/index.html","295b23c90ba8f2d8ba57e5740c5803eb"],["/categories/中间件/index.html","cf8c13be1a2cf6ded7971f0035e6707c"],["/categories/前端/Vue/index.html","0abcef8d55cb455656c98ec8bcbd4d26"],["/categories/前端/index.html","8b97766bd22e94e31433ad42657f4cae"],["/categories/大数据开发/ElasticSearch/index.html","0ea8705fa35fd50172b2289fc941e87f"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","355c8399a0bb3fa12835c54d9ae41c1f"],["/categories/大数据开发/HBase/index.html","5d8ab1709cccbd67919d5b20d5d99584"],["/categories/大数据开发/HBase/学习笔记/index.html","3347b6479937ac22670524e1369ad0ec"],["/categories/大数据开发/HBase/环境搭建/index.html","53ffb359f832fdadf3aaeb1fc3f0a9ec"],["/categories/大数据开发/Hadoop/index.html","0bc39b78824344aeeedd07417a489992"],["/categories/大数据开发/Hadoop/技术/index.html","f2e13afc7c49b938914d84acb05f8758"],["/categories/大数据开发/Hadoop/环境搭建/index.html","80545ae1341cc80cbe3fef0d030ff692"],["/categories/大数据开发/Redis/index.html","b898743bc15298100e739b5d4c473ec7"],["/categories/大数据开发/Redis/技术/index.html","a28908eda94d3f8f0d1058f960026f02"],["/categories/大数据开发/Redis/环境搭建/index.html","b2c8205b07c78d46597790b40077ca91"],["/categories/大数据开发/Spark/index.html","271ead6fac185c6623d5c4b875416dd8"],["/categories/大数据开发/Spark/环境搭建/index.html","c49932b47eb10f913df6aeeecff6e4b2"],["/categories/大数据开发/Zookeeper/index.html","25ef22cffdbce6f17eb95f47c02bc8b8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1a4987b75420df196ce86baedee04a1a"],["/categories/大数据开发/index.html","23a7a636dd8f757d544a994723fca1d4"],["/categories/学校课程/index.html","0e5ca293c0022a6e99cf3855b1c40cb1"],["/categories/学校课程/计算机操作系统/index.html","53ad433649f7ab0ccac9d08c061bd625"],["/categories/操作系统/Linux/index.html","0a93708646a3d1bd90d0960185c3e27b"],["/categories/操作系统/Mac/index.html","f259deb47f504ddff42385087f6122f6"],["/categories/操作系统/Windows/index.html","45d8e14931e256d6388cc9d0739cecdc"],["/categories/操作系统/index.html","d274c969a9fe8a1053b8da385286c58b"],["/categories/数学建模/index.html","07a994423cebc52ecf64afbe1c9b4202"],["/categories/数学建模/latex/index.html","901a99aa7e42fae332e7394bde539400"],["/categories/数学建模/优化类/index.html","7d9e93444a993c7728a96ec0d7f58e09"],["/categories/数学建模/优化类/现代优化算法/index.html","c6400e311d28c485d77e0cc700ea9813"],["/categories/数学建模/优化类/规划类/index.html","33c2d3438ea930d877b59e0ab4bea474"],["/categories/数学建模/绘图/index.html","7837e960c50bd0d40aca19060407c18c"],["/categories/数据库/MySQL/index.html","6d7bf53172e9cfbe14a92c8ddb1af047"],["/categories/数据库/index.html","31a92fd8f29a49fc111b8c27407da9ee"],["/categories/数据结构和算法/index.html","60fb957222bf7787c0c312a181fc8100"],["/categories/数据结构和算法/page/2/index.html","76a1c0e928304c394232b18dfa68cfe2"],["/categories/数据结构和算法/基本原理/bfs/index.html","5349e26c6c5c048cfa2057ee67c9b257"],["/categories/数据结构和算法/基本原理/dfs/index.html","982e94f282c3cf5193cfb8036b6291be"],["/categories/数据结构和算法/基本原理/index.html","a6a7d79daa04a2eb9e2df21720b10383"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","d75d1227e6c30864e525eee1df1de8e9"],["/categories/数据结构和算法/基本原理/动态规划/index.html","6331487237e69ba4d7d25242888836a1"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5adcb73bd863b913a223b507abf0950b"],["/categories/数据结构和算法/基本原理/图论/index.html","e77a6f679adccee3c8f1b504d0244a32"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","5b9284e627a652014de53ae7b1a8abf9"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","74056929c902278a52298d0ee7545488"],["/categories/数据结构和算法/基本原理/字符串/index.html","400bf2619eb7f68008664f1a75bef273"],["/categories/数据结构和算法/基本原理/排序/index.html","a122402370c4d54baa4dbf4570a7d4f1"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","71e9ba3bd05af86b1e9349766ed379bf"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","0499b04b6b61dc3362da08ba2184a157"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","ef7d128ac7a136cb6197ca9aec1cfb79"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","821bf2f0409ef14ae49debdd9b94aba1"],["/categories/数据结构和算法/基本原理/链表/index.html","a1e314091afbb778203cfaed20753fdd"],["/categories/数据结构和算法/算法题/index.html","eb5ad48e5cf9fb57767673b3f2b06312"],["/categories/数据结构和算法/算法题/二分查找/index.html","fe4312375b91bd4716440a8461b49f9a"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","8ff3d21c2bc619a350c52b21615c87b1"],["/categories/数据结构和算法/算法题/动态规划/index.html","6759d600845f39ad756d93cbfa8dda91"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","85e73232dd568e93cea2ab4d42f6bc1b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","e6c26218f6da8daadc780eb052f05e17"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c8ec5981b6562582d13c4d59df619b18"],["/categories/数据结构和算法/算法题/图论/index.html","de879dfa1300eece2d9a57cfd5a0b9d3"],["/categories/数据结构和算法/算法题/图论/树论/index.html","27c6a7d1359bc76cbaf64c7842d78448"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","17ab3494307540ba7f0e2541c42ea1ce"],["/categories/数据结构和算法/算法题/数论/index.html","fe6d30c7e1139aac00a3144622350b79"],["/categories/数据结构和算法/算法题/栈和队列/index.html","ad1fc0514356d833ee65e9d1a0808688"],["/categories/杂七杂八/index.html","eec6c95a2c5c9c4fc5c50504bb013dd8"],["/categories/杂七杂八/博客搭建/index.html","d266f70a74a380c187ff36bfe4d34fdf"],["/categories/编程工具下载/index.html","ce5ba8298e7dbd488baed080c8771e34"],["/categories/编程环境/index.html","30f18b8aac0dbfa44662cdfe67ce2f44"],["/categories/编程环境/大数据/index.html","f75e0a71ce4965c2c8e290a91a06dcda"],["/categories/英语学习/index.html","c821c87c0cf062bf7395371ad5d752bc"],["/categories/英语学习/英语语法/index.html","42e71c3eb2f2ee3156e806d93c8b88e8"],["/comments/index.html","1f02e80962ce6559f425f85cd5a1f574"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","d1c4e080e8a90bd37366af321ad9962a"],["/js/countup.js","7baadf7bad427c145fb31aa080534ec1"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","d08f6a7b4447f4d974511c14ac79bb75"],["/movies/index.html","873c07e7da53e37eb1838442f7ada27a"],["/music/index.html","f619d21b4ac4969b9e248da9b74194e0"],["/page/2/index.html","66f1858baa0cf24f79d3c5027269c231"],["/page/3/index.html","0658d7fea5ebb16b87bba7b8df353efb"],["/page/4/index.html","1c6dfb72447d6fbe159f64415b33f395"],["/page/5/index.html","bcee68de54be12f3295d861731b9d725"],["/page/6/index.html","cee97a04b5894192e5f79e43a60a56c2"],["/page/7/index.html","23913430b7021fc9b951f994219bf792"],["/posts/1021360842.html","080d38ee4c98b5e66fd757cd78cabbde"],["/posts/1120620192.html","2d4a5e30f1aa8c2160c7df04871308c1"],["/posts/1137707673.html","dda2270cd93ac82d5f53f0460f4d353e"],["/posts/1141628095.html","29217115d520a7cca249c6d16fe2e7af"],["/posts/1168613674.html","9f9ce7444fc6a8ea76b8c252b04321d7"],["/posts/1219920510.html","f47d5887f8f3ac0686da1f7825aaf640"],["/posts/1222166338.html","a455b29f556f984303070ab28a7d5dd4"],["/posts/1259097482.html","c3f8b2fbf49a53563f5714f5e46aceb9"],["/posts/1271036369.html","14d85799eeeb4c027bc77d065b5733fb"],["/posts/1312847445.html","6e66cb581f314aacd71dcc5a18c1f75f"],["/posts/135355774.html","7c669b025c75be929192a4cbf4abe056"],["/posts/1375344716.html","5940ba21ca9320c7dcffb9875911bf51"],["/posts/1388991698.html","9fe55372a04f3428655710e5f6eb3670"],["/posts/1410315814.html","868074ccc3a278b9839e4175dbe151e2"],["/posts/1452790229.html","bddc85cb6229b237f6e9d2d1170a0b32"],["/posts/1470079884.html","00d472b0053e9e0e44924a659af9b34e"],["/posts/1470079885.html","d525ec9c990f49e1c9b09abffd14b35a"],["/posts/1470079886.html","98231892a42bcc28e6a2d8251ffff6ac"],["/posts/1470079887.html","8760847cbfc7cfe95ddbba3f1408550b"],["/posts/1498536549.html","eb333d6ab9d69f7c6350c8c076a2a26e"],["/posts/1539568593.html","c14df50b9c6c6b8d2cf1050be220f310"],["/posts/1547067935.html","961e3ca924639abdb765450d4cb4fffd"],["/posts/1557866301.html","ae88297e2b2e63866b617aeb9eb28801"],["/posts/1571776361.html","74e49d4dc957eaeac28856eb368c52e1"],["/posts/1605124548.html","65dc3ee3b3cf3c7f7b9880c266605ece"],["/posts/1633036852.html","d7543b8198643982ba61e1d5aed1e3cb"],["/posts/1667740714.html","7052400353901f84b4828ac7073f4147"],["/posts/1674202625.html","7c0aa887621fc780fa06113bddc7ca01"],["/posts/1765123828.html","3de2698df4755734c5708490ec7a5b88"],["/posts/1767336200.html","33b07d00273543f8a3dde1fecd5ba64e"],["/posts/1776114197.html","9c3a2f12666e38139ca9380f26de5025"],["/posts/1817748743.html","dbfa1b5bef2ce26477b9c729b6c5bb45"],["/posts/1925125395.html","c94ea8492c8f38f4fefb4bf64a2caf79"],["/posts/1966191251.html","c1e5dec46d602cacd5c26604ece16827"],["/posts/1987617322.html","5841bac52bacf5e1b05b56723c936836"],["/posts/1999788039.html","6a981485e77595ab922d2145ca7d84cc"],["/posts/2007534187.html","659248db9cbc879f4c0f523193849373"],["/posts/2075104059.html","c7ebb9170fa3731bf016eefa821aa46f"],["/posts/2087796737.html","d683de1477fd4f0948f3c4e1863664ca"],["/posts/2106547339.html","a92f109e20780c0aeced389c0e0dba29"],["/posts/2207806286.html","a3ed55c95344d4f360481e502b393446"],["/posts/2225903441.html","2e626a1c7eeeb1ebdf81defe5145af0a"],["/posts/2265610284.html","0d112420be07b5570141737d6697ec1d"],["/posts/2281352001.html","adb1a5612e72f6301907a3364cd6b666"],["/posts/2364755265.html","056de43e74a148fd6fa56e24a3811a70"],["/posts/2414116852.html","252bef58d81ce024b8f8c077ab3cc1f0"],["/posts/2421785022.html","d439cdb63c4ca9c26b8840c953c24805"],["/posts/2482902029.html","23c201412c5b14518324b081074201a3"],["/posts/2495386210.html","4bcae64be42f94e7ac56796253478c42"],["/posts/2516528882.html","bb9e37e6432089a37163cca1e8007190"],["/posts/2522177458.html","28078af542e60317242bb16e674fa32e"],["/posts/2526659543.html","bdd02f886fc6c23e773fa55afe1c6f96"],["/posts/2529807823.html","869d2f1f5164ae620bb3fdcd3a04732a"],["/posts/2592249117.html","663b9affe07dc70565d8b1d9e05f3975"],["/posts/2596601004.html","9effd5c92126e039c4072e1d28f38c49"],["/posts/2697614349.html","41e36402cff0ac625c91c4066f0f9bd0"],["/posts/2742438348.html","810f13edf6ebe92af1d527d7b1edc7d5"],["/posts/2768249503.html","ab6d33fff5ad5a435a85e0b7d8f86eae"],["/posts/2864584994.html","85b98e9f2253d7afcff8cf385de73fa6"],["/posts/2888309600.html","cc8f73f38daad1ff300adaf2e3f28c4b"],["/posts/2891591958.html","ff83116e2fcef2b284fd68c370a7d0e4"],["/posts/2909934084.html","a561ba2b464075684a0c149b5533aff5"],["/posts/2920256992.html","7001636d1490932572a16caa6a88113e"],["/posts/2959474469.html","164d61b320394601199da26d8fefa936"],["/posts/3005926051.html","da51b262d513586e00469790e0d65833"],["/posts/309775400.html","331726f0eb164e7eb9ed207b4bc8c8f8"],["/posts/3156194925.html","3a64f1815253e9b7e4083dd4939aff73"],["/posts/3169224211.html","da9a73000551faae1d2bec352d26b94d"],["/posts/3183912587.html","9d607673aa5d9b10c16daf0d3666a94e"],["/posts/3213899550.html","0db7008f7e8416b03de3f1f6f7c91fbb"],["/posts/3259212833.html","692d51be1e7e054c899924b9a661b7b4"],["/posts/3265658309.html","82fe99260271df3cc4237c9092551269"],["/posts/3266130344.html","59241c760c84836303c4f1aa8afacd15"],["/posts/3292663995.html","df02eb145b19d8de3b35422a87a4741e"],["/posts/3297135020.html","ce210a29655783d2fecca2717bf88a50"],["/posts/3306641566.html","14107b82f7c8c67ff02514ebe8bcbdc2"],["/posts/3312011324.html","3215228e4e96c4ccbe3aeb9d232732da"],["/posts/336911618.html","bbd827dc86fde9524ac4260c76732c20"],["/posts/3402121571.html","31daa2db3430d8c09c484f59372b3b08"],["/posts/3405577485.html","d81c4b2e8ec6f781d457eb4f41a175fa"],["/posts/3498516849.html","477a453165a19a1ae740a9c84e2affc8"],["/posts/350679531.html","db79f6077ba9f55aed86b212ba13f1b9"],["/posts/3513711414.html","e3f4a118d7b42c1a8372d69071321bd0"],["/posts/3523095624.html","7eb02cf71c7b5d35e8fb4e3c0dd17ae0"],["/posts/3546711884.html","c182815a510eb9c5e18cad1a5bb3b734"],["/posts/362397694.html","0ab72744531e4103982729f8ccfcd91c"],["/posts/3731385230.html","d8e70c51e3126466b27044d1f56e42b7"],["/posts/3772089482.html","a228245ce4ebdcb57cebcced8b3709c5"],["/posts/386609427.html","0ad641e10c9d71101040103d8ba55f00"],["/posts/4044235327.html","5ee4dd42cbe09be0b23d8aba3b5fb546"],["/posts/4098221856.html","30cb5651b55b5a49ec513865c2ee0358"],["/posts/4115971639.html","9ed8cd78cb8c5ea617946dfebd61618b"],["/posts/4130790367.html","729b27eb0b075d6c7f2921712ee0d33b"],["/posts/4131986683.html","5f5e2ceadf466e4de6d63921c4cc8826"],["/posts/4177218757.html","1b155f1fc9c98319bc7c568203ca8fb0"],["/posts/4192183953.html","7e46df654c7a4dd84892c5cced7af981"],["/posts/4223662913.html","d077c39428e6a0f5f35147df92717a8b"],["/posts/4261103898.html","9295bfc48bfffdb237510dc548e8926e"],["/posts/4286605504.html","762a9965adaba1dd99c1b160326f4e3d"],["/posts/449089913.html","957f56b9701bff509321b768ba976dd3"],["/posts/469277133.html","0eeeb2c83c03feb793c46b5647f2f581"],["/posts/469711973.html","5a9dec1961ad1f140e03eb6c1f0140a3"],["/posts/482495853.html","a689f35da2c003f50ac2aa0b439e8826"],["/posts/488247922.html","e8091968b01072282b966d0a72c4cf02"],["/posts/517302816.html","7a70b61b2a9b4aae965f06e235ac63ff"],["/posts/570165348.html","989a764188f7ccce4e71ef0794e1247b"],["/posts/595890772.html","8f4118a6526236682110e8138bcc320a"],["/posts/67485572.html","57a45c6d9964376c4e78cb38792f98bf"],["/posts/694347442.html","2f78d7e1a7b3618897582f4e13ec637c"],["/posts/707384687.html","f6d8364ede0b6d44656cfb27e3a34ecd"],["/posts/71180092.html","31702d3e2320c61c2ee6da5a441c1e72"],["/posts/716459272.html","dddc9c6cf6fac679eac72b963368524e"],["/posts/765481613.html","2aae4a9338cb63df39e51deaa836e2ea"],["/posts/778231993.html","cc2a08dbce2533836843823284377180"],["/posts/795397410.html","571e1b6fe480b2a80eee38b1914c61fa"],["/posts/820223701.html","1386737c3289e774471d45a7c4b38bc4"],["/posts/830372185.html","10dfc07994c7d1a72d6381b1294151ab"],["/posts/88294277.html","5bc174f085dee7c53ade05d3befc49b0"],["/posts/939963535.html","07d4f4640013ee7ed412f477fd80cb03"],["/posts/983786067.html","4ba6bca0fdd86ee2a0b56bfe3db6c4d0"],["/sw-register.js","084dab52b7b74c897f76c3fc67d24562"],["/tags/C/index.html","aaa2d513587a02f41e78e89ea09c45ba"],["/tags/C/page/2/index.html","c665731a874b27ce3a83722ee7fff714"],["/tags/C/page/3/index.html","575061e1f5e3fc32d1e62e83097cc645"],["/tags/C/page/4/index.html","5e5e4f0ba8fd44c259a389f9be622ec6"],["/tags/ETL/index.html","c4b1c609f416747ebc2f5d74b166b111"],["/tags/ElasticSearch/index.html","312ee18fd19c41c27dbf1905e670e858"],["/tags/GUI/index.html","00340a667db57d7f55db15f0be9f559f"],["/tags/HBase/index.html","0e0da00a952bcf19b4daac810a68aa90"],["/tags/Hadoop/index.html","50c37b4f012fb2e59c462b8efaa9ac69"],["/tags/Hadoop/page/2/index.html","dbb35854d358f366b79c06ba3fd61ce9"],["/tags/Java/index.html","6a40382ff6dfff83c7412790284e4417"],["/tags/Java/page/2/index.html","348617f92acbc58fae6748d1ee9b1f92"],["/tags/Java后端/index.html","b76835dcce378d78b8c3d556949770e8"],["/tags/Java后端/page/2/index.html","3407814fc0a733cd5b95f09eb6003671"],["/tags/Kettle/index.html","efc91302fef77cfc40aabd8d83bfc92a"],["/tags/Kibana/index.html","194033efc40fd25d0b0da0fce26a4231"],["/tags/Linux/index.html","868ce80b8770c647ea4a67ae298927f4"],["/tags/Linux/page/2/index.html","bb384111c5ceb8935f27f465983e15f0"],["/tags/Linux/page/3/index.html","84f09696ff7845d5994b677f86074019"],["/tags/Mac/index.html","0db01e293221625d0e5bdfa5642190cd"],["/tags/Mac/page/2/index.html","1241072a4a8536395d588375c5e2d0b6"],["/tags/Maven/index.html","0fa420c3b2973a18ecc2cb6bd2eaf917"],["/tags/MySQL/index.html","461567fae4b99a2da94792aa2f1f6c14"],["/tags/Python/index.html","3ab3c964d79b1b3a5074e096f17419f3"],["/tags/Redis/index.html","27c3a24def5a92ba2baf384b5d10070a"],["/tags/R语言/index.html","dab04e6841f6a0e21b1521c657e8bcf9"],["/tags/Spark/index.html","0fe80f7618a24c6e95d30f0368b59b29"],["/tags/Ubuntu/index.html","b0a60c5d8543d866d14884808b9ef04f"],["/tags/Vue/index.html","b2a6653f81e54596f0a6889fc5063848"],["/tags/Windows/index.html","9ae28d083357be551e7b90228fb2a911"],["/tags/ZooKeeper/index.html","8222a392597f615d1464dc8b418648b0"],["/tags/bfs/index.html","f20721617c1c541df7f0e25fad9d0d64"],["/tags/dfs/index.html","71d9abfc37ce4056cdc82ef34d76e198"],["/tags/folium/index.html","81a9c6bcc32cecabec1c881f4790ffd5"],["/tags/git/index.html","5ac55e2f904efc8599e55da361f2c602"],["/tags/iPad找电子书/index.html","398c4cfcd215e65b173c81d3b5926ef4"],["/tags/index.html","f313c51aea55a7778598b56c0e1a1af9"],["/tags/latex/index.html","90b7cf3061b37a3bf8cb3cebf7948436"],["/tags/中间件/index.html","bc74a3b6dadecd3f00c9d374e195e68f"],["/tags/二分查找/index.html","c9e016d744eb0ea8acab1f47f526459a"],["/tags/优化类/index.html","aed4f68ee3d06631820a80d4d2c6387a"],["/tags/前端/index.html","9c937057416d530e0f50e692283cdd5e"],["/tags/前缀和与差分/index.html","4cf6730f8b06f80c5d321fc93d9b2566"],["/tags/动态规划/index.html","fc40e63bf3026196d028f7b11be7449d"],["/tags/动态规划/page/2/index.html","4115ee953e21c972232737f4d86200c9"],["/tags/博客搭建/index.html","8b9d5b11f8214187af3f00afe8a7a8b5"],["/tags/图论/index.html","b68daee5567fff9aa31c66c6155919d7"],["/tags/图论/page/2/index.html","5e7345caafd22a17a44220faa2026b68"],["/tags/大数据/index.html","85cbf91284055937f9e52eb35d872d57"],["/tags/大数据/page/2/index.html","8a60377270c2c174759dd5acb21d7c21"],["/tags/宽度优先搜索算法/index.html","9a1a9f14944e03bd432e3858095e5119"],["/tags/排序/index.html","2ea83656d059f8f130d8cb1e43b57a87"],["/tags/操作系统/index.html","952f1df2fdd521eabe8aba2883ee492c"],["/tags/数学建模/index.html","23d446393e55d374f8c1cf203c66e410"],["/tags/数据库/index.html","a0167ef0f7d25879a929978461beeb76"],["/tags/数据结构和算法/index.html","792e2bb202d2a558330bab3297ead853"],["/tags/数据结构和算法/page/2/index.html","fb18b2da8dab476858ae46f893319bdd"],["/tags/数据结构和算法/page/3/index.html","2ace025187a079d500a8eba8d3634b08"],["/tags/数据结构和算法/page/4/index.html","eb1a0a1f6ad2e3f6e0842fff04441044"],["/tags/数据结构和算法/page/5/index.html","87c9c2f5db1c3141c8283b45ca35ccd4"],["/tags/数组和字符串/index.html","0abe56ce32f1a0f52124a182125367e2"],["/tags/数论/index.html","57719b8251bace23982f0e51710df831"],["/tags/枚举类/index.html","35c396eb363837c01a4f5ca31a55508a"],["/tags/栈和队列/index.html","867fa4ff05b72bc09a305fa792071593"],["/tags/树论/index.html","e2bc2e038695a90bfff8ee5ffe485163"],["/tags/测试/index.html","977a1830f8e0d5b2945b195e78b34882"],["/tags/深度优先搜索算法/index.html","10ffa900e822388052102e9fdb9fb4de"],["/tags/环境/index.html","43e92758a225acf26b2e1bf37cf8af97"],["/tags/环境变量/index.html","04477f879c352d1432b2bda9f19633fa"],["/tags/绘图/index.html","0f101791e9e852b486e3b86573e7f48d"],["/tags/编程工具/index.html","8285de8c9e47a943255545d535b8e6a0"],["/tags/编程环境/index.html","79418d0d1c123338cec2473613873d4b"],["/tags/网络编程/index.html","8e03bd4275277801b9d488d87c6d24e0"],["/tags/英语语法/index.html","666b751038569cf8babb0bc2625ea5c0"],["/tags/计算机操作系统/index.html","b62830af09994f85529e345295185c79"],["/tags/论文/index.html","62916657d8216729aa30d53d5b06cf8b"],["/tags/资源下载/index.html","5139f7d8f90558f81f8104eeed010620"],["/tags/链表/index.html","05ca0ae486441179fdc7e940dd1900be"],["/tags/集合/index.html","81de23f9da3cf8571a70d472ab391271"],["/tags/集群/index.html","9b582d8015f2ca7c2bd23837deb33be3"]];
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
