/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","9785b7edd1e01e2fd94d17f1356cfbea"],["/about/index.html","daf894f876fb62fe7d95818fba0c1ed5"],["/archives/2023/01/index.html","c8a768e273a9f700bd887144dcdd6907"],["/archives/2023/02/index.html","73f3b3217114c42ba42d2875a652c0ee"],["/archives/2023/02/page/2/index.html","4c995c73cafe5de945ad7f561a1545cc"],["/archives/2023/02/page/3/index.html","58f9f2a2db01b0c5184f19cc9da1117a"],["/archives/2023/03/index.html","51bb23727b687929ad82155b80c05850"],["/archives/2023/05/index.html","03fda00e73c9fc4c13a496a8b13daf0a"],["/archives/2023/06/index.html","8635a70bc3a0457691cccdf513021dfd"],["/archives/2023/09/index.html","95196fd663923ed1e61510d829799e2d"],["/archives/2023/11/index.html","95e2f24b4bd5cb58f0ae2ff921455338"],["/archives/2023/12/index.html","d29f464b5575852cb3d2c8ef15533db3"],["/archives/2023/index.html","1ff455aecad9c1eba0ef43a009111c2a"],["/archives/2023/page/2/index.html","5072d999ff401f8168f29023cf69c7aa"],["/archives/2023/page/3/index.html","6bc18b65e8996ab9f56422dbb3619abe"],["/archives/2023/page/4/index.html","a00558fece28ae1bbcbce65a28a33478"],["/archives/2023/page/5/index.html","9be315705a77a6af0c39931ba92093b3"],["/archives/2024/02/index.html","eec33e8c6ee44a3b366d2fe929e084a2"],["/archives/2024/index.html","b2ae24aced3a6d78fd50936592b63e6b"],["/archives/index.html","9bc33d3ddd54eb14cc6c5ab5088ed408"],["/archives/page/2/index.html","1872c9b37176bbd1aa5dd4c2813af959"],["/archives/page/3/index.html","2cf860b82555cb11462fd508bebf944b"],["/archives/page/4/index.html","fccc7977cf96044983e26dac2a65e2e3"],["/archives/page/5/index.html","3397a33f0a3e6430a42adebf66b8243f"],["/baidu_verify_codeva-qQP2iZOMLX.html","fd54c48d2bbc5e4ffcb414770138a467"],["/categories/Java/index.html","a12489dd01fc3cd6d0428672dc6c9aa9"],["/categories/Java/后端/index.html","dacd9b48ec375aa842e49322ebe236ef"],["/categories/Java/基础/index.html","6f08431ebaade2e7a22c07328ff0eb6d"],["/categories/Java/基础/集合/index.html","69dbc1d64737864cb9a9a18f39837e68"],["/categories/Python/index.html","2e3f0b894680edf2bdc24c760d8b68fe"],["/categories/Python/编程环境/index.html","64615db41a92078cd018775ad39b84be"],["/categories/R语言/index.html","dc509978b07a7d35caabbd7b6c793077"],["/categories/R语言/编程环境/index.html","360674a8bd0d7b4e82d22e5cef68a662"],["/categories/iPad/index.html","b44c748e34d94de0748e0923396c3941"],["/categories/index.html","e56e3156ed1b43e3d1d1d3a4288111e0"],["/categories/中间件/index.html","3bb286aa372eb3a96dc14117f686b895"],["/categories/前端/Vue/index.html","76a3a57b79f26a3533be7d88137a034e"],["/categories/前端/index.html","2557f23a3fb08b25b05651273fc05713"],["/categories/大数据开发/ElasticSearch/index.html","0614b44cd8985a59e48bfa76de8cf8b3"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","9f564dd781c4b954273940e9f3f54ae8"],["/categories/大数据开发/HBase/index.html","7ed12bddfedc763a01466109cac13169"],["/categories/大数据开发/HBase/学习笔记/index.html","ca60e472ee692a37ee3188f9e1fdfb5e"],["/categories/大数据开发/HBase/环境搭建/index.html","4b0fb232429ef6b127119b7bdf3e1d5f"],["/categories/大数据开发/Hadoop/index.html","35cf83e23fbf7fc4a86e29948ec285c6"],["/categories/大数据开发/Hadoop/技术/index.html","23b023eb62e70dedbd4b80a3a8b6ad9e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","80669768eb2eb3cbde530da71e2bb581"],["/categories/大数据开发/Redis/index.html","577ac4708f8286981e9108ca45624019"],["/categories/大数据开发/Redis/技术/index.html","d6621da21d5fe22087d525f806c1c21b"],["/categories/大数据开发/Redis/环境搭建/index.html","c723bac2b9028251449c613f48f61fb1"],["/categories/大数据开发/Spark/index.html","5d21a803b679c495719cbac2ccb4d830"],["/categories/大数据开发/Spark/环境搭建/index.html","f05dcd19e790e6d71220af6d4075df25"],["/categories/大数据开发/Zookeeper/index.html","8e0fe6b9f3ace6d6f013e3045656821d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","168129f974eb992b78731856a7cdb8e9"],["/categories/大数据开发/index.html","46677d851510e364123090f7f88ed189"],["/categories/学校课程/index.html","3c934f9b6f0ac1a3822582285d7fad51"],["/categories/学校课程/计算机操作系统/index.html","0bd8c04b02575b8b94239b1664a69136"],["/categories/操作系统/Linux/index.html","72e5fa8a92b5b27e2c24efa353a34948"],["/categories/操作系统/Mac/index.html","a3da80034e0b5347df44a6681946df8d"],["/categories/操作系统/Windows/index.html","61e4b83497f5df974befcd904109934d"],["/categories/操作系统/index.html","fb9d487e16ca94157cccad9b64c1303a"],["/categories/数学建模/index.html","b0589d0c2b054cf765e2f024fccd7835"],["/categories/数学建模/latex/index.html","b9a710a6cb17cd88ffb846219d5a6364"],["/categories/数学建模/优化类/index.html","01badd8e9bf706c8149796b85ceae4c9"],["/categories/数学建模/优化类/现代优化算法/index.html","53b5489476ba63c1e0ce9fc1c4923962"],["/categories/数学建模/优化类/规划类/index.html","d069929608237be5f065931eddfe9c45"],["/categories/数学建模/绘图/index.html","7f47166dcdf3573067b63819c97be2aa"],["/categories/数据库/MySQL/index.html","9a1932455f4735a075d551e33b867088"],["/categories/数据库/index.html","8fe1b9434d0f237b571751a06c1d6cda"],["/categories/数据结构和算法/index.html","0d2a22a8dd784590ca3c85d64d56b3ed"],["/categories/数据结构和算法/page/2/index.html","04fc8069154b5ac73eae65c6e0f5ca8c"],["/categories/数据结构和算法/基本原理/bfs/index.html","83422b1097e4da5bb3e96267cc31cc2d"],["/categories/数据结构和算法/基本原理/dfs/index.html","d268ac1ce01a67acc90fef23d7f5d2b2"],["/categories/数据结构和算法/基本原理/index.html","0d8a228641619de068bfdc375eb9a785"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c14c22f169f0157d40f8210f2796faf8"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","1e3634b2557ab00076925baf54e50a3c"],["/categories/数据结构和算法/基本原理/图论/index.html","e0c3dad41a7b890c33c3dbe218050326"],["/categories/数据结构和算法/基本原理/字符串/index.html","261dd05830c7fa741bc92b4729d80d5d"],["/categories/数据结构和算法/基本原理/排序/index.html","c867a9e0b6c33d934e0d8d7a8ac6be96"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ac1b07283a164759d4577b6fbc803787"],["/categories/数据结构和算法/基本原理/数论/index.html","e16b29394d0f1cf1e4ccb2431b8d9db1"],["/categories/数据结构和算法/基本原理/树论/index.html","3291916223c5f8389e90d32afe03e7de"],["/categories/数据结构和算法/基本原理/链表/index.html","20ac07733bf6099f57824deaba23527a"],["/categories/数据结构和算法/算法题/index.html","9b90feafa84ffc8f4fcc46e3d7a0217d"],["/categories/数据结构和算法/算法题/二分查找/index.html","3e059340d59e551c8c9c6fd04c19203f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","b53c996227737f4612d94728d8627b54"],["/categories/数据结构和算法/算法题/动态规划/index.html","55bb99bbbc32ffced700ce222d35d63c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a2b49c200425a385336b9c290e93c293"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5e4a76d554ffdec717c5f06ab5c670d7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","5b63ff96ef51c0e960985c9cded46352"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","a940e1a7065b5c13ea4b22868da2ce66"],["/categories/数据结构和算法/算法题/数论/index.html","bbcc91ad5dbef5dd37530d661f1080ce"],["/categories/数据结构和算法/算法题/栈和队列/index.html","65da872b9fbbde01c3a246126d91be19"],["/categories/数据结构和算法/算法题/树论/index.html","010f125d439ce149f0886315678793e2"],["/categories/杂七杂八/index.html","e14e549491fca8f022579dc84864ed67"],["/categories/杂七杂八/博客搭建/index.html","cac37c1d284af90c71b691dacc8e671c"],["/categories/编程工具下载/index.html","318a24b384cbd448b62bcb9dfee1564d"],["/categories/编程环境/index.html","3654819d0be3a86d01d7e45b716a6c8e"],["/categories/编程环境/大数据/index.html","27e3c8fe00ad289d977670705fa263f2"],["/categories/英语学习/index.html","3fdcb4d359c237e3b6c60e196be02296"],["/categories/英语学习/英语语法/index.html","012d138caf0e890d88bf9e0c1dd2e9b4"],["/comments/index.html","7942f537d3aaf4b627add146ea998fd8"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","d3b08e0ff870f1704ac805a09791e2dc"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","2eab74971878ab6b1ec74de0920b18fa"],["/movies/index.html","a63eeb35164b278ed43914033080d037"],["/music/index.html","6ea4d8fa744d5f57ec93015335cdb8d3"],["/page/2/index.html","c12ca5cd6c78fefae2df8bdffeb41e91"],["/page/3/index.html","65e273b05edeaf0ff4d1d091c38911db"],["/page/4/index.html","07339537b60d09e5ee840c55f914dd33"],["/page/5/index.html","87ec781ecdd0dd8f7ef6f2d9de9f25e5"],["/page/6/index.html","b8b91ba62519d1393104e599bb706d09"],["/page/7/index.html","ec6fed539e36f7d8848697401189d302"],["/posts/1021360842.html","456e822585c1edf1a0785b93e251fbbc"],["/posts/1120620192.html","cc9bf28b2cb98f48fa74ff9155c039eb"],["/posts/1137707673.html","ec217a700fd51ee9bf109fd96fbe8d4a"],["/posts/1141628095.html","9af29bb4a075609c5a7c8a92fee6275c"],["/posts/1168613674.html","e0cfe2c078d475688b19fd17186784d4"],["/posts/1219920510.html","2d2f71eeaa04f20d97a8530f8ed1b1a9"],["/posts/1222166338.html","19c76bd7b665b95d83a1baad32cc0c20"],["/posts/1259097482.html","b82ba80b3fdc5f59f43d6fd84c83e3c8"],["/posts/1271036369.html","98b3a6a6b92743c797f076a2e3648451"],["/posts/1312847445.html","d777d353196da3c9b78d546b0c8d87fb"],["/posts/135355774.html","a2f6b3e8bfdc9e9b0b8a4a14278bdbde"],["/posts/1375344716.html","a791cda73fcd4b0d2b55ebd516bbc3a3"],["/posts/1388991698.html","393b9c1d5f8b531dbe25fdad088cb281"],["/posts/1410315814.html","abff499c6e2bb5d8e0cb2cc768870f93"],["/posts/1452790229.html","de3d26b42006f54ca31ad5e117b83961"],["/posts/1470079884.html","d933a5777f86c5e1ee57ff37e6f6a0da"],["/posts/1470079885.html","c4202cdc6f81b9cf3c7f21e54ba15e59"],["/posts/1470079886.html","50d724869f2433bf0f396387c6ce2a9b"],["/posts/1470079887.html","13136c68c19037e09dc4621d0fa61b19"],["/posts/1498536549.html","9e9942f425bfc78e446d367163b64d90"],["/posts/1539568593.html","5ddc25915065aa9545b17f16532c0771"],["/posts/1547067935.html","cba62abb21869a612142ba89f0937232"],["/posts/1557866301.html","d8a02bcecb964d19983b6576fb92be6b"],["/posts/1571776361.html","896de259ed8f86d5cd7055ca06fdade6"],["/posts/1605124548.html","d623eac3dfc123a10a1a961a89ff308b"],["/posts/1633036852.html","b66078555d8590a7fef81e62cb9dccbb"],["/posts/1667740714.html","4a9e0ff22ca1fac490146df1db8bf019"],["/posts/1674202625.html","98a6583b84cbf4f3bafb54113279cbf4"],["/posts/1765123828.html","34845a3872a86635fc27075a8421a13f"],["/posts/1767336200.html","ce06f7b365d97028e4370083089760f4"],["/posts/1776114197.html","df22b1fd7054c247c76167855a1d7a23"],["/posts/1817748743.html","06303189eae68dd7e511605e4f97a832"],["/posts/1925125395.html","21c1ccd38b34e40df08dca5f41a1974a"],["/posts/1966191251.html","ef99434c4ce6ef854d62dcad6d1b1f8e"],["/posts/1987617322.html","564ef2ccc65cc13f7dad1a0be9f931e4"],["/posts/1999788039.html","d73e67a8968ec1de4afc524370062c2b"],["/posts/2075104059.html","db4205555bd0cbbf706f15ad9f2826ea"],["/posts/2087796737.html","8da8aa5588fe93f8722fc4a3769201bb"],["/posts/2106547339.html","9f456ae857e958332a9f65545fa07c7e"],["/posts/2207806286.html","6fc22a22524e8f74eb60df9f12282c23"],["/posts/2225903441.html","3bfc70be3cbd0c0b43d825a33fce31e5"],["/posts/2265610284.html","db9124c8332611073aa9b9e261e7f615"],["/posts/2281352001.html","c3a94d205decc87b040c05a46dafb162"],["/posts/2364755265.html","250dda46ff57d7547bd7d82557e59a4c"],["/posts/2414116852.html","cec9688cf86b5b4f07485c9b6d46e98e"],["/posts/2421785022.html","0714e37d049de351245d5b8450e3d852"],["/posts/2482902029.html","882c7cfed34316a42d8177a4b57a90dd"],["/posts/2495386210.html","089f6c30d96bf57991df473722fb11d4"],["/posts/2516528882.html","b2cee83b680b8ee8f7da7b2362ffa2f7"],["/posts/2522177458.html","3f7ad6498bd973f0946e05b84c552ce5"],["/posts/2526659543.html","f36ece78311ea728e4787a74088e8189"],["/posts/2529807823.html","b04ed32c2359b6c10a05fbce8ac5c1f6"],["/posts/2596601004.html","5d2f3eac49eb24a0706627a7d8e95e98"],["/posts/2697614349.html","e0e27463496a43dc6fa4892567c17066"],["/posts/2742438348.html","581fa55369d14d5844439189070e8751"],["/posts/2768249503.html","0c3c3002208d8fc72224ef2022e78931"],["/posts/2864584994.html","9ed17ff9b0c30baf03036f39570684c0"],["/posts/2888309600.html","36c0d25e7187596d96ac856003f53087"],["/posts/2891591958.html","f4a897b6f87a1b1d6065baa4253da868"],["/posts/2909934084.html","364a997519565b196404c77e54c6e5b9"],["/posts/2920256992.html","d8910fc5d5e3750ebd17defc88bec0a1"],["/posts/2959474469.html","f1f442d37a05b7a606299427dcceebc1"],["/posts/3005926051.html","6a5c134a5e8f920b8c7149350dc70114"],["/posts/309775400.html","2ca1f4b1109ed9eb0037877ee2c216cf"],["/posts/3156194925.html","f2aced4df868af313444440c5b7c9afa"],["/posts/3169224211.html","f3cda7ccd5e099f3c3c04442509e17ab"],["/posts/3213899550.html","dc6202d986834ba3f45285cd518dc268"],["/posts/3259212833.html","170e7d9dc9f6f9747e536325830d5b24"],["/posts/3265658309.html","54a504f6935eb0b73d987a55391e60c1"],["/posts/3266130344.html","1a728ea1cf60d23f3bcb155e4c7aaf91"],["/posts/3292663995.html","27e992090e9d273f9ee1509a8784d4e2"],["/posts/3297135020.html","8d3ea1195d4ea708fb8eb31443a99304"],["/posts/3306641566.html","bbd97bf3ef2c747245584c33d7242e0e"],["/posts/3312011324.html","49f08507c773dcecaa4548d44026c5ab"],["/posts/336911618.html","df591d68aa7aff5f73f228946e719756"],["/posts/3402121571.html","3c6c5b32962fdb2ba63b8fc0e57d6587"],["/posts/3405577485.html","72987af02f8d9dd1fe44c1229db6db62"],["/posts/3498516849.html","3bcd728fb92b656aac0e7ed523eb46da"],["/posts/350679531.html","0e5cca3c57d21b823232d92ae46669fe"],["/posts/3513711414.html","1092411f633d2d5b6395b22c1a199233"],["/posts/3523095624.html","a6c7dd071d90ae1b33b80e2c73a14954"],["/posts/3546711884.html","ab92d1a7bf62acc45c908ce8f06764bf"],["/posts/362397694.html","f51d44fc6d9d000a3f64ee0eb9c675c6"],["/posts/3731385230.html","9b66c0ab224cee547836c5436138912a"],["/posts/3772089482.html","e8bacbc4cbee0455f7009d961b7debb9"],["/posts/386609427.html","a6b9a295f0759c41e0d91bece81feca7"],["/posts/4044235327.html","85a40686c0cea3b12abbf38fd3e62079"],["/posts/4115971639.html","a9e37bd2a92d4ec60f887f6197d84eb5"],["/posts/4130790367.html","05324be603f581de82fa4cf999da5a3f"],["/posts/4131986683.html","f44b5d304da4c893e4f9bc807e1141b0"],["/posts/4177218757.html","e29a170dc0dec61a47dedbe9538f2e4b"],["/posts/4192183953.html","12b2049cff74001c8878056f435a32f8"],["/posts/4223662913.html","74be574f5040be01c4535309c1c5f8b0"],["/posts/4261103898.html","cab075307c1e771c0b73b5c573a7442f"],["/posts/4286605504.html","84015eed03c9550a643633bbf57d3bba"],["/posts/449089913.html","99cf5e3d7ea5e860b0c8a6c18a54e29c"],["/posts/469711973.html","8749bde213156426f1d03d8317e10b45"],["/posts/482495853.html","93e97a119b100ede4f4bd53a9df55878"],["/posts/488247922.html","7a7b801c7951c6c433ae8450f9e73850"],["/posts/517302816.html","7e55bf3a689c7932c4a91ac96af40d2f"],["/posts/570165348.html","f3c85a1b1acb387364fe0d61653263c0"],["/posts/595890772.html","dda58385bf40389d781af092b1ccf995"],["/posts/67485572.html","a19276492c1de0d39bba628fd1dbf138"],["/posts/694347442.html","610f57d3d673ef236325913a9e6846b3"],["/posts/707384687.html","22b43b6ca676d9bc575fe88dc78a5faa"],["/posts/71180092.html","68f32aebc289043d0ed055db7d0c0127"],["/posts/716459272.html","f9cf661a81cbc16b2271a237546cc9d7"],["/posts/765481613.html","f2a20c282e32494c3fb8ea664a8ca8b2"],["/posts/778231993.html","290069f186b26681af99919203662877"],["/posts/795397410.html","4aaa629b0fc0ae76b0263b81b0f3b5b2"],["/posts/820223701.html","34f3504cd20ec1a3b82cad8574de6f3e"],["/posts/830372185.html","0f1579dd95415e1e112ff058ee890dcb"],["/posts/88294277.html","2587ac37ccdb5450d33f9e6df5e0f7c0"],["/posts/939963535.html","bd939842e5f522b78722667a93d57a5e"],["/posts/983786067.html","54d9e3cd18899aa78197b27f315c2e49"],["/sw-register.js","faff653c58ab45c3b2a425216c7dc884"],["/tags/C/index.html","931257324cfdace76f4c45d3c0655910"],["/tags/C/page/2/index.html","c9b8370cb5f0f86167d95cef3ca2cecf"],["/tags/C/page/3/index.html","945c1a3a850084f02f0b0d764e654273"],["/tags/C/page/4/index.html","f51e57a90532cfcd1d4c5b42e73f402e"],["/tags/ETL/index.html","3cc1f92852dc52c78436cac51da96690"],["/tags/ElasticSearch/index.html","ddff08b31045c7ab1d547ede63b118eb"],["/tags/GUI/index.html","0b4970b22abbf76b8d5cf473fb78f79a"],["/tags/HBase/index.html","d698f9ff528e7d92062e0aaeaadafd08"],["/tags/Hadoop/index.html","ef562efc5ca9836f11f791bf2b9dda12"],["/tags/Hadoop/page/2/index.html","027a9cc1a4f1b42d8d9bea2cf51bdafb"],["/tags/Java/index.html","6f4fb5efc6b7c02904e016fb612f93ca"],["/tags/Java后端/index.html","1e8953b658dd5e032bf10374330e9d59"],["/tags/Java后端/page/2/index.html","9d6299f82de6852a12f5e34130711768"],["/tags/Java基础/index.html","ce4e927415dbb55a57631e13e34a7866"],["/tags/Java基础/page/2/index.html","0ae1e0bce01b41945f5358034ef49b83"],["/tags/Kettle/index.html","6f59eba691ba5d27675a9122afb1d928"],["/tags/Kibana/index.html","6fb1ad021e7104f013615ec2196cf0c2"],["/tags/Linux/index.html","28873a36af15adff6faa2cc845ac5be8"],["/tags/Linux/page/2/index.html","587997ea079293141a4295c8088ab00e"],["/tags/Linux/page/3/index.html","74e279b860fe38e268f7d0460341eaed"],["/tags/Mac/index.html","68a5763fca01ea61e0bd6e9560606dc2"],["/tags/Mac/page/2/index.html","df6b3e21513dfaedd5ffa9666532afe0"],["/tags/Maven/index.html","30efc8d7fce3226283bf7f0497667c77"],["/tags/MySQL/index.html","90e267c1bee4584b60d2d5e2165b0bd7"],["/tags/Python/index.html","d6da19992d57e1e841a4c4143869a227"],["/tags/Redis/index.html","9fd4c078407ff8be2d41e0abe7afa747"],["/tags/R语言/index.html","38104b3b049666ea5be3c130b9b86dce"],["/tags/Spark/index.html","486670e3406c16efdd8c0078c2dc07fb"],["/tags/Ubuntu/index.html","40bc1cc978621a7afdce7075e4db4d26"],["/tags/Vue/index.html","1d74f6293db932a48256360d11c0f92c"],["/tags/Windows/index.html","156c8457881baccd351914cd436bfb5b"],["/tags/ZooKeeper/index.html","14fd8cdb173aead0f52fb71fc138037b"],["/tags/bfs/index.html","421be37dde33e296ddba884b6da69724"],["/tags/dfs/index.html","ed720c022d17661194cfb2dda516145b"],["/tags/folium/index.html","ad17dfaeac4e30b402a4ad3066a4ae2c"],["/tags/git/index.html","2b658f656b53128aacf0658aca94bfac"],["/tags/iPad找电子书/index.html","20356df0bf31953a2670226b49959866"],["/tags/index.html","c1472902842da3645c44e7a94b65af2a"],["/tags/latex/index.html","326d6398dfdbc402484ff6c1325e8b46"],["/tags/中间件/index.html","1a7ce6022c9a48497be8f731aea7f7e7"],["/tags/二分查找/index.html","a9aa29c21d3e7193186b41ba94101496"],["/tags/优化类/index.html","c392ca228b1f8197c646ba601d51a791"],["/tags/前端/index.html","3306edf389a58b7365f2305f65c62ac1"],["/tags/前缀和与差分/index.html","35a9d66ed765076a464b5ae09cf32728"],["/tags/动态规划/index.html","711fbf7cb4860a97b4dc1b2327bf0df7"],["/tags/动态规划/page/2/index.html","76f2846814fcced2df305f3a173795a7"],["/tags/博客搭建/index.html","e6bc5caa168074d274375cb5e65644f0"],["/tags/图论/index.html","3674f95730d0ce747c873dff31050f66"],["/tags/大数据/index.html","85bd52e6c51e07a3b7f244563643a87b"],["/tags/大数据/page/2/index.html","15e766250a696706ee804dedc01393a0"],["/tags/排序/index.html","9b64f82a2a3d09f92e8395fb10a05b17"],["/tags/操作系统/index.html","c47c4bb71542d1db1338d01de8573405"],["/tags/数学建模/index.html","462799920095292d8d7bccccb26e1a2b"],["/tags/数据库/index.html","f3d36d57c7fdb0d8257eb1e6c90f36fa"],["/tags/数据结构和算法/index.html","e8ce85ec2e135b9ca929af00887dc0ad"],["/tags/数据结构和算法/page/2/index.html","97e2d6e705b208d323dd72fbeddba39f"],["/tags/数据结构和算法/page/3/index.html","9854d540a38fbfc75b3e97f1e6feeea2"],["/tags/数据结构和算法/page/4/index.html","1818385a18acfd364b343d15bf1aa870"],["/tags/数据结构和算法/page/5/index.html","1815c79ebc17c5753d86868a001d330c"],["/tags/数组和字符串/index.html","2b6c302f010babfc655f8bfdaa5a003d"],["/tags/数论/index.html","f1b92981a9467645785b1cadb7b724d7"],["/tags/枚举类/index.html","006971136851606d08d7a9b76b0814e5"],["/tags/栈和队列/index.html","40686c6f7ab3366a285622c3b3120bc8"],["/tags/树论/index.html","e64972a942909d2e0e2df43bb75e82cb"],["/tags/测试/index.html","099db7ab8eb9ce438f4dfcc5594f9aac"],["/tags/环境/index.html","9fc3419622c06c116a284616641cd70f"],["/tags/环境变量/index.html","77757c02e576853993e3c8213c7cfd0b"],["/tags/绘图/index.html","805e114c006c91096b42bc9366a2f687"],["/tags/编程工具/index.html","697c02d53b39f6232bdd596639dc2d54"],["/tags/编程环境/index.html","f9b06b5bf31e2448625322da8121482e"],["/tags/网络编程/index.html","051bad755623563df3c8fce876227d6c"],["/tags/英语语法/index.html","37350a21c2f6fba58be8d3a9ff05c9c1"],["/tags/计算机操作系统/index.html","c60d06d7d147d56b06b9a1b671c3d5dd"],["/tags/论文/index.html","a5d0855b8a61b570609704bedf5d94a4"],["/tags/资源下载/index.html","7ea204728808224d0a0fbfc1782957c8"],["/tags/链表/index.html","1752e8a41a750086fa8b5de5f5593ab0"],["/tags/集合/index.html","c981cdb2dc15bad5799edf030518306a"],["/tags/集群/index.html","0b38edc71f6ba240f2742a1fbe138bf7"]];
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
