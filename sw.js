/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","1b0dacb990eaa7fc8c3b54b65061261f"],["/about/index.html","17f540a8ea9f47246b87e6679e3d3bfd"],["/archives/2023/01/index.html","5e689f09a65656606aed95400d7708be"],["/archives/2023/02/index.html","7a96f82cefa94c441819c2a1d6c4ecf3"],["/archives/2023/02/page/2/index.html","785ee0fb717f13aa5da00949520cbca1"],["/archives/2023/03/index.html","4494bb60575a7a7db7f101fa8fede529"],["/archives/2023/05/index.html","b55d16096d40b0d4c7e5641d27dd57fa"],["/archives/2023/06/index.html","a8c54b62ab9aecc119d82fa8739eed17"],["/archives/2023/09/index.html","5ffcb939a10cd33182dcf4b58f778f77"],["/archives/2023/11/index.html","6cd380e0cc6273e5f6cae5e3d7276c93"],["/archives/2023/12/index.html","cb10f6315dcf1d5e26ba86fa99583324"],["/archives/2023/index.html","5021090c995ebe69b0c0eaf9f0ec359b"],["/archives/2023/page/2/index.html","29b6f0f29ae04c0db6d1f229237f1bfd"],["/archives/2023/page/3/index.html","ef9313cf36377e7d58b46ab8b5fa230c"],["/archives/2023/page/4/index.html","6b164999e1f45088c0548e206ff29a31"],["/archives/2024/02/index.html","4ecd580ba4645af508518068adb52fbc"],["/archives/2024/index.html","7fb4c14542391199b5b9b7359e81c0bc"],["/archives/index.html","c2fd403aa2d3a4fb2a58b6d12789391e"],["/archives/page/2/index.html","674cb977622a31dfafeeceb7a017fee1"],["/archives/page/3/index.html","94d530e3bfd7b2147b3a23e12e581313"],["/archives/page/4/index.html","35fae86b361b698df92433ea32686491"],["/baidu_verify_codeva-qQP2iZOMLX.html","b21621751d08b82d944f2deb87347b32"],["/categories/Java/index.html","a2eff6951d0d33a4fb9644aa3d8a89fe"],["/categories/Java/后端/index.html","c1b16bee2aeec345e6abafac43c93b56"],["/categories/Java/基础/index.html","247511a9e71df47cb0fa7e1df35a0fa9"],["/categories/Java/基础/集合/index.html","946b68e9e71e6642fecdff53aab08ec0"],["/categories/Python/index.html","b32d7c004305bbf7e94af004f215a237"],["/categories/Python/编程环境/index.html","9af4d770092a73bc355ca23e035fb785"],["/categories/R语言/index.html","d6d8cebfc602e46c48b8eeea684dc03d"],["/categories/R语言/编程环境/index.html","7c6f90b6e217ddc75ac0883965750e5d"],["/categories/index.html","ebcdbc217701295b374cabfc0d6e07b7"],["/categories/中间件/index.html","58a490dcda20d2e98c0beb56cb22fb27"],["/categories/前端/Vue/index.html","8b5e40ee70bdb204c094e0538578f15f"],["/categories/前端/index.html","38a546d6eb1dd41b41cd8721cb0d29ba"],["/categories/大数据开发/ElasticSearch/index.html","92b16c57561d45f1a0cb8c8be5e32065"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","b6cca9d4869c471828304e1b835673c0"],["/categories/大数据开发/HBase/index.html","77d033915083623f55d3d7fca3f8fbc9"],["/categories/大数据开发/HBase/学习笔记/index.html","6abe8948c6818db457a2bec7356391b9"],["/categories/大数据开发/HBase/环境搭建/index.html","8236f36088ff3644e04046e2732a1966"],["/categories/大数据开发/Hadoop/index.html","f5dc0d3ecad21ba5e36a1b99fd56678d"],["/categories/大数据开发/Hadoop/技术/index.html","c94430de054b260fcec1521f75d3b72b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","4733f211b99138ef656b63adc5307847"],["/categories/大数据开发/Redis/index.html","43d0c3eab6ff5933ef76c3c8054b4376"],["/categories/大数据开发/Redis/技术/index.html","abcb85db90f48b31b19b935fb2312f76"],["/categories/大数据开发/Redis/环境搭建/index.html","374333412bc2cc4b67dd59df84ae531c"],["/categories/大数据开发/Spark/index.html","0ae57182b50758a7cdfbc42b99ae1238"],["/categories/大数据开发/Spark/环境搭建/index.html","51b069fbf122b899127440660ea315df"],["/categories/大数据开发/Zookeeper/index.html","ad38ca72fb9cb3288fb28bf940c7c6d0"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","857c1d0bbc71245378a78a919f2bb1ac"],["/categories/大数据开发/index.html","5c636cac97b5e4440b2a157ecc6f1d0f"],["/categories/学校课程/index.html","9b29715c212fb6ffbf8837f08b18e618"],["/categories/学校课程/计算机操作系统/index.html","fbf0f54b68d1fd538d2086da33d71da7"],["/categories/操作系统/Linux/index.html","26fe44b609f187d05b3f2ca601e6c7f1"],["/categories/操作系统/Mac/index.html","11343f0aa3a46859fe5bf1d41814298a"],["/categories/操作系统/Windows/index.html","e07ce517e79b17fe166f43004bfde64f"],["/categories/操作系统/index.html","ab4928eb3acade27a27f611dcb275d92"],["/categories/数学建模/index.html","c5a82291d394c51c72e210bc6987aa95"],["/categories/数学建模/latex/index.html","d454665a03c03d8c80414a9e00275d4d"],["/categories/数学建模/优化类/index.html","8497ab56768917547bff917058138bb0"],["/categories/数学建模/优化类/现代优化算法/index.html","88e4fa45433c1ab00cced53bd778cbd3"],["/categories/数学建模/优化类/规划类/index.html","63ac45edb4a15320af9e37046972365c"],["/categories/数学建模/绘图/index.html","34f69068f8da63aace246995a8a842fa"],["/categories/数据库/MySQL/index.html","3b3f8fbd6c5bb8739753671d2e37bcfe"],["/categories/数据库/index.html","6b3e0da1c5e0aec11662bb2f0294342d"],["/categories/数据结构和算法/index.html","a76639dbab284502fe84c55ead4a3f89"],["/categories/数据结构和算法/page/2/index.html","446e9b04f2d8e3828de94ad97006fb9a"],["/categories/数据结构和算法/基本原理/bfs/index.html","c287f32e91a1c7f40f8674467c514cd6"],["/categories/数据结构和算法/基本原理/dfs/index.html","7237a9b35daa1635ee1c1e6a56916327"],["/categories/数据结构和算法/基本原理/index.html","146ede812c3c91b3bf07f3d48dd09c33"],["/categories/数据结构和算法/基本原理/动态规划/index.html","19f964ae83a0b7c25df5ce6eb4d3f523"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","bbcb36f3c72666c235af8d2fc1b306cc"],["/categories/数据结构和算法/基本原理/图论/index.html","b9efc48f4d57909999c3c00eb0727b71"],["/categories/数据结构和算法/基本原理/字符串/index.html","27318e4b6275ed89096aca6fef310513"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b654c852bec05bee94a81802395ef354"],["/categories/数据结构和算法/基本原理/数论/index.html","9eaf62c37d04244aca8fb00249822674"],["/categories/数据结构和算法/基本原理/树论/index.html","9d76d9dfe5a96079786a496f377ba719"],["/categories/数据结构和算法/基本原理/链表/index.html","b681d7ebe44b1b52b13d7b543e64a7d3"],["/categories/数据结构和算法/算法题/index.html","ebec5b37278470bac1387a9e955dee02"],["/categories/数据结构和算法/算法题/二分查找/index.html","f45fe5f6bb0738baba69829c8559c137"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","7423c556662879466e491daa76573f9c"],["/categories/数据结构和算法/算法题/动态规划/index.html","f1e56c229dbfb4577c1a1fe3e11702dd"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e88fbfd1dbe8e8d34cc2be47d10769c1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","904cf18d4390ce4da1d14120768f64a5"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b942b44a05128e81755f48e0ab061a3b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ef42a8f4e99b495475aef30415fe579e"],["/categories/数据结构和算法/算法题/数论/index.html","369bbfa874e97594d2b2f26dd01e4922"],["/categories/数据结构和算法/算法题/栈和队列/index.html","e1374ce7041e67316326cf85011296d8"],["/categories/数据结构和算法/算法题/树论/index.html","7eac24be555bebe7537ed58886736f71"],["/categories/杂七杂八/index.html","3551db150efd7a83c01ab4a1725f1d6f"],["/categories/杂七杂八/博客搭建/index.html","491c46c1fa5a3b2aab934ec394420240"],["/categories/编程工具下载/index.html","31177631689024caba8032e67a143a57"],["/categories/编程环境/index.html","c27b54f12e895c953fd934c3a3646220"],["/categories/编程环境/大数据/index.html","bed82a7ba232a4bf0cc171afca809d3e"],["/categories/英语学习/index.html","68388e47a9a17bfe75c4404df247ecce"],["/categories/英语学习/英语语法/index.html","9e5a57c9c1b26a9c01761d81e66a6643"],["/comments/index.html","e62f5048eec9ce49e02d1c57a9210d7d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","b007e130df7f8815f0212e845851cdb4"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","90a31b794951892013ab6a76ec4233eb"],["/movies/index.html","bcc012fb8fb06c9915e7562595de8989"],["/music/index.html","3e623e450af65916c9d2a6d7097b6f19"],["/page/2/index.html","f8df4fa1ca1b69d1958c3ef0fc52db50"],["/page/3/index.html","04b84d6748af16ead9666259005108f9"],["/page/4/index.html","cccb47c6c3e36479a3972074305ca9a1"],["/page/5/index.html","f0767ee8f7cd0de9b23e72ce1c6eeedf"],["/page/6/index.html","85f451964ccf98e75c126f14019a17c0"],["/posts/1021360842.html","dd3632575c51838c269ab8a49b4ca348"],["/posts/1120620192.html","8520d57ea14deb4fb4ce43ab0151f867"],["/posts/1141628095.html","3d455ab0b69c458760e01d440ccf274f"],["/posts/1168613674.html","84a6a7907b0916fb3f1a17b423efab03"],["/posts/1219920510.html","5197ac2ef3967f5d73f0354bd60661f7"],["/posts/1222166338.html","9d66787eae04afd5d5578b322aea6d17"],["/posts/1259097482.html","58aea62f7474305a6ac03cfab90a0694"],["/posts/1271036369.html","2a3a06c431104183f3f7e98212287379"],["/posts/1312847445.html","98d0406920151e9a655cb35b4d275897"],["/posts/135355774.html","e2a3d0e847f29a214504db0f19814e26"],["/posts/1375344716.html","039c72861ca760d9a2d11ed1f8b35573"],["/posts/1388991698.html","48d9e4c88d13bb7407f390667ba6190c"],["/posts/1410315814.html","b6585d79dec15dd11575ef359d8d4dc4"],["/posts/1452790229.html","183f4a86d22fbda46afb5a0023972435"],["/posts/1470079884.html","10bb05d1edc50083622104eee7833c13"],["/posts/1470079885.html","85d696fd88fa304c1770611af8d10551"],["/posts/1470079886.html","193eb7005497447516c7444152a38b71"],["/posts/1470079887.html","52bc520496205c31549e06462fae1144"],["/posts/1498536549.html","ec33b7a8cf00ed9a227962e170cf5f78"],["/posts/1539568593.html","1dd3ab8a7aa99f1c6b37fbc22e151fba"],["/posts/1547067935.html","951106506f0089415549b94dafd75899"],["/posts/1557866301.html","f4260441cf693f7cf4887e673c29b1d3"],["/posts/1571776361.html","efdca041978c6fa114e6c3431296012f"],["/posts/1605124548.html","73279538440f9b7ced1bbe72a5719db1"],["/posts/1633036852.html","f0d848880751b524328d61e02fa02bf3"],["/posts/1674202625.html","34859406fc06ad417b2dc5e9869718cc"],["/posts/1765123828.html","b8b5916e3831be10b04c5ac8b81cb759"],["/posts/1767336200.html","402bd4c9035688190fa8d99eec373379"],["/posts/1776114197.html","e90be7ffae3de31d9d2bc98df3830713"],["/posts/1817748743.html","23ac335db73b63473998a1836b42cf02"],["/posts/1925125395.html","4ed614e7629bb8be674ff3803421880d"],["/posts/1966191251.html","19f5cb96e5992c8230f6a6596e8237ac"],["/posts/1987617322.html","088831e3bce93430587eefbd5079b4e0"],["/posts/1999788039.html","5892e52db3750927a3f96b85961076d2"],["/posts/2075104059.html","34aa920c46bc9dd3d49ff7d91a425af7"],["/posts/2087796737.html","935f096fef83804f5d8e111a602a2244"],["/posts/2106547339.html","906277ab9843a8efd9c15271182f5af2"],["/posts/2207806286.html","df174ead4f18a6477fed5fee2bf0a617"],["/posts/2225903441.html","91133072baa7de2084dc33d9f9f7aa9c"],["/posts/2265610284.html","83a6ae82f90e8241066d149d05c89eb0"],["/posts/2281352001.html","e7f39a5d17e3115056210ae03573577e"],["/posts/2364755265.html","7a58f670839d4cbe3940db9d8907c0a9"],["/posts/2414116852.html","88b54232ccbfceab7a3bece502224ca6"],["/posts/2421785022.html","9841a624afbad9e049af68f2f7520ae0"],["/posts/2482902029.html","ba9edfdcf52ee5d26223408ff453c833"],["/posts/2495386210.html","eb663e081ce89c7185f2b09e0b4334fa"],["/posts/2516528882.html","2e9ee1ddb5eaa55a92ef87095140cde9"],["/posts/2526659543.html","38115a73bf7275b79a5adc1cec5e8e28"],["/posts/2529807823.html","7a332e069024623c63f765d2d118c11e"],["/posts/2596601004.html","4139b9f102a96375a2bce1bfba57397a"],["/posts/2697614349.html","64d64c9a46ca630bfe9286dbf2c93622"],["/posts/2742438348.html","6723bfde6b061459faafc5be585de925"],["/posts/2864584994.html","9ab1cf1befd90c43a9473ce95a82b6e4"],["/posts/2888309600.html","3a594334c3dc3123ccc1185ad7ee5ecf"],["/posts/2891591958.html","b81baddda83b95889a2698c06a407e59"],["/posts/2909934084.html","6190aa99d6766783eb79340c52f3b7fc"],["/posts/2920256992.html","6d39e48f4ba812cbc6386b82a1b2057a"],["/posts/2959474469.html","e6e2af046337fb5bc0bb85c834c34c32"],["/posts/3005926051.html","617cffd7921da62fee5bb411bd3c4b97"],["/posts/309775400.html","0e0cd02abec2012f201d346dba39151f"],["/posts/3156194925.html","46102133a42eb193dbe10f4049c6badf"],["/posts/3169224211.html","9c853079882a142ce1aeeeaad2250d57"],["/posts/3213899550.html","2ac83ebbfe81efa3b485327de4e53b38"],["/posts/3259212833.html","7b1bf301bf24bc7122d039ad25142b0b"],["/posts/3266130344.html","3ffd27194bb9f0a9c303664f32f75247"],["/posts/3292663995.html","fc476debdece7223443d75a5f4f6d97c"],["/posts/3297135020.html","1649be3ab29b28236f094c37e2b5da39"],["/posts/3306641566.html","9c02c196585abf59cf15c05a219feb57"],["/posts/3312011324.html","c75bf88fe1c3abdab42eead033514653"],["/posts/336911618.html","bcff86c4eba754d8b069b180b2a1783a"],["/posts/3402121571.html","f2c4d7b97141f928488bc4cd7d6bb2a5"],["/posts/3405577485.html","b75ad917443b04df38c4e7bcf42addf9"],["/posts/3498516849.html","f83fc2f043a75d8425abd2ef3de0082e"],["/posts/3513711414.html","28714af26ac690b79a02d96c91e371d5"],["/posts/3523095624.html","023baa0953c6e11dabdf34e201a89567"],["/posts/3546711884.html","2846e6bf081ad6883bd7a7cfa6c43123"],["/posts/3731385230.html","5a7cd71340204780cd6c8f4b0444cd0e"],["/posts/3772089482.html","635693be5aa2d41150c933940f1ed3f3"],["/posts/386609427.html","7568e268d28df863ca3b00e5cd6b6870"],["/posts/4044235327.html","a87c25a637c8b943b65b2868d2945537"],["/posts/4115971639.html","524a0ecb459c42b52a07c7a37208d9f2"],["/posts/4130790367.html","aa6821a9442fbf9c2cea01dd396d2908"],["/posts/4131986683.html","9e371bda2cdb95d7b040fd05e670962e"],["/posts/4177218757.html","044901995af0efcdc6cb0277c8d8b89e"],["/posts/4192183953.html","e9bec1dbe3b5785517eb2b67eb3558fe"],["/posts/4261103898.html","4f359c01c90868d7928f21ad16cb4742"],["/posts/469711973.html","fdeb6efad095096935300fc8471b8b74"],["/posts/482495853.html","af18929eb7cce97d26cb0002ec4186e2"],["/posts/488247922.html","99536ac22d739d8339d5e7e8344fda2c"],["/posts/517302816.html","3fed02cb60a542fdae3c7fa470d94781"],["/posts/570165348.html","6de42eabd59b3c4a53abe87f1f87ea47"],["/posts/595890772.html","91dc7de5b6fe92cd8062ee84cad62c94"],["/posts/67485572.html","d9fbb95838c450b83afd818ce96b66f4"],["/posts/694347442.html","9113b7d24323e163c9906ada538710fc"],["/posts/707384687.html","b0bdc3ad9d2472a48a8172ad20a51114"],["/posts/71180092.html","e9c64105948a811231c1f474e47ee36e"],["/posts/716459272.html","aa8b7fededfc43bb39249c5bf705b71e"],["/posts/765481613.html","dc1981653f1d5dba7abc85594ef522e5"],["/posts/778231993.html","0810babd6fcc2c4581c8ed50bd7e329b"],["/posts/795397410.html","4b154242eeb715b3b18f96f793e0bf13"],["/posts/820223701.html","465721ba2c52310410587c344d68977d"],["/posts/830372185.html","2cbfc1cceef270e415628e143cebc0d9"],["/posts/88294277.html","321d0eb76287df9fbe67b5790e583dcc"],["/posts/939963535.html","0810e0d9e1e0ff81ac08523001e91e28"],["/posts/983786067.html","68ba1f0d4bc1f3255dacc1278fb7accc"],["/sw-register.js","4c6bc25c89defe564b4d61f5354aa13e"],["/tags/C/index.html","de4465df39babb74f46b459b142659ad"],["/tags/C/page/2/index.html","1c4ca981ec0e28ac28c9add2c70bbd3c"],["/tags/C/page/3/index.html","a858c3d6ff0af292360999ecfe9d207e"],["/tags/C/page/4/index.html","e65ba727f10b1c7990d1f4ec04c07167"],["/tags/ETL/index.html","428ebfa65df0310baf5e3149cacdd5c0"],["/tags/ElasticSearch/index.html","cfc942bef02f8dc9108f289a1c8d3e03"],["/tags/GUI/index.html","82678bcb536c2cfab0528aea4742cead"],["/tags/HBase/index.html","6ebca8ea8b5899f5872d6dc54ced440f"],["/tags/Hadoop/index.html","faa4bb3556da1c55af58dd98e31ea7d1"],["/tags/Hadoop/page/2/index.html","ee18d872814636f757e991d05dbc6f52"],["/tags/Java/index.html","639875fc8e85e7e5b2ac08dd58ac3677"],["/tags/Java后端/index.html","ae8d349b3d7ca20f849f959d9157d995"],["/tags/Java后端/page/2/index.html","6a45b78b4c56e733203c03fef6f519d8"],["/tags/Java基础/index.html","2ce09c4c57f7857f259aa298b7fbd194"],["/tags/Java基础/page/2/index.html","639a5095895be773710e291d2d451d6d"],["/tags/Kettle/index.html","1f3ebbac8af881cfbbe946ab0558fad6"],["/tags/Kibana/index.html","3e5fcf5d9d9f8f7aaec1b358e480e92c"],["/tags/Linux/index.html","d090eaf9242bb2a5aab34f9869a0b55b"],["/tags/Linux/page/2/index.html","9872a72508fcefcb258e1aac96369c26"],["/tags/Linux/page/3/index.html","ec5340d13d0955ff7ef3291edd868601"],["/tags/Mac/index.html","1b156019e96a21b019b541c9e14dd798"],["/tags/Mac/page/2/index.html","d3f40cb14189788a83adb859e150435d"],["/tags/Maven/index.html","10dd0d21ffc792ad7c8e066c664e27e0"],["/tags/MySQL/index.html","6c008282640e1bc178700c9ccb5df673"],["/tags/Python/index.html","715863c7f5e768349ac93c656898a400"],["/tags/Redis/index.html","57116434a796f8569943005c4fe910eb"],["/tags/R语言/index.html","733412ddceec78e96479efed3997822e"],["/tags/Spark/index.html","fef5f3495b007a8a5188f54efa0b4caa"],["/tags/Ubuntu/index.html","e72e0d8e7b34eeaaf2ee692b3f9fb995"],["/tags/Vue/index.html","47edbba4362a01e742ab3a3051dd6031"],["/tags/Windows/index.html","2509f6546e469ae278a535e3c28760fe"],["/tags/ZooKeeper/index.html","e7ea57e44d92d326953b66bfff11efb8"],["/tags/bfs/index.html","b227657bed4c866ef9301c934c521fe5"],["/tags/dfs/index.html","9eb74e17d8cdec1378e8a661c1fc645b"],["/tags/folium/index.html","21fac738ad3d3625eadf02747500faa6"],["/tags/git/index.html","09ca11b4f649210259b7d068efcd6ce8"],["/tags/index.html","8fd9d4e2a87d48a14c18cebbcefde7f0"],["/tags/latex/index.html","24f4f39ce08662aa6d0e8ce6095867ab"],["/tags/中间件/index.html","a90995dff26cd18b9f5658214391cdff"],["/tags/二分查找/index.html","4773ebe817df74ed72ffb7f905d42e7c"],["/tags/优化类/index.html","3e518ff72c646a3308fd7e3d69b1f415"],["/tags/前端/index.html","aad3cb95d4dac84a3da7073932e9cfbb"],["/tags/前缀和与差分/index.html","3e48caaa40fcee559ede18c0f4ac3ab9"],["/tags/动态规划/index.html","16393ef0b68543e8a6549afab754b4e6"],["/tags/动态规划/page/2/index.html","4566198302e9e699ae1616d3dc68e905"],["/tags/博客搭建/index.html","ab2848bc0fd1077195f16afa1a2fff43"],["/tags/图论/index.html","f1c3eaf3717e85d63f541b202301bc1f"],["/tags/大数据/index.html","8209e4ef2824af1b673b6efed993e000"],["/tags/大数据/page/2/index.html","5153ddd9fc03f5971d04bd494d97df45"],["/tags/操作系统/index.html","05cf29668ec2ab0dbeb7e104e62bba3f"],["/tags/数学建模/index.html","43844864a68f222f62d6e86f1f1d5869"],["/tags/数据库/index.html","4a38daaf4cc557783ae2a6ca3fc8a88e"],["/tags/数据结构和算法/index.html","55f85a553737c7738bf8496b190bf677"],["/tags/数据结构和算法/page/2/index.html","a0ef0b398f0a09e6d1fd1e6fef0f24b1"],["/tags/数据结构和算法/page/3/index.html","e6caef5ef613ce40a3ba66b40d114938"],["/tags/数据结构和算法/page/4/index.html","68b31bf20cb6e935343447c84b655acc"],["/tags/数组和字符串/index.html","f025e524f126bebbf5688e9db2a2835e"],["/tags/数论/index.html","96761a6a4e4c2a813bb0d37e40bd59cb"],["/tags/枚举类/index.html","f2dafbb99f3233fbd06c3aa354c330dc"],["/tags/栈和队列/index.html","b614b5b4f2489f3ec2ce16624a165f93"],["/tags/树论/index.html","9053dc8f0f837c6df6478ad9372ae7cd"],["/tags/测试/index.html","9cba48ef1e5450c8c99e6e9c8ea01f4a"],["/tags/环境/index.html","6a9103c83906940719a87cf998c22f09"],["/tags/环境变量/index.html","4f89d68ab520fe556dc5524301c5dd74"],["/tags/绘图/index.html","b3cfe809810ecba03fc00da13dea4971"],["/tags/编程工具/index.html","b3d6033177193b40e2dd92f57d82c1d3"],["/tags/编程环境/index.html","748ff92b6438c9bdd57d9d6d833152c7"],["/tags/网络编程/index.html","8ed8ae4dc6ad862d943a257d35ecf6cd"],["/tags/英语语法/index.html","695a22cb7ebe2189aaaba0fef8fcb1d2"],["/tags/计算机操作系统/index.html","d0f2cf9e7a64e36a8fb2570eb9b0b441"],["/tags/论文/index.html","cb37c510e9ebd53b0634361a309df6c4"],["/tags/资源下载/index.html","356f3fe57c40783bcfbe0f4f4e130ede"],["/tags/链表/index.html","a78f7947b295a0ee96317912142ab45b"],["/tags/集合/index.html","ef080936ba3a4cd9d54e898ae50a3e59"],["/tags/集群/index.html","d10277a43775d13200e0b4b3f2fb4eaf"]];
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
