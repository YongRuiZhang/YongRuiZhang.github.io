/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ab06184ed6a5765e84fcdbdd5de0d8c1"],["/about/index.html","638ec9bdf44a97e00314d5da410cb077"],["/archives/2023/01/index.html","dbb95e4597863556d4608b53414ace1f"],["/archives/2023/02/index.html","c4332c6d32d42ad4fd8ffbad878fc9e8"],["/archives/2023/02/page/2/index.html","bf668de4a422a6867f4558f5930a20ea"],["/archives/2023/03/index.html","42d76ed12dd40e582c2f0a8d2c277a7d"],["/archives/2023/05/index.html","d576ea19e58351ab02edd853048885de"],["/archives/2023/06/index.html","eceb8457204d4ba746c3527b5d78bc06"],["/archives/2023/09/index.html","10bc4fd653e9216e0c948ef78b32214e"],["/archives/2023/11/index.html","1c4b6bed926acab350ca50b9316a6784"],["/archives/2023/12/index.html","96260b2e8169890c2357e399972c2afc"],["/archives/2023/index.html","b051b7b2255cf30d31ae40f220dda263"],["/archives/2023/page/2/index.html","d7219b4e21351e288e1be259c96b64a5"],["/archives/2023/page/3/index.html","0269424cbbd3381ea03a0ead320e1335"],["/archives/2023/page/4/index.html","6ddc964a9a90b6bc160883efd05eb9f5"],["/archives/2024/02/index.html","98cd1daaeeae4b7d8f279b89571588b2"],["/archives/2024/index.html","bd91ede808667a9b2980867002feeccb"],["/archives/index.html","3dd6384e28e159277edb00b6848a6163"],["/archives/page/2/index.html","0b152af47f75d3e571e1379b74690ae8"],["/archives/page/3/index.html","9293d8da1351a30b4a44f940e7d9bd17"],["/archives/page/4/index.html","4f008681073b69accc9d6b4a9b69dcfa"],["/baidu_verify_codeva-qQP2iZOMLX.html","3ea8d505455a9e0fdbc71275579530f0"],["/categories/Java/index.html","9ce399d765ba8bc8dca3b67c24c5ec66"],["/categories/Java/后端/index.html","0d92492e24a5ca840db80a5594738d57"],["/categories/Java/基础/index.html","ccf04b00b3e59f7bd791c14569aee937"],["/categories/Java/基础/集合/index.html","142af031f4c1b164eae0f2168c9bca29"],["/categories/Python/index.html","78efd2c3bc45c159c7bb77bb49d45742"],["/categories/Python/编程环境/index.html","a73f433330de62adac701df5b31d4fa5"],["/categories/R语言/index.html","235cbf918c4840ad5f87e8b839137160"],["/categories/R语言/编程环境/index.html","e2b045633cc5d91dc69d0633fd5a4362"],["/categories/index.html","43548fbc2c9cbc5b7eaf19e66fc4d40f"],["/categories/中间件/index.html","5343b1ad5bca0741b700d92bc1a0cb87"],["/categories/前端/Vue/index.html","8382617af5f2460e2c7648c377c19faf"],["/categories/前端/index.html","a0e3d20b3e19b603011d709c7e040834"],["/categories/大数据开发/ElasticSearch/index.html","ee87f05281073282aafac6797ab37083"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","9be83637f3df3d1a5bcb39f86ece6952"],["/categories/大数据开发/HBase/index.html","ee1a13418364d961874d7fd5cb348517"],["/categories/大数据开发/HBase/学习笔记/index.html","a96b5d7a1fd525bf7c86a4d58f4174fb"],["/categories/大数据开发/HBase/环境搭建/index.html","a598929951920a61098c60b60787d67b"],["/categories/大数据开发/Hadoop/index.html","cfd13fcc3152b0e415259cfaca4b575a"],["/categories/大数据开发/Hadoop/技术/index.html","0ec26f6820c022fe3b5b6995b0830958"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1587566481ed025dd941a6759e6ad2d8"],["/categories/大数据开发/Redis/index.html","272fb6c84aee87fbe835a0ecfafe3f44"],["/categories/大数据开发/Redis/技术/index.html","dcb4eaed45ad0e9cce45293df152dec8"],["/categories/大数据开发/Redis/环境搭建/index.html","a8ac29859a2f407a47eb86697c422883"],["/categories/大数据开发/Spark/index.html","80bc1f5e20a87babe293ed983c1a8d47"],["/categories/大数据开发/Spark/环境搭建/index.html","51b978f5f1a469a471f652f5ff1dc888"],["/categories/大数据开发/Zookeeper/index.html","dface36b24b9dd59d9654cd8b83a3110"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","d19ed08316267fc67436503970c154b8"],["/categories/大数据开发/index.html","bae13c52c76505543b30f5b2f76f842b"],["/categories/学校课程/index.html","39330963dc6533ff09fc4bec70a2312f"],["/categories/学校课程/计算机操作系统/index.html","2f820412591715b141905f7f6c885ecd"],["/categories/操作系统/Linux/index.html","a510e7de65c533572b18c387c67d84fe"],["/categories/操作系统/Mac/index.html","1a9e509b2afd45004ba0c813b964dd6e"],["/categories/操作系统/Windows/index.html","a64e895c041eb27048e43211c4f9fa19"],["/categories/操作系统/index.html","c2f51bfe0d96e5ab9fae8d1234fe75ed"],["/categories/数学建模/index.html","5eb5f5fef0debf532159f13f60518fe9"],["/categories/数学建模/latex/index.html","e79623cac409d44f5edb93c53ad53078"],["/categories/数学建模/优化类/index.html","39dd0f60200e6ccfcb5c9cd4df9b0ecc"],["/categories/数学建模/优化类/现代优化算法/index.html","3773d61a91306a2482e905a9c71a420b"],["/categories/数学建模/优化类/规划类/index.html","e13ed75e600d7632b79515ca1fb54757"],["/categories/数学建模/绘图/index.html","1d4ab7166581ba45ca2fb5380dd302b6"],["/categories/数据库/MySQL/index.html","14a3afddfc0a958985754fea6437d6aa"],["/categories/数据库/index.html","d49a3a55b0bbba7f666f8b21cadf0927"],["/categories/数据结构和算法/index.html","36954a797aa86a37eefb0aa8f25f6125"],["/categories/数据结构和算法/page/2/index.html","6e5dc30dac49e3543e0bc5a9712851cf"],["/categories/数据结构和算法/基本原理/bfs/index.html","f3f0087ec9563475829ff0f767eec07a"],["/categories/数据结构和算法/基本原理/dfs/index.html","19c2e86a3818f7c0cd80b24aa6d900b6"],["/categories/数据结构和算法/基本原理/index.html","128d055d24b4f1ccc2fbd3d3a51417d3"],["/categories/数据结构和算法/基本原理/动态规划/index.html","7a82a34d67e33bf706d8da2dd2920a63"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","1e37a3ee584b81a62c9371c6b680ed04"],["/categories/数据结构和算法/基本原理/图论/index.html","cc4b0ef5e86917cbc4ac78bf67819678"],["/categories/数据结构和算法/基本原理/字符串/index.html","a303a21611d7b7b3eed951437094156e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","37d5fa3f0b79ee3a12b37b27b4a5e08d"],["/categories/数据结构和算法/基本原理/数论/index.html","23378ea217da40f9b618c15d448f9076"],["/categories/数据结构和算法/基本原理/树论/index.html","b3a4c1a537ade76f872b2d5a491fec41"],["/categories/数据结构和算法/基本原理/链表/index.html","d3efb579d66c7eb73680af5dc87d83bd"],["/categories/数据结构和算法/算法题/index.html","df3223ebf1779ba62b0ca074e7f3057b"],["/categories/数据结构和算法/算法题/二分查找/index.html","3f5bbefe6c42630c0f2e39e1c95ec65d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1a8fa667b07b53fada5f92738dabdedc"],["/categories/数据结构和算法/算法题/动态规划/index.html","76604089b875158a9a66889eee59586b"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d5fe6105bf41d374b816016c9b0fe3cc"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","4d6ca4ce1dd6ade16e0d83326203c010"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","2fee25cb2dc3f3d09a939bc00e85989c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","b83a987e68d4e40526825f85ec0338c3"],["/categories/数据结构和算法/算法题/数论/index.html","9072104b3b0c58b4d9d5def312d78cac"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d43ad5d3d9404944c437d5875fa56744"],["/categories/数据结构和算法/算法题/树论/index.html","a2a75a6c0ee8459e40f9388f5ee42508"],["/categories/杂七杂八/index.html","265fb4bced794edb5aebd9fd68d3eb4b"],["/categories/杂七杂八/博客搭建/index.html","c95bcef720b057d20de48f2e0bbc3098"],["/categories/编程工具下载/index.html","5d86c7948444f5ac0178127320b5217e"],["/categories/编程环境/index.html","06985783c1055b6e595433f60edad334"],["/categories/编程环境/大数据/index.html","d629d5719fba62759d68a4c200fc499e"],["/categories/英语学习/index.html","6f47e1466853e5c4da9e301435863df9"],["/categories/英语学习/英语语法/index.html","5bf5776117ef72c237abfa4c3da1d3cf"],["/comments/index.html","2a9a44952d2afbc693c93e4550162dda"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","c96337d1b6ead12cd775d65d1feddeaa"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","3189bb253ab67529066ab9031e4bafcb"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","7b506c004b0de2238cafd898b1e479df"],["/movies/index.html","83cd8c0e9b7b301ed60122d08a9dacb2"],["/music/index.html","b83e9c7fa05147437ce128392977510e"],["/page/2/index.html","dd7d3b9b0a13c1772d4a8ae13b540ee7"],["/page/3/index.html","db1f55fd917e900aa9347e769457a2a9"],["/page/4/index.html","319ca770a5d9f6439fba5d77c2dd093b"],["/page/5/index.html","2f47fcd0a969106dcc9019e9c89f7fa9"],["/page/6/index.html","da745d865a0f826876e73e454f6a7085"],["/posts/1021360842.html","a6df2264faec1aa7a3f5db8e33f721a5"],["/posts/1120620192.html","7175133c0e67b49e58c175913487d1ae"],["/posts/1141628095.html","57da976724a9e6ecd544dbe1c40752f0"],["/posts/1168613674.html","e58073fdcb0063e42feb1afbc38c6181"],["/posts/1219920510.html","c5cf3e2619d01dd6836e5f65d08b1cfb"],["/posts/1222166338.html","e57913ae533494df1eb522f20d28fdd0"],["/posts/1259097482.html","e7dda1f8977d1c1dbf313d46ed10736e"],["/posts/1271036369.html","5767d288e3a565bcca43d152d0c9441c"],["/posts/1312847445.html","61624bcc56aed1d58cbda5fe16c7d342"],["/posts/135355774.html","60f3dab17c46ddd4fe443cf9a4840d7d"],["/posts/1375344716.html","9113254876d87e53d6cf84a5b87c5f29"],["/posts/1388991698.html","e7b716c776fe06f39563033449855e8c"],["/posts/1410315814.html","eb795351debafc74c93ab0a79bcdb798"],["/posts/1452790229.html","e3dbfaa3d4a704045f17f6e616ff179b"],["/posts/1470079884.html","c4c187643b89c97564bde8c170ecfdb8"],["/posts/1470079885.html","d4fda0730f7192c63970b4e68002ca08"],["/posts/1470079886.html","97f0e91338e9d8941282994594c44dbf"],["/posts/1470079887.html","9d380438ee2069436ba25e80c177a823"],["/posts/1498536549.html","952a57628a68c7aaefe53ca8fb34fa82"],["/posts/1539568593.html","1089eda769fc8bb7c7253ae888a370fe"],["/posts/1547067935.html","1328f591cc084e93378574657da88e42"],["/posts/1557866301.html","828e893487aa9017725c72776c442ea5"],["/posts/1571776361.html","fe83a69a874350eff80e0b308c0ab67f"],["/posts/1605124548.html","1f3391592c8d8df0065c28ca97c27e12"],["/posts/1633036852.html","8febac1c69c02ae4f65fbcc5c8e97fea"],["/posts/1674202625.html","ff873d89d65ec7b488c34fe5968e5f59"],["/posts/1765123828.html","f441f1a4240859e9ea1aee0314b2d5e9"],["/posts/1767336200.html","31804e846d1452fac5739d7f11e56bd4"],["/posts/1776114197.html","e4196d9e59811bf329036682c66bfe29"],["/posts/1817748743.html","ff44b6f623ea4e3afa3b38164ff6c772"],["/posts/1925125395.html","345cbf97c1027983c4f6f358788d6313"],["/posts/1966191251.html","076c080bb38dfefaf2f95c9d301f6069"],["/posts/1987617322.html","dbb8dcf8ea898789a81e26f055a06081"],["/posts/1999788039.html","8556292fc19b0ac662809621a7799ec1"],["/posts/2075104059.html","e9fb1807f7f2881c843fedceaf9ab2c0"],["/posts/2087796737.html","3d847f6f6f1ebe165c61c9211f44278b"],["/posts/2106547339.html","d27f32bc168f64835be51feb1c47e003"],["/posts/2207806286.html","eb8d4f8144abcde13329f20bd757f476"],["/posts/2225903441.html","a6eb817fbac27af1e880ffbf0112ab08"],["/posts/2265610284.html","37b95927190ec0288cde3224b65e24ed"],["/posts/2281352001.html","2104bc2b32eafaa8cdc466660257816c"],["/posts/2364755265.html","ea762aac58ed9d93b48a24b801ccd992"],["/posts/2414116852.html","bc33f4349f442eb6439057ca7f1ebf5a"],["/posts/2421785022.html","d6e58cea8d62534c48bdb05dad3111c3"],["/posts/2482902029.html","5300f7082b24f48f3ae2b306e70af8c1"],["/posts/2495386210.html","2c0f32a70704289efaa7dc25ece7aa54"],["/posts/2516528882.html","20872cad33674ded496e824d21431fd1"],["/posts/2526659543.html","9874943595127c6c0d2a6a050eb64518"],["/posts/2529807823.html","21f7f743f827c68baf94368c69a11738"],["/posts/2596601004.html","8dc3c022cf166b9113d6b1aa6aafc2cd"],["/posts/2697614349.html","304a598ce0a9aeed0a4d531f2ada79db"],["/posts/2742438348.html","373482193cda136f4a283eeaa0c8c075"],["/posts/2768249503.html","31368ccc30ce27e7a77952778aeb0749"],["/posts/2864584994.html","8701a6840329a45164324661b27e8f5c"],["/posts/2888309600.html","397cebd265eef5c5b701cc2001f5a81a"],["/posts/2891591958.html","db25fcc75b89f78cf7d6a54a33250621"],["/posts/2909934084.html","07d46b3494d607b915e848c0c65146e8"],["/posts/2920256992.html","a793b7387f70cbac721be7d332fd6756"],["/posts/2959474469.html","83dd8c4d364ef8d3aec4dc6b6c63039e"],["/posts/3005926051.html","8f85a434b54511224d4ea6c073b3d166"],["/posts/309775400.html","5f4e5a98268251b19036a74d36ddf7c5"],["/posts/3156194925.html","fac723fce06ad87fe759b33572f21f46"],["/posts/3169224211.html","ef532292540db69a5a0f9a9f1d2c5a0d"],["/posts/3213899550.html","f0428108684e297bf65f210d5e05becd"],["/posts/3259212833.html","8f7afd72ab22d07f99e7d7c12710b68e"],["/posts/3266130344.html","c8048a194ef933122dcce6d538a323d2"],["/posts/3292663995.html","7d5c0b37c705ed025023e3c3940f709d"],["/posts/3297135020.html","41991dd94f9329eb846ef0764d609490"],["/posts/3306641566.html","cdfb926b18a5bf1538a08af5cd59fec6"],["/posts/3312011324.html","e48ff9c66585d555abfe215b72a7eea2"],["/posts/336911618.html","d36b3ad49baa52c84dac64a068e36cd2"],["/posts/3402121571.html","f23aa9866d6380145b76ffae4cb9d4a9"],["/posts/3405577485.html","734db202aa79049dac38cdd5349c0458"],["/posts/3498516849.html","1329ee9fe32c7150edfe722a08fd89c0"],["/posts/3513711414.html","2a4856b6d7461f20634ee2d1c98f4c5e"],["/posts/3523095624.html","59299bc449e777ac9cc06bdd307e65e1"],["/posts/3546711884.html","dc6021b10ece350025c0083c036c4196"],["/posts/3731385230.html","67e1d0b95abd54d549bff486cdf2785c"],["/posts/3772089482.html","96ae3bb4a9c90184172fc82b3bf6e518"],["/posts/386609427.html","852b449d5e10f6d51b408eec0a66cfc5"],["/posts/4044235327.html","e31a345a76555b731215f50b2aaa9404"],["/posts/4115971639.html","e81eb53e4c7c5aa54da00bffa876145e"],["/posts/4130790367.html","ca34a92c78c3bb37e76f9084ea68452a"],["/posts/4131986683.html","a010d2edd436ad9581cca991fca4c28e"],["/posts/4177218757.html","54ba2d734dcb363fa83f1ceb5157ac40"],["/posts/4192183953.html","e1d0ee928f4efd1b6dff5507924c7d62"],["/posts/4261103898.html","7722efd23b09f19cd800f5a8f90576ad"],["/posts/469711973.html","5cfd4a2e1d9211c4861ea97906a5054a"],["/posts/482495853.html","9a2ffa2b04d75299cd2f1e6ddcf66c67"],["/posts/488247922.html","e79fac1677b5efb62de2da762eb9f6e1"],["/posts/517302816.html","1dc8d12121b8ea0caab5827288e7356c"],["/posts/570165348.html","86be039fceaa0e09d25c21f5f996c0d5"],["/posts/595890772.html","44f379279de2303fe7946a14ef7449cf"],["/posts/67485572.html","f472ff0496b77511effe9a841c417446"],["/posts/694347442.html","ebe41ac979d6d975dadb14b23b8ca81e"],["/posts/707384687.html","eead9fc440b975f295cca7b5e678e152"],["/posts/71180092.html","d3c00a2c80f718ca2c489decfae73726"],["/posts/716459272.html","26217c0c94571d3cab02cfe159bd38ca"],["/posts/765481613.html","29e727101ae411aadd20ebaeaec0f651"],["/posts/778231993.html","e09bb49fdae99eb2f9c8be221c2c8852"],["/posts/795397410.html","7200f0f2001863fa6149d06bab608c5a"],["/posts/820223701.html","bc4bb85867c943588487ac2b1b102d46"],["/posts/830372185.html","74e2171c3bc0155f1105149f99078ffe"],["/posts/88294277.html","dcb2dc25059ade583eaf66dbd0378277"],["/posts/939963535.html","678a2b0594e879280770b25d0be34f87"],["/posts/983786067.html","f526ebe99e00fab615a6ab4270339295"],["/sw-register.js","e06fa107151e6ab62edfb7bae583bc71"],["/tags/C/index.html","b7c3d880e73abc424b55c31a683e1347"],["/tags/C/page/2/index.html","820f3420e08b4cd2a732d818b0037138"],["/tags/C/page/3/index.html","4a6cc5546d26d2e54681e06424ecf478"],["/tags/C/page/4/index.html","c94108354323894befc57736b1d186ac"],["/tags/ETL/index.html","520f9b5f2eabc09a86b98fde46f7e893"],["/tags/ElasticSearch/index.html","d4152c2d18d6bf4b1fa744d65e27b9d1"],["/tags/GUI/index.html","ca452db55d57aa5d3dfcb1e37dde7ee4"],["/tags/HBase/index.html","a0bbb9c2e7f7ea753eb2645f8725ad4e"],["/tags/Hadoop/index.html","1e82387e938208de5718d0e727e46b81"],["/tags/Hadoop/page/2/index.html","b7026870973011e59e151b6423f803f4"],["/tags/Java/index.html","3de94d590462656ce88fc6daf402c8da"],["/tags/Java后端/index.html","3313fb99da16c4af5cf757aac5e3d416"],["/tags/Java后端/page/2/index.html","863fb047cf4637767d0616cc1b5c9515"],["/tags/Java基础/index.html","f4660fdefda924c75f40effe85d25823"],["/tags/Java基础/page/2/index.html","350b9f1c384fb3191347523b9692368b"],["/tags/Kettle/index.html","cbcae5d14a175f192e4db092619ff9bf"],["/tags/Kibana/index.html","11eb872feb81ca895d2490d440edd121"],["/tags/Linux/index.html","832503cc8d922842b8f5212d90aea11e"],["/tags/Linux/page/2/index.html","fac02822c49839bb9cf2df3b1131ae7f"],["/tags/Linux/page/3/index.html","56e11469ab0c194eb01cd75b8e3666db"],["/tags/Mac/index.html","808cbcd5e7bc88042383db6cb7816086"],["/tags/Mac/page/2/index.html","3f3106f38eab391bd687127cc2f3f6b0"],["/tags/Maven/index.html","42bdfed7d9acdfe5ed3da5736494c168"],["/tags/MySQL/index.html","f3ad61fd2ecf11c42812fc2203f24c9f"],["/tags/Python/index.html","a1e09ddad81c6611094eafea7aeacb44"],["/tags/Redis/index.html","22eea823c6738d434c4e2cf04847281e"],["/tags/R语言/index.html","4699f7643319ebca17efdc3666dff14a"],["/tags/Spark/index.html","e2f8ec7a592ce53df5658b8abdacbc6b"],["/tags/Ubuntu/index.html","d783354247b1307cc99ece1546abc223"],["/tags/Vue/index.html","62c211101fef04ba490d7bf9fb7854d1"],["/tags/Windows/index.html","b8238666a84955fee8c649b65ff6a53d"],["/tags/ZooKeeper/index.html","7b2a9c40dc31894ca4bd85e7290846e6"],["/tags/bfs/index.html","3896b7950ea2a81df312910c0f6cac45"],["/tags/dfs/index.html","1d64b231d0dd3321d29670d93e8ea180"],["/tags/folium/index.html","d1d4becde467fa645643dacc8d65b551"],["/tags/git/index.html","b8388ed0563c34299b142235ba1891e4"],["/tags/index.html","d86f41d7d2f1cb57db7fe522f8405e01"],["/tags/latex/index.html","4e33bcb82b399f276417c3913b422a4a"],["/tags/中间件/index.html","bdb0ac3cb5ebd868818415aae1a24891"],["/tags/二分查找/index.html","b66a7ab3fac2904b6c45604e7984f05e"],["/tags/优化类/index.html","819955552d6119bd326edc5f24baecd7"],["/tags/前端/index.html","06689c85f7a12ce21b97cecab9749a4f"],["/tags/前缀和与差分/index.html","48e5e2ff5897a94e5f961a1b483cf87c"],["/tags/动态规划/index.html","c96db61b4135cbafe9867297359a581b"],["/tags/动态规划/page/2/index.html","41a9ec992310427e025a863a08943acb"],["/tags/博客搭建/index.html","c919aca23384c002a6fdf2868fd1b919"],["/tags/图论/index.html","0fa72f62f58732be24495590b2159f5d"],["/tags/大数据/index.html","06eea5f0b9ab15db860e080cb783e39c"],["/tags/大数据/page/2/index.html","e3ae36cab6f5b776d56743e53788ff44"],["/tags/操作系统/index.html","1dda863f8b79409de3f64ca0106bf80c"],["/tags/数学建模/index.html","df6278d056606353a3dbdb2516b74cf6"],["/tags/数据库/index.html","1fc07138bbc2045653de00d21dfc5278"],["/tags/数据结构和算法/index.html","319aad9df62a36192c1d6232cfa6e525"],["/tags/数据结构和算法/page/2/index.html","b2c695867271934b844e0fcead0d6399"],["/tags/数据结构和算法/page/3/index.html","6c6721bc00cf79ffd32fb84f532bc631"],["/tags/数据结构和算法/page/4/index.html","ade580cbe48fe457c7aeea0a20dcc2d0"],["/tags/数组和字符串/index.html","1c01aeffeccb84f04e261d6c20b6dfe7"],["/tags/数论/index.html","dbf2e717269ec4c39d140774853ebbcd"],["/tags/枚举类/index.html","2e0fb6e98e585b65b340acd862e10d5f"],["/tags/栈和队列/index.html","87a87cc165a1597f48f05a63736b5194"],["/tags/树论/index.html","2ec02fdad50b9cb1062fd6b943e595c5"],["/tags/测试/index.html","ffad59d3c6dc4cb1af856a42077f77d4"],["/tags/环境/index.html","2a96747b32fc4602f934377776f040c2"],["/tags/环境变量/index.html","2a7bbede73140eacd17dd4e691be56f0"],["/tags/绘图/index.html","1f316d4defe8d25bdbcf75f6fa537fba"],["/tags/编程工具/index.html","561838bf7a5e536a4f6ca78cadb0317c"],["/tags/编程环境/index.html","d2fd051aa0cde7f5debf9cc2b43f3a0a"],["/tags/网络编程/index.html","3b5c4b2cc3606f26842d35584d7da26d"],["/tags/英语语法/index.html","2d7f177ec1691bfe8b559e589ad04ba4"],["/tags/计算机操作系统/index.html","9b44e36d0b31120656ed6b388fbb863a"],["/tags/论文/index.html","32b218fdfab7fc0743ac49eb4bbcde15"],["/tags/资源下载/index.html","7b02a2888c0b33d260f0ba0899d22a1c"],["/tags/链表/index.html","68a6b2a352f8c45f0b472e6d44d40e12"],["/tags/集合/index.html","29566bece0d32d8c6db23c1f0fbdc8e5"],["/tags/集群/index.html","16375ce1769e7c9e7b2a77dd20637646"]];
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
