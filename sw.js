/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","acdb0425e5c6eeb49288362f30ace619"],["/about/index.html","30a13bdcc2291731ee6947dd68ac76fd"],["/archives/2023/01/index.html","2b9412cfc69926c24e30eebe00ad064d"],["/archives/2023/02/index.html","d8d16ae2d9c536b66ac9987b4e3d108e"],["/archives/2023/02/page/2/index.html","16f344ef46369308e49cd20095a19495"],["/archives/2023/03/index.html","f15daa4ad47dfaf5bda62512c456defb"],["/archives/2023/05/index.html","936d3b94aa7b902296bf86b01d0abbc6"],["/archives/2023/06/index.html","1642e5c4ebc0d81fb7ff69da39a3e3bb"],["/archives/2023/09/index.html","c776f309462f8654723779f7478cd539"],["/archives/2023/11/index.html","c107512613abcc5f950bf225d3b790d1"],["/archives/2023/12/index.html","f880050c65028e85d6cff6ba0e7167a6"],["/archives/2023/index.html","9b9c549def1c4c10a5ef639a5a918905"],["/archives/2023/page/2/index.html","9b9dbc7d507d716e65770c03965a0378"],["/archives/2023/page/3/index.html","8dca14bfc4ec04fb4d9058aba9fce67d"],["/archives/2023/page/4/index.html","dbab1fcad911c0985b26c2d0a0a31dc2"],["/archives/2024/02/index.html","14022600999adbbc9fdd7ffa8534817b"],["/archives/2024/index.html","d23f72c371d5ba9cf570f1e98785f88b"],["/archives/index.html","efcea8a986bacc4a491fe0b613055e3b"],["/archives/page/2/index.html","305b3065117f3afec38e819c8341df3b"],["/archives/page/3/index.html","cd104d9884c0d0c27194fd789bc9f6a3"],["/archives/page/4/index.html","2e4969ae838f9380f762abc3d823f0c7"],["/baidu_verify_codeva-qQP2iZOMLX.html","7c65bd8a5ef09e1186969b6824903214"],["/categories/Java/index.html","62f13ae5f782477f9dd0380f31677a3b"],["/categories/Java/后端/index.html","379e7746a82a62b851675202f3d69d94"],["/categories/Java/基础/index.html","5bc9110057a947edd1dd73242666d236"],["/categories/Java/基础/集合/index.html","020b100cef75974f5fd7efa877950ca8"],["/categories/Python/index.html","c6ad307bd5dbec900926631ebe4238d5"],["/categories/Python/编程环境/index.html","b0677683e497e1551b6ab60863460d22"],["/categories/R语言/index.html","55f585dd1a645841482cdc084e13fa30"],["/categories/R语言/编程环境/index.html","1103f4e16d9ac6fa66763d1ded82072b"],["/categories/index.html","63835c7e5e6487e39c222a58200244b5"],["/categories/中间件/index.html","2d6cb14caf8ab24ea8d94e4ce8daad1e"],["/categories/前端/Vue/index.html","81fee8904929f77afbdcec3b1500874f"],["/categories/前端/index.html","9ea2ca3378896fcd8cff7915dded74ef"],["/categories/大数据开发/ElasticSearch/index.html","da36a1663d0a4a10eabb1135d44dc7b5"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","5a23d00eb05c00a84e95d61228d31a07"],["/categories/大数据开发/HBase/index.html","b6b9945fbfac28bd22fd42b97c70b9d5"],["/categories/大数据开发/HBase/学习笔记/index.html","6b710351eec3117d0ae56502a7e03510"],["/categories/大数据开发/HBase/环境搭建/index.html","b640930e48f6b656a66e3a2e15fdd9c0"],["/categories/大数据开发/Hadoop/index.html","bd955fc390c21bd4b20db6ec2519118f"],["/categories/大数据开发/Hadoop/技术/index.html","13a6abc0312334a2fb1b76662e7582ec"],["/categories/大数据开发/Hadoop/环境搭建/index.html","4e86f5168b8655e0470d7b68a6a6a277"],["/categories/大数据开发/Redis/index.html","7291e997387213b5635233d868d0bc73"],["/categories/大数据开发/Redis/技术/index.html","b5b330a7248d1650482e128c370bf91b"],["/categories/大数据开发/Redis/环境搭建/index.html","21bca745065d1ed151f77e3bf716c456"],["/categories/大数据开发/Spark/index.html","5dd8a34a12c6fd85ab329ce98ea1b8d5"],["/categories/大数据开发/Spark/环境搭建/index.html","fe2e1b2a9d0cdc0934cee560ecc2aba4"],["/categories/大数据开发/Zookeeper/index.html","21748799685390b3d696707668550a88"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","00722c51404458a7150d80388a9a57b4"],["/categories/大数据开发/index.html","355b63ec9a227413463835655ff5e43f"],["/categories/学校课程/index.html","e208dbc19cfdcccd3a9d13ce2f01126a"],["/categories/学校课程/计算机操作系统/index.html","76b05e3fd5aa1b5d4cf22ff2699e1695"],["/categories/操作系统/Linux/index.html","2f7f6aab1f3f2e97bf342d9880d04700"],["/categories/操作系统/Mac/index.html","f241ea70c009c965d267ccb00a0658ba"],["/categories/操作系统/Windows/index.html","072b8a4ef266e7fc4b9a1e5a40ac480b"],["/categories/操作系统/index.html","78c6728eea95567425fe753f70875f7f"],["/categories/数学建模/index.html","8c8cce0080bf93049fc9600050f26c86"],["/categories/数学建模/latex/index.html","a017b9958d47f8fb688bf2965e546375"],["/categories/数学建模/优化类/index.html","babbfa403764e4a77271ba57fe1c52cc"],["/categories/数学建模/优化类/现代优化算法/index.html","b6ff4003b10aaf502b21c2501cda8d43"],["/categories/数学建模/优化类/规划类/index.html","e8965415e60da0743f68c6cb29734353"],["/categories/数学建模/绘图/index.html","2c178b7798adf702db2abe40a65e0200"],["/categories/数据库/MySQL/index.html","cb85cb10ca4216a4bff58a95bac98687"],["/categories/数据库/index.html","5bdeacbd520f4a0ccb867f84192742a1"],["/categories/数据结构和算法/index.html","738692149f69ede1d6c2fdbaf05ebd95"],["/categories/数据结构和算法/page/2/index.html","a464f059e8c3be3a85cd9307385ab883"],["/categories/数据结构和算法/基本原理/bfs/index.html","7f46f477284c29477a975531b8604d35"],["/categories/数据结构和算法/基本原理/dfs/index.html","276243fbfe81614cf5b655c93e114be5"],["/categories/数据结构和算法/基本原理/index.html","141ceed02ed3d6b458311d94d31607c0"],["/categories/数据结构和算法/基本原理/动态规划/index.html","43c645c826a8fbff192634d8e7b8ca60"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4eadc0c1f398f312764c397c2e00b53f"],["/categories/数据结构和算法/基本原理/图论/index.html","657f3e42392e0679708d882524d8d741"],["/categories/数据结构和算法/基本原理/字符串/index.html","d1a3b0b30ca09acb445b44073dd76fbc"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","358f8194125306f680639e993df9cd36"],["/categories/数据结构和算法/基本原理/数论/index.html","719c3d2626ab06d6e51ab48f0b057bcc"],["/categories/数据结构和算法/基本原理/树论/index.html","588d6f79a8b452290440625ed4bc4d21"],["/categories/数据结构和算法/基本原理/链表/index.html","e1a1ad551992966f8bee6462d43a9290"],["/categories/数据结构和算法/算法题/index.html","920fffc469e2939470dff21372d4fe73"],["/categories/数据结构和算法/算法题/二分查找/index.html","bd8b4e40333ada22dcc0e94bee5070b4"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4bb8cca443913bcfd825bb3cc679c12b"],["/categories/数据结构和算法/算法题/动态规划/index.html","41de1acfe7e487d538289ce957b0622d"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","b285ad18d15a784a7aa51720d989b3eb"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","83e43b72aad30f99b1b41dc367509a8f"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","538c2bbd9db5fd80a0edf2d512e3461c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d98b8c171fa040ff4d2ae9872112b1cf"],["/categories/数据结构和算法/算法题/数论/index.html","0733ee8d8ee2831f0a8ce15a0eca9eeb"],["/categories/数据结构和算法/算法题/栈和队列/index.html","ef7a07ad56dc9d88d3a0a3981cbc0c2e"],["/categories/数据结构和算法/算法题/树论/index.html","c088d4b70a932df1750629888d272c1b"],["/categories/杂七杂八/index.html","9bc14155ff1bfb310196ad76843b27c9"],["/categories/杂七杂八/博客搭建/index.html","fd2cfb78e7363672a5da60f83df80973"],["/categories/编程工具下载/index.html","5b64fd5f4aee1850780bc18f08de8580"],["/categories/编程环境/index.html","7162188bdc461b91676977ee6565c30e"],["/categories/编程环境/大数据/index.html","c03d7ae173bc139836bdf726a9a2f1d2"],["/categories/英语学习/index.html","89591f649598b22e7feb86bc2c08c992"],["/categories/英语学习/英语语法/index.html","e4605d25e4b249bcbeeed791c8a4f901"],["/comments/index.html","9ebfe1b56eb8b402cf3273d023130b75"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ba608065e8b68121337001da0ff40e13"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","4847108b9c80517e64fee0a72d33979c"],["/movies/index.html","183ea8efe57ab7ca9e46f1e821ac870d"],["/music/index.html","e60e1ab1379499b7779ade327234f3ed"],["/page/2/index.html","4b1e1323a539b65bd9646a3d9b6f19ce"],["/page/3/index.html","1ca0ad89dfec164c198de7df57ba0cd7"],["/page/4/index.html","52eb26b535a305fd42152946037e5db5"],["/page/5/index.html","c36c4e15f50ca68a0cf7ad6e763634fd"],["/page/6/index.html","5b2218e5dc40cbca2ec5ed06615f41bd"],["/posts/1021360842.html","1293ecbb8eec714db55758b9fa9dd8bc"],["/posts/1120620192.html","aab3848903121754f9d3d66ec75a2e57"],["/posts/1141628095.html","7914cc7b06f5cc03d223b6a58e7a2f86"],["/posts/1168613674.html","c5e7bdd4fc7ac6b15187f3c0703d754e"],["/posts/1219920510.html","865b9d36e46e8b29f9cbf3069223b40b"],["/posts/1222166338.html","f489269afa4fb7b4b6612d6b4234e71b"],["/posts/1259097482.html","fea82a98c54df025ebad26ecbc5b0508"],["/posts/1271036369.html","055ea8611048621b34a23572b8fde782"],["/posts/1312847445.html","13ec17796c5226ae5f98c39d55241588"],["/posts/135355774.html","8cded357fbcef69dd6eecb3d38693f0b"],["/posts/1375344716.html","05144ed28b39a2bb0c38614b529d2a6f"],["/posts/1388991698.html","9ebe0aec009d6e0b572c7d6c15aefd08"],["/posts/1410315814.html","bde81a7f3408d0606419588f0986f385"],["/posts/1452790229.html","13008e38fb3d4ab18e10cc01bd0d71c9"],["/posts/1470079884.html","aed96d9dd32a9b4d78bf5db0df2fc563"],["/posts/1470079885.html","961cd557ca4d6091db0a6dc865b1e281"],["/posts/1470079886.html","b41b0e036a4fe9f87a4b374e9104510f"],["/posts/1470079887.html","f7b38eba63e25b57e49bcb3a7f44622e"],["/posts/1498536549.html","f06d720dd24b6b2ea92d17a034428685"],["/posts/1539568593.html","6d1a7d72a00ce9163aefc2631de756ea"],["/posts/1547067935.html","180d0c026b1ce96271ddb515d75641d1"],["/posts/1557866301.html","59a200b9b508c7db7287e16bed02e03f"],["/posts/1571776361.html","b6b6929ce6134e7296d01bd835ec1e6a"],["/posts/1605124548.html","fbf6afeb3c095ee5f12d7e8c4c18c3b5"],["/posts/1633036852.html","678d46649541393e2bf4c21398c715f7"],["/posts/1674202625.html","7ac157cbc4f06499413738b5dcd591d9"],["/posts/1765123828.html","8c515600d506b40b92368509ccf1d3b2"],["/posts/1767336200.html","6981097bbee73a0007bc5fdba749f100"],["/posts/1776114197.html","a897d40b9c4cbfe532ed47b2e749afb5"],["/posts/1817748743.html","9270d50ce64b05503f7542a7ca9830b5"],["/posts/1925125395.html","48b9fabbce72d9aff2e6519cfe9edd0d"],["/posts/1966191251.html","2d7ecade9859fa9661f70bef013a492e"],["/posts/1987617322.html","d6066a367327e719763a460a67b9126f"],["/posts/1999788039.html","cdf9470281c258add3bf1be7882d6b68"],["/posts/2075104059.html","5a972e181debbbd476beda66a527fce0"],["/posts/2087796737.html","112b7d9bdfa004a131ff167ed24cffdf"],["/posts/2106547339.html","a5ea895dcfbc63f05904577dc9575807"],["/posts/2207806286.html","d6912cbe247d959c00d3e39c0981a59b"],["/posts/2225903441.html","a324650ac1fddf102570b48a6e9724d7"],["/posts/2265610284.html","6857bd9628ce62eba49c39d0309a1c4b"],["/posts/2281352001.html","54ff5f9ad2cbd29c93e3ae3d035c51e2"],["/posts/2364755265.html","02b6fb18e545e53a897b8e6569440060"],["/posts/2414116852.html","d30213eb1cf9ef054560fb61d4570f09"],["/posts/2421785022.html","65d6574a08667f26519c0973065be31d"],["/posts/2482902029.html","198c01cbecd6cf53960de81f9e0902b5"],["/posts/2495386210.html","264494cf7d84ffb8c66015c1175ec64b"],["/posts/2516528882.html","7c7d1d482a23f821fd78cceb5a54a230"],["/posts/2526659543.html","c145838568c08acca9c8af2881fbda59"],["/posts/2529807823.html","acf2e950068b8b80ef74117266af5168"],["/posts/2596601004.html","edf5962e7d96dbcfff0a7c60f2d04d03"],["/posts/2697614349.html","223ab9747be33e3726abf7abb6fc4076"],["/posts/2742438348.html","c1e121250ce5b81a987a08eead866f80"],["/posts/2768249503.html","33c7de506d3e7161c9c4705578826cca"],["/posts/2864584994.html","7c4da9136813e8d211c3a0ad82de835e"],["/posts/2888309600.html","8384b1ecce078ad2e4789eb5304860b3"],["/posts/2891591958.html","e813020392d9d50bf3905deb13364fee"],["/posts/2909934084.html","1341a58a2d9cbb98525aba1dea8cd687"],["/posts/2920256992.html","9ef649a7a8d5dc11981d7612be243e21"],["/posts/2959474469.html","6030454aa983622fef311c708942eb55"],["/posts/3005926051.html","030df0221cd268aaa6e130f3882d5f5f"],["/posts/309775400.html","dcdadf06f635cd19f850f14fbe389f0e"],["/posts/3156194925.html","c5830b77e9614a1608e1d4174a71c8cf"],["/posts/3169224211.html","d4a44b7e0216c26557dd3abb42bb3cd8"],["/posts/3213899550.html","eae16cfccdcd1a1c4c0279d56b6d1269"],["/posts/3259212833.html","5fb21956eaa03e86acaba058c03797c5"],["/posts/3266130344.html","886c8ffa8ff8a0867cedbb4a68ebd566"],["/posts/3292663995.html","847d4133af272143a967171580d7f391"],["/posts/3297135020.html","9e13a49e532524079e52333d213db746"],["/posts/3306641566.html","e58a054bab6ee2055969173ac2601c0c"],["/posts/3312011324.html","62b7716d6102cf72615f7a414b34de86"],["/posts/336911618.html","359be249e90838749d013f1d49fc9d2c"],["/posts/3402121571.html","ee53ea13c2ca07ea6eed3252e7521e12"],["/posts/3405577485.html","624d46a722e15560eb68fedb74576933"],["/posts/3498516849.html","783de79f129fb0671cadc99030c5cf8b"],["/posts/3513711414.html","e3735d7a781952cdd97be5fa00b53631"],["/posts/3523095624.html","261188108f9b7261886638a0fa198711"],["/posts/3546711884.html","db4d74e4c39f719917f9db9070312b49"],["/posts/3731385230.html","ae3711c57dea356cba4ea9d82b73679b"],["/posts/3772089482.html","bb355ddeafbdd295bd82c8f09a293544"],["/posts/386609427.html","a8674bcc90d2bf9d4c1989bae9b1357b"],["/posts/4044235327.html","1a884f0f6d6b8f8901a690394c27daf6"],["/posts/4115971639.html","b3f7849a0f269a40a570cc99ca242561"],["/posts/4130790367.html","57280be43c355fa156406f281a12d9c0"],["/posts/4131986683.html","8a43c2bfa09cbdd7e8b6864a202427d1"],["/posts/4177218757.html","166b6c7c030cf320955b76e32b71db94"],["/posts/4192183953.html","37b9818c5ee52ac85e78561241ea46af"],["/posts/4261103898.html","3eed42d0094522271b4f57b1ca197c11"],["/posts/469711973.html","392a6f3fc28f240e937711e5ce58171d"],["/posts/482495853.html","7e5700744fd3806295474bf74f58e418"],["/posts/488247922.html","f8bd63a3a36681053943b3c90605396c"],["/posts/517302816.html","b8a55583fca25e7b3e95a263a92433de"],["/posts/570165348.html","c100d80072e1af896a7bd39138f64fc6"],["/posts/595890772.html","5193cda50fb4014e6380b85317aad9af"],["/posts/67485572.html","a9233a6198346adb90df6ff43a622bf5"],["/posts/694347442.html","0e31a450b424163753db4a1357fc9ab1"],["/posts/707384687.html","f95ee4f7ade80a01607e4578f306a409"],["/posts/71180092.html","3b240707afff293d5870550a9d0842ba"],["/posts/716459272.html","a9ed1ef25405745346f8cccafe6c06c7"],["/posts/765481613.html","d4f1981a54da19d9bb377c587d9640a8"],["/posts/778231993.html","a75778ad2c25c417712ef381c01957d7"],["/posts/795397410.html","5378e350bff2b9e9e49970585a61c800"],["/posts/820223701.html","206342e31e5c5fa4ee0991532562eb83"],["/posts/830372185.html","3088e6b43b4d2a8d7bfa0b62d8492bbd"],["/posts/88294277.html","52e1a79183cca7612bc8a5915b49bac9"],["/posts/939963535.html","086cbdc5a5524ffe11e9b0e2e5ce7ddd"],["/posts/983786067.html","4af1c666e550ae2ad929c001b028121e"],["/sw-register.js","df2102d3b774197e18b1fb8da8e86ff5"],["/tags/C/index.html","11cdb9c7cd134ac86e26b294e1147c83"],["/tags/C/page/2/index.html","66340169a70d2622d0f264131e8cdfa2"],["/tags/C/page/3/index.html","ee169f877b0fd70f73cbc80f26a70c9e"],["/tags/C/page/4/index.html","8a2a1c72a425489eb1c3dcf4630d066d"],["/tags/ETL/index.html","e75a29e5a27f9c5299a351d1cff66964"],["/tags/ElasticSearch/index.html","66265d9992ce14586377b4ea52e9abaa"],["/tags/GUI/index.html","0eb766569147d030efb787cf7d94f18e"],["/tags/HBase/index.html","5006e84b8b57c086dea57ca91ae7de72"],["/tags/Hadoop/index.html","0c51ef75e1fd8f606a13672ecb3475d6"],["/tags/Hadoop/page/2/index.html","66fbfb060dcac6a47cd7a41340d5684f"],["/tags/Java/index.html","eb956ce7f70fdfd3865071b1e8368e55"],["/tags/Java后端/index.html","c10be4049199baa2718eb03dbdbc5739"],["/tags/Java后端/page/2/index.html","3e74a9798ef94cda6eb8ae288aae9592"],["/tags/Java基础/index.html","d9339a3695b7fcd3bf8f3ff20359d45d"],["/tags/Java基础/page/2/index.html","03784e81bc6a741509db3c6c4ef6ec5d"],["/tags/Kettle/index.html","e1ff288e08f3c64bb7bd9b32ddd9e539"],["/tags/Kibana/index.html","6122faaa892c9a4d64ded997f0c78e74"],["/tags/Linux/index.html","430428612ce6b49717aaa02dbb28ff18"],["/tags/Linux/page/2/index.html","68be5bdf8690e5d3ea51a36ae11fe9a1"],["/tags/Linux/page/3/index.html","378c817782efbd53023e4273a1445773"],["/tags/Mac/index.html","e76b20e14263782024b3136dbe735cd4"],["/tags/Mac/page/2/index.html","9552dad047771694bdf4bbd28b3f6e1a"],["/tags/Maven/index.html","8fc3141386f439ac39f99027dbcfe7cc"],["/tags/MySQL/index.html","fc5a88ca086b2be68edda27fcbf7df68"],["/tags/Python/index.html","c56430b5e0985a0e732d7bb962f6893b"],["/tags/Redis/index.html","e5a53461ffee19037d040e730df6cd36"],["/tags/R语言/index.html","305d918a8ad0737cf9000dc327a808d7"],["/tags/Spark/index.html","7b0ea24ef9f738c05c8cc9c3a2840e08"],["/tags/Ubuntu/index.html","af793b9d12a0365577cbe93a0dd64dcb"],["/tags/Vue/index.html","213ffd2a3a930a4632563e40ff7b29c6"],["/tags/Windows/index.html","f08463c785bd801ab93d7ea5c84ea224"],["/tags/ZooKeeper/index.html","521e670bc4f21799464a581e56175945"],["/tags/bfs/index.html","dd22cc6149942a7a2259f893cf320bfa"],["/tags/dfs/index.html","80c36c05edb05ee7473ac832816c9589"],["/tags/folium/index.html","6f325cc22734880c0f60c019b38b2848"],["/tags/git/index.html","cd3787a0ce97e8a5919c7006d6ab136d"],["/tags/index.html","2d9b99d004f14158c06fda6fd5da17ce"],["/tags/latex/index.html","e69daafda7cd5c4bf5b13db8cdcdce06"],["/tags/中间件/index.html","1c575845e8f25fc3b81c970858451159"],["/tags/二分查找/index.html","61c866fd8726dd84e12f9b3fb98a57e3"],["/tags/优化类/index.html","fd8bbc96e60e47f5bc1400f7a60ebd43"],["/tags/前端/index.html","852cb62b7881421da4dfb9b0762c814b"],["/tags/前缀和与差分/index.html","10e5227f70c8fea020bac44016d2152b"],["/tags/动态规划/index.html","260b8f8184681e6918de648c7405d91e"],["/tags/动态规划/page/2/index.html","dd1953449de305f9d8cfdf131910526f"],["/tags/博客搭建/index.html","48ae13de06612b9b19939cb4f6278d73"],["/tags/图论/index.html","2233108af800cd5b9d2a6ce12e62b214"],["/tags/大数据/index.html","9b297e2dc86a0df9c2e66278548868fb"],["/tags/大数据/page/2/index.html","db47c58f7140a694b6918aae691f3da6"],["/tags/操作系统/index.html","0f5331c76106a46c1c0954260f426819"],["/tags/数学建模/index.html","e60dc5c0357adf1fc6cfaf7c30afe298"],["/tags/数据库/index.html","a03e046f09ebaa0758b1d796831b45cc"],["/tags/数据结构和算法/index.html","7f0225d3be29832b2cec220d197753c9"],["/tags/数据结构和算法/page/2/index.html","09fc6968428bb31964e57392042d886c"],["/tags/数据结构和算法/page/3/index.html","69ab8e113a0afa8c2cd3ff9d77f650a1"],["/tags/数据结构和算法/page/4/index.html","395d52186cdc93888d4ace48d63746b4"],["/tags/数组和字符串/index.html","dcc78cf55118271459106f2b572b40f2"],["/tags/数论/index.html","fd9ec4f07683cef2222e2f02822860d0"],["/tags/枚举类/index.html","7e3e648d0b31c00c9dbcfb55b9bfeb9c"],["/tags/栈和队列/index.html","ce2d24e32f1638c17548463b12be1783"],["/tags/树论/index.html","0f1bd1e8ac3c38f8d2147afc6d6ae4bc"],["/tags/测试/index.html","9e6bda060e36f61ca29883e73eee5071"],["/tags/环境/index.html","de6023d21da8c93ec983014eba34f561"],["/tags/环境变量/index.html","d11a07a5178a79b77fdb9411fa1c68f3"],["/tags/绘图/index.html","241017bad92c462babd8d194cb633ec9"],["/tags/编程工具/index.html","1710fcb88d7bc68dd31730e77575d442"],["/tags/编程环境/index.html","56909f17829d82ce15973c3c8e4171f2"],["/tags/网络编程/index.html","91bcd49a42b44879e227d70a2aa3c072"],["/tags/英语语法/index.html","394f012f80d06152b21f37ec23ea2bc5"],["/tags/计算机操作系统/index.html","3de01922eff805023a09fc1d481762b4"],["/tags/论文/index.html","36f4d9b36bc3b76948599d402409682e"],["/tags/资源下载/index.html","7e836b45974c04767639ebc46dfecdd2"],["/tags/链表/index.html","c106ad6552c65fc70225c2b6e4b66e40"],["/tags/集合/index.html","f0075fef67a5a1db82d76b094b1591cd"],["/tags/集群/index.html","46a9930c51f9c973ce3b66b25e3056ca"]];
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
