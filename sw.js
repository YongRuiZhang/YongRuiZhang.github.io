/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","bb60695f4aebc61f4eb142998a7fbb97"],["/about/index.html","8b27792c62c63312f6cda047e50d3e40"],["/archives/2023/01/index.html","28ce9a4505c853c290da504ea7248875"],["/archives/2023/02/index.html","1ca1d301eabf2f947aba0a203995aec8"],["/archives/2023/02/page/2/index.html","e519575dd262cffda0493c173bd18437"],["/archives/2023/02/page/3/index.html","a69cbc42e152acc17409aeff14169b5b"],["/archives/2023/03/index.html","da57ee74318f50045231fd613f80305a"],["/archives/2023/05/index.html","0b7d05d1ebc83d4380e6f4ff4ba54f32"],["/archives/2023/06/index.html","2dc2fa990ba62affe943dd89418aca2b"],["/archives/2023/09/index.html","4256e8488f50baa24145f7da54835aa9"],["/archives/2023/11/index.html","ff2f2a1e8ad36700342b9a9f2be6a15c"],["/archives/2023/12/index.html","a5857b984aa93f00c9b85d0755ede789"],["/archives/2023/index.html","f6e47c6932eac1cec35a7d1224f94f5c"],["/archives/2023/page/2/index.html","c9bc4dc7c18993f84ed3d1393e2042ee"],["/archives/2023/page/3/index.html","6e74578f7abaaeea26a07859e78923d3"],["/archives/2023/page/4/index.html","e824ab6bee3c915b6c8dc766cfe9c35d"],["/archives/2023/page/5/index.html","e1a2d8ebb4da81ff87530127af2f80c6"],["/archives/2024/02/index.html","e2ef620064816582d941e175200dd6ef"],["/archives/2024/index.html","d7a51a8a7ba334a8e938248265c0bd75"],["/archives/index.html","99b0e2cf73defa76917d071864f92667"],["/archives/page/2/index.html","68e69870a0c573e5b2049d13c0525392"],["/archives/page/3/index.html","cd43dd1ef899870bee564d5227745884"],["/archives/page/4/index.html","1374d9da3cb00da1856d73b305e1c94b"],["/archives/page/5/index.html","ef87232c1488011d43a083038ddcdc99"],["/baidu_verify_codeva-qQP2iZOMLX.html","a8db40daa4679806e91aec083854e010"],["/categories/Java/index.html","1e87bc97138133072bdfaa4186d8f351"],["/categories/Java/后端/index.html","fac0324bd851471adc66dba38adb6d82"],["/categories/Java/基础/index.html","a53ba7cdccfc00e709b88d72b07558fa"],["/categories/Java/基础/集合/index.html","31333e7d3fb8c2c78c223420881001de"],["/categories/Python/index.html","d5de44ce736f5b473d2adf85c2f4b379"],["/categories/Python/编程环境/index.html","f9ce8911727e92df2f2f0e1bac1ddb2a"],["/categories/R语言/index.html","b5c8d9f8620e802d2527beaf73c17900"],["/categories/R语言/编程环境/index.html","04ff5746e61596a567c7fe010015741a"],["/categories/iPad/index.html","1c93b360f61c571dacaffafeefda0061"],["/categories/index.html","4f22613fc0ee8d04779ffb94c70ba029"],["/categories/中间件/index.html","deaf5263314a05342148951cdb8fd5cd"],["/categories/前端/Vue/index.html","70b2fa53b47559c6e852f068bd4852d2"],["/categories/前端/index.html","3511c47601e97d4bb3dfee95b959f5dc"],["/categories/大数据开发/ElasticSearch/index.html","d70d357d1cb7b295a279664eb3b18a4e"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","58c6f1dccc9211c4629f988aee25746b"],["/categories/大数据开发/HBase/index.html","cb4aee6a522d5d6e0da325d4e7449250"],["/categories/大数据开发/HBase/学习笔记/index.html","1b87720b30cab2815c133f06a223b290"],["/categories/大数据开发/HBase/环境搭建/index.html","3505fc88dfae6dbe09c6297412498667"],["/categories/大数据开发/Hadoop/index.html","2dc388f7c0b2dbb5764f9f20900a3ae9"],["/categories/大数据开发/Hadoop/技术/index.html","61106b8f763038a42f56cd15f630a8e0"],["/categories/大数据开发/Hadoop/环境搭建/index.html","97b32e6d58a541b04a2ae3d4552672c1"],["/categories/大数据开发/Redis/index.html","22c0e2025cc196c98acda4604d96d558"],["/categories/大数据开发/Redis/技术/index.html","ee8d4608bd1b4110c425e7cefa68901b"],["/categories/大数据开发/Redis/环境搭建/index.html","7f083b693b8a638293e0a192bb7672bf"],["/categories/大数据开发/Spark/index.html","35494ac815d3e77957aca6a1a9803dd2"],["/categories/大数据开发/Spark/环境搭建/index.html","a64ab3aa7837441a899d09b5f34953e3"],["/categories/大数据开发/Zookeeper/index.html","47a899c1c4899a1f06e49108215dda84"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","990273ec17cfec52c195e16e4dc237a9"],["/categories/大数据开发/index.html","06fd1409736cc9c78589cc9dd80fd9b4"],["/categories/学校课程/index.html","eb2d20eb0be306f24b25447cbb06ded6"],["/categories/学校课程/计算机操作系统/index.html","6dcf6dc4c1f39d8a47d1be42af54627c"],["/categories/操作系统/Linux/index.html","78695c9392d1d6b9bba5f139cbb4724f"],["/categories/操作系统/Mac/index.html","7824da20d83c3702d6f9fcc7358cf931"],["/categories/操作系统/Windows/index.html","260c667c20389f03265070e11ba25877"],["/categories/操作系统/index.html","1b7b43d76ea2665fc136b85245a10820"],["/categories/数学建模/index.html","51efe0b0ff66ca56a803c2590a2a9c89"],["/categories/数学建模/latex/index.html","c7c29c5b2bc899780197d4f4483d9568"],["/categories/数学建模/优化类/index.html","c940e38d175867b64cc8c9c7e3efc9b7"],["/categories/数学建模/优化类/现代优化算法/index.html","762202a82e9e934834108d336908233c"],["/categories/数学建模/优化类/规划类/index.html","41c1fdecc9d027c7361156598e2a88a0"],["/categories/数学建模/绘图/index.html","4975e114398a86fe197a8163cc629a75"],["/categories/数据库/MySQL/index.html","4ac3904f60975dcc0b2ef22cd347db9c"],["/categories/数据库/index.html","dac4ec55864fb46428aa54ea5a400bd7"],["/categories/数据结构和算法/index.html","5a3261c4dc293d74f10f1501b424ff37"],["/categories/数据结构和算法/page/2/index.html","fb744daab4339865aab3f0014f8f2700"],["/categories/数据结构和算法/基本原理/bfs/index.html","6e85181ece7fc7eed9c42d81ad6a35f0"],["/categories/数据结构和算法/基本原理/dfs/index.html","9f6884763f179f7ca66e7c4f99a45097"],["/categories/数据结构和算法/基本原理/index.html","3ce229bfa3252fcf9c5ce3f722af0fcf"],["/categories/数据结构和算法/基本原理/动态规划/index.html","b86a7ec7f1deadf8689b5ffe05610c90"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","75b1ad5bd9a0d0896ac8f6c56b261e45"],["/categories/数据结构和算法/基本原理/图论/index.html","fbe60276e2df0d216397b7b40aafc8f1"],["/categories/数据结构和算法/基本原理/字符串/index.html","30770aaa9ac443cdf0a195c58522ef7f"],["/categories/数据结构和算法/基本原理/排序/index.html","519b88f3dd2e32062abb410c6c6c93be"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","7566964c8f6f4aa151493322d16b0974"],["/categories/数据结构和算法/基本原理/数论/index.html","350e779b265daeedc1aafe9ab41c2313"],["/categories/数据结构和算法/基本原理/树论/index.html","db8057c4915c683caaabc86e4610b782"],["/categories/数据结构和算法/基本原理/链表/index.html","dd770e710d932f757fd3028fdfbec901"],["/categories/数据结构和算法/算法题/index.html","488837401d64474a6dec6428b249e444"],["/categories/数据结构和算法/算法题/二分查找/index.html","3489066ad1a5a486a150c8c0c959b438"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","85372259eda716c28f103ed7838a44e5"],["/categories/数据结构和算法/算法题/动态规划/index.html","02356ad63a9b91ec36ea37705e687cae"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","df98103a63f89fba864021ca22ea3821"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","3b950701eaad59f13a744e960effd5f7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","db5f6ebb088aede875e033be1435eed2"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","dbfb912f4dd772893047956c35c17a01"],["/categories/数据结构和算法/算法题/数论/index.html","79fd70d27c48360b8abb72ff11e8f3d0"],["/categories/数据结构和算法/算法题/栈和队列/index.html","299866f763620697850b449e6b867a48"],["/categories/数据结构和算法/算法题/树论/index.html","8398dd0d81601d6645f73843f1f1522b"],["/categories/杂七杂八/index.html","88df512e6658dbb4015471225badd0c2"],["/categories/杂七杂八/博客搭建/index.html","fc5560ce68079ebc8fcabd146b842c2f"],["/categories/编程工具下载/index.html","2bb534ee23cb142db03f9d376facdfdc"],["/categories/编程环境/index.html","d4bd46340378e5bda3a4a36a02b02bcb"],["/categories/编程环境/大数据/index.html","fd7120566c13ec76a55106779861b559"],["/categories/英语学习/index.html","5886cdc0af057f46092792748a3363b3"],["/categories/英语学习/英语语法/index.html","0fadd168c10da83cd0bf8eaba32ba45e"],["/comments/index.html","4f3e6f9de0ef1dae67fa845a4f79a0a4"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","5e9b4cb8f044b1d6bf7b9cb3bf88e548"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","5bd20e17c5aca2778c5bc95a8d66701e"],["/movies/index.html","6e4837efc5b66d2909bcea113f13e5bd"],["/music/index.html","63c865af82ff27c5dc5ad7dd1ed9fe73"],["/page/2/index.html","b4df1e563c5a4b058fee785f3b334cf5"],["/page/3/index.html","5670a69643bbd27f8fbfcabb9372a409"],["/page/4/index.html","dcf4521b0ae2fbde552106c0038063a2"],["/page/5/index.html","5babcf09726fa336ce3afbc252db2426"],["/page/6/index.html","78c4d28ce5217041766e765ae4c65c1a"],["/page/7/index.html","c8fed4974d77b56d233e24022412e9c5"],["/posts/1021360842.html","74304b70c9ffcba8d15ffff9302ca374"],["/posts/1120620192.html","6e37d6e7a6c72a6004f44f94e4bbb3c7"],["/posts/1137707673.html","6fdda828fbf1a8abb4dab92c8030f939"],["/posts/1141628095.html","bf2fe7d6cb1f926de2f9229ba0d4497f"],["/posts/1168613674.html","2086b75bff96ea64a196edb96f78f065"],["/posts/1219920510.html","7145f5fd3416e5768fec8eb4145dfaf7"],["/posts/1222166338.html","994008b33f6c35a482e990e6ddd12178"],["/posts/1259097482.html","85a5cafcd61eb72f295351322a4de51f"],["/posts/1271036369.html","f8ff55d31f895aada7533e0f256782e5"],["/posts/1312847445.html","087f3a572abfba3f851230e365aecdea"],["/posts/135355774.html","ad7dd92f8fd8d9259c271423f264ba70"],["/posts/1375344716.html","0d6da4103fb2b5e13f4eaea0b0f4e1a8"],["/posts/1388991698.html","4727adedc4482b48119376ae513e61b5"],["/posts/1410315814.html","fc671123a40020268e8e57f47045785e"],["/posts/1452790229.html","975d22d144734ce0f43211cf81416985"],["/posts/1470079884.html","6118f6c8b70c165560831f182eb02ad8"],["/posts/1470079885.html","75f6c16eefaead98302f85c3dbb035df"],["/posts/1470079886.html","045f0b459f46aad809333dd104005b54"],["/posts/1470079887.html","131565da6cb176e461271c6828860e1e"],["/posts/1498536549.html","ae6ff21b9376b762b1f28a73210fdf17"],["/posts/1539568593.html","af8b0c656ef4a1d009c19ca98649a855"],["/posts/1547067935.html","4326a424518bbba0a0a1bbfc6fdb8d96"],["/posts/1557866301.html","d1a986b3bde1bf5ff7d6b1469311172a"],["/posts/1571776361.html","c45dacb3e6d96f44062ed4ddbdb59569"],["/posts/1605124548.html","484b4f08e1209334e3ba3de78645366d"],["/posts/1633036852.html","9a4ce0c4024265141e4481b1ed0bdc37"],["/posts/1667740714.html","ec98352eaffdadca2a31bf3976b895df"],["/posts/1674202625.html","5fdbd6b5967dba69d0ba15113d5b7b55"],["/posts/1765123828.html","906d312b18fb7755ed1ccdcfbf65a27a"],["/posts/1767336200.html","76770ff75ec8708aca99cd93c9ad730c"],["/posts/1776114197.html","888dbb40fbacb627978a1781b92008d8"],["/posts/1817748743.html","69cfc375d1d27560ea22e9d6516dde58"],["/posts/1925125395.html","c75d88eefa2463f25a1a582142c05189"],["/posts/1966191251.html","1a06b6d4fa20791fcdb9afd7c7321fb8"],["/posts/1987617322.html","43ee3e98616f57cbb4ae47ca868fae9a"],["/posts/1999788039.html","4797fc1ed38b69a6e695946c5d019a3a"],["/posts/2075104059.html","93b528b6c6b6af2b67d58cca69bbad20"],["/posts/2087796737.html","e5d36c1c597e6bcef833f5494d43a2b3"],["/posts/2106547339.html","bbcbae856b95e47a9a1de513599f0352"],["/posts/2207806286.html","a6c9fd748510aa7bc62b7f6b6c4b69f8"],["/posts/2225903441.html","e1ad2d803272b8856521b509f0c485f6"],["/posts/2265610284.html","8fb25b798527ca7f947f52665396b897"],["/posts/2281352001.html","410b567b69e6e3d9b03a10bf9f6a3979"],["/posts/2364755265.html","a04ed4b21dd6d458c54f937b72f3d37d"],["/posts/2414116852.html","ace22ec89d2dcb778d8407dace2dcc99"],["/posts/2421785022.html","6065828c352069be9d5aca37eaccbd7e"],["/posts/2482902029.html","412ed47ebe06894893990e392681e129"],["/posts/2495386210.html","df1384a0c6d9571da80f99677e2039e9"],["/posts/2516528882.html","5231847cfb48f8cf596f03044868a804"],["/posts/2522177458.html","5e28f9ae77b910b8c28ed306cd4961ef"],["/posts/2526659543.html","a6fb6f2a5dbd166c4d4003c487d1cbe4"],["/posts/2529807823.html","555c70a3bbdafa7d798ff41875c86a8d"],["/posts/2596601004.html","134489e4f55c0719a825bd679a3a9a09"],["/posts/2697614349.html","e07968168cbd09585b7f9dd4a1036ebc"],["/posts/2742438348.html","355ceecdc33bb07513c9b60ea9bdca1d"],["/posts/2768249503.html","3d0940ffeaebca5c61ad87369eb06d84"],["/posts/2864584994.html","a6e6a9cbb0810af1e696c697408642d7"],["/posts/2888309600.html","9583dc70ce0720298f8d15278a2cbb8b"],["/posts/2891591958.html","eb04c37a11685e0f667f615951551292"],["/posts/2909934084.html","dfe245d10c9eb3f5eb41dbc86347bb83"],["/posts/2920256992.html","f952207effbfe9c184e2e73734ad0268"],["/posts/2959474469.html","a34cbf8aba8a0f843120a88630a13ded"],["/posts/3005926051.html","d26459c9ea2166a1d850ef4662629a72"],["/posts/309775400.html","10531c1cbb5b3311adcaecceeea6a9c1"],["/posts/3156194925.html","9fb1a6b74ea7208056803828067d8b0e"],["/posts/3169224211.html","2ddb4e18fde26b043b2ab3b8caa3034a"],["/posts/3213899550.html","961e28093174ff63fdb41cee5ee2bf20"],["/posts/3259212833.html","3dd82f52b87da77ac66f85b8c758a8bd"],["/posts/3265658309.html","1591435eec25d7be6a25dd8a02cb0b1e"],["/posts/3266130344.html","bee03ae152bfce33d369b4dedd0f53c7"],["/posts/3292663995.html","af556b6ee321fa9725bb7df8d2017853"],["/posts/3297135020.html","4cd1ff9849de6ec16cb009f7cb7d53bd"],["/posts/3306641566.html","7d06cc6bb4a418bdf3936ca52e1f65e3"],["/posts/3312011324.html","5d8abb76e1b192b703d61076ac5b285d"],["/posts/336911618.html","10a50397940a03545ac6f60e6ba62e2e"],["/posts/3402121571.html","75a5234df02f7dfd680dbb8da4cb0423"],["/posts/3405577485.html","5b8b59b9ca1555b2867966806084e29c"],["/posts/3498516849.html","98c09e769b329c90a5d89bafb583b686"],["/posts/350679531.html","2489bc933a1dd14a93e2af2a4981bd38"],["/posts/3513711414.html","b0f39f99b0653bbd7fc0d8b938d59189"],["/posts/3523095624.html","599f77b2967dba889fd6cb7a84002d18"],["/posts/3546711884.html","59b9449224dee637d586d3b4f439a1b4"],["/posts/362397694.html","3784a72fae74a9c1ee3f55909c2cb2ed"],["/posts/3731385230.html","b5e0c5521ab3b8aacd943004a9d4ed96"],["/posts/3772089482.html","03be3569d9e5e0f854816cb994d885bb"],["/posts/386609427.html","5f106323a6efc9b29e8f83efea7b14e2"],["/posts/4044235327.html","6b6fc896c7f07a8eb51965ece96b0c38"],["/posts/4115971639.html","c2057b9b1fa2326a3371edb7b2fabe6e"],["/posts/4130790367.html","cc431d0be22df4d981710162ebc9d9b2"],["/posts/4131986683.html","12a2cf69c977b0468365c9ae1abded17"],["/posts/4177218757.html","94487e9fbb6bd4f20ba237ec56a9b294"],["/posts/4192183953.html","260c60e6edd8363460e78fe7d79dd00a"],["/posts/4223662913.html","897b073dd7475bfa1e57d79e0a375605"],["/posts/4261103898.html","628feb8175d5e992dd356f8c6ed0789e"],["/posts/4286605504.html","1c63810379a5fdc6b1d0f2b09f178e2b"],["/posts/449089913.html","984827184cd94b2788f4b9015380d713"],["/posts/469711973.html","d22bdc84fd681f71d4bd0cd36a4b3073"],["/posts/482495853.html","e2721dc3a2be7ab501db2e24a4f8470a"],["/posts/488247922.html","05a68844d0d2d9cd3dd235f12a047581"],["/posts/517302816.html","6f3146eec1f99f5cd72ad4a2d37525b9"],["/posts/570165348.html","e849d4b5781c4cca9803346edc36324b"],["/posts/595890772.html","2576d53b6afe919971a397574110940d"],["/posts/67485572.html","276c83f29aa63758f0aae8e9510f6384"],["/posts/694347442.html","a03c6572c15ff207ca9e07e79b8ad710"],["/posts/707384687.html","f7c095c11410034ec81bc35538322485"],["/posts/71180092.html","bfc5db9bad35cc5bb472dd413de1533e"],["/posts/716459272.html","6f9da81a5c0cfd7c9280a6e20090e87f"],["/posts/765481613.html","f389cf1f56e5f59a3a17d8a073a4291e"],["/posts/778231993.html","77e4224740af02aadb17f59ff9641ea0"],["/posts/795397410.html","88b81f9e3b7b8a0afe55d9c5105f414d"],["/posts/820223701.html","c471497cd3c591ddb186269eb977daf7"],["/posts/830372185.html","b346a9ab68367c623dd43bd660f465f2"],["/posts/88294277.html","8a53c7bbad154d6ed5379363722f2011"],["/posts/939963535.html","0d62cacb16e83f1d730be444a0b4f391"],["/posts/983786067.html","3a72ed2b637d30891a35e66eaa3e32c1"],["/sw-register.js","22192057b44910d3362b3bf7bda299d6"],["/tags/C/index.html","60972300208a0a468d6290703851452b"],["/tags/C/page/2/index.html","7dc69610dec1e7d45dfeb5fbb49a0bc0"],["/tags/C/page/3/index.html","989704fca2da76a338c8408ef76baf74"],["/tags/C/page/4/index.html","6d603a804669905e24a5136d767eea7b"],["/tags/ETL/index.html","5712f0df4095540a2648125d5c82f527"],["/tags/ElasticSearch/index.html","4c5777c42fec9c15ea7e409c65d827d8"],["/tags/GUI/index.html","fccfcb2fa8788168500787e5bc089d02"],["/tags/HBase/index.html","59c18a6be48c4a237477c78592c4ccce"],["/tags/Hadoop/index.html","45bbbf010c73bd365b5b84f984014bd8"],["/tags/Hadoop/page/2/index.html","65441f3c940ce5dc7545bffb32cba92c"],["/tags/Java/index.html","88169e2338376eb833869317e6aa04af"],["/tags/Java后端/index.html","be55463de7823dd37b838268a4e25a9e"],["/tags/Java后端/page/2/index.html","2cfc292e784f6580da2341b67ca5c80f"],["/tags/Java基础/index.html","1fe2b4a456d6f9e2c05bbe10fbf4cbb7"],["/tags/Java基础/page/2/index.html","0f7e8ecb63466f5bf3e6b13eb9384189"],["/tags/Kettle/index.html","a3c48443891008530a90b45d459a3d00"],["/tags/Kibana/index.html","5a59f6306023413e28a5059f54c9685e"],["/tags/Linux/index.html","e93b7cfd7325a40ea5728859af0c8df1"],["/tags/Linux/page/2/index.html","6013d264c08ba1e6877729885c8a72c7"],["/tags/Linux/page/3/index.html","a18f80ba9ed17f4eb842fea0447f179c"],["/tags/Mac/index.html","d27861c99249c719ee8fc99c9dd9251b"],["/tags/Mac/page/2/index.html","b8c9d9cdb1a38b471586552afa278d54"],["/tags/Maven/index.html","8cbe34382d6189a8d052fa23ff5796a9"],["/tags/MySQL/index.html","2b303a3c894c5f9cd367906b34359cb0"],["/tags/Python/index.html","637c5960bca931c02670cfc1e030ee06"],["/tags/Redis/index.html","945520cd7a0f0d0863643e9f2d34446d"],["/tags/R语言/index.html","c3215a24e242df4909a3b506585dfc21"],["/tags/Spark/index.html","e489385ceb9b4e1888a716374b85cbd9"],["/tags/Ubuntu/index.html","749d9bf6b5bb4c711f165e2cfc781a4e"],["/tags/Vue/index.html","85ed5bdb84aa697256d9e7800341c786"],["/tags/Windows/index.html","26d64d8e1e66bff982fc2771f995caef"],["/tags/ZooKeeper/index.html","0019983b613e2e2188409a26ce1206a9"],["/tags/bfs/index.html","b1cfafba475b8dda345d199be672d13c"],["/tags/dfs/index.html","2fff0328bd345a1fc3eadd0399239972"],["/tags/folium/index.html","06dc862ca515e1b2104bc4ab8093fa91"],["/tags/git/index.html","b464f3ade28e43911f30385e5de6274d"],["/tags/iPad找电子书/index.html","407011cbac255fa1d4fa602105c55455"],["/tags/index.html","2f30663a9f361e2f3d56f4b0773af6ed"],["/tags/latex/index.html","0e78e5650d5c3a6489dd9002fed7a120"],["/tags/中间件/index.html","bd7447d1fe506bc1d807007a67d479cf"],["/tags/二分查找/index.html","f944c9bf310eb6fd0a8835044b88da9a"],["/tags/优化类/index.html","190eeb11d47be14d98245f1c7df8b80c"],["/tags/前端/index.html","4f8ce0f33bef7f6a2ca404438a457bfb"],["/tags/前缀和与差分/index.html","7e232f5fa5f8773c119dd70044a15bb6"],["/tags/动态规划/index.html","1f890e34966a8307931fc4e13b882be0"],["/tags/动态规划/page/2/index.html","8d69a0e41ed56648e5fc78329a51bf52"],["/tags/博客搭建/index.html","9f1b8abadb76e80d2e72c5ac1e806fbb"],["/tags/图论/index.html","b724d7bf3c5dfb0b8b0c8f3f507d18eb"],["/tags/大数据/index.html","484b5f378a8cda0382def01659da9705"],["/tags/大数据/page/2/index.html","cfbd3c0b46c92e9a8a1294abd8033da0"],["/tags/排序/index.html","dfeb01c065460d45733e5bb433924558"],["/tags/操作系统/index.html","9cb98ec73c2135398d3fa338206e019d"],["/tags/数学建模/index.html","23ea2cd7d4753dac11673e34f91c96a4"],["/tags/数据库/index.html","d9d86621765eb70833394fa40cb7ca64"],["/tags/数据结构和算法/index.html","e1fe401c9f1ee4d9614a60ebc2d28852"],["/tags/数据结构和算法/page/2/index.html","14d88078dca1cbffd9caaf8de356a635"],["/tags/数据结构和算法/page/3/index.html","dfbb71e98b0138c4639a54933bbc5a36"],["/tags/数据结构和算法/page/4/index.html","747da3fbc15506114e689aa62618c373"],["/tags/数据结构和算法/page/5/index.html","173528d35952d14ff1a373061d153daf"],["/tags/数组和字符串/index.html","d5abdb0188708a264145c869d164a75b"],["/tags/数论/index.html","4994f0d36217d7f9e1a252178804b177"],["/tags/枚举类/index.html","3139f531ce28dee207b6c57d17c77435"],["/tags/栈和队列/index.html","8b31873b3647fc6653978655f0cdc629"],["/tags/树论/index.html","2bd2519cc99caebe7a0fa1c9ec0e96df"],["/tags/测试/index.html","d77121072f8ae2047fd0c34bed5542a1"],["/tags/环境/index.html","ae79cd8f00aa5f0a47080bc5882975fe"],["/tags/环境变量/index.html","ae81cd4ad367e8ae4fa01bf90520e9d9"],["/tags/绘图/index.html","aa5fedfb160633f1870e3e52eb0c1ffd"],["/tags/编程工具/index.html","90d35d1264823eb43639617c1139def8"],["/tags/编程环境/index.html","3cfa30eee44c0eccf9009bf5b2d19d3f"],["/tags/网络编程/index.html","dd6c39682b943751e8562b707d0f4837"],["/tags/英语语法/index.html","0fc5537e273fa259c722bcfb2274b660"],["/tags/计算机操作系统/index.html","454e74d622d14acae8b543bd208960de"],["/tags/论文/index.html","26182602f5940ea1bd912586d2ca261a"],["/tags/资源下载/index.html","426dfc4f357605c7a63fe145eed81f83"],["/tags/链表/index.html","cf7d40d7c38fd875b5ff8c863caf775e"],["/tags/集合/index.html","90eff7542131e1be154e6d21b8868391"],["/tags/集群/index.html","204c587cedf1f2ded5c643dd009829a1"]];
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
