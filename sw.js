/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","45b9210ad2e261798c7f6869f7eccd96"],["/about/index.html","f79f070a2ce25a17dd1facdd45708f17"],["/archives/2023/01/index.html","0bcfab3a6df104164c405ddcf74b7a60"],["/archives/2023/02/index.html","8c38c08a9282b5c7261c32e8ae416e4e"],["/archives/2023/02/page/2/index.html","b8491d9a071a466501589a983a42cc61"],["/archives/2023/03/index.html","5a17aeebfc6b968f0b19afcb809d4bc3"],["/archives/2023/05/index.html","cce462b947d2c80251e429d4244033bf"],["/archives/2023/06/index.html","81941aea7a9d00bdb4bf059c56a584d6"],["/archives/2023/index.html","e90242603bb1b43ad8e7f45cd7597e54"],["/archives/2023/page/2/index.html","79d6d9a66295f014a25a8fbe0858ba84"],["/archives/2023/page/3/index.html","1a03fd12e1ceee3da25da284353aa144"],["/archives/2023/page/4/index.html","8be4bf4906956b21b4557f6d99392fcf"],["/archives/index.html","079b9ddf2ec4cca928560dcdf67be1a5"],["/archives/page/2/index.html","bb32d85314897997bfd3c816d97a657d"],["/archives/page/3/index.html","188788408c24f2d08f3bd725fc43ecfa"],["/archives/page/4/index.html","9431ae5794111e51cc248769c3dcd9a4"],["/categories/Java/index.html","08ee75e990e1e390ffba19cb86d9687d"],["/categories/Java/后端/index.html","102887de42b86d70f73efcb5567188cc"],["/categories/Java/基础/index.html","625d3391badb67d0a91d2ce652a92492"],["/categories/Java/基础/集合/index.html","b6bd552267559d301288b84496b8f744"],["/categories/Python/index.html","d6c743ec3934f966ba20d3e6a5a6aae0"],["/categories/Python/编程环境/index.html","3f13412821ca996fbeb0f0254b61af22"],["/categories/R语言/index.html","9c40c158100a1da58e70faffa9259ed3"],["/categories/R语言/编程环境/index.html","7e18bcdcab4bb5767f9b8571829e148d"],["/categories/index.html","28a8bc19cabc3bd531a34d3da9e57063"],["/categories/中间件/index.html","d26b1b5dc8b7ff4f819cdbb3458f1d74"],["/categories/前端/Vue/index.html","7a0d9d0a91322063388d5c41a21ecf11"],["/categories/前端/index.html","0225c2d4112eec1ed2ecb4c706f0ddad"],["/categories/大数据开发/ElasticSearch/index.html","c23446c7e5c4da28fe053b1efade5986"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","448fe1cb71e8e89d557ee5df41f57a97"],["/categories/大数据开发/HBase/index.html","fce89bbd34af5c593a44a820e56e93a9"],["/categories/大数据开发/HBase/学习笔记/index.html","20379c33594c993fa7b0e045cd73ec3e"],["/categories/大数据开发/HBase/环境搭建/index.html","24cd382d1ca474cf3af015d5fb09e655"],["/categories/大数据开发/Hadoop/index.html","bf3de052a93d044d89392932ed6f8f71"],["/categories/大数据开发/Hadoop/技术/index.html","8b45afd6c35966f863f35431a44a29b0"],["/categories/大数据开发/Hadoop/环境搭建/index.html","eebeee9d2afd4955570ad5ac54f1eae7"],["/categories/大数据开发/Redis/index.html","f8124bfdf12233762fbc56bee6968c0b"],["/categories/大数据开发/Redis/技术/index.html","369f2fc1983ea417ddde874e61ccca65"],["/categories/大数据开发/Redis/环境搭建/index.html","10eb9e0f7e76eddb94912e15c763ac4c"],["/categories/大数据开发/Zookeeper/index.html","c11e0db1f84587a85d147f607eeee550"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","a70b6878065a90f995d28d9ad6802f6f"],["/categories/大数据开发/index.html","5a9809e624bd0947fb3564cd251c70f0"],["/categories/操作系统/Linux/index.html","890cf590e3c018d2be07a9b867b26771"],["/categories/操作系统/Mac/index.html","753a129036c551608c692b352d6f9929"],["/categories/操作系统/Windows/index.html","fdcfdef76e9f9d24701c8326b5463b61"],["/categories/操作系统/index.html","4c76b291a00a6fc5d7457426849daaae"],["/categories/数学建模/index.html","f4ca04570f33ceec8b3e0172e553cb45"],["/categories/数学建模/latex/index.html","b0631b5a625cfa0646b37cdccbd3b787"],["/categories/数学建模/优化类/index.html","9f0536890404e730b41b78b60721422f"],["/categories/数学建模/优化类/现代优化算法/index.html","101d83bbb0245ad240479d6dc5cc2edb"],["/categories/数学建模/优化类/规划类/index.html","7c7ebc8fd77d317a6b65e2b12a49c1e4"],["/categories/数学建模/绘图/index.html","25b873ff963c1e7433067558f6c6dfbf"],["/categories/数据库/MySQL/index.html","089859d8aa63cdd7bbf8eccb13d65abf"],["/categories/数据库/index.html","678c61ffe77dc0a256c0e0dc851ff535"],["/categories/数据结构和算法/index.html","8994bc3c8c6dd86464afc7e6f3c09454"],["/categories/数据结构和算法/page/2/index.html","a75db50f4916748a2629d3843e509057"],["/categories/数据结构和算法/基本原理/bfs/index.html","fadae9cb3bf477fe67dc767c76f5b3af"],["/categories/数据结构和算法/基本原理/dfs/index.html","f3a289dcb6ef2ae4b0c61ee0624983bb"],["/categories/数据结构和算法/基本原理/index.html","b74b0bb0323807ab79cf45936f61315b"],["/categories/数据结构和算法/基本原理/动态规划/index.html","060034440368d18de9a46dfe58294739"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","49d7a0f73741b2581c6059e806708019"],["/categories/数据结构和算法/基本原理/图论/index.html","6a48fdc28dddf4c95a9fef0dee0c3abf"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","91e96ef27d4274c1731493819914304f"],["/categories/数据结构和算法/基本原理/数论/index.html","fd2820812571a438e13c7d3b7e5f019b"],["/categories/数据结构和算法/基本原理/树论/index.html","5e5ac6b90c557d99546d9438a2f36ded"],["/categories/数据结构和算法/基本原理/链表/index.html","4eb2f936a4ecf22a5800894ea554ec23"],["/categories/数据结构和算法/算法题/index.html","168a6251d1188195e4d9f031e6009ac2"],["/categories/数据结构和算法/算法题/二分查找/index.html","2c7ada7ca74c84d6e0dee8ab81b27028"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","e587363475e17c80c3f6a7b68bf3e900"],["/categories/数据结构和算法/算法题/动态规划/index.html","f45086bccbffa8a38e73179ff7a17e18"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","594870f618b0d9de77cde2aff168930a"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","005f2114cf07b861ed64c94cae422628"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","14c776b0193fc94996e9fc98b57b96e6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","5c58f37a8ffc1accc21b9585b9578759"],["/categories/数据结构和算法/算法题/栈和队列/index.html","85ce9a03cf19240d0e0f5ed7781fbaee"],["/categories/数据结构和算法/算法题/树论/index.html","d082fa521614227484d7843876bcc2c5"],["/categories/杂七杂八/index.html","e6e3e3ae9f9c2bdb1ef587dc196a9d2a"],["/categories/杂七杂八/博客搭建/index.html","4aa0689bb4492419c62ac78acb695c69"],["/categories/编程工具下载/index.html","33f9ae14746476b398068caa3c7cae96"],["/categories/编程环境/index.html","f0e1ab96c6bc4a8d9e843fc6e71b4870"],["/categories/英语学习/index.html","f354eaab063ba03164f513a15aa94ec1"],["/categories/英语学习/英语语法/index.html","1503a560b2f91f3dd35e68fda94d48a9"],["/comments/index.html","02ccbde1293fb53dcfd1322870aa595e"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ffb055b07ab8da4170562e28f10af71d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","005f652f9bcbfcc8f2d34a319715d03a"],["/movies/index.html","14d985924ecfad6cade9d01f5500faf3"],["/music/index.html","03f946f64098011312051c9e454ae9d4"],["/page/2/index.html","a476f63e126e5ded77ed8c5c64de9554"],["/page/3/index.html","0e42e08c78636f024242fe753d98ca75"],["/page/4/index.html","98c85e8a723b1a7c0d4947b3892001e4"],["/page/5/index.html","3fe50c24adc267d3e4bc77e74dd4f142"],["/page/6/index.html","225ea1d1f750f391cf87f9998ac459ba"],["/posts/1021360842.html","39ba291902c510a4fab62158722bff76"],["/posts/1120620192.html","799fffcd7b2e1d5628c651d6cfe664f8"],["/posts/1141628095.html","920d455012531a60073fa81a3dc490d0"],["/posts/1168613674.html","2d232a73f1ade5a3d1544850b778af63"],["/posts/1219920510.html","5dcdf428702f44d15b4a14f3a4adaef6"],["/posts/1222166338.html","9e0cd73390149dd446daa4ee23cf1de5"],["/posts/1259097482.html","d2d8a0b80255d1b9591965179fd2eca2"],["/posts/1271036369.html","c02f04ae6b939a78d20193a2d81f4bc4"],["/posts/1312847445.html","29f01f6b0d7323ab2ae609195d3e6e73"],["/posts/135355774.html","e24ed7cb1f16f10ce901d00c82410886"],["/posts/1375344716.html","bf2b8dd325757f7f625ea8ac0b621fcb"],["/posts/1388991698.html","d4022cf60aa0622c290b25aef83265fa"],["/posts/1410315814.html","da57ec157054b8309c86bad2510ef8b4"],["/posts/1452790229.html","702f682e43898c05931f1035938d03ed"],["/posts/1470079884.html","251b2c6c6db4ec674bf7e2e5ceca0f61"],["/posts/1470079885.html","be134f08a49b7ff074321a6a4ab0a7ec"],["/posts/1470079886.html","169ba67d24e8ef66d68bc20ec70281eb"],["/posts/1470079887.html","2beaa76f9df9f3842816c35b6a05d924"],["/posts/1498536549.html","523fc4fe43c17053f6839413567f4206"],["/posts/1547067935.html","3154f0c0d66df8e46b75848fd751dd8f"],["/posts/1557866301.html","d0b39097d6a47670c80a4607e5b8af4a"],["/posts/1571776361.html","0598246db7a09c1f55689cf7270b8ff7"],["/posts/1605124548.html","a95526e0644bea0480709cafe2666901"],["/posts/1633036852.html","41172142ea037a30828066d59ac3f3e8"],["/posts/1765123828.html","0084857560289bcfcdf53047fc9aa89d"],["/posts/1767336200.html","cdecfa237b9f828369c68646ded42aa0"],["/posts/1776114197.html","3173166dd7b8841ab472604ab6caf0c7"],["/posts/1817748743.html","5dac31666fe2277ac97d016db5c5782e"],["/posts/1925125395.html","7827b19e833096160ed9116642035c9f"],["/posts/1966191251.html","ff82cfe0b682c31f0ef6a872635b78d1"],["/posts/1987617322.html","41be1ec9fb51cc99ce3f709f3422070c"],["/posts/1999788039.html","9d50dbbe82310096764b1a99fbcaf989"],["/posts/2075104059.html","43a0051d0b21fe198444f3b04bbab2a9"],["/posts/2087796737.html","bb414b8dd332186648a54a7ba846b74e"],["/posts/2106547339.html","6800d9d909034f60d7cfaaf12aab8e87"],["/posts/2207806286.html","bd1e1a95aa2f2c738f526c6f27d612b8"],["/posts/2225903441.html","876826760951d27ce648cffffe45755f"],["/posts/2265610284.html","4026eea60ef24aaf76a72b2909bd64cd"],["/posts/2281352001.html","a7a848715c82593a016b22f2b3210fd1"],["/posts/2364755265.html","e3b0a9b39e8f3570b550543f810690e8"],["/posts/2414116852.html","70a7fe83235517950e35f50933917211"],["/posts/2421785022.html","db0bbb81d5abbf0303319a4bf0b6a708"],["/posts/2482902029.html","16946f1c29c7d9457cd44027acedba9f"],["/posts/2495386210.html","e9c42c96fa260a97f1d77b2eb95c34be"],["/posts/2516528882.html","eb19ad4d7b7f40e45e8bbad420b5edee"],["/posts/2526659543.html","f7bd50a850a6df666ce5abe3b39f1057"],["/posts/2529807823.html","24991b5989d17ae2dbccc60cb1ac1336"],["/posts/2596601004.html","c8df2b2c8c38e865f76ae7a3cc63b5b4"],["/posts/2742438348.html","3a9a55b4929fd141a4ceb070cc6ae340"],["/posts/2888309600.html","cf43df8762ea1deeea4887c0e42f9b22"],["/posts/2891591958.html","d79ed8916a049cbedc86f2870c13d549"],["/posts/2909934084.html","1bab6e0965cdf8aa701d94579b6e5665"],["/posts/2920256992.html","3d6fb3ebca81da96f8781e1579f5ecd9"],["/posts/3005926051.html","1ec5fc3d1aa43c6d7ba103a4fd51d693"],["/posts/309775400.html","4b3e6995668f46f431d21d1cd0a6c435"],["/posts/3156194925.html","a0900b9eb6238924479cd9b307d6021a"],["/posts/3169224211.html","c129d54e8752c0df3831b17c58138334"],["/posts/3213899550.html","e86c14d6628a581de73e592d8758f8ad"],["/posts/3259212833.html","5ec568c5a595b649dcd7b720c11010de"],["/posts/3266130344.html","12d51410be29ca969735621ab9bc6b67"],["/posts/3297135020.html","f9462e26671e684623914db337b54f62"],["/posts/3306641566.html","4746b118e4ee0fa2011264c9107a71f9"],["/posts/3312011324.html","3fae758dfce16e819c2843f6c5970cc2"],["/posts/336911618.html","01a9ea7c72a6450eaf09c208992e5204"],["/posts/3402121571.html","8a054fa4a07c3754e2181a9d226da19f"],["/posts/3405577485.html","93b6dbb9ea91b83bc7659649e599778e"],["/posts/3498516849.html","b37bdd57887d2a8277a38d5780790451"],["/posts/3513711414.html","c1722f62186bb01a63d927b2445f5468"],["/posts/3546711884.html","8036e0ca7721e2f412c3bf06656bc7ca"],["/posts/3731385230.html","cdaaa03971bf000e7e3e39ca631bea55"],["/posts/3772089482.html","ec29914cab33f8b9e5fb0f56cec32fab"],["/posts/386609427.html","b510a6a90b44c9ed2ed8c9a317b7c2d8"],["/posts/4044235327.html","cd2cdd794b1710e25f153e9ea60de8a9"],["/posts/4115971639.html","2f8889551e39ad6c30ca2c4aa69acea4"],["/posts/4130790367.html","f235f65b0deb48593cbecac13e9f00d9"],["/posts/4131986683.html","0f5f2df7c6d1b7e1082f52279a00d803"],["/posts/4177218757.html","2c6a8efbc579e29edffbc26e213713f9"],["/posts/4192183953.html","92f6c2383ce645367dd41223a8e8a0eb"],["/posts/4261103898.html","4cb937accd3907ef0729aa5ecaa78dcb"],["/posts/469711973.html","44f0cb4f4ef189f2ed4931358279d5cc"],["/posts/482495853.html","81656691f40e3d38036a23cd597aed6b"],["/posts/488247922.html","92f5e2824fe9fa4ddbc2cfbec5bd2a07"],["/posts/570165348.html","e0e4c9e7195825522ec5f1bd71a8e3cd"],["/posts/595890772.html","6bea16b53f43b4a886a80522de3e669f"],["/posts/67485572.html","4d231a1fd29804a22e1f323a7b1d8593"],["/posts/694347442.html","d868d44e0acb98d1b4485f3d0aff34b1"],["/posts/707384687.html","4899fd838be517f46c14eb66c2020aa0"],["/posts/71180092.html","8e76c781fc8cdb11a5c7cef4217c3a1a"],["/posts/716459272.html","faa083943c5798c16ba5a72906b2912c"],["/posts/778231993.html","db85ab3e17a5330a37c7d768f0659151"],["/posts/795397410.html","a7cd5e86386d8b6300869d5d075b8c40"],["/posts/820223701.html","37569866562dcd0cd546416271af5dbf"],["/posts/830372185.html","f889bc0a0130bfa80c859381fff296fb"],["/posts/88294277.html","934f14d5a73a5e8c7ab3711d0551fe48"],["/posts/939963535.html","94d4851f00f89d75238fe1cb5098f965"],["/posts/983786067.html","6c1f6f996f2d60af463df1416039d369"],["/sw-register.js","32ae23ce220d9f4b1c9fd62bf8eb0e02"],["/tags/C/index.html","8e3f363d09b2c47a899ec3ab3ffa10d7"],["/tags/C/page/2/index.html","6811fd62ad4143e74704fd134e8e5738"],["/tags/C/page/3/index.html","2f70e3857a84cd6259f45b696fec9e3a"],["/tags/ElasticSearch/index.html","9de5175f72e1ef7d6a5ee5f72c3a6231"],["/tags/GUI/index.html","fd8f6f0c7c0da97eac97e534729eedc6"],["/tags/HBase/index.html","996943b3e1450c1e9d220bb96bdd2f8f"],["/tags/Hadoop/index.html","cd48cfab776083b1e0d92d7a13d634f9"],["/tags/Hadoop/page/2/index.html","59ef92fcb7543adb759575600f97bff0"],["/tags/Java/index.html","cbc88087f77bb828ea1671f1ae8414e5"],["/tags/Java后端/index.html","0157f6f00c33e95eea805c2a35c0e759"],["/tags/Java后端/page/2/index.html","818cb102f6f2d77cf41c312878dd6959"],["/tags/Java基础/index.html","d6c4fa53c295a2bb8c95625ae6688577"],["/tags/Java基础/page/2/index.html","b2f55bc0fc52a460d501c28e2bfc8907"],["/tags/Kibana/index.html","8080fa3beddd3a497724b17a51ea1eda"],["/tags/Linux/index.html","2e951852e267131bc7c3b6c43e87353d"],["/tags/Linux/page/2/index.html","89f8972d3abdfc7cbcdac7cff9ce3a8c"],["/tags/Linux/page/3/index.html","2ed71f6eacfa2f869125d4b7c5003e00"],["/tags/Mac/index.html","7037a73de28dd8a2ffad84aad4a6d894"],["/tags/Mac/page/2/index.html","15d7c31c1e40bab210168a34b9f00a2b"],["/tags/Maven/index.html","f79d3fe6cdf22f43cb56fa1d5d981fda"],["/tags/MySQL/index.html","7f5bb059cf2d21d421349315b77ab252"],["/tags/Python/index.html","348f24e1eb8bbea4a7397ad20020de61"],["/tags/Redis/index.html","8a6390f2b49d21743cb91cf847bf40a1"],["/tags/R语言/index.html","795460a3a8ffa1ab5a65dfe1a9ccfa68"],["/tags/Ubuntu/index.html","2aeeb574e2e98a00b3e2392d28c61bb0"],["/tags/Vue/index.html","89138412d12ee930b3bef79885bba03a"],["/tags/Windows/index.html","adb178fff3e94e99e36cb7bf1f17f7ab"],["/tags/ZooKeeper/index.html","ce5ca1939cef08385c376dc8431d8620"],["/tags/bfs/index.html","1c3917015bfe36f8324d25851a02d937"],["/tags/dfs/index.html","2c78517b1b0f2f82e63538c8ffac0413"],["/tags/folium/index.html","cdf0f53e2fc59a41ccfb4cf4fd2c6744"],["/tags/git/index.html","0b01e32e2116c7aa389406dcfd85a80d"],["/tags/index.html","ef6a0a5d7197e6bf3f2fab90713431d2"],["/tags/latex/index.html","028444c384f2e64afd08347591c0f7a6"],["/tags/中间件/index.html","6adc7607642fbeb34bf4244c2f7aa419"],["/tags/二分查找/index.html","0caa974339012dd61cbeab8028c3d5a0"],["/tags/优化类/index.html","150a5e37a7c704e16daec1b4ea816806"],["/tags/前端/index.html","00249183132851e323dc9fa62a57b258"],["/tags/前缀和与差分/index.html","9a906afc62392da2f891525a2bdc09b0"],["/tags/动态规划/index.html","b6aa1ea7efa02f8703b5f44f9ecbe10b"],["/tags/动态规划/page/2/index.html","be4d38f31eace6e85a160a3b12c4297b"],["/tags/博客搭建/index.html","a79c59184be20dce93843ec901eaff2b"],["/tags/图论/index.html","feb1c3d4e8efa962221ece17ccf5ae4e"],["/tags/大数据/index.html","d1a67905ef3b8cc9499723cacdc611c3"],["/tags/大数据/page/2/index.html","3bf070ab527991c3640943aa25c05cc5"],["/tags/操作系统/index.html","86c735fb14f4c505d8dc19925427fe83"],["/tags/数学建模/index.html","06403c89f8af10fed0b66ed619034556"],["/tags/数据库/index.html","9a7cba678989840f75a882368932902a"],["/tags/数据结构和算法/index.html","fe4263962fce45ee8d443cf1689788a2"],["/tags/数据结构和算法/page/2/index.html","4650df24d4b81846821791e0da20169e"],["/tags/数据结构和算法/page/3/index.html","3a4f1870b26cb189357d9bb5435ccaf7"],["/tags/数组和字符串/index.html","1cccb9e423e22a7f5676f6a3b6535848"],["/tags/枚举类/index.html","00b3301faec0046650b9a31bbee76aa7"],["/tags/栈和队列/index.html","5940746ffe31577757b69238f6d6b5e9"],["/tags/树论/index.html","5d674d480bd870c768b6fcb36095b58b"],["/tags/测试/index.html","de76e5aa0ccad927b423e575461fdd2a"],["/tags/环境/index.html","bf5f84cb855621181827c770712d3f25"],["/tags/环境变量/index.html","a0780aa7ef15922ff8ec0e0d9df07918"],["/tags/绘图/index.html","9a71fddaa7c3754cf68970a723e4f95a"],["/tags/编程工具/index.html","11f626c9fbac9b30f7f517fb169474e4"],["/tags/编程环境/index.html","2415d8e3829adc67ca5b21f0528d459a"],["/tags/网络编程/index.html","df3abc653b938e41144784a6d647672e"],["/tags/英语语法/index.html","87d915fa2cc0e9c83c4e90ee6867226f"],["/tags/论文/index.html","bbede488d6306ad86de9142f3ba9e16f"],["/tags/资源下载/index.html","de7ee5bd722721a9a17378a197f92faa"],["/tags/链表/index.html","c790ffd5bc2b0f9fe67d8850eec35042"],["/tags/集合/index.html","6175ebaae274de628a0dafd8819ee332"],["/tags/集群/index.html","40353d25fc2224a50cefef814bc5a995"]];
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
