/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","e9feb6d86c7651b869961e39cc8a41b5"],["/about/index.html","e6ca5d681905dbc57c7de564dca95c12"],["/archives/2023/01/index.html","35ed2a95140ee66b55164ae8c5562446"],["/archives/2023/02/index.html","5a442a8fc9ae1cb838e1d4a177be74b4"],["/archives/2023/02/page/2/index.html","d27af1baa34b7c3dbbac633b56ae59d0"],["/archives/2023/03/index.html","5e2a0de6435c255096374a18a2eb4e08"],["/archives/2023/index.html","d9cdb749cb4eb479d3b424da5f101ce7"],["/archives/2023/page/2/index.html","1b8237b5740de6e4199ca384a2cd614e"],["/archives/2023/page/3/index.html","0ced49af5bf4b825120118678df069a5"],["/archives/2023/page/4/index.html","840a3a2bbea4ec8a202295c4cdfa950b"],["/archives/index.html","25a97e6bc26c37eed5692dd77ee83fad"],["/archives/page/2/index.html","314b78c90138d5ae104589f598084f7b"],["/archives/page/3/index.html","3fd6095314e4cf5a1e949f741387fd10"],["/archives/page/4/index.html","7c793f231cd09954856098169c850580"],["/categories/Java/index.html","409271b099f9b7ccf3fca731cc4009cf"],["/categories/Java/后端/index.html","cbf20936bdab2c28184dec4ada13c949"],["/categories/Java/基础/index.html","a8d70534827909efa5214b0ea0f06f69"],["/categories/Java/基础/集合/index.html","1f665c43878e2cd4d744940242d813bb"],["/categories/Python/index.html","bc2eb6683617cd9032a38c98fc0fc1e5"],["/categories/Python/编程环境/index.html","d0892e6f7fea575e24fd2c6af3b5c589"],["/categories/R语言/index.html","77bb50375c39fc5cabce94574951415d"],["/categories/R语言/编程环境/index.html","34a40c894633a40a582098c59c343524"],["/categories/index.html","f4dd00583ee1a5062bfc5ed4922ee486"],["/categories/大数据开发/ElasticSearch/index.html","4c7e855d2f5e064901f1a7b253e3cd9d"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","370b8d245e221e2966fa7dca002e44af"],["/categories/大数据开发/HBase/index.html","6df19b0bc4295116532e383af7ea4350"],["/categories/大数据开发/HBase/环境搭建/index.html","8fc2f9c2b3bf93bea70240abe5bff524"],["/categories/大数据开发/Hadoop/index.html","4c2acc6f4197f30af56d7511f4389bbf"],["/categories/大数据开发/Hadoop/环境搭建/index.html","2b6a2e6f8655913aeb9315f59eaeae8e"],["/categories/大数据开发/Zookeeper/index.html","e2d85afd629857b44ec8b90af1fd968c"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","054584025f91b56a99765706eb953132"],["/categories/大数据开发/index.html","be69cc2819e98940da35c229755add74"],["/categories/操作系统/Linux/index.html","089ae18518ca7c332cf50829a1b650b9"],["/categories/操作系统/Mac/index.html","951c7f3f3c489b11dbe3822151b2dab7"],["/categories/操作系统/Windows/index.html","740f4cf417dd4fef21256994e3ee37dd"],["/categories/操作系统/index.html","8eeec976b1137dc6c10b659c14f573b8"],["/categories/数学建模/index.html","c768fbd8b3b30e0b100ee4efacbe154c"],["/categories/数学建模/latex/index.html","ee03a48960e1425b23c411bf0befe07e"],["/categories/数学建模/优化类/index.html","70b4b9f19b4f48cc22cb2c06b00e109f"],["/categories/数学建模/优化类/现代优化算法/index.html","33a2b6dc587a4699f5d0591dba50a404"],["/categories/数学建模/优化类/规划类/index.html","2ebe06a0d8003729280f2e5030ebf420"],["/categories/数学建模/绘图/index.html","4e94c20963fa8b8db7fc2976d3fbb594"],["/categories/数据库/MySQL/index.html","7784f75dee7900d1c969dfc9dba33b18"],["/categories/数据库/index.html","c607395c3fb9f33f5a11e116ee3df04e"],["/categories/数据结构和算法/index.html","80d4f0f96fa90ec52dd50e429910ed7c"],["/categories/数据结构和算法/page/2/index.html","687515c0cc2148d19f1987835ec2f810"],["/categories/数据结构和算法/基本原理/bfs/index.html","6aa5a5627defcddd5a18d767bf0a6963"],["/categories/数据结构和算法/基本原理/dfs/index.html","6a4f25e996a606040359df05050b6c7d"],["/categories/数据结构和算法/基本原理/index.html","24b39526ec7ae8ac8d754e7ea2f94a0a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","92979095e9e21d57df8d51c61e15c44e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","494f4d089253e22da6a4e55d7b48cc83"],["/categories/数据结构和算法/基本原理/图论/index.html","c7e74c9bd6a9143cf1d238c0cae9bac0"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","2fce947fd980dbcc25ea792f739953a0"],["/categories/数据结构和算法/基本原理/数论/index.html","cb0011e8df11159102f199029a7e7ac3"],["/categories/数据结构和算法/基本原理/树论/index.html","11f212c2436009196eba24f7cec84ecf"],["/categories/数据结构和算法/基本原理/链表/index.html","0a87975002337617b6d448ba0a4915c7"],["/categories/数据结构和算法/算法题/index.html","fa00ce821bbdb28cd506a23a70c32505"],["/categories/数据结构和算法/算法题/二分查找/index.html","f8f904a60e1bf2783c50b346a2f6f95b"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5679d4a49c0da153e9af9f5e24ca4fdc"],["/categories/数据结构和算法/算法题/动态规划/index.html","dba811ffe38fdc8feb3c5235b75e30cc"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","afe602c66d887d55308e6a012be433a5"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b2c602daf70f557babc159213a3ae7a2"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","ee751e89c16a0d142bb64cfe4e1830d4"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","f15f2afc096eb05f4c26bfe6f9e33bc7"],["/categories/数据结构和算法/算法题/栈和队列/index.html","6462f198cf4cc10c13fe349fc5622ad2"],["/categories/数据结构和算法/算法题/树论/index.html","ad629be37085b6223c3bd461d762ff40"],["/categories/杂七杂八/index.html","5c591f17bbd2fdccc9e31d4574140574"],["/categories/杂七杂八/博客搭建/index.html","5dedd77ded52c6396dd1dd98361dc4ac"],["/categories/编程环境/index.html","cca51502c055d48d38e61b45ac5c20a0"],["/categories/英语学习/index.html","a519e28c97505918bc1ddbb5078b9a21"],["/categories/英语学习/英语语法/index.html","b4dcf377852825659b904beeaba5d097"],["/comments/index.html","6874547968bc56e723dd6d9dbc6f29ea"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","00897db7fface0678d41e9e255949e7d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","efa66b642873d8a673b4f671c7c1681c"],["/movies/index.html","39a05919493bf6c545c514e585baacc5"],["/music/index.html","97961ee5c7ddb772beae1f749b9aa71c"],["/page/2/index.html","a447dce82e6b77eee44ed4080264c97c"],["/page/3/index.html","37c992c0c00ee57a9c39a15156bf1e74"],["/page/4/index.html","0d442d89d2b43bfec4bcb9b2bfe875a1"],["/page/5/index.html","bca22a002fed7e080243ea43348c1d2a"],["/posts/1021360842.html","14bc69c962ea2065de95a38cc967e05e"],["/posts/1120620192.html","f589c850984bb150c896f67fb32bf352"],["/posts/1141628095.html","f6478bd35a7c7abd3a1ae8b6d1d8a614"],["/posts/1168613674.html","9a77d363ac9e30c9359f92fb302472dc"],["/posts/1219920510.html","b94af3cbc99bb2ad9e54f183d6b31202"],["/posts/1222166338.html","e430042c5c4746d28a7ccd2c07441667"],["/posts/1259097482.html","2368c3338c3cdf0f65100217af658158"],["/posts/1271036369.html","0daf442e5f6cded44b1e5e21da05e9ba"],["/posts/1312847445.html","1d151d638b8a2b3ca2d570422c38f480"],["/posts/135355774.html","e6388b3e11644418a6653e1113baf70a"],["/posts/1375344716.html","c43172cfcecff56ae1adcc57e75d2e39"],["/posts/1388991698.html","740ec05f1370c0abe91761cc68203537"],["/posts/1410315814.html","7129fdd6cb2f00e40f178ef1304050f0"],["/posts/1452790229.html","56dbaa4b9443c83ef9d8aba4fabfcdc6"],["/posts/1470079884.html","cad51ffd55ccdddc2db711acd9ddcdba"],["/posts/1470079885.html","9f780aee31bba0418893b2511e403449"],["/posts/1470079886.html","a078c8bf44f37a8b0fe7d35b1c0edb73"],["/posts/1470079887.html","9eaac60440d052588c612134e2b55e3a"],["/posts/1498536549.html","a9c161fe3ed408060c78b0a83bd4572b"],["/posts/1557866301.html","b638170ad4a981310914a83f1eefd594"],["/posts/1571776361.html","d507fc3f47eea6b9f6c7b51aa03f27cc"],["/posts/1605124548.html","5dd56ceb5a5c86a81618bdb5d8a900f6"],["/posts/1633036852.html","efc696600217e6162609310d145e6e3b"],["/posts/1765123828.html","83bd93d3e5d7196dbf3edeb326770fcc"],["/posts/1776114197.html","9e8f2af3dd609318da3f4b0e26d07f90"],["/posts/1817748743.html","8be5d42764e151ad912899429032394e"],["/posts/1925125395.html","169325b79f2390a54f17e9f4a136ecfa"],["/posts/1966191251.html","15edabcc35b9d8c73ed50c2a27408828"],["/posts/1987617322.html","8bcff9d551034dfc1b767a8f89a683ad"],["/posts/1999788039.html","7b94ca7a5115ecddb6605cb5ba7f708c"],["/posts/2075104059.html","581aa7fd88902802893c9cead98c201a"],["/posts/2087796737.html","0709ac6a44c2e32cb452be862c1083c4"],["/posts/2207806286.html","f4b61805903770011e98390256cb6bc1"],["/posts/2225903441.html","44075093f83682aa3058d3e624caaa31"],["/posts/2265610284.html","545df5004c4f5d6bd57d4654f4bc334a"],["/posts/2281352001.html","52c157d1379b04b57ba27252f58c9e32"],["/posts/2364755265.html","cacddbff0120e560a55bdee83bac5114"],["/posts/2414116852.html","5015949812152bff66470e3410068d1e"],["/posts/2482902029.html","e167b0a55a405b00a4b0d3e7cd483d12"],["/posts/2495386210.html","805b87d7ddffa442c2e92849fd642904"],["/posts/2516528882.html","88844594f390d9e44f3c8ef5970975e5"],["/posts/2526659543.html","9d4da011a4573d45d90f52f8296ab7e0"],["/posts/2529807823.html","e743db069e17a7fd0a74138c4957f1ec"],["/posts/2742438348.html","eb9c00786b178084a6631009746485bd"],["/posts/2888309600.html","1d5aa37b380bcd1b8e6844e4a04aa1ec"],["/posts/2891591958.html","fb54893c624fb831b38cd5627cff42a8"],["/posts/2909934084.html","7ad088b276df738024cd43f0a01f4a3f"],["/posts/2920256992.html","ad35d9c53016631887c8889f2619a4c4"],["/posts/3005926051.html","b9cbde344d3818eb5197bd15a3d11f67"],["/posts/3169224211.html","9398ee1db75cbac116d3096b3d1d9bcf"],["/posts/3259212833.html","316cd58bd893a0a22dceeef25e2c362c"],["/posts/3266130344.html","9735bb0799dca99142b1c1ff651ef491"],["/posts/3306641566.html","1a1b4313818015bd8e6f05fe76069336"],["/posts/3312011324.html","1f4d27d9f7d051565017cd0d9562be85"],["/posts/336911618.html","933b1bf5b21242215d04816fb1a12c43"],["/posts/3402121571.html","286339d0d43c60f45082cb4e84e2936b"],["/posts/3405577485.html","e2f2f208e01602aa2184d5fb31496894"],["/posts/3498516849.html","3b5d00f08e32d251f05e81363e0ef450"],["/posts/3513711414.html","8c6008cbacf8946609236d5bc9c0d7e7"],["/posts/3546711884.html","1540c4865c3014981c186a8c2c5ecdbd"],["/posts/3731385230.html","b6534926b23b865b0edcb86121e0ff89"],["/posts/3772089482.html","3a3f247c8891550cab3b5abcb1c3b433"],["/posts/386609427.html","0aa2ab063b8bc5e8116d196196d1b899"],["/posts/4044235327.html","db72886d6b4d74691514dc9b888deb11"],["/posts/4115971639.html","6ac870d960a22852c34f06b92afd67a4"],["/posts/4130790367.html","f69f71f3e24b803e228d7a71894ce75a"],["/posts/4131986683.html","82024abdae4fdd6fe6f132090bb9bec3"],["/posts/4177218757.html","f48c41a575a440205a5b371705683cdb"],["/posts/4192183953.html","86c094f8c822d6522e1efeb475ddfb1d"],["/posts/4261103898.html","eb5c80cce49d7a57ceeb196c2d4256f9"],["/posts/469711973.html","5fe4852735b7ee68189b946f4ccb5591"],["/posts/482495853.html","179da402919c36d0316cfaacb7f74642"],["/posts/488247922.html","ce5f66bc5b519d50eeb9cf39cbdc745b"],["/posts/570165348.html","dff895c2ba9a8fb64a7d597ab0090314"],["/posts/595890772.html","2ebd081429ecaf3b4831077755a62ba0"],["/posts/694347442.html","341f9283912f8d8736ea281d3692f189"],["/posts/707384687.html","09402c527dc67f52e0695865b4ea860b"],["/posts/71180092.html","d704faca320f797ee75ce26b68028581"],["/posts/716459272.html","4b46ff88949e67aecd6ffe8840206492"],["/posts/795397410.html","b8cf7ce6420a5df0b1e31453dc040e43"],["/posts/820223701.html","dfe936f98cef2e1b1e57a273e122ff1e"],["/posts/830372185.html","7cb595de0e2f49269765fca6435415b7"],["/posts/88294277.html","ba62b69898f95e55087585b3cb7118ba"],["/posts/939963535.html","c749f37d9be3f7b52da1e40419b80d1b"],["/posts/983786067.html","8f6c2b2ad5ad504878dbda521eba0002"],["/sw-register.js","963b4a381ad2496e87ed6e4530290ec0"],["/tags/C/index.html","719130773e6b80e0ddea3eb01d6d6932"],["/tags/C/page/2/index.html","24fc17c273460111fcca6b23dd9ece5d"],["/tags/C/page/3/index.html","26ca75d15a192e1c187b0f14dbed97d3"],["/tags/ElasticSearch/index.html","e71c890017f39e0be0a841edd689fd92"],["/tags/GUI/index.html","fac95a8eeb4d8fdd5b202767674ddeda"],["/tags/HBase/index.html","74c9c0fc776c37d6b39b1249a8f1fb99"],["/tags/Hadoop/index.html","49669c01e81e39dd19c320f54d7091d4"],["/tags/Java/index.html","deb2bbcb20b197b1ec1751ec9ada5639"],["/tags/Java后端/index.html","0d9abbb00b69ed75820a46bfa839a6e8"],["/tags/Java基础/index.html","e19daa476773ef05642d62b3ec8b66ee"],["/tags/Java基础/page/2/index.html","2b082c8b9424bbabed7c3242ab399278"],["/tags/Kibana/index.html","926813818bea40fb27c02bb864f428e0"],["/tags/Linux/index.html","44bcf71232412a241201131c92d10338"],["/tags/Linux/page/2/index.html","b24584b1f31e50e538fcd0b67839dbd4"],["/tags/Mac/index.html","9b5e5934a8e5ed10cffd3f9e78eb66cb"],["/tags/Mac/page/2/index.html","ef16b5a957375c26bb19cbbf10963621"],["/tags/Maven/index.html","9533970cbf4d8db55a6be6e1fe830d65"],["/tags/MySQL/index.html","7f905cf423dd5a7a02d363b8d0c06790"],["/tags/Python/index.html","d3ade9a09ed16eaa7bf4502ed9fd31ec"],["/tags/R语言/index.html","f9a00948ceb1137e185b58f60e6a810f"],["/tags/Ubuntu/index.html","e082d7b88c961d6b5d0bd73e8552a236"],["/tags/Windows/index.html","f2bb6ec4b4bdb8b0cf19bcb606bfaac4"],["/tags/ZooKeeper/index.html","4dd3c64afd71f98c2f45cc90de686d01"],["/tags/bfs/index.html","c3a3a7360460d7c84a6458328f471ef8"],["/tags/dfs/index.html","e83bc3047c8e46a8116becc56c4d0bb9"],["/tags/folium/index.html","75554eef17fca48571f445647e5e5c5b"],["/tags/git/index.html","18ba1160d108e7e20aac03af81e02fe6"],["/tags/index.html","99ef343289564df2d103bebcdad08700"],["/tags/latex/index.html","7970604b5dcf414bb67df03de6c55691"],["/tags/二分查找/index.html","8ac05e54001ed82e81bf886f10ea8156"],["/tags/优化类/index.html","48d54a51a32bc49c661398d5a6a4026d"],["/tags/前缀和与差分/index.html","42a891a1a25c64082c94b407ce4f9ec5"],["/tags/动态规划/index.html","19a328b2993826d43f7c38aacb4b22d8"],["/tags/动态规划/page/2/index.html","73800037af53a668f705c02e4d53f579"],["/tags/博客搭建/index.html","b07177f0997f0f99de5c481905091151"],["/tags/图论/index.html","11c753e97bc7dbe98d0d2a52f4b72777"],["/tags/大数据/index.html","b4686815ce865e8d81664a9ed9c351be"],["/tags/大数据/page/2/index.html","9d37232cf99fe4d2f57bc399be7af040"],["/tags/操作系统/index.html","ffa257e9fb49b1ebde4672e163d35e92"],["/tags/数学建模/index.html","1edaf1753cb49d06d13eada9956a94ca"],["/tags/数据库/index.html","0f19863167af1ef9ca06946ac50043a8"],["/tags/数据结构和算法/index.html","2b36ef9c8f13c1b3f22f8f3551948f46"],["/tags/数据结构和算法/page/2/index.html","a539c16f7c5a834c9049048056f8873d"],["/tags/数据结构和算法/page/3/index.html","9e852194b5907877b1eb7345871259a5"],["/tags/数组和字符串/index.html","c87305e9953b17f14fcda4a184cb1221"],["/tags/枚举类/index.html","cdd8fdf8be5d7cba13c067a25fc835bb"],["/tags/栈和队列/index.html","ce15481d65c201c7d6685a643112f70f"],["/tags/树论/index.html","64085fe0bcb2bfb8814e59f44a18976e"],["/tags/测试/index.html","4425b8b5539e9148779899d6d39e6758"],["/tags/环境/index.html","400b107bc0953c3ce0a6e2d7ebe9fabf"],["/tags/环境变量/index.html","e1143930d4060feae61cc9a47af53c99"],["/tags/绘图/index.html","94869bedd1d0e664850b6707e13965a6"],["/tags/编程环境/index.html","8a313b226aad8f5e193859e74facfd4e"],["/tags/网络编程/index.html","8fc72f62b0b0fbe6e89da1a1844c4389"],["/tags/英语语法/index.html","a7a75b6ab14b9ca434469211d717c01b"],["/tags/论文/index.html","1463ed84de700d0d60d145f981b1c0d5"],["/tags/资源下载/index.html","7ad0ea1c743290d1f93f9a3f3ec9983a"],["/tags/链表/index.html","e02223cede6c7f715358429031ba6b87"],["/tags/集合/index.html","ee703e225195ca02ec17b6024e2357a2"],["/tags/集群/index.html","87c1e61427116a20fcd6a9f63960e0b8"]];
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
