/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","435cc1e266fa39c2649fb46419c71b6b"],["/about/index.html","4fb408336f92b4442521836b876f728a"],["/archives/2023/01/index.html","7f4ffe33136aaa5bec1c9de0d72774d4"],["/archives/2023/02/index.html","2167412fae84f8c4aff9dd975e23bc71"],["/archives/2023/02/page/2/index.html","41fbeef9ee0f4b107bbfbb1754aa99eb"],["/archives/2023/02/page/3/index.html","ca4875943e8e33ed2609c64067583266"],["/archives/2023/03/index.html","cc21a5d9e9e5c276e5d4a2f198b111a9"],["/archives/2023/05/index.html","cb3b7d4aab977e6a21486c6a66365d94"],["/archives/2023/06/index.html","9acaa8afc501dee8f06a5a1a21c2e497"],["/archives/2023/09/index.html","13ea7bf58922457e746c14b9cb493d17"],["/archives/2023/11/index.html","c38fabe1ebb8285d6dd688bfcbd58eef"],["/archives/2023/12/index.html","f41f0201a94eee9e2e1ba2dab4d6f74c"],["/archives/2023/index.html","6ec3047517e36c067e05b4cc1f96a748"],["/archives/2023/page/2/index.html","3424acd34ba3522587a0bbc5d503016c"],["/archives/2023/page/3/index.html","59839d81ea015932fcd8f563b8ed0157"],["/archives/2023/page/4/index.html","9863ae3a05dc46c6cd6408f7587b91bf"],["/archives/2023/page/5/index.html","847863f0bc389d068f4b487227851eb7"],["/archives/2024/02/index.html","881e0575c710db8ae7781499dd23952a"],["/archives/2024/index.html","836643c4114b6b7eb0b369455f89f20b"],["/archives/index.html","70389290ce71c0bcb6dcd52ada84da1d"],["/archives/page/2/index.html","a916db319df55585f1eb574bc9b3707a"],["/archives/page/3/index.html","835604351fb902a79a0464d9e212018d"],["/archives/page/4/index.html","a3f6aa244ce7a6cc64efe69bfb08a7a5"],["/archives/page/5/index.html","b75b1ddd592bc6bed3e4a07b583809e5"],["/baidu_verify_codeva-qQP2iZOMLX.html","bfa72b1a755878910d1e78a72f52b814"],["/categories/Java/index.html","d31b6c4c88f754997a5900678ac76c62"],["/categories/Java/后端/index.html","06508199482ef544817e6b6647184901"],["/categories/Java/基础/index.html","60a6afa9835d381f4fd2b4187a1095b1"],["/categories/Java/基础/集合/index.html","1db14b84aebbb53a1b9e6d7cfc321a1c"],["/categories/Python/index.html","48abb7e001c7036c9408fa0ff03852f2"],["/categories/Python/编程环境/index.html","518c10b391f39681565546a84bd1d6ac"],["/categories/R语言/index.html","382c4094551209c07c21578d79e31fa6"],["/categories/R语言/编程环境/index.html","9c6b2b65d30c0b45296cc288d8c5d1e5"],["/categories/iPad/index.html","83b93d50fc29a088699a6acde9f92c5f"],["/categories/index.html","1a32912567a9257c7c0cd0ae5f9cb17b"],["/categories/中间件/index.html","630b3da313b9149de5c6d0dfac4386d5"],["/categories/前端/Vue/index.html","d37f4e81927aba1cab5b45629458c68d"],["/categories/前端/index.html","da887da0dcb0fc05c885c874b2123fec"],["/categories/大数据开发/ElasticSearch/index.html","ab2af35b2ae34d056f99d76eb3b9d152"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","a5f80e8d55c73f78604b342d041524f5"],["/categories/大数据开发/HBase/index.html","79f31ae453e1daa9e5f9d78792e7a669"],["/categories/大数据开发/HBase/学习笔记/index.html","91ffed76874399be67cfc8e96a3ace8a"],["/categories/大数据开发/HBase/环境搭建/index.html","89e260f26127c2880d03fef571d49250"],["/categories/大数据开发/Hadoop/index.html","8d9d28485e06d8dd80a6bb97b6e0aebf"],["/categories/大数据开发/Hadoop/技术/index.html","9a851b094862eb8cbad2d5580af1dfaa"],["/categories/大数据开发/Hadoop/环境搭建/index.html","55dda57847aa56ffa13ed11de426133f"],["/categories/大数据开发/Redis/index.html","cb19ac5f6adbb0cc24944ae0a3566457"],["/categories/大数据开发/Redis/技术/index.html","0a1314b3ddab2eb05a6638c748c3354f"],["/categories/大数据开发/Redis/环境搭建/index.html","66802b6143b4fb77abfafdb19745c7ff"],["/categories/大数据开发/Spark/index.html","a5ebc5a08e4fc8909707ac502786fc40"],["/categories/大数据开发/Spark/环境搭建/index.html","539da81b09337c1e775d77d2c6da3be1"],["/categories/大数据开发/Zookeeper/index.html","e184915b92c2285bfbe8c832a9b23ae7"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f95547ac1b5009444cd23e55bcb44b8b"],["/categories/大数据开发/index.html","9f824a36c8900b825066733f1436425d"],["/categories/学校课程/index.html","26c16bc20ec4de4ff6a13c71e000b15f"],["/categories/学校课程/计算机操作系统/index.html","26fdc37cf5a42cc867f1b991a1f550e4"],["/categories/操作系统/Linux/index.html","0c71c1243c1d22ee3ccb6660a3791b21"],["/categories/操作系统/Mac/index.html","84cd921d99f358bfe73d3305eef9b705"],["/categories/操作系统/Windows/index.html","b562b2b7e1cb84a757068cbdc97a6c3e"],["/categories/操作系统/index.html","0915bfc22e078d77b8af8c938c3f73e0"],["/categories/数学建模/index.html","7380ae413ab73c08a2c06d0d70ad79e9"],["/categories/数学建模/latex/index.html","eef8c62ae803808e1c895b77b3b3adbb"],["/categories/数学建模/优化类/index.html","370522de6129cb24c865094d4f4007ee"],["/categories/数学建模/优化类/现代优化算法/index.html","11264a70bb6961b2e4c16b8cae33c5b5"],["/categories/数学建模/优化类/规划类/index.html","5bf06ca5be897f892619fb98d3869509"],["/categories/数学建模/绘图/index.html","71dfc6023eb704957f46e0ede9617196"],["/categories/数据库/MySQL/index.html","fa6c169a163c40f1c269532a5af50773"],["/categories/数据库/index.html","5447a5c90d301041c9ed471b8a094bce"],["/categories/数据结构和算法/index.html","1bd7f196aa9ff1adc6114045e05464cf"],["/categories/数据结构和算法/page/2/index.html","a19901bad854e74b9e2a33b652c45a01"],["/categories/数据结构和算法/基本原理/bfs/index.html","063d934ee10f08e2fba334879cede68f"],["/categories/数据结构和算法/基本原理/dfs/index.html","03b957807b209c52a985db87f67aea0b"],["/categories/数据结构和算法/基本原理/index.html","cbfb96ccb7a673172f25befb6a22d4ba"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","34042403e971a5f319f274f70f844759"],["/categories/数据结构和算法/基本原理/动态规划/index.html","1578978736dc126dd2af343d9d331d9a"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","90a927f021ae553d92947b9eaef3df5c"],["/categories/数据结构和算法/基本原理/图论/index.html","8d00ceeb7b6bfaca016adac41376249e"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","b54e45dd6c8e40532660194c2dec16a4"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","2a0fb2b1411509dc754d51ff7f4a093c"],["/categories/数据结构和算法/基本原理/字符串/index.html","5b95ffee045f0d3da72efb52a2197e4d"],["/categories/数据结构和算法/基本原理/排序/index.html","af3a443a512dd9eb096d7d244b3ad5f2"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","085c928b69bb507408e58f01a763cdb3"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","e6932d7466a6a3d6c42e29b4310cd1b9"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","74ddb5522eb5ba35635cf29da767fec6"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","62ac983f5a36313a57a3a590d9055ce2"],["/categories/数据结构和算法/基本原理/链表/index.html","c678a79845899a701a21f423d6cfe053"],["/categories/数据结构和算法/算法题/index.html","0f564ae14349fb1509b247c1bdea00d4"],["/categories/数据结构和算法/算法题/二分查找/index.html","3abe81d078088796a4c5180c7c4d1efb"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","410bd5ee903ab51a1b28b893841fbc4b"],["/categories/数据结构和算法/算法题/动态规划/index.html","a0f76b3d06b6d89938b374051d811038"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2194989d020ec52e08e13026267f1d00"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","15e65f589c877de52a513a17709c8eb8"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","522f7f2c098a7ac14360fb7d8c5289ac"],["/categories/数据结构和算法/算法题/图论/index.html","d5debb7edd918a258c6c0c3e7b4d8584"],["/categories/数据结构和算法/算法题/图论/树论/index.html","11195812d47797980a98a8ec2c5279b2"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","9848f1d07bf040247456eb898d588a8e"],["/categories/数据结构和算法/算法题/数论/index.html","971239a88fb1d4d6ba1929d7be72420b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","92262d0688e9bed622541ec39577f19f"],["/categories/杂七杂八/index.html","d0216e228c8c92c85a201d71a96eb388"],["/categories/杂七杂八/博客搭建/index.html","68dd5ef05f7290568acfa0b9a1115918"],["/categories/编程工具下载/index.html","e82be3c8a95ee2e11336e23baf45b6c4"],["/categories/编程环境/index.html","21334e31f03cad210e991b4497eecaee"],["/categories/编程环境/大数据/index.html","cff28b42a139f690d5f1e8588adb4528"],["/categories/英语学习/index.html","e7ce5d867f3a826052ddc231e2227e65"],["/categories/英语学习/英语语法/index.html","e49d8f92ede77c7c7c3317210a32559b"],["/comments/index.html","a3080eacf68fab95c27ed85e900e961c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","70dbe8e37fb9c2d30d5b21f0e54b378d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","65bfa0643dd31a494c84bc1d044a056e"],["/movies/index.html","2e77cb01c364f08b0882bac91f50f9a3"],["/music/index.html","d9991555fc1800868a717ecf3d443336"],["/page/2/index.html","4db9b1debaa01bfc8924630965a3d56f"],["/page/3/index.html","a5e5011cbab9f8a8b301d23dfe0bb39f"],["/page/4/index.html","4f58345fd01d9eff3b7986a7763e4c1d"],["/page/5/index.html","24ab9c244ad15ed0c54f561a0a306a80"],["/page/6/index.html","2c3318cf1908e89facd618fa2be77110"],["/page/7/index.html","faac93fa7fa068ed4db0a2adadf46edb"],["/posts/1021360842.html","87650d53d5a6971ad956946f64bdb86f"],["/posts/1120620192.html","b9d82b0567a06d4eac0eff4c161db074"],["/posts/1137707673.html","ae47b1d904892afd3dff4ae2ec5e9ff9"],["/posts/1141628095.html","c1bdb069757242b322d17cd72c92a313"],["/posts/1168613674.html","754e11a70ee62369b56239ad0b6ecb25"],["/posts/1219920510.html","d89f586cb0e3f989685ec18919c7a631"],["/posts/1222166338.html","6ec1dc0a6d2e3499bab5741583a761d2"],["/posts/1259097482.html","a9a0311ac09b631d29d6fde3d0b37ed1"],["/posts/1271036369.html","91fb91dcb1bcbf8b6e71266ce6d11b89"],["/posts/1312847445.html","c0e73efdc6f8ff7338de7a34f054c54a"],["/posts/135355774.html","c37729cab4d88eb4255c8fdf29e29d18"],["/posts/1375344716.html","4c6869be815c1b158bc0b778a65de4ca"],["/posts/1388991698.html","e5c82f1123bd09fa085ea149ffa71565"],["/posts/1410315814.html","9c1431f2127c4aa07aa7a5e6a95b3ef7"],["/posts/1452790229.html","9195f4d61f4b8df631db6b463a620672"],["/posts/1470079884.html","b5ff12862faf79d36f3d9fa723aa9a87"],["/posts/1470079885.html","674648291d1fc39df75a105297221f31"],["/posts/1470079886.html","fd05e957b2a5a6559590ec1276971e95"],["/posts/1470079887.html","0db1f6abb511299941ca37229a8d873d"],["/posts/1498536549.html","da8a6ad7c871eb8d8e7765e66c756702"],["/posts/1539568593.html","a859477618fbf0c5dd272cb03430e14f"],["/posts/1547067935.html","98f5e932d8c8059c7bdce6f1723c8a6f"],["/posts/1557866301.html","84d26d8411b08fcebe5eaca38c762e6d"],["/posts/1571776361.html","c3fe3ee0ad77d173506a183f19724f79"],["/posts/1605124548.html","4790a20a380098aadeccedd35328094b"],["/posts/1633036852.html","b0e8822b126af95cbd9f1094b44d4503"],["/posts/1667740714.html","481b20159f469e6e956bb5654fe62b3e"],["/posts/1674202625.html","9345e601d32657ea50762ede9b2d77bf"],["/posts/1765123828.html","c65a95342886f75231f4b6e800c7f197"],["/posts/1767336200.html","7bdf93de0ffd0abb08210716a1e4b651"],["/posts/1776114197.html","6212dfe8235a6029a7cf530a158ed902"],["/posts/1817748743.html","937817469564a44890a5a0612a3d82e6"],["/posts/1925125395.html","d4b71333bbeba936d4582adb0005bd67"],["/posts/1966191251.html","282b24699d6f2c7c308d0f0735e8defd"],["/posts/1987617322.html","7d2132e13cfbbce15aeae0afa9097e7c"],["/posts/1999788039.html","964b288b4f3968d756cfce317f60b664"],["/posts/2007534187.html","959f9cd62fa4ee5587d7925e64cafa93"],["/posts/2075104059.html","91d23f1157b87ef33940c15e6ef7eb9c"],["/posts/2087796737.html","9de33608b072df169ddc94322bc7b008"],["/posts/2106547339.html","b63ff8af639bff935ca12424b5e18d86"],["/posts/2207806286.html","8fc6994bc6a107929f167e1d8b58b635"],["/posts/2225903441.html","d2697768884490f08393c55c121c66c1"],["/posts/2265610284.html","5994cad8ee32be57513e88baa0488f7c"],["/posts/2281352001.html","c9dd6fe947250d04b59b828e637e0cb4"],["/posts/2364755265.html","413cfa86e7016e01425a6489ee374470"],["/posts/2414116852.html","63801ef9a2d4cdb2c5d133f051b7c2e2"],["/posts/2421785022.html","9326fb5766b1cbb9eb2516db859bdaef"],["/posts/2482902029.html","b924dbbdfa50e51becc16861e8b9fe32"],["/posts/2495386210.html","9f36fcbb78c75ed31d3b8ee47a4abef6"],["/posts/2516528882.html","db8ba4db4f5201059bd5a01b82ae197f"],["/posts/2522177458.html","1852d16f9e378df57d58e4d9c992081d"],["/posts/2526659543.html","3224280fdf98bae418d239328c8cb443"],["/posts/2529807823.html","783d1a05752e6563fa22036019982838"],["/posts/2592249117.html","2c4f7fa75b7be2073c860e6a9caaf008"],["/posts/2596601004.html","c418722b1a7860f4a9ccc7ec017f56cd"],["/posts/2697614349.html","008e0cf281ae6f218a6a1c93d073764d"],["/posts/2742438348.html","6761717a26e1b62ef703a8dff3f14bfa"],["/posts/2768249503.html","9f83f8fe22662caa3a13388880686c0c"],["/posts/2864584994.html","e4cae15491c2685f959cc132ba9da750"],["/posts/2888309600.html","6e85f3fc9a7f5757e083f80d042aa854"],["/posts/2891591958.html","0886ede95f43b42bbc6b4659473fd8f8"],["/posts/2909934084.html","39234f508b8d3e41cad046a07273749d"],["/posts/2920256992.html","7ef1dd8c7bbc2eb65ca2cb928e042832"],["/posts/2959474469.html","ea796dd253729b2bbb8d33e43b9697ab"],["/posts/3005926051.html","2eeebb37dd7414f16883aa67725b064e"],["/posts/309775400.html","7fb29ef1747fd5607cbeb6ef57c44151"],["/posts/3156194925.html","ed77f58e81415aa31773ebe4613e2c98"],["/posts/3169224211.html","cfbb40a6f7d8b7a55b3fa831fb51c614"],["/posts/3183912587.html","4d44c157249fcc981fe1c90ac422183c"],["/posts/3213899550.html","8ab4f9ae15cb478d50370089268a5742"],["/posts/3259212833.html","18e7d42c6eafa96ecfa9187ed6e047ca"],["/posts/3265658309.html","c39a72231ac3e1b5bd371927dcccdf48"],["/posts/3266130344.html","17c44930644621cc9c8543fe57f1c0d3"],["/posts/3292663995.html","f156321f7dbefda6d821912f71887b59"],["/posts/3297135020.html","3cc961c9a38344ff7d99215c7bcce578"],["/posts/3306641566.html","b455818cf7ad156883afa09a791e8419"],["/posts/3312011324.html","f26afffe130bab5de9bbb6aa85c7a178"],["/posts/336911618.html","b3f518385aa31debe0036356fc1fa868"],["/posts/3402121571.html","724162622f22fd1b6633ed8aeebe0d71"],["/posts/3405577485.html","b3f25ad5b239522e386b6e8189a9d9d0"],["/posts/3498516849.html","7e27491d8b6937bf1257f0ae79fe197b"],["/posts/350679531.html","58b9f8c92500b27d6c425a209c651429"],["/posts/3513711414.html","8a5f5f3ce0797952595d4f16f91f1b44"],["/posts/3523095624.html","3b828fe126ca4d1824a410939a37dacf"],["/posts/3546711884.html","4af1f0e2a1f452cf1612c227381e11d7"],["/posts/362397694.html","a918fdbfabe415dea7d94f1df049f6ce"],["/posts/3731385230.html","c3a992f2d46883a4097fe12216b626cf"],["/posts/3772089482.html","8c0e6cffc6df11ce29f6e0ba625b9e08"],["/posts/386609427.html","a439a29f0430474373645640592727a5"],["/posts/4044235327.html","6c617987d3f17b2c53702c51cf28a17a"],["/posts/4098221856.html","c2d3c7aa4180ec1f8c8576e68e03cde9"],["/posts/4115971639.html","ef88b6fc01d755fcc12783823d7790f5"],["/posts/4130790367.html","6fd151a6b286a5c3e170625b7b695884"],["/posts/4131986683.html","0dc1eed865e0cfcabebba413dfd9d526"],["/posts/4177218757.html","6672b0479309aa128d40cb479c5c0033"],["/posts/4192183953.html","c62ca37bb0b542296a424846808efd02"],["/posts/4223662913.html","fc93ead3b99cab5b8eb84a7c68c0b945"],["/posts/4261103898.html","011c81edfa80962ed89332cdc114e6e5"],["/posts/4286605504.html","261171440742d6dd10817ff92e38e98a"],["/posts/449089913.html","a515692dde2ea50ea22f80dbfd60ab2f"],["/posts/469711973.html","77433ba22d79b0ba1a7a7e03055f661b"],["/posts/482495853.html","f7e612da496abe5126b711ef3c710515"],["/posts/488247922.html","59f9b0cc3dbf00ef79ca947bb8c52360"],["/posts/517302816.html","9f885b2de56fb1ca8afdcf4f2ddd3f49"],["/posts/570165348.html","28af51b4e36138b3f98fcbe751399ae6"],["/posts/595890772.html","b81ddfba48d21c1c6c8c09e1831cea0c"],["/posts/67485572.html","2b8190c34cb6b5d22c70c73b8607d3b9"],["/posts/694347442.html","5ebf5499d8ef240c63bf648c822f0d69"],["/posts/707384687.html","870f2cbb9e26fdb7f8d748b5feb1a2fe"],["/posts/71180092.html","b3da295f848152f93b486ccc83d7b4c9"],["/posts/716459272.html","81a0191caeaf095ac67afede1e2f8c65"],["/posts/765481613.html","599abcb34475499291cba1e6bc5df71f"],["/posts/778231993.html","7c789b357791e866dda2aca5d9e7a40c"],["/posts/795397410.html","f9d67c3dc76f2461f8bc5e845ac55bf6"],["/posts/820223701.html","f4f62ea0cf69b25b79b88e78a1a1ee40"],["/posts/830372185.html","e71d55aa6954295abaabd2fefefafc63"],["/posts/88294277.html","14ee35c843d0f1f2c36b93e5958e2c62"],["/posts/939963535.html","2f75fff79f271511a3a4c0c44ab1b754"],["/posts/983786067.html","d8456160e8b437d70be7e5f702db2cd1"],["/sw-register.js","377aec5510feae59bb1cb230096174e1"],["/tags/C/index.html","1b216ac03d6f68683a7a3a01c70c69ae"],["/tags/C/page/2/index.html","072d88f971b53277f613f4727938e662"],["/tags/C/page/3/index.html","88b3f9152f9d6d7fc2756814a10ea555"],["/tags/C/page/4/index.html","a54fe4a621035434950bda211f8bcab9"],["/tags/ETL/index.html","943b0f10ec2600a56a2c7f7283d07079"],["/tags/ElasticSearch/index.html","f15dbf4377cb7a4077b57ca765d96696"],["/tags/GUI/index.html","eacbf03252f7cf519f3446e6d4c985f7"],["/tags/HBase/index.html","f097262490c4d670e7d0cd60b5fa8d77"],["/tags/Hadoop/index.html","de37dc6d8352ccf7e57f3b4bc7997408"],["/tags/Hadoop/page/2/index.html","b72177490c8ff48a08e8bba0b007ffd5"],["/tags/Java/index.html","bf333848d03c011f72bd5ef537f43e96"],["/tags/Java/page/2/index.html","5edafc0baa8c61531d26e206d4bcc5d8"],["/tags/Java后端/index.html","172f5e90041fa4ab4fdfb5b82446afeb"],["/tags/Java后端/page/2/index.html","bcaf553ad155f51737fa5bf061bc60f1"],["/tags/Kettle/index.html","a61baa246619b2e636926f6d3c3d4a82"],["/tags/Kibana/index.html","586adf758f1b104e6993884195ca8b6b"],["/tags/Linux/index.html","ac375470cfcae4eb153cd9c11d63c5dd"],["/tags/Linux/page/2/index.html","050df78879ef31136d363e68cc428c6a"],["/tags/Linux/page/3/index.html","70d6a8448c3a49f18918589750e1d201"],["/tags/Mac/index.html","6789e4da1f9c094133e3869699bc3b6f"],["/tags/Mac/page/2/index.html","7f447b157bee913c35e1a4c53b22eab7"],["/tags/Maven/index.html","b29ee53348aa7db9e2581f0a7152070c"],["/tags/MySQL/index.html","110dbf72646bf832ed4ec5cb6a20f9e2"],["/tags/Python/index.html","eab9961010f8addff87e49668d90760b"],["/tags/Redis/index.html","453c70beb351618dc7467064d1845b05"],["/tags/R语言/index.html","85105ee7600eb59a3bdc29a816d4af2c"],["/tags/Spark/index.html","2f0d8336c5f75a652eadf1f2a943ddfc"],["/tags/Ubuntu/index.html","002b6b66bcbb51e0a8a2a7b146782384"],["/tags/Vue/index.html","6218e3c7b7cf75e87d0c3f1a684fb933"],["/tags/Windows/index.html","ab9c2e5af31c8e6aae9fe8729a50af4a"],["/tags/ZooKeeper/index.html","b8924f9714976bd0de5552aaab96daa0"],["/tags/bfs/index.html","4062dfffb0b091531387e7a75b0b3b26"],["/tags/dfs/index.html","306f3328d8c580ddd27e07e2b03448a3"],["/tags/folium/index.html","f0c2640f0c89cd7fd228f8b31d59cdfe"],["/tags/git/index.html","8a93f7d486ca05ffb3cdb2ac7a821c40"],["/tags/iPad找电子书/index.html","54fc51fc7a76f2591e05da2c389b9a6c"],["/tags/index.html","243b7fcc2dd81595f01ee020958a6890"],["/tags/latex/index.html","ac9f1c7acdfc2f75422d9e0a4a0271ba"],["/tags/中间件/index.html","4056f44716f06188ec835989f17eb878"],["/tags/二分查找/index.html","ef4b2b0366be18758f9d87d22a059603"],["/tags/优化类/index.html","689e01536c0f7104c52c2e954323156e"],["/tags/前端/index.html","df350da12156286dd1916b94f3825fa1"],["/tags/前缀和与差分/index.html","d4ff7aa56c090f8620741d767117216e"],["/tags/动态规划/index.html","bab5e45693fdd4890e8d8acbfc563448"],["/tags/动态规划/page/2/index.html","6571c274f79f7f43d68d3effa8e77ea7"],["/tags/博客搭建/index.html","3c38d13f29e9e4c1b71fa52b0b2b7567"],["/tags/图论/index.html","e89f78444241ff325dfaaa11ffb29b20"],["/tags/图论/page/2/index.html","9c40bf6f8ff279bb8ca0735fa593c6c2"],["/tags/大数据/index.html","c23ac365b0aa47057c5f8e8d3cb1e611"],["/tags/大数据/page/2/index.html","a4607b32ea7682044fa5be673c865d33"],["/tags/宽度优先搜索算法/index.html","502ede1a94f692d5dbfd505bc34e7efe"],["/tags/排序/index.html","24a02a636a3baa8eca0333938ade7b49"],["/tags/操作系统/index.html","c8494982a337f69c2c2acc23be6ab778"],["/tags/数学建模/index.html","3a5720d4fd42bf06ac8363986de7deae"],["/tags/数据库/index.html","cd1cc1a1cb6f6a07d46e8e0dc2913f22"],["/tags/数据结构和算法/index.html","5644ae3f33e35c5643db176ad5e89336"],["/tags/数据结构和算法/page/2/index.html","9709873901812957afab5c0c3d29643f"],["/tags/数据结构和算法/page/3/index.html","f5fce201c61cb2f3520557b971749f26"],["/tags/数据结构和算法/page/4/index.html","b3672fccfdd633d17182c2ad12180477"],["/tags/数据结构和算法/page/5/index.html","2bcf34511c27edeb10bcfafe6aa67643"],["/tags/数组和字符串/index.html","1cb55e089e0d1090cbae6f4029847923"],["/tags/数论/index.html","9d9fbf576ccb2d76e22416f25de84ab8"],["/tags/枚举类/index.html","86056964e8e94f8e558abd9a709cbc17"],["/tags/栈和队列/index.html","90cbb16c0b4dfb3c537aa8f1b03542d7"],["/tags/树论/index.html","6eded489f7a3f43bc908aa422275e862"],["/tags/测试/index.html","321060733eab25fa17d2395014b12861"],["/tags/深度优先搜索算法/index.html","7f92d4b151dfcefc96873b33ee725f4f"],["/tags/环境/index.html","d2d0792bf302e67875eeb00d61f359f0"],["/tags/环境变量/index.html","1f4ef3c6b57a6a99ce19f6d21d42f45a"],["/tags/绘图/index.html","89a36a85155a53ae4e6c7d0ffb2f4921"],["/tags/编程工具/index.html","6b198a3637c2e4e36760ae996b332b0d"],["/tags/编程环境/index.html","7e6c5d67812670ef6911a67c3000220a"],["/tags/网络编程/index.html","ce02207a0df6ca696ba9f17c3877fd33"],["/tags/英语语法/index.html","3ba5285fda01d007c1f323018835b067"],["/tags/计算机操作系统/index.html","b3d01c5e2e3fa1bcbc097634ffefcdc2"],["/tags/论文/index.html","2001fc0b43e0c71d736d25a681473a12"],["/tags/资源下载/index.html","256e9e36553c138ae0beb6de309f6eb8"],["/tags/链表/index.html","8eb362e70b62a73dc66059e4612fb7fe"],["/tags/集合/index.html","4a6248718705be891b08925740970a5e"],["/tags/集群/index.html","a2853c2d38a8677a7ac438c170f07cda"]];
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
