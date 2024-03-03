/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","3ef3ca74f529e36d97b96e98ef2ef28e"],["/about/index.html","b498a448ce74d5c23ccbaf6badb43057"],["/archives/2023/01/index.html","51e49c4c5371a9a8b4fad9a27e76e195"],["/archives/2023/02/index.html","99c15b754bd0ef899e1c5d87afab46bd"],["/archives/2023/02/page/2/index.html","7692feb8f5cc0d9eb49ff1667a803419"],["/archives/2023/02/page/3/index.html","ea9d611964a29a9993ca1ce3ffe1c77e"],["/archives/2023/03/index.html","be451e52951f01298673b238478676cc"],["/archives/2023/05/index.html","86287d00efbeea31919b41f2efa7f370"],["/archives/2023/06/index.html","05335a0f391dfbe77cb8d13db75077fd"],["/archives/2023/09/index.html","d97bc837c2aa4f2d2b74481169a16442"],["/archives/2023/11/index.html","4c2554659e65b2fd2a7470c39ab14d33"],["/archives/2023/12/index.html","72b92f7a0b92ef88560be467f38bab3c"],["/archives/2023/index.html","e142f77cecd5d0061c50ffe70bc94be5"],["/archives/2023/page/2/index.html","cf49eea60d07159817a4a53e1bbdcdeb"],["/archives/2023/page/3/index.html","11fe3a78a03be94b19cae9a42f85e56e"],["/archives/2023/page/4/index.html","dbb223232da4c0674a48797bb60cf488"],["/archives/2023/page/5/index.html","1a5f3914ed90cc2bd009c9ee5000feba"],["/archives/2024/02/index.html","1d42640a4bca7039f25d77d6ef1697c8"],["/archives/2024/03/index.html","ce7f6bf8a7bab7e02f7ba130b2090ded"],["/archives/2024/index.html","43f97b5c237279f92d7e0718db268791"],["/archives/index.html","b31e104ab091db0ce5a5aea9be5ca892"],["/archives/page/2/index.html","b4c6c98834ba0a4133648cf070788f54"],["/archives/page/3/index.html","fdd812f8a8eb2cb531001fe894baa6c2"],["/archives/page/4/index.html","e8d4af65aee91447a55a38e2887bf71f"],["/archives/page/5/index.html","49ff6edd7e04a66f61cf053a3e6f4936"],["/baidu_verify_codeva-qQP2iZOMLX.html","4e66b2ed0417487cdd8f9bd3c6f8871d"],["/categories/Java/index.html","052db0a7e91b5ae37b43544b190e482b"],["/categories/Java/后端/index.html","7aac3abc2e5f1d5c5152b8b595571143"],["/categories/Java/基础/index.html","3ae6d152eb6f840bad6c13f134010eac"],["/categories/Java/基础/集合/index.html","cb17f238e5b9562069e676653461e69f"],["/categories/Python/index.html","a130be68d228cf1374b2fa55e2a32b3c"],["/categories/Python/编程环境/index.html","3a3354e6b97d8f03850f5567c10944e4"],["/categories/R语言/index.html","02910e6b573235166a3fe74bc36ddc7d"],["/categories/R语言/编程环境/index.html","b14e36f6f9c34d2c955ff541566fa832"],["/categories/iPad/index.html","228095efadc5dffb121fa56ed9765d0f"],["/categories/index.html","096cc1965f96a5ad07cfb53c9ec006e7"],["/categories/中间件/index.html","b12e33359e3d74b9fab76e6f92878963"],["/categories/前端/Vue/index.html","78bb7328b5e457dfac0a4d9c851f10c1"],["/categories/前端/index.html","5120bfa44f8c826bcb98a7078084f651"],["/categories/大数据开发/ElasticSearch/index.html","7843d2dcaba7af7d91ace68d794290e4"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","ee36189a89f197b1320c96588fcb006f"],["/categories/大数据开发/HBase/index.html","209068fb64c407b42f1ee7878bc3365d"],["/categories/大数据开发/HBase/学习笔记/index.html","f3dc12656c4c010fadefbb530a3bcac7"],["/categories/大数据开发/HBase/环境搭建/index.html","a222cd3acd145598d5beaeaa43b07801"],["/categories/大数据开发/Hadoop/index.html","7e2ca8d810d44ca58c454c2443e3bf6a"],["/categories/大数据开发/Hadoop/技术/index.html","d0ea661b50fef7987b7733749451345e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","decfeb4ab3942651e33df7206649f3f4"],["/categories/大数据开发/Redis/index.html","3bbde974619ee06d183db9b2a124fa34"],["/categories/大数据开发/Redis/技术/index.html","9da67fd6e903935bf62c9ae20dc2c1cc"],["/categories/大数据开发/Redis/环境搭建/index.html","e72283283fda8d3a7433c564a4c4538f"],["/categories/大数据开发/Spark/index.html","c96fe17c76df96047517a743f6bbb519"],["/categories/大数据开发/Spark/环境搭建/index.html","25d9b7cad37a12bca2391f3e02766e5c"],["/categories/大数据开发/Zookeeper/index.html","aef280c380797a01df1c48f372e5a8a8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","569304ded787730be18e58acab3aabaf"],["/categories/大数据开发/index.html","00acdcf7cfd22ba6e542350c7e5d2bee"],["/categories/学校课程/index.html","52c751fb43f7acd36071eb8896fe62db"],["/categories/学校课程/计算机操作系统/index.html","c73ecdcf760ba0d001699448f0e423c8"],["/categories/操作系统/Linux/index.html","da8b9b566f156ffecee409313738f1fa"],["/categories/操作系统/Mac/index.html","ec8ab2bee059c107cd116cb304aadd01"],["/categories/操作系统/Windows/index.html","0f1c3af112df237cf8a38be22301670e"],["/categories/操作系统/index.html","19666b68504653fa000301ef67db3772"],["/categories/数学建模/index.html","e1a0a42b0e88e3c9a592910eac674f9b"],["/categories/数学建模/latex/index.html","3930cb051153da191c8ca8e1c0f922b8"],["/categories/数学建模/优化类/index.html","01bc20cc41883ca560816e84d4467445"],["/categories/数学建模/优化类/现代优化算法/index.html","b6af43c37f51379debbca381d51863ae"],["/categories/数学建模/优化类/规划类/index.html","06ef4c832617cf7000789a39e7ca06b4"],["/categories/数学建模/绘图/index.html","b22a03472baef4395aa9758bc771f4e7"],["/categories/数据库/MySQL/index.html","cde07d9dc94dd980845571c6b658ce9b"],["/categories/数据库/index.html","51a82f30ae89514dd1bb88446cf9a716"],["/categories/数据结构和算法/index.html","6e364ae193246729f722469fc0d5aabb"],["/categories/数据结构和算法/page/2/index.html","41b185b5237006b44c562cffe9e3cfcb"],["/categories/数据结构和算法/基本原理/bfs/index.html","f4fc6c7899b42513953b20129c999075"],["/categories/数据结构和算法/基本原理/dfs/index.html","2c2682219a76886fc09eeff8ba1ba55b"],["/categories/数据结构和算法/基本原理/index.html","6d4ca3524c95da6ea7738ac4a145d8d9"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","ec0561e07e39e7272d75aeb532a047c7"],["/categories/数据结构和算法/基本原理/动态规划/index.html","052c18d188925d3970807c1f4ee5e554"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","f1703483296aa313c91c4d282ddb220e"],["/categories/数据结构和算法/基本原理/图论/index.html","123113937dc3cd234e47c9c936ce5ae8"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","7a31de9647665b9a92e95f7bd6f7f02f"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","2c94266a69769c720ac7c70002ee639c"],["/categories/数据结构和算法/基本原理/字符串/index.html","92bc882beb4ec2e136bd6d3c2a9a9d5d"],["/categories/数据结构和算法/基本原理/排序/index.html","59175f4c5c2ef991bafb6b6c8eb485ef"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","77f4aa9e59b5b9ea7b736fc8908aac47"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","52b478cdb3d6a960262a79bd03aec1ed"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","50fd131cd89914cc6d7d5ff0e67429f6"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fd27684f887182a5988ad335f86f2b92"],["/categories/数据结构和算法/基本原理/链表/index.html","e089ed7fc0eca836a947bbb7cb5372ce"],["/categories/数据结构和算法/算法题/index.html","894102c7ac2a053e7fcd22d4083f0715"],["/categories/数据结构和算法/算法题/二分查找/index.html","b6ac72bb50a3b1342a49c5b2efb0257c"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","bcee814c9bb2d753707395529140fb85"],["/categories/数据结构和算法/算法题/动态规划/index.html","174f912d9597517bb3a2e173b84828e7"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","3d09b9131fc7dab52b0745d4e100f91c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","26abb724cfabe148a0624a2fd04a0aa8"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","6615a59a30dddffd8ef49433893af5d1"],["/categories/数据结构和算法/算法题/图论/index.html","6029094dff3e03a157036af3d13d2af6"],["/categories/数据结构和算法/算法题/图论/树论/index.html","a9b1a711c4a71eb8ffd26b78bccbfbdb"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","60425c6f84ccd5b437d73b3850acf4a3"],["/categories/数据结构和算法/算法题/数论/index.html","626379539f216b65108e1ae3c2d70809"],["/categories/数据结构和算法/算法题/栈和队列/index.html","ff8844a50c10474af6c1ff48ddad290b"],["/categories/杂七杂八/index.html","bd4349aed05bb871262ea190ce426369"],["/categories/杂七杂八/博客搭建/index.html","e532e8c6a8eacf4c3faf12a7430d4d99"],["/categories/编程工具下载/index.html","308d29646c2d14053c4e54e9f9f3b067"],["/categories/编程环境/index.html","b83bf83ca2e3cc5964a8510b677cd8d0"],["/categories/编程环境/大数据/index.html","bec6c7c1f66f026fd7e97f50ceb42292"],["/categories/英语学习/index.html","2c4fc9910316dcc9f5a543fd80a677b6"],["/categories/英语学习/英语语法/index.html","554e3d7a35ad9b04cedcc79ff9fb6297"],["/comments/index.html","92042d549a199dc037ca748fc96674a8"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9affc079585aaf345c56dda0c0e77a59"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","f1527274c0f2516f407ea1a75a0ab927"],["/movies/index.html","939b76432608aba1d1f86afaa6fe9786"],["/music/index.html","a620b8ea07ff24d8d2300af06b6031f4"],["/page/2/index.html","7ee3f5e2f150b21619ea1400fab9bb87"],["/page/3/index.html","eb636fdc32412c985dd6224d31214edd"],["/page/4/index.html","533183727ed15ff5b2621d6c072b794e"],["/page/5/index.html","d10f2545f136e567b7948f220be0aecf"],["/page/6/index.html","63c41b7217f01809889df3cedf4e9c14"],["/page/7/index.html","5c4ac9910200463d1d85db3f0bc71053"],["/posts/1021360842.html","42bf4a411222ab9d65a4a7a1d976b2b4"],["/posts/1120620192.html","aeab33207adfb962de163476c6702219"],["/posts/1137707673.html","11c04328f7faa6dc9a8604e2125f149d"],["/posts/1141628095.html","4ac3cabe463417773ca7ebe489162d13"],["/posts/1168613674.html","8e4e8b06377c36a23d94a8905dfe6cee"],["/posts/1219920510.html","6761148b106c33a73fa658443df2d8f5"],["/posts/1222166338.html","e37778acb3a42b3481b52e4a38e49978"],["/posts/1259097482.html","daaa0032ff5ecabffcd8bc14d7947b80"],["/posts/1271036369.html","db56f0c52b5f2b656b57dfe4dada4cb8"],["/posts/1312847445.html","ce9584e4e0f898f8847a1acc4105b007"],["/posts/135355774.html","32092cdf8f4da3241e76af01152b4ea8"],["/posts/1375344716.html","4a93f10c09ececdd01a26b126e8d5014"],["/posts/1388991698.html","0bd38d43054b30f5f6dd628cb0a35813"],["/posts/1410315814.html","c06bee416fc6d0298fe5f3cd72b75a4c"],["/posts/1452790229.html","62053b9de013b446793634f47cea3dde"],["/posts/1470079884.html","87d6154b17594f7dd20a5e17bbc0ddb0"],["/posts/1470079885.html","d192ba56cd0a7f636e852a5def587b1b"],["/posts/1470079886.html","d03cc0ce28927f65042342d1cb8bd1c4"],["/posts/1470079887.html","3ff893b07fd4c5833c14e464d0624321"],["/posts/1498536549.html","c2a6d5845fb9882891e46a6dc89b7283"],["/posts/1539568593.html","a5256b8a25828dc2e02b5074634f4d94"],["/posts/1547067935.html","0fdd07e53038b8aece68c38987070ced"],["/posts/1557866301.html","d58ddd50fcc3b2f231e7265ebb8f1d64"],["/posts/1571776361.html","13d722cf6a63204dd1da6245257d882c"],["/posts/1605124548.html","034cb910b51c3289de28ee20370a178d"],["/posts/1633036852.html","def0b9fe65d79b3dbc5408ae2f23c210"],["/posts/1667740714.html","fd48761026cbd5d0ccecc46e3ad510c3"],["/posts/1674202625.html","b9edb151fc719ab6be1b913120a7b0bf"],["/posts/1765123828.html","3cf7ae2b1cbf6b42a456738ad5b19b9f"],["/posts/1767336200.html","cd924fbb7745f94f71be3a580775f983"],["/posts/1776114197.html","b080864b1122c6c0f41fe0ea3e60a95d"],["/posts/1817748743.html","4c8dbe62056a0d6f2e704394c67bb284"],["/posts/1925125395.html","dbc903c7ce1691669d4ba66a547953e7"],["/posts/1966191251.html","0964a15e02e1b836dcb808878ba14675"],["/posts/1987617322.html","61ab772ea6b384a103e67b49f3b91934"],["/posts/1999788039.html","5f1aa777f05fca0f6541fd297fcce31e"],["/posts/2007534187.html","7e2790000357c2a6a4befdd444f0d011"],["/posts/2075104059.html","b12d677dd6cf00390b2faffa46c797e5"],["/posts/2087796737.html","1ff7f309d775ba1865b337d5eef9a4a9"],["/posts/2106547339.html","bc0dd23feee135d91269cda20d03f036"],["/posts/2207806286.html","071e1efee79feb289a90aad567cbaacc"],["/posts/2225903441.html","fa82c688e779f3101b334061bf4a78a2"],["/posts/2265610284.html","1b7bc4a7cbfa453dd58d81aa519d6942"],["/posts/2281352001.html","6c3569f7aba6ae6c8f89c6b5693c6806"],["/posts/2364755265.html","b8b0c3df1099ed20697fc90f1d612cb4"],["/posts/2414116852.html","f8385f96051fd88f7a625c837f6810c7"],["/posts/2421785022.html","3978989b1673d3c12024e5a93a10fcb5"],["/posts/2482902029.html","f9411e6c8fbc7d8d170ce1814ca6f794"],["/posts/2495386210.html","93749962deebaea2dba2fab37ce95b2d"],["/posts/2516528882.html","af597ba974b6af7d29f2606806561934"],["/posts/2522177458.html","24e087e1b4187c7b688348ea7e981c9e"],["/posts/2526659543.html","d47dc2cafa5d8240c148f2ec72ed1e87"],["/posts/2529807823.html","d1515d7697cf0e7f72571deb36c9fa5a"],["/posts/2592249117.html","859105d2d7d4a6522c37e275a101c61e"],["/posts/2596601004.html","3543abf969718a92a4deeae863216667"],["/posts/2697614349.html","72fafa027fc599b0c89bf08d422f9abd"],["/posts/2742438348.html","87f0691df9c1d9c7768008b9edbe7f7a"],["/posts/2768249503.html","d91acf0d720996f95a5ef91e199385a8"],["/posts/2864584994.html","730e9fbf2768fb0d1d3a361e1e404b5c"],["/posts/2888309600.html","ffad7e013bc0073b53c3726881bf0472"],["/posts/2891591958.html","3839aac6981ef10ab4a7dd2e033050d7"],["/posts/2909934084.html","144a53bb459e03f0f5fbe36cb112dc7b"],["/posts/2920256992.html","b619ea27b714fc8fe69d40599b8c272e"],["/posts/2959474469.html","94992b5ee1d62d4acfcf0c6e50d7f1d8"],["/posts/3005926051.html","c1776e98f4296bc4417fbdcc68b56079"],["/posts/309775400.html","6e6bdf1945ecc9440253f15b17be706a"],["/posts/3156194925.html","ead83139bd140a80df9eae349e61e58c"],["/posts/3169224211.html","1277d75d600a269a157f8d55d725fe29"],["/posts/3183912587.html","4db19920af0fee8dba417abf66e53854"],["/posts/3213899550.html","dabd3a0494e612ddeaf73b49ec6a5ca6"],["/posts/3259212833.html","441289e668de2e11093839b8bac5b9b3"],["/posts/3265658309.html","bab52761a3696778d5f3bdf61b17a1e0"],["/posts/3266130344.html","e8ac0a54043a283fdad0c05b28ecce3b"],["/posts/3292663995.html","77dfba192f825efe7d806525fccc7a18"],["/posts/3297135020.html","f72c9a58a84ba799859e6e33226120a6"],["/posts/3306641566.html","9f706c0eb607430912be6d8c1ab5eabf"],["/posts/3312011324.html","22d925489c42b202facf980fba4881e1"],["/posts/336911618.html","a3f3a7f45c50047eb969fdb69c6be2e5"],["/posts/3402121571.html","31da19b049671fac3a6e164744471b1b"],["/posts/3405577485.html","b1d5ec6aa85abe7fcf469f40f0063f3e"],["/posts/3498516849.html","6d4c286b63cb9860336c13442ad13520"],["/posts/350679531.html","08b4a6d4194940ecce318b70934a01a9"],["/posts/3513711414.html","1c9b4feaf634110a027341a91d314641"],["/posts/3523095624.html","b77c06c07748eaec42490251606739a8"],["/posts/3546711884.html","1bf2ffdaa2c30569fbd2ec4a04137a2d"],["/posts/362397694.html","59f10db8cf95761e10be1d25a79d9aa4"],["/posts/3731385230.html","c8b54234d59256ccaae017c5ca44ace6"],["/posts/3772089482.html","ffa6adaef4287f739d9c46084d19722c"],["/posts/386609427.html","f1510cf41f1725420264db54f9f754be"],["/posts/4044235327.html","49bfb953d133f8522d679a6e35c3ae9b"],["/posts/4098221856.html","17ec1fad1bf6a77330a10f3e206c6d4d"],["/posts/4115971639.html","3be3bdc34a14e10001e59515951f4b8d"],["/posts/4130790367.html","42ceccabc7ea72b5aed8885ee8b3334e"],["/posts/4131986683.html","4daf6be720b8cdb8ee6c9ae23f01fd54"],["/posts/4177218757.html","36a249e4b08224a210146e49d3df78cf"],["/posts/4192183953.html","7a0e8ea92f9594d050b7e3e7c82cbcec"],["/posts/4223662913.html","2055d7bb4e92718b37ef36105beda6a0"],["/posts/4261103898.html","3a570df9a15289c1d2811bc1ca6473dc"],["/posts/4286605504.html","98b4f0f34f96adbb0a45515c130719c8"],["/posts/449089913.html","a8a7635854ab5a494ffa38d2f101b2e5"],["/posts/469277133.html","1a6d1bc3b3b3eb595528d757839b3cfa"],["/posts/469711973.html","dc60823b745ba13ebc85ef1bc523b983"],["/posts/482495853.html","f95934a0bf54a735dbaf516a41281e42"],["/posts/488247922.html","a6d4f456757660e39df6fcc63e7ebd67"],["/posts/517302816.html","ad02c92b3ba75ac95b85e5aff6bc9f06"],["/posts/570165348.html","ade630138e2266270cfa9d250d91b6b8"],["/posts/595890772.html","ea0587b162db507f4f607f5a4d1b62e2"],["/posts/67485572.html","f31ef0df110250f37ad67010400f0287"],["/posts/694347442.html","196b6e00faba44763c15e22c8596ca13"],["/posts/707384687.html","4ddf863780ca42ec9fb11b6f4a4f851a"],["/posts/71180092.html","8026a4a81d74954b58533f0ae96c5d84"],["/posts/716459272.html","9f9ac0e2aae91107616134450f38007d"],["/posts/765481613.html","99388d09213ea7907b060c328356812f"],["/posts/778231993.html","7975a53fa0ff565b76d171018fdd9fb1"],["/posts/795397410.html","50377c50c3b823908cc52e7a1a8b22f1"],["/posts/820223701.html","c2cba9739321d501ce5cc3ed37c99d75"],["/posts/830372185.html","09ed0516d0c4cdea6228d19a8802ba98"],["/posts/88294277.html","bd798663fb740673b066077156a94f7f"],["/posts/939963535.html","643c870d22e4a054a288d5eef6dcfed0"],["/posts/983786067.html","97a9e327178c8ff967d0d1e187a8de71"],["/sw-register.js","3d0b52fab5a4dcbe78b220ea33e8a630"],["/tags/C/index.html","05c4aa733aad4ceffe4a26c1db77a8e7"],["/tags/C/page/2/index.html","d70a9602b954f72530d9050211b36a59"],["/tags/C/page/3/index.html","f445647b6dbe0f494e6c41d20dc1cc26"],["/tags/C/page/4/index.html","2ce4335d0a4b8236c5645fb3054813ae"],["/tags/ETL/index.html","31dc0829215a47ab58e2c35fcd978362"],["/tags/ElasticSearch/index.html","bc84bcf32ccf79ac3456c4c78d215675"],["/tags/GUI/index.html","a6e0c2746af700e522fc1752ea484ff0"],["/tags/HBase/index.html","3ad299f43ff0757604460d0828f71260"],["/tags/Hadoop/index.html","882c5c25a669ae97ab801100247733ce"],["/tags/Hadoop/page/2/index.html","40b5e1750c2b3337bb4be95c663f0810"],["/tags/Java/index.html","4db18e92ead28aac752ec60dcce669aa"],["/tags/Java/page/2/index.html","dac42bc16f66147c4a5ba81f58ed9dd9"],["/tags/Java后端/index.html","fdb1bea9786d9c146a6a5dfd83a63ee2"],["/tags/Java后端/page/2/index.html","06e7138567f4e90eae041545fac2f4af"],["/tags/Kettle/index.html","48ad8eb2ea82d64f2dd675ae25bf958d"],["/tags/Kibana/index.html","059e8c7061f4f0eae225d4d005f1cb93"],["/tags/Linux/index.html","36dc3e00f3f476e0a9efcb374c162084"],["/tags/Linux/page/2/index.html","7de478d9cb57f33b76414a50b7980f2b"],["/tags/Linux/page/3/index.html","6916f811cf201d2e160bc2b913126881"],["/tags/Mac/index.html","bdfe7ed5778dacbbe0b40c9f4ce32ae6"],["/tags/Mac/page/2/index.html","fc725c2ae7373ba9d31e703a1e0bb00a"],["/tags/Maven/index.html","55b010663e52e4fc908c9c8fc2571907"],["/tags/MySQL/index.html","2549736047004f86b42a0f686f7e1ce2"],["/tags/Python/index.html","134e1840f150dec706d430747e436b55"],["/tags/Redis/index.html","0b11b60660f4d0da956e692ad075b8c8"],["/tags/R语言/index.html","0464fa475bf487bf334fa4e001702d25"],["/tags/Spark/index.html","566c91ee0702e3d590396370a75f345e"],["/tags/Ubuntu/index.html","d7f3851fba611176efc95d598b7a0a9d"],["/tags/Vue/index.html","3c4f2c32679476511226e478015a236d"],["/tags/Windows/index.html","c7a558921bb82bb9e8856086c12380c2"],["/tags/ZooKeeper/index.html","54cc5760c2c7b3bd77a9d285b223c5fd"],["/tags/bfs/index.html","3993cc1858a994f51e0c104501d61b63"],["/tags/dfs/index.html","92ed94c62a44e19d7726d4dadfe00058"],["/tags/folium/index.html","97ed14f43026953fe4e3465f3f528b01"],["/tags/git/index.html","e967191dd8bee6c1a99e5a8ee056bfb3"],["/tags/iPad找电子书/index.html","2bb9cbb6f570cb1317203903becfce66"],["/tags/index.html","da1d27df32824a455cbe4027a32cbcbc"],["/tags/latex/index.html","cda782150e895ef7220a7902dbeabbf8"],["/tags/中间件/index.html","b9502c6a08f9d5a771fff9c2facac1c0"],["/tags/二分查找/index.html","88505b46551aa86326ff7afebc259b97"],["/tags/优化类/index.html","9b831c1fa28a31dee79169f86c6695a5"],["/tags/前端/index.html","d82184d8e39444b5e08c67d34aa12581"],["/tags/前缀和与差分/index.html","296c88a42b5c209c5a09670b447dd6d6"],["/tags/动态规划/index.html","25c77e81e24ee5e8d51b5b691ca03171"],["/tags/动态规划/page/2/index.html","921a3be63f87103e18a60f89a3d01b95"],["/tags/博客搭建/index.html","068386e62a827e545f9462d93fbe7e72"],["/tags/图论/index.html","badef83c5962e4900c37330fe95528f8"],["/tags/图论/page/2/index.html","5e0f36b4d91ead52b755850fc63c1027"],["/tags/大数据/index.html","7f2d1f2bafe0c1aa8cdc477e5e0d039c"],["/tags/大数据/page/2/index.html","0d6043a39cc1f11e04d74e21fd4c91a2"],["/tags/宽度优先搜索算法/index.html","857ff4feb1b0c64cf67c1cd02116c10f"],["/tags/排序/index.html","acbdcbf08312457f2dd7032949809cf1"],["/tags/操作系统/index.html","cdb6aff8bab74b79535bd71a624956cd"],["/tags/数学建模/index.html","d8726a4040ef068711ac3754e5bafeb1"],["/tags/数据库/index.html","188fc98fe0c192beb1595fecab101a6a"],["/tags/数据结构和算法/index.html","161c8537747d45ab9e6bc81c9a9dc868"],["/tags/数据结构和算法/page/2/index.html","e66aa9ed256abbd8364d84a18c147098"],["/tags/数据结构和算法/page/3/index.html","2706bdca4328195a57c8a13bbdf079a5"],["/tags/数据结构和算法/page/4/index.html","c67afd0d12f9ded312b333144037ff50"],["/tags/数据结构和算法/page/5/index.html","9fac1929d5e79f5c1fc582c4a6e904e2"],["/tags/数组和字符串/index.html","1d8fe1f788ad1402205936c1d8e34882"],["/tags/数论/index.html","65391a1a63fcaa6f7bf214a604a5ba4e"],["/tags/枚举类/index.html","d0a75ccab8e4b2c8618963201e197a3e"],["/tags/栈和队列/index.html","60494a23b757a5c623087f9d0cea99d4"],["/tags/树论/index.html","fa7120b6cc36cbb718834f0a0e0db867"],["/tags/测试/index.html","7d4b55162e5038638c452a19fe7026cd"],["/tags/深度优先搜索算法/index.html","ab2958173373751714de1dfdc56bd967"],["/tags/环境/index.html","59a6a02fc941fbbbb5cb95c669e1d11a"],["/tags/环境变量/index.html","5805b7cad5ffc828f42e03b7b312087a"],["/tags/绘图/index.html","a2be18558b58e0b2f864d1edf9f07eea"],["/tags/编程工具/index.html","9212e41ce86ce3acf2daea9d9fdb973b"],["/tags/编程环境/index.html","546f8f66a378d9c60715ab75a3218399"],["/tags/网络编程/index.html","61a4d931f39137da51b8f9d55e453f9e"],["/tags/英语语法/index.html","31ea2eef923a8a9e1398159a450a96ba"],["/tags/计算机操作系统/index.html","723295cd67f59be5f2d3439bd736e03f"],["/tags/论文/index.html","c8fc5fe7ecce5845e300addaf2751215"],["/tags/资源下载/index.html","3ad501e1b2d723d81962c64610083656"],["/tags/链表/index.html","d4dd6b992d9dfb9e221c0a4f53e2ee47"],["/tags/集合/index.html","a50a6a14d244db8be5ca15da9ad6a73d"],["/tags/集群/index.html","2a04c99148e60b2d98096f6c5929b2da"]];
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
