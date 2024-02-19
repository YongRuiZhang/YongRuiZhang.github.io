/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","d1707239b1fe458595f0bd88a4bb933d"],["/about/index.html","26ed1e10055323e1ba20e33c1d278e4a"],["/archives/2023/01/index.html","363060e2c0866d4db71c6c86d791e129"],["/archives/2023/02/index.html","7f2ffb670446fc76a4f7e516c4c168f8"],["/archives/2023/02/page/2/index.html","35e4f7800ed87cfd680acef26be68f46"],["/archives/2023/03/index.html","6f9d44e95b9104e796522a014093816e"],["/archives/2023/05/index.html","ebb8d0556f03a7f80b9e964574eff4d9"],["/archives/2023/06/index.html","9644b83f195c0e41f4f1d0511ae6bcfe"],["/archives/2023/09/index.html","1ea7baeccfaff0a5c0184e4888c7c5e4"],["/archives/2023/11/index.html","6161ec52c4b968ee89b1688344585f90"],["/archives/2023/12/index.html","152a47ff8dfab2cb17a95396ae92c66c"],["/archives/2023/index.html","d795364fbda05c40758d22a4b1d3cbf7"],["/archives/2023/page/2/index.html","d8bbf0d25890a2058337a828401d7439"],["/archives/2023/page/3/index.html","6ebdcd75512e8036e969a4a33e613af2"],["/archives/2023/page/4/index.html","de8866295422db31c18c99abe86f9666"],["/archives/2024/02/index.html","343d9ae97af767bbbd19889c4108f081"],["/archives/2024/index.html","ec9a268efd0d2fc966b7b87d2f87dbcd"],["/archives/index.html","5b7090e5a8a4f13ef8c84eb132202957"],["/archives/page/2/index.html","b70cd73766225e1cebbc6cc1be52797d"],["/archives/page/3/index.html","e988b8de1b7777530eea6128b2fb9496"],["/archives/page/4/index.html","6fa27baa1802108a2b36fd2d2ba46a80"],["/baidu_verify_codeva-qQP2iZOMLX.html","31731c00fab4db7ebb58913fb59d16a7"],["/categories/Java/index.html","d5d342b5655467d6734bd8b3d8e9612d"],["/categories/Java/后端/index.html","f23fb8f57537d502aba836b86fa9b71c"],["/categories/Java/基础/index.html","eba1670e98e7212e18f251569c32bc37"],["/categories/Java/基础/集合/index.html","200336d900dcd76d490be3b6de88da01"],["/categories/Python/index.html","a5184491197a5740bd902a149dd47ea4"],["/categories/Python/编程环境/index.html","4ba4e64cc0f02d6dd27a4240b16295fc"],["/categories/R语言/index.html","335a30141d0a6ef96a8330bfd3c8dc11"],["/categories/R语言/编程环境/index.html","a64d2d4642de8eee6d9e28947d200808"],["/categories/index.html","4f5e3e536a28b7a8e3212c00c2825d2f"],["/categories/中间件/index.html","be24f15b36d285cbc45ed58c0ee8ca5d"],["/categories/前端/Vue/index.html","01214392143998deed1936cf6acc47f7"],["/categories/前端/index.html","3b85685ba2ececd1c47a4c90a7c4f3ba"],["/categories/大数据开发/ElasticSearch/index.html","4f0bd752f984938eceaffb89e75174b7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","6174b3ad29e321529183ce0f23f3e8e7"],["/categories/大数据开发/HBase/index.html","9718b922d8cfc1592bc213e47b38015c"],["/categories/大数据开发/HBase/学习笔记/index.html","513848cfaee3d3016996ee57337fe092"],["/categories/大数据开发/HBase/环境搭建/index.html","aafffba75e3c1307335c7da50dca9f65"],["/categories/大数据开发/Hadoop/index.html","775ddfc311f28e20a01e09eb578d591f"],["/categories/大数据开发/Hadoop/技术/index.html","7e913f30d000566ba1aec99ef43e4eef"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3da13f9d9819fae2b460153779a0abe3"],["/categories/大数据开发/Redis/index.html","a2a0d5cac3fd380dd8f8820525acc4ee"],["/categories/大数据开发/Redis/技术/index.html","bb0732e30d6db90dd8d546a8671fdd9a"],["/categories/大数据开发/Redis/环境搭建/index.html","195c496c132ae3bf0173f0ccfca218f2"],["/categories/大数据开发/Spark/index.html","ab7a2dfaf5f4ad14a7d4788f3cd2eee4"],["/categories/大数据开发/Spark/环境搭建/index.html","51ee643c51f856be9bb5c8567d58e0bb"],["/categories/大数据开发/Zookeeper/index.html","62b532439a96dfc1ac40ff9ddf5c9b35"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","f3c933cfc3b90b1abe168e76b8767375"],["/categories/大数据开发/index.html","7a5a06071e2bb7a6e89060cd51ba3653"],["/categories/学校课程/index.html","08d583aa7ad0f07dcde415d2bd857d12"],["/categories/学校课程/计算机操作系统/index.html","b1b3230875eb66190203cbdaa72e8a85"],["/categories/操作系统/Linux/index.html","dc18d9ac2cd39476256d3884f3e9b592"],["/categories/操作系统/Mac/index.html","798814e0907732757323465c45df9c5b"],["/categories/操作系统/Windows/index.html","560696acd8d70a7b6569d5edbc8a2380"],["/categories/操作系统/index.html","53376f655e2e2aa233374c5f514e38d1"],["/categories/数学建模/index.html","5e1fab3fc14537359e19c77dae45320e"],["/categories/数学建模/latex/index.html","8ae1b6fc562ad3662481981c66a554c5"],["/categories/数学建模/优化类/index.html","76a467d5f79a6cb0802d79ebfe29aeb4"],["/categories/数学建模/优化类/现代优化算法/index.html","093a9b1e1abd25df0c10618f81273bb1"],["/categories/数学建模/优化类/规划类/index.html","9b5b2efae32a4803bad96c5e128b6e0c"],["/categories/数学建模/绘图/index.html","a91aeff3153d5b304434d2e394eaef91"],["/categories/数据库/MySQL/index.html","e48618e920124738d5d9a0f48216f4fd"],["/categories/数据库/index.html","f7de483915096f4ddabf88db34fa7125"],["/categories/数据结构和算法/index.html","1421ff2461f84704c962928dd69df124"],["/categories/数据结构和算法/page/2/index.html","985897e3284112f6193a6bf630d2c765"],["/categories/数据结构和算法/基本原理/bfs/index.html","3d65657f0bb9cc807dc5b83a0947a9c6"],["/categories/数据结构和算法/基本原理/dfs/index.html","6d3f3aee78f1caba08b55a96ee3eca6e"],["/categories/数据结构和算法/基本原理/index.html","038b2f7718117827a338830a348a8bac"],["/categories/数据结构和算法/基本原理/动态规划/index.html","cbd7a6a4fafeb92bdb34d264abead010"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","6e3eaee9a40750199f7ecb120815b679"],["/categories/数据结构和算法/基本原理/图论/index.html","5c4d1ab2b4c54a19ad0bd740b359fc64"],["/categories/数据结构和算法/基本原理/字符串/index.html","adc8c1f86eb3eb05fc43ee6e6ee87218"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","3865e940c658f47615722c6c1a172c68"],["/categories/数据结构和算法/基本原理/数论/index.html","5a65985b05f1851fd1e0b5347ca12892"],["/categories/数据结构和算法/基本原理/树论/index.html","bcdcc142816d07ef290a0b681fc43d9b"],["/categories/数据结构和算法/基本原理/链表/index.html","c5baec9077a00a2a2c3c5805a66ea4f5"],["/categories/数据结构和算法/算法题/index.html","2af718d0890bcad31a3042b2b64e6018"],["/categories/数据结构和算法/算法题/二分查找/index.html","c359bf95ed40d53849ae9d425752f17e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","410ffe140b7ef7217d3a1e387daae822"],["/categories/数据结构和算法/算法题/动态规划/index.html","1842eeecb1bcb9fd6bcd5b0ef040f709"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d2a239eb50ccd08ff54de3b7921c523c"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","4b30a6bd7657242ba356492fae1ecc0c"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","ae270cba6d8cce7f941f2d6c990a6ac8"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","91b54f230b5a3d968aa2cd75be1deb14"],["/categories/数据结构和算法/算法题/数论/index.html","9bd48f32686923ccbb15c5f418b33a12"],["/categories/数据结构和算法/算法题/栈和队列/index.html","656f43fcf022705de089dd50e5901420"],["/categories/数据结构和算法/算法题/树论/index.html","86709e47f5e6289cd28436bad8026e62"],["/categories/杂七杂八/index.html","a3e1f2a26b3965c12b500c8b03a359ec"],["/categories/杂七杂八/博客搭建/index.html","b84f85060a13c516ba624032de5b14bc"],["/categories/编程工具下载/index.html","1159e2c42b13034aff2a9cbcd23a16fc"],["/categories/编程环境/index.html","d11bbf0516d3417161ef57f7af321ec6"],["/categories/编程环境/大数据/index.html","0eb860cc2193ab993a01b805adc5c06a"],["/categories/英语学习/index.html","590f9e62603ae7a2b55585013303a3ce"],["/categories/英语学习/英语语法/index.html","aa0166f7bd4c1dcec1c02ceba38460d9"],["/comments/index.html","a1288e05c1e77a56b36cf06adadd760e"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","fa969dc4a8305aebdcf96f8fa72010d6"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","b4ff3e11e82be5ff10a6b08aa71d04b2"],["/movies/index.html","276501a245853bbe1ff837c9b8ce9cab"],["/music/index.html","520be3565e352c556e2698621437d256"],["/page/2/index.html","08625e5eb934448cfbee30e650115682"],["/page/3/index.html","a11b0523b9044b2648734e639ee936fa"],["/page/4/index.html","fe1fcc345110249bae2bb8959fc04840"],["/page/5/index.html","6c4fb2e8bc41e4807ae185caa3c5c9aa"],["/page/6/index.html","313b768321c9087874e525abb871d132"],["/posts/1021360842.html","dbed054348feb37061454023723831cf"],["/posts/1120620192.html","55dc96876ff66af4d630235237c05c01"],["/posts/1141628095.html","6ce83a78399f644751f3c94649189989"],["/posts/1168613674.html","772ef4edac4774051ec889cfc0fb8822"],["/posts/1219920510.html","8eae424667058d9348844b4732d0ea44"],["/posts/1222166338.html","9884cdd51d0be8357ca20fcc6ab966f0"],["/posts/1259097482.html","88b7fd0b5b813562e95d0a086e00a297"],["/posts/1271036369.html","45774712a84900f8cbc5f808dcd971e9"],["/posts/1312847445.html","ed10e4e3d87a611e684dd135ff4a28fc"],["/posts/135355774.html","bd463a2d71d2889651e941180f8345da"],["/posts/1375344716.html","538e5e6872468823ee115adbd48df5ae"],["/posts/1388991698.html","f4b9b79c39b42add436e80dfd55068e0"],["/posts/1410315814.html","77b31a12b25434b0a3411c998913760d"],["/posts/1452790229.html","0e50edcdae4ccad53a9e36adf4cc9f03"],["/posts/1470079884.html","b28ad0db2f2a4e67c9726762ec942429"],["/posts/1470079885.html","9f8eb15b120e7a4cc8dcdf9e2e8edaa3"],["/posts/1470079886.html","04587e7b87a9d55c967929f1c9a22479"],["/posts/1470079887.html","bb6065163aa852a648d3b1105ce8021d"],["/posts/1498536549.html","3fb4db05d6617bd37234d8560a5d949f"],["/posts/1547067935.html","12e890252896c5ed5dc2d418873593cf"],["/posts/1557866301.html","ed738f30b15f77b445eca2969eed9393"],["/posts/1571776361.html","defdb586351d62cbcc1c6597f1d6122d"],["/posts/1605124548.html","f2a842fd870987ab7c5ac2c96d93b2d2"],["/posts/1633036852.html","e3c2d2aea9b5ff961aec20740b1d1126"],["/posts/1674202625.html","62f1ccab319a98461adf6221d7c42faa"],["/posts/1765123828.html","dbd759af77923a772e26e2eebd2e2481"],["/posts/1767336200.html","862ba38140c0578a7c22f161491e4e7f"],["/posts/1776114197.html","e41431daafab1007744863c67f7c7f75"],["/posts/1817748743.html","db516be248270e2007949381c71adf53"],["/posts/1925125395.html","65032efef12e33a6321e9c043d9d22ee"],["/posts/1966191251.html","f98650a53816a09dc889997aa0170eb4"],["/posts/1987617322.html","cca2db337104ab8cb1bcafb1334a8455"],["/posts/1999788039.html","80e79dded965339f1c391ee6e44149e7"],["/posts/2075104059.html","9acef4e268bd8b7ab699ea936f6c03b7"],["/posts/2087796737.html","64adf5c8a9afacff949056b3dcfbe2fa"],["/posts/2106547339.html","d6c671fe7397064efbbf8dd1c5c5c10f"],["/posts/2207806286.html","c3997e65cf73849e41f7ea5c315bebf0"],["/posts/2225903441.html","1863817378c1284200c04a3630754fb5"],["/posts/2265610284.html","85c546aa5062a2f0225aab4024775a6c"],["/posts/2281352001.html","2ec577c68a4f81c7a3f1408ebf3a6a41"],["/posts/2364755265.html","4305fa8f61120c39c119f9192795bb95"],["/posts/2414116852.html","e886f5cad5bcecf0c1f6ffe91cc0b0b1"],["/posts/2421785022.html","63ae07c913375cfe66b40e2e94a0dc60"],["/posts/2482902029.html","78ff6c58d197d1368247133d85d96a78"],["/posts/2495386210.html","33f83d978aa879ffc29ad7768012e2f9"],["/posts/2516528882.html","1f2d17bda3855584d2cd6eea0ef2eda1"],["/posts/2526659543.html","d3afdbd3620cc5f3f8b534b7653acf17"],["/posts/2529807823.html","a65b4ec8f429f862b6a78bae603e1351"],["/posts/2596601004.html","dadc347ee911a649e817dcdf75163c69"],["/posts/2742438348.html","849635fda672b58f8db1b0afbd2cd7e9"],["/posts/2864584994.html","e6976e11cac337bd7131ff5d40ec9149"],["/posts/2888309600.html","305e77d0435660d977f700d62080946f"],["/posts/2891591958.html","9481a764b6734285f82dee4abee95d88"],["/posts/2909934084.html","d90117a951a97b5dd61a5fcf1ec269ea"],["/posts/2920256992.html","778950760e312f54f85f213d75386faa"],["/posts/2959474469.html","724ba409d88820442fe6bdca367aa176"],["/posts/3005926051.html","f5f3f4e052b38097f3662139e77de98c"],["/posts/309775400.html","53ca3e46292ca5294d8c6c395e67a03f"],["/posts/3156194925.html","be84ff4d6cd1d46da5c4fdeb93883952"],["/posts/3169224211.html","019832bc9a981fad7f7b54349c7f0206"],["/posts/3213899550.html","c82e6b8c3c742463beb78bc256200215"],["/posts/3259212833.html","1306c3d50ca3b672abe95f7a17d92997"],["/posts/3266130344.html","06f35ceecd6e8ace3ad7ada3e4864537"],["/posts/3292663995.html","65df2b288431c43968c49710665bad36"],["/posts/3297135020.html","cc8af41992695660a6a3a7e1538d5685"],["/posts/3306641566.html","5040fb1557efb91b9e9d96a13619d262"],["/posts/3312011324.html","86c4dbf47bafcaa42b499f138827429b"],["/posts/336911618.html","725b397b5d3986245a4ff30acfdb3949"],["/posts/3402121571.html","f973b2d262d44f64deba620487a44e9b"],["/posts/3405577485.html","6e21af0afb25d65b72fe7ee17b74d9e1"],["/posts/3498516849.html","c5719a376bb35da7c442e2b1389eef54"],["/posts/3513711414.html","7efe6c748bcff671ef3ee39f713cc861"],["/posts/3523095624.html","1e4ed96785d7f98f72383d0c117b896e"],["/posts/3546711884.html","7cb1f64875e0c8b9e303d0977fa2672a"],["/posts/3731385230.html","cd7edb4999e461f031dd29950cfb234f"],["/posts/3772089482.html","7ba7ce9eb16f267941c0eb70dae121d5"],["/posts/386609427.html","c5623539a9e9fe918495ddd0b766cc9c"],["/posts/4044235327.html","115409029fdade7940cd70aec6974013"],["/posts/4115971639.html","df86d7efadfc8d76dccd06afe1409c8f"],["/posts/4130790367.html","37bb878707cad06b76bcfd867d05504f"],["/posts/4131986683.html","480a5aec67c9b546b8c758a35874f4e6"],["/posts/4177218757.html","1d5e58496885cffdac1ed8986044b8a6"],["/posts/4192183953.html","1450b6a6f887d5d871c007f7a225dda6"],["/posts/4261103898.html","90acd808431647afc0650fcefbca4029"],["/posts/469711973.html","162af13a980536e9cfc20612591aa024"],["/posts/482495853.html","75497f92298206a5195374844dd2cba0"],["/posts/488247922.html","8ad28a22a21ddd36fa018eef6d927bc8"],["/posts/517302816.html","739e134eb3a010997056464ff27efb94"],["/posts/570165348.html","10ef0b62b95949bafc5278ffb8b4795f"],["/posts/595890772.html","36e77ecb311f6db1ddf8f2724db56e44"],["/posts/67485572.html","8626be20fe60b18c843b87ad1ac69330"],["/posts/694347442.html","279078ac93de7c57752d518ef954a693"],["/posts/707384687.html","2eb4faa4ebc0d0878f565870df93c83d"],["/posts/71180092.html","67f7996a67c8b725ef990d5196ad7aa9"],["/posts/716459272.html","f8b7636d6bbf44d466943ec236707c60"],["/posts/765481613.html","d7e3832b3a6a7dc5328b49a30295b32b"],["/posts/778231993.html","78ba93149b153f28d0d7cdb055e66f41"],["/posts/795397410.html","62769c33b41db40fdf89c3be3a9a0537"],["/posts/820223701.html","008330a5a4b42ee90ff37e1b15396c1e"],["/posts/830372185.html","eea01b16a8ef86aab7059b789202d8dd"],["/posts/88294277.html","7b94002474b3fa886275f3c9c59565ae"],["/posts/939963535.html","b22ba441fdbcb6bfacecb0e459280249"],["/posts/983786067.html","4b103df1f54c199cd642e785e0b04361"],["/sw-register.js","ab13bd3c3972ad2c9a3ccfd399637af1"],["/tags/C/index.html","49ffa115c746cbbdbb3ca1ea656a250b"],["/tags/C/page/2/index.html","2b1177db7c024569298376a6bf32881c"],["/tags/C/page/3/index.html","536d69547f76cf2ffffe3d8448e530d6"],["/tags/ETL/index.html","a70ebab776728feec0c62c2fdfc895d3"],["/tags/ElasticSearch/index.html","b912e8503eb0509e69cde8f9f66b007a"],["/tags/GUI/index.html","f3aa773f8a31e9c3788ca16b8df63699"],["/tags/HBase/index.html","0bafcf107ce689ba4f86f14e09c5a5c3"],["/tags/Hadoop/index.html","581b50ad29bd64038dcbc33a9a5fe342"],["/tags/Hadoop/page/2/index.html","3a42c825c090d32102d0bda578a0701b"],["/tags/Java/index.html","1b575bebcecd14be760d6c2158455e30"],["/tags/Java后端/index.html","a8bb9f36804a2344d3762486a4531a8a"],["/tags/Java后端/page/2/index.html","b30414eab20e098607f8d6eaa8567abd"],["/tags/Java基础/index.html","9009e48169468f243177d133d772d9e6"],["/tags/Java基础/page/2/index.html","d59e4725d507a53b4b927d09dcd2e07e"],["/tags/Kettle/index.html","c4a40497b65bd6d92e8cbb03d46dc338"],["/tags/Kibana/index.html","3d165bf9bdce4c206736ada2a5d8e357"],["/tags/Linux/index.html","f888877994ad7414631dad0c370e08a7"],["/tags/Linux/page/2/index.html","7ad398e5eb173d38f5ff1647b72f0c7e"],["/tags/Linux/page/3/index.html","f352e4229667676eaa0c5078caa063f7"],["/tags/Mac/index.html","74c0b09b4b43877950511911387c7373"],["/tags/Mac/page/2/index.html","1cd1b58a988e451b443880040f4e441c"],["/tags/Maven/index.html","c3ab3c971a0d754dbb132a786eba1587"],["/tags/MySQL/index.html","7834b94c5f1592d783588ae735134afd"],["/tags/Python/index.html","8e5b742245dc1390ab450993f2fc7056"],["/tags/Redis/index.html","e5dbcbdeae9b7ca5dae46b5670591520"],["/tags/R语言/index.html","0cefc9c4d4e68ddfcf4e9b24cb8d77e8"],["/tags/Spark/index.html","0af8f337cd474cfd9fdd4db4ed8d5133"],["/tags/Ubuntu/index.html","e1bc4ef8e6e4226715aab4017aebf1c0"],["/tags/Vue/index.html","8d1d7b5c606f14489f6292adadfee92a"],["/tags/Windows/index.html","835554373163392d611b03f6a1e9aea0"],["/tags/ZooKeeper/index.html","0522efe04e8f8799cef59edca0fa8333"],["/tags/bfs/index.html","cd92897f6f41000a812e7bde5c69ac53"],["/tags/dfs/index.html","f52373fa58586d7877c99af95eaedc15"],["/tags/folium/index.html","a1c06aae6521f7f124ff2c23cc907def"],["/tags/git/index.html","c1ef2d288b9c4c05b9b198a795bbf4de"],["/tags/index.html","7d289282704a2597d5f522343036d6f4"],["/tags/latex/index.html","3fda8cd153b4bf885470b3fbca1d5434"],["/tags/中间件/index.html","1bbfb11125eca2d93fb7ae02ff8ff945"],["/tags/二分查找/index.html","1c0eb320a02023c6f9ace5de9fdc3e3b"],["/tags/优化类/index.html","0ed7ab47528278e7f3018d6ffc1f5682"],["/tags/前端/index.html","aad4f0b0ba8b2ddb7f6caf5b92d18d72"],["/tags/前缀和与差分/index.html","fac91ab6e6d6dbaf3c2e586fa4ed2776"],["/tags/动态规划/index.html","b26580eec58045fb1f1a4f293e0cc91c"],["/tags/动态规划/page/2/index.html","97aecbfa7381129234ecd53a0e1d84c8"],["/tags/博客搭建/index.html","c1f97ceb0636ed59a3f5446908320dd5"],["/tags/图论/index.html","4aa3824ff5aa6ac400c329cd1265014f"],["/tags/大数据/index.html","2862a3731f94dbae8c2a7b766f428f98"],["/tags/大数据/page/2/index.html","9f51253b5af53411e93fbeed795f6318"],["/tags/操作系统/index.html","3ad89b4a2611a569c9890fd452d869f9"],["/tags/数学建模/index.html","fd8c75421c9f4c13a811e44f04d2f942"],["/tags/数据库/index.html","c5904c0359d41e35e2d44776dc37048a"],["/tags/数据结构和算法/index.html","c07e72224728cd3d6c85b7fdb61acaaa"],["/tags/数据结构和算法/page/2/index.html","3756246fab8dcb4f464a7d72dbab6f34"],["/tags/数据结构和算法/page/3/index.html","ffff702635c64909f8a137dc0ba3eff3"],["/tags/数据结构和算法/page/4/index.html","c508104a12f6bc711c8c880148f05c37"],["/tags/数组和字符串/index.html","4d4650cb2a0ced5986cf979016efb051"],["/tags/数论/index.html","321be3e8e20ff38eae524715103b6c1d"],["/tags/枚举类/index.html","3964eec6608c39b38c950fad39e83d62"],["/tags/栈和队列/index.html","4b7acaedbdd45f891ce6edd02a77d20c"],["/tags/树论/index.html","86038070c39cc30289c8612bfa080c3c"],["/tags/测试/index.html","2d253ea3b91f6568c49cce6f79646f84"],["/tags/环境/index.html","39287e3851d26d06e87b93aa78be14e3"],["/tags/环境变量/index.html","7870e12abbfd1afbd529c8f50cf18493"],["/tags/绘图/index.html","f04d52ad19371ba68d0cb50a51009580"],["/tags/编程工具/index.html","d5ab4620c407d252637c31fb070e65b1"],["/tags/编程环境/index.html","8d18a10c3a298871b4dfad765fd4572c"],["/tags/网络编程/index.html","690901a54880c4cdfe11ea17020aa432"],["/tags/英语语法/index.html","7125c5804ccc7a05af0d492fa5cd4c78"],["/tags/计算机操作系统/index.html","91940117c012314402027638ad181f56"],["/tags/论文/index.html","4352797f4356d4d8591f703bf0b935e2"],["/tags/资源下载/index.html","52bf4f27cc9f76a0eda8ca9fc54dd19d"],["/tags/链表/index.html","e5dd6d713e582e649a383c5f422953c7"],["/tags/集合/index.html","4fc6f9abed44fd6c584caa1c34c36a5b"],["/tags/集群/index.html","acfb7b26d0365f4a2056971d075ab237"]];
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
