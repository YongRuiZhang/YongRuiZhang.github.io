/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","35105649ce1b9c9116d1bd62f3a71cac"],["/about/index.html","b1ca1cb94e1da683784b40f82fc47943"],["/archives/2023/01/index.html","8a056d7e3572751eb9fafb12de4b6717"],["/archives/2023/02/index.html","2782332c6c3d8ceb53bb99e5927cb8e9"],["/archives/2023/02/page/2/index.html","aa4d3ce27910fde7e5b6fc589360116c"],["/archives/2023/03/index.html","81e6511b4f41b4091216b517fc20eb66"],["/archives/2023/05/index.html","1b2326fa3ff913fa380096563721bd8c"],["/archives/2023/06/index.html","d668762feb92db28a3508fe0b9675758"],["/archives/2023/09/index.html","bdc52918aef6931f9b8d8975ded90bfc"],["/archives/2023/11/index.html","14737c2da666f0b4cc54d1f275a8a423"],["/archives/2023/12/index.html","fb47ee6d9b6d8b8e2f724ac6baaf978d"],["/archives/2023/index.html","2e7c861013048afb52794ee76f93fe78"],["/archives/2023/page/2/index.html","8df6ffeada7920c94c1f5f5b41ccb501"],["/archives/2023/page/3/index.html","660e257e7226bacddc30c6f183dce56a"],["/archives/2023/page/4/index.html","d0a49a37e4557993c7c74d0c4684669f"],["/archives/2024/02/index.html","b8ac3e3bf5d0884991cce10e85e8bb50"],["/archives/2024/index.html","9f43729d1b062685f42c4032dcff495b"],["/archives/index.html","50347e7275248b19ec24a66a0fa20b1e"],["/archives/page/2/index.html","ec7cf9716765ffb3617a6fb0395f5b3a"],["/archives/page/3/index.html","5a2b9acabf8e646e39e83a8d3fd145c0"],["/archives/page/4/index.html","cc6503823b82641666b2c9df2c3c49af"],["/baidu_verify_codeva-qQP2iZOMLX.html","c8f97dd71807827a525904289d5f6bb3"],["/categories/Java/index.html","3f8a8b3eb16630f632d1baef225333b1"],["/categories/Java/后端/index.html","bd097cc5c3f206a4a8852b35cd37df70"],["/categories/Java/基础/index.html","c430e4b2c690b42a3989b6e1aed883bb"],["/categories/Java/基础/集合/index.html","4c46726f5dc503eca85098633cfde900"],["/categories/Python/index.html","479a22bc91d0e1ca36f2fdf759b90ff6"],["/categories/Python/编程环境/index.html","6d582143c1af0744e849151eb141d122"],["/categories/R语言/index.html","5e38936285ded4c573ad15914f8e25d0"],["/categories/R语言/编程环境/index.html","58b4d35130df05d5d0d21220c1375a8c"],["/categories/index.html","30bdabf7e4539c5591074f5ba98e0df2"],["/categories/中间件/index.html","6d5fca0c19e5e793ded7cc1873ab6539"],["/categories/前端/Vue/index.html","e82859a12b0b013c0094895609f4c351"],["/categories/前端/index.html","62274ec9795afad19423147ae4e7bbbc"],["/categories/大数据开发/ElasticSearch/index.html","73373bd1d2c99b7d2d5162778fdf4289"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","a878a589d20e4cfec97a2bf6390637fd"],["/categories/大数据开发/HBase/index.html","cb4a96008207e014b4f58caffc0d6590"],["/categories/大数据开发/HBase/学习笔记/index.html","9b89e0f234dea39e5655d32b8c3eb20d"],["/categories/大数据开发/HBase/环境搭建/index.html","45f7b3e34042758f4833c5318b40f7fe"],["/categories/大数据开发/Hadoop/index.html","cf240a1ffcc4fb032736a315dc421f68"],["/categories/大数据开发/Hadoop/技术/index.html","9c7388cb7777e25b458a285e4c2aae39"],["/categories/大数据开发/Hadoop/环境搭建/index.html","d826d0beaecb160325053c32267f02c4"],["/categories/大数据开发/Redis/index.html","564267517aff537a7f1f1fc5c7cb5363"],["/categories/大数据开发/Redis/技术/index.html","60295bb31d884cd9db3b4a7085b4b432"],["/categories/大数据开发/Redis/环境搭建/index.html","715beca24ff347c8d9fddf57daa674dc"],["/categories/大数据开发/Spark/index.html","c439330378f5071bcba68613bfa6a182"],["/categories/大数据开发/Spark/环境搭建/index.html","3d84fbca933fd5526a5663c112e9b04a"],["/categories/大数据开发/Zookeeper/index.html","635e66120bb7b3e7e4c6b4e07beb5ab8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","cda271b07c9d903fed0578a76fe8c1d0"],["/categories/大数据开发/index.html","2698c102755220db888fa743bebdd45b"],["/categories/学校课程/index.html","3a11472585e9ede269a87140dad81213"],["/categories/学校课程/计算机操作系统/index.html","ef76de162de9ead8444457165e0b6d0e"],["/categories/操作系统/Linux/index.html","b7e6a718a6b884cc818e7983090b1ccd"],["/categories/操作系统/Mac/index.html","afdd2a9b7a0f595a22ac4aad764094f4"],["/categories/操作系统/Windows/index.html","8b9807c5a3a5c59dbaea7219381b8a86"],["/categories/操作系统/index.html","cc345fe1cd44dc165942e3cdfdae87e8"],["/categories/数学建模/index.html","c0043fe41746f428d250fd41b28d491e"],["/categories/数学建模/latex/index.html","1b8afa8bac5ceab57c33da9edf49a240"],["/categories/数学建模/优化类/index.html","663e27c73fd3be74cf2bfaedd4b3de4f"],["/categories/数学建模/优化类/现代优化算法/index.html","11e58fb5c438ca324e2641218844d20c"],["/categories/数学建模/优化类/规划类/index.html","20785d850dcb98f2395a810ecdcc4918"],["/categories/数学建模/绘图/index.html","16d50125dc5c8134bf99fe9eddee2f3a"],["/categories/数据库/MySQL/index.html","8da1bfd10fedd8b84af2f5fba44bce6d"],["/categories/数据库/index.html","d4b5f80188f063885f3999e5a79fafd2"],["/categories/数据结构和算法/index.html","21965a091506816ae676fb7bed8de49e"],["/categories/数据结构和算法/page/2/index.html","e0bb1d70c5446d09663351675d6cff4a"],["/categories/数据结构和算法/基本原理/bfs/index.html","6ac630230b568bbfc5992024e027cb17"],["/categories/数据结构和算法/基本原理/dfs/index.html","2f592021b09fddd29bfae4d43ecdef0d"],["/categories/数据结构和算法/基本原理/index.html","db44e972c2b35c5997c08bbc5c8eedef"],["/categories/数据结构和算法/基本原理/动态规划/index.html","d022a8027cc5218b40a0fc2d1e58084e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","375420fd214f1261b77708731d04e57e"],["/categories/数据结构和算法/基本原理/图论/index.html","98d8197545fd82c094fb4bcb640b60c0"],["/categories/数据结构和算法/基本原理/字符串/index.html","f9adbdd6d12194e310415ab149e33fe9"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","c64459d7ed89a00cb79ce9042617d57e"],["/categories/数据结构和算法/基本原理/数论/index.html","18fa2778f1507e00da6303296f72f418"],["/categories/数据结构和算法/基本原理/树论/index.html","be474051f9c22a7cd25ffe7705e46143"],["/categories/数据结构和算法/基本原理/链表/index.html","9993857ccbb92b7d27f335c6665aa49d"],["/categories/数据结构和算法/算法题/index.html","55f3725d63a2fcb629b5bfd54ad8ad0b"],["/categories/数据结构和算法/算法题/二分查找/index.html","a7e5a76acc42756809ab7bfe5881ea6f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","08b1037b8232fb44395197f1c8199779"],["/categories/数据结构和算法/算法题/动态规划/index.html","d3494fc317126d5be4381c75f72d6920"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","f4d612596f6560c87ac39db96b65b332"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","dfec2f280a7ec07b01b4f4587b8b9353"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","be95f9109f3205d7d93549af3dfd509b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4d1d541abfe845da95d9a49a26079555"],["/categories/数据结构和算法/算法题/数论/index.html","ed98c13739164660d5031d731b623516"],["/categories/数据结构和算法/算法题/栈和队列/index.html","0d9d90d78ab1a706f559af0507a690d3"],["/categories/数据结构和算法/算法题/树论/index.html","2b19d203986a2418f749786ac20a6d96"],["/categories/杂七杂八/index.html","b2fb0eab7c22ed2fe2a714b8e14e63a3"],["/categories/杂七杂八/博客搭建/index.html","01485855eacfc6edd886b73ad29e5fe0"],["/categories/编程工具下载/index.html","53b99eb8847e19766bbc950305e3db13"],["/categories/编程环境/index.html","fb0fd5ef7ee2acb9ef606e814dd124a5"],["/categories/编程环境/大数据/index.html","8ea1902ed5809503dda21f48a2be7b4a"],["/categories/英语学习/index.html","d156982d7f862c650afb917f598f9ed5"],["/categories/英语学习/英语语法/index.html","f2d7ecd88426f1d1e0d7afa97f3310e3"],["/comments/index.html","d277dbe9fb2125fdf1135ae961bc1a37"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","fac74a1b0ca90d29de6678fbc369ad64"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","49758016ca46f317cc6e08ffc4204f16"],["/movies/index.html","3016b71f1f2e984d50b317d8a4c9300a"],["/music/index.html","8bed18f70d4c87f2a05bfdb3300f2b66"],["/page/2/index.html","a78c64cfac477dd575f1e513dfd9f8a9"],["/page/3/index.html","b8ad00c78fd02ff0fea06073d78519d8"],["/page/4/index.html","35f7a8551abccad086267646e6b53554"],["/page/5/index.html","9fe0b3e7789715275830522f17786616"],["/page/6/index.html","f00bdb5b0420f2ca107b96593af83a28"],["/posts/1021360842.html","bbfc544ead8c60b57bfb357d9f05b221"],["/posts/1120620192.html","b6f85cbdcdc5b0592f7985d74f618ac6"],["/posts/1141628095.html","53e06906c6b0447286c0e09cb99bc2ba"],["/posts/1168613674.html","37eeb434d385d625eb4b474c5eb824f4"],["/posts/1219920510.html","b782e1ad08c7c44f56ea5fd264e07959"],["/posts/1222166338.html","bee560216cee98da743a2a2193ead097"],["/posts/1259097482.html","676ba8c8907687757f286b2fcdea8edc"],["/posts/1271036369.html","17588456e079977db5bb820e5cecea19"],["/posts/1312847445.html","2ad1fedd21b0d01729166cfeb98e7dfe"],["/posts/135355774.html","4801c73178780a53b041f93809f2cd13"],["/posts/1375344716.html","c2d8ebfe59e1521113073132b852a5a5"],["/posts/1388991698.html","19eb61b342f3c973196dce8f2935b797"],["/posts/1410315814.html","6b89fc2e8bb8baa52635d0350d35a1a5"],["/posts/1452790229.html","3dfa8225416a4bf3c4f43d74ec92a67a"],["/posts/1470079884.html","db5fb878b7091dc8338fa2451416686c"],["/posts/1470079885.html","266a13b863eb9daa7203c10b932dee23"],["/posts/1470079886.html","33ac87b56af2ba8f4a7074535cdcd099"],["/posts/1470079887.html","92b740cc9b773c7aabd4d1b53d8ed32c"],["/posts/1498536549.html","86865496989ab3bae672652fb77b73c2"],["/posts/1539568593.html","5b9ea06ecabed739672740f433a6eadb"],["/posts/1547067935.html","d8bbacbeba51f02679cb596f4970553b"],["/posts/1557866301.html","7c427941e864921d47c9ee5a273cfde3"],["/posts/1571776361.html","e68bb5ccddd42d0bb7237f4f5b5594b2"],["/posts/1605124548.html","64a2ce7a481ec83c98bf649a6de3656b"],["/posts/1633036852.html","1b7c97cbd0a202887af0af5dc05445ef"],["/posts/1674202625.html","e3cdfa2807317ee70d3f4fd7ba622468"],["/posts/1765123828.html","852185664958531684f21564757bc954"],["/posts/1767336200.html","78b38bef363cf17f4301a4abca21daa9"],["/posts/1776114197.html","2a00e5d8b334ec692a622505590f8b83"],["/posts/1817748743.html","433a2286dfbbe3be319ef103289e50ac"],["/posts/1925125395.html","615acc9017554ae7d16c1c92aea5331d"],["/posts/1966191251.html","f6ad2d76d2ecaadca2fc3024e4ce643d"],["/posts/1987617322.html","06cef6a6c1cd538cd7c4cdd93d74eaab"],["/posts/1999788039.html","9c3df16adaf8e374682a9c2a74e5c515"],["/posts/2075104059.html","5bb709dd943509411f8ef74fb2fba4eb"],["/posts/2087796737.html","371a72f6119e0c4ba7873fb5551c3d67"],["/posts/2106547339.html","66a15ad58f404ab490cdf39317b7bf0d"],["/posts/2207806286.html","aab6eef1808b20c4eb777cea797186ef"],["/posts/2225903441.html","7e856a377a159d9f705fae5e9d990f14"],["/posts/2265610284.html","5c2b0971d07541038f296dd4d83f4850"],["/posts/2281352001.html","699ce1feefb5c0e5aefbec8d09fc59dd"],["/posts/2364755265.html","20d7b87b495af0d4fff2e31a96f95a3a"],["/posts/2414116852.html","d985a9bc6888d50461bb3fc560b2609d"],["/posts/2421785022.html","ddc2021ccb3428e59360d27e13cf8cb5"],["/posts/2482902029.html","b7ac670467e4d8de2835ccfa838b4a0a"],["/posts/2495386210.html","680ffa1285ab3b47765c830e1b916097"],["/posts/2516528882.html","3ed2a03ed27f62e0615d17158c8ba1d1"],["/posts/2526659543.html","755b53e81919adc2035237ad062fa240"],["/posts/2529807823.html","9bf03d7b8e50f681a1b3a2a8ecf6ad33"],["/posts/2596601004.html","a61ed5dea60b103ef5537a9bf76d5845"],["/posts/2697614349.html","76f6e5453555beb582c984a3239d7ba8"],["/posts/2742438348.html","a46d133cafd74768cf83c5310c10764e"],["/posts/2768249503.html","e18b83abed4b33e53de1146bf26cc278"],["/posts/2864584994.html","81d149236a559914cc6ed1d6598be169"],["/posts/2888309600.html","9997fb1039d70fbbaaed08eb03417ec3"],["/posts/2891591958.html","c1822e38dfc6bb6b43348c5eb2dcc5f8"],["/posts/2909934084.html","7803e556f8f2a25c61dbbdc3a854cc0d"],["/posts/2920256992.html","046a02dcc1e2e217f9f8e0638b0ddb3c"],["/posts/2959474469.html","a1204087d667417377d831ecad73fa88"],["/posts/3005926051.html","b1005be7d34e3244319603c340d3a283"],["/posts/309775400.html","7aea3dfc0c0893ce5d8cb3758cbe1b97"],["/posts/3156194925.html","8872f8afed9d239e4bd2d8096e3a5702"],["/posts/3169224211.html","9f1af0b907777bd7eebe3873b5ce7382"],["/posts/3213899550.html","549b9511180f98d6f7977cc4cc8739b3"],["/posts/3259212833.html","7935d7541f1d66b2deb72d581a499d09"],["/posts/3266130344.html","ccb4d0acd8a5af5e636250fdad100865"],["/posts/3292663995.html","708db42a55bdde64006310a5e96804fd"],["/posts/3297135020.html","814ad7ab5f6b4e7d4fa5af36b6c2db9d"],["/posts/3306641566.html","aa6c12bc058457d5590559820a144bf4"],["/posts/3312011324.html","d0655e84e52dcc33a2adc26d83010282"],["/posts/336911618.html","0f6847b832f76436563652d847151fbd"],["/posts/3402121571.html","d5795bffef4f705eaeac9d505b945aa2"],["/posts/3405577485.html","4eb78dceae182a4e7e5f667bbff2c279"],["/posts/3498516849.html","f53bfb17a3673cb3ab5e1ea4a800adc3"],["/posts/3513711414.html","5337713fedbfd8ffc7ef693b4eeafdcb"],["/posts/3523095624.html","79ac734e260c0a0ffd1dac5709c0523a"],["/posts/3546711884.html","c1675c62323266a453d1da87a1eff35f"],["/posts/3731385230.html","fdb83afd41e485ae18145c2293123137"],["/posts/3772089482.html","9106aa8946ce7f9bc6fa1ddeae1564c3"],["/posts/386609427.html","5157c15a81bf6654e9f1d9436a65270a"],["/posts/4044235327.html","6e0d461906cdfd212bdf8b57459ba2be"],["/posts/4115971639.html","8c7effa82018f93d9aef3f30866e45a1"],["/posts/4130790367.html","7b476edf24d3410aa00cae1f096713d9"],["/posts/4131986683.html","4aa3db6ef55c94c6be3b2fbcdbfb6e5e"],["/posts/4177218757.html","d6ae3a727adf1959c9bd6731b5c66c39"],["/posts/4192183953.html","b8b86e69967be8f2a972c53ed5fb349f"],["/posts/4261103898.html","e5eaf7a584d4a51a298a634aa4ea06ea"],["/posts/469711973.html","4119b0a724687ba52f35e87acbebeac9"],["/posts/482495853.html","0d24cc3352fd69029b3fb939557cf050"],["/posts/488247922.html","7cc76d09497a822195156208b64fd10e"],["/posts/517302816.html","489a8b5dc962b6f672582fdc9f5882d7"],["/posts/570165348.html","86cc9593e12dbc5ba41025e71d2e6e03"],["/posts/595890772.html","f599f3d9d174c645a5baf5be9cd1d7d7"],["/posts/67485572.html","e2aaf40a4dd19b075740ad0776ce4c93"],["/posts/694347442.html","6e1ef52d1a94f7d7b2ec5aaa7da67520"],["/posts/707384687.html","0e600ba05739060a08c508bc2f3308d4"],["/posts/71180092.html","90d6623f872edef6d8d872fc2e61781b"],["/posts/716459272.html","e3f4d6f439d3645c6d7bd032af04e13d"],["/posts/765481613.html","e8147158d496dffdca9e0b3ed23e2375"],["/posts/778231993.html","71162791e5d1b23f1ee019b78c54cea4"],["/posts/795397410.html","a2c912dc93287a7a1c349b7871d82dd7"],["/posts/820223701.html","44d44846b9749f67a0e76e7bbc13bf79"],["/posts/830372185.html","0082bce3c06097c29f2b8074262225a6"],["/posts/88294277.html","252efcc1c2ac1ee2f2ca7dcd810c5968"],["/posts/939963535.html","ff86f9c681a0b0bee2f84b3ac4569454"],["/posts/983786067.html","9c6b0e2459878608589a7c94dd4540ae"],["/sw-register.js","165a59beb1cd469e2bca37134d0fa3f0"],["/tags/C/index.html","9972384dd5dd8044365c8ec47c22ffec"],["/tags/C/page/2/index.html","76552d58c0bd9992e76d7ab05a0a68de"],["/tags/C/page/3/index.html","f7f2b2f232558f53cc353810415784a8"],["/tags/C/page/4/index.html","9109155de8de368a5926f2a1d8424111"],["/tags/ETL/index.html","c35d199b985dae0b1feba369851c476e"],["/tags/ElasticSearch/index.html","13a7e8fa7bd2bccfcb5b9baccc25b357"],["/tags/GUI/index.html","a6a1cc594e59582fbe084ae9b8f6fcde"],["/tags/HBase/index.html","58bd27bc70258e2ea56fa66d387dacda"],["/tags/Hadoop/index.html","05b8252669223745677893a88381b98f"],["/tags/Hadoop/page/2/index.html","e58293ec9736f9621cc745bda6300933"],["/tags/Java/index.html","ba1c6278a76dee32ceaca7df84132bbb"],["/tags/Java后端/index.html","e3b62ed5567f0e87cc8257c2a62dfd4e"],["/tags/Java后端/page/2/index.html","b0a47c890b7f56967a091170ed0ba213"],["/tags/Java基础/index.html","eb4374824f609113399a38da0ae33745"],["/tags/Java基础/page/2/index.html","212e300eea6247b3b83b474d66033853"],["/tags/Kettle/index.html","1d8832638a2732487ddfab66becb60fe"],["/tags/Kibana/index.html","7cc53820c563984eaef865d58643b3eb"],["/tags/Linux/index.html","35357a8a266a201e43dbf8bf36659e77"],["/tags/Linux/page/2/index.html","66e11628bad62bc6c6d1753a995dcfd4"],["/tags/Linux/page/3/index.html","c2049eee760b03150c96288e3b32bb5a"],["/tags/Mac/index.html","e273df3b58ad87c42e647d0c585c154f"],["/tags/Mac/page/2/index.html","a13bdf37b8d827c6dd686fffc8e829de"],["/tags/Maven/index.html","cea27c8c1e7a6e6860751f04e3076be5"],["/tags/MySQL/index.html","9a440a28e613618a48d03194e1b3e5d0"],["/tags/Python/index.html","6f626063e781b30082ef3b545137d5a9"],["/tags/Redis/index.html","d7fa47b60ced80285d9ba4be40026476"],["/tags/R语言/index.html","30e0da0b4c38eb2767ac4a6f14f07561"],["/tags/Spark/index.html","4bf40c17f0c148f1ce3fcdca775b6480"],["/tags/Ubuntu/index.html","9ee00f7b288e0ef786db47bfa67b0cdb"],["/tags/Vue/index.html","9a15fa73897090e97b149444db220f9e"],["/tags/Windows/index.html","69efb898b7ef8c2df0f3aac8eeea5a72"],["/tags/ZooKeeper/index.html","6a7b42e20b6a65a617e81c402315cec4"],["/tags/bfs/index.html","2c7594de89b72551b0bdafbefd67be9c"],["/tags/dfs/index.html","aa81922a506a6feb7b27955a65f9d1e6"],["/tags/folium/index.html","19a1bd82a305e9531fa1763f0441234e"],["/tags/git/index.html","3365f34a464efd11a957bcfcd643ef86"],["/tags/index.html","8863de2551c39941cce3060dbccf07bc"],["/tags/latex/index.html","a6f803d5edb70837994f0196c068ce19"],["/tags/中间件/index.html","ea7452c650713e368b760588c00d64a1"],["/tags/二分查找/index.html","db647934e75006745c00d30dbbf801c9"],["/tags/优化类/index.html","61137cdc8a47b252e36076d64716d6bc"],["/tags/前端/index.html","91d149089008fff3e17c321a389a5a00"],["/tags/前缀和与差分/index.html","35262146e199ec165152ead7a3231d18"],["/tags/动态规划/index.html","9da4c8854bd1ac3d7ac14674c4738132"],["/tags/动态规划/page/2/index.html","ed5cb0ac5b650cf201b5c27254f6af6f"],["/tags/博客搭建/index.html","99c299f04e5678476ae3db4908fa47fd"],["/tags/图论/index.html","cff7574b5da551d358057c1750ab8ab4"],["/tags/大数据/index.html","3808653f852f0b9dd78745c99716cf2c"],["/tags/大数据/page/2/index.html","32bb1f7658d573208907f3d3ff2977cb"],["/tags/操作系统/index.html","f3db8cc49ef562b7d764fe42eb3b452d"],["/tags/数学建模/index.html","0e102aade813838ff936067382410cf8"],["/tags/数据库/index.html","5e11ee2de77cb1e5ba98c7aa9945c025"],["/tags/数据结构和算法/index.html","a5f954c3766bca7e729b6f2a2be8a3ec"],["/tags/数据结构和算法/page/2/index.html","b789b0912561cbb4ee66a86f120b594a"],["/tags/数据结构和算法/page/3/index.html","6d6169b6be00d0ff7fa2d6ad04ca0947"],["/tags/数据结构和算法/page/4/index.html","27dabbcb8de3ada0fd62d7eda133a1c7"],["/tags/数组和字符串/index.html","b946452168082320e6db210f32370816"],["/tags/数论/index.html","0b15932f1887968c2f05e47eb606c5b7"],["/tags/枚举类/index.html","b767d65e4ffda6d0bb89a984fe6be182"],["/tags/栈和队列/index.html","5ce0bef29f1a2217b0ceaff19bd5d1cf"],["/tags/树论/index.html","d964023ced4c1cb088c66ad63bc20ea7"],["/tags/测试/index.html","30f3428c5519934bf27e7bfc6958b035"],["/tags/环境/index.html","6b38ed674832a73bcdb2fb79176c180b"],["/tags/环境变量/index.html","8f2c6b87739c826433b342e18e548bf3"],["/tags/绘图/index.html","73e4ae72e915977d9cc6d106b40ba3c4"],["/tags/编程工具/index.html","5185bcdf1ebe0078f4fbea5f113a069a"],["/tags/编程环境/index.html","47a4091f54040be32e912c011d5c7fa1"],["/tags/网络编程/index.html","6ea39fd2bbf00437ea2aacfb4fd0e263"],["/tags/英语语法/index.html","e23e3e0dd3e4bfde3567bf99dd57d2c0"],["/tags/计算机操作系统/index.html","1127a28120c106b57981b975f08e2948"],["/tags/论文/index.html","b02109b21f6befff06d39dfc53a9d866"],["/tags/资源下载/index.html","26e9dbead1aeb4fcbf46387aeef398dc"],["/tags/链表/index.html","49c157f5a020c0be12f6e546fb9123d0"],["/tags/集合/index.html","4b6f48194d86fc25cb765799964d550d"],["/tags/集群/index.html","6857fbfa830ab34ebeecb3e7ffc3037c"]];
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
