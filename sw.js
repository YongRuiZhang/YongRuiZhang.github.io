/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","18c5c980bfcbf52a0235c1f879de70da"],["/about/index.html","8dfd955db854a05214fa7f6674588ffc"],["/archives/2023/01/index.html","97b34e07c9646c4b961675098b14d8ac"],["/archives/2023/02/index.html","fbb07a6a1cc6f43148c0a83b3f925430"],["/archives/2023/02/page/2/index.html","f89c1f9257d6e65f9b1bb5b2dd111639"],["/archives/2023/03/index.html","14ca29eaeebf00c30c880f39a064ad1a"],["/archives/2023/05/index.html","e14770fafbdc680098ccd30ea14028f5"],["/archives/2023/06/index.html","69948fc653879a693cd246810cb7bf13"],["/archives/2023/09/index.html","d3ac74a781385e6c75c918139f6294a6"],["/archives/2023/11/index.html","1ec274343e551855ecb46a20666b5094"],["/archives/2023/12/index.html","9e94f4293df424d480f97f4dc550d503"],["/archives/2023/index.html","2bf9a28a7db88a31e63860853aef8635"],["/archives/2023/page/2/index.html","012a4444492451f158c12c666e453047"],["/archives/2023/page/3/index.html","f123eb9ea2944384873d098b8bcaa762"],["/archives/2023/page/4/index.html","2df90d51d09fe991191057f29b0110a5"],["/archives/2024/02/index.html","0158b6c12c9736d8a83e6a2272b772dc"],["/archives/2024/index.html","2d7f78d2db4678178d1a86ef653ceb19"],["/archives/index.html","32a4de2ea680921e1865edced880eb84"],["/archives/page/2/index.html","6bdcf183b622b0b8d2feb5d2fd47d400"],["/archives/page/3/index.html","42c1f3beffd71f85d8fe2de68f0caf52"],["/archives/page/4/index.html","935e8730bedd2219a20dc92291026ca6"],["/baidu_verify_codeva-qQP2iZOMLX.html","9b79ccbad8caba6cbde9be77a525caaf"],["/categories/Java/index.html","cc1efe5ba7faefd890f1efd0d876c914"],["/categories/Java/后端/index.html","60bbaada5ba43ac8d93b4a57fe691051"],["/categories/Java/基础/index.html","0428b613ffa1c4d1f65db5955eaffdb6"],["/categories/Java/基础/集合/index.html","2a2059764956ee7aa012940093b50ab9"],["/categories/Python/index.html","8774706f2c0b5fa774eaf14b35d56d11"],["/categories/Python/编程环境/index.html","8fe6c2ada5bf5244d8ff3d64cc4d04c9"],["/categories/R语言/index.html","82bf2f7138e20ee2084ca17c2453530d"],["/categories/R语言/编程环境/index.html","4d827a55121f62841e93bdccc520696a"],["/categories/index.html","a119e808c11c06610e0962c42e8a8e65"],["/categories/中间件/index.html","bfd1d7446a0c5ee383c60d7ae85578bb"],["/categories/前端/Vue/index.html","c41d8ef3fe6af3dadc43ff9cf02dedb9"],["/categories/前端/index.html","d992517ed7b88ee526a1597dd22f28ba"],["/categories/大数据开发/ElasticSearch/index.html","fa8b913133ac65aed5558e696cc8a4ef"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","ee7b4480ab717fdecda71fa0cd2f9c89"],["/categories/大数据开发/HBase/index.html","7481034e119ed0699979062c481182dc"],["/categories/大数据开发/HBase/学习笔记/index.html","b3291315c34b6ce5b5ba71f4e358e504"],["/categories/大数据开发/HBase/环境搭建/index.html","163713e836a17cc9bd8068163c22fb1a"],["/categories/大数据开发/Hadoop/index.html","b44568e386d8641b5f95d2e477c1dc9e"],["/categories/大数据开发/Hadoop/技术/index.html","b95aa0454c30889baf57dd208ec15048"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e11d4a2212a69c09026fe663478535a4"],["/categories/大数据开发/Redis/index.html","6e9dcfe61bf942e7e9092b5157a23d82"],["/categories/大数据开发/Redis/技术/index.html","21cff7bb294a44472e0b1ac9eccc4f31"],["/categories/大数据开发/Redis/环境搭建/index.html","25498deef1ef649b705258a096df260b"],["/categories/大数据开发/Spark/index.html","5a39322da2219a081af123958cae8008"],["/categories/大数据开发/Spark/环境搭建/index.html","6391c731c6bda963ef39aeb2a5c96962"],["/categories/大数据开发/Zookeeper/index.html","eb0138d1603565d0c697558ccee5f8ee"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","2f3011149409cf584a7cb8cbc9301da1"],["/categories/大数据开发/index.html","8da6c335bc3ffd76774b74081d3cb99f"],["/categories/学校课程/index.html","e426cae950585fd644473775a1960c7f"],["/categories/学校课程/计算机操作系统/index.html","f1d2a8584d52ba6c3e678d679bc4da4b"],["/categories/操作系统/Linux/index.html","11a8d28d815e6fd3acac2928dd5d50fe"],["/categories/操作系统/Mac/index.html","3f8f4bcc818ed61bd1899e50f7456726"],["/categories/操作系统/Windows/index.html","a35b1cbee341b450436b6519918c69eb"],["/categories/操作系统/index.html","3a2ad401d1a279feb0b73965a1a1e65f"],["/categories/数学建模/index.html","30d3b7e7aaa691d3fb6c734566ceb62d"],["/categories/数学建模/latex/index.html","ac2be4b833b78d1e1ad2eeb7533b0907"],["/categories/数学建模/优化类/index.html","19ef65db1a962a12aed098a7448d705f"],["/categories/数学建模/优化类/现代优化算法/index.html","73feeb2ae5ad9f5efb87f9601b0121df"],["/categories/数学建模/优化类/规划类/index.html","9f9da4341180a81479d4b9e8825f1e95"],["/categories/数学建模/绘图/index.html","ba108f5bd519d42b39d32aaf98898e22"],["/categories/数据库/MySQL/index.html","5815ec3f5f7e845cf014c16634ca3188"],["/categories/数据库/index.html","dfc6aa5584662f13a375aa9dc34eb70f"],["/categories/数据结构和算法/index.html","3080c335cc0669090cceeb1b269787ab"],["/categories/数据结构和算法/page/2/index.html","b44419c96d7d5e50e4e77f64b8143230"],["/categories/数据结构和算法/基本原理/bfs/index.html","e76352a81446b71f19b10cfbff536118"],["/categories/数据结构和算法/基本原理/dfs/index.html","232f197ad948128c834104b0f7f898f6"],["/categories/数据结构和算法/基本原理/index.html","5cd55343c64545fdc4b308274022de16"],["/categories/数据结构和算法/基本原理/动态规划/index.html","884a553ca2ff08556a8d35b3c843bd21"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","88d8951c33a583774b22bd279188f871"],["/categories/数据结构和算法/基本原理/图论/index.html","ef7cc4fa59941104957b27111e265379"],["/categories/数据结构和算法/基本原理/字符串/index.html","e97086dbd01c5ad33a6a033d731dc397"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fbbfff08ccc1dcca0fdaa4649966ea98"],["/categories/数据结构和算法/基本原理/数论/index.html","c7bc52beba29ecf73ab7ffb3029847aa"],["/categories/数据结构和算法/基本原理/树论/index.html","6d514dfadf0a4764c2c759eddc11f541"],["/categories/数据结构和算法/基本原理/链表/index.html","c46ad5751041085a3e1589c3c8258b26"],["/categories/数据结构和算法/算法题/index.html","be752c6f7fa20e517ff07331830fc3c3"],["/categories/数据结构和算法/算法题/二分查找/index.html","dd158ee8a772d8f57af06d9bbc6ab484"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6b32215e3ae1650e2f63cec3f7e255d0"],["/categories/数据结构和算法/算法题/动态规划/index.html","236d478ad4e04e7b30f442cb5572afb1"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","fabe936db58284d386867ba71d3991e4"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","fda6b28f20531d446988c0633b85779c"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","f7d9d7e3db004efef9b8fbd246dd5211"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","7f1aadf98e93ef8e2dbb96b47f6f4ffe"],["/categories/数据结构和算法/算法题/数论/index.html","d90e935a5ad5aa8839d702c6fc0ffa04"],["/categories/数据结构和算法/算法题/栈和队列/index.html","23edbae2765e166c90b2e21330e1cba0"],["/categories/数据结构和算法/算法题/树论/index.html","37fc0e5eb2e57caf49661732608cdfad"],["/categories/杂七杂八/index.html","1a595bb06f3adbed4ea6e4192740ac0b"],["/categories/杂七杂八/博客搭建/index.html","14bb8718620ade2ffa0fefb307ad5952"],["/categories/编程工具下载/index.html","3901fe0a60f9485f281914446c17c401"],["/categories/编程环境/index.html","e95bc12652abfa9d246a4d1943229dd6"],["/categories/编程环境/大数据/index.html","335ac6def5cc1b7024ce4266dfe640f7"],["/categories/英语学习/index.html","316afea81ba7f271ee27d4c05c276987"],["/categories/英语学习/英语语法/index.html","73dd1be8efadd579f8a07e0365796472"],["/comments/index.html","567ba616fda1ecf6a152ea2091bf4d59"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","3ea84efbdb6bd5ce98be3ab7d95ff9ec"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","a2a135311c4fc4dfbe9c752f0e19c190"],["/movies/index.html","cd4507532f757595935b780b5dec7fb4"],["/music/index.html","f8d58be635d5f7c3a35d34f773ad938a"],["/page/2/index.html","df55bb23980a6a9419ec5d95b14c790d"],["/page/3/index.html","916f256ddc8c0f4de2d99859f9268497"],["/page/4/index.html","2fc1a7420f140a6d1269d028f97b6e91"],["/page/5/index.html","15858e23ed4fe67a90149ada2397806a"],["/page/6/index.html","85d7a6734d4fc08198b12f891c9a2184"],["/posts/1021360842.html","60be2ba33275cbd1ff445d1210c31792"],["/posts/1120620192.html","c193a4d690979ebff6900474797df98b"],["/posts/1141628095.html","13aa0f80ab6ce533519a1f1250e3e5fb"],["/posts/1168613674.html","37aae59cf9bf3da8bcbf8ebd956a23b2"],["/posts/1219920510.html","932c6aedcddf71147a20ba673aa09837"],["/posts/1222166338.html","e9bf5612bb1a8621aa458c4ae8f84ff2"],["/posts/1259097482.html","f3c2b95b37457fde93ecd04f5cdba39c"],["/posts/1271036369.html","c30d02ab56819ad16b54fdc7a81405ec"],["/posts/1312847445.html","a2d9c92753373e9101babfe8d35d059e"],["/posts/135355774.html","ae5de6d988b912c1d439d6009d288464"],["/posts/1375344716.html","687632d7e454fdfa812673bf4b9bc0db"],["/posts/1388991698.html","d5078f99edc9b16ce75c55ca105e391c"],["/posts/1410315814.html","120b4ca7a5eeb9f7f379866d21d9db28"],["/posts/1452790229.html","38fae3ee682301234ec973801306c612"],["/posts/1470079884.html","9c9e8fc40324b25e669b08a4c56cd619"],["/posts/1470079885.html","ca5a708193a519ed233421a65e53351a"],["/posts/1470079886.html","d81f2a26b87c6391e27c38d78624f13f"],["/posts/1470079887.html","4f5874c263cabeb991fadbad80223892"],["/posts/1498536549.html","774fa089d34025acc5d33a6efe354127"],["/posts/1539568593.html","dea3a45651bac6e9c0cf61b4a5693e71"],["/posts/1547067935.html","9a0f261f2a8547f180b62e9a75efa934"],["/posts/1557866301.html","59ac873b90f4898ec8bf1fcee2aeae4a"],["/posts/1571776361.html","a51f1b2a101b8b3ca34fd0ccc420a0b8"],["/posts/1605124548.html","3cbd219e967a9a57791f33d88a409934"],["/posts/1633036852.html","ecd6069a7304a56194f9d982b523faa4"],["/posts/1674202625.html","71a5c10588d72c931b32c2e227545255"],["/posts/1765123828.html","26151d3f98c869104472428ba2656167"],["/posts/1767336200.html","e877b45bd56064bbe8a42e11f8d4038d"],["/posts/1776114197.html","7d998ca1a1b8eeb90680fe858feffc3f"],["/posts/1817748743.html","a3e21a7115391d45a2b376ff5ac233bc"],["/posts/1925125395.html","ed65a524b68bc1d6a5c8e33f37dbef66"],["/posts/1966191251.html","c6731b5b88fdaa594f4b1e20f74b44f8"],["/posts/1987617322.html","78cf5c7cb1bbc3a1f7a9dd44d422ef75"],["/posts/1999788039.html","1d3054a1735062c43a10974ac6252f6b"],["/posts/2075104059.html","6fbbec76e7fe77fd8def42b53cabf688"],["/posts/2087796737.html","8f4bea7483d3145278ca514e031164f0"],["/posts/2106547339.html","617f2a6a433241e69d6d79ad073674a6"],["/posts/2207806286.html","50e150d0c94d61dd81c85f3c1b29ee6c"],["/posts/2225903441.html","b305678b57c0b1c55043d24e7d85d9a3"],["/posts/2265610284.html","cd47a6397c09cbf6791b0ec030f61d32"],["/posts/2281352001.html","e78149e8ba3503454f8da4646612f929"],["/posts/2364755265.html","cb67ae97e660e3ab739dfa045a19f7cc"],["/posts/2414116852.html","a6cca4c583edf999eb522bd73dd2b3b5"],["/posts/2421785022.html","446624b5c3b25af5b5270ab925cddbd9"],["/posts/2482902029.html","949607b147daba7ce54e59e15ac506bb"],["/posts/2495386210.html","1e7b6c3aa9f784a9cda7df0c6c1d24d3"],["/posts/2516528882.html","5b1e08bf52d644eb6e38b26881ff0b14"],["/posts/2526659543.html","8ddfc0d0560568836049152d38e93cb3"],["/posts/2529807823.html","7e5b517223a65f139e43279cecc4d918"],["/posts/2596601004.html","2084a6a97e149bff1fb8ee7186a95845"],["/posts/2697614349.html","d9b3481b61257a9fb2f2decd4f8e5373"],["/posts/2742438348.html","3b127493cf620499d5c713f7ba34a5f4"],["/posts/2768249503.html","b7d72cc64a5a9c51020549840debff5e"],["/posts/2864584994.html","8b1aa6f0e3b098bd226dd84a16ae9e7c"],["/posts/2888309600.html","47c16d566f287066fbf3dc19435b3b57"],["/posts/2891591958.html","feefb422a93e31d2eb8a7ee06a7e649d"],["/posts/2909934084.html","60f2a4c90a836aa1435cd3e54a781ffa"],["/posts/2920256992.html","34145e63c015c480a67dacca50875818"],["/posts/2959474469.html","a26bc3bb7c470b746942441196692368"],["/posts/3005926051.html","6b76bc85561baedb1adb1b6eacde22f4"],["/posts/309775400.html","6b68811d5d69752c0d9720fc823bb14f"],["/posts/3156194925.html","b38bd0f560448e6ffb135188c87ce58a"],["/posts/3169224211.html","7d49efa6c23d98bee51469de4a24d86a"],["/posts/3213899550.html","b66039c6fe2b2abf294da37ad3d0a909"],["/posts/3259212833.html","8e6421cbc05fc258320021a12a06d750"],["/posts/3266130344.html","e04c822b8a316d18cc5658c50036f2f4"],["/posts/3292663995.html","8214fab1cd12775497aa17de51e9babc"],["/posts/3297135020.html","7f6d8e6b61d0e76600aefa0894d66aff"],["/posts/3306641566.html","219effc56f6d3d2178867578f9498034"],["/posts/3312011324.html","cc3796e88f85998eff35d79340bfbd2b"],["/posts/336911618.html","1b22b7857f1fbf1561d21fb7c4f6222a"],["/posts/3402121571.html","162dbfe46d69f24f83c0f4b3e5429469"],["/posts/3405577485.html","5fd11a78d9b4845a16d38609061bd8a0"],["/posts/3498516849.html","0aa94ad830bf1a5816e9df17ee827074"],["/posts/3513711414.html","bffc85d638eef024fe70924a7517fe63"],["/posts/3523095624.html","f58775eab0a948ad5202631edd089455"],["/posts/3546711884.html","339f66a7a89878a2a022522064508fcc"],["/posts/3731385230.html","ba241c54b5e5ec46ee7b5191cd26f57c"],["/posts/3772089482.html","a10dd05f24b2431d52d40118b24fbb27"],["/posts/386609427.html","9356cea233e886d0acb68e420c8495f0"],["/posts/4044235327.html","844b894181569059ce6d5f7797b0dd0e"],["/posts/4115971639.html","b84522b95a33db77e04b5a7e3c64a480"],["/posts/4130790367.html","34ea70647cc67962318fb39d85c71589"],["/posts/4131986683.html","5d735681082d495d5889600d3c55ac4a"],["/posts/4177218757.html","d46a6e6940a0e9b4f91e0fb544a98497"],["/posts/4192183953.html","eef69c3fcad0e22824cfcb18cff69f64"],["/posts/4261103898.html","3f3e6d0e73413f1c826d0042ad533b73"],["/posts/469711973.html","3d1b5e3efb4462b30c22552f95aa98f7"],["/posts/482495853.html","412e603403129e8f03aa50cd8f3663e8"],["/posts/488247922.html","7780207e7ef1c9176c2f64452d7e748f"],["/posts/517302816.html","90308a85f38ae2f1d7552e7a388bb076"],["/posts/570165348.html","808642df29837b94ceb46a5004b0e73f"],["/posts/595890772.html","08aa4c0c2f6f7349e555b6973a4fc27f"],["/posts/67485572.html","9d074283b83473d9d166bf1b36cc8bde"],["/posts/694347442.html","20bd9e9cbe0f4f13d3664cff348be84a"],["/posts/707384687.html","15866d5ea8f552bf819fdc5573ba2436"],["/posts/71180092.html","fd467d62d4435b52602a877e898adb28"],["/posts/716459272.html","cd11b53521d6a3f8a1ed46b13dfadd83"],["/posts/765481613.html","4cc6d9dd2588dfc42b8344bfc453bb86"],["/posts/778231993.html","e8068f8720ff544940f811f38e6df069"],["/posts/795397410.html","bd59b4674206d2b2b9e98c75f7632406"],["/posts/820223701.html","64fb2610086035f62f6abfe384281708"],["/posts/830372185.html","cda486af51f6aa9817a625b71df7c700"],["/posts/88294277.html","c58441980707b390350bc5db25e991f3"],["/posts/939963535.html","9307b3e55750076bd6a1dfa2b67bf79f"],["/posts/983786067.html","b4bbb831f50305b41cd7cad720b7b787"],["/sw-register.js","db9f4c68a66f0a063969e1a21bd9a50b"],["/tags/C/index.html","ad1023278d0dae25e28631db91079c8a"],["/tags/C/page/2/index.html","3791cff1a073424254ada8f9093cf26d"],["/tags/C/page/3/index.html","18aa0f348e5e1b1a52f4f846ad6401e9"],["/tags/C/page/4/index.html","4067e1c78e92ec731cb92c172a25aefa"],["/tags/ETL/index.html","19fa78f94be7b1c5802cc45709696c15"],["/tags/ElasticSearch/index.html","5e2cf52521726049f6a7109a52a25713"],["/tags/GUI/index.html","070310451165c16222ea357195b3dccd"],["/tags/HBase/index.html","b77e26a10f24580540963409a7c666bd"],["/tags/Hadoop/index.html","2a25821787f0bd002c0f96cbc041222f"],["/tags/Hadoop/page/2/index.html","9d92efcf935da580952ebacc385dd8d3"],["/tags/Java/index.html","059462abc07382e0da7d64b53d160230"],["/tags/Java后端/index.html","c1f0b90166d81309749906140b19b56f"],["/tags/Java后端/page/2/index.html","1c8219361ee981c1b8b255322d47d3f2"],["/tags/Java基础/index.html","4b47aaf64567264d07789ee21de3baf1"],["/tags/Java基础/page/2/index.html","e044cd01445c7c1c3faa95997ab0a7c5"],["/tags/Kettle/index.html","b6daf2cf385286c3da4782ac1ab229c7"],["/tags/Kibana/index.html","e36e1a0d665359204c28fa60af9d28e0"],["/tags/Linux/index.html","d54ffc0a9cff6796420b80b66b953c12"],["/tags/Linux/page/2/index.html","fcbbd26759f2b4fe07acd208dca01ce9"],["/tags/Linux/page/3/index.html","bc8ada596dd2ab993bcb08ae5d923d33"],["/tags/Mac/index.html","515012a6824d52290196bd8ebaded4e1"],["/tags/Mac/page/2/index.html","2f6fd4a1275d565eda1ea3afcea002b2"],["/tags/Maven/index.html","79d49fdcff4ca88dbfda057db0fe1006"],["/tags/MySQL/index.html","ae3f6d79aa1a1c7b683140616842a16d"],["/tags/Python/index.html","b40692548e46edc0f0be00c9d09a5705"],["/tags/Redis/index.html","b8cd64910da845d9df002616fb0858c3"],["/tags/R语言/index.html","faa3cc9e31853085b910ee7d249ef7b4"],["/tags/Spark/index.html","e3f415f4a154a3572d294c0887e63068"],["/tags/Ubuntu/index.html","b5bbbf29c404194ee6c8e303a68e05f3"],["/tags/Vue/index.html","b24b91eb05548156c304a438229815be"],["/tags/Windows/index.html","fde91052a467a206082e98d28aab9549"],["/tags/ZooKeeper/index.html","d5843d65661f7a3bfc1d3b12bde039b5"],["/tags/bfs/index.html","5edb493a7fc092b03dad4742fee79325"],["/tags/dfs/index.html","d94c260e4424c356a463bce1a0b42f6c"],["/tags/folium/index.html","5888112403262c0ac7ac1b13dd331655"],["/tags/git/index.html","12bc82e91271726214c6921334335bbf"],["/tags/index.html","a014bf0ff4f3a11506c9169ba91e0c65"],["/tags/latex/index.html","5584bb069d076448cbeb90773b208901"],["/tags/中间件/index.html","4f4f844b3fadc98bdfd1c0a343b1014d"],["/tags/二分查找/index.html","d7a85b85a3735fd8e2f5cafbd9f8f9f2"],["/tags/优化类/index.html","ef6c4cf5f660e6af2f2ba63a3bce2f23"],["/tags/前端/index.html","b9e866f6b9b68c221bf328dc1f07c602"],["/tags/前缀和与差分/index.html","cbecfef2a1ddce0e0ab7a61a19194258"],["/tags/动态规划/index.html","81b21f147a2bcc5c3a9f86c917a58dce"],["/tags/动态规划/page/2/index.html","4a3ceecf0b76bbe2c264760c85cd2b78"],["/tags/博客搭建/index.html","5275f457db19426a1ef8f1700a510a2b"],["/tags/图论/index.html","3da845a8cc8513335255f53a9ebc95c0"],["/tags/大数据/index.html","2e18f9a5c132549c8338ffacf0c0639d"],["/tags/大数据/page/2/index.html","e386a9af739b45631bd4b148a0a722ae"],["/tags/操作系统/index.html","1f0b93a7dd882dae54ae052da4aaabd8"],["/tags/数学建模/index.html","4de1c199e9840336a6c1da85be807237"],["/tags/数据库/index.html","7a267cc5b754be9a9d0c7f44d5014925"],["/tags/数据结构和算法/index.html","de3f74196498b1a9f975cddcceedb7f2"],["/tags/数据结构和算法/page/2/index.html","e4d98f58a5589b1799eb6af45f7caeec"],["/tags/数据结构和算法/page/3/index.html","1680a15c1ba71c220a2b2e9456388280"],["/tags/数据结构和算法/page/4/index.html","defe05a39c072be76dd652981472e48e"],["/tags/数组和字符串/index.html","a808593af8d1fade9942657823c355d2"],["/tags/数论/index.html","48b876e17091c6f3a51ffbfbdbc9ab2a"],["/tags/枚举类/index.html","169ff2f51c4abcc051330833dff397aa"],["/tags/栈和队列/index.html","abc90f30dec52892e15eaa743afefb66"],["/tags/树论/index.html","44c5f1321c08deb12a8817417f743b57"],["/tags/测试/index.html","ab44c6b8b0c5cc2163a28739af2551af"],["/tags/环境/index.html","9c4e79d7a3e18f56b8e9e81c692db477"],["/tags/环境变量/index.html","9c2ef46d60e18edf62df24561f123619"],["/tags/绘图/index.html","2453119ff2502941a1a06afc9fe5c370"],["/tags/编程工具/index.html","3029b51477db93f77e7fff4b15454a50"],["/tags/编程环境/index.html","cbadbc857951d8ad00275727ec92ecea"],["/tags/网络编程/index.html","ee31ab191c61aefe3ffa665153415a11"],["/tags/英语语法/index.html","2ebf8fec5465f58ca470facf2a21742c"],["/tags/计算机操作系统/index.html","43d7d935cfaf7cf76e2de0d59c579282"],["/tags/论文/index.html","328533614a52db499f5ed8a9d1735b25"],["/tags/资源下载/index.html","52c84a78308e5fe9987a70ac0be551f2"],["/tags/链表/index.html","b2c4fd590c0c4374174df71e92e43627"],["/tags/集合/index.html","17cdef8e31b2d3e0fd6a86585769b2da"],["/tags/集群/index.html","81714a2b847840c7fe62ce042c05487a"]];
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
