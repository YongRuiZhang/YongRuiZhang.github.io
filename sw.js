/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","3ae4a3a67607a1f16fe50ad9fdc31ec6"],["/about/index.html","5cc07c1790c21e8aaa4aacb14ec0482e"],["/archives/2023/01/index.html","7f3d5be14ae6956a37dcd7cf7e9033cd"],["/archives/2023/02/index.html","a2315055ef641c293aa2c91d5cb71923"],["/archives/2023/02/page/2/index.html","687b49e1ccf985e1838d7f8c5e1a345b"],["/archives/2023/03/index.html","d181e45d0b216f34e7a9a8531dc5bd08"],["/archives/2023/05/index.html","f4d703aea6f596f121aa57ac3c2acc50"],["/archives/2023/06/index.html","dd481b89b2c50527eb8efe3e5083ae30"],["/archives/2023/09/index.html","21a6b474e6df5995893230a8d00a3457"],["/archives/2023/11/index.html","ebf178a869798a082383007e231c71f3"],["/archives/2023/12/index.html","ef962df4fbbfa33b189e4aae8e86da64"],["/archives/2023/index.html","7d80ba61a5f8adb0ea045e7ac3e904a2"],["/archives/2023/page/2/index.html","195aefa2a71238baec2a5ef3d49abda3"],["/archives/2023/page/3/index.html","73cf7fe710e631428f2c8060c3ab3920"],["/archives/2023/page/4/index.html","add581d5b3f6a48a94fc2d3b8b929136"],["/archives/index.html","b33e42ed9b7a95ce95c74be5ee2ef399"],["/archives/page/2/index.html","e96f00756b1e5b2ac38e215307df70ff"],["/archives/page/3/index.html","3ecd5c7a9ecfa653035ae3066e42efdf"],["/archives/page/4/index.html","18f84306e498842c727d105a0b1e0319"],["/baidu_verify_codeva-qQP2iZOMLX.html","3247f909c59eecc49ad1c56fc3ee9c5b"],["/categories/Java/index.html","385ea0578d7f839a6fcba12d0436d806"],["/categories/Java/后端/index.html","3c640acfb327806b3dac65d2472baade"],["/categories/Java/基础/index.html","feed61c93b7ba9435e168cc466e98ae3"],["/categories/Java/基础/集合/index.html","ea27845aebab32d4c675a517dfad7530"],["/categories/Python/index.html","58ca6431c8269abc289e0a7618b43378"],["/categories/Python/编程环境/index.html","05d2efeea19ab07876684fb528a41821"],["/categories/R语言/index.html","281e2f571b76bcd123f050ef88fc1bc0"],["/categories/R语言/编程环境/index.html","73b648a908b5526f1bd36f0cceab863e"],["/categories/index.html","ca9a1dd26543e37bcf76efaf87cefa8d"],["/categories/中间件/index.html","c306d77dde1352334ee11c4ad7728512"],["/categories/前端/Vue/index.html","af41876697661f8d294e19004de612ed"],["/categories/前端/index.html","8ca1ff9e27fcbf513f291608fee3da3a"],["/categories/大数据开发/ElasticSearch/index.html","ed426601aa163883375ab9d2fa4c548f"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","61031094f6d61cfbad75d90d478da589"],["/categories/大数据开发/HBase/index.html","1b1909d0114b2ed98436869dcd6a7460"],["/categories/大数据开发/HBase/学习笔记/index.html","1295f0b83096cd4dbd50ec43bdd32753"],["/categories/大数据开发/HBase/环境搭建/index.html","eefdd2fc34e1f242f0d4af2a5560bd22"],["/categories/大数据开发/Hadoop/index.html","7323c1df69f19b2ffe7357c4c4d26581"],["/categories/大数据开发/Hadoop/技术/index.html","afebb642bae8e068ca6e6a1ade56cd85"],["/categories/大数据开发/Hadoop/环境搭建/index.html","e73b3c9c53c42555a1ccff54c6f2511d"],["/categories/大数据开发/Redis/index.html","c3f5d9baa29d93422806859c88465de6"],["/categories/大数据开发/Redis/技术/index.html","b7adb5564034f871edba5081754bc8ab"],["/categories/大数据开发/Redis/环境搭建/index.html","8dfc9033c53ce00fa5748affde946341"],["/categories/大数据开发/Spark/index.html","ac4f42117a221f0becf24f936e84fe16"],["/categories/大数据开发/Spark/环境搭建/index.html","723c87f3bc604ac95933cd81076d195d"],["/categories/大数据开发/Zookeeper/index.html","6536f3627695edac36ee0c21f1caac55"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","73160d9c4755b7e8dd03012d52ec2f43"],["/categories/大数据开发/index.html","da8d32db1cc0fdf83e815a4a7278c030"],["/categories/学校课程/index.html","f10003b5f671ee711fbae03e6810b2fb"],["/categories/学校课程/计算机操作系统/index.html","f366961e9db5ac403078672d9ba62dfe"],["/categories/操作系统/Linux/index.html","81e0a16cea303dc907cb02accf5610c0"],["/categories/操作系统/Mac/index.html","15c63bf4feb566ad1a8c0cf21907ad51"],["/categories/操作系统/Windows/index.html","715f86c6619980d62a146439794c5258"],["/categories/操作系统/index.html","6120f38f0a0396dfc742b71648bcbd1a"],["/categories/数学建模/index.html","486f8ac0d24c055ecb82fa6837f17191"],["/categories/数学建模/latex/index.html","1913afa5d415cecb8f5e7bcffa2a3396"],["/categories/数学建模/优化类/index.html","db739961bfdd7cbedd6373f469c71885"],["/categories/数学建模/优化类/现代优化算法/index.html","43475279ed305163518ed9fbb85dd6a6"],["/categories/数学建模/优化类/规划类/index.html","2d3c34f4536c11c87390d92325ac5a21"],["/categories/数学建模/绘图/index.html","4378fc455e481710664745dd70608785"],["/categories/数据库/MySQL/index.html","cb4bae85287060241daa720f1c737292"],["/categories/数据库/index.html","455b74c08e903cb0fd6ccfff9a847e66"],["/categories/数据结构和算法/index.html","b569e89dd2cb599213c6682072c7f081"],["/categories/数据结构和算法/page/2/index.html","ded37850f8668bfc3f5a9e68c4719fe8"],["/categories/数据结构和算法/基本原理/bfs/index.html","c97e3108cb050c326f6f049d9c33be84"],["/categories/数据结构和算法/基本原理/dfs/index.html","47a1c5986494509e515be6c49c0fee55"],["/categories/数据结构和算法/基本原理/index.html","670b1b15f1ae23d098453b2b89e5bede"],["/categories/数据结构和算法/基本原理/动态规划/index.html","bdfb514e12deb4b14de4105315fbd7a3"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","b443565d881d8eea35edc6d218bbe0a9"],["/categories/数据结构和算法/基本原理/图论/index.html","81b02593ae583c55f931735302b487f9"],["/categories/数据结构和算法/基本原理/字符串/index.html","95f43e1436696b8c2734dcd2abda40a3"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","80eb76854368a8a782ffefd0219b3337"],["/categories/数据结构和算法/基本原理/数论/index.html","9fe446bb835285d64f8e7c6c5cc1e779"],["/categories/数据结构和算法/基本原理/树论/index.html","9bc966dc1c16e4ac559976e1cf34e8e9"],["/categories/数据结构和算法/基本原理/链表/index.html","d3dea2678b36232ed392868100d0a5a2"],["/categories/数据结构和算法/算法题/index.html","484152c7ee34a84cea3a707ce028b3de"],["/categories/数据结构和算法/算法题/二分查找/index.html","cdb4b74e96425e86c723c615ffb4775f"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","97dae699e828e4e434bf0b19f571acf5"],["/categories/数据结构和算法/算法题/动态规划/index.html","5d1ad5785b33cdb4a7784904b89b2671"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","ed73c2df5ecb3da72a61cdc20a121ab5"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","87595fdf09f9593927762edc092dc810"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","bbe6d3548e5f95245bce1eb14f5ab87b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","092a100e986d8018290317f03a3831ff"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b564e698adeb56ed75c2617409afb5e8"],["/categories/数据结构和算法/算法题/树论/index.html","300d254e7f8e378cadb1c4ba5a06fbdd"],["/categories/杂七杂八/index.html","a597c3020006e03de757f4ab38902d0d"],["/categories/杂七杂八/博客搭建/index.html","79c9dbf534bb31024e0e790c2248352d"],["/categories/编程工具下载/index.html","eb3ba7603d7b6050048ff682efe9d009"],["/categories/编程环境/index.html","25c0e5dc9711fb1c235a216e4a506bd5"],["/categories/编程环境/大数据/index.html","d4e4fbbfe34c308734a30d454450307c"],["/categories/英语学习/index.html","368142bec7f8ece521028e66859c7264"],["/categories/英语学习/英语语法/index.html","e423504d5df517ed24e28b8c6e7c6577"],["/comments/index.html","9e99394e5a0f9eb50c4fe756a7bf2000"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","eff87e17f157b994016cc8d6a74ad53d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","c9a0751a897e60f40a5c8e42e325a901"],["/movies/index.html","0822f83c8c6137a15a39319084bad07a"],["/music/index.html","f3c7229a844465ec75a1f7856e810111"],["/page/2/index.html","c82275229099ca1eb03a3f94bf556f41"],["/page/3/index.html","978e516c1c65d3deea3d14ecb7e198f7"],["/page/4/index.html","0b0dda7b11d3321f58775386c9020c55"],["/page/5/index.html","5bec7c19ae4f5f0bfdf9def98c63bc0e"],["/page/6/index.html","0a61c8e608aa5fd5ef52de25d8438ccc"],["/posts/1021360842.html","c48290253b8bd641ba59f011c8ab013a"],["/posts/1120620192.html","3be1c98954b0d5533a04d3b3df69b94a"],["/posts/1141628095.html","e29b492d91d2812840bab0dd64d58cfa"],["/posts/1168613674.html","c1109aa05a1c153e04f13b0e96263923"],["/posts/1219920510.html","bd98222727bf148f4c77f26e084c4702"],["/posts/1222166338.html","1f7fbf268e2acf0b506ac510fe5561b5"],["/posts/1259097482.html","3e493ef48544fbe4dc9a393758092ed8"],["/posts/1271036369.html","c6a10519cce289a2172b89b88e9610d7"],["/posts/1312847445.html","f33ba9d03fb6a480880d01953e5b32e3"],["/posts/135355774.html","22cf650f39a7c5ba4842d4354a4b078f"],["/posts/1375344716.html","71abc10a87be63af342708cb554c61d0"],["/posts/1388991698.html","858c8e3da1e9ccc9e6d7c06cf48ebac2"],["/posts/1410315814.html","0378fa59f09b8c1f7d4e3287cac16339"],["/posts/1452790229.html","a20a293ee083f4e5ec258558f3b6b6e4"],["/posts/1470079884.html","dedb63bb6db8cf5c97da31e82f79e129"],["/posts/1470079885.html","afb2264d270bfe41eb4ab41747bf858d"],["/posts/1470079886.html","bf5f763ce44ed8d95fa56d4fe8913fd8"],["/posts/1470079887.html","3ee92e4218fd218f3a3126a57121141a"],["/posts/1498536549.html","fe0b45a597d132e2bf40c858b90be803"],["/posts/1547067935.html","87d5fa2a2c054e2e6676bd134ff09db4"],["/posts/1557866301.html","85546615768f76224ba1a6b274e29dda"],["/posts/1571776361.html","60f2e82f4ec22885e6c40474d74348b0"],["/posts/1605124548.html","ac0fb7b07151f7a3e3ebef24f0625c64"],["/posts/1633036852.html","e10ba6defb0608bf1cfdb8360f13270c"],["/posts/1674202625.html","870624d2063926c574bef7ed4a452c1e"],["/posts/1765123828.html","e8fab24e10156b19eaeb9a1395a6ee29"],["/posts/1767336200.html","6e801ebc3a60993a9f40c1dba8f649e4"],["/posts/1776114197.html","7f1006bf8cd11467bebae02b8c71922b"],["/posts/1817748743.html","f8b2747907b41390fdf409f8717d2970"],["/posts/1925125395.html","e54bc328b514bffc96ab909f2e7c4773"],["/posts/1966191251.html","bf6b05a05e648f1e3f1f74c299a8a082"],["/posts/1987617322.html","22bbe6f6aa9c866fa626ff52333b82fc"],["/posts/1999788039.html","307b79dfad1c7c65fbdeac9743ffb7ea"],["/posts/2075104059.html","3119581ae3420e00156d1a1b47040c8f"],["/posts/2087796737.html","fefba333b3e61594e9b18d5f4a90b626"],["/posts/2106547339.html","b04edcb48a772e363ded978073ac4e0f"],["/posts/2207806286.html","e34cc75525467753efee0f90aec2a912"],["/posts/2225903441.html","2f4ebfba0cadc133bdc45f5c7cb2f226"],["/posts/2265610284.html","e5ec2cd9120df45c2326ff37a1eeabfb"],["/posts/2281352001.html","83af6c2afedcf38070db56e27b3f23f4"],["/posts/2364755265.html","85316f33d04417144cf7768ddb6230f5"],["/posts/2414116852.html","acb164810220f9c112b859bf97364823"],["/posts/2421785022.html","ff41ad885e594e59e82cb9c654c3c2f0"],["/posts/2482902029.html","94771e6e6bd8ded280902fa532f2ec09"],["/posts/2495386210.html","d1d95d25e5cfa9f6726e820a97d611fc"],["/posts/2516528882.html","8ab815b6e21cb13e41054eb5caa40afc"],["/posts/2526659543.html","39efb9a176b5fe0e3e4b8d7dcf0a3e1c"],["/posts/2529807823.html","e220b5da38ae9c50559edfdcf9536ac3"],["/posts/2596601004.html","ec91e0e518749bf871a7fd3278276b6f"],["/posts/2742438348.html","825d66fda4ff6bc2df681da95f6f9d80"],["/posts/2864584994.html","5009022837bd31981b45a3ab59ff5944"],["/posts/2888309600.html","960d0207c746b87a5d98d35078bac01e"],["/posts/2891591958.html","4fb771d2e34d51e8d6e1915799fd5f76"],["/posts/2909934084.html","a58d7fd102f4c9594c247c6eaee8b199"],["/posts/2920256992.html","de16740ac9dcaa5580768eec3c13dd51"],["/posts/2959474469.html","066d1118a1b2b01435808798e2c24044"],["/posts/3005926051.html","7db22e6fc5f8815d639cd87d95e8592b"],["/posts/309775400.html","7100fb3d2f052f39da45d436a36c6156"],["/posts/3156194925.html","0c75d3a78cdf64436fdb44ac70039d5e"],["/posts/3169224211.html","a83ee5bcaab3f2d9f439f6223115d981"],["/posts/3213899550.html","1850544ac598c7842bc2b483b4e840ed"],["/posts/3259212833.html","49d68a38ccae6ba136240563fe220053"],["/posts/3266130344.html","92811da5d300983e060ee58ed98262b8"],["/posts/3292663995.html","47a394824224b314c9aff17d9a9f7d07"],["/posts/3297135020.html","d9490b9008802199549f9ffa76b89292"],["/posts/3306641566.html","7b195988019d3ef8cb8e365745b7e145"],["/posts/3312011324.html","6ae635581237ad9811d57a4c85f1d856"],["/posts/336911618.html","508936cf0bc3b6ae7133b2e217c5c84f"],["/posts/3402121571.html","9939cf6ad7a604965df4df2c839e9a08"],["/posts/3405577485.html","26cca8d54594c9834857549d79a30cb7"],["/posts/3498516849.html","9bf85370432fd25e2a1c3e0dcdd8f668"],["/posts/3513711414.html","94e0a7b981b4d565a10f28132096d414"],["/posts/3546711884.html","497f23c2f7a04f8bb72fc1a06f289418"],["/posts/3731385230.html","3be22dc17c81871e6d6abb0dba5a3db3"],["/posts/3772089482.html","a52f9b68b86cc5a7797f065a61d4a43e"],["/posts/386609427.html","5519232f387f1455b2ccac01a64f9030"],["/posts/4044235327.html","8e7e25e385bced9dffcd6c6fd03abb07"],["/posts/4115971639.html","d263948be1dff5f30600e1cc2a1550d2"],["/posts/4130790367.html","a81ea3f1f1bbc9d59bfb71acd524ff2b"],["/posts/4131986683.html","8fbc539a0d1eb1abac345cb0466d8e4a"],["/posts/4177218757.html","12502d16df645913ae13d49e3ffe39a2"],["/posts/4192183953.html","a5a5682e10a8b937be101cbee700ea0d"],["/posts/4261103898.html","006411a7309ee81734d7fb3da9ab25c9"],["/posts/469711973.html","d58da6336822e4e58db49e44d12ce6e5"],["/posts/482495853.html","0779fbf7d6b8bf57c6b3e687b03f29f5"],["/posts/488247922.html","08dfef3c53f8855b72c86153af218e35"],["/posts/517302816.html","c415e689583343549f1b128474d1cca1"],["/posts/570165348.html","bf1f3081950a99ad43538d81b3970748"],["/posts/595890772.html","d1a3d843c7a7e4f17f7b21730ec8c74f"],["/posts/67485572.html","4fc5170d7c24f569c1f9e5fb8349ca3a"],["/posts/694347442.html","12c561f84cfcd8001a444d4b87e9a8b9"],["/posts/707384687.html","084a48ca5b68c9c628d7ede3f110a4cd"],["/posts/71180092.html","c4b9e703e10187a494a6b11cee2ddbea"],["/posts/716459272.html","3d72d5ff7a594f0be1a7b81e99eaa344"],["/posts/765481613.html","bf952d310dd7755a6900c9ffd67e0185"],["/posts/778231993.html","f30a3859d6fb2c26e737867cfa64d937"],["/posts/795397410.html","6ebe86a3f2fdb8c059a7919d07ed4a9f"],["/posts/820223701.html","2c06e6a92760776f9d9d2e83a606b655"],["/posts/830372185.html","eabe2239a72fb01e2716ee2b355603f1"],["/posts/88294277.html","c27cca1ac6651c206034483af1aebdfc"],["/posts/939963535.html","fd0f6f537da55578bfda9bd2503ebd71"],["/posts/983786067.html","05fe7471a342635e1407ef716cc8d2e0"],["/sw-register.js","94797fb33c334625a292a75bf4f04c74"],["/tags/C/index.html","bc8e16f57dd1edd3ee357d954b696511"],["/tags/C/page/2/index.html","6b308aad5432c2bb7a7e0d2d027566a8"],["/tags/C/page/3/index.html","34fc5d2fcb06c2fc81181147ee165585"],["/tags/ETL/index.html","e9e462a032c9ea21748a6520edf5b650"],["/tags/ElasticSearch/index.html","a164ca27d85f17c41547aa8f91d28754"],["/tags/GUI/index.html","2b1aa5afb7570037721b43fe02c47897"],["/tags/HBase/index.html","32d86fbdc0921725402a05c7e7f4800a"],["/tags/Hadoop/index.html","05ea7baa2cb237e901c84f70d6426fa5"],["/tags/Hadoop/page/2/index.html","6be0b8332bfb8d9dbe41bc24e8c7e287"],["/tags/Java/index.html","356ebf9c556b6f76bb2ad7c01c31e7dc"],["/tags/Java后端/index.html","b01f900d045d8aa4923caeda8ec90d25"],["/tags/Java后端/page/2/index.html","3bf082ee6094cb97f14c1ce601c49d93"],["/tags/Java基础/index.html","3560941b8a540d12426e2540eaae4a7c"],["/tags/Java基础/page/2/index.html","18257845ad77b1d1e579b7eedaebccd4"],["/tags/Kettle/index.html","c2fae7d47a5e81ac8c5a0231a328038b"],["/tags/Kibana/index.html","964372a38bb97318078df8130b439299"],["/tags/Linux/index.html","b4eb403e1e384b7265a71430f330f94c"],["/tags/Linux/page/2/index.html","214cb0ad4d1bb3f9ab7a3e336c53d88a"],["/tags/Linux/page/3/index.html","25d51baa85ecc137f1a1877ff201182c"],["/tags/Mac/index.html","51d4867e4c78db5d1b131ea403036de5"],["/tags/Mac/page/2/index.html","d1b77ea9dc62aad90d12d2cc031b48b6"],["/tags/Maven/index.html","b274d45cef32ff3d86eee06ce95a4033"],["/tags/MySQL/index.html","f06f0e1eed4a2b70510088ad27b19fb7"],["/tags/Python/index.html","7e1c471aa8530da357a7fb00094a1bea"],["/tags/Redis/index.html","3faec10ca73c6f1452b2c01e5de9e108"],["/tags/R语言/index.html","2873256a0863677a12976a9988cc8d5f"],["/tags/Spark/index.html","9ae55cf3beb3768abd9b1be77249ba51"],["/tags/Ubuntu/index.html","152a6a17e1d790cd7df803d0f3afb453"],["/tags/Vue/index.html","e14564e723a5ef48a939e5077e16565a"],["/tags/Windows/index.html","999ff13168aa5d4756db6c9d89d6249b"],["/tags/ZooKeeper/index.html","b7ff59c31a8490c7736dc43bfa1cc4fb"],["/tags/bfs/index.html","7a1bba3580f56c1f48c68c384c6faa1c"],["/tags/dfs/index.html","5bc449c80d925334fd513d0adc84433e"],["/tags/folium/index.html","9c0dcad46dcfbb9e705f8b20154fcf36"],["/tags/git/index.html","fce9e5878c582716af14c172ed187ac7"],["/tags/index.html","303c5543ebe5ffeae4793b007b8fd806"],["/tags/latex/index.html","ce52c6ff0ba5c11b404f44ecec68a4e2"],["/tags/中间件/index.html","f7835f932b28bd710f83b5e490ff91b2"],["/tags/二分查找/index.html","68ba61faf7f93cb14161bcd9724e417f"],["/tags/优化类/index.html","616dd8ebdc80c5b5ac18980e681ebb87"],["/tags/前端/index.html","9dddb44a2ffe69fb096f8cf2c611a75b"],["/tags/前缀和与差分/index.html","61dd502315d23f1379c5b88883cbb2ac"],["/tags/动态规划/index.html","14788b25cfa7403d25963e9c76b7fe3a"],["/tags/动态规划/page/2/index.html","0dd5ec56c4fc0bbac1343749633855d1"],["/tags/博客搭建/index.html","3b3279a762a55e520ba578e6bb77912c"],["/tags/图论/index.html","beba8bec218c8829a8db0ee082672c54"],["/tags/大数据/index.html","26bbebd8f2431f3815f8fd28c75a80aa"],["/tags/大数据/page/2/index.html","9d9bea95907949380535a03aeea9e1b9"],["/tags/操作系统/index.html","3087b33bea04870571d8023f53e87b48"],["/tags/数学建模/index.html","af7b5570ff2374d205169d388030d8e2"],["/tags/数据库/index.html","69133e783bf64843ab303b1ddfb8df91"],["/tags/数据结构和算法/index.html","e864eeb23105adb65a2e983335039b8f"],["/tags/数据结构和算法/page/2/index.html","c32204eae5269e53cedcbb0482dc03c3"],["/tags/数据结构和算法/page/3/index.html","b8b1b4b041dfad80dcb505e5abb4555c"],["/tags/数组和字符串/index.html","9176d6aefad98c05bf42e41f2ae567f1"],["/tags/枚举类/index.html","44c077d37e0ac0a75064f0b198a7c295"],["/tags/栈和队列/index.html","10c7d3cfa6e3c88ad03d0caf366c7006"],["/tags/树论/index.html","92a15d02e56d313f6258883e708e5dde"],["/tags/测试/index.html","4ba6bc2336890442f8d44a2af0b1fa93"],["/tags/环境/index.html","cbdc8a2eace7e43d0971ba9e20d23dc5"],["/tags/环境变量/index.html","a59fc29c01a50f18bb75452526d82cbf"],["/tags/绘图/index.html","14e3cef0b97eb3f26800537ead378078"],["/tags/编程工具/index.html","aa10aa714f981b3f39ea904f9972f982"],["/tags/编程环境/index.html","4e9f1b8660ef11c4fe4fa33782febd63"],["/tags/网络编程/index.html","d03d56271a944041482e763bedce6a16"],["/tags/英语语法/index.html","02313ef20efc2aa4bdcecfb81a989fad"],["/tags/计算机操作系统/index.html","05955294d18df16e6cf5bb359a63db7b"],["/tags/论文/index.html","b747e78e93cca46007dacc6746898fbe"],["/tags/资源下载/index.html","859f8d50000d57a833b9d4e8c733dc7e"],["/tags/链表/index.html","4fad0237bfd1ed421c2a36fa49211371"],["/tags/集合/index.html","1e908b9519300d583b9a7769cb1de3de"],["/tags/集群/index.html","d7649796a0eb4dc4a816c925e4932847"]];
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
