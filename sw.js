/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","90a241b3bec12a88cbea10c9022a5d05"],["/about/index.html","1ec1499baca14e6993e2eb004dae0886"],["/archives/2023/01/index.html","d402989b2fa5c7af98001d2463243eac"],["/archives/2023/02/index.html","85bfac8b12bbca56cd8817884b537b11"],["/archives/2023/02/page/2/index.html","bd3324ee189a1f9cc2cf429877558770"],["/archives/2023/03/index.html","bc439308f0672e54a078aa56d202d8cd"],["/archives/2023/05/index.html","9980bb767f98305c55cfbfee4580d290"],["/archives/2023/06/index.html","4889755700ed0b0bae00bef5079db249"],["/archives/2023/index.html","d1595bd07f707bcdafa6560111d88533"],["/archives/2023/page/2/index.html","14d8847b07882f0a49ab8308ea4b985d"],["/archives/2023/page/3/index.html","cd9cc8aed0989a2f153d8c14385c5dc6"],["/archives/2023/page/4/index.html","cbd0bc8f47174b5ed7c6b17f2a77e077"],["/archives/index.html","fc009969211b401b671a2b64ad301fc5"],["/archives/page/2/index.html","3b0028c96a1fc1161c71b6efabd74a83"],["/archives/page/3/index.html","1238164f40c9d00136dbfb4ce07138ed"],["/archives/page/4/index.html","509f1a0d8f70e53a3a49c389757af908"],["/categories/Java/index.html","00710445ef227b6c12584c8d094675fe"],["/categories/Java/后端/index.html","20b52de1f90cf201b29e95c56bfae748"],["/categories/Java/基础/index.html","b34806d5027ce94008ac947a506bb192"],["/categories/Java/基础/集合/index.html","67f12270a3685c49acf0b9e23339ea13"],["/categories/Python/index.html","0c46b217317e2e612175c1000900d3f0"],["/categories/Python/编程环境/index.html","83b58cffc97df713516de65aad3ee850"],["/categories/R语言/index.html","62028f4376e6a68a4fbfc865e462208f"],["/categories/R语言/编程环境/index.html","27536bf90df5c2aefdf2482a5b05a69f"],["/categories/index.html","dd700099c3cc2570f6bd33fdbf69a902"],["/categories/中间件/index.html","809e363cd4e8e6f81d7c20e6035e172d"],["/categories/前端/Vue/index.html","2b8630357400675793d8cc693def5240"],["/categories/前端/index.html","e639a1d55ed7c6f2131927695ea4ea4e"],["/categories/大数据开发/ElasticSearch/index.html","48faa4a0714bb624a7f3aadbfc0dca81"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","0870b97f0ee9eb0897d562f980ea74b7"],["/categories/大数据开发/HBase/index.html","72ed1d230378e8eb67ec4736a1eedf92"],["/categories/大数据开发/HBase/学习笔记/index.html","aad9d87cb245bc42f448fcd2a3498f8e"],["/categories/大数据开发/HBase/环境搭建/index.html","ca72495373e49869ef10a8f4be2808dc"],["/categories/大数据开发/Hadoop/index.html","f2cb053f47cc38c2347dc72a120440bf"],["/categories/大数据开发/Hadoop/技术/index.html","902316c38b20520be7c36da6248433d6"],["/categories/大数据开发/Hadoop/环境搭建/index.html","fd53ce76ad8d51e742e2eeaa55291cc6"],["/categories/大数据开发/Redis/index.html","7d4b0c6fd2eccdc32cfc3a89deb38509"],["/categories/大数据开发/Redis/技术/index.html","a369676819dc6e64f444185c62333567"],["/categories/大数据开发/Redis/环境搭建/index.html","b1525e92b9fcf663083dd5ea0a3d0415"],["/categories/大数据开发/Zookeeper/index.html","e3d21e886725472cf9152ff4aeb3f79c"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e4f053b503103e4e8c7e41bb9e1f13e1"],["/categories/大数据开发/index.html","9923503b2974aaa3b26ce72a8af9f555"],["/categories/操作系统/Linux/index.html","f732ee59eda7fdd43c9dccc8443cd701"],["/categories/操作系统/Mac/index.html","8028413b6e111532abdad62eb17de27f"],["/categories/操作系统/Windows/index.html","3d8ada2ca1882025c5a6f974ea98dfe2"],["/categories/操作系统/index.html","520a473708bf4e208bcfa55e83799b5c"],["/categories/数学建模/index.html","3f45dd37ef40c14f085ce92ae267011c"],["/categories/数学建模/latex/index.html","a63b697f55876a73d2c411d5fb902885"],["/categories/数学建模/优化类/index.html","cdebab901fffd985cbd9d10edfc61ebd"],["/categories/数学建模/优化类/现代优化算法/index.html","f025a1edc0a42f005a7790822b7de910"],["/categories/数学建模/优化类/规划类/index.html","eece94e7351258c2e62d7b2557ea8cb3"],["/categories/数学建模/绘图/index.html","a21d3dbb71f7aeb5ce0895a65b5b6453"],["/categories/数据库/MySQL/index.html","0f821bfc2e8bc0c4b22265ad4e55c692"],["/categories/数据库/index.html","e7246d84981a9d42fbd2c4250009325d"],["/categories/数据结构和算法/index.html","f77f97787a28af0850a3ffdb99643173"],["/categories/数据结构和算法/page/2/index.html","87d048ed685abb6055529524b60c8877"],["/categories/数据结构和算法/基本原理/bfs/index.html","45d1221897484937530eff9a3a38c534"],["/categories/数据结构和算法/基本原理/dfs/index.html","cdf51d04498c9850bab17f2feee2992f"],["/categories/数据结构和算法/基本原理/index.html","a91bf98eff716648ef862428ff0ca557"],["/categories/数据结构和算法/基本原理/动态规划/index.html","0c75d10393ae7356b1eaf04db4fcda56"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","1938a87a97023fe59b3f8f753891d8ab"],["/categories/数据结构和算法/基本原理/图论/index.html","c2f6df4ca39fb32a68d74c87031154ed"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b77d2617949fe9e3bec9e4c61d77ecbe"],["/categories/数据结构和算法/基本原理/数论/index.html","26507baeb01b2c9ba589ff81b814a893"],["/categories/数据结构和算法/基本原理/树论/index.html","71b7f9a09664bd2c8a2bd3bdd3e3e6dd"],["/categories/数据结构和算法/基本原理/链表/index.html","549fe4407c94e76d3a7601cddd1f468a"],["/categories/数据结构和算法/算法题/index.html","f70dc7381d5515726f801b140db7545c"],["/categories/数据结构和算法/算法题/二分查找/index.html","10fde420089a47ecf66b8b705dd84445"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","9ec9671e2a86e3c58c4629adcf46e9d8"],["/categories/数据结构和算法/算法题/动态规划/index.html","1cdc90a562c06e399d9c2f9747cc7f39"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a07340d8fba104e855d8f27ba19cd6ac"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","87739d7cb05ac8feb37767073f496790"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","507b0d0682832835a4b02ced015f0878"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","fdffb105307341a087d434e1eaabf527"],["/categories/数据结构和算法/算法题/栈和队列/index.html","17434f2dd60d79b6b3f08e2a281cad97"],["/categories/数据结构和算法/算法题/树论/index.html","6de4971e42676763f7b567a074878cab"],["/categories/杂七杂八/index.html","ff924512ecc6e6fb77d2a67a13340ac7"],["/categories/杂七杂八/博客搭建/index.html","b37a6f663f521e5af63094251689eb75"],["/categories/编程工具下载/index.html","3b0a9967183da5fefa548353b45f7123"],["/categories/编程环境/index.html","396d194bb7aebfac0ba0ea9ef7916640"],["/categories/英语学习/index.html","987b44a70e22afafbe26d8a5e4b6a325"],["/categories/英语学习/英语语法/index.html","922a2cfe06d2e52c5a747d585d1ba007"],["/comments/index.html","23f49768036db4237d22a7f960776972"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","993e5bfa1e8452c9bc7efed69ff960c7"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","77fa1ea722d3e62ceac2c7a7e790d7c7"],["/movies/index.html","b9ddd6f211fb58e30ea11e1db410a32d"],["/music/index.html","7503e5eeb9baef7aef2756b02ae9258f"],["/page/2/index.html","c314aa772d33135e3815868e42dcee34"],["/page/3/index.html","c3f6e1b8ab2290a1d765ca756b18131d"],["/page/4/index.html","fc2b805374c70da87231c57a16fd33ea"],["/page/5/index.html","c6138b52f552625795b58ee8af3c0b83"],["/page/6/index.html","0fbc23c55f96291f8a4962a52ba21c91"],["/posts/1021360842.html","acbb8d1a3d0f3e2f4b4fc1cde6fd745e"],["/posts/1120620192.html","a7912ed411c4394245e2533312e9bd16"],["/posts/1141628095.html","9362cd2330ebf53c566e2f6a9d766dfd"],["/posts/1168613674.html","e9defed18d0258001eb5040618961dc8"],["/posts/1219920510.html","57c91fe30c5b82f20f5c10298e32facf"],["/posts/1222166338.html","ccb85aefe6432006223a7a6969d599e9"],["/posts/1259097482.html","4228cd1102897525e2a0fc58404f1206"],["/posts/1271036369.html","0b6673e41ab316bcac561187eb89fdc1"],["/posts/1312847445.html","58d309da0784569493316a2c9f1e1702"],["/posts/135355774.html","3636bf954bc46bc93d34b4dc138d908e"],["/posts/1375344716.html","1c14b646c1717f656f6b6b4f734ebf02"],["/posts/1388991698.html","3302818ef725d256c894e6251fdb143d"],["/posts/1410315814.html","ec40e7b76d6db2f77df0c7d3e9580d8e"],["/posts/1452790229.html","0e9178f2b201c903715bbc7bea98807a"],["/posts/1470079884.html","b04ecc9a46a9e727428fde8d88ac1012"],["/posts/1470079885.html","e74b2ae99cd4bc7ae461577e8c681831"],["/posts/1470079886.html","6aab0666dd19ea8065c9088a1f4c6b3d"],["/posts/1470079887.html","b99d9cb43f43a7b9410d851ec428abb0"],["/posts/1498536549.html","dea2e55737fa3a17a8514c6a9d53802a"],["/posts/1547067935.html","a1b910f1365eff2affff629936862aa0"],["/posts/1557866301.html","331b61ae8f4e7e72c8a90c97471c6e4e"],["/posts/1571776361.html","859848be2c57a7d80098d008c9dce767"],["/posts/1605124548.html","5537893fec143d1e54f35ee45a337a8a"],["/posts/1633036852.html","e99803959afd68ddde7a84780af29f5e"],["/posts/1765123828.html","83e24e6bb0a0180498571c60a64e8610"],["/posts/1767336200.html","d7809d2f17997ac9c7595a6085ca4f76"],["/posts/1776114197.html","861b64495a290979eb6f12c1734338fe"],["/posts/1817748743.html","e9941aa61c125319c1d96d468f9f9c5a"],["/posts/1925125395.html","d34edf84fbb524bdc994cc6116ebb122"],["/posts/1966191251.html","4be4963b54f8d35175ed784ed67f2571"],["/posts/1987617322.html","50327aed290e51db20eea1e8ebdc530f"],["/posts/1999788039.html","316f90ff4be3c6b36862fb064bb87f8a"],["/posts/2075104059.html","5737e7c8c4a2083391c0fb65fbd59d5b"],["/posts/2087796737.html","4d7f5f281c71c580c448e810d20bdb1f"],["/posts/2106547339.html","c66749f92dea4deae56537f100272d3a"],["/posts/2207806286.html","05530c778187e1581aa65d7aff2d3d7a"],["/posts/2225903441.html","95489c5552b614f290322ee3fa3dccbc"],["/posts/2265610284.html","b2a5e20e0ba19251d0136ede975b5231"],["/posts/2281352001.html","292915da7990bab222fd0481fc0b57d9"],["/posts/2364755265.html","53e18d31d7c10deb869299c6f9b6edad"],["/posts/2414116852.html","413eb5c48cde20c9cd698761667258fe"],["/posts/2421785022.html","607b0bb1b0584834cfd32c7aa410aca6"],["/posts/2482902029.html","b9c6b28ea1eca0976b49ef05aaa54cb0"],["/posts/2495386210.html","cee9dd35e4ccdc6c4f2a31a54404354c"],["/posts/2516528882.html","a0857bb4bb887b5cddf255a4491d2055"],["/posts/2526659543.html","37149d0ff5ac1add82c3d25ac0d080d0"],["/posts/2529807823.html","5b3142e440751ddb557b4e18ad0b1757"],["/posts/2596601004.html","e71dfa0b3af1526f85901f98f505ea35"],["/posts/2742438348.html","a8bdb86beb9cc1dc50e773d6a9e63cd7"],["/posts/2888309600.html","c29bd50111adebdfc1b37450e2fa562c"],["/posts/2891591958.html","41b1e735146fcc6c40423b26edad0600"],["/posts/2909934084.html","8f4f61bd756bd1af0d04a44464b88f76"],["/posts/2920256992.html","9195d4c144bd85d7b43022611f2161f8"],["/posts/3005926051.html","6b3f55e959411541b39c347dbe5ad1c2"],["/posts/309775400.html","5f9597c87d2971a6b01e5d689bb30367"],["/posts/3156194925.html","2b2b96a46b44faf5435565d2521bc5bb"],["/posts/3169224211.html","39c824599856eaa14ac54271d0a19161"],["/posts/3213899550.html","97e5638a7739eca2917b1d4ddfd7e202"],["/posts/3259212833.html","041acda84bb21a51caba78c4af02477c"],["/posts/3266130344.html","66a1ea8fa5e952a006fba31065248bf2"],["/posts/3292663995.html","171046c8303b526a3280a022b03f3294"],["/posts/3297135020.html","5642508a74c2474023654cfcaca86b99"],["/posts/3306641566.html","fac97abe9b1d802e28f474989e84ecf7"],["/posts/3312011324.html","180999d122db2ce4dd834afda05ed048"],["/posts/336911618.html","f2df812ca7ac53120885f649c173eafc"],["/posts/3402121571.html","ebe0d4f3384858cfc3e52cf11cb4258c"],["/posts/3405577485.html","54eedcda16dedae28bc7a6a328458ab3"],["/posts/3498516849.html","f6c7958e4d64643a246bfdf043d37333"],["/posts/3513711414.html","e53a3b0f1dc86ea13160ff1900b253c6"],["/posts/3546711884.html","de4d228b431af9adb13a9ebc551cc810"],["/posts/3731385230.html","39e4858c9dadce276f731ff30af52288"],["/posts/3772089482.html","0fba2d4913a5a594c3764d8b5eab740c"],["/posts/386609427.html","26a30496770d4100dbda475ca3a6ab5e"],["/posts/4044235327.html","dfc0bb382656be21ea5445d7eeb73665"],["/posts/4115971639.html","57409b9896d3da0d38a8cd4e04e11895"],["/posts/4130790367.html","be69a890cba5f2c2907ee36aef068c65"],["/posts/4131986683.html","c52a0593336ff5eed40f011ffff03486"],["/posts/4177218757.html","3d7d85de23abef9335a13b170c707c27"],["/posts/4192183953.html","007d7d3182b53aeb90e8deeaa781e021"],["/posts/4261103898.html","39035df2dc7260066a26282723dbc6ca"],["/posts/469711973.html","db376bf3b917ac6407801baa75a8c1c5"],["/posts/482495853.html","ad5428246d6cc6a81562c3150fb48540"],["/posts/488247922.html","cab0520c546492ca5446feb35035ec1d"],["/posts/570165348.html","cf3063d1429f8f9d0f557ddd9c84b38b"],["/posts/595890772.html","c7c05f01fe79f831a64ac6295dc68bf9"],["/posts/67485572.html","7511c7ead570c9d42d8d97a376c2a268"],["/posts/694347442.html","89f93761b7d5be9e5a8a7b4f4b2544c1"],["/posts/707384687.html","01024b658a969ea2029463bc83bded18"],["/posts/71180092.html","ebdfd76568476672daedd02247200473"],["/posts/716459272.html","6e711b223f3ee643cc621aea117cb912"],["/posts/778231993.html","2baf11a3e90b86ed78d5bcb25871e9f1"],["/posts/795397410.html","8ae0049663476e81d5a093e0367ad1c8"],["/posts/820223701.html","28ed9a722c64ffd5983ca624230191f2"],["/posts/830372185.html","119176128be591d8449e6c2635241d67"],["/posts/88294277.html","0154f126761105dfdb594fc5fb7d0b26"],["/posts/939963535.html","e1f02a23c071a301b915a9849e84c289"],["/posts/983786067.html","dcf43fac17ab013eea3341d2d63c36d0"],["/sw-register.js","850985a9548507ada2d4dda1e3bf2391"],["/tags/C/index.html","75f252ce612387391dafa1d7589b7b67"],["/tags/C/page/2/index.html","8dc907f547d6f75e897579a81a35e71b"],["/tags/C/page/3/index.html","853ff315fe9f9270c8229b96db29102d"],["/tags/ElasticSearch/index.html","6100fa7f183c680ce8faea35ac84d5e3"],["/tags/GUI/index.html","daaedfc1a39d31a5e5c46adce413e36c"],["/tags/HBase/index.html","3a20957a03881206bccb2e7fb3b9915d"],["/tags/Hadoop/index.html","cf1e7fbbd5264f6aa8316e4aadc94db3"],["/tags/Hadoop/page/2/index.html","9a0ddcf8b0bd588c820639652b41bc4b"],["/tags/Java/index.html","8c24fe0cb19fff75febb788f27dfbb3c"],["/tags/Java后端/index.html","4906c77e63117e126236d6f154d618c6"],["/tags/Java后端/page/2/index.html","bade08414189dcf34e97a5bf6c464205"],["/tags/Java基础/index.html","57102f939d78834fcb0d8dcbd8a48986"],["/tags/Java基础/page/2/index.html","408142aeec5f56d4304a5a08d9244a93"],["/tags/Kibana/index.html","ccd036d45022637d4bd00be62d14f91d"],["/tags/Linux/index.html","b42eb96b5f7a9f0c2a97de225245c069"],["/tags/Linux/page/2/index.html","aeeb005cf9a1abb292f7ec19691f2412"],["/tags/Linux/page/3/index.html","b70b43e63424977e7415e19b3ac2b246"],["/tags/Mac/index.html","b9f23683a3b2ccdae47c61591fb6e7e0"],["/tags/Mac/page/2/index.html","fc83afe39d22bfee4409761efe50dc4e"],["/tags/Maven/index.html","d80c6986c450c993f323e88dffd8aa13"],["/tags/MySQL/index.html","7ff7452d8b736058eb47fa846a5085db"],["/tags/Python/index.html","27a31aaee09fe0815574e58412bf6a17"],["/tags/Redis/index.html","76df87bdca2c5c18a70123c042d622b4"],["/tags/R语言/index.html","a422d483d2a47def2e0a1149d3d4628a"],["/tags/Ubuntu/index.html","a6fde25d2015ae230d43baa1d1b4eed5"],["/tags/Vue/index.html","94c94adf3207de016524064a1b4d2a8e"],["/tags/Windows/index.html","f1500c395400cee45e2aff04d40765f8"],["/tags/ZooKeeper/index.html","a1046c0f065df73e9400e2f7c4bfc95a"],["/tags/bfs/index.html","67288926723fb9521ccbcd8605c515fe"],["/tags/dfs/index.html","ac123c6b8909dc8b1ee83399975f1ad0"],["/tags/folium/index.html","7b542b9cba3b2850f2f9940daffbc714"],["/tags/git/index.html","33340a592bbecde1687ae7edadf65503"],["/tags/index.html","bed999bc027944eb966aad3320cf03d9"],["/tags/latex/index.html","ee6fb0f1b6148dc48a261658bf798e06"],["/tags/中间件/index.html","bddab404f0e9e919ce699647230920e7"],["/tags/二分查找/index.html","a0cdf1b6b90cac87eee0c03d2dafb24e"],["/tags/优化类/index.html","1fb8fbded6d28bb1aeab4d52c035745f"],["/tags/前端/index.html","19bf7046faf32149cfc660c2a544df88"],["/tags/前缀和与差分/index.html","49f5b46287d304f77851936e132dab35"],["/tags/动态规划/index.html","208a7ea067d5cbf2cd9072377023808d"],["/tags/动态规划/page/2/index.html","06139b5099c1184a10edf5dd117a933f"],["/tags/博客搭建/index.html","cd2f151872ddd4488bdb2ac2c5156cc3"],["/tags/图论/index.html","249db0beb06265c38bdf7cdc7eac2415"],["/tags/大数据/index.html","dfa2bf4cf768641e2bfe719b9c4bc17e"],["/tags/大数据/page/2/index.html","b8353d7d31f27bc748e69a694692c61c"],["/tags/操作系统/index.html","9e2e5bcf3ae2726a759b531449ced2df"],["/tags/数学建模/index.html","0d95a91ddc41b4f2938ebec228c0ad38"],["/tags/数据库/index.html","ac7ea14b42bd2eed9290ca4f05f6985c"],["/tags/数据结构和算法/index.html","28317e5bfb0426cb9970c0670fe3b09b"],["/tags/数据结构和算法/page/2/index.html","e301c1cc11cbd322fc89c8adfeaa5f49"],["/tags/数据结构和算法/page/3/index.html","1317d5d1e89d0cb97989d3ca34d97a25"],["/tags/数组和字符串/index.html","6378b1aedb269910e3cea9c98655da62"],["/tags/枚举类/index.html","210b6dcc83030d70a358e4f45a3c7d76"],["/tags/栈和队列/index.html","bd5e1203e7c6de128e2b853a9f0d8b78"],["/tags/树论/index.html","43e6991c554987411ee99b1ddb5b51b7"],["/tags/测试/index.html","a74be7aad525db1f1a83b7e0302b047c"],["/tags/环境/index.html","892cef38b4ac0021e6826c5b8e4c0036"],["/tags/环境变量/index.html","1572542feeb7c8f0ca4cb1af8530546b"],["/tags/绘图/index.html","511c57c9126d66d92659245ff5ec64de"],["/tags/编程工具/index.html","a317c24071d0003603c06f804e84ebd6"],["/tags/编程环境/index.html","473b756e36d8faeb03cc553ec83f34b9"],["/tags/网络编程/index.html","b184cb0b736cceccaadaec425021ee17"],["/tags/英语语法/index.html","11d5914e98c6b624ca11ca87a7a2d5e0"],["/tags/论文/index.html","b36bc9eb034f431c104ae72cbdfcc43f"],["/tags/资源下载/index.html","f6ed36e9a394af6fcbdd5f4e4fdd4c2c"],["/tags/链表/index.html","4974e7020581151dc6565bcff0261f7e"],["/tags/集合/index.html","776881b5db7bdea8f088d0eddd2b19f7"],["/tags/集群/index.html","848dfa6ec13b14c5f5f4d3b458906daa"]];
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
