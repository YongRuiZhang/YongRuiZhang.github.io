/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","588b593ba939089eaf1f2a81331dfe22"],["/about/index.html","5d3014f9be39e43393bb0d47bb6dd202"],["/archives/2023/01/index.html","77742f96b0e93a9c2a46326e623b6b0d"],["/archives/2023/02/index.html","a2813b04a1d0bd838db79559e398ae3c"],["/archives/2023/02/page/2/index.html","3e89352f2547c3b567792e14ac89bbfb"],["/archives/2023/02/page/3/index.html","af1a3a5662a15d08ab77791cf413880c"],["/archives/2023/03/index.html","3aa6f40fb89097674e41750b24217c91"],["/archives/2023/05/index.html","29c5615ab176a22c6128aa89945b6678"],["/archives/2023/06/index.html","992dbb9500289c5846e078c5f806561b"],["/archives/2023/09/index.html","c13d6a6676a8a6f705456a1b441eac98"],["/archives/2023/11/index.html","26b908afb310ec348b587e25d18a86ca"],["/archives/2023/12/index.html","387114faf62412f3cce3f40f7ebc23dc"],["/archives/2023/index.html","3bef46d265fc57b87b4eea3b6ba4740b"],["/archives/2023/page/2/index.html","8b00c9ececfc67a2c966319c401a5fc0"],["/archives/2023/page/3/index.html","d1c0d991ebdf7d504b45ccd07caab00f"],["/archives/2023/page/4/index.html","bdb2b323ecbc89def0dd7a633b9a931c"],["/archives/2023/page/5/index.html","8488aa20f5c0189da2613e12743925a8"],["/archives/2024/02/index.html","f94e083c8081d158c3745f4ac090b883"],["/archives/2024/index.html","acbb04a323b03b8a8ca9ee8dba2cdb1b"],["/archives/index.html","6347572186c3da738229be2f1748d561"],["/archives/page/2/index.html","f41af1f0f4e0b733a23633e4601c8b5b"],["/archives/page/3/index.html","472ee467a44733396a015f07300b1420"],["/archives/page/4/index.html","87fad59b34d93f217d8090dcc9455e86"],["/archives/page/5/index.html","e1558da4c59d342ae2203985f12b6cfe"],["/baidu_verify_codeva-qQP2iZOMLX.html","cb5837227cb4a2632d8e52759bc5d966"],["/categories/Java/index.html","51c7e1bde7e01538e60b7ed36959a583"],["/categories/Java/后端/index.html","eb39fd424caa5d4368c8f7762bb1e294"],["/categories/Java/基础/index.html","4ccaf072f683457e6003d330db47e2d7"],["/categories/Java/基础/集合/index.html","5677586d2ef8f739e0c10097168270bc"],["/categories/Python/index.html","aefd7e827e4302e7630f7bbe209c35c5"],["/categories/Python/编程环境/index.html","1e8e79488c6b90c5d4b1e29e08723646"],["/categories/R语言/index.html","5d1e6d930aa0c71321898c957afdb50d"],["/categories/R语言/编程环境/index.html","f3142882c2dd3cce5688656609296512"],["/categories/iPad/index.html","fa9d2294812b5ff91eacdef0931b65bc"],["/categories/index.html","5bc589614ac6b54a35748fa4868e2585"],["/categories/中间件/index.html","0c52798721f939f70183829363b961a0"],["/categories/前端/Vue/index.html","3830723886d5c2a0ebfca7cc3486bbc2"],["/categories/前端/index.html","bd1451bd5bb9f23068d6b149add476d2"],["/categories/大数据开发/ElasticSearch/index.html","61f59d2c63a0dcdb28bbe6938eb1d504"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","ba05363151a34a2cc4a8538fd34281d6"],["/categories/大数据开发/HBase/index.html","fe6a10a9a82bbd5d35c0a1dd15f1455d"],["/categories/大数据开发/HBase/学习笔记/index.html","733bc021e49bdbd05c8d7ac9ec8348e7"],["/categories/大数据开发/HBase/环境搭建/index.html","ea8f6e719f0e5111c1f56336f7ecbd05"],["/categories/大数据开发/Hadoop/index.html","7dd503ca22659277b928c632ac05696a"],["/categories/大数据开发/Hadoop/技术/index.html","9af21e9ebee3a1d28fdc2f7faf1afa77"],["/categories/大数据开发/Hadoop/环境搭建/index.html","7f462adb1d322ba4382d5e00a193c758"],["/categories/大数据开发/Redis/index.html","cfe21e82a57937de5bb8f418c944f22a"],["/categories/大数据开发/Redis/技术/index.html","e16aee4ee205e94a119c9ec1b9b5c9c1"],["/categories/大数据开发/Redis/环境搭建/index.html","ae7165a0deca449224340086acda18ae"],["/categories/大数据开发/Spark/index.html","ac91ba42d6724a0fcf00260a4957f8e5"],["/categories/大数据开发/Spark/环境搭建/index.html","d6d4d0aa3d39e363acbf62c04747c47c"],["/categories/大数据开发/Zookeeper/index.html","947c26a0fba68dd862449b4e1b5a23e8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","5454d1c2eaee0612e4926777e7339cfa"],["/categories/大数据开发/index.html","3012a0c5581b73fcf1f6723b9373efd5"],["/categories/学校课程/index.html","ebddfe3c5b4c655a172b692cc8a43edb"],["/categories/学校课程/计算机操作系统/index.html","c086dcd4fcd402a235bf95e2d0b31897"],["/categories/操作系统/Linux/index.html","08be56f3994d1169f177da1f63ab3b8b"],["/categories/操作系统/Mac/index.html","0cbebd632059305d2fda836abe8c4668"],["/categories/操作系统/Windows/index.html","b556e45802e0e283a21569495de56bd2"],["/categories/操作系统/index.html","485966348343b9cef848b8c2b22e062e"],["/categories/数学建模/index.html","b586cbdd6bf3c969b637f4da16a95c18"],["/categories/数学建模/latex/index.html","3f4072520edb92533d2634afa666581c"],["/categories/数学建模/优化类/index.html","1c505b3f8c5062be4f55c99e54021920"],["/categories/数学建模/优化类/现代优化算法/index.html","c8e72602e3e68adfbb07c23d0586716f"],["/categories/数学建模/优化类/规划类/index.html","f404b19668f1448790bc47d0b69e931b"],["/categories/数学建模/绘图/index.html","6d7e3a4d708268eb2c3be88f98f2ba09"],["/categories/数据库/MySQL/index.html","35f46d340e110862951addbdd726dd61"],["/categories/数据库/index.html","a9851753f344d0d3c378f25b76b2f241"],["/categories/数据结构和算法/index.html","e4c650b6365e73d21312cfcdaebf933a"],["/categories/数据结构和算法/page/2/index.html","3c445e9d8e993f89c65d1ae6cb239214"],["/categories/数据结构和算法/基本原理/bfs/index.html","2bff6eed5817f4d06e059c3f02d71eef"],["/categories/数据结构和算法/基本原理/dfs/index.html","bc3a30964d73cccd79a086354580b5a2"],["/categories/数据结构和算法/基本原理/index.html","9295d117a55f241d531500bf112dabd8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","42891af5118e34a6d2599719f59e6cc0"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","25637c16c748d2c6026e0b176d8dea54"],["/categories/数据结构和算法/基本原理/图论/index.html","7aa5c6145cb9b78ab87f7bb7facfa4c7"],["/categories/数据结构和算法/基本原理/字符串/index.html","5b24052a1c6f40e516b332605403b2c7"],["/categories/数据结构和算法/基本原理/排序/index.html","1e78a2592c984366f4f1f53bdfe6cf8e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","c67e2508311ff1f6b76b732610ba37bf"],["/categories/数据结构和算法/基本原理/数论/index.html","a0ce2dd4108acacef6c04331dd664fb6"],["/categories/数据结构和算法/基本原理/树论/index.html","61dcdfac395700d8b2b6ef05de9ae383"],["/categories/数据结构和算法/基本原理/链表/index.html","f88c5976f7088291596315c90fe74f66"],["/categories/数据结构和算法/算法题/index.html","f5b67df5e05afdc28e1f9ee8bde5a47a"],["/categories/数据结构和算法/算法题/二分查找/index.html","00a30edf2b76471f6b942edde0dfecc8"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","863cfaf3f91327cd76ae7d2ec8db5e90"],["/categories/数据结构和算法/算法题/动态规划/index.html","08e22382a58aa36d509f5f565ba77791"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","850a3a52ca9d7ab8dc2aea299e374051"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5532cbb044d34052e33c2d91e344ebde"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4f187d7a97d141e2bd1e22283e707039"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","22720cb4689e0395e00fd129ef01baa3"],["/categories/数据结构和算法/算法题/数论/index.html","fc03aa6d458402e3fe271e0590c1c079"],["/categories/数据结构和算法/算法题/栈和队列/index.html","13005328a43bf902c593b7133c9a4fa3"],["/categories/数据结构和算法/算法题/树论/index.html","32e65a966e7a8fcb757ce9a28ed3830a"],["/categories/杂七杂八/index.html","cdb2fca7d3ed1cfd699e7e47f9ae0f39"],["/categories/杂七杂八/博客搭建/index.html","c700b19e3dff2bcd274d3bc551ec934e"],["/categories/编程工具下载/index.html","ea64e1f7eb702b4ed3109b056015206d"],["/categories/编程环境/index.html","98544525727aa1b6ccfe9e44841ce1cd"],["/categories/编程环境/大数据/index.html","d1228161f2ad9bbb6757a31068a3568b"],["/categories/英语学习/index.html","25d77401b5fd771bd672ee96b402cdd2"],["/categories/英语学习/英语语法/index.html","9075927a7a13d3d97e9bc722a86dabe8"],["/comments/index.html","f4969db1315cd813c3aa5beddb3d474b"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a773b37c1f2fb5aaa6bfd718c4475665"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","342c5def1d68620d070e97fff6009c4e"],["/movies/index.html","045527a79f0ed540e1d7d657187b4978"],["/music/index.html","cfd0658555f5341f89f49a826e360d40"],["/page/2/index.html","064705d06d5be8e7d8e467d702ca28b9"],["/page/3/index.html","2de2741cba8e46e98e033ba60c17076d"],["/page/4/index.html","0e8ec75dcd94e24eafd1e9c60303aea0"],["/page/5/index.html","f1eb25182c11ea308dd4a1d8b36faba7"],["/page/6/index.html","a4e8832cfdee260efd9103d0e83b79b5"],["/page/7/index.html","e76c877cac98430fbf9b39626a809b63"],["/posts/1021360842.html","7bcec30556b292cb73741e59b91d3cbc"],["/posts/1120620192.html","7da37ddd57e8f40237a199ef3461f35f"],["/posts/1137707673.html","084c4ad9cc1689278dfe52fd47166f1f"],["/posts/1141628095.html","dcaa74a4828b37b6648dce9f9c3ddede"],["/posts/1168613674.html","afac14ea8d401527a7e62adab7822344"],["/posts/1219920510.html","dfb14d098368871a3afb62ee0f0f03c1"],["/posts/1222166338.html","c6d5cc57c95e4b8b01a6bfb00bd316d4"],["/posts/1259097482.html","d678d0d7291bb30bc9da10f707a1a0b4"],["/posts/1271036369.html","e08845831d1ff6d0e4c6414499ab6121"],["/posts/1312847445.html","d3f88d09e20e2d3a4fa5e729b1f0a07b"],["/posts/135355774.html","07291e3bf5495cb970d59c67713f76c1"],["/posts/1375344716.html","a96f9edced4f56d073e046f8d3b777b3"],["/posts/1388991698.html","23df936d3226b71ffa0375f329b74160"],["/posts/1410315814.html","dc84419d221532d7b987119fe9a5836b"],["/posts/1452790229.html","f122d9568584be5b0641e07283e3f0f7"],["/posts/1470079884.html","0cefa2785698127a195e8c7014ba335f"],["/posts/1470079885.html","ec1c4df31b6a0b769b38b42c975f59f0"],["/posts/1470079886.html","bbf227e27f145311e1b1acd363477c30"],["/posts/1470079887.html","e4c764a1785f0670ecd44d756fba486f"],["/posts/1498536549.html","955d828df581e85776464f9846add38e"],["/posts/1539568593.html","c71cb69421e46ee496c993820303a32c"],["/posts/1547067935.html","9387b7e6801d59c62f7b447a546669f5"],["/posts/1557866301.html","31dd7a2c2001061202f2a3fa76432631"],["/posts/1571776361.html","9dd21a1dbedbdfeb4895bc6e07c7f96c"],["/posts/1605124548.html","76cbdffc3c913e32bfb3966bf2586a78"],["/posts/1633036852.html","9dabf0ca48c22a68274bf956d12be94c"],["/posts/1667740714.html","f57a4f0f67f45492747853a3e074b734"],["/posts/1674202625.html","390ba1033071a15efe4a08ed0717ef76"],["/posts/1765123828.html","80b144ea686e1efb4282c24dbf446ba9"],["/posts/1767336200.html","b4ff5bc35b90f6ec6529ad9ef26e3a4b"],["/posts/1776114197.html","a4f9b99b30c85d0da59b96ec60badcfe"],["/posts/1817748743.html","5483c21e296a1f4b7d2ad42dd090c1b8"],["/posts/1925125395.html","c2698a87d805fcf0650c38aa18f3acd1"],["/posts/1966191251.html","642dfeeb899464c91b24dd120483b3e6"],["/posts/1987617322.html","c964db8f3c70762272d2f6e891d980c8"],["/posts/1999788039.html","0be9d7c9269f2b7276acc9c6d5210e59"],["/posts/2075104059.html","5a5642ce5771d0bb809f46278e685438"],["/posts/2087796737.html","bb8b835fae41fbc68454fc7d070efded"],["/posts/2106547339.html","5b7dd7467336071649a022de6c91bca9"],["/posts/2207806286.html","f502cb56b365ecfaca575be9dc38ab37"],["/posts/2225903441.html","92926d2122754ed6c81338bc86a13b97"],["/posts/2265610284.html","865cd78b5e309ade448d7f5eca8136db"],["/posts/2281352001.html","21faa9c1574baeed7bb7b05bc2cf29e0"],["/posts/2364755265.html","1f0adb343dcfcdd67bcada2b2cfbd6ca"],["/posts/2414116852.html","ef46d3556d09f4bd1da005ff6abd7976"],["/posts/2421785022.html","68ae7047143428708057b741e03a75bf"],["/posts/2482902029.html","3e1925d532e0b911af0588c877d29c6f"],["/posts/2495386210.html","a19545668eb06a0194321d80d9a34b00"],["/posts/2516528882.html","98f4ff4fa09c9f03898360664bf1d02b"],["/posts/2522177458.html","eda385b96d7bdcb340a0398896189935"],["/posts/2526659543.html","64015cd8030c777b5472268568fb8566"],["/posts/2529807823.html","f45376333c65cefdfbc523a1c538a58c"],["/posts/2596601004.html","2bc937c59eb598df4d30ffa1f7500d4a"],["/posts/2697614349.html","65fe1cb1ff7752e7537ad76c1148e1e6"],["/posts/2742438348.html","75c031c89594e971ec9f579934430225"],["/posts/2768249503.html","46f5ba004784a4edb3c8f0b48943fa13"],["/posts/2864584994.html","414d2e936a96ae9443252a5347525e78"],["/posts/2888309600.html","a0aedfd62f57c36448147e8d0a244061"],["/posts/2891591958.html","e900f6a23b832522fa3aac86f459168b"],["/posts/2909934084.html","5a1dbeb0119484669d21c42d1356cbb8"],["/posts/2920256992.html","ae601a5436889f379c1ecac207acef5f"],["/posts/2959474469.html","4390b17e436b38c970156a660c458635"],["/posts/3005926051.html","63e00922da07eb54e659279b4acd3447"],["/posts/309775400.html","7f4104749796eba792a1cf06bfdd51f1"],["/posts/3156194925.html","b39188139688cea960fc8fc6b3a80cc3"],["/posts/3169224211.html","d101275afdc7bf0a9a05fd17dc81e727"],["/posts/3213899550.html","c255df283362353a87c9d4763b80e57a"],["/posts/3259212833.html","9fc152e31d438948d76e5b02a0fe40bb"],["/posts/3265658309.html","8dc7e2e52ea10f257d24d59fa0411dac"],["/posts/3266130344.html","31da54e80e9fcf860bb8cf8f039f34d4"],["/posts/3292663995.html","c2c9b2f9b3743586faf9d070394213a7"],["/posts/3297135020.html","52a35934a2b31dcb2818ad49116a3e34"],["/posts/3306641566.html","44f3aedb8e344b903aa70a8906f9fa80"],["/posts/3312011324.html","b99436958f515383e39e2f84b2145991"],["/posts/336911618.html","9757b156b1ef042bba15cd859aa92f55"],["/posts/3402121571.html","5631761acbed29c9c58203791a259c8a"],["/posts/3405577485.html","ae5ab868fd15a0f39d36f787237e777e"],["/posts/3498516849.html","790cb9067b9016ed02b32be18e513255"],["/posts/350679531.html","1c4b1b397f855b655ddf285f6263255e"],["/posts/3513711414.html","ae4bd76f16af7bf913db79fee5cf8093"],["/posts/3523095624.html","2af0655bb0ebe4ca5616c31cfb6093aa"],["/posts/3546711884.html","3465282758ca26c0316ccd2777ecc634"],["/posts/362397694.html","b3e9d7425be9879bd2ff439b868b52bb"],["/posts/3731385230.html","9b2dca11e72728db34b7e1b203fa97de"],["/posts/3772089482.html","822c0eb4b60952aa209073a406e62074"],["/posts/386609427.html","deea4ae28fb9253721630412bd19bfa0"],["/posts/4044235327.html","7d7175250bcf23ca15affaf318765dc7"],["/posts/4115971639.html","5d248ff0d18b4759ffa496cbe0c2361b"],["/posts/4130790367.html","d6cb555d8d7c8f8a170ce28d86631f36"],["/posts/4131986683.html","68d1018c151deb96081ea8febb462929"],["/posts/4177218757.html","9611df239a14c1e9e4d90bd9c42fac90"],["/posts/4192183953.html","1780422bbe22901917d17a118f92c810"],["/posts/4223662913.html","b24abc77de1ed6c13d6d53ba7b5df318"],["/posts/4261103898.html","5165c29d4fe0e18a0740784019c2222f"],["/posts/4286605504.html","1451fc67de17a1e369848450ec4c9d46"],["/posts/449089913.html","986be189ba06273c773dd29f8acbe173"],["/posts/469711973.html","a393306bc1e6b652b9fa2aa62d1547fa"],["/posts/482495853.html","00c57eca17f57f197c45703c0634b565"],["/posts/488247922.html","c15d8af6234cd58014a9bd0ee280cbd5"],["/posts/517302816.html","e6b2825b86e8775a188a72ebfe304690"],["/posts/570165348.html","aafbb8ab2cfa7934f4ac4af5f9f8bde5"],["/posts/595890772.html","9b48da6c91b7c77c09f98459bfd78aa6"],["/posts/67485572.html","efee4248cda9263ca3dba4968f5890a9"],["/posts/694347442.html","c5dd43a35c2b4899e02e4fec29f33e47"],["/posts/707384687.html","e902a1e3413f4b91ccbc5e64de312925"],["/posts/71180092.html","0675429cb74ca15f1e2222d1eef136b3"],["/posts/716459272.html","30f7a065e4d3525171d21898bba3315f"],["/posts/765481613.html","0671a460e47d680b17a52d23823f8ef6"],["/posts/778231993.html","612ad60fb9d4d5cefd420e7e987b0efd"],["/posts/795397410.html","de02d93040fe51b0fde69b8fcc6d6fe4"],["/posts/820223701.html","06516f772a561cff94e77bd7d24b55f8"],["/posts/830372185.html","3dfbac36107a85df8bb73de2b4ba7bf3"],["/posts/88294277.html","041f172335483b54e18f14c6e2a8e160"],["/posts/939963535.html","775f288c949dcb6006dd2997fc5d4f62"],["/posts/983786067.html","c0a3c386452fe750d8238e272655bab7"],["/sw-register.js","29edf9e16c6c94d0bdd439f9c29d5dd8"],["/tags/C/index.html","7286f57ce0ebe5d96639445a6d5fe884"],["/tags/C/page/2/index.html","68d5b9d4222bf2c843234adefeb03766"],["/tags/C/page/3/index.html","ccd08303aab80f9ab79be45ef6857010"],["/tags/C/page/4/index.html","f995127a893402e63a84993ecad74784"],["/tags/ETL/index.html","a9d974520ba561beb94d1dc14d6e624c"],["/tags/ElasticSearch/index.html","c00a585fac8dc5caff97bd8d83e43289"],["/tags/GUI/index.html","74e4353055a302eea4f86204033d8c69"],["/tags/HBase/index.html","02faaa4edf01d7ce2010101f52c20750"],["/tags/Hadoop/index.html","f4e2f750327cf2421884802a2046e0b3"],["/tags/Hadoop/page/2/index.html","8d1bdc808a8580d37ed3dcc0d69926b2"],["/tags/Java/index.html","2c5129aeeb690d8dd7f072f855787eb1"],["/tags/Java后端/index.html","d8e459a12533ea1f300320a64262663d"],["/tags/Java后端/page/2/index.html","3ff074fb51805cc32b8adffb77395ebb"],["/tags/Java基础/index.html","b66b31bdc90cfe542450384eab4378c1"],["/tags/Java基础/page/2/index.html","8084333ca45a0f4e9986cb3c15ed16db"],["/tags/Kettle/index.html","0d3edd4bae12d008e415b8a8ca09a585"],["/tags/Kibana/index.html","536e47778dd1fe7226422c79da56ad9f"],["/tags/Linux/index.html","b0fd3a5796d28e5129ae82855aafcd09"],["/tags/Linux/page/2/index.html","63251ab2a4bc2e7e52d9e627e8036340"],["/tags/Linux/page/3/index.html","606c6c42a06ba603dbbccf564676d5e1"],["/tags/Mac/index.html","29a6331eb5b894944acae4e649d3898f"],["/tags/Mac/page/2/index.html","1d69a35f6e41dbee7cc861e9e9ee090a"],["/tags/Maven/index.html","a25a3a01ad3592d13ed5cf05cabd2f6e"],["/tags/MySQL/index.html","c1b8fc5f7b19af77bfa42bcab2efc50d"],["/tags/Python/index.html","8c7a6727d168f0cc5128dbe41bca36cf"],["/tags/Redis/index.html","b5502957606bc2c4124d9d3447fa7238"],["/tags/R语言/index.html","168952f65f88128542a54ae87f64d39a"],["/tags/Spark/index.html","33d1863591ac5bbc96b5996b18948c64"],["/tags/Ubuntu/index.html","2349727cfe099a17c04a254a89e1ab68"],["/tags/Vue/index.html","a4d7341fe31b64080c5a9c68159eba86"],["/tags/Windows/index.html","c94fa6580156b7f6bbf895e57ba9c3f5"],["/tags/ZooKeeper/index.html","b5916d5a0f2c16cc363da4a90ccbdcbd"],["/tags/bfs/index.html","5b3c287eab236fc6d85572f1774a4d47"],["/tags/dfs/index.html","185b0f760e7488f9100a71ea2fb5da63"],["/tags/folium/index.html","d36e4c59b186c30111b6531e1cbe8ae7"],["/tags/git/index.html","6efa8450d70479c9e6dbc662ec92bf21"],["/tags/iPad找电子书/index.html","d93b371fd3387761c8bf91dba380bd98"],["/tags/index.html","aa0cdaf7083cc5b0afaced174efd0646"],["/tags/latex/index.html","ea6d33ef24130fc411552a8cf6ac0cf7"],["/tags/中间件/index.html","42cef7b0796b4bc586489bc719cbdb2b"],["/tags/二分查找/index.html","3dacd582850ce2e4e64f844747001930"],["/tags/优化类/index.html","b974374d25619a9f4da7ffbdf41e18ae"],["/tags/前端/index.html","528c1a46a3155ed108bbbb22b19634fb"],["/tags/前缀和与差分/index.html","f04fdf8b21c689d46ac295f208efe442"],["/tags/动态规划/index.html","026982fdffc4cc72cfd81d35aaf6ee04"],["/tags/动态规划/page/2/index.html","e3ad9d97473642109cd982522b259d7e"],["/tags/博客搭建/index.html","87be79d42835ffbba23f82f63ef806c0"],["/tags/图论/index.html","e5be08adfdfe59c05d58ab1756932a3a"],["/tags/大数据/index.html","3a8654db667ba78b4d27f46d127b97d5"],["/tags/大数据/page/2/index.html","c891637d4cb0ad14f34b95d5d6995d56"],["/tags/排序/index.html","a679b3b00dcf678a00d9634fdc6cdb61"],["/tags/操作系统/index.html","481ce96a8c92297e69b8e7ca011a01ca"],["/tags/数学建模/index.html","21ee673a2cec3535b620f2b0eb7e88a7"],["/tags/数据库/index.html","10446723ab2643fd94b9d9ef86364ada"],["/tags/数据结构和算法/index.html","e461bcf2a87fa839376155b595ecd311"],["/tags/数据结构和算法/page/2/index.html","393adbd0e6ae51f70debc204e3657e3c"],["/tags/数据结构和算法/page/3/index.html","933962ee6717150ab8210b673457299f"],["/tags/数据结构和算法/page/4/index.html","a6d900efc4dae24f8f44cd1549805d91"],["/tags/数据结构和算法/page/5/index.html","fca95b9e74373c605a5435cb03df8478"],["/tags/数组和字符串/index.html","567ee3bae123fbf549bf59626f03b948"],["/tags/数论/index.html","4827cce3431259144fbda41bd0aebd21"],["/tags/枚举类/index.html","1f19a8e29cea1aa4de5cb63b2e5ceaa7"],["/tags/栈和队列/index.html","41f53d70e6fc944f145ff1235488f444"],["/tags/树论/index.html","fbe3c280208f38051a4023445661d24d"],["/tags/测试/index.html","2f5980241cd76d54e00d402d0a356e27"],["/tags/环境/index.html","1ce9a185ed4136506216ca5ad38bd1b8"],["/tags/环境变量/index.html","aa7f9adaaf91ad020e0a26befa6a41f7"],["/tags/绘图/index.html","42b727ba371c750aa8572891a7b61419"],["/tags/编程工具/index.html","ee35967c75a8b70ba52b9202f2e0f666"],["/tags/编程环境/index.html","451888d1b8380b46042a2ecb7440acf6"],["/tags/网络编程/index.html","bf8e2e16169cb1791ea4676ea4bbe468"],["/tags/英语语法/index.html","a22e7f2448ba9b926dc5908ed9fa4a74"],["/tags/计算机操作系统/index.html","65047f7c63bda08190dd5e0bd8f8b634"],["/tags/论文/index.html","7f882c5f846107935693d2f2fca67ed3"],["/tags/资源下载/index.html","9251702945598d74b7bcd03f8122d229"],["/tags/链表/index.html","c45f577b6eb0fba071a2b9fb336d640d"],["/tags/集合/index.html","0aec340cfd491c143a95e2862e093cc1"],["/tags/集群/index.html","a2dc826bae590a4bfd7cf08841aabce1"]];
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
