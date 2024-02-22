/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ff7e02517cad3e81cba3f7d89efbc7fd"],["/about/index.html","b94dd6d6364c618d1baa9c41c5e7dc00"],["/archives/2023/01/index.html","65927755f508ed9304e411770f1f1636"],["/archives/2023/02/index.html","f01a5e5d08f9f48fa5d71ef7c47b6d93"],["/archives/2023/02/page/2/index.html","ded394fd2ce1df3855204b7020d4eaa5"],["/archives/2023/03/index.html","126bfe0100d1559eb1bab95bc417bae8"],["/archives/2023/05/index.html","f674d6c2b6e2425769aef0e2527d0831"],["/archives/2023/06/index.html","10013c36b4875e29ba6b05439aec4e15"],["/archives/2023/09/index.html","cddef5f670e699bcf061e539904a4aab"],["/archives/2023/11/index.html","346fd9f9fd9870974e703e6613780558"],["/archives/2023/12/index.html","1f6af62d6890e16b53fd126fdcb2dd58"],["/archives/2023/index.html","320fdd9743cce3c6251435b59010c291"],["/archives/2023/page/2/index.html","9ca25aecb6e3f264b1a93c2999ff2bee"],["/archives/2023/page/3/index.html","dcc087610cbc0c24c55bf9b8c43b4563"],["/archives/2023/page/4/index.html","3a14f03a35a98cdc8178575912fe628c"],["/archives/2024/02/index.html","21f975ebbd5f6510b7da96fed8ccfa7d"],["/archives/2024/index.html","23c4baa5d4b2e7891185f9cca3acd9f1"],["/archives/index.html","f09a0724220391a5fd298a8f8c5dca98"],["/archives/page/2/index.html","21e26ff93e08b41f6687c726269efdac"],["/archives/page/3/index.html","0c06c9ab0090ea8c3319c6bdf8e44e9e"],["/archives/page/4/index.html","183bdb185f8ed5dbcb46b68af4f88003"],["/baidu_verify_codeva-qQP2iZOMLX.html","6bd532645ed678ab54c928ce9ab5ed25"],["/categories/Java/index.html","6734a98ff1e1f50d67c211877ef4a3be"],["/categories/Java/后端/index.html","2cc148135d64528ceb124476846db596"],["/categories/Java/基础/index.html","6aa7b5949a346d3725051705723dcc2a"],["/categories/Java/基础/集合/index.html","62d491cef10ea342899a0afb5418a651"],["/categories/Python/index.html","43fac84bee3f8e6eef64d620d216646d"],["/categories/Python/编程环境/index.html","405d3768adddaac8229c1d8bf8608e37"],["/categories/R语言/index.html","c83c2392b3c7ef6c2a47dd80029dbb7f"],["/categories/R语言/编程环境/index.html","39832e0612f2ab031668550ff726399d"],["/categories/index.html","17c65db694bde039d48404d0cd684b12"],["/categories/中间件/index.html","09f2bd470787c4aa790af1731f1b1462"],["/categories/前端/Vue/index.html","12dcda934ded1094f551ecca4aae8e16"],["/categories/前端/index.html","eb845d2c08d29e7ce28580b9d7f5e29f"],["/categories/大数据开发/ElasticSearch/index.html","120bda11fb2d56c17bec98306ad8e5ea"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","ff1c98c323ed462ed2d3c21f8695c24b"],["/categories/大数据开发/HBase/index.html","56449d17f5c06803e2ddbae0058fcc0f"],["/categories/大数据开发/HBase/学习笔记/index.html","2e496f61f6dd6f4f2c0f406b0d7f990e"],["/categories/大数据开发/HBase/环境搭建/index.html","50d886c088c90aaee5d1b90fd234c9b6"],["/categories/大数据开发/Hadoop/index.html","f4c88a672a0acc4c6234ecbcd35eac10"],["/categories/大数据开发/Hadoop/技术/index.html","956be9b18c3c3b9622a8dcf0aca90a1f"],["/categories/大数据开发/Hadoop/环境搭建/index.html","7d590c560b0dd4b954f59efe8d0e2866"],["/categories/大数据开发/Redis/index.html","ef15a5752d11e5e9fdf7cad417673f60"],["/categories/大数据开发/Redis/技术/index.html","fff0a4843accf147c847ffc53dc703ad"],["/categories/大数据开发/Redis/环境搭建/index.html","986db99241868690472ff35ea7470504"],["/categories/大数据开发/Spark/index.html","cb447030153bd8018703e94204cd0f64"],["/categories/大数据开发/Spark/环境搭建/index.html","c6f27ec055cbbe44581db2ba11ffdafe"],["/categories/大数据开发/Zookeeper/index.html","4d7524b2f11dd14dda5bd50304f49a54"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8a547edca79f45fab8a98281fe60cdd3"],["/categories/大数据开发/index.html","fb81f45aa2ea93d8168095b612f26eb8"],["/categories/学校课程/index.html","cbdccefd808f080145d912e240138a4a"],["/categories/学校课程/计算机操作系统/index.html","32c775ed373b71c9384f074f504e650f"],["/categories/操作系统/Linux/index.html","bd0b64fbdf793e8222ec5ed526234171"],["/categories/操作系统/Mac/index.html","d64de58ddb394cea8d372c9eda179928"],["/categories/操作系统/Windows/index.html","8d2bbc3841ec2f25cc7aa8227ce1f289"],["/categories/操作系统/index.html","346c1e5e497aea686677a5eee09078d1"],["/categories/数学建模/index.html","35f5ea8e0b2fd22780445ecdb06ae042"],["/categories/数学建模/latex/index.html","9e6e1c13742e962ff2bf9e664fe27dd1"],["/categories/数学建模/优化类/index.html","677c407b8f35a2a7398234d6507651a0"],["/categories/数学建模/优化类/现代优化算法/index.html","a05b1c51dd5024372bb520cf2c2203cd"],["/categories/数学建模/优化类/规划类/index.html","c8409a648c9a8ae7e67d878398a853be"],["/categories/数学建模/绘图/index.html","2188975521770d53c18e3f5957065cbd"],["/categories/数据库/MySQL/index.html","a4e7c8ed377ad48f5bcb828cca4d80b2"],["/categories/数据库/index.html","d03cd409ff26734f0d9cc149389d32dc"],["/categories/数据结构和算法/index.html","bc389847ad7b0f6a7461dcf125c66a2e"],["/categories/数据结构和算法/page/2/index.html","ab03c2e69a12318934769ef4c047073f"],["/categories/数据结构和算法/基本原理/bfs/index.html","90c34d9e066ecbd2960bd8dec1220851"],["/categories/数据结构和算法/基本原理/dfs/index.html","dab3ac980bc253a801c6409593ed66d6"],["/categories/数据结构和算法/基本原理/index.html","f827b97e849e4567cce39a27c8bb1263"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c58bd1e6236b6d5830fbbbef1b0f844e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5c2689a6ea9a407ffa29345978de21ae"],["/categories/数据结构和算法/基本原理/图论/index.html","20b65c63080dbe1df2a882498dbd2d62"],["/categories/数据结构和算法/基本原理/字符串/index.html","f3fc6ecea676c47247d7dd879bee6b83"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d207175e545f329afb71a5acb6377a07"],["/categories/数据结构和算法/基本原理/数论/index.html","bdb505b7982b4cdc4f50b5f16824d338"],["/categories/数据结构和算法/基本原理/树论/index.html","c9f898bfcfe317d263c739c632c3a828"],["/categories/数据结构和算法/基本原理/链表/index.html","3c82aa0d89058d0bd6a7b38b1c508bcd"],["/categories/数据结构和算法/算法题/index.html","3a2e400c9f407bb30a22e0c35bebd338"],["/categories/数据结构和算法/算法题/二分查找/index.html","614172be8c7d106bcaf191b6bc367d46"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","318b3936980352a4548b2970df20ac88"],["/categories/数据结构和算法/算法题/动态规划/index.html","df6b0955a09e1c4c7ba54e7fe92d3384"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","a302de98a5835ad0d127ef71c8f1ef31"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b495f892856e2e8447b2d44992fde0bf"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","0459609580d2590100cba57ef5f17d52"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d199ea13dbcd8fa642bf554d9690660d"],["/categories/数据结构和算法/算法题/数论/index.html","5dae66caa1b9ac48d8fa4c51e3ce8ca7"],["/categories/数据结构和算法/算法题/栈和队列/index.html","f4a2f1435427f3cba0ff10822eac3e99"],["/categories/数据结构和算法/算法题/树论/index.html","dd85a280c44a8e5902a979cb853013bb"],["/categories/杂七杂八/index.html","3f9a151902ea058b989e5a87c9a11031"],["/categories/杂七杂八/博客搭建/index.html","6a68bf8487b8a1409db3a707e646f507"],["/categories/编程工具下载/index.html","d4763da88df193f2b1d26c158f2dec5a"],["/categories/编程环境/index.html","7ff389da28612ecaaee38883c4a52bb2"],["/categories/编程环境/大数据/index.html","014a8f8795ee195cb73212259af7cd6a"],["/categories/英语学习/index.html","4a7dbdabc24a6d5e578188b00669c664"],["/categories/英语学习/英语语法/index.html","995e9338f517cabddf3d1350c158b76b"],["/comments/index.html","54d52c998bb2e654b097d2e351310d91"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","c96337d1b6ead12cd775d65d1feddeaa"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","90b409bf9b403907ac3e0d83d95c9460"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","c58e9e73c302b6560e2035bb9e642b91"],["/movies/index.html","272b4e5cf33a07412b98cc070ee3550c"],["/music/index.html","84dd95ce7302ff35ea298a270254147f"],["/page/2/index.html","f2a00218b61b351deb90aff5afeb8852"],["/page/3/index.html","0b224b4df940da28f91a66c6f83535ae"],["/page/4/index.html","d25c0a8aba53009264335dc5acd96416"],["/page/5/index.html","7387708ac01593d7f4e9e2cef5f6f767"],["/page/6/index.html","47a3e20291c98b49d5f31ead01955e73"],["/posts/1021360842.html","a590dbd9d15636beebc6985ba6ccccd3"],["/posts/1120620192.html","4cda135d499c57478cae3ada8ef5f207"],["/posts/1141628095.html","f0e7eeb427ed6789b60b0398ed4d079b"],["/posts/1168613674.html","9cae881a61ce0b72deb2d3c7f1421fd0"],["/posts/1219920510.html","c86815fa0a739723cce39a383b0dfbe2"],["/posts/1222166338.html","7f966aedf4a9c12d396a77cf19172ad9"],["/posts/1259097482.html","857cece470df540557f3bdb6243142b6"],["/posts/1271036369.html","af1d90698104eba89486c5d2cd2814a2"],["/posts/1312847445.html","c3f6c32ef8728d958b1e46b5993e15e8"],["/posts/135355774.html","27fa5ffc756b792f9e8952567949f212"],["/posts/1375344716.html","5697816379585d098e3e2af6add8477f"],["/posts/1388991698.html","4870e7e63787ff5d12c99c08120f1af8"],["/posts/1410315814.html","243210bb3504d50179fb780d3c7a3550"],["/posts/1452790229.html","5f2e6e0f9ba0cdefd3d08e31c681520e"],["/posts/1470079884.html","aaad6f4d2a18a286e79136c9fd6755de"],["/posts/1470079885.html","31d08e79f3446e97842a8c8cd597ac20"],["/posts/1470079886.html","7b0153f264469ac94d25db12b5027eef"],["/posts/1470079887.html","6c7005d758682f1a48712ea3859287c7"],["/posts/1498536549.html","86a6bc55b37791ccc9ad1608e1d7bede"],["/posts/1539568593.html","f84957a7af8dca49b8a969a7c5975b8b"],["/posts/1547067935.html","6f1c050dabe1ca3c6ab60f465dd7cc46"],["/posts/1557866301.html","4ac65352243cf1ee7e27cf730ce2398f"],["/posts/1571776361.html","914a227980c7f006a15e6cff82b103cd"],["/posts/1605124548.html","e0a337d9c6745e29188f04dab352e278"],["/posts/1633036852.html","3c96e408fb8e8ea6fa5856d8f6873193"],["/posts/1674202625.html","8c1cec6a99fd8bf25c3df636004855b9"],["/posts/1765123828.html","1cffe3495e0c46f026c9703180e9f98e"],["/posts/1767336200.html","1917dea08ae14781918597d6310e2db4"],["/posts/1776114197.html","38316e13989cd31d4c14a64434618afd"],["/posts/1817748743.html","fb947f0528d95605951f2dcd2b76cd61"],["/posts/1925125395.html","44c0472f709900cb324097c97b34ea09"],["/posts/1966191251.html","9d5b1491f603694f57265023ba42be76"],["/posts/1987617322.html","f30dcff015ff8d92badca04ecde2bee5"],["/posts/1999788039.html","8bbb3d678f46c2fa364bc603f0ed5d4f"],["/posts/2075104059.html","be2bf91ba4238c816157130e3c4f11a9"],["/posts/2087796737.html","5b4ad9bcdaca10919eb129352367e26b"],["/posts/2106547339.html","c8560969ce9446f83f2880c856969811"],["/posts/2207806286.html","57c739f868c2aa75b68d548afb0afb4c"],["/posts/2225903441.html","3190cd146a045499b8d22b13f926fb71"],["/posts/2265610284.html","299c9ff27ca9e2d11e70d8bd5c11c7a0"],["/posts/2281352001.html","fb929018aa32c133f14f7d1aef07e463"],["/posts/2364755265.html","fed62929722c36624520ba72eda1744b"],["/posts/2414116852.html","06bc62a9644e87303b35a8ddd012d44d"],["/posts/2421785022.html","c361a9704e39f5d2e21766b34202f8dd"],["/posts/2482902029.html","5f89c5fe79db07c93ef415acc77adc3a"],["/posts/2495386210.html","add578a3c54a8b319b9c741000fef57a"],["/posts/2516528882.html","c71e79aa60ce6ee21ba0ecd64c3e56b5"],["/posts/2526659543.html","691071fc14ee755565406a1ee3d44b68"],["/posts/2529807823.html","9a97b801733eb0f4731819edc0be84b0"],["/posts/2596601004.html","a4539f8f23c0fd6aa394a06534e560ea"],["/posts/2697614349.html","de59a5d26fc9b57705577f5336ac892b"],["/posts/2742438348.html","57b2c2b1140dbc9a299d2678e36772ba"],["/posts/2768249503.html","9b89684731b4f270bdffa86b0c4d35ba"],["/posts/2864584994.html","56d980938d9fb9beb611596635fbd3e4"],["/posts/2888309600.html","aa610fc7a7d98cf78f639b553372ddd6"],["/posts/2891591958.html","966bc1cf722e7a6e3de62060358abc4c"],["/posts/2909934084.html","d37ce7a60d2499e4564bc483eecc6258"],["/posts/2920256992.html","c25072108f4720619e26c014860249d2"],["/posts/2959474469.html","9daa49d6ce71b4863bef99dcc02f1f0a"],["/posts/3005926051.html","79e519bc2d84beec4f6751c4a3f578a0"],["/posts/309775400.html","6b97702786ea8885feabde9c549b5be5"],["/posts/3156194925.html","73726469c86bcf6eb0cd1818d2a1ff0c"],["/posts/3169224211.html","3a9aec5ce920f007464e892fb865712d"],["/posts/3213899550.html","a746580dac4c3e874647cb1b13b205f7"],["/posts/3259212833.html","288379ad0ca80282f1a96f846bb4138c"],["/posts/3266130344.html","f78e838bd63c02056412c238b8af32f3"],["/posts/3292663995.html","3a5f4c1d64042921bd557834337c474e"],["/posts/3297135020.html","823a3e8e74510c5ab6d94b7bb664ad3c"],["/posts/3306641566.html","c31753d4ded565c5d6c8b2e306b15f0a"],["/posts/3312011324.html","b2627faf590fb706bb2ff5e9133fcc94"],["/posts/336911618.html","abf9db092b2ba13828751beb9802e1e2"],["/posts/3402121571.html","617f877032e0da6193d9ee99cb2c7a1f"],["/posts/3405577485.html","d09a841577d1ca01205c45b92be90f9c"],["/posts/3498516849.html","5d4b543ecbc31efdf100369631297e19"],["/posts/3513711414.html","a03df8e9d11d1e2241b7817763f90bcb"],["/posts/3523095624.html","12820798de9694ed9882097ca0ac4401"],["/posts/3546711884.html","76a8ee4f0e2258cfb6f3b7d3caeec07f"],["/posts/3731385230.html","bf6bbcd118ec24442be2a37e9df2f569"],["/posts/3772089482.html","09ef16b33ef7ce9a231f3cf1039496d6"],["/posts/386609427.html","89bd897652a0226d8afb408fed57221f"],["/posts/4044235327.html","6b2d9665d1ec9de1285c05e9ab505b01"],["/posts/4115971639.html","654ea1e32a45ae23f10303b5d1ac2ccf"],["/posts/4130790367.html","a62930c7930b36857994f98474465446"],["/posts/4131986683.html","9675233e8f59ee0e6b1bfcbb0a60f5b4"],["/posts/4177218757.html","4f67a1d5853dc7c4c4eee4d07a91853b"],["/posts/4192183953.html","86c15578db798d77f5cab8c289baace7"],["/posts/4261103898.html","2cfadd55be6cb4e5bfbaccd978de0447"],["/posts/469711973.html","260adb5168e29220c26c2edfc3f98d1a"],["/posts/482495853.html","8eefc4a142e95052d3023e31110d623b"],["/posts/488247922.html","c6d701e29416f81e350681da3609dec4"],["/posts/517302816.html","58c6b36719f4ce619eff488c7ba550c6"],["/posts/570165348.html","ee5e9b8e3a054a0b7c973ed5f50e1ddd"],["/posts/595890772.html","0d5c270348323977a851418b4d899b80"],["/posts/67485572.html","ca31ecc88cf206a48f838e44c0ff4216"],["/posts/694347442.html","82004d5f7482cc3037b780fcee512f12"],["/posts/707384687.html","399707443bc26912e8d240922eda265b"],["/posts/71180092.html","11b62a85e1e6510cfe38866631b21f30"],["/posts/716459272.html","9c4b7e14d68542e56a2310915230b92d"],["/posts/765481613.html","a6717466f41b7903c844e1a5d31801a3"],["/posts/778231993.html","10c2eac20eb14f2bdeedc6bc1bf32a5b"],["/posts/795397410.html","0306f94503dcaa910ad76202815739f3"],["/posts/820223701.html","f0729a7c83a7a61b63193bba2529da47"],["/posts/830372185.html","3cd9282c39e75191cdd8864ae33cda2a"],["/posts/88294277.html","f6a1c937beec5fcba05af7071a64395b"],["/posts/939963535.html","b6473a3ce93cd6167dfe6c75706dbe3b"],["/posts/983786067.html","dee402411609a3879ffb0d3a80f3940b"],["/sw-register.js","45f19df734535507b2d31825efc457d4"],["/tags/C/index.html","fff96da2b89d2dcc4d9230ef6a96a362"],["/tags/C/page/2/index.html","cdd268ceba0ccba61849018e993ddf53"],["/tags/C/page/3/index.html","8b5bfcfe7d9cceb0ea044491a3e04d23"],["/tags/C/page/4/index.html","1b0e3b81f917b8e63fbb5ab993465449"],["/tags/ETL/index.html","92cd9bb63cf600152dbab73a3a3c83fa"],["/tags/ElasticSearch/index.html","a2df88764c6448fdc850b2e7fdda626c"],["/tags/GUI/index.html","73a579053166324446967e53bd809407"],["/tags/HBase/index.html","860ce48bd0c1795a889f5457495f08ac"],["/tags/Hadoop/index.html","fb711ec4fbacdd4f2825ed3b417fde98"],["/tags/Hadoop/page/2/index.html","9417394f1cd8a061804934f9b10c6ae0"],["/tags/Java/index.html","9179892fce9f8095b8f0c3e95dbdfe61"],["/tags/Java后端/index.html","8b1b67e3e9439e800d85045a114c3ffa"],["/tags/Java后端/page/2/index.html","ea54fdc01e6d8cd14ccbbab3134dc522"],["/tags/Java基础/index.html","be25a5d69d35a46bcedfe76cd4388c88"],["/tags/Java基础/page/2/index.html","70b95d3062fb875052241ab6e9d7987b"],["/tags/Kettle/index.html","8c264b4627b5ba2e380ca75965f2f614"],["/tags/Kibana/index.html","9c9407a0854285ce4ed88226836a277a"],["/tags/Linux/index.html","fe5817bb1b2e7288d4199e2cea0b47a3"],["/tags/Linux/page/2/index.html","1cbea164dcc118ce71c761efad5146e1"],["/tags/Linux/page/3/index.html","64d43a1153e02315685e691e6592d727"],["/tags/Mac/index.html","a9a6e11395cb4975cfd0ef7bbcaa183c"],["/tags/Mac/page/2/index.html","5574255effa5983adcc817cd964fe81a"],["/tags/Maven/index.html","5081b29a47282705021cb9d938f9a776"],["/tags/MySQL/index.html","ac768c3d451c27b1729c19904bf0c703"],["/tags/Python/index.html","28ff3ee5365ad84b08ae169d78ce2be2"],["/tags/Redis/index.html","b126b13b4c1ed2679208262b3350b59f"],["/tags/R语言/index.html","7689401a753e7a48c6dbe1a1a3771612"],["/tags/Spark/index.html","aa176cdc77edd403383c9d144791b9e2"],["/tags/Ubuntu/index.html","42089e52e2c5d85b8ce833ac4245f9b9"],["/tags/Vue/index.html","33bd9b32cad8426da6d63f0f3962423b"],["/tags/Windows/index.html","763ed945a74920fa2524b33bb319d46b"],["/tags/ZooKeeper/index.html","4eadddb614978ae1a773c3472684bc64"],["/tags/bfs/index.html","2f0a1fb581d00245be23c09e30343602"],["/tags/dfs/index.html","becf036c0ffd82f15ad1727015425d8f"],["/tags/folium/index.html","78f3d1acf873a8f32d0a2624d5df29c9"],["/tags/git/index.html","b3676ac17e0860fd0c6b9766aec6c21d"],["/tags/index.html","58f8733b32c26fe7dff78a4a5299dd07"],["/tags/latex/index.html","fe4b557185232cc3d8813fa25f5d0f78"],["/tags/中间件/index.html","5a2813b4ba81d658a5a18096c30c9cc5"],["/tags/二分查找/index.html","6d8733f2a254020a0b4a3d393e09c0a3"],["/tags/优化类/index.html","bcf944fd5f9b34444ce7466080e2060e"],["/tags/前端/index.html","f4c3065ec68a8d27e2b9fba61dcceed2"],["/tags/前缀和与差分/index.html","93b5054c7e5170aa39df28b51c158243"],["/tags/动态规划/index.html","b41dd482f7eb4f6129686daa7592cd2d"],["/tags/动态规划/page/2/index.html","eb7eb6c2784a5fbcfa7f4c4c07001193"],["/tags/博客搭建/index.html","336a9d41ceb1504b2d2923e2d30912eb"],["/tags/图论/index.html","d73fed1817c811ae8a6ed7fc2e96058c"],["/tags/大数据/index.html","dbad857484d939351b8601db782120ce"],["/tags/大数据/page/2/index.html","dc34d80af7899a548d8e5e129a2f93df"],["/tags/操作系统/index.html","cb92e2149e4cc8273759001551b2a505"],["/tags/数学建模/index.html","e1b5b8a744fbd529421b0cc862486f08"],["/tags/数据库/index.html","9f790ae1317ba7228f6c46e4571d07e3"],["/tags/数据结构和算法/index.html","c08ade47ec00000e3ca569d9ca34f8a5"],["/tags/数据结构和算法/page/2/index.html","c55d3890e67b3c4b9c4b19ae0d11cc57"],["/tags/数据结构和算法/page/3/index.html","4e218320724bf2a0043095cb6bfcf029"],["/tags/数据结构和算法/page/4/index.html","95dc439cb4c7eb15f9ee11338d693ccc"],["/tags/数组和字符串/index.html","e890b3906317cc57e78d14569af5a98e"],["/tags/数论/index.html","8a83754d427e851cda139ca3ec92bd9f"],["/tags/枚举类/index.html","d4c666836a7ee87fa54549d4040a98b3"],["/tags/栈和队列/index.html","41518a39aa107025951b8687c1ceb443"],["/tags/树论/index.html","c000a33a75d4c724ad8691f38ee3ecea"],["/tags/测试/index.html","7b80a4eafee1f61053139b55308f8805"],["/tags/环境/index.html","23ce837ba42f318848fa7eb5efd5fc78"],["/tags/环境变量/index.html","b23bf8a50651cb1b07fb262df3a2f2de"],["/tags/绘图/index.html","90bf40cb45414f37505312ebcdc68e66"],["/tags/编程工具/index.html","00435f0dc996298baad728661cbc7c93"],["/tags/编程环境/index.html","703dc3889fc1b9e1984918ed868d04ff"],["/tags/网络编程/index.html","53a3f1238fd42f8ad90a31b6542431f6"],["/tags/英语语法/index.html","e63323ff2ad5005b7d8f29e6969a41b4"],["/tags/计算机操作系统/index.html","7f32b1537e2090591851b25af4fe88d6"],["/tags/论文/index.html","56ff8f51e8c1f7dff7806dbe40a34741"],["/tags/资源下载/index.html","5c4ef66d2ce8772bcf31266c6e16ec95"],["/tags/链表/index.html","235bf3aebf1dc613f53105f3fb8d609f"],["/tags/集合/index.html","5cd347ca95a09f3ee0248dc961895183"],["/tags/集群/index.html","f44293b2cf00a3043fee006d8af133f0"]];
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
